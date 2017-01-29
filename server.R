library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(frailtySurv)
library(plyr)
library(survival)
library(coxphf)
library(ggfortify)
data(hdfail)

# https://www.backblaze.com/b2/hard-drive-test-data.html

shinyServer(function(input,output,session){
  
  hdfail$serial = as.factor(hdfail$serial)
  hdfail$model = as.factor(hdfail$model)
  hdfail$model = gsub('^ST','ST ', hdfail$model)
  hdfail$model_provider = sapply(strsplit(hdfail$model, ' '),"[",1)
  hdfail$status = as.factor(hdfail$status)
  hdfail$rsc = as.factor(hdfail$rsc)
  hdfail$rer = as.factor(hdfail$rer)
  hdfail$psc = as.factor(hdfail$psc)
  hdfail$temp_gp = ifelse(hdfail$temp < as.numeric(quantile(hdfail$temp, 0.5)), 'low', 'high')
    
  
  # library(outliers)
  # grubbs.test(hdfail$time)
  # hdfail = hdfail[hdfail$time < 4000,]
  
  #### 1. Exploration ####
  output$explore_dist = renderPlotly({
    if(input$explore_method=="individual"){
      if(input$explore_type=="target"){
        p = ggplot(hdfail, aes(x=status)) + geom_bar(fill="#d36666") + labs(x="Failure Status", y="No. of Observations")
        ggplotly(p)
      } else{
        if(input$explore_feature_type=="categorical"){
          p = ggplot(hdfail, aes_string(x=input$explore_catvar)) + geom_bar(fill="#20b2aa") + labs(x=input$explore_catvar, y="No. of Observations")
          ggplotly(p)
        } else{
          p = ggplot(hdfail, aes_string(x=input$explore_numvar)) + geom_histogram(fill="#caa225") + labs(x=input$explore_numvar, y="No. of Observations")
          ggplotly(p)
        }
      }
    }
  })
  
  output$uni_dist = renderPlotly({
    if(input$explore_method=="univariate"){
      if(input$uni_feature_type=="categorical"){
        cat_summary = ddply(hdfail, c("status", input$uni_catvar), summarise, freq=length(serial))
        cat_summary$status = as.factor(cat_summary$status)
        p = ggplot(cat_summary, aes_string(x="status", y=input$uni_catvar, fill='freq')) + 
          geom_tile(color="white") +
          scale_fill_gradient(low = "#f8f8fa", high = "#00a0b0") +
          geom_text(aes(label=freq))
        ggplotly(p)
      } else{
        hdfail$status = as.factor(hdfail$status)
        p = ggplot(hdfail, aes_string(x="status", y=input$uni_numvar, fill="status")) + 
          geom_boxplot() + 
          scale_fill_brewer(palette="BrBG", guide=FALSE)
        ggplotly(p)
      }
    }
  })
  
  output$uni_test = renderText({
    if(input$explore_method=="univariate"){
      if(input$uni_feature_type=="categorical"){
        test = chisq.test(hdfail[,input$uni_catvar], hdfail$status)
        print(paste(test$method, "X-squared =", round(test$statistic,2), ", df =",test$parameter, ", p-value =", round(test$p.value,4)))
      } else{
        formula_in = as.formula(paste(input$uni_numvar,"~status",collapse=""))
        test = summary(aov(formula_in, data = hdfail))
        print(paste("ANOVA test with F value:", unlist(test[[1]]["F value"])[1],"with p-value", unlist(test[[1]]["Pr(>F)"])[1]))
      }
    }
  })
  
  #### 2. Survival Esimation ####
  output$surv_pop = renderPlotly({
    hdfail$status = as.numeric(hdfail$status)
    surv = survfit(Surv(time, status)~ 1, data=hdfail)
    p = autoplot(surv, censor.size = 0.5, censor.colour = '#666666')
    ggplotly(p)
  })
  
  output$surv_gp = renderPlotly({
    if(!is.null(input$surv_features_gp)){
      hdfail$status = as.numeric(hdfail$status)
      formula_in = as.formula(paste("Surv(time, status) ~ strata(", input$surv_features_gp, ")"))
      surv_strata = survfit(formula_in, data=hdfail)
      p = autoplot(surv_strata, censor.size = 0.5, censor.colour = '#666666')
      ggplotly(p)
    }
  })
  
  output$surv_gp_test = renderPrint({
    if(!is.null(input$surv_features_gp)){
      formula_in = as.formula(paste("Surv(time, status) ~ ", input$surv_features_gp))
      surv_test = survdiff(formula_in, data=hdfail, rho=0)
      capture.output(surv_test)
    }
  })
  
  #### 3. Cox Proportional Model ####
  clicked = reactiveValues(train_data = NULL)
  
  observeEvent(input$coxph_submit, {
    if(!is.null(input$coxph_features)){
      clicked$train_data = hdfail[,c(input$coxph_features,"status","time")]
    }
    
  })
  
  # output$test_print = renderPrint({
  #   if(!is.null(clicked$train_data)){
  #     print(input$coxph_features)
  #   }
  # })
  
  output$test_data = renderDataTable({
    if(!is.null(clicked$train_data)){
      train_data = data.frame(clicked$train_data)
      if(length(input$coxph_features)>1){
        coxphf(Surv(time,status) ~ temp, data=hdfail)
        formula_in = as.formula(paste("Surv(time, status) ~ 1", paste("+", input$coxph_features, collapse='')))
        coxph(formula_in, data=clicked$train_data)
      }
    }
  })
  
})

