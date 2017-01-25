library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(frailtySurv)
library(plyr)
data(hdfail)

# https://www.backblaze.com/b2/hard-drive-test-data.html

shinyServer(function(input,output,session){
  
  hdfail$serial = as.factor(hdfail$serial)
  hdfail$model = as.factor(hdfail$model)
  hdfail$status = as.factor(hdfail$status)
  hdfail$rsc = as.factor(hdfail$rsc)
  hdfail$rer = as.factor(hdfail$rer)
  hdfail$psc = as.factor(hdfail$psc)
  
  # library(outliers)
  # grubbs.test(hdfail$time)
  # hdfail = hdfail[hdfail$time < 4000,]
  
  # Exploration
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
      } 
    }
  })
})

