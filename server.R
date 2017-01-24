library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(frailtySurv)
data(hdfail)

# https://www.backblaze.com/b2/hard-drive-test-data.html

shinyServer(function(input,output,session){
  
  hdfail$serial = as.factor(hdfail$serial)
  hdfail$model = as.factor(hdfail$model)
  hdfail$status = as.factor(hdfail$status)
  hdfail$rsc = as.factor(hdfail$rsc)
  hdfail$rer = as.factor(hdfail$rer)
  hdfail$psc = as.factor(hdfail$psc)
  
  # Exploration
  output$explore_dist = renderPlotly({
    if(!is.null(input$explore_type)){
      if(input$explore_type=="target"){
        p = ggplot(hdfail, aes(x=status)) + geom_bar(fill="#d36666") + labs(x="Failure Status", y="No. of Observations")
        ggplotly(p)
      } else{
        p = ggplot(hdfail, aes_string(x=input$explore_catvar)) + geom_bar(fill="#20b2aa") + labs(x=input$explore_catvar, y="No. of Observations")
        ggplotly(p)
      }
    }
  })
})

