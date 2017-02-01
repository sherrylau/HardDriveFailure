library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(skin = "red",
                dashboardHeader(title="Hard Drive Time to Failure Analysis", titleWidth=400),
                dashboardSidebar(disable=FALSE,
                                 sidebarMenu(
                                   menuItem("Problem Description", tabName = "description", icon = icon("th")),
                                   menuItem("Data Exploration", tabName = "explore", icon = icon("th")),
                                   menuItem("Survival Curve", tabName = "surv_curve", icon = icon("th")),
                                   menuItem("Cox Proportional Model", tabName = "coxph", icon = icon("th"))
                                 )),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "description",
                            absolutePanel(top=60, left=250, width=1170, height=80, draggable=FALSE,
                                          h2("Problem Description", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=250, width=1170, height=610, draggable=FALSE,
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                                          )
                            ),
                    tabItem(tabName = "explore",
                            absolutePanel(top=60, left=250, width=1170, height=80, draggable=FALSE,
                                          h2("Data Exploration", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=250, width=300, height=610, draggable=FALSE,
                                          p("Use the following dropdown menu to select variable to explore distribution"),
                                          radioButtons(inputId='explore_method', label="Choose to explore individual variable distribution or univariate against response", choices=c('individual','univariate'), width=200),
                                          # Individual Distribution
                                          conditionalPanel(condition="input.explore_method=='individual'",
                                                           selectInput(inputId='explore_type', label="Choose a type of variable", multiple=FALSE, choices=c("target", "features"), width=200)
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='individual' & input.explore_type=='features'", 
                                                           selectInput(inputId="explore_feature_type", label="Choose a type of feature", multiple=FALSE, choices=c("categorical","numerical"), width=200)
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='individual' & input.explore_type=='features' & input.explore_feature_type=='categorical'",
                                                           radioButtons(inputId="explore_catvar", label="Choose a feature", choices=c("Hard drive model provider"="model_provider",
                                                                                                                                      "(RSC) Whether there were sectors encountered read, write, or verification errors"="rsc", 
                                                                                                                                      "(RER) Whether a non-zero rate of errors occur in hardware when reading from data from disk"="rer", 
                                                                                                                                      "(PSC) Whether there were sectors waiting to be remapped due to an unrecoverable error"="psc"))
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='individual' & input.explore_type=='features' & input.explore_feature_type=='numerical'",
                                                           radioButtons(inputId="explore_numvar", label="Choose a feature", choices=c("Observed followup time"="time",
                                                                                                                                      "Temperature in Celsius"="temp"))
                                                           ),
                                          # Univariate Distribution
                                          conditionalPanel(condition="input.explore_method=='univariate'",
                                                           selectInput(inputId="uni_feature_type", label="Choose a type of feature", multiple=FALSE, choices=c("categorical","numerical"))
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='univariate' & input.uni_feature_type=='categorical'",
                                                           radioButtons(inputId="uni_catvar", label="Choose a feature", choices=c("Hard drive model provider"="model_provider",
                                                                                                                                  "(RSC) Whether there were sectors encountered read, write, or verification errors"="rsc", 
                                                                                                                                 "(RER) Whether a non-zero rate of errors occur in hardware when reading from data from disk"="rer", 
                                                                                                                                 "(PSC) Whether there were sectors waiting to be remapped due to an unrecoverable error"="psc"))
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='univariate' & input.uni_feature_type=='numerical'",
                                                           radioButtons(inputId="uni_numvar", label="Choose a feature", choices=c("Observed followup time"="time",
                                                                                                                                 "Temperature in Celsius"="temp"))
                                                           ),
                                          style = "padding: 30px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=545, width=875, height=610, draggable=FALSE,
                                          conditionalPanel(condition="input.explore_method=='individual'",
                                                           plotlyOutput(outputId="explore_dist", width="95%")
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='univariate'",
                                                           h3("Visualization", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                                           plotlyOutput(outputId="uni_dist", width="95%"),
                                                           br(),
                                                           h3("Statistical Testing", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                                           textOutput(outputId="uni_test")
                                                           ),
                                          style = "padding: 5px; border: 4px solid #343d46; background: #FFFFFF;"
                                          )
                            ),
                    tabItem(tabName = "surv_curve",
                            absolutePanel(top=60, left=250, width=1170, height=80, draggable=FALSE,
                                          h2("Hard Drive Survival Estimation and Log Rank Test", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=250, width=300, height=610, draggable=FALSE,
                                          p("Use the following dropdown menu to select variable to explore hard drive survival function"),
                                          selectInput(inputId="surv_method", label="Choose to view population or comparison by groups survival functions", choices=c("population","comparison"), width=200),
                                          conditionalPanel(condition="input.surv_method=='comparison'",
                                                           radioButtons(inputId="surv_features_gp", label="Choose a feature as group",  choices=c("Hard drive model provider"="model_provider",
                                                                                                                                                  "(RSC) Whether there were sectors encountered read, write, or verification errors"="rsc", 
                                                                                                                                                  "(RER) Whether a non-zero rate of errors occur in hardware when reading from data from disk"="rer", 
                                                                                                                                                  "(PSC) Whether there were sectors waiting to be remapped due to an unrecoverable error"="psc",
                                                                                                                                                  "Grouped Temperature in Celsius"="temp_gp"))
                                                           ),
                                          style = "padding: 10px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=135, left=545, width=875, height=610, draggable=FALSE,
                                          conditionalPanel(condition="input.surv_method=='population'",
                                                           h4("Hard Drive Survival Probability over time"),
                                                           plotlyOutput(outputId="surv_pop")
                                          ),
                                          conditionalPanel(condition="input.surv_method=='comparison'",
                                                           h4("Hard Drive Survival Probability over time by Features"),
                                                           plotlyOutput(outputId="surv_gp")
                                                           ),
                                          style = "padding: 5px; border: 4px solid #343d46; background: #FFFFFF;"
                            )
                            ),
                    tabItem(tabName = "coxph",
                            absolutePanel(top=60, left=250, width=1170, height=80, draggable=FALSE,
                                          h2("Cox Proportional Model", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=135, left=250, width=300, height=610, draggable=FALSE,
                                          p("70% of the data is stratified sampled by status as training data and the rest as testing data"),
                                          p("Use the following dropdown menu to select features to apply in cox proportional model"),
                                          checkboxGroupInput("coxph_features", 
                                                       label="Select features to be included for modeling then click submit",
                                                       choices=c("Hard drive model provider"="model_provider",
                                                                 "(RSC) Whether there were sectors encountered read, write, or verification errors"="rsc",
                                                                 "(RER) Whether a non-zero rate of errors occur in hardware when reading from data from disk"="rer", 
                                                                 "(PSC) Whether there were sectors waiting to be remapped due to an unrecoverable error"="psc",
                                                                 "Temperature in Celsius"="temp")
                                          ),
                                          actionButton("coxph_submit", "Submit"),
                                          style = "padding: 10px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=135, left=545, width=875, height=610, draggable=FALSE,
                                          h3("Model Result", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          dataTableOutput('coxph_model_summary'),
                                          style = "padding: 5px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=385, left=555, width=430, height=220, draggable=FALSE,
                                          h3("Testing Set Actual vs. Predicted over time", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          plotlyOutput('coxph_predplot'),
                                          style = "background: #FFFFFF;"
                                          )
                            )
                  ))
  )
)