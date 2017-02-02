library(shiny)
library(shinydashboard)
library(plotly)

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
                  tags$head(tags$style(HTML('
                                            /* logo */
                                            .skin-black .main-header .logo{
                                            background-color: #4f5b66; border-color:#4f5b66; color: #FFFFFF; font-weight: bold;
                                            }
                                            /* logo when hovered */
                                            .skin-black .main-header .logo:hover {
                                            background-color: #4f5b66; border-color:#4f5b66; color: #FFFFFF; font-weight: bold;
                                            }
                                            /* navbar */
                                            .skin-black .main-header .navbar {
                                            background-color: #4f5b66;
                                            }
                                            .content-wrapper,
                                            .right-side {
                                            overflow-y: auto; height:1100px; background-color: #343d46;  
                                            }'))),
                  tabItems(
                    tabItem(tabName = "description",
                            absolutePanel(top=60, left=250, width=1150, height=80, draggable=FALSE,
                                          h2("Problem Description", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=250, width=1150, height=610, draggable=FALSE,
                                          h3("Use Case", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          p("Asset lifetime is always the interest for industrial analytics. Knowing the probability of failure at particular time helps company to facilitate maintenance scheduling, asset supply planning, duration of warranty for assets, etc. Typical failure prediction or analysis only provide probability asset will be failing but using survival analysis also provide additional information of failure probability at different time. This application is intended to provide insights and built predictive model for hard drive for which dataset is publicly available"),
                                          h3("Data Description", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          p("This dataset is preprocessed from the source and retrieved from a R package 'frailtySurv' which contains the observed follow-up times and SMART statistics of 52k unique hard drives."),
                                          p("Daily snapshots of a large backup storage provider over 2 years were made publicly available at https://www.backblaze.com/hard-drive-test-data.html."),
                                          p("On each day, the self-monitoring, analysis and reporting technology (SMART) statistics of operational drives are recorded. When a hard drive is no longer operational, it is marked as failure and removed from the subsequent daily snapshots. New hard drives are also continuously added to the population. In total, there are over 52k unique hard drives over approximately 2 years and 2885 (5.5%) failures."),
                                          h3("Attributes Description", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          p("There's a total of 8 variables in the dataset"),
                                          p("serial = unique serial number of the hard drive. Primary key for the dataset."),
                                          p("model = hard drive model, model provider = column is modified from model to get the provider only information rather than including the device number"),
                                          p("time = the observed followup time"),
                                          p("status = failure indicator. binary where 0 = censored and 1 = failed"),
                                          p("temp = temperature in Celsius"),
                                          p("rsc = binary, where 1 = sectors that encountered read, write or vertification errors"),
                                          p("rer = binary, where 1 = a non-zero rate of errors that occur in hardware when reading from data from disk"),
                                          p("psc = binary, where 1 = there were sectors waiting to be remapped due to unrecoverable error"),
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
                                                           h3("Hard Drive Survival Probability over time", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                                           plotlyOutput(outputId="surv_pop")
                                          ),
                                          conditionalPanel(condition="input.surv_method=='comparison'",
                                                           h3("Hard Drive Survival Probability over time by Features", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
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
                            absolutePanel(top=135, left=250, width=300, height=1000, draggable=FALSE,
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
                            absolutePanel(top=135, left=545, width=875, height=1000, draggable=FALSE,
                                          h3("Model Result", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          dataTableOutput('coxph_model_summary'),
                                          style = "padding: 5px; border: 4px solid #343d46; background: #FFFFFF;"),
                            
                            absolutePanel(top=405, left=555, width=850, height=300, draggable=FALSE,
                                          h3("Predicted and Actual Survival Curve", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;"),
                                          plotlyOutput('coxph_plot'),
                                          conditionalPanel(condition="input.coxph_submit==1", h3("Predicted probability of hard drive at time t", style="font-size:20px; font-weight: bold; margin:0 0 8px 0;")),
                                          style = "background: #FFFFFF;"
                                          ),
                            absolutePanel(top=865, left=555, width=300, height=250, draggable=FALSE,
                                          conditionalPanel(condition="input.coxph_submit==1", 
                                                           selectInput("coxph_predict_id", label="Choose a hard drive serial number", choices=NULL)),
                                          textOutput("coxph_pred"),
                                          style = "background: #FFFFFF;"
                                          ),
                            absolutePanel(top=865, left=905, width=300, height=250, draggable=FALSE,
                                          conditionalPanel(condition="input.coxph_submit==1", 
                                                           selectInput("coxph_time", label="Choose a time to predict", choices=seq(100,5000,100))),
                                          style = "background: #FFFFFF;"
                                          )
                            )
                  ))
  )
)