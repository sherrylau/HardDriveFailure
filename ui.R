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
                                          conditionalPanel(condition="input.explore_method=='individual'",
                                                           selectInput(inputId='explore_type', label="Choose a type of variable", multiple=FALSE, choices=c("target", "features"), width=200)
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='individual' & input.explore_type=='features'", 
                                                           selectInput(inputId="explore_feature_type", label="Choose a type of feature", multiple=FALSE, choices=c("categorical","numerical"), width=200)
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='individual' & input.explore_type=='features' & input.explore_feature_type=='categorical'",
                                                           radioButtons(inputId="explore_catvar", label="Choose a feature", choices=c("(RSC) Whether there were sectors encountered read, write, or verification errors"="rsc", 
                                                                                                                                   "(RER) Whether a non-zero rate of errors occur in hardware when reading from data from disk"="rer", 
                                                                                                                                   "(PSC) Whether there were sectors waiting to be remapped due to an unrecoverable error"="psc"))
                                                           ),
                                          conditionalPanel(condition="input.explore_method=='individual' & input.explore_type=='features' & input.explore_feature_type=='numerical'",
                                                           radioButtons(inputId="explore_numvar", label="Choose a feature", choices=c("Observed followup time"="time",
                                                                                                                                      "Temperature in Celsius"="temp"))
                                                           ),
                                          style = "padding: 30px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=545, width=875, height=610, draggable=FALSE,
                                          plotlyOutput(outputId="explore_dist", width="95%"),
                                          style = "padding: 10px; border: 4px solid #343d46; background: #FFFFFF;"
                                          )
                            ),
                    tabItem(tabName = "surv_curve",
                            absolutePanel(top=60, left=250, width=1170, height=80, draggable=FALSE,
                                          h2("Survival Curve and Log Rank Test", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                                          ),
                            absolutePanel(top=135, left=250, width=300, height=610, draggable=FALSE,
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=135, left=545, width=875, height=610, draggable=FALSE,
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                            )
                            ),
                    tabItem(tabName = "coxph",
                            absolutePanel(top=60, left=250, width=1170, height=80, draggable=FALSE,
                                          h2("Cox Proportional Model", style="padding-left: 30px;"),
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=135, left=250, width=300, height=610, draggable=FALSE,
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                            ),
                            absolutePanel(top=135, left=545, width=875, height=610, draggable=FALSE,
                                          style = "padding: 1px; border: 4px solid #343d46; background: #FFFFFF;"
                            )
                            )
                  ))
  )
)