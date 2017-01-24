library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(rCharts)
library(shinyBS)

shinyUI(
  dashboardPage(skin=c("black"), 
                dashboardHeader(title=div(img(src="ge_monogram_primary_white_RGB.png",height = 50, style="text-align: center;"), "IPS Clustering")),
                dashboardSidebar(disable=TRUE),
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
                                            background-color: #343d46; height:2300px;
                                            }
                                            
                                            /* Miscellaneous UI classes and selectors */
                                            #intro {
                                            border:1px dotted #666;
                                            border-radius: 8px;
                                            padding:100px;
                                            background: rgba(255, 255, 255, 0.025);
                                            height: 250px;
                                            font-size: 24px;
                                            font-weight: bold;
                                            color: #ccc;
                                            text-align:center;
                                            }
                                            .white {
                                            color:#ccc;
                                            }
                                            .large {
                                            font-size:18px;
                                            }
                                            h1 {
                                            font-size:20px;
                                            font-weight: bold;
                                            margin:0 0 8px 0;
                                            }
                                            h2 {
                                            font-size:16px;
                                            font-weight: bold;
                                            margin:0 0 10px 0;
                                            }
                                            .tableHeader {
                                            font-size:14px;
                                            font-weight: normal;
                                            }
                                            .selectedCluster {
                                            font-size:18px;
                                            }
                                            .name {
                                            color:#FFF;
                                            opacity:0.50;
                                            }
                                            .nameHeader {
                                            color: #FFF;
                                            font-weight:bold;
                                            opacity:0.50;
                                            }
                                            .popupItem {
                                            margin-bottom:3px;
                                            }
                                            '
                  ))),
                  fluidRow(style="padding-top: 80px",
                           # Map w/base pipe segment data
                           absolutePanel(top=60, left=320, width=1080, height=550, draggable=FALSE,
                                         leafletOutput("map", width="100%", height="100%"),
                                         style = "padding: 10px; border: 4px solid #343d46; background: #FFFFFF; "
                           ),
                           # Header with summary pipe segment data
                           absolutePanel(top=620, left=330, width=1080, height=100, draggable=FALSE,
                                         p(class="white large", strong("Total pipe segments: "), textOutput(inline=TRUE, "segNum"), "(", textOutput(inline=TRUE, "segLength"), " mi.)")
                           ),
                           
                           #### Right Side Panel ####
                           conditionalPanel(condition="input.click_go!=true",
                                            # Instructional content
                                            absolutePanel(top=675, left=320, width=1080, height=500, draggable=FALSE,
                                                          div(id="intro", "SELECT CLUSTER ATTRIBUTES TO VIEW STATISTICS")
                                            )
                           ),
                           conditionalPanel(condition="input.click_go==true",
                                            absolutePanel(top=670, left=320, width=1080, height=950, draggable=FALSE,
                                                          h1(icon("tasks"), "CLUSTER DETAIL", style="margin-bottom:25px;"),
                                                          showOutput("parcoord", "parcoords"),
                                                          div(dataTableOutput("clusterDetailTable"), style = "font-size:12px; margin-bottom:25px; margin-top: 25px;"),
                                                          style = "padding: 20px; border: 4px solid #343d46; background: #e0e0e0; color: #343d46; opacity:1; border-radius: 8px;"   
                                            ),
                                            absolutePanel(top=693, left=550, width=50, height=40, draggable=FALSE,
                                                          div(class="selectedCluster", strong("Cluster: "))
                                            ),
                                            absolutePanel(top=688, left=620, width=75, height=40, draggable=FALSE,
                                                          selectInput("cluster_no", label=NULL, choices=NULL)
                                            ),
                                            absolutePanel(top=695, left=720, width=250, height=40, draggable=FALSE,
                                                          div(class="tableHeader", strong("Segments: "), textOutput(inline=TRUE, "clusterNoSeg"))
                                            ),
                                            absolutePanel(top=695, left=900, width=250, height=40, draggable=FALSE,
                                                          div(class="tableHeader", strong("Mean similarity: "), textOutput(inline=TRUE, "clusterDistance"))
                                            ),
                                            absolutePanel(top=1650, left=320, width=1080, height=500, draggable=FALSE,
                                                          h1(icon("tasks"), "OVERALL CLUSTER STATISTICS"),
                                                          div(class="tableHeader", strong("Selected features: "), textOutput(inline=TRUE, "noattr")),
                                                          div(class="tableHeader", strong("Number of clusters: "), textOutput(inline=TRUE, "nocluster")),
                                                          div(dataTableOutput("resultTableCluster"), style = "font-size:12px"),
                                                          bsPopover(id="resultTableCluster", 
                                                                    title="Field Explanation", 
                                                                    content=paste0('<div class="popupItem"; style = "font-size:12px"><strong>Similarity: </strong>', "Within cluster average distance", '</div>',
                                                                                   '<div class="popupItem"; style = "font-size:12px"><strong>Diameter: </strong>', "Maximum within cluster distances", '</div>',
                                                                                   '<div class="popupItem"; style = "font-size:12px"><strong>Separation: </strong>', "Minimum distances of a point in the cluster to a point of another cluster", '</div>',
                                                                                   '<div class="popupItem"; style = "font-size:12px"><strong>MeanToOther: </strong>', "Average distances of a point in the cluster to the points of other clusters", '</div>'), 
                                                                    placement = "left", 
                                                                    trigger = "click"),
                                                          style = "padding: 20px; border: 4px solid #343d46; background: #e0e0e0; color: #343d46; opacity:1; border-radius: 8px;"
                                            )             
                           ),
                           #### Left Side Panel ####
                           absolutePanel(top=60, left=10, width=300, height=585, draggable=FALSE,
                                         p("CLUSTER ATTRIBUTES", style = "color:#4f372d; font-size:18px; font-weight:bold;"),
                                         p("This Application is to demonstrate results from clustering similar pipelines based on selected attributes.", style = "color:#4f372d"),
                                         # checkboxGroupInput("clust_set", label="Choose a Threat Category", choices=NULL),
                                         radioButtons("clust_set", 
                                                      label="Choose a Threat Category", 
                                                      choices=c("External Corrosion" = "EXTERNAL.CORROSION",
                                                                "Internal Corrosion" = "INTERNAL.CORROSION",
                                                                "Mechanical Damage" = "MECHANICAL.DAMAGE",
                                                                "Fatigue" = "FATIGUE",
                                                                "Sour Cracking" = "SOUR.CRACKING",
                                                                "Incorrect Operations" = "INCORRECT.OPERATIONS")),
                                         conditionalPanel(condition="input.clust_set=='EXTERNAL.CORROSION'",
                                                          checkboxGroupInput("ec_attr",
                                                                             label="Choose Up to 3 Clustering Features (EC):", 
                                                                             choices=c("Previous Failure" = "HASPREVIOUSECFAILURE",
                                                                                       "Average CP Reading" = "AVGCPREADING",
                                                                                       "Coating" = "COATING",
                                                                                       "Max Depth (from ILI)" = "ECMAXDEPTFROMILI",
                                                                                       "Pressure" = "PRESSURE",
                                                                                       "Outside Diameter" = "OUTSIDEDIAMETER",
                                                                                       "Wall Thickness" = "WALLTHICKNESS",
                                                                                       "SMYS" = "SMYS"))),                                         
                                         conditionalPanel(condition="input.clust_set=='INTERNAL.CORROSION'",
                                                          checkboxGroupInput("ic_attr",
                                                                             label="Choose Up to 3 Clustering Features (IC):", 
                                                                             choices=c("Install year" = "PIPEINSTALLYEAR",
                                                                                       "Previous Failure" = "HASPREVIOUSICFAILURE",
                                                                                       "Max Depth (from ILI)" = "ICMAXDEPTFROMILI",
                                                                                       "Pressure" = "PRESSURE",
                                                                                       "Outside Diameter" = "OUTSIDEDIAMETER",
                                                                                       "Wall Thickness" = "WALLTHICKNESS",
                                                                                       "SMYS" = "SMYS"))),
                                         conditionalPanel(condition="input.clust_set=='MECHANICAL.DAMAGE'",
                                                          checkboxGroupInput("md_attr",label="Choose Up to 3 Clustering Features (MD):", 
                                                                             choices=c("Shared Row Count" = "SHAREDROWCOUNT",
                                                                                       "Land Use" = "LANDUSE",
                                                                                       "Max Depth (from ILI, 3GT)" = "MAXILIDEPTH_EC_G3T",
                                                                                       "Outside Diameter" = "OUTSIDEDIAMETER",
                                                                                       "Wall Thickness" = "WALLTHICKNESS",
                                                                                       "SMYS" = "SMYS"))),
                                         conditionalPanel(condition="input.clust_set=='FATIGUE'",
                                                          checkboxGroupInput("fat_attr",
                                                                             label="Choose Up to 3 Clustering Features (FAT):", 
                                                                             choices=c("Install year" = "PIPEINSTALLYEAR",
                                                                                       "Pressure" = "PRESSURE",
                                                                                       "Outside Diameter" = "OUTSIDEDIAMETER",
                                                                                       "Wall Thickness" = "WALLTHICKNESS",
                                                                                       "SMYS" = "SMYS",
                                                                                       "Pressure Range" = "PRESSURERANGE"))),
                                         conditionalPanel(condition="input.clust_set=='SOUR.CRACKING'",
                                                          checkboxGroupInput("sc_attr",
                                                                             label="Choose Up to 3 Clustering Features (SC):", 
                                                                             choices=c("Install year" = "PIPEINSTALLYEAR",
                                                                                       "Pressure" = "PRESSURE",
                                                                                       "Outside Diameter" = "OUTSIDEDIAMETER",
                                                                                       "Wall Thickness" = "WALLTHICKNESS",
                                                                                       "SMYS" = "SMYS"))),
                                         conditionalPanel(condition="input.clust_set=='INCORRECT.OPERATIONS'",
                                                          checkboxGroupInput("io_attr",label="Choose Up to 3 Clustering Features (IO):", 
                                                                             choices=c("Operating Training" = "OPERATINGTRAINING",
                                                                                       "Pressure Control System" = "PRESSURECONTROLSYS",
                                                                                       "SCADA System" = "SCADASYSTEM",
                                                                                       "Pressure" = "PRESSURE",
                                                                                       "Outside Diameter" = "OUTSIDEDIAMETER",
                                                                                       "Wall Thickness" = "WALLTHICKNESS",
                                                                                       "SMYS" = "SMYS"))),
                                         actionButton("click_go","Go", style="margin-bottom:25px"),
                                         style = "padding: 15px; border: 4px solid #343d46; background: #e0e0e0; border-radius: 8px; font-size:12.5px"
                           ),
                           absolutePanel(top=650, left=10, width=300, draggable=FALSE,
                                         p("Application provided by GE Digital",class="nameHeader"),
                                         p(strong("Algorithm: "), " Axel Hochstein (axel.hochstein@ge.com), Fei Huang (huang.fei@ge.com)",class="name"),
                                         p(strong("UX Design: "), "Ryan Mccormack (ryan.mccormack@ge.com)",class="name"),
                                         p(strong("Shiny: "), "Sherry Lau (sherry.lau@ge.com), Robert Clements (robert.clements@ge.com)",class="name"),
                                         style = "padding: 15px; border: 4px solid #343d46; background: rgba(255, 255, 255, 0.05); border-radius: 8px;"
                           )
                           
                  ))
                  ))



