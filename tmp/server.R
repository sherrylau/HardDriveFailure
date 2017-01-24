library(shiny) #*
library(shinyBS)
library(leaflet) #*
library(htmltools)
library(plyr)
library(DT)
library(rgdal) #*
library(maptools) #*
library(rCharts)

shinyServer(function(input,output,session){
  # Modified cluster.stats to include std calculation
  cluster.stats = function(d,clustering,alt.clustering=NULL,silhouette=TRUE,G2=FALSE,G3=FALSE,compareonly=FALSE){
    cn <- max(clustering)
    n <- length(clustering)
    diameter <- average.distance  <- sd.distance <- median.distance <- separation <-
      average.toother <- 
      cluster.size <- within.dist <- between.dist <- numeric(0) # change
    for (i in 1:cn)
      cluster.size[i] <- sum(clustering==i)
    pk1 <- cluster.size/n
    pk10 <- pk1[pk1>0]
    h1 <- -sum(pk10*log(pk10))
    corrected.rand <- vi <- NULL
    if (!is.null(alt.clustering)){
      choose2 <- function(v){
        out <- numeric(0)
        for (i in 1:length(v))
          out[i] <- ifelse(v[i]>=2,choose(v[i],2),0)
        out
      }
      cn2 <- max(alt.clustering)
      nij <- table(clustering,alt.clustering)
      dsum <- sum(choose2(nij))
      cs2 <- numeric(0)
      for (i in 1:cn2)
        cs2[i] <- sum(alt.clustering==i)
      sum1 <- sum(choose2(cluster.size))
      sum2 <- sum(choose2(cs2))
      pk2 <- cs2/n
      pk12 <- nij/n
      corrected.rand <- (dsum-sum1*sum2/choose2(n))/
        ((sum1+sum2)/2-sum1*sum2/choose2(n))
      pk20 <- pk2[pk2>0]
      h2 <- -sum(pk20*log(pk20))
      icc <- 0
      for (i in 1:cn)
        for (j in 1:cn2)
          if (pk12[i,j]>0)
            icc <- icc+pk12[i,j]*log(pk12[i,j]/(pk1[i]*pk2[j]))
      #    print(icc)
      vi <- h1+h2-2*icc 
    }
    if (compareonly){
      out <- list(corrected.rand=corrected.rand,vi=vi)
    }
    else{
      if (silhouette) require(cluster)
      dmat <- as.matrix(d)
      within.cluster.ss <- 0
      separation.matrix <- matrix(0,ncol=cn,nrow=cn)
      di <- list()
      for (i in 1:cn){
        cluster.size[i] <- sum(clustering==i)
        di <- as.dist(dmat[clustering==i,clustering==i])
        within.cluster.ss <- within.cluster.ss+sum(di^2)/cluster.size[i]
        within.dist <- c(within.dist,di)
        if (sum(clustering==i)>1)
          diameter[i] <- max(di)
        else
          diameter[i] <- 0        
        average.distance[i] <- mean(di)
        median.distance[i] <- median(di)
        sd.distance[i] <- sd(di) #change
        bv <- numeric(0)
        for (j in 1:cn){
          if (j!=i){
            sij <- dmat[clustering==i,clustering==j]
            bv <- c(bv,sij)
            if (i<j){
              separation.matrix[i,j] <- separation.matrix[j,i] <- min(sij)
              between.dist <- c(between.dist,sij)
            }
          }
        }
        separation[i] <- min(bv)
        average.toother[i] <- mean(bv)
      }
      average.between <- mean(between.dist)
      average.within <- mean(within.dist)
      nwithin <- length(within.dist)
      nbetween <- length(between.dist)
      clus.avg.widths <- avg.width <- NULL
      if (silhouette){
        sc <- summary(silhouette(clustering,dmatrix=dmat))
        clus.avg.widths <- sc$clus.avg.widths
        avg.width <- sc$avg.width
      }
      g2 <- g3 <- cn2 <- NULL
      if (G2){
        splus <- sminus <- 0
        for (i in 1:nwithin) {
          splus  <- splus  + sum(within.dist[i]<between.dist)
          sminus <- sminus + sum(within.dist[i]>between.dist) 
        }
        g2 <- (splus - sminus)/(splus + sminus)
      }
      if (G3){
        sdist <- sort(c(within.dist,between.dist))
        sr <- nwithin+nbetween
        dmin <- sum(sdist[1:nwithin])
        dmax <- sum(sdist[(sr-nwithin+1):sr])
        g3 <- (sum(within.dist)-dmin)/(dmax-dmin)
      }
      hubertgamma <- cor(c(within.dist,between.dist),c(rep(0,nwithin),
                                                       rep(1,nbetween)))
      dunn <- min(separation)/max(diameter)
      out <- list(
        n=n,
        cluster.number=cn,
        cluster.size=cluster.size, # vector of cluster sizes
        diameter=diameter, # vector of cluster diameters
        average.distance=average.distance,
        # vector of within cl. av. dist.
        median.distance=median.distance,
        sd.distance=sd.distance, #change
        # vector of within cl. median dist.
        separation=separation, # vector of min. clusterwise between dist.
        average.toother=average.toother, 
        # vector of mean clusterwise between dist.
        separation.matrix=separation.matrix,
        # clusterwise matrix of min. between dist.
        average.between=average.between, # mean between cl. distance
        average.within=average.within, # mean within cl. distance
        n.between=nbetween, # number of between cl. distances
        n.within=nwithin, # number of within cl. distances
        within.cluster.ss=within.cluster.ss,
        clus.avg.silwidths=clus.avg.widths,
        # vector of cluster avg. silhouette widths
        avg.silwidth=avg.width, # average silhouette width
        g2=g2, # Goodman and Kruskal coefficient, see Gordon p. 62
        g3=g3, # G3 index, see Gordon p. 62
        hubertgamma=hubertgamma, # Correlation between distances and
        # 0-1-vector same/different cluster
        dunn=dunn, # Dunn index, see Halkidi et al. (2002)
        # Min. sepatation / max. diameter
        entropy=h1,
        wb.ratio=average.within/average.between,
        corrected.rand=corrected.rand, vi=vi) # Corrected rand index between
      
      # clustering and alt.clustering
      #  class(out) <- "cluster.stats"
    }
    out
  }
  
  # Function to convert Points to PolyLines
  points_to_line = function(data, long, lat, id_field = NULL, sort_field = NULL) {
    library(sp)
    library(maptools)
    coordinates(data) = c(long, lat)
    if (!is.null(sort_field)) {
      if (!is.null(id_field)) {
        data <- data[order(data[[id_field]], data[[sort_field]]), ]
      } else {
        data <- data[order(data[[sort_field]]), ]
      }
    }
    if (is.null(id_field)) {   
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))    
      lines
    } else if (!is.null(id_field)) {      
      paths <- split(data, data[[id_field]])    
      sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))   
      outData = data.frame(paths[[1]]@data)
      for (p in 2:length(paths)) {
        pathdata = data.frame(paths[[p]]@data)
        pathdata = pathdata[!duplicated(pathdata),]
        outData = rbind(outData, pathdata)
        id <- paste0("line", as.character(p))
        l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
        sp_lines <- spRbind(sp_lines, l)
      }
      outData = outData[!duplicated(outData),]
      rownames(outData) = NULL
      splines_df = SpatialLinesDataFrame(sp_lines, data=outData, match.ID=FALSE)
      return(splines_df)
    }
  }
  
  showLoading <- function() {
    session$sendCustomMessage("loading", "show")
  }
  
  hideLoading <- function() {
    session$sendCustomMessage("loading", "hide")
  }
  
  input_data = read.csv("data/data_processed_cmg_risk_v2.csv")
  
  #pipe_segment = readOGR("data/dynamicpipesegments_cmg/","dynamicpipesegments_cmg")
  #getinfo.shape("data/dyn_segs/CPG_dynamic_segs.shp")
  pipe_segment = readOGR("data/dyn_segs_cmg","dyn_segs_cmg") #*
  str(pipe_segment)
  clusterCounter <- 0
  
  observe({
    if(length(input$ec_attr) > 3) {updateCheckboxGroupInput(session, "ec_attr", selected= tail(input$ec_attr,3))}
    if(length(input$ic_attr) > 3) {updateCheckboxGroupInput(session, "ic_attr", selected= tail(input$ic_attr,3))}
    if(length(input$md_attr) > 3) {updateCheckboxGroupInput(session, "md_attr", selected= tail(input$md_attr,3))}
    if(length(input$fat_attr) > 3) {updateCheckboxGroupInput(session, "fat_attr", selected= tail(input$fat_attr,3))}
    if(length(input$sc_attr) > 3) {updateCheckboxGroupInput(session, "sc_attr", selected= tail(input$sc_attr,3))}
    if(length(input$io_attr) > 3) {updateCheckboxGroupInput(session, "io_attr", selected= tail(input$io_attr,3))}
  })
  
  output$segNum = renderText({
    segNum = length(unique(input_data$SEGMENT_EVENTID))
    paste(segNum)
  })
  
  output$segLength = renderText({
    segLength = round(sum(input_data$ENDMEASURE-input_data$BEGINMEASURE)*0.00018939,2)
    paste(segLength)
  })
  
  output$segNumLength = renderText({
    segNum = length(unique(input_data$SEGMENT_EVENTID))
    segLength = round(sum(input_data$ENDMEASURE-input_data$BEGINMEASURE)*0.00018939,2)
    paste(segNum, " (", segLength, " mi.)", sep="")
  })
  
  output$map = renderLeaflet({ 
    names(pipe_segment@data)
    pipepop = paste0(
      '<div class="popupItem"><strong>Pipe Segment ID: </strong>', pipe_segment@data$eventid,  '</div>',
      '<div class="popupItem"><strong>Pipeline Name: </strong>', pipe_segment@data$pipelnn, '</div>',
      '<div class="popupItem"><strong>Pipeline Long Name: </strong>', pipe_segment@data$longnam, '</div>',
      '<div class="popupItem"><strong>Begin station: </strong> ', pipe_segment@data$BEGINME," ft.",  '</div>',
      '<div class="popupItem"><strong>End station: </strong>', pipe_segment@data$ENDMEAS," ft.", '</div>',
      '<div class="popupItem"><strong>Length: </strong>', pipe_segment@data$ENDMEAS - pipe_segment@data$BEGINME," ft.", '</div>',
      '<div class="popupItem"><strong>EC Leak Risk: </strong>', round(pipe_segment@data$EC_POF_L,2), ' </div>', 
      '<div class="popupItem"><strong>EC Rupture Risk: </strong>', round(pipe_segment@data$EC_POF_R,2),  '</div>', 
      '<div class="popupItem"><strong>IC Leak Risk: </strong>', round(pipe_segment@data$IC_POF_L,2), ' </div>',
      '<div class="popupItem"><strong>IC Rupture Risk: </strong>', round(pipe_segment@data$IC_POF_R,2), ' in.</div>',
      '<div class="popupItem"><strong>MD Leak Risk: </strong>', round(pipe_segment@data$MD_POF_L,2), ' </div>',
      '<div class="popupItem"><strong>MD Rupture Risk: </strong>', round(pipe_segment@data$MD_POF_R,2), ' </div>',
      '<div class="popupItem"><strong>FAT Leak Risk: </strong>', round(pipe_segment@data$FAT_POF_L,2), ' </div>',
      '<div class="popupItem"><strong>FAT Rupture Risk: </strong>', round(pipe_segment@data$FAT_POF_R,2), ' </div>',
      '<div class="popupItem"><strong>SC Leak Risk: </strong>', round(pipe_segment@data$SC_POF_L,2), ' </div>',
      '<div class="popupItem"><strong>SC Rupture Risk: </strong>', round(pipe_segment@data$SC_POF_R,2), ' </div>',
      '<div class="popupItem"><strong>IO Leak Risk: </strong>', round(pipe_segment@data$IO_POF_L,2), ' </div>',
      '<div class="popupItem"><strong>IO Rupture Risk: </strong>', round(pipe_segment@data$IO_POF_R,2), ' </div>')
    leaflet(pipe_segment) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addProviderTiles("Stamen.TonerHybrid", group = "Toner Hybrid") %>%
      addPolylines(color="#666666", weight = 3, opacity=0.75, popup=pipepop, group = "Segments") %>%
      addLegend(position="topright",colors="#666666", labels="Columbia Midstream Group", opacity = 0.75)  %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Toner Hybrid"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  clusterResult = reactive({
    library(mclust)
    if(input$click_go>0){
      input$click_go
      withProgress(message="Running Clustering Algorithm", value=0, {
        for(i in 1:10){
          incProgress(1/10)
          Sys.sleep(0.10)
        }
        isolate({
          if(input$clust_set=="EXTERNAL.CORROSION"){
            input_name = c(paste(input$ec_attr,".JS",sep=""))
          } else if(input$clust_set=="INTERNAL.CORROSION"){
            input_name = c(paste(input$ic_attr,".JS",sep=""))
          } else if(input$clust_set=="MECHANICAL.DAMAGE"){
            input_name = c(paste(input$md_attr,".JS",sep=""))
          } else if(input$clust_set=="FATIGUE"){
            input_name = c(paste(input$fat_attr,".JS",sep=""))
          } else if(input$clust_set=="SOUR.CRACKING"){
            input_name = c(paste(input$sc_attr,".JS",sep=""))
          } else {input_name = c(paste(input$io_attr,".JS",sep=""))}
          data = input_data[which(names(input_data) %in% input_name)]
          result = Mclust(data)
          result
        })
      })
    }
  })
  
  resultData = reactive({
    if(input$click_go>0){
      input$click_go
      result = clusterResult()
      resultData = input_data
      resultData$cluster = result$classification
      numCluster = max(unique(resultData$cluster))
      resultData$LENGTH = resultData$ENDMEASURE - resultData$BEGINMEASURE 
      resultData
    }
  })
  
  observe({
    if(input$click_go>0){
      input$click_go
      isolate({
        data = resultData()
        clusterno = unique(data$cluster)
        clusterno = clusterno[order(clusterno)]
        #         clusterno = c(0, clusterno)
        updateSelectInput(session, "cluster_no", choices=clusterno)
      })
    }
  })
  
  output$selectCluster = renderText({
    if(input$click_go>0){
      input$click_go
      if(!is.null(input$cluster_no)) paste(input$cluster_no)
    }
  })
  
  output$clusterSegNumLength = renderText({
    if(input$click_go>0){
      input$click_go
      data = resultData()
      if(!is.null(input$cluster_no)){
        cluster_data = data[data$cluster==input$cluster_no,]
        clusterSegNum = length(unique(cluster_data$SEGMENT_EVENTID))
        clusterSegLength = round(sum(cluster_data$ENDMEASURE-cluster_data$BEGINMEASURE)*0.00018939,2)
        paste(clusterSegNum, " (", clusterSegLength, " mi.)", sep="")
      }
    }
  })
  
  output$noattr = renderPrint({
    if(input$click_go>0){
      input$click_go
      if(input$clust_set=="EXTERNAL.CORROSION"){
        cat(input$ec_attr,sep=", ")
      } else if(input$clust_set=="INTERNAL.CORROSION"){
        cat(input$ic_attr,sep=", ")
      } else if(input$clust_set=="MECHANICAL.DAMAGE"){
        cat(input$md_attr,sep=", ")
      } else if(input$clust_set=="FATIGUE"){
        cat(input$fat_attr,sep=", ")
      } else if(input$clust_set=="SOUR.CRACKING"){
        cat(input$sc_attr,sep=", ")
      } else {cat(input$io_attr,sep=", ")}
    }
  })
  
  output$nocluster = renderText({
    if(input$click_go>0){
      input$click_go
      data = resultData()
      nocluster = length(unique(data$cluster))
      paste(nocluster)
    }
  })  
  
  output$resultTableCluster = renderDataTable({
    if(input$click_go>0){
      input$click_go
      isolate({
        data = resultData()
        if(input$clust_set=="EXTERNAL.CORROSION"){
          input_name = c(paste(input$ec_attr,".JS",sep=""))
          riskscore_leak = "EC_POF_LEAK"
          riskscore_rup = "EC_POF_RUPTURE"
        } else if(input$clust_set=="INTERNAL.CORROSION"){
          input_name = c(paste(input$ic_attr,".JS",sep=""))
          riskscore_leak = "IC_POF_LEAK"
          riskscore_rup = "IC_POF_RUPTURE"
        } else if(input$clust_set=="MECHANICAL.DAMAGE"){
          input_name = c(paste(input$md_attr,".JS",sep=""))
          riskscore_leak = "MD_POF_LEAK"
          riskscore_rup = "MD_POF_RUPTURE"
        } else if(input$clust_set=="FATIGUE"){
          input_name = c(paste(input$fat_attr,".JS",sep=""))
          riskscore_leak = "FAT_POF_LEAK"
          riskscore_rup = "FAT_POF_RUPTURE"
        } else if(input$clust_set=="SOUR.CRACKING"){
          input_name = c(paste(input$sc_attr,".JS",sep=""))
          riskscore_leak = "SC_POF_LEAK"
          riskscore_rup = "SC_POF_RUPTURE"
        } else {
          input_name = c(paste(input$io_attr,".JS",sep=""))
          riskscore_leak = "IO_POF_LEAK"
          riskscore_rup = "IO_POF_RUPTURE"
        }
        scale_data =data[which(names(data) %in% input_name)]
        result = clusterResult()
        distMatrix = dist(scale_data)
        distMatrix = as.matrix(distMatrix/sqrt(ncol(scale_data)))
        stats = cluster.stats(distMatrix, data$cluster)
        cluster.no = unique(data$cluster)
        cluster.no = cluster.no[order(cluster.no)]
        cluster.size = ddply(data, c("cluster"), summarise, size=length(unique(SEGMENT_EVENTID)))
        cluster.size = cluster.size[order(cluster.size$cluster),]
        cluster.size = cluster.size$size
        cluster.length = ddply(data, c("cluster"), summarise, length=round(sum(LENGTH)*0.00018939,2))
        cluster.length = cluster.length[order(cluster.length$cluster),]
        cluster.length = c(cluster.length$length)
        average.distance = c(stats$average.distance)
        median.distance = c(stats$median.distance)
        diameter = c(stats$diameter)
        separation = c(stats$separation)
        average.toother = c(stats$average.toother)
        cluster.maxleak = c(aggregate(data[,riskscore_leak], list(data$cluster), max))$x
        cluster.maxrup = c(aggregate(data[,riskscore_rup], list(data$cluster), max))$x
        
        table = data.frame(CLUSTER=cluster.no,
                           NO.SEGMENT=cluster.size,
                           LENGTH.MILES=cluster.length,
                           MAX.LEAK.RISK=round(cluster.maxleak,2),
                           MAX.RUPTURE.RISK=round(cluster.maxrup,2),
                           MEAN.EUCLIDEAN.DISTANCE=round(-average.distance+1,2),
                           CLUSTER.DIAMETER=round(diameter,2),
                           CLUSTER.SEPARATION=round(separation,2),
                           CLUSTER.MEAN.TOOTHER=round(average.toother,2)
        )
        colnames(table) = c("Cluster","Segments","Length (mi.)","Maximum Leak Risk","Maximum Rupture Risk","Similarity","Diameter","Separation","MeanToOther")
        table
      })
    }
  }, options = list(lengthChange = FALSE, pageLength = 10, searching = FALSE),  rownames = FALSE)
  
  pipeData = reactive({
    if(input$click_go>0){
      input$click_go
      data = resultData()
      pipe_segment_df = read.csv("data/pipeData.csv")
      pipe_segment_df = pipe_segment_df[which(names(pipe_segment_df) %in% c("ROUTEEVENTID","BEGINMEASURE","ENDMEASURE","lon","lat"))]
      pipeData = merge(pipe_segment_df, data, by=c("ROUTEEVENTID","BEGINMEASURE","ENDMEASURE"))
      pipeData 
    } 
  })
  
  clusterPipeData = reactive({
    if(input$click_go>0){
      input$click_go
      pipeData = pipeData()
      clusterPipeData = pipeData[pipeData$cluster==input$cluster_no,]
      clusterPipeData
    }
  })
  
  output$clusterDetailTable = renderDataTable({
    if(input$click_go>0){
      input$click_go
      clusterPipeData = clusterPipeData()
      names(clusterPipeData)
      if(input$clust_set=="EXTERNAL.CORROSION"){
        feature_name = input$ec_attr
        featurejs_name = paste(input$ec_attr ,".JS",sep="")
        riskscore_name = c("EC_POF_LEAK","EC_POF_RUPTURE")
        riskscore_cols = c("EC POF (Leak)", "EC POF (Rupture)")
      } else if(input$clust_set=="INTERNAL.CORROSION"){
        feature_name = input$ic_attr
        featurejs_name = paste(input$ic_attr ,".JS",sep="")
        riskscore_name = c("IC_POF_LEAK","IC_POF_RUPTURE")
        riskscore_cols = c("IC POF (Leak)", "IC POF (Rupture)")
      } else if(input$clust_set=="MECHANICAL.DAMAGE"){
        feature_name = input$md_attr
        featurejs_name = paste(input$md_attr ,".JS",sep="")
        riskscore_name = c("MD_POF_LEAK","MD_POF_RUPTURE")
        riskscore_cols = c("MD POF (Leak)", "MD POF (Rupture)")
      } else if(input$clust_set=="FATIGUE"){
        feature_name = input$fat_attr
        featurejs_name = paste(input$fat_attr ,".JS",sep="")
        riskscore_name = c("FAT_POF_LEAK","FAT_POF_RUPTURE")
        riskscore_cols = c("FAT POF (Leak)", "FAT POF (Rupture)")
      } else if(input$clust_set=="SOUR.CRACKING"){
        feature_name = input$sc_attr
        featurejs_name = paste(input$sc_attr ,".JS",sep="")
        riskscore_name = c("SC_POF_LEAK","SC_POF_RUPTURE")
        riskscore_cols = c("SC POF (Leak)", "SC POF (Rupture)")
      } else {
        feature_name = input$io_attr
        featurejs_name = paste(input$io_attr ,".JS",sep="")
        riskscore_name = c("IO_POF_LEAK","IO_POF_RUPTURE")
        riskscore_cols = c("IO POF (Leak)", "IO POF (Rupture)")
      }
      data = clusterPipeData[,c("SEGMENT_EVENTID","LENGTH", riskscore_name, feature_name, featurejs_name)]
      data = data[!duplicated(data),]
      data = data[order(-data[,3]),]
      outlierTempData = data[,featurejs_name]
      for(i in 1:ncol(outlierTempData)) {
        outlierTempData[,i] = jitter(outlierTempData[,i])
      }
      outliers = mvoutlier::sign1(outlierTempData, makeplot=FALSE)
      data$OUTLIER.SCORE = round(unlist(mvoutlier::sign1(outlierTempData, makeplot=FALSE)$x.dist),4)
      #       colnames(data) = c("Segment ID","Length (ft.)","Similarity","Pressure","SMYS","Wall Thickness (in.)","Outside Diameter (in.)", "OSF", "Outlier Score")
      data = data[,c("SEGMENT_EVENTID","LENGTH",riskscore_name,feature_name,"OUTLIER.SCORE")]
      colnames(data) = c("Segment ID","Length (ft.)", riskscore_cols,feature_name,"Outlier Score")
      return(data)
    }
  }, options = list(lengthChange = FALSE, pageLength = 10, searching = FALSE), rownames = FALSE)
  
  
  output$clusterNoSeg = renderText({
    if(input$click_go>0){
      input$click_go
      data = resultData()
      if(!is.null(input$cluster_no)){
        if(input$cluster_no==0){
          cluster_data = data
        } else{
          cluster_data = data[data$cluster==input$cluster_no,]
        }
        clusterSegNum = length(unique(cluster_data$SEGMENT_EVENTID))
        clusterSegLength = round(sum(cluster_data$LENGTH)*0.00018939,2)
      }
      paste(clusterSegNum," (",clusterSegLength," mi.)")
    }
  })
  
  output$clusterDistance = renderText({
    if(input$click_go>0){
      input$click_go
      data = resultData()
      if(input$clust_set=="EXTERNAL.CORROSION"){
        input_name = c(paste(input$ec_attr,".JS",sep=""))
      } else if(input$clust_set=="INTERNAL.CORROSION"){
        input_name = c(paste(input$ic_attr,".JS",sep=""))
      } else if(input$clust_set=="MECHANICAL.DAMAGE"){
        input_name = c(paste(input$md_attr,".JS",sep=""))
      } else if(input$clust_set=="FATIGUE"){
        input_name = c(paste(input$fat_attr,".JS",sep=""))
      } else if(input$clust_set=="SOUR.CRACKING"){
        input_name = c(paste(input$sc_attr,".JS",sep=""))
      } else {input_name = c(paste(input$io_attr,".JS",sep=""))}
      scale_data = data[which(names(data) %in% input_name)]
      result = clusterResult()
      distMatrix = dist(scale_data)
      distMatrix = as.matrix(distMatrix/sqrt(ncol(scale_data)))
      stats = cluster.stats(distMatrix, data$cluster)
      if(input$cluster_no==0){
        paste("NA")
      } else{
        cluster_no = input$cluster_no
        average.distance = data.frame(stats$average.distance)[cluster_no,]
        paste(round(-average.distance+1,4))
      }
    }
  })
  
  observeEvent(input$cluster_no,{
    if(input$click_go>0){
      input$click_go
      clusterPipeData = clusterPipeData()
      clusterPipeData$SEGMENT_EVENTID = as.character(clusterPipeData$SEGMENT_EVENTID)
      clusterPolyLine = points_to_line(data=clusterPipeData, long="lon", lat="lat", id_field="SEGMENT_EVENTID")
      # Determine risk score label
      if(input$clust_set=="EXTERNAL.CORROSION"){
        riskscore_leak = "EC_POF_LEAK"
        riskscore_rup = "EC_POF_RUPTURE"
        riskscore_leak_sh = "EC_POF_L"
        riskscore_rup_sh = "EC_POF_R"
      } else if(input$clust_set=="INTERNAL.CORROSION"){
        riskscore_leak = "IC_POF_LEAK"
        riskscore_rup = "IC_POF_RUPTURE"
        riskscore_leak_sh = "IC_POF_L"
        riskscore_rup_sh = "IC_POF_R"
      } else if(input$clust_set=="MECHANICAL.DAMAGE"){
        riskscore_leak = "MD_POF_LEAK"
        riskscore_rup = "MD_POF_RUPTURE"
        riskscore_leak_sh = "MD_POF_L"
        riskscore_rup_sh = "MD_POF_R"
      } else if(input$clust_set=="FATIGUE"){
        riskscore_leak = "FAT_POF_LEAK"
        riskscore_rup = "FAT_POF_RUPTURE"
        riskscore_leak_sh = "FAT_POF_L"
        riskscore_rup_sh = "FAT_POF_R"
      } else if(input$clust_set=="SOUR.CRACKING"){
        riskscore_leak = "SC_POF_LEAK"
        riskscore_rup = "SC_POF_RUPTURE"
        riskscore_leak_sh = "SC_POF_L"
        riskscore_rup_sh = "SC_POF_R"
      } else {
        riskscore_leak = "IO_POF_LEAK"
        riskscore_rup = "IO_POF_RUPTURE"
        riskscore_leak_sh = "IO_POF_L"
        riskscore_rup_sh = "IO_POF_R"
      }
      
      pipepop = paste0(
        '<div class="popupItem"><strong>Leak Risk: </strong>', round(pipe_segment@data[,riskscore_leak_sh],2), ' </div>', 
        '<div class="popupItem" style="margin-bottom:10px"><strong>Rupture Risk: </strong>', round(pipe_segment@data[,riskscore_rup_sh],2),  '</div>',
        '<div class="popupItem"><strong>Pipe Segment ID: </strong>', pipe_segment@data$eventid,  '</div>',
        '<div class="popupItem"><strong>Pipeline Name: </strong>', pipe_segment@data$pipelnn, '</div>',
        '<div class="popupItem"><strong>Pipeline Long Name: </strong>', pipe_segment@data$longnam, '</div>',
        '<div class="popupItem"><strong>Begin station: </strong> ', pipe_segment@data$BEGINME," ft.",  '</div>',
        '<div class="popupItem"><strong>End station: </strong>', pipe_segment@data$ENDMEAS," ft.", '</div>',
        '<div class="popupItem"><strong>Length: </strong>', pipe_segment@data$ENDMEAS - pipe_segment@data$BEGINME," ft.", '</div>'
        
      )
      clusterpop = paste0(
        '<div class="popupItem"><strong>Cluster: </strong>', clusterPolyLine@data$cluster,  '</div>',
        '<div class="popupItem"><strong>Leak Risk: </strong>', round(clusterPolyLine@data[,riskscore_leak],2), ' </div>', 
        '<div class="popupItem" style="margin-bottom:10px"><strong>Rupture Risk: </strong>', round(clusterPolyLine@data[,riskscore_rup],2),  '</div>',
        '<div class="popupItem"><strong>Pipe Segment ID: </strong>', clusterPolyLine@data$SEGMENT_EVENTID, '</div>',
        '<div class="popupItem"><strong>Begin station: </strong> ', clusterPolyLine@data$BEGINMEASURE," ft.",  '</div>',
        '<div class="popupItem"><strong>End station: </strong> ', clusterPolyLine@data$ENDMEASURE," ft.", '</div>',
        '<div class="popupItem"><strong>Length: </strong> ',clusterPolyLine@data$LENGTH," ft.", '</div>',
        '<div class="popupItem"><strong>Pressure: </strong> ', round(clusterPolyLine@data$PRESSURE,2),  ' psig</div>',
        '<div class="popupItem"><strong>Pressure Range: </strong> ', round(clusterPolyLine@data$PRESSURERANGE,2),  ' </div>',
        '<div class="popupItem"><strong>SMYS: </strong> ', round(clusterPolyLine@data$SMYS,2),  '</div>',
        '<div class="popupItem"><strong>Wall thickness: </strong> ', round(clusterPolyLine@data$WALLTHICKNESS,2),  ' in.</div>',
        '<div class="popupItem"><strong>Outside diameter: </strong> ', round(clusterPolyLine@data$OUTSIDEDIAMETER,2),  ' in.</div>',
        '<div class="popupItem"><strong>Pipe Install Year: </strong> ', round(clusterPolyLine@data$PIPEINSTALLYEAR,2),  '</div>',
        '<div class="popupItem"><strong>Shared Row Count: </strong> ', round(clusterPolyLine@data$SHAREDROWCOUNT,2),  '</div>',
        '<div class="popupItem"><strong>Operating Training: </strong> ', round(clusterPolyLine@data$OPERATINGTRAINING,2),  '</div>',
        '<div class="popupItem"><strong>Pressure Control System: </strong> ', round(clusterPolyLine@data$PRESSURECONTROLSYS,2),  '</div>',
        '<div class="popupItem"><strong>SCADA System: </strong> ', round(clusterPolyLine@data$SCADASYSTEM,2),  '</div>',
        '<div class="popupItem"><strong>Has Previous EC Failure: </strong> ', round(clusterPolyLine@data$HASPREVIOUSECFAILURE,2),  '</div>',
        '<div class="popupItem"><strong>Has Previous IC Failure: </strong> ', round(clusterPolyLine@data$HASPREVIOUSICFAILURE,2),  '</div>'
      )
      
      #         leak_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = c(0,1))
      #         rup_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = c(0,1))
      #         leak_sh_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = c(0,1))
      #         rup_sh_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = c(0,1))  
      leak_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = clusterPolyLine@data[,riskscore_leak])
      rup_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = clusterPolyLine@data[,riskscore_rup])
      leak_sh_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = pipe_segment@data[,riskscore_leak_sh])
      rup_sh_pal = colorNumeric(palette = c("#0000FF","#FF0000"), domain = pipe_segment@data[,riskscore_rup_sh])
      
      # Set up a proxy for map updates
      proxy = leafletProxy("map", session, data = clusterPolyLine)
      
      # Clear the clusters layer before drawing the next one (since the map is not being redrawn)
      if(clusterCounter > 0) {
        proxy %>% 
          clearGroup("All Leak") %>%
          clearGroup("All Rupture") %>%
          clearGroup("Cluster Leak") %>%
          clearGroup("Cluster Rupture") %>%
          clearControls() %>%
          addLegend(position="topright",colors="#666666", labels="Columbia Midstream Group", opacity = 0.75)
      }
      
      # Add cluster layer to the map
      proxy %>% 
        addPolylines(data=clusterPolyLine, color=~leak_pal(clusterPolyLine@data[,riskscore_leak]), weight = 10, opacity=0.5, popup=clusterpop, group="Cluster Leak") %>%
        addPolylines(data=clusterPolyLine, color=~rup_pal(clusterPolyLine@data[,riskscore_rup]), weight = 10, opacity=0.5, popup=clusterpop, group="Cluster Rupture") %>%
        addPolylines(data=pipe_segment, color=~leak_sh_pal(pipe_segment@data[,riskscore_leak_sh]), weight = 10, opacity=0.5, popup=pipepop, group="All Leak") %>%
        addPolylines(data=pipe_segment, color=~rup_sh_pal(pipe_segment@data[,riskscore_rup_sh]), weight = 10, opacity=0.5, popup=pipepop, group="All Rupture") %>%
        showGroup("Cluster Leak") %>%
        hideGroup("Cluster Rupture") %>%
        hideGroup("All Leak") %>%
        hideGroup("All Rupture") %>%
        #         addLegend(position="bottomright", pal = leak_pal, values = ~clusterPolyLine@data[,riskscore_leak], title="Risk Score") %>%
        addLayersControl(
          baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Toner Hybrid"),
          overlayGroups = c("Segments", "All Leak","All Rupture", "Cluster Leak", "Cluster Rupture"),
          options = layersControlOptions(collapsed = FALSE)) 
      
      # Increment cluster count (for first pass)
      clusterCounter <<- clusterCounter + 1
    }
  })
  
  output$parcoord = renderChart({
    if(input$click_go>0){
      input$click_go
      if(!is.null(resultData())){
        data = resultData()
        if(input$clust_set=="EXTERNAL.CORROSION"){
          riskscore_leak = "EC_POF_LEAK"
          riskscore_rup = "EC_POF_RUPTURE"
          feature_name = input$ec_attr
        } else if(input$clust_set=="INTERNAL.CORROSION"){
          riskscore_leak = "IC_POF_LEAK"
          riskscore_rup = "IC_POF_RUPTURE"
          feature_name = input$ic_attr
        } else if(input$clust_set=="MECHANICAL.DAMAGE"){
          riskscore_leak = "MD_POF_LEAK"
          riskscore_rup = "MD_POF_RUPTURE"
          feature_name = input$md_attr
        } else if(input$clust_set=="FATIGUE"){
          riskscore_leak = "FAT_POF_LEAK"
          riskscore_rup = "FAT_POF_RUPTURE"
        } else if(input$clust_set=="SOUR.CRACKING"){
          riskscore_leak = "SC_POF_LEAK"
          riskscore_rup = "SC_POF_RUPTURE"
          feature_name = input$sc_attr
        } else {
          riskscore_leak = "IO_POF_LEAK"
          riskscore_rup = "IO_POF_RUPTURE"
          feature_name = input$io_attr
        }
        featureData = data[, c(riskscore_leak,riskscore_rup,feature_name)]
        featureData[is.na(featureData)] = 0
        maxRecord = c(apply(featureData, MARGIN=c(2), max))
        minRecord = c(apply(featureData, MARGIN=c(2), min))
        clusterFeatureData = data[data$cluster==input$cluster_no,]
        clusterFeatureData = clusterFeatureData[, c(riskscore_leak,riskscore_rup,feature_name)]
        clusterFeatureData[is.na(clusterFeatureData)] = 0
        clusterFeatureData = rbind(clusterFeatureData, maxRecord)
        clusterFeatureData = rbind(clusterFeatureData, minRecord)
        p1 = rCharts$new()
        p1$setLib("parcoords")
        p1$set(data = toJSONArray(clusterFeatureData, json=F), 
               colorby=riskscore_leak,
               range = range(clusterFeatureData[,riskscore_leak]),
               colors=c("#0000FF","#FF0000"),
               padding = list(top = 30, left = 0, bottom = 20, right = 0), 
               width="1030", 
               height="400")
        p1$set(dom = "parcoord")
        p1
      }
    } else{
      return(rCharts$new())
    }
  })
})
