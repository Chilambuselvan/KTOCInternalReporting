#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
x <- list(title = "Month",titlefont = f,tickmode = "array",tickvals = 1:12,
          ticktext = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
pal <- colorFactor(c("navy", "green"), domain = c("SAP", "KTOC"))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ##################################################KTOC Overall##############
  output$KTOCplatformNEB=renderPlotly({
    plot_ly(x = KTOCPlatforms$YEAR, y=KTOCPlatforms$EU,name="Europe Platforms", line = list(shape = "spline"))%>%
      add_trace(x = KTOCPlatforms$YEAR, y=KTOCPlatforms$ENA,name="ENA Platforms", line = list(shape = "spline")) %>%
      add_trace(x = KTOCPlatforms$YEAR, y=KTOCPlatforms$SOC,name="SOC Platforms", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "YEAR", titlefont = f),yaxis=list(title = "No. of platforms", titlefont = f))%>%
      layout(legend = list(x = 10, y = max(KTOCPlatforms$SOC)))%>%
      layout(title="KTOC NEB / FRB Platforms")
  })
  
  output$KTOCplatformSEB=renderPlotly({
    plot_ly(x = KTOCPlatforms$YEAR, y=KTOCPlatforms$TRBSOF,name="Europe Platforms", line = list(shape = "spline"))%>%
      add_trace(x = KTOCPlatforms$YEAR, y=KTOCPlatforms$TRBSOC,name="SOC Platforms", line = list(shape = "spline")) %>%
      add_trace(x = KTOCPlatforms$YEAR, y=KTOCPlatforms$TRBSOA,name="SOA Platforms", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "YEAR", titlefont = f),yaxis=list(title = "No. of platforms", titlefont = f))%>%
      layout(legend = list(x = 10, y = max(KTOCPlatforms$TRBSOF)))%>%
      layout(title="KTOC SEB Platforms")
  })
  ##################################################WorkLoad Summary##############
  output$WorkloadLineChart=renderPlotly({
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOI,name="SOI Actual Load", line = list(shape = "spline"))%>%
      #add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOA,text=KTOCLoad$SOA,showlegend=FALSE,hoverinfo='none',textposition="top left", mode="text") %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOA,name="SOA Actual Load", line = list(shape = "spline")) %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOC,name="SOC Actual Load", line = list(shape = "spline")) %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$TRB,name="TRB Actual Load", line = list(shape = "spline")) %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$ESC,name="ESC Actual Load", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD KTOC VC")
  })
  output$WorkloadStackChart=renderPlotly({
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOI,name="SOI Actual Load", type="bar")%>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOA,name="SOA Actual Load", type="bar") %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOC,name="SOC Actual Load", type="bar") %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$TRB,name="TRB Actual Load", type="bar") %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$ESC,name="ESC Actual Load", type="bar") %>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$OverAllActual,name="Total Resource", line = list(color="blue",shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(barmode="stack" ,legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD KTOC VC")
  })
  ##################################################KONE VC Locations##############
  output$mapKONEVC = renderLeaflet({
    require(leaflet)
    m=leaflet(VCLocations) %>%
      addTiles() %>%
      setView(lng=mean(VCLocations$lng),lat=mean(VCLocations$lat), zoom=3)%>%
      #addMarkers(clusterOtions=markerClusterOptions())%>%
      addCircleMarkers(data = VCLocations, lng = ~ lng, lat = ~ lat, radius = 5, 
                       color = ~ ifelse(Departmant == 'SAP', 'Green', 'blue'),
                       clusterOptions = markerClusterOptions())%>%
      #addCircleMarkers(radius=~count*2,color="green",stroke = FALSE, fillOpacity = 0.5)%>%
      addCircles(popup= ~Label)
    m
  })
  ##################################################Query WorkLoad##############
  output$WorkloadLineChartQue=renderPlotly({
    # SelStream = as.list(c("SOI"))
    if (!is.null(input$Stream)) {
      SelStream = as.list(input$Stream)
    }
    
    
    
    #add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOA,name="SOA Actual Load", line = list(shape = "spline")) %>%
    # add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOC,name="SOC Actual Load", line = list(shape = "spline")) %>%
    # add_trace(x = KTOCLoad$Week, y=KTOCLoad$TRB,name="TRB Actual Load", line = list(shape = "spline")) %>%
    #   add_trace(x = KTOCLoad$Week, y=KTOCLoad$ESC,name="ESC Actual Load", line = list(shape = "spline")) %>%
    
    p=plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOI,name= "SOI Actual Load", line = list(shape = "spline"))
    
    for (item in SelStream) {
      
      p <- add_trace(p, x = KTOCLoad$Week, y = KTOCLoad[[item]],name= paste(KTOCLoad[[item]]," Actual Load"), evaluate = TRUE, line = list(shape="spline"))
    }
    
    #  p=layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
    # p= layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
    #  p=layout(title="WORK LOAD KTOC VC")
    print(p)
  })
  ##################################################Workload Streamwise##############
  output$WorkloadLineChartSOI=renderPlotly({
    #plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOI,name="SOI used resource",mode="line",line = list(shape = "spline"))%>%
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOI,name="SOI used resource",type="bar")%>% 
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOIActual,name="SOI Acutal Resource", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD SOI")
  })
  output$WorkloadLineChartSOA=renderPlotly({
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOA,name="SOA used resource", type="bar")%>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOAActual,name="SOA Acutal Resource", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD SOA")
  })
  output$WorkloadLineChartSOC=renderPlotly({
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$SOC,name="SOC used resource", type="bar")%>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$SOCActual,name="SOC Acutal Resource", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD SOC")
  })
  output$WorkloadLineChartTRB=renderPlotly({
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$TRB,name="TRB used resource", type="bar")%>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$TRBActual,name="TRB Acutal Resource", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD TRB")
  })
  output$WorkloadLineChartESC=renderPlotly({
    plot_ly(x = KTOCLoad$Week, y=KTOCLoad$ESC,name="ESC used resource", type="bar")%>%
      add_trace(x = KTOCLoad$Week, y=KTOCLoad$ESCActual,name="ESC Acutal Resource", line = list(shape = "spline")) %>%
      layout(xaxis=list(title = "Week", titlefont = f),yaxis=list(title = "Resource", titlefont = f))%>%
      layout(legend = list(x = 30, y = max(KTOCLoad$SOC)))%>%
      layout(title="WORK LOAD ESC")
  })
  ##################################################KTOC Overall##############
  output$Primarymetric =renderPlotly({
    #str(PerfTracker$Task.Handled.On)
    subdf=PerfTracker%>%filter("CA4" %in% PerfTracker$Server)%>%
      group_by(Month,Year)%>%summarise(cnt=n())
    plot_ly(x = subdf$Month, y=subdf$cnt,name="ESC used resource",group = subdf$Year, line = list(shape = "linear"))%>%
      add_trace(x = subdf$Month, y=rep.int(3.1245,12),name="ESC Acutal Resource", line = list(shape = "linear")) %>%
      layout(xaxis=x,yaxis=list(title = "Count", titlefont = f,autotick=FALSE,tick0=1))%>%
      layout(title="Primary Metrics")
  })
})
