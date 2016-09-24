#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(googleVis)
library(XLConnect)
library(tidyr)

myvar=0
if(myvar==1){

  setwd("F:/Analytics/KTOCInternalReporting/KTOCReport")
  Res=loadWorkbook("Resource.xlsx")
  VCResource = readWorksheet(Res, sheet = 1, header = TRUE)
  VCOrderHist = readWorksheet(Res, sheet = 2, header = TRUE)
  VCsummary=left_join(VCOrderHist,VCResource,by=c("Year"="Year"))
  
  wb = loadWorkbook("KTOCteamReport.xls")
  KTOCTaskTracker = readWorksheet(wb, sheet = 1, header = TRUE)
  Pt = loadWorkbook("performanceTracker.xlsx")
  PerfTracker = readWorksheet(Pt, sheet = 3, startRow = 6, header = TRUE)
  PerfTracker$Month=month(as.POSIXlt(PerfTracker$Task.Handled.On, format="%Y-%m-%d"))
  PerfTracker$Year=year(as.POSIXlt(PerfTracker$Task.Handled.On, format="%Y-%m-%d"))
  KTOCLoad=fread("Y2016Load.csv", stringsAsFactors = FALSE, header= TRUE)
  KTOCPlatforms=fread("KTOCPlatforms.csv", stringsAsFactors = FALSE, header= TRUE)
  VCLocations=fread("VCLocations.csv", stringsAsFactors = FALSE, header= TRUE)
}

f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
x <- list(title = "Month",titlefont = f,tickmode = "array",tickvals = 1:12,
          ticktext = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

VCLocations= data.frame(VCLocations)

Streamvars=c("SOC","SOA","TRB","ESC")
# Define UI for application that draws a histogram
shinyUI(fluidPage(title = "KTOC Analytics",
                  tabsetPanel(
                    tabPanel(title = "KTOC OverAll",
                             #plotlyOutput("Wherewearechart"),
                             plotlyOutput("KTOCplatformNEB"),
                             plotlyOutput("KTOCplatformSEB")
                    ),
                    tabPanel(title = "WorkLoad",
                             plotlyOutput("WorkloadLineChart"),
                             plotlyOutput("WorkloadStackChart")
                    ),
                    tabPanel(title = "Query WorkLoad",
                             sliderInput(inputId = "Week", label = "Rated Load", min=min(KTOCLoad$Week), max = max(KTOCLoad$Week),value = 1),
                             selectInput(inputId = "Stream","Choose Streams",Streamvars,multiple = TRUE),
                             plotlyOutput("WorkloadLineChartQue")
                    ),
                    tabPanel(title = "Map Interactive",
                             leafletOutput("mapKONEVC")
                    ),
                    tabPanel(title = "StreamWise",
                             plotlyOutput("WorkloadLineChartSOI"),
                             plotlyOutput("WorkloadLineChartSOA"),
                             plotlyOutput("WorkloadLineChartSOC"),
                             plotlyOutput("WorkloadLineChartTRB"),
                             plotlyOutput("WorkloadLineChartESC")
                    ),
                    tabPanel(title = "Six Sigma Reporting",
                             plotlyOutput("Primarymetric"),
                             plotlyOutput("SecMetric")
                    ),
                    tabPanel(title = "Gvis",
                             htmlOutput("motionCharts")
                    )
                  )
)
)
