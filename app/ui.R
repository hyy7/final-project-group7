## ui.R

library(shiny)
library(shinydashboard)
library(data.table)
library(ggmap)
library(leaflet)
library(plotly)
library('RJSONIO')
library('geosphere')
library('purrr')
library(tidyverse)
library(leaflet)
library(geojsonio)
library(tigris)
library(parcoords)
library(GGally)
library(htmltools)
library(fmsb)
library(sp)
library(rgdal)
library(choroplethrMaps)
library(choroplethr)
library(ggridges)
library(forcats)
library(GGally)

dashboardPage(
  
  ## dashboard header
  dashboardHeader( title = "Community Health Study in U.S." ),
  
  ## dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      
      ################################################################
      ## Introduction tab side
      ################################################################
      menuItem("Introduction", tabName = "intro"),

      menuItem("Map",  tabName = "stats",
               menuSubItem('State Level',
                           tabName = 'state' ),
               menuSubItem('County Level',
                           tabName = 'county')),
      
      menuItem("Clevland Plot",  tabName = "clev"),
      
      menuItem("Heatmap Plot",  tabName = "heatmap"),
     
      ################################################################
      ## Contact tab side
      ################################################################
      # create Data Tab with subitems
      menuItem("Contact us", tabName = "contact")
      
      
    )),
  
  ## dashboard body
  dashboardBody(

    tabItems(
      
      ################################################################
      ## Introduction tab body
      ################################################################
      # Introduction tab content
      tabItem(tabName = "intro",
              
              h2("Welcome", align = "center"),
              h3(""),
             h3(""),
              h3("Our topic is related to community health: "),
              h3(""),
              h4("How people can improve quality and length of their lives. In particular, we want to study how Behavioral factors, Cause of Death, Poverty level, and life expectancy are related, using 3141 counties in 51 states. "),
             h3(""),
             h4("In addition, we also compared the the conditions among different states and counties. We hope to provide some useful guidence of how people can live healthier and longer."),
              h3("")
                 
              
              ),
    
      
      ################################################################
      ## plot tab body
      ################################################################
      tabItem(tabName = "state",
              
              h2("Analysis for State Level"),
              position = "right",
              tabPanel("map", 
                       fluidRow(
                         h3("Information - State Level"),uiOutput("uiEda1"),plotOutput("demo.state"),
                         h3("Risk Factor - State Level"),uiOutput("uiEda2"),plotOutput("risk.state"),
                         h3("Disease - State Level"),uiOutput("uiEda3"),plotOutput("disease.state")))
              
      ),
      
      tabItem(
              tabName = "county",
              h2("Analysis for County Level"),
              position = "right",
              tabPanel("map", 
                       fluidRow(
                       h3("Information - County Level"),
                        uiOutput("uiEda4"),plotOutput("demo.county"),
                    
                       h3("Risk Factor - County Level"),   
                       uiOutput("uiEda5"),plotOutput("risk.county"),
                     
                       h3("Disease - County Level"),  
                       uiOutput("uiEda6"),plotOutput("disease.county")))
              ),
      
      tabItem(
        
        tabName = "clev",
        h2("Diseases Information - State Level"),
        position = "right",
        tabPanel("clevland", 
                 fluidRow( 
                           h3("Clevland Plot"),uiOutput("uiEda7"),plotOutput("clevland.plot")
                           ))
      ),
      
      tabItem(tabName = "heatmap",
              h2("Risk Factor V.S. Average Life Expectancy (ALE)"),
              tabPanel("heatmap", 
                       fluidRow( 
                                 h3("Heatmap"),uiOutput("uiEda8"),plotlyOutput("heat.map")
                       ))
            ),
     
      
      ################################################################
      ## Contact  tab body
      ################################################################
      # Introduction tab content
      tabItem(tabName = "contact",
              
              h2("Contact us"),
              
              h3( "We are Group 7!"),
            
              h5("Hu, Yiyao yh3076@columbia.edu"),
              h5("Wang, Yakun yw3211@columbia.edu")
           
      )
      
    )
))
