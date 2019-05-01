## server.R

library(shiny)
library(shinydashboard)
library(data.table)
library(ggmap)
library(plotly)
library(fmsb)
library(RJSONIO)
library(geosphere)
library(purrr)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(tigris)
library(parcoords)
library(GGally)
library(htmltools)
library(tidyverse)
library(maptools)
library(sp)
library(rgdal)
library(choroplethrMaps)
library(choroplethr)
library(ggridges)
library(forcats)
library(GGally)

shinyServer(
  
  function(input, output) 
  {
    
    ################################################################
    ## input data
    ################################################################

    health<-read.csv("./data/Health.csv")
    health.state<-read.csv("./data/Health.State.csv")
    
    # county
    countycode<-substr(paste("00",health[,2],sep=''),nchar(paste("00",health[,2],sep=''))-2,nchar(paste("00",health[,2],sep='')))
    code<-as.numeric(paste(health[,1],countycode,sep=''))
    # demo
    
    county.ALE<-data.frame(region=code,value=health[,15])
    county.Poverty<-cbind.data.frame(region=code,value=health[,9])
    county.All_Death<-cbind.data.frame(region=code,value=health[,16]/health[,7]*100)
    county.Health_Status<-cbind.data.frame(region=code,value=health[,17])
    county.Unhealthy_Days<-cbind.data.frame(region=code,value=health[,18])
  
    #risk
    county.No_Exercise<-cbind.data.frame(region=code,value=health[,19])
    county.Few_Fruit_Veg<-cbind.data.frame(region=code,value=health[,20])
    county.Obesity<-cbind.data.frame(region=code,value=health[,21])
    county.High_Blood_Pres<-cbind.data.frame(region=code,value=health[,22])
    county.Smoker<-cbind.data.frame(region=code,value=health[,23])
    county.Diabetes<-cbind.data.frame(region=code,value=health[,24])
     
    #disease
    county.Brst_Cancer<-cbind.data.frame(region=code,value=health[,25]/health[,7]*100)
    county.Col_Cancer<-cbind.data.frame(region=code,value=health[,26]/health[,7]*100)
    county.CHD<-cbind.data.frame(region=code,value=health[,27]/health[,7]*100)
    county.Lung_Cancer<-cbind.data.frame(region=code,value=health[,28]/health[,7]*100)
    county.Stroke<-cbind.data.frame(region=code,value=health[,29]/health[,7]*100)
    
    
    #state
    #demo
    state.ALE<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,5])
    state.Poverty<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,4])
    state.All_Death<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,6])
    state.Health_Status<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,7])
    state.Unhealthy_Days<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,8])
    
    #risk
    state.No_Exercise<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,9])
    state.Few_Fruit_Veg<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,10])
    state.Obesity<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,11])
    state.High_Blood_Pres<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,12])
    state.Smoker<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,13])
    state.Diabetes<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,14])
    
    #disease
    state.CHD<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,17])
    state.Stroke<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,19])
    state.Lung_Cancer<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,18])
    state.Col_Cancer<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,16])
    state.Brst_Cancer<-transmute(health.state,region = tolower(health.state[,1]), value =health.state[,15])
    
    #Clevland
    tidydf<-gather(health.state, key = "Diseases", value = "Percentage", colnames(health.state%>%select(15:19)))
    
    
    ################################################################
    ## UI rendered
    ################################################################

    #state
    output$uiEda1 <- renderUI({
      
      selectInput( 'state.demo', 'Choose Information', 
                   choices = c("ALE","Poverty","All_Death","Health_Status","Unhealthy_Days" ), selected = "ALE")
      
    })
    
    
    
    output$uiEda2 <- renderUI({
      
      selectInput( 'state.risk', 'Choose Risk Factor', 
                   choices = c("No_Exercise", "Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes"), selected = "No_Exercise")
      
    })
    
    output$uiEda3 <- renderUI({
      
      selectInput( 'state.disease', 'Choose Disease', 
                   choices = c("Brst_Cancer","Col_Cancer","CHD","Lung_Cancer","Stroke"), selected = "CHD")
      
    })
    
    #county
    output$uiEda4 <- renderUI({
      
      selectInput( 'county.demo', 'Choose Information', 
                   choices = c("ALE","Poverty","All_Death","Health_Status","Unhealthy_Days" ), selected = "ALE")
      
    })
    
    
    
    output$uiEda5 <- renderUI({
      
      selectInput( 'county.risk', 'Choose Risk Factor', 
                   choices = c("No_Exercise", "Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes"), selected = "No_Exercise")
      
    })
    
    output$uiEda6 <- renderUI({
      
      selectInput( 'county.disease', 'Choose Disease', 
                   choices = c("Brst_Cancer","Col_Cancer","CHD","Lung_Cancer","Stroke"), selected = "CHD")
      
    })
    
    #clevland
    
    output$uiEda7 <- renderUI({
      
      selectInput( 'clevland', 'Choose Disease', 
                   choices = c("Brst_Cancer","Col_Cancer","CHD","Lung_Cancer","Stroke"), selected = "CHD")
      
    })

    #heatmap
    output$uiEda8 <- renderUI({
      
      selectInput( 'heatmap', 'Choose Risk Factor', 
                   choices = c("No_Exercise", "Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes"), selected = "No_Exercise")
      
    })
    
    
    
    ###map.state
  
    output$demo.state <- renderPlot({
      if(input$state.demo=="ALE"){state_choropleth(state.ALE, 
                                                    title  = "County Data, Average Life Expectancy (ALE)",  
                                                    legend = "ALE")}
      else if(input$state.demo=="Poverty"){state_choropleth(state.Poverty, 
                                                            title  = "County Data, Poverty", 
                                                            legend = "Poverty")}
      else if(input$state.demo=="All_Death"){ state_choropleth(state.All_Death, 
                                                               title  = "County Data, All_Death", 
                                                               legend = "All_Death")}
      else if(input$state.demo=="Health_Status"){state_choropleth(state.Health_Status, 
                                                                  title  = "County Data, Health_Status", 
                                                                  legend = "Health_Status")}
      else if(input$state.demo=="Unhealthy_Days"){state_choropleth(state.Unhealthy_Days, 
                                                                   title  = "County Data, Unhealthy_Days", 
                                                                   legend = "Unhealthy_Days")}
    })
    
    output$risk.state <- renderPlot({
      if(input$state.risk=="Obesity"){state_choropleth(state.Obesity, 
                                                       title  = "State Data, Obesity", 
                                                       legend = "Obesity")}
      else if(input$state.risk=="Few_Fruit_Veg"){state_choropleth(state.Few_Fruit_Veg, 
                                                                  title  = "State Data, Few_Fruit_Veg", 
                                                                  legend = "Few_Fruit_Veg")}
      else if(input$state.risk=="No_Exercise"){state_choropleth(state.No_Exercise, 
                                                                title  = "State Data, No_Exercise", 
                                                                legend = "No_Exercise")}
      else if(input$state.risk=="High_Blood_Pres"){state_choropleth(state.High_Blood_Pres, 
                                                                    title  = "State Data, High_Blood_Pres", 
                                                                    legend = "High_Blood_Pres")}
      else if(input$state.risk=="Smoker"){state_choropleth(state.Smoker, 
                                                           title  = "State Data,Smoker", 
                                                           legend = "Smoker")}
      else if(input$state.risk=="Diabetes"){state_choropleth(state.Diabetes, 
                                                             title  = "State Data, Diabetes", 
                                                             legend = "Diabetes")} 
    })

    
    output$disease.state <- renderPlot({
      if(input$state.disease=="Brst_Cancer"){state_choropleth(state.Brst_Cancer, 
                                                                title  = "County Data, Brst_Cancer", 
                                                                legend = "Brst_Cancer")}
      else if(input$state.disease=="Col_Cancer"){state_choropleth(state.Col_Cancer, 
                                                                    title  = "County Data, Col_Cancer", 
                                                                    legend = "Col_Cancer")}
      else if(input$state.disease=="CHD"){state_choropleth(state.CHD, 
                                                             title  = "County Data, CHD", 
                                                             legend = "CHD")}
      else if(input$state.disease=="Lung_Cancer"){state_choropleth(state.Lung_Cancer, 
                                                                     title  = "County Data, Lung_Cancer", 
                                                                     legend = "Lung_Cancer")}
      else if(input$state.disease=="Stroke"){state_choropleth(state.Stroke, 
                                                                title  = "County Data, Stroke", 
                                                                legend = "Stroke")}
      
    })
    
    
    ###map.county
    output$demo.county <- renderPlot({
      if(input$county.demo=="ALE"){county_choropleth(county.ALE, 
                                            title  = "County Data, Average Life Expectancy (ALE)", 
                                            legend = "ALE")}
      else if(input$county.demo=="Poverty"){county_choropleth(county.Poverty, 
                                              title  = "County Data, Poverty", 
                                              legend = "Poverty")}
      else if(input$county.demo=="All_Death"){ county_choropleth(county.All_Death, 
                                                               title  = "County Data, All_Death", 
                                                               legend = "All_Death")}
      else if(input$county.demo=="Health_Status"){county_choropleth(county.Health_Status, 
                                                                  title  = "County Data, Health_Status", 
                                                                  legend = "Health_Status")}
      else if(input$county.demo=="Unhealthy_Days"){county_choropleth(county.Unhealthy_Days, 
                                                                title  = "County Data, Unhealthy_Days", 
                                                                legend = "Unhealthy_Days")}
    })
    
    output$risk.county <- renderPlot({
      if(input$county.risk=="Obesity"){county_choropleth(county.Obesity, 
                                               title  = "State Data,Obesity", 
                                               legend = "Obesity")}
      else if(input$county.risk=="Few_Fruit_Veg"){county_choropleth(county.Few_Fruit_Veg, 
                                                            title  = "State Data, Few_Fruit_Veg", 
                                                            legend = "Few_Fruit_Veg")}
      else if(input$county.risk=="No_Exercise"){county_choropleth(county.No_Exercise, 
                                                          title  = "State Data, No_Exercise", 
                                                          legend = "No_Exercise")}
      else if(input$county.risk=="High_Blood_Pres"){county_choropleth(county.High_Blood_Pres, 
                        title  = "County Data, High_Blood_Pres", 
                        legend = "High_Blood_Pres")}
      else if(input$county.risk=="Smoker"){county_choropleth(county.Smoker, 
                                                                      title  = "County Data, Smoker", 
                                                                      legend = "Smoker")}
      else if(input$county.risk=="Diabetes"){county_choropleth(county.Diabetes, 
                                                                              title  = "County Data, Diabetes", 
                                                                              legend = "Diabetes")} 
      })
  
    output$disease.county <- renderPlot({
      if(input$county.disease=="Brst_Cancer"){county_choropleth(county.Brst_Cancer, 
                      title  = "County Data, Brst_Cancer", 
                      legend = "Brst_Cancer")}
      else if(input$county.disease=="Col_Cancer"){county_choropleth(county.Col_Cancer, 
                                                                    title  = "County Data, Col_Cancer", 
                                                                    legend = "Col_Cancer")}
      else if(input$county.disease=="CHD"){county_choropleth(county.CHD, 
                      title  = "County Data, CHD", 
                      legend = "CHD")}
      else if(input$county.disease=="Lung_Cancer"){county_choropleth(county.Lung_Cancer, 
                                                                  title  = "County Data, Lung_Cancer", 
                                                                  legend = "Lung_Cancer")}
      else if(input$county.disease=="Stroke"){county_choropleth(county.Stroke, 
                                                                    title  = "County Data, Stroke", 
                                                                    legend = "Stroke")}
    })

    
    
    
    # clavland plot
    output$clevland.plot<-renderPlot({
      ggplot(tidydf, aes(Percentage, fct_reorder2(`CHSI_State_Name`, Diseases==input$clevland, Percentage, .desc = FALSE), color = Diseases)) +
        geom_point() +
        ggtitle("Diseases Information - State Level") + ylab("") 
    })
    
    #heatmap
    output$heat.map<-renderPlotly({ 
      layout(add_histogram2d(plot_ly(x=health[,"ALE"],
                                     y=health[,input$heatmap]),nbinsx=30, nbinsy=30),
        title = "Risk Factors VS. Average Life Expectancy",
        xaxis = list(title = "Life Expectancy"),
        yaxis = list(title = "Risk Factors")
      )})
    
  }
)

