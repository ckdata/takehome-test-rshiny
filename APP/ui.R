# Load libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Plot libraries for outputs
library(plotly)
library(rAmCharts)


shinyUI( dashboardPage( skin ="black",
  dashboardHeader(title= "Customer Advertisement Analysis"
                  ,titleWidth=400),
                  
  
  dashboardSidebar(
    width = 300,
    
    
    h2("Interactive Selection",align="center"),
   h4("View as per Selections",align="center"),
    uiOutput("yearSlider"),
    
    
    
    sidebarMenu(
      selectInput("cat", 
                 "Accounts",selected = "Grece Foundation",
                choice=c("Capitol Hill United","Cargill Entertainment", "Great Rants","Grece Foundation"
                         ,"Heschsoft","Liberty House","Loto Paper Laundry","Narisoft"
                         ,"New Belgium Party","NY Climate Collective","Pierce Associates","Sound Green Toronto"
                         ,"Spartan Consulting","Ted Hansen Campaign","The Taylor Project","Town Of Scottsdale")
                ,multiple = TRUE
    ),
    uiOutput("cat2"),
    
    p(actionButton(inputId = "reset", 
                   label = "Reset Fields", 
                   icon = icon("refresh")
    ), align = "center")
    
    
    )
    
    
  ),
  dashboardBody(
    
    tabsetPanel(
      id = 'tabs'
      ,
      tabPanel(
        title =p(icon("area-chart"), "Analysis"),
        value = "page1",
        fluidRow( 
          valueBoxOutput("value1"), #cost
          valueBoxOutput("value2"),#Impressions
          valueBoxOutput("value3"), #post reactions
          valueBoxOutput("value4"), #post comments
          valueBoxOutput("value5"), #post share
          valueBoxOutput("value6") #Sentiments
          ),
        fluidRow(
          box(width=4,h3(tags$b("Costs % Share by Adv 
                                Created Category"),style = "font-weight: 500; color: #4d3a7d;", align="center"),
                 p( 
                 amChartsOutput(outputId = "pie1",width = "100%") )
                 
                 )
          ,
          
          box(width = 4, h3(tags$b("Impressions % Share by Adv Created Category"), style = "font-weight: 500; color: #4d3a7d;",align="center"),
                 p( #plotOutput("pie2") )
                   amChartsOutput(outputId = "pie2",width  =  "100%")
                 )
          )
              
              ,
              box(width = 4, h3(tags$b("Gender % Share"), style = "font-weight: 500; color: #4d3a7d;",align="center"),
                  p(
                    
                    amChartsOutput(outputId = "pie3",width = "100%"))
              )
                  
        ),
        fluidRow(
          
          box(width=8, h2(tags$b("Impressions by Ad Created Category and CTA type"),style = "font-weight: 500; color: #4d3a7d;", align="center")
              ,
              
              p(
                plotlyOutput("stackplot")
              )
              
          ),
          
          box(width=4, h2(tags$b("Focused Topics"),style = "font-weight: 500; color: #4d3a7d;", align="center")
              ,
              
              p(
                plotOutput("wplot")
              )
              
              )
          
        )
        
        )
      
        )
          )
      )
)
