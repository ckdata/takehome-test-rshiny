# Load libraries ---------------------------------------------------------------
library(shiny)
require(shinydashboard)

# Data read & manipulation libraries
library(dplyr)
library(lubridate)
library(rJava)
library(xlsx)
library(scales)

# Visualization libraries
library(plotly)
library(rAmCharts)
library(wordcloud)
library(RColorBrewer)



valueBox2 <- function (value, subtitle, icon = NULL, backgroundColor = "#7cb5ec", textColor = "#FFF", width = 4, href = NULL)
{
  
  boxContent <- div(
    class = paste0("small-box"),
    style = paste0("background-color: ", backgroundColor, "; color: ", textColor, ";"),
    div(
      class = "inner",
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) {
      div(class = "icon-large", icon)
    }
  )
  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }
  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

valueBox3 <- function (value, subtitle, icon = NULL, backgroundColor = "#7cb5ec", textColor = "#FFF", width = 4, href = NULL)
{
  
  boxContent <- div(
    class = paste0("small-box"),
    style = paste0("background-color: ", backgroundColor, "; color: ", textColor, ";"),
    div(
      class = "inner",
      h4(tags$b(value)),
      p(subtitle)
    ),
    if (!is.null(icon)) {
      div(class = "icon-large", icon)
    }
  )
  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }
  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}



#setwd("C:\\Users\\Rishabh\\Documents\\R dash\\APP2\\data")

colClass=c("numeric","character","Date",rep("character",3),rep("numeric",7),"character","numeric","character","character"
           ,rep("numeric",4),rep("character",3),"numeric","character",rep("numeric",3),"character")

df <- read.csv("data/content_library_anonymised.csv",header=TRUE,colClasses = colClass)
df$Year<- format(as.Date(as_date(df$Promoted.post.created.date), format="%m/%d/%Y"),"%Y")


df$Cost <- as.integer(round(df$Cost,0))
df$Year <- as.integer(df$Year)
yearmax <- max(df$Year)
yearmin <- min(df$Year)



cat <- sort(unique(df$Ad.Account.Anonymised))
cat2 <-  levels(as.factor(df$Ad.creative.object.type))


shinyServer(function(input, output, session){
  
  ##Adding control widgets
  output$yearSlider <- renderUI({
    sliderInput(inputId = "year.in"
                ,label="Year Filter"
                ,min =yearmin
                ,max =yearmax
                ,step=1,
                value=c(yearmin,yearmax)
                ,ticks = T
                ,sep=""
                                )
  })
  
  
  output$cat2 <- renderUI({
    
    checkboxGroupInput('cat2', label = "Type of Created Advertisement",
                       choices = cat2, selected = cat2)
    
  })
  
  
  

  observe({
    if(input$reset != 0) {
      updateSliderInput(session, "year.in", val = c(yearmin, yearmax))
      updateSelectInput(session,"cat",choices = cat, selected = "Grece Foundation")
      updateCheckboxGroupInput(session,"cat2",choices=cat2,selected=cat2)
    }
    
  })
  
#Reactivity###################################################################
 
  #Filtering data as per selection
    df.filtered <- reactive({
      req(input$year.in)
     req(input$cat)
     req(input$cat2)
     # Error handling 
    if (is.null(input$year.in) | 
        is.null(input$cat)  |
        is.null(input$cat2)
            ) {
      return(NULL)
    } 
    
      filter(df, Ad.Account.Anonymised %in%  c(input$cat,'')) %>%
        filter(Ad.creative.object.type %in% c(input$cat2,'')) %>%
              filter(Year>=   as.integer(input$year.in[1]),Year<=  as.integer(input$year.in[2]))
           
  })
    
  
  Impression <- reactive({
     df.filtered() %>%
     group_by(Year,Ad.Account.Anonymised,Ad.creative.object.type
              ) %>%
    summarize(impressions.total   = sum(Impressions),reaction.total =sum(Post.reactions)
              ,comments.total =sum(Post.comments),
              share.total =sum(Post.shares))
      })
  
  costs <- reactive({
   df.filtered() %>%
      group_by(Year, Ad.Account.Anonymised,Ad.creative.object.type ) %>%
      summarize(cost.total = sum(Cost))
  })
  sentiments <- reactive({
    
    y <- df.filtered() %>%
      count(Year,Ad.Account.Anonymised,Ad.creative.object.type,Sentiment)
    x <- df.filtered() %>%
      count(Year,Ad.Account.Anonymised,Sentiment)
    f <- merge(y,x,by=c("Year","Ad.Account.Anonymised","Sentiment"))
    f1 <-  f %>% group_by(Sentiment
    ) %>%
      summarize(catsum=sum(n.x))
    f1$values <- round((f1$catsum/sum(f1$catsum))*100,2)
    colnames(f1)[1] <- "Sentiments"
    f1 <- f1[,c(1,3)]
    b <- filter(f1,f1$Sentiments=="Negative")$values
    a<- filter(f1,f1$Sentiments=="Positive")$values
    c <- filter(f1,f1$Sentiments=="Neutral")$values
    paste0("Positive:",a,"%"," ","Negative:",b,"%"," ","Neutral:",c,"%")
  })
  
  piedata1 <- reactive({
    y <-  df.filtered() %>% group_by(Year,Ad.Account.Anonymised,Ad.creative.object.type
    ) %>% 
      summarize(total.cost =sum(Cost))
    x <- df.filtered() %>% group_by(Year,Ad.Account.Anonymised) %>% 
      summarize(total.cost =sum(Cost))
    f <- merge(y,x,by=c("Year","Ad.Account.Anonymised"))
    
    f1 <-  f %>% group_by(Ad.creative.object.type
    ) %>%
      summarize(catsum=sum(total.cost.x))
    
    f1$value <- round((f1$catsum/sum(f1$catsum))*100,2)
    colnames(f1)[1] <- "label"
    f1 <- f1[,c(1,3)]
    f1
  }) 
  piedata2 <- reactive({
    y <-df.filtered() %>% group_by(Year,Ad.Account.Anonymised,Ad.creative.object.type
    ) %>% 
      summarize(total.Impression =sum(Impressions))
    x <- df.filtered() %>% group_by(Year,Ad.Account.Anonymised) %>% 
      summarize(total.Impression =sum(Impressions))
    f <- merge(y,x,by=c("Year","Ad.Account.Anonymised"))
    
    f1 <-  f %>% group_by(Ad.creative.object.type
    ) %>%
      summarize(catsum=sum(total.Impression.x))
    
    f1$value <- round((f1$catsum/sum(f1$catsum))*100,2)
    colnames(f1)[1] <- "label"
    f1 <- f1[,c(1,3)]
    f1
  })
  piedata3 <-  reactive({
    ##Gender
    y <- df.filtered() %>%
      count(Year,Ad.Account.Anonymised,Ad.creative.object.type,What.about.the.person.s...gender.)
    x <- df.filtered() %>%
      count(Year,Ad.Account.Anonymised,What.about.the.person.s...gender.)
    f <- merge(y,x,by=c("Year","Ad.Account.Anonymised","What.about.the.person.s...gender."))
    f1 <- f %>% group_by(What.about.the.person.s...gender.
    ) %>%
      summarize(catsum=sum(n.x))
    f1$value <- round((f1$catsum/sum(f1$catsum))*100,2)
    colnames(f1)[1] <- "label"
    f1[,c(1,3)]
      })
  wdata <-  reactive({
    
    
    x <-  df.filtered() %>% count(Year,Ad.Account.Anonymised,Ad.creative.object.type,Topic)
    y <- x %>% group_by(Topic
    ) %>%
      summarize(total.cnt =sum(n))
    y
  })
  stackdata <-  reactive({
    #P.Category --> Ad CTA --> Impressions
    x <- df.filtered() %>% group_by(Year,Ad.Account.Anonymised,Ad.creative.object.type,Ad.call_to_action.type
    ) %>% 
      summarize(total.Impression =sum(Impressions))
    
    y <- x %>% group_by(Ad.creative.object.type,Ad.call_to_action.type
    ) %>%
      summarize(total.Impression=sum(total.Impression))
    
    colnames(y) <- c("Advertisement Created Category","Ad CTA Type","Impressions")
    y
  })

  
  
  
  
  
#Plotting####################################
  #Cost
  output$value1 <- renderValueBox({ 
    out <- costs() 
    valueBox2(
             HTML(paste0("&#8364;",formatC(sum(out$cost.total), format="d", big.mark=','))),
             subtitle = 'Total Costs',
      icon = icon("money"),
      backgroundColor = "#434348") 
  })
  #Impression
  output$value2 <- renderValueBox({ 
    out <- Impression()
        valueBox2( 
            formatC(sum(out$impressions.total), format="d", big.mark=','),
            subtitle = 'Total Impressions',
            icon = icon("eye"),
            backgroundColor = "#f7a35c")
      
     
  })
  #reactions
  output$value3 <- renderValueBox({ 
    out <- Impression()
    valueBox2( 
      formatC(sum(out$reaction.total), format="d", big.mark=','),
      subtitle = 'Total Reactions',
      icon = icon("laugh-wink"),
      backgroundColor = "#2ECC71")
    
  })
  #Comments
  output$value4 <- renderValueBox({ 
    out <- Impression()
    valueBox2( 
      formatC(sum(out$comments.total), format="d", big.mark=','),
      subtitle = 'Total Comments',
      icon = icon("comments"),
      backgroundColor = "#FF5733")
    
  })
  #Share
  output$value5 <- renderValueBox({ 
    out <- Impression()
    valueBox2( 
      formatC(sum(out$share.total), format="d", big.mark=','),
      subtitle = 'Total Shares',
      icon = icon("share"),
      backgroundColor = "#5DADE2")
  })
  #Sentiments
  output$value6 <- renderValueBox({ 
    out <- sentiments()
    valueBox3( 
      formatC(out),
      subtitle = 'Ad Sentiments',
      icon = icon("thumbs-up"),
      backgroundColor = "#C70039")
    
  })
  
  
  #Cost% Share
  output$pie1 <- renderAmCharts({
    
    out <- piedata1()
    amPie(data = out, depth = 10)
  })
  #Impression% Share
  output$pie2 <- renderAmCharts({
    
    out <- piedata2()
   amPie(data = out, depth = 10)
  
    
    })
  #Gender% Share
  output$pie3 <-  renderAmCharts({
    
    out <- piedata3()
    amPie(data = out, inner_radius = 40, depth = 10, show_values = TRUE)
  })
  #WordCloud
  output$wplot <-  renderPlot({
    out <- wdata()
      wordcloud(words = out$Topic,freq=out$total.cnt
                ,scale=c(4,0.5),min.freq = 1,max.words = 300, random.order=FALSE
              ,rot.per=0.3,colors=brewer.pal(8, "Dark2")
              
              )
    
  })
  #Impression vs CTA & Category
  output$stackplot <-  renderPlotly({
    
    out <- stackdata()
    out %>% group_by(`Ad CTA Type`) %>% arrange(`Advertisement Created Category`) %>%
      plot_ly(  x=~Impressions,y= ~`Advertisement Created Category`, color=~`Ad CTA Type`, type = "bar",orientation = 'h'
               , colors ="Dark2"   ) %>%
      layout(yaxis = list(title = "Ad Created Category"), xaxis =list(title="Number of Impressions"),barmode = "stack")
    
    
  })
  
})
  
  
  
  
  
  
  
  
  
  
  
            
            
            


