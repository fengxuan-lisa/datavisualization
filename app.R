# shiny URL: https://datavisualization100.shinyapps.io/project_airbnbLA/
# github URL:https://github.com/fengxuan-lisa/datavisualization.git
#install.packages(syuzhet)
#install.packages("fmsb")
#install.packages("pheatmap")
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(wordcloud)
library(tm)
library(syuzhet)
library(fmsb)
library(pheatmap)
library(ggplot2)
library(SnowballC)
# processing the data
data<- read_csv("Lalistings_afterwashing.csv")
data1<- read.csv("Lalistings_afterwashing.csv",header = TRUE,row.names = 1)
data1$price<-gsub("[$,]", "",data1$price)
data1$price<-as.numeric(data1$price) 
data2<-data1[,-c(1,2,5,8,17)]
reviews<- read_csv("Lalistings.csv")
df<-reviews[,c(2,30)]
reviews <- reviews[,c(8,29)]
df<-left_join(data1,df,by=c("id"="id"))

neighbourhoods <- data %>%
  distinct(neighbourhood_cleansed) %>%
  pull(neighbourhood_cleansed)


ui <- dashboardPage(
  dashboardHeader(title = "Find Your Airbnb in LA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem("General Distribution", tabName = "page2", icon = icon("map-o")),
      menuItem("Reviews", tabName = "page3", icon = icon("users")),
      menuItem("Price", tabName = "page4", icon = icon("line-chart")),
      menuItem("FindAirbnb", tabName = "page5", icon = icon("filter")),
      menuItem("Data", tabName = "page6", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                box(
                  title = "About airbnb & LA", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  "• Project title
                  • Complete and clear description of the methods and analyses
                  performed
                  • Visualizations and dashboards
                  
                  "
                ),
                box(
                  title = "project", 
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  "• Project description
                   • Initial sketches
                   • Research questions that the application is trying to address
                   • Results and conclusion"
                ),
                box(
                  title = "data", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  " • Description of datasets and a few rows of datasets if they can be
                  made public"
                ),
                box(
                  title = "team", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  "name, course, program"
                ),
                box(
                  title = "References", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  "• References, sources, and other readings
                  
                  https://community.rstudio.com/t/warning-message/52176"
                )
              )
      ),
      tabItem(tabName = "page2",
              leafletOutput("myMap", width="100%",height=800)
              
      ),
      tabItem(tabName = "page3",
              fluidRow(

                box(
                
                  title = "Key Words in Reviews", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  column( width = 6,
                          selectInput(inputId = "neighbourhood1",label = "Choose the neighbourhood",
                                      choices = neighbourhoods),
                  plotOutput("plot1")
                  ),
                  column( width = 6,
                          selectInput(inputId = "neighbourhood1a",label = "Choose the neighbourhood",
                                      choices = neighbourhoods),
                          plotOutput("plot1a")
                          
                  )
                ),
                box(
                  title = "Average Review Score", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  column( width = 6,
                          
                          plotOutput("plot2"),
                          textOutput("text1")
                  ),
                  column( width = 6,
                          
                          plotOutput("plot2a"),
                          textOutput("text2")
                  )
                )
              ) 
              
      ),
      tabItem(tabName = "page4",
      
              fluidRow(
                
                box(
                  title = "The price in different groups of neibourhourds", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  plotOutput("plot3",height=500)
                ),
                box(
                  title = "The price in two different neibourhourds", 
                  status = "primary", 
                  width = 12,
                  height = 700,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  fluidRow(
                    column( width = 6,
                            selectInput(inputId = "neighbourhood3a",label = "Choose the neighbourhood",
                                        choices = neighbourhoods),  
                    ),
                    column( width = 6,
                            selectInput(inputId = "neighbourhood3b",label = "Choose the neighbourhood",
                                        choices = neighbourhoods),   
                    ),
                    plotOutput("plot3a",height=500)
                  )

                ),
                box(
                  title = "Relationship Between Price And Other Attributes", 
                  status = "primary", 
                  width = 12,
                   
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  plotOutput("plot4",height=800)
                )
              ) 
              
      ),
      tabItem(
              tabName = "page5",
              fluidRow(
                  
                column( width = 3,

              selectInput("neighbourhood2","neighbourhood",
                          choices = neighbourhoods)
                ),
              column( width = 3,

              sliderInput("accomomdation", "accomomdation", min = 1, max = 16, value = 1, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE))
              ),
              
              column( width = 3,

              selectInput(inputId = "room_type",label = "room type",
                          choices = "")
              ),
              
              column( width = 3,

              checkboxInput("instant_bookable", label = "instant bookable", value = TRUE)
              ),
              
              
               column( width = 3,
    
              checkboxInput("has_availability", label = "has availability", value = TRUE)
               )
              
              ),
              fluidRow(
                dataTableOutput("filter",width="100%")
              ),
              fluidRow(
                leafletOutput("myMap2", width="100%",height = 600)
              )
              
              
              
      ),
      tabItem(
              tabName="page6",
              dataTableOutput("myTable")
      )
    )
  )
)



server <- function(input, output, session) {
  
observe({
  roomType <- data %>%
    filter(neighbourhood_cleansed == input$neighbourhood2) %>%
   distinct(room_type) %>%
   pull (room_type)
  
 updateSelectInput(session = session,
                    inputId = "room_type",
                   choices = roomType)
 
   
 
 
}
  
)
  
  # GRAPHS
  # map for general distribution
    output$myMap = renderLeaflet({
      data %>%
        mutate(
          popusText=paste("price",price,"review rating score",review_scores_rating,"latitude",latitude,"longitude",longitude)
        )%>%
        leaflet() %>% 
        addTiles() %>%
        setView(-118.2437,34.0522, zoom=12) %>% 
        addProviderTiles(providers$Stamen.Toner, group = "Toner")%>% 
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE),
                         
        )%>%
        addMarkers(
          lat= ~latitude,
          lng = ~longitude,
          popup = ~ popusText,
          clusterOptions = markerClusterOptions()
        )
      
    })


   
    

 # wordcloud  
    output$plot1 <- renderPlot({

      
      reviews<-reviews%>%
        filter(neighbourhood_cleansed==input$neighbourhood1)
      myWords=c("can", "just", 
                "one", "think", 
                "like", "get", 
                "see", "will")
     
        w<-VCorpus(x=VectorSource( reviews$neighborhood_overview),
                   readerControl = list(
                     reader=readPlain,
                     language="en"))
          
        
        w<- w%>%
        #Corpus%>% 
        # Convert the text to lower case
        tm_map(content_transformer(tolower))%>%
        # Remove numbers
        tm_map(removeNumbers)%>%
        # Remove english common stopwords
        tm_map(removeWords, stopwords("english")) %>%
        # Remove your own stop word 
        # specify your stopwords as a character vector
        tm_map(removeWords, myWords) %>% 
        # Remove punctuations
        tm_map(removePunctuation) %>% 
        # Eliminate extra white spaces
        tm_map(stripWhitespace) %>%
        # Text stemming
        tm_map(stemDocument) %>%
        TermDocumentMatrix %>%
        as.matrix %>%
        rowSums %>%
        sort(decreasing=TRUE) 
      
      d <-data.frame(word=names(w),freq=w,stringsAsFactors = FALSE)
      sentiments= get_sentiment(d$word)
      NNWords = d %>% 
        mutate(sentRes=sentiments) %>%
        filter(sentRes!=0) %>%
        
        mutate(color=case_when(sentRes<0~"red", sentRes>0 ~ "green"))
      
      for (i in 1:nrow(NNWords)) {
        curRate=NNWords$sentRes[i]
        if(curRate<0){
          NNWords$color[i]=rgb(red = 179/255,
                               green = 5/255,blue = 9/255,alpha = -curRate)
        }
        else{
          NNWords$color[i]=rgb(red = 3/255,
                               green = 89/255,blue = 12/255,alpha = curRate)
        }
      }
      set.seed(5)
      wordcloud(words = NNWords$word, freq = sqrt(NNWords$freq), 
                min.freq = 1,
                max.words=70,random.order=FALSE,
                rot.per=0.35, 
                colors=NNWords$color, ordered.colors=TRUE
               
                )
      
      
    })
    
    output$plot1a <- renderPlot({
      
      
      reviews<-reviews%>%
        filter(neighbourhood_cleansed==input$neighbourhood1a)
      myWords=c("can", "just", 
                "one", "think", 
                "like", "get", 
                "see", "will")
      
      w<-VCorpus(x=VectorSource( reviews$neighborhood_overview),
                 readerControl = list(
                   reader=readPlain,
                   language="en"))
      
      
      w<- w%>%
        #Corpus%>% 
        # Convert the text to lower case
        tm_map(content_transformer(tolower))%>%
        # Remove numbers
        tm_map(removeNumbers)%>%
        # Remove english common stopwords
        tm_map(removeWords, stopwords("english")) %>%
        # Remove your own stop word 
        # specify your stopwords as a character vector
        tm_map(removeWords, myWords) %>% 
        # Remove punctuations
        tm_map(removePunctuation) %>% 
        # Eliminate extra white spaces
        tm_map(stripWhitespace) %>%
        # Text stemming
        tm_map(stemDocument) %>%
        TermDocumentMatrix %>%
        as.matrix %>%
        rowSums %>%
        sort(decreasing=TRUE) 
      
      d <-data.frame(word=names(w),freq=w,stringsAsFactors = FALSE)
      sentiments= get_sentiment(d$word)
      NNWords = d %>% 
        mutate(sentRes=sentiments) %>%
        filter(sentRes!=0) %>%
        
        mutate(color=case_when(sentRes<0~"red", sentRes>0 ~ "green"))
      
      for (i in 1:nrow(NNWords)) {
        curRate=NNWords$sentRes[i]
        if(curRate<0){
          NNWords$color[i]=rgb(red = 179/255,
                               green = 5/255,blue = 9/255,alpha = -curRate)
        }
        else{
          NNWords$color[i]=rgb(red = 3/255,
                               green = 89/255,blue = 12/255,alpha = curRate)
        }
      }
      set.seed(5)
      wordcloud(words = NNWords$word, freq = sqrt(NNWords$freq), 
                min.freq = 1,
                max.words=70,random.order=FALSE,
                rot.per=0.35, 
                colors=NNWords$color, ordered.colors=TRUE
                
      )
      
      
    })
    
    #radar plot
  
  output$plot2 <- renderPlot({

    scores <- data[,c(3,11:17)]
    avscore <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1)

    ascore <-  data.frame(
      "rating"=c(5,0,round(mean(avscore$review_scores_rating),3)),
      "accuracy"=c(5,0,round(mean(avscore$review_scores_accuracy),3)),
      "cleanliness"=c(5,0,round(mean(avscore$review_scores_cleanliness),3)),
      "checkin"=c(5,0,round(mean(avscore$review_scores_checkin),3)),
      "communication"=c(5,0,round(mean(avscore$review_scores_communication),3)),
      "location"=c(5,0,round(mean(avscore$review_scores_location),3)),
      "value"=c(5,0,round(mean(avscore$review_scores_value),3))
      
    )
    radarchart(
      ascore, 
      axistype = 1,
      seg=4,
      pcol = rgb(0.2,0.5,0.5,0.9),
      pfcol = rgb(0.2,0.5,0.5,0.5),
      plwd = 4,
      cglcol = "black",
      cglty = 4,
      axislabcol = "grey",
      caxislabels = seq(1,5,1),
      cglwd = 0.6,
      vlcex = 0.7,
      title = "Average score"
    )
  })
  
  output$text1<-renderText({
    scores <- data[,c(3,11:17)]
    avscore <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1)
    ascore <-  data.frame(
      "rating"=c(5,0,round(mean(avscore$review_scores_rating),3)),
      "accuracy"=c(5,0,round(mean(avscore$review_scores_accuracy),3)),
      "cleanliness"=c(5,0,round(mean(avscore$review_scores_cleanliness),3)),
      "checkin"=c(5,0,round(mean(avscore$review_scores_checkin),3)),
      "communication"=c(5,0,round(mean(avscore$review_scores_communication),3)),
      "location"=c(5,0,round(mean(avscore$review_scores_location),3)),
      "value"=c(5,0,round(mean(avscore$review_scores_value),3))
      
    )
    
    paste(
      "rating:",ascore$rating[3],
      "accuracy:",ascore$accuracy[3],
      "cleanliness:",ascore$cleanliness[3],
      "checkin:",ascore$checkin[3],
      "location:",ascore$location[3],
      "value:",ascore$value[3]
    )
    
  })
  
  output$plot2a <- renderPlot({
    
    scores <- data[,c(3,11:17)]
    avscore <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1a)
    ascore <-  data.frame(
      "rating"=c(5,0,round(mean(avscore$review_scores_rating),3)),
      "accuracy"=c(5,0,round(mean(avscore$review_scores_accuracy),3)),
      "cleanliness"=c(5,0,round(mean(avscore$review_scores_cleanliness),3)),
      "checkin"=c(5,0,round(mean(avscore$review_scores_checkin),3)),
      "communication"=c(5,0,round(mean(avscore$review_scores_communication),3)),
      "location"=c(5,0,round(mean(avscore$review_scores_location),3)),
      "value"=c(5,0,round(mean(avscore$review_scores_value),3))
      
    )
    radarchart(
      ascore, 
      axistype = 1,
      seg=4,
      pcol = rgb(0.2,0.5,0.5,0.9),
      pfcol = rgb(0.2,0.5,0.5,0.5),
      plwd = 4,
      cglcol = "black",
      cglty = 4,
      axislabcol = "grey",
      caxislabels = seq(1,5,1),
      cglwd = 0.6,
      vlcex = 0.7,
      title = "Average score"
    )
  })
  output$text2<-renderText({
    scores <- data[,c(3,11:17)]
    avscore <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1a)
    ascore <-  data.frame(
      "rating"=c(5,0,round(mean(avscore$review_scores_rating),3)),
      "accuracy"=c(5,0,round(mean(avscore$review_scores_accuracy),3)),
      "cleanliness"=c(5,0,round(mean(avscore$review_scores_cleanliness),3)),
      "checkin"=c(5,0,round(mean(avscore$review_scores_checkin),3)),
      "communication"=c(5,0,round(mean(avscore$review_scores_communication),3)),
      "location"=c(5,0,round(mean(avscore$review_scores_location),3)),
      "value"=c(5,0,round(mean(avscore$review_scores_value),3))
      
    )

    paste(
      "rating:", ascore$rating[3],
      "accuracy:",ascore$accuracy[3],
      "cleanliness:",ascore$cleanliness[3],
      "checkin:",ascore$checkin[3],
      "location:",ascore$location[3],
      "value:",ascore$value[3]
    )

    
    
  })
  
  #box plot
  output$plot3 <- renderPlot({
    
    ggplot(data = df) +
      geom_boxplot(mapping =
                     aes(x=neighbourhood_group_cleansed, y = price),
                   color="lightblue"
      )+
      theme(
        panel.grid.major = element_line(colour = "light grey",size=0.25), 
        panel.grid.minor = element_line(colour = "light grey",size=0.25),
        panel.background = element_rect(fill=NA),
        panel.border = element_rect(fill=NA,colour = "black",size = 0.3,linetype = "solid")
      )
    
  })
  
  output$plot3a <- renderPlot({
    dfa<-df%>%
      filter(neighbourhood_cleansed==input$neighbourhood3a)
    dfb<-df%>%
      filter(neighbourhood_cleansed==input$neighbourhood3b)
    dfc<- rbind(dfa,dfb)
    ggplot(data = dfc) +
      geom_boxplot(mapping =
                     aes(x=neighbourhood_cleansed, y = price,
                         color=neighbourhood_cleansed)
                 
                   
      )+
      theme(
        panel.grid.major = element_line(colour = "light grey",size=0.25), 
        panel.grid.minor = element_line(colour = "light grey",size=0.25),
        panel.background = element_rect(fill=NA),
        panel.border = element_rect(fill=NA,colour = "black",size = 0.3,linetype = "solid")
      )
    
  })
  
  #heat map
  output$plot4 <- renderPlot({

    
    r <- cor(data2,method = "pearson")
    
    pheatmap(
      r, 
      cellwidth=40,
      cellheight=40,
      border_color=NA,
      cluster_col=FALSE,
      main = "Heat map",
      fontsize=15,
      treeheight_row=120,
      cutree_row=3
      
    )
    
    
  })
  
# filter table
  output$filter <- renderDataTable({
    
   if(input$instant_bookable==TRUE){
     
     if(input$has_availability==TRUE){
       data_filted <- data%>%
         filter(neighbourhood_cleansed == input$neighbourhood2)%>% #neighbourhood
         filter(accommodates== input$accomomdation)%>% #accomadation
         filter(room_type== input$room_type)%>% # room_type
         filter(instant_bookable== "TRUE")%>% #instant_acceptable
         
         filter(has_availability=="TRUE")
       
       
       
     }else{
       
       data_filted <-data %>%
         filter(neighbourhood_cleansed == input$neighbourhood2)%>% #neighbourhood
         filter(accommodates == input$accomomdation)%>% #accomadation
         filter(room_type == input$room_type)%>% # room_type
         filter(instant_bookable== "TRUE")%>% #instant_acceptable
         
         filter(has_availability=="FALSE")
       
     }
     
     
     
   }else{
     
     if(input$has_availability==TRUE){
       data_filted <- data%>%
         filter(neighbourhood_cleansed == input$neighbourhood2)%>% #neighbourhood
         filter(accommodates == input$accomomdation)%>% #accomadation
         filter(room_type== input$room_type)%>% # room_type
         filter(instant_bookable== "FALSE")%>% #instant_acceptable
         
         filter(has_availability=="TRUE")
       
       
     }else{
       data_filted <-data%>%
         filter(neighbourhood_cleansed== input$neighbourhood2)%>% #neighbourhood
         filter(accommodates== input$accomomdation)%>% #accomadation
         filter(room_type== input$room_type)%>% # room_type
         filter(instant_bookable== "FALSE")%>% #instant_acceptable
         
         filter(has_availability=="FALSE")
       
     }
   }
    
  
    return(datatable(data_filted[,-c(1,4,5)], rownames= FALSE,options = list(scrollX = TRUE)))
    
  })
  
  # filter map
  output$myMap2 = renderLeaflet({
    
    if(input$instant_bookable==TRUE){
      
      if(input$has_availability==TRUE){
        data_filted <- data%>%
          filter(neighbourhood_cleansed == input$neighbourhood2)%>% #neighbourhood
          filter(accommodates== input$accomomdation)%>% #accomadation
          filter(room_type== input$room_type)%>% # room_type
          filter(instant_bookable== "TRUE")%>% #instant_acceptable
          
          filter(has_availability=="TRUE")
        
        
        
      }else{
        
        data_filted <-data %>%
          filter(neighbourhood_cleansed == input$neighbourhood2)%>% #neighbourhood
          filter(accommodates == input$accomomdation)%>% #accomadation
          filter(room_type == input$room_type)%>% # room_type
          filter(instant_bookable== "TRUE")%>% #instant_acceptable
          
          filter(has_availability=="FALSE")
        
      }
      
      
      
    }else{
      
      if(input$has_availability==TRUE){
        data_filted <- data%>%
          filter(neighbourhood_cleansed == input$neighbourhood2)%>% #neighbourhood
          filter(accommodates == input$accomomdation)%>% #accomadation
          filter(room_type== input$room_type)%>% # room_type
          filter(instant_bookable== "FALSE")%>% #instant_acceptable
          
          filter(has_availability=="TRUE")
        
        
      }else{
        data_filted <-data%>%
          filter(neighbourhood_cleansed== input$neighbourhood2)%>% #neighbourhood
          filter(accommodates== input$accomomdation)%>% #accomadation
          filter(room_type== input$room_type)%>% # room_type
          filter(instant_bookable== "FALSE")%>% #instant_acceptable
          
          filter(has_availability=="FALSE")
        
      }
    }
    data_filted%>%
    mutate(
       popusText=paste("price",price,"review rating score",review_scores_rating)
      )%>%
      leaflet() %>% 
      addTiles() %>%
      setView(data$longitude[1],data$latitude[1], zoom=12) %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner")%>% 
      addLayersControl(baseGroups = c("Toner", "OSM"),
                       options = layersControlOptions(collapsed = FALSE),
                       
      )%>%
     addMarkers(
        lat= ~latitude,
        lng = ~longitude,
        popup = ~ popusText,
        clusterOptions = markerClusterOptions()
      )
    
  })
  
  # show the data 
  output$myTable <- renderDataTable({
    return(datatable(data[,-c(1)], rownames= FALSE,options = list(scrollX = TRUE)))
  })
  
}

shinyApp(ui = ui, server = server)