# shiny URL: https://datavisualization100.shinyapps.io/Project134_Airbnb_LA/
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
library(png)
# processing the data
data<- read_csv("Lalistings_afterwashing.csv")
data1<- read.csv("Lalistings_afterwashing.csv",header = TRUE,row.names = 1)
data1$price<-gsub("[$,]", "",data1$price)
data1$price<-as.numeric(data1$price) 
data2<-data1[,-c(1,2,3,4,7,10,19)]
reviews<- read_csv("Lalistings.csv")
df<-reviews[,c(2,30)]
reviews <- reviews[,c(8,29)]
df<-left_join(data1,df,by=c("id"="id"))

neighbourhoods <- data %>%
  distinct(neighbourhood_cleansed) %>%
  pull(neighbourhood_cleansed)


groups<- df %>%
  distinct(neighbourhood_group_cleansed) %>%
  pull(neighbourhood_group_cleansed)

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
                  title = "About Airbnb & LA", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  HTML('<iframe width="100%" height="500" src="https://www.youtube.com/embed/cSjm76GW9fU" align="center" frameborder="0" allowfullscreen></iframe>'),
                 
                  tags$h3(" • About Airbnb:"),
                  tags$p("Airbnb, is an American company that operates an online marketplace for lodging, 
                         primarily homestays for vacation rentals, and tourism activities. 
                         Based in San Francisco, California, the platform is accessible via website and mobile app. 
                         Airbnb does not own any of the listed properties; instead, it profits by receiving commission from each booking. 
                         The company was founded in 2008 by Brian Chesky, Nathan Blecharczyk, and Joe Gebbia. 
                         Airbnb is a shortened version of its original name, AirBedandBreakfast.com."),
                  tags$h3(" • About LA:"),
                  tags$p("Los Angeles often referred to by its initials L.A., is a major city in the U.S. state of California. 
                         With a 2020 population of 3,898,747, it is the largest city in the state, as well as the second-largest city in the United States following New York City. 
                         Los Angeles is known for its Mediterranean climate, ethnic and cultural diversity, Hollywood film industry, and sprawling metropolitan area."),
                  tags$p('
                  
                  Los Angeles is often billed as the "Creative Capital of the World" because one in every six of its residents works in a creative industry 
                  and there are more artists, writers, filmmakers, actors, dancers and musicians living and working in Los Angeles than any other city at any other time in history.
                         '
                         
                         
                        )
                ),
                box(
                  title = "Project", 
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,

                  tags$h3("• Project description"), 
                  tags$p("
                  Losts of people would like to travel in Los Angeles when they are on holiday.
                  In this project, the users can find out the relationship between the room price and some factors,
                         such like the neibourhoud,the overveiw scores in several aspects. 
                         They can also make comparision between two neibourhoods by the prices and scores.
                         finally they can use the filter to find and select the rooms reaching their requirements"),
                  tags$h3 ("• Initial sketches"),
                  img(src="draftplan.png", width="50%",height=500),
                  #tags$a("click to see the doc about draft plan",
                   
                  #      href="https://docs.google.com/document/d/1SGTbahMm3s1P_kxcKb2b90vPe5wOMVCtNqiBYqzuFUM/edit?usp=sharing",
                   #      target="_blank"),
                  tags$h3 ("• Research questions that the application is trying to address"),
                  tags$ol("Find out the relationship between the room price and some factors"),
                  tags$ol("Make comparision between two neibourhoods"),
                  tags$ol("Use the filter to find and select the rooms reaching their requirements"),
                  tags$h3 ("• Methods"),
                  tags$ol("Map -> Show the general distribution or the location of the Airbnb user choose"),
                  tags$ol("Wordcloud -> Show the reviews about the neighbouhood choosed"),
                  tags$ol("Radar chart -> Show the scores about the neighbourhood"),
                  tags$ol("Box chart -> Show the relationship between price and neighbourhood"),
                  tags$ol("Heatmap -> Show the relationship between some features"),
                  tags$ol("Filter -> Be used to select"),
                  tags$ol("Datatable -> Show the whole dataset the project uses"),
                  tags$h3 ("• Results and conclusion"),
                  tags$p("the aveage score and reviews of neibourhoods here are good more than 4.5,
                          but some location such like East Hollywood is an exception.
                          We also find out that the review score of rating and  value are influenced by accuracy, communication and cleanliness.
                          The price is influenced by accommodates, location and review score of location. 
                          We also find that the room price in LA city is not the highest.
                          But that is the general conclusion from the charts. 
                         As for the price of room is highly influenced by which factors, still need further research")
                ),
                
                box(
                  title = "Data", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  tags$h3(" • Data Resources:"),
                  tags$a("• Airbnb officilal Datasets",href="http://insideairbnb.com/explore",target="_blank"),tags$br(),
                  tags$a("• To download raw data",href="http://data.insideairbnb.com/united-states/ca/los-angeles/2022-06-06/data/listings.csv.gz",target="_blank"),
                  tags$h3(" • Data Description:"),
                  tags$ol("Raw data:42041(items)*75(attributes),including 288075 N/A"),
                  tags$ol("Data after cleansing:"),
                  tags$li("data:32112(items)*19(attributes),no N/A"),
                  tags$li("reviews:32112(items)*2(attributes),no N/A")


                ),
                box(
                  title = "Team", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                    tags$img(src="dog.png", float="left"),
                 
                    
                    tags$li("  Name: XUAN FENG"),
                    tags$li(" Course:BU.520.650.51.SU22 Data Visualization"),
                    tags$li("  Program: MSIS")
                 


                ),
                box(
                  title = "References", 
                  status = "primary", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  
                  
                  tags$a("• https://community.rstudio.com/t/warning-message/52176",href="https://community.rstudio.com/t/warning-message/52176",target="_blank"),tags$br(),
                  tags$a("• https://en.wikipedia.org/wiki/Airbnb",href="https://en.wikipedia.org/wiki/Airbnb",target="_blank"),tags$br(),
                  tags$a("• https://en.wikipedia.org/wiki/Los_Angeles",href="https://en.wikipedia.org/wiki/Los_Angeles",target="_blank"),tags$br()
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
                  title = "The price in two different neighbourhourds", 
                  status = "primary", 
                  width = 12,
                  height = 700,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  fluidRow(

                              column( width=3,
                                      selectInput(inputId = "group1",label = "Choose the group",
                                                  choices = groups)   
                              ),
                              column( width=3,
                                      selectInput(inputId = "neighbourhood3a",label = "Choose the neighbourhood",
                                                  choices = "")                     
                              ),
                           
  
                   
                              column( width=3,
                                      selectInput(inputId = "group2",label = "Choose the group",
                                                  choices = groups)   
                              ),
                              column( width=3,
                                      selectInput(inputId = "neighbourhood3b",label = "Choose the neighbourhood",
                                                  choices = "")                  
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
                box(
                  title = "table", 
                  status = "primary", 
                  width = 12,
                  
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  dataTableOutput("filter",width="100%")
                )
                
              ),
              fluidRow(
                box(
                  title = "map", 
                  status = "primary", 
                  width = 12,
                  
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("myMap2", width="100%",height = 600)
                )
                
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
 
 neighbourhood3a <- df %>%
   filter(neighbourhood_group_cleansed == input$group1) %>%
   distinct(neighbourhood_cleansed) %>%
   pull (neighbourhood_cleansed)
 
 updateSelectInput(session = session,
                   inputId = "neighbourhood3a",
                   choices = neighbourhood3a) 
 
 neighbourhood3b <- df %>%
   filter(neighbourhood_group_cleansed == input$group2) %>%
   distinct(neighbourhood_cleansed) %>%
   pull (neighbourhood_cleansed)
 
 updateSelectInput(session = session,
                   inputId = "neighbourhood3b",
                   choices = neighbourhood3b) 
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
        setView(-118.2437,34.0522, zoom=10) %>% 
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

    scores <- data[,c(4,5,13:19)]
    avscore <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1)

    ascore <-  data.frame(
      "Rating"=c(5,4,round(mean(avscore$review_scores_rating),3)),
      "Accuracy"=c(5,4,round(mean(avscore$review_scores_accuracy),3)),
      "Cleanliness"=c(5,4,round(mean(avscore$review_scores_cleanliness),3)),
      "Checkin"=c(5,4,round(mean(avscore$review_scores_checkin),3)),
      "Communication"=c(5,4,round(mean(avscore$review_scores_communication),3)),
      "Location"=c(5,4,round(mean(avscore$review_scores_location),3)),
      "Value"=c(5,4,round(mean(avscore$review_scores_value),3))
      
    )

    radarchart(
      ascore, 
      axistype = 3,
      seg=5,
      pcol = rgb(0.2,0.5,0.5,0.9),
      pfcol = rgb(0.2,0.5,0.5,0.5),
      plwd = 4,
      cglcol = "black",
      cglty = 4,
      axislabcol = "grey",
      #caxislabels = seq(4,5,0.2),
      cglwd = 0.6,
      vlcex = 0.7,
      title =paste("Average score in",input$neighbourhood1)
    )
    

  })
  
  output$text1<-renderText({
    scores <- data[,c(4,5,13:19)]
    avscore1 <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1)
    ascore1 <-  data.frame(
      "rating"=c(round(mean(avscore1$review_scores_rating),3)),
      "accuracy"=c(round(mean(avscore1$review_scores_accuracy),3)),
      "cleanliness"=c(round(mean(avscore1$review_scores_cleanliness),3)),
      "checkin"=c(round(mean(avscore1$review_scores_checkin),3)),
      "communication"=c(round(mean(avscore1$review_scores_communication),3)),
      "location"=c(round(mean(avscore1$review_scores_location),3)),
      "value"=c(round(mean(avscore1$review_scores_value),3))
      
    )
    
   
    paste(
      "Rating:",ascore1$rating,
      "Accuracy:",ascore1$accuracy,
      "Cleanliness:",ascore1$cleanliness,
      "Checkin:",ascore1$checkin,
      "Location:",ascore1$location,
      "Value:",ascore1$value
    )
    
  })
  
  output$plot2a <- renderPlot({
    
    scores <- data[,c(4,5,13:19)]
    avscore <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1a)
    ascore <-  data.frame(
      "Rating"=c(5,4,round(mean(avscore$review_scores_rating),3)),
      "Accuracy"=c(5,4,round(mean(avscore$review_scores_accuracy),3)),
      "Cleanliness"=c(5,4,round(mean(avscore$review_scores_cleanliness),3)),
      "Checkin"=c(5,4,round(mean(avscore$review_scores_checkin),3)),
      "Communication"=c(5,4,round(mean(avscore$review_scores_communication),3)),
      "Location"=c(5,4,round(mean(avscore$review_scores_location),3)),
      "Value"=c(5,4,round(mean(avscore$review_scores_value),3))
      
    )
    radarchart(
      ascore, 
      axistype = 3,
      seg=5,
      pcol = rgb(0.2,0.5,0.5,0.9),
      pfcol = rgb(0.2,0.5,0.5,0.5),
      plwd = 4,
      cglcol = "black",
      cglty = 4,
      axislabcol = "grey",
      #caxislabels = seq(4,5,0.2),
      cglwd = 0.6,
      vlcex = 0.7,
      title =paste("Average score in",input$neighbourhood1a)
    )
  })
  output$text2<-renderText({
    scores <- data[,c(4,5,13:19)]
    avscore1 <- scores%>%
      filter(neighbourhood_cleansed==input$neighbourhood1a)
    ascore1 <-  data.frame(
      "rating"=c(round(mean(avscore1$review_scores_rating),3)),
      "accuracy"=c(round(mean(avscore1$review_scores_accuracy),3)),
      "cleanliness"=c(round(mean(avscore1$review_scores_cleanliness),3)),
      "checkin"=c(round(mean(avscore1$review_scores_checkin),3)),
      "communication"=c(round(mean(avscore1$review_scores_communication),3)),
      "location"=c(round(mean(avscore1$review_scores_location),3)),
      "value"=c(round(mean(avscore1$review_scores_value),3))
      
    )

    paste(
      "Rating:", ascore1$rating,
      "Accuracy:",ascore1$accuracy,
      "Cleanliness:",ascore1$cleanliness,
      "Checkin:",ascore1$checkin,
      "Location:",ascore1$location,
      "Value:",ascore1$value
    )

    
    
  })
  
  #box plot
  output$plot3 <- renderPlot({
    
    ggplot(data = df) +
      geom_boxplot(mapping =
                     aes(x=neighbourhood_group_cleansed, y = price,color=neighbourhood_group_cleansed),
                   
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
    
  
    return(datatable(data_filted[,-c(1,3,6,7)], rownames= FALSE,options = list(scrollX = TRUE)))
    
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
       popusText=paste("name:",name,"price:",price,"review rating score:",review_scores_rating,tags$a("clikck here to know more",href=listing_url,target="_blank"))
      )%>%
      leaflet() %>% 
      addTiles() %>%
      setView(data$longitude[2],data$latitude[2], zoom=10) %>% 
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