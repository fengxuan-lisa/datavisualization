install.packages("R.utils")
install.packages("fmsb")
install.packages("pheatmap")
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(plotly)
library (data.table)
library(wordcloud)
library(tm)
library(syuzhet)
library(fmsb)
library(pheatmap)
library(ggplot2)
if(FALSE){
  #transfer .gz file to .csv file
  ladata<- fread("http://data.insideairbnb.com/united-states/ca/los-angeles/2022-06-06/data/listings.csv.gz")
  write.csv(ladata,file = "Lalistings.csv")
  lareview<- fread("http://data.insideairbnb.com/united-states/ca/los-angeles/2022-06-06/data/reviews.csv.gz")
  write.csv(lareview,file = "Lareviews.csv")
  data<- read_csv("Lalistings.csv")
  #data
  #sum(is.na(data))
  
  review<- read_csv("Lareviews.csv")
  
  review <- review[,c(2,7)]
  
  data <- data[,c(2,17,18,29,31,32,34,35,41,51,57,62,63,64,65,66,67,68,70)]
  #sum(is.na(data))
  data <- na.omit(data)
  data <-data[,-c(2,3)]
  write.csv(data,file = "Lalistings_afterwashing.csv")
  neibourhoodreview<-left_join(review,data[,c(1,2)],by=c("listing_id"="id"))
  write.csv(neibourhoodreview,file = "Laneibourhoodreviews.csv")
  
  data<- read_csv("desktop/datavisualization/project/Lalistings_afterwashing.csv")
  roomType <- data %>%
    filter(neighbourhood_cleansed == "Arcadia") %>%
    distinct(room_type) %>%
    pull (room_type)
  roomType
}

# world cloud
reviews<- read_csv("Lalistings.csv")
reviews <- reviews[,c(8,29)]


myWords=c("can", "just", 
          "one", "think", 
          "like", "get", 
          "see", "will")
w<-reviews$neighborhood_overview%>%
  VectorSource%>% 
  Corpus%>% 
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
wordcloud(words = NNWords$word, freq = NNWords$freq, 
          min.freq = 10,
          max.words=70,random.order=FALSE,
          colors=NNWords$color, ordered.colors=TRUE)



#rader chart
data<- read_csv("Lalistings_afterwashing.csv")
scores <- data[,c(3,11:17)]
avscore <- scores%>%
  filter(neighbourhood_cleansed=="Mount Washington")
av<-max(avscore$review_scores_rating)
m<-mean(mean(avscore$review_scores_accuracy))
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
             #caxislabels = seq(1,5,1),
             cglwd = 0.6,
            vlcex = 0.7,
             title = "Average score"
  )

# line chart
df<- read_csv("Desktop/datavisualization/project/Lalistings.csv")
df<-df[,c(2,30)]
data1<- read.csv("Desktop/datavisualization/project/Lalistings_afterwashing.csv",header = TRUE,row.names = 1)
data1$price<-gsub("[$,]", "",data1$price)
data1$price<-as.numeric(data1$price) 
data2<-data1[,-c(1,2,5,8,17)]



sum(is.na(df))
df<-left_join(data1,df,by=c("id"="id"))
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

# heat map
data<- read.csv("Lalistings_afterwashing.csv",header = TRUE,row.names = 1)
data<-data[,-c(1)]
data1<-data[,-c(1,4,7,16)]
class(data1$latitude)
class(data1$longitude)
class(data1$accommodates)
class(data1$price)
class(data1$number_of_reviews)
data1$price<-gsub("[$,]", "",data1$price)
data1$price<-as.numeric(data1$price) 
class(data1$price)
r <- cor(data1,method = "pearson")

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

# map
#m<-data %>%
#  mutate(
#    popusText=paste("price",price,"review rating score",review_scores_rating)
#  )%>%
#  leaflet() %>% 
#  addTiles() %>%
 # setView(-118.2437,34.0522, zoom=12) %>% 
#  addProviderTiles(providers$Stamen.Toner, group = "Toner")%>% 
#  addLayersControl(baseGroups = c("Toner", "OSM"),
#                   options = layersControlOptions(collapsed = FALSE),
                   
#  )%>%
#  addMarkers(
#    lat= ~latitude,
#    lng = ~longitude,
#    popup = ~ popusText,
#    clusterOptions = markerClusterOptions()
#  )
#m
