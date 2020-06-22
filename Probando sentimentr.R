library(rtweet)
library(tidyverse)
library(tidytext)
library(lubridate)
library(sentimentr)
library(purrr)
library(pacman)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr, dplyr, magrittr)
library(plotly)
library(zoo) #Para hacer la media movil
library(sentimentr)
save.image(file="ugr.rdata")

sentencesugr <- get_sentences(ugrtweets$stripped_text) 

clean_text <- function (df) {

  #removes emoticons
#df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #Elimina emoticonos
#df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) 
#df <- df %>% plain_tweets()
  
  df$text <- sapply(df$text,function(row) iconv(row, "UTF-8", "ASCII//TRANSLIT", sub="")) #Elimina emoticonos y acentos
  df$text  <- gsub("http.*","",  df$text) #Esta es la buena, la que elimina cualquier tipo de enlace
  
  #df$text <- gsub("https://t.co/[A-Za-z\\d]+|&amp;","", df$text)
  #df$text <- gsub("https://t.co/*","",df$text)
#df$text = gsub("http\\w+", "", df$text)

df$text <- gsub("[[:digit:]]","" ,df$text)
#df$text= gsub("[[:digit:]]", "", df$text)
#df$text= gsub("[[:digit:]]", "", df$text)
#sp2$text  <- gsub("[[:punct:]]", "", sp2$text) #Remueve estos simbolos "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
#sp2$text <- sp2$text %>% iconv( from = 'UTF-8', to = 'ASCII//TRANSLIT') #Quita tildes pero elimina muchos tweets que empiezan con emoticono

df$text= gsub("@\\w+", "", df$text)

return(df)

}

ugrtweet55 <- clean_text(ugrtweets)

#ugrtweets$text2 <- sapply(ugrtweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#ugrtweet55<-plain_tweets(ugrtweets)

sentencesugr <- get_sentences(ugrtweet55$text) 

sentimentugr<-sentiment(sentencesugr)

Covid19 <- search_tweets("#COVID19",n=1000,lang="en")

Covid19cl <- clean_text(Covid19)
sentencesCovid <- get_sentences(Covid19cl$text)
sentiment(sentencesCovid)

sentimentcovid <- sentiment_by(sentencesCovid)

sentimentcovidpunt <- sentimentcovid %>% select(sd,ave_sentiment)
Covid19gg <- cbind(Covid19cl,sentimentcovidpunt)
#Covid19gg <-left_join(Covid19, sentimentcovidpunt,by="status_id")

Covid19gg %>% ggplot(aes(created_at,ave_sentiment))+geom_point()

Covid19gg <- Covid19gg %>% mutate(Tipo = ifelse(ave_sentiment > 0, "Positivo", "Negativo"))


Covid19gg %>% filter((Covid19gg$followers_count/Covid19gg$friends_count)>0.7) %>%  ## Plot de sentimiento Positivo negativo
  count(screen_name,Tipo, sort=TRUE) %>% top_n(12) %>%
  ggplot()+aes(screen_name,n,fill=Tipo)+geom_bar(stat = "identity")

Covid19gg %>% filter((Covid19gg$followers_count/Covid19gg$friends_count)>0.7) %>%  ## Plot de sentimiento Positivo negativo
  top_n(12) %>%
  ggplot()+
  aes(screen_name, ave_sentiment, fill = screen_name) +
  geom_boxplot() +
  tema_graf



tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))


arranged <- arrange(Covid19gg,ave_sentiment)
arranged <- arranged %>% filter(is_retweet==FALSE)
arranged <- unique(arranged$text)

get_sentiments("nrc")

probando2 <-get_timeline(c("canalugr","forocoches",""),n=100)
rm(probando2)


Colegas <-get_timeline(c("GrupoSalesland","seidor"), n=1000)
Colegas <- Colegas %>%  mutate(fecha = format(created_at, "%Y-%m-%d"))
Colegas <- Colegas %>%  mutate(fecha = as.Date(Colegas$fecha))
Colegas <- Colegas %>%  mutate(yday = yday(fecha))
Colegas <- Colegas %>%  mutate(año  = year(fecha))
Colegas <- Colegas %>%  unite(año_dia,año,yday,sep="-")

Colegascount <- Colegas %>% count(screen_name,año_dia)

pruebaspread <- Colegascount %>% spread(key = screen_name, value = n, fill = NA, drop = TRUE) #funcionó :D
 pruebaspread[is.na(pruebaspread)]<- 0
d[is.na(d)] <- 0

z <-plot_ly(data = pruebaspread, x=~año_dia, y=~seidor, name = 'trace 0', type = 'scatter', mode = 'lines')
z <- z %>% add_trace(y = ~GrupoSalesland, name = 'Salesland',mode = 'lines') 
z <- z %>% add_trace(y = ~seidor, name = 'Seidor',mode = 'lines')
z
Colegascount %>% ggplot()+aes(año_dia,n)+ geom_col()+facet_wrap(~screen_name)+ theme_minimal()



plot_ly(data = Colegascount,x=~año_dia,y=~n)
  
#geom_bar(stat="identity")  geom_smooth(method = lm)+

Covid19gg <- Covid19gg %>%  mutate(fecha = format(created_at, "%Y-%m-%d"))

Covid19gg <- Covid19gg %>% mutate(fechax = as.Date(Covid19gg$fecha))

Covid19gg <- Covid19gg %>% mutate(yday = yday(fechax))
Covid19gg <- Covid19gg %>% mutate(año  = year(fechax))

Covid19gg <- Covid19gg %>% unite(año_dia,año,yday,sep="-")


custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

covid <- search_tweets("#IfCovidNeverHappened",n=2000,lang="en")
covid3 <- clean_text(covid)
sentimentcovid <- sentiment_by(covid3$text)

summary(sentimentcovid$ave_sentiment)

covid3$punt <- sentimentcovid$ave_sentiment

covid3 <- covid3 %>% mutate(fecha= format(created_at,"%Y-%m-%d %h:%M:%S")) #"%Y-%m-%d %H:%M:%S"

covid2 <- covid2 %>% mutate(mydate = strptime('16/Oct/2005:07:51:00',format='%d/%b/%Y:%H:%M:%S'))

agregadp <- aggregate(covid2["punt"], by=covid2["created_at"], sum)

fig3 <- plot_ly(data=agregadp,x=~created_at,y=~punt,type = 'scatter', mode = 'lines')

 rango <- seq(0,172000,by=60)

xaxis <- now()-rango

fig2 <- plot_ly(data=covid2, x=~xaxis,y= ~punt,type = 'scatter', mode = 'lines')

qplot(sentimentcovid$ave_sentiment,   geom="histogram",binwidth=0.1, colour="green",main="Tweet Sentiment Histogram")

sentimentcovid <- sentimentcovid %>% mutate(Tipo = ifelse(ave_sentiment > 0, "Positivo", "Negativo"))

sentimentcovid %>%
  ggplot() +
  aes(ave_sentiment) +
  geom_density() +
  tema_graf

fig <- plot_ly(data=count(sentimentcovid,Tipo), labels = ~Tipo, values = ~n, type = 'pie')


obama <- get_timeline("BarackObama",n=3200)
obama2 <- clean_text(obama)
obamast <- get_sentences(obama2$text)
obamasent <- sentiment(obamast)
obamasent2 <- sentiment_by(obama2$text)

obama2$score <- obamasent2$ave_sentiment

obama2<- obama2 %>% mutate(fecha= format(created_at,"%Y-%m-%d"))
obama2<- obama2 %>% mutate(fecha=as.Date(fecha))

obamaplot <- aggregate(obama2["score"], by=obama2["created_at"], sum)

obamamean <- aggregate(obama2["score"],by=obama2["fecha"],mean)

obamamean <- obamamean %>% mutate(MediaR = rollmean(score, k = 21, align = "right", na.pad = TRUE)) 

obamaplot2 <- aggregate(obama2["score"],by=obama2["fecha"],sum)

fig4 <- plot_ly(data=donaldagre,x=~created_at,y=~score,type = 'scatter', mode = 'lines')

fig5 <- plot_ly(data=obamamean,x=~fecha,y=~score,type = 'scatter', mode = 'lines') %>% 
add_trace(y = ~MediaR, name = 'Media movil k=3',mode = 'lines')
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

count(covid,screen_name)

donald <- search_tweets("@realDonaldTrump",n=8000,lang="en")
donald <- clean_text(donald)
donaldsent <- sentiment_by(donald$text)
donald$score<- donaldsent$ave_sentiment

donaldagre <- aggregate(donald["score"],by=donald["created_at"],mean)

donaldfilt <- donald %>% filter(!is_retweet==TRUE)
donaldfiltag <- aggregate(donaldfilt["score"],by=donaldfilt["created_at"],mean)
summary(donaldsent)
wordsobama2 <- wordsobama2 %>% select(negative=unlist.wordsobama.negative.)
wordsobama <-  extract_sentiment_terms(obama2$text)
wordsobama2 <- data.frame(unlist(wordsobama$negative),unlist(wordsobama$neutral),unlist(wordsobama$positive),na.omit())

count(wordsobama2,negative,sort=TRUE)

sentimentplot <- function (df){
  
  
   df <- clean_text(df)
  df2 <- sentiment_by(df$text)
  df$score <- df2$ave_sentiment
  df <- df %>% mutate(Tipo = ifelse(score > 0, "Positivo", "Negativo"))
  
  #fig <- plot_ly(data=count(df,Tipo), labels = ~Tipo, values = ~n, type = 'pie')
  
  dfagre <- aggregate(df["score"],by=df["created_at"],mean)
  dfagre <- dfagre %>% mutate(MediaR = rollmean(score, k = 21, align = "right", na.pad = TRUE))
  
  fig <- plot_ly(data=dfagre, x=~created_at,y= ~score,type = 'scatter', mode = 'lines')  %>% 
add_trace(y = ~MediaR, name = 'Media Movil',mode = 'lines')
 
 return(fig)
 
}

#sentimentplot(donald)

donald<- clean_text(donald)

wordsdonald <-  extract_sentiment_terms(donald$text)
wordsobama2 <- wordsobama2 %>% select(negative=unlist.wordsobama.negative.)

wordsdonaldN <- data.frame(Negative=unlist(wordsdonald$negative))#,unlist(wordsobama$neutral),unlist(wordsobama$positive),na.omit())
wordsdonaldNN <- data.frame(Neutral=unlist(wordsdonald$neutral))
wordsdonaldP <- data.frame(Positive=unlist(wordsdonald$positive))
wordsdonaldjoin<-cbind.zoo(wordsdonaldN,wordsdonaldNN)
count(wordsdonaldjoin,Neutral,Negative,sort=TRUE)
count(wordsobama2,negative,sort=TRUE)
wordsdonaldjoin2 <- Map(merge, wordsdonaldN, wordsdonaldP)
