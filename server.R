#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rtweet)
library(tidytext)
library(dplyr)
library(wordcloud2)
library(reactable)
library(glue)
library(tm)
library(zoo)
#library(lubridate)
library(plotly)
library(stm)
library(quanteda)
library(sentimentr)
library(shinycssloaders)


authentication <- function (){
    api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    api_secret_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    
    ## authenticate via web browser
    token <- create_token(
        app = "sandboxMGTPN",
        consumer_key = api_key,
        consumer_secret = api_secret_key,
        access_token =access_token,
        access_secret=access_token_secret)
    
    return(token)
}


TopicModeler <- function(df,n) {
    
    df <- clean_text(df)
    
    df <- df %>% select(status_id,text) %>%
        unnest_tokens(word,text)
    
    df <- df %>% anti_join(custom_stop_words)
    
    dfdfm <-df %>% count(status_id,word,sort=TRUE) %>%
        
        cast_dfm(status_id, word, n)
    
    topicmodel <-  stm(dfdfm, K = n, 
                       verbose = FALSE, init.type = "Spectral")
    
    tm_beta <- tidy(topicmodel)
    
    
    tm_beta %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        mutate(topic = paste0("Topic ", topic),
               term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = as.factor(topic))) +
        geom_col(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free_y") +
        coord_flip() +
        scale_x_reordered() +
        labs(x = NULL, y = expression(beta),
             title = "Highest word probabilities for each topic",
             subtitle = "Different words are associated with different topics")
    
}

TopicModeler2 <- function(df,n,z) {
    
    df <- clean_text(df)
    
    if(z==1) {
        df <- df %>%
            select(c(status_id,text)) %>%      #Si no hacemos select podemos destriparlo simplemente para tener todo en una palabra por fila y tweet
            unnest_tokens(word, text)  
        df <- df %>%
            anti_join(custom_stop_words) %>%
            filter(!word=="t.co" %in% word)
        
    } else {
        df  <- df %>%
            select(status_id,text) %>%
            unnest_tokens(paired_words, text, token = "ngrams", n = 2) 
        
        df <- df %>%
            separate(paired_words, c("word1", "word2"), sep = " ")
        df <- df %>%
            filter(!word1 %in% custom_stop_words$word) %>%
            filter(!word2 %in% custom_stop_words$word)
        df <- df %>%
            unite(word, word1, word2, sep = " ")
        
    }
    
    dfdfm <-df %>% count(status_id,word,sort=TRUE) %>%
        
        cast_dfm(status_id, word, n)
    
    topicmodel <-  stm(dfdfm, K = n, 
                       verbose = FALSE, init.type = "Spectral")
    
    tm_beta <- tidy(topicmodel)
    
    
    tm_beta %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        mutate(topic = paste0("Topic ", topic),
               term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = as.factor(topic))) +
        geom_col(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free_y") +
        coord_flip() +
        scale_x_reordered() +
        labs(x = NULL, y = expression(beta),
             title = "Highest word probabilities for each topic",
             subtitle = "Different words are associated with different topics")
    
}

sentimentplot <- function (df){
    
    
    df <- clean_text(df)
    df2 <- sentiment_by(df$text)
    df$score <- df2$ave_sentiment
    df <- df %>% mutate(Tipo = ifelse(score > 0, "Positivo", "Negativo"))
    
    #fig2 <- plot_ly(data=count(df,Tipo), labels = ~Tipo, values = ~n, type = 'pie')
    
    dfagre <- aggregate(df["score"],by=df["created_at"],mean)
    dfagre <- dfagre %>% mutate(MediaR = rollmean(score, k = 21, align = "right", na.pad = TRUE))
    
    fig <- plot_ly(data=dfagre, x=~created_at,y= ~score,type = 'scatter', mode = 'lines')  %>% 
        add_trace(y = ~MediaR, name = 'Media Movil',mode = 'lines') %>%
        layout(title = "Sentiment of the user over time",
               xaxis = list(title = "Time"),
               yaxis = list (title = "Score"))
    
    
    return(fig)
    
}

sentimentuser <- function(df){
    df <- clean_text(df)
    dfsent <- sentiment_by(df$text)
    df$score <- dfsent$ave_sentiment
    df <- df %>% mutate(fecha=as.Date(format(created_at,"%Y-%m-%d")))
    df <- aggregate(df["score"],by=df["fecha"],mean)
    df <- df %>% mutate(MediaR = rollmean(score, k = 7, align = "right", na.pad = TRUE))
    
    fig <- plot_ly(data=df,x=~fecha,y=~score,name='Sentimiento',type = 'scatter', mode = 'lines')    %>%   
        add_trace(y = ~MediaR, name = 'Media movil k=7',mode = 'lines') %>%
        layout(title = "Sentiment of the user over time",
               xaxis = list(title = "Days"),
               yaxis = list (title = "Score"))
    
    return(fig)
}

sentimentpie <- function (df){
    
    
    df <- clean_text(df)
    df2 <- sentiment_by(df$text)
    df$score <- df2$ave_sentiment
    df <- df %>% mutate(Tipo = ifelse(score > 0, "Positivo", "Negativo"))
    
    fig2 <- plot_ly(data=count(df,Tipo), labels = ~Tipo, values = ~n, type = 'pie')%>%
        layout(title = "Polarity Pie Chart")
    
    
    
    return(fig2)
    
}



funwordcloud2 <- function(df,n,z){
    
    #df <- df %>% plain_tweets() # Esta funcion es util, si solo trabajas con tweets en ingles
    #df$text <- gsub("[^a-zA-Z0-9]","",df$text) #NADA
    #df$text  <- gsub("[^\x01-\x7F]", "", df$text) # Esta funcion elimina todos los simbolos ASCII excepto los especificados
    df$text  <- gsub("http.*","",  df$text) # Remueve cualquier tipo de enlace
    df$text <- sapply(df$text,function(row) iconv(row, "UTF-8", "ASCII//TRANSLIT", sub="")) #Elimina emoticonos y acentos transformando todo a ASCII. 
    
    #df$text = gsub("http\\w+", "", df$text)
    
    #df$text <- gsub("https://t.co/[A-Za-z\\d]+|&amp;","", df$text)
    #df$text <- gsub("//t.co/.*","" ,df$text)
    #df$text= gsub("[[:digit:]]", "", df$text)
    
    df$text= gsub("@\\w+", "", df$text) # Remueve las menciones dentro de cada tweet
    #df$text  <- gsub("[[:punct:]]", "", df$text) #Remueve estos simbolos ""
    
    if(n==1) {
        df <- df %>%
            select(c(screen_name,text)) %>%      #Si no hacemos select podemos destriparlo simplemente para tener todo en una palabra por fila y tweet
            unnest_tokens(word, text) ## Si la condicion se cumple (TRUE)
        df <- df %>%
            anti_join(custom_stop_words) %>%
            filter(!word=="t.co" %in% word)
        
    } else {
        df  <- df %>%
            select(screen_name,text) %>%
            unnest_tokens(paired_words, text, token = "ngrams", n = 2) ## Si la condicion no se cumple (FALSE)
        
        df <- df %>%
            separate(paired_words, c("word1", "word2"), sep = " ")
        df <- df %>%
            filter(!word1 %in% custom_stop_words$word) %>%
            filter(!word2 %in% custom_stop_words$word)
        df <- df %>%
            unite(word, word1, word2, sep = " ")
        
        
    }
    
    df <- df %>% 
        group_by(screen_name) %>%
        count(word, sort=TRUE)
    
    dfdtm <- df %>% cast_dtm(screen_name, word, n)
    
    dfmatrix <- as.matrix(dfdtm) 
    words <- sort(colSums(dfmatrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    #set.seed(1234) # for reproducibility 
    
    
    
    wordcloud2(df, size=z)
    
    
    
    
}

clean_text <- function (df) {
    
    #removes emoticons
    #df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #Elimina emoticonos
    #df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) 
    #df <- df %>% plain_tweets()
    
    df$text <- sapply(df$text,function(row) iconv(row, "UTF-8", "ASCII//TRANSLIT", sub="")) #Elimina emoticonos y acentos
    df$text  <- gsub("http.*","",  df$text) # Elimina cualquier tipo de enlace
    
    #df$text <- gsub("https://t.co/[A-Za-z\\d]+|&amp;","", df$text)
    #df$text <- gsub("https://t.co/*","",df$text)
    #df$text = gsub("http\\w+", "", df$text)
    
    df$text <- gsub("[[:digit:]]","" ,df$text)
    
    #df$text= gsub("[[:digit:]]", "", df$text)
    #sp2$text  <- gsub("[[:punct:]]", "", sp2$text) #Remueve estos simbolos ""
    #sp2$text <- sp2$text %>% iconv( from = 'UTF-8', to = 'ASCII//TRANSLIT') #Quita tildes pero elimina muchos tweets que empiezan con emoticono
    
    df$text= gsub("@\\w+", "", df$text)
    
    return(df)
    
}

make_url_html <- function(url) {
    if(length(url) < 2) {
        if(!is.na(url)) {
            as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
        } else {
            ""
        }
    } else {
        paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
}

densityplot <- function (df){
    df <- clean_text(df)
    df <- sentiment_by(df$text)
    d <- density(df$ave_sentiment)
    dfden <- data.frame(
        x= unlist(d$x),
        y= unlist(d$y)
        
        
    )
    
    fig <- plot_ly(dfden,x=~x,y=~y, type = 'scatter', mode = 'lines') %>%
        layout(title = "Polarity density distribution")
    
    return(fig)
    
}

custom_stop_words <- bind_rows(stop_words,   
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))


shinyServer(function(input, output) {
    token <- authentication()
    
    twtList<-reactive({twtList<-search_tweets(input$searchTerm,n=input$maxTweets,lang=input$lan) })
    twtList2 <- reactive ({twtList2 <- search_tweets(input$searchTerm2,n=input$maxTweets2,include_rts = input$RT)})
    twtList3 <- reactive ({ get_timeline(input$searchUser,n=input$maxTweetsUser,include_rts = input$RT2)})
    twtopic <- reactive ({search_tweets(input$topic1,n=input$maxtweettopic)})
    twtopic2 <- reactive ({search_tweets(input$topic2,n=input$maxtweettopic)  })
    twtopicusers <- reactive ({get_timeline(input$user1,n=input$maxtuser)})
    
    twsent <- reactive ({search_tweets(input$searchsent,n=input$maxTweetsent,lang="en")})
    twsentuser <- reactive ({get_timeline(input$searchsent2,n=input$maxTweetsent2)})
    
    twtopicut <- reactive ({bind_rows(req(twtopic()),req(twtopic2()))})
    #datal <-reactive({ funwordcloudsh(req(twtList()),input$inputnum,input$inputfrec,input$inputmax) })  
    
    output$wordcloud2 <- renderWordcloud2({
        
        funwordcloud2(req(twtList()), input$inputnum,input$size)
    }) 
    
    
    tweet_table_data <- reactive({
        req(twtList2())
        twtList2() %>%
            select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
            mutate(
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = purrr::map_chr(urls_expanded_url, make_url_html)
            )%>%
            select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })
    
    tweet_table_data2 <- reactive({
        req(twtList3())
        twtList3() %>%
            select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
            mutate(
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = purrr::map_chr(urls_expanded_url, make_url_html)
            )%>%
            select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })
    
    output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data2(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 URLs = colDef(html = TRUE)
                             )
        )
    })
    
    output$tweet_table2 <- renderReactable({
        reactable::reactable(tweet_table_data(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 URLs = colDef(html = TRUE)
                             )
        )
    })
    
    #sentimentplot(twsent())
    # fig <- reactive({sentimentplot(req(twsent()))})
    #fig2 <- reactive ({sentimentpie(req(twsent()))})
    #fig2 <- reactive({sentimentplot(twsent())})
    output$texto <- renderText("Hola")
    
    output$pie <- renderPlotly({fig2<-sentimentpie(req(twsent()))})
    
    output$density <- renderPlotly({fig <- densityplot(req(twsent())) }) 
    
    output$ts <- renderPlotly({fig<-sentimentplot(req(twsent()))})
    
    output$pie2 <- renderPlotly({fig2<-sentimentpie(req(twsentuser()))})
    
    output$density2 <- renderPlotly({fig <- densityplot(req(twsentuser())) }) 
    
    output$ts2 <- renderPlotly({fig<-sentimentuser(req(twsentuser()))})
    
    output$LDA <- renderPlot({TopicModeler2(twtopicut(),input$maxtopic,input$ngram)})
    
    output$LDA2 <- renderPlot({TopicModeler2(twtopicusers(),input$maxtopic2,input$ngram2)})

    })


