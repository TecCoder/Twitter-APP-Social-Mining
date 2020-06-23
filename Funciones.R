authentication <- function (){
api_key <- "leG8otLsNt4344LHPvBJZPGX7"
api_secret_key <- "12bvZcWDp8RaooR3Gypo39VCjJS7ZSzXlLeWhiikVfB5pwARHL"
access_token <- "239048889-sKudn7Y4ZKxRuY1GTymKsueWLWnjcJM8UpuhXEQk"
access_token_secret <- "DfXPH6Aw1ngJdK5U0tmMogTNotRSZapr6KqTiOqHHDsmZ"

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
      unnest_tokens(word, text) ## Si la condición se cumple (TRUE)
    df <- df %>%
      anti_join(custom_stop_words) %>%
      filter(!word=="t.co" %in% word)
    
  } else {
    df  <- df %>%
      select(status_id,text) %>%
      unnest_tokens(paired_words, text, token = "ngrams", n = 2) ## Si la condición no se cumple (FALSE)
    
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
    add_trace(y = ~MediaR, name = 'Media Movil',mode = 'lines')
  
  
  return(fig)
  
}
sentimentpie <- function (df){
  
  
  df <- clean_text(df)
  df2 <- sentiment_by(df$text)
  df$score <- df2$ave_sentiment
  df <- df %>% mutate(Tipo = ifelse(score > 0, "Positivo", "Negativo"))
  
  fig2 <- plot_ly(data=count(df,Tipo), labels = ~Tipo, values = ~n, type = 'pie')
  
 
  
  return(fig2)
  
}



funwordcloud2 <- function(df,n,z){
  
  #df <- df %>% plain_tweets() # Esta función es útil, si solo trabajas con tweets en inglés
  #df$text <- gsub("[^a-zA-Z0-9]","",df$text) #NADA
  #df$text  <- gsub("[^\x01-\x7F]", "", df$text) # Esta función elimina todos los simbolos ASCII excepto los especificados
  df$text  <- gsub("http.*","",  df$text) # Remueve cualquier tipo de enlace
  df$text <- sapply(df$text,function(row) iconv(row, "UTF-8", "ASCII//TRANSLIT", sub="")) #Elimina emoticonos y acéntos transformando todo a ASCII. 
  #df$text = gsub("http\\w+", "", df$text)
  
  #df$text <- gsub("https://t.co/[A-Za-z\\d]+|&amp;","", df$text)
  #df$text <- gsub("//t.co/.*","" ,df$text)
  #df$text= gsub("[[:digit:]]", "", df$text)
  
  df$text= gsub("@\\w+", "", df$text) # Remueve las menciones dentro de cada tweet
  #df$text  <- gsub("[[:punct:]]", "", df$text) #Remueve estos simbolos "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
  
  if(n==1) {
    df <- df %>%
      select(c(screen_name,text)) %>%      #Si no hacemos select podemos destriparlo simplemente para tener todo en una palabra por fila y tweet
      unnest_tokens(word, text) ## Si la condición se cumple (TRUE)
    df <- df %>%
      anti_join(custom_stop_words) %>%
      filter(!word=="t.co" %in% word)
    
  } else {
    df  <- df %>%
      select(screen_name,text) %>%
      unnest_tokens(paired_words, text, token = "ngrams", n = 2) ## Si la condición no se cumple (FALSE)
    
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
  
  set.seed(1234) # for reproducibility 
  
  
  
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
  #sp2$text  <- gsub("[[:punct:]]", "", sp2$text) #Remueve estos simbolos "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
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
  
  fig <- plot_ly(dfden,x=~x,y=~y, type = 'scatter', mode = 'lines')
  
  return(fig)
  
}

custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))


