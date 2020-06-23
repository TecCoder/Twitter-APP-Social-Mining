library(shiny)
library(shinydashboard)
library(tidyverse)
library(rtweet)
library(tidytext)

library(wordcloud2)
library(reactable)
library(glue)
library(tm)
library(plotly)
library(stm)
library(quanteda)

token <- authentication()

save.image(file="appshiny.RData")  # Ejecutar este comando si quieres guardar las variables de entorno generadas

ui <- dashboardPage(
  dashboardHeader(title = "Tweet Miner"),
  dashboardSidebar( sidebarMenu(
    menuItem("Wordcloud", tabName = "wordcloud", icon = icon("dashboard")),
    menuItem("Tweets", tabName = "widgets", icon = icon("th")),
    menuItem("Sentimiento",tabName="Sentimiento", icon=icon("f5c0")),
    menuItem("Topic Modelling",tabName="TopicModel")
    
  )),
  
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "wordcloud",
              # Boxes need to be put in a row (or column)
              fluidRow(
                
                #??? wordcloud2Output('plot1'),    #, step = NA, width = NULL)
                
                
                box(
                  title=" Wordcloud",
                  wordcloud2Output('wordcloud2'),width="100%")
                #box(plotOutput("plot1", height = 450)),
                
              ),  
              fluidRow(    
                box(textInput("searchTerm", "Enter data to be searched with '#'", ""),
                    sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
                    submitButton(text="Analyse")),
                   # vecFun()),
                
                box(
                  title=" Escoge entre 1 palabra o 2",
                  numericInput("inputnum","Numero de Palabras", 1, min = 1, max = 2),
                  numericInput("size","Tamaño de la nube", 1, min = 1, max = 20),
                  selectInput("lan","Language",c("en","es"))
                  
                  #title = "Controls",
                  #sliderInput("slider", "Number of words:", value= 1,min= 1,max= 2)
                )
              )
              
      ),
      
      tabItem(tabName = "widgets", tabsetPanel( tabPanel("User", fluidRow(    
        
        box(textInput("searchUser","Enter screen name of user",""),
            sliderInput("maxTweetsUser","Number of recent tweets to use for analysis:",min=5,max=3200,value=500), 
            submitButton(text="Analyse")
        ))
        ,fluidRow(
          reactableOutput("tweet_table"))),
        tabPanel("Topic", fluidRow(    
          
          box(textInput("searchTerm2","Enter screen name of user",""),
              sliderInput("maxTweets2","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
              submitButton(text="Analyse")
          ))
          ,fluidRow(
            reactableOutput("tweet_table2")))
        
      )),
      
      tabItem(tabName = "Sentimiento",fluidRow( box(textInput("searchsent", "Enter query to be searched with", ""),
                                                    sliderInput("maxTweetsent","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
                                                    selectInput("RT","Incluir RT",c(TRUE,FALSE)),
                                                    submitButton(text="Analyse"))),
              fluidRow(
        
             box( plotlyOutput("pie")),box(plotlyOutput("density")) 
              
            ),
        fluidRow(plotlyOutput("ts"))
        ), #Parantesis que cierra el tabItem
      
     
     tabItem(tabName="TopicModel",  tabsetPanel( tabPanel( "Query",
             fluidRow(box(title=" '\" Phrase\"' To search for a whole Phrase",
                          textInput("topic1","Enter Topic 1",""),
                          textInput("topic2","Enter topic 2"),
                          sliderInput("maxtopic","Enter number of topics:",min=1,max=4,value=1), 
                          sliderInput("maxtweettopic","Enter the number of tweets:",min=1,max=10000,value=4000),
                          numericInput("ngram","Chosse the n-gram",min=1,max=2,value=1),
                          submitButton(text="Analyse")
                          )
                    
                      ),
             plotOutput("LDA")
             
             ),
             
             tabPanel("User",
              fluidRow(box(textInput("user1","Enter Topic 1",""),
              textInput("user2","Enter topic 2"),
              sliderInput("maxtopic2","Enter number of topics:",min=1,max=4,value=1), 
              sliderInput("maxtuser","Enter the number of tweets:",min=1,max=3200,value=1000),
              submitButton(text="Analyse")
                      )
                      
                      ),
                     
               plotOutput("LDA2")                     
                      
                      
                      
                      )
             
      )
     
     ) #El que cierra el tabItem
    )
    
  )
  
)

server <- function(input, output,session) {  
  
  
  twtList<-reactive({twtList<-search_tweets(input$searchTerm,n=input$maxTweets,lang=input$lan) })
  twtList2 <- reactive ({twtList2 <- search_tweets(input$searchTerm2,n=input$maxTweets2)})
  twtList3 <- reactive ({ get_timeline(input$searchUser,n=input$maxTweetsUser)})
  twtopic <- reactive ({search_tweets(input$topic1,n=input$maxtweettopic)})
  twtopic2 <- reactive ({search_tweets(input$topic2,n=input$maxtweettopic)  })
  twtopicusers <- reactive ({get_timeline(c(input$user1,input$user2),n=input$maxtuser)})
  
  twsent <- reactive ({search_tweets(input$searchsent,n=input$maxTweetsent,lang="en")})
  
  twtopicut <- reactive ({bind_rows(req(twtopic()),req(twtopic2()))})
  #datal <-reactive({ funwordcloudsh(req(twtList()),input$inputnum,input$inputfrec,input$inputmax) })  
  
  output$wordcloud2 <- renderWordcloud2({
    
    funwordcloud2(req(twtList()), input$inputnum,input$size)
  }) 
  number <- reactive(nrow(req(twtList)))
  vecFun <- renderPrint({print(paste0( "Número de tweets analizados= ",number() ))})
  
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
  
  output$LDA <- renderPlot({TopicModeler2(twtopicut(),input$maxtopic,input$ngram)})
  
  output$LDA2 <- renderPlot({TopicModeler(twtopicusers(),input$maxtopic2)})
  
  #output$plot <- renderPlotly({plot_ly(data = pruebaspread, x=~año_dia, y=~seidor, name = 'trace 0', type = 'scatter', mode = 'lines')})
  
}
shinyApp(ui, server)      


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

custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

tabItem(tabName = "Topic Model", plotOutput("LDA")  )


prueba <- search_tweets("\"Elon Musk\" OR  Covid19",n=1000)

ds <- Map(
  "search_tweets",
  c("\"data science\"", "rstats OR python"),
  n = 1000
)

TopicModeler2(extremadura,3,2)
