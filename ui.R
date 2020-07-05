#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI( dashboardPage(
    dashboardHeader(title = "Tweet Miner"),
    dashboardSidebar( sidebarMenu(
        menuItem("Wordcloud", tabName = "wordcloud", icon = icon("dashboard")),
        menuItem("Tweets", tabName = "widgets", icon = icon("table")),
        menuItem("Sentimemt",tabName="Sentimiento", icon = icon("bar-chart-o")),
        menuItem("Topic Modelling",tabName="TopicModel", icon = icon("th"))
        
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
                            wordcloud2Output('wordcloud2')%>% withSpinner(),width="100%")
                        #box(plotOutput("plot1", height = 450)),
                        
                    ),  
                    fluidRow(    
                        box(textInput("searchTerm", "Enter data to be searched ", ""),
                            sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
                            submitButton(text="Analyse")),
                        # vecFun()),
                        
                        box(
                            title=" Choose between 1 or 2 words",
                            numericInput("inputnum","Number of words", 1, min = 1, max = 2),
                            numericInput("size","Zoom in", 1, min = 1, max = 20),
                            selectInput("lan","Language",c("en","es"))
                            
                            #title = "Controls",
                            #sliderInput("slider", "Number of words:", value= 1,min= 1,max= 2)
                        )
                    )
                    
            ),
            
            tabItem(tabName = "widgets", tabsetPanel( tabPanel("User", fluidRow(    
                
                box(textInput("searchUser","Enter screen name of user",""),
                    sliderInput("maxTweetsUser","Number of recent tweets to use for analysis:",min=5,max=3200,value=500), 
                    checkboxInput("RT2", "Include Retweets", TRUE),
                    submitButton(text="Analyse")
                ))
                ,fluidRow(
                    reactableOutput("tweet_table")%>% withSpinner())),
                tabPanel("Topic", fluidRow(    
                    
                    box(textInput("searchTerm2","Enter screen name of user",""),
                        sliderInput("maxTweets2","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
                        checkboxInput("RT", "Include Retweets", FALSE),
                        submitButton(text="Analyse")
                    ))
                    ,fluidRow(
                        reactableOutput("tweet_table2")%>% withSpinner()))
                
            )),
            
            tabItem(tabName = "Sentimiento", tabsetPanel( tabPanel("Topic",fluidRow( box(textInput("searchsent", "Enter query to be searched with", ""),
                                                                                         sliderInput("maxTweetsent","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
                                                                                         #selectInput("RT","Incluir RT",c(TRUE,FALSE)),
                                                                                         submitButton(text="Analyse"))),
                                                                   fluidRow(
                                                                       
                                                                       box( plotlyOutput("pie")%>% withSpinner()),box(plotlyOutput("density")%>% withSpinner()) 
                                                                       
                                                                   ),
                                                                   fluidRow(plotlyOutput("ts")%>% withSpinner())
            ), tabPanel("User",fluidRow( box(textInput("searchsent2", "Enter query to be searched with", ""),
                                             sliderInput("maxTweetsent2","Number of recent tweets to use for analysis:",min=5,max=10000,value=500), 
                                             #selectInput("RT","Incluir RT",c(TRUE,FALSE)),
                                             submitButton(text="Analyse"))),
                        fluidRow(
                            
                            box( plotlyOutput("pie2")%>% withSpinner()),box(plotlyOutput("density2")%>% withSpinner()) 
                            
                        ),
                        fluidRow(plotlyOutput("ts2")%>% withSpinner())))
            
            ), #Parantesis que cierra el tabItem
            
            
            tabItem(tabName="TopicModel",  tabsetPanel( tabPanel( "Query",
                                                                  fluidRow(box(title=" Enter the querys to search",
                                                                               textInput("topic1","Enter Topic 1",""),
                                                                               textInput("topic2","Enter topic 2"),
                                                                               sliderInput("maxtopic","Enter number of topics:",min=1,max=6,value=1), 
                                                                               sliderInput("maxtweettopic","Enter the number of tweets:",min=1,max=10000,value=4000),
                                                                               numericInput("ngram","Chosse the n-gram",min=1,max=2,value=1),
                                                                               submitButton(text="Analyse")
                                                                  )
                                                                  
                                                                  ),
                                                                  plotOutput("LDA")%>% withSpinner()
                                                                  
            ),
            
            tabPanel("User",
                     fluidRow(box(textInput("user1","Enter User 1",""),
                                
                                  sliderInput("maxtopic2","Enter number of topics:",min=1,max=4,value=1), 
                                  sliderInput("maxtuser","Enter the number of tweets:",min=1,max=3200,value=1000),
                                  numericInput("ngram2","Chosse the n-gram",min=1,max=2,value=1),
                                  submitButton(text="Analyse")
                     )
                     
                     ),
                     
                     plotOutput("LDA2") %>% withSpinner()                    
                     
                     
                     
            )
            
            )
            
            ) #El que cierra el tabItem
        )
        
    )
    
))
