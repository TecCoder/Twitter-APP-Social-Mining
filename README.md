# Twitter-APP-Social-Mining
![main](/images/main.PNG)
## Project Overview
The Twitter App social-Mining is a tool to perform queries to Twitter and retrieve useful information with 4 different kinds of visualizations. The 4 main functions of this app are:
1. Wordcloud
2. Dynamic table
3. Sentiment Analysis
4. Topic Modeling

## APP
You can find the application online on: [shiniapps.io](https://davidalvaro.shinyapps.io/Tweetanalyser/)

Notice that the online version might crash when using some functions such as Topic Modeling, so you should try the off-line version instead.

## Install off-line version
1. The app is built on top of R so the first step is to install the more recent version:[CRAN](https://cran.r-project.org/)
2. Install all the packages: 
`install.packages("tidytext")` 
Some packages may require to install through github to get the last update: ` library(devtools) install_github('')` 
3. Download the app script, you can choose either beetween the single file `app.R` or the 2 file version `ui.R` and `server.R`. You can download the `functions.R` and run it before the `app.R`. 2 file version contain the functions as well, in order to run it, place both files in a folder, open `server.R` and run first all the functions to the environment, then run the following code for authentication to the API:  `token <- authentication()`
and click on Run app button.

![runapp](/images/runapp.PNG)

## 1. Wordcloud
Search for a query on Twitter and get a visual summary of the published content. 
Parameters:
* Query to search: You can either look for single words or hashtag #Election. To search for a particular phrase enclose between backslash
   `\ The Phrase \`
* Number of tweets: Up to 10000
* Number of words: Choose beetween 1 or 2 word cluster.
* Zoom in: Zoom in the wordcloud and make the letters bigger.
* Language: Choose beetween english or spanish

![wordcloud](/images/wordcloud.PNG)

## 2. Tweets
Search for a query or an username account and retrieve the last tweets published, filter out by popularity, published date or containing text.
Parameters: You can choose wether you want to include Retweets for your analysis or not.

![table](/images/table.PNG)

## 3. Sentiment Analysis
Analyse the sentiment on twitter for a specific topic or an account. Analyse the sentiment score distribution, polarity proportion  and time series of the tweets published (Only English) Sentiment score obtained from the [sentimentr](https://github.com/cran/sentimentr/) package. 

![sentiment1](/images/sentiment1.PNG)
![sentiment2](/images/sentiment2.PNG)

## 4. Topic Modeling
Use Latent Dirichlet allocation to clasify words contained on tweets in differents topics. You can search for topic or username tweets, although username search won't return great results.
Parametres:
* Query to search: You may search for just on query or 2, 1 query search is better since it does not overload the application.
* Number of topics: This is number of topics that the algorithm will clasify the words into.
* Number of tweets: Recommendation is to search for less than 4000 so it doesn't overload, but it might depend on your computer.
* Choose the n-gram: Choose beetween 1 or 2 words cluster

![topicmodel1](/images/topicmodel1.PNG)

![topicmodel2](/images/topicmodel2.PNG)

Beta represent the likelyhood of belonging to that topic, therefore words with the highest probabilities are showed.

## About the Author
David Álvaro Martínez : Electronic Engineer, data science and information technology enthusiastic. [Linkedin](linkedin.com/in/david-álvaro-martínez-68667b127)

Project developed as Master Thesis at the University of Granada [UGR](https://lsi.ugr.es/lsi/postgrado/mgtpn) with [Carlos Molina Fernández](https://www.ujaen.es/departamentos/dinformatica/contactos/molina-fernandez-carlos) Tutoring
