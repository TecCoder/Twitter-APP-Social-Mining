# Twitter-APP-Social-Mining
![main](/images/main.PNG)
## Project Overview
The Twitter App social-Mining is a tool to perform queries to Twitter and retrieve useful information with 4 different kinds of visualizations. The 4 main functions of this app are:
1. Wordcloud
2. Dynamic table
3. Sentiment Analysis
4. Topic Modeling

## APP
You may find the application online on: 
Notice that the online version might crash when using some functions such as Topic Modeling, so you should try the off-line version instead.

## Install off-line version
1. The app is built on top of R so the first step is to install the more recent version:[CRAN](https://cran.r-project.org/)
2. Install all the packages: 
`install.packages("tidytext")` 
Some packages may require to install through github: ` library(devtools) install_github('')` 
3. Download the app script, you can choose either beetween the single file `app.R` or the 2 file version `ui.R` and `server.R`. You can download the `functions.R` and run it before the `app.R`. 2 file version contain the functions as well, in order to run it, place both files in a folder and click on Run app button.

![runapp](/images/runapp.PNG)
