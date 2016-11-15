
########################################################################################################################################################################
###    Author: Shatrunjai Singh
###
###    Title: Sentiment_Analysis_Modi_Demonitization.R
###
###    Date: 11/11/2016
###
###    Dataset: Tweets extracted from twitter
###
###    Purpose: Analyzing the sentiment of Modi goverments via tweets across cities in India (Cities=Bangalore	Delhi	Kolkata	Lucknow	Mumbai)
###
########################################################################################################################################################################

#Set working directory
setwd("C:/Users/sinshat/Documents/text_mining_and_web_scraping")

#Install pcakges
install.packages("twitteR")
install.packages("ROAuth")
require("RCurl")
install.packages("stringr")
install.packages("tm")
install.packages("ggmap")
install.packages("dplyr")
install.packages("plyr")
install.packages("tm")
install.packages("wordcloud")

#Load packages
library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)


#Twitter Authorization
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "nqhZAqkUAr3onlXSTgY8HLEza"
consumerSecret = "ASK2gSxhDE4iNqbd66Oq2Z3KDZj7bUpruXLKWy0afsvLn50KGj"

accessToken = "1950518124-Uhs8osbQ95bYzeXPWCMnxKvjVBiuSkSl41NjW2L"
accessSecret = "bSgAVaCD8ZHoZ5fGLFQI20snH9EuAad5sZPY6rxGJ6E2l"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)


#Get sample tweets
N=50  # tweets to request from each query
S=10  # radius in miles
lats=c(12.96999514,	28.6699929,	22.4949693,	26.85503908,	19.01699038)

lons=c(77.56000972,	77.23000403,	88.32467566,	80.91499874,	72.8569893)
#cities=Bangalore	Delhi	Kolkata	Lucknow	Mumbai

modi=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Modi+demonitization',
                                                                      lang="en",n=N,resultType="recent",
                                                                     geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))
#Extracting additional information from tweets
modilat=sapply(modi, function(x) as.numeric(x$getLatitude()))
modilat=sapply(modilat, function(z) ifelse(length(z)==0,NA,z))  

modilon=sapply(modi, function(x) as.numeric(x$getLongitude()))
modilon=sapply(modilon, function(z) ifelse(length(z)==0,NA,z))  

modidate=lapply(modi, function(x) x$getCreated())
modidate=sapply(modidate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

moditext=sapply(modi, function(x) x$getText())
moditext=unlist(moditext)
moditext<- sapply(moditext,function(row) iconv(row, "latin1", "ASCII", sub=""))

isretweet=sapply(modi, function(x) x$getIsRetweet())
retweeted=sapply(modi, function(x) x$getRetweeted())
retweetcount=sapply(modi, function(x) x$getRetweetCount())

favoritecount=sapply(modi, function(x) x$getFavoriteCount())
favorited=sapply(modi, function(x) x$getFavorited())

data=as.data.frame(cbind(tweet=moditext,date=modidate,lat=modilat,lon=modilon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

#Creating word cloud
# Create corpus
corpus=Corpus(VectorSource(data$tweet))

# Convert to lower-case
corpus=tm_map(corpus,tolower)
corpus$text <- sapply(corpus$text ,function(row) iconv(row, "latin1", "ASCII", sub=""))

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

#Creating word cloud
col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=15, scale=c(5,1),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

#Getting location of tweets
data=filter(data, !is.na(lat),!is.na(lon))
lonlat=select(data,lon,lat)

result <- do.call(rbind, lapply(1:nrow(lonlat),
                                function(i) revgeocode(as.numeric(lonlat[i,1:2]))))

#Some text cleaning:
tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet)
data$tweet=tweet

#reading list of positive and negative words
positives= readLines("positivewords.txt")
negatives = readLines("negativewords.txt")

#Further processing
sentiment_scores = function(tweets, positive_words, negative_words, .progress='none')
  {
  scores = laply(tweets,
                 function(tweet, positive_words, negative_words){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweet = sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words, positive_words)
                   negative.matches = match(words, negative_words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive_matches)
                   negative_matches = !is.na(negative_matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_matches, negative_matches, .progress=.progress )
  return(scores)
}

#Testing tweets
tweet1="I hate this world everything is so bad"
score = sentiment_scores(tweet1, positives, negatives, .progress='text')
data$score=score

#Plot histogram
hist(score,xlab=" ",main="Sentiment of sample tweetsn that have Donald Trump in them",
     border="black",col="skyblue")

