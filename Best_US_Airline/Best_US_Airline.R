# Name: Sanzida Parvin

# Read the 3 csv airline's files of sentiment analysis
Alaska <- read.csv("C:/Users/...../Alaska_SA.csv")
options(scipen=999)
head(Alaska)
length(Alaska$Sentiment)

JetBlue <- read.csv("C:/Users/tuhin/...../JetBlue_SA.csv")
head(JetBlue)
length(JetBlue$Sentiment)

Southwest <- read.csv("C:/Users/...../Southwest_SA.csv")
head(Southwest)
length(Southwest$Sentiment)

# get the polarity of sentiments
sentiment <- function(x){
  like = length(which(x == "Like"))
  dislike = length(which(x == "Dislike"))
  neutral = length(which(x == "Neutral"))
  return(c(like, dislike, neutral))
}
senti_alaska = sentiment(Alaska$Sentiment)
senti_jetblue = sentiment(JetBlue$Sentiment)
senti_jetblue
senti_SW = sentiment(Southwest$Sentiment)
senti_SW

# make tables for sentiments of different airlines
table_alaska = table(Alaska$Sentiment); table_alaska
table_jetblue = table(JetBlue$Sentiment); table_jetblue
table_southwest = table(Southwest$Sentiment); table_southwest

# Create BarPlots to show the polarities of the sentiments
par(mfrow = c(1,3))
color <- c("blue", "darkred", "gold")
barplot(table_alaska, col = color, main = "Tweets of Alaska Airline", xlab = "Sentiments", 
        ylab = "Number of Tweets", ylim = c(0,900), names.arg = c("Dislike", "Like", "Neutral"), 
        legend.text = c("Dislike", "Like", "Neutral"), las = 1, space = 0)

barplot(table_jetblue, col = color, main = "Tweets of JetBlue Airline", xlab = "Sentiments", 
        ylab = "Number of Tweets", ylim = c(0,900), names.arg = c("Dislike", "Like", "Neutral"), 
        legend.text = c("Dislike", "Like", "Neutral"), las = 1, space = 0)

barplot(table_southwest, col = color, main = "Tweets of Southwest Airline", xlab = "Sentiments", 
        ylab = "Number of Tweets", ylim = c(0,900), names.arg = c("Dislike", "Like", "Neutral"), 
        legend.text = c("Dislike", "Like", "Neutral"), las = 1, space = 0)
par(mfrow = c(1,1))

# Functon to get the percentages of different polarities
percentage <- function(x){
  dislike_per = (length(which(x == "Dislike"))/length(x))*100
  like_per = (length(which(x == "Like"))/length(x))*100
  neutral_per = (length(which(x == "Neutral"))/length(x))*100
  return (c(like_per, dislike_per, neutral_per))
}

# Calculate the percentages of the sentiment polarities
per_alaska = round(as.numeric(percentage(Alaska$Sentiment)),2)
per_jetblue = round(as.numeric(percentage(JetBlue$Sentiment)),2)
per_southwest = round(as.numeric(percentage(Southwest$Sentiment)),2)

par(mfrow = c(1,3))
plot_A = barplot(percentage(Alaska$Sentiment), col = color, main = "Sentiment Analysis of Alaska", xlab = "Sentiments", 
        ylab = "Percentage of Sentiment", ylim = c(0,100), names.arg = c("Like", "Dislike", "Neutral"), 
        legend.text = c("Like", "Dislike", "Neutral"), las = 1, space = 0)
text(x = plot_A, y = per_alaska, label = per_alaska, pos = 3, cex = 1.2, col = "darkblue")

plot_JB = barplot(percentage(JetBlue$Sentiment), col = color, main = "Sentiment Analysis of JetBlue", xlab = "Sentiments", 
        ylab = "Percentage of Sentiment", ylim = c(0,100), names.arg = c("Like", "Dislike", "Neutral"), 
        legend.text = c("Like", "Dislike", "Neutral"), las = 1, space = 0)
text(x = plot_JB, y = per_jetblue, label = per_jetblue, pos = 3, cex = 1.2, col = "darkblue")

plot_SW = barplot(percentage(Southwest$Sentiment), col = color, main = "Sentiment Analysis of Southwest", xlab = "Sentiments", 
        ylab = "Percentage of Sentiment", ylim = c(0,100), names.arg = c("Like", "Dislike", "Neutral"), 
        legend.text = c("Like", "Dislike", "Neutral"), las = 1, space = 0)
text(x = plot_SW, y = per_southwest, label = per_southwest, pos = 3, cex = 1.2, col = "darkblue")
par(mfrow = c(1,1))

likes <- function(x){
  return (length(which(x == "Like")))
}
percent_like <-function(x){
  return ((length(which(x == "Like"))/length(x))*100)
}

like_count = c(likes(Alaska$Sentiment), likes(JetBlue$Sentiment), likes(Southwest$Sentiment))
like_count
percent_count = round(c(percent_like(Alaska$Sentiment), percent_like(JetBlue$Sentiment), percent_like(Southwest$Sentiment)),2)
percent_count

# Plot the positive percentages of different airlines
barplot(like_count, col = c("olivedrab1", "olivedrab3", "olivedrab4"), main = "Positive Tweets of the Airlines", 
        xlab = "Airlines", ylab = "Number of Likes", ylim = c(0,900), 
        names.arg = c("Alaska", "JetBlue", "Southwest"), las = 1, space = 0.5)
plot_per = barplot(percent_count, col = c("olivedrab1", "olivedrab3", "olivedrab4"), main = "Percentage of Positive Tweets of the Airlines", 
        xlab = "Airlines", ylab = "Percentage of Likes", ylim = c(0,100), 
        names.arg = c("Alaska", "JetBlue", "Southwest"), las = 1, space = 0.5)
text(x = plot_per, y = percent_count, label = percent_count, pos = 3, cex = 1.2, col = "darkblue")

prop_like = 1/3
prop_like

# Chi-square test for equally likely positive sentiment for all three airlines
chisq.test(like_count)

"
  Chi-squared test for given probabilities
  
  data:  like_count
  X-squared = 75.125, df = 2, p-value < 0.00000000000000022
"

# two sided proportion test to determine that Alaska got higher positive tweets
prop1 = likes(Alaska$Sentiment)
prop1
total_tweet = (length(Alaska$Sentiment) + length(JetBlue$Sentiment) + length(Southwest$Sentiment))
total_tweet
p = 1/3
test1 = prop.test(prop1, n = total_tweet, p = p, alternative = "greater")
test1
"
  	1-sample proportions test with continuity correction
  
  data:  prop1 out of total_tweet, null probability p
  X-squared = 629.8, df = 1, p-value = 1
  alternative hypothesis: true p is greater than 0.3333333
  95 percent confidence interval:
   0.1319156 1.0000000
  sample estimates:
          p 
  0.1410865 
"

prop2 = likes(JetBlue$Sentiment)
prop2
test2 = prop.test(prop2, n = total_tweet, p = p, alternative = "greater")
test2

"
  1-sample proportions test with continuity correction
  
  data:  prop2 out of total_tweet, null probability p
  X-squared = 459.86, df = 1, p-value = 1
  alternative hypothesis: true p is greater than 0.3333333
  95 percent confidence interval:
  0.1591372 1.0000000
  sample estimates:
  p 
  0.1690401 
"

prop3 = likes(Southwest$Sentiment)
prop3
test3 = prop.test(prop3, n = total_tweet, p = p, alternative = "greater")
test3

"
  1-sample proportions test with continuity correction
  
  data:  prop3 out of total_tweet, null probability p
  X-squared = 204.87, df = 1, p-value = 1
  alternative hypothesis: true p is greater than 0.3333333
  95 percent confidence interval:
  0.2125686 1.0000000
  sample estimates:
  p 
  0.2236287 
"


