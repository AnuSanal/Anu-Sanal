library(rvest)
library(XML)
library(magrittr)
# Flipkart review of Realme 5 pro phone(6/128 GB)
flipurl <- "https://www.amazon.in/Redmi-Note-Pro-Gamma-Storage/product-reviews/B07X1KSLBV/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber"
flip_reviews <- NULL
for (i in 1:15){
  furl <- read_html(as.character(paste(flipurl,i,sep="=")))
  rev <- furl %>%
    html_nodes(".review-text") %>%
    html_text()
  flip_reviews <- c(flip_reviews,rev)

}
write.table(flip_reviews,"redmenote5pro.txt",row.names = F)

View(flip_reviews)
install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

install.packages("topicmodels")
library(topicmodels)
x <- readLines("redmenote5pro.txt")
x
length(x)

mydata.corpus <- Corpus(VectorSource(x))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)

## build a term-document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3

dim(mydata.dtm3)

# dtm <- as.DocumentTermMatrix(mydata.dtm3)
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(mydata.dtm3)

rowTotals <- apply(dtm, 1, sum)
?apply

dtm.new   <- dtm[rowTotals > 0, ]
dim(dtm.new)

lda <- LDA(dtm.new, 10) # find 10 topics
?LDA

term <- terms(lda, 20) # first 5 terms of every topic
term

tops <- terms(lda)
?terms
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)
?unlist

cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
?hclust
par(family = "HiraKakuProN-W3")
plot(cls)
?par
######

####################### Emotion mining ##############################

install.packages("syuzhet")
library("syuzhet")

f_text <- readLines("redmenote5pro.txt")

f_v <- get_sentences(f_text)
class(f_v)
str(f_v)
head(f_v)

sentimental_vector <- get_sentiment(f_v, method = "bing")
head(sentimental_vector)

afinn_f_v <- get_sentiment(f_v, method = "afinn")
head(afinn_f_v)

nrc_vectorf <- get_sentiment(f_v, method="nrc")
head(nrc_vector)

sum(sentimental_vector)
mean(sentimental_vector)
summary(sentimental_vector)

# plot
plot(sentimental_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- f_v[which.min(sentimental_vector)]
negative

# and to extract the most positive sentence
positive <- f_v[which.max(sentimental_vector)]
positive

# more depth
poa_vf <- f_text
poa_sentf <- get_sentiment(poa_vf, method="bing")
plot(
  poa_sentf, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_valsf <- get_percentage_values(poa_sentf)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sentf, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)
