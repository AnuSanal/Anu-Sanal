phone_data <- read.csv(file.choose())

View(phone_data)
str(phone_data)


# converting everything into character format 
phone_data[] <- lapply(phone_data1,as.character)
View(phone_data)


# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
phone_data["new_col"] <- apply(phone_data,1,paste_fun)
View(phone_data1)


#  text manipulation and forming DTM and TDM matrices
library(tm)
x <- Corpus(VectorSource(phone_data$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Association Rules 

library(arules)
library(arulesViz)

# Item Frequecy plot

# count of each item from all the transactions 
barplot(sapply(dtm0_df,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules)
plot(rules)
plot(rules,method = "scatterplot") # 9 rules
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

rules1 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.99,minlen=2))

plot(rules1,method = "scatterplot") # 9 rules
plot(rules1,method = "grouped")
plot(rules1,method = "graph")
plot(rules1,method = "mosaic")

rules2 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.001,confidence=0.5,minlen=3))

plot(rules2,method = "scatterplot") #3 rules
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(rules2,method = "mosaic")

rules3 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.99,minlen=2))

plot(rules3,method = "scatterplot") # 9 rules
plot(rules3,method = "grouped")
plot(rules3,method = "graph")
plot(rules3,method = "mosaic")

#similarly we can change the support,confidence,minlen and can obtain different rules.