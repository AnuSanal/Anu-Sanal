books <- read.csv(file.choose())
View(books)
library("recommenderlab")
library(caTools)
str(books)
books <- books[,-1:-2]
hist(books$ratings...3.)
book_rating_data_matrix <- as(books,'realRatingMatrix')
View(book_rating_data_matrix)
 

# recomendation using popularity
book_rec_model1 <- Recommender(book_rating_data_matrix, method = "POPULAR")

  #prediction for 6 users
rec_items1 <- predict(book_rec_model1,book_rating_data_matrix[1000:1005, n=3])
as(rec_items1, "list")

rec_items4 <- predict(book_rec_model1,book_rating_data_matrix[1000:1005], n=3 , type = "ratings")
as(rec_items4, "matrix")

rec_items5 <- predict(book_rec_model1,book_rating_data_matrix[1000:1005], n=3 , type = "topNList")
as(rec_items5, "matrix")


#popularity based recommendation is same for almost all the users. so we improe our recommendation based on collaborative filterings

    #Item Based Collaborative Filtering
#IBCF method
book_rec_model2 <- Recommender(book_rating_data_matrix, method = "IBCF")
  

  #prediction for 6 users

rec_items2 <- predict(book_rec_model2, book_rating_data_matrix[1000:1001], n =6)
as(rec_items2, "list")

#UBCF method (User based)

book_rec_items3 <- Recommender(book_rating_data_matrix, method = "UBCF")




# prediction

rec_items3 <- predict(book_rec_items3,book_rating_data_matrix[1000:1001],n = 3)
as(rec_items3, "matrix")






