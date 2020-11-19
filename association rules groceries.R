library(arules)
Groceries <- read.transactions(file.choose(),format = "basket")
View(Groceries)
inspect(Groceries[1:10])
class(Groceries)
itemFrequencyPlot(Groceries,topN = 15)
Groceries_rules <- apriori(Groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
library(arulesViz)
plot(Groceries_rules,method = "scatterplot") #118 rules
plot(Groceries_rules,method = "grouped")
plot(Groceries_rules,method = "graph")
plot(Groceries_rules,method = "mosaic")



Groceries_rules1 <- apriori(Groceries,parameter = list(support = 0.001,confidence = 0.05,minlen=3))

plot(Groceries_rules1,method = "scatterplot") #471 rules
plot(Groceries_rules1,method = "grouped")
plot(Groceries_rules1,method = "graph")
plot(Groceries_rules1,method = "mosaic")

Groceries_rules3 <- apriori(Groceries,parameter = list(support = 0.002,confidence = 0.09,minlen=3))

plot(Groceries_rules3,method = "scatterplot") #105 rules
plot(Groceries_rules3,method = "grouped")
plot(Groceries_rules3,method = "graph")
plot(Groceries_rules3,method = "mosaic")

Groceries_rules4 <- apriori(Groceries,parameter = list(support = 0.001,confidence = 0.09,minlen=3))

plot(Groceries_rules4,method = "scatterplot") #431 rules
plot(Groceries_rules4,method = "grouped")
plot(Groceries_rules4,method = "graph")
plot(Groceries_rules4,method = "mosaic")



#similarly we can change the support, confidence,and minimum length.
