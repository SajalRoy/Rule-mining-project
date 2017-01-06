#set directory

setwd("C:/Users/SAJAL/Desktop/PGDDA/Predictive Analytics 2/Association Rule Mining")

#loading the required packages

library(arules)

#Reading the data into data frame

global_data <- read.csv("Global Superstore.csv")

#separate the order id and Sub-category fields and store

temp_data <- global_data[,c(2,17)]

#export this data frame into a csv file

write.csv(temp_data, file = "global_basket.csv", row.names = FALSE)

basket_data <- read.transactions("global_basket.csv", format = "single",sep=",", cols = c("Order.ID","Sub.Category"))

#Plotting for item frequency

itemFrequencyPlot(basket_trans_data, topN = 10,type = "absolute")
summary(basket_trans_data)

#From summary of the transaction data we can see
#Number of elements in item sets transaction
#And most frequent items in the transaction
#Now applying apriori algorithm to generate rules

basket_rules <- apriori(basket_data, parameter = list(support = 0.001, confidence = 0.80))

#From above support and confidence we can see that there are no rules generated
#lets try with different parameters
#Reducing the values of support and confidence

basket_rules <- apriori(basket_data, parameter = list(support = 0.00016, confidence = 0.75, minlen = 3))
#sorting the rules 

basket_rules_sorted <- sort(basket_rules, by = "confidence", decreasing = TRUE)
inspect(basket_rules_sorted)

#Checking for the presence of the top frequent items which we got know from the graph we plotted earlier
#Checking for purchase of Binder category items

basket_binders <- subset(basket_rules_sorted, subset = rhs %in% "Binders"  & confidence >= 0.80)
inspect(basket_binders)

#Checking for storage category items

basket_storage <- subset(basket_rules_sorted, subset = rhs %in% "Storage" & confidence >= 0.80)
inspect(basket_storage)

#Checking for Art category items
basket_art <- subset(basket_rules_sorted, subset = rhs %in% "Art" & confidence >= 0.75)
inspect(basket_art)

#From the above rules what i can infer is that global store should send recommendations for category "Binders"(office supplies)
#for every purchase of Accessories, bookcases  
#it should send recommendations for "Art"items if the user buys chair or Furnishings or Bookcases or Fasteners
# Minimum Support  = 0.00016
# Lift Value = 4 and above
#Confidence = 75% and above


