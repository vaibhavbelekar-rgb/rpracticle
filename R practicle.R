# Practical 2 : Data Structures and Control Structures in R

# Data Frame : Use this Data frame to solve following Questions
set.seed(123)

sales_data <- data.frame(
  OrderID = 1:100,
  CustomerName = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),100,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),100,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),100,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),100,replace=TRUE),
  Quantity = sample(1:10,100,replace=TRUE),
  Price = sample(seq(500,50000,500),100,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),100,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),100,replace=TRUE)
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount

head(sales_data)

# ------------------------------------------------------------------

# Q1) Filtering Rows
# 1.	Show orders where Price > 20000.
# 2.	Show orders where City = Pune.
# 3.	Show orders where Quantity > 5.
# 4.	Show orders where Category = Electronics.
# 5.	Combine two conditions.

#solutions
# 1.	Show orders where Price > 20000.
orders <-sales_data$Price>20000
high_value_orders <- sales_data[sales_data$Price > 20000, ]
head(high_value_orders)

# 2.	Show orders where City = Pune.
pune_orders <- sales_data[sales_data$City == "Pune", ]
head(pune_orders)
# 3.	Show orders where Quantity > 5.

orders5 <- sales_data[sales_data$ Quantity >5,]
head(orders5)
# 4.	Show orders where Category = Electronics.
electronics_orders <- sales_data[sales_data$Category == "Electronics", ]
head(electronics_orders)
# 5.	Combine two conditions.



# Q2) Sorting Data
# 1.	Sort dataset by Price.
# 2.	Sort dataset by Revenue.
# 3.	Sort by City alphabetically.
# 4.	Sort by Quantity descending.
# 5.	Sort by multiple columns.

# Solution:

# 1.	Sort dataset by Price.
sorted_data_price<- sales_data[order(sales_data$Price), ]
head(sorted_data_price)

# 2.	Sort dataset by Revenue.
sorted_revenue <- sales_data[order(sales_data$Revenue), ]
head(sorted_revenue)
# 3.	Sort by City alphabetically.
sorted_city <- sales_data[order(sales_data$City), ]
head(sorted_city)

# 4.	Sort by Quantity descending.

sorted_quantity_desc <- sales_data[order(-sales_data$Quantity), ]
head(sorted_quantity_desc)
# 5.	Sort by multiple columns.
sorted_multi <- sales_data[order(sales_data$City, -sales_data$Price), ]
head(sorted_multi)


# Q3) Aggregation
# 1.	Total revenue by city.
# 2.	Average price by category.
# 3.	Total quantity sold by product.
# 4.	Maximum order value by city.
# 5.	Minimum order value by product.

# Solution:

# 1.	Total revenue by city.
aggregate(FinalAmount ~ City, data = sales_data, sum)

# 2.	Average price by category.
aggregate(Price ~ Category, data = sales_data, mean)

# 3.	Total quantity sold by product.
aggregate(Quantity ~ Product, data = sales_data, sum)

# 4.	Maximum order value by city.
aggregate(FinalAmount ~ City, data = sales_data, max)

# 5.	Minimum order value by product.
aggregate(FinalAmount ~ Product, data = sales_data, min)


# Q4) Conditional Statements
# 1. If discount > 10 mark "High Discount".
# 2. Classify orders into High / Medium / Low value by using if else.
# 3. Print revenue of each order by using for loop.
# 4. Calculate cumulative revenue by using while loop
# 5. Skip fashion category. 

# Solution :
# 1. Mark High Discount
sales_data$DiscountFlag <- ifelse(sales_data$Discount > 10, "High Discount", "Normal")

# 2. Classify orders into High / Medium / Low value
sales_data$OrderValueCategory <- ifelse(sales_data$FinalAmount > 100000, "High",
                                        ifelse(sales_data$FinalAmount > 50000, "Medium", "Low"))


# 3. Print revenue of each order using for loop
for(i in 1:nrow(filtered_data)) {
  cat("OrderID:", filtered_data$OrderID[i],
      "- Revenue:", filtered_data$Revenue[i], "\n")
}

# 4. Calculate cumulative revenue using while loop
i <- 1
cumulative_revenue <- 0

while(i <= nrow(filtered_data)) {
  cumulative_revenue <- cumulative_revenue + filtered_data$Revenue[i]
  i <- i + 1
}

cat("Total Cumulative Revenue:", cumulative_revenue, "\n")

head(filtered_data)


# 5. Skip Fashion category (keep only Electronics)
filtered_data <- subset(sales_data, Category != "Fashion")

# Q5) Discount Analysis
# 1.	Find Average discount.
# 2.	Orders with discount > 10%.
# 3.	Total discount amount.
# 4.	City with highest discount.
# 5.	Count discount levels.

# Solution:

# Discount Analysis - All in One

# 1. Average discount
avg_discount <- mean(sales_data$Discount)
avg_discount
# 2. Orders with discount > 10%
high_discount_orders <- subset(sales_data, Discount > 10)

head(high_discount_orders)

# 3. Total discount amount
total_discount_amount <- sum(sales_data$DiscountAmount)
total_discount_amount
# 4. City with highest total discount
city_discount <- aggregate(DiscountAmount ~ City, data = sales_data, sum)
city_highest_discount <- city_discount[which.max(city_discount$DiscountAmount), ]
city_highest_discount
# 5. Count of discount levels
discount_levels_count <- table(sales_data$Discount)
discount_levels_count

# Q6) Multi Condition Filtering
# 1.	Electronics + Price > 20000
# 2.	Fashion + Quantity > 5
# 3.	Pune + Discount > 10
# 4.	Revenue > 50000 + Cash payment
# 5.	High price + high quantity

# Solution:


# 1. Electronics products with Price > 20000
electronics_high_price <- subset(sales_data, Category == "Electronics" & Price > 20000)
head(electronics_high_price, 5)
# 2. Fashion products with Quantity > 5
fashion_high_quantity <- subset(sales_data, Category == "Fashion" & Quantity > 5)
head(fashion_high_quantity, 5)
# 3. Orders from Pune with Discount > 10%
pune_high_discount <- subset(sales_data, City == "Pune" & Discount > 10)
head(pune_high_discount, 5)
# 4. Orders with Revenue > 50000 and PaymentMethod = Cash
high_revenue_cash <- subset(sales_data, Revenue > 50000 & PaymentMethod == "Cash")
 head(high_revenue_cash, 5)
# 5. High price and high quantity (define high as above 75th percentile)
price_threshold <- quantile(sales_data$Price, 0.75)
quantity_threshold <- quantile(sales_data$Quantity, 0.75)
high_price_high_quantity <- subset(sales_data, Price > price_threshold & Quantity > quantity_threshold)
head(high_price_high_quantity, 5)



