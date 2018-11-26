# the challenge
# https://www.kaggle.com/olistbr/brazilian-ecommerce

library(tidyverse)
library(sqldf)
library(caret)

# the data
product1 <- read.csv('brazilian-ecommerce/olist_products_dataset.csv')
item1 <- read.csv('brazilian-ecommerce/olist_order_items_dataset.csv')
order1 <- read.csv('brazilian-ecommerce/olist_orders_dataset.csv')
customer1 <- read.csv('brazilian-ecommerce/olist_customers_dataset.csv')

# query minimum viable dataset
data <- sqldf('
SELECT  customer1.customer_id AS c,
        product1.product_id AS p,
        product1.product_category_name AS cat
FROM    customer1 
JOIN    order1 
        ON customer1.customer_id = order1.customer_id
JOIN    item1
        ON order1.order_id = item1.order_id
JOIN    product1
        ON item1.product_id = product1.product_id
              ')

# Clean data to remove NA categories
clean.data <- data %>%
  filter(!is.na(cat)) %>% 
  filter(!cat=='') %>% 
  mutate_all(as.factor)

# Sum product purchases by customer
customer.orders <- clean.data %>% 
  group_by(c,p,cat) %>% 
  summarise(qty = n())

# most customers have only bought one item
customer.orders %>% 
  group_by(c) %>% 
  summarise(TotQty=sum(qty)) %>% 
  arrange(-TotQty) %>% 
  ggplot(.,aes(TotQty))+
  geom_histogram()

# Remove customer orders with only 1 purchase
customer.orders.rm1 <- customer.orders %>% 
  group_by(c) %>% 
  mutate(TotQty = sum(qty)) %>% 
  ungroup() %>% 
  arrange(-qty) %>% 
  filter(!TotQty==1)

# Check customers with TotQty > 1
customer.orders.rm1 %>% 
  group_by(c) %>% 
  mutate(TotQty = sum(qty)) %>% 
  ungroup() %>% 
  arrange(TotQty)

# Not interested in individual products but product categories
customer.categories <- customer.orders.rm1 %>% 
  group_by(c,cat) %>% 
  summarise(qty=n())

# next steps

# Create a customer/categories trends matrix
customer.trends <- customer.categories %>% 
  spread(c, qty)
customer.trends[is.na(customer.trends)] <- 0
customer.trends <- as.data.frame(customer.trends)
row.names(customer.trends) <- customer.trends$cat

# scale purchase quantities before clustering
customer.trends.mat <- as.matrix(customer.trends[,-1]) 
customer.trends.mat <- prop.table(customer.trends.mat, margin = 2)
customer.trends <-  as.data.frame(customer.trends.mat)

# Perform Hierarchical Cluster analysis
# http://www.sthda.com/english/wiki/print.php?id=237
d <- dist(customer.trends[,-1], method = "euclidean")
customer.cluster <- hclust(d, method = "complete")

# Dendogram shows gradual step down in similarity.
# Therefore data is too sparse over the product categories.
# The gradual steps down represent a simple overlap in common customer purchases.
plot(customer.cluster)

