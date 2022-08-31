setwd('C:/Users/Nithin/Downloads/R - Retail Case study/R case study 1 (Retail)')

#**************************************************************************************************************#

#########################
#-->Required Packages<--#
#########################
require(dplyr)
require(ggplot2)
require(lubridate)

#**************************************************************************************************************#

#1. Merge the datasets Customers, Product Hierarchy and Transactions as Customer_Final. 
#   Ensure to keep all customers who have done transactions with us and select the join
#   type accordingly.
#   a.	Use the base merge()
#   b.	Dplyr merge functions
customers <- read.csv('Datasets/Customer.csv')
prod_cat <- read.csv('Datasets/prod_cat_info.csv')
Transactions <- read.csv('Datasets/Transactions.csv')

#Base merge
Customer_Final <- merge(x=Transactions,y=customers,by.x = 'cust_id',by.y = 'customer_Id')
Customer_Final <- merge(x=Customer_Final,y=prod_cat,by.x = 'prod_cat_code',by.y = 'prod_cat_code')

#dplyr merger
Customer_Fin <- dplyr::inner_join(x=Transactions,y=customers,by = c('cust_id'= 'customer_Id'))
Customer_Fin <- dplyr::inner_join(x=Customer_Fin,y=prod_cat,by = c('prod_cat_code'= 'prod_cat_code'))

#**************************************************************************************************************#

#2.	Prepare a summary report for the merged data set.
#   a.	Get the column names and their corresponding data types
#   b.	Top/Bottom 10 observations
#   c.	"Five-number summary" for continuous variables (min, Q1, median, Q3 and max)
#   d.	Frequency tables for all the categorical variables
#a.
str(Customer_Final) 
#b.
View(head(Customer_Final,10))
View(tail(Customer_Final,10))
#c.
summary(Customer_Final)
#d.
View(table(Customer_Final$prod_cat))

#**************************************************************************************************************#

#3.Generate histograms for all continuous variables and frequency bars for categorical variables.
numcols <- colnames(dplyr::select_if(Customer_Final,is.numeric))
numcols <- c('Qty','Rate','Tax','total_amt')

#Histogram
ggplot(Customer_Final)+aes(total_amt)+geom_histogram(binwidth = 500,color = 'Blue',show.legend = T)
ggplot(Customer_Final)+aes(Tax)+geom_histogram(binwidth = 200,color = 'black',show.legend = T)
ggplot(Customer_Final)+aes(Rate)+geom_histogram(binwidth = 200,color = 'black',show.legend = T)

#Frequency Bars
ggplot(Customer_Final)+aes(Store_type)+geom_bar(color = 'Blue',show.legend = T)
ggplot(Customer_Final)+aes(Gender)+geom_bar(color = 'Blue',show.legend = T,na.rm = T)
ggplot(Customer_Final)+aes(prod_cat)+geom_bar(color = 'Blue',show.legend = T)
ggplot(Customer_Final)+aes(prod_subcat)+geom_bar(color = 'Blue',show.legend = T)+coord_flip()

#**************************************************************************************************************#

#4.Calculate the following information using the merged dataset :
# a.Time period of the available transaction data
# b.Count of transactions where the total amount of transaction was negative
Customer_Final$tran_date <- lubridate::dmy(Customer_Final$tran_date)
Customer_Final$DOB <- lubridate::dmy(Customer_Final$DOB)

#a.
max(Customer_Final$tran_date)-min(Customer_Final$tran_date)

#b.
sum(Customer_Final$total_amt < 0)

#**************************************************************************************************************#

#5.Analyze which product categories are more popular among females vs male customers.

#Female
F_cust <- Customer_Final %>% dplyr::filter(Gender == 'F')
F_cust <- F_cust[,c('prod_cat','Gender')]
F_cust <- F_cust %>% dplyr::group_by(prod_cat) %>% summarise(cnt = n()) %>% arrange(desc(cnt))
head(F_cust,1) 

#Male
M_cust <- Customer_Final %>% dplyr::filter(Gender == 'M')
M_cust <- M_cust[,c('prod_cat','Gender')]
M_cust <- M_cust %>% dplyr::group_by(prod_cat) %>% summarise(cnt = n()) %>% arrange(desc(cnt))
head(M_cust,1) 

#**************************************************************************************************************#

#6.Which City code has the maximum customers and what was the percentage of customers from that city?
cust <- Customer_Final %>% dplyr::group_by(city_code) %>% summarise(cnt = n()) %>% arrange(desc(cnt))
cust <- cust %>% dplyr::mutate('Percent' = cnt/nrow(Customer_Final)*100 )

#**************************************************************************************************************#

#7.Which store type sells the maximum products by value and by quantity?
MAX <- Customer_Final %>% dplyr::group_by(Store_type) %>% 
  summarise(T_Qty = sum(Qty,na.rm = T),T_vale = sum(total_amt,na.rm = T))
MAX <- MAX %>% dplyr::arrange(desc(T_Qty)) %>% arrange(desc(T_vale))

#**************************************************************************************************************#

#8.What was the total amount earned from the "Electronics" and "Clothing" categories from Flagship Stores?
sum(Customer_Final[Customer_Final$Store_type =='Flagship store'& 
                     (Customer_Final$prod_cat=='Electronics'|Customer_Final$prod_cat=='Clothing'),'total_amt'],na.rm = T)

#**************************************************************************************************************#

#9.What was the total amount earned from "Male" customers under the "Electronics" category?
sum(Customer_Final[Customer_Final$Gender=='M'& 
                     Customer_Final$prod_cat=='Electronics','total_amt'],na.rm = T)

#**************************************************************************************************************#

#10.How many customers have more than 10 unique transactions, after removing all transactions 
#   which have any negative amounts?
unique <- Customer_Final[Customer_Final$total_amt > 0,]
unique <- unique %>% dplyr::group_by(cust_id) %>% summarise(cnt = n())
nrow(unique[unique$cnt >10,])

#**************************************************************************************************************#

#11.For all customers aged between 25 - 35, find out:
# a.What was the total amount spent for "Electronics" and "Books" product categories?
# b.What was the total amount spent by these customers between 1st Jan, 2014 to 1st Mar, 2014?

#a.
tday <- max(Customer_Final$tran_date)
cust_age <-  dplyr::mutate(Customer_Final,'Age' = as.numeric(round((tday - Customer_Final$DOB)/365.25,0)))

sum(cust_age[cust_age$Age>25 & cust_age$Age<35 & (cust_age$prod_cat=='Electronics'|
           cust_age$prod_cat=='Books'),'total_amt'],na.rm = T)

#b.
sum(cust_age[cust_age$Age>25 & cust_age$Age<35 & 
               (cust_age$tran_date>='2014-01-01'|cust_age$tran_date<='2014-03-01'),'total_amt'],na.rm = T)

#**************************************************************************************************************#