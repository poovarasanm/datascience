##### Sales Forecasting ####
# Learning Outcome: Time Series
# Expected output: R file & presentation in PDF format (zip together)
# Author: Poovarasan

#### Objective ####
# To forcast the next six month sales & demand

# install depenencies (One time execution)
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('forecast')
  install.packages('FinCal') # To find the coefficient.variation

#### load packages ####
  library('dplyr')
  library('tidyr')
  library('ggplot2')
  library('forecast')
  library('graphics')
  library('FinCal')


#### 1: Data Sourcing & understanding ####

  # Please keep the "Global Superstore.csv" file at the same location of this script & set the working directory manually
  sales <- read.csv('Global Superstore.csv', stringsAsFactors = F)

  # Data set contains 24 attributes of the online transaction
  head(sales)
  str(sales)
  glimpse(sales)
    # Observations: 51,290
    # Variables: 24
  
  # Find NAs
    colSums(is.na(sales))
    # Postal.Code - 80% obs. has NA
  # has duplicate cols
    length(colnames(sales) == unique(colnames(sales))) # 24 = no. of variables
    # No duplicate columns
    
  # find duplicate rows
    length(unique(sales$Row.ID)) != length(sales$Row.ID) # False - No


#### 2: Data cleaning ####

# I. Fix columns:
  # Delete unnecessary columns - NA
    # Let's remove the postal code column, since 80% of it are NA
    sales <- sales[, -12]
    # Remove Row.id column
    sales <- sales[, -1]
    # Let's remove Product.name & Product.Id, since its not required for the final modal
    sales <- sales[, -c(13, 16)]
    # Let's remove Customer.Id & Customer.Name, not required for final modal
    sales <- sales[, -c(5, 6)]
    # Let's remove Order.Id, not required for final modal
    sales <- sales[, -1]
# II. Convert the data type to valid data types
    cols <- c("Ship.Mode", "Segment",  "City", "State", "Country", "Market",        
              "Region",         "Category",       "Sub.Category",   "Order.Priority")
    # convert the char class to factors
    sales[cols] <- lapply(sales[cols], tolower)
    # first lowercase all below columns
    sales[cols] <- lapply(sales[cols], as.factor)

# III. Convert date cols to date format
    sales$Order.Date <- as.Date(sales$Order.Date, format = "%d-%m-%Y")
    sales$Ship.Date <- as.Date(sales$Ship.Date, format = "%d-%m-%Y")
    
# IV. Sort the data frame by order.date
    sales <- sales[order(sales$Order.Date),]

# V. Derive new variables:
  # Split order date into day, month & year
      sales <- separate(sales, Order.Date, into=c('Order.Year', 'Order.Month', 'Order.Day'), sep = '-', remove = FALSE)
      sales$Order.Day <- as.integer(sales$Order.Day)
      sales$Order.Month <- as.integer(sales$Order.Month)
      sales$Order.Year <- as.integer(sales$Order.Year)
      
  # Create new column called 'Order.Month.Number' - i.e, month number starting from 1, 2, 3... 12, 13,..24...
  # 2011th first month = 1
  # 2011th 12th month = 12
  # 2012th first month = 13 etc. upto year 2014
  # this column will be used for the model building
      start_year <- min(sales$Order.Year)
      sales$MonthNo <- (sales$Order.Year - start_year) * 12 + sales$Order.Month
    
# Final data verification
    head(sales)
    str(sales)
    glimpse(sales)
    summary(sales)

####3: Univariate analysis ####

# a. Identify types of variables - identify ordered, unordered categorical variables & quantitative variables
# Unordered categorical variables = Ship.Mode, Segment, City, State, Country, Marker, Region, Cagetory, Sub category, Priority
# Ordered categorical variables = Order date, Ship date
# Continuous variables - Sales, Quantity, Discount, Profit, Shipping cost

# b. Data distribution plots
# categorical variables - univariate analysis
    sales %>% ggplot(aes(sales$Order.Date)) + geom_bar() # No. of transactions per Date
    sales %>% ggplot(aes(sales$Order.Day)) + geom_bar() # No. of transactions per Day
    sales %>% ggplot(aes(sales$Order.Month)) + geom_bar() # No. of transactions per Month
    sales %>% ggplot(aes(sales$Order.Year)) + geom_bar() # No. of trans. per year
    sales %>% ggplot(aes(sales$Ship.Date)) + geom_bar() # No. of shipping per date
    sales %>% ggplot(aes(sales$Segment)) + geom_bar() # No. of trans per segment
    sales %>% ggplot(aes(sales$Market)) + geom_bar() # No. of trans per market

# c. Continuous variables
    sales %>% ggplot(aes(sales$Quantity)) + geom_histogram(stat="count")
    sales %>% ggplot(aes(sales$Discount)) + geom_histogram(stat="count")
    sales %>% ggplot(aes(sales$Shipping.Cost)) + geom_histogram(stat="count")

#### 4: Bivariate analysis ####
    #### 4.1. Market with various attributes ####
    # a. Market vs Order.Day
      sales %>%
        ggplot(aes(Order.Day, fill=Market)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=1))
    # b. Market vs Order.Month
      sales %>%
        ggplot(aes(Order.Month, fill=Market)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=1))
    # c. Market vs Order.Year
      sales %>%
        ggplot(aes(Order.Year, fill=Market)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # d. Market vs Segment
      sales %>%
        ggplot(aes(Market, fill=Segment)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # e. Market vs Sales
      sales %>% group_by(sales$Market) %>% summarise(total_sales = sum(Sales)) %>% arrange(desc(total_sales))
      ggplot(sales, aes(x=Market, y=Sales)) + 
        geom_line(aes(group=Market))+
        stat_summary(fun.y = sum, color = 'black', geom ='bar')
    # f. Market vs Quantity
      sales %>% group_by(sales$Market) %>% summarise(total_orders = sum(Quantity)) %>% arrange(desc(total_orders))
      ggplot(sales, aes(x=Market, y=Quantity)) + 
        geom_line(aes(group=Market))+
        stat_summary(fun.y = sum, color = 'black', geom ='bar')
    # g. Market vs Profit
      sales %>% group_by(sales$Market) %>% summarise(total_profit = sum(Profit)) %>% arrange(desc(total_profit))
      ggplot(sales, aes(x=Market, y=Profit)) + 
        geom_line(aes(group=Market))+
        stat_summary(fun.y = sum, color = 'black', geom ='bar')
      
    ##### 4.2. Category with various attributes ####
    # a. Category vs Order.Day
      sales %>%
        ggplot(aes(Order.Day, fill=Category)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # b. Category vs Order.Month
      sales %>%
        ggplot(aes(Order.Month, fill=Category)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # c. Category vs Order.Year
      sales %>%
        ggplot(aes(Order.Year, fill=Category)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # d. Category vs Shipping mode
      sales %>%
        ggplot(aes(Category, fill=Ship.Mode)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # e. Category vs Sub Category
      sales %>%
        ggplot(aes(Category, fill=Sub.Category)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # f. Category vs Sales
      sales %>% group_by(sales$Category) %>% summarise(total_sales = sum(Sales)) %>% arrange(desc(total_sales))
    # g. Category vs Quantity
      sales %>% group_by(sales$Category) %>% summarise(total_quantities = sum(Quantity)) %>% arrange(desc(total_quantities))
    # h. Category vs Discount
      sales %>% group_by(sales$Category) %>% summarise(total_discounts = sum(Discount)) %>% arrange(desc(total_discounts))
    # i. Category vs Profit
      sales %>% group_by(sales$Category) %>% summarise(total_profit = sum(Profit)) %>% arrange(desc(total_profit))
    
#### 5: Multivariate & Segmented analysis ####
      # Let's analyse the data by 21 subsets (7 markets * 3 segment)
        
      # 1. Find the total transactions of each market by segment
        market_segment_summary <- sales %>%
          group_by(sales$Market, sales$Segment) %>%
          summarise(Transactions = n(),
                    Quantities = sum(Quantity),
                    Sales= sum(Sales),
                    Profit = sum(Profit)) %>%
          arrange(desc(Profit))
        colnames(market_segment_summary) <- c('Market', 'Segment', 'TotalOrders', 'TotalQuantities', 'TotalSales', 'TotalProfit')
        market_segment_summary
      
#### 6. EDA summary ####
        # 1. Top 2 profitable segments are consumer
        # 2. Top 2 profitable segments are APAC & EU
        
#### 7. Find the 2 most profitable segments using CV ####
        
        buckets <- sales %>%
          group_by(sales$MonthNo, sales$Market, sales$Segment) %>%
          summarise(Transactions = n(),
                    Quantities = sum(Quantity),
                    Sales= sum(Sales),
                    Profit = sum(Profit))
        colnames(buckets) <- c('MonthNo', 'Market', 'Segment', 'TotalOrders', 'TotalQuantities', 'TotalSales', 'TotalProfit')
        markets <- unique(buckets$Market)
        segments <- unique(buckets$Segment)
        top_cv = 100
        top_2_cv = 100
        top_bucket = NA
        top_2_bucket = NA
        for(market in markets) {
          for(segment in segments) {
            current_bucket <- buckets[buckets$Market == market & buckets$Segment == segment, ]
            sdev <- sd(current_bucket$TotalProfit)
            avg <- mean(current_bucket$TotalProfit)
            cv <- sdev/avg
            print(paste("Market=", market, ", Segment=", segment, ", Coefficient of Variance=", cv, top_cv, top_2_cv))
            
            if(cv < top_2_cv) {
              top_cv <- top_2_cv
              top_bucket <- top_2_bucket
              top_2_cv <- cv
              top_2_bucket <- current_bucket
            }
          }
        }
        print("Top 2 coefficient of variance values:")
        top_cv # 0.6321323 - APAC - consumer
        top_2_cv # 0.6243052 - EU - consumer
        
      
#### 9. Modal building ####
        
      #### 8.1 Creation of Modal ####
      #### 8.2 Modal evluation ####
      
#### 10. Conclusion ####
      

