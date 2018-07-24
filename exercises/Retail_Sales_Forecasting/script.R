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
    sales %>% ggplot(aes(sales$Ship.Mode)) + geom_bar() # Shipping mode counts
    sales %>% ggplot(aes(sales$Segment)) + geom_bar() # No. of trans per segment
    sales %>% ggplot(aes(sales$Market)) + geom_bar() # No. of trans per market
    sales %>% ggplot(aes(sales$Region)) + geom_bar() # No. of trans per region
    sales %>% ggplot(aes(sales$Category)) + geom_bar() # No. of trans per category
    sales %>% ggplot(aes(sales$Sub.Category)) + geom_bar() # No. of trans per sub category
    sales %>% ggplot(aes(sales$Order.Priority)) + geom_bar() # No. of trans per priority

# c. Continuous variables
    sales %>% ggplot(aes(sales$Quantity)) + geom_histogram(stat="count")
    sales %>% ggplot(aes(sales$Discount)) + geom_histogram(stat="count")
    sales %>% ggplot(aes(sales$Shipping.Cost)) + geom_histogram(stat="count")
    # Sales, Quantity & Profit are taking long time to draw the plots, so skipping it for now

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
    # d. Market vs shipping mode
      sales %>%
        ggplot(aes(Market, fill=Ship.Mode)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # e. Market vs Segment
      sales %>%
        ggplot(aes(Market, fill=Segment)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # f. Market vs Category
      sales %>%
        ggplot(aes(Market, fill=Category)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # g. Market vs Sub. Category
      sales %>%
        ggplot(aes(Market, fill=Sub.Category)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    # h. Market vs Sales
      sales %>% group_by(sales$Market) %>% summarise(total_sales = sum(Sales)) %>% arrange(desc(total_sales))
      ggplot(sales, aes(x=Market, y=Sales)) + 
        geom_line(aes(group=Market))+
        stat_summary(fun.y = sum, color = 'black', geom ='bar')
    # i. Market vs Quantity
      sales %>% group_by(sales$Market) %>% summarise(total_orders = sum(Quantity)) %>% arrange(desc(total_orders))
      ggplot(sales, aes(x=Market, y=Quantity)) + 
        geom_line(aes(group=Market))+
        stat_summary(fun.y = sum, color = 'black', geom ='bar')
    # j. Market vs Profit
      sales %>% group_by(sales$Market) %>% summarise(total_profit = sum(Profit)) %>% arrange(desc(total_profit))
      ggplot(sales, aes(x=Market, y=Profit)) + 
        geom_line(aes(group=Market))+
        stat_summary(fun.y = sum, color = 'black', geom ='bar')
    # l. Market vs Shipping cost
      sales %>% group_by(sales$Market) %>% summarise(total_shipping_cost = sum(Shipping.Cost)) %>% arrange(desc(total_shipping_cost))
    # m. Market vs Order priority
      sales %>%
        ggplot(aes(Market, fill=Order.Priority)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
      
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
    # h. Category vs Quantity
      sales %>% group_by(sales$Category) %>% summarise(total_quantities = sum(Quantity)) %>% arrange(desc(total_quantities))
    # i. Category vs Discount
      sales %>% group_by(sales$Category) %>% summarise(total_discounts = sum(Discount)) %>% arrange(desc(total_discounts))
    # j. Category vs Profit
      sales %>% group_by(sales$Category) %>% summarise(total_profit = sum(Profit)) %>% arrange(desc(total_profit))
    # k. Category vs Shipping cost
      sales %>% group_by(sales$Category) %>% summarise(total_shipping_cost = sum(Shipping.Cost)) %>% arrange(desc(total_shipping_cost))
    # l. Category vs Order Priority
      sales %>%
        ggplot(aes(Category, fill=Order.Priority)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
    
#### 5: Multivariate & Segmented analysis ####
      # Let's analyse the data by 21 buckets (7 markets * 3 categories)
        
      # 1. Find the total transactions of each market by categories
        trans_by_market_category <- sales %>% group_by(sales$Market, sales$Category) %>% summarise(total_transactions = n()) %>% arrange(desc(total_transactions))
        trans_by_market_category
      # 2. Find the total quantities of each market by categories
        quantities_by_market_category <- sales %>% group_by(sales$Market, sales$Category) %>% summarise(total_quants = sum(Quantity)) %>% arrange(desc(total_quants))
        quantities_by_market_category
      # 3. Find the total sales of each market by categories
        sales_by_market_category <- sales %>% group_by(sales$Market, sales$Category) %>% summarise(total_sales = sum(Sales)) %>% arrange(desc(total_sales))
        sales_by_market_category
      # 4. Find the total discount provided for each market by categories
        discounts_by_market_category <- sales %>% group_by(sales$Market, sales$Category) %>% summarise(total_discounts = sum(Discount)) %>% arrange(desc(total_discounts))
        discounts_by_market_category
      # 5. Find the total shipping cost for each market by categories
        shipping_costs_by_market_category <- sales %>% group_by(sales$Market, sales$Category) %>% summarise(total_shipping_cost = sum(Shipping.Cost)) %>% arrange(desc(total_shipping_cost))
        shipping_costs_by_market_category
      
#### 6. EDA summary ####
      
#### 7. Bucketing or segmentation ####
        buckets <- sales %>% group_by(sales$MonthNo, sales$Market, sales$Category) %>% summarise(TotalQuantity = sum(Quantity), TotalSales = sum(Sales), TotalProfit = sum(Profit))
        colnames(buckets) <- c('Month', 'Market', 'Category', 'TotalQuantity',  'TotalSales', 'TotalProfit')
        buckets
        
        africa_tech_sales <- buckets[buckets$Market == 'africa' & buckets$Category == 'technology', ]
        africa_office_supplies <- buckets[buckets$Market == 'africa' & buckets$Category == 'office supplies', ]
        africa_furniture <- buckets[buckets$Market == 'africa' & buckets$Category == 'furniture', ]
        
        apac_tech_sales <- buckets[buckets$Market == 'apac' & buckets$Category == 'technology', ]
        apac_office_supplies <- buckets[buckets$Market == 'apac' & buckets$Category == 'office supplies', ]
        apac_furniture <- buckets[buckets$Market == 'apac' & buckets$Category == 'furniture', ]
        
        canada_tech_sales <- buckets[buckets$Market == 'canada' & buckets$Category == 'technology', ]
        canada_office_supplies <- buckets[buckets$Market == 'canada' & buckets$Category == 'office supplies', ]
        canada_furniture <- buckets[buckets$Market == 'canada' & buckets$Category == 'furniture', ]
        
        emea_tech_sales <- buckets[buckets$Market == 'emea' & buckets$Category == 'technology', ]
        emea_office_supplies <- buckets[buckets$Market == 'emea' & buckets$Category == 'office supplies', ]
        emea_furniture <- buckets[buckets$Market == 'emea' & buckets$Category == 'furniture', ]
        
        eu_tech_sales <- buckets[buckets$Market == 'eu' & buckets$Category == 'technology', ]
        eu_office_supplies <- buckets[buckets$Market == 'eu' & buckets$Category == 'office supplies', ]
        eu_furniture <- buckets[buckets$Market == 'eu' & buckets$Category == 'furniture', ]
        
        latam_tech_sales <- buckets[buckets$Market == 'latam' & buckets$Category == 'technology', ]
        latam_office_supplies <- buckets[buckets$Market == 'latam' & buckets$Category == 'office supplies', ]
        latam_furniture <- buckets[buckets$Market == 'latam' & buckets$Category == 'furniture', ]
        
        us_tech_sales <- buckets[buckets$Market == 'us' & buckets$Category == 'technology', ]
        us_office_supplies <- buckets[buckets$Market == 'us' & buckets$Category == 'office supplies', ]
        us_furniture <- buckets[buckets$Market == 'us' & buckets$Category == 'furniture', ]
        
#### 8. Find the 2 most profitable segments ####
        markets <- unique(buckets$Market)
        categories <- unique(buckets$Category)
        top_cv = 0
        top_2_cv = 0
        for(market in markets) {
          for(category in categories) {
            current_bucket = buckets[buckets$Market == market & buckets$Category == category, ]
            current_bucket
            sdev <- sd(current_bucket$TotalProfit)
            avg <- mean(current_bucket$TotalProfit)
            cv <- coefficient.variation(sdev, avg)
            print(paste("Market=", market, ", Category=", category, ", Coefficient of Variance=", cv))
            if(cv > top_cv & cv > top_2_cv) {
              top_cv <- cv
              top_bucket <- current_bucket
            }
            if(cv > top_2_cv & cv < top_cv) {
              top_2_cv <- cv
              top_2_bucket <- current_bucket
            }
          }
        }
        print("Top 2 coefficient of variance values:")
        top_cv # 3.853189 - emea - furniture
        top_2_cv # 3.148373 - emea - technology
        
      
#### 9. Modal building ####
        emea_furniture_sales_ts <- ts(top_bucket$TotalSales)
        plot(emea_furniture_sales_ts)
        emea_furniture_quantities_ts <- ts(top_bucket$TotalQuantity)
        plot(emea_furniture_quantities_ts)
        emea_furniture_profit_ts <- ts(top_bucket$TotalProfit)
        plot(emea_furniture_profit_ts)
        
        emea_tech_sales_ts <- ts(top_2_bucket$TotalSales)
        plot(emea_tech_sales_ts)
        emea_tech_quantities_ts <- ts(top_2_bucket$TotalQuantity)
        plot(emea_tech_quantities_ts)
        emea_tech_profit_ts <- ts(top_2_bucket$TotalProfit)
        plot(emea_tech_profit_ts)
        
      #### 8.1 Creation of Modal ####
      #### 8.2 Modal evluation ####
      
#### 10. Conclusion ####
      

