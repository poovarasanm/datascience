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

#### load packages ####
  library('dplyr')
  library('tidyr')
  library('ggplot2')
  library('forecast')
  library('graphics')


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
              "Region",         "Category",       "Sub.Category",   "Sales", "Order.Priority")
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
    sales %>% ggplot(aes(sales$Order.Date)) + geom_bar()
    sales %>% ggplot(aes(sales$Order.Day)) + geom_bar()
    sales %>% ggplot(aes(sales$Order.Month)) + geom_bar(binwidth = 1)
    sales %>% ggplot(aes(sales$Order.Year)) + geom_bar()
    sales %>% ggplot(aes(sales$Ship.Date)) + geom_bar()
    sales %>% ggplot(aes(sales$Ship.Mode)) + geom_bar()
    sales %>% ggplot(aes(sales$Segment)) + geom_bar()
    sales %>% ggplot(aes(sales$Market)) + geom_bar()
    sales %>% ggplot(aes(sales$Region)) + geom_bar()
    sales %>% ggplot(aes(sales$Category)) + geom_bar()
    sales %>% ggplot(aes(sales$Sub.Category)) + geom_bar()
    sales %>% ggplot(aes(sales$Order.Priority)) + geom_bar()

# c. Continuous variables
    sales %>% ggplot(aes(sales$Quantity)) + geom_histogram(stat="count")
    sales %>% ggplot(aes(sales$Discount)) + geom_histogram(stat="count")
    sales %>% ggplot(aes(sales$Shipping.Cost)) + geom_histogram(stat="count")
    # Sales, Quantity & Profit are taking long time to draw the plots, so skipping it for now

#### 4: Bivariate analysis ####

sales %>%
  filter(Status != 'Trip Completed') %>%
  ggplot(aes(Pickup.point, fill=Status)) + geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))

# Identify the most problematic the time slots
sales %>%
  filter(Status != 'Trip Completed') %>%
  ggplot(aes(Request.time_slot, fill=Status)) + geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))


# 2. find out gap between supply and demand & show them using plots
# Find the time slots when the highest gap exists
sales %>%
  ggplot(aes(Request.time_slot, fill=Status)) + geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))

# Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
problematic_time_slots <- c('Late night', 'Early morning', 'Morning', 'Evening', 'Late evening', 'Early night')
sales %>%
  filter(Request.time_slot %in%  problematic_time_slots) %>%
  ggplot(aes(x=Pickup.point, fill=Status)) +   
  geom_bar(position='stack', stat='count') +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5)) +
  facet_wrap(~Request.time_slot)

ggplot(sales, aes(x=Request.day, fill=Status)) +   
  geom_bar(position='stack', stat='count') +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5)) +
  facet_wrap(~Pickup.point)


# Find the frequency of cancellation of drivers
sales %>%
  filter(Driver.id <= 100) %>%
  ggplot(aes(Driver.id, fill=Status)) +
  ylab("Cancelled requests") +
  xlab("Driver id") +
  geom_bar(stat='count') +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))

#### 5: Multivariate & Segmented analysis ####

