# Uber - Supply Demand Gap
# Learning Outcome: EDA & Visualisation
# Expected output: R file & presentation in PDF format (zip together)
# Author: Poovarasan

# Objective:
# ----------
# Identify root cause of cancellation & non-availability of cars & recommand ways to improve the situation
# Expectation:
# 1. root cause analysis
# 2. possible hypothesis
# 3. recommand ways to improve

# install depenencies (One time execution)
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('chron')

# load packages
  library('dplyr')
  library('tidyr')
  library('ggplot2')

# -------------------------
# EDA Step-1: Data Sourcing
# -------------------------
  trips <- read.csv('Uber Request Data.csv', stringsAsFactors = F)

# Data set contains only July 2016 trips
# Observastions contains only the City - Airport & vice versa trips

  head(trips)
  str(trips)


# --------------------------
# EDA Step-2: Data cleaning:
# --------------------------
  
  # I. Fix rows: NA (All rows are already cleaned)
  
  # II. Fix columns:
    # a. Add column names if missing - NA
    # b. Rename columns consistently - NA
    # c. Delete unnecessary columns - NA
    # e. Split columns for more data - formatting Request.timestamp & Drop.timestamp first & splitting them to date, weekday, time etc
    # f. Merge columns for identifiers - NA
    # g. Align misaligned columns -formatting the Request.timestamp & Drop.timestamp will automatically aligns these columns
  
  # III. Fix missing values: NA (No missing values except the Drop.timestamp, but Drop.timestamp has NAs only for cancelled/no cars available rows)
  
  # IV. Standardise values:
    # a. check outliers & remove them
  
  # V. Correct invalid values & derive new variables: 
    # a. Format Request timestamp & Drop timestamp date formats
      formatDateTime <- function(variable)
      {
        
        # standardise the format of date string (day, month, year separators)
        variable <- gsub(variable, pattern = '-', replacement = '/')
        
        # format date time
        variable <- strptime(variable, '%d/%m/%Y %H:%M')
        return (as.POSIXct(variable))
      }
      
      trips$Request.timestamp <- formatDateTime(trips$Request.timestamp)
      trips$Drop.timestamp <- formatDateTime(trips$Drop.timestamp)
      
    
    # b. Derive new variables (Split columns into multiple columns for more insights)
    # Split Request.timestamp & Drop.timestamp into Request.date, Request.day, Request.time etc.
      
      trips$Request.date <- format(trips$Request.timestamp, '%Y/%m/%d')
      trips$Request.day <- format(trips$Request.timestamp, '%A')
      trips$Request.time <- format(trips$Request.timestamp, '%H:%M')
      trips$Request.hour <- as.integer(format(trips$Request.timestamp, '%H'))
      
      trips$Drop.time <- format(trips$Drop.timestamp, '%H:%M')
      trips$Drop.hour <- format(trips$Drop.timestamp, '%H')
      
      trips$Approx.travel.time <- round(difftime(trips$Drop.timestamp, trips$Request.timestamp, units = 'hours'), 2)
      
    # Create time slot variable
    # Assumption for time slots made from the following link
    # reference: http://www.angelfire.com/pa/pawx/time.html
      
      # 12AM - 2AM, 2AM - 4AM, 4AM - 6AM, 6AM - 8AM, 8AM - 10AM, 10AM - Noon
      #, 12PM - 2PM, 2PM - 4PM, 4PM - 6PM, 6PM - 8PM, 8PM - 10PM, 10PM - Midnight
      time_slopts <- c('Early overnight','Mid overnight','Late overnight','Early morning','Mid morning','Late morning'
                       ,'Early afternoon','Mid afternoon','Late afternoon','Early evening','Mid evening','Late evening')
      
      trips$Request.time_slot <- sapply(trips$Request.hour, function(hr) {
        index = ceiling(hr/2)
        if(index == 0) {
          index = 1
        }
        return(time_slopts[index])
      })
  

# -------------------------------
# EDA Step-3: Univariate analysis
# -------------------------------
    # a. Metadata description
      # 1.Request id: A unique identifier of the request
      # Time of request: The date and time at which the customer made the trip request
      # Drop-off time: The drop-off date and time, in case the trip was completed 
      # Pick-up point: The point from which the request was made
      # Driver id: The unique identification number of the driver
      # Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
      
    # b. Identify types of variables - identify ordered, unordered categorical variables & quantitative variables
      # Unordered categorical variables = Pickup.point, Status, Request.day
      # Ordered categorical variables = Request.timestamp, Drop.timestamp and all of the derived variables from these two variables
      # quantitative variables = Driver.id & Request.id
      
    # b. Data distribution plots
      # Unordered categorical variables - univariate analysis
        # Pickup.point univariate analysis
          trips_count_by_Pickup.point <- trips %>% 
            count(Pickup.point) %>%
            mutate(freq = round(n / sum(n), digits = 4) * 100)
          colnames(trips_count_by_Pickup.point) <- c('Pickup.point', 'Count', 'Percentage')
          
        # Status univariate analysis
          trips_count_by_Status  <- trips %>% 
            count(Status) %>%
            mutate(freq = round(n / sum(n), digits = 4) * 100)
          colnames(trips_count_by_Status) <- c('Status', 'Count', 'Percentage')
          
        # Request.day univariate analysis
          trips_count_by_Request.day <- trips %>% 
            count(Request.day) %>%
            mutate(freq = round(n / sum(n), digits = 4) * 100)
          colnames(trips_count_by_Request.day) <- c('Request.day', 'Count', 'Percentage')
          # Possible hypotheses (All day having nearly same percentage of requests)
          
        # Request.time_slot univariate analysis
          trips_count_Request.time_slot <- trips %>% 
            count(Request.time_slot) %>%
            mutate(freq = round(n / sum(n), digits = 4) * 100)
          colnames(trips_count_Request.time_slot) <- c('Request.time_slot', 'Count', 'Percentage')
          trips_count_Request.time_slot <- trips_count_Request.time_slot[order(trips_count_Request.time_slot$Percentage), ]
          
        # Request.hour univariate analysis
          trips_count_by_Request.hour <- trips %>% 
            count(Request.hour) %>%
            mutate(freq = round(n / sum(n), digits = 4) * 100)
          colnames(trips_count_by_Request.hour) <- c('Request.hour', 'Count', 'Percentage')
          trips_count_by_Request.hour <- trips_count_by_Request.hour[order(trips_count_by_Request.hour$hour), ]
          
      # Ordered categorical variables - univariate analysis
          # Request.timestamp
            Request.timestamp_univariate_analysis <- trips %>% group_by(Request.timestamp) %>% summarise(requests = length(Request.timestamp))
            ggplot(Request.timestamp_univariate_analysis, aes(Request.timestamp, requests)) + geom_line()
            
          # Request.date
            Request.date_univariate_analysis <- trips %>% group_by(Request.date) %>% summarise(requests = length(Request.date))
        

# -------------------------------
# EDA Step-4: Segmented analysis
# -------------------------------


# -------------------------------
# EDA Step-5: Bivariate analysis
# -------------------------------

# ---------------------------
# EDA Step-6: Derived metrics
# ---------------------------


# --------------------
# 4. Results expected:
# --------------------
  # 1. visually identify the most pression problems
    # Identify the most problamatic type of requests - City to Airport OR Airport to City?
      airtport2city_trips <- trips[trips$Pickup.point == 'Airport' & trips$Status != 'Trip Completed', ]
      city2airport_trips <- trips[trips$Pickup.point == 'City' & trips$Status != 'Trip Completed', ]
    # a. No Cars Available
    # b. Cancelled trips
      airtport2city_trips_summary <- airtport2city_trips %>% group_by(Status) %>% summarise(TripsCount = length(Status))
      city2airport_trips_summary <- city2airport_trips %>% group_by(Status) %>% summarise(TripsCount = length(Status))
    
    # Identify the time slots of cancelled trips
    # a. No Cars Available
    # b. Cancelled trips
      airtport2city_trips_timeslots <- airtport2city_trips %>% 
        group_by(Request.hour_string, Status) %>%
        summarise(TripsCount = length(Request.hour_string))
      
      city2airport_trips_timeslots <- city2airport_trips %>%
        group_by(Request.hour_string, Status) %>%
        summarise(TripsCount = length(Request.hour_string))
      
      # todo: show in percentage/percentile
      # can we show airport - city, city - airport in same plot?
      ggplot(airtport2city_trips_timeslots, aes(x=Request.hour_string, y=TripsCount, fill=Status)) + geom_bar(stat='identity')
    
    # Identify the time slots of No Cars Available
    # a. No Cars Available
    # b. Cancelled trips
  
  
  # 2. find out gap between supply and demand & show them using plots
    # Find the time slots when the highest gap exists
    # Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
  
  # 3. what do you think the reason for the issue - supply demand? describe in words with plot
  
  # 4. recommand some ways to resolve the supply - demand gap

