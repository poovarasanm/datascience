# Uber - Supply Demand Gap
# Learning Outcome: EDA & Visualisation
# Expected output: R file & presentation in PDF format (zip together)

# Objective:
# ----------
# Identify root cause of cancellation & non-availability of cars & recommand ways to improve the situation
# Expectation:
# 1. root cause analysis
# 2. possible hypothesis
# 3. recommand ways to improve

# Business understanding:
# --------------------------
#
#

# TODO::::: Create dashboard for the plots

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

# ------------------
# Data understanding
# ------------------
# Attributes -
# 1. request id - unique id of request
# 2. time of request - 
# 3. drop off time
# 4. pick up point
# 5. driver id
# 6. status of request - complete/cancelled/no cabs

# Data set contains only July 2016 trips
# Observastions contains only the City - Airport & vice versa trips

  head(trips)
  str(trips)


# --------------------------
# EDA Step-2: Data cleaning:
# --------------------------

  # 2a. Fix rows: - All rows are valid - no cleaning required
  
  # 2b. Fix missing values: - NA
  
  # 2c. Standardise values:
  # Standardise Request timestamp & Drop timestamp date formats
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
    
    # Standardise trips status into (Completed, Cancelled, No Cars Available)
    trips$Status <- trimws(gsub(trips$Status, pattern = 'Trip', replacement = ''))
  
  # 2d. Fix columns:
  # Split Request.timestamp & Drop.timestamp into Request.date, Request.day, Request.time,
  # Drop.time (Drop.date, Drop.day are not required)
    
    #trips$Request.year <- format(trips$Request.timestamp, '%Y') - data is only for 2016 year
    #trips$Request.month <- format(trips$Request.timestamp, '%B') - data is only for July month
    trips$Request.day <- format(trips$Request.timestamp, '%A')
    trips$Request.time <- format(trips$Request.timestamp, '%H:%M')
    trips$Request.hour <- as.integer(format(trips$Request.timestamp, '%H'))
    
    trips$Drop.time <- format(trips$Drop.timestamp, '%H:%M')
    
  # 2e. Remove outliers
  

# -------------------------------
# EDA Step-3: Univariate analysis
# -------------------------------


# -------------------------------
# EDA Step-4: Segmented analysis
# -------------------------------


# -------------------------------
# EDA Step-5: Bivariate analysis
# -------------------------------

# ---------------------------
# EDA Step-6: Derived metrics
# ---------------------------


  trips$ApproxTravelTime <- round(difftime(trips$Drop.timestamp, trips$Request.timestamp, units = 'hours'), 2)

  # Assumption for parts of the day
  # reference: http://www.angelfire.com/pa/pawx/time.html
  # describe here
  trips$Request.hour_string <- sapply(trips$Request.hour, function(hr)
  {
    parts_of_day <- c('Early overnight','Mid overnight','Late overnight','Early morning','Mid morning','Late morning'
                      ,'Early afternoon','Mid afternoon','Late afternoon','Early evening','Mid evening','Late evening')
    if(is.integer(hr)) {
      index = ceiling(hr/2)
      if(index == 0) {
        index = 1
      }
      return(parts_of_day[index])
    } else {
      return(NA)
    }
  })

  # Drop hour variable
  trips <- trips[, !(names(trips) %in% c('Request.hour'))]


# --------------------
# 4. Results expected:
# --------------------
  # 1. visually identify the most pression problems
    # Identify the most problamatic type of requests - City to Airport OR Airport to City?
      airtport2city_trips <- trips[trips$Pickup.point == 'Airport' & trips$Status != 'Completed', ]
      city2airport_trips <- trips[trips$Pickup.point == 'City' & trips$Status != 'Completed', ]
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

