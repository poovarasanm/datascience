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

# 1. Business understanding:
# ----------------------
#
#

# 2. Data understanding:
# ------------------
# Attributes -
# 1. request id - unique id of request
# 2. time of request - 
# 3. drop off time
# 4. pick up point
# 5. driver id
# 6. status of request - complete/cancelled/no cabs

# *** Consider only to & fro airport trips

# 3. Data cleaning:
# -----------------
# a.  identify data quality issues & clean data
# b.  ensure date & time formats are proper
# ba. derive new variables

# 4. Results expected:
# --------------------
# 1. visually identify the most pression problems
# 2. find out gap between supply and demand & show them using plots
# 3. what do you think the reason for the issue - supply demand? describe in words
# 4. recommand some ways to resolve the supply - demand gap

install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
install.packages('chron')

# load packages
library('dplyr')
library('tidyr')
library('ggplot2')
library('chron')

# load data
trips <- read.csv('Uber Request Data.csv', stringsAsFactors = F)

# data understanding
# head(trips)
# str(trips)

# Data cleaning
# 1. fix rows & columns
# 2. fill missing values

# 3. Standardise values
# a. Standardise Request timestamp & Drop timestamp date formats
formatDateTime <- function(variable)
{
  variable <- gsub(variable, pattern = '-', replacement = '/') # standardise the format string
  variable <- strptime(variable, '%d/%m/%Y %H:%M') # format time
  return (as.POSIXct(variable))
}

trips$Request.timestamp <- formatDateTime(trips$Request.timestamp)
trips$Drop.timestamp <- formatDateTime(trips$Drop.timestamp)

# b. Standardise trips status
trips$Status <- gsub(trips$Status, pattern = 'Trip', replacement = '')

# Derive new variables
#trips$Request.year <- format(trips$Request.timestamp, '%Y') - data is only for 2016 year
#trips$Request.month <- format(trips$Request.timestamp, '%B') - data is only for July month
trips$Request.day <- format(trips$Request.timestamp, '%A')
trips$Request.time <- format(trips$Request.timestamp, '%H:%M')
trips$Request.hour <- as.integer(format(trips$Request.timestamp, '%H'))

trips$Drop.time <- format(trips$Drop.timestamp, '%H:%M')

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



# 4. correct invalid values
# 5. filtering values
 View(summarize(group_by(trips, Request.hour_string), length(Request.hour_string))) 
 View(summarize(group_by(filter(trips, Status=='No Cars Available'), Request.hour_string), length(Request.hour_string)))
