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
# ----------------------------------------
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')

# load packages
# ------------- 
  library('dplyr')
  library('tidyr')
  library('ggplot2')
  

# -------------------------
# EDA Step-1: Data Sourcing

  trips <- read.csv('Uber Request Data.csv', stringsAsFactors = F)

  # Data set contains only July 2016 trips
  # Observastions contains only the City - Airport & vice versa trips
    head(trips)
    str(trips)


# --------------------------
# EDA Step-2: Data cleaning:
  
  # I. Fix rows: NA (All rows are already cleaned)
  
  # II. Fix columns:
    # a. Add column names if missing - NA
    # b. Rename columns consistently - NA
    # c. Delete unnecessary columns - NA
    # e. Split columns for more data - formatting Request.timestamp & Drop.timestamp first & splitting them to date, weekday, time etc
    # f. Merge columns for identifiers - NA
    # g. Align misaligned columns -formatting the Request.timestamp & Drop.timestamp will automatically aligns these columns
  
  # III. Fix missing values: NA (No missing values except the Drop.timestamp, but Drop.timestamp has NAs only for cancelled/no cars available rows)
  
  # IV. Correct invalid values & derive new variables: 
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
      
      #trips$Drop.time <- format(trips$Drop.timestamp, '%H:%M')
      #trips$Drop.hour <- format(trips$Drop.timestamp, '%H')
      
      trips$Approx.travel.time <- round(difftime(trips$Drop.timestamp, trips$Request.timestamp, units = 'hours'), 2)
      
      # To keep the bar chart sorted
      trips$Request.day <- factor(trips$Request.day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
      
    # Create time slot variable
      # 0  3  6  9 12 15 18 21 24 - split ups
      # 00:00 - 2:59  -> Night
      # 3:00 - 5:59   -> Late night
      # 6:00 - 8:59   -> Early morning
      # 9:00 - 11:59  -> Morning
      # 12:00 - 14:59 -> Afternoon
      # 15:00 - 17:59 -> Evening
      # 18:00 - 20:59 -> Late evening
      # 21:00 - 23:59 -> Early night
      
      time_slots <- c('Night', 'Late night', 'Early morning', 'Morning', 'Afternoon', 'Evening',
                       'Late evening', 'Early night')
      
      trips$Request.time_slot <- sapply(trips$Request.timestamp, function(t)
      {
        mins = as.integer(format(t, '%H')) * 60 + as.integer(format(t, '%M'))
        time_slot_index = 24/length(time_slots)
        index = ceiling((mins/60)/time_slot_index)
        if (index == 0) 
        {
          index = 1
        }
        return(time_slots[index])
      })
      
      # Covert it to factor so the plot can be in proper order as defined at time_slots
      trips$Request.time_slot <- factor(trips$Request.time_slot, levels = time_slots)
  

# -------------------------------
# EDA Step-3: Univariate analysis

    # a. Metadata description
      # 1.Request id: A unique identifier of the request
      # Time of request: The date and time at which the customer made the trip request
      # Drop-off time: The drop-off date and time, in case the trip was completed 
      # Pick-up point: The point from which the request was made
      # Driver id: The unique identification number of the driver
      # Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
      
    # b. Identify types of variables - identify ordered, unordered categorical variables & quantitative variables
      # Unordered categorical variables = Pickup.point, Status
      # Ordered categorical variables = Request.timestamp, Drop.timestamp and all of the derived variables from these two variables
      
    # c. Data distribution plots
      # Unordered categorical variables - univariate analysis
      
      # Draws bar chart having x = categorical variable, y = frequency in percentage
      drawUnivariatePlot <- function (variable, x_label, y_label = 'Freqency(%)', fill_color = 'orange')
      {
        plot <- ggplot(trips, aes(variable)) +
          scale_y_continuous(labels = scales::percent) +
          ylab(y_label) +
          xlab(x_label) +
          theme(axis.text.x = element_text(angle = 10))
        
        plot + geom_bar(aes(y = (..count../sum(..count..))), width = 0.7, fill = fill_color)
      }
      
      drawUnivariatePlot(trips$Pickup.point, 'Pickup point', '% of requests')
      drawUnivariatePlot(trips$Status, 'Trip status', '% of requests')
      
      # Possible hypotheses (All day having nearly same percentage of requests)
      drawUnivariatePlot(trips$Request.date, 'Request date', '% of requests', 'purple')
      drawUnivariatePlot(trips$Request.day, 'Request day', '% of requests', 'purple')
      
      drawUnivariatePlot(trips$Request.time_slot, 'Time slot', '% of requests', 'purple')
      trips %>%
        ggplot(aes(Request.hour)) +
        ylab('Frequency (Request count)') +
        xlab('Request hour') +
        geom_bar(width = 0.9)

# -------------------------------
# EDA Step-4: Segmented analysis
      
      # Draws stack chart having x = categorical variable, y = frequency in percentage
      drawSegmentedPlot <- function (x_var, fill_var, x_label, y_label = 'Freqency(%)', legend_title = '')
      {
        plot1 <- ggplot(trips, aes(x_var)) +
          scale_y_continuous(labels = scales::percent) +
          ylab(y_label) +
          xlab(x_label) +
          theme(axis.text.x = element_text(angle = 10))
        
        plot1 + geom_bar(aes(y = (..count../sum(..count..)), fill = fill_var), width = 0.7) +
          scale_fill_discrete(name = legend_title)
      }
      
      # Pickup.point frequency per date
      drawSegmentedPlot(trips$Request.date, trips$Pickup.point, 'Request date', '% of requests', 'Pickup point')
      
      # Status frequency per date
      drawSegmentedPlot(trips$Request.date, trips$Status, 'Request date', '% of requests', 'Status')
      
      # Status frequency per time slot
      drawSegmentedPlot(trips$Request.time_slot, trips$Status, 'Request time slot', '% of requests', 'Status')

# -------------------------------
# EDA Step-5: Bivariate analysis
# -------------------------------
      # 1. visually identify the most pressing problems
      # Identify the most problamatic type of requests - City to Airport OR Airport to City?
      trips %>%
        filter(Status != 'Trip Completed') %>%
        ggplot(aes(Pickup.point, fill=Status)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
      
      # Identify the most problematic the time slots
      trips %>%
        filter(Status != 'Trip Completed') %>%
        ggplot(aes(Request.time_slot, fill=Status)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
      
      
      # 2. find out gap between supply and demand & show them using plots
      # Find the time slots when the highest gap exists
      trips %>%
        ggplot(aes(Request.time_slot, fill=Status)) + geom_bar() +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
      
      # Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
      problematic_time_slots <- c('Late night', 'Early morning', 'Morning', 'Evening', 'Late evening', 'Early night')
      trips %>%
        filter(Request.time_slot %in%  problematic_time_slots & Status != 'Trip Completed') %>%
        ggplot(aes(x=Pickup.point, fill=Status)) +   
        geom_bar(position='stack', stat='count') +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5)) +
        facet_wrap(~Request.time_slot)
      
      ggplot(trips, aes(x=Request.day, fill=Status)) +   
        geom_bar(position='stack', stat='count') +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5)) +
        facet_wrap(~Pickup.point)
      
      
      
      # Find the frequency of cancellation of drivers
      trips %>%
        filter(Driver.id <= 100) %>%
        ggplot(aes(Driver.id, fill=Status)) +
        ylab("Cancelled requests") +
        xlab("Driver id") +
        geom_bar(stat='count') +
        geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust=0.5))
      

# ---------------------------
# EDA Step-6: Derived metrics
# ---------------------------
