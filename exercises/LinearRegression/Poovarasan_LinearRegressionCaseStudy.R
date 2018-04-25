# Assignment: Linear Regression
# Client: Geely Auto
# Learning Outcome: Understanding Linear Regression
# Expected output: R file
# Author: Poovarasan

#### I. Problem statement ####
# A Chinese automobile company Geely Auto aspires to enter the US market by setting up their manufacturing
# unit there and producing cars locally to give competition to their US and European counterparts. 

# they want to understand the factors on which the pricing of a car depends.
# Specifically, they want to understand the factors affecting the pricing of cars in the American marketing,
# since those may be very different from the Chinese market. Essentially, the company wants to know:
# Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car
# Based on various market surveys, the consulting firm has gathered a large dataset of different types of cars across 
# the Americal market. 

##### II. Objective ####
# 1. To create the Modal for the Price variable from the independent variables
# 2. Based on the modal the management wants to understand how to design, manufacture & the plan the business strategy to 
#     compete with the existing competitor.

#### III. Business understanding ####
# Geely (officially Zhejiang Geely Holding Group Co., Ltd) is a Chinese multinational automotive manufacturing company
# headquartered in Hangzhou, Zhejiang.

# They want to set up their manufacturing unit there and producing cars locally to give competition to their US and European
# counterparts.

#### IV. Data understanding #####
# Data source: https://archive.ics.uci.edu/ml/datasets/Automobile
# The data is collected from an automobile consulting company which contains the 23 parameters about the 205 unique rows of 
# cars with the price of each cars.

# Install dependencies
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('MASS')
  install.packages('car')
  install.packages('treemap')

# load dependencies
  library('dplyr')
  library('tidyr')
  library('ggplot2')
  library('MASS')
  library('car')
  library('treemap')

# load data from csv
  cars <- read.csv('CarPrice_Assignment.csv')

  head(cars)
  str(cars)

# How many variables?
  length(cars) # 26

# How many rows?
  nrow(cars) # 205

# Has duplicate rows?
  length(unique(cars$car_Id)) == length(cars$car_ID) # False - No duplicates

# Has duplicate cols?
  length(colnames(cars)) == length(unique(colnames(cars))) # False - no duplicates

# Has NA values? No
  View(colSums(is.na(cars)))

#### V. Data preparation ####
  # fix headers - make the headers uniform
    names(cars) <- tolower(colnames(cars))
  # fix columns
  # Remove/replace NAs - no NA values found
  # create car company column
    cars <- separate(cars, col = carname, into = c('company', 'carname'), sep = ' ', extra="merge", fill="right")
  # remove carname column as stated at the requirement (Company name alone is required for analysis)
    cars <- cars[, -4]
  # let's remove car_id column
    cars <- cars[, -1]
  # let's remove enginelocation column?
    levels(cars$enginelocation) # only two levels front & rear
    summary(cars$enginelocation) # 202 are front & only 3 are rear
    # only 1% of enginelocation is rear type, so we can remove this column
    cars <- cars[, -8]
    
  # spell check & correction of car company column and replace '-' in the company name
    cars$company <- tolower(cars$company)
    cars$company <- as.factor(cars$company)
    misspelled <- c('alfa-romero', 'maxda', 'porcshce', 'toyouta', 'vokswagen', 'vw')
    replacement <- c('alfa romeo', 'mazda', 'porsche',  'toyota', 'volkswagen', 'volkswagen')
    i <- 1
    while(i <= length(misspelled)) {
      levels(cars$company)[levels(cars$company)==misspelled[i]] <- replacement[i]
      i <- i+1
    }
  
  # check the levels of columns
    sapply(cars, levels) # to verify if all categorical columns has valid & no duplicate or incorrect values
    
  # Cross check the data type of each column with the data definition
  # let's convert symboling column from integer to categorical
    cars$symboling <- factor(cars$symboling)
    
  # verify the data are cleaned & ready for analysis
    str(cars)
    summary(cars)
    
#### VI. EDA ####
    # Univariate analysis
      # Categorical variables
      # the summary of each graph is commented at right side
      cars %>% ggplot(aes(symboling)) + geom_bar() # Moderate to risky cars are more than the pretty safe cars
      cars %>% ggplot(aes(company)) + geom_bar()   # Toyato, mazda, honda & nissan are top 4 companies
      cars %>% ggplot(aes(fueltype)) + geom_bar()  # gas is the preferred type of fueltype(175+ cars) than diesel typed cars
      cars %>% ggplot(aes(aspiration)) + geom_bar() # standard aspiration is preferred more( 175+) than turbo
      cars %>% ggplot(aes(doornumber)) + geom_bar() # 150+ cars having 4 doors, 90 cars having 2 doors
      cars %>% ggplot(aes(carbody)) + geom_bar()    # sedan & hatchpack are the highest type of cars, remaining are at least count
      cars %>% ggplot(aes(drivewheel)) + geom_bar() # front - 100+, rear drive - 75 but 4 wheel drives are at least manufactured
      cars %>% ggplot(aes(enginetype)) + geom_bar() # OHC engine type is comparitively very high than any other engines
      cars %>% ggplot(aes(cylindernumber)) + geom_bar() # 4 cylinder typed cars are comparitively very high than any other types
      cars %>% ggplot(aes(fuelsystem)) + geom_bar() # MPFI & 2BBL typed fuel systems are more than any other types
      
    # Continuous variables
      cars %>% ggplot(aes(wheelbase)) + geom_histogram(binwidth = 1)
      cars %>% ggplot(aes(carlength)) + geom_histogram(binwidth = 20)
      cars %>% ggplot(aes(carwidth)) + geom_histogram(binwidth = 1)
      cars %>% ggplot(aes(carheight)) + geom_histogram(binwidth = 1)
      cars %>% ggplot(aes(curbweight)) + geom_histogram(binwidth = 500)
      cars %>% ggplot(aes(enginesize)) + geom_histogram(binwidth = 30)
      cars %>% ggplot(aes(boreratio)) + geom_histogram(binwidth = 0.25)
      cars %>% ggplot(aes(stroke)) + geom_histogram(binwidth = 0.5)
      cars %>% ggplot(aes(compressionratio)) + geom_histogram(binwidth = 15)
      cars %>% ggplot(aes(horsepower)) + geom_histogram(binwidth = 20)
      cars %>% ggplot(aes(peakrpm)) + geom_histogram(binwidth = 1000)
      cars %>% ggplot(aes(citympg)) + geom_histogram(binwidth = 5)
      cars %>% ggplot(aes(highwaympg)) + geom_histogram(binwidth = 5)
      cars %>% ggplot(aes(price)) + geom_histogram(binwidth = 10000)
    
    # Bivariate analysis
      # Since we want to understand the price variations per other parameters, let's analyse other variable impacts on the prices
      # vs categorical variables
      cars %>% ggplot(aes(x=symboling, y=price)) + geom_col()
      cars %>% ggplot(aes(x=fueltype, y=price)) + geom_col()
      cars %>% ggplot(aes(x=company, y=price)) + geom_col()
      cars %>% ggplot(aes(x=aspiration, y=price)) + geom_col()
      cars %>% ggplot(aes(x=doornumber, y=price)) + geom_col()
      cars %>% ggplot(aes(x=carbody, y=price)) + geom_col()
      cars %>% ggplot(aes(x=drivewheel, y=price)) + geom_col()
      cars %>% ggplot(aes(x=enginetype, y=price)) + geom_col()
      cars %>% ggplot(aes(x=cylindernumber, y=price)) + geom_col()
      cars %>% ggplot(aes(x=fuelsystem, y=price)) + geom_col()
      
      # vs continuous variables
      cars %>% ggplot(aes(x=wheelbase, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=carlength, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=carwidth, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=carheight, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=curbweight, y=price)) + geom_point() + geom_smooth(se=F, method = "lm")
      cars %>% ggplot(aes(x=enginesize, y=price)) + geom_point() + geom_smooth(se=F, method = "lm")
      cars %>% ggplot(aes(x=boreratio, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=compressionratio, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=horsepower, y=price)) + geom_point() + geom_smooth(se=F, method = "lm")
      cars %>% ggplot(aes(x=peakrpm, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=citympg, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=highwaympg, y=price)) + geom_point() + geom_smooth(se=F)
      cars %>% ggplot(aes(x=citympg, y=price)) + geom_point() + geom_smooth(se=F)

    # Multivariate analysis
      cars %>% ggplot(aes(x=company, y=price, fill=symboling)) + geom_col()
      cars %>% ggplot(aes(x=fueltype, y=price, fill=fuelsystem)) + geom_col()
      cars %>% ggplot(aes(x=fueltype, y=price, fill=enginetype)) + geom_col()
      cars %>% ggplot(aes(x=fuelsystem, y=price, fill=enginetype)) + geom_col()
      
      treemap(cars, 
              index = c('carbody', 'fueltype'),
              vSize = 'price',
              vColor = 'carbody',
              type = "index",
              title = "Price by carbody, fueltype")
      
      treemap(cars, 
              index = c('company', 'carbody'),
              vSize = 'price',
              vColor = 'fuelsystem',
              type = "index",
              title = "Price by carbody, fueltype")
      
    # Segmented analysis

#### VII. Modal building ####
    # Convert factor columns to numeric/integers for modal building
