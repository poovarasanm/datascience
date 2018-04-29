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
  install.packages('corrplot')

# load dependencies
  library('dplyr')
  library('tidyr')
  library('ggplot2')
  library('MASS')
  library('car')
  library('treemap')
  library('corrplot')

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
  length(colnames(cars)) == length(unique(colnames(cars))) # True - no duplicates

# Has NA values? No
  colSums(is.na(cars))

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
    #cars <- cars[, -8]
    # but it's still we can see if there is a price impact by the enginelocation
    
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
  
  # check the levels of columns (manually check the levels)
    sapply(cars, levels) # to verify if all categorical columns has valid & no duplicate or incorrect values
    
  # Convert symboling symboling integer
    cars$symboling <- as.integer(cars$symboling)
    
  # Convert cylindernumbers into integer
    levels(cars$cylindernumber) <- c(8, 5, 4, 6, 3, 12, 2)
    # convert cylinders to integer
      cars$cylindernumber <- as.integer(cars$cylindernumber)
      
  # Convert doornumber into integer
      levels(cars$doornumber) <- c(4, 2)
      # convert doors to integer
        cars$doornumber <- as.integer(cars$doornumber)
        
  # Dummy variable creation
      cars <- cbind(cars, model.matrix( ~ company - 1, cars))
      cars <- cbind(cars, model.matrix( ~ fueltype - 1, cars))
      cars <- cbind(cars, model.matrix( ~ aspiration - 1, cars))
      cars <- cbind(cars, model.matrix( ~ carbody - 1, cars))
      cars <- cbind(cars, model.matrix( ~ drivewheel - 1, cars))
      cars <- cbind(cars, model.matrix( ~ enginetype - 1, cars))
      cars <- cbind(cars, model.matrix( ~ fuelsystem - 1, cars))
      cars <- cbind(cars, model.matrix( ~ enginelocation - 1, cars))
    
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
      cars %>% ggplot(aes(enginelocation)) + geom_bar() # almost all cars having front engines
      
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
      cars %>% ggplot(aes(x=enginelocation, y=price)) + geom_col()
      
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

    # Multivariate & segmented analysis
      # Price distribution by fueltype, fuelsystem per company
      cars %>% ggplot(aes(x=fueltype, y=price)) +
        geom_col(aes(fill=fuelsystem), position = 'dodge') +
        facet_wrap(~ company)
      
      # Price distribution by aspiration, enginetype per company
      cars %>% ggplot(aes(x=aspiration, y=price)) +
        geom_col(aes(fill=enginetype), position = 'dodge') +
        facet_wrap(~ company)
        
      # Price distribution by carbody, symboling(risky) per company
      cars %>% ggplot(aes(x=carbody, y=price)) +
        geom_col(aes(fill=symboling), position = 'dodge') +
        facet_wrap(~ company)
      
      # Price variation by citympg, highwaympg
      cars %>% ggplot(aes(x=price, group = 1)) +
        geom_line(aes(y=citympg), color='red') +
        geom_line(aes(y=highwaympg), color='green')
      
      # Price variation by horsepower, enginesize
      cars %>% ggplot(aes(x=price,y=horsepower)) +
        geom_line(aes(y=horsepower), color='red') +
        geom_line(aes(y=enginesize), color='green')
      
      cor_values <- cor(cars[, c("symboling", "wheelbase", "carlength", "carwidth", "carheight",  "curbweight",       
                                 "cylindernumber", "enginesize", "boreratio", "stroke",           
                                 "compressionratio", "horsepower", "peakrpm", "citympg", "highwaympg", "price")])
      corrplot(cor_values, method="square")
      heatmap(cor_values)
      
      # let's create a dataframe for the modal building - which excludes the base variable used for 
      # the dummy variable creation
      cars_for_model <- cars[, !(colnames(cars) %in% c('company', 'fueltype', 'aspiration', 'carbody', 'drivewheel', 'enginetype',
                                 'fuelsystem', 'enginelocation'))]

#### VII. Modal building ####
    # Seed
      set.seed(100)
      
    # Create trainning & test data
      cars_subset <- sample(1:nrow(cars_for_model),0.7*nrow(cars_for_model))
      car_samples <-cars_for_model[cars_subset, ]
      cars_for_test <- cars[-cars_subset, ]
    
    #### Model - 1  ####
      price_prediction_model_1 <- lm(price ~ ., data = car_samples)
      summary(price_prediction_model_1)
      
      # let's use step aic in both direction
      stepAIC(price_prediction_model_1, direction = 'both')
    
    #### Model - 2  ####
      # let's use the result lm() of step aic
      price_prediction_model_2 <- lm(formula = price ~ carlength + carwidth + curbweight + enginesize + 
                                       stroke + compressionratio + peakrpm + citympg + companybmw + 
                                       companybuick + companychevrolet + companydodge + companyjaguar + 
                                       companymazda + companymitsubishi + companynissan + companypeugeot + 
                                       companyplymouth + companyporsche + companyrenault + companysaab + 
                                       companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                       carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                       enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                     data = car_samples)
      
      # let's see R-squared, adjusted R-squared etc
      summary(price_prediction_model_2)
      
      # let's see the VIP
      vif(price_prediction_model_2)
    
    #### Model - 3  ####
      # let's remove curbweight as it is having high p values & vip
      price_prediction_model_3 <- lm(formula = price ~ carlength + carwidth + enginesize + 
                                       stroke + compressionratio + peakrpm + citympg + companybmw + 
                                       companybuick + companychevrolet + companydodge + companyjaguar + 
                                       companymazda + companymitsubishi + companynissan + companypeugeot + 
                                       companyplymouth + companyporsche + companyrenault + companysaab + 
                                       companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                       carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                       enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                     data = car_samples)
      summary(price_prediction_model_3)
      vif(price_prediction_model_3)
      
    #### Model - 4 ####
      # let's remove compressionratio as it has high p value
      price_prediction_model_4 <- lm(formula = price ~ carlength + carwidth + enginesize + 
                                       stroke + peakrpm + citympg + companybmw + 
                                       companybuick + companychevrolet + companydodge + companyjaguar + 
                                       companymazda + companymitsubishi + companynissan + companypeugeot + 
                                       companyplymouth + companyporsche + companyrenault + companysaab + 
                                       companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                       carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                       enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                     data = car_samples)
      summary(price_prediction_model_4)
      vif(price_prediction_model_4)
      
    ### Model - 5 ####
      # remove citympg as it has no star rating & also considerable high p value
      price_prediction_model_5 <- lm(formula = price ~ carlength + carwidth + enginesize + 
                                       stroke + peakrpm + companybmw + 
                                       companybuick + companychevrolet + companydodge + companyjaguar + 
                                       companymazda + companymitsubishi + companynissan + companypeugeot + 
                                       companyplymouth + companyporsche + companyrenault + companysaab + 
                                       companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                       carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                       enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                     data = car_samples)
      summary(price_prediction_model_5)
      vif(price_prediction_model_5)
      
    #### Model - 6 ####
    # remove companysaab as it has . as the rating & considerable high p value
    price_prediction_model_6 <- lm(formula = price ~ carlength + carwidth + enginesize + 
                                     stroke + peakrpm + companybmw + 
                                     companybuick + companychevrolet + companydodge + companyjaguar + 
                                     companymazda + companymitsubishi + companynissan + companypeugeot + 
                                     companyplymouth + companyporsche + companyrenault + 
                                     companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                     carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                     enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                   data = car_samples)
    summary(price_prediction_model_6)
    vif(price_prediction_model_6)
    
    #### Model - 7 ####
    # remove carlength as it has . as the rating & considerable high p value
    price_prediction_model_7 <- lm(formula = price ~ carwidth + enginesize + 
                                     stroke + peakrpm + companybmw + 
                                     companybuick + companychevrolet + companydodge + companyjaguar + 
                                     companymazda + companymitsubishi + companynissan + companypeugeot + 
                                     companyplymouth + companyporsche + companyrenault + 
                                     companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                     carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                     enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                   data = car_samples)
    summary(price_prediction_model_7)
    vif(price_prediction_model_7)
    
    #### Model - 8 ####
    # remove peakrpm as it has only 1 rating
    price_prediction_model_8 <- lm(formula = price ~ carwidth + enginesize + 
                                     stroke + companybmw + 
                                     companybuick + companychevrolet + companydodge + companyjaguar + 
                                     companymazda + companymitsubishi + companynissan + companypeugeot + 
                                     companyplymouth + companyporsche + companyrenault + 
                                     companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                     carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                     enginetypedohcv + enginetypeohc + enginetypeohcv + fuelsystem1bbl, 
                                   data = car_samples)
    summary(price_prediction_model_8)
    vif(price_prediction_model_8)
    
    #### Model - 9 ####
    # remove fuelsystem1bbl as it has only 1 rating
    price_prediction_model_9 <- lm(formula = price ~ carwidth + enginesize + 
                                     stroke + companybmw + 
                                     companybuick + companychevrolet + companydodge + companyjaguar + 
                                     companymazda + companymitsubishi + companynissan + companypeugeot + 
                                     companyplymouth + companyporsche + companyrenault + 
                                     companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                     carbodyconvertible + drivewheel4wd + drivewheelfwd + enginetypedohc + 
                                     enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                   data = car_samples)
    summary(price_prediction_model_9)
    vif(price_prediction_model_9)
    
    #### Model - 10 ####
    # remove drivewheelfwd as it has only 1 rating
    price_prediction_model_10 <- lm(formula = price ~ carwidth + enginesize + 
                                     stroke + companybmw + 
                                     companybuick + companychevrolet + companydodge + companyjaguar + 
                                     companymazda + companymitsubishi + companynissan + companypeugeot + 
                                     companyplymouth + companyporsche + companyrenault + 
                                     companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                     carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                     enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                   data = car_samples)
    summary(price_prediction_model_10)
    vif(price_prediction_model_10)
    
    #### Model - 11 ####
    # remove companychevrolet as it has only 1 rating & high vif than other 1 rating vars
    price_prediction_model_11 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companydodge + companyjaguar + 
                                      companymazda + companymitsubishi + companynissan + companypeugeot + 
                                      companyplymouth + companyporsche + companyrenault + 
                                      companysubaru + companytoyota + companyvolkswagen + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_11)
    vif(price_prediction_model_11)
    
    #### Model - 12 ####
    # remove companyvolkswagen as it has only 1 rating & high vif than other 1 rating vars
    price_prediction_model_12 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companydodge + companyjaguar + 
                                      companymazda + companymitsubishi + companynissan + companypeugeot + 
                                      companyplymouth + companyporsche + companyrenault + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_12)
    vif(price_prediction_model_12)
    
    #### Model - 13 ####
    # remove companyrenault as it has only 1 rating & high p value than other 1 rating vars
    price_prediction_model_13 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companydodge + companyjaguar + 
                                      companymazda + companymitsubishi + companynissan + companypeugeot + 
                                      companyplymouth + companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_13)
    vif(price_prediction_model_13)
    
    #### Model - 14 ####
    # remove companydodge as it has only 1 rating & high p value than other 1 rating vars
    price_prediction_model_14 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymazda + companymitsubishi + companynissan + companypeugeot + 
                                      companyplymouth + companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_14)
    vif(price_prediction_model_14)
    
    #### Model - 15 ####
    # remove companynissan as it has only 1 rating & high p value than other 1 rating vars
    price_prediction_model_15 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymazda + companymitsubishi + companypeugeot + 
                                      companyplymouth + companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_15)
    vif(price_prediction_model_15)
    
    #### Model - 16 ####
    # remove companyplymouth as it has only 0 rating & high p value
    price_prediction_model_16 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymazda + companymitsubishi + companypeugeot + 
                                      companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_16)
    vif(price_prediction_model_16)
    
    #### Model - 17 ####
    # remove companymazda as it has only 2 rating & high p value than other 2 rating vars
    price_prediction_model_17 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymitsubishi + companypeugeot + 
                                      companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd + enginetypedohc + 
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_17)
    vif(price_prediction_model_17)
    
    #### Model - 18 ####
    # remove enginetypedohc as it has only 2 rating & high p value than other 2 rating vars
    price_prediction_model_18 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymitsubishi + companypeugeot + 
                                      companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd +
                                      enginetypedohcv + enginetypeohc + enginetypeohcv, 
                                    data = car_samples)
    summary(price_prediction_model_18)
    vif(price_prediction_model_18)
    
    #### Model - 19 ####
    # remove enginetypeohcv as it has no rating & high p value
    price_prediction_model_19 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymitsubishi + companypeugeot + 
                                      companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      carbodyconvertible + drivewheel4wd +
                                      enginetypedohcv + enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_19)
    vif(price_prediction_model_19)
    
    #### Model - 20 ####
    # remove carbodyconvertible as it has 1 rating
    price_prediction_model_20 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companymitsubishi + companypeugeot + 
                                      companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      drivewheel4wd +
                                      enginetypedohcv + enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_20)
    vif(price_prediction_model_20)
    
    #### Model - 21 ####
    # remove companymitsubishi as it has 1 rating
    price_prediction_model_21 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companypeugeot + 
                                      companyporsche + 
                                      companysubaru + companytoyota + aspirationstd + 
                                      drivewheel4wd +
                                      enginetypedohcv + enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_21)
    vif(price_prediction_model_21)
    
    #### Model - 22 ####
    # remove companytoyota as it has 2 rating & high p value than other 2 rating vars
    price_prediction_model_22 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companypeugeot + 
                                      companyporsche + 
                                      companysubaru + aspirationstd + 
                                      drivewheel4wd +
                                      enginetypedohcv + enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_22)
    vif(price_prediction_model_22)
    
    #### Model - 23 ####
    # remove drivewheel4wd as it has only 2 rating
    price_prediction_model_23 <- lm(formula = price ~ carwidth + enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companypeugeot + 
                                      companyporsche + 
                                      companysubaru + aspirationstd + 
                                      enginetypedohcv + enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_23)
    vif(price_prediction_model_23)
    
    #### Model - 24 ####
    # remove carwidth as it has high p value & high vip value than other high p value vars
    price_prediction_model_24 <- lm(formula = price ~ enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companypeugeot + 
                                      companyporsche + 
                                      companysubaru + aspirationstd + 
                                      enginetypedohcv + enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_24)
    vif(price_prediction_model_24)
    
    #### Model - 25 ####
    # remove enginetypedohcv as it has no rating & high p and vif value than other no rating var
    price_prediction_model_25 <- lm(formula = price ~ enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companypeugeot + 
                                      companyporsche + 
                                      companysubaru + aspirationstd + 
                                      enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_25)
    vif(price_prediction_model_25)
    
    #### Model - 26 ####
    # remove companypeugeot as it has no rating & high p and vif value
    price_prediction_model_26 <- lm(formula = price ~ enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companyporsche + 
                                      companysubaru + aspirationstd + 
                                      enginetypeohc, 
                                    data = car_samples)
    summary(price_prediction_model_26)
    vif(price_prediction_model_26)
    
    #### Model - 27 ####
    # remove enginetypeohc as it has 2 rating & all others having 3 rating
    price_prediction_model_27 <- lm(formula = price ~ enginesize + 
                                      stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companyporsche + 
                                      companysubaru + aspirationstd, 
                                    data = car_samples)
    summary(price_prediction_model_27)
    vif(price_prediction_model_27)
    
    #### Model - 28 ####
    # remove enginesize as it has high p value & vif value
    price_prediction_model_28 <- lm(formula = price ~  stroke + companybmw + 
                                      companybuick + companyjaguar + 
                                      companyporsche + 
                                      companysubaru + aspirationstd, 
                                    data = car_samples)
    summary(price_prediction_model_28)
    vif(price_prediction_model_28)
    
    #### Model - 29 ####
    # remove stroke as it has . rating & high p value and vif
    price_prediction_model_29 <- lm(formula = price ~  companybmw + 
                                      companybuick + companyjaguar + 
                                      companyporsche + 
                                      companysubaru + aspirationstd, 
                                    data = car_samples)
    summary(price_prediction_model_29)
    vif(price_prediction_model_29)
    
    #### Model - 30 ####
    # remove companysubaru as it has no rating & high p value
    price_prediction_model_30 <- lm(formula = price ~  companybmw + 
                                      companybuick + companyjaguar + 
                                      companyporsche + 
                                      aspirationstd, 
                                    data = car_samples)
    summary(price_prediction_model_30)
    vif(price_prediction_model_30)
    
  #### Final model ####
    # all vars has 3 rating & further regression leads < 50 adjusted R squared
    # this has ~70% of adjusted r squared value
    final_model <- lm(formula = price ~  companybmw + 
                        companybuick + companyjaguar + 
                        companyporsche + 
                        aspirationstd, 
                      data = car_samples)
    summary(final_model)
    vif(final_model)
    
  #### verify ####
    prediction_result <- predict(final_model, cars_for_test)
    View(prediction_result)
    
    # let's set the predicted price to the cars_for_test
    cars_for_test$predicted_price <- prediction_result
    
    # correlation
    cars_price_r <- cor(cars_for_test$price, cars_for_test$predicted_price)
    # 0.7214849 which is good value
    
    # what is r squared value
    r_squared <- cars_price_r ^ 2
    # 0.5205404
    
    par(mfrow = c(2, 2))
    plot(final_model)
