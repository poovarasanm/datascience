#### 1. Problem statement ####
# HR Analytics

library(dplyr)

#### 2. Load data ####
  employees <- read.csv('general_data.csv', stringsAsFactors = F)
  emp_surveys <- read.csv('employee_survey_data.csv', stringsAsFactors = F)
  mgr_surveys <- read.csv('manager_survey_data.csv', stringsAsFactors = F)
  in_times <- read.csv('in_time.csv', stringsAsFactors = F)
  out_times <- read.csv('out_time.csv', stringsAsFactors = F)

#### 3. Data description ####
  sprintf("Employees has %d obs & %d vars", nrow(employees), length(employees))
  sprintf("Employee surveys has %d obs & %d vars", nrow(emp_surveys), length(emp_surveys))
  sprintf("Manager surveys has %d obs & %d vars", nrow(mgr_surveys), length(mgr_surveys))
  sprintf("In tme has %d obs & %d vars", nrow(in_times), length(in_times))
  sprintf("Out time has %d obs & %d vars", nrow(out_times), length(out_times))

#### 4. Identify data quality issues ####
  # has duplicates obs?
    sprintf("Has duplicate employees? %i", nrow(employees) != length(unique(employees$EmployeeID)))
    sprintf("Has duplicate employee surveys? %i", nrow(emp_surveys) != length(unique(emp_surveys$EmployeeID)))
    sprintf("Has duplicate manager surveys? %i", nrow(mgr_surveys) != length(unique(mgr_surveys$EmployeeID)))
    sprintf("Has duplicate intime? %i", nrow(in_times) != length(unique(in_times$X)))
    sprintf("Has duplicate outtime? %i", nrow(out_times) != length(unique(out_times$X)))
  
  # has duplicate cols?
    sprintf("Has duplicate cols in employees? %i", length(employees) != length(unique(colnames(employees))))
    sprintf("Has duplicate cols in employee surveys? %i", length(emp_surveys) != length(unique(colnames(emp_surveys))))
    sprintf("Has duplicate cols in manager surveys? %i", length(mgr_surveys) != length(unique(colnames(mgr_surveys))))
    sprintf("Has duplicate cols in in time? %i", length(in_times) != length(unique(colnames(in_times))))
    sprintf("Has duplicate cols in out time? %i", length(out_times) != length(unique(colnames(out_times))))
  
  # NAs
    colSums(is.na(employees)) # TotalWorkingYears  - 9, NumCompaniesWorked - 19
    
    colSums(is.na(emp_surveys)) # EnvironmentSatisfaction - 25, 
                                # JobSatisfaction    - 20, WorkLifeBalance         - 38,
                                
    colSums(is.na(mgr_surveys)) # No NAs
    colSums(is.na(in_times))    # Yes - all columns except employee Id
    colSums(is.na(out_times))    # Yes - all columns except employee Id

#### 5. Data preparation ####
    str(employees)
    # lets remove Over18, since all obs. has same value
      employees <- employees[, -c(which(colnames(employees)=='Over18'))]
    # lets remove StandardHours, since all obs has same values
      employees <- employees[, -c(which(colnames(employees)=='StandardHours'))]
    # lets remove EmployeeCount, since all obs has same values
      employees <- employees[, -c(which(colnames(employees)=='EmployeeCount'))]
    # Treating NAs
      # 1. TotalWorkingYears - 0.20% has NAs, probably we can assign same value as YearsAtCompany
      # because, a. age b/w 28 - 43, b. among 9 only 2 left the comp. last year,
      # c. 32% of employess having equal no. of total & years at compnay***
      BlankTotalWorkingYearsIndex <- which(is.na(employees$TotalWorkingYears))
      employees[BlankTotalWorkingYearsIndex, ]$TotalWorkingYears <- employees[BlankTotalWorkingYearsIndex, ]$YearsAtCompany
      
      # 2. NumCompaniesWorked
      # a. if Total exp & Years at company are same, set NumCompaniesWorkd = 0
      indices <- which(is.na(employees$NumCompaniesWorked) & employees$YearsAtCompany == employees$TotalWorkingYears)
      employees[indices, ]$NumCompaniesWorked <- 0
      # b. lets set 0 since the NAs are only 0.32% which is very less
      indices <- which(is.na(employees$NumCompaniesWorked))
      sprintf("NA%% of NumCompaniesWorked = %f", (length(indices)/nrow(employees))*100)
      employees[indices, ]$NumCompaniesWorked <- 0
      
    # Convert Char cols to Factors
      employees <- employees %>% mutate_if(is.character,as.factor)
      
      
      summary(employees)
      
     emp_surveys$EnvironmentSatisfaction <- as.integer(emp_surveys$EnvironmentSatisfaction)
    