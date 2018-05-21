#### 1. Problem statement ####
# HR Analytics

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
colSums(is.na(employees)) # No NAs
colSums(is.na(emp_surveys)) # NumCompaniesWorked - 19, EnvironmentSatisfaction - 25, 
                            # JobSatisfaction    - 20, WorkLifeBalance         - 38,
                            # TotalWorkingYears  - 9
colSums(is.na(mgr_surveys)) # No NAs
colSums(is.na(in_times))    # Yes - all columns except employee Id
colSums(is.na(out_times))    # Yes - all columns except employee Id

#### 5. Data preparation ####
