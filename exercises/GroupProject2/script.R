##
##
##

# -------load libs---------
  install.packages('ggplot2')
  install.packages('dplyr')
  
  library('ggplot2')
  library('dplyr')

# ------- load data -------
  loan <- read.csv('loan.csv')
  
  str(loan)
  head(loan)
  summary(loan)
  length(loan)
  nrow(loan)

# ----------Business objective--------

# ----------Business understanding------------
# 

# ---------Data understanding---------
  # 
  # URL - https://lendingclub.com/browse/loanDetail.action?loan_id=<id>
  # payment_plan - n
  # initial_list_status - f
  # application_type - INDIVIDUAL
  # delinq_amount - 0
  # policy_code - 1
  # acc_now_delinq - 1

# --------Data cleaning----------
  
  # Characters
  loan$term <- as.integer(gsub(loan$term, pattern = '\\s+|months', replacement = ''))
  loan$int_rate <- as.double(gsub(loan$int_rate, pattern = '\\s+|%', replacement = ''), length = 2)
  loan$revol_util <- as.double(gsub(loan$revol_util, pattern = '\\s+|%', replacement = ''), length = 2)
  
  loan$emp_length_rounded <- gsub(loan$emp_length, pattern = '\\s+|\\+|years?', replacement = '', ignore.case = T)
  loan$emp_length_rounded <- gsub(loan$emp_length_rounded, pattern = '<1', replacement = '0')
  loan$emp_length_rounded <- as.integer(loan$emp_length_rounded)
  
  
  loan$addr_state <- toupper(loan$addr_state)
  loan$sub_grade <- toupper(loan$sub_grade)
  
  loan$emp_title <- tolower(loan$emp_title)
  loan$desc <- tolower(loan$desc)
  loan$purpose <- tolower(loan$purpose)
  loan$title <- tolower(loan$title)
  
  # convert year into length years (4 digits)
  loan$earliest_cr_line <- gsub(loan$earliest_cr_line, pattern="-([0-1])(\\d)$", replacement = "-20\\1\\2")
  loan$earliest_cr_line <- gsub(loan$earliest_cr_line, pattern="-([^0-1])(\\d)$", replacement = "-19\\1\\2")
  
  # Dates
  formatDate <- function (df_variable, format = '%d-%b-%y')
  {
    return (as.POSIXct(strptime(paste('1-', df_variable, sep=""), format)))
  }
  
  loan$issue_d <- formatDate(loan$issue_d)
  loan$earliest_cr_line <- formatDate(loan$earliest_cr_line, '%d-%b-%Y')
  loan$last_pymnt_d <- formatDate(loan$last_pymnt_d)
  loan$next_pymnt_d <- formatDate(loan$next_pymnt_d)
  loan$last_credit_pull_d <- formatDate(loan$last_credit_pull_d)
  
  # Numeric formats
  loan$total_rec_late_fee <- as.double(loan$total_rec_late_fee, digits = 2)
  
  # Filtering observations
  no_of_loan_observations <- nrow(loan)
  # remove variable, if all observations are NA
  loan_filtered <- loan[, colSums(is.na(loan)) < no_of_loan_observations]
  
  # remove the columns if
  # 1. more NAs
  # 2. few NAs & rest of them having a constant values eg: 0
  # loan_next_payment_date, loan_next_payment_date, mths_since_last_record
  # remote tax_lines, collections_12_mths_ex_med, chargeoff_within_12_mths
  
  # remove variables (Findings from summary() results)
  # lc_page_url - equivalent to the load Id
  # payment_plan, initial_list_status, application_type, policy_code, acc_now_delinq - same for all obs
  loan_filtered <- loan_filtered[, !(names(loan_filtered) %in% c('id', 'member_id','url', 'pymnt_plan', 'initial_list_status',
                                                                 'application_type', 'delinq_amnt', 'policy_code',
                                                                 'acc_now_delinq', 'emp_length', 'desc', 'next_pymnt_d',
                                                                 'mths_since_last_record', 'tax_liens', 'collections_12_mths_ex_med',
                                                                 'zip_code', 'chargeoff_within_12_mths'))]
  
      
  summary(loan_filtered)
  str(loan_filtered)
  
  
  # remove aggregated/summary columns OR expand them to new columns
  
  # TODO: Derive borrower's age based on Earliest Cr Line year? Or consider it in analysis


# Univariate analysis ----
  
  drawPlotOfCategrocialUniVar <- function (variable, x_label, y_label = 'Freqency(%)', fill_color = 'orange')
  {
    plot <- ggplot(loan_filtered, aes(variable)) +
      scale_y_discrete(labels = scales::percent) +
      ylab(y_label) +
      xlab(x_label) +
      theme(axis.text.x = element_text(angle = 10))
    
    plot + geom_bar(aes(y = (..count../sum(..count..))), width = 0.7, fill = fill_color)
    
  }
  
  drawUnivariatePlotForContinuousVariable <- function (variable, x_label, y_label = 'Freqency(%)', fill_color = 'orange')
  {
    plot <- ggplot(loan_filtered, aes(variable)) +
      scale_y_continuous(labels = scales::percent) +
      ylab(y_label) +
      xlab(x_label) +
      theme(axis.text.x = element_text(angle = 10))
    
    plot + geom_bar(aes(y = (..count../sum(..count..))), width = 0.7, fill = fill_color)
  }
  
  univAnalysis <- function (variable, find_mean = F)
  {   
    group_by_result <- loan_filtered %>% group_by(loan_filtered[,variable])
    if(find_mean == T)
    {
      return(group_by_result %>% summarise(count = n(), mean = mean(loan_filtered[,variable])))
    } else {
      return(group_by_result %>% summarise(count = n()))
    }
  }
  
  # Categorical variables
  loan_filtered$term <- as.factor(loan_filtered$term)
  univAnalysis('term', T)
  drawPlotOfCategrocialUniVar(loan_filtered$term, 'Payment term(months)')
  sub_grade
  emp_title
  home_ownership
  verification_status
  issue_d
  loan_status
  purpose
  title
  addr_state
  delinq_2yrs
  earliest_cr_line
  inq_last_6mths
  mths_since_last_delinq
  open_acc
  pub_rec
  total_acc
  last_pymnt_d
  last_credit_pull_d
  pub_rec_bankruptcies
  emp_length_rounded
    
  # Continuous variables
    loan_amnt
    funded_amnt
    funded_amnt_inv
    installment
    int_rate
    annual_inc
    dti
    revol_bal
    revol_util
    out_prncp
    out_prncp_inv
    total_pymnt
    total_pymnt_inv
    total_rec_prncp
    total_rec_int
    total_rec_late_fee
    recoveries
    collection_recovery_fee
    last_pymnt_amnt
  
# ---------Bivariate analysis-----------  
 
  rep_job_titles <-  loan_filtered %>% group_by(grade, sub_grade, job_title) %>% summarise(n = n()) %>% filter(n()>5)