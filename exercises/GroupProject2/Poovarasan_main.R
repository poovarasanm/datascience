##### EDA Case Study: ##### 
  #   Client                : lendingclub.com
  #   Analytics team        : Jayakumar, Pranesh, Chaitanya, Poovarasan
  #   Learning outcome      : EDA, basic understaning of Risk analytics in banking & how data used to minimise the risk of
  #                           losing money while lending to customer

#### Expected result ####
  #   R code, Presentation (approach, univariate, bivariate analysis & summary)
  #   1. Data understanding & preparation - a. report data quality issues & b. interpretation of variables.
  #   2. Data cleaning & manipulation - a. missing values, b. outlier treatment, c. formatting, d. date & string manip.
  #   3. Data analysis - a. flow & structure of analysis, b. univariate analysis, c. bivariate analysis,
  #                      d. 5 important derived variables (i.e. variables which are strong indicators of default)
  #                      e. business-deriven, type-deriven, data-deriven metrics & the explanations for each.
  #                      f. bivariate analysis of important variables
  #   4. Presentation - approaches, assumptions, plots & recommandatons.

#### Business understanding ####
  #   lendingclub.com specialises in lending various type of loan to the urban customers
  #   Risks   : 1. if applicant likely to repay, then not approving loan results int lose of business to company.
  #             2. if applicant not likely to replay, then approving the loan may lead to financial loss

####  What does the data contains: ####
  # Types of decisions on the application:
  #   1. Loan accepted: a. fully paid, b. current, c. charged-off/defaulted
  #   2. Loan rejected: does not met the crieterian for approving the loan, hence the rejected records are NOT AVAILABLE.

####  Business objectives ####
  # lending club offers loan by online for personal loan, business loans & financing medical procedures.
  # credit loss - the amount of money lost when the borrower refuses to pay/runs away with the money owed.
  # Charged off/defaulted - due not paid for longer period.
  # *** OBJECTIVE: Derive the factors behind the loan default.

#### Data understanding ####
  # Description: Each row contains the information of the accepted loan( fully paid, current or charged off) & the borrower information.
  # 1. Borrower's id, address, job & income details.
  # 2. Borrower's delinquency information, credit line, public record details
  # 3. Loan informations - funded amount, funded amount by investors, installment, status, payment information etc.

#### Assumptions ####

#### Install & load libraries ####
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('ggplot2')
  
  library('ggplot2')
  library('dplyr')
  library('tidyr')
  
#### Load data ####
  loan <- read.csv('loan.csv', stringsAsFactors = F) # stringsAsFactor=F, so charactor column can be formatted then it can be converted to factor
  
#### Summarise the data ####
  # following methods are useful for understanding the data & identifying the quality issues
    # identify duplicate rows
    dim(loan)                                       # 39717 obsercations, 111 variables
    dim(loan)[1] == length(unique(loan$id))         # No duplicate records 
    dim(loan)[1] == length(unique(loan$member_id))
    
    str(loan)
    glimpse(loan)
    head(loan)
    summary(loan)
    
    # identify NA counts
    table(colSums(is.na(loan)))
    #  0     1    39    56   697 25682 36931 39717 
    # 50     1     1     2     1     1     1    54

#### 1. Data understanding & preparation ####
  # 1a. Data quality issues:
    na_cols <- colnames(loan[colSums(is.na(loan)) == dim(loan)[1]])
    paste(na_cols, collapse = ', ')
    # 1. Columns having NA for all the rows/obs.(54 columns)
      #  mths_since_last_major_derog, annual_inc_joint, dti_joint, verification_status_joint, tot_coll_amt, tot_cur_bal, open_acc_6m,
      #  open_il_6m, open_il_12m, open_il_24m, mths_since_rcnt_il, total_bal_il, il_util, open_rv_12m, open_rv_24m, max_bal_bc, all_util,
      #  total_rev_hi_lim, inq_fi, total_cu_tl, inq_last_12m, acc_open_past_24mths, avg_cur_bal, bc_open_to_buy, bc_util, mo_sin_old_il_acct,
      #  mo_sin_old_rev_tl_op, mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, mort_acc, mths_since_recent_bc, mths_since_recent_bc_dlq,
      #  mths_since_recent_inq, mths_since_recent_revol_delinq, num_accts_ever_120_pd, num_actv_bc_tl, num_actv_rev_tl, num_bc_sats,
      #  num_bc_tl, num_il_tl, num_op_rev_tl, num_rev_accts, num_rev_tl_bal_gt_0, num_sats, num_tl_120dpd_2m, num_tl_30dpd, num_tl_90g_dpd_24m,
      #  num_tl_op_past_12m, pct_tl_nvr_dlq, percent_bc_gt_75, tot_hi_cred_lim, total_bal_ex_mort, total_bc_limit, total_il_high_credit_limit
    
    # 2. Columns having more no. of NAs(36931 - 1 column, 25682 - 1 column)
    # mths_since_last_record, mths_since_last_delinq
    
    # 3. Columns having less no. of NAs (697 - 1 column - pub_rec_bankruptcies, 
    #    56 - 1 column - collections_12_mths_ex_med, chargeoff_within_12_mths, 39 - 1 column - tax_liens)
      summary(loan$tax_liens)
      unique(loan$tax_liens)
      summary(loan$collections_12_mths_ex_med)
      unique(loan$collections_12_mths_ex_med)
      summary(loan$chargeoff_within_12_mths)
      unique(loan$chargeoff_within_12_mths)
      # min, 1st, 3d Q, median, max are same for above columns & they are 0
      # 0 & NA are the unique values
      # so, we can consider this column to remove
  
    # 2. Find columns having constant values( eg: 0, INDIVIDAUL etc)
      const_cols <- c()
      for (col in colnames(loan)) {
        if(length(unique(loan[, col])) == 1 && !is.na(loan[, col])) {
          #print(paste("column name = ", col, ", value =", loan[1, col]))
          const_cols <- c(const_cols, col)
        }
      }
      print (const_cols)
      # column name =  pymnt_plan , value = n
      # column name =  initial_list_status , value = f
      # column name =  policy_code , value = 1
      # column name =  application_type , value = INDIVIDUAL
      # column name =  acc_now_delinq , value = 0
      # column name =  delinq_amnt , value = 0
  
    # 4. Columns having a constant value for most of the obs. & rest of the obs. are having NA (if min, max & mean are equal)
    #     collections_12_mths_ex_med, tax_liens, chargeoff_within_12_mths
    
    # 5. data format issues
    # term - remove "months" & convert to integer
    # int_rate, revol_util - remove % & convert it to dobule
    # emp_title - case is not uniform
    # emp_length - standardise into integer
    # date issues
    #   issue_d, earliet_cr_line, etc.
    
    # 6. Unnecessary columns for analysis
    # url, desc, title, zip_code, id, member_id
    
  ## 1b. Variable interpretation ##
    # Define other variables here.
    
#### 2. Data cleaning ####
    # 2a. Remove NA columns & constant value columns
      to_remove <- c(na_cols, const_cols)
      loan <- loan[, !colnames(loan) %in% to_remove]
      
    # 2b. Remove unnecessary columns
      loan <- loan[, !colnames(loan) %in% c('url', 'desc', 'title', 'zip_code', 'id', 'member_id', 'tax_liens', 'collections_12_mths_ex_med',
                                            'chargeoff_within_12_mths')]
      
    # 2c. Format percentage column into double
      loan$int_rate <- as.double(gsub(loan$int_rate, pattern = '\\s+|%', replacement = ''), length = 2)
      loan$revol_util <- as.double(gsub(loan$revol_util, pattern = '\\s+|%', replacement = ''), length = 2)
      
    # 2d. Standardise emp_length column
      loan$emp_length <- gsub(loan$emp_length, pattern = '\\s+|\\+|years?', replacement = '', ignore.case = T)
      loan$emp_length <- gsub(loan$emp_length, pattern = '<1', replacement = '0')
      loan$emp_length <- as.integer(loan$emp_length)
      
      
    # 2e. Format date columns  
      # convert year into length years (4 digits)
      loan$earliest_cr_line <- gsub(loan$earliest_cr_line, pattern="-([0-1])(\\d)$", replacement = "-20\\1\\2")
      loan$earliest_cr_line <- gsub(loan$earliest_cr_line, pattern="-([^0-1])(\\d)$", replacement = "-19\\1\\2")
      
      # Format Date function
      formatDate <- function (df_variable, format = '%d-%b-%y')
      {
        return (as.POSIXct(strptime(paste('1-', df_variable, sep=""), format)))
      }
      
      loan$issue_d <- formatDate(loan$issue_d)
      loan$earliest_cr_line <- formatDate(loan$earliest_cr_line, '%d-%b-%Y')
      loan$last_pymnt_d <- formatDate(loan$last_pymnt_d)
      loan$next_pymnt_d <- formatDate(loan$next_pymnt_d)
      loan$last_credit_pull_d <- formatDate(loan$last_credit_pull_d)
      
    # 2f. Format string columns
      loan$term <- as.integer(gsub(loan$term, pattern = '\\s+|months', replacement = ''))
      loan$emp_title <- ifelse(trimws(loan$emp_title) == "", "unspecified", loan$emp_title) # replace "" emp_title to "unspecified"
      loan$addr_state <- toupper(loan$addr_state)
      loan$sub_grade <- toupper(loan$sub_grade)
      loan$purpose <- tolower(loan$purpose)
      
    # 2g. Format numeric columns
      loan$total_rec_late_fee <- as.double(loan$total_rec_late_fee, digits = 2)
    
    # 2h. Convert strings to factors for applicable columns
      loan$emp_title <- as.factor(tolower(loan$emp_title))
      loan$purpose <- as.factor(tolower(loan$purpose))
      loan$term <- as.factor(loan$term)
      loan$grade <- as.factor(loan$grade)
      loan$sub_grade <- as.factor(loan$sub_grade)
      loan$home_ownership <- as.factor(loan$home_ownership)
      loan$verification_status <- as.factor(loan$verification_status)
      loan$loan_status <- as.factor(loan$loan_status)
      loan$addr_state <- as.factor(loan$addr_state)
      loan$inq_last_6mths <- as.factor(loan$inq_last_6mths)
      loan$mths_since_last_delinq <- as.factor(loan$mths_since_last_delinq)
      
    # 2i. Fill NA values
      loan$revol_util[is.na(loan$revol_util)] <- 0 # revol_util has 50 out of 39717 records, so assign 0
    
    # KEEP IT AS THE LAST STEP OF DATA CLEANING, SO WE DON'T NEED TO ABBREAVTE/CORRECT UNNECESSARY COLUMNS.    
    # 2j. Fix rows - spelling correction & abbrevate
      colnames(loan) <- c("loan_amount", "funded_amount", "funded_amount_by_investor", "payment_term_months", "interest_rate",
                          "installment_amount", "grade", "sub_grade", "emp_job_title",
                          "employment_length", "home_ownership", "annual_income", "verification_status", "issue_date", 
                          "loan_status", "purpose",
                          "address_state", "debt_to_incom_ratio", "delinq_2years", "earliest_credit_line",
                          "inqquires_in_last_6months", "months_since_last_delinq",
                          "months_since_last_record", "open_accounts", "pub_records", "revolving_balance", "revolving_utilization_rate",
                          "total_accounts", "outstanding_principal",
                          "outstanding_principal_investor", "total_payment", "total_pyment_investor", "total_received_principal",
                          "total_received_interest", "total_received_late_fee",
                          "recoveries", "collection_recovery_fee", "last_payment_date", "last_payment_amount", "next_payment_date",
                          "last_credit_pull_date", "public_record_bankruptcies")
      
#### 3a. Data analysis - define flow of analysis ####
    # I.univariate analysis
    # II. bivariate analysis
    # III. segmented analysis
    # IV. derive metrics
    # All above should be done with the 2 considerations of two risks (1. losing business, 2. losing finance)
      
#### 3b. Univariate analysis ####
      drawCategoricalUnivariatePlot <- function (variable, x_label, y_label = 'Freqency(%)', fill_color = 'orange')
      {
        plot <- ggplot(loan, aes(variable)) +
          scale_y_continuous(labels = scales::percent) +
          ylab(y_label) +
          xlab(x_label) +
          theme(axis.text.x = element_text(angle = 10))
        
        plot + geom_bar(aes(y = (..count../sum(..count..))), width = 0.7, fill = fill_color)
      }
    # I. categorical variable analysis
      drawCategoricalUnivariatePlot(loan$payment_term_months, 'Payment term( months)')
      drawCategoricalUnivariatePlot(loan$grade, 'LC assigned grade', 'Percentage', 'green')
      drawCategoricalUnivariatePlot(loan$sub_grade, 'LC assigned sub grade', 'Percentage', 'blue')
      # drawCategoricalUnivariatePlot(loan$emp_job_title, 'Job title', 'Percentage')
      drawCategoricalUnivariatePlot(loan$employment_length, 'Employment length(years)', 'Percentage', 'purple')
      drawCategoricalUnivariatePlot(loan$home_ownership, 'Home ownership', 'Percentage', 'magenta')
      drawCategoricalUnivariatePlot(loan$verification_status, 'Verification status', 'Percentage', 'red')
      drawCategoricalUnivariatePlot(loan$loan_status, 'Loan status', 'Percentage', 'yellow')
      drawCategoricalUnivariatePlot(loan$purpose, 'Loan purpose', 'Percentage', 'maroon')
      drawCategoricalUnivariatePlot(loan$address_state, 'Borrower state', 'Percentage', 'skyblue')
      
      ## TODO: Create function to get the actual counts
      
      ## TODO: employment length - NA - decide value based on the other details???

      