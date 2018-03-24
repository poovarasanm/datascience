##
##
##
loan <- read.csv('loan.csv')

str(loan)
head(loan)
summary(loan)
length(loan)
nrow(loan)
# Business objective

# Business understanding

# Data understanding
# URL - https://lendingclub.com/browse/loanDetail.action?loan_id=<id>
# payment_plan - n
# initial_list_status - f
# application_type - INDIVIDUAL
# delinq_amount - 0
# policy_code - 1

# Data cleaning

# Characters
loan$term <- as.integer(gsub(loan$term, pattern = '\\s+|months', replacement = ''))
loan$int_rate <- as.double(gsub(loan$int_rate, pattern = '\\s+|%', replacement = ''), length = 2)
loan$revol_util <- as.double(gsub(loan$revol_util, pattern = '\\s+|%', replacement = ''), length = 2)

loan$emp_length_rounded <- gsub(loan$emp_length, pattern = '\\s+|\\+|years?', replacement = '', ignore.case = T)
loan$emp_length_rounded <- gsub(loan$emp_length_rounded, pattern = '<1', replacement = '0')

loan$title <- tolower(loan$title)
loan$desc <- tolower(loan$desc)

# convert year into length years (4 digits)
loan$earliest_cr_line <- gsub(loan$earliest_cr_line, pattern="-([0-1])(\\d)$", replacement = "-20\\1\\2")
loan$earliest_cr_line <- gsub(loan$earliest_cr_line, pattern="-([^0-1])(\\d)$", replacement = "-19\\1\\2")

# Dates
formatDate <- function (df_variable, format = '%d- %b-%y')
{
  return(strptime(paste('1-', df_variable), format))
}

loan$issue_d <- formatDate(loan$issue_d)
loan$earliest_cr_line <- formatDate(loan$earliest_cr_line, '%d- %b-%Y')
loan$last_pymnt_d <- formatDate(loan$last_pymnt_d)
loan$next_pymnt_d <- formatDate(loan$next_pymnt_d)
loan$last_credit_pull_d <- formatDate(loan$last_credit_pull_d)

# Numeric formats
loan$total_rec_late_fee <- as.double(loan$total_rec_late_fee, digits = 2)

# Filtering observations
no_of_loan_observations <- nrow(loan)
# remove variable, if all observations are NA
loan_filtered <- loan[, colSums(is.na(loan)) < no_of_loan_observations]

# remove variables (Findings from summary() results)
# lc_page_url - equivalent to the load Id
# payment_plan, initial_list_status, application_type, policy_code - same for all obs
loan_filtered <- loan_filtered[, !(names(loan_filtered) %in% c('url', 'pymnt_plan', 'initial_list_status',
                                                               'application_type', 'delinq_amnt', 'policy_code'))]

    
summary(loan_filtered)

# Column correction
# 1. abbrevate the columns
colnames(loan_filtered) <- c('id', 'member_id', 'loan_amount', 'funded_amount', 'funded_amount_investor', 'payment_term',
                    'int_rate', 'installment_amount', 'grade', 'sub_grade', 'borrower_job_title', 'borrower_employment_length',
                    'borrower_home_ownership', 'borrower_annual_income', 'income_verification_status', 'issue_date',
                    'loan_status', 'loan_description', 'purpose_category', 'loan_title',
                    'borrower_zipcode', 'borrower_addr_state', 'debt_to_incom_ratio', 'delinq_2yrs', 'earliest_cr_line',
                    'inquiries_last_6months', 'months_since_last_delinq', 'months_since_last_public_record',
                    'open_acc', 'public_rec', 'revolving_balance', 'revolve_line_until_rate', 'total_acc',
                    'outstanding_principal_amount', 'outstanding_principal_amount_investor',
                    'total_payment', 'total_payment_investor', 'total_principal_received_amount', 'total_interest_received_amount',
                    'total_late_fee_received', 'recoveries', 'collection_recovery_fee', 'last_payment_date',
                    'last_payment_amount', 'next_payment_date', 'last_credit_pull_date', 'collection_12_months_excluding_medical',
                    'acc_now_delinq', 'chargeoff_within_12_months',
                    'no_of_public_bankruptics', 'emp_length_rounded')

# remove aggregated/summary columns OR expand them to new columns