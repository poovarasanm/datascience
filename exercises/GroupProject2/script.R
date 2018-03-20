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

# Column correction
# 1. abbrevate the columns
colnames(loan) <- c('id', 'member_id', 'loan_amount', 'funded_amount', 'funded_amount_investor', 'payment_term',
                    'int_rate', 'installment_amount', 'grade', 'sub_grade', 'borrower_job_title', 'borrower_employment_length',
                    'borrower_home_ownership', 'borrower_annual_income', 'income_verification_status', 'issue_date',
                    'loan_status', 'payment_plan', 'lc_page_url', 'loan_description', 'purpose_category', 'loan_title',
                    'borrower_zipcode', 'borrower_addr_state', 'debt_to_incom_ratio', 'delinq_2yrs', 'earliest_cr_line',
                    'inquiries_last_6months', 'months_since_last_delinq', 'months_since_last_public_record',
                    'open_acc', 'public_rec', 'revolving_balance', 'revolve_line_until_rate', 'total_acc',
                    'initial_list_status', 'outstanding_principal_amount', 'outstanding_principal_amount_investor',
                    'total_payment', 'total_payment_investor', 'total_principal_received_amount', 'total_interest_received_amount',
                    'total_late_fee_received', 'recoveries', 'collection_recovery_fee', 'last_payment_date')...

# Data cleaning

# Characters
loan$term <- as.integer(gsub(loan$term, pattern = '\\s+|months', replacement = ''))
loan$int_rate <- as.double(gsub(loan$int_rate, pattern = '\\s+|%', replacement = ''), length = 2)
loan$revol_util <- as.double(gsub(loan$revol_util, pattern = '\\s+|%', replacement = ''), length = 2)

loan$emp_length_rounded <- gsub(loan$emp_length, pattern = '\\s+|\\+|years?', replacement = '', ignore.case = T)
loan$emp_length_rounded <- gsub(loan$emp_length_rounded, pattern = '<1', replacement = '0')

loan$title <- tolower(loan$title)
loan$desc <- tolower(loan$desc)

# Dates
formatDate <- function (df_variable)
{
  return(strptime(paste('1-', df_variable), '%d- %b-%y'))
}

loan$issue_d <- formatDate(loan$issue_d)
loan$earliest_cr_line <- formatDate(loan$earliest_cr_line)
loan$last_pymnt_d <- formatDate(loan$last_pymnt_d)
loan$next_pymnt_d <- formatDate(loan$next_pymnt_d)
loan$last_credit_pull_d <- formatDate(loan$last_credit_pull_d)

# remove aggregated/summary columns OR expand them to new columns