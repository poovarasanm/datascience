##### EDA Case Study: ##### 
#   Client                : lendingclub.com
#   Analytics team        : Jayakumar, Pranesh, Chaitanya, Poovarasan
#   Learning outcome      : EDA, basic understaning of Risk analytics in banking & how data used to minimise the risk of
#                           losing money while lending to customer

#### Expected result ####
#   R code, Presentation (approach, univariate, bivariate analysis & summary)
#   1. Data understanding & preparation - a. data quality correction & b. interpretation of variables.
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