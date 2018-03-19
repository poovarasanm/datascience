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

# Data cleaning
loan$term <- as.integer(gsub(loan$term, pattern = '\\s+|months', replacement = ''))
loan$int_rate <- as.double(gsub(loan$int_rate, pattern = '\\s+|%', replacement = ''), length = 2)
loan$emp_length_rounded <- as.integer(gsub(loan$emp_length, pattern = '\\s+|\\+|\\<|years?', replacement = ''))
