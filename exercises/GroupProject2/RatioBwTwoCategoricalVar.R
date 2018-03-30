install.packages('data.table')
library('data.table')

grade_loan_status <- loan %>% group_by(grade, loan_status) %>% summarise(count = n())
      grade_loan_status$loan_status <- as.factor(grade_loan_status$loan_status)
      dcast(setDT(grade_loan_status), formula = grade ~ loan_status, value.var = 'count')[, percentage:= round((`Charged Off`/`Fully Paid`)*100, digits=2)][]
      # grade Charged Off Fully Paid percentage
      # 1:     A         602       9443       6.38
      # 2:     B        1425      10250      13.90
      # 3:     C        1347       6487      20.76
      # 4:     D        1118       3967      28.18
      # 5:     E         715       1948      36.70
      # 6:     F         319        657      48.55
      # 7:     G         101        198      51.01
      # If grade decreases, rise in the charged off count
