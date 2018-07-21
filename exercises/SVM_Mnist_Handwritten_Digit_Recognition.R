#### SVM - Handwritten digit recognition ####
# Author         : Poovarasan
# Objective      : To develop a model using Support Vector Machine which should correctly classify
#                    the handwritten digits based on the pixel values given as features.
# Input          : MNIST data      
# Result expected: R code
# NOTE           : Use stratified sampling for train & test data sampling


#### 1. Load the dataset ####
  train_data <- read.csv('mnist_train.csv')
  test_data <- read.csv('mnist_test.csv')
  
#### 2. Install & load libs ####
  install.packages("caret")
  install.packages("kernlab")
  install.packages("dplyr")
  install.packages("readr")
  install.packages("ggplot2")
  install.packages("gridExtra")
  
  library("caret")
  library("kernlab")
  library("dplyr")
  library("readr")
  library("ggplot2")
  library("gridExtra")
  
#### 3. Data understanding ####
  glimpse(train_data)
  head(train_data)
  dim(train_data)
  #### 3a. Train data ####
  # Train_data has 59999 observations of 785 variables
  # Variable X5 - labels
  # remaining 784 variables - pixel value ranging 0-255 i.e, 28 x 28 matrix
  
  #### 3b. Test data ####
  glimpse(test_data)
  head(test_data)
  dim(test_data)
  
  # Test data has 9999 obsercations of 785 variables
  # Test & train data has same variables
  
#### 4. Identify & fix data quality issues ####
  
  # Column names quality issues
    train_cols <- colnames(train_data)
    test_cols <- colnames(test_data)
    
    # has difference?
    setdiff(train_cols, test_cols) # Yes, header names are different
    
    # The CSV file does not have the header row
    # The header names are not consistent
    # First column can be renamed to label, and remaning can be renamed to pixel_1, pixel_2... pixel_784
    header <- c()
    for(i in 1:784) {
      header[i] <- paste('pixel_', i, sep='')
    }
    header <- c('label', header)
    
    # set header
    colnames(train_data) <- header
    colnames(test_data) <- header
  
  # has NAs?
    table(colSums(is.na(train_data))) # No
    table(colSums(is.na(test_data))) # No
  
  # has duplicate digit labels?
    dim(train_data)[1] == length(unique(train_data[, 1])) # False - yes has duplicates
    dim(test_data)[1] == length(unique(test_data[, 1]))  # False - yes has duplicates
  
  # ok, how many duplicates?
  # to check, convert digit value to factor
    train_data$label <- as.factor(train_data$label)
    train_data %>% group_by(label) %>% summarise(n = n())
    # A tibble: 10 x 2
    # label   n
    #<fct> <int>
    #  0     5923
    # 1      6742
    # 2      5958
    # 3      6131
    # 4      5842
    # 5      5420
    # 6      5918
    # 7      6265
    # 8      5851
    # 9      5949
    # so, we can use stratified sampling
    
    test_data$label <- as.factor(test_data$label)
    test_data %>% group_by(label) %>% summarise(n = n())
    # A tibble: 10 x 2
    #label    n
    #<fct> <int>
    # 0       980
    # 1      1135
    # 2      1032
    # 3      1010
    # 4       982
    # 5       892
    # 6       958
    # 7      1027
    # 8       974
    # 9      1009
    
#### 5. Scaling ####
    # Value ranging from 0 - 255, so scaling them by dividing 255 would be better
    train_data[, -1] = train_data[, -1]/255
    test_data[, -1] = test_data[, -1]/255
  
#### 6. Sample selection ####
    set.seed(100)
    # Using stratified sampling method
    train <- train_data %>% group_by(label) %>% sample_n(500) # <5000 - choosing 2000 for fast run
    
#### 7. Model building & evaluation #####
    #### Assumption - considerning the warnings can be suppressed ####
    options(warn = -1)
    # Approach:
      # 1. Create the model with the different Kernal types
      # 2. Evaluate the model & create the confusion matrix to find the performance
      # 3. Train the model on test data with tuning parameters
      # 4. Plot the performance of the model
      # 5. Predict the class on test dataset
      # 6. Evaluate the prediction perfromance
      # 7. Choose the model which performed well on the test dataset
    # Training the model & performance tuning approach
      # 1. Assume the range of values for the parameters eg: 0.1, 1, 10, 100
      # 2. Train the model with above set & plot the accuracy
      # 3. If the accuracy line has no variance, (straight line) the increase the difference b/w the values
      # 4. If the value causes no change in accuracy, then remove that value from the set
      # 5. Choose the final param set to train the model
    
    # Define validation params, so can be used for different kernal types
    # The best Sigma, hyperparameters are chosen from multiple trial runs
    trainControl <- trainControl(method="cv", number=2, verboseIter=TRUE) # Using 2 fold CV
    metric <- "Accuracy"
  
    #### 7b. Linear kernal ####
      linear_svm <- ksvm(label~ ., data = train, kernel = "vanilladot")
      linear_svm_evaluation <- predict(linear_svm, test_data)
      
      #confusion matrix - Linear Kernel
        caret::confusionMatrix(linear_svm_evaluation, test_data$label)
        # Accuracy : ~0.907        
        # 95% CI : (0.9013, 0.9128)
        
      # train & tuning performance
        grid <- expand.grid(.C= c(0.05, 0.01, 1))
        fit.linear_kvm <- train(label~., data=train, method="svmLinear",
              metric=metric, tuneGrid=grid, trControl=trainControl)
        print(fit.linear_kvm)
        plot(fit.linear_kvm)
        # C=0.05, Accuracy ~ 0.913
      
    #### 7c. Polynomial kernal ####
      poly_svm <- ksvm(label~ ., data = train, kernel = "polydot")
      poly_svm_evaluation <- predict(poly_svm, test_data)
      
      #confusion matrix - Polynomial Kernel
        caret::confusionMatrix(poly_svm_evaluation, test_data$label)
        # Accuracy : ~0.907         
        # 95% CI : (0.9013, 0.9128)
        
      # train & tuning performance
        grid <- expand.grid(.C= c(0.01, 0.05), .degree=c(1, 3), .scale = c(5, 10))
        fit.poly_kvm <- train(label~., data=train, method="svmPoly",
                              metric=metric, tuneGrid=grid, trControl=trainControl)
        print(fit.poly_kvm)
        # Fitting degree = 3, scale = 5 and C = 0.01, Accuracy ~0.921
        plot(fit.poly_kvm)
      
    #### 7d. RBF kernal ####
      rbf_kernel_svm <- ksvm(label~ ., data = train, kernel = "rbfdot")
      rbf_kernel_svm_evaluation <- predict(rbf_kernel_svm, test_data)
      
      # confusion matrix - Polynomial Kernel
        caret::confusionMatrix(rbf_kernel_svm_evaluation, test_data$label)
        
        #  Accuracy : ~ 0.950       
        #  95% CI : (0.9461, 0.9547)
        
      # train & tuning performance
        grid <- expand.grid(.sigma = c(0.01, 0.05), .C= c(1, 3))
        fit.rbf_svm <- train(label~., data=train, method="svmRadial",
                         metric=metric, tuneGrid=grid, trControl=trainControl)
        
        print(fit.rbf_svm)
        # sigma = 0.01 and C = 3, Accuracy ~0.942
        plot(fit.rbf_svm)
        
#### 8. Final model selection ####
        evaluate_fit.linear_kvm <- predict(fit.linear_kvm, test_data)
        confusionMatrix(evaluate_fit.linear_kvm, test_data$label)
        # Accuracy : ~0.924
        
        evaluate_fit.poly_kvm <- predict(fit.poly_kvm, test_data)
        confusionMatrix(evaluate_fit.poly_kvm, test_data$label)
        # Accuracy : ~0.940
        
        evaluate_fit.rbf_svm <- predict(fit.rbf_svm, test_data)
        confusionMatrix(evaluate_fit.rbf_svm, test_data$label)
        # Accuracy : ~0.957
        
        final_model = fit.poly_kvm
      
#### 9. Conclusion ####
      # Validation done on 1000 samples by the propotional stratified sampling method.
        
      # SVM with RBF Kernal has given the highest accuracy of ~95.7%( 0.957) on the test data prediction.
      # when sigma = 0.05 and C = 3
      # Per class Sensitivity & Specificity are
      #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
      #  Sensitivity          0.98571   0.9824  0.95833  0.94059  0.96436  0.96525  0.96347  0.93768  0.94867  0.92567
      #  Specificity          0.99734   0.9984  0.98985  0.99522  0.99490  0.99495  0.99723  0.99621  0.99335  0.99511
        
      # Both the models Linear, polynomial & RBF are almost having the equal performance
      # When the speed is considered, linear model can be chosed & when performance considered RBF can be chosed.
      
      # Final model would be the SVM with the RBF Kernal.
        
      # The dataset is having clear classification hence all the models are performing well.