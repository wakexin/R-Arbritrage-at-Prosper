# R-Arbritrage-at-Prosper


#### Tianze Wang, Kexin Wang, Chuxin Wu, Fenglin Liu, Renjie Liu, Matthew
Cline

#### 10/23/2019

# 1\. Introduction

In this project, we would like to study about the loan market. To be specific,
we are specifically interested in two problems:

  1. How interest rate for loans is determined.
  2. What factors lead to a loan default.

To study the first problem, we will build a linear regression model to get
idea of what is considered as the credit risk. To study the second problem, we
will build a logistic regression model to analyze factors lead to loan
default. We will start by looking at the first problem.

_Pleae note that lines commented with double pound sign`##` means explanation
for the codes for better understanding._

_Please note that codes commented with single pound sign`#` means original
codes for us to view the intermediate results. For simplicity in the final
report, we commented these lines._

# 2\. Problem 1: Loan interest rate

## 2.1 Initialization and Load packages

Our initial step is to load all the packages to be used in this part. We will
only need to use lmtest and car for the first problem. The packages are
`lmtest` to see the p-value without generating summary to ruin the output, and
`car` access the `vif()` method.

    
    
    library(lmtest) 
    
    
    ## Loading required package: zoo
    
    
    ## 
    ## Attaching package: 'zoo'
    
    
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    
    
    library(car)
    
    
    ## Loading required package: carData
    
    
    ## Read the data
    listings <- read.csv("ProjectA_Listings2013.csv")
    # summary(Project1)

## 2.2 Data Cleaning

In this part, we cleaned our data based on our discussion of the importance of
each column. For detailed information of both questions, please refer to [this
page](https://docs.google.com/spreadsheets/d/1jiFpqwbf4P_CW_nqFJ5SfonhvmX1Ep2TDNupmQa4mhA/edit?usp=sharing)
for how we dealt with each variable.

For loan status, we classified both charge-off and defaulted as defaulted and
divieded the loan_status into two main categories: completed and defaulted.

    
    
    listings$loan_status <- as.factor(listings$loan_status)
    # class(listings$loan_status)
    # levels(listings$loan_status)
    ## 1 means completed, and 2 means defaulted
    levels(listings$loan_status) <- c("1", "2", "2", "1")

We redefined listing_term based on years for ease of use

    
    
    levels(listings$listing_term) <- c("1", "3", "5")
    # levels(listings$listing_term)

We factorized some categorical variables as shown below.

    
    
    listings$listing_term <- as.factor(listings$listing_term)
    listings$prosper_score <- as.factor(listings$prosper_score)
    listings$income_range <- as.factor(listings$income_range)
    listings$prosper_score <- as.factor(listings$prosper_score)
    listings$lender_indicator <- as.factor(listings$lender_indicator)
    listings$employment_status_description <- as.factor(listings$employment_status_description)
    listings$income_verifiable <- as.factor(listings$income_verifiable)
    listings$income_range <- as.factor(listings$income_range)

Instead of factorizing, we turned prosper_rating and scorex into numeric
values to better interprite the relationship between prospe_rating and
interest rate. To be specific, we assigned 1-7 to different prosper_rating_id
and 1-11 to different scorex_id respectively where 1 indicating the lowest
prosper_rating and lowest scorex.

    
    
    ## change categorical varibales into numeric value in order to better capture the relationship between prospe_rating and interest rate
    listings$prosper_rating_id <- 0
    listings$prosper_rating_id[listings$prosper_rating == "AA"] <- 7
    listings$prosper_rating_id[listings$prosper_rating == "A"] <- 6
    listings$prosper_rating_id[listings$prosper_rating == "B"] <- 5
    listings$prosper_rating_id[listings$prosper_rating == "C"] <- 4
    listings$prosper_rating_id[listings$prosper_rating == "D"] <- 3
    listings$prosper_rating_id[listings$prosper_rating == "E"] <- 2
    listings$prosper_rating_id[listings$prosper_rating == "HR"] <- 1
    listings$prosper_rating <- NULL
    colnames(listings)[colnames(listings)=="prosper_rating_id"] <- "prosper_rating"
    ## Process scorex
    listings$scorex_id <- 0
    listings$scorex_id[listings$scorex == "< 600"] <- 1
    listings$scorex_id[listings$scorex == "600-619"] <- 2
    listings$scorex_id[listings$scorex == "620-639"] <- 3
    listings$scorex_id[listings$scorex == "640-649"] <- 4
    listings$scorex_id[listings$scorex == "650-664"] <- 5
    listings$scorex_id[listings$scorex == "665-689"] <- 6
    listings$scorex_id[listings$scorex == "690-701"] <- 7
    listings$scorex_id[listings$scorex == "702-723"] <- 8
    listings$scorex_id[listings$scorex == "724-747"] <- 9
    listings$scorex_id[listings$scorex == "748-777"] <- 10
    listings$scorex_id[listings$scorex == "778+"] <- 11
    listings$scorex <- NULL
    colnames(listings)[which(colnames(listings)=="scorex_id")] <- "scorex"

We dropped the following columns of variables because they are either too
fragmented, or highly correlated, or not relevant to the question. For
example, some of the varaibles are generated after interest rate is
determined. Therefore, they are not useful in terms of determining interest
rate.

    
    
    listings$loan_origination_date <- NULL
    listings$listing_monthly_payment <- NULL
    listings$listing_category_id <- NULL
    # levels(listings$income_range)
    listings$loan_origination_date <- NULL
    listings$occupation <- NULL
    listings$borrower_city <- NULL
    listings$borrower_state <- NULL
    # Process prosper_rating
    # unique(listings$prosper_rating)
    listings$occupation <- NULL
    listings$borrower_city <- NULL
    listings$borrower_state <- NULL
    listings$current_delinquencies <- NULL
    listings$first_recorded_credit_line <- NULL
    listings$credit_lines_last7_years <- NULL
    listings$inquiries_last6_months <- NULL
    listings$credit_lines_notontime <- NULL
    listings$listing_monthly_payment <- NULL
    listings$listing_category_id <- NULL
    listings$stated_monthly_income <- NULL
    listings$loan_status_description<- NULL

Convert boolean into numeric value.

    
    
    listings$income_verifiable <-  ifelse(listings$income_verifiable == "TRUE",1,0)

### 2.1.0 Dealing with variables that don’t make sense

In this part, we are dealing with abnormally high dti ratio. The ratio of
1e+06 can be interpreted as either the person has no income or all of his or
her incomes are used to pay for the debt. Therefore, we replace 1e+06 with 1.

    
    
    listings$dti_wprosper_loan <- ifelse(listings$dti_wprosper_loan == 1e+06, 1, listings$dti_wprosper_loan)

Dealing with error: we replaced months_employed data which are less than 0
with mean values.

    
    
    listings$months_employed <- ifelse(listings$months_employed<0, mean(listings$months_employed), listings$months_employed)

Dealing with missing value: We used the following code to check missing value

    
    
    # Check missing value for whole dataframe, found missing values
    # is.na(listings$months_employed) 
    # Check 
    # for (i in colnames(listings)) {
    #   print(i)
    #   print(mean(is.na(listings[i])))
    # }

It told us both `months_employed` and `installment_balance` contain missing
values. Since the scale of the missing value is small, we chose to interpolate
the missing valuew with mean value of the column instead of dropping the whole
rows or column.

    
    
    listings$months_employed <- ifelse(is.na(listings$months_employed), mean(listings$months_employed, na.rm = TRUE), listings$months_employed)
    listings$installment_balance <- ifelse(is.na(listings$installment_balance), mean(listings$installment_balance, na.rm = TRUE), listings$installment_balance)

## 2.3 Model Setup

### 2.3.0 Train-test split

We divided 75% of all data to be trained and 25% to be tested.

    
    
    smp_size = floor(0.75*nrow(listings))
    train_ind = sample(seq_len(nrow(listings)),size = smp_size)
    listings_train = listings[train_ind,]
    listings_test = listings[-train_ind,]

### 2.3.1 Model 1

We built the first model based on all the data we have.

    
    
    listings_cols = colnames(listings)
    y_borrower_interest_rate = listings$borrower_rate
    listings_cols = listings_cols[which(listings_cols != "borrower_rate")]
    relevant.x = listings_cols
    sig.formula <- as.formula(paste("borrower_rate ~",paste(relevant.x, collapse= "+")))
    model1 = lm(sig.formula, listings_train)
    sm1 = summary(model1)

The mean square error of the model is 1.640239

    
    
    mse1 = mean(sm1$residuals^2)
    cat(mse1)
    
    
    ## 1.745232

Below are the accuracy of our model in different confidence intervals

    
    
    listings_cp = listings_test
    listings_cp$predicted = predict(model1, newdata = listings_test)
    
    
    ## Warning in predict.lm(model1, newdata = listings_test): prediction from a
    ## rank-deficient fit may be misleading
    
    
    accuracy = listings_cp$predicted/listings_cp$borrower_rate*100
    
    print('model 1')
    
    
    ## [1] "model 1"
    
    
    for (interval_len in c(5,10,15) ){
      cat("for interval of", 100-interval_len,"% ~", 100+interval_len, "%: ")
      cat(length(accuracy[accuracy>(100-interval_len)& accuracy<(100+interval_len)])/length(accuracy), "\r\n")
    }
    
    
    ## for interval of 95 % ~ 105 %: 0.4729311 
    ## for interval of 90 % ~ 110 %: 0.7937038 
    ## for interval of 85 % ~ 115 %: 0.9469354 

### 2.3.2 Model 2

Our second model is based on the first model. In this model, we removed all
the insignificant variables.

    
    
    # Significant model2 based on model 1
    toselect.x = summary(model1)$coeff[-1,4] < 0.05
    relevant.x <- names(toselect.x)[toselect.x == TRUE] 
    # sig.formula <- as.formula(paste("borrower_rate ~",paste(relevant.x, collapse= "+")))
    # sig.formula
    model2 = lm(borrower_rate ~ number_of_days + principal_balance + loan_status + 
        amount_funded + listing_term + 
        dti_wprosper_loan + employment_status_description +
        + lender_indicator + 
        public_records_last12_months + amount_delinquent + current_credit_lines + 
        open_credit_lines + total_open_revolving_accounts + delinquencies_over30_days + 
        is_homeowner + prosper_rating + scorex, listings_train)
    sm2 = summary(model2)
    summary(model2)
    
    
    ## 
    ## Call:
    ## lm(formula = borrower_rate ~ number_of_days + principal_balance + 
    ##     loan_status + amount_funded + listing_term + dti_wprosper_loan + 
    ##     employment_status_description + +lender_indicator + public_records_last12_months + 
    ##     amount_delinquent + current_credit_lines + open_credit_lines + 
    ##     total_open_revolving_accounts + delinquencies_over30_days + 
    ##     is_homeowner + prosper_rating + scorex, data = listings_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6658 -0.9762  0.0019  1.0053  5.8546 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error  t value
    ## (Intercept)                                 2.792e+01  1.384e-01  201.702
    ## number_of_days                              3.521e-03  7.920e-05   44.456
    ## principal_balance                          -3.415e-06  2.845e-06   -1.200
    ## loan_status2                                1.052e+00  3.250e-02   32.364
    ## amount_funded                              -2.114e-05  1.725e-06  -12.251
    ## listing_term36                              4.320e+00  1.064e-01   40.602
    ## listing_term60                              4.937e+00  1.070e-01   46.140
    ## dti_wprosper_loan                           1.586e-03  2.437e-03    0.651
    ## employment_status_descriptionFull-time      1.212e-01  8.267e-02    1.466
    ## employment_status_descriptionNot employed   6.253e-01  2.278e-01    2.746
    ## employment_status_descriptionOther          5.077e-02  4.049e-02    1.254
    ## employment_status_descriptionPart-time      1.447e-01  8.053e-01    0.180
    ## employment_status_descriptionRetired       -5.271e-01  3.387e-01   -1.556
    ## employment_status_descriptionSelf-employed -3.547e-03  4.055e-02   -0.087
    ## lender_indicator1                          -3.770e-02  4.004e-02   -0.942
    ## public_records_last12_months                7.963e-02  5.777e-02    1.378
    ## amount_delinquent                           2.789e-06  1.077e-06    2.590
    ## current_credit_lines                        1.681e-02  6.651e-03    2.528
    ## open_credit_lines                          -2.694e-02  7.730e-03   -3.485
    ## total_open_revolving_accounts               6.572e-03  3.932e-03    1.671
    ## delinquencies_over30_days                   6.257e-03  1.418e-03    4.411
    ## is_homeownerTRUE                            6.298e-05  2.010e-02    0.003
    ## prosper_rating                             -3.909e+00  7.913e-03 -494.026
    ## scorex                                     -5.805e-02  4.781e-03  -12.142
    ##                                            Pr(>|t|)    
    ## (Intercept)                                 < 2e-16 ***
    ## number_of_days                              < 2e-16 ***
    ## principal_balance                          0.229984    
    ## loan_status2                                < 2e-16 ***
    ## amount_funded                               < 2e-16 ***
    ## listing_term36                              < 2e-16 ***
    ## listing_term60                              < 2e-16 ***
    ## dti_wprosper_loan                          0.515197    
    ## employment_status_descriptionFull-time     0.142708    
    ## employment_status_descriptionNot employed  0.006044 ** 
    ## employment_status_descriptionOther         0.209841    
    ## employment_status_descriptionPart-time     0.857374    
    ## employment_status_descriptionRetired       0.119654    
    ## employment_status_descriptionSelf-employed 0.930311    
    ## lender_indicator1                          0.346396    
    ## public_records_last12_months               0.168062    
    ## amount_delinquent                          0.009602 ** 
    ## current_credit_lines                       0.011481 *  
    ## open_credit_lines                          0.000493 ***
    ## total_open_revolving_accounts              0.094639 .  
    ## delinquencies_over30_days                  1.03e-05 ***
    ## is_homeownerTRUE                           0.997501    
    ## prosper_rating                              < 2e-16 ***
    ## scorex                                      < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.394 on 25132 degrees of freedom
    ## Multiple R-squared:  0.9475, Adjusted R-squared:  0.9474 
    ## F-statistic: 1.972e+04 on 23 and 25132 DF,  p-value: < 2.2e-16

The MSE is

    
    
    mse2 = mean(sm2$residuals^2)
    cat(mse2)
    
    
    ## 1.941644

The accuracy of prediction is

    
    
    listings_cp$predicted = predict(model2, newdata = listings_test)
    accuracy = listings_cp$predicted/listings_cp$borrower_rate*100
    print('model 2')
    
    
    ## [1] "model 2"
    
    
    for (interval_len in c(5,10,15) ){
      cat("for interval of", 100-interval_len,"% ~", 100+interval_len, "%: ")
      cat(length(accuracy[accuracy>(100-interval_len)& accuracy<(100+interval_len)])/length(accuracy), "\r\n")
    }
    
    
    ## for interval of 95 % ~ 105 %: 0.4481278 
    ## for interval of 90 % ~ 110 %: 0.7538755 
    ## for interval of 85 % ~ 115 %: 0.9285714 

Both factors indicate that the second model is not as good as the first one.

### 2.3.3 Model 3

With the same strategy, we further removed non-significant variables in model
2, and after this we found almost all the remaining variables are somehow
significant.

    
    
    # Model 3: Refined model based on model 2
    toselect.x = summary(model2)$coeff[-1,4] < 0.05
    relevant.x <- names(toselect.x)[toselect.x == TRUE] 
    sig.formula2 <- (paste("borrower_rate ~",paste(relevant.x, collapse= " + ")))
    sig.formula2
    
    
    ## [1] "borrower_rate ~ number_of_days + loan_status2 + amount_funded + listing_term36 + listing_term60 + employment_status_descriptionNot employed + amount_delinquent + current_credit_lines + open_credit_lines + delinquencies_over30_days + prosper_rating + scorex"
    
    
    model3 = lm(borrower_rate ~ number_of_days + loan_status + amount_funded + listing_term + dti_wprosper_loan + employment_status_description + amount_delinquent + current_credit_lines + open_credit_lines + delinquencies_over30_days + prosper_rating + scorex, listings_train)
    sm3 = summary(model3)
    sm3
    
    
    ## 
    ## Call:
    ## lm(formula = borrower_rate ~ number_of_days + loan_status + amount_funded + 
    ##     listing_term + dti_wprosper_loan + employment_status_description + 
    ##     amount_delinquent + current_credit_lines + open_credit_lines + 
    ##     delinquencies_over30_days + prosper_rating + scorex, data = listings_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6433 -0.9770  0.0028  1.0036  5.8683 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error  t value
    ## (Intercept)                                 2.790e+01  1.367e-01  204.156
    ## number_of_days                              3.545e-03  7.523e-05   47.124
    ## loan_status2                                1.047e+00  3.225e-02   32.458
    ## amount_funded                              -2.186e-05  1.557e-06  -14.040
    ## listing_term36                              4.324e+00  1.063e-01   40.661
    ## listing_term60                              4.929e+00  1.068e-01   46.150
    ## dti_wprosper_loan                           1.630e-03  2.437e-03    0.669
    ## employment_status_descriptionFull-time      1.101e-01  8.219e-02    1.339
    ## employment_status_descriptionNot employed   6.229e-01  2.277e-01    2.736
    ## employment_status_descriptionOther          5.546e-02  4.035e-02    1.375
    ## employment_status_descriptionPart-time      1.424e-01  8.052e-01    0.177
    ## employment_status_descriptionRetired       -5.431e-01  3.386e-01   -1.604
    ## employment_status_descriptionSelf-employed -3.422e-03  4.036e-02   -0.085
    ## amount_delinquent                           2.790e-06  1.075e-06    2.597
    ## current_credit_lines                        1.678e-02  6.643e-03    2.526
    ## open_credit_lines                          -2.174e-02  7.100e-03   -3.062
    ## delinquencies_over30_days                   6.179e-03  1.413e-03    4.372
    ## prosper_rating                             -3.911e+00  7.762e-03 -503.793
    ## scorex                                     -5.759e-02  4.462e-03  -12.908
    ##                                            Pr(>|t|)    
    ## (Intercept)                                 < 2e-16 ***
    ## number_of_days                              < 2e-16 ***
    ## loan_status2                                < 2e-16 ***
    ## amount_funded                               < 2e-16 ***
    ## listing_term36                              < 2e-16 ***
    ## listing_term60                              < 2e-16 ***
    ## dti_wprosper_loan                           0.50340    
    ## employment_status_descriptionFull-time      0.18047    
    ## employment_status_descriptionNot employed   0.00622 ** 
    ## employment_status_descriptionOther          0.16927    
    ## employment_status_descriptionPart-time      0.85959    
    ## employment_status_descriptionRetired        0.10871    
    ## employment_status_descriptionSelf-employed  0.93243    
    ## amount_delinquent                           0.00942 ** 
    ## current_credit_lines                        0.01156 *  
    ## open_credit_lines                           0.00220 ** 
    ## delinquencies_over30_days                  1.24e-05 ***
    ## prosper_rating                              < 2e-16 ***
    ## scorex                                      < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.394 on 25137 degrees of freedom
    ## Multiple R-squared:  0.9475, Adjusted R-squared:  0.9474 
    ## F-statistic: 2.519e+04 on 18 and 25137 DF,  p-value: < 2.2e-16

The MSE is

    
    
    mse3 = mean(sm3$residuals^2)
    cat(mse3)
    
    
    ## 1.9422

The accuracy is

    
    
    listings_cp$predicted = predict(model3, newdata = listings_test)
    accuracy = listings_cp$predicted/listings_cp$borrower_rate*100
    print('model 3')
    
    
    ## [1] "model 3"
    
    
    for (interval_len in c(5,10,15) ){
      cat("for interval of", 100-interval_len,"% ~", 100+interval_len, "%: ")
      cat(length(accuracy[accuracy>(100-interval_len)& accuracy<(100+interval_len)])/length(accuracy), "\r\n")
    }
    
    
    ## for interval of 95 % ~ 105 %: 0.4478893 
    ## for interval of 90 % ~ 110 %: 0.7533985 
    ## for interval of 85 % ~ 115 %: 0.9285714 

which is worse than the second one. So here we reached an intermediate
conclusion, it seems that the initial model with all the data included is the
best one.

### 2.3.4 Model 4

We need to adopt another strategy. Based on the result from model3, we
examined the VIF and p-test of the model 3, and we found that
`current_credit_lines` and `open_credit_lines` have significant VIF (larger
than 10), which means both predictors are highly correlated

    
    
    # Model 4, refined based on vif results
    # summary(model3)
    # plot(x=model3, which =2)
    
    bptest(model3)
    
    
    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  model3
    ## BP = 1748.3, df = 18, p-value < 2.2e-16
    
    
    vif(model3)
    
    
    ##                                    GVIF Df GVIF^(1/(2*Df))
    ## number_of_days                 1.530063  1        1.236957
    ## loan_status                    1.518263  1        1.232178
    ## amount_funded                  1.353883  1        1.163565
    ## listing_term                   1.118979  2        1.028503
    ## dti_wprosper_loan              1.004584  1        1.002289
    ## employment_status_description  1.060685  6        1.004922
    ## amount_delinquent              1.013306  1        1.006631
    ## current_credit_lines          14.676317  1        3.830968
    ## open_credit_lines             14.729284  1        3.837875
    ## delinquencies_over30_days      1.079357  1        1.038921
    ## prosper_rating                 1.668545  1        1.291722
    ## scorex                         1.525685  1        1.235186

We dropped these variables, which leads to model 4.

    
    
    model4 = lm(borrower_rate ~ number_of_days + loan_status + amount_funded + listing_term + dti_wprosper_loan + employment_status_description + amount_delinquent  + delinquencies_over30_days + prosper_rating + scorex, listings_train)
    sm4 = summary(model4)

The MSE of model 4 is

    
    
    mse4 = mean(sm4$residuals^2)
    cat(mse4)
    
    
    ## 1.943119

And the accuracy of model 4 is

    
    
    listings_cp$predicted = predict(model4, newdata = listings_test)
    accuracy = listings_cp$predicted/listings_cp$borrower_rate*100
    print('model 4')
    
    
    ## [1] "model 4"
    
    
    for (interval_len in c(5,10,15) ){
      cat("for interval of", 100-interval_len,"% ~", 100+interval_len, "%: ")
      cat(length(accuracy[accuracy>(100-interval_len)& accuracy<(100+interval_len)])/length(accuracy), "\r\n")
    }
    
    
    ## for interval of 95 % ~ 105 %: 0.449201 
    ## for interval of 90 % ~ 110 %: 0.7542332 
    ## for interval of 85 % ~ 115 %: 0.9283329 

which is almost the same as model 3.

### 2.3.5 Model 5

Based on our previous result, we found all of model2, model3, model4 do not
perform so good as Model1. So based on Model1, we let the `R` language decide
which column to drop by running the `step()` function with a backward keyward.
To save time, we will only demonstrate the final result instead of showing the
whole output of step function. The refined formula is shown as before.

    
    
    # step(object = model1, direction = "backward")
    formula_refined = borrower_rate ~ number_of_days + loan_status + amount_funded + 
        listing_term + prosper_score + income_range + income_verifiable + 
        dti_wprosper_loan + employment_status_description + lender_indicator + 
        monthly_debt + public_records_last12_months + amount_delinquent + 
        current_credit_lines + open_credit_lines + total_open_revolving_accounts + 
        revolving_available_percent + total_trade_items + now_delinquent_derog + 
        was_delinquent_derog + delinquencies_over30_days + is_homeowner + 
        prosper_rating + scorex
    model5 = lm(formula = formula_refined, data = listings_train)
    sm5 = summary(model5)
    sm5
    
    
    ## 
    ## Call:
    ## lm(formula = formula_refined, data = listings_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0676 -0.9420  0.0013  0.9392  6.3324 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error  t value
    ## (Intercept)                                 2.931e+01  1.628e+00   18.007
    ## number_of_days                              3.251e-03  7.264e-05   44.760
    ## loan_status2                                9.390e-01  3.083e-02   30.457
    ## amount_funded                              -1.250e-05  1.652e-06   -7.567
    ## listing_term36                              4.316e+00  1.011e-01   42.706
    ## listing_term60                              5.104e+00  1.016e-01   50.226
    ## prosper_score2                              3.351e-01  1.323e+00    0.253
    ## prosper_score3                             -3.023e-01  1.323e+00   -0.228
    ## prosper_score4                             -1.039e+00  1.323e+00   -0.785
    ## prosper_score5                             -1.084e+00  1.324e+00   -0.819
    ## prosper_score6                             -1.462e+00  1.324e+00   -1.104
    ## prosper_score7                             -1.762e+00  1.324e+00   -1.331
    ## prosper_score8                             -1.865e+00  1.324e+00   -1.409
    ## prosper_score9                             -1.893e+00  1.324e+00   -1.430
    ## prosper_score10                            -2.144e+00  1.324e+00   -1.619
    ## prosper_score11                            -2.133e+00  1.325e+00   -1.610
    ## income_range2                              -1.574e+00  9.376e-01   -1.678
    ## income_range3                              -1.700e+00  9.369e-01   -1.814
    ## income_range4                              -1.653e+00  9.370e-01   -1.765
    ## income_range5                              -1.644e+00  9.371e-01   -1.755
    ## income_range6                              -1.570e+00  9.373e-01   -1.675
    ## income_range7                              -6.078e-01  1.002e+00   -0.607
    ## income_verifiable                           1.376e-01  6.576e-02    2.092
    ## dti_wprosper_loan                           2.439e-03  2.319e-03    1.052
    ## employment_status_descriptionFull-time      1.939e-01  7.883e-02    2.460
    ## employment_status_descriptionNot employed   3.109e-01  4.068e-01    0.764
    ## employment_status_descriptionOther          4.842e-02  3.931e-02    1.232
    ## employment_status_descriptionPart-time      3.912e-01  7.646e-01    0.512
    ## employment_status_descriptionRetired       -5.040e-01  3.220e-01   -1.565
    ## employment_status_descriptionSelf-employed -2.025e-01  7.174e-02   -2.824
    ## lender_indicator1                           2.768e-01  4.043e-02    6.846
    ## monthly_debt                                1.412e-05  1.425e-05    0.991
    ## public_records_last12_months                8.868e-02  5.483e-02    1.617
    ## amount_delinquent                           2.004e-06  1.065e-06    1.882
    ## current_credit_lines                        1.644e-02  6.644e-03    2.474
    ## open_credit_lines                          -2.937e-02  7.405e-03   -3.966
    ## total_open_revolving_accounts               1.824e-02  3.834e-03    4.757
    ## revolving_available_percent                -1.244e-03  4.325e-04   -2.876
    ## total_trade_items                          -4.544e-03  9.803e-04   -4.635
    ## now_delinquent_derog                        5.663e-02  8.331e-03    6.797
    ## was_delinquent_derog                        1.670e-02  3.956e-03    4.220
    ## delinquencies_over30_days                   2.163e-03  1.860e-03    1.163
    ## is_homeownerTRUE                           -6.029e-02  2.024e-02   -2.979
    ## prosper_rating                             -3.527e+00  1.075e-02 -328.154
    ## scorex                                     -8.189e-02  5.391e-03  -15.189
    ##                                            Pr(>|t|)    
    ## (Intercept)                                 < 2e-16 ***
    ## number_of_days                              < 2e-16 ***
    ## loan_status2                                < 2e-16 ***
    ## amount_funded                              3.96e-14 ***
    ## listing_term36                              < 2e-16 ***
    ## listing_term60                              < 2e-16 ***
    ## prosper_score2                              0.80008    
    ## prosper_score3                              0.81932    
    ## prosper_score4                              0.43234    
    ## prosper_score5                              0.41268    
    ## prosper_score6                              0.26944    
    ## prosper_score7                              0.18305    
    ## prosper_score8                              0.15896    
    ## prosper_score9                              0.15276    
    ## prosper_score10                             0.10541    
    ## prosper_score11                             0.10743    
    ## income_range2                               0.09331 .  
    ## income_range3                               0.06964 .  
    ## income_range4                               0.07766 .  
    ## income_range5                               0.07935 .  
    ## income_range6                               0.09399 .  
    ## income_range7                               0.54394    
    ## income_verifiable                           0.03643 *  
    ## dti_wprosper_loan                           0.29293    
    ## employment_status_descriptionFull-time      0.01390 *  
    ## employment_status_descriptionNot employed   0.44472    
    ## employment_status_descriptionOther          0.21809    
    ## employment_status_descriptionPart-time      0.60889    
    ## employment_status_descriptionRetired        0.11750    
    ## employment_status_descriptionSelf-employed  0.00475 ** 
    ## lender_indicator1                          7.76e-12 ***
    ## monthly_debt                                0.32178    
    ## public_records_last12_months                0.10582    
    ## amount_delinquent                           0.05986 .  
    ## current_credit_lines                        0.01336 *  
    ## open_credit_lines                          7.34e-05 ***
    ## total_open_revolving_accounts              1.98e-06 ***
    ## revolving_available_percent                 0.00403 ** 
    ## total_trade_items                          3.59e-06 ***
    ## now_delinquent_derog                       1.09e-11 ***
    ## was_delinquent_derog                       2.45e-05 ***
    ## delinquencies_over30_days                   0.24479    
    ## is_homeownerTRUE                            0.00289 ** 
    ## prosper_rating                              < 2e-16 ***
    ## scorex                                      < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.322 on 25111 degrees of freedom
    ## Multiple R-squared:  0.9528, Adjusted R-squared:  0.9527 
    ## F-statistic: 1.152e+04 on 44 and 25111 DF,  p-value: < 2.2e-16

The MSE is

    
    
    mse5 = mean(sm5$residuals^2)
    cat(mse5)
    
    
    ## 1.745768

And the accuracy of model 5 is

    
    
    listings_cp$predicted = predict(model5, newdata = listings_test)
    accuracy = listings_cp$predicted/listings_cp$borrower_rate*100
    print('model 5')
    
    
    ## [1] "model 5"
    
    
    for (interval_len in c(5,10,15) ){
      cat("for interval of", 100-interval_len,"% ~", 100+interval_len, "%: ")
      cat(length(accuracy[accuracy>(100-interval_len)& accuracy<(100+interval_len)])/length(accuracy), "\r\n")
    }
    
    
    ## for interval of 95 % ~ 105 %: 0.4724541 
    ## for interval of 90 % ~ 110 %: 0.7931076 
    ## for interval of 85 % ~ 115 %: 0.9466969 

which yields a better accuracy rate than the one from Model 1.

## 2.4 Conclusion of Problem 1

So far, we could conclude that model5, given its highest accuracy, is the best
model to predict the interest rate of the borrowers. However, model 5 includes
factors that have high p-value in its prediction. Given the prompt of the
question, the model is tasked to find the factors that have significant
relaitonship with determinating the interest rate for borrowers. Model 3, in
this case, is the one that containing only the 12 significant factors for such
prediction.

In the summary of Model 3, number_of_days, loan_status, listing_terms,
dti_wprosper_loan, amount_delinquent, current_credit_lines and
delinquencies_over30_days will have a significant and positive effect on the
borrower’s interest rate. That being said,if a borrower scores high in the
varaible mentioned above, he or she will get a higher interest rate. Because
the higher a borrower scores in these categories, the more credit risks are
associated with this person, leading to a higher interest rate. For example,
the factor number_of_days has a positive estimate of 3.583e-03, meaning if the
number of days for a particular loan increases by one, the specific interest
rate for this loan will increase by 3.583e-03. For the factor listing term,
the default value is 12. According to the summary of Model 3, if the listing
term is 36 months, the interest rate will increase by 4.400. If the listing
term is 60 months, the interest rate will increasing by 5.016.

Among all the levels of the employment status besides the default value
Employed, only the employment_status_descriptionNot employed factor is
significant when determining the interest rate. The factor
employment_status_descriptionNot employed has a value of 5.246e-01, meaning if
the candidate is not employed, his or her interest rate for a particular loan
will increase by 5.246e-01.This makes sense becuse lending money to an
unemployed person is typically riskier compared with an employed person.

On the other side, factors of amount_funded, open_credit_lines, prosper_rating
and scorex have a significantly negative effect when determining interest rate
for loans.In another word, these factors are negatively correlated with
interest rate. Because the higher a borrower scores in these categories, the
less credit risks are associated with this person, leading to a higherlower
interest rate, the less credit risks are associated with a particular
borrower, leading to a lower interest rate. For example, the variable
amount_funded has a negative value of -2.071e-05, meaning if the amount funded
for the loan increases by $1, the specific interest rate for this loan will
decrease by -2.071e-05. This negative correlaiton makes sense because in order
to increase the overall profit, the issuer has the incentive to set a lower
interest rate for a borrower with low risk.

# 3\. Problem 2. Loan Default

## 3.1 Initialization:

To maintain the data completency, we apply the same strategy of intialize the
data as what we did before.

    
    
    library(caret)
    
    
    ## Loading required package: lattice
    
    
    ## Loading required package: ggplot2
    
    
    library(e1071)
    listings <- read.csv("ProjectA_Listings2013.csv")

## 3.2 Data Cleaning

We apply the same strategy to clean the data as question 2. You can ignore
this part since it’s identical to what we have before.

For loan status, according to our definition, both charge-off and defaulted
means defaulted, so we re-classify them into two sets based on this standard.

    
    
    listings$loan_status <- as.factor(listings$loan_status)
    # class(listings$loan_status)
    # levels(listings$loan_status)
    ## 1 means completed, and 2 means defaulted
    levels(listings$loan_status) <- c("1", "2", "2", "1")

Redefine listing_term in terms of year for better visualization

    
    
    levels(listings$listing_term) <- c("1", "3", "5")
    # levels(listings$listing_term)

We factorize some categorized data as below.

    
    
    listings$listing_term <- as.factor(listings$listing_term)
    listings$prosper_score <- as.factor(listings$prosper_score)
    listings$income_range <- as.factor(listings$income_range)
    listings$prosper_score <- as.factor(listings$prosper_score)
    listings$lender_indicator <- as.factor(listings$lender_indicator)
    listings$employment_status_description <- as.factor(listings$employment_status_description)
    listings$income_verifiable <- as.factor(listings$income_verifiable)
    listings$income_range <- as.factor(listings$income_range)

Instead of factorizing, we decided to change prosper_rating and scorex into
numeric values for better understanding of the score reflected by these two
columns. To be specific, we use 1 to 7 and 1 to 11 respectively, where 1 means
the worst and vice versa.

    
    
    ## change categorical varibales into numeric value in order to better capture the relationship between prospe_rating and interest rate
    listings$prosper_rating_id <- 0
    listings$prosper_rating_id[listings$prosper_rating == "AA"] <- 7
    listings$prosper_rating_id[listings$prosper_rating == "A"] <- 6
    listings$prosper_rating_id[listings$prosper_rating == "B"] <- 5
    listings$prosper_rating_id[listings$prosper_rating == "C"] <- 4
    listings$prosper_rating_id[listings$prosper_rating == "D"] <- 3
    listings$prosper_rating_id[listings$prosper_rating == "E"] <- 2
    listings$prosper_rating_id[listings$prosper_rating == "HR"] <- 1
    listings$prosper_rating <- NULL
    colnames(listings)[colnames(listings)=="prosper_rating_id"] <- "prosper_rating"
    ## Process scorex
    listings$scorex_id <- 0
    listings$scorex_id[listings$scorex == "< 600"] <- 1
    listings$scorex_id[listings$scorex == "600-619"] <- 2
    listings$scorex_id[listings$scorex == "620-639"] <- 3
    listings$scorex_id[listings$scorex == "640-649"] <- 4
    listings$scorex_id[listings$scorex == "650-664"] <- 5
    listings$scorex_id[listings$scorex == "665-689"] <- 6
    listings$scorex_id[listings$scorex == "690-701"] <- 7
    listings$scorex_id[listings$scorex == "702-723"] <- 8
    listings$scorex_id[listings$scorex == "724-747"] <- 9
    listings$scorex_id[listings$scorex == "748-777"] <- 10
    listings$scorex_id[listings$scorex == "778+"] <- 11
    listings$scorex <- NULL
    colnames(listings)[which(colnames(listings)=="scorex_id")] <- "scorex"

We drop the following columns based on our discussion. These lines are either
too fragmented, or related to other columns, or not related to the question
since the event recorded by the column is later than the timestamp when
interest rate is generated.

    
    
    listings$loan_origination_date <- NULL
    # levels(listings$income_range)
    listings$occupation <- NULL
    # Process prosper_rating
    # unique(listings$prosper_rating)
    listings$borrower_city <- NULL
    listings$borrower_state <- NULL
    listings$current_delinquencies <- NULL
    listings$first_recorded_credit_line <- NULL
    listings$credit_lines_last7_years <- NULL
    listings$inquiries_last6_months <- NULL
    listings$credit_lines_notontime <- NULL
    listings$listing_monthly_payment <- NULL
    listings$listing_category_id <- NULL
    listings$stated_monthly_income <- NULL
    listings$loan_status_description<- NULL

Convert boolean into numeric value.

    
    
    listings$income_verifiable <-  ifelse(listings$income_verifiable == "TRUE",1,0)

### 3.2.0 Unreasonable Variables

Deal with super big dti ratio. The ratio of 1e+06 means the person has no
income, which could also be interpreted as all of the persons income is used
to account for the debt. So we replace 1e+06 as 1.

    
    
    listings$dti_wprosper_loan <- ifelse(listings$dti_wprosper_loan == 1e+06, 1, listings$dti_wprosper_loan)

Deal with months employed less than 0. May be error data, we replace them with
mean values.

    
    
    listings$months_employed <- ifelse(listings$months_employed<0, mean(listings$months_employed), listings$months_employed)

Deal with missing value: We use the following code to check missing value

    
    
    # Check missing value for whole dataframe, found missing values
    # is.na(listings$months_employed) 
    # Check 
    # for (i in colnames(listings)) {
    #   print(i)
    #   print(mean(is.na(listings[i])))
    # }

which tells us both `months_employed` and `installment_balance` contains
missing values. Since the scale of the missing value is small, we choose to
interpolate the missing value by mean value of the column instead of dropping
rows or column.

    
    
    listings$months_employed <- ifelse(is.na(listings$months_employed), mean(listings$months_employed, na.rm = TRUE), listings$months_employed)
    listings$installment_balance <- ifelse(is.na(listings$installment_balance), mean(listings$installment_balance, na.rm = TRUE), listings$installment_balance)

## 3.3 Model setup

In the second regression model, our goal is to eliminate negative false, since
negative false is worse than negative true, considering negative true will
only lead to loss of interest from buisness, while not accurately forcasing a
potential defualt will lead to great money loss. We carry out our model
refining work based on this idea.

### 2.3.0 Train-test split

We split train data and test data as below with 75% of all data to be train,
25% to be test.

    
    
    smp_size = floor(0.75*nrow(listings))
    train_ind = sample(seq_len(nrow(listings)),size = smp_size)
    listings_train = listings[train_ind,]
    listings_test = listings[-train_ind,]

### 3.3.1 Model 1

We base this model on Model 3 from question 1.

    
    
    logit.model1 <- glm(loan_status~ number_of_days +  amount_funded + listing_term + dti_wprosper_loan + employment_status_description + amount_delinquent + current_credit_lines + open_credit_lines + delinquencies_over30_days + prosper_rating + scorex, listings_train,family = "binomial")
    sm1 = summary(logit.model1)
    sm1
    
    
    ## 
    ## Call:
    ## glm(formula = loan_status ~ number_of_days + amount_funded + 
    ##     listing_term + dti_wprosper_loan + employment_status_description + 
    ##     amount_delinquent + current_credit_lines + open_credit_lines + 
    ##     delinquencies_over30_days + prosper_rating + scorex, family = "binomial", 
    ##     data = listings_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5139  -0.4418  -0.2624  -0.1283   3.6039  
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                 1.256e+01  5.176e-01  24.267
    ## number_of_days                             -1.384e-02  2.685e-04 -51.555
    ## amount_funded                              -1.246e-05  4.662e-06  -2.673
    ## listing_term36                             -1.964e+00  4.266e-01  -4.603
    ## listing_term60                             -1.585e+00  4.274e-01  -3.709
    ## dti_wprosper_loan                           5.070e-03  3.023e-03   1.677
    ## employment_status_descriptionFull-time      9.832e-01  2.480e-01   3.964
    ## employment_status_descriptionNot employed  -4.172e-01  8.549e-01  -0.488
    ## employment_status_descriptionOther          2.091e-01  1.021e-01   2.047
    ## employment_status_descriptionPart-time      9.601e-01  1.281e+00   0.749
    ## employment_status_descriptionRetired        1.024e+00  9.138e-01   1.121
    ## employment_status_descriptionSelf-employed -3.257e-01  1.183e-01  -2.752
    ## amount_delinquent                           4.622e-06  2.681e-06   1.724
    ## current_credit_lines                        1.411e-02  1.884e-02   0.749
    ## open_credit_lines                          -1.129e-02  2.010e-02  -0.562
    ## delinquencies_over30_days                  -2.935e-03  4.089e-03  -0.718
    ## prosper_rating                             -3.323e-01  2.182e-02 -15.232
    ## scorex                                      2.530e-02  1.154e-02   2.193
    ##                                            Pr(>|z|)    
    ## (Intercept)                                 < 2e-16 ***
    ## number_of_days                              < 2e-16 ***
    ## amount_funded                              0.007515 ** 
    ## listing_term36                             4.16e-06 ***
    ## listing_term60                             0.000208 ***
    ## dti_wprosper_loan                          0.093498 .  
    ## employment_status_descriptionFull-time     7.36e-05 ***
    ## employment_status_descriptionNot employed  0.625532    
    ## employment_status_descriptionOther         0.040662 *  
    ## employment_status_descriptionPart-time     0.453610    
    ## employment_status_descriptionRetired       0.262408    
    ## employment_status_descriptionSelf-employed 0.005919 ** 
    ## amount_delinquent                          0.084746 .  
    ## current_credit_lines                       0.453787    
    ## open_credit_lines                          0.574364    
    ## delinquencies_over30_days                  0.472945    
    ## prosper_rating                              < 2e-16 ***
    ## scorex                                     0.028332 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 19244  on 25155  degrees of freedom
    ## Residual deviance: 11646  on 25138  degrees of freedom
    ## AIC: 11682
    ## 
    ## Number of Fisher Scoring iterations: 6

Then we analyze the accuracy with the cross table.

    
    
    predictions <- predict(logit.model1, newdata = listings_test, type = "response")
    levels(listings_test$loan_status) <- c("Yes","No")
    listings_test$loan_status <- as.factor(listings_test$loan_status)
    predictions <- ifelse(predictions >=0.5,2,1)
    predictions <- as.factor(predictions)
    levels(predictions)
    
    
    ## [1] "1" "2"
    
    
    levels(predictions) <- c("Yes","No")
    confusionMatrix(listings_test$loan_status,predictions)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  Yes   No
    ##        Yes 7284    4
    ##        No   525  573
    ##                                          
    ##                Accuracy : 0.9369         
    ##                  95% CI : (0.9315, 0.942)
    ##     No Information Rate : 0.9312         
    ##     P-Value [Acc > NIR] : 0.01923        
    ##                                          
    ##                   Kappa : 0.6529         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2e-16        
    ##                                          
    ##             Sensitivity : 0.9328         
    ##             Specificity : 0.9931         
    ##          Pos Pred Value : 0.9995         
    ##          Neg Pred Value : 0.5219         
    ##              Prevalence : 0.9312         
    ##          Detection Rate : 0.8686         
    ##    Detection Prevalence : 0.8691         
    ##       Balanced Accuracy : 0.9629         
    ##                                          
    ##        'Positive' Class : Yes            
    ## 

### 3.3.2 Model 2

Model 2 is derived from manually selecting all the significant feature with
`P-value < 0.05` from a model with all the features, repeating this process,
until we reach a model with all the feature to be significant.

    
    
    logit.model2 <- glm(loan_status~ number_of_days +principal_balance+amount_funded+borrower_rate+listing_term+income_verifiable+employment_status_description+lender_indicator+real_estate_balance+prosper_rating+scorex, listings_train,family = "binomial")
    summary(logit.model2)
    
    
    ## 
    ## Call:
    ## glm(formula = loan_status ~ number_of_days + principal_balance + 
    ##     amount_funded + borrower_rate + listing_term + income_verifiable + 
    ##     employment_status_description + lender_indicator + real_estate_balance + 
    ##     prosper_rating + scorex, family = "binomial", data = listings_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7520  -0.4044  -0.2621  -0.1268   3.7342  
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                 1.739e+00  7.423e-01   2.342
    ## number_of_days                             -1.422e-02  3.238e-04 -43.914
    ## principal_balance                           8.871e-05  9.078e-06   9.772
    ## amount_funded                              -3.181e-05  6.331e-06  -5.025
    ## borrower_rate                               3.827e-01  1.860e-02  20.579
    ## listing_term36                             -3.579e+00  4.380e-01  -8.171
    ## listing_term60                             -3.580e+00  4.391e-01  -8.155
    ## income_verifiable                          -7.891e-01  1.960e-01  -4.025
    ## employment_status_descriptionFull-time      4.797e-01  2.627e-01   1.826
    ## employment_status_descriptionNot employed  -1.611e+00  9.077e-01  -1.775
    ## employment_status_descriptionOther          9.791e-02  1.055e-01   0.928
    ## employment_status_descriptionPart-time      1.667e-01  1.771e+00   0.094
    ## employment_status_descriptionRetired        5.930e-01  9.762e-01   0.607
    ## employment_status_descriptionSelf-employed -1.128e+00  2.258e-01  -4.997
    ## lender_indicator1                           4.920e-01  1.270e-01   3.874
    ## real_estate_balance                        -7.335e-07  2.068e-07  -3.547
    ## prosper_rating                              1.179e+00  7.649e-02  15.415
    ## scorex                                      5.896e-02  1.296e-02   4.548
    ##                                            Pr(>|z|)    
    ## (Intercept)                                0.019174 *  
    ## number_of_days                              < 2e-16 ***
    ## principal_balance                           < 2e-16 ***
    ## amount_funded                              5.03e-07 ***
    ## borrower_rate                               < 2e-16 ***
    ## listing_term36                             3.05e-16 ***
    ## listing_term60                             3.49e-16 ***
    ## income_verifiable                          5.69e-05 ***
    ## employment_status_descriptionFull-time     0.067783 .  
    ## employment_status_descriptionNot employed  0.075958 .  
    ## employment_status_descriptionOther         0.353389    
    ## employment_status_descriptionPart-time     0.925023    
    ## employment_status_descriptionRetired       0.543521    
    ## employment_status_descriptionSelf-employed 5.83e-07 ***
    ## lender_indicator1                          0.000107 ***
    ## real_estate_balance                        0.000389 ***
    ## prosper_rating                              < 2e-16 ***
    ## scorex                                     5.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 19244  on 25155  degrees of freedom
    ## Residual deviance: 11005  on 25138  degrees of freedom
    ## AIC: 11041
    ## 
    ## Number of Fisher Scoring iterations: 6
    
    
    predictions2 <- predict(logit.model2, newdata = listings_test, type = "response")
    library(caret)
    library(e1071)
    
    levels(listings_test$loan_status) <- c("Yes","No")
    
    listings_test$loan_status <- as.factor(listings_test$loan_status)
    predictions2 <- ifelse(predictions2 >= 0.5,2,1)
    predictions2 <- as.factor(predictions2)
    levels(predictions2)
    
    
    ## [1] "1" "2"
    
    
    levels(predictions2) <- c("Yes","No")
    
    confusionMatrix(listings_test$loan_status,predictions2)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  Yes   No
    ##        Yes 7281    7
    ##        No   493  605
    ##                                           
    ##                Accuracy : 0.9404          
    ##                  95% CI : (0.9351, 0.9454)
    ##     No Information Rate : 0.927           
    ##     P-Value [Acc > NIR] : 7.277e-07       
    ##                                           
    ##                   Kappa : 0.6774          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.9366          
    ##             Specificity : 0.9886          
    ##          Pos Pred Value : 0.9990          
    ##          Neg Pred Value : 0.5510          
    ##              Prevalence : 0.9270          
    ##          Detection Rate : 0.8682          
    ##    Detection Prevalence : 0.8691          
    ##       Balanced Accuracy : 0.9626          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

### 3.1.3 Model 3

Model 3 is derived from a model with all the features, with the step function,
we get the following result. For simplicity of the results, we only retain the
final step results.

    
    
    logit.model_all <- glm(loan_status~., listings_train, family = "binomial")
    #step(logit.model_all)
    logit.model3 <- glm(formula = loan_status ~ number_of_days + principal_balance + 
        amount_funded + borrower_rate + listing_term + prosper_score + 
        income_range + income_verifiable + employment_status_description + 
        months_employed + lender_indicator + bankcard_utilization + 
        real_estate_balance + real_estate_payment + total_trade_items + 
        was_delinquent_derog + delinquencies_over30_days + prosper_rating + 
        scorex , family = "binomial", data = listings_train)

Then the accuracy is

    
    
    predictions3 <- predict(logit.model3, newdata = listings_test, type = "response")
    levels(listings_test$loan_status) <- c("Yes","No")
    listings_test$loan_status <- as.factor(listings_test$loan_status)
    predictions3 <- ifelse(predictions3 >=0.5,2,1)
    predictions3 <- as.factor(predictions3)
    levels(predictions3)
    
    
    ## [1] "1" "2"
    
    
    levels(predictions3) <- c("Yes","No")
    confusionMatrix(listings_test$loan_status,predictions3)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  Yes   No
    ##        Yes 7280    8
    ##        No   480  618
    ##                                           
    ##                Accuracy : 0.9418          
    ##                  95% CI : (0.9366, 0.9467)
    ##     No Information Rate : 0.9254          
    ##     P-Value [Acc > NIR] : 1.613e-09       
    ##                                           
    ##                   Kappa : 0.6872          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.9381          
    ##             Specificity : 0.9872          
    ##          Pos Pred Value : 0.9989          
    ##          Neg Pred Value : 0.5628          
    ##              Prevalence : 0.9254          
    ##          Detection Rate : 0.8681          
    ##    Detection Prevalence : 0.8691          
    ##       Balanced Accuracy : 0.9627          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

### 3.3.4 Model 4

Model 4 is based on model 3, by adding several more features.

    
    
    logit.model_all <- glm(loan_status~., listings_train, family = "binomial")
    #step(logit.model_all)
    logit.model4 <- glm(formula = loan_status ~ number_of_days + principal_balance + 
        amount_funded + borrower_rate + listing_term + prosper_score + 
        income_range + income_verifiable + employment_status_description + 
        months_employed + lender_indicator + bankcard_utilization + 
        real_estate_balance + real_estate_payment + total_trade_items + 
        was_delinquent_derog + delinquencies_over30_days + prosper_rating + 
        scorex+ amount_funded*income_range + principal_balance*income_range + principal_balance*listing_term +  real_estate_balance * real_estate_payment , family = "binomial", data = listings_train)
    
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Then the accuracy is

    
    
    predictions4 <- predict(logit.model4, newdata = listings_test, type = "response")
    levels(listings_test$loan_status) <- c("Yes","No")
    listings_test$loan_status <- as.factor(listings_test$loan_status)
    predictions4 <- ifelse(predictions4 >=0.5,2,1)
    predictions4 <- as.factor(predictions4)
    levels(predictions4)
    
    
    ## [1] "1" "2"
    
    
    levels(predictions4) <- c("Yes","No")
    confusionMatrix(listings_test$loan_status,predictions4)
    
    
    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  Yes   No
    ##        Yes 7277   11
    ##        No   454  644
    ##                                           
    ##                Accuracy : 0.9446          
    ##                  95% CI : (0.9394, 0.9494)
    ##     No Information Rate : 0.9219          
    ##     P-Value [Acc > NIR] : 2.832e-16       
    ##                                           
    ##                   Kappa : 0.706           
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.9413          
    ##             Specificity : 0.9832          
    ##          Pos Pred Value : 0.9985          
    ##          Neg Pred Value : 0.5865          
    ##              Prevalence : 0.9219          
    ##          Detection Rate : 0.8678          
    ##    Detection Prevalence : 0.8691          
    ##       Balanced Accuracy : 0.9622          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

## 3.4 Conclusion for Problem 2

In Model 1, we used all the variables that are significant in 2.3.3 (problem
1) to determine factors that lead to default. The accuracy of this model is
0.9356. However, notice that there are 538 predictions of default that we will
miss if this model is adopted, leading to potential loss.

In Model 2, we added factors including
principal_balance,borrower_rate,income_verifiable,lender_indicator and
real_estate_balance to predict odds of defaults. We found that are
prosper_ratingAA ,scorex724-747 and lender_indicator1 are the most significant
variables that positively correlated with odds of defaults. That being said,if
a borrower scores high in the varaibles mentioned above, he or she will have a
higher chance of default. For example, the factor prosper_ratingAA has a
positive estimate of 7.964e-01, meaning if prosper_ratingAA for a particular
loan increases by one, the chance of default for this loan will increase by
7.964e-01. On the other hand, number_of_days, amount_funded, borrower_rate,
listing_term36, listing_term60, income_verifiable are the most significant
variables that negatively correlated with odds of defaults.Therefore, That
being said,if a borrower scores high in the varaible mentioned here, he or she
has a higher chance of default.

In Model 3, we used the step function to determine which factors are
signifiant. From the model we noticed that,
number_of_days,principal_balance,amount_funded, borrower_rate, listing_term,
prosper_score, income_range, income_verifiable, employment_status_description,
months_employed, lender_indicator, bankcard_utilization, real_estate_balance,
real_estate_payment, total_trade_items, was_delinquent_derog,
delinquencies_over30_days, prosper_rating, scorex are all the factors that are
significant to predicit the default rate of a person. The accuracy of the
model is 94.25%, and it is higher than the hand picked facotrs model,
therefore it means that the factors from the step function are will have
greater impact in the default rate. From the corsstables we see that false
negatives number is 469, and this is also lower than our hand selected models.
Therefore, we conclude that this model is more accurate than the previous
models in predicting the default rate.

In Model 4, in order to improve the perforamnce of the model by limiting the
false negatives and increasing the overall accuracy, we added interactions
between different factors into the model. We included interaction between
amount_funded & income_range, real_estate_balance & real_estate_payment,
principal_balance & income_range and principal_balance & listing term. The
interaction between factors can be interpreted intuitively. For example, the
interaction between principal_balance & income_range can be explained by
considering the difference in impact of the principal balance on people with
different income level. Principal balance of $5000 have limited impact on
people with income level of $100,000+ in conmpairon to people with income
level of $25000. Including the interactions we have discovered, model 4
decreases the false negatives from 469 to 438 and increase the overall
accuracy from 94.25% to 94.59%.

# 4 Conclusion

In this project, our team are tasked to identify potential opportunities in
the peer to peer lending space. we built a linear regression model to
determine how interest rate for loans is determined in the first place. After
that, we built a logistic model of what factors lead to a loan default.

From the models we built for different problems, we found factors that the
model in problem 1 showed as credit risks leading to compensatory adjustment
in interest rate.We found that there are 7 factors (see Common factors) that
determine both interest rate and default odds. However, not all those items
really cause defaults (see interest_rate). In addition, there are other items
that cause default but does not show in the models built for problem 1 (see
default_odds). The factors are shown as below:

Common factors: number_of_days, amount_funded, listing_term,
prosper_score,employment_status_description, lender_indicator, prosper_rating,
scorex

interst_rate: dti_wprosper_loan, amount_delinquent, current_credit_lines,
open_credit_lines, delinquencies_over30_days

default_odds:
principal_balance,borrower_rate,income_verifiable,lender_indicator,real_estate_balance

In conclusion, the factors that lead to high default odds and, are only
existing in the second model, will be the risk factors the market tend to
ignore, such as occupation, inquiries and bankcard utilization. On the other
hand, the factors that only reside in the first model might not be a proper
consideration for raising the interest rate as they may not cause the loan to
default.

To sum, we have learnt how to clean the data, how to apply linear regression
and logistic regression algorithms in R, and how to refine our existing models
with either `step` method from R, or manually selecting significant factors.
By combining all the steps together, we have successfully built totally 9
models to study the relations of interest rate and default rate. We did a good
job considering our model accuracy based on our prediction.

