
library(tidyverse)
library(gtsummary)
library(broom)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

## Basic Summary function ##

BasicSummary <- function(df, dgts = 3){
  
  ## ################################################################
  ## #
  ## # Create a basic summary of variables in the data frame df,
  ## # a data frame with one row for each column of df giving the
  ## # variable name, type, number of unique levels, the most
  ## # frequent level, its frequency and corresponding fraction of
  ## # records, the number of missing values and its corresponding
  ## # fraction of records
  ## #
  ## ################################################################
  
  m <- ncol(df)
  varNames <- colnames(df)
  varType <- vector("character",m)
  topLevel <- vector("character",m)
  topCount <- vector("numeric",m)
  missCount <- vector("numeric",m)
  levels <- vector("numeric", m)
  
  for (i in 1:m)
  {
    x <- df[,i]
    varType[i] <- class(x)
    xtab <- table(x, useNA = "ifany")
    levels[i] <- length(xtab)
    nums <- as.numeric(xtab)
    maxnum <- max(nums)
    topCount[i] <- maxnum
    maxIndex <- which.max(nums)
    lvls <- names(xtab)
    topLevel[i] <- lvls[maxIndex]
    missIndex <- which((is.na(x)) | (x == "") | (x == " "))
    missCount[i] <- length(missIndex)
  }
  
  n <- nrow(df)
  topFrac <- round(topCount/n, digits = dgts)
  missFrac <- round(missCount/n, digits = dgts)
  #
  
  summaryFrame <- data.frame(variable = varNames, type = varType,
                             levels = levels, topLevel = topLevel,
                             topCount = topCount, topFrac = topFrac,
                             missFreq = missCount, missFrac = missFrac)
  return(summaryFrame)
  
}


cervical <- read.csv("risk_factors_cervical_cancer.csv", na.strings = "?")


####  generate descriptive statistics table with N, median and IQR 
#### Only selected variables in the table:
cervical %>% tbl_summary(by=Num.of.pregnancies, include = c(Age, Number.of.sexual.partners, First.sexual.intercourse, Smokes, Smokes..years., IUD))


### After **STR**  **BASIC SUMMARY**  &  **tblsummary**,  we decide which variables will be used for a logistic model:

## AGE
## Number.of.sexual.partners
## First.sexual.intercourse
## Num.of.pregnancies
## Smokes..pack.year
## Hormonal.Contraceptives..year
## IUD..years
## STDs..number
## Dx.CIN
## Dx.HPV
## Dx  -this is our BINARY OUTCOME Variable!


cervical_selected_data <- cervical %>% select(Age, Number.of.sexual.partners, First.sexual.intercourse, Num.of.pregnancies,
                                            Smokes..packs.year., Hormonal.Contraceptives..years., IUD..years., STDs..number.,
                                            Dx.CIN, Dx.HPV, Dx)


### But.... we need to deal with the missing values
## 1./ for 'HORMONAL CONTRACEPTIVES...YEAR' and for 'IUD..years' we can inpute with Mean or Median.
## Both do not have a normal distribution (after plotting histogram)  --we used the MEDIAN then.

### cervicaldata <- cervicaldata %>% mutate(Hormonal.Contraceptives..years. = if(is.na(Hormonal.Contraceptives..years.), 
###                                                                                    median(Hormonal.Contraceptives..years.)))

cervical_selected_data <- cervical_selected_data %>%
  mutate(Years.Hormon.Contracep.imputed = if_else(is.na(Hormonal.Contraceptives..years.), 
                                                   median(Hormonal.Contraceptives..years., na.rm = TRUE),Hormonal.Contraceptives..years.))

cervical_selected_data <- cervical_selected_data %>% 
  mutate(IUD.Years.Imputed = if_else(is.na(IUD..years.), median(IUD..years., na.rm=TRUE),IUD..years.))


## 2./ for other variables with missing values, we are going to have to drop those that are not complete cases.

cervical_selected_data_no_na <- na.omit(cervical_selected_data)

## remove original HORMONE CONTRACEPTIVE and IUD YEARS variables

cervical_selected_data_no_na <- cervical_selected_data_no_na %>% select (!c(Hormonal.Contraceptives..years., IUD..years.))


###  1st
### remove outliers - **REHACER***   DEBE HACERSE SOLO CON LAS CONTINUOS/NUMERICAL VARIABLES!!! ###
###  1.1 Ver si hago normalization o standardization 
###  STANDARDIZATION- si hay NORMAL DISTRIBUTION.   Si no tengo que hacer NORMALIZATION.

## Esta es formula de STANDARDIZATION -  no de normalization  ****CAMBIARLA****  !!!!   ####
z_scores <- as.data.frame(sapply(cervical_selected_data_no_na, function(cervical_selected_data_no_na) 
  (abs(cervical_selected_data_no_na-mean(cervical_selected_data_no_na)) / sd(cervical_selected_data_no_na))))
z_scores_dframe <- z_scores[!rowSums(z_scores>2), ]


### find outliers ###
### Step Nr.1 use Z-scores for normally distributed data
cervical_selected_data_no_na <- cervical_selected_data_no_na %>% 
                                mutate (Age = abs (Age - mean(Age)) / sd (Age) )   %>%
                                mutate (First.sexual.intercourse = abs(First.sexual.intercourse - mean(First.sexual.intercourse) / sd(First.sexual.intercourse)))


Q1 <- quantile(cervical_prueba_outliers$Number.of.sexual.partners, .25)
Q3 <- quantile(cervical_prueba_outliers$Number.of.sexual.partners, .75)
IQR <- IQR(cervical_prueba_outliers$Number.of.sexual.partners)

IQR_Age <- IQR(cervical_selected_data_no_na$Age)

## 2n - Pasar A FACTOR A los DX variable ###




### Convert factor variables, to factor
## cervical_selected_data_no_na <- cervical_selected_data_no_na %>% mutate_at (c('Dx.CIN','Dx.HPV', 'Dx'), as.factor )


CC_logistic_model <- 
  glm(Dx ~ Age + Number.of.sexual.partners + First.sexual.intercourse + Num.of.pregnancies + Smokes..packs.year. + STDs..number. + Dx.CIN
      + Dx.HPV + Years.Hormon.Contracep.imputed + IUD.Years.Imputed ,
      data = cervical_selected_data_no_na, 
      family = binomial(link = 'logit'))
summary(CC_logistic_model)


## este no funciona ##
odds_ratio <- tidy(CC_logistic_model, exponentiate = TRUE, conf.int = TRUE)




### DA  '0' en todos los casos!
odds_ratios <- exp(coef(CC_logistic_model))


### hacer un boxplot para ver los outliers
ggplot(stack(cervical_selected_data_no_na), aes(x = ind, y = values)) +  geom_boxplot()


corrplot(cor(cervical_selected_data_no_na))




cervical <- write.csv(cervical_selected_data_no_na, "Cervical Cancer_DB_clean.csv")