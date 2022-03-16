LineaerModeling.R
Cameron Willliams
2020-03-08
library(rvest)
## Warning: package 'rvest' was built under R version 3.6.3
## Loading required package: xml2
library(readxl)
## Warning: package 'readxl' was built under R version 3.6.3
library(ggplot2)

data <- read_excel("mlr01.xls")
data
## # A tibble: 8 x 4
##      X1    X2    X3    X4
##   <dbl> <dbl> <dbl> <dbl>
## 1  2.90  9.20  13.2     2
## 2  2.40  8.70  11.5     3
## 3  2     7.20  10.8     4
## 4  2.30  8.5   12.3     2
## 5  3.20  9.6   12.6     3
## 6  1.90  6.80  10.6     5
## 7  3.40  9.70  14.1     1
## 8  2.10  7.90  11.2     3
str(data)
## Classes 'tbl_df', 'tbl' and 'data.frame':    8 obs. of  4 variables:
##  $ X1: num  2.9 2.4 2 2.3 3.2 ...
##  $ X2: num  9.2 8.7 7.2 8.5 9.6 ...
##  $ X3: num  13.2 11.5 10.8 12.3 12.6 ...
##  $ X4: num  2 3 4 2 3 5 1 3
colnames(data)
## [1] "X1" "X2" "X3" "X4"
cnames <- colnames(data)
cnames[1] <- "springFcount"
cnames
## [1] "springFcount" "X2"           "X3"           "X4"
colnames(data) <- cnames
data
## # A tibble: 8 x 4
##   springFcount    X2    X3    X4
##          <dbl> <dbl> <dbl> <dbl>
## 1         2.90  9.20  13.2     2
## 2         2.40  8.70  11.5     3
## 3         2     7.20  10.8     4
## 4         2.30  8.5   12.3     2
## 5         3.20  9.6   12.6     3
## 6         1.90  6.80  10.6     5
## 7         3.40  9.70  14.1     1
## 8         2.10  7.90  11.2     3
cnames <- colnames(data)
cnames[2] <- "Sizeofadultpop"
cnames
## [1] "springFcount"   "Sizeofadultpop" "X3"             "X4"
colnames(data) <- cnames
data
## # A tibble: 8 x 4
##   springFcount Sizeofadultpop    X3    X4
##          <dbl>          <dbl> <dbl> <dbl>
## 1         2.90           9.20  13.2     2
## 2         2.40           8.70  11.5     3
## 3         2              7.20  10.8     4
## 4         2.30           8.5   12.3     2
## 5         3.20           9.6   12.6     3
## 6         1.90           6.80  10.6     5
## 7         3.40           9.70  14.1     1
## 8         2.10           7.90  11.2     3
cnames <- colnames(data)
cnames[3] <- "Perc.in"
colnames(data) <- cnames
data
## # A tibble: 8 x 4
##   springFcount Sizeofadultpop Perc.in    X4
##          <dbl>          <dbl>   <dbl> <dbl>
## 1         2.90           9.20    13.2     2
## 2         2.40           8.70    11.5     3
## 3         2              7.20    10.8     4
## 4         2.30           8.5     12.3     2
## 5         3.20           9.6     12.6     3
## 6         1.90           6.80    10.6     5
## 7         3.40           9.70    14.1     1
## 8         2.10           7.90    11.2     3
cnames <- colnames(data)
cnames[4] <- "WinterSeverity"
cnames
## [1] "springFcount"   "Sizeofadultpop" "Perc.in"        "WinterSeverity"
colnames(data) <- cnames
data
## # A tibble: 8 x 4
##   springFcount Sizeofadultpop Perc.in WinterSeverity
##          <dbl>          <dbl>   <dbl>          <dbl>
## 1         2.90           9.20    13.2              2
## 2         2.40           8.70    11.5              3
## 3         2              7.20    10.8              4
## 4         2.30           8.5     12.3              2
## 5         3.20           9.6     12.6              3
## 6         1.90           6.80    10.6              5
## 7         3.40           9.70    14.1              1
## 8         2.10           7.90    11.2              3
##==============================================
plot(data$Sizeofadultpop, data$springFcount)
 
plot(data$Perc.in, data$springFcount)
 
plot(data$WinterSeverity, data$springFcount)
 
## y = SpringFawnCount and x = SizeOfAdult
S <- ggplot(data, aes(x = Sizeofadultpop, y = springFcount)) 
S <- S + geom_point(aes(size = WinterSeverity))
S
 
##==============================================
## y = springFcount and x = Perc.in
M <- ggplot(data, aes(x = Perc.in, y = springFcount))
M <- M + geom_point(aes(colour  = factor(Sizeofadultpop)))
M
 
##==============================================
## y = springFcount and x = Perc.in
C <- ggplot(data, aes(x = WinterSeverity, y = springFcount))
C <- C + geom_point(aes(colour = factor(WinterSeverity), size = Sizeofadultpop))
C
 
##=============================================
model1 <- lm(formula = springFcount ~ WinterSeverity, data = data)
plot(data$WinterSeverity, data$springFcount)
abline(model1)
 
summary(model1)
## 
## Call:
## lm(formula = springFcount ~ WinterSeverity, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.52069 -0.20431 -0.00172  0.13017  0.71724 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      3.4966     0.3904   8.957 0.000108 ***
## WinterSeverity  -0.3379     0.1258  -2.686 0.036263 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.415 on 6 degrees of freedom
## Multiple R-squared:  0.5459, Adjusted R-squared:  0.4702 
## F-statistic: 7.213 on 1 and 6 DF,  p-value: 0.03626
model2 <- lm(forumla = springFcount ~ WinterSeverity + Sizeofadultpop, data = data)
## Warning: In lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
##  extra argument 'forumla' will be disregarded
summary(model2)
## 
## Call:
## lm(data = data, forumla = springFcount ~ WinterSeverity + Sizeofadultpop)
## 
## Residuals:
##        1        2        3        4        5        6        7        8 
## -0.11533 -0.02661  0.09882 -0.11723  0.02734 -0.04854  0.11715  0.06441 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    -5.92201    1.25562  -4.716   0.0092 **
## Sizeofadultpop  0.33822    0.09947   3.400   0.0273 * 
## Perc.in         0.40150    0.10990   3.653   0.0217 * 
## WinterSeverity  0.26295    0.08514   3.089   0.0366 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1209 on 4 degrees of freedom
## Multiple R-squared:  0.9743, Adjusted R-squared:  0.955 
## F-statistic: 50.52 on 3 and 4 DF,  p-value: 0.001229
model3 <- lm(formula = springFcount ~ WinterSeverity + Sizeofadultpop + Perc.in, data = data)
summary(model3)
## 
## Call:
## lm(formula = springFcount ~ WinterSeverity + Sizeofadultpop + 
##     Perc.in, data = data)
## 
## Residuals:
##        1        2        3        4        5        6        7        8 
## -0.11533 -0.02661  0.09882 -0.11723  0.02734 -0.04854  0.11715  0.06441 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    -5.92201    1.25562  -4.716   0.0092 **
## WinterSeverity  0.26295    0.08514   3.089   0.0366 * 
## Sizeofadultpop  0.33822    0.09947   3.400   0.0273 * 
## Perc.in         0.40150    0.10990   3.653   0.0217 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1209 on 4 degrees of freedom
## Multiple R-squared:  0.9743, Adjusted R-squared:  0.955 
## F-statistic: 50.52 on 3 and 4 DF,  p-value: 0.001229
mod <- lm(formula = springFcount ~ data$Sizeofadultpop, data = data)
summary(mod)
## 
## Call:
## lm(formula = springFcount ~ data$Sizeofadultpop, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.24988 -0.17586  0.04938  0.12611  0.25309 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.67914    0.63422  -2.648 0.038152 *  
## data$Sizeofadultpop  0.49753    0.07453   6.676 0.000547 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2121 on 6 degrees of freedom
## Multiple R-squared:  0.8813, Adjusted R-squared:  0.8616 
## F-statistic: 44.56 on 1 and 6 DF,  p-value: 0.0005471
##==================================================
## which model Works best ? 
## Both the model2 and model3 work best due to the fact that their P-value is low 
## and can be considered statistically significant, the R- sqaured value of both also are both 97% 
## which means that the variables that are independant can almost perfectely predict the dependant variable. 

##Which of the predictors are statistically significant in each model?
## in model1 the WinterSeverity P value is 0.36263 which makes in fairly close to 
## 0.05 and not maintaining great statistical signifiance. In Model2, the most statisticially significant coefficient
## would have to be the Pericipitation variable seeing that its 0.0217 which is also the same in the 
## third model. 

##If you wanted to create the most parsimonious model (i.e., the one that did the best job with the fewest predictors), 
##what would it contain?
## Since the sizeofadultpop has the lowest p value of 0.0005471 as well as a
## 88% R - squared value when paired with the dependent value of springFcount it is the greatest pairing that offers
## the best job with the fewest amount of variables