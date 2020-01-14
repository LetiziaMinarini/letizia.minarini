## Encoding countries (weighted)
wvs_data1$V2= factor(wvs_data1$V2)
wvs_data1 = wvs_data
wvs_data$V2a= wvs_data$V2
contrasts(wvs_data$V2a)
contrasts(wvs_data$V2a) <- contr.wec(wvs_data$V2, omitted = "156")
is.factor(wvs_data$V2w)
wvs_data1$V2= factor(wvs_data1$V2)
levels(wvs_data$V2)
str(wvs_data$V2w)
summary(wvs_data$V2)
wvs_data$V2w = factor(wvs_data$V2)
levels(wvs_data$V2w)
wvs_data1$V2 <- factor(wvs_data1$V2, levels = c(1, 2, 3,4,5), labels = c("China", "Germany", "India", "Russia", "United States"))
contrasts(wvs_data1$V2w)= contr.wec(wvs_data1$V2, omitted = "China")
contrasts(wvs_data1$V2w)
## Domanda 1)
factor(wvs_data$V2)
contrasts(wvs_data$V2)
# Risposta= 5 Cina, Germania, India Russia e USA.

## Domanda 2) What are the sample sizes for each country represented in these data?
count(wvs_data, 'V2')
#V2 freq
#1 156 2300
#2 276 2046
#3 356 5659
#4 643 2500
#5 840 2232

## Domanda 3) Overall, is there a significant effect of country on feelings of happiness?
lmOut1= lm(V10~ V2, data= wvs_data )
summary(lmOut1)
##COUNTRY_CODE-> 156:china, 276: Germany; 356:India; 643:Russia; 840:United States
## RISPOSTA : No R-squared very low (0.0006758)
#Call:
#lm(formula = V10 ~ V2a, data = wvs_data)

#Residuals:
 # Min       1Q   Median       3Q      Max 
#-1.10920 -0.73611  0.08895  0.19844  2.26389 

#Coefficients:
#                     Estimate Std. Error  t value Pr(>|t|)    
  #(Intercept)       1.99391    0.01410 141.389  < 2e-16 ***
 # V2aGermany       -0.08287    0.02055  -4.032 5.56e-05 ***
 # V2aIndia         -0.19236    0.01672 -11.502  < 2e-16 ***
 # V2aRussia         0.11529    0.01954   5.900 3.72e-09 ***
 # V2aUnited states -0.25780    0.02010 -12.829  < 2e-16 ***
 # ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6763 on 14732 degrees of freedom
#Multiple R-squared:  0.0348,	Adjusted R-squared:  0.03454 
#F-statistic: 132.8 on 4 and 14732 DF,  p-value: < 2.2e-16
out1= lm(V10~V2a, data=wvs_data1)
summary(out1)
## Domanda 4) Which country has the highest level of feelings of happiness? Russia
## Domanda 5) Which country has the lowest level of feelings of happiness? United states
## Domanda 6) How do the country-specific levels of feelings of happiness change after controlling for
##subjective state of health?
out4= lm(V10 ~  V2a, data = wvs_data1)
summary(out4)
out5= lm(V10 ~ V11 + V2a, data = wvs_data1)
out5.1= lm(V10 ~ V2a +V11, data = wvs_data1)
summary(out5) 
summary(out5.1)  
## PARTE2
## Domanda 2.1)After controlling for country, does the importance people afforded to democracy (DemImp) (V140)
##significantly predict the extent to which they think their country is being run
##democratically (DemRun)(V141)? Yes, apart from india
  out5= lm(V141~V2+V140, data=wvs_data)
  out6= lm(V141~V140, data=wvs_data)
  summary(out5)
  summary(out6)
 
## Domanda 2.2)After controlling for country, does the DemImp --> DemRun effect vary as a function of
## peoples’ satisfaction with their lives (SWL)?
  out7= lm(V141~V140*V23+V2, data=wvs_data)##GIUSTO
  summary(out7)
  out8= lm(V141~V140*V23, data=wvs_data)
  summary(out8)
zMean= mean(wvs_data$V23)
zSd= sd(wvs_data$V23)
wvs_data$V23m = wvs_data$V23 -zMean
wvs_data$V23hi = wvs_data$V23 -(zMean+zSd)
wvs_data$V23lo=wvs_data$V23 -(zMean-zSd)
#MODELLO CENTERING:
out7.1= lm(V141~V2a+V140*V23m, data=wvs_data)
summary(out7.1)
#MODELLO CENTERIN + 1SD:
out7.2= lm(V141~V2a+V140*V23hi, data=wvs_data)
summary(out7.2)
#MODELLO CENTERING -1SD:
out7.3= lm(V141~V2a+V140*V23lo, data=wvs_data)
summary(out7.3)

plotOut1 <- plotSlopes(out7,
                       plotx      = "V140",
                       modx       = "V23",
                       
                       modxVals   = "std.dev")
testOut1 <- testSlopes(plotOut1)
ls(testOut1)
testOut1$hypotests
plot(testOut1)
ls(testOut1)
ls(testOut1$jn)                       
testOut1$jn$roots
## DOMANDA 2.3.1) After controlling for SWL, does the DemImp ! DemRun effect vary significantly by
##country? YeS we runned an anova test on both model
out8= lm(V141~V23+V140+V2a, data=wvs_data)#Additive effect
summary(out8)

out9= lm(V141~V23+V140*V2a, data=wvs_data)#moderated effect
summary(out9)

anova(out8, out9)
## DOMANDA 2.3.2) Visualize the results from Question 1 in a suitable way
plotOut2 <- plotSlopes(model      = out9,
                       plotx      = "V140",
                       modx       = "V2a",
                       
                       plotPoints = FALSE, main= "categorical moderator")

testOut2 <- testSlopes(plotOut2)
testOut2$hypotests
plot(testOut2)
testOut2$jn$roots
##DOMANDA2.3.3)For which country is the effect of DemImp on DemRun strongest, after controlling for
##SWL? 356
##DOMANDA 2.3.4) For which country is the effect of DemImp on DemRun weakest, after controlling for
##SWL? 643


K      <- 10
k      <- 1
data   <- wvs_data
models<- c( "V59 ~ V238+V239+V237",
            "V59 ~ V55+V56+V170",
            "V59 ~ V58+V57+V248")
model  <- models[1]

## Specify a function to do K-Fold Cross-Validation with lm():
cv.lm <- function(data, models, K = 10) {
  ## Create a partition vector:
  part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]
  
  ## Find the DV:
  dv <- trimws(strsplit(models[1], "~")[[1]][1])
  
  ## Apply over candidate models:
  sapply(X = models, FUN = function(model, data, dv, K, part) {
    ## Loop over K repititions:
    mse <- c()
    for(k in 1 : K) {
      ## Partition data:
      train <- data[part != k, ]
      valid <- data[part == k, ]
      
      ## Fit model, generate predictions, and save the MSE:
      fit    <- lm(model, data = train)
      pred   <- predict(fit, newdata = valid)
      mse[k] <- MSE(y_pred = pred, y_true = valid[ , dv])
    }
    ## Return the CVE:
    sum((table(part) / length(part)) * mse)
    
  },
  data = data,
  K    = K,
  dv   = dv,
  part = part)
}

rm(list = c("K", "k", "data", "models", "model"))# Clean up

cv.lm(data, models, K=10)
## Compare the four models from above using 10-fold cross-validation:
cv.lm(data   = rbind(train, valid),
      models = c("charges ~ age + sex + children",
                 "charges ~ age + sex + region",
                 "charges ~ age + sex + bmi",
                 "charges ~ age + sex + smoker")
)

mod1=lm(V59 ~ V238+V239+V237, data=wvs_data)
mod2=lm(V59 ~ V55+V56+V170, data=wvs_data)
mod3=lm(V59 ~ V58+V57+V248, data=wvs_data)
attributes(wvs_data$V237)
attributes(mod1)





