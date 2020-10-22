version
# Kian Sweeney, ID: 18306226
#platform       x86_64-w64-mingw32          
#arch           x86_64                                  
#status                                     
#major          3                           
#minor          6.3                                               
#svn rev        77875                       
#language       R                           
#version.string R version 3.6.3 (2020-02-29)

mydata = swiss
mydata

#q1 i
cor(mydata, method="spearman")
# 0.2886878 highest (ag and catholic) next is ag and fertility 0.242

#q1 ii
plot(mydata$Fertility, mydata$Education, main="fert vs ed")

#q1 iii a
model1 <- lm(mydata$Fertility ~ mydata$Education)
summary(model1)
model1$coefficients
# intercept = 79.61 slope = -0.8623
# p-values are very low

#q1 iii b
plot(mydata$Fertility, mydata$Education, main="fert vs ed")
abline(model1, col='red', lwd=2, lty='dotted')

#q1 iii c
# the limitations of analysing this plot is the number of values to the left of the regression line

#q1 iii d
dof = model1$df
b = model1$coef[2]
se <- summary(model1)$coef[2,2]
b + c(-1,1)*qt(0.9999,dof)*se
# (-1.4488, -0.2758248)

#q1 iv a
coefs = coefficients(model1)
coefs
ans <- coefs[1] + coefs[2] * 15.7
ans
# ans = 66.07116

#q1 iv b
newdata <- data.frame(Education=15.7)
predict(model1, newdata, interval='confidence', level=0.9)
# (66.934, 71.58912)

#q1 iv c
plot(model1, 1)
se <- summary(model1)$coef[2,2]
se
model1$residuals
# glane, franches-mt, orbe highest values
# they are large outliers judging by the standard error as they are well above the mean with the SE a mere 0.1448447

#q1 iv d
plot(model1, 2)
# the quality of normality assumption holds true here

#q1 v
model1$residuals[42]
# 12.38515
newdata0 <- data.frame(Agriculture=17.6,Examination=35,Education=32,Catholic=16.92, Infant.Mortality=23)
predict(model1, newdata0, interval='confidence', level=0.99)
# (65.53473 72.98898)

#q1 vi
fit0 <- lm(mydata$Fertility ~ 1, data=mydata)
summary(fit0)
# beta = 70.143
cor(mydata)
#catholic best fit, infant, age, exam, education in order then

fit1 <- lm(mydata$Fertility ~ mydata$Catholic, data=mydata)
summary(fit1)
# beta = 64.42826 , is significant
# r square =  0.1976 

fit2 <-  lm(mydata$Fertility ~ mydata$Infant.Mortality + mydata$Catholic, data=mydata)
summary(fit2)
# r squared improved to 0.3005
# beta significant for infant but not catholic here

fit3 <-  lm(mydata$Fertility ~ mydata$Agriculture + mydata$Infant.Mortality + mydata$Catholic, data=mydata)
summary(fit3)
# r squared only slightly improved to 0.343
# infant again significant at 1.633, ag (0.14229) and catholic(0.087778) not so much

fit4 <-  lm(mydata$Fertility ~ mydata$Examination + mydata$Agriculture + mydata$Infant.Mortality + mydata$Catholic, data=mydata)
summary(fit4)
# r squared improved again at 0.5014
# beta significant for infant and exam 

fit5 <-  lm(mydata$Fertility ~ mydata$Education + mydata$Examination + mydata$Agriculture + mydata$Infant.Mortality + mydata$Catholic, data=mydata)
summary(fit5)
# r squared 0.671 improved significantly again
# beta significant for education and infant again

anova(fit5)
# intercept fit is most significant for fit1 and fit4 and fit5
# there is a large change in values here from previous values
# this is due to the high correlations between variables at play
# p value improves (moves away from 0) with each variable added, fits criteria
# education and infant were of most significance from anova test, highest values

summary(fit1)$adj.r.sq
summary(fit2)$adj.r.sq
summary(fit3)$adj.r.sq
summary(fit4)$adj.r.sq
summary(fit5)$adj.r.sq

finalfit <- lm(mydata$Fertility ~ mydata$Infant.Mortality + mydata$Education, data=mydata)
finalfit

# q1 viii a
newdata2 <- data.frame(Agriculture=17.6,Examination=35,Education=32,Catholic=16.92, Infant.Mortality=23)
predict(finalfit, newdata2, interval='confidence', level=0.99)
# (68.48853, 76.98536)

#q1 viii b
# the fit of the second model is significantly more central than the first model.

