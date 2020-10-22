DF = iris

DF[,5]

#q1 i
VDF <- DF[c(51:100),c(1:4)]
VDF
dim(VDF)

#q1 ii
model2 <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data=VDF)

#q1 iii
summary(model2)$coef
coef(model2)

#q1 iv
pw <- VDF[4]

coef(model2) 

fitted(model2)[1]

#q1v i
coeffs = coefficients(model2)
duration = coeffs[1] + coeffs[2] * 5.1 + coeffs[3] * 3.5 + coeffs[4] * 1.4
duration
#q1v ii
newdata = data.frame(Sepal.Length=5.1, Sepal.Width=3.5, Petal.Length=1.4)
predict(model2, newdata)

#q2 i a
x = model2$fitted - VDF
x[,4]

#q2 i b
model2$residual

#q2 ii a
plot(model2, 1)

#q2 iii a
plot(model2, 2)

#q2 iv
plot(model2)

#q3 i
fit0 <- lm(Petal.Width ~ 1, data=VDF)
summary(fit0)

#q3 ii
cor(VDF)

#q3 iii
fit1 <- lm(Petal.Width ~ Petal.Length, data=VDF)

#q3 iv a
fit2 <- lm(Petal.Width ~ Sepal.Width + Petal.Length, data=VDF)

#q3 iv b
summary(fit1)$adj.r.sq
summary(fit2)$adj.r.sq
# fit1 has highest value for r^2

#q3 v a&b
fit3 <- lm(Petal.Width ~ Sepal.Length + Petal.Length + Sepal.Width, data=VDF)
summary(fit3)$adj.r.sq
# fit3 highest r^2 of 3 models

#q3 vi
anova(fit3)

