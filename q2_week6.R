#q2 i a
baby = read.table("babyw.txt")
baby

#q2 i b
gest <- c(30, 32, 34, 36, 38, 40)
weight <- c(1.6, 1.7, 2.5, 2.8, 3.2, 3.5)

scatter <- data.frame(gest, weight)

model1 <- lm(scatter$weight~scatter$gest)

#q2 ii a
model1
# intercept = -4.6 slope=0.2043

#q2 ii b
plot(gest, weight)
abline(model1,lty="dashed",col="red")

#q2 iii a
model1$fitted

#q2 iii b
fitted(model1)

#q2 iii c
predict(model1)

#q2 iv
plot(model1$fitted, pch=1, col='blue')
lines(model1$fitted, lty=2, col='red')

#q2 v a
coeffs = coefficients(model1)
duration = coeffs[1] + coeffs[2] * 42
duration

#q2 v b
newdata <-data.frame(gest=42)

#then apply predict to that data frame

predict(model1,newdata)

# q2 vi
summary(model1)
model$stat
model$p.value
#p-value=0.0003661
#statistic=sqrt(124.7)=11.1669=11.17

#q2 vii a
slope <- model1$coef[1]
confint(model1, slope, level=0.99)

#q2 vii b
confint(model1, slope=0.24, level=0.95)
# do not reject, value contained in range

#q2 viii
summary(model1)
b <- model1$coef[2]
se <-summary(model1)$coef[2,2]
dof <- model1$df
b

#q2 ix a
ans <- b+c(-1,1)*qt(0.90,dof)*se
diff <- ans[2] - model1$coef[2]

upp <- model1$coef[2] + diff
lower <- model1$coef[2] - diff 
answer <- c(lower,upp)
answer

confint(model1,2,level=0.90)

#q2 ix b




