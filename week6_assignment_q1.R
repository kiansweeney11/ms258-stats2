#q1 i a
baby = read.table("babyw.txt")

baby

#q1 i b
V1 <- c(30, 32, 34, 36, 38, 40)
V2 <- c(1.6, 1.7, 2.5, 2.8, 3.2, 3.5)

scatter <- data.frame(V1, V2)
scatter

attach(scatter)
plot(V1, V2, main="Scatterplot", xlab="gest", ylab="weight(kg)")

#q1 i c
# strong positive linear relationship between v1 and v2

#q1 ii a
cor(V1, V2,  method = "pearson")
#q1 ii b
cor(V1, V2,  method = "spearman")
#q1 ii c
cor(V1, V2,  method = "kendall")

#q1 iii a
x <- scatter$V1
x

y <- scatter$V2
y

#q1 iii b
# x^2 - sum(x)^2/n
x_square <- 30^2 + 32^2 + 34^2 + 36^2 + 38^2 + 40^2
sxx <- x_square - (sum(x)^2 / 6)
sxx

y_square <- 1.6^2 + 1.7^2 + 2.5^2 + 2.8^2 + 3.2^2 + 3.5^2
syy <- y_square - (sum(y)^2 / 6)
syy

xy<- 30*1.6 + 32*1.7 + 34*2.5 + 36*2.8 + 38*3.2 + 40*3.5
xy
sxy <- xy - ((sum(x)*sum(y)) /6)
sxy

#q1 c iii
sxx * syy
# answer = 211.05
211.05^0.5

pearson <- sxy / 14.52756
pearson

#q1 d i
cor(V1, V2, method="spearman")

#q1 d ii
d_square = (0^2) * 6
spearman <- 1 - 0
spearman

# q1 e i
cor.test(V1, V2)$p.value

# q1 e ii
b <- (1 - pearson^2) / 4
p_val <- pearson / (b^0.5)
p_val

#q1 f 
cor.test(V1, V2, method = c("spearman"),alternative = "greater", conf.level = 0.99)

#q1 g
cor.test(V1, V2, method = c("kendall"),alternative = "less")
#do not reject h0

#q1 h
FisherZ(rho)
