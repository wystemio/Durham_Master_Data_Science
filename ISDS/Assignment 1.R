# load the data ToothGrowth
data("ToothGrowth")
?ToothGrowth
# preview the structure of the data
str(ToothGrowth)
len <- ToothGrowth$len
supp <- ToothGrowth$supp

# install.packages("car")
library(car)

View(ToothGrowth)

# Which chart type is most appropriate to compare tooth length ’len’ by supplement type ’supp’? (Q1)
# Boxplot
plot(x = supp, y = len, main = 'len by supp', xlab = 'supp', ylab = 'len')

# Considering tooth length ’len’ by supplement type ’supp’, what type of experiment design do we have? (Q2)
# Two independent samples

# Provide the means and standard deviations of the tooth length by supplement type (Q3).
# OJ <- ToothGrowth[which(ToothGrowth$supp == 'OJ'),] #提取OJ类的数据
# VC <- ToothGrowth[which(ToothGrowth$supp == 'VC')] # 提取VC类的数据

OJ <- subset(ToothGrowth, ToothGrowth$supp == 'OJ')
VC <- subset(ToothGrowth, ToothGrowth$supp == 'VC')
View(OJ)
View(VC)

mean(OJ$len)
sd(OJ$len)
mean(VC$len)
sd(VC$len)

lenOJ <- OJ$len
lenVC <- VC$len

leveneTest(len~supp, ToothGrowth)
leveneTest(len~supp, ToothGrowth, center=mean)


t.test(lenOJ, lenVC, "two.sided", paired = FALSE,var.equal = TRUE, conf.level = 0.99)

# Regression analysis
# install.packages("ggplot2")
library(ggplot2)
data(mpg)
?mpg
str(mpg)
head(mpg)

fit1 <- lm(hwy~displ, data=mpg)
summary(fit1)

summary(fit1)$r.sq
summary(fit1)$adj.r.squared

displ = data.frame(displ = 4.5)
predict(fit1, displ)

View(mpg)

# write.csv(mpg, file = "mpg.csv")

par(mfrow=c(2,2))
plot(fit1)

fit2 <- lm(hwy~displ+I(displ^2), data = mpg)  # https://zhuanlan.zhihu.com/p/341195608
summary(fit2)
predict(fit2, displ)

fit3 <- lm(hwy~displ+(displ^2), data = mpg)
summary(fit3)
