library("faraway")
library('splines')
library('glmnet')
library('leaps')
library('pls')
library('gam')
library('Matrix')

# Mammals
?mammalsleep
sleep <- which( rowSums( is.na( mammalsleep ) ) > 0 )
mammal <- mammalsleep[-sleep,]
dim(mammal)
y = mammal$lifespan
x = mammal$sleep
percentiles = quantile(x, probs = c(0.25, 0.50, 0.75))
knots = summary(x)[c(2,3,5)]
spline1 = lm(y ~ bs(x, degree = 1, knots = summary(x)[c(2,3,5)]))
spline1

poly1 = lm(y ~ x )
poly2 = lm(y ~ poly(x, 2, raw = TRUE))
poly3 = lm(y ~ poly(x,  3))
poly4 = lm(y ~ poly(x,  4))
poly5 = lm(y ~ poly(x, 5))
poly6 = lm(y ~ poly(x, 6))
anova(poly1, poly2, poly3, poly4, poly5, poly6)

step = lm(y ~ cut(x,4))
pred = predict(step, newdata = list(x = sort(x)), se = TRUE)
se.bands = cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
plot(x, y, cex.lab = 1.1, col = 'darkgrey', bty = 'l')
lines(x = sort(x), y = pred$fit, lwd = 2, col = 'red')
matlines(sort(x), se.bands, lwd = 1.4, col = 'red', lty = 3)


# Brozek
?fat
meas = fat[,-c(2:8)]
sum(is.na(meas))
fat[1,c(2:8)]
dim(meas)

cor(meas$brozek, meas[,-1])
cor(meas[,-1])
pairs( meas, pch = 16, col =2)
cor(meas[,])

fwd = regsubsets(brozek~., data = meas, method = 'forward', nvmax = 10)
results = summary(fwd)
results
Cp = results$cp
which.min(Cp)
coef(fwd, 5)
BIC = results$bic
which.min(BIC)
coef(fwd, 3)


predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

repetitions = 150
cor.bss = c()
cor.ridge = c()
cor.pcr = c()

set.seed(3)
for(i in 1:repetitions){
  training.obs = sample(1:252, 200)
  y.train = meas$brozek[training.obs]
  x.train = model.matrix(brozek~., meas[training.obs,])[,-1]
  y.test = meas$brozek[-training.obs]
  x.test = model.matrix(brozek~., meas[-training.obs,])[,-1]
  
  bss.train = regsubsets(brozek~., data = meas[training.obs,], nvmax = 10)
  min.bic = which.min(summary(bss.train)$bic)
  ridge.train = cv.glmnet(x.train, y.train, alpha = 0, nfolds = 10)
  pcr.train = pcr(brozek~., data = meas[training.obs,], scale = TRUE, validation = 'CV')
  min.pcr = which.min(MSEP(pcr.train)$val[1,1, ]) - 1
  
  predict.bss = predict.regsubsets(bss.train, meas[-training.obs,], min.bic)
  predict.ridge = predict(ridge.train, x.test, s = 'lambda.min')
  predict.pcr = predict(pcr.train, meas[-training.obs,], ncomp = min.pcr)
  
  cor.bss[i] = cor(y.test, predict.bss)
  cor.ridge[i] = cor(y.test, predict.ridge)
  cor.pcr[i] = cor(y.test, predict.pcr)
}

boxplot(cor.bss, cor.ridge, cor.pcr,
        names = c('BSS', 'Ridge', 'PCR'),
        ylab = 'Test Correlation', col = 2:4)




min.bic = c()
for(i in 1:repetitions){
  training.obs = sample(1:252, 200)
  meas.train = meas[training.obs,]
  bss.train = regsubsets(brozek~., data = meas.train, nvmax = 10)
  min.bic[i] = which.min(summary(bss.train)$bic)
}

hist(min.bic)
hist(min.bic, col = 'red', breaks = seq(0, 10, 1), xlab = 'Predictions',main = 'Best Subset with BIC')

pcr.train = pcr(brozek~., data = meas.train, scale = TRUE, validation = 'CV')
min.pcr = which.min(MSEP(pcr.train)$val[1,1,]) -1
for(i in 1:repetitions){
  training.obs = sample(1:252, 200)
  meas.train = meas[training.obs,]
  pcr.train = pcr(brozek~., data = meas.train, scale = TRUE, validation = 'CV')
  min.pcr[i] = which.min(MSEP(pcr.train)$val[1,1,]) -1
}
min.pcr
hist(min.pcr, col = 'green', breaks = seq(from=0.5, to=14.5, length=15), xlab = 'Predictions', main = 'Min PCR')


gam = gam(brozek ~ s(abdom, df= 5) + ns(hip, df = 7) + ankle, data = meas)
par( mfrow = c(2,3) )
plot( gam,  se = TRUE, col = "blue" )
# Compare with the following plots.
plot( meas$abdom, meas$brozek, pch = 16, col = 2, 
      ylab = "brozek", xlab = "abdom" )
plot( meas$hip, meas$brozek, pch = 16, col = 2, 
      ylab = "brozek", xlab = "hip" )
plot( meas$ankle, meas$brozek, pch = 16, col = 2, 
      ylab = "brozek", xlab = "ankle" )
