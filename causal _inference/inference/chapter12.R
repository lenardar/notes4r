#####################################################
# PROGRAM 12.1
# Descriptive statistics from NHEFS data (Table 12.1)
#####################################################

#install.packages("readxl") # install package if required
library("readxl")

nhefs <- read_excel("F:/Homework/Textbooks/Hernan - Causal Inferences/nhefs.xls")
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

nhefs.nmv <- nhefs[which(!is.na(nhefs$wt82)),] # provisionally ignore subjects with missing values for weight in 1982

lm(wt82_71 ~ qsmk, data=nhefs.nmv)
predict(lm(wt82_71 ~ qsmk, data=nhefs.nmv), data.frame(qsmk=1)) # Smoking cessation
predict(lm(wt82_71 ~ qsmk, data=nhefs.nmv), data.frame(qsmk=0)) # No smoking cessation

# Table
summary(nhefs.nmv[which(nhefs.nmv$qsmk==0),]$age)
summary(nhefs.nmv[which(nhefs.nmv$qsmk==0),]$wt71) 
summary(nhefs.nmv[which(nhefs.nmv$qsmk==0),]$smokeintensity) 
summary(nhefs.nmv[which(nhefs.nmv$qsmk==0),]$smokeyrs) 

summary(nhefs.nmv[which(nhefs.nmv$qsmk==1),]$age)
summary(nhefs.nmv[which(nhefs.nmv$qsmk==1),]$wt71) 
summary(nhefs.nmv[which(nhefs.nmv$qsmk==1),]$smokeintensity) 
summary(nhefs.nmv[which(nhefs.nmv$qsmk==1),]$smokeyrs) 

table(nhefs.nmv$qsmk, nhefs.nmv$sex)
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$sex), 1)

table(nhefs.nmv$qsmk, nhefs.nmv$race)
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$race), 1)

table(nhefs.nmv$qsmk, nhefs.nmv$education)
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$education), 1)

table(nhefs.nmv$qsmk, nhefs.nmv$exercise)
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$exercise), 1)

table(nhefs.nmv$qsmk, nhefs.nmv$active)
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$active), 1)


#####################################################
# PROGRAM 12.2
# Estimating IP weights
# Data from NHEFS
#####################################################

# Estimation of ip weights via a logistic model
fit <- glm(qsmk ~ sex + race + age + I(age^2) + 
  as.factor(education) + smokeintensity + 
  I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
  as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
           family = binomial(), data = nhefs.nmv)
summary(fit)

p.qsmk.obs <- ifelse(nhefs.nmv$qsmk == 0, 1 - predict(fit, type = "response"),
                     predict(fit, type = "response"))

nhefs.nmv$w <- 1/p.qsmk.obs
summary(nhefs.nmv$w)
sd(nhefs.nmv$w)

#install.packages("geepack") # install package if required
library("geepack")
msm.w <- geeglm(wt82_71 ~ qsmk, data=nhefs.nmv, weights=w, id=seqn,
                corstr="independence")
summary(msm.w)

beta <- coef(msm.w)
SE <- coef(summary(msm.w))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)

# no association between sex and qsmk in pseudo-population
xtabs(nhefs.nmv$w ~ nhefs.nmv$sex + nhefs.nmv$qsmk)

# "check" for positivity (White women)
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1], 
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1])


#####################################################
# PROGRAM 12.3
# Estimating stabilized IP weights
# Data from NHEFS
#####################################################

# estimation of denominator of ip weights
denom.fit <- glm(qsmk ~ as.factor(sex) + as.factor(race) + age + I(age^2) + 
  as.factor(education) + smokeintensity + 
  I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
  as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
                 family = binomial(), data = nhefs.nmv)
summary(denom.fit)

pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights
numer.fit <- glm(qsmk~1, family = binomial(), data = nhefs.nmv)
summary(numer.fit)

pn.qsmk <- predict(numer.fit, type = "response")

nhefs.nmv$sw <- ifelse(nhefs.nmv$qsmk == 0, ((1-pn.qsmk)/(1-pd.qsmk)),
                    (pn.qsmk/pd.qsmk))

summary(nhefs.nmv$sw)


msm.sw <- geeglm(wt82_71 ~ qsmk, data=nhefs.nmv, weights=sw, id=seqn,
                corstr="independence")
summary(msm.sw)

beta <- coef(msm.sw)
SE <- coef(summary(msm.sw))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)

# no association between sex and qsmk in pseudo-population
xtabs(nhefs.nmv$sw ~ nhefs.nmv$sex + nhefs.nmv$qsmk)


#####################################################
# PROGRAM 12.4
# Estimating the parameters of a marginal structural mean model
# with a continuous treatment Data from NHEFS
#####################################################

# Analysis restricted to subjects reporting <=25 cig/day at baseline
nhefs.nmv.s <- subset(nhefs.nmv, smokeintensity <=25)

# estimation of denominator of ip weights
den.fit.obj <- lm(smkintensity82_71 ~ as.factor(sex) + 
  as.factor(race) + age + I(age^2) + 
  as.factor(education) + smokeintensity + I(smokeintensity^2) +
  smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) + wt71  + 
  I(wt71^2), data = nhefs.nmv.s)
p.den <- predict(den.fit.obj, type = "response")
dens.den <- dnorm(nhefs.nmv.s$smkintensity82_71, p.den, summary(den.fit.obj)$sigma)

# estimation of numerator of ip weights
num.fit.obj <- lm(smkintensity82_71 ~ 1, data = nhefs.nmv.s)
p.num <- predict(num.fit.obj, type = "response")
dens.num <- dnorm(nhefs.nmv.s$smkintensity82_71, p.num, summary(num.fit.obj)$sigma)

nhefs.nmv.s$sw.a = dens.num/dens.den
summary(nhefs.nmv.s$sw.a)

msm.sw.cont <- geeglm(wt82_71 ~ smkintensity82_71 + I(smkintensity82_71*smkintensity82_71), 
                 data=nhefs.nmv.s, weights=sw.a, id=seqn, corstr="independence")
summary(msm.sw.cont)

beta <- coef(msm.sw.cont)
SE <- coef(summary(msm.sw.cont))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)


#####################################################
# PROGRAM 12.5
# Estimating the parameters of a marginal structural logistic model
# Data from NHEFS
#####################################################

table(nhefs.nmv$qsmk, nhefs.nmv$death)

# First, estimation of stabilized weights sw.a (same as in PROGRAM 12.3)
# Second, fit logistic model below
msm.logistic <- geeglm(death ~ qsmk, data=nhefs.nmv, weights=sw.a, 
                      id=seqn, family=binomial(), corstr="independence")
summary(msm.logistic)

beta <- coef(msm.logistic)
SE <- coef(summary(msm.logistic))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)


#####################################################
# PROGRAM 12.6
# Assessing effect modification by sex using a marginal structural mean model
# Data from NHEFS
#####################################################

table(nhefs.nmv$sex)

# estimation of denominator of ip weights
denom.fit <- glm(qsmk ~ as.factor(sex) + as.factor(race) + age + I(age^2) + 
  as.factor(education) + smokeintensity + 
  I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
  as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
                 family = binomial(), data = nhefs.nmv)
summary(denom.fit)

pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights
numer.fit <- glm(qsmk~as.factor(sex), family = binomial(), data = nhefs.nmv)
summary(numer.fit)
pn.qsmk <- predict(numer.fit, type = "response")

nhefs.nmv$sw.a <- ifelse(nhefs.nmv$qsmk == 0, ((1-pn.qsmk)/(1-pd.qsmk)),
                     (pn.qsmk/pd.qsmk))

summary(nhefs.nmv$sw.a)
sd(nhefs.nmv$sw.a)

# Estimating parameters of a marginal structural mean model
msm.emm <- geeglm(wt82_71~as.factor(qsmk) + as.factor(sex) 
                  + as.factor(qsmk):as.factor(sex), data=nhefs.nmv, 
                  weights=sw.a, id=seqn, corstr="independence")
summary(msm.emm)

beta <- coef(msm.emm)
SE <- coef(summary(msm.emm))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)


#####################################################
# PROGRAM 12.7
# Estimating IP weights to adjust for selection bias due to censoring
# Data from NHEFS
#####################################################

table(nhefs$qsmk, nhefs$cens)

summary(nhefs[which(nhefs$cens==0),]$wt71)
summary(nhefs[which(nhefs$cens==1),]$wt71)

# estimation of denominator of ip weights for A
denom.fit <- glm(qsmk ~ as.factor(sex) + as.factor(race) + age + I(age^2) + 
  as.factor(education) + smokeintensity + 
  I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
  as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
                 family = binomial(), data = nhefs)
summary(denom.fit)

pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights for A
numer.fit <- glm(qsmk~1, family = binomial(), data = nhefs)
summary(numer.fit)
pn.qsmk <- predict(numer.fit, type = "response")

# estimation of denominator of ip weights for C
denom.cens <- glm(cens ~ as.factor(qsmk) + as.factor(sex) + 
  as.factor(race) + age + I(age^2) + 
  as.factor(education) + smokeintensity + 
  I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
  as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
                  family = binomial(), data = nhefs)
summary(denom.cens)

pd.cens <- 1-predict(denom.cens, type = "response")

# estimation of numerator of ip weights for C
numer.cens <- glm(cens~as.factor(qsmk), family = binomial(), data = nhefs)
summary(numer.cens)
pn.cens <- 1-predict(numer.cens, type = "response")

nhefs$sw.a <- ifelse(nhefs$qsmk == 0, ((1-pn.qsmk)/(1-pd.qsmk)),
                     (pn.qsmk/pd.qsmk))
nhefs$sw.c <- pn.cens/pd.cens
nhefs$sw <- nhefs$sw.c*nhefs$sw.a

summary(nhefs$sw.a)
sd(nhefs$sw.a)
summary(nhefs$sw.c)
sd(nhefs$sw.c)
summary(nhefs$sw)
sd(nhefs$sw)

msm.sw <- geeglm(wt82_71~qsmk, data=nhefs, 
                  weights=sw, id=seqn, corstr="independence")
summary(msm.sw)

beta <- coef(msm.sw)
SE <- coef(summary(msm.sw))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)