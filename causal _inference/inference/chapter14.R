################################################################
# PROGRAM 14.1
# Preprocessing, ranks of extreme observations, IP weights for censoring
# Data from NHEFS
################################################################

#install.packages("readxl") # install package if required
library("readxl")
nhefs <- read_excel("F:/Homework/Textbooks/Hernan - Causal Inferences/nhefs.xls")

# some processing of the data
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

# ranking of extreme observations
#install.packages("Hmisc")
library(Hmisc)
describe(nhefs$wt82_71)

# estimation of denominator of ip weights for C
cw.denom <- glm(cens==0 ~ qsmk + sex + race + age + I(age^2) 
                     + as.factor(education) + smokeintensity + I(smokeintensity^2) 
                     + smokeyrs + I(smokeyrs^2) + as.factor(exercise) 
                     + as.factor(active) + wt71 + I(wt71^2), 
                     data = nhefs, family = binomial("logit"))
summary(cw.denom)
nhefs$pd.c <- predict(cw.denom, nhefs, type="response")
nhefs$wc <- ifelse(nhefs$cens==0, 1/nhefs$pd.c, NA)  # observations with cens=1 only contribute to censoring models


##################################################################
#  PROGRAM 14.2
# G-estimation of a 1-parameter structural nested mean model
# Brute force search
# Data from NHEFS
##################################################################

####################################################
# G-estimation: Checking one possible value of psi #
####################################################
#install.packages("geepack")
library("geepack")

nhefs$psi <- 3.446
nhefs$Hpsi <- nhefs$wt82_71 - nhefs$psi*nhefs$qsmk

fit <- geeglm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs,
           weights=wc, id=seqn, corstr="independence")
summary(fit)


##########################################################
# G-estimation: Checking multiple possible values of psi #
##########################################################

# install.packages("geepack")

grid <- seq(from = 2,to = 5, by = 0.1)
j = 0
Hpsi.coefs <- cbind(rep(NA,length(grid)), rep(NA, length(grid)))
colnames(Hpsi.coefs) <- c("Estimate", "p-value")

for (i in grid){
  psi = i
  j = j+1
  nhefs$Hpsi <- nhefs$wt82_71 - psi * nhefs$qsmk 
  
  gest.fit <- geeglm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
                  + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                  + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                  + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs,
                  weights=wc, id=seqn, corstr="independence")
  Hpsi.coefs[j,1] <- summary(gest.fit)$coefficients["Hpsi", "Estimate"]
  Hpsi.coefs[j,2] <- summary(gest.fit)$coefficients["Hpsi", "Pr(>|W|)"]
}
Hpsi.coefs


##################################################################
#  PROGRAM 14.3
# G-estimation for 2-parameter structural nested mean model
# Closed form estimator
# Data from NHEFS
##################################################################

##########################################################
# G-estimation: Closed form estimator linear mean models #
##########################################################
logit.est <- glm(qsmk ~ sex + race + age + I(age^2) + as.factor(education) 
                 + smokeintensity + I(smokeintensity^2) + smokeyrs 
                 + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) 
                 + wt71 + I(wt71^2), data = nhefs, weight = wc, 
                 family = binomial())
summary(logit.est)
nhefs$pqsmk <- predict(logit.est, nhefs, type = "response")
describe(nhefs$pqsmk)
summary(nhefs$pqsmk)

# solve sum(w_c * H(psi) * (qsmk - E[qsmk | L]))  = 0
# for a single psi and H(psi) = wt82_71 - psi * qsmk
# this can be solved as psi = sum( w_c * wt82_71 * (qsmk - pqsmk)) / sum(w_c * qsmk * (qsmk - pqsmk))

nhefs.c <- nhefs[which(!is.na(nhefs$wt82)),]
with(nhefs.c, sum(wc*wt82_71*(qsmk-pqsmk)) / sum(wc*qsmk*(qsmk - pqsmk)))

#############################################################
# G-estimation: Closed form estimator for 2-parameter model #
#############################################################

diff = with(nhefs.c, qsmk - pqsmk)
diff2 = with(nhefs.c, wc * diff)

lhs = matrix(0,2,2)
lhs[1,1] = with(nhefs.c, sum(qsmk * diff2))
lhs[1,2] = with(nhefs.c, sum(qsmk * smokeintensity  * diff2))
lhs[2,1] = with(nhefs.c, sum(qsmk * smokeintensity * diff2))
lhs[2,2] = with(nhefs.c, sum(qsmk * smokeintensity * smokeintensity * diff2))

rhs = matrix(0,2,1)
rhs[1] = with(nhefs.c, sum(wt82_71 * diff2))
rhs[2] = with(nhefs.c, sum(wt82_71 * smokeintensity * diff2))

psi = t(solve(lhs,rhs))
psi
