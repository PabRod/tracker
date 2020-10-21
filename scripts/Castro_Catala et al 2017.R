library(ggplot2)
library(lattice)
library(MuMIn)
library (MASS)
library(car)
library(lme4)
library(glmmADMB)

# read the data and select the columns of interest and the complete cases
#behavior1 <- read.csv2("behavior feeding experiment.csv", skip = 1)
behavior1 <- gammarus_acute
behavior1 <- filter(behavior1, Treatment_chem != 'SMX')
#behavior2 <- subset(behavior1, select = id:vel)
#behavior3 <- subset(behavior2, complete.cases(behavior2))

# indicate that certain variables are factors:
ind <-factor(behavior1$ind)  #subject, random effects (repeated measures)
fluoxetine <- factor(behavior1$Treatment_conc)   #fluoxetine in the conditioning
light <- factor(ifelse(behavior1$time_bin == 1 | behavior1$time_bin == 3, 0, 1)) #light/darkness cycles

# verify the differences in behavior (vel=swimming velocity) due to the light/darkness factor:
boxplot(aspeed~light, data=behavior1)
boxplot(aspeed~light*time_bin, data=behavior1)
anovaLight<-aov(aspeed~light, data=behavior1) #significant
summary(anovaLight)

# select only the velocity under light conditions (when G. pulex reacts):
behavior1$light <- light
behavior2 <- behavior1[ which(behavior1$light==1 ),]
summary(behavior2)

# Summarise data to timebins
behavior2_summarised <- summarise_data(input_data = behavior2, chemical = 'FLU',
                                       exp_dur = 480, timebin = 10, expsr_dur = 2)

# determine which is the distribution of the velocity data
library(fitdistrplus)
descdist(behavior2_summarised$avaspeed, boot = 1000)

qqp(behavior2_summarised$avaspeed, "norm")
qqp(behavior2_summarised$avaspeed, "lnorm")
gamma <- fitdistr(behavior2_summarised$avaspeed, "gamma")  
qqp(behavior2_summarised$avaspeed, "gamma", 
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
qqp(behavior2_summarised$avaspeed, "beta", 
    shape1 = mean(behavior2_summarised$avaspeed), 
    shape2 = var(behavior2_summarised$avaspeed))
#the gamma distribution fits much better than the normal or Gaussian distribution, or the lognormal


# which factors may affect the velocity? visual checking
boxplot(aspeed~time, data=behavior2) 
boxplot(aspeed~fluoxetine, data=behavior2)

# models with glmmADMB, with subject (id) as random effects
# First, we fit the null model without fixed effects (including random effects)
modADMBn <- glmmadmb(aspeed ~ 1 + time + (1 | ind),
                     family = "gamma", data = behavior2)
# Second, we build an alternative model including all fixed and random effects, and potential interactions
modADMBf <- glmmadmb(aspeed ~ light*fluoxetine + time*fluoxetine + time + (1 | id), 
                     family = "gamma", data = behavior2)    #all the variables included in the model are significant
# We compare the model with the null model
anova(modADMBn, modADMBf) #significantly different; LogLik and AIC of modADMBf smaller than modADMBn

# Calculation of the variance in fitted values (from Nakagawa and Schielzeth, 2016)
VarF <- var(as.vector(model.matrix(modADMBf) %*% fixef(modADMBf)))

# getting the observation-level variance Null model
nuN <- modADMBn$alpha # alpha=overdispersion 
VarOtN <- trigamma(nuN) # trigamma function

# ... and for the Full model
nuF <- modADMBf$alpha # alpha=overdispersion 
VarOtF <- trigamma(nuF) # trigamma function

#calcultion of the marginal R2 (fixed effects), and the conditional R2 (fixed and random effects)
# R2[GLMM(m)] - marginal R2[GLMM]
R2glmmM <- VarF/(VarF + sum(as.numeric(VarCorr(modADMBf))) + VarOtF)
R2glmmM  
# R2[GLMM(c)] - conditional R2[GLMM] for full model
R2glmmC <- (VarF + sum(as.numeric(VarCorr(modADMBf))))/(VarF + sum(as.numeric(VarCorr(modADMBf))) + VarOtF)                                            
R2glmmC
