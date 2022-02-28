#'#############################################################
#' Bayesian statistics seminar -  
#' 
#' * Dummy example to fit a linear regression in Stan
#'
#'  Based on data from package caper (Orme et al. )
#'  
#'  by Ignacio Morales-Castilla 
#'  feb 2022
#'#############################################################


# houskeeping,  loading packages
rm(list  =  ls())
options(stringsAsFactors  =  FALSE)

packs.to.extract <- list('caper',  'rstan',  'brms')

lapply(packs.to.extract,  require,  character.only  =  T)




#'###############################
#### Guided example for BRMS ####
#'###############################


#### get data ####
data(shorebird)




## we are going to explore how egg size relates to body size

## generate the comparative.data
shorebird <- comparative.data(shorebird.tree, shorebird.data, Species, vcv=TRUE)





#### inspect data ####
head(shorebird.data)

malemass <- log(shorebird.data$M.Mass)
eggmass <-  log(shorebird.data$Egg.Mass)
shorebird.data$malemass <- malemass
shorebird.data$eggmass <- eggmass 
plot(malemass,
     eggmass,
     pch=16,
     xlab="Male body mass (log-scale)",
     ylab="Egg mass (log-scale)")


#### fit bayesian model ####

prior1 <- prior(uniform(0, 1))

fit1 <- brm(eggmass ~ malemass,
            data = shorebird.data, 
            prior = prior1)

summary(fit1)



#### fit other two models with more informative priors ####

prior2 <- prior(normal(0, 1))

fit2 <- brm(eggmass ~ malemass,
            data = shorebird.data, 
            prior = prior2)
summary(fit2)


prior3 <- prior(beta(200, 1))

fit3 <- brm(eggmass ~ malemass,
            data = shorebird.data, 
            prior = prior3)
summary(fit3)


#### check results ####
plot(fit1)
plot(fit3)

plot(conditional_effects(fit1), points = TRUE)
plot(conditional_effects(fit3), points = TRUE)

pp_check(fit2, ndraws=20)
pp_check(fit3, ndraws=100)
pp_check(fit1, ndraws=100)




#### compare against alternative model ####

loo(fit1, fit2, fit3)









