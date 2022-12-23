## Install MuMIn package
install.packages("MuMIn", repos="http://R-Forge.R-project.org")

## Load libraries
library(tidyverse)
library(lme4)
library(DHARMa)
library(MuMIn)
library(lmerTest)

## Set working directory
setwd('/Users/Wendy/github/wildfire-smoke-orangutan-voice/data')

## Read files
enivronment <- read.csv('environment.csv')
context <- read.csv('context.csv')

Ncall_PreSmo <- read.csv('N.LC_PreSmoke.csv')
Ncall_PrePos <- read.csv('N.LC_PrePost.csv')

duration_PreSmo <- read.csv('durat_PreSmoke.csv')
duration_PrePos <- read.csv('durat_PrePost.csv')

voice_PreSmo<- read.csv('voice_PreSmoke.csv')
voice_PrePos<- read.csv('voice_PrePost.csv')

nlp_PreSmo<- read.csv('nlp_PreSmoke.csv')
nlp_PrePos <- read.csv('nlp_PrePost.csv')


# 1. Variation in environmental variables & call context across pe --------

# Create & re-level factors to compare to Pre-Smoke reference
enivronment$Period <- as.factor(enivronment$Period)
enivronment$Period <- relevel(enivronment$Period, ref = 2)  

context$Period <- as.factor(context$Period)
context$Period <- relevel(context$Period, ref = 2)  


## Temperature: all comparisons sig.
period.temp = lm(MaxTemp ~ Period, data = enivronment)
period.temp
summary(period.temp)

## Day rain: pre-smoke vs. smoke comparison sig.
period.rain = lm(DayRain ~ Period, data = enivronment)
period.rain
summary(period.rain)

## Context: no comparisons sig.
period.spont = lm(Spontaneous ~ Period, data = context)
period.spont
summary(period.spont)


# 2.  Model building for acoustic variables -------------------------------

## General linear mixed models & model residuals (use this for all variables in 'voice' data file; for jitter only, use "log(jit)" to log-transform this variable)
mod.f0.PreSmo = lmer(f0 ~ Period + (1 | Male.ID), data = voice_PreSmo)
summary(mod.f0.PreSmo)
plot(mod.f0.PreSmo)  
require(DHARMa)
fittedModel=mod.f0.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(lmer(f0 ~ Period + (1 | Male.ID), data = voice_PreSmo))


## General linear mixed model & model residuals including call context (use for 'duration')
mod.durat.PreSmo = lmer(duration ~ Period + Spontaneous + (1 | Male.ID), data = duration_PreSmo)
summary(mod.durat.PreSmo)
plot(mod.durat.PreSmo)  
require(DHARMa)
fittedModel=mod.durat.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(lmer(duration ~ Period + Spontaneous + (1 | Male.ID), data = duration_PreSmo))

## Generalized linear mixed models (Poisson, log) & model residuals (use this for 'Ncall' (# long calls) data files)
mod.NLC.PreSmo = glmer(N.LC ~ Period + (1 | Male.ID), family = poisson(link = "log"), data = Ncall_PreSmo)
summary(mod.NLC.PreSmo)
plot(mod.NLC.PreSmo)  
require(DHARMa)
fittedModel=mod.NLC.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(glmer(N.LC ~ Period + (1 | Male.ID), family = poisson(link = "log"), data = Ncall_PreSmo))

## Generalized linear mixed models (binomial, logit) & model residuals (use this for variables in 'nlp' data files)
mod.bp.PreSmo = glmer(BP.01 ~ Period + (1 | Male.ID), family = binomial(link = "logit"),  data = nlp_PreSmo)
summary(mod.bp.PreSmo)
plot(mod.bp.PreSmo)  
require(DHARMa)
fittedModel=mod.bp.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(glmer(BP.01 ~ Period + (1 | Male.ID), family = binomial(link = "logit"),  data = nlp_PreSmo))

## Jittered boxplots
nlc.smoke <- ggplot(PreSmo_Ncall, aes(x = Period, y = N.LC)) +
  geom_boxplot(varwidth = FALSE, outlier.color = NA) +
  scale_x_discrete(labels = c("Pre-Smoke","Smoke")) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0) +
  theme_classic() +
  labs(x = "", y = "N calls") 
nlc.smoke
