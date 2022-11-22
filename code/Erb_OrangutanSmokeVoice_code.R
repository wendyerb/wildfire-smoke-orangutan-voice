## Load libraries
library(tidyverse)
library(lme4)
library(DHARMa)
library(MuMIn)
library(lmerTest)


## Set working directory
setwd('/Users/Wendy/github/wildfire-smoke-orangutan-voice/data')

## Read files
Ncall_PreSmo <- read.csv('N.LC_PreSmoke.csv')
Ncall_PrePos <- read.csv('N.LC_PrePost.csv')

duration_PreSmo <- read.csv('durat_PreSmoke.csv')
duration_PrePos <- read.csv('durat_PrePost.csv')

voice_PreSmo<- read.csv('voice_PreSmoke.csv')
voice_PrePos<- read.csv('voice_PrePost.csv')

nlp_PreSmo<- read.csv('nlp_PreSmoke.csv')
nlp_PrePos <- read.csv('nlp_PrePost.csv')

## Code for general linear mixed models & model residuals (use this for all variables in 'duration' and 'voice' data files; for jitter only, use "log(jit)" to log-transform this variable)
mod.f0.PreSmo = lmer(f0 ~ Period + (1 | Male.ID), data = voice_PreSmo)
summary(mod.f0.PreSmo)
plot(mod.f0.PreSmo)  
require(DHARMa)
fittedModel=mod.shim.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(lmer(f0 ~ Period + (1 | Male.ID), data = voice_PreSmo))

## Code for generalized linear mixed models (Poisson, log) & model residuals (use this for 'Ncall' (# long calls) data files)
mod.NLC.PreSmo = glmer(N.LC ~ Period + (1 | Male.ID), family = poisson(link = "log"), data = Ncall_PreSmo)
summary(mod.NLC.PreSmo)
plot(mod.NLC.PreSmo)  
require(DHARMa)
fittedModel=mod.NLC.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(glmer(N.LC ~ Period + (1 | Male.ID), family = poisson(link = "log"), data = Ncall_PreSmo))

## Code for generalized linear mixed models (binomial, logit) & model residuals (use this for variables in 'nlp' data files)
mod.bp.PreSmo = glmer(BP.01 ~ Period + (1 | Male.ID), family = binomial(link = "logit"),  data = nlp_PreSmo)
summary(mod.bp.PreSmo)
plot(mod.bp.PreSmo)  
require(DHARMa)
fittedModel=mod.bp.PreSmo
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)
MuMIn::r.squaredGLMM(glmer(BP.01 ~ Period + (1 | Male.ID), family = binomial(link = "logit"),  data = nlp_PreSmo))

## Code for jittered boxplots
nlc.smoke <- ggplot(PreSmo_Ncall, aes(x = Period, y = N.LC)) +
  geom_boxplot(varwidth = FALSE, outlier.color = NA) +
  scale_x_discrete(labels = c("Pre-Smoke","Smoke")) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0) +
  theme_classic() +
  labs(x = "", y = "N calls") 
nlc.smoke
