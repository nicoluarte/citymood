library(pacman)
p_load(tidyverse, purrr, dplyr, caret, lmtest, rstan, boot, finalfit, pROC, ggplot2, glmperm)
setwd('/home/nicoluarte/citymood')
data <- read.csv('dataProc.csv')
head(data)

## caret model noise
## train model
mod0 <- train(Valence ~ Noise, data = data,
              method = "glm",
              metric = "ROC",
              family = "binomial",
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary,
                                       savePredictions = "all"))
## get optimal thresholds
mdl0Thresholds <- thresholder(mod0, threshold = seq(0, 1, by = 0.01), final = TRUE)
##plot them
## ggplot(mdl0Thresholds, aes(x = prob_threshold, y = Kappa)) + geom_point()
## ggplot(mdl0Thresholds, aes(x = prob_threshold, y = Sensitivity)) +
##     geom_line() +
##     geom_line(aes(y = Specificity), col = "red")
## get confusion matrix with optimal thresholds, based on kappa
## get minimal Kappa
maxKappaMdl0 <- mdl0Thresholds[which.max(mdl0Thresholds[, "Kappa"]), ]

## caret model noise
## train model
mod1 <- train(Valence ~ Socioeconomic + Socioeconomic:Noise, data = data,
              method = "glm",
              metric = "ROC",
              family = "binomial",
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary,
                                       savePredictions = "all"))
mod1$finalModel
## get optimal thresholds
mdl1Thresholds <- thresholder(mod1, threshold = seq(0, 1, by = 0.01), final = TRUE)
##plot them
## ggplot(mdl0Thresholds, aes(x = prob_threshold, y = Kappa)) + geom_point()
## ggplot(mdl0Thresholds, aes(x = prob_threshold, y = Sensitivity)) +
##     geom_line() +
##     geom_line(aes(y = Specificity), col = "red")
## get confusion matrix with optimal thresholds, based on kappa
## get minimal Kappa
maxKappaMdl1 <- mdl1Thresholds[which.max(mdl1Thresholds[, "Kappa"]), ]

## permutations test model 0
dataNum <- read.csv("data.csv")
model0 <- glm(Valence ~ Noise,
              data = data,
              family = binomial())
model1 <- glm(Valence ~ Socioeconomic + Socioeconomic:Noise,
              data = data,
              family = binomial())
pTestModel0 <- prr.test(Valence~Noise,
                        "Noise",
                        data = data,
                        family = binomial,
                        nrep = 10000)
pTestModel1Main <- prr.test(Valence~Socioeconomic_num + Socioeconomic_num:Noise,
                            "Socioeconomic_num:Noise",
                            data = dataNum,
                            family = binomial,
                            nrep = 10000)
pTestModel1Interaction <- prr.test(Valence~Socioeconomic_num + Socioeconomic_num:Noise,
                                   "Socioeconomic_num:Noise",
                                   data = dataNum,
                                   family = binomial,
                                   nrep = 10000)


