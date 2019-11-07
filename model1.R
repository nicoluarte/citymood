library(pacman)
p_load(tidyverse, purrr, dplyr, caret, lmtest, rstan,
       boot, finalfit, pROC, ggplot2, glmperm,
       ggpubr, effects, gridExtra)
setwd('/home/nicoluarte/citymood')
data <- read.csv('dataProc.csv')
head(data)

## caret model noise
## train model
dataNum <- read.csv("data.csv")
mod0 <- train(Valence ~ Noise, data = data,
              method = "glm",
              metric = "ROC",
              family = "binomial",
              trControl = trainControl(method = "repeatedcv",
                                       repeats = 5,
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary,
                                       savePredictions = "all"))

                                        # R regression object
## get optimal thresholds
logModel0 <- glm(Valence ~ Noise_num, data = dataNum,
                 family = binomial())
summary(logModel0)
## get points
noiseValues <- seq(0, 1, 0.01)
logModel0Predict <- predict(logModel0, list(Noise = noiseValues),type="response")

plot(data$Noise, data$Valence)
plot(noiseValues, logModel0Predict)

tiff("/home/nicoluarte/citymood/noisePlot.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
ggplot(dataNum, aes(x = Noise, y = Valence_num)) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x,
                alpha = 0.2, size = 0.5, color = "black") +
    geom_point(position = position_jitter(height = 0.03, width = 0), aes(color = factor(Valence_num,
                                                                                        labels = c("Postive", "Negative"))), alpha = 1/5) +
    xlab("Noise") + ylab("Pr (positve mood)")  +
    labs(color = "Mood") +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    theme_pubr()
dev.off()

## interaction plot
byIncomeLow <- filter(dataNum, Socioeconomic == "Low")
byIncomeMiddle <- filter(dataNum, Socioeconomic == "Middle")
byIncomeHigh <- filter(dataNum, Socioeconomic == "High")

tiff("/home/nicoluarte/citymood/interaction.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
low <- ggplot(byIncomeLow, aes(x = Noise, y = Valence_num)) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x,
                alpha = 0.2, size = 0.5, color = "black") +
    geom_point(position = position_jitter(height = 0.03, width = 0), aes(color = factor(Valence_num,
                                                                                        labels = c("Postive", "Negative"))), alpha = 1/5) +
    xlab("Noise") + ylab("Pr (positve mood)")  +
    labs(color = "Mood") +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    ggtitle("Low income") +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
middle <- ggplot(byIncomeMiddle, aes(x = Noise, y = Valence_num)) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x,
                alpha = 0.2, size = 0.5, color = "black") +
    geom_point(position = position_jitter(height = 0.03, width = 0), aes(color = factor(Valence_num,
                                                                                        labels = c("Postive", "Negative"))), alpha = 1/5) +
    xlab("Noise") + ylab("Pr (positve mood)")  +
    labs(color = "Mood") +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    ggtitle("Middle income") +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
high <- ggplot(byIncomeHigh, aes(x = Noise, y = Valence_num)) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x,
                alpha = 0.2, size = 0.5, color = "black") +
    geom_point(position = position_jitter(height = 0.03, width = 0), aes(color = factor(Valence_num,
                                                                                        labels = c("Postive", "Negative"))), alpha = 1/5) +
    xlab("Noise") + ylab("Pr (positve mood)")  +
    labs(color = "Mood") +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    ggtitle("High income") +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
grid.arrange(low, middle, high, nrow = 1)
dev.off()

plot(allEffects(logModel0))


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

socio <- prr.test(Valence~Socioeconomic_num,
                  "Socioeconomic_num",
                  data = dataNum,
                  family = binomial,
                  nrep = 100000)
summary(socio)

summary(pTestModel1Main)

## overall socioeconomic

nullModel <- glm(Valence ~ 0,
              data = data,
              family = binomial())
Fullmodel1 <- glm(Valence ~ Socioeconomic + 0,
                 data = data,
                 family = binomial())
summary(Fullmodel1)

anova(nullModel, Fullmodel1, test = "LRT")
anova(nullModel, Fullmodel1)

## other permutation test
set.seed(101)
n <- 1000
res <- numeric(n)
for (i in 1:n){
    perm <- sample(nrow(data))
    bdat <- transform(data, Valence = Valence[perm])
    res[i] <- glm(Valence ~ Socioeconomic + 0,
                  data = bdat,
                  family = binomial())$coefficients[1]
}

hist(res)
abline(v = Fullmodel1$coefficients[1], col="red")

logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}

logit2prob(coef(Fullmodel1))
summary(Fullmodel1)

waldtest(Fullmodel1)
