---
title: "Environmental noise is differently associated with negative and positive urban experience: an exploratory first-person mobile study in Santiago de Chile"
author: Ismael Palacios-García, Nicolás Luarte, Geraldine Herrmann & Francisco Parada
date: 22 de Octubre
output: 
    beamer_presentation:
theme: "Boadilla"
---

## Background

- Specific city features related to the etiology of stress include neighborhood
  quality, the amount of greenspace, industrial activity, and traffic volume
  [^2] 
- A relevant and measurable stressor emerging from cities is environmental noise
  or sonic pollution[^3] 
  
## Research question
- How first-person experience of the urban environment in presence of
  environmental noise is modulated by different socioeconomic/demographic
  factors
![Eye tracker](/home/nicoluarte/citymood/tobii.jpg)  
 
## Noise has an impact on health

- Induces hearing loss[^4] 
- Sleep disturbances, increased nor-adrenaline and cortisol levels, perceived stress augmentation, and higher cardiovascular risk, among others[^5] 
- Others ...

## Effects of noise might be modulated by socioeconomic and topographic factors

- Evidence suggests that **socio-demographic factors mediate as risk and
  protections variables** [^6] 
- From different stressors measured in 14 low socioeconomic-level neighborhoods
  among different communities in New York City, environmental noise was only
  considered a stressor in 4 neighborhoods and restricted only to specific city
  places [^7]. In contrast, traffic noise
  had no relation to socioeconomic differences in London[^8] 
- Socioeconomic modulation is not clear

## Socioeconomic effects might be accentuated in Chile
- Chile leads the ranking of most unequal countries in the world
- Santiago has one of the highest environmental noise levels
- Socioeconomic factors impact on stress and mental healt remains unknown

## Natural walking behavior task
- Preguntas?

## Data processing
- Environmental noise data was extracted from video recordings, and normalized
  to obtain a comparable metric between participants
- 'Positive' and 'negative' moods were assigned to a 4 second time window
  centered at the begginning of each reported event
- A mean noise level was computed per window

## Data analysis
- Two logistic models were fitted to the data
- Model 1: noise was used as predictor of mood
- Model 2: interaction of noise and socioeconomic status was used as a predictor
  of mood
  
## Logistic regression
- Predicts a binary outcome variable
- Is a classifier
- Basically a linear regression, but with log odd ratios
- $Odds ratio = \frac{Good mood | High noise}{Good mood | Low noise}$
- $logit(Y) = natural-log(odds) = ln(\frac{Pr}{1 - Pr}) = \alpha + \beta X$
- $Pr = Probability (Y = outcome \quad of \quad interest | X = x, specific \quad
value \quad of \quad X) = \frac{\exp(\alpha + \beta x)}{1 + \exp(\alpha + \beta
x)}$

## Fitting the model
- Fitted logistic regression
- Wald's test for coefficient significance
- Likelihood ratio for model comparison against null
- Kappa & accuracy for model performance
- K-fold cross validation
![K-fold cross validation](/home/nicoluarte/citymood/kfold.svg "K-fold cross validation")

## Results
- Both models are significant
- Noise seems to be a "good" predictor of mood (Kappa = 0.21)
- Model 2, as classifier, improves upon model 1, (Kappa = 0.26)
- Socioeconomic Mid and Low have significant interactions with noise, when
  predicting for mood
  
## Results
![Model 1](/home/nicoluarte/citymood/noisePlot.png)

## Results
![Model 2](/home/nicoluarte/citymood/interaction.png)

## Discusion
- Is it noise related to safety?
- Accesibility to urban benefits?
- More green-space in low income areas?
- There seems to be a clear general effect of noise 
- Interaction effect of noise, however, is less clear
  
[^2]: (Gong et al., 2016)

[^3]: (Pedersen, 2015)

[^4]: (Kryter, 1994; Nandi & Dhatrak, 2008)

[^5]: (Barbaresco, Reis, Lopes, Boaventura, Castro, Vilanova, Da Cunha Júnior, Pires, Pôrto Filho, & Pereira, 2019; Münzel et al., 2018; S. A. Stansfeld & M. P. Matheson, 2003; Tonne et al., 2016)

[^6]: (El-Gilany, Amr, & Hammad, 2008; Klein & Forehand, 2000; Lederbogen et al., 2011; Lindencrona, Ekblad, & Hauff, 2008; Reynolds, O’Koon, Papademetriou, Szczygiel, & Grant, 2001)

[^7]: (Shmool et al., 2014; Shmool et al., 2015)

[^8]: (Tonne et al., 2018)
