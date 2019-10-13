library(pacman)
p_load(tidyverse, purrr, dplyr, fastDummies)
setwd('/home/nicoluarte/citymood')
data <- read.csv('data.csv')
head(data)
data$Sex_num[data$Sex_num == 1] <- "Female"
data$Sex_num[data$Sex_num == 0] <- "Male"
dataToProcess <- subset(data, select = -c(Pupil,
                                          Socioeconomic_num,
                                          Valence_num,
                                          Cars,
                                          Neighbourhood_num,
                                          Pedestrians))
names(dataToProcess)[names(dataToProcess) == "Sex_num"] <- "Sex"
write.csv(dataToProcess, "dataProc.csv", row.names = FALSE)
