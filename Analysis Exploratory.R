require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)
source("Analysis Speed Estimation.r")
source("Analysis Prediction.r")

# Additional Exploratory Analyses

FittedPsychometricFunctions_Analysis = FittedPsychometricFunctions_Analysis %>% 
  mutate(RetSpeed = case_when(
    Congruent == "No Motion" & velH == 4 ~ 22.2,
    Congruent == "No Motion" & velH == 5 ~ 28.4,
    Congruent == "No Motion" & velH == 6 ~ 34,
    Congruent == "Same Direction" & velH == 4 ~ 0.4,
    Congruent == "Same Direction" & velH == 5 ~ 5.9,
    Congruent == "Same Direction" & velH == 6 ~ 11.6,
    Congruent == "Opposite Directions" & velH == 4 ~ 44.4,
    Congruent == "Opposite Directions" & velH == 5 ~ 50.4,
    Congruent == "Opposite Directions" & velH == 6 ~ 55.8))

Prediction = Prediction %>% 
  mutate(RetSpeed = case_when(
    Congruent == "No Motion" & abs(velH) == 4 ~ 22.2,
    Congruent == "No Motion" & abs(velH) == 4.75 ~ 28.4,
    Congruent == "No Motion" & abs(velH) == 5.5 ~ 34,
    Congruent == "Same Direction" & abs(velH) == 4 ~ 0.4,
    Congruent == "Same Direction" & abs(velH) == 4.75 ~ 5.9,
    Congruent == "Same Direction" & abs(velH) == 5.5 ~ 11.6,
    Congruent == "Opposite Directions" & abs(velH) == 4 ~ 44.4,
    Congruent == "Opposite Directions" & abs(velH) == 4.75 ~ 50.4,
    Congruent == "Opposite Directions" & abs(velH) == 5.5 ~ 55.8))

Prediction_SDs = Prediction_SDs %>% 
  mutate(RetSpeed = case_when(
    Congruent == "No Motion" & abs(velH) == 4 ~ 22.2,
    Congruent == "No Motion" & abs(velH) == 4.75 ~ 28.4,
    Congruent == "No Motion" & abs(velH) == 5.5 ~ 34,
    Congruent == "Same Direction" & abs(velH) == 4 ~ 0.4,
    Congruent == "Same Direction" & abs(velH) == 4.75 ~ 5.9,
    Congruent == "Same Direction" & abs(velH) == 5.5 ~ 11.6,
    Congruent == "Opposite Directions" & abs(velH) == 4 ~ 44.4,
    Congruent == "Opposite Directions" & abs(velH) == 4.75 ~ 50.4,
    Congruent == "Opposite Directions" & abs(velH) == 5.5 ~ 55.8))