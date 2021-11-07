require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)
require(brms)

set.seed(4)

####Pilot data prediction
MotionEstimation = read.csv("Data/4_SpeedEstimation/Data_SpeedEstimation.csv")

MotionEstimation = MotionEstimation %>% mutate(Congruent = case_when(velH*velH_Subject > 0 ~ "Same Direction",
                                                         velH*velH_Subject < 0 ~ "Opposite Directions",
                                                         velH*velH_Subject == 0 ~ "No Motion"))
###outlier analysis:
MotionEstimation = MotionEstimation %>% 
  group_by(subject,velH,velH_Subject) %>% 
  mutate(OutlierTrial = case_when(
                abs(velH_Pest) >= 3*abs(velH) | abs(velH_Pest) <= 0.33*abs(velH) ~ 1,
                TRUE ~ 0),
    NumberOutlierLengths = sum(abs(velH_Pest) >= 3*abs(velH) | abs(velH_Pest) <= 0.33*abs(velH))) %>% 
  mutate(Outlier_Condition = case_when(NumberOutlierLengths > 5 ~ "Outlier",
                                 TRUE ~ "No Outlier"))

nStaircasesBefore = length((MotionEstimation %>% 
                              group_by(subject,velH,velH_Subject) %>% 
                              slice(1))$subject)
nStaircasesBefore

InspectOutliers = MotionEstimation %>% 
  group_by(subject,velH,velH_Subject) %>% 
  slice(1)

MotionEstimation = MotionEstimation %>%
  filter(Outlier_Condition == "No Outlier")

nStaircasesAfter = length((MotionEstimation %>% 
                             group_by(subject,velH,velH_Subject) %>% 
                             slice(1))$subject)
nStaircasesAfter
###outlier analysis end


Parameters = quickpsy::quickpsy(MotionEstimation,velH_Pest,Response_MotionEstimation,
                                grouping = .(subject,Congruent,velH),
                                bootstrap = "none")$par

Parameters2 = Parameters %>%
  filter(parn == "p1") %>%
  select(subject,Congruent,Mean=par, velH)
Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
FittedPsychometricFunctions_Analysis = Parameters2

FittedPsychometricFunctions_Analysis = FittedPsychometricFunctions_Analysis %>% 
  mutate(RetinalSpeed = case_when(
    Congruent == "Same Direction" & velH == 4 ~ 21.5,
    Congruent == "Same Direction" & velH == 5 ~ 23.3,
    Congruent == "Same Direction" & velH == 6 ~ 25.5,
    Congruent == "Opposite Directions" & velH == 4 ~ 42.9,
    Congruent == "Opposite Directions" & velH == 5 ~ 48.5,
    Congruent == "Opposite Directions" & velH == 6 ~ 53.7 ,
    Congruent == "No Motion" & velH == 4 ~ 22.8,
    Congruent == "No Motion" & velH == 5 ~ 28.5,
    Congruent == "No Motion" & velH == 6 ~ 34.0
  ))


#####stats tests for motion estimation
LMM_Mean = lmer(Mean ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis)
LMM_Mean_CI = confint(LMM_Mean,method = "boot")

LMM_SD_Opposite_Test = lmer(log(SD) ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Same Direction"))
LMM_SD_Opposite_Null = lmer(log(SD) ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Same Direction"))
anova(LMM_SD_Opposite_Test,LMM_SD_Opposite_Null)

LMM_SD_Same_Test = lmer(log(SD) ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Opposite Directions"))
LMM_SD_Same_Null = lmer(log(SD) ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Opposite Directions"))
anova(LMM_SD_Same_Test,LMM_SD_Same_Null)

save(FittedPsychometricFunctions_Analysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                            "/SavedVariables/FittedPsychometricFunctions_Analysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/FittedPsychometricFunctions_Analysis.RData"))
save(LMM_Mean_CI, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                            "/SavedVariables/LMM_Mean_CI.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/LMM_Mean_CI.RData"))
save(LMM_SD_CI, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                "/SavedVariables/LMM_SD_CI.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/LMM_SD_CI.RData"))