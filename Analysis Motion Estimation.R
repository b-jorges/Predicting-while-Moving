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
MotionEstimation = read.csv("Data/3_MotionEstimation/Data_MotionEstimation.csv")

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

#####stats tests for motion estimation
LMM_Mean = lmer(Mean ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis)
LMM_Mean_CI = confint(LMM_Mean,method = "boot")


LMM_SD = lmer(SD ~ Congruent + (velH | subject),
              data = FittedPsychometricFunctions_Analysis)
LMM_SD_CI = confint(LMM_SD,method = "boot")


save(FittedPsychometricFunctions_Analysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                            "/SavedVariables/FittedPsychometricFunctions_Analysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/FittedPsychometricFunctions_Analysis.RData"))
save(LMM_Mean_CI, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                            "/SavedVariables/LMM_Mean_CI.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/LMM_Mean_CI.RData"))
save(LMM_SD_CI, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                            "/SavedVariables/LMM_SD_CI.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/LMM_SD_CI.RData"))

paste0("For the LMM fitted to test the impact of visually simulated self-motion on PSEs, we found an intercept of ",
       round(summary(LMM_Mean)$coef[1],2),". The regression coefficient for Motion Profile: Opposite Directions was ", round(summary(LMM_Mean)$coef[3],2),
       " (95% CI = [", round(LMM_Mean_CI["CongruentOpposite Directions",1],2),";",round(LMM_Mean_CI["CongruentOpposite Directions",2],2),
       "]). For the regression coefficient corresponding to  Motion Profile: Same Direction, we found a value of ", round(summary(LMM_Mean)$coef[3],2),
       " (95% CI = [", round(LMM_Mean_CI["CongruentSame Direction",1],2),";",round(LMM_Mean_CI["CongruentSame Direction",2],2),"]).")

paste0("For the LMM fitted to test the impact of visually simulated self-motion on PSEs, we found an intercept of ",
       round(summary(LMM_SD)$coef[1],2),". The regression coefficient for Motion Profile: Opposite Directions was ", round(summary(LMM_SD)$coef[3],2),
       " (95% CI = [", round(LMM_SD_CI["CongruentOpposite Directions",1],2),";",round(LMM_SD_CI["CongruentOpposite Directions",2],2),
       "]). For the regression coefficient corresponding to  Motion Profile: Same Direction, we found a value of ", round(summary(LMM_SD)$coef[3],2),
       " (95% CI = [", round(LMM_SD_CI["CongruentSame Direction",1],2),";",round(LMM_SD_CI["CongruentSame Direction",2],2),"]).")