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
nStaircasesBefore - nStaircasesAfter
100*(nStaircasesBefore - nStaircasesAfter)/nStaircasesBefore


MotionEstimation = MotionEstimation %>% 
  mutate(nOutsideOfRange = (abs(RadiansToDegree(xRotations_first5)) > 2.5) + (abs(RadiansToDegree(xRotations_first1)) > 2.5) + 
           (abs(RadiansToDegree(xRotations_first2)) > 2.5) + (abs(RadiansToDegree(xRotations_first3)) > 2.5) + 
           (abs(RadiansToDegree(xRotations_first4)) > 2.5) +
           (abs(RadiansToDegree(yRotations_first5)) > 2.5) + (abs(RadiansToDegree(yRotations_first1)) > 2.5) + 
           (abs(RadiansToDegree(yRotations_first2)) > 2.5) + (abs(RadiansToDegree(yRotations_first3)) > 2.5) + 
           (abs(RadiansToDegree(yRotations_first4)) > 2.5) +
           (abs(RadiansToDegree(xRotations_second5)) > 2.5) + (abs(RadiansToDegree(xRotations_second1)) > 2.5) + 
           (abs(RadiansToDegree(xRotations_second2)) > 2.5) + (abs(RadiansToDegree(xRotations_second3)) > 2.5) + 
           (abs(RadiansToDegree(xRotations_second4)) > 2.5) +
           (abs(RadiansToDegree(yRotations_second5)) > 2.5) + (abs(RadiansToDegree(yRotations_second1)) > 2.5) + 
           (abs(RadiansToDegree(yRotations_second2)) > 2.5) + (abs(RadiansToDegree(yRotations_second3)) > 2.5) + 
           (abs(RadiansToDegree(yRotations_second4)) > 2.5))

nPostOutliers = length(MotionEstimation$velH)
MotionEstimation = MotionEstimation %>% filter(nOutsideOfRange < 10)
nPostOutliers2 = length(MotionEstimation$velH)
nPostOutliers - nPostOutliers2
100*(nPostOutliers-nPostOutliers2)/nPostOutliers
###outlier analysis end


# Parameters = quickpsy::quickpsy(MotionEstimation,velH_Pest,Response_MotionEstimation,
#                                 grouping = .(subject,Congruent,velH),
#                                 bootstrap = "none")$par
# 
# Parameters2 = Parameters %>%
#   filter(parn == "p1") %>%
#   select(subject,Congruent,Mean=par, velH)
# Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
# FittedPsychometricFunctions_Analysis = Parameters2
# 
# save(FittedPsychometricFunctions_Analysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                          "/SavedVariables/FittedPsychometricFunctions_Analysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/FittedPsychometricFunctions_Analysis.RData"))

#####stats tests for motion estimation
LMM_Mean = lmer(Mean ~ Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis)
# LMM_Mean_CI = confint(LMM_Mean,method = "boot")
# save(LMM_Mean_CI, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                             "/SavedVariables/LMM_Mean_CI.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/LMM_Mean_CI.RData"))

#outliers for precision
sum(FittedPsychometricFunctions_Analysis$SD < 0.01)
mean(FittedPsychometricFunctions_Analysis$SD < 0.01)

LMM_SD_Opposite_Test = lmer(log(SD) ~ Mean + Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Same Direction" & SD > 0.01))
LMM_SD_Opposite_Null = lmer(log(SD) ~ Mean + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Same Direction" & SD > 0.01))
anova(LMM_SD_Opposite_Test,LMM_SD_Opposite_Null)
summary(LMM_SD_Opposite_Test)

LMM_SD_Same_Test = lmer(log(SD) ~ Mean + Congruent + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Opposite Directions" & SD > 0.01))
LMM_SD_Same_Null = lmer(log(SD) ~ Mean + (velH | subject),
                data = FittedPsychometricFunctions_Analysis %>% filter(Congruent != "Opposite Directions" & SD > 0.01))
anova(LMM_SD_Same_Test,LMM_SD_Same_Null)
summary(LMM_SD_Same_Test)

# Actual Data Plots (Figure 08)
Figure_Speed_PSEs = ggplot(FittedPsychometricFunctions_Analysis %>% mutate(velH_Factor = paste0(velH," m/s")) %>% 
                             mutate(Congruent2 = case_when(
                               Congruent == "No Motion" ~ "1Observer Static",
                               Congruent == "Same Direction" ~ "2Same Direction",
                               Congruent == "Opposite Directions" ~ "3Opposite Directions")),
                                  aes(velH_Factor,Mean,color = Congruent2)) +
  geom_boxplot(size = 1.5) +
  scale_color_manual(name = "Motion Profile", values = c("red","blue","orange")) +
  xlab("Motion Profile") +
  ylab("PSE (m/s)") +
  scale_x_discrete(name = "")  +
  theme(legend.position = "none")  +
  ggtitle("A.")

Figure_Speed_SD = ggplot(FittedPsychometricFunctions_Analysis %>% mutate(velH_Factor = paste0(velH," m/s")) %>% 
                           mutate(Congruent2 = case_when(
                             Congruent == "No Motion" ~ "1Observer Static",
                             Congruent == "Same Direction" ~ "2Same Direction",
                             Congruent == "Opposite Directions" ~ "3Opposite Directions")),
                         aes(Mean,SD,color = Congruent2)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(method = "lm",
              formula = y ~ x, se = FALSE) +
  xlab("PSE (m/s)") + 
  ylab("JND (m/s)") +
  scale_color_manual(name = "Motion Profile", values = c("red","blue","orange"), labels = c("Observer Static","Same Direction","Opposite Direction"))  +
  ggtitle("B.")

plot_grid(Figure_Speed_PSEs,Figure_Speed_SD, rel_widths = c(0.8,1.1))
ggsave("Figures/(Figure 08) Data Hypotheses 2a and 2b (Speed).jpg", w = 12, h = 6)
