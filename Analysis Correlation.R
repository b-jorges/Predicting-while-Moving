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



####Pre-work to get the values needed for correlation stuff
FittedPsychometricFunctions_Corr = FittedPsychometricFunctions_Analysis %>% 
  group_by(subject) %>%
  ###make new columns where we get the mean PSEs/JNDs for the perceived speed (speed estimation task) for each condition and participant
  mutate(PSE_Opposite = mean(Mean[Congruent == "Opposite Directions"], na.rm = TRUE),
         SD_Opposite = mean(SD[Congruent == "Opposite Directions"], na.rm = TRUE),
         PSE_Same = mean(Mean[Congruent == "Same Direction"], na.rm = TRUE),
         SD_Same = mean(SD[Congruent == "Same Direction"], na.rm = TRUE),
         PSE_Static = mean(Mean[Congruent == "No Motion"], na.rm = TRUE),
         SD_Static = mean(SD[Congruent == "No Motion"], na.rm = TRUE)) %>% 
  slice(1)

#make yet more columns where we get the difference between the Opposite/Same and the Static condition in terms of the means and standard deviations computed above
FittedPsychometricFunctions_Corr = FittedPsychometricFunctions_Corr %>% 
  mutate(Diff_PSE_Opposite = PSE_Opposite-PSE_Static,
         Diff_PSE_Same = PSE_Same-PSE_Static,
         Diff_SD_Opposite = SD_Opposite-SD_Static,
         Diff_SD_Same = SD_Same-SD_Static)

Correlations = Prediction %>%
  group_by(ID) %>%
  filter(Occlusion_Duration %in% c(0.5,0.6,0.7)) %>% 
  mutate(###make new columns where we get the mean/SD of the perceived duration for each condition and participant
    MeanExtrapolatedDuration_Opposite = mean(Response_Time[Congruent == "Opposite Directions"]),
    SDExtrapolatedDuration_Opposite = sd(Response_Time[Congruent == "Opposite Directions"]),
    MeanExtrapolatedDuration_Static = mean(Response_Time[Congruent == "Observer Static"]),
    SDExtrapolatedDuration_Static = sd(Response_Time[Congruent == "Observer Static"]),
    MeanExtrapolatedDuration_Same = mean(Response_Time[Congruent == "Same Direction"]),
    SDExtrapolatedDuration_Same = sd(Response_Time[Congruent == "Same Direction"])) %>%
  slice(1)

Correlations = Correlations %>%
  #make yet more columns where we get the difference between the Opposite/Same and the Static condition in terms of the means and standard deviations computed above
  mutate(DifMeanExtrapolatedMotion_Opposite = MeanExtrapolatedDuration_Opposite-MeanExtrapolatedDuration_Static,
         DifMeanExtrapolatedMotion_Same = MeanExtrapolatedDuration_Same-MeanExtrapolatedDuration_Static,
         DifSDExtrapolatedMotion_Opposite = SDExtrapolatedDuration_Opposite-SDExtrapolatedDuration_Static,
         DifSDExtrapolatedMotion_Same = SDExtrapolatedDuration_Same-SDExtrapolatedDuration_Static)

###match values from the speed estimation task into the dataframe with the prediction values, for each condition and participant
for(i in 1:length(FittedPsychometricFunctions_Corr$subject)){
  
  Diff_PSE_Opposite = FittedPsychometricFunctions_Corr$Diff_PSE_Opposite[i]
  Diff_PSE_Same = FittedPsychometricFunctions_Corr$Diff_PSE_Same[i]
  Diff_SD_Opposite = FittedPsychometricFunctions_Corr$Diff_SD_Opposite[i]
  Diff_SD_Same = FittedPsychometricFunctions_Corr$Diff_SD_Same[i]
  
  ID = FittedPsychometricFunctions_Corr$subject[i]
    
  Correlations$Diff_PSE_Opposite[Correlations$ID == ID] = Diff_PSE_Opposite
  Correlations$Diff_PSE_Same[Correlations$ID == ID] = Diff_PSE_Same
  Correlations$Diff_SD_Opposite[Correlations$ID == ID] = Diff_SD_Opposite
  Correlations$Diff_SD_Same[Correlations$ID == ID] = Diff_SD_Same
}

####Statistical tests for correlations
####Statistical tests for correlations
#for opposite direction
#accuracy
PSE_LMM_Corr_Opposite = lm(DifMeanExtrapolatedMotion_Opposite ~  Diff_PSE_Opposite,
                             data =  Correlations)
confint(PSE_LMM_Corr_Opposite)
summary(PSE_LMM_Corr_Opposite)


#precision
SD_LMM_Corr_Opposite_Test = lm(DifSDExtrapolatedMotion_Opposite ~  DifMeanExtrapolatedMotion_Opposite + Diff_SD_Opposite,
                                 data =  Correlations %>% filter(!is.na(Diff_SD_Opposite)))
SD_LMM_Corr_Opposite_Null = lm(DifSDExtrapolatedMotion_Opposite ~  DifMeanExtrapolatedMotion_Opposite,
                                 data =  Correlations %>% filter(!is.na(Diff_SD_Opposite)))
anova(SD_LMM_Corr_Opposite_Test,SD_LMM_Corr_Opposite_Null)
summary(SD_LMM_Corr_Opposite_Test)

#for same direction
#accuracy
PSE_LMM_Corr_Same = lm(DifMeanExtrapolatedMotion_Same ~  Diff_PSE_Same,
                         data =  Correlations)
summary(PSE_LMM_Corr_Same)

#precision
SD_LMM_Corr_Same_Test = lm(DifSDExtrapolatedMotion_Same ~  DifMeanExtrapolatedMotion_Same + Diff_SD_Same,
                             data =  Correlations %>% filter(!is.na(Diff_SD_Same)))
SD_LMM_Corr_Same_Null = lm(DifSDExtrapolatedMotion_Same ~  DifMeanExtrapolatedMotion_Same,
                             data =  Correlations %>% filter(!is.na(Diff_SD_Same)))
anova(SD_LMM_Corr_Same_Test,SD_LMM_Corr_Same_Null)

#Plots (Figure 9)
Figure_Correlations_PSE_Oppo = ggplot(Correlations %>% 
                                            mutate(velH_Factor = paste0(velH," m/s")),
                                          aes(Diff_PSE_Opposite,DifMeanExtrapolatedMotion_Opposite)) +
  geom_point(alpha = 1, 
             size = 4) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 3)  +
  xlab(expression("PSE"["Opposite"]*" - PSE"["Static"]*" (m/s)")) +
  ylab(expression("Extrapolated Duration"["Opposite"]*" - Extrapolated Duration"["Static"]*" (m/s)")) +
  theme(legend.position = c(0.65,0.8)) +
  ggtitle("A. Opposite Directions – Accuracy")

Figure_Correlations_SD_Oppo = ggplot(Correlations %>% 
                                            mutate(velH_Factor = paste0(velH," m/s")),
                                          aes(Diff_SD_Opposite,DifSDExtrapolatedMotion_Opposite)) +
  geom_point(alpha = 1, 
             size = 4) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 3)  +
  xlab(expression("SD of PF"["Opposite"]*" - SD of PF"["Static"]*" (m/s)")) +
  ylab(expression("SD Extrapolated Duration"["Opposite"]*" - SD Extrapolated Duration"["Static"]*" (m/s)")) +
  ggtitle("B.") +
  theme(axis.title=element_text(size=13.5,face="bold"),
        legend.position = "none") +
  ggtitle("B. Opposite Directions – Variability")

Figure_Correlations_PSE_Same = ggplot(Correlations %>% 
                                        mutate(velH_Factor = paste0(velH," m/s")),
                                      aes(Diff_PSE_Same,DifMeanExtrapolatedMotion_Same)) +
  geom_point(alpha = 1, 
             size = 4) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 3)  +
  xlab(expression("PSE"["Opposite"]*" - PSE"["Static"]*" (m/s)")) +
  ylab(expression("Extrapolated Duration"["Opposite"]*" - Extrapolated Duration"["Static"]*" (m/s)")) +
  theme(legend.position = c(0.65,0.8)) +
  ggtitle("C. Same Direction – Accuracy")

Figure_Correlations_SD_Same = ggplot(Correlations %>% 
                                       mutate(velH_Factor = paste0(velH," m/s")),
                                     aes(Diff_SD_Same,DifSDExtrapolatedMotion_Same)) +
  geom_point(alpha = 1, 
             size = 4) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 3)  +
  xlab(expression("SD of PF"["Opposite"]*" - SD of PF"["Static"]*" (m/s)")) +
  ylab(expression("SD Extrapolated Duration"["Opposite"]*" - SD Extrapolated Duration"["Static"]*" (m/s)")) +
  theme(axis.title=element_text(size=13.5,face="bold"),
        legend.position = "none") +
  ggtitle("D. Same Direction – Precision")

plot_grid(Figure_Correlations_PSE_Oppo,Figure_Correlations_SD_Oppo,
          Figure_Correlations_PSE_Same,Figure_Correlations_SD_Same)
ggsave("Figures/(Figure 9) Data Hypotheses 3a and 3b (Correlation).jpg", w = 12, h = 12)