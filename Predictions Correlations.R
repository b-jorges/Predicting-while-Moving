require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(quickpsy)
require(purrr)
source("Utilities/parabolic.r")
require(cowplot)
theme_set(theme_cowplot())

Participants = paste0("p",1:40)

velH = c(4, 5, 6) #m/s
self_velH = c(-3.6,0,3.6) #meters over half a second, = 3.6m/s on average, but Gaussian motion profile
occlusion_times = c(0.5,0.6,0.7)

reps = 1:13
reps_MotionEstimation = 70


###Get variability factors for each participant, between-participant variability:
ParticipantVariability = data.frame(ID = Participants,
                                    Bias_ID = rnorm(length(Participants),0.2,0.3), #get self-motion bias for each participant
                                    Variability_ID = rnorm(length(Participants),0.2,0.3), #get self-motion variability for each participant
                                    PSE_ID = 1,#rnorm(length(Participants),1,0.15), #we assume that people estimate velocity accuretely on average
                                    SD_ID = rnorm(length(Participants),1,0.15)) #multiplicator for the


###########with variability
#trials per participant:
#NumberTrials = length(s)*length(occlusion_times)*length(self_velH)*length(velH)

#duration (3s per trial)
#3*NumberTrials/60 #minutes

#Weber Fraction for distance estimation is about 5%
WFtoSD(0.05)

Predictions = expand.grid(velH = velH,
                          self_velH = self_velH,
                          occlusion_time = occlusion_times,
                          Participant = Participants,
                          rep = reps)

Occlusion_Times_SDControl = c(seq(0.1,0.4,0.1),seq(0.8,1,0.1))

SD_Control = expand.grid(velH = velH,
                         self_velH = 0,
                         occlusion_time = Occlusion_Times_SDControl,
                         Participant = Participants,
                         rep = reps)
Predictions$Participant
Predictions = rbind(Predictions,SD_Control)

for (i in 1:length(ParticipantVariability$ID)){
  ID = ParticipantVariability$ID[i]
  Predictions$EffectID_PSE[Predictions$Participant == ID] = ParticipantVariability$Bias_ID[i]
  Predictions$EffectID_JND[Predictions$Participant == ID] = ParticipantVariability$Variability_ID[i]
  Predictions$PSE_ID[Predictions$Participant == ID] = ParticipantVariability$PSE_ID[i]
  Predictions$SD_ID[Predictions$Participant == ID] = ParticipantVariability$SD_ID[i]
}

Predictions = Predictions %>%
  mutate(SelfmotionDirection = case_when(
    velH*self_velH > 0 ~ "Same Direction",
    velH*self_velH < 0 ~ "Opposite Directions",
    velH*self_velH == 0 ~ "Observer Static"),
    Bias_selfmotion = case_when(
      SelfmotionDirection == "Same Direction" ~ 0,
      SelfmotionDirection == "Opposite Directions" ~ EffectID_PSE*abs(self_velH), #overestimated by 20% (on average, but see between-participant variability) of self-motion speed when in opposite directions
      SelfmotionDirection == "Observer Static" ~ 0), #no bias when no observer motion
    VariabilityDiff_selfmotion = case_when(
      SelfmotionDirection == "Same Direction" ~ 0, #no difference in variability when Same Direction
      SelfmotionDirection == "Opposite Directions" ~ EffectID_JND, #higher when in opposite directions
      SelfmotionDirection == "Observer Static" ~ 0)) %>% #no difference in variability when static
  group_by(Participant) %>%
  mutate(VariabilityPerceivedDistance_SD = rnorm(1,WFtoSD(0.05),0.01), #this is to simulate between-participant variability in the sensitivity to distances
         VariabilityPerceivedDistance_Mean = 1, #here we could introduce a bias term, but we believe that participants estimate the distance accurately
         VariabilityPerceivedSpeed_SD = VariabilityDiff_selfmotion*WFtoSD(0.1)*velH*SD_ID+WFtoSD(0.1)*velH*SD_ID, #between-participant variability in the sensitivity to speed
         VariabilityPerceivedSpeed_Mean = Bias_selfmotion) %>% #bias term, only bias considered = bias due to self-motion
  mutate(CorrectDistance = velH*occlusion_time,
         PerceivedDistance = CorrectDistance*rnorm(length(velH),1,VariabilityPerceivedDistance_SD),
         DistanceError = PerceivedDistance-CorrectDistance,
         CorrectTime = occlusion_time,
         PerceivedvelH = rnorm(length(velH),PSE_ID*(velH + VariabilityPerceivedSpeed_Mean),VariabilityPerceivedSpeed_SD), #within-participant variability in perceived velocity
         time_perceived = PerceivedDistance/PerceivedvelH,
         TimingError = time_perceived-CorrectTime) %>%
  filter(abs(time_perceived) < abs(3*CorrectTime))

######
#and the extrapolated SD for the corresponding mean timing error
Prediction_SDs = Predictions %>%
  group_by(Participant, SelfmotionDirection, velH, occlusion_time) %>% 
  mutate(Mean_per_Condition = mean(time_perceived),
         SD_per_Condition = sd(time_perceived),
         SD_Adjusted_for_Bias = SD_per_Condition/Mean_per_Condition) %>% 
  slice(1)


######make data frame for motion estimation based on same individual values as for prediction:
ConditionOfInterest = c("Observer Static", "Same Direction", "Opposite Directions")
StandardValues = c(4,5,6)


Psychometric = expand.grid(Participant=Participants,
                           ConditionOfInterest=ConditionOfInterest,
                           StandardValues=StandardValues,
                           reps = 1:reps_MotionEstimation)

for (i in 1:length(ParticipantVariability$ID)){
  ID = ParticipantVariability$ID[i]
  Psychometric$EffectID_PSE[Psychometric$Participant == ID] = ParticipantVariability$Bias_ID[i]
  Psychometric$EffectID_JND[Psychometric$Participant == ID] = ParticipantVariability$Variability_ID[i]
  Psychometric$PSE_ID[Psychometric$Participant == ID] = ParticipantVariability$PSE_ID[i]
  Psychometric$SD_ID[Psychometric$Participant == ID] = ParticipantVariability$SD_ID[i]
}

Psychometric = Psychometric %>%
  group_by(Participant) %>%#
  mutate(PSE_Factor_ID = PSE_ID, #how much variability is in the means of the psychometric functions between subjects?
         SD_Factor_ID = SD_ID) #how much variability is in the standard deviations of the psychometric functions between subjects?

Multiplicator_PSE_Standard = 1
Multiplicator_SD_Standard = WFtoSD(0.1)

Psychometric = Psychometric %>%
  mutate(
    Mean_Standard = StandardValues*Multiplicator_PSE_Standard, #get the mean of the psychometric function for the baseline condition
    SD_Standard = StandardValues*Multiplicator_SD_Standard, #get the standard deviation of the psychometric function for the baseline condition
    Mean = case_when(ConditionOfInterest %in% c("Observer Static", "Same Direction") ~ Mean_Standard*PSE_Factor_ID,
                     ConditionOfInterest == "Opposite Directions" ~ (Mean_Standard + EffectID_PSE)*PSE_Factor_ID),#same but for condition of interest
    SD = case_when(ConditionOfInterest %in% c("Observer Static", "Same Direction") ~ abs(SD_Standard*SD_Factor_ID),
                   ConditionOfInterest == "Opposite Directions" ~ abs(SD_Standard + EffectID_JND*SD_Standard)*SD_Factor_ID))#same but for condition of interest

SD_ResponseFunction = 0.1

Psychometric = Psychometric %>%
  mutate(
    #same but values drawn from a Cauchy function
    staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction))

Psychometric = Psychometric %>%
  mutate(Presented_TestStimulusStrength = Mean*staircase_factor, #which stimulus strengths are shown? transform values from above (standardized to 1) to the stimulus strengths in condition of interest
         Difference = Presented_TestStimulusStrength - StandardValues, #difference in stimulus strength between reference stimulus and test stimulus strength (chosen by staircase)
         AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD), #choose for each difference how likely the participant is to choose one or the other as more intense
         ##get binary answers ("Test was stronger" yes/no) from probabilities for each trial
         Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability)) #draw answers based on probability
  )

#####fit psychometric functions
Parameters = quickpsy::quickpsy(Psychometric,Difference,Answer,
                                grouping = .(Participant,ConditionOfInterest,StandardValues),
                                bootstrap = "none")$par

Parameters2 = Parameters %>%
  filter(parn == "p1") %>%
  select(Participant,ConditionOfInterest,Mean=par, StandardValues)
Parameters2$SD = Parameters$par[Parameters$parn == "p2"]

####Pre-work to get the values needed for correlation stuff (Hypotheses 3a and 3b)
FittedPsychometricFunctions = Parameters2 %>% 
  group_by(Participant) %>% 
  ###make new columns where we get the mean PSEs/JNDs for the perceived speed (speed estimation task) for each condition and participant
  mutate(PSE_Opposite = mean(Mean[ConditionOfInterest == "Opposite Directions"]),
         SD_Opposite = mean(SD[ConditionOfInterest == "Opposite Directions"]),
         PSE_Same = mean(Mean[ConditionOfInterest == "Same Direction"]),
         SD_Same = mean(SD[ConditionOfInterest == "Same Direction"]),
         PSE_Static = mean(Mean[ConditionOfInterest == "Observer Static"]),
         SD_Static = mean(SD[ConditionOfInterest == "Observer Static"])) %>% 
  slice(1)

#make yet more columns where we get the difference between the Opposite/Same and the Static condition in terms of the means and standard deviations computed above
FittedPsychometricFunctions = FittedPsychometricFunctions %>% 
  mutate(Diff_PSE_Opposite = PSE_Opposite-PSE_Static,
         Diff_PSE_Same = PSE_Same-PSE_Static,
         Diff_SD_Opposite = SD_Opposite-SD_Static,
         Diff_SD_Same = SD_Same-SD_Static)




Correlations = Predictions %>%
  group_by(Participant) %>%
  filter(occlusion_time %in% c(0.5,0.6,0.7)) %>% 
  mutate(###make new columns where we get the mean/SD of the perceived duration for each condition and participant
         MeanExtrapolatedDuration_Opposite = mean(time_perceived[SelfmotionDirection == "Opposite Directions"]),
         SDExtrapolatedDuration_Opposite = sd(time_perceived[SelfmotionDirection == "Opposite Directions"]),
         MeanExtrapolatedDuration_Static = mean(time_perceived[SelfmotionDirection == "Observer Static"]),
         SDExtrapolatedDuration_Static = sd(time_perceived[SelfmotionDirection == "Observer Static"]),
         MeanExtrapolatedDuration_Same = mean(time_perceived[SelfmotionDirection == "Same Direction"]),
         SDExtrapolatedDuration_Same = sd(time_perceived[SelfmotionDirection == "Same Direction"])) %>%
  slice(1)

Correlations = Correlations %>% #make yet more columns where we get the difference between the Opposite/Same and the Static condition in terms of the means and standard deviations computed above
  mutate(DifMeanExtrapolatedMotion_Opposite = MeanExtrapolatedDuration_Opposite-MeanExtrapolatedDuration_Static,
         DifMeanExtrapolatedMotion_Same = MeanExtrapolatedDuration_Same-MeanExtrapolatedDuration_Static,
         DifSDExtrapolatedMotion_Opposite = SDExtrapolatedDuration_Opposite-SDExtrapolatedDuration_Static,
         DifSDExtrapolatedMotion_Same = SDExtrapolatedDuration_Same-SDExtrapolatedDuration_Static)


###match values from the speed estimation task into the dataframe with the prediction values, for each condition and participant
for(i in 1:length(FittedPsychometricFunctions$Participant)){

  Diff_PSE_Opposite = FittedPsychometricFunctions$Diff_PSE_Opposite[i]
  Diff_PSE_Same = FittedPsychometricFunctions$Diff_PSE_Same[i]
  Diff_SD_Opposite = FittedPsychometricFunctions$Diff_SD_Opposite[i]
  Diff_SD_Same = FittedPsychometricFunctions$Diff_SD_Same[i]
  
  velH = FittedPsychometricFunctions$StandardValues[i]
  ID = FittedPsychometricFunctions$Participant[i]
  
  Correlations$Diff_PSE_Opposite[Correlations$velH == velH &
                     Correlations$Participant == ID] = Diff_PSE_Opposite
  Correlations$Diff_PSE_Same[Correlations$velH == velH &
                     Correlations$Participant == ID] = Diff_PSE_Same
  Correlations$Diff_SD_Opposite[Correlations$velH == velH &
                     Correlations$Participant == ID] = Diff_SD_Opposite
  Correlations$Diff_SD_Same[Correlations$velH == velH &
                     Correlations$Participant == ID] = Diff_SD_Same
}


ggplot(Correlations,aes(Diff_PSE_Opposite,DifMeanExtrapolatedMotion_Opposite)) +
  geom_point() +
  geom_smooth(formula = y~x,
              method = "lm") +
  xlab("Error Task1") +
  ylab("Error Task2")
  

####Statistical tests for correlations
#for opposite direction
PSE_LMM_Corr_Opposite = lm(DifMeanExtrapolatedMotion_Opposite ~  Diff_PSE_Opposite,
                    data =  Correlations)
summary(PSE_LMM_Corr_Opposite)

SD_LMM_Corr_Opposite_Test = lm(DifSDExtrapolatedMotion_Opposite ~  DifMeanExtrapolatedMotion_Opposite + Diff_SD_Opposite,
                    data =  Correlations)
SD_LMM_Corr_Opposite_Null = lm(DifSDExtrapolatedMotion_Opposite ~ DifMeanExtrapolatedMotion_Opposite,
                                 data =  Correlations)
anova(SD_LMM_Corr_Opposite_Test,SD_LMM_Corr_Opposite_Null)
summary(SD_LMM_Corr_Opposite_Test)

#for same direction ("control" to see if everything works alright)
PSE_LMM_Corr_Same = lm(DifMeanExtrapolatedMotion_Same ~  Diff_PSE_Same,
                             data =  Correlations)
summary(PSE_LMM_Corr_Same)

SD_LMM_Corr_Same_Test = lm(DifSDExtrapolatedMotion_Same ~  DifMeanExtrapolatedMotion_Same + Diff_SD_Same,
                                 data =  Correlations)
SD_LMM_Corr_Same_Null = lm(DifSDExtrapolatedMotion_Same ~  DifMeanExtrapolatedMotion_Same,
                                 data =  Correlations)
anova(SD_LMM_Corr_Same_Test,SD_LMM_Corr_Same_Null)



#Plots (Figure 7)
Figure_Predictions_Correlations1 = ggplot(Correlations %>% 
                                            mutate(velH_Factor = paste0(velH," m/s"),
                                                   occlusion_time_Factor = paste0(occlusion_time, " s")),
                                               aes(Diff_PSE_Opposite,DifMeanExtrapolatedMotion_Opposite)) +
  geom_point(alpha = 1, 
             size = 2) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 2,
              se = FALSE)  +
  xlab(expression("PSE"["Opposite"]*" - PSE"["Static"]*" (m/s)")) +
  ylab(expression("Extrapolated Duration"["Opposite"]*" - Extrapolated Duration"["Static"]*" (m/s)")) +
  theme(legend.position = c(0.65,0.8)) +
  ggtitle("A.")

Figure_Predictions_Correlations2 = ggplot(Correlations %>% 
                                            mutate(velH_Factor = paste0(velH," m/s"),
                                                   occlusion_time_Factor = paste0(occlusion_time, " s")),
                                          aes(Diff_SD_Opposite,DifSDExtrapolatedMotion_Opposite)) +
  geom_point(alpha = 1, 
             size = 2) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 2,
              se = FALSE)  +
  xlab(expression("SD of PF"["Opposite"]*" - SD of PF"["Static"]*" (m/s)")) +
  ylab(expression("SD Extrapolated Duration"["Opposite"]*" - SD Extrapolated Duration"["Static"]*" (m/s)")) +
  ggtitle("B.") +
  theme(axis.title=element_text(size=13.5,face="bold"),
        legend.position = "none")

plot_grid(Figure_Predictions_Correlations1,Figure_Predictions_Correlations2)
ggsave("Figures/(Figure 05) Predictions Hypotheses 3a and 3b.jpg", w = 12, h = 6)