require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(quickpsy)
require(purrr)
source("Utilities/parabolic.r")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

velH = c(4, 5, 6) #m/s
self_velH = c(-3.6,0,3.6) #meters over half a second, = 4m/s on average, but Gaussian motion profile
Participants = paste0("p",1:40)
ConditionOfInterest = c("Observer Static", "Same Direction", "Opposite Directions")
StandardValues = c(4,5,6)
reps_MotionEstimation = 70

###Get variability factors for each participant, between-participant variability:
ParticipantVariability = data.frame(ID = Participants,
                                    Bias_ID = rnorm(length(Participants),0.2,0.3), #get self-motion bias for each participant
                                    Variability_ID = rnorm(length(Participants),0.2,0.3), #get self-motion variability for each participant
                                    PSE_ID = 1,#rnorm(length(Participants),1,0.15), #multiplicator for the PSE for each person
                                    SD_ID = rnorm(length(Participants),1,0.15)) #multiplicator for the

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

Parameters = quickpsy::quickpsy(Psychometric,Presented_TestStimulusStrength,Answer,
                                grouping = .(Participant,ConditionOfInterest,StandardValues),
                                bootstrap = "none")$par

Parameters2 = Parameters %>%
  filter(parn == "p1") %>%
  select(Participant,ConditionOfInterest,Mean=par, StandardValues)
Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
FittedPsychometricFunctions = Parameters2

#See how the model used for analysis fares on the simulated data. Model for accuracy:
Model1 = lmer(Mean ~ ConditionOfInterest + (StandardValues | Participant),
          data = FittedPsychometricFunctions,
          control = lmerControl(optimizer = "bobyqa",
                                optCtrl = list(maxfun = 2e4)))
summary(Model1)


#Models for precision
Model_Opposite_Test = lmer(log(SD) ~ ConditionOfInterest + Mean + (StandardValues | Participant),
                       data = FittedPsychometricFunctions %>% filter(ConditionOfInterest != "Same Direction" & SD > 0.1))
Model_Opposite_Null = lmer(log(SD) ~ Mean + (StandardValues | Participant),
                       data = FittedPsychometricFunctions %>% filter(ConditionOfInterest != "Same Direction" & SD > 0.1))

Model_Same_Test = lmer(log(SD) ~ ConditionOfInterest + Mean + (StandardValues | Participant),
              data = FittedPsychometricFunctions %>% filter(ConditionOfInterest != "Opposite Directions" & SD > 0.1))
Model_Same_Null = lmer(log(SD) ~ Mean + (StandardValues | Participant),
              data = FittedPsychometricFunctions %>% filter(ConditionOfInterest != "Opposite Directions" & SD > 0.1))
anova(Model_Same_Test,Model_Same_Null)

# Plots with Predictions (Figure 03)
Figure_SpeedPredictions1 = ggplot(FittedPsychometricFunctions %>% mutate(velH_Factor = paste0(StandardValues," m/s")),
                                  aes(velH_Factor,Mean,color = ConditionOfInterest)) +
  geom_boxplot(size = 1.5) +
  scale_color_manual(name = "Motion Profile", values = c("red","blue","orange")) +
  xlab("Motion Profile") +
  ylab("PSE (m/s)") +
  scale_x_discrete(name = "")  +
  theme(legend.position = c(0.1,0.8))

Predictions$SelfmotionDirection
FittedPsychometricFunctions$ConditionOfInterest
Figure_SpeedPredictions2 = ggplot(FittedPsychometricFunctions %>% mutate(velH_Factor = paste0(StandardValues," m/s")),
                                  aes(velH_Factor,SD,color = ConditionOfInterest)) +
  geom_boxplot(size = 1.5) +
  scale_color_manual(name = "Motion Profile", values = c("red","blue","orange")) +
  scale_x_discrete(name = "Motion Profile") +
  xlab("Motion Profile") + 
  ylab("SD of Psychometric Function (m/s)") +
  theme(legend.position = "none")
plot_grid(Figure_SpeedPredictions1,Figure_SpeedPredictions2)
ggsave("Figures/(Figure 04) Predictions Hypotheses 2a and 2b.jpg", w = 12, h = 6)
