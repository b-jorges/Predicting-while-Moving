require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)
require(purrr)

#set.seed(4)
velH = c(4, 5, 6) #m/s
self_velH = c(-1.8,0,1.8) #meters over half a second, = 4m/s on average, but Gaussian motion profile
occlusion_times = c(0.5,0.6,0.7)
Participants = paste0("p",1:30)
reps = 1:5


###Get variability factors for each participant, between-participant variability:
ParticipantVariability = data.frame(ID = Participants,
                                    Bias_ID = rnorm(length(Participants),0.2,0.3), #get self-motion bias for each participant
                                    Variability_ID = rnorm(length(Participants),0.2,0.5), #get self-motion variability for each participant
                                    PSE_ID = rnorm(length(Participants),1,0.15), #multiplicator for the PSE for each person
                                    SD_ID = rnorm(length(Participants),1,0.15)) #multiplicator for the 


###########with variability
#trials per participant:
NumberTrials = length(reps)*length(occlusion_times)*length(self_velH)*length(velH)

#duration (3s per trial)
3*NumberTrials/60 #minutes

#Weber Fraction for distance estimation is about 5%
WFtoSD(0.05)

Predictions = expand.grid(velH = velH,
            self_velH = self_velH,
            occlusion_time = occlusion_times,
            Participant = Participants,
            rep = reps)

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
            velH*self_velH == 0 ~ "Static"),
         Bias_selfmotion = case_when(
           SelfmotionDirection == "Same Direction" ~ 0, 
           SelfmotionDirection == "Opposite Directions" ~ EffectID_PSE*abs(self_velH), #overestimated by 20% (on average, but see between-participant variability) of self-motion speed when in opposite directions
           SelfmotionDirection == "Static" ~ 0), #no bias when no observer motion
         VariabilityDiff_selfmotion = case_when(
           SelfmotionDirection == "Same Direction" ~ 0, #no difference in variability when Same Direction 
           SelfmotionDirection == "Opposite Directions" ~ EffectID_JND, #overestimated by 10% of self-motion speed when in opposite directions
           SelfmotionDirection == "Static" ~ 0)) %>% #no difference in variability when static 
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

ggplot(Predictions, aes(as.factor(velH),DistanceError)) +
  geom_boxplot()

ggplot(Predictions, aes(as.factor(velH),PerceivedvelH-velH)) +
  geom_boxplot()

ggplot(Predictions %>% filter(SelfmotionDirection != "Opposite Directions"), aes(as.factor(velH),PerceivedvelH-velH)) +
  geom_boxplot()

ggplot(Predictions, aes(as.factor(occlusion_time),TimingError, color = SelfmotionDirection)) +
  geom_boxplot() +
  scale_color_manual(values = c(Red, Yellow,BlauUB),
                     name = "") +
  labs(x = "", y = "Error (Perceived Duration - Occluded Duration; s)") +
  facet_grid(.~velH)
ggsave("Figures/(Figure XX) Predictions_Prediction_WithVariability.jpg", w = 12, h = 6)





ConditionOfInterest = c("Static", "Same Direction", "Opposite Directions")
StandardValues = c(4,5,6)
reps_MotionEstimation = 70

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

Mean_Variability_Between = 0.2
SD_Variability_Between = 0.2
Psychometric = Psychometric %>%
  group_by(Participant) %>%#
  mutate(PSE_Factor_ID = PSE_ID, #how much variability is in the means of the psychometric functions between subjects?
         SD_Factor_ID = SD_ID) #how much variability is in the standard deviations of the psychometric functions between subjects?

Multiplicator_PSE_Standard = 1
Multiplicator_SD_Standard = WFtoSD(0.1)

Psychometric = Psychometric %>%
  mutate(
    Mean_Standard = StandardValues+StandardValues*Multiplicator_PSE_Standard, #get the mean of the psychometric function for the baseline condition
    SD_Standard = StandardValues*Multiplicator_SD_Standard, #get the standard deviation of the psychometric function for the baseline condition
    Mean = case_when(ConditionOfInterest %in% c("Static", "Same Direction") ~ Mean_Standard*PSE_Factor_ID,
                     ConditionOfInterest == "Opposite Directions" ~ (Mean_Standard + EffectID_PSE)*PSE_Factor_ID),#same but for condition of interest
    SD = case_when(ConditionOfInterest %in% c("Static", "Same Direction") ~ abs(SD_Standard*SD_Factor_ID),
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

Parameters = quickpsy::quickpsy(Psychometric,Difference,Answer,
                             grouping = .(Participant,ConditionOfInterest,StandardValues),
                             bootstrap = "none")$par

Parameters2 = Parameters %>%
        filter(parn == "p1") %>%
        select(Participant,ConditionOfInterest,Mean=par, StandardValues)
Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
FittedPsychometricFunctions = Parameters2


Correlations = Predictions %>% 
  group_by(Participant,velH,SelfmotionDirection,occlusion_time) %>% 
  mutate(MeanError = mean(TimingError),
         SDError = sd(TimingError)) %>%
  slice(1)

for(i in 1:length(FittedPsychometricFunctions$Participant)){
  PSE = FittedPsychometricFunctions$Mean[i]
  SD = FittedPsychometricFunctions$SD[i]
  velH = FittedPsychometricFunctions$StandardValues[i]
  MotionProfile = FittedPsychometricFunctions$ConditionOfInterest[i]
  ID = FittedPsychometricFunctions$Participant[i]
  
  Correlations$PSE[Correlations$velH == velH & 
                   Correlations$SelfmotionDirection == MotionProfile &
                   Correlations$Participant == ID] = PSE 
  Correlations$SD[Correlations$velH == velH & 
                     Correlations$SelfmotionDirection == MotionProfile &
                     Correlations$Participant == ID] = SD 
}

ggplot(Correlations,aes(PSE,MeanError)) +
  geom_point()
ggplot(Correlations,aes(SD,SDError)) +
  geom_smooth() +
  geom_point()

cor.test(Correlations$SD,Correlations$SDError)

summary(lmer(SDError ~ SD + (velH + occlusion_time | Participant),
             data = Correlations))
summary(lmer(MeanError ~ PSE + (velH + occlusion_time | Participant),
             data = Correlations))