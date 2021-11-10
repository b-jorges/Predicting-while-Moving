require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
#setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)
require(purrr)

Powerframe = data.frame()

iterations_lower = 1
iterations_upper = 150

set.seed(iterations_lower)

time_init_overall = Sys.time()

for (l in iterations_lower:iterations_upper){
  time_init = Sys.time()

  for (j in c(20,30,40)){
    for (k in c(5,9,13)){ ###this is for the number of trials
      for(m in c(50, 70, 90)){

        Participants = paste0("p",1:j)

        velH = c(4, 5, 6) #m/s
        self_velH = c(-3.6,0,3.6) #meters over half a second, = 4m/s on average, but Gaussian motion profile
        occlusion_times = c(0.5,0.6,0.7)

        reps = 1:k

        reps_MotionEstimation = m


        ###Get variability factors for each participant, between-participant variability:
        ParticipantVariability = data.frame(ID = Participants,
                                            Bias_ID = rnorm(length(Participants),0.2,0.3), #get self-motion bias for each participant
                                            Variability_ID = rnorm(length(Participants),0.2,0.3), #get self-motion variability for each participant
                                            PSE_ID = 1, #rnorm(length(Participants),1,0.15), #multiplicator for the PSE for each person
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

        #Prediction Accuracy
        LMM_Prediction_Accuracy = lmer(TimingError ~ SelfmotionDirection + (velH | Participant) + (1 | occlusion_time),
                                       data = Predictions)
        summary(LMM_Prediction_Accuracy)

        #Prediction Precision
        LMM_Prediction_SD1 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + SelfmotionDirection + (velH | Participant) + (1 | occlusion_time),
                                  data = Prediction_SDs %>%
                                    filter(SelfmotionDirection != "Same Direction" & SD_per_Condition > 0.01),
                                  REML = FALSE)
        LMM_Prediction_SD2 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + (velH | Participant) + (1 | occlusion_time),
                                  data = Prediction_SDs %>%
                                    filter(SelfmotionDirection != "Same Direction" & SD_per_Condition > 0.01),
                                  REML = FALSE)
        anova(LMM_Prediction_SD1,LMM_Prediction_SD2)


        #Prediction Precision
        LMM_Prediction_SD3 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + SelfmotionDirection + (velH | Participant) + (1 | occlusion_time),
                                  data = Prediction_SDs %>%
                                    filter(SelfmotionDirection != "Opposite Directions" & SD_per_Condition > 0.01),
                                  REML = FALSE)
        LMM_Prediction_SD4 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + (velH | Participant) + (1 | occlusion_time),
                                  data = Prediction_SDs %>%
                                    filter(SelfmotionDirection != "Opposite Directions" & SD_per_Condition > 0.01),
                                  REML = FALSE)

######make data frame for motion estimation based on same individual values as for prediction:
        ConditionOfInterest = c("Observer Static", "Same Direction", "Opposite Directions")
        StandardValues = c(4,5,6)
        reps_MotionEstimation = m

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
        FittedPsychometricFunctions = Parameters2


#####stats tests for motion estimation
        LMM_Mean = lmer(Mean ~ ConditionOfInterest + (StandardValues | Participant),
                         data = FittedPsychometricFunctions)

        LMM_SD_Test_Opposite = lmer(log(SD) ~ ConditionOfInterest + Mean + (StandardValues | Participant),
                         data = FittedPsychometricFunctions  %>%
                                  filter(SD > 0.01 & ConditionOfInterest != "Same Direction"),
                         REML = FALSE)
        LMM_SD_Null_Opposite = lmer(log(SD) ~ Mean + (StandardValues | Participant),
                                data = FittedPsychometricFunctions  %>%
                                  filter(SD > 0.01 & ConditionOfInterest != "Same Direction"),
                                REML = FALSE)
        summary(LMM_SD_Test_Opposite)

        LMM_SD_Test_Same = lmer(log(SD) ~ ConditionOfInterest + Mean + (StandardValues | Participant),
                                data = FittedPsychometricFunctions  %>%
                                  filter(SD > 0.01 & ConditionOfInterest != "Opposite Directions"),
                                REML = FALSE)
        LMM_SD_Null_Same = lmer(log(SD) ~ Mean + (StandardValues | Participant),
                                data = FittedPsychometricFunctions  %>%
                                  filter(SD > 0.01 & ConditionOfInterest != "Opposite Directions"),
                                REML = FALSE)
        anova(LMM_SD_Test_Same,LMM_SD_Null_Same)
        summary(LMM_SD_Test_Same)
        
        anova(LMM_SD_Test_Opposite,LMM_SD_Null_Opposite)
        summary(LMM_SD_Test_Opposite)

        ####Pre-work to get the values needed for correlation stuff
        #we compute the difference between baseline and opposite/same for each condition
        #for motion estimation thats for each combination of velH and participant
        FittedPsychometricFunctions = FittedPsychometricFunctions %>%
          group_by(Participant) %>%
          mutate(PSE_Opposite = mean(Mean[ConditionOfInterest == "Opposite Directions"]),
                 SD_Opposite = mean(SD[ConditionOfInterest == "Opposite Directions"]),
                 PSE_Same = mean(Mean[ConditionOfInterest == "Same Direction"]),
                 SD_Same = mean(SD[ConditionOfInterest == "Same Direction"]),
                 PSE_Static = mean(Mean[ConditionOfInterest == "Observer Static"]),
                 SD_Static = mean(SD[ConditionOfInterest == "Observer Static"])) %>%
          slice(1)

        FittedPsychometricFunctions = FittedPsychometricFunctions %>%
          mutate(Diff_PSE_Opposite = PSE_Opposite-PSE_Static,
                 Diff_PSE_Same = PSE_Same-PSE_Static,
                 Diff_SD_Opposite = SD_Opposite-SD_Static,
                 Diff_SD_Same = SD_Same-SD_Static)


        #for prediction thats for each combination of velH, occlusion time and participant
        Correlations = Predictions %>%
          group_by(Participant) %>%
          filter(occlusion_time %in% c(0.5,0.6,0.7)) %>% #we take out the 0.1, ... 1.0 ones because fuck them (and we dont have counterparts in opposite/same)
          mutate(
            MeanExtrapolatedDuration_Opposite = mean(time_perceived[SelfmotionDirection == "Opposite Directions"]),
            SDExtrapolatedDuration_Opposite = sd(time_perceived[SelfmotionDirection == "Opposite Directions"]),
            MeanExtrapolatedDuration_Static = mean(time_perceived[SelfmotionDirection == "Observer Static"]),
            SDExtrapolatedDuration_Static = sd(time_perceived[SelfmotionDirection == "Observer Static"]),
            MeanExtrapolatedDuration_Same = mean(time_perceived[SelfmotionDirection == "Same Direction"]),
            SDExtrapolatedDuration_Same = sd(time_perceived[SelfmotionDirection == "Same Direction"])) %>%
          slice(1)

        Correlations = Correlations %>%
          mutate(DifMeanExtrapolatedMotion_Opposite = MeanExtrapolatedDuration_Opposite-MeanExtrapolatedDuration_Static,
                 DifMeanExtrapolatedMotion_Same = MeanExtrapolatedDuration_Same-MeanExtrapolatedDuration_Static,
                 DifSDExtrapolatedMotion_Opposite = SDExtrapolatedDuration_Opposite-SDExtrapolatedDuration_Static,
                 DifSDExtrapolatedMotion_Same = SDExtrapolatedDuration_Same-SDExtrapolatedDuration_Static)

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

        Correlations = Correlations[complete.cases(Correlations),]

####Statistical tests for correlations
        ####Statistical tests for correlations
        #for opposite direction
        #accuracy
        PSE_LMM_Corr_Opposite = lm(DifMeanExtrapolatedMotion_Opposite ~  Diff_PSE_Opposite,
                                     data =  Correlations)
        summary(PSE_LMM_Corr_Opposite)

        #precision
        SD_LMM_Corr_Opposite_Test = lm(DifSDExtrapolatedMotion_Opposite ~  DifMeanExtrapolatedMotion_Opposite + Diff_SD_Opposite,
                                         data =  Correlations)
        SD_LMM_Corr_Opposite_Null = lm(DifSDExtrapolatedMotion_Opposite ~  DifMeanExtrapolatedMotion_Opposite,
                                         data =  Correlations)
        anova(SD_LMM_Corr_Opposite_Test,SD_LMM_Corr_Opposite_Null)

        #for same direction
        #accuracy
        PSE_LMM_Corr_Same = lm(DifMeanExtrapolatedMotion_Same ~  Diff_PSE_Same,
                                 data =  Correlations)

        #precision
        SD_LMM_Corr_Same_Test = lm(DifSDExtrapolatedMotion_Same ~  DifMeanExtrapolatedMotion_Same + Diff_SD_Same,
                                     data =  Correlations)
        SD_LMM_Corr_Same_Null = lm(DifSDExtrapolatedMotion_Same ~  DifMeanExtrapolatedMotion_Same,
                                     data =  Correlations)


        Powerframe = rbind(Powerframe,
                                  data.frame(nParticipants=rep(j,2),
                                             reps_Prediction=rep(k,2),
                                             reps_MotionEstimation=rep(reps_MotionEstimation,2),

                                             #power: chance to  detect effect for Opposite Directions
                                             pvalue_Prediction = c(summary(LMM_Prediction_Accuracy)$coef["SelfmotionDirectionOpposite Directions","Pr(>|t|)"],
                                                                   anova(LMM_Prediction_SD1,LMM_Prediction_SD2)$`Pr(>Chisq)`[2]),
                                             pvalue_MotionEstimation = c(summary(LMM_Mean)$coef["ConditionOfInterestOpposite Directions","Pr(>|t|)"],
                                                                         anova(LMM_SD_Test_Opposite,LMM_SD_Null_Opposite)$`Pr(>Chisq)`[2]),
                                             pvalue_Correlation = c(summary(PSE_LMM_Corr_Opposite)$coef["Diff_PSE_Opposite","Pr(>|t|)"],
                                                                    anova(SD_LMM_Corr_Opposite_Test,SD_LMM_Corr_Opposite_Null)$`Pr(>F)`[2]),

                                             #as control: Same Direction where no effect should be visible
                                             pvalue_Prediction_Same = c(summary(LMM_Prediction_Accuracy)$coef["SelfmotionDirectionSame Direction","Pr(>|t|)"],
                                                                   anova(LMM_Prediction_SD3,LMM_Prediction_SD4)$`Pr(>Chisq)`[2]),
                                             pvalue_MotionEstimation_Same = c(summary(LMM_Mean)$coef["ConditionOfInterestSame Direction","Pr(>|t|)"],
                                                                              anova(LMM_SD_Test_Same,LMM_SD_Null_Same)$`Pr(>Chisq)`[2]),
                                             pvalue_Correlation_Same = c(summary(PSE_LMM_Corr_Same)$coef["Diff_PSE_Same","Pr(>|t|)"],
                                                                         anova(SD_LMM_Corr_Same_Test,SD_LMM_Corr_Same_Null)$`Pr(>F)`[2]),
                                             iteration = rep(l,2),
                                             Label = c("Accuracy","Precision")))

        #keep us posted on how its going:
        print(paste0("Iteration: ", l, "; Participants: ", j, "; reps motion estimation: ", k, "; reps prediction: ", m))

        remove(Psychometric,Correlations,LMM_Mean,LMM_SD,Parameters,Parameters2,FittedPsychometricFunctions,
               LMM_Prediction_Accuracy,LMM_Prediction_SD1,LMM_Prediction_SD2,PSE_LMM_Corr_Opposite,SD_LMM_Corr_Opposite_Test,
               SD_LMM_Corr_Opposite_Null,LMM_Prediction_SD3,LMM_Prediction_SD4,PSE_LMM_Corr_Same,SD_LMM_Corr_Same_Test,SD_LMM_Corr_Same_Null)

        save(Powerframe, file = "C:/Users/bjoer/Documents/GitHub/Predicting-while-Moving/SavedVariables/Powerframe_PC1.RData")
        # save(Powerframe, file = "C:/Users/bjoer/Documents/GitHub/Predicting-while-Moving/SavedVariables/Powerframe_PC2.RData")
      }
    }
  }

  print(paste0("Iteration #", l, " took ", round(difftime(Sys.time(), time_init, units='mins'),2), " minute(s). That means, the whole damn thing should take about ",
         round((iterations_upper-iterations_lower+1)*difftime(Sys.time(), time_init, units='mins'),2), " minute(s). ", round(difftime(Sys.time(), time_init_overall, units='mins'),2),
         " minutes have already passed."))
}

# print(paste0("Guuuuurl, enough. We're done. And it's only taken us ", round(difftime(Sys.time(), time_init_overall, units='mins'),2), " minutes of our lives."))
# 
# 
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                "/SavedVariables/Powerframe_PC1.RData"))
# Powerframe_PC1 = Powerframe
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                "/SavedVariables/Powerframe_PC2.RData"))
# Powerframe_PC2 = Powerframe
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                "/SavedVariables/Powerframe_PC3.RData"))
# Powerframe_PC3 = Powerframe
# 
# Powerframe = rbind(Powerframe_PC1,Powerframe_PC2,Powerframe_PC3)
# 
# 
# #do everything at alpha level of 0.05
# alpha = 0.05
# 
# # pre-process
# Power = Powerframe %>%
#   group_by(nParticipants,reps_Prediction,Label) %>%
#   mutate(Power_Prediction = mean(pvalue_Prediction < alpha),
#          Power_Prediction_Same = mean(pvalue_Prediction_Same < alpha)) %>%
#   group_by(nParticipants,reps_MotionEstimation,Label) %>%
#   mutate(Power_MotionEstimation = mean(pvalue_MotionEstimation < alpha),
#          Power_MotionEstimation_Same = mean(pvalue_MotionEstimation_Same < alpha)) %>%
#   group_by(nParticipants,reps_Prediction,reps_MotionEstimation,Label) %>%
#   mutate(Power_Correlation = mean(pvalue_Correlation < alpha),
#          Power_Correlation_Same = mean(pvalue_Correlation_Same < alpha),
#          Label_Correlation = paste0("P: ", reps_Prediction, "; S: ", reps_MotionEstimation)) %>%
#   slice(1)
# 
# 
# #####Figure power predictgion
# Power_Prediction = ggplot(Power, aes(nParticipants,Power_Prediction,
#                                      color = as.factor(reps_Prediction),
#                                      linetype = as.factor(reps_Prediction))) +
#   geom_line(size = 2) +
#   xlab("Number of Participants") +
#   ylab("Power") +
#   scale_x_continuous(breaks = c(20,30,40)) +
#   scale_color_manual(name = "Repetitions\nper Condition",
#                      values = c("darkgreen", "grey", "pink")) +
#   scale_linetype_manual(name = "Repetitions\nper Condition",
#                      values = c(3,2,1)) +
#   geom_hline(yintercept = 0.9, linetype=2) +
#   geom_hline(yintercept = 0.95, linetype=3) +
#   geom_hline(yintercept = 0.8, linetype=5) +
#   ylim(c(0,1)) +
#   facet_grid(.~Label) +
#   theme(legend.position = c(0.2,0.3),
#         legend.background = element_rect(fill = "lightgrey")) +
#   ggtitle("A. Prediction")
# 
# Power_Prediction_Same = ggplot(Power, aes(nParticipants,Power_Prediction_Same,
#                                           color = as.factor(reps_Prediction),
#                                           linetype = as.factor(reps_Prediction))) +
#   geom_line(size = 1) +
#   xlab("Number of Participants") +
#   ylab("Power") +
#   scale_x_continuous(breaks = c(10,20,30)) +
#   scale_color_manual(name = "Repetitions\nper Condition",
#                      values = c("pink","grey","darkgreen")) +
#   scale_linetype_manual(name = "Repetitions\nper Condition",
#                         values = c(3,2,1)) +
#   geom_hline(yintercept = 0.05, linetype=2) +
#   ylim(c(0,1)) +
#   facet_grid(.~Label) +
#   theme(legend.position = c(0.2,0.3)) +
#   ggtitle("A. Prediction (No Effect)")
# 
# 
# #####Figure power motion estimation
# Power_MotionEstimation = ggplot(Power, aes(nParticipants,Power_MotionEstimation,
#                                            color = as.factor(reps_MotionEstimation),
#                                            linetype = as.factor(reps_MotionEstimation))) +
#   geom_line(size = 2) +
#   xlab("Number of Participants") +
#   ylab("Power") +
#   scale_x_continuous(breaks = c(20,30,40)) +
#   scale_color_manual(name = "Repetitions\nper Condition",
#                      values = c("darkgreen", "grey", "pink")) +
#   scale_linetype_manual(name = "Repetitions\nper Condition",
#                         values = c(3,2,1)) +
#   geom_hline(yintercept = 0.9, linetype=2) +
#   geom_hline(yintercept = 0.95, linetype=3) +
#   geom_hline(yintercept = 0.8, linetype=5) +
#   ylim(c(0,1)) +
#   facet_grid(.~Label) +
#   theme(legend.position = c(0.2,0.3),
#         legend.background = element_rect(fill = "lightgrey")) +
#   ggtitle("B. Speed Estimation")
# 
# Power_MotionEstimation_Same = ggplot(Power, aes(nParticipants,Power_MotionEstimation_Same, color = as.factor(reps_MotionEstimation))) +
#   geom_line(size = 1) +
#   xlab("Number of Participants") +
#   ylab("Power") +
#   scale_x_continuous(breaks = c(10,20,30)) +
#   scale_color_manual(name = "Repetitions\nper Condition",
#                      values = c("red","blue","orange")) +
#   geom_hline(yintercept = 0.05, linetype=2) +
#   ylim(c(0,1)) +
#   facet_grid(.~Label) +
#   theme(legend.position = c(0.2,0.3)) +
#   ggtitle("B. Speed Estimation (No Effect)")
# 
# 
# #####Figure power correlations
# Power_Correlation = ggplot(Power, aes(nParticipants,Power_Correlation, color = as.factor(Label_Correlation),
#                                                                        linetype = as.factor(Label_Correlation))) +
#   geom_line(size = 2) +
#   xlab("Number of Participants") +
#   ylab("Power") +
#   scale_x_continuous(breaks = c(20,30,40)) +
#   scale_color_manual(name = "Repetitions",
#                      values = c(rep("darkgreen",3), rep("grey",3), rep("pink",3))) +
#   scale_linetype_manual(name = "Repetitions",
#                      values = rep(c(3,2,1),3)) +
#   geom_hline(yintercept = 0.9, linetype=2) +
#   geom_hline(yintercept = 0.95, linetype=3) +
#   geom_hline(yintercept = 0.8, linetype=5) +
#   ylim(c(0,1)) +
#   facet_wrap(Label~.) +
#   theme(legend.position = c(0.1,0.22),
#         legend.key.size = unit(0.4,"cm"),
#         legend.background = element_rect(fill = "lightgrey")) +
#   ggtitle("C. Correlation")
# 
# Power_Correlation_Same = ggplot(Power, aes(nParticipants,Power_Correlation_Same, color = as.factor(Label_Correlation),
#                                            linetype = as.factor(Label_Correlation))) +
#   geom_line(size = 1) +
#   xlab("Number of Participants") +
#   ylab("Power") +
#   scale_x_continuous(breaks = c(10,20,30)) +
#   scale_color_manual(name = "Repetitions",
#                      values = c(rep("darkgreen",3), rep("grey",3), rep("pink",3))) +
#   scale_linetype_manual(name = "Repetitions",
#                         values = rep(c(3,2,1),3)) +
#   geom_hline(yintercept = 0.05, linetype=2) +
#   ylim(c(0,1)) +
#   facet_wrap(Label~.) +
#   theme(legend.position = c(0.1,0.6),
#         legend.background = element_rect(fill = "lightgrey")) +
#   ggtitle("C. Correlation (No Effect)")
# 
# plot_grid(Power_Prediction,Power_MotionEstimation,Power_Correlation, nrow = 1)
# ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#               "/Figures/(Figure 06) Power Analysis Correlation.jpg"), w = 18, h = 6)
