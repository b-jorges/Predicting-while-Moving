setwd(dirname(rstudioapi::getSourceEditorContext()$path))
require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(quickpsy)
require(purrr)
require(ggdist)
source("Utilities/parabolic.r")
source("Analysis Correlation.R")
source("Analysis Speed Estimation.R")
source("Analysis Prediction.R")
require(cowplot)
theme_set(theme_cowplot())

# GetRMSE_Prediction = function(SelfMotionEffectAccuracy,
#                               SelfMotionEffectPrecision,
#                               Predictions,
#                               TypeOfRMSE){
# 
#   # SelfMotionEffectAccuracy = x[1]
#   # SelfMotionEffectPrecision = x[2]
# 
#   Prediction_Simulate = Predictions %>%
#     mutate(TimingError = Response_Time-Occlusion_Duration) %>%
#     filter(Occlusion_Duration %in% c(0.5,0.6, 0.7)) %>%
#     select(Occlusion_Duration,velH_Abs,velH_Subject,ID,TimingError, Congruent) %>%
#     mutate(EffectID_PSE = SelfMotionEffectAccuracy,
#            EffectID_JND = SelfMotionEffectPrecision,
#            PSE_ID = 1,
#            SD_ID = 1)
# 
#   RMSE_Vector_Accuracy = c()
#   RMSE_Vector_Precision = c()
#   RMSE_Vector = c()
# 
#   self_velH = 3.6
# 
#   for (iteration in 1:50){
# 
#     Prediction_Simulate = Prediction_Simulate %>%
#       mutate(Bias_selfmotion = case_when(
#           Congruent != "Observer Static" ~ EffectID_PSE*abs(self_velH), #overestimated by 20% (on average, but see between-participant variability) of self-motion speed when in opposite directions
#           Congruent == "Observer Static" ~ 0), #no bias when no observer motion
#         VariabilityDiff_selfmotion = case_when(
#           Congruent != "Observer Static" ~ EffectID_JND, #higher when in opposite directions
#           Congruent == "Observer Static" ~ 0)) %>% #no difference in variability when static
#       mutate(VariabilityPerceivedDistance_SD = rnorm(1,WFtoSD(0.05),0.01), #this is to simulate between-participant variability in the sensitivity to distances
#              VariabilityPerceivedDistance_Mean = 1, #here we could introduce a bias term, but we believe that participants estimate the distance accurately
#              VariabilityPerceivedSpeed_SD = VariabilityDiff_selfmotion*WFtoSD(0.1)*velH_Abs*SD_ID+WFtoSD(0.1)*velH_Abs*SD_ID, #between-participant variability in the sensitivity to speed
#              VariabilityPerceivedSpeed_Mean = Bias_selfmotion) %>% #bias term, only bias considered = bias due to self-motion
#       mutate(CorrectDistance = velH_Abs*Occlusion_Duration,
#              PerceivedDistance = CorrectDistance*rnorm(length(velH_Abs),1,VariabilityPerceivedDistance_SD),
#              DistanceError = PerceivedDistance-CorrectDistance,
#              CorrectTime = Occlusion_Duration,
#              PerceivedvelH = rnorm(length(velH_Abs),PSE_ID*(velH_Abs + VariabilityPerceivedSpeed_Mean),abs(VariabilityPerceivedSpeed_SD)), #within-participant variability in perceived velocity
#              time_perceived = PerceivedDistance/PerceivedvelH,
#              TimingError_Simulated = time_perceived-CorrectTime,
#              ExtrapolatedTime_Simulated = TimingError_Simulated + Occlusion_Duration,
#              ExtrapolatedTime = TimingError + Occlusion_Duration) %>%
#       filter(abs(ExtrapolatedTime_Simulated) < abs(3*CorrectTime))
# 
#     ######
#     #and the extrapolated SD for the corresponding mean timing error
#     Prediction_Simulate_SDs = Prediction_Simulate %>%
#       group_by(Congruent, velH_Abs, Occlusion_Duration) %>%
#       mutate(Mean_per_Condition = mean(time_perceived),
#              Mean_per_Condition_Simulated = mean(ExtrapolatedTime_Simulated),
#              SD_per_Condition = sd(time_perceived),
#              SD_per_Condition_Simulated = sd(ExtrapolatedTime_Simulated),
#              SD_Adjusted_for_Bias = SD_per_Condition/Mean_per_Condition) %>%
#       slice(1)
# 
#     Prediction_Simulate_SDs_RelBaseline = Prediction_Simulate %>%
#       group_by(velH_Abs,Occlusion_Duration) %>%
#       mutate(Mean_per_Condition_rel_Baseline = mean(ExtrapolatedTime[Congruent != "Observer Static"], na.rm = TRUE) -
#                                                mean(ExtrapolatedTime[Congruent == "Observer Static"], na.rm = TRUE),
#              SD_per_Condition_rel_Baseline = sd(ExtrapolatedTime[Congruent != "Observer Static"], na.rm = TRUE)-
#                sd(ExtrapolatedTime[Congruent == "Observer Static"], na.rm = TRUE),
#              Mean_per_Condition_rel_Baseline_Simulated = mean(ExtrapolatedTime_Simulated[Congruent != "Observer Static"], na.rm = TRUE)-
#                mean(ExtrapolatedTime_Simulated[Congruent == "Observer Static"], na.rm = TRUE),
#              SD_per_Condition_rel_Baseline_Simulated = sd(ExtrapolatedTime_Simulated[Congruent != "Observer Static"], na.rm = TRUE)-
#                sd(ExtrapolatedTime_Simulated[Congruent == "Observer Static"], na.rm = TRUE)) %>%
#       slice(1)
# 
#     #get the baseline error to account for biases/precision effects that are not of interest to us
#     RMSE_Accuracy = median((mean(Prediction_Simulate_SDs_RelBaseline$Mean_per_Condition_rel_Baseline,na.rm = TRUE) -
#                             mean(Prediction_Simulate_SDs_RelBaseline$Mean_per_Condition_rel_Baseline_Simulated, na.rm = TRUE))^2, na.rm = TRUE)^0.5
#     RMSE_Precision = median((mean(Prediction_Simulate_SDs_RelBaseline$SD_per_Condition_rel_Baseline, na.rm = TRUE) -
#                             mean(Prediction_Simulate_SDs_RelBaseline$SD_per_Condition_rel_Baseline_Simulated, na.rm = TRUE))^2, na.rm = TRUE)^0.5
# 
#     RMSE_Vector_Accuracy = c(RMSE_Vector_Accuracy,RMSE_Accuracy)
#     RMSE_Vector_Precision = c(RMSE_Vector_Precision,RMSE_Precision)
#     RMSE_Vector = c(RMSE_Vector,mean(c(RMSE_Accuracy,RMSE_Precision)))
# 
#     if(iteration %% 50 == 0){
#       print(iteration)
#       print(paste0("SelfMotionEffect_Accuracy: ", SelfMotionEffectAccuracy))
#       print(paste0("SelfMotionEffectPrecision: ", SelfMotionEffectPrecision))
#       print(paste0("ID: ", unique(Prediction_Simulate_SDs_RelBaseline$ID)))}
#   }
# 
#   if(TypeOfRMSE == "Overall"){
#     print(paste0("Mean RMSE: ", mean(RMSE_Vector,na.rm = TRUE)))
#     median(RMSE_Vector,na.rm = TRUE)
#   } else if (TypeOfRMSE == "Accuracy"){
#     print(paste0("RMSE_Vector_Accuracy: ", mean(RMSE_Vector_Accuracy,na.rm = TRUE)))
#     median(RMSE_Vector_Accuracy,na.rm = TRUE)
#   } else if (TypeOfRMSE == "Precision"){
#     print(paste0("RMSE_Vector_Precision: ", mean(RMSE_Vector_Precision,na.rm = TRUE)))
#     median(RMSE_Vector_Precision,na.rm = TRUE)
#   } else{
#     print("Lol no. This TypeOfRMSE won't do. Try with 'Overall', 'Accuracy', or 'Precision'.")
#   }
# }

##############################################################################################################################
#########Prediction Task
######Opposite Directions
#Accuracy

# ResultsOptimization_Opposite_Accuracy_Predictions = data.frame()
# for (i in setdiff(unique(Prediction$ID), unique(ResultsOptimization_Opposite_Accuracy_Predictions$ID))){
# 
#   Optim_Opposite = optimize(GetRMSE_Prediction,
#                             c(-1,1),
#                             SelfMotionEffectPrecision = 0,
#                             Predictions = Prediction %>% filter(ID == i & Congruent != "Same Direction"),
#                             TypeOfRMSE = "Accuracy")
# 
#   ResultsOptimization_Opposite_Accuracy_Predictions = rbind(ResultsOptimization_Opposite_Accuracy_Predictions,
#                                                             c(Optim_Opposite$minimum,Optim_Opposite$objective,i))
#   save(ResultsOptimization_Opposite_Accuracy_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Accuracy_Predictions.RData"))
# }
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Accuracy_Predictions.RData"))
# colnames(ResultsOptimization_Opposite_Accuracy_Predictions) = c("Minimum_Accuracy","AccuracyObjective", "ID")

#Precision

# ResultsOptimization_Opposite_Predictions = data.frame()
# for (i in setdiff(unique(Prediction$ID), unique(ResultsOptimization_Opposite_Predictions$ID))){
# 
#   EffectAccuracy = (ResultsOptimization_Opposite_Accuracy_Predictions %>% filter(ID == i))$Minimum_Accuracy
# 
#   Optim_Opposite = optimize(GetRMSE_Prediction,
#                             c(-1,1),
#                             SelfMotionEffectAccuracy = EffectAccuracy,
#                             Predictions = Prediction %>% filter(ID == i & Congruent != "Same Direction"),
#                             TypeOfRMSE = "Precision")
# 
#   ResultsOptimization_Opposite_Predictions = rbind(ResultsOptimization_Opposite_Predictions,
#                                                    c(Optim_Opposite$minimum,Optim_Opposite$objective,i))
#   save(ResultsOptimization_Opposite_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Predictions.RData"))
# }
# 
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Predictions.RData"))
# 
# ResultsOptimization_Opposite_Predictions$Objectives_Opposite_Accuracy = ResultsOptimization_Opposite_Accuracy_Predictions$AccuracyObjective
# ResultsOptimization_Opposite_Predictions$AccuracyEffectFitted = ResultsOptimization_Opposite_Accuracy_Predictions$Minimum_Accuracy
# colnames(ResultsOptimization_Opposite_Predictions) = c("PrecisionEffectFitted", "PrecisionObjective","ID","AccuracyEffectFitted","AccuracyObjective")
# save(ResultsOptimization_Opposite_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Predictions.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Predictions.RData"))


#############################################################################################################################
########Prediction Task
#####Same Direction

# ResultsOptimization_Same_Accuracy_Predictions = data.frame()
# for (i in setdiff(unique(Prediction$ID), unique(ResultsOptimization_Same_Accuracy_Predictions$ID))){
# 
#   Optim_Same = optimize(GetRMSE_Prediction,
#                             c(-1,1),
#                             SelfMotionEffectPrecision = 0,
#                             Predictions = Prediction %>% filter(ID == i & Congruent != "Opposite Directions"),
#                             TypeOfRMSE = "Accuracy")
# 
#   ResultsOptimization_Same_Accuracy_Predictions = rbind(ResultsOptimization_Same_Accuracy_Predictions,
#                                                               c(Optim_Same$minimum,Optim_Same$objective,i))
#   save(ResultsOptimization_Same_Accuracy_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Accuracy_Predictions.RData"))
# }
# colnames(ResultsOptimization_Same_Accuracy_Predictions) = c("Minimum_Accuracy","AccuracyObjective", "ID")
# save(ResultsOptimization_Same_Accuracy_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Accuracy_Predictions.RData"))
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Accuracy_Predictions.RData"))


#Precision
# ResultsOptimization_Same_Predictions = data.frame()
# for (i in setdiff(unique(Prediction$ID), unique(ResultsOptimization_Same_Predictions$ID))){
# 
#   EffectAccuracy = (ResultsOptimization_Same_Accuracy_Predictions %>% filter(ID == i))$Minimum_Accuracy[1]
# 
#   Optim_Same = optimize(GetRMSE_Prediction,
#                             c(-1,1),
#                             SelfMotionEffectAccuracy = EffectAccuracy,
#                             Predictions = Prediction %>% filter(ID == i & Congruent != "Opposite Directions"),
#                             TypeOfRMSE = "Precision")
# 
#   ResultsOptimization_Same_Predictions = rbind(ResultsOptimization_Same_Predictions,
#                                                      c(Optim_Same$minimum,Optim_Same$objective,i))
#   save(ResultsOptimization_Same_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Predictions.RData"))
# }
# 
# ResultsOptimization_Same_Predictions$Objectives_Opposite_Accuracy = ResultsOptimization_Same_Accuracy_Predictions$AccuracyObjective
# ResultsOptimization_Same_Predictions$AccuracyEffectFitted = ResultsOptimization_Same_Accuracy_Predictions$Minimum_Accuracy
# colnames(ResultsOptimization_Same_Predictions) = c("PrecisionEffectFitted", "PrecisionObjective","ID","AccuracyObjective","AccuracyEffectFitted")
# save(ResultsOptimization_Same_Predictions, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Predictions.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Predictions.RData"))

###############################################################################
###############################################################################




####################################################
##################Speed Estimation##################
####################################################
# GetRMSE_SpeedEstimation = function(SelfMotionEffectAccuracy,
#                                    SelfMotionEffectPrecision,
#                                    Results_SpeedEstimation,
#                                    TypeOfRMSE,
#                                    Multiplicator_PSE_Standard){
# 
#   # SelfMotionEffectAccuracy = x[1]
#   # SelfMotionEffectPrecision = x[2]
# 
#   if (is.nan(Multiplicator_PSE_Standard)){
#     1000
#   } else{
# 
#   Psychometric = expand.grid(Participant=unique(Results_SpeedEstimation$subject),
#                              ConditionOfInterest=unique(Results_SpeedEstimation$Congruent),
#                              StandardValues=unique(Results_SpeedEstimation$velH),
#                              reps = 1:70) %>%
#     mutate(EffectID_PSE = SelfMotionEffectAccuracy,
#            EffectID_JND = SelfMotionEffectPrecision,
#            PSE_ID = 1,
#            SD_ID = 1)
# 
#   Psychometric = Psychometric %>%
#     mutate(PSE_Factor_ID = PSE_ID, #how much variability is in the means of the psychometric functions between subjects?
#            SD_Factor_ID = SD_ID) #how much variability is in the standard deviations of the psychometric functions between subjects?
# 
#   Multiplicator_PSE_Standard = Multiplicator_PSE_Standard
#   Multiplicator_SD_Standard = WFtoSD(0.1)
# 
#   RMSE_PSE_Vector = c()
#   RMSE_SD_Vector = c()
#   RMSE_Vector = c()
# 
# 
#   for (iteration in 1:25){
# 
#     Psychometric = Psychometric %>%
#       mutate(
#         Mean_Standard = StandardValues*Multiplicator_PSE_Standard, #get the mean of the psychometric function for the baseline condition
#         SD_Standard = StandardValues*Multiplicator_SD_Standard, #get the standard deviation of the psychometric function for the baseline condition
#         Mean = case_when(ConditionOfInterest == "No Motion" ~ Mean_Standard*PSE_Factor_ID,
#                          ConditionOfInterest != "No Motion" ~ (Mean_Standard + Mean_Standard*EffectID_PSE)*PSE_Factor_ID),#same but for condition of interest
#         SD = case_when(ConditionOfInterest == "No Motion" ~ abs(SD_Standard*SD_Factor_ID),
#                        ConditionOfInterest != "No Motion" ~ abs(SD_Standard + EffectID_JND*SD_Standard)*SD_Factor_ID))#same but for condition of interest
# 
#     SD_ResponseFunction = 0.1
# 
#     Psychometric = Psychometric %>%
#       mutate(
#         #same but values drawn from a Cauchy function
#         staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction))
# 
#     Psychometric = Psychometric %>%
#       mutate(Presented_TestStimulusStrength = Mean*staircase_factor, #which stimulus strengths are shown? transform values from above (standardized to 1) to the stimulus strengths in condition of interest
#              AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD), #choose for each difference how likely the participant is to choose one or the other as more intense
#              ##get binary answers ("Test was stronger" yes/no) from probabilities for each trial
#              Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability)) #draw answers based on probability
#       )
# 
#     #####fit psychometric functions
#     Parameters = quickpsy::quickpsy(Psychometric %>% filter(Presented_TestStimulusStrength > 0.33 * StandardValues & Presented_TestStimulusStrength < 3*StandardValues),
#                                     Presented_TestStimulusStrength,
#                                     Answer,
#                                     grouping = .(Participant,ConditionOfInterest,StandardValues),
#                                     bootstrap = "none")$par
# 
#     Parameters2 = Parameters %>%
#       filter(parn == "p1") %>%
#       select(Participant,ConditionOfInterest,Mean_Simulated=par, StandardValues)
#     Parameters2$SD_Simulated = Parameters$par[Parameters$parn == "p2"]
# 
#     for (i in 1:length(Results_SpeedEstimation$velH)){
#       Mean = Results_SpeedEstimation$Mean[i]
#       SD = Results_SpeedEstimation$SD[i]
#       StandardValues = Results_SpeedEstimation$velH[i]
#       ConditionOfInterest = Results_SpeedEstimation$Congruent[i]
# 
#       Parameters2$Mean_Observed[Parameters2$ConditionOfInterest == ConditionOfInterest & Parameters2$StandardValues == StandardValues] = Mean
#       Parameters2$SD_Observed[Parameters2$ConditionOfInterest == ConditionOfInterest & Parameters2$StandardValues == StandardValues] = SD
#     }
# 
#     ####Pre-work to get the values needed for correlation stuff (Hypotheses 3a and 3b)
#     FittedPsychometricFunctions = Parameters2 %>%
#       group_by(Participant,StandardValues) %>%
#       ###make new columns where we get the mean PSEs/JNDs for the perceived speed (speed estimation task) for each condition and participant
#       mutate(PSE_Test_Observed = mean(Mean_Observed[ConditionOfInterest != "No Motion"], na.rm = TRUE),
#              SD_Test_Observed = mean(SD_Observed[ConditionOfInterest != "No Motion"], na.rm = TRUE),
#              PSE_Static_Observed = mean(Mean_Observed[ConditionOfInterest == "No Motion"], na.rm = TRUE),
#              SD_Static_Observed = mean(SD_Observed[ConditionOfInterest == "No Motion"], na.rm = TRUE),
#              PSE_Test_Simulated = mean(Mean_Simulated[ConditionOfInterest != "No Motion"], na.rm = TRUE),
#              SD_Test_Simulated = mean(SD_Simulated[ConditionOfInterest != "No Motion"], na.rm = TRUE),
#              PSE_Static_Simulated = mean(Mean_Simulated[ConditionOfInterest == "No Motion"], na.rm = TRUE),
#              SD_Static_Simulated = mean(SD_Simulated[ConditionOfInterest == "No Motion"], na.rm = TRUE)) %>%
#       slice(1)
# 
#     #make yet more columns where we get the difference between the Opposite/Same and the Static condition in terms of the means and standard deviations computed above
#     FittedPsychometricFunctions = FittedPsychometricFunctions %>%
#       mutate(Diff_PSE_Test_Observed = PSE_Test_Observed-PSE_Static_Observed,
#              Diff_SD_Test_Observed = SD_Test_Observed-SD_Static_Observed,
#              Diff_PSE_Test_Simulated = PSE_Test_Simulated-PSE_Static_Simulated,
#              Diff_SD_Test_Simulated = SD_Test_Simulated-SD_Static_Simulated)
# 
#     RMSE_PSE = median((mean(FittedPsychometricFunctions$Diff_PSE_Test_Observed, na.rm = TRUE)-mean(FittedPsychometricFunctions$Diff_PSE_Test_Simulated,na.rm = TRUE))^2)^0.5
#     RMSE_SD = median((mean(FittedPsychometricFunctions$Diff_SD_Test_Observed, na.rm = TRUE)-mean(FittedPsychometricFunctions$Diff_SD_Test_Simulated, na.rm = TRUE))^2)^0.5
#     RMSE = median(c(RMSE_PSE,RMSE_SD))
# 
#     RMSE_PSE_Vector = c(RMSE_PSE_Vector,RMSE_PSE)
#     RMSE_SD_Vector = c(RMSE_SD_Vector,RMSE_SD)
#     RMSE_Vector = c(RMSE_Vector,RMSE)
# 
#     if(iteration %% 25 == 0){
#       print(iteration)
#       print(paste0("SelfMotionEffect_Accuracy: ", SelfMotionEffectAccuracy))
#       print(paste0("SelfMotionEffectPrecision: ", SelfMotionEffectPrecision))
#       print(paste0("ID: ", unique(FittedPsychometricFunctions$Participant)))
#     }
#   }
# 
#   if(TypeOfRMSE == "Overall"){
#     print(paste0("Mean RMSE: ", mean(RMSE_Vector,na.rm = TRUE)))
#     median(RMSE_Vector,na.rm = TRUE)
#   } else if (TypeOfRMSE == "Accuracy"){
#     print(paste0("RMSE_PSE_Vector: ", mean(RMSE_PSE_Vector,na.rm = TRUE)))
#     median(RMSE_PSE_Vector,na.rm = TRUE)
#   } else if (TypeOfRMSE == "Precision"){
#     print(paste0("RMSE_SD_Vector: ", mean(RMSE_SD_Vector,na.rm = TRUE)))
#     median(RMSE_SD_Vector,na.rm = TRUE)
#   } else{
#     print("Lol no. This TypeOfRMSE won't do. Try with 'Overall', 'Accuracy', or 'Precision'.")
#   }
# 
#   }
# }

##############################################################################################################################
#########Speed Task
######Opposite Directions
#Accuracy

# ResultsOptimization_Opposite_Accuracy_Speed = data.frame()
# for (i in setdiff(unique(FittedPsychometricFunctions_Analysis$subject), unique(ResultsOptimization_Opposite_Accuracy_Speed$ID))){
#   
#   
#   if(!is.na(mean((FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent != "Same Direction"))$Mean))){
#     
#   Optim_Opposite = optimize(GetRMSE_SpeedEstimation,
#                             c(-1,1),
#                             SelfMotionEffectPrecision = 0,
#                             Results_SpeedEstimation = FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent != "Same Direction"),
#                             TypeOfRMSE = "Accuracy",
#                             Multiplicator_PSE_Standard = mean((FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent == "No Motion"))$Mean, na.rm = TRUE)/5)
# 
#   ResultsOptimization_Opposite_Accuracy_Speed = rbind(ResultsOptimization_Opposite_Accuracy_Speed,
#                                                                 c(Optim_Opposite$minimum,Optim_Opposite$objective,i))
#   save(ResultsOptimization_Opposite_Accuracy_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Accuracy_Speed.RData"))}
#   
#   else{
#     ResultsOptimization_Opposite_Accuracy_Speed = rbind(ResultsOptimization_Opposite_Accuracy_Speed,
#                                                         c(NA,NA,i))
#     save(ResultsOptimization_Opposite_Accuracy_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Accuracy_Speed.RData"))}
# }
# colnames(ResultsOptimization_Opposite_Accuracy_Speed) = c("MinimumAcc","ObjectiveAcc","ID")
# save(ResultsOptimization_Opposite_Accuracy_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Accuracy_Speed.RData"))
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Accuracy_Speed.RData"))
# 
# 
# #Precision
# ResultsOptimization_Opposite_Speed = data.frame()
# for (i in setdiff(unique(FittedPsychometricFunctions_Analysis$subject), unique(ResultsOptimization_Opposite_Speed$ID))){
# 
#   EffectAccuracy = (ResultsOptimization_Opposite_Accuracy_Speed %>% filter(ID == i))$MinimumAcc
# 
#   if (!is.na(EffectAccuracy) & EffectAccuracy != 1000){
# 
#   Optim_Opposite = optimize(GetRMSE_SpeedEstimation,
#                             c(-1,1),
#                             SelfMotionEffectAccuracy = EffectAccuracy,
#                             Results_SpeedEstimation = FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent != "Same Direction"),
#                             TypeOfRMSE = "Precision",
#                             Multiplicator_PSE_Standard = mean((FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent == "No Motion"))$Mean, na.rm = TRUE)/5)}
# 
#   ResultsOptimization_Opposite_Speed = rbind(ResultsOptimization_Opposite_Speed,
#                                                       c(Optim_Opposite$minimum,Optim_Opposite$objective,i))
#   save(ResultsOptimization_Opposite_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                              "/SavedVariables/ResultsOptimization_Opposite_Speed.RData"))
# }
# ResultsOptimization_Opposite_Speed$Objectives_Opposite_Accuracy = ResultsOptimization_Opposite_Accuracy_Speed$ObjectiveAcc
# ResultsOptimization_Opposite_Speed$AccuracyEffectFitted = ResultsOptimization_Opposite_Accuracy_Speed$MinimumAcc
# colnames(ResultsOptimization_Opposite_Speed) = c("PrecisionEffectFitted", "PrecisionObjective","ID","AccuracyObjective","AccuracyEffectFitted")
# ResultsOptimization_Opposite_Speed = ResultsOptimization_Opposite_Speed[complete.cases(ResultsOptimization_Opposite_Speed),]
# ResultsOptimization_Opposite_Speed = ResultsOptimization_Opposite_Speed %>% filter(PrecisionObjective != 1000) 
# save(ResultsOptimization_Opposite_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                    "/SavedVariables/ResultsOptimization_Opposite_Speed.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Opposite_Speed.RData"))


# ##############################################################################################################################
# #########Motion Estimation Task
# ######Same Direction
# #Accuracy
# ResultsOptimization_Same_Accuracy_Speed = data.frame()
# for (i in setdiff(unique(FittedPsychometricFunctions_Analysis$subject), unique(ResultsOptimization_Same_Accuracy_Speed$ID))){
# 
#   if(!is.na(mean((FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent != "Opposite Directions"))$Mean))){
#   
#   Optim_Same = optimize(GetRMSE_SpeedEstimation,
#                             c(-1,1),
#                             SelfMotionEffectPrecision = 0,
#                         Results_SpeedEstimation = FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent != "Opposite Directions"),
#                             TypeOfRMSE = "Accuracy",
#                         Multiplicator_PSE_Standard = mean((FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent == "No Motion"))$Mean, na.rm = TRUE)/5)
#   
#   ResultsOptimization_Same_Accuracy_Speed = rbind(ResultsOptimization_Same_Accuracy_Speed,
#                                                   c(Optim_Same$minimum,Optim_Same$objective,i))
#   save(ResultsOptimization_Same_Accuracy_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Accuracy_Speed.RData"))}
#   
#   else{
#     ResultsOptimization_Same_Accuracy_Speed = rbind(ResultsOptimization_Same_Accuracy_Speed,
#                                                     c(NA,NA,i))
#     save(ResultsOptimization_Same_Accuracy_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Accuracy_Speed.RData"))  
#   }
# 
# 
# }
# 
# colnames(ResultsOptimization_Same_Accuracy_Speed) = c("MinimumAcc","ObjectiveAcc","ID")
# load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Accuracy_Speed.RData"))


#Precision
# ResultsOptimization_Same_Speed = data.frame()
# for (i in setdiff(unique(FittedPsychometricFunctions_Analysis$subject), unique(ResultsOptimization_Same_Speed$ID))){
# 
#   EffectAccuracy = (ResultsOptimization_Same_Accuracy_Speed %>% filter(ID == i))$MinimumAcc
# 
#   if (!is.na(EffectAccuracy) & EffectAccuracy != 1000){
# 
#   Optim_Same = optimize(GetRMSE_SpeedEstimation,
#                             c(-1,1),
#                             SelfMotionEffectAccuracy = EffectAccuracy,
#                             Results_SpeedEstimation = FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent != "Opposite Directions"),
#                             TypeOfRMSE = "Precision",
#                             Multiplicator_PSE_Standard = mean((FittedPsychometricFunctions_Analysis %>% filter(subject == i & Congruent == "No Motion"))$Mean, na.rm = TRUE)/5)}
# 
#   ResultsOptimization_Same_Speed = rbind(ResultsOptimization_Same_Speed,
#                                              c(Optim_Same$minimum,Optim_Same$objective,i))
#   save(ResultsOptimization_Same_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                          "/SavedVariables/ResultsOptimization_Same_Speed.RData"))
# 
# }
# 
# ResultsOptimization_Same_Speed$Objectives_Opposite_Accuracy = ResultsOptimization_Same_Accuracy_Speed$ObjectiveAcc
# ResultsOptimization_Same_Speed$AccuracyEffectFitted = ResultsOptimization_Same_Accuracy_Speed$MinimumAcc
# colnames(ResultsOptimization_Same_Speed) = c("PrecisionEffectFitted", "PrecisionObjective","ID","AccuracyObjective","AccuracyEffectFitted")
# ResultsOptimization_Same_Speed = ResultsOptimization_Same_Speed[complete.cases(ResultsOptimization_Same_Speed),]
# ResultsOptimization_Same_Speed = ResultsOptimization_Same_Speed %>% filter(PrecisionObjective != 1000)
# save(ResultsOptimization_Same_Speed, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                    "/SavedVariables/ResultsOptimization_Same_Speed.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ResultsOptimization_Same_Speed.RData"))

DF_Correlation_Opposite = inner_join(ResultsOptimization_Opposite_Predictions,ResultsOptimization_Opposite_Speed, by = "ID")
DF_Correlation_Same = inner_join(ResultsOptimization_Same_Predictions,ResultsOptimization_Same_Speed, by = "ID")

Range_Prediction_Opposite_Accuracy = max(ResultsOptimization_Opposite_Predictions$AccuracyEffectFitted) - min(ResultsOptimization_Opposite_Predictions$AccuracyEffectFitted) 
Prediction_Opposite_Accuracy = ggplot(ResultsOptimization_Opposite_Predictions,aes(AccuracyEffectFitted)) +
  stat_dots(side = "right", binwidth = Range_Prediction_Opposite_Accuracy/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = mean(ResultsOptimization_Opposite_Predictions$AccuracyEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Opposite_Predictions$AccuracyEffectFitted))
ggsave("Figures/(Figure 10) Prediction_Opposite_Accuracy.jpg", w = 5, h = 2.5)

Range_Prediction_Opposite_Precision = range(ResultsOptimization_Opposite_Predictions$PrecisionEffectFitted)[2]-range(ResultsOptimization_Opposite_Predictions$PrecisionEffectFitted)[1]
Prediction_Opposite_Precision = ggplot(ResultsOptimization_Opposite_Predictions,aes(PrecisionEffectFitted)) +
  stat_dots(side = "right", binwidth = Range_Prediction_Opposite_Precision/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = mean(ResultsOptimization_Opposite_Predictions$PrecisionEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Opposite_Predictions$PrecisionEffectFitted))
ggsave("Figures/(Figure 10) Prediction_Opposite_Precision.jpg", w = 5, h = 2.5)

Range_Prediction_Same_Accuracy = max(DF_Correlation_Same$AccuracyEffectFitted.x) - min(DF_Correlation_Same$AccuracyEffectFitted.x) 
Prediction_Same_Accuracy = ggplot(ResultsOptimization_Same_Predictions,aes(AccuracyEffectFitted)) +
  stat_dots(side = "right", binwidth = Range_Prediction_Same_Accuracy/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = mean(ResultsOptimization_Same_Predictions$AccuracyEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(DF_Correlation_Same$AccuracyEffectFitted.x))
ggsave("Figures/(Figure 10) Prediction_Same_Accuracy.jpg", w = 5, h = 2.5)

Range_Prediction_Same_Precision = max(ResultsOptimization_Same_Predictions$PrecisionEffectFitted) - min(ResultsOptimization_Same_Predictions$PrecisionEffectFitted) 
Prediction_Same_Precision = ggplot(ResultsOptimization_Same_Predictions,aes(PrecisionEffectFitted)) +
  stat_dots(side = "right", binwidth = Range_Prediction_Same_Precision/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = mean(ResultsOptimization_Same_Predictions$PrecisionEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Same_Predictions$PrecisionEffectFitted))
ggsave("Figures/(Figure 10) Prediction_Same_Precision.jpg", w = 5, h = 2.5)

Range_Speed_Opposite_Accuracy = max(ResultsOptimization_Opposite_Speed$AccuracyEffectFitted) - min(ResultsOptimization_Opposite_Speed$AccuracyEffectFitted) 
Speed_Opposite_Accuracy = ggplot(ResultsOptimization_Opposite_Speed,aes(AccuracyEffectFitted)) +
  stat_dots(side = "left", binwidth = Range_Speed_Opposite_Accuracy/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = median((ResultsOptimization_Opposite_Speed)$AccuracyEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Opposite_Speed$AccuracyEffectFitted))
ggsave("Figures/(Figure 10) Speed_Opposite_Accuracy.jpg", w = 5, h = 2.5)

Range_Speed_Opposite_Precision = max(ResultsOptimization_Opposite_Speed$PrecisionEffectFitted) - min(ResultsOptimization_Opposite_Speed$PrecisionEffectFitted) 
Speed_Opposite_Precision = ggplot(ResultsOptimization_Opposite_Speed,aes(PrecisionEffectFitted)) +
  stat_dots(side = "left", binwidth = Range_Speed_Opposite_Precision/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = median((ResultsOptimization_Opposite_Speed %>% filter(AccuracyObjective != 1000))$PrecisionEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Opposite_Speed$PrecisionEffectFitted))
ggsave("Figures/(Figure 10) Speed_Opposite_Precision.jpg", w = 5, h = 2.5)

Range_Speed_Same_Accuracy = max(ResultsOptimization_Same_Speed$AccuracyEffectFitted) - min(ResultsOptimization_Same_Speed$AccuracyEffectFitted) 
Speed_Same_Accuracy = ggplot(ResultsOptimization_Same_Speed,aes(AccuracyEffectFitted)) +
  stat_dots(side = "left", binwidth = Range_Speed_Same_Accuracy/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = median((ResultsOptimization_Same_Speed)$AccuracyEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Same_Speed$AccuracyEffectFitted))
ggsave("Figures/(Figure 10) Speed_Same_Accuracy.jpg", w = 5, h = 2.5)

Range_Speed_Same_Precision = max(ResultsOptimization_Same_Speed$PrecisionEffectFitted) - min(ResultsOptimization_Same_Speed$PrecisionEffectFitted) 
Speed_Same_Precision = ggplot(ResultsOptimization_Same_Speed,aes(PrecisionEffectFitted)) +
  stat_dots(side = "left", binwidth = Range_Speed_Same_Precision/20, size = 2) +
  geom_vline(xintercept = 0,
             linewidth = 2,
             linetype = 2) +
  geom_vline(xintercept = median((ResultsOptimization_Same_Speed)$PrecisionEffectFitted),
             color = "red",
             linetype = 2,
             linewidth = 2) +
  xlab("Effect of Self-Motion") +
  ylab("Density") +
  xlim(range(ResultsOptimization_Same_Speed$PrecisionEffectFitted))
ggsave("Figures/(Figure 10) Speed_Same_Precision.jpg", w = 5, h = 2.5)

Correlation_Opposite_Accuracy = ggplot(DF_Correlation_Opposite,aes(AccuracyEffectFitted.x,AccuracyEffectFitted.y)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", linewidth = 2) +
  xlim(range(DF_Correlation_Opposite$AccuracyEffectFitted.x)) +
  ylim(range(DF_Correlation_Opposite$AccuracyEffectFitted.y)) +
  xlab("Effect of Self-Motion in Prediction") +
  ylab("Effect of Self-Motion in Speed Estimation")
ggsave("Figures/(Figure 10) Correlation_Opposite_Accuracy.jpg", w = 5, h = 5)

Correlation_Opposite_Precision = ggplot(DF_Correlation_Opposite,aes(PrecisionEffectFitted.x,PrecisionEffectFitted.y)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", linewidth = 2) +
  xlab("Effect of Self-Motion in Prediction") +
  ylab("Effect of Self-Motion in Speed Estimation") +
  xlim(range(DF_Correlation_Opposite$PrecisionEffectFitted.x)) +
  ylim(range(DF_Correlation_Opposite$PrecisionEffectFitted.y))
ggsave("Figures/(Figure 10) Correlation_Opposite_Precision.jpg", w = 5, h = 5)

Correlation_Same_Accuracy = ggplot(DF_Correlation_Same,aes(AccuracyEffectFitted.x,AccuracyEffectFitted.y)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", linewidth = 2) +
  xlab("Effect of Self-Motion in Prediction") +
  ylab("Effect of Self-Motion in Speed Estimation") +
  xlim(range(DF_Correlation_Same$AccuracyEffectFitted.x)) +
  ylim(range(DF_Correlation_Same$AccuracyEffectFitted.y))
ggsave("Figures/(Figure 10) Correlation_Same_Accuracy.jpg", w = 5, h = 5)

Correlation_Same_Precision = ggplot(DF_Correlation_Same,aes(PrecisionEffectFitted.x,PrecisionEffectFitted.y)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", linewidth = 2) +
  xlab("Effect of Self-Motion in Prediction") +
  ylab("Effect of Self-Motion in Speed Estimation") +
  xlim(range(DF_Correlation_Same$PrecisionEffectFitted.x)) +
  ylim(range(DF_Correlation_Same$PrecisionEffectFitted.y))
ggsave("Figures/(Figure 10) Correlation_Same_Precision.jpg", w = 5, h = 5)


#Prediction
Stats_Prediction_Opposite_Accuracy = lm(AccuracyEffectFitted ~ 1,
                             data = ResultsOptimization_Opposite_Predictions)
summary(Stats_Prediction_Opposite_Accuracy)

Stats_Prediction_Opposite_Precision = lm(PrecisionEffectFitted ~ 1,
                                        data = ResultsOptimization_Opposite_Predictions)
summary(Stats_Prediction_Opposite_Precision)

Stats_Prediction_Same_Accuracy = lm(AccuracyEffectFitted ~ 1,
                                        data = ResultsOptimization_Same_Predictions)
summary(Stats_Prediction_Same_Accuracy)

Stats_Prediction_Same_Precision = lm(PrecisionEffectFitted ~ 1,
                                         data = ResultsOptimization_Same_Predictions)
summary(Stats_Prediction_Same_Precision)


#Speed
Stats_Speed_Opposite_Accuracy = lm(AccuracyEffectFitted ~ 1,
                                        data = ResultsOptimization_Opposite_Speed)
summary(Stats_Speed_Opposite_Accuracy)

Stats_Speed_Opposite_Precision = lm(PrecisionEffectFitted ~ 1,
                                         data = ResultsOptimization_Opposite_Speed)
summary(Stats_Speed_Opposite_Precision)

Stats_Speed_Same_Accuracy = lm(AccuracyEffectFitted ~ 1,
                                    data = ResultsOptimization_Same_Speed)
summary(Stats_Speed_Same_Accuracy)

Stats_Speed_Same_Precision = lm(PrecisionEffectFitted ~ 1,
                                     data = ResultsOptimization_Same_Speed)
summary(Stats_Speed_Same_Precision)





#Correlations
Stats_Opposite_Accuracy = lm(AccuracyEffectFitted.x ~ AccuracyEffectFitted.y,
                             data = DF_Correlation_Opposite)
summary(Stats_Opposite_Accuracy)

Stats_Opposite_Precision = lm(PrecisionEffectFitted.x ~ PrecisionEffectFitted.y,
                             data = DF_Correlation_Opposite)
summary(Stats_Opposite_Precision)

Stats_Same_Accuracy = lm(AccuracyEffectFitted.x ~ AccuracyEffectFitted.y,
                             data = DF_Correlation_Same)
summary(Stats_Same_Accuracy)

Stats_Same_Precision = lm(PrecisionEffectFitted.x ~ PrecisionEffectFitted.y,
                              data = DF_Correlation_Same)
summary(Stats_Same_Precision)


#################Try the potential explanation
require(ggplot2)
require(dplyr)
SimmedData = expand.grid(ActualTTC = seq(0,5,0.01),
                        MotionProfile = c("No Motion","Same Direction", "Opposite Directions"))


SimmedData = SimmedData %>%
  mutate(PerceivedTTCNoBayes = case_when(
    MotionProfile == "No Motion" ~ dnorm(ActualTTC, 2, 1),
    MotionProfile == "Same Direction" ~ dnorm(ActualTTC, 1.7, 0.3),
    MotionProfile == "Opposite Directions" ~ dnorm(ActualTTC, 2.5, 1.3)))


Parameters = data.frame(MotionProfile = c("No Motion","Same Direction", "Opposite Directions"),
                        PriorMean = 2,
                        PriorSD = 0.3) %>% 
  mutate(SD = case_when(
            MotionProfile == "No Motion" ~ 30*0.02,
            MotionProfile == "Same Direction" ~ 5*0.02 + 0.3,
            MotionProfile == "Opposite Directions" ~ 60*0.02 + 0.3),
         Mean = case_when(
            MotionProfile == "No Motion" ~ 1,
            MotionProfile == "Same Direction" ~ 0.9,
            MotionProfile == "Opposite Directions" ~ 1.1),
    PosteriorMeans = SD*Mean/(SD+PriorSD) + 
           PriorSD*PriorMean/(SD+PriorSD),
         PosteriorSDs = SD*PriorSD)


ggplot(Parameters, aes(MotionProfile,PosteriorSDs)) +
  geom_point(size = 5)

ggplot(Parameters, aes(MotionProfile,PosteriorMeans)) +
  geom_point(size = 5)

Prediction = Prediction %>%
  group_by(ID, velH, velH_Subject, Occlusion_Duration) %>%
  mutate(Deviation = abs(Response_Time - Occlusion_Duration - mean(Response_Time - Occlusion_Duration)))

ggplot(Prediction %>% filter(Congruent == "Observer Static"),aes(Occlusion_Duration,Deviation)) +
  geom_point() +
  geom_smooth()

lm(Deviation ~ abs(velH),
   data = Prediction %>% filter(Congruent == "Observer Static"))


UGH = expand.grid(velH = c(-10, -3, 3, 10),
                  velH_Self = c(-9,0,9),
                  t = seq(0,0.5,0.01)) %>%
  mutate(StartX = case_when(
                  velH >= velH_Self ~ 5,
                  TRUE ~ 5 + (velH_Self - velH)*0.5),
         Pos_tar = StartX + t*velH,
         Pos_self = t*velH_Self,
         Congruent = case_when(
           velH*velH_Self == 0 ~ "Static",
           velH*velH_Self > 0 ~ "Same",
           velH*velH_Self < 0 ~ "Opposite"))

ggplot(UGH,aes(t, Pos_tar-Pos_self, color = as.factor(velH))) +
  geom_point() +
  facet_grid(velH_Self~.)
