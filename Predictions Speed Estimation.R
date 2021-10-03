require(dplyr)
require(ggplot)
require(lme4)
require(lmerTest)
require(quickpsy)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("SimulateDataFunction.R")
set.seed(25)

Power_SpeedEstimation = data.frame()

RangeParticipants = c(10,15,20,25,30)
RangeReps = c(50,70,90)
nIterations = 1

for (i in RangeParticipants){
  
  print(paste0(i, " participants"))
  
  
  for (k in RangeReps){
    
    print(paste0(k, " trials"))
  
    for (j in 1:nIterations) {
      
      print(paste0(j, " iteration(s)"))
      
      SimedData = SimulatePsychometricData(nParticipants = i,
                                           ConditionOfInterest = c(0,1),
                                           StandardValues = c(4, 5, 6),
                                           reps = k,
                                           PSE_Difference = -0.1,
                                           JND_Difference = -0.1,
                                           Multiplicator_PSE_Standard = 0,
                                           Multiplicator_SD_Standard = 0.2,
                                           Type_ResponseFunction = "Cauchy",
                                           SD_ResponseFunction = 0.08,
                                           Mean_Variability_Between = 0.2,
                                           SD_Variability_Between = 0.2)

      (Parameters = quickpsy(SimedData,Difference,Answer,
                             grouping = .(ID,ConditionOfInterest,StandardValues), 
                             bootstrap = "none")$par)
      Parameters2 = Parameters %>%
        filter(parn == "p1") %>%
        select(ID,ConditionOfInterest,Mean=par, StandardValues)
      Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
      FittedPsychometricFunctions = Parameters2

      LMM_Mean = lmer(Mean ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                 data = FittedPsychometricFunctions)
      
      LMM_SD = lmer(SD ~ ConditionOfInterest + (1 | ID) + (1 | StandardValues),
                 data = FittedPsychometricFunctions)
      
      summary(GLMM)$coef[15]
      summary(GLMM)$coef[16]
          
      Power_SpeedEstimation = rbind(Power_SpeedEstimation,
                                data.frame(nParticipants=rep(i,4),
                                  reps=rep(k,4), 
                                  pvalue = c(summary(GLMM)$coefficients[15],
                                             summary(GLMM)$coefficients[16],
                                             summary(LMM_Mean)$coef[10],
                                             summary(LMM_SD)$coef[10]),
                                  iteration = rep(j,4),
                                  Label = c("PSE (GLMM)","JND (GLMM)","PSE (LMM)","SD (LMM)")))
    }
  }
}


save(Power_SpeedEstimation, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                   "/SavedVariables/Power_SpeedEstimation.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Power_SpeedEstimation.RData"))


#do everything at alpha level of 0.05
alpha = 0.05

Power_SpeedEstimation = Power_SpeedEstimation %>% group_by(nParticipants,reps,Label) %>% 
  mutate(Power = mean(pvalue < alpha))

Power_SpeedEstimation = (Power_SpeedEstimation %>% group_by(nParticipants,reps, Label) %>% 
                        slice(1))


ggplot(Power_SpeedEstimation, aes(nParticipants,Power, color = as.factor(reps))) +
  geom_line(size = 1) +
  xlab("Number of Participants") +
  ylab("Power") +
  scale_x_continuous(breaks = RangeParticipants) +
  scale_color_manual(name = "Repetitions\nper Staircase", 
                     values = c(Red,BlauUB,Yellow)) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  facet_wrap(Label~.) +
  theme(legend.position = c(0.4,0.2))
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              "/Figures/Power Analysis Speed Estimation.jpg"), w = 5, h = 5)