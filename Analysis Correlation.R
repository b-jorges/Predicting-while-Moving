require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)
require(brms)
source("Analysis Motion Estimation.r")
source("Analysis Prediction.r")



####Pre-work to get the values needed for correlation stuff
Correlations = Prediction %>%
    group_by(ID,velH,Congruent) %>%
    mutate(MeanError = mean(Response_Time-Occlusion_Duration),
          SDError = sd(Response_Time-Occlusion_Duration),
          velH_Abs = abs(velH)) %>%
    slice(1)

#match data from both tasks in one dataframe
for(i in 1:length(FittedPsychometricFunctions_Analysis$subject)){
  PSE = FittedPsychometricFunctions_Analysis$Mean[i]
  SD = FittedPsychometricFunctions_Analysis$SD[i]
  velH = FittedPsychometricFunctions_Analysis$velH[i]
  MotionProfile = FittedPsychometricFunctions_Analysis$Congruent[i]
  ID = FittedPsychometricFunctions_Analysis$subject[i]

  Correlations$PSE[Correlations$velH_Abs == velH &
                             Correlations$Congruent == MotionProfile &
                             Correlations$ID == ID] = PSE
  Correlations$SD[Correlations$velH_Abs == velH &
                            Correlations$Congruent == MotionProfile &
                            Correlations$ID == ID] = SD
          
}
        

####Statistical tests for correlations
PSE_LMM_Corr = lmer(MeanError ~ PSE + (velH + Occlusion_Duration | ID),
                                    data = Correlations)
PSE_LMM_Corr_CI = confint(PSE_LMM_Corr,method = "boot")

SD_LMM_Corr = lmer(SDError ~ SD + (velH + Occlusion_Duration | ID),
                                   data = Correlations)
SD_LMM_Corr_CI = confint(SD_LMM_Corr,method = "boot")




anova(lm(SDError ~ SD + Occlusion_Duration + ID,
   data = Correlations))

anova(lm(MeanError ~ PSE,
           data = Correlations))




Model0 = lm(MeanError ~ 0,
            data = Correlations)
Res0 = mean(residuals(Model0)^2)

Model1 = lm(MeanError ~ 1,
            data = Correlations)
Res1 = mean(residuals(Model1)^2)

Model2 = lmer(MeanError ~ 1 + (1 | ID),
            data = Correlations)
Res2 = mean(residuals(Model2)^2)

Model3 = lmer(MeanError ~ 1 + (Occlusion_Duration | ID),
              data = Correlations)
Res3 = mean(residuals(Model3)^2)

Model4 = lmer(MeanError ~ 1 + (velH + Occlusion_Duration | ID),
              data = Correlations)
Res4 = mean(residuals(Model4)^2)

Model5 = lmer(MeanError ~ 1 + PSE + (velH + Occlusion_Duration | ID),
              data = Correlations)
Res5 = mean(residuals(Model5)^2)


Res0-Res1
Res1-Res2
Res2-Res3
Res3-Res4
Res4-Res5

vc = VarCorr(Model5)
anova(Model5)

print(vc,comp=c("Variance"))