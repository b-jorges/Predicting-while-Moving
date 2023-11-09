require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
library(scales)
require(ggdist)
require(lmerTest)

set.seed(5)

###
Prediction = read.csv("Data/2_Prediction/Data_Prediction.csv")

#### outlier analysis
nPreOutliers = length(Prediction$velH)
Prediction = Prediction %>% mutate(Congruent = case_when(velH*velH_Subject > 0 ~ "Same Direction",
                                                         velH*velH_Subject < 0 ~ "Opposite Directions",
                                                         velH*velH_Subject == 0 ~ "Observer Static")) %>% 
  filter(Response_Time < 3*Occlusion_Duration) %>% 
  mutate(velH_Abs = abs(velH))
nPostOutliers = length(Prediction$velH)
nPreOutliers-nPostOutliers
100*(nPreOutliers-nPostOutliers)/nPreOutliers

Prediction = Prediction %>% 
  mutate(nOutsideOfRange = (abs(RadiansToDegree(xRotations_first0)) > 2.5) + (abs(RadiansToDegree(xRotations_first1)) > 2.5) + 
                           (abs(RadiansToDegree(xRotations_first2)) > 2.5) + (abs(RadiansToDegree(xRotations_first3)) > 2.5) + 
                           (abs(RadiansToDegree(xRotations_first4)) > 2.5) +
                           (abs(RadiansToDegree(yRotations_first0)) > 2.5) + (abs(RadiansToDegree(yRotations_first1)) > 2.5) + 
                           (abs(RadiansToDegree(yRotations_first2)) > 2.5) + (abs(RadiansToDegree(yRotations_first3)) > 2.5) + 
                           (abs(RadiansToDegree(yRotations_first4)) > 2.5))

Prediction = Prediction %>% filter(nOutsideOfRange < 5)
nPostOutliers2 = length(Prediction$velH)
nPostOutliers - nPostOutliers2
100*(nPostOutliers-nPostOutliers2)/nPostOutliers


####################### Accuracy
####LMM
# Model_Prediction_Accuracy = lmer(Response_Time-Occlusion_Duration ~ Congruent + (velH | ID) + (1 | Occlusion_Duration),
#                                  data = Prediction)
# #fit confidence interval
# Model_Prediction_Accuracy_CI = confint(Model_Prediction_Accuracy,method = "boot")
# 
# save(Model_Prediction_Accuracy, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                               "/SavedVariables/Model_Prediction_Accuracy.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Model_Prediction_Accuracy.RData"))
# save(Model_Prediction_Accuracy_CI, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                         "/SavedVariables/Model_Prediction_Accuracy_CI.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Model_Prediction_Accuracy_CI.RData"))


###################### Precision analysis
#add the extrapolated SD for the corresponding mean timing error
Prediction_SDs = Prediction %>%
  group_by(ID, Congruent, velH_Abs, Occlusion_Duration) %>% 
  mutate(Mean_per_Condition = mean(Response_Time),
         SD_per_Condition = sd(Response_Time)) %>% 
  slice(1)

# precision outliers
100*mean(is.na(Prediction_SDs$SD_per_Condition)) + 100*mean(Prediction_SDs$SD_per_Condition <= 0.01, na.rm = TRUE)
sum(Prediction_SDs$SD_per_Condition <= 0.01, na.rm = TRUE) + sum(is.na(Prediction_SDs$SD_per_Condition))

#Opposite Directions
Model1 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + Congruent + (velH | ID) + (1 | Occlusion_Duration),
              data = Prediction_SDs %>% 
                filter(Congruent != "Same Direction")  %>% 
                filter(SD_per_Condition > 0.01),
              REML = FALSE)
Model2 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + (velH | ID) + (1 | Occlusion_Duration),
              data = Prediction_SDs %>% 
                filter(Congruent != "Same Direction")  %>% 
                filter(SD_per_Condition > 0.01),
              REML = FALSE)
anova(Model1,Model2)
summary(Model1)


#Same Direction
Model3 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + Congruent + (velH | ID) + (1 | Occlusion_Duration),
              data = Prediction_SDs %>% 
                filter(Congruent != "Opposite Directions") %>% 
                filter(SD_per_Condition > 0.01),
              REML = FALSE)
Model4 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + (velH | ID) + (1 | Occlusion_Duration),
              data = Prediction_SDs %>% 
                filter(Congruent != "Opposite Directions") %>% 
                filter(SD_per_Condition > 0.01),
              REML = FALSE)
anova(Model3,Model4)
summary(Model3)

##########plots
Figure_Predictions1 = ggplot(Prediction %>% filter(Occlusion_Duration %in% c(0.5,0.6,0.7)) %>% 
                               mutate(Congruent2 = case_when(
                                 Congruent == "Observer Static" ~ "1Observer Static",
                                 Congruent == "Same Direction" ~ "2Same Direction",
                                 Congruent == "Opposite Directions" ~ "3Opposite Directions")),
                             aes(as.factor(Occlusion_Duration),Response_Time-Occlusion_Duration,col = Congruent2)) +
  geom_boxplot(size = 1.5) +
  xlab("Occlusion Duration (s)") +
  ylab("Error (Perceived Duration - Occluded Duration; s)") +
  scale_color_manual(name = "Motion Profile", values = c("red","blue","orange")) +
  ggtitle("A.") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 2)

Figure_Predictions2 = ggplot(Prediction_SDs %>% filter(Occlusion_Duration %in% c(0.5,0.6,0.7)) %>% 
                               mutate(velH_Factor = paste0(velH_Abs," m/s")) %>% 
                               filter(SD_per_Condition > 0.01) %>% 
                               mutate(Congruent2 = case_when(
                                 Congruent == "Observer Static" ~ "1Observer Static",
                                 Congruent == "Same Direction" ~ "2Same Direction",
                                 Congruent == "Opposite Directions" ~ "3Opposite Directions")),
                             aes(Mean_per_Condition,SD_per_Condition,color = Congruent2)) +
  geom_point(size = 2, alpha = 0.1) +
  geom_smooth(method = "lm",
              formula = y ~ x, se = FALSE) +
  xlab("Mean Extrapolated Duration per Condition and Participant (s)") +
  ylab("SD of Extrapolated Duration per Condition and Participant (s)") +
  scale_color_manual(name = "Motion Profile", values = c("red","blue","orange"), labels = c("Observer Static","Same Direction","Opposite Direction"))  +
  ggtitle("B.")

plot_grid(Figure_Predictions1,Figure_Predictions2, rel_widths = c(0.8,1.1))
ggsave("Figures/(Figure 07) Data Hypotheses 1a and 1b (Prediction).jpg", w = 12, h = 6)