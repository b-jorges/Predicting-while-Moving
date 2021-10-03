require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)

set.seed(4)
velH = c(-6,-5, -4, 4, 5, 6) #m/s
self_velH = c(-1.8,0,1.8) #meters over half a second, = 4m/s on average, but Gaussian motion profile
occlusion_times = c(0.5,0.6,0.7)
Participants = paste0("p",1:20)
reps = 1:10



###########without variability
Predictions_NoVariability = expand.grid(occlusion_time = occlusion_times,
                                        velH = velH[4:6],
                                        self_velH = self_velH)
Predictions_NoVariability = Predictions_NoVariability %>% 
  mutate(Distance = velH * occlusion_time,
         PerceivedOcclusion = case_when(
           self_velH == -1.8 ~ Distance/(velH-self_velH*0.2),
           TRUE ~ occlusion_time),
         ObjectSpeed = paste0(velH," m/s"),
         MotionProfile = case_when(
           self_velH == 1.8 ~ "Same Direction",
           self_velH == 0 ~ "Observer Static",
           self_velH == -1.8 ~ "Opposite Directions"))

ggplot(Predictions_NoVariability,aes(occlusion_time,PerceivedOcclusion-occlusion_time,col = as.factor(MotionProfile))) +
  geom_point(size = 10) +
  facet_grid(.~ObjectSpeed) +
  xlab("Occlusion Duration (s)") +
  ylab("Error (Perceived Duration - Occluded Duration; s)") +
  scale_color_manual(values = c(Red,Yellow,BlauUB), name = "")
ggsave("Figures/(Figure XX) Predictions_Prediction_WithoutVariability.jpg", w = 12, h = 6)



###########with variability
#trials per participant:
NumberTrials = length(reps)*length(occlusion_times)*length(self_velH)*length(velH)

#duration (3s per trial)
3*NumberTrials/60

#Weber Fraction for distance estimation is about 5%
WFtoSD(0.05)

Predictions = expand.grid(velH = velH,
            self_velH = self_velH,
            occlusion_time = occlusion_times,
            Participant = Participants,
            rep = reps)

Predictions = Predictions %>% 
  mutate(SelfmotionDirection = case_when(
            velH*self_velH > 0 ~ "Same Direction",
            velH*self_velH < 0 ~ "Opposite Directions",
            velH*self_velH == 0 ~ "Observer Static"),
         Bias_selfmotion = case_when(
           SelfmotionDirection == "Same Direction" ~ 0*abs(self_velH), #speed is underestimated by 5% of self-motion speed on avg when target and self-motion in same direction
           SelfmotionDirection == "Opposite Directions" ~ 0.2*abs(self_velH), #overestimated by 10% of self-motion speed when in opposite directions
           SelfmotionDirection == "Observer Static" ~ 0), #no bias when no observer motion
        LeftRight = case_when(
            velH > 0 ~ "Right",
            velH < 0 ~ "Left"),
        velH_Abs = abs(velH)) %>% 
  group_by(Participant) %>% 
  mutate(VariabilityPerceivedDistance_SD = rnorm(1,WFtoSD(0.05),0.02), #this is to simulate between-participant variability in the sensitivity to distances
         VariabilityPerceivedDistance_Mean = 1, #here we could introduce a bias term, but we believe that participants estimate the distance accurately
         VariabilityPerceivedSpeed_SD = rnorm(1,WFtoSD(0.1),0.02), #between-participant variability in the sensitivity to speed
         VariabilityPerceivedSpeed_Mean = Bias_selfmotion) %>% #bias term, only bias considered = bias due to self-motion
  mutate(CorrectDistance = velH_Abs*occlusion_time,
         PerceivedDistance = CorrectDistance*rnorm(length(velH),1,VariabilityPerceivedDistance_SD),
         CorrectTime = occlusion_time,
         PerceivedvelH = (velH_Abs + abs(self_velH)*VariabilityPerceivedSpeed_Mean)*rnorm(length(velH),1,VariabilityPerceivedSpeed_SD),
         time_perceived = PerceivedDistance/PerceivedvelH,
         TimingError = time_perceived-CorrectTime)

ggplot(Predictions, aes(as.factor(occlusion_time),TimingError, color = SelfmotionDirection)) +
  geom_boxplot() +
  scale_color_manual(values = c(Red, Yellow,BlauUB),
                     name = "") +
  labs(x = "", y = "Error (Perceived Duration - Occluded Duration; s)") +
  facet_grid(.~velH_Abs)
ggsave("Figures/(Figure XX) Predictions_Prediction_WithVariability.jpg", w = 12, h = 6)