require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)

#set.seed(4)
velH = c(4, 5, 6) #m/s
self_velH = c(-3.6,0,3.6) #1.8 meters over half a second, = 3.6m/s on average (Gaussian motion profile neglected here)
occlusion_times = c(0.5,0.6,0.7) #three occlusion durations
Participants = paste0("p",1:40) #number of participants
reps = 1:13 #number of repetitions for each condition. We have 3 velocities * 3 motion profiles * 3 occlusion times = 9 conditions

#expand a dataframe with all these values
Predictions = expand.grid(velH = velH,
            self_velH = self_velH,
            occlusion_time = occlusion_times,
            Participant = Participants,
            rep = reps)

#we also want higher and lower occlusion durations in the Observer Static motion profile, so we can assess what happens to variability when biases occur
Occlusion_Times_SDControl = c(seq(0.1,0.4,0.1),seq(0.8,1,0.1))

#expand a second data frame with these values:
SD_Control = expand.grid(velH = velH,
                         self_velH = 0,
                         occlusion_time = Occlusion_Times_SDControl,
                         Participant = Participants,
                         rep = reps)

#put both dataframes together
Predictions = rbind(Predictions,SD_Control)

#how many trials per participant:
NumberTrials = length(reps)*length(occlusion_times)*length(self_velH)*length(velH) + length(reps)*length(Occlusion_Times_SDControl)*length(velH)
  
#how long does this task take? (2.5s per trial)
2.5*NumberTrials/60


Predictions = Predictions %>% 
  mutate(SelfmotionDirection = case_when( #make a column with the terminology we will use (observer motion relative to ball motion, we don't care about left or right)
            velH*self_velH > 0 ~ "Same Direction", 
            velH*self_velH < 0 ~ "Opposite Directions",
            velH*self_velH == 0 ~ "Observer Static"),
        velH_Abs = abs(velH)) %>% 
  mutate(Bias_selfmotion = case_when(#add a column with the bias each condition introduces:
    SelfmotionDirection == "Same Direction" ~ 0*abs(self_velH), #speed is estimated correctly for Same Direction
    SelfmotionDirection == "Opposite Directions" ~ 0.2*abs(self_velH), #overestimated by 20% of self-motion speed when in opposite directions
    SelfmotionDirection == "Observer Static" ~ 0), #no bias when no observer motion
    VariabilityDiff_selfmotion = case_when(
      SelfmotionDirection == "Same Direction" ~ 1, #no impact on variability
      SelfmotionDirection == "Opposite Directions" ~ 1.2, #variability 20% higher for Opposite Directions (here we use a multiplier)
      SelfmotionDirection == "Observer Static" ~ 1)) %>%  #no impact on variability
    group_by(Participant) %>% 
      mutate(
        VariabilityPerceivedDistance_SD = rnorm(1,WFtoSD(0.05),0.02), #this is to simulate between-participant variability in the sensitivity to distances
        VariabilityPerceivedDistance_Mean = 1, #here we could introduce a bias term, but we believe that participants estimate the distance accurately
        CorrectDistance = velH_Abs*occlusion_time, #compute correct distance between point of disappearance and target rectangle
        PerceivedDistance = CorrectDistance*rnorm(length(velH),1,VariabilityPerceivedDistance_SD), #use these values to draw one perceived distance per trial. We use a mean of 1 and then multiply because law of Weber: higher values should lead to higher variability in absolute terms, and constant variability in relative terms
        VariabilityPerceivedSpeed_SD = rnorm(1,WFtoSD(0.1),0.02)*VariabilityDiff_selfmotion, #between-participant variability in the sensitivity to speed, get one value per participant
        VariabilityPerceivedSpeed_Mean = Bias_selfmotion, #bias term, only bias considered = bias due to self-motion, could add e.g. Aubert-Fleischl if they were following the ball with their gaze
        CorrectTime = occlusion_time,
        PerceivedvelH = abs((velH_Abs + abs(self_velH)*VariabilityPerceivedSpeed_Mean)*rnorm(length(velH),1,VariabilityPerceivedSpeed_SD)), #draw one perceived speed per trial. Take absolutes because certain values can't be negative. Only very few of these should occur anyway, so this is a quick-and-dirty fix. (Could be solved by using different distributions that can't be negative, but ugh, not worth the extra work, probably)
        time_perceived = PerceivedDistance/PerceivedvelH,
        TimingError = time_perceived-CorrectTime) %>% 
  filter(time_perceived < 3*occlusion_time) #apply same outlier criterion as for final data

#compute mean and standard deviation for the extrapolated time, for each participant and condition separately
Prediction_SDs = Predictions %>%
  group_by(Participant, SelfmotionDirection, velH, occlusion_time) %>% 
  mutate(Mean_per_Condition = mean(time_perceived),
         SD_per_Condition = sd(time_perceived)) %>% 
  slice(1)


#See how the model used for analysis fares on the simulated data. Model for accuracy:
LMM_Prediction_Accuracy = lmer(TimingError ~ SelfmotionDirection + (velH | Participant) + (1 | occlusion_time),
                               data = Predictions)
summary(LMM_Prediction_Accuracy)

#Models for precision
Model1 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + SelfmotionDirection + (velH | Participant) + (1 | occlusion_time),
              data = Prediction_SDs %>%
                filter(SelfmotionDirection != "Same Direction" & SD_per_Condition > 0.1),
              REML = FALSE)
Model2 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + (velH | Participant) + (1 | occlusion_time),
              data = Prediction_SDs %>%
                filter(SelfmotionDirection != "Same Direction" & SD_per_Condition > 0.1),
              REML = FALSE)
anova(Model1,Model2)
summary(Model1)

#Same Direction ("control" to see if everything works alright. Same Direction has, we assume, no affect, so this should come out not significant in 95% of the times)
Model3 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + SelfmotionDirection + (velH | Participant) + (1 | occlusion_time),
              data = Prediction_SDs %>%
                filter(SelfmotionDirection != "Opposite Directions" & SD_per_Condition > 0.1),
              REML = FALSE)
Model4 = lmer(log(SD_per_Condition) ~ Mean_per_Condition + (velH | Participant) + (1 | occlusion_time),
              data = Prediction_SDs %>%
                filter(SelfmotionDirection != "Opposite Directions" & SD_per_Condition > 0.1),
              REML = FALSE)
anova(Model3,Model4)
summary(Model3)



#Plot Predictions (Figure 02 in paper)
Figure_Predictions1 = ggplot(Predictions %>% filter(occlusion_time %in% c(0.5,0.6,0.7)),aes(as.factor(occlusion_time),time_perceived-occlusion_time,col = SelfmotionDirection)) +
  geom_boxplot(size = 1.5) +
  xlab("Occlusion Duration (s)") +
  ylab(expression("Duration"["Perceived"]*" - Duration"["Correct"]*" (m/s)")) +
  scale_color_manual(name = "Motion Profile", values = c("red","orange","blue")) +
  ggtitle("A.") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 2)
  

Figure_Predictions2 = ggplot(Prediction_SDs %>% 
                               filter(occlusion_time %in% c(0.5,0.6,0.7)) %>% 
                               mutate(velH_Factor = paste0(velH," m/s")),
       aes(Mean_per_Condition,SD_per_Condition,
           color = SelfmotionDirection, 
           shape = velH_Factor,
           linetype = SelfmotionDirection)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              size = 1.5,
              se = FALSE) +
  xlab(expression("Mean of Duration"["Perceived"]*" per Condition and Participant (m/s)")) +
  ylab(expression("SD of Duration"["Perceived"]*" per Condition and Participant (m/s)")) +
  scale_color_manual(name = "Motion Profile", values = c("red","orange","blue")) +
  scale_linetype_manual(name = "Motion Profile", values = c(1,2,4)) +
  scale_shape_manual(name = "Ball Speed", values = c("triangle","square","circle")) +
  ggtitle("B.")

plot_grid(Figure_Predictions1,Figure_Predictions2, rel_widths = c(0.8,1))
ggsave("Figures/(Figure 03) Predictions Hypotheses 1a and 1b.jpg", w = 12, h = 6)