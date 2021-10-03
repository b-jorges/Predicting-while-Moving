require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
require(lme4)
require(lmerTest)
require(brms)

set.seed(4)

#Parameters for the stimulus:
velH = c(4,5,6) #m/s
self_velH = c(-1.8,0,1.8) #meters over half a second, = 3.6m/s on average, but Gaussian motion profile
occlusion_times = c(0.5,0.6,0.7)

#Weber Fraction for distance estimation is about 5%
WFtoSD(0.05)

WFtoSD = function(WF){

  FitSD = function(WF,SD){
    ((pnorm(1-WF,1,SD) - 0.25)^2)^0.5}
  
  optimize(FitSD,WF = WF,interval = c(0,10))$minimum
  
}

###set prior for Bayes model
priors <- c(prior(normal(0, 2), class = Intercept),
            prior(normal( -0.05, 0.7), class = "b", coef = "SelfmotionDirectionOppositedirections"),
            prior(normal( 0, 0.7), class = "b", coef = "SelfmotionDirectionSamedirection"),
            prior(lkj(4), class = cor))

#Set parameters for power analysis. Since we do the Bayesian thing, lets only do the lowest number of everything
nTrials = c(5)
nParticipants = c(10)
nIterations = 50


#Monte Carlo Simulations
PowerfulDataframe = data.frame()

for (j in nParticipants){
  
  Participants = paste0("p",1:j)
  
  for (k in nTrials){
    
    reps = 1:k
    
    for (i in 1:nIterations){
      
      print(j)
      print(k)
      print(i)
      
      Predictions = expand.grid(velH = velH,
                                self_velH = self_velH,
                                occlusion_time = occlusion_times,
                                Participant = Participants,
                                rep = reps)
      Predictions$occlusion_time
      Predictions = Predictions %>% 
        mutate(SelfmotionDirection = case_when(
          velH*self_velH > 0 ~ "Same direction",
          velH*self_velH < 0 ~ "Opposite directions",
          velH*self_velH == 0 ~ "Observer Static"),
          Bias_selfmotion = case_when(
            SelfmotionDirection == "Same direction" ~ 0,
            SelfmotionDirection == "Opposite directions" ~ 0.2*abs(self_velH)*1.8,
            SelfmotionDirection == "Observer Static" ~ 0),
          LeftRight = case_when(
            velH > 0 ~ "Right",
            velH < 0 ~ "Left"),
          velH_Abs = abs(velH)) %>% 
        group_by(Participant) %>% 
        mutate(VariabilityPerceivedDistance_SD = rnorm(1,WFtoSD(0.05),0.02),
               VariabilityPerceivedDistance_Mean = 1,
               VariabilityPerceivedSpeed_SD = rnorm(1,WFtoSD(0.1)*1.2,0.02), ###1.2 = 20% higher Weber Fraction when moving
               VariabilityPerceivedSpeed_Mean = Bias_selfmotion) %>%
        mutate(CorrectDistance = velH_Abs*occlusion_time,
               PerceivedDistance = CorrectDistance*rnorm(length(velH),1,VariabilityPerceivedDistance_SD),
               CorrectTime = occlusion_time,
               PerceivedvelH = (velH_Abs + abs(self_velH)*VariabilityPerceivedSpeed_Mean)*rnorm(length(velH),1,VariabilityPerceivedSpeed_SD),
               time_perceived = PerceivedDistance/PerceivedvelH,
               TimingError = time_perceived-CorrectTime)
      
      
      PredictionBayesianModel = brm(bf(TimingError ~ SelfmotionDirection + (velH + occlusion_time | Participant),
                                       sigma ~  SelfmotionDirection + (velH + occlusion_time | Participant)),
                                    data = Predictions,
                                    cores = 4,
                                    prior = priors)
      
      PostProbMean = hypothesis(PredictionBayesianModel,"SelfmotionDirectionOppositedirections < 0")$hypothesis$Post.Prob
      PostProbSigma = hypothesis(PredictionBayesianModel,"sigma_SelfmotionDirectionOppositedirections < 0")$hypothesis$Post.Prob
      

      if (i == 1 & j == nParticipants[1] & k == nTrials[1]){
        
        PowerfulDataframe = data.frame(PostProbMean = PostProbMean,
                                       PostProbSigma = PostProbSigma,
                                       nTrials = k,
                                       nParticipants = j,
                                       Iteration = i)}
      
      else{
        PowerfulDataframe = rbind(PowerfulDataframe,data.frame(PostProbMean = PostProbMean,
                                                               PostProbSigma = PostProbSigma,
                                                               nTrials = k,
                                                               nParticipants = j,
                                                               Iteration = i)
                                 )
        
        save(PowerfulDataframe, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                              "/SavedVariables/PowerfulDataframe_Prediction.RData"))
        rm(PredictionBayesianModel)
      }
    }
  }
}
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                        "/SavedVariables/PowerfulDataframe_Prediction.RData"))

ggplot(PowerfulDataframe,aes(PostProbMean)) +
  geom_density() +
  labs(x = "# Participants",y = "Power") +
  scale_color_manual(values = colorRampPalette(c("orange","blue"))(6),
                     name = "# Trials")
ggsave("Figures/PowerPredictions.jpg", w = 5, h = 5)