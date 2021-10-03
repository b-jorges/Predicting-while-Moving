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

####Pilot data prediction
PredictionAmbika = read.table("Data/2_Prediction/Prediction_Ambika.txt", head = TRUE)
PredictionAmbika$ID = "Ambika"
PredictionAhmed = read.table("Data/2_Prediction/Prediction_Ahmed.txt", head = TRUE)
PredictionAhmed$ID = "Ahmed"
PredictionJohn = read.table("Data/2_Prediction/Prediction_John.txt", head = TRUE)
PredictionJohn$ID = "John"
PredictionLaurence = read.table("Data/2_Prediction/Prediction_Laurence.txt", head = TRUE)
PredictionLaurence$ID = "Laurence"
Prediction = rbind(PredictionAmbika,PredictionAhmed,PredictionJohn,PredictionLaurence)
Prediction$velH = abs(Prediction$velH)
# Prediction = read.csv("Data/2_Prediction/Data_Prediction.csv")

nPreOutliers = length(Prediction$trial)
Prediction = Prediction %>% mutate(Congruent = case_when(velH*velH_Subject > 0 ~ "Same Direction",
                                                         velH*velH_Subject < 0 ~ "Opposite Directions",
                                                         velH*velH_Subject == 0 ~ "No Motion")) %>% 
             filter(Response_Time < 3*Occlusion_Duration) %>% 
             mutate(velH_Abs = abs(velH))
nPostOutliers = length(Prediction$trial)
(nPreOutliers-nPostOutliers)/nPreOutliers

ggplot(Prediction,aes(Response_Time-Occlusion_Duration,color = Congruent)) +
  geom_density() +
  facet_wrap(ID~.)

#Full model Opposite Directions
priors_OppositeDirections <- c(prior(normal(0, 1), class = Intercept),
            prior(normal( -0.05, 0.7), class = "b", coef = "CongruentOppositeDirections"),
            prior(normal( 0.05, 0.4), class = "b", dpar = "sigma", coef = "CongruentOppositeDirections"),
            prior(lkj(4), class = cor))
PredictionBayesianModel_OppositeDirections = brm(bf(Response_Time-Occlusion_Duration ~ Congruent + (velH + Occlusion_Duration | ID),
                          sigma ~  Congruent + (velH + Occlusion_Duration | ID)),
            data = Prediction %>% filter(Congruent %in% c("Opposite Directions", "No Motion")),
            cores = 4,
            warmup = 2000,
            iter = 20000,
            prior = priors_OppositeDirections,
            save_all_pars = TRUE,
            control = list(adapt_delta = 0.95))
save(PredictionBayesianModel_OppositeDirections, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                            "/SavedVariables/PredictionBayesianModel_OppositeDirections.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PredictionBayesianModel_OppositeDirections.RData"))

#Full model Same Direction
priors_SameDirection <- c(prior(normal(0, 1), class = Intercept),
            prior(normal( 0, 0.7), class = "b", coef = "CongruentSameDirection"),
            prior(normal( 0, 0.4), class = "b", dpar = "sigma", coef = "CongruentSameDirection"),
            prior(lkj(4), class = cor))
PredictionBayesianModel_SameDirection = brm(bf(Response_Time-Occlusion_Duration ~ Congruent + (velH + Occlusion_Duration | ID),
                                                    sigma ~  Congruent + (velH + Occlusion_Duration | ID)),
                                                 data = Prediction %>% filter(Congruent %in% c("Same Direction", "No Motion")),
                                                 cores = 4,
                                                 warmup = 2000,
                                                 iter = 20000,
                                                 prior = priors_SameDirection,
                                            save_all_pars = TRUE,
                                            control = list(adapt_delta = 0.95))
save(PredictionBayesianModel_SameDirection, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                               "/SavedVariables/PredictionBayesianModel_SameDirection.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PredictionBayesianModel_SameDirection.RData"))

#Null models Opposite Directions
PredictionBayesianModel_OppositeDirections_Null_Accuracy = brm(bf(Response_Time-Occlusion_Duration ~ (velH + Occlusion_Duration | ID),
                                 sigma ~  Congruent + (velH + Occlusion_Duration | ID)),
                              data = Prediction %>% filter(Congruent %in% c("Opposite Directions", "No Motion")),
                              cores = 4,
                              warmup = 2000,
                              iter = 20000,
                              prior = priors_OppositeDirections[-2,],
                              save_all_pars = TRUE,
                              control = list(adapt_delta = 0.9))
save(PredictionBayesianModel_OppositeDirections_Null_Accuracy, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                                             "/SavedVariables/PredictionBayesianModel_OppositeDirections_Null_Accuracy.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PredictionBayesianModel_OppositeDirections_Null_Accuracy.RData"))

PredictionBayesianModel_OppositeDirections_Null_Precision = brm(bf(Response_Time-Occlusion_Duration ~ Congruent + (velH + Occlusion_Duration | ID),
                                               sigma ~ (velH + Occlusion_Duration | ID)),
                                               data = Prediction %>% filter(Congruent %in% c("Opposite Directions", "No Motion")),
                                            cores = 4,
                                            warmup = 2000,
                                            iter = 20000,
                                            prior = priors_OppositeDirections[-3,],
                                            save_all_pars = TRUE,
                                            control = list(adapt_delta = 0.95))
save(PredictionBayesianModel_OppositeDirections_Null_Precision, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                               "/SavedVariables/PredictionBayesianModel_OppositeDirections_Null_Precision.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PredictionBayesianModel_OppositeDirections_Null_Precision.RData"))

#Null models Same Direction
PredictionBayesianModel_SameDirection_Null_Accuracy = brm(bf(Response_Time-Occlusion_Duration ~ (velH + Occlusion_Duration | ID),
                                                                  sigma ~  Congruent + (velH + Occlusion_Duration | ID)),
                                                               data = Prediction %>% filter(Congruent %in% c("Same Direction", "No Motion")),
                                                               cores = 4,
                                                               warmup = 2000,
                                                               iter = 20000,
                                                               prior = priors_SameDirection[-2,],
                                                          save_all_pars = TRUE,
                                                          control = list(adapt_delta = 0.95))
save(PredictionBayesianModel_SameDirection_Null_Accuracy, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                                        "/SavedVariables/PredictionBayesianModel_SameDirection_Null_Accuracy.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PredictionBayesianModel_SameDirection_Null_Accuracy.RData"))

PredictionBayesianModel_SameDirection_Null_Precision = brm(bf(Response_Time-Occlusion_Duration ~ Congruent + (velH + Occlusion_Duration | ID),
                                                             sigma ~ (velH + Occlusion_Duration | ID)),
                                                          data = Prediction %>% filter(Congruent %in% c("Same Direction", "No Motion")),
                                                          cores = 4,
                                                          warmup = 2000,
                                                          iter = 20000,
                                                          prior = priors_SameDirection[-3,],
                                                          save_all_pars = TRUE,
                                                          control = list(adapt_delta = 0.95))
save(PredictionBayesianModel_SameDirection_Null_Precision, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                               "/SavedVariables/PredictionBayesianModel_SameDirection_Null_Precision.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PredictionBayesianModel_SameDirection_Null_Precision.RData"))

#Get Bayes factor for everything
#Opposite Directions
margLogLik_TestModel_OppositeDirections <- brms::bridge_sampler(PredictionBayesianModel_OppositeDirections, silent = TRUE)
margLogLik_NullModel_OppositeDirections_Accuracy <- brms::bridge_sampler(PredictionBayesianModel_OppositeDirections_Null_Accuracy, silent = TRUE)
margLogLik_NullModel_OppositeDirections_Precision <- brms::bridge_sampler(PredictionBayesianModel_OppositeDirections_Null_Precision, silent = TRUE)
save(margLogLik_TestModel_OppositeDirections, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                              "/SavedVariables/margLogLik_TestModel_OppositeDirections.RData"))
save(margLogLik_NullModel_OppositeDirections_Accuracy, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                              "/SavedVariables/margLogLik_NullModel_OppositeDirections_Accuracy.RData"))
save(margLogLik_NullModel_OppositeDirections_Precision, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                                     "/SavedVariables/margLogLik_NullModel_OppositeDirections_Precision.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_TestModel_OppositeDirections.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_NullModel_OppositeDirections_Accuracy.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_NullModel_OppositeDirections_Precision.RData"))

BF_OppositeDirections_Accuracy <- brms::bayes_factor(margLogLik_TestModel_OppositeDirections,margLogLik_NullModel_OppositeDirections_Accuracy)
BF_OppositeDirections_Precision <- brms::bayes_factor(margLogLik_TestModel_OppositeDirections,margLogLik_NullModel_OppositeDirections_Precision)
BF_OppositeDirections_Accuracy
BF_OppositeDirections_Precision


#Same Direction
margLogLik_TestModel_SameDirection <- brms::bridge_sampler(PredictionBayesianModel_SameDirection, silent = TRUE)
margLogLik_NullModel_SameDirection_Accuracy <- brms::bridge_sampler(PredictionBayesianModel_SameDirection_Null_Accuracy, silent = TRUE)
margLogLik_NullModel_SameDirection_Precision <- brms::bridge_sampler(PredictionBayesianModel_SameDirection_Null_Precision, silent = TRUE)
save(margLogLik_TestModel_SameDirection, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                            "/SavedVariables/margLogLik_TestModel_SameDirection.RData"))
save(margLogLik_NullModel_SameDirection_Accuracy, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                                     "/SavedVariables/margLogLik_NullModel_SameDirection_Accuracy.RData"))
save(margLogLik_NullModel_SameDirection_Precision, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                                                                      "/SavedVariables/margLogLik_NullModel_SameDirection_Precision.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_TestModel_SameDirection.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_NullModel_SameDirection_Accuracy.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_NullModel_SameDirection_Precision.RData"))

BF_SameDirection_Accuracy <- brms::bayes_factor(margLogLik_TestModel_SameDirection,margLogLik_NullModel_SameDirection_Accuracy)
BF_SameDirection_Precision <- brms::bayes_factor(margLogLik_TestModel_SameDirection,margLogLik_NullModel_SameDirection_Precision)
BF_SameDirection_Accuracy
BF_SameDirection_Precision

plot(PredictionBayesianModel_SameDirection_Null_Accuracy)
