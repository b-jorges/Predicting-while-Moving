SimulatePsychometricFunction_Staircase = function(ID, 
                                                  ConditionOfInterest, 
                                                  StandardValues, 
                                                  reps, 
                                                  PSE_Difference, 
                                                  JND_Difference, 
                                                  Multiplicator_PSE_Standard, 
                                                  Multiplicator_SD_Standard, 
                                                  SD_ResponseFunction, 
                                                  Mean_Variability_Between = 0.1, 
                                                  SD_Variability_Between = 0.1){
  Psychometric = expand.grid(ID=ID, ConditionOfInterest=ConditionOfInterest, StandardValues=StandardValues, reps = reps)
  
  Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,Mean_Variability_Between),
           SD_Factor_ID = rnorm(1,1,SD_Variability_Between))
  
  Psychometric = Psychometric %>%
    mutate(
      Mean_Standard = StandardValues+StandardValues*Multiplicator_PSE_Standard,
      SD_Standard = StandardValues*Multiplicator_SD_Standard,
      Mean = (Mean_Standard + (ConditionOfInterest==ConditionOfInterest[2])*StandardValues*PSE_Difference)*PSE_Factor_ID,
      SD = abs((SD_Standard + (ConditionOfInterest==ConditionOfInterest[2])*SD_Standard*JND_Difference)*SD_Factor_ID),
      staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction), 
      Presented_TestStimulusStrength = Mean*staircase_factor,
      Difference = Presented_TestStimulusStrength - StandardValues,
      AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD),
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )
  
  Psychometric = Psychometric %>%
    filter(abs(staircase_factor-1) < 0.75) %>%
    group_by(ID,ConditionOfInterest,StandardValues,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(ConditionOfInterest))
  
  Psychometric
}


Analyze_Pychometric_GLMM = function(Psychometric){
  
  TimeBeginning = Sys.time()
  
  GLMM_Precision = glmer(cbind(Yes, Total - Yes) ~ ConditionOfInterest*Difference + 
                                    (Difference + ConditionOfInterest | ID) + 
                                    (Difference + ConditionOfInterest | StandardValues), 
                         family = binomial(link = "probit"),
                         data = Psychometric,
                         nAGQ = 0,
                         control = glmerControl(optimizer = "nloptwrap"))
  
  p = summary(GLMM_Precision)$coefficients[c(15,16)]
  
  
  #print(p)
  
  p
}

GetParametersOfPsychometricFunction = function(Psychometric){
  
  Parameters = quickpsy(Psychometric,Difference,Answer,grouping = .(ID,ConditionOfInterest,StandardValues), bootstrap = "none")$par
  
  Parameters2 = data.frame(ID = Parameters$ID[Parameters$parn == "p1"],
                           ConditionOfInterest = Parameters$ConditionOfInterest[Parameters$parn == "p1"],
                           Mean = Parameters$par[Parameters$parn == "p1"],
                           StandardValues = Parameters$StandardValues[Parameters$parn == "p1"])
  
  Parameters2$SD = Parameters$par[Parameters$parn == "p2"]
  Parameters2
}


Analyze_Pychometric_Accuracy_2Level = function(Parameters){
  
  ANOVA_Mean = aov(Mean ~ as.factor(ConditionOfInterest)*StandardValues,Parameters)
  Coefficients = summary(ANOVA_Mean)[[1]]
  Coefficients$`Pr(>F)`[1]
}


Analyze_Pychometric_Precision_2Level = function(Parameters){
  
  ANOVA_SD = aov(SD ~ as.factor(ConditionOfInterest)*StandardValues,Parameters)
  Coefficients = summary(ANOVA_SD)[[1]]
  Coefficients$`Pr(>F)`[1]
}

ComparePowers = function(ConditionOfInterest, StandardValues, reps, PSE_Difference, JND_Difference, 
                         Multiplicator_PSE_Standard, Multiplicator_SD_Standard, SD_ResponseFunction, Mean_Variability_Between = 0.1, SD_Variability_Between = 0.1,
                         NumbersOfSubjects){
  for (i in NumbersOfSubjects){
    
    ID = paste0("s",1:i)
    TimeBeginning = Sys.time()
    Dataframe_Temp = c()
    
    for (j in 1:nIterations){
      
      Dataframe = SimulatePsychometricFunction_Staircase(ID, ConditionOfInterest, StandardValues, reps, PSE_Difference, JND_Difference, 
                                                         Multiplicator_PSE_Standard, Multiplicator_SD_Standard, SD_ResponseFunction, Mean_Variability_Between = 0.1, SD_Variability_Between = 0.1)
      
      Parameters = GetParametersOfPsychometricFunction(Dataframe)
      
      p = c(Analyze_Pychometric_Accuracy_GLMM(Dataframe),
            Analyze_Pychometric_Precision_GLMM(Dataframe),
            Analyze_Pychometric_Accuracy_2Level(Parameters),    
            Analyze_Pychometric_Precision_2Level(Parameters))
      
      
      Dataframe_Temp = rbind(Dataframe_Temp,p)
      
      if ((j/25) %in% 1:40){
        (print(j))
      }
    }
    
    Power = rbind(Power,
                  data.frame(value = c(mean(Dataframe_Temp[,1] < pvalue),
                                       mean(Dataframe_Temp[,2] < pvalue),
                                       mean(Dataframe_Temp[,3] < pvalue),
                                       mean(Dataframe_Temp[,4] < pvalue)),
                             label = c("Accuracy GLMM",
                                       "Precision GLMM",
                                       "Accuracy Two-Level",
                                       "Precision Two-Level"),
                             reps = reps[length(reps)],
                             PSE_Difference = PSE_Difference,
                             JND_Difference = JND_Difference,
                             StandardValues = paste0(StandardValues[1],StandardValues[length(StandardValues)]),
                             nStandardValues = length(StandardValues),
                             TrialsPerSubject = length(StandardValues)*length(reps)*length(ConditionOfInterest),
                             SD_ResponseFunction = SD_ResponseFunction,
                             Mean_Variability_Between = Mean_Variability_Between,
                             SD_Variability_Between = SD_Variability_Between,
                             nSubjects = i))
    
    print(paste0("This iteration has taken ", Sys.time() - TimeBeginning))  ###This is two show how long each iteration takes
    print(paste0("Accuracy GLMM for ", i, " subjects: ", mean(Dataframe_Temp[,1] < pvalue))) #outputs an estimate of the power for each n
    print(paste0("Precision GLMM for ", i, " subjects: ", mean(Dataframe_Temp[,2] < pvalue))) #outputs an estimate of the power for each n
    print(paste0("Accuracy 2Level for ", i, " subjects: ", mean(Dataframe_Temp[,3] < pvalue))) #outputs an estimate of the power for each n
    print(paste0("Precision 2Level for ", i, " subjects: ", mean(Dataframe_Temp[,4] < pvalue))) #outputs an estimate of the power for each n
  }
  
  Power
}




