########Code cemetery
#####

#####evaluate stuff: bimodal distribution???
ggplot(response[response$Condition == "Different g",],aes(as.factor(g),TemporalError)) +
  facet_wrap(.~LongOcclusion) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1)



#x versus y
ggplot(KK[KK$id == "s10" & 
            KK$WithinTimeFrame == 1 & 
            KK$block == 2 & 
            KK$trial > 280 & 
            KK$trial < 300,]) +
  geom_point(aes(x_eyes_adjusted.filtered,y_eyes_adjusted.filtered),size = 0.01) +
  geom_line(aes(x_actual,y_actual),color = Red) +
  facet_wrap(~trial) +
  geom_vline(aes(xintercept = x_eyes_adjusted.filtered-0.01),KK[KK$id == "s10" & 
                                                                  KK$WithinTimeFrame == 1 & 
                                                                  KK$block == 2 & 
                                                                  KK$trial > 280 & 
                                                                  KK$trial < 300 &
                                                                  KK$saccade == 1,],
             alpha = 0.2,
             color = BlauUB) +
  xlab("X position (m)") +
  ylab("Y position (m)")
ggsave("x by y.jpg", w = 10, h = 6)


#t versus x
#saccades are blue, yellow signifies point of disappearance
ggplot(KK[KK$id == "s10" & 
            KK$WithinTimeFrame == 1 & 
            KK$block == 4 & 
            KK$trial > 280 & 
            KK$trial < 300,]) +
  geom_point(aes(TimeInTrialActual,x_eyes_adjusted.filtered),size = 0.01) +
  geom_line(aes(TimeInTrialActual,x_actual),color = Red) +
  geom_vline(aes(xintercept = TimeInTrialActual-0.01),KK[KK$id == "s10" & 
                                                           KK$WithinTimeFrame == 1 & 
                                                           KK$block == 4 & 
                                                           KK$trial > 280 & 
                                                           KK$trial < 300 &
                                                           KK$saccade == 1,],
             alpha = 0.2,
             color = BlauUB) +
  geom_vline(aes(xintercept = TimeInTrialActual),KK[KK$id == "s10" & 
                                                      KK$block == 4 & 
                                                      KK$trial > 280 & 
                                                      KK$trial < 300 &
                                                      KK$TimeInTrialActual == KK$PointOfDisappearance,], 
             alpha = 1,
             color = Yellow,
             size = 1) +
  facet_wrap(~trial) +
  xlab("Time (s)") +
  ylab("X Position (m)")

ggsave("x by t.jpg", w = 10, h = 6)


#t versus vr
#saccades are blue, yellow signifies point of disappearance
ggplot(KK[KK$id == "s10" & 
            KK$WithinTimeFrame == 1 & 
            KK$block == 2 & 
            KK$trial > 280 & 
            KK$trial < 300,]) +
  geom_line(aes(TimeInTrialActual,vr_eyes)) +
  geom_line(aes(TimeInTrialActual,vr_target),color = Red) +
  facet_wrap(~trial) +
  geom_vline(aes(xintercept = TimeInTrialActual-0.01),KK[KK$id == "s10" & 
                                                           KK$WithinTimeFrame == 1 & 
                                                           KK$block == 2 & 
                                                           KK$trial > 280 & 
                                                           KK$trial < 300 &
                                                           KK$saccade == 1,], 
             alpha = 0.2,
             color = BlauUB) +
  geom_vline(aes(xintercept = TimeInTrialActual),KK[KK$id == "s10" & 
                                                      KK$block == 2 & 
                                                      KK$trial > 280 & 
                                                      KK$trial < 300 &
                                                      KK$TimeInTrialActual == KK$PointOfDisappearance,], 
             alpha = 1,
             color = Yellow,
             size = 1) +
  coord_cartesian(ylim = c(-1,40))  +
  xlab("Time (s)") +
  ylab("Tangential Speed (m/s)")
ggsave("Tangential Speed by time.jpg", w = 10, h = 6)


#t versus ar
#saccades are blue
ggplot(KK[KK$id == "s10" & 
            KK$WithinTimeFrame == 1 & 
            KK$block == 2 & 
            KK$trial > 280 & 
            KK$trial < 300,]) +
  geom_line(aes(TimeInTrialActual,ar_eyes)) +
  geom_line(aes(TimeInTrialActual,ar_target),color = Red) +
  geom_vline(aes(xintercept = TimeInTrialActual-0.01),KK[KK$id == "s10" & 
                                                           KK$WithinTimeFrame == 1 & 
                                                           KK$block == 2 & 
                                                           KK$trial > 280 & 
                                                           KK$trial < 300 &
                                                           KK$saccade == 1,], 
             alpha = 0.2,
             color = BlauUB) +
  facet_wrap(~trial) +
  coord_cartesian(ylim = c(-350,350)) +
  xlab("Time (s)") +
  ylab("Acceleration (m/s²)")

ggsave("Aceleration by time.jpg", w = 10, h = 6)

####Gain 1g/1g overall
Gains <- Gains %>%
  group_by(g_2,Condition) %>%
  mutate(n_block_g_overall = length(Gain_Overall),
         Gain_mean_block_g_overall = mean(Gain_Overall),
         Gain_sd_block_g_overall = sd(Gain_Overall))
Gains$Gain_SE_block_g_overall <- Gains$Gain_sd_block_g_overall/sqrt(Gains$n_block_g_overall)

ggplot(Gains[Gains$Condition == "-1g" & Gains$g != 11.2815 & Gains$g != 6.867,], aes(x=g_2, y=Gain_mean_block_g_overall, fill=g_2)) + 
  geom_bar(stat = "identity",position = "dodge") +
  geom_errorbar(aes(ymin=Gain_mean_block_g_overall-Gain_SE_block_g_overall, ymax=Gain_mean_block_g_overall+Gain_SE_block_g_overall),
                size=.3,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  scale_x_discrete("",labels=c("-1g","1g")) +
  scale_y_continuous("Gain",breaks=c(0.5,0.75,1)) +
  coord_cartesian(ylim = c(0.3,0.8)) +
  scale_fill_manual(name = "", values = c(a[1],a[6])) +
  theme(legend.position = "")
ggsave("Gain_-1g.jpg",w=4,h=4)





ggplot(Spatial_Gaze_Error[abs(Spatial_Gaze_Error$X_Eyes_Error_After_Saccade) < 4 & 
                            Spatial_Gaze_Error$Condition == "-1g",],
       aes(as.factor(g),X_Eyes_Error_After_Saccade, fill = as.factor(g))) + 
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1) +
  geom_hline(aes(yintercept = X_Eyes_Error_After_Saccade_Median,color="Median Spatial Error 1g"),Spatial_Gaze_Error[Spatial_Gaze_Error$g == 9.81 & Spatial_Gaze_Error$X_Error_After_Last_Saccade < 4 & Spatial_Gaze_Error$Condition == "-1g",], 
             alpha = 1,
             color = "black",
             size = 1) +
  scale_x_discrete("Gravity",labels=c("-1g","1g")) +
  scale_y_continuous("Horizontal Error (m)",breaks=c(-4,-2,0)) +
  coord_flip(ylim = c(-4,1)) +
  theme(legend.position = "") +
  scale_fill_manual(values = c(a[2],a[2],a[4]))


ggplot(Spatial_Gaze_Error[abs(Spatial_Gaze_Error$X_Eyes_Error_After_Saccade) < 4 & 
                            Spatial_Gaze_Error$Condition == "Different g",], 
       aes(as.factor(g),X_Eyes_Error_Lastsaccade, fill = as.factor(g))) + 
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1) +
  geom_hline(aes(yintercept = X_Eyes_Error_After_Saccade_G,color="Median Spatial Error 1g"),Spatial_Gaze_Error[Spatial_Gaze_Error$g == 9.81 & Spatial_Gaze_Error$X_Eyes_Error_After_Saccade < 4,], 
             alpha = 1,
             color = "black",
             size = 1) +
  scale_x_discrete("Gravity",labels=c("0.7g","0.85g","1g","1.15g","1.3g")) +
  scale_y_continuous("Horizontal Error (m)",breaks=c(-4,-2,0)) +
  coord_flip(ylim = c(-4,1)) +
  theme(legend.position = "") +
  scale_fill_manual(values = colorRampPalette(c(BlauUB,Yellow))(6))
ggsave("Spatial_Error_After_Saccade.jpg",w=6,h=5)





#Curvature matched and Time matched





Gains$MeanRadiusOfCurvature_b <- AddBinnedValues(Gains$RadiusOfCurvature)
Gains <- Gains %>%
  group_by(id,MeanRadiusOfCurvature_b,Condition) %>%
  mutate(n_Curv = length(Gain_Overall),
         Gain_Overall_Mean_Curv = mean(Gain_Overall),
         Gain_Overall_SD_Curv = sd(Gain_Overall))
Gains$Gain_SE_Curv <- Gains$Gain_Overall_SD_Curv/sqrt(Gains$n_Curv)



ggplot(Gains[Gains$SameCurvature==1,], aes(x=id, y=Gain_Overall_Mean_G_Curv, fill=as.factor(g))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Gain_Overall_Mean_G_Curv-Gain_SE_G_Curv, ymax=Gain_Overall_Mean_G_Curv+Gain_SE_G_Curv),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete("",labels=c("S1","S2","S3","S4","S5")) +
  scale_y_continuous("Gain",breaks=c(0.5,0.75,1)) +
  coord_cartesian(ylim = c(0.5,1)) +
  scale_fill_manual(name = "",
                    values = colorRampPalette(c(BlauUB,Yellow))(3),
                    labels = c("0.7g","1g","1.3g")) +
  ggtitle("Matched for Curvature")
ggsave("Gains_Curv_g.jpg",w=12/1.5,h=7/1.5)



###(3.2) Eye-tracking plots Gains
###
#Gains for Earth Gravity versus no Earth Gravity, only for 9.81m/s² versus -9.81m/s²
ggplot(Gains[Gains$Condition == "-1g",],aes(as.factor(g),Gain_Overall)) +
  facet_wrap(~id) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1)

#Gains for different gravity levels, including 0.7g etc.
ggplot(Gains[Gains$Condition == "Different g",],aes(as.factor(g),Gain_Overall)) +
  geom_violin() +
  #  stat_summary(fun.y=mean, geom="point", shape=23, size=2,position=position_dodge(0.9)) +
  geom_boxplot(width=0.1, position=position_dodge(0.9))
#  scale_fill_manual(values = colorRampPalette(c(BlauUB,Yellow))(5))


ggplot(Gains[Gains$Gain_y < 1 & Gains$Gain_y > 0 & Gains$Condition == "-1g" & Gains$g != 11.2815 & Gains$g != 6.8670 ,],aes(as.factor(id),Gain_y,fill=as.factor(g))) +
  geom_bar() +
  #  stat_summary(fun.y=mean, geom="point", shape=23, size=2,position=position_dodge(0.9)) +
  #  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  scale_fill_manual(values = colorRampPalette(c(BlauUB,Yellow))(2))


#Gains for different gravity levels and initial speeds
ggplot(Gains[Gains$Condition == "Different g" & Gains$id == "Bjorn",],aes(id,Gain_Overall,fill=as.factor(g)))+
  geom_bar(stat="identity",position='dodge') +
  facet_grid(vy~vx) +
  scale_fill_manual(values = colorRampPalette(c(Turquoise,Red))(5))

ggplot(Gains[Gains$Condition == "-1g" & Gains$g != 11.2815 & Gains$g != 6.867,],aes(id,Gain_Overall,fill=g_2))+
  geom_bar(stat="identity",position='dodge') +
  scale_fill_manual(values = colorRampPalette(c(Turquoise,Red))(4))
###


###(3.3) Coincidence timing: plots
###













###Spatial Error in time
KK$KineticProfile <- KK$vx+KK$vy + KK$g
ggplot(KK[abs(KK$TimeInTrialActual) < 3,],aes(x=TimeInTrialActual, y=SpatialError_xy)) +
  geom_point() +
  facet_wrap(KineticProfile~.)


###
###End code cemetery
#####End of Code cemetery