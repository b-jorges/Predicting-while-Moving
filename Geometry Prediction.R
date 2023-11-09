require(ggplot2)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"

#this file describes and visualizes the geometry of the prediction task
velH = c(-8,-6.6, 6.6, 8) #m/s
self_velH = c(-2,0,2) #meters over half a second, = 4m/s on average, but Gaussian motion profile
mean_Gaussian_self = 1
sd_Gaussian_self = 0.12
depth = 8
occlusion_durations = c(0.5,0.6,0.7)
selfmotion_duration = 0.5
TimePercentage = seq(0,1,0.01)

Errything = expand.grid(TimePercentage=TimePercentage,
                        velH=velH,
                        self_velH=self_velH,
                        mean_Gaussian_self=mean_Gaussian_self,
                        sd_Gaussian_self=sd_Gaussian_self,
                        depth=depth,
                        occlusion_duration=occlusion_durations,
                        selfmotion_duration=selfmotion_duration)

Errything = Errything %>% 
  mutate(Time_Total = TimePercentage*(occlusion_duration+selfmotion_duration),
         Time_Percentage_visible = case_when(
           Time_Total <= 0.5 ~ Time_Total*2,
           Time_Total > 0.5 ~ 0
         ),
         Displacement_Target = Time_Total*velH,
         Displacement_Observer = case_when(
           self_velH > 0 & Time_Total <= 0.5 ~ pnorm(Time_Percentage_visible,0.5,sd_Gaussian_self)*self_velH,
           self_velH > 0 & Time_Total > 0.5 ~ self_velH,
           self_velH == 0 ~ 0,
           self_velH < 0 & Time_Total <= 0.5 ~ pnorm(Time_Percentage_visible,0.5,sd_Gaussian_self)*self_velH,
           self_velH < 0 & Time_Total > 0.5 ~ self_velH),
         PartOfTrajectory = case_when(
           Time_Total <= 0.5 ~ "visible",
           Time_Total > 0.5 ~ "invisible"
         ),
         InitialPosition_Target  = (self_velH*selfmotion_duration*2 - velH*selfmotion_duration) / 2, 
         #such that the visible phase is centered in front of the observer
         #see /Figures/geometry starting positions.png
         
         Position_Goal = InitialPosition_Target + (occlusion_duration+selfmotion_duration)*velH,
         Position_Target = InitialPosition_Target + Displacement_Target,
         InitialPosition_Observer = 0,
         Position_Observer = InitialPosition_Observer + Displacement_Observer,
         z_position_Target = 8,
         z_position_Observer = 0,
         #tangent function: tan(alpha) = opposite/adjacent; 
         #alpha = tan-1(opposite/adjacent); 
         #opposite = x difference between observer and target; 
         #adjacent = z difference between observer and target
         Angle_Target_Observer = RadiansToDegree(atan((Position_Target-Position_Observer)/depth)),
         Delta_t = Time_Total-lag(Time_Total),
         Delta_angle = Angle_Target_Observer-lag(Angle_Target_Observer),
         Angular_Speed = Delta_angle/Delta_t) %>% 
  group_by(velH,self_velH,occlusion_duration, PartOfTrajectory) %>% 
  mutate(Mean_Angular_Speed_PerPart = mean(Angular_Speed, na.rm = TRUE))


#make 
LegendDF = data.frame(Label = c("1Visible",
                     "2Invisible",
                     "3Observer",
                     "4Target"))
LegendPlot = ggplot(LegendDF,aes(Label,Label,color = Label)) +
  geom_point() +
  scale_color_manual(values = c("blue",
                                "lightblue",
                                "red",
                                "black"),
                     labels = c("Visible",
                                "Invisible",
                                "Goal",
                                "Observer"))

Legend = get_legend(LegendPlot)


####Plot of all conditions, in space
Plot_Conditions = ggplot(Errything %>% filter(velH %in% c(-8,8) & occlusion_duration == 0.7),aes(z_position_Target,Position_Target,color = PartOfTrajectory)) +
  geom_point(size = 3) +
  coord_flip() +
  geom_point(aes(z_position_Observer, Position_Observer), 
             col = "black", 
             size = 3) +
  geom_point(aes(z_position_Target, Position_Goal), 
             col = "red", 
             size = 5) +
  facet_grid(self_velH~velH+occlusion_duration) +
  xlab("z (m)") +
  ylab("x (m)") +
  scale_color_manual(values = c("lightblue",
                                "blue",
                                "black",
                                "red")) +
  theme(legend.position = "none")
plot_grid(Plot_Conditions,Legend, rel_widths = c(1,0.2))
ggsave("Figures/AllTrajectories.jpg", w = 16, h = 8)

####GIF of select conditions
Animate = ggplot(Errything %>% filter(velH == 8 & occlusion_duration == 0.7),
                 aes(z_position_Target,Position_Target,color = PartOfTrajectory)) +
  geom_point(size = 10) +
  coord_flip() +
  geom_point(aes(z_position_Observer, Position_Observer), 
             col = "black", 
             size = 10,
             data = Errything %>% filter(velH == 8 & occlusion_duration == 0.7)) +
  geom_point(aes(z_position_Target, Position_Goal), 
             col = "red", 
             size = 15,
             data = Errything %>% filter(velH == 8 & occlusion_duration == 0.7)) +
  facet_grid(self_velH~.) +
  xlab("z (m)") +
  ylab("x (m)") +
  scale_color_manual(values = c("lightblue","blue")) +
  transition_time(Time_Total)  +
  ease_aes('linear') +
  labs(title = 'Time in trial: {round(frame_time,2)}', x = 'z (m)', y = 'x (m)') +
  theme(legend.position = "none")
anim_save("Figures/ExampleGIF.gif",Animate)

####angular velocity
ggplot(Errything %>% filter(occlusion_duration == 0.5 & velH %in% c(-8,8)),aes(Time_Total,Angular_Speed,color = PartOfTrajectory)) +
  geom_point(size = 3) +
  facet_grid(velH~self_velH) +
  labs(x = "Time in trial (s)",
       y = "Angular Speed (?/s)") +
  scale_color_manual(name = "",
                     values = c("lightblue","blue"))
ggsave("Figures/Angular Speed.jpg", w = 6, h = 6)

ggplot(Errything %>% filter(occlusion_duration == 0.5 & velH %in% c(-8,8)),aes(Time_Total,Angle_Target_Observer,color = PartOfTrajectory)) +
  geom_point(size = 3) +
  facet_grid(velH~self_velH) +
  labs(x = "Time in trial (s)",
       y = "Angular Speed (?/s)") +
  scale_color_manual(name = "",
                     values = c("lightblue","blue"))
ggsave("Figures/Angle.jpg", w = 6, h = 6)

lol = Errything %>% filter(occlusion_duration == 0.5 & velH %in% c(-8,8))

ggplot(Errything,aes(Time_Total,Displacement_Observer)) +
  geom_point()
