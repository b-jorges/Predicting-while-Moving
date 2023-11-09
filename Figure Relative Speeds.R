require(ggplot2)
require(dplyr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory

DF = expand.grid(t = seq(0,0.5,0.001),
                 MotionProfile = c("Same Direction","Opposite Directions","Observer Static"),
                 velH_Target = c(4,5,6)) %>% 
  mutate(velH_Self_t = case_when(
                  t <= 0.05 & MotionProfile == "Same Direction" ~ dnorm(t,0.05,0.02)/5,
                  t >= 0.45 & MotionProfile == "Same Direction"  ~ dnorm(t,0.45,0.02)/5,
                  t > 0.05 & t < 0.45  & MotionProfile == "Same Direction" ~ 4,
                  t <= 0.05 & MotionProfile == "Opposite Directions"  ~ -dnorm(t,0.05,0.02)/5,
                  t >= 0.45 & MotionProfile == "Opposite Directions" ~ -dnorm(t,0.45,0.02)/5,
                  t > 0.05 & t < 0.45 & MotionProfile == "Opposite Directions" ~ -4,
                  MotionProfile == "Observer Static" ~ 0),
         RelativeSpeed = velH_Target - velH_Self_t)

DF2 = data.frame(t = rep(DF$t,3),
                 MotionProfile = rep(DF$MotionProfile,3),
                 Speed = c(DF$velH_Target,DF$velH_Self_t,DF$RelativeSpeed),
                 Label = c(rep("Target Speed", length(DF$velH_Target)),
                           rep("Self-Motion Speed", length(DF$velH_Self_t)),
                           rep("Relative Speed\n(Target-Self)", length(DF$RelativeSpeed))))

ggplot(DF2,aes(t, Speed, color = Label)) +
  geom_point() +
  facet_wrap(MotionProfile ~ .) +
  xlab("Time (s)") +
  ylab("Simulated Speed (m/s)") +
  scale_color_manual("",
                     values = c("orange","grey","red"))
ggsave("Figures/RAW Figure 2e.jpg", w = 10, h = 4)
