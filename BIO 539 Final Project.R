library(tidyverse)
library(lme4)
library(AICcmodavg)

dat <- read.csv("2021_SHMW.csv") %>% 
  mutate(Color = as.factor(Color),
         Season = factor(Season, levels = c("Summer", "Fall", "Winter", "Spring")), 
         Depth = Depth * 1.8288, ## Convert fathoms to meters
         Color = case_when(
           Color == "White" ~ "White",
           Color != "White" ~ "Discolored"))%>%
  filter(!is.na(Meat_Weight)) ## Filter any cases where there is no mw data
stat <- read.csv("2021_Stations.csv")
dat <- left_join(dat, stat, by = "Station") %>% 
  mutate(Depth2 = as.numeric(scale(log(Depth))),
         shell = as.numeric(scale(log(Shell_Height))),
         Lat2 = as.numeric(scale(log(Lat)))) ## Log transforms and scales all continuous variables
## Shell height impact on meat weight
dat %>% 
  ggplot(aes(x = Shell_Height, y = Meat_Weight)) +
  geom_jitter() +
  theme_bw() +
  labs(y = "Meat Weight (g)", x = "Shell Height (mm)") +
  geom_smooth(method = lm, se = FALSE)

summary(lm(Meat_Weight ~ Shell_Height, data = dat))

## Seasonal impact of meat weight
ggplot(dat, aes(x = factor(Season, levels = c("Summer", "Fall", "Winter", "Spring")), y = Meat_Weight)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Meat Weight (g)", x = "Season") +
  facet_wrap(~shells)

kruskal.test(Meat_Weight ~ Season, data = dat)
## Depth impact on meat weight
dat %>% 
  ggplot(aes(x = Depth, y = Meat_Weight)) +
  geom_jitter() +
  theme_bw() +
  labs(y = "Meat Weight (g)", x = "Depth") +
  geom_smooth(method = lm, se = FALSE)

summary(lm(Meat_Weight~Depth, data = dat))
## Color impact on meat weight
dat %>% 
  ggplot(aes(x = Color, y = Meat_Weight)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Meat Weight (g)", x = "Color") +
  facet_wrap(~shells)

wilcox.test(Meat_Weight ~ Color, data = dat) ## Test the difference between Colors

## Create a list of GLMMs with a wide variety of different parameters for SHMW
fits <- list() 

fits[[1]] <- glmer((Meat_Weight ~ shell + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[2]] <- glmer((Meat_Weight ~ Depth2 + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[3]] <- glmer((Meat_Weight ~ Season + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[4]] <- glmer((Meat_Weight ~ Color + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[5]] <- glmer((Meat_Weight ~ Lat2 + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[6]] <- glmer((Meat_Weight ~ shell + Season + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[7]] <- glmer((Meat_Weight ~ shell + Depth2 + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[8]] <- glmer((Meat_Weight ~ shell + Color + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[9]] <- glmer((Meat_Weight ~ shell + Lat2 + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[10]] <- glmer((Meat_Weight ~ shell + Season + Depth2 + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[11]] <- glmer((Meat_Weight ~ shell + Season + Color + (1|Station)),
                    family = poisson(link = "log"), data = dat)
fits[[12]] <- glmer((Meat_Weight ~ shell + Season + Lat2 + (1|Station)),
                   family = poisson(link = "log"), data = dat)
fits[[13]] <- glmer((Meat_Weight ~ shell + Season + Depth2 + Color 
                     + (1|Station)),
                    family = poisson(link = "log"), data = dat)
fits[[14]] <- glmer((Meat_Weight ~ shell + Season + Depth2 + Lat2 
                     + (1|Station)),
                    family = poisson(link = "log"), data = dat)
fits[[15]] <- glmer((Meat_Weight ~ shell + Season + Lat2 + Color 
                     + (1|Station)),
                    family = poisson(link = "log"), data = dat)
fits[[16]] <- glmer((Meat_Weight ~ shell + Season + Depth2 + Color 
                     + Lat2 + (1|Station)),
                    family = poisson(link = "log"), data = dat)

Model_names <- c("Shell Height", "Depth", "Season", "Color", "Lat",
                 "Shell Height + Season", "Shell Height + Depth", 
                 "Shell Height + Color", "Shell Height + Lat",
                 "Shell Height + Season + Depth",
                 "Shell Height + Season + Color",
                 "Shell Height + Season + Lat",
                 "Shell Height + Season + Depth + Color",
                 "Shell Height + Season + Depth + Lat",
                 "Shell Height + Season + Lat + Color",
                 "Shell Height + Season + Depth + Color + Lat")
aictab(cand.set = fits, modnames = Model_names, sort = TRUE) ## Model selection table

## Creation of new data to get model output of Meat Weight
newdat <- crossing(shell1 = seq(40, 160, 10),
                   Season = as.factor(c("Summer", "Fall", "Winter", "Spring")),
                   Station = c(704, 734, 730),
                   Color = as.factor(c("White", "Discolored"))) %>% 
  left_join(stat, by = "Station") %>% 
  mutate(Depth = case_when(
    Station == 704 ~ 90,
    Station == 734 ~ 90, 
    Station == 730 ~ 60), ## Rescales the new data to the original data
    Lat2 = as.numeric(scale(log(Lat), center = mean(log(dat$Lat)),
                            scale = sd(log(dat$Lat)))),
    shell = as.numeric(scale(log(shell1), center = mean(log(dat$Shell_Height)),
                             scale = sd(log(dat$Shell_Height)))),
    Depth2 = as.numeric(scale(log(Depth), center = mean(log(dat$Depth)),
                              scale = sd(log(dat$Depth)))))
newdat$MW <- predict(fits[[16]], newdata = newdat) ## Model predictions of MW
newdat$MW <- exp(newdat$MW)
## Shell Height Meat Weight Curves
newdat %>% 
  ggplot() +
  geom_line(aes(x = shell1, y = MW, group = Season, color = Season)) +
  facet_grid(cols = vars(Station), rows = vars(Color)) +
  labs(x = "Shell Height (mm)", y = "Predicted Meat Weight (g)") +
  theme_bw()

