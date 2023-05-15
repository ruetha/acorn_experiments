getwd()

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(performance)


seedexp_dataset<-read.csv("Seed_experiment_master_R.csv", header = T, sep=";", dec=".")
seedexp_dataset$date=as.factor(seedexp_dataset$date)
seedexp_dataset$year=as.factor(seedexp_dataset$year)
seedexp_dataset$session=as.factor(seedexp_dataset$session)
seedexp_dataset$plot=as.factor(seedexp_dataset$plot)
seedexp_dataset$transect=as.factor(seedexp_dataset$transect)
seedexp_dataset$block=as.factor(seedexp_dataset$block)
seedexp_dataset$cage=as.factor(seedexp_dataset$cage)
seedexp_dataset$species=as.factor(seedexp_dataset$species)
seedexp_dataset$invasiveness=as.factor(seedexp_dataset$invasiveness)
seedexp_dataset$density=as.numeric(seedexp_dataset$density)
seedexp_dataset$treatment=as.factor(seedexp_dataset$treatment)
seedexp_dataset$actual_treatment=as.factor(seedexp_dataset$actual_treatment)
seedexp_dataset$failed_exc=as.factor(seedexp_dataset$failed_exc)
seedexp_dataset$survival_detailed=as.factor(seedexp_dataset$survival_detailed)
seedexp_dataset$survival_detailed = factor(seedexp_dataset$survival_detailed, 
                                  levels = c('missing', 'damaged', 'not_ger', 'ger_root', 'ger_shoot'))
seedexp_dataset$survival=as.factor(seedexp_dataset$survival)
seedexp_dataset$survival = factor(seedexp_dataset$survival, 
                                  levels = c('missing', 'damaged', 'not_ger', 'germinated'))
seedexp_dataset$viability=as.factor(seedexp_dataset$viability)
seedexp_dataset$viability = factor(seedexp_dataset$viability, 
                                  levels = c('missing', 'not_viable', 'viable'))
seedexp_dataset$damage=as.factor(seedexp_dataset$damage)
seedexp_dataset$weevil=as.factor(seedexp_dataset$weevil)
seedexp_dataset$root_lenght=as.numeric(seedexp_dataset$root_lenght)
seedexp_dataset$shoot_lenght=as.numeric(seedexp_dataset$shoot_lenght)
summary(seedexp_dataset)
colnames(seedexp_dataset)
View(seedexp_dataset)
#str(seedexp_dataset)
#head(seedexp_dataset)
#dim(seedexp_dataset)

# dataset with ACORN FATE one hot encoded
acorn_fate <- seedexp_dataset %>%
  group_by(number, 
           plot, transect, block, cage,
           species, invasiveness, density, treatment, actual_treatment, failed_exc,
           survival, viability, damage, weevil) %>%
  filter(failed_exc != "failed") %>%
  summarise(damaged = sum(survival == "damaged"),
            germinated = sum(survival == "germinated"),
            missing = sum(survival == "missing"), 
            not_germinated = sum(survival == "not_ger"),
            viable = sum(viability == "viable"),
            not_viable = sum(viability == "not_viable"),
            missing = sum(viability == "missing"),
            animal = sum(damage == "animal"), 
            insect = sum(damage == "insect"),
            mold_disseccation = sum(damage == "mold"),
            inside_alive = sum(weevil == "inside_alive"),
            inside_dead = sum(weevil == "inside_dead"),
            pupa = sum(weevil == "pupa"),
            exited = sum(weevil == "escaped"))
View(acorn_fate)

# dataset with SURVIVAL counting per species and treatment
survival_count <- seedexp_dataset %>%
  filter(failed_exc == "ok") %>% 
  group_by(species, treatment) %>%
  summarise(damaged = sum(survival == "damaged"),
            germinated = sum(survival == "germinated"),
            missing = sum(survival == "missing"), 
            not_germinated = sum(survival == "not_ger"),
            viable = sum(viability == "viable"),
            not_viable = sum(viability == "not_viable"),
            missing = sum(viability == "missing"),
            animal = sum(damage == "animal"), 
            insect = sum(damage == "insect"),
            mold_disseccation = sum(damage == "mold"),
            inside_alive = sum(weevil == "inside_alive"),
            inside_dead = sum(weevil == "inside_dead"),
            pupa = sum(weevil == "pupa"),
            exited = sum(weevil == "escaped"))
View(survival_count)

# dataset with SURVIVAL counting per species, treatment and density
survival_density <- seedexp_dataset %>%
  filter(failed_exc == "ok") %>% 
  group_by(species, treatment, density, survival) %>%
  summarise(n_survival = n())
survival_density$density=as.factor(survival_density$density)
View(survival_density)

# WRONG dataset with SURVIVAL stats per species and density
survival_density_stats <- seedexp_dataset %>%
  filter(failed_exc == "ok") %>% 
  group_by(species, treatment, density, survival) %>%
  summarise(n_survival = n(),
            mean_surv = mean(n_survival, na.rm=TRUE),
            sd_surv = sd(n_survival, na.rm=TRUE),
            SE_surv = sd_surv/sqrt(n()))
View(survival_density_stats)

# dataset with SURVIVAL counting per species, treatment and density
survival_spe_tr <- seedexp_dataset %>%
  filter(failed_exc == "ok") %>% 
  group_by(species, treatment, survival) %>%
  summarise(n_survival = n())
survival_density$density=as.factor(survival_density$density)
View(survival_spe_tr)

# dataset with weevil counting
weevil_counting <- seedexp_dataset %>%
  group_by(species) %>%
  summarise(larvae = sum(weevil == "inside_alive", na.rm = T))
View(weevil_counting)

# dataset with 
damage_count <- seedexp_dataset %>%
  group_by(cage, species) %>%
  summarise(damaged = sum(survival == "damaged"))
View(damage_count)

# group of SURVIVAL dataframes per piechart
rocontr <- data.frame(acorns = c(582,
                                 3,
                                 5,
                                 3),
                      Survival = c("missing", "germinated", "damaged", "non germinated"))

roexcl <- data.frame(acorns = c(0, 
                                35, 
                                272, 
                                97),
                     Survival = c("missing", "germinated", "damaged", "non germinated"))
socontr <- data.frame(acorns = c(566,
                                 5,
                                 14,
                                 8),
                      Survival = c("missing", "germinated", "damaged", "non germinated"))

soexcl <- data.frame(acorns = c(0,
                                10,
                                158,
                                127),
                     Survival = c("missing", "germinated", "damaged", "non germinated"))


# GRAPHS

# basic hist missing count
hist(acorn_fate$missing)

# stack bar survival per density
new_labels <- c("RO" = "Red oak", "SO" = "Sessile oak") #to change facet_wrap labels
SB_density_survival <- ggplot(survival_density, aes(fill=factor(survival, 
                                                                level = c("germinated", "not_ger", "damaged", "missing")), 
                                                    y=n_survival, x=density)) + 
  geom_bar(position="fill", stat="identity", colour="black") + 
  scale_fill_manual(labels = c("germinated", "not germinated", "damaged", "missing"),
                    values = c("#1f78b4", "#a6cee3", "#b2df8a", "#33a02c")) +
  facet_grid(treatment ~ species,
             labeller = labeller(species = new_labels)) +
  labs(fill="Survival", x="Density", y="Acorns") +
  theme_classic()
SB_density_survival

RColorBrewer::display.brewer.all()

#values = c("#a0db01", "#b4048a", "#048ab4", "#db0133")

# pie chart weevils
PIE_w <- ggplot(weevil_counting, aes(x="", y=larvae, fill=species)) +
  geom_bar(stat="identity", alpha=.5, width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c('#A901DB','#04B486')) +
  labs(fill="Survival") +
  geom_text(aes(x=1.6, label=larvae),
            position = position_stack(vjust=0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
PIE_w

# pie chart with SURVIVAL counting RO CONTR
PIE_rocontr <- ggplot(rocontr, aes(x="", y=acorns, 
                                  fill=factor(Survival, level = c("germinated", "damaged", "non germinated", "missing")))) +
  geom_bar(stat="identity", alpha=.5, width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#a901db", "#be41e4", "#d480ed", "#f4dffb")) +
  labs(fill="Survival") +
  geom_text(aes(x=1.6, label=acorns),
            position = position_stack(vjust=0.5)) +
  ggtitle("Red Oak - control tretment") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
PIE_rocontr

# pie chart with SURVIVAL counting RO EXCL
PIE_roexcl <- ggplot(roexcl, aes(x="", y=acorns, 
                                  fill=factor(Survival, level = c("germinated", "damaged", "non germinated", "missing")))) +
  geom_bar(stat="identity", alpha=.5, width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#a901db", "#be41e4", "#d480ed", "#f4dffb")) +
  labs(fill="Survival") +
  geom_text(aes(x=1.6, label=acorns),
            position = position_stack(vjust=0.5)) +
  ggtitle("Red Oak - exclusion tretment") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
PIE_roexcl

# pie chart with SURVIVAL counting SO CONTR
PIE_socontr <- ggplot(socontr, aes(x="", y=acorns, 
                                  fill=factor(Survival, level = c("germinated", "damaged", "non germinated", "missing")))) +
  geom_bar(stat="identity", alpha=.5, width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#04b486", "#43c7a4", "#82dac3", "#c0ece1")) +
  labs(fill="Survival") +
  geom_text(aes(x=1.6, label=acorns),
            position = position_stack(vjust=0.5)) +
  ggtitle("Sessile Oak - control tretment") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
PIE_socontr

# pie chart with SURVIVAL counting SO EXCL
PIE_soexcl <- ggplot(soexcl, aes(x="", y=acorns, 
                                  fill=factor(Survival, level = c("germinated", "damaged", "non germinated", "missing")))) +
  geom_bar(stat="identity", alpha=.5, width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#04b486", "#43c7a4", "#82dac3", "#c0ece1")) +
  labs(fill="Survival") +
  geom_text(aes(x=1.6, label=acorns),
            position = position_stack(vjust=0.5)) +
  ggtitle("Sessile Oak - exclusion tretment") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
PIE_soexcl

# Piechart together
PIE_survival <- ggarrange(PIE_roexcl, PIE_soexcl, PIE_rocontr, PIE_socontr,
                    ncol = 2, nrow = 2)
PIE_survival

# MODELS

# T TESTS
t.test(damaged ~ species, data = damage_count)

# model MISSING
mod_missing <- glmmTMB(missing~species+density+(1|transect)+(1|block),
                      family = "nbinom2",
                      zi = ~.,
                      data = acorn_fate)
summary(mod_missing)

check_zeroinflation(mod_missing) # check zero inflation with performance

# diagnostics with DHARMa
testDispersion(mod_missing)

simulationOutput_mod_missing <- simulateResiduals(mod_missing)
plot(simulationOutput_mod_missing)


# model GERMINATED
mod_germinated <- glmmTMB(germinated~species+density+(1|transect)+(1|block),
                       family = nbinom1,
                       zi = ~.,
                       data = acorn_fate)
summary(mod_germinated)

check_zeroinflation(mod_germinated)

# diagnostics with DHARMa
testDispersion(mod_germinated)

simulationOutput_mod_germinated <- simulateResiduals(mod_germinated)
plot(simulationOutput_mod_germinated)