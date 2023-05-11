getwd()

# Importing standard packages
library(dplyr)
library(ggplot2)
library(plotly)

seedrain_dataset<-read.csv("Acorn counting_master_new.csv", header = T, sep=";", dec=".")
seedrain_dataset$year=as.factor(seedrain_dataset$year)
seedrain_dataset$month=as.factor(seedrain_dataset$month)
seedrain_dataset$plot=as.factor(seedrain_dataset$plot)
seedrain_dataset$tree=as.factor(seedrain_dataset$tree)
seedrain_dataset$species=as.factor(seedrain_dataset$species)
seedrain_dataset$area_traps_m2=as.numeric(seedrain_dataset$area_traps_m2)
seedrain_dataset$acorns=as.numeric(seedrain_dataset$acorns)
seedrain_dataset$aborted_tot=as.numeric(seedrain_dataset$aborted_tot)
summary(seedrain_dataset)

# dataset with acorns stats
acorns_stats <- seedrain_dataset %>%
  group_by(species, month) %>% 
  filter(!is.na(species)) %>%
  summarize(mean_acorns = mean(acorns, na.rm=TRUE), 
            sd_acorns = sd(acorns, na.rm=TRUE),
            n_acorns = n(),
            SE_acorns = sd_acorns/sqrt(n()))
View(acorns_stats)

# dataset with ABORTED acorns stats
ab_acorns_stats <- seedrain_dataset %>%
  group_by(species, month) %>% 
  filter(!is.na(species)) %>%
  summarize(mean_abacorns = mean(aborted_tot, na.rm=TRUE), 
            sd_abacorns = sd(aborted_tot, na.rm=TRUE),
            n_abacorns = n(),
            SE_abacorns = sd_abacorns/sqrt(n()))
View(ab_acorns_stats)

# T Tests
t.test(acorns~species, 
       data = seedrain_dataset,
       conf.level = 0.95)

# basic hist
hist(seedrain_dataset$acorns,
     xlab="Tick load on host",
     main="Ticks distribution",
     col="grey",
     breaks=50)

# Setting resolution options
options(repr.plot.width = 10, repr.plot.height = 4, repr.plot.res = 300)

# density chart acorn distribution among species
acornDC <- ggplot (subset(seedrain_dataset, !is.na(acorns), !is.na(species)), 
                             aes(acorns, fill = species)) +
  geom_density(alpha = 0.5, size=.5) +
  labs(x=expression(paste("Acorns/ ", "m"^"2 ", "per month")), 
       y="Density", fill="Species") + 
  scale_x_log10() +
  #facet_grid(~factor(month, level = c("October", "November"))) +
  scale_fill_manual(values = c('#A901DB','#04B486'),
                    labels = c("Red aok", "Sessile oak")) +
  theme_classic()
acornDC

# density chart ABORTED acorn distribution among species
acornDC <- ggplot (subset(seedrain_dataset, !is.na(acorns), !is.na(species)), 
                   aes(x=aborted_tot, fill = species)) +
  geom_density(alpha = 0.5, size=.5) +
  labs(x=expression(paste("Aborted acorns/ ", "m"^"2 ", "per month")), 
       y="Density", fill="Species") + 
  scale_x_log10() +
  facet_grid(~factor(month, level = c("October", "November"))) +
  ggtitle("Red aok and sessile oak aborted acorns in 2021 (log scale)") +
  scale_fill_manual(values = c('#A901DB','#04B486')) +
  theme_bw()
acornDC

# bar graph mean acorn/m2 variation along season
acorns_b <- ggplot(acorns_stats, aes(x=factor (month, level=c("October", "November")), 
                                     y=mean_acorns, fill=species, na.rm=TRUE)) +
  geom_bar(stat = "identity", color = "black", size = 0.5,
           position = position_dodge()) +
  geom_errorbar(aes(ymin=mean_acorns - SE_acorns, ymax=mean_acorns + SE_acorns), 
                width=.2, size=0.5, position = position_dodge(.9)) +
  labs(x="Month", y=expression(paste("Mean acorns/ ", " m"^"2")), fill="Species") + 
  theme_classic() +
  scale_fill_manual(values=c('#a6cee3','#b2df8a'),
                    labels = c("Red oak", "Sessile oak")) +
  theme_classic()
acorns_b

ggarrange(acornDC, acorns_b, ncol=2, common.legend = TRUE, legend="bottom")

# bar graph mean ABORTED acorn/m2 variation along season
abacorns_b <- ggplot(ab_acorns_stats, aes(x=factor (month, level=c("October", "November")), 
                                     y=mean_abacorns, fill=species, na.rm=TRUE)) +
  geom_bar(stat = "identity", color = "black", size = 0.5,
           position = position_dodge()) +
  geom_errorbar(aes(ymin=mean_abacorns - SE_abacorns, ymax=mean_abacorns + SE_abacorns), 
                width=.2, size=0.5, position = position_dodge(.9)) +
  labs(x="Month", y=expression(paste("Mean aborted acorns/ ", "m"^"2")), fill = "Species") + 
  theme_classic() +
  scale_fill_manual(values=c('#a6cee3','#b2df8a'),
                    labels=c("Red oak", "Sessile oak")) +
  theme_classic()
abacorns_b

