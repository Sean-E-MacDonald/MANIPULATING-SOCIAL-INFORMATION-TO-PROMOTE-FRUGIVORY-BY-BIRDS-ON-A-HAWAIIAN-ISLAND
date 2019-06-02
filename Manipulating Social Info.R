###############################################ANALYSES##########################################################
#################################################################################################################

# dataframe of no. of frugivores detected at 30m during the control period vs
# expected frequency during control periods based on avian surveys
df.chi.sqr.con <- data.frame(Observed = c(102, 71, 17, 66),
                             Expected = c(80.207, 109.357, 17.615, 48.822))

# dataframe of no. of frugivores detected at 30m during the treatment period vs
# expected frequency during treatment periods based on avian surveys
df.chi.sqr.treat <- data.frame(Observed = c(563, 169, 98, 173),
                               Expected = c(314.248, 428.456, 69.015, 191.281))

# dataframe of no. of frugivores detected at 30m during the both periods of the experiment vs
# expected frequency during both periods of the experiment based on avian surveys
# Apply this analysis? Didn't do in paper, but should I include? 
df.chi.sqr.obs.compare.exp.sur <- data.frame(Observed = c(665, 240, 115, 239),
                                             Expected = c(394.455, 537.813, 86.63, 240.103))

# dataframe of no. of frugivores detected at 30m during the control periods vs treatment periods
df.chi.sqr.obs.compare.exp <- data.frame(Observed = c(563, 169, 98, 173),
                                         Expected = c(102, 71, 17, 66))

# Applying chi-square tests to above dataframes
chisq.test(df.chi.sqr.con)
chisq.test(df.chi.sqr.treat)
chisq.test(df.chi.sqr.obs.compare.exp)

######################################################################################################################
# Create a dataframe of the no. of jawe observed at any distance during the control and treatment periods compared 
# to the no. expected during these periods based on avian survey data
df.odds.ratio.jawe <- matrix(c(80, 102, 314, 563), 2,2)
rownames(df.odds.ratio.jawe) <- c("Expected", "Observed")
colnames(df.odds.ratio.jawe) <- c("Control", "Treatment")

# Create a dataframe of the no. of rble observed during the control and treatment periods compared to the no. 
# expected during these periods based on avian survey data
df.odds.ratio.rble <- matrix(c(109, 71, 428, 169), 2,2)
rownames(df.odds.ratio.rble) <-  c("Expected", "Observed")
colnames(df.odds.ratio.rble) <-  c("Control", "Treatment")

# Create a dataframe of the no. of rvbu observed during the control and treatment periods compared to the no. 
# expected during these periods based on avian survey data
df.odds.ratio.rvbu <- matrix(c(17, 17, 69, 98), 2,2)
rownames(df.odds.ratio.rvbu) <- c("Expected", "Observed")
colnames(df.odds.ratio.rvbu) <- c("Control", "Treatment")

# Create a dataframe of the no. of rwbu observed during the control and treatment periods compared to the no. 
# expected during these periods based on avian survey data
df.odds.ratio.rwbu <- matrix(c(49, 66, 191, 173), 2,2)
rownames(df.odds.ratio.rwbu) <-  c("Expected", "Observed")
colnames(df.odds.ratio.rwbu) <-  c("Control", "Treatment")

library(epitools)
# Apply a Wald's odds-ratio test to each dataframe to determine if the differences in frequency of species between 
# the experiment and the avian surveys were significant 
oddsratio.wald(df.odds.ratio.jawe)
oddsratio.wald(df.odds.ratio.rble)
oddsratio.wald(df.odds.ratio.rvbu)
oddsratio.wald(df.odds.ratio.rwbu)

######################################################################################################################
# make lists of raw data of the # of frugivores (jawe, rble, rvbu, rwbu) that were detected at any distance
frug.con = c(0, 2,	0,	4,	2,	1,	2,	0,	1,	6,	5,	0,	0,	3,	3,	3,	4,	3,	0,	0,	2,	0,	0,	2,	4,	0,	0,	9,	3,	2,	11,	3,	17,	6,	0,	2,	0,	3,	1,	2,	0,	0,	0,	7,	2,	7,	5,	6,	3,	0,	2,	1,	7,	1,	6,	5,	7,	3,	3,	15,	2,	6,	0,	1,	5,	6,	5,	1,	6,	1,	3,	2,	3,	1,	2,	6,	5,	3,	2,	10)
frug.treat = c(5,	7,	1,	7,	2,	2,	9,	7,	9,	24,	7,	14,	4,	8,	14,	14,	15,	10,	21,	5,	5,	1,	13,	31,	46,	17,	17,	0, 20,	15,	22,	21,	29,	12,	21,	9,	7,	13,	11,	9,	5,	10,	4,	21,	11,	2,	16,	8,	18,	15,	16,	11,	19,	6,	7,	7,	4,	21,	6,	14,	19,	10,	19,	7,	4,	18,	12,	11,	23,	6,	13,	9,	5,	5,	18,	25,	10,	15,	17,	32)

#data frame of raw data so I can make some figures
frug.all <- data.frame(period = rep(c("Control", "Treatment"), each = 80),
                       frugivore = c(frug.con, frug.treat))

# create column of experiment
frug.all$experiment <- data.ff.area$experiment

# Change class from integer to factor
frug.all$experiment <- as.factor(df.frug$experiment)

# Check dataframe for correct class across different columns
sapply(frug.all, class)

sum(frug.all$frugivore)

library(lme4)
# Apply a generalized linear mixed model with a poisson distribution
# For each glmm period is the fixed effect and trial is the random effect

# a glmm for the no. of frugivores detected comparing control v. treatment periods
model1a <- glmer(frugivore ~ period + (1 | experiment), data = frug.all, family = poisson)

summary(model1a)
####################################################################################################################

# Import dataset and establish it as dataframe
time.all.count <- read.csv("time.all.spp.count.csv", header = TRUE)

# Change class from integer to factor
time.all.count$experiment <- as.factor(time.all.count$experiment)

# Check dataframe for correct class across different columns
sapply(time.all.count, class)

library(lme4)
# Apply a generalized linear mixed model with a poisson distribution
# For each glmm period is the fixed effect and trial is the random effect

# a glmm for the amount of time each jawe was within 10m of focal plant; control v. treatment periods
model1 <- glmer(jawe ~ period + (1 | experiment), data = time.all.count, family = poisson)

# a glmm for the amount of time each rble was within 10m of focal plant; control v. treatment periods
model2 <- glmer(rble ~ period + (1 | experiment), data = time.all.count, family = poisson)

# a glmm for the amount of time each rvbu was within 10m of focal plant; control v. treatment periods
model3 <- glmer(rvbu ~ period + (1 | experiment), data = time.all.count, family = poisson)

# a glmm for the amount of time each rwbu was within 10m of focal plant; control v. treatment periods
model4 <- glmer(rwbu ~ period + (1 | experiment), data = time.all.count, family = poisson)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
####################################################################################################

# Import dataset and establish it as dataframe
data.ate <- read.csv("data.ate.treat.counts.csv", header = TRUE)

# create a dataframe to to determine if no. of focal bird species that 
# ate fruit during control vs. treatment differs by bird species 
jawe <- cbind(data.ate$jawe)

rble <- cbind(data.ate$rble)

rvbu <- cbind(data.ate$rvbu)

rwbu <- cbind(data.ate$rwbu)

data.ate2 <- data.frame(trial = rep(1:80, each = 2),
                        period = rep(c("control", "treatment"), each = 1),                    
                        species = rep(c("jawe", "rble", "rvbu", "rwbu"), each = 160),
                        response = rbind(jawe, rble, rvbu, rwbu))

names(data.ate2) <- c("trial", "period", "species", "response")

# Change class from integer to factor
data.ate2$trial <- as.factor(data.ate2$trial)

sapply(data.ate2, class)

library(lme4)
library(lsmeans)
# Apply a generalized linear model
# For each glmm, total number of frugivores that ate fruit from focal plants and at distance as 
# response variable; period and frugivore species as fixed effects; 
# trial as random effect 

model5 <- glmer(response ~ species * period + (1 | trial), data = data.ate2, family = poisson)

summary(model5)

# Generate summary of coefficients for each model
m = lsmeans(model5, ~ species + period, type = "response")

contrast(m, method = "pairwise")

summary(m)

##################################################################################################################
# Import dataset and establish it as dataframe
cax_data <- read.csv("glmm.df.1.csv", header = TRUE)

# Check dataframe for correct class across different columns
sapply(cax_data, class)

# Change class from integer to factor
cax_data$experiment <- as.factor(cax_data$experiment)
cax_data$track.order <- as.factor(cax_data$track.order)

library(lme4)
# Apply a generalized linear mixed model with repeated measures and has a binomial distribution
# For each glmm have bird species as response variable; track species as fixed effect; 
# Order and trial as random effects

# Apply a glmm to dataframe for Japanese white-eye (jawe)
model6 <- glmer(jawe ~ track.spp + (1 | track.order) + (1 | experiment), data = cax_data, family = binomial)

# Apply a glmm to dataframe for Red-billed leiothrix (rble)
model7 <- glmer(rble ~ track.spp + (1 | track.order) + (1 | experiment), data = cax_data, family = binomial)

# Apply a glmm to dataframe for Red-whiskered bulbul (rwbu)
model8 <- glmer(rvbu ~ track.spp + (1 | track.order) + (1 | experiment), data = cax_data, family = binomial)

# Apply a glmm to dataframe for Red-vented bulbul (rvbu)
model9 <- glmer(rwbu ~ track.spp + (1 | track.order) + (1 | experiment), data = cax_data, family = binomial)

# Generate summary of coefficients for each model
summary(model6)
summary(model7)
summary(model8)
summary(model9)

library(lsmeans)
a = lsmeans(model6, ~track.spp, type = "response")
b = lsmeans(model7, ~track.spp, type = "response")
c = lsmeans(model8, ~track.spp, type = "response")
d = lsmeans(model9, ~track.spp, type = "response")

summary(a)
summary(b)
summary(c)
summary(d)

a2 <- summary(a)
b2 <- summary(b)
c2 <- summary(c)
d2 <- summary(d)

a2$prob
b2$prob
c2$prob
d2$prob

# made a dataframe with the predicted probabilities of birds response strength from glmm's
df.glmm <- rbind(a2, b2, c2, d2)
Model.name <- c(rep("JAWE", 6), rep("RBLE", 6), rep("RVBU", 6), rep("RWBU", 6))
df.glmm <- cbind(Model.name, df.glmm)
df.glmm$track.spp <- toupper(df.glmm$track.spp)
df.glmm$track.spp[df.glmm$track.spp=="CONTROL"] <- "Control"

#################################################################################################################
###############################################FIGURES###########################################################

# dataframe of mean and SE of # of frugivores detected by period of raw data
frug.det.mean <- data.frame(Period=c("Control", "Treatment"), Mean_frug=c(3.2, 12.54))
lower.mean = c(2.83, 11.63)
upper.mean = c(3.57, 13.45)

# create a dotplot of the mean # of frugivores detected by period of raw abundance data
library(ggplot2)
dp1 <-ggplot(data = frug.det.mean, aes(x=Period, y=Mean_frug, fill=Period)) +  
  geom_errorbar(aes(ymin = lower.mean, ymax = upper.mean, width = 0.1), size =1.15) +  
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize=1.5, show.legend = FALSE) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  labs(x="", y="Mean # Frugivores Detected / Trial") +
  theme_classic(base_size = 22) +
  scale_fill_brewer(palette = "Set1")

dp1

# Dataframe of means and sems of raw data 
foc.fru <- data.frame(Period=c("Control", "Treatment"), Mean_frug=c(0.11, 1.61))
lower.mean.2 = c(0.032, 1.127)
upper.mean.2 = c(0.192, 2.098)

# doplot of mean and SE bars for frugivores that consumed focal fruit during control vs treatment periods
library(ggplot2)
dp2 <- ggplot(data = foc.fru, aes(x=Period, y=Mean_frug, fill=Period)) +  
  geom_errorbar(aes(ymin = lower.mean.2, ymax = upper.mean.2, width = 0.1), size =1.15) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize=1.5, show.legend = FALSE) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.25)) +
  labs(x="", y="Mean # of Frugivory Events / Trial") +
  theme_classic(base_size = 22) +
  scale_fill_brewer(palette = "Set1")  

dp2

####################################################################################################################
# create a dataframe of the pearsons chi-squared estimates vs. observed no. of frugivores 
# during control and treatment periods
df.chi.sqr <- data.frame(Species = c("Japanese white-eye", "Red-billed leiothrix",
                                     "Red-whiskered bulbul", "Red-vented bulbul",
                                     "Japanese white-eye", "Red-billed leiothrix",
                                     "Red-whiskered bulbul", "Red-vented bulbul"),
                         Period = rep(c("Control", "Treatment"), each = 4),
                         Method = rep(c("Expected", "Observed"), each = 8),
                         Frugivore = c(80, 109, 49, 18, 314, 428, 191, 69,
                                       102, 71, 66, 17, 563, 169, 173, 98))

#Bar graph of the chi squared estimates compared to observed no. of frugivores detected
Fig6 <- ggplot(data = df.chi.sqr, aes(x = Species, y = Frugivore, fill=Method)) +  #fill color by group
  geom_bar(position = "dodge", stat="identity", width = 0.7, colour="black", show.legend = FALSE) + 
  facet_grid(~Period) +
  scale_x_discrete(limits = c("Japanese white-eye", "Red-billed leiothrix",
                              "Red-whiskered bulbul", "Red-vented bulbul")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) + #move the floating bar to 0,0 and set the yaxis
  labs(x="Species", y="Number of Frugivores") + #label the axes
  theme_classic(base_size = 15) +  # increase font size of all text 
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set1")

Fig6

##############################################################################################################
# Create a dataframe of the total no. of minutes each frugivore species was within 10m of a plant during
# control vs treatment periods of the experiment
df.frug.time <- data.frame(Species = c("Japanese white-eye", "Red-whiskered bulbul",
                                       "Red-billed leiothrix", "Red-vented bulbul",
                                       "Japanese white-eye", "Red-whiskered bulbul",
                                       "Red-billed leiothrix", "Red-vented bulbul"),
                           Period = rep(c("Control", "Treatment"), each = 4),
                           Frugivore = c(253, 264, 139, 34, 2302, 637, 636, 367))

Fig9 <- ggplot(data = df.frug.time, aes(x = Species, y = Frugivore, fill=Period)) +  #fill color by group
  geom_bar(position = position_dodge(width=0.7), stat="identity", width = 0.7, colour="black", show.legend = FALSE) + 
  scale_x_discrete(limits = c("Japanese white-eye", "Red-whiskered bulbul",
                              "Red-billed leiothrix", "Red-vented bulbul")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2500)) + #move the floating bar to 0,0 and set the yaxis
  labs(x="Species", y="No. of Minutes near Plant") + #label the axes
  theme_classic(base_size = 15) +  # increase font size of all text 
  theme(legend.position = "top") +
  scale_fill_grey()

Fig9

##############################################################################################################
# create a dataframe of the mean no. of frugivory events detected at any distance and plant by focal frugivores
# comparing control and treatment periods of the experiment
df.frug.ate.dp <- data.frame(Species = c("Japanese white-eye", "Red-whiskered bulbul",
                                         "Red-billed leiothrix", "Red-vented bulbul",
                                         "Japanese white-eye", "Red-whiskered bulbul",
                                         "Red-billed leiothrix", "Red-vented bulbul"),
                             Period = rep(c("Control", "Treatment"), each = 4),
                             Frugivore = c(0.0875, 0.025, 0, 0, 1.2125, 0.25, 0.075, 0.075))

# standard error bars
se.low = c(0.012564, 0.000311, 0, 0, 0.805883, 0.050851, 0.012207, 0.022958)
se.high = c(0.162436, 0.049689, 0, 0, 1.619117, 0.449149, 0.137793, 0.127042)

# Create a bargraph of the mean no. of frugivory events detected at any distance and plant by focal frugivores
# comparing control and treatment periods of the experiment with standard error bars
Fig8 <- ggplot(data = df.frug.ate.dp, aes(x = Species, y = Frugivore, fill=Period)) +  #fill color by group
  geom_bar(position = position_dodge(width=0.7), stat="identity", width = 0.7, colour="black", show.legend = FALSE) + 
  geom_errorbar(position = position_dodge(width=0.7), aes(ymin = se.low, ymax = se.high, width = 0.2), size =1.15) +
  scale_x_discrete(limits = c("Japanese white-eye", "Red-whiskered bulbul",
                              "Red-billed leiothrix", "Red-vented bulbul")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) + #move the floating bar to 0,0 and set the yaxis
  labs(x="Species", y="Mean No. of Frugivory Events") + #label the axes
  theme_classic(base_size = 15) +  # increase font size of all text 
  theme(legend.position = "top") +
  scale_fill_grey()

Fig8

##############################################################################################################
# A bar graph of the predicted probability of a JAWE responding to a given playback track
library(ggplot2)
fig.glmm.1 <- ggplot(data = df.glmm, aes(x=Model.name, y=prob, fill= factor(track.spp))) +  
  geom_bar(stat="identity", position = position_dodge(0.8), width = 0.75, show.legend = FALSE,
           colour="black") + 
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size = 1.15, width=0.2,
                position=position_dodge(0.8)) +
  scale_x_discrete(limits = "JAWE") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x="Playback track", y="Probability of Response") +
  theme_classic(base_size = 22) +  # increase font size of all text
  theme(legend.position = "top") +
  ggtitle("Japanese white - eye") +
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette="YlGnBu")

fig.glmm.1

# A bar graph of the predicted probability of a RBLE responding to a given playback track
library(ggplot2)
fig.glmm.2 <- ggplot(data = df.glmm, aes(x=Model.name, y=prob, fill= factor(track.spp))) +  
  geom_bar(stat="identity", position = position_dodge(0.8), width = 0.75, show.legend = FALSE,
           colour="black") + 
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size = 1.15, width=0.2,
                position=position_dodge(0.8)) +
  scale_x_discrete(limits = c("RBLE")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x="Playback track", y="Probability of Response") +
  theme_classic(base_size = 22) +  # increase font size of all text
  theme(legend.position = "top") +
  ggtitle("Red - billed leiothrix") +
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette="YlGnBu")

fig.glmm.2

# A bar graph of the predicted probability of a RVBU responding to a given playback track
library(ggplot2)
fig.glmm.3 <- ggplot(data = df.glmm, aes(x=Model.name, y=prob, fill= factor(track.spp))) +  
  geom_bar(stat="identity", position = position_dodge(0.8), width = 0.75, show.legend = FALSE,
           colour="black") + 
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size = 1.15, width=0.2,
                position=position_dodge(0.8)) +
  scale_x_discrete(limits = c("RVBU")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x="Playback track", y="Probability of Response") +
  theme_classic(base_size = 22) +  # increase font size of all text
  theme(legend.position = "top") +
  ggtitle("Red - vented bulbul") +
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette="YlGnBu")

fig.glmm.3

# A bar graph of the predicted probability of a RWBU responding to a given playback track
library(ggplot2)
fig.glmm.4 <- ggplot(data = df.glmm, aes(x=Model.name, y=prob, fill= factor(track.spp))) +  
  geom_bar(stat="identity", position = position_dodge(0.8), width = 0.75, show.legend = FALSE,
           colour="black") + 
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size = 1.15, width=0.2,
                position=position_dodge(0.8)) +
  scale_x_discrete(limits = c("RWBU")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x="Playback track", y="Probability of Response") +
  theme_classic(base_size = 22) +  # increase font size of all text
  theme(legend.position = "top") +
  ggtitle("Red - whiskered bulbul") +
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette="YlGnBu")

fig.glmm.4

###############################################################################################################