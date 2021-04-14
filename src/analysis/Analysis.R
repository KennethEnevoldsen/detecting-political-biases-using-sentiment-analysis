setwd("~/Dropbox/Cog'com exam/Results")

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(multcomp)
library(pastecs)
library(lme4)
library(lmerTest)
library(Hmisc)
library(car)
library(compute.es)

#read data
folder = "~/Dropbox/Cog'com exam/Results/First result"
fileList = list.files(path = folder, pattern = "*.csv")
data1 = do.call(rbind, lapply(fileList, function(x)
  read.csv(x, stringsAsFactors = F)))
data1$period = 'Jan 1, 2015 - Jun 31, 2015'

folder = "~/Dropbox/Cog'com exam/Results/Second result"
fileList = list.files(path = folder, pattern = "*.csv")
data2 = do.call(rbind, lapply(fileList, function(x)
  read.csv(x, stringsAsFactors = F)))
data2$period = 'Jul 1, 2015 - Dec 31, 2015'

folder = "~/Dropbox/Cog'com exam/Results/Third result"
fileList = list.files(path = folder, pattern = "*.csv")
data3 = do.call(rbind, lapply(fileList, function(x)
  read.csv(x, stringsAsFactors = F)))
data3$period = 'Jan 1, 2016 - Jun 31, 2016'

data = dplyr::bind_rows(data1, data2, data3)
data$all = 'Jan 1, 2015 - Jun 31, 2016'

#releveling period factors
data$period = factor(
  data$period,
  levels = c(
    'Jan 1, 2015 - Jun 31, 2015',
    'Jul 1, 2015 - Dec 31, 2015',
    'Jan 1, 2016 - Jun 31, 2016'
  )
)
levels(data$period)

#renaming variables
data$avis[data$avis == "berlingske"] = "Berlingske"
data$avis[data$avis == "information"] = "Information"
data$parti[data$parti == "a"] = "Alternativet"
data$parti[data$parti == "la"] = "Liberal Alliance"


#constructing a new column (avis og parti combined)
data$avisparti = as.factor(paste(data$avis, data$parti))

#leveneTest for homogeneity of variance
leveneTest(data$a_score, interaction(data$avis, data$parti))

#Boxplots
ggplot(data, aes(avis, a_score)) +
  geom_boxplot()

ggplot(data, aes(avisparti, a_score)) +
  geom_boxplot()

#barGraphs
ggplot(data, aes(avis, a_score, fill = avis)) +
  geom_bar(stat = 'summary', fun.y = mean) +
  geom_errorbar(stat = 'summary',
                fun.data = mean_se,
                width = 0.2) +
  theme(legend.position = "none")

ggplot(data, aes(parti, a_score, fill = parti)) +
  geom_bar(stat = 'summary', fun.y = mean) +
  geom_errorbar(stat = 'summary',
                fun.data = mean_se,
                width = 0.2) +
  theme(legend.position = "none")

ggplot(data, aes(avisparti, a_score, fill = avisparti)) +
  geom_bar(stat = 'summary', fun.y = mean) +
  geom_errorbar(stat = 'summary',
                fun.data = mean_se,
                width = 0.2) +
  theme(legend.position = "none")

#grouped barplot
ggplot(data, aes(parti, a_score)) +
  geom_bar(aes(fill = avis), position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 0, color = 'black')


ggplot(data, aes(parti, a_score)) +
  geom_bar(aes(fill = avis), position = 'dodge', stat = 'summary')

#sentiment curve
ggplot(data, aes(avis, a_score, color = parti, group = parti)) +
  geom_point(stat = 'summary', fun.y = mean) +
  stat_summary(fun.y = mean, geom = "line", aes(group = parti)) +
  geom_errorbar(stat = 'summary',
                fun.data = mean_se,
                width = 0.1) +
  labs(x = "Paper", y = "Mean Sentiment Score", color = "Party") +
  scale_y_continuous(breaks = seq(-0.005, 0.025, 0.005)) +
  facet_wrap( ~ period)

ggplot(data, aes(avis, a_score, color = parti, group = parti)) +
  geom_point(stat = 'summary', fun.y = mean) +
  stat_summary(fun.y = mean, geom = "line", aes(group = parti)) +
  geom_errorbar(stat = 'summary',
                fun.data = mean_se,
                width = 0.1) +
  labs(x = "Paper", y = "Mean Sentiment Score", color = "Party") +
  scale_y_continuous(breaks = seq(-0.005, 0.025, 0.005)) +
  facet_wrap( ~ all)


#Assumptions
by(data$a_score, data$avis, stat.desc, F, norm = T)
histogrammer = function (x) {
  temp = data[data$avis == x,]
  ggplot(temp, aes(temp$a_score)) +
    theme(legend.position = "none") +
    geom_histogram(
      aes(y = ..density..),
      fill = "white",
      color = "black",
      binwidth = 0.005
    ) +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean(temp$a_score, na.rm = T),
        sd = sd(temp$a_score, na.rm = T)
      ),
      color = "blue",
      size = 1
    )
}
histogrammer('Berlingske')
histogrammer('Information')
qplot(sample = data$a_score[data$avis == 'Berlingske'])
qplot(sample = data$a_score[data$avis == 'Information'])
qplot(sample = data$a_score)

#ANOVA - planned contrast
contrasts(data$avisparti) = cbind(c(1, -1, 1, -1), c(0, 1, 0, -1), c(-1, 0, 1, 0))
m = aov(a_score ~ avisparti, data)
summary.lm(m)
#significant as fuck
contrasts(data$avisparti) = cbind(c(-1, -1, 1, 1), c(-1, 1, 0, 0), c(0, 0, 1, -1))
m1 = aov(a_score ~ avisparti, data)
summary.lm(m1)
table(data$avisparti)

#two by two factorial anova
m2 = aov(a_score ~ avis * parti, data)
summary(m2)
summary.lm(m2)
Anova(m2, type = 'III')
# post hoc pairwise t.test
pairwise.t.test(data$a_score, data$avisparti, p.adjust.method = "BH")

pairwise.t.test(data$a_score, data$avisparti, p.adjust.method = "bonferroni")

BLA - BA
IA - BA
ILA - BA

ILA - IA
ILA - BLA
IA - BLA

summary(multcomp::glht(m1, linfct = mcp(avisparti = "Tukey")))

# means and sd for all combinations of parties and papers
mba = mean(data$a_score[data$avisparti == 'Berlingske Alternativet'])
mbla = mean(data$a_score[data$avisparti == 'Berlingske Liberal Alliance'])
mia = mean(data$a_score[data$avisparti == 'Information Alternativet'])
mila = mean(data$a_score[data$avisparti == 'Information Liberal Alliance'])

sdba = sd(data$a_score[data$avisparti == 'Berlingske Alternativet'])
sdbla = sd(data$a_score[data$avisparti == 'Berlingske Liberal Alliance'])
sdia = sd(data$a_score[data$avisparti == 'Information Alternativet'])
sdila = sd(data$a_score[data$avisparti == 'Information Liberal Alliance'])

#standard deviations and correlations between papers
mes(mba, mbla, sdba, sdbla, 90, 90)
mes(mia, mila, sdia, sdila, 90, 90)
#standard deviations and correlations between parties
mes(mba, mia, sdba, sdia, 90, 90)
mes(mbla, mila, sdba, sdila, 90, 90)

mila
sdila
miaa
sdia
mbla
mba
