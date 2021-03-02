## Statistical analyses for the manuscript:
## "Acquisition of conditioned fear is impaired in schizophrenia – a pooled analysis of 4 Pavlovian fear conditioning studies"
##  by Lauri Tuominen, Liana Romaniuk, Mohammed R Milad, Donald C. Goff, Jeremy Hall, and Daphne Holt
# load libraries 
lapply(c('nlme', 'lmerTest', 'robustbase', 'effsize','metafor','ggplot2','gtable'), require, character.only = TRUE)

# load data
data <- read.csv('~/Documents/Research/SCZ_SCR/CombinedSCR_all.csv')
data <- subset(data, Exclusion == 'Incl')

# calculate a contrast between CS+ and CS-
data$contrast <- data$Csplus-data$Csminus

# remove an outlier from the data 
data <- subset(data, ID!='USRE')

# test if SCR responses to the contrast, CS+ or CS- are different between the groups while accounting for study

m.contrast.cov <- summary(lme(contrast ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))
m.Csminus.cov <- summary(lme(Csminus ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))
m.Csplus.cov <- summary(lme(Csplus ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))

# write group effects 
sink('~/Documents/Research/SCZ_SCR/group.differences.txt')
print('Group difference in CS+ CS- contrast age sex covariates')
print(m.contrast.cov)
print('Group difference in CS- age sex covariates')
print(m.Csminus.cov)
print('Group difference in CS+ age sex covariates')
print(m.Csplus.cov)
sink()


### Test for symptom correlations in the SCZ group 
SCZdata <- subset(data, Group=='SCZ')
clinical <- c("PANSS.POS", "PANSS.NEG", "PANSS.GEN", "PANSS.TOT", "CPZ")
outcome <- c('contrast', 'Csplus', 'Csminus' )

results <- matrix(nrow=3, ncol=15)
r=1 
for (y in outcome){
  k=1
  for (c in clinical){
    f <- as.formula(paste(paste(y, c, sep='~'), "gender + Age.Y + STUDY", sep ='+' ))
    m <- summary(lmrob(f, data=SCZdata))
    results[r,k] <- round(m$coefficients[2,1],5)
    results[r,k+1] <- round(m$coefficients[2,3],2)
    results[r,k+2] <- round(m$coefficients[2,4],3)
    k=k+3
    }
  r=r+1 
}
cnames <- paste(rep(clinical, each=3), c('b', 't', 'p'), sep='.')
colnames(results) <- cnames
rownames(results) <- outcome
write.csv( results, '~/Documents/Research/SCZ_SCR/clinical_correlations.csv')

## test for the effects of PDI on CS- 
PDIdata <- SCZdata[complete.cases(SCZdata$PDI.total), ]
SCZdata$PDIadjN <- ifelse(SCZdata$STUDY=='TUOMINEN_XXXX', SCZdata$PDI.total/21, SCZdata$PDI.total/40) 
mrob.PDI <- summary(lmrob(Csminus ~ PDIadjN + STUDY +Age.Y+gender, data=PDIdata))
sink('~/Documents/Research/SCZ_SCR/PDIxCSminus.txt')
print(mrob.PDI)
sink()

## calculate overall effect size using metafor 
means = aggregate(data$contrast, by = list(data$STUDY,data$Group),FUN = mean)
sds = aggregate(data$contrast, by = list(data$STUDY,data$Group),FUN = sd)
d <- data.frame(means$Group.1,means$Group.2,means$x,sds$x)
N <- data.frame(table(data$STUDY, data$Group))
d <- cbind(d, N$Freq)
d_SCZ <- subset(d, means.Group.2 == 'SCZ')
names(d_SCZ) <- c('Study','Group','SCZ_AVG', 'SCZ_STD', 'SCZ_N') 
d_CTR <- subset(d, means.Group.2 == 'CTR')
names(d_CTR) <- c('Study','Group','CTR_AVG', 'CTR_STD', 'CTR_N') 
D <- cbind(d_SCZ[,c('Study', 'SCZ_AVG','SCZ_STD', 'SCZ_N')], d_CTR[,c('CTR_AVG', 'CTR_STD', 'CTR_N')])
metadat <- escalc(measure="SMD", m2i=SCZ_AVG, sd2i=SCZ_STD, n2i=SCZ_N,
               m1i=CTR_AVG, sd1i=CTR_STD, n1i=CTR_N, data=D)
metares <- rma(yi, vi, data=metadat)
combined.cohend <- round(metares$b[1],2)
write.csv(metadat, '~/Documents/Research/SCZ_SCR/metadata.csv')

sink('~/Documents/Research/SCZ_SCR/sessionInfo.txt')
print(sessionInfo(),locale=FALSE)
sink()

