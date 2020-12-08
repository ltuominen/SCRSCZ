## Statistical analyses for the manuscript:
## "Acquisition of conditioned fear is impaired in schizophrenia – a pooled analysis of 4 Pavlovian fear conditioning studies"
##  by Lauri Tuominen, Liana Romaniuk, Mohammed R Milad, Donald C. Goff, Jeremy Hall, and Daphne Holt

# load libraries 
lapply(c('nlme', 'lmerTest','effsize','metafor','ggplot2','gtable'), require, character.only = TRUE)
source('~/Documents/Research/SCZ_SCR/returnAdj.R')

# load data
data <- read.csv('~/Documents/Research/SCZ_SCR/CombinedSCR_new.csv')
data <- subset(data, Exclusion == 'Incl')

# calculate a contrast between CS+ and CS-
data$contrast <- data$Csplus-data$Csminus

# remove an outlier from the data 
data <- subset(data, ID!='USRE')

# test if SCR responses to the contrast, CS+ or CS- are different between the groups while accounting for study
m.contrast <- anova(lme(contrast ~ Group, random =~ 1|STUDY, data=data))
m.Csminus <- anova(lme(Csminus ~ Group, random =~ 1|STUDY, data=data))
m.Csplus <- anova(lme(Csplus ~ Group, random =~ 1|STUDY, data=data))

m.contrast.cov <- anova(lme(contrast ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))
m.Csminus.cov <- anova(lme(Csminus ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))
m.Csplus.cov <- anova(lme(Csplus ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))

# write group effects 
sink('~/Documents/Research/SCZ_SCR/group.differences.txt')
print('Group difference in CS+ CS- contrast no covariates')
print(m.contrast)
print('Group difference in CS+ CS- contrast age sex covariates')
print(m.contrast.cov)
print('Group difference in CS- no covariates')
print(m.Csminus)
print('Group difference in CS- age sex covariates')
print(m.Csminus.cov)
print('Group difference in CS+ no covariates')
print(m.Csplus)
print('Group difference in CS+ age sex covariates')
print(m.Csplus.cov)
sink()


### Explore symptom correlations in the SCZ group 
SCZdata <- subset(data, Group=='SCZ')
clinical <- c("PANSS.POS", "PANSS.NEG", "PANSS.GEN", "PANSS.TOT", "CPZ")
outcome <- c('contrast', 'Csplus', 'Csminus' )

results <- matrix(nrow=3, ncol=15)
r=1 
for (y in outcome){
  k=1
  for (c in clinical){
    f <- as.formula(paste(y, c, sep='~'))
    m <- lme(f, random =~ 1|STUDY, data=SCZdata)
    results[r,k]=fixef(m)[2]
    results[r,k+1]=sqrt(anova(m)[2,3]) * sign(fixef(m)[2])
    results[r,k+2]=anova(m)[2,4]
    k=k+3
    }
  r=r+1 
}
cnames <- paste(rep(clinical, each=3), c('b', 't', 'p'), sep='.')
colnames(results) <- cnames
rownames(results) <- outcome
write.csv( results, '~/Documents/Research/SCZ_SCR/clinical_correlations.csv')

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

## plot figure 1 & figure 2 ## 

# for plotting, calculate between-group t-tests & cohen d for CS+ vs CS- contrasts 
HOLT2009.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='H2009'))
HOLT2009.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='H2009'))
HOLT2012.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='H2012'))
HOLT2012.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='H2012'))
TUOMINENXXXX.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='TXXXX'))
TUOMINENXXXX.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='TXXXX'))
ROMANIUK2010.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='R2010'))
ROMANIUK2010.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='R2010'))

# helper df to control for axis, etc
df <- data.frame(sp = c(0.6, 1.1, 1.6, 0.05), 
                 pvals = c(round(HOLT2009.ttest$p.value,3), 
                           round(HOLT2012.ttest$p.value,3), 
                           round(TUOMINENXXXX.ttest$p.value,3),
                           round(ROMANIUK2010.ttest$p.value,3)),
                 dvals = c(round(HOLT2009.cohend$estimate,2), 
                           round(HOLT2012.cohend$estimate,2), 
                           round(TUOMINENXXXX.cohend$estimate,2),
                           round(ROMANIUK2010.cohend$estimate,2)), 
                 names = c("Holt et al., 2009", "Holt et al., 2012", 
                           "Tuominen et al., 2020","Romaniuk et al., 2010"),
                 STUDY = c("H2009", "H2012", "TXXXX","R2010"), 
                 ymin = c(-0.375, -0.4, -1.0, -0.04), 
                 ymax = c(0.75, 1.5 , 2.25, 0.065 ))

# function for graphs
create_fig2 <- dget('~/Documents/Research/SCZ_SCR/create_fig2.R')
  
p1 <- ggplotGrob(create_fig2(data, df, "H2009", FALSE))
p2 <- ggplotGrob(create_fig2(data, df, "R2010", FALSE))
p3 <- ggplotGrob(create_fig2(data, df, "H2012", FALSE))
p4 <- ggplotGrob(create_fig2(data, df, "TXXXX", FALSE))

# Set the widths
p1$widths <- p2$widths
p3$widths <- p2$widths
p4$widths <- p2$widths
p <- grid.arrange(p1, p2, p3, p4, nrow = 1)

# save file 
ggsave(file="~/Documents/Research/SCZ_SCR/figure2.png", plot=p, width=16, height=5, dpi=300, units='in') 

## plot the combined data for figure 1

# create adjusted values
newdata <- data
newdata$contrast <- returnAdj(data = data, measure = "contrast", covars = "STUDY")
newdata$ID <- paste('pseudoID', seq(1,nrow(newdata)), sep='')
newdata$SL <- 'Combined' 

# control axis, etc
label <- paste(paste('P-value = ', round(m.contrast[2,4],3), sep=''), paste("Cohen's d = ", combined.cohend, sep=''), sep='\n')
t <- data.frame(y= 1*1.3, x=1.5, label=label, Group='CTR')
yminmax <- c(-1.1, 2.1)
hh <- ((yminmax[2] - yminmax[1] ) * 0.82 ) +yminmax[1] 
ht <- ((yminmax[2]  - yminmax[1] ) * 0.92 ) +yminmax[1] 
h <- data.frame(a = c(1,1,1.5,2,2), 
                b = c(hh, hh*1.05, hh*1.05, hh*1.05, hh))

# plot
p <-ggplot(newdata, aes(x = Group, y = contrast, fill = Group)) + 
  theme(strip.text.x = element_text(size = 12)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_point(data=newdata, aes(x = Group, y = contrast), color="black", alpha =0.6,
             position = position_jitter(width = 0.10, height = 0.0), show.legend = FALSE) +
  scale_fill_manual(values = c("turquoise3",  "deeppink1")) +
  labs(y = "CS+ vs CS-" , x = "") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22)) + 
  geom_text(data= t, mapping = aes(x = x, y = ht, label = label), size=5) + 
  geom_line(data = h, aes(x = a, y = b), color ='black', inherit.aes = FALSE)+
  ylim(yminmax[1], yminmax[2])

# save plot 
jpeg('~/Documents/Research/SCZ_SCR/figure1.png', width = 5, height = 5, res = 300, units = 'in')
print(p)
dev.off()




