## Figures for the manuscript:
## "Acquisition of conditioned fear is impaired in schizophrenia – a pooled analysis of 4 Pavlovian fear conditioning studies"
##  by Lauri Tuominen, Liana Romaniuk, Mohammed R Milad, Donald C. Goff, Jeremy Hall, and Daphne Holt
# load libraries 
lapply(c('nlme', 'lmerTest', 'robustbase', 'ggplot2','gtable', "MASS", "gridExtra"), require, character.only = TRUE)
source('~/Documents/Research/SCZ_SCR/returnAdj.R')

# load data
data <- read.csv('~/Documents/Research/SCZ_SCR/CombinedSCR_all.csv')
data <- subset(data, Exclusion == 'Incl')

# calculate a contrast between CS+ and CS-
data$contrast <- data$Csplus-data$Csminus

# remove an outlier from the data 
data <- subset(data, ID!='USRE')

##############################
## plot for figure 1        ##
##############################

# get get stats for the plots  
m.contrast.cov <- summary(lme(contrast ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))
m.Csminus.cov <- summary(lme(Csminus ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))
m.Csplus.cov <- summary(lme(Csplus ~ Group + gender + Age.Y, random =~ 1|STUDY, data=data))

# create adjusted values
newdata <- data
newdata$contrast <- returnAdj(data = data, measure = "contrast", covars = c("STUDY", "Age.Y", "gender" ))
newdata$Csminus <- returnAdj(data = data, measure = "Csminus", covars = c("STUDY", "Age.Y", "gender" ))
newdata$Csplus <- returnAdj(data = data, measure = "Csplus", covars = c("STUDY", "Age.Y", "gender" ))
newdata$ID <- paste('pseudoID', seq(1,nrow(newdata)), sep='')

# control axis, etc
fig_annot <- data.frame(contrast = c(round(m.contrast.cov$tTable[2,5],3), round(m.contrast.cov$tTable[2,1],3), 'CS+ vs CS-'),
                        Csminus = c(round(m.Csminus.cov$tTable[2,5],3), round(m.Csminus.cov$tTable[2,1],3), 'CS-'),
                        Csplus = c(round(m.Csplus.cov$tTable[2,5],3), round(m.Csplus.cov$tTable[2,1],3), 'CS+'))
                        
yminmax <- c(-1.1, 2.1)
dy <- yminmax[2] - yminmax[1]
hh <- (dy * 0.82 ) +yminmax[1] 
ht <- (dy * 0.96 ) +yminmax[1] 
hv <- (dy * 0.88 ) +yminmax[1] 

h <- data.frame(a = c(1,1,1.5,2,2), 
                b = c(hh, hh*1.05, hh*1.05, hh*1.05, hh))

outcome <- c('contrast', 'Csminus', 'Csplus')
for (c in outcome){
t <- data.frame(y=1.3, x=1.5, label1=paste('P-value =', fig_annot[[c]][1], sep=' '),label2=paste('beta==',fig_annot[[c]][2], sep=' '), Group='CTR')
p <- ggplot(newdata, aes(x = Group, y = newdata[[c]], fill = Group)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_point(data=newdata, aes(x = Group, y = newdata[[c]]), color="black", size=2, alpha =0.6,
             position = position_jitter(width = 0.15, height = 0.0), show.legend = FALSE) +
  scale_fill_manual(values = c("turquoise3",  "deeppink1")) +
  labs(y = 'SCR (a.u.)' , x = "", title = fig_annot[[c]][3]) +
  scale_y_continuous(breaks = c(-1, 0, 1))  +
  ylim(-1.1, 1.6) + 
  theme(plot.title = element_text(size = 28, hjust = 0.5),
    axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24))  
 
  #geom_text(data= t, mapping = aes(x = x, y = ht, label = label1), size=6) + 
  #geom_text(data= t, mapping = aes(x = x, y = hv, label = label2), parse=T, size=6) + 
  #geom_line(data = h, aes(x = a, y = b), color ='black', inherit.aes = FALSE)+
  

  # save plot 
  jpeg(paste('~/Documents/Research/SCZ_SCR/fig1_no_stats_', c, '.png', sep=''), width = 4, height = 6, res = 300, units = 'in')
  print(p)
  dev.off()
}

##############################
## plot for figure 2        ##
##############################

# calculate between-group t-tests & cohen d for CS+ vs CS- contrasts 
HOLT2009.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='H2009'))
HOLT2009.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='H2009'))
HOLT2012.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='H2012'))
HOLT2012.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='H2012'))
TUOMINEN2021.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='T2021'))
TUOMINEN2021.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='T2021'))
ROMANIUK2010.ttest <- t.test(contrast~Group, data = subset(data, STUDY=='R2010'))
ROMANIUK2010.cohend <- cohen.d(contrast~Group, data = subset(data, STUDY=='R2010'))

# helper df to control for axis, etc
df <- data.frame(sp = c(0.6, 1.1, 1.6, 0.05), 
                 pvals = c(round(HOLT2009.ttest$p.value,3), 
                           round(HOLT2012.ttest$p.value,3), 
                           round(TUOMINEN2021.ttest$p.value,3),
                           round(ROMANIUK2010.ttest$p.value,3)),
                 dvals = c(round(HOLT2009.cohend$estimate,2), 
                           round(HOLT2012.cohend$estimate,2), 
                           round(TUOMINEN2021.cohend$estimate,2),
                           round(ROMANIUK2010.cohend$estimate,2)), 
                 names = c("Holt et al., 2009", "Holt et al., 2012", 
                           "Tuominen et al., 2021","Romaniuk et al., 2010"),
                 STUDY = c("H2009", "H2012", "T2021","R2010"), 
                 ymin = c(-0.375, -0.4, -1.0, -0.04), 
                 ymax = c(0.75, 1.5 , 2.25, 0.065 ))

# function for graphs
create_fig2 <- dget('~/Documents/Research/SCZ_SCR/create_fig2.R')

p1 <- ggplotGrob(create_fig2(data, df, "H2009", FALSE))
p2 <- ggplotGrob(create_fig2(data, df, "R2010", FALSE))
p3 <- ggplotGrob(create_fig2(data, df, "H2012", FALSE))
p4 <- ggplotGrob(create_fig2(data, df, "T2021", FALSE))

# Set the widths
p1$widths <- p2$widths
p3$widths <- p2$widths
p4$widths <- p2$widths
p <- grid.arrange(p1, p2, p3, p4, nrow = 1)

# save file 
ggsave(file="~/Documents/Research/SCZ_SCR/figure2.png", plot=p, width=16, height=5, dpi=300, units='in') 


##############################
## plot for figure 3        ##
##############################

PDIdata <- subset(data, Group=='SCZ')
PDIdata <- PDIdata[complete.cases(PDIdata$PDI.total), ]
mrob.PDI <- summary(lmrob(Csminus ~ PDI.total + STUDY +Age.Y+gender, data=PDIdata))

PDInoout <- PDIdata[(PDIdata$PDI.total < 30) , ]
mrob.PDI2 <- summary(lmrob(Csminus ~ PDI.total + STUDY +Age.Y+gender, data=PDInoout))

PDIdata$adj <- returnAdj(data = PDIdata, measure = "Csminus", covars = c("STUDY", "Age.Y", "gender" ))

p <- ggplot(data=PDIdata, aes(x=PDI.total, y=adj)) + 
  geom_point(color="deeppink1", size=3, shape=21) +
  labs(y ='SCR', x = "PDI score", title = 'CS-') +
  theme(plot.title = element_text(size=26, hjust = 0.5),
        axis.text=element_text(size=22),
        axis.title=element_text(size=22),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22)) +
  scale_x_continuous(breaks = c(0,20,40)) +
  scale_y_continuous(breaks = c(-0.5,0,0.5)) +
  stat_smooth(method=function(formula,data,weights=weight) rlm(formula,
                                                               data,
                                                               weights=weight,
                                                               method="MM"),
              fullrange=TRUE) +
  geom_text(mapping = aes(x = 30, y = 0.5, label = "Beta = 0.007 \n P-value = 0.033",fontface='plain', family = "Arial"), size=5)

ggsave(file="~/Documents/Research/SCZ_SCR/CSminus_delusions.png", plot=p, width=5, height=6, dpi=300, units='in') 

