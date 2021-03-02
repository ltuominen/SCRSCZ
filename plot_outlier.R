# plot outlier in Holt et al 2009 
outlier <- subset(data, STUDY=='H2009' & Group =='SCZ')
outlier$ID <- factor(outlier$ID)
label=ifelse(outlier$contrast>4*IQR(outlier$contrast), TRUE, FALSE)
criterion <- 4*IQR(outlier$contrast)
IDnames <- as.character(outlier$ID)
IDnames <- ifelse(label==TRUE, IDnames, '')

# plot 
jpeg('outlierHolt2009.tiff', width = 3, height = 3, res = 300, units = 'in')
ggplot(outlier, aes(x=Group, y=contrast, label=IDnames)) + 
  geom_text(hjust = 0, nudge_x = 0.05) +
  geom_boxplot()  +
  labs(y= 'CS+ vs CS-', title = 'Outlier in Holt et al., 2009 ')+
  theme(plot.title = element_text(hjust = 0.5, size=16))
dev.off()