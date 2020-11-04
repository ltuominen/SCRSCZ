function(data, df, study, legend){
  d <- subset(data, data$STUDY==study)
  fname <- paste(study, 'text.png',sep='')
  helpvals <- subset(df,df$STUDY==study)
  hh <- ((helpvals$ymax - helpvals$ymin) * 0.82 ) +helpvals$ymin
  ht <- ((helpvals$ymax - helpvals$ymin) * 0.92 ) +helpvals$ymin
  h <- data.frame(a = c(1,1,1.5,2,2), 
                  b = c(hh, hh*1.05, hh*1.05, hh*1.05, hh))
  label <- paste(paste('P-value = ', helpvals$pvals, sep=''), paste("Cohen's d = ", helpvals$dvals, sep=''), sep='\n')
  t <- data.frame(y= helpvals$sp*1.2, x=1.5, label=label, Group='CTR')
  r <- range(d$contrast)
  
  p <- ggplot(d, aes(x = Group, y = contrast, fill = Group)) + 
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_point(data=d, aes(x = Group, y = contrast), color="black", alpha = 0.7,
               position = position_jitter(width = 0.10, height = 0.0), show.legend = FALSE) +
    scale_fill_manual(values = c("turquoise3",  "deeppink1")) +
    labs(y = "CS+ vs CS-" , x = "", title = helpvals$names) +
    geom_line(data = h, aes(x = a, y =  b), color ='black', inherit.aes = FALSE) +
    geom_text(data= t, mapping = aes(x = x, y =  ht, label = label), lineheight = 1, size=5) +
    ylim(helpvals$ymin, helpvals$ymax) +
    theme(plot.title = element_text(hjust = 0.5, size=18), 
          axis.text=element_text(size=22),
          axis.title=element_text(size=22),
          legend.text = element_text(size=18),
          legend.title = element_text(size=22))
  
  if (legend==FALSE) {
    p <- p + theme(legend.position = "none") 
  }
  return(p)
}