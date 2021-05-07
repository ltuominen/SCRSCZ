# create table one for SCZ SCR study

library('tableone')

# load data
data <- read.csv('~/Documents/Research/SCZ_SCR/CombinedSCR_all.csv')
data <- subset(data,  Exclusion == 'Incl')
data <- subset(data, ID!='USRE')

data$Sex <- data$gender
vars <- c("Age.Y", "Sex", "PANSS.POS","PANSS.NEG", "PANSS.TOT","CPZ")

t1 <- print(CreateTableOne(vars=vars,  strata = c('Group','STUDY'), data=data , test=FALSE) )
t2 <- print(CreateTableOne(vars=vars,  strata = c('Group'), data=data , test=FALSE) )
t3 <- cbind(t1,t2)
write.csv(t3, '~/Documents/Research/SCZ_SCR/tableone.csv')

