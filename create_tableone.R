# create table one for SCZ SCR study

library('tableone')

# load data
data <- read.csv('~/Documents/Research/SCZ_SCR/CombinedSCR_new.csv')
data <- subset(data,  Exclusion == 'Incl')
data <- subset(data, ID!='USRE')

data$Sex <- data$gender
vars <- c("Age.Y", "Sex", "PANSS.POS","PANSS.NEG","CPZ")

t1 <- CreateTableOne(vars=vars,  strata = c('Group','STUDY'), data=data , test=FALSE) 
t1 <- print(t1)
write.csv(t1, '~/Documents/Research/SCZ_SCR/tableone.csv')

t2 <- CreateTableOne(vars=vars,  strata = c('Group'), data=data , test=FALSE) 
t2 <- print(t2)
write.csv(t2, '~/Documents/Research/SCZ_SCR/tableone_extended.csv')