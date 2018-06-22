library(ggplot2)
library(data.table)

#Import data
setwd('C:/Mathis/SAFS/HEED')
heed <-read.csv('data/Higher Education Ecological Data (HEED)_19JUNE2018_text.csv')

#Dataframe of questions
qs <- t(heed[1:2,])
row.names(qs) <- colnames(heed)
heed <- heed[3:nrow(heed),]


#Keep responses that are at least 80% finished
heed$Progress <- as.numeric(as.character(heed$Progress))
heedsel <- heed[(heed$Finished==T | heed$Progress>80) & heed$Q1.2=='Yes' & #Consented to taking the survey
                  !(heed$Q2.1 == 'No' & heed$Q3.1 == 'No'),] #Are at least interest in teaching a class
nrow(heedsel[heedsel$Q2.1=='Yes',])
write.csv(heedsel, 'results/heedsel.csv')

#Check out time of survey completion
heedsel$StartDate <- as.POSIXct(heedsel$StartDate,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
min(heedsel$StartDate)
max(heedsel$StartDate)
heedsel$StartDate_simple <- as.Date(format(heedsel$StartDate, "%m/%d/%Y"), "%m/%d/%Y")
datecount <- setDT(heedsel)[,length(unique(ResponseId)), .(StartDate_simple)]

ggplot(datecount, aes(StartDate_simple, V1)) + 
  geom_bar(stat='identity') + 
  scale_x_date(date_breaks='1 week') + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

#Check out survey duration distribution
str(heedsel)
heedsel$Duration..in.seconds. <- as.numeric(as.character(heedsel$Duration..in.seconds.))
ggplot(heedsel, aes(x=Duration..in.seconds.)) + 
  geom_histogram() +
  scale_x_log10()

median(heedsel$Duration..in.seconds.)/60 #Median survey completion time

#Look at those surveys that took less than 5 minutes
short <- heedsel[heedsel$Duration..in.seconds.<300,]
heedsel <- heedsel[heedsel$ResponseId!='R_1P5lhMCVvn3syjv',]


