library(ggplot2)
library(data.table)

#Import data
setwd('C:/Mathis/SAFS/HEED') ##UPDATE##
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
heedsel <- read.csv('results/heedsel.csv') #Re-read to get correct column types
str(heedsel)
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
ggplot(heedsel, aes(x=Duration..in.seconds./60)) + 
  geom_histogram() 

median(heedsel$Duration..in.seconds.)/60 #Median survey completion time

#Look at those surveys that took less than 5 minutes
short <- heedsel[heedsel$Duration..in.seconds.<300,]
heedsel <- as.data.frame(heedsel[heedsel$ResponseId!='R_OKnjv2yeoOI8wj7',])
#Look for duplicated ID addresses and surveys taken in the same location
IPduplis <- heedsel[heedsel$IPAddress==as.character(heedsel[duplicated(heedsel$IPAddress),'IPAddress']),] 
#IP duplicate: answered for existing + prospective class
LLduplis <-  heedsel[which(duplicated(heedsel[,c("LocationLatitude","LocationLongitude")])),c("LocationLatitude","LocationLongitude")]
LLduplis <- heedsel[paste(heedsel$LocationLatitude, heedsel$LocationLongitude) %in% paste(LLduplis[,1], LLduplis[,2]),]
#Location duplicate: all IP addresses associated with a given university are assigned the same geographic coordinates (different people).

#Subset respondents according to main branch (teaching a class versus interested in teaching a class)
heedteach <- heedsel[heedsel$Q2.1=='Yes',c(1:23,75:(ncol(heedsel)))]
heednoteach <- heedsel[heedsel$Q2.1=='No',c(1:19,24:74)]
write.csv(heedteach, 'results/heedsel_teach.csv')
write.csv(heednoteach, 'results/heedsel_noteach.csv')

#_________________________________________________________________________
# QA/QC DATA FOR THOSE SURVEY RESPONDENTS THAT EACH A FIELD-BASED CLASS #
#_________________________________________________________________________
write.csv(heedteach, 'results/heedteach.csv')
heedteach$flag <- 0

#Q2.2 How many classes do you teach? - check whether 1 or +5 by going to Data & Analysis/Data/Actions/View response
ggplot(heedteach, aes(Q2.2_1)) + geom_histogram()
heedteach[heedteach$Q2.2_1==5 & !is.na(heedteach$Q2.2_1),'flag'] <- heedteach[heedteach$Q2.2_1==5 & !is.na(heedteach$Q2.2_1),'flag']+1
nrow(heedteach[is.na(heedteach$Q2.2_1),])

#Q2.4
ggplot(heedteach, aes(Q2.4)) + geom_histogram(stat="count")


#Q2.5_1
ggplot(heedteach, aes(Q2.5_1)) + geom_histogram(stat="count")
table(heedteach$Q2.5_1)


