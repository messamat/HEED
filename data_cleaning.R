library(ggplot2)
library(data.table)
library(stringr)

#Import data
setwd('C:/Mathis/SAFS/HEED') ##UPDATE##
heed <-read.csv('data/Higher Education Ecological Data (HEED)_October 18, 2018_10.34.csv')

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

#Went through every response manually to check whether toggle bars with -99 were untouched or checked the 'NA' button
heededit <- read.csv('data/heedsel_edit_20181018.csv')


#Check out date/time of survey completion
str(heededit)
heededit$StartDate <- as.POSIXct(heededit$StartDate,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
min(heededit$StartDate)
max(heededit$StartDate)
heededit$StartDate_simple <- as.Date(format(heededit$StartDate, "%m/%d/%Y"), "%m/%d/%Y")
datecount <- setDT(heededit)[,length(unique(ResponseId)), .(StartDate_simple)]

ggplot(datecount, aes(StartDate_simple, V1)) + 
  geom_bar(stat='identity') + 
  scale_x_date(date_breaks='1 week') + 
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

#Check out survey duration distribution
str(heededit)
heededit$Duration..in.seconds. <- as.numeric(as.character(heededit$Duration..in.seconds.))
ggplot(heededit, aes(x=Duration..in.seconds./60)) + 
  geom_histogram() 

median(heededit$Duration..in.seconds.)/60 #Median survey completion time

#Look at those surveys that took less than 5 minutes
short <- heededit[heededit$Duration..in.seconds.<300,]
heededit <- as.data.frame(heededit[heededit$ResponseId!='R_OKnjv2yeoOI8wj7',]) #did not honestly fill the survey
#Look for duplicated ID addresses and surveys taken in the same location
IPduplis <- heededit[heededit$IPAddress==as.character(heededit[duplicated(heededit$IPAddress),'IPAddress']),] 
#IP duplicate: answered for existing + prospective class
LLduplis <-  heededit[which(duplicated(heededit[,c("LocationLatitude","LocationLongitude")])),c("LocationLatitude","LocationLongitude")]
LLduplis <- heededit[paste(heededit$LocationLatitude, heededit$LocationLongitude) %in% paste(LLduplis[,1], LLduplis[,2]),]
#Location duplicate: all IP addresses associated with a given university are assigned the same geographic coordinates (different people).

#Subset respondents according to main branch (teaching a class versus interested in teaching a class)
heedteach <- setDT(heededit[heededit$Q2.1=='Yes',c(1:20,83:(ncol(heededit)))])
heednoteach <- setDT(heededit[heededit$Q2.1=='No',c(1:15,21:82)])
write.csv(heedteach, 'results/heededit_teach.csv')
write.csv(heednoteach, 'results/heededit_noteach.csv')

#_________________________________________________________________________
# QA/QC DATA FOR THOSE SURVEY RESPONDENTS THAT EACH A FIELD-BASED CLASS #
#_________________________________________________________________________
heedteach$flag <- 0

################ Q2.2 How many classes do you teach? ################
heedteach[is.na(Q2.2_1), Q2.2_1 := 6] 
ggplot(heedteach, aes(Q2.2_1)) + 
  geom_histogram() + 
  scale_x_continuous(breaks=seq(1,6), labels=c(seq(1,5),'>5'), expand=c(0,0), 
                     name='Number of classes taught') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Teaching Q2.2: How many classes do you teach? Number of respondents: ', 
                heedteach[!is.na(Q2.2_1) & Q2.2_1 != -99, .N], '/', heedteach[, .N])) + 
  theme_classic()
heedteach[Q2.2_1>5, flag := flag+1] 
nrow(heedteach[is.na(Q2.2_1),])

################ Q2.4 - Are/were the ecological data collected during the field excursion(s) retained after the class' end date?################
ggplot(heedteach, aes(Q2.4)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Teaching Q2.4: Were the ecological data kept? Number of respondents: ', 
                heedteach[!is.na(Q2.4) & Q2.4 != -99, .N], '/', heedteach[, .N])) + 
  theme_classic()

################ Q2.5_1 and Q2.6_1 - In what year did data collection start in the context of this class?  When was the last year of data collection, if applicable? ################
heedteach[is.na(Q2.6_1), Q2.6_1 := 2019]
heedteach[,ResponseId := factor(ResponseId, levels = unique(heedteach$ResponseId[order(heedteach$Q2.6_1, heedteach$Q2.5_1)]))]

heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, dataduration := Q2.6_1-Q2.5_1]
durahist <- ggplot(heedteach, aes(x=dataduration)) + 
  geom_histogram(binwidth=1) +
  scale_x_continuous(expand=c(0,0), name='Number of years of data') + 
  scale_y_continuous(expand=c(0,0), name='') + 
  geom_vline(xintercept=heedteach[,mean(dataduration, na.rm=T)]) + 
  geom_text(aes(x=heedteach[,mean(dataduration, na.rm=T)]-1, y=15), 
            label=paste0('Mean:',heedteach[,mean(dataduration, na.rm=T)], ' years'),angle=90) +
  geom_vline(xintercept=heedteach[,median(dataduration, na.rm=T)]) + 
  geom_text(aes(x=heedteach[,median(dataduration, na.rm=T)]-1, y=15), 
            label=paste0('Median:',heedteach[,median(dataduration, na.rm=T)], ' years'),angle=90) +
  theme_classic()

ggplot(heedteach[Q2.5_1 != -99 & Q2.6_1 != -99,]) + 
  geom_segment(aes(x=Q2.5_1, xend=Q2.6_1, y=ResponseId, yend=ResponseId), size=1.2) +
  scale_x_continuous(expand=c(0,0), name='Start and end years of data collection') + 
  scale_y_discrete(expand=c(0,0), name='') + 
  ggtitle(paste('Teaching Q2.5 and 2.6: In what year did data collection start and end. Number of respondents:', 
                heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, .N], '/', heedteach[, .N])) + 
  annotation_custom(ggplotGrob(durahist), 
                    xmin=1960, xmax=1990, 
                    ymin=quantile(heedteach[,as.numeric(ResponseId)], .50), ymax=quantile(heedteach[,as.numeric(ResponseId)], 0.95)) + 
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color='lightgrey'),
        panel.grid.minor.x = element_line(color='lightgrey'))

table(heedteach$Q2.5_1)
table(heedteach$Q2.6_1)
check <- heedteach[heedteach$Q2.5_1 < 1970 & !is.na(heedteach$Q2.5_1),] #NTR

################ Q8.2 - For what level of higher education is/was the class offered? ########
check <- heedteach[!is.na(Q8.2_6_TEXT) & Q8.2_6_TEXT != "" & Q8.2_6_TEXT != '-99',] 
heedteach[Q8.2_6_TEXT %in% c('Master','M.Sc  students'), `:=`(Q8.2_6 = '-99', Q8.2_6_TEXT = '-99', 
                                                              Q8.2_5 = 'Graduate')]
heedteach[Q8.2_6_TEXT == 'Third and fourth year', `:=`(Q8.2_6 = '-99', Q8.2_6_TEXT = '-99', 
                                                       Q8.2_4 = 'Fourth Year -  undergraduate')]
heedteach[Q8.2_6_TEXT == 'ND I and NDII', `:=`(Q8.2_6 = '-99', Q8.2_6_TEXT = '-99', 
                                               Q8.2_1 = 'First year - undergraduate', 
                                               Q8.2_2 = 'Second year -  undergraduate') ]
heedteach[Q8.2_6_TEXT %in% c('bank employees','Kindergarten'), flag := flag+1] 

q8_2cols <- names(heedteach)[grep('Q8.2',names(heedteach))]
heedteach[, (q8_2cols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = q8_2cols]
q8_2_melt <- melt(heedteach, id.vars='ResponseId', measure.vars=q8_2cols)

flevels <- data.frame(variable=q8_2cols, levels=grep('Q8.2',names(heedteach))-min(grep('Q8.2',names(heedteach)))+1)
q8_2_melt <- q8_2_melt[flevels, on='variable']
q8_2_melt[,value := factor(value, levels = unique(q8_2_melt$value[order(q8_2_melt$levels)]))]
ggplot(q8_2_melt[!is.na(value) & value!='Other (please specify)'], aes(x=value)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='Level') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('For what level of higher education is/was the class offered? Number of respondents: ', 
                length(q8_2_melt[!is.na(value), unique(ResponseId)]), '/', heedteach[, .N])) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=10, vjust=0.5))


heedteach[!(apply(is.na(heedteach[,q8_2cols[1:4],with=FALSE]), 1,  all)), courselevel := 'Undergraduate']
heedteach[!is.na(Q8.2_5) & is.na(courselevel), courselevel := 'Graduate']
heedteach[!is.na(Q8.2_5) & courselevel == 'Undergraduate', courselevel := 'Mixed']
ggplot(heedteach[!is.na(courselevel)], aes(x=courselevel)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='Level') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('For what level of higher education is/was the class offered? Number of respondents: ', 
                heedteach[!is.na(courselevel), .N], '/', heedteach[, .N])) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=10, vjust=0.5))

################ Q8.3 - How often was the class offered? ################
heedteach[,Q8.3 := factor(Q8.3, levels = unique(heedteach$Q8.3)[c(2,1,4,5,3)])]

ggplot(heedteach, aes(Q8.3)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Teaching Q8.3: How often is/was the class offered? Number of respondents: ', 
                heedteach[!is.na(Q8.3) & Q8.3 != -99, .N], '/', heedteach[, .N])) + 
  theme_classic()

################ Q8.4 - Including yourself, how many instructors (and teaching assistants) are/were involved in implementing the field excursion(s) in a typical year? ################
check <- heedteach[Q8.4_1 %in% c(0,10, NA),]

heedteach[is.na(Q8.4_1), Q8.4_1 := 11]
heedteach[Q8.4_1 %in% c(0,11), flag:=flag+1]
heedteach[Q8.4_1 == 0, Q8.4_1 := 1] 
valrange <- do.call(seq, as.list(range(heedteach$Q8.4_1)))
ggplot(heedteach, aes(Q8.4_1)) + 
  geom_histogram() + 
  scale_x_continuous(breaks=valrange, labels=c(valrange[-length(valrange)],paste0('>',valrange[length(valrange)-1])),
                     expand=c(0,0), 
                     name='Number of classes taught') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Teaching Q8.4: How many instructors are/were involved? Number of respondents: ', 
                heedteach[!is.na(Q8.4_1) & Q8.4_1 != -99, .N], '/', heedteach[, .N])) + 
  theme_classic()

################ Q9.2 - What is/was the total number of students participating in the field excursions in a typical year? ################
check <- heedteach[Q8.4_1 %in% c(0,10, NA),]

heedteach[is.na(Q8.4_1), Q8.4_1 := 11]
heedteach[Q8.4_1 %in% c(0,11), flag:=flag+1]
heedteach[Q8.4_1 == 0, Q8.4_1 := 1] 
valrange <- do.call(seq, as.list(range(heedteach$Q8.4_1)))
ggplot(heedteach, aes(Q8.4_1)) + 
  geom_histogram() + 
  scale_x_continuous(breaks=valrange, labels=c(valrange[-length(valrange)],paste0('>',valrange[length(valrange)-1])),
                     expand=c(0,0), 
                     name='Number of classes taught') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Teaching Q8.4: How many instructors are/were involved? Number of respondents: ', 
                heedteach[!is.na(Q8.4_1) & Q8.4_1 != -99, .N], '/', heedteach[, .N])) + 
  theme_classic()


#q15.5 NOT DISPLAYED TO THOSE WHO PRESSED > 10 PUBLICATIONS


#Check ratio of instructor to students