#+ fig.width=10, fig.height=6, dpi=300, out.width="1920px",out.height="1080px"
############# Import libraries ##################
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(egg)
library(RColorBrewer)
library(data.table)
library(arsenal)
library(stringr)
library(sp)
library(rgdal)
library(leaflet)
library(knitr)
library(kableExtra)
library(ggrepel)
library(hrbrthemes)
library(extrafont)
#extrafont::font_import()
extrafont::loadfonts()

############# Import and format data ##################
setwd(gsub('\\/src.*', '', getwd())) ##UPDATE##
heed <-read.csv('data/Higher Education Ecological Data (HEED)_October 18, 2018_text.csv')
heednum <- read.csv('data/Higher Education Ecological Data (HEED)_October 18, 2018_numeric.csv')

#Dataframe of questions
qs <- as.data.frame(t(heed[1:2,]))
row.names(qs) <- colnames(heed)
heed <- heed[3:nrow(heed),]
heednum <- heednum[3:nrow(heednum),]

#Keep responses that are at least 80% finished
heed$Progress <- as.numeric(as.character(heed$Progress))
heedsel <- heed[(heed$Finished==T | heed$Progress>80) & heed$Q1.2=='Yes' & #Consented to taking the survey
                  !(heed$Q2.1 == 'No' & heed$Q3.1 == 'No'),] #Are at least interest in teaching a class
nrow(heedsel[heedsel$Q2.1=='Yes',])
write.csv(heedsel, 'results/heedsel.csv', row.names = F)

#Went through every response manually to check whether toggle bars with -99 were untouched or checked the 'NA' button
#Left cases where didn't touch the toggle bar to -99, otherwise, changed to NA (-> heedsel_edit_20180118.csv)
#Then, JD went through to clean "Other" responses for the questions that we plan to report in the manuscript 
heededit <- read.csv('data/heedsel_edit_20181219.csv')

#TO DO: integrate changes to table in the code for those that weren't already included
#library(arsenal)
#heededit2 <- read.csv('data/heedsel_edit_20181018.csv')
#cmp <- compare(heededit, heededit2)
#diffs(cmp, by.var = TRUE)
#diffs(cmp, vars = c("ps", "ast"))


#Check number of respondents and countries
length(unique(heededit$ResponseId)) #of respondents
length(heededit[!(heededit$Q20.2_1 %in% c('-99','')), 'Q20.2_1']) + 
                  length(heededit[!(heededit$Q7.2_1 %in% c('-99','')), 'Q7.2_1']) # of respondents that entered their country
length(unique(c(as.character(heededit[!(heededit$Q20.2_1 %in% c('-99','')), 'Q20.2_1']), 
                as.character(heededit[!(heededit$Q7.2_1 %in% c('-99','')), 'Q7.2_1'])))) #number of unique countries


unis_all <- c(as.character(heededit[!(heededit$Q20.1_1 %in% c('-99','')), 'Q20.1_1']), 
              as.character(heededit[!(heededit$Q7.1_1 %in% c('-99','')), 'Q7.1_1']))
length(unis_all) # of respondents that entere their university
unis_unique <- sort(unique(unis_all)) #Unique universities
length(unis)-5 #Number of unique universities (University of Zagreb, Charles University, Eastern Washington, National University of Science and Technology, University of SOuth Bohemia)


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
write.csv(heedteach, 'results/heededit_teach.csv', row.names = F)
write.csv(heednoteach, 'results/heededit_noteach.csv', row.names = F)

median(heedteach$Duration..in.seconds.)/60 #Median survey completion time
median(heednoteach$Duration..in.seconds.)/60 #Median survey completion time


#subset rows and columns in numeric results to match the text version (heedteach and heednoteach)
heednumteach <- setDT(heednum)[ResponseId %in% heedteach$ResponseId,
                               colnames(heednum) %in% colnames(heedteach), with=FALSE] 
heednumnoteach <- setDT(heednum)[ResponseId %in% heednoteach$ResponseId,
                                 colnames(heednum) %in% colnames(heednoteach), with=FALSE] 




#_________________________________________________________________________####
# QA/QC AND PLOT DATA 
# For those survey respondents that teach a field-based class 
#________________________________________________________________________####
heedteach$flag <- 0

################################## Plot functions ###############################################
commapos <- function(x, ...) {
  format(abs(x), big.mark = ",", trim = TRUE,
         scientific = FALSE, ...)
}

lastel <- function(x) x[[length(x)]]

histoplot <- function(data, col, unit, binw=1, checkNA=TRUE, qstext=qs) {
  setDT(data)
  meacol <- data[,mean(get(col), na.rm=T)]
  medcol <- data[,median(get(col), na.rm=T)]
  p <- ggplot(data, aes_string(x=col)) + 
    geom_histogram(binwidth=binw) +
    scale_x_continuous(expand=c(0,0), name=paste0('Number of ',unit)) + 
    scale_y_continuous(expand=c(0,0)) + 
    geom_vline(xintercept=meacol) + 
    annotate("text", x=meacol-binw/5, y=Inf, hjust=1.2, angle=90,
             label = paste('Mean:',round(meacol,1), unit)) +
    geom_vline(xintercept=medcol) + 
    annotate("text", x=medcol-binw/5, y=Inf, hjust=1.2,  angle=90,
             label = paste('Median:',round(medcol,1), unit)) +
    theme_classic()
  if (checkNA) {
    maxcol <- max(data[,get(col)], na.rm=T)
    valrange <- seq(min(data[,get(col)], na.rm=T),maxcol, by=binw)
    p <- p + scale_x_continuous(breaks=c(valrange[-length(valrange)], maxcol), 
                                labels=c(valrange[-length(valrange)],paste0('>',maxcol-1)),
                                expand=c(0,0), name=paste0('Number of ',unit)) +
      theme(axis.text.x = element_text(angle=90))
  }
  
  try({
    title <- tstrsplit(qstext[rownames(qstext) == col, 1], '?', fixed=T)[[1]]
    p <- p + ggtitle(paste(title, '? Number of respondents:',
                           data[!is.na(get(col)) & get(col)!=-99, .N], '/', data[, .N]))
  }, silent = T)
  
  return(p)
}

singleAplot <- function(data, col, unit, binw=1, checkNA=TRUE, qstext=qs) {
  data <- setDT(data)
  title <- tstrsplit(qstext[rownames(qstext) == col, 1], '?', fixed=T)[[1]]
  p <- ggplot(data[!(get(col) %in% c('-99',-99,'')),], aes_string(col)) + 
    geom_histogram(stat="count") + 
    scale_x_discrete(expand=c(0,0), name='') + 
    scale_y_continuous(expand=c(0,0)) + 
    ggtitle(paste(title, '? Number of respondents: ', 
                  data[!is.na(get(col)) & !(get(col)%in% c(-99,'-99','')), .N], '/', heedteach[, .N])) + 
    theme_classic()
  return(p)
}

multiformat <- function(data, pattern) {
  colind <- grep(pattern, names(data))
  qcols <- names(data)[colind]
  data[, (qcols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = qcols]
  qmelt <- melt(data, id.vars='ResponseId', measure.vars=qcols)
  flevels <- data.frame(variable=qcols, levels=colind-min(colind)+1)
  qmelt <- qmelt[flevels, on='variable']
  return(qmelt[,value := factor(value, levels = unique(qmelt$value[order(qmelt$levels)]))])
}

multiAhisto <- function(data, pattern, xaxis, qstext=qs) {
  qmelt <- multiformat(data, pattern)
  title <- tstrsplit(qstext[rownames(qstext) == qmelt$variable[1], 1], '?', fixed=T)[[1]]
  p <- ggplot(qmelt[!is.na(value) & !(value %in% c('Other (please specify)','','-99')),], aes(x=value)) + 
    geom_histogram(stat="count") + 
    scale_x_discrete(expand=c(0,0), name=xaxis) + 
    scale_y_continuous(expand=c(0,0)) + 
    ggtitle(paste(title, '? Number of respondents: ', 
                  length(qmelt[!is.na(value) & !(value %in% c('','-99')), unique(ResponseId)]), '/', data[, .N])) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle=10, vjust=0.5))
  return(p)
}

likertformat <- function(datatext, datanum, pattern, diverging=FALSE, qstext=qs, qsplit = '?') {
  datnumformat <- multiformat(datanum, pattern)
  if (diverging) {
    midlikert <- median(unique(as.numeric(as.character(datnumformat$value))), na.rm=T)
    datnumformat$value <- as.numeric(as.character(datnumformat$value))-midlikert
  } else{
    datnumformat$value <- as.numeric(as.character(datnumformat$value))-1
  }
  dattextformat <- multiformat(datatext, pattern)
  datjoin <- datnumformat[dattextformat, on=c('ResponseId', 'variable')]
  rowind <- grep(pattern, rownames(qstext))
  qslikert <- data.frame(choices = tstrsplit(qstext[rowind, '1'], qsplit, fixed=T)[[2]])
  qslikert$levels <- as.numeric(rownames(qslikert))
  datjoinq <- datjoin[qslikert, on='levels']
  datjoinq[!is.na(value), `:=`(varmean=mean(value), N=.N), by = variable]
  return(datjoinq[, `:=`(variable=factor(variable,levels= unique(datjoinq$variable[order(-datjoinq$varmean)])),
                         choices= factor(choices, levels=unique(datjoinq$choices[order(-datjoinq$varmean)])))]) 
}

likertboxplot <- function(dataformat, pattern, qstext=qs) {
  title <- tstrsplit(qstext[rownames(qstext) == dataformat$variable[1], 1], '?', fixed=T)[[1]]
  p <- ggplot(dataformat, aes(x=variable, y=value)) + 
    geom_boxplot(draw_quantiles = c(0.25, 0.5, 0.75)) + 
    geom_point(aes(y=varmean), color='red', size=3, shape=18) + 
    geom_text(aes(y=varmean-0.1, label = paste0('Mean:',round(varmean,1), ' (n=', N,')'))) +
    scale_x_discrete(labels = str_wrap(levels(dataformat$choices), width=10))+
    scale_y_continuous(name= 'Response', expand=c(0,0), breaks=unique(dataformat$value), 
                       labels=paste0(unique(dataformat$i.value), ' (', unique(dataformat$value),')')) +
    theme_classic() + 
    theme(axis.title.x = element_blank()) +
    ggtitle(paste0(title,'?'))
  print(p)
}

likertstackedbar <- function(dataformat, diverging=FALSE, qstext=qs) {
  title <- tstrsplit(qstext[rownames(qstext) == dataformat$variable[1], 1], '?', fixed=T)[[1]]
  
  dataformat_summary <- dataformat[!is.na(value),{
    tot = .N
    .SD[,.(frac=.N/tot),by=value]
  },by=variable]
  dataformat_summaryattri <- dataformat_summary[unique(dataformat[,.(variable, value, i.value, choices, varmean, N)]),
                                                on= c('variable','value'), nomatch=0]
  
  if (diverging) {
    dataformat_summaryattri[, frac2 := ifelse(value <0, -frac, frac)]
    up <- dataformat_summaryattri[frac2 >= 0,]
    down <- dataformat_summaryattri[frac2 < 0,]
    
    p <- ggplot(dataformat_summaryattri[order(dataformat_summaryattri$value, dataformat_summaryattri$variable),]) + 
      geom_bar(data = up,aes(x = variable,y = frac2,fill = factor(value)),
               stat = "identity", position = position_stack(reverse = TRUE)) + 
      geom_bar(data = down,aes(x = variable,y = frac2,fill = factor(value)),
               stat = "identity", position = position_stack(reverse = TRUE)) + 
      scale_x_discrete(name = 'Choice', labels = str_wrap(paste0(levels(dataformat$choices),' (',
                                                                 unique(dataformat_summaryattri[order(-varmean), .(variable,N)])$N,')'), width=10)) +
      scale_y_continuous(name= 'Response', expand=c(0,0), limits=c(-1,1), breaks=seq(-1,1,0.25),labels=commapos) +
      theme_classic() + 
      theme(axis.title.x = element_blank()) +
      ggtitle(paste0(title,'?')) +
      coord_flip() +
      scale_fill_brewer(palette = "RdYlBu", 
                        labels= unique(dataformat_summaryattri[order(value),]$i.value)) +
      theme(legend.title = element_blank())
    
  } else {
    p <- ggplot(dataformat_summaryattri[order(dataformat_summaryattri$value, dataformat_summaryattri$variable),], 
                (aes(x=variable, y=frac, fill=value, group=value))) +
      geom_bar(stat="identity") + 
      scale_x_discrete(labels = str_wrap(paste0(levels(dataformat$choices),' (',
                                                unique(dataformat_summaryattri[order(-varmean), .(variable,N)])$N,')'), width=10)) +
      scale_y_continuous(name= 'Response', expand=c(0,0)) +
      theme_classic() + 
      theme(axis.title.x = element_blank()) +
      ggtitle(paste0(title,'?')) +
      theme(legend.title = element_blank())
  }
  print(p)
}

resize_heights <- function(g, heights = rep(1, length(idpanels))){
  idpanels <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  if(getRversion() < "3.3.0"){
    g$heights <- grid:::unit.list(g$heights)
  }
  g$heights[idpanels] <- unit.c(do.call(unit, list(heights, 'null')))
  g
}

resize_widths <- function(g, widths = rep(1, length(idpanels))){
  idpanels <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  if(getRversion() < "3.3.0"){
    g$widths <- grid:::unit.list(g$widths)
  }
  g$widths[idpanels] <- unit.c(do.call(unit, list(widths, 'null')))
  g
}

################ Q2.2 - How many classes do you teach? ################
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
heedteach[dataduration==0, dataduration := 1] #For those that only lasted one year, change to 1 (e.g start and end in 2015)

#Histogram of duration
durahist <- histoplot(heedteach, col='dataduration', unit='Years')

#Consider adding color (color = X1_Q11.4_1)
ggplot(heedteach[Q2.5_1 != -99 & Q2.6_1 != -99,]) + 
  geom_segment(aes(x=Q2.5_1, xend=Q2.6_1, y=ResponseId, yend=ResponseId), size=1.2) +
  scale_color_distiller(palette='Spectral') + 
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
                     name='Number of instructors') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Teaching Q8.4: How many instructors are/were involved? Number of respondents: ', 
                heedteach[!is.na(Q8.4_1) & Q8.4_1 != -99, .N], '/', heedteach[, .N])) + 
  theme_classic()

################ Q9.2 - What is/was the total number of students participating in the field excursions in a typical year? ################
##qplot(heedteach$Q9.2_1)

check <- heedteach[Q9.2_1 %in% c(0,-99, NA),]
heedteach[is.na(Q9.2_1), Q9.2_1 := 101]
heedteach[Q9.2_1 %in% c(0,101), flag:=flag+1]

print(histoplot(data=heedteach, col='Q9.2_1',unit='Students', binw=5))

################ Q9.3 - In how many field excursions are/were ecological data collected during the class? ################
#qplot(heedteach$Q9.3_1)

check <- heedteach[Q9.3_1 %in% c(0,-99, NA),]
heedteach[is.na(Q9.3_1), Q9.3_1 := max(heedteach$Q9.3_1, na.rm=T)+1]
heedteach[Q9.3_1 %in% c(0,max(heedteach$Q9.3_1, na.rm=T)), flag:=flag+1]

print(histoplot(data=heedteach, col='Q9.3_1',unit='Excursions', binw=1))

################ Q9.4 - How many total days of ecological data collection are/were involved across all field excursions in a typical year? ################
#qplot(heedteach$Q9.4_1)

check <- heedteach[Q9.4_1 %in% c(0,-99, NA),]
heedteach[is.na(Q9.4_1), Q9.4_1 := 31]
heedteach[Q9.4_1==-99, Q9.4_1 := NA]
heedteach[Q9.4_1 %in% c(NA, 0,max(heedteach$Q9.4_1, na.rm=T)), flag:=flag+1]

print(histoplot(data=heedteach, col='Q9.4_1',unit='Days', binw=2))

################ Q9.5 - Across all field excursions, in how many locations does/did the class collect ecological data? ##############
#qplot(heedteach$Q9.5_1)

check <- heedteach[Q9.5_1 %in% c(0,-99, NA),]
heedteach[is.na(Q9.5_1), Q9.5_1 := 51]
heedteach[Q9.5_1==-99, Q9.5_1 := NA]
heedteach[Q9.5_1 %in% c(NA, 0,max(heedteach$Q9.5_1, na.rm=T)), flag:=flag+1]

heedteach[Q9.5_1 < 4, .N]/heedteach[!(Q9.5_1 %in% c(NA, 0)), .N]

print(histoplot(data=heedteach, col='Q9.5_1', unit='Locations', binw=2))
################ Q10.2 - What is/was the total cost of running the field excursion(s) for the class ($US) in a typical year? ##############
#A bug in the survey: some weird condition made it in there? - we got no data

################ Q10.3 - What are/were the typical funding sources to support the field excursion(s) costs? ##############
check <- heedteach[(Q10.3_6_TEXT != "" & Q10.3_6_TEXT != '-99') |
                     (Q10.3_7_TEXT != "" & Q10.3_7_TEXT != '-99'),] 

grant_reclass<- c('grant funding','Project if possible', 'Scientific grants', 'Internal grant','Federal grant', 'Various projects')
none_reclass <- c('I contributed my research lab equipment.', 'Field site was local (Botanic Garden on campus) so field excursion costs were minimal', 'none needed')
course_reclass <- c('Course fees', 'Course fees and department support', 'Course fees')

heedteach[(Q10.3_6_TEXT) %in% grant_reclass,`:=`(Q10.3_6 = '-99', Q10.3_6_TEXT = 'Grant')]
heedteach[(Q10.3_7_TEXT) %in% grant_reclass,`:=`(Q10.3_7 = '-99', Q10.3_7_TEXT = 'Grant')]
heedteach[(Q10.3_6_TEXT) %in% none_reclass,`:=`(Q10.3_6 = '-99', Q10.3_6_TEXT = 'None needed')]
heedteach[(Q10.3_7_TEXT) %in% none_reclass,`:=`(Q10.3_7 = '-99', Q10.3_7_TEXT = 'None needed')]
heedteach[(Q10.3_6_TEXT) %in% course_reclass,`:=`(Q10.3_6 = '-99', Q10.3_6_TEXT = '-99', 
                                                  Q10.3_1 = 'University/academic department')]
heedteach[(Q10.3_7_TEXT) %in% course_reclass,`:=`(Q10.3_7 = '-99', Q10.3_7_TEXT = '-99',
                                                  Q10.3_1 = 'University/academic department')]
heedteach[(Q10.3_6_TEXT) == 'Third party funding',`:=`(Q10.3_6 = '-99', Q10.3_6_TEXT = '-99', 
                                                       Q10.3_2 = 'Outside organization(s)')]

q10_3_melt <- multiformat(heedteach, pattern='Q10.3')
ggplot(q10_3_melt[!is.na(value) & !(value %in% c('Other #1 (please specify)','Other #2 (please specify)')),], aes(x=value)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='Source') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Q10.3 - What are/were the typical funding sources to support the field excursion(s) costs? Number of respondents: ', 
                length(q10_3_melt[!is.na(value), unique(ResponseId)]), '/', heedteach[, .N])) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=10, vjust=0.5))

#General stats
q10_3_melt[!is.na(value) & !(value %in% c('Other #1 (please specify)','Other #2 (please specify)')),
           .N, by=value]


fundingsource<- dcast(q10_3_melt[!is.na(value) & !(value %in% c('Other #1 (please specify)','Other #2 (please specify)')),
                          .(ResponseId, variable, value)], 
               ResponseId~variable, value.var = 'value', drop=F)

#Number of CUREs that rely exclusively on university funding
sum(rowSums(fundingsource[!is.na(Q10.3_1), (lapply(.SD, function(x) !is.na(x))),
                          .SDcols=as.character(unique(q10_3_melt$variable))])==1)


################ Q10.4 - Does/did an organization outside of your higher-education institution provide some level of support? ##############
check <- heedteach[(Q10.4_9_TEXT != "" & Q10.4_9_TEXT != '-99') |
                     (Q10.4_9_TEXT != "" & Q10.4_9_TEXT != '-99'),] 
check$Q10.4_9_TEXT
heedteach[(Q10.4_9_TEXT) %in% c('Conservation Management Institute at Virginia Tech', "I don't know"),
          `:=`(Q10.4_9 = '-99', Q10.4_9_TEXT = '-99')]
heedteach[(Q10.4_9_TEXT) %in% c('field station', "Federal Research Institute (academic, but not a university)"),
          `:=`(Q10.4_9 = '-99', Q10.4_9_TEXT = '-99', Q10.4_2 = 'Another higher-education institution (e.g. university)')]


q10_4_melt <- multiformat(heedteach, pattern='Q10.4')
ggplot(q10_4_melt[!is.na(value) & !(value %in% c('Other (please specify)')),], aes(x=value)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='Institution') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Q10.4 - Does/did an organization outside of your higher-education institution provide some level of support? Number of respondents: ', 
                length(q10_4_melt[!is.na(value), unique(ResponseId)]), '/', heedteach[, .N])) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=10, vjust=0.5))

################ Q10.5 - What type of support do/did these partner organizations provide? ##############
#Check the one who keeps answering I don't know 
check <- heedteach[(Q10.5_5_TEXT != "" & Q10.5_5_TEXT != '-99') |
                     (Q10.5_5_TEXT != "" & Q10.5_5_TEXT != '-99'),] 
check$Q10.5_5_TEXT
heedteach[(Q10.5_5_TEXT) %in% c('Permit',"I don't know"),
          `:=`(Q10.5_5 = '-99', Q10.5_5_TEXT = '-99')]
heedteach[(Q10.5_5_TEXT) %in% c('Additional background data', "Background information, show-and-tell visits"),
          `:=`(Q10.5_5 = '-99', Q10.5_5_TEXT = 'Background data/Training')]
heedteach[(Q10.5_5_TEXT) %in% c('waived camping fees'),
          `:=`(Q10.5_5 = '-99', Q10.5_5_TEXT = '-99', Q10.5_4= 'Financial support')]
heedteach[(Q10.5_5_TEXT) %in% c('Access with user fees'),
          `:=`(Q10.5_5 = '-99', Q10.5_5_TEXT = '-99', Q10.5_1= 'Access to field location')]


q10_5_melt <- multiformat(heedteach, pattern='Q10.5')
ggplot(q10_5_melt[!is.na(value) & !(value %in% c('Other (please specify)')) & value!='',], aes(x=value)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='Institution') + 
  scale_y_continuous(expand=c(0,0)) + 
  ggtitle(paste('Q10.5 - What type of support do/did these partner organizations provide? Number of respondents: ', 
                length(q10_5_melt[!is.na(value) & value!='', unique(ResponseId)]), '/', heedteach[, .N])) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=10, vjust=0.5))

################ Q11.2 - country where the ecological data are/were collected ##############
heedteach[grep('(u[.]*s[.]*a*$)|(united.*states)', heedteach$X1_Q11.2, ignore.case = TRUE),
          X1_Q11.2 := 'USA']

singleAplot(heedteach, 'X1_Q11.2') + theme(axis.text.x = element_text(angle=40, vjust=0.5))

################ Q11.3 - Please drag the pin to the field location as precisely as possible in the map below ##############

fieldlocs <- multiformat(heedteach, pattern='Q11[.][34]') %>%
  .[!(is.na(value) | value==''), ] %>%
  .[, `:=`(mainq = substr(variable, 5,12), 
           ite = substr(variable, 1,2))]
fieldlocs_cast <- dcast(fieldlocs, ResponseId+ite~mainq, value.var='value') %>%
  .[, c('lat', 'long') := tstrsplit(`11.3`, ',', fixed=T)] %>%
  .[, `:=`(long = as.numeric(str_extract(long, "[-+]?\\d+(\\.\\d+)?")),
           lat = as.numeric(str_extract(lat, "[-+]?\\d+(\\.\\d+)?")))] %>%
  .[heedteach[, .(ResponseId, Q9.2_1)], on='ResponseId']

#Unique field locations
nrow(fieldlocs_cast[, unique(.(lat, long))])
length(unique(fieldlocs_cast[!is.na(lat), ResponseId]))

#Write geolocated data out
flocs <- SpatialPointsDataFrame(coords=coordinates(data.frame(fieldlocs_cast[!is.na(long),long],
                                                              fieldlocs_cast[!is.na(long),lat])),
                                data = fieldlocs_cast[!is.na(long),],
                                proj4string=CRS("+init=epsg:4326"))

writeOGR(flocs, dsn='results/GIS', layer='fieldlocations2', driver='ESRI Shapefile')
                                
leaflet(data = flocs) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

################ Q11.4 - In what ecosystem/biome/habitat? ##############
table(heedteach$X1_Q11.4_1)
heedteach[,X1_Q11.4_2:=substr(X1_Q11.4_2, 1,25)]
ggplot(heedteach, aes(x=X1_Q11.4_1, fill=X1_Q11.4_2)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(name='Ecosystem') + 
  theme_classic() +
  theme(axis.text.x = element_text(angle=15, vjust=0.6))

################ Q12.2 - What type(s) of variables are/were collected during the field excursion(s)?  ##############
multiAhisto(heedteach, pattern= 'Q12.2', xaxis = 'Data type')
multiAhisto(heedteach, pattern= 'Q12.3', xaxis = 'Data type') + 
  theme(axis.text.x = element_text(angle=15))
multiAhisto(heedteach, pattern= 'Q12.4', xaxis = 'Data type') + 
  theme(axis.text.x = element_text(angle=30))
multiAhisto(heedteach, pattern= 'Q12.5', xaxis = 'Data type')  + 
  theme(axis.text.x = element_text(angle=30))
#axis.ticks.length = unit(1, "cm")

################ Q13.2 - Did you collect the data with the intention to study a specific threat to the factors of interest/environment?  ##############
singleAplot(heedteach, col='Q13.2')

################ Q13.3 - What threat(s) is/was the data collection intended to study?  ##############
multiAhisto(heedteach, pattern= 'Q13.3',xaxis = 'Threat')  + theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

################ Q15.2 - What benefits do you think your students gain/gained from collecting and working with the class' ecological dataset? ########
q15_2format <- likertformat(heedteach, heednumteach, 'Q15[.]2.*[^TEXT]$')
likertboxplot(q15_2format)
likertstackedbar(q15_2format)

################ Q15.3 - Have you ever used the data collected in this class outside of class-based projects (e.g. publications, blog posts, grant proposals)? ########
singleAplot(heedteach, 'Q15.3')

#Dependent on data duration
heedteach[dataduration<5, sum(Q15.3 == 'Yes')/.N]
heedteach[dataduration>5, sum(Q15.3 == 'Yes')/.N]

################ Q15.4 - How many peer-reviewed publications have resulted from the data collected in this class?  ########
print(histoplot(data=heedteach, col='Q15.4_1', unit='Publications', binw=1))
sum(heedteach$Q15.4_1, na.rm=T)
sum(heedteach[, Q15.4_1>0], na.rm=T)


################ Q15.5 - How many peer-reviewed publications were the students involved in?  ########
check <- heedteach[Q15.5_1 %in% c(0,-99, NA),]
heedteach[Q15.4_1>0 & is.na(Q15.5_1), Q15.5_1 := 11]
heedteach[Q15.5_1 %in% c(-99,11), flag:=flag+1]
heedteach[Q15.5_1==-99, Q15.5_1 := NA]

#Proportion publications involving students
studpubpor <- heedteach[Q15.4_1>0, Q15.5_1/Q15.4_1]
studpubpor[studpubpor > 10] <- 0
studpubpor[studpubpor > 1] <- 1
mean(studpubpor, na.rm=T) #Average % of publications involving students per class
print(histoplot(data=heedteach, col='Q15.5_1', unit='Publications', binw=1))

################ Q15.6 - Have these data resulted in subsequent grants or projects?  ########
singleAplot(heedteach, 'Q15.6')

################ Q15.7 - Have these data been used by local or national government agencies?  ########
multiAhisto(heedteach, pattern= 'Q15[.]7.*[^TEXT]$', xaxis = 'Uses') 

multiformat(heedteach, pattern= 'Q15[.]7.*[^TEXT]$')[
  !is.na(value) & !(value %in% c('Other (please specify)','','-99')),][
    ,.N/132, by=value]

################ Q15.8 - What other personal benefits have you gained from teaching a class involving ecological data collection?  ########
q15_8format <- likertformat(heedteach, heednumteach, 'Q15[.]8.*[^TEXT]$')
likertboxplot(q15_8format)
likertstackedbar(q15_8format)

################ Q15.9 - What other significant outcomes have you seen as a result of your class-based ecological data collection?  ########
kable(heedteach[!(Q15.9 %in% c('','-99')),'Q15.9', with=FALSE], 
      caption = paste0(tstrsplit(qs[rownames(qs) == 'Q15.9', 1], '?', fixed=T)[[1]],'?')) %>%
  kable_styling(bootstrap_options = c("striped", "hover","responsive"), full_width=T) 

################ Q15.10 - For what main reasons have the data not been used outside of class-based projects? ########
multiAhisto(heedteach, pattern= 'Q15[.]10.*[^TEXT]$', xaxis = '') 
unique(multiformat(heedteach, pattern= 'Q15[.]10.*[^TEXT]$')[!is.na(value), .(value, levels)])
multiformat(heedteach, pattern= 'Q15[.]10.*[^TEXT]$')[,sum(ifelse(!is.na(value) & value != '', 1,0)), by=levels]

################ Q16.2 - In your experience, what are the main challenges to implementing and maintaining class-based data collection? ########
q16_2format <- likertformat(heedteach, heednumteach, 'Q16[.]2.*[^TEXT]$',diverging = TRUE)
likertstackedbar(q16_2format, diverging=TRUE)

q16_2format[i.levels==5, .N, by=i.value] #Lack of instructor's time numbers 
q16_2format[i.levels==6, .N, by=i.value] #Logistical complexity


################ Q16.3 - Do you have additional comments or advice from your experience implementing and maintaining class-based data collection? ########
kable(heedteach[!(Q16.3 %in% c('','-99')),'Q16.3', with=FALSE], 
      caption = paste0(tstrsplit(qs[rownames(qs) == 'Q16.3', 1], '?', fixed=T)[[1]],'?')) %>%
  kable_styling(bootstrap_options = c("striped", "hover","responsive"), full_width=T)

################ Q16.4 - In your experience, what are the main challenges to analyzing data collected as part of field excursions?########
multiAhisto(heedteach, pattern= 'Q16[.]4.*[^TEXT]$', xaxis = '') 
unique(multiformat(heedteach, pattern= 'Q16[.]4.*[^TEXT]$')[!is.na(value), .(value, levels)])
multiformat(heedteach, pattern= 'Q16[.]4.*[^TEXT]$')[,sum(ifelse(!is.na(value) & value != '', 1,0)), by=levels]

################ Q16.5 - For what percentage of the study period do gaps in data collection exist?  ###########
singleAplot(heedteach, col='Q16.5')
table(heedteach[!(Q16.5 %in% c('-99',-99,'')), Q16.5])


################ Q16.6 - What are the main reasons for these gaps in data collection?  ###########
multiAhisto(heedteach, pattern= 'Q16[.]6.*[^TEXT]$', xaxis = '') 
unique(multiformat(heedteach, pattern= 'Q16[.]6.*[^TEXT]$')[!is.na(value), .(value, levels)])
multiformat(heedteach, pattern= 'Q16[.]6.*[^TEXT]$')[,sum(ifelse(!is.na(value) & value != '', 1,0)), by=levels]

dcast(multiformat(heedteach, 'Q16[.]6.*(TEXT)*$'), ResponseId~variable, var.name = value)[
  (!is.na(Q16.6_1) & Q16.6_1 != '') | (!is.na(Q16.6_2) & Q16.6_2 != ''), .N/83] 


################ Q16.7 - For which of the following applications do you feel comfortable using your class-based dataset? ########
#BUG in the survey, did not display!!?

################ Q16.8 - What factors, if any, limit your confidence in using the data for other purposes than the class requirements? #######
multiAhisto(heedteach, pattern= 'Q16[.]8.*[^TEXT]$', xaxis = '') 

################ Q16.9 - Do you have additional comments or advice from your experience analyzing and disseminating data from a class-based ecological dataset? ##############
kable(heedteach[!(Q16.9 %in% c('','-99')),'Q16.9', with=FALSE], 
      caption = paste0(tstrsplit(qs[rownames(qs) == 'Q16.9', 1], '?', fixed=T)[[1]],'?')) %>%
  kable_styling(bootstrap_options = c("striped", "hover","responsive"), full_width=T)

################ Q18.2 - How are/were data stored for purposes other than the class? ####
multiAhisto(heedteach, pattern= 'Q18[.]2.*[^TEXT]$', xaxis = '') 

#Archived and locally stored only
dcast(multiformat(heedteach, 'Q18[.]2.*(TEXT)*$'), ResponseId~variable, var.name = value)[
  !((!is.na(Q18.2_3) & Q18.2_3 != '') | (!is.na(Q18.2_4) & Q18.2_4 != '')) &
    ((!is.na(Q18.2_1) & Q18.2_1 != '') | (!is.na(Q18.2_2) & Q18.2_2 != '')), .N/132] 

#Not archived or analyzed
dcast(multiformat(heedteach, 'Q18[.]2.*(TEXT)*$'), ResponseId~variable, var.name = value)[
  ((!is.na(Q18.2_6) & Q18.2_6 != '') | (!is.na(Q18.2_7) & Q18.2_7 != '')) &
    !((!is.na(Q18.2_1) & Q18.2_1 != '') | (!is.na(Q18.2_2) & Q18.2_2 != '')), .N/132] 


################ Q18.3 - Do you currently share the data collected as part of this class? ####
singleAplot(heedteach, 'Q18.3')
table(heedteach[, Q18.3])



################ Q18.4 - At what level(s) do you currently share your data? ####
multiAhisto(heedteach, 'Q18.4', xaxis='Choice')

#See PUBLICATION PLOTS AND STATISTICS for statistics

################ Q18.5 - What level(s) of data sharing would you be interested in, if any? ####
multiAhisto(heedteach, 'Q18.5', xaxis='Choice')

#See PUBLICATION PLOTS AND STATISTICS for statistics

################ Q18.6 - Under what condition(s) do you share your data? ####
multiAhisto(heedteach, 'Q18.6', xaxis='Choice')

#See PUBLICATION PLOTS AND STATISTICS for statistics

################ Q18.7 - Under what condition(s) would you be interested in sharing your data, if any? ####
multiAhisto(heedteach, 'Q18.7', xaxis='Choice')

#See PUBLICATION PLOTS AND STATISTICS for statistics

################ Q18.8 - Would you be interested in an online community repository specifically designed to host class-based ecological datasets? ####
q18_8format <- likertformat(heedteach, heednumteach, 'Q18[.]8.*[^TEXT]$',diverging = TRUE)
likertstackedbar(q18_8format, diverging=TRUE) 

################ Q19.1 - Do you have additional comments on courses involving ecological data collection you would like to share? ####
kable(heedteach[!(Q19.1 %in% c('','-99')),'Q19.1', with=FALSE], 
      caption = paste0(tstrsplit(qs[rownames(qs) == 'Q19.1', 1], '?', fixed=T)[[1]],'?')) %>%
  kable_styling(bootstrap_options = c("striped", "hover","responsive"), full_width=T)

#Note: #q15.5 NOT DISPLAYED TO THOSE WHO PRESSED > 10 PUBLICATIONS


#####_________________________________________________________________________####
#    PUBLICATION PLOTS AND STATISTICS
#####_________________________________________________________________________####
################ Survey and respondents charateristics #######
################ Q2.5_1 and Q2.6_1 - In what year did data collection start in the context of this class?  When was the last year of data collection, if applicable? ################
heedteach[is.na(Q2.6_1), Q2.6_1 := 2019]
heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, dataduration := Q2.6_1-Q2.5_1]

#---- Histogram of duration ----
setDT(heedteach)
binw=1

sum(heedteach$dataduration>=15, na.rm=T)/sum(!is.na(heedteach$dataduration)) # of classes lasting at least 15 years

meacol <- heedteach[,mean(dataduration, na.rm=T)]
medcol <- heedteach[,median(dataduration, na.rm=T)]
maxcol <- max(heedteach[,dataduration], na.rm=T)
valrange <- seq(min(heedteach[,dataduration], na.rm=T),maxcol, by=5)

durahist <- ggplot(heedteach, aes(x=dataduration)) + 
  geom_histogram(binwidth=binw, alpha=1/3) +
  scale_x_continuous(breaks=c(valrange, maxcol), 
                     labels=c(valrange, maxcol),
                     expand=c(0,0), name='Number of years') +
  scale_y_continuous(expand=c(0,0), name='Count') + 
  # geom_vline(xintercept=meacol) + 
  # annotate("text", x=meacol-binw/5, y=Inf, hjust=1.2, angle=90,
  #          label = paste('Mean:',round(meacol,1), 'years')) +
  geom_vline(xintercept=medcol+3) + 
  annotate("text", x=medcol-1.5, y=Inf, hjust=1.2,  angle=90,
           label = paste('Median:',round(medcol,1), 'years')) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90))

#---- Cumulative number ----
cumdat <- merge(heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, .N, by=Q2.5_1][order(Q2.5_1),list(year=Q2.5_1,cumstart=cumsum(N))],
                heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, .N, by=Q2.6_1][order(Q2.6_1),list(year=Q2.6_1,cumend=cumsum(N))],
                on='year', all.x=T) %>%
  .[is.na(cumend), cumend := 0,] %>%
  .[, cumactive := cumstart-cumend]

ggplot(cumdat, aes(x=year)) + 
  geom_ribbon(aes(ymin=cumend, ymax=cumstart), fill='red') +
  geom_ribbon(aes(ymin=0, ymax=cumend), fill='blue')

#---- Top plot ----
heedteach[,ResponseId := factor(ResponseId, levels = unique(heedteach$ResponseId[order(heedteach$Q2.6_1, heedteach$Q2.5_1)]))]

top <- ggplot(data=cumdat, aes(x=year)) + 
  geom_line(aes(y=cumstart), color='red', size=1.5, alpha=1/3) +
  geom_line(aes(y=cumend), color='blue', size=1.5, alpha=1/3) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#---- Main plot (consider adding color based on biome? aes(color = X1_Q11.4_1)) ----
main <- ggplot(data=heedteach[Q2.5_1 != -99 & Q2.6_1 != -99,]) +
  geom_segment(aes(x=Q2.5_1, xend=Q2.6_1, y=as.numeric(ResponseId), yend=as.numeric(ResponseId), color=dataduration), size=1.2) +
  scale_y_continuous(expand=c(0,0), name='Number of classes') +
  scale_x_continuous(expand=c(0,-0.5), name='Start and end years of data collection') + 
  scale_color_distiller(palette='Spectral', name=str_wrap('Class duration (years)', 20)) +  
  # ggtitle(paste('Teaching Q2.5 and 2.6: In what year did data collection start and end. Number of respondents:', 
  #               heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, .N], '/', heedteach[, .N])) + 
  annotation_custom(ggplotGrob(durahist), 
                    xmin=1958, xmax=1995, 
                    ymin=quantile(heedteach[,as.numeric(ResponseId)], .52), 
                    ymax=quantile(heedteach[,as.numeric(ResponseId)], 1)) + 
  theme_classic() +
  theme(legend.position=c(0.30, 0.05),
        legend.direction="horizontal",
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.title.align=1,
        axis.text.x = element_text(size=11),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm"),
        panel.grid.major.x = element_line(color='lightgrey'),
        panel.grid.minor.x = element_blank()) #element_line(color='lightgrey'))

png('results/startendyears_minimal2.png', width=14, height=9, units='in', res=400)
grid.draw(main)
dev.off()

#---- Side plot ----
sidedat <- data.frame(year=2019,num=cumdat[year==2018, c(cumactive, cumend)],
                      duration = c(heedteach[Q2.5_1 != -99 & Q2.6_1 != -99 & Q2.6_1==2019, mean(dataduration)],
                                   heedteach[Q2.5_1 != -99 & Q2.6_1 != -99 & Q2.6_1<2019, mean(dataduration)]),
                      cat=factor(c('Ongoing', 'Ended'),c('Ongoing', 'Ended')))
rightside <- ggplot(sidedat, aes(x=year, y=num, fill=cat, 
                                 label = paste0(cat,': ', round(100*num/cumdat[year==2018,cumstart]), '%'))) + 
  geom_bar(stat='identity') +
  geom_text(aes(y=cumdat[year==2018, c((cumstart+cumend)/2, cumend/2)]), angle=-90) + 
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c('#99d396', '#72adaa')) +
  theme_void() + 
  theme(legend.position = 'none')
#rightside

#g <- gtable_rbind(ggplotGrob(top), ggplotGrob(main))
#grid.draw(resize_heights(g, c(1,3)))

#---- Plot out ----
g <- gtable_cbind(ggplotGrob(main), ggplotGrob(rightside))
png('results/startendyears.png', width=4, height=6, units='in', res=600)
grid.draw(resize_widths(g, c(20,1)))
dev.off()

################ Compute the total number of students or days involved ##########
#Number of students * number of days * number of times a year * number of years
heedteach_sub <- heedteach[data.frame(Q8.3=levels(heedteach[,Q8.3]), 
                                      Q8.3num = c(2, 1, 1/3, 0.5)), on='Q8.3'][
                                        ,.(ResponseId, Q8.3num,dataduration,Q9.2_1,Q9.4_1)]

setnames(heedteach_sub, old=c('Q8.3num', 'Q9.2_1', 'Q9.4_1'), new=c('frequency', 'students', 'days'))
#For those with a missing component, assign median
heedteach_sub[frequency != 0, `:=`(dataduration = ifelse(is.na(dataduration), median(heedteach_sub$dataduration,na.rm=T), dataduration),
                                   days = ifelse(is.na(days), median(heedteach_sub$days,na.rm=T),days))]


#Total number of involved students
heedteach_sub[, sum(students*frequency*dataduration, na.rm=T)]

#Total number of days
heedteach_sub[, sum(frequency*days*dataduration, na.rm=T)]

################ What is being collected? #######################
#---- Biological ----
#Prep data for chart
bioqcols <- names(heedteach)[grep('Q12.3', names(heedteach))]
dat <- heedteach[, (bioqcols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = bioqcols]
bioqmelt <- melt(dat, id.vars='ResponseId', measure.vars=bioqcols)
old <- c("Occurrence/abundance/density/biomass (single species or multiple species)", 
         "Individual physical attributes (body length, mass, leaf area, DBH, sex, age)", 
         "Life history (e.g. fecundity, age)",
         "Individual behavior", 
         "Physiology",
         "Phenology (e.g. migration timing, date of flowering)",
         "Biogeochemistry (e.g. stable isotope, nutrient cycling, metabolism)",
         "Genetics", 
         "Molecular/Biochemistry",
         "Other (specify)",
         "" ,
         "Ch a, Blue-Green-Algae, dbh",
         "Biological indices")

new <- c('Species',
         'Individual attributes',
         'Individual attributes',
         'Individual behavior/physiology',
         'Individual behavior/physiology',
         'Phenology',
         'Biogeochemistry',
         'Genetics/Molecular',
         'Genetics/Molecular',
         rep(NA, 4))

bioqmelt[, value := new[match(value, old)]]
bioqmeltord <- unique(bioqmelt[,value := factor(value, levels = unique(new))][, .(ResponseId, value)])

#Make pie chart
biopie <- ggplot(bioqmeltord[!is.na(value),list(varn = .N),ResponseId][order(varn),.N, varn], 
                 aes(x=factor(1), y=N, fill=-varn, label = varn)) + 
  coord_polar(theta = "y", start=0) +
  geom_bar(stat='identity', color='black')+
  geom_text_repel(position = position_stack(vjust = 0.5), size=4, force=0.5, box.padding=0.02, max.iter=100000) +
  scale_fill_distiller(palette='YlGn')+
  theme_void()  +
  theme(legend.position = 'none') 

#Main chart
biovarplot <- ggplot(bioqmeltord[!is.na(value),], aes(x=value)) + 
  geom_bar(aes(y=100*(..count..)/length(unique(bioqmeltord[!is.na(value),ResponseId]))), fill='#addd8e') + 
  scale_x_discrete(expand=c(0,0), labels = str_wrap(unique(new), width=10), name= 'Data type') + 
  scale_y_continuous(expand=c(0,0), limits=c(0,100), breaks=seq(0,100,25)) + 
  annotation_custom(ggplotGrob(biopie), 
                    ymin=50, ymax=135, xmin = 5, xmax = 6) + 
  # ggtitle(paste(title, '? Number of respondents: ', 
  #               length(bioqmelt[!is.na(value) & !(value %in% c('','-99')), unique(ResponseId)]), '/', data[, .N])) + 
  theme_classic() +
  theme(axis.title= element_blank())
biovarplot


#---- Physical ----
phyqcols <- names(heedteach)[grep('Q12.4', names(heedteach))]
dat <- heedteach[, (phyqcols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = phyqcols]
phyqmelt <- melt(dat, id.vars='ResponseId', measure.vars=phyqcols)
old <- unique(phyqmelt$value)
new <- c('Temp.', NA, NA, 'Light', 'Light', 'Light',
         'Precip.', 'Topo.', 'Topo.', 'Humidity',
         'Radia.', 'Sound/Noise', 
         'Soil attri.',  'Soil attri.', 'Soil attri.',
         'Veg. coverage', 'Hydrol.', 'Habitat attri.', 'Habitat attri.', 'Hydrol.',
         'Depth/Bathy.', 'Wind speed', 'Hydrol.', 'Habitat attri.', 'Water attri.', 
         'Water attri.', 'Water attri.', NA, 'Other', 'Water attri.', 'Soil attri.',
         'Other', NA, 'Other', 'Light', 'Hydrol.', NA)

phyqmeltord <- unique(phyqmelt[, value := new[match(value, old)]][!is.na(value), .(ResponseId, value)])
phyqmeltord[, value := factor(value, 
                              levels = unique(value)[order(-phyqmeltord[, .N, by=value]$N)])]

#Make pie chart
physpie <- ggplot(phyqmeltord[!is.na(value),list(varn = .N),ResponseId][order(varn),.N, varn], 
                  aes(x=factor(1), y=N, fill=-varn, label = varn)) + 
  coord_polar(theta = "y", start=0) +
  geom_bar(stat='identity', color='black')+
  geom_text_repel(position = position_stack(vjust = 0.5), size=4, force=0.5, box.padding=0.02, max.iter=100000) +
  scale_fill_distiller(palette='YlOrRd')+
  theme_void()  +
  theme(legend.position = 'none') 

#Make main chart
physvarplot <- ggplot(phyqmeltord, aes(x=value)) + 
  geom_bar(aes(y=100*(..count..)/length(unique(phyqmeltord[!is.na(value),ResponseId]))), fill='#fed976') + 
  scale_x_discrete(expand=c(0,0), labels = str_wrap(levels(phyqmeltord$value), width=8), name= 'Data type') + 
  scale_y_continuous(expand=c(0,0), limits=c(0,100), breaks=seq(0,100,25)) + 
  annotation_custom(ggplotGrob(physpie), 
                    ymin=40, ymax=95, xmin = 11, xmax = 15) + 
  # ggtitle(paste(title, '? Number of respondents: ', 
  #               length(phyqmelt[!is.na(value) & !(value %in% c('','-99')), unique(ResponseId)]), '/', data[, .N])) + 
  theme_classic()+
  theme(axis.title = element_blank())
#physvarplot

#---- Chemical ----
cheqcols <- names(heedteach)[grep('Q12.5.*(?<!TEXT)$', names(heedteach), perl=T)]
dat <- heedteach[, (cheqcols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = cheqcols]
cheqmelt <- melt(dat, id.vars='ResponseId', measure.vars=cheqcols)
old <- unique(cheqmelt$value)
new <- c(NA, "pH",NA,"EC/Salinity/TDS","Pollutant","N","P","CO3-2 HCO3","Ca","Fe",
         "Mg","Al","K","H","S","Cl","DO/BOD/COD", "Alka. Acid.", "Other")
cheqmeltord <- unique(cheqmelt[, value := new[match(value, old)]][!is.na(value), .(ResponseId, value)])
cheqmeltord[, value := factor(value, levels = unique(value)[order(-cheqmeltord[, .N, by=value]$N)])]

#Make pie chart
chempie <- ggplot(cheqmeltord[!is.na(value),list(varn = .N),ResponseId][order(varn),.N, varn],  
                  aes(fill=-varn, label = varn)) + 
  coord_polar(theta = "y", start=0) +
  geom_bar(aes(x=factor(1), y=N), stat='identity', color='black')+
  #geom_histogram(aes(x=varn)) +
  #coord_polar(theta = "x", start=0) +
  geom_text_repel(aes(x=factor(1), y=N), position = position_stack(vjust = 0.5), size=4, force=0.5, box.padding=0.02, max.iter=100000) +
  scale_fill_distiller(palette='PuBu')+
  theme_void()  +
  theme(legend.position = 'none') 

#Make main chart
chemvarplot <- ggplot(cheqmeltord, aes(x=value)) + 
  geom_bar(aes(y=100*(..count..)/length(unique(cheqmeltord[!is.na(value),ResponseId]))), fill='#a6bddb') + 
  scale_x_discrete(expand=c(0,0), labels = str_wrap(levels(cheqmeltord$value), width=5), name= 'Data type') + 
  scale_y_continuous(expand=c(0,0), limits=c(0,100), breaks=seq(0,100,25)) + 
  annotation_custom(ggplotGrob(chempie), 
                    ymin=35, ymax=70, xmin = 13, xmax = 17) + 
  # ggtitle(paste(title, '? Number of respondents: ', 
  #               length(cheqmelt[!is.na(value) & !(value %in% c('','-99')), unique(ResponseId)]), '/', data[, .N])) + 
  theme_classic()+
  theme(axis.title.y = element_blank())
chemvarplot

#---- Plot out ----
g <- gtable_rbind(ggplotGrob(biovarplot), ggplotGrob(physvarplot), ggplotGrob(chemvarplot))
pdf('results/variables.pdf', width=6, height=6)
grid.draw(g)
dev.off()
#Bunch of formatting in inkscape afterward

#---- Compute # of variable type combinations for tree ----
varnjoin <- merge(bioqmeltord[!is.na(value),list(biovarn = .N),ResponseId], phyqmeltord[!is.na(value),list(phyvarn = .N),ResponseId], 
                  on='ResponseId', all.x=T, all.y=T) %>%
  merge(cheqmeltord[!is.na(value),list(chemvarn = .N),ResponseId], on='ResponseId', all.x=T, all.y=T)
varncols <- c('biovarn', 'phyvarn', 'chemvarn')
varnjoin[,(varncols) := lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), .SDcols = varncols][
  ,combination := paste0(biovarn, phyvarn, chemvarn)]
table(varnjoin$combination)
varnjoin[,.N]

#Average number of variables recorded by type
heedteach[, .(ResponseId)][
  bioqmeltord[,.N, by='ResponseId'], on='ResponseId', all=T][
    cheqmeltord[,.N, by='ResponseId'], on='ResponseId'][
      phyqmeltord[,.N, by='ResponseId'], on='ResponseId'][
        , lapply(.SD, function(x) mean(x, na.rm=T)), .SDcols = c('N', 'i.N', 'i.N.1')]

################ Q16.2 - Main challenges to implementing and maintaining class-based data collection? ########
#---- Format data ----
q16_2format <- likertformat(heedteach, heednumteach, 'Q16[.]2.*[^TEXT]$',diverging = TRUE)
q16_2format_summary <- q16_2format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q16_2format_summaryattri <- q16_2format_summary[unique(q16_2format[,.(variable, value, i.value, choices, varmean, N)]),
                                                on= c('variable','value'), nomatch=0] %>%
  .[, frac2 := ifelse(value ==0, frac/2, frac)] %>%
  .[, challengelabels := gsub('\\s*\\([^\\)]+\\)|^\\s*\\-\\s*', '', choices)] #format labels

up <- q16_2format_summaryattri[value >= 0 & choices != ' - Other (please specify)',] %>%
  .[, `:=`(value = factor(value, levels= unique(value[order(value)])),
           respondent = paste0('Already teaching, N=', max(N)),
           variable_short = substr(variable, str_length(variable), 50))]%>%
  .[, variable_short := factor(variable_short, levels=unique(variable_short[order(varmean)]))]
down <- q16_2format_summaryattri[value <= 0 & choices != ' - Other (please specify)',] %>%
  .[, `:=`(value = factor(value, levels= unique(value[order(value)])),
           respondent = paste0('Already teaching, N=', max(N)),
           variable_short = substr(variable, str_length(variable), 50))]%>%
  .[, variable_short := factor(variable_short, levels=unique(variable_short[order(varmean)]))]

#Inspect 'Others'
heedteach[!(Q19.1 %in% c('','-99')),"Q16.2_8_TEXT", with=FALSE]

#---- Plot out ----
#up[order(varmean),unique(choices

challengeplot <- ggplot() + 
  geom_bar(data = up, aes(x = variable_short, y = 100*frac2, fill = value),
           stat = "identity", position = position_stack(reverse = TRUE)) + 
  geom_bar(data = down, aes(x = variable_short, y = -100*frac2, fill = value),
           stat = "identity", position = position_stack()) +
  geom_text(data=up, aes(x= variable_short, y = 0, label = round(varmean,2))) +
  scale_x_discrete(name = 'Challenge', position = "top",
                   labels = str_wrap(up[order(varmean),unique(challengelabels)], width=10)) +
  scale_y_continuous(name= '% of responses', expand=c(0,0), limits=c(-100,100), 
                     breaks=seq(-100,100,25),labels=commapos) +
  theme_classic() + 
  coord_flip() +
  facet_wrap(~respondent, scales='free_y') +
  scale_fill_manual(values = c('#dfc27d', '#a6611a', '#f5f5f5', '#80cdc1', '#018571'), 
                    labels= paste(c('-2','-1',' 0', ' 1',' 2'),
                                  unique(q16_2format_summaryattri[order(q16_2format_summaryattri$value),]$i.value))) +
  theme(legend.title = element_blank(),
        #axis.title.y =  element_blank(),
        #axis.title.x =  element_blank(),
        panel.grid.major.x = element_line(color='lightgrey'),
        legend.position = c(0.15, 0.85),
        legend.background = element_blank(),
        text= element_text(size=12))

################ Q5.1-16.2 - Challenges across prospective and current instructors? ########
#---- Format data ----
q5_3format <- likertformat(heednoteach, heednumnoteach, 'Q5[.]3.*[^TEXT]$',diverging = TRUE)
q5_3format_summary <- q5_3format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q5_3format_summaryattri <- q5_3format_summary[unique(q5_3format[,.(variable, value, i.value, choices, varmean, N)]),
                                              on= c('variable','value'), nomatch=0] %>%
  .[, frac2 := ifelse(value ==0, frac/2, frac)] %>%
  .[, challengelabels := gsub('\\s*\\([^\\)]+\\)|^\\s*\\-\\s*', '', choices)] #format labels


up_prospect <- q5_3format_summaryattri[value >= 0 & choices != ' - Other (please specify)',] %>%
  .[, `:=`(value = factor(value, levels= unique(value[order(value)])),
           respondent = paste0('Prospective, N=', max(N)),
           variable_short = substr(variable, str_length(variable), 50))] %>%
  .[, variable_short := factor(variable_short, levels=unique(variable_short[order(varmean)]))]

down_prospect <- q5_3format_summaryattri[value <= 0 & choices != ' - Other (please specify)',] %>%
  .[, `:=`(value = factor(value, levels= unique(value[order(value)])),
           respondent = paste0('Prospective, N=', max(N)),
           variable_short = substr(variable, str_length(variable), 50))]%>%
  .[, variable_short := factor(variable_short, levels=unique(variable_short[order(varmean)]))]

#---- Plot out ----
challengeplot_prospective <- ggplot() + 
  geom_bar(data = up_prospect[order(varmean, value),],
           aes(x = variable_short, y = 100*frac2, fill = value),
           stat = "identity", position = position_stack(reverse = TRUE)) + 
  geom_bar(data = down_prospect[order(varmean, value),],
           aes(x = variable_short, y = -100*frac2, fill = value),
           stat = "identity", position = position_stack()) +
  geom_text(data=up_prospect, aes(x= variable_short, y = 0, label = round(varmean,2))) +
  scale_x_discrete(name = 'Challenge', 
                   labels = str_wrap(up_prospect[order(varmean),unique(challengelabels)], width=10)) +
  scale_y_continuous(name= '% of responses', expand=c(0,0), limits=c(-100,100), breaks=seq(-100,100,25),labels=commapos) +
  theme_classic() + 
  coord_flip() +
  scale_fill_manual(values = c('#dfc27d', '#a6611a', '#f5f5f5', '#80cdc1', '#018571'), 
                    labels= paste(c('-2','-1',' 0', ' 1',' 2'),
                                  unique(q5_3format_summaryattri[order(value),]$i.value))) +
  facet_wrap(~respondent, scales='free_y') +
  theme(legend.title = element_blank(),
        axis.title.y =  element_blank(),
        #axis.title.x =  element_blank(),
        panel.grid.major.x = element_line(color='lightgrey'),
        legend.position = 'none', #c(0.15, 0.15),
        legend.background = element_blank(),
        text= element_text(size=12))
grid.arrange(challengeplot,challengeplot_prospective, ncol=2)

pdf('results/challenges.pdf', width=12, height=6)
grid.arrange(challengeplot,challengeplot_prospective, ncol=2)
dev.off()


################ Q15.2 and Q15.8 student and teacher benefits ########################
#---- Format data for student benefits ----
q15_2format <- likertformat(heedteach, heednumteach, 'Q15[.]2.*[^TEXT]$')
q15_2format_summary <- q15_2format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q15_2format_summaryattri <- q15_2format_summary[unique(q15_2format[,.(variable, value, i.value, choices, varmean, N)]),
                                              on= c('variable','value'), nomatch=0] %>%
  .[data.frame(choices = unique(.[choices != ' - Other', choices]),
               formatlabels = c('Field sampling', 'Lab. methods', 'Data analysis', 
                             'Collaborative research','Scientific writing', 'Public speaking', 'Scientific process',
                             'Networking w/ scientists','Relationship building w/ classmates', 'Increased topical interest', 'Awareness of nature'),
               groupin = factor(c(rep('Hard skills', 3), rep('Soft skills', 4), rep('Personal growth', 4)), 
                              levels=c( 'Personal growth', 'Hard skills', 'Soft skills')),
             groupout = rep('Benefits to students', 11),
             respondent = paste0('Already teaching', max(N))),
  on='choices']

q15_2format_summaryattri[, `:=`(formatlabels = factor(formatlabels, unique(formatlabels[order(varmean)])))]
#,groupin = factor(groupin,levels = unique(groupin)[order(-q15_2format_summaryattri[, mean(varmean), by=groupin]$V1)])

q15_2format_summaryattri[, sum(value*frac)/sum(frac), by=groupin] #Average score per type of benefits for students
q15_2format_summaryattri[, sum(value*frac)/sum(frac), by=groupin]

#---- format data for instructors' benefits ----
q15_8format <- likertformat(heedteach, heednumteach, 'Q15[.]8.*[^TEXT]$')
q15_8format_summary <- q15_8format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]

q15_8format_summaryattri <- q15_8format_summary[unique(q15_8format[,.(variable, value, i.value, choices, varmean, N)]),
                                                on= c('variable','value'), nomatch=0] %>%
  .[data.frame(choices = unique(.[choices != ' - Other (please specify)', choices]),
               formatlabels = c('Ideas & data for projects', 'Career advancement', 'Peer recognition', 
                                'Mentoring students','Inspiration'),
               groupin = factor(c(rep('Academic growth', 3), rep('Personal growth', 2)), 
                                levels=c('Academic growth', 'Personal growth')),
               groupout = rep('Benefits to instructors', 5)),
    on='choices']

q15_8format_summaryattri[, `:=`(formatlabels = factor(formatlabels, unique(formatlabels[order(varmean)])),
                                groupin = factor(groupin, 
                                                 levels = unique(groupin)[order(-q15_8format_summaryattri[, mean(varmean), by=groupin]$V1)]))]

#---- Plot out ----
benefitplot <- ggplot(rbind(q15_2format_summaryattri, q15_8format_summaryattri), 
                      (aes(x=formatlabels, y=100*frac, fill=interaction(factor(value), groupout)))) +
  geom_bar(stat="identity", alpha=0.8) + 
  #geom_bar(data = q15_8format_summaryattri[order(q15_8format_summaryattri$value, q15_8format_summaryattri$variable),], stat="identity", alpha=0.8) + 
  geom_text(aes(y = 5, label = paste0('Mean ', round(varmean,2))), hjust = 0) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20), position='top') +
  scale_y_continuous(name= '% of responses', expand=c(0,0)) +
  scale_fill_manual(values=c(brewer.pal(4, "RdPu"), brewer.pal(4, "YlGnBu")), 
                    labels = paste(0:3, rep(rev(levels(q15_2format_summaryattri$i.value)),2))) + 
  ggtitle(paste0('Already teaching, N=', max(q15_2format_summaryattri$N))) +
  coord_flip() +
  facet_grid(groupout*groupin~., scales ='free_y', space='free', switch='y') +
  #theme_ipsum() + 
  theme_minimal() +
  theme(text = element_text(size=16), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(color='black', size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.placement = 'outside',
        strip.text.y = element_text(face='bold'))

# pdf('results/benefits.pdf', width=6, height=8)
print(benefitplot)
# dev.off()
#Quick formatting in inkscape afterward



#---- Format data for prospective student benefits ----
q5_1format <- likertformat(heednoteach, heednumnoteach, 'Q5[.]1.*[^TEXT]$', qsplit = ' - ')
q5_1format_summary <- q5_1format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q5_1format_summaryattri <- q5_1format_summary[unique(q5_1format[,.(variable, value, i.value, choices, varmean, N)]),
                                              on= c('variable','value'), nomatch=0] 
q5_1format_summaryattri <- q5_1format_summaryattri[
  data.frame(choices = unique(q5_1format_summaryattri[choices != 'Other (please specify)', choices]),
             formatlabels = c('Field sampling', 'Lab. methods', 'Data analysis', 
                              'Collaborative research','Scientific writing', 'Public speaking', 'Scientific process',
                              'Networking w/ scientists','Relationship building w/ classmates', 'Increased topical interest', 'Awareness of nature'),
             groupin = factor(c(rep('Hard skills', 3), rep('Soft skills', 4), rep('Personal growth', 4)), 
                              levels=c( 'Personal growth', 'Hard skills', 'Soft skills')),
             groupout = rep('Benefits to students', 11),
             respondent = paste0('Prospective', max(N))),
  on='choices']

q5_1format_summaryattri[, `:=`(formatlabels = factor(formatlabels, unique(formatlabels[order(varmean)])))]
#,groupin = factor(groupin,levels = unique(groupin)[order(-q5_1format_summaryattri[, mean(varmean), by=groupin]$V1)])

#---- format data for prospective instructors' benefits ----
q5_2format <- likertformat(heednoteach, heednumnoteach, 'Q5[.]2.*[^TEXT]$', qsplit = ' - ')
q5_2format_summary <- q5_2format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q5_2format_summaryattri <- q5_2format_summary[unique(q5_2format[,.(variable, value, i.value, choices, varmean, N)]),
                                              on= c('variable','value'), nomatch=0] %>%
  .[choices %in% unique(choices)[5:20], ]
q5_2format_summaryattri <- q5_2format_summaryattri[
  data.frame(choices = unique(q5_2format_summaryattri[choices != 'Other (please specify)', choices]),
             formatlabels = c('Ideas & data for projects', 'Peer recognition', 'Career advancement', 
                              'Mentoring students','Inspiration'),
             groupin = factor(c(rep('Academic growth', 3), rep('Personal growth', 2)), 
                              levels=c('Academic growth', 'Personal growth')),
             groupout = rep('Benefits to instructors', 5),
             respondent = paste0('Prospective', max(N))),
  on='choices']

q5_2format_summaryattri[, `:=`(formatlabels = factor(formatlabels, unique(formatlabels[order(varmean)])),
                               groupin = factor(groupin, 
                                                levels = unique(groupin)[order(-q5_2format_summaryattri[, mean(varmean), by=groupin]$V1)]))]

#---- Plot out ----
benefitplot_prospect <- ggplot(rbind(q5_1format_summaryattri, q5_2format_summaryattri), 
                               (aes(x=formatlabels, y=100*frac, fill=interaction(factor(value), groupout)))) +
  geom_bar(stat="identity", alpha=0.8) + 
  geom_text(aes(y = 5, label = paste0('Mean ', round(varmean,2))), hjust = 0) +
  #geom_bar(data = q5_2format_summaryattri[order(q5_2format_summaryattri$value, q5_2format_summaryattri$variable),], stat="identity", alpha=0.8) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(name= '% of responses', expand=c(0,0)) +
  scale_fill_manual(values=c(brewer.pal(4, "RdPu"), brewer.pal(4, "YlGnBu")), 
                    labels = paste(0:3, rep(rev(levels(q5_1format_summaryattri$i.value)),2))) + 
  ggtitle(paste0('Prospective, N=', max(q5_1format_summaryattri$N))) +
  coord_flip() +
  facet_grid(groupout*groupin~., scales ='free_y', space='free', switch='y') +
  #theme_ipsum() + 
  theme_minimal() +
  theme(text = element_text(size=16), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(color='black', size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.placement = 'outside',
        strip.text.y = element_text(face='bold'))

#pdf('results/benefits.pdf', width=6, height=8)
print(benefitplot_prospect)
#dev.off()
#Quick formatting in inkscape afterward

pdf('results/benefits_large.pdf', width=14, height=10)
grid.arrange(benefitplot, benefitplot_prospect, ncol=2)
dev.off()

################ Data availability ########################
#---- Q2.4 did you keep data? ----
ggplot(heedteach[!(get('Q2.4') %in% c('-99',-99,'')),], aes(x=Q2.4)) + 
  geom_histogram(stat="count") + 
  scale_x_discrete(expand=c(0,0), name='') + 
  scale_y_continuous(expand=c(0,0)) + 
  theme_classic()

#---- Row 1: Q18.3 - Do you currently share the data collected as part of this class?----
share_plot <- ggplot(heedteach[!(get('Q18.3') %in% c('-99',-99,'')),][, Q18.3 := factor(Q18.3, levels=c('Yes', 'No'))], 
                     aes(Q18.3, fill = Q18.3)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..)), alpha=0.8) + 
  scale_fill_manual(values=c('#80cdc1', '#dfc27d')) +
  scale_x_discrete(expand=c(0,0), name='') + 
  scale_y_continuous(expand=c(0,0), name='% of responses') + 
  theme_classic() + 
  theme(legend.position = 'none')
share_plot

#---- Row 2 Yes: Q18.4 - At what level(s) do you currently share your data?----
q18.4cols <- names(heedteach)[grep('Q18.4', names(heedteach))]
dat <- heedteach[, (q18.4cols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = q18.4cols]
check <- dat[, .SD, .SDcols = c('ResponseId', 'Q2.4', q18.4cols)] #Don't get why people said that they collected data, didn't keep them, and yet are currently sharing them. Ignore
q18.4melt <- melt(dat, id.vars='ResponseId', measure.vars=q18.4cols) %>%
  .[!(value %in% c('', NA)),] %>%
  .[ value =='Access through the national phenology network', 
     value := 'Open online access'] %>%
  .[ value =='Only for government agency partners and educational purpose', 
     value := 'Only for government partners and education']

q18.4melt[, value := factor(value, 
                            levels = rev(c('Open online access', 'Only for government partners and education',
                                           'Available upon request','With students on the course within and across years')))] #unique(value)[order(q18.4melt[,.N, by=value]$N)]


sharelevel_plot <- ggplot(q18.4melt[value != 'Other',.SD[which.max(value)], by = 'ResponseId'], aes(x=value)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), fill='#80cdc1', alpha=0.8) + 
  scale_x_discrete(expand=c(0,0), labels = function(x) {str_wrap(x, width=20)}) + 
  scale_y_continuous(expand=c(0,0), name='% of responses') + 
  coord_flip () +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

#% of each
q18.4melt[,.SD[which.max(value)], by = 'ResponseId'][
  ,{totwt = .N
  .SD[,.(frac=.N/totwt),by=value]}]

#---- Row 2 No: Q18.5 - What level(s) of data sharing would you be interested in, if any? ----
q18.5cols <- names(heedteach)[grep('Q18.5', names(heedteach))]
dat <- heedteach[, (q18.5cols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = q18.5cols]
check <- dat[, .SD, .SDcols = c('ResponseId', 'Q2.4', q18.5cols)]
q18.5melt <- melt(dat, id.vars='ResponseId', measure.vars=q18.5cols) %>%
  .[!(value %in% c('', NA)),] %>%
  .[ value =='Only for government agency partners and educational purpose', 
     value := 'Only for government partners and education'] %>%
  .[value %in% c('too busy at present; 1-2 y time',  
                 'I am interested in sharing it, but not sure how the other more senior instructors feel.',
                 'The data exists in student lab reports. Each report would have to be independantly combed through to recollect the data',
                 'Being used by other researchers at my institution and not up to me'), 
    value := 'Not able to share at the moment'] %>%
  .[value %in% c('The quality of the student-collected data is too poor to share the data.',
                 'Quality is not good enough to share.', 'I would not trust a good deal of the data collected.'), 
    value := 'Not interested due to insufficient data quality'] %>%
  .[value %in% c('Depending on the data usage','Depending on the agreement among instructors',
                 'Shared scientific publications', 'not applicable', 'Unclear'), 
    value := 'Other or not applicable'] %>%
  .[, value := factor(value, 
                      levels = rev(c('Not interested in data sharing for the moment', 'Not able to share at the moment', 
                                     'Open online access', 'Available upon request', 'Only for government partners and education',
                                     'Other or not applicable')))]
nosharelevel_plot <- ggplot(q18.5melt[value != 'Other',.SD[which.max(value)], by = 'ResponseId'], aes(x=value)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), fill='#dfc27d', alpha=0.8) + 
  scale_x_discrete(expand=c(0,0), labels = function(x) {str_wrap(x, width=20)}) + 
  scale_y_continuous(expand=c(0,0), name='% of responses') + 
  coord_flip () +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
nosharelevel_plot

q18.5melt[,.SD[which.max(value)], by = 'ResponseId'][
  ,{totwt = .N
  .SD[,.(frac=.N/totwt),by=value]}]


#---- Row 3 Yes: Q18.6 - Under what condition(s) do you share your data? ----
q18.6cols <- names(heedteach)[grep('Q18.6', names(heedteach))]
dat <- heedteach[, (q18.6cols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = q18.6cols]
check <- dat[, .SD, .SDcols = c('ResponseId', 'Q2.4', q18.6cols)]
q18.6melt <- melt(dat, id.vars='ResponseId', measure.vars=q18.6cols) %>%
  .[!(value %in% c('', NA)),] %>%
  .[, value := factor(value, 
                      levels = rev(c('Require authorship', 'Require citation', 'Require acknowledgment',
                                     'Depends on the data use', 'No requirement', 'Other', 'No one has asked yet!',
                                     'None for those released through public databases', 'Past years data shared with current years students',
                                     'To the Natural Hertiage Museum where collection data is stored.')))]
unique(q18.6melt$value)

sharecondition_plot <- ggplot(q18.6melt[,.SD[which.max(value)], by = 'ResponseId'], aes(x=value)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), fill='#80cdc1', alpha=0.8) + 
  scale_x_discrete(expand=c(0,0), labels = function(x) {str_wrap(x, width=20)}) + 
  scale_y_continuous(expand=c(0,0), name='% of responses') + 
  coord_flip () +
  theme_classic() +
  theme(axis.title = element_blank())
sharecondition_plot

#Get % of each
q18.6melt[,.SD[which.max(value)], by = 'ResponseId'][
  ,{totwt = .N
  .SD[,.(frac=.N/totwt),by=value]}]


#---- Row 3 No: Q18.7 - Under what condition(s) would you be interested in sharing your data, if any? ----
q18.7cols <- names(heedteach)[grep('Q18.7', names(heedteach))]
dat <- heedteach[, (q18.7cols) := lapply(.SD, function(x){x[x==-99] <- NA; x}), .SDcols = q18.7cols]
check <- dat[, .SD, .SDcols = c('ResponseId', 'Q2.4', q18.7cols)]
q18.7melt <- melt(dat, id.vars='ResponseId', measure.vars=q18.7cols) %>%
  .[!(value %in% c('', NA)),] %>%
  .[value == 'I don`t understand the question', value := NA] %>%
  .[, value := factor(value,
                      levels = rev(c('Require authorship', 'Require citation', 'Require acknowledgment',
                                     'Depends on the data use', 'No requirement', 'Other')))]
#unique(q18.7melt$value)

nosharecondition_plot <- ggplot(q18.7melt[,.SD[which.max(value)], by = 'ResponseId'], aes(x=value)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), fill='#dfc27d', alpha=0.8) + 
  scale_x_discrete(expand=c(0,0), labels = function(x) {str_wrap(x, width=20)}) + 
  scale_y_continuous(expand=c(0,0), name='% of responses') + 
  coord_flip () +
  theme_classic() +
  theme(axis.title = element_blank())
nosharecondition_plot 

#Get % of each
q18.7melt[,.SD[which.max(value)], by = 'ResponseId'][
  ,{totwt = .N
  .SD[,.(frac=.N/totwt),by=value]}]

#---- Row 4 Q18.8 - Would you be interested in an online community repository specifically designed to host class-based ecological datasets? ----
q18_8format <- likertformat(heedteach, heednumteach, 'Q18[.]8.*[^TEXT]$',diverging = TRUE)
q18_8format[, `:=`(share = ifelse(ResponseId %in% heedteach[Q18.3=='Yes', as.character(ResponseId)],'Yes', 'No'),
                   value = -value)]
q18_8format_summary <- q18_8format[!is.na(value) & value != '',{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=.(variable,share)]

length(which(q18_8format$value > 0))/132

q18_8format_summaryattri <- q18_8format_summary[unique(q18_8format[,.(variable, value, i.value, choices, varmean, N, share)]),
                                                on= c('variable','value','share'), nomatch=0] %>%
  .[, frac2 := ifelse(value == 0, frac/2, frac)]  %>%
  .[, i.value := factor(i.value, levels=c("Probably yes","Definitely yes","Might or might not","Probably not"))]

up <- q18_8format_summaryattri[value >= 0 & choices != ' - Other (please specify)',] %>%
  .[, value := factor(value, levels= unique(value)[order(value)])]
down <- q18_8format_summaryattri[value <= 0 & choices != ' - Other (please specify)',] %>%
  .[, value := factor(value, levels= unique(value)[order(-value)])]

onlinerepo <- ggplot(q18_8format_summaryattri[order(q18_8format_summaryattri$value, q18_8format_summaryattri$variable),]) + 
  geom_hline(yintercept=0) +
  geom_bar(data = up,aes(x = share, y = 100*frac2, fill = interaction(share, value)),
           stat = "identity", position = position_stack(reverse = TRUE), alpha=0.8) + 
  geom_bar(data = down,aes(x = share, y = -100*frac2, fill = interaction(share, factor(value, levels=c('-1.5', '-0.5')))),
           stat = "identity", position = position_stack(reverse = TRUE), alpha=0.8) +
  scale_x_discrete(name = 'Currently sharing') +
  scale_y_continuous(name= '% of responses', expand=c(0,0), limits=c(-100,100), breaks=seq(-100,100,25),labels=commapos) +
  scale_fill_manual(values=c('#543005', '#8c510a', '#bf812d', '#dfc27d', '#01665e','#35978f', '#80cdc1'), 
                    labels = rep(rev(levels(q18_8format_summaryattri$i.value)),2)) + 
  theme_classic() + 
  coord_flip() +
  theme(legend.title = element_blank(),
        legend.position = c(0.3,0.5))
onlinerepo

#---- Plot it out ----
datagrobs <- list(arrangeGrob(share_plot, top='Do you currently share the data collected as part of this class?'),
                  textGrob("Level of sharing"),
                  arrangeGrob(sharelevel_plot), arrangeGrob(nosharelevel_plot),
                  textGrob("Conditions of use"),
                  arrangeGrob(sharecondition_plot), arrangeGrob(nosharecondition_plot),
                  textGrob("Interest in online community repository"),
                  arrangeGrob(onlinerepo))
lay <- rbind(c(NA, 1, 1,NA),
             c(NA, 2, 2,NA),
             c(3, 3, 4, 4),
             c(NA, 5, 5,NA),
             c(6, 6, 7, 7),
             c(NA, 8, 8,NA),
             c(NA, 9, 9,NA))

pdf('results/sharing.pdf', width=6, height=9)
grid.arrange(grobs = datagrobs, layout_matrix = lay, heights=unit(c(8,1,10,1,10,1,8), rep('null', 5)))
dev.off()


################ Data vulnerability ########################
#---- Q18.2 - How are/were data stored for purposes other than the class? ----
check <- dcast(multiformat(heedteach, 'Q18[.]2.*(TEXT)*$'), ResponseId~variable, var.name = value) #No need to include other

q18.2melt <- multiformat(heedteach, 'Q18[.]2.*[^TEXT]$') %>%
  .[data.frame(value=unique(q18.2melt$value),
               formatlabels = factor(c('Hard copies', NA, 'Local computer storage', 'Restricted cloud-based', 'Public respository', 
                                       'None, samples not analyzed yet','None, data not archived', 'Other'),
                                     levels = rev(c('Public respository',  'Restricted cloud-based', 'Local computer storage', 
                                                    'Hard copies', 'None, data not archived', 'None, samples not analyzed yet', 'Other', NA)))),
    on = 'value']


storageplot <- ggplot(q18.2melt[!is.na(value) & !(value %in% c('Other','','-99')), .SD[which.max(formatlabels)], by = 'ResponseId'],
                      aes(x=formatlabels)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..)), alpha=0.8) + 
  scale_x_discrete(expand=c(0,0), labels=function(x) {str_wrap(x, width=20)}) + 
  scale_y_continuous(expand=c(0,0), '% of responses') + 
  coord_flip() +
  theme_classic() + 
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color='lightgray'))
png('results/storage.png', width=4, height=4, unit='in', res=600)
storageplot
dev.off()
