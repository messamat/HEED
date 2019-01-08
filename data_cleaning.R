#+ fig.width=10, fig.height=6, dpi=300, out.width="1920px",out.height="1080px"
library(ggplot2)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(data.table)
library(arsenal)
library(stringr)
library(sp)
library(leaflet)
library(knitr)
library(kableExtra)
library(ggrepel)

#Import data
setwd('C:/Mathis/SAFS/HEED') ##UPDATE##
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
length(heededit[heededit$Q20.2_1 != '-99', 'Q20.2_1']) # of respondents that entered their country
length(unique(heededit[heededit$Q20.2_1 != '-99', 'Q20.2_1'])) #number of unique countries

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

#subset rows and columns in numeric results to match the text version (heedteach and heednoteach)
heednumteach <- setDT(heednum)[ResponseId %in% heedteach$ResponseId,
                               colnames(heednum) %in% colnames(heedteach), with=FALSE] 
heednumnoteach <- setDT(heednum)[ResponseId %in% heednoteach$ResponseId,
                                 colnames(heednum) %in% colnames(heednoteach), with=FALSE] 

#_________________________________________________________________________
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

likertformat <- function(datatext, datanum, pattern, diverging=FALSE, qstext=qs) {
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
  qslikert <- data.frame(choices = tstrsplit(qstext[rowind, '1'], '?', fixed=T)[[2]])
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
  g$heights <- grid:::unit.list(g$heights)
  g$heights[idpanels] <- unit.c(do.call(unit, list(heights, 'null')))
  g
}

resize_widths <- function(g, widths = rep(1, length(idpanels))){
  idpanels <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g$widths <- grid:::unit.list(g$widths)
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
heedteach[, c('lat', 'long') := tstrsplit(X1_Q11.3, ',', fixed=T),]
heedteach[, long := as.numeric(str_extract(long, "[-+]?\\d+(\\.\\d+)?")),]
heedteach[, lat := as.numeric(str_extract(lat, "[-+]?\\d+(\\.\\d+)?")),]
heedteach[lat==-99 | long==-99, `:=`(lat=NA, long=NA)]


flocs <- SpatialPoints(data.frame(heedteach[!is.na(long),long],heedteach[!is.na(long),lat]))
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

################ Q15.4 - How many peer-reviewed publications have resulted from the data collected in this class?  ########
print(histoplot(data=heedteach, col='Q15.4_1', unit='Publications', binw=1))

################ Q15.5 - How many peer-reviewed publications were the students involved in?  ########
check <- heedteach[Q15.5_1 %in% c(0,-99, NA),]
heedteach[Q15.4_1>0 & is.na(Q15.5_1), Q15.5_1 := 11]
heedteach[Q15.5_1 %in% c(-99,11), flag:=flag+1]
heedteach[Q15.5_1==-99, Q15.5_1 := NA]

print(histoplot(data=heedteach, col='Q15.5_1', unit='Publications', binw=1))

################ Q15.6 - Have these data resulted in subsequent grants or projects?  ########
singleAplot(heedteach, 'Q15.6')

################ Q15.7 - Have these data been used by local or national government agencies?  ########
multiAhisto(heedteach, pattern= 'Q15[.]7.*[^TEXT]$', xaxis = 'Uses') 

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

################ Q16.2 - In your experience, what are the main challenges to implementing and maintaining class-based data collection? ########
q16_2format <- likertformat(heedteach, heednumteach, 'Q16[.]2.*[^TEXT]$',diverging = TRUE)
likertstackedbar(q16_2format, diverging=TRUE)

################ Q16.3 - Do you have additional comments or advice from your experience implementing and maintaining class-based data collection? ########
kable(heedteach[!(Q16.3 %in% c('','-99')),'Q16.3', with=FALSE], 
      caption = paste0(tstrsplit(qs[rownames(qs) == 'Q16.3', 1], '?', fixed=T)[[1]],'?')) %>%
  kable_styling(bootstrap_options = c("striped", "hover","responsive"), full_width=T)

################ Q16.4 - In your experience, what are the main challenges to analyzing data collected as part of field excursions?########
multiAhisto(heedteach, pattern= 'Q16[.]4.*[^TEXT]$', xaxis = '') 

################ Q16.5 - For what percentage of the study period do gaps in data collection exist?  ###########
singleAplot(heedteach, col='Q16.5')

################ Q16.6 - What are the main reasons for these gaps in data collection?  ###########
multiAhisto(heedteach, pattern= 'Q16[.]6.*[^TEXT]$', xaxis = '') 

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

################ Q18.3 - Do you currently share the data collected as part of this class? ####
singleAplot(heedteach, 'Q18.3')

################ Q18.4 - At what level(s) do you currently share your data? ####
multiAhisto(heedteach, 'Q18.4', xaxis='Choice')

################ Q18.5 - What level(s) of data sharing would you be interested in, if any? ####
multiAhisto(heedteach, 'Q18.5', xaxis='Choice')

################ Q18.6 - Under what condition(s) do you share your data? ####
multiAhisto(heedteach, 'Q18.6', xaxis='Choice')

################ Q18.7 - Under what condition(s) would you be interested in sharing your data, if any? ####
multiAhisto(heedteach, 'Q18.7', xaxis='Choice')

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
################ Q2.5_1 and Q2.6_1 - In what year did data collection start in the context of this class?  When was the last year of data collection, if applicable? ################
heedteach[is.na(Q2.6_1), Q2.6_1 := 2019]
heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, dataduration := Q2.6_1-Q2.5_1]

#---- Histogram of duration ----
setDT(heedteach)
binw=1
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
  geom_vline(xintercept=medcol) + 
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
  scale_color_distiller(palette='Spectral', name='Class duration') + 
  scale_x_continuous(expand=c(0,-0.5), name='Start and end years of data collection') + 
  # ggtitle(paste('Teaching Q2.5 and 2.6: In what year did data collection start and end. Number of respondents:', 
  #               heedteach[Q2.5_1 != -99 & Q2.6_1 != -99, .N], '/', heedteach[, .N])) + 
  annotation_custom(ggplotGrob(durahist), 
                    xmin=1960, xmax=1990, 
                    ymin=quantile(heedteach[,as.numeric(ResponseId)], .50), 
                    ymax=quantile(heedteach[,as.numeric(ResponseId)], 0.95)) + 
  theme_classic() +
  theme(legend.position=c(0.25, 0.05),
        legend.direction="horizontal",
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color='lightgrey'),
        panel.grid.minor.x = element_line(color='lightgrey'))

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
png('results/startendyears.png', width=6, height=6, units='in', res=400)
grid.draw(resize_widths(g, c(20,1)))
dev.off()

################ Compute the total number of students or days involved ##########
#Number of students * number of days * number of times a year * number of years
heedteach_sub <- heedteach[data.frame(Q8.3=levels(heedteach[,Q8.3]), 
                                      Q8.3num = c(0,0,2,0.5,1/3,1)), on='Q8.3'][
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
bioqcols <- names(heedteach)[grep('Q12.3', names(data))]
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
  geom_histogram(stat="count", fill='#addd8e') + 
  scale_x_discrete(expand=c(0,0), labels = str_wrap(unique(new), width=10), name= 'Data type') + 
  scale_y_continuous(expand=c(0,0)) + 
  annotation_custom(ggplotGrob(biopie), 
                    ymin=50, ymax=135, xmin = 5, xmax = 6) + 
  # ggtitle(paste(title, '? Number of respondents: ', 
  #               length(bioqmelt[!is.na(value) & !(value %in% c('','-99')), unique(ResponseId)]), '/', data[, .N])) + 
  theme_classic() +
  theme(axis.title= element_blank())
#biovarplot


#---- Physical ----
phyqcols <- names(heedteach)[grep('Q12.4', names(data))]
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
  geom_histogram(stat="count", fill='#fed976') + 
  scale_x_discrete(expand=c(0,0), labels = str_wrap(levels(phyqmeltord$value), width=8), name= 'Data type') + 
  scale_y_continuous(expand=c(0,0)) + 
  annotation_custom(ggplotGrob(physpie), 
                    ymin=40, ymax=95, xmin = 11, xmax = 15) + 
  # ggtitle(paste(title, '? Number of respondents: ', 
  #               length(phyqmelt[!is.na(value) & !(value %in% c('','-99')), unique(ResponseId)]), '/', data[, .N])) + 
  theme_classic()+
  theme(axis.title = element_blank())
#physvarplot

#---- Chemical ----
cheqcols <- names(heedteach)[grep('Q12.5.*(?<!TEXT)$', names(data), perl=T)]
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
  geom_histogram(stat="count", fill='#a6bddb') + 
  scale_x_discrete(expand=c(0,0), labels = str_wrap(levels(cheqmeltord$value), width=5), name= 'Data type') + 
  scale_y_continuous(expand=c(0,0)) + 
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

#---- Compute # of variable type combinations ----
varnjoin <- merge(bioqmeltord[!is.na(value),list(biovarn = .N),ResponseId], phyqmeltord[!is.na(value),list(phyvarn = .N),ResponseId], 
                  on='ResponseId', all.x=T, all.y=T) %>%
  merge(cheqmeltord[!is.na(value),list(chemvarn = .N),ResponseId], on='ResponseId', all.x=T, all.y=T)
varncols <- c('biovarn', 'phyvarn', 'chemvarn')
varnjoin[,(varncols) := lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), .SDcols = varncols][
  ,combination := paste0(biovarn, phyvarn, chemvarn)]
table(varnjoin$combination)
varnjoin[,.N]
          



################ Q16.2 - Main challenges to implementing and maintaining class-based data collection? ########
#---- Format data ----
q16_2format <- likertformat(heedteach, heednumteach, 'Q16[.]2.*[^TEXT]$',diverging = TRUE)
q16_2format_summary <- q16_2format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q16_2format_summaryattri <- q16_2format_summary[unique(q16_2format[,.(variable, value, i.value, choices, varmean, N)]),
                                                on= c('variable','value'), nomatch=0] %>%
  .[, frac2 := ifelse(value ==0, frac/2, frac)]


up <- q16_2format_summaryattri[value >= 0 & choices != ' - Other (please specify)',] %>%
  .[, value := factor(value, levels= unique(value)[order(value)])]
down <- q16_2format_summaryattri[value <= 0 & choices != ' - Other (please specify)',] %>%
  .[, value := factor(value, levels= unique(value)[order(value)])]

challengelabels <- gsub('\\s*\\([^\\)]+\\)|^\\s*\\-\\s*', '', 
                        unique(q16_2format[choices != ' - Other (please specify)', choices])) #format labels

#Inspect 'Others'
heedteach[!(Q19.1 %in% c('','-99')),"Q16.2_8_TEXT", with=FALSE]

#---- Plot out ----
challengeplot <- ggplot(q16_2format_summaryattri[order(q16_2format_summaryattri$value, q16_2format_summaryattri$variable),]) + 
  geom_bar(data = up, aes(x = variable, y = 100*frac2, fill = value),
           stat = "identity", position = position_stack(reverse = TRUE)) + 
  geom_bar(data = down, aes(x = variable, y = -100*frac2, fill = value),
           stat = "identity", position = position_stack()) +
  scale_x_discrete(name = 'Challenge', labels = str_wrap(challengelabels, width=10)) +
  scale_y_continuous(name= '% of responses', expand=c(0,0), limits=c(-100,100), breaks=seq(-100,100,25),labels=commapos) +
  theme_classic() + 
  coord_flip() +
  scale_fill_manual(values = c('#dfc27d', '#a6611a', '#f5f5f5', '#80cdc1', '#018571'), 
                    labels= unique(q16_2format_summaryattri[order(value),]$i.value)) +
  theme(legend.title = element_blank(),
        #axis.title.y =  element_blank(),
        #axis.title.x =  element_blank(),
        panel.grid.major.x = element_line(color='lightgrey'),
        legend.position = c(0.15, 0.15),
        legend.background = element_blank(),
        text= element_text(size=12))
challengeplot

png('results/challenges.png', width=6, height=6, units='in', res=600)
print(challengeplot)
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
  .[data.frame(choices = unique(q15_2format_summaryattri[choices != ' - Other', choices]),
               formatlabels = c('Field sampling', 'Lab. methods', 'Data analysis', 
                             'Collaborative research','Scientific writing', 'Public speaking', 'Scientific process',
                             'Networking w/ scientists','Relationship building w/ classmates', 'Increased topical interest', 'Awareness of nature'),
               groupin = factor(c(rep('Hard skills', 3), rep('Soft skills', 4), rep('Personal growth', 4)), 
                              levels=c( 'Personal growth', 'Hard skills', 'Soft skills')),
               groupout = rep('Benefits to students', 11)),
               on='choices']

q15_2format_summaryattri[, `:=`(formatlabels = factor(formatlabels, unique(formatlabels[order(varmean)])))]
#,groupin = factor(groupin,levels = unique(groupin)[order(-q15_2format_summaryattri[, mean(varmean), by=groupin]$V1)])

#---- format data for instructors' benefits ----
q15_8format <- likertformat(heedteach, heednumteach, 'Q15[.]8.*[^TEXT]$')
q15_8format_summary <- q15_8format[!is.na(value),{
  tot = .N
  .SD[,.(frac=.N/tot),by=value]
},by=variable]
q15_8format_summaryattri <- q15_8format_summary[unique(q15_8format[,.(variable, value, i.value, choices, varmean, N)]),
                                                on= c('variable','value'), nomatch=0] %>%
  .[data.frame(choices = unique(q15_8format_summaryattri[choices != ' - Other (please specify)', choices]),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width=15)) +
  scale_y_continuous(name= '% of responses', expand=c(0,0)) +
  scale_fill_manual(values=c(brewer.pal(4, "RdPu"), brewer.pal(4, "YlGnBu")), 
                    labels = rep(rev(levels(q15_2format_summaryattri$i.value)),2)) + 
  coord_flip() +
  facet_grid(groupout*groupin~., scales ='free_y', space='free', switch='y') +
  theme_minimal() + 
  theme(text = element_text(size=12), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(color='black', size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.placement = 'outside',
        strip.text.y = element_text(face='bold'))

pdf('results/benefits.pdf', width=6, height=8)
print(benefitplot)
dev.off()
#Quick formatting in inkscape afterward
