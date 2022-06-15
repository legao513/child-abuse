library(readxl)
library(dplyr)
library(lubridate)
#MPH cohort
mph9320 <- readRDS("M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/mph9320.rds")
death <- read_xlsx ("M:/Cohort Raw Data (do not edit)/TIMESPAN/2.CONVERT/3.ADHD_MPH_DEATH/5979156_mph_adhd_9320cohort_birth_death_20210505(download_all_files_please_COM_conv.xlsx")
colnames(death) <- make.names(colnames(death))


tast <- afinal1 %>% 
  mutate(md=substring(DOB,6,10)) %>% 
  filter(md=="02-29")

#sccs
a <- merge(mph9320,death,by="Reference.Key.",all.x = T) %>% 
  mutate(Reference.Key.=as.numeric(Reference.Key.)) %>% 
  mutate(Rxst=as.Date(Prescription.Start.Date.),Rxed = as.Date(Prescription.End.Date.)) %>%
  mutate(disdt = as.Date(Dispensing.Date..yyyy.mm.dd..),
         DOD = as.Date(Date.of.Registered.Death.),
         Date.of.Birth..yyyy.mm.dd..=as.Date(Date.of.Birth..yyyy.mm.dd..),
         md=substring(Date.of.Birth..yyyy.mm.dd..,6,10),
         DOB1 = ifelse(md=="02-29",as.character(paste0(year(Date.of.Birth..yyyy.mm.dd..),"-02-28")),as.character(Date.of.Birth..yyyy.mm.dd..)),
         DOB=as.Date(DOB1))


#keep the first record of any HA records (cohort, all dx, all rx and ae)
#cohort
mph_rx_first <- a %>% 
  arrange(Reference.Key.,disdt) %>% 
  filter(duplicated(Reference.Key.)==F) %>% 
  select(Reference.Key.,disdt) %>% 
  `colnames<-` (c('Reference.Key.','HA_record'))

#all_dx

#find the first diagnosis record in database all dx
# path <- "M:/Cohort Raw Data (do not edit)/TIMESPAN/2.CONVERT/4.ADHD_MOH_alldx"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# adhd_mph_alldx <- do.call(rbind.data.frame, All)
# colnames(adhd_mph_alldx) <- make.names(colnames(adhd_mph_alldx))
# adhd_mph_alldx <- adhd_mph_alldx %>% 
#   mutate(Reference.Key.=as.numeric(Reference.Key.)) %>% 
#   mutate(admdt=as.Date(Reference.Date.))
# 
# saveRDS(adhd_mph_alldx,"M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_alldx.rds")
adhd_mph_alldx <- readRDS("M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_alldx.rds")

alldx_first <- adhd_mph_alldx %>% 
  arrange(Reference.Key., admdt) %>% 
  filter(duplicated(Reference.Key.) == FALSE) %>% 
  select(Reference.Key.,admdt) %>% 
  `colnames<-` (c('Reference.Key.','HA_record'))


#find the first diagnosis record in database ip

# path <- "M:/Cohort Raw Data (do not edit)/TIMESPAN/2.CONVERT/5.ADHD_MPH_ip"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# adhd_mph_ip <- do.call(rbind.data.frame, All)
# colnames(adhd_mph_ip) <- make.names(colnames(adhd_mph_ip))
# adhd_mph_ip1 <- adhd_mph_ip %>%
#   mutate(Reference.Key.=as.numeric(Reference.Key.)) %>%
#   mutate(Admission.Date..yyyy.mm.dd..=as.Date(Admission.Date..yyyy.mm.dd..))
# 
# saveRDS(adhd_mph_ip1,"M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_ip.rds")
adhd_mph_ip1 <- readRDS("M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_ip.rds")

ip_first <- adhd_mph_ip1 %>% 
  arrange(Reference.Key., Admission.Date..yyyy.mm.dd..) %>% 
  filter(duplicated(Reference.Key.) == FALSE) %>% 
  select(Reference.Key.,Admission.Date..yyyy.mm.dd..) %>% 
  `colnames<-` (c('Reference.Key.','HA_record'))


#find the first diagnosis record in database all rx

# path <- "M:/Cohort Raw Data (do not edit)/TIMESPAN/2.CONVERT/6.ADHD_MPH_allrx"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# adhd_mph_allrx <- do.call(rbind.data.frame, All)
# colnames(adhd_mph_allrx) <- make.names(colnames(adhd_mph_allrx))
# adhd_mph_allrx1 <- adhd_mph_allrx %>%
#   mutate(Reference.Key.=as.numeric(Reference.Key.)) %>%
#   mutate(Dispensing.Date..yyyy.mm.dd..=as.Date(Dispensing.Date..yyyy.mm.dd..))
# 
# saveRDS(adhd_mph_allrx1,"M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_allrx.rds")
adhd_mph_allrx1 <- readRDS("M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_allrx.rds")

allrx_first <- adhd_mph_allrx1 %>% 
  arrange(Reference.Key., Dispensing.Date..yyyy.mm.dd..) %>% 
  filter(duplicated(Reference.Key.) == FALSE) %>% 
  select(Reference.Key.,Dispensing.Date..yyyy.mm.dd..) %>% 
  `colnames<-` (c('Reference.Key.','HA_record'))




#find the first diagnosis record in database ae

# path <- "M:/Cohort Raw Data (do not edit)/TIMESPAN/2.CONVERT/7.ADHD_MPH_ae"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# adhd_mph_ae <- do.call(rbind.data.frame, All)
# colnames(adhd_mph_ae) <- make.names(colnames(adhd_mph_ae))
# adhd_mph_ae1 <- adhd_mph_ae %>%
#   mutate(Reference.Key.=as.numeric(Reference.Key.)) %>%
#   mutate(Attendance.Date..yyyy.mm.dd..=as.Date(Attendance.Date..yyyy.mm.dd..))
# 
# saveRDS(adhd_mph_ae1,"M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_ae.rds")
adhd_mph_ae1 <- readRDS("M:/Cohort Raw Data (do not edit)/TIMESPAN/3.combine/adhd_mph_ae.rds")

ae_first <- adhd_mph_ae1 %>% 
  arrange(Reference.Key., Attendance.Date..yyyy.mm.dd..) %>% 
  filter(duplicated(Reference.Key.) == FALSE) %>% 
  select(Reference.Key.,Attendance.Date..yyyy.mm.dd..) %>% 
  `colnames<-` (c('Reference.Key.','HA_record'))


#combine first records from cohort, alldx, allrx, ae and ip, and find the first HA records
HA_first <- rbind(mph_rx_first,alldx_first,ip_first,allrx_first,ae_first) %>% 
  arrange(Reference.Key.,HA_record) %>% 
  filter(duplicated(Reference.Key.)==F) %>% 
  filter(Reference.Key.%in%a$Reference.Key.)


a0_1 <- a %>% 
  filter(duplicated(Reference.Key.) == FALSE) %>%
  mutate(age5 = (ymd(DOB)+years(5)),age18end = (ymd(DOB)+years(17)-days(1)),
         age5=if_else(is.na(age5),ymd(DOB)-days(1)+years(5),age5),
         age18end=if_else(is.na(age18end),ymd(DOB)-days(1)+years(17),age18end)) %>% 
  select(Reference.Key.,DOD, age5, age18end)

#generate the observation start and end date
abc2 <- merge(a0_1,HA_first,by="Reference.Key.") %>% 
  mutate(s=ymd('2001-01-01'),e=ymd('2020-12-31')) %>% 
  mutate(sdate = pmax(HA_record,s,age5,na.rm = TRUE)) %>% 
  mutate(edate = pmin(DOD,e,age18end,na.rm = TRUE)) %>% 
  select(Reference.Key., sdate, edate) %>% 
  filter(sdate < edate)









a11 <- merge(a,abc2,by ="Reference.Key.",all.x=T) 

length(unique(a11$Reference.Key.))



#5 find out missing Rxed

a3 <- a11 %>%
  mutate(Rxst=if_else(is.na(Rxst),disdt,Rxst))



length(unique(a3$Reference.Key.))
table(factor(a3$Drug.Frequency.)) %>% as.data.frame()

#see the frequency of each unique variable
#judge whether Rxed is NA and then seperate it into 2 tabels, a4_1 is observations with Rxed
a3_1 <- a3 %>% filter(is.na(Rxed))
a3_2 <- a3 %>% filter(!is.na(Rxed))
table(factor(a3_1$Drug.Frequency.)) %>% as.data.frame()
table(factor(a3_1$Dosage.)) %>% as.data.frame()
#transfer the drug frequency into character
a3_1$Drug.Frequency.Fix <- 1
#assign numbers to text
# a3_1$Drug.Frequency.Fix [which(grepl("NECESSARY",a3_1$Drug.Frequency.)==F)] <- NA

a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("DAILY FOR 5 DAYS IN A WEEK","FIVE TIMES WEEKLY"))] <- 5/7
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("IN THE MORNING AND [0.5] AT NOON","IN THE MORNING AND [0.5] AT NIGHT","IN THE MORNING AND [0.5] IN THE AFTERNOON"))] <- 1.5
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("IN THE MORNING AND [1] AT NOON","IN THE MORNING AND [1] IN THE AFTERNOON","IN THE MORNING AND [1] AT NIGHT","TWICE DAILY"))] <- 2
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("IN THE MORNING AND [1.5] AT NIGHT","IN THE MORNING AND [1.5] AT NOON"))] <- 2.5
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("THREE TIMES DAILY","IN THE MORNING AND [2] AT NOON","IN THE MORNING AND [2] AT NIGHT","IN THE MORNING AND [2] AT NOON","IN THE MORNING AND [2] IN THE AFTERNOON"))] <- 3
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("IN THE MORNING AND [2.5] AT NOON"))] <- 3.5
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("FOUR TIMES DAILY"))] <- 4
a3_1$Drug.Frequency.Fix [which(a3_1$Drug.Frequency. %in% c("AS DIRECTED","IN THE MORNING AND ___ AT NOON"))] <- NA
a3_1$Drug.Frequency.Fix [which(grepl("WHEN NECESSARY",a3_1$Drug.Frequency.)==T)] <- NA
a3_1$Drug.Frequency.Fix [which(is.na(a3_1$Drug.Frequency.))] <- NA

#assign numbers to dosage
a3_1$dosagefix <- stri_extract_first_regex(a3_1$Dosage.,"\\d+\\.*\\d*")
table(a3_1$dosagefix)
table(a3_1$Dosage.)




#calculate the daily dosage
a3_1_1 <- a3_1 %>% 
  mutate(dailydose=as.numeric(as.numeric(dosagefix)*Drug.Frequency.Fix)) %>% 
  mutate(duration1=as.numeric(Quantity..Named.Patient..)/as.numeric(dailydose)) %>% #gengerate the duration by using (Dispensing_Quantity__Named_Patie)/(daily dosage)
  mutate(Rxed = ymd(Rxst) + days(ceiling(duration1)),
         note=1)#calcute the Rxed by using Rxst + duration1





#combine two dataset a4_1 and a4_2
a3_2_1 <- a3_2 %>% 
  mutate(Drug.Frequency.Fix=NA,dosagefix=NA,dailydose=NA) %>% 
  mutate(duration1 = as.numeric(difftime(Rxed, Rxst, units="days")),
         note=NA)
a3new <- rbind(a3_1_1, a3_2_1) 

#split the data into 2 set (full data of duration and without duration)
a4new_1 <- a3new %>% filter(note==1,is.na(Rxed)|duration1==0)
a4new_2 <- a3new %>% filter(is.na(note)|(note==1&!is.na(Rxed)&duration1!=0))
table(factor(a4new_1$Drug.Item.Code.)) %>% as.data.frame()
table(factor(a4new_2$Drug.Item.Code.)) %>% as.data.frame()
median(a4new_2$duration1[which(a4new_2$Drug.Item.Code.=="METH27")])#83
median(a4new_2$duration1[which(a4new_2$Drug.Item.Code.=="MODA01")]) #55

a4new_1$duration1[which(a4new_1$Drug.Item.Code.=="METH27")] <- 83
a4new_1$duration1[which(a4new_1$Drug.Item.Code.=="MODA01")] <- 55

a4new_1$Rxed <- a4new_1$Rxst + days(a4new_1$duration1)
a5 <- rbind(a4new_1, a4new_2)

a9 <- a5 %>% 
  filter(Rxst<=Rxed) %>% 
  arrange(Reference.Key.,Rxst)

length(unique(a9$Reference.Key.))


#7 if individuals first use MPH then use ATX, then the end date of follow up will be the first use of ATX----
a9_2 <- a9 %>% 
  mutate(Drug.Name.=toupper(Drug.Name.),
         drug_id = ifelse(grepl('METH',Drug.Name.)==T,2,1)) %>% 
  arrange(Reference.Key.,Rxst,drug_id) %>% 
  filter(duplicated(Reference.Key.) == FALSE,
         drug_id==2)

a9_3 <- a9 %>% 
  filter(Reference.Key. %in% a9_2$Reference.Key.)#first rx is MPH

a9_4_2 <- filter(a9_3,grepl('METH',Drug.Name.)== T)

a9_4_1 <- a9_3 %>% 
  filter(grepl('METH',Drug.Name.)==F) %>% 
  arrange(Reference.Key.,Rxst) %>% 
  filter(duplicated(Reference.Key.)==F) %>% #select the first record of non-MPH
  select(Reference.Key.,Rxst) %>% 
  `colnames<-` (c('Reference.Key.','cens'))

a9_5 <- merge(a9_4_2,a9_4_1,by='Reference.Key.', all.x = TRUE) %>% 
  mutate(edate1=pmin(cens,edate,na.rm = T))

#abuse 
adhd_mph_ip2 <- adhd_mph_ip1 %>% 
  mutate(admdt=as.Date(Admission.Date..yyyy.mm.dd..)) %>% 
  select(Reference.Key.,admdt,Dx.Px.code.) %>% 
  `colnames<-` (c('Reference.Key.','admdt','All.Diagnosis.Code..ICD9..')) %>% 
  mutate(Patient.Type..IP.OP.A.E..="I")

adhd_mph_alldx1 <- adhd_mph_alldx %>% 
  filter(Patient.Type..IP.OP.A.E..!="I") %>% 
  select(Reference.Key.,admdt,All.Diagnosis.Code..ICD9..,Patient.Type..IP.OP.A.E..)

bc1_1 <- rbind(adhd_mph_ip2,adhd_mph_alldx1) %>% 
  filter(grepl('^E967|^995.54',All.Diagnosis.Code..ICD9..)==T,
         Reference.Key.%in%a9_5$Reference.Key.) %>% 
  arrange(Reference.Key.,admdt) %>% 
  mutate(du_note=duplicated(Reference.Key.))


bc1_2 <- merge(bc1_1,a9_5 %>% filter(duplicated(Reference.Key.)==F) %>% select(Reference.Key.,sdate,edate1),
               by="Reference.Key.") %>% 
  filter(admdt>=sdate,admdt<=edate1)


afinal_2 <- a9_5 %>% 
  filter(Rxst<=edate1,Rxed>=sdate) %>%
  # filter(admdt>=sdate,admdt<=edate1) %>% 
  mutate(Rxst=if_else(Rxst<sdate,sdate,Rxst)) %>% 
  mutate(Rxed=if_else(Rxed>edate1,edate1,Rxed))
length(unique(afinal_2$Reference.Key.))






#baseline dosage
table(dosage_data$mg)

dosage_data <- afinal_2 %>% 
  select(Reference.Key.,Drug.Frequency.,Drug.Frequency.Fix,Dosage.,Sex.,Drug.Strength.,Rxst,Rxed)

library(stringi)
dosage_data$tabno <- stri_extract_first_regex(dosage_data$Dosage.,"\\d+\\.*\\d*")
dosage_data$mg <- stri_extract_first_regex(dosage_data$Drug.Strength.,"\\d+\\.*\\d*")


table(dosage_data$Drug.Frequency.)
dosage_data$fre1 <- 1
dosage_data$fre1[which(dosage_data$Drug.Frequency. %in% c("FIVE TIMES WEEKLY","IN THE MORNING (5 DAYS PER WEEK)"))] <- 5/7
dosage_data$fre1[which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING (1 DAYS PER WEEK)"))] <- 1/7
dosage_data$fre1[which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING (2 DAYS PER WEEK)"))] <- 2/7
dosage_data$fre1[which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING (6 DAYS PER WEEK)"))] <- 6/7
dosage_data$fre1 [which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING AND [0.5] AT NOON","IN THE MORNING AND [0.5] IN THE AFTERNOON"))] <- 1.5
dosage_data$fre1 [which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING AND [1] AT NOON","IN THE MORNING AND [1] IN THE AFTERNOON"))] <- 2
dosage_data$fre1[which(grepl("TWICE DAILY",dosage_data$Drug.Frequency.))] <- 2
dosage_data$fre1 [which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING AND [1.5] AT NOON","IN THE MORNING AND [1.5] IN THE AFTERNOON"))] <- 2.5
dosage_data$fre1 [which(dosage_data$Drug.Frequency. %in% c("IN THE MORNING AND [2] AT NOON","THREE TIMES DAILY","THREE TIMES DAILY (5 DAYS PER WEEK)","THREE TIMES DAILY (DAYS PER WEEK)"))] <- 3
dosage_data$fre1[which(dosage_data$Drug.Frequency. %in% c("AS DIRECTED","AT ONCE","ONCE"))] <- NA
dosage_data$fre1[which(grepl("NECESSARY|DIRECTED",dosage_data$Drug.Frequency.))] <- NA


dosage_data$dosage_cal <- as.numeric(dosage_data$tabno)*as.numeric(dosage_data$mg)*as.numeric(dosage_data$fre1)




#8 solve the overlapping of exposure time----
aoverlapping <- dosage_data %>%
  arrange(Reference.Key., Rxst, Rxed,dosage_cal) %>% # as suggested by @Jonno in case the data is unsorted
  group_by(Reference.Key.) %>%
  mutate(indx = c(0, cumsum(as.numeric(lead(Rxst)-1) >
                              cummax(as.numeric(Rxed)))[-n()])) %>%
  group_by(Reference.Key., indx) %>%
  summarise(start = first(Rxst), end = max(Rxed),dose_combine=last(dosage_cal))









#combine basic information with afinaloverlapping
a10_2 <- afinal_2 %>% 
  select(Reference.Key.,DOB,sdate,edate1,Drug.Name.,Dosage.,Drug.Frequency.,Drug.Strength.,DOD,Sex.) %>% 
  filter(duplicated(Reference.Key.) == FALSE)
afinal <- merge(aoverlapping,a10_2,by="Reference.Key.",all.x = TRUE)
colnames(afinal)[3] <- 'Rxst'
colnames(afinal)[4] <- 'Rxed' 
length(unique(afinal$Reference.Key.))#1124






#9 if Rxst is earlier than sdate, then use sdate as 1st Rxst----
afinalb <- afinal %>% 
  # filter(admdt >= sdate,admdt <= edate1) %>% 
  mutate(Rxst=if_else(Rxst<sdate,sdate,Rxst),
         Rxed=if_else(Rxed>edate1,edate1,Rxed),
         du = Rxst-sdate)
length(unique(afinalb$Reference.Key.)) #1124



# afinalb <- readRDS("D:/OneDrive - connect.hku.hk/Other-share/5.Phd project/17.MPH_fracture/1.data_ana/2.data/afinalb_allfracture.rds")
# afinalb <- readRDS("D:/OneDrive - connect.hku.hk/Other-share/5.Phd project/17.MPH_fracture/1.data_ana/2.data/afinalb_appenfracture.rds")



afinal01 <- afinalb[c('Reference.Key.','Rxst','Rxed','DOB','sdate','edate1','Sex.','DOD','dose_combine')] 
# afinal01_M <- afinal01 %>% filter(Sex.=="M")

afinal1 <- afinal01 %>% 
  mutate(Rxst0=NA,Rxed0=NA,
         Rxst1=NA,Rxed1=NA,
         Rxst2=NA,Rxed2=NA,
         Rxstw=NA,Rxedw=NA) %>% 
  arrange(Reference.Key.,Rxst) %>% 
  mutate(fi=duplicated(Reference.Key.)) %>% 
  mutate(bexo=as.numeric(Rxst - sdate)) %>% 
  group_by(Reference.Key.) %>% 
  mutate(gap=lead(Rxst)-1-Rxed,
         gap1=if_else(is.na(gap),edate1-Rxed,gap)) %>%
  ungroup(Reference.Key.)


afinal1$bexo1 <- ifelse (afinal1$fi == FALSE, afinal1$bexo, NA)
afinal1 <- afinal1 %>% mutate(du=as.numeric(Rxed - Rxst))
afinal1$csumdu <- ave(afinal1$du, afinal1$Reference.Key., FUN=cumsum)
#calculate cutoff date of each ID
cutoff <- afinal1 %>% 
  arrange(Reference.Key.,Rxst)%>% 
  filter(duplicated(Reference.Key.)==F)%>% 
  mutate(cutoff90 = Rxst+89,ID = NA) %>% 
  select(Reference.Key.,cutoff90)
afinal1 <- merge(afinal1,cutoff,by="Reference.Key.")
#gengerate 30days of pre-exposure period 
afinal1$Rxst0[which ((afinal1$bexo1>0)&(afinal1$bexo1<=90))] <- afinal1$sdate [which ((afinal1$bexo1>0)&(afinal1$bexo1<=90))]
afinal1$Rxed0[which ((afinal1$bexo1>0)&(afinal1$bexo1<=90))] <- (afinal1$Rxst -1) [which ((afinal1$bexo1>0)&(afinal1$bexo1<=90))]
afinal1$Rxst0[which (afinal1$bexo1>90)] <- (afinal1$Rxst -90) [which (afinal1$bexo1>90)]
afinal1$Rxed0[which (afinal1$bexo1>90)] <- (afinal1$Rxst -1) [which (afinal1$bexo1>90)]
afinal1$Rxst0 <- as.Date(afinal1$Rxst0, origin = "1970-01-01")
afinal1$Rxed0 <- as.Date(afinal1$Rxed0, origin = "1970-01-01")
#generate post-exposure group
afinal1$Rxst1[which (afinal1$Rxed <= afinal1$cutoff90)] <- afinal1$Rxst[which (afinal1$Rxed <= afinal1$cutoff90)]
afinal1$Rxed1[which (afinal1$Rxed <= afinal1$cutoff90)] <- afinal1$Rxed[which (afinal1$Rxed <= afinal1$cutoff90)]
afinal1$Rxst1[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))] <- afinal1$Rxst[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))]
afinal1$Rxed1[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))] <- afinal1$cutoff90[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))]
afinal1$Rxst2[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))] <- (afinal1$cutoff90+1)[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))]
afinal1$Rxed2[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))] <- afinal1$Rxed[which ((afinal1$Rxst <= afinal1$cutoff90)&(afinal1$Rxed > afinal1$cutoff90))]
afinal1$Rxst2[which (afinal1$Rxst > afinal1$cutoff90)] <- afinal1$Rxst[which (afinal1$Rxst > afinal1$cutoff90)]
afinal1$Rxed2[which (afinal1$Rxst > afinal1$cutoff90)] <- afinal1$Rxed[which (afinal1$Rxst > afinal1$cutoff90)]

afinal1$Rxst1 <- as.Date(afinal1$Rxst1, origin = "1970-01-01")
afinal1$Rxed1 <- as.Date(afinal1$Rxed1, origin = "1970-01-01")
afinal1$Rxst2 <- as.Date(afinal1$Rxst2, origin = "1970-01-01")
afinal1$Rxed2 <- as.Date(afinal1$Rxed2, origin = "1970-01-01")

# afinal1$Rxstw[which(afinal1$gap1 > 0)] <- (afinal1$Rxed+1)[which(afinal1$gap1 > 0)]
# afinal1$Rxedw [which(afinal1$gap1>90)] <- (afinal1$Rxstw+89) [which(afinal1$gap1>90)]
# afinal1$Rxedw [which((afinal1$gap1 <= 90)&(afinal1$gap1 > 0))] <- (afinal1$Rxstw+afinal1$gap1-1) [which((afinal1$gap1 <= 90)&(afinal1$gap1 > 0))]
afinal1$Rxstw[which(afinal1$gap1 > 90)] <- (afinal1$Rxed+1)[which(afinal1$gap1 > 90)]
afinal1$Rxedw [which(afinal1$gap1>90)] <- (afinal1$Rxstw+89) [which(afinal1$gap1>90)]
afinal1$Rxstw <- as.Date(afinal1$Rxstw, origin = "1970-01-01")
afinal1$Rxedw <- as.Date(afinal1$Rxedw, origin = "1970-01-01")



#gen new obs start date
adhd_mph_ip2 <- adhd_mph_ip1 %>% 
  filter(grepl('^E967|^995.5',Dx.Px.code.)==T) %>% 
  mutate(admdt=as.Date(Admission.Date..yyyy.mm.dd..),
         sdate1=as.Date(Discharge.Date..yyyy.mm.dd..)) %>% 
  select(Reference.Key.,admdt,Dx.Px.code.,sdate1) %>% 
  `colnames<-` (c('Reference.Key.','admdt','All.Diagnosis.Code..ICD9..','sdate1')) %>% 
  mutate(Patient.Type..IP.OP.A.E..="0I")

adhd_mph_alldx1 <- adhd_mph_alldx %>% 
  filter(grepl('^E967|^995.5',All.Diagnosis.Code..ICD9..)==T) %>% 
  filter(Patient.Type..IP.OP.A.E..!="I") %>% 
  mutate(sdate1=admdt+days(6)) %>% 
  select(Reference.Key.,admdt,All.Diagnosis.Code..ICD9..,sdate1,Patient.Type..IP.OP.A.E..)



bc_1st <- rbind(adhd_mph_ip2,adhd_mph_alldx1) %>% 
  # filter(grepl('^E967|^995.5',All.Diagnosis.Code..ICD9..)==T) %>% 
  arrange(Reference.Key.,admdt,Patient.Type..IP.OP.A.E..) %>% 
  filter(Reference.Key.%in%a$Reference.Key.,
         duplicated(Reference.Key.)==F) %>%  
  select(Reference.Key.,sdate1)




afinal_new1 <- merge(afinal1,bc_1st,by="Reference.Key.",all.x = T)%>% 
  mutate(sdate1=pmax(sdate1,sdate,na.rm = T))
afinal_new2 <- merge(afinal_new1 %>% select(Reference.Key.,sdate1,edate1) %>% filter(duplicated(Reference.Key.)==F),
                     bc1_1 %>% select(Reference.Key.,admdt),by="Reference.Key.",all.x = T)%>% 
  filter(admdt>=sdate1,admdt<=edate1) %>% 
  arrange(Reference.Key.,admdt) %>% 
  filter(duplicated(Reference.Key.)==F) %>% 
  select(Reference.Key.,admdt)
afinal_new3 <- merge(afinal_new1,afinal_new2,by="Reference.Key.")




# fm <- readRDS("M:/Personal/FAN Min/to Le/recurrent_240ppl.rds")
# fm[which(!as.numeric(fm)%in%afinal_new3$Reference.Key.)]


#11 SCCS analysis
#add covid index


afinal2 <- afinal_new3 %>% 
  mutate(DOB=as.Date(DOB),
         Rxst=as.numeric(difftime(Rxst, DOB, unit="days") +1),
         Rxed=as.numeric(difftime(Rxed, DOB, unit="days") +1), 
         Rxst0=as.numeric(difftime(Rxst0, DOB, unit="days") +1),
         Rxed0=as.numeric(difftime(Rxed0, DOB, unit="days") +1), 
         Rxst1=as.numeric(difftime(Rxst1, DOB, unit="days") +1),
         Rxed1=as.numeric(difftime(Rxed1, DOB, unit="days") +1),
         Rxst2=as.numeric(difftime(Rxst2, DOB, unit="days") +1),
         Rxed2=as.numeric(difftime(Rxed2, DOB, unit="days") +1),
         Rxstw=as.numeric(difftime(Rxstw, DOB, unit="days") +1),
         Rxedw=as.numeric(difftime(Rxedw, DOB, unit="days") +1),
         sdate1=as.numeric(difftime(sdate1, DOB, unit="days") +1),
         edate1=as.numeric(difftime(edate1, DOB, unit="days") +1),
         admdt=as.numeric(difftime(admdt, DOB, unit="days") +1),
         covid_s=as.numeric(difftime(ymd('2020-01-23'), DOB, unit="days") +1),
         covid_e=as.numeric(difftime(ymd('2020-12-31'), DOB, unit="days") +1),
         c_i_under40_s=as.numeric(difftime(ymd('2020-01-21'), DOB, unit="days") +1),
         c_i_under40_e=as.numeric(difftime(ymd('2020-01-26'), DOB, unit="days") +1),
         c_i_40to50_1_s=as.numeric(difftime(ymd('2020-01-27'), DOB, unit="days") +1),
         c_i_40to50_1_e=as.numeric(difftime(ymd('2020-02-04'), DOB, unit="days") +1),
         c_i_40to50_2_s=as.numeric(difftime(ymd('2020-03-02'), DOB, unit="days") +1),
         c_i_40to50_2_e=as.numeric(difftime(ymd('2020-03-22'), DOB, unit="days") +1),
         c_i_40to50_3_s=as.numeric(difftime(ymd('2020-06-19'), DOB, unit="days") +1),
         c_i_40to50_3_e=as.numeric(difftime(ymd('2020-07-14'), DOB, unit="days") +1),
         c_i_50to60_1_s=as.numeric(difftime(ymd('2020-02-05'), DOB, unit="days") +1),
         c_i_50to60_1_e=as.numeric(difftime(ymd('2020-03-01'), DOB, unit="days") +1),
         c_i_50to60_2_s=as.numeric(difftime(ymd('2020-03-23'), DOB, unit="days") +1),
         c_i_50to60_2_e=as.numeric(difftime(ymd('2020-03-28'), DOB, unit="days") +1),
         c_i_50to60_3_s=as.numeric(difftime(ymd('2020-05-29'), DOB, unit="days") +1),
         c_i_50to60_3_e=as.numeric(difftime(ymd('2020-06-18'), DOB, unit="days") +1),
         c_i_50to60_4_s=as.numeric(difftime(ymd('2020-09-23'), DOB, unit="days") +1),
         c_i_50to60_4_e=as.numeric(difftime(ymd('2020-11-29'), DOB, unit="days") +1),
         c_i_60to70_1_s=as.numeric(difftime(ymd('2020-03-29'), DOB, unit="days") +1),
         c_i_60to70_1_e=as.numeric(difftime(ymd('2020-05-28'), DOB, unit="days") +1),
         c_i_60to70_2_s=as.numeric(difftime(ymd('2020-07-15'), DOB, unit="days") +1),
         c_i_60to70_2_e=as.numeric(difftime(ymd('2020-09-22'), DOB, unit="days") +1),
         c_i_60to70_3_s=as.numeric(difftime(ymd('2020-11-30'), DOB, unit="days") +1),
         c_i_60to70_3_e=as.numeric(difftime(ymd('2020-12-03'), DOB, unit="days") +1),
         c_i_up70_s=as.numeric(difftime(ymd('2020-12-04'), DOB, unit="days") +1),
         c_i_up70_e=as.numeric(difftime(ymd('2020-12-31'), DOB, unit="days") +1),
         Reference.Key.=as.numeric(Reference.Key.))
# saveRDS(afinal2,"D:/OneDrive - connect.hku.hk/Other-share/5.Phd project/17.MPH_fracture/1.data_ana/2.data/afinal2_allfracture_1strw.rds")
# saveRDS(afinal2,"D:/OneDrive - connect.hku.hk/Other-share/5.Phd project/17.MPH_fracture/1.data_ana/2.data/afinal2_appenfracture_1strw.rds")




test <- afinal2 %>% arrange(Reference.Key.,Rxst)
  






library(SCCS)
table(year(test$DOB))
test$year_cat <- NA
test$year_cat[which((year(test$DOB) %in% c(1988,1992,1996,2000,2004,2008,2012))&(month(test$DOB)<3))] <- 1
test$year_cat[which((year(test$DOB) %in% c(1988,1992,1996,2000,2004,2008,2012))&(month(test$DOB)>=3))] <- 2
test$year_cat[which((year(test$DOB) %in% c(1989,1993,1997,2001,2005,2009,2013))&(month(test$DOB)<3))] <- 2
test$year_cat[which((year(test$DOB) %in% c(1989,1993,1997,2001,2005,2009,2013))&(month(test$DOB)>=3))] <- 3
test$year_cat[which((year(test$DOB) %in% c(1990,1994,1998,2002,2006,2010,2014))&(month(test$DOB)<3))] <- 3
test$year_cat[which((year(test$DOB) %in% c(1990,1994,1998,2002,2006,2010,2014))&(month(test$DOB)>=3))] <- 4
test$year_cat[which((year(test$DOB) %in% c(1987,1991,1995,1999,2003,2007,2011))&(month(test$DOB)<3))] <- 4
test$year_cat[which((year(test$DOB) %in% c(1987,1991,1995,1999,2003,2007,2011))&(month(test$DOB)>=3))] <- 1


data1 <- test %>% filter(year_cat==1)
data1_dob <- unique(data1[,c("Reference.Key.","DOB")])
dob1 <- as.numeric(format(data1_dob$DOB,"%d%m%Y"))

data2 <- test %>% filter(year_cat==2)
data2_dob <- unique(data2[,c("Reference.Key.","DOB")])
dob2 <- as.numeric(format(data2_dob$DOB,"%d%m%Y"))

data3 <- test %>% filter(year_cat==3)
data3_dob <- unique(data3[,c("Reference.Key.","DOB")])
dob3 <- as.numeric(format(data3_dob$DOB,"%d%m%Y"))

data4 <- test %>% filter(year_cat==4)
data4_dob <- unique(data4[,c("Reference.Key.","DOB")])
dob4 <- as.numeric(format(data4_dob$DOB,"%d%m%Y"))



formatdata1 <- formatdata(indiv=Reference.Key., astart=sdate1, aend=edate1, aevent=admdt, 
                          adrug=list(Rxst0,Rxst1,Rxst2,Rxstw,c_i_under40_s,c_i_40to50_1_s,c_i_40to50_2_s,c_i_40to50_3_s,c_i_50to60_1_s,c_i_50to60_2_s,c_i_50to60_3_s,c_i_50to60_4_s,c_i_60to70_1_s,c_i_60to70_2_s,c_i_60to70_3_s,c_i_up70_s),
                          aedrug=list(Rxed0,Rxed1,Rxed2,Rxedw,c_i_under40_e,c_i_40to50_1_e,c_i_40to50_2_e,c_i_40to50_3_e,c_i_50to60_1_e,c_i_50to60_2_e,c_i_50to60_3_e,c_i_50to60_4_e,c_i_60to70_1_e,c_i_60to70_2_e,c_i_60to70_3_e,c_i_up70_e),
                          agegrp = cumsum(c(366+365+365+365+366+365+1,365,365,366,365,365,365,366,365,365,365)),
                          seasongrp = c(0203,0205,0209,0211),dob=dob1,
                          dataformat="stack", data=data1)

# saveRDS(data1,"M:\\Personal\\Gao Le\\to_fm\\data1.rds")
formatdata2 <- formatdata(indiv=Reference.Key., astart=sdate1, aend=edate1, aevent=admdt, 
                          adrug=list(Rxst0,Rxst1,Rxst2,Rxstw,c_i_under40_s,c_i_40to50_1_s,c_i_40to50_2_s,c_i_40to50_3_s,c_i_50to60_1_s,c_i_50to60_2_s,c_i_50to60_3_s,c_i_50to60_4_s,c_i_60to70_1_s,c_i_60to70_2_s,c_i_60to70_3_s,c_i_up70_s),
                          aedrug=list(Rxed0,Rxed1,Rxed2,Rxedw,c_i_under40_e,c_i_40to50_1_e,c_i_40to50_2_e,c_i_40to50_3_e,c_i_50to60_1_e,c_i_50to60_2_e,c_i_50to60_3_e,c_i_50to60_4_e,c_i_60to70_1_e,c_i_60to70_2_e,c_i_60to70_3_e,c_i_up70_e),
                          agegrp = cumsum(c(365+365+365+366+365+365+1,365,366,365,365,365,366,365,365,365,366)),
                          seasongrp = c(0203,0205,0209,0211),dob=dob2,
                          dataformat="stack", data=data2)


formatdata3 <- formatdata(indiv=Reference.Key., astart=sdate1, aend=edate1, aevent=admdt, 
                          adrug=list(Rxst0,Rxst1,Rxst2,Rxstw,c_i_under40_s,c_i_40to50_1_s,c_i_40to50_2_s,c_i_40to50_3_s,c_i_50to60_1_s,c_i_50to60_2_s,c_i_50to60_3_s,c_i_50to60_4_s,c_i_60to70_1_s,c_i_60to70_2_s,c_i_60to70_3_s,c_i_up70_s),
                          aedrug=list(Rxed0,Rxed1,Rxed2,Rxedw,c_i_under40_e,c_i_40to50_1_e,c_i_40to50_2_e,c_i_40to50_3_e,c_i_50to60_1_e,c_i_50to60_2_e,c_i_50to60_3_e,c_i_50to60_4_e,c_i_60to70_1_e,c_i_60to70_2_e,c_i_60to70_3_e,c_i_up70_e),
                          agegrp = cumsum(c(365+365+366+365+365+365+1,366,365,365,365,366,365,365,365,366,365)),
                          seasongrp = c(0203,0205,0209,0211),dob=dob3,
                          dataformat="stack", data=data3)


formatdata4 <- formatdata(indiv=Reference.Key., astart=sdate1, aend=edate1, aevent=admdt, 
                          adrug=list(Rxst0,Rxst1,Rxst2,Rxstw,c_i_under40_s,c_i_40to50_1_s,c_i_40to50_2_s,c_i_40to50_3_s,c_i_50to60_1_s,c_i_50to60_2_s,c_i_50to60_3_s,c_i_50to60_4_s,c_i_60to70_1_s,c_i_60to70_2_s,c_i_60to70_3_s,c_i_up70_s),
                          aedrug=list(Rxed0,Rxed1,Rxed2,Rxedw,c_i_under40_e,c_i_40to50_1_e,c_i_40to50_2_e,c_i_40to50_3_e,c_i_50to60_1_e,c_i_50to60_2_e,c_i_50to60_3_e,c_i_50to60_4_e,c_i_60to70_1_e,c_i_60to70_2_e,c_i_60to70_3_e,c_i_up70_e),
                          agegrp = cumsum(c(365+366+365+365+365+366+1,365,365,365,366,365,365,365,366,365,365)),
                          seasongrp = c(0203,0205,0209,0211),dob=dob4,
                          dataformat="stack", data=data4)

formatdata_all1 <- rbind(formatdata1,formatdata2,formatdata3,formatdata4) 

coln_need_to_change <- colnames(formatdata_all1)[grepl("c_i",colnames(formatdata_all1))]

for(i in coln_need_to_change){
  formatdata_all1[[i]] <- as.numeric(formatdata_all1[[i]])
}
formatdata_all2 <- formatdata_all1%>% 
  mutate(c_i_40to50_s=c_i_40to50_1_s+c_i_40to50_2_s+c_i_40to50_3_s,
         c_i_50to60_s=c_i_50to60_1_s+c_i_50to60_2_s+c_i_50to60_3_s+c_i_50to60_4_s,
         c_i_60to70_s=c_i_60to70_1_s+c_i_60to70_2_s+c_i_60to70_3_s)
coln_need_to_change <- colnames(formatdata_all2)[grepl("c_i",colnames(formatdata_all2))]

for(i in coln_need_to_change){
  formatdata_all2[[i]] <- as.factor(formatdata_all2[[i]])
}




#py calculate
tapply(formatdata_all2$event, formatdata_all2$Rxst0, sum)
tapply(formatdata_all2$interval, formatdata_all2$Rxst0, sum)/365.25

tapply(formatdata_all2$event, formatdata_all2$Rxst1, sum)
tapply(formatdata_all2$interval, formatdata_all2$Rxst1, sum)/365.25

tapply(formatdata_all2$event, formatdata_all2$Rxst2, sum)
tapply(formatdata_all2$interval, formatdata_all2$Rxst2, sum)/365.25

tapply(formatdata_all2$event, formatdata_all2$Rxstw, sum)
tapply(formatdata_all2$interval, formatdata_all2$Rxstw, sum)/365.25


fmla <- paste(event~Rxst0+Rxst1+Rxst2+c_i_under40_s+c_i_40to50_s+c_i_50to60_s+c_i_60to70_s+c_i_up70_s+age+season, "+", "strata(indiv)", 
              "+", "offset(log(interval))")
fmla1 <- as.formula(paste("event~", fmla[3]))
library(survival)
mod <- clogit(formula = fmla1, data = formatdata_all2)
summary(mod)



test_recurrent <- formatdata_all2 %>% 
  filter(event==1&Rxst0==0&Rxst1==0&Rxst2==0)
length(unique(test_recurrent$indiv)) #705


test_all <- test_recurrent %>% 
  filter(indv%in%test$indiv)


test_recurrent <- formatdata_all2 %>% 
  filter((event==1&Rxst0==0&Rxst1==1&Rxst2==0)|(event==1&Rxst0==0&Rxst1==0&Rxst2==1))
length(unique(test_recurrent$indiv)) #68


dose_recurrent <- afinal_new3 %>% 
  filter(Reference.Key.%in%test_recurrent$indiv)
length(unique(dose_recurrent$Reference.Key.)) #68

ttt1 <- dose_recurrent %>% 
  filter(admdt>=Rxst,admdt<=Rxed)
summary(ttt1$dose_combine)

ttt2 <- dose_recurrent %>% 
  filter(admdt<Rxst|admdt>Rxed)
summary(ttt2$dose_combine)

wilcox.test(ttt1$dose_combine,ttt2$dose_combine)




#direct comparison 200 day before as ref
formatdata_all3 <- formatdata_all2 
formatdata_all3$rx <- NA
formatdata_all3$rx[which(formatdata_all3$Rxst0==1)] <- " ref"
formatdata_all3$rx[which(formatdata_all3$Rxst1==1)] <- "1.90DAYS_BEFORE"
formatdata_all3$rx[which(formatdata_all3$Rxst2==1)] <- "2.SUBSEQUENT"
formatdata_all3$rx[which(is.na(formatdata_all3$rx))] <- "other non-rx"

formatdata_all3$rx_fac <- as.factor(formatdata_all3$rx)


fmla <- paste(event~rx_fac+c_i_under40_s+c_i_40to50_s+c_i_50to60_s+c_i_60to70_s+c_i_up70_s+age+season, "+", "strata(indiv)", 
              "+", "offset(log(interval))")
fmla1 <- as.formula(paste("event~", fmla[3]))
library(survival)
mod <- clogit(formula = fmla1, data = formatdata_all3)
summary(mod)


