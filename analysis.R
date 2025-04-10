library(tidyverse)
library(reshape2)

##################################################
### questionnaires analysis

# for all months/time points
allq<-read.csv("./data/all_questionnaires.csv", sep=",")
w.all<-allq %>% filter(questionnaire_regrouped=="weekly")
w.all$year<-format(as.Date(w.all$answer_given_at, format="%Y-%m-%d"),"%Y")
w.all$month<-format(as.Date(w.all$answer_given_at, format="%Y-%m-%d"),"%m")
w.all<-w.all[!grepl("iamtest", w.all$person_id),]
weekly2<-w.all
unique(weekly2$person_id)

colnames(weekly2)
w.all$Month<-ifelse(w.all$month=="03" & w.all$year=="2019",1,
                    ifelse(w.all$month=="04",2,
                           ifelse(w.all$month=="05",3,
                                  ifelse(w.all$month=="06",4,
                                         ifelse(w.all$month=="07",5,
                                                ifelse(w.all$month=="08",6,
                                                       ifelse(w.all$month=="09",7,
                                                              ifelse(w.all$month=="10",8,
                                                                     ifelse(w.all$month=="11",9,
                                                                            ifelse(w.all$month=="12",10,
                                                                                   ifelse(w.all$month=="01",11,
                                                                                          ifelse(w.all$month=="02",12,13))))))))))))


w.all.freq <- ddply(w.all, .(w.all$person_id, w.all$Month, w.all$question_simplified, w.all$answer), nrow)
head(w.all.freq)
colnames(w.all.freq)<-c("person_id", "month","question","answer","freq")
w.all.freq<-w.all.freq %>% filter(question!="algemeenGevoelVoorbijeWeek")
w.all.freq$answer<-as.numeric(w.all.freq$answer)
mean.answer<-w.all.freq %>% group_by(person_id, month, question) %>% summarise_at("answer", mean)

mean.answer$iam_id<-paste0(mean.answer$person_id,"_",mean.answer$month)

mean.answer$iam_id<-ifelse(nchar(mean.answer$month)==2, paste0(mean.answer$person_id,"_",mean.answer$month),
                           paste0(mean.answer$person_id,"_0",mean.answer$month))

x.order<-c('Bloody nose','Bruises','Shaking hands','Migraine','Acne','Abdominal pain','Shortness of breath',
           'Constipation','Tense ligaments','Bad appetite','Nausea/vomiting','Chest pain','Feeling faint','Indigestion',
           'Hot/cold shivers','Severe pains/ailments','Blurry vision','Muscle cramps','Weight changes',
           'Pounding heart','Diarrhea','Numbness/tingling','Uncomfortable feeling','Dizziness',
           'Stuffy nose/head','Cold or cough','Low energy','Muscle pain','Headache','Tense muscles',
           'Constant Fatigue','Sleeping problems','Back pain')

x.order<-c('Acne','Bloody nose', 'Pounding heart','Abdominal pain', 'Constant Fatigue', 'Constipation','Diarrhea',
           'Dizziness','Severe pains/ailments','Feeling faint','Tense ligaments','Tense muscles','Numbness/tingling'
           ,'Weight changes','Headache','Bruises','Shortness of breath','Low energy','Indigestion',
           'Migraine','Nausea/vomiting','Uncomfortable feeling','Chest pain','Back pain',
           'Sleeping problems','Bad appetite','Muscle cramps','Muscle pain','Shaking hands','Cold or cough','Stuffy nose/head','Hot/cold shivers',
           'Blurry vision')

mean.answer<-mean.answer[order(mean.answer$person_id, mean.answer$month),]
iam<-read.csv("./data/IAM_ID_ori.csv",sep=",",row.names = 1)
mean.answer<-mean.answer %>% left_join(iam[,c(1,6)], by="person_id")
mean.answer$iam_id2<-ifelse(nchar(mean.answer$month)==2, paste0(mean.answer$codename,"_",mean.answer$month),
                            paste0(mean.answer$codename,"_0",mean.answer$month))
y.order<-unique(mean.answer$iam_id2)
mean.wide <- dcast(mean.answer, question ~ iam_id2, 
                   value.var = "answer", fun.aggregate = function(X) mean(X, na.rm=TRUE))
rownames(mean.wide)<-x.order
mean.wide<-mean.wide[,-1]
mean.wide<-as.matrix(mean.wide[,y.order])

mean.answer2<-filter(mean.answer, month==1 | month==4 | month==8 | month==12)
y.order<-unique(mean.answer2$iam_id2)
mean.wide <- dcast(mean.answer2, question ~ iam_id2, 
                   value.var = "answer", fun.aggregate = function(X) mean(X, na.rm=TRUE))
rownames(mean.wide)<-x.order
mean.wide<-mean.wide[,-1]
mean.wide<-mean.wide[,y.order]
save(mean.wide, file="./data/mean_wide_weekly.RData")

##################################################
### clinical biochemistry analysis

df<-read.csv("./data/clinical_clean.csv", header = T, sep = ";", row.names = 1)
df$sex<-ifelse(df$gender=="M",1,0)
db<-df %>% rename(subject=person_id, time=month)
# wide format
db<-dcast(db, subject+time+age+sex ~ label, value.var = "value")

outlier_mad<-function(db){
  subject<-unique(db$subject)
  db$time<-as.numeric(db$time)
  N<-length(subject)
  conf<-c("subject","time","age","sex")
  p<-sum(colnames(db)==conf)
  d.mad_subj<-list()
  i<-1; d.ik<-0
  #<-"IAM09"
  for (s in subject) {
    db<-db[order(db$subject,db$time),]
    d.i<-db[db$subject==s,] 
    
    d.mad_prot<-list()
    #j=30
    for (j in 1:(ncol(d.i)-p)) {
      d.ij<-d.i[,c(1:2,j+p)]
      
      #e check if there are outlying observation in one individual, if so we put 1. otherwise 0
      d.ij$res<-ifelse(d.ij[,3]>median(d.ij[,3], na.rm = TRUE)+2.3*mad(d.ij[,3], na.rm = TRUE) | 
                         d.ij[,3]<median(d.ij[,3], na.rm = TRUE)-2.3*mad(d.ij[,3], na.rm = TRUE),
                       1,0) #1 if outliers
      d.ij$variable<-names(d.ij)[3]
      d.ik<-rbind(d.ik,d.ij[,-3]) # all outliers data
    }
    i<-i+1
  }
  
  #From 32 to 33 remove
  
  d.out<-d.ik[-1,] # data in long format
  # wide format
  d.outw<-dcast(d.out, subject+time~variable, value.var = "res")
  # count per row; at 1 ind 1 time point, accross all variables
  d.outw$count.id<-rowSums(d.outw[,-c(1:2)], na.rm = T)
  # count percentage per total number of variables
  d.outw$pct<-d.outw$count.id/(ncol(d.outw)-3)
  
  return(list(
    d.out=d.outw
  ))
}

fun_d<-outlier_mad(db)
d.outw<-fun_d$d.out
# percentage of outlying per subject accross all variables 
d.out_meanallvar<-d.outw %>% group_by(subject) %>% summarise(pct_mean=mean(pct, na.rm=TRUE))

# percentage of outlying per subject
d.out_mean<-d.outw %>% group_by(subject) %>% summarise_all(.funs = c(mean="mean"), na.rm=TRUE)
colnames(d.out_mean)
d.out_mean2 <- d.out_mean %>% select(-c(subject, time_mean, count.id_mean, pct_mean))
rownames(d.out_mean2)<-d.out_mean$subject
save(d.out_mean2, file="./data/pct_out_clinical.RData")

##################################################
### principal component analysis

## clinical data
dt2 <- read.csv("./data/clinical_pca.csv", sep=";", header = T, row.names = 1)
dt2$age_cat <- as.factor(dt2$age_cat)
dt2 <- dt2 %>% select(-c(person_id:age_cat))
d.clin.pca <- prcomp(dt2, center = T, scale. = T)
summary(d.clin.pca)
save(d.clin.pca, file="./data/clin_pca_list.RData")

## metabolomics
df2 <- read.csv("./data/metabolomics_pca.csv", sep=",")
df2 <- df2 %>% select(-c(person_id:age))
d.metabo.pca <- prcomp(df2, center = T, scale. = T)
summary(d.metabo.pca)
save(d.metabo.pca, file="./data/metabo_pca_list.RData")

## proteomics
dt <- read.csv("./data/proteomics_pca.csv", sep=",")
dt <- dt %>% select(-c(person_id:age_cat))
d.prot.pca <- prcomp(dt, center = T, scale. = T)
summary(d.prot.pca)
save(d.prot.pca, file="./data/prot_pca_list.RData")

## genomics/WGS
library(flashpcaR)

# PCA analysis with FlashPCA2

path.bed = "/input_bed" # Path to BED files (WES or array)

pca = flashpca(path.bed, ndim = 40)

# Create a data frame with the generated eigenvectors and individuals IDs as row names

pca.vectors = as.data.frame(pca$vectors)
id = rownames(pca.vectors)
rownames(pca.vectors) = NULL
FID = id
IID = FID
pca = data.frame(FID, IID, pca.vectors)
pca$FID = gsub('.*:', '', pca$FID)
pca$IID = gsub('.*:', '', pca$IID)

# Save results 
path.pca = "results.pca"
saveRDS(pca, "resuls.rds")
write.table(pca, path.pca, row.names = FALSE, col.names = TRUE, quote = FALSE)