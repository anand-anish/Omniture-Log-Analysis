rm(list=ls())

setwd("C:/Users/aa16400/Desktop/Omniture_log_analysis")
#installing and extracting the packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(ggplot2)

#loading the files in the R environment
Omniture.0 <- read.delim("./Omniture.0.tsv", header=FALSE)
View(Omniture.0)
products <- read.delim("./products.tsv")
View(products)
users <- read.delim("./users.tsv")
View(users)
which(is.na(users)==T)
which(is.na(Omniture.0)==T)
which(is.na(products)==T)
#taking out meaningful relevant columns from the data frame
omniture<-Omniture.0[,c(2,8,13,14,50,51,53)]
names(omniture)[names(omniture)=="V2"]<-"ts"
names(omniture)[names(omniture)=="V8"]<-"ip"
names(omniture)[names(omniture)=="V13"]<-"url"
names(omniture)[names(omniture)=="V14"]<-"swid"
names(omniture)[names(omniture)=="V50"]<-"city"

names(omniture)[names(omniture)=="V51"]<-"country"
names(omniture)[names(omniture)=="V53"]<-"state"' users$BIRTH<- as.Date(users$BIRTH_DT, "%d-%b-%y")   #%b<-abbreviated month  %B<-unabbreviated month  %y<-2-digit year      %Y<-4-digit year
users$BIRTH_DT <- as.Date(ifelse(users$BIRTH > Sys.Date(),format(users$BIRTH, "19%y-%m-%d"),format(users$BIRTH)))
users$BIRTH=NULL

#merging the data frames
list<-left_join(omniture,products)
list<-merge(list,users,by.x = c("swid"),by.y=c("SWID"),all.x=T)

list$BIRTH_year<-sapply(list$BIRTH_DT,FUN=function(x) strsplit(as.character(x),split="-")[[1]][1])
list$present_year<-2016
list$age<-list$present_year-as.numeric(list$BIRTH_year)

list$BIRTH_DT<-NULL;
list$BIRTH_year<-NULL;
list$present_year<-NULL;
list$ts<-NULL;


length(which(is.na(list$age==T)))
#[1] 26795
length(which(is.na(list$GENDER_CD==T)))
#[1] 26341
#so we are choosing to remove the missing vaues from our observation,
#as the proportion of male:female is almost same and using median will unevenly increase the ratio to one side
list<-na.omit(list)  #remove all missing values

#filtering the data by product category. Here we are just considering categories clothing and shoes due to their high interest among buyers
a<-filter(list2,category=="clothing")
b<-filter(list2,category=="shoes")
hist(a$age)

#clicks by state and gender in clothing category
qplot(state,data=a,facets = GENDER_CD~.)+theme(text = element_text(size=12),axis.text.x = 
                                                 element_text(angle = 90, hjust = 1))+labs(title="Clickstream in CLOTHING Category ")

#clicks on urls inside clothing category
qplot(url,data=a)+theme(text = element_text(size=12),axis.text.x = 
                          element_text(angle = 90, hjust = 1))+labs(title="Url's usage IN CLOTHING Category ")
#similarly other categories and respective insights inside categories can be calculated

#BOUNCE RATE -it is defined by Total number of visitors viewing one page only(Tv) divided by Total entries to page(Te  , B.R=Tv/Te
list2<-list 
list2$swid<-as.factor(list2$swid)
#to represent the factors in the numeric form,hence comparison becomes easy
list2$swid_num<-as.numeric(list2$swid)

#to get total count of clicks on a url
b_list <- aggregate(ind ~ url ,data = cbind(list2,ind=1),FUN=sum) #responsevariable(ind)

#to get unique visitors url
list2<-arrange(list2,swid)
c=1
for( i in 1:(nrow(list2)-1))
{ 
  list2$c[i]=c
  if(list2$swid_num[i]==list2$swid_num[i+1])
    c=c+1
  else
    c=1
  list2$c[i+1]=c
}

list3 <- aggregate(c ~ swid,data = list2,FUN=sum) 
list3<-filter(list3,c==1)
list3<-merge(list3,list2,all.x=T)
#count of unique visitors having surfed the only page as that of the given url(not visited any other page except this)
b_v_list <- aggregate(ind1 ~ url ,data = cbind(list3,ind1=1),FUN=sum) 

bounce<-left_join(b_list,b_v_list,by="url")
bounce$ind1[is.na(bounce$ind1)==T]<-0
bounce$bounce_rate<-bounce$ind1/bounce$ind
bounce$bounce_rate<-bounce$bounce_rate*100


#VISUALISATIONS

#to see clickstream count bycountry
qplot(country,data=list)
#usa no doubt has around 87048 counts outof 87675 obsv.
#so the analysis is done on the states and people of usa
list2<<-filter(list,country=="usa")

#click by state
p<-qplot(state,data=list2,fill=category)
p + theme(axis.title=element_text(face="bold", size="12", color="gray"), legend.position="right") + theme(text = element_text(size=18),axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="CLICKSTREAM BY LOCATION") +labs(x="STATE",y="COUNT OF CLICKS")
#website user data by age 
#PLOT 1(HISTOGRAM)
p<-qplot(age,data=list2,bins=30)
q<-p+theme_bw()
q + theme(axis.title=element_text(face="bold",  size="10")) +  labs(title="Clickstream by Age") +labs(x="AGE",y="COUNT OF CLICKS")

#PLOT2 (WEBSITE DATA BY CATEGORY AND AGE)
table(list2$category) # to get exact count of category websites clicks

qplot(age,data=list2,fill=category) +theme_bw()+ theme(axis.title=element_text(face="bold",size="10")) +  labs(title="Website data by Age") 

#PLOT3 (USING GENDER AS A FACET)
qplot(age,data=list2,fill=category,facets = GENDER_CD~.) +theme_bw()+theme(axis.title=element_text(face="bold",size="10")) +  labs(title="Website data by Age and Gender")

#website user data by gender 
#Plot-1
qplot(category,data=list2,fill=GENDER_CD)+ theme(axis.title=element_text(face="bold",size="10"))+theme(text = element_text(size=18),axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Clickstream by Gender")
#Plot-2
qplot(category,data=list2,facets=GENDER_CD~.) +theme_gray()+theme(axis.title=element_text(face="bold",size="15")) + theme(text = element_text(size=18),axis.text.x = 
                                                                                                                            element_text(angle = 90, hjust = 1))+ labs(title="Clickstream by Gender") 

#PLOT TO FIND THE TARGET AGE SEGMENT
#PLOT 1(HISTOGRAM TO SEE THE AGE TREND)
p<-qplot(age,data=list2,bins=30,facets = GENDER_CD~.)
q<-p+theme_bw()
q + theme(axis.title=element_text(face="bold", size="10")) +  labs(title="Clickstream by Age") +labs(x="AGE",y="COUNT OF CLICKS")

#PLOT 2(DENSITY PLOT TO FIND THE TARGET AGE SEGMENT AND MARKING THE AGE SEGMENT BY INTERCEPTS)
p<-qplot(age,data=list2,geom="density",facets = GENDER_CD~.)
q<-p+theme_bw()+geom_vline(xintercept=24) +geom_vline(xintercept=33)
q + theme(axis.title=element_text(face="bold", 
                                  size="10")) +  labs(title="target age segment") 

#bounce rate
qplot(url,bounce_rate,data=bounce,geom=c("point","line"),group=1) +theme_linedraw()+
  theme(axis.title=element_text(face="bold",size="15")) +  labs(title="Clickstream by Gender") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

######################################END############################################################ 

