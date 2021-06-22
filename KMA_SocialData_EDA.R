#Social Data analysis 
## Data load, preprocessing

#package & library
library(data.table)
library(dplyr) 
library(stringr) #str_sub fucntion

#get data
sns <- fread("G:/내 드라이브/2021-1/날씨와 소비/data/sns_df.csv", header=T,encoding="UTF-8")
head(sns)
str(sns)

sns = as.data.frame(sns)
str(sns)
head(sns)

sns$DATE <- ymd(sns$DATE)
sns$MONTH <- month(sns$DATE)
sns$YEAR <- year(sns$DATE)
sns$DAY <- day(sns$DATE)
sns$WEEKDAY <- weekdays(sns$DATE)
sns$YDAY <- yday(sns$DATE)
sns$QUARTER <- quarter(sns$DATE)

#계절생성함수
seasons = function(x){
  if(x %in% 2:4) return('Spring')
  if(x %in% 5:7) return('Summer')
  if(x %in% 8:10) return('Fall')
  if(x %in% c(11,12,1)) return('Winter')
}
sns$SEASON = sapply(sns$MONTH, seasons) #만든 함수 자체가 메모리 하자가 너무 큼. 
sns$SEASON = as.factor(sns$SEASON)
str(sns)
sns <- sns[,-1]
names(sns)

sns <- sns[,c("DATE","YEAR","MONTH","DAY","WEEKDAY","YDAY","SEASON","QUARTER","CAT1","CAT2","CAT3","CNT")]
str(sns)



#Social Data analysis 
## EDA
attach(sns)

#Keword Frequancy CAT1
cat_cat1_cnt <- aggregate(CNT, by=list(DATE,CAT1), FUN=sum)
library(ggplot2)

cat_cat1_cnt_graph <- cat_cat1_cnt %>%
  ggplot(mapping=aes(x=Group.1, y=x, color=Group.2, group=Group.2))+
  geom_line()+
  labs(title="품목별 키워드 빈도", x="Time",y="Keyword Frequency")

cat_cat1_cnt_graph


#Keword Frequancy CAT2
cat2_cnt = aggregate(CNT, by=list(DATE, CAT2),FUN=sum)
cat2_cnt_graph <- cat2_cnt %>%
  ggplot(mapping=aes(x=Group.1, y=x, color=Group.2, group=Group.2))+
  geom_line()+
  labs(title="중분류별 키워드 빈도", x="Time",y="Keyword Frequency")
