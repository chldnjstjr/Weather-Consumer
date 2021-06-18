library(pacman)
pacman::p_load("data.table", "dplyr", "pastecs", "tidyverse", "lubridate", "ggplot2","stringr")

trans <- fread("G:/내 드라이브/2021-1/날씨와 소비/data/trans.csv",  header = TRUE,encoding="UTF-8")
trans <- rename(trans, "CAT3" = "CAT2") #소분류로 이름 미리 변경, 이따 조인해야함. 
trans <- trans[,-1] #앞에 왜 추가됐는지 모르겠는 열 삭제

trans$MONTH <- as.numeric(str_sub(trans$DATE, 5,6)) #문자열을 일단 그대로 두고 월 파생변수 추가
trans$YEAR <- as.factor(str_sub(trans$DATE, 1,4))
trans$DATE <- ymd(trans$DATE)
trans$WEEKDAY <- as.factor(weekdays.POSIXt(trans$DATE))
trans$DAY <- as.factor(day(trans$DATE)) #문자로 안하고 그냥 이렇게 하면됨...year, month 함수도 있음. 요일은 wday. 그 해에 몇 번째 날이었는지 yday. 상반기 하반기는 semester. 4분기 구분은 quarter. 
trans$YDAY <- as.factor(yday(trans$DATE))
trans$SEME <- as.factor(semester(trans$DATE))
trans$QUARTER <- as.factor(quarter(trans$DATE))

#계절생성함수
seasons = function(x){
  if(x %in% 2:4) return('Spring')
  if(x %in% 5:7) return('Summer')
  if(x %in% 8:10) return('Fall')
  if(x %in% c(11,12,1)) return('Winter')
}
trans$SEASON = sapply(trans$MONTH, seasons) #만든 함수 자체가 메모리 하자가 너무 큼. 
trans$SEASON = as.factor(trans$SEASON)
trans$MONTH = as.factor(trans$MONTH)

#확인 
str(trans)
head(trans)
sum(is.na(trans))

#품목 추가
dict <- fread("G:/내 드라이브/2021-1/날씨와 소비/data/dict.csv", header=T,encoding="UTF-8")
head(dict)
trans <- left_join(trans,dict,by='CAT3')
head(trans)
names(trans)
trans <- trans[,c("DATE","YEAR","MONTH","DAY","WEEKDAY","YDAY","SEASON","SEME","QUARTER","SEX","AGE","CAT1","CAT2","CAT3","QTY")]
head(trans)
str(trans)

attach(trans)

table(SEX)
sex_freq <- table(SEX)
round(prop.table(sex_freq)*100,1)#비율
barplot(sex_freq) #그래프 예쁘게 하는건 나중에!

