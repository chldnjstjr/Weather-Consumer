library(pacman)
pacman::p_load("data.table", "dplyr", "pastecs", "tidyverse", "lubridate", "ggplot2","stringr","productplots","CGPfunctions")

#3D 그래프 라이브러리 호출
pacman::p_load("plotly","plot3D","tidyr","gridExtra","knitr","reshape2","plot3Drgl",)
library("plot3Drgl")
library("googleVis")

#CGPfunctions, productplots: PlpotXTabs함수
#gridExtra, grid: 그래프 밑에 테이블 추가하기 위함. grid.table 함수
library(gridExtra)#그래프 밑에 테이블 추가
library(grid) #그래프 밑에 테이블 추가 grid.table 함수

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
trans$SEASON = sapply(trans$MONTH, seasons)
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

#성별 통계
table(SEX)
sex_freq <- table(SEX)
round(prop.table(sex_freq)*100,1)#비율
barplot(sex_freq) #그래프 예쁘게 하는건 나중에!


# 시간에 따른 성별
yearly_sexcount <- xtabs(~YEAR+SEX)
round(prop.table(yearly_sexcount)*100,1)

## 월별 성별 합계
monthly_sexcounts <- data.frame(xtabs(~MONTH+SEX))
monthly_sexcounts
monthly_sexcounts_graph <- monthly_sexcounts %>%
  ggplot(mapping = aes(x = MONTH, y = Freq, color = SEX, group=SEX)) +
  geom_line()+
  geom_text(aes(label=Freq), vjust=1)#+ facet_wrap(facets = vars(genus)) 그래프 분리 옵션
monthly_sexcounts_graph

## 일별 성별 합계
daily_sexcounts <- data.frame(xtabs(~DAY+SEX))
daily_sexcounts
daily_sexcounts_graph <- daily_sexcounts %>%
  ggplot(mapping = aes(x = DAY, y = Freq, color = SEX, group=SEX)) +
  geom_line()+
  labs(title="일별 성별 합계")+theme(legend.position = c(0.1, 0.2))#+ facet_wrap(facets = vars(genus)) 그래프 분리 옵션
daily_sexcounts_graph

## 요일별 성별 합계
weekdaily_sexcounts <- data.frame(xtabs(~WEEKDAY+SEX))
weekdaily_sexcounts_graph <- weekdaily_sexcounts %>%
  ggplot(mapping = aes(x=WEEKDAY, y=Freq, color=SEX, group=SEX))+
  geom_line()+
  geom_text(aes(label=Freq), vjust=-0.5)+
  labs(title = "요일별 성별 거래 빈도 합계")+theme(legend.position = c(0.2,0.15))+
  scale_x_discrete(limits=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
weekdaily_sexcounts_graph

## 반기별 성별 합계
seme_sexcounts <- xtabs(~SEME+SEX)
seme_sexcounts
PlotXTabs(trans,SEME,SEX)

## 분기별 성별 합계
quart_sexcounts <- data.frame(xtabs(~QUARTER+SEX))
xtabs(~QUARTER+SEX)
quart_sexcounts_graph <- quart_sexcounts %>%
  ggplot(mapping=aes(x=QUARTER, y=Freq, color=SEX, group=SEX))+
  geom_line()+
  labs(title="분기별 성별 합계")
quart_sexcounts_graph
PlotXTabs(trans,SEX,QUARTER,"side")

## 계절별 성별 합계
seansonal_sexcounts <- data.frame(xtabs(~SEASON+SEX))
seansonal_sexcounts
seansonal_sexcounts_graph <- seansonal_sexcounts %>%
  ggplot(mapping=aes(x=SEASON, y=Freq, color=SEX, group=SEX))+
  geom_line()+
  labs(title="계절별 성별 합계")+
  scale_x_discrete(limits=c("Spring","Summer","Fall","Winter"))
seansonal_sexcounts_graph

## 전체 기간 성별 구매량
sex_date_qty <- aggregate(QTY, by=list(DATE,SEX), FUN=sum)
sex_date_qty <- data.frame(sex_qty)
sex_date_qty <- na.omit(sex_date_qty)
sum(is.na(sex_date_qty))
sex_date_qty_graph <- sex_date_qty %>%
  ggplot(mapping = aes(x = Group.1, y = x, color = Group.2, group=Group.2)) +
  geom_line()+
  labs(title="전체 기간 성별 구매량 합계")
sex_date_qty_graph

## 연도별 성별 구매량
sex_year_qty <- aggregate(QTY, by=list(YEAR,SEX), FUN=sum)
sex_year_qty_graph <- sex_year_qty %>%
  ggplot(mapping = aes(x=Group.1, y=x, color=Group.2, group=Group.2))+
  geom_bar(stat="identity")+
  geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
  labs(title="연간 성별 구매량", x="연도",y="구매량")
sex_year_qty_graph

## 월별 성별 구매량 
sex_month_qty <- aggregate(QTY, by=list(MONTH,SEX), FUN=sum)
sex_month_qty_graph <- sex_month_qty %>%
  ggplot(mapping = aes(x=Group.1, y=x, color=Group.2, group=Group.2))+
  geom_bar(stat="identity")+
  geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
  labs(title="월간 성별 구매량", x="월",y="구매량")
sex_month_qty_graph

##요일별 성별 구매량 
sex_weekday_qty <- aggregate(QTY, by=list(WEEKDAY,SEX), FUN=sum)
sex_weekday_qty_graph <- sex_weekday_qty %>%
  ggplot(mapping = aes(x=Group.1, y=x, color=Group.2, group=Group.2))+
  geom_bar(stat="identity")+
  geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
  labs(title="요일별 성별 구매량", x="월",y="구매량")+
  scale_x_discrete(limits=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
sex_weekday_qty_graph

##계절별 성별 구매량
sex_season_qty <- aggregate(QTY, by=list(SEASON,SEX), FUN=sum)
sex_season_qty_graph <- sex_season_qty %>%
  ggplot(mapping = aes(x=Group.1, y=x, color=Group.2, group=Group.2))+
  geom_bar(stat="identity")+
  geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
  labs(title="계절별 성별 구매량", x="계절",y="구매량")+
  scale_x_discrete(limits=c("Spring","Summer","Fall","Winter"))
sex_season_qty_graph

## 시간에 따른 성별*연령별*품목*구매량

### 대분류
sex_age_cat1_qty <- aggregate(QTY, by=list(DATE,SEX,AGE,CAT1), FUN=sum)
sex_age_cat1_qty = sex_age_cat1_qty[order(sex_age_cat1_qty$x,decreasing = T),] #성별나이카테고리별 총 판매량
sex_age_cat1_qty = na.omit(sex_age_cat1_qty)
str(sex_age_cat1_qty)
sex_age_cat1_qty_graph <- sex_age_cat1_qty %>%
  ggplot(mapping=aes(x=Group.1, y=x, color=Group.2, group=Group.3))+
  geom_line()
ggplot(sex_age_cat1_qty, aes(x=Group.1, y=x, group=Group.4, colour=Group.4))+
  geom_line()

### 중분류
sex_age_cat2_qty <- aggregate(QTY, by=list(SEX,AGE,CAT1,CAT2), FUN=sum)[order(QTY),]
sex_age_cat2_qty[order(sex_age_cat2_qty$x,decreasing = T),] #성별나이카테고리별 총 판매량

##구매량*시간
### 2D 선형 그래프
#### 월간 연간 구매량
year_month_qty = aggregate(QTY, by=list(YEAR,MONTH), FUN=sum)
year_month_qty_graph <- year_month_qty %>%
  ggplot(mapping = aes(x=Group.2, y=x, color=Group.1, group=Group.1))+
  geom_line()+
  geom_text(aes(label=x), vjust=1.6, color="black", size=3.5)+
  labs(title="연간 월간 구매량 추이", x="월",y="구매량")+
  scale_color_discrete(name="연도")+
  theme(legend.position = c(0.9,0.9),legend.key.size=unit(1,"cm"))
year_month_qty_graph

#### 월간 연간 평균구매량
year_month_meanqty = aggregate(QTY, by=list(YEAR,MONTH), FUN=mean)
year_month_meanqty$x = round(year_month_meanqty$x,2)
year_month_meanqty_graph <- year_month_meanqty %>%
  ggplot(mapping = aes(x=Group.2, y=x, color=Group.1, group=Group.1))+
  geom_line()+
  geom_text(aes(label=x), vjust=1.6, color="black", size=3.5)+
  labs(title="연간 월간 평균 구매량 추이", x="월",y="구매량")+
  scale_color_discrete(name="연도")+
  theme(legend.position = c(0.9,0.9),legend.key.size=unit(1,"cm"))
year_month_meanqty_graph


#PlotXTabs출처: https://ibecav.github.io/CGPfunctions/reference/PlotXTabs.html
#ggplot출처: https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
