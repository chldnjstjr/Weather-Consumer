#Naver Trends
#데이터 불러오기
naver_trend <- fread("G:/내 드라이브/2021-1/날씨와 소비/data/naver_trends/naver_beauty_201819_daily.csv",  header = TRUE,encoding="UTF-8")
naver_trend <- as.data.frame(naver_trend)
naver_trend$DATE <- ymd(naver_trend$DATE)

trans_cat1_trend <- trans %>% select(DATE,CAT1,QTY)
trans_cat1_trend <- as.data.frame(trans_cat1_trend)
sns_cat1_trends <- sns %>% select(DATE,CAT1,CNT)

str(naver_trend)
str(trans_cat1_trend)
str(sns_cat1_trends)


trend <- left_join(trans_cat1_trend,sns_cat1_trends,by=c('DATE','CAT1'))

memory.size(max = TRUE) #최대 가용 메모리 
memory.size(max=F) #현재 사용 중인 메모리
memory.limit(size=NA) #한계치

memory.limit(size= 56000) #16gb까지 강제로 높인다.

trend <- left_join(trans_cat1_trend,sns_cat1_trends,by=c('DATE'='DATE','CAT1'='CAT1'))


#httr 참고 https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html



#Google Trends

#gtrendsR setting
#install.packages("devtools")
#install.packages("extrafont")
library(devtools)
devtools::install_github("PMassicotte/gtrendsR")
library(gtrendsR)
library(tidyverse)
library(extrafont)
loadfonts()
par(family = "NanumGothic")

#gtrendsR 패키지 살펴보기
# 활용가능한 함수 
ls("package:gtrendsR")

# 로컬(locale) 문자집합 확인
localeToCharset()

kw <- "webzen"

if (!(Encoding(kw) == "utf-8")) {
  kw <- iconv(kw, "latin1", "utf-8", sub = "byte")
}

#검색어 하나 검색
gtrends("호날두") %>% 
  plot()

#복수 검색어 비교 
wz <- gtrends(c("메시","호날두","네이마르","베일"), geo = "KR", time = "today 3-m") %>%
  plot()

#데이터의 값들도 볼 수 있다.
head(wz$data)


#참고 http://statkclee.github.io/politics/google-trend.html
