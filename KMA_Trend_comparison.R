#Naver Trends
#데이터 불러오기
naver_trend <- fread("G:/내 드라이브/2021-1/날씨와 소비/data/naver_trends/naver_beauty_201819_daily.csv", header = TRUE,encoding="UTF-8")
naver_trend <- as.data.frame(naver_trend)
naver_trend$DATE <- ymd(naver_trend$DATE)
nrow(naver_trend)

trans_cat1_trend <- trans[, sum(QTY), by=c("DATE","CAT1")]
trasn_cat1_trend <- as.data.frame(trans_cat1_trend)

sns_cat1_trends <- aggregate(sns$CNT, by=list(sns$DATE,sns$CAT1), FUN=sum)

colnames(sns_cat1_trends) = c("DATE", "CAT1","CNT")

trend <- left_join(trans_cat1_trend,sns_cat1_trends,by=c('DATE','CAT1'))
trend <- left_join(trend, naver_trend, by=c('DATE','CAT1'))
str(trend)
trend <- rename(trend, "QTY" = "V1")
nrow(trend)

trend_1 <- trend[,c(3,4,5)]
str(trend_1)
cor(trend_1)

write.csv(trend,"G:/내 드라이브/2021-1/날씨와 소비/data/naver_trends/trend.csv",row.names = FALSE)

#정규화
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
  
}
trend_1 <- normalize(trend_1)
head(trend_1)

#각 품목별 탐색 
trend_food <- subset(trend, CAT1=="식품")
trend_1_food <- trend_food[,c(3,4,5)]
cor(trend_1_food)

trend_beauty <- subset(trend, CAT1=="뷰티")
trend_1_beauty <- trend_beauty[,c(3,4,5)]

trend_app <- subset(trend, CAT1=="냉난방가전")
trend_1_app <- trend_app[,c(3,4,5)]

cor(trend_1_app)

cor(trend_1_beauty)
plot(trend_1_food)
plot(trend_1_beauty)

svi <- function(x){
  round((x/max(x))*100,2)
}



trend_1_beauty$CNT <- svi(trend_1_beauty$CNT)
trend_1_beauty$QTY <- svi(trend_1_beauty$QTY)

