#Naver Trends

##대분류(CAT1)
### 데이터 불러오기
#### ⓐ네이버 쇼핑 트렌드는 이미 엑셀로 편집한 상태
naver_trend <- fread("G:/내 드라이브/2021-1/날씨와 소비/data/naver_trends/naver_beauty_201819_daily.csv", header = TRUE,encoding="UTF-8")
naver_trend <- as.data.frame(naver_trend)
naver_trend$DATE <- ymd(naver_trend$DATE)
nrow(naver_trend) #행 개수를 꼼꼼히 확인하자~

#### ⓑ 구매량 트렌드
trans_cat1_trend <- trans[, sum(QTY), by=c("DATE","CAT1")] #대분류별 합계
trasn_cat1_trend <- as.data.frame(trans_cat1_trend) #df로 변환

#### ⓒ 소셜 언급량 트렌드
sns_cat1_trends <- aggregate(sns$CNT, by=list(sns$DATE,sns$CAT1), FUN=sum) #대분류별 합계, datatable이 아니라서 aggregate 사용
colnames(sns_cat1_trends) = c("DATE", "CAT1","CNT")

### ⓐ+ⓑ+ⓒ 병합 
trend <- left_join(trans_cat1_trend,sns_cat1_trends,by=c('DATE','CAT1'))
trend <- left_join(trend, naver_trend, by=c('DATE','CAT1'))
str(trend) #잘되었나 보자보자 어디보자
trend <- rename(trend, "QTY" = "V1") #변수명 예쁘게~
nrow(trend)


cor(trend[,c(3,4,5)] )

write.csv(trend,"G:/내 드라이브/2021-1/날씨와 소비/data/naver_trends/trend.csv",row.names = FALSE)

### 정규화
#### 정규화하여 같은 범위 안에 두고 시각화하면 잘 보일 것 같아서
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
  
}
head(trend)
trend_1 <- normalize(trend[,c(3,4,5)])
head(trend_1)

### SVI 
#### 네이버의 지수 계산 방식과 동일하게 계산하여 트렌드를 보면 더 잘 보일 것 같아서~

svi <- function(x){
  round((x/max(x))*100,0)
} #자체 함수 생성

trend_svi <- trend
trend_svi$CNT <- svi(trend$CNT)
trend_svi$QTY <- svi(trend$QTY)
str(trend_svi)

### 각 품목별 탐색
#### 정규화X, SVI X
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

#### 정규화 했을 때 각 대분류별 트렌드
#### SVI 했을 때 각 대분류별 트렌드


### Cross Correlation Functions
#### 참고: https://online.stat.psu.edu/stat510/lesson/8/8.2
#### 판매량, 언급량, 클릭량의 교차상관을 보자보자
head(trend_food)
str(trend)
trend_food_QTY <- ts(trend_food[,c(1,2)])
trend_food_CNT <- ts(trend_food[,c(1,3)])
trend_food_NTREND <- ts(trend_food[,c(1,4)])

ccf_food_QTYCNT_values = ccf(trend_food_QTY[,2],trend_food_CNT[,2])
ccf_food_QTYCNT_values
cor(trend_food_QTY[,2],trend_food_CNT[,2])

ccf(trend_food_QTY[,2],trend_food_NTREND[,2])
ccf_food_QTYNTREND_values = ccf(trend_food_QTY[,2],trend_food_NTREND[,2])
ccf_food_QTYNTREND_values

ccf_food_CNTNTREND_values = ccf(trend_food_CNT[,2],trend_food_NTREND[,2])
ccf_food_CNTNTREND_values
cor(trend_food_CNT[,2],trend_food_NTREND[,2])

#### Beauty
trend_beauty_QTY <- ts(trend_beauty[,c(1,2)])
trend_beauty_CNT <- ts(trend_beauty[,c(1,3)])
trend_beauty_NTREND <- ts(trend_beauty[,c(1,4)])

ccf_beauty_QTYCNT_values = ccf(trend_beauty_QTY[,2],trend_beauty_CNT[,2])
ccf_beauty_QTYCNT_values
cor(trend_beauty_QTY[,2],trend_beauty_CNT[,2])

ccf(trend_beauty_QTY[,2],trend_beauty_NTREND[,2])
ccf_beauty_QTYNTREND_values = ccf(trend_beauty_QTY[,2],trend_beauty_NTREND[,2])
ccf_beauty_QTYNTREND_values

ccf_beauty_CNTNTREND_values = ccf(trend_beauty_CNT[,2],trend_beauty_NTREND[,2])
ccf_beauty_CNTNTREND_values
cor(trend_beauty_CNT[,2],trend_beauty_NTREND[,2])

#### appliance
trend_app_QTY <- ts(trend_app[,c(1,2)])
trend_app_CNT <- ts(trend_app[,c(1,3)])
trend_app_NTREND <- ts(trend_app[,c(1,4)])

ccf_app_QTYCNT_values = ccf(trend_app_QTY[,2],trend_app_CNT[,2])
ccf_app_QTYCNT_values
cor(trend_app_QTY[,2],trend_app_CNT[,2])

ccf(trend_app_QTY[,2],trend_app_NTREND[,2])
ccf_app_QTYNTREND_values = ccf(trend_app_QTY[,2],trend_app_NTREND[,2])
ccf_app_QTYNTREND_values
cor(trend_app_QTY[,2],trend_app_NTREND[,2])

ccf_app_CNTNTREND_values = ccf(trend_app_CNT[,2],trend_app_NTREND[,2])
ccf_app_CNTNTREND_values
cor(trend_app_CNT[,2],trend_app_NTREND[,2])

#### 기상 변수들과 교차상관을 보자


