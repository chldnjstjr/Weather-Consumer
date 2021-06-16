#Dataset list 
#list <- dbGetQuery(conn, "show tables")
#list

#buy2018_1 <- dbGetQuery(conn, "select * from buy2018_1")
#buy2018_2 <- dbGetQuery(conn, "select * from buy2018_2")
#buy2019_1 <- dbGetQuery(conn, "select * from buy2019_1")
#buy2019_2 <- dbGetQuery(conn, "select * from buy2019_2")

#rbind 
#https://rfriend.tistory.com/225
#library(data.table)
#trans <- rbindlist(list(buy2018_1, buy2018_2, buy2019_1, buy2019_2))
#head(trans)
#nrow(trans)
#write.csv(trans, "trans.csv", fileEncoding = "utf-8")
#sum(is.na(trans))
#names(trans) <- c("DATE","SEX","AGE","CAT1","CAT2", "QTY")
#str(trans)

trans <- read.csv("G:/내 드라이브/2021-1/날씨와 소비/data/trans.csv",  header = TRUE, fileEncoding = "UTF-8")
head(trans)
sum(is.na(trans))

#인구 통계
attach(trans)
##
###SEX
table(SEX)
sex_freq <- table(SEX)
round(prop.table(sex_freq)*100,1)
barplot(sex_freq)

###AGE
table(AGE)
age_freq <- table(AGE)
round(prop.table(age_freq)*100,1)
install.packages("plotrix")
library(plotrix)

###AGE*SEX
age_sex <- ftable(AGE~SEX, data=trans) #연령별 성별 분포
barplot(age_sex)
round(prop.table(age_sex)*100, 1) #연령 성별 분포 

## Product
### Catergory
table(CAT1)
table(CAT2)
cat1_freq <- table(CAT1)
cat2_freq <- table(CAT2)
barplot(cat1_freq)
prop.table(cat1_freq)*100
tail(sort(prop.table(cat2_freq)*100), n=10) #가장 높은게 1%도 안됨


#왜 생수가 여러개? 

### QTY
#install.packages("dplyr")
library(dplyr)

#### 연속형변수에 대한 기초적인 분포 확인
summary(QTY)
install.packages("pastecs")
library(pastecs)
round(stat.desc(QTY),2) #총판매랑 3억9천, 한 번에 최다판매 2774개, 평균이 19, 중앙 6
hist(QTY,breaks="FD",xlab="판매량", main="판매량 히스토그램")#거의 무슨 카이제곱분포

##### 이상치 제거해보기
Q1 = quantile(QTY,probs = c(0.25),na.rm = TRUE) #1분위수
Q3 = quantile(QTY,probs = c(0.75),na.rm = TRUE) #3분위수
LC = Q1 - 1.5 * (Q3 - Q1) # 아래 울타리
UC = Q3 + 1.5 * (Q3 - Q1) # 위 울타리
QTY_outlier_none = subset(QTY,QTY >  LC & QTY < UC)
hist(QTY_outlier_none,xlab="판매량", main="판매량 히스토그램")#거의 무슨 카이제곱분포

### CAT*QTY
trans %>% 
  select(CAT1,CAT2,QTY) %>%
  arrange(desc(QTY)) %>%
  head(10)

aggregate(QTY, by=list(CAT1,CAT2), FUN=sum)

### SEX*CAT*QTY
aggregate(QTY, by=list(SEX,CAT1,CAT2), FUN=sum)
aggregate(QTY, by=list(SEX,CAT1,CAT2), FUN=mean)
ddply(Fruits, .(Year), mutate, average = mean(Sales), avg_per = average/100) 

### AGE*CAT*QTY
aggregate(QTY, by=list(AGE,CAT1,CAT2), FUN=sum)
aggregate(QTY, by=list(AGE,CAT1,CAT2), FUN=mean)

### SEX*AGE*CAY*QTY
aggregate(QTY, by=list(SEX,AGE,CAT1,CAT2), FUN=sum)
aggregate(QTY, by=list(SEX,AGE,CAT1,CAT2), FUN=mean)
