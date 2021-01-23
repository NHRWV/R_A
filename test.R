if("condition" == "condition") {
  print(123)
}

#install.packages("Hmisc")
library(Hmisc)

setwd("C:/Users/Namlister/Desktop/Assignments/R_A")
test <- spss.get("04-1 중2 패널 1차년도 데이터(SPSS).sav",
                 use.value.labels = FALSE)

dim(test)
names(test)
str(test)

test <- read.csv("04-1 중2 패널 1차년도 데이터(SPSS).csv",
                  header = TRUE, sep = ",")

names(test)[1] <- "id"
str(test)


select_variables <- c("id", "sexw1", "scharew1", "areaw1",
                      "q2w1", "q18a1w1", "q18a2w1", "q18a3w1", "q33a01w1",
                      "q33a02w1", "q33a03w1", "q33a04w1", "q33a05w1", "q33a06w1",
                      "q33a07w1", "q33a08w1", "q33a09w1", "q33a10w1", "q33a12w1",
                      "q33a13w1", "q33a14w1", "q33a15w1", "q34a1w1", "q34a2w1",
                      "q34a3w1", "q34a4w1", "q34a5w1", "q34a6w1", "q37a01w1",
                      "q37a02w1", "q37a03w1", "q37a04w1", "q48a01w1", "q48a02w1",
                      "q48a03w1", "q48a04w1", "q48a05w1", "q48a06w1", "q48b1w1",
                      "q48b2w1", "q48b3w1", "q48c1w1", "q48c2w1", "q48c3w1",
                      "q48c4w1", "q48c5w1", "q48c6w1", "q50w1")

test1 <- test[select_variables]

csvdata <- test1[which(test1$scharew1 >= 100 &
                         test1$scharew1 < 200),]
csvdata

table(csvdata$q33a07w1)
csvdata$q33a07w1[csvdata$q33a07w1 == 9] <- NA
table(csvdata$q33a07w1, useNA = "ifany")

spssdata <- csvdata #강의노트에 spssdata로 잘못 표기되어 있다.
#So I changed the name to follow the lecturenote.

spssdata$attachment <- spssdata$q33a01w1+spssdata$q33a02w1+
  spssdata$q33a03w1+spssdata$q33a04w1+spssdata$q33a05w1+
  spssdata$q33a06w1

str(spssdata) #see the change in spssdata

attach(spssdata)
spssdata$attachment <- q33a01w1+q33a02w1+q33a03w1+q33a04w1+q33a05w1+q33a06w1
detach(spssdata)

str(spssdata)

attach(spssdata)
spssdata$grade <- q18a1w1+q18a2w1+q18a3w1
detach(spssdata)

str(spssdata) #You just have added a new column in spssdata.

table(spssdata$grade) #it shows the number of the each grade..
prop.table(table(spssdata$grade)) #it shows the propotions of the each grade

hist(spssdata$grade, label = T, right = T)
ggplot(data = spssdata, aes(x=spssdata$grade)) +
  geom_histogram(fill="lightgreen",
                 bins = 13,
                 color="grey50") +
  labs(title = "Histogram for Grades", x = "Grade", y = "Count")
# 솔직히 나도 잘 모른다.
round(prop.table(table(spssdata$grade)), 2)

attach(spssdata)
spssdata$grp.grade[grade<=8] <- 1
spssdata$grp.grade[grade>=9 & grade<=10] <- 2
spssdata$grp.grade[grade>=11] <- 3
detach(spssdata)

str(spssdata)
table(spssdata$grp.grade)

attach(spssdata)
spssdata$satisfaction[q50w1<=2] <- 1 # 만족하지 못하는 편
spssdata$satisfaction[q50w1==3] <- 2 # 보통
spssdata$satisfaction[q50w1>=4] <- 3 # 만족하는 편
detach(spssdata)

attach(spssdata)
spssdata$rq48a04w1[q48a04w1==1] <- 5
spssdata$rq48a04w1[q48a04w1==2] <- 4
spssdata$rq48a04w1[q48a04w1==3] <- 3
spssdata$rq48a04w1[q48a04w1==4] <- 2
spssdata$rq48a04w1[q48a04w1==5] <- 1
spssdata$rq48a05w1[q48a05w1==1] <- 5
spssdata$rq48a05w1[q48a05w1==2] <- 4
spssdata$rq48a05w1[q48a05w1==3] <- 3
spssdata$rq48a05w1[q48a05w1==4] <- 2
spssdata$rq48a05w1[q48a05w1==5] <- 1
spssdata$rq48a06w1[q48a06w1==1] <- 5
spssdata$rq48a06w1[q48a06w1==2] <- 4
spssdata$rq48a06w1[q48a06w1==3] <- 3
spssdata$rq48a06w1[q48a06w1==4] <- 2
spssdata$rq48a06w1[q48a06w1==5] <- 1
detach(spssdata) # you are fixing the number, according to the survey result.

attach(spssdata)
spssdata$grp.sex.grade[sexw1==1 & grp.grade==1] <- 11
spssdata$grp.sex.grade[sexw1==1 & grp.grade==2] <- 12
spssdata$grp.sex.grade[sexw1==1 & grp.grade==3] <- 13
spssdata$grp.sex.grade[sexw1==2 & grp.grade==1] <- 21
spssdata$grp.sex.grade[sexw1==2 & grp.grade==2] <- 22
spssdata$grp.sex.grade[sexw1==2 & grp.grade==3] <- 23
detach(spssdata)

spssdata$q33a07w1[spssdata$q33a07w1==9] <- NA

#save.image함수 알아두기 

# 위에서 나오는 변수방향 역으로 재부호화하기 한 줄로 해보기 
# 예를 들면 이렇게 spssdata$rq48a04w1 <- 6 - q48a04w1

#View(spssdata)



# 날짜관련 변수의 사용
# 관련 데이터프레임 만들기
id <- c(1, 2, 3, 4, 5)
born <- c("1989-02-13", "1990-05-25", "1992-11-30", "1993-07-01",
          "1991-09-22")
first.crime <- c("2007-05-17", "2009-02-21", "2006-09-01",
                 "2009-08-19", "2010-01-02")
second.crime <- c("2010-03-10", "2011-10-01", "2007-12-21",
                  "2012-01-05", "2015-10-12")
sampledata <- data.frame(id, born, first.crime, second.crime)
sampledata
str(sampledata$born) # since I did not set stringsAsFactors as F above, it is set as default.

sampledata$born.date <- as.Date(sampledata$born)
sampledata$first.crime.date <- as.Date(sampledata$first.crime)
sampledata$second.crime.date <- as.Date(sampledata$second.crime)
str(sampledata$born.date)

# 나이계산하기
today <- Sys.Date()
difftime(today, sampledata$born.date, units="days")
# 재범기간 계산하기
difftime(sampledata$second.crime.date, sampledata$first.crime.date,
         units="days")

# 변수를 병합하는 경우
# 2차년도 데이터 불러오기
second <- read.table("04-2 중2 패널 2차년도 데이터(SPSS).csv",
                     header=TRUE, sep=",")
names(second)[1] <- "id"
# 데이터 합치기
mergedata <- merge(spssdata, second, by="id") #id값 기준. 레코드의 유일성을 증명하는 PK값 
dim(mergedata)

# spssdata의 첫 세 변수만 추출하는 경우 1
newdata <- spssdata[,c(1:3)]
# spssdata의 첫 세 변수만 추출하는 경우 2
var <- c("id", "sexw1", "scharew1")
newdata <- spssdata[var]

# spssdata의 처음 5개 사례만 추출하는 경우
newdata <- spssdata[1:5,]
# 남자만 선택하는 경우
newdata <- spssdata[which(spssdata$sexw1==1),]
# 남자 중에서 성적이 낮은 경우만 선택하는 경우
#newdata <- spssdata[which(spssdata$sexw1==1 & grp.grade==1),]

# spssdata[,spssdata$sexw1==1 & grp.grade==1] 이렇게 해도 되지 않나 


# subset 함수를 사용하여 사례와 변수를 추출하는 경우
newdata <- subset(test, scharew1 >= 100 & scharew1 < 200, select=
                    c("id", "sexw1", "scharew1", "areaw1", "q2w1",
                      "q18a1w1", "q18a2w1", "q18a3w1", "q33a01w1", "q33a02w1", "q33a03w1",
                      "q33a04w1", "q33a05w1", "q33a06w1", "q33a07w1", "q33a08w1",
                      "q33a09w1", "q33a10w1", "q33a12w1", "q33a13w1", "q33a14w1",
                      "q33a15w1", "q34a1w1", "q34a2w1", "q34a3w1", "q34a4w1", "q34a5w1",
                      "q34a6w1", "q37a01w1", "q37a02w1", "q37a03w1", "q37a04w1",
                      "q48a01w1", "q48a02w1", "q48a03w1", "q48a04w1", "q48a05w1",
                      "q48a06w1", "q48b1w1", "q48b2w1", "q48b3w1", "q48c1w1", "q48c2w1",
                      "q48c3w1", "q48c4w1", "q48c5w1", "q48c6w1", "q50w1"))

save(spssdata, file="spssdata.Rda") #작업 데이터 저장
rm(spssdata) #spssdata 삭제
load("spssdata.Rda") #저장 데이터 불러들이기

##############################################################################################
##############################################################################################
### test for normality
## Generate two data sets
## First Normal, second from a t-distribution
words1 = rnorm(100); words2 = rt(100, df=3) #if df is large enough, t-dist approaches Z-dist
## Plot histogram
par(mfrow=c(1,2))
hist(words1, breaks = 10, col = 2)
hist(words2, breaks = 10, col = 2)
## Perform Shapiro-Wilk normality test
shapiro.test(words1); shapiro.test(words2)
## Perform Kolmogorov-Smirnov test
ks.test(words1, rnorm(100));ks.test(words2, rnorm(100))
## Plot using a qqplot
qqnorm(words1);qqline(words1, col = 2)
qqnorm(words2);qqline(words2, col = 2)
par(mfrow=c(1,1))




var.test(attachment ~ sexw1, data=spssdata)

t.test(attachment ~ sexw1, var.equal=TRUE, data=spssdata)

library(psych)
describeBy(spssdata$attachment, spssdata$sexw1) #치우치지도 솟아있지도 않은 예쁜 데이터.
4.37/sqrt(296)
#skim package?


library(gplots)
plotmeans(attachment ~ sexw1, data = spssdata, xlab="성별",
          ylab="부모에 대한 애착", ci.label=TRUE, mean.label=TRUE, ylim=c(18, 22),
          barwidth=5, main="성별에 따른 부모에 대한 애착 수준", digits=3, pch="*")



second <- read.table("04-2 중2 패널 2차년도 데이터(SPSS).csv",
                     header=TRUE, sep=",")
names(second)[1] <- "id"
mergedata <- merge(spssdata, second, by="id")

attach(mergedata)
mergedata$rq48a04w1 <- 6 - q48a04w1 #already made
mergedata$rq48a05w1 <- 6 - q48a05w1 #already made
mergedata$rq48a06w1 <- 6 - q48a06w1 #already made
mergedata$rq48a04w2 <- 6 - q48a04w2
mergedata$rq48a05w2 <- 6 - q48a05w2
mergedata$rq48a06w2 <- 6 - q48a06w2
mergedata[c('q48a04w2', 'rq48a04w2')]
detach(mergedata)

attach(mergedata)
mergedata$self.esteemw1 <- q48a01w1+q48a02w1+q48a03w1+rq48a04w1+
  rq48a05w1+rq48a06w1
mergedata$self.esteemw2 <- q48a01w2+q48a02w2+q48a03w2+rq48a04w2+
  rq48a05w2+rq48a06w2
detach(mergedata)

t.test(mergedata$self.esteemw1, mergedata$self.esteemw2, paired=TRUE)

sum()

# ⑤ 기술통계량 구하기
# 기술통계량을 구할 대상 변수를 선택하여 데이터로 만들기
describe_self.esteem <- mergedata[c("self.esteemw1", "self.esteemw2")]
# ‘psych’ 패키지의 describe 함수로 기술통계량 구하기
describe(describe_self.esteem)

describe_self.esteem$none_self.esteemw1 <- describe_self.esteem$self.esteemw1
describe_self.esteem$none_self.esteemw1[is.na(describe_self.esteem
                                              $self.esteemw1)|is.na(describe_self.esteem$self.esteemw2)] <- NA
describe_self.esteem$none_self.esteemw2 <- describe_self.esteem$self.esteemw2
describe_self.esteem$none_self.esteemw2[is.na(describe_self.esteem
                                              $self.esteemw1)|is.na(describe_self.esteem$self.esteemw2)] <- NA
describe(describe_self.esteem)


describe_self.esteem$none_self.esteemw1[is.na(describe_self.esteem
                                              $self.esteemw1)|is.na(describe_self.esteem$self.esteemw2)] <- NA
describe_self.esteem$none_self.esteemw2 <- describe_self.esteem$self.esteemw2
describe_self.esteem$none_self.esteemw2[is.na(describe_self.esteem
                                              $self.esteemw1)|is.na(describe_self.esteem$self.esteemw2)] <- NA
describe(describe_self.esteem)




## paired sample t-test (step by step)
## 1) paired sample of self-esteem
x1 <- describe_self.esteem$none_self.esteemw1
x2 <- describe_self.esteem$none_self.esteemw3
## 2) difference of x1 and x2
diff_x <- x1 - x2
## 3) mean of differences
mean_diff_x <- mean(diff_x, na.rm = T)
mean_diff_x
## 4) standard deviation of differences
sd_diff_x <- sd(diff_x, na.rm = T)
sd_diff_x
## 5) valid number of elements
n_diff_x <- length(diff_x) - sum(is.na(diff_x))
n_diff_x
## 6) t statistic and p value
t_x <- mean_diff_x/(sd_diff_x/sqrt(n_diff_x))
t_x
pt(t_x, n_diff_x-1) * 2
## Compare with t.test
t.test(x1, x2, alternative = "two.sided", paired = T, conf.level = 0.95)

#######일표본 T검증

# ① 분석에 사용되는 변수 만들기
# 역부호화
attach(spssdata)
spssdata$rq34a1w1 <- 6 - q34a1w1
spssdata$rq34a2w1 <- 6 - q34a2w1
spssdata$rq34a3w1 <- 6 - q34a3w1
spssdata$rq34a4w1 <- 6 - q34a4w1
spssdata$rq34a5w1 <- 6 - q34a5w1
spssdata$rq34a6w1 <- 6 - q34a6w1
detach(spssdata)
# ② 자기통제 변수를 만들기 위한 변수 합치기
attach(spssdata)
spssdata$self.control <-
  rq34a1w1+rq34a2w1+rq34a3w1+rq34a4w1+rq34a5w1+rq34a6w1
detach(spssdata)

# ③ 일표본 T 검증
t.test(spssdata$self.control, mu=18.5)
t.test(spssdata$self.control, mu = 18.5, alternative = "two.sided", conf.level = .99)

