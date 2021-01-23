library(dplyr)

ggr <- read.csv("GDP_성장률.csv", stringsAsFactors = F, header = T, na.strings = "-")
hdi <- read.csv("인간개발지수.csv", stringsAsFactors = F, header = T, na.strings = "-")

# 소속된 대륙을 나타내는 열 추가
ggr$대륙 <- "아시아"
ggr$대륙[50:54] <- "북아메리카"
ggr$대륙[55:91] <- "남아메리카"
ggr$대륙[92:135] <- "유럽"
ggr$대륙[136:186] <- "아프리카"
ggr$대륙[187:204] <- "오세아니아"

colnames(ggr) <- c("국가", "2015년", "2016년", "2017년", "2018년", "대륙") 

ggr <- ggr[-c(1, 50, 55, 92, 136, 187),] # 행에 존재했던 지역이름 제거
ggr <- na.omit(ggr) # na 존재하는 행 제거.
hdi <- na.omit(hdi)

for(i in 1:length(ggr$국가)){
  ggr$평균성장률[i] <- (ggr$`2015년`[i] + ggr$`2016년`[i] + ggr$`2017년`[i] + ggr$`2018년`[i])/4
}

colnames(hdi) <- c("국가", "HDI_순위", "인간개발지수", "기대수명", 
                   "평균교육기간", "기대교육기간", "1인당_GNI")

df <- inner_join(ggr, hdi, by = "국가")

result <- lm(평균성장률 ~ 대륙 + HDI_순위 + 평균교육기간 + 기대교육기간, data = df)
summary(result)

# 아시아 국가면 성장률 평균이 1.59 증가..?
# 전부 유의도가 0.05보다 높다..종속변수에 영향을 미치지 않는다?
# 근데 F값 유의도가 0.2339라서 회귀모형이 그닥 의미 있어 보이지는 않아.
# 종속변수의 분산 중에서 독립변수에 의해 결정되는 분산은 5.9퍼 정도.

library(QuantPsyc)
lm.beta(result)
round(lm.beta(result), 2)
# 가장 큰 영향을 미치는 독립변수는 아시아???

library(car)
vif(result)

