# 데이터 분석 준비하기 

# 패키지 준비하기 
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 준비하기 

# 데이터 불러오기 
raw_welfare = read.spss(file = "E:/08_R_20_jmj/RclassMJ/Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)

welfare = raw_welfare

# 데이터 검토하기 
head(welfare) 

tail(welfare) 

View(welfare) 

dim(welfare) 

str(welfare) 

summary(welfare)

welfare = rename(welfare, 
                 sex = h10_g3, # 성별 
                 birth = h10_g4, # 태어난 연도 
                 marriage = h10_g10, # 혼인 상태 
                 religion = h10_g11, # 종교 
                 income = p1002_8aq1, # 월급 
                 code_job = h10_eco9, # 직종 코드 
                 code_region = h10_reg7) # 지역 코드

# 성별에 따른 월급 차이 - "성별에 따라 월급이 다를까?"

# 분석 절차 
# 1. 변수 검토 및 전처리 
# 성별
# 월급 

# 2. 변수 간 관계 분석 
# 성별 월급 평균표 만들기 
# 그래프 만들기 

# 성별 변수 검토 및 전처리 

# 1. 변수 검토하기 
class(welfare$sex)
table(welfare$sex)

# 2. 전처리 

# 이상치 확인 
table(welfare$sex)

# 이상치 결측 처리 
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인 
table(is.na(welfare$sex))

# 성별 항목 이름 부여 
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

qplot(welfare$sex)

# 월급 변수 검토 및 전처리 

# 1. 변수 검토하기 
class(welfare$income)
summary(welfare$income)

qplot(welfare$income)

qplot(welfare$income) + xlim(0, 1000)

# 이상치 확인
summary(welfare$income)

# 이상치 결측 처리 
welfare$income = ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인 
table(is.na(welfare$income))

# 성별에 따른 월급 차이 분석하기 

# 1. 성별 월급 평균표 만들기 
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

# 2. 그래프 만들기 
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 나이와 월급의 관계 - "몇 살 때 월급을 가장 많이 받을까?"

# 분석 절차

# 1. 변수 검토 및 전처리 
# 나이
# 월급 

# 2. 변수 간 관계 분석 
# 나이에 따른 월급 평균표 만들기 
# 그래프 만들기 

# 1. 변수 검토하기 
class(welfare$birth)

summary(welfare$birth)

qplot(welfare$birth)

# 2. 전처리 

# 이상치 확인 
summary(welfare$birth)

# 결측치 확인 
table(is.na(welfare$birth))

# 이상치 결측 처리 
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 3. 파생변수 만들기 - 나이 
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)

qplot(welfare$age)

# 나이와 월급의 관계 분석하기 

# 1. 나이에 따른 월급 퍙균표 만들기 
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

# 2. 그래프 만들기 
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

# 연령대에 따른 월급 차이 - "어떤 연력대의 월급이 가장 많을까?" 

# 분석 절차 

# 1. 변수 검토 및 전처리 
# 연령대 
# 월급 

# 2. 변수 간 관계 분석 
# 연령대별 월급 평균표 만들기 
# 그래프 만들기 

# 연령대 변수 검토 및 전처리하기 

# 파생변수 만들기 - 연령대 
welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)

qplot(welfare$ageg)

# 10 ~ -- 만들어보기 
welfare = welfare %>% 
  mutate(ageg = ifelse(age <=10, "10",
                       ifelse(age <=20, "20",
                              ifelse(age <=30, "30",
                                     ifelse(age <= 40, "40",
                                            ifelse(age <= 50, "50",
                                                   ifelse(age <=60, "60",
                                                          ifelse(age <= 70, "70", "old"))))))))
table(welfare$ageg)

qplot(welfare$ageg)

# 연령대에 따른 월급 차이 분석하기 

# 1. 연령대별 월급 평균표 만들기 
ageg_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income

# 2. 그래프 만들기 
ggplot(data = ageg_income, aes(x = ageg, y = mean_income))

# 막대 정렬 : 초년, 중년
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

# 연령대 및 성별 월급 차이 - "성별 월급 차이는 연령대별로 다를까?" 

# 분석 절차 

# 1. 변수 검토 및 전처리 
# 연령대 
# 성별 
# 월급 

# 2. 변수 간 관계 분석 
# 연령대 및 성별 월급 평균표 만들기 
# 그래프 만들기 

# 연령대 및 성별 월급 차이 분석하기 

# 1. 연령대 및 성별 월급 평균표 만들기 
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))

sex_income

# 2. 그래프 만들기 
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + 
  geom_col() + 
  scale_x_discrete(limits = c("young", "middle", "old"))

# 성별 막대 분리 
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + 
  geom_col(position = "dodge") + 
  scale_x_discrete(limits = c("young", "middle", "old"))

# 나이 및 성별 월급 차이 분석하기 

# 성별 연령별 월급 평균표 만들기 
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

# 2. 그래프 만들기 
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()

# 직업별 월급 차이 - "어떤 직업이 월급을 가장 많이 받을까?" 


# 분석 절차 

# 1. 변수 겸토 및 전처리 
# 직업 
# 월급 

# 2. 변수 간 관계 분석 
# 직업별 월급 평균표 만들기 
# 그래프 만들기 

# 1. 변수 검토하기 
class(welfare$code_job)

table(welfare$code_job)

# 2. 전처리 

# 직업분류코드 목록 불러오기 
library(readxl)

list_job = read_excel("E:/08_R_20_jmj/HisRClass/Data/Koweps_Codebook.xlsx", col_names = T, sheet = 2)

head(list_job)

dim(list_job)

# welfare에 직업명 결합 
welfare = left_join(welfare, list_job, id = "code_job")

welfare %>%
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

# 직업별 월급 차이 분석하기 

# 1. 직업별 월급 평균표 만들기 
job_income = welfare %>%
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

# 2. 상위 10개 추출 
top10 = job_income %>%
  arrange(desc(mean_income)) %>% 
  head(10)

top10

# 3. 그래프 만들기 
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

# 4. 하위 10개 추출 
bottom10 = job_income %>%
  arrange(mean_income) %>% 
  head(10)

bottom10

# 5. 그래프 만들기 
ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col() + 
  coord_flip() +
  ylim(0, 850)
