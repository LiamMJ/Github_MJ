# 성별 직업 빈도 - "성별로 어떤 직업이 가장 많을까?" 

# 분석 절차 

# 1. 변수 검토 및 전처리 
# 성별 
# 직업 

# 2. 변수 간 관계 분석 
# 성별 직업 빈도표 만들기
# 그래프 만들기 

# 성별 직업 빈도 분석하기 

# 1. 성별 직업 빈도표 만들기 

# 남성 직업 빈도 상위 10개 추출 
job_male = welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_male

# 여성 직업 빈도 상위 10개 추출 
job_female = welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female

# 2. 그래프 만들기 

# 남성 직업 빈도 상위 10개 직업 
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# 여성 직업 빈도 상위 10개 직업 
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# 종교 유무에 따른 이혼율 - "종교가 있는 사람들이 이혼을 덜 할까?" 

# 분석 절차 

# 1. 변수 검토 및 전처리 
# 종교
# 혼인 상태 

# 2. 변수 간 관계 분석 
# 종교 유무에 따른 이혼율 표 만들기 
# 그래프 만들기 

# 종교 변수 검토 및 전처리 하기 

# 1. 변수 검토하기 
class(welfare$religion)

table(welfare$religion)

# 2. 전처리 

# 종교 유무 이름 부여 
welfare$religion = ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)

qplot(welfare$religion)

# 혼인 상태 변수 검토 및 전처리 하기 

# 1. 변수 검토하기 
class(welfare$marriage)

table(welfare$marriage)

# 2. 전처리 
welfare$group_marriage = ifelse(welfare$marriage == 1, "marriage",
                         ifelse(welfare$marriage == 3, "divorce", NA))

table(welfare$group_marriage)

table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

# 종교 유무에 따른 이혼율 분석하기 

# 1. 종교 유무에 따른 이혼율 표 만들기 
religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n / tot_group * 100, 1))

religion_marriage

# count() 활용 
religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n / sum(n) * 100, 1))

# 2. 이혼율 표 만들기 

# 이혼 추출 
divorce = religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce

# 3. 그래프 만들기 
ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

# 연령대 및 종교 유무에 따른 이혼율 분석하기 

# 1. 연령대별 이혼율 표 만들기 
ageg_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n / tot_group * 100, 1))

ageg_marriage

# count() 활용 
ageg_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

# 2. 연령대별 이혼율 그래프 만들기 

# 초년 제외, 이혼 추출 
ageg_divorce = ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg, pct)

ageg_divorce

# 그래프 만들기 
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()

# 3. 연령대 및 종교 유무에 따른 이혼율 표 만들기 

# 연령대, 종교유무, 결혼상태별 비율표 만들기 
ageg_religion_marriage = welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n / tot_group * 100, 1))

ageg_religion_marriage

# count() 활용 
ageg_religion_marriage = welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

# 연령대 및 종교 유무별 이혼율 표 만들기 
df_divorce = ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)

df_divorce

# 4. 연령대 및 종교 유무에 따른 이혼율 그래프 만들기 
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = "dodge")

# 지역별 연령대 비율 - "노년층이 많은 지역은 어디일까?" 

# 분석 절차 

# 1. 변수 검토 및 전처리 
# 지역 
# 연령대 

# 2. 변수 간 관계 분석 
# 지역별 연령대 비율표 만들기 
# 그래프 만들기 

# 지역 변수 검토 및 전처리 하기 

# 1. 변수 검토하기 
class(welfare$code_region)

table(welfare$code_region)

# 2.전처리 

# 지역 코드 목록 만들기 
list_region = data.frame(code_region = c(1:7),
                         region = c("서울",
                                    "수도권(인천/경기)",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))

list_region

# welfare에 지역명 변수 추가 
welfare = left_join(welfare, list_region, id = "code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

# 지역별 연령대 비율 분석하기 

# 1. 지역별 연령대 비율표 만들기 
region_ageg = welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n / tot_group * 100, 2))

head(region_ageg)

# count() 활용 
region_ageg = welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))

# 2. 그래프 만들기 
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + 
  geom_col() + 
  coord_flip()

# 3. 막대 정렬하기 : 노년층 비율 높은 순 

# 노년층 비율 내림차순 정렬 
list_order_old = region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)

list_order_old

# 지역명 순서 변수 만들기 
order = list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = order)

# 4. 연령대 순으로 막대 색깔 나열하기 
class(region_ageg$ageg)

levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg, 
                           level = c("old", "middle", "young"))

class(region_ageg$ageg)

levels(region_ageg$ageg)

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = order)
