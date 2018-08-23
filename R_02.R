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
