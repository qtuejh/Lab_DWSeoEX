rm(list = ls())
# setwd("C:/Users/lrdseries/Desktop/성균관대학교 대학원/응용통계연구소/의뢰/서동욱_삼성인력개발원/data")
setwd("C:/Users/ConditionKim/Desktop/성균관대학교 대학원/응용통계연구소/의뢰/서동욱_삼성인력개발원/data")

library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
library(tidytext)

library(doParallel)


source("../code/tfidf_library.R")

removed_text <- c("기업,회장,시장,사업,대표,세계,한국,투자,글로벌,서비스,미국,경제,경영,
                  산업,정부,회사,사장,분야,삼성전자,사회,제품,평가,확대,제공,부회장,
                  중국,사람,서울,예정,최고,규모,설명,교수,ceo,이사,생각,그룹,
                  고객,계획,교육,정책,진행,미래,성장,리더십,지원,강화,미래,변화,
                  경영,혁신,산업,가능,lg,삼성,시작,중요,sm,하이브,최대,중요,대상,
                  기준,주주,국가,일본,조직,중심,시대,강조,인수,이사회,가격,대비,사용,
                  지분,배터리,결과,수준,tv,정도,관계자,프로그램,지역,문화,중심,상황,
                  마련,게임,역할,업계,성공,임원,소비자,출시,방식,핵심,네이버,생산,환경,
                  공개,선정,적용,결정,총괄,이날,이수만,기관,자리,내년,대학,
                  모델,sk,활용,추진,조사,여성,직원,현대차,전문,선임,
                  전문가,구조,경영진,해외,제시,의미,부사장,달러,포함,행사,집중,
                  이용,의장,후보,공장,포함,인재,메모리,감산,유지,영향,기록,판매,활동,
                  확보,현장,바탕,구성,차량,맥주,기간,지급,평균,업체,확인,전년,지난달,청년,
                  시행,하루,기업들,단위,사태,공간,신종,상품,감염증,예상,사람들,지적,경기,
                  직원들,lh,지급,국민,신규,샌프란시스코,svb,gm,대통령,은행권,유출,
                  희망,코로나19,방역,콜센터,감염,안전,마스크,현대중공업,
                  카카오,현대모비스,어촌,노동자들,팬데믹,건물,자동차,버스,사측,타결,
                  대한항공,확산,확진자,PC,도시,롯데,대한민국,하이텔레서비스,아마존,
                  넷플릭스,비자,실리콘밸리,운영,a씨,거리,gs홈쇼핑,셀트리온,미만,
                  ls,셀트리온,일주일,인상,건설,노사,gs,형태,백신,마감,설문,물건,
                  확진,가구,직장,직장인,발생,확진,노동자,자녀,중소기업,근로자,제도,
                  인력,근무제,조치,임직원,사업장,증가,노동,소득,채용,의료,영업,개선,
                  임직원,사내,자유,응답자,경총,본점,센터,분위기,선택,직장인들,폐쇄,
                  포장,응답,사례,계열사,황금,브랜드,모습,도움,기기,노트북,검사,대기업,
                  균형,주장,지침,수요,고민,구성원,야간,어린이집,조성,질문,컨설팅,성과,
                  건강,소비,구축,조정,합의,부족,
                  근무,시간,회의,출근")

filter_text <- c("윤석열,문재인,이재명,민주노총,경사노위,노사정,청와대,경제사회노동위원회,
                  민노총,로톡,변호사,노동부,국회,고용부,포스코,
                  조합원,노조,아마존,실리콘밸리,바디프랜드,대웅제약,카풀,
                  경찰청장,ISO,고용노동청,유림개발,하이텔레서비스,장관")

#################################################
##                                             ##
##        Data preprocessing : Raw Data        ##
##                                             ##
#################################################
#################################
##                             ##
## 1. Data merge and filtering ##
##                             ##
#################################
#################################
## (1). organizational culture ##
#################################
# 일자, 언론사, 통합 분류1, 키워드
oc_merge <- read_bigkinds("raw_data/organizational_culture/NewsResult_20180101-20181231.xlsx",
                          filter_text = filter_text)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

oc_merge_filtered <- oc_merge %>% 
  mutate(keyword = parSapply(cl,
                             keyword,
                             make_text_clean,
                             removed_text,
                             simplify = T, USE.NAMES = F)) %>% 
  mutate(category = case_when(
    category == "미분류" ~ "미분류>미분류",
    TRUE ~ category)) %>% 
  separate(col = category, sep = ">", into = c("main_category", "sub_category")) 

stopCluster(cl)
rm(cl)

oc_covid_count_table <- make_keyword_count(oc_merge_filtered,
                                           keyword_col = "keyword",
                                           group_col = "covid_type")

oc_merge_filtered %>% 
  fwrite("preprocessed_data/organizational_culture.csv",
         row.names = F,
         na = c(NA, ""))

###########################
## (2). employee welfare ##
###########################
ew_merge <- read_bigkinds("raw_data/employee_welfare/NewsResult_20180101-20181231.xlsx",
                          filter_text = filter_text)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

ew_merge_filtered <- ew_merge %>% 
  mutate(keyword = parSapply(cl,
                             keyword,
                             make_text_clean,
                             removed_text,
                             simplify = T, USE.NAMES = F)) %>% 
  mutate(category = case_when(
    category == "미분류" ~ "미분류>미분류",
    TRUE ~ category)) %>% 
  separate(col = category, sep = ">", into = c("main_category", "sub_category")) 

stopCluster(cl)
rm(cl)

ew_covid_count_table <- make_keyword_count(ew_merge_filtered,
                                           keyword_col = "keyword",
                                           group_col = "covid_type")

ew_merge_filtered %>% 
  fwrite("preprocessed_data/employee_welfare.csv",
         row.names = F,
         na = c(NA, ""))

##############################
## (3). working environment ##
##############################
we_merge <- read_bigkinds(file = "raw_data/working_environment/NewsResult_20180101-20230415.xlsx",
                          filter_text = filter_text)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

we_merge_filtered <- we_merge %>% 
  mutate(keyword = parSapply(cl,
                             keyword,
                             make_text_clean,
                             removed_text,
                             simplify = T, USE.NAMES = F)) %>% 
  mutate(category = case_when(
    category == "미분류" ~ "미분류>미분류",
    TRUE ~ category)) %>% 
  separate(col = category, sep = ">", into = c("main_category", "sub_category")) 

stopCluster(cl)
rm(cl)

# we_covid_count_table <- make_keyword_count(we_merge_filtered,
#                                            keyword_col = "keyword",
#                                            group_col = "covid_type")

we_merge_filtered %>% 
  fwrite("preprocessed_data/working_environment.csv",
         row.names = F,
         na = c(NA, ""))

#################
## (4). career ##
#################
filter_text_career <- c("방시혁,JYP,베트남,BBQ,ENM,사모펀드,SK이노베이션,패션산업,
                        쏘카,병수씨,알파브라더스,MLB,탈원전,칙필레")
removed_text_career <- c("lg화학,간호사,cj,상사,방법,중간")

career_merge <- read_bigkinds("raw_data/career/NewsResult_20180101-20230415.xlsx",
                              filter_text = paste0(filter_text, filter_text_career))
  
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

career_merge_filtered <- career_merge %>% 
  mutate(keyword = parSapply(cl,
                             keyword,
                             make_text_clean,
                             paste0(removed_text, removed_text_career),
                             simplify = T, USE.NAMES = F)) %>% 
  mutate(category = case_when(
    category == "미분류" ~ "미분류>미분류",
    TRUE ~ category)) %>% 
  separate(col = category, sep = ">", into = c("main_category", "sub_category")) 

stopCluster(cl)
rm(cl)

career_covid_count_table <- make_keyword_count(career_merge_filtered,
                                               keyword_col = "keyword",
                                               group_col = "covid_type")

career_merge_filtered %>% 
  fwrite("preprocessed_data/career.csv",
         row.names = F,
         na = c(NA, ""))
