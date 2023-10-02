rm(list = ls())

setwd("/Users/fran/Desktop/Lab_Applied Statistics/request/DWSeo/code/")
library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
library(tidytext)
library(tm)
library(doParallel)
library(extrafont)
library(wordcloud)
library(RColorBrewer)

source("tfidf_library.r")
font_import()
y
theme_set(theme_grey(base_family='NanumGothic'))

##################
## 커리어(CA) ##
##################

library(ggwordcloud)

# 1. TF-IDF, TF
data_ca <- fread("../preprocessed_data/career.csv")

ca_count <- make_tf_table(data_ca, 
                          text_col = "keyword", 
                          group_col = "covid_type", 
                          top_n = 20)

geom_tf_plot(ca_count, x = "word", y = "n", facet_group = "covid_type", 
             title = "커리어",
             need_factor_arrange = T,
             factor_order = c("pre_covid19","covid19","post_covid19"))

ca_tfidf <- make_tf_idf_table(data_ca,
                              text_col = "keyword", 
                              group_col = "covid_type", 
                              top_n.1 = 50,
                              top_n.2 = 10)
geom_tf_plot(ca_tfidf, x = "word", y = "tf_idf", facet_group = "covid_type", 
             title = "근무환경",
             need_factor_arrange = T,
             factor_order = c("pre_covid19","covid19","post_covid19"))



# 2. 워드 클라우드
# (1) pre-corona
ca_count %>% 
  filter(covid_type == "pre_covid19") %>% 
  arrange(desc(n)) %>%  
  ggplot(aes(label = word, size = n,family = "AppleGothic",color = word)) + 
  geom_text_wordcloud_area(eccentricity=.5)+
  scale_size_area(max_size = 15) +
  theme_minimal()

# (2) corona
ca_count %>% 
  filter(covid_type == "covid19") %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(label = word, size = n,family = "AppleGothic",color = word)) + 
  geom_text_wordcloud_area(eccentricity=.5)+
  scale_size_area(max_size = 15) +
  theme_minimal()

# (3) post - corona
ca_count %>% 
  filter(covid_type == "post_covid19") %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(label = word, size = n,family = "AppleGothic",color = word)) + 
  geom_text_wordcloud_area(eccentricity=.5)+
  scale_size_area(max_size = 15) +
  theme_minimal()


# 3. 토픽모델링

#install.packages(c("cli","hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")

Sys.setenv("JAVA_HOME"='/Library/Java/JavaVirtualMachines/zulu-19.jdk/Contents/Home')
dyn.load("/Library/Java/JavaVirtualMachines/zulu-19.jdk/Contents/Home/lib/server/libjvm.dylib")

install.packages("remotes")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"),force = T)
library(KoNLP)
library(rJava)
library(tidyverse)
library(lda)
library(stringr)
library(tm)
library(topicmodels)
library(LDAvis)
library(servr)
library(LDAvisData)
library(ldatuning)
library(extrafont)
library(tidytext)
library(progress)
library(magrittr)

data_ca = fread("/Users/fran/Desktop/Lab_Applied Statistics/request/DWSeo/preprocessed_data/career.csv")


# (1) pre_covid

data_ca_pre = data_ca %>% mutate(
  article = str_replace_all(keyword,","," ")
) %>% filter(covid_type=="pre_covid19")



myRemove = content_transformer(function(x, pattern){return(gsub(pattern, "",x))})
toSpace = content_transformer(function(x, pattern){return(gsub(pattern," ",x))})

cps <- VCorpus(VectorSource(data_ca_pre$article))
cps = tm_map(cps, removePunctuation)
cps = tm_map(cps, removeNumbers)
cps = tm_map(cps, myRemove, '김혜민')
cps = tm_map(cps, myRemove, 'gs건설')

dtm <- DocumentTermMatrix(cps,
                          control = list(weighting= weightTf))
inspect(dtm)



result1 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to =15, by = 3),
  metrics = c("Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 4242),
  mc.cores = 8L,
  verbose = TRUE
)

FindTopicsNumber_plot(result1)

# LDA fit

lda_data = LDA(dtm, k=5, method = "Gibbs",
               control = list(seed=123,burnin=1000,iter=1000,thin=100))


(lda_data_term = tidy(lda_data, matrix="beta"))
lda_data_term %>% arrange(desc(beta))


lda_data_term %>% arrange(topic,desc(beta)) %>% 
  openxlsx::write.xlsx("../result/ca/pre_covid/붙임1.lda_beta_precovid.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(topic) %>% 
  ungroup() %>% group_by(topic) %>% 
  summarise(
    N = n()
  ) %>% 
  openxlsx::write.xlsx("../result/ca/pre_covid/붙임2.토픽별대표문서_precovid.xlsx")

tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice(which.max(gamma)) %>% 
  openxlsx::write.xlsx("../result/ca/pre_covid/붙임3.토픽별대표문서리스트_precovid.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(as.numeric(document)) %>% 
  openxlsx::write.xlsx("../result/ca/pre_covid/붙임4.문서별토픽_precovid.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  summarise(mean_p = mean(gamma)) %>% 
  arrange(topic) %>% 
  openxlsx::write.xlsx("../result/ca/pre_covid/붙임5.토픽출현율_평균_precovid.xlsx")

lda_term_top = lda_data_term %>%
  group_by(topic) %>% top_n(5,beta) %>%
  ungroup() %>% arrange(topic, -beta)

theme_set(theme_grey(base_family='NanumGothic'))

lda_term_top[lda_term_top$term%in%c("세일즈"),2]<-"성과급"
lda_term_top[lda_term_top$term%in%c("부동산"),2]<-"상여금"
lda_term_top[lda_term_top$term%in%c("베이직"),2]<-"연봉"

ggplot(lda_term_top, aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales="free")+
  labs(x=NULL, y="Word-Topic Probability (Beta)") + coord_flip()+
  theme(text = element_text(size = 7))


# (2) covid

data_ca_covid = data_ca %>% mutate(
  article = str_replace_all(keyword,","," ")
) %>% filter(covid_type=="covid19")


cps <- VCorpus(VectorSource(data_ca_covid$article))
cps = tm_map(cps, removePunctuation)
cps = tm_map(cps, removeNumbers)
cps = tm_map(cps, myRemove, '롯데푸드')
cps = tm_map(cps, myRemove, 'gs건설')
dtm <- DocumentTermMatrix(cps,
                          control = list(weighting= weightTf))
inspect(dtm)



result1 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to =15, by = 3),
  metrics = c("Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 4242),
  mc.cores = 8L,
  verbose = TRUE
)

FindTopicsNumber_plot(result1)

# LDA fit

lda_data = LDA(dtm, k=5, method = "Gibbs",
               control = list(seed=123,burnin=1000,iter=1000,thin=100))


(lda_data_term = tidy(lda_data, matrix="beta"))
lda_data_term %>% arrange(desc(beta))


lda_data_term %>% arrange(topic,desc(beta)) %>% 
  openxlsx::write.xlsx("../result/ca/covid19/붙임1.lda_beta_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(topic) %>% 
  ungroup() %>% group_by(topic) %>% 
  summarise(
    N = n()
  ) %>% 
  openxlsx::write.xlsx("../result/ca/covid19/붙임2.토픽별대표문서_covid19.xlsx")

tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice(which.max(gamma)) %>% 
  openxlsx::write.xlsx("../result/ca/covid19/붙임3.토픽별대표문서리스트_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(as.numeric(document)) %>% 
  openxlsx::write.xlsx("../result/ca/covid19/붙임4.문서별토픽_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  summarise(mean_p = mean(gamma)) %>% 
  arrange(topic) %>% 
  openxlsx::write.xlsx("../result/ca/covid19/붙임5.토픽출현율_평균_covid19.xlsx")

lda_term_top = lda_data_term %>%
  group_by(topic) %>% top_n(5,beta) %>%
  ungroup() %>% arrange(topic, -beta)

theme_set(theme_grey(base_family='NanumGothic'))



lda_term_top[lda_term_top$term%in%c("김효신"),2]<-"성과금"
lda_term_top[lda_term_top$term%in%c("최형진"),2]<-"상여금"
lda_term_top[lda_term_top$term%in%c("베이직"),2]<-"월급"



ggplot(lda_term_top, aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales="free")+
  labs(x=NULL, y="Word-Topic Probability (Beta)") + coord_flip()+
  theme(text = element_text(size = 7))


# (3) post_covid

data_ca_post = data_ca %>% mutate(
  article = str_replace_all(keyword,","," ")
) %>% filter(covid_type=="post_covid19")


cps <- VCorpus(VectorSource(data_ca_post$article))
cps = tm_map(cps, removePunctuation)
cps = tm_map(cps, removeNumbers)
cps = tm_map(cps, myRemove, 'bat')
cps = tm_map(cps, myRemove, 'BAT')
cps = tm_map(cps, myRemove, 'ces')
cps = tm_map(cps, myRemove, '오비맥주')
cps = tm_map(cps, myRemove, '롯데홈쇼핑')
cps = tm_map(cps, myRemove, '')
dtm <- DocumentTermMatrix(cps,
                          control = list(weighting= weightTf))
inspect(dtm)



result1 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to =15, by = 3),
  metrics = c("Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 4242),
  mc.cores = 8L,
  verbose = TRUE
)

FindTopicsNumber_plot(result1)

# LDA fit

lda_data = LDA(dtm, k=5, method = "Gibbs",
               control = list(seed=123,burnin=1000,iter=1000,thin=100))


(lda_data_term = tidy(lda_data, matrix="beta"))
lda_data_term %>% arrange(desc(beta))


lda_data_term %>% arrange(topic,desc(beta)) %>% 
  openxlsx::write.xlsx("../result/ca/post_covid/붙임1.lda_beta_post_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(topic) %>% 
  ungroup() %>% group_by(topic) %>% 
  summarise(
    N = n()
  ) %>% 
  openxlsx::write.xlsx("../result/ca/post_covid/붙임2.토픽별대표문서_post_covid19.xlsx")

tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice(which.max(gamma)) %>% 
  openxlsx::write.xlsx("../result/ca/post_covid/붙임3.토픽별대표문서리스트_post_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(as.numeric(document)) %>% 
  openxlsx::write.xlsx("../result/ca/post_covid/붙임4.문서별토픽_post_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  summarise(mean_p = mean(gamma)) %>% 
  arrange(topic) %>% 
  openxlsx::write.xlsx("../result/ca/post_covid/붙임5.토픽출현율_평균_post_covid19.xlsx")

lda_term_top = lda_data_term %>%
  group_by(topic) %>% top_n(5,beta) %>%
  ungroup() %>% arrange(topic, -beta)

theme_set(theme_grey(base_family='NanumGothic'))


ggplot(lda_term_top, aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales="free")+
  labs(x=NULL, y="Word-Topic Probability (Beta)") + coord_flip()+
  theme(text = element_text(size = 7))

