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
## 근무환경(WE) ##
##################
install.packages('ggwordcloud')
library(ggwordcloud)

# 1. TF-IDF, TF
data_we <- fread("../preprocessed_data/working_environment.csv")

we_count <- make_tf_table(data_we, 
                          text_col = "keyword", 
                          group_col = "covid_type", 
                          top_n = 20)

geom_tf_plot(we_count, x = "word", y = "n", facet_group = "covid_type", 
             title = "근무환경",
             need_factor_arrange = T,
             factor_order = c("pre_covid19","covid19","post_covid19"))

we_tfidf <- make_tf_idf_table(data_we,
                              text_col = "keyword", 
                              group_col = "covid_type", 
                              top_n.1 = 50,
                              top_n.2 = 10)
geom_tf_plot(we_tfidf, x = "word", y = "tf_idf", facet_group = "covid_type", 
             title = "근무환경",
             need_factor_arrange = T,
             factor_order = c("pre_covid19","covid19","post_covid19"))



# 2. 워드 클라우드
# (1) pre-corona
we_count %>% 
  filter(covid_type == "pre_covid19") %>% 
  arrange(desc(n)) %>%  
  ggplot(aes(label = word, size = n,family = "AppleGothic",color = word)) + 
  geom_text_wordcloud_area(eccentricity=.5)+
  scale_size_area(max_size = 15) +
  theme_minimal()

# (2) corona
we_count %>% 
  filter(covid_type == "covid19") %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(label = word, size = n,family = "AppleGothic",color = word)) + 
  geom_text_wordcloud_area(eccentricity=.5)+
  scale_size_area(max_size = 15) +
  theme_minimal()

# (3) post - corona
we_count %>% 
  filter(covid_type == "post_covid19") %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(label = word, size = n,family = "AppleGothic",color = word)) + 
  geom_text_wordcloud_area(eccentricity=.5)+
  scale_size_area(max_size = 15) +
  theme_minimal()


# 3. 토픽모델링

install.packages(c("cli","hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")

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

data_we = fread("/Users/fran/Desktop/Lab_Applied Statistics/request/DWSeo/preprocessed_data/working_environment.csv")


# (1) pre_covid

data_we_pre = data_we %>% mutate(
  article = str_replace_all(keyword,","," ")
) %>% filter(covid_type=="pre_covid19")
  


myRemove = content_transformer(function(x, pattern){return(gsub(pattern, "",x))})
toSpace = content_transformer(function(x, pattern){return(gsub(pattern," ",x))})

cps <- VCorpus(VectorSource(data_we_pre$article))
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
  openxlsx::write.xlsx("../result/we/pre_covid/붙임1.lda_beta_precovid.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(topic) %>% 
  ungroup() %>% group_by(topic) %>% 
  summarise(
    N = n()
  ) %>% 
  openxlsx::write.xlsx("../result/we/pre_covid/붙임2.토픽별대표문서_precovid.xlsx")

tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice(which.max(gamma)) %>% 
  openxlsx::write.xlsx("../result/we/pre_covid/붙임3.토픽별대표문서리스트_precovid.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(as.numeric(document)) %>% 
  openxlsx::write.xlsx("../result/we/pre_covid/붙임4.문서별토픽_precovid.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  summarise(mean_p = mean(gamma)) %>% 
  arrange(topic) %>% 
  openxlsx::write.xlsx("../result/we/pre_covid/붙임5.토픽출현율_평균_precovid.xlsx")

lda_term_top = lda_data_term %>%
  group_by(topic) %>% top_n(5,beta) %>%
  ungroup() %>% arrange(topic, -beta)

theme_set(theme_grey(base_family='NanumGothic'))


ggplot(lda_term_top, aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales="free")+
  labs(x=NULL, y="Word-Topic Probability (Beta)") + coord_flip()+
  theme(text = element_text(size = 7))


# (2) covid

data_we_covid = data_we %>% mutate(
  article = str_replace_all(keyword,","," ")
) %>% filter(covid_type=="covid19")


cps <- VCorpus(VectorSource(data_we_covid$article))
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
  openxlsx::write.xlsx("../result/we/covid19/붙임1.lda_beta_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(topic) %>% 
  ungroup() %>% group_by(topic) %>% 
  summarise(
    N = n()
  ) %>% 
  openxlsx::write.xlsx("../result/we/covid19/붙임2.토픽별대표문서_covid19.xlsx")

tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice(which.max(gamma)) %>% 
  openxlsx::write.xlsx("../result/we/covid19/붙임3.토픽별대표문서리스트_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(as.numeric(document)) %>% 
  openxlsx::write.xlsx("../result/we/covid19/붙임4.문서별토픽_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  summarise(mean_p = mean(gamma)) %>% 
  arrange(topic) %>% 
  openxlsx::write.xlsx("../result/we/covid19/붙임5.토픽출현율_평균_covid19.xlsx")

lda_term_top = lda_data_term %>%
  group_by(topic) %>% top_n(5,beta) %>%
  ungroup() %>% arrange(topic, -beta)

theme_set(theme_grey(base_family='NanumGothic'))


ggplot(lda_term_top, aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales="free")+
  labs(x=NULL, y="Word-Topic Probability (Beta)") + coord_flip()+
  theme(text = element_text(size = 7))


# (3) post_covid

data_we_post = data_we %>% mutate(
  article = str_replace_all(keyword,","," ")
) %>% filter(covid_type=="post_covid19")


cps <- VCorpus(VectorSource(data_we_post$article))
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

lda_data = LDA(dtm, k=3, method = "Gibbs",
               control = list(seed=123,burnin=1000,iter=1000,thin=100))


(lda_data_term = tidy(lda_data, matrix="beta"))
lda_data_term %>% arrange(desc(beta))


lda_data_term %>% arrange(topic,desc(beta)) %>% 
  openxlsx::write.xlsx("../result/we/post_covid/붙임1.lda_beta_post_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(topic) %>% 
  ungroup() %>% group_by(topic) %>% 
  summarise(
    N = n()
  ) %>% 
  openxlsx::write.xlsx("../result/we/post_covid/붙임2.토픽별대표문서_post_covid19.xlsx")

tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  slice(which.max(gamma)) %>% 
  openxlsx::write.xlsx("../result/we/post_covid/붙임3.토픽별대표문서리스트_post_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) %>% 
  arrange(as.numeric(document)) %>% 
  openxlsx::write.xlsx("../result/we/post_covid/붙임4.문서별토픽_post_covid19.xlsx")


tidy(lda_data,matrix = "gamma") %>% 
  group_by(topic) %>% 
  summarise(mean_p = mean(gamma)) %>% 
  arrange(topic) %>% 
  openxlsx::write.xlsx("../result/we/post_covid/붙임5.토픽출현율_평균_post_covid19.xlsx")

lda_term_top = lda_data_term %>%
  group_by(topic) %>% top_n(3,beta) %>%
  ungroup() %>% arrange(topic, -beta)

theme_set(theme_grey(base_family='NanumGothic'))


ggplot(lda_term_top, aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales="free")+
  labs(x=NULL, y="Word-Topic Probability (Beta)") + coord_flip()+
  theme(text = element_text(size = 7))

