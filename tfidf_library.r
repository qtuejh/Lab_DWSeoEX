#################
## Source code ## 
#################
read_bigkinds <- function(file, filter_text){
  require(dplyr)
  require(stringr)
  require(readxl)
  require(lubridate)
  
  if(missing(filter_text))
  {
    warning("Missing filter_text. There are no filtering in keyword.", call. = FALSE)
    
    result = readxl::read_xlsx(file) %>% 
      data.table() %>% 
      filter(is.na(`분석제외 여부`)) %>% 
      filter(is.na(`사건/사고 분류1`)) %>% 
      filter(str_detect(`통합 분류1`, "경제일반|노동_복지|산업_기업")) %>% 
      select(일자, 언론사, `통합 분류1`, 키워드) %>% 
      rename(date = 일자,
             media = 언론사,
             category = `통합 분류1`,
             keyword = 키워드) %>% 
      mutate(date = ymd(date)) %>% 
      mutate(covid_type = case_when(
        ymd("2018-01-01") <= date & 
          date < ymd("2020-03-01") ~ "pre_covid19",
        ymd("2020-03-01") <= date &
          date < ymd("2023-01-01") ~ "covid19",
        TRUE ~ "post_covid19")) 
  }
  else
  {
    filter_text = filter_text %>% 
      str_replace_all(pattern = "\n", replacement = "") %>%
      str_replace_all(pattern = " ", replacement = "") %>% 
      str_split(pattern = ",", simplify = T) %>% 
      as.vector() %>% 
      unique()
    
    filter_text = unique(c(filter_text, toupper(filter_text), tolower(filter_text)))
    filter_text = paste0(filter_text, collapse = "|")
    
    result = readxl::read_xlsx(file) %>% 
      data.table() %>% 
      filter(is.na(`분석제외 여부`)) %>% 
      filter(is.na(`사건/사고 분류1`)) %>% 
      filter(str_detect(`통합 분류1`, "경제일반|노동_복지|산업_기업")) %>% 
      select(일자, 언론사, `통합 분류1`, 키워드) %>% 
      rename(date = 일자,
             media = 언론사,
             category = `통합 분류1`,
             keyword = 키워드) %>% 
      mutate(date = ymd(date)) %>% 
      mutate(covid_type = case_when(
        ymd("2018-01-01") <= date & 
          date < ymd("2020-03-01") ~ "pre_covid19",
        ymd("2020-03-01") <= date &
          date < ymd("2023-01-01") ~ "covid19",
        TRUE ~ "post_covid19")) %>% 
      filter(!str_detect(keyword, filter_text))
  }
  return(result)
}

make_text_clean <- function(X, removed_text){
  require(dplyr)
  require(stringr)
  if(missing(removed_text))
  {
    warning("Missing removed_text. All keyword are printed.", call. = FALSE)
  }
  else
  {
    removed_text = removed_text %>% 
      str_replace_all(pattern = "\n", replacement = "") %>% 
      str_replace_all(pattern = " ", replacement = "") %>% 
      str_split(pattern = ",", simplify = T) %>% 
      as.vector() %>% 
      unique()
    
    removed_col = unique(c(removed_text, toupper(removed_text), tolower(removed_text)))
  }
  X = str_replace_all(X, "인공,지능", "인공지능")
  
  X = str_replace_all(X, "화상,회의", "화상회의")
  X = str_replace_all(X, "온라인,회의", "온라인회의")
  X = str_replace_all(X, "대면,회의", "대면회의")
  
  X = str_replace_all(X, "소통,강화", "소통강화")
  X = str_replace_all(X, "소통,원할", "소통원할")
  
  X = str_replace_all(X, "소통,부족", "소통부족")
  X = str_replace_all(X, "소통,부재", "소통부재")
  X = str_replace_all(X, "소통,단절", "소통단절")
  X = str_replace_all(X, "소통,어려움", "소통어려움")
  
  
  X = str_replace_all(X, "근무,태만", "근무태만")
  X = str_replace_all(X, "근태,관리", "근태관리")
  X = str_replace_all(X, "근태,불량", "근태불량")
  X = str_replace_all(X, "근태,감시", "근태감시")
  
  X = str_replace_all(X, "근로,시간", "근로시간")
  X = str_replace_all(X, "근무,시간", "근무시간")
  X = str_replace_all(X, "노동,시간", "노동시간")
  X = str_replace_all(X, "업무,시간", "업무시간")
  X = str_replace_all(X, "휴게,시간", "휴게시간")
  X = str_replace_all(X, "출퇴근,시간", "출퇴근 시간")
  X = str_replace_all(X, "출근,시간", "출근 시간")
  X = str_replace_all(X, "퇴근,시간", "퇴근 시간")
  
  X = str_replace_all(X, "휴가,제도", "휴가제도")
  X = str_replace_all(X, "후생,제도", "후생제도")
  X = str_replace_all(X, "복지,제도", "복지제도")
  X = str_replace_all(X, "휴직직,제도", "휴직제도")
  X = str_replace_all(X, "근무,제도", "근무제도")
  X = str_replace_all(X, "근로,제도", "근로제도")
  X = str_replace_all(X, "출퇴근,제도", "출퇴근제도")
  
  X = str_replace_all(X, "온라인,강의", "온라인강의")
  X = str_replace_all(X, "온라인,수업", "온라인수업")
  X = str_replace_all(X, "온라인,구매", "온라인구매")
  X = str_replace_all(X, "온라인,업무", "온라인업무")
  X = str_replace_all(X, "온라인,교육", "온라인교육")
  X = str_replace_all(X, "온라인,쇼핑", "온라인쇼핑")
  
  X = str_replace_all(X, "최저,임금", "최저임금")
  X = str_replace_all(X, "육아,휴직", "육아휴직")
  
  
  X = str_replace_all(X, "하이브리드,근무", "하이브리드근무")
  
  X = str_replace_all(X, "탄력,근무", "탄력근무")
  X = str_replace_all(X, "단축,근무", "단축근무")
  X = str_replace_all(X, "재택,근무", "재택근무")
  X = str_replace_all(X, "원격,근무", "원격근무")
  X = str_replace_all(X, "사무실,근무", "사무실근무")
  X = str_replace_all(X, "정상,근무", "정상근무")
  X = str_replace_all(X, "사업장,근무", "사업장근무")
  X = str_replace_all(X, "오피스,근무", "오피스근무")
  
  X = str_replace_all(X, "탄력,근무", "탄력근무")
  X = str_replace_all(X, "연장,근무", "연장근무")
  X = str_replace_all(X, "초과,근무", "초과근무")
  X = str_replace_all(X, "장시간,근무", "장시간근무")
  
  X = str_replace_all(X, "근무,제도", "근무제도")
  
  X = str_replace_all(X, "정시,퇴근", "정시퇴근")
  X = str_replace_all(X, "사무실,출근", "사무실 출근")
  X = str_replace_all(X, "정상,출근", "정상 출근")
  X = str_replace_all(X, "직장,출근", "직장 출근")
  X = str_replace_all(X, "오피스,출근", "오피스 출근")
  X = str_replace_all(X, "사무실,복귀", "사무실 복귀")
  X = str_replace_all(X, "사무실,출퇴근", "사무실 출퇴근")
  
  X = str_replace_all(X, "조기,퇴근", "조기퇴근")
  
  X = str_replace_all(X, "mz,세대", "MZ세대")
  X = str_replace_all(X, "MZ,세대", "MZ세대")
  
  X = str_replace_all(X, "이직,준비", "이직준비")
  X = str_replace_all(X, "이직,시도", "이직시도")
  X = str_replace_all(X, "이직,결심", "이직결심")
  X = str_replace_all(X, "이직,의사", "이직의사")
  
  X = str_replace_all(X, "이직,사유", "이직사유")
  X = str_replace_all(X, "이직,제안", "이직제안")
  
  X = str_replace_all(X, "이직,경력", "이직경력")
  X = str_replace_all(X, "이직,경험", "이직경험")
  
  X = str_replace_all(X, "퇴사,경험", "퇴사경험")
  X = str_replace_all(X, "퇴사,사유", "퇴사사유")
  X = str_replace_all(X, "퇴사,결심", "퇴사결심")
  
  X = str_replace_all(X, "스타트업,이직", "스타트업 이직")
  X = str_replace_all(X, "경쟁사,이직", "경쟁사 이직")
  
  X = str_replace_all(X, "임금,감소", "임금감소")
  X = str_replace_all(X, "임금,상승", "임금상승")
  X = str_replace_all(X, "임금,격차", "임금격차")
  
  X = str_replace_all(X, "스톡,옵션", "스톡옵션")
  
  split_X  = unlist(strsplit(X, ","))
  split_X = case_when(
    split_X %in% c("covid19", "COVID-", "COVID", "COVID19",
                   "코로나19", "코로나", "코로나바이러스",
                   "우한코로나", "신종코로나바이러스", "감염증") ~ "코로나19",
    split_X %in% c("Leadership", "leadership", "리더쉽") ~ "리더십",
    split_X %in% c("근무", "근로", "업무") ~ "근무",
    split_X %in% c("근무제도도", "근로제도") ~ "근무제도",
    split_X %in% c("인공지능", "ai") ~ "ai",
    split_X %in% c("근로시간", "근무시간", "노동시간", "업무시간") ~ "근로시간",
    split_X %in% c("사무실 출근", "사무실 복귀", "사무실 출퇴근",
                   "오피스 출근", "정상 출근", "직장 출근") ~ "사무실 출근",
    split_X %in% c("소통부재", "소통어려움", "소통단절", "소통부족") ~ "소통부족",
    split_X %in% c("근무태만", "근태관리", "근태불량", "근태감시") ~ "근무태만",
    split_X %in% c("연장근무", "초과근무", "장시간근무") ~ "초과근무",
    split_X %in% c("사무실근무", "정상근무", "오피스근무", "사업장근무") ~ "사무실근무",
    split_X %in% c("사무실", "오피스") ~ "사무실",
    split_X %in% c("이직준비", "이직시도", "이직결심", "이직의사") ~ "이직시도",
    split_X %in% c("이직경력", "이직경험") ~ "이직경험",
    split_X %in% c("퇴사", "퇴직") ~ "퇴사",
    TRUE ~ split_X)
  
  ## 단순 공백 제거 
  split_X = split_X[!(split_X == "" &
                        str_detect(split_X, "ㅤ"))]
  
  ## 숫자 제거
  split_X = split_X[!(str_detect(split_X, "[0-9]+") & 
                        split_X != "코로나19")]
  ## 특수 기호 1차 제거
  split_X = split_X[!str_detect(split_X, "[^[:alnum:][:space:]]")]
  ## 특수 기호 2차 제거
  split_X = split_X[!str_detect(split_X, "[ᆞㆍㅣーᅳᆫ]")]
  
  ## 한글 자음, 모음으로만 되어 있는 값 제거
  split_X = split_X[!str_detect(split_X, "[ㄱ-ㅎ]+") &
                      !str_detect(split_X, "[ㅏ-ㅢ]+")]
  
  ## 중국어, 한자, 일본어어 제거
  split_X = split_X[!str_detect(split_X, "[\u4e00-\u9fff]") &
                      !str_detect(split_X, "[\u3040-\u30FF\uFF00-\uFFEF\u4E00-\u9FAF\u3400-\u4DBF]")]
  
  ## 1글자 제거
  split_X = split_X[nchar(split_X) != 1]
  
  ## 특정 문자열 제거
  split_X = split_X[! split_X %in% removed_col]
  
  result = toupper(paste(split_X, collapse = ","))
  
  return(result)
}

count_merged_text <- function(df, keyword_col, text, foreword = T){
  check_merged_text = function(X, text, foreword = TRUE){
    if(foreword)
    {
      split_X = unlist(strsplit(X, split = ","))
      result = paste0(split_X[which(split_X == text)-1], collapse = ",")
    }else
    {
      split_X = unlist(strsplit(X, split = ","))
      result = paste0(split_X[which(split_X == text)+1], collapse = ",")
    }
    if(result == "") result = NA
    return(result)
  }
  keyword_col = sym(keyword_col)
  
  test = df %>% 
    mutate(check = sapply(!! keyword_col,
                          check_merged_text,
                          text = text,
                          foreword = foreword,
                          simplify = T, USE.NAMES = F)) %>% 
    filter(!is.na(check)) 
  
  keyword = sapply(test$check,
                   function(X) unlist(strsplit(X, split = ",")),
                   simplify = T, USE.NAMES = F) %>% 
    unlist()
  
  data.frame(keyword) %>% 
    group_by(keyword) %>% 
    summarize(N = n()) %>% 
    ungroup() %>% 
    arrange(desc(N)) %>% 
    data.table() 
}

## Make keyword-count table for each covid19 type
# df : data.frame
# keyword_col : character value for keyword column in df
# group_col : character value for covid19 type in df 
make_keyword_count <- function(df, keyword_col, group_col){
  require(dplyr)
  require(stringr)
  group_column = pull(unique(select(df, !!sym(group_col))))
  result = vector(mode = "list", length = length(group_column))
  names(result) = group_column
  
  for(i in 1:length(group_column))
  {
    temp_df = df %>% 
      filter(!!sym(group_col) == group_column[i])
    
    temp_count <- data.table(
      keyword = unlist(strsplit(pull(select(temp_df, !!sym(keyword_col))), 
                                ","))) %>% 
      group_by(!!sym(keyword_col)) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      arrange(desc(count)) %>% 
      mutate(type = group_column[i]) %>% 
      data.table()
    
    result[[i]] = temp_count
  }
  return(result)
} # 얘는 굳이 안써도 됨


## Make TF table
make_tf_table <- function(df, text_col, group_col, top_n){
  require(dplyr)
  require(tidytext)
  text_col = sym(text_col)
  group_col = sym(group_col)
  
  df %>% 
    unnest_tokens(word, !! text_col, token = 'regex', pattern = ',') %>% 
    count(!! group_col, word) %>% 
    group_by(!! group_col) %>% 
    top_n(n = top_n, wt = n) %>% 
    ungroup() %>% 
    data.table()
}

## Make TF-IDF plot 
make_tf_idf_table <- function(df, text_col, group_col, top_n.1 = 100, top_n.2 = 50){
  require(tm)
  require(dplyr)
  require(tidytext)
  
  text_col = sym(text_col)
  group_col = sym(group_col)
  
  df %>% 
    unnest_tokens(word, !! text_col, token = 'regex', pattern = ",") %>% 
    add_count(!! group_col, name = "total_words") %>%
    group_by(!! group_col, total_words) %>% 
    count(word, sort = TRUE) %>% 
    ungroup() %>% 
    group_by(!! group_col) %>% 
    top_n(n = top_n.1, wt = n) %>% 
    ungroup() %>% 
    data.table() %>% 
    select(-total_words) %>%
    bind_tf_idf(term = word, document = !! group_col, n = n) %>% 
    group_by(!! group_col) %>% 
    top_n(n = top_n.2, wt = tf_idf) %>% 
    ungroup() %>% 
    data.table()
}

## Make TF plot
geom_tf_plot <- function(df, x, y, facet_group, title = "", 
                         need_factor_arrange = F, factor_order){
  require(ggplot2)
  require(dplyr)
  if(need_factor_arrange){
    if(!missing(factor_order))
    {
      df[[facet_group]] = factor(df[[facet_group]],
                                 levels = factor_order)
    }
    else
    {
      warning(paste0("No factor order for arrange. ", 
                     facet_group,"'s factor levels will be not arranged."),
              call. = FALSE)
    }
  }
  x = sym(x)
  y = sym(y)
  facet_group = sym(facet_group)
  
  df %>% 
    ggplot(aes(x = reorder_within(!!x, by = !!y, within = !!facet_group), 
               y = !!y,
               fill = !!facet_group)) +
    geom_col() +
    facet_wrap(~covid_type, scales = "free") + 
    labs(x = "", y = "", title = title) + 
    coord_flip() +
    scale_x_reordered() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      strip.background = element_rect(fill = "white", color = "black"),
      text = element_text(face = "bold"),
      title = element_text(size = 25),
      legend.position = "none")  
}