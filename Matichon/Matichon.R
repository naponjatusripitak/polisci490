########## Set Up ########## 
# Set working directory
setwd("~/polisci490/Matichon")

# Load packages
packages <- c("xml2","rvest", "dplyr", "tm", "tidytext", "ggplot2", "stopwords", "SnowballC", "tidyverse", "lubridate", "stringr", "httr", "SnowballC", "wdman", "RSelenium", "tcltk", "XML", "topicmodels", "stringi", "LDAvis", "slam", "ldatuning", "kableExtra", "widyr", "igraph", "ggraph", "fmsb", "pander")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)

########## Data Collection ###########
### Matichon
rD <- rsDriver()
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4567L
                      , browserName = "chrome"
)

remDr$open()
n <- 380
#matichon <- data.frame(title=NULL, text=NULL, date=NULL, link=NULL)

remDr$setTimeout(type = "page load", milliseconds = 100000)
remDr$setImplicitWaitTimeout(milliseconds = 100000)

for(i in 2035:2249) {
  tryCatch({
    site <- paste0("https://www.matichon.co.th/category/politics/page/",i) # create URL for each page to scrape
    remDr$navigate(site) # navigates to webpage
    elem <- remDr$findElements(using="class", value="item-details")
    url_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/h3/a")
    
    url <- unlist(sapply(url_find, function(x) {x$getElementAttribute("href")}))
    title <- unlist(sapply(url_find, function(x) {x$getElementAttribute("title")}))
    meta_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/div[1]/span[2]/time")
    date <- unlist(sapply(meta_find, function(x) {x$getElementAttribute("datetime")}))
    
    article_body <- NULL
    for(j in 1:length(url)){
      article_html <- url[j] %>%
        read_html() 
      article_body[j] <- article_html %>%
        html_nodes(xpath="//div[@class='td-post-content']/p") %>%
        html_text() %>%
        paste(collapse = '\n\n')
    }
    
    matichon_page <- data.frame(title=title, text=article_body, date=date, link=url)
    matichon <- rbind(matichon, matichon_page)
    
  }, error=function(e){
    Sys.sleep(sample(1:10, 1))
    remDr$navigate(site) # navigates to webpage
    elem <- remDr$findElements(using="class", value="item-details")
    url_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/h3/a")
    
    url <- unlist(sapply(url_find, function(x) {x$getElementAttribute("href")}))
    title <- unlist(sapply(url_find, function(x) {x$getElementAttribute("title")}))
    meta_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/div[1]/span[2]/time")
    date <- unlist(sapply(meta_find, function(x) {x$getElementAttribute("datetime")}))
    
    article_body <- NULL
    for(j in 1:length(url)){
      article_html <- url[j] %>%
        read_html() 
      article_body[j] <- article_html %>%
        html_nodes(xpath="//div[@class='td-post-content']/p") %>%
        html_text() %>%
        paste(collapse = '\n\n')
    }
    
    matichon_page <- data.frame(title=title, text=article_body, date=date, link=url)
    matichon <- rbind(matichon, matichon_page)})
}
########## Pre-Processing ###########
#devtools::install_github("pichaio/thainltk")
library(thainltk)
tt <- thaiTokenizer(skipSpace = T)


matichon$text <- as.character(matichon$text)
matichon$title <- as.character(matichon$title)
matichon$link <- as.character(matichon$link)
matichon$date <- as_date(matichon$date)
matichon <- matichon %>% filter(str_length(text) > 100)

tokenize <- function(x){
  texts <- lapply(x, tt)
  tokens <- sapply(texts, function(k){
    paste(k, collapse=" ")
  })
  result <- gsub("”|“", "", tokens)
  return(result)
}


matichon$text <- tokenize(matichon$text)

# Remove duplicate rows
matichon <- distinct(matichon)

# Add doc_id
matichon <- matichon %>% mutate(doc_id = seq.int(nrow(matichon)))

# Finalize dataset
matichon <- matichon %>% select(doc_id, text, everything())

# 
matichon.corpus <- VCorpus(DataframeSource(matichon))

#save(matichon.dtm, file="matichondtm.Rda")

# Pre-process + generate document-term matrix
th.stop <- c(stopwords("th", source = "stopwords-iso"), c("ที่", "ให้", "ได้", "นี้", "ใช้", "ไม่", "แต่", "รู้", "ฝ่าย", "อยู่", "ผู้", "ไว้","อัน", "ใส่", "ตัว") )
th.stop <- unique(th.stop)
pattern <- paste(th.stop, collapse="|")

matichon.dtm <- matichon.corpus %>% 
  tm_map(content_transformer(function(x) gsub(x, pattern = pattern, replacement = ""))) %>%
  tm_map(removeWords, character(0)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace) %>%
  DocumentTermMatrix() %>%
  removeSparseTerms(sparse = 0.99)



     
### Top Frequency

termFreq <- colSums(as.matrix(matichon.dtm))
head(termFreq)

tf <- data.frame(term = names(termFreq), freq = termFreq)
tf <- tf[order(-tf[,2]),]
head(tf)

###

#save(matichon, file="matichonclean.Rda")

rowTotals <- apply(matichon.dtm, 1, sum) 

#Remove all docs without words
matichon.dtm   <- matichon.dtm[rowTotals> 0, ] 

#LDA
matichon.60 <- LDA(matichon.dtm, k=60, control = list(seed=6))
save(matichon.60, file="matichonlda.Rda")

class(matichon.60) <- "LDA"
tidy(matichon.60)

### Generate topic labels (top 30 terms)
matichon.topics <- topics(matichon.60, 1)
mathichon.terms <- as.data.frame(terms(matichon.60, 30), stringsAsFactors = FALSE)

topic_list <- data.frame(t(mathichon.terms))


####
topic_list <- within(topic_list, x <- paste(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, sep=", ")) %>% select(x)
colnames(topic_list) <- NULL
topic_list <- tibble::rownames_to_column(topic_list)
colnames(topic_list) <- c("Topic", "Keys")
knitr::kable(topic_list, format = "latex", booktabs = T)


### Plot Topics and Top 20 Words
top.terms <- tidy(matichon.60) %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top.terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + ggtitle("LDA")

### LDAvis https://github.com/cpsievert/LDAvis & https://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  ## Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  ## Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- as.matrix(doc_term[1:nrow(doc_term), 1:ncol(doc_term)])
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  ## Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}


matichon.jason <- topicmodels_json_ldavis(matichon.60, matichon.corpus, matichon.dtm)
serVis(matichon.jason, out.dir = "~/polisci490/Matichon")

### Topic Frequencies Over Time
matichon.plot <- data.frame(id=names(topics(matichon.60)),                 
                          date=matichon$date)

dft <- cbind(matichon.plot,posterior(matichon.60)$topics)

M <- gather(dft,topic,value,-id,-date) %>%
  group_by(topic,month = floor_date(date, "month")) %>%
  summarize(value=mean(value))

ggplot(M,aes(x=month,y=value, col=topic)) + 
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks= "1 month", date_labels = "%B-%Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_vline(xintercept = as.numeric(as.Date("2014-05-22")), linetype=4)

###done

### Selected Topic Frequencies Over Time
dft.select <- dft %>% select(id, date, "2","24", "33", "38", "46", "51")

M <- gather(dft.select,topic,value,-id,-date) %>%
  group_by(topic,month = floor_date(date, "month")) %>%
  summarize(value=mean(value))

ggplot(M,aes(x=month,y=value, col=factor(topic, labels=c("Agriculture", "Law", "Political Reform", "Junta", "Government", "Political Conflict")))) + 
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks= "1 month", date_labels = "%B-%Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_vline(xintercept = as.numeric(as.Date("2014-05-22")), linetype=4) +
  #ggtitle("Topic Prevalence") +
  labs(color = "Topic")


########## Content Analysis ########## 
### Attach most associated topic to each document
nation.topics.df <- as.data.frame(nation.topics)
nation.topics.df <- transmute(nation.topics.df, doc_id = rownames(nation.topics.df), topic = nation.topics)
nation.topics.df$doc_id <- as.integer(nation.topics.df$doc_id)
df.out <- inner_join(df.2013, nation.topics.df, by = "doc_id")

### Tidy
tidy_df <- df.out %>%
  group_by(topic) %>%
  ungroup() %>%
  unnest_tokens(word, text)

### Word frequencies for selected topics
tidy_df %>% filter(topic == "51") %>% 
  group_by(date) %>%
  summarize(value=n()) %>%
  ggplot(aes(x=date,y=value)) + 
  geom_bar(stat = "identity", fill="skyblue") +
  scale_x_date(date_breaks= "3 month", date_labels = "%B %d, %Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_vline(xintercept = as.numeric(as.Date("2014-05-22")), linetype=4) +
  ggtitle("The Number of Words Written on the ‘Political Conflict’ Topic Per Day")

tidy_df %>% filter(topic == "38") %>% 
  group_by(date) %>%
  summarize(value=n()) %>%
  ggplot(aes(x=date,y=value)) + 
  geom_bar(stat = "identity", fill="skyblue") +
  scale_x_date(date_breaks= "3 month", date_labels = "%B %d, %Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_vline(xintercept = as.numeric(as.Date("2014-05-22")), linetype=4) +
  ggtitle("The Number of Words Written on the ‘Junta’ Topic Per Day")

### Sentiment analysis for selected topics
topic.sentiment1  <- tidy_df %>% filter(topic == c("2","24", "33", "38", "46", "51")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(topic, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

topic.sentiment1 %>%
  ggplot(aes(factor(topic, labels=c("Agriculture", "Law", "Political Reform", "Junta", "Government", "Political Conflict")), sentiment)) +
  geom_bar(stat="identity", fill="skyblue3") +
  ylim(-700, 700) +
  xlab("Topic") +
  ylab("Sentiments")

### Sentiment analysis for selected topics over time
topic.sentiment  <- tidy_df %>% filter(topic == c("2","24", "33", "38", "46", "51")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(topic, date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(topic.sentiment, aes(date, sentiment, fill = topic)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~factor(topic, labels=c("Agriculture", "Law", "Political Reform", "Junta", "Government", "Political Conflict")), ncol = 2, scales = "free_x") +
  ylab("Sentiments") +
  xlab("Date") +
  ggtitle("Sentiment Analysis")

########## Prayut vs Yinngluck ########## 
### Top Topics associated with Yingluck and Prayut
tidy(mod.out.60) %>%
  filter(term == "yingluck") %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  ggplot(aes(x=reorder(topic, -beta), y=beta)) +
  geom_bar(stat="identity", fill="tomato2") +
  coord_flip()

tidy(mod.out.60) %>%
  filter(term == "prayut"|term == "prayuth") %>%
  top_n(20, beta) %>%
  arrange(-beta) %>%
  ggplot(aes(x=reorder(topic, -beta), y=beta)) +
  geom_bar(stat="identity", fill="darkgreen")

### Table
topic_list <- data.frame(t(nation.terms))
topic_list <- within(topic_list, x <- paste(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, sep=", ")) %>% select(x)
colnames(topic_list) <- NULL
topic_list <- tibble::rownames_to_column(topic_list)
colnames(topic_list) <- c("Topic", "Keys")

yingluck.pattern <- tidy(mod.out.60) %>%
  filter(term == "yingluck") %>%
  top_n(5, beta) %>%
  arrange(-beta) %>%
  select(topic)
knitr::kable(topic_list[yingluck.pattern$topic, ], format = "latex", booktabs = T)


prayut.pattern <- tidy(mod.out.60) %>%
  filter(term == "prayut") %>%
  top_n(5, beta) %>%
  arrange(-beta) %>%
  select(topic)
knitr::kable(topic_list[prayut.pattern$topic, ], format = "latex", booktabs = T)

### Sentiment Analysis
# Break doc into sections
section_words <- tidy_df %>% 
  group_by(doc_id) %>%
  mutate(section = row_number() %/% 30) %>%
  filter(section > 0) %>%
  filter(!word %in% stop_words$word)

# Yingluck
yingluck_sections <- section_words %>%
  group_by(doc_id, section) %>%
  filter(word=="yingluck") %>%
  select(doc_id, section) %>%
  inner_join(section_words)

yingluck.sentiments  <- yingluck_sections %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  group_by(week = floor_date(date, "week")) %>%
  summarize(value=mean(sentiment))

# Prayut
prayut_sections <- section_words %>%
  group_by(doc_id, section) %>%
  filter(word == "prayut"|word == "prayuth") %>%
  select(doc_id, section) %>%
  inner_join(section_words)

prayut.sentiments  <- prayut_sections %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  group_by(week = floor_date(date, "week")) %>%
  summarize(value=mean(sentiment))

# Graph
yingluck.sentiments$label <- "Yingluck"
prayut.sentiments$label <- "Prayut"
yingluck.vs.prayut <- full_join(yingluck.sentiments, prayut.sentiments)

ggplot(yingluck.vs.prayut, aes(week, value, col=label)) +
  geom_line() +
  theme_classic() +
  scale_x_date(date_breaks= "3 month", date_labels = "%b %Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_vline(xintercept = as.numeric(as.Date("2014-05-22")), linetype=4) +
  geom_hline(yintercept = 0, size=0.1) +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  xlab("Date") +
  ylab("Sentiment") +
  scale_color_manual(values=c("#9999CC", "#CC6666"))

