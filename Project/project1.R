########## Set Up ########## 
# Set working directory
setwd("~/polisci490/Project")

# Load packages
packages <- c("xml2","rvest", "dplyr", "tm", "tidytext", "ggplot2", "SnowballC", "tidyverse", "lubridate", "stringr", "httr", "SnowballC", "wdman", "RSelenium", "tcltk", "XML", "topicmodels", "stringi", "LDAvis", "slam", "ldatuning", "kableExtra", "widyr", "igraph", "ggraph", "fmsb", "pander")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)

########## Data Collection (Don't Run this!!)########## 

# Run Selenium Server
rD <- rsDriver()
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4567L
                      , browserName = "chrome"
)


# Log in
remDr$open()
remDr$navigate("https://advance-lexis-com.turing.library.northwestern.edu/api/permalink/920dc8d1-6625-47c3-bdd4-b1c073f28f78/?context=1516831")
username <- remDr$findElement(using = 'id', value = "IDToken1")
password <- remDr$findElement(using = 'id', value = "IDToken2")
username$sendKeysToElement(list("id"))
password$sendKeysToElement(list("pass", "\uE007"))

# click

continue <- remDr$findElement(using = "xpath", "//*[@id='33Lk']/div/div/div/section/div/menu/input[1]")
continue$clickElement()

# Initializing an empy dataframe
# df <- data.frame(title=NULL, text=NULL, date=NULL, length=NULL)

# Scraping

remDr$setTimeout(type = "page load", milliseconds = 100000)
remDr$setImplicitWaitTimeout(milliseconds = 100000)
for(i in 1:20000){
  tryCatch({
    title <- remDr$findElement(using = "id", "SS_DocumentTitle")
    date <- remDr$findElement(using = "xpath", "//*[@id='document']/section/header/p[3]")
    length <- remDr$findElement(using = "xpath", "//*[@id='document']/section/span/div[2]")
    body <- remDr$findElement(using = "xpath", "//*[@id='document']/section/span")
    
    preptext <- function(x){ data.frame(matrix(unlist(x), nrow=length(x), byrow=T))}
    
    title_txt <- preptext(title$getElementText())
    date_txt <- preptext(date$getElementText())
    length_txt <- preptext(length$getElementText())
    body_txt <- preptext(body$getElementText())
    
    docdf <- data.frame(title=title_txt, text=body_txt, date=date_txt, length=length_txt)
    df <- rbind(df, docdf)
    
    Sys.sleep(sample(1:4, 1))
    next_page <- remDr$findElement(using="xpath", "//*[@id='_gLdk']/div/form/div[1]/div/div[14]/nav/span/button[2]")
    next_page$highlightElement()
    next_page$clickElement()
    
  }, error=function(e){
    Sys.sleep(sample(1:10, 1))
    title <- remDr$findElement(using = "id", "SS_DocumentTitle")
    date <- remDr$findElement(using = "xpath", "//*[@id='document']/section/header/p[3]")
    length <- remDr$findElement(using = "xpath", "//*[@id='document']/section/span/div[2]")
    body <- remDr$findElement(using = "xpath", "//*[@id='document']/section/span")
    
    preptext <- function(x){ data.frame(matrix(unlist(x), nrow=length(x), byrow=T))}
    
    title_txt <- preptext(title$getElementText())
    date_txt <- preptext(date$getElementText())
    length_txt <- preptext(length$getElementText())
    body_txt <- preptext(body$getElementText())
    
    docdf <- data.frame(title=title_txt, text=body_txt, date=date_txt, length=length_txt)
    df <- rbind(df, docdf)
    
    Sys.sleep(sample(1:4, 1))
    next_page <- remDr$findElement(using="xpath", "//*[@id='_gLdk']/div/form/div[1]/div/div[14]/nav/span/button[2]")
    next_page$highlightElement()
    next_page$clickElement()
  })}

#save(df, file="nation.Rda")

########## Pre-Processing ########## 
### Cleaning
# Import data
load(file = "nation.Rda")


# Assign variable names
names(df) <- c("title", "text", "date", "wordcount")

# Set date
df$date <- mdy(as.character(df$date))

# Clean content
df$text <- gsub("^.*Body\n|Classification.*$","", as.character(df$text))
df$text <- gsub( " *@.*?; *", "", as.character(df$text))
df$text <- gsub( "*â€", "", as.character(df$text))
df$text <- gsub( "*â€“", "", as.character(df$text))
df$title <- gsub( " *@.*?; *", "", as.character(df$title))
df$title <- gsub( "*â€", "", as.character(df$title))
df$title <- gsub( "*â€“", "", as.character(df$title))
df$title <- gsub( "*#124", "", as.character(df$title))
df$text <- gsub( "*#124", "", as.character(df$text))
df$text <- gsub("*Prayuth", "Prayut", as.character(df$text)) # Prayut & Prayuth
df$title <- gsub("*Prayuth", "Prayut", as.character(df$title)) # Prayut & Prayuth

# Get word count
df$wordcount <- as.numeric(gsub("Length: | words", "", df$wordcount))

# Filter for politics
keyword <- c("politic*", "gov*", "democ*", "Yingluck", "junta", "Prayut", "Pheu", "NCPO")
pattern <- grepl(paste(keyword, collapse = "|"), df$text)
df <- df[pattern, ]

# select date Jan 2013 - Sep 2015
df.2013 <- filter(df, date >= "2013-01-01" & date < "2016-01-01")

# Remove duplicate rows
df.2013 <- distinct(df.2013)

# Add doc_id
df.2013 <- df.2013 %>% mutate(doc_id = seq.int(nrow(df.2013)))

# Finalize dataset
df.2013 <- df.2013 %>% select(doc_id, text, everything())

# Example
example <- df.2013[9135:9144, ]
example$title <- substr(example$title, 1, 20)
example$text <- substr(example$text, 1, 50)
rownames(example) <- NULL

knitr::kable(example, format = "latex", booktabs = T) %>%
  kable_styling(latex_options = "scale_down")

### Corpus -> Document-Term Matrix

# Create corpus
nation <- VCorpus(DataframeSource(df.2013))

# Pre-process + generate document-term matrix
nation.dtm <- nation %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace) %>%
  DocumentTermMatrix() %>%
  removeSparseTerms(sparse = 0.99)

########## Topic Modeling ########## 
##### LDA Tuning
result <- FindTopicsNumber(
  nation.dtm,
  topics = seq(from = 20, to = 200, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

#save(result, file="tuning.Rda")
FindTopicsNumber_plot(result)

##### LDA (2013-2015) k=60
mod.out.60 <- LDA(nation.dtm, k=60, control = list(seed=6))

mod.out.60 <- LDA(nation.dtm, k=60, control = list(seed=6))

#save(mod.out.60, file="reallda.60.Rda")

class(mod.out.60) <- "LDA"
tidy(mod.out.60)

### Generate topic labels (top 30 terms)
nation.topics <- topics(mod.out.60, 1)
nation.terms <- as.data.frame(terms(mod.out.60, 30), stringsAsFactors = FALSE)

### Table
topic_list <- data.frame(t(nation.terms))
topic_list <- within(topic_list, x <- paste(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, sep=", ")) %>% select(x)
colnames(topic_list) <- NULL
topic_list <- tibble::rownames_to_column(topic_list)
colnames(topic_list) <- c("Topic", "Keys")
knitr::kable(topic_list, format = "latex", booktabs = T)

### Example Table
selected.topics <- nation.terms %>% select("Topic 2", "Topic 24", "Topic 33", "Topic 38", "Topic 46", "Topic 51")
colnames(selected.topics) <- c("Agriculture", "Law", "Political Reform", "Junta", "Government", "Political Conflict")
selected.topics[1:20,]
knitr::kable(selected.topics[1:15, ], format = "latex", booktabs = T, align = "c") %>% kable_styling(position = "float_right")

### Plot Topics and Top 20 Words
top.terms <- tidy(mod.out.60) %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top.terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + ggtitle("LDA (2013-2015)")

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


nation.jason <- topicmodels_json_ldavis(mod.out.60, nation, nation.dtm)
serVis(nation.jason, out.dir = "~/polisci490/Project")

### Topic Frequencies Over Time
nation.plot <- data.frame(id=names(topics(mod.out.60)),                 
                          date=df.2013$date)

dft <- cbind(nation.plot,posterior(mod.out.60)$topics)

M <- gather(dft,topic,value,-id,-date) %>%
  group_by(topic,month = floor_date(date, "month")) %>%
  summarize(value=mean(value))

ggplot(M,aes(x=month,y=value, col=topic)) + 
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks= "1 month", date_labels = "%B-%Y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  geom_vline(xintercept = as.numeric(as.Date("2014-05-22")), linetype=4)

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
