# load libraries ----
suppressPackageStartupMessages({
library(extrafont)
library(ggraph)
library(ggridges)
library(pdftools)
library(tidyverse)
library(tidytext)
library(forcats)
library(reshape2)
library(tidyr)
library(igraph)
library(widyr)
library(viridis)}

)
fed_import1 <- pdf_text("https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf")
str(fed_import1)
substr(fed_import1[7],1,500)
fed_text_raw <- 
  data.frame(text=unlist(strsplit(fed_import1,"\r"))) %>% 
  mutate(report="July2018", 
         line=row_number(),
         text=gsub("\n","",text))
head(fed_text_raw)

fed_text <- 
  fed_text_raw %>% 
  as_tibble() %>%
  unnest_tokens(word,text)
fed_text

fed_text  %>%
  count(word, sort = TRUE) 
  
  fed_text  %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) 
  
  fed_text2 <- 
  fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%
  filter(word != "")

fed_text2  %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) 
  
  fed_text2 %>%
  inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE)
    
    fed_bigrams <-   
  fed_text_raw %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  as_tibble()

fed_bigrams

fed_bigrams %>%
  count(bigram, sort = TRUE)
  
 bigrams_separated <- fed_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# unite them

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 

bigrams_united

bigrams_filtered %>%
  filter(word1 == "gross") %>%
  count( word2, sort = TRUE)

custom_stop_words2 <- 
  bind_rows(data_frame(word = c("debt",
                                "gross",
                                "crude",
                                "well",
                                "maturity",
                                "work",
                                "marginally",
                                "leverage"), 
                       lexicon = c("custom")), 
            stop_words)


fed_sentiment <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
  
 ggplot(fed_sentiment,  aes(index, sentiment, fill = sentiment>0)) +
  geom_col(show.legend = FALSE) +
    scale_fill_manual(values=c("red","#27408b"))+
  facet_wrap(~report, ncol = 5, scales = "free_x")+
  theme_ridges(font_family="Roboto")+
  labs(x="index (approximately 3 pages per unit)",y="sentiment",
       title="Sentiment through Federal Reserve Monetary Policy Report",
       subtitle="customized bing lexicon",
       caption="@lenkiefer\nSource: https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf")
  
  # list of reports, comments indicate important events around release of report
fed.links=c("https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf",  
            "https://www.federalreserve.gov/monetarypolicy/files/20170707_mprfullreport.pdf",
            "https://www.federalreserve.gov/monetarypolicy/files/20160621_mprfullreport.pdf",            # released in jun 2016, but we'll label it July
            "https://www.federalreserve.gov/monetarypolicy/files/20150715_mprfullreport.pdf",            # July 2015  ( before lift off)
            "https://www.federalreserve.gov/monetarypolicy/files/20140715_mprfullreport.pdf",
            "https://www.federalreserve.gov/monetarypolicy/files/20130717_mprfullreport.pdf",            # July 2013  ( after Taper Tantrum)
            "https://www.federalreserve.gov/monetarypolicy/files/20120717_mprfullreport.pdf",
            "https://www.federalreserve.gov/monetarypolicy/files/20110713_mprfullreport.pdf",            # July 2011  ( early recovery)
            "https://www.federalreserve.gov/monetarypolicy/files/20100721_mprfullreport.pdf",
            "https://www.federalreserve.gov/monetarypolicy/files/20090721_mprfullreport.pdf",            # July 2009  ( end of Great Recession)
            "https://www.federalreserve.gov/monetarypolicy/files/20080715_mprfullreport.pdf",
            "https://www.federalreserve.gov/monetarypolicy/files/20070718_mprfullreport.pdf" ,           # July 2007  ( eve of  Great Recession)
            "https://www.federalreserve.gov/boarddocs/hh/2006/july/fullreport.pdf",
            "https://www.federalreserve.gov/boarddocs/hh/2005/july/fullreport.pdf",                      # July 2005  ( housing boom)
            "https://www.federalreserve.gov/boarddocs/hh/2004/july/fullreport.pdf",
            "https://www.federalreserve.gov/boarddocs/hh/2003/july/FullReport.pdf" ,                     # July 2003  ( deflation fears)
            "https://www.federalreserve.gov/boarddocs/hh/2002/july/FullReport.pdf",
            "https://www.federalreserve.gov/boarddocs/hh/2001/july/FullReport.pdf",                      # July 2001  ( dot come Recession)
            "https://www.federalreserve.gov/boarddocs/hh/2000/July/FullReport.pdf",
            "https://www.federalreserve.gov/boarddocs/hh/1999/July/FullReport.pdf",                      # July 1999  ( eve of dotcom Recession)
            "https://www.federalreserve.gov/boarddocs/hh/1998/july/FullReport.pdf",
            "https://www.federalreserve.gov/boarddocs/hh/1997/july/FullReport.pdf",                       # July 1997 ( irrational exhuberance)
            "https://www.federalreserve.gov/boarddocs/hh/1996/july/FullReport.pdf"
            )


df_fed <- 
  data.frame(report=c("Jul2018",paste0("Jul",seq(2017,1996,-1))),stringsAsFactors = FALSE) %>%
  mutate(text= map(fed.links,pdf_text)) %>% unnest(text) %>% 
  group_by(report) %>% mutate(page=row_number()) %>%
  ungroup() %>% mutate(text=strsplit(text,"\r")) %>% unnest(text) %>% mutate(text=gsub("\n","",text)) %>%
  group_by(report) %>% mutate(line=row_number()) %>% ungroup() %>% select(report,line,page,text)

fed_words <- df_fed %>%
  unnest_tokens(word, text) %>%
  count(report, word, sort = TRUE) %>%
  ungroup()

total_words <- fed_words %>% 
  group_by(report) %>% 
  summarize(total = sum(n))

# total words per report

ggplot(data=total_words, aes(x=seq(1996,2018),y=total))+
  geom_line(color="#27408b")+
  geom_point(shape=21,fill="white",color="#27408b",size=3,stroke=1.1)+
  scale_y_continuous(labels=scales::comma)+
  theme_ridges(font_family="Roboto")+
  labs(x="year",y="number of words",
       title="Number of words in Federal Reserve Monetary Policy Report",
       subtitle="July of each year 1996-2018",
       caption="@Anoop Source: Federal Reserve Board Monetary Policy Reports")
 
 
 fed_text <- 
  df_fed %>% 
  select(report,page,line,text) %>%
  unnest_tokens(word,text)

fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  anti_join(stop_words) %>%
  group_by(report) %>%
  count(word,sort=TRUE) %>% 
  mutate(rank=row_number()) %>%
  ungroup() %>% 
  arrange(rank,report) %>%
  filter(rank<11) %>% 
  ggplot(aes(y=n,x=fct_reorder(word,n))) +
  geom_col(fill="#27408b")+
  facet_wrap(~report,scales="free", ncol=5)+
  coord_flip()+
  theme_ridges(font_family="Roboto", font_size=10)+
  labs(x="",y="",
       title="Most Frequent Words Federal Reserve Monetary Policy Report",
       subtitle="Excluding stop words and numbers.",
       caption="@Anoop Source: Federal Reserve Board Monetary Policy Reports")
   
  # Custom stop words 
custom_stop_words <- 
  bind_rows(data_frame(word = c(tolower(month.abb), "one","two","three","four","five","six",
                                "seven","eight","nine","ten","eleven","twelve","mam","ered",
                                "produc","ing","quar","ters","sug","quar",'fmam',"sug",
                                "cient","thirty","pter",
                                "pants","ter","ening","ances","www.federalreserve.gov",
                                "tion","fig","ure","figure","src"), 
                       lexicon = c("custom")), 
            stop_words)


fed_textb <- 
  fed_text %>%

  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  count(report,word,sort=TRUE) %>%
  bind_tf_idf(word, report, n) %>%
  arrange(desc(tf_idf))

fed_textb %>% 
    anti_join(custom_stop_words, by="word") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(report) %>%
  mutate(id=row_number()) %>%
  ungroup() %>%
  filter(id<11) %>%
  ggplot(aes(word, tf_idf, fill = report)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~report,scales="free", ncol=5)+
  coord_flip()+
  theme_ridges(font_family="Roboto", font_size=10)+
  theme(axis.text.x=element_blank())+
  labs(x="",y ="tf-idf",
       title="Highest tf-idf words in each Federal Reserve Monetary Policy Report: 1996-2018",
       subtitle="Top 10 terms by tf-idf statistic: term frequncy and inverse document frequency",
       caption="@Anoop Source: Federal Reserve Board Monetary Policy Reports\nNote: omits stop words, date abbreviations and numbers.")
    
   fed_sentiment <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
  
  ggplot(fed_sentiment,  aes(index, sentiment, fill = sentiment>0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("red","#27408b"))+
  facet_wrap(~report, ncol = 5, scales = "free_x")+
  theme_ridges(font_family="Roboto")+
  labs(x="index (approximately 3 pages per unit)",y="sentiment",
       title="Sentiment through Federal Reserve Monetary Policy Report",
       subtitle="customized bing lexicon",
       caption="@lenkiefer Source: Federal Reserve Board Monetary Policy Reports")
       
   fed_sentiment2 <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
  
  ggplot(fed_sentiment2,  aes(factor(1996:2018), sentiment/(negative+positive), fill = sentiment)) +
  geom_col(show.legend = FALSE) +scale_fill_viridis_c(option="C")+
    theme_ridges(font_family="Roboto",font_size=10)+
  labs(x="report for July of each year",y="Sentiment (>0 positive, <0 negtaive)",
       title="Sentiment of Federal Reserve Monetary Policy Report: 1996-2018",
       subtitle="customized bing lexicon",
       caption="@Anoop Source: Federal Reserve Board Monetary Policy Reports")
   
  
  
  word_cors <- 
  fed_text2 %>% 
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  filter(!word %in% stop_words$word) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color ="#27408b", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void(base_family="Roboto")+
  labs(title="  Pairs of words in Federal Reserve Monetary Policy Reports that show at\n  least a 0.15 correlation of appearing within the same 10-line section", caption="  @lenkiefer Source: July Federal Reserve Board Monetary Policy Reports 1996-2018    \n")
  
  
