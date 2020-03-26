##############################################################################
######Importing Data of Mandolorian  Reviews from IMDB, Forbes and NYT########
##############################################################################
#word tekenization
library(textreadr)
NYT <- read_document(file="C:/Users/Asad's PC/Desktop/Text Analysis/Business Case/The New York Times.txt")
#View(NYT)

#word tekenization
Forbes <- read_document(file="C:/Users/Asad's PC/Desktop/Text Analysis/Business Case/Forbes.txt")
#View(Forbes)

#word tekenization
IMDB <- read_document(file="C:/Users/Asad's PC/Desktop/Text Analysis/Business Case/IMDB.txt")
#View(IMDB)

######################################################
###STEP2: Putting the vector in a data frame##########
######################################################

library(dplyr)
NYT_df    <- data_frame(text=NYT)
Forbes_df <- data_frame(text=Forbes)
IMDB_df   <- data_frame(text=IMDB)

#View(NYT_df)

combined_reviews <- bind_rows(
  mutate(NYT_df),
  mutate(Forbes_df),
  mutate(IMDB_df)
)

######################################################
######## Step3: tokenizing the mydf dataframe#########
######################################################
#install.packages("tidytext")
#install.packages("tidyverse")
library(tidyverse)
library(tidytext)

NYT_token_list <- NYT_df %>%
  unnest_tokens(word, text)
#no punctutation, no upper case letters

Forbes_token_list <- Forbes_df %>%
  unnest_tokens(word, text)

IMDB_token_list <- IMDB_df %>%
  unnest_tokens(word, text)
#######################################################################################
combined_reviews_token_list <- combined_reviews %>%
  unnest_tokens(word, text)

#######################################################
##########STEP4: token frequencies####################
#######################################################
library(stringr)

cust_word <- bind_rows(data_frame(word = c("vote", "helpful", "star", "found", "like", stopwords("en"))))

combined_frequencies_tokens <- combined_reviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_word) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sort=TRUE)
print(combined_frequencies_tokens)


#########################################CWF######################
library(wordcloud)

combined_frequencies_tokens %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
combined_frequencies_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(0.5,0.5),
                   fixed.asp=TRUE,
                   title.size = 1
  )


combined_frequencies_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(2,2),
                   fixed.asp=TRUE,
                   title.size = 1
  )




################################################################################################


my_combined_reviews <- bind_rows(
  mutate(lienumber = row_number(),IMDB_token_list,location="one"),
  mutate(lienumber = row_number(),Forbes_token_list,location = "two"),
  mutate(lienumber = row_number(),NYT_token_list, location = "three")
)

#my_combined_reviews %>%
#  bind_tf_idf(word, location, n)



#Do Sentiment Score
library(tidytext)

afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")


sentiments <- bind_rows(mutate(afinn, lexicom="afinn"),
                        mutate(nrc, lexicon="nrc"),
                        mutate(bing, lexicon = "bing"))



#Get sentiment graphs

afinn <- my_combined_reviews %>%
  inner_join(get_sentiments("afinn"))%>%
  anti_join(cust_word) %>%
  group_by(index=lienumber %/% 80) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  my_combined_reviews%>%
    inner_join(get_sentiments("bing"))%>%
    anti_join(cust_word) %>%
    mutate(method = "Bing et al."),
  my_combined_reviews %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    anti_join(cust_word) %>%
    mutate(method = "NRC")) %>%
  count(method, index=lienumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

bing_counts <- my_combined_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(cust_word) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



####################################################################################################################
###################################################################################################################
#Do n-grams : combined_reviews
###################################################################################################################
library(dplyr)
library(tidytext)
library(tidyr)

mando_bigrams_combined_reviews <- combined_reviews%>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

mando_bigrams_combined_reviews #We want to see the bigrams (words that appear together, "pairs")

mando_bigrams_combined_reviews %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated_combined_reviews <- mando_bigrams_combined_reviews %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_combined_reviews <- bigrams_separated_combined_reviews %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts_combined_reviews <- bigrams_filtered_combined_reviews %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_combined_reviews

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram_combined_reviews <- combined_reviews %>%
  unnest_tokens(quadrogram_combined_reviews, text, token = "ngrams", n=4) %>%
  separate(quadrogram_combined_reviews, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_combined_reviews


######################################################
######## visualising negated words ###################
###### negated words in sentiment analysis ###########
######################################################

negation_tokens <- c('not', 'no', 'never', 'without')#what negation tokens do you want to use?

negated_words_combined_reviews <- bigrams_separated_combined_reviews %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words_combined_reviews



#################################################
#### we can visuals the negated words ###########
#we'll create a function to plot the negations###
#################################################
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*score, fill = n*score >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph_combined_reviews <- bigram_counts_combined_reviews %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph_combined_reviews

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_combined_reviews, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

####################################################################################################################
