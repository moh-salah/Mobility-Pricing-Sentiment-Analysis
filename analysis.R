# How do Vancouverites feel about (de)-congestion pricing?
# load libraries and authorization keys ####
# goolge nlp api authorization key
GL_AUTH <- "google_api_authorization.json"
library(googleLanguageR)
library(tidyverse)
library(purrr)
library(Rfacebook)
library(RCurl)
library(wordcloud)
library(tidytext)
library(tm)
library(broom)
library(sentimentr)
library(syuzhet)
library(stringi)
library(plotly)
library(broom)

# load helper functions
source("helper_functions.R")

# facebook api authorization- run once and save to load later
# app_id <- "your_app_id"
# app_secret <- "your_app_secret"
# fb_oauth <- fbOAuth(app_id=app_id,app_secret=app_secret)
# save(fb_oauth, file="fb_oauth")
load('fb_oauth')

# facebook page id of "It's Time, Metro Vancouver"
page_id <- 1973735886181805

# get data from facebook page ####
# get posts
posts <- getPage(page_id, token = fb_oauth, n = 5e10, reactions = TRUE)

# get post comments
c <- NULL
for (i in 1:nrow(posts)) {
  c[[i]] <- getPost(posts$id[i], token = fb_oauth, n = 5e10)[['comments']]
}
comments <- do.call(rbind, c)

# get comment replies
cr <- NULL
for (i in 1:nrow(comments)) {
  cr[[i]] <- getCommentReplies(comments$id[i], token = fb_oauth, n = 5e10)[['comments']]
}
comments_replies <- do.call(rbind, cr)

# make sure all messages are encoded as "UTF-8"
posts$message <- stri_encode(posts$message, "", "UTF-8")
comments$message <- stri_encode(comments$message, "", "UTF-8")
comments_replies$message <- stri_encode(comments_replies$message, "", "UTF-8")

# all comments (main and replies):
main <- comments %>%
  subset(select = -c(comments_count)) %>% #drop comments_count column
  mutate(type = 1) #create an comment type id; 1 means main comments

reply <- comments_replies %>%
  mutate(type = 2) #create an comment type id; 2 means comment replies

colnames(reply) <-
  names(main) #rename columns in reply df as main to merge
all_comments <- rbind(main, reply) #merge the 2 dfs

# save dfs
# write_delim(posts, paste0('posts', Sys.Date(),'.txt'), delim = '\t', col_names = TRUE)
# write_delim(comments, paste0('comments', Sys.Date(),'.txt'), delim = '\t', col_names = TRUE)
# write_delim(comments_replies, paste0('comments_replies', Sys.Date(),'.txt'), delim = '\t', col_names = TRUE)
# write_delim(all_comments, paste0('all_comments', Sys.Date(),'.txt'), delim = '\t', col_names = TRUE)

# create datetime variable for trend analysis
posts$datetime <- format(format.facebook.date(posts$created_time), tz = "US/Pacific")
posts$month <- format(as.Date(posts$datetime), "%Y-%m")
all_comments$datetime <- format(format.facebook.date(all_comments$created_time), tz = "US/Pacific")
all_comments$month <- format(as.Date(all_comments$datetime), "%Y-%m")

# link post and comment ids
posts$post_id <- gsub(".*_", "", posts$id)
all_comments$post_id <- gsub("_.*", "", all_comments$id)

# posts analysis ####

# which post got the most likes
posts[which(posts$likes_count == max(posts$likes_count)), ]$message
# which post got the most angry emotions
posts[which(posts$angry_count == max(posts$angry_count)), ]$message
# which post got the most comments
posts[which(posts$comments_count == max(posts$comments_count)), ]$message
# it seems that 'distance-based' charging is the most controversial topic - not surprising!

# number of posts per month
posts %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_col(aes(x = month, y = count, fill = month), alpha = 0.9) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Month") + ylab("Number of Posts") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("posts_per_month.png", width = 10, height = 6.6)

# engagement level per month
posts %>%
  group_by(month) %>%
  summarise(
    engagement = sum(
      likes_count + angry_count + sad_count + love_count + wow_count + haha_count +
        shares_count + comments_count
    )
  ) %>%
  ggplot() +
  geom_col(aes(x = month, y = engagement, fill = month), alpha = 0.9) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Month") + ylab("Engagment Level") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggsave("engagment_level_per_month.png", width = 10, height = 6.6)

# summary of post reactions
posts %>%
  summarise(
    Like = sum(likes_count),
    Love = sum(love_count),
    Angry = sum(angry_count),
    Sad = sum(sad_count),
    Wow = sum(wow_count),
    Haha = sum(haha_count)
  ) %>%
  gather(reaction, count, 1:6) %>%
  mutate(reaction = factor(reaction,levels = c('Like', 'Angry', 'Haha', 'Love', 'Sad', 'Wow')),
         percent = 100*prop.table(count)) %>%
  ggplot(aes(reaction, percent)) +
  geom_col(aes(fill = reaction), alpha = 0.9) +
  geom_text(aes(label= paste0(round(percent,0),'%'), x=reaction, y=percent + 2)) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Reactions") + ylab("%") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggsave("reactions.png", width = 10, height = 6.6)

# likes/angry emotions per month
posts %>%
  group_by(month) %>%
  summarise(
    Like = mean(likes_count),
    Angry = mean(angry_count)
  ) %>%
  gather(reaction, mean, 2:3) %>%
  mutate(reaction = factor(reaction,levels = c('Like', 'Angry'))) %>%
  ggplot(aes(month, mean)) +
  geom_col(aes(fill = reaction), alpha = 0.9) +
  facet_wrap( ~ reaction) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Month") + ylab("Average Number of Reactions") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 13))+
  ggsave("likes_angry_per_month.png", width = 10, height = 6.6)

# reactions classified into positve and negative
posts %>%
  summarise(
    positive = sum(likes_count)+sum(love_count),
    negative = sum(angry_count)+sum(sad_count)+sum(haha_count)+sum(wow_count)
  ) %>%
  mutate(Positive = 100*positive/sum(positive+negative),
         Negative = 100*negative/sum(positive+negative)) %>%
  gather(reaction, percent, 3:4) %>%
  mutate(reaction = factor(reaction, levels = c('Positive', 'Negative'))) %>%
  ggplot(aes(factor(reaction), percent)) +
  geom_col(aes(fill = reaction), alpha = 0.9) +
  geom_text(aes(label= paste0(round(percent,0),'%'), x=reaction, y=percent + 2)) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Reaction") + ylab("%") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("pos_neg.png", width = 10, height = 6.6)

# likes by post type
posts %>%
  group_by(type) %>%
  summarise(count = n(),
            likes = sum(likes_count),
            avg_likes = mean(likes_count)) %>%
  ggplot()+
  geom_col(aes(type, avg_likes, fill=type), alpha=0.9)+
  xlab("Post Type") + ylab("Average Number of Likes")+
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggsave("likes_by_post_type.png", width = 10, height = 6.6)

# comments analysis ####

# drop all comments by page admins to analyze user comments only
comments_analysis <- filter(all_comments,!(from_name %in% c("It's Time, Metro Vancouver")))

# analyze as words ####

# remove numbers
comments_analysis$message <- removeNumbers(comments_analysis$message)
# word tokenization
comments_words <- comments_analysis %>%
  unnest_tokens(word, message) %>%
  #mutate(word = SnowballC::wordStem(word,language = 'en')) %>% #stemming is not working very well here...
  anti_join(stop_words, by = "word")

# manual stemming for top words
comments_words$word <- ifelse(comments_words$word == 'taxes', 'tax', comments_words$word)
comments_words$word <- ifelse(comments_words$word == 'roads', 'road', comments_words$word)

# top used words
comments_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#7570B3") +
  ylab("Occurrences") +
  xlab(NULL) +
  coord_flip()+
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggsave("top_words.png", width = 10, height = 6.6)

# wordcloud
png(filename = "wordcloud.png", width = 20, height = 15, units = "cm", res=600)

comments_words %>%
  count(word) %>%
  with(
    wordcloud(
      words = word,
      freq = n,
      max.words = 250,
      min.freq = 25,
      rot.per = .15,
      random.order = TRUE,
      colors = brewer.pal(8, 'Dark2')
    )
  )

dev.off()

# sentiment analysis
# using nrc lexicon
comments_words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(sentiment, sort = TRUE)  %>%
  mutate(sentiment = factor(sentiment, levels = c('positive', 'negative',"trust","anger", "disgust", "fear", "anticipation", "sadness", "joy", "surprise"),
                          labels = c('Positive', 'Negative', "Trust","Anger", "Disgust", "Fear", "Anticipation", "Sadness", "Joy", "Surprise")),
       percent = 100*prop.table(n)) %>%
  ggplot(aes(sentiment, percent)) +
  geom_col(aes(fill = sentiment), alpha = 0.9) +
  scale_fill_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666", '#e9967a', '#006633')) +
  xlab("Sentiment") + ylab("% of Words") +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("nrc_sentiments.png", width = 10, height = 6.6)

# using bing lexicon
comments_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment, sort = TRUE) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"), 
                            labels = c("Positive", "Negative")),
         percent = 100*prop.table(n)) %>% 
  ggplot(aes(sentiment, percent)) +
  geom_col(aes(fill = sentiment), alpha = 0.9) +
  geom_text(aes(label= paste0(round(percent,0),'%'), x=sentiment, y=percent + 2)) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Sentiment") + ylab("% of Words") +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("bing_posneg.png", width = 10, height = 6.6)

# using afinn lexicon
comments_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  ggplot(aes(score)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#7570B3") +
  scale_x_continuous(breaks = seq(-5,5,1)) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  xlab("Score") + ylab("% of Words") +
  ggsave("afinn_score.png", width = 10, height = 6.6)
# afinn score by month
comments_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(month) %>%
  summarise(score = mean(score)) %>%
  ggplot(aes(month, score)) +
  geom_col(aes(fill = month), alpha = 0.9) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  xlab("Month") + ylab("Average Score") +
  ggsave("words_score_per_month.png", width = 10, height = 6.6)

# analyze as comments ####
# sentiment analysis

# using google nlp api
gl_auth("google_api_authorization.json")
# analyze text
nlp_results <- gl_nlp(comments_analysis$message)
# extract sentiment score and magnitude from nlp_results
sentiment_google <- nlp_results$documentSentiment 
# save df
# write_delim(sentiment_google, paste0('google_nlp_results_',Sys.Date(),'.txt'), delim = '\t', col_names = TRUE)

# merge analysis df with sentiment score/magnitude from google nlp
comments_analysis <- cbind(comments_analysis, sentiment_google)
# drop NA values
comments_analysis <- comments_analysis[!is.na(comments_analysis$score),]

# score by month
comments_analysis %>%
  group_by(month) %>%
  summarise(score = mean(score)) %>%
  ggplot() +
  geom_col(aes(month, score, fill = month), alpha = 0.8) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Month") + ylab("Average Sentiment Score") +
  theme(legend.position = 'none')

# relationship between sentiment magnitude and comment length
ggplot(comments_analysis) +
  geom_point(aes(
    x = magnitude,
    y = nchar(comments_analysis$message),
    color = score
  ), alpha = 0.6) +
  scale_colour_gradientn("Sentiment Score", colours = rainbow(3)) +
  xlab("Sentiment Magitude") + ylab("Comment Length (# of Characters)")

# analyze sentiment magnitude
summary(comments_analysis$magnitude)
# histogram of sentiment magnitude
ggplot(comments_analysis) +
  geom_histogram(aes(magnitude), bins = 40, fill = "#7570B3") +
  scale_x_continuous(breaks = c(0:15)) +
  xlab('Sentiment Magnitude') + ylab('Comment Count')
# kde sentiment magnitude
ggplot(comments_analysis) +
  geom_density(aes(magnitude), fill = "#7570B3", alpha = 0.5) +
  xlab('Sentiment Magnitude') + ylab('Density') 

# analyze sentiment score
summary(comments_analysis$score)
# histogram of sentiment score
ggplot(comments_analysis, aes(score)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#7570B3") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent)+
  xlab('Sentiment Score') + ylab('% Comments')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("score_distn.png", width = 10, height = 6.6)

# kde sentiment score
ggplot(comments_analysis) +
  geom_density(aes(score), fill = "#7570B3", alpha = 0.8) +
  xlab('Sentiment Score') + ylab('Density')

# break sentiment scores into buckets according to google's documentation
length(which(comments_analysis$score >= 0.2)) #clearly positive
length(which(comments_analysis$score > 0 & comments_analysis$score < 0.2)) #low-emotion comments
length(which(comments_analysis$score == 0)) #low-emotion comments could be either neutral or mixed feelings
length(which(comments_analysis$score < 0 & comments_analysis$score > -0.2)) #low-emotion comments
length(which(comments_analysis$score <= -0.2)) #clearly negative

# examine the distribution of the sentiment magnitude for sentiment score between -0.2 and 0.2
ggplot(filter(comments_analysis, comments_analysis$score>-0.2 & comments_analysis$score<0.2)) +
  geom_histogram(aes(magnitude), bins = 20, fill = "#7570B3", alpha = 0.8) +
  scale_x_continuous(breaks = c(0:10))+
  geom_vline(xintercept = 0.5)

summary(filter(comments_analysis, comments_analysis$score>-0.2 & comments_analysis$score<0.2)$magnitude)
# it seems that 0.5 is a reasonable cut off to define high/low magnitude

# how many comments where the score is low (between -0.2 and 0.2) and the magnitude is high >=0.5 - mixed feelings
length(which(comments_analysis$score > -0.2 &
               comments_analysis$score < 0.2 & 
               comments_analysis$magnitude >= 0.5))
# how many comments where the score is low (between -0.2and 0.2) and the magnitude is low<0.5 - neutral feelings
# how many?
length(which(comments_analysis$score > -0.2 &
      comments_analysis$score < 0.2 & 
      comments_analysis$magnitude < 0.5))
# pretty close 

# define emotion level based on score and magnitude
comments_analysis$emotionlevel <-
  ifelse(
    comments_analysis$score >= 0.2, 'Positive',
    ifelse(
      comments_analysis$score <= -0.2, 'Negative',
      ifelse((comments_analysis$score > -0.15 &
                comments_analysis$score < 0.15 &
                comments_analysis$magnitude >= 0.5), "Mixed Feelings",
      "Neutral Feelings")))

# plot emotion level
comments_analysis %>%
  group_by(emotionlevel) %>%
  summarise(count = n()) %>%
  mutate(count_percent = 100*prop.table(count),
         emotionlevel = factor(emotionlevel,
                               levels = c('Positive', 'Negative', 'Neutral Feelings', 'Mixed Feelings'))
  ) %>%
  ggplot() +
  geom_col(aes(emotionlevel, count_percent, fill = emotionlevel), alpha =0.9) +
  geom_text(aes(label= paste0(round(count_percent,1),'%'), x=emotionlevel, y=count_percent + 2)) +
  scale_fill_manual(values = c("green3", 'red3', 'orange', 'blue3')) +
  xlab("Emotion Level") + ylab("% of Comments") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("gg_emotionlevel.png", width = 10, height = 6.6)

  # plot by type (main comment or reply)
comments_analysis %>%
  mutate(emotionlevel = factor(emotionlevel,
                               levels = c('Positive', 'Negative', 'Neutral Feelings', 'Mixed Feelings')))%>%
  group_by(emotionlevel, type) %>%
  summarise(count = n()) %>%
  mutate(percent = 100*prop.table(count),
         type = factor(type, levels = c("1", "2"), labels = c("Comment", "Comment Reply"))) %>% 
  ggplot() +
  geom_col(aes(emotionlevel, count, fill = emotionlevel), alpha = 0.9) +
  scale_fill_manual(values = c("green3", 'red3', 'orange', 'blue3')) +
  xlab("Emotion Level") + ylab("Count") +
  theme(legend.position = 'none')+
  facet_wrap(~type, labeller = label_value)

# add the number of likes
comments_analysis %>%
  group_by(emotionlevel) %>%
  summarise(
    count = n(),
    likes = sum(likes_count)
    ) %>%
  mutate(count_plus_likes= count+likes,
         count_percent = 100*prop.table(count),
         count_plus_likes_percent =100*prop.table(count_plus_likes),
         emotionlevel = factor(emotionlevel,
           levels = c('Positive', 'Negative', 'Neutral Feelings', 'Mixed Feelings'))
         ) %>%
  ggplot() +
  geom_col(aes(emotionlevel, count_plus_likes_percent, fill = emotionlevel), alpha =0.9) +
  geom_text(aes(label= paste0(round(count_plus_likes_percent,0),'%'), x=emotionlevel, y=count_plus_likes_percent + 2)) +
  scale_fill_manual(values = c("green3", 'red3', 'orange', 'blue3')) +
  xlab("Emotion Level") + ylab("% of Comments & Likes") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("gg_emotionlevel_by_likes.png", width = 10, height = 6.6)

# final plot
grouped_reactions <- reactions %>%
  gather() %>%
  mutate(emotionlevel = ifelse(key %in% c('Like', 'Love'), 'Positive', 'Negative')) %>%
  group_by(emotionlevel) %>%
  summarise(reactions = sum(value))

comments_analysis %>%
  group_by(emotionlevel) %>%
  summarise(
    count = n(),
    likes = sum(likes_count)
  ) %>%
  merge(grouped_reactions, all.x = T) %>%
  mutate(total = rowSums(.[c("count", "likes", "reactions")], na.rm = T),
         total_percent =100*prop.table(total),
         emotionlevel = factor(emotionlevel,
                               levels = c('Positive', 'Negative', 'Neutral Feelings', 'Mixed Feelings'))) %>%
  ggplot() +
  geom_col(aes(emotionlevel, total_percent, fill = emotionlevel), alpha =0.9) +
  geom_text(aes(label= paste0(round(total_percent,0),'%'), x=emotionlevel, y=total_percent + 2)) +
  scale_fill_manual(values = c("green3", 'red3', 'orange', 'blue3')) +
  xlab("Emotion Level") + ylab("% of Impressions") +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggsave("gg_emotionlevel_total.png", width = 10, height = 6.6)