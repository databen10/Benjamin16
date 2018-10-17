data<-read.csv("C:/Users/wise/Desktop/corr/olympic.csv")
mysport<-as.numeric(data)
View(data)
summary(data)
summary(data$Sport)
data$Sport[7,8,9,11,15,16, drop=TRUE]
G<-droplevels(data$Sport, except, exclude = c(1:6,10))
#####FREQUENCY##########
olympic<-ts(mysport,frequency=1,start=c(1896,1),end=c(2016,1))
plot(olympic)
  olymp<-window(olympic,start=c(2014,6), end=c(2015,6))
  
########################FEESMUSTFALL SA##################################
  # In running the read.csv there is need to explicitly set stringsAsFactors=FALSE so as to get rid off converting the timestamp issue
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(tidytext)
  data<-read.csv("C:/Users/wise/Documents/Feesmustfall.csv", stringsAsFactors=FALSE)
  times = data[,"Date"]
  stringr::str_replace_all(times, " \\+0000", "") -> times
  as.POSIXlt(times) %>% 
    weekdays() %>% table() %>% 
    data.frame() -> days
  names(days) = c("Days", "Count")
  days = days[c(2,6,7,5,1,3,4),]
  days %>% ggplot(aes(Days, Count, fill = Count)) + geom_bar(stat = "identity") + 
    scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", 
                                "Thursday", "Friday", "Saturday", "Sunday")) + 
    ggtitle("Total Number of Tweets on Each Day of the Week") + 
    theme(plot.title=element_text(face="bold")) + guides(fill=FALSE)
  as.POSIXct(times) %>% 
    months() -> x
  
  as.POSIXct(times) %>% 
    weekdays() -> y
  
  substr(times, 0, 4) -> z
  
  tDates = data.frame(x,y,z)
  
  names(tDates) = c("Month", "Day", "Year")
  
  s_tDates = summarise(group_by(tDates,Day, Month, Year), Count = length(Year))
  
  s_tDates$Month = factor(s_tDates$Month, levels = c("January", "February", "March", "April", 
                                                     "May", "June", "July", "August", "September", 
                                                     "October", "November", "December"))
  
  o_days = c("Monday", "Tuesday", "Wednesday", 
             "Thursday", "Friday", "Saturday", "Sunday")
  
  ggplot(s_tDates, aes(Day, Count, fill = Year)) + geom_bar(stat = "identity") + 
    facet_wrap(~Month) + scale_x_discrete(limits = o_days) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    scale_fill_brewer(palette = "Set2") + 
    theme(axis.text.y=element_text(face = "bold", color = "black"), 
          axis.text.x=element_text(face = "bold", color = "black"))
  
summarise(group_by(tDates, Month, Year), Count = length(Year)) -> s_tDates
  
  o_months = c("January", "February", "March", "April", "May", "June", 
               "July", "August", "September", "October", "November", "December")
  
  ggplot(s_tDates, aes(Month, Count, fill = Count)) + geom_bar(stat = "identity") + 
    facet_wrap(~Year, nrow = 1) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_discrete(limits = o_months) + scale_fill_gradient(low="Black", high="Blue") + 
    guides(fill=FALSE) + theme(axis.text.y=element_text(face = "bold", color = "black"), 
                               axis.text.x=element_text(face = "bold", color = "black"))
  
  #####To have a plot of multiple sentiment in different 
  lyrics$lyric <- as.character(lyrics$lyric)
  data$Tweet<-as.character(data$Tweet)
  tidy_lyrics <- lyrics %>% 
    unnest_tokens(word,lyric)
  tidy_Tweets<-data %>%
    unnest_tokens(word,Tweet)
  
  song_wrd_count <- tidy_lyrics %>% count(track_title)
  Tweet_count<-tidy_Tweets %>% count(User.Name) 
  
  lyric_counts <- tidy_lyrics %>%
    left_join(song_wrd_count, by = "track_title") %>% 
    rename(total_words=n)
  Tweet_count<-tidy_Tweets %>%
    left_join(Tweet_count, by="User.Name") %>%
    rename(total_words=n)
  
  lyric_sentiment <- tidy_lyrics %>% 
    inner_join(get_sentiments("nrc"),by="word")
  Tweet_sentiment<- tidy_Tweets %>%
    inner_join(get_sentiments("nrc"), by="word")
  
  lyric_sentiment %>% 
    count(word,sentiment,sort=TRUE) %>%
    group_by(sentiment)%>%top_n(n=10) %>% 
    ungroup() %>%
    ggplot(aes(x=reorder(word,n),y=n,fill=sentiment)) + 
    geom_col(show.legend = FALSE) + 
    facet_wrap(~sentiment,scales="free") + 
    coord_flip()
  lyric_sentiment %>%
    count(word,sentiment,sort=TRUE) %>%
    group_by(sentiment)%>%top_n(n=10) %>%
    ungroup() %>%
    ggplot(aes(x=reorder(word,n),y=n,fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment,scales="free") +
    coord_flip()