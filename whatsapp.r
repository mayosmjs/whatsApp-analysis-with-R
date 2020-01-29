library("readr")
library("tidyverse")
library("lubridate")
library(plotly)
library(ggrepel)#geom_label with no overlaps
library(qdapRegex)#exctract links
library(urltools)
library(Rexperigen)
library("tidytext")
library(rwhatsapp)


wdata <- rwa_read("txtx.txt")
# Filter rows of <Media omitted> and messages that dont have authors
wdata <- wdata %>% filter(author != "" & text != "<Media omitted>" & !is.na(author))


#NUMBER OF MESSAGES [ MOST ACTIVE MEMBER ]
ggplotly(
  wdata %>%
    count(author) %>%
    ggplot(aes(x = reorder(author, n), y = n ,fill = author)) +
    geom_bar(stat = "identity") +
    ylab("Totals") + xlab("Group Members") +
    coord_flip() +
    ggtitle("Number of messages sent") +
    theme_minimal()
)

#DAYS OF WEEK MOST ACTIVE
wdata <- wdata %>% mutate(Dow =  wday(as.Date(wdata$time), label=TRUE))

dow <- wdata %>% filter(Dow !='') %>% group_by(Dow) %>% summarise(count = n())
ggplot(dow,aes(x=Dow,y = count, fill = Dow))+
  geom_bar(stat = "identity")+
  xlab("Days of the week")+
  ylab("Messages")+
  coord_flip()+
  geom_text(aes(label = scales::comma(count)), hjust = 3) +
  ggtitle("Days most active")+
  theme_minimal()



#MONTHS MOST ACTIVE AND TOP MEMBER
wdata <- wdata %>% mutate(months = month(as.POSIXct(wdata$time,'%m'),label = TRUE))

mnths <- wdata %>% filter(months !='') %>% group_by(months) %>% summarise(mcount = n())
actMember <- wdata %>% filter(months != '')%>% group_by(months,author)%>%summarise(scount = n())%>% slice(which.max(scount))
mnthsactMember <-  merge(mnths, actMember,by="months")


ggplot(mnthsactMember)+
  geom_bar(aes(x=months,y = mcount, fill = months),stat = "identity",width = 1)+
  geom_point(aes(x=months,y = scount,color = author),
             size = 4, alpha = 0.5,
             stat = "identity",
  )+
  # geom_text(aes(x=months,y = scount,label = Name), vjust = 0.5,hjust = -1,color ="white")+
  geom_label(aes(x=months,y = scount,label = paste0(author," (",scount,")")),
             fill = 'black', vjust = 0.5,hjust = -0.4,color ="white",alpha = 0.5,size = 3.5
  )+
  xlab("Months")+
  ylab("Messages")+
  coord_flip()+
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("MONTH ACTIVITY AND MOST ACTIVE MEMBER EACH MONTH")+
  theme_minimal(base_size = 10)



#in facet leads

ggplot(mnthsactMember)+
  geom_bar(aes(x=months,y = mcount, fill = months),stat = "identity",width = 1)+
  geom_point(aes(x=months,y = scount,color = author),
             size = 4, alpha = 0.5,
             stat = "identity",
  )+
  # geom_text(aes(x=months,y = scount,label = Name), vjust = 0.5,hjust = -1,color ="white")+
  geom_label(aes(x=months,y = scount,label = paste0(author," (",scount,")")),
             fill = 'black', vjust = 0.5,hjust = -0.4,color ="white",alpha = 0.5,size = 3.5
  )+
  xlab("Months")+
  ylab("Messages")+
  coord_flip()+
  ggtitle("MONTH ACTIVITY AND MOST ACTIVE MEMBER EACH MONTH")+
  theme_minimal(base_size = 10)





#MEMBER TOTAL WORDS USED
wdata = wdata %>% mutate(word_count = sapply(strsplit(wdata$text, " "), length))

words <- wdata %>% group_by(author)%>% summarise(count= sum(word_count))
ggplot(words,aes(x=author,y = count, fill = author))+
  geom_bar(stat = "identity")+
  xlab("Days of the week")+
  ylab("Messages")+
  geom_text(size = 3,aes(label = paste0(scales::comma(count), " (",round(count/sum(count)*100,0) ,"%)")), vjust = -1) +
  ggtitle("WRITTEN WORDS")+
  theme_minimal()





#AVERAGE MESSAGES PER DAY OF WEEK
wdata <- wdata %>% mutate(Tarehe = format(as.Date(wdata$time,format="%Y-%m-%d %H:%M:%S"), format = "%d"))
txtfreq <- wdata %>%  group_by(Dow,Tarehe) %>% summarise(count =n())

ggplot(txtfreq, aes(Tarehe, Dow)) + 
  geom_tile(aes(fill = count), colour = "white") + 
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  xlab("Days of month")+
  ylab("Week days")+
  ggtitle("AVERAGE MESSAGES PER DAY OF WEEK ")+
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"), panel.grid = element_blank()) + 
  coord_equal()+
  geom_text(aes(label=count),size = 3,color = "black")



# LNKS SHARED
links <- data %>%  select(Message) %>% transmute(url = rm_url(data$Message,extract = TRUE)) %>% filter(url != "") 
links$url <- as.character(links$url)
links$url <- urltools::domain(links$url)
links <- links %>% group_by(url) %>% summarise(count = n())
linksTop10 <- links  %>%  top_n(10) %>% arrange(desc(count))



#OFTEN USED WORDS
wordsToRemove<- c(stopwords(language = "en"),"media","omitted")

wdata %>% unnest_tokens(input = Message,output = word) %>%
  filter(!word %in% wordsToRemove) %>%
  count(Name, word, sort = TRUE) %>% group_by(author) %>%
  top_n(n = 1, n) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used word") +
  theme_bw()


#TIME SERIES CONVERSATION ( Time when most guys talk )
wdata = wdata %>% mutate(tHours = hour(data$Datetime))
ts <- wdata %>% group_by(tHours,author) %>% summarize(n = n())
ggplot(ts, aes(tHours, author)) + 
  geom_tile(aes(fill = n), colour = "white") + 
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"), panel.grid = element_blank()) + 
  xlab("Time in 24H")+
  coord_equal()+
  geom_text(aes(label=n),size = 3,color = "navy")




