#--------------------------
#Loading required libraries
#--------------------------
library(readr)
library(stringr)
library(devtools)
library(syuzhet)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)

#Reading the Novel's text file as raw character vectors from Project Gutenberg

raw_wonderland <- read_lines("https://www.gutenberg.org/files/11/11-0.txt",skip = 31,n_max = 3340)

#Splitting raw text into paragraphs containing 10 lines
typeof(raw_wonderland)


wonderland <-character()
  for(i in seq_along(raw_wonderland)){
   if (i%%10==1) wonderland[ceiling(i/10)] <- str_c(raw_wonderland[i],
                                                    raw_wonderland[i+1],
                                                    raw_wonderland[i+2],
                                                    raw_wonderland[i+3],
                                                    raw_wonderland[i+4],
                                                    raw_wonderland[i+5],
                                                    raw_wonderland[i+6],
                                                    raw_wonderland[i+7],
                                                    raw_wonderland[i+8],
                                                    raw_wonderland[i+9],sep = ""
                                                    )
  }

#Adding the line number and getting the sentiment for the text
wonderland_nrc <- cbind(linenumber = seq_along(wonderland), get_nrc_sentiment(wonderland))

#Finding linenumbers of Chapters
grep("CHAPTER ",wonderland)

#Creating 3 volumes with 4 chapters each
wonderland_nrc$volume <- "Volume I"
wonderland_nrc[67:length(wonderland), 'volume']<- "Volume II"
wonderland_nrc[188:length(wonderland),'volume']<- "Volume III"
wonderland_nrc$linenumber[wonderland_nrc$volume=="Volume II"] <- seq_along(wonderland)
wonderland_nrc$linenumber[wonderland_nrc$volume=="Volume III"] <- seq_along(wonderland)
wonderland_nrc$volume <- as.factor(wonderland_nrc$volume)
levels(wonderland_nrc$volume)<- c("Volume I","Volume II","Volume III")

#Exploring Positive and negative sentiments
wonderland_nrc$negative <- -(wonderland_nrc$negative)
yinyang <- wonderland_nrc %>% select(linenumber,volume,positive,negative) %>% 
  melt(id=c("linenumber","volume"))
names(yinyang)<- c("linenumber","volume","Sentiment","value")
yinyang$Sentiment<- as.factor(yinyang$Sentiment)
typeof(yinyang$Sentiment)

#Plotting POsitive and negative sentiments
ggplot(data=yinyang, aes(x = linenumber,y = value, color = yinyang$Sentiment))+
  geom_point(size = 4, alpha = 0.5)+ facet_wrap(~volume ,nrow = 3)
  
 levels(yinyang$Sentiment) 
   + theme_minimal() +
  ylab("Sentient") 
