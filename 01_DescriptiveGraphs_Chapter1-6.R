

#This is the code for the graphs in chapters 1-7.1

#load libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)#to change format
library(stargazer)#for tables
library(reshape2)#for melting 
library(effects)  #for multinominal predicted prob
library(scales)#for scale



##############-Election Share 

df_election <- read_excel ("other_data.xlsx", sheet = "election_results")

#reshape
df_election_long <- df_election %>% 
  pivot_longer(cols=c(sd_share,
                      s_share,
                      m_share),
               names_to = "party",
               values_to="share")

df_election_long <- df_election_long %>% 
  mutate(share =share/100,
         election_year = as.factor(election_year))

ggplot(df_election_long, aes(x=election_year, y= share, fill=party))+
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal()+
  labs(title="Voter's Share between 2006 and 2022",
       subtitle ="Moderate, Social Democratic, and SD Party",
       x="Election Year",
       y= "",
       fill="")+
  geom_text(aes(label = paste0(round(share,4)*100, "%")),
            vjust = -0.5,
            size = 2.4,
            position = position_dodge(width = 0.9))+
  scale_y_continuous(labels = scales ::percent_format(),
                     limits = c(0,0.35))+
  scale_fill_manual(values= c("m_share"= "#5e5e5e",
                              "s_share"="#c6c6c6",
                              "sd_share"="#00c5cf"),
                    labels = c("Moderate Party","Social Democratic Party","SD Party"))+
  theme(legend.position = "bottom")
ggsave("figures/01_votershare.png", width = 6, height = 4)

##############----Weekly Media use According to Reuters
df_weekly_media <- read_excel("other_data.xlsx", sheet = "weekly_use" )
df_weekly_media <- df_weekly_media %>% 
  mutate(year= as.factor(year),
         weekly_use=weekly_use/100)
df_weekly_media <- df_weekly_media %>% 
  mutate(company = factor (company,levels = c("Aftonbladet",
                                              "Expressen",
                                              "DagensNyheter",
                                              "Svenska Dagbladet",
                                              "SVT News")))

ggplot(df_weekly_media, aes(x = year, y = weekly_use, fill = company)) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs (title="Weekly Use of Online News Sites in %",
        subtitle ="Years 2018 and 2022",
        x= "Year",
        y= "",
        fill= "")+
  theme_minimal()+
  geom_text(aes(label = paste0(round(weekly_use,2)*100, "%")),
            vjust = -0.5,
            size = 3,
            position = position_dodge(width = 0.9))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.5))+
  scale_fill_manual(values= c("Aftonbladet"= rgb(104/255, 89/255, 108/255, 1),
                              "Expressen" = rgb(136/255, 115/255, 133/255, 0.7),                      
                              "DagensNyheter" = rgb(255/255, 150/255, 57/255, alpha = 0.9),
                              "Svenska Dagbladet" = rgb(255/255, 150/255, 57/255, alpha = 0.6),
                              "SVT News" = "#95ed94"))+
  theme(legend.position = "bottom")
ggsave("figures/02_weekly_us_online.png", width = 7, height = 4)


#load newspaper dataset
df <- read_excel("04-21-24_df_headings_sen.xlsx")


##############----Newspaper Frequency in Dataset
#data wrangling
table(df$newspaper)
afton <- sum(df$newspaper=="aftonbladet")
dagen <- sum(df$newspaper=="dagensnyheter")
expressen <- sum(df$newspaper=="expressen")
svenskadagbladet <- sum(df$newspaper=="svenskadagbladet")
svt <- sum(df$newspaper=="svt")
newspaper_frequency <- c(afton,dagen,expressen,svenskadagbladet,svt)
newspaper <- c("aftonbladet",
               "dagensnyheter",
               "expressen",
               "svenskadagbladet",
               "svt")
#creating df for graph
df_newspaper <- data.frame(Column1= newspaper, Column2 = newspaper_frequency)
colnames(df_newspaper)<- c("newspaper", "newspaper_frequency")
df_newspaper$newspaper <- factor(df_newspaper$newspaper, levels = df_newspaper$newspaper[order(-df_newspaper$newspaper_frequency)])

#graph
ggplot(df_newspaper, aes(x= newspaper,
                         y= newspaper_frequency,
                         fill=newspaper))+
  geom_bar(stat = "identity")+
  labs(title= "Frequency of Each Online News Site",
       x="",
       y="")+
  geom_text(aes(label = comma(newspaper_frequency)),
            vjust = -0.5,
            size = 3)+
  scale_x_discrete(labels=c("Aftonbladet",
                            "Svenska Dagbladet",
                            "Expressen",
                            "SVT Nyheter",
                            "Dagens Nyheter"))+
  scale_fill_manual(values= c("aftonbladet"= rgb(104/255, 89/255, 108/255, 1),
                              "expressen" = rgb(136/255, 115/255, 133/255, 0.7),                      
                              "dagensnyheter" = rgb(255/255, 150/255, 57/255, alpha = 0.9),
                              "svenskadagbladet" = rgb(255/255, 150/255, 57/255, alpha = 0.6),
                              "svt" = "#95ed94"))+
  scale_y_continuous(limits = c(0,2600),
                     breaks = c(500, 1000, 1500, 2000, 2500))+
  theme_minimal()+
  theme(legend.position ="none")
ggsave("figures/03_frequency_online_news.png", width = 7, height = 4)


##############----Elections comparison articles
#2018 
election_2018 <- sum(df$electionyear == 2018)

election_2018_per <- election_2018/7794
#2022
election_2022 <- sum(df$electionyear == 2022)
election_2022_per <- election_2022/7794

#dataframe 
election <- c("election_2018", "election_2022")
#percentages
percentages_election <- c(election_2018_per,election_2022_per)
#absolute nr. 
absolute_election <- c(election_2018,election_2022)
#dataframe 
df_election<- data.frame(Column1 = election, Column2= absolute_election, Column3=percentages_election)
colnames(df_election) <- c("election","absolute_election", "percentages_election")
df_election$parties <- factor(df_election$election, levels = df_election$election[order(-df_election$absolute_election)])

#barplot
ggplot(df_election, aes(x = election, y = absolute_election, fill= election)) +
  geom_bar(stat = "identity") +
  labs(title = "Articles For Each Electoral Context",
       subtitle = "n=7794",
       x = "Election",
       y = "") +
  geom_text(aes(label = paste0(comma(absolute_election),
                               " (",
                               round(percentages_election, 2) * 100,
                               "%)")),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("election_2018"= "#bbbbbb",
                               "election_2022"="#7c7c7c"))+
  scale_y_continuous(limits = c(0, 5000)) +
  scale_x_discrete(labels = c("2018", "2022")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/04_articles_elections.png", width = 6, height = 3)


##############----Mentioning of each party
#sd
sd <- sum(df$sd)
sd_per <- sd/7794
#s
s <- sum(df$s)
s_per <- s/7794
#moderate
m <- sum(df$m)
m_per <- m/7794

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_party <- c(sd_per,s_per, m_per )
#absolute nr. 
absolute_party <- c(sd,s,m)
#dataframe 
df_party<- data.frame(Column1 = parties, Column2= absolute_party, Column3=percentages_party)
colnames(df_party) <- c("parties","absolute_party", "percentages_party")
df_party$parties <- factor(df_party$parties, levels = df_party$parties[order(-df_party$absolute_party)])


#barchart absolute numbers 
ggplot(df_party, aes (x= parties, y=absolute_party, fill=parties))+
  geom_bar(stat ="identity")+
  labs (title= "Mentioning of Each Party",
        subtitle = "Relative and Absolute Numbers",
        x= "Party",
        y="")+
  geom_text(aes(label = paste0(comma(absolute_party), " (", round(percentages_party * 100, 2), "%)"),
                vjust = -0.5),
            size = 3)+
  scale_fill_manual(values= c("s"="#c6c6c6",
                              "sd"="#00c5cf",
                              "m"="#5e5e5e"))+
  scale_x_discrete(labels= c("Social Democratic Party",
                             "SD Party", 
                             "Moderate Party"))+
  scale_y_continuous(limits = c(0,3500))+
  theme_minimal()+
  theme(legend.position="none")

ggsave("figures/05_mentioning_each_party.png", width = 6, height = 3)


##############----Share of Party Coverage

#general
#barchart percentages 
ggplot(df_party, aes (x= parties, y=percentages_party, fill=parties))+
  geom_bar(stat ="identity")+
  labs (title= "Share of Party Coverage",
        x= "Party",
        y="")+
  geom_text(aes(label = paste0(round(percentages_party,2)*100, "%")),
            vjust = -0.5,
            size = 4,
            fontface = "bold")+
  scale_fill_manual(values= c("s"="#c6c6c6",
                              "sd"="#00c5cf",
                              "m"="#5e5e5e"))+
  scale_x_discrete(labels= c("Social Democratic Party",
                             "SD Party", 
                             "Moderate Party"))+
  scale_y_continuous(limits = c(0,0.5),
                     labels = scales ::percent_format(),)+
  theme_minimal()+
  theme(legend.position="none")
ggsave("figures/06_share_partycoverage.png", width = 6, height = 3.7)

#across elections
#2018
df_election_2018 <- df %>% 
  filter(electionyear==2018)

#sd
sd_2018 <- sum(df_election_2018$sd)
sd_2018_per <- sd_2018/4852
#s
s_2018 <- sum(df_election_2018$s)
s_2018_per <- s_2018/4852
#moderate
m_2018 <- sum(df_election_2018$m)
m_2018_per <- m_2018/4852

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_party_2018 <- c(sd_2018_per,s_2018_per, m_2018_per )
#absolute nr. 
absolute_party_2018 <- c(sd_2018,s_2018,m_2018)
#dataframe 
df_party_2018<- data.frame(Column1 = parties, Column2= absolute_party_2018, Column3=percentages_party_2018)
colnames(df_party_2018) <- c("parties","absolute_party", "percentages_party")
df_party_2018$parties <- factor(df_party_2018$parties,
                                levels = df_party_2018$parties[order(-df_party_2018$absolute_party)])

df_party_2018 <- df_party_2018 %>% 
  mutate(electionyear =2018)

#2022
df_election_2022 <- df %>% 
  filter(electionyear==2022)
#sd
sd_2022 <- sum(df_election_2022$sd)
sd_2022_per <- sd_2022/2942
#s
s_2022 <- sum(df_election_2022$s)
s_2022_per <- s_2022/2942
#moderate
m_2022 <- sum(df_election_2022$m)
m_2022_per <- m_2022/2942

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_party_2022 <- c(sd_2022_per,s_2022_per, m_2022_per )
#absolute nr. 
absolute_party_2022 <- c(sd_2022,s_2022,m_2022)
#dataframe 
df_party_2022<- data.frame(Column1 = parties,
                           Column2= absolute_party_2022,
                           Column3=percentages_party_2022)
colnames(df_party_2022) <- c("parties","absolute_party", "percentages_party")
df_party_2022$parties <- factor(df_party_2022$parties,
                                levels = df_party_2022$parties[order(-df_party_2022$absolute_party)])
df_party_2022 <- df_party_2022 %>% 
  mutate(electionyear =2022)
df_party_election <- rbind(df_party_2018, df_party_2022)
df_party_election <- df_party_election %>% 
  mutate(electionyear=as.factor(electionyear))

#barchart
ggplot(df_party_election, aes (x= electionyear, y=percentages_party, fill=parties))+
  geom_bar(stat ="identity",
           position=position_dodge())+
  labs (title= " Share of Party Coverage for Each Electoral Context",
        x= "Election Year",
        y="",
        fill= "")+
  geom_text(aes(label = paste0(round(percentages_party,2)*100, "%")),
            vjust = -0.5,
            size = 4,
            fontface = "bold",
            position = position_dodge(width = 0.9))+
  scale_fill_manual(values= c("s"="#c6c6c6",
                              "sd"="#00c5cf",
                              "m"="#5e5e5e"),
                    labels= c("Social Democratic Party",
                              "SD Party",
                              "Moderate Party"))+
  scale_x_discrete(labels= c("2018",
                             "2022"))+
  scale_y_continuous(labels = scales ::percent_format(),
                     limits = c(0,0.5))+
  theme_minimal()+
  theme(legend.position="bottom")
ggsave("figures/07_share_partycoverage_elections.png", width = 6, height = 3.7)

#across mediatypes
#State Finansed
#svt
df_mediatype_svt <- df %>% 
  filter(mediatype=="state_finansed")
#sd
sd_svt <- sum(df_mediatype_svt$sd)
sd_svt_per <- sd_svt/1680
#s
s_svt <- sum(df_mediatype_svt$s)
s_svt_per <- s_svt/1680
#moderate
m_svt <- sum(df_mediatype_svt$m)
m_svt_per <- m_svt/1680

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_party_svt <- c(sd_svt_per,s_svt_per, m_svt_per )
#absolute nr. 
absolute_party_svt <- c(sd_svt,s_svt,m_svt)
#dataframe 
df_mediatype_svt<- data.frame(Column1 = parties, Column2= absolute_party_svt, Column3=percentages_party_svt)
colnames(df_mediatype_svt) <- c("parties",
                                "absolute_party",
                                "percentages_party")

df_mediatype_svt <- df_mediatype_svt %>% 
  mutate(mediatype ="state_finansed")

#Qualitative
df_mediatype_qualitative <- df %>% 
  filter(mediatype=="qualitative")
#sd
sd_qualitative <- sum(df_mediatype_qualitative$sd)
sd_qualitative_per <- sd_qualitative/1916
#s
s_qualitative <- sum(df_mediatype_qualitative$s)
s_qualitative_per <- s_qualitative/1916
#moderate
m_qualitative <- sum(df_mediatype_qualitative$m)
m_qualitative_per <- m_qualitative/1916

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_party_qualitative <- c(sd_qualitative_per,s_qualitative_per, m_qualitative_per )
#absolute nr. 
absolute_party_qualitative <- c(sd_qualitative,s_qualitative,m_qualitative)
#dataframe 
df_mediatype_qualitative<- data.frame(Column1 = parties,
                                      Column2= absolute_party_qualitative,
                                      Column3=percentages_party_qualitative)
colnames(df_mediatype_qualitative) <- c("parties","absolute_party", "percentages_party")

df_mediatype_qualitative <- df_mediatype_qualitative %>% 
  mutate(mediatype ="qualitative")

#Tabloid
df_mediatype_tabloid <- df %>% 
  filter(mediatype=="tabloid")
#sd
sd_tabloid <- sum(df_mediatype_tabloid$sd)
sd_tabloid_per <- sd_tabloid/4198
#s
s_tabloid <- sum(df_mediatype_tabloid$s)
s_tabloid_per <- s_tabloid/4198
#moderate
m_tabloid <- sum(df_mediatype_tabloid$m)
m_tabloid_per <- m_tabloid/4198

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_party_tabloid <- c(sd_tabloid_per,s_tabloid_per, m_tabloid_per )
#absolute nr. 
absolute_party_tabloid <- c(sd_tabloid,s_tabloid,m_tabloid)
#dataframe 
df_mediatype_tabloid<- data.frame(Column1 = parties,
                                  Column2= absolute_party_tabloid,
                                  Column3=percentages_party_tabloid)
colnames(df_mediatype_tabloid) <- c("parties","absolute_party", "percentages_party")

df_mediatype_tabloid <- df_mediatype_tabloid %>% 
  mutate(mediatype ="tabloid")

#fuse mediatypes
df_party_mediatype = rbind(df_mediatype_svt, df_mediatype_qualitative, df_mediatype_tabloid)
#relevel so that SD in the middle 
df_party_mediatype$parties <- as.factor(df_party_mediatype$parties)
df_party_mediatype$parties <- factor(df_party_mediatype$parties, levels = c("s", "sd", "m"))
df_party_mediatype$mediatype <- factor(df_party_mediatype$mediatype, levels = c("state_finansed",
                                                                                "qualitative",
                                                                                "tabloid"))


#barchart
ggplot(df_party_mediatype, aes (x= mediatype, y=percentages_party, fill=parties))+
  geom_bar(stat ="identity",
           position=position_dodge())+
  labs (title= " Share of Party Coverage for Each Media Type",
        x= "Media Type",
        y="",
        fill= "")+
  geom_text(aes(label = paste0(round(percentages_party,2)*100, "%")),
            vjust = -0.5,
            size = 4,
            fontface = "bold",
            position = position_dodge(width = 0.9))+
  scale_y_continuous(labels = scales ::percent_format(),
                     limits=c(0, 0.45))+
  scale_fill_manual(values= c("s"="#c6c6c6",
                              "sd"="#00c5cf",
                              "m"="#5e5e5e"),
                    labels= c("Social Democratic Party",
                              "SD Party",
                              "Moderate Party"))+
  scale_x_discrete(labels= c("State-Financed",
                             "Broadsheet",
                             "Tabloid"))+
  theme_minimal()+
  theme(legend.position="bottom")
ggsave("figures/08_share_partycoverage_mediatypes.png", width = 6, height = 3.7)

