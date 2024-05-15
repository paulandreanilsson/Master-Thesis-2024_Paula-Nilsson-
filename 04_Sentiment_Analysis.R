

#This is the code for the analysis related with the Sentiments in Chapter 7.4

#load libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)#to change format
library(nnet) #for multinominal regression
library(stargazer)#for tables
library(reshape2)#for melting 
library(gridExtra)#ggplots next to each other 
library(effects)  #for multinominal predicted prob
library(scales)#for scale

df <- read_excel("04-21-24_df_headings_sen.xlsx")

###################################################SENTIMENT
#RESEARCH QUESTION 1 

#------------data preparation 

#count frequencies negative, positive, neutral
sentiment_counts_sd <- df%>%count(sd_sentiment) %>% na.omit()
sentiment_counts_s <- df %>% count(s_sentiment)%>% na.omit()
sentiment_counts_m <- df %>% count(m_sentiment)%>% na.omit()

#change variable name 
#sd
sentiment_counts_sd <- sentiment_counts_sd %>% 
  rename(sd_sentiment_count = n,
         sentiment= sd_sentiment)
#s
sentiment_counts_s <- sentiment_counts_s %>% 
  rename(s_sentiment_count = n,
         sentiment= s_sentiment)
#m
sentiment_counts_m <- sentiment_counts_m %>% 
  rename(m_sentiment_count = n,
         sentiment= m_sentiment)
#merging to a df according to sentiment for each party
table_rq1 <- merge(merge(sentiment_counts_sd, sentiment_counts_s, by = "sentiment"), sentiment_counts_m, by = "sentiment")

#calculate percentages for sentiemnt for each party
table_rq1_percentage <- table_rq1 %>%
  mutate(sd_sentiment_percentage = as.numeric(sd_sentiment_count) / sum(as.numeric(sd_sentiment_count)),
         s_sentiment_percentage = as.numeric(s_sentiment_count) / sum(as.numeric(s_sentiment_count)),
         m_sentiment_percentage = as.numeric(m_sentiment_count) / sum(as.numeric(m_sentiment_count)))

table_rq1_percentage <- subset(table_rq1_percentage, select = -c(sd_sentiment_count, s_sentiment_count, m_sentiment_count))

#change into long format for graph
table_rq1_percentage_long <- table_rq1_percentage %>% 
  pivot_longer(cols=c(sd_sentiment_percentage,
                      s_sentiment_percentage,
                      m_sentiment_percentage),
               names_to ="party",
               values_to = "percentage")

table_rq1_absolute_long <- table_rq1 %>% 
  pivot_longer(cols=c(sd_sentiment_count,
                      s_sentiment_count,
                      m_sentiment_count),
               names_to ="party",
               values_to = "absolute")


#------------Visualisation
table_rq1_percentage_long$party <- factor(table_rq1_percentage_long$party, levels = c("s_sentiment_percentage",
                                                                                      "sd_sentiment_percentage",
                                                                                      "m_sentiment_percentage"))


#barplot sentiments across parties 
ggplot(table_rq1_percentage_long, aes(y=percentage, x=party, fill =sentiment))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Party",
        y= "Percentage",
        title="Distribution of Sentiments",
        fill ="")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("negative"="#fb6945", "neutral"="#c6c6c6", "positive"="#78c496"),
                    labels = c("Negative", "Neutral", "Positive"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.7))+
  scale_x_discrete (labels=c("Social Democratic Party",
                             "SD Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme (legend.position = "bottom")
ggsave("figures/27_sentiment.png", width = 6, height = 4)


#compare relatived differences per sentiment grouped by party
#neutral 
df_percentage_neutral <- table_rq1_percentage_long %>% 
  filter(sentiment== "neutral")
s_neutral_dif <- df_percentage_neutral$percentage[df_percentage_neutral$party == "s_sentiment_percentage"] - df_percentage_neutral$percentage[df_percentage_neutral$party == "sd_sentiment_percentage"]
m_neutral_dif <- df_percentage_neutral$percentage[df_percentage_neutral$party == "m_sentiment_percentage"] - df_percentage_neutral$percentage[df_percentage_neutral$party == "sd_sentiment_percentage"]
df_neutral_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_neutral_dif, m_neutral_dif),
  sentiment= "neutral"
)
#positive
df_percentage_positive <- table_rq1_percentage_long %>% 
  filter(sentiment== "positive")
s_positive_dif <- df_percentage_positive$percentage[df_percentage_positive$party == "s_sentiment_percentage"] - df_percentage_positive$percentage[df_percentage_positive$party == "sd_sentiment_percentage"]
m_positive_dif <- df_percentage_positive$percentage[df_percentage_positive$party == "m_sentiment_percentage"] - df_percentage_positive$percentage[df_percentage_positive$party == "sd_sentiment_percentage"]
df_positive_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_positive_dif, m_positive_dif),
  sentiment= "positive"
)
#negative
df_percentage_negative <- table_rq1_percentage_long %>% 
  filter(sentiment== "negative")
s_negative_dif <- df_percentage_negative$percentage[df_percentage_negative$party == "s_sentiment_percentage"] - df_percentage_negative$percentage[df_percentage_negative$party == "sd_sentiment_percentage"]
m_negative_dif <- df_percentage_negative$percentage[df_percentage_negative$party == "m_sentiment_percentage"] - df_percentage_negative$percentage[df_percentage_negative$party == "sd_sentiment_percentage"]
df_negative_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_negative_dif, m_negative_dif),
  sentiment= "negative"
)
df_sentiments_dif<- rbind(df_neutral_dif,df_positive_dif, df_negative_dif)
#relevel
df_sentiments_dif$parties <- factor(df_sentiments_dif$parties, levels = c("s", "m"))
df_sentiments_dif$sentiment <- factor(df_sentiments_dif$sentiment, levels = c("neutral", "positive", "negative"))

#Visualising 
ggplot(df_sentiments_dif, aes(x=sentiment, y=difference, fill=parties))+
  geom_bar(stat = "identity", position= position_dodge())+
  labs (x= "Sentiment",
        y= "Percentage",
        title="Sentiments",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = ifelse(difference > 0, paste0("+", round(difference, 2) * 100, "%"), 
                               paste0(round(difference, 2) * 100, "%"))),
            vjust = 1.3,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.15, 0.15))+
  scale_x_discrete (labels=c("Neutral",
                             "Positive",
                             "Negative"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6"),
                    labels=c("Social Democratic Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color = "black", linewidth = 1.5)

ggsave("figures/28_sentiment_diff.png", width = 6, height = 3.5)


#------------Multinomial Logistic Regression
#Data Preparation
#select relevant variables
df_rq1_reg <- df %>% 
  select(m_sentiment, s_sentiment, sd_sentiment, electionyear, mediatype, newspaper, english_headline, ID, s, sd, m)
#change to long format
df_rq1_reg<-df_rq1_reg %>% 
  pivot_longer(cols=c(m_sentiment,
                      sd_sentiment,
                      s_sentiment),
               names_to ="party",
               values_to = "sentiment")
#drop when sentiment column has no value
df_rq1_reg <- df_rq1_reg[complete.cases(df_rq1_reg$sentiment), ]
#as factor
df_rq1_reg <- df_rq1_reg %>% 
  mutate(sentiment=as.factor(sentiment),
         party=as.factor(party),
         electionyear=as.factor(electionyear))
#define reference category 
df_rq1_reg$party <- relevel(df_rq1_reg$party, ref = "sd_sentiment")
df_rq1_reg$sentiment <- relevel(df_rq1_reg$sentiment, ref = "neutral")

#Applying model 
model_rq1 <- multinom(sentiment ~ party, data = df_rq1_reg)
summary(model_rq1)


#Table with coefficents
stargazer(model_rq1,
          type="html",
          out="multi1.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("<b>Negative", "<b>Positive"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Multinominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001))
browseURL("multi1.htm")

#two tailed signifiance test
z<- summary(model_rq1)$coefficients/summary(model_rq1)$standard.errors
p <- (1-pnorm(abs(z), 0,1))*2
p


#predcited propabilities
fit_effect <- Effect("party", model_rq1)

#into data framr for predicted propabilities
effect_df <- as.data.frame(cbind(fit_effect$x, fit_effect$prob, fit_effect$lower.prob, fit_effect$upper.prob))
#label the values
names(effect_df) <- c("party",
                      "prob_neutral",
                      "prob_negative",
                      "prob_positive",
                      "prob_neutral_low",
                      "prob_negative_low",
                      "prob_positive_low",
                      "prob_neutral_upper",
                      "prob_negative_upper",
                      "prob_positive_upper")

#Positive Sentiments df
df_positive <- data.frame(
  party = effect_df$party,
  propability = effect_df$prob_positive,
  lower = effect_df$prob_positive_low,
  upper = effect_df$prob_positive_upper
)

#plot of predcited propbilities for positive sentiments 
ggplot(df_positive, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Positive Sentiments",
       subtitle = "With 95% Confidence Interval") +
  scale_x_discrete(labels = c("SD Party", "Moderate\nParty", "Social Democratic\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0.05, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf", "m_sentiment" = "#5e5e5e", "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/29_positive_pp.png", width = 3.5, height = 4)

#Negative Sentiments df
df_negative <- data.frame(
  party = effect_df$party,
  propability = effect_df$prob_negative,
  lower = effect_df$prob_negative_low,
  upper = effect_df$prob_negative_upper
)

#plot of predcited propbilities for negative sentiments 
ggplot(df_negative, aes(x = party,
                                     y = propability,
                                     color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "",
       title = "Negative Sentiments",
       subtitle = "With 95% Confidence Interval") +
  scale_x_discrete(labels = c("SD Party", "Moderate\nParty", "Social Democratic\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0.05, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf", "m_sentiment" = "#5e5e5e", "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/30_negative_pp.png", width = 3.5, height = 4)

#p_po_ne <- grid.arrange(p_positive,p_negative, ncol = 2)


#----------------------------------------------------------RESEARCH QUESTION 2
#create datasets

#election year 2018
df_2018 <- df %>% 
  filter(df$electionyear==2018)
table(df_2018$sd)
table(df_2018$s)
table(df_2018$m)

#election year 2022
df_2022 <- df %>% 
  filter(df$electionyear==2022)
table(df_2022$sd)
table(df_2022$s)
table(df_2022$m)

#--------------------------------DATA PREPARATION 
#-----2018
#count frequencies negative, positive, neutral
sentiment_counts_sd_2018 <- df_2018%>%count(sd_sentiment) %>% na.omit()
sentiment_counts_s_2018 <- df_2018 %>% count(s_sentiment)%>% na.omit()
sentiment_counts_m_2018 <- df_2018 %>% count(m_sentiment)%>% na.omit()

#change variable name 
#sd
sentiment_counts_sd_2018 <- sentiment_counts_sd_2018 %>% 
  rename(sd_sentiment_count = n,
         sentiment= sd_sentiment)
#s
sentiment_counts_s_2018 <- sentiment_counts_s_2018 %>% 
  rename(s_sentiment_count = n,
         sentiment= s_sentiment)
#m
sentiment_counts_m_2018 <- sentiment_counts_m_2018 %>% 
  rename(m_sentiment_count = n,
         sentiment= m_sentiment)
#merging
table_rq2_2018 <- merge(merge(sentiment_counts_sd_2018, sentiment_counts_s_2018, by = "sentiment"), sentiment_counts_m_2018, by = "sentiment")
#change rownames according to sentiment
rownames(table_rq2_2018)<-table_rq2_2018$sentiment

#calculate percentages
table_rq2_percentage_2018 <- table_rq2_2018 %>%
  mutate(sd_sentiment_percentage = as.numeric(sd_sentiment_count) / sum(as.numeric(sd_sentiment_count)),
         s_sentiment_percentage = as.numeric(s_sentiment_count) / sum(as.numeric(s_sentiment_count)),
         m_sentiment_percentage = as.numeric(m_sentiment_count) / sum(as.numeric(m_sentiment_count)))
#create percentages table
table_rq2_percentage_2018 <- subset(table_rq2_percentage_2018, select = -c(sd_sentiment_count, s_sentiment_count, m_sentiment_count))

#change into long format for graph
table_rq2_percentage_2018_long <- table_rq2_percentage_2018 %>% 
  pivot_longer(cols=c(sd_sentiment_percentage,
                      s_sentiment_percentage,
                      m_sentiment_percentage),
               names_to ="party",
               values_to = "percentage")
#absolute
table_rq2_absolute_2018_long <- table_rq2_2018 %>% 
  pivot_longer(cols=c(sd_sentiment_count,
                      s_sentiment_count,
                      m_sentiment_count),
               names_to ="party",
               values_to = "absolute")

#--------------------------------2022
#count frequencies negative, positive, neutral
sentiment_counts_sd_2022 <- df_2022%>%count(sd_sentiment) %>% na.omit()
sentiment_counts_s_2022 <- df_2022 %>% count(s_sentiment)%>% na.omit()
sentiment_counts_m_2022 <- df_2022 %>% count(m_sentiment)%>% na.omit()
#change variable name 
#sd
sentiment_counts_sd_2022 <- sentiment_counts_sd_2022 %>% 
  rename(sd_sentiment_count = n,
         sentiment= sd_sentiment)
#s
sentiment_counts_s_2022 <- sentiment_counts_s_2022 %>% 
  rename(s_sentiment_count = n,
         sentiment= s_sentiment)
#m
sentiment_counts_m_2022 <- sentiment_counts_m_2022 %>% 
  rename(m_sentiment_count = n,
         sentiment= m_sentiment)
#merging
table_rq2_2022 <- merge(merge(sentiment_counts_sd_2022, sentiment_counts_s_2022, by = "sentiment"), sentiment_counts_m_2022, by = "sentiment")
rownames(table_rq2_2022) <-table_rq2_2022$sentiment

#calculate percentages
table_rq2_percentage_2022 <- table_rq2_2022 %>%
  mutate(sd_sentiment_percentage = as.numeric(sd_sentiment_count) / sum(as.numeric(sd_sentiment_count)),
         s_sentiment_percentage = as.numeric(s_sentiment_count) / sum(as.numeric(s_sentiment_count)),
         m_sentiment_percentage = as.numeric(m_sentiment_count) / sum(as.numeric(m_sentiment_count)))
#create percentages table
table_rq2_percentage_2022 <- subset(table_rq2_percentage_2022, select = -c(sd_sentiment_count, s_sentiment_count, m_sentiment_count))

#change into long format for graph
#percentages
table_rq2_percentage_2022_long <- table_rq2_percentage_2022 %>% 
  pivot_longer(cols=c(sd_sentiment_percentage,
                      s_sentiment_percentage,
                      m_sentiment_percentage),
               names_to ="party",
               values_to = "percentage")
#absolute
table_rq2_absolute_2022_long <- table_rq2_2022 %>% 
  pivot_longer(cols=c(sd_sentiment_count,
                      s_sentiment_count,
                      m_sentiment_count),
               names_to ="party",
               values_to = "absolute")

#------------------------------VISUALISATION

#----------2018
#barplot sentiment
ggplot(table_rq2_percentage_2018_long, aes(y=percentage, x=party, fill =sentiment))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Party",
        y= "Percentage",
        title="Election Year 2018",
        subtitle= "Distribution of Sentiments",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("negative"="#fb6945", "neutral"="#c6c6c6", "positive"="#78c496"),
                    labels= c("Negative", "Neutral", "Positive"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.7))+
  scale_x_discrete (labels=c("Moderate Party", "Social Democratic Party", "SD Party"))+
  theme_minimal()+
  theme (legend.position = "right")


ggsave("figures/33_sentiment_2018.png", width = 6, height = 4)

#---------2022
#barplot sentiment
ggplot(table_rq2_percentage_2022_long, aes(y=percentage, x=party, fill =sentiment))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Party",
        y= "",
        title="Election Year 2022",
        subtitle = "Distribution of Sentiments",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("negative"="#fb6945", "neutral"="#c6c6c6", "positive"="#78c496"),
                    labels = c("Negative", "Neutral", "Positive"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.7))+
  scale_x_discrete (labels=c("Moderate Party", "Social Democratic Party", "SD Party"))+
  theme_minimal()+
  theme(legend.position = "right")


ggsave("figures/34_sentiment_2022.png", width = 6, height = 4)



#grid.arrange(p_2018, p_2022, ncol = 2)


#
#compare relatived differences per sentiment grouped by party
#--- 2018 #compare relatived differences per sentiment grouped by party
#neutral 
df_percentage_neutral_2018<- table_rq2_percentage_2018_long %>% 
  filter(sentiment== "neutral")
s_neutral_dif_2018 <- df_percentage_neutral_2018$percentage[df_percentage_neutral_2018$party == "s_sentiment_percentage"] - df_percentage_neutral_2018$percentage[df_percentage_neutral_2018$party == "sd_sentiment_percentage"]
m_neutral_dif_2018 <- df_percentage_neutral_2018$percentage[df_percentage_neutral_2018$party == "m_sentiment_percentage"] - df_percentage_neutral_2018$percentage[df_percentage_neutral_2018$party == "sd_sentiment_percentage"]
df_neutral_dif_2018 <- data.frame(
  parties = c("s", "m"),
  difference = c(s_neutral_dif_2018, m_neutral_dif_2018),
  sentiment= "neutral"
)
#positive
df_percentage_positive_2018 <- table_rq2_percentage_2018_long %>% 
  filter(sentiment== "positive")
s_positive_dif_2018 <- df_percentage_positive_2018$percentage[df_percentage_positive_2018$party == "s_sentiment_percentage"] - df_percentage_positive_2018$percentage[df_percentage_positive_2018$party == "sd_sentiment_percentage"]
m_positive_dif_2018 <- df_percentage_positive_2018$percentage[df_percentage_positive_2018$party == "m_sentiment_percentage"] - df_percentage_positive_2018$percentage[df_percentage_positive_2018$party == "sd_sentiment_percentage"]
df_positive_dif_2018 <- data.frame(
  parties = c("s", "m"),
  difference = c(s_positive_dif_2018, m_positive_dif_2018),
  sentiment= "positive"
)
#negative
df_percentage_negative_2018 <- table_rq2_percentage_2018_long %>% 
  filter(sentiment== "negative")
s_negative_dif_2018 <- df_percentage_negative_2018$percentage[df_percentage_negative_2018$party == "s_sentiment_percentage"] - df_percentage_negative_2018$percentage[df_percentage_negative_2018$party == "sd_sentiment_percentage"]
m_negative_dif_2018 <- df_percentage_negative_2018$percentage[df_percentage_negative_2018$party == "m_sentiment_percentage"] - df_percentage_negative_2018$percentage[df_percentage_negative_2018$party == "sd_sentiment_percentage"]
df_negative_dif_2018 <- data.frame(
  parties = c("s", "m"),
  difference = c(s_negative_dif_2018, m_negative_dif_2018),
  sentiment= "negative"
)
df_sentiments_dif_2018<- rbind(df_neutral_dif_2018,df_positive_dif_2018, df_negative_dif_2018)
#relevel
df_sentiments_dif_2018$parties <- factor(df_sentiments_dif_2018$parties, levels = c("s", "m"))
df_sentiments_dif_2018$sentiment <- factor(df_sentiments_dif_2018$sentiment, levels = c("neutral", "positive", "negative"))

#Visualising 
ggplot(df_sentiments_dif_2018, aes(x=sentiment, y=difference, fill=parties))+
  geom_bar(stat = "identity", position= position_dodge())+
  labs (x= "Sentiment",
        y= "Percentage",
        title="Election 2018",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = ifelse(difference > 0, paste0("+", round(difference, 2) * 100, "%"), 
                               paste0(round(difference, 2) * 100, "%"))),
            vjust = 1,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.15, 0.15))+
  scale_x_discrete (labels=c("Neutral",
                             "Positive",
                             "Negative"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6"),
                    labels=c("Social Democratic Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color = "black", size = 1.5)

ggsave("figures/31_sentiment_2018.png", width = 4, height = 3)


#----2022
#neutral 
df_percentage_neutral_2022<- table_rq2_percentage_2022_long %>% 
  filter(sentiment== "neutral")
s_neutral_dif_2022 <- df_percentage_neutral_2022$percentage[df_percentage_neutral_2022$party == "s_sentiment_percentage"] - df_percentage_neutral_2022$percentage[df_percentage_neutral_2022$party == "sd_sentiment_percentage"]
m_neutral_dif_2022 <- df_percentage_neutral_2022$percentage[df_percentage_neutral_2022$party == "m_sentiment_percentage"] - df_percentage_neutral_2022$percentage[df_percentage_neutral_2022$party == "sd_sentiment_percentage"]
df_neutral_dif_2022 <- data.frame(
  parties = c("s", "m"),
  difference = c(s_neutral_dif_2022, m_neutral_dif_2022),
  sentiment= "neutral"
)
#positive
df_percentage_positive_2022 <- table_rq2_percentage_2022_long %>% 
  filter(sentiment== "positive")
s_positive_dif_2022 <- df_percentage_positive_2022$percentage[df_percentage_positive_2022$party == "s_sentiment_percentage"] - df_percentage_positive_2022$percentage[df_percentage_positive_2022$party == "sd_sentiment_percentage"]
m_positive_dif_2022 <- df_percentage_positive_2022$percentage[df_percentage_positive_2022$party == "m_sentiment_percentage"] - df_percentage_positive_2022$percentage[df_percentage_positive_2022$party == "sd_sentiment_percentage"]
df_positive_dif_2022 <- data.frame(
  parties = c("s", "m"),
  difference = c(s_positive_dif_2022, m_positive_dif_2022),
  sentiment= "positive"
)
#negative
df_percentage_negative_2022 <- table_rq2_percentage_2022_long %>% 
  filter(sentiment== "negative")
s_negative_dif_2022 <- df_percentage_negative_2022$percentage[df_percentage_negative_2022$party == "s_sentiment_percentage"] - df_percentage_negative_2022$percentage[df_percentage_negative_2022$party == "sd_sentiment_percentage"]
m_negative_dif_2022 <- df_percentage_negative_2022$percentage[df_percentage_negative_2022$party == "m_sentiment_percentage"] - df_percentage_negative_2022$percentage[df_percentage_negative_2022$party == "sd_sentiment_percentage"]
df_negative_dif_2022 <- data.frame(
  parties = c("s", "m"),
  difference = c(s_negative_dif_2022, m_negative_dif_2022),
  sentiment= "negative"
)
df_sentiments_dif_2022<- rbind(df_neutral_dif_2022,df_positive_dif_2022, df_negative_dif_2022)
#relevel
df_sentiments_dif_2022$parties <- factor(df_sentiments_dif_2022$parties, levels = c("s", "m"))
df_sentiments_dif_2022$sentiment <- factor(df_sentiments_dif_2022$sentiment, levels = c("neutral", "positive", "negative"))

#Visualising 
ggplot(df_sentiments_dif_2022, aes(x=sentiment, y=difference, fill=parties))+
  geom_bar(stat = "identity", position= position_dodge())+
  labs (x= "Sentiment",
        y= "Percentage",
        title="Election 2022",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = ifelse(difference > 0, paste0("+", round(difference, 2) * 100, "%"), 
                               paste0(round(difference, 2) * 100, "%"))),
            vjust = 1,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.15, 0.15))+
  scale_x_discrete (labels=c("Neutral",
                             "Positive",
                             "Negative"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6"),
                    labels=c("Social Democratic Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color = "black", size = 1.5)

ggsave("figures/32_sentiment_2022.png", width = 4, height = 3)


#------------Multinomial Logistic Regression

#-------2018
#Data Preparation
#select relevant variables
df_rq2_reg_2018 <- df_2018 %>% 
  select(m_sentiment, s_sentiment, sd_sentiment, electionyear, mediatype, newspaper, english_headline, ID, s, sd, m)
#change to long format
df_rq2_reg_2018<-df_rq2_reg_2018 %>% 
  pivot_longer(cols=c(m_sentiment,
                      sd_sentiment,
                      s_sentiment),
               names_to ="party",
               values_to = "sentiment")
#drop when sentiment column has no value
df_rq2_reg_2018 <- df_rq2_reg_2018[complete.cases(df_rq2_reg_2018$sentiment), ]
#as factor
df_rq2_reg_2018 <- df_rq2_reg_2018 %>% 
  mutate(sentiment=as.factor(sentiment),
         party=as.factor(party),
         electionyear=as.factor(electionyear))
#define reference category 
df_rq2_reg_2018$party <- relevel(df_rq2_reg_2018$party, ref = "sd_sentiment")
df_rq2_reg_2018$sentiment <- relevel(df_rq2_reg_2018$sentiment, ref = "neutral")
#apply model
model_rq2_2018 <- multinom(sentiment ~ party, data = df_rq2_reg_2018)
summary(model_rq2_2018)

#----------2022
#Data Preparation
#select relevant variables
df_rq2_reg_2022 <- df_2022 %>% 
  select(m_sentiment, s_sentiment, sd_sentiment, electionyear, mediatype, newspaper, english_headline, ID, s, sd, m)
#change to long format
df_rq2_reg_2022<-df_rq2_reg_2022 %>% 
  pivot_longer(cols=c(m_sentiment,
                      sd_sentiment,
                      s_sentiment),
               names_to ="party",
               values_to = "sentiment")
#drop when sentiment column has no value
df_rq2_reg_2022 <- df_rq2_reg_2022[complete.cases(df_rq2_reg_2022$sentiment), ]
#as factor
df_rq2_reg_2022 <- df_rq2_reg_2022 %>% 
  mutate(sentiment=as.factor(sentiment),
         party=as.factor(party),
         electionyear=as.factor(electionyear))
#define reference category 
df_rq2_reg_2022$party <- relevel(df_rq2_reg_2022$party, ref = "sd_sentiment")
df_rq2_reg_2022$sentiment <- relevel(df_rq2_reg_2022$sentiment, ref = "neutral")
#apply model
model_rq2_2022 <- multinom(sentiment ~ party, data = df_rq2_reg_2022)
summary(model_rq2_2022)



#TABLE MULTINOMINAL REGRESSION ANALYSIS
#Table with coefficents
stargazer(model_rq2_2018,model_rq2_2022,
          type="html",
          out="model_rq2.htm",
          intercept.bottom = FALSE,
          column.labels = c("<b>Election 2018</b><br>Negative", "<br>Positive",
                            "<b>Election 2022</b><br>Negative", "<br>Positive"),
          dep.var.labels = c("", "","", ""),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Multinominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_rq2.htm")

#2018
#two tailed signifiance test 
z_2018<- summary(model_rq2_2018)$coefficients/summary(model_rq2_2018)$standard.errors
p_2018 <- (1-pnorm(abs(z_2018), 0,1))*2
p_2018

#2022
#two tailed significance test 
z_2022<- summary(model_rq2_2022)$coefficients/summary(model_rq2_2022)$standard.errors
p_2022 <- (1-pnorm(abs(z_2022), 0,1))*2
p_2022


#predicted propabilities 
fit_effect_2018 <- Effect("party", model_rq2_2018)
#into data framr for predicted propabilities
effect_df_2018 <- as.data.frame(cbind(fit_effect_2018$x,
                                      fit_effect_2018$prob,
                                      fit_effect_2018$lower.prob,
                                      fit_effect_2018$upper.prob))
#label the values
names(effect_df_2018) <- c("party",
                           "prob_neutral",
                           "prob_negative",
                           "prob_positive",
                           "prob_neutral_low",
                           "prob_negative_low",
                           "prob_positive_low",
                           "prob_neutral_upper",
                           "prob_negative_upper",
                           "prob_positive_upper")

#Positive Sentiments df
df_positive_2018 <- data.frame(
  party = effect_df_2018$party,
  propability = effect_df_2018$prob_positive,
  lower = effect_df_2018$prob_positive_low,
  upper = effect_df_2018$prob_positive_upper)

#relevel
df_positive_2018$party <- factor(df_positive_2018$party, levels = c("s_sentiment","sd_sentiment", "m_sentiment"))

#plot of predcited propbilities for positive sentiments 
ggplot(df_positive_2018,
                          aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Election 2018",
       subtitle = "Positive Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.4,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf",
                                "m_sentiment" = "#5e5e5e",
                                "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/appendix1_pos_2018.png", width = 6, height = 3)


#negative Sentiments df
df_negative_2018 <- data.frame(
  party = effect_df_2018$party,
  propability = effect_df_2018$prob_negative,
  lower = effect_df_2018$prob_negative_low,
  upper = effect_df_2018$prob_negative_upper)

df_negative_2018$party <- factor(df_negative_2018$party, levels = c("s_sentiment","sd_sentiment", "m_sentiment"))

#plot of predcited propbilities for negative sentiments 
ggplot(df_negative_2018, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Election 2018",
       subtitle = "Negative Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.4,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf",
                                "m_sentiment" = "#5e5e5e",
                                "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/appendix2_neg_2018.png", width = 6, height = 3)


#predicted propabilities 
fit_effect_2022 <- Effect("party", model_rq2_2022)

#into data framr for predicted propabilities
effect_df_2022 <- as.data.frame(cbind(fit_effect_2022$x,
                                      fit_effect_2022$prob,
                                      fit_effect_2022$lower.prob,
                                      fit_effect_2022$upper.prob))
#label the values
names(effect_df_2022) <- c("party",
                           "prob_neutral",
                           "prob_negative",
                           "prob_positive",
                           "prob_neutral_low",
                           "prob_negative_low",
                           "prob_positive_low",
                           "prob_neutral_upper",
                           "prob_negative_upper",
                           "prob_positive_upper")

#Positive Sentiments df
df_positive_2022 <- data.frame(
  party = effect_df_2022$party,
  propability = effect_df_2022$prob_positive,
  lower = effect_df_2022$prob_positive_low,
  upper = effect_df_2022$prob_positive_upper)

#relevel
df_positive_2022$party <- factor(df_positive_2022$party, levels = c("s_sentiment","sd_sentiment", "m_sentiment"))
#plot of predcited propbilities for positive sentiments 
ggplot(df_positive_2022, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Election 2022",
       subtitle = "Positive Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.4,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf", "m_sentiment" = "#5e5e5e", "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/appendix3_pos_2022.png", width = 6, height = 3)


#negative Sentiments df
df_negative_2022 <- data.frame(
  party = effect_df_2022$party,
  propability = effect_df_2022$prob_negative,
  lower = effect_df_2022$prob_negative_low,
  upper = effect_df_2022$prob_negative_upper)


#relevel
df_negative_2022$party <- factor(df_negative_2022$party, levels = c("s_sentiment","sd_sentiment", "m_sentiment"))

#plot of predcited propbilities for negative sentiments 
ggplot(df_negative_2022, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Election 2022",
       subtitle = "Negative Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.4,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf", "m_sentiment" = "#5e5e5e", "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none") 
ggsave("figures/appendix4_neg_2022.png", width = 6, height = 3)


#-------------------------------------------------------------------------RESEARCH QUESTION 3 Mediatypes
#--------------------Datasets

#df for state-financed
df_svt <- df %>% 
  filter(mediatype=="state_finansed")
#df for qualitative
df_qualitative <- df %>% 
  filter(mediatype=="qualitative")
#df for tabloid
df_tabloid <- df %>% 
  filter(mediatype=="tabloid")

#--------------------DATA PREPARATION 

#------------svt
#count frequencies negative, positive, neutral
sentiment_counts_sd_svt <- df_svt%>%count(sd_sentiment) %>% na.omit()
sentiment_counts_s_svt <- df_svt %>% count(s_sentiment)%>% na.omit()
sentiment_counts_m_svt <- df_svt %>% count(m_sentiment)%>% na.omit()
#change variable name 
#sd
sentiment_counts_sd_svt <- sentiment_counts_sd_svt %>% 
  rename(sd_sentiment_count = n,
         sentiment= sd_sentiment)
#s
sentiment_counts_s_svt <- sentiment_counts_s_svt %>% 
  rename(s_sentiment_count = n,
         sentiment= s_sentiment)

#m
sentiment_counts_m_svt <- sentiment_counts_m_svt %>% 
  rename(m_sentiment_count = n,
         sentiment= m_sentiment)
#merging datasets
table_rq3_svt <- merge(merge(sentiment_counts_sd_svt, sentiment_counts_s_svt, by = "sentiment"), sentiment_counts_m_svt, by = "sentiment")
#change rownames according to sentiment
rownames(table_rq3_svt)<-table_rq3_svt$sentiment

#calculate percentages
table_rq3_percentage_svt <- table_rq3_svt %>%
  mutate(sd_sentiment_percentage = as.numeric(sd_sentiment_count) / sum(as.numeric(sd_sentiment_count)),
         s_sentiment_percentage = as.numeric(s_sentiment_count) / sum(as.numeric(s_sentiment_count)),
         m_sentiment_percentage = as.numeric(m_sentiment_count) / sum(as.numeric(m_sentiment_count)))
#percentage df
table_rq3_percentage_svt <- subset(table_rq3_percentage_svt, select = -c(sd_sentiment_count, s_sentiment_count, m_sentiment_count))

#change into long format for graph
#percentages
table_rq3_percentage_svt_long <- table_rq3_percentage_svt %>% 
  pivot_longer(cols=c(sd_sentiment_percentage,
                      s_sentiment_percentage,
                      m_sentiment_percentage),
               names_to ="party",
               values_to = "percentage")
#absolute numbers
table_rq3_absolute_svt_long <- table_rq3_svt %>% 
  pivot_longer(cols=c(sd_sentiment_count,
                      s_sentiment_count,
                      m_sentiment_count),
               names_to ="party",
               values_to = "absolute")

#------------tabloid
#count frequencies negative, positive, neutral
sentiment_counts_sd_tabloid <- df_tabloid%>%count(sd_sentiment) %>% na.omit()
sentiment_counts_s_tabloid <- df_tabloid %>% count(s_sentiment)%>% na.omit()
sentiment_counts_m_tabloid <- df_tabloid %>% count(m_sentiment)%>% na.omit()
#change variable name 
#sd
sentiment_counts_sd_tabloid <- sentiment_counts_sd_tabloid %>% 
  rename(sd_sentiment_count = n,
         sentiment= sd_sentiment)
#s
sentiment_counts_s_tabloid <- sentiment_counts_s_tabloid %>% 
  rename(s_sentiment_count = n,
         sentiment= s_sentiment)

#m
sentiment_counts_m_tabloid <- sentiment_counts_m_tabloid %>% 
  rename(m_sentiment_count = n,
         sentiment= m_sentiment)
#merging datasets
table_rq3_tabloid <- merge(merge(sentiment_counts_sd_tabloid, sentiment_counts_s_tabloid, by = "sentiment"), sentiment_counts_m_tabloid, by = "sentiment")
#change rownames according to sentiment
rownames(table_rq3_tabloid)<-table_rq3_tabloid$sentiment

#calculate percentages
table_rq3_percentage_tabloid <- table_rq3_tabloid %>%
  mutate(sd_sentiment_percentage = as.numeric(sd_sentiment_count) / sum(as.numeric(sd_sentiment_count)),
         s_sentiment_percentage = as.numeric(s_sentiment_count) / sum(as.numeric(s_sentiment_count)),
         m_sentiment_percentage = as.numeric(m_sentiment_count) / sum(as.numeric(m_sentiment_count)))
#percentage df
table_rq3_percentage_tabloid <- subset(table_rq3_percentage_tabloid, select = -c(sd_sentiment_count, s_sentiment_count, m_sentiment_count))

#change into long format for graph
#percentages
table_rq3_percentage_tabloid_long <- table_rq3_percentage_tabloid %>% 
  pivot_longer(cols=c(sd_sentiment_percentage,
                      s_sentiment_percentage,
                      m_sentiment_percentage),
               names_to ="party",
               values_to = "percentage")
#absolute numbers
table_rq3_absolute_tabloid_long <- table_rq3_tabloid %>% 
  pivot_longer(cols=c(sd_sentiment_count,
                      s_sentiment_count,
                      m_sentiment_count),
               names_to ="party",
               values_to = "absolute")

#------------qualitative
#count frequencies negative, positive, neutral
sentiment_counts_sd_qualitative <- df_qualitative%>%count(sd_sentiment) %>% na.omit()
sentiment_counts_s_qualitative <- df_qualitative %>% count(s_sentiment)%>% na.omit()
sentiment_counts_m_qualitative <- df_qualitative %>% count(m_sentiment)%>% na.omit()
#change variable name 
#sd
sentiment_counts_sd_qualitative <- sentiment_counts_sd_qualitative %>% 
  rename(sd_sentiment_count = n,
         sentiment= sd_sentiment)
#s
sentiment_counts_s_qualitative <- sentiment_counts_s_qualitative %>% 
  rename(s_sentiment_count = n,
         sentiment= s_sentiment)

#m
sentiment_counts_m_qualitative <- sentiment_counts_m_qualitative %>% 
  rename(m_sentiment_count = n,
         sentiment= m_sentiment)
#merging datasets
table_rq3_qualitative <- merge(merge(sentiment_counts_sd_qualitative, sentiment_counts_s_qualitative, by = "sentiment"), sentiment_counts_m_qualitative, by = "sentiment")
#change rownames according to sentiment
rownames(table_rq3_qualitative)<-table_rq3_qualitative$sentiment

#calculate percentages
table_rq3_percentage_qualitative <- table_rq3_qualitative %>%
  mutate(sd_sentiment_percentage = as.numeric(sd_sentiment_count) / sum(as.numeric(sd_sentiment_count)),
         s_sentiment_percentage = as.numeric(s_sentiment_count) / sum(as.numeric(s_sentiment_count)),
         m_sentiment_percentage = as.numeric(m_sentiment_count) / sum(as.numeric(m_sentiment_count)))
#percentage df
table_rq3_percentage_qualitative <- subset(table_rq3_percentage_qualitative, select = -c(sd_sentiment_count, s_sentiment_count, m_sentiment_count))

#change into long format for graph
#percentages
table_rq3_percentage_qualitative_long <- table_rq3_percentage_qualitative %>% 
  pivot_longer(cols=c(sd_sentiment_percentage,
                      s_sentiment_percentage,
                      m_sentiment_percentage),
               names_to ="party",
               values_to = "percentage")
#absolute numbers
table_rq3_absolute_qualitative_long <- table_rq3_qualitative %>% 
  pivot_longer(cols=c(sd_sentiment_count,
                      s_sentiment_count,
                      m_sentiment_count),
               names_to ="party",
               values_to = "absolute")

#relevel
table_rq3_percentage_svt_long$party <- factor(table_rq3_percentage_svt_long$party, levels = c("s_sentiment_percentage",
                                                                                              "sd_sentiment_percentage",
                                                                                              "m_sentiment_percentage"))
table_rq3_percentage_qualitative_long$party <- factor(table_rq3_percentage_qualitative_long$party, levels = c("s_sentiment_percentage",
                                                                                                              "sd_sentiment_percentage",
                                                                                                              "m_sentiment_percentage"))
table_rq3_percentage_tabloid_long$party <- factor(table_rq3_percentage_tabloid_long$party, levels = c("s_sentiment_percentage",
                                                                                                      "sd_sentiment_percentage",
                                                                                                      "m_sentiment_percentage"))
#--------------------VISUALISATION

#----------svt
#barplot sentiment
ggplot(table_rq3_percentage_svt_long, aes(y=percentage, x=party, fill =sentiment))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Sentiment",
        y= "Percentage",
        title="State-Financed News",
        subtitle= "Distribution of Sentiments",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("negative"="#fb6945",
                              "neutral"="#c6c6c6",
                              "positive"="#78c496"),
                    labels=c("Negative",
                             "Neutral",
                             "Positive"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.8))+
  scale_x_discrete (labels=c("Social Democratic\nParty",
                             "SD Party",
                             "Moderate\nParty"))+
  theme_minimal()+
  theme(legend.position = "right")
ggsave("figures/35sentiments_svt.png", width = 6, height = 3)

#----------qualitative
#barplot sentiment
ggplot(table_rq3_percentage_qualitative_long, aes(y=percentage, x=party, fill =sentiment))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Sentiment",
        y= "Percentage",
        subtitle="Distribution of Sentiments",
        title="Broadsheet News",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("negative"="#fb6945",
                              "neutral"="#c6c6c6",
                              "positive"="#78c496"),
                    labels= c("Negative", "Neutral", "Positive"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits=c(0,0.8))+
  scale_x_discrete (labels=c("Social Democratic\nParty",
                             "SD Party",
                             "Moderate\nParty"))+
  theme_minimal()+
  theme(legend.position = "right")
ggsave("figures/36_sentiments_qualitative.png", width = 6, height = 3)


#----------tabloid
#barplot sentiment
ggplot(table_rq3_percentage_tabloid_long, aes(y=percentage, x=party, fill =sentiment))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Sentiment",
        y= "Percentage",
        subtitle="Distribution of Sentiments",
        title="Tabloid News",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("negative"="#fb6945",
                              "neutral"="#c6c6c6",
                              "positive"="#78c496"),
                    labels= c("Negative", "Neutral", "Positive"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.8))+
  scale_x_discrete (labels=c("Social Democratic\nParty",
                             "SD Party",
                             "Moderate\nParty"))+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave("figures/37_sentiments_tabloid.png", width = 6, height = 3)




#visualise negative

##Barplots for negative sentiments in different mediatypes
#data preparation
#svt
df_media_negative_svt <- table_rq3_percentage_svt_long %>% 
  filter(sentiment == "negative")
df_media_negative_svt <- df_media_negative_svt %>% 
  mutate(mediatype = "svt")
view(df_media_negative_svt)

s_negative_svt_dif <- df_media_negative_svt$percentage[df_media_negative_svt$party == "s_sentiment_percentage"] - df_media_negative_svt$percentage[df_media_negative_svt$party == "sd_sentiment_percentage"]
m_negative_svt_dif <- df_media_negative_svt$percentage[df_media_negative_svt$party == "m_sentiment_percentage"] - df_media_negative_svt$percentage[df_media_negative_svt$party == "sd_sentiment_percentage"]
df_negative_svt_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_negative_svt_dif, 
                 m_negative_svt_dif),
  sentiment= "negative",
  mediatype = "svt")

#qualitative
df_media_negative_qualitative <- table_rq3_percentage_qualitative_long %>% 
  filter(sentiment == "negative")
df_media_negative_qualitative <- df_media_negative_qualitative %>% 
  mutate(mediatype = "qualitative")
s_negative_qualitative_dif <- df_media_negative_qualitative$percentage[df_media_negative_qualitative$party == "s_sentiment_percentage"] - df_media_negative_qualitative$percentage[df_media_negative_qualitative$party == "sd_sentiment_percentage"]
m_negative_qualitative_dif <- df_media_negative_qualitative$percentage[df_media_negative_qualitative$party == "m_sentiment_percentage"] - df_media_negative_qualitative$percentage[df_media_negative_qualitative$party == "sd_sentiment_percentage"]
df_negative_qualitative_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_negative_qualitative_dif, 
                 m_negative_qualitative_dif),
  sentiment= "negative",
  mediatype = "qualitative")

#tabloid
df_media_negative_tabloid <- table_rq3_percentage_tabloid_long %>% 
  filter(sentiment == "negative")
df_media_negative_tabloid <- df_media_negative_tabloid %>% 
  mutate(mediatype = "tabloid")

s_negative_tabloid_dif <- df_media_negative_tabloid$percentage[df_media_negative_tabloid$party == "s_sentiment_percentage"] - df_media_negative_tabloid$percentage[df_media_negative_tabloid$party == "sd_sentiment_percentage"]
m_negative_tabloid_dif <- df_media_negative_tabloid$percentage[df_media_negative_tabloid$party == "m_sentiment_percentage"] - df_media_negative_tabloid$percentage[df_media_negative_tabloid$party == "sd_sentiment_percentage"]
df_negative_tabloid_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_negative_tabloid_dif, 
                 m_negative_tabloid_dif),
  sentiment= "negative",
  mediatype = "tabloid")
#fuse
df_media_negative <- rbind (df_media_negative_svt,
                            df_media_negative_qualitative,
                            df_media_negative_tabloid)
df_sentiments_mediatype_negative_dif<- rbind(df_negative_svt_dif,df_negative_qualitative_dif, df_negative_tabloid_dif)
#relevel
df_sentiments_mediatype_negative_dif$parties <- factor(df_sentiments_mediatype_negative_dif$parties, levels = c("s", "m"))
df_sentiments_mediatype_negative_dif$mediatype <- factor(df_sentiments_mediatype_negative_dif$mediatype, levels = c("svt",
                                                                                                                    "qualitative",                                                                                                                "tabloid"))


#relevel
df_media_negative$mediatype <- factor (df_media_negative$mediatype, levels = c("svt",
                                                                               "qualitative",
                                                                               "tabloid"))
ggplot(df_media_negative, aes(y=percentage, x=mediatype, fill =party))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        subtitle="Across Media Types",
        title="Negative Sentiments",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("m_sentiment_percentage"="#5e5e5e",
                              "s_sentiment_percentage"="#c6c6c6",
                              "sd_sentiment_percentage"="#00c5cf"),
                    labels= c("Moderate Party",
                              "Social Democratic Party",
                              "SD Party"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.35))+
  scale_x_discrete (labels=c("State-Financed",
                             "Broadsheet",
                             "Tabloid"))+
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave("figures/38_sentiments_neg_mediatype.png", width = 6, height = 3)


#visualise negative differences
ggplot(df_sentiments_mediatype_negative_dif, aes(x=mediatype, y=difference, fill=parties))+
  geom_bar(stat = "identity", position= position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        title="Negative Sentiments",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = ifelse(difference > 0, paste0("+", round(difference, 2) * 100, "%"), 
                               paste0(round(difference, 2) * 100, "%"))),
            vjust = 1.3,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.15, 0.05))+
  scale_x_discrete (labels=c("State-Financed",
                             "Broadsheet",
                             "Tabloid"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6"),
                    labels=c("Social Democratic Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, 
             color = "black",
             size = 1.5)
ggsave("figures/39_sentiments_neg_mediatype_diff.png", width = 6, height = 3)



##Barplots for positive sentiments in different mediatypes
#data preparation
#svt
df_media_positive_svt <- table_rq3_percentage_svt_long %>% 
  filter(sentiment == "positive")
df_media_positive_svt <- df_media_positive_svt %>% 
  mutate(mediatype = "svt")
#qualitative
df_media_positive_qualitative <- table_rq3_percentage_qualitative_long %>% 
  filter(sentiment == "positive")
df_media_positive_qualitative <- df_media_positive_qualitative %>% 
  mutate(mediatype = "qualitative")
#tabloid
df_media_positive_tabloid <- table_rq3_percentage_tabloid_long %>% 
  filter(sentiment == "positive")
df_media_positive_tabloid <- df_media_positive_tabloid %>% 
  mutate(mediatype = "tabloid")
#fuse
df_media_positive <- rbind (df_media_positive_svt,
                            df_media_positive_qualitative,
                            df_media_positive_tabloid)
#relevel 
df_media_positive$mediatype <- factor (df_media_positive$mediatype, levels = c("svt",
                                                                               "qualitative",
                                                                               "tabloid"))
#visualise negative
ggplot(df_media_positive, aes(y=percentage, x=mediatype, fill =party))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        subtitle="Across Media Types",
        title="Positive Sentiments",
        fill = "")+
  geom_text(aes(label = paste0(round(percentage,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 2.8,
            fontface = "bold")+
  scale_fill_manual(values= c("m_sentiment_percentage"="#5e5e5e",
                              "s_sentiment_percentage"="#c6c6c6",
                              "sd_sentiment_percentage"="#00c5cf"),
                    labels= c("Moderate Party", "Social Democratic Party", "SD Party"))+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.35))+
  scale_x_discrete (labels=c("State-Financed", "Broadsheet", "Tabloid"))+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave("figures/40_sentiments_pos_mediatype.png", width = 6, height = 3)


#differences
s_positive_svt_dif <- df_media_positive_svt$percentage[df_media_positive_svt$party == "s_sentiment_percentage"] - df_media_positive_svt$percentage[df_media_positive_svt$party == "sd_sentiment_percentage"]
m_positive_svt_dif <- df_media_positive_svt$percentage[df_media_positive_svt$party == "m_sentiment_percentage"] - df_media_positive_svt$percentage[df_media_positive_svt$party == "sd_sentiment_percentage"]
df_positive_svt_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_positive_svt_dif, 
                 m_positive_svt_dif),
  sentiment= "positive",
  mediatype = "svt")

#qualitative
s_positive_qualitative_dif <- df_media_positive_qualitative$percentage[df_media_positive_qualitative$party == "s_sentiment_percentage"] - df_media_positive_qualitative$percentage[df_media_positive_qualitative$party == "sd_sentiment_percentage"]
m_positive_qualitative_dif <- df_media_positive_qualitative$percentage[df_media_positive_qualitative$party == "m_sentiment_percentage"] - df_media_positive_qualitative$percentage[df_media_positive_qualitative$party == "sd_sentiment_percentage"]
df_positive_qualitative_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_positive_qualitative_dif, 
                 m_positive_qualitative_dif),
  sentiment= "positive",
  mediatype = "qualitative")

#tabloid
s_positive_tabloid_dif <- df_media_positive_tabloid$percentage[df_media_positive_tabloid$party == "s_sentiment_percentage"] - df_media_positive_tabloid$percentage[df_media_positive_tabloid$party == "sd_sentiment_percentage"]
m_positive_tabloid_dif <- df_media_positive_tabloid$percentage[df_media_positive_tabloid$party == "m_sentiment_percentage"] - df_media_positive_tabloid$percentage[df_media_positive_tabloid$party == "sd_sentiment_percentage"]
df_positive_tabloid_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_positive_tabloid_dif, 
                 m_positive_tabloid_dif),
  sentiment= "positive",
  mediatype = "tabloid")
#fuse
df_media_positive <- rbind (df_media_positive_svt,
                            df_media_positive_qualitative,
                            df_media_positive_tabloid)
df_sentiments_mediatype_positive_dif<- rbind(df_positive_svt_dif,df_positive_qualitative_dif, df_positive_tabloid_dif)
#relevel
df_sentiments_mediatype_positive_dif$parties <- factor(df_sentiments_mediatype_positive_dif$parties, levels = c("s", "m"))
df_sentiments_mediatype_positive_dif$mediatype <- factor(df_sentiments_mediatype_positive_dif$mediatype, levels = c("svt",
                                                                                                                    "qualitative",                                                                                                                    "tabloid"))
#Visualising 
ggplot(df_sentiments_mediatype_positive_dif,
                              aes(x=mediatype,
                                  y=difference,
                                  fill=parties))+
  geom_bar(stat = "identity", position= position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        title="Positive Sentiments",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = ifelse(difference > 0, paste0("+", round(difference, 2) * 100, "%"), 
                               paste0(round(difference, 2) * 100, "%"))),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.05, 0.1))+
  scale_x_discrete (labels=c("State-Financed",
                             "Broadsheet",
                             "Tabloid"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6"),
                    labels=c("Social Democratic Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, 
             color = "black",
             size = 1.5)
ggsave("figures/41_sentiments_pos_mediatype_diff.png", width = 6, height = 3)


#------------MULTINOMINAL LOG REGRESSION
#-------svt
#Data Preparation
#select relevant variables
df_rq3_reg_svt <- df_svt %>% 
  select(m_sentiment, s_sentiment, sd_sentiment, electionyear, mediatype, newspaper, english_headline, ID, s, sd, m)
#change to long format
df_rq3_reg_svt<-df_rq3_reg_svt %>% 
  pivot_longer(cols=c(m_sentiment,
                      sd_sentiment,
                      s_sentiment),
               names_to ="party",
               values_to = "sentiment")
#drop when sentiment column has no value
df_rq3_reg_svt <- df_rq3_reg_svt[complete.cases(df_rq3_reg_svt$sentiment), ]
#as factor
df_rq3_reg_svt <- df_rq3_reg_svt %>% 
  mutate(sentiment=as.factor(sentiment),
         party=as.factor(party),
         electionyear=as.factor(electionyear))
#define reference category 
df_rq3_reg_svt$party <- relevel(df_rq3_reg_svt$party, ref = "sd_sentiment")
df_rq3_reg_svt$sentiment <- relevel(df_rq3_reg_svt$sentiment, ref = "neutral")
#apply model
model_rq3_svt <- multinom(sentiment ~ party, data = df_rq3_reg_svt)
summary(model_rq3_svt)

#-------qualitative
#Data Preparation
#select relevant variables
df_rq3_reg_qualitative <- df_qualitative %>% 
  select(m_sentiment, s_sentiment, sd_sentiment, electionyear, mediatype, newspaper, english_headline, ID, s, sd, m)
#change to long format
df_rq3_reg_qualitative<-df_rq3_reg_qualitative %>% 
  pivot_longer(cols=c(m_sentiment,
                      sd_sentiment,
                      s_sentiment),
               names_to ="party",
               values_to = "sentiment")
#drop when sentiment column has no value
df_rq3_reg_qualitative <- df_rq3_reg_qualitative[complete.cases(df_rq3_reg_qualitative$sentiment), ]
#as factor
df_rq3_reg_qualitative <- df_rq3_reg_qualitative %>% 
  mutate(sentiment=as.factor(sentiment),
         party=as.factor(party),
         electionyear=as.factor(electionyear))
#define reference category 
df_rq3_reg_qualitative$party <- relevel(df_rq3_reg_qualitative$party, ref = "sd_sentiment")
df_rq3_reg_qualitative$sentiment <- relevel(df_rq3_reg_qualitative$sentiment, ref = "neutral")
#apply model
model_rq3_qualitative <- multinom(sentiment ~ party, data = df_rq3_reg_qualitative)
summary(model_rq3_qualitative)


#-------tabloid
#Data Preparation
#select relevant variables
df_rq3_reg_tabloid <- df_tabloid %>% 
  select(m_sentiment, s_sentiment, sd_sentiment, electionyear, mediatype, newspaper, english_headline, ID, s, sd, m)
#change to long format
df_rq3_reg_tabloid<-df_rq3_reg_tabloid %>% 
  pivot_longer(cols=c(m_sentiment,
                      sd_sentiment,
                      s_sentiment),
               names_to ="party",
               values_to = "sentiment")
#drop when sentiment column has no value
df_rq3_reg_tabloid <- df_rq3_reg_tabloid[complete.cases(df_rq3_reg_tabloid$sentiment), ]
#as factor
df_rq3_reg_tabloid <- df_rq3_reg_tabloid %>% 
  mutate(sentiment=as.factor(sentiment),
         party=as.factor(party),
         electionyear=as.factor(electionyear))
#define reference category 
df_rq3_reg_tabloid$party <- relevel(df_rq3_reg_tabloid$party, ref = "sd_sentiment")
df_rq3_reg_tabloid$sentiment <- relevel(df_rq3_reg_tabloid$sentiment, ref = "neutral")
#apply model
model_rq3_tabloid <- multinom(sentiment ~ party, data = df_rq3_reg_tabloid)
summary(model_rq3_tabloid)


#Tables
#Table with coefficents
stargazer(model_rq3_svt,model_rq3_qualitative,model_rq3_tabloid,
          type="html",
          out="model_rq3.htm",
          intercept.bottom = FALSE,
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          column.labels = c("<b>State-Financed</b><br>Negative", "<br>Positive",
                            "<b>Broadsheet</b><br>Negative","<br>Positive",
                            "<b>Tabloid</b><br>Negative", "<br>Positive"),
          dep.var.labels = c("", "","", "","", ""),
          title="Multinominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_rq3.htm")



#two tailed signifiance test 
#svt
z_svt<- summary(model_rq3_svt)$coefficients/summary(model_rq3_svt)$standard.errors
p_svt <- (1-pnorm(abs(z_svt), 0,1))*2
p_svt
#tabloid
z_tabloid<- summary(model_rq3_tabloid)$coefficients/summary(model_rq3_tabloid)$standard.errors
p_tabloid <- (1-pnorm(abs(z_tabloid), 0,1))*2
p_tabloid
#qualitative
z_qualitative<- summary(model_rq3_qualitative)$coefficients/summary(model_rq3_qualitative)$standard.errors
p_qualitative <- (1-pnorm(abs(z_qualitative), 0,1))*2
p_qualitative

#predicted propabilities 
fit_effect_svt <- Effect("party", model_rq3_svt)
#into data framr for predicted propabilities
effect_df_svt <- as.data.frame(cbind(fit_effect_svt$x,
                                     fit_effect_svt$prob,
                                     fit_effect_svt$lower.prob,
                                     fit_effect_svt$upper.prob))
#label the values
names(effect_df_svt) <- c("party",
                          "prob_neutral",
                          "prob_negative",
                          "prob_positive",
                          "prob_neutral_low",
                          "prob_negative_low",
                          "prob_positive_low",
                          "prob_neutral_upper",
                          "prob_negative_upper",
                          "prob_positive_upper")

#Positive Sentiments df
df_positive_svt <- data.frame(
  party = effect_df_svt$party,
  propability = effect_df_svt$prob_positive,
  lower = effect_df_svt$prob_positive_low,
  upper = effect_df_svt$prob_positive_upper)
#plot of predcited propbilities for positive sentiments 

#relevel
df_positive_svt$party <- factor(df_positive_svt$party, levels = c("s_sentiment",
                                                                  "sd_sentiment",
                                                                  "m_sentiment"))

ggplot(df_positive_svt,
                       aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "State-Financed News",
       subtitle = "Positive Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party", 
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf",
                                "m_sentiment" = "#5e5e5e",
                                "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/42_sentiments_pos_mediatype_pp.png", width = 6, height = 3)

#negative Sentiments df
df_negative_svt <- data.frame(
  party = effect_df_svt$party,
  propability = effect_df_svt$prob_negative,
  lower = effect_df_svt$prob_negative_low,
  upper = effect_df_svt$prob_negative_upper)
#relevel
df_negative_svt$party <- factor(df_negative_svt$party, levels = c("s_sentiment",
                                                                  "sd_sentiment",
                                                                  "m_sentiment"))
#plot of predcited propbilities for negative sentiments 
ggplot(df_negative_svt, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "State-Financed News",
       subtitle = "Negative Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party", 
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.32), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf", "m_sentiment" = "#5e5e5e", "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/43_sentiments_neg_mediatype_pp.png", width = 6, height = 3)


#predicted propabilities 
fit_effect_qualitative <- Effect("party", model_rq3_qualitative)
#into data framr for predicted propabilities
effect_df_qualitative <- as.data.frame(cbind(fit_effect_qualitative$x,
                                             fit_effect_qualitative$prob,
                                             fit_effect_qualitative$lower.prob,
                                             fit_effect_qualitative$upper.prob))
#label the values
names(effect_df_qualitative) <- c("party",
                                  "prob_neutral",
                                  "prob_negative",
                                  "prob_positive",
                                  "prob_neutral_low",
                                  "prob_negative_low",
                                  "prob_positive_low",
                                  "prob_neutral_upper",
                                  "prob_negative_upper",
                                  "prob_positive_upper")

#Positive Sentiments df
df_positive_qualitative <- data.frame(
  party = effect_df_qualitative$party,
  propability = effect_df_qualitative$prob_positive,
  lower = effect_df_qualitative$prob_positive_low,
  upper = effect_df_qualitative$prob_positive_upper)

#relevel
df_positive_qualitative$party <- factor(df_positive_qualitative$party, levels = c("s_sentiment",
                                                                                  "sd_sentiment",
                                                                                  "m_sentiment"))
#plot of predcited propbilities for positive sentiments 
ggplot(df_positive_qualitative,
                                 aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Broadsheet News",
       subtitle = "Positive Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.4,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf",
                                "m_sentiment" = "#5e5e5e",
                                "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/44_sentiments_pos_qual_pp.png", width = 6, height = 3)


#negative Sentiments df
df_negative_qualitative <- data.frame(
  party = effect_df_qualitative$party,
  propability = effect_df_qualitative$prob_negative,
  lower = effect_df_qualitative$prob_negative_low,
  upper = effect_df_qualitative$prob_negative_upper)
#relevel
df_negative_qualitative$party <- factor(df_negative_qualitative$party, levels = c("s_sentiment",
                                                                                  "sd_sentiment",
                                                                                  "m_sentiment"))
#plot of predcited probabilities for negative sentiments p_negative_qualitative <- 
ggplot(df_negative_qualitative, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Broadsheet News",
       subtitle = "Negative Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.32), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf",
                                "m_sentiment" = "#5e5e5e",
                                "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/45_sentiments_neg_qual_pp.png", width = 6, height = 3)


#predicted propabilities 
fit_effect_tabloid <- Effect("party", model_rq3_tabloid)
#into data framr for predicted propabilities
effect_df_tabloid <- as.data.frame(cbind(fit_effect_tabloid$x,
                                         fit_effect_tabloid$prob,
                                         fit_effect_tabloid$lower.prob,
                                         fit_effect_tabloid$upper.prob))
#label the values
names(effect_df_tabloid) <- c("party",
                              "prob_neutral",
                              "prob_negative",
                              "prob_positive",
                              "prob_neutral_low",
                              "prob_negative_low",
                              "prob_positive_low",
                              "prob_neutral_upper",
                              "prob_negative_upper",
                              "prob_positive_upper")

#Positive Sentiments df
df_positive_tabloid <- data.frame(
  party = effect_df_tabloid$party,
  propability = effect_df_tabloid$prob_positive,
  lower = effect_df_tabloid$prob_positive_low,
  upper = effect_df_tabloid$prob_positive_upper)

#relevel
df_positive_tabloid$party <- factor(df_positive_tabloid$party, levels = c("s_sentiment",
                                                                          "sd_sentiment",
                                                                          "m_sentiment"))

#plot of predcited propbilities for positive sentiments 
ggplot(df_positive_tabloid,
                             aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Tabloid Newspaper",
       subtitle = "Positive Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf",
                                "m_sentiment" = "#5e5e5e",
                                "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/46_sentiments_pos_tab_pp.png", width = 6, height = 3)


#negative Sentiments df
df_negative_tabloid <- data.frame(
  party = effect_df_tabloid$party,
  propability = effect_df_tabloid$prob_negative,
  lower = effect_df_tabloid$prob_negative_low,
  upper = effect_df_tabloid$prob_negative_upper)

#relevel
df_negative_tabloid$party <- factor(df_negative_tabloid$party, levels = c("s_sentiment",
                                                                          "sd_sentiment",
                                                                          "m_sentiment"))
#plot of predcited propbilities for negative sentiments 
ggplot(df_negative_tabloid, aes(x = party, y = propability, color = party)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Tabloid News",
       subtitle = "Negative Sentiments With 95% Confidence Interval") +
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty")) +
  geom_text(aes(label = paste0(round(propability, 4) * 100, "%")),
            vjust = 0,
            hjust = -0.37,
            size = 3,
            check_overlap = TRUE,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.32), labels = scales::percent_format()) +
  scale_color_manual(values = c("sd_sentiment" = "#00c5cf", "m_sentiment" = "#5e5e5e", "s_sentiment" = "#c6c6c6")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/47_sentiments_neg_tab_pp.png", width = 6, height = 3)

