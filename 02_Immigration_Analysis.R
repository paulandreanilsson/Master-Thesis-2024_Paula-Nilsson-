#This is the code for the analysis concerning the immigration topic in Chapter 7.2

#load libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)#to change format
library(stargazer)#for tables
library(reshape2)#for melting 
library(gridExtra)#ggplots next to each other 
library(effects)  #for multinominal predicted prob
library(scales)#for scale

df <- read_excel("04-21-24_df_headings_sen.xlsx")

###################################################IMMIGRATION
#RESEARCH QUESTION 1 

#----------------------------------------------VISUALISATION
df_migration<- df %>% 
  filter(migration==TRUE)
migration <- sum(df_migration$migration)
#sd
df_migration_sd <- df_migration %>% 
  filter(sd==TRUE)
migration_sd = sum(df_migration_sd$sd)
migration_sd_per = sum(df_migration_sd$sd)/migration
#s
df_migration_s <- df_migration%>% 
  filter(s==TRUE)
migration_s = sum(df_migration_s$s)
migration_s_per = sum(df_migration_s$s)/migration
#m
df_migration_m <- df_migration %>% 
  filter(m==TRUE)
migration_m = sum(df_migration_m$m)
migration_m_per = sum(df_migration_m$m)/migration

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_migration <- c(migration_sd_per,
                           migration_s_per,
                           migration_m_per)
df_migration_per<- data.frame(Column1 = parties,
                              Column2= percentages_migration)
colnames(df_migration_per) <- c("parties", "percentages_migration")
#absolute 
absolute_migration<- c(migration_sd,migration_s, migration_m)
df_migration_absolute<- data.frame(Column1 = parties, Column2= absolute_migration)
colnames(df_migration_absolute) <- c("parties","migration")

#relevel so that SD in the middle 
df_migration_per$parties <- as.factor(df_migration_per$parties)
df_migration_per$parties <- factor(df_migration_per$parties, levels = c("s", "sd", "m"))

#Visualising 
ggplot(df_migration_per, aes(x=parties, y=percentages_migration, fill=parties))+
  geom_bar(stat = "identity")+
  labs (x= "Parties",
        y= "Percentage",
        title="Immigration Topic",
        subtitle = "Share of Each Party",
        fill = "Parties")+
  geom_text(aes(label = paste0(round(percentages_migration,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.5))+
  scale_x_discrete (labels=c("Social Democratic Party",
                             "SD Party",
                             "Moderate Party"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6", "sd"="#00c5cf"))+
  theme_minimal()+
  theme(legend.position = "none")
ggsave("figures/09_immigration.png", width = 6, height = 3)


#visualise differences

#extract percentage
s_migration_dif <- df_migration_per$percentages[df_migration_per$parties == "s"] - df_migration_per$percentages[df_migration_per$parties == "sd"]
m_migration_dif <- df_migration_per$percentages[df_migration_per$parties == "m"] - df_migration_per$percentages[df_migration_per$parties == "sd"]
#create df
df_migration_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_migration_dif, m_migration_dif))
#relevel
df_migration_dif$parties <- factor(df_migration_dif$parties, levels = c("s", "m"))

#Visualising 
ggplot(df_migration_dif, aes(x=parties, y=difference, fill=parties))+
  geom_bar(stat = "identity")+
  labs (x= "Parties",
        y= "Percentage",
        title="Immigration Topic",
        subtitle = "Relative Differences with SD Party",
        fill = "Parties")+
  geom_text(aes(label = paste0(round(difference,2)*100, "%")),
            vjust = 1.3,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.15, 0.05))+
  scale_x_discrete (labels=c("Social Democratic\nParty",
                             "Moderate\nParty"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6"))+
  theme_minimal()+
  theme(legend.position = "none")+
  geom_hline(yintercept = 0, color = "black", linewidth = 1.5)  
ggsave("figures/10_immigration_diff.png", width = 6, height = 3)


#-------------------------------Binominal regression analysis

#DATA PREPARATION 
df_migration_reg <- df_migration %>% 
  select(mediatype, electionyear, s, m, sd, english_headline_intro)
df_migration_reg_long <- df_migration_reg %>%
  pivot_longer(cols = c(s, sd, m), names_to = "party", values_to = "migration")
#change variables into factors
df_migration_reg_long <- df_migration_reg_long %>% 
  mutate(party = as.factor(party),
         migration= as.factor(migration))
#reference category
df_migration_reg_long$party <- relevel(df_migration_reg_long$party, ref="sd")


#CREATING MODEL
model_migration <- glm(migration ~ party, 	
                       family = binomial(), data = df_migration_reg_long)
#modelsummary
summary(model_migration)

#stargazer
stargazer(model_migration,
          type="html",
          out="model_migration.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("Immigration"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Binominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_migration.htm")


#predicted propabilities 
df_migration_reg_long$predicted_prob <- predict(model_migration, type = "response")
#Calculation for 95%confidence intervall
log_odds_migration <- predict(model_migration, type = "link", se.fit = TRUE)
p_migration <- df_migration_reg_long$predicted_prob
derivative_migration = p_migration * (1 - p_migration)
se_prob_migration <- log_odds_migration$se.fit * derivative_migration
z_value <- qnorm(0.975) 
df_migration_reg_long$ci_lower <- df_migration_reg_long$predicted_prob - z_value * se_prob_migration
df_migration_reg_long$ci_upper <- df_migration_reg_long$predicted_prob + z_value * se_prob_migration

#relevel 
df_migration_reg_long_graph <- df_migration_reg_long
df_migration_reg_long_graph$party <- factor(df_migration_reg_long_graph$party, levels = c("s","sd", "m"))

#display predicted propabilities
ggplot(df_migration_reg_long_graph, aes(x = party, y = predicted_prob, color= party)) +
  geom_point(alpha = 0.5,  size=7) +
  geom_errorbar(aes(ymin = ci_lower,
                    ymax = ci_upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predicted Probability",
       title = "Immigration Topic",
       subtitle = "With 95% Confidence Interval")+
  scale_x_discrete(labels = c("Social Democratic\nParty",
                              "SD Party",
                              "Moderate\nParty"))+
  geom_text(aes(label = paste0(round(predicted_prob, 4)*100, "%")),  
            vjust = 0, 
            hjust = -0.5, 
            size = 3,      
            check_overlap = TRUE,
            color="black") +
  scale_y_continuous(limits = c(0.2,0.5),
                     labels = scales::percent_format())+
  scale_color_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6", "sd"="#00c5cf"))+
  theme_minimal()+
  theme (legend.position = "none")

ggsave("figures/11_immigration_pp.png", width = 6, height = 3)


#Subsidiary Question 1

#----------------------------------------------VISUALISATION

#Data Preparation 
df_migration_2018 <- df_migration %>% 
  filter(electionyear==2018)
migration_2018 <- sum(df_migration_2018$migration)

#sd
df_migration_sd_2018 <- df_migration_2018 %>% 
  filter(sd==TRUE)
migration_sd_2018 = sum(df_migration_sd_2018$sd)
migration_sd_per_2018 = sum(df_migration_sd_2018$sd)/migration_2018
#s
df_migration_s_2018 <- df_migration_2018 %>% 
  filter(s==TRUE)
migration_s_2018 = sum(df_migration_s_2018$s)
migration_s_per_2018 = sum(df_migration_s_2018$s)/migration_2018
#m
df_migration_m_2018 <- df_migration_2018 %>% 
  filter(m==TRUE)
migration_m_2018 = sum(df_migration_m_2018$m)
migration_m_per_2018 = sum(df_migration_m_2018$m)/migration_2018

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_migration_2018 <- c(migration_sd_per_2018,migration_s_per_2018, migration_m_per_2018 )
df_migration_per_2018<- data.frame(Column1 = parties, Column2= percentages_migration_2018)
colnames(df_migration_per_2018) <- c("parties","percentages_migration")
df_migration_per_2018<- df_migration_per_2018 %>% 
  mutate(electionyear=2018)

#absolute numbers
absolute_migration_2018 <- c(migration_sd_2018,migration_s_2018, migration_m_2018)
df_migration_absolute_2018<- data.frame(Column1 = parties, Column2= absolute_migration_2018)
colnames(df_migration_absolute_2018) <- c("parties","migration")

#2022 
#Data Preparation 
df_migration_2022 <- df_migration %>% 
  filter(electionyear==2022)
migration_2022 <- sum(df_migration_2022$migration)

#sd
df_migration_sd_2022 <- df_migration_2022 %>% 
  filter(sd==TRUE)
migration_sd_2022 = sum(df_migration_sd_2022$sd)
migration_sd_per_2022 = sum(df_migration_sd_2022$sd)/migration_2022
#s
df_migration_s_2022 <- df_migration_2022 %>% 
  filter(s==TRUE)
migration_s_2022 = sum(df_migration_s_2022$s)
migration_s_per_2022 = sum(df_migration_s_2022$s)/migration_2022
#m
df_migration_m_2022 <- df_migration_2022 %>% 
  filter(m==TRUE)
migration_m_2022 = sum(df_migration_m_2022$m)
migration_m_per_2022 = sum(df_migration_m_2022$m)/migration_2022

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_migration_2022 <- c(migration_sd_per_2022,migration_s_per_2022, migration_m_per_2022 )
df_migration_per_2022<- data.frame(Column1 = parties, Column2= percentages_migration_2022)
colnames(df_migration_per_2022) <- c("parties","percentages_migration")
df_migration_per_2022 <- df_migration_per_2022 %>% 
  mutate(electionyear=2022)


#absolute numbers
absolute_migration_2022 <- c(migration_sd_2022,migration_s_2022, migration_m_2022)
df_migration_absolute_2022<- data.frame(Column1 = parties, Column2= absolute_migration_2022)
colnames(df_migration_absolute_2022) <- c("parties","migration")


#fuse 2018 and 2022
df_migration_per_2018_2022 <- rbind(df_migration_per_2018,df_migration_per_2022)
df_migration_per_2018_2022 <- df_migration_per_2018_2022 %>% 
  mutate(electionyear=as.factor(electionyear))

#relevel
df_migration_per_2018_2022$parties <- factor(df_migration_per_2018_2022$parties, levels = c("s","sd", "m"))


#Visualising 
ggplot(df_migration_per_2018_2022, aes(x=electionyear, y=percentages_migration, fill=parties))+
  geom_bar(stat = "identity", position=position_dodge())+
  labs (x= "Election Year",
        y= "Percentage",
        title="Immigration Topic",
        subtitle="Share of Each Party",
        fill="")+
  geom_text(aes(label = paste0(round(percentages_migration,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.6))+
  scale_x_discrete (labels=c("2018",
                             "2022"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6",
                              "sd"="#00c5cf"),
                    labels=c("Social Democratic Party",
                             "SD Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme (legend.position = "bottom")
ggsave("figures/12_immigration_election.png", width = 6, height = 4)


#visualise differences
#extract percentage 2018
s_migration_dif_2018 <- df_migration_per_2018$percentages[df_migration_per_2018$parties == "s"] - df_migration_per_2018$percentages[df_migration_per$parties == "sd"]
m_migration_dif_2018 <- df_migration_per_2018$percentages[df_migration_per_2018$parties == "m"] - df_migration_per_2018$percentages[df_migration_per$parties == "sd"]
#extract percentage 2022
s_migration_dif_2022 <- df_migration_per_2022$percentages[df_migration_per_2022$parties == "s"] - df_migration_per_2022$percentages[df_migration_per$parties == "sd"]
m_migration_dif_2022 <- df_migration_per_2022$percentages[df_migration_per_2022$parties == "m"] - df_migration_per_2022$percentages[df_migration_per$parties == "sd"]


#create df
df_migration_dif_election <- data.frame(
  parties = c("s", "m","s", "m" ),
  difference = c(s_migration_dif_2018, m_migration_dif_2018,s_migration_dif_2022,m_migration_dif_2022),
  electionyear = c("2018", "2018", "2022", "2022")
)
#relevel
df_migration_dif_election$parties <- factor(df_migration_dif_election$parties, levels = c("s", "m"))

#Visualising 
ggplot(df_migration_dif_election, aes(x=electionyear, y=difference, fill=parties))+
  geom_bar(stat = "identity", position=position_dodge())+
  labs (x= "Election",
        y= "Percentage",
        title="Immigration Topic",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = paste0(round(difference,2)*100, "%")),
            vjust = 1.3,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.25, 0.05))+
  scale_x_discrete (labels=c("2018",
                             "2022"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6"),
                    labels = c("Social Democratic Party",
                               "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color = "black",
             size = 1.5)
ggsave("figures/13_immigration_election_diff.png", width = 6, height = 3)

#----------------------------------------------Binominal Regression Analysis

#---2018
#DATA PREPARATION
df_migration_reg_long_2018 <- df_migration_reg_long %>% 
  filter(electionyear==2018)

#CREATING MODEL
model_migration_2018 <- glm(migration ~ party, 	
                            family = binomial(), data = df_migration_reg_long_2018)
#modelsummary
summary(model_migration_2018)

#----2022
#DATA PREPARATION
df_migration_reg_long_2022 <- df_migration_reg_long %>% 
  filter(electionyear==2022)
#CREATING MODEL
model_migration_2022 <- glm(migration ~ party, 	
                            family = binomial(), data = df_migration_reg_long_2022)
#modelsummary
summary(model_migration_2022)

#STARGAZER 2018 & 2022
stargazer(model_migration_2018,model_migration_2022,
          type="html",
          out="model_migration_electionyears.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("Immigration"),
          column.labels = c("<b>Election<br>2018",
                            "<b>Election<br>2022"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Binominal Regression Analysis; Log(Odds) ",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_migration_electionyears.htm")


#predicted propabilities 
df_migration_reg_long_2018$predicted_prob <- predict(model_migration_2018, type = "response")
#Calculation for 95%confidence intervall
log_odds_migration_2018 <- predict(model_migration_2018, type = "link", se.fit = TRUE)
p_migration_2018 <- df_migration_reg_long_2018$predicted_prob
derivative_migration_2018 = p_migration_2018 * (1 - p_migration_2018)
se_prob_migration_2018 <- log_odds_migration_2018$se.fit * derivative_migration_2018
z_value <- qnorm(0.975) 
df_migration_reg_long_2018$ci_lower <- df_migration_reg_long_2018$predicted_prob - z_value * se_prob_migration_2018
df_migration_reg_long_2018$ci_upper <- df_migration_reg_long_2018$predicted_prob + z_value * se_prob_migration_2018

#predicted propabilities 
df_migration_reg_long_2022$predicted_prob <- predict(model_migration_2022, type = "response")
#Calculation for 95%confidence intervall
log_odds_migration_2022 <- predict(model_migration_2022, type = "link", se.fit = TRUE)
p_migration_2022 <- df_migration_reg_long_2022$predicted_prob
derivative_migration_2022 = p_migration_2022 * (1 - p_migration_2022)
se_prob_migration_2022 <- log_odds_migration_2022$se.fit * derivative_migration_2022
z_value <- qnorm(0.975) 
df_migration_reg_long_2022$ci_lower <- df_migration_reg_long_2022$predicted_prob - z_value * se_prob_migration_2022
df_migration_reg_long_2022$ci_upper <- df_migration_reg_long_2022$predicted_prob + z_value * se_prob_migration_2022

#create dataset for plot 
#2018

#sd
#pp
migration_2018_sd_pp <- df_migration_reg_long_2018 %>%
  filter(party == "sd") %>%
  pull(predicted_prob) %>% 
  first()
#ci upper
migration_2018_sd_upper <- df_migration_reg_long_2018 %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_2018_sd_lower <- df_migration_reg_long_2018 %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()

#s
#pp
migration_2018_s_pp <- df_migration_reg_long_2018 %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_2018_s_upper <- df_migration_reg_long_2018 %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_2018_s_lower <- df_migration_reg_long_2018 %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()

#m
#pp
migration_2018_m_pp <- df_migration_reg_long_2018 %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_2018_m_upper <- df_migration_reg_long_2018 %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_2018_m_lower <- df_migration_reg_long_2018 %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#2022

#sd
#pp
migration_2022_sd_pp <- df_migration_reg_long_2022 %>%
  filter(party == "sd") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_2022_sd_upper <- df_migration_reg_long_2022 %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_2022_sd_lower <- df_migration_reg_long_2022 %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()
#s
#pp
migration_2022_s_pp <- df_migration_reg_long_2022 %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_2022_s_upper <- df_migration_reg_long_2022 %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_2022_s_lower <- df_migration_reg_long_2022 %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()
#m
#pp
migration_2022_m_pp <- df_migration_reg_long_2022 %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_2022_m_upper <- df_migration_reg_long_2022 %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_2022_m_lower <- df_migration_reg_long_2022 %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()
migration_election_pp <- c(migration_2018_sd_pp,
                           migration_2018_s_pp,
                           migration_2018_m_pp,
                           migration_2022_sd_pp,
                           migration_2022_s_pp,
                           migration_2022_m_pp)
migration_election_upper <- c(migration_2018_sd_upper,
                              migration_2018_s_upper,
                              migration_2018_m_upper,
                              migration_2022_sd_upper,
                              migration_2022_s_upper,
                              migration_2022_m_upper)
migration_election_lower <- c(migration_2018_sd_lower,
                              migration_2018_s_lower,
                              migration_2018_m_lower,
                              migration_2022_sd_lower,
                              migration_2022_s_lower,
                              migration_2022_m_lower)
migration_election <- c("2018",
                        "2018",
                        "2018",
                        "2022",
                        "2022",
                        "2022")
migration_election_party <- c("sd",
                        "s",
                        "m",
                        "sd",
                        "s",
                        "m")
df_migration_pp_election <- data.frame(Column1= migration_election_pp,
                           Column2 = migration_election_upper,
                           Column3= migration_election_lower,
                           Column4= migration_election,
                           Column4 = migration_election_party)
colnames(df_migration_pp_election)<- c("predicted_prob",
                                       "ci_upper",
                                       "ci_lower",
                                       "electionyear",
                                       "party")
df_migration_pp_election$party <- factor(df_migration_pp_election$party, levels = c("s", "sd", "m"))

#display predicted propabilities
ggplot(df_migration_pp_election, aes(x = electionyear,
                                     y = predicted_prob,
                                     group=party,
                                     color= party)) +
  geom_point(size=7,
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_lower,
                    ymax = ci_upper),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Election",
       y = "Prediced Propbability",
       title = "Immigration Topic",
       subtitle= "With 95% Confidence Interval",
       color= "")+
  scale_x_discrete(labels = c("2018", "2022"))+
   geom_text(aes(label = paste0(round(predicted_prob, 4)*100, "%")),
             vjust = -1.3,
             hjust =-0.4,
             size = 3,
             position = position_dodge2(width = 0.9),
             color="black") +
  scale_y_continuous(limits = c(0.2,0.6),
                     labels = scales::percent_format())+
  scale_color_manual(values= c("m"="#5e5e5e",
                               "s"="#c6c6c6",
                               "sd"="#00c5cf"),
                     labels = c("Social Democratic Party",
                                "SD Party",
                                "Moderate Party"))+
  theme_minimal()+
  theme (legend.position = "bottom")
ggsave("figures/14_immigration_election_pp.png", width = 6, height = 3.5)



#Research Questions 3 

##----------------------------------------------Visualisation 
#svt
#Data Preparation 
df_migration_svt <- df_migration %>% 
  filter(mediatype=='state_finansed')
migration_svt <- sum(df_migration_svt$migration)

#sd
df_migration_sd_svt <- df_migration_svt %>% 
  filter(sd==TRUE)
migration_sd_svt = sum(df_migration_sd_svt$sd)
migration_sd_per_svt = sum(df_migration_sd_svt$sd)/migration_svt

#s
df_migration_s_svt <- df_migration_svt %>% 
  filter(s==TRUE)
migration_s_svt = sum(df_migration_s_svt$s)
migration_s_per_svt = sum(df_migration_s_svt$s)/migration_svt
#m
df_migration_m_svt <- df_migration_svt %>% 
  filter(m==TRUE)
migration_m_svt = sum(df_migration_m_svt$m)
migration_m_per_svt = sum(df_migration_m_svt$m)/migration_svt

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_migration_svt <- c(migration_sd_per_svt,migration_s_per_svt, migration_m_per_svt )
df_migration_per_svt<- data.frame(Column1 = parties, Column2= percentages_migration_svt)
colnames(df_migration_per_svt) <- c("parties","percentages_migration")
df_migration_per_svt <- df_migration_per_svt %>% 
  mutate(mediatype="state_finansed")

#absolute numbers
absolute_migration_svt <- c(migration_sd_svt,migration_s_svt, migration_m_svt)
df_migration_absolute_svt<- data.frame(Column1 = parties, Column2= absolute_migration_svt)
colnames(df_migration_absolute_svt) <- c("parties","migration")

#qualitative
#Data Preparation 
df_migration_qualitative <- df_migration %>% 
  filter(mediatype=='qualitative')
migration_qualitative <- sum(df_migration_qualitative$migration)


#sd
df_migration_sd_qualitative <- df_migration_qualitative %>% 
  filter(sd==TRUE)
migration_sd_qualitative = sum(df_migration_sd_qualitative$sd)
migration_sd_per_qualitative = sum(df_migration_sd_qualitative$sd)/migration_qualitative
#s
df_migration_s_qualitative <- df_migration_qualitative %>% 
  filter(s==TRUE)
migration_s_qualitative = sum(df_migration_s_qualitative$s)
migration_s_per_qualitative = sum(df_migration_s_qualitative$s)/migration_qualitative
#m
df_migration_m_qualitative <- df_migration_qualitative %>% 
  filter(m==TRUE)
migration_m_qualitative = sum(df_migration_m_qualitative$m)
migration_m_per_qualitative = sum(df_migration_m_qualitative$m)/migration_qualitative

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_migration_qualitative <- c(migration_sd_per_qualitative,migration_s_per_qualitative, migration_m_per_qualitative )
df_migration_per_qualitative<- data.frame(Column1 = parties, Column2= percentages_migration_qualitative)
colnames(df_migration_per_qualitative) <- c("parties","percentages_migration")
df_migration_per_qualitative <- df_migration_per_qualitative %>% 
  mutate(mediatype="qualitative")

#absolute numbers
absolute_migration_qualitative <- c(migration_sd_qualitative,migration_s_qualitative, migration_m_qualitative)
df_migration_absolute_qualitative<- data.frame(Column1 = parties, Column2= absolute_migration_qualitative)
colnames(df_migration_absolute_qualitative) <- c("parties","migration")



#tabloid
#Data Preparation 
df_migration_tabloid <- df_migration %>% 
  filter(mediatype=='tabloid')
migration_tabloid <- sum(df_migration_tabloid$migration)

#sd
df_migration_sd_tabloid <- df_migration_tabloid %>% 
  filter(sd==TRUE)
migration_sd_tabloid = sum(df_migration_sd_tabloid$sd)
migration_sd_per_tabloid = sum(df_migration_sd_tabloid$sd)/migration_tabloid

#s
df_migration_s_tabloid <- df_migration_tabloid %>% 
  filter(s==TRUE)
migration_s_tabloid = sum(df_migration_s_tabloid$s)
migration_s_per_tabloid = sum(df_migration_s_tabloid$s)/migration_tabloid

#m
df_migration_m_tabloid <- df_migration_tabloid %>% 
  filter(m==TRUE)
migration_m_tabloid = sum(df_migration_m_tabloid$m)
migration_m_per_tabloid= sum(df_migration_m_tabloid$m)/migration_tabloid

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_migration_tabloid <- c(migration_sd_per_tabloid,
                                   migration_s_per_tabloid,
                                   migration_m_per_tabloid )
df_migration_per_tabloid<- data.frame(Column1 = parties, Column2= percentages_migration_tabloid)
colnames(df_migration_per_tabloid) <- c("parties","percentages_migration")
df_migration_per_tabloid <- df_migration_per_tabloid %>% 
  mutate(mediatype="tabloid")

#absolute numbers
absolute_migration_tabloid <- c(migration_sd_tabloid,migration_s_tabloid, migration_m_tabloid)
df_migration_absolute_tabloid<- data.frame(Column1 = parties, Column2= absolute_migration_tabloid)
colnames(df_migration_absolute_tabloid) <- c("parties","migration")

#Fusing mediatypes
df_migration_per_mediatype = rbind(df_migration_per_svt,
                                   df_migration_per_qualitative,
                                   df_migration_per_tabloid)

#relevel 
df_migration_per_mediatype$parties <- factor(df_migration_per_mediatype$parties, levels = c("s","sd", "m"))
df_migration_per_mediatype$mediatype <- factor(df_migration_per_mediatype$mediatype,
                                               levels = c("state_finansed","qualitative", "tabloid"))

#Visualising 
ggplot(df_migration_per_mediatype, aes(x=mediatype, y=percentages_migration, fill=parties))+
  geom_bar(stat = "identity", 
           position = position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        title="Immigration Topic",
        subtitle= "Share of Each Party",
        fill = "")+
  geom_text(aes(label=paste0(round(percentages_migration,2)*100, "%")),
            vjust=-0.5, 
            position=position_dodge(width=0.9),
            size=4,
            fontface="bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.6))+
  scale_x_discrete (labels=c("State-Financed",
                             "Broadsheet",
                             "Tabloid"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6",
                              "sd"="#00c5cf"),
                    labels=c("Social Democratic Party",
                             "SD Party",
                             "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave("figures/15_immigration_mediatypes.png", width = 6, height = 4)


#visualise differences
#extract percentage state-financed
s_migration_dif_svt <- df_migration_per_svt$percentages[df_migration_per_svt$parties == "s"] - df_migration_per_svt$percentages[df_migration_per_svt$parties == "sd"]
m_migration_dif_svt <- df_migration_per_svt$percentages[df_migration_per_svt$parties == "m"] - df_migration_per_svt$percentages[df_migration_per_svt$parties == "sd"]
#extract percentage qualtiative
s_migration_dif_qualitative <- df_migration_per_qualitative$percentages[df_migration_per_qualitative$parties == "s"] - df_migration_per_qualitative$percentages[df_migration_per_tabloid$parties == "sd"]
m_migration_dif_qualitative <- df_migration_per_qualitative$percentages[df_migration_per_qualitative$parties == "m"] - df_migration_per_tabloid$percentages[df_migration_per_tabloid$parties == "sd"]
#extract percentage tabloid
s_migration_dif_tabloid <- df_migration_per_tabloid$percentages[df_migration_per_tabloid$parties == "s"] - df_migration_per_tabloid$percentages[df_migration_per_tabloid$parties == "sd"]
m_migration_dif_tabloid<- df_migration_per_tabloid$percentages[df_migration_per_tabloid$parties == "m"] - df_migration_per_tabloid$percentages[df_migration_per_tabloid$parties == "sd"]

#create df
df_migration_dif_media <- data.frame(
  parties = c("s","m",
              "s", "m",
              "s", "m" ),
  difference = c(s_migration_dif_svt, m_migration_dif_svt,
                 s_migration_dif_qualitative,m_migration_dif_qualitative,
                 s_migration_dif_tabloid,m_migration_dif_tabloid),
  mediatype = c("svt","svt",
                "qualitative", "qualitative",
                "tabloid", "tabloid")
)
#relevel
df_migration_dif_media$parties <- factor(df_migration_dif_media$parties, levels = c("s", "m"))
df_migration_dif_media$mediatype <- factor(df_migration_dif_media$mediatype, levels = c("svt", "qualitative", "tabloid"))

#Visualising 
ggplot(df_migration_dif_media, aes(x=mediatype, y=difference, fill=parties))+
  geom_bar(stat = "identity", position=position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        title="Immigration Topic",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = ifelse(difference > 0, paste0("+", round(difference, 2) * 100, "%"), 
                               paste0(round(difference, 2) * 100, "%"))),
            vjust = 2,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.25, 0.05))+
  scale_x_discrete (labels=c("State-Financed",
                             "Broadsheet",
                             "Tabloid"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6"),
                    labels = c("Social Democratic Party",
                               "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color = "black",
             size = 1.5)
ggsave("figures/16_immigration_mediatypes_diff.png", width = 6, height = 3.5)


##----------------------------------------------Binominal Regression Analysis 

#---svt
#filter
df_migration_reg_long_svt <- df_migration_reg_long %>% 
  filter(mediatype=="state_finansed")

#model
model_migration_svt <- glm(migration ~ party, 	
                           family = binomial(), data = df_migration_reg_long_svt)
#modelsummary
summary(model_migration_svt)

#----qualitative
#filter
df_migration_reg_long_qualitative <- df_migration_reg_long %>% 
  filter(mediatype=="qualitative")
#model
model_migration_qualitative <- glm(migration ~ party, 	
                                   family = binomial(), data = df_migration_reg_long_qualitative)
#modelsummary
summary(model_migration_qualitative)

#----tabloid
#filter
df_migration_reg_long_tabloid <- df_migration_reg_long %>% 
  filter(mediatype=="tabloid")
#model
model_migration_tabloid <- glm(migration ~ party, 	
                               family = binomial(), data = df_migration_reg_long_tabloid)
#modelsummary
summary(model_migration_tabloid)

#STARGAZER svt & qualitative & tabloid
stargazer(model_migration_svt,model_migration_qualitative,model_migration_tabloid,
          type="html",
          out="model_migration_mediatype.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("Immigration"),
          column.labels = c("<b>State-<br>Financed",
                            "<b>Broadsheet",
                            "<b>Tabloid"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Binominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_migration_mediatype.htm")

#Predicted Probabilities 
#predicted propabilities svt
df_migration_reg_long_svt$predicted_prob <- predict(model_migration_svt, type = "response")
#Calculation for 95%confidence intervall
log_odds_migration_svt <- predict(model_migration_svt, type = "link", se.fit = TRUE)
p_migration_svt <- df_migration_reg_long_svt$predicted_prob
derivative_migration_svt = p_migration_svt * (1 - p_migration_svt)
se_prob_migration_svt <- log_odds_migration_svt$se.fit * derivative_migration_svt
z_value <- qnorm(0.975) 
df_migration_reg_long_svt$ci_lower <- df_migration_reg_long_svt$predicted_prob - z_value * se_prob_migration_svt
df_migration_reg_long_svt$ci_upper <- df_migration_reg_long_svt$predicted_prob + z_value * se_prob_migration_svt


#predicted propabilities qualitative
df_migration_reg_long_qualitative$predicted_prob <- predict(model_migration_qualitative, type = "response")
#Calculation for 95%confidence intervall
log_odds_migration_qualitative <- predict(model_migration_qualitative, type = "link", se.fit = TRUE)
p_migration_qualitative <- df_migration_reg_long_qualitative$predicted_prob
derivative_migration_qualitative = p_migration_qualitative * (1 - p_migration_qualitative)
se_prob_migration_qualitative <- log_odds_migration_qualitative$se.fit * derivative_migration_qualitative
z_value <- qnorm(0.975) 
df_migration_reg_long_qualitative$ci_lower <- df_migration_reg_long_qualitative$predicted_prob - z_value * se_prob_migration_qualitative
df_migration_reg_long_qualitative$ci_upper <- df_migration_reg_long_qualitative$predicted_prob + z_value * se_prob_migration_qualitative

#predicted propabilities tabloid
df_migration_reg_long_tabloid$predicted_prob <- predict(model_migration_tabloid, type = "response")
#Calculation for 95%confidence intervall
log_odds_migration_tabloid <- predict(model_migration_tabloid, type = "link", se.fit = TRUE)
p_migration_tabloid <- df_migration_reg_long_tabloid$predicted_prob
derivative_migration_tabloid = p_migration_tabloid * (1 - p_migration_tabloid)
se_prob_migration_tabloid <- log_odds_migration_tabloid$se.fit * derivative_migration_tabloid
z_value <- qnorm(0.975) 
df_migration_reg_long_tabloid$ci_lower <- df_migration_reg_long_tabloid$predicted_prob - z_value * se_prob_migration_tabloid
df_migration_reg_long_tabloid$ci_upper <- df_migration_reg_long_tabloid$predicted_prob + z_value * se_prob_migration_tabloid

#svt

#sd
#pp
migration_svt_sd_pp <- df_migration_reg_long_svt %>%
  filter(party == "sd") %>%
  pull(predicted_prob) %>% 
  first()
#ci upper
migration_svt_sd_upper <- df_migration_reg_long_svt %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_svt_sd_lower <- df_migration_reg_long_svt %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()

#s
#pp
migration_svt_s_pp <- df_migration_reg_long_svt %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_svt_s_upper <- df_migration_reg_long_svt %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_svt_s_lower <- df_migration_reg_long_svt %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()

#m
#pp
migration_svt_m_pp <- df_migration_reg_long_svt %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_svt_m_upper <- df_migration_reg_long_svt %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_svt_m_lower <- df_migration_reg_long_svt %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#qualitative

#sd
#pp
migration_qualitative_sd_pp <- df_migration_reg_long_qualitative %>%
  filter(party == "sd") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_qualitative_sd_upper <- df_migration_reg_long_qualitative %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_qualitative_sd_lower <- df_migration_reg_long_qualitative %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()
#s
#pp
migration_qualitative_s_pp <- df_migration_reg_long_qualitative %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_qualitative_s_upper <- df_migration_reg_long_qualitative %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_qualitative_s_lower <- df_migration_reg_long_qualitative %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()
#m
#pp
migration_qualitative_m_pp <- df_migration_reg_long_qualitative %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_qualitative_m_upper <- df_migration_reg_long_qualitative %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_qualitative_m_lower <- df_migration_reg_long_qualitative %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#tabloid

#sd
#pp
migration_tabloid_sd_pp <- df_migration_reg_long_tabloid %>%
  filter(party == "sd") %>%
  pull(predicted_prob) %>% 
  first()
#ci upper
migration_tabloid_sd_upper <- df_migration_reg_long_tabloid %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_tabloid_sd_lower <- df_migration_reg_long_tabloid %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()

#s
#pp
migration_tabloid_s_pp <- df_migration_reg_long_tabloid %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_tabloid_s_upper <- df_migration_reg_long_tabloid %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_tabloid_s_lower <- df_migration_reg_long_tabloid %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()

#m
#pp
migration_tabloid_m_pp <- df_migration_reg_long_tabloid %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
migration_tabloid_m_upper <- df_migration_reg_long_tabloid %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
migration_tabloid_m_lower <- df_migration_reg_long_tabloid %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#fuse
migration_mediatype_pp <- c(migration_svt_sd_pp,
                            migration_svt_s_pp,
                            migration_svt_m_pp,
                            migration_qualitative_sd_pp,
                            migration_qualitative_s_pp,
                            migration_qualitative_m_pp,
                            migration_tabloid_sd_pp,
                            migration_tabloid_s_pp,
                            migration_tabloid_m_pp)
migration_mediatype_upper <- c(migration_svt_sd_upper,
                               migration_svt_s_upper,
                               migration_svt_m_upper,
                               migration_qualitative_sd_upper,
                               migration_qualitative_s_upper,
                               migration_qualitative_m_upper,
                               migration_tabloid_sd_upper,
                               migration_tabloid_s_upper,
                               migration_tabloid_m_upper)
migration_mediatype_lower <- c(migration_svt_sd_lower,
                               migration_svt_s_lower,
                               migration_svt_m_lower,
                               migration_qualitative_sd_lower,
                               migration_qualitative_s_lower,
                               migration_qualitative_m_lower,
                               migration_tabloid_sd_lower,
                               migration_tabloid_s_lower,
                               migration_tabloid_m_lower)
migration_mediatype <- c("svt",
                         "svt",
                         "svt",
                         "qualitative",
                         "qualitative",
                         "qualitative",
                         "tabloid",
                         "tabloid",
                         "tabloid")
migration_mediatype_party <- c("sd",
                               "s",
                               "m",
                               "sd",
                               "s",
                               "m",
                               "sd",
                               "s",
                               "m")
df_migration_pp_mediatype <- data.frame(Column1= migration_mediatype_pp,
                                        Column2 = migration_mediatype_upper,
                                        Column3= migration_mediatype_lower,
                                        Column4= migration_mediatype,
                                        Column4 = migration_mediatype_party)
colnames(df_migration_pp_mediatype)<- c("predicted_prob",
                                        "ci_upper",
                                        "ci_lower",
                                        "mediatype",
                                        "party")
#relevel
df_migration_pp_mediatype$party <- factor(df_migration_pp_mediatype$party, levels = c("s", "sd", "m"))
df_migration_pp_mediatype$mediatype <- factor(df_migration_pp_mediatype$mediatype, levels = c("svt",
                                                                                          "qualitative",
                                                                                          "tabloid"))

ggplot(df_migration_pp_mediatype, aes(x = mediatype,
                                        y = predicted_prob,
                                        color= party,
                                        group=party)) +
  geom_point(size=7,
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_lower,
                    ymax = ci_upper),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Media Type",
       y = "Predicted Probability",
       title = "Immigration Topic",
       subtitle = "With 95% Confidence Interval",
       color= "")+
  scale_x_discrete(labels = c("State-Financed",
                              "Broadsheet",
                              "Tabloid"))+
  geom_text(aes(label = paste0(round(predicted_prob, 4)*100, "%")),  
            vjust = -1.3, 
            hjust = -0.4, 
            size = 3,
            color="black",
            position=position_dodge(width=0.9)) +
  scale_y_continuous(limits = c(0.2,0.6),
                     labels = scales::percent_format())+
  scale_color_manual(values= c("m"="#5e5e5e",
                               "s"="#c6c6c6",
                               "sd"="#00c5cf"),
                     labels= c("Social Democratic Party",
                               "SD Party",
                               "Moderate Party"))+
  theme_minimal()+
  theme (legend.position = "bottom")

ggsave("figures/17_immigration_mediatype_pp.png", width = 7, height = 3.5)

