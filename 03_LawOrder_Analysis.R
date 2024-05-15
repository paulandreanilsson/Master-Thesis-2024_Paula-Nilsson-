

#This is the code for the analysis concerning the law & order topic in Chapter 7.3

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)#to change format
library(stargazer)#for tables
library(reshape2)#for melting 
library(effects)  #for multinominal predicted prob
library(scales)#for scale

df <- read_excel("04-21-24_df_headings_sen.xlsx")

###################################################Law & Order
#RESEARCH QUESTION 1 

#----------------------------------------------VISUALISATION
##-------------------------------Law & Order
df_autho<- df %>% 
  filter(autho==TRUE)
autho <- sum(df_autho$autho)

#sd
df_autho_sd <- df_autho %>% 
  filter(sd==TRUE)
autho_sd = sum(df_autho_sd$sd)
autho_sd_per = sum(df_autho_sd$sd)/autho
#s
df_autho_s <- df_autho %>% 
  filter(s==TRUE)
autho_s = sum(df_autho_s$s)
autho_s_per = sum(df_autho_s$s)/autho

#m
df_autho_m <- df_autho %>% 
  filter(m==TRUE)
autho_m = sum(df_autho_m$m)
autho_m_per = sum(df_autho_m$m)/autho

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_autho <- c(autho_sd_per,autho_s_per, autho_m_per )
df_autho_per<- data.frame(Column1 = parties, Column2= percentages_autho)
colnames(df_autho_per) <- c("parties","percentages_autho")
#absolute 
absolute_autho<- c(autho_sd,autho_s, autho_m)
df_autho_absolute<- data.frame(Column1 = parties, Column2= absolute_autho)
colnames(df_autho_absolute) <- c("parties","autho")

#relevel 
df_autho_per$parties <- factor(df_autho_per$parties, levels = c("s", "sd", "m"))

#Visualising 
ggplot(df_autho_per, aes(x=parties,
                         y=percentages_autho,
                         fill=parties))+
  geom_bar(stat = "identity")+
  labs (x= "Party",
        y= "Percentage",
        title="Law & Order Topic",
        subtitle = "Share of Each Party")+
  geom_text(aes(label = paste0(round(percentages_autho,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.5))+
  scale_x_discrete (labels=c("Social Democratic Party",
                             "SD Party",
                             "Moderate Party"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6",
                              "sd"="#00c5cf"))+
  theme_minimal()+
  theme(legend.position = "none")
ggsave("figures/18_autho.png", width = 6, height = 3)


#visualise differences

#extract percentage
s_autho_dif <- df_autho_per$percentages[df_autho_per$parties == "s"] - df_autho_per$percentages[df_autho_per$parties == "sd"]
m_autho_dif <- df_autho_per$percentages[df_autho_per$parties == "m"] - df_autho_per$percentages[df_autho_per$parties == "sd"]
#create df
df_autho_dif <- data.frame(
  parties = c("s", "m"),
  difference = c(s_autho_dif, m_autho_dif)
)
#relevel
df_autho_dif$parties <- factor(df_autho_dif$parties, levels = c("s", "m"))

#Visualising 
ggplot(df_autho_dif, aes(x=parties, y=difference, fill=parties))+
  geom_bar(stat = "identity")+
  labs (x= "Parties",
        y= "Percentage",
        title="Law & Order Topic",
        subtitle = "Relative Differences with SD Party",
        fill = "Parties")+
  geom_text(aes(label = paste0("+",round(difference,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.05, 0.15))+
  scale_x_discrete (labels=c("Social Democratic\nParty",
                             "Moderate\nParty"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6"))+
  theme_minimal()+
  theme(legend.position = "none")+
  geom_hline(yintercept = 0,
             color = "black",
             size = 1.5)  
ggsave("figures/19_autho_diff.png", width = 6, height = 3)


#----------------------------------------------BINOMINAL REGRESSION ANALYSIS

#-------------------------------AUTHO
#data preparation 
df_autho_reg <- df_autho %>% 
  select(mediatype, electionyear, s, m, sd, english_headline_intro)
df_autho_reg_long <- df_autho_reg %>%
  pivot_longer(cols = c(s, sd, m), names_to = "party", values_to = "autho")

#change variables into factors
df_autho_reg_long <- df_autho_reg_long %>% 
  mutate(party = as.factor(party),
         autho= as.factor(autho))
#reference category
df_autho_reg_long$party <- relevel(df_autho_reg_long$party, ref="sd")

#creating model 
model_autho <- glm(autho ~ party, 	
                   family = binomial(), data = df_autho_reg_long)
#modelsummary
summary(model_autho)


#stargazer
stargazer(model_autho,
          type="html",
          out="model_autho.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("Law and Order"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Binominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_autho.htm")


#predicted propabilities 
df_autho_reg_long$predicted_prob <- predict(model_autho, type = "response")
#Calculation for 95%confidence intervall
log_odds_autho <- predict(model_autho, type = "link", se.fit = TRUE)
p_autho <- df_autho_reg_long$predicted_prob
derivative_autho = p_autho * (1 - p_autho)
se_prob_autho <- log_odds_autho$se.fit * derivative_autho
z_value <- qnorm(0.975) 
df_autho_reg_long$ci_lower <- df_autho_reg_long$predicted_prob - z_value * se_prob_autho
df_autho_reg_long$ci_upper <- df_autho_reg_long$predicted_prob + z_value * se_prob_autho

#relevel 
df_autho_reg_long_graph <-df_autho_reg_long 
df_autho_reg_long_graph$party <- factor(df_autho_reg_long_graph$party, levels = c("s", "sd", "m"))

#display predicted propabilities
ggplot(df_autho_reg_long_graph, aes(x = party, y = predicted_prob, color= party)) +
  geom_point(alpha = 0.5,  size=7) +
  geom_errorbar(aes(ymin = ci_lower,
                    ymax = ci_upper),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  labs(x = "Party",
       y = "Predcited Probability",
       subtitle = "With 95% Confidence Interval",
       title = "Law & Order Topic")+
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
  scale_color_manual(values= c("m"="#5e5e5e",
                               "s"="#c6c6c6",
                               "sd"="#00c5cf"))+
  theme_minimal()+
  theme (legend.position = "none")

ggsave("figures/20_laworder_pp.png", width = 6, height = 3)


#Subsidiary Question 1

#----------------------------------------------VISUALISATION

#2018 
#Data Preparation 
df_autho_2018 <- df_autho %>% 
  filter(electionyear==2018)
autho_2018 <- sum(df_autho_2018$autho)

#sd
df_autho_sd_2018 <- df_autho_2018 %>% 
  filter(sd==TRUE)
autho_sd_2018 = sum(df_autho_sd_2018$sd)
autho_sd_per_2018 = sum(df_autho_sd_2018$sd)/autho_2018
#s
df_autho_s_2018 <- df_autho_2018 %>% 
  filter(s==TRUE)
autho_s_2018 = sum(df_autho_s_2018$s)
autho_s_per_2018 = sum(df_autho_s_2018$s)/autho_2018
#m
df_autho_m_2018 <- df_autho_2018 %>% 
  filter(m==TRUE)
autho_m_2018 = sum(df_autho_m_2018$m)
autho_m_per_2018 = sum(df_autho_m_2018$m)/autho_2018

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_autho_2018 <- c(autho_sd_per_2018,autho_s_per_2018, autho_m_per_2018 )
df_autho_per_2018<- data.frame(Column1 = parties, Column2= percentages_autho_2018)
colnames(df_autho_per_2018) <- c("parties","percentages_autho")
#add election year 
df_autho_per_2018 <- df_autho_per_2018 %>% 
  mutate(electionyear=2018)

#absolute numbers
absolute_autho_2018 <- c(autho_sd_2018,autho_s_2018, autho_m_2018)
df_autho_absolute_2018<- data.frame(Column1 = parties, Column2= absolute_autho_2018)
colnames(df_autho_absolute_2018) <- c("parties","autho")

#2022 
#Data Preparation 
df_autho_2022 <- df_autho %>% 
  filter(electionyear==2022)
autho_2022 <- sum(df_autho_2022$autho)

#sd
df_autho_sd_2022 <- df_autho_2022 %>% 
  filter(sd==TRUE)
autho_sd_2022 = sum(df_autho_sd_2022$sd)
autho_sd_per_2022 = sum(df_autho_sd_2022$sd)/autho_2022
#s
df_autho_s_2022 <- df_autho_2022 %>% 
  filter(s==TRUE)
autho_s_2022 = sum(df_autho_s_2022$s)
autho_s_per_2022 = sum(df_autho_s_2022$s)/autho_2022
#m
df_autho_m_2022 <- df_autho_2022 %>% 
  filter(m==TRUE)
autho_m_2022 = sum(df_autho_m_2022$m)
autho_m_per_2022 = sum(df_autho_m_2022$m)/autho_2022

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_autho_2022 <- c(autho_sd_per_2022,autho_s_per_2022, autho_m_per_2022 )
df_autho_per_2022<- data.frame(Column1 = parties, Column2= percentages_autho_2022)
colnames(df_autho_per_2022) <- c("parties","percentages_autho")
#add election year 
df_autho_per_2022 <- df_autho_per_2022 %>% 
  mutate(electionyear=2022)

#absolute numbers
absolute_autho_2022 <- c(autho_sd_2022,autho_s_2022, autho_m_2022)
df_autho_absolute_2022<- data.frame(Column1 = parties, Column2= absolute_autho_2022)
colnames(df_autho_absolute_2022) <- c("parties","autho")

#fuse 2018 and 2022
df_autho_per_2018_2022 <- rbind(df_autho_per_2018, df_autho_per_2022)
df_autho_per_2018_2022<- df_autho_per_2018_2022 %>% 
  mutate(electionyear=as.factor(electionyear))


df_autho_per_2018_2022$parties <- factor(df_autho_per_2018_2022$parties, levels = c("s","sd", "m"))

#Visualising 
ggplot(df_autho_per_2018_2022, aes(x=electionyear, y=percentages_autho, fill=parties))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Election Year",
        y= "Percentage",
        title="Law & Order Topic",
        subtitle="Share of Each Party",
        fill = "")+
  geom_text(aes(label = paste0(round(percentages_autho,2)*100, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.5))+
  scale_x_discrete (labels=c("2018", "2022"))+
  scale_fill_manual(values= c("m"="#5e5e5e",
                              "s"="#c6c6c6",
                              
                              "sd"="#00c5cf"),
                    labels= c("Social Democratic Party",
                              "SD Party",
                              "Moderate Party"))+
  theme_minimal()+
  theme(legend.position="bottom")

ggsave("figures/21_autho_election.png", width = 6, height = 3.6)

#visualise differences
#extract percentage 2018
s_autho_dif_2018 <- df_autho_per_2018$percentages[df_autho_per_2018$parties == "s"] - df_autho_per_2018$percentages[df_autho_per$parties == "sd"]
m_autho_dif_2018 <- df_autho_per_2018$percentages[df_autho_per_2018$parties == "m"] - df_autho_per_2018$percentages[df_autho_per$parties == "sd"]
#extract percentage 2022
s_autho_dif_2022 <- df_autho_per_2022$percentages[df_autho_per_2022$parties == "s"] - df_autho_per_2022$percentages[df_autho_per$parties == "sd"]
m_autho_dif_2022 <- df_autho_per_2022$percentages[df_autho_per_2022$parties == "m"] - df_autho_per_2022$percentages[df_autho_per$parties == "sd"]


#create df
df_autho_dif_election <- data.frame(
  parties = c("s", "m","s", "m" ),
  difference = c(s_autho_dif_2018, m_autho_dif_2018,s_autho_dif_2022,m_autho_dif_2022),
  electionyear = c("2018", "2018", "2022", "2022"))
#relevel
df_autho_dif_election$parties <- factor(df_autho_dif_election$parties, levels = c("s", "m"))

#Visualising 
ggplot(df_autho_dif_election, aes(x=electionyear, y=difference, fill=parties))+
  geom_bar(stat = "identity", position=position_dodge())+
  labs (x= "Election",
        y= "Percentage",
        title="Law & Order Topic",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = paste0("+",round(difference,2)*100, "%")),
            vjust = - 0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.05, 0.25))+
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
ggsave("figures/22_autho_election_diff.png", width = 6, height = 3)


#----------------------------------------------Binominal Regression Analysis
#-------------------------------AUTHO


#---2018
#DATA PREPARATION
df_autho_reg_long_2018 <- df_autho_reg_long %>% 
  filter(electionyear==2018)

#CREATING MODEL
model_autho_2018 <- glm(autho ~ party, 	
                        family = binomial(), data = df_autho_reg_long_2018)
#modelsummary
summary(model_autho_2018)


#----2022
#DATA PREPARATION
df_autho_reg_long_2022 <- df_autho_reg_long %>% 
  filter(electionyear==2022)
#CREATING MODEL
model_autho_2022 <- glm(autho ~ party, 	
                        family = binomial(), data = df_autho_reg_long_2022)
#modelsummary
summary(model_autho_2022)


#STARGAZER 2018 & 2022
stargazer(model_autho_2018,model_autho_2022,
          type="html",
          out="model_autho_electionyears.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("Law and Order"),
          column.labels = c("<b>Election<br>2018",
                            "<b>Election<br>2022"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Binominal Regression Analysis; Log(Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_autho_electionyears.htm")

#predicted propabilities 
#2018
df_autho_reg_long_2018$predicted_prob <- predict(model_autho_2018, type = "response")
#Calculation for 95%confidence intervall
log_odds_autho_2018 <- predict(model_autho_2018, type = "link", se.fit = TRUE)
p_autho_2018 <- df_autho_reg_long_2018$predicted_prob
derivative_autho_2018 = p_autho_2018 * (1 - p_autho_2018)
se_prob_autho_2018 <- log_odds_autho_2018$se.fit * derivative_autho_2018
z_value <- qnorm(0.975) 
df_autho_reg_long_2018$ci_lower <- df_autho_reg_long_2018$predicted_prob - z_value * se_prob_autho_2018
df_autho_reg_long_2018$ci_upper <- df_autho_reg_long_2018$predicted_prob + z_value * se_prob_autho_2018

#2022
df_autho_reg_long_2022$predicted_prob <- predict(model_autho_2022, type = "response")
#Calculation for 95%confidence intervall
log_odds_autho_2022 <- predict(model_autho_2022, type = "link", se.fit = TRUE)
p_autho_2022 <- df_autho_reg_long_2022$predicted_prob
derivative_autho_2022 = p_autho_2022 * (1 - p_autho_2022)
se_prob_autho_2022 <- log_odds_autho_2022$se.fit * derivative_autho_2022
z_value <- qnorm(0.975) 
df_autho_reg_long_2022$ci_lower <- df_autho_reg_long_2022$predicted_prob - z_value * se_prob_autho_2022
df_autho_reg_long_2022$ci_upper <- df_autho_reg_long_2022$predicted_prob + z_value * se_prob_autho_2022

#create dataset for plot 
#2018

#sd
#pp
autho_2018_sd_pp <- df_autho_reg_long_2018 %>%
  filter(party == "sd") %>%
  pull(predicted_prob) %>% 
  first()
#ci upper
autho_2018_sd_upper <- df_autho_reg_long_2018 %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_2018_sd_lower <- df_autho_reg_long_2018 %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()

#s
#pp
autho_2018_s_pp <- df_autho_reg_long_2018 %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_2018_s_upper <- df_autho_reg_long_2018 %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_2018_s_lower <- df_autho_reg_long_2018 %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()

#m
#pp
autho_2018_m_pp <- df_autho_reg_long_2018 %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_2018_m_upper <- df_autho_reg_long_2018 %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_2018_m_lower <- df_autho_reg_long_2018 %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#2022

#sd
#pp
autho_2022_sd_pp <- df_autho_reg_long_2022 %>%
  filter(party == "sd") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_2022_sd_upper <- df_autho_reg_long_2022 %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_2022_sd_lower <- df_autho_reg_long_2022 %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()
#s
#pp
autho_2022_s_pp <- df_autho_reg_long_2022 %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_2022_s_upper <- df_autho_reg_long_2022 %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_2022_s_lower <- df_autho_reg_long_2022 %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()
#m
#pp
autho_2022_m_pp <- df_autho_reg_long_2022 %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_2022_m_upper <- df_autho_reg_long_2022 %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_2022_m_lower <- df_autho_reg_long_2022 %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()
autho_election_pp <- c(autho_2018_sd_pp,
                       autho_2018_s_pp,
                       autho_2018_m_pp,
                       autho_2022_sd_pp,
                       autho_2022_s_pp,
                       autho_2022_m_pp)
autho_election_upper <- c(autho_2018_sd_upper,
                          autho_2018_s_upper,
                          autho_2018_m_upper,
                          autho_2022_sd_upper,
                          autho_2022_s_upper,
                          autho_2022_m_upper)
autho_election_lower <- c(autho_2018_sd_lower,
                          autho_2018_s_lower,
                          autho_2018_m_lower,
                          autho_2022_sd_lower,
                          autho_2022_s_lower,
                          autho_2022_m_lower)
autho_election <- c("2018",
                    "2018",
                    "2018",
                    "2022",
                    "2022",
                    "2022")
autho_election_party <- c("sd",
                          "s",
                          "m",
                          "sd",
                          "s",
                          "m")
df_autho_pp_election <- data.frame(Column1= autho_election_pp,
                                   Column2 = autho_election_upper,
                                   Column3= autho_election_lower,
                                   Column4= autho_election,
                                   Column4 = autho_election_party)
colnames(df_autho_pp_election)<- c("predicted_prob",
                                   "ci_upper",
                                   "ci_lower",
                                   "electionyear",
                                   "party")
df_autho_pp_election$party <- factor(df_autho_pp_election$party, levels = c("s", "sd", "m"))

#display predicted propabilities
ggplot(df_autho_pp_election, aes(x = electionyear,
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
       title = "Law & Order Topic",
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
ggsave("figures/23_autho_election_pp.png", width = 6, height = 3.5)


#Research Questions 3 

##----------------------------------------------Visualisation 

#svt
#Data Preparation 
df_autho_svt <- df_autho %>% 
  filter(mediatype=='state_finansed')
autho_svt <- sum(df_autho_svt$autho)

#sd
df_autho_sd_svt <- df_autho_svt %>% 
  filter(sd==TRUE)
autho_sd_svt = sum(df_autho_sd_svt$sd)
autho_sd_per_svt = sum(df_autho_sd_svt$sd)/autho_svt
#s
df_autho_s_svt <- df_autho_svt %>% 
  filter(s==TRUE)
autho_s_svt = sum(df_autho_s_svt$s)
autho_s_per_svt = sum(df_autho_s_svt$s)/autho_svt
#m
df_autho_m_svt <- df_autho_svt %>% 
  filter(m==TRUE)
autho_m_svt = sum(df_autho_m_svt$m)
autho_m_per_svt = sum(df_autho_m_svt$m)/autho_svt

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_autho_svt <- c(autho_sd_per_svt,autho_s_per_svt, autho_m_per_svt )
df_autho_per_svt<- data.frame(Column1 = parties, Column2= percentages_autho_svt)
colnames(df_autho_per_svt) <- c("parties","percentages_autho")
df_autho_per_svt <- df_autho_per_svt %>% 
  mutate(mediatype="state_finansed")

#absolute numbers
absolute_autho_svt <- c(autho_sd_svt,autho_s_svt, autho_m_svt)
df_autho_absolute_svt<- data.frame(Column1 = parties, Column2= absolute_autho_svt)
colnames(df_autho_absolute_svt) <- c("parties","autho")

#Qualitative
#Data Preparation 
df_autho_qualitative <- df_autho %>% 
  filter(mediatype=='qualitative')
autho_qualitative <- sum(df_autho_qualitative$autho)

#sd
df_autho_sd_qualitative <- df_autho_qualitative %>% 
  filter(sd==TRUE)
autho_sd_qualitative = sum(df_autho_sd_qualitative$sd)
autho_sd_per_qualitative = sum(df_autho_sd_qualitative$sd)/autho_qualitative
#s
df_autho_s_qualitative <- df_autho_qualitative %>% 
  filter(s==TRUE)
autho_s_qualitative = sum(df_autho_s_qualitative$s)
autho_s_per_qualitative = sum(df_autho_s_qualitative$s)/autho_qualitative
#m
df_autho_m_qualitative <- df_autho_qualitative %>% 
  filter(m==TRUE)
autho_m_qualitative = sum(df_autho_m_qualitative$m)
autho_m_per_qualitative = sum(df_autho_m_qualitative$m)/autho_qualitative

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_autho_qualitative <- c(autho_sd_per_qualitative,autho_s_per_qualitative, autho_m_per_qualitative )
df_autho_per_qualitative<- data.frame(Column1 = parties, Column2= percentages_autho_qualitative)
colnames(df_autho_per_qualitative) <- c("parties","percentages_autho")
df_autho_per_qualitative <- df_autho_per_qualitative %>% 
  mutate(mediatype="qualitative")

#absolute numbers
absolute_autho_qualitative <- c(autho_sd_qualitative,autho_s_qualitative, autho_m_qualitative)
df_autho_absolute_qualitative<- data.frame(Column1 = parties, Column2= absolute_autho_qualitative)
colnames(df_autho_absolute_qualitative) <- c("parties","autho")

#tabloid
#Data Preparation 
df_autho_tabloid <- df_autho %>% 
  filter(mediatype=='tabloid')
autho_tabloid <- sum(df_autho_tabloid$autho)

#sd
df_autho_sd_tabloid <- df_autho_tabloid %>% 
  filter(sd==TRUE)
autho_sd_tabloid = sum(df_autho_sd_tabloid$sd)
autho_sd_per_tabloid = sum(df_autho_sd_tabloid$sd)/autho_tabloid
#s
df_autho_s_tabloid <- df_autho_tabloid %>% 
  filter(s==TRUE)
autho_s_tabloid = sum(df_autho_s_tabloid$s)
autho_s_per_tabloid = sum(df_autho_s_tabloid$s)/autho_tabloid
#m
df_autho_m_tabloid <- df_autho_tabloid %>% 
  filter(m==TRUE)
autho_m_tabloid = sum(df_autho_m_tabloid$m)
autho_m_per_tabloid = sum(df_autho_m_tabloid$m)/autho_tabloid

#dataframe 
parties <- c("sd", "s", "m")
#percentages
percentages_autho_tabloid <- c(autho_sd_per_tabloid,autho_s_per_tabloid, autho_m_per_tabloid )
df_autho_per_tabloid<- data.frame(Column1 = parties, Column2= percentages_autho_tabloid)
colnames(df_autho_per_tabloid) <- c("parties","percentages_autho")
df_autho_per_tabloid <- df_autho_per_tabloid %>% 
  mutate(mediatype = "tabloid")

#absolute numbers
absolute_autho_tabloid <- c(autho_sd_tabloid,autho_s_tabloid, autho_m_tabloid)
df_autho_absolute_tabloid<- data.frame(Column1 = parties, Column2= absolute_autho_tabloid)
colnames(df_autho_absolute_tabloid) <- c("parties","autho")

#fuse mediatypes
df_autho_per_mediatypes <- rbind(df_autho_per_svt, df_autho_per_qualitative, df_autho_per_tabloid)

#relevel
df_autho_per_mediatypes$parties <- factor(df_autho_per_mediatypes$parties, levels = c("s","sd", "m"))
df_autho_per_mediatypes$mediatype <- factor(df_autho_per_mediatypes$mediatype, levels = c("state_finansed","qualitative", "tabloid"))

#Visualising 
ggplot(df_autho_per_mediatypes, aes(x=mediatype, y=percentages_autho, fill=parties))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        title="Law & Order Topic",
        subtitle= "Share of Each Party",
        fill = "")+
  geom_text(aes(label = paste0(round(percentages_autho,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.5))+
  scale_x_discrete (labels=c("State Finansed",
                             "Broadsheet",
                             "Tabloid"))+
  scale_fill_manual(values= c("m"="#5e5e5e", "s"="#c6c6c6", "sd"="#00c5cf"),
                    labels= c("Social Democratic Party",
                              "SD Party", 
                              "Moderate Party"))+
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave("figures/24_autho_mediatypes.png", width = 6, height = 4)


#visualise differences
#extract percentage state-financed
s_autho_dif_svt <- df_autho_per_svt$percentages[df_autho_per_svt$parties == "s"] - df_autho_per_svt$percentages[df_autho_per_svt$parties == "sd"]
m_autho_dif_svt <- df_autho_per_svt$percentages[df_autho_per_svt$parties == "m"] - df_autho_per_svt$percentages[df_autho_per_svt$parties == "sd"]
#extract percentage qualtiative
s_autho_dif_qualitative <- df_autho_per_qualitative$percentages[df_autho_per_qualitative$parties == "s"] - df_autho_per_qualitative$percentages[df_autho_per_tabloid$parties == "sd"]
m_autho_dif_qualitative <- df_autho_per_qualitative$percentages[df_autho_per_qualitative$parties == "m"] - df_autho_per_tabloid$percentages[df_autho_per_tabloid$parties == "sd"]
#extract percentage tabloid
s_autho_dif_tabloid <- df_autho_per_tabloid$percentages[df_autho_per_tabloid$parties == "s"] - df_autho_per_tabloid$percentages[df_autho_per_tabloid$parties == "sd"]
m_autho_dif_tabloid<- df_autho_per_tabloid$percentages[df_autho_per_tabloid$parties == "m"] - df_autho_per_tabloid$percentages[df_autho_per_tabloid$parties == "sd"]

#create df
df_autho_dif_media <- data.frame(
  parties = c("s","m",
              "s", "m",
              "s", "m" ),
  difference = c(s_autho_dif_svt, m_autho_dif_svt,
                 s_autho_dif_qualitative,m_autho_dif_qualitative,
                 s_autho_dif_tabloid,m_autho_dif_tabloid),
  mediatype = c("svt","svt",
                "qualitative", "qualitative",
                "tabloid", "tabloid"))
#relevel
df_autho_dif_media$parties <- factor(df_autho_dif_media$parties, levels = c("s", "m"))
df_autho_dif_media$mediatype <- factor(df_autho_dif_media$mediatype, levels = c("svt", "qualitative", "tabloid"))

#Visualising 
ggplot(df_autho_dif_media, aes(x=mediatype, y=difference, fill=parties))+
  geom_bar(stat = "identity", position=position_dodge())+
  labs (x= "Media Type",
        y= "Percentage",
        title="Law & Order Topic",
        subtitle = "Relative Differences with SD Party",
        fill = "")+
  geom_text(aes(label = paste0("+",round(difference,2)*100, "%")),
            vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4,
            fontface = "bold")+
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(-0.05, 0.2))+
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
ggsave("figures/25_autho_mediatypes_diff.png", width = 6, height = 3.5)

##----------------------------------------------Binominal Regression Analysis 
#-------------------------------AUTHO
#---svt
#DATA PREPARATION
df_autho_reg_long_svt <- df_autho_reg_long %>% 
  filter(mediatype=="state_finansed")

#CREATING MODEL
model_autho_svt <- glm(autho ~ party, 	
                       family = binomial(), data = df_autho_reg_long_svt)
#modelsummary
summary(model_autho_svt)

#----qualitative
#DATA PREPARATION

df_autho_reg_long_qualitative <- df_autho_reg_long %>% 
  filter(mediatype=="qualitative")
#CREATING MODEL
model_autho_qualitative <- glm(autho ~ party, 	
                               family = binomial(), data = df_autho_reg_long_qualitative)
#modelsummary
summary(model_autho_qualitative)

#----tabloid
#DATA PREPARATION
df_autho_reg_long_tabloid <- df_autho_reg_long %>% 
  filter(mediatype=="tabloid")
#CREATING MODEL
model_autho_tabloid <- glm(autho ~ party, 	
                           family = binomial(), data = df_autho_reg_long_tabloid)
#modelsummary
summary(model_autho_tabloid)

#STARGAZER svt & qualitative & tabloid
stargazer(model_autho_svt,model_autho_qualitative,model_autho_tabloid,
          type="html",
          out="model_autho_mediatypes.htm",
          intercept.bottom = FALSE,
          dep.var.labels = c("Law and Order"),
          column.labels = c("<b>State-<br>Financed",
                            "<b>Broadsheet",
                            "<b>Tabloid"),
          covariate.labels = c("Intercept",
                               "Moderate Party (SD Party=0)",
                               "Social Democratic Party (SD Party=0)"),
          title="Binominal Regression Analysis, Log (Odds)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.numbers =TRUE)
browseURL("model_autho_mediatypes.htm")


#predicted propabilities 
#svt
df_autho_reg_long_svt$predicted_prob <- predict(model_autho_svt, type = "response")
#Calculation for 95%confidence intervall
log_odds_autho_svt <- predict(model_autho_svt, type = "link", se.fit = TRUE)
p_autho_svt <- df_autho_reg_long_svt$predicted_prob
derivative_autho_svt = p_autho_svt * (1 - p_autho_svt)
se_prob_autho_svt <- log_odds_autho_svt$se.fit * derivative_autho_svt
z_value <- qnorm(0.975) 
df_autho_reg_long_svt$ci_lower <- df_autho_reg_long_svt$predicted_prob - z_value * se_prob_autho_svt
df_autho_reg_long_svt$ci_upper <- df_autho_reg_long_svt$predicted_prob + z_value * se_prob_autho_svt

#qualitative
df_autho_reg_long_qualitative$predicted_prob <- predict(model_autho_qualitative, type = "response")
#Calculation for 95%confidence intervall
log_odds_autho_qualitative <- predict(model_autho_qualitative, type = "link", se.fit = TRUE)
p_autho_qualitative <- df_autho_reg_long_qualitative$predicted_prob
derivative_autho_qualitative = p_autho_qualitative * (1 - p_autho_qualitative)
se_prob_autho_qualitative <- log_odds_autho_qualitative$se.fit * derivative_autho_qualitative
z_value <- qnorm(0.975) 
df_autho_reg_long_qualitative$ci_lower <- df_autho_reg_long_qualitative$predicted_prob - z_value * se_prob_autho_qualitative
df_autho_reg_long_qualitative$ci_upper <- df_autho_reg_long_qualitative$predicted_prob + z_value * se_prob_autho_qualitative

#tabloid
df_autho_reg_long_tabloid$predicted_prob <- predict(model_autho_tabloid, type = "response")
#Calculation for 95%confidence intervall
log_odds_autho_tabloid <- predict(model_autho_tabloid, type = "link", se.fit = TRUE)
p_autho_tabloid <- df_autho_reg_long_tabloid$predicted_prob
derivative_autho_tabloid = p_autho_tabloid * (1 - p_autho_tabloid)
se_prob_autho_tabloid <- log_odds_autho_tabloid$se.fit * derivative_autho_tabloid
z_value <- qnorm(0.975) 
df_autho_reg_long_tabloid$ci_lower <- df_autho_reg_long_tabloid$predicted_prob - z_value * se_prob_autho_tabloid
df_autho_reg_long_tabloid$ci_upper <- df_autho_reg_long_tabloid$predicted_prob + z_value * se_prob_autho_tabloid

#svt

#sd
#pp
autho_svt_sd_pp <- df_autho_reg_long_svt %>%
  filter(party == "sd") %>%
  pull(predicted_prob) %>% 
  first()
#ci upper
autho_svt_sd_upper <- df_autho_reg_long_svt %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_svt_sd_lower <- df_autho_reg_long_svt %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()

#s
#pp
autho_svt_s_pp <- df_autho_reg_long_svt %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_svt_s_upper <- df_autho_reg_long_svt %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_svt_s_lower <- df_autho_reg_long_svt %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()

#m
#pp
autho_svt_m_pp <- df_autho_reg_long_svt %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_svt_m_upper <- df_autho_reg_long_svt %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_svt_m_lower <- df_autho_reg_long_svt %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#qualitative

#sd
#pp
autho_qualitative_sd_pp <- df_autho_reg_long_qualitative %>%
  filter(party == "sd") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_qualitative_sd_upper <- df_autho_reg_long_qualitative %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_qualitative_sd_lower <- df_autho_reg_long_qualitative %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()
#s
#pp
autho_qualitative_s_pp <- df_autho_reg_long_qualitative %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_qualitative_s_upper <- df_autho_reg_long_qualitative %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_qualitative_s_lower <- df_autho_reg_long_qualitative %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()
#m
#pp
autho_qualitative_m_pp <- df_autho_reg_long_qualitative %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_qualitative_m_upper <- df_autho_reg_long_qualitative %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_qualitative_m_lower <- df_autho_reg_long_qualitative %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#tabloid

#sd
#pp
autho_tabloid_sd_pp <- df_autho_reg_long_tabloid %>%
  filter(party == "sd") %>%
  pull(predicted_prob) %>% 
  first()
#ci upper
autho_tabloid_sd_upper <- df_autho_reg_long_tabloid %>%
  filter(party == "sd") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_tabloid_sd_lower <- df_autho_reg_long_tabloid %>%
  filter(party == "sd") %>%
  pull(ci_lower)%>% 
  first()

#s
#pp
autho_tabloid_s_pp <- df_autho_reg_long_tabloid %>%
  filter(party == "s") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_tabloid_s_upper <- df_autho_reg_long_tabloid %>%
  filter(party == "s") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_tabloid_s_lower <- df_autho_reg_long_tabloid %>%
  filter(party == "s") %>%
  pull(ci_lower)%>% 
  first()

#m
#pp
autho_tabloid_m_pp <- df_autho_reg_long_tabloid %>%
  filter(party == "m") %>%
  pull(predicted_prob)%>% 
  first()
#ci upper
autho_tabloid_m_upper <- df_autho_reg_long_tabloid %>%
  filter(party == "m") %>%
  pull(ci_upper)%>% 
  first()
# ci lower 
autho_tabloid_m_lower <- df_autho_reg_long_tabloid %>%
  filter(party == "m") %>%
  pull(ci_lower)%>% 
  first()

#fuse
autho_mediatype_pp <- c(autho_svt_sd_pp,
                        autho_svt_s_pp,
                        autho_svt_m_pp,
                        autho_qualitative_sd_pp,
                        autho_qualitative_s_pp,
                        autho_qualitative_m_pp,
                        autho_tabloid_sd_pp,
                        autho_tabloid_s_pp,
                        autho_tabloid_m_pp)
autho_mediatype_upper <- c(autho_svt_sd_upper,
                           autho_svt_s_upper,
                           autho_svt_m_upper,
                           autho_qualitative_sd_upper,
                           autho_qualitative_s_upper,
                           autho_qualitative_m_upper,
                           autho_tabloid_sd_upper,
                           autho_tabloid_s_upper,
                           autho_tabloid_m_upper)
autho_mediatype_lower <- c(autho_svt_sd_lower,
                           autho_svt_s_lower,
                           autho_svt_m_lower,
                           autho_qualitative_sd_lower,
                           autho_qualitative_s_lower,
                           autho_qualitative_m_lower,
                           autho_tabloid_sd_lower,
                           autho_tabloid_s_lower,
                           autho_tabloid_m_lower)
autho_mediatype <- c("svt",
                     "svt",
                     "svt",
                     "qualitative",
                     "qualitative",
                     "qualitative",
                     "tabloid",
                     "tabloid",
                     "tabloid")
autho_mediatype_party <- c("sd",
                           "s",
                           "m",
                           "sd",
                           "s",
                           "m",
                           "sd",
                           "s",
                           "m")
df_autho_pp_mediatype <- data.frame(Column1= autho_mediatype_pp,
                                    Column2 = autho_mediatype_upper,
                                    Column3= autho_mediatype_lower,
                                    Column4= autho_mediatype,
                                    Column4 = autho_mediatype_party)
colnames(df_autho_pp_mediatype)<- c("predicted_prob",
                                    "ci_upper",
                                    "ci_lower",
                                    "mediatype",
                                    "party")
#relevel
df_autho_pp_mediatype$party <- factor(df_autho_pp_mediatype$party, levels = c("s", "sd", "m"))
df_autho_pp_mediatype$mediatype <- factor(df_autho_pp_mediatype$mediatype, levels = c("svt",
                                                                                      "qualitative",
                                                                                      "tabloid"))

ggplot(df_autho_pp_mediatype, aes(x = mediatype,
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
       title = "Law & Order Topic",
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

ggsave("figures/26_autho_mediatype_pp.png", width = 7, height = 3.5)

