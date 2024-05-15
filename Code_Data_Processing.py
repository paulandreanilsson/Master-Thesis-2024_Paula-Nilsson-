# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 11:23:54 2024

@author: Paula
"""

#IMPORT GENERAL LIBRARIES
import pandas as pd
import re
import os
import numpy as np

#set working directory
path = "..."
os.chdir(path)

#-----------------------------load raw data
df_raw = pd.read_excel("df.xlsx", header=0)

#assigning each row unique ID
df_raw['ID'] = range(1, len(df_raw) + 1)
#-----------------------------------------------------------remove dates
df_raw = df_raw[(df_raw['date']<'2018-09-10')|(df_raw['date']>'2018-09-20')]
df_raw = df_raw[(df_raw['date']<"2022-09-12")]
#check how many from which sources
source_counts = df_raw['sourceName'].value_counts()
print(source_counts)

#-----------------------------FILTERING HEADLINES --------FILTER 1

#REDEFINE RETRIEVER SEARCH STRING
#filter more specific M SD S socialdemokrat, moderaterna, sverigedemokrat 
df_1 = df_raw[
    (df_raw['text'].str.contains(r'socialdemokrat|moderaterna|sverigedemokrat', case=False)) | \
    (df_raw['text'].str.contains(r'\b(?:M|SD|S)\b', case=True)) | \
    df_raw['headline'].str.contains(r'socialdemokrat|moderaterna|sverigedemokrat', case=False) | \
    (df_raw['headline'].str.contains(r'\b(?:M|SD|S)\b', case=True)) | \
    df_raw['intro'].str.contains(r'socialdemokrat|moderaterna|sverigedemokrat', case=False) | \
    (df_raw['intro'].str.contains(r'\b(?:M|SD|S)\b', case=True))]
#save df_1    
#df_1.to_excel('df_1.xlsx', index=0)


#REMOVE UNNECESSARY INFORMATION    
#remove daytime pattern string    
strings_daytime_pattern = [
    "Premium Näringsliv 2017-09-29 15.07 Kontrollerad och lite fyrkantig, så är finansminister Magdalena Andersson när SvD följer",
    "2017-09-21 16.02 Han var komikern som var gift med en partiledare. När han äntligen",
    "Premium Kultur 2017-10-31 13.27",
    "Premium Sverige 2017-12-30 13.28 Mitt under brinnande regeringskris besöker Elisabeth Svantesson Köpenhamn. Moderaternas skuggfinansminister bygger nätverk",
    "Premium Sverige 2017-12-25 07.00 Illegala kopieringsmaskiner, stödaktioner. När Berlinmuren föll reste handelsstudenten Magdalena Andersson (S) till",
    "Kultur 2018-01-27 08.00 Sveket genomsyrar hela befolkningen här.",
    "Sverige 2018-02-09 14.59 Trots rekordsysselsättning ser det mörkt ut för Socialdemokraternas främsta vallöfte - att",
    "Kultur 2018-02-18 10.00 Sverige hade ett nära militärt samarbete med USA. Och Europas socialdemokratiska ledare",
    "Sverige 2018-02-12 10.48 Om det blir upplopp i Husby igen vill Jimmie Åkesson sätta in",
    "Premium Sverige 2018-02-23 10.39 I Danmark vill Socialdemokraterna att högst 30 procent i varje bostadsområde ska",
    "Premium Sverige 2018-04-14 06.45 I publiken sitter invandrare och avhoppade moderater när Jimmie Åkesson är på",
    "Kultur 2018-04-29 08.00 När Stefan Löfven valde sin regering var det killarna som gick vidare",
    "Litteratur 2018-05-03 21.15 Kristina Sandberg skapade romanfiguren Maj. Nu har Lena Andersson skrivit om Ragnar",
    "Premium Kultur 2018-05-01 06.45 Hur skulle samhället förändras om Socialdemokraterna fick egen majoritet? Kungafamiljen sitter kvar",
    "2018-05-12 13.53 Hans jobb: Skipa fred mellan grannar där polisen är portad. SvD har",
    "Premium Sverige 2018-08-04 20.00 M-ledarens fru har hjälpt Svenska Akademien med krishanteringen under våren, sitter i",
    "Sverige 2018-08-29 19.59 Moderaterna vill stärka flyget och bygga ut kapaciteten på Arlanda. Men hur",
    "Sverige 2018-08-27 12.23 Kan Sverigedemokraterna vinna över kristna väljare från Kristdemokraterna? SvD besöker showen i",
    "Premium Sverige 2018-08-23 20.00 Sverige bör gå med i euron och det snart - en åsikt Jan",
    "Premium Sverige 2018-08-22 20.00 Sverigedemokraterna vill låsa in asylsökande som inte kan styrka sin identitet i",
    "Sverige 2018-09-05 20.01 Hur ska den akuta bristen på vårdpersonal lösas - när utbildning av",
    "Sverige 2018-09-15 06.45 Nådde Sverigedemokraterna sin peak i valet eller har vi bara sett början"
    ]

#remove date & time pattern
for string in strings_daytime_pattern:
    df_1['headline'] = df_1['headline'].str.replace(string, '', regex=False)       

#remove Anderssons
strings_andersson = [
    'Pernilla Andersson',
    'Lena Andersson',
    'Elisabet Andersson',
    'Fredrik Andersson',
    'Kjell Andersson',
    'P-G Andersson',
    'Ulla Andersson',
    'Janne Andersson']
#apply Andersson strings
for string in strings_andersson:
    df_1['headline']= df_1['headline'].str.replace(string,'', regex=False)
    
#create election year column
def electionyear (date):
    comparison_date = pd.Timestamp('2018-09-11')
    if date <= comparison_date:
        return '2018'
    else:
        return '2022'
df_1['electionyear'] = df_1['date'].apply(electionyear)       

#FILTERTING STEP 2 
#Define strings to filter for each party
#socialdemokrat
s_andersson = r'\bandersson'
s_other = r'socialdemokrat|\blöfven'
s_S = r'\(S\)|\sS\s|\sS-|\sS:\sS,'
s_S_start = r'^S\s|^S-|^S:|^S,'
s_S_end = r'\sS$'

#moderate
m_batra = r'\bbatra kinberg'
m_other = r'moderaterna|\bkristersson'
m_M = r'\(M\)|\sM\s|\sM-|:M\s|\sM:|\sM,'
m_M_start = r'^M\s|^M-|^M:|^M,'
m_M_end = r'\sM$'

#swedishdemokrat
sd_other = r'sverigedemokrat|\bÅkesson'
sd_SD = r'\(SD\)|\sSD\s|\sSD-|:SD\s|\sSD:|\sSD,'
sd_SD_start = r'^SD\s|^SD-|^SD:|^SD,'
sd_SD_end = r'\sSD$'

#filter for parties in headline 
df_headings = df_1[
     df_1['headline'].str.contains(s_other, case=False)| 
     df_1['headline'].str.contains(m_other, case=False)| 
     df_1['headline'].str.contains(sd_other, case=False)|
     df_1['headline'].str.contains(s_S, case=True)| 
     df_1['headline'].str.contains(s_S_start, case=True)| 
     df_1['headline'].str.contains(s_S_end, case=True)| 
     df_1['headline'].str.contains(m_M, case=True)| 
     df_1['headline'].str.contains(m_M_start, case=True)| 
     df_1['headline'].str.contains(m_M_end, case=True)| 
     df_1['headline'].str.contains(sd_SD, case=True)|
     df_1['headline'].str.contains(sd_SD_start, case=True)|
     df_1['headline'].str.contains(sd_SD_end, case=True)|
     (df_1['headline'].str.contains(s_andersson, case=False) &
     (df_1['electionyear'] == 2022))|
     (df_1['headline'].str.contains(m_batra, case=False) &
     (df_1['electionyear'] == 2018))
     ]

#-------------------------------------------------DATA CLEANING    

#check H&M
filtered_df1 = df_headings[df_headings['headline'].str.contains(r'H&M', case=True)]
ID_HM = filtered_df1['ID'].tolist()
#remove all H&M rows 
mask = ~df_headings['ID'].isin(ID_HM)
df_headings = df_headings[mask]

#check ...
filtered_df2 = df_headings[df_headings['headline'].str.contains(r"\.\.\.|\…", na=False)]
#filtered_df2.to_excel('filtered_df/filtered_df2.xlsx', index=False) # in total 223 
newspaper_counts = filtered_df2['sourceName'].value_counts()
# Print the counts of each unique value in the 'newspaper' column
print(newspaper_counts)
df_headings = df_headings[~df_headings['headline'].str.contains(r"\.\.\.|\…", na=False)]

#check whether there are direct duplicates
df_headings_duplicates = df_headings.drop (labels='ID', axis=1).copy()
duplicate_rows = df_headings_duplicates.duplicated().sum()
print(f"Number of duplicate rows: {duplicate_rows}")
#no they are none 

#manually add 5 rows that contain :S 
df_Sspecial = df_1.loc[df_1['ID'].isin([73301, 80946, 85526, 101262, 110918])].copy()
#Fuse all int one dataframe
df_headings = pd.concat([df_headings, df_Sspecial], ignore_index=True)                       

#check if time pattern is there
time_pattern_count = df_headings['headline'].str.contains(r'\d{2}.\d{2}', regex=True).sum()
# Print the count
print("Number of rows containing the time pattern:", time_pattern_count)
# Filter the DataFrame to select rows where the title contains the time pattern
rows_with_time = df_headings[df_headings['headline'].str.contains(r'\d{2}.\d{2}', regex=True)]
# Print the filtered rows
print("Rows containing the time pattern:")
print(rows_with_time['ID'])

strings_time_pattern = [
    '14.00:',
    '06.46:',
    '15.30:',
    '11.45:',
    '00:19',
    '04:46',
    '00:54',
    '00:41',
    '02:15'
    ]
#remove time pattern 
for string in strings_time_pattern:
    df_headings['headline']= df_headings['headline'].str.replace(string, '', regex=False)

#remove other unnecessary strings
strings_beginning = [
    "Analyser:",
    "analys:",
    "live:", 
    "Just nu:",
    "POLITIK:",
    "extra!"
    ]

#apply removal for strings at the beginning of titles
for string in strings_beginning:
    df_headings['headline'] = df_headings['headline'].str.replace(r'^' + string, '', case= False, regex=True)

#-------------------------------------DATA WRANGLING

#drop certain unnecessary columns 
df_headings= df_headings.drop(columns= ['sourceId', 'author', 'page'])

#rename column name 
df_headings.rename(columns={'sourceName':'newspaper_version'}, inplace=True)

#create newspaper column
df_headings ['newspaper']= df_headings ['newspaper_version']
df_headings['newspaper'] = df_headings ['newspaper']. replace(['Aftonbladet','Aftonbladet Plus'], "aftonbladet")
df_headings['newspaper'] = df_headings ['newspaper']. replace(['Svenska Dagbladet','Svenska Dagbladet Premium'], "svenskadagbladet")
df_headings['newspaper'] = df_headings ['newspaper']. replace('SVT Nyheter', "svt")
df_headings['newspaper'] = df_headings ['newspaper']. replace('Expressen', "expressen")
df_headings['newspaper'] = df_headings ['newspaper']. replace('Dagens Nyheter', "dagensnyheter")

#create mediatype column 
df_headings ['mediatype']= df_headings ['newspaper']
df_headings['mediatype'] = df_headings ['mediatype']. replace(['aftonbladet','expressen'], "tabloid")
df_headings['mediatype'] = df_headings ['mediatype']. replace(['svenskadagbladet','dagensnyheter'], "qualitative")
df_headings['mediatype'] = df_headings ['mediatype']. replace('svt', "state_finansed")


#create s, m, & sd column with true or false column
df_headings['s'] = (df_headings['headline'].str.contains(s_other, case=False, regex=True) |
                    df_headings['headline'].str.contains(s_S, case=True, regex=True)|
                    df_headings['headline'].str.contains(s_S_end, case=True, regex=True)|
                    df_headings['headline'].str.contains(s_S_start, case=True, regex=True))

df_headings['m'] = (df_headings['headline'].str.contains(m_other, case=False, regex=True) |
                    df_headings['headline'].str.contains(m_M, case=True, regex=True)|
                    df_headings['headline'].str.contains(m_M_end, case=True, regex=True)|
                    df_headings['headline'].str.contains(m_M_start, case=True, regex=True))

df_headings['sd'] = (df_headings['headline'].str.contains(sd_other, case=False, regex=True) |
                    df_headings['headline'].str.contains(sd_SD, case=True, regex=True)|
                    df_headings['headline'].str.contains(sd_SD_end, case=True, regex=True)|
                    df_headings['headline'].str.contains(sd_SD_start, case=True, regex=True))

#check how many from which sources
source_counts = df_headings['newspaper'].value_counts()
print(source_counts)


#-----------------------------------------------------------TRANSLATION of the titles 
import requests
import deepl

#translation tols 
auth_key = "1ab0e68d-2b96-4f6e-9dc0-81e01c16de80" 
translator = deepl.Translator(auth_key)


#translate everything
df_headings['english_headline']=df_headings['headline'].apply(lambda x: translator.translate_text(x, target_lang="EN-US").text)

df_headings.to_excel('03-28-24_df_headings_translated.xlsx', index=0)


#--------------------------------------------------------------PRETRAINED MODELS SENTIMENT

#-----------------------------------------SENTIMENT NEW SENTIMENT

#define entities  for Sentiment Analysis
entities_patterns = {
    'andersson': {'patterns':[r'\bandersson'],
                  'case_sensitive':False,
                  'electionyear':2022},
    'socialdemokrat': {'patterns': [r'social democrat|\blöfven'],
                       'case_sensitive':False},
    's': {'patterns': [r"\(S\)|\sS\s|\sS-|\sS:|\sS's|\sS,",
                     r"^S\s|^S-|^S:|^S's|^S,",
                     r"\sS$|\sS\."],
         'case_sensitive':True},
    'batra':{'patterns':[r'\bbatra kinberg'],
             'case_sensitive':False,
             'electionyear':2018},
    'moderate': {'patterns':[r'moderate|\bkristersson'],
                 'case_sensitive':False},
    'm': {'patterns':[r"\(M\)|\sM\s|\sM-|:M\s|\sM:|\sM's|\sM,",
                     r"^M\s|^M-|^M:|^M's|^M,",
                     r"\sM$|\sM\."],
          'case_sensitive':True},
    'swedishdemokrat': {'patterns': [r'sweden democrat|sverigedemokrat|\bÅkesson'],
                        'case_sensitive':False},
    'sd': {'patterns':[r"\(SD\)|\sSD\s|\sSD-|:SD\s|\sSD:|\sSD's|\sSD",
                      r"^SD\s|^SD-|^SD:|^SD's|^SD,",
                      r"\sSD$|\sSD\.$|\sSD?$|\sSDs$"],
           'case_sensitive':True}
    }

#function to check if any entity pattern matches the headline
def entity_match(headline, year):
    for party, info in entities_patterns.items():
        #Check year if applicable
        if 'electionyear' in info and info['electionyear'] != year:
            continue
        patterns = info['patterns']
        case_sensitive = info['case_sensitive']
        for pattern in patterns:
            regex_flag = 0 if case_sensitive else re.IGNORECASE
            if re.search(pattern, headline, regex_flag):
                return True
    return False

#list to keep track of rows with no entities detected
rows_with_no_entities = []

#iterate over df_headings
for index, row in df_headings.iterrows():
    headline = row['english_headline']
    year = row['electionyear']
    if not entity_match(headline, year):
        # If no entity matches, add row index to list
        rows_with_no_entities.append(index)

#display headings without entity
print("Headlines with no entities detected:")
for index in rows_with_no_entities:
    print(df_headings.loc[index, 'english_headline'])

#remove where entities are not detected 
df_headings.drop(rows_with_no_entities, inplace=True)

#install necessary packages for NewsSentiment
from NewsSentiment import TargetSentimentClassifier
#newssentiment 1.2.28 requires transformers<=4.24,>=4.17
tsc = TargetSentimentClassifier()
data = []

#create entities for NewsSentiment package
for index, row in df_headings.iterrows():
    IDnumber = row ['ID']
    headline = row['english_headline']  #access the headline text for the current row
    year = row['electionyear']  #access the year for the current row
    for party, info in entities_patterns.items():
        patterns = info['patterns']
        case_sensitive = info['case_sensitive']
        #check if a year is specified and if it matches the row's year; skip if not
        if 'electionyear' in info and info['electionyear'] != year:
            continue
        for pattern in patterns:
            if case_sensitive:
                matches = re.finditer(pattern, headline)
            else:
                matches = re.finditer(pattern, headline, re.IGNORECASE)
            for match in matches:
                start, end = match.span()
                prefix = headline[:start].strip()
                target = headline[start:end].strip()
                suffix = headline[end:].strip()
                # Append a tuple with the (prefix, target, suffix) for this match
                data.append((prefix, target, suffix, IDnumber, party))
                
#adjusting input without IDnumber and party
sentiment_input = [(prefix, target, suffix) for prefix, target, suffix, _, _ in data]
#creating list with id_number and party 
id_party_pairs = [(id_number, party) for _, _, _, id_number, party in data]

#apply model
sentiments = tsc.infer(targets=sentiment_input)

#empty list to store sentiemnt with highest prob
filtered_sentiments = []
for sentiment_tuple in sentiments:
    #initialize a variable to keep track of the dictionary with the highest class_prob
    highest_prob_dict = None
    for sentiment_dict in sentiment_tuple:
        #if highest_prob_dict is None or the current dictionary's class_prob is higher, update highest_prob_dict
        if highest_prob_dict is None or sentiment_dict['class_prob'] > highest_prob_dict['class_prob']:
            highest_prob_dict = sentiment_dict
    #after finding the dictionary with the highest class_prob in the tuple, add it to the result list
    filtered_sentiments.append(highest_prob_dict)
filtered_sentiments_with_ids_party = [(sentiment, id_num, party) for sentiment, (id_num, party) in zip(filtered_sentiments, id_party_pairs)]
#create df 
flattened_data = []
for (sentiment, id_num, party) in filtered_sentiments_with_ids_party:
    flattened_data.append({
        'ID': id_num,
        'entity': party, 
        'class_id': sentiment['class_id'],
        'class_label': sentiment['class_label'],
        'class_prob': sentiment['class_prob']
    })
df_sentiments = pd.DataFrame(flattened_data)

#adjust variable names 
df_sentiments ['party']= df_sentiments ['entity']
df_sentiments['party'] = df_sentiments ['party']. replace(['andersson','socialdemokrat','s'], "s_sentiment")
df_sentiments['party'] = df_sentiments ['party']. replace(['batra','moderate','m'], "m_sentiment")
df_sentiments['party'] = df_sentiments ['party']. replace(['swedishdemokrat','sd'], "sd_sentiment")

#create df with one ID for each row 
pivot_class_label = df_sentiments.pivot_table(index= 'ID', columns='party', values='class_label', aggfunc='first')
pivot_class_prob = df_sentiments.pivot_table(index= 'ID', columns='party', values='class_prob', aggfunc='first')
pivot_class_prob.columns = [f"{col}_prob" for col in pivot_class_prob.columns]

#merge into df and make ID as a column
df_sentiments_2 = pivot_class_label.join(pivot_class_prob)
df_sentiments_2.reset_index(inplace=True)

#merge df_sentiments_2 and df_headings into df_headings_sen
df_headings_sen= pd.merge(df_sentiments_2, df_headings, on='ID', how='inner')

#rearrange column order
order= ['ID',
        'electionyear',
        'newspaper',
        'mediatype',
        'headline',
        'english_headline',
        'm_sentiment',
        'm_sentiment_prob',
        'm',
        's_sentiment',
        's_sentiment_prob',
        's',
        'sd_sentiment',
        'sd_sentiment_prob',
        'sd',
        'newspaper_version',
        'date']
df_headings_sen=df_headings_sen[order]


#check if s, m, sd column coincide with content in column s/m/sd_sentiment

#-------------Moderate Party
#M True but m_sentiment missing
m_check1=df_headings_sen[(df_headings_sen['m']==True)&(df_headings_sen['m_sentiment'].isnull())]
condition_m_1 = (df_headings_sen['m'] == True) & (df_headings_sen['m_sentiment'].isnull())
df_headings_sen.loc[condition_m_1, 'm'] =False

#M False but m_sentiment value exists
m_check2=df_headings_sen[(df_headings_sen['m']==False)&(df_headings_sen['m_sentiment'].notnull())]
condition_m_2 = (df_headings_sen['m'] == False) & (df_headings_sen['m_sentiment'].notnull())
df_headings_sen.loc[condition_m_2, 'm'] =True

#-------------Social Democratic Party
#S True but s_sentiment missing
s_check1=df_headings_sen[(df_headings_sen['s']==True)&(df_headings_sen['s_sentiment'].isnull())]
condition_s_1 = (df_headings_sen['s'] == True) & (df_headings_sen['s_sentiment'].isnull())
df_headings_sen.loc[condition_s_1, 's']=False

#S False but s_sentiment value exists
s_check2_2=df_headings_sen[(df_headings_sen['s']==False)&(df_headings_sen['s_sentiment'].notnull())]
condition_s_2 = (df_headings_sen['s']==False) & (df_headings_sen['s_sentiment'].notnull())
df_headings_sen.loc[condition_s_2, 's'] =True

#-------------Swedish Democratic Party
#SD True but sd_sentiment missing
sd_check1_1=df_headings_sen[(df_headings_sen['sd']==True)&(df_headings_sen['sd_sentiment'].isnull())]
condition_sd_1 = (df_headings_sen['sd'] == True) & (df_headings_sen['sd_sentiment'].isnull())
df_headings_sen.loc[condition_sd_1,'sd']=False

#SD False but sd_sentiment value exists
sd_check2=df_headings_sen[(df_headings_sen['sd']==False)&(df_headings_sen['sd_sentiment'].notnull())]
#I need to correct \sSD\?$
patterns_sd = sd_check2[sd_check2['english_headline'].str.contains(r'\sSD?$', regex=True)]
id_numbers_patterns_sd =patterns_sd['ID'].tolist()
df_headings_sen.loc[df_headings_sen['ID'].isin(id_numbers_patterns_sd),['sd_sentiment','sd_sentiment_prob']]=pd.NA
sd_check2_2=df_headings_sen[(df_headings_sen['sd']==False)&(df_headings_sen['sd_sentiment'].notnull())]
#turn into True when value exists
condition_sd_2 = (df_headings_sen['sd'] == False) & (df_headings_sen['sd_sentiment'].notnull())
df_headings_sen.loc[condition_sd_2, 'sd'] =True

#checkpoint (this is also the dataset I want to use for the other sentiments evaluations)
df_headings_sen.to_excel('03-30-24_df_headings_sen02.xlsx', index=0)



#-----------------------------------------------------------CHECK & CLEAN INTROS


#systematically detect strings
#foto
filtered_df_3 = df_headings_sen[df_headings_sen['intro'].str.contains(r"Foto", na=False)]
filtered_df_3.to_excel('filtered_df/filtered_df3_sub_foto.xlsx', index=False) # in total 223 
#av:
filtered_df_4 = df_headings_sen[df_headings_sen['intro'].str.contains(r"Av:",case=False, na=False)]
filtered_df_4.to_excel('filtered_df/filtered_df3_sub_av.xlsx', index=False) 
#arkivbild
filtered_df_5 = df_headings_sen[df_headings_sen['intro'].str.contains(r"arkivbild",case=False, na=False)]
filtered_df_5.to_excel('filtered_df/filtered_df5_sub_arkivbild.xlsx', index=False)
#DBEATT
filtered_df_6 = df_headings_sen[df_headings_sen['intro'].str.contains(r"DEBATT",case=True, na=False)]
filtered_df_6.to_excel('filtered_df/filtered_df6_sub_DEBATT.xlsx', index=False) 
#analys
filtered_df_7 = df_headings_sen[df_headings_sen['intro'].str.contains(r"analys",case=False, na=False)]
filtered_df_7.to_excel('filtered_df/filtered_df7_analys.xlsx', index=False) 
#SLUTERPLIK
filtered_df_8 = df_headings_sen[df_headings_sen['intro'].str.contains(r"slutreplik",case=False, na=False)]
filtered_df_8.to_excel('filtered_df/filtered_df8_slutreplik.xlsx', index=False) 
#REPLIK
filtered_df_9 = df_headings_sen[df_headings_sen['intro'].str.contains(r"replik",case=False, na=False)]
filtered_df_9.to_excel('filtered_df/filtered_df8_replik.xlsx', index=False) 
#Afonbladet
filtered_df_10 = df_headings_sen[df_headings_sen['intro'].str.contains(r"aftonbladet",case=False, na=False)]
filtered_df_10.to_excel('filtered_df/filtered_df10_afton.xlsx', index=False) 

#-------------remove detected text strings after evaluating the results above 
#analys 
analys_string = r'^analys\b'
df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(analys_string, '',flags=re.IGNORECASE, regex=True)
#debatt
debatt_strings = [
    'DEBATT Detta är en debattartikel. Det är skribenten som står för åsikterna som förs fram i texten, inte Aftonbladet. REPLIK.',
    'DEBATT SLUTERPLIK.',
    'DEBATT 12 februari 2018 10:50 DEBATT.',
    'DEBATT Detta är en debattartikel.',
    'EKONOMISTUDION DEBATT.',
    'DEBATTARTIKEL',
    'DEBATTINLÄGG',
    'DEBATT.',
    'DEBATT/REPLIK',
    'DEBATT ']
for strings in debatt_strings:
    df_headings_sen['intro']= df_headings_sen['intro'].str.replace(strings, '', regex=False)
#Arkivbild
arkiv_string = [r'Arkivbild\.\b',
                r'\(Arkivbild\)\b']
for strings in arkiv_string:
    df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(strings, '', flags=re.IGNORECASE, regex=True)
#av
av_strings = [
    'Av: Oskar Forsberg , Beri Zangana',
    'Av: Oskar Forsberg , Anna Sjögren',
    'Av: John Granlund , Olof Svensson',
    'Av: Olof Svensson , John Granlund',
    'Av: Marcus Älverbrandt , Fanny Westling',
    'Av: Sebastian Comar Alm , Love Isakson Svensén',
    'Av: Olof Svensson , John Granlund',
    'Av: Hanna Olsson Berg , Ebba Thornéus , Matilda Aprea Malmqvist',
    'Av: Amanda Hällsten , Christoffer Nilsson',
    'Av: Joachim Kerpner',
    'Av: Anna Sjögren',
    'Av: Manne Berggren Wiklund',
    'Av: Josefine Karlsson',    
    'Av: Olof Svensson',
    'Av: Linda Hjertén',
    'Av: Marcus Älverbrandt',
    'Av: Olof Svensson',
    'Av: Fanny Westling',
    'Av: Mikaela Somnell',
    'Av: Susanna Nygren',
    'Av: Sebastian Comar',
    'Av: Jonatan Westerlind',
    'Av: Aftonbladets ledarsida är oberoende socialdemokratisk.',
    'Av: Malin Wollin',
    'Av: Joachim Kerpner',
    'Av: Sebastian Comar Alm',
    'Av: Joachim Kerpner',
    'Av: TT ']
for strings in av_strings:
    df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(strings, '', regex=False)
#foto
foto_strings = [
    '1 av 2 | Foto: Peter Wixtröm',
    '1 av 2 | Foto: Magnus Sandberg / FOTO: MAGNUS SANDBERG',
    '1 av 2 | Foto: BJÖRN LINDAHL',
    '1 av 4 | Foto: LASSE ALLARD',
    '1 av 2 | Foto: Daniel Ohlsson/TV4',
    '1 av 2 | Foto: URBAN ANDERSSON',
    'Foto: Timbro',
    'Foto: Henrik Montgomery/TT / TT',
    'Foto: Henrik Montgomery/TT',
    'Foto: Lasse Allard / AFTONBLADET / 2116',
    'Foto: Margareta Bloom Sandebäck / AFTONBLADET / 85657',
    'Foto: Emil Wesolowski / AFTONBLADET / 85777',
    'Foto: MARCUS ERICSSON',
    'Foto: Lasse Allard / AFTONBLADET / 2116',
    'Foto: Fredrik Sandberg/TT / TT',
    'Foto: Krister Hansson / AFTONBLADET / 3950',
    'Foto: TT NYHETSBYRÅN Hanna Wigh.',
    'Foto: ADAM IHSE / TT Hanna Wigh.',
    'Foto: JERKER IVARSSON/LASSE ALLARD',
    'Foto: Urban Andersson / AFTONBLADET / 4216',
    'Foto: TT/BJÖRN LINDAHL',
    'Foto: Jonas Bilberg / AFTONBLADET / 7690',
    'Foto: Johan Nilsson/TT ',
    'Foto: Bosse Schöns arkiv',
    'Foto: Robin Lorentz-Allard',
    'Foto: URB',
    'Foto: Janerik Henriksson/TT / TT',
    'Foto: Lasse Allard / AFTONBLADET',
    'Foto: Andreas Bardell',
    'Foto: TT/PRIVAT',
    'Foto: NILUFER DEMIR/TT',
    'Foto: Urban Andersson',
    'Foto: Emil Wesolowski / AFTONBLADET / 85777',
    'Foto: JIM BOURG / IBL',
    'Foto: Paul Wallander',
    'Foto: ROBERT NYBERG',
    'Foto: YOUSSEF BADAWI / EPA / TT / EPA TT NYHETSBYR N',
    'Foto: Privat',
    'Foto: SVENSKA DAGBLADET TT',
    'Foto: Björn Lindahl / AFTONBLADET',
    'Foto: Magnus Sandberg LEDARE',
    'Foto: Plainpicture',
    'Foto: Anna Tärnhuvud / AFTONBLADET / 85705',
    'Foto: Rene Schutze / AP TT',
    'Foto: Susanne Walström',
    'Foto: Anna Tärnhuvud / AFTONBLADET / 85705',
    'Foto: FOTO:LASSE ALLARD',
    'Foto: Paul Wallander LEDARE',
    'Foto: TT NYHETSBYRÅN LEDARE',
    'Foto: Kenneth Paulsson',
    'Foto: Lotte Fernvall',
    'Foto: LASSE ALLARD',
    'Foto: Urban Andersson',
    'Foto: Pernilla Zetterman',
    'Foto: Stefan Mattsson',
    'Foto: Illustration: Patrik Lindvall.',
    'Foto: TECKNING: PATRIK LINDVALL',
    'Foto: Robin Lorentz-Allard / AFTONBLADET / 85392',
    'Foto: Kicki Nilsson',
    'Foto: Ulf Höjer',
    'Foto: BJORN LINDAHL',
    'Foto: Stefan Mattsson',
    'Foto: Peter Wixtröm',
    'Foto: Jonas Ekströmer',
    'Foto: CLAUDIO BRESCIANI/TT',
    'Foto: Claudio Bresciani/TT / TT',
    'Foto: Stefan Mattsson / AFTONBLADET',
    'Foto: TT, PETER KNUTSON',
    'Foto: Pontus Lundahl/TT',
    'Foto: Mats Andersson / TT / TT',
    'Foto: PONTUS LUNDAHL / TT',
    'Foto: CAROLINA BYRMO / 85440',
    'Foto: Roksana Bashyrova',
    'Foto: Rene Schutze / AP TT NYHETSBYR N',
    'Foto: Thomas Johansson/TT Lars Hansson.',
    'Foto: BJÖRN LINDAHL',
    'Foto: Pontus Orre',
    'Foto: Johan Nilsson/TT / TT',
    'Foto: LOTTE FERNVALL',
    'Foto: NORA SAVOSNICK',
    'Foto: ORRE PONTUS/Aftonbladet/TT / TT',
    'Foto: Andreas Hillergren/TT',
    'Foto: Fredrik Sandberg/TT / TT',
    'Foto: Fredrik Sandberg/TT',
    'Foto: Pontus Lundahl/TT',
    'Foto: Henrik Montgomery/TT',
    'Foto: Urban Andersson',
    'Foto: NORA SAVOSNICK',
    'Foto: Johan Nilsson/TT / TT',
    'Foto: SVT',
    'Foto: AFTONBLADET',
    'Foto: TT '
    ]
for strings in foto_strings:
    df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(strings, '', regex=False)
    
#slutreplik
slutreplik_strings =[
    r'^SLUTREPLIK',
    r'\bSLUTREPLIK\.']
for strings in slutreplik_strings:
    df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(strings, '', regex=True)

#replik
replik_strings =[
    r'^REPLIK',
    r'\bREPLIK\.']    
for strings in replik_strings:
    df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(strings, '', regex=True)
    
#other text strings
sub_other_strings = [
    'LEDARE Aftonbladets ledarsida är oberoende socialdemokratisk.',
    'KULTUR Detta är en kulturartikel som är en del av Aftonbladets opinionsjournalistik.',
    'NYHETSBYRÅN', 
    'KOLUMNISTER',
    'Texten uppdateras.',
    'NYHETER',
    'LEDARE',
    'Aftonbladets ledarsida är oberoende socialdemokratisk.',
    '/ AFTONBLADET / 85392',
    '/ AFTONBLADET']    
for strings in sub_other_strings:
    df_headings_sen['intro'] = df_headings_sen['intro'].str.replace(strings, '', regex=False)

#-----------------------------------------------------------TRANSLATION INTROS
import requests
import deepl

#translation tols 
auth_key = "" 
translator = deepl.Translator(auth_key)
#translate everything
df_headings_sen['english_intro']=df_headings_sen['intro'].apply(lambda x: translator.translate_text(x, target_lang="EN-US").text)
#save translation
df_headings_sen.to_excel('04-04-24_df_headings_sen_translated.xlsx', index=0)

#--------------------------------------------------------------Remove Points
#check which intros are incomplete
filtered_df_11 = df_headings_sen[df_headings_sen['intro'].str.contains(r"\.\.\.|\…", na=False)]
filtered_df_11.to_excel('filtered_df/filtered_df11_points.xlsx', index=False) 
filtered_df_11['newspaper'].value_counts()
#copy column
df_headings_sen['english_intro_clean']= df_headings_sen['english_intro']
#remove points
point_string = r'\.\.\.|\…'
df_headings_sen['english_intro_clean'] = df_headings_sen['english_intro_clean'].str.replace(point_string, '', regex=True)


#-----------------------------------------------------------Apply BERT Topic Modellin to headline and intro
from bertopic import BERTopic

#fuse intro and headline 
df_headings_sen['english_headline_intro'] = df_headings_sen['english_headline'] + ' ' + df_headings_sen['english_intro_clean']

#convert the 'english_headline_intro' column to a list
headline_intro_list = df_headings_sen['english_headline_intro'].tolist()  

#instantiating BERTopic
topic_model = BERTopic(language="english", calculate_probabilities=True, verbose=True)

#apply model 
topics, probs = topic_model.fit_transform(headline_intro_list)
#create 
df_topics =pd.DataFrame({"topic":topics, "document": headline_intro_list})
#print into excel 
df_topics.to_excel("bert/04-05-2024_df_topics.xlsx", index=0)
 
#extracting topics (most frequent results)
freq = topic_model.get_topic_info()
#print into excel 
freq.to_excel("bert/04-05-2024_df_frequency_topics.xlsx", index=0)
freq.head(5)
#take a lot at a frequent topics 
topic_model.get_topic(0)  # Select the most frequent topic

#visualise topics
topic_model.visualize_topics().write_html("bert/topic_visualization2.html")
#how confident BERTtopic is regarding topics that are found
topic_model.visualize_distribution(probs[200], min_probability=0.015)


#-----------------------------------------------------------NORMALISE TEXT

#spacy for nlp analysis
import spacy #for lemmatising and stopwords
#load large model
nlp = spacy.load("en_core_web_lg") 
#en_core_web_lg -> for the large one
#define stopwords and punctuation 
stop = nlp.Defaults.stop_words #stopwords
def clean(doc):
    # tokenize the document using SpaCy
    doc = nlp(doc)
    # lowercase, remove stopwords and punctuation, and lemmatize
    normalised = " ".join(token.lemma_ for token in doc if token.text.lower() not in stop and not token.is_punct)
    return normalised
#apply function 
df_headings_sen['english_headline_intro_normal'] = df_headings_sen['english_headline_intro'].apply(clean)
#changing everything into lowercase
df_headings_sen['english_headline_intro_normal'] = df_headings_sen['english_headline_intro_normal'].str.lower()
#checkpoint
df_headings_sen.to_excel('04-05-24_df_headings_sen_intro_translated_normalised.xlsx', index=0)


#-----------------------------------------------------------CLUSTERING K Means 

#--------------Remove the following entities for clustering
df_headings_sen['english_headline_intro_noparty']=df_headings_sen['english_headline_intro']
entities_patterns = {
    'andersson': {'patterns':[r'\bandersson'],
                  'case_sensitive':False,
                  'electionyear':2022},
    'socialdemokrat': {'patterns': [r'social democrat|\blöfven'],
                       'case_sensitive':False},
    's': {'patterns': [r"\(S\)|\sS\s|\sS-|\sS:|\sS's|\sS,",
                     r"^S\s|^S-|^S:|^S's|^S,",
                     r"\sS$|\sS\."],
         'case_sensitive':True},
    'batra':{'patterns':[r'\bbatra kinberg'],
             'case_sensitive':False,
             'electionyear':2018},
    'moderate': {'patterns':[r'moderate|\bkristersson'],
                 'case_sensitive':False},
    'm': {'patterns':[r"\(M\)|\sM\s|\sM-|:M\s|\sM:|\sM's|\sM,",
                     r"^M\s|^M-|^M:|^M's|^M,",
                     r"\sM$|\sM\."],
          'case_sensitive':True},
    'swedishdemokrat': {'patterns': [r'sweden democrat|sverigedemokrat|\bÅkesson'],
                        'case_sensitive':False},
    'sd': {'patterns':[r"\(SD\)|\sSD\s|\sSD-|:SD\s|\sSD:|\sSD's|\sSD",
                      r"^SD\s|^SD-|^SD:|^SD's|^SD,",
                      r"\sSD$|\sSD\.$|\sSD\?$|\sSDs$"],
           'case_sensitive':True}
    }

#Function to compile regex patterns based on the case sensitivity
def compile_patterns(entity_info):
    compiled_patterns = []
    for pattern in entity_info['patterns']:
        if entity_info.get('case_sensitive'):
            compiled_patterns.append(re.compile(pattern))
        else:
            compiled_patterns.append(re.compile(pattern, re.IGNORECASE))
    return compiled_patterns
# Function to remove matches from a string
def remove_matches(text, compiled_patterns):
    for pattern in compiled_patterns:
        text = pattern.sub('', text)
    return text
# Compile all patterns first
all_compiled_patterns = {}
for entity, info in entities_patterns.items():
    all_compiled_patterns[entity] = compile_patterns(info)
# Apply the removal function to the DataFrame
for entity, compiled_patterns in all_compiled_patterns.items():
    df_headings_sen['english_headline_intro_noparty'] = df_headings_sen['english_headline_intro_noparty'].apply(lambda x: remove_matches(x, compiled_patterns))
#remove " 's"  
string = [r"\s\'s\s"]
df_headings_sen['english_headline_intro_noparty'] = df_headings_sen['english_headline_intro_noparty'].str.replace(string[0], '', regex=True)



#-------------NORMALISATION of the column
#spacy for nlp analysis
import spacy #for lemmatising and stopwords
#load large model
nlp = spacy.load("en_core_web_lg") 
#en_core_web_lg -> for the large one
#define stopwords and punctuation 
stop = nlp.Defaults.stop_words #stopwords
def clean(doc):
    # tokenize the document using SpaCy
    doc = nlp(doc)
    # lowercase, remove stopwords and punctuation, and lemmatize
    normalised = " ".join(token.lemma_ for token in doc if token.text.lower() not in stop and not token.is_punct)
    return normalised
#apply function 
df_headings_sen['english_headline_intro_noparty_normal'] = df_headings_sen['english_headline_intro_noparty'].apply(clean)
#changing everything into lowercase
df_headings_sen['english_headline_intro_noparty_normal'] = df_headings_sen['english_headline_intro_noparty_normal'].str.lower()
    
#----------------Create Clusters

#load packages
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
tfidf_vectorizer = TfidfVectorizer()

#------- transform data into tf_IDF matrix & show Tf_IDF for the data without entitiy
tfidf_matrix_party = tfidf_vectorizer.fit_transform(df_headings_sen['english_headline_intro_noparty_normal'])

#-------SILHOUETTE TEST party
from sklearn.metrics import silhouette_score
silhouette_scores = [] 
k_range = range(2, 20)
for k in k_range:
    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10) # Execute the k-means with the "k" number
    cluster_labels = kmeans.fit_predict(tfidf_matrix_party) #label clusters
    silhouette_avg = silhouette_score(tfidf_matrix_party, cluster_labels) #calculate the silhouette score
    silhouette_scores.append(silhouette_avg) # append the score to the list
    print(f"For n_clusters={k}, the silhouette score is {silhouette_avg}") # printing message
#find the optimal number of clusters with the highest silhouette score
optimal_k = k_range[silhouette_scores.index(max(silhouette_scores))] # define the optimal
print(f"\nOptimal number of clusters: {optimal_k}") 

#-------APPLY K-MEANS 7 party
kmeans_optimal_party = KMeans(n_clusters=10, random_state=42, n_init=10)
kmeans_optimal_party.fit(tfidf_matrix_party)
cluster_labels_optimal_party = kmeans_optimal_party.labels_ #label the data with the clusters
#assigning cluster label 
df_headings_sen['cluster_tdidf_party_7'] = cluster_labels_optimal_party #Add clusters to the original df
#-------PLOT CLUSTERS
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
# Reduce dimensionality of TF-IDF features to 2 dimensions using PCA
pca = PCA(n_components=2)
tfidf_matrix_2d = pca.fit_transform(tfidf_matrix_party.toarray())
# Plot the clusters
plt.figure(figsize=(10, 6))
plt.scatter(tfidf_matrix_2d[:, 0], tfidf_matrix_2d[:, 1], c=cluster_labels_optimal_party, cmap='viridis')
plt.title('2D PCA of TF-IDF Features with Cluster Labels')
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')
plt.colorbar(label='Cluster')
plt.show()
#-------top 20 words 
import numpy as np
# Retrieve the centroids of each cluster
centroids_optimal_party = kmeans_optimal_party.cluster_centers_
# Number of top words to display for each cluster
top_n_words = 20
# Get the indices of the top words for each centroid
top_word_indices_party = centroids_optimal_party.argsort(axis=1)[:, :-top_n_words-1:-1]
# Get the feature names (words) from the TF-IDF vectorizer
feature_names = np.array(tfidf_vectorizer.get_feature_names_out())
# Print the most frequent words for each cluster
for i, centroid in enumerate(top_word_indices_party):
    print(f"Cluster {i}:")
    for index in centroid:
        print(f"\t- {feature_names[index]}")
    print()

 
#------------------for each election

########################2022_party
#create dataset
df_headings_sen_2022_party= df_headings_sen[df_headings_sen['electionyear']==2022].copy()

#------- transform data into tf_IDF matrix & show Tf_IDF for the data 2022_party
tfidf_matrix_2022_party = tfidf_vectorizer.fit_transform(df_headings_sen_2022_party['english_headline_intro_noparty_normal'])

#-------SILHOUETTE TEST 2022_party
from sklearn.metrics import silhouette_score
silhouette_scores = [] 
k_range = range(2, 20)
for k in k_range:
    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10) # Execute the k-means with the "k" number
    cluster_labels = kmeans.fit_predict(tfidf_matrix_2022_party) #label clusters
    silhouette_avg = silhouette_score(tfidf_matrix_2022_party, cluster_labels) #calculate the silhouette score
    silhouette_scores.append(silhouette_avg) # append the score to the list
    print(f"For n_clusters={k}, the silhouette score is {silhouette_avg}") # printing message
#find the optimal number of clusters with the highest silhouette score
optimal_k = k_range[silhouette_scores.index(max(silhouette_scores))] # define the optimal
print(f"\nOptimal number of clusters: {optimal_k}") 

#-------APPLY K-MEANS 3 2022_party
kmeans_optimal_2022_party = KMeans(n_clusters=10, random_state=42, n_init=10)
kmeans_optimal_2022_party.fit(tfidf_matrix_2022_party)
cluster_labels_optimal_2022_party = kmeans_optimal_2022_party.labels_ #label the data with the clusters
#assigning cluster label 
df_headings_sen_2022_party['cluster_tdidf_party_3'] = cluster_labels_optimal_2022_party #Add clusters to the original df
#-------PLOT CLUSTERS
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
# Reduce dimensionality of TF-IDF features to 2 dimensions using PCA
pca = PCA(n_components=2)
tfidf_matrix_2d = pca.fit_transform(tfidf_matrix_2022_party.toarray())
# Plot the clusters
plt.figure(figsize=(10, 6))
plt.scatter(tfidf_matrix_2d[:, 0], tfidf_matrix_2d[:, 1], c=cluster_labels_optimal_2022_party, cmap='viridis')
plt.title('2D PCA of TF-IDF Features with Cluster Labels')
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')
plt.colorbar(label='Cluster')
plt.show()
#-------TOP 20 for each cluster in 2022
import numpy as np
# Retrieve the centroids of each cluster
centroids_optimal_2022_party = kmeans_optimal_2022_party.cluster_centers_
# Number of top words to display for each cluster
top_n_words = 20
# Get the indices of the top words for each centroid
top_word_indices_2022_party = centroids_optimal_2022_party.argsort(axis=1)[:, :-top_n_words-1:-1]
# Get the feature names (words) from the TF-IDF vectorizer
feature_names = np.array(tfidf_vectorizer.get_feature_names_out())
# Print the most frequent words for each cluster
for i, centroid in enumerate(top_word_indices_2022_party):
    print(f"Cluster {i}:")
    for index in centroid:
        print(f"\t- {feature_names[index]}")
    print()
    

########################2018_party
#create dataset
df_headings_sen_2018_party= df_headings_sen[df_headings_sen['electionyear']==2018].copy()

#-------transform data into tf_IDF matrix & show Tf_IDF for the data 2018_party
tfidf_matrix_2018_party = tfidf_vectorizer.fit_transform(df_headings_sen_2018_party['english_headline_intro_noparty_normal'])

#-------SILHOUETTE TEST 2018_party
from sklearn.metrics import silhouette_score
silhouette_scores = [] 
k_range = range(2, 20)
for k in k_range:
    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10) # Execute the k-means with the "k" number
    cluster_labels = kmeans.fit_predict(tfidf_matrix_2018_party) #label clusters
    silhouette_avg = silhouette_score(tfidf_matrix_2018_party, cluster_labels) #calculate the silhouette score
    silhouette_scores.append(silhouette_avg) # append the score to the list
    print(f"For n_clusters={k}, the silhouette score is {silhouette_avg}") # printing message
#find the optimal number of clusters with the highest silhouette score
optimal_k = k_range[silhouette_scores.index(max(silhouette_scores))] # define the optimal
print(f"\nOptimal number of clusters: {optimal_k}") 

#-------APPLY K-MEANS 8 2018_party
kmeans_optimal_2018_party = KMeans(n_clusters=9, random_state=42, n_init=10)
kmeans_optimal_2018_party.fit(tfidf_matrix_2018_party)
cluster_labels_optimal_2018_party = kmeans_optimal_2018_party.labels_ #label the data with the clusters
#assigning cluster label 
df_headings_sen_2018_party['cluster_tdidf_party_10'] = cluster_labels_optimal_2018_party #Add clusters to the original df
#-------PLOT CLUSTERS
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
# Reduce dimensionality of TF-IDF features to 2 dimensions using PCA
pca = PCA(n_components=2)
tfidf_matrix_2d = pca.fit_transform(tfidf_matrix_2018_party.toarray())
# Plot the clusters
plt.figure(figsize=(10, 6))
plt.scatter(tfidf_matrix_2d[:, 0], tfidf_matrix_2d[:, 1], c=cluster_labels_optimal_2018_party, cmap='viridis')
plt.title('2D PCA of TF-IDF Features with Cluster Labels')
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')
plt.colorbar(label='Cluster')
plt.show()
#-------top 20 words for each party 
import numpy as np
# Retrieve the centroids of each cluster
centroids_optimal_2018_party = kmeans_optimal_2018_party.cluster_centers_
# Number of top words to display for each cluster
top_n_words = 20
# Get the indices of the top words for each centroid
top_word_indices_2018_party = centroids_optimal_2018_party.argsort(axis=1)[:, :-top_n_words-1:-1]
# Get the feature names (words) from the TF-IDF vectorizer
feature_names = np.array(tfidf_vectorizer.get_feature_names_out())
# Print the most frequent words for each cluster
for i, centroid in enumerate(top_word_indices_2018_party):
    print(f"Cluster {i}:")
    for index in centroid:
        print(f"\t- {feature_names[index]}")
    print()



#---------------------Law & Order Dictionary
autho = [r'drug',
         r'crime',
         r'criminal',
         r'bombing',
         r'shooting',
         r'terror',
         r'violence',
         r'violent',
         r'police',
         r'surveillance',
         r'justice (?!\s+minister\b)',
         r'judicial system',
         r'prison',
         r'\brape',
         r'rapist',
         r'\bpenalt']
         
pattern_autho = r'(?i)(' + '|'.join(autho) + r')'

df_headings_sen.loc[:, 'autho'] = df_headings_sen['english_headline_intro'].str.contains(pattern_autho, regex=True)
#manually remove some rape cases
exceptions = [3542, 3707, 16501, 16570, 16577, 16704, 16706, 16709, 22664, 69683, 70013, 96605]
df_headings_sen.loc[df_headings_sen['ID'].isin(exceptions), 'autho'] = False
df_headings_sen['autho'].value_counts()


#CHECK AUTHO DICTIONARY
def count_occurrences_autho(word, df):
    # Filter the DataFrame to only rows where 'autho' is True
    filtered_df = df[df['autho'] == True]
    # Regex pattern for the word, case insensitive
    pattern = rf'(?i){word}'
    # Apply str.contains to find whether the word is in each document within filtered data
    # sum() will count True values indicating the presence of the word
    return filtered_df['english_headline_intro'].str.contains(pattern, regex=True).sum()
# Creating a new DataFrame to store results
word_autho_frequencies = pd.DataFrame({
    'Word': autho,
    'Frequency': [count_occurrences_autho(word, df_headings_sen) for word in autho]
})
print(word_autho_frequencies)
#save
word_autho_frequencies.to_excel("dictionary/04-21_word_autho_frequencies_3.xlsx", index=0)

#create random sample to check whether words are fitting
sampled_dataframes_autho = {}
#creat subsamples
for keyword in autho:
    # Filter rows where the english_headline_intro column contains the keyword
    filtered_df_autho = df_headings_sen[df_headings_sen['english_headline_intro'].str.contains(keyword,
                                                                                         case=False,
                                                                                         regex= True,
                                                                                         na=False)]
    filtered_df_autho = filtered_df_autho[['english_headline_intro', 'electionyear', 'newspaper']]
    # Sample up to 10 rows from the filtered DataFrame
    if len(filtered_df_autho) > 10:
        sampled_df_autho = filtered_df_autho.sample(n=10)  # Randomly select 10 entries
    else:
        sampled_df_autho = filtered_df_autho  # Take all entries if less than 10
    sampled_dataframes_autho[keyword] = sampled_df_autho
#path
file_path_autho = 'dictionary/04-21_autho_data_3.xlsx'

#clean sheet name 
def sanitize_sheet_name(name):
    invalid_chars = ['\\', '/', '*', '?', ':', '[', ']']
    for char in invalid_chars:
        name = name.replace(char, '')
    return name[:31]
#print
with pd.ExcelWriter(file_path_autho, engine='openpyxl') as writer:
    for keyword, df in sampled_dataframes_autho.items():
        sanitized_name = sanitize_sheet_name(keyword)
        df.to_excel(writer, sheet_name=sanitized_name)

##------------------Define Immmigration Dictionary
migration = [r'refugee',
             r'migration',
             r'migrant',
             r'foreign(?!\s+(security\b|affairs|minister|interference|policy|countr|investment|media\b|influence|power|haulier|image|actor|interest|talent|ministry|prison))',
             r'asyl',
             r'integration',
             r'ethnic',
             r'islam',
             r'muslim',
             r'burqa',
             r'mosque']
pattern_migration = r'(?i)(' + '|'.join(migration) + r')'
df_headings_sen['migration'] = df_headings_sen['english_headline_intro'].str.contains(pattern_migration, regex=True)
df_headings_sen['migration'].value_counts()


#check immigration dictionary
def count_occurrences_migration(word, df):
    # Filter the DataFrame to only rows where 'autho' is True
    filtered_df = df[df['migration'] == True]
    # Regex pattern for the word, case insensitive
    pattern = rf'(?i){word}'
    # Apply str.contains to find whether the word is in each document within filtered data
    # sum() will count True values indicating the presence of the word
    return filtered_df['english_headline_intro'].str.contains(pattern, regex=True).sum()

#Creating a new DataFrame to store results
word_migration_frequencies = pd.DataFrame({
    'Word': migration,
    'Frequency': [count_occurrences_migration(word, df_headings_sen) for word in migration]
})

#Display the new DataFrame
print(word_migration_frequencies)

word_migration_frequencies.to_excel("dictionary/04-21_word_migration_frequencies_2.xlsx", index=0)


#Sample to store the results 
sampled_dataframes_migration = {}
#creat subsamples
for keyword in migration:
    # Filter rows where the english_headline_intro column contains the keyword
    filtered_df_migration = df_headings_sen[df_headings_sen['english_headline_intro'].str.contains(keyword,
                                                                                         case=False,
                                                                                         regex= True,
                                                                                         na=False)]
    filtered_df_migration = filtered_df_migration[['english_headline_intro', 'electionyear', 'newspaper']]
    if len(filtered_df_migration) > 10:
        sampled_df_migration = filtered_df_migration.sample(n=10)  #Randomly select 10 entries
    else:
        sampled_df_migration = filtered_df_migration  #Take all entries if less than 10
    
    #Store the result in the dictionary with the keyword as the key
    sampled_dataframes_migration[keyword] = sampled_df_migration
    
#Path where the Excel file will be saved
file_path_migration = 'dictionary/04-21_migration_data_2.xlsx'
# Create an ExcelWriter object to write to an Excel file
with pd.ExcelWriter(file_path_migration, engine='openpyxl') as writer:
    for keyword, df in sampled_dataframes_migration.items():
        sanitized_name = sanitize_sheet_name(keyword)
        # Each DataFrame is written to a separate sheet named after the keyword
        df.to_excel(writer, sheet_name=sanitized_name)

#Confirm that the file has been saved
print(f"DataFrames have been saved to {file_path_migration}.")


#SAVE
df_headings_sen.to_excel("04-21-24_df_headings_sen.xlsx", index=0)

