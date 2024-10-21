library(tidyverse)
library(ggplot2)
url<-"https://raw.githubusercontent.com/stormwhale/data-mines/refs/heads/main/job_skills.csv"
df<-read.csv(url, sep=',')
nrow(df) #12217

#remove empty cells:
df1<- na.omit(if_else(df$job_skills=="", NA, df))
nrow(df1) #12212
view(df1)


#separate the skills:
df_mod<- df1 %>%
  separate_wider_delim(job_skills, delim=',',
                       names_sep = '_',
                       too_few = 'align_start')

view(df_mod)

#Select only the first 20 skills listed:
df_mod<- df_mod %>%
  subset(select = c(1:21))

view(df_mod)

#pivot_longer to put all skills into one column:
df_mod_long<- df_mod %>%
  pivot_longer(cols = c(2:21), #This is where we can adjust how many columns we want to include. Too many will cause loading issue.
               names_to = 'sets',
               values_to = 'Skills',
               values_drop_na = TRUE)
  
#remove 'sets' column:
df_mod_long<- df_mod_long%>%
  subset(select = c(job_link, Skills))
  
#trimming the white spaces:
df_skills<-data.frame('Skills'=trimws(df_mod_long$Skills))

#Filter out the top 10 wanted skills:
top_ten<- df_skills %>%
  count(Skills) %>%
  slice_max(n, n= 10)

#plotting it in a histogram:
ggplot(top_ten, aes(x=reorder(Skills, n), y=n)) +
  geom_bar(stat='identity') +
  labs(x='Skills', y="", title='Top 10 Data Science Job skills wanted') +
  theme(axis.text.x = element_text(angle = 45, hjust= 1))
