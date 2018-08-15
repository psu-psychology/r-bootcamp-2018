# Modified by ROG on 2018-08-15 from Hallquist code

library(readr)
library(dplyr)
library(haven)
library(tidyr)
library(ggplot2)

survey_file_name <- "data/R+Bootcamp+2018_August+13%2C+2018_10.49.sav"

df <- read_spss(survey_file_name) %>% 
  filter(DistributionChannel != "preview") %>% filter(Contact_2 != "Hallquist" & !(is.na(Contact_1) & is.na(Contact_4))) %>%
  select(Contact_1, Contact_2, Contact_3, Contact_4, Role, Role_6_TEXT, starts_with("Th"), starts_with("Fr"))

#Thursday intro
table(df$ThIntro_1, useNA = "always") #52
 
#10am-12pm BASIC
table(df$ThParallel_1, useNA = "always") #41

#1pm-1:45pm BASIC data indexing
table(df$ThParallel_2, useNA = "always") #50

#1:45pm-3pm BASIC functions and packages
table(df$ThParallel_3, useNA = "always") #44

#10am-12pm EFA Ram
table(df$ThParallel_4, useNA = "always") #27

#1pm-2pm lavaan
table(df$ThParallel_5, useNA = "always") #22

#2pm-3pm parallel computing in R
table(df$ThParallel_6, useNA = "always") #29

#3pm-4:30pm reproducible R
table(df$ThEnd_1, useNA="always") #47


###
#Friday: 9:15am - 10:30am data wrangling 
table(df$Fr_1, useNA = "always") #59

#10:45am-12pm data visualization
table(df$Fr_2, useNA = "always") #60

#1pm-2pm core programming skills
table(df$Fr_3, useNA="always") #61

#best practices in R
table(df$Fr_4, useNA = "always") #58

#basic data analyses in R
table(df$Fr_5, useNA = "always") #59

df_agg <- df %>% 
  gather(key="Event", value="Count", starts_with("Fr"), starts_with("Th")) %>%
  group_by(Event) %>% 
  summarize(Attendance=sum(Count, na.rm=TRUE)) %>%
  mutate(Event=factor(Event, 
                      levels=c("ThIntro_1", "ThParallel_1", "ThParallel_2", "ThParallel_3", "ThParallel_4", "ThParallel_5", "ThParallel_6", "ThEnd_1", "Fr_1", "Fr_2", "Fr_3", "Fr_4", "Fr_5"),
                      labels=c("Th Intro", "Th Basic 10am", "Th Basic 1pm", "Th Basic 1:45pm", "Th EFA 10am", "Th lavaan 1pm", "Th big data 2pm", "Th Repro R", 
                               "Fr wrangling 9:15am", "Fr visualization 10:45am", "Fr core programming 1pm", "Fr best practices 2pm", "Fr analyses 3pm")
  ))

# pdf("R_bootcamp_attendance.pdf", width=12, height=8)
attend_plot <- ggplot(df_agg, aes(x=Event, y=Attendance)) + 
  geom_bar(stat="identity") + 
  theme_bw(base_size=17) + 
  theme(axis.text.x=element_text(angle=90))
attend_plot
# dev.off()

