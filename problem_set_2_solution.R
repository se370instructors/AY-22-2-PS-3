#---Problem Set 2 - Solution
#-Feb 2022

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

ap_df <- read_csv('covid_approval_polls.csv')
con_df <- read_csv('covid_concern_polls.csv')


#---Q1

plot_1_df <- ap_df %>%
  mutate(year = paste0(year(end_date), '.', quarter(end_date))) %>%
  filter(party == 'all') %>%
  group_by(year) %>%
  summarize(ap_per = mean(approve)) %>%
  arrange(year) %>%
  mutate(ap_per = round(ap_per, 0))


ggplot(plot_1_df, aes(x = year, y = ap_per, fill = ap_per)) + geom_bar(stat = 'identity', width = 1) + theme_classic(base_size = 14) + ylim(c(0, 100)) +
  geom_text(aes(label = ap_per), color = 'white', vjust = 1.5) +
  xlab('Year (By Quarter)') + ylab('Approval Percentage') + scale_fill_continuous(name = "Approval") + ggtitle('Public Opinion of Presidential Handling of COVID-19') +
  theme(plot.title = element_text(hjust = 0.5)) 


    
#---Q2
plot_2_df <- con_df %>%
  filter(pollster %in% c('YouGov', 'Harris Poll'), as.character(end_date) %in% c('2020-02-04', '2020-03-13', '2021-04-20', '2021-03-14')) %>%
  mutate(issue_clean = ifelse(subject == 'concern-economy', 'Economy', 'COVID')) %>%
  mutate(label = paste0(issue_clean, ' - ', end_date, ' - ', pollster)) %>%
  select(label, very, somewhat, not_very, not_at_all) %>%
  rename('Very' = 'very', 'Somewhat' = 'somewhat', 'Not Very' = 'not_very', 'Not At All' = 'not_at_all') %>%
  pivot_longer(cols = c('Very', 'Somewhat','Not Very','Not At All')) %>%
  mutate(name = factor(name))
  


ggplot(plot_2_df, aes(x = name, y = value)) + theme_light(base_size = 14) + geom_point(color = 'red') + 
  geom_segment(aes(x = name, xend = name, y = 0, yend = value), color = 'blue') + facet_wrap(~label) + coord_flip() +
  ylab('Population (%)') + xlab('Level of Concern') + ggtitle('Shifting Concern about COVID and the Economy') +
  theme(plot.title = element_text(hjust = 0.5)) 


#---Q3
table(ap_df$party)

plot_3_df <- ap_df %>%
  mutate(month_floor = floor_date(end_date, 'month')) %>%
  group_by(month_floor, party) %>%
  summarize(med = median(disapprove, na.rm = TRUE)) %>%
  mutate(party = recode(party, 'D' = 'Democrat', 'R' = 'Republican', 'I' = 'Independent', 'all' = 'All'))


ggplot(plot_3_df) + geom_line(aes(x = month_floor, y = med, color = party, group = party), size = 1.25) + theme_classic(base_size = 14) +
  scale_color_manual(values = c('black','blue','purple','red'), name = "Party") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_date(date_labels = '%b %Y', date_breaks = '1 month') + geom_vline(xintercept = as.Date('2021-01-20'), size = 1, linetype = 'dashed') +
  xlab('Date') + ylab('Median Disapproval Rate') + ggtitle('Monthly Median Disapproval Rate for Each Party\nBefore and After Inauguration Day') +
  theme(plot.title = element_text(hjust = 0.5)) 








