# clear global environment and load libraries
rm(list=ls())
library(rvest)
library(jsonlite)
library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)


# add link for simplejob.com
website <- "https://simplejob.com/search/all?"

# save html
t <- read_html(website)
write_html(t, "t.html")

# get the JSON
job_list <- fromJSON(
t %>%
  html_nodes(xpath = "//script[@type='application/json']") %>%
  html_text())

# unbox the JSON
toJSON(job_list, auto_unbox = T)

# load data into dataset 
job_df <- job_list$props$pageProps$jobsPageData$data$positions
company_df <- job_list$props$pageProps$jobs$company




###### some cleaning in job_df:

job_df <- separate_rows(job_df,url_slug, convert = T)
job_df <- job_df %>% mutate("Category" = url_slug)
job_df <- job_df %>%  select(-c(url_slug, type))

#drop unnassacry categories:
job_df <- job_df %>% subset(Category != "allasok" & Category != "munkas" & Category != "konyhai" & Category != "center" & Category != "ugyintezo" & Category != "munkak" & Category != "munkatars" & Category != "szelli" & Category != "munka" & Category != "allas" & Category != "allasok" & Category != "egyeb" & Category != "instruktor" & Category != "fizikai")

# rename some categories
job_df$Category <- gsub("kisegito", "konyhai kisegito", job_df$Category)
job_df$Category <- gsub("call", "call-center", job_df$Category)
job_df$Category <- gsub("logisztikai", "logisztikai ugyintezo", job_df$Category)
job_df$Category <- gsub("raktarvezeto", "raktarvezeto helyettes", job_df$Category)
job_df$Category <- gsub("szakmunkak", "egyeb szakmunkak", job_df$Category)
job_df$Category <- gsub("fitness", "fitness instruktor", job_df$Category)
job_df$Category <- gsub("konnyu", "konnyu fizikai", job_df$Category)

# View(job_df)


####### some cleaning in company_df
# selecting necassary columns:
company_df <- company_df %>% select(c(company_id_ai, active_followers, name, introduction, motto, web, facebook, instagram, video_url, count_active_job_offers, url_slug))
company_df <- company_df %>% distinct()

# View(company_df)


### Analysis of job_df:
# select the highest number of listings in categories:
frequency_job <- job_df %>% group_by(Category) %>% count(Category)
# sum_job <- job_df %>% count(name)


# select categories where the frequency is higher than 1
frequency_job <- filter(frequency_job, n > 1 )

# total number of offerings:
# num_off <- c("Total number of offerings: ", cbind(sum(sum_job$n)))


# View(frequency_job)

# visualize the frequencies
ggplot(frequency_job, aes(x = reorder(Category, -n), y = n)) +
  geom_bar(stat = 'identity', fill = 'red') +
  labs(x = "Category Name", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Analysis of company_df
summary_comp_foll <- company_df %>% select(c(active_followers, name, count_active_job_offers))
# View(summary_comp_foll)

# select most nessacary columns for visualization and exclude companies with 0 followers
company_active_foll <- summary_comp_foll %>%  filter(active_followers > 0)

# visualization of company and their number of followers
ggplot(company_active_foll, aes(x = reorder(name, -active_followers), y = active_followers)) +
  geom_bar(stat = 'identity', fill = 'Orange') +
  labs(x = "Company Name", y = "Number of active followers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# visualization of actibe job offers by companies
ggplot(company_active_foll, aes(x = reorder(name, -count_active_job_offers), y = count_active_job_offers)) +
         geom_bar(stat = 'identity', fill = 'gold') +
        labs(x = "Company name", y = "Number of active job offerings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
