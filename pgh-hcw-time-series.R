library(tidyverse) # dataframe functions
library(dslabs) # dataframe functions
library(dplyr) # math functions
library(readr) # for parsing csv files
library(ggplot2) # for graphing
library(lubridate) # for parsing date
library(scales) # scaling functions
library(gridExtra) # arranging graphs

# PGH data

up_pgh_daily_report <- read.csv("up-pgh-daily-report.csv")
colnames(up_pgh_daily_report)[1] <- "report_date"

up_pgh_hcw_time_series <- up_pgh_daily_report %>%
  select(report_date, tested_hcw, positive_hcw, hcw_covid_wards, doctors_covid_wards, nurses_covid_wards,
         others_covid_wards, hcw_elsewhere, doctors_elsewhere, nurses_elsewhere, others_elsewhere) %>%
  mutate(report_date = mdy(report_date),
         positive_rate = positive_hcw / tested_hcw,
         ratio = hcw_elsewhere / hcw_covid_wards)

# graphing

ratio_data <- up_pgh_hcw_time_series %>%
  mutate(doctors = doctors_covid_wards + doctors_elsewhere, nurses = nurses_covid_wards + nurses_elsewhere, others = others_covid_wards + others_elsewhere, n_d_ratio = nurses / doctors, o_d_ratio = others / doctors, o_n_ratio = others / nurses) %>%
  select(report_date, ratio, n_d_ratio, o_d_ratio, o_n_ratio, positive_rate)

ratio_plot <- ratio_data %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_line(aes(color = variable)) +
  #scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "COVID(+) Health Care Workers in Philippine General Hospital") +
  xlab("Reporting Date") +
  ylab("Value") +
  scale_color_discrete(name = "Legend", labels = c("Nurses : Doctors", "Others : Doctors", "Others : Nurses", "Positivity Rate", "Non-COVID : COVID Areas"))
ratio_plot
ggsave("ratio_plot.png")

# infected hcws breakdown

infected_hcws_breakdown <- up_pgh_hcw_time_series %>% 
  select(report_date, hcw_covid_wards, hcw_elsewhere) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_bar(aes(fill = factor(variable, levels = c("hcw_elsewhere", "hcw_covid_wards"))), position = "stack", stat = "identity", alpha = 0.3) +
  scale_color_manual(values = c("red", "green")) +
  labs(title = "COVID(+) Health Care Workers in Philippine General Hospital") +
  xlab("Reporting Date") +
  ylab("Number of Cases") +
  scale_fill_discrete(name = "Legend", labels = c("Non-COVID Areas", "COVID Areas"))
infected_hcws_breakdown
ggsave("infected_hcws_breakdown.png")

doctors_nurses_sub_breakdown <- up_pgh_hcw_time_series %>%
  select(report_date, doctors_covid_wards, doctors_elsewhere, nurses_covid_wards, nurses_elsewhere, others_covid_wards, others_elsewhere) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_bar(aes(fill = factor(variable, levels = c("doctors_covid_wards", "doctors_elsewhere", "nurses_covid_wards", "nurses_elsewhere", "others_covid_wards", "others_elsewhere"))), position = "stack", stat = "identity", alpha = 0.3) +
  scale_color_manual(values = c("red", "green", "dark_red", "blue", "violet", "brown")) +
  labs(title = "COVID(+) Health Care Workers in Philippine General Hospital") +
  xlab("Reporting Date") +
  ylab("Number of Cases") +
  scale_fill_discrete(name = "Legend", labels = c("COVID Doctors", "Non-COVID Doctors", "COVID Nurses", "Non-COVID Nurses", "COVID Others", "Non-COVID Others"))
doctors_nurses_sub_breakdown
ggsave("doctors_nurses_sub_breakdown.png")

doctors_nurses_breakdown <- up_pgh_hcw_time_series %>%
  mutate(doctors = doctors_covid_wards + doctors_elsewhere, nurses = nurses_covid_wards + nurses_elsewhere, others = others_covid_wards + others_elsewhere) %>%
  select(report_date, doctors, nurses, others) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_bar(aes(fill = factor(variable, levels = c("doctors", "nurses", "others"))), position = "stack", stat = "identity", alpha = 0.3) +
  scale_color_manual(values = c("red", "green", "blue")) +
  labs(title = "COVID(+) Health Care Workers in Philippine General Hospital") +
  xlab("Reporting Date") +
  ylab("Number of Cases") +
  scale_fill_discrete(name = "Legend", labels = c("Doctors", "Nurses", "Others"))
doctors_nurses_breakdown
ggsave("doctors_nurses_breakdown.png")

# extreme value statistics

print("Maximum Nurse to Doctor Infection Ratio:")
print(max(ratio_data$n_d_ratio, na.rm = TRUE))

print("Minimum Nurse to Doctor Infection Ratio:")
print(min(ratio_data$n_d_ratio, na.rm = TRUE))

print("Maximum Non-COVID to COVID Area Infection Ratio:")
print(max(ratio_data$ratio, na.rm = TRUE))

print("Minimum Non-COVID to COVID Area Infection Ratio:")
print(min(ratio_data$ratio, na.rm = TRUE))

# moving average statistics

#ratio_data_ma <- ratio_data %>% mutate()