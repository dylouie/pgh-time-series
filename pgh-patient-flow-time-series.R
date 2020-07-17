library(tidyverse) # dataframe functions
library(dslabs) # dataframe functions
library(dplyr) # math functions
library(readr) # for parsing csv files
library(ggplot2) # for graphing
library(lubridate) # for parsing date
library(scales) # scaling functions
library(gridExtra) # arranging graphs
library(fpp2) # for time series

# PGH data

up_pgh_daily_report <- read.csv("up-pgh-daily-report.csv")
colnames(up_pgh_daily_report)[1] <- "report_date"

up_pgh_patient_flow_time_series <- up_pgh_daily_report %>%
  select(report_date, total_covid_admissions, recovered_improved, cumulative_discharged, cumulative_expired, cumulative_admissions) %>%
  mutate(report_date = mdy(report_date),
         hospital_fatality_rate = cumulative_expired / cumulative_admissions,
         hospital_recovery_rate = recovered_improved / cumulative_admissions)

# patient flow counts

patient_flow_plot <- up_pgh_patient_flow_time_series %>%
  select(report_date, total_covid_admissions, recovered_improved, cumulative_discharged, cumulative_expired, cumulative_admissions) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_line(aes(color = variable)) +
  scale_color_manual(values = c("red", "blue", "green", "violet", "brown")) +
  labs(title = "Patient Flow in Philippine General Hospital") +
  xlab("Reporting Date") +
  ylab("Count") +
  scale_color_discrete(name = "Legend", labels = c("Cumulative Admissions", "Cumulative Discharged", "Cumulative Expired", "Cumulative Recovered/Improved", "Current/Daily Admissions"))
patient_flow_plot
ggsave("patient_flow_plot.png")

# fatality and recovery rates

hospital_rates_plot <- up_pgh_patient_flow_time_series %>%
  select(report_date, hospital_fatality_rate, hospital_recovery_rate) %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_line(aes(color = variable)) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Philippine General Hospital Recovery and Fatality Rates") +
  xlab("Reporting Date") +
  ylab("Value") +
  scale_color_discrete(name = "Legend", labels = c("Fatality Rate", "Recovery Rate"))
hospital_rates_plot
ggsave("hospital_rates_plot.png")

# time series analysis - net admission

net_admission_flow <- up_pgh_patient_flow_time_series %>%
  filter(report_date >= mdy("5/24/2020")) %>%
  select(report_date, total_covid_admissions)

net_admission_flow_plot <- net_admission_flow %>%
  ggplot(aes(x = report_date, y = total_covid_admissions)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Philippine General Hospital Net Admissions (Bed Demand) since May 24") +
  xlab("Reporting Date") +
  ylab("Net Admissions")
net_admission_flow_plot
ggsave("net_admission_flow_plot.png")

# time series analysis - deaths

death_flow <- up_pgh_patient_flow_time_series %>%
  filter(report_date >= mdy("5/24/2020")) %>%
  select(report_date, cumulative_expired)

death_flow_plot <- death_flow %>%
  ggplot(aes(x = report_date, y = cumulative_expired)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Philippine General Hospital Total Deaths since May 24") +
  xlab("Reporting Date") +
  ylab("Total Deaths")
death_flow_plot
ggsave("death_flow_plot.png")

# time series analysis - discharge

discharge_flow <- up_pgh_patient_flow_time_series %>%
  filter(report_date >= mdy("5/24/2020")) %>%
  select(report_date, cumulative_discharged)

discharge_flow_plot <- discharge_flow %>%
  ggplot(aes(x = report_date, y = cumulative_discharged)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Philippine General Hospital Total Discharged since May 24") +
  xlab("Reporting Date") +
  ylab("Total Discharged")
discharge_flow_plot
ggsave("discharge_flow_plot.png")

# time series analysis - new admissions

admission_flow <- up_pgh_patient_flow_time_series %>%
  filter(report_date >= mdy("5/24/2020")) %>%
  select(report_date, cumulative_admissions)

admission_flow_plot <- admission_flow %>%
  ggplot(aes(x = report_date, y = cumulative_admissions)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Philippine General Hospital Total Admissions since May 24") +
  xlab("Reporting Date") +
  ylab("Total Admitted")
admission_flow_plot
ggsave("admission_flow_plot.png")

# fusion flow graph

fusion_flow <- up_pgh_patient_flow_time_series %>%
  filter(report_date >= mdy("5/24/2020")) %>%
  select(report_date, total_covid_admissions, cumulative_discharged, cumulative_expired, cumulative_admissions)

fusion_flow_plot <- fusion_flow %>%
  gather(key = "variable", value = "value", -report_date) %>%
  ggplot(aes(x = report_date, y = value)) +
  geom_smooth(aes(color = variable)) +
  #scale_color_manual(values = c("red", "blue")) +
  labs(title = "Philippine General Hospital Daily Admissions, Deaths, and Discharges") +
  xlab("Reporting Date") +
  ylab("Value") +
  scale_color_discrete(name = "Legend", labels = c("Admissions", "Discharges", "Deaths", "Net Admissions"))
fusion_flow_plot
ggsave("fusion_flow_plot.png")

# ARIMA model - net admissions

auto.arima(ts(net_admission_flow$total_covid_admissions), d = 1)
checkresiduals(auto.arima(ts(net_admission_flow$total_covid_admissions)))

# ARIMA model - deaths

auto.arima(ts(death_flow$cumulative_expired))
checkresiduals(auto.arima(ts(death_flow$cumulative_expired)))

# ARIMA model - discharges

auto.arima(ts(discharge_flow$cumulative_discharged), d = 1)
checkresiduals(auto.arima(ts(death_flow$cumulative_discharged)))

# ARIMA model - admissions

auto.arima(ts(admission_flow$cumulative_admissions), d = 1)
checkresiduals(auto.arima(ts(admission_flow$cumulative_admissions)))
ggsave("admissions_residuals.png")
