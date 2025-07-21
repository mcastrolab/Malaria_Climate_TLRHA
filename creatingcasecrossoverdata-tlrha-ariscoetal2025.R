# Case-Crossover Dataset Creation for Malaria Study
# Author: Nicholas J. Arisco
# Purpose: Process case and climate data to prepare for analysis
# Date: YYYY-MM-DD

#============================#
#     Load Required Libraries #
#============================#
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
library(lubridate)

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
library(zoo)

if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(tidyr)

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
library(sf)

if (!requireNamespace("geobr", quietly = TRUE)) install.packages("geobr")
library(geobr)

if (!requireNamespace("sp", quietly = TRUE)) install.packages("sp")
library(sp)

if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
library(gstat)

#============================#
#     Set Working Directory  #
#============================#
# NOTE: Replace with relative path if sharing code
setwd("")

#============================#
#     Load Annual Case Data  #
#============================#
# Load all years from 2003–2022
years <- 2003:2022
case_files <- paste0("NOTI", substr(years, 3, 4), ".csv")
case_data <- lapply(case_files, fread, sep = ";")
names(case_data) <- paste0("d", substr(years, 3, 4))

# Harmonize column structure for recent years (2019–2022)
common_cols <- names(case_data[["d18"]])
for (y in 19:22) {
  case_data[[paste0("d", y, "b")]] <- subset(case_data[[paste0("d", y)]], select = common_cols)
}

#============================#
#   Filter for Valid Records  #
#============================#
# Filter out negative tests and lamina type 3 for 2021 and 2022
d22c <- case_data$d22b[RES_EXAM > 1 & TIPO_LAM != 3]
d21c <- case_data$d21b[RES_EXAM > 1 & TIPO_LAM != 3]

#============================#
#    Combine All Years Data   #
#============================#
d_all <- rbindlist(c(case_data[1:16], case_data$d19b, case_data$d20b, case_data$d21b, case_data$d22b), use.names = TRUE, fill = TRUE)

# Remove large objects to free memory
rm(list = ls(pattern = "^d[0-9]{2}$"))
gc()

#============================#
#     Process Dates & Flags   #
#============================#
d_all[, date_notif := as.Date(DT_NOTIF, format = "%d/%m/%Y")]
d_all[, year := year(date_notif)]
d_all[, month := month(date_notif)]
d_all[, ym := ym(paste(year, month, sep = "-"))]

# Define outcome flags
d_all[, case := ifelse(RES_EXAM > 1 & TIPO_LAM != 3 & ID_LVC != 1, 1, 0)]
d_all[, LVC := ifelse(RES_EXAM > 1 & (TIPO_LAM == 3 | ID_LVC == 1), 1, 0)]

# Filter confirmed cases with valid municipality of infection
d_cases <- d_all[case == 1 & !is.na(MUN_INFE)]

#============================#
#   Symptom Date Processing   #
#============================#
d_cases[, date_symp := as.Date(DT_SINTO, format = "%d/%m/%Y")]

# Impute missing symptom dates using mean notification lag
lag_dist <- na.omit(d_cases[, date_notif - date_symp])
mean_lag <- mean(lag_dist)

# Impute with random samples from lag distribution
d_cases[is.na(date_symp), date_symp := date_notif - sample(lag_dist, .N, replace = TRUE)]

# Adjust outliers
d_cases[date_symp > as.Date("2020-12-31") | date_symp < as.Date("2002-12-01"),
        date_symp := date_notif - sample(lag_dist, .N, replace = TRUE)]

# Estimate infection date assuming 7–14 day incubation
d_cases[, date_infe_14daysprior := date_symp - 14]

#============================#
#   Create Case-Crossover Set #
#============================#
# Identify cases near month boundaries
d_cases[, day := day(date_infe_14daysprior)]
d_cases[, days_remaining := days_in_month(date_infe_14daysprior) - day]
d_cases[, four_or_less_start := as.integer(day <= 4)]
d_cases[, four_or_less_end := as.integer(days_remaining <= 4)]
d_cases[, ID := .I]  # Unique ID

# Helper: Generate control periods
sample_controls <- function(df, start_offset, end_offset, n_controls = 5) {
  df[, .(date = seq(date_infe_14daysprior + start_offset, date_infe_14daysprior + end_offset, by = "3 days")), by = ID] %>%
    group_by(ID) %>% slice_sample(n = n_controls)
}

# Block 1: Cases at start of month
controls_1 <- sample_controls(d_cases[four_or_less_start == 1], 4, days_in_month(date_infe_14daysprior))

# Block 2: Cases at end of month
controls_2 <- sample_controls(d_cases[four_or_less_end == 1], -days_in_month(date_infe_14daysprior), -4)

# Block 3: Cases in middle of month
controls_3_top <- sample_controls(d_cases[four_or_less_start == 0 & four_or_less_end == 0], 4, days_in_month(date_infe_14daysprior))
controls_3_bottom <- sample_controls(d_cases[four_or_less_start == 0 & four_or_less_end == 0], -days_in_month(date_infe_14daysprior), -4)
controls_3 <- bind_rows(controls_3_top, controls_3_bottom)

# Combine all controls and cases
cases_final <- d_cases[, .(ID, date = date_infe_14daysprior, case = 1, MUN_INFE, RES_EXAM)]
controls_final <- bind_rows(controls_1, controls_2, controls_3) %>% mutate(case = 0)
final_crossover <- bind_rows(cases_final, controls_final)

#============================#
# Merge Climate Data         #
#============================#
# Load climate data
clim <- fread("")
clim[, date := as.Date(date)]
clim[, diurnal_variation := temperature_2m_max - temperature_2m_min]

# Compute lagged weather summaries
clim_w <- clim %>%
  group_by(mun) %>%
  arrange(mun, date) %>%
  mutate(
    precipitation_1week_sum = lag(rollapply(total_precipitation_sum, 7, sum, fill = NA, align = "right"), 0),
    mean_temp_1week_avg = lag(rollapply(temperature_2m, 7, mean, fill = NA, align = "right"), 0),
    diurnal_variation_1week = lag(rollapply(diurnal_variation, 7, mean, fill = NA, align = "right"), 0)
  ) %>%
  ungroup()

# Merge case-crossover set with climate data
final_crossover <- merge(final_crossover, clim_w, by.x = c("MUN_INFE", "date"), by.y = c("mun", "date"), all.x = TRUE)

#============================#
# Export Final Dataset       #
#============================#
output_path <- ""
fwrite(final_crossover, file = output_path)

#============================#
# Summary Statistics         #
#============================#
summary(final_crossover)

cat("\nCase-crossover dataset successfully created and saved to:", output_path, "\n")
