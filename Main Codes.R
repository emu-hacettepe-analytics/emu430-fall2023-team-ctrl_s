library(readr)
library(readxl)
library(utf8)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(ggthemes)
library(ggplot2)
library(patchwork)
library(tidyr)
library(openxlsx)

new_data <- read_excel("GitHub/emu430-fall2023-team-ctrl_s/new_data.xlsx")

new_data_sep_year <- new_data %>% separate(Date, c("Year","Month"), "-", remove = FALSE)


data_samsung <- new_data_sep_year %>% filter(Brand == "Samsung")
data_apple <- new_data_sep_year %>% filter(Brand == "Apple")
data_xiaomi <- new_data_sep_year %>% filter(Brand == "Xiaomi")
data_huawei <- new_data_sep_year %>% filter(Brand == "Huawei")
data_oppo <- new_data_sep_year %>% filter(Brand == "Oppo")
data_unknown <- new_data_sep_year %>% filter(Brand == "Unknown")
data_motorola <- new_data_sep_year %>% filter(Brand == "Motorola")
data_lg <- new_data_sep_year %>% filter(Brand == "LG")
data_lenovo <- new_data_sep_year %>% filter(Brand == "Lenovo")
data_vivo <- new_data_sep_year %>% filter(Brand == "Vivo")
data_nokia <- new_data_sep_year %>% filter(Brand == "Nokia")
data_realme <- new_data_sep_year %>% filter(Brand == "Realme")
data_sony <- new_data_sep_year %>% filter(Brand == "Sony")
data_asus <- new_data_sep_year %>% filter(Brand == "Asus")
data_oneplus <- new_data_sep_year %>% filter(Brand == "OnePlus")
data_htc <- new_data_sep_year %>% filter(Brand == "HTC")
data_google <- new_data_sep_year %>% filter(Brand == "Google")
data_other <- new_data_sep_year %>% filter(Brand == "Other")
data_tecno <- new_data_sep_year %>% filter(Brand == "Tecno")


new_data_tr <- read_excel("GitHub/emu430-fall2023-team-ctrl_s/new_data_tr.xlsx")

new_data_sep_year_tr <- new_data_tr %>% separate(Date, c("Year","Month"), "-", remove = FALSE)

data_samsung_tr <- new_data_sep_year_tr %>% filter(Brand == "Samsung")
data_apple_tr <- new_data_sep_year_tr %>% filter(Brand == "Apple")
data_xiaomi_tr <- new_data_sep_year_tr %>% filter(Brand == "Xiaomi")
data_huawei_tr <- new_data_sep_year_tr %>% filter(Brand == "Huawei")
data_oppo_tr <- new_data_sep_year_tr %>% filter(Brand == "Oppo")
data_unknown_tr <- new_data_sep_year_tr %>% filter(Brand == "Unknown")
data_motorola_tr <- new_data_sep_year_tr %>% filter(Brand == "Motorola")
data_lg_tr <- new_data_sep_year_tr %>% filter(Brand == "LG")
data_lenovo_tr <- new_data_sep_year_tr %>% filter(Brand == "Lenovo")
data_vivo_tr <- new_data_sep_year_tr %>% filter(Brand == "Vivo")
data_nokia_tr <- new_data_sep_year_tr %>% filter(Brand == "Nokia")
data_realme_tr <- new_data_sep_year_tr %>% filter(Brand == "Realme")
data_sony_tr <- new_data_sep_year_tr %>% filter(Brand == "Sony")
data_asus_tr <- new_data_sep_year_tr %>% filter(Brand == "Asus")
data_oneplus_tr <- new_data_sep_year_tr %>% filter(Brand == "OnePlus")
data_htc_tr <- new_data_sep_year_tr %>% filter(Brand == "HTC")
data_google_tr <- new_data_sep_year_tr %>% filter(Brand == "Google")
data_other_tr <- new_data_sep_year_tr %>% filter(Brand == "Other")
data_tecno_tr <- new_data_sep_year_tr %>% filter(Brand == "Tecno")





combined_plot <- plot_htc + plot_other
print(combined_plot)

