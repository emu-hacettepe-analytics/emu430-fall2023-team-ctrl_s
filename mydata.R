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

#fdatason <- read_excel("GitHub/emu430-fall2023-team-ctrl_s/vendordatason.xlsx")

new_data <- read_excel("GitHub/emu430-fall2023-team-ctrl_s/new_data.xlsx")

head(vendordatason)

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



data_samsung %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 35)) +
  # theme(axis.text.x = element_blank()) +
  ggtitle("Samsung's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#0c4da2", size = 35))

  #A3AAAE Apple

data_apple %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 35)) +
  ggtitle("Apple's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#A3AAAE", size = 35))

#FF6900 Xiaomi

data_xiaomi %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 35)) +
  ggtitle("Xiaomi's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#FF6900", size = 35))


data_huawei %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Huawei's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#CF0A2C", size = 35))


data_oppo %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Oppo's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) +
  theme(plot.title = element_text(color = "#1EA366", size = 35))

data_vivo %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Vivo's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#0072B8", size = 35))

#990033 LG

data_lg %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("LG's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#990033", size = 35))

data_htc %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("HTC's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#8CC751", size = 35))

#8CC751 HTC

data_other %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  # scale_y_continuous(limits = c(0, max(data_samsung$Rate))) +
  ggtitle("Other Brand's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#660099", size = 35))


data_tecno %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Tecno's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#006B8B", size = 35))


data_sony %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Sony's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "black", size = 35))


# TR piyasası

#datayı düzenleme kodu: vendordatatr <- gather(vendordatatr, "Samsung", "Apple", "Huawei", "Xiaomi", 
#                       "LG", "Sony", "Oppo", "HTC", "Lenovo", "Asus",
#                       "Nokia","Unknown", "Realme", "Motorola", "Tecno", "Vivo",
#                      "OnePlus", "Google", "Other", key = "Brand", value = "Rate" )

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

data_samsung_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 35)) +
  # theme(axis.text.x = element_blank()) +
  ggtitle("Samsung's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#0c4da2", size = 35))

#A3AAAE Apple

data_apple_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 35)) +
  ggtitle("Apple's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#A3AAAE", size = 35))

#FF6900 Xiaomi

data_xiaomi_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 35)) +
  ggtitle("Xiaomi's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#FF6900", size = 35))


data_huawei_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Huawei's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#CF0A2C", size = 35))


data_oppo_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Oppo's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) +
  theme(plot.title = element_text(color = "#1EA366", size = 35))

data_vivo_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Vivo's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#0072B8", size = 35))

#990033 LG

data_lg_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("LG's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#990033", size = 35))

data_htc_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("HTC's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#8CC751", size = 35))

#8CC751 HTC

data_other_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  # scale_y_continuous(limits = c(0, max(data_samsung$Rate))) +
  ggtitle("Other Brand's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#660099", size = 35))


data_tecno_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Tecno's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "#006B8B", size = 35))


data_sony_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Sony's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(plot.title = element_text(color = "black", size = 35))


