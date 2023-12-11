# install.packages("readr")
library(readr)
#install.packages("readxl")
library(readxl)
#install.packages("utf8")
library(utf8)
library(ggplot2)
library(tidyverse)
# install.packages("hrbrthemes")
library(hrbrthemes)
# install.packages("ggthemes")
library(ggthemes)
library(ggplot2)

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


new_data_first_four_imp  %>% 
  ggplot(aes(x = Date, y = Rate)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~ Brand) 


new_data

new_data_sep_year %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Brand)) +
  theme_ipsum() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Global Dominance")



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






