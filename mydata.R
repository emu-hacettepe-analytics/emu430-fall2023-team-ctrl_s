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


new_data_first_four_imp  %>% 
  ggplot(aes(x = Date, y = Rate)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~ Brand) 


new_data



data_samsung %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Samsung's Global Dominance")

data_apple %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Apple's Global Dominance")

data_xiaomi %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Xiaomi's Global Dominance")

data_oppo %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Oppo's Global Dominance")

data_vivo %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Vivo's Global Dominance")

data_apple %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Apple's Global Dominance")

data_lg %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("LG's Global Dominance")

data_htc %>% ggplot(aes(Date, Rate)) +
  geom_point(aes(color = Year)) +
  theme_wsj() +
  scale_y_log10() +
  theme(axis.text.x = element_blank()) +
  ggtitle("HTC's Global Dominance")




