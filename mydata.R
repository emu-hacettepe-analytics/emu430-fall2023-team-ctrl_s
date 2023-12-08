install.packages("readr")
library(readr)
install.packages("readxl")
library(readxl)
install.packages("utf8")
library(utf8)
library(ggplot2)
library(tidyverse)

head(vendordatason)

vendordatason_samsung <- vendordatason %>% filter(Brand == "Samsung")
vendordatason_apple <- vendordatason %>% filter(Brand == "Apple")
vendordatason_xiaomi <- vendordatason %>% filter(Brand == "Xiaomi")
vendordatason_huawei <- vendordatason %>% filter(Brand == "Huawei")
vendordatason_oppo <- vendordatason %>% filter(Brand == "Oppo")
vendordatason_unknown <- vendordatason %>% filter(Brand == "Unknown")
vendordatason_motorola <- vendordatason %>% filter(Brand == "Motorola")
vendordatason_lg <- vendordatason %>% filter(Brand == "LG")
vendordatason_lenovo <- vendordatason %>% filter(Brand == "Lenovo")
vendordatason_mobicel <- vendordatason %>% filter(Brand == "Mobicel")
vendordatason_vivo <- vendordatason %>% filter(Brand == "Vivo")
vendordatason_nokia <- vendordatason %>% filter(Brand == "Nokia")
vendordatason_realme <- vendordatason %>% filter(Brand == "Realme")
vendordatason_sony <- vendordatason %>% filter(Brand == "Sony")
vendordatason_asus <- vendordatason %>% filter(Brand == "Asus")
vendordatason_bbk <- vendordatason %>% filter(Brand == "BBK")
vendordatason_oneplus <- vendordatason %>% filter(Brand == "OnePlus")
vendordatason_htc <- vendordatason %>% filter(Brand == "HTC")
vendordatason_google <- vendordatason %>% filter(Brand == "Google")
vendordatason_other <- vendordatason %>% filter(Brand == "Other")



vendordatason_samsung %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("Samsung's Market Dominance")

vendordatason_apple %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("Apple's Market Dominance")

vendordatason_xiaomi %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("Xiaomi's Market Dominance")

vendordatason_oppo %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("Oppo's Market Dominance")

vendordatason_vivo %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("Vivo's Market Dominance")

vendordatason_apple %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("Apple's Market Dominance")

vendordatason_lg %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("LG's Market Dominance")

vendordatason_htc %>% ggplot(aes(Date, Rates)) +
  geom_point(aes(color = Date)) +
  ggtitle("HTC's Market Dominance")

