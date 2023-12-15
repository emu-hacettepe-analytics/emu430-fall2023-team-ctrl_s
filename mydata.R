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

new_data_sep_year <- new_data %>% separate(Date, c("Year","Month"), "-", remove = FALSE)



# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------

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



glob_samsung_plot <- data_samsung %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  # theme(axis.text.x = element_blank()) +
  ggtitle("Samsung's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_samsung_plot)

# ----------------------------------------------------

  #A3AAAE Apple

glob_apple_plot <- data_apple %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  ggtitle("Apple's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#A3AAAE", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_apple_plot)


# ----------------------------------------------------

#FF6900 Xiaomi

glob_xiaomi_plot <- data_xiaomi %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  ggtitle("Xiaomi's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#FF6900", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_xiaomi_plot)

# ----------------------------------------------------


glob_huawei_plot <- data_huawei %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Huawei's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#CF0A2C", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_huawei_plot)


# ----------------------------------------------------


glob_oppo_plot <- data_oppo %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Oppo's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#1EA366", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_oppo_plot)


# ----------------------------------------------------


glob_vivo_plot <- data_vivo %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Vivo's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#0072B8", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_vivo_plot)


# ----------------------------------------------------

#990033 LG

glob_lg_plot <- data_lg %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("LG's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#990033", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_lg_plot)


# ----------------------------------------------------

glob_htc_plot <- data_htc %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("HTC's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#8CC751", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_htc_plot)

#8CC751 HTC

# ----------------------------------------------------


glob_other_plot <- data_other %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  # scale_y_continuous(limits = c(0, max(data_samsung$Rate))) +
  ggtitle("Other Brand's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#660099", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_other_plot)


# ----------------------------------------------------


glob_tecno_plot <- data_tecno %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Tecno's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#006B8B", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_tecno_plot)


# ----------------------------------------------------


glob_sony_plot <- data_sony %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Sony's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "black", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_sony_plot)


# ----------------------------------------------------


glob_realme_plot <- data_realme %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Realme's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#FFC916", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_realme_plot)


# ----------------------------------------------------


glob_google_plot <- data_google %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Google's Global Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#4285F4", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_google_plot)

# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------

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



# ----------------------------------------------------

tr_samsung_plot <- data_samsung_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  # theme(axis.text.x = element_blank()) +
  ggtitle("Samsung's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_samsung_plot)

# ----------------------------------------------------

#A3AAAE Apple

tr_apple_plot <- data_apple_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  ggtitle("Apple's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#A3AAAE", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_apple_plot)


# ----------------------------------------------------

#FF6900 Xiaomi

tr_xiaomi_plot <- data_xiaomi_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  ggtitle("Xiaomi's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#FF6900", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_xiaomi_plot)


# ----------------------------------------------------


tr_huawei_plot <- data_huawei_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Huawei's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#CF0A2C", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_huawei_plot)


# ----------------------------------------------------


tr_oppo_plot <- data_oppo_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Oppo's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#1EA366", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_oppo_plot)


# ----------------------------------------------------


tr_vivo_plot <- data_vivo_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("Vivo's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#0072B8", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_vivo_plot)


# ----------------------------------------------------

#990033 LG

tr_lg_plot <- data_lg_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color =  "black", size = 3 ) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("LG's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#990033", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_lg_plot)


# ----------------------------------------------------


tr_htc_plot <- data_htc_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 15)) +
  ggtitle("HTC's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#8CC751", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_htc_plot)


#8CC751 HTC
# ----------------------------------------------------


tr_other_plot <- data_other_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  # scale_y_continuous(limits = c(0, max(data_samsung$Rate))) +
  ggtitle("Other Brand's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#660099", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_other_plot)


# ----------------------------------------------------


tr_tecno_plot <- data_tecno_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Tecno's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#006B8B", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_tecno_plot)


# ----------------------------------------------------


tr_sony_plot <- data_sony_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Sony's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "black", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_tecno_plot)


# ----------------------------------------------------


tr_realme_plot <- data_realme_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Realme's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#FFC916", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_realme_plot)


# ----------------------------------------------------


tr_google_plot <- data_google_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Google's Turkey Dominance") +
  scale_x_discrete(breaks = c("2017-12", "2018-12", "2019-12", "2020-12", "2021-12", "2022-12")) + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#4285F4", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_google_plot)


# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# COMBINED PLOTS


samsung_compare_globtr <- tr_samsung_plot + glob_samsung_plot
print(samsung_compare_globtr)

# ----------------------------------------------------

apple_compare_globtr <- tr_apple_plot + glob_apple_plot
print(apple_compare_globtr)

# ----------------------------------------------------

xiaomi_compare_globtr <- tr_xiaomi_plot + glob_xiaomi_plot
print(xiaomi_compare_globtr)

# ----------------------------------------------------

huawei_compare_globtr <- tr_huawei_plot + glob_huawei_plot
print(huawei_compare_globtr)

# ----------------------------------------------------

oppo_compare_globtr <- tr_oppo_plot + glob_oppo_plot
print(oppo_compare_globtr)

# ----------------------------------------------------

unknown_compare_globtr <- tr_unknown_plot + glob_unknown_plot
print(unknown_compare_globtr)

# ----------------------------------------------------

motorola_compare_globtr <- tr_motorola_plot + glob_motorola_plot
print(motorola_compare_globtr)

# ----------------------------------------------------

lg_compare_globtr <- tr_lg_plot + glob_lg_plot
print(lg_compare_globtr)

# ----------------------------------------------------

lenovo_compare_globtr <- tr_lenovo_plot + glob_lenovo_plot
print(lenovo_compare_globtr)

# ----------------------------------------------------

vivo_compare_globtr <- tr_vivo_plot + glob_vivo_plot
print(vivo_compare_globtr)

# ----------------------------------------------------

realme_compare_globtr <- tr_realme_plot + glob_realme_plot
print(realme_compare_globtr)

# ----------------------------------------------------

sony_compare_globtr <- tr_sony_plot + glob_sony_plot
print(sony_compare_globtr)

# ----------------------------------------------------

asus_compare_globtr <- tr_asus_plot + glob_asus_plot
print(asus_compare_globtr)

# ----------------------------------------------------

oneplus_compare_globtr <- tr_oneplus_plot + glob_oneplus_plot
print(oneplus_compare_globtr)

# ----------------------------------------------------

htc_compare_globtr <- tr_htc_plot + glob_htc_plot
print(htc_compare_globtr)

# ----------------------------------------------------

google_compare_globtr <- tr_google_plot + glob_google_plot
print(google_compare_globtr)

# ----------------------------------------------------

other_compare_globtr <- tr_other_plot + glob_other_plot
print(other_compare_globtr)

# ----------------------------------------------------

tecno_compare_globtr <- tr_tecno_plot + glob_tecno_plot
print(tecno_compare_globtr)

# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------








