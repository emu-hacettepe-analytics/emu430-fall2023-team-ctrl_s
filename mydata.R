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
library(lubridate)

#fdatason <- read_excel("GitHub/emu430-fall2023-team-ctrl_s/vendordatason.xlsx")



new_data <- read_excel("GitHub/emu430-fall2023-team-ctrl_s/new_data.xlsx")

new_data_sep_year <- new_data %>% separate(Date, c("Year","Month"), "-", remove = FALSE)
new_data_sep_year$Date <- ym(new_data_sep_year$Date)




# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------

data_samsung <- new_data_sep_year %>% filter(Brand == "Samsung") 
data_apple <- new_data_sep_year %>% filter(Brand == "Apple")
data_xiaomi <- new_data_sep_year %>% filter(Brand == "Xiaomi")
data_huawei <- new_data_sep_year %>% filter(Brand == "Huawei")
data_oppo <- new_data_sep_year %>% filter(Brand == "Oppo")
data_unknown <- new_data_sep_year %>% filter(Brand == "Unknown")
data_lg <- new_data_sep_year %>% filter(Brand == "LG")
data_vivo <- new_data_sep_year %>% filter(Brand == "Vivo")
data_nokia <- new_data_sep_year %>% filter(Brand == "Nokia")
data_realme <- new_data_sep_year %>% filter(Brand == "Realme")
data_sony <- new_data_sep_year %>% filter(Brand == "Sony")
data_oneplus <- new_data_sep_year %>% filter(Brand == "OnePlus")
data_htc <- new_data_sep_year %>% filter(Brand == "HTC")
data_other <- new_data_sep_year %>% filter(Brand == "Other")
data_tecno <- new_data_sep_year %>% filter(Brand == "Tecno")



glob_samsung_plot <- data_samsung %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0, 60)) +
  # theme(axis.text.x = element_blank()) +
  ggtitle("Samsung's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
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
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
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
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Oppo's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Vivo's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("LG's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("HTC's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Other Brand's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Tecno's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Sony's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Realme's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Google's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#4285F4", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_google_plot)

# ----------------------------------------------------


glob_unknown_plot <- data_unknown %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  ggtitle("Unknown Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "black", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_unknown_plot)


# ----------------------------------------------------


glob_oneplus_plot <- data_oneplus %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Oneplus's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#F50514", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_oneplus_plot)


# ----------------------------------------------------


glob_nokia_plot <- data_nokia %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  ggtitle("Nokia's Global Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#183693", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(glob_nokia_plot)


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
new_data_sep_year_tr$Date <- ym(new_data_sep_year_tr$Date)

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
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 60)) +
  ggtitle("Huawei's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Oppo's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("Vivo's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("LG's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits = c(0, 10)) +
  ggtitle("HTC's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 15)) +
  ggtitle("Other Brand's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Tecno's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Sony's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
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
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Realme's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#FFC916", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_realme_plot)

#continuos x ekseni kategorik değişken 
# ----------------------------------------------------

data_google_tr$Year <- as.integer(data_google_tr$Year)
tr_google_plot <- data_google_tr %>% ggplot(aes(Year, Rate)) +
  geom_line(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Google's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#4285F4", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_google_plot)


# ----------------------------------------------------


tr_unknown_plot <- data_unknown_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  # scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Unknown Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "black", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_unknown_plot)


# ----------------------------------------------------


tr_oneplus_plot <- data_oneplus_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Oneplus's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#F50514", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_oneplus_plot)



# ----------------------------------------------------


tr_nokia_plot <- data_nokia_tr %>% ggplot(aes(Date, Rate)) +
  geom_point(color = "black", size = 3) +
  theme_ipsum() +
  scale_y_continuous(limits =c(0, 10)) +
  ggtitle("Nokia's Turkey Dominance") +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") + 
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(axis.title.x = element_text(size = 30)) +     # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30))+
  theme(plot.title = element_text(color = "#183693", size = 48)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1.00))  # Yardımcı ızgara çizgileri kalınlığı

print(tr_nokia_plot)


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

# Lenovo iptla

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

# Asus iptal

# ----------------------------------------------------

oneplus_compare_globtr <- tr_oneplus_plot + glob_oneplus_plot
print(oneplus_compare_globtr)

# ----------------------------------------------------

htc_compare_globtr <- tr_htc_plot + glob_htc_plot
print(htc_compare_globtr)

# ----------------------------------------------------

# Google iptal

# ----------------------------------------------------

other_compare_globtr <- tr_other_plot + glob_other_plot
print(other_compare_globtr)

# ----------------------------------------------------

tecno_compare_globtr <- tr_tecno_plot + glob_tecno_plot
print(tecno_compare_globtr)
# ----------------------------------------------------

nokia_compare_globtr <- tr_nokia_plot + glob_nokia_plot
print(nokia_compare_globtr)

# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------


compare_glob_five <- ggplot() +
  geom_line(data = data_samsung, aes(x = Date, y = Rate, color = "Samsung"), size = 2) +
  geom_line(data = data_apple, aes(x = Date, y = Rate, color = "Apple"), size = 2) +
  geom_line(data = data_xiaomi, aes(x = Date, y = Rate, color = "Xiaomi"), size = 2) +
  geom_line(data = data_huawei, aes(x = Date, y = Rate, color = "Huawei"), size = 2) +
  geom_line(data = data_oppo, aes(x = Date, y = Rate, color = "Oppo"), size = 2) +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
  theme_ipsum() +
  scale_color_manual(values = c("Samsung" = "blue", "Apple" = "red",
                                "Xiaomi" = "green", "Huawei" = "yellow", "Oppo" = "purple")) +
  labs(title = "Brands", color = "Brand") +
  ggtitle("Global Market Percentages") +
  theme(legend.text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +  # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1))  # Yardımcı ızgara çizgileri kalınlığı

print(compare_glob_five)

compare_glob_ten <- ggplot() +
  geom_line(data = data_unknown, aes(x = Date, y = Rate, color = "Unknown"), size = 2) +
  geom_line(data = data_lg, aes(x = Date, y = Rate, color = "LG"), size = 2) +
  geom_line(data = data_vivo, aes(x = Date, y = Rate, color = "Vivo"), size = 2) +
  geom_line(data = data_nokia, aes(x = Date, y = Rate, color = "Nokia"), size = 2) +
  geom_line(data = data_realme, aes(x = Date, y = Rate, color = "Realme"), size = 2) +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
  theme_ipsum() +
  scale_color_manual(values = c("Unknown" = "blue", "LG" = "red",
                                "Vivo" = "green", "Nokia" = "yellow", "Realme" = "purple")) +
  labs(title = "Brands", color = "Brand") +
  ggtitle("Global Market Percentages") +
  theme(legend.text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +  # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1))  # Yardımcı ızgara çizgileri kalınlığı

print(compare_glob_ten)

compare_glob_fifteen <- ggplot() +
  geom_line(data = data_sony, aes(x = Date, y = Rate, color = "Sony"), size = 2) +
  geom_line(data = data_oneplus, aes(x = Date, y = Rate, color = "OnePlus"), size = 2) +
  geom_line(data = data_htc, aes(x = Date, y = Rate, color = "HTC"), size = 2) +
  geom_line(data = data_other, aes(x = Date, y = Rate, color = "Other"), size = 2) +
  geom_line(data = data_tecno, aes(x = Date, y = Rate, color = "Tecno"), size = 2) +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
  theme_ipsum() +
  scale_color_manual(values = c("Sony" = "blue", "OnePlus" = "red",
                                "HTC" = "green", "Other" = "yellow", "Tecno" = "purple")) +
  labs(title = "Brands", color = "Brand") +
  ggtitle("Global Market Percentages") +
  theme(legend.text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +  # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1))  # Yardımcı ızgara çizgileri kalınlığı

print(compare_glob_fifteen)


#---------------------TR-------------------------

compare_tr_five <- ggplot() +
  geom_line(data = data_samsung_tr, aes(x = Date, y = Rate, color = "Samsung"), size = 2) +
  geom_line(data = data_apple_tr, aes(x = Date, y = Rate, color = "Apple"), size = 2) +
  geom_line(data = data_xiaomi_tr, aes(x = Date, y = Rate, color = "Xiaomi"), size = 2) +
  geom_line(data = data_huawei_tr, aes(x = Date, y = Rate, color = "Huawei"), size = 2) +
  geom_line(data = data_oppo_tr, aes(x = Date, y = Rate, color = "Oppo"), size = 2) +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
  theme_ipsum() +
  scale_color_manual(values = c("Samsung" = "blue", "Apple" = "red",
                                "Xiaomi" = "green", "Huawei" = "yellow", "Oppo" = "purple")) +
  labs(title = "Brands", color = "Brand") +
  ggtitle("Turkey Market Percentages") +
  theme(legend.text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +  # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1))  # Yardımcı ızgara çizgileri kalınlığı

print(compare_tr_five)

compare_tr_ten <- ggplot() +
  geom_line(data = data_unknown_tr, aes(x = Date, y = Rate, color = "Unknown"), size = 2) +
  geom_line(data = data_lg_tr, aes(x = Date, y = Rate, color = "LG"), size = 2) +
  geom_line(data = data_vivo_tr, aes(x = Date, y = Rate, color = "Vivo"), size = 2) +
  geom_line(data = data_nokia_tr, aes(x = Date, y = Rate, color = "Nokia"), size = 2) +
  geom_line(data = data_realme_tr, aes(x = Date, y = Rate, color = "Realme"), size = 2) +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
  theme_ipsum() +
  scale_color_manual(values = c("Unknown" = "blue", "LG" = "red",
                                "Vivo" = "green", "Nokia" = "yellow", "Realme" = "purple")) +
  labs(title = "Brands", color = "Brand") +
  ggtitle("Turkey Market Percentages") +
  theme(legend.text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +  # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1))  # Yardımcı ızgara çizgileri kalınlığı

print(compare_tr_ten)

compare_tr_fifteen <- ggplot() +
  geom_line(data = data_sony_tr, aes(x = Date, y = Rate, color = "Sony"), size = 2) +
  geom_line(data = data_oneplus_tr, aes(x = Date, y = Rate, color = "OnePlus"), size = 2) +
  geom_line(data = data_htc_tr, aes(x = Date, y = Rate, color = "HTC"), size = 2) +
  geom_line(data = data_other_tr, aes(x = Date, y = Rate, color = "Other"), size = 2) +
  geom_line(data = data_tecno_tr, aes(x = Date, y = Rate, color = "Tecno"), size = 2) +
  scale_x_date(date_labels = "20%y-%m", date_breaks = "12 months") +
  theme_ipsum() +
  scale_color_manual(values = c("Sony" = "blue", "OnePlus" = "red",
                                "HTC" = "green", "Other" = "yellow", "Tecno" = "purple")) +
  labs(title = "Brands", color = "Brand") +
  ggtitle("Turkey Market Percentages") +
  theme(legend.text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 29)) +
  theme(plot.title = element_text(color = "#0c4da2", size = 48)) +
  theme(axis.title.x = element_text(size = 30)) +  # X ekseni etiketi boyutu
  theme(axis.title.y = element_text(size = 30)) +
  theme(panel.grid.major = element_line(size = 1.25),  # Ana ızgara çizgileri kalınlığı
        panel.grid.minor = element_line(size = 1))  # Yardımcı ızgara çizgileri kalınlığı

print(compare_tr_fifteen)


library(readxl)
excel_data1 <- read_excel("new_data_tr.xlsx")
excel_data2 <- read_excel("new_data.xlsx")
save(excel_data1, file = "new_data_tr.RData")
save(excel_data2, file = "new_data.RData")

