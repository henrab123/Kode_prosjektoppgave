library(dplyr)
library(ggplot2)
library(forecast) 
RTMB_V22

V22_TS <- RTMB_V22 %>%
  filter(kommisjon == 5) %>%
  mutate(Y_total = Y_m[,19] + Y_m[,20]) %>%  
  mutate(index = row_number()) %>%
  arrange(kandidatnummer) %>%
  select(index, kandidatnummer, Y_total)  


V22_TS_ts <- ts(V22_TS$Y_total, start = 1, frequency = 1)  

# Create the ggplot visualization
V22_plot <- ggplot(V22_TS, aes(x = index, y = Y_total)) +
            geom_line(linewidth = 0.7, color = "blue") + 
            labs(title = "Y_total Over Kandidatnummer",
            x = "Kandidatnummer",
            y = "Y_total") +
            theme_light(base_size = 20)

V22_plot


acf_V22 <- ggAcf(V22_TS_ts, lag.max = 50) + 
  ggtitle("ACF of V22") +
  theme_light()

acf_V22