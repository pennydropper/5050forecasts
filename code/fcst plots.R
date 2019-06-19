
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Ambitious forecasting plots ---------------------------------------------

months <- seq.Date(from = ymd("20180701"), to = ymd("20190601"), by = "month") %>% 
  glimpse()

actual <- c(100, 120, 130, 125, 100, 90, 80, 85, 100, 105, 115, 100)

act_tbl <- bind_cols(list(month = months, qty = actual, type = rep("Actual", 12)))

exp_growth <- function(st_act, st_mon, gr_in = 1.1, gr_min = 1, n_per = 12) {
  # Create tibble of exponential growth
  # st_act = starting actual volume for period 0
  # st_mon = starting month (date)
  # gr_in = maximumn period on period growth
  # gr_min = minimum period on period growth
  # n_per = number of periods to return
  
  gr_pop <- c(1, runif(n_per - 1, min = gr_min, max = gr_in)) %>%
    cumprod() # %>% glimpse()
  
  # st_act = actual[3]; st_mon = months[3]
  
  bind_cols(list(month = seq.Date(st_mon, length.out = n_per, by = "month"),
                 qty = gr_pop * st_act,
                 type = rep(str_c("Fc", format(st_mon, "%b-%y"), sep = ":"), n_per)))
  
}


# . Build fcst data -------------------------------------------------------

fcst_per <- seq.Date(ymd("20180801"), by = "2 month", length.out = 4)

fcst_per <- c(fcst_per, ymd("20181101"))

fcst_lst <- vector("list", fcst_per %>% length())

set.seed(1234)

for (i in seq_along(fcst_per)) {
  
  mon_snap <- fcst_per[[i]]
  mon_act <-
    act_tbl %>% 
      filter(month == mon_snap) %>% 
      pull(qty) %>% head(1)
  
  fcst_lst[[i]] <-
    exp_growth(st_act = mon_act,
               st_mon = mon_snap, gr_in = 1.1, gr_min = 1, n_per = 12)
    
  
}

actfcst <- 
  bind_rows(act_tbl, fcst_lst) %>%   
  mutate(type = factor(type,
                       levels = c("Actual",
                                  fcst_per %>% sort() %>% 
                                    format("%b-%y") %>% 
                                    str_c("Fc:", ., sep = ""))))


# Plot data ---------------------------------------------------------------

actfcst_p <- 
  actfcst %>%
  ggplot(aes(x = month, y = qty, colour = type)) +
  geom_line(linetype = "dashed") +
  geom_line(data = actfcst %>% filter(type == "Actual")) +
  scale_y_continuous(limits = c(50, 250)) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%y",
               limits = c(ymd("20180701"), ymd("20190701"))) +
  scale_colour_viridis_d("") +
  labs(title = "Forecast snapshots",
       y = "units", x = "")

actfcst_p  


# Save plot ---------------------------------------------------------------

ggsave("./static/img/fcst_snaps.jpg", actfcst_p, width = 31, height = 10.5, units = "cm")


# NPD density plot --------------------------------------------------------

# library(readxl)
# p <-
readxl::read_xlsx(str_c(getwd(), 'NPD forecasting.xlsx', sep = "/"),
                  range = 'Charts!A1:A1001') %>%
                  # range = 'Doodle!M10:M1010') %>%
  ggplot(aes(x = fcst_vols)) +
  geom_density() +
  # plotly::ggplotly(p)
  # scale_x_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 100)) 
  geom_histogram(aes(y = ..density..), fill = NA, colour = "grey",
                 breaks = seq(200, 2600, 200), na.rm = TRUE) +
  scale_x_continuous(limits = c(0, 2500), breaks = seq(100, 2500, 200)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0001)) +
  theme_minimal() +
  labs(title = "Probability density of NPD volumes",
       x = "New product volumes",
       y = "density")
