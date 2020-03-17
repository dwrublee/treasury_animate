library(tidyverse)
library(rvest)

url <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldAll"

dl_tbl <- url %>%
  read_html() %>%
  html_node(".t-chart") %>%
  html_table()

sapply(dl_tbl, class)

col_names <- colnames(dl_tbl)
dl_tbl[col_names[-1]] <- sapply(dl_tbl[col_names[-1]],as.numeric)

dl_fin <- dl_tbl %>%
  gather("Tenor","Yield",-Date) %>%
  mutate(unit = as.integer(str_extract(Tenor,"^\\d+"))) %>%
  mutate(time_val = str_extract(Tenor,"\\w+$")) %>%
  mutate(MonthAdj = ifelse(time_val == "yr", 12*unit, unit)) %>%
  select(Date,Tenor,Yield,MonthAdj)


get_date <- function(dl,curr_date){
  dl %>%
    filter(Date == curr_date)
}


plot_date <- function(dl,curr_date){
  get_date(dl,curr_date) %>%
    filter(!is.na(Yield)) %>%
    ggplot(aes(MonthAdj,Yield)) +
    geom_line() + 
    geom_point()
}


dl_fin %>%
  filter(str_extract(Date,"\\d{2}$") == "20") %>%
  ggplot(aes(MonthAdj,Yield)) + 
  geom_line() +
  geom_point() +
  labs(title = "{closest_state}") +
  transition_states(Date)
