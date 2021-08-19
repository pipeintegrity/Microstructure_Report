
# microstructure RM plots -------------------------------------------------

library(janitor)
library(patchwork)
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)

theme_set(theme_bw(16)) #set theme to save typing
setwd("C:\\Users\\Joel\\OneDrive - RSI Pipeline Solutions\\PGE\\charpy\\Microstructure_Report\\")

# data import ----------------------------------------
micro <- read_excel("Microstructure_Data_Collection_Online_2021_01_05_S5CJ.xlsx",
                    sheet = "Collection2") %>%
  mutate(Sample_Type = str_replace(Sample_Type, "-", "_"),
         name = paste0(Group," ",Feature),
         year = year(Evaluation_Date),
         Path = str_remove(string = Path,pattern = ".jpg"),
         Path = str_remove(string = Path, pattern = ".tif")) %>%
  clean_names()



feat_count <- micro %>%
  filter(!is.na(astm_grain_size)) %>%
  group_by(name) %>%
  count(sort=T) %>%
  ungroup() %>%
  slice_head(n=9)


cum_mean1 <- micro %>%
  filter(#group=="Modesto Yard",
    name %in% feat_count$name,
    !is.na(astm_grain_size)) %>%
  group_by(feature, method) %>%
  mutate(cum_avg_gs = cummean(astm_grain_size))


  cum_mean2 <- micro %>%
  filter(#group=="Modesto Yard",
    name %in% feat_count$name,
    !is.na(pct_dark_phase)) %>%
    group_by(feature,method) %>%
    mutate(cum_avg_dp = cummean(pct_dark_phase))



mean_gs <- cum_mean1 %>%
  group_by(name,method) %>%
  summarise(mgs = mean(astm_grain_size))

mean_dp <- cum_mean2 %>%
  group_by(name,method) %>%
  summarise(mdp = mean(pct_dark_phase))


# Working facet plot ------------------------------------------------------


    counting <- cum_mean1 %>%
      filter(method == "Counting") %>%
      mutate(idx = 1:n()) %>%
      ggplot(aes(idx, cum_avg_dp)) +
      geom_line(col='royalblue',
                lwd=0.8) +
      facet_wrap(~name, scales = "free")+
      geom_hline(
        data = mean_dp %>% filter(method=="Counting"),
        aes(yintercept = mdp),
        col = 'red',
        lty = 2
      )+
      labs(title ="Cumulative Mean ASTM Grain Size",
           subtitle = "Counting Method",
           x = "Count",
           y = "Cummulative Mean")



    compare <- cum_mean2 %>%
      filter(method == "Comparison") %>%
      mutate(idx = 1:n()) %>%
      ggplot(aes(idx, cum_avg_dp)) +
      geom_line(col='royalblue',
                lwd=0.8) +
      facet_wrap(~name, scales = "free")+
      geom_hline(
        data =  mean_dp %>% filter(method=="Comparison"),
        aes(yintercept = mdp),
        col = 'red',
        lty = 2
      )+
      labs(title ="Cumulative Mean ASTM Grain Size",
           subtitle = "Comparison Method",
           x = "Count",
           y = "Cummulative Mean"
           )

    counting/compare


# DP plots ----------------------------------------------------------------

    counting_dp <- cum_mean2 %>%
      filter(method == "Counting") %>%
      mutate(idx = 1:n()) %>%
      ggplot(aes(idx, cum_avg_dp)) +
      geom_line(col='royalblue',
                lwd=0.8) +
      facet_wrap(~name, scales = "free")+
      geom_hline(
        data = mean_dp %>% filter(method=="Counting"),
        aes(yintercept = mdp),
        col = 'red',
        lty = 2
      )+
      labs(title ="Cumulative Mean ASTM Grain Size",
           subtitle = "Counting Method",
           x = "Count",
           y = "Cummulative Mean")



    compare_dp <- cum_mean2 %>%
      filter(method == "Comparison") %>%
      mutate(idx = 1:n()) %>%
      ggplot(aes(idx, cum_avg_dp)) +
      geom_line(col='royalblue',
                lwd=0.8) +
      facet_wrap(~name, scales = "free")+
      geom_hline(
        data =  mean_dp %>% filter(method=="Comparison"),
        aes(yintercept = mdp),
        col = 'red',
        lty = 2
      )+
      labs(title ="Cumulative Mean ASTM Grain Size",
           subtitle = "Comparison Method",
           x = "Count",
           y = "Cummulative Mean"
      )

    counting_dp/compare_dp
