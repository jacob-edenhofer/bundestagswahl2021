
library(tidyverse)
library(readr)
library(scales)
library(stringr)
library(janitor)
library(sf)

# 2013 in 2017 constituencies 
btw13_header <- read_csv2("Literature/Graphs_Overview/Bundestagswahl2021_anaylsis/Data/btw13_ergebnisse_umgerechnet_auf_wahlkreise_btw2017.csv",
                   skip = 4, col_names = F, n_max = 2) 

btw13_header <- btw13_header %>%
  sapply(., paste, collapse = "_") %>%
  str_remove_all(., "_NA")

btw13_header <- sapply(btw13_header, paste, "_btw13")


btw13 <- read_csv2("Literature/Graphs_Overview/Bundestagswahl2021_anaylsis/Data/btw13_ergebnisse_umgerechnet_auf_wahlkreise_btw2017.csv",
                   skip = 6, col_names = F)
names(btw13) <- btw13_header

btw13_pruned <- btw13 %>%
  clean_names() %>%
  filter(!wahlkreisname_btw13 %in% c("Land", "Insgesamt")) %>%
  select(1:25)

# 2017
btw17_header <- read_csv2("Literature/Graphs_Overview/Bundestagswahl2021_anaylsis/Data/bundestagswahl17_results_by_constituency.csv",
                          col_names = F, n_max = 3)

btw17_header <- btw17_header %>%
  rownames_to_column(var = "rowname") %>%
  pivot_longer(cols = !rowname, 
               names_to = "super",
               values_to = "sub") %>%
  fill(sub, .direction = "down") %>%
  pivot_wider(id_cols = rowname, 
              names_from = super, values_from = sub) %>%
  sapply(., paste, collapse = "_") 

btw17_header[2] <- "wkr_nr"
btw17_header[3] <- "wahlkreis"
btw17_header[4] <- "bundesland"

btw17_header <- sapply(btw17_header, paste, "_btw17")


btw17 <-  read_csv2("Literature/Graphs_Overview/Bundestagswahl2021_anaylsis/Data/bundestagswahl17_results_by_constituency.csv",
                    skip = 3, 
                    col_names = F) %>%
  remove_empty(which = "rows") 

names(btw17) <- btw17_header[2:192]

btw17_pruned <- btw17 %>%
  clean_names() %>%
  filter(!(bundesland_btw17 == 99 | wahlkreis_btw17 == "Bundesgebiet")) %>%
  select(1:59)

# 2021
btw21_header <- read_delim("https://www.bundeswahlleiter.de/bundestagswahlen/2021/ergebnisse/opendata/csv/kerg.csv",
                           col_names = F, skip = 2, n_max = 3, delim = ";") 
btw21_header <- btw21_header %>%
  rownames_to_column(var = "rowname") %>% # assign rownames to be used as identifiers
  pivot_longer(cols = !rowname, names_to = "super", values_to = "sub") %>% 
  fill(sub, .direction = "down") %>% # turn data into long format to re-shape multi-line headers into single line headers by assigning super-categories to all columns
  pivot_wider(id_cols = rowname, names_from = super, values_from = sub) %>% # turn data back into wide format tom match headers in results data frame
  sapply(., paste, collapse = "_") 

btw21_header[2] <- "wkr_nr"
btw21_header[3] <- "wahlkreis"
btw21_header[4] <- "bundesland"
btw21_header[213] <- "uebrige_zweitstimmen_alternative"

btw21_header <- sapply(btw21_header, paste, "_btw21")

btw21 <- read_delim("https://www.bundeswahlleiter.de/bundestagswahlen/2021/ergebnisse/opendata/csv/kerg.csv",
                    col_names = F,  skip = 5, delim = ";") %>% # do not use column names and skip first five rows, delimiter is semicolon 
  remove_empty(which = "rows") 

## assign header names 
names(btw21) <- btw21_header[2:213] # I omit the first name since this is used the row identifier column.

## filter 
btw21_pruned <- btw21 %>%
  clean_names() %>%
  filter(!(bundesland_btw21 == "99" | wahlkreis_btw21 == "Bundesgebiet")) %>%
  select(1:51)

# new variable types 
btw21_pruned$wkr_nr_btw21 <- as.numeric(as.character(btw21_pruned$wkr_nr_btw21))
btw21_pruned$bundesland_btw21 <- as.numeric(as.character(btw21_pruned$bundesland_btw21)) 


# Merged data frame 
btws_13_21 <- btw21_pruned %>%
  left_join(btw17_pruned, by = c("wkr_nr_btw21" = "wkr_nr_btw17")) %>%
  left_join(btw13_pruned, by = c("wkr_nr_btw21" = "wkr_nr_btw13"))

# This should be a 299-by-133 data frame. 
myshape_btw21 <- "Literature/Graphs_Overview/Bundestagswahl2021_anaylsis/Data/btw21_geometrie_wahlkreise_geo_shp/Geometrie_Wahlkreise_20DBT_geo.shp"

shape21 <- st_read(myshape_btw21)


## merge again 
btws_13_21 <- btws_13_21 %>%
  left_join(shape21, by = c("wkr_nr_btw21" = "WKR_NR"))

## This is a 299-by-137 data frame. 

############################
# Creating useful variables. 
############################

btws_13_21 <- btws_13_21 %>%
  mutate(union_zweitstimmen_btw13 = ifelse(cdu_zweitstimmen_btw13 == 0, csu_zweitstimmen_btw13, cdu_zweitstimmen_btw13),
         union_zweitstimmen_btw17 = ifelse(is.na(christlich_demokratische_union_deutschlands_zweitstimmen_endgultig_btw17), christlich_soziale_union_in_bayern_e_v_zweitstimmen_endgultig_btw17, christlich_demokratische_union_deutschlands_zweitstimmen_endgultig_btw17),
         union_zweitstimmen_btw21 = ifelse(is.na(christlich_demokratische_union_deutschlands_zweitstimmen_endgultig_btw21), christlich_soziale_union_in_bayern_e_v_zweitstimmen_endgultig_btw21, christlich_demokratische_union_deutschlands_zweitstimmen_endgultig_btw21),
         union_zweitstimmen_share_btw13 = union_zweitstimmen_btw13/g_lti_zweitstimmen_btw13,
         union_zweitstimmen_share_btw17 = union_zweitstimmen_btw17/wahler_zweitstimmen_endgultig_btw17,
         union_zweitstimmen_share_btw21 = union_zweitstimmen_btw21/wahlende_zweitstimmen_endgultig_btw21,
         spd_zweitstimmen_share_btw21 = sozialdemokratische_partei_deutschlands_zweitstimmen_endgultig_btw21/wahlende_zweitstimmen_endgultig_btw21,
         gruene_zweitstimmen_share_btw21 = bundnis_90_die_grunen_zweitstimmen_endgultig_btw21/wahlende_zweitstimmen_endgultig_btw21,
         fdp_zweitstimmen_share_btw21 = freie_demokratische_partei_zweitstimmen_endgultig_btw21/wahlende_zweitstimmen_endgultig_btw21, 
         linke_zweitstimmen_share_btw21 = die_linke_zweitstimmen_endgultig_btw21/wahlende_zweitstimmen_endgultig_btw21,
         spd_zweitstimmen_share_btw13 = spd_zweitstimmen_btw13/g_lti_zweitstimmen_btw13,
         green_zweitstimmen_share_btw13 = gr_zweitstimmen_btw13/g_lti_zweitstimmen_btw13,
         fdp_zweitstimmen_share_btw13 = fdp_zweitstimmen_btw13/g_lti_zweitstimmen_btw13,
         linke_zweitstimmen_share_btw13 = die_linke_zweitstimmen_btw13/g_lti_zweitstimmen_btw13,
         CDU_CSU_delta_21_13 = union_zweitstimmen_share_btw21 - union_zweitstimmen_share_btw13,
         SPD_delta_21_13 = union_zweitstimmen_share_btw21 - spd_zweitstimmen_share_btw13,
         Gruene_B90_delta_21_13 = gruene_zweitstimmen_share_btw21 - green_zweitstimmen_share_btw13,
         FDP_delta_21_13 = fdp_zweitstimmen_share_btw21 - fdp_zweitstimmen_share_btw13,
         LINKE_delta_21_13 = linke_zweitstimmen_share_btw21 - linke_zweitstimmen_share_btw13) %>%
  select(wkr_nr_btw21, wahlkreis_btw21, geometry, ends_with("delta_21_13"))
save(btws_13_21, file = "Literature/Graphs_Overview/Bundestagswahl2021_anaylsis/Data/btws_13_21.RData")

