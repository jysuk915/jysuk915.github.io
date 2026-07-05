# Load necessary libraries
if (!require("bib2df")) install.packages("bib2df")
library(bib2df)
library(dplyr)
library(tidyverse)
library(yaml)

# 1. Read your bib file
bib_raw <- bib2df("bibtex.bib")

# 2. Process into a clean format
pubs_processed <- bib_raw %>%
  rowwise() %>%
  mutate(
    date = URLDATE,
    
    author_text = paste(AUTHOR, collapse = ", "),
    
    primary_link = case_when(
      !is.na(DOI) ~ paste0("https://doi.org/", DOI),
      !is.na(URL) ~ as.character(URL),
      TRUE        ~ NA_character_
    ),
    
    link_label = case_when(
      !is.na(DOI) ~ "DOI",
      !is.na(URL) ~ "Link",
      TRUE        ~ ""
    ),
    
    outlet = case_when(
      CATEGORY == "TECHREPORT" ~ "White Paper",
      CATEGORY == "INCOLLECTION" ~ BOOKTITLE,
      CATEGORY == "INPROCEEDINGS" ~ BOOKTITLE,
      CATEGORY == "BOOK" ~ PUBLISHER,
      TRUE ~ JOURNAL),
    
    description = paste0(
      "<i>", outlet, "</i>", 
      if (!is.na(primary_link)) {
        paste0("<br><a href='", primary_link, "' target='_blank'>{{< fa link >}} ", link_label, "</a>")
      } else { "" }
    ),
    
    
#    description = paste(
#      paste0(author_text),
#      if (!is.na(primary_link)) {
#        paste0("[{{< fa link >}} ", link_label, "](", primary_link, ")")
#      } else {
#        NULL
#      },
#      sep = "<br>"
#    ),

    


    categories = list({
      cat_raw <- if (!is.na(CATEGORY) && nzchar(CATEGORY)) as.character(CATEGORY) else ""
      cat_raw <- dplyr::recode(
        cat_raw,
        BOOK = "BOOK CHAPTER",
        INCOLLECTION = "BOOK CHAPTER",
        .default = cat_raw
      )
      
      if (nzchar(cat_raw)) stringr::str_split(cat_raw, "\\s*,\\s*")[[1]] else character(0)
    }),
    
    title = str_remove_all(TITLE, "[{}]")) %>%
  ungroup() %>%
  select(title, author_text, date, description, categories)

pubs_processed <- pubs_processed %>%
  mutate(
    author_text = case_when(
      title == "Battleground: Asymmetric communication ecologies and the erosion of civil society in Wisconsin" ~
        paste0("**Book Authors:** ", author_text),
      
      TRUE ~ author_text
    )
  )


# KEY FIX: write a list of records (one list per row), not transpose(columns)
pubs_list <- pubs_processed %>%
  mutate(row_id = row_number()) %>%
  split(.$row_id) %>%
  lapply(function(dfrow) {
    dfrow$row_id <- NULL
    as.list(dfrow)
  }) %>% unname()  

write_yaml(pubs_list, "publications.yml")
