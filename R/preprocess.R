library(dplyr)
library(tidyr)
library(httr)
library(stringi)
library(data.table)

url <- 'https://localhost/microtask.csv'

#' Download microtask dataset and 
#'
#' @param url character
#' @return data.frame
#'
download_data <- function(url) {
    r <- httr::GET(url)
    if (r$status_code != 200) stop()
    con <- textConnection(httr::content(r, as="text"))
    df <- read.csv(con, header=FALSE)
    close(con)
    df
}

#' Convert do tbl_dt, add names, and clean strings
#'
#' @param df data.frame
#' @return tbl_dt
#'
preprocess_data <- function(df) {
    clean_stri <- function(x) stri_replace_all_fixed(x, '&Acirc;&nbsp;', '')
    
    df <- tbl_dt(df) %>% rename(
        geo_accession=V1, control=V2, treatment=V3,
        gene=V4, perturbation=V5, species=V6, tissue_cell_line=V7,
        upregulated=V8, downregulated=V9, user=V10, datetime=V11, id=V12
    ) %>% mutate_each(funs(clean_stri), control:treatment)
    
    df
}

#' Create tidy table with sample ids
#'
#' @param df tbl_df as returned from preprocess_data
#' @return tbl_df with 3 columns, id, group and sample
#'
extract_samples <- function(df) {
    df <- df %>% select(id, control, treatment) %>% 
        gather('group', 'samples', control:treatment)
    tbl_df(as.data.frame(do.call(rbind, mapply(
        cbind,
        df$id,
        as.character(df$group),
        stri_split_fixed(df$samples, ',')
    )))) %>% rename(id=V1, group=V2, sample=V3)
}
