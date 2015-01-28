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
    
    tbl_dt(df) %>% rename(
        geo_accession=V1, control=V2, treatment=V3,
        gene=V4, perturbation=V5, species=V6, tissue_cell_line=V7,
        upregulated=V8, downregulated=V9, user=V10, datetime=V11, id=V12
    ) %>% 
    # Remove weird characters    
    mutate_each(funs(clean_stri), control:treatment) %>%
    # Remove [ACCN]
    mutate(geo_accession = factor(
        stri_trim_both(stri_replace_all_fixed(geo_accession, '[ACCN]', '')))) %>%
    # Strip whitespaces
    mutate(gene = factor(stri_trim_both(as.character(gene))))
}

#' Create tidy table with sample ids
#'
#' @param df tbl_df as returned from preprocess_data
#' @return tbl_df with 3 columns: id, group and sample
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



#' Create tidy table with gene ids and optional chdir coefficients
#'
#' @param df tbl_df as returned from preprocess_data
#' @return tbl_df with 4 columns: id, category, gene, chdir
#'
extract_genes <- function(df) {
    split_fields <- function(x) {
        as.data.frame(do.call(rbind, lapply(
            stri_split_regex(x, ',|\\s+', omit_empty = TRUE),
            function(x) if(length(x) == 1) { c(x[1], "") } else x
        )))
    }
    
    df <- df %>% select(id, upregulated, downregulated) %>%
        gather('category', 'samples', upregulated:downregulated)
    
    tbl_df(as.data.frame(do.call(rbind, mapply(
        cbind,
        df$id,
        as.character(df$category),
        stri_split_regex(df$samples, '\n', omit_empty = TRUE)
    )))) %>%
    rename(id=V1, category=V2) %>%
    mutate(gene = stri_extract_first_regex(V3, '([^\t,]+)')) %>%
    mutate(chdir = stri_extract_first_regex(V3, '(?<=\t|,)([0-9\\.]+)$')) %>%
    select(id, category, gene, chdir)
}
