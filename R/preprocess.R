library(dplyr)
library(tidyr)
library(httr)
library(stringi)
library(data.table)


#' Download microtask dataset and 
#'
#' @param url character
#' @return data.frame
#'
download_data <- function(url = 'https://localhost/microtask.csv') {
    r <- httr::GET(url)
    if (r$status_code != 200) stop()
    con <- textConnection(httr::content(r, as="text"))
    df <- read.csv(con, header=FALSE)
    close(con)
    as.data.table(df)
}


#' Convert do tbl_dt, add names, and clean strings
#'
#' @param dt data.table
#' @return tbl_dt
#'
preprocess_data <- function(dt) {
    tbl_dt(dt) %>% rename(
        geo_accession=V1, control=V2, treatment=V3,
        gene=V4, perturbation=V5, species=V6, tissue_cell_line=V7,
        upregulated=V8, downregulated=V9, user=V10, datetime=V11, id=V12
    ) %>% 
    # Remove weird characters    
    mutate(control = stri_replace_all_fixed(control, '&Acirc;&nbsp;', '')) %>% 
    mutate(treatment = stri_replace_all_fixed(treatment, '&Acirc;&nbsp;', '')) %>%
    # Remove [ACCN]
    mutate(geo_accession = factor(
        stri_trim_both(stri_replace_all_fixed(geo_accession, '[ACCN]', '')))) %>%
    # Strip whitespaces
    mutate(gene = factor(stri_trim_both(as.character(gene))))
}


#' Create tidy table with sample ids
#'
#' @param dt tbl_dt as returned from preprocess_data
#' @return tbl_dt with 3 columns: id, group and sample
#'
extract_samples <- function(dt) {
    dt <- dt %>% select(id, control, treatment) %>% 
        gather('group', 'samples', control:treatment)
    tbl_dt(as.data.table(do.call(rbind, mapply(
        cbind,
        dt$id,
        as.character(dt$group),
        stri_split_fixed(dt$samples, ',')
    )))) %>% rename(id=V1, group=V2, sample=V3)
}


#' Create tidy table with gene ids and optional chdir coefficients
#'
#' @param dt tbl_dt as returned from preprocess_data
#' @return tbl_dt with 4 columns: id, category, gene, chdir
#'
extract_genes <- function(dt) {
    split_fields <- function(x) {
        as.data.frame(do.call(rbind, lapply(
            stri_split_regex(x, ',|\\s+', omit_empty = TRUE),
            function(x) if(length(x) == 1) { c(x[1], "") } else x
        )))
    }
    
    dt <- dt %>% select(id, upregulated, downregulated) %>%
        gather('category', 'samples', upregulated:downregulated)
    
    tbl_dt(as.data.table(do.call(rbind, mapply(
        cbind,
        dt$id,
        as.character(dt$category),
        stri_split_regex(dt$samples, '\n', omit_empty = TRUE)
    )))) %>%
    rename(id=V1, category=V2) %>%
    mutate(gene = stri_extract_first_regex(V3, '([^\t,]+)')) %>%
    mutate(chdir = stri_extract_first_regex(V3, '(?<=\t|,)([0-9\\.]+)$')) %>%
    select(id, category, gene, chdir)
}


#' Extract general information about the dataset
#' 
#' @param dt tbl_dt as returned from preprocess_data
#' @return tbl_dt with columns: id, geo_accession, gene, perturbation, species, tissue_cell_line
extract_description <- function(dt) {
    dt %>% select(id, geo_accession, gene, perturbation, species, tissue_cell_line)
}


#' Download dataset and extract relevant data
#' 
#' @param url 
#' @return list with description, genes and samples dt
#'
preprocess <- function(
    url='https://localhost/microtask.csv') {
    dt <- preprocess_data(download_data(url))
    list(
        description = extract_description(dt),
        genes = extract_genes(dt),
        samples = extract_samples(dt)
    )
}


#' Take genes table and prepare list of the gene sets
#' 
#' @param genes tbl_dt
#' @return list of the character vectors
#'
prepare_gene_sets <- function(genes) {
    genes_grouped <- genes %>% group_by(id, category) %>%
        summarise(genes=list(gene)) %>%
        mutate(id_cat = stri_join(id, category, sep="_"))
    mapply(c, genes_grouped$id_cat, genes_grouped$genes)
}