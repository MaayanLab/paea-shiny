config <- list(
    # Data path
    genes_data_path = 'data/microtask_gene_lists.csv',
    drugs_data_path = 'data/drug_perturbation_microtask_gene_lists.csv',
    disease_data_path = 'data/disease_signature_microtask_gene_lists.csv',
    # Maximum number of genes to keepp
    max_ngenes_tokeep = 250,
    # Maximum fraction of genes to keep
    max_fgenes_tokeep = 1,
    drop_duplicates = TRUE,
    id_filter = 'control|chr([0-9]+|[xy]):[0-9]+-[0-9]+'
)