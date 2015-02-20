config <- list(
    # Data path
    data_path = 'data/microtask_gene_lists.csv',
    # Maximum number of genes to keepp
    max_ngenes_tokeep = 250,
    # Maximum fraction of genes to keep
    max_fgenes_tokeep = 1,
    drop_duplicates = TRUE,
    id_filter = 'control|chr([0-9]+|[xy]):[0-9]+-[0-9]+'
)