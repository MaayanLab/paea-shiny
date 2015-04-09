config <- list(
    # Data path
    data_paths = list(
        genes = 'data/microtask_gene_lists.csv',
        drugs = 'data/drug_perturbation_microtask_gene_lists.csv',
        diseases = 'data/disease_signature_microtask_gene_lists.csv'
    ),
    # Maximum number of genes to keepp
    max_ngenes_tokeep = 250,
    # Maximum fraction of genes to keep
    max_fgenes_tokeep = 1,
    drop_duplicates = TRUE,
    id_filter = 'control|chr([0-9]+|[xy]):[0-9]+-[0-9]+|//',
    sigs_path = 'http://static.zero323.net/disease-signatures/',
    sigs_list_path = 'data/disease_signatures.csv',
    maxRequestSize = 120*1024^2
)
