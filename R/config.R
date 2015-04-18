config <- list(
    app_name = 'NASB Microtask Viewer',
    # Data path
    datasets = list(
        genes = list(name='Genes', path='data/microtask_gene_lists.csv'),
        drugs = list(name='Drugs', path='data/drug_perturbation_microtask_gene_lists.csv'),
        diseases = list(name='Diseases', path='data/disease_signature_microtask_gene_lists.csv')
    ),
    # Maximum number of genes to keepp
    max_ngenes_tokeep = 250,
    # Maximum fraction of genes to keep
    max_fgenes_tokeep = 1,
    drop_duplicates = TRUE,
    id_filter = 'control|chr([0-9]+|[xy]):[0-9]+-[0-9]+|//',
    sigs_path = 'http://static.zero323.net/disease-signatures/',
    sigs_list_path = 'data/disease_signatures.csv',
    maxRequestSize = 120*1024^2,
    navbar_tabs = list(
       list(name="Home", href='/'),
       list(name="Analyze", href='/analyze/'),
       list(name="About", href='/about/')
    )

)
