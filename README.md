# WikiLingSupp

This github-reposiotory holds the code written to create the WikiLingSupp-dataset. The script acesses for each year an archived wikipedia statistics table from 2008 to 2025 indicating the current activity surrounding each language edition of wikipedia. The script then formats the data according to year and language to derive a dataset that can be used for timeseries analysis of wikipedia support for 350 individual languages.

For this purpose, the number of articles, edits and admins is used as proximate indicators of support, and a composite index is derived by z-score normalizing each indicator, averaging, an then min-max scale the index to bound it between 0 and 1.

To acess and cite the datset, please use: Essfors, H. (2026). WikiLingSupp (1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.18742003
