# Machine learning approaches for electronic health records phenotyping: A methodical review

R code for replication of the analyses in our systematic review of the machine learning based phentoyping literature.  

## Aricle Information

Machine learning approaches for electronic health records phenotyping: A methodical review

Siyue Yang, Paul Varghese, Ellen Stephenson, Karen Tu, Jessica Gronsbell

medRxiv 2022.04.23.22274218

doi: https://doi.org/10.1101/2022.04.23.22274218

## Data

Basic information on all articles identified from our search of PubMed and Web of Science as well as detailed recording of 37 variables on 100 articles selected for the review.

- [Article Review Data](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/data/Review_Data_0923222.csv)

## Analysis Results

Results from the key analysis steps of our review.

- [Merging](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/report/1-merging.pdf): Results of merging articles from PubMed and Web of Science. 

- [Exclusion](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/report/2-exclusion.pdf): Results from excluding articles not meeting the selection criteria.

- [Analysis](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/report/3-analysis.pdf): Results summarizing characteristics of the 100 articles selected for our review.

## Analysis 

R code for the key analysis steps of our review.

We performed all analyses using R version 4.1.0 and used a variety of packages for data processing, graphing, and reporting.

- [1-merging.Rmd](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/rmd/1-merging.Rmd): Merges search results from PubMed and Web of Science. 

- [2-exclusion.Rmd](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/rmd/2-exclusion.Rmd): Excludes articles not meeting our selection criteria.

- [3-analysis.Rmd](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/rmd/3-analysis.Rmd): Analysis of the selected articles.

- [analysis_functions.R](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/rmd/analysis_functions.R): Helper functions for analysis. 

### Data for Analysis

- [PubMed data](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/data/pubmed20220414.csv): All articles extracted from PubMed.  

- [Web of Science data](https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/data/webofscience20220414.csv): All articles extracted from Web of Science.

- [AMIA Publication Date data]([https://github.com/siyueyang/ml-phenotyping-review-2022/blob/main/data/annotations050822.csv](https://github.com/jlgrons/ML-Phenotyping-Review-Final-2022/blob/main/Data/amia20220414.csv): Correct publication dates for AMIA articles; used to resolve a publication date conflict observed in PubMed.

