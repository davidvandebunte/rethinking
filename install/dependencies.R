#/usr/bin/env Rscript

# Setup up CRAN mirror io install packages.
options(repos = list(CRAN="http://cran.rstudio.com/"))
install.packages(c('ape', 'coda', 'dagitty', 'devtools', 'ellipse', 'mvtnorm', 'styler'))
