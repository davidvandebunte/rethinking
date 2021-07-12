#/usr/bin/env Rscript

# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#installation-of-rstan
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
