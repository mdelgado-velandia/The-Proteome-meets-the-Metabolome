# Base image with R and Shiny Server
FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
        git \
        libxml2-dev \
        libmagick++-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install 'remotes' for installing specific versions of CRAN packages
RUN Rscript -e "install.packages('remotes', dependencies = TRUE)"

# Install specific versions of CRAN packages (with dependencies = TRUE)
RUN Rscript -e "remotes::install_version('shiny', version = '1.9.1', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('bslib', version = '0.8.0', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('reactable', version = '0.4.4', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('rio', version = '1.2.3', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('tidyverse', version = '2.0.0', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('shinyFeedback', version = '0.4.0', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('circlize', version = '0.4.16', repos = 'https://cran.rstudio.com', dependencies = TRUE)" && \
    Rscript -e "remotes::install_version('fst', version = '0.9.18', repos = 'https://cran.rstudio.com', dependencies = TRUE)"

# Install Bioconductor and specific version of ComplexHeatmap
RUN Rscript -e "install.packages('BiocManager', repos = 'https://cran.rstudio.com')" && \
    Rscript -e "BiocManager::install('ComplexHeatmap', ask = FALSE, update = TRUE)"

# Clean previous app content and copy your Shiny app
RUN rm -rf /srv/shiny-server/*
COPY /app/ /srv/shiny-server/

# Create app cache directory and set correct permissions
RUN mkdir /srv/shiny-server/app_cache && \
    chown -R shiny:shiny /srv/shiny-server/

# Declare a volume for persistent/shared storage
VOLUME ["/srv/shiny-server/app_cache"]

# Use the non-root 'shiny' user
USER shiny

# Expose default Shiny port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]
