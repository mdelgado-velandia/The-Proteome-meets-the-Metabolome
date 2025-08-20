FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
        git \
        libxml2-dev \
        libmagick++-dev \
        curl && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install CRAN packages
RUN Rscript -e 'install.packages(c(\
    "bslib", "reactable", "rio", "tidyverse", "shinyFeedback", \
    "circlize", "BiocManager", "shinybusy", "fst"), dependencies = TRUE)'

# Install TwoSampleMR from MRC IEU universe
RUN Rscript -e 'install.packages("TwoSampleMR", repos = c(\
    "https://mrcieu.r-universe.dev", "https://cloud.r-project.org"))'

# Install Bioconductor packages
RUN Rscript -e 'BiocManager::install("ComplexHeatmap", ask = FALSE)'

# Remove default Shiny Server content
RUN rm -rf /srv/shiny-server/*

# Copy Shiny app into container
COPY /app/ /srv/shiny-server/

# Create cache directory for app
RUN mkdir /srv/shiny-server/app_cache/ && \
    chown -R shiny:shiny /srv/shiny-server/

# Declare a volume for persistent app cache
VOLUME ["/srv/shiny-server/app_cache"]

# Use the shiny user
USER shiny

# Expose Shiny Server port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
