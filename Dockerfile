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

# Create app directory
WORKDIR /app

# Copy your Shiny app code into the image
COPY app/ /app/

# Install renv
RUN Rscript -e "install.packages('renv', repos = 'https://cran.rstudio.com')"

# Initialize renv and install required packages with pinned versions
RUN Rscript -e "\
  renv::init(bare = TRUE); \
  renv::install('shiny@1.9.1'); \
  renv::install('bslib@0.8.0'); \
  renv::install('reactable@0.4.4'); \
  renv::install('rio@1.2.3'); \
  renv::install('tidyverse@2.0.0'); \
  renv::install('shinyFeedback@0.4.0'); \
  renv::install('circlize@0.4.16'); \
  renv::install('fst@0.9.18'); \
  renv::install('BiocManager'); \
  BiocManager::install('ComplexHeatmap', ask = FALSE, update = TRUE); \
  renv::snapshot(prompt = FALSE)"

# Create cache directory and fix permissions
RUN mkdir -p /app/app_cache && \
    chown -R shiny:shiny /app

# Declare volume for shared/persistent app cache
VOLUME ["/app/app_cache"]

# Use non-root user for security
USER shiny

# Expose Shiny port
EXPOSE 3838

# Launch the Shiny server
CMD ["/usr/bin/shiny-server"]
