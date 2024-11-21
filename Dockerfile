FROM rocker/shiny:4.4.1

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git libxml2-dev libmagick++-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Command to install standard R packages from CRAN; enter the list of required packages for your app here
RUN Rscript -e 'install.packages(c("bslib","reactable", "rio", "tidyverse", "shinyFeedback", "circlize", "BiocManager"), dependencies = TRUE)'

# Command to install packages from Bioconductor; enter the list of required Bioconductor packages for your app here
RUN Rscript -e 'BiocManager::install(c("ComplexHeatmap"),ask = F)'

RUN rm -rf /srv/shiny-server/*
COPY /app/ /srv/shiny-server/

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]