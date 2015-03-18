# nasb/shiny-with-deps 

FROM rocker/shiny:latest

MAINTAINER Maciej Szymkiewicz "matthew.szymkiewicz@gmail.com"

RUN Rscript -e "install.packages('stringi')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('data.table')"
RUN Rscript -e "install.packages('ggvis')"
RUN Rscript -e "install.packages('devtools')"

RUN Rscript -e "source('http://bioconductor.org/biocLite.R'); biocLite('preprocessCore')"


