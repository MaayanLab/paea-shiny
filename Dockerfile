FROM rocker/shiny:latest

MAINTAINER Maciej Szymkiewicz "matthew.szymkiewicz@gmail.com"

RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "source('http://bioconductor.org/biocLite.R'); biocLite('preprocessCore')"
RUN Rscript -e "devtools::install_github('nasb-course/GeoDE@fast_paea')"
RUN Rscript -e "devtools::install_github('nasb-course/nasb-microtask-viewer-helpers')"

CMD ["/usr/bin/shiny-server.sh"]
