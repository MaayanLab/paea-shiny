# nasb/nasb

FROM nasb/shiny-with-deps:latest

MAINTAINER Maciej Szymkiewicz "matthew.szymkiewicz@gmail.com"

RUN Rscript -e "devtools::install_github('nasb-course/GeoDE@fast_paea')"
RUN Rscript -e "devtools::install_github('nasb-course/nasb-microtask-viewer-helpers')"

CMD ["/usr/bin/shiny-server.sh"]
