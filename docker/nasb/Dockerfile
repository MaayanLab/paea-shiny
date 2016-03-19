# nasb/nasb

FROM nasb/shiny-with-deps:latest

MAINTAINER Maciej Szymkiewicz "matthew.szymkiewicz@gmail.com"

RUN Rscript -e "devtools::install_github('nasb-course/nasb-microtask-viewer-shiny/geode')"
RUN Rscript -e "devtools::install_github('nasb-course/nasb-microtask-viewer-shiny/helpers')"
RUN Rscript -e "devtools::install_github('nasb-course/nasb-microtask-viewer-shiny/config')"

CMD ["/usr/bin/shiny-server.sh"]
