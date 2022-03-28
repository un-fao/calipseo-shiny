FROM rocker/shiny:4.0.5

MAINTAINER Emmanuel Blondel "emmanuel.blondel@fao.org"

# system libraries of general use
#---------------------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    sudo \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git 

#Install geospatial deps over shiny server
#---------------------------------------------------------------------------------------
RUN /install_geospatial.sh
RUN chown shiny:shiny /var/lib/shiny-server

# install dependencies of the app
#R CRAN packages
#---------------------------------------------------------------------------------------
#dev
RUN R -e "install.packages(c('remotes'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('testthat'), repos='https://cran.r-project.org/')"
#shiny
RUN R -e "install.packages(c('shiny'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinydashboard'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinydashboardPlus'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinyWidgets'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinycssloaders'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinyjs'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinyFiles'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('htmltools'), repos='https://cran.r-project.org/')"
#sys
RUN R -e "install.packages(c('openssl'), repos='https://cran.r-project.org/')"
#dbms
RUN R -e "install.packages(c('DBI'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('RMySQL'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('RMariaDB'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('pool'), repos='https://cran.r-project.org/')"
#format bindings
RUN R -e "install.packages(c('yaml'), repos='https://cran.r-project.org/')"  
RUN R -e "install.packages(c('mime'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('jsonlite'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('readr'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('readxl'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('writexl'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('openxlsx'), repos='https://cran.r-project.org/')"
#RUN R -e "install.packages(c('sf'), repos='https://cran.r-project.org/')"
#RUN R -e "install.packages(c('sp'), repos='https://cran.r-project.org/')" 
#processing
RUN R -e "install.packages(c('dplyr'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('tidyr'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('reshape2'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('uuid'), repos='https://cran.r-project.org/')" 
#ui
RUN R -e "install.packages(c('RColorBrewer'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('data.table'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('DT'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('plotly'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('leaflet'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('leaflet.minicharts'), repos='https://cran.r-project.org/')" 

#R GitHub packages (with release in CRAN, but not re-published yet)
#---------------------------------------------------------------------------------------
#None

#R GitHub packages (not yet released in CRAN)
#---------------------------------------------------------------------------------------
#None

#Git clone shiny app
#---------------------------------------------------------------------------------------
ADD ./shiny/shiny-calipseo /srv/shiny-server/
#check dir is created
RUN ls -ls /srv/shiny-server

RUN mkdir -p /etc/shiny-server
COPY shinyconfigs/calipseo_shiny_config.yml /etc/shiny-server/config.yml
RUN ls -ls /etc/shiny-server
RUN chmod -R a+w /srv/shiny-server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN ls -ls /srv/shiny-server
 
# EXPOSE 3838
EXPOSE 8080

#RUN apt-get install -y curl
#CMD ["R", "-e shiny::runApp('/srv/shiny-server', port=8080, host='0.0.0.0')"]
CMD ["/init"] 
