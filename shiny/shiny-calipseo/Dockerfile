FROM rocker/r-ver:4.0.5

MAINTAINER Emmanuel Blondel "emmanuel.blondel@fao.org"

# system libraries of general use
#---------------------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git 

# install dependencies of the app
#R CRAN packages
#---------------------------------------------------------------------------------------
#dev
RUN R -e "install.packages(c('remotes'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('testthat'), repos='https://cran.r-project.org/')"
#shiny
RUN R -e "install.packages(c('shiny'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinydashboard'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinycssloaders'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinyjs'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('shinyFiles'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('htmltools'), repos='https://cran.r-project.org/')"
#sys
RUN R -e "install.packages(c('openssl'), repos='https://cran.r-project.org/')"
#dbms
RUN R -e "install.packages(c('DBI'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('RMySQL'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('pool'), repos='https://cran.r-project.org/')"
#format bindings
RUN R -e "install.packages(c('yaml'), repos='https://cran.r-project.org/')"  
RUN R -e "install.packages(c('mime'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('jsonlite'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('readr'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('readxl'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('writexl'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('openxlsx'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('sf'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('sp'), repos='https://cran.r-project.org/')" 
#processing
RUN R -e "install.packages(c('dplyr'), repos='https://cran.r-project.org/')"
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
RUN git -C /root/ clone https://bitbucket.org/cioapps/fao-calipseo-stats.git && echo "OK!"
RUN mkdir -p /srv/shiny/
RUN ln -s /root/shiny/shiny-calipseo /srv/shiny/shiny-calipseo

ENV SHINY_CONFIG=""
RUN mkdir -p /etc/shiny/shiny-calipseo
COPY $SHINY_CONFIG /etc/shiny/shiny-calipseo/config.yml
 
EXPOSE 3838

RUN apt-get install -y curl
CMD ["R", "-e shiny::runApp('/srv/shiny/shiny-calipseo', port=3838, host='0.0.0.0')"]
#CMD ["R", "-e shiny::runApp('/srv/shiny/shiny-calipseo')"]