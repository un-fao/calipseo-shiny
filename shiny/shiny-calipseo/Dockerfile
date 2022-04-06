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
# RUN chown shiny:shiny /var/lib/shiny-server
#COPY install_geospatial.sh /srv/shiny-server/install_geospatial.sh
#RUN ls -ls /srv/shiny-server/
#RUN chmod +x /srv/shiny-server/install_geospatial.sh
#RUN ./srv/shiny-server/install_geospatial.sh
RUN /rocker_scripts/install_geospatial.sh


#R shiny server
#---------------------------------------------------------------------------------------
# RUN chown shiny:shiny /var/lib/shiny-server
RUN chown shiny:shiny /srv/shiny-server

#Git clone shiny app
#---------------------------------------------------------------------------------------
ADD ./shiny/shiny-calipseo /srv/shiny-server/
#check dir is created
RUN ls -ls /srv/shiny-server

RUN mkdir -p /etc/shiny-server
COPY shinyconfigs/config.yml /etc/shiny-server/config.yml
RUN ls -ls /etc/shiny-server
RUN chmod -R a+w /srv/shiny-server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN ls -ls /srv/shiny-server

#R packages
#---------------------------------------------------------------------------------------
# install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes','testthat','jsonlite','yaml'), repos='https://cran.r-project.org/')"
# install dependencies of the app
RUN R -e "source('./srv/shiny-server/install.R')"
 
# EXPOSE 3838
EXPOSE 8080

#RUN apt-get install -y curl
#CMD ["R", "-e shiny::runApp('/srv/shiny-server', port=8080, host='0.0.0.0')"]
CMD ["/init"] 