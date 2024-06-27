FROM inseefrlab/onyxia-rstudio:r4.3.2-2024.02.13

RUN Rscript -e 'install.packages("shinydashboard", repos = "https://cran.r-project.org")'

# Add files necessary for the running app
ADD correction/global.R .
ADD correction/ui.R .
ADD correction/server.R .
ADD correction/module_accueil.R .
ADD correction/module_cie_aeriennes.R .
ADD correction/module_freq_fr.R .
ADD correction/module_comparaison_annuelle.R .
ADD correction/module_fret.R .
ADD correction/module_liaisons.R .
ADD correction/module_zone.R .
ADD correction/sources.yml .
COPY correction/R R/
COPY . .
ADD renv.lock .
ADD renv .

# Expose port where shiny app will broadcast
ARG SHINY_PORT=3838
EXPOSE $SHINY_PORT
RUN echo "local({options(shiny.port = ${SHINY_PORT}, shiny.host = '0.0.0.0')})" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e "renv::restore()"

# Endpoint
CMD ["Rscript", "-e", "shiny::runApp()"]
