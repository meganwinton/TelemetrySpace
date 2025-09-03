FROM rocker/r2u

LABEL \
    org.opencontainers.image.authors="Michael O'Brien <mike@obrien.page>, Benjamin Hlina <benjamin.hlina@gmail.com>" \
    org.opencontainers.image.version="1.2.0" \
    org.opencontainers.image.source="https://github.com/trackyverse/TelemetrySpace" \
    org.opencontainers.image.licenses="GPL-3.0"

RUN apt update && apt upgrade -y
RUN Rscript -e 'install.packages("rstan", \
  repos = c("https://stan-dev.r-universe.dev", getOption("repos")))'
RUN Rscript -e 'install.packages("TelemetrySpace", \
  repos = c("https://trackyverse.r-universe.dev", getOption("repos")), \
  dependencies = TRUE)'
RUN Rscript -e "install.packages(c('ggpubr', 'httpgd'))"

# Set default values used in the httpgd::hgd() function.
RUN echo 'options(httpgd.host = "0.0.0.0", httpgd.port = 8888)' >> /etc/R/Rprofile.site

EXPOSE 8888

CMD [ "R" ]

# docker build -t telemetryspace .
# docker run --rm -it -p 8888:8888 telemetryspace
# run "httpgd::hgd()" in R to start the graphics device