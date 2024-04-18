# suggested build command
# cd to directory with this file
# docker build -t biolockjdevteam/anigma:v1 .
# OR (if running on an M1 mac)
# docker build --platform linux/amd64 -t biolockjdevteam/anigma:v1 .

FROM rocker/verse:4.3.1

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("ggrepel", "0.9.3")' \
    && Rscript -e 'remotes::install_version("markdown", "1.9")' \
    && Rscript -e 'remotes::install_version("vegan", "2.6-4")' \
    && Rscript -e 'source("http://bioconductor.org/biocLite.R"); biocLite("phyloseq")'

