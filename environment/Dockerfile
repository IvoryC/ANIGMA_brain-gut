# hash:sha256:ccc29378ee37b0d0e7de092a611519eccee8058fdfd5fdc478138e4c230c0e9c
FROM registry.codeocean.com/codeocean/r-studio:2022.07.0-548-r4.2.1-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("dplyr", "1.1.3")' \
    && Rscript -e 'remotes::install_version("ggplot2", "3.4.3")' \
    && Rscript -e 'remotes::install_version("ggrepel", "0.9.3")' \
    && Rscript -e 'remotes::install_version("tidyr", "1.3.0")'

COPY postInstall /
RUN /postInstall
