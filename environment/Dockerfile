# hash:sha256:d94c0b938de8faa0891e60f7f2c1fb0ad0c0abd67cecc1be90099693068b6657
FROM registry.codeocean.com/codeocean/r-studio:2022.07.0-548-r4.2.1-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("dplyr", "1.1.2")' \
    && Rscript -e 'remotes::install_version("ggpot2", "3.4.2")' \
    && Rscript -e 'remotes::install_version("ggrepel", "0.9.3")' \
    && Rscript -e 'remotes::install_version("tidyr", "1.3.0")'

COPY postInstall /
RUN /postInstall
