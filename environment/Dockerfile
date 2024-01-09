# hash:sha256:1fdaaac2d5b87c0465e04d9724e882be148247785b1b1775f6130c047a99a386
FROM registry.codeocean.com/codeocean/r-studio:2022.07.0-548-r4.2.1-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        openjdk-8-jdk=8u372-ga~us1-0ubuntu1~18.04 \
        pandoc=1.19.2.4~dfsg-1build4 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'remotes::install_version("dplyr", "1.1.4")' \
    && Rscript -e 'remotes::install_version("ggplot2", "3.4.4")' \
    && Rscript -e 'remotes::install_version("ggrepel", "0.9.3")' \
    && Rscript -e 'remotes::install_version("markdown", "1.9")' \
    && Rscript -e 'remotes::install_version("tidyr", "1.3.0")' \
    && Rscript -e 'remotes::install_version("vegan", "2.6-4")'

COPY postInstall /
RUN /postInstall
