# hash:sha256:c5e750400e90d3a9d45a57b0333df2586bcec9e79a0414f05a9c945b996231b4
FROM registry.codeocean.com/codeocean/r-studio:2022.07.0-548-r4.2.1-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        openjdk-8-jdk=8u372-ga~us1-0ubuntu1~18.04 \
        pandoc=1.19.2.4~dfsg-1build4 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'remotes::install_version("dplyr", "1.1.3")' \
    && Rscript -e 'remotes::install_version("ggplot2", "3.4.3")' \
    && Rscript -e 'remotes::install_version("ggrepel", "0.9.3")' \
    && Rscript -e 'remotes::install_version("tidyr", "1.3.0")'

COPY postInstall /
RUN /postInstall
