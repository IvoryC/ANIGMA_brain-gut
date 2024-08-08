# hash:sha256:2131dacc3ae5d1b415a487aa2092187e6d2274c57ae74b765dd01537d19b5faf
FROM registry.codeocean.com/codeocean/r-studio:2022.07.0-548-r4.2.1-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        openjdk-8-jdk=8u372-ga~us1-0ubuntu1~18.04 \
        pandoc=1.19.2.4~dfsg-1build4 \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'remotes::install_version("cowplot", "1.1.3")' \
    && Rscript -e 'remotes::install_version("dplyr", "1.1.4")' \
    && Rscript -e 'remotes::install_version("ggplot2", "3.5.1")' \
    && Rscript -e 'remotes::install_version("ggrepel", "0.9.3")' \
    && Rscript -e 'remotes::install_version("ggsignif", "0.6.4")' \
    && Rscript -e 'remotes::install_version("markdown", "1.9")' \
    && Rscript -e 'remotes::install_version("rmarkdown", "2.27")' \
    && Rscript -e 'remotes::install_version("tidyr", "1.3.0")' \
    && Rscript -e 'remotes::install_version("vegan", "2.6-4")'

RUN Rscript -e 'options(warn=2); install.packages("BiocManager")'
RUN Rscript -e 'options(warn=2); BiocManager::install(c( \
        "phyloseq" \
    ))' # Original versions: 1.42.0

COPY postInstall /
RUN /postInstall
