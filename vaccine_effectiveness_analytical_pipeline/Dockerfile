#FROM r-base:4.2.3
FROM registry.gitlab.com/quarto-forge/docker/rstats
USER root
#RUN apt update \
#    && apt install -y curl libfreetype6-dev libssl-dev libcurl4-openssl-dev libxml2-dev libfontconfig1-dev cmake build-essentials \
#    && rm -rf /var/lib/apt/lists/* \
#    && curl -L $(curl https://quarto.org/docs/download/_prerelease.json | grep -oP "(?<=\"download_url\":\s\")https.*${ARCH}\.deb") -o /tmp/quarto.deb \
#    && dpkg -i /tmp/quarto.deb \
#    && rm /tmp/quarto.deb
USER $MAMBA_USER
ADD environment.yml /tmp
RUN micromamba install -y -n base -f /tmp/environment.yml \
    && micromamba clean --all --yes \
    && rm -rf /opt/conda/conda-meta /tmp/env.yaml
#ADD docker/Rprofile.site /home/$MAMBA_USER/.Rprofile
ADD docker/Rprofile.site /opt/conda/lib/R/etc/
ADD install.R /tmp
RUN source _activate_current_env.sh ; \
  conda env update -f /tmp/environment.yml ; \
  Rscript /tmp/install.R
