#FROM r-base:4.2.3
FROM registry.gitlab.com/quarto-forge/docker/rstats
USER root
USER $MAMBA_USER
ADD environment.yml /tmp
# Install conda requirements using micromamba, but avoid
# a second install of quarto as it is in our base image
RUN grep -v -- "- quarto" /tmp/environment.yml > /tmp/env.yml \
    && micromamba install -y -n base -f /tmp/env.yml \
    && micromamba clean --all --yes \
    && rm -rf /opt/conda/conda-meta
ADD docker/Rprofile.site /opt/conda/lib/R/etc/
ADD install.R /tmp
RUN source _activate_current_env.sh ; \
  conda env update -f /tmp/environment.yml ; \
  Rscript /tmp/install.R
