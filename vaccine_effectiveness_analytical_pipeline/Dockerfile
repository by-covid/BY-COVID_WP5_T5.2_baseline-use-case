FROM registry.gitlab.com/quarto-forge/docker/rstats
LABEL org.opencontainers.image.source https://github.com/by-covid/baseline-use-case-synthetic-crate
USER root
RUN mkdir /pipeline && chmod 777 /pipeline && chown $MAMBA_USER /pipeline
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

RUN mkdir -p /pipeline/output /pipeline/input
ADD scripts /pipeline/scripts/
ADD input/vaccine_effectiveness_synthetic_pop_10k_v.1.1.0.csv /pipeline/input/
USER root 
RUN chown -R $MAMBA_USER /pipeline

VOLUME /pipeline/input
VOLUME /pipeline/output

USER $MAMBA_USER
WORKDIR /pipeline/scripts
CMD ["quarto", "render", "analytical-pipeline.QMD", "--execute"]
