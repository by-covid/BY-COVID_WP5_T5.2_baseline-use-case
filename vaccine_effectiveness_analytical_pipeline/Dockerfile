FROM mambaorg/micromamba:1.4.2-bullseye-slim

USER root
RUN apt update && apt install -y --no-install-recommends \
  && apt install -y xdg-mime libgtk-3-0 libcurl4 libnss3 libv8-dev libwebkit2gtk-4.0-dev \
  && rm -rf /var/lib/apt/lists/*

USER $MAMBA_USER
COPY --chown=$MAMBA_USER:$MAMBA_USER environment.yml /tmp
COPY --chown=$MAMBA_USER:$MAMBA_USER install.R /tmp

## Install conda requirements using micromamba
RUN micromamba create -y -f /tmp/environment.yml \
    && micromamba -n vaccine_effectiveness run Rscript /tmp/install.R
    && micromamba clean --all --yes \
    && rm -rf /opt/conda/conda-meta /tmp/environment.yml \
    
RUN echo "micromamba activate vaccine_effectiveness" >> ~/.bashrc
RUN echo "export PATH=/opt/conda/envs/vaccine_effectiveness/bin:${PATH}"  >> ~/.bashrc

## Copy files
WORKDIR /home/mambauser
#USER root
RUN mkdir -p /home/mambauser/output /home/mambauser/scripts/input /home/mambauser/scripts/logs /home/mambauser/logs /home/mambauser/scripts /home/mambauser/app /home/mambauser/app/www

#USER $MAMBA_USER
COPY --chown=$MAMBA_USER:$MAMBA_USER scripts /home/mambauser/scripts/
COPY --chown=$MAMBA_USER:$MAMBA_USER app/app.R /home/mambauser/app/
COPY --chown=$MAMBA_USER:$MAMBA_USER app/www/analytical-pipeline.png /home/mambauser/app/www/

# USER root
#RUN chown -R $MAMBA_USER /pipeline
USER $MAMBA_USER
## Command
CMD ["micromamba","run","-n","vaccine_effectiveness","Rscript", "-e", "shiny::runApp('/home/mambauser/app/app.R', host = '0.0.0.0', port = 3838)"]
