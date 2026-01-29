# Use Rocker's RStudio image as the base
FROM rocker/geospatial:4.4.1

# Install Quarto and system dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    gdebi-core \
    libglpk-dev \
    && curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb \
    && gdebi --non-interactive quarto-1.3.450-linux-amd64.deb \
    && rm quarto-1.3.450-linux-amd64.deb \
    && rm -rf /var/lib/apt/lists/*
    
# --- RSTUDIO PROJECT AUTO-LOAD CONFIG ---
RUN mkdir -p /home/rstudio/.local/share/rstudio/projects_settings
RUN echo "/project/SignBase.Rproj" > /home/rstudio/.local/share/rstudio/projects_settings/last-project-path
RUN mkdir -p /home/rstudio/.config/rstudio
RUN echo '{"initial_working_directory": "/project"}' > /home/rstudio/.config/rstudio/rstudio-prefs.json
RUN chown -R rstudio:rstudio /home/rstudio/.local /home/rstudio/.config

# --- TERMINAL CONFIG ---
RUN echo 'cd /project' >> /home/rstudio/.bashrc
RUN echo "source /opt/conda/etc/profile.d/conda.sh" >> /home/rstudio/.bashrc

#  Set up Project
WORKDIR /project
COPY . /project

# ---  RENV RESTORE ---

# A. Set RENV paths to location OUTSIDE /project 
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_PATHS_CACHE=/opt/renv/cache

#  Create directories and give 'rstudio' user permission
RUN mkdir -p /opt/renv && chown -R rstudio:rstudio /opt/renv

# Install renv and restore
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "options(renv.config.cache.symlinks = FALSE); renv::restore(prompt = FALSE)"


