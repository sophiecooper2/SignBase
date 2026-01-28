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

# Set working directory
WORKDIR /home/rstudio/SignBase

# --- RSTUDIO PROJECT AUTO-LOAD CONFIG ---
RUN mkdir -p /home/rstudio/.local/share/rstudio/projects_settings
RUN echo "/project/SignBase.Rproj" > /home/rstudio/.local/share/rstudio/projects_settings/last-project-path
RUN mkdir -p /home/rstudio/.config/rstudio
RUN echo '{"initial_working_directory": "/project"}' > /home/rstudio/.config/rstudio/rstudio-prefs.json
RUN chown -R rstudio:rstudio /home/rstudio/.local /home/rstudio/.config

# Copy renv configuration files
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Restore R packages from renv.lock
# We install renv first to ensure the restore command works
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN R -e "renv::restore()"

# Copy the rest of your repository
COPY . .

# Ensure permissions are correct for the rstudio user
RUN chown -R rstudio:rstudio /home/rstudio/SignBase