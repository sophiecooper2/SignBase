# Use Rocker's RStudio image as the base
FROM rocker/rstudio:4.4.1

# Install Quarto and system dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    gdebi-core \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    && curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb \
    && gdebi --non-interactive quarto-1.3.450-linux-amd64.deb \
    && rm quarto-1.3.450-linux-amd64.deb \
    && rm -rf /var/lib/apt/lists/*


# Set working directory
WORKDIR /home/rstudio/SignBase

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