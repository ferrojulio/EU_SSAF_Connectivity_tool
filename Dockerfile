# Use a base R image with Shiny Server
FROM rocker/shiny:4.4.1

# Set working directory
WORKDIR /srv/shiny-server

# Install system dependencies
# These are based on common R package requirements and the deployment notes
RUN apt-get update -qq && apt-get install -y \
    libpq-dev \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libjpeg-dev \
    libtiff-dev \
    libpng-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libglpk-dev \
    libgmp-dev \
    libxt-dev \
    libssh2-1-dev \
    libgit2-dev \
    libv8-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libgeos-dev \
    libproj-dev \
    # Clean up apt cache to reduce image size
    && rm -rf /var/lib/apt/lists/*

# Copy renv files first to leverage Docker cache
COPY renv.lock .Rprofile ./

# Restore R packages using renv
# This will install all R package dependencies specified in renv.lock
RUN R -e "install.packages(\'renv\', repos = \'https://cloud.r-project.org/\')" && \
    R -e "renv::restore()"

# Copy the rest of the application files
COPY . /srv/shiny-server/

# Expose Shiny Server port
EXPOSE 3838

# Command to run Shiny Server
CMD ["/usr/bin/shiny-server"]
