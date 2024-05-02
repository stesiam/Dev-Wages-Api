# Use the latest Rocker R image as the base image
FROM rocker/r-ver:4.1.2


# Install necessary system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libtool \
    libz-dev \ 
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*
    
# Install renv globally
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/')"

# Set the working directory
WORKDIR /app

# Copy the renv.lock file and the Plumber API script to the working directory
COPY renv.lock .
COPY api/plumber.R api/
COPY run.R .
COPY devwages.rds .

# Restore renv packages
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/'); library(renv); renv::restore()"

# Expose the port that Plumber will run on
EXPOSE 8000

# Command to run the Plumber API
ENTRYPOINT ["Rscript", "/app/run.R"]