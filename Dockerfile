# https://hub.docker.com/r/jupyter/r-notebook/tags/
FROM "jupyter/r-notebook:5211732116f7"

USER root

# Install `less` for the R interpreter's help functions.
RUN apt-get update && apt-get install --yes --no-install-recommends \
    less \
    && rm -rf /var/lib/apt/lists/*

# Install rstan and dependencies separately to take advantage of docker caching.
COPY install/rstan.R .
RUN R -f rstan.R
COPY install/dependencies.R .
RUN R -f dependencies.R

# Copy the package from the docker context to the container filesystem.
COPY . /rethinking/
RUN R -f /rethinking/install/rethinking.R

# Install papermill to support publishing html files from notebooks.
RUN python3 -m pip install papermill

USER jovyan
