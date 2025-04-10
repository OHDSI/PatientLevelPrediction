FROM docker.io/rocker/r-ver:4.4 AS build

RUN --mount=type=secret,id=build_github_pat export GITHUB_PAT=$(cat /run/secrets/build_github_pat)

ARG GIT_BRANCH='develop'
ARG GIT_COMMIT_ID_ABBREV

RUN apt-get -y update && apt-get install -y \
      default-jre \
      default-jdk \
      libssl-dev  \
      python3-dev \
      curl \
      ca-certificates \
      libpcre2-dev \
      libdeflate-dev \
      liblzma-dev \
      libbz2-dev \
      libicu-dev \
      --no-install-recommends \
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/*
RUN R CMD javareconf

RUN echo 'options(repos = c(CRAN = "https://p3m.dev/cran/__linux__/noble/latest"))' >>"${R_HOME}/etc/Rprofile.site"

RUN install2.r -n -1 \
        remotes \
        CirceR \
        Eunomia \
        duckdb \
        CohortGenerator \
    && installGithub.r \
        OHDSI/ROhdsiWebApi
        
RUN Rscript -e "DatabaseConnector::downloadJdbcDrivers(dbms='all', pathToDriver='/database_drivers/')"
ENV DATABASECONNECTOR_JAR_FOLDER=/database_drivers/

# install uv
ADD https://astral.sh/uv/0.6.13/install.sh /uv-installer.sh
RUN sh /uv-installer.sh \
    && rm /uv-installer.sh
ENV PATH="/root/.local/bin/:$PATH"

# install python dependancies
RUN uv pip install --system --no-cache-dir --break-system-packages \
    scikit-learn \
    numpy \
    scipy \
    && rm -rf /root/.cache/pip

RUN Rscript -e "ref <- Sys.getenv('GIT_COMMIT_ID_ABBREV', unset = Sys.getenv('GIT_BRANCH')); remotes::install_github('ohdsi/PatientLevelPrediction', ref=ref, dependencies = TRUE)"

FROM docker.io/rocker/rstudio:4.4
#
COPY --from=build /usr/local/lib/python3.12/dist-packages /usr/local/lib/python3.12/dist-packages
COPY --from=build /database_drivers /database_drivers
COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=build /usr/local/lib/R/library /usr/local/lib/R/library

ENV RETICULATE_PYTHON=/usr/bin/python3
# runtime dependanceis
RUN apt-get -y update && apt-get install -y \
      default-jre \
      default-jdk \
      libssl3 \
      python3-dev \
      libpcre2-8-0 \
      libdeflate0 \
      liblzma5 \
      libbz2-1.0 \
      libicu70 \
      --no-install-recommends \
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/* \
      && R CMD javareconf

