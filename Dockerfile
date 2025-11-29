FROM rocker/r-ver:4.3.3

ENV SHINY_HOST=0.0.0.0 \
    SHINY_PORT=3838 \
    RMP_DATA_DIR=/app/project/data \
    RMP_REVIEWS_DB_PATH=/app/project/data/reviews.sqlite \
    RMP_CREDENTIALS_DB_PATH=/app/project/data/credentials.sqlite \
    RMP_CREDENTIALS_PASSPHRASE=change-this-passphrase

RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev libpq-dev libsqlite3-dev libxml2-dev libcurl4-openssl-dev libscrypt-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY project /app/project
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh

RUN chmod +x /usr/local/bin/entrypoint.sh && \
    install2.r --error --skipinstalled \
      shiny bslib shinyWidgets shinyjs shinyvalidate DT \
      shinymanager DBI RSQLite RPostgres pool dplyr dbplyr scrypt \
      tidyr readr jsonlite ggplot2 plotly

EXPOSE 3838

CMD ["entrypoint.sh"]
