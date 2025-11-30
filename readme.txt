Rate-My-Prof-WSU (Shiny App)

Overview
- Shiny application built in R for WSU course and professor reviews. Developed in WSL/Ubuntu with VS Code and version-controlled via GitHub.

System prerequisites (Ubuntu)
sudo apt update && sudo apt install -y \
  build-essential \
  libssl-dev \
  libpq-dev \
  libsqlite3-dev \
  libxml2-dev \
  libcurl4-openssl-dev \
  r-base \
  wslu

R packages (install from an R session)
install.packages(c(
  "DBI","DT","RSQLite","bslib","dbplyr","dplyr","ggplot2","httr",
  "jsonlite","plotly","pool","readr","scrypt","shiny","shinyWidgets",
  "shinyjs","shinymanager","shinyvalidate","stringr","tidyr","pdftools"
))

Run the app
- From the repository root: R -e "shiny::runApp('project')"
- Optional browser integration for Windows: echo 'options(browser="/usr/bin/wslview")' >> ~/.Rprofile

Data scripts
- Refresh courses: Rscript project/scripts/fetch_wsu_courses.R (writes project/data/wsu_courses.json; uses httr/jsonlite/dplyr).
- Build professor list: Rscript project/scripts/extract_wsu_professors.R (reads project/data/wsufacultyroster.pdf; uses pdftools/stringr/dplyr/jsonlite).

Docker
- Build: docker build -t rmp-shiny:latest .
- Run with persisted data: docker run --rm -p 3838:3838 -v rmp_data:/app/project/data rmp-shiny:latest
- Compose: docker compose up --build (or docker compose --profile dev up --build to bind-mount ./project).

Git quickstart (push and pull)
- Set remote once: git remote add origin git@github.com:Bioticcc/Rate-My-Prof-WSU.git && git branch -M main
- Pull latest changes: git pull origin main
- Commit updates: git add . && git commit -m "Your message"
- Push: git push -u origin main

Troubleshooting
- "No Shiny application exists at the path ...": run from the project directory and ensure app.R or ui.R/server.R exist.
- Browser does not open: confirm wslu is installed and options(browser="/usr/bin/wslview") is in ~/.Rprofile.
- Git remote errors: check git remote -v and verify your GitHub SSH key is added.
