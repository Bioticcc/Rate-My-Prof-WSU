# Rate-My-Prof-WSU (Shiny App)

A small R Shiny project where we prototype a WSU-themed "Rate My Professor" experience. The UI runs inside VS Code on WSL/Ubuntu, but it is just as happy in plain R if you install the same packages.

## System setup (Ubuntu / WSL)
1. Update packages and install system deps:
   ```bash
   sudo apt update && sudo apt install -y \
     build-essential libssl-dev libpq-dev libsqlite3-dev \
     libxml2-dev libcurl4-openssl-dev r-base wslu
   ```
2. Open R and install the libraries used in `global.R` and `server.R`:
   ```r
   install.packages(c(
     "DBI","DT","RSQLite","bslib","dbplyr","dplyr","ggplot2","httr",
     "jsonlite","plotly","pool","readr","scrypt","shiny","shinyWidgets",
     "shinyjs","shinymanager","shinyvalidate","stringr","tidyr","pdftools"
   ))
   ```

## Run the app (local R session)
1. From the repo root run `R -e "shiny::runApp('project')"`.
2. If you want the browser to open through Windows automatically, add `options(browser="/usr/bin/wslview")` to `~/.Rprofile`.
3. Logins are handled by `shinymanager`; the credentials database lives at `project/credentials.sqlite`.

## Run with Docker
```bash
docker compose up --build   # builds the image and runs the Shiny app + Mongo service
docker compose down         # stop everything when you finish
```
- The Compose file bind-mounts `./project` when you use the dev profile and keeps course/professor JSON files under a named volume.

## Data sources and mock content
- `project/data/wsu_courses.json` holds the current course catalog export. The Courses tab filters this file, with a fallback to `project/data/courses_mock.json` if the live export is missing.
- `project/data/professors_mock.json` is a lightweight placeholder list until we finish parsing the official roster (`project/data/wsufacultyroster.pdf`).
- `project/data/reviews.sqlite` stores any prototype reviews you add while testing. Delete it if you want to reset.

## Project structure
- `ui.R`, `server.R`, `global.R` - core Shiny files.
- `www/style.css` - WSU-themed styles (supports dark mode and responsive cards).
- `project/scripts/` - helper scripts to refresh catalog/professor JSON.
- `Dockerfile`, `docker-compose.yml`, `.dockerignore`, `docker/entrypoint.sh` - container tooling.

## Contributing / Git workflow
1. Pull from `main` before you start: `git pull origin main`.
2. Create a branch for your tweak (`git checkout -b feature/small-fix`).
3. Run the app, make a focused change, and commit with a short description.
4. Push your branch and open a PR so the team can review it.

## Quick troubleshooting
- "No Shiny application exists at the path" ? run the command from the repo root or point `shiny::runApp()` at `project/`.
- Browser does not open ? install `wslu` and set `options(browser="/usr/bin/wslview")`.
- Docker networking errors ? make sure Docker Desktop has WSL integration enabled for Ubuntu.
- Git remote errors ? check `git remote -v` and ensure your SSH key is added to GitHub.

Questions, ideas, or mock data updates? Open an issue on GitHub or email Jaydon (`jaydon.devictoria@wsu.edu`) and we will take a look before the next sprint.

