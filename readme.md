# 📊 Rate-My-Prof-WSU (Shiny App)

This project is a Shiny application developed in **R**, running in **WSL Ubuntu** with **VS Code** integration, and version-controlled via **GitHub**.

RUN THIS IN UBUNTU TERMINAL:
sudo apt update && sudo apt install -y \
  build-essential \
  libssl-dev \
  libpq-dev \
  libsqlite3-dev \
  libxml2-dev \
  libcurl4-openssl-dev


THEN RUN THIS IN YOUR R TERMINAL:
install.packages(c(
  "shiny","bslib","shinyWidgets","shinyjs","shinyvalidate","DT",
  "shinymanager","DBI","RSQLite","RPostgres","pool","dplyr","dbplyr",
  "tidyr","readr","jsonlite","ggplot2","plotly"
))

---

## 🚀 Setup Guide

### 1. Install R in WSL
```bash
sudo apt update
sudo apt install r-base
```

Check installation:
```bash
R --version
```

### 2. Install Shiny
Start R:
```bash
R
```

Inside R:
```r
install.packages("shiny")
```

Exit R:
```r
q()
```

---

### 3. Enable Browser Auto-Open (Windows)
Install `wslu`:
```bash
sudo apt install wslu
```

Tell R to use it:
```bash
echo 'options(browser="/usr/bin/wslview")' >> ~/.Rprofile
```

--
DOWNLOAD THE REPO FROM GITHUB AND PUT IT IN YOUR LINUX/VSCODE FILE AREA
--

## 📂 Project Structure

Your project folder (`project/`) should look like this:

```
project/
 ├── app.R
 ├── ui.R
 ├── server.R
 └── www/
      └── style.css   # (optional custom styles)
```

---
### ✅ Run the App
From inside the `project/` folder:
```r
shiny::runApp(".")
```

---

## 🔧 Git + GitHub Setup

### Install Git
```bash
sudo apt install git
```

Configure:
```bash
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
```

---

### SSH Setup
Generate key (if not already):
```bash
ssh-keygen -t ed25519 -C "you@example.com"
```

Copy your key:
```bash
cat ~/.ssh/id_ed25519.pub
```
Add this to GitHub → **Settings → SSH Keys**.

Test:
```bash
ssh -T git@github.com
```

---

## 📤 Upload Project to GitHub

SIDE NOTE: at this point it aint a new repo, the repo already exists
ask GPT or something on how to download existing repo and set up the github stuff

Connect to GitHub:
```bash
git remote add origin git@github.com:Bioticcc/Rate-My-Prof-WSU.git
git branch -M main
```

If the remote isn’t empty:
```bash
git pull origin main --allow-unrelated-histories
```

Push:
```bash
git push -u origin main
```

## 🛠 Troubleshooting

### ❌ Error: “No Shiny application exists at the path …”
- Ensure the file is named **`app.R`** (capital `R`) or that your folder has `ui.R` and `server.R`.
- Run with:
  ```r
  shiny::runApp(".")
  ```

### ❌ Browser doesn’t open
- Ensure `wslu` is installed:
  ```bash
  sudo apt install wslu
  ```
- Add to `~/.Rprofile`:
  ```r
  options(browser="/usr/bin/wslview")
  ```

### ❌ GitHub “Repository not found”
- Check the remote:
  ```bash
  git remote -v
  ```
- Make sure the repo exists on GitHub and your SSH key is linked.

### ❌ Push rejected (remote has commits)
- Pull remote commits first:
  ```bash
  git pull origin main --allow-unrelated-histories
  ```
- Then push again:
  ```bash
  git push -u origin main
  ```

---

## ✅ Summary

You now have:
- R + Shiny installed in **WSL Ubuntu**  
- Auto-opening Shiny apps in your **Windows browser**  
- A proper Shiny **project structure** (`app.R`, `ui.R`, `server.R`, `www/style.css`)  
- Git + GitHub integration with **SSH authentication**  
- A clean daily workflow for commits and pushes  

You’re ready to build and share your Shiny app 🚀
