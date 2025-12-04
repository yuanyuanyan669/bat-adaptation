# BIOS 611 Final Project – Bat Adaptation

This repo analyzes a Kaggle bat dataset to explore ecological, text-based, and categorical
patterns related to adaptation.

## Data

- Source: https://www.kaggle.com/datasets/jockeroika/bats-data
- Download `bats_information.csv` and place it at:

  `bat_adaptation/project/data/FP/bats_information.csv`

## How to Build and Run with Docker

```bash
# Clone this repo
git clone git@github.com:yuanyuanyan669/bat-adaptation.git

# Build image
cd bat-adaptation
docker build . -t bat-adaptation --platform linux/amd64

# Run container (RStudio Server on port 8787)

docker run -p 8787:8787 bat-adaptation

Open http://localhost:8787 in your browser.

Log in as user rstudio with the password printed in the terminal.

In the RStudio Terminal,cd bat-adaptation and run: make report.pdf
