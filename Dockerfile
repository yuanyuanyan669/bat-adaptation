# Dockerfile

# R + RStudio + tidyverse + LaTeX (for PDF rendering)
FROM rocker/verse:4.4.0

# Install additional R packages required by the project
RUN Rscript -e "install.packages(c( \
    'tidyverse', 'janitor', 'ggthemes', 'viridis', 'ggrepel', 'patchwork', 'cowplot', \
    'tidytext', 'text2vec', 'irlba', 'Matrix', 'naniar', 'plotly', 'uwot', 'scales', \
    'clustMixType', 'klaR', 'rmarkdown', 'knitr' \
  ), repos = 'https://cloud.r-project.org')"

# working directory inside the container
WORKDIR /home/rstudion

# Copy project files into the image
COPY . /home/rstudio/bat-adaptation

RUN chown -R rstudio:rstudio /home/rstudio/bat-adaptation
