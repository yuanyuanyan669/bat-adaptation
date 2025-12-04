# ---------------------------
# Makefile for BIOS611 Project
# ---------------------------

# Default target
all: report.pdf

# 1. Generate all figures by running analysis.R
figures:
	Rscript project/code/analysis.R

# 2. Knit the final report (depends on figures and report.Rmd)
report.pdf: report.Rmd figures
	Rscript -e "rmarkdown::render('report.Rmd', output_format = 'pdf_document')"

# 3. Clean outputs
clean:
	rm -f report.pdf
	rm -f project/outputs/figures_FP/*.png

.PHONY: all figures clean

