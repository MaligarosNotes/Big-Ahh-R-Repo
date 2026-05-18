# Assignment Template (R + R Markdown)

Use this folder as a starter for school assignments.

## Structure

- `analysis.R` - reusable data loading and analysis functions
- `report.Rmd` - your written submission with code and results
- `data/raw/` - original input files
- `data/processed/` - cleaned/generated files
- `figures/` - exported plots for reports/slides

## Quick start

1. Open this repository in RStudio.
2. Copy `assignment_template` to a new folder for your assignment.
3. Put your dataset in `data/raw/`.
4. Edit `analysis.R` and `report.Rmd`.
5. Render your report:

```r
rmarkdown::render("report.Rmd")
```

## Suggested packages

```r
install.packages(c("rmarkdown", "knitr", "tidyverse"))
```
