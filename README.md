# Big-Ahh-R-Repo

A starter repository for learning R and completing school assignments with R Markdown.

## Included resources

- `guide/r-basics-guide.tex` - comprehensive LaTeX guide covering core R basics and R Markdown workflow.
- `assignment_template/` - reusable folder structure and starter files for assignments.

## Build the PDF guide

From the repository root:

```bash
pdflatex -output-directory guide guide/r-basics-guide.tex
```

This produces `guide/r-basics-guide.pdf`.

## Assignment workflow

1. Copy `assignment_template/` into a new assignment folder.
2. Place raw data in `data/raw/`.
3. Write helper code in `analysis.R`.
4. Write and render your report from `report.Rmd`.
