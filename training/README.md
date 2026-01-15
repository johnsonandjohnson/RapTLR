# RapTLR Training Materials

This folder contains a complete bookdown training guide for the RapTLR package.

## Contents

1. **Introduction** - Overview of RapTLR and installation
2. **TLR Shell** - How to prepare your Word template
3. **Output Conversion** - Converting TLFs to PDF format
4. **Appendix Creation** - Using run_apdx() to add TLFs
5. **Text Replacement** - Using textReplace() for dynamic content
6. **Creating Text with R** - Using fct_var_text() for formatted descriptions

## Building the Book

### Prerequisites

```r
install.packages("bookdown")
```

### Build HTML Version

```r
bookdown::render_book("index.Rmd", "bookdown::gitbook")
```

### Build PDF Version

```r
bookdown::render_book("index.Rmd", "bookdown::pdf_book")
```

### Build All Formats

```r
bookdown::render_book("index.Rmd", "all")
```

## Output Location

The compiled book will be in the `_book/` directory.

## Customization

- Edit `_bookdown.yml` to change chapter order or book settings
- Edit `_output.yml` to customize output formats
- Add CSS styling in `style.css` (create if needed)

## Quick Start

From R console in this directory:

```r
setwd("/home/astos1/RapTLR/training")
bookdown::render_book("index.Rmd")
```

Then open `_book/index.html` in your browser.
