# templates
All templates for writing LibRArs publications.

## R package
All available templates in LibRArs are available in this R package, published under the name: `librarstemplates`.
To install the package, use:

```r
devtools::install_github("librars/templates")
```

## Available templates
Following is the list of available templates:

| Class name | Template name | Format |
|------------|---------------|--------|
| book       | `book_tex`    | Latex  |
|            | `book_html`   | HTML   |

# API call
A correct call:

```
bookdown::render_book("./index.Rmd", rmarkdown::pdf_document())
```

Or:

```
bookdown::render_book(".", rmarkdown::pdf_document())
```
