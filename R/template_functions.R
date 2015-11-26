# Useful functions:
#
# bind_cv_sections: function to join diferent rmd documents in one rmd document
#
# cvR_format_pdf: format function to create the output.


#' Bind Rmd files
#'
#' Function to bind rmd files containing CV sections
#'
#' @param sections List containing the names and rutes (as character) of
#'   files to bind. Default to \code{list.files(".", ".Rmd")}.
#'
#' @param name Name and rute of the returned file. Default to
#'   \code{paste(Sys.Date(), '_CV.Rmd', sep = '')}
#'
#' @return A file resulting of the bind of the selected Rmd files
#'
#' @export
bind_cv_sections <- function(sections = list.files(".", ".Rmd"),
                             name = paste(Sys.Date(), '_CV.Rmd', sep = '')) {
  # Para cada uno de los archivos en la lista (secciones)
  # saco el texto, localizo el yaml, lo elimino y escribo
  # la sección en el archivo anterior
  tmp <- lapply(sections, function(x){
    section <- readLines(x)
    yaml_loc <- grep("---", section)
    # TO DO, check if yaml_loc is longer than 1 to remove the yaml preamble
    section <- section[-c(yaml_loc[1]:yaml_loc[2])]
    write(section, sep = '/n', file = name, append = TRUE)
  })
  print(paste(name, ' file created in working directory'))
}

#' cvR format (PDF)
#'
#' Template for creating a CV in PDF format from Rmd file,
#'   adapted from \code{\link[rmarkdown]{tufte_handout}}
#'
#' @param fig_width Default width (in inches) for figures.
#'
#' @param fig_height Default height (in inches) for figures.
#'
#' @param fig_crop \code{TRUE} to automatically apply the \code{pdfcrop} utility
#'   (if available) to pdf figures.
#'
#' @param dev Graphics device to use for figure output (defaults to pdf).
#'
#' @param highlight Syntax highlighting style. Default to "pygments", see
#'   \code{\link[rmarkdown]{pdf_document}} for supported styles. Pass \code{NULL}
#'   to prevent syntax hightlighting.
#'
#' @param keep_tex Keep the intermediate tex file used in the conversion to PDF.
#'
#' @param number_sections \code{TRUE} to number sections headings.
#'
#' @param includes Named list of additional content to include within the
#'   document (typically created using the includes function).
#'
#' @param md_extensions Markdown extensions to be added or removed from the
#'   default definition or R Markdown. See the
#'   \code{\link[rmarkdown]{rmarkdown_format}} for additional details.
#'
#' @param pandoc_args Additional command line options to pass to pandoc
#'
#' @inheritParams pdf_document
#'
#' @export
cvR_format_pdf <- function(fig_width = 4,
                           fig_height = 2.5,
                           fig_crop = TRUE,
                           dev = 'pdf',
                           highlight = "default",
                           keep_tex = FALSE,
                           number_sections = FALSE,
                           includes = NULL,
                           md_extensions = NULL,
                           pandoc_args = NULL) {
  # resolve default highlight
  if (identical(highlight, 'default'))
    highlight <- 'pygments'

  # get the latex template
  template <- system.file(
    'rmarkdown/templates/cv/resources/cvR.tex',
    package = 'cvR'
  )

  # call the base pdf_document format with the appropriate options
  format <- rmarkdown::pdf_document(fig_width = fig_width,
                                    fig_height = fig_height,
                                    fig_crop = fig_crop,
                                    dev = dev,
                                    highlight = highlight,
                                    template = template,
                                    keep_tex = keep_tex,
                                    number_sections = number_sections,
                                    # citation_package = citation_package,
                                    latex_engine = "pdflatex",
                                    includes = includes,
                                    md_extensions = md_extensions,
                                    pandoc_args = pandoc_args)

  # return the format
  format
}

