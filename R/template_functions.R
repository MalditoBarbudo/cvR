# Useful functions:
#
# bind_cv_sections: function to join diferent rmd documents in one rmd document
#
# cvR_format_pdf: format function to create the output.

bind_cv_sections <- function(rmdlist, name = 'cv.Rmd') {
  # sacamos el primer documento, la portada
  front <- readLines(rmdlist[[1]])
  # creamos el archivo con la portada
  write(front, sep = '/n', file = name)
  # quitamos la portada de la lista
  sections <- rmdlist[-1]
  # Para cada uno de los archivos en la lista (secciones)
  # saco el texto, localizo el yaml, lo elimino y escribo
  # la sección en el archivo anterior
  tmp <- lapply(sections, function(x){
    section <- readLines(x)
    yaml_loc <- grep("---", section)
    section <- section[-c(yaml_loc[1]:yaml_loc[2])]
    write(section, sep = '/n', file = name, append = TRUE)
  })
  print(paste(name, ' file created in working directory'))
}

#' cvR format (PDF)
#'
#' Template for creating a CV in R and Rmarkdown
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
                           citation_package = c("natbib", "biblatex"),
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
                                    # citation_package = citation_package,
                                    latex_engine = "pdflatex",
                                    includes = includes,
                                    md_extensions = md_extensions,
                                    pandoc_args = pandoc_args)

  # return the format
  format
}

