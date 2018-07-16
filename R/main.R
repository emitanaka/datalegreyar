insert_at <- function(a, pos, symbols){
  code <- sapply(symbols, function(x) switch(x, min="[--]", max="[++]", down="[-]", up="[+]"))
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
  result[c(FALSE,TRUE)] <- 4
  out <- unlist(result)
  names(out)[out==4 & !is.na(out)] <- code
  out
}

str_style <- function(x){
  out <- NULL
  if(!is.null(x$options$size)){
    if(class(x$options$size) %in% c("numeric", "integer")) {
      x$options$size <- paste0(x$options$size, "pt")
    }
    out <- paste0(out, " font-size:", x$options$size, ";")
  }
  if(!is.null(x$options$colour)){
    out <- paste0(out, " color:", x$options$colour, ";")
  }
  if(!is.null(x$options$bgcolour)){
    out <- paste0(out, " background:", x$options$bgcolour, ";")
  }
  if(!is.null(x$options$sother)){
    out <- paste(out, paste(paste0(x$options$sother, ";"), collapse=" "))
  }
  out
}

#' Insert a datalegreya font easily into html outputs using R.
#'
#' @param object A named integer vector with elements of 0, 1, 2, 3 and names corresponding to the characters of the text.
#' @param symbol A named integer vector where the name should signify which symbol (min, max, down or up) and the integer specifying above which character it should be placed.
#' @param ylabs A string vector of length 2 comprising of string of max length 5 for the bottom and top label of the y-axis. This is NOT working for some reason.
#' @param xlabs A string vector of length 2 comprising of string (of max length 5) to be placed on top of the main text. The first string is placed on the start of the main text and the second at the end. Leave as NA to have one or both to display no labels.
#' @param start_value An integer between 0 and 3 (inclusive) to specify the starting value of the graph.
#' @param size The size of the font.
#' @param type The type of datalegreya font, either thin, gradient or dot.
#' @param sother Character vector of other parameters directly added to the style classes.
#' @param color,colour Colour to be given to the icon
#' @param bgcolor,bgcolour Colour to be given to the background
#'
#' @references [Datalegreya](https://github.com/figs-lab/datalegreya)
#'
#' @export
fig <- function(object, symbol=NULL, ylabs=rep(NA, 2), xlabs=rep(NA, 2),
                start_value=0, size=NULL, type="dot", color=NULL, colour=color,
                bgcolor=NULL, bgcolour=bgcolor,
                sother=NULL) {
  start_text <- paste0("§", start_value)
  start_label <- ""
  if(!is.na(xlabs[1])) {
    xlab1 <- as.character(xlabs[1])
    nx1 <- nchar(xlab1)
    if(nx1 > 5) stop("The first xlabs is longer than 5 characters. Datalegreya does not support more than 5 characters.")
    if(nx1 < 5) xlab1 <- paste0(xlab1, paste0(rep(" ", 5 - nx1), collapse=""))
    start_label <- paste0("{", xlab1, "}")
  }
  end_label <- ""
  if(!is.na(xlabs[2])) {
    xlab2 <- as.character(xlabs[2])
    nx2 <- nchar(xlab2)
    if(nx2 > 5) stop("The second xlabs is longer than 5 characters. Datalegreya does not support more than 5 characters.")
    if(nx2 < 5) xlab2 <- paste0(xlab2, paste0(rep(" ", 5 - nx2), collapse=""))
    end_label <- paste0("{", xlab2, "}")
  }
  ylabel <- ""
  if(any(!is.na(ylabs))) {
    warning("The ylabs is not working... work on web version so not sure why... investigating.")
    ylab1 <- as.character(ylabs[1])
    ylab2 <- as.character(ylabs[2])
    ny1 <- nchar(ylab1)
    ny2 <- nchar(ylab2)
    if(ny1 > 5) stop("The first ylabs is longer than 5 characters. Datalegreya does not support more than 5 characters.")
    if(ny2 > 5) stop("The second ylabs is longer than 5 characters. Datalegreya does not support more than 5 characters.")
    if(ny1 < 5) ylab1 <- paste0(ylab1, paste0(rep(" ", 5 - ny1), collapse=""))
    if(ny2 < 5) ylab2 <- paste0(ylab2, paste0(rep(" ", 5 - ny2), collapse=""))
    ylabel <- paste0("[", ylab2, "[", ylab1, "]")
  }

  dpcy <- htmltools::htmlDependency("datalegreya", "1.0", src=system.file("datalegreya", package="datalegreyar"),
                                  stylesheet="datalegreya.css")

  header <- htmltools::tags$head(dpcy)

  if(!is.null(symbol)) object <- insert_at(object, symbol, names(symbol))
  main_text <- paste0(tolower(names(object)), "|", unname(object), collapse="")
  main_text <- gsub("|4", "", main_text, fixed=T)
  main_text <- gsub("|NA", "\\", main_text, fixed=T)

  out_text <- paste0(start_label, start_text, main_text, end_label, ylabel)

  x <- structure(list(options=list(size=size, sother=sother, colour=colour, bgcolour=bgcolour)),
                 class=c("fig"))

  texttag <- htmltools::tags$span(out_text, class=paste0("figs", type),
                                  style=str_style(x))

  out <- htmltools::tagList(header, texttag)
  class(out) <- c("datalegreyar", class(out))
  out
}



#' This function 'datafies' a string and data suitable in a format as input for the fig function.
#'
#' @param values A vector of y-values which scales to the length of the text and maps the value to the closest value from 0 to 3.
#' @param text A character string which can't be left empty.
#' @param ignore_space Don't have any data value for empty spaces in text.
#'
#' @export
datafy <- function(values, text=NULL, ignore_space=TRUE, ...) {
  if(is.null(text)) {
    range <- max(values) - min(values)
    out <- round((values - min(values)) / range * 3)
    names(out) <- rep(" ", length(out))
    class(out) <- c("datafied", class(out))
    return(out)
  }
  excluded <- c("+", "-", "=", "×", "÷", "", "", "%", "!", "?",
                "/", "(", ")", ",", ";", ":", "&", "*", "_")
  if(any(strsplit(text, split="")[[1]] %in% excluded)) {
    stop(paste("Some character(s) in the text is not supported. The characters not supported are:", paste(excluded, collapse=", ")) )
  }
  space_exists <- FALSE
  if(ignore_space) {
    mod_text <- tolower(gsub(" ", "", text, fixed=T))
    if(grepl(" ", text)) {
      space_exists <- TRUE
      space_pos <- stringr::str_locate_all(text, " ")[[1]][, "start"]
    }
  } else {
    mod_text <- tolower(text)
  }
  l <- nchar(mod_text)
  m <- length(values)
  if(l==m) {
    range <- max(values) - min(values)
    out <- round((values - min(values)) / range * 3)
  }
  if(l < m) {
    # more data than characters
    x <- sort(rep(1:l, times=ceiling(m/l))[1:m])
    suppressWarnings(p <- predict(loess(values ~ x, ...), data.frame(x=1:l)))
    range <- max(p) - min(p)
    out <- round((p - min(p)) / range * 3)
  }
  if(l > m) {
    # less data than characters
    x <- seq(1, l, length=m)
    stop("The number of numerical data is less than number of characters. This is not yet supported.")
  }
  names(out) <- strsplit(mod_text, "")[[1]]
  if(ignore_space & space_exists) {
    # this can be done better I'm sure but it works
    # tried it another way but lots of consecutive spaces broke it and this was a patch solution that works
    outNA <- rep(NA, nchar(text))
    names(outNA) <- paste0(strsplit(text, "")[[1]], 1:nchar(text))
    out2 <- setNames(out, names(outNA)[-space_pos])
    outNA[names(out2)] <- out2
    out <- setNames(outNA, strsplit(text, "")[[1]])
  }
  class(out) <- c("datafied", class(out))
  return(out)
}
