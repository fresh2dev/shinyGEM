

coalesce <- function(input, ifnull) {
  if (is.null(input)) {
    return(ifnull)
  } else {
    return(input)
  }
}

stringsAsFactors <- function (dt) {
  na_cols <- dt[, sapply(.SD, anyNA)]
  for (c in colnames(dt)[na_cols]) {
    if (is.POSIXct(dt[[c]])) {
      dt[is.na(get(c)), (c) := as.POSIXct('0001-01-01 00:00:00', tz = 'UTC')]
    } else if (is.Date(dt[[c]])) {
      dt[is.na(get(c)), (c) := as.Date('0001-01-01')]
    } else {
      if (!is.character(dt[[c]])) {
        dt[, (c) := as.character(get(c))]
      }
      dt[is.na(get(c)), (c) := 'NULL']
    }
  }

  dt <- dt[, lapply(.SD, function(x) {
    if (is.logical(x)) {
      factor(x)
    } else {
      new_x <- if (!is.factor(x)) {
        x
      } else {
        as.character(x)
      }

      if (!is.character(new_x)) {
        new_x
      } else if (all(grepl(new_x, pattern = '^\\d+$'))) {
        as.numeric(new_x)
      } else if (all(grepl(new_x, pattern = '^\\d+.*%.*$'))) {
        as.numeric(sub(
          new_x,
          pattern = '%',
          replacement = '',
          fixed = T
        )) / 100
      } else {
        factor(gsub(
          new_x,
          pattern = "'",
          replacement = "`",
          fixed = T
        ))
      }
    }
  })]

  return(dt)
}


# https://rdrr.io/cran/kimisc/src/R/cut.R
cut_format <-
  function(x,
           breaks,
           include.lowest = FALSE,
           right = TRUE,
           ordered_result = FALSE,
           ...,
           format_fun = format,
           sep = ", ",
           paren = c(')', '[', '(', ']')) {
    if (length(breaks) < 2L) {
      stop("Please specify breaks as a numeric vector of length >= 2",
           call. = FALSE)
    }

    if (right) {
      ob <- c(include.lowest, rep(FALSE, length(breaks) - 2L))
      cb <- rep(TRUE, length(breaks) - 1L)
    } else {
      ob <- rep(TRUE, length(breaks) - 1L)
      cb <- c(rep(FALSE, length(breaks) - 2L), include.lowest)
    }

    ob <- ifelse(ob, paren[[2L]], paren[[1L]])
    cb <- ifelse(cb, paren[[4L]], paren[[3L]])

    formatted_breaks <- format_fun(breaks)
    labels <-
      paste0(ob,
             head(formatted_breaks,-1L),
             sep,
             tail(formatted_breaks,-1L),
             cb)
    cut.default(
      x = x,
      breaks = breaks,
      labels = labels,
      include.lowest = include.lowest,
      right = right,
      ordered_result = ordered_result,
      ...
    )
  }

# https://stackoverflow.com/a/8197703
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
