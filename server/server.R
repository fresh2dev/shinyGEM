
source('server/helpers.R', local=TRUE)

function(input, output, session) {

  my <- reactiveValues(dataset_original=NULL, dataset=NULL, dataset_grouped=NULL, theme=NULL)

  c <- reactiveValues(date=NULL, group=NULL, feature=NULL, value=NULL)

  plot_title_dbnce <- reactive({ input$title }) %>% debounce(millis=3000)
  plot_subtitle_dbnce <- reactive({ input$subtitle }) %>% debounce(millis=3000)
  plot_caption_dbnce <- reactive({ input$caption }) %>% debounce(millis=3000)
  plot_xlab_dbnce <- reactive({ input$xlab }) %>% debounce(millis=3000)
  plot_ylab_dbnce <- reactive({ input$ylab }) %>% debounce(millis=3000)
  plot_height_dbnce <- reactive({ input$plotHeight }) %>% debounce(millis=3000)
  plot_width_dbnce <- reactive({ input$plotWidth }) %>% debounce(millis=3000)
  decimal_precision_dbnce <- reactive({ input$decimalPrecision }) %>% debounce(millis=3000)
  n_x_breaks_dbnce <- reactive({ input$n_x_breaks }) %>% debounce(millis=3000)
  n_y_breaks_dbnce <- reactive({ input$n_y_breaks }) %>% debounce(millis=3000)
  hist_bins_dbnce <- reactive({ input$bins }) %>% debounce(millis=3000)

  observe({

    req(input$presetDataset)

    # print('Entered origin block')

    dataset <- NULL
    if (input$presetDataset == 'Game Ratings') {
      dataset <- readRDS('./data/rds/ign.RDS')
    } else if (input$presetDataset == 'Border Patrol Apprehensions') {
      dataset <- readRDS('./data/rds/border.RDS')
    } else if (input$presetDataset == 'College Scorecards') {
      dataset <- readRDS('./data/rds/college.RDS') %>% stringsAsFactors()
    } else if (input$presetDataset == 'Starbucks Nutrition') {
      dataset <- readRDS('./data/rds/starbucks.RDS')
    }

    my$dataset_original <- dataset
    # my$dataset <- dataset

    updateTextInput(session, 'title', value=input$presetDataset)
  })

  observe(priority = 10, {
    req(my$dataset_original)

    # also fire when 'startOver', 'original_as_source' is clicked.
    input$startOver
    input$original_as_source

    dataset <- copy(my$dataset_original)

    isolate({

      dataset[, '#id' := 1:.N]
      n_col <- ncol(dataset)
      setcolorder(dataset, neworder=c(n_col, 1:(n_col-1)))
      setkeyv(dataset, cols = '#id')

      for (col in c('date', 'group', 'feature', 'value')) {
        if (!is.null(c[[col]]) && !(c[[col]] %in% colnames(dataset))) {
          c[[col]] <- NULL
        }
      }

      my$dataset <- dataset
    })
  })

  observeEvent(input$userFile, {

    file_ext <- toupper(regmatches(input$userFile$name, regexpr(input$userFile$name, pattern='\\.(.+)$')))

    ext_i <- match(file_ext, c('.CSV', '.XLS', '.XLSX', '.RDS'))

    if (!is.na(ext_i)) {
      if (ext_i > 3) {
        my$dataset_original <- readRDS(input$userFile$datapath) %>% as.data.table() # %>% stringsAsFactors()
      } else {
        read_FUN <- if (ext_i == 1) { read_csv } else { read_excel }

        my$dataset_original <- suppressWarnings(read_FUN(input$userFile$datapath, col_names = TRUE,
                                        trim_ws = FALSE, guess_max = 1000, na=c('', '-', 'NA', 'NULL'))) %>%
                                as.data.table() %>% stringsAsFactors()

      }
    }

    updateTextInput(session, 'title',
                    value=tools::toTitleCase(sub(tolower(input$userFile$name), pattern = tolower(file_ext), replacement='', fixed=T)))
  })

  observeEvent(input$meltData, {

    # print('Entered melt block')

    if (length(input$userFileIdCols) > 0) {

      var_name <- 'feature'
      if (nchar(input$meltedKeyName) > 0) { var_name <- input$meltedKeyName }

      value_name <- 'value'
      if (nchar(input$meltedValueName) > 0) { value_name <- input$meltedValueName }

      dataset <- my$dataset

      id_vars <- input$userFileIdCols

      num_cols_TF <- sapply(dataset, is.numeric)
      num_cols <- colnames(dataset)[num_cols_TF] %>% .[!(. %in% id_vars)]
      char_cols <- colnames(dataset) %>% .[!(. %in% id_vars) & !(. %in% num_cols)]

      count_char_cols <- FALSE

      if (length(char_cols) > 0) { count_char_cols <- TRUE }

      if (count_char_cols && length(num_cols) > 0 &&
          nrow(dataset[, id_vars, with=F] %>% unique()) < nrow(dataset)) {
        output$sourceDatasetMessage <- renderText({
          'Non-distinct ID columns specified; summed numeric values.'
        })
      } else {
        output$sourceDatasetMessage <- renderText({''})
      }

      if (count_char_cols) {
        dataset <- dataset[, c(list('# group obs.' = .N), lapply(.SD, function(x) {
                      if (is.numeric(x)) { sum(x) } else { uniqueN(x) }
                    })), by=id_vars]

        setnames(dataset, old=char_cols, new=paste('#', char_cols))
        if (length(num_cols) > 0) {
          setnames(dataset, old=num_cols, new=paste('Total', num_cols))
        }

        my$dataset <- dataset
      } else {
        updateSelectizeInput(session, 'userFileIdCols', selected=NA)
        updateTextInput(session, inputId = 'meltedKeyName', value = NA)
        updateTextInput(session, inputId = 'meltedValueName', value = NA)

        dataset <- melt(dataset,
                         id.vars = c('#id', id_vars),
                         variable.name = var_name,
                         value.name = value_name,
                         verbose = FALSE)

        dataset[, '#id' := 1:.N]

        for (col in c('date', 'group', 'feature', 'value')) {
          if (!is.null(c[[col]]) && !(c[[col]] %in% colnames(dataset))) {
            c[[col]] <- NULL
          }
        }

        my$dataset <- dataset
      }
    }
  })

  observeEvent(input$combineCols, {

    req(input$columnsToCombine)

    # print('Entered combine block')

    dataset <- copy(my$dataset)

    to_combine <- input$columnsToCombine
    col_name <- input$combinedColName

    if (nchar(col_name) > 0) {
      new_col_names <- strsplit(col_name, split=',\\s*')[[1]]

      if (length(new_col_names) == length(to_combine)) {
        ren_cols <- duplicated(new_col_names)
        new_col_names[ren_cols] <- paste0(new_col_names[ren_cols], '.1')
        ren_cols <- new_col_names %in% colnames(dataset)
        new_col_names[ren_cols] <- paste0(new_col_names[ren_cols], '.1')
        setnames(dataset, old=input$columnsToCombine, new=new_col_names)
      } else {
        if (col_name %in% colnames(dataset)) { col_name <- paste0(col_name, '.1') }

        sep_char <- ''
        if (nchar(input$sepChar) > 0) {
          sep_char <- input$sepChar
        }

        dataset[[col_name]] <- dataset[, do.call(paste, c(.SD, sep=sep_char)), .SDcols=c(to_combine)]
      }

      updateSelectizeInput(session, 'columnsToCombine', selected=NA)
      updateTextInput(session, 'combinedColName', value = NA)
      updateTextInput(session, 'sepChar', value = NA)
    }

    if (!input$keepCombinedCols) {
      dataset[, c(input$columnsToCombine) := NULL]
    }

    for (col in c('date', 'group', 'feature', 'value')) {
      if (!is.null(c[[col]]) && !(c[[col]] %in% colnames(dataset))) {
        c[[col]] <- NULL
      }
    }

    my$dataset <- dataset %>% stringsAsFactors()
  })

  observeEvent(input$separateCols, {
    req(input$columnToSplit)

    dataset <- my$dataset

    split_regex <- input$splitChar
    if (nchar(split_regex) == 0) {
      split_regex <- '[[:punct:]]+|[[:space:]]+'
    }

    split_cells <- strsplit(as.character(dataset[[input$columnToSplit]]), split=split_regex)

    if (length(split_cells) > 0) {
      n_cols <- max(sapply(split_cells, length))
      col_names <- NULL # paste0('V', 1:n_cols)
      for (a in LETTERS) {
        col_names <- paste0(a, 1:n_cols)
        if (!any(col_names %in% colnames(dataset))) {
          break
        }
      }
      dataset <- copy(my$dataset)
      dataset[, c(col_names) := transpose(split_cells)]
      if (!input$keepSplitCols) { dataset[, c(input$columnToSplit) := NULL] }

      for (col in c('date', 'group', 'feature', 'value')) {
        if (!is.null(c[[col]]) && !(c[[col]] %in% colnames(dataset))) {
          c[[col]] <- NULL
        }
      }

      my$dataset <- dataset %>% stringsAsFactors()

      updateSelectizeInput(session, 'columnToSplit', selected=NA)
      updateTextInput(session, 'splitChar', value = NA)
    }
  })

  cols <- reactiveValues(date=NULL, group=NULL, feature=NULL, value=NULL)

  observe(priority=100, { c$date <- input$date })
  observe(priority=100, { c$group <- input$group })
  observe(priority=100, { c$feature <- input$feature })
  observe(priority=100, { c$value <- input$value })

  observe(priority=50, {
    if (coalesce(input$date, '') != coalesce(c$date, '')) {
      updateSelectizeInput(session, 'date', selected = coalesce(c$date, NA))
    }
  })
  observe(priority=50, {
    if (coalesce(input$group, '') != coalesce(c$group, '')) {
      updateSelectizeInput(session, 'group', selected = coalesce(c$group, NA))
    }
  })
  observe(priority=50, {
    if (coalesce(input$feature, '') != coalesce(c$feature, '')) {
      updateSelectizeInput(session, 'feature', selected = coalesce(c$feature, NA))
    }
  })
  observe(priority=50, {
    if (coalesce(input$value, '') != coalesce(c$value, '')) {
      updateSelectizeInput(session, 'value', selected = coalesce(c$value, NA))
    }
  })

  observe({
    req(my$dataset, input$dataset_rows_all)

    dataset <- NULL

    if (length(c(c$date, c$group, c$feature, c$value)) > 0) {
    # if (length(c(c$date, c$group, c$feature, c$value)) > 0) {
      if (between(length(input$dataset_rows_all), 0, nrow(my$dataset), incbounds = F)) {
        dataset <- my$dataset[input$dataset_rows_all, ]
      } else {
        dataset <- my$dataset
      }
    }

    my$dataset_filtered <- dataset
  })

  setComboBox <- function(session, column, col_names, col_pattern) {
    if (is.null(c[[column]]) || !(c[[column]] %in% col_names)) {
      best_guess <- NA

      if (length(col_pattern) > 0) { best_guess <- col_names %>% .[grepl(tolower(.), pattern=col_pattern)] %>% head(1) }

      if (length(best_guess) > 0) {
        cols[[column]] <- best_guess
        updateSelectizeInput(session, column, choices = col_names, selected=best_guess)
      }
      else {
        cols[[column]] <- NULL
        updateSelectizeInput(session, column, choices = col_names, selected=NA)
      }
    }
  }

  observe({
    req(is.null(c$date))

    c$date

    updateSelectizeInput(session, 'dateFormat', selected = NA)
    updateSelectizeInput(session, 'dateTransform', selected = NA)
  })

  observe(priority = 2, {
    req(my$dataset)

    # print('Populating column lists')

    col_names <- colnames(my$dataset) %>% .[. != '#id']

    isolate({
      updateSelectizeInput(session, 'userFileIdCols', choices = col_names, selected=input$userFileIdCols)
      updateSelectizeInput(session, 'columnsToCombine', choices = col_names, selected=input$columnsToCombine)
      updateSelectizeInput(session, 'columnToSplit', choices = col_names, selected=input$columnToSplit)

      setComboBox(session, 'date', col_names, col_pattern='year|stamp')

      if (is.null(cols$date)) {
        updateSelectizeInput(session, 'dateFormat', selected = NA)
        updateSelectizeInput(session, 'dateTransform', selected = NA)
      }

      setComboBox(session, 'group', col_names, col_pattern='group')

      setComboBox(session, 'feature', col_names, col_pattern='feature')

      num_col_names <- col_names[my$dataset[, lapply(.SD, class), .SDcols=col_names][1] %in% c('numeric', 'integer', 'double')]

      setComboBox(session, 'value', num_col_names, col_pattern='value')
    })
  })

  observe(priority = 1, {
    req(my$dataset, c$date)

    # print('Guessing date format.')

    dates <- as.character(my$dataset[[c$date]][1:100])

    # http://gamon.webfactional.com/regexnumericrangegenerator/
    patterns <- c('Year'='^\\d{3,4}$',
                  'Month'='^0*([1-9]|1[0-2])$',
                  'Day'='^0*([0-9]|[12][0-9]|3[01])$',
                  'Day of Year'='^0*([1-9]|[1-8][0-9]|9[0-9]|[12][0-9]{2}|3[0-5][0-9]|36[0-6])$',
                  'ymd_hms'='^\\d{4}.*\\d{1,2}.*\\d{1,2}\\s.+$',
                  'ymd'='^\\d{4}.*\\d{1,2}.*\\d{1,2}',
                  'mdy_hms'='^\\d{1,2}.*\\d{1,2}.*\\d{4}\\s.+$',
                  'mdy'='^\\d{1,2}.*\\d{1,2}.*\\d{4}',
                  'Year-Month'='^\\d{4}.*\\d{1,2}$',
                  'Hour:Minute'='^\\d{1,2}:\\d{2}$',
                  'Hour:Minute:Second'='^\\d{,2}:\\d{2}:\\d{2}$')

    format_match <- NULL # ' - Guess - '

    for (f in names(patterns)) {
      if (all(grepl(dates, pattern=patterns[f]))) {
        format_match <- f
        break
      }
    }

    if (!is.null(format_match) && (is.null(isolate(input$dateFormat)) || isolate(input$dateFormat) != format_match)) {
      updateSelectizeInput(session, 'dateFormat', selected = format_match)

      if (is.null(isolate(input$dateTransform)) || isolate(input$dateTransform) != format_match) {
        updateSelectizeInput(session, 'dateTransform', selected = format_match)
      }
    }
  })

  observe({
    req(my$dataset_grouped())

    if (is.null(c$value)) {
      updateRadioButtons(session, 'plotValues', selected = '# observations')
    } else {
      updateRadioButtons(session, 'plotValues', selected = 'values')
    }
  })

  decimalPrecision <- reactive({
    if (is.na(decimal_precision_dbnce())) {
      return(2L)
    }
    return(as.integer(decimal_precision_dbnce()))
  })

  my$dataset_dates_parsed <- reactive({

    req(my$dataset_filtered,
        # any(!is.null(c(cols$date, cols$group, cols$feature, cols$value))),
        length(c(c$date, input$dateFormat)) %in% c(0, 2))

    # print('Entered dates_parsed block')

    dataset <- copy(my$dataset_filtered)

    date_col <- c$date

    if (is.null(date_col) || is.null(input$dateFormat)) {
      date_col <- NULL
    } else {
      if (input$dateFormat == ' - Guess - ') {
        dataset[, c(date_col) := anytime(dataset[[date_col]])]
      } else {
        date_format_str <- NULL
        date_format_str <- sapply(date_format_list, extract, input$dateFormat) %>% .[!is.na(.)]
        dataset[, c(date_col) := parse_date_time(dataset[[date_col]], orders = date_format_str)]
      }

      if (anyNA(dataset[[date_col]])) {
        dataset[, c(date_col) := NULL]
        date_col <- NULL
      }
    }

    # print('Entered date_filtered block')

    if (!is.null(date_col)) {
      if (sum(!is.na(input$dateRange)) == 2) {
        dataset <- dataset[between(get(isolate(date_col)), ymd(input$dateRange[1]), ymd(input$dateRange[2]), incbounds = T), ]
      } else if (!is.na(input$dateRange[1])) {
        dataset <- dataset[get(isolate(date_col)) >= ymd(input$dateRange[1]), ]
      } else if (!is.na(input$dateRange[2])) {
        dataset <- dataset[get(isolate(date_col)) <= ymd(input$dateRange[2]), ]
      }

      # print('Entered days_of_week block')

      if (between(length(input$daysOfWeek), 0, 7, incbounds = F)) {
        dataset <- dataset[weekdays(get(date_col)) %in% input$daysOfWeek,]
      }
    }

    # print('Entered dates_transformed block')

    if (!is.null(date_col) && !is.null(input$dateTransform)) {
      date_trans_str <- sapply(date_trans_list, extract, input$dateTransform) %>% .[!is.na(.)]

      if (grepl(date_trans_str, pattern = '%', fixed = TRUE)) {
        dataset[, c(date_col) := format(dataset[[date_col]], date_trans_str)]
        if (all(grepl(dataset[[date_col]], pattern='^\\d+$'))) {
          dataset[, c(date_col) := factor(as.integer(dataset[[date_col]]), ordered = TRUE)] # for week of year
        }
      } else {
        date_trans_FUN <- match.fun(date_trans_str)
        if (input$dateTransform =='Year-Quarter') {
          dataset[, c(date_col) := factor(date_trans_FUN(dataset[[date_col]], with_year=TRUE), ordered=TRUE)]
        } else if (is.null(as.list(args(date_trans_FUN))[['label']])) {
          dataset[, c(date_col) := factor(date_trans_FUN(dataset[[date_col]]), ordered=TRUE)]
        } else {
          dataset[, c(date_col) := factor(date_trans_FUN(dataset[[date_col]], label = TRUE), ordered=TRUE)]
        }
      }

      if (grepl(names(date_trans_str), pattern='Parts', fixed=T)) {
        setnames(dataset, old=date_col, new=input$dateTransform)
        date_col <- input$dateTransform
      }
    }

    cols$date <- date_col

    return(dataset)
  })

  total_n <- reactiveValues(groups=0, features=0)

  my$dataset_grouped <- reactive({
    req(my$dataset_dates_parsed())

    # print('Entered dataset_grouped block')

    dataset <- copy(my$dataset_dates_parsed())

    group_col <- c$group
    feature_col <- c$feature
    value_col <- c$value

    isolate({
      date_col <- cols$date

      if (!is.null(date_col)) {
        if (!is.null(group_col) && group_col == c$date) { group_col <- date_col }
        if (!is.null(feature_col) && feature_col == c$date) { feature_col <- date_col }
      }
    })

    id_col <- if (any(!is.null(c(date_col, group_col, feature_col, value_col)))) { '#id' } else { NULL}

    # print('Entered agg_within block')

    grouping <- c(date_col, group_col, feature_col)

    if (is.null(value_col)) {
      dataset <- dataset[, .('value'=NA, '# observations'=.N), by = grouping]
      id_col <- NULL
    } else {
      if (!is.numeric(dataset[[value_col]])) {
        dataset[, c(value_col) := as.numeric(gsub(get(value_col), pattern = ',', replacement = '', fixed=T))]
      }

      if (is.null(input$aggWithinFUN)) {
        grouping <- c(id_col, grouping, value_col)

        dataset <- dataset[, grouping, with=FALSE]
        setnames(dataset, old=value_col, new='value')

        dataset[, '# observations' := 1]
      } else {

        aggWithinFUN <- match.fun(input$aggWithinFUN)

        dataset <- dataset[, .('value'=aggWithinFUN(get(value_col), na.rm = T),
                               '# observations'=.N), by = grouping]

        id_col <- NULL
      }
    }

    if (!is.null(value_col) && sum(dataset[['# observations']]) > nrow(dataset)) {
      value_col <- paste(input$aggWithinFUN, value_col, sep = ' ')
    }

    # print('Entered agg_between block')

    if (!is.null(value_col)) {
      setnames(dataset, 'value', value_col)
    } else {
      dataset[, value := NULL]
    }

    # print('Entered rounded_values_filtered block')

    if (!is.null(value_col)) {
      dataset[, c(value_col) := round(dataset[[value_col]], decimalPrecision())]
    }

    if (!is.null(value_col)) {
      value_col <- value_col
    } else if ('# observations' %in% colnames(dataset)) {
      value_col <- '# observations'
    }

    if (!is.null(group_col)) {
      if (!is.factor(dataset[[group_col]])) { # must be int, dbl, or num.
        dataset[, c(group_col) := factor(dataset[[group_col]], ordered = T)]
      } else {
        group_totals <- dataset[, sum(abs(get(value_col)), na.rm=T), by=c(group_col)]
        setorder(group_totals, -V1)
        dataset[, c(group_col) := factor(dataset[[group_col]], levels = group_totals[[group_col]])]
      }
    }

    if (!is.null(feature_col)) {
      if (!is.factor(dataset[[feature_col]])) { # must be int, dbl, or num.
        dataset[, c(feature_col) := factor(dataset[[feature_col]], ordered = T)]
      } else {
        feature_totals <- dataset[, sum(abs(get(value_col)), na.rm=T), by=c(feature_col)]
        setorder(feature_totals, -V1)
        dataset[, c(feature_col) := factor(dataset[[feature_col]], levels = feature_totals[[feature_col]])]
      }
    }

    # print('Entered topN block')

    if (is.null(group_col)) {
      total_n$groups <- 0L
    } else {
      n_groups <- length(levels(dataset[[group_col]]))
      total_n$groups <- n_groups
      max_groups <- 50L
      if (!between(n_groups, 0L, max_groups, incbounds = T)) {
        dataset <- dataset[get(group_col) %in% (levels(dataset[[group_col]])[1:max_groups]),]
      }
    }

    if (is.null(feature_col)) {
      total_n$features <- 0L
    } else {
      n_features <- length(levels(dataset[[feature_col]]))
      total_n$features <- n_features
      max_features <- 100L
      if (!between(n_features, 0L, max_features, incbounds = T)) {
        dataset <- dataset[get(feature_col) %in% (levels(dataset[[feature_col]])[1:max_features]),]
      }
    }

    setorderv(dataset, cols=c(id_col, date_col, group_col, feature_col), order = 1)

    cols$id <- id_col
    cols$date <- date_col
    cols$group <- group_col
    cols$feature <- feature_col
    cols$value <- value_col

    return(dataset)
  })

  output$dataset <- DT::renderDataTable(server=TRUE, options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15), select=FALSE),
                                        filter='top', { my$dataset })

  output$dataset_grouped <- DT::renderDataTable(server=TRUE, # options = list(pageLength = 18, lengthChange=FALSE, select=FALSE),
                                options = list(pageLength = 5, lengthMenu = c(3, 5, 10, 15), select=FALSE),
                                filter='top', {
                                  my$dataset_grouped()
                                })

  coalesce <- function(text, ifnull) {
    if (!is.null(text)) {
      return(text)
    }

    return(ifnull)
  }

  observeEvent(c(input$tabContainer, my$dataset_p), priority = 1, {
    # clear selections when plot tab changes.
    session$sendCustomMessage(type = 'counts_plot_set', message = character(0))
    session$sendCustomMessage(type = 'distribution_plot_set', message = character(0))
    session$sendCustomMessage(type = 'trends_plot_set', message = character(0))
    session$sendCustomMessage(type = 'pairs_plot_set', message = character(0))
  })

  my$dataset_selected <- reactive({

    if (all(is.null(c(input$count_plot_selected,
                      input$trends_plot_selected,
                      input$distribution_plot_selected,
                      input$pairs_plot_selected)))) {
      return(NULL)
    } else {

      selected_list <- NULL
      col_names <- NULL
      dataset <- NULL

      # id_col <- coalesce(cols$id, 'V1')
      date_col <- coalesce(cols$date, 'V1')
      group_col <- coalesce(pcols$group, 'V2')
      feature_col <- coalesce(pcols$feature, 'V3')

      if (!is.null(input$pairs_plot_selected)) {
        selected_list <- input$pairs_plot_selected

        dataset <- my$dataset_wide()

        col_names <- colnames(dataset) %>% .[!(. %in% c(input$featureX, input$featureY))]
      } else {

        col_names <- c(date_col, group_col, feature_col)

        if (!is.null(input$count_plot_selected)) {
          selected_list <- input$count_plot_selected
        } else if (!is.null(input$trends_plot_selected)) {
          selected_list <- input$trends_plot_selected
        } else if (!is.null(input$distribution_plot_selected)) {
          selected_list <- input$distribution_plot_selected
        }

        dataset <- my$dataset_dates_parsed()
      }

      dt_selected <- as.data.table(tstrsplit(selected_list, '|', fixed=T))

      if (ncol(dt_selected) == 1) {
        setnames(dt_selected, '#id')
        join_cols <- '#id'
      } else {
        setnames(dt_selected, col_names)

        join_cols <- dt_selected[, !sapply(.SD, function(x) { all(x=='') || anyNA(x) })]

        dt_selected <- dt_selected[, (join_cols), with=F]

        join_cols <- colnames(dt_selected) %>% .[. %in% colnames(dataset)]

        dt_selected <- dt_selected[, (join_cols), with=F]
      }

      if (length(join_cols) == 0) {
        return(dataset)
      }
      else {
        isolate ({
          # ensure selected date is same format as dataset date.
          if (!is.null(cols$date) && cols$date %in% join_cols && is.character(dataset[[cols$date]])) {
            date_trans_str <- sapply(date_trans_list, extract, input$dateTransform) %>% .[!is.na(.)]
            dt_selected[, c(cols$date) := format(as_datetime(get(cols$date)), date_trans_str)]
            # dataset <- copy(dataset)
            # dataset[, c(date_col) := as.character(parse_date_time(get(date_col), orders = date_trans_str))]
          }
        })

        col_types <- dataset[, lapply(.SD, class), .SDcols=join_cols][1]

        lapply(1:length(join_cols), function(i) {
          this_col <- join_cols[[i]]
          is_fun <- match.fun(paste0('is.', col_types[[i]]))
          if (!is_fun(dt_selected[[this_col]])) {
            as_fun <- match.fun(paste0('as.', col_types[[i]]))
            dt_selected[, c(this_col) := as_fun(get(this_col))]
          }
          invisible(NULL)
        })

        return(merge(dataset, dt_selected, by=join_cols))
      }
    }
  })

  output$dataset_selected <- DT::renderDataTable(server=TRUE, filter='top',
                                        options = list(pageLength = 15, lengthMenu = c(3, 5, 10, 15), select=FALSE),
                                        { my$dataset_selected() })

  observeEvent(input$selected_as_source, {
    req(my$dataset_selected())

    selected_ids <- my$dataset_selected()[['#id']]

    isolate({
      my$dataset <- my$dataset[my$dataset[['#id']] %in% selected_ids]
    })
  })

  n <- reactiveValues(dates=0, groups=0, features=0, x_breaks=30L, y_breaks=10L)

  # columns specific to plots.
  pcols <- reactiveValues(group=NULL, feature=NULL, value=NULL)

  observe(priority = 1, {
    req(my$dataset_grouped())

    # print('Getting plot dataset.')

    dataset <- NULL

    if (between(length(input$dataset_grouped_rows_all), 0, nrow(my$dataset_grouped()), incbounds = F)) {
      dataset <- my$dataset_grouped()[input$dataset_grouped_rows_all, ]
    } else {
      dataset <- my$dataset_grouped()
    }

    # drop levels.
    my$dataset_p <- dataset[, lapply(.SD, function(x) { if (is.factor(x)) { factor(x) } else { x } })]
  })


  observe({
    req(my$dataset_p)

    dataset <- my$dataset_p

    group_col <- isolate(cols$group)
    feature_col <- isolate(cols$feature)
    value_col <- cols$value
    if (is.null(value_col) || input$plotValues == '# observations') {
      value_col <- '# observations'
    }

    swap <- params$swap

    isolate({
      if (swap) {
        if (coalesce(pcols$group, '') != coalesce(feature_col, '')) {
          pcols$group <- feature_col
        }
        if (coalesce(pcols$feature, '') != coalesce(group_col, '')) {
          pcols$feature <- group_col
        }
      } else {
        if (coalesce(pcols$group, '') != coalesce(group_col, '')) {
          pcols$group <- group_col
        }
        if (coalesce(pcols$feature, '') != coalesce(feature_col, '')) {
          pcols$feature <- feature_col
        }
      }

      pcols$value <- value_col

      n$dates <- if (is.null(cols$date)) { 0L } else { length(unique(dataset[[cols$date]])) }
      n$groups <- if (is.null(pcols$group)) { 0L } else { length(levels(dataset[[pcols$group]])) }
      n$features <- if (is.null(pcols$feature)) { 0L } else { length(levels(dataset[[pcols$feature]])) }
    })
  })


  params <- reactiveValues(swap=FALSE, facetFeatures=FALSE, rotate=FALSE, fixLimsX=FALSE, fixLimsY=FALSE)

  observe({
    for (param in names(isolate(reactiveValuesToList(params)))) {
      params[[param]] <- param %in% input$plotParams
    }
  })

  observeEvent(params$rotate, {
    fix_y <- params$fixLimsX
    fix_x <- params$fixLimsY
    if (fix_x != fix_y) {
      selected <- input$plotParams

      if (!fix_x) { selected <- selected[selected != 'fixLimsX'] }
      else { selected <- c(selected, 'fixLimsX')  }

      if (!fix_y) { selected <- selected[selected != 'fixLimsY'] }
      else { selected <- c(selected, 'fixLimsY')  }

      updateCheckboxGroupInput(session, 'plotParams', selected = selected)
    }
  })

  toName <- function(text, envir = parent.frame()) {
    if (is.null(text)) {
      return('')
    }
    return(eval(sym(text), envir = envir))
  }

  plot <- reactiveValues(counts=NULL, trends=NULL, dist=NULL)
  plot_final <- reactiveValues(counts=NULL, trends=NULL, dist=NULL)

  output$groupCountText <- renderText({
    return(paste0(n$groups, ' of ', total_n$groups, ' groups shown.'))
  })

  output$featureCountText <- renderText({
    return(paste0(n$features, ' of ', total_n$features, ' features shown.'))
  })

  n$x_breaks <- reactive({
    n <- 30L
    if (!is.na(n_x_breaks_dbnce()) && as.integer(n_x_breaks_dbnce()) > 0) {
      n <- as.integer(n_x_breaks_dbnce())
    }
    return(n)
  })

  n$y_breaks <- reactive({
    n <- 10L
    if (!is.na(n_y_breaks_dbnce()) && as.integer(n_y_breaks_dbnce()) > 0) {
      n <- as.integer(n_y_breaks_dbnce())
    }
    return(n)
  })

  output$countPlotMessage <- renderText({
    req(my$dataset_p)

    if (n$groups > 50) {
      return('Max of 50 groups exceeded.')
    } else if (n$features > 100) {
      return('Max of 100 features exceeeded.')
    } else if (n$dates > 1) {
      return('Summed out date column.')
    }
  })


  plot$counts <- reactive({
    req(my$dataset_p)

    # print('Entered counts_plot block')

    dataset <- copy(my$dataset_p)

    group_col <- pcols$group
    feature_col <- pcols$feature
    value_col <- pcols$value

    geom <- input$countGeom
    values_as <- input$countValuesAs
    rotate <- params$rotate

    isolate({
      id_col <- cols$id
      date_col <- cols$date

      if (nrow(dataset) > 1 && !is.null(value_col) && all(is.null(c(date_col, group_col, feature_col)))) {
        # do this to prevent several unnecessary layers plotted atop one another
        dataset <- dataset[, .(value=sum(get(value_col))), by=NULL]
        value_col <- paste0('Sum of ', value_col)
        setnames(dataset, old='value', new=value_col)
        id_col <- NULL
      }

      if (n$dates > 1 || (is.null(input$aggWithinFUN) && sum(dataset[['# observations']], na.rm=T) == nrow(dataset))) {
        # contains non-distinct columns; sum them if no agg-within function is specified.
        grouping <- c(group_col, feature_col)
        dataset <- dataset[, .('value'=sum(get(value_col), na.rm = T),
                               '# observations'=sum(get('# observations'))), by = grouping]
        value_col <- paste0('Sum of ', value_col)
        setnames(dataset, old='value', new=value_col)
        y_lab <- value_col
        date_col <- NULL
        id_col <- NULL
      }

      y_format <- scales::comma
      y_lab <- value_col
      x_lab <- NULL

      if (values_as == '% of Grand Total') {
        dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
        y_lab <- paste(y_lab, paste0('(', values_as, ')'))
        y_format <- scales::percent
      } else if (values_as == '% of Group Total' || geom == 'Pie') {
        if (is.null(group_col)) {
          dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
        } else {
          dataset[, c(value_col) := as.numeric(get(value_col))]
          dataset[, c(value_col) := list(get(value_col)/sum(get(value_col))), by=c(group_col)]
        }
        y_lab <- paste(y_lab, '(% of Group Total)')
        y_format <- scales::percent
      }

      no_legend <- FALSE

      if (geom == 'Bars') {
        p <- ggplot(dataset, aes(x=toName(feature_col), y=toName(value_col),
                                 group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col),
                                 tooltip=paste(paste0('y = ',y_format(toName(value_col))),
                                               paste0('n = ',comma(toName('# observations'))),
                                               sep='\n'),
                                 data_id=paste0(paste(toName(date_col), toName(group_col), toName(feature_col), sep='|'), '|'))) +
          geom_bar_interactive(stat='identity', position=position_dodge(width=1), na.rm = T)
          # geom_bar(stat='identity', position=position_dodge(width=1), na.rm = T)

        no_legend <- TRUE

        if (rotate) {
          p <- p + scale_x_discrete(limits = factor(rev(levels(dataset[[feature_col]]))))
        }
      } else {
        x_text <- NULL # feature_col
        if (is.null(x_text)) { x_text <- '' }
        # Bars stacked
        p <- ggplot(dataset, aes(x=x_text, y=toName(value_col),
                                 group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col),
                                 tooltip=paste(paste0('y = ',y_format(toName(value_col))),
                                               paste0('n = ',comma(toName('# observations'))),
                                               sep='\n'),
                                 data_id=paste0(paste(toName(date_col), toName(group_col), toName(feature_col), sep='|'), '|'))) +
          geom_bar_interactive(stat='identity', width=1, position=position_stack(reverse=T), na.rm = T)
                # geom_bar(stat='identity', width=1, position=position_stack(reverse=T), na.rm = T)

        if (geom == 'Pie') {
          p <- p + coord_polar('y', start=0)
        }
      }
    })

    return(facet_features(p, x_lab=x_lab, y_lab=y_lab, x_format=NULL, y_format=y_format, no_legend = no_legend))
  })

  plot_final$counts <- reactive({
    req(my$dataset_p,
        n$groups <= 50, n$features <= 100)

    return(postProcessPlot(plot$counts()))
  })

  observeEvent(input$reactivity, {
    if (input$reactivity==TRUE) {
      output$count_plot <- renderggiraph({ plot_final$counts() %>% asGGiraph() })
    } else {
        output$count_plot <- renderggiraph({
          input$submit # fire on 'submit'
          isolate({ plot_final$counts() %>% asGGiraph() })
        })
    }
  })

  output$trendPlotMessage <- renderText({
    if (is.null(cols$date)) {
      return('Specify a date column.')
    } else if (n$groups > 50) {
      return('Max of 50 groups exceeded.')
    } else if (n$features > 100) {
      return('Max of 100 features exceeeded.')
    } else {
      return('')
    }
  })

  plot$trends <- reactive({

    # print('Entered trends_plot block')

    dataset <- copy(my$dataset_p)

    group_col <- pcols$group
    feature_col <- pcols$feature
    value_col <- pcols$value

    values_as <- input$trendValuesAs

    geom <- input$trendGeom

    isolate({
      id_col <- cols$id
      date_col <- cols$date

      if (nrow(dataset) > 1 && !is.null(value_col) && all(is.null(c(date_col, group_col, feature_col)))) {
        # do this to prevent several unnecessary layers plotted atop one another
        dataset <- dataset[, .(value=round(mean(get(value_col)), isolate(decimalPrecision()))), by=NULL]
        value_col <- paste0('mean ', value_col)
        setnames(dataset, old='value', new=value_col)
        id_col <- NULL
      }

      p <- NULL
      x_lab <- date_col
      y_lab <- value_col
      y_format <- scales::comma
      x_format <- NULL

      if (values_as == '% of Grand Total') {
        dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
        y_lab <- paste(y_lab, paste0('(', values_as, ')'))
        y_format <- scales::percent
      } else if (values_as == '% of Group Total') {
        if (is.null(group_col)) {
          dataset[, c(value_col) := list(get(value_col)/sum(get(value_col)))]
        } else {
          dataset[, c(value_col) := as.numeric(get(value_col))]
          dataset[, c(value_col) := list(get(value_col)/sum(get(value_col))), by=c(group_col)]
        }
        y_lab <- paste(y_lab, paste0('(', values_as, ')'))
        y_format <- scales::percent
      }

      x_format <- NULL

      if (is.factor(dataset[[date_col]]) && all(grepl(dataset[[date_col]], pattern='^\\d+$'))) {
        dataset[, c(date_col) := as.integer(as.character(get(date_col)))]
        x_format <- waiver() # leave x-axis text as-is.
      }

      p <- ggplot(dataset, aes(x=toName(date_col), y=toName(value_col),
                               group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col),
                               tooltip=paste(toName(feature_col),
                                             paste(date_col, toName(date_col), sep=': '),
                                             paste0('y = ', y_format(toName(value_col))),
                                             paste0('n = ', comma(toName('# observations'))),
                                             sep='\n'),
                               data_id=paste0(paste(toName(date_col), toName(group_col), toName(feature_col), sep='|'), '|')))

      non_distinct_rows <- (is.null(input$aggWithinFUN) && sum(dataset[['# observations']], na.rm=T) == nrow(dataset))

      if (non_distinct_rows) {

        if (geom == 'Lines') {
          p <- p <- ggplot(dataset, aes(x=toName(date_col), y=toName(value_col),
                          group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col),
                          tooltip=paste(toName(feature_col),
                                        paste(date_col, toName(date_col), sep=': '),
                                        paste0('y = ', y_format(toName(value_col))),
                                        paste0('n = ', comma(toName('# observations'))),
                                        sep='\n'),
                          data_id=toName(id_col))) +
            geom_smooth(stat='smooth', method = 'loess', se = F, na.rm = T) +
            geom_point_interactive(size=2, alpha=0.5)
        } else {
          id_col <- NULL

          grouping <- c(date_col, group_col, feature_col)
          dataset <- dataset[, .('value'=sum(get(value_col), na.rm = T),
                                 '# observations'=sum(get('# observations'))),
                             by = grouping]

          value_col <- paste0('Sum of ', value_col)
          setnames(dataset, old='value', new=value_col)
          y_lab <- value_col

          p <- ggplot(dataset, aes(x=toName(date_col), y=toName(value_col),
                                   group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col),
                                   tooltip=paste(toName(feature_col),
                                                 paste(date_col, toName(date_col), sep=': '),
                                                 paste0('y = ', y_format(toName(value_col))),
                                                 paste0('n = ', comma(toName('# observations'))),
                                                 sep='\n'),
                                   data_id=paste0(paste(toName(date_col), toName(group_col), toName(feature_col), sep='|'), '|')))
        }
      }

      if (!non_distinct_rows && geom == 'Lines') {
        p <- p + geom_line(size=1, na.rm = T) + geom_point_interactive(size=2, alpha=1, na.rm = T)
      } else if (geom == 'Bars') {
        p <- p + geom_bar_interactive(stat = 'identity', width=1, color='gray', size=0.5, position = position_stack(reverse = TRUE), na.rm = T)
      }

      if (!is.integer(dataset[[date_col]])) {
        n_dateSplits <- min(n$dates, n$x_breaks())
        n_dateLabels <- n_dateSplits # min(n$dates, n_dateLabels, n$x_breaks())

        if (is.factor(dataset[[date_col]])) {

          i_splits <- round(seq(from=1, to=length(levels(dataset[[date_col]])), length.out = n_dateSplits))
          date_limits <- levels(dataset[[date_col]])
          date_labels <- date_limits
          date_labels[!(date_labels %in% date_labels[i_splits])] <- ''

          if (params$rotate) { date_limits <- rev(date_limits) }

          p <- p + scale_x_discrete(name=date_col, limits = date_limits, labels = date_labels)

          date_labels <- date_labels[date_labels != '']

        } else {
          date_trans_str <- NULL

          if (is.character(dataset[[date_col]])) {
            date_trans_str <- sapply(date_trans_list, extract, input$dateTransform) %>% .[!is.na(.)]
            dataset[, c(date_col) := parse_date_time(get(date_col), orders = date_trans_str)]
          }

          date_labels <- pretty_dates(dataset[[date_col]], n=n_dateSplits)

          scale_x <- scale_x_datetime

          if (is.Date(dataset[[date_col]])) {
            scale_x <- scale_x_date
            date_labels <- as_date(date_labels)
          }

          if (params$rotate) {
            date_labels <- rev(date_labels)
          }

          if (is.null(date_trans_str)) {
            p <- p + scale_x(breaks = date_labels, expand=c(0.01, 0))
          } else {
            p <- p + scale_x(breaks = date_labels, date_labels=date_trans_str, expand=c(0.01, 0))
          }
        }
      }
    })

    return(facet_features(p, x_lab=x_lab, y_lab=y_lab, x_format=x_format, y_format=y_format))
  })

  plot_final$trends <- reactive({
    req(my$dataset_p, cols$date, n$groups <= 50, n$features <= 100)

    return(postProcessPlot(plot$trends()))
  })

  observeEvent(input$reactivity, {
    if (input$reactivity==TRUE) {
      output$trends_plot <- renderggiraph({ plot_final$trends() %>% asGGiraph() })
    } else {
      output$trends_plot <- renderggiraph({
        input$submit # fire on 'submit'
        isolate({ plot_final$trends() %>% asGGiraph() })
      })
    }
  })

  output$distPlotMessage <- renderText({
    if (n$groups > 50) {
      return('Max of 50 groups exceeded.')
    } else if (n$features > 100) {
      return('Max of 100 features exceeeded.')
    } else if (!is.null(input$aggWithinFUN)) {
      return('Consider removing within-groups aggregation.')
    } else {
      return('')
    }
  })

  plot$dist <- reactive({

    # print('Entered dist_plot block')

    dataset <- copy(my$dataset_p)

    group_col <- pcols$group
    feature_col <- pcols$feature
    value_col <- pcols$value

    geom <- input$distGeom
    show_rug <- input$showRug

    n_bins <- as.integer(hist_bins_dbnce())
    values_as <- input$histValuesAs

    x_breaks <- n$x_breaks()

    rotate <- params$rotate

    isolate({

      id_col <- cols$id
      date_col <- NULL

      # if (params$showLabels) {
      #   grouping <- c(group_col, feature_col)
      #   dataset[, ':=' (Mean = round(weighted.mean(get(value_col), w=get('# observations')), digits = decimalPrecision()),
      #                   SD = round(sd(get(value_col)), digits=decimalPrecision())),
      #           by=grouping][, c('id', 'AbsErr') := list(1:.N, abs(get(value_col)-Mean))]
      #
      #   min_ids <- dataset[, .SD[which.min(AbsErr)], by=grouping, .SDcols='id']$id
      #
      #   dataset[min_ids,]$Label <- paste0('atop(', paste0("mu=='", comma(dataset[min_ids,]$Mean), "', sigma=='", comma(dataset[min_ids,]$SD)), "')")
      #
      #   dataset[, c('id', 'Mean', 'SD', 'AbsErr') := NULL]
      # }

      p <- NULL
      x_lab <- value_col
      y_lab <- NULL

      y_format <- scales::comma
      x_format <- scales::comma

      if (geom == 'Histogram') {

        if (is.na(n_bins) || n_bins < 2) { n_bins <- 30 }

        grouping <- c(group_col, feature_col)

        val_range <- range(dataset[[value_col]])
        if (length(unique(val_range)) > 1) {
          dataset <- dataset[, .(value_bins = cut_format(get(value_col), format_fun = x_format, sep = ' ~ ',
                                                  breaks=unique(round(seq(from=val_range[1], to=val_range[2], length.out=n_bins), digits=decimalPrecision())), # Inf),
                                                  right=FALSE, include.lowest = T, ordered_result = T, dig.lab=10)), by=grouping]
        } else {
          dataset[, value_bins := factor(comma(val_range[1]))]
        }

        dataset <- dataset[, .(n = .N), by=c(grouping, 'value_bins')]

        id_col <- NULL
        y_lab <- 'Binned Frequency'
        x_format <- NULL

        if (values_as == '% of Grand Total') {
          dataset[, n := n/sum(n)]
          y_lab <- paste(y_lab, paste0('(', values_as, ')'))
          y_format <- scales::percent
        } else if (values_as == '% of Group Total') {
          if (is.null(group_col)) {
            dataset[, n := n/sum(n)]
          } else {
            dataset[, n := as.numeric(n)]
            dataset[, n := n/sum(n), by=c(group_col)]
          }
          y_lab <- paste(y_lab, '(% of Group Total)')
          y_format <- scales::percent
        }

        p <- ggplot(dataset, aes(x=value_bins, y=n,
                                 group=toName(feature_col), fill=toName(feature_col), color=toName(feature_col),
                                 tooltip=paste(toName(feature_col),
                                               paste0('y = ', y_format(n)),
                                               paste0('x = ', value_bins),
                                               sep='\n'),
                                 data_id=paste0(paste(toName(date_col), toName(group_col), toName(feature_col), sep='|'), '|'))) +
              geom_bar_interactive(stat = 'identity', width=1, position = position_stack(reverse=T), color='gray', size=0.5, na.rm = T)

        x_limits <- levels(dataset[['value_bins']])
        i_splits <- round(seq(from=1, to=length(x_limits), length.out = x_breaks))

        x_labels <- x_limits
        x_labels[!(x_labels %in% x_labels[i_splits])] <- ''

        p <- p + scale_x_discrete(limits = x_limits, labels = x_labels)

      } else {

        x_lab <- NULL
        y_lab <- value_col
        x_format <- NULL

        # TODO
        # q1 <- quantile(dataset[[value_col]], probs = 0.25)
        # q3 <- quantile(dataset[[value_col]], probs = 0.75)
        # iqr <- IQR(dataset[[value_col]], na.rm = T)

        grouping <- c(group_col, feature_col)

        dt_quantiles <- dataset[, .(q1 = quantile(get(value_col), probs = 0.25, na.rm = T),
                                    q3 = quantile(get(value_col), probs = 0.75, na.rm = T),
                                    iqr = IQR(get(value_col), na.rm=T)), by=grouping]

        dt_bounds <- dt_quantiles[, .(lower = q1 - 1.5*iqr,
                                      upper = q3 + 1.5*iqr), by=grouping]

        dt_outliers <- NULL

        if (!is.null(grouping)) {
          dt_outliers <- merge(dataset, dt_bounds, by=grouping)[!between(get(value_col), lower, upper),]
        } else {
          dt_outliers <- dataset[!between(get(value_col), dt_bounds$lower, dt_bounds$upper),]
        }

        p <- ggplot(dataset, aes(x=toName(feature_col), y=toName(value_col),
                                 group=toName(feature_col), color=toName(feature_col), fill=toName(feature_col))) +
          geom_boxplot_interactive(aes(tooltip=paste(toName(feature_col),
                                                     toName(value_col),
                                                     sep='\n'),
                                       data_id=paste0(paste(toName(date_col), toName(group_col), toName(feature_col), sep='|'), '|')),
                                   fill=NA, size=1, outlier.shape=NA, na.rm = T, show.legend = F) # position = position_dodge(1),

        if (nrow(dt_outliers) > 0) {
          p <- p + geom_point_interactive(data=dt_outliers,
                                 aes(color=toName(feature_col), fill=toName(feature_col),
                                     tooltip=paste0('y = ', comma(toName(value_col))),
                                     data_id=toName('#id')),
                                 size=2, alpha=1, position = position_jitter(height=0, width=0.375), show.legend = F)
        }

        if (rotate) {
          p <- p + scale_x_discrete(limits = rev(levels(dataset[[feature_col]])))
        }
      }

      if (show_rug) {
        if ((input$distGeom == 'Histogram' && !params$rotate) ||
            (input$distGeom != 'Histogram' && params$rotate)) {
          p <- p + geom_rug(sides='b', size=0.25, # aes(y=0),
                            position = position_jitter(height=0), alpha=0.25, show.legend = F, na.rm = T)
        } else {
          p <- p + geom_rug(sides='l', size=0.25, # aes(x=0),
                            position = position_jitter(width=0), alpha=0.25, show.legend = F, na.rm = T)
        }
      }
    })

    return(facet_features(p, x_lab=x_lab, y_lab=y_lab, x_format=x_format, y_format=y_format))

  })

  plot_final$dist <- reactive({
    req(my$dataset_p,
        n$groups <= 50, n$features <= 100)

    return(postProcessPlot(plot$dist()))
  })

  observeEvent(input$reactivity, {
    if (input$reactivity==TRUE) {
      output$distribution_plot <- renderggiraph({ plot_final$dist() %>% asGGiraph() })
    } else {
      output$distribution_plot <- renderggiraph({
        input$submit # fire on 'submit'
        isolate({ plot_final$dist() %>% asGGiraph() })
      })
    }
  })

  my$dataset_wide <- reactive({

    req(my$dataset_dates_parsed(),
        input$featureX, input$featureY)

    # print('Getting wide dataset.')

    if (n$features <= 1) {
      return(my$dataset_dates_parsed())
    } else {

      dataset <- my$dataset_dates_parsed()

      feature_col <- cols$feature
      value_col <- isolate(c$value)

      id_cols <- paste(colnames(dataset) %>% .[!(. %in% c(feature_col, value_col)) & !grepl(., pattern='^#')],
                       collapse = '+')

      if (length(id_cols) == 0 || id_cols == '') { id_cols <- '.' }
      else { id_cols <- sub(id_cols, pattern='(.+\\s.+)', replacement='`\\1`') }

      f <- paste(id_cols, feature_col, sep='~')

      return(dataset[get(feature_col) %in% c(input$featureX, input$featureY), ] %>%
                          dcast(formula = f, value.var=value_col, fun.aggregate=mean)) #, fill=NA))
    }
  })

  output$pairsPlotMessage <- renderText({
    if (n$features > 1 && is.null(c$value)) {
      'Select a value column.'
    } else if (is.null(input$featureX) || is.null(input$featureY)) {
      'Specify the X and Y axes.'
    } else { # if (is.null(input$aggWithinFUN) || n$features == nrow(my$dataset_p)) {
      'Using source dataset.'
    }
  })

  observe({
    req(my$dataset_p) #, pcols$feature)

    all_features <- NULL

    if (between(n$features, 1, 100)) {
      all_features <- levels(my$dataset_p[[pcols$feature]])
    } else {
      col_names <- colnames(my$dataset_dates_parsed()) %>% .[. != '#id']
      all_features <- col_names[my$dataset_dates_parsed()[, lapply(.SD, class), .SDcols=col_names][1] %in% c('numeric', 'integer', 'double')]
    }

    if (length(all_features) > 100) {
      all_features <- all_features[1:100]
    }

    updateSelectizeInput(session, 'featureX', choices = all_features)
    updateSelectizeInput(session, 'featureY', choices = all_features)
  })

  plot$pairs <- reactive({
    req(my$dataset_wide(), input$featureX, input$featureY)

    # print('Entered pairs_plot block.')

    col_names <- colnames(my$dataset_wide())

    group_col <- cols$group

    isolate({
      feature_x <- input$featureX
      feature_y <- input$featureY

      if ('#id' %in% col_names) {
        id_expr <- paste0("toName('#id')")
      } else {
        col_names <- col_names[!(col_names %in% c('#id', feature_x, feature_y))]
        id_expr <- paste0('paste0(paste(', paste(col_names, collapse = ', '), ', sep="|"), "|")')
      }

      p <- ggplot(my$dataset_wide(), aes(x=toName(feature_x), y=toName(feature_y),
                                         group=toName(group_col), color=toName(group_col), fill=toName(group_col),
                                         tooltip=paste(paste0('x = ',comma(toName(feature_x))),
                                                       paste0('y = ',comma(toName(feature_y))),
                                                       sep='\n'),
                                         data_id = eval(parse(text=id_expr)))) +
        geom_point_interactive(size=2, alpha=0.5) +
        geom_smooth(method='lm', se=F, color='gray20', linetype='dashed')

    })

    return(facet_features(p, x_lab=feature_x, y_lab=feature_y,
                          x_format=scales::comma, y_format = scales::comma, no_legend = TRUE))
  })

  plot_final$pairs <- reactive({
    req(my$dataset_wide(), n$groups <= 50, plot$pairs())

    return(postProcessPlot(plot$pairs(), n_colors = isolate(n$groups)))
  })

  observeEvent(input$reactivity, {
    if (input$reactivity==TRUE) {
      output$pairs_plot <- renderggiraph({ plot_final$pairs() %>% asGGiraph() })
    } else {
      output$pairs_plot <- renderggiraph({
        input$submit # fire on 'submit'
        isolate({ plot_final$pairs() %>% asGGiraph() })
      })
    }
  })

  my$theme <- reactive({
    # print('Entered theme block.')

    angle_x <- as.integer(input$angleX)

    t <- theme(axis.text.x = element_text(size=rel(1.5)),
              axis.text.y = element_text(size=rel(1.5)),
              legend.title = element_blank(),
              legend.position = input$legendPos,
              legend.justification = 'top',
              legend.box.background = element_rect(color='gray30'),
              legend.text = element_text(size=rel(1.25)),
              strip.background = element_rect(color = 'gray30'),
              strip.text = element_text(size=rel(1.25)),
              # panel.background = element_rect(color='gray30'),
              panel.border = element_rect(color='gray30', fill=NA),
              axis.title.y = element_text(size=rel(1.25), face='bold'),
              axis.title.x = element_text(size=rel(1.25), face='bold'),
              plot.title = element_text(size=rel(2.0), hjust=0.5, face='bold'),
              plot.subtitle = element_text(size=rel(1.5), hjust=0.5),
              plot.caption = element_text(size=rel(1.5), face = 'italic'),
              plot.background = element_rect(color=NA)
              )

    if (angle_x > 0) {
      if (angle_x == 90) {
        t <- t + theme(axis.text.x = element_text(size=rel(1.5), hjust=1, vjust=0.35, angle=angle_x))
      } else {
        t <- t + theme(axis.text.x = element_text(size=rel(1.5), hjust=1, angle=angle_x))
      }
    }

    if (params$rotate) {
      t <- t + theme(panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank())
    } else {
      t <- t + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank())
    }

    return(t)
  })

  facet_features <- function(p, x_lab=NULL, y_lab=NULL, no_legend = FALSE,
                             x_format=scales::comma, y_format=scales::comma) {

    # print('Entered facet_features function.')

    if (nchar(plot_xlab_dbnce()) > 0) {
      if (grepl(plot_xlab_dbnce(), pattern='^\\s+$')) {
        x_lab <- NULL
      } else {
        x_lab <- plot_xlab_dbnce()
      }
    }

    if (nchar(plot_ylab_dbnce()) > 0) {
      if (grepl(plot_ylab_dbnce(), pattern='^\\s+$')) {
        y_lab <- NULL
      } else {
        y_lab <- plot_ylab_dbnce()
      }
    }

    if (params$rotate) {
      p <- p + coord_flip()
    }

    n_x_breaks <- n$x_breaks()
    n_y_breaks <- n$y_breaks()

    if (!is.null(x_format)) {


      scale_x_args <- list(name=x_lab, labels=x_format, breaks=scales::pretty_breaks(n=n_x_breaks), expand=c(0.01, 0))

      if (params$fixLimsX && isolate(n$groups) <= 1 && !identical(x_format, waiver())) {
        # waiver is only used when leaving numeric part of date as-is.
        scale_x_args <- c(scale_x_args, list(limits=c(0, NA)))
      }

      p <- p + do.call(scale_x_continuous, scale_x_args)

    } else { # if (!missing(x_lab)) {
      p <- p + labs(x=x_lab)
    }

    is_polar_coords <- as.logical((class(p$coordinates)[1] == 'CoordPolar'))

    if (!is.null(y_format)) {

      xpand <- if (is_polar_coords) { c(0, 0) } else { c(0.025, 0) }

      scale_y_args <- list(name=y_lab, labels = y_format, breaks=scales::pretty_breaks(n=n_y_breaks), expand=xpand)

      if (params$fixLimsY && n$groups <= 1) {
        scale_y_args <- c(scale_y_args, list(limits=c(0, NA)))
      }

      p <- p + do.call(scale_y_continuous, scale_y_args)

    } else { # if (!missing(y_lab)) {
      p <- p + labs(y=y_lab)
    }

    facet_scales <- 'fixed'

    if (!is_polar_coords) {
      if (!params$fixLimsY && !params$fixLimsX) {
        facet_scales <- 'free'
      } else if (!params$fixLimsX) {
        facet_scales <- 'free_x'
      } else if (!params$fixLimsY) {
        facet_scales <- 'free_y'
      } else {
        facet_scales <- 'fixed'
      }
    }

    if (params$facetFeatures) {
      isolate({
        p <- p + facet_grid(toName(pcols$feature)~toName(pcols$group), scales = facet_scales) + guides(color=F, fill=F)
      })
    } else {
      legend_pos <- input$legendPos
      isolate({
        if (n$groups > 0) {
          p <- p + facet_wrap(~toName(pcols$group), scales = facet_scales, nrow=3)
        }
        if (n$features == 0 || no_legend == TRUE) {
          p <- p + guides(color=F, fill=F)
        } else {
          legend_params <- NULL
          if (legend_pos == 'bottom') {
            legend_params <- guide_legend(title=NULL, direction='horizontal', nrow=1, byrow=T)
          } else { # input$legendPos == 'top'
            legend_params <- guide_legend(title=NULL, direction='vertical', ncol=1, byrow=F)
          }
          p <- p + guides(color=legend_params, fill=legend_params)
        }
      })
    }

    return(p)
  }

  postProcessPlot <- function (p, myTheme=my$theme, baseTheme=theme_bw, n_colors = NULL,
                               title=NULL, subtitle=NULL, caption=NULL) { #xlab=NULL, ylab=NULL) {

    # print('Entered post_process block')

    if (missing(n_colors)) {
      n_colors <- max(1, isolate(n$features))
    } else {
      n_colors <- max(1, n_colors)
    }

    color_pal <- NULL

    if (is.null(input$plotColors)) {
      # https://stackoverflow.com/a/8197703
      color_pal <- gg_color_hue(n_colors)
    } else if (input$plotColors %in% colors(distinct=T)) {
        # RColorBrewer uses cap-letters
        # color_pal <- RColorBrewer::brewer.pal(n_colors, name=input$plotColors)[1:n_colors]
      color_pal <- gradient_n_pal(c(input$plotColors, 'gray90'))(seq(0, 1, length.out = n_colors))
    } else {
      color_pal <- match.fun(paste0(input$plotColors, '_pal'))()(n_colors)[1:n_colors]

      color_pal[is.na(color_pal)] <- '#808080'
    }

    p <- p + scale_color_manual(values=color_pal) + scale_fill_manual(values=color_pal)

    if (missing(title)) {
      title <- if (!is.null(p$labels$title)) { p$labels$title } else { plot_title_dbnce() }
    }
    if (missing(subtitle)) {
      subtitle <- if (!is.null(p$labels$subtitle)) { p$labels$subtitle } else { plot_subtitle_dbnce() }
    }
    # xlab <- NULL
    if (missing(caption)) {
      caption <- if (!is.null(p$labels$caption)) { p$labels$caption } else { plot_caption_dbnce() }
    }

    if (missing(baseTheme) && !is.null(input$plotTheme)) {
      baseTheme <- match.fun(paste0('theme_', input$plotTheme))
    }

    p <- p + baseTheme() + my$theme() +
          labs(title=bquote(underline(.(title))),
               subtitle=subtitle,
               caption=caption)
  }

  plot$height <- reactive({
    if (!between(plot_height_dbnce(), 3, 120)) { 10L } else { as.integer(plot_height_dbnce()) }
  })

  plot$width <- reactive({
    if (!between(plot_width_dbnce(), 3, 120)) { 16L } else { as.integer(plot_width_dbnce()) }
  })

  asGGiraph <- function(p) {
    return(ggiraph(code = print(p), selection_type = 'multiple', zoom_max = 5, width = 1,
                   height_svg = plot$height(), width_svg = plot$width(),
                   hover_css = "cursor:pointer;stroke:red;stroke-opacity:1;stroke-width:2;stroke-dasharray:10,10",
                   selected_css = "stroke:red;stroke-opacity:1;stroke-width:1.5;stroke-dasharray:10,10"))
  }

  output$download_dist_plot <- downloadHandler(
    filename = function() { paste('distribution.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_final$dist(), device = "png",
             height = plot$height(), width = plot$width(), units = 'in')
    }
  )

  output$download_trend_plot <- downloadHandler(
    filename = function() { paste('trends.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_final$trends(), device = "png",
             height = plot$height(), width = plot$width(), units = 'in')
    }
  )

  output$download_count_plot <- downloadHandler(
    filename = function() { paste('composition.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_final$counts(), device = "png",
             height = plot$height(), width = plot$width(), units = 'in')
    }
  )

  output$download_pairs_plot <- downloadHandler(
    filename = function() { paste('feature-comparison.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_final$pairs(), device = "png",
             height = plot$height(), width = plot$width(), units = 'in')
    }
  )

  # https://yihui.shinyapps.io/DT-info/
  output$download = downloadHandler('data.csv', content = function(file) {
    req(my$dataset, input$dataset_rows_all)

    fwrite(my$dataset[input$dataset_rows_all,], file)
  })

  output$download_final = downloadHandler('data-filtered.csv', content = function(file) {
    req(my$dataset_grouped(), input$dataset_grouped_rows_all)

    fwrite(my$dataset_grouped()[input$dataset_grouped_rows_all,], file)
  })

  output$download_selected = downloadHandler('data-selected.csv', content = function(file) {
    req(my$dataset_selected())

    fwrite(my$dataset_selected(), file)
  })

  outputOptions(output, 'dataset', suspendWhenHidden=FALSE, priority=3)
  outputOptions(output, 'dataset_grouped', suspendWhenHidden=FALSE, priority=2)
  outputOptions(output, 'dataset_selected', suspendWhenHidden=FALSE, priority=1)
}
