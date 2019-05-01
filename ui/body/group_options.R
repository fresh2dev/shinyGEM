fluidRow(
  width = '100%',
  box(
    width = 12,
    collapsible = TRUE,
    title = 'Grouping',
    solidHeader = TRUE,
    status = 'primary',
    box(
      width = 12,
      # background = 'light-blue',
      shiny::splitLayout(
        selectizeInput(
          'date',
          'Date Column:',
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),

        selectizeInput(
          'dateFormat',
          'Date Format:',
          choices = lapply(date_format_list, names),
          selected = ' * Guess * ',
          multiple = TRUE,
          options = list(maxItems = 1)
        ),

        dateRangeInput(
          'dateRange',
          'Date Range:',
          format = 'mm/dd/yyyy',
          startview = 'month',
          separator = 'to',
          start = NA,
          end = NA
        ),

        selectizeInput(
          'dateTransform',
          'Date Transformation:',
          choices = lapply(date_trans_list, names),
          selected = NA,
          multiple = TRUE,
          options = list(maxItems = 1)
        )
      ),
      conditionalPanel(
        'input.date',
        checkboxGroupInput(
          'daysOfWeek',
          'Included Days of Week:',
          inline = TRUE,
          choices = wday(1:7, label = T, abbr = F),
          selected = wday(1:7, label = T, abbr = F)
        )
      )
    ),

    box(
      width = 3,
      # background = 'light-blue',
      selectizeInput(
        'group',
        'Group Column (facet by):',
        choices = NULL,
        multiple = TRUE,
        options = list(maxItems = 1)
      )
    ),

    box(
      width = 3,
      # background = 'light-blue',
      selectizeInput(
        'feature',
        'Feature Column (color by):',
        choices = NULL,
        multiple = TRUE,
        options = list(maxItems = 1)
      )
    ),

    box(
      width = 6,
      shiny::splitLayout(
        selectizeInput(
          'value',
          'Value Column:',
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          'aggWithinFUN',
          'Within-Groups Aggregation:',
          choices = c('sum', 'mean', 'median', 'min', 'max'),
          selected = NA,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        numericInput(
          'decimalPrecision',
          'Rounding Precision:',
          min = -10,
          max = 10,
          value = 2,
          step = 1
        )
      )
    )
  )
)
