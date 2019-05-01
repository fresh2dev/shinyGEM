fluidRow(
  width = '100%',
  box(
    width = 12,
    collapsible = TRUE,
    title = 'Load & Transform',
    solidHeader = TRUE,
    status = 'primary',

    box(
      width = 3,
      title = 'Load',
      # background = 'light-blue',

      selectizeInput(
        'presetDataset',
        label = 'Example Datasets:',
        choices = c(
          'Game Ratings',
          'College Scorecards',
          'Starbucks Nutrition',
          'Border Patrol Apprehensions'
        ),
        selected = 'College Scorecards',
        multiple = FALSE
      ),

      br(),

      tags$label(class = 'control-label', 'User Dataset:'),
      fileInput(
        'userFile',
        label = NA,
        buttonLabel = 'Upload',
        placeholder = 'CSV - Excel - RDS',
        multiple = FALSE,
        accept = c('.csv', '.xlsx', '.rds')
      ),
      # c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

      actionButton(
        'startOver',
        'Undo Changes',
        icon = icon('undo'),
        width = '100%'
      )
    ),

    box(
      width = 3,
      title = 'Combine / Rename / Drop',
      # background = 'light-blue',
      selectizeInput(
        'columnsToCombine',
        label = 'Columns to Combine:',
        choices = NULL,
        multiple = TRUE
      ),
      textInput('combinedColName', label = NA, placeholder = 'new_column_name'),
      shiny::splitLayout(
        textInput('sepChar', label = NA, placeholder = 'separator'),
        checkboxInput('keepCombinedCols', label = 'Keep combined cols.', value =
                        FALSE)
      ),
      actionButton(
        'combineCols',
        width = '100%',
        icon = icon('compress'),
        label = 'Combine'
      )
    ),

    box(
      width = 3,
      title = 'Separate',
      # background = 'light-blue',
      selectizeInput(
        'columnToSplit',
        label = 'Column to Split:',
        choices = NULL,
        multiple = TRUE,
        options = list(maxItems = 1)
      ),
      shiny::splitLayout(
        textInput('splitChar', label = NA, placeholder = 'Split RegEx'),
        checkboxInput('keepSplitCols', label = 'Keep split col.', value =
                        FALSE)
      ),
      actionButton(
        'separateCols',
        width = '100%',
        icon = icon('sitemap'),
        label = 'Separate'
      )
    ),

    box(
      width = 3,
      title = 'Reshape',
      # background = 'light-blue',
      selectizeInput(
        'userFileIdCols',
        label = 'ID columns:',
        choices = NULL,
        multiple = TRUE
      ),

      shiny::splitLayout(
        textInput('meltedKeyName', label = NA, placeholder = 'key_column'),
        textInput('meltedValueName', label = NA, placeholder = 'value_column')
      ),

      actionButton(
        'meltData',
        width = '100%',
        icon = icon('long-arrow-down'),
        label = 'Melt (wide to long)'
      )
    )
  )
)
