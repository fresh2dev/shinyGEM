fluidRow(
  box(
    width = 12,
    title = 'Selected Dataset',
    solidHeader = TRUE,
    collapsible = FALSE,
    status = 'primary',
    div(
      downloadButton('download_selected', label = 'Download'),
      actionButton(
        'selected_as_source',
        label = 'Set Selected as Source Dataset',
        icon = icon('arrow-circle-up')
      ),
      actionButton(
        'original_as_source',
        'Set Original as Source Dataset',
        icon = icon('undo')
      )
    ),
    hr(),
    DT::dataTableOutput('dataset_selected') %>% withSpinner(type = 6, color = '#3c8dbc')
  )
)
