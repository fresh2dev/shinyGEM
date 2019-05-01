fluidRow(
  box(
    width = 12,
    title = 'Source Dataset',
    solidHeader = TRUE,
    collapsible = FALSE,
    status = 'primary',
    splitLayout(
      downloadButton('download', label = 'Download'),
      verbatimTextOutput('sourceDatasetMessage', placeholder = TRUE)
    ),
    hr(),
    DT::dataTableOutput('dataset') %>% withSpinner(type = 6, color = '#3c8dbc')
  )
)
