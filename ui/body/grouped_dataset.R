fluidRow(
  box(
    width = 12,
    title = 'Grouped Dataset',
    solidHeader = TRUE,
    collapsible = FALSE,
    status = 'primary',
    shiny::splitLayout(
      downloadButton('download_final', label = 'Download'),
      verbatimTextOutput('groupCountText', placeholder = T),
      verbatimTextOutput('featureCountText', placeholder = T)
    ),
    hr(),
    DT::dataTableOutput('dataset_grouped') %>% withSpinner(type = 6, color = '#3c8dbc')
  )
)
