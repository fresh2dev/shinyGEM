#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyCustom)
library(DT)
library(rlang)
library(data.table)
library(readxl)
library(readr)
library(magrittr)
library(ggplot2)
library(scales)
library(ggthemes)
library(ggiraph)
library(grid)
library(gridExtra)
library(GGally)
library(lubridate)
library(anytime)

source('date_formats.R')

options(spinner.type=6, spinner.color='#3c8dbc')

theme_pattern <- '^theme\\_(?!get|update|void|set|linedraw|replace|wsj|map|solid|fivethirtyeight)'

date_format_list <- list('year-month-day' = c('ymd'='ymd', 'ymd_h'='ymd_H', 'ymd_hm'='ymd_HM', 'ymd_hms'='ymd_HMS'),
                         'month-day-year' = c('mdy'='mdy', 'mdy_h'='mdy_H', 'mdy_hm'='mdy_HM', 'mdy_hms'='mdy_HMS'),
                         'day-month-year' = c('dmy'='dmy', 'dmy_h'='dmy_H', 'dmy_hm'='dmy_HM', 'dmy_hms'='dmy_HMS'),
                         'Date Parts' = c('Year'='%Y', 'Month'='%m',
                                          'Day of Year'='%j',
                                          'Year-Month'='%Y-%m', 'Month-Year'='%m-%Y', 'Month-Day'='%m-%d'),
                         'Time Parts' = c('Hour'='%H', 'Minute'='%M', 'Second'='%S',
                                          'Hour:Minute'='%H:%M', 'Hour:Minute:Second'='%H:%M:%S'),
                         'Other' = c(' - Guess - ' = '-', ' '='-')
                         )

date_trans_list <- list('year-month-day' = c('ymd'='%Y-%m-%d', 'ymd_h'='%Y-%m-%d %H', 'ymd_hm'='%Y-%m-%d %H:%M', 'ymd_hms'='%Y-%m-%d %H:%M:%S'),
                        'month-day-year' = c('mdy'='%m-%d-%Y', 'mdy_h'='%m-%d-%Y %H', 'mdy_hm'='%m-%d-%Y %H:%M', 'mdy_hms'='%m-%d-%Y %H:%M:%S'),
                        'day-month-year' = c('dmy'='%d-%m-%Y', 'dmy_h'='%d-%m-%Y %H', 'dmy_hm'='%d-%m-%Y %H:%M', 'dmy_hms'='%d-%m-%Y %H:%M:%S'),
                        'Date Parts' = c('Year'='year', 'Month'='month',
                                         'Day of Year'='%j', 'Week of Year'='%V', 'Quarter of Year'='quarter',
                                         'Day of Week'='wday', 'Day of Month'='mday',
                                         'Year-Quarter'='quarter', 'Year-Month'='%Y-%m',
                                         'Month-Day'='%m-%d'),
                        'Time Parts' = c('Hour'='hour', 'Minute'='minute', 'Second'='second',
                                         'Hour:Minute'='%H:%M', 'Hour:Minute:Second'='%H:%M:%S'))

header <- dashboardHeader(titleWidth = 150,
  title = 'Shiny GEM')

sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(id = 'sb_dataset',
              menuItem('Home', tabName='load_dataset', icon = icon('home'), selected = TRUE),
              menuItem('Documentation', icon = icon('book'), href = 'https://www.donaldmellenbruch.com/post/introducing-shiny-gem/', newtab = TRUE),
              # menuItem('Say Hi!', icon = icon('twitter'), href = 'https://twitter.com/dm3ll3n', newtab = TRUE),
              menuItem('GitHub', icon = icon('github'), href = 'https://github.com/dm3ll3n/Shiny-GEM', newtab = TRUE),
              menuItem('Me', icon = icon('user-o'), href = 'https://www.donaldmellenbruch.com', newtab = TRUE)
  )
)

body <- dashboardBody(
  # https://stackoverflow.com/a/36471739
  tags$style(type = "text/css", "div.nav-tabs-custom {height: 110vh !important;}"),
  tags$style(type = "text/css", "#count_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(type = "text/css", "#trends_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(type = "text/css", "#distribution_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(type = "text/css", "#pairs_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(type = "text/css", ".box {border: 2px solid #3c8dbc !important;}"),
  tags$style(type = "text/css", "hr.blackline { margin-top: 0px; border-top: 2px solid #808080 !important;}"),
  # https://stackoverflow.com/a/40098855
  tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
  tags$head(
    tags$style(type="text/css", "text {font-family: Segoe UI,sans-serif}")
  ),

  # https://stackoverflow.com/a/32244289
  tags$style("
.nav-tabs {
  background-color: #3c8dbc;
}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
  background-color: transparent;
  color: #FFF;
  border-color: transparent;
  border-left-color: #FFF;
  border-right-color: #FFF;
}

.nav-tabs-custom .nav-tabs li.active {
  border-top-color: #FFF;
  color: #FFF;
}"),

  tabItems(

    tabItem(tabName = 'load_dataset',

            useShinyCustom(slider_delay = 1500, text_delay = 1500, text_policy = 'throttle'),

            fluidRow(width='100%',
                     box(width = 12, collapsible = TRUE, title = 'Load & Transform', solidHeader = TRUE, status='primary',

                         box(width=3, title = 'Load', # background = 'light-blue',

                             selectizeInput('presetDataset', label='Example Datasets:',
                                            choices=c('Game Ratings', 'College Scorecards', 'Starbucks Nutrition', 'Border Patrol Apprehensions'),
                                            selected = 'College Scorecards', multiple=FALSE),

                             br(),

                             tags$label(class='control-label', 'User Dataset:'),
                             fileInput('userFile', label = NA, # 'Custom Dataset:',
                                       buttonLabel = 'Upload', placeholder = 'CSV - Excel - RDS', multiple = FALSE,
                                       accept = c('.csv', '.xlsx', '.rds')), # c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

                             actionButton('startOver', 'Undo Changes', icon=icon('undo'), width = '100%')
                         ),

                         box(width=3, title = 'Combine / Rename / Drop', # background = 'light-blue',
                             selectizeInput('columnsToCombine', label = 'Columns to Combine:', choices = NULL, multiple = TRUE),
                             textInput('combinedColName', label=NA, placeholder = 'new_column_name'),
                             shiny::splitLayout(
                               textInput('sepChar', label=NA, placeholder = 'separator'),
                               checkboxInput('keepCombinedCols', label='Keep combined cols.', value=FALSE)
                             ),
                             actionButton('combineCols', width='100%', icon = icon('compress'), label = 'Combine')
                         ),

                         box(width=3, title = 'Separate', # background = 'light-blue',
                             selectizeInput('columnToSplit', label = 'Column to Split:', choices = NULL, multiple = TRUE, options=list(maxItems=1)),
                             shiny::splitLayout(
                               textInput('splitChar', label=NA, placeholder = 'Split RegEx'),
                               checkboxInput('keepSplitCols', label='Keep split col.', value=FALSE)
                             ),
                             actionButton('separateCols', width='100%', icon = icon('sitemap'), label = 'Separate')
                         ),

                         box(width=3, title = 'Reshape', # background = 'light-blue',
                             selectizeInput('userFileIdCols', label = 'ID columns:',
                                            choices = NULL, multiple = TRUE),

                             shiny::splitLayout(
                               textInput('meltedKeyName', label=NA, placeholder = 'key_column'),
                               textInput('meltedValueName', label=NA, placeholder = 'value_column')
                             ),

                             actionButton('meltData', width='100%', icon = icon('long-arrow-down'), label = 'Melt (wide to long)')
                         )
                     )
            ),

            hr(class='blackline'),

            fluidRow(
              box(width=12, title = 'Source Dataset', solidHeader = TRUE, collapsible = FALSE, status='primary',
                  splitLayout(
                    downloadButton('download', label = 'Download'),
                    verbatimTextOutput('sourceDatasetMessage', placeholder = TRUE)
                  ),
                  hr(),
                  DT::dataTableOutput('dataset') %>% withSpinner(type=6, color = '#3c8dbc')
              )
            ),

            hr(class='blackline'),

            fluidRow(width='100%',
                     box(width = 12, collapsible = TRUE, title = 'Grouping', solidHeader = TRUE, status='primary',
                         box(width = 12, # background = 'light-blue',
                             shiny::splitLayout(
                             selectizeInput('date', 'Date Column:', choices=NULL, multiple = TRUE, options=list(maxItems=1)),

                             selectizeInput('dateFormat', 'Date Format:',
                                            choices=lapply(date_format_list, names),
                                            selected = ' * Guess * ', multiple = TRUE, options=list(maxItems=1)),

                             dateRangeInput('dateRange', 'Date Range:', format = 'mm/dd/yyyy', startview = 'month', separator = 'to',
                                            start = NA, end = NA),

                             selectizeInput('dateTransform', 'Date Transformation:',
                                            choices=lapply(date_trans_list, names),
                                            selected = NA, multiple = TRUE, options=list(maxItems=1))
                            ),
                            conditionalPanel('input.date',
                              checkboxGroupInput('daysOfWeek', 'Included Days of Week:', inline = TRUE,
                                                 choices = wday(1:7, label = T, abbr = F),
                                                selected = wday(1:7, label = T, abbr = F))
                            )
                         ),

                         box(width = 3, # background = 'light-blue',
                             selectizeInput('group', 'Group Column (facet by):', choices=NULL, multiple = TRUE, options=list(maxItems=1))
                         ),

                         box(width = 3, # background = 'light-blue',
                             selectizeInput('feature', 'Feature Column (color by):', choices=NULL, multiple = TRUE, options=list(maxItems=1))
                         ),

                         box(width=6,
                             shiny::splitLayout(
                               selectizeInput('value', 'Value Column:', choices=NULL, multiple = TRUE, options=list(maxItems=1)),
                               selectizeInput('aggWithinFUN', 'Within-Groups Aggregation:',
                               							 choices=c('sum', 'mean', 'median', 'min', 'max'),
                               							 selected = NA, multiple = TRUE, options=list(maxItems=1)),
                               customNumericInput('decimalPrecision', 'Rounding Precision:', min = -10, max = 10, value = 2, step = 1) #, round = T)
                             )
                           )
                     )
            ),

            hr(class='blackline'),

            fluidRow(
              box(width=12, title = 'Grouped Dataset', solidHeader = TRUE, collapsible = FALSE, status='primary',
              		shiny::splitLayout(
              			downloadButton('download_final', label = 'Download'),
              			verbatimTextOutput('groupCountText', placeholder = T),
              			verbatimTextOutput('featureCountText', placeholder = T)
              		),
                  hr(),
                  DT::dataTableOutput('dataset_grouped') %>% withSpinner(type=6, color = '#3c8dbc')
              )
            ),

            hr(class='blackline'),

            fluidRow(
              box(width = 12, collapsible = TRUE, title = 'Plot Options', solidHeader = TRUE, status='primary',
                  box(width=2,
                      radioButtons('plotValues', label='Values to Plot:', inline = TRUE,
                                   choices=c('values', '# observations'),
                                   selected='# observations'),
                      hr(),
                      radioButtons('legendPos', label='Legend Position:', inline = TRUE,
                                   choices=c('bottom', 'right'),
                                   selected='bottom'),
                  		hr(),
                  		checkboxInput('reactivity', label='Auto-Refresh', value=TRUE),
                  		conditionalPanel('input.reactivity==0',
                  			actionButton('submit', label='Refresh')
                  		)
                  ),
                  box(width=3,
                      checkboxGroupInput('plotParams', label='Parameters:', inline = FALSE,
                                         choices = c('Swap Groups & Features'='swap',
                                                     'Facet Features'='facetFeatures',
                                                     'Rotate'='rotate',
                                                     'Fix X Limits'='fixLimsX',
                                                     'Fix Y Limits'='fixLimsY',
                                                     'Show Data Labels'='showLabels'),
                                         selected = c('fixLimsX', 'fixLimsY')),
                  		shiny::splitLayout(
                  			customNumericInput('n_x_breaks', 'Desired X breaks:', min=1, value=31, step=1),
                  			customNumericInput('n_y_breaks', 'Desired Y breaks:', min=1, value=10, step=1)
                  		)
                    ),
                  box(width=3, # height='85px',
                      tags$label(class='control-label', 'Labels:'),
                        # shiny::splitLayout(
                          customTextInput('title', label=NA, value=NA, placeholder = 'Title'),
                          customTextInput('subtitle', label=NA, value=NA, placeholder = 'Subtitle'),
                          customTextInput('caption', label=NA, value=NA, placeholder = 'Caption'),
                  				shiny::splitLayout(
                  					customTextInput('xlab', label=NA, value=NA, placeholder = 'x-label'),
                  					customTextInput('ylab', label=NA, value=NA, placeholder = 'y-label')
                  				),
                  				radioButtons('angleX', label = 'Text Angle (X):', choices = c(0, 35, 45, 90), inline=T, selected = 35)
                        # )
                    ),
                  box(width = 4,
                      selectizeInput('plotTheme', 'Plot Theme:',
                                     choices=all_themes <- list('Stock Themes'=ls('package:ggplot2') %>% .[grepl(., pattern=theme_pattern, perl=T)] %>%
                                                                  sub(pattern=theme_pattern, replacement = '', perl=T),
                                                                'GG Themes'=ls('package:ggthemes') %>% .[grepl(., pattern=theme_pattern, perl=T)] %>%
                                                                  sub(pattern=theme_pattern, replacement = '', perl=T)),
                                     selected = 'gray', multiple = TRUE, options=list(maxItems=1)),

                      selectizeInput('plotColors', 'Plot Colors:',
                                     choices=list('GG Colors'=ls('package:ggthemes') %>% .[grepl(., pattern='(?<!shape|linetype|gradient)\\_pal$', perl = T)] %>%
                                                    sub(pattern='_pal', replacement = '', fixed=T),
                                                  'Gradients' = colors(distinct = T) %>% .[!grepl(., pattern='\\d')]),
                                     selected = 'gdocs', multiple = TRUE, options=list(maxItems=1)),

                  		shiny::splitLayout(
                  			customNumericInput('plotHeight', label = 'Plot Image Height (in.)', min = 3, max=99, value=10),
                  			customNumericInput('plotWidth', label = 'Plot Image Width (in.)', min = 3, max=99, value=16)
                  		)
                  )
              )
            ),

            hr(class='blackline'),

            fluidRow(
              box(width = 12, collapsible = TRUE,
                tabBox(id = 'tabContainer', width=12,
                			 tabPanel('Distribution', icon=icon('bar-chart'),
                			 				 shiny::splitLayout(
                			 				 	shiny::splitLayout(
                			 				 		downloadButton('download_dist_plot', label = 'Download'),
  	              			 				 	radioButtons('distGeom', label='Geom:', choices = c('Histogram', 'Boxplot'), selected = 'Histogram', inline = T),
  	              			 				 	checkboxInput('showRug', label='Show Rug', value = TRUE)
                			 				 	),
              			 				 		conditionalPanel('input.distGeom=="Histogram"',
              			 				 										 radioButtons('histValuesAs', label='Values As:', choices = c('Frequency', '% of Grand Total', '% of Group Total'), selected = 'Frequency', inline = T)
              			 				 		),
              			 				 		conditionalPanel('input.distGeom=="Histogram"',
              			 				 			customNumericInput('bins', 'Bins', min=2, value = 30, step=1, width='50%')
              			 				 		),
                			 				 	verbatimTextOutput('distPlotMessage', placeholder = TRUE)
                			 				 ),
                			 				 hr(),
                			 				 ggiraphOutput('distribution_plot') %>% withSpinner(type=6, color = '#3c8dbc')
                			 ),
                       tabPanel('Composition', icon=icon('pie-chart'),
                                shiny::splitLayout(
                                	downloadButton('download_count_plot', label = 'Download'),
                                  radioButtons('countGeom', label='Geom:', choices = c('Bars', 'Bars Stacked', 'Pie'), selected = 'Bars', inline = T),
                                  conditionalPanel('input.countGeom != "Pie"',
                                    radioButtons('countValuesAs', label='Values As:', choices = c('Values', '% of Grand Total', '% of Group Total'), selected = 'Values', inline = T)
                                  ),
                                  verbatimTextOutput('countPlotMessage', placeholder = TRUE)
                                ),
                                hr(),
                                ggiraphOutput('count_plot', width='100%') %>% withSpinner(type=6, color = '#3c8dbc')
                       ),
                         tabPanel('Trends', icon=icon('line-chart'),
                                  shiny::splitLayout(
                                  	downloadButton('download_trend_plot', label = 'Download'),
                                    radioButtons('trendGeom', label='Geom:', choices = c('Lines', 'Bars'), selected = 'Lines', inline = T),
                                    radioButtons('trendValuesAs', label='Values As:', choices = c('Values', '% of Grand Total', '% of Group Total'), selected = 'Values', inline = T),
                                    verbatimTextOutput('trendPlotMessage', placeholder = TRUE)
                                  ),
                                  hr(),
                                  ggiraphOutput('trends_plot') %>% withSpinner(type=6, color = '#3c8dbc')
                         ),
                       tabPanel('Feature Pairs', icon=icon('exchange'),
                                shiny::splitLayout(
                                	downloadButton('download_pairs_plot', label = 'Download'),
                                  selectizeInput('featureX', label='X Feature', choices=NULL, selected=NA, multiple = TRUE, options=list(maxItems=1)),
                                  selectizeInput('featureY', label='Y Feature', choices=NULL, selected=NA, multiple = TRUE, options=list(maxItems=1)),
                                  # box(background = 'light-blue', actionButton('viewPairsPlot', 'View / Refresh', icon=icon('refresh'), width='100%')),
                                  verbatimTextOutput('pairsPlotMessage', placeholder = T)
                                ),
                                hr(),
                                ggiraphOutput('pairs_plot') %>% withSpinner(type=6, color = '#3c8dbc')
                          )
                )
              )
            ),

    				hr(class='blackline'),
    				hr(class='blackline'),

    				fluidRow(
    					box(width=12, title = 'Selected Dataset', solidHeader = TRUE, collapsible = FALSE, status='primary',
    					    div(
    							  downloadButton('download_selected', label = 'Download'),
    							  actionButton('selected_as_source', label = 'Set Selected as Source Dataset', icon = icon('arrow-circle-up')),
    							  actionButton('original_as_source', 'Set Original as Source Dataset', icon=icon('undo'))
    					    ),
    							hr(),
    							DT::dataTableOutput('dataset_selected') %>% withSpinner(type=6, color = '#3c8dbc')
    					)
    				)
    )
  )
)

dashboardPage(header, sidebar, body, skin = 'blue')
