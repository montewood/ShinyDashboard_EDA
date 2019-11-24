
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(data.table)
library(plotly)


source('./chart_functions.R', encoding = "UTF-8") # 차트함수 불러오기


# testdata

testdata <- fread('C:/Users/JDW/Desktop/my_cloud/OneDrive/R/shiny/EDA/data/dc_sample.csv', fill = T)
glimpse(testdata)


# app start ----

ui <- dashboardPage(
    dashboardHeader(title = 'EDA'),
    dashboardSidebar(
        fileInput(inputId = 'file1',
                  h4('Insert Data'),
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
        #sampleing 기능
        conditionalPanel(
            condition = "output.fileUploaded == 1",
            checkboxInput(inputId = 'sample_checkbox', label = strong('Setting sample'))
        ),
        conditionalPanel(
            condition = "input.sample_checkbox == 1",
            textInput(inputId = 'sample_inputbox', label = 'Set sample amount', value = 0)
        ),
        conditionalPanel(
            condition = 'input.sample_checkbox == 1',
            div(style = "position:absolute;right:0.5em;",
                actionButton(inputId = 'sample_Act_btn', label = 'Enter', placement = 'right')),
            br()
        ),
        br(),
        sidebarMenu(
            menuItem(text = 'View table', tabName = 'table_dataset'),
            menuItem(text = 'barchart', tabName = 'chart_bar'),
            menuItem(text = 'density', tabName = 'chart_den'),
            menuItem(text = 'box_chart', tabName = 'chart_box'),
            menuItem(text = 'qq_chart', tabName = 'chart_qq'),
            menuItem(text = 'scatterplot', tabName = 'chart_scatter'),
            menuItem(text = 'linechart', tabName = 'chart_line')
        )
    ),
    dashboardBody(
        tabItems(
            #datatable body ----
            tabItem(tabName = 'table_dataset',
                    fluidRow(
                        box(title = 'datatable',
                            width = 12, height = 660, status = 'success',
                            DT::dataTableOutput(outputId = 'table', height = 660)
                        )
                    )
            ),
            # bar chart body ----
            tabItem(tabName = 'chart_bar',
                    fluidRow(
                        box(title = 'Bar chart',
                            width = 12, height = 660, status = 'success',
                            plotlyOutput(outputId = 'barchart', height = 500),
                            column(3, checkboxInput(inputId = 'option.bar.title_center', label = 'Title center')),
                            column(3, checkboxInput(inputId = 'option.bar.axis_x_angle45', label = 'Axis x angle 45')),
                            column(3, checkboxInput(inputId = 'option.bar.Remove_legend', label = 'Remove legend')),
                            column(3, checkboxInput(inputId = 'option.bar.View_value', label = 'Show values')),
                            column(12, selectInput(inputId = 'option.bar.themes', label = '', choices = theme_list))
                        )
                    ),
                    fluidRow(
                        box(
                            title = 'Data Setting',
                            id = 'box.set_data',
                            solidHeader = TRUE,
                            # collapsible = TRUE,
                            width = 4, height = 320,
                            selectInput(inputId = 'bar.x_axis',
                                        label = 'Select X axis',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'bar.color',
                                        label = 'Color',
                                        choices = c("")
                            ),
                        ),
                        box(
                            title = 'Labs Setting',
                            id = 'box.set_labs',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'bar_labs', label = 'set labs'),
                            conditionalPanel(condition = 'input.bar_labs == 1',
                                             textInput(inputId = 'bar.title_text', label = 'Title', value = 'Plot_title'),
                                             textInput(inputId = 'bar.axis_x_text', label = 'x_axis', value = 'x'),
                                             textInput(inputId = 'bar.axis_y_text', label = 'y_axis', value = 'y')),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs_text Setting',
                            id = 'set_text_box',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'bar_themes', label = 'set themes'),
                            conditionalPanel(condition = 'input.bar_themes == 1',
                                             # *!중요!* sliderinput 값으로 변경 테스트 해보기
                                             textInput(inputId = 'bar.title_size', label = 'Title_size', value = 20),
                                             textInput(inputId = 'bar.axis_x_size', label = 'x_axis_size', value = 14),
                                             textInput(inputId = 'bar.axis_y_size', label = 'y_axis_size', value = 14))
                        )
                    ),
                    div(style = "position:absolute;right:0.5em;",
                        actionButton(inputId = 'barchart_btn', label = 'Change', placement = 'right')
                    )
            ),

            # density chart body ----
            tabItem(tabName = 'chart_den',
                    fluidRow(
                        box(title = 'density chart',
                            width = 12, height = 660, status = 'success',
                            plotlyOutput(outputId = 'densitychart', height = 500),
                            column(3, checkboxInput(inputId = 'option.den.title_center', label = 'Title center')),
                            column(3, checkboxInput(inputId = 'option.den.axis_x_angle45', label = 'Axis x angle 45')),
                            column(3, checkboxInput(inputId = 'option.den.Remove_legend', label = 'Remove legend')),
                            column(3, checkboxInput(inputId = 'option.den.View_value', label = 'Show values')),
                            column(12, selectInput(inputId = 'option.den.themes', label = '', choices = theme_list))
                        )
                    ),
                    fluidRow(
                        box(
                            title = 'Data Setting',
                            id = 'den.set_data',
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 4, height = 320,
                            selectInput(inputId = 'den.x_axis',
                                        label = 'Select X axis',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'den.color',
                                        label = 'Color',
                                        choices = c("")
                            ),
                        ),
                        box(
                            title = 'Labs Setting',
                            id = 'den.set_labs',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'den_labs', label = 'set labs'),
                            conditionalPanel(condition = 'input.den_labs == 1',
                                             textInput(inputId = 'den.title_text', label = 'Title', value = 'Plot_title'),
                                             textInput(inputId = 'den.axis_x_text', label = 'x_axis', value = 'x'),
                                             textInput(inputId = 'den.axis_y_text', label = 'y_axis', value = 'y')),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs_text Setting',
                            id = 'den.set_labs_text',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'den_themes', label = 'set themes'),
                            conditionalPanel(condition = 'input.den_themes == 1',
                                             # *!중요!* sliderinput 값으로 변경 테스트 해보기
                                             textInput(inputId = 'den.title_size', label = 'Title_size', value = 20),
                                             textInput(inputId = 'den.axis_x_size', label = 'x_axis_size', value = 14),
                                             textInput(inputId = 'den.axis_y_size', label = 'y_axis_size', value = 14))
                        )
                    ),
                    div(style = "position:absolute;right:0.5em;",
                        actionButton(inputId = 'densitychart_btn', label = 'Change', placement = 'right')
                    )
            ),

            # boxchart body ----
            tabItem(tabName = 'chart_box',
                    fluidRow(
                        box(title = 'box chart',
                            width = 12, height = 660, status = 'success',
                            plotlyOutput(outputId = 'boxchart', height = 500),
                            column(3, checkboxInput(inputId = 'option.box.title_center', label = 'Title center')),
                            column(3, checkboxInput(inputId = 'option.box.axis_x_angle45', label = 'Axis x angle 45')),
                            column(3, checkboxInput(inputId = 'option.box.Remove_legend', label = 'Remove legend')),
                            column(3, checkboxInput(inputId = 'option.box.View_value', label = 'Show values')),
                            column(12, selectInput(inputId = 'option.box.themes', label = '', choices = theme_list))
                        ),
                    ),
                    fluidRow(
                        box(
                            title = 'Data Setting',
                            id = 'set_data_box',
                            width = 4, height = 320,
                            selectInput(inputId = 'box.option1',
                                        label = 'choice box option 1',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'box.option2',
                                        label = 'choice box option 2',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'box.value',
                                        label = 'choice box value',
                                        choices = c("")
                            ),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs Setting',
                            id = 'box.set_labs',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'box_labs', label = 'set labs'),
                            conditionalPanel(condition = 'input.box_labs == 1',
                                             textInput(inputId = 'box.title_text', label = 'Title', value = 'Plot_title'),
                                             textInput(inputId = 'box.axis_x_text', label = 'x_axis', value = 'x'),
                                             textInput(inputId = 'box.axis_y_text', label = 'y_axis', value = 'y')),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs_text Setting',
                            id = 'box.set_labs_text',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'box_themes', label = 'set themes'),
                            conditionalPanel(condition = 'input.box_themes == 1',
                                             # *!중요!* sliderinput 값으로 변경 테스트 해보기
                                             textInput(inputId = 'box.title_size', label = 'Title_size', value = 20),
                                             textInput(inputId = 'box.axis_x_size', label = 'x_axis_size', value = 14),
                                             textInput(inputId = 'box.axis_y_size', label = 'y_axis_size', value = 14))
                        )
                    ),
                    div(style = "position:absolute;right:0.5em;",
                        actionButton(inputId = 'boxchart_btn', label = 'Change', placement = 'right')
                    )
            ),

            # qqchart body ----
            tabItem(tabName = 'chart_qq',
                    fluidRow(
                        box(title = 'QQ chart',
                            width = 12, height = 660, status = 'success',
                            plotlyOutput(outputId = 'qqchart', height = 500),
                            column(3, checkboxInput(inputId = 'option.qq.title_center', label = 'Title center')),
                            column(3, checkboxInput(inputId = 'option.qq.axis_x_angle45', label = 'Axis x angle 45')),
                            column(3, checkboxInput(inputId = 'option.qq.Remove_legend', label = 'Remove legend')),
                            column(3, checkboxInput(inputId = 'option.qq.View_value', label = 'Show values')),
                            column(12, selectInput(inputId = 'option.qq.themes', label = '', choices = theme_list))
                        ),
                    ),
                    fluidRow(
                        box(
                            title = 'Data Setting',
                            id = 'set_data_box',
                            width = 4, height = 320,
                            selectInput(inputId = 'qq.x_axis',
                                        label = 'Select X axis',
                                        choices = c("")
                            ),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs Setting',
                            id = 'set_visual_box',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'qq_labs', label = 'set labs'),
                            conditionalPanel(condition = 'input.qq_labs == 1',
                                             textInput(inputId = 'qq.title_text', label = 'Title', value = 'Plot_title'),
                                             textInput(inputId = 'qq.axis_x_text', label = 'x_axis', value = 'x'),
                                             textInput(inputId = 'qq.axis_y_text', label = 'y_axis', value = 'y')),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs_text Setting',
                            id = 'qq.set_labs_text',
                            width = 4, height = 320,
                            checkboxInput(inputId = 'qq_themes', label = 'set themes'),
                            conditionalPanel(condition = 'input.qq_themes == 1',
                                             # *!중요!* sliderinput 값으로 변경 테스트 해보기
                                             textInput(inputId = 'qq.title_size', label = 'Title_size', value = 20),
                                             textInput(inputId = 'qq.axis_x_size', label = 'x_axis_size', value = 14),
                                             textInput(inputId = 'qq.axis_y_size', label = 'y_axis_size', value = 14))
                        )
                    ),
                    div(style = "position:absolute;right:0.5em;",
                        actionButton(inputId = 'qqchart_btn', label = 'Change', placement = 'right')
                    )
            ),
            #scatterchart body ----
            tabItem(tabName = 'chart_scatter',
                    fluidRow(
                        box(title = 'Scatter chart',
                            width = 12, height = 660, status = 'success',
                            plotlyOutput(outputId = 'scatterchart', height = 500),
                            column(3, checkboxInput(inputId = 'option.scatter.title_center', label = 'Title center')),
                            column(3, checkboxInput(inputId = 'option.scatter.axis_x_angle45', label = 'Axis x angle 45')),
                            column(3, checkboxInput(inputId = 'option.scatter.Remove_legend', label = 'Remove legend')),
                            column(3, checkboxInput(inputId = 'option.scatter.View_value', label = 'Show values')),
                            column(12, selectInput(inputId = 'option.scatter.themes', label = '', choices = theme_list))
                        ),
                    ),
                    fluidRow(
                        box(
                            title = 'Data Setting',
                            id = 'set_data_box',
                            width = 4, height = 320,
                            selectInput(inputId = 'scatter.x_axis',
                                        label = 'Select X axis',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'scatter.y_axis',
                                        label = 'Select y axis',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'scatter.color',
                                        label = 'Scatter_Color',
                                        choices = c("")
                            ),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs Setting',
                            id = 'scatter.set_labs',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'scatter_labs', label = 'set labs'),
                            conditionalPanel(condition = 'input.scatter_labs == 1',
                                             textInput(inputId = 'scatter.title_text', label = 'Title', value = 'Plot_title'),
                                             textInput(inputId = 'scatter.axis_x_text', label = 'x_axis', value = 'x'),
                                             textInput(inputId = 'scatter.axis_y_text', label = 'y_axis', value = 'y')),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs_text Setting',
                            id = 'box.set_labs_text',
                            width = 4, height = 320,
                            checkboxInput(inputId = 'scatter_themes', label = 'set themes'),
                            conditionalPanel(condition = 'input.scatter_themes == 1',
                                             # *!중요!* sliderinput 값으로 변경 테스트 해보기
                                             textInput(inputId = 'box.title_size', label = 'Title_size', value = 20),
                                             textInput(inputId = 'scatter.axis_x_size', label = 'x_axis_size', value = 14),
                                             textInput(inputId = 'scatter.axis_y_size', label = 'y_axis_size', value = 14))
                        )
                    ),
                    div(style = "position:absolute;right:0.5em;",
                        actionButton(inputId = 'scatterchart_btn', label = 'Change', placement = 'right')
                    )
            ),

            #linechart body ----
            tabItem(tabName = 'chart_line',
                    fluidRow(
                        box(title = 'Line chart',
                            width = 12, height = 660, status = 'success',
                            plotlyOutput(outputId = 'linechart', height = 500),
                            column(3, checkboxInput(inputId = 'option.line.title_center', label = 'Title center')),
                            column(3, checkboxInput(inputId = 'option.line.axis_x_angle45', label = 'Axis x angle 45')),
                            column(3, checkboxInput(inputId = 'option.line.Remove_legend', label = 'Remove legend')),
                            column(3, checkboxInput(inputId = 'option.line.View_value', label = 'Show values')),
                            column(12, selectInput(inputId = 'option.line.themes', label = '', choices = theme_list))
                        ),
                    ),
                    fluidRow(
                        box(
                            title = 'Data Setting',
                            id = 'set_data_box',
                            width = 4, height = 320,
                            selectInput(inputId = 'line.x_axis',
                                        label = 'Select X axis',
                                        choices = c("")
                            ),
                            selectInput(inputId = 'line.y_axis',
                                        label = 'Select y axis',
                                        choices = c("")
                            ),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs Setting',
                            id = 'set_visual_box',
                            width = 4, height = 320,
                            # conditionalPanel condition이 '.' 인식이 안되므로 예외적으로 '_' 사용
                            checkboxInput(inputId = 'line_labs', label = 'set labs'),
                            conditionalPanel(condition = 'input.line_labs == 1',
                                             textInput(inputId = 'line.title_text', label = 'Title', value = 'Plot_title'),
                                             textInput(inputId = 'line.axis_x_text', label = 'x_axis', value = 'x'),
                                             textInput(inputId = 'line.axis_y_text', label = 'y_axis', value = 'y')),
                            collapsible = T
                        ),
                        box(
                            title = 'Labs_text Setting',
                            id = 'line.set_labs_text',
                            width = 4, height = 320,
                            checkboxInput(inputId = 'line_themes', label = 'set themes'),
                            conditionalPanel(condition = 'input.line_themes == 1',
                                             # *!중요!* sliderinput 값으로 변경 테스트 해보기
                                             textInput(inputId = 'line.title_size', label = 'Title_size', value = 20),
                                             textInput(inputId = 'line.axis_x_size', label = 'x_axis_size', value = 14),
                                             textInput(inputId = 'line.axis_y_size', label = 'y_axis_size', value = 14))
                        )
                    ),
                    div(style = "position:absolute;right:0.5em;",
                        actionButton(inputId = 'linechart_btn', label = 'Change', placement = 'right')
                    )
            )

            # tabItem(tabName = 'tt',
            #         fluidRow(box(
            #             selectInput(inputId = 'testinput',
            #                         label = 'testinput',
            #                         choices = c(""))
            #         ))) #
        ),
    ),
    skin = 'black'
)

# ----




server <- function(input, output, session){

    # insertdata <- fread('C:/Users/JDW/Desktop/my_cloud/OneDrive/R/shiny/EDA/data/dc_sample.csv', fill = T)

    observeEvent(input$file1, {

        #file1 data
        insertdata <- reactive({
            if(input$sample_Act_btn == 0)
                return(fread(gsub('\\\\', '/', input$file1$datapath), fill = T))
            return(sample_n(fread(gsub('\\\\', '/', input$file1$datapath), fill = T),
                            as.numeric(input$sample_inputbox), replace = F))
        })


        #conditionalPanel option
        output$fileUploaded <- reactive({
            return(!is.null(insertdata()))
        })

        outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

        # View table ----

        # View table
        output$table <- DT::renderDataTable({
            if(is.null(input$file1))
                return(NULL)
            insertdata()
        })



        # barchart ----

        ## bar set column
        observeEvent(input$file1,{

            bar_colname <- insertdata() %>% colnames()
            bar_colname[length(bar_colname) + 1] <- 'count'

            updateSelectInput(session, "bar.x_axis", choices = bar_colname)
            updateSelectInput(session, "bar.color", choices = bar_colname,
                              selected = colnames(insertdata())[2])
        })

        ## bar chart

        output$barchart <- renderPlotly({
            if(input$barchart_btn == 0)
                return()
            isolate({

                c.data <- insertdata()
                c.x_axis <- input$bar.x_axis
                c.col <- input$bar.color

                c.title_text <- input$bar.title_text
                c.x_text <- input$bar.axis_x_text
                c.y_text <- input$bar.axis_y_text

                c.title_themes <- input$bar.title_size
                c.x_themes <- input$bar.axis_x_size
                c.y_themes <- input$bar.axis_y_size


                if(input$bar_labs == 0){
                    p <- shiny_barchart(data = c.data, x = c.x_axis, col = c.col)
                }else if(input$bar_labs == 1){
                    # browser()
                    if(input$bar_themes == 0){
                        p <- shiny_barchart(data = c.data, x = c.x_axis, col = c.col) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text)
                    }else if(input$bar_themes == 1){
                        p <- shiny_barchart(data = c.data, x = c.x_axis, col = c.col) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text) +
                            chart.themes(title = c.title_themes, x = c.x_themes, y = c.y_themes)
                    }
                }


                ggplotly(p)
            })

            if(input$option.bar.themes != 'None'){
                p <- p + shiny_themes(input$option.bar.themes)
            }

            if(input$option.bar.title_center == 1){
                p <- p + theme(plot.title = element_text(hjust = 0.5))
            }

            if(input$option.bar.axis_x_angle45 == 1){
                p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }

            if(input$option.bar.Remove_legend == 1){
                p <- p + theme(legend.position = "none")
            }

            if(input$option.bar.View_value == 1){
                p <- p + stat_count(aes(y=..count.. ,label=..count..), geom = "text", vjust = -1.5)
            }

            ggplotly(p)
        })



        # density ----

        ## histogram set column
        observeEvent(input$file1,{

            updateSelectInput(session, "den.x_axis", choices = insertdata() %>% colnames())
            updateSelectInput(session, "den.color", choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[2])
        })

        ## density chart

        output$densitychart <- renderPlotly({
            if(input$densitychart_btn == 0)
                return()
            isolate({

                c.data <- insertdata()
                c.x_axis <- input$den.x_axis
                c.col <- input$den.color

                c.title_text <- input$den.title_text
                c.x_text <- input$den.axis_x_text
                c.y_text <- input$den.axis_y_text

                c.title_themes <- input$den.title_size
                c.x_themes <- input$den.axis_x_size
                c.y_themes <- input$den.axis_y_size


                if(input$den_labs == 0){

                    p <- shiny_densitychart(data = c.data, x = c.x_axis, col = c.col)

                }else if(input$den_labs == 1){
                    # browser()
                    if(input$den_themes == 0){

                        p <- shiny_densitychart(data = c.data, x = c.x_axis, col = c.col) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text)

                    }else if(input$den_themes == 1){

                        p <- shiny_densitychart(data = c.data, x = c.x_axis, col = c.col) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text) +
                            chart.themes(title = c.title_themes, x = c.x_themes, y = c.y_themes)

                    }
                }

                ggplotly(p)
            })

            if(input$option.den.themes != 'None'){
                p <- p + shiny_themes(input$option.den.themes)
            }

            if(input$option.den.title_center == 1){
                p <- p + theme(plot.title = element_text(hjust = 0.5))
            }

            if(input$option.den.axis_x_angle45 == 1){
                p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }

            if(input$option.den.Remove_legend == 1){
                p <- p + theme(legend.position = "none")
            }

            if(input$option.den.View_value == 1){
                p <- p + stat_count(aes(y=..count.. ,label=..count..), geom = "text", vjust = -1.5)
            }

            ggplotly(p)
        })

        #boxchart ----

        ## boxchart set column
        observeEvent(input$file1, {

            updateSelectInput(session, 'box.option1', choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[10])
            updateSelectInput(session, 'box.option2', choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[6])
            updateSelectInput(session, 'box.value', choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[6])
        })

        ## boxchart
        output$boxchart <- renderPlotly({
            if(input$boxchart_btn == 0)
                return()
            isolate({

                c.data <- insertdata()
                c.op1 <- input$box.option1
                c.op2 <- input$box.option2
                c.val <- input$box.value

                c.title_text <- input$box.title_text
                c.x_text <- input$box.axis_x_text
                c.y_text <- input$box.axis_y_text

                c.title_themes <- input$box.title_size
                c.x_themes <- input$box.axis_x_size
                c.y_themes <- input$box.axis_y_size

                if(input$box_labs == 0){

                    p <- shiny_boxchart(data = c.data, op1 = c.op1, op2 = c.op2, boxval = c.val)

                }else if(input$box_labs == 1){

                    if(input$box_themes == 0){

                        p <- shiny_boxchart(data = c.data, op1 = c.op1, op2 = c.op2, boxval = c.val) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text)

                    }else if(input$box_themes == 1){

                        p <- shiny_boxchart(data = c.data, op1 = c.op1, op2 = c.op2, boxval = c.val) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text) +
                            chart.themes(title = c.title_themes, x = c.x_themes, y = c.y_themes)
                    }
                }

                ggplotly(p)

            })

            if(input$option.box.themes != 'None'){
                p <- p + shiny_themes(input$option.box.themes)
            }

            if(input$option.box.title_center == 1){
                p <- p + theme(plot.title = element_text(hjust = 0.5))
            }

            if(input$option.box.axis_x_angle45 == 1){
                p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }

            if(input$option.box.Remove_legend == 1){
                p <- p + theme(legend.position = "none")
            }

            if(input$option.box.View_value == 1){
                p <- p + stat_count(aes(y=..count.. ,label=..count..), geom = "text", vjust = -1.5) # 적절하게 조절 필요
            }

            ggplotly(p)
        })


        #qqchart ----

        ## qq set column
        observeEvent(input$file1,{
            updateSelectInput(session, "qq.x_axis", choices = insertdata() %>% colnames())
        })

        ## qq chart
        output$qqchart <- renderPlotly({
            if(input$qqchart_btn == 0)
                return()
            isolate({

                c.data <- insertdata()
                c.x <- input$qq.x_axis

                c.title_text <- input$qq.title_text
                c.x_text <- input$qq.axis_x_text
                c.y_text <- input$qq.axis_y_text

                c.title_themes <- input$qq.title_size
                c.x_themes <- input$qq.axis_x_size
                c.y_themes <- input$qq.axis_y_size

                if(input$qq_labs == 0){

                    p <- shiny_qqchart(data = c.data, x = c.x)

                }else if(input$qq_labs == 1){

                    if(input$qq_themes == 0){

                        p <- shiny_qqchart(data = c.data, x = c.x) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text)

                    }else if(input$qq_themes == 1){

                        p <- shiny_qqchart(data = c.data, x = c.x) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text) +
                            chart.themes(title = c.title_themes, x = c.x_themes, y = c.y_themes)

                    }
                }

                ggplotly(p)

            })

            if(input$option.qq.themes != 'None'){
                p <- p + shiny_themes(input$option.qq.themes)
            }

            if(input$option.qq.title_center == 1){
                p <- p + theme(plot.title = element_text(hjust = 0.5))
            }

            if(input$option.qq.axis_x_angle45 == 1){
                p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }

            if(input$option.qq.Remove_legend == 1){
                p <- p + theme(legend.position = "none")
            }

            if(input$option.qq.View_value == 1){
                p <- p + stat_count(aes(y=..count.. ,label=..count..), geom = "text", vjust = -1.5) # 적절하게 조절 필요
            }

            ggplotly(p)

        })


        #scatterchart----


        ## scatterchart set column
        observeEvent(input$file1,{

            updateSelectInput(session, 'scatter.x_axis', choices = insertdata() %>% colnames())
            updateSelectInput(session, 'scatter.y_axis', choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[2])
            updateSelectInput(session, 'scatter.color', choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[3])
        })

        ## scatter chart
        output$scatterchart <- renderPlotly({
            if(input$scatterchart_btn == 0)
                return()
            isolate({

                c.data <- insertdata()
                c.x <- input$scatter.x_axis
                c.y <- input$scatter.y_axis
                c.color <- input$scatter.color

                c.title_text <- input$box.title_text
                c.x_text <- input$box.axis_x_text
                c.y_text <- input$box.axis_y_text

                c.title_themes <- input$box.title_size
                c.x_themes <- input$box.axis_x_size
                c.y_themes <- input$box.axis_y_size

                if(input$scatter_labs == 0){

                    p <- shiny_scatterchart(data = c.data, x = c.x, y = c.y, color = c.color)

                }else if(input$scatter_labs == 1){

                    if(input$scatter_themes == 0){

                        p <- shiny_scatterchart(data = c.data, x = c.x, y = c.y, color = c.color) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text)

                    }else if(input$scatter_themes == 1){

                        p <- shiny_scatterchart(data = c.data, x = c.x, y = c.y, color = c.color) +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text) +
                            chart.themes(title = c.title_themes, x = c.x_themes, y = c.y_themes)
                    }
                }

                ggplotly(p)
            })

            if(input$option.scatter.themes != 'None'){
                p <- p + shiny_themes(input$option.scatter.themes)
            }

            if(input$option.scatter.title_center == 1){
                p <- p + theme(plot.title = element_text(hjust = 0.5))
            }

            if(input$option.scatter.axis_x_angle45 == 1){
                p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }

            if(input$option.scatter.Remove_legend == 1){
                p <- p + theme(legend.position = "none")
            }

            if(input$option.scatter.View_value == 1){
                p <- p + stat_count(aes(y=..count.. ,label=..count..), geom = "text", vjust = -1.5) # 적절하게 조절 필요
            }


            ggplotly(p)
        })



        #linechart ----

        ## linechart set column
        observeEvent(input$file1,{

            updateSelectInput(session, 'line.x_axis', choices = insertdata() %>% colnames())
            updateSelectInput(session, 'line.y_axis', choices = insertdata() %>% colnames(),
                              selected = colnames(insertdata())[2])
        })


        ## line chart
        output$linechart <- renderPlotly({
            if(input$linechart_btn == 0)
                return()
            isolate({

                c.data <- insertdata()
                c.axis_x <- input$line.x_axis
                c.axis_y <- input$line.y_axis

                c.title_text <- input$line.title_text
                c.x_text <- input$line.axis_x_text
                c.y_text <- input$line.axis_y_text

                c.title_themes <- input$line.title_size
                c.x_themes <- input$line.axis_x_size
                c.y_themes <- input$line.axis_y_size

                if(input$line_labs == 0){

                    p <- shiny_linechart(data = c.data, x = c.axis_x, y = c.axis_y)

                }else if(input$line_labs == 1){
                    # browser()
                    if(input$line_themes == 0){

                        p <- shiny_linechart(data = c.data, x = c.axis_x, y = c.axis_y)  +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text)

                    }else if(input$line_themes == 1){

                        p <- shiny_linechart(data = c.data, x = c.axis_x, y = c.axis_y)  +
                            chart.labs(chart_title = c.title_text, chart_x = c.x_text, chart_y = c.y_text) +
                            chart.themes(title = c.title_themes, x = c.x_themes, y = c.y_themes)

                    }
                }

                ggplotly(p)


            })

            if(input$option.line.themes != 'None'){
                p <- p + shiny_themes(input$option.line.themes)
            }

            if(input$option.line.title_center == 1){
                p <- p + theme(plot.title = element_text(hjust = 0.5))
            }

            if(input$option.line.axis_x_angle45 == 1){
                p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }

            if(input$option.line.Remove_legend == 1){
                p <- p + theme(legend.position = "none")
            }

            if(input$option.line.View_value == 1){
                p <- p + stat_count(aes(y=..count.. ,label=..count..), geom = "text", vjust = -1.5) # 적절하게 조절 필요
            }

            ggplotly(p)

        })


    })


}



#run app

shinyApp(ui, server)
