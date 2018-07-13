library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(chron)

setwd('/Users/benito/Documents/SourceData/Shiny')

annotation_data <- read.delim('Live_Manuscripts.txt', stringsAsFactors = FALSE)
annotation_data = annotation_data[1:186, -c(19:28) ]
annotation_data$Journal = as.factor(annotation_data$Journal)



convert_times = function(input_time)
{
    60 * 24 * as.numeric(times(input_time))
}

annotation_data$timeInMins = convert_times(annotation_data$Time.required)


ui = fluidPage(
    titlePanel('Visualization of Manu\'s annotation times'),
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                h3('Manuscript statistics'),
                p('Choose which variables to plot on the y axis for each manuscript. Some variables are not available for all manuscripts.'),
                selectInput(
                    inputId = 'y_axis',
                    choices = colnames(annotation_data)[c(14:19)],
                    label = colnames(annotation_data)[c(14:19)],
                    selected = colnames(annotation_data)[19] 
                            ),
                checkboxInput(
                    inputId = 'colorByJournal',
                    label = 'Check to color by Journal',
                    value = FALSE
                            ),
                checkboxInput(
                    inputId = 'displayMean',
                    label = 'Check to display mean of y axis',
                    value = FALSE
                             )
                     ),
            wellPanel(
                h3('Journal statistics'),
                p('Choose which average variable to plot on the y axis for each journal. Except for time, all the other variables are based only on the subset of manuscripts with underlying source data.'),
                selectInput(
                    inputId = 'journal_y_axis',
                    choices = colnames(annotation_data)[14:19],
                    label = colnames(annotation_data)[14:19],
                    selected = colnames(annotation_data)[19]
                           )
                     )
                     ),
            # sidebarPanel(width = 3,
            #     selectInput(
            #         inputId = 'journal_selection',
            #         label = 'Select a journal',
            #         choices = levels(annotation_data$Journal),
            #         selected = 'EMBO Journal'
            #                )
            #             ), 
        mainPanel(width = 4,
            plotOutput(outputId = 'ms_vs_variable'),
            PlotOutput(outputId = 'journal_vs_variable')
                 )
                 )
              )



server = function(input, output)
{

    selected_y_var = reactive(
        {
            req(input$y_axis)
            dplyr::select(annotation_data, matches(input$y_axis))
        }
                             )

    output$ms_vs_variable = renderPlot(
        {
            timeInMins = annotation_data$timeInMins

            if (input$colorByJournal == FALSE)
            {
                mainplot = 
                    ggplot(annotation_data, 
                        aes(x = 1:length(annotation_data$Tracking.number),y = selected_y_var())) + 
                        geom_bar(stat = 'identity', fill = '#1a3f7a', width = 0.6) + 
                        xlab('Manuscript') + 
                        ylab(names(selected_y_var())) + 
                        ggtitle(paste(names(selected_y_var()), 'per manuscript')) +
                        theme_bw() + 
                        theme(panel.border = element_blank(), 
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        axis.line = element_line(colour = "black")
                          )
                if (input$displayMean == FALSE)
                {
                    mainplot
                }
                else
                {
                    y_axis_mean = mean(na.omit(selected_y_var()[,1]))
                    observe({print(selected_y_var())})
                    print(y_axis_mean)
                    mainplot + geom_hline(yintercept = y_axis_mean, col =  'darkred', size = 0.5)
                }
            }
            else
            {
                mainplot = 
                    ggplot(annotation_data, 
                        aes(x = 1:length(annotation_data$Tracking.number),y = selected_y_var(), fill = Journal)) + 
                        geom_bar(stat = 'identity', width = 0.6) + 
                        xlab('Manuscript') + 
                        ylab(names(selected_y_var())) + 
                        ggtitle(paste(names(selected_y_var()), 'per manuscript')) +
                        theme_bw() + 
                        theme(panel.border = element_blank(), 
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        axis.line = element_line(colour = "black"),
                        legend.position = 'top'
                          )
                if (input$displayMean == FALSE)
                {
                    mainplot
                }
                else
                {
                    y_axis_mean = mean(na.omit(selected_y_var()[,1]))
                    observe({print(selected_y_var())})
                    print(y_axis_mean)
                    mainplot + geom_hline(yintercept = y_axis_mean, col =  'darkred', size = 0.5)
                }
            }
        }
                                         )
    
    output$journal_vs_variable = renderPlot(
        {
            
        }
                                           )
}


shinyApp(ui = ui, server = server)






