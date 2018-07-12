library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(chron)

setwd('/Users/benito/Documents/SourceData/Shiny')

annotation_data <- read.delim('Live_Manuscripts.txt', stringsAsFactors = FALSE)
annotation_data = annotation_data[1:186, -c(19:28) ]
annotation_data$Journal = as.factor(annotation_data$Journal)


ui = fluidPage(
    titlePanel('Visualization of Manu\'s annotation times'),
    HTML('test text'),
    sidebarLayout(
        sidebarPanel(
            checkboxInput(
                inputId = 'colorByJournal',
                label = 'Check to color by Journal',
                value = FALSE
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
            plotOutput(outputId = 'ms_vs_time')
                 )
                 )
              )


convert_times = function(input_time)
{
    60 * 24 * as.numeric(times(input_time))
}



server = function(input, output)
{
    # selected = reactive(
    #     {
    #         annotation_data %>% filter(Journal == input$journal_selection)
    #     }
    #                    )

    output$ms_vs_time = renderPlot(
        {
            timeInMins = convert_times(annotation_data$Time.required)
            averageAnnotationTime = mean(na.omit(timeInMins))
            
            if (input$colorByJournal == FALSE)
            {
                ggplot(annotation_data, 
                    aes(x = 1:length(annotation_data$Tracking.number),y = timeInMins)) + 
                    geom_bar(stat = 'identity', fill = '#1a3f7a', width = 0.5) + 
                    xlab('Manuscript') + 
                    ylab('Time required for annotation (min)') + 
                    ggtitle('Time per manuscript (min)') +
                    geom_hline(yintercept = averageAnnotationTime, col =  'darkred', size = 0.5) +
                    theme_bw() + 
                    theme(panel.border = element_blank(), 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")
                    )
            }
            else 
            {
                ggplot(annotation_data, 
                    aes(x = 1:length(annotation_data$Tracking.number),y = timeInMins), fill = Journal) + 
                    geom_bar(stat = 'identity', width = 0.5) + 
                    xlab('Manuscript') + 
                    ylab('Time required for annotation (min)') + 
                    ggtitle('Time per manuscript (min)') +
                    geom_hline(yintercept = averageAnnotationTime, col =  'darkred', size = 0.5) +
                    theme_bw() + 
                    theme(panel.border = element_blank(), 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")
                    )
            }
        }
                                         )
    
}


shinyApp(ui = ui, server = server)


