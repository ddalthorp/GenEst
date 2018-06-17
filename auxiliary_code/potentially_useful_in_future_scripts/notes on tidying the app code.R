stuff that will be useful for cp and such

    selectedCols <- c(input$obsCols_SE, input$sizeclassCol, input$preds_SE)
    selectedData <- selectData(rv$data_SE, selectedCols)
    output$selected_SE <- renderDataTable(selectedData)


    msg_RunModSE <- showNotification(msgSampleSize("SE"),  duration = NULL)
    rv$sizeclasses <- updateSizeclasses(rv$data_SE, rv$sizeclassCol)


consider getting rid of rv$sizeclasses as being used at all in SE or CP



to import from shiny

reactiveValues
observeEvent
updateSelectizeInput
updateTabsetPanel

renderDataTable

showNotification
removeNotification
outputOptions


import from hellno
data.table