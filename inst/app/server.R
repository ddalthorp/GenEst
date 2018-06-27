function(input, output, session){

#modalWelcome()
rv <- createReactiveValues()
output$versionInfo <- renderText(createvtext())
output$SStext <- renderText(rv$SStext)
msgs <- msgList()

observeEvent(input$file_SE, {
  rv <- update_rv_data_SE(rv, input)
  output <- update_output_data_SE(rv, output)
  update_input_data_SE(rv, session)
})
observeEvent(input$file_CP, {
  rv <- update_rv_data_CP(rv, input)
  output <- update_output_data_CP(rv, output)
  update_input_data_CP(rv, session)
})
observeEvent(input$file_SS, {
  rv <- update_rv_data_SS(rv, input)
  output <- update_output_data_SS(rv, output)
  update_input_data_SS(rv, session)
})
observeEvent(input$file_DWP, {
  rv <- update_rv_data_DWP(rv, input)
  output <- update_output_data_DWP(rv, output)
  update_input_data_DWP(rv, session)
})
observeEvent(input$file_CO, {
  rv <- update_rv_data_CO(rv, input)
  output <- update_output_data_CO(rv, output)
  update_input_data_CO(rv, session)
})

observeEvent(input$sizeclassCol, {
  output <- update_output_sizeclassCol(rv, input, output)
  update_input_sizeclassCol(rv, input, session)
})

observeEvent(input$obsCols_SE, {
  output <- update_output_cols_SE(rv, input, output)
  update_input_cols_SE(rv, input, session, "obsCols")
})
observeEvent(input$preds_SE, {
  output <- update_output_cols_SE(rv, input, output)
  update_input_cols_SE(rv, input, session, "preds")
})
observeEvent(input$runMod_SE, {
  msgs$ModSE <<- msgModRun(msgs, "SE")
  rv <- update_rv_run_SE(rv, input)
  output <- update_output_run_SE(rv, output, session)
  update_input_run_SE(rv, session)
  msgs$ModSE <<- msgModDone(msgs, "SE", rv)
})
observeEvent(input$outsizeclassSE, {
  rv <- update_rv_outsc_SE(rv, input)
  output <- update_output_outsc_SE(rv, output, session)
  update_input_outsc_SE(rv, session)
})
observeEvent(input$outSEp, {
  rv <- update_rv_outpk_SE(rv, input)
  output <- update_output_outpk_SE(rv, output, session)
})
observeEvent(input$outSEk, {
  rv <- update_rv_outpk_SE(rv, input)
  output <- update_output_outpk_SE(rv, output, session)
})

observeEvent(input$ltp, {
  output <- update_output_cols_CP(rv, input, output)
  update_input_cols_CP(rv, input, session, "ltp")
})
observeEvent(input$fta, {
  output <- update_output_cols_CP(rv, input, output)
  update_input_cols_CP(rv, input, session, "fta")
})
observeEvent(input$preds_CP, {
  output <- update_output_cols_CP(rv, input, output)
  update_input_cols_CP(rv, input, session, "preds")
})
observeEvent(input$runMod_CP, {
  msgs$ModCP <<- msgModRun(msgs, "CP")
  rv <- update_rv_run_CP(rv, input)
  output <- update_output_run_CP(rv, output, session)
  update_input_run_CP(rv, session)
  msgs$ModCP <<- msgModDone(msgs, "CP", rv)
})
observeEvent(input$outsizeclassCP, {
  rv <- update_rv_outsc_CP(rv, input)
  output <- update_output_outsc_CP(rv, output, session)
  update_input_outsc_CP(rv, session)
})
observeEvent(input$outCPdist, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output, session)
})
observeEvent(input$outCPl, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output, session)
})
observeEvent(input$outCPs, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output, session)
})

}