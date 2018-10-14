function(input, output, session){

  rv <- initialReactiveValues()
  output <- initialOutput(rv, output)
  msgs <- msgList()
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  options(DT.options = list(pageLength = 25))

  eventAndReaction("clear_all", rv, input, output, session, msgs = msgs, 
                 clear = TRUE)

  eventAndReaction("file_SE", rv, input, output, session)
  eventAndReaction("file_SE_clear", rv, input, output, session)
  eventAndReaction("file_CP", rv, input, output, session)
  eventAndReaction("file_CP_clear", rv, input, output, session)
  eventAndReaction("file_SS", rv, input, output, session)
  eventAndReaction("file_SS_clear", rv, input, output, session)
  eventAndReaction("file_DWP", rv, input, output, session)
  eventAndReaction("file_DWP_clear", rv, input, output, session)
  eventAndReaction("file_CO", rv, input, output, session)
  eventAndReaction("file_CO_clear", rv, input, output, session)

  eventAndReaction("class", rv, input, output, session, ignoreNull = FALSE)

  eventAndReaction("obsSE", rv, input, output, session, ignoreNull = FALSE)
  eventAndReaction("predsSE", rv, input, output, session, ignoreNull = FALSE)
  #eventAndReaction("run_SE", rv, input, output, session, msgs = msgs)

  observeEvent(input$run_SE,  {
    msgs$ModSE <<- msgModRun(msgs$ModSE, "SE")
    eventReaction("run_SE", rv, input, output, session)
    msgs$ModSE <<- msgModDone(msgs$ModSE, rv, "SE")
  })

  eventAndReaction("run_SE_clear", rv, input, output, session, msgs = msgs,
      clear=TRUE)



  eventAndReaction("outSEclass", rv, input, output, session)
  eventAndReaction("outSEp", rv, input, output, session)
  eventAndReaction("outSEk", rv, input, output, session)

  eventAndReaction("ltp", rv, input, output, session, ignoreNull = FALSE)
  eventAndReaction("fta", rv, input, output, session, ignoreNull = FALSE)
  eventAndReaction("predsCP", rv, input, output, session, ignoreNull = FALSE)

  eventAndReaction("run_CP", rv, input, output, session, msgs = msgs)
  eventAndReaction("run_CP_clear", rv, input, output, session, msgs = msgs,
    clear = TRUE)


#observeEvent(input$runMod_CP, {
#  msgs$ModCP <<- msgModRun(msgs, "CP")
#  rv <- update_rv_run_CP(rv, input)
#  output <- update_output_run_CP(rv, output)
#  update_input_run_CP(rv, session)
#  msgs$ModCP <<- msgModDone(msgs, rv, "CP")
#})
#observeEvent(input$runMod_CP_clear, {
#  clearNotifications(msgs)
#  rv <- update_rv_run_CP_clear(rv, input)
#  output <- update_output_run_CP_clear(rv, output)
#  update_input_run_CP_clear(rv, session)
#})


observeEvent(input$outsizeclassCP, {
  rv <- update_rv_outsc_CP(rv, input)
  output <- update_output_outsc_CP(rv, output)
  update_input_outsc_CP(rv, session)
})
observeEvent(input$outCPdist, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output)
})
observeEvent(input$outCPl, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output)
})
observeEvent(input$outCPs, {
  rv <- update_rv_outdls_CP(rv, input)
  output <- update_output_outdls_CP(rv, output)
})

observeEvent(input$runMod_M, {
  msgs$ModM <<- msgModRun(msgs, "M")
  rv <- update_rv_run_M(rv, input)
  output <- update_output_run_M(rv, output)
  update_input_run_M(rv, session)
  msgs$ModM <<- msgModDone(msgs, rv, "M")
})
observeEvent(input$runMod_M_clear, {
  clearNotifications(msgs)
  rv <- update_rv_run_M_clear(rv, input)
  output <- update_output_run_M_clear(rv, output)
  update_input_run_M_clear(rv, session)
})
observeEvent(input$splitM, {
  rv <- update_rv_split_M(rv, input)
  output <- update_output_split_M(rv, output)
  msgs$ModM <<- msgModDone(msgs, rv, "split")
})
observeEvent(input$splitM_clear, {
  clearNotifications(msgs)
  rv <- update_rv_split_M_clear(rv, input)
  output <- update_output_split_M_clear(rv, output)
  update_input_split_M_clear(rv, session)
})
observeEvent(input$transposeSplit, {
  rv <- update_rv_transpose_split(rv)
  output <- update_output_transpose_split(rv, output)
})

observeEvent(input$useSSdata, {
  rv <- update_rv_useSSdata(rv)
  msgs$SS <<- msgSSavgFail(msgs, rv)
  output <- update_output_SS(rv, output)
  update_input_useSSdata(rv, session)
})
observeEvent(input$useSSinputs, {
  rv <- update_rv_useSSinputs(rv, input)
  msgs$SS <<- msgSSinputFail(msgs, rv)
  output <- update_output_SS(rv, output)
})
observeEvent(input$runMod_g, {
  msgs$Modg <<- msgModRun(msgs, "g")
  rv <- update_rv_run_g(rv, input)
  output <- update_output_run_g(rv, output)
  update_input_run_g(rv, session)
  msgs$Modg <<- msgModDone(msgs, rv, "g")
})
observeEvent(input$runMod_g_clear, {
  clearNotifications(msgs)
  rv <- update_rv_run_g_clear(rv, input)
  output <- update_output_run_g_clear(rv, output)
  update_input_run_g_clear(rv, session)
})
observeEvent(input$outsizeclassg, {
  rv <- update_rv_outsc_g(rv, input)
  output <- update_output_outsc_g(rv, output)
})
}

