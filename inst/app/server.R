function(input, output, session){

  rv <- initialReactiveValues()
  output <- initialOutput(rv, output)
  msgs <- msgList()
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
  options(DT.options = list(pageLength = 25))

  observeEvent(input$clear_all,  eval(reaction("clear_all")))

  observeEvent(input$file_SE, eval(reaction("file_SE")))
  observeEvent(input$file_SE_clear, eval(reaction("file_SE_clear")))
  observeEvent(input$file_CP, eval(reaction("file_CP")))
  observeEvent(input$file_CP_clear, eval(reaction("file_CP_clear")))
  observeEvent(input$file_SS, eval(reaction("file_SS")))
  observeEvent(input$file_SS_clear, eval(reaction("file_SS_clear")))
  observeEvent(input$file_DWP, eval(reaction("file_DWP")))
  observeEvent(input$file_DWP_clear, eval(reaction("file_DWP_clear")))
  observeEvent(input$file_CO, eval(reaction("file_CO")))
  observeEvent(input$file_CO_clear, eval(reaction("file_CO_clear")))

  observeEvent(input$class, eval(reaction("class")), ignoreNULL = FALSE)

  observeEvent(input$obsSE, eval(reaction("obsSE")), ignoreNULL = FALSE)
  observeEvent(input$predsSE, eval(reaction("predsSE")), ignoreNULL = FALSE)
  observeEvent(input$run_SE, eval(reaction("run_SE")))
  observeEvent(input$run_SE_clear, eval(reaction("run_SE_clear")))

  observeEvent(input$outSEclass, eval(reaction("outSEclass")))
  observeEvent(input$outSEp, eval(reaction("outSEp")))
  observeEvent(input$outSEk, eval(reaction("outSEk")))

  observeEvent(input$ltp, eval(reaction("ltp")), ignoreNULL = FALSE)
  observeEvent(input$fta, eval(reaction("fta")), ignoreNULL = FALSE)
  observeEvent(input$predsCP, eval(reaction("predsCP")), ignoreNULL = FALSE)

  observeEvent(input$run_CP, eval(reaction("run_CP")))
  observeEvent(input$run_CP_clear, eval(reaction("run_CP_clear")))

  observeEvent(input$outCPclass, eval(reaction("outCPclass")))
  observeEvent(input$outCPdist, eval(reaction("outCPdist")))
  observeEvent(input$outCPl, eval(reaction("outCPl")))
  observeEvent(input$outCPs, eval(reaction("outCPs")))


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

