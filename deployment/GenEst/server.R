## server.R

library(shiny)
library(GenEst)

shinyServer(
  function(input, output, session){

    rv <- createReactiveValues()
    output <- initialOutput(rv, output)
    msgs <- msgList()

    observeEvent(input$clear_all, {
      rv <- update_rv_clear_all(rv, input)
      output <- update_output_clear_all(rv, output)
      update_input_clear_all(rv, session)
    })

    observeEvent(input$file_SE, {
      rv <- update_rv_data_SE(rv, input)
      output <- update_output_data_SE(rv, output)
      update_input_data_SE(rv, session)
    })
    observeEvent(input$file_SE_clear, {
      rv <- update_rv_data_SE_clear(rv, input)
      output <- update_output_data_SE_clear(rv, output)
      update_input_data_SE_clear(rv, session)
    })
    observeEvent(input$file_CP, {
      rv <- update_rv_data_CP(rv, input)
      output <- update_output_data_CP(rv, output)
      update_input_data_CP(rv, session)
    })
    observeEvent(input$file_CP_clear, {
      rv <- update_rv_data_CP_clear(rv, input)
      output <- update_output_data_CP_clear(rv, output)
      update_input_data_CP_clear(rv, session)
    })
    observeEvent(input$file_SS, {
      rv <- update_rv_data_SS(rv, input)
      output <- update_output_data_SS(rv, output)
      update_input_data_SS(rv, session)
    })
    observeEvent(input$file_SS_clear, {
      rv <- update_rv_data_SS_clear(rv, input)
      output <- update_output_data_SS_clear(rv, output)
      update_input_data_SS_clear(rv, session)
    })
    observeEvent(input$file_DWP, {
      rv <- update_rv_data_DWP(rv, input)
      output <- update_output_data_DWP(rv, output)
      update_input_data_DWP(rv, session)
    })
    observeEvent(input$file_DWP_clear, {
      rv <- update_rv_data_DWP_clear(rv, input)
      output <- update_output_data_DWP_clear(rv, output)
      update_input_data_DWP_clear(rv, session)
    })
    observeEvent(input$file_CO, {
      rv <- update_rv_data_CO(rv, input)
      output <- update_output_data_CO(rv, output)
      update_input_data_CO(rv, session)
    })
    observeEvent(input$file_CO_clear, {
      rv <- update_rv_data_CO_clear(rv, input)
      output <- update_output_data_CO_clear(rv, output)
      update_input_data_CO_clear(rv, session)
    })

    observeEvent(input$sizeCol, ignoreNULL = FALSE, {
      rv <- update_rv_sizeCol(rv, input)
      output <- update_output_sizeCol(rv, output)
      update_input_sizeCol(rv, input, session)
    })
    
    observeEvent(input$obsCols_SE, ignoreNULL = FALSE, {
      rv <- update_rv_cols_SE_obs(rv, input)
      output <- update_output_cols_SE(rv, output)
      update_input_cols_SE_obs(rv, session)
    })
    observeEvent(input$preds_SE, ignoreNULL = FALSE, {
      rv <- update_rv_cols_SE_preds(rv, input)
      output <- update_output_cols_SE(rv, output)
      update_input_cols_SE_preds(rv, session)
    })
    observeEvent(input$runMod_SE, {
      msgs$ModSE <<- msgModRun(msgs, "SE")
      rv <- update_rv_run_SE(rv, input)
      output <- update_output_run_SE(rv, output)
      update_input_run_SE(rv, session)
      msgs$ModSE <<- msgModDone(msgs, rv, "SE")
    })
    observeEvent(input$runMod_SE_clear, {
      clearNotifications(msgs)
      rv <- update_rv_run_SE_clear(rv, input)
      output <- update_output_run_SE_clear(rv, output)
      update_input_run_SE_clear(rv, session)  
    })
    observeEvent(input$outsizeclassSE, {
      rv <- update_rv_outsc_SE(rv, input)
      output <- update_output_outsc_SE(rv, output)
      update_input_outsc_SE(rv, session)
    })
    observeEvent(input$outSEp, {
      rv <- update_rv_outpk_SE(rv, input)
      output <- update_output_outpk_SE(rv, output)
    })
    observeEvent(input$outSEk, {
      rv <- update_rv_outpk_SE(rv, input)
      output <- update_output_outpk_SE(rv, output)
    })
    
    observeEvent(input$ltp, ignoreNULL = FALSE, {
      rv <- update_rv_cols_ltp(rv, input)
      output <- update_output_cols_CP(rv, output)
      update_input_cols_ltp(rv, session)
    })
    observeEvent(input$fta, ignoreNULL = FALSE, {
      rv <- update_rv_cols_fta(rv, input)
      output <- update_output_cols_CP(rv, output)
      update_input_cols_fta(rv, session)
    })
    observeEvent(input$preds_CP, ignoreNULL = FALSE, {
      rv <- update_rv_cols_CP_preds(rv, input)
      output <- update_output_cols_CP(rv, output)
      update_input_cols_CP_preds(rv, session)
    })
    observeEvent(input$runMod_CP, {
      msgs$ModCP <<- msgModRun(msgs, "CP")
      rv <- update_rv_run_CP(rv, input)
      output <- update_output_run_CP(rv, output)
      update_input_run_CP(rv, session)
      msgs$ModCP <<- msgModDone(msgs, rv, "CP")
    })    
    observeEvent(input$runMod_CP_clear, {
      clearNotifications(msgs)
      rv <- update_rv_run_CP_clear(rv, input)
      output <- update_output_run_CP_clear(rv, output)
      update_input_run_CP_clear(rv, session)  
    })
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
)