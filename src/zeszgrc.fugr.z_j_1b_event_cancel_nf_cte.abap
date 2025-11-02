function z_j_1b_event_cancel_nf_cte.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CHNFE) TYPE  ZESZDE_CHAVE_NFE
*"     VALUE(I_TPEVENTO) TYPE  ZESZXNFE_EV_TPEVENTO
*"     VALUE(I_CSTAT) TYPE  ZESZXNFE_STATUSCODE
*"----------------------------------------------------------------------


  data: wa_cancel type zib_cancel_grc.

  wa_cancel-chnfe         = i_chnfe.
  wa_cancel-tpevento      = i_tpevento.
  wa_cancel-dt_registro   = sy-datum.
  wa_cancel-hr_registro   = sy-uzeit.
  wa_cancel-us_registro   = sy-uname.
  wa_cancel-rg_atualizado = '0'.
  wa_cancel-cstat         = i_cstat.

  modify zib_cancel_grc from wa_cancel.
  commit work and wait.

endfunction.
