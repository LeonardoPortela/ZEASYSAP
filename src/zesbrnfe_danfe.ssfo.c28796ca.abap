
* data PE
IF i_cabecalho-bukrs = '1003'AND
   i_cabecalho-branch = '0008'.
  WRITE i_cabecalho-pstdat TO  g_data_pe.
ENDIF.





















