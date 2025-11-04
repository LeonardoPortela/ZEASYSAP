FUNCTION Z_GRC_MDFE_AVULSA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"----------------------------------------------------------------------

  CK_EDITANDO = ABAP_FALSE.

  CALL FUNCTION 'Z_GRC_MDFE_LOAD'
    EXPORTING
      I_DOCNUM = I_DOCNUM.

  CALL SCREEN 1000 STARTING AT 50 01.

ENDFUNCTION.
