FUNCTION Z_GRC_SEND_EMAIL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(PDOCNUM) TYPE  J_1BDOCNUM
*"     VALUE(EMAIL_DESTINO) TYPE  AD_SMTPADR OPTIONAL
*"  EXPORTING
*"     VALUE(CK_ENVIADO) TYPE  CHAR01
*"----------------------------------------------------------------------

  WAIT UP TO 2 SECONDS.

  PERFORM ENVIA_EMAIL IN
   PROGRAM ZESZGRC_SEND_EMAIL
     USING PDOCNUM EMAIL_DESTINO ABAP_TRUE
  CHANGING CK_ENVIADO.

ENDFUNCTION.
