FUNCTION Z_GRC_NEW_NFE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_INFO) TYPE  ZDE_INTEGRACAO_HTTP_CONFIG
*"  EXPORTING
*"     REFERENCE(E_PROTOCOLO) TYPE  STRING
*"  RAISING
*"      ZCX_INTEGRACAO
*"      ZCX_ERROR
*"----------------------------------------------------------------------


  ZCL_INTEGRACAO_GRC_NEW_NFE=>ZIF_INTEGRACAO_GRC_NEW_NFE~GET_INSTANCE(
     )->SET_DS_DATA( I_INFO = I_INFO
     )->SET_SEND_MSG( IMPORTING E_PROTOCOLO = E_PROTOCOLO
     ).

ENDFUNCTION.
