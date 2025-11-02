FUNCTION ZCTE_DIST_FATURAR_INFO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_INDEX_ULTIMO) TYPE  I
*"  EXPORTING
*"     REFERENCE(E_COMANDO) TYPE  CHAR01
*"  CHANGING
*"     REFERENCE(I_ZIB_CTE) TYPE REF TO  ZCL_CTE_PAGAMENTO
*"     REFERENCE(I_INDEX) TYPE  I
*"  EXCEPTIONS
*"      BLOQUEADO_USUARIO
*"----------------------------------------------------------------------

  PERFORM YCTE_FATURAR_INFO USING I_ZIB_CTE I_INDEX_ULTIMO CHANGING I_INDEX E_COMANDO.

ENDFUNCTION.
