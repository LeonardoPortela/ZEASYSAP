FUNCTION Z_GRC_ENVIA_LEGADO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"----------------------------------------------------------------------

  SUBMIT Z_ENVIA_XML_LEGADO
    WITH PDOCSEND EQ I_DOCNUM
     AND RETURN.

ENDFUNCTION.
