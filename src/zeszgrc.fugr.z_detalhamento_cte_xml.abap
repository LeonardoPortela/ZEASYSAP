FUNCTION z_detalhamento_cte_xml.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE_NFE) TYPE  ZESZDE_CHAVE_DOC_E
*"  EXPORTING
*"     REFERENCE(E_XML_CTE) TYPE  ZESZCTE_XML_SEFAZ_AUTH_AJ
*"----------------------------------------------------------------------

  DATA: l_chave_nfe      TYPE string,
        l_xstring_out    TYPE xstring,
        l_xml_doc        TYPE string,
        lt_element_array TYPE zde_element_array_t.

  FREE: l_xml_doc,
          l_xstring_out,
          t_xml_sefaz,
          t_element_array,
          l_ativa_ficha.

  l_chave_nfe = i_chave_nfe.

*-------------------------------------
* recuperar xml
*-------------------------------------
  CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
    EXPORTING
      i_chave = l_chave_nfe
      i_tipo  = 'XML'
    IMPORTING
      out     = l_xstring_out.

*-------------------------------------
* convert xtring to string
*-------------------------------------
  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring  = l_xstring_out
      im_encoding = '1100'
    IMPORTING
      ex_string   = l_xml_doc.

  APPEND 'NFref' TO lt_element_array.
  APPEND 'obsCont' TO lt_element_array.
  APPEND 'infNFe' TO lt_element_array.
  APPEND 'infNF' TO lt_element_array.
  APPEND 'Balsa' TO lt_element_array.
  APPEND 'Veic' TO lt_element_array.
  APPEND 'Comp' TO lt_element_array.
  APPEND 'infQ' TO lt_element_array.
  APPEND 'idDocAnt' TO lt_element_array.
  APPEND 'idDocAntEle' TO lt_element_array.

  DATA(_json) = zcl_string=>xml_to_json( i_xml           =  l_xml_doc
                                         i_element_array =  lt_element_array ).

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = e_xml_cte.

  IF e_xml_cte IS INITIAL.
    RAISE no_found.
  ENDIF.

ENDFUNCTION.
