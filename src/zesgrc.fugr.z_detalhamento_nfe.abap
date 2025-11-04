FUNCTION Z_DETALHAMENTO_NFE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE_NFE) TYPE  ZDE_CHAVE_DOC_E
*"  EXPORTING
*"     REFERENCE(E_XML_NFE) TYPE  ZNFE_XML_SEFAZ_AUTH
*"  EXCEPTIONS
*"      NO_FOUND
*"----------------------------------------------------------------------

  FREE: l_xml_doc,
        l_xstring_out,
        t_xml_sefaz,
        t_element_array,
        l_ativa_ficha.

  l_chave_nfe = i_chave_nfe.

*-------------------------------------
* Descricoes
*-------------------------------------
  SELECT *
    FROM zsdt0285
    INTO TABLE t_0285.

*-------------------------------------
* posiciona tabstrip
*-------------------------------------
  g_znfe-pressed_tab        = 'ZNFE_FC4'.
  g_tc_prod_det-pressed_tab = 'TC_PROD_DET_FC1'.

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

*-------------------------------------
* estrutura XML
*-------------------------------------
  APPEND 'det'       TO t_element_array.
  APPEND 'detPag'    TO t_element_array.
  APPEND 'NFref'     TO t_element_array.
  APPEND 'DI'        TO t_element_array.
  APPEND 'adi'       TO t_element_array.
  APPEND 'detExport' TO t_element_array.
  APPEND 'med'       TO t_element_array.
  APPEND 'arma'      TO t_element_array.
  APPEND 'comb'      TO t_element_array.
  APPEND 'lacres'    TO t_element_array.
  APPEND 'dup'       TO t_element_array.
  APPEND 'pag'       TO t_element_array.
  APPEND 'procRef'   TO t_element_array.
  APPEND 'obsCont'   TO t_element_array.
  APPEND 'obsFisco'  TO t_element_array.
  APPEND 'vol'       TO t_element_array.

* PBI - 65246 - CBRAND - INICIO
  APPEND 'NFref'      TO t_element_array.
  APPEND 'RefNFe'     TO t_element_array.

  APPEND 'cUF'        TO t_element_array.
  APPEND 'AAMM'       TO t_element_array.
  APPEND 'CNPJ'       TO t_element_array.
  APPEND 'mod'        TO t_element_array.
  APPEND 'serie'      TO t_element_array.
  APPEND 'nNF'        TO t_element_array.
  APPEND 'refNFP'     TO t_element_array.


* PBI - 65246 - CBRAND - FFIM

*------------------------------------------
* elementos XML
*------------------------------------------
  DATA(_json) = zcl_string=>xml_to_json( i_xml           =  l_xml_doc
                                         i_element_array =  t_element_array ).

*------------------------------------------
* descerializacao XML
*------------------------------------------
  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = t_xml_sefaz.

  IF t_xml_sefaz IS INITIAL.
    RAISE no_found.
  else.
    E_XML_NFE = t_xml_sefaz.
  ENDIF.

*------------------------------------------
* Atribuicao valores campos as tabelas internas
*------------------------------------------
  PERFORM f_atribuir_campos USING l_chave_nfe.

ENDFUNCTION.
