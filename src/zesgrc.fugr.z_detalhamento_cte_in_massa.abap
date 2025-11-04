FUNCTION z_detalhamento_cte_in_massa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CHAVE_NFE) TYPE  ZCTMM_CHAVE_NFE
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"     VALUE(I_BRANCH) TYPE  J_1BBRANC_
*"     VALUE(I_MODEL) TYPE  J_1BMODEL
*"  EXPORTING
*"     VALUE(E_XML_CTE) TYPE  ZCTMM_XML_CTE
*"----------------------------------------------------------------------

  DATA: t_xstring       TYPE TABLE OF zde_s_xstring,
        lv_evento       TYPE n LENGTH 6,
        lv_destination  TYPE syhost,
        lv_rfcdest      TYPE rfcdest,
        lv_xnfeactive   TYPE j_1bxnfeactive,
        wl_xml_cte      TYPE zcte_xml_sefaz_auth,
        wl_zde_xml_cte  TYPE zde_xml_cte,
        t_zctmm_xml_cte TYPE zctmm_xml_cte.

  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      i_bukrs      = '0001'
      i_branch     = '0103'
      i_model      = '57'
    IMPORTING
      e_rfcdest    = lv_rfcdest
      e_xnfeactive = lv_xnfeactive
    EXCEPTIONS
      rfc_error    = 1
      OTHERS       = 2.

  IF sy-subrc IS INITIAL.
    lv_destination = lv_rfcdest.
  ENDIF.

  CHECK lv_destination IS NOT INITIAL.

  CALL FUNCTION 'ZSD_READ_XML_DOC_ELETRONICO' DESTINATION lv_destination
    EXPORTING
      i_nfeid         = i_chave_nfe
      i_evento        = lv_evento
    IMPORTING
      e_xmlstring     = t_xstring
    EXCEPTIONS
      erro_nenhum_xml = 1
      OTHERS          = 2.


  DATA: l_chave_nfe      TYPE string,
        l_xstring_out    TYPE xstring,
        l_xml_doc        TYPE string,
        lt_element_array TYPE zde_element_array_t.

  LOOP AT t_xstring INTO DATA(wl_xstring).

    l_xstring_out = wl_xstring-xstring.
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

    DATA(_json) = zcl_string=>xml_to_json( i_xml           =  l_xml_doc
                                     i_element_array =  lt_element_array ).

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = _json
      CHANGING
        data = wl_xml_cte.

    wl_zde_xml_cte-xml_sefaz_auth = wl_xml_cte.
    wl_zde_xml_cte-nfeid = wl_xstring-nfeid.
    APPEND wl_zde_xml_cte TO t_zctmm_xml_cte.
    CLEAR: wl_xml_cte,wl_zde_xml_cte.

  ENDLOOP.

  e_xml_cte = t_zctmm_xml_cte.

ENDFUNCTION.
