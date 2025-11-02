FUNCTION Z_GRC_UPDATE_INBOUND_EVENTOS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_XML) TYPE  STRING
*"     VALUE(I_DOCTYPE) TYPE  J_1B_NFE_DOCTYPE
*"     VALUE(I_CD_ST_SEFAZ) TYPE  J_1BSTATUSCODE
*"     VALUE(I_FORCE_UPD_DIST) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FORCE_UPD_ZIB) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_XML_COM_ERRO) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_MSG_ERRO) TYPE  CHAR255 OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_element_array TYPE zde_element_array_t,
        lv_nfe           TYPE j_1bnfdoc-nfenum,
        lv_serie         TYPE j_1bnfdoc-series,
        v_status         TYPE zib_nfe_forn-st_nota,
        v_cod_sefaz      TYPE j_1bstatuscode.


  CLEAR: gs_xml_sefaz, gs_xml_sefaz_cte, lt_element_array[].

  SET LOCALE LANGUAGE 'P'.

*  v_cod_sefaz = i_cd_st_sefaz.

*  IF i_xml_com_erro EQ abap_false.
*    CASE v_cod_sefaz.
*      WHEN '100' OR '150'. "Autorizado o uso
*        v_status  = '1'.
*      WHEN '101' OR '151'. "Cancelamento
*        v_status  = '2'.
*      WHEN OTHERS.
*        EXIT.
*    ENDCASE.
*  ENDIF.

  IF i_doctype EQ 'NFE'.

    APPEND 'itensAverbados' TO lt_element_array.
    APPEND 'Transforms' TO lt_element_array.

    DATA(_json) = zcl_string=>xml_to_json( i_xml           =  i_xml
                                           i_element_array =  lt_element_array ).

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = _json
      CHANGING
        data = gs_xml_averb.

    CREATE OBJECT lo_xml.

    IF lo_xml->parse_string( i_xml ) = 0.
      lo_node = lo_xml->find_node( 'detEvento' ).
      IF lo_node IS BOUND.
        lo_elem ?= lo_node.
        IF lo_elem IS BOUND.
          lv_versao = lo_elem->get_attribute( 'versao' ).
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK gs_xml_averb-proceventonfe-retevento-infevento-chnfe IS NOT INITIAL.

*--------------------------------------------------------------------------------------------------------------*
*   Gravação dos Dados Analisticos do XML nas tabelas de distribuição
*--------------------------------------------------------------------------------------------------------------*

    SELECT SINGLE *
      FROM zib_nfe_dist_avb INTO gs_zib_nfe_dist_avb
     WHERE chave_nfe = gs_xml_averb-proceventonfe-retevento-infevento-chnfe.

    IF ( sy-subrc IS NOT INITIAL OR i_force_upd_zib IS NOT INITIAL ) AND ( gs_xml_averb-proceventonfe-retevento-infevento-tpevento = '790700' ).

      CLEAR: gs_zib_nfe_dist_avi, gs_zib_nfe_dist_avb.
      REFRESH: gt_zib_nfe_dist_avi, gt_zib_nfe_dist_avb.

      PERFORM: zf_atribuir_nfe_dist_avb,
               zf_atribuir_nfe_dist_avi.

      COMMIT WORK AND WAIT.

    ENDIF.

  ENDIF.

ENDFUNCTION.
