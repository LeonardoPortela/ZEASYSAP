FUNCTION z_grc_update_inbound.
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

  v_cod_sefaz = i_cd_st_sefaz.

  IF i_xml_com_erro EQ abap_false.
    CASE v_cod_sefaz.
      WHEN '100' OR '150'. "Autorizado o uso
        v_status  = '1'.
      WHEN '101' OR '151'. "Cancelamento
        v_status  = '2'.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.

  IF i_doctype EQ 'NFE'.

    APPEND 'det' TO lt_element_array.
    APPEND 'detPag' TO lt_element_array.
    APPEND 'NFref' TO lt_element_array.
    APPEND 'DI' TO lt_element_array.
    APPEND 'adi' TO lt_element_array.
    APPEND 'detExport' TO lt_element_array.
    APPEND 'med' TO lt_element_array.
    APPEND 'arma' TO lt_element_array.
    APPEND 'comb' TO lt_element_array.
    APPEND 'vol' TO lt_element_array.
    APPEND 'lacres' TO lt_element_array.
    APPEND 'dup' TO lt_element_array.
    APPEND 'pag' TO lt_element_array.
    APPEND 'procRef' TO lt_element_array.
    APPEND 'obsCont' TO lt_element_array.
    APPEND 'obsFisco' TO lt_element_array.

    DATA(_json) = zcl_string=>xml_to_json( i_xml           =  i_xml
                                           i_element_array =  lt_element_array ).

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = _json
      CHANGING
        data = gs_xml_sefaz.

    CHECK gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe IS NOT INITIAL.

    gs_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+25(09).
    gs_xml_sefaz-nfeproc-nfe-infnfe-ide-serie = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+22(03).

*--------------------------------------------------------------------------------------------------------------*
*   Gravação Dados XML com Erro
*--------------------------------------------------------------------------------------------------------------*
    IF i_xml_com_erro EQ abap_true.
      PERFORM f_atrib_nfe_erro USING i_msg_erro.
      EXIT.
    ENDIF.

*--------------------------------------------------------------------------------------------------------------*
*   Gravação dos Dados Sintetico do XML na tabela ZIB_NFE_FORN
*--------------------------------------------------------------------------------------------------------------*

    SELECT SINGLE *
      INTO gs_zib_nfe_forn
      FROM zib_nfe_forn
     WHERE nu_chave = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe
       AND st_nota  = v_status.

    IF ( sy-subrc IS NOT INITIAL ) OR ( i_force_upd_zib EQ abap_true ).

      IF i_force_upd_zib EQ abap_false.
        CLEAR: gs_zib_nfe_forn.
      ENDIF.

      PERFORM zf_atribuir_nfe_forn USING v_status v_cod_sefaz.
    ENDIF.

*--------------------------------------------------------------------------------------------------------------*
*   Gravação dos Dados Analisticos do XML nas tabelas de distribuição
*--------------------------------------------------------------------------------------------------------------*

    SELECT SINGLE *
      FROM ZESZIB_NFE_DIST_TER INTO gs_zib_nfe_dist_ter
     WHERE chave_nfe = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.

    IF ( sy-subrc IS NOT INITIAL ) OR ( i_force_upd_dist EQ abap_true ).

      IF i_force_upd_dist EQ abap_false.
        CLEAR: gs_zib_nfe_dist_ter.
      ENDIF.

      CLEAR: gs_zib_nfe_dist_itm.
      REFRESH: gt_zib_nfe_dist_itm.

      PERFORM zf_atribuir_nfe_dist_ter USING v_status
                                             v_cod_sefaz.
      PERFORM zf_atribuir_nfe_dist_itm.
      PERFORM zf_atribuir_nfe_dist_tvo.
      PERFORM zf_atribuir_nfe_dist_ref.

      IF v_status = '2'. "Recebimento de XML de cancelamento
        PERFORM zf_bloq_nfe_cancel.
      ENDIF.
      COMMIT WORK AND WAIT.
    ELSE.
      PERFORM zf_atrib_st_sefaz_zib_nfe USING v_status v_cod_sefaz
                                     CHANGING gs_zib_nfe_dist_ter.

      gs_zib_nfe_dist_ter-dt_atualizacao = sy-datum.
      MODIFY ZESZIB_NFE_DIST_TER FROM gs_zib_nfe_dist_ter.

      COMMIT WORK AND WAIT.
    ENDIF.

    "Enviar Para Legado """""""""""""""""""""""""""""""""""""""
    DATA: lc_autorizacao TYPE zib_autorit_grc.

    CLEAR: lc_autorizacao.
    lc_autorizacao-chnfe         = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
    lc_autorizacao-dt_registro   = sy-datum.
    lc_autorizacao-hr_registro   = sy-uzeit.
    lc_autorizacao-us_registro   = sy-uname.
    lc_autorizacao-rg_atualizado = '0'.
    lc_autorizacao-direction     = 'INBD'.
    lc_autorizacao-code          = v_cod_sefaz.
    IF ( v_cod_sefaz EQ '100' ) OR ( v_cod_sefaz EQ '150' ).
      lc_autorizacao-cancel      = abap_false.
    ELSEIF ( v_cod_sefaz EQ '101' ) OR ( v_cod_sefaz EQ '151' ).
      lc_autorizacao-cancel        = abap_true.
    ELSE.
      EXIT.
    ENDIF.

    MODIFY zib_autorit_grc FROM lc_autorizacao.
    COMMIT WORK AND WAIT.

  ELSEIF i_doctype EQ 'CTE'.

    CLEAR: gwa_cte_read.

    APPEND 'NFref' TO lt_element_array.
    APPEND 'obsCont' TO lt_element_array.
    APPEND 'infNFe' TO lt_element_array.
    APPEND 'infNF' TO lt_element_array.
    APPEND 'Balsa' TO lt_element_array.
    APPEND 'Veic' TO lt_element_array.
    APPEND 'Comp' TO lt_element_array.
    APPEND 'infQ' TO lt_element_array.

    _json = zcl_string=>xml_to_json( i_xml           =  i_xml
                                     i_element_array =  lt_element_array ).

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = _json
      CHANGING
        data = gs_xml_sefaz_cte.

    IF gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte IS NOT INITIAL.

      gwa_cte_read-cte_57      = abap_true.
      gwa_cte_read-chave       = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
      gwa_cte_read-ie_emissor  = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-ie.
      gwa_cte_read-versao_xml  = gs_xml_sefaz_cte-cteproc-cte-infcte-a_versao.
      gwa_cte_read-protocolo   = gs_xml_sefaz_cte-cteproc-protcte-infprot-nprot.

      PERFORM zf_get_data_utc USING gs_xml_sefaz_cte-cteproc-protcte-infprot-dhrecbto
                           CHANGING gwa_cte_read-dt_protocolo.

      PERFORM zf_get_hora_utc USING gs_xml_sefaz_cte-cteproc-protcte-infprot-dhrecbto
                           CHANGING gwa_cte_read-hr_protocolo.

      PERFORM zf_get_data_utc USING gs_xml_sefaz_cte-cteproc-cte-infcte-ide-dhemi
                           CHANGING gwa_cte_read-dt_emissao.

    ELSE.

      "Verificar se XML é de um CT-e OS
      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json = _json
        CHANGING
          data = gs_xml_sefaz_cte_os.

      CHECK gs_xml_sefaz_cte_os-cteosproc-protcte-infprot-chcte IS NOT INITIAL.

      gwa_cte_read-cte_67      = abap_true.
      gwa_cte_read-chave       = gs_xml_sefaz_cte_os-cteosproc-protcte-infprot-chcte.
      gwa_cte_read-ie_emissor  = gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-emit-ie.
      gwa_cte_read-versao_xml  = gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-a_versao.
      gwa_cte_read-protocolo   = gs_xml_sefaz_cte_os-cteosproc-protcte-infprot-nprot.

      PERFORM zf_get_data_utc USING gs_xml_sefaz_cte_os-cteosproc-protcte-infprot-dhrecbto
                           CHANGING gwa_cte_read-dt_protocolo.

      PERFORM zf_get_hora_utc USING gs_xml_sefaz_cte_os-cteosproc-protcte-infprot-dhrecbto
                           CHANGING gwa_cte_read-hr_protocolo.

      PERFORM zf_get_data_utc USING gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-ide-dhemi
                           CHANGING gwa_cte_read-dt_emissao.



    ENDIF.

    gs_xml_sefaz_cte-cteproc-cte-infcte-ide-nnf     = gwa_cte_read-chave+25(09).
    gs_xml_sefaz_cte-cteproc-cte-infcte-ide-serie   = gwa_cte_read-chave+22(03).

*--------------------------------------------------------------------------------------------------------------*
*   Gravação Dados XML com Erro
*--------------------------------------------------------------------------------------------------------------*

    IF i_xml_com_erro EQ abap_true.
      PERFORM f_atrib_cte_erro USING i_msg_erro.
      EXIT.
    ENDIF.

*--------------------------------------------------------------------------------------------------------------*
*   Gravação dos Dados Sintetico do XML na tabela ZIB_NFE_FORN
*--------------------------------------------------------------------------------------------------------------*

    SELECT SINGLE *
      INTO gs_zib_nfe_forn
      FROM zib_nfe_forn
     WHERE nu_chave = gwa_cte_read-chave
       AND st_nota  = v_status.

    IF ( sy-subrc IS NOT INITIAL ) OR ( i_force_upd_zib EQ abap_true ).

      IF i_force_upd_zib EQ abap_false.
        CLEAR: gs_zib_nfe_forn.
      ENDIF.

      PERFORM zf_atribuir_nfe_forn_cte USING v_status
                                             v_cod_sefaz.
      COMMIT WORK AND WAIT.
    ENDIF.

*--------------------------------------------------------------------------------------------------------------*
*   Gravação dos Dados Analisticos do XML nas tabelas de distribuição
*--------------------------------------------------------------------------------------------------------------*
    CASE abap_true.
      WHEN gwa_cte_read-cte_57.

        SELECT SINGLE *
          FROM ZESZIB_CTE_DIST_TER INTO gs_zib_cte_dist_ter
         WHERE cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.

        IF ( sy-subrc IS NOT INITIAL ) OR ( i_force_upd_dist EQ abap_true ).

          IF i_force_upd_dist EQ abap_false.
            CLEAR: gs_zib_cte_dist_ter.
          ENDIF.

          CLEAR: gs_zib_cte_dist_ant, gs_zib_cte_dist_c57,
                 gs_zib_cte_dist_cvl, gs_zib_cte_dist_d55, gs_zib_cte_dist_mot,
                 gs_zib_cte_dist_n01, gs_zib_cte_dist_d01, gs_zib_cte_dist_dup,
                 gs_zib_cte_dist_n55, gs_zib_cte_dist_vei, gs_zib_cte_dist_vga,
                 gs_zib_cte_dist_001, gs_zib_cte_dist_cpl.

          REFRESH: gt_zib_cte_dist_c57, gt_zib_cte_dist_cvl, gt_zib_cte_dist_d55,
                   gt_zib_cte_dist_dup, gt_zib_cte_dist_mot, gt_zib_cte_dist_n01,
                   gt_zib_cte_dist_d01, gt_zib_cte_dist_n55, gt_zib_cte_dist_ant,
                   gt_zib_cte_dist_vei, gt_zib_cte_dist_vga, gt_zib_cte_dist_cpl,
                   gt_zib_cte_dist_001.

          IF ( i_force_upd_dist EQ abap_true ) AND ( gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte IS NOT INITIAL ).
            DELETE FROM ZESZIB_CTE_DIST_N55 WHERE cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
            DELETE FROM ZESZIB_CTE_DIST_N01 WHERE cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
          ENDIF.

          PERFORM zf_atribuir_cte_dist_ter USING v_status v_cod_sefaz.
          PERFORM zf_atribuir_cte_dist_c57.
          PERFORM zf_atribuir_cte_dist_cvl.
          PERFORM zf_atribuir_cte_dist_mot.

          IF gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnf[] IS NOT INITIAL.
            PERFORM zf_atribuir_cte_dist_n01.
            PERFORM zf_atribuir_cte_dist_d01.
          ENDIF.

          IF gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnfe[] IS NOT INITIAL.
            PERFORM zf_atribuir_cte_dist_n55.
            PERFORM zf_atribuir_cte_dist_d55.
          ENDIF.

          PERFORM zf_atribuir_cte_dist_vei.
          PERFORM zf_atribuir_cte_dist_vga.
          PERFORM zf_atribuir_cte_dist_cpl.
          PERFORM zf_atribuir_cte_dist_001.

          COMMIT WORK AND WAIT.

        ELSE.

          PERFORM zf_atrib_st_sefaz_zib_cte USING v_status v_cod_sefaz
                                         CHANGING gs_zib_cte_dist_ter.

          gs_zib_cte_dist_ter-dt_atualizacao = sy-datum.
          MODIFY ZESZIB_CTE_DIST_TER FROM gs_zib_cte_dist_ter.

          COMMIT WORK AND WAIT.
        ENDIF.

      WHEN OTHERS.
        EXIT.
    ENDCASE.

  ENDIF.

ENDFUNCTION.
