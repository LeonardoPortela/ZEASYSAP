FUNCTION z_grc_monta_link.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_CHAVE) TYPE  ZDE_CHAVE_DOC_E OPTIONAL
*"     REFERENCE(I_ID_CCE) TYPE  NUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_LINK_PDF) TYPE  STRING
*"     REFERENCE(E_LINK_XML) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: lc_tp_ambiente TYPE zde_tp_ambiente,
        it_urllist     TYPE tihttpurls2.

  CLEAR: e_link_pdf,
         e_link_xml.

  IF i_docnum IS NOT INITIAL.

    zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = i_docnum
       )->set_registro( EXPORTING i_docnum = i_docnum i_sem_bloqueio = abap_true
       )->get_urls_docs( EXPORTING i_id_cce = i_id_cce
                         IMPORTING
                           e_link_pdf  = e_link_pdf
                           e_link_xml  = e_link_xml
       ).

  ELSEIF i_chave IS NOT INITIAL.

    CHECK strlen( i_chave ) EQ 44.

    CALL FUNCTION 'HTTP_GET_URL2'
      EXPORTING
        handlerclass     = 'ZCL_FMCALL_DOC_FISCAL'
      IMPORTING
        urllist          = it_urllist
      EXCEPTIONS
        http_not_enabled = 1
        OTHERS           = 2.

    CHECK sy-subrc IS INITIAL.
    READ TABLE it_urllist INDEX 1 INTO DATA(wa_urllist).


    CASE sy-sysid.
      WHEN 'PRD'.
        wa_urllist-host = 's4hanaprd.sap.aroeira.corp'.
        wa_urllist-port = '8080'.
      WHEN 'QAS'.
        wa_urllist-host = 'sapqas.aroeira.corp'.
        wa_urllist-port = '8000'.
      WHEN 'DEV'.
        wa_urllist-host = 'sapdev.aroeira.corp'.
        wa_urllist-port = '8000'.
    ENDCASE.

    zcl_drc_utils=>get_host_port_url_documento( CHANGING  c_http_url = wa_urllist ).

    CHECK sy-subrc IS INITIAL.

    DATA(sap_client) = 'sap-client=' && sy-mandt.
    DATA(wa_dominio) = wa_urllist-protocol && '://' && wa_urllist-host && ':' && wa_urllist-port && wa_urllist-url.

    CASE i_chave+20(2).
      WHEN '55'.

        SELECT SINGLE *
          FROM zib_nfe_dist_ter INTO @DATA(wl_nfe_dist_ter)
         WHERE chave_nfe EQ @i_chave
           AND cancel    EQ @abap_false.

        CHECK sy-subrc EQ 0.

        e_link_pdf = wa_dominio && '/getnfepdf?' && 'sap-client=' && sy-mandt && '&i_chave=' && i_chave.
        e_link_xml = wa_dominio && '/getnfexml?' && 'sap-client=' && sy-mandt && '&i_chave=' && i_chave.

      WHEN '57'.

        SELECT SINGLE *
          FROM zib_cte_dist_ter INTO @DATA(wl_cte_dist_ter)
         WHERE cd_chave_cte EQ @i_chave
           AND cancel       EQ @abap_false.

        CHECK sy-subrc EQ 0.

        e_link_pdf = wa_dominio && '/getctepdf?' && 'sap-client=' && sy-mandt && '&i_chave=' && i_chave.
        e_link_xml = wa_dominio && '/getctexml?' && 'sap-client=' && sy-mandt && '&i_chave=' && i_chave.

    ENDCASE.

  ENDIF.

ENDFUNCTION.
