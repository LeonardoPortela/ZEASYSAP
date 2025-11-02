class ZESZCL_MDFE_ definition
  public
  inheriting from ZESZCL_DOC_ELETRONICO
  final
  create public .

public section.

  methods ZESZIF_DOC_ELETRONICO~GET_URLS_DOCS
    redefinition .
  methods ZESZIF_DOC_ELETRONICO~SET_ENVIAR_CANCELAMENTO
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZESZCL_MDFE_ IMPLEMENTATION.


  METHOD ZESZIF_DOC_ELETRONICO~get_urls_docs.

    DATA: lc_tp_ambiente TYPE ZESZDE_TP_AMBIENTE,
          it_urllist     TYPE tihttpurls2.

    r_instancia = super->ZESZIF_DOC_ELETRONICO~get_urls_docs(
                      IMPORTING
                        e_link_pdf  = e_link_pdf
                        e_link_xml  = e_link_xml ).

    TRY .
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
        SELECT SINGLE contingencia
          INTO @DATA(lv_contingencia)
          FROM ZESZSDT0102
         WHERE docnum = @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

        IF NOT ( sy-subrc = 0 AND lv_contingencia = abap_true ).
          me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
        ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

        CLEAR: e_link_pdf, e_link_xml.

        CALL FUNCTION 'HTTP_GET_URL2'
          EXPORTING
            handlerclass     = 'ZCL_FMCALL_DOC_FISCAL'
          IMPORTING
            urllist          = it_urllist
          EXCEPTIONS
            http_not_enabled = 1
            OTHERS           = 2.

        CHECK sy-subrc IS INITIAL.

        READ TABLE it_urllist WITH KEY protocol = 'http' INTO DATA(wa_urllist).

        zcl_drc_utils=>get_host_port_url_documento( CHANGING  c_http_url = wa_urllist ).

        "http://sapqas.maggi.corp:8001/custom/docfiscal?sap-client=300
        DATA(wa_dominio) = wa_urllist-protocol && '://' && wa_urllist-host && ':' && wa_urllist-port && wa_urllist-url.
        e_link_pdf = wa_dominio && '/getmdfepdf?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
        e_link_xml = wa_dominio && '/getmdfexml?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

      CATCH ZESZCX_DOC_ELETRONICO.    "
    ENDTRY.


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~SET_ENVIAR_CANCELAMENTO.

    DATA: I_JUSTIFICATIVA_CANCEL TYPE CHAR255.

    R_INSTANCIA = ME.

    SUPER->ZESZIF_DOC_ELETRONICO~SET_ENVIAR_CANCELAMENTO(
        I_RESEND = I_RESEND
    ).

*    R_INSTANCIA = ME.
*    I_JUSTIFICATIVA_CANCEL = ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO-REASON1 &&
*             ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO-REASON2 &&
*             ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO-REASON3 &&
*             ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO-REASON4.
*
*    CALL FUNCTION 'Z_J_1B_MDFE_CANCEL'
*      EXPORTING
*        I_DOCNUM               = ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO-DOCNUM
**       I_MODE                 = '1'
*        I_JUSTIFICATIVA_CANCEL = I_JUSTIFICATIVA_CANCEL
**     IMPORTING
**       ZET_BAPIRET2           =
**       ZEV_ERROR_STATUS       =
**       LV_RFCDEST_MDFE        =
*      EXCEPTIONS
*        RFC_ERROR              = 1
*        COMMUNICATION_FAILURE  = 2
*        SYSTEM_FAILURE         = 3
*        OTHERS                 = 4.
*
*    IF SY-SUBRC IS NOT INITIAL.
*
*      RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
*        EXPORTING
*          TEXTID = VALUE #( MSGID = SY-MSGID
*                            MSGNO = SY-MSGNO
*                            ATTR1 = CONV #( SY-MSGV1 )
*                            ATTR2 = CONV #( SY-MSGV2 )
*                            ATTR3 = CONV #( SY-MSGV3 )
*                            ATTR4 = CONV #( SY-MSGV4 ) )
*          MSGID  = SY-MSGID
*          MSGNO  = SY-MSGNO
*          MSGTY  = 'E'
*          MSGV1  = SY-MSGV1
*          MSGV2  = SY-MSGV2
*          MSGV3  = SY-MSGV3
*          MSGV4  = SY-MSGV4.
*
*    ENDIF.
*
*    ME->ZESZIF_DOC_ELETRONICO~SET_REGISTRA_CANCELAMENTO( ).

  ENDMETHOD.
ENDCLASS.
