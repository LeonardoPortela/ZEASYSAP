class ZESZCL_NFE definition
  public
  inheriting from ZESZCL_DOC_ELETRONICO
  final
  create public .

public section.

  methods ZESZIF_DOC_ELETRONICO~GET_VAL_AUTORIZACAO_MODAL
    redefinition .
  methods ZESZIF_DOC_ELETRONICO~GET_VAL_CANCELAMENTO_MODAL
    redefinition .
  methods ZESZIF_DOC_ELETRONICO~GET_VAL_REINICIALIZAR_MODAL
    redefinition .
  methods ZESZIF_DOC_ELETRONICO~GET_URLS_DOCS
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZESZCL_NFE IMPLEMENTATION.


  METHOD ZESZIF_DOC_ELETRONICO~get_urls_docs.

    DATA: lc_tp_ambiente TYPE ZESZDE_TP_AMBIENTE,
          it_urllist     TYPE tihttpurls2.

    r_instancia = super->ZESZIF_DOC_ELETRONICO~get_urls_docs(
                      IMPORTING
                        e_link_pdf  = e_link_pdf
                        e_link_xml  = e_link_xml ).

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).

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

        IF i_id_cce IS NOT INITIAL.
          e_link_pdf = wa_dominio && '/getccenfepdf?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum && '&i_id_cce=' && i_id_cce.
        ELSE.
          e_link_pdf = wa_dominio && '/getnfepdf?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
          e_link_xml = wa_dominio && '/getnfexml?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
        ENDIF.

      CATCH ZESZCX_DOC_ELETRONICO.    "
    ENDTRY.


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~GET_VAL_AUTORIZACAO_MODAL.

    DATA: WA_ZNFECOMEX     TYPE ZESZNFECOMEX,
          VL_ICMS_PARTILHA TYPE C.

    R_INSTANCIA = SUPER->ZESZIF_DOC_ELETRONICO~GET_VAL_AUTORIZACAO_MODAL(
                      )->GET_CK_VERIFICA_MODAL( I_MODAL = ME->ZESZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE
                      ).

    SELECT SINGLE * INTO @DATA(WA_ITENS)
      FROM J_1BNFLIN
     WHERE DOCNUM EQ @ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM.

    IF ( ( SY-SUBRC IS INITIAL ) AND ( WA_ITENS-CFOP(1) EQ '7' ) ).

      CALL FUNCTION 'Z_SD_INFO_NFE_EXPORTACAO'
        EXPORTING
          P_DOCNUM       = ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM
          P_TELA         = SPACE
          P_ZNFECOMEX    = WA_ZNFECOMEX
        EXCEPTIONS
          NAO_LOCALIZADO = 1
          OTHERS         = 2.

      IF SY-SUBRC IS NOT INITIAL.
        ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
      ENDIF.

    ENDIF.

    CALL FUNCTION 'ZPLANCOMP_REC_DEV_EXPORTACAO'
      EXPORTING
        P_DOCNUM_REC       = ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM
      EXCEPTIONS
        CANCELADO          = 1
        NOTA_FISCAL_COMPRO = 2
        OTHERS             = 3.

    IF ( SY-SUBRC EQ 1 ) OR ( SY-SUBRC EQ 2 ).
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ENDIF.

    "Verifica se Emissão da NF já foi liberada pelo Depto. Fiscal. """"""""""""""""">
    CLEAR: VL_ICMS_PARTILHA.

    CALL FUNCTION 'ZCALC_ICMS_VENDA_INTERESTADUAL'
      EXPORTING
        I_DOCNUM    = WA_ITENS-DOCNUM
        I_ITMNUM    = WA_ITENS-ITMNUM
        I_CFOP      = WA_ITENS-CFOP
      IMPORTING
        E_CALCULADO = VL_ICMS_PARTILHA.

    IF VL_ICMS_PARTILHA IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(WL_ZSDT0128)
        FROM ZESZSDT0128
       WHERE DOCNUM = @ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM.

      IF SY-SUBRC NE 0.
        MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 023
           WITH 'Essa NF de Venda Interestadual para não'
                'contribuinte requer aprovação do Departamento'
                'Fiscal. Favor criar uma FI ao indiretos no Soft '
                'Expert solicitando a validação deste documento'.
        ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
      ENDIF.

    ENDIF.
    "Verifica se Emissão da NF já foi liberada pelo Depto. Fiscal. """""""""""""""""<

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~GET_VAL_CANCELAMENTO_MODAL.

    DATA: LC_STATUS TYPE C LENGTH 1,
          LC_CTE    TYPE J_1BNFE_ACTIVE.

    R_INSTANCIA = SUPER->ZESZIF_DOC_ELETRONICO~GET_VAL_CANCELAMENTO_MODAL(
                          )->GET_CK_VERIFICA_MODAL( I_MODAL = ME->ZESZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE
                          ).

    "Verifica CT-e Vinculado """"""""""""""""""""""""""""""""""""""""""""""""""""""">
    CALL FUNCTION 'Z_SD_CTE_DA_NFE'
      EXPORTING
        P_DOCNUM  = ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM
      CHANGING
        MR_STATUS = LC_STATUS
        MR_CTE    = LC_CTE.

    IF LC_STATUS EQ 'M'.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 005 WITH ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ELSEIF LC_STATUS EQ 'E'.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 006 WITH LC_CTE-DOCNUM.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ELSEIF LC_STATUS EQ 'N'.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 007 WITH LC_CTE-DOCNUM.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ELSEIF LC_STATUS EQ 'D'.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 008 WITH LC_CTE-DOCNUM.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ELSEIF LC_STATUS EQ 'F'.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 009 WITH LC_CTE-DOCNUM.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ENDIF.
    "Verifica CT-e Vinculado """""""""""""""""""""""""""""""""""""""""""""""""""""""<

    "Verificar se Remessa formação Lote já foi vinculada à um retorno.
    SELECT SINGLE * INTO @DATA(WL_ZSDT_RETLOTE)
      FROM ZESZSDT_RETLOTE
     WHERE DOCNUM EQ @ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM.

    IF SY-SUBRC IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 039 WITH WL_ZSDT_RETLOTE-DOCNUM_RET.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ENDIF.

    "Verificar se Retorno formação Lote já foi vinculada à O.V Exportação.
    SELECT SINGLE * INTO @DATA(WL_ZSDT_EXPORT)
      FROM ZESZSDT_EXPORT
     WHERE DOCNUM EQ @ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM
       AND ORDEM  NE @SPACE.

    IF SY-SUBRC IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER 040 WITH WL_ZSDT_EXPORT-ORDEM.
      ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~GET_VAL_REINICIALIZAR_MODAL.

    R_INSTANCIA = SUPER->ZESZIF_DOC_ELETRONICO~GET_VAL_REINICIALIZAR_MODAL( ).

  ENDMETHOD.
ENDCLASS.
