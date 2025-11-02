class ZESZCL_DOC_ELETRONICO definition
  public
  create public .

public section.

  interfaces ZESZIF_DOC_ELETRONICO .

  class-methods GET_ARQ_DOC_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM optional
      !I_CHAVE type ZESZDE_CHAVE_NFE optional
      !I_TIPO type STRING default 'PDF'
      !I_JSON type STRING optional
    exporting
      !E_NAME type STRING
      !E_TIPO type STRING
    returning
      value(R_ARQUIVO) type XSTRING
    raising
      ZESZCX_DOC_ELETRONICO .
  class-methods VALIDACAO_AUTORIZACAO_USO_0001
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods VALIDACAO_AUTORIZACAO_USO
    importing
      !I_DOCNUM type J_1BDOCNUM
    raising
      ZESZCX_DOC_ELETRONICO .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZESZCL_DOC_ELETRONICO IMPLEMENTATION.


  METHOD get_arq_doc_fiscal.

    DATA: chamada TYPE ZESZDE_GET_ARQ_DOC_FISCAL.

    CLEAR: e_name, e_tipo.

    IF i_json IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_json CHANGING data = chamada ).
    ELSE.
      chamada-chave  = i_chave.
      chamada-docnum = i_docnum.
      chamada-tipo   = i_tipo.
    ENDIF.

    e_tipo = i_tipo.

    DATA: lc_docnum TYPE  j_1bdocnum.
    lc_docnum = chamada-docnum.

    CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
      EXPORTING
        i_docnum = lc_docnum
        i_chave  = chamada-chave
        i_tipo   = chamada-tipo
      IMPORTING
        out      = r_arquivo
        e_name   = e_name.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~download_doc_fiscal.

    r_instancia = me.

    IF i_xml EQ abap_true.
      me->ZESZIF_DOC_ELETRONICO~get_xml( IMPORTING e_xml = e_xml ).
    ENDIF.

    IF i_pdf EQ abap_true.
      me->ZESZIF_DOC_ELETRONICO~get_pdf( IMPORTING e_pdf = e_pdf ).
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~GET_AGUARDAR.

    "Aguardar Sair de Processamento
    IF I_CICLOS IS NOT INITIAL.
      ME->ZESZIF_DOC_ELETRONICO~AT_QTD_CICLOS = I_CICLOS.
    ENDIF.

    IF I_SEGUNDOS IS NOT INITIAL.
      ME->ZESZIF_DOC_ELETRONICO~AT_QTD_SEGUNDOS = I_SEGUNDOS.
    ENDIF.

    DATA: LC_CICLOS	TYPE ZESZDE_QTD_CICLOS.

    R_INSTANCIA = ME.

    CHECK I_AGUARDAR EQ ABAP_TRUE.

    DATA(LC_TEXTO) = |Aguardar Autorização do Documento { ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM } modal { ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-MODEL }|.

    IF SY-BATCH NE ABAP_TRUE.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          PERCENTAGE = 0
          TEXT       = LC_TEXTO.
    ELSE.
      MESSAGE S047 WITH ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-MODEL.
    ENDIF.

    WAIT UP TO 1 SECONDS.
*
    SELECT SINGLE * INTO ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO
      FROM J_1BNFE_ACTIVE
     WHERE DOCNUM EQ ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM.

    LC_CICLOS = 1.

    DATA: PERCENTAGE TYPE I.

    WHILE LC_CICLOS LE ME->ZESZIF_DOC_ELETRONICO~AT_QTD_CICLOS AND ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO-ACTION_REQU EQ ABAP_FALSE.

      PERCENTAGE = ( LC_CICLOS / ME->ZESZIF_DOC_ELETRONICO~AT_QTD_CICLOS ) * 100.

      IF PERCENTAGE IS INITIAL.
        PERCENTAGE = 1.
      ENDIF.

      IF SY-BATCH NE ABAP_TRUE.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = PERCENTAGE
            TEXT       = LC_TEXTO.
      ELSE.
        MESSAGE S047 WITH ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-MODEL.
      ENDIF.

      "Tempo de Cadas Ciclo
      WAIT UP TO ME->ZESZIF_DOC_ELETRONICO~AT_QTD_SEGUNDOS SECONDS.

      SELECT SINGLE * INTO ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO
        FROM J_1BNFE_ACTIVE
       WHERE DOCNUM EQ ME->ZESZIF_DOC_ELETRONICO~AT_DOCUMENTO-DOCNUM.

      ADD 1 TO LC_CICLOS.

    ENDWHILE.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_aguardando_aut_uso.

    r_instancia = me.

    CHECK NOT (
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ '3' AND
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat      EQ abap_false ).

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_nao_aguard_aut_uso-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_nao_aguard_aut_uso-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_nao_aguard_aut_uso-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_nao_aguard_aut_uso-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_autorizado_cancel.

    r_instancia = me.

    CHECK NOT (
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta EQ '1' AND
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta EQ '2' ).

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_cancel-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_cancel-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_cancel-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_cancel-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso.

    r_instancia = me.

    CHECK NOT ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta EQ '1' AND
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-cancel EQ abap_false )
              OR
              ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta EQ '1' AND
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta EQ '2' ).

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_uso-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_uso-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_uso-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_nao_autorizado_uso-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_certidao_negativa.

    r_instancia = me.

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
      CATCH ZESZCX_DOC_ELETRONICO.    "

        SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
              FROM j_1bbranch
             WHERE bukrs  EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-bukrs
               AND branch EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-branch.

        CHECK sy-subrc IS INITIAL.

        SELECT SINGLE * INTO @DATA(wa_adrc)
          FROM adrc
         WHERE addrnumber EQ @wa_j_1bbranch-adrnr.

        CHECK sy-subrc IS INITIAL.

        CHECK wa_adrc-country EQ 'BR' AND wa_adrc-region EQ 'MT'.

        SELECT SINGLE * INTO @DATA(wa_zjcnd_branch)
          FROM ZESZJCND_BRANCH
         WHERE bukrs       EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-bukrs
           AND branch      EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-branch
           AND dt_emissao  LE @me->ZESZIF_DOC_ELETRONICO~at_documento-pstdat
           AND dt_validade GE @me->ZESZIF_DOC_ELETRONICO~at_documento-pstdat.

        CHECK sy-subrc IS NOT INITIAL.

        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_sem_certidao_negativa-msgid
                              msgno = ZESZCX_DOC_ELETRONICO=>zcx_sem_certidao_negativa-msgno
                              attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-branch ) )
            msgid  = ZESZCX_DOC_ELETRONICO=>zcx_sem_certidao_negativa-msgid
            msgno  = ZESZCX_DOC_ELETRONICO=>zcx_sem_certidao_negativa-msgno
            msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-branch )
            msgty  = 'E'.

    ENDTRY.


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_data_documento.

    r_instancia = me.

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
      CATCH ZESZCX_DOC_ELETRONICO.    "

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
        SELECT SINGLE *
          FROM ZESZSDT0102 INTO @DATA(wa_zsdt0102)
         WHERE docnum = @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

        IF sy-subrc = 0 AND wa_zsdt0102-contingencia = abap_true.
          RETURN.
        ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

        CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-docdat NE sy-datum.

        CASE me->ZESZIF_DOC_ELETRONICO~at_documento-model.
          WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_mdfe.
            sy-subrc = 1.
          WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_cte.
            sy-subrc = 1.
          WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_nfe.
            AUTHORITY-CHECK OBJECT 'ZSDRETRONF' ID 'Z_DT_RETNF' FIELD '1'.
        ENDCASE.

        CHECK sy-subrc IS NOT INITIAL.

        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_data_emissao_retroativa-msgid
                              msgno = ZESZCX_DOC_ELETRONICO=>zcx_data_emissao_retroativa-msgno
                              attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-branch ) )
            msgid  = ZESZCX_DOC_ELETRONICO=>zcx_data_emissao_retroativa-msgid
            msgno  = ZESZCX_DOC_ELETRONICO=>zcx_data_emissao_retroativa-msgno
            msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-branch )
            msgty  = 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_determinar_numero.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ NE '9'.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_nao_necessario_numero-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_nao_necessario_numero-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_nao_necessario_numero-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_nao_necessario_numero-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_doc_cancel.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta NE '2'.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-cancel EQ abap_false.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_cancelado-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_cancelado-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_cancelado-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_cancelado-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_doc_nao_cancel.

    r_instancia = me.

    SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
      FROM j_1bnfdoc
     WHERE docref EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-cancel           EQ abap_true OR
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-cancel EQ abap_true .

    "Encontrou o documento de estorno
    CHECK wa_j_1bnfdoc-docnum IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancelado-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancelado-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancelado-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancelado-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_doc_proprio.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-form IS INITIAL.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_emissao_propria-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_emissao_propria-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_emissao_propria-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_emissao_propria-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_doc_relacionado.

    r_instancia = me.

    DATA(lo_cte_switch)        = cl_j_1bcte_swf=>get_instance( ).
    DATA(is_cte_ctx_by_model)  = lo_cte_switch->is_cte_ctx_by_model( me->ZESZIF_DOC_ELETRONICO~at_documento-model ).
    DATA(is_cte_ctx_by_docnum) = lo_cte_switch->is_cte_ctx_by_docnum( iv_docnum = me->ZESZIF_DOC_ELETRONICO~at_documento-docref ).
    CLEAR: lo_cte_switch.

    CHECK NOT ( is_cte_ctx_by_model EQ abap_true OR is_cte_ctx_by_docnum EQ abap_true ).

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_relacionado-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_relacionado-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_relacionado-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_nao_relacionado-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_estornar_doc_origem.

    r_instancia = me.

    CHECK NOT ( ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ '1' AND
                  me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta      EQ '2' ) OR
                ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ '1' AND
                  me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta      EQ '3' ) OR
                ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ '8' AND
                  me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat      EQ 'V' )
              ).

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_nao_est_doc_origem-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_nao_est_doc_origem-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_nao_est_doc_origem-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_nao_est_doc_origem-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_estorno.

    r_instancia = me.

    TRY .
        "Documento Já cancelado sai da rotina
        me->ZESZIF_DOC_ELETRONICO~get_ck_doc_cancel( ).
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO.    "
    ENDTRY.

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_numero_determinado( ).
      CATCH ZESZCX_DOC_ELETRONICO.
        "Não foi Determinado número
        EXIT.
    ENDTRY.

    me->ZESZIF_DOC_ELETRONICO~get_ck_estorno_mdfe_possivel(
    )->get_val_cancelamento_modal(
    ).

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_estorno_mdfe_possivel.

    r_instancia = me.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_fatura_ativa.

    r_instancia = me.


    SELECT SINGLE * INTO @DATA(wl_lin)
      FROM j_1bnflin
     WHERE docnum EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

    CHECK ( sy-subrc IS INITIAL ) AND ( wl_lin-refkey IS NOT INITIAL ) AND ( wl_lin-reftyp EQ 'BI' ).

    SELECT SINGLE vbeln INTO @DATA(vl_vbeln)
      FROM vbfa
     WHERE vbtyp_n EQ 'N' " Estorno fatura
       AND vbtyp_v EQ 'M' " Fatura
       AND vbelv   EQ @wl_lin-refkey.

    CHECK sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_fatura_estornada-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_fatura_estornada-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_fatura_estornada-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_fatura_estornada-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_nao_processamento.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ abap_false.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ abap_false OR
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ 'A' OR
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ 'G' OR
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ 'V' OR
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ 'D'.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_uso-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_uso-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_uso-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_uso-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_nao_proc_cancel.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ abap_false AND   "Documento em Processamento
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta      EQ '1' AND          "Documento Autorizado na SEFAZ
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta      EQ '1'.             "Documento Autorizado e Solicitação de Cancelamento Solicitada...

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_can-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_can-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_can-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_can-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_nao_proc_inutilizacao.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ abap_false AND  "Em processamento; nenhuma ação manual necessária

          ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta      EQ '3' OR      "Solicitação de rejeição & autorização para inutilização
            me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta      EQ '9' ) AND   "Erro de validação & inutilização solicitada

          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat      EQ 'C' AND      "Solicitação de não utilização recebida pelo SM
          me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason      IS NOT INITIAL. "Motivo para Estorno/Não utilização

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_inut-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_inut-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_inut-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_em_processamento_aut_inut-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_numero_determinado.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-nfenum IS INITIAL.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_nao_determinado-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_nao_determinado-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_nao_determinado-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_nao_determinado-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_numero_nao_determinado.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-nfenum IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_determinado-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_determinado-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_determinado-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_num_determinado-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_sem_erro_autorizacao.

    r_instancia = me.

    IF ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta      NE '2' ) AND
       ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ NE '8' ).

      CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta NE space.
      CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta NE '1'.
      CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-cancel NE abap_true.

      CHECK ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ '1' AND
              me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta EQ '0' ) OR
            ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta EQ '3' ).

    ENDIF.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_sem_erro_cancelamento.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ 'C'.
    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ 'A'.
    CHECK me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason IS NOT INITIAL.

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_logs_erro( ).
      CATCH ZESZCX_DOC_ELETRONICO.
        EXIT.
    ENDTRY.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_autorizacao_uso-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_valor_documento.

    DATA: lc_netwr TYPE j_1bnflin-netwr,
          wa_lin_i TYPE j_1binlin.

    r_instancia = me.

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
      CATCH ZESZCX_DOC_ELETRONICO.    "

        CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-model EQ ZESZIF_DOC_ELETRONICO=>at_st_model_nfe OR
              me->ZESZIF_DOC_ELETRONICO~at_documento-model EQ ZESZIF_DOC_ELETRONICO=>at_st_model_cte.

        SELECT * INTO TABLE @DATA(it_itens)
          FROM j_1bnflin
         WHERE docnum EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

        CHECK sy-subrc IS INITIAL.

        SELECT * INTO TABLE @DATA(it_tax)
          FROM j_1bnfstx
         WHERE docnum EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

        TRY.
            me->ZESZIF_DOC_ELETRONICO~get_ck_doc_relacionado( ).
            DATA(ck_doc_related) = abap_true.
          CATCH ZESZCX_DOC_ELETRONICO .
            ck_doc_related = abap_false.
        ENDTRY.

        lc_netwr = 0.

        LOOP AT it_itens INTO DATA(wa_item).

          DATA(it_tax_item) = it_tax[].
          DELETE it_tax_item WHERE itmnum NE wa_item-itmnum.

          CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION_I'
            EXPORTING
              nf_item                 = wa_item
              ix_posted_with_xml_data = me->ZESZIF_DOC_ELETRONICO~at_documento-autom_incoming
              ix_cte_related          = ck_doc_related
              iv_nf_direction         = me->ZESZIF_DOC_ELETRONICO~at_documento-direct
            IMPORTING
              ext_item                = wa_lin_i
            TABLES
              nf_item_tax             = it_tax_item.

          IF wa_lin_i-nftot > 0 .
            ADD wa_lin_i-nftot TO lc_netwr.
          ELSEIF wa_lin_i-nfnett > 0 .
            ADD wa_lin_i-nfnett TO lc_netwr.
          ELSEIF wa_lin_i-netwrt > 0.
            ADD wa_lin_i-netwrt TO lc_netwr.
          ENDIF.

        ENDLOOP.

        CASE me->ZESZIF_DOC_ELETRONICO~at_documento-model.
          WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_nfe.
            SELECT SINGLE * INTO @DATA(wl_setleaf)
              FROM setleaf
             WHERE setname = 'MAGGI_VLIM_EMI_NFE'.
          WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_cte.
            SELECT SINGLE * INTO wl_setleaf
              FROM setleaf
             WHERE setname = 'MAGGI_VLIM_EMI_CTE'.
        ENDCASE.

        "Achou parâmetros
        CHECK sy-subrc IS INITIAL.

        "Valor do Parâmetro não é vazio
        CHECK wl_setleaf-valfrom IS NOT INITIAL.

        wa_lin_i-nftot = wl_setleaf-valfrom.

        "Verifica se o Limite é maio que zero
        CHECK wa_lin_i-nftot GT 0.

        "Verificar se Valor é Maio que o Limite
        CHECK lc_netwr GT wa_lin_i-nftot.

        "Verifica Tabela para aprovação Emissão NF-e/CT-e por Valor
        SELECT SINGLE * INTO @DATA(wa_zsdt0146)
          FROM ZESZSDT0146
         WHERE docnum EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

        "Se encontrou aprovação sai da validação
        CHECK sy-subrc IS NOT INITIAL.

        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                              msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                              attr1 = 'Valor do documento ultrapassa o limite permitido '
                              attr2 = 'e necessita de uma aprovação do Departamento '
                              attr3 = 'Fiscal. Favor criar uma FI ao indiretos no Soft'
                              attr4 = 'Expert solicitando a validação deste documento' )
            msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
            msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = 'Valor do documento ultrapassa o limite permitido '
            msgv2  = 'e necessita de uma aprovação do Departamento '
            msgv3  = 'Fiscal. Favor criar uma FI ao indiretos no Soft'
            msgv4  = 'Expert solicitando a validação deste documento'.

    ENDTRY.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_ck_verifica_modal.

    r_instancia = me.

    CHECK i_modal NE me->ZESZIF_DOC_ELETRONICO~at_documento-model.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_modelo-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_modelo-msgno
                          attr1 = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
                          attr2 = CONV #( i_modal ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_modelo-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_modelo-msgno
        msgv1  = CONV #( me->ZESZIF_DOC_ELETRONICO~at_documento-docnum )
        msgv2  = CONV #( i_modal )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_erro_geral.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = sy-msgid
                          msgno = sy-msgno
                          attr1 = CONV #( sy-msgv1 )
                          attr2 = CONV #( sy-msgv2 )
                          attr3 = CONV #( sy-msgv3 )
                          attr4 = CONV #( sy-msgv4 ) )
        msgid  = sy-msgid
        msgno  = sy-msgno
        msgty  = 'E'
        msgv1  = sy-msgv1
        msgv2  = sy-msgv2
        msgv3  = sy-msgv3
        msgv4  = sy-msgv4.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_erro_geral_string.

    DATA: lc_texto TYPE c LENGTH 200.
    lc_texto = i_texto.
    sy-msgv1 = lc_texto+000(50).
    sy-msgv2 = lc_texto+050(50).
    sy-msgv3 = lc_texto+100(50).
    sy-msgv4 = lc_texto+150(50).

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                          attr1 = CONV #( sy-msgv1 )
                          attr2 = CONV #( sy-msgv2 )
                          attr3 = CONV #( sy-msgv3 )
                          attr4 = CONV #( sy-msgv4 ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
        msgty  = 'E'
        msgv1  = sy-msgv1
        msgv2  = sy-msgv2
        msgv3  = sy-msgv3
        msgv4  = sy-msgv4.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_instance.

    IF ZESZIF_DOC_ELETRONICO~at_instance IS NOT BOUND.

      SELECT SINGLE model INTO @DATA(i_model)
        FROM j_1bnfdoc
       WHERE docnum EQ @i_docnum.

      CASE i_model.
        WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_cte.
          CREATE OBJECT ZESZIF_DOC_ELETRONICO~at_instance TYPE ZESZCL_CTE.
        WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_nfe.
          CREATE OBJECT ZESZIF_DOC_ELETRONICO~at_instance TYPE ZESZCL_NFE.
        WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_mdfe.
          CREATE OBJECT ZESZIF_DOC_ELETRONICO~at_instance TYPE ZESZCL_MDFE_.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_modelo_nao_previsto-msgid
                                msgno = ZESZCX_DOC_ELETRONICO=>zcx_modelo_nao_previsto-msgno
                                attr1 = CONV #( i_docnum )
                                attr2 = CONV #( i_model  ) )
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_modelo_nao_previsto-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_modelo_nao_previsto-msgno
              msgty  = 'E'
              msgv1  = CONV #( i_docnum )
              msgv2  = CONV #( i_model ).
      ENDCASE.
      r_instancia = ZESZIF_DOC_ELETRONICO~at_instance.

    ELSE.
      r_instancia = ZESZIF_DOC_ELETRONICO~at_instance.
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_load_log.

    "Objeto de exemplo : CL_CFA_MESSAGE_HANDLER

    DATA: gs_log_filter   TYPE bal_s_lfil,
          gs_extnum       TYPE bal_s_extn,
          it_log_header2  TYPE balhdr_t,
          it_log_header2a TYPE balhdr_t,
          wa_log_header   TYPE balhdr,
          it_log_handle   TYPE bal_t_logh,
          it_msg_handle   TYPE bal_t_msgh,
          ls_msg          TYPE bal_s_msg.

    CLEAR: et_message, et_message[].

    gs_extnum-sign   = 'I'.
    gs_extnum-option = 'EQ'.
    gs_extnum-low = gs_extnum-high = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    APPEND gs_extnum TO gs_log_filter-extnumber.

    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_refresh_all = 'X'.

    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'J_1B_NFE_PROCESSING' .

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = gs_log_filter
      IMPORTING
        e_t_log_header     = it_log_header2
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.

    IF sy-subrc <>  0.
      MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '037'
         WITH me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
      me->ZESZIF_DOC_ELETRONICO~get_erro_geral( ).
    ENDIF.

    CHECK it_log_header2[] IS NOT INITIAL.

    LOOP AT it_log_header2 INTO DATA(wa_log_header2).
      IF wa_log_header IS INITIAL.
        wa_log_header = wa_log_header2.
        CONTINUE.
      ENDIF.
      IF wa_log_header2-lognumber GT wa_log_header-lognumber.
        wa_log_header = wa_log_header2.
      ENDIF.
    ENDLOOP.

    APPEND wa_log_header TO it_log_header2a.

    CLEAR it_log_handle.
    CLEAR it_msg_handle.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = it_log_header2a
        i_client           = sy-mandt
      IMPORTING
        e_t_log_handle     = it_log_handle
        e_t_msg_handle     = it_msg_handle
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
    ENDIF.

    LOOP AT it_msg_handle INTO DATA(wa_msg_handle).
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = wa_msg_handle
        IMPORTING
          e_s_msg        = ls_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.

      CHECK sy-subrc IS INITIAL.
      APPEND ls_msg TO et_message.
    ENDLOOP.

    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_refresh_all = 'X'.

    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_logs_erro.

    DATA: gs_log_filter TYPE bal_s_lfil,
          gs_subobject  TYPE bal_s_sub,
          gs_object     TYPE bal_s_obj,
          gs_extnum     TYPE bal_s_extn,
          c_object      TYPE balobj    VALUE 'NFE',
          c_subobject   TYPE balsubobj VALUE 'MONITOR'.

    r_instancia = me.

    CLEAR gs_log_filter.
    CLEAR e_balhdr.
    gs_subobject-sign   = gs_object-sign    = gs_extnum-sign   = 'I'.
    gs_subobject-option = gs_object-option  = gs_extnum-option = 'EQ'.
    gs_object-low       = gs_object-high    = c_object.
    gs_subobject-low    = gs_subobject-high = c_subobject.

    APPEND gs_object    TO gs_log_filter-object.
    APPEND gs_subobject TO gs_log_filter-subobject.
    gs_extnum-low = gs_extnum-high = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    APPEND gs_extnum TO gs_log_filter-extnumber.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = gs_log_filter
      IMPORTING
        e_t_log_header     = e_balhdr
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '034'.
      me->ZESZIF_DOC_ELETRONICO~get_erro_geral( ).
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_motivo_cancelamento.

    SELECT SINGLE * INTO e_motivo
      FROM j_1bnfe_cancelrt
     WHERE spras  EQ sy-langu
       AND reason EQ i_motivo.

    CHECK sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
      EXPORTING
        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_motivo_cancel_nao_enc-msgid
                          msgno = ZESZCX_DOC_ELETRONICO=>zcx_motivo_cancel_nao_enc-msgno
                          attr1 = CONV #( i_motivo ) )
        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_motivo_cancel_nao_enc-msgid
        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_motivo_cancel_nao_enc-msgno
        msgv1  = CONV #( i_motivo )
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_pdf.

    DATA: it_urllist TYPE tihttpurls2.

    r_instancia = me.

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

*    CASE sy-sysid.
*      WHEN 'PRD'.
*        wa_urllist-host = 'sapprd.maggi.corp'.
*        wa_urllist-port = '8000'.
*      WHEN 'QAS'.
*        wa_urllist-host = 'sapqas.aroeira.corp'.
*        wa_urllist-port = '8000'.
*      WHEN 'DEV'.
*        wa_urllist-host = 'sapdev.aroeira.corp'.
*        wa_urllist-port = '8000'.
*    ENDCASE.

    "http://sapqas.maggi.corp:8001/custom/docfiscal?sap-client=300
    DATA(wa_dominio) = wa_urllist-protocol && '://' && wa_urllist-host && ':' && wa_urllist-port && wa_urllist-url.

    CASE me->ZESZIF_DOC_ELETRONICO~at_documento-model.
      WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_nfe.
        DATA(e_link_pdf) = wa_dominio && '/getnfepdf?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
      WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_cte.
        e_link_pdf = wa_dominio && '/getctepdf?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
      WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_mdfe.
        e_link_pdf = wa_dominio && '/getmdfepdf?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    ENDCASE.

    TRY .
        zcl_arquivo=>get_file_uri_get_(
          EXPORTING
            i_uri          = e_link_pdf
          IMPORTING
            e_texto_2      = e_pdf
            e_code         = DATA(e_code)
            e_reason       = DATA(e_reason)
        ).

        IF e_code NE '200'.

          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                attr1 = e_reason )
              msgty  = 'E'
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
              msgv1  = CONV #( e_reason ).

        ENDIF.

      CATCH zcx_arquivo INTO DATA(ex_arquivo).

        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ex_arquivo->msgid
                              msgno = ex_arquivo->msgno
                              attr1 = ex_arquivo->msgv1
                              attr2 = ex_arquivo->msgv2
                              attr3 = ex_arquivo->msgv3
                              attr4 = ex_arquivo->msgv4 )
            msgty  = 'E'
            msgid  = ex_arquivo->msgid
            msgno  = ex_arquivo->msgno
            msgv1  = ex_arquivo->msgv1
            msgv2  = ex_arquivo->msgv2
            msgv3  = ex_arquivo->msgv3
            msgv4  = ex_arquivo->msgv4.


    ENDTRY.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_registro.
    r_instancia = me.
    e_documento = me->ZESZIF_DOC_ELETRONICO~at_documento.
    e_info_doc_eletronico = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico.
  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_status.

    r_instancia = me.

    "Documento Estornado
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_doc_nao_cancel( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        e_status = '09'.
        e_status_desc = 'Documento Estornado'.
        EXIT.
    ENDTRY.

    "Autorizado o Cancelamento
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_cancel( ).
        e_status = '07'.
        e_status_desc = 'Autorizado o Cancelamento'.
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO .
    ENDTRY.

    "Documento Cancelado
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_doc_nao_cancel( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        e_status = '05'.
        e_status_desc = 'Documento Cancelado'.
        EXIT.
    ENDTRY.

    "Necessário Determinar Número
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_determinar_numero( ).
        e_status = '00'.
        e_status_desc = 'Necessário Determinar Número'.
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO .
    ENDTRY.

    "Em processamento de Autorização de USO
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_nao_processamento( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        e_status = '01'.
        e_status_desc = 'Em processamento de Autorização de USO'.
        EXIT.
    ENDTRY.

    "Em Processamento de Autorização para Cancelar
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_nao_proc_cancel( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        e_status = '06'.
        e_status_desc = 'Em Processamento de Autorização para Cancelar'.
        EXIT.
    ENDTRY.

    "Autorizado o USO
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
        e_status = '02'.
        e_status_desc = 'Autorizado o USO'.
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO.    "
    ENDTRY.

    "Aguardando Envio de Autorização de USO
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_aguardando_aut_uso( ).
        e_status = '04'.
        e_status_desc = 'Aguardando Envio de Autorização de USO'.
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO.    "
    ENDTRY.

    "Em Processamento de Autorização para Inutilização
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_nao_proc_inutilizacao( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        e_status = '10'.
        e_status_desc = 'Em Processamento de Autorização para Inutilização'.
        EXIT.
    ENDTRY.

    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_sem_erro_cancelamento( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        e_status = '08'.
        e_status_desc = 'Erro ao solicitar cancelamento'.
        EXIT.
    ENDTRY.

    "Erro Autorizado o USO
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_sem_erro_autorizacao( ).
      CATCH ZESZCX_DOC_ELETRONICO.    "

        "Tentar obter Log de erros, pois pode levar alguns segundos para gravar nas tabelas
        DO 25 TIMES.
          TRY.
              me->ZESZIF_DOC_ELETRONICO~get_load_log( IMPORTING et_message = DATA(et_message) ).
            CATCH ZESZCX_DOC_ELETRONICO.
          ENDTRY.

          IF et_message[] IS NOT INITIAL.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.

        e_status = '03'.
        e_status_desc = 'Erro Autorizar o USO'.

        IF et_message[] IS NOT INITIAL.

          CLEAR: e_status_desc.

          LOOP AT et_message INTO DATA(wa_message).
            MESSAGE ID wa_message-msgid TYPE wa_message-msgty NUMBER wa_message-msgno
               WITH wa_message-msgv1 wa_message-msgv2 wa_message-msgv3 wa_message-msgv4
               INTO DATA(ms_erro).

            e_status_desc = zcl_string=>concat(
              EXPORTING
                s1 = e_status_desc
                s2 = ms_erro
                sp = '*'
            ).

          ENDLOOP.
        ENDIF.

        EXIT.
    ENDTRY.

    e_status = '99'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_urls_docs.

    r_instancia = me.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_val_autorizacao_modal.

    r_instancia = me.

    "Implementar nas classes filhas


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_val_cancelamento_modal.

    r_instancia = me.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_val_reinicializar_modal.

    r_instancia = me.

    "Implementar nas classes filhas

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_xml.

    DATA: it_urllist TYPE tihttpurls2.

    r_instancia = me.

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

*    CASE sy-sysid.
*      WHEN 'PRD'.
*        wa_urllist-host = 'sapprd.maggi.corp'.
*        wa_urllist-port = '8000'.
*      WHEN 'QAS'.
*        wa_urllist-host = 'sapqas.aroeira.corp'.
*        wa_urllist-port = '8000'.
*      WHEN 'DEV'.
*        wa_urllist-host = 'sapdev.aroeira.corp'.
*        wa_urllist-port = '8000'.
*    ENDCASE.

    "http://sapqas.maggi.corp:8001/custom/docfiscal?sap-client=300
    DATA(wa_dominio) = wa_urllist-protocol && '://' && wa_urllist-host && ':' && wa_urllist-port && wa_urllist-url.

    CASE me->ZESZIF_DOC_ELETRONICO~at_documento-model.
      WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_nfe.
        DATA(e_link_xml) = wa_dominio && '/getnfexml?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
      WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_cte.
        e_link_xml = wa_dominio && '/getctexml?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
      WHEN ZESZIF_DOC_ELETRONICO=>at_st_model_mdfe.
        e_link_xml = wa_dominio && '/getmdfexml?' && 'sap-client=' && sy-mandt && '&i_docnum=' && me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    ENDCASE.

    TRY .
        zcl_arquivo=>get_file_uri_get_(
          EXPORTING
            i_uri          = e_link_xml
          IMPORTING
            e_texto_2      = e_xml
            e_code         = DATA(e_code)
            e_reason       = DATA(e_reason)
        ).

        IF e_code NE '200'.

          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                attr1 = e_reason )
              msgty  = 'E'
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
              msgv1  = CONV #( e_reason ).

        ENDIF.

      CATCH zcx_arquivo INTO DATA(ex_arquivo).    "
        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ex_arquivo->msgid
                              msgno = ex_arquivo->msgno
                              attr1 = ex_arquivo->msgv1
                              attr2 = ex_arquivo->msgv2
                              attr3 = ex_arquivo->msgv3
                              attr4 = ex_arquivo->msgv4 )
            msgty  = 'E'
            msgid  = ex_arquivo->msgid
            msgno  = ex_arquivo->msgno
            msgv1  = ex_arquivo->msgv1
            msgv2  = ex_arquivo->msgv2
            msgv3  = ex_arquivo->msgv3
            msgv4  = ex_arquivo->msgv4.
    ENDTRY.

  ENDMETHOD.


  method ZESZIF_DOC_ELETRONICO~GET_XML_EVENTO_GRC.

    DATA: lv_destination TYPE syhost,
          lv_nfeid       TYPE char44,
          lv_return      TYPE sy-subrc,
          lv_rfcdest     TYPE rfcdest,
          lv_xnfeactive  TYPE j_1bxnfeactive.

*    CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
*      EXPORTING
*        i_bukrs      = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-bukrs
*        i_branch     = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-branch
*        i_model      = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-model
*      IMPORTING
*        e_rfcdest    = lv_rfcdest
*        e_xnfeactive = lv_xnfeactive
*      EXCEPTIONS
*        rfc_error    = 1
*        OTHERS       = 2.
*
*    IF sy-subrc IS INITIAL. "AND lv_xnfeactive = 'X'.
*      lv_destination = lv_rfcdest.
*    ENDIF.
*
*    CHECK lv_destination IS NOT INITIAL.

    CONCATENATE me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-regio
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfyear
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfmonth
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-stcd1
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-model
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-serie
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfnum9
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-tpemis
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docnum9+1(8)
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-cdv
           INTO lv_nfeid.

    CHECK: lv_nfeid IS NOT INITIAL.


    e_xml_string = zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = conv #( lv_nfeid )
                                                                          i_direcao = 'OUT'
                                                                          i_evento  = conv #( i_tpevento )
                                                                IMPORTING e_xml_raw = e_xml_xstring   ).

    IF e_xml_string is NOT INITIAL.
      e_retorno = 0.
    ELSE.
      e_retorno = 4.
    ENDIF.

    "Função que Le o XML no GRC
*    CALL FUNCTION 'ZSD_LER_XML_NFE_OUTBOUND' DESTINATION lv_destination
*      EXPORTING
*        i_nfeid         = lv_nfeid
*        i_evento        = i_tpevento
*      IMPORTING
*        e_xmlstring     = e_xml_xstring
*      EXCEPTIONS
*        erro_nenhum_xml = 1
*        OTHERS          = 2.
*
*    e_xml_string = zcl_string=>xstring_to_string( i_xstring =  e_xml_xstring ).
*
*    e_retorno = sy-subrc.

  endmethod.


  METHOD ZESZIF_DOC_ELETRONICO~get_xml_grc.

    DATA: lv_destination TYPE char40, "syhost, ---> S4 Migration - 10/06/2023 - DG
          lv_nfeid       TYPE char44,
          lv_return      TYPE sy-subrc,
          lv_rfcdest     TYPE rfcdest,
          lv_xnfeactive  TYPE j_1bxnfeactive.

*-S4H-US 122597-07.09.2023-JT-inicio

*    CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
*      EXPORTING
*        i_bukrs      = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-bukrs
*        i_branch     = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-branch
*        i_model      = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-model
*      IMPORTING
*        e_rfcdest    = lv_rfcdest
*        e_xnfeactive = lv_xnfeactive
*      EXCEPTIONS
*        rfc_error    = 1
*        OTHERS       = 2.
*
*    IF sy-subrc IS INITIAL. "AND lv_xnfeactive = 'X'.
*      lv_destination = lv_rfcdest.
*    ENDIF.
*
*    CHECK lv_destination IS NOT INITIAL.
*
    CONCATENATE me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-regio
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfyear
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfmonth
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-stcd1
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-model
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-serie
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfnum9
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-tpemis
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docnum9+1(8)
                me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-cdv
           INTO lv_nfeid.

    CHECK: lv_nfeid IS NOT INITIAL.


    "Função que Le o XML no GRC
*    CALL FUNCTION 'ZSD_LER_XML_NFE_OUTBOUND' DESTINATION lv_destination
*      EXPORTING
*        i_nfeid         = lv_nfeid
*      IMPORTING
*        e_xmlstring     = e_xml_xstring
*      EXCEPTIONS
*        erro_nenhum_xml = 1
*        OTHERS          = 2.

    e_xml_string = zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = conv #( lv_nfeid )
                                                                          i_direcao = 'OUT'
                                                                IMPORTING e_xml_raw = e_xml_xstring ).

    IF e_xml_string is NOT INITIAL.
      e_retorno = 0.
    ELSE.
      e_retorno = 4.
    ENDIF.


*-S4H-US 122597-07.09.2023-JT-fim

*    IF ( sy-subrc EQ 0 ) AND ( e_xml_xstring IS NOT INITIAL ).
*      e_xml_string = zcl_string=>xstring_to_string( i_xstring =  e_xml_xstring ).
*    ELSE.
*      sy-subrc = 4.
*    ENDIF.
*
*    e_retorno = sy-subrc.


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~get_xml_terceiro.

    DATA: wa_active TYPE j_1bnfe_active.

    DATA: lv_destination TYPE char40, "syhost, ---> S4 Migration - 10/06/2023 - DG
          lv_nfeid       TYPE char44,
          lv_return      TYPE sy-subrc,
          lv_rfcdest     TYPE rfcdest,
          lv_xnfeactive  TYPE j_1bxnfeactive.

*    CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
*      EXPORTING
*        i_bukrs      = '0001'
*        i_branch     = '0111'
*        i_model      = '55'
*      IMPORTING
*        e_rfcdest    = lv_rfcdest
*        e_xnfeactive = lv_xnfeactive
*      EXCEPTIONS
*        rfc_error    = 1
*        OTHERS       = 2.
*
*    IF sy-subrc IS INITIAL. "AND lv_xnfeactive = 'X'.
*      lv_destination = lv_rfcdest.
*    ENDIF.

    wa_active-regio     = i_chave(2).
    wa_active-nfyear    = i_chave+2(2).
    wa_active-nfmonth   = i_chave+4(2).
    wa_active-stcd1     = i_chave+6(14).
    wa_active-model     = i_chave+20(2).
    wa_active-serie     = i_chave+22(3).
    wa_active-nfnum9    = i_chave+25(9).
    wa_active-docnum9   = i_chave+34(9).
    wa_active-cdv       = i_chave+43(1).

    SELECT SINGLE * INTO @wa_active
      FROM j_1bnfe_active
     WHERE regio   EQ @wa_active-regio
       AND nfyear  EQ @wa_active-nfyear
       AND nfmonth EQ @wa_active-nfmonth
       AND stcd1   EQ @wa_active-stcd1
       AND model   EQ @wa_active-model
       AND serie   EQ @wa_active-serie
       AND nfnum9  EQ @wa_active-nfnum9
       AND docnum9 EQ @wa_active-docnum9
       AND cdv     EQ @wa_active-cdv
       AND form    NE @space
       AND NOT EXISTS ( SELECT * FROM ZESZIB_NFE_DIST_TER AS a WHERE a~chave_nfe EQ @i_chave ).

    IF sy-subrc IS INITIAL.

      e_xml_string = zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = conv #( i_chave )
                                                                            i_direcao = 'OUT'
                                                                  IMPORTING e_xml_raw = e_xml_xstring

                                                                  ).

*      "Função que Le o XML no GRC
*      CALL FUNCTION 'ZSD_LER_XML_NFE_OUTBOUND' DESTINATION lv_destination
*        EXPORTING
*          i_nfeid         = i_chave
*          i_direcao       = 'OUT'
*        IMPORTING
*          e_xmlstring     = e_xml_xstring
*        EXCEPTIONS
*          erro_nenhum_xml = 1
*          OTHERS          = 2.

    ELSE.

      e_xml_string = zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = conv #( i_chave )
                                                                            i_direcao = 'IN'
                                                                  IMPORTING e_xml_raw = e_xml_xstring  ).

*      "Função que Le o XML no GRC
*      CALL FUNCTION 'ZSD_LER_XML_NFE_OUTBOUND' DESTINATION lv_destination
*        EXPORTING
*          i_nfeid         = i_chave
*          i_direcao       = 'IN'
*        IMPORTING
*          e_xmlstring     = e_xml_xstring
*        EXCEPTIONS
*          erro_nenhum_xml = 1
*          OTHERS          = 2.



    ENDIF.

    "e_xml_string = zcl_string=>xstring_to_string( i_xstring =  e_xml_xstring ).

  ENDMETHOD.


  method ZESZIF_DOC_ELETRONICO~IMPRIMIR_DOCUMENTO_AUX.

    DATA: LVA_LINK_PDF  TYPE STRING,
          LVA_NODE_DATA TYPE BXMNODES-URL.

    DATA: OB_WEB_SERVICE TYPE REF TO ZESZCL_WEBSERVICE.

    CHECK STRLEN( I_CHAVE ) EQ 44.

    CLEAR: LVA_LINK_PDF.

    CASE I_CHAVE+20(2).
      WHEN '55'.

        SELECT SINGLE *
          FROM ZESZIB_NFE_DIST_TER INTO @DATA(WL_NFE_DIST_TER)
         WHERE CHAVE_NFE EQ @I_CHAVE
           AND CANCEL    EQ @ABAP_FALSE.

        CHECK SY-SUBRC EQ 0.

        IF WL_NFE_DIST_TER-URLDANFE IS NOT INITIAL.

          LVA_LINK_PDF = WL_NFE_DIST_TER-URLDANFE.

        ELSEIF WL_NFE_DIST_TER-ID_SIMETRYA IS NOT INITIAL.

          CREATE OBJECT OB_WEB_SERVICE.

          TRY .
            OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'YN' ).
            OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'Y' ).
            "http://simetrya.grupomaggi.com.br:8080/nfe/monitorNfeEntradaDanfePdf?numrNfeSeqc=
            DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).

          CATCH ZESZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
            RAISE EXCEPTION TYPE ZESZCX_NFE_INBOUND_EXCEPTION
              EXPORTING
                TEXTID = VALUE #(  MSGNO = ZESZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGNO
                                   MSGID = ZESZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGID )
                MSGTY  = 'E'
                MSGNO  = ZESZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGNO
                MSGID  = ZESZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGID.
          ENDTRY.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WL_NFE_DIST_TER-ID_SIMETRYA
            IMPORTING
              OUTPUT = WL_NFE_DIST_TER-ID_SIMETRYA.

          LVA_LINK_PDF = LC_URI && WL_NFE_DIST_TER-ID_SIMETRYA && ','.

        ENDIF.

      WHEN '57'.

        SELECT SINGLE *
          FROM ZESZIB_CTE_DIST_TER INTO @DATA(WL_CTE_DIST_TER)
         WHERE CD_CHAVE_CTE EQ @I_CHAVE
           AND CANCEL       EQ @ABAP_FALSE.

        CHECK SY-SUBRC EQ 0.

        IF WL_CTE_DIST_TER-ID_SIMETRYA IS NOT INITIAL.

          CREATE OBJECT OB_WEB_SERVICE.

          TRY.
             OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'YC' ).
             OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'Y' ).
             "http://simetrya.grupomaggi.com.br:8080/cte/cteTerceirosDactePdf?numrCteSeqc=
             LC_URI = OB_WEB_SERVICE->GET_URI(  ).

          CATCH ZESZCX_WEBSERVICE INTO LC_EXCEPTION.
             RAISE EXCEPTION TYPE ZESZCX_CTE_INBOUND
              EXPORTING
                TEXTID = VALUE #(  MSGNO = ZESZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGNO
                                   MSGID = ZESZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGID )
                MSGTY  = 'E'
                MSGNO  = ZESZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGNO
                MSGID  = ZESZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGID.
          ENDTRY.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WL_CTE_DIST_TER-ID_SIMETRYA
            IMPORTING
              OUTPUT = WL_CTE_DIST_TER-ID_SIMETRYA.

          LVA_LINK_PDF = LC_URI && WL_CTE_DIST_TER-ID_SIMETRYA.

        ENDIF.

      WHEN OTHERS.
        EXIT.

    ENDCASE.

    IF LVA_LINK_PDF IS INITIAL.
      CALL FUNCTION 'Z_GRC_MONTA_LINK'
        EXPORTING
          I_CHAVE    = I_CHAVE
       IMPORTING
          E_LINK_PDF = LVA_LINK_PDF.
    ENDIF.

    CHECK LVA_LINK_PDF IS NOT INITIAL.

    LVA_NODE_DATA = LVA_LINK_PDF.

    CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
       EXPORTING
         NODE_DATA = LVA_NODE_DATA.


  endmethod.


  METHOD ZESZIF_DOC_ELETRONICO~set_altera_status.

    DATA: lc_texto TYPE c LENGTH 200.

    DATA(symsgid) = sy-msgid.
    DATA(symsgno) = sy-msgno.
    DATA(symsgv1) = sy-msgv1.
    DATA(symsgv2) = sy-msgv2.
    DATA(symsgv3) = sy-msgv3.
    DATA(symsgv4) = sy-msgv4.

    r_instancia = me.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lc_texto WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    DATA: it_notas TYPE TABLE OF ZESZIB_NOTA_FISCAL_SAP,
          wa_nota  TYPE ZESZIB_NOTA_FISCAL_SAP.

    CLEAR: it_notas[], wa_nota.

    "Verifica se CT-e está autorizado.
    SELECT SINGLE docnum INTO @DATA(v_docnum)
      FROM j_1bnfe_active
     WHERE docnum   = @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
       AND docsta   = '1'.

    CHECK sy-subrc NE 0.

    wa_nota-tp_authcod       = i_tp_authcod.
    wa_nota-nu_documento_sap = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    wa_nota-dt_authcod       = sy-datum.

    IF sy-timlo IS INITIAL.
      wa_nota-hr_authcod   = sy-uzeit.
    ELSE.
      wa_nota-hr_authcod   = sy-timlo.
    ENDIF.

    wa_nota-ms_erro = lc_texto.
    APPEND wa_nota TO it_notas.

    CALL FUNCTION 'Z_SD_INBOUND_NFE_XML'
      TABLES
        it_notas = it_notas.

    sy-msgid = symsgid.
    sy-msgno = symsgno.
    sy-msgv1 = symsgv1.
    sy-msgv2 = symsgv2.
    sy-msgv3 = symsgv3.
    sy-msgv4 = symsgv4.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_autorizar.

    r_instancia = me.

    IF i_ciclos IS NOT INITIAL.
      me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos = i_ciclos.
    ENDIF.

    IF i_segundos IS NOT INITIAL.
      me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos = i_segundos.
    ENDIF.

    me->ZESZIF_DOC_ELETRONICO~get_ck_doc_nao_cancel(
     )->get_ck_data_documento(
     )->get_ck_certidao_negativa(
     )->get_ck_doc_proprio(
     )->get_ck_valor_documento(
     ).

    ZESZCL_DOC_ELETRONICO=>validacao_autorizacao_uso( i_docnum =  me->ZESZIF_DOC_ELETRONICO~at_documento-docnum ).

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
      CATCH ZESZCX_DOC_ELETRONICO.    "
        "Validações Específicas do MODAL (Redefinir Método)
        "me->ZESZIF_DOC_ELETRONICO~get_val_autorizacao_modal( ).

        "Se Número não estiver gerado gera número
        TRY.
            me->ZESZIF_DOC_ELETRONICO~get_ck_numero_determinado( ).
          CATCH ZESZCX_DOC_ELETRONICO .
            "Determinar Numeração
            me->ZESZIF_DOC_ELETRONICO~set_autoriza_viagem_tip_frete(
            )->get_val_autorizacao_modal(
            )->set_det_numero(
            )->set_registra_autorizacao(
            )->get_aguardar(
                 i_aguardar = i_aguardar
                 i_ciclos = me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos
                 i_segundos = me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos
            ).
            EXIT.
        ENDTRY.
    ENDTRY.

    "Verifica se não está em Processamento
    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_nao_processamento( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        "Enviar solicitação de autorização de uso
        me->ZESZIF_DOC_ELETRONICO~set_enviar_autorizacao_uso( i_resend = abap_true
         )->get_aguardar( i_aguardar = i_aguardar i_ciclos = i_ciclos i_segundos = i_segundos
         ).
        EXIT.
    ENDTRY.

    "Verifica se não tem Erro de Autorização
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_sem_erro_autorizacao( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        me->ZESZIF_DOC_ELETRONICO~set_eliminar_logs(
         )->set_reinicializar( ).
    ENDTRY.

    "Verificar se Autorizado o USO
    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso( ).
      CATCH ZESZCX_DOC_ELETRONICO.    "
        "Enviar solicitação de autorização de uso
        me->ZESZIF_DOC_ELETRONICO~set_enviar_autorizacao_uso(
         )->get_aguardar( i_aguardar = i_aguardar i_ciclos = i_ciclos i_segundos = i_segundos
         ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_autoriza_mdfe.

    r_instancia = me.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_autoriza_mfde.
    r_instancia = me.
  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_autoriza_viagem_tip_frete.

    r_instancia = me.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_bloquear.

    r_instancia = me.

    CASE i_bloquear.
      WHEN abap_true.
        me->ZESZIF_DOC_ELETRONICO~at_sem_lock = abap_false.
      WHEN abap_false.
        me->ZESZIF_DOC_ELETRONICO~at_sem_lock = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_bloquear_registro.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_sem_lock EQ abap_false.

    CALL FUNCTION 'ZENQUEUE_DOC_ELETRONICO'
      EXPORTING
        docnum         = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS NOT INITIAL.

      me->ZESZIF_DOC_ELETRONICO~set_clear(
      )->get_erro_geral( ).

    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_cancelar.

    r_instancia = me.

    IF i_ciclos IS NOT INITIAL.
      me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos = i_ciclos.
    ENDIF.

    IF i_segundos IS NOT INITIAL.
      me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos = i_segundos.
    ENDIF.

    TRY .
        "Documento Já cancelado sai da rotina
        me->ZESZIF_DOC_ELETRONICO~get_ck_doc_cancel( ).
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO.    "
    ENDTRY.

    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_numero_determinado( ).
      CATCH ZESZCX_DOC_ELETRONICO.
        "Não foi Determinado número
        EXIT.
    ENDTRY.

    me->ZESZIF_DOC_ELETRONICO~get_ck_doc_nao_cancel(
     )->get_ck_doc_proprio(
     "Validações Específicas do MODAL (Redefinir Método)
     )->get_val_cancelamento_modal(
     ).

    "Verifica se Não está em processamento o Cancelamento
    TRY.
        me->ZESZIF_DOC_ELETRONICO~get_ck_nao_proc_cancel( ).
      CATCH ZESZCX_DOC_ELETRONICO .
        "Reenvia Cancelamento
        me->ZESZIF_DOC_ELETRONICO~set_enviar_cancelamento( i_resend = abap_true
         )->get_aguardar(
           i_aguardar = i_aguardar
           i_ciclos   = me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos
           i_segundos = me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos ).
        EXIT.
    ENDTRY.

    "Verificar Erro de Solicitação de Cancelmanto
    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_sem_erro_cancelamento( ).

      CATCH ZESZCX_DOC_ELETRONICO .
        me->ZESZIF_DOC_ELETRONICO~set_eliminar_logs( ).
    ENDTRY.


    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_estornar_doc_origem(
         )->set_motivo_cancelamento(
              i_motivo    = i_motivo
              i_ds_motivo = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( i_ds_motivo ) )
         )->set_enviar_cancelamento(
         )->get_aguardar(
           i_aguardar = i_aguardar
           i_ciclos   = me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos
           i_segundos = me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos ).
        EXIT.
      CATCH ZESZCX_DOC_ELETRONICO.
    ENDTRY.

    "Verifica se Documento Está Autorizado e Somente Envia o Cancelamento
    TRY .
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_cancel( ).
      CATCH ZESZCX_DOC_ELETRONICO.
        "Solicita Cancelamento
        me->ZESZIF_DOC_ELETRONICO~get_ck_autorizado_uso(
         )->set_motivo_cancelamento(
              i_motivo    = i_motivo
              i_ds_motivo = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( i_ds_motivo ) )
         )->set_enviar_cancelamento(
         )->get_aguardar(
           i_aguardar = i_aguardar
           i_ciclos   = me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos
           i_segundos = me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_clear.

    r_instancia = me.

    me->ZESZIF_DOC_ELETRONICO~set_liberar_registro( ).

    CLEAR: me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico,
           me->ZESZIF_DOC_ELETRONICO~at_documento,
           me->ZESZIF_DOC_ELETRONICO~at_instance.

    me->ZESZIF_DOC_ELETRONICO~at_qtd_ciclos    = i_ciclos.
    me->ZESZIF_DOC_ELETRONICO~at_qtd_segundos  = i_segundos.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_clear_log_erro.

    DATA: gs_subobject   TYPE bal_s_sub,
          gs_object      TYPE bal_s_obj,
          gs_extnum      TYPE bal_s_extn,
          c_object       TYPE balobj  VALUE 'NFE',
          c_subobject    TYPE balsubobj VALUE 'MONITOR',
          gs_log_filter  TYPE bal_s_lfil,
          it_log_header2 TYPE balhdr_t.

    r_instancia  = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_documento-docnum IS NOT INITIAL.

    gs_subobject-sign   = gs_object-sign    = gs_extnum-sign   = 'I'.
    gs_subobject-option = gs_object-option  = gs_extnum-option = 'EQ'.
    gs_object-low       = gs_object-high    = c_object.
    gs_subobject-low    = gs_subobject-high = c_subobject.

    CLEAR gs_log_filter.
    CLEAR it_log_header2.
    APPEND gs_object    TO gs_log_filter-object.
    APPEND gs_subobject TO gs_log_filter-subobject.
    gs_extnum-low = gs_extnum-high = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    APPEND gs_extnum TO gs_log_filter-extnumber.

* Get header of application log for selected NF-e documents
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = gs_log_filter
      IMPORTING
        e_t_log_header     = it_log_header2
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      "MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '034'.
    ENDIF.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'BAL_DB_DELETE'
      EXPORTING
        i_t_logs_to_delete = it_log_header2
        i_client           = sy-mandt
        i_with_commit_work = 'X'
      EXCEPTIONS
        no_logs_specified  = 1
        OTHERS             = 2.

    CHECK sy-subrc IS INITIAL.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_det_numero.

    DATA: wa_zib_nfe TYPE ZESZIB_NFE.

    r_instancia = me.

* Lock J_1BNFDOC

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
      EXPORTING
        doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        lock_mode  = 'X'.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
      EXPORTING
        doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        lock_mode  = 'S'.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
      EXPORTING
        doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        lock_mode  = 'E'.

    CALL FUNCTION 'J_1B_NFE_SET_NUMBER'
      EXPORTING
        iv_docnum                 = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
      IMPORTING
        es_active_mod             = e_j_1bnfe_active
      EXCEPTIONS
        numbering_not_possible    = 1
        lock_error_nfdoc          = 2
        lock_error_active         = 3
        already_numbered          = 4
        rfc_failure               = 5
        lock_error_nfdoc_for_rfc  = 6
        lock_error_active_for_rfc = 7
        OTHERS                    = 8.

    IF sy-subrc IS NOT INITIAL.

      CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
        EXPORTING
          doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
          lock_mode  = 'X'.

      CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
        EXPORTING
          doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
          lock_mode  = 'S'.

      CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
        EXPORTING
          doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
          lock_mode  = 'E'.

      me->ZESZIF_DOC_ELETRONICO~get_erro_geral( ).

    ENDIF.

    me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico = e_j_1bnfe_active.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
      EXPORTING
        doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        lock_mode  = 'X'.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
      EXPORTING
        doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        lock_mode  = 'S'.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UNLOCK'
      EXPORTING
        doc_number = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        lock_mode  = 'E'.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_eliminar_logs.

    r_instancia = me.

    TRY .

        me->ZESZIF_DOC_ELETRONICO~get_logs_erro( IMPORTING e_balhdr = DATA(e_balhdr) ).

* Delete application-log entries for selected NF-e documents
        CALL FUNCTION 'BAL_DB_DELETE'
          EXPORTING
            i_t_logs_to_delete = e_balhdr
            i_client           = sy-mandt
            i_with_commit_work = 'X'
          EXCEPTIONS
            no_logs_specified  = 1
            OTHERS             = 2.

        "IF SY-SUBRC IS NOT INITIAL.
        "  ME->ZESZIF_DOC_ELETRONICO~GET_ERRO_GERAL( ).
        "ENDIF.

      CATCH ZESZCX_DOC_ELETRONICO.
    ENDTRY.


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_enviar_autorizacao_uso.

    r_instancia = me.

    IF me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-nfnum9 IS NOT INITIAL.
      IF me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ  EQ '3' AND
         me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-docsta EQ space AND
         me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta EQ space AND
         me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-msstat EQ space.
        DATA(lc_resend) = abap_false.
      ELSE.
        lc_resend = abap_true.
      ENDIF.
    ELSE.
      lc_resend = abap_false.
    ENDIF.

    CALL FUNCTION 'J_1B_NFE_SEND_C_NFE'
      EXPORTING
        iv_docnum           = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
        iv_resend           = lc_resend
      IMPORTING
        es_active_mod       = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico
      EXCEPTIONS
        not_sent            = 1
        not_allowed_to_send = 2
        OTHERS              = 3.

    IF sy-subrc IS NOT INITIAL.
      me->ZESZIF_DOC_ELETRONICO~get_erro_geral( ).
    ENDIF.

    me->ZESZIF_DOC_ELETRONICO~set_registra_autorizacao( ).

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_enviar_cancelamento.

    DATA: it_acttab TYPE TABLE OF j_1bnfe_active.

    r_instancia = me.

    APPEND me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico TO it_acttab.
    "APPEND ME->ZESZIF_DOC_ELETRONICO~AT_INFO_DOC_ELETRONICO TO IT_ACTTAB.

    CASE me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-scssta.
      WHEN '1'.
        DATA(lc_resend) = abap_true.
      WHEN OTHERS.
        lc_resend = i_resend.
    ENDCASE.

    CALL FUNCTION 'J_1B_NFE_SEND_REQUESTS'
      EXPORTING
        iv_resend           = lc_resend
      TABLES
        it_acttab           = it_acttab
        it_acttab_mod       = it_acttab
      EXCEPTIONS
        status_error        = 1
        rfc_failure         = 2
        cancel_reason_error = 3
        OTHERS              = 4.

    IF sy-subrc IS NOT INITIAL.
      me->ZESZIF_DOC_ELETRONICO~get_erro_geral( ).
    ENDIF.

    me->ZESZIF_DOC_ELETRONICO~set_registra_cancelamento( ).

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_liberar_registro.

    r_instancia = me.

    CHECK me->ZESZIF_DOC_ELETRONICO~at_sem_lock EQ abap_false.

    CALL FUNCTION 'ZDENQUEUE_DOC_ELETRONICO'
      EXPORTING
        docnum = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_motivo_cancelamento.

    r_instancia = me.

    IF i_ds_motivo IS INITIAL.
      me->ZESZIF_DOC_ELETRONICO~get_motivo_cancelamento(
        EXPORTING
          i_motivo    = i_motivo
        IMPORTING
          e_motivo    = DATA(e_motivo) ).
    ENDIF.

    IF i_ds_motivo IS NOT INITIAL.
      DATA: lc_reason TYPE c LENGTH 256.
      lc_reason = i_ds_motivo.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason  = '01'.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason1 = lc_reason+000(64).
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason2 = lc_reason+064(64).
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason3 = lc_reason+128(64).
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason4 = lc_reason+192(64).
    ELSE.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason  = e_motivo-reason.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason1 = e_motivo-reason1.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason2 = e_motivo-reason2.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason3 = e_motivo-reason3.
      me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-reason4 = e_motivo-reason4.
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_registra_autorizacao.

    DATA: wa_zib_nfe TYPE ZESZIB_NFE.

    r_instancia = me.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    SELECT SINGLE *
      FROM ZESZIB_NFE
      INTO wa_zib_nfe
     WHERE docnum = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    "Regitrar Autorização
    wa_zib_nfe-docnum     = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
    wa_zib_nfe-date_aut_1 = sy-datum.
    wa_zib_nfe-time_aut_1 = sy-uzeit.
    wa_zib_nfe-user_aut_1 = sy-uname.
    MODIFY ZESZIB_NFE FROM wa_zib_nfe.
    COMMIT WORK.


  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_registra_cancelamento.

    r_instancia = me.

    "Regitrar Cancelamento
    SELECT SINGLE * INTO @DATA(wa_zib_nfe)
      FROM ZESZIB_NFE
     WHERE docnum EQ @me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.

    IF sy-subrc IS INITIAL.
      wa_zib_nfe-date_aut_2 = sy-datum.
      wa_zib_nfe-time_aut_2 = sy-uzeit.
      wa_zib_nfe-user_aut_2 = sy-uname.
      MODIFY ZESZIB_NFE FROM wa_zib_nfe.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_registro.

    r_instancia = me.

    me->ZESZIF_DOC_ELETRONICO~set_clear( ).

    me->ZESZIF_DOC_ELETRONICO~at_sem_lock = i_sem_bloqueio.

    SELECT SINGLE * INTO me->ZESZIF_DOC_ELETRONICO~at_documento
      FROM j_1bnfdoc
     WHERE docnum EQ i_docnum.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
        EXPORTING
          textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno attr1 = CONV #( i_docnum ) )
          msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid
          msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno
          msgv1  = CONV #( i_docnum )
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE * INTO me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico
      FROM j_1bnfe_active
     WHERE docnum EQ i_docnum.

    IF sy-subrc IS NOT INITIAL.

      me->ZESZIF_DOC_ELETRONICO~set_clear( ).

      RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
        EXPORTING
          textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_eletronic_nao_enc-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_eletronic_nao_enc-msgno attr1 = CONV #( i_docnum ) )
          msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_eletronic_nao_enc-msgid
          msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_eletronic_nao_enc-msgno
          msgv1  = CONV #( i_docnum )
          msgty  = 'E'.

    ENDIF.

    me->ZESZIF_DOC_ELETRONICO~set_bloquear_registro( ).

  ENDMETHOD.


  METHOD ZESZIF_DOC_ELETRONICO~set_reinicializar.

    r_instancia = me->ZESZIF_DOC_ELETRONICO~get_ck_fatura_ativa(
                  "Verifica Reinicializar por MODAL
                   )->get_val_reinicializar_modal(
                   ).


    DATA(_erro_reinicializar) = abap_false.

    TRY .

        CALL FUNCTION 'J_1B_NFE_RESET_REJECT_STATUS'
          EXPORTING
            i_docnum           = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum
          IMPORTING
            es_active_mod      = me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico
          EXCEPTIONS
            document_not_found = 1
            enqueue_error      = 2
            invalid_status     = 3
            OTHERS             = 4.

      CATCH cx_root.
    ENDTRY.

    IF sy-subrc IS NOT INITIAL.
      _erro_reinicializar = abap_true.
    ELSE.

      DATA(_docnum)         = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
      DATA(_reinicializado) = abap_false.

      DO 100 TIMES.
        SELECT SINGLE * INTO me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico
          FROM j_1bnfe_active
         WHERE docnum EQ _docnum.

        IF ( sy-subrc = 0 ) AND ( me->ZESZIF_DOC_ELETRONICO~at_info_doc_eletronico-action_requ EQ '3' ).
          _reinicializado = abap_true.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

      IF _reinicializado EQ abap_false.
        _erro_reinicializar = abap_true.
      ENDIF.

    ENDIF.

    IF ( _erro_reinicializar EQ abap_true ).

* Unlock J_1BNFDOC
      CALL FUNCTION 'DEQUEUE_EJ_1BNFS'
        EXPORTING
          mode_j_1bnfdoc = 'E'
          mandt          = sy-mandt
          docnum         = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
* Unlock J_1BNFE_ACTIVE
      CALL FUNCTION 'DEQUEUE_E_J1BNFE'
        EXPORTING
          mode_j_1bnfe_active = 'E'
          mandt               = sy-mandt
          docnum              = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.
* Unlock J_1BNFDE_INVALID
      CALL FUNCTION 'DEQUEUE_E_J1B_INVALID'
        EXPORTING
          mode_j_1bnfe_invalid = 'E'
          mandt                = sy-mandt
          docnum               = me->ZESZIF_DOC_ELETRONICO~at_documento-docnum.


      RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
        EXPORTING
          textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_reinicializar_doc-msgid
                            msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_reinicializar_doc-msgno )
          msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_reinicializar_doc-msgid
          msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_reinicializar_doc-msgno
          msgty  = 'E'.

    ENDIF.

  ENDMETHOD.


  METHOD VALIDACAO_AUTORIZACAO_USO.


    DATA: lva_msg_error TYPE c LENGTH 200.

    CHECK i_docnum IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(lwa_doc)
    WHERE docnum EQ @i_docnum.

    CASE lwa_doc-model.
      WHEN '55' OR '57'.

        "Validação CST x Impostos Documentos
        DATA(r_msg_error) = ZESZCL_DOC_ELETRONICO=>validacao_autorizacao_uso_0001( i_docnum = lwa_doc-docnum ).

        ".... Exemplo de como colocar mais Validações abaixo
*        IF R_MSG_ERROR IS INITIAL.
*           R_MSG_ERROR = ZESZCL_DOC_ELETRONICO=>validacao_autorizacao_uso_0002( I_DOCNUM = i_docnum ).
*        ENDIF.

    ENDCASE.

    CASE lwa_doc-model.
      WHEN '55'.
      WHEN '57'.
    ENDCASE.

    IF r_msg_error IS NOT INITIAL.

      lva_msg_error = r_msg_error.

      RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
        EXPORTING
          textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                            msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                            attr1 = lva_msg_error+000(50)
                            attr2 = lva_msg_error+050(50)
                            attr3 = lva_msg_error+100(50)
                            attr4 = lva_msg_error+150(50) )
          msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
          msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = lva_msg_error+000(50)
          msgv2  = lva_msg_error+050(50)
          msgv3  = lva_msg_error+100(50)
          msgv4  = lva_msg_error+150(50).

    ENDIF.


  ENDMETHOD.


  METHOD validacao_autorizacao_uso_0001.

    DATA: vl_aux(2)  TYPE c.

    CLEAR: r_msg_error.

    CHECK i_docnum IS NOT INITIAL.

    SELECT *
      FROM j_1bnflin INTO TABLE @DATA(it_itens)
    WHERE docnum EQ @i_docnum.

    CHECK it_itens[] IS NOT INITIAL.

    SELECT *
      FROM j_1bnfstx INTO TABLE @DATA(it_stx)
      FOR ALL ENTRIES IN @it_itens
      WHERE docnum EQ @it_itens-docnum
        AND itmnum EQ @it_itens-itmnum
        AND taxtyp EQ 'ICM3'.

    SELECT *
      FROM j_1batl1 INTO TABLE @DATA(it_j_1batl1)
       FOR ALL ENTRIES IN @it_itens
     WHERE taxlaw EQ @it_itens-taxlw1.


    LOOP AT it_itens INTO DATA(wl_itens).

      READ TABLE it_j_1batl1 INTO DATA(wl_j_1batl1) WITH KEY taxlaw =  wl_itens-taxlw1.
      CHECK sy-subrc EQ 0.

      CLEAR vl_aux.
      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = wl_j_1batl1-taxsit
        IMPORTING
          output = vl_aux.

      CASE vl_aux.
        WHEN '00'.
          READ TABLE it_stx INTO DATA(wl_stx) WITH KEY docnum = wl_itens-docnum
                                                       itmnum = wl_itens-itmnum.
          IF sy-subrc EQ 0.
            IF wl_stx-base < wl_itens-netwr.
              MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023
                 WITH 'A BC do ICMS não condiz com o CST da Lei Fiscal.'
                      'Abra uma FI para o CSC Fiscal!' INTO r_msg_error.

            ENDIF.
          ENDIF.
        WHEN '20'.
          READ TABLE it_stx INTO wl_stx WITH KEY docnum = wl_itens-docnum
                                                 itmnum = wl_itens-itmnum.
          IF sy-subrc EQ 0.
            IF wl_stx-base >= wl_itens-netwr OR wl_stx-base IS INITIAL.
              MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023
                 WITH 'A BC do ICMS não condiz com o CST da Lei Fiscal.'
                      'Abra uma FI para o CSC Fiscal!' INTO r_msg_error.

            ENDIF.
          ENDIF.
        WHEN '40' OR '41' OR '50' OR '51'.
          READ TABLE it_stx INTO wl_stx WITH KEY docnum = wl_itens-docnum
                                                 itmnum = wl_itens-itmnum.
          IF sy-subrc EQ 0.
            IF wl_stx-base > 0.
              MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023
                 WITH 'A BC do ICMS não condiz com o CST da Lei Fiscal.'
                      'Abra uma FI para o CSC Fiscal!' INTO r_msg_error.

            ENDIF.
          ENDIF.
      ENDCASE.

    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
