*----------------------------------------------------------------------*
***INCLUDE LZCTE_DISTF04.
*----------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_0301 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_0301  TYPE REF TO lcl_event_handler_0301.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_0301 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_0301 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: it_0301_dup TYPE TABLE OF ZESZIB_CTE_DIST_DUP    WITH HEADER LINE,
      it_0301_n55 TYPE TABLE OF ZESZIB_CTE_DIST_N55    WITH HEADER LINE,
      it_0301_n01 TYPE TABLE OF ZESZIB_CTE_DIST_N01    WITH HEADER LINE,
      it_0301_nit TYPE TABLE OF ZESZIB_CTE_DIST_NIT    WITH HEADER LINE.

DATA: gb_index            TYPE i,
      gb_index_ultimo     TYPE i,
      gb_ordem            TYPE char01,
      gb_index_ajuste     TYPE sy-tabix,
      gb_qtd_linhas       TYPE i,
      gb_ctr_altera_frete TYPE char01,
      gb_ctr_calcul_frete TYPE char01,
      gb_ctr_alterou_peso TYPE char01,
      gb_tela_block       TYPE char01,
      gb_peso_chegada     TYPE char01,
      ck_peso_liberado    TYPE char01,
      ck_peso_manual      TYPE char01,
      wa_info_forne       TYPE ty_info_forne,
      wa_zib_cte_dist_eap TYPE ZESZIB_CTE_DIST_EAP,
      e_tipo_contrato     TYPE char04,
      it_exclude_fcode    TYPE TABLE OF sy-ucomm,
      it_0301_vt          TYPE TABLE OF ZESZDE_CTE_DIST_VT_ALV WITH HEADER LINE,
      it_0301_vt_aux      TYPE TABLE OF ZESZDE_CTE_DIST_VT_ALV WITH HEADER LINE.


DATA: ctl_alv_0301       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0301       TYPE REF TO cl_gui_custom_container,
      gs_lay_0301        TYPE lvc_s_layo,
      gs_var_0301        TYPE disvariant,
      gs_scroll_col_0301 TYPE lvc_s_col,
      gs_scroll_row_0301 TYPE lvc_s_roid,
      it_catalog_0301    TYPE lvc_t_fcat,
      it_exclude_0301    TYPE ui_functions,
      wa_exclude_0301    LIKE LINE OF it_exclude_0301,
      wa_lfbk            TYPE lfbk,
      wa_bnka            TYPE bnka,
      ck_zbvtyp          TYPE char01.


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0301
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_0301
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_0301_vt INDEX row_id.
  gb_index_ajuste = row_id.

  CASE fieldname.
    WHEN 'IC_EDITAR'.
      PERFORM editar_0301_vt.
      LEAVE TO SCREEN 0301.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit0301 INPUT.

  CASE ok_code.
    WHEN ok_primeiro.
      IF gb_index NE 1.
        gb_index = 1.
      ENDIF.
    WHEN ok_anterior.
      IF gb_index NE 1.
        ADD -1 TO gb_index.
      ENDIF.
    WHEN ok_proximo.
      IF gb_index NE gb_index_ultimo.
        ADD 1 TO gb_index.
      ENDIF.
    WHEN ok_ultimo.
      IF gb_index NE gb_index_ultimo.
        gb_index = gb_index_ultimo.
      ENDIF.
    WHEN ok_cancel.
      IF gb_qtd_linhas EQ 1.
        gb_ordem = abap_true.
      ENDIF.
  ENDCASE.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " USER_COMMAND_EXIT0301  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301 INPUT.
  CASE ok_code.
    WHEN ok_salvar.
      PERFORM salvar_informacoes_doc.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0301  INPUT


*&---------------------------------------------------------------------*
*&      Form  FATURAR_CTE
*&---------------------------------------------------------------------*
*       Faturamento de CT-e
*----------------------------------------------------------------------*
FORM faturar_cte  USING p_cte TYPE ZESZIB_CTE_DIST_TER
                        p_index_ultimo TYPE i
               CHANGING p_index TYPE i
                        p_dt_vencimento TYPE ZESZDT_VENCTO
                        p_ordem TYPE char01.

  DATA: ck_primeiro_peso TYPE char01.

  CLEAR: it_0301_dup[],
         it_0301_n55[],
         it_0301_n01[],
         it_0301_nit[],
         it_0301_vt[],
         e_tipo_contrato,
         wa_info_forne.

  FIELD-SYMBOLS: <fs_0301_vt> TYPE ZESZDE_CTE_DIST_VT_ALV.

  gb_index               = p_index.
  gb_index_ultimo        = p_index_ultimo.
  gb_tela_block          = abap_false.

  SELECT SINGLE * INTO ZESZIB_CTE_DIST_TER
    FROM ZESZIB_CTE_DIST_TER
   WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

  IF sy-subrc IS INITIAL.
    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    IF ZESZIB_CTE_DIST_TER-ck_finalizado EQ abap_true.
      gb_tela_block = abap_true.
    ENDIF.

    "Sugerir data de Vencimento
    IF ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO IS INITIAL AND gb_tela_block EQ abap_false.
      SELECT *
        INTO TABLE it_0301_dup
        FROM ZESZIB_CTE_DIST_DUP
       WHERE cd_chave_cte  EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
         AND dt_vencimento GE sy-datum
       ORDER BY dt_vencimento.

      IF sy-subrc IS INITIAL.
        READ TABLE it_0301_dup INDEX 1.
        ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO = it_0301_dup-dt_vencimento.
      ENDIF.
    ENDIF.

    IF ( p_dt_vencimento IS NOT INITIAL ) AND ( gb_tela_block EQ abap_false ).
      ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO = p_dt_vencimento.
    ELSEIF ( p_dt_vencimento IS INITIAL ) AND ( gb_tela_block EQ abap_false ) AND ( ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO IS INITIAL ).

      CALL METHOD zcl_cte_dist_g=>busca_proximo_venc_fatura
        IMPORTING
          e_data_vencimento = p_dt_vencimento.

      ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO = p_dt_vencimento.
    ENDIF.

    "0  CT-e Normal
    "1  CT-e de Complemento de Valores
    "2  CT-e de Anula├º├úo de Valores
    "3  CT-e Substituto

    CASE ZESZIB_CTE_DIST_TER-cd_tipo_cte.
      WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

        SELECT *
          INTO TABLE it_0301_n55
          FROM ZESZIB_CTE_DIST_N55
         WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
           AND tknum        NE space
           AND docnum_nfe   NE space.

        IF it_0301_n55[] IS NOT INITIAL.

          LOOP AT it_0301_n55 INTO DATA(wa_n55).

            IF wa_n55-docnum_nfe IS INITIAL.
              CONTINUE.
            ENDIF.

            SELECT SINGLE * INTO @DATA(wa_itens)
              FROM j_1bnflin
             WHERE docnum EQ @wa_n55-docnum_nfe.

            IF sy-subrc IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria est├í parametrizado por frete lota├º├úo
            SELECT SINGLE * INTO @DATA(wa_zlest0154)
              FROM zlest0154
             WHERE bukrs EQ @ZESZIB_CTE_DIST_TER-e_tomadora
               AND lifnr EQ @ZESZIB_CTE_DIST_TER-p_emissor
               AND matkl EQ @wa_itens-matkl.

            IF sy-subrc IS INITIAL.
              e_tipo_contrato = '0002'.
            ENDIF.

          ENDLOOP.

          SELECT *
            INTO TABLE it_0301_nit
            FROM ZESZIB_CTE_DIST_NIT
             FOR ALL ENTRIES IN it_0301_n55
           WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
             AND docnum       EQ it_0301_n55-docnum_nfe.
        ENDIF.

        SELECT *
          INTO TABLE it_0301_n01
          FROM ZESZIB_CTE_DIST_N01
         WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
           AND tknum        NE space
           AND docnum_nf    NE space.

        IF it_0301_n01[] IS NOT INITIAL.
          SELECT *
            APPENDING TABLE it_0301_nit
            FROM ZESZIB_CTE_DIST_NIT
             FOR ALL ENTRIES IN it_0301_n01
           WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
             AND docnum       EQ it_0301_n01-docnum_nf.
        ENDIF.

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "" Verfica Autoriza├º├úo de Pagamento """""""""""""""""""""""""""""""""""""""""""
        DATA: lc_ck_autorizados TYPE c LENGTH 1,
              lc_matnr          TYPE matnr,
              lc_grupo          TYPE matkl,
              lc_tipo           TYPE ZESZDE_TP_AUT_FRETE.

        lc_ck_autorizados = abap_true.

        LOOP AT it_0301_nit.

          SELECT SINGLE matnr INTO lc_matnr
            FROM j_1bnflin
           WHERE docnum EQ it_0301_nit-docnum
             AND itmnum EQ it_0301_nit-itmnum.

          SELECT SINGLE matkl INTO lc_grupo
            FROM mara
           WHERE matnr EQ lc_matnr.

          SELECT SINGLE tp_aut_frete
            INTO lc_tipo
            FROM ZESZIB_CTE_DIST_GM
           WHERE matkl EQ lc_grupo.

          IF ( sy-subrc IS INITIAL ) AND ( it_0301_nit-ck_autorizado NE abap_true ) AND ( ZESZIB_CTE_DIST_TER-cd_modal NE '04' ) AND ( e_tipo_contrato NE '0002' ).
            lc_ck_autorizados = abap_false.
          ENDIF.
        ENDLOOP.
        "" Verfica Autoriza├º├úo de Pagamento """""""""""""""""""""""""""""""""""""""""""
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        LOOP AT it_0301_n55.
          CLEAR: it_0301_vt.
          READ TABLE it_0301_nit WITH KEY docnum = it_0301_n55-docnum_nfe.
          it_0301_vt-tknum            = it_0301_n55-tknum.
          it_0301_vt-auart_va         = it_0301_n55-auart_va.
          it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
          it_0301_vt-zvlr_vi          = it_0301_n55-zvlr_vi.
          it_0301_vt-zvlr_mercadoria  = it_0301_n55-zvlr_mercadoria.

          "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
          IF it_0301_nit-ck_autorizado EQ abap_true.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_n55-ZESZVLR_FRETE.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-ZESZPESO_DIFERENCA.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-ZESZQUEBRA          = it_0301_nit-ZESZQUEBRA.
          it_0301_vt-ZESZPERDA           = it_0301_nit-ZESZPERDA.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-ZESZVLR_QUEBRA      = it_0301_n55-ZESZVLR_QUEBRA.
          it_0301_vt-ZESZVLR_PERDA	      = it_0301_n55-ZESZVLR_PERDA.
          it_0301_vt-ZESZVLR_LIQ_PAGAR	  = it_0301_n55-ZESZVLR_LIQ_PAGAR.
          it_0301_vt-ck_peso_digitado = it_0301_n55-ck_peso_digitado.
          it_0301_vt-docnum           = it_0301_nit-docnum.
          it_0301_vt-itmnum           = it_0301_nit-itmnum.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( ZESZIB_CTE_DIST_TER-ck_peso_chegada EQ abap_true ).
            it_0301_vt-ic_editar = icon_set_state.
          ELSE.
            it_0301_vt-ic_editar = icon_change_number.
          ENDIF.
          APPEND it_0301_vt.
        ENDLOOP.

        LOOP AT it_0301_n01.
          CLEAR: it_0301_vt.
          READ TABLE it_0301_nit WITH KEY docnum = it_0301_n01-docnum_nf.
          it_0301_vt-tknum            = it_0301_n01-tknum.
          it_0301_vt-auart_va	        = it_0301_n01-auart_va.
          it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
          it_0301_vt-zvlr_vi          = it_0301_n01-zvlr_vi.
          it_0301_vt-zvlr_mercadoria  = it_0301_n01-zvlr_mercadoria.

          "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
          IF it_0301_nit-ck_autorizado EQ abap_true.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_n01-ZESZVLR_FRETE.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-ZESZPESO_DIFERENCA.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-ZESZQUEBRA          = it_0301_nit-ZESZQUEBRA.
          it_0301_vt-ZESZPERDA           = it_0301_nit-ZESZPERDA.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-ZESZVLR_QUEBRA      = it_0301_n01-ZESZVLR_QUEBRA.
          it_0301_vt-ZESZVLR_PERDA	      = it_0301_n01-ZESZVLR_PERDA.
          it_0301_vt-ZESZVLR_LIQ_PAGAR	  = it_0301_n01-ZESZVLR_LIQ_PAGAR.
          it_0301_vt-ck_peso_digitado = it_0301_n01-ck_peso_digitado.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( ZESZIB_CTE_DIST_TER-ck_peso_chegada EQ abap_true ).
            it_0301_vt-ic_editar = icon_set_state.
          ELSE.
            it_0301_vt-ic_editar = icon_change_number.
          ENDIF.

          APPEND it_0301_vt.
        ENDLOOP.

        "Conhecimento de Transporte - Modal: Ferrovi├írio
        IF ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002'.
          IF ZESZIB_CTE_DIST_TER-dt_chegada IS INITIAL.
            ZESZIB_CTE_DIST_TER-dt_chegada = ZESZIB_CTE_DIST_TER-dt_emissao.
          ENDIF.
        ENDIF.

      WHEN 1. "CT-e de Complemento de Valores
        it_0301_vt-zvlr_vi          = 0.
        it_0301_vt-ZESZVLR_FRETE       = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
        it_0301_vt-zvlr_mercadoria  = 0.
        it_0301_vt-peso_origem      = 0.
        it_0301_vt-peso_chegada     = 0.
        it_0301_vt-ZESZPESO_DIFERENCA  = 0.
        it_0301_vt-zvlr_kg_transp   = 0.
        it_0301_vt-zvlr_kg_mercad   = 0.
        it_0301_vt-ZESZQUEBRA          = 0.
        it_0301_vt-ZESZPERDA           = 0.
        it_0301_vt-ZESZVLR_QUEBRA      = 0.
        it_0301_vt-ZESZVLR_PERDA       = 0.
        it_0301_vt-ZESZVLR_LIQ_PAGAR   = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
        it_0301_vt-pc_quebra        = 0.
        it_0301_vt-pc_tolerancia    = 0.
        APPEND it_0301_vt.
    ENDCASE.

    DESCRIBE TABLE it_0301_vt LINES gb_qtd_linhas.
    CLEAR: it_0301_vt.

    IF lc_ck_autorizados = abap_false AND ZESZIB_CTE_DIST_TER-cd_tipo_cte NE '1'.
      MESSAGE s107.
      gb_tela_block = abap_true.
    ENDIF.

    IF gb_qtd_linhas EQ 0.
      ADD 1 TO gb_index.
      MESSAGE s088.
    ELSEIF gb_qtd_linhas EQ 1.
      "Chama Tela com TV j├í Selecionada
      READ TABLE it_0301_vt INDEX 1.

      IF it_0301_vt-peso_chegada IS INITIAL AND ( ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ).
        it_0301_vt-peso_chegada = it_0301_vt-peso_origem.
      ENDIF.

      gb_index_ajuste = 1.
      PERFORM editar_0301_vt.
    ELSE.
      SORT it_0301_vt BY tknum.
      DELETE ADJACENT DUPLICATES FROM it_0301_vt COMPARING tknum.

      LOOP AT it_0301_vt ASSIGNING <fs_0301_vt> WHERE ck_peso_digitado EQ abap_false.
        <fs_0301_vt>-zvlr_mercadoria = 0.
        <fs_0301_vt>-peso_origem     = 0.

        IF ( ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ) AND ( <fs_0301_vt>-peso_chegada IS INITIAL ) .
          <fs_0301_vt>-peso_chegada  = 0.
          ck_primeiro_peso = abap_true.
        ELSE.
          ck_primeiro_peso = abap_false.
        ENDIF.

        LOOP AT it_0301_n55 WHERE tknum EQ <fs_0301_vt>-tknum.

          LOOP AT it_0301_nit WHERE docnum EQ it_0301_n55-docnum_nfe.
            ADD it_0301_n55-zvlr_mercadoria TO <fs_0301_vt>-zvlr_mercadoria.
            IF it_0301_nit-ck_autorizado EQ abap_true.
              ADD it_0301_nit-peso_origem_apro TO <fs_0301_vt>-peso_origem.
            ELSE.
              ADD it_0301_nit-peso_origem TO <fs_0301_vt>-peso_origem.
            ENDIF.
          ENDLOOP.

          IF ck_primeiro_peso EQ abap_true.
            ADD it_0301_nit-peso_origem TO <fs_0301_vt>-peso_chegada.
          ENDIF.

        ENDLOOP.
      ENDLOOP.

      DESCRIBE TABLE it_0301_vt LINES gb_qtd_linhas.

      IF gb_qtd_linhas EQ 1.
        "Chama Tela com TV j├í Selecionada
        READ TABLE it_0301_vt INDEX 1.
        gb_index_ajuste = 1.
        PERFORM editar_0301_vt.
      ELSE .
        CALL SCREEN 0301 STARTING AT 15 02.
      ENDIF.
      "Chama Tela para Selecionar a TV
    ENDIF.
    "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  ELSE.
    ADD 1 TO gb_index.
  ENDIF.

  p_index         = gb_index.
  p_ordem         = gb_ordem.

  IF gb_tela_block EQ abap_false.
    p_dt_vencimento = ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO.
  ENDIF.

  CLEAR: ZESZIB_CTE_DIST_TER, gb_ordem, gb_index.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_0301_VT
*&---------------------------------------------------------------------*
FORM editar_0301_vt.

  it_0301_vt-ck_peso_digitado = abap_false.

  "Buscar Peso de Chegada do Item Transportado.
  "IT_0301_VT-PESO_CHEGADA - Peso de Chegada
  "ZESZIB_CTE_DIST_TER-DT_CHEGADA  - Data de Chegada
  sy-subrc = 0.
  PERFORM busca_peso_chegada CHANGING it_0301_vt sy-subrc.

  IF sy-subrc IS INITIAL.
    gb_peso_chegada = abap_true.
  ELSE.
    gb_peso_chegada = abap_false.
  ENDIF.

  IF it_0301_vt-ZESZVLR_FRETE GT 0 AND it_0301_vt-peso_origem GT 0.
    it_0301_vt-zvlr_kg_transp = it_0301_vt-ZESZVLR_FRETE / it_0301_vt-peso_origem.
  ENDIF.

  "Se achar o peso informado calcular quebra e perda
  IF sy-subrc IS INITIAL AND gb_tela_block EQ abap_false.

    CALL METHOD zcl_cte_dist_g=>calcula_quebra_perda
      EXPORTING
        p_cod_mercadoria    = it_0301_vt-zmatnr_merc
        p_peso_origem       = it_0301_vt-peso_origem
        p_peso_destino      = it_0301_vt-peso_chegada
        p_vlr_frete         = it_0301_vt-ZESZVLR_FRETE
        p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
        p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
      IMPORTING
        e_peso_diferenca    = it_0301_vt-ZESZPESO_DIFERENCA
        e_peso_quebra       = it_0301_vt-ZESZQUEBRA
        e_peso_perda        = it_0301_vt-ZESZPERDA
        e_vlr_quebra        = it_0301_vt-ZESZVLR_QUEBRA
        e_vlr_perda         = it_0301_vt-ZESZVLR_PERDA
        e_vlr_liq_pagar     = it_0301_vt-ZESZVLR_LIQ_PAGAR
        e_pc_quebra         = it_0301_vt-pc_quebra
        e_pc_tolerancia     = it_0301_vt-pc_tolerancia.
  ENDIF.
  gb_ctr_altera_frete = abap_false.
  gb_ctr_calcul_frete = abap_false.

  IF gb_tela_block EQ abap_false.
    IF ZESZIB_CTE_DIST_TER-zbvtyp IS INITIAL.
      ZESZIB_CTE_DIST_TER-zbvtyp = '0001'.
    ENDIF.

    IF ZESZIB_CTE_DIST_TER-ZESZDT_MOV IS INITIAL.
      ZESZIB_CTE_DIST_TER-ZESZDT_MOV = sy-datum.
    ELSEIF ZESZIB_CTE_DIST_TER-ZESZDT_MOV IS NOT INITIAL AND ZESZIB_CTE_DIST_TER-belnr IS INITIAL.
      ZESZIB_CTE_DIST_TER-ZESZDT_MOV = sy-datum.
    ENDIF.
  ENDIF.

  ck_zbvtyp = abap_false.

  IF ZESZIB_CTE_DIST_TER-zbvtyp IS NOT INITIAL.
    CLEAR: wa_info_forne.
    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = wa_lfbk
        e_bnka     = wa_bnka
      CHANGING
        p_cte      = ZESZIB_CTE_DIST_TER
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      wa_info_forne-bvtyp = wa_lfbk-bvtyp.
      wa_info_forne-bankl = wa_bnka-bankl(3).
      wa_info_forne-banka = wa_bnka-banka.
      wa_info_forne-bankn = wa_lfbk-bankn.

      IF NOT wa_lfbk-bkont IS INITIAL.
        CONCATENATE wa_lfbk-bankl+4(11) '-' wa_lfbk-bkont INTO wa_info_forne-agenc.
      ELSE.
        wa_info_forne-agenc = wa_lfbk-bankl+4(11).
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: wa_info_forne.
  ENDIF.


  CALL SCREEN 0302 STARTING AT 30 10.


  IF gb_tela_block EQ abap_false.
    IF it_0301_vt-ck_peso_digitado EQ abap_true.
      IF it_0301_vt-ck_peso_digitado EQ abap_true.
        it_0301_vt-ic_editar = icon_set_state.
      ELSE.
        it_0301_vt-ic_editar = icon_change_number.
      ENDIF.
      MODIFY it_0301_vt INDEX gb_index_ajuste.

      "CT-e Possuir somente uma VT, e o usu├írio confirmou ou salvou, deve ser gravado
      IF gb_qtd_linhas EQ 1 AND gb_tela_block EQ abap_false.
        PERFORM salvar_informacoes_doc.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " EDITAR_0301_VT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_PESO_CHEGADA
*&---------------------------------------------------------------------*
*       Importa├º├úo de Peso Digitado
*----------------------------------------------------------------------*
FORM busca_peso_chegada  CHANGING p_digitar  TYPE ZESZDE_CTE_DIST_VT_ALV
                                  p_achou    TYPE sy-subrc.

  DATA: wa_0039 TYPE zlest0039.
  CLEAR:  wa_0039.

  CHECK gb_tela_block EQ abap_false.

*01	Rodovi├írio
*02	A├®reo
*03	Aquavi├írio
*04	Ferrovi├írio
*05	Dutovi├írio
  IF p_digitar-peso_chegada IS INITIAL AND ZESZIB_CTE_DIST_TER-dt_chegada IS INITIAL.
    p_achou = 4.
    CASE ZESZIB_CTE_DIST_TER-cd_modal.
      WHEN '01'.  "Rodovi├írio
        "Comparativo de saidas e chegadas
        SELECT SINGLE * INTO wa_0039 FROM zlest0039 WHERE docnum EQ p_digitar-docnum.

        IF sy-subrc IS INITIAL.
          IF wa_0039-pontotransb IS INITIAL.
            IF ( wa_0039-pesochegada IS NOT INITIAL ) AND ( wa_0039-datachegada IS NOT INITIAL ).
              p_digitar-peso_chegada       = wa_0039-pesochegada.
              ZESZIB_CTE_DIST_TER-dt_chegada  = wa_0039-datachegada.
              p_achou = 0.
            ELSE.
              p_achou = 4.
            ENDIF.
          ELSE.
            IF ( wa_0039-pesotransb IS NOT INITIAL ) AND ( wa_0039-datatransb IS NOT INITIAL ).
              p_digitar-peso_chegada       = wa_0039-pesotransb.
              ZESZIB_CTE_DIST_TER-dt_chegada  = wa_0039-datatransb.
              p_achou = 0.
            ELSE.
              p_achou = 4.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN '02'.  "A├®reo
      WHEN '03'.  "Aquavi├írio
      WHEN '04'.  "Ferrovi├írio
      WHEN '05'.  "Dutovi├írio
    ENDCASE.
  ENDIF.

ENDFORM.                    " BUSCA_PESO_CHEGADA

*&---------------------------------------------------------------------*
*&      Form  SALVAR_INFORMACOES_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_informacoes_doc .

  FIELD-SYMBOLS: <n55> TYPE ZESZIB_CTE_DIST_N55,
                 <n01> TYPE ZESZIB_CTE_DIST_N01,
                 <nit> TYPE ZESZIB_CTE_DIST_NIT.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anula├º├úo de Valores
*  3  CT-e Substituto
  CASE ZESZIB_CTE_DIST_TER-cd_tipo_cte.
    WHEN 0 OR 3. "0  CT-e Normal/CT-e Substituto

      CLEAR: it_0301_n55[], it_0301_n01[], it_0301_nit[].

      SELECT *
        INTO TABLE it_0301_n55
        FROM ZESZIB_CTE_DIST_N55
       WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM ZESZIB_CTE_DIST_NIT
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM ZESZIB_CTE_DIST_N01
       WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM ZESZIB_CTE_DIST_NIT
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      ZESZIB_CTE_DIST_TER-zvlr_vi         = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE      = 0.
      ZESZIB_CTE_DIST_TER-zvlr_mercadoria = 0.
      ZESZIB_CTE_DIST_TER-peso_origem     = 0.
      ZESZIB_CTE_DIST_TER-peso_chegada    = 0.
      ZESZIB_CTE_DIST_TER-ZESZPESO_DIFERENCA = 0.
      ZESZIB_CTE_DIST_TER-ZESZQUEBRA         = 0.
      ZESZIB_CTE_DIST_TER-ZESZPERDA          = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_QUEBRA     = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_PERDA      = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_LIQ_PAGAR  = 0.
      ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_true.

      LOOP AT it_0301_vt.
        ADD it_0301_vt-zvlr_vi         TO ZESZIB_CTE_DIST_TER-zvlr_vi.
        ADD it_0301_vt-ZESZVLR_FRETE      TO ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
        ADD it_0301_vt-zvlr_mercadoria TO ZESZIB_CTE_DIST_TER-zvlr_mercadoria.
        ADD it_0301_vt-ZESZPESO_DIFERENCA TO ZESZIB_CTE_DIST_TER-ZESZPESO_DIFERENCA.
        ADD it_0301_vt-ZESZQUEBRA         TO ZESZIB_CTE_DIST_TER-ZESZQUEBRA.
        ADD it_0301_vt-ZESZPERDA          TO ZESZIB_CTE_DIST_TER-ZESZPERDA.
        ADD it_0301_vt-ZESZVLR_QUEBRA     TO ZESZIB_CTE_DIST_TER-ZESZVLR_QUEBRA.
        ADD it_0301_vt-ZESZVLR_PERDA      TO ZESZIB_CTE_DIST_TER-ZESZVLR_PERDA.
        ADD it_0301_vt-ZESZVLR_LIQ_PAGAR  TO ZESZIB_CTE_DIST_TER-ZESZVLR_LIQ_PAGAR.
        ADD it_0301_vt-peso_chegada    TO ZESZIB_CTE_DIST_TER-peso_chegada.

        IF it_0301_vt-ck_autorizado EQ abap_false.
          ADD it_0301_vt-peso_origem  TO ZESZIB_CTE_DIST_TER-peso_origem.
        ENDIF.

        "Ajustando valores notas 55
        LOOP AT it_0301_n55 ASSIGNING <n55> WHERE tknum EQ it_0301_vt-tknum.
          <n55>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
          <n55>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n55>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
          <n55>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
          <n55>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR.
          <n55>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n55>-docnum_nfe.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.

            IF it_0301_vt-ck_autorizado EQ abap_false.
              <nit>-peso_origem    = it_0301_vt-peso_origem.
              <nit>-peso_chegada   = it_0301_vt-peso_chegada.
            ELSE.
              ADD <nit>-peso_origem TO ZESZIB_CTE_DIST_TER-peso_origem.
            ENDIF.

            <nit>-ZESZPESO_DIFERENCA  = it_0301_vt-ZESZPESO_DIFERENCA.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-ZESZQUEBRA          = it_0301_vt-ZESZQUEBRA.
            <nit>-ZESZPERDA           = it_0301_vt-ZESZPERDA.
            <nit>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
            <nit>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
            <nit>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
            IF it_0301_vt-ck_peso_digitado EQ abap_false.
              ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "Ajustando valores notas 01
        LOOP AT it_0301_n01 ASSIGNING <n01> WHERE tknum EQ it_0301_vt-tknum.
          <n01>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
          <n01>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n01>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
          <n01>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
          <n01>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR .
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n01>-docnum_nf.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
            <nit>-peso_origem      = it_0301_vt-peso_origem.
            <nit>-peso_chegada     = it_0301_vt-peso_chegada.
            <nit>-ZESZPESO_DIFERENCA  = it_0301_vt-ZESZPESO_DIFERENCA.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-ZESZQUEBRA          = it_0301_vt-ZESZQUEBRA.
            <nit>-ZESZPERDA           = it_0301_vt-ZESZPERDA.
            <nit>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
            <nit>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
            <nit>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
            IF it_0301_vt-ck_peso_digitado EQ abap_false.
              ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      ZESZIB_CTE_DIST_TER-zbase_icms    = 0.
      ZESZIB_CTE_DIST_TER-zbase_pis     = 0.
      ZESZIB_CTE_DIST_TER-zbase_cofins  = 0.
      ZESZIB_CTE_DIST_TER-zrate_icms    = 0.
      ZESZIB_CTE_DIST_TER-zrate_pis     = 0.
      ZESZIB_CTE_DIST_TER-zrate_cofins  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_icms   = 0.
      ZESZIB_CTE_DIST_TER-zvalor_pis    = 0.
      ZESZIB_CTE_DIST_TER-zvalor_cofins = 0.

      IF it_0301_n55[] IS NOT INITIAL.
        MODIFY ZESZIB_CTE_DIST_N55 FROM TABLE it_0301_n55.
      ENDIF.

      IF it_0301_n01[] IS NOT INITIAL.
        MODIFY ZESZIB_CTE_DIST_N01 FROM TABLE it_0301_n01.
      ENDIF.

      IF it_0301_nit[] IS NOT INITIAL.
        MODIFY ZESZIB_CTE_DIST_NIT FROM TABLE it_0301_nit.
      ENDIF.

    WHEN 1. "1  CT-e de Complemento de Valores
      ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_true.
  ENDCASE.

  "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
    FROM j_1bnflin AS l
   INNER JOIN ZESZIB_CTE_DIST_N55 AS n ON n~docnum_nfe EQ l~docnum
   WHERE n~cd_chave_cte EQ @ZESZIB_CTE_DIST_TER-cd_chave_cte
     AND n~docnum_nfe   NE @space.

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE l~matnr INTO lc_matnr
      FROM j_1bnflin AS l
     INNER JOIN ZESZIB_CTE_DIST_N01 AS n ON n~docnum_nf EQ l~docnum
     WHERE n~cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
       AND n~docnum_nf    NE space.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL METHOD zcl_cte_dist_g=>busca_impostos_taxas
    EXPORTING
      p_iva            = ZESZIB_CTE_DIST_TER-mwskz
      p_data_documento = ZESZIB_CTE_DIST_TER-dt_emissao
      p_shipfrom       = ZESZIB_CTE_DIST_TER-inicio_uf
      p_shipto         = ZESZIB_CTE_DIST_TER-termino_uf
      e_tomadora       = ZESZIB_CTE_DIST_TER-e_tomadora
      f_tomadora       = ZESZIB_CTE_DIST_TER-f_tomadora
      p_emissora       = ZESZIB_CTE_DIST_TER-p_emissor
      p_matnr          = lc_matnr
    IMPORTING
      e_rate_icms      = ZESZIB_CTE_DIST_TER-zrate_icms
      e_rate_pis       = ZESZIB_CTE_DIST_TER-zrate_pis
      e_rate_cofins    = ZESZIB_CTE_DIST_TER-zrate_cofins
    EXCEPTIONS
      sem_iva          = 1
      OTHERS           = 2.

  IF sy-subrc IS INITIAL.
    IF ZESZIB_CTE_DIST_TER-zrate_icms GT 0.
      ZESZIB_CTE_DIST_TER-zbase_icms  = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
      ZESZIB_CTE_DIST_TER-zvalor_icms = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE * ( ZESZIB_CTE_DIST_TER-zrate_icms / 100 ).
    ELSE.
      ZESZIB_CTE_DIST_TER-zbase_icms  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_icms = 0.
    ENDIF.

    DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE )
                                                                           i_valor_icms  =   CONV #( ZESZIB_CTE_DIST_TER-zvalor_icms ) ).

    IF ZESZIB_CTE_DIST_TER-zrate_pis GT 0.
      ZESZIB_CTE_DIST_TER-zbase_pis  = lva_base_calc_pis_cofins.
      ZESZIB_CTE_DIST_TER-zvalor_pis = lva_base_calc_pis_cofins * ( ZESZIB_CTE_DIST_TER-zrate_pis / 100 ).
    ELSE.
      ZESZIB_CTE_DIST_TER-zbase_pis  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_pis = 0.
    ENDIF.

    IF ZESZIB_CTE_DIST_TER-zrate_cofins GT 0.
      ZESZIB_CTE_DIST_TER-zbase_cofins  = lva_base_calc_pis_cofins.
      ZESZIB_CTE_DIST_TER-zvalor_cofins = lva_base_calc_pis_cofins * ( ZESZIB_CTE_DIST_TER-zrate_cofins / 100 ).
    ELSE.
      ZESZIB_CTE_DIST_TER-zbase_cofins  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_cofins = 0.
    ENDIF.
  ENDIF.

  MODIFY ZESZIB_CTE_DIST_TER.
  COMMIT WORK.

  MESSAGE s059.

ENDFORM.                    " SALVAR_INFORMACOES_DOC

*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.

  CLEAR: it_exclude_fcode.

  IF gb_index_ultimo EQ 1.
    APPEND ok_primeiro TO it_exclude_fcode.
    APPEND ok_anterior TO it_exclude_fcode.
    APPEND ok_proximo  TO it_exclude_fcode.
    APPEND ok_ultimo   TO it_exclude_fcode.
  ENDIF.

  IF gb_tela_block EQ abap_true.
    APPEND ok_salvar TO it_exclude_fcode.
  ENDIF.

  "IF GB_INDEX EQ 1.
  "  APPEND OK_PRIMEIRO TO IT_EXCLUDE_FCODE.
  "  APPEND OK_ANTERIOR TO IT_EXCLUDE_FCODE.
  "ELSEIF GB_INDEX EQ GB_INDEX_ULTIMO.
  "  APPEND OK_PROXIMO  TO IT_EXCLUDE_FCODE.
  "  APPEND OK_ULTIMO   TO IT_EXCLUDE_FCODE.
  "ENDIF.

  SET PF-STATUS 'PF0301' EXCLUDING it_exclude_fcode.
  SET TITLEBAR 'TL0301'.

  IF ctl_con_0301 IS INITIAL.

    CREATE OBJECT ctl_con_0301
      EXPORTING
        container_name = 'ALV_TKNUM'.

    CREATE OBJECT ctl_alv_0301
      EXPORTING
        i_parent = ctl_con_0301.

    PERFORM fill_it_fieldcatalog_0301.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0301.
*   Set layout parameters for ALV grid

    gs_lay_0301-sel_mode   = space.
    gs_lay_0301-zebra      = abap_true.

    CALL METHOD ctl_alv_0301->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0301
        is_variant           = gs_var_0301
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0301
      CHANGING
        it_fieldcatalog      = it_catalog_0301
        it_outtab            = it_0301_vt[].

    CALL METHOD ctl_alv_0301->refresh_table_display.

    CREATE OBJECT event_handler_0301.
    SET HANDLER event_handler_0301->handle_hotspot_click
            FOR ctl_alv_0301.

  ELSE.
    CALL METHOD ctl_alv_0301->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0301->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0301
      es_row_no   = gs_scroll_row_0301.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0301 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0301> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZESZDE_CTE_DIST_VT_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_0301.


  LOOP AT it_catalog_0301 ASSIGNING <fs_cat_0301>.
    CASE <fs_cat_0301>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_0301>-col_pos = 1.
    ENDCASE.
  ENDLOOP.

  lc_col_pos = 2.

  LOOP AT it_catalog_0301 ASSIGNING <fs_cat_0301>.
    IF <fs_cat_0301>-col_pos IS INITIAL.
      <fs_cat_0301>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.
    <fs_cat_0301>-tabname = 'IT_0301_VT'.
    CASE <fs_cat_0301>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_0301>-key     = abap_true.
        <fs_cat_0301>-hotspot = abap_true.
        <fs_cat_0301>-just    = 'C'.
      WHEN 'ZVLR_VI' OR 'ZESZVLR_FRETE' OR 'ZVLR_MERCADORIA' OR 'PESO_ORIGEM' OR 'PESO_CHEGADA' OR 'ZESZPESO_DIFERENCA' OR
           'ZESZQUEBRA' OR 'ZESZPERDA' OR 'ZESZVLR_QUEBRA' OR 'ZESZVLR_PERDA' OR 'ZESZVLR_LIQ_PAGAR'.
        <fs_cat_0301>-do_sum  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0301

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0301 .

  gs_var_0301-report      = sy-repid.
  gs_var_0301-handle      = '0301'.
  gs_var_0301-log_group   = abap_false.
  gs_var_0301-username    = abap_false.
  gs_var_0301-variant     = abap_false.
  gs_var_0301-text        = abap_false.
  gs_var_0301-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0301

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_INFORMACAO_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_informacao_frete INPUT.
  gb_ctr_calcul_frete = abap_false.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0302 INPUT.

  IF gb_ctr_alterou_peso EQ abap_true.
    ok_code = ok_verificar.
  ELSE.
    IF gb_ctr_altera_frete EQ abap_true.
      ok_code             = ok_verificar.
      gb_ctr_altera_frete = abap_false.
    ELSEIF gb_ctr_calcul_frete EQ abap_true.
      ok_code             = ok_confirmar.
    ENDIF.
  ENDIF.

  CASE ok_code.
    WHEN ok_confirmar OR ok_salvar.

      IF it_0301_vt-ZESZQUEBRA GT 0 AND it_0301_vt-ZESZVLR_QUEBRA EQ 0.
        MESSAGE s171 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      IF ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO LT sy-datum.
        MESSAGE s100 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      IF ck_peso_liberado NE abap_true.
        MESSAGE s169 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      IF ZESZIB_CTE_DIST_TER-mwskz IS INITIAL.
        MESSAGE s069 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      "Salvar caso somente exista um registro de VT na CT-e
      "Confirmar caso existe mais de um regsitro de VT na CT-e
      it_0301_vt-ck_peso_digitado = abap_true.

      IF gb_index NE gb_index_ultimo.
        ADD 1 TO gb_index.
      ELSE.
        IF gb_qtd_linhas EQ 1.
          gb_ordem = abap_true.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN ok_verificar.

      IF it_0301_vt-ZESZVLR_FRETE GT 0 AND it_0301_vt-peso_origem GT 0.
        it_0301_vt-zvlr_kg_transp = it_0301_vt-ZESZVLR_FRETE / it_0301_vt-peso_origem.
      ENDIF.

      CALL METHOD zcl_cte_dist_g=>calcula_quebra_perda
        EXPORTING
          p_cod_mercadoria    = it_0301_vt-zmatnr_merc
          p_peso_origem       = it_0301_vt-peso_origem
          p_peso_destino      = it_0301_vt-peso_chegada
          p_vlr_frete         = it_0301_vt-ZESZVLR_FRETE
          p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
          p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
        IMPORTING
          e_peso_diferenca    = it_0301_vt-ZESZPESO_DIFERENCA
          e_peso_quebra       = it_0301_vt-ZESZQUEBRA
          e_peso_perda        = it_0301_vt-ZESZPERDA
          e_vlr_quebra        = it_0301_vt-ZESZVLR_QUEBRA
          e_vlr_perda         = it_0301_vt-ZESZVLR_PERDA
          e_vlr_liq_pagar     = it_0301_vt-ZESZVLR_LIQ_PAGAR
          e_pc_quebra         = it_0301_vt-pc_quebra
          e_pc_tolerancia     = it_0301_vt-pc_tolerancia.

      gb_ctr_calcul_frete = abap_true.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0302 OUTPUT.

  CLEAR: it_exclude_fcode.

  IF gb_index_ultimo EQ 1.
    APPEND ok_primeiro TO it_exclude_fcode.
    APPEND ok_anterior TO it_exclude_fcode.
    APPEND ok_proximo  TO it_exclude_fcode.
    APPEND ok_ultimo   TO it_exclude_fcode.
  ENDIF.

  ck_peso_liberado = abap_true.

  IF gb_tela_block EQ abap_true.
    APPEND ok_salvar    TO it_exclude_fcode.
    APPEND ok_confirmar TO it_exclude_fcode.

    LOOP AT SCREEN.
      IF screen-input NE 0.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anula├º├úo de Valores
*  3  CT-e Substituto
    CASE ZESZIB_CTE_DIST_TER-cd_tipo_cte.
      WHEN 1. "1  CT-e de Complemento de Valores
        LOOP AT SCREEN.
          IF ( screen-input NE 0 ) AND
             ( "SCREEN-NAME EQ 'ZESZIB_CTE_DIST_TER-MWSKZ' OR
               screen-name EQ 'ZESZIB_CTE_DIST_TER-DT_CHEGADA' OR
               screen-name EQ 'IT_0301_VT-ZESZVLR_FRETE' OR
               screen-name EQ 'IT_0301_VT-PESO_ORIGEM' OR
               screen-name EQ 'IT_0301_VT-PESO_CHEGADA' ).
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    "Valores informados na autoriza├º├úo de pagamento
    IF it_0301_vt-ck_autorizado EQ abap_true.
      LOOP AT SCREEN.
        IF ( screen-input NE 0 ) AND
           ( screen-name EQ 'ZESZIB_CTE_DIST_TER-DT_CHEGADA' OR
             screen-name EQ 'IT_0301_VT-ZESZVLR_FRETE' OR
             screen-name EQ 'IT_0301_VT-PESO_ORIGEM' OR
             screen-name EQ 'IT_0301_VT-PESO_CHEGADA' ).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002'.
      LOOP AT SCREEN.
        IF ( screen-input NE 0 ) AND
           ( screen-name EQ 'ZESZIB_CTE_DIST_TER-DT_CHEGADA' OR
             screen-name EQ 'IT_0301_VT-PESO_CHEGADA' "OR
             "SCREEN-NAME EQ 'ZESZIB_CTE_DIST_TER-MWSKZ'
          ).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Verifica├º├úo de Peso de Chegada """"""""""""""""""""""""""""""""""""
    IF ZESZIB_CTE_DIST_TER-tp_processo_cte EQ '01' AND ( ZESZIB_CTE_DIST_TER-cd_modal = '01' AND e_tipo_contrato NE '0002' ) AND ZESZIB_CTE_DIST_TER-cd_tipo_cte NE '1'.

      IF it_0301_vt-auart_va EQ 'ZRFL' OR "Remessa Form. Lote.
         it_0301_vt-auart_va EQ 'ZRDC'.   "Rem Form Lote DCO

        ck_peso_manual   = abap_false.

        IF gb_peso_chegada EQ abap_false.
          ck_peso_liberado = abap_false.
        ELSE.
          ck_peso_liberado = abap_true.
        ENDIF.

        wa_zib_cte_dist_eap-tp_aprovacao  = '04'. "Peso/Data de Chegada
        wa_zib_cte_dist_eap-tp_autorizado = '01'. "Autorizado
        wa_zib_cte_dist_eap-cd_chave_cte  = ZESZIB_CTE_DIST_TER-cd_chave_cte.

        SELECT SINGLE * INTO wa_zib_cte_dist_eap
          FROM ZESZIB_CTE_DIST_EAP
         WHERE tp_aprovacao  EQ wa_zib_cte_dist_eap-tp_aprovacao
           AND cd_chave_cte  EQ wa_zib_cte_dist_eap-cd_chave_cte
           AND tp_autorizado EQ wa_zib_cte_dist_eap-tp_autorizado
           AND ck_ultimo     EQ abap_true.

        IF sy-subrc IS INITIAL.
          ck_peso_manual   = abap_true.
          ck_peso_liberado = abap_true.
        ENDIF.

        "N├úo autorizado peso/data chegada manual
        IF ck_peso_manual EQ abap_false.
          LOOP AT SCREEN.
            IF ( screen-input NE 0 ) AND ( screen-name EQ 'IT_0301_VT-PESO_CHEGADA' OR screen-name EQ 'ZESZIB_CTE_DIST_TER-DT_CHEGADA' ).
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      ck_peso_liberado = abap_true.
    ENDIF.
  ENDIF.

  "Se existir somente uma VT n├úo precisa confirmar
  IF gb_qtd_linhas EQ 1.
    APPEND ok_confirmar TO it_exclude_fcode.
  ELSE.
    APPEND ok_primeiro TO it_exclude_fcode.
    APPEND ok_anterior TO it_exclude_fcode.
    APPEND ok_proximo  TO it_exclude_fcode.
    APPEND ok_ultimo   TO it_exclude_fcode.
    APPEND ok_salvar   TO it_exclude_fcode.
  ENDIF.

  IF ck_zbvtyp EQ abap_true.
    CLEAR: wa_info_forne.
    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = wa_lfbk
        e_bnka     = wa_bnka
      CHANGING
        p_cte      = ZESZIB_CTE_DIST_TER
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      wa_info_forne-bvtyp = wa_lfbk-bvtyp.
      wa_info_forne-bankl = wa_bnka-bankl(3).
      wa_info_forne-banka = wa_bnka-banka.
      wa_info_forne-bankn = wa_lfbk-bankn.

      IF NOT wa_lfbk-bkont IS INITIAL.
        CONCATENATE wa_lfbk-bankl+4(11) '-' wa_lfbk-bkont INTO wa_info_forne-agenc.
      ELSE.
        wa_info_forne-agenc = wa_lfbk-bankl+4(11).
      ENDIF.
    ENDIF.
    ck_zbvtyp = abap_false.
  ELSE.
    CLEAR: wa_info_forne.
  ENDIF.

  SET PF-STATUS 'PF0302' EXCLUDING it_exclude_fcode.
  SET TITLEBAR 'TL0302' WITH it_0301_vt-tknum.
  gb_ctr_alterou_peso = abap_false.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PRIORIDADE_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_prioridade_banco INPUT.
  ck_zbvtyp = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301_exit INPUT.

  CASE ok_code.
    WHEN ok_primeiro.
      IF gb_index NE 1.
        gb_index = 1.
      ENDIF.
    WHEN ok_anterior.
      IF gb_index NE 1.
        ADD -1 TO gb_index.
      ENDIF.
    WHEN ok_proximo.
      IF gb_index NE gb_index_ultimo.
        ADD 1 TO gb_index.
      ENDIF.
    WHEN ok_ultimo.
      IF gb_index NE gb_index_ultimo.
        gb_index = gb_index_ultimo.
      ENDIF.
    WHEN ok_cancel.
      gb_ordem = abap_true.
  ENDCASE.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_DATA_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verificar_data_vencimento INPUT.

  CALL METHOD zcl_cte_dist_g=>verifica_vencimento_fatura
    EXPORTING
      i_data_vencimento = ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO
    EXCEPTIONS
      nao_valida        = 1
      OTHERS            = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PESO_CHEGADA_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_peso_chegada_frete INPUT.
  gb_ctr_alterou_peso = abap_true.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FATURAR_CTE_FERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_CTE  text
*      -->P_I_INDEX_ULTIMO  text
*      <--P_I_INDEX  text
*      <--P_I_DT_VENCIMENTO  text
*      <--P_E_ORDEM  text
*----------------------------------------------------------------------*
FORM faturar_cte_ferr  USING p_cte TYPE ZESZIB_CTE_DIST_TER
                        p_index_ultimo TYPE i
               CHANGING p_index TYPE i
                        p_dt_vencimento TYPE ZESZDT_VENCTO
                        p_ordem TYPE char01.

  DATA: ck_primeiro_peso TYPE char01.

  CLEAR: it_0301_dup[],
         it_0301_n55[],
         it_0301_n01[],
         it_0301_nit[],
         it_0301_vt[],
         e_tipo_contrato,
         wa_info_forne.

  FIELD-SYMBOLS: <fs_0301_vt> TYPE ZESZDE_CTE_DIST_VT_ALV.

  gb_index               = p_index.
  gb_index_ultimo        = p_index_ultimo.
  gb_tela_block          = abap_false.

  SELECT SINGLE * INTO ZESZIB_CTE_DIST_TER
    FROM ZESZIB_CTE_DIST_TER
   WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

  IF sy-subrc IS INITIAL.
    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    IF ZESZIB_CTE_DIST_TER-ck_finalizado EQ abap_true.
      gb_tela_block = abap_true.
    ENDIF.

    "Sugerir data de Vencimento
    IF ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO IS INITIAL AND gb_tela_block EQ abap_false.
      SELECT *
        INTO TABLE it_0301_dup
        FROM ZESZIB_CTE_DIST_DUP
       WHERE cd_chave_cte  EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
         AND dt_vencimento GE sy-datum
       ORDER BY dt_vencimento.

      IF sy-subrc IS INITIAL.
        READ TABLE it_0301_dup INDEX 1.
        ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO = it_0301_dup-dt_vencimento.
      ENDIF.
    ENDIF.

    IF ( p_dt_vencimento IS NOT INITIAL ) AND ( gb_tela_block EQ abap_false ).
      ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO = p_dt_vencimento.
    ELSEIF ( p_dt_vencimento IS INITIAL ) AND ( gb_tela_block EQ abap_false ) AND ( ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO IS INITIAL ).

      CALL METHOD zcl_cte_dist_g=>busca_proximo_venc_fatura
        IMPORTING
          e_data_vencimento = p_dt_vencimento.

      ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO = p_dt_vencimento.
    ENDIF.

    "0  CT-e Normal
    "1  CT-e de Complemento de Valores
    "2  CT-e de Anula├º├úo de Valores
    "3  CT-e Substituto

    CASE ZESZIB_CTE_DIST_TER-cd_tipo_cte.
      WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

        SELECT *
          INTO TABLE it_0301_n55
          FROM ZESZIB_CTE_DIST_N55
         WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
           AND tknum        NE space
           AND docnum_nfe   NE space.

        IF it_0301_n55[] IS NOT INITIAL.

          LOOP AT it_0301_n55 INTO DATA(wa_n55).

            IF wa_n55-docnum_nfe IS INITIAL.
              CONTINUE.
            ENDIF.

            SELECT SINGLE * INTO @DATA(wa_itens)
              FROM j_1bnflin
             WHERE docnum EQ @wa_n55-docnum_nfe.

            IF sy-subrc IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria est├í parametrizado por frete lota├º├úo
            SELECT SINGLE * INTO @DATA(wa_zlest0154)
              FROM zlest0154
             WHERE bukrs EQ @ZESZIB_CTE_DIST_TER-e_tomadora
               AND lifnr EQ @ZESZIB_CTE_DIST_TER-p_emissor
               AND matkl EQ @wa_itens-matkl.

            IF sy-subrc IS INITIAL.
              e_tipo_contrato = '0002'.
            ENDIF.

          ENDLOOP.

          SELECT *
            INTO TABLE it_0301_nit
            FROM ZESZIB_CTE_DIST_NIT
             FOR ALL ENTRIES IN it_0301_n55
           WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
             AND docnum       EQ it_0301_n55-docnum_nfe.
        ENDIF.

        SELECT *
          INTO TABLE it_0301_n01
          FROM ZESZIB_CTE_DIST_N01
         WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
           AND tknum        NE space
           AND docnum_nf    NE space.

        IF it_0301_n01[] IS NOT INITIAL.
          SELECT *
            APPENDING TABLE it_0301_nit
            FROM ZESZIB_CTE_DIST_NIT
             FOR ALL ENTRIES IN it_0301_n01
           WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
             AND docnum       EQ it_0301_n01-docnum_nf.
        ENDIF.

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "" Verfica Autoriza├º├úo de Pagamento """""""""""""""""""""""""""""""""""""""""""
        DATA: lc_ck_autorizados TYPE c LENGTH 1,
              lc_matnr          TYPE matnr,
              lc_grupo          TYPE matkl,
              lc_tipo           TYPE ZESZDE_TP_AUT_FRETE.

        lc_ck_autorizados = abap_true.

        LOOP AT it_0301_nit.

          SELECT SINGLE matnr INTO lc_matnr
            FROM j_1bnflin
           WHERE docnum EQ it_0301_nit-docnum
             AND itmnum EQ it_0301_nit-itmnum.

          SELECT SINGLE matkl INTO lc_grupo
            FROM mara
           WHERE matnr EQ lc_matnr.

          SELECT SINGLE tp_aut_frete
            INTO lc_tipo
            FROM ZESZIB_CTE_DIST_GM
           WHERE matkl EQ lc_grupo.

          IF ( sy-subrc IS INITIAL ) AND ( it_0301_nit-ck_autorizado NE abap_true ) AND ( ZESZIB_CTE_DIST_TER-cd_modal NE '04' ) AND ( e_tipo_contrato NE '0002' ).
            lc_ck_autorizados = abap_false.
          ENDIF.
        ENDLOOP.
        "" Verfica Autoriza├º├úo de Pagamento """""""""""""""""""""""""""""""""""""""""""
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        LOOP AT it_0301_n55.
          CLEAR: it_0301_vt.
          READ TABLE it_0301_nit WITH KEY docnum = it_0301_n55-docnum_nfe.
          it_0301_vt-tknum            = it_0301_n55-tknum.
          it_0301_vt-auart_va         = it_0301_n55-auart_va.
          it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
          it_0301_vt-zvlr_vi          = it_0301_n55-zvlr_vi.
          it_0301_vt-zvlr_mercadoria  = it_0301_n55-zvlr_mercadoria.

          "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
          IF it_0301_nit-ck_autorizado EQ abap_true.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_n55-ZESZVLR_FRETE.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-ZESZPESO_DIFERENCA.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-ZESZQUEBRA          = it_0301_nit-ZESZQUEBRA.
          it_0301_vt-ZESZPERDA           = it_0301_nit-ZESZPERDA.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-ZESZVLR_QUEBRA      = it_0301_n55-ZESZVLR_QUEBRA.
          it_0301_vt-ZESZVLR_PERDA	      = it_0301_n55-ZESZVLR_PERDA.
          it_0301_vt-ZESZVLR_LIQ_PAGAR	  = it_0301_n55-ZESZVLR_LIQ_PAGAR.
          it_0301_vt-ck_peso_digitado = it_0301_n55-ck_peso_digitado.
          it_0301_vt-docnum           = it_0301_nit-docnum.
          it_0301_vt-itmnum           = it_0301_nit-itmnum.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( ZESZIB_CTE_DIST_TER-ck_peso_chegada EQ abap_true ).
            it_0301_vt-ic_editar = icon_set_state.
          ELSE.
            it_0301_vt-ic_editar = icon_change_number.
          ENDIF.
          APPEND it_0301_vt.
        ENDLOOP.

        LOOP AT it_0301_n01.
          CLEAR: it_0301_vt.
          READ TABLE it_0301_nit WITH KEY docnum = it_0301_n01-docnum_nf.
          it_0301_vt-tknum            = it_0301_n01-tknum.
          it_0301_vt-auart_va	        = it_0301_n01-auart_va.
          it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
          it_0301_vt-zvlr_vi          = it_0301_n01-zvlr_vi.
          it_0301_vt-zvlr_mercadoria  = it_0301_n01-zvlr_mercadoria.

          "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
          IF it_0301_nit-ck_autorizado EQ abap_true.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-ZESZVLR_FRETE	      = it_0301_n01-ZESZVLR_FRETE.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-ZESZPESO_DIFERENCA  = it_0301_nit-ZESZPESO_DIFERENCA.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-ZESZQUEBRA          = it_0301_nit-ZESZQUEBRA.
          it_0301_vt-ZESZPERDA           = it_0301_nit-ZESZPERDA.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-ZESZVLR_QUEBRA      = it_0301_n01-ZESZVLR_QUEBRA.
          it_0301_vt-ZESZVLR_PERDA	      = it_0301_n01-ZESZVLR_PERDA.
          it_0301_vt-ZESZVLR_LIQ_PAGAR	  = it_0301_n01-ZESZVLR_LIQ_PAGAR.
          it_0301_vt-ck_peso_digitado = it_0301_n01-ck_peso_digitado.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( ZESZIB_CTE_DIST_TER-ck_peso_chegada EQ abap_true ).
            it_0301_vt-ic_editar = icon_set_state.
          ELSE.
            it_0301_vt-ic_editar = icon_change_number.
          ENDIF.

          APPEND it_0301_vt.
        ENDLOOP.

        "Conhecimento de Transporte - Modal: Ferrovi├írio
        IF ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002'.
          IF ZESZIB_CTE_DIST_TER-dt_chegada IS INITIAL.
            ZESZIB_CTE_DIST_TER-dt_chegada = ZESZIB_CTE_DIST_TER-dt_emissao.
          ENDIF.
        ENDIF.

      WHEN 1. "CT-e de Complemento de Valores
        it_0301_vt-zvlr_vi          = 0.
        it_0301_vt-ZESZVLR_FRETE       = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
        it_0301_vt-zvlr_mercadoria  = 0.
        it_0301_vt-peso_origem      = 0.
        it_0301_vt-peso_chegada     = 0.
        it_0301_vt-ZESZPESO_DIFERENCA  = 0.
        it_0301_vt-zvlr_kg_transp   = 0.
        it_0301_vt-zvlr_kg_mercad   = 0.
        it_0301_vt-ZESZQUEBRA          = 0.
        it_0301_vt-ZESZPERDA           = 0.
        it_0301_vt-ZESZVLR_QUEBRA      = 0.
        it_0301_vt-ZESZVLR_PERDA       = 0.
        it_0301_vt-ZESZVLR_LIQ_PAGAR   = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
        it_0301_vt-pc_quebra        = 0.
        it_0301_vt-pc_tolerancia    = 0.
        APPEND it_0301_vt.
    ENDCASE.

    DESCRIBE TABLE it_0301_vt LINES gb_qtd_linhas.
    CLEAR: it_0301_vt.

    IF lc_ck_autorizados = abap_false AND ZESZIB_CTE_DIST_TER-cd_tipo_cte NE '1'.
      MESSAGE s107.
      gb_tela_block = abap_true.
    ENDIF.

    IF gb_qtd_linhas EQ 0.
      ADD 1 TO gb_index.
      MESSAGE s088.
    ELSEIF gb_qtd_linhas EQ 1.
      "Chama Tela com TV j├í Selecionada
      READ TABLE it_0301_vt INDEX 1.

      IF it_0301_vt-peso_chegada IS INITIAL AND ( ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ).
        it_0301_vt-peso_chegada = it_0301_vt-peso_origem.
      ENDIF.

      gb_index_ajuste = 1.
      PERFORM editar_0301_vt_ferr.

    ELSE.
      SORT it_0301_vt BY tknum.
      DELETE ADJACENT DUPLICATES FROM it_0301_vt COMPARING tknum.

      LOOP AT it_0301_vt ASSIGNING <fs_0301_vt> WHERE ck_peso_digitado EQ abap_false.
        <fs_0301_vt>-zvlr_mercadoria = 0.
        <fs_0301_vt>-peso_origem     = 0.

        IF ( ZESZIB_CTE_DIST_TER-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ) AND ( <fs_0301_vt>-peso_chegada IS INITIAL ) .
          <fs_0301_vt>-peso_chegada  = 0.
          ck_primeiro_peso = abap_true.
        ELSE.
          ck_primeiro_peso = abap_false.
        ENDIF.

        LOOP AT it_0301_n55 WHERE tknum EQ <fs_0301_vt>-tknum.

          LOOP AT it_0301_nit WHERE docnum EQ it_0301_n55-docnum_nfe.
            ADD it_0301_n55-zvlr_mercadoria TO <fs_0301_vt>-zvlr_mercadoria.
            IF it_0301_nit-ck_autorizado EQ abap_true.
              ADD it_0301_nit-peso_origem_apro TO <fs_0301_vt>-peso_origem.
            ELSE.
              ADD it_0301_nit-peso_origem TO <fs_0301_vt>-peso_origem.
            ENDIF.
          ENDLOOP.

          IF ck_primeiro_peso EQ abap_true.
            ADD it_0301_nit-peso_origem TO <fs_0301_vt>-peso_chegada.
          ENDIF.

        ENDLOOP.
      ENDLOOP.

      DESCRIBE TABLE it_0301_vt LINES gb_qtd_linhas.

      IF gb_qtd_linhas EQ 1.
        "Chama Tela com TV j├í Selecionada
        READ TABLE it_0301_vt INDEX 1.
        gb_index_ajuste = 1.
        PERFORM editar_0301_vt_ferr.
      ELSE.
        FREE: it_0301_vt_aux.
        it_0301_vt_aux = it_0301_vt.

        FREE: it_0301_vt.

        LOOP AT it_0301_vt_aux.
          APPEND  it_0301_vt_aux TO it_0301_vt.
          PERFORM editar_0301_vt_ferr.

          FREE: it_0301_vt[].
          CLEAR: it_0301_vt.
        ENDLOOP.

*        CALL SCREEN 0301 STARTING AT 15 02.
      ENDIF.
      "Chama Tela para Selecionar a TV
    ENDIF.
    "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  ELSE.
    ADD 1 TO gb_index.
  ENDIF.

  p_index         = gb_index.
  p_ordem         = gb_ordem.

  IF gb_tela_block EQ abap_false.
    p_dt_vencimento = ZESZIB_CTE_DIST_TER-ZESZDT_VENCTO.
  ENDIF.

  CLEAR: ZESZIB_CTE_DIST_TER, gb_ordem, gb_index.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EDITAR_0301_VT_FERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM editar_0301_vt_ferr .

  it_0301_vt-ck_peso_digitado = abap_false.

  "Buscar Peso de Chegada do Item Transportado.
  "IT_0301_VT-PESO_CHEGADA - Peso de Chegada
  "ZESZIB_CTE_DIST_TER-DT_CHEGADA  - Data de Chegada
  sy-subrc = 0.
  PERFORM busca_peso_chegada CHANGING it_0301_vt sy-subrc.

  IF sy-subrc IS INITIAL.
    gb_peso_chegada = abap_true.
  ELSE.
    gb_peso_chegada = abap_false.
  ENDIF.

  IF it_0301_vt-ZESZVLR_FRETE GT 0 AND it_0301_vt-peso_origem GT 0.
    it_0301_vt-zvlr_kg_transp = it_0301_vt-ZESZVLR_FRETE / it_0301_vt-peso_origem.
  ENDIF.

  "Se achar o peso informado calcular quebra e perda
  IF sy-subrc IS INITIAL AND gb_tela_block EQ abap_false.

    CALL METHOD zcl_cte_dist_g=>calcula_quebra_perda
      EXPORTING
        p_cod_mercadoria    = it_0301_vt-zmatnr_merc
        p_peso_origem       = it_0301_vt-peso_origem
        p_peso_destino      = it_0301_vt-peso_chegada
        p_vlr_frete         = it_0301_vt-ZESZVLR_FRETE
        p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
        p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
      IMPORTING
        e_peso_diferenca    = it_0301_vt-ZESZPESO_DIFERENCA
        e_peso_quebra       = it_0301_vt-ZESZQUEBRA
        e_peso_perda        = it_0301_vt-ZESZPERDA
        e_vlr_quebra        = it_0301_vt-ZESZVLR_QUEBRA
        e_vlr_perda         = it_0301_vt-ZESZVLR_PERDA
        e_vlr_liq_pagar     = it_0301_vt-ZESZVLR_LIQ_PAGAR
        e_pc_quebra         = it_0301_vt-pc_quebra
        e_pc_tolerancia     = it_0301_vt-pc_tolerancia.
  ENDIF.
  gb_ctr_altera_frete = abap_false.
  gb_ctr_calcul_frete = abap_false.

  IF gb_tela_block EQ abap_false.
    IF ZESZIB_CTE_DIST_TER-zbvtyp IS INITIAL.
      ZESZIB_CTE_DIST_TER-zbvtyp = '0001'.
    ENDIF.

    IF ZESZIB_CTE_DIST_TER-ZESZDT_MOV IS INITIAL.
      ZESZIB_CTE_DIST_TER-ZESZDT_MOV = sy-datum.
    ELSEIF ZESZIB_CTE_DIST_TER-ZESZDT_MOV IS NOT INITIAL AND ZESZIB_CTE_DIST_TER-belnr IS INITIAL.
      ZESZIB_CTE_DIST_TER-ZESZDT_MOV = sy-datum.
    ENDIF.
  ENDIF.

  ck_zbvtyp = abap_false.

  IF ZESZIB_CTE_DIST_TER-zbvtyp IS NOT INITIAL.
    CLEAR: wa_info_forne.
    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = wa_lfbk
        e_bnka     = wa_bnka
      CHANGING
        p_cte      = ZESZIB_CTE_DIST_TER
      EXCEPTIONS
        erro_banco = 1
        OTHERS     = 2.

    IF sy-subrc IS INITIAL.
      wa_info_forne-bvtyp = wa_lfbk-bvtyp.
      wa_info_forne-bankl = wa_bnka-bankl(3).
      wa_info_forne-banka = wa_bnka-banka.
      wa_info_forne-bankn = wa_lfbk-bankn.

      IF NOT wa_lfbk-bkont IS INITIAL.
        CONCATENATE wa_lfbk-bankl+4(11) '-' wa_lfbk-bkont INTO wa_info_forne-agenc.
      ELSE.
        wa_info_forne-agenc = wa_lfbk-bankl+4(11).
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: wa_info_forne.
  ENDIF.

*  IF gb_tela_block EQ abap_false.
*    IF it_0301_vt-ck_peso_digitado EQ abap_true.
*      IF it_0301_vt-ck_peso_digitado EQ abap_true.
*        it_0301_vt-ic_editar = icon_set_state.
*      ELSE.
*        it_0301_vt-ic_editar = icon_change_number.
*      ENDIF.
  MODIFY it_0301_vt INDEX gb_index_ajuste.

  "CT-e Possuir somente uma VT, e o usu├írio confirmou ou salvou, deve ser gravado
  IF gb_qtd_linhas EQ 1 AND gb_tela_block EQ abap_false.
    PERFORM salvar_informacoes_doc_ferr.
  ENDIF.
*    ENDIF.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_INFORMACOES_DOC_FERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_informacoes_doc_ferr .

  FIELD-SYMBOLS: <n55> TYPE ZESZIB_CTE_DIST_N55,
                 <n01> TYPE ZESZIB_CTE_DIST_N01,
                 <nit> TYPE ZESZIB_CTE_DIST_NIT.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anula├º├úo de Valores
*  3  CT-e Substituto
  CASE ZESZIB_CTE_DIST_TER-cd_tipo_cte.
    WHEN 0 OR 3. "0  CT-e Normal/CT-e Substituto

      CLEAR: it_0301_n55[], it_0301_n01[], it_0301_nit[].

      SELECT *
        INTO TABLE it_0301_n55
        FROM ZESZIB_CTE_DIST_N55
       WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM ZESZIB_CTE_DIST_NIT
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM ZESZIB_CTE_DIST_N01
       WHERE cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM ZESZIB_CTE_DIST_NIT
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      ZESZIB_CTE_DIST_TER-zvlr_vi         = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE      = 0.
      ZESZIB_CTE_DIST_TER-zvlr_mercadoria = 0.
      ZESZIB_CTE_DIST_TER-peso_origem     = 0.
      ZESZIB_CTE_DIST_TER-peso_chegada    = 0.
      ZESZIB_CTE_DIST_TER-ZESZPESO_DIFERENCA = 0.
      ZESZIB_CTE_DIST_TER-ZESZQUEBRA         = 0.
      ZESZIB_CTE_DIST_TER-ZESZPERDA          = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_QUEBRA     = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_PERDA      = 0.
      ZESZIB_CTE_DIST_TER-ZESZVLR_LIQ_PAGAR  = 0.
      ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_true.

      LOOP AT it_0301_vt.
        ADD it_0301_vt-zvlr_vi         TO ZESZIB_CTE_DIST_TER-zvlr_vi.
        ADD it_0301_vt-ZESZVLR_FRETE      TO ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
        ADD it_0301_vt-zvlr_mercadoria TO ZESZIB_CTE_DIST_TER-zvlr_mercadoria.
        ADD it_0301_vt-ZESZPESO_DIFERENCA TO ZESZIB_CTE_DIST_TER-ZESZPESO_DIFERENCA.
        ADD it_0301_vt-ZESZQUEBRA         TO ZESZIB_CTE_DIST_TER-ZESZQUEBRA.
        ADD it_0301_vt-ZESZPERDA          TO ZESZIB_CTE_DIST_TER-ZESZPERDA.
        ADD it_0301_vt-ZESZVLR_QUEBRA     TO ZESZIB_CTE_DIST_TER-ZESZVLR_QUEBRA.
        ADD it_0301_vt-ZESZVLR_PERDA      TO ZESZIB_CTE_DIST_TER-ZESZVLR_PERDA.
        ADD it_0301_vt-ZESZVLR_LIQ_PAGAR  TO ZESZIB_CTE_DIST_TER-ZESZVLR_LIQ_PAGAR.
        ADD it_0301_vt-peso_chegada    TO ZESZIB_CTE_DIST_TER-peso_chegada.

        IF it_0301_vt-ck_autorizado EQ abap_false.
          ADD it_0301_vt-peso_origem  TO ZESZIB_CTE_DIST_TER-peso_origem.
        ENDIF.

        "Ajustando valores notas 55
        LOOP AT it_0301_n55 ASSIGNING <n55> WHERE tknum EQ it_0301_vt-tknum.
          <n55>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
          <n55>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n55>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
          <n55>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
          <n55>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR.
          <n55>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n55>-docnum_nfe.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.

            IF it_0301_vt-ck_autorizado EQ abap_false.
              <nit>-peso_origem    = it_0301_vt-peso_origem.
              <nit>-peso_chegada   = it_0301_vt-peso_chegada.
            ELSE.
              ADD <nit>-peso_origem TO ZESZIB_CTE_DIST_TER-peso_origem.
            ENDIF.

            <nit>-ZESZPESO_DIFERENCA  = it_0301_vt-ZESZPESO_DIFERENCA.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-ZESZQUEBRA          = it_0301_vt-ZESZQUEBRA.
            <nit>-ZESZPERDA           = it_0301_vt-ZESZPERDA.
            <nit>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
            <nit>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
            <nit>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*            IF it_0301_vt-ck_peso_digitado EQ abap_false.
*              ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_false.
*            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "Ajustando valores notas 01
        LOOP AT it_0301_n01 ASSIGNING <n01> WHERE tknum EQ it_0301_vt-tknum.
          <n01>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
          <n01>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n01>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
          <n01>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
          <n01>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR .
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n01>-docnum_nf.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-ZESZVLR_FRETE       = it_0301_vt-ZESZVLR_FRETE.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
            <nit>-peso_origem      = it_0301_vt-peso_origem.
            <nit>-peso_chegada     = it_0301_vt-peso_chegada.
            <nit>-ZESZPESO_DIFERENCA  = it_0301_vt-ZESZPESO_DIFERENCA.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-ZESZQUEBRA          = it_0301_vt-ZESZQUEBRA.
            <nit>-ZESZPERDA           = it_0301_vt-ZESZPERDA.
            <nit>-ZESZVLR_QUEBRA      = it_0301_vt-ZESZVLR_QUEBRA.
            <nit>-ZESZVLR_PERDA       = it_0301_vt-ZESZVLR_PERDA.
            <nit>-ZESZVLR_LIQ_PAGAR   = it_0301_vt-ZESZVLR_LIQ_PAGAR.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*            IF it_0301_vt-ck_peso_digitado EQ abap_false.
*              ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_false.
*            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      ZESZIB_CTE_DIST_TER-zbase_icms    = 0.
      ZESZIB_CTE_DIST_TER-zbase_pis     = 0.
      ZESZIB_CTE_DIST_TER-zbase_cofins  = 0.
      ZESZIB_CTE_DIST_TER-zrate_icms    = 0.
      ZESZIB_CTE_DIST_TER-zrate_pis     = 0.
      ZESZIB_CTE_DIST_TER-zrate_cofins  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_icms   = 0.
      ZESZIB_CTE_DIST_TER-zvalor_pis    = 0.
      ZESZIB_CTE_DIST_TER-zvalor_cofins = 0.

      IF it_0301_n55[] IS NOT INITIAL.
        MODIFY ZESZIB_CTE_DIST_N55 FROM TABLE it_0301_n55.
      ENDIF.

      IF it_0301_n01[] IS NOT INITIAL.
        MODIFY ZESZIB_CTE_DIST_N01 FROM TABLE it_0301_n01.
      ENDIF.

      IF it_0301_nit[] IS NOT INITIAL.
        MODIFY ZESZIB_CTE_DIST_NIT FROM TABLE it_0301_nit.
      ENDIF.

    WHEN 1. "1  CT-e de Complemento de Valores
      ZESZIB_CTE_DIST_TER-ck_peso_chegada = abap_true.
  ENDCASE.

  "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
    FROM j_1bnflin AS l
   INNER JOIN ZESZIB_CTE_DIST_N55 AS n ON n~docnum_nfe EQ l~docnum
   WHERE n~cd_chave_cte EQ @ZESZIB_CTE_DIST_TER-cd_chave_cte
     AND n~docnum_nfe   NE @space.

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE l~matnr INTO lc_matnr
      FROM j_1bnflin AS l
     INNER JOIN ZESZIB_CTE_DIST_N01 AS n ON n~docnum_nf EQ l~docnum
     WHERE n~cd_chave_cte EQ ZESZIB_CTE_DIST_TER-cd_chave_cte
       AND n~docnum_nf    NE space.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL METHOD zcl_cte_dist_g=>busca_impostos_taxas
    EXPORTING
      p_iva            = ZESZIB_CTE_DIST_TER-mwskz
      p_data_documento = ZESZIB_CTE_DIST_TER-dt_emissao
      p_shipfrom       = ZESZIB_CTE_DIST_TER-inicio_uf
      p_shipto         = ZESZIB_CTE_DIST_TER-termino_uf
      e_tomadora       = ZESZIB_CTE_DIST_TER-e_tomadora
      f_tomadora       = ZESZIB_CTE_DIST_TER-f_tomadora
      p_emissora       = ZESZIB_CTE_DIST_TER-p_emissor
      p_matnr          = lc_matnr
    IMPORTING
      e_rate_icms      = ZESZIB_CTE_DIST_TER-zrate_icms
      e_rate_pis       = ZESZIB_CTE_DIST_TER-zrate_pis
      e_rate_cofins    = ZESZIB_CTE_DIST_TER-zrate_cofins
    EXCEPTIONS
      sem_iva          = 1
      OTHERS           = 2.

  IF sy-subrc IS INITIAL.
    IF ZESZIB_CTE_DIST_TER-zrate_icms GT 0.
      ZESZIB_CTE_DIST_TER-zbase_icms  = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE.
      ZESZIB_CTE_DIST_TER-zvalor_icms = ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE * ( ZESZIB_CTE_DIST_TER-zrate_icms / 100 ).
    ELSE.
      ZESZIB_CTE_DIST_TER-zbase_icms  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_icms = 0.
    ENDIF.

    DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( ZESZIB_CTE_DIST_TER-ZESZVLR_FRETE )
                                                                           i_valor_icms  =   CONV #( ZESZIB_CTE_DIST_TER-zvalor_icms ) ).

    IF ZESZIB_CTE_DIST_TER-zrate_pis GT 0.
      ZESZIB_CTE_DIST_TER-zbase_pis  = lva_base_calc_pis_cofins.
      ZESZIB_CTE_DIST_TER-zvalor_pis = lva_base_calc_pis_cofins * ( ZESZIB_CTE_DIST_TER-zrate_pis / 100 ).
    ELSE.
      ZESZIB_CTE_DIST_TER-zbase_pis  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_pis = 0.
    ENDIF.

    IF ZESZIB_CTE_DIST_TER-zrate_cofins GT 0.
      ZESZIB_CTE_DIST_TER-zbase_cofins  = lva_base_calc_pis_cofins.
      ZESZIB_CTE_DIST_TER-zvalor_cofins = lva_base_calc_pis_cofins * ( ZESZIB_CTE_DIST_TER-zrate_cofins / 100 ).
    ELSE.
      ZESZIB_CTE_DIST_TER-zbase_cofins  = 0.
      ZESZIB_CTE_DIST_TER-zvalor_cofins = 0.
    ENDIF.
  ENDIF.

  MODIFY ZESZIB_CTE_DIST_TER.
  COMMIT WORK.

ENDFORM.
