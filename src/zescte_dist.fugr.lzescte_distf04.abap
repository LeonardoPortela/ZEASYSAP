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

DATA: it_0301_dup TYPE TABLE OF zib_cte_dist_dup    WITH HEADER LINE,
      it_0301_n55 TYPE TABLE OF zib_cte_dist_n55    WITH HEADER LINE,
      it_0301_n01 TYPE TABLE OF zib_cte_dist_n01    WITH HEADER LINE,
      it_0301_nit TYPE TABLE OF zib_cte_dist_nit    WITH HEADER LINE.

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
      wa_zib_cte_dist_eap TYPE zib_cte_dist_eap,
      e_tipo_contrato     TYPE char04,
      it_exclude_fcode    TYPE TABLE OF sy-ucomm,
      it_0301_vt          TYPE TABLE OF zde_cte_dist_vt_alv WITH HEADER LINE,
      it_0301_vt_aux      TYPE TABLE OF zde_cte_dist_vt_alv WITH HEADER LINE.


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
FORM faturar_cte  USING p_cte TYPE zib_cte_dist_ter
                        p_index_ultimo TYPE i
               CHANGING p_index TYPE i
                        p_dt_vencimento TYPE zdt_vencto
                        p_ordem TYPE char01.

  DATA: ck_primeiro_peso TYPE char01.

  CLEAR: it_0301_dup[],
         it_0301_n55[],
         it_0301_n01[],
         it_0301_nit[],
         it_0301_vt[],
         e_tipo_contrato,
         wa_info_forne.

  FIELD-SYMBOLS: <fs_0301_vt> TYPE zde_cte_dist_vt_alv.

  gb_index               = p_index.
  gb_index_ultimo        = p_index_ultimo.
  gb_tela_block          = abap_false.

  SELECT SINGLE * INTO zib_cte_dist_ter
    FROM zib_cte_dist_ter
   WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

  IF sy-subrc IS INITIAL.
    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    IF zib_cte_dist_ter-ck_finalizado EQ abap_true.
      gb_tela_block = abap_true.
    ENDIF.

    "Sugerir data de Vencimento
    IF zib_cte_dist_ter-zdt_vencto IS INITIAL AND gb_tela_block EQ abap_false.
      SELECT *
        INTO TABLE it_0301_dup
        FROM zib_cte_dist_dup
       WHERE cd_chave_cte  EQ zib_cte_dist_ter-cd_chave_cte
         AND dt_vencimento GE sy-datum
       ORDER BY dt_vencimento.

      IF sy-subrc IS INITIAL.
        READ TABLE it_0301_dup INDEX 1.
        zib_cte_dist_ter-zdt_vencto = it_0301_dup-dt_vencimento.
      ENDIF.
    ENDIF.

    IF ( p_dt_vencimento IS NOT INITIAL ) AND ( gb_tela_block EQ abap_false ).
      zib_cte_dist_ter-zdt_vencto = p_dt_vencimento.
    ELSEIF ( p_dt_vencimento IS INITIAL ) AND ( gb_tela_block EQ abap_false ) AND ( zib_cte_dist_ter-zdt_vencto IS INITIAL ).

      CALL METHOD zcl_cte_dist_g=>busca_proximo_venc_fatura
        IMPORTING
          e_data_vencimento = p_dt_vencimento.

      zib_cte_dist_ter-zdt_vencto = p_dt_vencimento.
    ENDIF.

    "0  CT-e Normal
    "1  CT-e de Complemento de Valores
    "2  CT-e de Anulação de Valores
    "3  CT-e Substituto

    CASE zib_cte_dist_ter-cd_tipo_cte.
      WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

        SELECT *
          INTO TABLE it_0301_n55
          FROM zib_cte_dist_n55
         WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
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

            "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
            SELECT SINGLE * INTO @DATA(wa_zlest0154)
              FROM zlest0154
             WHERE bukrs EQ @zib_cte_dist_ter-e_tomadora
               AND lifnr EQ @zib_cte_dist_ter-p_emissor
               AND matkl EQ @wa_itens-matkl.

            IF sy-subrc IS INITIAL.
              e_tipo_contrato = '0002'.
            ENDIF.

          ENDLOOP.

          SELECT *
            INTO TABLE it_0301_nit
            FROM zib_cte_dist_nit
             FOR ALL ENTRIES IN it_0301_n55
           WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
             AND docnum       EQ it_0301_n55-docnum_nfe.
        ENDIF.

        SELECT *
          INTO TABLE it_0301_n01
          FROM zib_cte_dist_n01
         WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
           AND tknum        NE space
           AND docnum_nf    NE space.

        IF it_0301_n01[] IS NOT INITIAL.
          SELECT *
            APPENDING TABLE it_0301_nit
            FROM zib_cte_dist_nit
             FOR ALL ENTRIES IN it_0301_n01
           WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
             AND docnum       EQ it_0301_n01-docnum_nf.
        ENDIF.

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
        DATA: lc_ck_autorizados TYPE c LENGTH 1,
              lc_matnr          TYPE matnr,
              lc_grupo          TYPE matkl,
              lc_tipo           TYPE zde_tp_aut_frete.

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
            FROM zib_cte_dist_gm
           WHERE matkl EQ lc_grupo.

          IF ( sy-subrc IS INITIAL ) AND ( it_0301_nit-ck_autorizado NE abap_true ) AND ( zib_cte_dist_ter-cd_modal NE '04' ) AND ( e_tipo_contrato NE '0002' ).
            lc_ck_autorizados = abap_false.
          ENDIF.
        ENDLOOP.
        "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
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
            it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-zvlr_frete	      = it_0301_n55-zvlr_frete.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-zquebra          = it_0301_nit-zquebra.
          it_0301_vt-zperda           = it_0301_nit-zperda.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-zvlr_quebra      = it_0301_n55-zvlr_quebra.
          it_0301_vt-zvlr_perda	      = it_0301_n55-zvlr_perda.
          it_0301_vt-zvlr_liq_pagar	  = it_0301_n55-zvlr_liq_pagar.
          it_0301_vt-ck_peso_digitado = it_0301_n55-ck_peso_digitado.
          it_0301_vt-docnum           = it_0301_nit-docnum.
          it_0301_vt-itmnum           = it_0301_nit-itmnum.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
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
            it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-zvlr_frete	      = it_0301_n01-zvlr_frete.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-zquebra          = it_0301_nit-zquebra.
          it_0301_vt-zperda           = it_0301_nit-zperda.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-zvlr_quebra      = it_0301_n01-zvlr_quebra.
          it_0301_vt-zvlr_perda	      = it_0301_n01-zvlr_perda.
          it_0301_vt-zvlr_liq_pagar	  = it_0301_n01-zvlr_liq_pagar.
          it_0301_vt-ck_peso_digitado = it_0301_n01-ck_peso_digitado.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
            it_0301_vt-ic_editar = icon_set_state.
          ELSE.
            it_0301_vt-ic_editar = icon_change_number.
          ENDIF.

          APPEND it_0301_vt.
        ENDLOOP.

        "Conhecimento de Transporte - Modal: Ferroviário
        IF zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002'.
          IF zib_cte_dist_ter-dt_chegada IS INITIAL.
            zib_cte_dist_ter-dt_chegada = zib_cte_dist_ter-dt_emissao.
          ENDIF.
        ENDIF.

      WHEN 1. "CT-e de Complemento de Valores
        it_0301_vt-zvlr_vi          = 0.
        it_0301_vt-zvlr_frete       = zib_cte_dist_ter-zvlr_frete.
        it_0301_vt-zvlr_mercadoria  = 0.
        it_0301_vt-peso_origem      = 0.
        it_0301_vt-peso_chegada     = 0.
        it_0301_vt-zpeso_diferenca  = 0.
        it_0301_vt-zvlr_kg_transp   = 0.
        it_0301_vt-zvlr_kg_mercad   = 0.
        it_0301_vt-zquebra          = 0.
        it_0301_vt-zperda           = 0.
        it_0301_vt-zvlr_quebra      = 0.
        it_0301_vt-zvlr_perda       = 0.
        it_0301_vt-zvlr_liq_pagar   = zib_cte_dist_ter-zvlr_frete.
        it_0301_vt-pc_quebra        = 0.
        it_0301_vt-pc_tolerancia    = 0.
        APPEND it_0301_vt.
    ENDCASE.

    DESCRIBE TABLE it_0301_vt LINES gb_qtd_linhas.
    CLEAR: it_0301_vt.

    IF lc_ck_autorizados = abap_false AND zib_cte_dist_ter-cd_tipo_cte NE '1'.
      MESSAGE s107.
      gb_tela_block = abap_true.
    ENDIF.

    IF gb_qtd_linhas EQ 0.
      ADD 1 TO gb_index.
      MESSAGE s088.
    ELSEIF gb_qtd_linhas EQ 1.
      "Chama Tela com TV já Selecionada
      READ TABLE it_0301_vt INDEX 1.

      IF it_0301_vt-peso_chegada IS INITIAL AND ( zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ).
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

        IF ( zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ) AND ( <fs_0301_vt>-peso_chegada IS INITIAL ) .
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
        "Chama Tela com TV já Selecionada
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
    p_dt_vencimento = zib_cte_dist_ter-zdt_vencto.
  ENDIF.

  CLEAR: zib_cte_dist_ter, gb_ordem, gb_index.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_0301_VT
*&---------------------------------------------------------------------*
FORM editar_0301_vt.

  it_0301_vt-ck_peso_digitado = abap_false.

  "Buscar Peso de Chegada do Item Transportado.
  "IT_0301_VT-PESO_CHEGADA - Peso de Chegada
  "ZIB_CTE_DIST_TER-DT_CHEGADA  - Data de Chegada
  sy-subrc = 0.
  PERFORM busca_peso_chegada CHANGING it_0301_vt sy-subrc.

  IF sy-subrc IS INITIAL.
    gb_peso_chegada = abap_true.
  ELSE.
    gb_peso_chegada = abap_false.
  ENDIF.

  IF it_0301_vt-zvlr_frete GT 0 AND it_0301_vt-peso_origem GT 0.
    it_0301_vt-zvlr_kg_transp = it_0301_vt-zvlr_frete / it_0301_vt-peso_origem.
  ENDIF.

  "Se achar o peso informado calcular quebra e perda
  IF sy-subrc IS INITIAL AND gb_tela_block EQ abap_false.

    CALL METHOD zcl_cte_dist_g=>calcula_quebra_perda
      EXPORTING
        p_cod_mercadoria    = it_0301_vt-zmatnr_merc
        p_peso_origem       = it_0301_vt-peso_origem
        p_peso_destino      = it_0301_vt-peso_chegada
        p_vlr_frete         = it_0301_vt-zvlr_frete
        p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
        p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
      IMPORTING
        e_peso_diferenca    = it_0301_vt-zpeso_diferenca
        e_peso_quebra       = it_0301_vt-zquebra
        e_peso_perda        = it_0301_vt-zperda
        e_vlr_quebra        = it_0301_vt-zvlr_quebra
        e_vlr_perda         = it_0301_vt-zvlr_perda
        e_vlr_liq_pagar     = it_0301_vt-zvlr_liq_pagar
        e_pc_quebra         = it_0301_vt-pc_quebra
        e_pc_tolerancia     = it_0301_vt-pc_tolerancia.
  ENDIF.
  gb_ctr_altera_frete = abap_false.
  gb_ctr_calcul_frete = abap_false.

  IF gb_tela_block EQ abap_false.
    IF zib_cte_dist_ter-zbvtyp IS INITIAL.
      zib_cte_dist_ter-zbvtyp = '0001'.
    ENDIF.

    IF zib_cte_dist_ter-zdt_mov IS INITIAL.
      zib_cte_dist_ter-zdt_mov = sy-datum.
    ELSEIF zib_cte_dist_ter-zdt_mov IS NOT INITIAL AND zib_cte_dist_ter-belnr IS INITIAL.
      zib_cte_dist_ter-zdt_mov = sy-datum.
    ENDIF.
  ENDIF.

  ck_zbvtyp = abap_false.

  IF zib_cte_dist_ter-zbvtyp IS NOT INITIAL.
    CLEAR: wa_info_forne.
    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = wa_lfbk
        e_bnka     = wa_bnka
      CHANGING
        p_cte      = zib_cte_dist_ter
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

      "CT-e Possuir somente uma VT, e o usuário confirmou ou salvou, deve ser gravado
      IF gb_qtd_linhas EQ 1 AND gb_tela_block EQ abap_false.
        PERFORM salvar_informacoes_doc.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " EDITAR_0301_VT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_PESO_CHEGADA
*&---------------------------------------------------------------------*
*       Importação de Peso Digitado
*----------------------------------------------------------------------*
FORM busca_peso_chegada  CHANGING p_digitar  TYPE zde_cte_dist_vt_alv
                                  p_achou    TYPE sy-subrc.

  DATA: wa_0039 TYPE zlest0039.
  CLEAR:  wa_0039.

  CHECK gb_tela_block EQ abap_false.

*01	Rodoviário
*02	Aéreo
*03	Aquaviário
*04	Ferroviário
*05	Dutoviário
  IF p_digitar-peso_chegada IS INITIAL AND zib_cte_dist_ter-dt_chegada IS INITIAL.
    p_achou = 4.
    CASE zib_cte_dist_ter-cd_modal.
      WHEN '01'.  "Rodoviário
        "Comparativo de saidas e chegadas
        SELECT SINGLE * INTO wa_0039 FROM zlest0039 WHERE docnum EQ p_digitar-docnum.

        IF sy-subrc IS INITIAL.
          IF wa_0039-pontotransb IS INITIAL.
            IF ( wa_0039-pesochegada IS NOT INITIAL ) AND ( wa_0039-datachegada IS NOT INITIAL ).
              p_digitar-peso_chegada       = wa_0039-pesochegada.
              zib_cte_dist_ter-dt_chegada  = wa_0039-datachegada.
              p_achou = 0.
            ELSE.
              p_achou = 4.
            ENDIF.
          ELSE.
            IF ( wa_0039-pesotransb IS NOT INITIAL ) AND ( wa_0039-datatransb IS NOT INITIAL ).
              p_digitar-peso_chegada       = wa_0039-pesotransb.
              zib_cte_dist_ter-dt_chegada  = wa_0039-datatransb.
              p_achou = 0.
            ELSE.
              p_achou = 4.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN '02'.  "Aéreo
      WHEN '03'.  "Aquaviário
      WHEN '04'.  "Ferroviário
      WHEN '05'.  "Dutoviário
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

  FIELD-SYMBOLS: <n55> TYPE zib_cte_dist_n55,
                 <n01> TYPE zib_cte_dist_n01,
                 <nit> TYPE zib_cte_dist_nit.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
  CASE zib_cte_dist_ter-cd_tipo_cte.
    WHEN 0 OR 3. "0  CT-e Normal/CT-e Substituto

      CLEAR: it_0301_n55[], it_0301_n01[], it_0301_nit[].

      SELECT *
        INTO TABLE it_0301_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      zib_cte_dist_ter-zvlr_vi         = 0.
      zib_cte_dist_ter-zvlr_frete      = 0.
      zib_cte_dist_ter-zvlr_mercadoria = 0.
      zib_cte_dist_ter-peso_origem     = 0.
      zib_cte_dist_ter-peso_chegada    = 0.
      zib_cte_dist_ter-zpeso_diferenca = 0.
      zib_cte_dist_ter-zquebra         = 0.
      zib_cte_dist_ter-zperda          = 0.
      zib_cte_dist_ter-zvlr_quebra     = 0.
      zib_cte_dist_ter-zvlr_perda      = 0.
      zib_cte_dist_ter-zvlr_liq_pagar  = 0.
      zib_cte_dist_ter-ck_peso_chegada = abap_true.

      LOOP AT it_0301_vt.
        ADD it_0301_vt-zvlr_vi         TO zib_cte_dist_ter-zvlr_vi.
        ADD it_0301_vt-zvlr_frete      TO zib_cte_dist_ter-zvlr_frete.
        ADD it_0301_vt-zvlr_mercadoria TO zib_cte_dist_ter-zvlr_mercadoria.
        ADD it_0301_vt-zpeso_diferenca TO zib_cte_dist_ter-zpeso_diferenca.
        ADD it_0301_vt-zquebra         TO zib_cte_dist_ter-zquebra.
        ADD it_0301_vt-zperda          TO zib_cte_dist_ter-zperda.
        ADD it_0301_vt-zvlr_quebra     TO zib_cte_dist_ter-zvlr_quebra.
        ADD it_0301_vt-zvlr_perda      TO zib_cte_dist_ter-zvlr_perda.
        ADD it_0301_vt-zvlr_liq_pagar  TO zib_cte_dist_ter-zvlr_liq_pagar.
        ADD it_0301_vt-peso_chegada    TO zib_cte_dist_ter-peso_chegada.

        IF it_0301_vt-ck_autorizado EQ abap_false.
          ADD it_0301_vt-peso_origem  TO zib_cte_dist_ter-peso_origem.
        ENDIF.

        "Ajustando valores notas 55
        LOOP AT it_0301_n55 ASSIGNING <n55> WHERE tknum EQ it_0301_vt-tknum.
          <n55>-zvlr_frete       = it_0301_vt-zvlr_frete.
          <n55>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n55>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
          <n55>-zvlr_perda       = it_0301_vt-zvlr_perda.
          <n55>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
          <n55>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n55>-docnum_nfe.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.

            IF it_0301_vt-ck_autorizado EQ abap_false.
              <nit>-peso_origem    = it_0301_vt-peso_origem.
              <nit>-peso_chegada   = it_0301_vt-peso_chegada.
            ELSE.
              ADD <nit>-peso_origem TO zib_cte_dist_ter-peso_origem.
            ENDIF.

            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-zquebra          = it_0301_vt-zquebra.
            <nit>-zperda           = it_0301_vt-zperda.
            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
            IF it_0301_vt-ck_peso_digitado EQ abap_false.
              zib_cte_dist_ter-ck_peso_chegada = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "Ajustando valores notas 01
        LOOP AT it_0301_n01 ASSIGNING <n01> WHERE tknum EQ it_0301_vt-tknum.
          <n01>-zvlr_frete       = it_0301_vt-zvlr_frete.
          <n01>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n01>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
          <n01>-zvlr_perda       = it_0301_vt-zvlr_perda.
          <n01>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar .
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n01>-docnum_nf.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
            <nit>-peso_origem      = it_0301_vt-peso_origem.
            <nit>-peso_chegada     = it_0301_vt-peso_chegada.
            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-zquebra          = it_0301_vt-zquebra.
            <nit>-zperda           = it_0301_vt-zperda.
            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
            IF it_0301_vt-ck_peso_digitado EQ abap_false.
              zib_cte_dist_ter-ck_peso_chegada = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      zib_cte_dist_ter-zbase_icms    = 0.
      zib_cte_dist_ter-zbase_pis     = 0.
      zib_cte_dist_ter-zbase_cofins  = 0.
      zib_cte_dist_ter-zrate_icms    = 0.
      zib_cte_dist_ter-zrate_pis     = 0.
      zib_cte_dist_ter-zrate_cofins  = 0.
      zib_cte_dist_ter-zvalor_icms   = 0.
      zib_cte_dist_ter-zvalor_pis    = 0.
      zib_cte_dist_ter-zvalor_cofins = 0.

      IF it_0301_n55[] IS NOT INITIAL.
        MODIFY zib_cte_dist_n55 FROM TABLE it_0301_n55.
      ENDIF.

      IF it_0301_n01[] IS NOT INITIAL.
        MODIFY zib_cte_dist_n01 FROM TABLE it_0301_n01.
      ENDIF.

      IF it_0301_nit[] IS NOT INITIAL.
        MODIFY zib_cte_dist_nit FROM TABLE it_0301_nit.
      ENDIF.

    WHEN 1. "1  CT-e de Complemento de Valores
      zib_cte_dist_ter-ck_peso_chegada = abap_true.
  ENDCASE.

  "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
    FROM j_1bnflin AS l
   INNER JOIN zib_cte_dist_n55 AS n ON n~docnum_nfe EQ l~docnum
   WHERE n~cd_chave_cte EQ @zib_cte_dist_ter-cd_chave_cte
     AND n~docnum_nfe   NE @space.

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE l~matnr INTO lc_matnr
      FROM j_1bnflin AS l
     INNER JOIN zib_cte_dist_n01 AS n ON n~docnum_nf EQ l~docnum
     WHERE n~cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
       AND n~docnum_nf    NE space.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL METHOD zcl_cte_dist_g=>busca_impostos_taxas
    EXPORTING
      p_iva            = zib_cte_dist_ter-mwskz
      p_data_documento = zib_cte_dist_ter-dt_emissao
      p_shipfrom       = zib_cte_dist_ter-inicio_uf
      p_shipto         = zib_cte_dist_ter-termino_uf
      e_tomadora       = zib_cte_dist_ter-e_tomadora
      f_tomadora       = zib_cte_dist_ter-f_tomadora
      p_emissora       = zib_cte_dist_ter-p_emissor
      p_matnr          = lc_matnr
    IMPORTING
      e_rate_icms      = zib_cte_dist_ter-zrate_icms
      e_rate_pis       = zib_cte_dist_ter-zrate_pis
      e_rate_cofins    = zib_cte_dist_ter-zrate_cofins
    EXCEPTIONS
      sem_iva          = 1
      OTHERS           = 2.

  IF sy-subrc IS INITIAL.
    IF zib_cte_dist_ter-zrate_icms GT 0.
      zib_cte_dist_ter-zbase_icms  = zib_cte_dist_ter-zvlr_frete.
      zib_cte_dist_ter-zvalor_icms = zib_cte_dist_ter-zvlr_frete * ( zib_cte_dist_ter-zrate_icms / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_icms  = 0.
      zib_cte_dist_ter-zvalor_icms = 0.
    ENDIF.

    DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( zib_cte_dist_ter-zvlr_frete )
                                                                           i_valor_icms  =   CONV #( zib_cte_dist_ter-zvalor_icms ) ).

    IF zib_cte_dist_ter-zrate_pis GT 0.
      zib_cte_dist_ter-zbase_pis  = lva_base_calc_pis_cofins.
      zib_cte_dist_ter-zvalor_pis = lva_base_calc_pis_cofins * ( zib_cte_dist_ter-zrate_pis / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_pis  = 0.
      zib_cte_dist_ter-zvalor_pis = 0.
    ENDIF.

    IF zib_cte_dist_ter-zrate_cofins GT 0.
      zib_cte_dist_ter-zbase_cofins  = lva_base_calc_pis_cofins.
      zib_cte_dist_ter-zvalor_cofins = lva_base_calc_pis_cofins * ( zib_cte_dist_ter-zrate_cofins / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_cofins  = 0.
      zib_cte_dist_ter-zvalor_cofins = 0.
    ENDIF.
  ENDIF.

  MODIFY zib_cte_dist_ter.
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
      i_structure_name = 'ZDE_CTE_DIST_VT_ALV'
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
      WHEN 'ZVLR_VI' OR 'ZVLR_FRETE' OR 'ZVLR_MERCADORIA' OR 'PESO_ORIGEM' OR 'PESO_CHEGADA' OR 'ZPESO_DIFERENCA' OR
           'ZQUEBRA' OR 'ZPERDA' OR 'ZVLR_QUEBRA' OR 'ZVLR_PERDA' OR 'ZVLR_LIQ_PAGAR'.
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

      IF it_0301_vt-zquebra GT 0 AND it_0301_vt-zvlr_quebra EQ 0.
        MESSAGE s171 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      IF zib_cte_dist_ter-zdt_vencto LT sy-datum.
        MESSAGE s100 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      IF ck_peso_liberado NE abap_true.
        MESSAGE s169 DISPLAY LIKE 'E'.
        CLEAR ok_code.
        EXIT.
      ENDIF.

      IF zib_cte_dist_ter-mwskz IS INITIAL.
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

      IF it_0301_vt-zvlr_frete GT 0 AND it_0301_vt-peso_origem GT 0.
        it_0301_vt-zvlr_kg_transp = it_0301_vt-zvlr_frete / it_0301_vt-peso_origem.
      ENDIF.

      CALL METHOD zcl_cte_dist_g=>calcula_quebra_perda
        EXPORTING
          p_cod_mercadoria    = it_0301_vt-zmatnr_merc
          p_peso_origem       = it_0301_vt-peso_origem
          p_peso_destino      = it_0301_vt-peso_chegada
          p_vlr_frete         = it_0301_vt-zvlr_frete
          p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
          p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
        IMPORTING
          e_peso_diferenca    = it_0301_vt-zpeso_diferenca
          e_peso_quebra       = it_0301_vt-zquebra
          e_peso_perda        = it_0301_vt-zperda
          e_vlr_quebra        = it_0301_vt-zvlr_quebra
          e_vlr_perda         = it_0301_vt-zvlr_perda
          e_vlr_liq_pagar     = it_0301_vt-zvlr_liq_pagar
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
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
    CASE zib_cte_dist_ter-cd_tipo_cte.
      WHEN 1. "1  CT-e de Complemento de Valores
        LOOP AT SCREEN.
          IF ( screen-input NE 0 ) AND
             ( "SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-MWSKZ' OR
               screen-name EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR
               screen-name EQ 'IT_0301_VT-ZVLR_FRETE' OR
               screen-name EQ 'IT_0301_VT-PESO_ORIGEM' OR
               screen-name EQ 'IT_0301_VT-PESO_CHEGADA' ).
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    "Valores informados na autorização de pagamento
    IF it_0301_vt-ck_autorizado EQ abap_true.
      LOOP AT SCREEN.
        IF ( screen-input NE 0 ) AND
           ( screen-name EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR
             screen-name EQ 'IT_0301_VT-ZVLR_FRETE' OR
             screen-name EQ 'IT_0301_VT-PESO_ORIGEM' OR
             screen-name EQ 'IT_0301_VT-PESO_CHEGADA' ).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002'.
      LOOP AT SCREEN.
        IF ( screen-input NE 0 ) AND
           ( screen-name EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR
             screen-name EQ 'IT_0301_VT-PESO_CHEGADA' "OR
             "SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-MWSKZ'
          ).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Verificação de Peso de Chegada """"""""""""""""""""""""""""""""""""
    IF zib_cte_dist_ter-tp_processo_cte EQ '01' AND ( zib_cte_dist_ter-cd_modal = '01' AND e_tipo_contrato NE '0002' ) AND zib_cte_dist_ter-cd_tipo_cte NE '1'.

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
        wa_zib_cte_dist_eap-cd_chave_cte  = zib_cte_dist_ter-cd_chave_cte.

        SELECT SINGLE * INTO wa_zib_cte_dist_eap
          FROM zib_cte_dist_eap
         WHERE tp_aprovacao  EQ wa_zib_cte_dist_eap-tp_aprovacao
           AND cd_chave_cte  EQ wa_zib_cte_dist_eap-cd_chave_cte
           AND tp_autorizado EQ wa_zib_cte_dist_eap-tp_autorizado
           AND ck_ultimo     EQ abap_true.

        IF sy-subrc IS INITIAL.
          ck_peso_manual   = abap_true.
          ck_peso_liberado = abap_true.
        ENDIF.

        "Não autorizado peso/data chegada manual
        IF ck_peso_manual EQ abap_false.
          LOOP AT SCREEN.
            IF ( screen-input NE 0 ) AND ( screen-name EQ 'IT_0301_VT-PESO_CHEGADA' OR screen-name EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' ).
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

  "Se existir somente uma VT não precisa confirmar
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
        p_cte      = zib_cte_dist_ter
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
      i_data_vencimento = zib_cte_dist_ter-zdt_vencto
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
FORM faturar_cte_ferr  USING p_cte TYPE zib_cte_dist_ter
                        p_index_ultimo TYPE i
               CHANGING p_index TYPE i
                        p_dt_vencimento TYPE zdt_vencto
                        p_ordem TYPE char01.

  DATA: ck_primeiro_peso TYPE char01.

  CLEAR: it_0301_dup[],
         it_0301_n55[],
         it_0301_n01[],
         it_0301_nit[],
         it_0301_vt[],
         e_tipo_contrato,
         wa_info_forne.

  FIELD-SYMBOLS: <fs_0301_vt> TYPE zde_cte_dist_vt_alv.

  gb_index               = p_index.
  gb_index_ultimo        = p_index_ultimo.
  gb_tela_block          = abap_false.

  SELECT SINGLE * INTO zib_cte_dist_ter
    FROM zib_cte_dist_ter
   WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

  IF sy-subrc IS INITIAL.
    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    IF zib_cte_dist_ter-ck_finalizado EQ abap_true.
      gb_tela_block = abap_true.
    ENDIF.

    "Sugerir data de Vencimento
    IF zib_cte_dist_ter-zdt_vencto IS INITIAL AND gb_tela_block EQ abap_false.
      SELECT *
        INTO TABLE it_0301_dup
        FROM zib_cte_dist_dup
       WHERE cd_chave_cte  EQ zib_cte_dist_ter-cd_chave_cte
         AND dt_vencimento GE sy-datum
       ORDER BY dt_vencimento.

      IF sy-subrc IS INITIAL.
        READ TABLE it_0301_dup INDEX 1.
        zib_cte_dist_ter-zdt_vencto = it_0301_dup-dt_vencimento.
      ENDIF.
    ENDIF.

    IF ( p_dt_vencimento IS NOT INITIAL ) AND ( gb_tela_block EQ abap_false ).
      zib_cte_dist_ter-zdt_vencto = p_dt_vencimento.
    ELSEIF ( p_dt_vencimento IS INITIAL ) AND ( gb_tela_block EQ abap_false ) AND ( zib_cte_dist_ter-zdt_vencto IS INITIAL ).

      CALL METHOD zcl_cte_dist_g=>busca_proximo_venc_fatura
        IMPORTING
          e_data_vencimento = p_dt_vencimento.

      zib_cte_dist_ter-zdt_vencto = p_dt_vencimento.
    ENDIF.

    "0  CT-e Normal
    "1  CT-e de Complemento de Valores
    "2  CT-e de Anulação de Valores
    "3  CT-e Substituto

    CASE zib_cte_dist_ter-cd_tipo_cte.
      WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

        SELECT *
          INTO TABLE it_0301_n55
          FROM zib_cte_dist_n55
         WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
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

            "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
            SELECT SINGLE * INTO @DATA(wa_zlest0154)
              FROM zlest0154
             WHERE bukrs EQ @zib_cte_dist_ter-e_tomadora
               AND lifnr EQ @zib_cte_dist_ter-p_emissor
               AND matkl EQ @wa_itens-matkl.

            IF sy-subrc IS INITIAL.
              e_tipo_contrato = '0002'.
            ENDIF.

          ENDLOOP.

          SELECT *
            INTO TABLE it_0301_nit
            FROM zib_cte_dist_nit
             FOR ALL ENTRIES IN it_0301_n55
           WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
             AND docnum       EQ it_0301_n55-docnum_nfe.
        ENDIF.

        SELECT *
          INTO TABLE it_0301_n01
          FROM zib_cte_dist_n01
         WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
           AND tknum        NE space
           AND docnum_nf    NE space.

        IF it_0301_n01[] IS NOT INITIAL.
          SELECT *
            APPENDING TABLE it_0301_nit
            FROM zib_cte_dist_nit
             FOR ALL ENTRIES IN it_0301_n01
           WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
             AND docnum       EQ it_0301_n01-docnum_nf.
        ENDIF.

        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
        DATA: lc_ck_autorizados TYPE c LENGTH 1,
              lc_matnr          TYPE matnr,
              lc_grupo          TYPE matkl,
              lc_tipo           TYPE zde_tp_aut_frete.

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
            FROM zib_cte_dist_gm
           WHERE matkl EQ lc_grupo.

          IF ( sy-subrc IS INITIAL ) AND ( it_0301_nit-ck_autorizado NE abap_true ) AND ( zib_cte_dist_ter-cd_modal NE '04' ) AND ( e_tipo_contrato NE '0002' ).
            lc_ck_autorizados = abap_false.
          ENDIF.
        ENDLOOP.
        "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
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
            it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-zvlr_frete	      = it_0301_n55-zvlr_frete.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-zquebra          = it_0301_nit-zquebra.
          it_0301_vt-zperda           = it_0301_nit-zperda.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-zvlr_quebra      = it_0301_n55-zvlr_quebra.
          it_0301_vt-zvlr_perda	      = it_0301_n55-zvlr_perda.
          it_0301_vt-zvlr_liq_pagar	  = it_0301_n55-zvlr_liq_pagar.
          it_0301_vt-ck_peso_digitado = it_0301_n55-ck_peso_digitado.
          it_0301_vt-docnum           = it_0301_nit-docnum.
          it_0301_vt-itmnum           = it_0301_nit-itmnum.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
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
            it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
          ELSE.
            it_0301_vt-zvlr_frete	      = it_0301_n01-zvlr_frete.
            it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
            it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
            it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
          ENDIF.

          it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
          it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
          it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
          it_0301_vt-zquebra          = it_0301_nit-zquebra.
          it_0301_vt-zperda           = it_0301_nit-zperda.
          it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
          it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
          it_0301_vt-zvlr_quebra      = it_0301_n01-zvlr_quebra.
          it_0301_vt-zvlr_perda	      = it_0301_n01-zvlr_perda.
          it_0301_vt-zvlr_liq_pagar	  = it_0301_n01-zvlr_liq_pagar.
          it_0301_vt-ck_peso_digitado = it_0301_n01-ck_peso_digitado.

          IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( zib_cte_dist_ter-ck_peso_chegada EQ abap_true ).
            it_0301_vt-ic_editar = icon_set_state.
          ELSE.
            it_0301_vt-ic_editar = icon_change_number.
          ENDIF.

          APPEND it_0301_vt.
        ENDLOOP.

        "Conhecimento de Transporte - Modal: Ferroviário
        IF zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002'.
          IF zib_cte_dist_ter-dt_chegada IS INITIAL.
            zib_cte_dist_ter-dt_chegada = zib_cte_dist_ter-dt_emissao.
          ENDIF.
        ENDIF.

      WHEN 1. "CT-e de Complemento de Valores
        it_0301_vt-zvlr_vi          = 0.
        it_0301_vt-zvlr_frete       = zib_cte_dist_ter-zvlr_frete.
        it_0301_vt-zvlr_mercadoria  = 0.
        it_0301_vt-peso_origem      = 0.
        it_0301_vt-peso_chegada     = 0.
        it_0301_vt-zpeso_diferenca  = 0.
        it_0301_vt-zvlr_kg_transp   = 0.
        it_0301_vt-zvlr_kg_mercad   = 0.
        it_0301_vt-zquebra          = 0.
        it_0301_vt-zperda           = 0.
        it_0301_vt-zvlr_quebra      = 0.
        it_0301_vt-zvlr_perda       = 0.
        it_0301_vt-zvlr_liq_pagar   = zib_cte_dist_ter-zvlr_frete.
        it_0301_vt-pc_quebra        = 0.
        it_0301_vt-pc_tolerancia    = 0.
        APPEND it_0301_vt.
    ENDCASE.

    DESCRIBE TABLE it_0301_vt LINES gb_qtd_linhas.
    CLEAR: it_0301_vt.

    IF lc_ck_autorizados = abap_false AND zib_cte_dist_ter-cd_tipo_cte NE '1'.
      MESSAGE s107.
      gb_tela_block = abap_true.
    ENDIF.

    IF gb_qtd_linhas EQ 0.
      ADD 1 TO gb_index.
      MESSAGE s088.
    ELSEIF gb_qtd_linhas EQ 1.
      "Chama Tela com TV já Selecionada
      READ TABLE it_0301_vt INDEX 1.

      IF it_0301_vt-peso_chegada IS INITIAL AND ( zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ).
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

        IF ( zib_cte_dist_ter-cd_modal EQ '04' OR e_tipo_contrato EQ '0002' ) AND ( <fs_0301_vt>-peso_chegada IS INITIAL ) .
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
        "Chama Tela com TV já Selecionada
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
    p_dt_vencimento = zib_cte_dist_ter-zdt_vencto.
  ENDIF.

  CLEAR: zib_cte_dist_ter, gb_ordem, gb_index.

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
  "ZIB_CTE_DIST_TER-DT_CHEGADA  - Data de Chegada
  sy-subrc = 0.
  PERFORM busca_peso_chegada CHANGING it_0301_vt sy-subrc.

  IF sy-subrc IS INITIAL.
    gb_peso_chegada = abap_true.
  ELSE.
    gb_peso_chegada = abap_false.
  ENDIF.

  IF it_0301_vt-zvlr_frete GT 0 AND it_0301_vt-peso_origem GT 0.
    it_0301_vt-zvlr_kg_transp = it_0301_vt-zvlr_frete / it_0301_vt-peso_origem.
  ENDIF.

  "Se achar o peso informado calcular quebra e perda
  IF sy-subrc IS INITIAL AND gb_tela_block EQ abap_false.

    CALL METHOD zcl_cte_dist_g=>calcula_quebra_perda
      EXPORTING
        p_cod_mercadoria    = it_0301_vt-zmatnr_merc
        p_peso_origem       = it_0301_vt-peso_origem
        p_peso_destino      = it_0301_vt-peso_chegada
        p_vlr_frete         = it_0301_vt-zvlr_frete
        p_vlr_kg_trasport   = it_0301_vt-zvlr_kg_transp
        p_vlr_kg_mercadoria = it_0301_vt-zvlr_kg_mercad
      IMPORTING
        e_peso_diferenca    = it_0301_vt-zpeso_diferenca
        e_peso_quebra       = it_0301_vt-zquebra
        e_peso_perda        = it_0301_vt-zperda
        e_vlr_quebra        = it_0301_vt-zvlr_quebra
        e_vlr_perda         = it_0301_vt-zvlr_perda
        e_vlr_liq_pagar     = it_0301_vt-zvlr_liq_pagar
        e_pc_quebra         = it_0301_vt-pc_quebra
        e_pc_tolerancia     = it_0301_vt-pc_tolerancia.
  ENDIF.
  gb_ctr_altera_frete = abap_false.
  gb_ctr_calcul_frete = abap_false.

  IF gb_tela_block EQ abap_false.
    IF zib_cte_dist_ter-zbvtyp IS INITIAL.
      zib_cte_dist_ter-zbvtyp = '0001'.
    ENDIF.

    IF zib_cte_dist_ter-zdt_mov IS INITIAL.
      zib_cte_dist_ter-zdt_mov = sy-datum.
    ELSEIF zib_cte_dist_ter-zdt_mov IS NOT INITIAL AND zib_cte_dist_ter-belnr IS INITIAL.
      zib_cte_dist_ter-zdt_mov = sy-datum.
    ENDIF.
  ENDIF.

  ck_zbvtyp = abap_false.

  IF zib_cte_dist_ter-zbvtyp IS NOT INITIAL.
    CLEAR: wa_info_forne.
    CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
      IMPORTING
        e_lfbk     = wa_lfbk
        e_bnka     = wa_bnka
      CHANGING
        p_cte      = zib_cte_dist_ter
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

  "CT-e Possuir somente uma VT, e o usuário confirmou ou salvou, deve ser gravado
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

  FIELD-SYMBOLS: <n55> TYPE zib_cte_dist_n55,
                 <n01> TYPE zib_cte_dist_n01,
                 <nit> TYPE zib_cte_dist_nit.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
  CASE zib_cte_dist_ter-cd_tipo_cte.
    WHEN 0 OR 3. "0  CT-e Normal/CT-e Substituto

      CLEAR: it_0301_n55[], it_0301_n01[], it_0301_nit[].

      SELECT *
        INTO TABLE it_0301_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      zib_cte_dist_ter-zvlr_vi         = 0.
      zib_cte_dist_ter-zvlr_frete      = 0.
      zib_cte_dist_ter-zvlr_mercadoria = 0.
      zib_cte_dist_ter-peso_origem     = 0.
      zib_cte_dist_ter-peso_chegada    = 0.
      zib_cte_dist_ter-zpeso_diferenca = 0.
      zib_cte_dist_ter-zquebra         = 0.
      zib_cte_dist_ter-zperda          = 0.
      zib_cte_dist_ter-zvlr_quebra     = 0.
      zib_cte_dist_ter-zvlr_perda      = 0.
      zib_cte_dist_ter-zvlr_liq_pagar  = 0.
      zib_cte_dist_ter-ck_peso_chegada = abap_true.

      LOOP AT it_0301_vt.
        ADD it_0301_vt-zvlr_vi         TO zib_cte_dist_ter-zvlr_vi.
        ADD it_0301_vt-zvlr_frete      TO zib_cte_dist_ter-zvlr_frete.
        ADD it_0301_vt-zvlr_mercadoria TO zib_cte_dist_ter-zvlr_mercadoria.
        ADD it_0301_vt-zpeso_diferenca TO zib_cte_dist_ter-zpeso_diferenca.
        ADD it_0301_vt-zquebra         TO zib_cte_dist_ter-zquebra.
        ADD it_0301_vt-zperda          TO zib_cte_dist_ter-zperda.
        ADD it_0301_vt-zvlr_quebra     TO zib_cte_dist_ter-zvlr_quebra.
        ADD it_0301_vt-zvlr_perda      TO zib_cte_dist_ter-zvlr_perda.
        ADD it_0301_vt-zvlr_liq_pagar  TO zib_cte_dist_ter-zvlr_liq_pagar.
        ADD it_0301_vt-peso_chegada    TO zib_cte_dist_ter-peso_chegada.

        IF it_0301_vt-ck_autorizado EQ abap_false.
          ADD it_0301_vt-peso_origem  TO zib_cte_dist_ter-peso_origem.
        ENDIF.

        "Ajustando valores notas 55
        LOOP AT it_0301_n55 ASSIGNING <n55> WHERE tknum EQ it_0301_vt-tknum.
          <n55>-zvlr_frete       = it_0301_vt-zvlr_frete.
          <n55>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n55>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
          <n55>-zvlr_perda       = it_0301_vt-zvlr_perda.
          <n55>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
          <n55>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n55>-docnum_nfe.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.

            IF it_0301_vt-ck_autorizado EQ abap_false.
              <nit>-peso_origem    = it_0301_vt-peso_origem.
              <nit>-peso_chegada   = it_0301_vt-peso_chegada.
            ELSE.
              ADD <nit>-peso_origem TO zib_cte_dist_ter-peso_origem.
            ENDIF.

            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-zquebra          = it_0301_vt-zquebra.
            <nit>-zperda           = it_0301_vt-zperda.
            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*            IF it_0301_vt-ck_peso_digitado EQ abap_false.
*              zib_cte_dist_ter-ck_peso_chegada = abap_false.
*            ENDIF.
          ENDLOOP.
        ENDLOOP.

        "Ajustando valores notas 01
        LOOP AT it_0301_n01 ASSIGNING <n01> WHERE tknum EQ it_0301_vt-tknum.
          <n01>-zvlr_frete       = it_0301_vt-zvlr_frete.
          <n01>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
          <n01>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
          <n01>-zvlr_perda       = it_0301_vt-zvlr_perda.
          <n01>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar .
          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n01>-docnum_nf.
            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
            <nit>-peso_origem      = it_0301_vt-peso_origem.
            <nit>-peso_chegada     = it_0301_vt-peso_chegada.
            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
            <nit>-zquebra          = it_0301_vt-zquebra.
            <nit>-zperda           = it_0301_vt-zperda.
            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*            IF it_0301_vt-ck_peso_digitado EQ abap_false.
*              zib_cte_dist_ter-ck_peso_chegada = abap_false.
*            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      zib_cte_dist_ter-zbase_icms    = 0.
      zib_cte_dist_ter-zbase_pis     = 0.
      zib_cte_dist_ter-zbase_cofins  = 0.
      zib_cte_dist_ter-zrate_icms    = 0.
      zib_cte_dist_ter-zrate_pis     = 0.
      zib_cte_dist_ter-zrate_cofins  = 0.
      zib_cte_dist_ter-zvalor_icms   = 0.
      zib_cte_dist_ter-zvalor_pis    = 0.
      zib_cte_dist_ter-zvalor_cofins = 0.

      IF it_0301_n55[] IS NOT INITIAL.
        MODIFY zib_cte_dist_n55 FROM TABLE it_0301_n55.
      ENDIF.

      IF it_0301_n01[] IS NOT INITIAL.
        MODIFY zib_cte_dist_n01 FROM TABLE it_0301_n01.
      ENDIF.

      IF it_0301_nit[] IS NOT INITIAL.
        MODIFY zib_cte_dist_nit FROM TABLE it_0301_nit.
      ENDIF.

    WHEN 1. "1  CT-e de Complemento de Valores
      zib_cte_dist_ter-ck_peso_chegada = abap_true.
  ENDCASE.

  "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
    FROM j_1bnflin AS l
   INNER JOIN zib_cte_dist_n55 AS n ON n~docnum_nfe EQ l~docnum
   WHERE n~cd_chave_cte EQ @zib_cte_dist_ter-cd_chave_cte
     AND n~docnum_nfe   NE @space.

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE l~matnr INTO lc_matnr
      FROM j_1bnflin AS l
     INNER JOIN zib_cte_dist_n01 AS n ON n~docnum_nf EQ l~docnum
     WHERE n~cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte
       AND n~docnum_nf    NE space.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL METHOD zcl_cte_dist_g=>busca_impostos_taxas
    EXPORTING
      p_iva            = zib_cte_dist_ter-mwskz
      p_data_documento = zib_cte_dist_ter-dt_emissao
      p_shipfrom       = zib_cte_dist_ter-inicio_uf
      p_shipto         = zib_cte_dist_ter-termino_uf
      e_tomadora       = zib_cte_dist_ter-e_tomadora
      f_tomadora       = zib_cte_dist_ter-f_tomadora
      p_emissora       = zib_cte_dist_ter-p_emissor
      p_matnr          = lc_matnr
    IMPORTING
      e_rate_icms      = zib_cte_dist_ter-zrate_icms
      e_rate_pis       = zib_cte_dist_ter-zrate_pis
      e_rate_cofins    = zib_cte_dist_ter-zrate_cofins
    EXCEPTIONS
      sem_iva          = 1
      OTHERS           = 2.

  IF sy-subrc IS INITIAL.
    IF zib_cte_dist_ter-zrate_icms GT 0.
      zib_cte_dist_ter-zbase_icms  = zib_cte_dist_ter-zvlr_frete.
      zib_cte_dist_ter-zvalor_icms = zib_cte_dist_ter-zvlr_frete * ( zib_cte_dist_ter-zrate_icms / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_icms  = 0.
      zib_cte_dist_ter-zvalor_icms = 0.
    ENDIF.

    DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( zib_cte_dist_ter-zvlr_frete )
                                                                           i_valor_icms  =   CONV #( zib_cte_dist_ter-zvalor_icms ) ).

    IF zib_cte_dist_ter-zrate_pis GT 0.
      zib_cte_dist_ter-zbase_pis  = lva_base_calc_pis_cofins.
      zib_cte_dist_ter-zvalor_pis = lva_base_calc_pis_cofins * ( zib_cte_dist_ter-zrate_pis / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_pis  = 0.
      zib_cte_dist_ter-zvalor_pis = 0.
    ENDIF.

    IF zib_cte_dist_ter-zrate_cofins GT 0.
      zib_cte_dist_ter-zbase_cofins  = lva_base_calc_pis_cofins.
      zib_cte_dist_ter-zvalor_cofins = lva_base_calc_pis_cofins * ( zib_cte_dist_ter-zrate_cofins / 100 ).
    ELSE.
      zib_cte_dist_ter-zbase_cofins  = 0.
      zib_cte_dist_ter-zvalor_cofins = 0.
    ENDIF.
  ENDIF.

  MODIFY zib_cte_dist_ter.
  COMMIT WORK.

ENDFORM.
