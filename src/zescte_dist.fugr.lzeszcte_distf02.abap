*----------------------------------------------------------------------*
***INCLUDE LZCTE_DISTF02.
*----------------------------------------------------------------------*

DATA: ctl_alv_0101       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0101       TYPE REF TO cl_gui_custom_container,
      gs_lay_0101        TYPE lvc_s_layo,
      gs_var_0101        TYPE disvariant,
      gs_scroll_col_0101 TYPE lvc_s_col,
      gs_scroll_row_0101 TYPE lvc_s_roid,
      it_catalog_0101    TYPE lvc_t_fcat,
      it_opcoes          TYPE TABLE OF dd07v WITH HEADER LINE,
      wa_opcoes          TYPE dd07v.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: event_handler TYPE REF TO lcl_event_handler.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING p_row TYPE lvc_s_row.
  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.
    APPEND p_row TO lc_row.

    CALL METHOD ctl_alv_0101->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    READ TABLE it_opcoes INDEX p_row-index INTO wa_opcoes.

    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  ESCOLHER_LIBERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM escolher_liberacao  TABLES   pit_liberado STRUCTURE dd07v
                         CHANGING pe_tp_aprovacao TYPE ZESZDE_TIP_APROVACAO
                                  pe_domname      TYPE char30
                                  pe_ddtext	      TYPE val_text.

  DATA: lv_domname    TYPE char30,
        lv_domvalue_l TYPE ZESZDE_TIP_APROVACAO,
        lv_ddtext	    TYPE val_text.

  CLEAR: wa_opcoes, it_opcoes[].

  LOOP AT pit_liberado.
    MOVE-CORRESPONDING pit_liberado TO it_opcoes.
    APPEND it_opcoes.
  ENDLOOP.

* In├¡cio - RJF - 07.07.2022 - bloq. mult.
  IMPORT lv_domname    TO wa_opcoes-domname
         lv_domvalue_l TO wa_opcoes-domvalue_l
         lv_ddtext     TO wa_opcoes-ddtext FROM MEMORY ID 'B_MULT'.
  IF wa_opcoes-domname IS INITIAL
    AND wa_opcoes-domvalue_l IS INITIAL
    AND wa_opcoes-ddtext IS INITIAL.
* Fim - - RJF - 07.07.2022 - bloq. mult.

    CALL SCREEN 0101 STARTING AT 5 5.

* In├¡cio - RJF - 07.07.2022 - bloq. mult.
  ENDIF.
* Fim - - RJF - 07.07.2022 - bloq. mult.

  IF wa_opcoes IS NOT INITIAL.
    MOVE: wa_opcoes-domname    TO pe_domname,
          wa_opcoes-domvalue_l TO pe_tp_aprovacao,
          wa_opcoes-ddtext     TO pe_ddtext.

* In├¡cio - RJF - 07.07.2022 - bloq. mult.
    MOVE: pe_domname      TO lv_domname,
          pe_tp_aprovacao TO lv_domvalue_l,
          pe_ddtext       TO lv_ddtext.
    EXPORT: lv_domname
            lv_domvalue_l
            lv_ddtext TO MEMORY ID 'B_MULT'.
* Fim - - RJF - 07.07.2022 - bloq. mult.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'PF0101'.
  SET TITLEBAR  'TL0101'.

* In├¡cio - RJF - 07.07.2022 - bloq. mult.
  IMPORT gv_prim TO gv_prim FROM MEMORY ID 'B_MULT_P'.
  IF gv_prim IS INITIAL.
    IF ctl_alv_0101 IS NOT INITIAL.
      CALL METHOD ctl_alv_0101->refresh_table_display.
      FREE ctl_alv_0101.
    ENDIF.
    IF ctl_con_0101 IS NOT INITIAL.
      FREE ctl_con_0101.
    ENDIF.
  ENDIF.
* Fim - RJF - 07.07.2022 - bloq. mult.

  IF ctl_con_0101 IS INITIAL.

    CREATE OBJECT ctl_con_0101
      EXPORTING
        container_name = 'ALV_0101'.

    CREATE OBJECT ctl_alv_0101
      EXPORTING
        i_parent = ctl_con_0101.

    PERFORM fill_it_fieldcatalog_0101.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0101.
*   Set layout parameters for ALV grid

    gs_lay_0101-sel_mode   = 'A'.
    gs_lay_0101-zebra      = abap_true.

    CALL METHOD ctl_alv_0101->set_table_for_first_display
      EXPORTING
        is_layout       = gs_lay_0101
        is_variant      = gs_var_0101
        i_default       = space
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_catalog_0101
        it_outtab       = it_opcoes[].

    CALL METHOD ctl_alv_0101->refresh_table_display.

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_double_click FOR ctl_alv_0101.
  ELSE.
    CALL METHOD ctl_alv_0101->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0101->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0101
      es_row_no   = gs_scroll_row_0101.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0101.

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0101> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'DD07V'
    CHANGING
      ct_fieldcat      = it_catalog_0101.

*DOMNAME  CHAR  30  Denomina├º├úo de um dom├¡nio
*VALPOS NUMC  4 Chave de valores de um dom├¡nio
*DDLANGUAGE LANG  1 C├│digo de idioma
*DOMVALUE_L CHAR  10  Valores p/dom├¡nios: valor ind./limite inferior
*DOMVALUE_H CHAR  10  Valores p/dom├¡nios, limite superior
*DDTEXT CHAR  60  Texto breve para valores fixos
*DOMVAL_LD  CHAR  10  Valores para dom├¡nios dependentes de idioma, limite inferior
*DOMVAL_HD  CHAR  10  Valores para dom├¡nios dependentes de idioma, limite superior
*APPVAL CHAR  1 DD: indica se um VF do apendente pertence ao append

  lc_col_pos = 1.

  LOOP AT it_catalog_0101 ASSIGNING <fs_cat_0101>.
    CASE <fs_cat_0101>-fieldname.
        "WHEN 'DOMVALUE_L'.
        "  <FS_CAT_0101>-OUTPUTLEN = 03.
        "  <FS_CAT_0101>-SCRTEXT_L = 'C├│d'.
        "  <FS_CAT_0101>-SCRTEXT_M = 'C├│d'.
        "  <FS_CAT_0101>-SCRTEXT_S = 'C├│d'.
      WHEN 'DDTEXT'.
        <fs_cat_0101>-outputlen = 40.
        <fs_cat_0101>-scrtext_l = 'Descri├º├úo'.
        <fs_cat_0101>-scrtext_m = 'Descri├º├úo'.
        <fs_cat_0101>-scrtext_s = 'Descri├º├úo'.
      WHEN OTHERS.
        <fs_cat_0101>-no_out = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_N55

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0101.

  gs_var_0101-report      = sy-repid.
  gs_var_0101-handle      = '0101'.
  gs_var_0101-log_group   = abap_false.
  gs_var_0101-username    = abap_false.
  gs_var_0101-variant     = abap_false.
  gs_var_0101-text        = abap_false.
  gs_var_0101-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_N55

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
