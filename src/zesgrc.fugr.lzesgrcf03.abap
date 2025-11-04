*----------------------------------------------------------------------*
***INCLUDE LZGRCF03.
*----------------------------------------------------------------------*

CLASS lcl_alv_toolbar_1001a DEFINITION DEFERRED.
CLASS lcl_alv_toolbar_1001b DEFINITION DEFERRED.
CLASS lcl_alv_toolbar_1002a DEFINITION DEFERRED.
CLASS lcl_alv_toolbar_1002b DEFINITION DEFERRED.
CLASS lcl_alv_toolbar_1003a DEFINITION DEFERRED.
CLASS lcl_alv_toolbar_1003b DEFINITION DEFERRED.

DATA: ccontainer_1001       TYPE REF TO cl_gui_custom_container,

      ccontainer_1001a      TYPE REF TO cl_gui_container,
      it_fieldcatalog_1001a TYPE lvc_t_fcat,
      gs_variant_1001a      TYPE disvariant,
      gs_layout_1001a       TYPE lvc_s_layo,
      alv_1001a             TYPE REF TO cl_gui_alv_grid,
      toolbar_1001a         TYPE REF TO lcl_alv_toolbar_1001a,
      toolbarmanager_1001a  TYPE REF TO cl_alv_grid_toolbar_manager,

      ccontainer_1001b      TYPE REF TO cl_gui_container,
      it_fieldcatalog_1001b TYPE lvc_t_fcat,
      gs_variant_1001b      TYPE disvariant,
      gs_layout_1001b       TYPE lvc_s_layo,
      alv_1001b             TYPE REF TO cl_gui_alv_grid,
      toolbar_1001b         TYPE REF TO lcl_alv_toolbar_1001b,
      toolbarmanager_1001b  TYPE REF TO cl_alv_grid_toolbar_manager,

      splitter_1001         TYPE REF TO cl_gui_splitter_container.

DATA: ccontainer_1002       TYPE REF TO cl_gui_custom_container,

      ccontainer_1002a      TYPE REF TO cl_gui_container,
      it_fieldcatalog_1002a TYPE lvc_t_fcat,
      gs_variant_1002a      TYPE disvariant,
      gs_layout_1002a       TYPE lvc_s_layo,
      alv_1002a             TYPE REF TO cl_gui_alv_grid,
      toolbar_1002a         TYPE REF TO lcl_alv_toolbar_1002a,
      toolbarmanager_1002a  TYPE REF TO cl_alv_grid_toolbar_manager,

      ccontainer_1002b      TYPE REF TO cl_gui_container,
      it_fieldcatalog_1002b TYPE lvc_t_fcat,
      gs_variant_1002b      TYPE disvariant,
      gs_layout_1002b       TYPE lvc_s_layo,
      alv_1002b             TYPE REF TO cl_gui_alv_grid,
      toolbar_1002b         TYPE REF TO lcl_alv_toolbar_1002b,
      toolbarmanager_1002b  TYPE REF TO cl_alv_grid_toolbar_manager,

      splitter_1002         TYPE REF TO cl_gui_splitter_container.

DATA: ccontainer_1003       TYPE REF TO cl_gui_custom_container,

      ccontainer_1003a      TYPE REF TO cl_gui_container,
      it_fieldcatalog_1003a TYPE lvc_t_fcat,
      gs_variant_1003a      TYPE disvariant,
      gs_layout_1003a       TYPE lvc_s_layo,
      alv_1003a             TYPE REF TO cl_gui_alv_grid,
      toolbar_1003a         TYPE REF TO lcl_alv_toolbar_1003a,
      toolbarmanager_1003a  TYPE REF TO cl_alv_grid_toolbar_manager,

      ccontainer_1003b      TYPE REF TO cl_gui_container,
      it_fieldcatalog_1003b TYPE lvc_t_fcat,
      gs_variant_1003b      TYPE disvariant,
      gs_layout_1003b       TYPE lvc_s_layo,
      alv_1003b             TYPE REF TO cl_gui_alv_grid,
      toolbar_1003b         TYPE REF TO lcl_alv_toolbar_1003b,
      toolbarmanager_1003b  TYPE REF TO cl_alv_grid_toolbar_manager,

      splitter_1003         TYPE REF TO cl_gui_splitter_container.

CONTROLS: tabprinc TYPE TABSTRIP,
          tab01    TYPE TABLEVIEW USING SCREEN 1001,
          tab02    TYPE TABLEVIEW USING SCREEN 1002,
          tab03    TYPE TABLEVIEW USING SCREEN 1003.

CLASS lcl_alv_toolbar_1001a DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_1001b DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_1002a DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_1002b DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_1003a DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_1003b DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_1001a IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1001a
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-quickinfo = 'Incluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-quickinfo = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1001a->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.
    CLEAR: et_index_rows[].
    CALL METHOD alv_1001a->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    DATA: it_zde_zsdt0238 TYPE zde_zsdt0238_t.

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE gt_zde_zsdt0238 INTO DATA(wa_zde_zsdt0238) INDEX wa_index_rows-index.
      APPEND wa_zde_zsdt0238 TO it_zde_zsdt0238.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'EXCLUIR'.

        LOOP AT it_zde_zsdt0238 INTO wa_zde_zsdt0238.
          DELETE gt_zde_zsdt0238 WHERE sequencia EQ wa_zde_zsdt0238-sequencia.
        ENDLOOP.

        alv_1001a->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

      WHEN 'NOVO'.

        CALL SCREEN 2001 STARTING AT 55 05.
        alv_1001a->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_alv_toolbar_1001b IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1001b
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-quickinfo = 'Incluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-quickinfo = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1001b->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.
    CLEAR: et_index_rows[].
    CALL METHOD alv_1001b->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    DATA: it_zde_zsdt0240 TYPE zde_zsdt0240_t.

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE gt_zde_zsdt0240 INTO DATA(wa_zde_zsdt0240) INDEX wa_index_rows-index.
      APPEND wa_zde_zsdt0240 TO it_zde_zsdt0240.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'EXCLUIR'.

        LOOP AT it_zde_zsdt0240 INTO wa_zde_zsdt0240.
          DELETE gt_zde_zsdt0240 WHERE sequencia EQ wa_zde_zsdt0240-sequencia.
        ENDLOOP.

        alv_1001b->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

      WHEN 'NOVO'.

        CALL SCREEN 2002 STARTING AT 55 05.
        alv_1001b->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_alv_toolbar_1002a IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1002a
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-quickinfo = 'Incluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-quickinfo = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1002a->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.
    CLEAR: et_index_rows[].
    CALL METHOD alv_1002a->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    DATA: it_zde_zsdt0239 TYPE zde_zsdt0239_t.

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE gt_zde_zsdt0239 INTO DATA(wa_zde_zsdt0239) INDEX wa_index_rows-index.
      APPEND wa_zde_zsdt0239 TO it_zde_zsdt0239.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'EXCLUIR'.

        LOOP AT it_zde_zsdt0239 INTO wa_zde_zsdt0239.
          DELETE gt_zde_zsdt0239 WHERE sequencia EQ wa_zde_zsdt0239-sequencia.
          DELETE gt_zsdt0241 WHERE sequencia EQ wa_zde_zsdt0239-sequencia.
        ENDLOOP.

        alv_1002a->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

      WHEN 'NOVO'.

        CALL SCREEN 2003 STARTING AT 55 05.
        alv_1002a->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_alv_toolbar_1002b IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1002b
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-quickinfo = 'Incluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-quickinfo = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1002b->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.
    CLEAR: et_index_rows[].

    CALL METHOD alv_1002b->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    DATA: it_zde_zsdt0241 TYPE TABLE OF zsdt0241.

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE gt_zsdt0241 INTO DATA(wa_zsdt0241) INDEX wa_index_rows-index.
      APPEND wa_zsdt0241 TO it_zde_zsdt0241.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'EXCLUIR'.

        LOOP AT it_zde_zsdt0241 INTO DATA(wa_zde_zsdt0241).
          DELETE gt_zsdt0241
           WHERE sequencia EQ wa_zde_zsdt0241-sequencia
             AND chave     EQ wa_zde_zsdt0241-chave.
        ENDLOOP.

        alv_1002b->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

      WHEN 'NOVO'.

        CLEAR: wa_local_desc_sel.

        DESCRIBE TABLE gt_zde_zsdt0239 LINES DATA(qt_linhas).

        IF qt_linhas IS INITIAL.
          MESSAGE s007 DISPLAY LIKE 'E'.
          EXIT.
        ELSEIF qt_linhas EQ 1.
          READ TABLE gt_zde_zsdt0239 INDEX 1 INTO wa_local_desc_sel.
        ELSE.
          CLEAR: et_index_rows[].

          CALL METHOD alv_1002a->get_selected_rows
            IMPORTING
              et_index_rows = et_index_rows.

          IF et_index_rows[] IS INITIAL.
            MESSAGE s007 DISPLAY LIKE 'E'.
          ENDIF.

          READ TABLE et_index_rows INDEX 1 INTO wa_index_rows.
          READ TABLE gt_zde_zsdt0239 INDEX wa_index_rows-index INTO wa_local_desc_sel.

        ENDIF.

        CALL SCREEN 2004 STARTING AT 55 05.
        alv_1002b->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_alv_toolbar_1003a IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1003a
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-quickinfo = 'Incluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-quickinfo = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1003a->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.
    CLEAR: et_index_rows[].

    CALL METHOD alv_1003a->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    DATA: it_zde_zsdt0242 TYPE zde_zsdt0242_t.

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE gt_zde_zsdt0242 INTO DATA(wa_zde_zsdt0242) INDEX wa_index_rows-index.
      APPEND wa_zde_zsdt0242 TO it_zde_zsdt0242.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'EXCLUIR'.

        LOOP AT it_zde_zsdt0242 INTO wa_zde_zsdt0242.
          DELETE gt_zde_zsdt0242 WHERE lifnr EQ wa_zde_zsdt0242-lifnr.
        ENDLOOP.

        alv_1003a->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

      WHEN 'NOVO'.

        CALL SCREEN 2005 STARTING AT 55 05.
        alv_1003a->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_alv_toolbar_1003b IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1003b
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-quickinfo = 'Incluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-quickinfo = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1003b->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.
    CLEAR: et_index_rows[].
    CALL METHOD alv_1003b->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    DATA: it_zde_zsdt0243 TYPE TABLE OF zsdt0243.

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE gt_zsdt0243 INTO DATA(wa_zde_zsdt0243) INDEX wa_index_rows-index.
      APPEND wa_zde_zsdt0243 TO it_zde_zsdt0243.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'EXCLUIR'.

        LOOP AT it_zde_zsdt0243 INTO wa_zde_zsdt0243.
          DELETE gt_zsdt0243 WHERE pc_veiculo EQ wa_zde_zsdt0243-pc_veiculo.
        ENDLOOP.

        alv_1003b->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

      WHEN 'NOVO'.

        CALL SCREEN 2006 STARTING AT 55 05.
        alv_1003b->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
        LEAVE TO SCREEN 1000.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.


FORM carrega_empresa .

  CLEAR: zde_zsdt0237-butxt.

  IF zde_zsdt0237-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO @zde_zsdt0237-butxt
      FROM t001
     WHERE bukrs EQ @zde_zsdt0237-bukrs.
  ENDIF.

ENDFORM.

FORM carrega_filial .

  CLEAR: zde_zsdt0237-name.

  IF zde_zsdt0237-bukrs IS NOT INITIAL.
    SELECT SINGLE name INTO @zde_zsdt0237-name
      FROM j_1bbranch
     WHERE branch EQ @zde_zsdt0237-branch.
  ENDIF.

ENDFORM.

FORM carrega_municiopio_origem .

  CLEAR: zde_zsdt0237-tx_mun_origem.

  IF zde_zsdt0237-cmunini IS NOT INITIAL.
    SELECT SINGLE text INTO @zde_zsdt0237-tx_mun_origem
      FROM j_1btxjurt
     WHERE spras      EQ @sy-langu
       AND country    EQ @zde_zsdt0237-country
       AND taxjurcode EQ @zde_zsdt0237-cmunini.
  ENDIF.

ENDFORM.

FORM carrega_municiopio_final .

  CLEAR: zde_zsdt0237-tx_mun_final.

  IF zde_zsdt0237-cmunfim IS NOT INITIAL.
    SELECT SINGLE text INTO @zde_zsdt0237-tx_mun_final
      FROM j_1btxjurt
     WHERE spras      EQ @sy-langu
       AND country    EQ @zde_zsdt0237-country
       AND taxjurcode EQ @zde_zsdt0237-cmunfim.
  ENDIF.

ENDFORM.

FORM carrega_doc_eletronico .

  CLEAR: zde_zsdt0237-nfenum, zde_zsdt0237-series, zde_zsdt0237-chave.

  IF zde_zsdt0237-docnum IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(lc_mdfe)
      FROM j_1bnfe_active
     WHERE docnum EQ @zde_zsdt0237-docnum.

    IF sy-subrc IS INITIAL.
      zde_zsdt0237-nfenum = lc_mdfe-nfnum9.
      zde_zsdt0237-series = lc_mdfe-serie.
      zde_zsdt0237-chave  = lc_mdfe-regio && lc_mdfe-nfyear && lc_mdfe-nfmonth && lc_mdfe-stcd1 && lc_mdfe-model && lc_mdfe-serie &&
                            lc_mdfe-nfnum9 && lc_mdfe-docnum9 && lc_mdfe-cdv.
    ENDIF.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_LOCAL_CARREGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_local_carregamentos .

  CLEAR: gt_zde_zsdt0238[], gt_zde_zsdt0238.

  CHECK gt_zsdt0238[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_j_1btxjurt)
    FROM j_1btxjurt
     FOR ALL ENTRIES IN @gt_zsdt0238
   WHERE spras      EQ @sy-langu
     AND country    EQ @gt_zsdt0238-country
     AND taxjurcode EQ @gt_zsdt0238-cmuncar.

  SORT it_j_1btxjurt BY taxjurcode.

  LOOP AT gt_zsdt0238 INTO DATA(wa_zsdt0238).
    CLEAR zde_zsdt0238.
    MOVE-CORRESPONDING wa_zsdt0238 TO zde_zsdt0238.
    READ TABLE it_j_1btxjurt WITH KEY taxjurcode = zde_zsdt0238-cmuncar INTO DATA(wa_j_1btxjurt) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      zde_zsdt0238-text = wa_j_1btxjurt-text.
    ENDIF.
    APPEND zde_zsdt0238 TO gt_zde_zsdt0238.
  ENDLOOP.

ENDFORM.

FORM carrega_ufs_percurso .

  CLEAR: gt_zde_zsdt0240[], gt_zde_zsdt0240.

  CHECK gt_zsdt0240[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_t005u)
    FROM t005u
     FOR ALL ENTRIES IN @gt_zsdt0240
   WHERE spras EQ @sy-langu
     AND land1 EQ @gt_zsdt0240-land1
     AND bland EQ @gt_zsdt0240-bland.

  SORT it_t005u BY bland.

  LOOP AT gt_zsdt0240 INTO DATA(wa_zsdt0240).
    CLEAR zde_zsdt0240.
    MOVE-CORRESPONDING wa_zsdt0240 TO zde_zsdt0240.
    READ TABLE it_t005u WITH KEY bezei = zde_zsdt0240-bezei INTO DATA(wa_t005u) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      zde_zsdt0240-bezei = wa_t005u-bezei.
    ENDIF.
    APPEND zde_zsdt0240 TO gt_zde_zsdt0240.
  ENDLOOP.

ENDFORM.

FORM carrega_mun_descarregamentos .

  CLEAR: gt_zde_zsdt0239[], gt_zde_zsdt0239.

  CHECK gt_zsdt0239[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_j_1btxjurt)
    FROM j_1btxjurt
     FOR ALL ENTRIES IN @gt_zsdt0239
   WHERE spras      EQ @sy-langu
     AND country    EQ @gt_zsdt0239-country
     AND taxjurcode EQ @gt_zsdt0239-cmundesc.

  SORT it_j_1btxjurt BY taxjurcode.

  LOOP AT gt_zsdt0239 INTO DATA(wa_zsdt0239).
    CLEAR zde_zsdt0239.
    MOVE-CORRESPONDING wa_zsdt0239 TO zde_zsdt0239.
    READ TABLE it_j_1btxjurt WITH KEY taxjurcode = zde_zsdt0239-cmundesc INTO DATA(wa_j_1btxjurt) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      zde_zsdt0239-text = wa_j_1btxjurt-text.
    ENDIF.
    APPEND zde_zsdt0239 TO gt_zde_zsdt0239.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  CASE ok_code.
    WHEN 'GERAR'.
      CLEAR: ok_code.
      PERFORM gerar_mdfe.
    WHEN 'FECHAR'.
      CLEAR: ok_code.
    WHEN 'ESTORNAR'.
      CLEAR: ok_code.
    WHEN 'EDITAR'.
      CLEAR: ok_code.
    WHEN 'GRAVAR'.
      CLEAR: ok_code.
    WHEN 'TAB01'.
      gb_tela = '1001'.
      tabprinc-activetab = 'TAB01'.
    WHEN 'TAB02'.
      gb_tela = '1002'.
      tabprinc-activetab = 'TAB02'.
    WHEN 'TAB03'.
      gb_tela = '1003'.
      tabprinc-activetab = 'TAB03'.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  DATA: it_ucomm TYPE TABLE OF sy-ucomm.

  CLEAR: it_ucomm[], it_ucomm.


  IF gb_zsdt0237-docnum IS INITIAL.
    APPEND 'ESTORNAR' TO it_ucomm.
    APPEND 'GRAVAR'   TO it_ucomm.
    APPEND 'EDITAR'   TO it_ucomm.
  ELSE.

    LOOP AT SCREEN.
      IF screen-name NE 'TAB01' AND
         screen-name NE 'TAB02' AND
         screen-name NE 'TAB03' AND
         screen-name NE 'TABPRINC'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    APPEND 'GERAR' TO it_ucomm.

    SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
      FROM j_1bnfe_active
     WHERE docnum EQ @gb_zsdt0237-docnum.

    IF wa_j_1bnfe_active-scssta EQ space OR wa_j_1bnfe_active-docsta EQ '1'.
      APPEND 'EDITAR'   TO it_ucomm.
      APPEND 'ESTORNAR' TO it_ucomm.
    ENDIF.

    IF ck_editando EQ abap_false.
      APPEND 'GRAVAR'   TO it_ucomm.
    ENDIF.

  ENDIF.

  PERFORM carrega_empresa.
  PERFORM carrega_filial.
  PERFORM carrega_municiopio_origem.
  PERFORM carrega_municiopio_final.
  PERFORM carrega_cep.
  PERFORM carrega_prod_predominante.

  SET PF-STATUS 'PFMDFE' EXCLUDING it_ucomm.
  SET TITLEBAR 'TLMDFE'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERAR_MDFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_mdfe .

  DATA: mdfe TYPE REF TO zcl_mdfe.

  CREATE OBJECT mdfe.
  mdfe->set_data_emi( sy-datlo ).
  mdfe->set_hora_emi( sy-timlo ).

  IF ( sy-subrc = 0 ) AND ( zde_zsdt0237-docnum IS NOT INITIAL ).
    DATA: v_time_br TYPE erzet.
    CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
      EXPORTING
        i_bukrs  = zde_zsdt0237-bukrs
        i_branch = zde_zsdt0237-branch
      IMPORTING
        e_time   = v_time_br.
    IF v_time_br IS NOT INITIAL.
      mdfe->set_hora_emi( v_time_br ).
    ENDIF.
  ENDIF.

  "Adiciona UFs de Percurso.
  LOOP AT gt_zde_zsdt0240 INTO DATA(wa_zde_zsdt0240) WHERE NOT ( bland IS INITIAL ).
    mdfe->add_uf_perc( CONV #( wa_zde_zsdt0240-bland ) ).
  ENDLOOP.

  IF gt_zsdt0243[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_zlest0003)
      FROM zlest0002
       FOR ALL ENTRIES IN @gt_zsdt0243
     WHERE pc_veiculo EQ @gt_zsdt0243-pc_veiculo.
  ENDIF.

  READ TABLE it_zlest0003 INTO DATA(pc_tracao) WITH KEY tp_veiculo = '0'.
  IF sy-subrc IS INITIAL.
    mdfe->set_placa_cav( i_placa_cav = pc_tracao-pc_veiculo ).
  ENDIF.

  mdfe->set_placa_car1( i_placa_car1 = '' ).
  mdfe->set_placa_car2( i_placa_car2 = '' ).
  mdfe->set_placa_car3( i_placa_car3 = '' ).
  "MDFE->SET_PLACA_CAR4( I_PLACA_CAR1 = '' ).

  DATA: lv_tabix TYPE sy-tabix. "*-US 145384-18-07-2024-#145384-RJF-inicio
  LOOP AT gt_zsdt0243 INTO DATA(wa_zsdt0243) WHERE pc_veiculo NE pc_tracao-pc_veiculo.
    lv_tabix = lv_tabix + 1.  "*-US 145384-18-07-2024-#145384-RJF-inicio
*    CASE sy-tabix.         "*-US 145384-18-07-2024-#145384-RJF-inicio
    CASE lv_tabix.          "*-US 145384-18-07-2024-#145384-RJF-inicio
      WHEN 1.
        mdfe->set_placa_car1( i_placa_car1 = wa_zsdt0243-pc_veiculo ).
      WHEN 2.
        mdfe->set_placa_car2( i_placa_car2 = wa_zsdt0243-pc_veiculo ).
      WHEN 3.
        mdfe->set_placa_car3( i_placa_car3 = wa_zsdt0243-pc_veiculo ).
      WHEN 4.
        "MDFE->SET_PLACA_CAR4( I_PLACA_CAR3 = WA_ZSDT0243-PC_VEICULO ).
    ENDCASE.
  ENDLOOP.

  READ TABLE gt_zde_zsdt0242 INDEX 1 INTO DATA(wa_zsdt0242).
  IF sy-subrc IS INITIAL.
    mdfe->set_motorista( i_motorista  = wa_zsdt0242-lifnr ).
  ENDIF.

  "Adiciona Documentos ao MDF-e
  LOOP AT gt_zsdt0241 INTO DATA(wa_zsdt0241) WHERE docnum_ref IS NOT INITIAL.
    mdfe->add_documento( wa_zsdt0241-docnum_ref ).
  ENDLOOP.

  "01	Rodoviário
  "02	Aéreo
  "03	Hidroviário
  "04	Ferroviário

  mdfe->at_modal   = zde_zsdt0237-modal.
  mdfe->at_ufini   = zde_zsdt0237-cmunini(2).
  mdfe->at_cmunini = zde_zsdt0237-cmunini+3(7).

  SELECT SINGLE text INTO @mdfe->at_nmunini
    FROM j_1btxjurt
   WHERE spras      EQ @sy-langu
     AND country    EQ @zde_zsdt0237-country
     AND taxjurcode EQ @zde_zsdt0237-cmunini.

  mdfe->at_uffim   = zde_zsdt0237-cmunfim(2).
  mdfe->at_cmunfim = zde_zsdt0237-cmunfim+3(7).

  SELECT SINGLE text INTO @mdfe->at_nmunfim
    FROM j_1btxjurt
   WHERE spras      EQ @sy-langu
     AND country    EQ @zde_zsdt0237-country
     AND taxjurcode EQ @zde_zsdt0237-cmunfim.

  mdfe->at_qcarga    = zde_zsdt0237-qcarga.
  mdfe->at_vcarga    = zde_zsdt0237-vcarga.
  mdfe->at_cunid     = zde_zsdt0237-cunid.
  CASE zde_zsdt0237-cunid.
    WHEN '01'.
      mdfe->at_cunid_sap = 'KG'.
    WHEN '02'.
      mdfe->at_cunid_sap = 'TO'.
  ENDCASE.

  DATA: lc_zsdt0237 TYPE zsdt0237.
  MOVE-CORRESPONDING zde_zsdt0237 TO lc_zsdt0237.

  CLEAR: mdfe->at_it_uf_perc[].

  LOOP AT gt_zde_zsdt0240 INTO wa_zde_zsdt0240.
    APPEND VALUE #( uf = wa_zde_zsdt0240-bland ) TO mdfe->at_it_uf_perc.
  ENDLOOP.

  zde_zsdt0237-docnum = mdfe->gravar_mdfe( i_zsdt0237 = lc_zsdt0237 ).

  CHECK zde_zsdt0237-docnum IS NOT INITIAL.

  PERFORM gravar_mdfe.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GRAVAR_MDFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gravar_mdfe .

  DATA: wa_zsdt0238 TYPE zsdt0238.
  DATA: wa_zsdt0240 TYPE zsdt0240.
  DATA: wa_zsdt0239 TYPE zsdt0239.
  DATA: wa_zsdt0242 TYPE zsdt0242.

  CLEAR: gb_zsdt0237, gt_zsdt0238[], gt_zsdt0240[], gt_zsdt0239[], gt_zsdt0242[].

  MOVE-CORRESPONDING zde_zsdt0237 TO gb_zsdt0237.
  DELETE FROM zsdt0238 WHERE docnum EQ gb_zsdt0237-docnum AND docnum NE space.
  DELETE FROM zsdt0239 WHERE docnum EQ gb_zsdt0237-docnum AND docnum NE space.
  DELETE FROM zsdt0240 WHERE docnum EQ gb_zsdt0237-docnum AND docnum NE space.
  DELETE FROM zsdt0241 WHERE docnum EQ gb_zsdt0237-docnum AND docnum NE space.
  DELETE FROM zsdt0242 WHERE docnum EQ gb_zsdt0237-docnum AND docnum NE space.
  DELETE FROM zsdt0243 WHERE docnum EQ gb_zsdt0237-docnum AND docnum NE space.

  LOOP AT gt_zde_zsdt0238 INTO DATA(wa_zde_zsdt0238).
    CLEAR: wa_zsdt0238.
    MOVE-CORRESPONDING wa_zde_zsdt0238 TO wa_zsdt0238.
    wa_zsdt0238-docnum = gb_zsdt0237-docnum.
    APPEND wa_zsdt0238 TO gt_zsdt0238.
  ENDLOOP.

  LOOP AT gt_zde_zsdt0240 INTO DATA(wa_zde_zsdt0240).
    CLEAR: wa_zsdt0240.
    MOVE-CORRESPONDING wa_zde_zsdt0240 TO wa_zsdt0240.
    wa_zsdt0240-docnum = gb_zsdt0237-docnum.
    APPEND wa_zsdt0240 TO gt_zsdt0240.
  ENDLOOP.

  LOOP AT gt_zde_zsdt0239 INTO DATA(wa_zde_zsdt0239).
    CLEAR: wa_zsdt0239.
    MOVE-CORRESPONDING wa_zde_zsdt0239 TO wa_zsdt0239.
    wa_zsdt0239-docnum = gb_zsdt0237-docnum.
    APPEND wa_zsdt0239 TO gt_zsdt0239.
  ENDLOOP.

  LOOP AT gt_zde_zsdt0242 INTO DATA(wa_zde_zsdt0242).
    CLEAR: wa_zsdt0242.
    MOVE-CORRESPONDING wa_zde_zsdt0242 TO wa_zsdt0242.
    wa_zsdt0242-docnum = gb_zsdt0237-docnum.
    APPEND wa_zsdt0242 TO gt_zsdt0242.
  ENDLOOP.

  LOOP AT gt_zsdt0241 ASSIGNING FIELD-SYMBOL(<fs241>).
    <fs241>-docnum = gb_zsdt0237-docnum.
  ENDLOOP.

  LOOP AT gt_zsdt0243 ASSIGNING FIELD-SYMBOL(<fs243>).
    <fs243>-docnum = gb_zsdt0237-docnum.
  ENDLOOP.

  MODIFY zsdt0237 FROM gb_zsdt0237.
  MODIFY zsdt0238 FROM TABLE gt_zsdt0238.
  MODIFY zsdt0240 FROM TABLE gt_zsdt0240.
  MODIFY zsdt0239 FROM TABLE gt_zsdt0239.
  MODIFY zsdt0241 FROM TABLE gt_zsdt0241.
  MODIFY zsdt0243 FROM TABLE gt_zsdt0243.
  MODIFY zsdt0242 FROM TABLE gt_zsdt0242.
  COMMIT WORK AND WAIT.

  CALL FUNCTION 'Z_GRC_MDFE_LOAD'
    EXPORTING
      i_docnum = gb_zsdt0237-docnum.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.

  IF ccontainer_1001 IS INITIAL.

    CREATE OBJECT ccontainer_1001
      EXPORTING
        container_name = 'C1001'.

    CREATE OBJECT splitter_1001
      EXPORTING
        parent  = ccontainer_1001
        rows    = 1
        columns = 2.

    ccontainer_1001a = splitter_1001->get_container( row = 1 column = 1 ).
    ccontainer_1001b = splitter_1001->get_container( row = 1 column = 2 ).

    splitter_1001->set_column_width( EXPORTING id = 2 width = 30  ).

    "Criar ALV de Municípios de Carregmento
    CREATE OBJECT alv_1001a
      EXPORTING
        i_parent = ccontainer_1001a.

    PERFORM fill_it_fieldcatalog_1001a.

    PERFORM fill_gs_variant_1001a.

    gs_layout_1001a-sel_mode   = 'A'.
    gs_layout_1001a-zebra      = abap_true.
    gs_layout_1001a-cwidth_opt = abap_true.
    gs_layout_1001a-grid_title = 'Municípios de Carregamento'.

    CREATE OBJECT toolbar_1001a
      EXPORTING
        io_alv_grid = alv_1001a.

    SET HANDLER toolbar_1001a->on_toolbar FOR alv_1001a.
    SET HANDLER toolbar_1001a->handle_user_command FOR alv_1001a.

    CALL METHOD alv_1001a->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_1001a
        is_variant      = gs_variant_1001a
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_1001a
        it_outtab       = gt_zde_zsdt0238[].

    "Criar ALV de UF's do Percurso

    CREATE OBJECT alv_1001b
      EXPORTING
        i_parent = ccontainer_1001b.

    PERFORM fill_it_fieldcatalog_1001b.

    PERFORM fill_gs_variant_1001b.

    gs_layout_1001b-sel_mode   = 'A'.
    gs_layout_1001b-zebra      = abap_true.
    gs_layout_1001b-cwidth_opt = abap_true.
    gs_layout_1001b-grid_title = 'UF''s do Percurso'.

    CREATE OBJECT toolbar_1001b
      EXPORTING
        io_alv_grid = alv_1001b.

    SET HANDLER toolbar_1001b->on_toolbar FOR alv_1001b.
    SET HANDLER toolbar_1001b->handle_user_command FOR alv_1001b.

    CALL METHOD alv_1001b->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_1001b
        is_variant      = gs_variant_1001b
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_1001b
        it_outtab       = gt_zde_zsdt0240[].

  ENDIF.

  alv_1001a->refresh_table_display( ).
  alv_1001b->refresh_table_display( ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1001A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1001a .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_1001a[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0238'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_1001a.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1001A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1001a .

  gs_variant_1001a-report      = sy-repid.
  gs_variant_1001a-handle      = '0001'.
  gs_variant_1001a-log_group   = abap_false.
  gs_variant_1001a-username    = abap_false.
  gs_variant_1001a-variant     = abap_false.
  gs_variant_1001a-text        = abap_false.
  gs_variant_1001a-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2001 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.
      DATA: lc_tabix TYPE syst_tabix.
      LOOP AT gt_zde_zsdt0238 ASSIGNING FIELD-SYMBOL(<fs_0238>).
        <fs_0238>-sequencia = sy-tabix.
        lc_tabix = sy-tabix.
      ENDLOOP.

      ADD 1 TO lc_tabix.

      zde_zsdt0238-sequencia = lc_tabix.

      IF zde_zsdt0238-cmuncar IS NOT INITIAL.
        SELECT SINGLE text INTO @zde_zsdt0238-text
          FROM j_1btxjurt
         WHERE spras      EQ @sy-langu
           AND country    EQ @zde_zsdt0238-country
           AND taxjurcode EQ @zde_zsdt0238-cmuncar.

      ENDIF.

      APPEND zde_zsdt0238 TO gt_zde_zsdt0238.
      CLEAR: zde_zsdt0238.
      CLEAR: ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2001 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2001'.

  zde_zsdt0238-country = 'BR'.

  IF zde_zsdt0238-cmuncar IS NOT INITIAL.
    SELECT SINGLE text INTO @zde_zsdt0238-text
      FROM j_1btxjurt
     WHERE spras      EQ @sy-langu
       AND country    EQ @zde_zsdt0238-country
       AND taxjurcode EQ @zde_zsdt0238-cmuncar.

  ENDIF.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1001A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1001b .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_1001b[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0240'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_1001b.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1001A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1001b .

  gs_variant_1001b-report      = sy-repid.
  gs_variant_1001b-handle      = '0002'.
  gs_variant_1001b-log_group   = abap_false.
  gs_variant_1001b-username    = abap_false.
  gs_variant_1001b-variant     = abap_false.
  gs_variant_1001b-text        = abap_false.
  gs_variant_1001b-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2002 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2002'.

  zde_zsdt0240-land1 = 'BR'.

  IF zde_zsdt0240-bland IS NOT INITIAL.
    SELECT SINGLE bezei INTO @zde_zsdt0240-bezei
      FROM t005u
     WHERE spras EQ @sy-langu
       AND land1 EQ @zde_zsdt0240-land1
       AND bland EQ @zde_zsdt0240-bland.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2002_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2002 INPUT.


  CASE ok_code.
    WHEN 'SAVE'.

      LOOP AT gt_zde_zsdt0240 ASSIGNING FIELD-SYMBOL(<fs_0240>).
        <fs_0240>-sequencia = sy-tabix.
        lc_tabix = sy-tabix.
      ENDLOOP.

      ADD 1 TO lc_tabix.

      zde_zsdt0240-sequencia = lc_tabix.

      IF zde_zsdt0240-bland IS NOT INITIAL.
        SELECT SINGLE bezei INTO @zde_zsdt0240-bezei
          FROM t005u
         WHERE spras EQ @sy-langu
           AND land1 EQ @zde_zsdt0240-land1
           AND bland EQ @zde_zsdt0240-bland.
      ENDIF.

      APPEND zde_zsdt0240 TO gt_zde_zsdt0240.
      CLEAR: zde_zsdt0240.
      CLEAR: ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2003 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2003'.

  zde_zsdt0239-country = 'BR'.

  IF zde_zsdt0239-cmundesc IS NOT INITIAL.
    SELECT SINGLE text INTO @zde_zsdt0239-text
      FROM j_1btxjurt
     WHERE spras      EQ @sy-langu
       AND country    EQ @zde_zsdt0239-country
       AND taxjurcode EQ @zde_zsdt0239-cmundesc.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2003_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2003 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.

      LOOP AT gt_zde_zsdt0239 ASSIGNING FIELD-SYMBOL(<fs_0239>).
        <fs_0239>-sequencia = sy-tabix.
        lc_tabix = sy-tabix.
      ENDLOOP.

      ADD 1 TO lc_tabix.

      zde_zsdt0239-sequencia = lc_tabix.

      IF zde_zsdt0239-cmundesc IS NOT INITIAL.
        SELECT SINGLE text INTO @zde_zsdt0239-text
          FROM j_1btxjurt
         WHERE spras      EQ @sy-langu
           AND country    EQ @zde_zsdt0239-country
           AND taxjurcode EQ @zde_zsdt0239-cmundesc.

      ENDIF.

      APPEND zde_zsdt0239 TO gt_zde_zsdt0239.
      CLEAR: zde_zsdt0239.
      CLEAR: ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1002 OUTPUT.

  IF ccontainer_1002 IS INITIAL.

    CREATE OBJECT ccontainer_1002
      EXPORTING
        container_name = 'C1002'.

    CREATE OBJECT splitter_1002
      EXPORTING
        parent  = ccontainer_1002
        rows    = 2
        columns = 1.

    ccontainer_1002a = splitter_1002->get_container( row = 1 column = 1 ).
    ccontainer_1002b = splitter_1002->get_container( row = 2 column = 1 ).

    splitter_1002->set_row_height( EXPORTING id = 2 height = 60 ).

    "Criar ALV de Municípios de Descarregamento
    CREATE OBJECT alv_1002a
      EXPORTING
        i_parent = ccontainer_1002a.

    PERFORM fill_it_fieldcatalog_1002a.

    PERFORM fill_gs_variant_1002a.

    gs_layout_1002a-sel_mode   = 'A'.
    gs_layout_1002a-zebra      = abap_true.
    gs_layout_1002a-cwidth_opt = abap_true.
    gs_layout_1002a-grid_title = 'Municípios de Descarregamento'.

    CREATE OBJECT toolbar_1002a
      EXPORTING
        io_alv_grid = alv_1002a.

    SET HANDLER toolbar_1002a->on_toolbar FOR alv_1002a.
    SET HANDLER toolbar_1002a->handle_user_command FOR alv_1002a.

    CALL METHOD alv_1002a->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_1002a
        is_variant      = gs_variant_1002a
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_1002a
        it_outtab       = gt_zde_zsdt0239[].

    "Criar ALV de Documentos Referênciados

    CREATE OBJECT alv_1002b
      EXPORTING
        i_parent = ccontainer_1002b.

    PERFORM fill_it_fieldcatalog_1002b.

    PERFORM fill_gs_variant_1002b.

    gs_layout_1002b-sel_mode   = 'A'.
    gs_layout_1002b-zebra      = abap_true.
    gs_layout_1002b-cwidth_opt = abap_true.
    gs_layout_1002b-grid_title = 'Documentos Referênciados'.

    CREATE OBJECT toolbar_1002b
      EXPORTING
        io_alv_grid = alv_1002b.

    SET HANDLER toolbar_1002b->on_toolbar FOR alv_1002b.
    SET HANDLER toolbar_1002b->handle_user_command FOR alv_1002b.

    CALL METHOD alv_1002b->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_1002b
        is_variant      = gs_variant_1002b
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_1002b
        it_outtab       = gt_zsdt0241[].

  ENDIF.

  alv_1002a->refresh_table_display( ).
  alv_1002b->refresh_table_display( ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1002A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1002a .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_1002a[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0239'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_1002a.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1002A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1002a .

  gs_variant_1002a-report      = sy-repid.
  gs_variant_1002a-handle      = '0003'.
  gs_variant_1002a-log_group   = abap_false.
  gs_variant_1002a-username    = abap_false.
  gs_variant_1002a-variant     = abap_false.
  gs_variant_1002a-text        = abap_false.
  gs_variant_1002a-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1002B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1002b .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_1002b[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0241'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_1002b.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1002B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1002b .

  gs_variant_1002b-report      = sy-repid.
  gs_variant_1002b-handle      = '0004'.
  gs_variant_1002b-log_group   = abap_false.
  gs_variant_1002b-username    = abap_false.
  gs_variant_1002b-variant     = abap_false.
  gs_variant_1002b-text        = abap_false.
  gs_variant_1002b-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2004_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2004 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2004'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2004 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.
      zsdt0241-sequencia = wa_local_desc_sel-sequencia.
      APPEND zsdt0241 TO gt_zsdt0241.
      CLEAR: zsdt0241.
      CLEAR: ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_CONDUTORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_condutores .

  CLEAR: gt_zde_zsdt0242[], gt_zde_zsdt0242.

  CHECK gt_zsdt0242[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_lfa1)
    FROM lfa1
     FOR ALL ENTRIES IN @gt_zsdt0242
   WHERE lifnr EQ @gt_zsdt0242-lifnr.

  SORT it_lfa1 BY lifnr.

  LOOP AT gt_zsdt0242 INTO DATA(wa_zsdt0242).
    CLEAR zde_zsdt0242.
    MOVE-CORRESPONDING wa_zsdt0242 TO zde_zsdt0242.
    READ TABLE it_lfa1 WITH KEY lifnr = zde_zsdt0242-lifnr INTO DATA(wa_lfa1) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      zde_zsdt0242-name1 = wa_lfa1-name1.
      zde_zsdt0242-stcd2 = wa_lfa1-stcd2.
    ENDIF.
    APPEND zde_zsdt0242 TO gt_zde_zsdt0242.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1003 OUTPUT.

  IF ccontainer_1003 IS INITIAL.

    CREATE OBJECT ccontainer_1003
      EXPORTING
        container_name = 'C1003'.

    CREATE OBJECT splitter_1003
      EXPORTING
        parent  = ccontainer_1003
        rows    = 2
        columns = 1.

    ccontainer_1003a = splitter_1003->get_container( row = 1 column = 1 ).
    ccontainer_1003b = splitter_1003->get_container( row = 2 column = 1 ).

    splitter_1003->set_row_height( EXPORTING id = 2 height = 60 ).

    "Criar ALV de Municípios de Descarregamento
    CREATE OBJECT alv_1003a
      EXPORTING
        i_parent = ccontainer_1003a.

    PERFORM fill_it_fieldcatalog_1003a.

    PERFORM fill_gs_variant_1003a.

    gs_layout_1003a-sel_mode   = 'A'.
    gs_layout_1003a-zebra      = abap_true.
    gs_layout_1003a-cwidth_opt = abap_true.
    gs_layout_1003a-grid_title = 'Condutores'.

    CREATE OBJECT toolbar_1003a
      EXPORTING
        io_alv_grid = alv_1003a.

    SET HANDLER toolbar_1003a->on_toolbar FOR alv_1003a.
    SET HANDLER toolbar_1003a->handle_user_command FOR alv_1003a.

    CALL METHOD alv_1003a->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_1003a
        is_variant      = gs_variant_1003a
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_1003a
        it_outtab       = gt_zde_zsdt0242[].

    "Criar ALV de Documentos Referênciados

    CREATE OBJECT alv_1003b
      EXPORTING
        i_parent = ccontainer_1003b.

    PERFORM fill_it_fieldcatalog_1003b.

    PERFORM fill_gs_variant_1003b.

    gs_layout_1003b-sel_mode   = 'A'.
    gs_layout_1003b-zebra      = abap_true.
    gs_layout_1003b-cwidth_opt = abap_true.
    gs_layout_1003b-grid_title = 'Veículos'.

    CREATE OBJECT toolbar_1003b
      EXPORTING
        io_alv_grid = alv_1003b.

    SET HANDLER toolbar_1003b->on_toolbar FOR alv_1003b.
    SET HANDLER toolbar_1003b->handle_user_command FOR alv_1003b.

    CALL METHOD alv_1003b->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_1003b
        is_variant      = gs_variant_1003b
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_1003b
        it_outtab       = gt_zsdt0243[].

  ENDIF.

  alv_1003a->refresh_table_display( ).
  alv_1003b->refresh_table_display( ).


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2005_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2005_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2005 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.

      IF zde_zsdt0242-lifnr IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(wa_lfa1)
          FROM lfa1
         WHERE lifnr EQ @zde_zsdt0242-lifnr.

        IF sy-subrc IS INITIAL.
          zde_zsdt0242-name1 = wa_lfa1-name1.
          zde_zsdt0242-stcd2 = wa_lfa1-stcd2.
        ENDIF.
      ENDIF.

      APPEND zde_zsdt0242 TO gt_zde_zsdt0242.
      CLEAR: zde_zsdt0242.

      CLEAR: ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2005 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2005'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1003A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1003a .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_1003a[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0242'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_1003a.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1003A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1003a .

  gs_variant_1003a-report      = sy-repid.
  gs_variant_1003a-handle      = '0005'.
  gs_variant_1003a-log_group   = abap_false.
  gs_variant_1003a-username    = abap_false.
  gs_variant_1003a-variant     = abap_false.
  gs_variant_1003a-text        = abap_false.
  gs_variant_1003a-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1003B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1003b .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_1003b[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0243'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_1003b.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1003B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1003b .

  gs_variant_1003b-report      = sy-repid.
  gs_variant_1003b-handle      = '0006'.
  gs_variant_1003b-log_group   = abap_false.
  gs_variant_1003b-username    = abap_false.
  gs_variant_1003b-variant     = abap_false.
  gs_variant_1003b-text        = abap_false.
  gs_variant_1003b-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2006_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2006_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2006 OUTPUT.

  SET PF-STATUS 'PF2001'.
  SET TITLEBAR 'TL2006'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2006 INPUT.
  DATA: vlifnr_f TYPE lfa1-lifnr,
        vtipo(1).
  CASE ok_code.
    WHEN 'SAVE'.

      IF zsdt0243-pc_veiculo IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(wa_zlest0002)
          FROM zlest0002
         WHERE pc_veiculo EQ @zsdt0243-pc_veiculo.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE s008 DISPLAY LIKE 'E'.
          EXIT.
*-US 145384-18-07-2024-#145384-RJF-inicio
        ELSEIF sy-subrc IS INITIAL.
          IF gt_zsdt0243[] IS NOT INITIAL.
            SELECT * INTO @DATA(wa_zlest2)
              UP TO 1 ROWS
              FROM zlest0002
             FOR ALL ENTRIES IN @gt_zsdt0243
             WHERE pc_veiculo EQ @gt_zsdt0243-pc_veiculo
               AND ct_veiculo EQ '1'.
            ENDSELECT.
            IF sy-subrc IS INITIAL AND wa_zlest0002-ct_veiculo EQ wa_zlest2-ct_veiculo.
              MESSAGE s016 DISPLAY LIKE 'E'. " Validação: Já existe um veículo de categoria(Trator)!
              EXIT.
            ENDIF.
          ENDIF.
*-US 145384-18-07-2024-#145384-RJF-fim
        ENDIF.
*alterado por guilherme rabelo inicio
        DATA:wl_lfa1_prop TYPE lfa1.
        vtipo = 'P'.
        IF wa_zlest0002-propriet_comodato <> ' '.
          SELECT SINGLE stcd1
            FROM lfa1
            INTO @DATA(v_stcd1_c)
            WHERE lifnr = @wa_zlest0002-propriet_comodato.

*          SELECT SINGLE stcd1
*           FROM lfa1
*           INTO @DATA(v_stcd1_p)
*           WHERE lifnr = @wa_zlest0002-proprietario.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = zde_zsdt0237-branch
            IMPORTING
              output = vlifnr_f.

          SELECT SINGLE stcd1
            FROM lfa1
            INTO @DATA(v_stcd1_f)
           WHERE lifnr = @vlifnr_f.

          IF v_stcd1_f+0(8) = v_stcd1_c+0(8).
            vtipo = 'C'.
          ENDIF.

        ENDIF.

        IF wa_zlest0002-cto_comodato = '1' AND wa_zlest0002-propriet_comodato <> ' ' AND vtipo = 'C'.

          SELECT SINGLE *
              FROM lfa1 INTO wl_lfa1_prop
             WHERE lifnr = wa_zlest0002-propriet_comodato.
        ELSE.
*alterado por guilherme rabelo fim
          SELECT SINGLE *
            FROM lfa1 INTO wl_lfa1_prop
           WHERE lifnr = wa_zlest0002-proprietario.
        ENDIF.
        IF ( wl_lfa1_prop IS NOT INITIAL ).
          SELECT SINGLE *
            FROM adrc INTO @DATA(wa_adrc)
           WHERE addrnumber EQ @wl_lfa1_prop-adrnr.

          SELECT SINGLE *
            FROM adr6 INTO @DATA(wa_adr6)
           WHERE addrnumber EQ @wl_lfa1_prop-adrnr.
        ENDIF.

        zsdt0243-cpf     = wl_lfa1_prop-stcd2.
        zsdt0243-cnpj    = wl_lfa1_prop-stcd1.
        zsdt0243-rntrc   = wl_lfa1_prop-bahns.
        zsdt0243-x_nome  = wl_lfa1_prop-name1.
        zsdt0243-ie      = wl_lfa1_prop-stcd3.
        zsdt0243-uf_prop = wa_adrc-region.
        zsdt0243-tp_prop = '0'.

        zsdt0243-c_int    = wa_zlest0002-proprietario.
        zsdt0243-renavam  = wa_zlest0002-cd_renavam.
        zsdt0243-tara     = wa_zlest0002-tara.
        zsdt0243-cap_kg   = wa_zlest0002-cap_kg.
        zsdt0243-cap_m3   = wa_zlest0002-cap_m3.
        zsdt0243-tp_rod   = wa_zlest0002-tp_rodado.
        zsdt0243-tp_car   = wa_zlest0002-tp_carroceria2.
        zsdt0243-uf_veic  = wa_zlest0002-cd_uf.

        CASE wa_zlest0002-tp_veiculo.
          WHEN '0'.
            zsdt0243-tp_rod = '03'.
          WHEN '1'.
            IF zsdt0243-tp_rod = '0' OR zsdt0243-tp_rod = '00'.
              zsdt0243-tp_rod = '03'.
            ENDIF.
        ENDCASE.
      ENDIF.
      APPEND zsdt0243 TO gt_zsdt0243.
      CLEAR: zsdt0243.

      CLEAR: ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_PROD_PREDOMINANTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_prod_predominante .

  CLEAR: zde_zsdt0237-tpcarga,
         zde_zsdt0237-xprod.

  IF zde_zsdt0237-matnr IS NOT INITIAL.

    SELECT SINGLE matkl INTO @zde_zsdt0237-matkl
      FROM mara
     WHERE matnr EQ @zde_zsdt0237-matnr.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE * INTO @DATA(wa_zlest0193)
        FROM zlest0193
       WHERE matkl EQ @zde_zsdt0237-matkl.

      IF sy-subrc IS INITIAL.
        zde_zsdt0237-tpcarga = zcl_string=>lpad( i_str  = CONV #( wa_zlest0193-tipo_carga ) i_qtd  = 2 i_char = '0' ).
      ENDIF.

      SELECT SINGLE maktx INTO @zde_zsdt0237-xprod
        FROM makt
       WHERE matnr EQ @zde_zsdt0237-matnr
         AND spras EQ @sy-langu.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carrega_cep
*&---------------------------------------------------------------------*
FORM carrega_cep .

  SELECT c~pstcd_from, c~taxjurcode INTO TABLE @DATA(lt_cep) FROM  j_1btxjurt AS m
  INNER JOIN j_1btreg_city AS c ON m~country = c~country
    AND m~taxjurcode = c~taxjurcode
      WHERE m~text IN ( @zde_zsdt0237-tx_mun_origem,@zde_zsdt0237-tx_mun_final ).
  ""AND spras EQ 'P'. ""AHSS 11/07/2024 ajuste para buscar somente brasil


  READ TABLE lt_cep INTO DATA(ls_cep) WITH KEY taxjurcode = zde_zsdt0237-cmunini.
  "138695 CS2024000345 Ajustes emissão de MDFe manual - PSA DEVK9A1ZPM
  IF sy-subrc EQ 0.
    IF zde_zsdt0237-locc_cep IS INITIAL.
      zde_zsdt0237-locc_cep = ls_cep-pstcd_from.
    ENDIF.
    "    zde_zsdt0237-locc_cep = ls_cep-pstcd_from. "138695 CS2024000345 Ajustes emissão de MDFe manual - PSA DEVK9A1ZPM
  ENDIF.

  READ TABLE lt_cep INTO ls_cep WITH KEY taxjurcode = zde_zsdt0237-cmunfim.
  IF sy-subrc EQ 0.
    "138695 CS2024000345 Ajustes emissão de MDFe manual - PSA
    IF zde_zsdt0237-locd_cep IS INITIAL.
      zde_zsdt0237-locd_cep = ls_cep-pstcd_from.
    ENDIF.
    "    zde_zsdt0237-locd_cep = ls_cep-pstcd_from. "138695 CS2024000345 Ajustes emissão de MDFe manual - PSA
  ENDIF.


  IF zde_zsdt0237-locd_cep IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF '-' IN zde_zsdt0237-locd_cep WITH ''.
  ENDIF.

  IF zde_zsdt0237-locc_cep IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF '-' IN zde_zsdt0237-locc_cep WITH ''.
  ENDIF.


ENDFORM.
