*----------------------------------------------------------------------*
***INCLUDE LZCTE_DISTF05.
*----------------------------------------------------------------------*
TABLES: ZDE_CTE_DIST_VT_ALV,
        ZIB_CTE_DIST_NT_ALV.

*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER_0303 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER_0305 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS LCL_EVENT_RECEIVER_0306 DEFINITION.
  PUBLIC SECTION.
    METHODS: DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID IMPORTING E_MODIFIED ET_GOOD_CELLS.
ENDCLASS.                    "lcl_event_receiver DEFINITION

DATA: IT_05_NIT_AJUSTE   TYPE TABLE OF ZIB_CTE_DIST_NIT WITH HEADER LINE.

DATA: CTL_ALV_0303       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0303       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0303        TYPE LVC_S_LAYO,
      GS_VAR_0303        TYPE DISVARIANT,
      GS_SCROLL_COL_0303 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0303 TYPE LVC_S_ROID,
      IT_CATALOG_0303    TYPE LVC_T_FCAT,
      IT_EXCLUDE_0303    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0303    LIKE LINE OF IT_EXCLUDE_0303.

DATA: CTL_ALV_0305       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0305       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0305        TYPE LVC_S_LAYO,
      GS_VAR_0305        TYPE DISVARIANT,
      GS_SCROLL_COL_0305 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0305 TYPE LVC_S_ROID,
      IT_CATALOG_0305    TYPE LVC_T_FCAT,
      IT_EXCLUDE_0305    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0305    LIKE LINE OF IT_EXCLUDE_0305.

DATA: CTL_CON_0306       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0306        TYPE LVC_S_LAYO,
      GS_VAR_0306        TYPE DISVARIANT,
      CTL_ALV_0306       TYPE REF TO CL_GUI_ALV_GRID,
      GS_SCROLL_COL_0306 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0306 TYPE LVC_S_ROID,
      IT_CATALOG_0306    TYPE LVC_T_FCAT,
      IT_SELECTED_0306   TYPE LVC_T_ROW,
      WA_SELECTED_0306   TYPE LVC_S_ROW,
      IT_EXCLUDE_0306    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0306    LIKE LINE OF IT_EXCLUDE_0306,
      WA_STABLE_0306     TYPE LVC_S_STBL.

DATA: EVENT_HANDLER_0303  TYPE REF TO LCL_EVENT_HANDLER_0303.
DATA: EVENT_HANDLER_0305  TYPE REF TO LCL_EVENT_HANDLER_0305.
DATA: EVENT_HANDLER_0306  TYPE REF TO LCL_EVENT_RECEIVER_0306.

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER_0303 IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_0303 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

CLASS LCL_EVENT_HANDLER_0305 IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_0305 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

CLASS LCL_EVENT_RECEIVER_0306 IMPLEMENTATION.
  METHOD DATA_CHANGED_FINISHED.
    WA_STABLE_0306-ROW = ABAP_TRUE.
    WA_STABLE_0306-COL = ABAP_TRUE.
    CALL METHOD CTL_ALV_0306->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE_0306.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA: IT_05_VT                TYPE TABLE OF ZDE_CTE_DIST_VT_ALV WITH HEADER LINE,
      IT_05_DUP               TYPE TABLE OF ZIB_CTE_DIST_DUP    WITH HEADER LINE,
      IT_05_N55               TYPE TABLE OF ZIB_CTE_DIST_N55    WITH HEADER LINE,
      IT_05_N01               TYPE TABLE OF ZIB_CTE_DIST_N01    WITH HEADER LINE,
      IT_05_NIT               TYPE TABLE OF ZIB_CTE_DIST_NIT    WITH HEADER LINE,
      IT_05_NF_ALV            TYPE TABLE OF ZIB_CTE_DIST_NT_ALV WITH HEADER LINE,
      QT_LINHAS_VT            TYPE I,
      LC_ZIB_CTE              TYPE REF TO ZCL_CTE_PAGAMENTO,
      LC_INDEX_ATUAL          TYPE SY-TABIX,
      LC_INDEX_ULTIMO         TYPE SY-TABIX,
      LC_INDEX                TYPE I,
      LC_ORDEM                TYPE CHAR01,
      LC_TELA_BLOCK           TYPE CHAR01,
      LC_EXCLUDE_FCODE        TYPE TABLE OF SY-UCOMM,
      LC_PESO_LIBERADO        TYPE CHAR01,
      LC_ZBVTYP               TYPE CHAR01,
      LC_INFO_FORNE           TYPE TY_INFO_FORNE,
      LC_LFBK                 TYPE LFBK,
      LC_BNKA                 TYPE BNKA,
      LC_ALTEROU              TYPE CHAR01,
      LC_ALTEROU_PESO_CHEGADA TYPE CHAR01,
      LC_DOCNUM               TYPE J_1BDOCNUM,
      LC_TKNUM                TYPE TKNUM.

FIELD-SYMBOLS: <FS_05_N55> TYPE ZIB_CTE_DIST_N55,
               <FS_05_N01> TYPE ZIB_CTE_DIST_N01,
               <FS_05_NIT> TYPE ZIB_CTE_DIST_NIT,
               <FS_05_VT>  TYPE ZDE_CTE_DIST_VT_ALV.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0303
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_0303
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_05_VT INTO ZDE_CTE_DIST_VT_ALV INDEX ROW_ID.
  LC_INDEX_ATUAL = ROW_ID.

  CASE FIELDNAME.
    WHEN 'IC_EDITAR'.
      "Chamar Tela Individual """"""""""""""""""""""""""""""""""""""""""""""""
      "Chamar Tela Individual """"""""""""""""""""""""""""""""""""""""""""""""
      "Chamar Tela Individual """"""""""""""""""""""""""""""""""""""""""""""""
      CALL SCREEN 0304 STARTING AT 15 05.
      LEAVE TO SCREEN 0303.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  YCTE_FATURAR_INFO
*&---------------------------------------------------------------------*
*       Faturamento de CT-e --> Entrada de Dados para Pagamento
*----------------------------------------------------------------------*
FORM YCTE_FATURAR_INFO  USING    P_I_ZIB_CTE TYPE REF TO ZCL_CTE_PAGAMENTO
                                 P_I_INDEX_ULTIMO TYPE I
                        CHANGING P_I_INDEX TYPE I
                                 P_E_COMANDO TYPE CHAR01.

  CLEAR: LC_ORDEM, LC_INDEX.

  LC_ZIB_CTE = P_I_ZIB_CTE.
  LC_ZIB_CTE->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = ZIB_CTE_DIST_TER ).

  CLEAR: IT_05_VT[],
         IT_05_N55[],
         IT_05_N01[],
         IT_05_NIT[],
         IT_05_DUP[].

  IT_05_N55[] = LC_ZIB_CTE->GET_IT_N55( ).
  IT_05_N01[] = LC_ZIB_CTE->GET_IT_N01( ).
  IT_05_NIT[] = LC_ZIB_CTE->GET_IT_NIT( ).
  IT_05_DUP[] = LC_ZIB_CTE->GET_IT_DUP( ).
  IT_05_VT[]  = LC_ZIB_CTE->GET_IT_VT( ).

  LC_INDEX_ULTIMO = P_I_INDEX_ULTIMO.

  LC_TELA_BLOCK = ABAP_TRUE.
  LC_ZBVTYP     = ABAP_TRUE.

  "Distribuir Valores
  IF ZIB_CTE_DIST_TER-CK_FINALIZADO EQ ABAP_FALSE AND LC_ZIB_CTE->GET_CK_AUTORIZADO_PAGAMENTO( ) EQ ABAP_TRUE.
    LC_TELA_BLOCK = ABAP_FALSE.
  ENDIF.

  IF LC_ZIB_CTE->GET_CK_AUTORIZADO_PAGAMENTO( ) EQ ABAP_FALSE.
    MESSAGE S107.
  ENDIF.

  DESCRIBE TABLE IT_05_VT LINES QT_LINHAS_VT.
  LC_ALTEROU = ABAP_FALSE.

  IF QT_LINHAS_VT EQ 0.
    LC_INDEX = P_I_INDEX.
    ADD 1 TO LC_INDEX.
    MESSAGE I088.
  ELSEIF QT_LINHAS_VT EQ 1.
    LC_INDEX_ATUAL = 1.
    LC_INDEX = P_I_INDEX.
    READ TABLE IT_05_VT INTO ZDE_CTE_DIST_VT_ALV INDEX LC_INDEX_ATUAL.
    "Chamar Tela Individual """"""""""""""""""""""""""""""""""""""""""""""""
    "Chamar Tela Individual """"""""""""""""""""""""""""""""""""""""""""""""
    "Chamar Tela Individual """"""""""""""""""""""""""""""""""""""""""""""""
    CALL SCREEN 0304 STARTING AT 15 05.
  ELSEIF QT_LINHAS_VT GT 1.
    CALL SCREEN 0303 STARTING AT 15 02.
  ENDIF.

  P_I_INDEX   = LC_INDEX.
  P_E_COMANDO = LC_ORDEM.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0303  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0303 OUTPUT.

  CLEAR: LC_EXCLUDE_FCODE.

  IF LC_INDEX_ULTIMO EQ 1.
    APPEND OK_PRIMEIRO TO LC_EXCLUDE_FCODE.
    APPEND OK_ANTERIOR TO LC_EXCLUDE_FCODE.
    APPEND OK_PROXIMO  TO LC_EXCLUDE_FCODE.
    APPEND OK_ULTIMO   TO LC_EXCLUDE_FCODE.
  ENDIF.

  IF LC_TELA_BLOCK EQ ABAP_TRUE.
    APPEND OK_SALVAR TO LC_EXCLUDE_FCODE.
  ENDIF.

  SET PF-STATUS 'PF0301' EXCLUDING LC_EXCLUDE_FCODE.
  SET TITLEBAR 'TL0301'.

  IF CTL_CON_0303 IS INITIAL.

    CREATE OBJECT CTL_CON_0303
      EXPORTING
        CONTAINER_NAME = 'ALV_TKNUM'.

    CREATE OBJECT CTL_ALV_0303
      EXPORTING
        I_PARENT = CTL_CON_0303.

    PERFORM FILL_IT_FIELDCATALOG_0303.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0303.
*   Set layout parameters for ALV grid

    GS_LAY_0303-SEL_MODE   = SPACE.
    GS_LAY_0303-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0303->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0303
        IS_VARIANT           = GS_VAR_0303
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0303
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0303
        IT_OUTTAB            = IT_05_VT[].

    CALL METHOD CTL_ALV_0303->REFRESH_TABLE_DISPLAY.

    CREATE OBJECT EVENT_HANDLER_0303.
    SET HANDLER EVENT_HANDLER_0303->HANDLE_HOTSPOT_CLICK
            FOR CTL_ALV_0303.

  ELSE.
    CALL METHOD CTL_ALV_0303->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0303->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0303
      ES_ROW_NO   = GS_SCROLL_ROW_0303.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0303
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0303 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0303> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_CTE_DIST_VT_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0303.

  LOOP AT IT_CATALOG_0303 ASSIGNING <FS_CAT_0303>.
    CASE <FS_CAT_0303>-FIELDNAME.
      WHEN 'IC_EDITAR'.
        <FS_CAT_0303>-COL_POS = 1.
    ENDCASE.
  ENDLOOP.

  LC_COL_POS = 2.

  LOOP AT IT_CATALOG_0303 ASSIGNING <FS_CAT_0303>.
    IF <FS_CAT_0303>-COL_POS IS INITIAL.
      <FS_CAT_0303>-COL_POS = LC_COL_POS.
      ADD 1 TO LC_COL_POS.
    ENDIF.
    <FS_CAT_0303>-TABNAME = 'IT_05_VT'.
    CASE <FS_CAT_0303>-FIELDNAME.
      WHEN 'IC_EDITAR'.
        <FS_CAT_0303>-KEY     = ABAP_TRUE.
        <FS_CAT_0303>-HOTSPOT = ABAP_TRUE.
        <FS_CAT_0303>-JUST    = 'C'.
      WHEN 'ZVLR_VI' OR 'ZVLR_FRETE' OR 'ZVLR_MERCADORIA' OR 'PESO_ORIGEM' OR 'PESO_CHEGADA' OR 'ZPESO_DIFERENCA' OR
           'ZQUEBRA' OR 'ZPERDA' OR 'ZVLR_QUEBRA' OR 'ZVLR_PERDA' OR 'ZVLR_LIQ_PAGAR'.
        <FS_CAT_0303>-DO_SUM  = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0303
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0303 .

  GS_VAR_0303-REPORT      = SY-REPID.
  GS_VAR_0303-HANDLE      = '0303'.
  GS_VAR_0303-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0303-USERNAME    = ABAP_FALSE.
  GS_VAR_0303-VARIANT     = ABAP_FALSE.
  GS_VAR_0303-TEXT        = ABAP_FALSE.
  GS_VAR_0303-DEPENDVARS  = ABAP_FALSE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0303_EXIT INPUT.

  CASE OK_CODE.
    WHEN OK_PRIMEIRO.
      IF LC_INDEX NE 1.
        IF LC_ZIB_CTE IS NOT INITIAL.
          IF LC_ZIB_CTE->GET_CK_ALTERADO( ) EQ ABAP_TRUE.
            CHECK LC_ZIB_CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) EQ ABAP_TRUE.
          ENDIF.
        ENDIF.
        LC_INDEX = 1.
      ENDIF.
    WHEN OK_ANTERIOR.
      IF LC_INDEX NE 1.
        IF LC_ZIB_CTE IS NOT INITIAL.
          IF LC_ZIB_CTE->GET_CK_ALTERADO( ) EQ ABAP_TRUE.
            CHECK LC_ZIB_CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) EQ ABAP_TRUE.
          ENDIF.
        ENDIF.
        ADD -1 TO LC_INDEX.
      ENDIF.
    WHEN OK_PROXIMO.
      IF LC_INDEX NE LC_INDEX_ULTIMO.
        IF LC_ZIB_CTE IS NOT INITIAL.
          IF LC_ZIB_CTE->GET_CK_ALTERADO( ) EQ ABAP_TRUE.
            CHECK LC_ZIB_CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) EQ ABAP_TRUE.
          ENDIF.
        ENDIF.
        ADD 1 TO LC_INDEX.
      ENDIF.
    WHEN OK_ULTIMO.
      IF LC_INDEX NE LC_INDEX_ULTIMO.
        IF LC_ZIB_CTE IS NOT INITIAL.
          IF LC_ZIB_CTE->GET_CK_ALTERADO( ) EQ ABAP_TRUE.
            CHECK LC_ZIB_CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) EQ ABAP_TRUE.
          ENDIF.
        ENDIF.
        LC_INDEX = LC_INDEX_ULTIMO.
      ENDIF.
    WHEN OK_CANCEL.
      LC_ORDEM = ABAP_TRUE.
  ENDCASE.

  LC_ALTEROU = ABAP_FALSE.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0303 INPUT.

  CASE OK_CODE.
    WHEN OK_SALVAR.
      IF LC_ZIB_CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) EQ ABAP_TRUE.
        LC_ALTEROU = ABAP_FALSE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0304  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0304 OUTPUT.

  IF LC_ALTEROU EQ ABAP_TRUE.
    LC_ZIB_CTE->SET_ZDT_MOV( EXPORTING I_ZDT_MOV = SY-DATUM ).
    LC_ZIB_CTE->SET_ZBVTYP( EXPORTING I_ZBVTYP = ZIB_CTE_DIST_TER-ZBVTYP ).
    LC_ZIB_CTE->SET_ZDT_VENCTO( EXPORTING I_ZDT_VENCTO = ZIB_CTE_DIST_TER-ZDT_VENCTO ).
    LC_ZIB_CTE->SET_MWSKZ( EXPORTING I_MWSKZ = ZIB_CTE_DIST_TER-MWSKZ ).
    LC_ZIB_CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = ZIB_CTE_DIST_TER-DT_CHEGADA ).
  ENDIF.

  LC_ZIB_CTE->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = ZIB_CTE_DIST_TER ).

  CLEAR: LC_EXCLUDE_FCODE.

  IF LC_INDEX EQ 1.
    APPEND OK_PRIMEIRO TO LC_EXCLUDE_FCODE.
    APPEND OK_ANTERIOR TO LC_EXCLUDE_FCODE.
  ENDIF.
  IF LC_INDEX EQ LC_INDEX_ULTIMO.
    APPEND OK_PROXIMO  TO LC_EXCLUDE_FCODE.
    APPEND OK_ULTIMO   TO LC_EXCLUDE_FCODE.
  ENDIF.

  LC_PESO_LIBERADO = ABAP_TRUE.

  IF LC_TELA_BLOCK EQ ABAP_TRUE.
    APPEND OK_SALVAR    TO LC_EXCLUDE_FCODE.
    APPEND OK_CONFIRMAR TO LC_EXCLUDE_FCODE.

    LOOP AT SCREEN.
      IF SCREEN-INPUT NE 0.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
    CASE ZIB_CTE_DIST_TER-CD_TIPO_CTE.
      WHEN 1. "1  CT-e de Complemento de Valores
        LOOP AT SCREEN.
          IF ( SCREEN-INPUT NE 0 ) AND
             (
               "SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-MWSKZ' OR
               SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR
               SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-ZVLR_FRETE' OR
               SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_ORIGEM' OR
               SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA' ).
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    "Valores informados na autorização de pagamento
    IF IT_05_VT-CK_AUTORIZADO EQ ABAP_TRUE.
      LOOP AT SCREEN.
        IF ( SCREEN-INPUT NE 0 ) AND
           ( SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR
             SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-ZVLR_FRETE' OR
             SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_ORIGEM' OR
             SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA' ).
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ZIB_CTE_DIST_TER-CD_MODAL EQ '04'.
      LOOP AT SCREEN.
        IF ( SCREEN-INPUT NE 0 ) AND
           ( SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA' OR SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA' ).
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE IT_05_VT INDEX LC_INDEX_ATUAL.
    READ TABLE IT_05_N55 WITH KEY TKNUM = IT_05_VT-TKNUM.
    IF SY-SUBRC IS NOT INITIAL.
      READ TABLE IT_05_N01 WITH KEY TKNUM = IT_05_VT-TKNUM.
      LC_DOCNUM = IT_05_N01-DOCNUM_NF.
    ELSE.
      LC_DOCNUM = IT_05_N55-DOCNUM_NFE.
    ENDIF.
    LC_TKNUM = IT_05_VT-TKNUM.

    CASE ZIB_CTE_DIST_TER-CD_MODAL .
      WHEN '03'.

        LOOP AT SCREEN.
          IF SCREEN-INPUT NE '0'.
            SCREEN-INPUT = '0'.
          ENDIF.
          IF SCREEN-NAME EQ 'ZIB_CTE_DIST_TER-DT_CHEGADA'.
            SCREEN-INPUT = '1'.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.

      WHEN OTHERS.

        IF ( ( LC_ZIB_CTE->GET_QT_REG_NF( EXPORTING I_TKNUM  = LC_TKNUM  ) EQ 1 AND
               LC_ZIB_CTE->GET_QT_REG_NI( EXPORTING I_DOCNUM = LC_DOCNUM ) EQ 1 ) OR
             ( LC_ZIB_CTE->GET_CK_FATURA_PELA_VT( ) EQ ABAP_TRUE ) )
           AND LC_ZIB_CTE->GET_CK_ENTRADA_MANUAL_PESO( ) EQ ABAP_TRUE.

          IF LC_ALTEROU_PESO_CHEGADA EQ ABAP_TRUE.
            "Nota Fiscal Possui Somente um Item
            READ TABLE IT_05_NIT WITH KEY DOCNUM = LC_DOCNUM.
            IT_05_NIT-ZVLR_VI         = ZDE_CTE_DIST_VT_ALV-ZVLR_VI.
            IT_05_NIT-ZVLR_FRETE      = ZDE_CTE_DIST_VT_ALV-ZVLR_FRETE.
            IT_05_NIT-PESO_ORIGEM     = ZDE_CTE_DIST_VT_ALV-PESO_ORIGEM.
            IT_05_NIT-PESO_CHEGADA    = ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA.
            IT_05_NIT-ZVLR_MERCADORIA = ZDE_CTE_DIST_VT_ALV-ZVLR_MERCADORIA.
            CALL METHOD LC_ZIB_CTE->SET_PESO_CHEGADA_ITEM
              EXPORTING
                I_NIT = IT_05_NIT.
            IT_05_N55[] = LC_ZIB_CTE->GET_IT_N55( ).
            IT_05_N01[] = LC_ZIB_CTE->GET_IT_N01( ).
            IT_05_NIT[] = LC_ZIB_CTE->GET_IT_NIT( ).
            IT_05_DUP[] = LC_ZIB_CTE->GET_IT_DUP( ).
            IT_05_VT[]  = LC_ZIB_CTE->GET_IT_VT( ).
            READ TABLE IT_05_VT INTO ZDE_CTE_DIST_VT_ALV WITH KEY TKNUM = LC_TKNUM.
          ELSE.
            IT_05_VT[]  = LC_ZIB_CTE->GET_IT_VT( ).
            READ TABLE IT_05_VT INTO ZDE_CTE_DIST_VT_ALV WITH KEY TKNUM = LC_TKNUM.
          ENDIF.

          IF LC_ZIB_CTE->GET_CK_FATURA_PELA_VT( ) EQ ABAP_TRUE.
            LOOP AT SCREEN.
              IF ( SCREEN-INPUT NE 1 ) AND
                 ( SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-ZVLR_FRETE' OR
                   SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-ZVLR_VI' OR
                   SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_ORIGEM' ).
                SCREEN-INPUT = 1.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          ENDIF.

          LOOP AT SCREEN.
            IF ( SCREEN-INPUT NE 1 ) AND
               ( SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-DT_CHEGADA' OR
                 SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA' ).
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            ENDIF.
            IF ( SCREEN-INPUT NE 0 ) AND ( SCREEN-NAME EQ 'BTN_NOTAS' ).
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

        ELSEIF LC_ZIB_CTE->GET_CK_ENTRADA_MANUAL_PESO( ) EQ ABAP_FALSE.

          IT_05_VT[]  = LC_ZIB_CTE->GET_IT_VT( ).
          READ TABLE IT_05_VT INTO ZDE_CTE_DIST_VT_ALV WITH KEY TKNUM = LC_TKNUM.

          LOOP AT SCREEN.
            IF ( SCREEN-INPUT NE 0 ) AND ( SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA' OR SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-DT_CHEGADA' ).
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
            IF ( SCREEN-INPUT NE 0 ) AND ( SCREEN-NAME EQ 'BTN_NOTAS' ).
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

        ELSEIF LC_ZIB_CTE->GET_CK_ENTRADA_MANUAL_PESO( ) EQ ABAP_TRUE.

          "Não autorizado peso/data chegada manual
          LOOP AT SCREEN.
            IF ( SCREEN-INPUT NE 0 ) AND ( SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA' OR SCREEN-NAME EQ 'ZDE_CTE_DIST_VT_ALV-DT_CHEGADA' ).
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

          IF LC_ZIB_CTE->GET_QT_REG_NF( EXPORTING I_TKNUM  = IT_05_VT-TKNUM ) NE 1 OR LC_ZIB_CTE->GET_QT_REG_NI( EXPORTING I_DOCNUM = LC_DOCNUM ) NE 1.
            LOOP AT SCREEN.
              IF ( SCREEN-INPUT NE 1 ) AND ( SCREEN-NAME EQ 'BTN_NOTAS' ).
                SCREEN-INPUT = 1.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          ELSE.
            LOOP AT SCREEN.
              IF ( SCREEN-INPUT NE 0 ) AND ( SCREEN-NAME EQ 'BTN_NOTAS' ).
                SCREEN-INPUT = 0.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDIF.

  "Se existir somente uma VT não precisa confirmar
  IF QT_LINHAS_VT EQ 1.
    APPEND OK_CONFIRMAR TO LC_EXCLUDE_FCODE.
  ELSE.
    APPEND OK_PRIMEIRO TO LC_EXCLUDE_FCODE.
    APPEND OK_ANTERIOR TO LC_EXCLUDE_FCODE.
    APPEND OK_PROXIMO  TO LC_EXCLUDE_FCODE.
    APPEND OK_ULTIMO   TO LC_EXCLUDE_FCODE.
    APPEND OK_SALVAR   TO LC_EXCLUDE_FCODE.
  ENDIF.

  IF LC_ZBVTYP EQ ABAP_TRUE.
    CLEAR: LC_INFO_FORNE.
    CALL METHOD ZCL_CTE_DIST_G=>BUSCA_BANCO_PARCEIRO
      IMPORTING
        E_LFBK     = LC_LFBK
        E_BNKA     = LC_BNKA
      CHANGING
        P_CTE      = ZIB_CTE_DIST_TER
      EXCEPTIONS
        ERRO_BANCO = 1
        OTHERS     = 2.

    IF SY-SUBRC IS INITIAL.
      LC_INFO_FORNE-BVTYP = LC_LFBK-BVTYP.
      LC_INFO_FORNE-BANKL = LC_BNKA-BANKL(3).
      LC_INFO_FORNE-BANKA = LC_BNKA-BANKA.
      LC_INFO_FORNE-BANKN = LC_LFBK-BANKN.

      IF NOT LC_LFBK-BKONT IS INITIAL.
        CONCATENATE LC_LFBK-BANKL+4(11) '-' LC_LFBK-BKONT INTO LC_INFO_FORNE-AGENC.
      ELSE.
        LC_INFO_FORNE-AGENC = LC_LFBK-BANKL+4(11).
      ENDIF.
    ENDIF.
    LC_ZBVTYP = ABAP_FALSE.
  ENDIF.

  SET PF-STATUS 'PF0302' EXCLUDING LC_EXCLUDE_FCODE.
  SET TITLEBAR 'TL0302' WITH IT_05_VT-TKNUM.
  LC_ALTEROU = ABAP_FALSE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PRIORIDADE_BANCO_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_PRIORIDADE_BANCO_0304 INPUT.
  LC_ZBVTYP  = ABAP_TRUE.
  LC_ALTEROU = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_INFORMACAO_FRETE_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_INFORMACAO_FRETE_0304 INPUT.
  LC_ALTEROU = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0304 INPUT.

  CHECK LC_ALTEROU EQ ABAP_FALSE.

  CASE OK_CODE.

    WHEN OK_BTN_NOTAS.

      PERFORM INFORMA_DADOS_POR_NOTA USING ZDE_CTE_DIST_VT_ALV.

    WHEN OK_CONFIRMAR OR OK_SALVAR.

      IF ZDE_CTE_DIST_VT_ALV-ZQUEBRA GT 0 AND ZDE_CTE_DIST_VT_ALV-ZVLR_QUEBRA EQ 0.
        MESSAGE S171 DISPLAY LIKE 'E'.
        CLEAR OK_CODE.
        EXIT.
      ENDIF.

      IF ZIB_CTE_DIST_TER-ZDT_VENCTO LT SY-DATUM.
        MESSAGE S100 DISPLAY LIKE 'E'.
        CLEAR OK_CODE.
        EXIT.
      ENDIF.

      IF ZDE_CTE_DIST_VT_ALV-PESO_CHEGADA IS INITIAL AND LC_ZIB_CTE->GET_CK_ENTRADA_MANUAL_PESO( ) EQ ABAP_FALSE.
        MESSAGE S169 DISPLAY LIKE 'E'.
        CLEAR OK_CODE.
        EXIT.
      ENDIF.

      IF ZIB_CTE_DIST_TER-MWSKZ IS INITIAL.
        MESSAGE S069 DISPLAY LIKE 'E'.
        CLEAR OK_CODE.
        EXIT.
      ENDIF.

      "Salvar caso somente exista um registro de VT na CT-e
      "Confirmar caso existe mais de um regsitro de VT na CT-e
      ZDE_CTE_DIST_VT_ALV-CK_PESO_DIGITADO = ABAP_TRUE.
      CLEAR: LC_ORDEM.

      "Salvar Registro quando Existir Somente uma VT
      IF QT_LINHAS_VT EQ 1.
        IF LC_ZIB_CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) NE ABAP_TRUE.
          EXIT.
        ENDIF.
      ENDIF.

      IF LC_INDEX NE LC_INDEX_ULTIMO.
        ADD 1 TO LC_INDEX.
      ELSEIF LC_INDEX EQ LC_INDEX_ULTIMO.
        LC_ORDEM = ABAP_TRUE.
      ELSE.
        IF QT_LINHAS_VT EQ 1.
          LC_ORDEM = ABAP_TRUE.
        ENDIF.
      ENDIF.

      LC_ALTEROU = ABAP_FALSE.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_PESO_CHEGADA_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_PESO_CHEGADA_0304 INPUT.
  LC_ALTEROU = ABAP_TRUE.
  LC_ALTEROU_PESO_CHEGADA = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_DATA_VENCIMENTO_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICAR_DATA_VENCIMENTO_0304 INPUT.

  CALL METHOD ZCL_CTE_DIST_G=>VERIFICA_VENCIMENTO_FATURA
    EXPORTING
      I_DATA_VENCIMENTO = ZIB_CTE_DIST_TER-ZDT_VENCTO
    EXCEPTIONS
      NAO_VALIDA        = 1
      OTHERS            = 2.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LC_ZIB_CTE->SET_ZDT_VENCTO( EXPORTING I_ZDT_VENCTO = ZIB_CTE_DIST_TER-ZDT_VENCTO ).
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INFORMA_DADOS_POR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_CTE_DIST_VT_ALV  text
*----------------------------------------------------------------------*
FORM INFORMA_DADOS_POR_NOTA  USING P_VT_ALV TYPE ZDE_CTE_DIST_VT_ALV.

  IT_05_NIT[] = LC_ZIB_CTE->GET_IT_NIT( ).

  PERFORM POPULA_NOTAS_TKNUM USING P_VT_ALV-TKNUM.

  IF LC_ZIB_CTE->GET_QT_REG_NF( EXPORTING I_TKNUM = P_VT_ALV-TKNUM ) = 1.
    READ TABLE IT_05_NF_ALV INTO ZIB_CTE_DIST_NT_ALV INDEX 1.
    PERFORM SELECIONA_ITENS_DA_NOTA USING ZIB_CTE_DIST_NT_ALV-DOCNUM.
    CALL SCREEN 0306 STARTING AT 16 06.
  ELSE.
    "Escolher Nota Fiscal
    CALL SCREEN 0305 STARTING AT 16 06.
  ENDIF.

  IT_05_N55[] = LC_ZIB_CTE->GET_IT_N55( ).
  IT_05_N01[] = LC_ZIB_CTE->GET_IT_N01( ).
  IT_05_NIT[] = LC_ZIB_CTE->GET_IT_NIT( ).
  IT_05_DUP[] = LC_ZIB_CTE->GET_IT_DUP( ).
  IT_05_VT[]  = LC_ZIB_CTE->GET_IT_VT( ).

  READ TABLE IT_05_VT INTO ZDE_CTE_DIST_VT_ALV WITH KEY TKNUM = P_VT_ALV-TKNUM.
  LC_INDEX_ATUAL = SY-TABIX.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0306_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0306_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_ITENS_DA_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_ITENS_DA_NOTA USING P_DOCNUM TYPE J_1BDOCNUM.

  CLEAR: IT_05_NIT_AJUSTE[], IT_05_NIT_AJUSTE.

  LOOP AT IT_05_NIT WHERE DOCNUM EQ P_DOCNUM.
    APPEND IT_05_NIT TO IT_05_NIT_AJUSTE.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0306 INPUT.

  CASE OK_CODE.
    WHEN OK_CONFIRMAR.
      LOOP AT IT_05_NIT_AJUSTE.
        CALL METHOD LC_ZIB_CTE->SET_PESO_CHEGADA_ITEM
          EXPORTING
            I_NIT = IT_05_NIT_AJUSTE.
      ENDLOOP.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0306  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0306 OUTPUT.

  DATA: FS_SORT_0306 TYPE LVC_S_SORT,
        GT_SORT_0306 TYPE LVC_T_SORT.

  SET PF-STATUS 'PF0306'.
  SET TITLEBAR 'TL0306' WITH ZIB_CTE_DIST_NT_ALV-TKNUM ZIB_CTE_DIST_NT_ALV-DOCNUM.

  IF CTL_CON_0306 IS INITIAL.

    CREATE OBJECT CTL_CON_0306
      EXPORTING
        CONTAINER_NAME = 'ALV_ITENS_NOTA'.

    CREATE OBJECT CTL_ALV_0306
      EXPORTING
        I_PARENT = CTL_CON_0306.

    PERFORM FILL_IT_FIELDCATALOG_0306.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0306.
*   Set layout parameters for ALV grid

    GS_LAY_0306-ZEBRA      = 'X'.
    GS_LAY_0306-EDIT_MODE  = 'X'.

    APPEND '&LOCAL&APPEND'        TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&COPY'          TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&COPY_ROW'      TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&DELETE_ROW'    TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&INSERT_ROW'    TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&CUT'           TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&MOVE_ROW'      TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&PASTE'         TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO IT_EXCLUDE_0306.
    APPEND '&VARI_ADMIN'          TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&COPY'          TO IT_EXCLUDE_0306.
    APPEND '&LOCAL&COPY_ROW'      TO IT_EXCLUDE_0306.
    APPEND '&VLOTUS'              TO IT_EXCLUDE_0306.
    APPEND '&AQW'                 TO IT_EXCLUDE_0306.
    APPEND '&PRINT'               TO IT_EXCLUDE_0306.
    APPEND '&MB_SUM'              TO IT_EXCLUDE_0306.
    APPEND '&AVERAGE'             TO IT_EXCLUDE_0306.
    APPEND '&MB_VIEW'             TO IT_EXCLUDE_0306.
    APPEND '&MB_EXPORT'           TO IT_EXCLUDE_0306.
    APPEND '&MB_FILTER'           TO IT_EXCLUDE_0306.
    APPEND '&GRAPH'               TO IT_EXCLUDE_0306.
    APPEND '&INFO'                TO IT_EXCLUDE_0306.
    APPEND '&CHECK'               TO IT_EXCLUDE_0306.

    CALL METHOD CTL_ALV_0306->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0306
        IS_VARIANT           = GS_VAR_0306
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0306        "I_SAVE = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0306 "IT_EXCEPT_QINFO = IT_HINTS
        IT_OUTTAB            = IT_05_NIT_AJUSTE[]
        IT_SORT              = GT_SORT_0306[].

    CALL METHOD CTL_ALV_0306->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD CTL_ALV_0306->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CREATE OBJECT EVENT_HANDLER_0306.
    SET HANDLER EVENT_HANDLER_0306->DATA_CHANGED_FINISHED FOR CTL_ALV_0306.
*    SET HANDLER EVENT_HANDLER_0306->DATA_CHANGED          FOR CTL_ALV_0306.
*    SET HANDLER EVENT_HANDLER_0306->SUBTOTAL_TEXT         FOR CTL_ALV_0306.
*    SET HANDLER EVENT_HANDLER_0306->ON_F4                 FOR CTL_ALV_0306.
    CALL METHOD CTL_ALV_0306->REFRESH_TABLE_DISPLAY.

    "DATA: GS_F4 TYPE LVC_S_F4,
    "      GT_F4 TYPE LVC_T_F4.
    "GS_F4-FIELDNAME  = 'TP_LCTO'.
    "GS_F4-REGISTER   = 'X'.
    "APPEND GS_F4 TO GT_F4.
    "CALL METHOD CTL_ALV_0306->REGISTER_F4_FOR_FIELDS
    "  EXPORTING
    "    IT_F4 = GT_F4.

  ELSE.
    WA_STABLE_0306-ROW = ABAP_TRUE.
    WA_STABLE_0306-COL = ABAP_TRUE.
    CALL METHOD CTL_ALV_0306->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE_0306.
  ENDIF.

  CALL METHOD CTL_ALV_0306->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0306
      ES_ROW_NO   = GS_SCROLL_ROW_0306.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0306 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0306> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_NIT'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0306.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0306 ASSIGNING <FS_CAT_0306>.
    <FS_CAT_0306>-COL_POS = LC_COL_POS.
    <FS_CAT_0306>-TABNAME   = 'IT_05_NIT_AJUSTE'.
    <FS_CAT_0306>-FIELDNAME = <FS_CAT_0306>-FIELDNAME.
    ADD 1 TO LC_COL_POS.

    CASE <FS_CAT_0306>-DATATYPE.
      WHEN 'QUAN' OR 'DEC' OR 'CURR'.
        <FS_CAT_0306>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0306>-OUTPUTLEN = 15.
    ENDCASE.

    CASE <FS_CAT_0306>-FIELDNAME.
      WHEN 'ZVLR_VI' OR 'ZVLR_FRETE' OR 'PESO_ORIGEM' OR 'PESO_CHEGADA'.
        <FS_CAT_0306>-EDIT = ABAP_TRUE.
      WHEN 'CD_CHAVE_CTE' OR 'DOCNUM' OR 'CK_PESO_DIGITADO' OR 'CK_AUTORIZADO' OR 'PESO_ORIGEM_APRO' OR 'PESO_CHEGADA_APR' OR 'PESO_DIFERE_APRO' OR 'ZVLR_FRETE_APRO'.
        <FS_CAT_0306>-NO_OUT = ABAP_TRUE.
      WHEN OTHERS.
        <FS_CAT_0306>-EDIT  = ABAP_FALSE.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0306

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0306 .

  GS_VAR_0306-REPORT      = SY-REPID.
  GS_VAR_0306-HANDLE      = '0304'.
  GS_VAR_0306-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0306-USERNAME    = ABAP_FALSE.
  GS_VAR_0306-VARIANT     = ABAP_FALSE.
  GS_VAR_0306-TEXT        = ABAP_FALSE.
  GS_VAR_0306-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0306

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_ZBVTYP_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICAR_ZBVTYP_0304 INPUT.
  LC_ZIB_CTE->SET_ZBVTYP( EXPORTING I_ZBVTYP = ZIB_CTE_DIST_TER-ZBVTYP ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_MWSKZ_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICAR_MWSKZ_0304 INPUT.
  LC_ZIB_CTE->SET_MWSKZ( EXPORTING I_MWSKZ = ZIB_CTE_DIST_TER-MWSKZ ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_DT_CHEGADA_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICAR_DT_CHEGADA_0304 INPUT.
  LC_ZIB_CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = ZIB_CTE_DIST_TER-DT_CHEGADA ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  POPULA_NOTAS_TKNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VT_ALV_TKNUM  text
*----------------------------------------------------------------------*
FORM POPULA_NOTAS_TKNUM  USING  P_TKNUM TYPE TKNUM.

  DATA: WA_DIST_N55 TYPE ZIB_CTE_DIST_N55,
        WA_DIST_N01 TYPE ZIB_CTE_DIST_N01.

  CLEAR: IT_05_NF_ALV[].

  IT_05_N55[] = LC_ZIB_CTE->GET_IT_N55( ).
  IT_05_N01[] = LC_ZIB_CTE->GET_IT_N01( ).

  LOOP AT IT_05_N55 INTO WA_DIST_N55 WHERE TKNUM = P_TKNUM.
    CLEAR: ZIB_CTE_DIST_NT_ALV.
    MOVE-CORRESPONDING WA_DIST_N55 TO ZIB_CTE_DIST_NT_ALV.
    ZIB_CTE_DIST_NT_ALV-DOCNUM    = WA_DIST_N55-DOCNUM_NFE.
    ZIB_CTE_DIST_NT_ALV-IC_EDITAR = ICON_CHANGE_TEXT.
    APPEND ZIB_CTE_DIST_NT_ALV TO IT_05_NF_ALV.
  ENDLOOP.

  LOOP AT IT_05_N01 INTO WA_DIST_N01 WHERE TKNUM = P_TKNUM.
    CLEAR: ZIB_CTE_DIST_NT_ALV.
    MOVE-CORRESPONDING WA_DIST_N01 TO ZIB_CTE_DIST_NT_ALV.
    ZIB_CTE_DIST_NT_ALV-DOCNUM    = WA_DIST_N01-DOCNUM_NF.
    ZIB_CTE_DIST_NT_ALV-IC_EDITAR = ICON_CHANGE_TEXT.
    APPEND ZIB_CTE_DIST_NT_ALV TO IT_05_NF_ALV.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0305_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0305_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0305  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0305 OUTPUT.
  SET PF-STATUS 'PF0305'.
  SET TITLEBAR 'TL0306' WITH ZIB_CTE_DIST_NT_ALV-TKNUM ZIB_CTE_DIST_NT_ALV-DOCNUM.

  IF CTL_CON_0305 IS INITIAL.

    CREATE OBJECT CTL_CON_0305
      EXPORTING
        CONTAINER_NAME = 'ALV_NOTAS'.

    CREATE OBJECT CTL_ALV_0305
      EXPORTING
        I_PARENT = CTL_CON_0305.

    PERFORM FILL_IT_FIELDCATALOG_0305.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0305.
*   Set layout parameters for ALV grid

    GS_LAY_0305-SEL_MODE   = SPACE.
    GS_LAY_0305-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0305->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0305
        IS_VARIANT           = GS_VAR_0305
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0305
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0305
        IT_OUTTAB            = IT_05_NF_ALV[].

    CALL METHOD CTL_ALV_0305->REFRESH_TABLE_DISPLAY.

    CREATE OBJECT EVENT_HANDLER_0305.
    SET HANDLER EVENT_HANDLER_0305->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_0305.

  ELSE.
    CALL METHOD CTL_ALV_0305->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0305->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0305
      ES_ROW_NO   = GS_SCROLL_ROW_0305.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0305
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_0305
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_05_NF_ALV INTO ZIB_CTE_DIST_NT_ALV INDEX ROW_ID.

  CASE FIELDNAME.
    WHEN 'IC_EDITAR'.
      PERFORM SELECIONA_ITENS_DA_NOTA USING ZIB_CTE_DIST_NT_ALV-DOCNUM.
      CALL SCREEN 0306 STARTING AT 15 05.
      LEAVE TO SCREEN 0305.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0303
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0305 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0305> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_NT_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0305.

  LOOP AT IT_CATALOG_0305 ASSIGNING <FS_CAT_0305>.
    CASE <FS_CAT_0305>-FIELDNAME.
      WHEN 'IC_EDITAR'.
        <FS_CAT_0305>-COL_POS = 1.
    ENDCASE.
  ENDLOOP.

  LC_COL_POS = 2.

  LOOP AT IT_CATALOG_0305 ASSIGNING <FS_CAT_0305>.
    IF <FS_CAT_0305>-COL_POS IS INITIAL.
      <FS_CAT_0305>-COL_POS = LC_COL_POS.
      ADD 1 TO LC_COL_POS.
    ENDIF.
    <FS_CAT_0305>-TABNAME = 'IT_05_NF_ALV'.

    CASE <FS_CAT_0305>-DATATYPE.
      WHEN 'QUAN' OR 'DEC' OR 'CURR'.
        <FS_CAT_0305>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0305>-OUTPUTLEN = 15.
    ENDCASE.

    CASE <FS_CAT_0305>-FIELDNAME.
      WHEN 'IC_EDITAR'.
        <FS_CAT_0305>-KEY     = ABAP_TRUE.
        <FS_CAT_0305>-HOTSPOT = ABAP_TRUE.
        <FS_CAT_0305>-JUST    = 'C'.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0303
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0305 .

  GS_VAR_0305-REPORT      = SY-REPID.
  GS_VAR_0305-HANDLE      = '0305'.
  GS_VAR_0305-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0305-USERNAME    = ABAP_FALSE.
  GS_VAR_0305-VARIANT     = ABAP_FALSE.
  GS_VAR_0305-TEXT        = ABAP_FALSE.
  GS_VAR_0305-DEPENDVARS  = ABAP_FALSE.

ENDFORM.
