FUNCTION-POOL ZESZGRC MESSAGE-ID ZESZGRC.

TYPES: BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.

CONSTANTS:                                                  "1789219
  gc_ms_error_g TYPE j_1bnfe_ms_error VALUE 'G',            "1789219
  gc_ms_error_d TYPE j_1bnfe_ms_error VALUE 'D'.            "1789219
DATA: gt_intev TYPE TABLE OF j_1bnfe_intev.
DATA: gt_eve_map          TYPE TABLE OF j_1bnfe_evemap,
      gt_eve_group        TYPE TABLE OF j_1bnfe_evegroup,
      gv_message_key      TYPE string, "for BAPIRET2 messages for GRC"1711095
      gs_xml_sefaz        TYPE ZESZNFE_XML_SEFAZ_AUTH,
      gs_xml_sefaz_cte    TYPE ZESZCTE_XML_SEFAZ_AUTH,
      gs_xml_sefaz_cte_os TYPE ZESZCTE_OS_XML_SEFAZ_AUTH,
      gs_zib_nfe_forn     TYPE zib_nfe_forn,
      gs_zib_nfe_dist_ter TYPE ZESZIB_NFE_DIST_TER,
      gs_zib_nfe_dist_itm TYPE zib_nfe_dist_itm,
      gt_zib_nfe_dist_itm TYPE TABLE OF zib_nfe_dist_itm,
      gs_zib_cte_dist_ter TYPE ZESZIB_CTE_DIST_TER,

      gt_zib_cte_dist_c57 TYPE TABLE OF ZESZIB_CTE_DIST_C57,
      gs_zib_cte_dist_c57 TYPE ZESZIB_CTE_DIST_C57,
      gt_zib_cte_dist_cvl TYPE TABLE OF ZESZIB_CTE_DIST_CVL,
      gs_zib_cte_dist_cvl TYPE ZESZIB_CTE_DIST_CVL,
      gt_zib_cte_dist_d55 TYPE TABLE OF ZESZIB_CTE_DIST_D55,
      gs_zib_cte_dist_d55 TYPE ZESZIB_CTE_DIST_D55,

      gt_zib_cte_dist_d01 TYPE TABLE OF ZESZIB_CTE_DIST_D01,
      gs_zib_cte_dist_d01 TYPE ZESZIB_CTE_DIST_D01,

      gt_zib_cte_dist_dup TYPE TABLE OF ZESZIB_CTE_DIST_DUP,
      gs_zib_cte_dist_dup TYPE ZESZIB_CTE_DIST_DUP,
      gt_zib_cte_dist_mot TYPE TABLE OF ZESZIB_CTE_DIST_MOT,
      gs_zib_cte_dist_mot TYPE ZESZIB_CTE_DIST_MOT,

      gt_zib_cte_dist_n01 TYPE TABLE OF ZESZIB_CTE_DIST_N01,
      gs_zib_cte_dist_n01 TYPE ZESZIB_CTE_DIST_N01,

      gt_zib_cte_dist_ant TYPE TABLE OF ZESZIB_CTE_DIST_ANT,
      gs_zib_cte_dist_ant TYPE ZESZIB_CTE_DIST_ANT,

      gt_zib_cte_dist_n55 TYPE TABLE OF ZESZIB_CTE_DIST_N55,
      gs_zib_cte_dist_n55 TYPE ZESZIB_CTE_DIST_N55,
      gt_zib_cte_dist_vei TYPE TABLE OF ZESZIB_CTE_DIST_VEI,
      gs_zib_cte_dist_vei TYPE ZESZIB_CTE_DIST_VEI,
      gt_zib_cte_dist_vga TYPE TABLE OF ZESZIB_CTE_DIST_VGA,
      gs_zib_cte_dist_vga TYPE ZESZIB_CTE_DIST_VGA,
      gs_zib_cte_dist_001 TYPE ZESZIB_CTE_DIST_001,
      gt_zib_cte_dist_001 TYPE TABLE OF ZESZIB_CTE_DIST_001,
      gt_zib_cte_dist_cpl TYPE TABLE OF ZESZIB_CTE_DIST_CPL,
      gs_zib_cte_dist_cpl TYPE ZESZIB_CTE_DIST_CPL.

*** US #173081 - MMSILVA - 07.07.2025 - Ini ***
DATA: gs_zib_nfe_dist_avb TYPE zib_nfe_dist_avb,
      gt_zib_nfe_dist_avb TYPE TABLE OF zib_nfe_dist_avb,
      gs_zib_nfe_dist_avi TYPE zib_nfe_dist_avi,
      gt_zib_nfe_dist_avi TYPE TABLE OF zib_nfe_dist_avi,
      gs_xml_averb        TYPE zaverb_xml_sefaz_auth,
      lo_xml              TYPE REF TO cl_xml_document,
      lo_node             TYPE REF TO if_ixml_node,
      lo_elem             TYPE REF TO if_ixml_element,
      lv_versao           TYPE string.
*** US #173081 - MMSILVA - 07.07.2025 - Fim ***

DATA: editor    TYPE REF TO cl_gui_textedit,
      c_editor  TYPE REF TO cl_gui_custom_container,
      txtopen   TYPE c,
      wa_editor TYPE ty_editor,
      w_text    TYPE char72,
      it_editor TYPE TABLE OF ty_editor.

*-- BADI                                                  "2152098
DATA: go_nfe_print   TYPE REF TO if_ex_cl_nfe_print.        "2152098

* Constants
CONSTANTS:                                                  "1711095
*      gc_authorized TYPE j_1bnfedocstatus VALUE '1',
  gc_cce_int                    TYPE  j_1bnfe_int_event VALUE 'EV_CCE',
  gc_cancel_int                 TYPE  j_1bnfe_int_event VALUE 'EV_CANCEL', "1711095
  gc_epec_int                   TYPE  j_1bnfe_int_event VALUE 'EV_EPEC', "2152098
*     Message Types of Event:                                   "1711095
  gc_event_msgtyp_authorized    TYPE j_1bnfe_event_message_type "1711095
       VALUE '1',                                           "1711095
  gc_event_msgtyp_rejected      TYPE j_1bnfe_event_message_type "1711095
         VALUE '2',                                         "1711095
*     Document Status of Event:                                 "1711095
  gc_event_docstatus_initial    TYPE j_1bnfe_event-docsta   "1711095
       VALUE space,                                         "1711095
  gc_event_docstatus_authorized TYPE j_1bnfe_event-docsta   "1711095
    VALUE '1',                                              "1711095
  gc_event_docstatus_rejected   TYPE j_1bnfe_event-docsta   "1711095
      VALUE '2'.                                            "1711095
CONSTANTS: gc_model_cte TYPE j_1bmodel VALUE '57'.          "1890145


DATA  lo_cancel TYPE REF TO cl_j_1bnfe_cf_cancel.
DATA lo_service_locator  TYPE REF TO cl_j_1bnfe_cf_service_loc.
DATA lo_service_communicator TYPE REF TO cl_j_1bnfe_cf_service_comm.
DATA gt_bapiret_service TYPE TABLE OF bapiret2.

" MDF-e Avulsa """""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MDF-e Avulsa """""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MDF-e Avulsa """""""""""""""""""""""""""""""""""""""""""""""""""""""""

DATA: ok_code     TYPE sy-ucomm,
      ck_editando TYPE c.

DATA: gb_tela TYPE c LENGTH 4 VALUE '1001'.

TABLES: zde_zsdt0237, zde_zsdt0238, zde_zsdt0240, zde_zsdt0239, zsdt0241, zde_zsdt0242, zsdt0243.

DATA: gb_zsdt0237 TYPE ZESZSDT0237.
DATA: gt_zsdt0238       TYPE TABLE OF zsdt0238,
      gt_zde_zsdt0238   TYPE zde_zsdt0238_t,
      gt_zsdt0240       TYPE TABLE OF zsdt0240,
      gt_zde_zsdt0240   TYPE zde_zsdt0240_t,
      gt_zsdt0239       TYPE TABLE OF zsdt0239,
      gt_zde_zsdt0239   TYPE zde_zsdt0239_t,
      gt_zsdt0241       TYPE TABLE OF zsdt0241,
      gt_zsdt0242       TYPE TABLE OF zsdt0242,
      gt_zde_zsdt0242   TYPE zde_zsdt0242_t,
      gt_zsdt0243       TYPE TABLE OF zsdt0243,
      wa_local_desc_sel TYPE zde_zsdt0239,
      gt_jdoc           TYPE TABLE OF zsds0040,
      wa_jdoc           TYPE zsds0040.

TYPES: BEGIN OF ty_cte_read,
         cte_57       TYPE c,
         cte_67       TYPE c,
         chave        TYPE zib_nfe_forn-nu_chave,
         dt_emissao   TYPE zib_nfe_forn-dt_emissao,
         ie_emissor   TYPE zib_nfe_forn-nu_ie,
         protocolo    TYPE zib_nfe_forn-nu_protocolo,
         dt_protocolo TYPE zib_nfe_forn-dt_protocolo,
         hr_protocolo TYPE zib_nfe_forn-hr_protocolo,
         versao_xml   TYPE zib_nfe_forn-xmlvers,
       END OF ty_cte_read.

DATA: gwa_cte_read TYPE ty_cte_read.

*-CS2020001253 - 09.07.2021 - JT - inicio
*************************************************************************************
* classes / btree
*************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: tree1              TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar         TYPE REF TO cl_gui_toolbar,
      g_selecao          TYPE c,
      g_edit             TYPE c,
      g_container        TYPE scrfname VALUE 'CONTAINER',
      g_cont_5126        TYPE scrfname VALUE 'CC_5126',
      g_cont_5009        TYPE scrfname VALUE 'CC_5009',
      g_cont_5010        TYPE scrfname VALUE 'CC_5010',
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_cc_5126          TYPE REF TO cl_gui_custom_container,
      g_cc_5009          TYPE REF TO cl_gui_custom_container,
      g_cc_5010          TYPE REF TO cl_gui_custom_container,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      g_grid_5126        TYPE REF TO cl_gui_alv_grid,
      g_grid_5009        TYPE REF TO cl_gui_alv_grid,
      g_grid_5010        TYPE REF TO cl_gui_alv_grid,
      w_tool             TYPE stb_button,
      t_fieldc_5126      TYPE lvc_t_fcat, "Fieldcatalog
      t_fieldcatalog     TYPE lvc_t_fcat, "Fieldcatalog
      t_fieldc_5009      TYPE lvc_t_fcat, "Fieldcatalog
      t_fieldc_5010      TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab           TYPE slis_t_extab,
      w_exctab           TYPE slis_extab,
      w_item_layout      TYPE lvc_s_laci,
      w_layout           TYPE lvc_s_layo,
      ls_fieldcatalog    TYPE lvc_s_fcat,
      ls_fieldcat5009    TYPE lvc_s_fcat,
      ls_fieldcat5010    TYPE lvc_s_fcat,
      ls_exclude         TYPE ui_func,
      pt_exclude         TYPE ui_functions,
      t_fcat_lvc         TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb         TYPE kkblo_t_fieldcat,
      l_row_id           TYPE lvc_s_row,
      l_column_id        TYPE lvc_s_col,
      l_stable           TYPE lvc_s_stbl.

*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.

  ENDMETHOD.             "DISPLAY

  METHOD user_command.

    CALL METHOD g_grid->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*************************************************************************************
* visao da NFE
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'ZESZNFE'
*************************************************************************************
CONSTANTS: BEGIN OF c_znfe,
             tab1  LIKE sy-ucomm VALUE 'ZNFE_FC1',
             tab2  LIKE sy-ucomm VALUE 'ZNFE_FC2',
             tab3  LIKE sy-ucomm VALUE 'ZNFE_FC3',
             tab4  LIKE sy-ucomm VALUE 'ZNFE_FC4',
             tab5  LIKE sy-ucomm VALUE 'ZNFE_FC5',
             tab6  LIKE sy-ucomm VALUE 'ZNFE_FC6',
             tab7  LIKE sy-ucomm VALUE 'ZNFE_FC7',
             tab8  LIKE sy-ucomm VALUE 'ZNFE_FC8',
             tab9  LIKE sy-ucomm VALUE 'ZNFE_FC9',
             tab10 LIKE sy-ucomm VALUE 'ZNFE_FC10',
             tab11 LIKE sy-ucomm VALUE 'ZNFE_FC11',
             tab12 LIKE sy-ucomm VALUE 'ZNFE_FC12',
           END OF c_znfe.

*************************************************************************************
* visao da NFE
*&SPWIZARD: DATA FOR TABSTRIP 'ZESZNFE'
*************************************************************************************
CONTROLS:  ZESZNFE TYPE TABSTRIP.
DATA: BEGIN OF g_znfe,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'SAPLZGRC',
        pressed_tab LIKE sy-ucomm VALUE c_znfe-tab2,
      END OF g_znfe.

*************************************************************************************
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TC_PROD_DET'
*************************************************************************************
CONSTANTS: BEGIN OF c_tc_prod_det,
             tab1  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC1',
             tab2  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC2',
             tab3  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC3',
             tab4  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC4',
             tab5  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC5',
             tab6  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC6',
             tab7  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC7',
             tab8  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC8',
             tab9  LIKE sy-ucomm VALUE 'TC_PROD_DET_FC9',
             tab10 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC10',
             tab11 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC11',
             tab12 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC12',
             tab13 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC13',
             tab14 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC14',
             tab15 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC15',
             tab16 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC16',
             tab17 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC17',
             tab18 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC18',
             tab19 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC19',
             tab20 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC20',
             tab21 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC21',
             tab22 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC22',
             tab23 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC23',
             tab24 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC24',
             tab25 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC25',
             tab26 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC26',
             tab27 LIKE sy-ucomm VALUE 'TC_PROD_DET_FC27',
           END OF c_tc_prod_det.

*************************************************************************************
*&SPWIZARD: DATA FOR TABSTRIP 'TC_PROD_DET'
*************************************************************************************
CONTROLS:  tc_prod_det TYPE TABSTRIP.
DATA: BEGIN OF g_tc_prod_det,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'SAPLZGRC',
        pressed_tab LIKE sy-ucomm VALUE c_tc_prod_det-tab1,
      END OF g_tc_prod_det.

*************************************************************************************
* variaveis visao NFE
*************************************************************************************
TYPES: BEGIN OF ty_prod.
         INCLUDE TYPE zprod.
TYPES:   det_nitem TYPE char4,
         icone     TYPE char6,
         color     TYPE char4.
TYPES: END   OF ty_prod.

TYPES: BEGIN OF ty_dup.
         INCLUDE TYPE ZESZINTG_DUP.
TYPES:   det_dup TYPE char4,
         icone   TYPE char6,
         color   TYPE char4.
TYPES: END   OF ty_dup.

TYPES: BEGIN OF ty_nrefe.
TYPES: refnfe   TYPE string.
TYPES:END   OF ty_nrefe.

TYPES: BEGIN OF ty_nreff.
TYPES:cuf   TYPE string,
      aamm  TYPE string,
      cnpj  TYPE string,
      mod   TYPE string,
      serie TYPE string,
      nnf   TYPE string.
TYPES: END   OF ty_nreff.

TYPES: BEGIN OF ty_tabstrip,
         name TYPE char40.
TYPES: END   OF ty_tabstrip.

FIELD-SYMBOLS: <f_campo> TYPE any.

DATA: l_xstring_out   TYPE xstring,
      l_xml_doc       TYPE string,
      l_chave_nfe     TYPE string,
      l_det_nitem     TYPE i,
      l_field         TYPE char30,
      l_line          TYPE i,
      l_ativa_ficha   TYPE c,
      l_screen_5400   TYPE c,
      t_element_array TYPE zde_element_array_t,
      t_xml_sefaz     TYPE ZESZNFE_XML_SEFAZ_AUTH,
      t_det           TYPE TABLE OF ZESZDET_NITEM,
      w_det           TYPE ZESZDET_NITEM,
      "Inicio CS2022000981 Novos campos ZFIS25 x XML / Anderson Oenning
      w_emit          TYPE ZESZINTG_EMIT,
      w_dest          TYPE ZESZINTG_DEST,
      w_transp        TYPE ZESZINTG_TRANSPORTA,
      w_transp_mod    TYPE ZESZINTG_TRANSP,
      w_infadic       TYPE ZESZINTG_INFADIC,
      t_volume_transp TYPE TABLE OF ZESZINTG_TRANSP_VOL,
      w_volume_transp TYPE ZESZINTG_TRANSP_VOL,
      w_enderemit     TYPE ZESZINTG_EMIT_END,
      w_enderdest     TYPE ZESZENDERDEST,
      "Inicio CS2022000981 Novos campos ZFIS25 x XML / Anderson Oenning

      t_nfref         TYPE TABLE OF znfe_nfref,
      w_nfref         TYPE znfe_nfref,
      t_tabstrip      TYPE TABLE OF ty_tabstrip,
      w_tabstrip      TYPE ty_tabstrip,
      t_0285          TYPE TABLE OF zsdt0285,
      w_0285          TYPE zsdt0285,
*
      w_a_id(60)      TYPE c,
      w_ide_nnf       TYPE string,
      w_a_versao      TYPE string,
*
      t_dup           TYPE TABLE OF ty_dup,
      w_dup           TYPE ty_dup,
      t_prod          TYPE TABLE OF ty_prod,
      w_prod          TYPE ty_prod,
      t_nrefe         TYPE TABLE OF ty_nrefe,
      w_nrefe         TYPE ty_nrefe,
      t_nreff         TYPE TABLE OF ty_nreff,
      w_nreff         TYPE ty_nreff,
      t_imposto       TYPE TABLE OF ZESZINTG_IMPOSTO,
      w_imposto       TYPE ZESZINTG_IMPOSTO,
      w_icms00        TYPE ZESZINTG_ICMS00,
      w_icms10        TYPE ZESZINTG_ICMS10,
      w_icms20        TYPE ZESZINTG_ICMS20,
      w_icms30        TYPE ZESZINTG_ICMS30,
      w_icms40        TYPE ZESZINTG_ICMS40,
      w_icms41        TYPE ZESZINTG_ICMS41,
      w_icms50        TYPE ZESZINTG_ICMS50,
      w_icms51        TYPE ZESZINTG_ICMS51,
      w_icms60        TYPE ZESZINTG_ICMS60,
      w_icms70        TYPE ZESZINTG_ICMS70,
      w_icms90        TYPE ZESZINTG_ICMS90,
      w_icmspart      TYPE ZESZINTG_ICMSPART,
      w_icmsst        TYPE ZESZINTG_ICMSST,
      w_icmssn101     TYPE ZESZINTG_ICMSSN101,
      w_icmssn102     TYPE ZESZINTG_ICMSSN102,
      w_icmssn201     TYPE ZESZINTG_ICMSSN201,
      w_icmssn202     TYPE ZESZINTG_ICMSSN202,
      w_icmssn500     TYPE ZESZINTG_ICMSSN500,
      w_icmssn900     TYPE ZESZINTG_ICMSSN900,
*
      w_pisaliq       TYPE ZESZINTG_PISALIQ,
      w_pisqtde       TYPE ZESZINTG_PISQTDE,
      w_pisnt         TYPE ZESZINTG_PISNT,
      w_pisoutr       TYPE ZESZINTG_PISOUTR,
      w_pisst         TYPE ZESZINTG_PISST,
      w_pis           TYPE ZESZINTG_PISST,
*
      w_cofinsaliq    TYPE ZESZINTG_COFINSALIQ,
      w_cofinsqtde    TYPE ZESZINTG_COFINSQTDE,
      w_cofinsnt      TYPE ZESZINTG_COFINSNT,
      w_cofinsoutr    TYPE ZESZINTG_COFINSOUTR,
      w_cofinsst      TYPE ZESZINTG_COFINSST,
      w_cofins        TYPE ZESZINTG_COFINSST,
*
      w_ipi           TYPE ZESZINTG_IPI,
      w_ipitrib       TYPE ZESZINTG_IPITRIB,
      w_ipint         TYPE ZESZINTG_IPINT,
*
      w_ii_imp        TYPE ZESZINTG_II,
*
      w_icmstot       TYPE ZESZICMSTOT.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zsdgrce_toolbar_event_receiver.
INCLUDE zsdgrce_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event.
*-CS2020001253 - 09.07.2021 - JT - fim

*************************************************************************************
*************************************************************************************
