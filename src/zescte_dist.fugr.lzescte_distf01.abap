*----------------------------------------------------------------------*
***INCLUDE LZCTE_DISTF01.
*----------------------------------------------------------------------*

TABLES: zib_cte_dist_eap.

DATA: container       TYPE REF TO cl_gui_custom_container,
      editor          TYPE REF TO cl_gui_textedit,
      "MANAGER         TYPE REF TO CL_GOS_MANAGER,
      wl_header       TYPE thead,
      longtext_tab    TYPE catsxt_longtext_itab,
      tl_tlines       LIKE tline OCCURS 0 WITH HEADER LINE,
      wa_line         TYPE txline,
      ck_confirmado   TYPE char01,
      btn_aprovacao   TYPE char50,
      ck_read_only    TYPE char01,
      ck_bt_tela      TYPE char04,
      ck_bt_text      TYPE char100,
      ck_tp_aprovacao TYPE zde_tip_aprovacao.

DATA: gv_longtext_tab TYPE catsxt_longtext_itab,
      gv_confirmado   TYPE abap_bool,
      gv_prim         TYPE abap_bool.

DATA: status_icon   TYPE icons-text,
      icon_name(20) TYPE c,
      icon_text(10) TYPE c,
      icon_info(50) TYPE c.


CONSTANTS: ck_aprovado TYPE zde_tp_autorizacao VALUE '01', "Autorizado
           ck_negado   TYPE zde_tp_autorizacao VALUE '02'. "Negado

*&---------------------------------------------------------------------*
*&      Form  AUTORIZACAO_CTE
*&---------------------------------------------------------------------*
*       Procedimento Para Est. de Aprovação de CT-e
*----------------------------------------------------------------------*
*      -->P_P_CD_CHAVE_CTE   Chave de Referência do CT-e
*      -->P_P_TP_APROVACAO   Tipo de Aprovação
*      <--P_P_CD_APROVACAO   Código de Retorna da "Aprovação"
*      <--P_P_TP_AUTORIZADO  Qual Autorização foi Informada Pelo Usuário
*----------------------------------------------------------------------*
FORM autorizacao_cte  USING    p_cd_chave_cte  TYPE zde_chave_doc_e
                               p_tp_aprovacao  TYPE zde_tip_aprovacao
                      CHANGING p_cd_aprovacao  TYPE zde_est_aprovacao
                               p_tp_autorizado TYPE zde_tp_autorizacao.

  CLEAR: zib_cte_dist_eap, ck_confirmado, ck_read_only.

  zib_cte_dist_eap-tp_aprovacao = p_tp_aprovacao.
  zib_cte_dist_eap-cd_chave_cte = p_cd_chave_cte.

  IF p_tp_autorizado IS NOT INITIAL.
    ck_confirmado = abap_true.
    PERFORM atribuir_usuario_data_hora.
    zib_cte_dist_eap-tp_autorizado = p_tp_autorizado.
    PERFORM gravar_autorizacao CHANGING p_cd_aprovacao p_tp_autorizado.
    PERFORM sair_tela_0100.
  ELSE.
* Início - RJF - 07.07.2022 - bloq. mult.
    IMPORT gv_longtext_tab TO gv_longtext_tab
           gv_confirmado   TO gv_confirmado FROM MEMORY ID 'B_MULTI'.
    IF gv_confirmado IS NOT INITIAL AND gv_prim IS NOT INITIAL.
      MOVE gv_confirmado TO ck_confirmado.
    ELSE.
      FREE ck_confirmado.
    ENDIF.
    IF ck_confirmado IS INITIAL.
* Fim - RJF - 007.07.2022 - bloq. mult.
      CALL SCREEN 0100 STARTING AT 5 5.
* Início - RJF - 07.07.2022 - bloq. mult.
    ENDIF.
* Fim - RJF - 07.07.2022 - bloq. mult.

    IF ck_confirmado EQ abap_true.
      PERFORM atribuir_usuario_data_hora.
      PERFORM gravar_autorizacao CHANGING p_cd_aprovacao p_tp_autorizado.
      PERFORM sair_tela_0100.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATRIBUIR_USUARIO
*&---------------------------------------------------------------------*
FORM atribuir_usuario_data_hora .
  zib_cte_dist_eap-ds_name_usuario = sy-uname.
  zib_cte_dist_eap-dt_autorizacao  = sy-datum.
  zib_cte_dist_eap-hr_autorizacao  = sy-uzeit.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GRAVAR_AUTORIZACAO
*&---------------------------------------------------------------------*
*       Grava Registro de Autorização
*----------------------------------------------------------------------*
FORM gravar_autorizacao CHANGING pcd_aprovacao TYPE zde_est_aprovacao ptp_autorizado TYPE zde_tp_autorizacao.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZIB_CTEEAP'
    IMPORTING
      number                  = pcd_aprovacao
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  zib_cte_dist_eap-cd_aprovacao = pcd_aprovacao.
  zib_cte_dist_eap-ck_ultimo    = abap_true.
  MODIFY zib_cte_dist_eap.

  ptp_autorizado = zib_cte_dist_eap-tp_autorizado.

  "01	Chegada de Documentos
  "02	Complemento
  "03	Travar Pagamento

  UPDATE zib_cte_dist_eap
     SET ck_ultimo = abap_false
   WHERE tp_aprovacao EQ zib_cte_dist_eap-tp_aprovacao
     AND cd_chave_cte EQ zib_cte_dist_eap-cd_chave_cte
     AND ck_ultimo    EQ zib_cte_dist_eap-ck_ultimo
     AND cd_aprovacao NE pcd_aprovacao.

  "Grava Texto Digitado na Tela
  IF editor IS NOT INITIAL.

* Início - RJF - 007.07.2022 - bloq. mult.
    IF gv_longtext_tab IS INITIAL.
* Fim - RJF - 007.07.2022 - bloq. mult.

      CALL METHOD editor->get_text_as_r3table
        IMPORTING
          table           = longtext_tab
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          OTHERS          = 3.

* Início - RJF - 007.07.2022 - bloq. mult.
    ENDIF.
    IF longtext_tab IS NOT INITIAL.
      MOVE: longtext_tab  TO gv_longtext_tab,
            ck_confirmado TO gv_confirmado.
      EXPORT: gv_longtext_tab
              gv_confirmado TO MEMORY ID 'B_MULTI'.
      MOVE abap_true TO gv_prim.
      EXPORT: gv_prim TO MEMORY ID 'B_MULT_P'.
    ENDIF.
* Fim - - RJF - 007.07.2022 - bloq. mult.

    CLEAR: tl_tlines[].

    LOOP AT longtext_tab INTO wa_line.
      tl_tlines-tdline = wa_line.
      APPEND tl_tlines.
    ENDLOOP.
* Início - RJF - 007.07.2022 - bloq. mult.
  ELSEIF gv_longtext_tab IS NOT INITIAL.
    MOVE gv_longtext_tab TO longtext_tab.
    FREE: tl_tlines[].
    LOOP AT longtext_tab INTO wa_line.
      tl_tlines-tdline = wa_line.
      APPEND tl_tlines.
    ENDLOOP.
* Fim - RJF - 007.07.2022 - bloq. mult.
  ELSE.
    "Textos Automáticos """""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CLEAR: tl_tlines[].

    PERFORM texto_tela USING ck_bt_text.
    MOVE 'Texto Automático' TO tl_tlines-tdline.
    APPEND tl_tlines.

    MOVE ck_bt_text TO tl_tlines-tdline.
    APPEND tl_tlines.
  ENDIF.

  IF ( tl_tlines[] IS NOT INITIAL ).

    wl_header-tdobject = 'ZAPROVACAO'.
    wl_header-tdid     = 'ZCTE'.
    wl_header-tdspras  = sy-langu.
    CONCATENATE sy-mandt pcd_aprovacao INTO wl_header-tdname.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = wl_header
        insert          = abap_true
        savemode_direct = abap_true
      TABLES
        lines           = tl_tlines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
  ENDIF.

  COMMIT WORK.

  ck_bt_tela = icon_failure.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: it_dd07v TYPE TABLE OF dd07v WITH HEADER LINE,
        it_ucomm TYPE TABLE OF sy-ucomm.
  "WA_OBJ   TYPE BORIDENT,
  "IP_MODE  TYPE SGS_RWMOD.

  CLEAR: it_ucomm[], it_ucomm.

  IF ck_read_only EQ abap_true.
    IF editor IS NOT INITIAL.
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->true.
    ENDIF.
    APPEND: 'CONFIRMAR' TO it_ucomm,
            'CANCELAR'  TO it_ucomm,
            'APROVAR'   TO it_ucomm,
            'RECUSAR'   TO it_ucomm.
  ELSEIF ( ck_bt_tela EQ icon_failure ) OR ( ck_bt_tela IS INITIAL ).
    IF editor IS NOT INITIAL AND ck_read_only EQ abap_false.
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->false.
    ENDIF.
    APPEND: 'CONFIRMAR' TO it_ucomm,
            'CANCELAR'  TO it_ucomm.
    ck_bt_tela = icon_failure.
  ELSE.
    IF editor IS NOT INITIAL AND ck_read_only EQ abap_false.
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->true.
    ENDIF.
    APPEND: 'APROVAR' TO it_ucomm,
            'RECUSAR' TO it_ucomm.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING it_ucomm.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZDM_TIP_APROVACAO'
    TABLES
      values_tab = it_dd07v.

  READ TABLE it_dd07v WITH KEY domvalue_l = zib_cte_dist_eap-tp_aprovacao.

  SET TITLEBAR 'TL0100' WITH it_dd07v-ddtext.

  zib_cte_dist_eap-ck_ultimo = abap_true.

  IF ( editor IS INITIAL ).

    CREATE OBJECT container
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT editor
      EXPORTING
        parent                 = container
        wordwrap_mode          = '2'
        wordwrap_position      = '72'
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    IF ck_read_only EQ abap_true.

      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->true.

*      IP_MODE = 'D'.

      CLEAR: longtext_tab.

      LOOP AT tl_tlines.
        APPEND tl_tlines-tdline TO longtext_tab.
      ENDLOOP.

      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table           = longtext_tab
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          OTHERS          = 3.
    ELSE.
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->false.

*      IP_MODE = 'E'.
    ENDIF.

*    WA_OBJ-OBJTYPE = 'ZMM0079'.
*    CONCATENATE ZIB_CTE_DIST_EAP-CD_CHAVE_CTE ZIB_CTE_DIST_EAP-TP_APROVACAO INTO WA_OBJ-OBJKEY.
*
*    CREATE OBJECT MANAGER
*      EXPORTING
*        IO_CONTAINER     = CONTAINER
*        IS_OBJECT        = WA_OBJ
*        IP_NO_COMMIT     = 'R'
*        IP_MODE          = IP_MODE
*      EXCEPTIONS
*        OBJECT_INVALID   = 1
*        CALLBACK_INVALID = 2
*        OTHERS           = 3.

  ENDIF.

  CLEAR: ck_bt_text.

  CASE ck_bt_tela.
    WHEN icon_allow.
      icon_name = 'ICON_ALLOW'.
      IF ck_read_only NE abap_true.
        MESSAGE s154 WITH zib_cte_dist_eap-cd_chave_cte INTO ck_bt_text.
      ELSE.
        PERFORM texto_tela USING ck_bt_text.
      ENDIF.
      icon_text = 'Aprovar'.
      icon_info = ck_bt_text.
    WHEN icon_reject.
      icon_name = 'ICON_REJECT'.
      IF ck_read_only NE abap_true.
        MESSAGE s155 WITH zib_cte_dist_eap-cd_chave_cte INTO ck_bt_text.
      ELSE.
        PERFORM texto_tela USING ck_bt_text.
      ENDIF.
      icon_text = 'Rejeitar'.
      icon_info = ck_bt_text.
    WHEN icon_failure.
      icon_name = 'ICON_FAILURE'.
      IF ck_read_only NE abap_true.
        MESSAGE s156 INTO ck_bt_text.
      ELSE.
        PERFORM texto_tela USING ck_bt_text.
      ENDIF.
      icon_text = 'Selecionar'.
      icon_info = ck_bt_text.
  ENDCASE.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_name
      text                  = icon_text
    IMPORTING
      result                = btn_aprovacao
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  ck_confirmado = abap_false.

  PERFORM sair_tela_0100.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: qtd_linhas TYPE i."  boolean like value: true=1, false = 0)

  CHECK ck_read_only = abap_false.

  CALL METHOD editor->get_text_as_r3table
    IMPORTING
      table           = longtext_tab
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

  DESCRIBE TABLE longtext_tab LINES qtd_linhas.

  IF qtd_linhas IS INITIAL.
    MESSAGE s153.
    EXIT.
  ENDIF.

  CASE ok_code.
    WHEN ok_aprovar.

      CASE zib_cte_dist_eap-tp_aprovacao.
        WHEN '03'. "Travar Pagamento
          ck_bt_tela = icon_reject.
        WHEN OTHERS.
          "Chegada de Documentos
          "Autorização de Pagamento Complemento
          "Peso/Data de Chegada Manual
          ck_bt_tela = icon_allow.
      ENDCASE.

    WHEN ok_recusar.

      CASE zib_cte_dist_eap-tp_aprovacao.
        WHEN '03'. "Travar Pagamento
          ck_bt_tela = icon_allow.
        WHEN OTHERS.
          "Chegada de Documentos
          "Autorização de Pagamento Complemento
          "Peso/Data de Chegada Manual
          ck_bt_tela = icon_reject.
      ENDCASE.

    WHEN ok_confirmar.
      CASE ck_bt_tela.
        WHEN icon_allow.

          CASE zib_cte_dist_eap-tp_aprovacao.
            WHEN '03'. "Travar Pagamento
              zib_cte_dist_eap-tp_autorizado = ck_negado.
            WHEN OTHERS.
              "Chegada de Documentos
              "Autorização de Pagamento Complemento
              "Peso/Data de Chegada Manual
              zib_cte_dist_eap-tp_autorizado = ck_aprovado.
          ENDCASE.

          CHECK zib_cte_dist_eap-ck_ultimo EQ abap_true.
          ck_confirmado = abap_true.
          LEAVE TO SCREEN 0.

        WHEN icon_reject.

          CASE zib_cte_dist_eap-tp_aprovacao.
            WHEN '03'. "Travar Pagamento
              zib_cte_dist_eap-tp_autorizado = ck_aprovado.
            WHEN OTHERS.
              "Chegada de Documentos
              "Autorização de Pagamento Complemento
              "Peso/Data de Chegada Manual
              zib_cte_dist_eap-tp_autorizado = ck_negado.
          ENDCASE.

          CHECK zib_cte_dist_eap-ck_ultimo EQ abap_true.
          ck_confirmado = abap_true.
          LEAVE TO SCREEN 0.

      ENDCASE.

    WHEN ok_cancelar.

      ck_bt_tela = icon_failure.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SAIR_TELA_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair_tela_0100 .

  IF editor IS NOT INITIAL.
    CALL METHOD editor->free.
    CLEAR: editor.
  ENDIF.

  IF container IS NOT INITIAL.
    CALL METHOD container->free.
    CLEAR: container.
  ENDIF.

*  IF MANAGER IS NOT INITIAL.
*    CALL METHOD MANAGER->UNPUBLISH.
*    CLEAR: MANAGER.
*  ENDIF.

  CLEAR: ck_read_only, ck_bt_tela, zib_cte_dist_eap, tl_tlines[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TEXTO_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CK_BT_TEXT  text
*----------------------------------------------------------------------*
FORM texto_tela  USING p_ck_bt_text.

  DATA: it_dd07v_tp TYPE TABLE OF dd07v WITH HEADER LINE,
        it_dd07v_tu TYPE TABLE OF dd07v WITH HEADER LINE.

  CLEAR p_ck_bt_text.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZDM_TIP_APROVACAO'
    TABLES
      values_tab = it_dd07v_tp.

  READ TABLE it_dd07v_tp WITH KEY domvalue_l = zib_cte_dist_eap-tp_aprovacao.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZDM_TP_AUTORIZACAO'
    TABLES
      values_tab = it_dd07v_tu.

  READ TABLE it_dd07v_tu WITH KEY domvalue_l = zib_cte_dist_eap-tp_autorizado.

  CASE zib_cte_dist_eap-tp_aprovacao.
    WHEN '01'. "Chegada de Documentos
      CASE zib_cte_dist_eap-tp_autorizado.
        WHEN '01'. "Autorizado
          ck_bt_tela = icon_allow.
        WHEN '02'. "Negado
          ck_bt_tela = icon_reject.
      ENDCASE.
    WHEN '02'. "Autorização de Pagamento Complemento
      CASE zib_cte_dist_eap-tp_autorizado.
        WHEN '01'. "Autorizado
          ck_bt_tela = icon_allow.
        WHEN '02'. "Negado
          ck_bt_tela = icon_reject.
      ENDCASE.
    WHEN '03'. "Travar Pagamento
      CASE zib_cte_dist_eap-tp_autorizado.
        WHEN '01'. "Autorizado
          ck_bt_tela = icon_reject.
        WHEN '02'. "Negado
          ck_bt_tela = icon_allow.
      ENDCASE.
  ENDCASE.

  CONCATENATE it_dd07v_tu-ddtext it_dd07v_tp-ddtext INTO p_ck_bt_text SEPARATED BY space.

ENDFORM.
