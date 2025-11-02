FUNCTION z_j_1b_mdfe_close.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(I_MODE) TYPE  CHAR01 DEFAULT '1'
*"  EXPORTING
*"     VALUE(ZET_BAPIRET2) TYPE  ZBAPIRETTAB
*"     VALUE(ZEV_ERROR_STATUS) TYPE  ZESZXNFE_ERRSTATUS
*"     VALUE(LV_RFCDEST_MDFE) TYPE  RFCDEST
*"  EXCEPTIONS
*"      RFC_ERROR
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------

  DATA: lv_xmdfeactive TYPE j_1bxnfeactive,
        ziv_resend     TYPE ZESZXNFE_RESEND.

  DATA: ls_event        TYPE j_1bnfe_event,
        ls_orig_event   TYPE j_1bnfe_event,
        lv_first_event  TYPE flag,
        lv_new_sequence TYPE flag,
        lv_updmode      TYPE char1.

  DATA: ziv_access_key   TYPE ZESZXNFE_ID,
        "ZIV_INTERNAL_SEQUENCE_NUMBER TYPE  ZESZXNFE_EV_NSEQEVENTO,
        ziv_timestamp    TYPE timestampl,
        ziv_timezone     TYPE ad_tzone,
        lv_time          TYPE sy-uzeit,
        ziv_cuf          TYPE ZESZXNFE_CUF,
        ziv_cmun         TYPE ZESZXNFE_CMUN,
        ziv_dtenc        TYPE ZESZXNFE_DTENC,
        gc_mdfe_encerrar TYPE j_1bnfe_int_event VALUE 'EV_ENC',
        lc_max_seqnum    TYPE j_1bnfe_event_seqno_ext VALUE 20,
        gc_err_g         TYPE j_1bnfe_ms_error VALUE 'G',
        gc_err_v         TYPE j_1bnfe_ms_error VALUE 'V',
        gc_err_r         TYPE j_1bnfe_ms_error VALUE 'R',
        es_cust3         TYPE j_1bnfe_cust3.


* ENQUEUE J_1BNFE_EVENT
  CALL FUNCTION 'ENQUEUE_E_J_1BNFE_EVENT'
    EXPORTING
      mode_j_1bnfe_event = 'E'
      mandt              = sy-mandt
      docnum             = i_docnum
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      OTHERS             = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING rfc_error.
  ENDIF.

  SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
    FROM j_1bnfe_active
   WHERE docnum EQ @i_docnum.

  DATA(_lva_send_to_cloud) = abap_false.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc)
   WHERE name = 'BRANCH_DRC'
      AND low = @wa_j_1bnfdoc-branch.
  IF sy-subrc EQ 0 .
    _lva_send_to_cloud = abap_true.
  ENDIF.

  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      i_bukrs      = wa_j_1bnfdoc-bukrs
      i_branch     = wa_j_1bnfdoc-branch
      i_model      = wa_j_1bnfdoc-model
    IMPORTING
      e_rfcdest    = lv_rfcdest_mdfe
      e_xnfeactive = lv_xmdfeactive
    EXCEPTIONS
      rfc_error    = 1
      OTHERS       = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING rfc_error.
  ENDIF.

  CALL FUNCTION 'J_1BNFE_EVENT_PREPARE'
    EXPORTING
      iv_docnum       = wa_j_1bnfdoc-docnum
      iv_event        = gc_mdfe_encerrar
    IMPORTING
      es_event        = ls_event
      es_orig_event   = ls_orig_event
      ev_first_event  = lv_first_event
      ev_new_sequence = lv_new_sequence
    EXCEPTIONS
      invalid_event   = 1
      OTHERS          = 2.

  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
      EXPORTING
        mode_j_1bnfe_event = 'E'
        mandt              = sy-mandt
        docnum             = i_docnum.
    MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '001' RAISING rfc_error.
  ENDIF.

  CALL FUNCTION 'J_1BNFE_CUST3_READ'
    EXPORTING
      iv_bukrs       = wa_j_1bnfdoc-bukrs
      iv_branch      = wa_j_1bnfdoc-branch
      iv_model       = wa_j_1bnfdoc-model
    IMPORTING
      es_cust3       = es_cust3
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
      EXPORTING
        mode_j_1bnfe_event = 'E'
        mandt              = sy-mandt
        docnum             = i_docnum.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING rfc_error.
  ENDIF.

  CALL FUNCTION 'J_1BNFE_EVENT_GROUP_CHECK'
    EXPORTING
      iv_event     = gc_mdfe_encerrar
      iv_group     = es_cust3-event_group
    EXCEPTIONS
      not_assigned = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
      EXPORTING
        mode_j_1bnfe_event = 'E'
        mandt              = sy-mandt
        docnum             = i_docnum.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING rfc_error.
  ENDIF.

* Check whether max official sequence number 20 is reached
  IF ls_orig_event-ext_seqnum >= lc_max_seqnum.
    CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
      EXPORTING
        mode_j_1bnfe_event = 'E'
        mandt              = sy-mandt
        docnum             = i_docnum.

    MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '002' RAISING rfc_error.
    EXIT.
  ENDIF.

  IF NOT ls_orig_event IS INITIAL. " it is not the first seq no
* Do not allow new CCe for MS status G
    IF ls_orig_event-ms_error = gc_err_g.

      CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
        EXPORTING
          mode_j_1bnfe_event = 'E'
          mandt              = sy-mandt
          docnum             = i_docnum.

      CASE i_mode.
        WHEN 1.
          MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '003' RAISING rfc_error.
        WHEN 2.
          MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '004' RAISING rfc_error.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
* Do not allow new CCe if another one is still processed
    IF ls_orig_event-docsta IS INITIAL AND ls_orig_event-ms_error IS INITIAL.

      CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
        EXPORTING
          mode_j_1bnfe_event = 'E'
          mandt              = sy-mandt
          docnum             = i_docnum.

      CASE i_mode.
        WHEN 1.
          MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '003' RAISING rfc_error.
        WHEN 2.
          MESSAGE ID 'ZJ1B_NFE' TYPE 'E' NUMBER '004' RAISING rfc_error.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDIF.

  ziv_access_key = wa_j_1bnfdoc-regio && wa_j_1bnfdoc-nfyear && wa_j_1bnfdoc-nfmonth && wa_j_1bnfdoc-stcd1 && wa_j_1bnfdoc-model &&
                   wa_j_1bnfdoc-serie && wa_j_1bnfdoc-nfnum9 && wa_j_1bnfdoc-docnum9 && wa_j_1bnfdoc-cdv.

*  CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
*    EXPORTING
*      I_BUKRS      = WA_J_1BNFDOC-BUKRS
*      I_BRANCH     = WA_J_1BNFDOC-BRANCH
*    IMPORTING
*      "E_TZNZONE    = ZIV_TIMEZONE
*      E_TIMESTAMPL = ZIV_TIMESTAMP.

  SELECT SINGLE * INTO @DATA(wa_j_1bbranch)
    FROM j_1bbranch
   WHERE bukrs  EQ @wa_j_1bnfdoc-bukrs
     AND branch EQ @wa_j_1bnfdoc-branch.

  SELECT SINGLE * INTO @DATA(wa_adrc)
    FROM adrc
   WHERE addrnumber EQ @wa_j_1bbranch-adrnr.

  DATA: lc_gov_timestamp TYPE c LENGTH 25.
  DATA: iv_timestamp TYPE c LENGTH 14.

  if _lva_send_to_cloud eq abap_false.

    CALL FUNCTION 'ZXNFE_CONV_UTC_TIMESTMP_TO_GOV'
      DESTINATION lv_rfcdest_mdfe
      EXPORTING
        iv_timestamp          = iv_timestamp
        iv_uf                 = wa_adrc-region
      IMPORTING
        cv_sefaz_date_utc     = lc_gov_timestamp
        ef_timezone           = ziv_timezone
        lv_timestampl         = ziv_timestamp
        ev_error_status       = zev_error_status
        et_bapiret2           = zet_bapiret2
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

    CASE sy-subrc.
      WHEN 1.

        CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
          EXPORTING
            mode_j_1bnfe_event = 'E'
            mandt              = sy-mandt
            docnum             = i_docnum.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING communication_failure.
      WHEN 2.

        CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
          EXPORTING
            mode_j_1bnfe_event = 'E'
            mandt              = sy-mandt
            docnum             = i_docnum.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING system_failure.
      WHEN OTHERS.
        IF sy-subrc IS NOT INITIAL.

          CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
            EXPORTING
              mode_j_1bnfe_event = 'E'
              mandt              = sy-mandt
              docnum             = i_docnum.

          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING system_failure.
        ENDIF.
    ENDCASE.
  ENDIF.


  DATA: iv_event_type TYPE ZESZXNFE_EV_TPEVENTO.
  DATA: iv_internal_sequence_number TYPE ZESZXNFE_EV_NSEQEVENTO.

  iv_event_type = '110112'.
  iv_internal_sequence_number = ls_event-seqnum.
  ziv_dtenc = sy-datlo.

  SELECT SINGLE * INTO @DATA(wa_zsdt0102)
    FROM ZESZSDT0102
   WHERE docnum     EQ @wa_j_1bnfdoc-docnum
     AND autorizado EQ @abap_true.

  ziv_cuf  = wa_zsdt0102-uffim.
  ziv_cmun = wa_zsdt0102-cmunfim.

  zcl_estado=>zif_estado~get_instance(
    )->get_id_bacen(
    EXPORTING
      i_uf        = CONV #( ziv_cuf )
    IMPORTING
       e_id_bacen  = DATA(e_id_bacen)
   ).

  ziv_cuf = e_id_bacen.


  SELECT SINGLE * FROM setleaf INTO @DATA(lwa_mdfe_inc) WHERE setname = 'GRC_CONFIG' AND valfrom = 'MDFE_INC'.

  IF sy-subrc EQ 0 and _lva_send_to_cloud eq abap_false.

    DATA: lv_time_zone      TYPE tznzone,
          iv_dhemi          TYPE c LENGTH 14,
          iv_tpamb          TYPE c LENGTH 01,
          iv_nprot          TYPE c LENGTH 15,
          iv_cnpj_emit      TYPE c LENGTH 14,
          lv_error          TYPE j_1bnfe_message_type,
          ls_acc_key        TYPE j_1b_nfe_access_key,
          lv_acc_key44      TYPE j_1b_nfe_access_key_dtel44,
          iv_nseqevento_cce TYPE ZESZXNFE_EV_NSEQEVENTO,
          lt_bapiret2       TYPE bapirettab,
          ls_bapiret2       TYPE bapiret2.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(lwa_doc_mdfe)
     WHERE docnum EQ @wa_zsdt0102-docnum.

    IF ( sy-subrc EQ 0 ) AND ( wa_zsdt0102-docnum IS NOT INITIAL ).

      iv_dhemi  = lwa_doc_mdfe-cre_timestamp.

      CASE sy-sysid.
        WHEN 'PRD'.
          iv_tpamb = '1'.
        WHEN OTHERS.
          iv_tpamb = '2'.
      ENDCASE.

      iv_cnpj_emit = wa_j_1bnfdoc-stcd1.
      iv_nprot     = wa_j_1bnfdoc-authcod.
      iv_nseqevento_cce = '00'.

      lv_acc_key44 = ziv_access_key.

      "Cria Documento se Não Existe
      CALL FUNCTION 'ZNFE_INCLUIR_MDFE_SIMETRYA'
        DESTINATION lv_rfcdest_mdfe
        EXPORTING
          iv_nfe_access_key     = lv_acc_key44
          iv_docnum             = wa_j_1bnfdoc-docnum
          iv_timestamp          = ls_event-issue_tmpl
          iv_timezone           = lv_time_zone
          iv_dhemi              = iv_dhemi
          iv_tpamb              = iv_tpamb
          iv_cnpj_emit          = iv_cnpj_emit
          iv_nseqevento_cce     = iv_nseqevento_cce
          iv_nprot              = iv_nprot
        IMPORTING
          ev_error_status       = lv_error
          et_bapiret2           = lt_bapiret2
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2.

    ENDIF.

  ENDIF.

*Begin - CS969098 - Event IR08316 - Fmartins - 15/02/2022
  SELECT SINGLE * INTO @DATA(vl_j_1bnfe_event)
    FROM j_1bnfe_event
   WHERE docnum    EQ @wa_j_1bnfdoc-docnum
     AND int_event EQ @gc_mdfe_encerrar
     AND seqnum    EQ @ls_event-seqnum.

  lv_updmode = 'I'.
  IF sy-subrc IS INITIAL.
    lv_updmode = 'U'.
  ELSE.
    ls_event-eve_group = es_cust3-event_group.
  ENDIF.

* Update the event tables
  CALL FUNCTION 'J_1BNFE_EVENT_UPDATE'
    EXPORTING
      is_event_common = ls_event
      iv_updmode      = lv_updmode.                        "2000504

  CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
    EXPORTING
      mode_j_1bnfe_event = 'E'
      mandt              = sy-mandt
      docnum             = i_docnum.
*End - CS969098 - Event IR08316 - Fmartins - 15/02/2022


  IF _lva_send_to_cloud = abap_true.

    DATA(lwa_event_info) = VALUE nfe_cloud_mdfe_map_event_info( ).

    DATA: lva_dhemit_utc TYPE ZESZXNFE_DHEMI_UTC.
    PERFORM f_conv_timestamp_to_utc USING ls_event-issue_tmpl CHANGING lva_dhemit_utc.

    PERFORM f_set_data_sefaz USING lva_dhemit_utc CHANGING lwa_event_info-issuing_date_time.

    lwa_event_info-event_detail-closing-authorization_protocol = wa_j_1bnfdoc-authcod.
    lwa_event_info-event_detail-closing-city_code    = ziv_cmun.
    lwa_event_info-event_detail-closing-closing_date = ziv_dtenc.
    lwa_event_info-event_detail-closing-state_code   = ziv_cuf.

    lwa_event_info-sequence_number = ls_event-seqnum.
    lwa_event_info-type            = '110112'. "Evento Encerramento

    PERFORM f_send_mdfe_event_to_cloud USING wa_j_1bnfdoc-docnum
                                             ls_event
                                             lwa_event_info.

  ELSE.

    CALL FUNCTION '/XNFE/EV_ISSUE_MDFE_CLOSE'
      DESTINATION lv_rfcdest_mdfe
      EXPORTING
        iv_access_key               = ziv_access_key
        iv_docnum                   = wa_j_1bnfdoc-docnum
        iv_event_type               = iv_event_type
        iv_internal_sequence_number = iv_internal_sequence_number
        iv_timestamp                = ziv_timestamp
        iv_timezone                 = ziv_timezone
        iv_cuf                      = ziv_cuf
        iv_cmun                     = ziv_cmun
        iv_dtenc                    = ziv_dtenc
        iv_resend                   = ziv_resend
      IMPORTING
        ev_error_status             = zev_error_status
        et_bapiret2                 = zet_bapiret2
      EXCEPTIONS
        communication_failure       = 1
        system_failure              = 2.



    CASE sy-subrc.
      WHEN 1.
        CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
          EXPORTING
            mode_j_1bnfe_event = 'E'
            mandt              = sy-mandt
            docnum             = i_docnum.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING communication_failure.
      WHEN 2.
        CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
          EXPORTING
            mode_j_1bnfe_event = 'E'
            mandt              = sy-mandt
            docnum             = i_docnum.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING system_failure.
      WHEN OTHERS.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING system_failure.
        ENDIF.
    ENDCASE.

  ENDIF.



*Begin - CS969098 - Event IR08316 - Fmartins - 15/02/2022
*  SELECT SINGLE * INTO @DATA(VL_J_1BNFE_EVENT)
*    FROM J_1BNFE_EVENT
*   WHERE DOCNUM    EQ @WA_J_1BNFDOC-DOCNUM
*     AND INT_EVENT EQ @GC_MDFE_ENCERRAR
*     AND SEQNUM    EQ @LS_EVENT-SEQNUM.
*
*  LV_UPDMODE = 'I'.
*  IF SY-SUBRC IS INITIAL.
*    LV_UPDMODE = 'U'.
*  ELSE.
*    LS_EVENT-EVE_GROUP = ES_CUST3-EVENT_GROUP.
*  ENDIF.
*
** Update the event tables
*  CALL FUNCTION 'J_1BNFE_EVENT_UPDATE'
*    EXPORTING
*      IS_EVENT_COMMON = LS_EVENT
*      IV_UPDMODE      = LV_UPDMODE.                        "2000504
*
*  CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
*    EXPORTING
*      MODE_J_1BNFE_EVENT = 'E'
*      MANDT              = SY-MANDT
*      DOCNUM             = I_DOCNUM.
*End - CS969098 - Event IR08316 - Fmartins - 15/02/2022
ENDFUNCTION.
