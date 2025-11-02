FUNCTION z_j_1bmfe_cancel_event_send.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACCESS_KEY) TYPE  J_1B_NFE_ACCESS_KEY
*"     REFERENCE(IS_ACTTAB) TYPE  J_1BNFE_ACTIVE
*"     REFERENCE(IV_EVENT_GROUP) TYPE  J_1BNFE_EVENT_GROUP
*"     REFERENCE(IV_REASON) TYPE  CHAR255
*"     REFERENCE(IV_RESEND) TYPE  FLAG
*"     REFERENCE(IV_AUTHCODE) TYPE  J_1BNFEAUTHCODE
*"     REFERENCE(IV_ACTTAB_LINES) TYPE  I
*"     REFERENCE(IV_INDEX_ACTTAB) TYPE  SYST_TABIX
*"     REFERENCE(IV_NFTYPE) TYPE  J_1BNFTYPE OPTIONAL
*"     REFERENCE(IV_NFE_VERSION) TYPE  J_1BNFEXMLVERSION
*"  EXPORTING
*"     REFERENCE(EV_MSSTAT) TYPE  J_1BNFE_MS_STATUS
*"     REFERENCE(EV_RFCDEST) TYPE  RFCDEST
*"     REFERENCE(ES_BAPIRET2) TYPE  BAPIRET2
*"  EXCEPTIONS
*"      RFC_ERROR
*"      CLOUD_CONNECTION_ERROR
*"----------------------------------------------------------------------

  CONSTANTS: lc_max_seqnum TYPE j_1bnfe_event_seqno_ext VALUE 1,
             lc_mssta_b    TYPE j_1bnfe_ms_status VALUE 'B'.

  DATA: lv_rfcdest     TYPE rfcdest,
        lv_xnfeactive  TYPE j_1bxnfeactive,
        ls_event       TYPE j_1bnfe_event,
        ls_branch_sadr TYPE sadr,
        lv_time_zone   TYPE tznzone,
        lv_error       TYPE j_1bnfe_ms_status,
        lv_updmode     TYPE char1,
        lv_text        TYPE string,                         "1805626
        lv_ev_type_grc TYPE num6,
        lv_callmode    TYPE flag    VALUE  space,           "1774636
        ls_bapiret2    TYPE bapiret2,
        lt_bapiret2    TYPE TABLE OF bapiret2.

  STATICS lv_size TYPE i.

*  DATA lo_service_locator  TYPE REF TO cl_j_1bnfe_cf_service_loc.
*  DATA lo_service_communicator TYPE REF TO cl_j_1bnfe_cf_service_comm.
  DATA lo_cf_exception     TYPE REF TO cx_j_1bnfe_cf.

  DATA lv_accesskey    TYPE j_1b_nfe_access_key_dtel44.
  DATA lv_regio        TYPE regio.
  DATA lv_dh_str       TYPE string.
  DATA lv_timestmp(14) TYPE c.
  DATA lv_dhevent(14)  TYPE c.

  DATA lv_acckey(44).                                       "2739356
  lv_acckey = iv_access_key.                                "2739356


  DATA(_lva_send_to_cloud) = abap_false.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc)
   WHERE name = 'BRANCH_DRC'
      AND low = @is_acttab-branch.
  IF sy-subrc EQ 0 .
    _lva_send_to_cloud = abap_true.
  ENDIF.



  "Comentando abaixo para projeto do DRC
*  PERFORM SERVICE_CANCEL_OBJ_CREATE USING IS_ACTTAB-DOCNUM
*                                          IS_ACTTAB-BUKRS
*                                          IS_ACTTAB-BRANCH
*                                          IS_ACTTAB-MODEL
*                                          IV_NFTYPE
*                                          CL_J_1BNFE_CF_CONSTANT=>C_SERVICE_EVENT_CANCELLATION.

* Enqueue J_1BNFE_EVENT
  CALL FUNCTION 'ENQUEUE_E_J_1BNFE_EVENT'
    EXPORTING
      mode_j_1bnfe_event = 'E'
      mandt              = sy-mandt
      docnum             = is_acttab-docnum
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    PERFORM service_cancel_send_then_error USING is_acttab-docnum.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

* Create cancellation event
  CALL FUNCTION 'J_1BNFE_EVENT_PREPARE'
    EXPORTING
      iv_docnum     = is_acttab-docnum
      iv_event      = gc_cancel_int
    IMPORTING
      es_event      = ls_event
    EXCEPTIONS
      invalid_event = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    PERFORM service_cancel_send_then_error USING  is_acttab-docnum.
    PERFORM dequeue_event USING is_acttab-docnum.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '458'.
  ENDIF.

* Read Business Place address data
  CALL FUNCTION 'J_1B_BRANCH_READ'
    EXPORTING
      branch            = is_acttab-branch
      company           = is_acttab-bukrs
    IMPORTING
      address           = ls_branch_sadr
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    PERFORM service_cancel_send_then_error USING  is_acttab-docnum.
    PERFORM dequeue_event USING is_acttab-docnum.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Determine time zone
  IF NOT ls_branch_sadr-tzone IS INITIAL.
    lv_time_zone = ls_branch_sadr-tzone.
  ELSE.
    lv_time_zone = sy-zonlo.
  ENDIF.

* type conversion from char to string                      "1805626
  lv_text = iv_reason.                                      "1805626

  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      i_bukrs      = is_acttab-bukrs
      i_branch     = is_acttab-branch
      i_model      = is_acttab-model
    IMPORTING
      e_rfcdest    = lv_rfcdest
      e_xnfeactive = lv_xnfeactive
    EXCEPTIONS
      rfc_error    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    PERFORM dequeue_event USING is_acttab-docnum.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '066' WITH lv_rfcdest.
  ENDIF.

  ev_rfcdest = lv_rfcdest.                                  "1890145
  lv_ev_type_grc = ls_event-ext_event.                      "1774636

* Check if GRC is used as messaging system
  IF lv_xnfeactive IS NOT INITIAL.
*   Convert data type to NUMC 6 for GRC interface
*   lv_ev_type_grc = ls_event-ext_event.                     "1774636

**   If the cloud service cancel object is not initial, send to cloud.
    "IF lo_cancel IS NOT INITIAL.
*      IF is_acttab-model <> gc_model_cte.
**       NF-e Cloud Services - Cancel
*        PERFORM service_cancel_obj_process USING   iv_access_key
*                                                   is_acttab
*                                                   ls_event
*                                                   iv_reason
*                                                   lv_time_zone
*                                                   iv_acttab_lines
*                                                   iv_nfe_version
*                                         CHANGING  lv_size
*                                                   lv_error.
*        IF lv_error IS NOT INITIAL.
*          lt_bapiret2 = gt_bapiret_service.
*          READ TABLE lt_bapiret2 INTO es_bapiret2 INDEX 1.
*          ev_msstat = lv_error.
*          return.
*        ENDIF.
*
*      ELSE. "CC-e will be covered later.
*        "TODO
*      ENDIF.
    "ELSE.
*    IF IS_ACTTAB-MODEL = GC_MODEL_CTE. "CT-e                  "1890145
*      CALL FUNCTION '/XNFE/EV_ISSUE_CTE_CANCEL'               "1890145
*        DESTINATION LV_RFCDEST                                "1890145
*        EXPORTING                                             "1890145
*          IV_ACCESS_KEY               = LV_ACCKEY             "2739356 "1890145
*          IV_DOCNUM                   = IS_ACTTAB-DOCNUM      "1890145
*          IV_EVENT_TYPE               = LV_EV_TYPE_GRC        "1890145
*          IV_INTERNAL_SEQUENCE_NUMBER = LS_EVENT-SEQNUM       "1890145
*          IV_TIMESTAMP                = LS_EVENT-ISSUE_TMPL   "1890145
*          IV_TIMEZONE                 = LV_TIME_ZONE          "1890145
*          IV_XJUST                    = IV_REASON             "1890145
*          IV_RESEND                   = IV_RESEND             "1890145
*        IMPORTING                                             "1890145
*          EV_ERROR_STATUS             = LV_ERROR              "1890145
*          ET_BAPIRET2                 = LT_BAPIRET2           "1890145
*        EXCEPTIONS                                            "1890145
*          RFC_ERROR                   = 1                                     "1890145
*          OTHERS                      = 2.                                    "1890145
*    ELSE. "NF-e                                               "1890145
*      CALL FUNCTION '/XNFE/ISSUE_CANCEL_EVENT'
*        DESTINATION LV_RFCDEST
*        EXPORTING
*          IV_NFE_ACCESS_KEY           = LV_ACCKEY               "2739356
*          IV_DOCNUM                   = IS_ACTTAB-DOCNUM
*          IV_EVENT_TYPE               = LV_EV_TYPE_GRC
*          IV_INTERNAL_SEQUENCE_NUMBER = LS_EVENT-SEQNUM
*          IV_TIMESTAMP                = LS_EVENT-ISSUE_TMPL
*          IV_TIMEZONE                 = LV_TIME_ZONE
*          IV_XJUST                    = IV_REASON
*          IV_RESEND                   = IV_RESEND
*        IMPORTING
*          EV_ERROR_STATUS             = LV_ERROR
*          ET_BAPIRET2                 = LT_BAPIRET2
*        EXCEPTIONS
*          RFC_ERROR                   = 1
*          OTHERS                      = 2.
*    ENDIF.                                                  "1890145

    IF _lva_send_to_cloud = abap_true.

      DATA(lwa_event_info) = VALUE nfe_cloud_mdfe_map_event_info( ).

      DATA: lva_dhemit_utc TYPE ZESZXNFE_DHEMI_UTC.
      PERFORM f_conv_timestamp_to_utc USING ls_event-issue_tmpl CHANGING lva_dhemit_utc.

      PERFORM f_set_data_sefaz USING lva_dhemit_utc CHANGING lwa_event_info-issuing_date_time.
      lwa_event_info-event_detail-cancellation-authorization_protocol = is_acttab-authcod.
      lwa_event_info-event_detail-cancellation-reason                 = iv_reason.
      lwa_event_info-sequence_number = ls_event-seqnum.
      lwa_event_info-type            = '110111'. "Cancelamento

      PERFORM f_send_mdfe_event_to_cloud USING is_acttab-docnum
                                               ls_event
                                               lwa_event_info.

    ELSE.


      CALL FUNCTION '/XNFE/EV_ISSUE_MDFE_CANCEL'
        DESTINATION lv_rfcdest
        EXPORTING
          iv_access_key               = lv_acckey
          iv_docnum                   = is_acttab-docnum
          iv_event_type               = lv_ev_type_grc
          iv_internal_sequence_number = ls_event-seqnum
          iv_timestamp                = ls_event-issue_tmpl
          iv_timezone                 = lv_time_zone
          iv_xjust                    = iv_reason
          iv_resend                   = iv_resend
        IMPORTING
          ev_error_status             = lv_error
          et_bapiret2                 = lt_bapiret2
        EXCEPTIONS
          communication_failure       = 1
          system_failure              = 2.

      IF sy-subrc IS NOT INITIAL.
        PERFORM dequeue_event USING is_acttab-docnum.
        RAISE rfc_error.
      ENDIF.

    ENDIF.

  ELSE.
*   TRY.                                                     "1774636
*     Call to Non-GRC messaging system
* gv_callmode can be switched in debbuging to call the RFC synchronous
    IF lv_callmode IS INITIAL.                              "1774636
      CALL FUNCTION 'J_1BNFE_EVENT_OUT'                     "1774636
        IN BACKGROUND TASK "1774636
        DESTINATION lv_rfcdest "1774636
        EXPORTING                                           "1774636
          iv_nfe_access_key           = lv_acckey            "2739356 "1774636
          iv_docnum                   = is_acttab-docnum    "1774636
          iv_event_type               = lv_ev_type_grc      "1774636
          iv_internal_sequence_number = ls_event-seqnum     "1774636
          iv_authcode                 = iv_authcode         "1838557
          iv_timestamp                = ls_event-issue_tmpl "1774636
          iv_timezone                 = lv_time_zone        "1774636
*         iv_text                     = iv_reason    "1774636 1806065
          iv_text                     = lv_text             "1806065
          iv_resend                   = iv_resend.          "1774636
    ELSE.                                                   "1774636
      CALL FUNCTION 'J_1BNFE_EVENT_OUT'
        DESTINATION lv_rfcdest
        EXPORTING
          iv_nfe_access_key           = lv_acckey            "2739356
          iv_docnum                   = is_acttab-docnum
          iv_event_type               = lv_ev_type_grc
          iv_internal_sequence_number = ls_event-seqnum
          iv_authcode                 = iv_authcode          "1838557
          iv_timestamp                = ls_event-issue_tmpl
          iv_timezone                 = lv_time_zone
*         iv_text                     = iv_reason            "1805626
          iv_text                     = lv_text              "1805626
          iv_resend                   = iv_resend
        EXCEPTIONS
          communication_failure       = 1
          system_failure              = 2.
      IF sy-subrc IS NOT INITIAL.
        PERFORM dequeue_event USING is_acttab-docnum.
        RAISE rfc_error.
      ENDIF.
*     CATCH cx_sy_dyn_call_illegal_func .                    "1774636
*   ENDTRY.                                                  "1774636
    ENDIF.                                                  "1774636
  ENDIF.

* Set messaging system status
  IF lv_error IS NOT INITIAL.
    ls_event-ms_error = lv_error.
    ev_msstat = lv_error.
  ELSE.
    ev_msstat = lc_mssta_b. "B:  Cancellation request received by MS
  ENDIF.

  IF iv_resend IS NOT INITIAL.
*   Event exists already in data base
    lv_updmode = 'U'. "update
  ELSE.
*   No event exists in data base
    lv_updmode = 'I'. "insert
    ls_event-eve_group = iv_event_group.
  ENDIF.

* Update events table;
* Update of ACTIVE and commit done in J_1B_NFE_SEND_REQUESTS
  CALL FUNCTION 'J_1BNFE_EVENT_UPDATE' IN UPDATE TASK
    EXPORTING
      is_event_common = ls_event
      iv_updmode      = lv_updmode.

* Move errors from messaging system to NF-e Monitor log
  IF lt_bapiret2 IS NOT INITIAL.
    CALL FUNCTION 'J_1B_NFE_BAPIRET2_MAP_TO_LOG1'
      EXPORTING
        iv_docnum   = is_acttab-docnum
        it_bapiret2 = lt_bapiret2.
  ENDIF.

ENDFUNCTION.
