*----------------------------------------------------------------------*
***INCLUDE LZGRCF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_EVENT
*&--------------------------------------------------------------1711095*
FORM DEQUEUE_EVENT USING IV_DOCNUM TYPE J_1BDOCNUM.
  CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
    EXPORTING
      MODE_J_1BNFE_EVENT = 'E'
      MANDT              = SY-MANDT
      DOCNUM             = IV_DOCNUM.
ENDFORM.                    " DEQUEUE_EVENT

*&---------------------------------------------------------------------*
*& Form SERVICE_CANCEL_SEND_THEN_ERROR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SERVICE_CANCEL_SEND_THEN_ERROR USING  IV_DOCNUM  TYPE J_1BDOCNUM .
  DATA LO_CF_EXCEPTION TYPE REF TO CX_J_1BNFE_CF.
  DATA LV_REQUEST_JSON TYPE STRING.

  IF LO_CANCEL->CHECK_CANCEL_TABLE_IS_NOT_INIT( ) AND LO_SERVICE_LOCATOR->IS_CLOUD_SERVICE( ).
    TRY .
        LV_REQUEST_JSON = LO_CANCEL->GET_JSON( ).
        PERFORM SERVICE_EVENT_OBJ_SEND USING LV_REQUEST_JSON.
        FREE LO_CANCEL.
      CATCH CX_J_1BNFE_CF INTO LO_CF_EXCEPTION.
        PERFORM SERVICE_EVENT_ERROR USING  IV_DOCNUM
                                           LO_CF_EXCEPTION.
    ENDTRY.
    FREE LO_CANCEL.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SERVICE_EVENT_ERROR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SERVICE_EVENT_ERROR  USING  IV_DOCNUM       TYPE J_1BDOCNUM
                                 IO_CF_EXCEPTION TYPE REF TO CX_J_1BNFE_CF.

  DATA LT_BAPIRET2 TYPE TABLE OF BAPIRET2.
  DATA LS_BAPIRET2 TYPE BAPIRET2.

  LS_BAPIRET2 = CL_J_1BNFE_EXTERNAL_CF=>FILL_BAPIRET_W_J_1BNFE_CF_EXCP( IO_CF_EXCEPTION ).
  APPEND LS_BAPIRET2 TO LT_BAPIRET2.

  CALL FUNCTION 'J_1B_NFE_BAPIRET2_MAP_TO_LOG1'
    EXPORTING
      IV_DOCNUM   = IV_DOCNUM
      IT_BAPIRET2 = LT_BAPIRET2.

  GT_BAPIRET_SERVICE = LT_BAPIRET2.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form SERVICE_EVENT_OBJ_SEND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SERVICE_EVENT_OBJ_SEND USING   IV_REQUEST_JSON TYPE STRING
                            RAISING CX_J_1BNFE_CF.

  DATA LV_HTTP_STATUS_RESPONSE TYPE I.

* Set JSON to client.
  LO_SERVICE_COMMUNICATOR->SET_REQUEST_JSON( IV_REQUEST_JSON ).

* Send Request to Cloud Services.
  LO_SERVICE_COMMUNICATOR->SEND_REQUEST_TO_CLOUD( ).

* Get HTTP response.
  LV_HTTP_STATUS_RESPONSE = LO_SERVICE_COMMUNICATOR->GET_HTTP_STATUS_FROM_CLOUD( ).

  IF LV_HTTP_STATUS_RESPONSE <> CL_J_1BNFE_CF_CONSTANT=>C_SERVICE_STATUS_ACCEPTED. "202
*   Raise HTTP response exception.
    LO_SERVICE_COMMUNICATOR->HTTP_ERROR_HANDLER( LV_HTTP_STATUS_RESPONSE ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SERVICE_CANCEL_OBJ_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM service_cancel_obj_create USING  iv_docnum       TYPE j_1bdocnum
                                      iv_bukrs        TYPE bukrs
                                      iv_branch       TYPE j_1bbranc_
                                      iv_model        TYPE j_1bmodel
                                      iv_nftype       TYPE j_1bnftype
                                      iv_service_type TYPE string.

  DATA lo_cf_exception TYPE REF TO cx_j_1bnfe_cf.

  TRY .
      PERFORM service_event_obj_create USING  iv_docnum
                                              iv_bukrs
                                              iv_branch
                                              iv_model
                                              iv_nftype
                                              iv_service_type.

      IF lo_service_locator->is_cloud_service( ) AND lo_cancel IS INITIAL.
        CREATE OBJECT lo_cancel.
      ENDIF.
    CATCH cx_j_1bnfe_cf INTO lo_cf_exception.
      PERFORM service_event_error USING iv_docnum
                                        lo_cf_exception.
      FREE lo_cancel.
  ENDTRY.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form SERVICE_EVENT_OBJ_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM service_event_obj_create USING iv_docnum           TYPE j_1bdocnum
                                    iv_bukrs            TYPE bukrs
                                    iv_branch           TYPE j_1bbranc_
                                    iv_model            TYPE j_1bmodel
                                    iv_nftype           TYPE j_1bnftype
                                    iv_service_type     TYPE string
                            RAISING cx_j_1bnfe_cf.

  lo_service_locator = cl_j_1bnfe_cf_monitor=>create_service_locator_object(
    iv_bukrs        = iv_bukrs
    iv_branch       = iv_branch
    iv_model        = iv_model
    iv_nftype       = iv_nftype
    iv_service_type = iv_service_type
  ).

  IF lo_service_locator->is_cloud_service( ).
*   Create Client and Oa2c for nfe cloud services
    CREATE OBJECT lo_service_communicator
      EXPORTING
        iv_service_url        = lo_service_locator->get_service_url_with_path( )
        iv_oa2c_profile       = lo_service_locator->get_service_oa2c_profile( )
        iv_oa2c_configuration = lo_service_locator->get_service_oa2c_configurator( ).

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form SERVICE_CANCEL_OBJ_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM service_cancel_obj_process USING    iv_access_key   TYPE j_1b_nfe_access_key
                                         is_active       TYPE j_1bnfe_active
                                         is_event        TYPE j_1bnfe_event
                                         iv_reason       TYPE char255
                                         iv_time_zone    TYPE tznzone
                                         iv_acttab_lines TYPE i
                                         iv_nfe_version  TYPE j_1bnfexmlversion
                                CHANGING iv_size         TYPE i
                                         iv_error        TYPE j_1bnfe_ms_status.

  DATA lo_cf_exception TYPE REF TO cx_j_1bnfe_cf.
  DATA lt_cancel       TYPE j_1bnfe_t_cancel.
  DATA lv_accesskey    TYPE j_1b_nfe_access_key_dtel44.
  DATA lv_regio        TYPE regio.
  DATA lv_dh_str       TYPE string.
  DATA lv_timestmp(14) TYPE c.
  DATA lv_dhevent(14)  TYPE c.
  DATA lv_event_type   TYPE num6.
  DATA lv_request_json TYPE string.
  data lv_offseqnum    type J_1BNFE_EVENT_OFFSEQNO.  "*---> 01/06/2023 - Migração S4 - JS

  IF lo_service_locator->is_cloud_service( ).

*   Prepare Event Variables.
    lv_accesskey    = iv_access_key.
    lv_regio        = is_active-regio.
    lv_event_type   = is_event-ext_event.
    iv_size         = iv_size + 1.

    IF is_event-issue_tmpl IS NOT INITIAL.
      CLEAR lv_dh_str.
      lv_dh_str   = is_event-issue_tmpl.
      lv_timestmp = lv_dh_str(14).
      lv_dhevent  = lv_dh_str(14).
    ENDIF.

    TRY.

        IF lo_cancel->check_cancel_table_is_not_init( ).
*         Send cancel events by limit (20)
          IF lo_cancel->check_event_quantity( ).
            lv_request_json = lo_cancel->structure_serialization( lt_cancel ).
            PERFORM service_event_obj_send USING lv_request_json.
            lo_cancel->clear_cancel_structure( ).
            FREE lo_cancel.
            CREATE OBJECT lo_cancel.
*         Send cancel event by cnpj
          ELSEIF lo_cancel->check_event_cnpj_changed( is_active-stcd1 ).
            lo_cancel->set_cnpj( is_active-stcd1 ).
            lv_request_json = lo_cancel->structure_serialization( lt_cancel ).
            PERFORM service_event_obj_send USING lv_request_json.
            lo_cancel->clear_cancel_structure( ).
            FREE lo_cancel.
            CREATE OBJECT lo_cancel.
          ENDIF.
        ENDIF.



        lt_cancel = lo_cancel->structure_map(
           iv_docnum        = is_active-docnum
           iv_accesskey     = lv_accesskey
           iv_offseqnum     = lv_offseqnum "---> 01/06/2023 - Migração S4 - JS
           iv_event_type    = lv_event_type
           iv_seqnum        = is_event-seqnum
           iv_timestamp     = lv_timestmp
           iv_timezone      = iv_time_zone
           iv_reason        = iv_reason
           iv_tpamb         = is_active-tpamb
           iv_dhevent       = lv_dhevent
           iv_uf            = lv_regio
           iv_nprot         = is_active-authcod
           iv_cnpj          = is_active-stcd1
           iv_nfe_version   = iv_nfe_version ).

        IF iv_acttab_lines = iv_size.
          CLEAR iv_size.
          lv_request_json = lo_cancel->structure_serialization( lt_cancel ).
          PERFORM service_event_obj_send USING lv_request_json.
          lo_cancel->clear_cancel_structure( ).
          FREE lo_cancel.
        ENDIF.
      CATCH cx_j_1bnfe_cf INTO lo_cf_exception.
        PERFORM service_event_error USING is_active-docnum lo_cf_exception.
        PERFORM dequeue_event USING is_active-docnum.
        lo_cancel->clear_cancel_structure( ).
        FREE lo_cancel.
        iv_error = abap_true.
    ENDTRY.
  ENDIF.
ENDFORM.
