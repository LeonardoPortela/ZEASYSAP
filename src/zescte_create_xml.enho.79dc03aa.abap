"Name: \PR:SAPLJ_1B_CTE\FO:CALL_CTE_CREATE\SE:BEGIN\EI
ENHANCEMENT 0 ZESZCTE_CREATE_XML.


* CASE GS_NFDOC-MODEL.
*   WHEN ZESZCL_DOC_ELETRONICO=>ZESZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE.
*    select SINGLE *
*      FROM setleaf INTO @data(wl_setleaf_model_xi)
*     WHERE setname EQ 'GRC_NFE_CALL_XI_BRANCH'
*       AND valfrom EQ @GS_NFDOC-BRANCH.
*
*    IF SY-SUBRC IS INITIAL.
*       SELECT SINGLE * INTO @DATA(WA_URL)
*         FROM ZESZIB_NFE
*        WHERE DOCNUM EQ @GS_NFDOC-DOCNUM
*          AND DS_URL_DANFE NE @SPACE.
*
*       IF ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*         SY-SUBRC = 8.
*       else.
*         SY-SUBRC = 0.
*       ENDIF.
*    ENDIF.
*
*   WHEN ZESZCL_DOC_ELETRONICO=>ZESZIF_DOC_ELETRONICO~AT_ST_MODEL_CTE.
*    select SINGLE *
*      FROM setleaf INTO @wl_setleaf_model_xi
*     WHERE setname EQ 'GRC_CTE_CALL_XI_BRANCH'
*       AND valfrom EQ @GS_NFDOC-BRANCH.
*
*    IF SY-SUBRC IS INITIAL.
*       SELECT SINGLE * INTO @WA_URL
*         FROM ZESZIB_NFE
*        WHERE DOCNUM EQ @GS_NFDOC-DOCNUM
*          AND DS_URL_DANFE NE @SPACE.
*
*       IF ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*         SY-SUBRC = 8.
*       else.
*         SY-SUBRC = 0.
*       ENDIF.
*    ENDIF.
*
*   WHEN ZESZCL_DOC_ELETRONICO=>ZESZIF_DOC_ELETRONICO~AT_ST_MODEL_MDFE.
*    select SINGLE *
*      FROM setleaf INTO @wl_setleaf_model_xi
*     WHERE setname EQ 'GRC_MDFE_CALL_XI_BRANCH'
*       AND valfrom EQ @GS_NFDOC-BRANCH.
*
*    IF SY-SUBRC IS INITIAL.
*       SELECT SINGLE * INTO @WA_URL
*         FROM ZESZIB_NFE
*        WHERE DOCNUM EQ @GS_NFDOC-DOCNUM
*          AND DS_URL_DANFE NE @SPACE.
*
*       IF ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*         SY-SUBRC = 8.
*       else.
*         SY-SUBRC = 0.
*       ENDIF.
*    ENDIF.
*
* ENDCASE.

*IF sy-subrc is not INITIAL.
*
*  CALL FUNCTION 'Z_MONTA_XML'
*   EXPORTING
*     "XML_IN                      = xmlh
*     "XML_HEAD_TAB                = xmlh_tab
*     "XML_ITEM_TAB                = xmli_tab
*     "XML_BATCH                   = xmlb_tab
*     "XML_REF                     = xmlr_tab
*     "XML_DUP                     = xmld_tab
*     "XML_VOL                     = xmlv_tab
*     "XML_IMP                     = xml_import_tab
*     "XML_EXT1                    = xml_ext1_tab
*     "XML_EXT2                    = xml_ext2_tab
*     GS_NFDOC                   = GS_NFDOC
*   EXCEPTIONS
*     COMMUNICATION_FAILURE       = 1
*     SYSTEM_FAILURE              = 2
*     OTHERS                      = 3.
*
*  IF sy-subrc <> 0.
*    p_rfcerror = sy-subrc.
*    exit.
*  ENDIF.
*
*  exit.
*
*ENDIF.


ENDENHANCEMENT.
