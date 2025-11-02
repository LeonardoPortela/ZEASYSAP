*----------------------------------------------------------------------*
***INCLUDE LZGRCF04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_DIST_TVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_atribuir_nfe_dist_tvo .

  DATA: wa_zib_nfe_dist_tvo TYPE zib_nfe_dist_tvo.

  CHECK gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe IS NOT INITIAL.

  TRY .
      DELETE FROM zib_nfe_dist_tvo WHERE chave_nfe EQ gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
      LOOP AT gs_xml_sefaz-nfeproc-nfe-infnfe-transp-vol INTO DATA(wa_vol).
        CLEAR: wa_zib_nfe_dist_tvo.
        wa_zib_nfe_dist_tvo-chave_nfe  = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
        wa_zib_nfe_dist_tvo-ds_especie = zcl_string=>upper( wa_vol-esp ).
        wa_zib_nfe_dist_tvo-nm_qvol    = wa_vol-qvol.
        wa_zib_nfe_dist_tvo-ds_marca   = wa_vol-marca.
        wa_zib_nfe_dist_tvo-ds_nvol    = wa_vol-nvol.
        wa_zib_nfe_dist_tvo-nm_pesol   = wa_vol-pesol.
        wa_zib_nfe_dist_tvo-nm_pesob   = wa_vol-pesob.
        MODIFY zib_nfe_dist_tvo FROM wa_zib_nfe_dist_tvo.
      ENDLOOP.
    CATCH cx_root.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_DIST_REF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_atribuir_nfe_dist_ref .

  DATA: wa_zib_nfe_dist_ref TYPE zib_nfe_dist_ref.

  CHECK gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe IS NOT INITIAL.

  TRY .
      DELETE FROM zib_nfe_dist_ref WHERE chave_nfe EQ gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
      LOOP AT gs_xml_sefaz-nfeproc-nfe-infnfe-ide-nfref INTO DATA(wa_nfe_ref).

        IF wa_nfe_ref-refnfe IS NOT INITIAL.

          CLEAR: wa_zib_nfe_dist_ref.
          wa_zib_nfe_dist_ref-chave_nfe     = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
          wa_zib_nfe_dist_ref-chave_nfe_ref = wa_nfe_ref-refnfe.
          MODIFY zib_nfe_dist_ref FROM wa_zib_nfe_dist_ref.

        ELSEIF wa_nfe_ref-refnf IS NOT INITIAL.

        ELSEIF wa_nfe_ref-refnfp IS NOT INITIAL.

        ELSEIF wa_nfe_ref-refcte IS NOT INITIAL.

        ENDIF.

      ENDLOOP.
    CATCH cx_root.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_bloq_nfe_cancel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_bloq_nfe_cancel.

  CALL FUNCTION 'ZSDF_EXPORT_1X1_MANUT_NFE' IN UPDATE TASK
    EXPORTING
      i_nfe  = gs_xml_sefaz
      i_bloq = 'A'.

ENDFORM.
