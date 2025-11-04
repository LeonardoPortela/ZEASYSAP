*----------------------------------------------------------------------*
***INCLUDE LZGRCO01.
*----------------------------------------------------------------------*

************************************************************************
* atribuir campos tabelas
************************************************************************
FORM f_atribuir_campos USING p_chave_nfe.

  FREE: t_det,
        t_prod,
        t_imposto,
        w_icmstot,
        t_nfref,
        t_nrefe,
        t_nreff.

  DATA: vg_texto   TYPE string,
        t_tab_text TYPE TABLE OF string.

*----------------------------------
*-header
*----------------------------------
  WRITE p_chave_nfe TO w_a_id USING EDIT MASK '____ ____ ____ ____ ____ ____ ____ ____ ____ ____ ____'.
  w_ide_nnf  = t_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.
  w_a_versao = t_xml_sefaz-nfeproc-nfe-infnfe-a_versao.

*----------------------------------
*-monta detalhe
*----------------------------------
  t_det[]    = t_xml_sefaz-nfeproc-nfe-infnfe-det[].

*----------------------------------
*-icms total
*----------------------------------
  w_icmstot  = t_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot.

*----------------------------------
*-Dados do emitente
*----------------------------------
  w_emit  = t_xml_sefaz-nfeproc-nfe-infnfe-emit.
  w_enderemit  = t_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit.

*----------------------------------
*-Dados do destinatario
*----------------------------------
  w_dest = t_xml_sefaz-nfeproc-nfe-infnfe-dest.
  w_enderdest  = t_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest.

*----------------------------------
*-Dados do transporte
*----------------------------------
  w_transp_mod    = t_xml_sefaz-nfeproc-nfe-infnfe-transp.
  w_transp        = t_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta.
  t_volume_transp = t_xml_sefaz-nfeproc-nfe-infnfe-transp-vol[].
  READ TABLE t_volume_transp INTO w_volume_transp INDEX 1.

*---------------------------------
*-Dados informações adicionais
*----------------------------------
  w_infadic    = t_xml_sefaz-nfeproc-nfe-infnfe-infadic.

  "==FI - CS2022000981 Novos campos ZFIS25 x XML #93902 / ANDERSON OENNING
  CLEAR: vg_texto . "Retornando as informações gerais.
  IF w_infadic-infcpl IS NOT INITIAL.
    vg_texto = w_infadic-infcpl.
    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = vg_texto
        i_tabline_length = 72
      TABLES
        et_table         = t_tab_text.

    IF t_tab_text IS NOT INITIAL.
      it_editor[] = VALUE #( FOR i IN t_tab_text ( line    = i ) ).
    ENDIF.
  ENDIF.
  "=================================================

*----------------------------------
*-produtos
*----------------------------------
  CLEAR l_det_nitem.

  LOOP AT t_det                    INTO w_det.
    CLEAR: w_prod,
           w_imposto.

    ADD 1                            TO l_det_nitem.
    MOVE l_det_nitem                 TO w_prod-det_nitem.
    MOVE icon_select_detail          TO w_prod-icone.
    MOVE-CORRESPONDING w_det-prod    TO w_prod.
    APPEND w_prod                    TO t_prod.
    MOVE-CORRESPONDING w_det-imposto TO w_imposto.
    APPEND w_imposto                 TO t_imposto.
  ENDLOOP.

*** PBI - 65246 - Inicio - CBRAND
  t_nfref[] = t_xml_sefaz-nfeproc-nfe-infnfe-ide-nfref[].

  LOOP AT t_nfref                    INTO w_nfref.

    CLEAR: w_nrefe,
           w_nrefe.

    w_nrefe-refnfe = w_nfref-refnfe.
    APPEND w_nrefe TO  t_nrefe.

    MOVE-CORRESPONDING w_nfref-refnf   TO w_nreff.
    APPEND  w_nreff TO  t_nreff.

  ENDLOOP.

  "Passar valores duplicata.
  CLEAR: w_dup.
  FREE: t_dup.
  LOOP AT t_xml_sefaz-nfeproc-nfe-infnfe-cobr-dup INTO DATA(w_dupl).
    MOVE-CORRESPONDING w_dupl TO w_dup.
    w_dup-dvenc = |{ w_dup-dvenc+8(02) }.{ w_dup-dvenc+5(02) }.{ w_dup-dvenc(04) }|. "2022-04-05
    APPEND w_dup TO t_dup.
  ENDLOOP.

ENDFORM.

************************************************************************
* atribuir campos tabelas
************************************************************************
FORM f_mover_campos_tela USING e_row TYPE lvc_s_row.

  FREE: w_prod,       w_imposto,   w_icms00,    w_icms10,   w_icms20,
        w_icms30,     w_icms40,    w_icms41,    w_icms50,   w_icms51,
        w_icms60,     w_icms70,    w_icms90,    w_icmspart, w_icmsst,
        w_icmssn101,  w_icmssn102, w_icmssn201, w_icmssn202,
        w_icmssn500,  w_icmssn900,
        w_pisaliq,    w_pisqtde,    w_pisnt,    w_pisoutr,    w_pisst,    w_pis,
        w_cofinsaliq, w_cofinsqtde, w_cofinsnt, w_cofinsoutr, w_cofinsst, w_cofins,
        w_ipi,        w_ipitrib,    w_ipint,
        w_ii_imp,
        t_tabstrip.

  g_tc_prod_det-pressed_tab = 'TC_PROD_DET_FC1'.

  READ TABLE t_prod    INTO w_prod    INDEX e_row-index.
  READ TABLE t_imposto INTO w_imposto INDEX e_row-index.

*-------------------------------------
* ICMS
*-------------------------------------
  w_icms00    = w_imposto-icms-icms00.
  w_icms10    = w_imposto-icms-icms10.
  w_icms20    = w_imposto-icms-icms20.
  w_icms30    = w_imposto-icms-icms30.
  w_icms40    = w_imposto-icms-icms40.
  w_icms41    = w_imposto-icms-icms41.
  w_icms50    = w_imposto-icms-icms50.
  w_icms51    = w_imposto-icms-icms51.
  w_icms60    = w_imposto-icms-icms60.
  w_icms70    = w_imposto-icms-icms70.
  w_icms90    = w_imposto-icms-icms90.
  w_icmspart  = w_imposto-icms-icmspart.
  w_icmsst    = w_imposto-icms-icmsst.
  w_icmssn101 = w_imposto-icms-icmssn101.
  w_icmssn102 = w_imposto-icms-icmssn102.
  w_icmssn201 = w_imposto-icms-icmssn201.
  w_icmssn202 = w_imposto-icms-icmssn202.
  w_icmssn500 = w_imposto-icms-icmssn500.
  w_icmssn900 = w_imposto-icms-icmssn900.

*-------------------------------------
* PIS
*-------------------------------------
  w_pisaliq    = w_imposto-pis-pisaliq.
  w_pisqtde    = w_imposto-pis-pisqtde.
  w_pisnt      = w_imposto-pis-pisnt.
  w_pisoutr    = w_imposto-pis-pisoutr.
  w_pisst      = w_imposto-pis-pisst.

  IF     w_pisaliq-cst <> abap_off.
    MOVE-CORRESPONDING w_pisaliq  TO w_pis.
  ELSEIF w_pisqtde-cst <> abap_off.
    MOVE-CORRESPONDING w_pisqtde  TO w_pis.
  ELSEIF w_pisnt-cst   <> abap_off.
    MOVE-CORRESPONDING w_pisnt    TO w_pis.
  ELSEIF w_pisoutr-cst <> abap_off.
    MOVE-CORRESPONDING w_pisoutr  TO w_pis.
  ELSEIF w_pisst-cst   <> abap_off.
    MOVE-CORRESPONDING w_pisst    TO w_pis.
  ENDIF.

*-------------------------------------
* COFINS
*-------------------------------------
  w_cofinsaliq = w_imposto-cofins-cofinsaliq.
  w_cofinsqtde = w_imposto-cofins-cofinsqtde.
  w_cofinsnt   = w_imposto-cofins-cofinsnt.
  w_cofinsoutr = w_imposto-cofins-cofinsoutr.
  w_cofinsst   = w_imposto-cofins-cofinsst.

  IF     w_cofinsaliq-cst <> abap_off.
    MOVE-CORRESPONDING w_cofinsaliq  TO w_cofins.
  ELSEIF w_cofinsqtde-cst <> abap_off.
    MOVE-CORRESPONDING w_cofinsqtde  TO w_cofins.
  ELSEIF w_cofinsnt-cst   <> abap_off.
    MOVE-CORRESPONDING w_cofinsnt    TO w_cofins.
  ELSEIF w_cofinsoutr-cst <> abap_off.
    MOVE-CORRESPONDING w_cofinsoutr  TO w_cofins.
  ELSEIF w_cofinsst-cst   <> abap_off.
    MOVE-CORRESPONDING w_cofinsst    TO w_cofins.
  ENDIF.

*-------------------------------------
* IPI
*-------------------------------------
  w_ipi        = w_imposto-ipi.
  w_ipitrib    = w_imposto-ipi-ipitrib.
  w_ipint      = w_imposto-ipi-ipint.

*-------------------------------------
* II
*-------------------------------------
  w_ii_imp     = w_imposto-ii.

*-------------------------------------
* descricao dos codigos
*-------------------------------------
  PERFORM f_descricao_cod USING 'PROD'
                                'INDTOT'
                                'W_PROD-INDTOT'.

*-icms00 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS00-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS00-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMS00-MODBC'.

*-icms10 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS10-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS10-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMS10-MODBC'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMS10-MODBCST'.

*-icms20 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS20-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS20-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMS20-MODBC'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS20-MOTDESICMS'.

*-icms30 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS30-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS30-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMS30-MODBCST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS30-MOTDESICMS'.

*-icms40 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS40-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS40-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS40-MOTDESICMS'.

*-icms41 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS41-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS41-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS41-MOTDESICMS'.

*-icms50 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS50-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS50-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS50-MOTDESICMS'.

*-icms51 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS51-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS51-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS51-MODBC'.

*-icms60 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS60-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS60-CST'.

*-icms70 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS70-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS70-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMS70-MODBC'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMS70-MODBCST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS70-MOTDESICMS'.

*-icms90 -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMS90-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMS90-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMS90-MODBC'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMS90-MODBCST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MOTDESICMS'
                                'W_ICMS90-MOTDESICMS'.

*-icms part -----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSPART-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMSPART-CST'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMSPART-MODBC'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMSPART-MODBCST'.

*-icms st -------------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSST-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CST'
                                'W_ICMSST-CST'.

*-icms sn101 ----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSSN101-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CSOSN'
                                'W_ICMSSN101-CSOSN'.

*-icms sn102 ----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSSN102-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CSOSN'
                                'W_ICMSSN102-CSOSN'.

*-icms sn201 ----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSSN201-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CSOSN'
                                'W_ICMSSN201-CSOSN'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMSSN201-MODBCST'.

*-icms sn202 ----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSSN202-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CSOSN'
                                'W_ICMSSN202-CSOSN'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMSSN202-MODBCST'.

*-icms sn500 ----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSSN500-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CSOSN'
                                'W_ICMSSN500-CSOSN'.

*-icms sn900 ----------------------------------------------
  PERFORM f_descricao_cod USING 'ICMS'
                                'ORIG'
                                'W_ICMSSN900-ORIG'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'CSOSN'
                                'W_ICMSSN900-CSOSN'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBC'
                                'W_ICMSSN900-MODBC'.

  PERFORM f_descricao_cod USING 'ICMS'
                                'MODBCST'
                                'W_ICMSSN900-MODBCST'.

*-pis -----------------------------------------------------
  PERFORM f_descricao_cod USING 'PIS'
                                'CST'
                                'W_PIS-CST'.

*-cofins --------------------------------------------------
  PERFORM f_descricao_cod USING 'COFINS'
                                'CST'
                                'W_COFINS-CST'.

*-ipi -----------------------------------------------------
  PERFORM f_descricao_cod USING 'IPI'
                                'CST'
                                'W_IPITRIB-CST'.

  PERFORM f_descricao_cod USING 'IPI'
                                'CST'
                                'W_IPINT-CST'.

*-------------------------------------
* exibir tabstrips
*-------------------------------------
  PERFORM f_controle_tabstrip.

ENDFORM.

************************************************************************
* descricao codigo
************************************************************************
FORM f_descricao_cod USING p_grupo_trib
                           p_campo
                           p_codigo.

  ASSIGN (p_codigo) TO <f_campo>.

  CHECK sy-subrc = 0.

  READ TABLE t_0285 INTO w_0285 WITH KEY grupo_trib = p_grupo_trib
                                         campo      = p_campo
                                         codigo     = <f_campo>.
  IF sy-subrc = 0.
    <f_campo> = |{ <f_campo> } { '-' } { w_0285-descricao }|.
  ENDIF.

ENDFORM.

************************************************************************
* controle tabtrip
************************************************************************
FORM f_controle_tabstrip.

  w_tabstrip-name     = 'TC_PROD_DET_TAB2'.
  APPEND w_tabstrip  TO t_tabstrip.

  IF w_icms00 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB3'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms10 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB4'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms20 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB5'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms30 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB6'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms40 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB7'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms41 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB8'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms50 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB9'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms51 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB10'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms60 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB11'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms70 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB12'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icms90 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB13'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmspart IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB14'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmsst IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB15'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmssn101 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB16'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmssn102 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB17'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmssn201 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB18'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmssn202 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB19'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmssn500 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB20'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_icmssn900 IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB21'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_pis IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB22'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_cofins IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB23'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_ipi     IS INITIAL AND
     w_ipitrib IS INITIAL AND
     w_ipint   IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB24'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

  IF w_ii_imp IS INITIAL.
    w_tabstrip-name     = 'TC_PROD_DET_TAB25'.
    APPEND w_tabstrip  TO t_tabstrip.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5000 OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'ZNFE_TAB1' OR screen-name = 'ZNFE_TAB7' OR screen-name = 'ZNFE_TAB8'.

      "OR "screen-name = 'ZNFE_TAB2' OR screen-name = 'ZNFE_TAB03' OR screen-name = 'ZNFE_TAB06'.
      "OR "screen-name = 'ZNFE_TAB9' OR screen-name = 'ZNFE_TAB10'.
      screen-active = 0.
      screen-input  = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  SET PF-STATUS 'SHOW_NFE'.
* SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5001 OUTPUT.
*  SET PF-STATUS 'SHOW_NFE_5001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5004 OUTPUT.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

*  SET PF-STATUS 'SHOW_NFE_5004'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5100 OUTPUT.

  LOOP AT SCREEN.
    READ TABLE t_tabstrip INTO w_tabstrip WITH KEY name = screen-name.
    IF sy-subrc = 0.
      screen-active = 0.
      screen-input  = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*  SET PF-STATUS 'SHOW_NFE_5001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5000 INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'SAIR'.
*     CALL METHOD g_grid->free.
*     CALL METHOD g_custom_container->free.
*     CALL METHOD cl_gui_cfw=>flush.

      LEAVE TO SCREEN 0.

    WHEN 'VOLTAR'.
      l_ativa_ficha = abap_false.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5001 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5004 INPUT.

  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5100 INPUT.

  CASE ok_code.
    WHEN 'VOLTAR'.
      l_ativa_ficha = abap_false.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog.
  PERFORM toolbar_alv.

  l_stable-row         = abap_true.
  l_stable-col         = abap_true.
  w_layout-zebra       = abap_false.
* w_layout-edit        = abap_true. " Makes all Grid editable
  w_layout-no_totarr   = abap_true.
  w_layout-no_totexp   = abap_true.
  w_layout-no_totline  = abap_true.
  w_layout-no_toolbar  = abap_false.
  w_layout-info_fname  = 'COLOR'.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = t_prod.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
    SET HANDLER : m_event_handler->toolbar FOR g_grid.
    SET HANDLER : m_event_handler->user_command FOR g_grid.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid,
               lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = l_stable.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog.

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_PROD'.
  ls_fieldcatalog-fieldname = 'ICONE'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 02.
  ls_fieldcatalog-dd_outlen = 02.
  ls_fieldcatalog-coltext   = ''.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_PROD'.
  ls_fieldcatalog-fieldname = 'DET_NITEM'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 07.
  ls_fieldcatalog-dd_outlen = 07.
  ls_fieldcatalog-coltext   = 'Num.'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_PROD'.
  ls_fieldcatalog-fieldname = 'XPROD'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 81.
  ls_fieldcatalog-dd_outlen = 81.
  ls_fieldcatalog-coltext   = 'Descrição'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_PROD'.
  ls_fieldcatalog-fieldname = 'QTRIB'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-coltext   = 'Qtd.'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_PROD'.
  ls_fieldcatalog-fieldname = 'UTRIB'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-coltext   = 'Unidade Comercial'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_PROD'.
  ls_fieldcatalog-fieldname = 'VPROD'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-coltext   = 'Valor'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv.

  FREE: pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
*
  IF g_edit = abap_false.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.
  ENDIF.

* IF ( p_escala = abap_false ) OR l_edit = abap_false.
*   APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
*   APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.
* ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&SPWIZARD: OUTPUT MODULE FOR TS 'ZNFE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
*&---------------------------------------------------------------------*
MODULE znfe_active_tab_set OUTPUT.
  znfe-activetab = g_znfe-pressed_tab.
  CASE g_znfe-pressed_tab.
    WHEN c_znfe-tab1.
      g_znfe-subscreen = '5001'.
    WHEN c_znfe-tab2.
      g_znfe-subscreen = '5002'.
    WHEN c_znfe-tab3.
      g_znfe-subscreen = '5003'.
    WHEN c_znfe-tab4.
      IF l_ativa_ficha = abap_true.
        g_znfe-subscreen = '5100'.
      ELSE.
        g_znfe-subscreen = '5004'.
      ENDIF.
    WHEN c_znfe-tab5.
      g_znfe-subscreen = '5005'.
    WHEN c_znfe-tab6.
      g_znfe-subscreen = '5006'.
    WHEN c_znfe-tab7.
      g_znfe-subscreen = '5007'.
    WHEN c_znfe-tab8.
      g_znfe-subscreen = '5008'.
    WHEN c_znfe-tab9.
      g_znfe-subscreen = '5009'.
    WHEN c_znfe-tab10.
      g_znfe-subscreen = '5010'.
    WHEN c_znfe-tab11.
      g_znfe-subscreen = '5126'.
    WHEN c_znfe-tab12.
      g_znfe-subscreen = '5127'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&SPWIZARD: INPUT MODULE FOR TS 'ZNFE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
*&---------------------------------------------------------------------*
MODULE znfe_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_znfe-tab1.
      g_znfe-pressed_tab = c_znfe-tab1.
    WHEN c_znfe-tab2.
      g_znfe-pressed_tab = c_znfe-tab2.
    WHEN c_znfe-tab3.
      g_znfe-pressed_tab = c_znfe-tab3.
    WHEN c_znfe-tab4.
      g_znfe-pressed_tab = c_znfe-tab4.
    WHEN c_znfe-tab5.
      g_znfe-pressed_tab = c_znfe-tab5.
    WHEN c_znfe-tab6.
      g_znfe-pressed_tab = c_znfe-tab6.
    WHEN c_znfe-tab7.
      g_znfe-pressed_tab = c_znfe-tab7.
    WHEN c_znfe-tab8.
      g_znfe-pressed_tab = c_znfe-tab8.
    WHEN c_znfe-tab9.
      g_znfe-pressed_tab = c_znfe-tab9.
    WHEN c_znfe-tab10.
      g_znfe-pressed_tab = c_znfe-tab10.
    WHEN c_znfe-tab11.
      g_znfe-pressed_tab = c_znfe-tab11.
    WHEN c_znfe-tab12.
      g_znfe-pressed_tab = c_znfe-tab12.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TC_PROD_DET'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tc_prod_det_active_tab_set OUTPUT.
  tc_prod_det-activetab = g_tc_prod_det-pressed_tab.
  CASE g_tc_prod_det-pressed_tab.
    WHEN c_tc_prod_det-tab1.
      g_tc_prod_det-subscreen = '5101'.
    WHEN c_tc_prod_det-tab2.
      g_tc_prod_det-subscreen = '5102'.
    WHEN c_tc_prod_det-tab3.
      g_tc_prod_det-subscreen = '5103'.
    WHEN c_tc_prod_det-tab4.
      g_tc_prod_det-subscreen = '5104'.
    WHEN c_tc_prod_det-tab5.
      g_tc_prod_det-subscreen = '5105'.
    WHEN c_tc_prod_det-tab6.
      g_tc_prod_det-subscreen = '5106'.
    WHEN c_tc_prod_det-tab7.
      g_tc_prod_det-subscreen = '5107'.
    WHEN c_tc_prod_det-tab8.
      g_tc_prod_det-subscreen = '5108'.
    WHEN c_tc_prod_det-tab9.
      g_tc_prod_det-subscreen = '5109'.
    WHEN c_tc_prod_det-tab10.
      g_tc_prod_det-subscreen = '5110'.
    WHEN c_tc_prod_det-tab11.
      g_tc_prod_det-subscreen = '5111'.
    WHEN c_tc_prod_det-tab12.
      g_tc_prod_det-subscreen = '5112'.
    WHEN c_tc_prod_det-tab13.
      g_tc_prod_det-subscreen = '5113'.
    WHEN c_tc_prod_det-tab14.
      g_tc_prod_det-subscreen = '5114'.
    WHEN c_tc_prod_det-tab15.
      g_tc_prod_det-subscreen = '5115'.
    WHEN c_tc_prod_det-tab16.
      g_tc_prod_det-subscreen = '5116'.
    WHEN c_tc_prod_det-tab17.
      g_tc_prod_det-subscreen = '5117'.
    WHEN c_tc_prod_det-tab18.
      g_tc_prod_det-subscreen = '5118'.
    WHEN c_tc_prod_det-tab19.
      g_tc_prod_det-subscreen = '5119'.
    WHEN c_tc_prod_det-tab20.
      g_tc_prod_det-subscreen = '5102'.  "'5120'.
    WHEN c_tc_prod_det-tab21.
      g_tc_prod_det-subscreen = '5121'.
    WHEN c_tc_prod_det-tab22.
      g_tc_prod_det-subscreen = '5122'.
    WHEN c_tc_prod_det-tab23.
      g_tc_prod_det-subscreen = '5123'.
    WHEN c_tc_prod_det-tab24.
      g_tc_prod_det-subscreen = '5124'.
    WHEN c_tc_prod_det-tab25.
      g_tc_prod_det-subscreen = '5125'.
    WHEN c_tc_prod_det-tab26.
      g_tc_prod_det-subscreen = '5126'.
    WHEN c_tc_prod_det-tab27.
      g_tc_prod_det-subscreen = '5127'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TC_PROD_DET'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tc_prod_det_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tc_prod_det-tab1.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab1.
    WHEN c_tc_prod_det-tab2.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab2.
    WHEN c_tc_prod_det-tab3.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab3.
    WHEN c_tc_prod_det-tab4.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab4.
    WHEN c_tc_prod_det-tab5.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab5.
    WHEN c_tc_prod_det-tab6.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab6.
    WHEN c_tc_prod_det-tab7.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab7.
    WHEN c_tc_prod_det-tab8.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab8.
    WHEN c_tc_prod_det-tab9.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab9.
    WHEN c_tc_prod_det-tab10.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab10.
    WHEN c_tc_prod_det-tab11.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab11.
    WHEN c_tc_prod_det-tab12.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab12.
    WHEN c_tc_prod_det-tab13.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab13.
    WHEN c_tc_prod_det-tab14.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab14.
    WHEN c_tc_prod_det-tab15.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab15.
    WHEN c_tc_prod_det-tab16.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab16.
    WHEN c_tc_prod_det-tab17.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab17.
    WHEN c_tc_prod_det-tab18.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab18.
    WHEN c_tc_prod_det-tab19.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab19.
    WHEN c_tc_prod_det-tab20.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab20.
    WHEN c_tc_prod_det-tab21.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab21.
    WHEN c_tc_prod_det-tab22.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab22.
    WHEN c_tc_prod_det-tab23.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab23.
    WHEN c_tc_prod_det-tab24.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab24.
    WHEN c_tc_prod_det-tab25.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab25.
    WHEN c_tc_prod_det-tab26.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab26.
    WHEN c_tc_prod_det-tab27.
      g_tc_prod_det-pressed_tab = c_tc_prod_det-tab27.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_5009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5009 OUTPUT.
  PERFORM init_alv_5009.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5010 OUTPUT.
  PERFORM init_alv_5010.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_5009
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_5009 .
  IF g_cc_5009 IS INITIAL.
    CREATE OBJECT g_cc_5009 EXPORTING container_name = g_cont_5009.
    CREATE OBJECT g_grid_5009 EXPORTING i_parent = g_cc_5009.
  ENDIF.

  PERFORM build_fieldcatalog_5009.
  PERFORM toolbar_alv.

  l_stable-row         = abap_true.
  l_stable-col         = abap_true.
  w_layout-zebra       = abap_false.
  w_layout-no_totarr   = abap_true.
  w_layout-no_totexp   = abap_true.
  w_layout-no_totline  = abap_true.
  w_layout-no_toolbar  = abap_false.
  w_layout-info_fname  = 'COLOR'.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid_5009->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldc_5009
      it_outtab            = t_nrefe.

*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*  IF m_event_handler IS INITIAL.
*    CREATE OBJECT m_event_handler.
*    SET HANDLER : m_event_handler->toolbar FOR g_grid.
*    SET HANDLER : m_event_handler->user_command FOR g_grid.
*  ENDIF.
*
*  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid,
*               lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid_5009->refresh_table_display
    EXPORTING
      is_stable = l_stable.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_5009
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_5009 .
  FREE: t_fieldc_5009.

  CLEAR ls_fieldcat5009.
  ls_fieldcat5009-tabname   = 'T_NREFE'.
  ls_fieldcat5009-fieldname = 'REFNFE'.
  ls_fieldcat5009-ref_table = ''.
  ls_fieldcat5009-ref_field = ''.
  ls_fieldcat5009-col_pos   = 1.
  ls_fieldcat5009-outputlen = 45.
  ls_fieldcat5009-dd_outlen = 45.
  ls_fieldcat5009-coltext   = 'Chave de acesso da NF-e referenciada'.
  APPEND ls_fieldcat5009 TO t_fieldc_5009.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_5010
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_5010 .
  IF g_cc_5010  IS INITIAL.
    CREATE OBJECT g_cc_5010 EXPORTING container_name = g_cont_5010.
    CREATE OBJECT g_grid_5010 EXPORTING i_parent = g_cc_5010.
  ENDIF.

  PERFORM build_fieldcatalog_5010.
  PERFORM toolbar_alv.

  l_stable-row         = abap_true.
  l_stable-col         = abap_true.
  w_layout-zebra       = abap_false.
  w_layout-no_totarr   = abap_true.
  w_layout-no_totexp   = abap_true.
  w_layout-no_totline  = abap_true.
  w_layout-no_toolbar  = abap_false.
  w_layout-info_fname  = 'COLOR'.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid_5010->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldc_5010
      it_outtab            = t_nreff.

*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*  IF m_event_handler IS INITIAL.
*    CREATE OBJECT m_event_handler.
*    SET HANDLER : m_event_handler->toolbar FOR g_grid.
*    SET HANDLER : m_event_handler->user_command FOR g_grid.
*  ENDIF.
*
*  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid,
*               lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid_5010->refresh_table_display
    EXPORTING
      is_stable = l_stable.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_5009
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_5010 .
  FREE: t_fieldc_5010.

*  CLEAR ls_fieldcat5010.
*  ls_fieldcat5010-tabname   = 'T_NREFF'.
*  ls_fieldcat5010-fieldname = 'ICONE'.
*  ls_fieldcat5010-ref_table = ''.
*  ls_fieldcat5010-ref_field = ''.
*  ls_fieldcat5010-col_pos   = 1.
*  ls_fieldcat5010-outputlen = 02.
*  ls_fieldcat5010-dd_outlen = 02.
*  ls_fieldcat5010-coltext   = ''.
*  APPEND ls_fieldcat5010 TO t_fieldc_5010.

  ls_fieldcat5010-tabname   = 'T_NREFF'.
  ls_fieldcat5010-fieldname = 'CUF' .
  ls_fieldcat5010-ref_table = ''.
  ls_fieldcat5010-ref_field = ''.
  ls_fieldcat5010-col_pos   = 1.
  ls_fieldcat5010-outputlen = 06.
  ls_fieldcat5010-dd_outlen = 06.
  ls_fieldcat5010-coltext   = 'Estado'.
  APPEND ls_fieldcat5010 TO t_fieldc_5010.

  ls_fieldcat5010-tabname   = 'T_NREFF'.
  ls_fieldcat5010-fieldname = 'AAMM' .
  ls_fieldcat5010-ref_table = ''.
  ls_fieldcat5010-ref_field = ''.
  ls_fieldcat5010-col_pos   = 1.
  ls_fieldcat5010-outputlen = 08.
  ls_fieldcat5010-dd_outlen = 08.
  ls_fieldcat5010-coltext   = 'Ano/Mês'.
  APPEND ls_fieldcat5010 TO t_fieldc_5010.

  ls_fieldcat5010-tabname   = 'T_NREFF'.
  ls_fieldcat5010-fieldname = 'CNPJ'.
  ls_fieldcat5010-ref_table = ''.
  ls_fieldcat5010-ref_field = ''.
  ls_fieldcat5010-col_pos   = 1.
  ls_fieldcat5010-outputlen = 15.
  ls_fieldcat5010-dd_outlen = 15.
  ls_fieldcat5010-coltext   = 'CNPJ'.

  APPEND ls_fieldcat5010 TO t_fieldc_5010.
  ls_fieldcat5010-tabname   = 'T_NREFF'.
  ls_fieldcat5010-fieldname = 'MOD'  .
  ls_fieldcat5010-ref_table = ''.
  ls_fieldcat5010-ref_field = ''.
  ls_fieldcat5010-col_pos   = 1.
  ls_fieldcat5010-outputlen = 16.
  ls_fieldcat5010-dd_outlen = 16.
  ls_fieldcat5010-coltext   = 'Modelo DocFiscal'.

  APPEND ls_fieldcat5010 TO t_fieldc_5010.
  ls_fieldcat5010-tabname   = 'T_NREFF'.
  ls_fieldcat5010-fieldname = 'SERIE'.
  ls_fieldcat5010-ref_table = ''.
  ls_fieldcat5010-ref_field = ''.
  ls_fieldcat5010-col_pos   = 1.
  ls_fieldcat5010-outputlen = 08.
  ls_fieldcat5010-dd_outlen = 08.
  ls_fieldcat5010-coltext   = 'Serie'.
  APPEND ls_fieldcat5010 TO t_fieldc_5010.

  ls_fieldcat5010-tabname   = 'T_NREFF'.
  ls_fieldcat5010-fieldname = 'NNF'.
  ls_fieldcat5010-ref_table = ''.
  ls_fieldcat5010-ref_field = ''.
  ls_fieldcat5010-col_pos   = 1.
  ls_fieldcat5010-outputlen = 17.
  ls_fieldcat5010-dd_outlen = 17.
  ls_fieldcat5010-coltext   = 'Número DocFiscal'.
  APPEND ls_fieldcat5010 TO t_fieldc_5010.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_5026  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5126 OUTPUT.
  PERFORM init_alv_5126.
  CALL METHOD cl_gui_cfw=>flush.

*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_5126
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_5126 .

  IF g_cc_5126 IS INITIAL.
    CREATE OBJECT g_cc_5126 EXPORTING container_name = g_cont_5126.
    CREATE OBJECT g_grid_5126 EXPORTING i_parent = g_cc_5126.
  ENDIF.

  PERFORM build_fieldcatalog_5126.
  PERFORM toolbar_alv.

  l_stable-row         = abap_true.
  l_stable-col         = abap_true.
  w_layout-zebra       = abap_false.
* w_layout-edit        = abap_true. " Makes all Grid editable
  w_layout-no_totarr   = abap_true.
  w_layout-no_totexp   = abap_true.
  w_layout-no_totline  = abap_true.
  w_layout-no_toolbar  = abap_false.
  w_layout-cwidth_opt  = abap_true.
  w_layout-info_fname  = 'COLOR'.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid_5126->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldc_5126
*     it_sort              = lt_sort
      it_outtab            = t_dup.

  CALL METHOD g_grid_5126->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid_5126->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
    SET HANDLER : m_event_handler->toolbar FOR g_grid_5126.
    SET HANDLER : m_event_handler->user_command FOR g_grid_5126.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid_5126,
               lcl_event_handler=>on_double_click  FOR g_grid_5126.

  CALL METHOD g_grid_5126->refresh_table_display
    EXPORTING
      is_stable = l_stable.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_5126
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog_5126 .

  FREE: t_fieldc_5126.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_DUP'.
  ls_fieldcatalog-fieldname = 'NDUP'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 07.
  ls_fieldcatalog-dd_outlen = 07.
  ls_fieldcatalog-coltext   = 'Numero de Duplicatas.'.
  APPEND ls_fieldcatalog  TO t_fieldc_5126.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_DUP'.
  ls_fieldcatalog-fieldname = 'DVENC'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 81.
  ls_fieldcatalog-dd_outlen = 81.
  ls_fieldcatalog-coltext   = 'Data Vencimento'.
  APPEND ls_fieldcatalog  TO t_fieldc_5126.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_DUP'.
  ls_fieldcatalog-fieldname = 'VDUP'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-coltext   = 'Valor Duplicata'.
  APPEND ls_fieldcatalog  TO t_fieldc_5126.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5126  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5126 INPUT.
  CLEAR ok_code.
ENDMODULE.
