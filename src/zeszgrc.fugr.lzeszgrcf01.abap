*----------------------------------------------------------------------*
***INCLUDE LZGRCF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONVERT_UTC_TO_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ADDR1_VAL_TIME_ZONE  text
*      <--P_LV_TIME  text
*----------------------------------------------------------------------*

FORM convert_utc_to_time USING p_tzone TYPE ad_tzone        "2050660
                      CHANGING p_time_to_convert.           "2050660
                                                            "2050660
  DATA lv_stamp TYPE timestamp.                             "2050660
  DATA dat TYPE sy-datum.                                   "2050660
  DATA tim TYPE sy-uzeit.                                   "2050660
                                                            "2050660
  GET TIME STAMP FIELD lv_stamp.                            "2050660
                                                            "2050660
  CONVERT TIME STAMP lv_stamp TIME ZONE p_tzone             "2050660
     INTO DATE dat                                          "2050660
     TIME tim.                                              "2050660
                                                            "2050660
  p_time_to_convert = tim.                                  "2050660
                                                            "2050660
ENDFORM.                                                    "2050660

FORM convert_timespan_to_utc USING p_time_zone TYPE ad_tzone
                          CHANGING p_timespan_to_convert.

  DATA time_stamp TYPE timestamp.
  DATA date       TYPE sy-datum.
  DATA time       TYPE sy-uzeit.
  DATA tzon       TYPE ttzz-tzone.

  date = p_timespan_to_convert(8).
  time = p_timespan_to_convert+8(6).
  tzon = p_time_zone.

  CONVERT DATE date
          TIME time
          INTO TIME STAMP time_stamp
          TIME ZONE tzon.

  p_timespan_to_convert = time_stamp.

ENDFORM.                    " CONVERT_TIMESPAN_TO_UTC
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_FORN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_nfe_forn USING p_status
                                p_code_sefaz.

*  Atualiza Tabela zib_nfe_forn

  IF gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj IS NOT INITIAL.
    gs_zib_nfe_forn-nu_chave_cnpj   = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj.
  ELSE.
    gs_zib_nfe_forn-nu_chave_cnpj   = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cpf.
  ENDIF.

  gs_zib_nfe_forn-nu_chave_modelo = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-mod.
  gs_zib_nfe_forn-nu_chave_serie  = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-serie.
  gs_zib_nfe_forn-nu_chave_numero = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.
  gs_zib_nfe_forn-st_nota         = p_status.


  PERFORM zf_get_data_utc USING gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                       CHANGING gs_zib_nfe_forn-dt_emissao.

  gs_zib_nfe_forn-nu_protocolo = gs_xml_sefaz-nfeproc-protnfe-infprot-nprot.

  PERFORM zf_get_data_utc USING gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       CHANGING gs_zib_nfe_forn-dt_protocolo.

  PERFORM zf_get_hora_utc USING gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       CHANGING gs_zib_nfe_forn-hr_protocolo.

  gs_zib_nfe_forn-nu_chave_regiao  = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe(2).
  gs_zib_nfe_forn-nu_chave_ano     = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+2(2).
  gs_zib_nfe_forn-nu_chave_mes     = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+4(2).
  gs_zib_nfe_forn-nu_chave_aleator = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+34(9).
  gs_zib_nfe_forn-nu_chave_dv      = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+43(1).
  gs_zib_nfe_forn-nu_chave         = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
  gs_zib_nfe_forn-nu_ie            = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie.
  gs_zib_nfe_forn-nu_code          = p_code_sefaz.

  PERFORM zf_buscar_empresa USING gs_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-ie
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-indiedest
                         CHANGING gs_zib_nfe_forn-bukrs
                                  gs_zib_nfe_forn-branch.

  gs_zib_nfe_forn-xmlvers = gs_xml_sefaz-nfeproc-nfe-infnfe-a_versao.
  gs_zib_nfe_forn-dt_atualizacao = sy-datum.
  gs_zib_nfe_forn-hr_atualizacao = sy-uzeit.

*  gs_zib_nfe_forn-DOCNUM = gs_xml_sefaz
*  gs_zib_nfe_forn-ATUALIZADO = gs_xml_sefaz
*  gs_zib_nfe_forn-VLR_NOTA = gs_xml_sefaz
*  gs_zib_nfe_forn-INC_MANUAL = gs_xml_sefaz
*  gs_zib_nfe_forn-MANIFESTO_ENV = gs_xml_sefaz
*  gs_zib_nfe_forn-STOP_ENV_MAN = gs_xml_sefaz
*  gs_zib_nfe_forn-DOC_MANIFESTO = gs_xml_sefaz
*  gs_zib_nfe_forn-QTDE_ENV_MAN = gs_xml_sefaz
  MODIFY zib_nfe_forn FROM gs_zib_nfe_forn.

ENDFORM.

FORM zf_atribuir_nfe_forn_cte USING p_status
                                    p_code_sefaz.

  DATA: v_cd_tomador TYPE c.

*  Atualiza Tabela zib_nfe_forn

  gs_zib_nfe_forn-nu_chave_cnpj    = gwa_cte_read-chave+6(14).
  gs_zib_nfe_forn-nu_chave_modelo  = gwa_cte_read-chave+20(02).
  gs_zib_nfe_forn-nu_chave_serie   = gwa_cte_read-chave+22(03).
  gs_zib_nfe_forn-nu_chave_numero  = gwa_cte_read-chave+25(09).
  gs_zib_nfe_forn-nu_chave_regiao  = gwa_cte_read-chave(2).
  gs_zib_nfe_forn-nu_chave_ano     = gwa_cte_read-chave+2(2).
  gs_zib_nfe_forn-nu_chave_mes     = gwa_cte_read-chave+4(2).
  gs_zib_nfe_forn-nu_chave_aleator = gwa_cte_read-chave+34(9).
  gs_zib_nfe_forn-nu_chave_dv      = gwa_cte_read-chave+43(1).
  gs_zib_nfe_forn-nu_chave         = gwa_cte_read-chave.
  gs_zib_nfe_forn-st_nota          = p_status.
  gs_zib_nfe_forn-nu_code          = p_code_sefaz.
  gs_zib_nfe_forn-dt_emissao       = gwa_cte_read-dt_emissao.
  gs_zib_nfe_forn-nu_protocolo     = gwa_cte_read-protocolo.
  gs_zib_nfe_forn-dt_protocolo     = gwa_cte_read-dt_protocolo.
  gs_zib_nfe_forn-hr_protocolo     = gwa_cte_read-hr_protocolo.
  gs_zib_nfe_forn-nu_ie            = gwa_cte_read-ie_emissor.
  gs_zib_nfe_forn-xmlvers          = gwa_cte_read-versao_xml.
  gs_zib_nfe_forn-dt_atualizacao   = sy-datum.
  gs_zib_nfe_forn-hr_atualizacao   = sy-uzeit.
  PERFORM zf_get_dados_tomador CHANGING gs_zib_nfe_forn-bukrs
                                        gs_zib_nfe_forn-branch
                                        v_cd_tomador.

  MODIFY zib_nfe_forn FROM gs_zib_nfe_forn.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_GET_DATA_UTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_get_data_utc USING p_data_hora
                  CHANGING c_data.

  CHECK p_data_hora IS NOT INITIAL.

  CHECK strlen( p_data_hora ) GE 10.

  c_data = p_data_hora(4) &&
           p_data_hora+05(02) &&
           p_data_hora+08(02).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GET_HORA_UTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_get_hora_utc USING p_data_hora
                  CHANGING c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  CHECK strlen( p_data_hora ) GE 19.

  c_hora  = p_data_hora+11(02) &&
            p_data_hora+14(02) &&
            p_data_hora+17(02).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_buscar_empresa USING p_cnpj
                             p_ie
                             p_ind_ie
                    CHANGING p_bukrs
                             p_branch.

  CLEAR: p_bukrs, p_branch.

  DATA(_force_ck_ie) = abap_false.

  IF ( p_ind_ie EQ '2' ) AND ( p_ie IS INITIAL ). "Contribuinte isento de Inscrição no cadastro de Contribuintes do ICMS
    p_ie         = 'ISENTO'.
    _force_ck_ie = abap_true.
  ENDIF.

  IF ( p_ind_ie EQ '9' ). "Não Contribuinte, que pode ou não possuir Inscrição Estadual no Cadastro de Contribuintes do ICMS.
    _force_ck_ie = abap_true.
  ENDIF.

  CHECK ( p_cnpj IS NOT INITIAL ) AND ( p_ie IS NOT INITIAL OR p_ind_ie EQ '9' ).

  TRY .

      zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro_cnpj_cpf_ie(
        EXPORTING
          i_cnpj             = CONV #( p_cnpj )
          i_insc_estatual    = CONV #( p_ie )
          i_ck_ie            = CONV #( _force_ck_ie )
        )->ck_parceiro_local_negocio(
         IMPORTING
           e_j_1bbranch  = DATA(e_j_1bbranch)
        ).

      p_bukrs  = e_j_1bbranch-bukrs.
      p_branch = e_j_1bbranch-branch.

    CATCH zcx_parceiros.    "

  ENDTRY.

*  CLEAR: P_BUKRS, P_BRANCH.
*
*  SELECT SINGLE BUKRS BRANCH
*    INTO (P_BUKRS, P_BRANCH)
*    FROM J_1BBRANCH
*   WHERE STCD1 = P_CNPJ.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_DIST_TER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_nfe_dist_ter USING p_status
                                    p_cd_st_sefaz.

  DATA: lt_zintg_pag TYPE zeszintg_pag,
        ls_zintg_pag TYPE zeszdetpag.

  DATA: ls_zintg_t_dup TYPE zeszintg_dup.

* Atualiza Tabela ZESZIB_NFE_DIST_TER
  gs_zib_nfe_dist_ter-chave_nfe = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
  gs_zib_nfe_dist_ter-forne_cnpj = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj.
  gs_zib_nfe_dist_ter-forne_cpf = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cpf.
  gs_zib_nfe_dist_ter-forne_ie = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie.
  gs_zib_nfe_dist_ter-forne_razao = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-xnome.
  gs_zib_nfe_dist_ter-destino_cnpj = gs_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj.
  gs_zib_nfe_dist_ter-destino_ie = gs_xml_sefaz-nfeproc-nfe-infnfe-dest-ie.
  gs_zib_nfe_dist_ter-numero = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.

  PERFORM zf_get_data_utc USING gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                       CHANGING gs_zib_nfe_dist_ter-dt_emissao.

  PERFORM zf_get_hora_utc USING gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                       CHANGING gs_zib_nfe_dist_ter-hr_emissao.

  gs_zib_nfe_dist_ter-serie = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-serie.
  gs_zib_nfe_dist_ter-model = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-mod.
  gs_zib_nfe_dist_ter-regio = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe(2).
  gs_zib_nfe_dist_ter-nfyear = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+2(2).
  gs_zib_nfe_dist_ter-nfmonth = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+4(2).
  gs_zib_nfe_dist_ter-docnum9 = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+34(9).
  gs_zib_nfe_dist_ter-cdv = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+43(1).
  gs_zib_nfe_dist_ter-ds_nat_operacao = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-natop.
  gs_zib_nfe_dist_ter-indicador_presenca = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-indpres. "USER STORY 170650 / AOENNING

  lt_zintg_pag[] = gs_xml_sefaz-nfeproc-nfe-infnfe-pag[].

  READ TABLE lt_zintg_pag INTO ls_zintg_pag INDEX 1.

  gs_zib_nfe_dist_ter-cd_form_pag = ls_zintg_pag-indpag.

  gs_zib_nfe_dist_ter-cd_tipo_doc = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-tpnf.
  gs_zib_nfe_dist_ter-cd_form_emissao = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-tpemis.

  PERFORM zf_get_data_utc USING gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient
                       CHANGING gs_zib_nfe_dist_ter-dt_saida.

  PERFORM zf_get_hora_utc USING gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient
                       CHANGING gs_zib_nfe_dist_ter-hr_saida.

  gs_zib_nfe_dist_ter-cd_fina_emissao = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-finnfe.
  gs_zib_nfe_dist_ter-nr_protocolo = gs_xml_sefaz-nfeproc-protnfe-infprot-nprot.

  PERFORM zf_get_data_utc USING gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       CHANGING gs_zib_nfe_dist_ter-dt_protocolo.

  PERFORM zf_get_hora_utc USING gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       CHANGING gs_zib_nfe_dist_ter-hr_protocolo.

*  gs_ZIB_NFE_DIST_TER-NR_PED_COMPRA = gs_xml_sefaz-XPED.
  gs_zib_nfe_dist_ter-nr_ctr_compra = gs_xml_sefaz-nfeproc-nfe-infnfe-compra-xcont.
  gs_zib_nfe_dist_ter-nr_fatura = gs_xml_sefaz-nfeproc-nfe-infnfe-cobr-fat-nfat.
  gs_zib_nfe_dist_ter-vl_total_fatura = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vprod.
  gs_zib_nfe_dist_ter-vl_desco_fatura = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vdesc.
  gs_zib_nfe_dist_ter-vl_liquido = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vnf.
  gs_zib_nfe_dist_ter-vl_icms_base = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vbc.
  gs_zib_nfe_dist_ter-vl_icms_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vicms.
  gs_zib_nfe_dist_ter-vl_icms_st_base = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vbcst.
  gs_zib_nfe_dist_ter-vl_icms_st_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vst.
  gs_zib_nfe_dist_ter-vl_produtos = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vprod.
  gs_zib_nfe_dist_ter-vl_frete = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vfrete.
  gs_zib_nfe_dist_ter-vl_seguro = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vseg.
  gs_zib_nfe_dist_ter-vl_desconto = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vdesc.
  gs_zib_nfe_dist_ter-vl_icms_desonerado = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vicmsdeson.
  gs_zib_nfe_dist_ter-vl_ii_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vii.
  gs_zib_nfe_dist_ter-vl_ipi_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vipi.
  gs_zib_nfe_dist_ter-vl_pis_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vpis.
  gs_zib_nfe_dist_ter-vl_cof_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vcofins.
  gs_zib_nfe_dist_ter-vl_despesas = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-voutro.
  gs_zib_nfe_dist_ter-vl_total = gs_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vnf.

  PERFORM zf_buscar_empresa USING gs_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-ie
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-indiedest
                         CHANGING gs_zib_nfe_dist_ter-bukrs
                                  gs_zib_nfe_dist_ter-branch.

  "=========================================================================================Ajuste BUG 48012 / AOENNING
  IF gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj IS NOT INITIAL.
    IF gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie = 'ISENTO' OR
       gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie IS INITIAL.

      DATA(xndiedest) = '2'.
    ELSE.
      xndiedest = '1'.
    ENDIF.

    PERFORM zf_buscar_empresa USING gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj
                                    gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie xndiedest
                            CHANGING gs_zib_nfe_dist_ter-bukrs_e gs_zib_nfe_dist_ter-branch_e.
  ENDIF.
  "========================================================================================

  PERFORM zf_atrib_st_sefaz_zib_nfe USING p_status
                                          p_cd_st_sefaz
                                 CHANGING gs_zib_nfe_dist_ter.

*  gs_ZIB_NFE_DIST_TER-INC_MANUAL
  gs_zib_nfe_dist_ter-xmlvers = gs_xml_sefaz-nfeproc-nfe-infnfe-a_versao.
*  gs_ZIB_NFE_DIST_TER-DOCNUM_NFE
  gs_zib_nfe_dist_ter-e_tomadora = gs_zib_nfe_dist_ter-bukrs.
  gs_zib_nfe_dist_ter-f_tomadora = gs_zib_nfe_dist_ter-branch.
*  gs_zib_nfe_dist_ter-p_emissor =
*  gs_zib_nfe_dist_ter-ebeln =
*  gs_ZIB_NFE_DIST_TER-BELNR
*  gs_ZIB_NFE_DIST_TER-GJAHR
*  gs_ZIB_NFE_DIST_TER-ST_FISCAL
*  gs_ZIB_NFE_DIST_TER-ST_FISICO
*  gs_ZIB_NFE_DIST_TER-ST_ARMAZEM
*  gs_ZIB_NFE_DIST_TER-ST_DOCUMENTO
*  gs_ZIB_NFE_DIST_TER-CD_DEPARTAMENTO
*  gs_ZIB_NFE_DIST_TER-F_ARMAZEM
*  gs_ZIB_NFE_DIST_TER-ARMAZEM_CNPJ
*  gs_ZIB_NFE_DIST_TER-ARMAZEM_IE
*  gs_ZIB_NFE_DIST_TER-ARMAZEM_RAZAO
*  gs_ZIB_NFE_DIST_TER-WAERK
*  gs_ZIB_NFE_DIST_TER-TIMESTAMP
*  gs_ZIB_NFE_DIST_TER-MANUAL
*  gs_ZIB_NFE_DIST_TER-CK_POSSUI_FRETE
*  gs_ZIB_NFE_DIST_TER-F_TRANSPORTE
*  gs_ZIB_NFE_DIST_TER-VBELN
*  gs_ZIB_NFE_DIST_TER-TKNUM
*  gs_ZIB_NFE_DIST_TER-FKNUM
*  gs_ZIB_NFE_DIST_TER-CK_FISCAL
*  gs_ZIB_NFE_DIST_TER-CK_FISICO
*  gs_ZIB_NFE_DIST_TER-CK_ARMAZEM
*  gs_ZIB_NFE_DIST_TER-MWSKZ
*  gs_ZIB_NFE_DIST_TER-MBLNR
*  gs_ZIB_NFE_DIST_TER-MJAHR
*  gs_ZIB_NFE_DIST_TER-CD_ROMANEIO
*  gs_ZIB_NFE_DIST_TER-NR_FASE
  READ TABLE gs_xml_sefaz-nfeproc-nfe-infnfe-cobr-dup INTO ls_zintg_t_dup INDEX 1.
  IF sy-subrc = 0 AND ls_zintg_t_dup-dvenc IS NOT INITIAL.
    CONCATENATE ls_zintg_t_dup-dvenc(04) ls_zintg_t_dup-dvenc+5(02) ls_zintg_t_dup-dvenc+8(02) INTO gs_zib_nfe_dist_ter-dt_vencimento.
*    gs_zib_nfe_dist_ter-dt_vencimento = ls_zintg_t_dup-dvenc.
  ELSE.
    gs_zib_nfe_dist_ter-dt_vencimento = ''.
  ENDIF.

*  gs_ZIB_NFE_DIST_TER-ZBVTYP
*  gs_ZIB_NFE_DIST_TER-ZLSPR
*  gs_ZIB_NFE_DIST_TER-LAND1
*  gs_ZIB_NFE_DIST_TER-PYMT_METH
*  gs_ZIB_NFE_DIST_TER-HOUSEBANKID
*  gs_ZIB_NFE_DIST_TER-PC_PARTINER
*  gs_ZIB_NFE_DIST_TER-LR_PARTINER
*  gs_ZIB_NFE_DIST_TER-CK_COMPRA_FUTURA
*  gs_ZIB_NFE_DIST_TER-TP_COMPRA_FUTURA
*  gs_ZIB_NFE_DIST_TER-VLR_DESCONTO
*  gs_ZIB_NFE_DIST_TER-OBS_FINANCEIRA
  gs_zib_nfe_dist_ter-transportador_cnpj = gs_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cnpj.
  gs_zib_nfe_dist_ter-transportador_cpf  = gs_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cpf.
  gs_zib_nfe_dist_ter-transportador_ie   = gs_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-ie.
  gs_zib_nfe_dist_ter-dt_atualizacao = sy-datum.
  gs_zib_nfe_dist_ter-hr_atualizacao = sy-uzeit.
*  gs_ZIB_NFE_DIST_TER-BOLETO
*  gs_ZIB_NFE_DIST_TER-ID_SIMETRYA
*  gs_ZIB_NFE_DIST_TER-US_MIRO
*  gs_ZIB_NFE_DIST_TER-CTR_WAERS
*  gs_ZIB_NFE_DIST_TER-CTR_WKURS
*  gs_ZIB_NFE_DIST_TER-CTR_KUFIX
*  gs_ZIB_NFE_DIST_TER-CTR_SINAL
*  gs_ZIB_NFE_DIST_TER-CTR_VALOR_TOTAL
*  gs_ZIB_NFE_DIST_TER-CTR_ZTERM
*  gs_ZIB_NFE_DIST_TER-SE_STATUS
*  gs_ZIB_NFE_DIST_TER-SE_CODE
*  gs_ZIB_NFE_DIST_TER-SE_DETAIL
*  gs_ZIB_NFE_DIST_TER-SE_RECORDKEY
*  gs_ZIB_NFE_DIST_TER-SE_RECORDID
*  gs_ZIB_NFE_DIST_TER-FORNE_CPF
*  gs_ZIB_NFE_DIST_TER-CK_REVISAO
*  gs_ZIB_NFE_DIST_TER-CK_TRANS_NF_PROPRI
*  gs_ZIB_NFE_DIST_TER-MBLNR_ARM
*  gs_ZIB_NFE_DIST_TER-MJAHR_ARM
*  gs_ZIB_NFE_DIST_TER-DOCNUM_ARM
  MODIFY zeszib_nfe_dist_ter FROM gs_zib_nfe_dist_ter.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_DIST_ITM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_nfe_dist_itm .

  FIELD-SYMBOLS: <zeszintg_det> TYPE zeszdet_nitem.

  DATA: lva_vlr_unit_com  TYPE p,
        lva_vlr_unit_trib TYPE p.

  DATA: gt_zintg_det TYPE zeszintg_det.

  gt_zintg_det[] = gs_xml_sefaz-nfeproc-nfe-infnfe-det[].

  DELETE FROM zib_nfe_dist_itm WHERE chave_nfe EQ gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.

* Atualiza Tabela ZIB_NFE_DIST_ITM
  LOOP AT gt_zintg_det ASSIGNING <zeszintg_det>.

    CLEAR: gs_zib_nfe_dist_itm.

    lva_vlr_unit_com  = <zeszintg_det>-prod-vuncom.
    lva_vlr_unit_trib = <zeszintg_det>-prod-vuntrib.

    gs_zib_nfe_dist_itm-chave_nfe = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
    gs_zib_nfe_dist_itm-prod_item = <zeszintg_det>-a_nitem.
    gs_zib_nfe_dist_itm-prod_codigo = <zeszintg_det>-prod-cprod.
    gs_zib_nfe_dist_itm-prod_cfop = <zeszintg_det>-prod-cfop.
    gs_zib_nfe_dist_itm-prod_descricao = <zeszintg_det>-prod-xprod.
    gs_zib_nfe_dist_itm-prod_ean = <zeszintg_det>-prod-cean.
    gs_zib_nfe_dist_itm-prod_ncm = <zeszintg_det>-prod-ncm.
    gs_zib_nfe_dist_itm-prod_extipi = <zeszintg_det>-prod-extipi.
*  gs_zib_nfe_dist_itm-PROD_NCM_GENERO
    gs_zib_nfe_dist_itm-prod_und_comerci = <zeszintg_det>-prod-ucom.
    gs_zib_nfe_dist_itm-prod_qtd_comerci = <zeszintg_det>-prod-qcom.

    IF lva_vlr_unit_com <= 9999999999.
      gs_zib_nfe_dist_itm-prod_vlr_und_com = <zeszintg_det>-prod-vuncom.
    ENDIF.

    gs_zib_nfe_dist_itm-prod_vlr_total_b = <zeszintg_det>-prod-vprod.
    gs_zib_nfe_dist_itm-prod_ean_trib = <zeszintg_det>-prod-ceantrib .
    gs_zib_nfe_dist_itm-prod_und_trib = <zeszintg_det>-prod-utrib.
    gs_zib_nfe_dist_itm-prod_qtd_trib = <zeszintg_det>-prod-qtrib.

    IF lva_vlr_unit_trib <= 9999999999.
      gs_zib_nfe_dist_itm-prod_vlr_und_tri = <zeszintg_det>-prod-vuntrib.
    ENDIF.

    gs_zib_nfe_dist_itm-prod_vl_frete = <zeszintg_det>-prod-vfrete.
    gs_zib_nfe_dist_itm-prod_vl_seguro = <zeszintg_det>-prod-vseg.
    gs_zib_nfe_dist_itm-prod_vl_desconto = <zeszintg_det>-prod-vdesc.
    gs_zib_nfe_dist_itm-prod_vl_outro = <zeszintg_det>-prod-voutro.
    gs_zib_nfe_dist_itm-prod_ind_total = <zeszintg_det>-prod-indtot.
    gs_zib_nfe_dist_itm-prod_pedido_comp = <zeszintg_det>-prod-xped.
*  gs_zib_nfe_dist_itm-PROD_NR_PED_COMP


    "GS_ZIB_NFE_DIST_ITM-ICMS_CST = <ZESZINTG_DET>-IMPOSTO-ICMS-CST.

    IF <zeszintg_det>-imposto-icms-icms00-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms00-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms00-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms10-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms10-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms10-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms20-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms20-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms20-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms30-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms30-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms30-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms40-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms40-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms40-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms51-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms51-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms51-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms60-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms60-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms60-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms70-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms70-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms70-orig.
    ELSEIF <zeszintg_det>-imposto-icms-icms90-cst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icms90-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icms90-orig.
*** US #181947 - MMSILVA - 18.06.2025 - Ini ***
    ELSEIF <zeszintg_det>-imposto-icms-icmssn202 IS NOT INITIAL.
      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icmssn202-csosn+1(2).
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icmssn202-orig.
*** US #181947 - MMSILVA - 18.06.2025 - Fim ***
    ENDIF.


*   DETERMINAÇÃO ICMS

*ICMS ST - NOTAS FISCAIS DE VEÍCULO - BG #155190 - INICIO
    IF <zeszintg_det>-imposto-icms-icmspart-cst IS NOT INITIAL.

      gs_zib_nfe_dist_itm-icms_cst        = <zeszintg_det>-imposto-icms-icmspart-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <zeszintg_det>-imposto-icms-icmspart-orig.
      gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icmspart-modbc.
      gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icmspart-vbc.
      gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icmspart-picms.
      gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icmspart-predbc.
      gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icmspart-vicms.
      gs_zib_nfe_dist_itm-icms_st_md_base = <zeszintg_det>-imposto-icms-icmspart-modbcst.
      gs_zib_nfe_dist_itm-icms_st_base    = <zeszintg_det>-imposto-icms-icmspart-vbcst.
      gs_zib_nfe_dist_itm-icms_st_aqt     = <zeszintg_det>-imposto-icms-icmspart-picmsst.
      gs_zib_nfe_dist_itm-icms_st_valor   = <zeszintg_det>-imposto-icms-icmspart-vicmsst.
      gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icmspart-motdesicms.
      gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icmspart-vicmsdeson.
      gs_zib_nfe_dist_itm-icms_pbcop = <zeszintg_det>-imposto-icms-icmspart-pbcop.
      gs_zib_nfe_dist_itm-icms_ufst = <zeszintg_det>-imposto-icms-icmspart-ufst.

    ELSE.

      CASE gs_zib_nfe_dist_itm-icms_cst.
        WHEN '00'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms00-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms00-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms00-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms00-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms00-vicms.

          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms00-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms00-vicmsdeson.

        WHEN '10'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms10-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms10-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms10-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms10-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms10-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <zeszintg_det>-imposto-icms-icms10-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <zeszintg_det>-imposto-icms-icms10-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <zeszintg_det>-imposto-icms-icms10-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <zeszintg_det>-imposto-icms-icms10-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms10-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms10-vicmsdeson.

        WHEN '20'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms20-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms20-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms20-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms20-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms20-vicms.

          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms20-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms20-vicmsdeson.

        WHEN '30'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms30-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms30-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms30-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms30-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms30-vicms.


          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <zeszintg_det>-imposto-icms-icms30-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <zeszintg_det>-imposto-icms-icms30-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <zeszintg_det>-imposto-icms-icms30-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <zeszintg_det>-imposto-icms-icms30-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms30-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms30-vicmsdeson.


        WHEN '40' OR '41' OR '50'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms40-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms40-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms40-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms40-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms40-vicms.
*  gs_zib_nfe_dist_itm-ICMS_ST_MD_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_MARGEM
*  gs_zib_nfe_dist_itm-ICMS_ST_RED_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_AQT
*  gs_zib_nfe_dist_itm-ICMS_ST_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_VALOR
          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms40-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms40-vicmsdeson.

        WHEN '51'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms51-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms51-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms51-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms51-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms51-vicms.
*  gs_zib_nfe_dist_itm-ICMS_ST_MD_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_MARGEM
*  gs_zib_nfe_dist_itm-ICMS_ST_RED_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_AQT
*  gs_zib_nfe_dist_itm-ICMS_ST_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_VALOR
          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms51-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms51-vicmsdeson.

        WHEN '60'.
*        gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms160-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms60-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms60-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms60-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms60-vicms.
*  gs_zib_nfe_dist_itm-ICMS_ST_MD_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_MARGEM
*  gs_zib_nfe_dist_itm-ICMS_ST_RED_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_AQT
*  gs_zib_nfe_dist_itm-ICMS_ST_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_VALOR
          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms60-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms60-vicmsdeson.

        WHEN '70'.
*        gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms170-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms70-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms70-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms70-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms70-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <zeszintg_det>-imposto-icms-icms70-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <zeszintg_det>-imposto-icms-icms70-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <zeszintg_det>-imposto-icms-icms70-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <zeszintg_det>-imposto-icms-icms70-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms70-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms70-vicmsdeson.

        WHEN '90'.
          gs_zib_nfe_dist_itm-icms_md_base = <zeszintg_det>-imposto-icms-icms90-modbc.
          gs_zib_nfe_dist_itm-icms_base = <zeszintg_det>-imposto-icms-icms90-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <zeszintg_det>-imposto-icms-icms90-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <zeszintg_det>-imposto-icms-icms90-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <zeszintg_det>-imposto-icms-icms90-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <zeszintg_det>-imposto-icms-icms90-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <zeszintg_det>-imposto-icms-icms90-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <zeszintg_det>-imposto-icms-icms90-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <zeszintg_det>-imposto-icms-icms90-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <zeszintg_det>-imposto-icms-icms90-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <zeszintg_det>-imposto-icms-icms90-vicmsdeson.

*** US #181947 - MMSILVA - 18.06.2025 - Ini ***
        WHEN '02'.
*         gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icmssn202-modbc.
*          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icmssn202--vbc.
*          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icmssn202-picms.
*          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icmssn202-predbc.
*          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icmssn202-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <zeszintg_det>-imposto-icms-icmssn202-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <zeszintg_det>-imposto-icms-icmssn202-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <zeszintg_det>-imposto-icms-icmssn202-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <zeszintg_det>-imposto-icms-icmssn202-vicmsst.
          "Campos ST - WPP


*          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icmssn202-motdesicms.
*          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icmssn202-vicmsdeson.
*** US #181947 - MMSILVA - 18.06.2025 - Fim ***

      ENDCASE.

    ENDIF.
*ICMS ST - NOTAS FISCAIS DE VEÍCULO - BG #155190 - FIM

*   DETERMINAÇÃO IPI
    gs_zib_nfe_dist_itm-ipi_cod_enquadra = <zeszintg_det>-imposto-ipi-cenq.
    gs_zib_nfe_dist_itm-ipi_cla_enquadra = <zeszintg_det>-imposto-ipi-clenq.
    gs_zib_nfe_dist_itm-ipi_cnpj_prod = <zeszintg_det>-imposto-ipi-cnpjprod.
    gs_zib_nfe_dist_itm-ipi_cod_selo_con = <zeszintg_det>-imposto-ipi-cselo.
    gs_zib_nfe_dist_itm-ipi_qtd_selo_con = <zeszintg_det>-imposto-ipi-qselo.


    IF <zeszintg_det>-imposto-ipi-ipint IS NOT INITIAL.
      gs_zib_nfe_dist_itm-ipi_cst = <zeszintg_det>-imposto-ipi-ipint-cst.
    ELSE.
      gs_zib_nfe_dist_itm-ipi_cst = <zeszintg_det>-imposto-ipi-ipitrib-cst.
    ENDIF.

    CASE gs_zib_nfe_dist_itm-ipi_cst .
      WHEN '00' OR '49' OR '50' OR '99'.
        gs_zib_nfe_dist_itm-ipi_cst = <zeszintg_det>-imposto-ipi-ipitrib-cst.
        gs_zib_nfe_dist_itm-ipi_base = <zeszintg_det>-imposto-ipi-ipitrib-vbc.
        gs_zib_nfe_dist_itm-ipi_aqt = <zeszintg_det>-imposto-ipi-ipitrib-pipi.
        gs_zib_nfe_dist_itm-ipi_valor = <zeszintg_det>-imposto-ipi-ipitrib-vipi.
      WHEN '01' OR '02' OR '03' OR '04' OR '51' OR '52' OR '53' OR '54' OR '55'.
        gs_zib_nfe_dist_itm-ipi_cst = <zeszintg_det>-imposto-ipi-ipint-cst.
    ENDCASE.

*   DETERMINAÇÃO PIS
    gs_zib_nfe_dist_itm-pis_cst = <zeszintg_det>-imposto-pis-cst.

    IF <zeszintg_det>-imposto-pis-pisaliq IS NOT INITIAL.
      gs_zib_nfe_dist_itm-pis_base = <zeszintg_det>-imposto-pis-pisaliq-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <zeszintg_det>-imposto-pis-pisaliq-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <zeszintg_det>-imposto-pis-pisaliq-vpis.

      IF gs_zib_nfe_dist_itm-pis_cst IS INITIAL.
        gs_zib_nfe_dist_itm-pis_cst = <zeszintg_det>-imposto-pis-pisaliq-cst.
      ENDIF.

    ELSEIF <zeszintg_det>-imposto-pis-pisqtde IS NOT INITIAL.
      gs_zib_nfe_dist_itm-pis_base = <zeszintg_det>-imposto-pis-pisqtde-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <zeszintg_det>-imposto-pis-pisqtde-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <zeszintg_det>-imposto-pis-pisqtde-vpis.

      IF gs_zib_nfe_dist_itm-pis_cst IS INITIAL.
        gs_zib_nfe_dist_itm-pis_cst = <zeszintg_det>-imposto-pis-pisqtde-cst.
      ENDIF.

    ELSEIF <zeszintg_det>-imposto-pis-pisnt IS NOT INITIAL.

      IF gs_zib_nfe_dist_itm-pis_cst IS INITIAL.
        gs_zib_nfe_dist_itm-pis_cst = <zeszintg_det>-imposto-pis-pisnt-cst.
      ENDIF.

    ELSEIF <zeszintg_det>-imposto-pis-pisoutr IS NOT INITIAL.
      gs_zib_nfe_dist_itm-pis_base = <zeszintg_det>-imposto-pis-pisoutr-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <zeszintg_det>-imposto-pis-pisoutr-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <zeszintg_det>-imposto-pis-pisoutr-vpis.

      IF gs_zib_nfe_dist_itm-pis_cst IS INITIAL.
        gs_zib_nfe_dist_itm-pis_cst = <zeszintg_det>-imposto-pis-pisoutr-cst.
      ENDIF.

    ELSEIF <zeszintg_det>-imposto-pis-pisst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-pis_base = <zeszintg_det>-imposto-pis-pisst-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <zeszintg_det>-imposto-pis-pisst-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <zeszintg_det>-imposto-pis-pisst-vpis.

      IF gs_zib_nfe_dist_itm-pis_cst IS INITIAL.
        gs_zib_nfe_dist_itm-pis_cst = <zeszintg_det>-imposto-pis-pisst-cst.
      ENDIF.

    ENDIF.

*    gs_zib_nfe_dist_itm-PIS_QTD_VENDIDA
*    gs_zib_nfe_dist_itm-PIS_AQT_REAIS
*    gs_zib_nfe_dist_itm-PIS_ST_BASE
*    gs_zib_nfe_dist_itm-PIS_ST_AQT
*    gs_zib_nfe_dist_itm-PIS_ST_QTD_VENDI
*    gs_zib_nfe_dist_itm-PIS_ST_AQT_REAIS
*    gs_zib_nfe_dist_itm-PIS_ST_VALOR

*   DETERMINAÇÃO COFINS
    gs_zib_nfe_dist_itm-cof_cst = <zeszintg_det>-imposto-cofins-cst.

    IF <zeszintg_det>-imposto-cofins-cofinsaliq IS NOT INITIAL.
      gs_zib_nfe_dist_itm-cof_base = <zeszintg_det>-imposto-cofins-cofinsaliq-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <zeszintg_det>-imposto-cofins-cofinsaliq-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <zeszintg_det>-imposto-cofins-cofinsaliq-vcofins.
    ELSEIF <zeszintg_det>-imposto-cofins-cofinsqtde IS NOT INITIAL.
      gs_zib_nfe_dist_itm-cof_base = <zeszintg_det>-imposto-cofins-cofinsqtde-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <zeszintg_det>-imposto-cofins-cofinsqtde-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <zeszintg_det>-imposto-cofins-cofinsqtde-vcofins.
    ELSEIF <zeszintg_det>-imposto-cofins-cofinsoutr IS NOT INITIAL.
      gs_zib_nfe_dist_itm-cof_base = <zeszintg_det>-imposto-cofins-cofinsoutr-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <zeszintg_det>-imposto-cofins-cofinsoutr-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <zeszintg_det>-imposto-cofins-cofinsoutr-vcofins.
    ELSEIF <zeszintg_det>-imposto-cofins-cofinsst IS NOT INITIAL.
      gs_zib_nfe_dist_itm-cof_base = <zeszintg_det>-imposto-cofins-cofinsst-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <zeszintg_det>-imposto-cofins-cofinsst-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <zeszintg_det>-imposto-cofins-cofinsst-vcofins.
    ENDIF.

*    gs_zib_nfe_dist_itm-COF_CST
*    gs_zib_nfe_dist_itm-COF_BASE
*    gs_zib_nfe_dist_itm-COF_AQT
*    gs_zib_nfe_dist_itm-COF_VALOR
*    gs_zib_nfe_dist_itm-COF_QTD_VENDIDA
*    gs_zib_nfe_dist_itm-COF_AQT_REAIS
*    gs_zib_nfe_dist_itm-COF_ST_BASE
*    gs_zib_nfe_dist_itm-COF_ST_AQT
*    gs_zib_nfe_dist_itm-COF_ST_QTD_VENDI
*    gs_zib_nfe_dist_itm-COF_ST_AQT_REAIS
*    gs_zib_nfe_dist_itm-COF_ST_VALOR
*    gs_zib_nfe_dist_itm-INC_MANUAL

*    gs_zib_nfe_dist_itm-DOCNUM_NFE
*    gs_zib_nfe_dist_itm-ITMNUM_NFE
*    gs_zib_nfe_dist_itm-MATNR
*    gs_zib_nfe_dist_itm-EBELN
*    gs_zib_nfe_dist_itm-EBELP
*    gs_zib_nfe_dist_itm-MENGE
*    gs_zib_nfe_dist_itm-MEINS
*    gs_zib_nfe_dist_itm-NETPR
*    gs_zib_nfe_dist_itm-NETWR
*    gs_zib_nfe_dist_itm-BELNR_FT
*    gs_zib_nfe_dist_itm-GJAHR_FT
*    gs_zib_nfe_dist_itm-BUZEI_FT
*    gs_zib_nfe_dist_itm-DELIV_NUMB
*    gs_zib_nfe_dist_itm-DELIV_ITEM
*    gs_zib_nfe_dist_itm-PROD_ITEM_ORIGEM
*    gs_zib_nfe_dist_itm-MBLNR
*    gs_zib_nfe_dist_itm-MJAHR
*    gs_zib_nfe_dist_itm-ZEILE
*    gs_zib_nfe_dist_itm-LGORT
    gs_zib_nfe_dist_itm-dt_atualizacao = sy-datum.

    MODIFY zib_nfe_dist_itm FROM gs_zib_nfe_dist_itm.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_TER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_ter USING p_status p_cod_sefaz.

  DATA: lv_stcd1    TYPE lfa1-stcd1,
        lv_domvalue TYPE dd07v-domvalue_l,
        lv_ddtext   TYPE dd07v-ddtext.

* Atualiza Tabela ZESZIB_CTE_DIST_TER
  gs_zib_cte_dist_ter-cd_chave_cte   = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE
*  GS_ZIB_CTE_DIST_TER-CK_FINALIZADO
  gs_zib_cte_dist_ter-cd_status_dist = '2'.

  lv_domvalue = gs_zib_cte_dist_ter-cd_status_dist.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZDM_CTE_DIST_ST'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_status_dist  = lv_ddtext.
  gs_zib_cte_dist_ter-cd_status_sefaz = gs_xml_sefaz_cte-cteproc-protcte-infprot-cstat.
  gs_zib_cte_dist_ter-numr_cte        = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-nnf.
  gs_zib_cte_dist_ter-numr_serie      = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-serie.
  gs_zib_cte_dist_ter-xmlvers         = gs_xml_sefaz_cte-cteproc-cte-infcte-a_versao.

  PERFORM zf_get_data_utc USING gs_xml_sefaz_cte-cteproc-cte-infcte-ide-dhemi
                       CHANGING gs_zib_cte_dist_ter-dt_emissao.

  PERFORM zf_get_hora_utc USING gs_xml_sefaz_cte-cteproc-cte-infcte-ide-dhemi
                       CHANGING gs_zib_cte_dist_ter-hr_emissao.

  gs_zib_cte_dist_ter-valor_prestacao = gs_xml_sefaz_cte-cteproc-cte-infcte-vprest-vtprest.
  gs_zib_cte_dist_ter-valor_receber   = gs_xml_sefaz_cte-cteproc-cte-infcte-vprest-vrec.

  "   DETERMINAÇÃO ICMS
  IF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-cst IS NOT INITIAL.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-cst.

  ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20 IS NOT INITIAL.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20-cst.


  ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms45 IS NOT INITIAL.

    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms45-cst.

  ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60 IS NOT INITIAL.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60-cst.

  ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90 IS NOT INITIAL.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90-cst.

  ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-cst IS NOT INITIAL.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-vbcoutrauf.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-vicmsoutrauf.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-cst.

  ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmssn-cst IS NOT INITIAL.

    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmssn-cst.

  ENDIF.


  PERFORM zf_get_dados_tomador CHANGING gs_zib_cte_dist_ter-e_tomadora
                                        gs_zib_cte_dist_ter-f_tomadora
                                        gs_zib_cte_dist_ter-cd_tomador.

  CLEAR: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_tomador.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZCTE_TOMA'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_tomador            = lv_ddtext.

*-----------------------------------------------------------------------------------------------------*
* Emitiente
*-----------------------------------------------------------------------------------------------------*

  gs_zib_cte_dist_ter-emit_cnpj             = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj.
  gs_zib_cte_dist_ter-emit_ie               = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-ie.
  gs_zib_cte_dist_ter-emit_rsocial          = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-xnome.
  gs_zib_cte_dist_ter-emit_fantasia         = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-xfant.

  gs_zib_cte_dist_ter-emit_tp_doc           = 1.

*-----------------------------------------------------------------------------------------------------*
* Remetente
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-reme_cnpj             = gs_xml_sefaz_cte-cteproc-cte-infcte-rem-cnpj.
  gs_zib_cte_dist_ter-reme_cpf              = gs_xml_sefaz_cte-cteproc-cte-infcte-rem-cpf.
  gs_zib_cte_dist_ter-reme_ie               = gs_xml_sefaz_cte-cteproc-cte-infcte-rem-ie.
  gs_zib_cte_dist_ter-reme_rsocial          = gs_xml_sefaz_cte-cteproc-cte-infcte-rem-xnome.
  gs_zib_cte_dist_ter-reme_fantasia         = gs_xml_sefaz_cte-cteproc-cte-infcte-rem-xfant.

  IF gs_zib_cte_dist_ter-reme_cnpj IS NOT INITIAL.
    gs_zib_cte_dist_ter-reme_tp_doc           = 1.
  ELSEIF gs_zib_cte_dist_ter-reme_cpf IS NOT INITIAL.
    gs_zib_cte_dist_ter-reme_tp_doc           = 2.
  ENDIF.

*-----------------------------------------------------------------------------------------------------*
* Expedidor
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-exped_cnpj            = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-cnpj.
  gs_zib_cte_dist_ter-exped_cpf             = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-cpf.
  gs_zib_cte_dist_ter-exped_ie              = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-ie.
  gs_zib_cte_dist_ter-exped_rsocial         = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-xnome.
  gs_zib_cte_dist_ter-exped_fantasia        = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-xfant.

  IF gs_zib_cte_dist_ter-exped_cnpj IS NOT INITIAL.
    gs_zib_cte_dist_ter-exped_tp_doc           = 1.
  ELSEIF gs_zib_cte_dist_ter-exped_cpf IS NOT INITIAL.
    gs_zib_cte_dist_ter-exped_tp_doc           = 2.
  ENDIF.

*-----------------------------------------------------------------------------------------------------*
* Recebedor
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-receb_cnpj            = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-cnpj.
  gs_zib_cte_dist_ter-receb_cpf             = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-cpf.
  gs_zib_cte_dist_ter-receb_ie              = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-ie.
  gs_zib_cte_dist_ter-receb_rsocial         = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-xnome.
  gs_zib_cte_dist_ter-receb_fantasia        = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-xfant.

  IF gs_zib_cte_dist_ter-receb_cnpj IS NOT INITIAL.
    gs_zib_cte_dist_ter-receb_tp_doc           = 1.
  ELSEIF gs_zib_cte_dist_ter-receb_cpf IS NOT INITIAL.
    gs_zib_cte_dist_ter-receb_tp_doc           = 2.
  ENDIF.

*-----------------------------------------------------------------------------------------------------*
* Destinatario
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-dest_cnpj             = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-cnpj.
  gs_zib_cte_dist_ter-dest_cpf              = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-cpf.
  gs_zib_cte_dist_ter-dest_ie               = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-ie.
  gs_zib_cte_dist_ter-dest_rsocial          = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-xnome.

  IF gs_zib_cte_dist_ter-dest_cnpj IS NOT INITIAL.
    gs_zib_cte_dist_ter-dest_tp_doc           = 1.
  ELSEIF gs_zib_cte_dist_ter-dest_cpf IS NOT INITIAL.
    gs_zib_cte_dist_ter-dest_tp_doc           = 2.
  ENDIF.

*-----------------------------------------------------------------------------------------------------*
* Outros
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-toma4_cnpj            = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-cnpj.
  gs_zib_cte_dist_ter-toma4_cpf             = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-cpf.
  gs_zib_cte_dist_ter-toma4_ie              = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-ie.
  gs_zib_cte_dist_ter-toma4_rsocial         = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-xnome.
  gs_zib_cte_dist_ter-toma4_fantasia        = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-xfant.

  IF gs_zib_cte_dist_ter-toma4_cnpj IS NOT INITIAL.
    gs_zib_cte_dist_ter-toma4_tp_doc           = 1.
  ELSEIF gs_zib_cte_dist_ter-toma4_cpf IS NOT INITIAL.
    gs_zib_cte_dist_ter-toma4_tp_doc           = 2.
  ENDIF.

  "-------------------------------------------------------------------------------------------------------*

  gs_zib_cte_dist_ter-modelo                = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-mod.
  gs_zib_cte_dist_ter-cd_modal              = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-modal.


  CLEAR: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_modal.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZMODAL'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_modal = lv_ddtext.
  gs_zib_cte_dist_ter-cd_tipo_servico       = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-tpserv.

  CLEAR: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_tipo_servico.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZTPSERV'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_tipo_servico = lv_ddtext.
  gs_zib_cte_dist_ter-cd_tipo_cte           = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-tpcte.

  CLEAR: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_tipo_cte.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZTPSERV'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_tipo_cte = lv_ddtext.

  gs_zib_cte_dist_ter-cd_fpagamento         = 0.
  CLEAR: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_fpagamento.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZFORPAG'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_fpagamento = lv_ddtext.

  gs_zib_cte_dist_ter-cd_femissao           = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-tpemis.

  CLEAR: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_femissao.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = 'ZESZTFOREM'
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  gs_zib_cte_dist_ter-ds_femissao           = lv_ddtext.
  gs_zib_cte_dist_ter-cd_aplicativo         = 0.
  gs_zib_cte_dist_ter-codg_cfop             = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-cfop.
  gs_zib_cte_dist_ter-inicio_ibge           = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-cmunini.
  gs_zib_cte_dist_ter-inicio_muni           = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-xmunini.
  gs_zib_cte_dist_ter-inicio_uf             = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-ufini.
  gs_zib_cte_dist_ter-termino_ibge          = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-cmunfim.
  gs_zib_cte_dist_ter-termino_muni          = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-xmunfim.
  gs_zib_cte_dist_ter-termino_uf            = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-uffim.
  gs_zib_cte_dist_ter-nr_protocolo          = gs_xml_sefaz_cte-cteproc-protcte-infprot-nprot.

  PERFORM zf_get_data_utc USING gs_xml_sefaz_cte-cteproc-protcte-infprot-dhrecbto
                         CHANGING gs_zib_cte_dist_ter-dt_protocolo.

  PERFORM zf_get_hora_utc USING gs_xml_sefaz_cte-cteproc-protcte-infprot-dhrecbto
                               CHANGING gs_zib_cte_dist_ter-hr_protocolo.

  PERFORM zf_atrib_st_sefaz_zib_cte USING p_status
                                          p_cod_sefaz
                                 CHANGING gs_zib_cte_dist_ter.

  gs_zib_cte_dist_ter-regio                  = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte(2).
  gs_zib_cte_dist_ter-nfyear                 = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+2(2).
  gs_zib_cte_dist_ter-nfmonth                = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+4(2).
  gs_zib_cte_dist_ter-docnum9                = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+34(9).
  gs_zib_cte_dist_ter-cdv                    = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+43(1).
  gs_zib_cte_dist_ter-ds_prod_pred           = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-propred.
  gs_zib_cte_dist_ter-vl_total_merc          = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-vcarga.

  IF gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj EQ '09257877000218' OR
     gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj EQ '42276907000209' OR
     gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj EQ '00924429000175' .

    READ TABLE gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-infq INTO DATA(wa_carga) WITH KEY tpmed = 'PESO BASE DE CALCULO'.
    IF sy-subrc IS INITIAL.
      CASE wa_carga-cunid.
        WHEN '01'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga.
        WHEN '02'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga * 1000.
      ENDCASE.
    ENDIF.

  ELSE.

    LOOP AT gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-infq INTO wa_carga WHERE cunid EQ '01' OR cunid EQ '02'.
      CASE wa_carga-cunid.
        WHEN '01'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga.
        WHEN '02'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga * 1000.
      ENDCASE.
      EXIT.
    ENDLOOP.

  ENDIF.

  lv_stcd1 = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj.

  SELECT SINGLE lifnr
    INTO gs_zib_cte_dist_ter-p_emissor
    FROM lfa1
   WHERE stcd1 = lv_stcd1.

  PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj
                                  gs_xml_sefaz_cte-cteproc-cte-infcte-emit-ie
                                  space
                         CHANGING gs_zib_cte_dist_ter-e_emissor
                                  gs_zib_cte_dist_ter-f_emissor.

  gs_zib_cte_dist_ter-dt_atualizacao = sy-datum.
  gs_zib_cte_dist_ter-hr_atualizacao = sy-uzeit.
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE_C
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE_A
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE_S
*  GS_ZIB_CTE_DIST_TER-EBELN
*  GS_ZIB_CTE_DIST_TER-EBELP
*  GS_ZIB_CTE_DIST_TER-MWSKZ
*  GS_ZIB_CTE_DIST_TER-BELNR
*  GS_ZIB_CTE_DIST_TER-GJAHR
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE_SUB
*  GS_ZIB_CTE_DIST_TER-CK_MANUAL
*  GS_ZIB_CTE_DIST_TER-WAERK_VI
*  GS_ZIB_CTE_DIST_TER-KURSK_VI
*  GS_ZIB_CTE_DIST_TER-ZVLR_VI
*  GS_ZIB_CTE_DIST_TER-ZESZVLR_FRETE
*  GS_ZIB_CTE_DIST_TER-ZVLR_MERCADORIA
*  GS_ZIB_CTE_DIST_TER-PESO_ORIGEM
*  GS_ZIB_CTE_DIST_TER-PESO_CHEGADA
*  GS_ZIB_CTE_DIST_TER-DT_CHEGADA
*  GS_ZIB_CTE_DIST_TER-ZESZDT_MOV
*  GS_ZIB_CTE_DIST_TER-ZESZDT_VENCTO
*  GS_ZIB_CTE_DIST_TER-ZESZPESO_DIFERENCA
*  GS_ZIB_CTE_DIST_TER-ZESZQUEBRA
*  GS_ZIB_CTE_DIST_TER-ZESZPERDA
*  GS_ZIB_CTE_DIST_TER-ZESZVLR_QUEBRA
*  GS_ZIB_CTE_DIST_TER-ZESZVLR_PERDA
*  GS_ZIB_CTE_DIST_TER-ZESZVLR_LIQ_PAGAR
*  GS_ZIB_CTE_DIST_TER-ZBVTYP
*  GS_ZIB_CTE_DIST_TER-MATNS
*  GS_ZIB_CTE_DIST_TER-ZBASE_ICMS
*  GS_ZIB_CTE_DIST_TER-ZBASE_PIS
*  GS_ZIB_CTE_DIST_TER-ZBASE_COFINS
*  GS_ZIB_CTE_DIST_TER-ZRATE_ICMS
*  GS_ZIB_CTE_DIST_TER-ZRATE_PIS
*  GS_ZIB_CTE_DIST_TER-ZRATE_COFINS
*  GS_ZIB_CTE_DIST_TER-ZVALOR_ICMS
*  GS_ZIB_CTE_DIST_TER-ZVALOR_PIS
*  GS_ZIB_CTE_DIST_TER-ZVALOR_COFINS
*  GS_ZIB_CTE_DIST_TER-ZVALOR_PEDAGIO
*  GS_ZIB_CTE_DIST_TER-CK_PESO_CHEGADA
*  GS_ZIB_CTE_DIST_TER-TIMESTAMP
*  GS_ZIB_CTE_DIST_TER-CK_AUTORIZADO
*  GS_ZIB_CTE_DIST_TER-ZTERM
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE_P
  MODIFY zeszib_cte_dist_ter FROM gs_zib_cte_dist_ter.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_C57
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_c57 .

* Atualiza Tabela ZESZIB_CTE_DIST_C57
  CHECK gs_xml_sefaz_cte-cteproc-cte-infcte-infctecomp-chcte IS NOT INITIAL.

  gs_zib_cte_dist_c57-cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
  gs_zib_cte_dist_c57-c57_chave_acesso = gs_xml_sefaz_cte-cteproc-cte-infcte-infctecomp-chcte.
  MODIFY zeszib_cte_dist_c57 FROM gs_zib_cte_dist_c57.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_CVL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_cvl .

* Atualiza Tabela ZESZIB_CTE_DIST_C57

  LOOP AT gs_xml_sefaz_cte-cteproc-cte-infcte-vprest-comp INTO DATA(wl_comp).
    gs_zib_cte_dist_cvl-cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_cvl-nome_componente = wl_comp-xnome.
    gs_zib_cte_dist_cvl-valr_componente = wl_comp-vcomp.
    MODIFY zeszib_cte_dist_cvl FROM gs_zib_cte_dist_cvl.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_D55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_d55 .

  DATA: lt_zde_inf_cte_norm TYPE zeszde_inf_cte_norm_inf_nfe_t,
        ls_zde_inf_cte_norm TYPE zeszde_inf_cte_norm_inf_nfe.

*  Atualiza Tabela ZESZIB_CTE_DIST_D55
  lt_zde_inf_cte_norm[] =  gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnfe[].

  LOOP AT lt_zde_inf_cte_norm INTO ls_zde_inf_cte_norm.
    gs_zib_cte_dist_d55-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_d55-n55_chave_acesso = ls_zde_inf_cte_norm-chave.

    CHECK ls_zde_inf_cte_norm-infunidtransp-tpunidtransp IS NOT INITIAL.

    gs_zib_cte_dist_d55-tp_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-tpunidtransp.
    gs_zib_cte_dist_d55-id_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-idunidtransp.
    gs_zib_cte_dist_d55-valr_peso_rate   = ls_zde_inf_cte_norm-infunidtransp-qtdrat.
    MODIFY zeszib_cte_dist_d55 FROM gs_zib_cte_dist_d55.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_dup .

*  Atualiza Tabela ZESZIB_CTE_DIST_D55
  " GS_ZIB_CTE_DIST_D55-CD_CHAVE_CTE = GS_XML_SEFAZ_CTE-CTEPROC-PROTCTE-INFPROT-CHCTE.
*  GS_ZIB_CTE_DIST_D55-NR_DUPLICATA
*  GS_ZIB_CTE_DIST_D55-DT_VENCIMENTO
*  GS_ZIB_CTE_DIST_D55-VL_DUPLICATA
  "  MODIFY ZESZIB_CTE_DIST_D55 FROM GS_ZIB_CTE_DIST_D55.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_MOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_mot .

*  Atualiza Tabela ZESZIB_CTE_DIST_MOT

  CHECK gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-moto-xnome IS NOT INITIAL.

  gs_zib_cte_dist_mot-cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
  gs_zib_cte_dist_mot-moto_cpf     = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-moto-cpf.
  gs_zib_cte_dist_mot-moto_nome    = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-moto-xnome.
  MODIFY zeszib_cte_dist_mot FROM gs_zib_cte_dist_mot  .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_N01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_n01 .


  DATA: lt_zde_inf_cte_norm_inf TYPE zeszde_inf_cte_norm_inf_nff_t,
        ls_zde_inf_cte_norm_inf TYPE zeszde_inf_cte_norm_inf_nff.

*  Atualiza Tabela ZESZIB_CTE_DIST_N01
  lt_zde_inf_cte_norm_inf[] = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnf[].

  LOOP AT lt_zde_inf_cte_norm_inf INTO ls_zde_inf_cte_norm_inf.
    gs_zib_cte_dist_n01-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_n01-n01_modelo_nf    = ls_zde_inf_cte_norm_inf-mod.
    gs_zib_cte_dist_n01-n01_numr_serie   = ls_zde_inf_cte_norm_inf-serie.
    gs_zib_cte_dist_n01-n01_nr_nf        = ls_zde_inf_cte_norm_inf-ndoc.

    PERFORM zf_get_data_utc USING ls_zde_inf_cte_norm_inf-demi
                         CHANGING gs_zib_cte_dist_n01-n01_data_emissao.

    gs_zib_cte_dist_n01-n01_vl_base_icms  = ls_zde_inf_cte_norm_inf-vbc.
    gs_zib_cte_dist_n01-n01_vl_icms       = ls_zde_inf_cte_norm_inf-vicms.
    gs_zib_cte_dist_n01-n01_vl_bicms_st   = ls_zde_inf_cte_norm_inf-vbcst.
    gs_zib_cte_dist_n01-n01_vl_icms_st    = ls_zde_inf_cte_norm_inf-vst.
    gs_zib_cte_dist_n01-n01_vl_produtos   = ls_zde_inf_cte_norm_inf-vprod.
    gs_zib_cte_dist_n01-n01_vl_nota       = ls_zde_inf_cte_norm_inf-vnf.
    gs_zib_cte_dist_n01-n01_codg_cfop     = ls_zde_inf_cte_norm_inf-ncfop.

    MODIFY zeszib_cte_dist_n01 FROM gs_zib_cte_dist_n01.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_n55.

  DATA: lt_zde_inf_cte_norm_inf TYPE zeszde_inf_cte_norm_inf_nfe_t,
        ls_zde_inf_cte_norm_inf TYPE zeszde_inf_cte_norm_inf_nfe.

*  Atualiza Tabela ZESZIB_CTE_DIST_N55
  lt_zde_inf_cte_norm_inf[] = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnfe[].

  SELECT SINGLE text
    INTO gs_zib_cte_dist_n55-n55_desc_sefaz
    FROM j_1bstscodet
   WHERE spras = sy-langu
     AND code = gs_xml_sefaz_cte-cteproc-protcte-infprot-cstat.

  LOOP AT lt_zde_inf_cte_norm_inf INTO ls_zde_inf_cte_norm_inf.
    gs_zib_cte_dist_n55-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_n55-n55_chave_acesso = ls_zde_inf_cte_norm_inf-chave.
    gs_zib_cte_dist_n55-n55_stat_sefaz   = gs_xml_sefaz_cte-cteproc-protcte-infprot-cstat.
    MODIFY zeszib_cte_dist_n55 FROM gs_zib_cte_dist_n55.
  ENDLOOP.

*  GS_ZIB_CTE_DIST_D55-DOCNUM_NFE       =
*  GS_ZIB_CTE_DIST_D55-N55_PIN_SUFRAMA  =
*  GS_ZIB_CTE_DIST_D55-BUKRS
*  GS_ZIB_CTE_DIST_D55-BRANCH
*  GS_ZIB_CTE_DIST_D55-FORM
*  GS_ZIB_CTE_DIST_D55-PARVW
*  GS_ZIB_CTE_DIST_D55-PARID
*  GS_ZIB_CTE_DIST_D55-DIRECT
*  GS_ZIB_CTE_DIST_D55-TKNUM
*  GS_ZIB_CTE_DIST_D55-FKNUM
*  GS_ZIB_CTE_DIST_D55-LBLNI
*  GS_ZIB_CTE_DIST_D55-EBELN
*  GS_ZIB_CTE_DIST_D55-EBELP
*  GS_ZIB_CTE_DIST_D55-BELNR
*  GS_ZIB_CTE_DIST_D55-GJAHR
*  GS_ZIB_CTE_DIST_D55-VBELN_VF
*  GS_ZIB_CTE_DIST_D55-VBELN_VA
*  GS_ZIB_CTE_DIST_D55-AUART_VA
*  GS_ZIB_CTE_DIST_D55-VBELN_VL
*  GS_ZIB_CTE_DIST_D55-VBELN_RE
*  GS_ZIB_CTE_DIST_D55-GJAHR_RE
*  GS_ZIB_CTE_DIST_D55-ZW_LCTO
*  GS_ZIB_CTE_DIST_D55-MBLNR
*  GS_ZIB_CTE_DIST_D55-MJAHR
*  GS_ZIB_CTE_DIST_D55-ZEILE
*  GS_ZIB_CTE_DIST_D55-WAERK_VI
*  GS_ZIB_CTE_DIST_D55-KURSK_VI
*  GS_ZIB_CTE_DIST_D55-ZVLR_VI
*  GS_ZIB_CTE_DIST_D55-ZESZVLR_FRETE
*  GS_ZIB_CTE_DIST_D55-ZVLR_MERCADORIA
*  GS_ZIB_CTE_DIST_D55-ZESZVLR_QUEBRA
*  GS_ZIB_CTE_DIST_D55-ZESZVLR_PERDA
*  GS_ZIB_CTE_DIST_D55-ZESZVLR_LIQ_PAGAR
*  GS_ZIB_CTE_DIST_D55-CK_PESO_DIGITADO
*  GS_ZIB_CTE_DIST_D55-CK_NFE_CTA_ORDEM
*  GS_ZIB_CTE_DIST_D55-CK_INC_MANUAL
*  GS_ZIB_CTE_DIST_D55-PESO_ORIGEM
*  GS_ZIB_CTE_DIST_D55-PESO_CHEGADA
*  GS_ZIB_CTE_DIST_D55-ZESZPESO_DIFERENCA
*  GS_ZIB_CTE_DIST_D55-ZESZQUEBRA
*  GS_ZIB_CTE_DIST_D55-ZESZPERDA
*  GS_ZIB_CTE_DIST_D55-ZVLR_KG_TRANSP
*  GS_ZIB_CTE_DIST_D55-ZVLR_KG_MERCAD

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_VEI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_vei .

  DATA: lt_zesde_inf_modal_rodo TYPE zesde_inf_modal_rodo_t,
        ls_zesde_inf_modal_rodo TYPE zesde_inf_modal_rodo_m.

  lt_zesde_inf_modal_rodo[] = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-veic[].

*  Atualiza Tabela ZESZIB_CTE_DIST_VEI
  LOOP AT lt_zesde_inf_modal_rodo INTO ls_zesde_inf_modal_rodo.
    gs_zib_cte_dist_vei-cd_chave_cte            = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_vei-veic_placa              = ls_zesde_inf_modal_rodo-placa.
    gs_zib_cte_dist_vei-veic_tara_kg            = ls_zesde_inf_modal_rodo-tara.
    gs_zib_cte_dist_vei-veic_capacidade_kg      = ls_zesde_inf_modal_rodo-capkg.
    gs_zib_cte_dist_vei-veic_capacidade_m3      = ls_zesde_inf_modal_rodo-capm3.
    gs_zib_cte_dist_vei-veic_tipo_propriedade   = ls_zesde_inf_modal_rodo-tpprop.
    gs_zib_cte_dist_vei-veic_tipo_veiculo       = ls_zesde_inf_modal_rodo-tpveic.
    gs_zib_cte_dist_vei-veic_tipo_rodado        = ls_zesde_inf_modal_rodo-tprod.
    gs_zib_cte_dist_vei-veic_tipo_carroceria    = ls_zesde_inf_modal_rodo-tpcar.
    gs_zib_cte_dist_vei-veic_uf_licenciamento   = ls_zesde_inf_modal_rodo-uf.
    gs_zib_cte_dist_vei-veic_renavam            = ls_zesde_inf_modal_rodo-renavam.
    gs_zib_cte_dist_vei-veic_rntrc              = ls_zesde_inf_modal_rodo-prop-rntrc.
    gs_zib_cte_dist_vei-prop_tp_doc             = 1.
    gs_zib_cte_dist_vei-prop_cnpj               = ls_zesde_inf_modal_rodo-prop-cnpj.
*    GS_ZIB_CTE_DIST_VEI-PROP_CPF                = ls_ZESDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_IE                 = ls_ZESDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_UF_IE              = ls_ZESDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_RSOCIAL            = ls_ZESDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_TIPO_PROP          = ls_ZESDE_INF_MODAL_RODO-PLACA.
    MODIFY zeszib_cte_dist_vei FROM gs_zib_cte_dist_vei.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_VGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribuir_cte_dist_vga .

*  Atualiza Tabela ZESZIB_CTE_DIST_VGA

  CHECK gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-nvag IS NOT INITIAL.

  gs_zib_cte_dist_vga-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
  gs_zib_cte_dist_vga-numr_ident_vagao = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-nvag.
*  GS_ZIB_CTE_DIST_VGA-INFO_CAPACIDADE
  gs_zib_cte_dist_vga-tipo_vagao       = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-tpvag.
  gs_zib_cte_dist_vga-valr_peso_real   = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-pesor.
  gs_zib_cte_dist_vga-valr_peso_bc     = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-pesobc.
  MODIFY zeszib_cte_dist_vga FROM gs_zib_cte_dist_vga.

ENDFORM.

FORM zf_get_dados_tomador CHANGING c_e_tomadora
                                   c_f_tomadora
                                   c_cd_tomador TYPE c.

  CLEAR: c_cd_tomador, c_e_tomadora,  c_f_tomadora.

  CASE abap_true.
    WHEN gwa_cte_read-cte_57.

      IF gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma3 IS NOT INITIAL.
        c_cd_tomador   = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma3-toma.
      ELSEIF gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4 IS NOT INITIAL.
        c_cd_tomador   = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-toma.
      ENDIF.

      CASE c_cd_tomador.
        WHEN '0'.  "Remetente

          PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte-cteproc-cte-infcte-rem-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-rem-ie
                                          space
                                 CHANGING c_e_tomadora
                                          c_f_tomadora.
        WHEN '1'.  "Expedidor

          PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte-cteproc-cte-infcte-exped-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-exped-ie
                                          space
                                 CHANGING c_e_tomadora
                                          c_f_tomadora.

        WHEN '2'.  "Recebedor

          PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte-cteproc-cte-infcte-receb-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-receb-ie
                                          space
                                 CHANGING c_e_tomadora
                                          c_f_tomadora.

        WHEN '3'.  "Destinatário

          PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte-cteproc-cte-infcte-dest-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-dest-ie
                                          space
                                 CHANGING c_e_tomadora
                                          c_f_tomadora.
        WHEN '4'.  "Outros

          PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-ie
                                          space
                                 CHANGING c_e_tomadora
                                          c_f_tomadora.
      ENDCASE.

    WHEN gwa_cte_read-cte_67.

      PERFORM zf_buscar_empresa USING gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-toma-cnpj
                                      gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-toma-ie
                                      space
                             CHANGING c_e_tomadora
                                      c_f_tomadora.

    WHEN OTHERS.
      EXIT.
  ENDCASE.



ENDFORM.

FORM zf_atribuir_cte_dist_cpl.

  DATA: lt_zde_inf_compl TYPE zeszde_inf_cte_compl_obs_cont_t,
        ls_zde_inf_compl TYPE zeszde_inf_cte_compl_obs_cont.

  DATA: v_contador TYPE zeszib_cte_dist_cpl-cd_contador.

*  Atualiza Tabela ZESZIB_CTE_DIST_D55
  lt_zde_inf_compl[] =  gs_xml_sefaz_cte-cteproc-cte-infcte-compl-obscont[].

  LOOP AT lt_zde_inf_compl INTO ls_zde_inf_compl.

    ADD 1 TO v_contador.

    gs_zib_cte_dist_cpl-cd_chave_cte  = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_cpl-tp_identifica = '1'.
    gs_zib_cte_dist_cpl-ds_campo      = ls_zde_inf_compl-a_xcampo.
    gs_zib_cte_dist_cpl-cd_contador   = v_contador.
    gs_zib_cte_dist_cpl-ds_texto      = ls_zde_inf_compl-xtexto.

    MODIFY zeszib_cte_dist_cpl FROM gs_zib_cte_dist_cpl.
  ENDLOOP.


ENDFORM.

FORM zf_atribuir_cte_dist_001.

  DATA vg_container(14) TYPE c.



  LOOP AT gs_xml_sefaz_cte-cteproc-cte-infcte-compl-obscont INTO DATA(wl_inf_compl).

    CLEAR: gs_zib_cte_dist_001.

    TRY.

        gs_zib_cte_dist_001-cd_chave_cte  = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.

        CLEAR vg_container.

        vg_container = wl_inf_compl-a_xcampo.

        "CASE wl_inf_compl-a_xcampo+0(14).
        CASE vg_container.
          WHEN 'CONTAINER_NOTA'.

            SPLIT wl_inf_compl-xtexto AT ';' INTO: DATA(lc_nm_container) DATA(lc_nfe) DATA(lc_quantidade).

            gs_zib_cte_dist_001-nm_container     = lc_nm_container.
            gs_zib_cte_dist_001-n55_chave_acesso = lc_nfe.

            REPLACE ALL OCCURRENCES OF ',' IN lc_quantidade WITH '.'.
            gs_zib_cte_dist_001-nm_quantidade  = lc_quantidade.

          WHEN 'CONTAINER'.

            SPLIT wl_inf_compl-xtexto AT ';' INTO: lc_nm_container lc_quantidade.

            gs_zib_cte_dist_001-nm_container     = lc_nm_container.

            REPLACE ALL OCCURRENCES OF ',' IN lc_quantidade WITH '.'.
            gs_zib_cte_dist_001-nm_quantidade  = lc_quantidade.

          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        MODIFY zeszib_cte_dist_001 FROM gs_zib_cte_dist_001.

      CATCH cx_sy_conversion_no_number.

    ENDTRY.

  ENDLOOP.

ENDFORM.


FORM zf_atribuir_cte_dist_d01 .

  DATA: lt_zde_inf_cte_norm TYPE zeszde_inf_cte_norm_inf_nff_t,
        ls_zde_inf_cte_norm TYPE zeszde_inf_cte_norm_inf_nff.

*  Atualiza Tabela ZESZIB_CTE_DIST_D01
  lt_zde_inf_cte_norm[] =  gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnf[].

  LOOP AT lt_zde_inf_cte_norm INTO ls_zde_inf_cte_norm.
    gs_zib_cte_dist_d01-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.

    gs_zib_cte_dist_d01-n01_modelo_nf    = ls_zde_inf_cte_norm-mod.
    gs_zib_cte_dist_d01-n01_numr_serie   = ls_zde_inf_cte_norm-serie.
    gs_zib_cte_dist_d01-n01_nr_nf        = ls_zde_inf_cte_norm-ndoc.

    CHECK ls_zde_inf_cte_norm-infunidtransp-tpunidtransp IS NOT INITIAL.

    gs_zib_cte_dist_d01-tp_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-tpunidtransp.
    gs_zib_cte_dist_d01-id_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-idunidtransp.
    gs_zib_cte_dist_d01-valr_peso_rate   = ls_zde_inf_cte_norm-infunidtransp-qtdrat.

    MODIFY zeszib_cte_dist_d01 FROM gs_zib_cte_dist_d01.
  ENDLOOP.

ENDFORM.

FORM zf_atrib_st_sefaz_zib_cte USING p_status
                                     p_cod_sefaz
                            CHANGING c_zib_cte_dist_ter TYPE zeszib_cte_dist_ter.

  c_zib_cte_dist_ter-cd_status_sefaz = p_cod_sefaz.

  CASE p_status.
    WHEN '1'.
      c_zib_cte_dist_ter-docsta          = '1'.
      c_zib_cte_dist_ter-cancel          = ' '.
    WHEN '2'.
      c_zib_cte_dist_ter-docsta          = '1'.
      c_zib_cte_dist_ter-cancel          = 'X'.
  ENDCASE.

ENDFORM.


FORM zf_atrib_st_sefaz_zib_nfe USING p_status
                                     p_cod_sefaz
                            CHANGING c_zib_nfe_dist_ter TYPE zeszib_nfe_dist_ter.

  c_zib_nfe_dist_ter-cd_msg_sefaz = p_cod_sefaz.

  CASE p_status.
    WHEN '1'.
      c_zib_nfe_dist_ter-docsta = '1'.
      c_zib_nfe_dist_ter-cancel = ' '.
    WHEN '2'.
      c_zib_nfe_dist_ter-docsta = '1'.
      c_zib_nfe_dist_ter-cancel = 'X'.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_5128  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5128 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  "PERFORM caixa_txt_obs. "Comentado 03/05/2023 para publicar versão PRD
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CAIXA_TXT_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM caixa_txt_obs .

  IF NOT c_editor IS INITIAL AND NOT editor IS INITIAL.
    CALL METHOD c_editor->free( ).
    CALL METHOD editor->free( ).
  ENDIF.

  CREATE OBJECT: c_editor EXPORTING container_name = 'C_EDITOR', "CONTAINER
                   editor EXPORTING parent         = c_editor.

  CALL METHOD editor->set_toolbar_mode( toolbar_mode = editor->false ).
  CALL METHOD editor->set_statusbar_mode( statusbar_mode = editor->false ).

  CASE txtopen.
    WHEN 'X'.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->false ).
    WHEN OTHERS.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->true ).
  ENDCASE.

  CALL METHOD editor->set_text_as_stream
    EXPORTING
      text = it_editor.


  FREE: txtopen, it_editor.

ENDFORM.

*** US #173081 - MMSILVA - 03.07.2025 - Ini ***
FORM zf_atribuir_nfe_dist_avb.
  DATA: ls_infevento    TYPE zeszintg_evento,
        ls_retinfevento TYPE zinfevento.

  ls_infevento                          = gs_xml_averb-proceventonfe-evento-infevento.
  ls_retinfevento                       = gs_xml_averb-proceventonfe-retevento-infevento.

  gs_zib_nfe_dist_avb-orgao_recep       = ls_infevento-corgao.
  gs_zib_nfe_dist_avb-tp_ambiente       = ls_infevento-tpamb.
  gs_zib_nfe_dist_avb-cnpj              = ls_infevento-cnpj.
  gs_zib_nfe_dist_avb-chave_nfe         = ls_infevento-chnfe.

  DATA(data_evento_avb)                 = ls_infevento-dhevento(12).
  REPLACE ALL OCCURRENCES OF '-' IN data_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-dt_evento         = data_evento_avb.

  DATA(hora_evento_avb)                 = ls_infevento-dhevento+11(8).
  REPLACE ALL OCCURRENCES OF ':' IN hora_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-hr_evento         = hora_evento_avb.

  gs_zib_nfe_dist_avb-tp_evento         = ls_infevento-tpevento.
  gs_zib_nfe_dist_avb-seq_evento        = ls_infevento-nseqevento.
  gs_zib_nfe_dist_avb-versao_evento     = ls_infevento-verevento.
  gs_zib_nfe_dist_avb-det_evento_versao = lv_versao.

  DATA(data_reg_evento_avb)             = ls_retinfevento-dhregevento(10).
  REPLACE ALL OCCURRENCES OF '-' IN data_reg_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-dt_reg_evento     = data_reg_evento_avb.

  DATA(hora_reg_evento_avb)             = ls_retinfevento-dhregevento+11(8).
  REPLACE ALL OCCURRENCES OF ':' IN hora_reg_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-hr_reg_evento     = hora_reg_evento_avb.

  gs_zib_nfe_dist_avb-n_prot            = ls_retinfevento-nprot.

  MODIFY zib_nfe_dist_avb FROM gs_zib_nfe_dist_avb.
ENDFORM.

FORM zf_atribuir_nfe_dist_avi.
  DATA: ls_infevento    TYPE zeszintg_evento.

  ls_infevento                              = gs_xml_averb-proceventonfe-evento-infevento.

  LOOP AT gs_xml_averb-proceventonfe-evento-infevento-detevento-itensaverbados INTO DATA(ls_itens_averbados).
    gs_zib_nfe_dist_avi-chave_nfe             = ls_infevento-chnfe.
    gs_zib_nfe_dist_avi-detalhe_evento_versao = lv_versao.
    gs_zib_nfe_dist_avi-desc_evento           = ls_infevento-detevento-descevento.
    gs_zib_nfe_dist_avi-tp_autor              = ls_infevento-detevento-tpautor.
    gs_zib_nfe_dist_avi-ver_aplic             = ls_infevento-detevento-veraplic.

    DATA(data_embarque)                       = ls_itens_averbados-dhembarque(10).
    REPLACE ALL OCCURRENCES OF '-' IN data_embarque WITH ''.
    gs_zib_nfe_dist_avi-dt_embarque           = data_embarque.

    DATA(hora_embarque)                       = ls_itens_averbados-dhembarque+11(8).
    REPLACE ALL OCCURRENCES OF ':' IN hora_embarque WITH ''.
    gs_zib_nfe_dist_avi-hr_embarque           = hora_embarque.

    DATA(data_averb)                          = ls_itens_averbados-dhaverbacao(10).
    REPLACE ALL OCCURRENCES OF '-' IN data_averb WITH ''.
    gs_zib_nfe_dist_avi-dt_averbacao          = data_averb.

    DATA(hora_averb)                          = ls_itens_averbados-dhaverbacao+11(8).
    REPLACE ALL OCCURRENCES OF ':' IN hora_averb WITH ''.
    gs_zib_nfe_dist_avi-hr_averbacao          = hora_averb.

    gs_zib_nfe_dist_avi-n_due                 = ls_itens_averbados-ndue.
    gs_zib_nfe_dist_avi-n_item                = ls_itens_averbados-nitem.
    gs_zib_nfe_dist_avi-n_item_due            = ls_itens_averbados-nitemdue.
    gs_zib_nfe_dist_avi-qitem                 = ls_itens_averbados-qitem.
    gs_zib_nfe_dist_avi-mot_alteracao         = ls_itens_averbados-motalteracao.
    gs_zib_nfe_dist_avi-seq_evento            = ls_infevento-nseqevento. "SMC - 173081 18-09-2025

    MODIFY zib_nfe_dist_avi FROM gs_zib_nfe_dist_avi.
  ENDLOOP.
ENDFORM.
*** US #173081 - MMSILVA - 03.07.2025 - Fim ***