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

form convert_utc_to_time using p_tzone type ad_tzone        "2050660
                      changing p_time_to_convert.           "2050660
                                                            "2050660
  data lv_stamp type timestamp.                             "2050660
  data dat type sy-datum.                                   "2050660
  data tim type sy-uzeit.                                   "2050660
                                                            "2050660
  get time stamp field lv_stamp.                            "2050660
                                                            "2050660
  convert time stamp lv_stamp time zone p_tzone             "2050660
     into date dat                                          "2050660
     time tim.                                              "2050660
                                                            "2050660
  p_time_to_convert = tim.                                  "2050660
                                                            "2050660
endform.                                                    "2050660

form convert_timespan_to_utc using p_time_zone type ad_tzone
                          changing p_timespan_to_convert.

  data time_stamp type timestamp.
  data date       type sy-datum.
  data time       type sy-uzeit.
  data tzon       type ttzz-tzone.

  date = p_timespan_to_convert(8).
  time = p_timespan_to_convert+8(6).
  tzon = p_time_zone.

  convert date date
          time time
          into time stamp time_stamp
          time zone tzon.

  p_timespan_to_convert = time_stamp.

endform.                    " CONVERT_TIMESPAN_TO_UTC
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_FORN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_nfe_forn using p_status
                                p_code_sefaz.

*  Atualiza Tabela zib_nfe_forn

  if gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj is not initial.
    gs_zib_nfe_forn-nu_chave_cnpj   = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj.
  else.
    gs_zib_nfe_forn-nu_chave_cnpj   = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cpf.
  endif.

  gs_zib_nfe_forn-nu_chave_modelo = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-mod.
  gs_zib_nfe_forn-nu_chave_serie  = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-serie.
  gs_zib_nfe_forn-nu_chave_numero = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.
  gs_zib_nfe_forn-st_nota         = p_status.


  perform zf_get_data_utc using gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                       changing gs_zib_nfe_forn-dt_emissao.

  gs_zib_nfe_forn-nu_protocolo = gs_xml_sefaz-nfeproc-protnfe-infprot-nprot.

  perform zf_get_data_utc using gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       changing gs_zib_nfe_forn-dt_protocolo.

  perform zf_get_hora_utc using gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       changing gs_zib_nfe_forn-hr_protocolo.

  gs_zib_nfe_forn-nu_chave_regiao  = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe(2).
  gs_zib_nfe_forn-nu_chave_ano     = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+2(2).
  gs_zib_nfe_forn-nu_chave_mes     = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+4(2).
  gs_zib_nfe_forn-nu_chave_aleator = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+34(9).
  gs_zib_nfe_forn-nu_chave_dv      = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe+43(1).
  gs_zib_nfe_forn-nu_chave         = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
  gs_zib_nfe_forn-nu_ie            = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie.
  gs_zib_nfe_forn-nu_code          = p_code_sefaz.

  perform zf_buscar_empresa using gs_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-ie
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-indiedest
                         changing gs_zib_nfe_forn-bukrs
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
  modify zib_nfe_forn from gs_zib_nfe_forn.

endform.

form zf_atribuir_nfe_forn_cte using p_status
                                    p_code_sefaz.

  data: v_cd_tomador type c.

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
  perform zf_get_dados_tomador changing gs_zib_nfe_forn-bukrs
                                        gs_zib_nfe_forn-branch
                                        v_cd_tomador.

  modify zib_nfe_forn from gs_zib_nfe_forn.

endform.


*&---------------------------------------------------------------------*
*&      Form  ZF_GET_DATA_UTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_get_data_utc using p_data_hora
                  changing c_data.

  check p_data_hora is not initial.

  check strlen( p_data_hora ) ge 10.

  c_data = p_data_hora(4) &&
           p_data_hora+05(02) &&
           p_data_hora+08(02).

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_GET_HORA_UTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_get_hora_utc using p_data_hora
                  changing c_hora.

  check p_data_hora is not initial.

  check strlen( p_data_hora ) ge 19.

  c_hora  = p_data_hora+11(02) &&
            p_data_hora+14(02) &&
            p_data_hora+17(02).

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_buscar_empresa using p_cnpj
                             p_ie
                             p_ind_ie
                    changing p_bukrs
                             p_branch.

  clear: p_bukrs, p_branch.

  data(_force_ck_ie) = abap_false.

  if ( p_ind_ie eq '2' ) and ( p_ie is initial ). "Contribuinte isento de Inscrição no cadastro de Contribuintes do ICMS
    p_ie         = 'ISENTO'.
    _force_ck_ie = abap_true.
  endif.

  if ( p_ind_ie eq '9' ). "Não Contribuinte, que pode ou não possuir Inscrição Estadual no Cadastro de Contribuintes do ICMS.
    _force_ck_ie = abap_true.
  endif.

  check ( p_cnpj is not initial ) and ( p_ie is not initial or p_ind_ie eq '9' ).

  try .

      zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro_cnpj_cpf_ie(
        exporting
          i_cnpj             = conv #( p_cnpj )
          i_insc_estatual    = conv #( p_ie )
          i_ck_ie            = conv #( _force_ck_ie )
        )->ck_parceiro_local_negocio(
         importing
           e_j_1bbranch  = data(e_j_1bbranch)
        ).

      p_bukrs  = e_j_1bbranch-bukrs.
      p_branch = e_j_1bbranch-branch.

    catch zcx_parceiros.    "

  endtry.

*  CLEAR: P_BUKRS, P_BRANCH.
*
*  SELECT SINGLE BUKRS BRANCH
*    INTO (P_BUKRS, P_BRANCH)
*    FROM J_1BBRANCH
*   WHERE STCD1 = P_CNPJ.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_DIST_TER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_nfe_dist_ter using p_status
                                    p_cd_st_sefaz.

  data: lt_zintg_pag type ZESZINTG_PAG,
        ls_zintg_pag type ZESZDETPAG.

  data: ls_zintg_t_dup type ZESZINTG_DUP.

* Atualiza Tabela ZESZIB_NFE_DIST_TER
  gs_zib_nfe_dist_ter-chave_nfe = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
  gs_zib_nfe_dist_ter-forne_cnpj = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj.
  gs_zib_nfe_dist_ter-forne_cpf = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cpf.
  gs_zib_nfe_dist_ter-forne_ie = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie.
  gs_zib_nfe_dist_ter-forne_razao = gs_xml_sefaz-nfeproc-nfe-infnfe-emit-xnome.
  gs_zib_nfe_dist_ter-destino_cnpj = gs_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj.
  gs_zib_nfe_dist_ter-destino_ie = gs_xml_sefaz-nfeproc-nfe-infnfe-dest-ie.
  gs_zib_nfe_dist_ter-numero = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.

  perform zf_get_data_utc using gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                       changing gs_zib_nfe_dist_ter-dt_emissao.

  perform zf_get_hora_utc using gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                       changing gs_zib_nfe_dist_ter-hr_emissao.

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

  read table lt_zintg_pag into ls_zintg_pag index 1.

  gs_zib_nfe_dist_ter-cd_form_pag = ls_zintg_pag-indpag.

  gs_zib_nfe_dist_ter-cd_tipo_doc = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-tpnf.
  gs_zib_nfe_dist_ter-cd_form_emissao = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-tpemis.

  perform zf_get_data_utc using gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient
                       changing gs_zib_nfe_dist_ter-dt_saida.

  perform zf_get_hora_utc using gs_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient
                       changing gs_zib_nfe_dist_ter-hr_saida.

  gs_zib_nfe_dist_ter-cd_fina_emissao = gs_xml_sefaz-nfeproc-nfe-infnfe-ide-finnfe.
  gs_zib_nfe_dist_ter-nr_protocolo = gs_xml_sefaz-nfeproc-protnfe-infprot-nprot.

  perform zf_get_data_utc using gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       changing gs_zib_nfe_dist_ter-dt_protocolo.

  perform zf_get_hora_utc using gs_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                       changing gs_zib_nfe_dist_ter-hr_protocolo.

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

  perform zf_buscar_empresa using gs_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-ie
                                  gs_xml_sefaz-nfeproc-nfe-infnfe-dest-indiedest
                         changing gs_zib_nfe_dist_ter-bukrs
                                  gs_zib_nfe_dist_ter-branch.

  "=========================================================================================Ajuste BUG 48012 / AOENNING
  if gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj is not initial.
    if gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie = 'ISENTO' or
       gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie is initial.

      data(xndiedest) = '2'.
    else.
      xndiedest = '1'.
    endif.

    perform zf_buscar_empresa using gs_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj
                                    gs_xml_sefaz-nfeproc-nfe-infnfe-emit-ie xndiedest
                            changing gs_zib_nfe_dist_ter-bukrs_e gs_zib_nfe_dist_ter-branch_e.
  endif.
  "========================================================================================

  perform zf_atrib_st_sefaz_zib_nfe using p_status
                                          p_cd_st_sefaz
                                 changing gs_zib_nfe_dist_ter.

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
  read table gs_xml_sefaz-nfeproc-nfe-infnfe-cobr-dup into ls_zintg_t_dup index 1.
  if sy-subrc = 0 and ls_zintg_t_dup-dvenc is not initial.
    concatenate ls_zintg_t_dup-dvenc(04) ls_zintg_t_dup-dvenc+5(02) ls_zintg_t_dup-dvenc+8(02) into gs_zib_nfe_dist_ter-dt_vencimento.
*    gs_zib_nfe_dist_ter-dt_vencimento = ls_zintg_t_dup-dvenc.
  else.
    gs_zib_nfe_dist_ter-dt_vencimento = ''.
  endif.

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
  modify ZESZIB_NFE_DIST_TER from gs_zib_nfe_dist_ter.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_NFE_DIST_ITM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_nfe_dist_itm .

  field-symbols: <ZESZINTG_DET> type ZESZDET_NITEM.

  data: lva_vlr_unit_com  type p,
        lva_vlr_unit_trib type p.

  data: gt_zintg_det type ZESZINTG_DET.

  gt_zintg_det[] = gs_xml_sefaz-nfeproc-nfe-infnfe-det[].

  delete from zib_nfe_dist_itm where chave_nfe eq gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.

* Atualiza Tabela ZIB_NFE_DIST_ITM
  loop at gt_zintg_det assigning <ZESZINTG_DET>.

    clear: gs_zib_nfe_dist_itm.

    lva_vlr_unit_com  = <ZESZINTG_DET>-prod-vuncom.
    lva_vlr_unit_trib = <ZESZINTG_DET>-prod-vuntrib.

    gs_zib_nfe_dist_itm-chave_nfe = gs_xml_sefaz-nfeproc-protnfe-infprot-chnfe.
    gs_zib_nfe_dist_itm-prod_item = <ZESZINTG_DET>-a_nitem.
    gs_zib_nfe_dist_itm-prod_codigo = <ZESZINTG_DET>-prod-cprod.
    gs_zib_nfe_dist_itm-prod_cfop = <ZESZINTG_DET>-prod-cfop.
    gs_zib_nfe_dist_itm-prod_descricao = <ZESZINTG_DET>-prod-xprod.
    gs_zib_nfe_dist_itm-prod_ean = <ZESZINTG_DET>-prod-cean.
    gs_zib_nfe_dist_itm-prod_ncm = <ZESZINTG_DET>-prod-ncm.
    gs_zib_nfe_dist_itm-prod_extipi = <ZESZINTG_DET>-prod-extipi.
*  gs_zib_nfe_dist_itm-PROD_NCM_GENERO
    gs_zib_nfe_dist_itm-prod_und_comerci = <ZESZINTG_DET>-prod-ucom.
    gs_zib_nfe_dist_itm-prod_qtd_comerci = <ZESZINTG_DET>-prod-qcom.

    if lva_vlr_unit_com <= 9999999999.
      gs_zib_nfe_dist_itm-prod_vlr_und_com = <ZESZINTG_DET>-prod-vuncom.
    endif.

    gs_zib_nfe_dist_itm-prod_vlr_total_b = <ZESZINTG_DET>-prod-vprod.
    gs_zib_nfe_dist_itm-prod_ean_trib = <ZESZINTG_DET>-prod-ceantrib .
    gs_zib_nfe_dist_itm-prod_und_trib = <ZESZINTG_DET>-prod-utrib.
    gs_zib_nfe_dist_itm-prod_qtd_trib = <ZESZINTG_DET>-prod-qtrib.

    if lva_vlr_unit_trib <= 9999999999.
      gs_zib_nfe_dist_itm-prod_vlr_und_tri = <ZESZINTG_DET>-prod-vuntrib.
    endif.

    gs_zib_nfe_dist_itm-prod_vl_frete = <ZESZINTG_DET>-prod-vfrete.
    gs_zib_nfe_dist_itm-prod_vl_seguro = <ZESZINTG_DET>-prod-vseg.
    gs_zib_nfe_dist_itm-prod_vl_desconto = <ZESZINTG_DET>-prod-vdesc.
    gs_zib_nfe_dist_itm-prod_vl_outro = <ZESZINTG_DET>-prod-voutro.
    gs_zib_nfe_dist_itm-prod_ind_total = <ZESZINTG_DET>-prod-indtot.
    gs_zib_nfe_dist_itm-prod_pedido_comp = <ZESZINTG_DET>-prod-xped.
*  gs_zib_nfe_dist_itm-PROD_NR_PED_COMP


    "GS_ZIB_NFE_DIST_ITM-ICMS_CST = <ZESZINTG_DET>-IMPOSTO-ICMS-CST.

    if <ZESZINTG_DET>-imposto-icms-icms00-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms00-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms00-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms10-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms10-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms10-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms20-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms20-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms20-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms30-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms30-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms30-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms40-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms40-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms40-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms51-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms51-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms51-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms60-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms60-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms60-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms70-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms70-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms70-orig.
    elseif <ZESZINTG_DET>-imposto-icms-icms90-cst is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icms90-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icms90-orig.
*** US #181947 - MMSILVA - 18.06.2025 - Ini ***
    elseif <ZESZINTG_DET>-imposto-icms-icmssn202 is not initial.
      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icmssn202-csosn+1(2).
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icmssn202-orig.
*** US #181947 - MMSILVA - 18.06.2025 - Fim ***
    endif.


*   DETERMINAÇÃO ICMS

*ICMS ST - NOTAS FISCAIS DE VEÍCULO - BG #155190 - INICIO
    if <ZESZINTG_DET>-imposto-icms-icmspart-cst is not initial.

      gs_zib_nfe_dist_itm-icms_cst        = <ZESZINTG_DET>-imposto-icms-icmspart-cst.
      gs_zib_nfe_dist_itm-icms_origem_mec = <ZESZINTG_DET>-imposto-icms-icmspart-orig.
      gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icmspart-modbc.
      gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icmspart-vbc.
      gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icmspart-picms.
      gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icmspart-predbc.
      gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icmspart-vicms.
      gs_zib_nfe_dist_itm-icms_st_md_base = <ZESZINTG_DET>-imposto-icms-icmspart-modbcst.
      gs_zib_nfe_dist_itm-icms_st_base    = <ZESZINTG_DET>-imposto-icms-icmspart-vbcst.
      gs_zib_nfe_dist_itm-icms_st_aqt     = <ZESZINTG_DET>-imposto-icms-icmspart-picmsst.
      gs_zib_nfe_dist_itm-icms_st_valor   = <ZESZINTG_DET>-imposto-icms-icmspart-vicmsst.
      gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icmspart-motdesicms.
      gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icmspart-vicmsdeson.
      gs_zib_nfe_dist_itm-icms_pbcop = <ZESZINTG_DET>-imposto-icms-icmspart-pbcop.
      gs_zib_nfe_dist_itm-ICMS_ufst = <ZESZINTG_DET>-imposto-icms-icmspart-ufst.

    else.

      case gs_zib_nfe_dist_itm-icms_cst.
        when '00'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms00-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms00-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms00-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms00-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms00-vicms.

          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms00-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms00-vicmsdeson.

        when '10'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms10-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms10-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms10-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms10-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms10-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <ZESZINTG_DET>-imposto-icms-icms10-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <ZESZINTG_DET>-imposto-icms-icms10-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <ZESZINTG_DET>-imposto-icms-icms10-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <ZESZINTG_DET>-imposto-icms-icms10-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms10-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms10-vicmsdeson.

        when '20'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms20-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms20-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms20-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms20-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms20-vicms.

          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms20-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms20-vicmsdeson.

        when '30'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms30-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms30-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms30-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms30-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms30-vicms.


          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <ZESZINTG_DET>-imposto-icms-icms30-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <ZESZINTG_DET>-imposto-icms-icms30-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <ZESZINTG_DET>-imposto-icms-icms30-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <ZESZINTG_DET>-imposto-icms-icms30-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms30-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms30-vicmsdeson.


        when '40' or '41' or '50'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms40-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms40-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms40-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms40-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms40-vicms.
*  gs_zib_nfe_dist_itm-ICMS_ST_MD_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_MARGEM
*  gs_zib_nfe_dist_itm-ICMS_ST_RED_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_AQT
*  gs_zib_nfe_dist_itm-ICMS_ST_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_VALOR
          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms40-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms40-vicmsdeson.

        when '51'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms51-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms51-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms51-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms51-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms51-vicms.
*  gs_zib_nfe_dist_itm-ICMS_ST_MD_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_MARGEM
*  gs_zib_nfe_dist_itm-ICMS_ST_RED_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_AQT
*  gs_zib_nfe_dist_itm-ICMS_ST_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_VALOR
          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms51-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms51-vicmsdeson.

        when '60'.
*        gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms160-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms60-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms60-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms60-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms60-vicms.
*  gs_zib_nfe_dist_itm-ICMS_ST_MD_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_MARGEM
*  gs_zib_nfe_dist_itm-ICMS_ST_RED_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_AQT
*  gs_zib_nfe_dist_itm-ICMS_ST_BASE
*  gs_zib_nfe_dist_itm-ICMS_ST_VALOR
          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms60-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms60-vicmsdeson.

        when '70'.
*        gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms170-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms70-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms70-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms70-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms70-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <ZESZINTG_DET>-imposto-icms-icms70-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <ZESZINTG_DET>-imposto-icms-icms70-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <ZESZINTG_DET>-imposto-icms-icms70-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <ZESZINTG_DET>-imposto-icms-icms70-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms70-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms70-vicmsdeson.

        when '90'.
          gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icms90-modbc.
          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icms90-vbc.
          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icms90-picms.
          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icms90-predbc.
          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icms90-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <ZESZINTG_DET>-imposto-icms-icms90-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <ZESZINTG_DET>-imposto-icms-icms90-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <ZESZINTG_DET>-imposto-icms-icms90-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <ZESZINTG_DET>-imposto-icms-icms90-vicmsst.
          "Campos ST - WPP


          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icms90-motdesicms.
          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icms90-vicmsdeson.

*** US #181947 - MMSILVA - 18.06.2025 - Ini ***
        when '02'.
*         gs_zib_nfe_dist_itm-icms_md_base = <ZESZINTG_DET>-imposto-icms-icmssn202-modbc.
*          gs_zib_nfe_dist_itm-icms_base = <ZESZINTG_DET>-imposto-icms-icmssn202--vbc.
*          gs_zib_nfe_dist_itm-icms_aqt = <ZESZINTG_DET>-imposto-icms-icmssn202-picms.
*          gs_zib_nfe_dist_itm-icms_red_base = <ZESZINTG_DET>-imposto-icms-icmssn202-predbc.
*          gs_zib_nfe_dist_itm-icms_valor = <ZESZINTG_DET>-imposto-icms-icmssn202-vicms.

          "Campos ST - WPP
          gs_zib_nfe_dist_itm-icms_st_md_base = <ZESZINTG_DET>-imposto-icms-icmssn202-modbcst.

          gs_zib_nfe_dist_itm-icms_st_base    = <ZESZINTG_DET>-imposto-icms-icmssn202-vbcst.
          gs_zib_nfe_dist_itm-icms_st_aqt     = <ZESZINTG_DET>-imposto-icms-icmssn202-picmsst.
          gs_zib_nfe_dist_itm-icms_st_valor   = <ZESZINTG_DET>-imposto-icms-icmssn202-vicmsst.
          "Campos ST - WPP


*          gs_zib_nfe_dist_itm-icms_mt_desonera = <ZESZINTG_DET>-imposto-icms-icmssn202-motdesicms.
*          gs_zib_nfe_dist_itm-icms_vl_desonerado = <ZESZINTG_DET>-imposto-icms-icmssn202-vicmsdeson.
*** US #181947 - MMSILVA - 18.06.2025 - Fim ***

      endcase.

    endif.
*ICMS ST - NOTAS FISCAIS DE VEÍCULO - BG #155190 - FIM

*   DETERMINAÇÃO IPI
    gs_zib_nfe_dist_itm-ipi_cod_enquadra = <ZESZINTG_DET>-imposto-ipi-cenq.
    gs_zib_nfe_dist_itm-ipi_cla_enquadra = <ZESZINTG_DET>-imposto-ipi-clenq.
    gs_zib_nfe_dist_itm-ipi_cnpj_prod = <ZESZINTG_DET>-imposto-ipi-cnpjprod.
    gs_zib_nfe_dist_itm-ipi_cod_selo_con = <ZESZINTG_DET>-imposto-ipi-cselo.
    gs_zib_nfe_dist_itm-ipi_qtd_selo_con = <ZESZINTG_DET>-imposto-ipi-qselo.


    if <ZESZINTG_DET>-imposto-ipi-ipint is not initial.
      gs_zib_nfe_dist_itm-ipi_cst = <ZESZINTG_DET>-imposto-ipi-ipint-cst.
    else.
      gs_zib_nfe_dist_itm-ipi_cst = <ZESZINTG_DET>-imposto-ipi-ipitrib-cst.
    endif.

    case gs_zib_nfe_dist_itm-ipi_cst .
      when '00' or '49' or '50' or '99'.
        gs_zib_nfe_dist_itm-ipi_cst = <ZESZINTG_DET>-imposto-ipi-ipitrib-cst.
        gs_zib_nfe_dist_itm-ipi_base = <ZESZINTG_DET>-imposto-ipi-ipitrib-vbc.
        gs_zib_nfe_dist_itm-ipi_aqt = <ZESZINTG_DET>-imposto-ipi-ipitrib-pipi.
        gs_zib_nfe_dist_itm-ipi_valor = <ZESZINTG_DET>-imposto-ipi-ipitrib-vipi.
      when '01' or '02' or '03' or '04' or '51' or '52' or '53' or '54' or '55'.
        gs_zib_nfe_dist_itm-ipi_cst = <ZESZINTG_DET>-imposto-ipi-ipint-cst.
    endcase.

*   DETERMINAÇÃO PIS
    gs_zib_nfe_dist_itm-pis_cst = <ZESZINTG_DET>-imposto-pis-cst.

    if <ZESZINTG_DET>-imposto-pis-pisaliq is not initial.
      gs_zib_nfe_dist_itm-pis_base = <ZESZINTG_DET>-imposto-pis-pisaliq-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <ZESZINTG_DET>-imposto-pis-pisaliq-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <ZESZINTG_DET>-imposto-pis-pisaliq-vpis.

      if gs_zib_nfe_dist_itm-pis_cst is initial.
        gs_zib_nfe_dist_itm-pis_cst = <ZESZINTG_DET>-imposto-pis-pisaliq-cst.
      endif.

    elseif <ZESZINTG_DET>-imposto-pis-pisqtde is not initial.
      gs_zib_nfe_dist_itm-pis_base = <ZESZINTG_DET>-imposto-pis-pisqtde-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <ZESZINTG_DET>-imposto-pis-pisqtde-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <ZESZINTG_DET>-imposto-pis-pisqtde-vpis.

      if gs_zib_nfe_dist_itm-pis_cst is initial.
        gs_zib_nfe_dist_itm-pis_cst = <ZESZINTG_DET>-imposto-pis-pisqtde-cst.
      endif.

    elseif <ZESZINTG_DET>-imposto-pis-pisnt is not initial.

      if gs_zib_nfe_dist_itm-pis_cst is initial.
        gs_zib_nfe_dist_itm-pis_cst = <ZESZINTG_DET>-imposto-pis-pisnt-cst.
      endif.

    elseif <ZESZINTG_DET>-imposto-pis-pisoutr is not initial.
      gs_zib_nfe_dist_itm-pis_base = <ZESZINTG_DET>-imposto-pis-pisoutr-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <ZESZINTG_DET>-imposto-pis-pisoutr-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <ZESZINTG_DET>-imposto-pis-pisoutr-vpis.

      if gs_zib_nfe_dist_itm-pis_cst is initial.
        gs_zib_nfe_dist_itm-pis_cst = <ZESZINTG_DET>-imposto-pis-pisoutr-cst.
      endif.

    elseif <ZESZINTG_DET>-imposto-pis-pisst is not initial.
      gs_zib_nfe_dist_itm-pis_base = <ZESZINTG_DET>-imposto-pis-pisst-vbc.
      gs_zib_nfe_dist_itm-pis_aqt = <ZESZINTG_DET>-imposto-pis-pisst-ppis.
      gs_zib_nfe_dist_itm-pis_valor = <ZESZINTG_DET>-imposto-pis-pisst-vpis.

      if gs_zib_nfe_dist_itm-pis_cst is initial.
        gs_zib_nfe_dist_itm-pis_cst = <ZESZINTG_DET>-imposto-pis-pisst-cst.
      endif.

    endif.

*    gs_zib_nfe_dist_itm-PIS_QTD_VENDIDA
*    gs_zib_nfe_dist_itm-PIS_AQT_REAIS
*    gs_zib_nfe_dist_itm-PIS_ST_BASE
*    gs_zib_nfe_dist_itm-PIS_ST_AQT
*    gs_zib_nfe_dist_itm-PIS_ST_QTD_VENDI
*    gs_zib_nfe_dist_itm-PIS_ST_AQT_REAIS
*    gs_zib_nfe_dist_itm-PIS_ST_VALOR

*   DETERMINAÇÃO COFINS
    gs_zib_nfe_dist_itm-cof_cst = <ZESZINTG_DET>-imposto-cofins-cst.

    if <ZESZINTG_DET>-imposto-cofins-cofinsaliq is not initial.
      gs_zib_nfe_dist_itm-cof_base = <ZESZINTG_DET>-imposto-cofins-cofinsaliq-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <ZESZINTG_DET>-imposto-cofins-cofinsaliq-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <ZESZINTG_DET>-imposto-cofins-cofinsaliq-vcofins.
    elseif <ZESZINTG_DET>-imposto-cofins-cofinsqtde is not initial.
      gs_zib_nfe_dist_itm-cof_base = <ZESZINTG_DET>-imposto-cofins-cofinsqtde-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <ZESZINTG_DET>-imposto-cofins-cofinsqtde-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <ZESZINTG_DET>-imposto-cofins-cofinsqtde-vcofins.
    elseif <ZESZINTG_DET>-imposto-cofins-cofinsoutr is not initial.
      gs_zib_nfe_dist_itm-cof_base = <ZESZINTG_DET>-imposto-cofins-cofinsoutr-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <ZESZINTG_DET>-imposto-cofins-cofinsoutr-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <ZESZINTG_DET>-imposto-cofins-cofinsoutr-vcofins.
    elseif <ZESZINTG_DET>-imposto-cofins-cofinsst is not initial.
      gs_zib_nfe_dist_itm-cof_base = <ZESZINTG_DET>-imposto-cofins-cofinsst-vbc.
      gs_zib_nfe_dist_itm-cof_aqt = <ZESZINTG_DET>-imposto-cofins-cofinsst-pcofins.
      gs_zib_nfe_dist_itm-cof_valor = <ZESZINTG_DET>-imposto-cofins-cofinsst-vcofins.
    endif.

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

    modify zib_nfe_dist_itm from gs_zib_nfe_dist_itm.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_TER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_ter using p_status p_cod_sefaz.

  data: lv_stcd1    type lfa1-stcd1,
        lv_domvalue type dd07v-domvalue_l,
        lv_ddtext   type dd07v-ddtext.

* Atualiza Tabela ZESZIB_CTE_DIST_TER
  gs_zib_cte_dist_ter-cd_chave_cte   = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
*  GS_ZIB_CTE_DIST_TER-DOCNUM_CTE
*  GS_ZIB_CTE_DIST_TER-CK_FINALIZADO
  gs_zib_cte_dist_ter-cd_status_dist = '2'.

  lv_domvalue = gs_zib_cte_dist_ter-cd_status_dist.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZDM_CTE_DIST_ST'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

  gs_zib_cte_dist_ter-ds_status_dist  = lv_ddtext.
  gs_zib_cte_dist_ter-cd_status_sefaz = gs_xml_sefaz_cte-cteproc-protcte-infprot-cstat.
  gs_zib_cte_dist_ter-numr_cte        = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-nnf.
  gs_zib_cte_dist_ter-numr_serie      = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-serie.
  gs_zib_cte_dist_ter-xmlvers         = gs_xml_sefaz_cte-cteproc-cte-infcte-a_versao.

  perform zf_get_data_utc using gs_xml_sefaz_cte-cteproc-cte-infcte-ide-dhemi
                       changing gs_zib_cte_dist_ter-dt_emissao.

  perform zf_get_hora_utc using gs_xml_sefaz_cte-cteproc-cte-infcte-ide-dhemi
                       changing gs_zib_cte_dist_ter-hr_emissao.

  gs_zib_cte_dist_ter-valor_prestacao = gs_xml_sefaz_cte-cteproc-cte-infcte-vprest-vtprest.
  gs_zib_cte_dist_ter-valor_receber   = gs_xml_sefaz_cte-cteproc-cte-infcte-vprest-vrec.

  "   DETERMINAÇÃO ICMS
  if gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-cst is not initial.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms00-cst.

  elseif gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20 is not initial.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms20-cst.


  elseif gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms45 is not initial.

    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms45-cst.

  elseif gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60 is not initial.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms60-cst.

  elseif gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90 is not initial.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90-vbc.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90-vicms.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icms90-cst.

  elseif gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-cst is not initial.

    gs_zib_cte_dist_ter-valor_base_icms = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-vbcoutrauf.
    gs_zib_cte_dist_ter-valor_icms      = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-vicmsoutrauf.
    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmsoutrauf-cst.

  elseif gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmssn-cst is not initial.

    gs_zib_cte_dist_ter-cst_icms        = gs_xml_sefaz_cte-cteproc-cte-infcte-imp-icms-icmssn-cst.

  endif.


  perform zf_get_dados_tomador changing gs_zib_cte_dist_ter-e_tomadora
                                        gs_zib_cte_dist_ter-f_tomadora
                                        gs_zib_cte_dist_ter-cd_tomador.

  clear: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_tomador.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZCTE_TOMA'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

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

  if gs_zib_cte_dist_ter-reme_cnpj is not initial.
    gs_zib_cte_dist_ter-reme_tp_doc           = 1.
  elseif gs_zib_cte_dist_ter-reme_cpf is not initial.
    gs_zib_cte_dist_ter-reme_tp_doc           = 2.
  endif.

*-----------------------------------------------------------------------------------------------------*
* Expedidor
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-exped_cnpj            = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-cnpj.
  gs_zib_cte_dist_ter-exped_cpf             = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-cpf.
  gs_zib_cte_dist_ter-exped_ie              = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-ie.
  gs_zib_cte_dist_ter-exped_rsocial         = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-xnome.
  gs_zib_cte_dist_ter-exped_fantasia        = gs_xml_sefaz_cte-cteproc-cte-infcte-exped-xfant.

  if gs_zib_cte_dist_ter-exped_cnpj is not initial.
    gs_zib_cte_dist_ter-exped_tp_doc           = 1.
  elseif gs_zib_cte_dist_ter-exped_cpf is not initial.
    gs_zib_cte_dist_ter-exped_tp_doc           = 2.
  endif.

*-----------------------------------------------------------------------------------------------------*
* Recebedor
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-receb_cnpj            = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-cnpj.
  gs_zib_cte_dist_ter-receb_cpf             = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-cpf.
  gs_zib_cte_dist_ter-receb_ie              = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-ie.
  gs_zib_cte_dist_ter-receb_rsocial         = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-xnome.
  gs_zib_cte_dist_ter-receb_fantasia        = gs_xml_sefaz_cte-cteproc-cte-infcte-receb-xfant.

  if gs_zib_cte_dist_ter-receb_cnpj is not initial.
    gs_zib_cte_dist_ter-receb_tp_doc           = 1.
  elseif gs_zib_cte_dist_ter-receb_cpf is not initial.
    gs_zib_cte_dist_ter-receb_tp_doc           = 2.
  endif.

*-----------------------------------------------------------------------------------------------------*
* Destinatario
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-dest_cnpj             = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-cnpj.
  gs_zib_cte_dist_ter-dest_cpf              = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-cpf.
  gs_zib_cte_dist_ter-dest_ie               = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-ie.
  gs_zib_cte_dist_ter-dest_rsocial          = gs_xml_sefaz_cte-cteproc-cte-infcte-dest-xnome.

  if gs_zib_cte_dist_ter-dest_cnpj is not initial.
    gs_zib_cte_dist_ter-dest_tp_doc           = 1.
  elseif gs_zib_cte_dist_ter-dest_cpf is not initial.
    gs_zib_cte_dist_ter-dest_tp_doc           = 2.
  endif.

*-----------------------------------------------------------------------------------------------------*
* Outros
*-----------------------------------------------------------------------------------------------------*
  gs_zib_cte_dist_ter-toma4_cnpj            = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-cnpj.
  gs_zib_cte_dist_ter-toma4_cpf             = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-cpf.
  gs_zib_cte_dist_ter-toma4_ie              = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-ie.
  gs_zib_cte_dist_ter-toma4_rsocial         = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-xnome.
  gs_zib_cte_dist_ter-toma4_fantasia        = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-xfant.

  if gs_zib_cte_dist_ter-toma4_cnpj is not initial.
    gs_zib_cte_dist_ter-toma4_tp_doc           = 1.
  elseif gs_zib_cte_dist_ter-toma4_cpf is not initial.
    gs_zib_cte_dist_ter-toma4_tp_doc           = 2.
  endif.

  "-------------------------------------------------------------------------------------------------------*

  gs_zib_cte_dist_ter-modelo                = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-mod.
  gs_zib_cte_dist_ter-cd_modal              = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-modal.


  clear: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_modal.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZMODAL'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

  gs_zib_cte_dist_ter-ds_modal = lv_ddtext.
  gs_zib_cte_dist_ter-cd_tipo_servico       = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-tpserv.

  clear: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_tipo_servico.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZTPSERV'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

  gs_zib_cte_dist_ter-ds_tipo_servico = lv_ddtext.
  gs_zib_cte_dist_ter-cd_tipo_cte           = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-tpcte.

  clear: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_tipo_cte.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZTPSERV'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

  gs_zib_cte_dist_ter-ds_tipo_cte = lv_ddtext.

  gs_zib_cte_dist_ter-cd_fpagamento         = 0.
  clear: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_fpagamento.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZFORPAG'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

  gs_zib_cte_dist_ter-ds_fpagamento = lv_ddtext.

  gs_zib_cte_dist_ter-cd_femissao           = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-tpemis.

  clear: lv_domvalue, lv_ddtext.
  lv_domvalue = gs_zib_cte_dist_ter-cd_femissao.

  call function 'DOMAIN_VALUE_GET'
    exporting
      i_domname  = 'ZESZTFOREM'
      i_domvalue = lv_domvalue
    importing
      e_ddtext   = lv_ddtext
    exceptions
      not_exist  = 1
      others     = 2.

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

  perform zf_get_data_utc using gs_xml_sefaz_cte-cteproc-protcte-infprot-dhrecbto
                         changing gs_zib_cte_dist_ter-dt_protocolo.

  perform zf_get_hora_utc using gs_xml_sefaz_cte-cteproc-protcte-infprot-dhrecbto
                               changing gs_zib_cte_dist_ter-hr_protocolo.

  perform zf_atrib_st_sefaz_zib_cte using p_status
                                          p_cod_sefaz
                                 changing gs_zib_cte_dist_ter.

  gs_zib_cte_dist_ter-regio                  = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte(2).
  gs_zib_cte_dist_ter-nfyear                 = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+2(2).
  gs_zib_cte_dist_ter-nfmonth                = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+4(2).
  gs_zib_cte_dist_ter-docnum9                = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+34(9).
  gs_zib_cte_dist_ter-cdv                    = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte+43(1).
  gs_zib_cte_dist_ter-ds_prod_pred           = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-propred.
  gs_zib_cte_dist_ter-vl_total_merc          = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-vcarga.

  if gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj eq '09257877000218' or
     gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj eq '42276907000209' or
     gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj eq '00924429000175' .

    read table gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-infq into data(wa_carga) with key tpmed = 'PESO BASE DE CALCULO'.
    if sy-subrc is initial.
      case wa_carga-cunid.
        when '01'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga.
        when '02'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga * 1000.
      endcase.
    endif.

  else.

    loop at gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infcarga-infq into wa_carga where cunid eq '01' or cunid eq '02'.
      case wa_carga-cunid.
        when '01'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga.
        when '02'.
          gs_zib_cte_dist_ter-qt_carga_cte = wa_carga-qcarga * 1000.
      endcase.
      exit.
    endloop.

  endif.

  lv_stcd1 = gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj.

  select single lifnr
    into gs_zib_cte_dist_ter-p_emissor
    from lfa1
   where stcd1 = lv_stcd1.

  perform zf_buscar_empresa using gs_xml_sefaz_cte-cteproc-cte-infcte-emit-cnpj
                                  gs_xml_sefaz_cte-cteproc-cte-infcte-emit-ie
                                  space
                         changing gs_zib_cte_dist_ter-e_emissor
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
  modify ZESZIB_CTE_DIST_TER from gs_zib_cte_dist_ter.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_C57
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_c57 .

* Atualiza Tabela ZESZIB_CTE_DIST_C57
  check gs_xml_sefaz_cte-cteproc-cte-infcte-infctecomp-chcte is not initial.

  gs_zib_cte_dist_c57-cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
  gs_zib_cte_dist_c57-c57_chave_acesso = gs_xml_sefaz_cte-cteproc-cte-infcte-infctecomp-chcte.
  modify ZESZIB_CTE_DIST_C57 from gs_zib_cte_dist_c57.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_CVL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_cvl .

* Atualiza Tabela ZESZIB_CTE_DIST_C57

  loop at gs_xml_sefaz_cte-cteproc-cte-infcte-vprest-comp into data(wl_comp).
    gs_zib_cte_dist_cvl-cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_cvl-nome_componente = wl_comp-xnome.
    gs_zib_cte_dist_cvl-valr_componente = wl_comp-vcomp.
    modify ZESZIB_CTE_DIST_CVL from gs_zib_cte_dist_cvl.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_D55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_d55 .

  data: lt_zde_inf_cte_norm type ZESZDE_INF_CTE_NORM_INF_NFE_T,
        ls_zde_inf_cte_norm type ZESZDE_INF_CTE_NORM_INF_NFE.

*  Atualiza Tabela ZESZIB_CTE_DIST_D55
  lt_zde_inf_cte_norm[] =  gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnfe[].

  loop at lt_zde_inf_cte_norm into ls_zde_inf_cte_norm.
    gs_zib_cte_dist_d55-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_d55-n55_chave_acesso = ls_zde_inf_cte_norm-chave.

    check ls_zde_inf_cte_norm-infunidtransp-tpunidtransp is not initial.

    gs_zib_cte_dist_d55-tp_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-tpunidtransp.
    gs_zib_cte_dist_d55-id_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-idunidtransp.
    gs_zib_cte_dist_d55-valr_peso_rate   = ls_zde_inf_cte_norm-infunidtransp-qtdrat.
    modify ZESZIB_CTE_DIST_D55 from gs_zib_cte_dist_d55.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_dup .

*  Atualiza Tabela ZESZIB_CTE_DIST_D55
  " GS_ZIB_CTE_DIST_D55-CD_CHAVE_CTE = GS_XML_SEFAZ_CTE-CTEPROC-PROTCTE-INFPROT-CHCTE.
*  GS_ZIB_CTE_DIST_D55-NR_DUPLICATA
*  GS_ZIB_CTE_DIST_D55-DT_VENCIMENTO
*  GS_ZIB_CTE_DIST_D55-VL_DUPLICATA
  "  MODIFY ZESZIB_CTE_DIST_D55 FROM GS_ZIB_CTE_DIST_D55.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_MOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_mot .

*  Atualiza Tabela ZESZIB_CTE_DIST_MOT

  check gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-moto-xnome is not initial.

  gs_zib_cte_dist_mot-cd_chave_cte = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
  gs_zib_cte_dist_mot-moto_cpf     = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-moto-cpf.
  gs_zib_cte_dist_mot-moto_nome    = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-moto-xnome.
  modify ZESZIB_CTE_DIST_MOT from gs_zib_cte_dist_mot  .

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_N01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_n01 .


  data: lt_zde_inf_cte_norm_inf type ZESZDE_INF_CTE_NORM_INF_NFF_T,
        ls_zde_inf_cte_norm_inf type ZESZDE_INF_CTE_NORM_INF_NFF.

*  Atualiza Tabela ZESZIB_CTE_DIST_N01
  lt_zde_inf_cte_norm_inf[] = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnf[].

  loop at lt_zde_inf_cte_norm_inf into ls_zde_inf_cte_norm_inf.
    gs_zib_cte_dist_n01-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_n01-n01_modelo_nf    = ls_zde_inf_cte_norm_inf-mod.
    gs_zib_cte_dist_n01-n01_numr_serie   = ls_zde_inf_cte_norm_inf-serie.
    gs_zib_cte_dist_n01-n01_nr_nf        = ls_zde_inf_cte_norm_inf-ndoc.

    perform zf_get_data_utc using ls_zde_inf_cte_norm_inf-demi
                         changing gs_zib_cte_dist_n01-n01_data_emissao.

    gs_zib_cte_dist_n01-n01_vl_base_icms  = ls_zde_inf_cte_norm_inf-vbc.
    gs_zib_cte_dist_n01-n01_vl_icms       = ls_zde_inf_cte_norm_inf-vicms.
    gs_zib_cte_dist_n01-n01_vl_bicms_st   = ls_zde_inf_cte_norm_inf-vbcst.
    gs_zib_cte_dist_n01-n01_vl_icms_st    = ls_zde_inf_cte_norm_inf-vst.
    gs_zib_cte_dist_n01-n01_vl_produtos   = ls_zde_inf_cte_norm_inf-vprod.
    gs_zib_cte_dist_n01-n01_vl_nota       = ls_zde_inf_cte_norm_inf-vnf.
    gs_zib_cte_dist_n01-n01_codg_cfop     = ls_zde_inf_cte_norm_inf-ncfop.

    modify ZESZIB_CTE_DIST_N01 from gs_zib_cte_dist_n01.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_n55.

  data: lt_zde_inf_cte_norm_inf type ZESZDE_INF_CTE_NORM_INF_NFE_T,
        ls_zde_inf_cte_norm_inf type ZESZDE_INF_CTE_NORM_INF_NFE.

*  Atualiza Tabela ZESZIB_CTE_DIST_N55
  lt_zde_inf_cte_norm_inf[] = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnfe[].

  select single text
    into gs_zib_cte_dist_n55-n55_desc_sefaz
    from j_1bstscodet
   where spras = sy-langu
     and code = gs_xml_sefaz_cte-cteproc-protcte-infprot-cstat.

  loop at lt_zde_inf_cte_norm_inf into ls_zde_inf_cte_norm_inf.
    gs_zib_cte_dist_n55-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_n55-n55_chave_acesso = ls_zde_inf_cte_norm_inf-chave.
    gs_zib_cte_dist_n55-n55_stat_sefaz   = gs_xml_sefaz_cte-cteproc-protcte-infprot-cstat.
    modify ZESZIB_CTE_DIST_N55 from gs_zib_cte_dist_n55.
  endloop.

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

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_VEI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_vei .

  data: lt_zde_inf_modal_rodo type zde_inf_modal_rodo_t,
        ls_zde_inf_modal_rodo type zde_inf_modal_rodo_m.

  lt_zde_inf_modal_rodo[] = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-rodo-veic[].

*  Atualiza Tabela ZESZIB_CTE_DIST_VEI
  loop at lt_zde_inf_modal_rodo into ls_zde_inf_modal_rodo.
    gs_zib_cte_dist_vei-cd_chave_cte            = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_vei-veic_placa              = ls_zde_inf_modal_rodo-placa.
    gs_zib_cte_dist_vei-veic_tara_kg            = ls_zde_inf_modal_rodo-tara.
    gs_zib_cte_dist_vei-veic_capacidade_kg      = ls_zde_inf_modal_rodo-capkg.
    gs_zib_cte_dist_vei-veic_capacidade_m3      = ls_zde_inf_modal_rodo-capm3.
    gs_zib_cte_dist_vei-veic_tipo_propriedade   = ls_zde_inf_modal_rodo-tpprop.
    gs_zib_cte_dist_vei-veic_tipo_veiculo       = ls_zde_inf_modal_rodo-tpveic.
    gs_zib_cte_dist_vei-veic_tipo_rodado        = ls_zde_inf_modal_rodo-tprod.
    gs_zib_cte_dist_vei-veic_tipo_carroceria    = ls_zde_inf_modal_rodo-tpcar.
    gs_zib_cte_dist_vei-veic_uf_licenciamento   = ls_zde_inf_modal_rodo-uf.
    gs_zib_cte_dist_vei-veic_renavam            = ls_zde_inf_modal_rodo-renavam.
    gs_zib_cte_dist_vei-veic_rntrc              = ls_zde_inf_modal_rodo-prop-rntrc.
    gs_zib_cte_dist_vei-prop_tp_doc             = 1.
    gs_zib_cte_dist_vei-prop_cnpj               = ls_zde_inf_modal_rodo-prop-cnpj.
*    GS_ZIB_CTE_DIST_VEI-PROP_CPF                = ls_ZDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_IE                 = ls_ZDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_UF_IE              = ls_ZDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_RSOCIAL            = ls_ZDE_INF_MODAL_RODO-PLACA.
*    GS_ZIB_CTE_DIST_VEI-PROP_TIPO_PROP          = ls_ZDE_INF_MODAL_RODO-PLACA.
    modify ZESZIB_CTE_DIST_VEI from gs_zib_cte_dist_vei.
  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_ATRIBUIR_CTE_DIST_VGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zf_atribuir_cte_dist_vga .

*  Atualiza Tabela ZESZIB_CTE_DIST_VGA

  check gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-nvag is not initial.

  gs_zib_cte_dist_vga-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
  gs_zib_cte_dist_vga-numr_ident_vagao = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-nvag.
*  GS_ZIB_CTE_DIST_VGA-INFO_CAPACIDADE
  gs_zib_cte_dist_vga-tipo_vagao       = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-tpvag.
  gs_zib_cte_dist_vga-valr_peso_real   = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-pesor.
  gs_zib_cte_dist_vga-valr_peso_bc     = gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infmodal-ferrov-detvag-pesobc.
  modify ZESZIB_CTE_DIST_VGA from gs_zib_cte_dist_vga.

endform.

form zf_get_dados_tomador changing c_e_tomadora
                                   c_f_tomadora
                                   c_cd_tomador type c.

  clear: c_cd_tomador, c_e_tomadora,  c_f_tomadora.

  case abap_true.
    when gwa_cte_read-cte_57.

      if gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma3 is not initial.
        c_cd_tomador   = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma3-toma.
      elseif gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4 is not initial.
        c_cd_tomador   = gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-toma.
      endif.

      case c_cd_tomador.
        when '0'.  "Remetente

          perform zf_buscar_empresa using gs_xml_sefaz_cte-cteproc-cte-infcte-rem-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-rem-ie
                                          space
                                 changing c_e_tomadora
                                          c_f_tomadora.
        when '1'.  "Expedidor

          perform zf_buscar_empresa using gs_xml_sefaz_cte-cteproc-cte-infcte-exped-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-exped-ie
                                          space
                                 changing c_e_tomadora
                                          c_f_tomadora.

        when '2'.  "Recebedor

          perform zf_buscar_empresa using gs_xml_sefaz_cte-cteproc-cte-infcte-receb-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-receb-ie
                                          space
                                 changing c_e_tomadora
                                          c_f_tomadora.

        when '3'.  "Destinatário

          perform zf_buscar_empresa using gs_xml_sefaz_cte-cteproc-cte-infcte-dest-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-dest-ie
                                          space
                                 changing c_e_tomadora
                                          c_f_tomadora.
        when '4'.  "Outros

          perform zf_buscar_empresa using gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-cnpj
                                          gs_xml_sefaz_cte-cteproc-cte-infcte-ide-toma4-ie
                                          space
                                 changing c_e_tomadora
                                          c_f_tomadora.
      endcase.

    when gwa_cte_read-cte_67.

      perform zf_buscar_empresa using gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-toma-cnpj
                                      gs_xml_sefaz_cte_os-cteosproc-cteos-infcte-toma-ie
                                      space
                             changing c_e_tomadora
                                      c_f_tomadora.

    when others.
      exit.
  endcase.



endform.

form zf_atribuir_cte_dist_cpl.

  data: lt_zde_inf_compl type ZESZDE_INF_CTE_COMPL_OBS_CONT_T,
        ls_zde_inf_compl type ZESZDE_INF_CTE_COMPL_OBS_CONT.

  data: v_contador type ZESZIB_CTE_DIST_CPL-cd_contador.

*  Atualiza Tabela ZESZIB_CTE_DIST_D55
  lt_zde_inf_compl[] =  gs_xml_sefaz_cte-cteproc-cte-infcte-compl-obscont[].

  loop at lt_zde_inf_compl into ls_zde_inf_compl.

    add 1 to v_contador.

    gs_zib_cte_dist_cpl-cd_chave_cte  = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.
    gs_zib_cte_dist_cpl-tp_identifica = '1'.
    gs_zib_cte_dist_cpl-ds_campo      = ls_zde_inf_compl-a_xcampo.
    gs_zib_cte_dist_cpl-cd_contador   = v_contador.
    gs_zib_cte_dist_cpl-ds_texto      = ls_zde_inf_compl-xtexto.

    modify ZESZIB_CTE_DIST_CPL from gs_zib_cte_dist_cpl.
  endloop.


endform.

form zf_atribuir_cte_dist_001.

  data vg_container(14) type c.



  loop at gs_xml_sefaz_cte-cteproc-cte-infcte-compl-obscont into data(wl_inf_compl).

    clear: gs_zib_cte_dist_001.

    try.

        gs_zib_cte_dist_001-cd_chave_cte  = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.

        clear vg_container.

        vg_container = wl_inf_compl-a_xcampo.

        "CASE wl_inf_compl-a_xcampo+0(14).
        case vg_container.
          when 'CONTAINER_NOTA'.

            split wl_inf_compl-xtexto at ';' into: data(lc_nm_container) data(lc_nfe) data(lc_quantidade).

            gs_zib_cte_dist_001-nm_container     = lc_nm_container.
            gs_zib_cte_dist_001-n55_chave_acesso = lc_nfe.

            replace all occurrences of ',' in lc_quantidade with '.'.
            gs_zib_cte_dist_001-nm_quantidade  = lc_quantidade.

          when 'CONTAINER'.

            split wl_inf_compl-xtexto at ';' into: lc_nm_container lc_quantidade.

            gs_zib_cte_dist_001-nm_container     = lc_nm_container.

            replace all occurrences of ',' in lc_quantidade with '.'.
            gs_zib_cte_dist_001-nm_quantidade  = lc_quantidade.

          when others.
            continue.
        endcase.

        modify ZESZIB_CTE_DIST_001 from gs_zib_cte_dist_001.

      catch cx_sy_conversion_no_number.

    endtry.

  endloop.

endform.


form zf_atribuir_cte_dist_d01 .

  data: lt_zde_inf_cte_norm type ZESZDE_INF_CTE_NORM_INF_NFF_T,
        ls_zde_inf_cte_norm type ZESZDE_INF_CTE_NORM_INF_NFF.

*  Atualiza Tabela ZESZIB_CTE_DIST_D01
  lt_zde_inf_cte_norm[] =  gs_xml_sefaz_cte-cteproc-cte-infcte-infctenorm-infdoc-infnf[].

  loop at lt_zde_inf_cte_norm into ls_zde_inf_cte_norm.
    gs_zib_cte_dist_d01-cd_chave_cte     = gs_xml_sefaz_cte-cteproc-protcte-infprot-chcte.

    gs_zib_cte_dist_d01-n01_modelo_nf    = ls_zde_inf_cte_norm-mod.
    gs_zib_cte_dist_d01-n01_numr_serie   = ls_zde_inf_cte_norm-serie.
    gs_zib_cte_dist_d01-n01_nr_nf        = ls_zde_inf_cte_norm-ndoc.

    check ls_zde_inf_cte_norm-infunidtransp-tpunidtransp is not initial.

    gs_zib_cte_dist_d01-tp_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-tpunidtransp.
    gs_zib_cte_dist_d01-id_unid_transp   = ls_zde_inf_cte_norm-infunidtransp-idunidtransp.
    gs_zib_cte_dist_d01-valr_peso_rate   = ls_zde_inf_cte_norm-infunidtransp-qtdrat.

    modify ZESZIB_CTE_DIST_D01 from gs_zib_cte_dist_d01.
  endloop.

endform.

form zf_atrib_st_sefaz_zib_cte using p_status
                                     p_cod_sefaz
                            changing c_zib_cte_dist_ter type ZESZIB_CTE_DIST_TER.

  c_zib_cte_dist_ter-cd_status_sefaz = p_cod_sefaz.

  case p_status.
    when '1'.
      c_zib_cte_dist_ter-docsta          = '1'.
      c_zib_cte_dist_ter-cancel          = ' '.
    when '2'.
      c_zib_cte_dist_ter-docsta          = '1'.
      c_zib_cte_dist_ter-cancel          = 'X'.
  endcase.

endform.


form zf_atrib_st_sefaz_zib_nfe using p_status
                                     p_cod_sefaz
                            changing c_zib_nfe_dist_ter type ZESZIB_NFE_DIST_TER.

  c_zib_nfe_dist_ter-cd_msg_sefaz = p_cod_sefaz.

  case p_status.
    when '1'.
      c_zib_nfe_dist_ter-docsta = '1'.
      c_zib_nfe_dist_ter-cancel = ' '.
    when '2'.
      c_zib_nfe_dist_ter-docsta = '1'.
      c_zib_nfe_dist_ter-cancel = 'X'.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_5128  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_5128 output.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  "PERFORM caixa_txt_obs. "Comentado 03/05/2023 para publicar versão PRD
endmodule.
*&---------------------------------------------------------------------*
*&      Form  CAIXA_TXT_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form caixa_txt_obs .

  if not c_editor is initial and not editor is initial.
    call method c_editor->free( ).
    call method editor->free( ).
  endif.

  create object: c_editor exporting container_name = 'C_EDITOR', "CONTAINER
                   editor exporting parent         = c_editor.

  call method editor->set_toolbar_mode( toolbar_mode = editor->false ).
  call method editor->set_statusbar_mode( statusbar_mode = editor->false ).

  case txtopen.
    when 'X'.
      call method editor->set_readonly_mode( readonly_mode = editor->false ).
    when others.
      call method editor->set_readonly_mode( readonly_mode = editor->true ).
  endcase.

  call method editor->set_text_as_stream
    exporting
      text = it_editor.


  free: txtopen, it_editor.

endform.

*** US #173081 - MMSILVA - 03.07.2025 - Ini ***
form zf_atribuir_nfe_dist_avb.
  data: ls_infevento    type ZESZINTG_EVENTO,
        ls_retinfevento type zinfevento.

  ls_infevento                          = gs_xml_averb-proceventonfe-evento-infevento.
  ls_retinfevento                       = gs_xml_averb-proceventonfe-retevento-infevento.

  gs_zib_nfe_dist_avb-orgao_recep       = ls_infevento-corgao.
  gs_zib_nfe_dist_avb-tp_ambiente       = ls_infevento-TPAMB.
  gs_zib_nfe_dist_avb-cnpj              = ls_infevento-cnpj.
  gs_zib_nfe_dist_avb-chave_nfe         = ls_infevento-chnfe.

  data(data_evento_avb)                 = ls_infevento-dhevento(12).
  replace all occurrences of '-' IN data_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-dt_evento         = data_evento_avb.

  data(hora_evento_avb)                 = ls_infevento-dhevento+11(8).
  replace all occurrences of ':' IN hora_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-hr_evento         = hora_evento_avb.

  gs_zib_nfe_dist_avb-tp_evento         = ls_infevento-tpevento.
  gs_zib_nfe_dist_avb-seq_evento        = ls_infevento-nseqevento.
  gs_zib_nfe_dist_avb-versao_evento     = ls_infevento-verevento.
  gs_zib_nfe_dist_avb-det_evento_versao = lv_versao.

  data(data_reg_evento_avb)             = ls_retinfevento-dhregevento(10).
  replace all occurrences of '-' IN data_reg_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-dt_reg_evento     = data_reg_evento_avb.

  data(hora_reg_evento_avb)             = ls_retinfevento-dhregevento+11(8).
  replace all occurrences of ':' IN hora_reg_evento_avb WITH ''.
  gs_zib_nfe_dist_avb-hr_reg_evento     = hora_reg_evento_avb.

  gs_zib_nfe_dist_avb-n_prot            = ls_retinfevento-nprot.

  modify zib_nfe_dist_avb from gs_zib_nfe_dist_avb.
endform.

form zf_atribuir_nfe_dist_avi.
  data: ls_infevento    type ZESZINTG_EVENTO.

  ls_infevento                              = gs_xml_averb-proceventonfe-evento-infevento.

  loop at gs_xml_averb-proceventonfe-evento-infevento-detevento-itensaverbados into data(ls_itens_averbados).
    gs_zib_nfe_dist_avi-chave_nfe             = ls_infevento-chnfe.
    gs_zib_nfe_dist_avi-detalhe_evento_versao = lv_versao.
    gs_zib_nfe_dist_avi-desc_evento           = ls_infevento-detevento-descevento.
    gs_zib_nfe_dist_avi-tp_autor              = ls_infevento-detevento-tpautor.
    gs_zib_nfe_dist_avi-ver_aplic             = ls_infevento-detevento-veraplic.

    data(data_embarque)                       = ls_itens_averbados-dhembarque(10).
    replace all occurrences of '-' IN data_embarque WITH ''.
    gs_zib_nfe_dist_avi-dt_embarque           = data_embarque.

    data(hora_embarque)                       = ls_itens_averbados-dhembarque+11(8).
    replace all occurrences of ':' IN hora_embarque WITH ''.
    gs_zib_nfe_dist_avi-hr_embarque           = hora_embarque.

    data(data_averb)                          = ls_itens_averbados-dhaverbacao(10).
    replace all occurrences of '-' IN data_averb WITH ''.
    gs_zib_nfe_dist_avi-dt_averbacao          = data_averb.

    data(hora_averb)                          = ls_itens_averbados-dhaverbacao+11(8).
    replace all occurrences of ':' IN hora_averb WITH ''.
    gs_zib_nfe_dist_avi-hr_averbacao          = hora_averb.

    gs_zib_nfe_dist_avi-n_due                 = ls_itens_averbados-ndue.
    gs_zib_nfe_dist_avi-n_item                = ls_itens_averbados-nitem.
    gs_zib_nfe_dist_avi-n_item_due            = ls_itens_averbados-nitemdue.
    gs_zib_nfe_dist_avi-qitem                 = ls_itens_averbados-qitem.
    gs_zib_nfe_dist_avi-mot_alteracao         = ls_itens_averbados-motalteracao.
    gs_zib_nfe_dist_avi-seq_evento            = ls_infevento-nseqevento. "SMC - 173081 18-09-2025

    modify zib_nfe_dist_avi from gs_zib_nfe_dist_avi.
  endloop.
endform.
*** US #173081 - MMSILVA - 03.07.2025 - Fim ***
