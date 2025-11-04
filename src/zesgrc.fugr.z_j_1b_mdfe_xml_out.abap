FUNCTION z_j_1b_mdfe_xml_out.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(BRANCH) TYPE  J_1BBRANC_
*"     REFERENCE(MODEL) TYPE  J_1BMODEL
*"     VALUE(XMLH) TYPE  J1B_NF_XML_HEADER
*"     VALUE(XMLH_310) TYPE  J_1BNFE_S_LAYOUT_310
*"     REFERENCE(WK_HEADER) TYPE  J_1BNFDOC
*"     REFERENCE(RESEND) TYPE  CHAR01 DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(ZET_BAPIRET2) TYPE  ZBAPIRETTAB
*"     REFERENCE(ZEV_ERROR_STATUS) TYPE  ZXNFE_ERRSTATUS
*"     REFERENCE(LV_RFCDEST_MDFE) TYPE  RFCDEST
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RFC_ERROR
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data      |Request   |Autor       |Alteração                         *
*----------------------------------------------------------------------*
* 13/08/2025|DEVK9A2QU1|NSEGATIN    |Implementação da Nota Técnica     *
*                                   |2025.001 MDFE Versão 1.02.        *
*                                   |Chamado: 185281.                  *
*----------------------------------------------------------------------*
  DATA: x_cust3         TYPE j_1bnfe_cust3.
  DATA: lv_xmdfeactive  TYPE j_1bxnfeactive.
  DATA: ls_dhcont    TYPE string,
        ls_sadr      TYPE sadr,
        ls_branch    TYPE j_1bbranch,
        lv_cgc       TYPE j_1bcgc,
        ls_addr1_val TYPE addr1_val,
        lv_time      TYPE sy-uzeit,
        pid_text     TYPE i VALUE 1,
        e_time       TYPE  erzet,
        v_line_id    TYPE zxnfe_lineid,
        wa_retorno   TYPE bapiret2.

  DATA: lc_latitude      TYPE p LENGTH 8 DECIMALS 6,
        lc_longitude     TYPE p LENGTH 9 DECIMALS 6,
        lc_coordenada_la TYPE c LENGTH 10,
        lc_coordenada_lo TYPE c LENGTH 11.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  DATA: l_acckey              TYPE j_1b_nfe_access_key,
        lwa_contingencia_mdfe TYPE char01,
        lc_url                TYPE string.

  DATA(obj_mdfe) = NEW zcl_mdfe( ).
  lwa_contingencia_mdfe = obj_mdfe->check_contingencia_mdfe( branch ).
  lwa_contingencia_mdfe = obj_mdfe->check_conting_mdfe_reenvio( xmlh-docnum ).


*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

* Get settings from configuration table J_1BNFE_CUST2
  CALL FUNCTION 'J_1BNFE_CUST3_READ'
    EXPORTING
      iv_bukrs       = bukrs
      iv_branch      = branch
      iv_model       = model
    IMPORTING
      es_cust3       = x_cust3
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF NOT sy-subrc IS INITIAL.
    CLEAR x_cust3.
  ENDIF.

  xmlh-tpamb       = x_cust3-tpamb.
  xmlh-verproc     = x_cust3-verproc.
  xmlh-procemi     = x_cust3-procemi.
  xmlh-alter_regio = x_cust3-region.

  IF wk_header-conting IS INITIAL.
    xmlh-tpemis  = x_cust3-tpemisnorm.
  ELSE.
    xmlh-tpemis  = x_cust3-tpemiscont.
  ENDIF.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  IF lwa_contingencia_mdfe = abap_true.
    xmlh-tpemis  = '2'.
    xmlh-conting = abap_true.
  ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      i_bukrs      = bukrs
      i_branch     = branch
      i_model      = model
    IMPORTING
      e_rfcdest    = lv_rfcdest_mdfe
      e_xnfeactive = lv_xmdfeactive
    EXCEPTIONS
      rfc_error    = 1
      OTHERS       = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING rfc_error.
  ENDIF.

  DATA: zis_mdfe_header               TYPE zxnfe_if_mdfe_header_s,
        zit_mdfe_text                 TYPE zxnfe_if_mdfe_text_t,
        wit_mdfe_text                 TYPE zxnfe_if_mdfe_text_s,
        zis_mdfe_ide                  TYPE zxnfe_if_mdfe_ide_300_s,
        zit_mdfe_inf_percurso         TYPE zxnfe_if_mdfe_inf_percurso_t,
        wat_mdfe_inf_percurso         TYPE zxnfe_if_mdfe_inf_percurso_s,
        zit_mdfe_inf_muncarrega       TYPE zxnfe_if_mdfe_inf_muncarrega_t,
        wit_mdfe_inf_muncarrega       TYPE zxnfe_if_mdfe_inf_muncarrega_s,
        zis_mdfe_emit                 TYPE zxnfe_if_mdfe_emit_s,
        zis_mdfe_infmodal_aquav       TYPE zxnfe_if_mdfe_aquav_300_s,
        zit_mdfe_inftermcarreg        TYPE zxnfe_if_mdfe_inftermcarreg_t,
        zit_mdfe_inftermdescarreg     TYPE zxnfe_if_mdfe_inftermdescarr_t,
        wit_mdfe_inftermdescarreg     TYPE zxnfe_if_mdfe_inftermdescarr_s,
        zit_mdfe_infembcomb           TYPE zxnfe_if_mdfe_infembcomb_300_t,
        zit_mdfe_infunidcargavazia    TYPE zxnfe_if_mdfe_infunidcargava_t,
        zit_mdfe_infunidtranspvazia   TYPE zxnfe_if_mdfe_infunidtranspv_t,
        zis_mdfe_infmodal_rodo        TYPE zxnfe_if_mdfe_rodo_300_s,
        zit_mdfe_rodo_veic            TYPE zxnfe_if_mdfe_rodo_veic_t,
        wit_mdfe_rodo_veic            TYPE zxnfe_if_mdfe_rodo_veic_s,
        zit_mdfe_rodo_prop            TYPE zxnfe_if_mdfe_rodo_prop_t,
        wit_mdfe_rodo_prop            TYPE zxnfe_if_mdfe_rodo_prop_s,
        zit_mdfe_rodo_condutor        TYPE zxnfe_if_mdfe_rodo_condutor_t,
        wit_mdfe_rodo_condutor        TYPE zxnfe_if_mdfe_rodo_condutor_s,
        zit_mdfe_rodo_inf_ciot        TYPE zxnfe_if_mdfe_rodo_inf_ciot_t,
        wit_mdfe_rodo_inf_ciot        TYPE zxnfe_if_mdfe_rodo_inf_ciot_s,
        zit_mdfe_rodo_disp            TYPE zxnfe_if_mdfe_rodo_disp_300_t,
        zit_mdfe_rodo_inf_contratante TYPE zxnfe_if_mdfe_rodo_inf_cont_t,
        wit_mdfe_rodo_inf_contratante TYPE zxnfe_if_mdfe_rodo_inf_cont_s,
**<<<------"185281 - NMS - INI------>>>
        zit_mdfe_rodo_inf_pagamento   TYPE zxnfe_if_mdfe_rodo_inf_cont_t,
        wit_mdfe_rodo_inf_pagamento   TYPE zxnfe_if_mdfe_rodo_inf_cont_s,
        zit_mdfe_rodo_inf_comp        TYPE zxnfe_if_mdfe_rodo_inf_comp_t,
        wit_mdfe_rodo_inf_comp        TYPE zxnfe_if_mdfe_rodo_inf_comp_s,
        wit_mdfe_rodo_inf_cmp_t_s     TYPE zxnfe_if_mdfe_rodo_inf_cmp_t_s,
        zit_mdfe_rodo_inf_banco       TYPE zxnfe_if_mdfe_rodo_inf_banco_t,
        wit_mdfe_rodo_inf_banco       TYPE zxnfe_if_mdfe_rodo_inf_banco_s,
**<<<------"185281 - NMS - FIM------>>>
        zit_mdfe_inf_mundescarg       TYPE zxnfe_if_mdfe_inf_mundescarg_t,
        wit_mdfe_inf_mundescarg       TYPE zxnfe_if_mdfe_infmundescarg_s,
        zit_mdfe_infcte               TYPE zxnfe_if_mdfe_infcte_300_t,
        wit_mdfe_infcte               TYPE zxnfe_if_mdfe_infcte_300_s,
        zit_mdfe_infnfe               TYPE zxnfe_if_mdfe_infnfe_300_t,
        wit_mdfe_infnfe               TYPE zxnfe_if_mdfe_infnfe_300_s,
        zit_mdfe_infmdfetransp        TYPE zxnfe_if_mdfe_infmdfetra_300_t,
        zit_mdfe_infunidtransp        TYPE zxnfe_if_cte_infunidtransp_t,
        zit_mdfe_infunidcarga         TYPE zxnfe_if_cte_infunidcarga_t,
        zit_mdfe_seg                  TYPE zxnfe_if_mdfe_seg_t,
        wit_mdfe_seg                  TYPE zxnfe_if_mdfe_seg_s,
        wis_mdfe_tot                  TYPE zxnfe_if_mdfe_tot_s,
        zit_mdfe_autxml               TYPE zxnfe_if_mdfe_autxml_t,
        zis_mdfe_infadic              TYPE zxnfe_if_mdfe_infadic_s,
        zis_mdfe_infresptec           TYPE zxnfe_if_mdfe_infresptec_s,
        ziv_text_id_lacre             TYPE zxnfe_text_ref,
        zis_mdfe_prodpred             TYPE zrsi_mdfe_prodpred,
        ziv_resend                    TYPE zxnfe_resend.

  DATA: v_seqno_ref_cte_nfe TYPE zxnfe_seq_no.

  ziv_resend = resend.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  IF lwa_contingencia_mdfe = abap_true.
    xmlh-id+34(1)             = '2'.
    l_acckey                  = xmlh-id.

    CALL FUNCTION 'J_1B_NFE_CREATE_CHECK_DIGIT'
      CHANGING
        c_acckey = l_acckey.

    xmlh-id                   = l_acckey.
    xmlh-cdv                  = xmlh-id+43(1).
  ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  zis_mdfe_header-rfc_version = '0001'.
  zis_mdfe_header-docnum      = xmlh-docnum.
  zis_mdfe_header-accesskey   = xmlh-id.
  zis_mdfe_header-logsys      = xmlh-logsys.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = xmlh-serie
    IMPORTING
      output = xmlh-serie.

  SHIFT xmlh-cnf.
  DATA: lc_cnf TYPE c LENGTH 9.
  lc_cnf = xmlh-cnf.
  DATA: lc_nnf TYPE c LENGTH 9.
  lc_nnf = xmlh-nnf.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lc_nnf
    IMPORTING
      output = lc_nnf.

  zis_mdfe_ide-c_uf         = xmlh-cuf.
  zis_mdfe_ide-tp_amb       = xmlh-tpamb.
  zis_mdfe_ide-tp_emis      = xmlh-tpemis.
  zis_mdfe_ide-mod          = xmlh-mod.
  zis_mdfe_ide-serie        = xmlh-serie.
  zis_mdfe_ide-n_mdf        = lc_nnf.
  zis_mdfe_ide-ver_proc     = xmlh-verproc.
  zis_mdfe_ide-proc_emi     = xmlh-procemi.
  zis_mdfe_ide-c_mdf        = lc_cnf.
  zis_mdfe_ide-c_dv         = xmlh-cdv.

  SELECT SINGLE * INTO @DATA(wa_zsdt0102)
    FROM zsdt0102
   WHERE docnum EQ @xmlh-docnum.

  IF ( sy-subrc EQ 0 ) AND ( wa_zsdt0102-nmdfe IS INITIAL ).

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(wl_active_mdfe)
     WHERE docnum EQ @xmlh-docnum.

    IF sy-subrc EQ 0.
      wa_zsdt0102-nmdfe = wl_active_mdfe-nfnum9.
      wa_zsdt0102-serie = wl_active_mdfe-serie.

      UPDATE zsdt0102
         SET nmdfe = wa_zsdt0102-nmdfe
             serie = wa_zsdt0102-serie
       WHERE docnum EQ xmlh-docnum.

      UPDATE zsdt0105
         SET nmdfe = wa_zsdt0102-nmdfe
       WHERE docnum_ref EQ xmlh-docnum.

      UPDATE zsdt0107
         SET nmdfe = wa_zsdt0102-nmdfe
       WHERE docnum EQ xmlh-docnum.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
      IF wa_zsdt0102-contingencia = abap_true.
        CALL FUNCTION 'Z_GRC_MONTA_LINK'
          EXPORTING
            i_docnum   = xmlh-docnum
          IMPORTING
            e_link_pdf = lc_url.

        CALL FUNCTION 'Z_GRC_REGISTRA_INF_ZIB_NFE'
          EXPORTING
            i_docnum = xmlh-docnum
            i_active = wl_active_mdfe.

        UPDATE zsdt0102
           SET url_sefaz = lc_url
         WHERE docnum EQ xmlh-docnum.
      ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

      COMMIT WORK.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO @DATA(wa_zsdt0237)
    FROM zsdt0237
   WHERE docnum EQ @xmlh-docnum.

  IF sy-subrc IS INITIAL.
    zis_mdfe_ide-tp_emit = wa_zsdt0237-tpemit.
  ELSE.
    CLEAR: wa_zsdt0237.

    CASE wa_zsdt0102-tp_doc_ref.
      WHEN '1'.
        zis_mdfe_ide-tp_emit   = '1'. "Prestador de serviço de transporte
      WHEN '2'.
        zis_mdfe_ide-tp_emit   = '2'. "Transportador de Carga Própria
    ENDCASE.
  ENDIF.

  zis_mdfe_ide-uffim = wa_zsdt0102-uffim.": Estado (UF) de destino do MDF-e. Campo não pode estar em branco
  zis_mdfe_ide-ufini = wa_zsdt0102-ufini.": Estado (UF) de emissão do MDF-e. Campo não pode estar em branco

  "Erro de atribuição: nenhuma linha encontrada na tabela IT_MDFE_INF_MUNCARREGA
  wa_zsdt0102-nmunini = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( CONV #( wa_zsdt0102-nmunini ) ) ).
  CLEAR: wit_mdfe_inf_muncarrega.
  wit_mdfe_inf_muncarrega-id = 1.
  wit_mdfe_inf_muncarrega-c_mun_carrega = wa_zsdt0102-cmunini.
  wit_mdfe_inf_muncarrega-x_mun_carrega = wa_zsdt0102-nmunini.
  APPEND wit_mdfe_inf_muncarrega TO zit_mdfe_inf_muncarrega.

  SELECT SINGLE * INTO @DATA(wa_zsdt0118)
    FROM zsdt0118
   WHERE docnum EQ @xmlh-docnum.

  SELECT * INTO TABLE @DATA(it_zsdt0105)
    FROM zsdt0105
   WHERE docnum_ref EQ @xmlh-docnum.

  CASE wk_header-transp_mode.
    WHEN '1'.

      zis_mdfe_ide-modal = '01'.

      LOOP AT it_zsdt0105 INTO DATA(wa_zsdt0105).

        "Informações de Seguro da Carga
        SELECT SINGLE * FROM zcte_seguro INTO @DATA(wa_zcte_seguro)
         WHERE docnum EQ @wa_zsdt0105-docnum.

        IF sy-subrc IS INITIAL.

          IF wa_zcte_seguro-resp_codigo IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(wa_segurado) FROM lfa1 WHERE lifnr EQ @wa_zcte_seguro-resp_codigo.
            IF sy-subrc IS NOT INITIAL.
              CLEAR: wa_segurado.
            ENDIF.
          ENDIF.

          SELECT SINGLE * FROM zlest0143 INTO @DATA(wa_zlest0143) WHERE docnum EQ @wa_zsdt0105-docnum.

          CLEAR: wit_mdfe_seg.
          wit_mdfe_seg-id       = 1.
          wit_mdfe_seg-seg_cnpj = wa_segurado-stcd1.
          wit_mdfe_seg-resp_seg = '1'.
          wit_mdfe_seg-x_seg    = wa_zcte_seguro-xseg.
          wit_mdfe_seg-n_apol   = wa_zcte_seguro-napol.
          wit_mdfe_seg-text_id_n_aver = pid_text.
          APPEND wit_mdfe_seg TO zit_mdfe_seg.

          CLEAR: wit_mdfe_text.
          wit_mdfe_text-id   = pid_text.
          wit_mdfe_text-text = wa_zlest0143-nr_averbacao.
          APPEND wit_mdfe_text TO zit_mdfe_text.
          ADD 1 TO pid_text.

        ENDIF.

        SELECT SINGLE * INTO @DATA(wa_ciot) FROM zcte_ciot WHERE docnum EQ @wa_zsdt0105-docnum.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        SELECT SINGLE * INTO @DATA(wa_identifica) FROM zcte_identifica WHERE docnum EQ @wa_zsdt0105-docnum.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        zis_mdfe_infmodal_rodo-rntrc = wa_identifica-rodo_rntrc.

        SELECT SINGLE * INTO @DATA(wa_parceiro)
          FROM zcte_parceiros WHERE docnum EQ @wa_zsdt0105-docnum.

        wit_mdfe_rodo_inf_contratante-id    = 1.
        wit_mdfe_rodo_inf_contratante-xnome = wa_parceiro-reme_xnome.
        IF wa_parceiro-reme_cnpj IS NOT INITIAL.
          wit_mdfe_rodo_inf_contratante-cnpj = wa_parceiro-reme_cnpj.
        ELSE.
          wit_mdfe_rodo_inf_contratante-cpf = wa_parceiro-reme_cpf.
        ENDIF.
        APPEND wit_mdfe_rodo_inf_contratante TO zit_mdfe_rodo_inf_contratante.

        IF wa_ciot-nr_ciot IS NOT INITIAL.
          CLEAR: wit_mdfe_rodo_inf_ciot.
          wit_mdfe_rodo_inf_ciot-id   = sy-tabix.
          wit_mdfe_rodo_inf_ciot-ciot = wa_ciot-nr_ciot.
          IF wa_ciot-tr_cnpj IS NOT INITIAL.
            wit_mdfe_rodo_inf_ciot-cnpj = wa_ciot-tr_cnpj.
          ENDIF.
          IF wa_ciot-tr_cpf IS NOT INITIAL.
            wit_mdfe_rodo_inf_ciot-cpf = wa_ciot-tr_cpf.
          ENDIF.
          APPEND wit_mdfe_rodo_inf_ciot TO zit_mdfe_rodo_inf_ciot.
        ENDIF.

      ENDLOOP.

      DATA: rgplacas TYPE RANGE OF zpc_veiculo.

      IF wa_zsdt0118-placa_cav IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low  = wa_zsdt0118-placa_cav high = wa_zsdt0118-placa_cav ) TO rgplacas.
      ENDIF.

      IF wa_zsdt0118-placa_car1 IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low  = wa_zsdt0118-placa_car1 high = wa_zsdt0118-placa_car1 ) TO rgplacas.
      ENDIF.

      IF wa_zsdt0118-placa_car2 IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low  = wa_zsdt0118-placa_car2 high = wa_zsdt0118-placa_car2 ) TO rgplacas.
      ENDIF.

      IF wa_zsdt0118-placa_car3 IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low  = wa_zsdt0118-placa_car3 high = wa_zsdt0118-placa_car3 ) TO rgplacas.
      ENDIF.

      IF rgplacas[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(it_zlest0002)
          FROM zlest0002
         WHERE pc_veiculo IN @rgplacas.
      ENDIF.

      DATA(it_prop) = it_zlest0002[].
      SORT: it_prop BY proprietario propriet_comodato.
      DELETE ADJACENT DUPLICATES FROM it_prop COMPARING proprietario.

      DATA(qtd_veiculo) = 0.
      DATA(qtd_proprie) = 0.


      DATA:wl_lfa1_prop TYPE lfa1,
           wa_adrc      TYPE adrc,
           wa_adr6      TYPE adr6.

      LOOP AT it_prop INTO DATA(wa_prop).

        IF wa_prop-cto_comodato  = '1' AND wa_prop-propriet_comodato <> ' '.

          SELECT SINGLE *
               FROM lfa1 INTO wl_lfa1_prop
              WHERE lifnr = wa_prop-propriet_comodato.

          IF ( wl_lfa1_prop IS NOT INITIAL ).
            SELECT SINGLE *
              FROM adrc INTO wa_adrc
             WHERE addrnumber EQ wl_lfa1_prop-adrnr.

            SELECT SINGLE *
              FROM adr6 INTO wa_adr6
             WHERE addrnumber EQ wl_lfa1_prop-adrnr.
          ENDIF.



        ELSE.

          SELECT SINGLE *
            FROM lfa1 INTO wl_lfa1_prop
           WHERE lifnr = wa_prop-proprietario.

          IF ( wl_lfa1_prop IS NOT INITIAL ).
            SELECT SINGLE *
              FROM adrc INTO wa_adrc
             WHERE addrnumber EQ wl_lfa1_prop-adrnr.

            SELECT SINGLE *
              FROM adr6 INTO wa_adr6
             WHERE addrnumber EQ wl_lfa1_prop-adrnr.
          ENDIF.

        ENDIF.

        IF wl_lfa1_prop-stkzn IS INITIAL AND zis_mdfe_ide-tp_emit EQ '2'. "Emissor Próprio.
          zis_mdfe_ide-tp_transp = '2'.
        ENDIF.

        ADD 1 TO qtd_proprie.

        wl_lfa1_prop-name1 = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( i_texto = CONV #( wl_lfa1_prop-name1 ) ) ).

        CLEAR: wit_mdfe_rodo_prop.
        wit_mdfe_rodo_prop-id      = qtd_proprie.
        wit_mdfe_rodo_prop-seq_no  = qtd_proprie.
        wit_mdfe_rodo_prop-cpf     = wl_lfa1_prop-stcd2.
        wit_mdfe_rodo_prop-cnpj    = wl_lfa1_prop-stcd1.
        wit_mdfe_rodo_prop-rntrc   = wl_lfa1_prop-bahns.
        wit_mdfe_rodo_prop-x_nome  = wl_lfa1_prop-name1.
        wit_mdfe_rodo_prop-ie      = wl_lfa1_prop-stcd3.
        wit_mdfe_rodo_prop-uf      = wa_adrc-region.

        CASE wl_lfa1_prop-stkzn.
          WHEN abap_true.
            "wit_mdfe_rodo_prop-tp_prop = '0'. "TAC - Agregado
            wit_mdfe_rodo_prop-tp_prop = '1'. "TAC - Independente - Regra CS2021001284
          WHEN abap_false.
            wit_mdfe_rodo_prop-tp_prop = '2'. "Outros
        ENDCASE.

        APPEND wit_mdfe_rodo_prop TO zit_mdfe_rodo_prop.

        LOOP AT it_zlest0002 INTO DATA(wa_zlest0002) WHERE proprietario = wa_prop-proprietario.
          ADD 1 TO qtd_veiculo.
          CLEAR: wit_mdfe_rodo_veic.
          wit_mdfe_rodo_veic-seq_no   = qtd_veiculo.

          IF wa_zlest0002-cto_comodato  = '1' AND wa_zlest0002-propriet_comodato <> ' '.

            wit_mdfe_rodo_veic-c_int    = wa_zlest0002-propriet_comodato.

          ELSE.

            wit_mdfe_rodo_veic-c_int    = wa_zlest0002-proprietario.

          ENDIF.

          wit_mdfe_rodo_veic-placa    = wa_zlest0002-pc_veiculo.
          wit_mdfe_rodo_veic-renavam  = wa_zlest0002-cd_renavam.
          wit_mdfe_rodo_veic-tara     = wa_zlest0002-tara.
          wit_mdfe_rodo_veic-cap_kg   = wa_zlest0002-cap_kg.
          wit_mdfe_rodo_veic-cap_m3   = wa_zlest0002-cap_m3.
          wit_mdfe_rodo_veic-tp_rod   = wa_zlest0002-tp_rodado.
          wit_mdfe_rodo_veic-tp_car   = wa_zlest0002-tp_carroceria2.
          wit_mdfe_rodo_veic-uf       = wa_zlest0002-cd_uf.
          wit_mdfe_rodo_veic-prop_ref = qtd_proprie.

          CASE wa_zlest0002-tp_veiculo.
            WHEN '0'.
              wit_mdfe_rodo_veic-id                 = 1.
              zis_mdfe_infmodal_rodo-veictracao_ref = wit_mdfe_rodo_veic-id.

              wit_mdfe_rodo_veic-tp_rod = wa_zlest0002-tp_rodado. "'03'. "#137820-10.04.2024-JT
            WHEN '1'.

              wit_mdfe_rodo_veic-id                  = 2.
              zis_mdfe_infmodal_rodo-veicreboque_ref = wit_mdfe_rodo_veic-id.

              IF wit_mdfe_rodo_veic-tp_rod = '0' OR wit_mdfe_rodo_veic-tp_rod = '00'.
                wit_mdfe_rodo_veic-tp_rod = '03'.
              ENDIF.
          ENDCASE.
          APPEND wit_mdfe_rodo_veic TO zit_mdfe_rodo_veic.

        ENDLOOP.
      ENDLOOP.

      "Motorista
      IF wa_zsdt0118-motorista IS NOT INITIAL.

        SELECT SINGLE *
          FROM lfa1 INTO @DATA(wl_lfa1_moto)
         WHERE lifnr = @wa_zsdt0118-motorista.

        wl_lfa1_moto-name1 = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( i_texto = CONV #( wl_lfa1_moto-name1 ) ) ).

        CLEAR: wit_mdfe_rodo_condutor.
        wit_mdfe_rodo_condutor-id  = 1.
        wit_mdfe_rodo_condutor-cpf = wl_lfa1_moto-stcd2.
        wit_mdfe_rodo_condutor-x_nome = wl_lfa1_moto-name1.
        APPEND wit_mdfe_rodo_condutor TO zit_mdfe_rodo_condutor.
      ENDIF.

    WHEN '3'.

      DATA: wa_zlest0066  TYPE zlest0066,
            wa_zlest0061  TYPE zlest0061,
            it_zlest0063  TYPE TABLE OF zlest0063,
            wa_zlest0063  TYPE zlest0063,
            it_zlest0053  TYPE TABLE OF zlest0053,
            wa_zlest0053  TYPE zlest0053,
            it_zlest0056  TYPE TABLE OF zlest0056,
            wa_zlest0056  TYPE zlest0056,
            it_zlest0106  TYPE TABLE OF zlest0106,
            wa_zlest0106  TYPE zlest0106,
            it_j_1bnfdoc  TYPE TABLE OF j_1bnfdoc,
            wa_j_1bnfdoc  TYPE j_1bnfdoc,
            it_j_1bbranch TYPE TABLE OF j_1bbranch,
            wa_j_1bbranch TYPE j_1bbranch.

      "Váriaveis XML
      DATA: vl_tp_emb         TYPE string,
            vl_tp_irin        TYPE string,
            vl_cembar         TYPE string,
            vl_xembar         TYPE string,
            vl_cprtemb        TYPE string,
            vl_cprtdest       TYPE string,
            vl_ctermcarreg    TYPE string,
            vl_xtermcarreg    TYPE string,
            vl_ctermdescarreg TYPE string,
            vl_xtermdescarreg TYPE string,
            vl_cnpjagenav     TYPE string.

      zis_mdfe_ide-modal = '03'.

      LOOP AT it_zsdt0105 INTO wa_zsdt0105.

        "Frete Aquaviário - Ordem de Venda
        CLEAR: wa_zlest0061.
        SELECT SINGLE *
          INTO wa_zlest0061
          FROM zlest0061
         WHERE docnum = wa_zsdt0105-docnum.

        "Busca Dados Doc.
        CLEAR: wa_j_1bnfdoc.
        SELECT SINGLE *
          INTO wa_j_1bnfdoc
          FROM j_1bnfdoc
         WHERE docnum = wa_zsdt0105-docnum.

        IF sy-subrc NE 0.
          MESSAGE 'Não encontrado os dados do documento vinculado ao MDF-e!' TYPE 'E'.
          RETURN.
        ENDIF.

        "Busca CNPJ Matriz
        CLEAR: wa_j_1bbranch.
        SELECT SINGLE *
          INTO wa_j_1bbranch
          FROM j_1bbranch
         WHERE bukrs      EQ  wa_j_1bnfdoc-bukrs
           AND branch     NE '0001'
           AND cgc_branch EQ '1'.

        IF ( sy-subrc NE 0 ) OR ( wa_j_1bbranch-stcd1 IS INITIAL ).
          MESSAGE 'Não encontrado os dados do documento vinculado ao MDF-e!' TYPE 'E'.
          RETURN.
        ENDIF.

        "Frete Aquaviário - Viagem
        CLEAR: wa_zlest0056.
        SELECT SINGLE *
          INTO wa_zlest0056
          FROM zlest0056
         WHERE bukrs       EQ wa_zlest0061-bukrs
           AND werks       EQ wa_zlest0061-werks
           AND ano_viagem  EQ wa_zlest0061-ano_viagem
           AND nr_viagem   EQ wa_zlest0061-nr_viagem.

        IF sy-subrc NE 0.
          MESSAGE 'Não encontrado os dados da viagem!' TYPE 'E'.
          RETURN.
        ENDIF.

        "Cadastro Porto x Ministério Transporte

        "Porto Embarque
        CLEAR: wa_zlest0106.
        SELECT SINGLE *
          INTO wa_zlest0106
          FROM zlest0106
         WHERE cod_porto = wa_zlest0056-po_embarque.

        IF sy-subrc NE 0.
          MESSAGE | Não cadastrado o Porto de Embarque:  { wa_zlest0056-po_embarque } na transação ZLES0112! | TYPE 'E'.
          RETURN.
        ELSE.

          vl_cprtemb     = wa_zlest0106-cod_porto_min.
          vl_ctermcarreg = wa_zlest0106-cod_terminal.
          vl_xtermcarreg = wa_zlest0106-desc_terminal.

          IF vl_cprtemb IS INITIAL.
            MESSAGE | Não cadastrado para Porto de Embarque: {  wa_zlest0056-po_embarque } (Transação ZLES0112), o Código Porto Ministério Transp.!|  TYPE 'E'.
            RETURN.
          ENDIF.

        ENDIF.

        "Porto Destino
        CLEAR: wa_zlest0106.
        SELECT SINGLE *
          INTO wa_zlest0106
          FROM zlest0106
         WHERE cod_porto = wa_zlest0056-po_destino.

        IF sy-subrc NE 0.
          MESSAGE |Não cadastrado o Porto de Destino:  { wa_zlest0056-po_destino } na transação ZLES0112!' | TYPE 'E'.
          RETURN.
        ELSE.

          vl_cprtdest       = wa_zlest0106-cod_porto_min.
          vl_ctermdescarreg = wa_zlest0106-cod_terminal.
          vl_xtermdescarreg = wa_zlest0106-desc_terminal.

          IF vl_cprtdest IS INITIAL.
            MESSAGE |Não cadastrado para Porto de Embarque: { wa_zlest0056-po_destino } (Transação ZLES0112), o Código Porto Ministério Transp.!| TYPE 'E'.
            RETURN.
          ENDIF.

        ENDIF.

        CLEAR: wa_zlest0063.
        SELECT SINGLE *
          INTO wa_zlest0063
          FROM zlest0063
         WHERE bukrs       EQ wa_zlest0061-bukrs
           AND werks       EQ wa_zlest0061-werks
           AND ano_viagem  EQ wa_zlest0061-ano_viagem
           AND nr_viagem   EQ wa_zlest0061-nr_viagem
           AND ( ( embarcacao  EQ 'E') OR
                 ( embarcacao  EQ 'R' ) ).

        IF sy-subrc NE 0.
          MESSAGE 'Não encontrado Empurrador/Rebocador do comboio!' TYPE 'E'.
          RETURN.
        ENDIF.

        CLEAR: wa_zlest0053.
        SELECT SINGLE *
          INTO wa_zlest0053
          FROM zlest0053
         WHERE bukrs      EQ wa_zlest0063-bukrs
           AND nome       EQ wa_zlest0063-nome_emb
           AND embarcacao EQ wa_zlest0063-embarcacao.

        IF sy-subrc NE 0.
          MESSAGE 'Não encontrado cadastro do Empurrador/Rebocador do comboio!' TYPE 'E'.
          RETURN.
        ENDIF.

        "Atribui dados (Rebocador/Empurrador)

        vl_tp_emb  = wa_zlest0053-cod_tp_emb_min. "Cód.Tp Embarcação de acordo com o Ministério Transp.
        vl_tp_irin = wa_zlest0053-irin.
        vl_cembar  = wa_zlest0053-apelido.
        vl_xembar  = wa_zlest0053-nome.

        IF vl_tp_irin IS INITIAL.
          MESSAGE |Não cadastrado o IRIN do Empurrador/Rebocador:  { wa_zlest0053-nome } !| TYPE 'E'.
          RETURN.
        ENDIF.

        IF vl_tp_emb IS INITIAL.
          MESSAGE |Não cadastrado o Código do Tipo da Embarcação(Ministério Transp.) do Empurrador/Rebocador:' { wa_zlest0053-nome } !| TYPE 'E'.
          RETURN.
        ENDIF.

        IF vl_cembar IS INITIAL.
          MESSAGE |Não encontrado o Apelido da Embarcação(Empurrador/Rebocador): { wa_zlest0053-nome } !| TYPE 'E'.
          RETURN.
        ENDIF.

        IF vl_xembar IS INITIAL.
          MESSAGE |Não encontrado o nome da Embarcação(Empurrador/Rebocador): { wa_zlest0053-nome } !| TYPE 'E'.
          RETURN.
        ENDIF.


        EXIT.
      ENDLOOP.

      zis_mdfe_infmodal_aquav-c_embar    = vl_cembar.
      zis_mdfe_infmodal_aquav-c_prt_dest = vl_cprtdest.
      zis_mdfe_infmodal_aquav-c_prt_emb  = vl_cprtemb.
      zis_mdfe_infmodal_aquav-irin       = vl_tp_irin.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zlest0061-nr_viagem
        IMPORTING
          output = wa_zlest0061-nr_viagem.

      zis_mdfe_infmodal_aquav-n_viag     = wa_zlest0061-nr_viagem.

      zis_mdfe_infmodal_aquav-tp_emb     = vl_tp_emb.
      zis_mdfe_infmodal_aquav-x_embar    = vl_xembar.

      "Frete Aquáviário - Comboio
      "Seleção Barcaças
      REFRESH it_zlest0063.

      SELECT *
        FROM zlest0063
        INTO TABLE it_zlest0063
       WHERE bukrs       EQ wa_zlest0061-bukrs
         AND werks       EQ wa_zlest0061-werks
         AND ano_viagem  EQ wa_zlest0061-ano_viagem
         AND nr_viagem   EQ wa_zlest0061-nr_viagem
         AND embarcacao  NE 'E'
         AND embarcacao  NE 'R'.

      "Código da embarcação.
      DATA(count_id_comb) = 1.
      LOOP AT it_zlest0063 INTO wa_zlest0063.
        CLEAR: wa_zlest0053.
        SELECT SINGLE *
          INTO wa_zlest0053
          FROM zlest0053
         WHERE bukrs      EQ wa_zlest0063-bukrs
           AND nome       EQ wa_zlest0063-nome_emb
           AND embarcacao EQ wa_zlest0063-embarcacao.

        IF ( sy-subrc NE 0 ) OR ( wa_zlest0053-apelido IS INITIAL ).
          MESSAGE |Não encontrado o cadastro do Apelido da Embarcação:' { wa_zlest0063-embarcacao } '!'| TYPE 'E'.
          RETURN.
        ENDIF.

        vl_cembar = wa_zlest0053-apelido.

        APPEND VALUE #( id = count_id_comb c_emb_comb = vl_cembar x_balsa =  wa_zlest0063-nome_emb ) TO zit_mdfe_infembcomb.
        ADD 1 TO count_id_comb.
      ENDLOOP.

      IF ( vl_ctermcarreg    IS NOT INITIAL ) AND
         ( vl_ctermdescarreg IS NOT INITIAL ).

        APPEND VALUE #( id = 1 c_term_carreg    = vl_ctermcarreg x_term_carreg  = vl_xtermcarreg )         TO zit_mdfe_inftermcarreg.
        APPEND VALUE #( id = 1 c_term_descarreg = vl_ctermdescarreg x_term_descarreg = vl_xtermdescarreg ) TO zit_mdfe_inftermdescarreg.

      ENDIF.

    WHEN '4'.
      zis_mdfe_ide-modal = '04'.
  ENDCASE.

  zis_mdfe_ide-dh_emi = wk_header-docdat.
  zis_mdfe_ide-ind_canal_verde = '1'.

  CALL FUNCTION 'J_1B_BRANCH_READ'
    EXPORTING
      branch            = branch
      company           = bukrs
    IMPORTING
      address           = ls_sadr
      branch_record     = ls_branch
      cgc_number        = lv_cgc
      address_value     = ls_addr1_val
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  IF xmlh_310-hemi IS INITIAL OR strlen( xmlh_310-hemi ) = 0.
    "PERFORM CONVERT_UTC_TO_TIME USING LS_ADDR1_VAL-TIME_ZONE CHANGING LV_TIME.
    CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
      EXPORTING
        i_bukrs  = bukrs
        i_branch = branch
      IMPORTING
        e_time   = e_time.

    IF e_time IS INITIAL.
      lv_time = sy-timlo.
    ELSE.
      lv_time = e_time.
    ENDIF.

  ELSE.
    lv_time = xmlh_310-hemi.
  ENDIF.

  CONCATENATE
  zis_mdfe_ide-dh_emi
  lv_time
  INTO zis_mdfe_ide-dh_emi.

  "PERFORM CONVERT_TIMESPAN_TO_UTC USING LS_ADDR1_VAL-TIME_ZONE CHANGING ZIS_MDFE_IDE-DH_EMI.

  "GET TIME STAMP FIELD ZIV_TIMESTAMP.

  zis_mdfe_ide-dh_iniviagem = zis_mdfe_ide-dh_emi.

  zis_mdfe_emit-cnpj     = wk_header-cnpj_bupla.
  zis_mdfe_emit-ie       = wk_header-ie_bupla.
  zis_mdfe_emit-x_nome   = xmlh-c_xnome.
  zis_mdfe_emit-x_fant   = xmlh-c_xfant.
  zis_mdfe_emit-x_lgr    = xmlh-c1_xlgr.
  zis_mdfe_emit-nro      = xmlh-c1_nro.
  zis_mdfe_emit-x_bairro = xmlh-c1_xbairro.
  zis_mdfe_emit-c_mun    = xmlh-c1_cmun.
  zis_mdfe_emit-x_mun    = xmlh-c1_xmun.
  zis_mdfe_emit-uf       = xmlh-c1_uf.
  zis_mdfe_emit-fone     = xmlh-c1_fone.
  zis_mdfe_emit-cep      = xmlh-c1_cep.

  SELECT * INTO TABLE @DATA(it_zsdt0104)
    FROM zsdt0104
   WHERE docnum EQ @xmlh-docnum.

  SORT it_zsdt0104 BY ordem_uf ASCENDING.

  LOOP AT it_zsdt0104 INTO DATA(wa_zsdt0104).
    IF NOT ( wa_zsdt0104-uf IS INITIAL ).
      CLEAR: wat_mdfe_inf_percurso.
      wat_mdfe_inf_percurso-id    = sy-tabix.
      wat_mdfe_inf_percurso-ufper = wa_zsdt0104-uf.
      APPEND wat_mdfe_inf_percurso TO zit_mdfe_inf_percurso.
    ENDIF.
  ENDLOOP.

  IF it_zsdt0105[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_doc_eletronico)
      FROM j_1bnfe_active
       FOR ALL ENTRIES IN @it_zsdt0105
     WHERE docnum EQ @it_zsdt0105-docnum.
  ENDIF.

  CLEAR: wit_mdfe_infcte, wit_mdfe_infnfe.

  wit_mdfe_infcte-id     = 1.
  wit_mdfe_infnfe-id     = 1.
  v_seqno_ref_cte_nfe    = 0.

  SELECT * INTO TABLE @DATA(gt_zsdt0239)
    FROM zsdt0239
   WHERE docnum EQ @wk_header-docnum.

  SELECT * INTO TABLE @DATA(gt_zsdt0241)
    FROM zsdt0241
   WHERE docnum EQ @wk_header-docnum.

  LOOP AT gt_zsdt0239 INTO DATA(wa_zsdt0239).

    CLEAR: wit_mdfe_inf_mundescarg.

    DATA(lc_tabix) = sy-tabix.

    SELECT SINGLE text INTO @zde_zsdt0239-text
      FROM j_1btxjurt
     WHERE spras      EQ @sy-langu
       AND country    EQ @wa_zsdt0239-country
       AND taxjurcode EQ @wa_zsdt0239-cmundesc.

    wit_mdfe_inf_mundescarg-id             = lc_tabix.
    wit_mdfe_inf_mundescarg-c_mun_descarga = wa_zsdt0239-cmundesc.
    wit_mdfe_inf_mundescarg-x_mun_descarga = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( CONV #( zde_zsdt0239-text ) ) ).
    wit_mdfe_inf_mundescarg-infcte_ref     = lc_tabix.
    wit_mdfe_inf_mundescarg-infnfe_ref     = lc_tabix.
    APPEND wit_mdfe_inf_mundescarg TO zit_mdfe_inf_mundescarg.

    LOOP AT gt_zsdt0241 INTO DATA(wa_zsdt0241) WHERE sequencia EQ wa_zsdt0239-sequencia.

      ADD 1 TO v_seqno_ref_cte_nfe.

      CASE wa_zsdt0241-chave+20(2).
        WHEN '55'.
          wit_mdfe_infnfe-seq_no = v_seqno_ref_cte_nfe.
          wit_mdfe_infnfe-ch_nfe = wa_zsdt0241-chave.
          wit_mdfe_infnfe-id     = lc_tabix.
          APPEND wit_mdfe_infnfe TO zit_mdfe_infnfe.
          ADD 1 TO wis_mdfe_tot-q_nfe.
        WHEN '57'.
          wit_mdfe_infcte-seq_no = v_seqno_ref_cte_nfe.
          wit_mdfe_infcte-ch_cte = wa_zsdt0241-chave.
          wit_mdfe_infcte-id     = lc_tabix.
          APPEND wit_mdfe_infcte TO zit_mdfe_infcte.
          ADD 1 TO wis_mdfe_tot-q_cte.
      ENDCASE.

    ENDLOOP.
  ENDLOOP.

  LOOP AT it_doc_eletronico INTO DATA(wa_doc_eletronico).

    IF wa_doc_eletronico-docnum IS NOT INITIAL AND wa_doc_eletronico-form IS NOT INITIAL.
      TRY .
          zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum =  wa_doc_eletronico-docnum
            )->set_registro( EXPORTING i_docnum = wa_doc_eletronico-docnum i_sem_bloqueio = abap_true
            )->get_ck_doc_nao_cancel(
            ).
        CATCH zcx_doc_eletronico INTO DATA(ex_erro).    " .
          zev_error_status      = 'V'.
          wa_retorno-type       = ex_erro->msgty.
          wa_retorno-id         = ex_erro->msgid.
          wa_retorno-number     = ex_erro->msgno.
          wa_retorno-message_v1 = ex_erro->msgv1.
          wa_retorno-message_v2 = ex_erro->msgv2.
          wa_retorno-message_v3 = ex_erro->msgv3.
          wa_retorno-message_v4 = ex_erro->msgv4.
          APPEND wa_retorno TO zet_bapiret2.

          MESSAGE ID  ex_erro->msgid TYPE ex_erro->msgty NUMBER ex_erro->msgno
             WITH ex_erro->msgv1 ex_erro->msgv2 ex_erro->msgv3 ex_erro->msgv4
          RAISING rfc_error.
      ENDTRY.
    ENDIF.

    CASE wa_zsdt0102-tp_doc_ref.
      WHEN '1'.

        ADD 1 TO v_seqno_ref_cte_nfe.

        wit_mdfe_infcte-seq_no = v_seqno_ref_cte_nfe.
        wit_mdfe_infcte-ch_cte = wa_doc_eletronico-regio && wa_doc_eletronico-nfyear && wa_doc_eletronico-nfmonth && wa_doc_eletronico-stcd1 && wa_doc_eletronico-model &&
                                 wa_doc_eletronico-serie && wa_doc_eletronico-nfnum9 && wa_doc_eletronico-docnum9 && wa_doc_eletronico-cdv.
        APPEND wit_mdfe_infcte TO zit_mdfe_infcte.

        "Erro de atribuição: nenhuma linha encontrada na tabela IT_MDFE_INF_MUNDESCARG
        CLEAR: wit_mdfe_inf_mundescarg.

        READ TABLE zit_mdfe_inf_mundescarg INTO DATA(wl_mdfe_inf_descarga_aux) WITH KEY c_mun_descarga = wa_zsdt0102-cmunfim.
        IF sy-subrc NE 0.
          wa_zsdt0102-nmunfim = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( CONV #( wa_zsdt0102-nmunfim ) ) ).
          wit_mdfe_inf_mundescarg-id             = wit_mdfe_infcte-id.
          wit_mdfe_inf_mundescarg-c_mun_descarga = wa_zsdt0102-cmunfim.
          wit_mdfe_inf_mundescarg-x_mun_descarga = wa_zsdt0102-nmunfim.
          wit_mdfe_inf_mundescarg-infcte_ref     = wit_mdfe_infcte-id.
          APPEND wit_mdfe_inf_mundescarg TO zit_mdfe_inf_mundescarg.
        ENDIF.

        ADD 1 TO wis_mdfe_tot-q_cte.

      WHEN '2'.

        ADD 1 TO v_seqno_ref_cte_nfe.

        wit_mdfe_infnfe-seq_no = v_seqno_ref_cte_nfe.
        wit_mdfe_infnfe-ch_nfe = wa_doc_eletronico-regio && wa_doc_eletronico-nfyear && wa_doc_eletronico-nfmonth && wa_doc_eletronico-stcd1 && wa_doc_eletronico-model &&
                                 wa_doc_eletronico-serie && wa_doc_eletronico-nfnum9 && wa_doc_eletronico-docnum9 && wa_doc_eletronico-cdv.
        APPEND wit_mdfe_infnfe TO zit_mdfe_infnfe.

        "Erro de atribuição: nenhuma linha encontrada na tabela IT_MDFE_INF_MUNDESCARG
        CLEAR: wit_mdfe_inf_mundescarg.

        READ TABLE zit_mdfe_inf_mundescarg INTO wl_mdfe_inf_descarga_aux WITH KEY c_mun_descarga = wa_zsdt0102-cmunfim.
        IF sy-subrc NE 0.
          wa_zsdt0102-nmunfim = zcl_string=>tira_acentos( zcl_string=>convert_to_utf8( CONV #( wa_zsdt0102-nmunfim ) ) ).
          wit_mdfe_inf_mundescarg-id             = wit_mdfe_infnfe-id.
          wit_mdfe_inf_mundescarg-c_mun_descarga = wa_zsdt0102-cmunfim.
          wit_mdfe_inf_mundescarg-x_mun_descarga = wa_zsdt0102-nmunfim.
          wit_mdfe_inf_mundescarg-infnfe_ref     = wit_mdfe_infnfe-id.
          APPEND wit_mdfe_inf_mundescarg TO zit_mdfe_inf_mundescarg.
        ENDIF.

        ADD 1 TO wis_mdfe_tot-q_nfe.

    ENDCASE.
  ENDLOOP.

  wis_mdfe_tot-c_unid  = wa_zsdt0118-cunid.
  wis_mdfe_tot-q_carga = wa_zsdt0118-qcarga.
  wis_mdfe_tot-v_carga = wa_zsdt0118-vcarga.

  "Erro de atribuição: nenhuma linha encontrada na tabela IT_MDFE_INF_MUNCARREGA
  "Erro de atribuição: nenhuma linha encontrada na tabela IT_MDFE_INF_MUNDESCARG
  DATA(out_tech_resp) = zcl_im_cl_nfe_print=>get_resp_tecnico( i_uf  = xmlh-c1_uf i_bukrs = wk_header-bukrs i_branch = wk_header-branch ).
  zis_mdfe_infresptec-cnpj      = out_tech_resp-cnpj.
  zis_mdfe_infresptec-email     = out_tech_resp-email.
  zis_mdfe_infresptec-fone      = out_tech_resp-fone.
  zis_mdfe_infresptec-x_contato = out_tech_resp-x_contato.
  zis_mdfe_infresptec-id_csrt   = out_tech_resp-id_csrt.
  zis_mdfe_infresptec-hash_csrt = out_tech_resp-csrt.

  "CNPJ/CPF Autorizados Download
  DATA: it_zsdt0228 TYPE TABLE OF zsdt0228.

  CLEAR: it_zsdt0228[], v_line_id.
  SELECT *
    FROM zsdt0228 INTO TABLE it_zsdt0228
   WHERE bukrs EQ wk_header-bukrs.

  LOOP AT it_zsdt0228 INTO DATA(wl_zsdt0228).
    ADD 1 TO v_line_id.
    APPEND VALUE #( id = v_line_id cnpj = wl_zsdt0228-stcd1 ) TO zit_mdfe_autxml.
  ENDLOOP.

  "Informações de Produto Predominante
  zis_mdfe_prodpred-tpcarga            = wa_zsdt0102-tpcarga.
  zis_mdfe_prodpred-xprod              = wa_zsdt0102-xprod.
  DESCRIBE TABLE zit_mdfe_infcte LINES DATA(qtcte).
  DESCRIBE TABLE zit_mdfe_infnfe LINES DATA(qtnfe).

  qtcte = qtcte + qtnfe.

  IF qtcte EQ 1.
    zis_mdfe_prodpred-cep_carga        = wa_zsdt0102-locc_cep.
    zis_mdfe_prodpred-cep_descarga     = wa_zsdt0102-locd_cep.

    "IR253973 - Preenchimento logitude latitude - WPP --->>
*    IF zis_mdfe_prodpred-cep_carga IS NOT INITIAL.
*      lc_latitude  = wa_zsdt0102-locc_latitude.
*      WRITE abs( lc_latitude ) TO lc_coordenada_la.
*      CONDENSE lc_coordenada_la NO-GAPS.
*      IF lc_latitude LT 0.
*        lc_coordenada_la = '-' && lc_coordenada_la.
*      ENDIF.
*      zis_mdfe_prodpred-latitude_carga = zcl_string=>replace( i_str = CONV #( lc_coordenada_la ) i_char_old   = ',' i_char_new   = '.' ) .
*
*      lc_longitude = wa_zsdt0102-locc_longitude.
*      WRITE abs( lc_longitude ) TO lc_coordenada_lo.
*      CONDENSE lc_coordenada_lo NO-GAPS.
*      IF lc_longitude LT 0.
*        lc_coordenada_lo = '-' && lc_coordenada_lo.
*      ENDIF.
*      zis_mdfe_prodpred-longitude_carga = zcl_string=>replace( i_str = CONV #( lc_coordenada_lo ) i_char_old   = ',' i_char_new   = '.' ) .
*    ENDIF.
*
*    IF zis_mdfe_prodpred-cep_descarga IS NOT INITIAL.
*      lc_latitude  = wa_zsdt0102-locd_latitude.
*      WRITE abs( lc_latitude ) TO lc_coordenada_la.
*      CONDENSE lc_coordenada_la NO-GAPS.
*      IF lc_latitude LT 0.
*        lc_coordenada_la = '-' && lc_coordenada_la.
*      ENDIF.
*      zis_mdfe_prodpred-latitude_descarga  = zcl_string=>replace( i_str = CONV #( lc_coordenada_la ) i_char_old   = ',' i_char_new   = '.' ) .
*
*      lc_longitude  = wa_zsdt0102-locd_longitude.
*      WRITE abs( lc_longitude ) TO lc_coordenada_lo.
*      CONDENSE lc_coordenada_lo NO-GAPS.
*      IF lc_longitude LT 0.
*        lc_coordenada_lo = '-' && lc_coordenada_lo.
*      ENDIF.
*      zis_mdfe_prodpred-longitude_descarga = zcl_string=>replace( i_str = CONV #( lc_coordenada_lo ) i_char_old   = ',' i_char_new   = '.' ) .
*    ENDIF.
    "IR253973 - Preenchimento logitude latitude - WPP <<<---

  ENDIF.

**********************************************************************
*  CS2021000244 Ajustar regra para informação de TAG <prop> XML MDFe
**********************************************************************
  IF zis_mdfe_ide-modal = '01'. " Rodoviario
    IF zis_mdfe_ide-tp_emit = '2'."Frota própria
      CLEAR zis_mdfe_ide-tp_transp. "Elimina tag <tpTransp>
      IF zit_mdfe_rodo_prop[] IS NOT INITIAL.
        READ TABLE zit_mdfe_rodo_prop INTO wit_mdfe_rodo_prop INDEX 1.
        IF wit_mdfe_rodo_prop-cnpj+0(8) = zis_mdfe_emit-cnpj+0(8). " Emitente e proprietario veiculo da mesma empresa
          REFRESH zit_mdfe_rodo_prop. " Não envia a TAG <prop>
        ENDIF.
      ENDIF.
    ELSEIF zis_mdfe_ide-tp_emit = '1'. " Prestador de Serviço de Transporte

      zis_mdfe_ide-tp_transp = '1'.

      "Regra do CS2021001284 - Ini
      READ TABLE it_prop INTO wa_prop INDEX 1.
      IF sy-subrc EQ 0.
        SELECT SINGLE *
          FROM lfa1 INTO wl_lfa1_prop
         WHERE lifnr = wa_prop-proprietario.

        IF ( sy-subrc EQ 0 ) AND ( wl_lfa1_prop-stkzn IS NOT INITIAL ). "Pessoa Fisica
          zis_mdfe_ide-tp_transp = '2'. "Tac
        ENDIF.
      ENDIF.
      "Regra do CS2021001284 - Fim

    ENDIF.

    "Rejeição 741
    IF zit_mdfe_rodo_prop[] IS NOT INITIAL. " AND sy-sysid = 'QAS'. "Regra apenas para validar QAS / PRD apenas 02.08.2021
      REFRESH: zit_mdfe_rodo_inf_contratante. "CNPJ deve ser do contratante
      CLEAR: wit_mdfe_rodo_inf_contratante.
      wit_mdfe_rodo_inf_contratante-id    = 1.
      wit_mdfe_rodo_inf_contratante-xnome = zis_mdfe_emit-x_nome.
      IF zis_mdfe_emit-cnpj IS NOT INITIAL.
        wit_mdfe_rodo_inf_contratante-cnpj  = zis_mdfe_emit-cnpj.
      ELSE.
        wit_mdfe_rodo_inf_contratante-cpf   = zis_mdfe_emit-cpf.
      ENDIF.

      APPEND wit_mdfe_rodo_inf_contratante TO zit_mdfe_rodo_inf_contratante.
    ENDIF.
**<<<------"185281 - NMS - INI------>>>
**<<<------"192137 - NMS - INI------>>>
    SELECT SINGLE low FROM tvarvc INTO @DATA(vl_tag_yes) WHERE name EQ 'MDFE_ATIVA_TAG' AND type EQ 'P'.

    IF     sy-subrc IS INITIAL  AND
       NOT vl_tag_yes IS INITIAL.
**<<<------"192137 - NMS - FIM------>>>
      IF zis_mdfe_ide-tp_emit NE '2' AND  "Frota própria
         qtcte                EQ 1.
*** Produto predominante - NCM.
        SELECT SINGLE c~steuc, b~docnum
          FROM zsdt0105 AS a
           INNER JOIN zcte_info_nota AS b
            ON a~docnum EQ b~docnum
           INNER JOIN marc AS c
            ON b~material EQ c~matnr
          INTO ( @DATA(vl_ncm), @DATA(vl_docnum_ori) )
        WHERE a~docnum_ref EQ @xmlh-docnum.

        IF sy-subrc IS INITIAL.
          TRANSLATE vl_ncm USING '. '.
          CONDENSE  vl_ncm NO-GAPS.
          zis_mdfe_prodpred-ncm = vl_ncm.

        ELSE.
          CLEAR zis_mdfe_prodpred-ncm.

        ENDIF.
*** Informações do Pagamento do Frete
        wit_mdfe_rodo_inf_pagamento-cnpj  = zis_mdfe_emit-cnpj.
        wit_mdfe_rodo_inf_Pagamento-xnome = zis_mdfe_emit-x_nome.
        APPEND wit_mdfe_rodo_inf_Pagamento TO zit_mdfe_rodo_inf_pagamento.
*** Componentes do Pagamento do Frete
        SELECT SINGLE * FROM zcte_ciot INTO @DATA(el_ciot) WHERE docnum EQ @vl_docnum_ori.
        IF sy-subrc IS INITIAL.
**<<<------"192137 - NMS - INI------>>>
** Tipo do Componentes do Pagamento do Frete - Vale Pedágio
*          IF NOT el_ciot-vlr_pedagio IS INITIAL.
*            wit_mdfe_rodo_inf_cmp_t_s-tp_comp = 1.
*            wit_mdfe_rodo_inf_cmp_t_s-v_comp  = el_ciot-vlr_pedagio.
*            APPEND wit_mdfe_rodo_inf_cmp_t_s TO wit_mdfe_rodo_inf_comp-tipo.
*            CLEAR  wit_mdfe_rodo_inf_cmp_t_s.
*
*          ENDIF.
***<<<------"192137 - NMS - FIM------>>>
* Tipo do Componentes do Pagamento do Frete - Impostos, taxas e contribuições
          IF NOT el_ciot-vlr_impostos IS INITIAL.
            wit_mdfe_rodo_inf_cmp_t_s-tp_comp = 2.
            wit_mdfe_rodo_inf_cmp_t_s-v_comp  = el_ciot-vlr_impostos.
            APPEND wit_mdfe_rodo_inf_cmp_t_s TO wit_mdfe_rodo_inf_comp-tipo.
            CLEAR  wit_mdfe_rodo_inf_cmp_t_s.

          ENDIF.
* Tipo do Componentes do Pagamento do Frete - Despesas (bancárias, meios de pagamento, outras)
          IF NOT el_ciot-vlr_seguro IS INITIAL.
            wit_mdfe_rodo_inf_cmp_t_s-tp_comp = 3.
            wit_mdfe_rodo_inf_cmp_t_s-v_comp  = el_ciot-vlr_seguro.
            APPEND wit_mdfe_rodo_inf_cmp_t_s TO wit_mdfe_rodo_inf_comp-tipo.
            CLEAR  wit_mdfe_rodo_inf_cmp_t_s.

          ENDIF.
* Tipo do Componentes do Pagamento do Frete - Frete
**<<<------"192137 - NMS - INI------>>>
*          wit_mdfe_rodo_inf_cmp_t_s-v_comp  = el_ciot-vlr_frete - el_ciot-vlr_pedagio - el_ciot-vlr_impostos - el_ciot-vlr_seguro.
          wit_mdfe_rodo_inf_cmp_t_s-v_comp  = el_ciot-vlr_frete - el_ciot-vlr_impostos - el_ciot-vlr_seguro.
***<<<------"192137 - NMS - FIM------>>>
          wit_mdfe_rodo_inf_cmp_t_s-tp_comp = 4.
          APPEND wit_mdfe_rodo_inf_cmp_t_s TO wit_mdfe_rodo_inf_comp-tipo.
          CLEAR  wit_mdfe_rodo_inf_cmp_t_s.
* Frete - Valor total do Contrato.
          wit_mdfe_rodo_inf_comp-v_contrato  = el_ciot-vlr_frete.
          wit_mdfe_rodo_inf_comp-ind_pag     = 0.
          APPEND wit_mdfe_rodo_inf_comp TO zit_mdfe_rodo_inf_comp.
          CLEAR  wit_mdfe_rodo_inf_comp.

        ENDIF.

      ENDIF.
* Informações TAC e equiparado a TAC.
**<<<------"192137 - NMS - INI------>>>
*      IF NOT zis_mdfe_infmodal_rodo-rntrc IS INITIAL.
      IF NOT zis_mdfe_infmodal_rodo-rntrc IS INITIAL. "AND
*             wit_mdfe_rodo_prop-tp_prop   NE '2'.        "Outros.
**<<<------"192137 - NMS - FIM------>>>
        SELECT SINGLE * FROM zcte_identifica INTO @DATA(el_identifica) WHERE docnum EQ @vl_docnum_ori.
        IF sy-subrc IS INITIAL                 AND
           el_identifica-tp_admim_frete = '09'.
          wit_mdfe_rodo_inf_banco-cnpjipef = '10314753000125'.
          APPEND wit_mdfe_rodo_inf_banco TO zit_mdfe_rodo_inf_banco.
          CLEAR  wit_mdfe_rodo_inf_banco.

        ENDIF.

      ENDIF.
**<<<------"192137 - NMS - INI------>>>
    ENDIF.
**<<<------"192137 - NMS - FIM------>>>
**<<<------"185281 - NMS - FIM------>>>
  ENDIF.
  "
**********************************************************************
*  CS2021000244 Ajustar regra para informação de TAG <prop> XML MDFe - FIm
**********************************************************************

  DATA(_lva_send_to_cloud) = abap_false.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc)
   WHERE name = 'BRANCH_DRC'
      AND low = @branch.
  IF sy-subrc EQ 0 .
    _lva_send_to_cloud = abap_true.
  ENDIF.

  IF _lva_send_to_cloud = abap_true.

    PERFORM f_send_mdfe_to_cloud USING wk_header
                                       zis_mdfe_header
                                       zit_mdfe_text
                                       zis_mdfe_ide
                                       zit_mdfe_inf_percurso
                                       zit_mdfe_inf_muncarrega
                                       zis_mdfe_emit
                                       zis_mdfe_infmodal_aquav
                                       zit_mdfe_inftermcarreg
                                       zit_mdfe_inftermdescarreg
                                       zit_mdfe_infembcomb
                                       zit_mdfe_infunidcargavazia
                                       zit_mdfe_infunidtranspvazia
                                       zis_mdfe_infmodal_rodo
                                       zit_mdfe_rodo_veic
                                       zit_mdfe_rodo_prop
                                       zit_mdfe_rodo_condutor
                                       zit_mdfe_rodo_inf_ciot
                                       zit_mdfe_rodo_disp
                                       zit_mdfe_rodo_inf_contratante
**<<<------"185281 - NMS - INI------>>>
                                       zit_mdfe_rodo_inf_pagamento
                                       zit_mdfe_rodo_inf_comp
                                       zit_mdfe_rodo_inf_banco
**<<<------"185281 - NMS - FIM------>>>
                                       zit_mdfe_inf_mundescarg
                                       zit_mdfe_infcte
                                       zit_mdfe_infnfe
                                       zit_mdfe_infmdfetransp
                                       zit_mdfe_infunidtransp
                                       zit_mdfe_infunidcarga
                                       zit_mdfe_seg
                                       wis_mdfe_tot
                                       zit_mdfe_autxml
                                       zis_mdfe_infadic
                                       zis_mdfe_prodpred
                                       zis_mdfe_infresptec
                                       ziv_text_id_lacre
                                       ziv_resend.
  ELSE.

    CALL FUNCTION '/XNFE/OUTMDFE_58_CREATE'
      DESTINATION lv_rfcdest_mdfe
      EXPORTING
        is_mdfe_header               = zis_mdfe_header
        it_mdfe_text                 = zit_mdfe_text
        is_mdfe_ide                  = zis_mdfe_ide
        it_mdfe_inf_percurso         = zit_mdfe_inf_percurso
        it_mdfe_inf_muncarrega       = zit_mdfe_inf_muncarrega
        is_mdfe_emit                 = zis_mdfe_emit
        is_mdfe_infmodal_aquav       = zis_mdfe_infmodal_aquav
        it_mdfe_inftermcarreg        = zit_mdfe_inftermcarreg
        it_mdfe_inftermdescarreg     = zit_mdfe_inftermdescarreg
        it_mdfe_infembcomb           = zit_mdfe_infembcomb
        it_mdfe_infunidcargavazia    = zit_mdfe_infunidcargavazia
        it_mdfe_infunidtranspvazia   = zit_mdfe_infunidtranspvazia
        is_mdfe_infmodal_rodo        = zis_mdfe_infmodal_rodo
        it_mdfe_rodo_veic            = zit_mdfe_rodo_veic
        it_mdfe_rodo_prop            = zit_mdfe_rodo_prop
        it_mdfe_rodo_condutor        = zit_mdfe_rodo_condutor
        it_mdfe_rodo_inf_ciot        = zit_mdfe_rodo_inf_ciot
        it_mdfe_rodo_disp            = zit_mdfe_rodo_disp
        it_mdfe_rodo_inf_contratante = zit_mdfe_rodo_inf_contratante
        it_mdfe_inf_mundescarg       = zit_mdfe_inf_mundescarg
        it_mdfe_infcte               = zit_mdfe_infcte
        it_mdfe_infnfe               = zit_mdfe_infnfe
        it_mdfe_infmdfetransp        = zit_mdfe_infmdfetransp
        it_mdfe_infunidtransp        = zit_mdfe_infunidtransp
        it_mdfe_infunidcarga         = zit_mdfe_infunidcarga
        it_mdfe_seg                  = zit_mdfe_seg
        is_mdfe_tot                  = wis_mdfe_tot
        it_mdfe_autxml               = zit_mdfe_autxml
        is_mdfe_infadic              = zis_mdfe_infadic
        is_mdfe_prodpred             = zis_mdfe_prodpred
        is_mdfe_infresptec           = zis_mdfe_infresptec
        iv_text_id_lacre             = ziv_text_id_lacre
        iv_resend                    = ziv_resend
      IMPORTING
        ev_error_status              = zev_error_status
        et_bapiret2                  = zet_bapiret2
      EXCEPTIONS
        communication_failure        = 1
        system_failure               = 2.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING communication_failure.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING system_failure.
      WHEN OTHERS.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING system_failure.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFUNCTION.
