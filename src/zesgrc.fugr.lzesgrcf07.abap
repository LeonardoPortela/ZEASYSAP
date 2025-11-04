*----------------------------------------------------------------------*
***INCLUDE LZGRCF07.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data      |Request   |Autor       |Alteração                         *
*----------------------------------------------------------------------*
* 14/08/2025|DEVK9A2QU1|NSEGATIN    |Implementação da Nota Técnica     *
*                                   |2025.001 MDFE Versão 1.02.        *
*                                   |Chamado: 185281.                  *
*----------------------------------------------------------------------*
FORM  f_send_mdfe_to_cloud  USING   p_j_1bnfdoc                             TYPE j_1bnfdoc                                "ok
                                    zis_mdfe_header                         TYPE zxnfe_if_mdfe_header_s                   "ok
                                    zit_mdfe_text                           TYPE zxnfe_if_mdfe_text_t                     "ok
                                    zis_mdfe_ide                            TYPE zxnfe_if_mdfe_ide_300_s                  "ok
                                    zit_mdfe_inf_percurso                   TYPE zxnfe_if_mdfe_inf_percurso_t             "ok
                                    zit_mdfe_inf_muncarrega                 TYPE zxnfe_if_mdfe_inf_muncarrega_t           "ok
                                    zis_mdfe_emit                           TYPE zxnfe_if_mdfe_emit_s                     "ok

                                    zis_mdfe_infmodal_aquav                 TYPE zxnfe_if_mdfe_aquav_300_s                "ok
                                    zit_mdfe_inftermcarreg                  TYPE zxnfe_if_mdfe_inftermcarreg_t            "ok
                                    zit_mdfe_inftermdescarreg               TYPE zxnfe_if_mdfe_inftermdescarr_t           "ok
                                    zit_mdfe_infembcomb                     TYPE zxnfe_if_mdfe_infembcomb_300_t           "ok
                                    zit_mdfe_infunidcargavazia              TYPE zxnfe_if_mdfe_infunidcargava_t           "ok
                                    zit_mdfe_infunidtranspvazia             TYPE zxnfe_if_mdfe_infunidtranspv_t           "ok

                                    zis_mdfe_infmodal_rodo                  TYPE zxnfe_if_mdfe_rodo_300_s                 "ok
                                    zit_mdfe_rodo_veic                      TYPE zxnfe_if_mdfe_rodo_veic_t                "ok
                                    zit_mdfe_rodo_prop                      TYPE zxnfe_if_mdfe_rodo_prop_t                "ok
                                    zit_mdfe_rodo_condutor                  TYPE zxnfe_if_mdfe_rodo_condutor_t            "ok
                                    zit_mdfe_rodo_inf_ciot                  TYPE zxnfe_if_mdfe_rodo_inf_ciot_t            "ok
                                    zit_mdfe_rodo_disp                      TYPE zxnfe_if_mdfe_rodo_disp_300_t            "ok
                                    zit_mdfe_rodo_inf_contratante           TYPE zxnfe_if_mdfe_rodo_inf_cont_t            "ok
**<<<------"185281 - NMS - INI------>>>
                                    zit_mdfe_rodo_inf_pagamento             TYPE zxnfe_if_mdfe_rodo_inf_cont_t            "ok
                                    zit_mdfe_rodo_inf_comp                  TYPE zxnfe_if_mdfe_rodo_inf_comp_t            "ok
                                    zit_mdfe_rodo_inf_banco                 TYPE zxnfe_if_mdfe_rodo_inf_banco_t           "ok
**<<<------"185281 - NMS - FIM------>>>
                                    zit_mdfe_inf_mundescarg                 TYPE zxnfe_if_mdfe_inf_mundescarg_t           "ok
                                    zit_mdfe_infcte                         TYPE zxnfe_if_mdfe_infcte_300_t               "ok
                                    zit_mdfe_infnfe                         TYPE zxnfe_if_mdfe_infnfe_300_t               "ok

                                    zit_mdfe_infmdfetransp                  TYPE zxnfe_if_mdfe_infmdfetra_300_t           "ok
                                    zit_mdfe_infunidtransp                  TYPE zxnfe_if_cte_infunidtransp_t             "ok
                                    zit_mdfe_infunidcarga                   TYPE zxnfe_if_cte_infunidcarga_t              "ok
                                    zit_mdfe_seg                            TYPE zxnfe_if_mdfe_seg_t                      "ok
                                    wis_mdfe_tot                            TYPE zxnfe_if_mdfe_tot_s                      "ok
                                    zit_mdfe_autxml                         TYPE zxnfe_if_mdfe_autxml_t                   "ok
                                    zis_mdfe_infadic                        TYPE zxnfe_if_mdfe_infadic_s                  "ok
                                    zis_mdfe_prodpred                       TYPE zrsi_mdfe_prodpred                       "ok
                                    zis_mdfe_infresptec                     TYPE zxnfe_if_mdfe_infresptec_s               "ok
                                    ziv_text_id_lacre                       TYPE zxnfe_text_ref                           "ok
                                    ziv_resend                              TYPE zxnfe_resend.

  DATA: lob_json_transformer_double TYPE REF TO if_nfe_json_transformation,
        lob_com_factory_double      TYPE REF TO if_nfe_cloud_com_factory,
        lob_communicator_double     TYPE REF TO if_nfe_cloud_com,
        lob_download_service_double TYPE REF TO if_nfe_cloud_download_service,
        lob_mdfe_processor          TYPE REF TO cl_nfe_cloud_mdfe_processor.

  DATA: lwa_authorize          TYPE nfe_cloud_mdfe_map_auth,
        lwa_company_definition TYPE j_1bnfe_s_company_code_def.

  CREATE OBJECT lob_mdfe_processor.

  DATA: lwa_unoload_info  TYPE nfe_cloud_mdfe_map_unload_info.
  DATA: lit_info_cte      TYPE nfe_cloud_mdfe_map_cte_info_tt.
  DATA: lit_info_nfe      TYPE nfe_cloud_mdfe_map_nfe_info_tt.

*-CONTINGENCIA MDF-E - JT - 06.05.2024 ==================
  DATA: lo_service_locator      TYPE REF TO cl_j_1bnfe_cf_service_loc,
        lo_cloud_mdfe_processor TYPE REF TO cl_nfe_cloud_mdfe_processor.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 ==================

*  lob_com_factory_double      ?= cl_abap_testdouble=>create( 'IF_NFE_CLOUD_COM_FACTORY' ).
*  lob_communicator_double     ?= cl_abap_testdouble=>create( 'IF_NFE_CLOUD_COM' ).
*  lob_download_service_double ?= cl_abap_testdouble=>create( 'IF_NFE_CLOUD_DOWNLOAD_SERVICE' ).

*  CREATE OBJECT lob_json_transformer_double TYPE td_nfe_json_transformation.
*
*  cl_abap_testdouble=>configure_call( lob_com_factory_double )->returning( lob_communicator_double ).
*  lob_com_factory_double->create( ).
*
*  lob_mdfe_processor = NEW #( io_com_factory      = lob_com_factory_double
*                              io_json_transformer = lob_json_transformer_double
*                              io_download_service = lob_download_service_double ).


*      "CONTINGENCIA MDF-E  Ignorar Validações de Reenvio Contingencia
  SELECT SINGLE *
     FROM tvarvc INTO @DATA(lw_IGN_VAL_RESND_CONTI)
    WHERE name EQ 'ZRDC_IGN_VAL_RESND_CONTI'
      AND low  EQ @sy-uname.


  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(lwa_active)
   WHERE docnum = @p_j_1bnfdoc-docnum.



  IF sy-subrc NE 0.
    MESSAGE |J_1BNFE_ACTIVE não encontrada para o MDF-e Documento: { lwa_active-docnum } !| TYPE 'E'.
    EXIT.
  ENDIF.

*-------------------------------------------------------------------------------------------------*
* Dados Empresa
*-------------------------------------------------------------------------------------------------*

  lwa_company_definition-bukrs       = p_j_1bnfdoc-bukrs.
  lwa_company_definition-branch      = p_j_1bnfdoc-branch.
  lwa_company_definition-model       = p_j_1bnfdoc-model.
  lwa_company_definition-nftype      = p_j_1bnfdoc-nftype.

*-------------------------------------------------------------------------------------------------*
* Dados Iniciais Autorização
*-------------------------------------------------------------------------------------------------*

  lwa_authorize-uuid                 = lwa_active-cloud_guid.
  lwa_authorize-issuing_state        = zis_mdfe_emit-uf.
  CASE zis_mdfe_ide-tp_amb.
    WHEN '1'.
      lwa_authorize-environment_type     = 'PRODUCTION'.
    WHEN '2'.
      lwa_authorize-environment_type     = 'HOMOLOGATION'.
    WHEN OTHERS.
      MESSAGE |TpAmb não previsto Documento: { lwa_active-docnum } !| TYPE 'E'.
      EXIT.
  ENDCASE.

  lwa_authorize-issuing_type         = 'NORMAL'. "Para MDF-e só tem previsto o valor Noraml Enum: [ NORMAL ]
  lwa_authorize-issuer_identifier    = zis_mdfe_header-accesskey+6(14).
  PERFORM f_set_data_sefaz USING zis_mdfe_ide-dh_emi CHANGING lwa_authorize-issuing_date_time.

  lwa_authorize-document_info-layout_version =  p_j_1bnfdoc-xmlvers.
  lwa_authorize-document_info-access_key     =  zis_mdfe_header-accesskey.

  CONDENSE: lwa_authorize-document_info-layout_version NO-GAPS.

*-------------------------------------------------------------------------------------------------*
* Dados Cabecalho MDF-e
*-------------------------------------------------------------------------------------------------*
  PERFORM f_set_data_sefaz USING zis_mdfe_ide-dh_emi CHANGING lwa_authorize-document_info-header-issuing_date_time.

  lwa_authorize-document_info-header-state_code                    = zis_mdfe_ide-c_uf.
  lwa_authorize-document_info-header-environment_type              = zis_mdfe_ide-tp_amb.
  lwa_authorize-document_info-header-issuer_type                   = zis_mdfe_ide-tp_emit.
  lwa_authorize-document_info-header-transport_type                = zis_mdfe_ide-tp_transp.
  lwa_authorize-document_info-header-document_model                = zis_mdfe_ide-mod.
  lwa_authorize-document_info-header-document_series               = zis_mdfe_ide-serie.
  lwa_authorize-document_info-header-document_number               = zis_mdfe_ide-n_mdf.
  lwa_authorize-document_info-header-access_key_random_number      = zis_mdfe_ide-c_mdf.
  lwa_authorize-document_info-header-access_key_check_digit        = zis_mdfe_ide-c_dv.
  lwa_authorize-document_info-header-mode_of_transport             = zis_mdfe_ide-modal.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lwa_authorize-document_info-header-mode_of_transport
    IMPORTING
      output = lwa_authorize-document_info-header-mode_of_transport.



  lwa_authorize-document_info-header-issuing_type                  = zis_mdfe_ide-tp_emis.
  lwa_authorize-document_info-header-issuing_process_identifier    = zis_mdfe_ide-proc_emi.
  lwa_authorize-document_info-header-application_version           = zis_mdfe_ide-ver_proc.
  lwa_authorize-document_info-header-loading_start_uf              = zis_mdfe_ide-ufini.
  lwa_authorize-document_info-header-loading_end_uf                = zis_mdfe_ide-uffim.

  LOOP AT zit_mdfe_inf_muncarrega INTO DATA(lwa_muncarrega) .
    APPEND VALUE #( city_code = lwa_muncarrega-c_mun_carrega
                    city_name = lwa_muncarrega-x_mun_carrega ) TO lwa_authorize-document_info-header-loading_city_info.
  ENDLOOP.

  LOOP AT zit_mdfe_inf_percurso INTO DATA(lwa_inf_percurso) .
    APPEND VALUE #( state = lwa_inf_percurso-ufper ) TO lwa_authorize-document_info-header-route_info.
  ENDLOOP.

  PERFORM f_set_data_sefaz USING zis_mdfe_ide-dh_iniviagem CHANGING lwa_authorize-document_info-header-start_journey_date_time.
  lwa_authorize-document_info-header-canal_verde_indicator         = zis_mdfe_ide-ind_canal_verde.
  "lwa_authorize-document_info-header-subsequent_event_indicator    =

  CONDENSE: lwa_authorize-document_info-header-mode_of_transport
      NO-GAPS.

*-------------------------------------------------------------------------------------------------*
* Dados Emissor MDF-e
*-------------------------------------------------------------------------------------------------*

  lwa_authorize-document_info-issuer-cnpj                 = zis_mdfe_emit-cnpj.
  lwa_authorize-document_info-issuer-cpf                  = zis_mdfe_emit-cpf.
  lwa_authorize-document_info-issuer-state_inscription    = zis_mdfe_emit-ie.
  lwa_authorize-document_info-issuer-company_name         = zis_mdfe_emit-x_nome.
  lwa_authorize-document_info-issuer-trading_name         = zis_mdfe_emit-x_fant.

  lwa_authorize-document_info-issuer-address-name         = zis_mdfe_emit-x_lgr.
  lwa_authorize-document_info-issuer-address-number       = zis_mdfe_emit-nro.
  lwa_authorize-document_info-issuer-address-complement   = zis_mdfe_emit-x_cpl.
  lwa_authorize-document_info-issuer-address-district     = zis_mdfe_emit-x_bairro.
  lwa_authorize-document_info-issuer-address-city_code    = zis_mdfe_emit-c_mun.
  lwa_authorize-document_info-issuer-address-city_name    = zis_mdfe_emit-x_mun.
  lwa_authorize-document_info-issuer-address-zip          = zis_mdfe_emit-cep.
  lwa_authorize-document_info-issuer-address-state        = zis_mdfe_emit-uf.
  lwa_authorize-document_info-issuer-address-phone        = zis_mdfe_emit-fone.
  lwa_authorize-document_info-issuer-address-email        = zis_mdfe_emit-email.


*-------------------------------------------------------------------------------------------------*
* Dados Modal
*-------------------------------------------------------------------------------------------------*

  lwa_authorize-document_info-modal_info-version = p_j_1bnfdoc-xmlvers.

  CASE zis_mdfe_ide-modal.
    WHEN '1'. "Rodoviario

      PERFORM f_build_info_modal_rodo USING zis_mdfe_infmodal_rodo
                                            zit_mdfe_rodo_inf_ciot
                                            zit_mdfe_rodo_veic
                                            zit_mdfe_rodo_prop
                                            zit_mdfe_rodo_condutor
                                            zit_mdfe_rodo_disp
                                            zit_mdfe_rodo_inf_contratante
**<<<------"185281 - NMS - INI------>>>
                                            zit_mdfe_rodo_inf_pagamento
                                            zit_mdfe_rodo_inf_comp
                                            zit_mdfe_rodo_inf_banco
**<<<------"185281 - NMS - FIM------>>>
                                   CHANGING lwa_authorize.

    WHEN '2'. "Aereo
    WHEN '3'. "Aquaviario

      PERFORM f_build_info_modal_aqua USING zis_mdfe_infmodal_aquav
                                            zit_mdfe_inftermcarreg
                                            zit_mdfe_inftermdescarreg
                                            zit_mdfe_infembcomb
                                            zit_mdfe_infunidcargavazia
                                            zit_mdfe_infunidtranspvazia
                                   CHANGING lwa_authorize.

    WHEN '4'. "Ferroviario

  ENDCASE.

  CONDENSE:  lwa_authorize-document_info-modal_info-version
      NO-GAPS.


*-------------------------------------------------------------------------------------------------*
* Informações Documentos
*-------------------------------------------------------------------------------------------------*

  LOOP AT zit_mdfe_inf_mundescarg INTO DATA(lwa_inf_mun_descarga).
    CLEAR: lwa_unoload_info, lit_info_cte[], lit_info_nfe[].

    lwa_unoload_info-city_code = lwa_inf_mun_descarga-c_mun_descarga.
    lwa_unoload_info-city_name = lwa_inf_mun_descarga-x_mun_descarga.

    LOOP AT zit_mdfe_infcte INTO DATA(lwa_inf_cte) WHERE id = lwa_inf_mun_descarga-id.

      APPEND INITIAL LINE TO lit_info_cte ASSIGNING FIELD-SYMBOL(<fs_info_cte>).

      <fs_info_cte>-access_key                 = lwa_inf_cte-ch_cte.
      "<fs_info_cte>-second_bar_code            = lwa_inf_cte-seg_cod_barra.
      <fs_info_cte>-renewed_delivery_indicator = lwa_inf_cte-ind_reentrega.
*      <fs_info_cte>-transp_unity_info          =
*      <fs_info_cte>-dangerous_products         =
*      <fs_info_cte>-partial_delivery           =
    ENDLOOP.


    LOOP AT zit_mdfe_infnfe INTO DATA(lwa_inf_nfe) WHERE id = lwa_inf_mun_descarga-id.

      APPEND INITIAL LINE TO lit_info_nfe ASSIGNING FIELD-SYMBOL(<fs_info_nfe>).

      <fs_info_nfe>-access_key                 = lwa_inf_nfe-ch_nfe.
      "<fs_info_nfe>-second_bar_code            = lwa_inf_nfe-seg_cod_barra.
      <fs_info_nfe>-renewed_delivery_indicator = lwa_inf_nfe-ind_reentrega.
*      <fs_info_cte>-transp_unity_info          =
*      <fs_info_cte>-dangerous_products         =
*      <fs_info_cte>-partial_delivery           =
    ENDLOOP.

    lwa_unoload_info-cte_info = lit_info_cte.
    lwa_unoload_info-nfe_info = lit_info_nfe.

    APPEND lwa_unoload_info TO lwa_authorize-document_info-documents_info-unload_info.

  ENDLOOP.

*-------------------------------------------------------------------------------------------------*
* Informações Seguro
*-------------------------------------------------------------------------------------------------*
  LOOP AT zit_mdfe_seg INTO DATA(lwa_info_seguro).
    APPEND INITIAL LINE TO lwa_authorize-document_info-insurance ASSIGNING FIELD-SYMBOL(<fs_insurance>).

    <fs_insurance>-insurance_info-responsible   = lwa_info_seguro-resp_seg.
    <fs_insurance>-insurance_info-cnpj          = lwa_info_seguro-cnpj.
    <fs_insurance>-insurance_info-cpf           = lwa_info_seguro-cpf.
    <fs_insurance>-insurance_company_info-name  = lwa_info_seguro-x_seg.
    <fs_insurance>-insurance_company_info-cnpj  = lwa_info_seguro-seg_cnpj.
    <fs_insurance>-contract_number              = lwa_info_seguro-n_apol.

    IF lwa_info_seguro-text_id_n_aver IS NOT INITIAL.
      LOOP AT zit_mdfe_text INTO DATA(lwa_mdfe_text) WHERE id = lwa_info_seguro-text_id_n_aver.
        APPEND INITIAL LINE TO <fs_insurance>-averbation_number ASSIGNING FIELD-SYMBOL(<fs_averbation_number>).
        <fs_averbation_number> = lwa_mdfe_text-text.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*-------------------------------------------------------------------------------------------------*
* Produtos Predominante
*-------------------------------------------------------------------------------------------------*
  lwa_authorize-document_info-predominant_product-cargo_type                        = zis_mdfe_prodpred-tpcarga.
  lwa_authorize-document_info-predominant_product-product_description               = zis_mdfe_prodpred-xprod.
  lwa_authorize-document_info-predominant_product-cean                              = zis_mdfe_prodpred-cean.
  lwa_authorize-document_info-predominant_product-ncm                               = zis_mdfe_prodpred-ncm.

  "Informações Lotação
  lwa_authorize-document_info-predominant_product-workload_info-loading_info-cep    = zis_mdfe_prodpred-cep_carga.
  lwa_authorize-document_info-predominant_product-workload_info-unloading_info-cep  = zis_mdfe_prodpred-cep_descarga.


*-------------------------------------------------------------------------------------------------*
* Totais
*-------------------------------------------------------------------------------------------------*
  IF wis_mdfe_tot-q_cte > 0.
    lwa_authorize-document_info-totals-related_cte_quantities             = wis_mdfe_tot-q_cte.
  ENDIF.

  IF wis_mdfe_tot-q_nfe IS NOT INITIAL.
    lwa_authorize-document_info-totals-related_nfe_quantities             = wis_mdfe_tot-q_nfe.
  ENDIF.

  IF wis_mdfe_tot-q_mdfe IS NOT INITIAL.
    lwa_authorize-document_info-totals-related_mdfe_quantities              = wis_mdfe_tot-q_mdfe.
  ENDIF.

  lwa_authorize-document_info-totals-cargo_total_value                    = wis_mdfe_tot-v_carga.
  lwa_authorize-document_info-totals-unit_of_measure_code                 = wis_mdfe_tot-c_unid.
  lwa_authorize-document_info-totals-total_quantity                       = wis_mdfe_tot-q_carga.

  CONDENSE: lwa_authorize-document_info-totals-related_cte_quantities,
            lwa_authorize-document_info-totals-related_nfe_quantities,
            lwa_authorize-document_info-totals-related_mdfe_quantities,
            lwa_authorize-document_info-totals-cargo_total_value,
            lwa_authorize-document_info-totals-unit_of_measure_code,
            lwa_authorize-document_info-totals-total_quantity.

*-------------------------------------------------------------------------------------------------*
*  Informações Selo
*-------------------------------------------------------------------------------------------------*
  "  lwa_authorize-document_info-seals

*-------------------------------------------------------------------------------------------------*
*  Informações Autorização
*-------------------------------------------------------------------------------------------------*
  LOOP AT zit_mdfe_autxml INTO DATA(lwa_autxml).
    APPEND INITIAL LINE TO lwa_authorize-document_info-authorized ASSIGNING FIELD-SYMBOL(<fs_authorized>).
    <fs_authorized>-cnpj = lwa_autxml-cnpj.
    <fs_authorized>-cpf = lwa_autxml-cpf.
  ENDLOOP.

*-------------------------------------------------------------------------------------------------*
*  Informações Adicionais
*-------------------------------------------------------------------------------------------------*

  lwa_authorize-document_info-additional_info-complementary = zis_mdfe_infadic-inf_cpl.
  lwa_authorize-document_info-additional_info-fisco         = zis_mdfe_infadic-inf_ad_fisco.

*-------------------------------------------------------------------------------------------------*
*  Informações Tecnico Responsavel
*-------------------------------------------------------------------------------------------------*

  lwa_authorize-document_info-technical_responsible-cnpj             = zis_mdfe_infresptec-cnpj.
  lwa_authorize-document_info-technical_responsible-contact_name     = zis_mdfe_infresptec-x_contato.
  lwa_authorize-document_info-technical_responsible-email            = zis_mdfe_infresptec-email.
  lwa_authorize-document_info-technical_responsible-phone            = zis_mdfe_infresptec-fone.
  lwa_authorize-document_info-technical_responsible-security_code    = zis_mdfe_infresptec-id_csrt.
  lwa_authorize-document_info-technical_responsible-hash             = zis_mdfe_infresptec-hash_csrt.

*-------------------------------------------------------------------------------------------------*
*  Informações Suplementares
*-------------------------------------------------------------------------------------------------*

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  IF lwa_active-tpemis = '2'.
    lo_service_locator = cl_j_1bnfe_cf_monitor=>create_service_locator_object(
       iv_bukrs        = p_j_1bnfdoc-bukrs
       iv_branch       = p_j_1bnfdoc-branch
       iv_model        = p_j_1bnfdoc-model
       iv_nftype       = p_j_1bnfdoc-nftype
       iv_service_type = cl_nfe_cloud_constant_service=>qr_code ).

    IF lo_service_locator->is_cloud_service( ) = abap_true.
      CREATE OBJECT lo_cloud_mdfe_processor.
      lwa_authorize-supplementary_info-qr_code = lo_cloud_mdfe_processor->get_qrcode(
          is_document_header     = p_j_1bnfdoc
          is_electronic_document = lwa_active
          iv_environment         = lwa_active-tpamb ).

    ENDIF.
  ELSE.
    lwa_authorize-supplementary_info-qr_code = 'https://dfe-portal.svrs.rs.gov.br/mdfe/qrCode?chMDFe=' &&
                                               zis_mdfe_header-accesskey && '&tpAmb=' && zis_mdfe_ide-tp_amb.
  ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  DATA(lit_return_msgs) = lob_mdfe_processor->authorize( EXPORTING is_authorize          = lwa_authorize
                                                                   is_company_definition = lwa_company_definition ).

  READ TABLE lit_return_msgs INTO DATA(lwa_message) WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    IF lines( lit_return_msgs ) > 1.
      LOOP AT lit_return_msgs INTO DATA(lwa_message_v1) WHERE type = 'E'.
        MESSAGE ID lwa_message-id TYPE 'I' NUMBER lwa_message_v1-number WITH lwa_message_v1-message(50) lwa_message_v1-message+50(50) lwa_message_v1-message+100(50) lwa_message_v1-message+150(50).
      ENDLOOP.
    ENDIF.
    "CONTINGENCIA MDF-E  Ignorar Validações de Reenvio Contingencia

    IF  lw_IGN_VAL_RESND_CONTI IS INITIAL.
      MESSAGE ID lwa_message-id TYPE 'E' NUMBER lwa_message-number WITH lwa_message-message(50) lwa_message-message+50(50) lwa_message-message+100(50) lwa_message-message+150(50).
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_build_info_modal_rodo USING zis_mdfe_infmodal_rodo           TYPE zxnfe_if_mdfe_rodo_300_s
                                   zit_mdfe_rodo_inf_ciot           TYPE zxnfe_if_mdfe_rodo_inf_ciot_t
                                   zit_mdfe_rodo_veic               TYPE zxnfe_if_mdfe_rodo_veic_t
                                   zit_mdfe_rodo_prop               TYPE zxnfe_if_mdfe_rodo_prop_t
                                   zit_mdfe_rodo_condutor           TYPE zxnfe_if_mdfe_rodo_condutor_t
                                   zit_mdfe_rodo_disp               TYPE zxnfe_if_mdfe_rodo_disp_300_t
                                   zit_mdfe_rodo_inf_contratante    TYPE zxnfe_if_mdfe_rodo_inf_cont_t
**<<<------"185281 - NMS - INI------>>>
                                   zit_mdfe_rodo_inf_pagamento      TYPE zxnfe_if_mdfe_rodo_inf_cont_t
                                   zit_mdfe_rodo_inf_comp           TYPE zxnfe_if_mdfe_rodo_inf_comp_t
                                   zit_mdfe_rodo_inf_banco          TYPE zxnfe_if_mdfe_rodo_inf_banco_t
**<<<------"185281 - NMS - FIM------>>>
                          CHANGING c_authorize TYPE nfe_cloud_mdfe_map_auth.

  DATA: lwa_trailer_info      TYPE nfe_cloud_mdfe_map_trailer_inf,
**<<<------"185281 - NMS - INI------>>>
        el_payment_info       TYPE nfe_cloud_mdfe_map_payment_inf,
        el_payment_components TYPE nfe_cloud_mdfe_map_paym_compon.
**<<<------"185281 - NMS - FIM------>>>
  "Informações ANTT

  IF zis_mdfe_infmodal_rodo-rntrc IS NOT INITIAL.
    c_authorize-document_info-modal_info-road-antt_info-rntrc              = zis_mdfe_infmodal_rodo-rntrc.
  ENDIF.

  "Informação  Ciot
  LOOP AT  zit_mdfe_rodo_inf_ciot INTO DATA(lwa_inf_ciot).
    APPEND VALUE #( code = lwa_inf_ciot-ciot
                    cnpj = lwa_inf_ciot-cnpj
                    cpf = lwa_inf_ciot-cpf ) TO c_authorize-document_info-modal_info-road-antt_info-ciot_info.
  ENDLOOP.

  "Informações de Vale Pedagio
  "c_authorize-document_info-modal_info-road-antt_info-toll_voucher       =


  "Informação Contratante
  LOOP AT  zit_mdfe_rodo_inf_contratante   INTO DATA(lwa_inf_contratante).
    APPEND VALUE #( name = lwa_inf_contratante-xnome
                    cnpj = lwa_inf_contratante-cnpj
                    cpf = lwa_inf_contratante-cpf
                    foreigner_id = lwa_inf_contratante-id_estrangeiro ) TO c_authorize-document_info-modal_info-road-antt_info-contractors_info  .
  ENDLOOP.

  "Informações de Pagamento
  "c_authorize-document_info-modal_info-road-antt_info-payment_info       =


  "Informação Veiculo Tração
  LOOP AT zit_mdfe_rodo_veic INTO DATA(lwa_inf_veiculo) WHERE id = '1'.


    c_authorize-document_info-modal_info-road-vehicle_info-internal_code  = lwa_inf_veiculo-c_int.
    c_authorize-document_info-modal_info-road-vehicle_info-plate          = lwa_inf_veiculo-placa.
    c_authorize-document_info-modal_info-road-vehicle_info-renavam        = lwa_inf_veiculo-renavam.
    c_authorize-document_info-modal_info-road-vehicle_info-weight         = lwa_inf_veiculo-tara.
    c_authorize-document_info-modal_info-road-vehicle_info-capacity_kg    = lwa_inf_veiculo-cap_kg.

    IF lwa_inf_veiculo-cap_m3 IS NOT INITIAL.
      c_authorize-document_info-modal_info-road-vehicle_info-capacity_m3    = lwa_inf_veiculo-cap_m3.
    ENDIF.

    "Proprietario
    LOOP AT  zit_mdfe_rodo_prop INTO DATA(lwa_inf_rodo_prop) WHERE id = lwa_inf_veiculo-prop_ref.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-cnpj              = lwa_inf_rodo_prop-cnpj.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-cpf               = lwa_inf_rodo_prop-cpf.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-rntrc             = lwa_inf_rodo_prop-rntrc.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-name              = lwa_inf_rodo_prop-x_nome.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-state_inscription = lwa_inf_rodo_prop-ie.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-state             = lwa_inf_rodo_prop-uf.
      c_authorize-document_info-modal_info-road-vehicle_info-owner-type              = lwa_inf_rodo_prop-tp_prop.
      EXIT.
    ENDLOOP.

    "Informãções Condutor
    LOOP AT zit_mdfe_rodo_condutor INTO DATA(lwa_inf_condutor).
      APPEND  VALUE #( cpf  = lwa_inf_condutor-cpf
                       name = lwa_inf_condutor-x_nome ) TO c_authorize-document_info-modal_info-road-vehicle_info-conductor.
    ENDLOOP.

    c_authorize-document_info-modal_info-road-vehicle_info-means_type     = lwa_inf_veiculo-tp_rod.
    c_authorize-document_info-modal_info-road-vehicle_info-trailer_type   = lwa_inf_veiculo-tp_car.
    c_authorize-document_info-modal_info-road-vehicle_info-state          = lwa_inf_veiculo-uf.

    CONDENSE: c_authorize-document_info-modal_info-road-vehicle_info-weight,
              c_authorize-document_info-modal_info-road-vehicle_info-capacity_kg,
              c_authorize-document_info-modal_info-road-vehicle_info-capacity_m3
       NO-GAPS.

  ENDLOOP.

  "Informação Veiculo Reboque
  LOOP AT zit_mdfe_rodo_veic INTO lwa_inf_veiculo WHERE id = '2'.

    CLEAR: lwa_trailer_info.

    lwa_trailer_info-internal_code  = lwa_inf_veiculo-c_int.
    lwa_trailer_info-plate          = lwa_inf_veiculo-placa.
    lwa_trailer_info-renavam        = lwa_inf_veiculo-renavam.
    lwa_trailer_info-weight         = lwa_inf_veiculo-tara.
    lwa_trailer_info-capacity_kg    = lwa_inf_veiculo-cap_kg.

    IF lwa_inf_veiculo-cap_m3 IS NOT INITIAL.
      lwa_trailer_info-capacity_m3    = lwa_inf_veiculo-cap_m3.
    ENDIF.

    "Proprietario
    LOOP AT  zit_mdfe_rodo_prop INTO lwa_inf_rodo_prop WHERE id = lwa_inf_veiculo-prop_ref.
      lwa_trailer_info-owner-cnpj              = lwa_inf_rodo_prop-cnpj.
      lwa_trailer_info-owner-cpf               = lwa_inf_rodo_prop-cpf.
      lwa_trailer_info-owner-rntrc             = lwa_inf_rodo_prop-rntrc.
      lwa_trailer_info-owner-name              = lwa_inf_rodo_prop-x_nome.
      lwa_trailer_info-owner-state_inscription = lwa_inf_rodo_prop-ie.
      lwa_trailer_info-owner-state             = lwa_inf_rodo_prop-uf.
      lwa_trailer_info-owner-type              = lwa_inf_rodo_prop-tp_prop.
      EXIT.
    ENDLOOP.

    lwa_trailer_info-trailer_type   = lwa_inf_veiculo-tp_car.
    lwa_trailer_info-state          = lwa_inf_veiculo-uf.

    CONDENSE: lwa_trailer_info-weight,
              lwa_trailer_info-capacity_kg,
              lwa_trailer_info-capacity_m3
      NO-GAPS.

    APPEND lwa_trailer_info TO c_authorize-document_info-modal_info-road-trailer_info.

  ENDLOOP.
**<<<------"185281 - NMS - INI------>>>
**<<<------"192137 - NMS - INI------>>>
  SELECT SINGLE low FROM tvarvc INTO @DATA(vl_tag_yes) WHERE name EQ 'MDFE_ATIVA_TAG' AND type EQ 'P'.

  IF     sy-subrc IS INITIAL  AND
     NOT vl_tag_yes IS INITIAL.
**<<<------"192137 - NMS - FIM------>>>
* Informações do Pagamento do Frete.
    LOOP AT zit_mdfe_rodo_inf_pagamento INTO DATA(el_mdfe_rodo_inf_pagamento).
      el_payment_info-name         = el_mdfe_rodo_inf_pagamento-xnome.
      el_payment_info-cpf          = el_mdfe_rodo_inf_pagamento-cpf.
      el_payment_info-cnpj         = el_mdfe_rodo_inf_pagamento-cnpj.
      el_payment_info-foreigner_id = el_mdfe_rodo_inf_pagamento-id_estrangeiro.
* Componentes do Pagamento do Frete.
      LOOP AT zit_mdfe_rodo_inf_comp INTO DATA(el_mdfe_rodo_inf_comp).
        LOOP AT el_mdfe_rodo_inf_comp-tipo INTO DATA(el_tipo).
          el_payment_components-type  = el_tipo-tp_comp.
          el_payment_components-value = el_tipo-v_comp.

          APPEND el_payment_components TO el_payment_info-payment_components.
          CLEAR  el_payment_components.

        ENDLOOP.

        el_payment_info-contract_total_value = el_mdfe_rodo_inf_comp-v_contrato.
        el_payment_info-payment_type         = el_mdfe_rodo_inf_comp-ind_pag.

      ENDLOOP.
* Informações bancárias.
      LOOP AT zit_mdfe_rodo_inf_banco INTO DATA(el_mdfe_rodo_inf_banco).
        el_payment_info-bank_info-bank_code     = el_mdfe_rodo_inf_banco-cod_banco.
        el_payment_info-bank_info-agency_number = el_mdfe_rodo_inf_banco-cod_agencia.
        el_payment_info-bank_info-ipef_cnpj     = el_mdfe_rodo_inf_banco-cnpjipef.

      ENDLOOP.

      APPEND el_payment_info TO c_authorize-document_info-modal_info-road-antt_info-payment_info.

    ENDLOOP.
**<<<------"192137 - NMS - INI------>>>
  ENDIF.
**<<<------"192137 - NMS - FIM------>>>
**<<<------"185281 - NMS - FIM------>>>
  "Informações Lacre
  "c_authorize-document_info-modal_info-road-seals


ENDFORM.

FORM f_build_info_modal_aqua  USING zis_mdfe_infmodal_aquav                 TYPE zxnfe_if_mdfe_aquav_300_s        "ok
                                    zit_mdfe_inftermcarreg                  TYPE zxnfe_if_mdfe_inftermcarreg_t            "ok
                                    zit_mdfe_inftermdescarreg               TYPE zxnfe_if_mdfe_inftermdescarr_t           "ok
                                    zit_mdfe_infembcomb                     TYPE zxnfe_if_mdfe_infembcomb_300_t           "ok
                                    zit_mdfe_infunidcargavazia              TYPE zxnfe_if_mdfe_infunidcargava_t           "ok
                                    zit_mdfe_infunidtranspvazia             TYPE zxnfe_if_mdfe_infunidtranspv_t           "ok
                           CHANGING c_authorize TYPE nfe_cloud_mdfe_map_auth.

  c_authorize-document_info-modal_info-waterway-irin                      = zis_mdfe_infmodal_aquav-irin.
  c_authorize-document_info-modal_info-waterway-vessel_type               = zis_mdfe_infmodal_aquav-tp_emb.
  c_authorize-document_info-modal_info-waterway-vessel_code               = zis_mdfe_infmodal_aquav-c_embar.
  c_authorize-document_info-modal_info-waterway-vessel_name               = zis_mdfe_infmodal_aquav-x_embar.
  c_authorize-document_info-modal_info-waterway-trip_number               = zis_mdfe_infmodal_aquav-n_viag.
  c_authorize-document_info-modal_info-waterway-departure_port_code       = zis_mdfe_infmodal_aquav-c_prt_emb.
  c_authorize-document_info-modal_info-waterway-destination_port_code     = zis_mdfe_infmodal_aquav-c_prt_dest.
  c_authorize-document_info-modal_info-waterway-transhipment_port         = zis_mdfe_infmodal_aquav-prt_trans.
  c_authorize-document_info-modal_info-waterway-navigation_type           = zis_mdfe_infmodal_aquav-tp_nav.

  LOOP AT zit_mdfe_inftermcarreg INTO DATA(lwa_inf_term_car).
    APPEND VALUE #(  code = lwa_inf_term_car-c_term_carreg
                     name = lwa_inf_term_car-x_term_carreg  ) TO c_authorize-document_info-modal_info-waterway-loading_terminal_info.
  ENDLOOP.

  LOOP AT zit_mdfe_inftermdescarreg INTO DATA(lwa_inf_term_descar).
    APPEND VALUE #(  code = lwa_inf_term_descar-c_term_descarreg
                     name = lwa_inf_term_descar-x_term_descarreg  ) TO c_authorize-document_info-modal_info-waterway-unloading_terminal_info.
  ENDLOOP.


  LOOP AT zit_mdfe_infembcomb INTO DATA(lwa_inf_comb).
    APPEND VALUE #( shipping_code = lwa_inf_comb-c_emb_comb
                    ferryboat_id  = lwa_inf_comb-x_balsa  )  TO c_authorize-document_info-modal_info-waterway-convoy_info.
  ENDLOOP.

  LOOP AT zit_mdfe_infunidcargavazia INTO DATA(lwa_inf_carga_vazia).
    APPEND VALUE #(  id    = lwa_inf_carga_vazia-id_unid_carga_vazia
                     type  = lwa_inf_carga_vazia-tp_unid_carga_vazia ) TO c_authorize-document_info-modal_info-waterway-empty_cargo_info.
  ENDLOOP.

  LOOP AT zit_mdfe_infunidtranspvazia INTO DATA(lwa_inf_transp_vazia).
    APPEND VALUE #(  id    = lwa_inf_transp_vazia-id_unid_transp_vazia
                     type  = lwa_inf_transp_vazia-tp_unid_transp_vazia ) TO c_authorize-document_info-modal_info-waterway-empty_transp_info.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_data_sefaz
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZIS_MDFE_IDE_DH_EMI
*&      <-- LWA_AUTHORIZE_DOCUMENT_INFO_HE
*&---------------------------------------------------------------------*


FORM f_send_mdfe_event_to_cloud USING p_docnum        TYPE j_1bnfdoc-docnum
                                      p_j_1bnfe_event TYPE j_1bnfe_event
                                      p_info_event    TYPE nfe_cloud_mdfe_map_event_info.

  DATA: lwa_company_definition TYPE j_1bnfe_s_company_code_def.
  DATA: lwa_nfe_cloud_event   TYPE nfe_cloud_mdfe_map_event.

  DATA: lob_mdfe_processor  TYPE REF TO cl_nfe_cloud_mdfe_processor.

  CREATE OBJECT lob_mdfe_processor.

  "CONTINGENCIA MDF-E  Ignorar Validações de Reenvio Contingencia
  SELECT SINGLE *
     FROM tvarvc INTO @DATA(lw_IGN_VAL_RESND_CONTI)
    WHERE name EQ 'ZRDC_IGN_VAL_RESND_CONTI'
      AND low  EQ @sy-uname.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(lwa_doc)
   WHERE docnum = @p_docnum.


  IF sy-subrc NE 0.
    MESSAGE |J_1BNFDOC não encontrada para o MDF-e Documento: { p_docnum  } !| TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(lwa_active)
   WHERE docnum = @p_docnum.

  IF sy-subrc NE 0.
    MESSAGE |J_1BNFE_ACTIVE não encontrada para o MDF-e Documento: { p_docnum  } !| TYPE 'E'.
    EXIT.
  ENDIF.



*-------------------------------------------------------------------------------------------------*
* Dados Empresa
*-------------------------------------------------------------------------------------------------*

  lwa_company_definition-bukrs       = lwa_doc-bukrs.
  lwa_company_definition-branch      = lwa_doc-branch.
  lwa_company_definition-model       = lwa_doc-model.
  lwa_company_definition-nftype      = lwa_doc-nftype.

*-------------------------------------------------------------------------------------------------*
* Dados Evento
*-------------------------------------------------------------------------------------------------*

  lwa_nfe_cloud_event-uuid                  = p_j_1bnfe_event-cloud_uuid.
  lwa_nfe_cloud_event-authorize_uuid        = lwa_active-cloud_guid.

  zcl_estado=>zif_estado~get_instance( )->get_sigla_estado( EXPORTING i_id_bacen = CONV #( lwa_active-regio )
                                                            IMPORTING e_uf       = lwa_nfe_cloud_event-issuing_state ).

  CASE lwa_active-tpamb.
    WHEN '1'.
      lwa_nfe_cloud_event-environment_type     = 'PRODUCTION'.
    WHEN '2'.
      lwa_nfe_cloud_event-environment_type     = 'HOMOLOGATION'.
    WHEN OTHERS.
      MESSAGE |TpAmb não previsto Documento: { lwa_active-docnum } !| TYPE 'E'.
      EXIT.
  ENDCASE.

  lwa_nfe_cloud_event-issuing_type          = 'NORMAL'. "Para MDF-e só tem previsto o valor Noraml Enum: [ NORMAL ]

  lwa_nfe_cloud_event-issuer_identifier     = lwa_active-stcd1.
  lwa_nfe_cloud_event-receiver_identifier   = lwa_active-stcd1.
  lwa_nfe_cloud_event-issuing_date_time     = p_info_event-issuing_date_time.

  lwa_nfe_cloud_event-event_info-access_key = lwa_active-regio &&
                                              lwa_active-nfyear &&
                                              lwa_active-nfmonth &&
                                              lwa_active-stcd1 &&
                                              lwa_active-model &&
                                              lwa_active-serie &&
                                              lwa_active-nfnum9 &&
                                              lwa_active-docnum9 &&
                                              lwa_active-cdv.

  lwa_nfe_cloud_event-event_info-state_code          = lwa_active-regio.
  lwa_nfe_cloud_event-event_info-environment_type    = lwa_active-tpamb.
  lwa_nfe_cloud_event-event_info-cnpj                = lwa_active-stcd1.
  lwa_nfe_cloud_event-event_info-issuing_date_time   = p_info_event-issuing_date_time.
  lwa_nfe_cloud_event-event_info-type                = p_info_event-type.
  lwa_nfe_cloud_event-event_info-sequence_number     = p_info_event-sequence_number.

  CASE lwa_nfe_cloud_event-event_info-type.
    WHEN '110111'. "Cancelamento
      lwa_nfe_cloud_event-event_info-event_detail-cancellation  = p_info_event-event_detail-cancellation.
    WHEN '110112'. "Encerramento
      lwa_nfe_cloud_event-event_info-event_detail-closing       = p_info_event-event_detail-closing.
    WHEN OTHERS.
  ENDCASE.

  WAIT UP TO 2 SECONDS. "Tratar erro do BTP "Field issuingDateTime: must be a date in the past or in the present"

  TRY.
      DATA(lit_messagers) = lob_mdfe_processor->send_event( EXPORTING is_event              = lwa_nfe_cloud_event
                                                                      is_company_definition = lwa_company_definition ).

      READ TABLE lit_messagers INTO DATA(lwa_message) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        DELETE FROM j_1bnfe_event WHERE docnum    = p_j_1bnfe_event-docnum
                                    AND int_event = p_j_1bnfe_event-int_event
                                    AND seqnum    = p_j_1bnfe_event-seqnum.
        PERFORM dequeue_event USING  p_docnum.

        "CONTINGENCIA MDF-E  Ignorar Validações de Reenvio Contingencia

        IF  lw_IGN_VAL_RESND_CONTI IS INITIAL.
          MESSAGE ID lwa_message-id TYPE 'E' NUMBER lwa_message-number WITH lwa_message-message(50) lwa_message-message+50(50) lwa_message-message+100(50) lwa_message-message+150(50).
        ENDIF.
      ENDIF.

      DO 3 TIMES.
        DATA(_no_exists_btp) = abap_false.
        PERFORM f_get_evento_mdfe_btp USING lwa_company_definition
                                            lwa_nfe_cloud_event
                                   CHANGING _no_exists_btp.

        IF _no_exists_btp EQ abap_false.
          EXIT.
        ELSE.
          WAIT UP TO 2 SECONDS.
        ENDIF.
      ENDDO.

      IF _no_exists_btp = abap_true.
        DELETE FROM j_1bnfe_event WHERE docnum    = p_j_1bnfe_event-docnum
                                    AND int_event = p_j_1bnfe_event-int_event
                                    AND seqnum    = p_j_1bnfe_event-seqnum.
        PERFORM dequeue_event USING  p_docnum.
        IF  lw_IGN_VAL_RESND_CONTI IS INITIAL.
          MESSAGE 'Evento de encerramento não pode ser enviado a SEFAZ! Tente novamente!' TYPE 'I'.
        ENDIF.
      ENDIF.

    CATCH cx_nfe_cloud_invalid_value.

  ENDTRY.


ENDFORM.

FORM f_set_data_sefaz  USING p_data_hora  TYPE zxnfe_dhemi_utc
                    CHANGING p_data_sefaz TYPE string.

  p_data_sefaz = p_data_hora+00(4) && '-' &&
                 p_data_hora+04(2) && '-' &&
                 p_data_hora+06(2) && 'T' &&
                 p_data_hora+08(2) && ':' &&
                 p_data_hora+10(2) && ':' &&
                 p_data_hora+12(2) && '-04:00'.

ENDFORM.

FORM f_conv_timestamp_to_utc USING p_timestamp
                          CHANGING c_dhemi_utc TYPE zxnfe_dhemi_utc.

  DATA: l_timestamp  TYPE char30.

  CLEAR: c_dhemi_utc.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  l_timestamp        = p_timestamp.
  CONDENSE l_timestamp.
  l_timestamp+12(02) = '00'.
  p_timestamp        = l_timestamp.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  cl_abap_tstmp=>systemtstmp_utc2syst(
    EXPORTING
      utc_tstmp = CONV #( p_timestamp )
    IMPORTING
      syst_date = DATA(lv_system_date)    " System Date
      syst_time = DATA(lv_system_time)    " System Time
  ).

  c_dhemi_utc =  lv_system_date && lv_system_time.

ENDFORM.


FORM f_get_evento_mdfe_btp USING p_company_definition TYPE j_1bnfe_s_company_code_def
                                 p_nfe_cloud_event    TYPE nfe_cloud_mdfe_map_event
                        CHANGING c_no_exists_btp.

  DATA: lob_mdfe_processor TYPE REF TO cl_nfe_cloud_mdfe_processor.

  DATA: lwa_status_request     TYPE nfe_cloud_request_uuid_list,
        lwa_company_definition TYPE j_1bnfe_s_company_code_def.

  CONSTANTS: c_action_ev_status	 TYPE c LENGTH 50 VALUE 'EV_STATUS'.

  CLEAR: c_no_exists_btp.

  CREATE OBJECT lob_mdfe_processor.

  APPEND p_nfe_cloud_event-uuid TO lwa_status_request-uuid_list.

  lwa_company_definition-branch  = p_company_definition-branch.
  lwa_company_definition-bukrs   = p_company_definition-bukrs.
  lwa_company_definition-model   = p_company_definition-model.
  lwa_company_definition-nftype  = p_company_definition-nftype.

  DATA(lwa_result) = lob_mdfe_processor->get_status(
    EXPORTING
      is_status_request     = lwa_status_request
      is_company_definition = lwa_company_definition
      iv_action             = CONV #( c_action_ev_status ) ).

  LOOP AT lwa_result-statuses INTO DATA(lwa_statuses) WHERE processing_status = 'NOT_FOUND'.
    c_no_exists_btp = abap_true.
    EXIT.
  ENDLOOP.


ENDFORM.
