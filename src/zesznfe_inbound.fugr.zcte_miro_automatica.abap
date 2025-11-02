FUNCTION ZCTE_MIRO_AUTOMATICA.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE_NFE) TYPE  ZESZDE_CHAVE_DOC_E
*"     REFERENCE(I_DEPARTAMENTO) TYPE  ZESZDE_DEPARTAMENTO OPTIONAL
*"     REFERENCE(I_DATA_VENCIMENTO) TYPE  ZESZDE_DT_VENCIMENTO OPTIONAL
*"     REFERENCE(I_BANCO_PARCEIRO) TYPE  BVTYP OPTIONAL
*"     REFERENCE(I_ITENS_NFE) TYPE  ZDE_NFE_INFO_ITEM_T OPTIONAL
*"     REFERENCE(I_OBS_FINANCEIRA) TYPE  ZESZDE_OBS_FINANCEIRA_CTR
*"         OPTIONAL
*"     REFERENCE(I_BLOQUEIO_PAGAMENTO) TYPE  DZLSPR OPTIONAL
*"     REFERENCE(I_CK_SOMENTE_UMA_MIGO_PEDIDO) TYPE  CHAR01
*"         DEFAULT 'X'
*"     REFERENCE(I_CK_ESTORNAR) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(I_CPF) TYPE  STRING OPTIONAL
*"     REFERENCE(I_CK_REVISAR) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_DS_REVISAR_MOTIVO) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MESSAGEM_ERRO) TYPE  STRING
*"     REFERENCE(E_SUCESSO) TYPE  CHAR01
*"     REFERENCE(E_BELNR) TYPE  RE_BELNR
*"     REFERENCE(E_GJAHR) TYPE  GJAHR
*"--------------------------------------------------------------------


  DATA: nfe               TYPE REF TO zcl_nfe_inbound,
        e_caracteristicas	TYPE ZESZIB_NFE_DIST_LCA_T,
        cpf_limpo         TYPE string.

  e_sucesso = '0'.

  "Encontrar Usu├írio pelo CPF
  IF i_cpf IS NOT INITIAL.

    cpf_limpo = i_cpf.
    REPLACE ALL OCCURRENCES OF '.' IN cpf_limpo WITH '' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '-' IN cpf_limpo WITH '' IGNORING CASE.

    DATA(qtd_cpf) = strlen( cpf_limpo ).
    IF qtd_cpf GT 11.
      MESSAGE s127 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_adcp)
      FROM adcp
     WHERE fax_number EQ @cpf_limpo.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s128 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_usr21)
      FROM usr21
      FOR ALL ENTRIES IN @it_adcp
    WHERE persnumber EQ @it_adcp-persnumber.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s128 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    DESCRIBE TABLE it_usr21 LINES DATA(qtd_usuarios).
    IF qtd_usuarios GT 1.
      MESSAGE s129 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    READ TABLE it_usr21 INDEX 1 INTO DATA(wa_usr21).

  ENDIF.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Revis├úo de Lan├ºamento de SM no SE."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF i_ck_revisar EQ abap_true.
    TRY .
        CREATE OBJECT nfe.
        "Localizando a NF-e
        nfe->zif_cadastro~set_registro( i_id_registro = i_chave_nfe ).
        nfe->ck_ignora_data_se_vencimento = abap_false.
        nfe->set_nfe_em_revisao(
          EXPORTING
            i_motivo_revisao      = i_ds_revisar_motivo
            i_usuario_solicitante = wa_usr21-bname
        ).

        e_sucesso = '1'.

      CATCH ZESZCX_NFE_INBOUND_EXCEPTION INTO DATA(ex_nfein).
        MESSAGE ID ex_nfein->msgid TYPE 'S' NUMBER ex_nfein->msgno WITH ex_nfein->msgv1 ex_nfein->msgv2 ex_nfein->msgv3 ex_nfein->msgv4 INTO e_messagem_erro.
    ENDTRY.
    IF nfe IS NOT INITIAL.
      nfe->free( ).
      CLEAR: nfe.
    ENDIF.
  ENDIF.
  CHECK i_ck_revisar NE abap_true.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  TRY .
      CREATE OBJECT nfe.

      "Localizando a NF-e
      nfe->zif_cadastro~set_registro( i_id_registro = i_chave_nfe ).

*      " 20.09.2022 - Set valor imposto -->
*
*      CALL METHOD nfe->set_impostos
*        EXPORTING
*          i_issqn  = i_issqn
*          i_inss   = i_inss
*          i_pis    = i_pis
*          i_cofins = i_cofins
*          i_csll   = i_csll
*          i_irrf   = i_irrf.
*
*      " 20.09.2022 - Set valor imposto --<

      CASE i_ck_estornar.
        WHEN abap_true.

          nfe->nfe_inbound_cancela_fatura( ).
          e_sucesso = '1'.

        WHEN abap_false.

          "Aceite Fatura
          nfe->ck_ignora_data_se_vencimento = abap_true.
          nfe->set_aceitar_faturar( EXPORTING i_us_miro = wa_usr21-bname ).
          IF nfe->zif_cadastro~gravar_registro( ) EQ abap_true.
            DATA(nota) = nfe->get_info_nota( ).
            e_belnr   = nota-nfe_base-belnr.
            e_gjahr   = nota-nfe_base-gjahr.
            IF e_belnr IS NOT INITIAL AND e_gjahr IS NOT INITIAL.
              e_sucesso = '1'.
            ENDIF.
          ELSE.
            DATA(retorno) = nfe->mensagens_retorno.
            nfe->free( ).
            CLEAR: nfe.
            LOOP AT retorno INTO DATA(wa_retorno) WHERE type EQ 'E'.
              MESSAGE ID wa_retorno-id TYPE wa_retorno-type NUMBER wa_retorno-number
                WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4 INTO e_messagem_erro.
              EXIT.
            ENDLOOP.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO e_messagem_erro.
            EXIT.
          ENDIF.
      ENDCASE.

    CATCH zcx_miro_exception INTO ex_miro.
      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_cadastro INTO ex_cadastro.
      MESSAGE ID ex_cadastro->msgid TYPE 'S' NUMBER ex_cadastro->msgno WITH ex_cadastro->msgv1 ex_cadastro->msgv2 ex_cadastro->msgv3 ex_cadastro->msgv4 INTO e_messagem_erro.
    CATCH zcx_pedido_compra_exception INTO DATA(ex_pedido).
      MESSAGE ID ex_pedido->msgid TYPE 'S' NUMBER ex_pedido->msgno WITH ex_pedido->msgv1 ex_pedido->msgv2 ex_pedido->msgv3 ex_pedido->msgv4 INTO e_messagem_erro.
    CATCH zcx_charg_exception INTO DATA(ex_charg).
      MESSAGE ID ex_charg->msgid TYPE 'S' NUMBER ex_charg->msgno WITH ex_charg->msgv1 ex_charg->msgv2 ex_charg->msgv3 ex_charg->msgv4 INTO e_messagem_erro.
    CATCH ZESZCX_NFE_INBOUND_EXCEPTION INTO ex_nfein.
      MESSAGE ID ex_nfein->msgid TYPE 'S' NUMBER ex_nfein->msgno WITH ex_nfein->msgv1 ex_nfein->msgv2 ex_nfein->msgv3 ex_nfein->msgv4 INTO e_messagem_erro.
  ENDTRY.

  IF nfe IS NOT INITIAL.
    nfe->free( ).
    CLEAR: nfe.
  ENDIF.

ENDFUNCTION.
