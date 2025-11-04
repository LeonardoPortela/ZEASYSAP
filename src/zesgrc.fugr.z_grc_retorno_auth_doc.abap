FUNCTION z_grc_retorno_auth_doc .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ACTTAB) TYPE  J_1BNFE_ACTIVE
*"----------------------------------------------------------------------

  DATA: wl_zsdt0102     TYPE zsdt0102,
        wl_zsdt0107     TYPE zsdt0107,
        wl_j_1bstscodet TYPE j_1bstscodet,
        it_zsdt0232     TYPE TABLE OF zsdt0232,
        wl_zsdt0232     TYPE zsdt0232,
        wl_zsdt0234     TYPE zsdt0234.

  DATA: it_zlest0061 TYPE TABLE OF zlest0061,
        it_zlest0060 TYPE TABLE OF zlest0060.

  DATA: BEGIN OF wl_err_tab.
          INCLUDE STRUCTURE bdcmsgcoll.
        DATA: END OF wl_err_tab.

  DATA: it_err_tab LIKE STANDARD TABLE OF wl_err_tab.

  DATA: v_url            TYPE string.

  CLEAR: wl_j_1bstscodet, wl_zsdt0234, wl_zsdt0232.

  "Gerar Active Tmp ---------------------------------------
  DATA: wl_zact_tmp TYPE zact_tmp.
  FREE: it_zsdt0232.


  SELECT MAX( seq ) INTO @DATA(max_seq)
    FROM zact_tmp
   WHERE docnum = @i_acttab-docnum.

  ADD  1 TO max_seq.

  CLEAR: wl_zact_tmp.
  MOVE-CORRESPONDING i_acttab TO wl_zact_tmp.

  wl_zact_tmp-seq         = max_seq.
  wl_zact_tmp-dt_registro = sy-datum.
  wl_zact_tmp-hr_registro = sy-uzeit.
  wl_zact_tmp-us_registro = sy-uname.

  MODIFY zact_tmp FROM wl_zact_tmp.
  "Gerar Active Tmp Fim ---------------------------------------


  CHECK ( i_acttab-form IS NOT INITIAL ) AND ( i_acttab-code IS NOT INITIAL   ).

  CASE i_acttab-model.
    WHEN '58'.
      WAIT UP TO 02 SECONDS.
    WHEN OTHERS.
      WAIT UP TO 10 SECONDS.
  ENDCASE.

  CHECK i_acttab-docnum IS NOT INITIAL.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(wl_doc)
   WHERE docnum EQ @i_acttab-docnum.

  CHECK sy-subrc EQ 0.

  IF i_acttab-code IS NOT INITIAL.
    SELECT SINGLE *
      FROM j_1bstscodet INTO wl_j_1bstscodet
     WHERE spras EQ 'P'
       AND code  EQ i_acttab-code.

    SELECT SINGLE *
      FROM zsdt0234 INTO wl_zsdt0234
     WHERE code EQ i_acttab-code.
  ENDIF.

  CHECK ( i_acttab-action_requ IS NOT INITIAL ) AND "Não esta em processamento
        ( i_acttab-event_flag  EQ abap_false  ).    "Não continuar se for Retorno de Eventos(Cancelamento/CC-e/Encerramento)...

  CASE i_acttab-docsta.
    WHEN ' '.    "Não Enviado

      IF i_acttab-model  EQ zif_doc_eletronico=>at_st_model_mdfe AND
         i_acttab-docsta EQ space AND
         i_acttab-scssta EQ '0' AND
         i_acttab-action_requ EQ '8' .

        SELECT SINGLE *
          FROM zsdt0102 INTO wl_zsdt0102
         WHERE docnum = i_acttab-docnum.

        IF sy-subrc IS INITIAL.

          zcl_mdfe_=>zif_doc_eletronico~get_instance( i_docnum = i_acttab-docnum
            )->set_registro( EXPORTING i_docnum = i_acttab-docnum  i_sem_bloqueio = abap_true
            )->get_load_log( IMPORTING et_message = DATA(et_message)
            ).

          IF et_message[] IS NOT INITIAL.

            READ TABLE et_message INTO DATA(wa_message) INDEX 1.
            MESSAGE ID wa_message-msgid TYPE wa_message-msgty NUMBER wa_message-msgno
               WITH wa_message-msgv1 wa_message-msgv2 wa_message-msgv3 wa_message-msgv4
               INTO DATA(lc_texto).

            wl_zsdt0102-status        = '3'.
            wl_zsdt0102-code          = i_acttab-code.
            wl_zsdt0102-dt_atualizado = sy-datum.
            wl_zsdt0102-hr_atualizado = sy-uzeit.
            wl_zsdt0102-msg           = lc_texto.

            CLEAR: wl_zsdt0102-transmissao.
            CLEAR: wl_zsdt0102-docnum9, wl_zsdt0102-cdv, wl_zsdt0102-authcode.
            MODIFY zsdt0102 FROM wl_zsdt0102.
            UPDATE j_1bnfe_active SET docsta = '3' WHERE docnum EQ wl_doc-docnum. "MDF-e não tem inutilização. Status para estornar documento direto

          ENDIF.
        ENDIF.

      ENDIF.

    WHEN '1'.    "Autorizado

      CASE wl_doc-model.
        WHEN '55'.

          CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
            EXPORTING
              i_tp_mov        = 'S'
              i_docnum        = wl_doc-docnum
              i_autorizar_mov = 'X'.

          DATA: lc_autorizacao TYPE zib_autorit_grc.
          lc_autorizacao-chnfe = i_acttab-regio && i_acttab-nfyear && i_acttab-nfmonth &&
                                 i_acttab-stcd1 && i_acttab-model && i_acttab-serie &&
                                 i_acttab-nfnum9 && i_acttab-docnum9 && i_acttab-cdv.

          lc_autorizacao-dt_registro   = sy-datum.
          lc_autorizacao-hr_registro   = sy-uzeit.
          lc_autorizacao-us_registro   = sy-uname.
          lc_autorizacao-rg_atualizado = '0'.
          lc_autorizacao-direction     = 'OUTB'.
          lc_autorizacao-docnum        = i_acttab-docnum.
          lc_autorizacao-code          = i_acttab-code.
          lc_autorizacao-cancel        = abap_false.
          MODIFY zib_autorit_grc FROM lc_autorizacao.

          "Retorno Formação de Lote
          UPDATE zsdt_retlote
             SET status     = 'V'
                 nf_retorno = wl_doc-nfenum
           WHERE docnum_ret EQ wl_doc-docnum.

          UPDATE zsdt_export
             SET status = 'X'
                 nf_retorno = wl_doc-nfenum
           WHERE docnum EQ wl_doc-docnum.

          "Ajusta Romaneio de Entrada e Cria Romaneio de Saída
          TRY.
              zcl_carga_recebimento=>zif_carga~set_nota_entrada_propria( i_docnum = wl_doc-docnum ).
            CATCH zcx_cadastro.
            CATCH zcx_carga.
            CATCH zcx_parceiros.
            CATCH zcx_ordem_venda.
            CATCH zcx_job.
            CATCH zcx_pedido_compra_exception.
            CATCH zcx_ordem_carregamento.
          ENDTRY.
*** BUG - 71952 - Inicio - CBRAND
          SELECT SINGLE *
            FROM zsdt0231 INTO @DATA(wl_zsdt0231)
           WHERE docnum EQ @i_acttab-docnum.

          IF sy-subrc = 0.
            SELECT *
            FROM zsdt0232 INTO TABLE it_zsdt0232
            WHERE obj_key EQ wl_zsdt0231-obj_key
             AND refnfe <> ''.

            IF lines( it_zsdt0232 ) EQ 1.
              READ TABLE it_zsdt0232 INTO wl_zsdt0232 INDEX 1.
            ELSEIF lines( it_zsdt0232 ) > 1.
              READ TABLE it_zsdt0232 INTO wl_zsdt0232 WITH KEY nf_complementada = abap_true.
            ELSE.
              sy-subrc = 4.
            ENDIF.

            IF sy-subrc = 0.
              IF strlen( wl_zsdt0232-refnfe ) = 44.

                UPDATE zib_nfe_forn SET docnum_ref =  i_acttab-docnum
                                user_identificador = sy-uname
                                data_identificador = sy-datum
                                hora_identificador = sy-uzeit
                WHERE nu_chave EQ wl_zsdt0232-refnfe
                  AND docnum_ref = '0000000000'. " USER STORY 81890 24-08-2022 / Anderson Oenning

              ENDIF.
            ENDIF.
          ENDIF.
*** BUG - 71952 - Fim - CBRAND
        WHEN '57'.

          "Finaliza VT
          CALL FUNCTION 'Z_LES_FINALIZA_VT'
            EXPORTING
              p_docnum    = wl_doc-docnum
              p_j_1bnfdoc = wl_doc.

          "Aquaviário
          CLEAR: it_zlest0061[], it_zlest0060[].

          SELECT *
            FROM zlest0061 INTO TABLE it_zlest0061
           WHERE docnum EQ wl_doc-docnum.

          IF it_zlest0061[] IS NOT INITIAL.
            SELECT *
              FROM zlest0060 INTO TABLE it_zlest0060
              FOR ALL ENTRIES IN it_zlest0061
            WHERE bukrs      EQ it_zlest0061-bukrs
              AND werks      EQ it_zlest0061-werks
              AND nr_viagem  EQ it_zlest0061-nr_viagem
              AND ano_viagem EQ it_zlest0061-ano_viagem
              AND nr_dco     EQ it_zlest0061-nr_dco
              AND safra      EQ it_zlest0061-safra
              AND cl_codigo  EQ it_zlest0061-cl_codigo
              AND nome_emb   EQ it_zlest0061-nome_emb
              AND docnum     EQ it_zlest0061-docnum.

            LOOP AT it_zlest0060 INTO DATA(wl_zlest0060).
              READ TABLE it_zlest0061 INTO DATA(wl_zlest0061) WITH KEY bukrs      = wl_zlest0060-bukrs
                                                                       werks      = wl_zlest0060-werks
                                                                       nr_viagem  = wl_zlest0060-nr_viagem
                                                                       ano_viagem = wl_zlest0060-ano_viagem
                                                                       nr_dco     = wl_zlest0060-nr_dco
                                                                       safra      = wl_zlest0060-safra
                                                                       cl_codigo  = wl_zlest0060-cl_codigo
                                                                       nome_emb   = wl_zlest0060-nome_emb
                                                                       docnum     = wl_zlest0060-docnum.
              IF ( sy-subrc EQ 0 ).
                UPDATE zsdt0001 SET docnum_aquav =  wl_doc-docnum
                                    ct_aquav     = 'X'
                              WHERE nr_romaneio  EQ wl_zlest0060-nr_romaneio
                                AND tp_movimento EQ 'E'
                                AND nr_safra     EQ wl_zlest0060-safra
                                AND bukrs        EQ wl_zlest0060-bukrs
                                AND branch       EQ wl_zlest0060-dt_codigo.
              ENDIF.
            ENDLOOP.
          ENDIF.

        WHEN '58'.

          CLEAR: wl_zsdt0102.

          SELECT SINGLE *
            FROM zsdt0102 INTO wl_zsdt0102
           WHERE docnum = wl_doc-docnum.

          CHECK sy-subrc EQ 0.

          wl_zsdt0102-tp_authcod    = '4'.
          wl_zsdt0102-authcode      = i_acttab-authcod.
          wl_zsdt0102-dt_authcod    = i_acttab-authdate .
          wl_zsdt0102-hr_authcod    = i_acttab-authtime .
          wl_zsdt0102-code          = i_acttab-code.
          wl_zsdt0102-dt_atualizado = sy-datum.
          wl_zsdt0102-hr_atualizado = sy-uzeit.
          wl_zsdt0102-msg           = wl_zsdt0234-text.
          wl_zsdt0102-cdv           = i_acttab-cdv.
          wl_zsdt0102-docnum9       = i_acttab-docnum9.
          wl_zsdt0102-status        = '1'.
          wl_zsdt0102-autorizado    = 'X'.

          CALL FUNCTION 'Z_GRC_MONTA_LINK'
            EXPORTING
              i_docnum   = wl_doc-docnum
            IMPORTING
              e_link_pdf = v_url.

          wl_zsdt0102-url_sefaz     = v_url.

          CLEAR: wl_zsdt0102-transmissao.

          MODIFY zsdt0102 FROM wl_zsdt0102.

          "Registra Historico
          CLEAR: wl_zsdt0107.

          wl_zsdt0107-docnum        = wl_zsdt0102-docnum.
          wl_zsdt0107-nmdfe         = wl_zsdt0102-nmdfe.
          wl_zsdt0107-tp_authcod    = '4'.
          wl_zsdt0107-authcode      = i_acttab-authcod.
          wl_zsdt0107-dt_authcod    = i_acttab-authdate.
          wl_zsdt0107-hr_authcod    = i_acttab-authtime.
          wl_zsdt0107-code          = i_acttab-code.
          wl_zsdt0107-msg           = wl_zsdt0234-text.

          INSERT zsdt0107 FROM wl_zsdt0107.

      ENDCASE.

      CALL FUNCTION 'Z_GRC_REGISTRA_INF_ZIB_NFE'
        EXPORTING
          i_docnum = wl_doc-docnum
          i_active = i_acttab.

    WHEN '2' OR  "Recusado
         '3'.    "Rejeitado

      CASE wl_doc-model.
        WHEN '55'.

          CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
            EXPORTING
              i_tp_mov  = 'S'
              i_docnum  = wl_doc-docnum
              i_del_mov = 'X'.

        WHEN '57'.

*          IF wl_doc-xmlvers >= 4.
*            UPDATE j_1bnfe_active
*               SET docsta = '3'
*             WHERE docnum EQ wl_doc-docnum. "MDF-e não tem inutilização. Status para estornar documento direto
*          ENDIF.

        WHEN '58'.

          CLEAR: wl_zsdt0102.

          SELECT SINGLE *
            FROM zsdt0102 INTO wl_zsdt0102
           WHERE docnum = wl_doc-docnum.

          CHECK sy-subrc EQ 0.

          wl_zsdt0102-status        = '3'.
          wl_zsdt0102-code          = i_acttab-code.
          wl_zsdt0102-dt_atualizado = sy-datum.
          wl_zsdt0102-hr_atualizado = sy-uzeit.
          wl_zsdt0102-msg           = wl_zsdt0234-text.

          CLEAR: wl_zsdt0102-transmissao.

          CLEAR: wl_zsdt0102-docnum9, wl_zsdt0102-cdv, wl_zsdt0102-authcode.

          MODIFY zsdt0102 FROM wl_zsdt0102.

          UPDATE j_1bnfe_active
             SET docsta = '3'
           WHERE docnum EQ wl_doc-docnum. "MDF-e não tem inutilização. Status para estornar documento direto

      ENDCASE.

      "Registra Log Erro
      IF wl_doc-model EQ '55' OR wl_doc-model EQ '57' OR wl_doc-model EQ '58'.
        CLEAR: wl_err_tab.

        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
          EXPORTING
            input  = 'PT'
          IMPORTING
            output = wl_err_tab-msgspra.

        wl_err_tab-msgtyp  = 'E'.
        wl_err_tab-msgid   = '000'.
        wl_err_tab-msgnr   = 1.

        CASE wl_doc-model.
          WHEN '55'.
            wl_err_tab-msgv1   = wl_j_1bstscodet-text+000(50).
            wl_err_tab-msgv2   = wl_j_1bstscodet-text+050(30).
          WHEN '57'.
            wl_err_tab-msgv1   = wl_j_1bstscodet-textcte+000(50).
            wl_err_tab-msgv2   = wl_j_1bstscodet-textcte+050(30).
          WHEN '58'.
            wl_err_tab-msgv1   = wl_zsdt0234-text+000(50).
            wl_err_tab-msgv2   = wl_zsdt0234-text+050(50).
            wl_err_tab-msgv3   = wl_zsdt0234-text+100(50).
            wl_err_tab-msgv4   = wl_zsdt0234-text+150(50).
        ENDCASE.

        APPEND wl_err_tab TO it_err_tab.

        CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'
          EXPORTING
            i_docnum   = wl_doc-docnum
            i_tab_only = 'X'
          TABLES
            it_err_tab = it_err_tab.
      ENDIF.

      CALL FUNCTION 'Z_GRC_REGISTRA_LOG_DOC'
        EXPORTING
          i_docnum     = wl_doc-docnum
          i_event_flag = abap_false.

  ENDCASE.


ENDFUNCTION.
