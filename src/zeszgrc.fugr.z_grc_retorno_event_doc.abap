FUNCTION z_grc_retorno_event_doc .
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |11/11/2024 |Comentar comando DELETE na ta- &*
*&                                    |bela ZESZSDT_EXPORT e na tabela   &*
*&                                    |ZSDT_RETLOT. Chamado: 157683.  &*
*&--------------------------------------------------------------------&*
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_EVENT_COMMON) TYPE  J_1BNFE_EVENT
*"----------------------------------------------------------------------


  "Gerar event Tmp ---------------------------------------
  DATA: wl_event_tmp TYPE zevent_tmp,
        vg_lines     TYPE char10,
        it_zsdt0232  TYPE TABLE OF zsdt0232,
        wl_zsdt0232  TYPE zsdt0232.

  FREE: it_zsdt0232.
  CLEAR: wl_zsdt0232.

  SELECT       MAX( seq ) INTO @DATA(max_seq)
          FROM zevent_tmp
         WHERE docnum = @is_event_common-docnum.

  ADD  1 TO max_seq.

  CLEAR: wl_event_tmp.
  MOVE-CORRESPONDING is_event_common TO wl_event_tmp.

  wl_event_tmp-seq         = max_seq.
  wl_event_tmp-dt_registro = sy-datum.
  wl_event_tmp-hr_registro = sy-uzeit.
  wl_event_tmp-us_registro = sy-uname.

  MODIFY zevent_tmp FROM wl_event_tmp.
  COMMIT WORK.
  "Gerar event Tmp Fim ---------------------------------------

  CONSTANTS: lc_tz TYPE ttzz-tzone VALUE 'BRZLWE'.

  DATA: wl_zcarta_correcao TYPE zcarta_correcao,
        wl_j_1bstscodet    TYPE j_1bstscodet,
        wl_zsdt0234        TYPE zsdt0234,
        wl_zsdt0102        TYPE ZESZSDT0102,
        wl_zsdt0107        TYPE zsdt0107.

  DATA: lva_url            TYPE string.

  DATA: BEGIN OF wl_err_tab.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF wl_err_tab.

  DATA: it_err_tab LIKE STANDARD TABLE OF wl_err_tab.

  CLEAR: wl_j_1bstscodet, wl_zsdt0234.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(wl_doc)
   WHERE docnum EQ @is_event_common-docnum.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(wl_active)
   WHERE docnum EQ @wl_doc-docnum.

  CHECK sy-subrc EQ 0.

  IF is_event_common-code IS NOT INITIAL.
    SELECT SINGLE *
      FROM j_1bstscodet INTO wl_j_1bstscodet
     WHERE spras EQ 'P'
       AND code  EQ is_event_common-code.

    SELECT SINGLE *
      FROM zsdt0234 INTO wl_zsdt0234
     WHERE code EQ is_event_common-code.
  ENDIF.

  CASE is_event_common-ext_event. "Evento Externo
    WHEN '110110'.  "Carta de Correção  |================================================================================================================

      CLEAR: wl_zcarta_correcao.

      SELECT SINGLE *
        FROM zcarta_correcao INTO wl_zcarta_correcao
       WHERE docnum EQ wl_doc-docnum
         AND id_cc  EQ ( SELECT MAX( id_cc ) FROM zcarta_correcao WHERE docnum EQ wl_doc-docnum ).

      CHECK sy-subrc EQ 0.

      CASE is_event_common-docsta.
        WHEN '1' OR "Autorizado
            '2' OR "Recusado
            '3'.   "Rejeitado

          wl_zcarta_correcao-tp_authcod    = '3'.
          wl_zcarta_correcao-code          = is_event_common-code.
          wl_zcarta_correcao-authcode      = is_event_common-authcod .

          CONVERT TIME STAMP is_event_common-reply_tmpl TIME ZONE lc_tz
                   INTO DATE wl_zcarta_correcao-dt_authcod
                        TIME wl_zcarta_correcao-hr_authcod.

          IF is_event_common-code IS NOT INITIAL.
            CASE wl_doc-model.
              WHEN '55'.
                wl_zcarta_correcao-msg = wl_j_1bstscodet-text.
              WHEN '57'.
                wl_zcarta_correcao-msg = wl_j_1bstscodet-textcte.
            ENDCASE.
          ENDIF.

          wl_zcarta_correcao-dt_atualizado = sy-datum.
          wl_zcarta_correcao-hr_atualizado = sy-uzeit.

          IF ( is_event_common-docsta EQ '1' ) AND ( is_event_common-authcod IS NOT INITIAL )  .

            CALL FUNCTION 'Z_GRC_MONTA_LINK'
              EXPORTING
                i_docnum   = wl_doc-docnum
                i_id_cce   = wl_zcarta_correcao-id_cc
              IMPORTING
                e_link_pdf = lva_url.

            wl_zcarta_correcao-ds_url = lva_url.

          ENDIF.

          MODIFY zcarta_correcao FROM  wl_zcarta_correcao.
      ENDCASE.

    WHEN '110111'.  "Cancelamento |====================================================================================================================

      CASE is_event_common-docsta.
        WHEN '1'. "Autorizado

          wl_active-cancel  = abap_true.

*          IF ( wl_doc-model EQ '55' ) OR ( wl_doc-model EQ '57' ).
*            TRY.
*                zcl_averbacao_seguro=>cancelar_averbacao_cte( i_docnum = wl_doc-docnum ).
*              CATCH zcx_averbacao_seguro.
*              CATCH zcx_arquivo.
*              CATCH zcx_cadastro.
*            ENDTRY.
*          ENDIF.

          CASE wl_doc-model.
            WHEN '55'.
              CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
                EXPORTING
                  i_tp_mov         = 'S'
                  i_docnum         = wl_doc-docnum
                  i_marcar_estorno = 'X'.

              DATA: lc_autorizacao TYPE zib_autorit_grc.

              lc_autorizacao-chnfe = wl_active-regio  && wl_active-nfyear  && wl_active-nfmonth &&
                                     wl_active-stcd1  && wl_active-model   && wl_active-serie &&
                                     wl_active-nfnum9 && wl_active-docnum9 && wl_active-cdv.

              lc_autorizacao-dt_registro   = sy-datum.
              lc_autorizacao-hr_registro   = sy-uzeit.
              lc_autorizacao-us_registro   = sy-uname.
              lc_autorizacao-rg_atualizado = '0'.
              lc_autorizacao-direction     = 'OUTB'.
              lc_autorizacao-docnum        = is_event_common-docnum.
              lc_autorizacao-code          = '101'.
              lc_autorizacao-cancel        = abap_true.
              MODIFY zib_autorit_grc FROM lc_autorizacao.
**<<<------"157683 - NMS - INI------>>>
*              DELETE FROM ZESZSDT_RETLOTE
*                WHERE docnum_ret EQ wl_doc-docnum.
*
*              DELETE FROM ZESZSDT_EXPORT
*               WHERE docnum EQ wl_doc-docnum.
**<<<------"157683 - NMS - FIM------>>>
              "Exclui Romaneio de Saída
              TRY.
                  zcl_carga_recebimento=>zif_carga~get_verifica_existe_saida( i_docnum = wl_doc-docnum i_excluir_romaneio = abap_true ).
                CATCH zcx_carga.
                CATCH zcx_cadastro.
              ENDTRY.

*** BUG - 71952 - Inicio - CBRAND
              SELECT SINGLE *
                FROM zsdt0231 INTO @DATA(wl_zsdt0231)
               WHERE docnum EQ @is_event_common-docnum.

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

                    UPDATE zib_nfe_forn SET docnum_ref =  ''
                                    user_identificador = sy-uname
                                    data_identificador = sy-datum
                                    hora_identificador = sy-uzeit
                    WHERE nu_chave EQ wl_zsdt0232-refnfe
                      AND docnum_ref = '0000000000'. " USER STORY 81890 24-08-2022 / Anderson Oenning

                  ENDIF.
                ENDIF.
              ENDIF.
*** BUG - 71952 - Fim - CBRAND


              "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
              DATA(lwa_romaneio_doc_fiscal) = zcl_les_utils=>get_romaneio_documento_fiscal( i_docnum = wl_doc-docnum ).

              IF lwa_romaneio_doc_fiscal-id_interface = '48' AND lwa_romaneio_doc_fiscal-nro_cg IS NOT INITIAL.
                SELECT SINGLE *
                  FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
                 WHERE nro_cg EQ @lwa_romaneio_doc_fiscal-nro_cg.

                IF sy-subrc EQ 0 AND lwa_zsdt0133-id_carga_safra_control IS NOT INITIAL.
                  DELETE FROM zsdt0422 WHERE nro_cg = lwa_romaneio_doc_fiscal-nro_cg AND tipo_anexo = 'NFE'.
                  UPDATE zsdt0133 SET int_anexos_nfe_safra_ctrl = abap_false
                   WHERE nro_cg = lwa_romaneio_doc_fiscal-nro_cg.
                ENDIF.
              ENDIF.


              "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

            WHEN '57'.
            WHEN '58'.

              SELECT SINGLE *
                FROM ZESZSDT0102 INTO wl_zsdt0102
               WHERE docnum EQ wl_doc-docnum.

              CHECK sy-subrc EQ 0.

              wl_zsdt0102-tp_authcod    = '5'.
              wl_zsdt0102-code          = is_event_common-code.
              wl_zsdt0102-authcode      = is_event_common-authcod.

              CONVERT TIME STAMP is_event_common-reply_tmpl TIME ZONE lc_tz
                       INTO DATE wl_zsdt0102-dt_authcod
                            TIME wl_zsdt0102-hr_authcod.

              wl_zsdt0102-dt_atualizado = sy-datum.
              wl_zsdt0102-hr_atualizado = sy-uzeit.
              wl_zsdt0102-msg           = wl_zsdt0234-text.
              wl_zsdt0102-cancel        = 'X'.
              wl_zsdt0102-status        = '1'.

              CLEAR: wl_zsdt0102-transmissao.

              MODIFY ZESZSDT0102 FROM wl_zsdt0102.

              "Registra Historico
              CLEAR: wl_zsdt0107.

              wl_zsdt0107-docnum        = wl_zsdt0102-docnum.
              wl_zsdt0107-nmdfe         = wl_zsdt0102-nmdfe.
              wl_zsdt0107-tp_authcod    = '5'.
              wl_zsdt0107-authcode      = wl_zsdt0102-authcode.
              wl_zsdt0107-dt_authcod    = wl_zsdt0102-dt_authcod.
              wl_zsdt0107-hr_authcod    = wl_zsdt0102-hr_authcod.
              wl_zsdt0107-code          = wl_zsdt0102-code.
              wl_zsdt0107-msg           = wl_zsdt0234-text.

              INSERT zsdt0107 FROM wl_zsdt0107.

          ENDCASE.

          CALL FUNCTION 'Z_GRC_REGISTRA_INF_ZIB_NFE'
            EXPORTING
              i_docnum = wl_doc-docnum
              i_active = wl_active.


        WHEN '2' OR "Recusado
             '3'.   "Rejeitado

          CASE wl_doc-model.
            WHEN '55'.

            WHEN '57'.

            WHEN '58'.

              SELECT SINGLE *
                FROM ZESZSDT0102 INTO wl_zsdt0102
               WHERE docnum EQ wl_doc-docnum.

              CHECK sy-subrc EQ 0.

              wl_zsdt0102-code          = is_event_common-code.
              wl_zsdt0102-dt_atualizado = sy-datum.
              wl_zsdt0102-hr_atualizado = sy-uzeit.
              wl_zsdt0102-msg           = wl_zsdt0234-text.

              CLEAR: wl_zsdt0102-transmissao.

              MODIFY ZESZSDT0102 FROM wl_zsdt0102.

          ENDCASE.

          "Registra Log Erro
          IF wl_doc-model EQ '55' OR wl_doc-model EQ '57'.
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
              i_event_flag = abap_true.

      ENDCASE.

      "18.09.2019 - Ini
      IF wl_active-docsta EQ '1'.
        UPDATE j_1bnfe_active SET action_requ = 'C'
         WHERE docnum EQ wl_active-docnum.
      ENDIF.
      "18.09.2019 - Fim

    WHEN '110112'.  "Encerramento |========================================================================================================================

      CASE is_event_common-docsta.
        WHEN '1'. "Autorizado

          CASE wl_doc-model.
            WHEN '55'.
            WHEN '57'.
            WHEN '58'.

              SELECT SINGLE *
                FROM ZESZSDT0102 INTO wl_zsdt0102
               WHERE docnum EQ wl_doc-docnum.

              CHECK sy-subrc EQ 0.

              wl_zsdt0102-tp_authcod    = '6'.
              wl_zsdt0102-code          = is_event_common-code.
              wl_zsdt0102-authcode      = is_event_common-authcod.

              CONVERT TIME STAMP is_event_common-reply_tmpl TIME ZONE lc_tz
                       INTO DATE wl_zsdt0102-dt_authcod
                            TIME wl_zsdt0102-hr_authcod.

              wl_zsdt0102-dt_atualizado = sy-datum.
              wl_zsdt0102-hr_atualizado = sy-uzeit.
              wl_zsdt0102-msg           = wl_zsdt0234-text.
              wl_zsdt0102-encerrado     = 'X'.
              wl_zsdt0102-status        = '1'.

              CLEAR: wl_zsdt0102-transmissao.

              MODIFY ZESZSDT0102 FROM wl_zsdt0102.

              "Registra Historico
              CLEAR: wl_zsdt0107.

              wl_zsdt0107-docnum        = wl_zsdt0102-docnum.
              wl_zsdt0107-nmdfe         = wl_zsdt0102-nmdfe.
              wl_zsdt0107-tp_authcod    = '6'.
              wl_zsdt0107-authcode      = wl_zsdt0102-authcode.
              wl_zsdt0107-dt_authcod    = wl_zsdt0102-dt_authcod.
              wl_zsdt0107-hr_authcod    = wl_zsdt0102-hr_authcod.
              wl_zsdt0107-code          = wl_zsdt0102-code.
              wl_zsdt0107-msg           = wl_zsdt0234-text.

              INSERT zsdt0107 FROM wl_zsdt0107.

              "Notifica usuarios interessados sobre o encerramento
              zcl_mdfe=>notifica_encerramento( i_docnum = wl_zsdt0102-docnum ).


          ENDCASE.

        WHEN '2' OR "Recusado
             '3'.   "Rejeitado

          SELECT SINGLE *
            FROM ZESZSDT0102 INTO wl_zsdt0102
           WHERE docnum EQ wl_doc-docnum.

          CHECK sy-subrc EQ 0.

          wl_zsdt0102-code          = is_event_common-code.
          wl_zsdt0102-dt_atualizado = sy-datum.
          wl_zsdt0102-hr_atualizado = sy-uzeit.
          wl_zsdt0102-msg           = wl_zsdt0234-text.

          CLEAR: wl_zsdt0102-transmissao.

          MODIFY ZESZSDT0102 FROM wl_zsdt0102.

      ENDCASE.

  ENDCASE.

  UPDATE zevent_in SET rg_atualizado = abap_true
   WHERE docnum    = is_event_common-docnum
     AND ext_event = is_event_common-ext_event
     AND seqnum    = is_event_common-seqnum.

  COMMIT WORK.

ENDFUNCTION.
