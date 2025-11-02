FUNCTION z_grc_retorno_event_inb .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_EVENT) TYPE  ZDE_RET_EVENT_INB
*"     VALUE(I_FORCE_DADOS_AUTH) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_ANULAR_REJEICAO) TYPE  CHAR01 OPTIONAL
*"----------------------------------------------------------------------


  "Gerar event Tmp ---------------------------------------
  DATA: wl_event_tmp TYPE zevent_tmp.

  SELECT MAX( seq ) INTO @DATA(max_seq)
    FROM zevent_tmp
   WHERE chave = @i_event-chave.

  ADD 1 TO max_seq.

  CLEAR: wl_event_tmp.
  MOVE-CORRESPONDING i_event TO wl_event_tmp.

  wl_event_tmp-seq         = max_seq.
  wl_event_tmp-dt_registro = sy-datum.
  wl_event_tmp-hr_registro = sy-uzeit.
  wl_event_tmp-us_registro = sy-uname.

  MODIFY zevent_tmp FROM wl_event_tmp.
  COMMIT WORK.
  "Gerar event Tmp Fim ---------------------------------------

  DATA: wa_zsdt0127 TYPE zsdt0127.

  CASE i_event-ext_event. "Evento Externo

    WHEN '210210' OR  "Ciência da Operação
         '210200' OR  "Confirmação da Operação
         '210240' OR  "Operação não Realizada
         '210220' OR  "Desconhecimento da Operação
         '610110'.    "Desacordo de Entrega de Serviços (CT-e) "CS2022000243-#76365-20.04.2022-JT-inicio

      CLEAR: wa_zsdt0127.

      SELECT SINGLE *
        FROM zsdt0127 INTO wa_zsdt0127
       WHERE chave         EQ i_event-chave
         AND cd_operacao   EQ i_event-ext_event
         AND doc_manifesto EQ ( SELECT MAX( doc_manifesto )
                                  FROM zsdt0127
                                 WHERE chave         EQ i_event-chave
                                   AND cd_operacao   EQ i_event-ext_event ).

*#127333 - 04.12.2023 - JT - inicio
      IF sy-subrc = 0 AND i_anular_rejeicao IS NOT INITIAL.
        wa_zsdt0127-rejeicao_anulada = abap_true.
        wa_zsdt0127-user_anula       = sy-uname.
        wa_zsdt0127-data_anula       = sy-datum.
        wa_zsdt0127-hora_anula       = sy-uzeit.
        MODIFY zsdt0127           FROM wa_zsdt0127.
        COMMIT WORK.
        EXIT.
      ENDIF.
*#127333 - 04.12.2023 - JT - fim

      IF ( sy-subrc EQ 0 AND wa_zsdt0127-autorizado IS INITIAL ) OR "Atualizar registro se o mesmo não tiver dados de autorização
         ( sy-subrc NE 0  ). "Não tem evento no SAP para o evento.

        IF ( wa_zsdt0127-chave IS INITIAL ) AND ( wa_zsdt0127-doc_manifesto IS INITIAL ). "Não tem registro para o evento, gerar um novo.

          wa_zsdt0127-chave       = i_event-chave.
          wa_zsdt0127-cd_operacao = i_event-ext_event.
          wa_zsdt0127-usnam       = sy-uname.
          wa_zsdt0127-data_emi    = sy-datum.
          wa_zsdt0127-hora_emi    = sy-uzeit.

          SELECT SINGLE *
            FROM ZESZIB_NFE_DIST_TER INTO @DATA(wl_dist_ter)
           WHERE chave_nfe EQ @i_event-chave.

          IF sy-subrc EQ 0.
            wa_zsdt0127-bukrs     = wl_dist_ter-bukrs.
            wa_zsdt0127-branch    = wl_dist_ter-branch.
            wa_zsdt0127-cnpj_dest = wl_dist_ter-destino_cnpj.
            wa_zsdt0127-ie_dest   = wl_dist_ter-destino_ie.
          ENDIF.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZMAN_DEST'
            IMPORTING
              number      = wa_zsdt0127-doc_manifesto.
        ENDIF.

        wa_zsdt0127-tp_authcod    =  '4'.
        wa_zsdt0127-code          = i_event-code.
        wa_zsdt0127-msg_retorno   = i_event-motivo.
        wa_zsdt0127-dt_atualizado = sy-datum.
        wa_zsdt0127-hr_atualizado = sy-uzeit.

        IF ( i_event-protocolo IS NOT INITIAL ) or ( i_event-code = '135' ).

          wa_zsdt0127-authcode      = i_event-protocolo.
          wa_zsdt0127-status        = '1'.
          wa_zsdt0127-autorizado    = 'X'.

          PERFORM zf_get_data_utc USING i_event-dhregevento
                               CHANGING wa_zsdt0127-dt_authcod.

          PERFORM zf_get_hora_utc USING i_event-dhregevento
                               CHANGING wa_zsdt0127-hr_authcod.
        ELSE.
          wa_zsdt0127-status        = '3'.
        ENDIF.

        MODIFY zsdt0127 FROM wa_zsdt0127.

      ELSEIF sy-subrc EQ 0 AND i_force_dados_auth IS NOT INITIAL.

        IF ( i_event-protocolo IS NOT INITIAL ).

          wa_zsdt0127-authcode      = i_event-protocolo.
          wa_zsdt0127-status        = '1'.
          wa_zsdt0127-autorizado    = 'X'.

          PERFORM zf_get_data_utc USING i_event-dhregevento
                               CHANGING wa_zsdt0127-dt_authcod.

          PERFORM zf_get_hora_utc USING i_event-dhregevento
                               CHANGING wa_zsdt0127-hr_authcod.

          MODIFY zsdt0127 FROM wa_zsdt0127.

        ENDIF.

      ENDIF.

  ENDCASE.

  COMMIT WORK.

ENDFUNCTION.
