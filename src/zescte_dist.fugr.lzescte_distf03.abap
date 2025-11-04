*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_AUTORIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_TLINES  text
*      -->P_WA_AUTORIZACAO  text
*----------------------------------------------------------------------*
FORM VISUALIZAR_AUTORIZACAO  TABLES   P_TL_TLINES STRUCTURE TLINE
                             USING    P_AUTORIZACAO TYPE ZIB_CTE_DIST_EAP.

*01	Autorizado
*02	Negado

  CK_READ_ONLY = ABAP_TRUE.

  MOVE-CORRESPONDING P_AUTORIZACAO TO ZIB_CTE_DIST_EAP.

  MOVE P_TL_TLINES[] TO TL_TLINES[].

  CASE P_AUTORIZACAO-TP_APROVACAO.
    WHEN '01'. "Chegada de Documentos
      CASE P_AUTORIZACAO-TP_AUTORIZADO.
        WHEN '01'. "Autorizado
          CK_BT_TELA = ICON_ALLOW.
        WHEN '02'. "Negado
          CK_BT_TELA = ICON_REJECT.
      ENDCASE.
    WHEN '02'. "Autorização de Pagamento Complemento
      CASE P_AUTORIZACAO-TP_AUTORIZADO.
        WHEN '01'. "Autorizado
          CK_BT_TELA = ICON_ALLOW.
        WHEN '02'. "Negado
          CK_BT_TELA = ICON_REJECT.
      ENDCASE.
    WHEN '03'. "Travar Pagamento
      CASE P_AUTORIZACAO-TP_AUTORIZADO.
        WHEN '01'. "Autorizado
          CK_BT_TELA = ICON_REJECT.
        WHEN '02'. "Negado
          CK_BT_TELA = ICON_ALLOW.
      ENDCASE.
  ENDCASE.

  CALL SCREEN 0100 STARTING AT 5 5.

ENDFORM.
