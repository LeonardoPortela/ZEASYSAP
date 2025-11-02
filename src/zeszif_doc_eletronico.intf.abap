interface ZESZIF_DOC_ELETRONICO
  public .


  class-data AT_INSTANCE type ref to ZESZIF_DOC_ELETRONICO .
  data AT_DOCUMENTO type J_1BNFDOC .
  data AT_INFO_DOC_ELETRONICO type J_1BNFE_ACTIVE .
  constants AT_ST_MODEL_NFE type J_1BMODEL value '55' ##NO_TEXT.
  constants AT_ST_MODEL_CTE type J_1BMODEL value '57' ##NO_TEXT.
  constants AT_ST_MODEL_MDFE type J_1BMODEL value '58' ##NO_TEXT.
  data AT_SEM_LOCK type CHAR01 .
  constants AT_ACAO_VIAGEM_FRETE_CRIAR type CHAR01 value '1' ##NO_TEXT.
  constants AT_ACAO_VIAGEM_FRETE_AUTORIZAR type CHAR01 value '2' ##NO_TEXT.
  constants AT_ACAO_VIAGEM_FRETE_CANCELAR type CHAR01 value '3' ##NO_TEXT.
  constants AT_ACAO_MDFE_AUTORIZA type CHAR01 value '1' ##NO_TEXT.
  constants AT_ACAO_MDFE_CANCELAR type CHAR01 value '2' ##NO_TEXT.
  data AT_QTD_CICLOS type ZESZDE_QTD_CICLOS .
  data AT_QTD_SEGUNDOS type ZESZDE_QTD_SEGUNDOS_CICLO .

  class-methods GET_INSTANCE
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  class-methods GET_XML_TERCEIRO
    importing
      !I_CHAVE type ZESZDE_CHAVE_NFE
    exporting
      !E_XML_XSTRING type FPCONTENT
      !E_XML_STRING type STRING
    returning
      value(R_XML) type XSTRING
    raising
      ZESZCX_DOC_ELETRONICO .
  class-methods IMPRIMIR_DOCUMENTO_AUX
    importing
      !I_CHAVE type ZESZDE_CHAVE_DOC_E .
  methods SET_AUTORIZAR
    importing
      !I_AGUARDAR type CHAR01 default ' '
      !I_CICLOS type ZESZDE_QTD_CICLOS default 0
      !I_SEGUNDOS type ZESZDE_QTD_SEGUNDOS_CICLO default 0
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_CANCELAR
    importing
      !I_MOTIVO type J_1BNFE_CANCEL_REASON optional
      !I_DS_MOTIVO type STRING optional
      !I_AGUARDAR type CHAR01 default ' '
      !I_CICLOS type ZESZDE_QTD_CICLOS default 0
      !I_SEGUNDOS type ZESZDE_QTD_SEGUNDOS_CICLO default 0
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_DOC_ACESSORIO
    importing
      !E_PDF type STRING
      !E_URI type STRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    exceptions
      ZESZCX_DOC_ELETRONICO .
  methods GET_PDF
    exporting
      !E_PDF type XSTRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_XML
    exporting
      !E_XML type XSTRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_REGISTRO
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_SEM_BLOQUEIO type CHAR01 default ABAP_FALSE
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_STATUS
    exporting
      !E_STATUS type ZESZDE_STATUS_DOC_ELETRONICO
      !E_STATUS_DESC type STRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO .
  methods SET_DET_NUMERO
    exporting
      !E_J_1BNFE_ACTIVE type J_1BNFE_ACTIVE
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_AGUARDAR
    importing
      !I_AGUARDAR type CHAR01 default ' '
      !I_CICLOS type ZESZDE_QTD_CICLOS default 0
      !I_SEGUNDOS type ZESZDE_QTD_SEGUNDOS_CICLO default 0
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_ELIMINAR_LOGS
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_REINICIALIZAR
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_REGISTRO
    exporting
      !E_DOCUMENTO type J_1BNFDOC
      !E_INFO_DOC_ELETRONICO type J_1BNFE_ACTIVE
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO .
  methods SET_CLEAR
    importing
      !I_CICLOS type ZESZDE_QTD_CICLOS default 60
      !I_SEGUNDOS type ZESZDE_QTD_SEGUNDOS_CICLO default 10
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_BLOQUEAR_REGISTRO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_LIBERAR_REGISTRO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_ESTORNO_MDFE_POSSIVEL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_DOC_CANCEL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_ESTORNAR_DOC_ORIGEM
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_DOC_NAO_CANCEL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_VERIFICA_MODAL
    importing
      !I_MODAL type J_1BMODEL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_DOC_PROPRIO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_NUMERO_DETERMINADO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_NUMERO_NAO_DETERMINADO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_VALOR_DOCUMENTO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_DOC_RELACIONADO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_SEM_ERRO_AUTORIZACAO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_SEM_ERRO_CANCELAMENTO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_FATURA_ATIVA
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_AUTORIZADO_USO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_AUTORIZADO_CANCEL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_NAO_PROCESSAMENTO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_CERTIDAO_NEGATIVA
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_DATA_DOCUMENTO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_NAO_PROC_CANCEL
    exporting
      !R_INSTANCIA type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_NAO_PROC_INUTILIZACAO
    exporting
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_DETERMINAR_NUMERO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_AGUARDANDO_AUT_USO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_VAL_AUTORIZACAO_MODAL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_VAL_CANCELAMENTO_MODAL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_ERRO_GERAL
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_ERRO_GERAL_STRING
    importing
      !I_TEXTO type STRING
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_REGISTRA_AUTORIZACAO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO .
  methods GET_VAL_REINICIALIZAR_MODAL
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_ENVIAR_AUTORIZACAO_USO
    importing
      !I_RESEND type CHAR01 default ' '
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_ALTERA_STATUS
    importing
      !I_TP_AUTHCOD type CHAR01
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_REGISTRA_CANCELAMENTO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_ENVIAR_CANCELAMENTO
    importing
      !I_RESEND type CHAR01 default ' '
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_MOTIVO_CANCELAMENTO
    importing
      !I_MOTIVO type J_1BNFE_CANCEL_REASON
      !I_DS_MOTIVO type STRING optional
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_MOTIVO_CANCELAMENTO
    importing
      !I_MOTIVO type J_1BNFE_CANCEL_REASON
    exporting
      !E_MOTIVO type J_1BNFE_CANCELRT
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_LOGS_ERRO
    exporting
      !E_BALHDR type BALHDR_T
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_BLOQUEAR
    importing
      !I_BLOQUEAR type CHAR01
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_AUTORIZA_VIAGEM_TIP_FRETE
    importing
      !I_ACAO type CHAR01 default ZESZIF_DOC_ELETRONICO=>AT_ACAO_VIAGEM_FRETE_CRIAR
      !I_MOTIVO_CANCEL type J_1BNFE_CANCEL_REASON default '01'
    exporting
      !E_JUSTIFICATIVA_CANCEL type STRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_AUTORIZA_MDFE
    importing
      !I_ACAO type CHAR01 default ZESZIF_DOC_ELETRONICO=>AT_ACAO_MDFE_AUTORIZA
      !I_JUSTIFICATIVA_CANCEL type STRING optional
      !I_AGUARDAR type CHAR01 default ' '
      !I_CICLOS type ZESZDE_QTD_CICLOS default 0
      !I_SEGUNDOS type ZESZDE_QTD_SEGUNDOS_CICLO default 0
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_CK_ESTORNO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_XML_GRC
    exporting
      value(E_RETORNO) type SYSUBRC
      value(E_XML_XSTRING) type FPCONTENT
      value(E_XML_STRING) type STRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_URLS_DOCS
    importing
      !I_ID_CCE type NUM optional
    exporting
      !E_LINK_PDF type STRING
      !E_LINK_XML type STRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods DOWNLOAD_DOC_FISCAL
    importing
      !I_XML type CHAR01 default ABAP_TRUE
      !I_PDF type CHAR01 default ABAP_TRUE
    exporting
      !E_XML type XSTRING
      !E_PDF type XSTRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_LOAD_LOG
    exporting
      !ET_MESSAGE type BAL_T_MSG
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods SET_CLEAR_LOG_ERRO
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
  methods GET_XML_EVENTO_GRC
    importing
      !I_TPEVENTO type NUMC06
    exporting
      value(E_RETORNO) type SYSUBRC
      value(E_XML_XSTRING) type FPCONTENT
      value(E_XML_STRING) type STRING
    returning
      value(R_INSTANCIA) type ref to ZESZIF_DOC_ELETRONICO
    raising
      ZESZCX_DOC_ELETRONICO .
endinterface.
