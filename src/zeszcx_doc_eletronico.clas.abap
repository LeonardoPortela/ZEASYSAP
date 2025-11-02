class ZESZCX_DOC_ELETRONICO definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ERRO_GERAL .
  constants:
    begin of ZCX_DOC_CABE_NAO_ENC,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_CABE_NAO_ENC .
  constants:
    begin of ZCX_DOC_ELETRONIC_NAO_ENC,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_ELETRONIC_NAO_ENC .
  constants:
    begin of ZCX_DOC_CANCELADO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_CANCELADO .
  constants:
    begin of ZCX_ERRO_MODELO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_MODELO .
  constants:
    begin of ZCX_ERRO_EMISSAO_PROPRIA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_EMISSAO_PROPRIA .
  constants:
    begin of ZCX_ERRO_NUM_NAO_DETERMINADO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_NUM_NAO_DETERMINADO .
  constants:
    begin of ZCX_DOC_NAO_RELACIONADO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_NAO_RELACIONADO .
  constants:
    begin of ZCX_MODELO_NAO_PREVISTO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MODELO_NAO_PREVISTO .
  constants:
    begin of ZCX_ERRO_AUTORIZACAO_USO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_AUTORIZACAO_USO .
  constants:
    begin of ZCX_ERRO_FATURA_ESTORNADA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_FATURA_ESTORNADA .
  constants:
    begin of ZCX_NAO_AUTORIZADO_USO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NAO_AUTORIZADO_USO .
  constants:
    begin of ZCX_EM_PROCESSAMENTO_AUT_USO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EM_PROCESSAMENTO_AUT_USO .
  constants:
    begin of ZCX_SEM_CERTIDAO_NEGATIVA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_CERTIDAO_NEGATIVA .
  constants:
    begin of ZCX_DATA_EMISSAO_RETROATIVA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_EMISSAO_RETROATIVA .
  constants:
    begin of ZCX_NOTA_FISCAL_NAO_AUTORIZADA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NOTA_FISCAL_NAO_AUTORIZADA .
  constants:
    begin of ZCX_NOTA_FISCAL_CANCELADA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '016',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NOTA_FISCAL_CANCELADA .
  constants:
    begin of ZCX_VT_SEM_ORGANIZACAO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VT_SEM_ORGANIZACAO .
  constants:
    begin of ZCX_VT_SEM_REGISTRO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '018',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VT_SEM_REGISTRO .
  constants:
    begin of ZCX_VT_SEM_INICIO_CARREG,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VT_SEM_INICIO_CARREG .
  constants:
    begin of ZCX_VT_SEM_FIM_CARREG,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VT_SEM_FIM_CARREG .
  constants:
    begin of ZCX_VT_SEM_PROCMTO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VT_SEM_PROCMTO .
  constants:
    begin of ZCX_VT_SEM_INICIO_TRANSPORTE,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '022',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_VT_SEM_INICIO_TRANSPORTE .
  constants:
    begin of ZCX_MOTIVO_CANCEL_NAO_ENC,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '023',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MOTIVO_CANCEL_NAO_ENC .
  constants:
    begin of ZCX_EM_PROCESSAMENTO_AUT_CAN,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EM_PROCESSAMENTO_AUT_CAN .
  constants:
    begin of ZCX_NAO_AUTORIZADO_CANCEL,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '025',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NAO_AUTORIZADO_CANCEL .
  constants:
    begin of ZCX_NAO_NECESSARIO_NUMERO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NAO_NECESSARIO_NUMERO .
  constants:
    begin of ZCX_EM_PROCESSAMENTO_AUT_INUT,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '040',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EM_PROCESSAMENTO_AUT_INUT .
  constants:
    begin of ZCX_NAO_AGUARD_AUT_USO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NAO_AGUARD_AUT_USO .
  constants:
    begin of ZCX_SEM_CONTRATO_VIAGEM,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '028',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_CONTRATO_VIAGEM .
  constants:
    begin of ZCX_SEM_CONTRATO_CREDIDATO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '029',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_CONTRATO_CREDIDATO .
  constants:
    begin of ZCX_SEM_CONTRATO_CANCELADO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '030',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_CONTRATO_CANCELADO .
  constants:
    begin of ZCX_MDFE_NAO_AUTORIZADA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '031',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MDFE_NAO_AUTORIZADA .
  constants:
    begin of ZCX_MDFE_NAO_CANCELADA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '032',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MDFE_NAO_CANCELADA .
  constants:
    begin of ZCX_CTE_SEM_ZCTE_IDENTIFICA,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '033',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CTE_SEM_ZCTE_IDENTIFICA .
  constants:
    begin of ZCX_SEM_MOTIVO_CANCEL,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '034',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_MOTIVO_CANCEL .
  constants:
    begin of ZCX_DOC_NAO_CANCELADO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '035',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_NAO_CANCELADO .
  constants:
    begin of ZCX_MDFE_AUTORIZADA_24HRS,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '036',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MDFE_AUTORIZADA_24HRS .
  constants:
    begin of ZCX_CTE_CANCEL_EXPIROU,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '037',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CTE_CANCEL_EXPIROU .
  constants:
    begin of ZCX_ERRO_NUM_DETERMINADO,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '038',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_NUM_DETERMINADO .
  constants:
    begin of ZCX_DOC_CANCEL,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '045',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_DOC_CANCEL .
  constants:
    begin of ZCX_NAO_EST_DOC_ORIGEM,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '039',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NAO_EST_DOC_ORIGEM .
  constants:
    begin of ZCX_ERRO_REINICIALIZAR_DOC,
      msgid type symsgid value 'ZDOCE',
      msgno type symsgno value '046',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_REINICIALIZAR_DOC .
  data MSGTY type SYST_MSGTY .
  data MSGNO type SYST_MSGNO .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data MSGID type SYST_MSGID .
  data TRANSACAO type TCODE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYST_MSGTY optional
      !MSGNO type SYST_MSGNO optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !MSGID type SYST_MSGID optional
      !TRANSACAO type TCODE optional .
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY optional
      !I_MSGTY_DISPLAY type SYST_MSGTY optional .
protected section.
private section.
ENDCLASS.



CLASS ZESZCX_DOC_ELETRONICO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MSGID = MSGID .
me->TRANSACAO = TRANSACAO .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD PUBLISHED_ERRO.

    DATA: P_MSGTY	        TYPE SYST_MSGTY,
          P_MSGTY_DISPLAY	TYPE SYST_MSGTY.


    IF I_MSGTY IS NOT INITIAL.
      P_MSGTY = I_MSGTY.
    ELSE.
      P_MSGTY = ME->MSGTY.
    ENDIF.

    IF I_MSGTY_DISPLAY IS NOT INITIAL.
      P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.
    ELSE.
      P_MSGTY_DISPLAY = ME->MSGTY.
    ENDIF.

    IF ME->TRANSACAO IS NOT INITIAL.
      IF ME->MSGV1 IS INITIAL.
        ME->MSGV1 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV2 IS INITIAL.
        ME->MSGV2 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV3 IS INITIAL.
        ME->MSGV3 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV4 IS INITIAL.
        ME->MSGV4 = ME->TRANSACAO.
      ENDIF.
    ENDIF.

    MESSAGE ID ME->MSGID TYPE P_MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
