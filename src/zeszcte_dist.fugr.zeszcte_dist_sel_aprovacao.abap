FUNCTION ZCTE_DIST_SEL_APROVACAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CD_CHAVE_CTE) TYPE  ZESZDE_CHAVE_DOC_E
*"  EXPORTING
*"     REFERENCE(E_DOMNAME) TYPE  CHAR30
*"     REFERENCE(E_DDTEXT) TYPE  VAL_TEXT
*"  CHANGING
*"     VALUE(E_TP_APROVACAO) TYPE  ZESZDE_TIP_APROVACAO
*"  EXCEPTIONS
*"      CTE_NAO_LOC
*"      SEM_AUTORIZACAO
*"      SEM_ESCOLHA
*"----------------------------------------------------------------------

  DATA: IT_LIBERADO TYPE TABLE OF DD07V WITH HEADER LINE,
        LC_QTD_LIB  TYPE I.

  FIELD-SYMBOLS: <FS_DOMI> TYPE DD07V.

  "01	Chegada de Documentos
  "02	Autoriza├º├úo de Pagamento Complemento
  "03	Travar Pagamento
  "04 Entrada de Peso Manual
  "05	Liberar Peso CT-e Ferrovi├írio
  "06	Autoriza├º├úo de Pagamento por Produto

  DATA: WA_CTE         TYPE ZESZIB_CTE_DIST_TER,
        IT_N55         TYPE TABLE OF ZESZIB_CTE_DIST_N55,
        WA_N55         TYPE ZESZIB_CTE_DIST_N55,
        CK_PESO_MANUAL TYPE C LENGTH 1.

  CLEAR: WA_CTE, IT_N55, IT_N55[].

  SELECT SINGLE * INTO WA_CTE
    FROM ZESZIB_CTE_DIST_TER
   WHERE CD_CHAVE_CTE EQ I_CD_CHAVE_CTE.

  "Verifica se a CT-e Existe
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE E001 WITH I_CD_CHAVE_CTE RAISING CTE_NAO_LOC.
  ENDIF.

  "Verifica acesso de Chegada de Documentos
  IF E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '01'.
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '01'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "01	Chegada de Documentos
      IT_LIBERADO-DOMNAME	   = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '01'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.

  "Verifica acesso de Autoriza├º├úo de Pagamento Complemento
  IF ( E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '02' ) AND WA_CTE-CD_TIPO_CTE EQ '1'.
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '02'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "02	Autoriza├º├úo de Pagamento Complemento
      IT_LIBERADO-DOMNAME	   = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '02'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.

  "Verifica acesso de Travar Pagamento
  IF E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '03'.
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '03'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "03 Travar Pagamento
      IT_LIBERADO-DOMNAME	   = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '03'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.

* Verifica acesso de entrada de peso manula para remessas de forma├º├úo de lote - ZRFL e ZRDC
* Somente Rodovi├írio

* Deve ser disponibilizado uma autoriza├º├úo de entrada de peso e data, para modal rodovi├írio,
* de chegada manual para os fretes de mercadoria de sob ordem de venda de remessa forma├º├úo
* de lote (ZRFL) ou remessa de DCO (ZRDC), pois as mesmas por default n├úo bloqueadas n├úo podendo
* o usu├írio entrar de forma manual, porem em alguns casos as cargas n├úo s├úo descarregadas em seu
* local de destino, com isso n├úo ter├úo uma entrada de peso e data de chegada.

  IF ( E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '04' ) AND ( WA_CTE-CD_MODAL EQ '01' ) AND ( WA_CTE-CD_TIPO_CTE NE '1' ).
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '04'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.

      CK_PESO_MANUAL = ABAP_FALSE.

      SELECT * INTO TABLE IT_N55
        FROM ZESZIB_CTE_DIST_N55
       WHERE CD_CHAVE_CTE EQ WA_CTE-CD_CHAVE_CTE.

      LOOP AT IT_N55 INTO WA_N55.
        CASE WA_N55-AUART_VA.
          WHEN 'ZRFL'.
            CK_PESO_MANUAL = ABAP_TRUE.
          WHEN 'ZRDC'.
            CK_PESO_MANUAL = ABAP_TRUE.
        ENDCASE.
      ENDLOOP.

      IF CK_PESO_MANUAL = ABAP_TRUE.
        "04 Entrada de Peso Manual
        IT_LIBERADO-DOMNAME	   = 'ZESZDM_TIP_APROVACAO'.
        IT_LIBERADO-DOMVALUE_L = '04'.
        APPEND IT_LIBERADO.
      ENDIF.
    ENDIF.
  ENDIF.

  "Verifica acesso de Travar Pagamento
  IF ( E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '05' ) AND ( WA_CTE-CK_FINALIZADO EQ ABAP_FALSE ).
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '05'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "03 Travar Pagamento
      IT_LIBERADO-DOMNAME	   = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '05'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.

  IF E_TP_APROVACAO IS INITIAL.
    "Verifica acesso de Diferen├ºa de Peso CT-e Ferrovi├írio
    AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '08'.
    IF SY-SUBRC IS INITIAL.
      "08	Libera├º├úo de Difer├¬n├ºa de Peso da CT-e
      IT_LIBERADO-DOMNAME	   = 'ZESZDM_AUT_FRETE_TER'.
      IT_LIBERADO-DOMVALUE_L = '08'.
      APPEND IT_LIBERADO.
    ENDIF.

    IF WA_CTE-CD_MODAL NE '04'.
      "Verifica Autoriza├º├úo de Pagamento - Fardos de Algod├úo
      AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '01'.
      IF SY-SUBRC IS INITIAL.
        "ZESZDM_AUT_FRETE_TER
        "01 Autorizar Pagamento
        IT_LIBERADO-DOMNAME	   = 'ZESZDM_AUT_FRETE_TER'.
        IT_LIBERADO-DOMVALUE_L = '01'.
        APPEND IT_LIBERADO.
      ENDIF.
    ENDIF.
  ENDIF.

  "Verifica acesso de Ignorar a S├®rie na Busca de Nota Fiscal (Modelo 01/04)
  IF ( E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '06' ) AND ( WA_CTE-CK_FINALIZADO EQ ABAP_FALSE ).
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '06'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "03 Travar Pagamento
      IT_LIBERADO-DOMNAME	   = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '06'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.

  "07  Liberar Modifica├º├úo Nota Fiscal (Modelo 01/1A/04)
  IF ( E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '07' ) AND ( WA_CTE-CK_FINALIZADO EQ ABAP_FALSE ).
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '07'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "07 Liberar Edi├º├úo/Exclus├úo/Inser├º├úo de Nota Fiscal Modelo 01/1A/04 da CT-e.
      IT_LIBERADO-DOMNAME    = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '07'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.

  "08  Liberar Modifica├º├úo Nota Fiscal (Modelo 55)
  IF ( E_TP_APROVACAO IS INITIAL OR E_TP_APROVACAO EQ '08' ) AND ( WA_CTE-CK_FINALIZADO EQ ABAP_FALSE ).
    AUTHORITY-CHECK OBJECT 'ZEAP_CTE'
      ID 'ZT_EAP_CTE' FIELD '08'
      ID 'ZE_EAP_CTE' FIELD WA_CTE-E_TOMADORA.

    IF SY-SUBRC IS INITIAL.
      "07 Liberar Edi├º├úo/Exclus├úo/Inser├º├úo de Nota Fiscal Modelo 01/1A/04 da CT-e.
      IT_LIBERADO-DOMNAME    = 'ZESZDM_TIP_APROVACAO'.
      IT_LIBERADO-DOMVALUE_L = '08'.
      APPEND IT_LIBERADO.
    ENDIF.
  ENDIF.


  IF IT_LIBERADO[] IS INITIAL.
    MESSAGE E157 RAISING SEM_AUTORIZACAO.
  ENDIF.

  DESCRIBE TABLE IT_LIBERADO LINES LC_QTD_LIB.

  DATA: IT_DD07V TYPE TABLE OF DD07V WITH HEADER LINE.

  IF LC_QTD_LIB > 1.

    LOOP AT IT_LIBERADO ASSIGNING <FS_DOMI>.
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          DOMNAME    = <FS_DOMI>-DOMNAME
        TABLES
          VALUES_TAB = IT_DD07V.

      READ TABLE IT_DD07V WITH KEY DOMVALUE_L = <FS_DOMI>-DOMVALUE_L.
      IF SY-SUBRC IS INITIAL.
        <FS_DOMI>-DDTEXT = IT_DD07V-DDTEXT.
      ENDIF.
    ENDLOOP.

    "Escolher libera├º├úo """""""""""""""""""""""""""""""
    CLEAR: E_TP_APROVACAO, E_DOMNAME, E_DDTEXT.

    PERFORM ESCOLHER_LIBERACAO TABLES IT_LIBERADO CHANGING E_TP_APROVACAO E_DOMNAME E_DDTEXT.
    """""""""""""""""""""""""""""""""""""""""""""""""""

    IF E_TP_APROVACAO IS INITIAL.
      MESSAGE E158 RAISING SEM_ESCOLHA.
    ENDIF.

  ELSE.
    READ TABLE IT_LIBERADO INDEX 1.
    MOVE: IT_LIBERADO-DOMNAME    TO E_DOMNAME,
          IT_LIBERADO-DOMVALUE_L TO E_TP_APROVACAO.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        DOMNAME    = E_DOMNAME
      TABLES
        VALUES_TAB = IT_DD07V.

    READ TABLE IT_DD07V WITH KEY DOMVALUE_L = E_TP_APROVACAO.
    IF SY-SUBRC IS INITIAL.
      E_DDTEXT = IT_DD07V-DDTEXT.
    ENDIF.

  ENDIF.

ENDFUNCTION.
