CLASS zescl_webservice_tx_curva DEFINITION
  PUBLIC
  INHERITING FROM zescl_webservice
  CREATE PUBLIC .

*"* public components of class ZESCL_WEBSERVICE_TX_CURVA
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS buscar_taxa
    IMPORTING
      !i_data TYPE datum
      !i_data_lib TYPE datum OPTIONAL
      !i_tipo TYPE char01 DEFAULT 'C'
    RETURNING
      VALUE(e_cotacao) TYPE kurrf
    RAISING
      zescx_webservice .
    METHODS executar
    IMPORTING
      !i_numero TYPE zsded013
      !i_tipo TYPE char03 OPTIONAL
      !i_fixacao TYPE posnr OPTIONAL
      !i_ucomm TYPE syucomm OPTIONAL
      !i_status TYPE c OPTIONAL
      !i_tcode TYPE sytcode OPTIONAL
      !i_vbeln TYPE vbeln OPTIONAL
      !i_auart TYPE auart OPTIONAL
      !i_vbap TYPE vbap_t OPTIONAL .
    CLASS-METHODS hedge_insumos
    IMPORTING
      !i_numero TYPE zsded003 OPTIONAL
      !i_acao TYPE sy-ucomm OPTIONAL
      !i_tipo TYPE char3 OPTIONAL
      !i_itens TYPE STANDARD TABLE OPTIONAL
      !i_0090 TYPE zsdt0090 OPTIONAL
      !i_vbeln TYPE vbeln OPTIONAL
      !i_dir TYPE bezei30 OPTIONAL
      !i_seq TYPE numc4 OPTIONAL
      !i_taxa_boleta TYPE ukursp OPTIONAL
      !i_0090_manual TYPE flag OPTIONAL .
    CLASS-METHODS hedge_aquaviario
    IMPORTING
      !_vbrk TYPE vbrk
      !_vbrp TYPE vbrpvb OPTIONAL
      !_code TYPE sytcode
      !_auart TYPE auart OPTIONAL .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE_TX_CURVA
*"* do not include other source files here!!!
  PRIVATE SECTION.

*"* private components of class ZCL_WEBSERVICE_TX_CURVA
*"* do not include other source files here!!!
    METHODS monta_xml_taxa
    IMPORTING
      !i_data TYPE sydatum
      !i_data_lib TYPE datum OPTIONAL
      !i_tipo TYPE char01
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS ler_xml_taxa
    IMPORTING
      !i_xml TYPE string
    RETURNING
      VALUE(e_cotacao) TYPE kurrf
    RAISING
      zescx_webservice .
ENDCLASS.



CLASS zcl_webservice_tx_curva IMPLEMENTATION.


  METHOD buscar_taxa.

    DATA: var_msg  TYPE string, "Variavel para mostrar a Mensagem texto da exception.
          var_http TYPE REF TO if_http_client. "Interface HTTP Client

    DATA: cx_exception TYPE REF TO zescx_webservice. "Referencia para a Classe de Exception.

    DATA: gw_xml      TYPE string, "String para guardar informações do XML
          gw_xml_taxa TYPE string. "String para guardar informações do XML da Rota.

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "TX = Taxa Curva.
        me->set_servico( EXPORTING i_servico = 'TX' ).

      CATCH zescx_webservice INTO cx_exception .
        var_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH var_msg.
    ENDTRY.

    "Atribui o Tipo de Serviço
    "A = atualização.
    me->set_tipo( EXPORTING i_tipo = 'C').

    TRY .

        "Atribui as Informações do HTTP Client para consultar o WebService.
        var_http = me->url( ).

      CATCH zescx_webservice INTO cx_exception .
        var_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH var_msg.
    ENDTRY.

    "Abrir a conexão com o serviço.
    me->zesif_webservice~abrir_conexao( var_http ).
    gw_xml = monta_xml_taxa( i_data     = i_data
                             i_data_lib = i_data_lib
                             i_tipo = i_tipo ).

    "Envia para Consultar a rota com as informações preenchidas acima.
    "O retorno é de um arquivo XML com todas as rotas que precisam ser atualizadas.
* Modificação - RIM-SKM - IR127835 - Inicio
*    GW_XML_TAXA = ME->ZESIF_WEBSERVICE~CONSULTAR( I_HTTP = VAR_HTTP
*                                                I_XML  = GW_XML
*                                                ).
    me->zesif_webservice~consultar( EXPORTING
                                    i_http = var_http
                                    i_xml  = gw_xml
                                  RECEIVING
                                    e_resultado = gw_xml_taxa
                                  EXCEPTIONS
                                    http_communication_failure = 1
                                    http_invalid_state = 2
                                    http_processing_failed = 3
                                    http_invalid_timeout = 4
                                ).
    IF sy-subrc NE 0.
      DATA: w_zsdt0094_log TYPE zsdt0094_log.
      w_zsdt0094_log-data_registro = sy-datum.
      w_zsdt0094_log-hora_registro = sy-uzeit.
      w_zsdt0094_log-programa = sy-cprog.
      w_zsdt0094_log-ernam = sy-uname.
      w_zsdt0094_log-data = i_data.
      w_zsdt0094_log-data_lib = i_data_lib.
      w_zsdt0094_log-tipo = i_tipo.
      w_zsdt0094_log-erro = sy-subrc.
      MODIFY zsdt0094_log FROM w_zsdt0094_log.
    ENDIF.
* Modificação - RIM-SKM - IR127835 - Fim

    e_cotacao = ler_xml_taxa( gw_xml_taxa ).


  ENDMETHOD.


  METHOD executar.

    DATA: gobj_taxa_curva_db TYPE REF TO zescl_taxa_curva_db.
    DATA: lw_zsdt0094 TYPE zsdt0094.
    DATA: lw_setleaf  TYPE setleaf.

    CREATE OBJECT gobj_taxa_curva_db.

    "Caso encontre qualquer registro referente ao número de solicitação de venda que tenha o status inicializado como 'X'
    "não deverá ser feito nenhum tipo de registro na ZSDT0094.
    SELECT SINGLE * FROM zsdt0094 INTO lw_zsdt0094 WHERE nro_sol_ov EQ i_numero
                                                     AND edicao     EQ 'X'.
    IF ( sy-subrc NE 0 ).

      "Caso a solicitação seja encontrada, não pode fazer o lançamento  novamente, porque se trata de uma SOV antiga.
      SELECT SINGLE * FROM setleaf INTO lw_setleaf WHERE setname = 'MAGGI_SOV_ANTIGA'
                                                     AND valfrom = i_numero.
      IF ( sy-subrc NE 0 ).
        CASE i_tipo.
          WHEN: 'LIB'. "Liberação de OV.
            gobj_taxa_curva_db->liberar_ov( i_numero  = i_numero
                                            i_tcode   = i_tcode
                                            i_fixacao = i_fixacao ).
          WHEN: 'FRA'."Frame
            gobj_taxa_curva_db->frame( i_numero = i_numero
                                       i_ucomm  = i_ucomm
                                       i_tcode  = i_tcode
                                       i_vbeln  = i_vbeln
                                       i_auart  = i_auart
                                       ).
          WHEN: 'FRE'. "Frete.
            gobj_taxa_curva_db->frete( i_numero  = i_numero
                                       i_fixacao = i_fixacao
                                       i_status  = i_status
                                       i_tcode   = i_tcode
                                       i_vbeln   = i_vbeln
                                       i_auart   = i_auart
                                       ).

          WHEN: 'EDI' OR 'DEL' . "Edição / Delete.
            gobj_taxa_curva_db->edicao( i_numero  = i_numero
                                        i_fixacao = i_fixacao
                                        i_ucomm   = i_ucomm
                                        i_vbeln   = i_vbeln
                                        i_tcode   = i_tcode
                                        ).

          WHEN: 'EDF'."Edição de Frame
            gobj_taxa_curva_db->frame_edicao( i_numero  = i_numero
                                              i_fixacao = i_fixacao
                                              i_vbeln   = i_vbeln
                                              i_tcode   = i_tcode
                                              i_auart   = i_auart
                                              i_tipo    = i_tipo
                                              i_status  = i_status ).

          WHEN: 'ENC'.  "Encerramento de Venda Simples

            "Repetir essa merda porque o usuário não sabe o que quer da vida.
            gobj_taxa_curva_db->encerramento( i_numero  = i_numero
                                              i_tcode   = i_tcode
                                              i_fixacao = i_fixacao
                                              i_auart   = i_auart
                                              i_status  = i_status
                                              i_vbeln   = i_vbeln
                                              i_vbap    = i_vbap ). "ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*.


          WHEN: 'LOG'.  "Atualiza a Aba logista, das vendas que não dispara o Hedge

            gobj_taxa_curva_db->calcula_frete( i_numero  = i_numero
                                               i_fixacao = i_fixacao
                                               i_ucomm   = i_ucomm
                                               i_vbeln   = i_vbeln
                                               i_tcode   = i_tcode ).


        ENDCASE.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD hedge_aquaviario.

    DATA(obj_tx_curva)    = NEW zescl_taxa_curva( ).
    DATA(obj_tx_curva_db) = NEW zescl_taxa_curva_db( ).

    DATA(r_aqv) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_AQV' ).
    DATA(r_spt) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_SPT' ).
    DATA(r_tbo) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_TB' ).

    DATA(r_geral) = r_aqv.
    APPEND LINES OF r_spt TO r_geral.
    APPEND LINES OF r_tbo TO r_geral.

    DATA(r_burks) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_BUKRS' ).

    DATA: v_aquav_ini TYPE bsak-augdt.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_zfi0064_ini_aquav)
     WHERE setname = 'ZFI0064_INI_AQUAV'.

    IF ( sy-subrc EQ 0 ) AND ( _wl_zfi0064_ini_aquav-valfrom IS NOT INITIAL ).
      v_aquav_ini = _wl_zfi0064_ini_aquav-valfrom.

      "// Verifica se a data que está no Set é menor ou igual a data de hoje, para fazer fazer o disparo.
      IF v_aquav_ini LE sy-datum.

        CHECK r_geral IS NOT INITIAL.
        CHECK r_burks IS NOT INITIAL.

        CHECK _vbrk-fkart IS NOT INITIAL.
        CHECK _vbrk-vkorg IS NOT INITIAL.

        CHECK _vbrk-vkorg IN r_burks.
        CHECK _vbrk-waerk EQ 'BRL'.

*   "// Condição para contemplar a ZLES0077
        DATA(code) = _code.
        code = COND #( WHEN _vbrk-fkart EQ 'S1' THEN 'VF11' ELSE code ).

        CASE code.
          WHEN 'VF01' OR 'ZLES0077' OR 'ZNFW0005' OR 'ZSDT0158' OR 'ZSDT0200' OR 'ZSDT0201'.

            IF _auart IS NOT INITIAL.
              CHECK _auart IN r_geral.
            ELSE.
              CHECK _vbrk-fkart IN r_geral.
            ENDIF.

            obj_tx_curva_db->frete_aqv(
                                        _vbrk  = _vbrk
                                        _vbrp  = _vbrp
                                        _auart = _auart
                                      ).

          WHEN 'VF11'.
            obj_tx_curva_db->estorno_aqv( _vbrk-sfakn ).
        ENDCASE.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD hedge_insumos.

    DATA: obj_taxa   TYPE REF TO zescl_taxa_curva_db.
    CREATE OBJECT obj_taxa.

    CASE i_tipo.

      WHEN 'VDI'. "Venda mercado Interno
**************************************************************************************************
*        Dispara o HEDGE VENDA na Transação ZSDT0044 quando:
*        Aprova uma Solicitação
*        Inclui um desconto Absoluto.
*        e na Transação ZSDT0087 quando:
*        Altera Quantidade da Ordem
*        Traca de Materiais
*        Encerramento
*        Trava Cambio
*        Redistribuição de Quantidade
**************************************************************************************************
        obj_taxa->venda_in( i_numero = i_numero
                            i_tipo   = i_tipo
                            i_acao   = i_acao
                            i_0090   = i_0090
                            t_itens  = i_itens
                            i_dir    = i_dir
                            i_taxa_boleta = i_taxa_boleta ).

      WHEN 'FRI'. "Frete Mercado interno
**************************************************************************************************
*        Dispara o HEDGE FRETE na Transação ZSDT0044 quando:
*        Aprova uma Solicitação
*        Inclui um desconto Absoluto.
*        e na Transação ZSDT0087 quando:
*        Altera Quantidade da Ordem
*        Traca de Materiais
*        Encerramento
*        Redistribuição de quantiodade
**************************************************************************************************
        obj_taxa->frete_in( i_numero = i_numero
                            i_tipo   = i_tipo
                            i_acao   = i_acao
                            i_0090   = i_0090
                            t_itens  = i_itens
                            i_dir    = i_dir
                            i_taxa_boleta = i_taxa_boleta ).

      WHEN 'EST'. "Estorno dos lançamentos disparados
**************************************************************************************************
*        Dispara o HEDGE quando é REPROVADO uma Simulação na ZSDT0044
*        ou quando Estorna uma OV na ZSDT0087
*        I_NUMERO -> Documento de Simulação
*        I_TIPO   -> Parametro de Identificação
*        I_VBELN  -> Documento da Ordem
*        I-DIR    -> Direção da VF11
**************************************************************************************************
        obj_taxa->estorno_in( i_numero = i_numero
                              i_tipo   = i_tipo
                              i_0090   = i_0090
                              i_vbeln  = i_vbeln
                              i_dir    = i_dir
                              i_seq    = i_seq
                              i_0090_manual = i_0090_manual ).

      WHEN 'INV'.
**************************************************************************************************
*
*
*        I_NUMERO -> Documento de Simulação
*        I_TIPO   -> Parametro de Identificação
*        I_VBELN  -> Documento da Ordem
*
**************************************************************************************************
        obj_taxa->reversao_frete_in( i_numero = i_numero
                                     i_tipo   = i_tipo
                                     i_0090   = i_0090
                                     i_vbeln  = i_vbeln
                                     i_dir    = i_dir
                                     i_seq    = i_seq ).
    ENDCASE.
  ENDMETHOD.


  METHOD ler_xml_taxa.

    TYPES: BEGIN OF ty_taxa,
             cotacao TYPE c LENGTH 10,
             status  TYPE c LENGTH 1,
             message TYPE c LENGTH 50,
           END OF ty_taxa.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_document      TYPE REF TO if_ixml_document,

          if_node          TYPE REF TO if_ixml_node,
          if_map           TYPE REF TO if_ixml_named_node_map,
          if_attr          TYPE REF TO if_ixml_node,

          iterator         TYPE REF TO if_ixml_node_iterator,
          tag_name         TYPE string,
          name_dom         TYPE string,
          count_dom        TYPE i,
          index_dom        TYPE i,
          prefix_dom       TYPE string,
          valor_dom        TYPE string,

          node_filho       TYPE REF TO if_ixml_node,
          valor_filho      TYPE string.

    DATA: gt_taxa TYPE TABLE OF ty_taxa,
          gw_taxa TYPE ty_taxa.

    FIELD-SYMBOLS: <fs_taxa> TYPE ty_taxa.


    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).

    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory
                                               istream        = if_stream
                                               document       = if_document ).

    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).


    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.


        CASE if_node->get_type( ).

          WHEN: if_ixml_node=>co_node_element.

            tag_name = if_node->get_name( ).
            if_map   = if_node->get_attributes( ).

            IF NOT ( if_map IS INITIAL ).

              count_dom = if_map->get_length( ).

              DO count_dom TIMES.

                index_dom  = sy-index - 1.
                if_attr    = if_map->get_item( index_dom ).
                name_dom   = if_attr->get_name( ).
                prefix_dom = if_attr->get_namespace_prefix( ).
                valor_dom  = if_attr->get_value( ).

              ENDDO.

              CASE tag_name.
                WHEN: 'cotacao'.
                  gw_taxa-cotacao = if_node->get_value( ).
                  e_cotacao = gw_taxa-cotacao.
                WHEN: 'status'.
                  gw_taxa-status = if_node->get_value( ).
                WHEN: 'message'.
                  gw_taxa-message = if_node->get_value( ).

              ENDCASE.
            ENDIF.
        ENDCASE.
        if_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.



  ENDMETHOD.


  METHOD monta_xml_taxa.
    CLEAR: e_xml. "Limpar a variavel de retorno.

    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    DATA: obj_zcl_util TYPE REF TO zcl_util.
    DATA: var_data     TYPE c LENGTH 10.
    DATA: var_data_lib TYPE c LENGTH 10.

    FREE: obj_zcl_util.

    CREATE OBJECT obj_zcl_util.
    obj_zcl_util->conv_data_us_br( EXPORTING i_data = i_data
                                             i_opcao = '.'
                                   RECEIVING e_data = var_data ).

    obj_zcl_util->conv_data_us_br( EXPORTING i_data = i_data_lib
                                             i_opcao = '.'
                                   RECEIVING e_data = var_data_lib ).

    CONC_XML '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:tem="http://tempuri.org/">'.
    CONC_XML  '<soap:Header/>'.
    CONC_XML '<soap:Body>'.

    IF ( sy-cprog EQ 'ZCARGA').
      CONC_XML       '<tem:GetHedge>'.
      CONC_XML          '<tem:dataRef>'.
      CONC_XML           VAR_DATA_LIB.
      CONC_XML         '</tem:dataRef>'.
      CONC_XML          '<tem:dataVencimento>'.
      CONC_XML           VAR_DATA.
      CONC_XML         '</tem:dataVencimento>'.
    ELSE.
      CONC_XML       '<tem:Get>'.
      CONC_XML          '<tem:data>'.
      CONC_XML           VAR_DATA.
      CONC_XML         '</tem:data>'.
    ENDIF.

    CONC_XML          '<tem:tipo>'.
    CONC_XML           I_TIPO.
    CONC_XML       '</tem:tipo>'.

    IF ( sy-cprog EQ 'ZCARGA').
      CONC_XML       '</tem:GetHedge>'.
    ELSE.
      CONC_XML       '</tem:Get>'.
    ENDIF.

    CONC_XML    '</soap:Body>'.
    CONC_XML  '</soap:Envelope>'.


  ENDMETHOD.
ENDCLASS.