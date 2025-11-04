CLASS zescl_webservice_ord_car DEFINITION
  PUBLIC
  INHERITING FROM zescl_webservice
  FINAL
  CREATE PUBLIC .

*"* public components of class ZESCL_WEBSERVICE_ORD_CAR
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS buscar_transporte
    IMPORTING
      VALUE(i_nr_ordem) TYPE string
      VALUE(i_safra) TYPE string
      VALUE(i_filial) TYPE werks_d OPTIONAL
    RETURNING
      VALUE(e_ordem_car) TYPE REF TO zcl_ordem_car .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE_ORD_CAR
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_WEBSERVICE_ORD_CAR
*"* do not include other source files here!!!

    METHODS monta_xml_transp
    IMPORTING
      !i_nr_ordem TYPE string
      !i_safra TYPE string
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS ler_xml_transp
    IMPORTING
      !i_xml TYPE string
    RETURNING
      VALUE(e_ordem_car) TYPE REF TO zcl_ordem_car .
ENDCLASS.



CLASS zcl_webservice_ord_car IMPLEMENTATION.


  METHOD buscar_transporte.

    DATA: var_msg  TYPE string, "Variavel para mostrar a Mensagem texto da exception.
        var_http TYPE REF TO if_http_client. "Interface HTTP Client

    DATA: cx_exception TYPE REF TO zescx_webservice. "Referencia para a Classe de Exception.

    DATA: gw_xml        TYPE string, "String para guardar informações do XML
        gw_xml_transp TYPE string, "String para guardar informações do XML do Transp.
        lc_nr_ordem   TYPE zsdt0001od-nr_ordem,  "*-CS2024000522-12.09.2024-JT-#152417-inicio
        lc_nr_safra   TYPE zsdt0001od-nr_safra,  "*-CS2024000522-12.09.2024-JT-#152417-inicio
        lra_filial    TYPE RANGE OF  werks_d."IR246503 - Correção busca OC #184471 - BG

    lc_nr_ordem = i_nr_ordem.  "*-CS2024000522-12.09.2024-JT-#152417-inicio
    lc_nr_safra = i_safra.     "*-CS2024000522-12.09.2024-JT-#152417-inicio

    APPEND VALUE #( sign = 'I' option = 'EQ' low = i_filial ) TO lra_filial. "IR246503 - Correção busca OC #184471 - BG

    SELECT SINGLE * INTO @DATA(wa_zsdt0001od)
    FROM zsdt0001od
   WHERE nr_ordem  EQ @lc_nr_ordem  "@i_nr_ordem
     AND nr_safra  EQ @lc_nr_safra "@i_safra.
     AND id_branch IN @lra_filial "IR246503 - Correção busca OC #184471 - BG
     AND tp_status EQ 'AB'.       "IR246503 - Correção busca OC #184471 - BG

    IF sy-subrc IS NOT INITIAL.

      CREATE OBJECT e_ordem_car.
      e_ordem_car->set_mensagem_ret( 'Ordem de Carregamento não encontrada!' ).

*    TRY .
*        "Atribui o serviço que precisa ser consultado.
*        "OC = Ordem Carrramento.
*        ME->SET_SERVICO( EXPORTING I_SERVICO = 'OC' ).
*
*      CATCH ZESCX_WEBSERVICE INTO CX_EXCEPTION .
*        VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
*        MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
*    ENDTRY.
*
*    "Atribui o Tipo de Serviço
*    "A = atualização.
*    ME->SET_TIPO( EXPORTING I_TIPO = 'C').
*
*    TRY .
*
*        "Atribui as Informações do HTTP Client para consultar o WebService.
*        VAR_HTTP = ME->URL( ).
*
*      CATCH ZESCX_WEBSERVICE INTO CX_EXCEPTION .
*        VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
*        MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
*    ENDTRY.
*
*    "Abrir a conexão com o serviço.
*    ME->ZESIF_WEBSERVICE~ABRIR_CONEXAO( VAR_HTTP ).
*
*    GW_XML = MONTA_XML_TRANSP( I_NR_ORDEM = I_NR_ORDEM
*                               I_SAFRA    = I_SAFRA ).
*
*    "Envia para Consultar os dados transporte com as informações preenchidas acima.
*    "O retorno é de um arquivo XML com todas os dados do Transporte(Placas e Motorista).
*    GW_XML_TRANSP = ME->ZESIF_WEBSERVICE~CONSULTAR( I_HTTP = VAR_HTTP
*                                                  I_XML  = GW_XML
*                                                ).
*
*    ME->LER_XML_TRANSP( EXPORTING
*                          I_XML = GW_XML_TRANSP
*                        RECEIVING
*                          E_ORDEM_CAR = E_ORDEM_CAR  ).
    ELSE.

      CREATE OBJECT e_ordem_car.
      e_ordem_car->set_motorista( i_motorista = wa_zsdt0001od-id_motorista ).
      e_ordem_car->set_placa_cav( i_placa_cav = wa_zsdt0001od-ds_placa_trator ).
      e_ordem_car->set_placa1( i_placa1 = wa_zsdt0001od-ds_placa_reboq_1 ).
      e_ordem_car->set_placa2( i_placa2 = wa_zsdt0001od-ds_placa_reboq_2 ).
      e_ordem_car->set_placa3( i_placa3 = wa_zsdt0001od-ds_placa_reboq_3 ).
      e_ordem_car->at_zsdt0001od = wa_zsdt0001od.
      e_ordem_car->set_mensagem_ret( 'Sucesso' ).

    ENDIF.


  ENDMETHOD.


  METHOD ler_xml_transp.

    DATA: obj_ordem_car    TYPE REF TO zcl_ordem_car.

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

        node_filho      TYPE REF TO if_ixml_node,
        valor_filho     TYPE string,

        vl_motorista    TYPE zmotorista,
        vl_placa_cav    TYPE zplaca,
        vl_placa1       TYPE zplaca,
        vl_placa2       TYPE zplaca,
        vl_mensagem     TYPE string.

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

      FREE: obj_ordem_car.
      CREATE OBJECT obj_ordem_car.

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
                WHEN: 'codigoSapMotorista'.
                  vl_motorista = if_node->get_value( ).
                  obj_ordem_car->set_motorista( vl_motorista ).
                WHEN: 'placaCavalo'.
                  vl_placa_cav = if_node->get_value( ).
                  obj_ordem_car->set_placa_cav( vl_placa_cav ).
                WHEN: 'placaCarreta1'.
                  vl_placa1 = if_node->get_value( ).
                  obj_ordem_car->set_placa1( vl_placa1 ).
                WHEN: 'placaCarreta2'.
                  vl_placa2 = if_node->get_value( ).
                  obj_ordem_car->set_placa2( vl_placa2 ).
                WHEN: 'mensagem'.
                  vl_mensagem = if_node->get_value( ).
                  obj_ordem_car->set_mensagem_ret( vl_mensagem ).

              ENDCASE.
            ENDIF.
        ENDCASE.
        if_node = iterator->get_next( ).
      ENDWHILE.

      e_ordem_car = obj_ordem_car.


    ENDIF.


  ENDMETHOD.


  METHOD monta_xml_transp.

    CLEAR: e_xml. "Limpar a variavel de retorno.

    DEFINE conc_xml.
    CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    DATA: var_nr_ordem   TYPE string,
        var_safra      TYPE string.

    var_nr_ordem   = i_nr_ordem.
    var_safra      = i_safra.

    CONC_XML '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:tem="http://tempuri.org/">'.
    CONC_XML    '<soap:Header/>'.
    CONC_XML    '<soap:Body>'.
    CONC_XML       '<tem:Get>'.

    CONC_XML         '<tem:numeroOrdem>'.
    CONC_XML            VAR_NR_ORDEM.
    CONC_XML         '</tem:numeroOrdem>'.

    CONC_XML          '<tem:safra>'.
    CONC_XML            I_SAFRA.
    CONC_XML         '</tem:safra>'.

    CONC_XML       '</tem:Get>'.
    CONC_XML    '</soap:Body>'.
    CONC_XML '</soap:Envelope>'.

  ENDMETHOD.
ENDCLASS.