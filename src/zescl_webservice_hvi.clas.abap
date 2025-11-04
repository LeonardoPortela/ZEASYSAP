CLASS zescl_webservice_hvi DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZESCL_WEBSERVICE_HVI
*"* do not include other source files here!!!
  PUBLIC SECTION.

    TYPES:
    BEGIN OF ty_oauth,
        token      TYPE string,
        identidade TYPE string,
        idusuario  TYPE string,
        usuario    TYPE string,
        success    TYPE string,
      END OF ty_oauth .
    TYPES:
    BEGIN OF ty_resultado_fardo,
        nr_fardo       TYPE string,
        vl_uhml        TYPE string,
        vl_ml          TYPE string,
        vl_ui          TYPE string,
        vl_str         TYPE string,
        vl_elg         TYPE string,
        vl_mic         TYPE string,
        vl_amt         TYPE string,
        vl_rd          TYPE string,
        vl_mb          TYPE string,
        vl_cg          TYPE string,
        vl_tcnt        TYPE string,
        vl_tarea       TYPE string,
        vl_leaf        TYPE string,
        vl_mr          TYPE string,
        vl_sfi         TYPE string,
        vl_mst         TYPE string,
        sci            TYPE string,
        csp            TYPE string,
        mala           TYPE string,
        dt_finalizacao TYPE string,
        hr_finalizacao TYPE string,
        os             TYPE string,
        nr_lote        TYPE string,
      END OF ty_resultado_fardo .
    TYPES:
    ty_t_resultado_fardo TYPE TABLE OF ty_resultado_fardo WITH EMPTY KEY .
    TYPES:
    BEGIN OF ty_root_resultado_fardo,
        success TYPE string,
        result  TYPE ty_t_resultado_fardo,
      END OF ty_root_resultado_fardo .

    METHODS busca_mala_hvi
    IMPORTING
      !i_login TYPE string
      !i_senha TYPE string
      !i_datai TYPE datum
      !i_dataf TYPE datum
    RETURNING
      VALUE(result) TYPE ty_root_resultado_fardo .
    METHODS authentication
    IMPORTING
      !i_login TYPE string
      !i_senha TYPE string
    RETURNING
      VALUE(result) TYPE ty_oauth .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE_HVI
*"* do not include other source files here!!!
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_webservice_hvi IMPLEMENTATION.


  METHOD authentication.
    "//Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( 'http://kerpweb.kuhlmann.agr.br/rest/usuarios/login' )
      IMPORTING
        client             = DATA(http_client)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD http_client->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/x-www-form-urlencoded'.

    http_client->request->set_form_field(
      EXPORTING
        name  = 'login'
        value = i_login
    ).

    http_client->request->set_form_field(
      EXPORTING
        name  = 'senha'
        value = i_senha
    ).

    http_client->send( ).

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    DATA(_result) = http_client->response->get_cdata( ).
    DATA(_json_deserializer) = NEW cl_trex_json_deserializer( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = _result
      CHANGING
        data        = result
    ).

  ENDMETHOD.


  METHOD busca_mala_hvi.
    DATA: v_author TYPE ty_oauth.
    DATA: obj_zcl_util TYPE REF TO zcl_util.
    DATA: var_datai     TYPE string. " C LENGTH 10.
    DATA: var_dataf TYPE string. " C LENGTH 10.

    FREE: obj_zcl_util.

    CREATE OBJECT obj_zcl_util.
    obj_zcl_util->conv_data_us_br( EXPORTING i_data = i_datai
                                             i_opcao = '/'
                                   RECEIVING e_data = var_datai ).

    obj_zcl_util->conv_data_us_br( EXPORTING i_data = i_dataf
                                             i_opcao = '/'
                                   RECEIVING e_data = var_dataf ).

    v_author = authentication( i_login = i_login
                               i_senha = i_senha ).

    IF v_author-token IS NOT INITIAL.
      "//Call service
      CALL METHOD cl_http_client=>create_by_url
        EXPORTING
          url                = CONV #( 'http://kerpweb.kuhlmann.agr.br/rest/hvi-fardos/list-fardos-by-cliente' )
        IMPORTING
          client             = DATA(http_client)
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4.

      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.

      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = '~server_protocol'
          value = 'HTTP/1.1'.

      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/x-www-form-urlencoded'.

      http_client->request->set_form_field(
        EXPORTING
          name  = 'token'
          value = v_author-token
      ).

      http_client->request->set_form_field(
        EXPORTING
          name  = 'data_inicial'
          value = var_datai
      ).

      http_client->request->set_form_field(
        EXPORTING
          name  = 'data_final'
          value = var_dataf
      ).

      http_client->send( ).

      CALL METHOD http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.

      DATA(_result) = http_client->response->get_cdata( ).
      DATA(_json_deserializer) = NEW cl_trex_json_deserializer( ).

      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = _result
        CHANGING
          data        = result
      ).

    ELSE.

    ENDIF.

  ENDMETHOD.
ENDCLASS.