CLASS zescl_webservice_tipcard DEFINITION
  PUBLIC
  INHERITING FROM zescl_webservice
  CREATE PUBLIC .

*"* public components of class ZESCL_WEBSERVICE_TIPCARD
*"* do not include other source files here!!!
  PUBLIC SECTION.

    DATA ck_salvar_xml_local TYPE char01 .

    METHODS autenticacao
    IMPORTING
      !i_http TYPE REF TO if_http_client OPTIONAL
      !i_operadora TYPE char3
      !i_grupo TYPE ZESDE_ds_grupo_empresa
    RETURNING
      VALUE(e_chave) TYPE char32
    RAISING
      zescx_webservice .
    METHODS buscar_rotas
    IMPORTING
      !i_grupo TYPE ZESDE_ds_grupo_empresa
    RAISING
      zescx_webservice .
    METHODS ler_xml_rotas
    IMPORTING
      !i_grupo TYPE ZESDE_ds_grupo_empresa
      !i_xml TYPE string
    RAISING
      zescx_webservice .
    METHODS chave_seguranca
    IMPORTING
      !i_grupo TYPE ZESDE_ds_grupo_empresa
    RETURNING
      VALUE(e_chave) TYPE char32
    RAISING
      zescx_webservice .
    METHODS atualizar_valores
    IMPORTING
      !i_grupo TYPE ZESDE_ds_grupo_empresa .
    METHODS xml_autentica
    IMPORTING
      !i_usuario TYPE zuse_web
      !i_senha TYPE zpass_web
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS xml_consulta_rota .
    METHODS xml_atualiza_rota
    IMPORTING
      !i_chave TYPE char32
      !i_cnpjcontratante TYPE setvalmin
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS consultar_arquivo_cobranca
    IMPORTING
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
      !i_fatura TYPE zpfe_nr_lote_adm OPTIONAL
    EXPORTING
      !e_msg TYPE char255
      VALUE(r_linkarquivo) TYPE string
      VALUE(r_linkarquivo_pedagio) TYPE string
      VALUE(r_content_arquivo) TYPE string
      VALUE(r_content_arquivo_pedagio) TYPE string
    EXCEPTIONS
      zwebservice .
    METHODS consultar_arquivo_conferencia
    IMPORTING
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
      !i_dt_inicial TYPE datum
      !i_dt_final TYPE datum
    EXPORTING
      !e_msg TYPE char255
    RETURNING
      VALUE(r_linkarquivo) TYPE string
    EXCEPTIONS
      zwebservice .
    METHODS consultar_rota
    IMPORTING
      !i_rota TYPE ZESDE_id_rota
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
    EXPORTING
      !e_msg TYPE char255
    EXCEPTIONS
      zwebservice .
    METHODS solicita_rota
    IMPORTING
      !i_rota TYPE ZESDE_id_rota
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
    EXPORTING
      !e_msg TYPE char255
      !e_id_rota_adm TYPE ZESDE_id_rota_adm
    EXCEPTIONS
      zwebservice .
    METHODS atualizar_rota
    IMPORTING
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
    EXPORTING
      !e_msg TYPE char255
    EXCEPTIONS
      zwebservice .
    CLASS-METHODS cons_situacao_transportador
    IMPORTING
      !i_bukrs TYPE bukrs DEFAULT '0001'
      !i_branch TYPE j_1bbranc_ DEFAULT '0116'
      !i_partiner TYPE j_1bparid OPTIONAL
      !i_placa TYPE zpc_veiculo OPTIONAL
      !i_pesquisa_livre TYPE char01 DEFAULT ' '
      !i_cnpj TYPE stcd1 OPTIONAL
      !i_rntrc TYPE stcd3 OPTIONAL
      !i_ck_consulta TYPE ZESDE_ck_rntrc_cs OPTIONAL
    RETURNING
      VALUE(e_consultas) TYPE zlest0135_t
    EXCEPTIONS
      erro
      webservice .
    METHODS salva_xml
    IMPORTING
      !i_name_file TYPE string
      !i_xml TYPE string .
    METHODS xml_consultar_arq_cobranca
    IMPORTING
      !i_chave TYPE char32
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
      !i_fatura TYPE zpfe_nr_lote_adm OPTIONAL
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS xml_consultar_arq_conferencia
    IMPORTING
      !i_chave TYPE char32
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
      !i_dt_inicial TYPE datum
      !i_dt_final TYPE datum
    RETURNING
      VALUE(e_xml) TYPE string .
    CLASS-METHODS processar_arquivo_cobranca
    IMPORTING
      !i_filename TYPE string
      !i_filename_pedagio TYPE string
      !i_content_filename TYPE string OPTIONAL
      !i_content_filename_pedagio TYPE string OPTIONAL
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
    EXPORTING
      !e_lotes TYPE zpfe_numero_lote_t
    RAISING
      zcx_cadastro
      zcx_erro_arquivo .
    CLASS-METHODS processar_arquivo_conferencia
    IMPORTING
      !i_filename TYPE string
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
      !i_dt_inicial TYPE datum
      !i_dt_final TYPE datum
    RAISING
      zcx_cadastro
      zcx_erro_arquivo .
    METHODS check_data_envio_email
    IMPORTING
      !e_consultas TYPE zlest0135 .
    METHODS cons_status_parceiro
    IMPORTING
      !i_bukrs TYPE bukrs DEFAULT '0001'
      !i_branch TYPE j_1bbranc_ DEFAULT '0116'
      !i_partiner TYPE j_1bparid OPTIONAL
      !i_placa TYPE zpc_veiculo OPTIONAL
    RETURNING
      VALUE(e_consultas) TYPE zlest0135_t
    EXCEPTIONS
      erro
      webservice .
    METHODS xml_consulta_status_parceiro
    IMPORTING
      !i_chave TYPE char32
      !i_consulta TYPE ZESDE_consulta_parceiro
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS enviar_email_tip
    IMPORTING
      !e_consultas TYPE zlest0135 .
  PROTECTED SECTION.

*"* protected components of class ZCL_WEBSERVICE_TIPCARD
*"* do not include other source files here!!!
    METHODS xml_atualizar_rota
    IMPORTING
      !i_chave TYPE char32
      !i_bukrs TYPE bukrs
      !i_branch TYPE j_1bbranc_
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS xml_consultar_rota
    IMPORTING
      !i_chave TYPE char32
      !i_zlest0101 TYPE zlest0101
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS xml_criar_rota
    IMPORTING
      !i_chave TYPE char32
      !i_zlest0101 TYPE zlest0101
      !i_zlest0107 TYPE zlest0107_t
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS ler_xml_atualizar_rota
    IMPORTING
      !i_xml TYPE string
    EXPORTING
      !e_msg TYPE char255
      !e_rotas TYPE zlest0101_t
      !e_pracas TYPE zlest0102_t .
    METHODS ler_xml_consultar_rota
    IMPORTING
      !i_xml TYPE string
    EXPORTING
      !e_rotas TYPE zlest0101_t
      !e_pracas TYPE zlest0102_t
      !e_msg TYPE char255 .
    METHODS ler_xml_criar_rota
    IMPORTING
      !i_xml TYPE string
    EXPORTING
      !e_msg TYPE char255
      !e_id_rota_adm TYPE ZESDE_id_rota_adm .
    METHODS xml_consulta_transportador
    IMPORTING
      !i_chave TYPE char32
      !i_consulta_rntrc TYPE ZESDE_consulta_rntrc
    RETURNING
      VALUE(e_xml) TYPE string .
    METHODS ler_xml_consultar_arq_cobranca
    IMPORTING
      !i_xml TYPE string
    EXPORTING
      !e_msg TYPE char255
      VALUE(r_linkarquivo) TYPE string
      VALUE(r_linkarquivo_pedagio) TYPE string
      VALUE(r_content_arquivo) TYPE string
      VALUE(r_content_arquivo_pedagio) TYPE string .
    METHODS ler_xml_consultar_arq_confere
    IMPORTING
      !i_xml TYPE string
    EXPORTING
      !e_msg TYPE char255
      !e_codigo TYPE string
    RETURNING
      VALUE(r_linkarquivo) TYPE string .
    METHODS ler_xml_situacao_transportador
    IMPORTING
      !i_xml TYPE string
    RETURNING
      VALUE(e_zlest0135) TYPE zlest0135 .
  PRIVATE SECTION.

    METHODS ler_xml_status_parceiro
    IMPORTING
      !i_xml TYPE string
    RETURNING
      VALUE(e_zlest0135) TYPE zlest0135 .
*"* private components of class ZCL_WEBSERVICE_TIPCARD
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_webservice_tipcard IMPLEMENTATION.


  METHOD atualizar_rota.

    DATA: e_chave      TYPE char32,
          lc_msg       TYPE string,
          lc_msg_adm   TYPE string,
          cx_exception TYPE REF TO zescx_webservice,
          lc_xml       TYPE string,
          lc_http      TYPE REF TO if_http_client,
          e_rotas	     TYPE	zlest0101_t,
          e_pracas     TYPE zlest0102_t,
          it_zlest0101 TYPE TABLE OF zlest0101,
          it_zlest0102 TYPE TABLE OF zlest0102,
          it_zlest0084 TYPE TABLE OF zlest0084,
          it_zlest0091 TYPE TABLE OF zlest0091,
          wa_zlest0101 TYPE zlest0101,
          wa_zlest010a TYPE zlest0101,
          wa_zlest0102 TYPE zlest0102,
          wa_zlest0084 TYPE zlest0084,
          wa_zlest0091 TYPE zlest0091,
          cd_id_rota   TYPE ZESDE_id_rota.

    FIELD-SYMBOLS: <z101> TYPE zlest0101,
                   <z102> TYPE zlest0102,
                   <z084> TYPE zlest0084.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    TRY .
        e_chave = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    lc_xml = xml_atualizar_rota( i_chave = e_chave i_bukrs = i_bukrs i_branch = i_branch ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RO - Criar Rota.
        me->set_servico( EXPORTING i_servico = 'RA' ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'R').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        lc_http = me->url( ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zesif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zesif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = lc_msg_adm
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    CALL METHOD me->ler_xml_atualizar_rota
      EXPORTING
        i_xml    = lc_msg_adm
      IMPORTING
        e_msg    = e_msg
        e_rotas  = e_rotas
        e_pracas = e_pracas.

    IF e_msg NE 'Sucesso'.

      MESSAGE |Falha ao consultar Rotas Empresa: { i_bukrs } Filial: { i_branch } | TYPE 'S'.
      MESSAGE |Mensagem Retorno Serviço: { e_msg } Data: { sy-datum } Horas: { sy-uzeit } | TYPE 'S'.

      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ELSE.

      SELECT * INTO TABLE it_zlest0101
        FROM zlest0101
       WHERE bukrs  EQ i_bukrs
         AND branch EQ i_branch.

      SELECT * INTO TABLE it_zlest0091
        FROM zlest0091.

      SELECT * INTO TABLE it_zlest0084
        FROM zlest0084
       WHERE bukrs  EQ i_bukrs
         AND branch EQ i_branch.

      IF it_zlest0101 IS NOT INITIAL.
        SELECT * INTO TABLE it_zlest0102
          FROM zlest0102
           FOR ALL ENTRIES IN it_zlest0101
         WHERE id_rota EQ it_zlest0101-id_rota
           AND bukrs   EQ it_zlest0101-bukrs
           AND branch  EQ it_zlest0101-branch.
      ENDIF.

      SORT e_rotas      BY bukrs branch country cd_cid_origem cd_cid_destino tp_rota_perc id_rota_adm.
      SORT it_zlest0101 BY bukrs branch country cd_cid_origem cd_cid_destino tp_rota_perc id_rota_adm.

      SELECT MAX( id_rota ) INTO cd_id_rota
        FROM zlest0101.

      IF cd_id_rota IS INITIAL OR cd_id_rota EQ 0.
        MOVE 1 TO cd_id_rota.
      ELSE.
        ADD 1 TO cd_id_rota.
      ENDIF.

      "Marcar Rota como Ativa/Desativa
      LOOP AT it_zlest0101 ASSIGNING <z101>.
        READ TABLE e_rotas INTO wa_zlest010a
                           WITH KEY bukrs          = <z101>-bukrs
                                    branch         = <z101>-branch
                                    country        = <z101>-country
                                    cd_cid_origem  = <z101>-cd_cid_origem
                                    cd_cid_destino = <z101>-cd_cid_destino
                                    tp_rota_perc   = <z101>-tp_rota_perc
                                    id_rota_adm    = <z101>-id_rota_adm.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: <z101>-st_rota.
        ELSE.
          <z101>-st_rota = abap_true.
        ENDIF.
      ENDLOOP.

      "Incluir Nova Rota
      LOOP AT e_rotas ASSIGNING <z101>.
        READ TABLE it_zlest0101 INTO wa_zlest010a
                            WITH KEY bukrs          = <z101>-bukrs
                                     branch         = <z101>-branch
                                     country        = <z101>-country
                                     cd_cid_origem  = <z101>-cd_cid_origem
                                     cd_cid_destino = <z101>-cd_cid_destino
                                     tp_rota_perc   = <z101>-tp_rota_perc
                                     id_rota_adm    = <z101>-id_rota_adm.
        IF sy-subrc IS NOT INITIAL.
          "Nova Rota
          CLEAR: <z101>-st_rota.
          <z101>-id_rota = cd_id_rota.
          <z101>-st_rota = abap_true.
          LOOP AT e_pracas ASSIGNING <z102> WHERE id_rota_adm EQ <z101>-id_rota_adm.
            <z102>-id_rota   = <z101>-id_rota.
            <z102>-bukrs     = <z101>-bukrs.
            <z102>-branch    = <z101>-branch.
            <z102>-st_praca  = abap_true.
            <z102>-vl_eixo1  = <z102>-vl_eixo1 / 100.
            <z102>-vl_eixo2  = <z102>-vl_eixo2 / 100.
            <z102>-vl_eixo3  = <z102>-vl_eixo3 / 100.
            <z102>-vl_eixo4  = <z102>-vl_eixo4 / 100.
            <z102>-vl_eixo5  = <z102>-vl_eixo5 / 100.
            <z102>-vl_eixo6  = <z102>-vl_eixo6 / 100.
            <z102>-vl_eixo7  = <z102>-vl_eixo7 / 100.
            <z102>-vl_eixo8  = <z102>-vl_eixo8 / 100.
            <z102>-vl_eixo9  = <z102>-vl_eixo9 / 100.
            <z102>-vl_eixo10 = <z102>-vl_eixo10 / 100.
*** Inicio - Rubenilson - 17.12.24 - US160867
            <z102>-vl_tageixo1  = <z102>-vl_tageixo1 / 100.
            <z102>-vl_tageixo2  = <z102>-vl_tageixo2 / 100.
            <z102>-vl_tageixo3  = <z102>-vl_tageixo3 / 100.
            <z102>-vl_tageixo4  = <z102>-vl_tageixo4 / 100.
            <z102>-vl_tageixo5  = <z102>-vl_tageixo5 / 100.
            <z102>-vl_tageixo6  = <z102>-vl_tageixo6 / 100.
            <z102>-vl_tageixo7  = <z102>-vl_tageixo7 / 100.
            <z102>-vl_tageixo8  = <z102>-vl_tageixo8 / 100.
            <z102>-vl_tageixo9  = <z102>-vl_tageixo9 / 100.
            <z102>-vl_tageixo10 = <z102>-vl_tageixo10 / 100.
***Fim - Rubenilson - 17.12.24 - US160867
            APPEND <z102> TO it_zlest0102.
          ENDLOOP.
          APPEND <z101> TO it_zlest0101.
          ADD 1 TO cd_id_rota.
        ELSE.
          LOOP AT e_pracas ASSIGNING <z102> WHERE id_rota_adm EQ wa_zlest010a-id_rota_adm.
            <z102>-id_rota   = wa_zlest010a-id_rota.
            <z102>-bukrs     = wa_zlest010a-bukrs.
            <z102>-branch    = wa_zlest010a-branch.
            <z102>-st_praca  = abap_true.
            <z102>-vl_eixo1  = <z102>-vl_eixo1 / 100.
            <z102>-vl_eixo2  = <z102>-vl_eixo2 / 100.
            <z102>-vl_eixo3  = <z102>-vl_eixo3 / 100.
            <z102>-vl_eixo4  = <z102>-vl_eixo4 / 100.
            <z102>-vl_eixo5  = <z102>-vl_eixo5 / 100.
            <z102>-vl_eixo6  = <z102>-vl_eixo6 / 100.
            <z102>-vl_eixo7  = <z102>-vl_eixo7 / 100.
            <z102>-vl_eixo8  = <z102>-vl_eixo8 / 100.
            <z102>-vl_eixo9  = <z102>-vl_eixo9 / 100.
            <z102>-vl_eixo10 = <z102>-vl_eixo10 / 100.
*** Inicio - Rubenilson - 17.12.24 - US160867
            <z102>-vl_tageixo1  = <z102>-vl_tageixo1 / 100.
            <z102>-vl_tageixo2  = <z102>-vl_tageixo2 / 100.
            <z102>-vl_tageixo3  = <z102>-vl_tageixo3 / 100.
            <z102>-vl_tageixo4  = <z102>-vl_tageixo4 / 100.
            <z102>-vl_tageixo5  = <z102>-vl_tageixo5 / 100.
            <z102>-vl_tageixo6  = <z102>-vl_tageixo6 / 100.
            <z102>-vl_tageixo7  = <z102>-vl_tageixo7 / 100.
            <z102>-vl_tageixo8  = <z102>-vl_tageixo8 / 100.
            <z102>-vl_tageixo9  = <z102>-vl_tageixo9 / 100.
            <z102>-vl_tageixo10 = <z102>-vl_tageixo10 / 100.
*** Fim - Rubenilson - 17.12.24 - US160867
            READ TABLE it_zlest0102 INTO wa_zlest0102
                                    WITH KEY id_rota_adm = wa_zlest010a-id_rota_adm
                                             id_praca    = <z102>-id_praca.
            IF sy-subrc IS INITIAL.
              MODIFY it_zlest0102 INDEX sy-tabix FROM <z102>.
            ELSE.
              APPEND <z102> TO it_zlest0102.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      LOOP AT it_zlest0102 ASSIGNING <z102>.
        READ TABLE e_pracas INTO wa_zlest0102
                            WITH KEY id_rota_adm = <z102>-id_rota_adm
                                     id_praca    = <z102>-id_praca.
        IF sy-subrc IS NOT INITIAL.
          <z102>-st_praca = abap_false.
        ENDIF.
      ENDLOOP.

      LOOP AT it_zlest0101 ASSIGNING <z101> WHERE st_rota = abap_true.
        <z101>-vl_eixo1  = 0.
        <z101>-vl_eixo2  = 0.
        <z101>-vl_eixo3  = 0.
        <z101>-vl_eixo4  = 0.
        <z101>-vl_eixo5  = 0.
        <z101>-vl_eixo6  = 0.
        <z101>-vl_eixo7  = 0.
        <z101>-vl_eixo8  = 0.
        <z101>-vl_eixo9  = 0.
        <z101>-vl_eixo10 = 0.
*** Inicio - Rubenilson - 17.12.24 - US160867
        <z101>-vl_tageixo1  = 0.
        <z101>-vl_tageixo2  = 0.
        <z101>-vl_tageixo3  = 0.
        <z101>-vl_tageixo4  = 0.
        <z101>-vl_tageixo5  = 0.
        <z101>-vl_tageixo6  = 0.
        <z101>-vl_tageixo7  = 0.
        <z101>-vl_tageixo8  = 0.
        <z101>-vl_tageixo9  = 0.
        <z101>-vl_tageixo10 = 0.
*** Fim - Rubenilson - 17.12.24 - US160867
        LOOP AT it_zlest0102 INTO wa_zlest0102 WHERE st_praca = abap_true AND id_rota EQ <z101>-id_rota.
          ADD wa_zlest0102-vl_eixo1  TO <z101>-vl_eixo1.
          ADD wa_zlest0102-vl_eixo2  TO <z101>-vl_eixo2.
          ADD wa_zlest0102-vl_eixo3  TO <z101>-vl_eixo3.
          ADD wa_zlest0102-vl_eixo4  TO <z101>-vl_eixo4.
          ADD wa_zlest0102-vl_eixo5  TO <z101>-vl_eixo5.
          ADD wa_zlest0102-vl_eixo6  TO <z101>-vl_eixo6.
          ADD wa_zlest0102-vl_eixo7  TO <z101>-vl_eixo7.
          ADD wa_zlest0102-vl_eixo8  TO <z101>-vl_eixo8.
          ADD wa_zlest0102-vl_eixo9  TO <z101>-vl_eixo9.
          ADD wa_zlest0102-vl_eixo10 TO <z101>-vl_eixo10.
*** Inicio - Rubenilson - 17.12.24 - US160867
          ADD wa_zlest0102-vl_tageixo1  TO <z101>-vl_tageixo1.
          ADD wa_zlest0102-vl_tageixo2  TO <z101>-vl_tageixo2.
          ADD wa_zlest0102-vl_tageixo3  TO <z101>-vl_tageixo3.
          ADD wa_zlest0102-vl_tageixo4  TO <z101>-vl_tageixo4.
          ADD wa_zlest0102-vl_tageixo5  TO <z101>-vl_tageixo5.
          ADD wa_zlest0102-vl_tageixo6  TO <z101>-vl_tageixo6.
          ADD wa_zlest0102-vl_tageixo7  TO <z101>-vl_tageixo7.
          ADD wa_zlest0102-vl_tageixo8  TO <z101>-vl_tageixo8.
          ADD wa_zlest0102-vl_tageixo9  TO <z101>-vl_tageixo9.
          ADD wa_zlest0102-vl_tageixo10 TO <z101>-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
        ENDLOOP.
      ENDLOOP.

      SORT it_zlest0101 BY id_rota_adm.

      LOOP AT it_zlest0084 ASSIGNING <z084>.
        READ TABLE it_zlest0101 INTO wa_zlest0101 WITH KEY id_rota_adm = <z084>-id_rota.
        IF sy-subrc IS NOT INITIAL.
          <z084>-st_rota = abap_false.
        ELSE.
          READ TABLE it_zlest0091 INTO wa_zlest0091 WITH KEY categoria = <z084>-cat_veiculo.
          <z084>-st_rota  = wa_zlest0101-st_rota.
          CASE wa_zlest0091-qtd_eixo.
            WHEN 1.
              <z084>-vlr_pedagio    = wa_zlest0101-vl_eixo1.
              <z084>-nv_vl_pedagio  = wa_zlest0101-vl_eixo1.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo1."Rubenilson - 17.12.24 - US160867
            WHEN 2.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo2.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo2.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo2."Rubenilson - 17.12.24 - US160867
            WHEN 3.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo3.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo3.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo3."Rubenilson - 17.12.24 - US160867
            WHEN 4.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo4.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo4.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo4."Rubenilson - 17.12.24 - US160867
            WHEN 5.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo5.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo5.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo5."Rubenilson - 17.12.24 - US160867
            WHEN 6.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo6.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo6.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo6."Rubenilson - 17.12.24 - US160867
            WHEN 7.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo7.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo7.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo7."Rubenilson - 17.12.24 - US160867
            WHEN 8.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo8.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo8.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo8."Rubenilson - 17.12.24 - US160867
            WHEN 9.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo9.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo9.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo9."Rubenilson - 17.12.24 - US160867
            WHEN 10.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo10.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo10.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo10."Rubenilson - 17.12.24 - US160867
          ENDCASE.
        ENDIF.
      ENDLOOP.

      SORT it_zlest0084 BY id_rota cat_veiculo.

      LOOP AT it_zlest0101 INTO wa_zlest0101.
        LOOP AT it_zlest0091 INTO wa_zlest0091.
          READ TABLE it_zlest0084 INTO wa_zlest0084 WITH KEY id_rota     = wa_zlest0101-id_rota_adm
                                                             bukrs       = wa_zlest0101-bukrs
                                                             branch      = wa_zlest0101-branch
                                                             cat_veiculo = wa_zlest0091-categoria.
          IF sy-subrc IS INITIAL.
            wa_zlest0084-id_rota       = wa_zlest0101-id_rota_adm.

            CASE wa_zlest0091-qtd_eixo.
              WHEN 1.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo1.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo1.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo1."Rubenilson - 17.12.24 - US160867
              WHEN 2.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo2.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo2.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo2."Rubenilson - 17.12.24 - US160867
              WHEN 3.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo3.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo3.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo3."Rubenilson - 17.12.24 - US160867
              WHEN 4.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo4.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo4.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo4."Rubenilson - 17.12.24 - US160867
              WHEN 5.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo5.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo5.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo5."Rubenilson - 17.12.24 - US160867
              WHEN 6.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo6.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo6.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo6."Rubenilson - 17.12.24 - US160867
              WHEN 7.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo7.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo7.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo7."Rubenilson - 17.12.24 - US160867
              WHEN 8.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo8.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo8.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo8."Rubenilson - 17.12.24 - US160867
              WHEN 9.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo9.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo9.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo9."Rubenilson - 17.12.24 - US160867
              WHEN 10.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo10.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo10.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo10."Rubenilson - 17.12.24 - US160867
            ENDCASE.

            wa_zlest0084-st_rota       = wa_zlest0101-st_rota.
            MODIFY it_zlest0084 INDEX sy-tabix FROM wa_zlest0084.
          ELSE.
            CLEAR: wa_zlest0084.
            wa_zlest0084-id_rota       = wa_zlest0101-id_rota_adm.
            wa_zlest0084-bukrs         = wa_zlest0101-bukrs.
            wa_zlest0084-branch        = wa_zlest0101-branch.
            wa_zlest0084-cat_veiculo   = wa_zlest0091-categoria.
            wa_zlest0084-munic_origem  = wa_zlest0101-cd_cid_origem+3(7).
            wa_zlest0084-munic_destino = wa_zlest0101-cd_cid_destino+3(7).

            CASE wa_zlest0091-qtd_eixo.
              WHEN 1.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo1.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo1.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo1."Rubenilson - 17.12.24 - US160867
              WHEN 2.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo2.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo2.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo2."Rubenilson - 17.12.24 - US160867
              WHEN 3.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo3.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo3.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo3."Rubenilson - 17.12.24 - US160867
              WHEN 4.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo4.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo4.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo4."Rubenilson - 17.12.24 - US160867
              WHEN 5.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo5.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo5.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo5."Rubenilson - 17.12.24 - US160867
              WHEN 6.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo6.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo6.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo6."Rubenilson - 17.12.24 - US160867
              WHEN 7.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo7.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo7.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo7."Rubenilson - 17.12.24 - US160867
              WHEN 8.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo8.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo8.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo8."Rubenilson - 17.12.24 - US160867
              WHEN 9.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo9.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo9.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo9."Rubenilson - 17.12.24 - US160867
              WHEN 10.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo10.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo10.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo10."Rubenilson - 17.12.24 - US160867
            ENDCASE.

            wa_zlest0084-st_rota       = wa_zlest0101-st_rota.
            APPEND wa_zlest0084 TO it_zlest0084.
            SORT it_zlest0084 BY id_rota cat_veiculo.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      MODIFY zlest0101 FROM TABLE it_zlest0101.
      MODIFY zlest0102 FROM TABLE it_zlest0102.
      MODIFY zlest0084 FROM TABLE it_zlest0084.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD atualizar_valores.
*****************
*  Descrição: Método responsavel por atualizar os novos valores para o pedágio e sua data de vigencia.
*  Data: 05.05.2014 14:43:10
*****************

    DATA: obj_zcl_rota_db TYPE REF TO zcl_rota_db.

    FREE obj_zcl_rota_db.

    CREATE OBJECT obj_zcl_rota_db.
    obj_zcl_rota_db->seleciona_novos( i_grupo = i_grupo ).

  ENDMETHOD.


  METHOD autenticacao.
    "Método para Autenticação com a Tipcard.

    "Esse item trata do serviço de autenticação para consumir os serviços
    "disponibilizados pelo WSConvenio. Na requisição é informado um usuário e
    "uma senha. E na reposta é retornada uma chave válida para consumo dos
    "serviços.

    "Estrutura
    "<autenticacao>
    "  <usuario>XXXXXXXXXXXX</usuario>
    "  <senha>XXXXXXXXXXXXXX</senha>
    "</autenticacao>

    DATA: gw_zlest0085  TYPE zlest0085, "Tabela de Cadastro de Usuário e Senha para autenticação TIPCARD.
          xml_ret       TYPE REF TO cl_xml_document,
          xml_node      TYPE REF TO if_ixml_node,
          xml_autentica TYPE string. "Montagem do XML Para Autenticação.

    DATA: var_resultado TYPE string, "Resultado do XML de Autenticação.
          var_tam       TYPE i, "Variavel para guardar o tamanho do XML de retorno.
          var_valor     TYPE string. "Valor da TAG.

    DATA: zescl_rota_db TYPE REF TO zescl_rota_db. "Classe para Persistencia no Banco de Dados.


    "Limpar as variaveis, work areas e objetos.
    CLEAR: xml_autentica, var_resultado, var_tam,
           gw_zlest0085,  xml_ret, xml_node, e_chave.

    "Clear no Objeto.
    FREE: zescl_rota_db.

    "Selecionar o usuário e senha de autenticação da tipcard.
    SELECT SINGLE * FROM zlest0085 INTO gw_zlest0085
     WHERE operadora EQ i_operadora
       AND ds_grupo  EQ i_grupo.

    IF ( sy-subrc EQ 0 ).

      "Monta o arquivo
      xml_autentica = me->xml_autentica(
                                         i_usuario = gw_zlest0085-usuario
                                         i_senha   = gw_zlest0085-senha
                                       ).

      me->zesif_webservice~abrir_conexao( i_http = i_http ).

      CALL METHOD me->zesif_webservice~consultar
        EXPORTING
          i_http                     = i_http
          i_xml                      = xml_autentica
        RECEIVING
          e_resultado                = var_resultado
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.

      IF sy-subrc IS NOT INITIAL.
*-168836-26.02.2025-JT-inicio
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(msg_texto).
*       zescx_webservice=>gera_erro_geral( i_texto = msg_texto ).
*-168836-26.02.2025-JT-fim
      ENDIF.

      CREATE OBJECT xml_ret.

      CALL METHOD xml_ret->parse_string
        EXPORTING
          stream  = var_resultado
        RECEIVING
          retcode = var_tam.

      CALL METHOD xml_ret->find_node
        EXPORTING
          name = 'msg'
        RECEIVING
          node = xml_node.

      IF ( sy-subrc EQ 0 ) AND ( NOT xml_node IS INITIAL ).

        CALL METHOD xml_node->get_value
          RECEIVING
            rval = var_valor.

        CASE var_valor.
          WHEN: 'ok' OR 'OK'.

            CLEAR: var_valor, xml_node.

            CALL METHOD xml_ret->find_node
              EXPORTING
                name = 'chave'
              RECEIVING
                node = xml_node.

            IF ( sy-subrc EQ 0 ) AND ( NOT xml_node IS INITIAL ).

              CALL METHOD xml_node->get_value
                RECEIVING
                  rval = var_valor.

              CREATE OBJECT zescl_rota_db.

              zescl_rota_db->gravar_chave( i_chave = var_valor i_grupo = i_grupo ). "Método para gravar a chave no banco de dados e manter o seu controle.

              e_chave = var_valor. "Retorna o Valor da nova CHAVE

            ELSE.
              RAISE EXCEPTION TYPE zescx_webservice EXPORTING textid = zescx_webservice=>erro_no_xml.
            ENDIF.

          WHEN: OTHERS.
            MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH 'Ocorreu um erro no retorno do XML de autenticação'
                                                             var_valor.
        ENDCASE.
      ELSE.
        RAISE EXCEPTION TYPE zescx_webservice EXPORTING textid = zescx_webservice=>erro_no_xml.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zescx_webservice EXPORTING textid = zescx_webservice=>autenticacao_nao_encontrado.
    ENDIF.

  ENDMETHOD.


  METHOD buscar_rotas.

    CONSTANTS: var_set_contratante TYPE setnamenew VALUE 'MAGGI_PEDAGIO_AG_PROP'. "Constante para o SET Agentes Proprio.

    DATA: gt_setleaf TYPE TABLE OF setleaf, "Internal Table para o SET
          gw_setleaf TYPE setleaf. "Work Area para o SET

    DATA: gw_xml      TYPE string, "String para guardar informações do XML
          gw_xml_rota TYPE string. "String para guardar informações do XML da Rota.

    DATA: var_chave TYPE c LENGTH 32, "Variavel para guardar a CHAVE
          var_msg   TYPE string, "Variavel para mostrar a Mensagem texto da exception.
          var_http  TYPE REF TO if_http_client. "Interface HTTP Client

    DATA: cx_exception TYPE REF TO zescx_webservice. "Referencia para a Classe de Exception.

    "Limpar tabela internal e work area.
    REFRESH: gt_setleaf.
    CLEAR: gw_setleaf.

    "Selecionar o SET para o Agente Proprio.
    SELECT * FROM setleaf
      INTO TABLE gt_setleaf
    WHERE setname EQ var_set_contratante.

    IF ( sy-subrc EQ 0 ).

      LOOP AT gt_setleaf INTO gw_setleaf.

        TRY .

            var_chave = me->chave_seguranca( i_grupo = i_grupo ). "Recuperar a Chave de Segurança.

          CATCH zescx_webservice INTO cx_exception .
            var_msg  = cx_exception->get_text( ).
            MESSAGE e007(zwebservice) WITH var_msg.
        ENDTRY.

        "Caso a chave não seja encontrada o loop deve continuar.
        IF ( var_chave IS INITIAL ).
          CLEAR: gw_setleaf.
          CONTINUE.
        ELSE.

          "Chama o Método XML_ATUALIZ_ROTA onde é feito a montagem do arquivo
          "XML e para que possamos enviar para a TIPCARD e consultar toda as
          "rotas que precisam ser atualizadas referente ao CNPJ do Contratante.
          gw_xml = xml_atualiza_rota( i_chave = var_chave
                                      i_cnpjcontratante = gw_setleaf-valfrom
                                    ).

          TRY .

              "Atribui o serviço que precisa ser consultado.
              "PE = Pedagio.
              me->set_servico( EXPORTING i_servico = 'PE' ).

            CATCH zescx_webservice INTO cx_exception .
              var_msg  = cx_exception->get_text( ).
              MESSAGE e007(zwebservice) WITH var_msg.
          ENDTRY.

          "Atribui o Tipo de Serviço
          "A = atualização.
          me->set_tipo( EXPORTING i_tipo = 'A').

          TRY .

              "Atribui as Informações do HTTP Client para consultar o WebService.
              var_http = me->url( ).


            CATCH zescx_webservice INTO cx_exception .
              var_msg  = cx_exception->get_text( ).
              MESSAGE e007(zwebservice) WITH var_msg.
          ENDTRY.

          me->zesif_webservice~abrir_conexao( var_http ).

        ENDIF.

        "Envia para Consultar a rota com as informações preenchidas acima.
        "O retorno é de um arquivo XML com todas as rotas que precisam ser atualizadas.
        gw_xml_rota = me->zesif_webservice~consultar( i_http = var_http
                                                    i_xml  = gw_xml
                                                   ).

        me->ler_xml_rotas( i_grupo = i_grupo i_xml = gw_xml_rota ).

        CLEAR: gw_setleaf, gw_xml, gw_xml_rota.
        CLEAR: var_chave, var_http, var_msg.
      ENDLOOP.

    ELSE.
      "Quando não existir parametrização no SET para o contratante a mensagem abaixo devera
      "ser exibida.
      MESSAGE e007(zwebservice) WITH 'Nenhum contratante foi cadastrado no SET' 'MAGGI_PEDAGIO_AG_PROP'.
    ENDIF.
  ENDMETHOD.


  METHOD chave_seguranca.
*****************
*  Descrição: Método para retornar a chave de Segurança da Tipcard.
*  Data: 07.04.2014 14:09:41
*  Developer: Victor Hugo Souza Nunes
*****************

    DATA: gw_zlest0086 TYPE zlest0086, "Controle da Chave de Autenticação TIPCARD
          calc_hora    TYPE i,         "Variavel para calcular a hora.
          var_http     TYPE REF TO if_http_client,
          cx_exception TYPE REF TO zescx_webservice, "Classe da Exception
          msg          TYPE string. "String de Mensagem.

    CLEAR: gw_zlest0086, calc_hora.

    "Selecionar na tabela de Controle de Chave a chave atual.
    SELECT SINGLE * INTO gw_zlest0086
      FROM zlest0086
     WHERE ds_grupo EQ i_grupo.

    IF ( sy-subrc EQ 0 ).

      calc_hora = ( ( sy-uzeit - gw_zlest0086-hora ) / 60 ). "Transformar a hora cadastrada em minutos.
      IF ( calc_hora < 0 ).
        calc_hora = calc_hora * -1.
      ENDIF.



      "Caso a hora seja maior ou igual a 50 minutos
      "Solicitar uma nova chave ao serviço de Autenticação da Tipcard.
      IF 1 EQ 1."( CALC_HORA >= 50 ) OR ( GW_ZLEST0086-DATA NE SY-DATUM ).
        "IF ( CALC_HORA >= 50 ) OR ( GW_ZLEST0086-DATA NE SY-DATUM ).

        "TRY .

        "Atribui o serviço que precisa ser consultado.
        "PE = Pedagio.
        "TRY .
        me->set_servico( i_servico = 'VI' ).
        "  CATCH ZESCX_WEBSERVICE INTO CX_EXCEPTION.
        "    MSG = CX_EXCEPTION->GET_TEXT( ).
        "    MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
        "ENDTRY.

        "Atribui o Tipo de Serviço
        "A = atualização.
        me->set_tipo( i_tipo = 'C').

        var_http = me->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.

        "Recuperar a Chave de Acesso cas não tenha registro na tabela de controle
        "ou o seu tempo de autorização esteja ultrapassado.

        e_chave = me->autenticacao(
                                   i_http      = var_http
                                   i_operadora = 'TIP'
                                   i_grupo     = i_grupo
                                   ).


        "   CATCH ZESCX_WEBSERVICE INTO CX_EXCEPTION.
        "     MSG = CX_EXCEPTION->GET_TEXT( ).
        "     MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
        " ENDTRY.
      ELSE.
        "Retornar a chave que esta cadastrada caso a mesma ainda seja menor que 50 minutos.
        e_chave = gw_zlest0086-chave.
      ENDIF.
    ELSE.
      "RAISE EXCEPTION TYPE ZESCX_WEBSERVICE EXPORTING TEXTID = ZESCX_WEBSERVICE=>AUTENTICACAO_NAO_ENCONTRADO.
      "TRY .
      "Atribui o serviço que precisa ser consultado.
      "PE = Pedagio.
      "TRY .
      me->set_servico( i_servico = 'VI' ).
      "  CATCH ZESCX_WEBSERVICE INTO CX_EXCEPTION.
      "    MSG = CX_EXCEPTION->GET_TEXT( ).
      "    MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
      "ENDTRY.

      "Atribui o Tipo de Serviço
      "A = atualização.
      me->set_tipo( i_tipo = 'C').

      var_http = me->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.

      "Recuperar a Chave de Acesso cas não tenha registro na tabela de controle
      "ou o seu tempo de autorização esteja ultrapassado.

      e_chave = me->autenticacao( i_http = var_http i_operadora = 'TIP' i_grupo = i_grupo ).

      "   CATCH ZESCX_WEBSERVICE INTO CX_EXCEPTION.
      "     MSG = CX_EXCEPTION->GET_TEXT( ).
      "     MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
      " ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD consultar_arquivo_cobranca.

    DATA: cx_exception TYPE REF TO zescx_webservice.

    CLEAR: r_linkarquivo, e_msg.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    TRY .
        DATA(e_chave) = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zescx_webservice INTO cx_exception .
        DATA(lc_msg)  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    DATA(lc_xml) = me->xml_consultar_arq_cobranca( i_chave = e_chave i_bukrs = i_bukrs i_branch = i_branch  i_fatura = i_fatura ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RC - Consultar Rota.
        me->set_servico( EXPORTING i_servico = 'CB' ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'B').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        DATA(lc_http) = me->url( ).
      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zesif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zesif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = DATA(lc_msg_adm)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    me->ler_xml_consultar_arq_cobranca( EXPORTING i_xml                     = lc_msg_adm
                                        IMPORTING e_msg                     = e_msg
                                                  r_linkarquivo             = r_linkarquivo
                                                  r_linkarquivo_pedagio     = r_linkarquivo_pedagio
                                                  r_content_arquivo         = r_content_arquivo            "*-US 130492-08.04.2024-JT
                                                  r_content_arquivo_pedagio = r_content_arquivo_pedagio ). "*-US 130492-08.04.2024-JT

    IF e_msg NE 'SUCESSO'.
      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ENDIF.

  ENDMETHOD.


  METHOD consultar_arquivo_conferencia.

    DATA: cx_exception TYPE REF TO zescx_webservice.

    CLEAR: r_linkarquivo, e_msg.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs   EQ @i_bukrs
       AND branch  EQ @i_branch.

    TRY .
        DATA(e_chave) = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zescx_webservice INTO cx_exception .
        DATA(lc_msg)  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    DATA(lc_xml) = me->xml_consultar_arq_conferencia( i_chave = e_chave i_bukrs = i_bukrs i_branch = i_branch i_dt_inicial = i_dt_inicial i_dt_final = i_dt_final ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RC - Consultar Rota.
        me->set_servico( EXPORTING i_servico = 'CA' ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'Q').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        DATA(lc_http) = me->url( ).
      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zesif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zesif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = DATA(lc_msg_adm)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    r_linkarquivo = me->ler_xml_consultar_arq_confere( EXPORTING i_xml = lc_msg_adm IMPORTING e_msg = e_msg ).

    IF e_msg NE 'SUCESSO'.
      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ENDIF.

  ENDMETHOD.


  METHOD consultar_rota.

    DATA: e_chave      TYPE char32,
          lc_msg       TYPE string,
          lc_msg_adm   TYPE string,
          cx_exception TYPE REF TO zescx_webservice,
          lc_xml       TYPE string,
          lc_http      TYPE REF TO if_http_client,
          wa_zlest0101 TYPE zlest0101,
          e_rotas	     TYPE	zlest0101_t,
          wa_rotas     TYPE	zlest0101,
          e_pracas     TYPE zlest0102_t,
          it_zlest0091 TYPE TABLE OF zlest0091,
          wa_zlest0091 TYPE zlest0091,
          it_zlest0102 TYPE TABLE OF zlest0102,
          wa_zlest0102 TYPE zlest0102,
          it_zlest0084 TYPE TABLE OF zlest0084,
          wa_zlest0084 TYPE zlest0084,
          lc_divisaor  TYPE i.

    FIELD-SYMBOLS: <z101> TYPE zlest0101,
                   <z102> TYPE zlest0102,
                   <z084> TYPE zlest0084.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    SELECT SINGLE * INTO wa_zlest0101
      FROM zlest0101
     WHERE id_rota EQ i_rota
       AND bukrs   EQ i_bukrs
       AND branch  EQ i_branch.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e088(zles) WITH i_rota RAISING zwebservice.
      "elseif wa_zlest0101-id_rota_adm is not initial.
      "  message e087(zles) raising zwebservice.
    ENDIF.

    TRY .
        e_chave = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    lc_xml = xml_consultar_rota( i_chave = e_chave i_zlest0101 = wa_zlest0101 ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RC - Consultar Rota.
        me->set_servico( EXPORTING i_servico = 'RC' ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'R').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        lc_http = me->url( ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zesif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zesif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = lc_msg_adm
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    CALL METHOD me->ler_xml_consultar_rota
      EXPORTING
        i_xml    = lc_msg_adm
      IMPORTING
        e_rotas  = e_rotas
        e_pracas = e_pracas
        e_msg    = e_msg.

    IF e_msg NE 'Sucesso'.
      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ELSE.

      SELECT SINGLE * INTO wa_zlest0101
        FROM zlest0101
       WHERE id_rota EQ i_rota
         AND bukrs   EQ i_bukrs
         AND branch  EQ i_branch.

      SELECT * INTO TABLE it_zlest0091
        FROM zlest0091.

      IF sy-subrc IS INITIAL.
        SELECT * INTO TABLE it_zlest0102
          FROM zlest0102
         WHERE id_rota EQ wa_zlest0101-id_rota
           AND bukrs   EQ wa_zlest0101-bukrs
           AND branch  EQ wa_zlest0101-branch.

        SELECT * INTO TABLE it_zlest0084
          FROM zlest0084
         WHERE id_rota EQ wa_zlest0101-id_rota_adm
           AND bukrs   EQ wa_zlest0101-bukrs
           AND branch  EQ wa_zlest0101-branch.
      ENDIF.

      READ TABLE e_rotas INDEX 1 INTO wa_rotas.
      IF sy-subrc IS INITIAL.
        IF wa_rotas-bukrs          EQ wa_zlest0101-bukrs   AND
           wa_rotas-branch         EQ wa_zlest0101-branch  AND
           wa_rotas-country        EQ wa_zlest0101-country AND
           wa_rotas-cd_cid_origem  EQ wa_zlest0101-cd_cid_origem  AND
           wa_rotas-cd_cid_destino EQ wa_zlest0101-cd_cid_destino AND
           wa_rotas-tp_rota_perc   EQ wa_zlest0101-tp_rota_perc AND
           wa_rotas-id_rota_adm    EQ wa_zlest0101-id_rota_adm.

          wa_rotas-st_rota = abap_true.
          wa_rotas-id_rota = wa_zlest0101-id_rota.

          "Atualiza Praças
          LOOP AT e_pracas ASSIGNING <z102> WHERE id_rota_adm EQ wa_zlest0101-id_rota_adm.

            IF <z102>-vl_precisao EQ 0.
              lc_divisaor = 1.
            ELSE.
              lc_divisaor = 0.
              DO <z102>-vl_precisao TIMES.
                IF lc_divisaor EQ 0.
                  lc_divisaor = 10.
                ELSE.
                  lc_divisaor = 10 * lc_divisaor.
                ENDIF.
              ENDDO.
            ENDIF.
            <z102>-bukrs     = wa_zlest0101-bukrs.
            <z102>-branch    = wa_zlest0101-branch.
            <z102>-id_rota   = wa_zlest0101-id_rota.
            <z102>-st_praca  = abap_true.
            <z102>-vl_eixo1  = <z102>-vl_eixo1  / lc_divisaor.
            <z102>-vl_eixo2  = <z102>-vl_eixo2  / lc_divisaor.
            <z102>-vl_eixo3  = <z102>-vl_eixo3  / lc_divisaor.
            <z102>-vl_eixo4  = <z102>-vl_eixo4  / lc_divisaor.
            <z102>-vl_eixo5  = <z102>-vl_eixo5  / lc_divisaor.
            <z102>-vl_eixo6  = <z102>-vl_eixo6  / lc_divisaor.
            <z102>-vl_eixo7  = <z102>-vl_eixo7  / lc_divisaor.
            <z102>-vl_eixo8  = <z102>-vl_eixo8  / lc_divisaor.
            <z102>-vl_eixo9  = <z102>-vl_eixo9  / lc_divisaor.
            <z102>-vl_eixo10 = <z102>-vl_eixo10 / lc_divisaor.
*** Inicio - Rubenilson - 17.12.24 - US160867
            <z102>-vl_tageixo1  = <z102>-vl_tageixo1 / lc_divisaor.
            <z102>-vl_tageixo2  = <z102>-vl_tageixo2 / lc_divisaor.
            <z102>-vl_tageixo3  = <z102>-vl_tageixo3 / lc_divisaor.
            <z102>-vl_tageixo4  = <z102>-vl_tageixo4 / lc_divisaor.
            <z102>-vl_tageixo5  = <z102>-vl_tageixo5 / lc_divisaor.
            <z102>-vl_tageixo6  = <z102>-vl_tageixo6 / lc_divisaor.
            <z102>-vl_tageixo7  = <z102>-vl_tageixo7 / lc_divisaor.
            <z102>-vl_tageixo8  = <z102>-vl_tageixo8 / lc_divisaor.
            <z102>-vl_tageixo9  = <z102>-vl_tageixo9 / lc_divisaor.
            <z102>-vl_tageixo10 = <z102>-vl_tageixo10 / lc_divisaor.
***Fim - Rubenilson - 17.12.24 - US160867
            READ TABLE it_zlest0102 INTO wa_zlest0102
                                    WITH KEY id_rota_adm = wa_zlest0101-id_rota_adm
                                             id_praca    = <z102>-id_praca.
            IF sy-subrc IS INITIAL.
              MODIFY it_zlest0102 INDEX sy-tabix FROM <z102>.
            ELSE.
              APPEND <z102> TO it_zlest0102.
            ENDIF.
          ENDLOOP.

          "Desativa Praças
          LOOP AT it_zlest0102 ASSIGNING <z102>.
            READ TABLE e_pracas INTO wa_zlest0102
                                WITH KEY id_rota_adm = <z102>-id_rota_adm
                                         id_praca    = <z102>-id_praca.
            IF sy-subrc IS NOT INITIAL.
              <z102>-st_praca = abap_false.
            ENDIF.
          ENDLOOP.

          wa_rotas-vl_eixo1  = 0.
          wa_rotas-vl_eixo2  = 0.
          wa_rotas-vl_eixo3  = 0.
          wa_rotas-vl_eixo4  = 0.
          wa_rotas-vl_eixo5  = 0.
          wa_rotas-vl_eixo6  = 0.
          wa_rotas-vl_eixo7  = 0.
          wa_rotas-vl_eixo8  = 0.
          wa_rotas-vl_eixo9  = 0.
          wa_rotas-vl_eixo10 = 0.
*** Inicio - Rubenilson - 17.12.24 - US160867
          wa_rotas-vl_tageixo1  = 0.
          wa_rotas-vl_tageixo2  = 0.
          wa_rotas-vl_tageixo3  = 0.
          wa_rotas-vl_tageixo4  = 0.
          wa_rotas-vl_tageixo5  = 0.
          wa_rotas-vl_tageixo6  = 0.
          wa_rotas-vl_tageixo7  = 0.
          wa_rotas-vl_tageixo8  = 0.
          wa_rotas-vl_tageixo9  = 0.
          wa_rotas-vl_tageixo10 = 0.
*** Fim - Rubenilson - 17.12.24 - US160867
          LOOP AT it_zlest0102 INTO wa_zlest0102 WHERE st_praca = abap_true AND id_rota EQ wa_rotas-id_rota.
            ADD wa_zlest0102-vl_eixo1 TO wa_rotas-vl_eixo1.
            ADD wa_zlest0102-vl_eixo2  TO wa_rotas-vl_eixo2.
            ADD wa_zlest0102-vl_eixo3  TO wa_rotas-vl_eixo3.
            ADD wa_zlest0102-vl_eixo4  TO wa_rotas-vl_eixo4.
            ADD wa_zlest0102-vl_eixo5  TO wa_rotas-vl_eixo5.
            ADD wa_zlest0102-vl_eixo6  TO wa_rotas-vl_eixo6.
            ADD wa_zlest0102-vl_eixo7  TO wa_rotas-vl_eixo7.
            ADD wa_zlest0102-vl_eixo8  TO wa_rotas-vl_eixo8.
            ADD wa_zlest0102-vl_eixo9  TO wa_rotas-vl_eixo9.
            ADD wa_zlest0102-vl_eixo10 TO wa_rotas-vl_eixo10.
*** Início - Rubenilson - 17.12.24 - US160867
            ADD wa_zlest0102-vl_tageixo1 TO wa_rotas-vl_tageixo1.
            ADD wa_zlest0102-vl_tageixo2  TO wa_rotas-vl_tageixo2.
            ADD wa_zlest0102-vl_tageixo3  TO wa_rotas-vl_tageixo3.
            ADD wa_zlest0102-vl_tageixo4  TO wa_rotas-vl_tageixo4.
            ADD wa_zlest0102-vl_tageixo5  TO wa_rotas-vl_tageixo5.
            ADD wa_zlest0102-vl_tageixo6  TO wa_rotas-vl_tageixo6.
            ADD wa_zlest0102-vl_tageixo7  TO wa_rotas-vl_tageixo7.
            ADD wa_zlest0102-vl_tageixo8  TO wa_rotas-vl_tageixo8.
            ADD wa_zlest0102-vl_tageixo9  TO wa_rotas-vl_tageixo9.
            ADD wa_zlest0102-vl_tageixo10 TO wa_rotas-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
          ENDLOOP.

          LOOP AT it_zlest0084 ASSIGNING <z084>.
            READ TABLE it_zlest0091 INTO wa_zlest0091 WITH KEY categoria = <z084>-cat_veiculo.
            <z084>-st_rota  = wa_rotas-st_rota.
            CASE wa_zlest0091-qtd_eixo.
              WHEN 1.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo1.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo1.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo1."Rubenilson - 17.12.24 - US160867
              WHEN 2.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo2.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo2.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo2."Rubenilson - 17.12.24 - US160867
              WHEN 3.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo3.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo3.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo3."Rubenilson - 17.12.24 - US160867
              WHEN 4.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo4.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo4.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo4."Rubenilson - 17.12.24 - US160867
              WHEN 5.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo5.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo5.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo5."Rubenilson - 17.12.24 - US160867
              WHEN 6.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo6.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo6.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo6."Rubenilson - 17.12.24 - US160867
              WHEN 7.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo7.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo7.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo7."Rubenilson - 17.12.24 - US160867
              WHEN 8.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo8.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo8.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo8."Rubenilson - 17.12.24 - US160867
              WHEN 9.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo9.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo9.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo9."Rubenilson - 17.12.24 - US160867
              WHEN 10.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo10.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo10.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo10."Rubenilson - 17.12.24 - US160867
            ENDCASE.
          ENDLOOP.

          SORT it_zlest0084 BY id_rota cat_veiculo.

          LOOP AT it_zlest0091 INTO wa_zlest0091.
            READ TABLE it_zlest0084 INTO wa_zlest0084 WITH KEY id_rota     = wa_rotas-id_rota_adm
                                                               cat_veiculo = wa_zlest0091-categoria.
            IF sy-subrc IS INITIAL.
              wa_zlest0084-id_rota       = wa_rotas-id_rota_adm.

              CASE wa_zlest0091-qtd_eixo.
                WHEN 1.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo1.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo1.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo1."Rubenilson - 17.12.24 - US160867
                WHEN 2.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo2.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo2.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo2."Rubenilson - 17.12.24 - US160867
                WHEN 3.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo3.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo3.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo3."Rubenilson - 17.12.24 - US160867
                WHEN 4.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo4.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo4.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo4."Rubenilson - 17.12.24 - US160867
                WHEN 5.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo5.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo5.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo5."Rubenilson - 17.12.24 - US160867
                WHEN 6.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo6.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo6.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo6."Rubenilson - 17.12.24 - US160867
                WHEN 7.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo7.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo7.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo7."Rubenilson - 17.12.24 - US160867
                WHEN 8.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo8.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo8.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo8."Rubenilson - 17.12.24 - US160867
                WHEN 9.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo9.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo9.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo9."Rubenilson - 17.12.24 - US160867
                WHEN 10.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo10.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo10.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo10."Rubenilson - 17.12.24 - US160867
              ENDCASE.

              wa_zlest0084-st_rota       = wa_rotas-st_rota.
              MODIFY it_zlest0084 INDEX sy-tabix FROM wa_zlest0084.
            ELSE.
              CLEAR: wa_zlest0084.
              wa_zlest0084-id_rota       = wa_rotas-id_rota_adm.
              wa_zlest0084-cat_veiculo   = wa_zlest0091-categoria.
              wa_zlest0084-munic_origem  = wa_rotas-cd_cid_origem+3(7).
              wa_zlest0084-munic_destino = wa_rotas-cd_cid_destino+3(7).
              wa_zlest0084-bukrs  = wa_zlest0101-bukrs.
              wa_zlest0084-branch = wa_zlest0101-branch.

              CASE wa_zlest0091-qtd_eixo.
                WHEN 1.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo1.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo1.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo1."Rubenilson - 17.12.24 - US160867
                WHEN 2.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo2.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo2.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo2."Rubenilson - 17.12.24 - US160867
                WHEN 3.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo3.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo3.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo3."Rubenilson - 17.12.24 - US160867
                WHEN 4.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo4.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo4.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo4."Rubenilson - 17.12.24 - US160867
                WHEN 5.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo5.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo5.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo5."Rubenilson - 17.12.24 - US160867
                WHEN 6.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo6.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo6.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo6."Rubenilson - 17.12.24 - US160867
                WHEN 7.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo7.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo7.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo7."Rubenilson - 17.12.24 - US160867
                WHEN 8.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo8.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo8.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo8."Rubenilson - 17.12.24 - US160867
                WHEN 9.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo9.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo9.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo9."Rubenilson - 17.12.24 - US160867
                WHEN 10.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo10.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo10.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo10."Rubenilson - 17.12.24 - US160867
              ENDCASE.

              wa_zlest0084-st_rota       = wa_rotas-st_rota.
              APPEND wa_zlest0084 TO it_zlest0084.
              SORT it_zlest0084 BY id_rota cat_veiculo.
            ENDIF.
          ENDLOOP.

          MODIFY zlest0101 FROM wa_rotas.
          MODIFY zlest0102 FROM TABLE it_zlest0102.
          MODIFY zlest0084 FROM TABLE it_zlest0084.
          COMMIT WORK.

          MESSAGE e_msg TYPE 'S'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD cons_situacao_transportador.

    DATA: lc_webservice TYPE REF TO zescl_webservice_tipcard.

    DATA: e_chave       TYPE char32,
          cx_exception  TYPE REF TO zescx_webservice,
          lc_msg        TYPE string,
          lc_xml        TYPE string,
          lc_msg_adm    TYPE string,
          it_consultas  TYPE TABLE OF ZESDE_consulta_rntrc,
          wa_consultas  TYPE ZESDE_consulta_rntrc,
          wa_j_1bbranch TYPE j_1bbranch,
          wa_lfa1       TYPE lfa1,
          it_lfa1       TYPE TABLE OF lfa1,
          lc_http       TYPE REF TO if_http_client,
          wa_zlest0135  TYPE zlest0135,
          it_zlest0135  TYPE TABLE OF zlest0135,
          it_veiculos   TYPE TABLE OF zlest0002,
          wa_veiculos   TYPE zlest0002,
          wa_lifnr      TYPE range_lifnr,
          it_lifnr      TYPE TABLE OF range_lifnr,
          wa_placa      TYPE ZESDE_placa_range,
          it_placa      TYPE TABLE OF ZESDE_placa_range,
          i_name_file   TYPE string,
          wa_zlest0137  TYPE zlest0137,
          i_parceiro    TYPE j_1bparid.

    SELECT SINGLE * INTO wa_zlest0137 FROM zlest0137.

    IF wa_zlest0137-ck_consulta IS NOT INITIAL OR i_ck_consulta IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_zlest0160)
        FROM zlest0160
       WHERE bukrs   EQ @i_bukrs
         AND branch  EQ @i_branch.

      CREATE OBJECT lc_webservice.

      TRY .
          e_chave = lc_webservice->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
        CATCH zescx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      SELECT SINGLE * INTO wa_j_1bbranch FROM j_1bbranch
        WHERE bukrs  EQ i_bukrs
          AND branch EQ i_branch.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e112(zles) WITH i_bukrs i_branch RAISING erro.

        IF wa_j_1bbranch IS INITIAL.

          i_parceiro = wa_j_1bbranch-branch.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = i_parceiro
            IMPORTING
              output = i_parceiro.

          SELECT SINGLE * INTO @DATA(wa_lfa1_01)
            FROM lfa1
           WHERE lifnr EQ @i_parceiro.

          wa_j_1bbranch-stcd1 = wa_lfa1_01-stcd1.
        ENDIF.
      ENDIF.

      IF ( i_partiner IS INITIAL OR i_placa IS INITIAL ) AND ( i_pesquisa_livre IS INITIAL ).

        CLEAR: it_lifnr.

        IF i_partiner IS NOT INITIAL.
          wa_lifnr-sign   = 'I'.
          wa_lifnr-option = 'EQ'.
          wa_lifnr-low    = i_partiner.
          wa_lifnr-high   = i_partiner.
          APPEND wa_lifnr TO it_lifnr.
        ENDIF.

        IF i_placa IS NOT INITIAL.
          wa_placa-sign   = 'I'.
          wa_placa-option = 'EQ'.
          wa_placa-low    = i_placa.
          wa_placa-high   = i_placa.
          APPEND wa_placa TO it_placa.
        ENDIF.

        SELECT * INTO TABLE it_veiculos
          FROM zlest0002
         WHERE proprietario IN it_lifnr
           AND pc_veiculo   IN it_placa.

        IF sy-subrc IS NOT INITIAL AND i_placa IS INITIAL AND i_partiner IS NOT INITIAL.
          MESSAGE e118(zles) WITH i_partiner RAISING erro.
        ENDIF.

        IF sy-subrc IS NOT INITIAL AND i_placa IS NOT INITIAL AND i_partiner IS NOT INITIAL.
          MESSAGE e117(zles) WITH i_placa i_partiner RAISING erro.
        ENDIF.

        IF sy-subrc IS NOT INITIAL AND i_placa IS NOT INITIAL AND i_partiner IS INITIAL.
          MESSAGE e116(zles) WITH i_placa RAISING erro.
        ENDIF.

        CHECK it_veiculos IS NOT INITIAL.

        SELECT * INTO TABLE it_lfa1
          FROM lfa1
           FOR ALL ENTRIES IN it_veiculos
         WHERE lifnr EQ it_veiculos-proprietario.

        SORT it_lfa1 BY lifnr.

        LOOP AT it_veiculos INTO wa_veiculos.

          CLEAR: wa_consultas, wa_zlest0135.

          wa_consultas-cd_transportador = wa_veiculos-proprietario.
          wa_consultas-ds_placa         = wa_veiculos-pc_veiculo.

          "Fornecedor
          READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_veiculos-proprietario BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE e113(zles) WITH i_partiner RAISING erro.
          ENDIF.

          "RNTRC
          IF ( wa_lfa1-bahns IS INITIAL ) AND ( wa_zlest0135-ds_msg_transportador IS INITIAL ).
            MESSAGE e114(zles) WITH i_partiner RAISING erro.
          ENDIF.

          IF wa_zlest0135-ds_msg_transportador IS INITIAL.
            wa_consultas-cnpj_contratante = wa_j_1bbranch-stcd1.
            CASE wa_lfa1-stkzn."sfrgr. "Ajuste realizado U.S #117449 AOENNING.
              WHEN abap_true.
                wa_consultas-cnpj_contratado  = wa_lfa1-stcd2.
              WHEN abap_false.
                wa_consultas-cnpj_contratado  = wa_lfa1-stcd1.
            ENDCASE.
            wa_consultas-rntrc            = wa_lfa1-bahns.
            APPEND wa_consultas TO it_consultas.
          ELSE.
            "Não fará consulta WebService
            APPEND wa_zlest0135 TO it_zlest0135.
          ENDIF.

        ENDLOOP.

      ELSE.
        IF i_pesquisa_livre IS INITIAL.
          SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr EQ i_partiner.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE e113(zles) WITH i_partiner RAISING erro.
          ENDIF.

          IF wa_lfa1-bahns IS INITIAL.
            MESSAGE e114(zles) WITH i_partiner RAISING erro.
          ENDIF.
          wa_consultas-cd_transportador = i_partiner.
          wa_consultas-ds_placa         = i_placa.
          wa_consultas-cnpj_contratante = wa_j_1bbranch-stcd1.

          CASE wa_lfa1-sfrgr.
            WHEN abap_true.
              wa_consultas-cnpj_contratado  = wa_lfa1-stcd2.
            WHEN abap_false.
              wa_consultas-cnpj_contratado  = wa_lfa1-stcd1.
          ENDCASE.

          wa_consultas-rntrc            = wa_lfa1-bahns.
          APPEND wa_consultas TO it_consultas.
        ELSE.
          wa_consultas-ds_placa         = i_placa.
          wa_consultas-cnpj_contratante = wa_j_1bbranch-stcd1.
          wa_consultas-cnpj_contratado  = i_cnpj.
          wa_consultas-rntrc            = i_rntrc.
          APPEND wa_consultas TO it_consultas.
        ENDIF.

      ENDIF.

      TRY .
          lc_webservice->set_servico( EXPORTING i_servico = 'ST' ).
        CATCH zescx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      lc_webservice->set_tipo( EXPORTING i_tipo = 'S').

      TRY .
          lc_http = lc_webservice->url( ).
        CATCH zescx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      LOOP AT it_consultas INTO wa_consultas.

        CLEAR: lc_msg_adm.

        lc_webservice->zesif_webservice~abrir_conexao( lc_http ).

        lc_xml = lc_webservice->xml_consulta_transportador( EXPORTING i_chave = e_chave i_consulta_rntrc = wa_consultas ).

        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consTransportadorReq.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_xml.
        ENDIF.

        CALL METHOD lc_webservice->zesif_webservice~consultar
          EXPORTING
            i_http                     = lc_http
            i_xml                      = lc_xml
          RECEIVING
            e_resultado                = lc_msg_adm
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING webservice.
        ENDIF.

        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consTransportadorResp.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_msg_adm.
        ENDIF.

        wa_zlest0135 = lc_webservice->ler_xml_situacao_transportador( EXPORTING i_xml = lc_msg_adm ).
        wa_zlest0135-cd_transportador = wa_consultas-cd_transportador.
        wa_zlest0135-ds_placa         = wa_consultas-ds_placa.
        wa_zlest0135-dt_atualizacao   = sy-datum.
        wa_zlest0135-hr_atualizacao   = sy-uzeit.
        APPEND wa_zlest0135 TO it_zlest0135.

      ENDLOOP.

      IF it_zlest0135 IS NOT INITIAL AND i_pesquisa_livre IS INITIAL.
        MOVE it_zlest0135[] TO e_consultas[].
        MODIFY zlest0135 FROM TABLE it_zlest0135.
        "COMMIT WORK.
      ELSE.
        MOVE it_zlest0135[] TO e_consultas[].
      ENDIF.
    ELSE.
      wa_zlest0135-ck_rntrc_ativo = abap_true.
      wa_zlest0135-ck_sem_parar   = abap_true.

      IF ( i_placa IS NOT INITIAL ).

        wa_placa-sign   = 'I'.
        wa_placa-option = 'EQ'.
        wa_placa-low    = i_placa.
        wa_placa-high   = i_placa.
        APPEND wa_placa TO it_placa.

        SELECT * INTO TABLE it_veiculos
          FROM zlest0002
         WHERE pc_veiculo IN it_placa.

        IF sy-subrc IS INITIAL.
          READ TABLE it_veiculos INTO wa_veiculos INDEX 1.
          wa_zlest0135-cd_transportador = wa_veiculos-proprietario.
          wa_zlest0135-ds_placa         = wa_veiculos-pc_veiculo.
        ENDIF.

      ENDIF.

      APPEND wa_zlest0135 TO e_consultas.
    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_atualizar_rota.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_document      TYPE REF TO if_ixml_document,

          if_node          TYPE REF TO if_ixml_node,
          if_map           TYPE REF TO if_ixml_named_node_map,
          if_attr          TYPE REF TO if_ixml_node,

          iterator         TYPE REF TO if_ixml_node_iterator,
          iterator_pracas  TYPE REF TO if_ixml_node_iterator,
          iterator_pracas2 TYPE REF TO if_ixml_node_iterator,
          tag_name         TYPE string,
          name_dom         TYPE string,
          count_dom        TYPE i,
          index_dom        TYPE i,
          prefix_dom       TYPE string,
          valor_dom        TYPE string,

          filho            TYPE REF TO if_ixml_node_list,
          if_node_filho    TYPE REF TO if_ixml_node,
          if_map_filho     TYPE REF TO if_ixml_named_node_map,
          count_dom_filho  TYPE i,
          valor_filho      TYPE string,

          filho2           TYPE REF TO if_ixml_node_list,
          if_node_filho2   TYPE REF TO if_ixml_node,
          if_map_filho2    TYPE REF TO if_ixml_named_node_map,
          count_dom_filho2 TYPE i,
          valor_filho2     TYPE string,

          wa_rotas         TYPE zlest0101,
          wa_pracas        TYPE zlest0102,
          wa_j_1bbranch    TYPE j_1bbranch,
          wa_j_1btxjur     TYPE j_1btxjur.

    CLEAR: e_rotas, e_pracas.

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
      if_node  = iterator->get_next( ).

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

                "VALOR_DOM.
              ENDDO.

              CASE tag_name.
                WHEN: 'msg'.
                  e_msg = if_node->get_value( ).
                WHEN: 'cnpjContratante'.
                  CLEAR: wa_rotas.
                  valor_dom = if_node->get_value( ).
                  SELECT SINGLE * INTO wa_j_1bbranch
                    FROM j_1bbranch
                   WHERE stcd1 EQ valor_dom.
                  IF sy-subrc IS INITIAL.
                    wa_rotas-bukrs  = wa_j_1bbranch-bukrs.
                    wa_rotas-branch = wa_j_1bbranch-branch.
                  ENDIF.
                WHEN: 'codigoRota'.
                  wa_rotas-id_rota_adm = if_node->get_value( ).
                WHEN: 'codigoCidadeOrigem'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country       = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_origem = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'codigoCidadeDestino'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country        = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_destino = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'retornoOrigem'.
                  wa_rotas-tp_rota_perc = if_node->get_value( ).
                  APPEND wa_rotas TO e_rotas.
                WHEN: 'pracasPedagio'.
                  filho           = if_node->get_children( ).
                  iterator_pracas = filho->create_iterator( ).
                  if_node_filho   = iterator_pracas->get_next( ).

                  WHILE NOT if_node_filho IS INITIAL.
                    filho2           = if_node_filho->get_children( ).
                    iterator_pracas2 = filho2->create_iterator( ).
                    if_node_filho2   = iterator_pracas2->get_next( ).

                    WHILE NOT if_node_filho2 IS INITIAL.
                      tag_name = if_node_filho2->get_name( ).
                      CASE if_node_filho2->get_type( ).
                        WHEN: if_ixml_node=>co_node_element.
                          tag_name     = if_node_filho2->get_name( ).
                          if_map_filho2 = if_node_filho2->get_attributes( ).
                          IF NOT ( if_map_filho2 IS INITIAL ).
                            CASE tag_name.
                              WHEN: 'codigoPracaAilog'.
                                valor_dom = if_node_filho2->get_value( ).
                                CLEAR: wa_pracas.
                                wa_pracas-id_rota_adm = wa_rotas-id_rota_adm.
                                MOVE valor_dom TO wa_pracas-id_praca.
                              WHEN: 'codigoPracaSemParar'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-id_praca_sem_par.
                              WHEN: 'nomePraca'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-nm_praca.
                              WHEN: 'eixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo1.
                              WHEN: 'eixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo2.
                              WHEN: 'eixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo3.
                              WHEN: 'eixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo4.
                              WHEN: 'eixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo5.
                              WHEN: 'eixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo6.
                              WHEN: 'eixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo7.
                              WHEN: 'eixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo8.
                              WHEN: 'eixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo9.
                              WHEN: 'eixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo10.
                              WHEN: 'precisaoEixo'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_precisao.
                                APPEND wa_pracas TO e_pracas.
*** Inicio - Rubenilson - 17.12.24 - US160867
                              WHEN: 'tageixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo1.
                              WHEN: 'tageixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo2.
                              WHEN: 'tageixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo3.
                              WHEN: 'tageixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo4.
                              WHEN: 'tageixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo5.
                              WHEN: 'tageixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo6.
                              WHEN: 'tageixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo7.
                              WHEN: 'tageixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo8.
                              WHEN: 'tageixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo9.
                              WHEN: 'tageixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
                            ENDCASE.
                          ENDIF.
                      ENDCASE.
                      if_node_filho2 = iterator_pracas2->get_next( ).
                    ENDWHILE.
                    if_node_filho = iterator_pracas->get_next( ).
                  ENDWHILE.
              ENDCASE.
            ENDIF.
        ENDCASE.
        if_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.

*<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
*<retornoAtualizaRota>
*    <msg>Sucesso</msg>
*    <rotas>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>523</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1002</codigoRota>
*            <codigoCidadeOrigem>2915353</codigoCidadeOrigem>
*            <codigoCidadeDestino>3550308</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*            <pracasPedagio>
*                <praca>
*                    <codigoPraca>886</codigoPraca>
*                    <nomePraca>Vitória da Conquista</nomePraca>
*                    <eixo1>340</eixo1>
*                    <eixo2>680</eixo2>
*                    <eixo3>1020</eixo3>
*                    <eixo4>1360</eixo4>
*                    <eixo5>1700</eixo5>
*                    <eixo6>2040</eixo6>
*                    <eixo7>2380</eixo7>
*                    <eixo8>2720</eixo8>
*                    <eixo9>3060</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>772</codigoPraca>
*                    <nomePraca>Itatiaiaçú</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>771</codigoPraca>
*                    <nomePraca>Carmópolis de Minas</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>770</codigoPraca>
*                    <nomePraca>Santo Antonio do Amparo</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>769</codigoPraca>
*                    <nomePraca>Carmo da Cachoeira</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>768</codigoPraca>
*                    <nomePraca>São Gonçalo do Sapucaí</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>767</codigoPraca>
*                    <nomePraca>Cambuí</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>766</codigoPraca>
*                    <nomePraca>Vargem</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>765</codigoPraca>
*                    <nomePraca>Mairiporã</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*            </pracasPedagio>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1003</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1004</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1005</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1006</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*    </rotas>
*</retornoAtualizaRota>


  ENDMETHOD.


  METHOD ler_xml_consultar_arq_cobranca.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_document      TYPE REF TO if_ixml_document,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_node          TYPE REF TO if_ixml_node,
          iterator         TYPE REF TO if_ixml_node_iterator.

    DATA: tag_name      TYPE string,
          valor_dom     TYPE string,
          filho         TYPE REF TO if_ixml_node_list,
          iterator2     TYPE REF TO if_ixml_node_iterator,
          if_node_filho TYPE REF TO if_ixml_node,
*-US 130492-08.04.2024-JT-inicio
          xml_ret       TYPE REF TO cl_xml_document,
          xml_ret_itm   TYPE REF TO cl_xml_document,
          xml_node_root TYPE REF TO if_ixml_node,
          xml_node      TYPE REF TO if_ixml_node,
          v_tamanhoi    TYPE i.
*-US 130492-08.04.2024-JT-fim

    CLEAR: r_linkarquivo, r_linkarquivo_pedagio, r_content_arquivo, r_content_arquivo_pedagio.

    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).
    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory istream = if_stream document = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.
        CASE if_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = if_node->get_name( ).

            CASE tag_name.
              WHEN 'retornoArquivoCobrancaPedagio'.
                filho         = if_node->get_children( ).
                iterator2     = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'mensagem'.
                      TRANSLATE valor_dom TO UPPER CASE.
                      e_msg = valor_dom.
*-US 130492-08.04.2024-JT-inicio
                    WHEN 'linkArquivoCobranca'.
                      r_linkarquivo = valor_dom.
                    WHEN 'linkArquivoPedagio'.
                      r_linkarquivo_pedagio = valor_dom.
*-US 130492-08.04.2024-JT-fim
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.

*-US 130492-08.04.2024-JT-inicio ----------------------------------------
      CREATE OBJECT xml_ret.

      IF r_linkarquivo IS NOT INITIAL AND r_linkarquivo(4) <> 'http'.
        CALL METHOD xml_ret->parse_string
          EXPORTING
            stream  = i_xml
          RECEIVING
            retcode = v_tamanhoi.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'arquivoCobranca'
          RECEIVING
            node = xml_node.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
          iterator = xml_node->create_iterator( ).
          xml_node = iterator->get_next( ).
          WHILE NOT xml_node IS INITIAL.
            CASE xml_node->get_type( ).
              WHEN: if_ixml_node=>co_node_element.
                tag_name = xml_node->get_name( ).
                IF tag_name EQ 'nome'.
                  r_linkarquivo = xml_node->get_value( ).
                ENDIF.
                IF tag_name EQ 'dados'.
                  r_content_arquivo = xml_node->get_value( ).
                ENDIF.
            ENDCASE.
            xml_node = iterator->get_next( ).
          ENDWHILE.
        ENDIF.
      ENDIF.

      IF r_linkarquivo_pedagio IS NOT INITIAL AND r_linkarquivo_pedagio(4) <> 'http'.
        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'arquivoPedagio'
          RECEIVING
            node = xml_node.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
          iterator = xml_node->create_iterator( ).
          xml_node = iterator->get_next( ).
          WHILE NOT xml_node IS INITIAL.
            CASE xml_node->get_type( ).
              WHEN: if_ixml_node=>co_node_element.
                tag_name = xml_node->get_name( ).
                IF tag_name EQ 'nome'.
                  r_linkarquivo_pedagio = xml_node->get_value( ).
                ENDIF.
                IF tag_name EQ 'dados'.
                  r_content_arquivo_pedagio = xml_node->get_value( ).
                ENDIF.
            ENDCASE.
            xml_node = iterator->get_next( ).
          ENDWHILE.
        ENDIF.
      ENDIF.
*-US 130492-08.04.2024-JT-fim -------------------------------------------

    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_consultar_arq_confere.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_document      TYPE REF TO if_ixml_document,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_node          TYPE REF TO if_ixml_node,
          iterator         TYPE REF TO if_ixml_node_iterator.

    DATA: tag_name      TYPE string,
          valor_dom     TYPE string,
          filho         TYPE REF TO if_ixml_node_list,
          iterator2     TYPE REF TO if_ixml_node_iterator,
          if_node_filho TYPE REF TO if_ixml_node.

    CLEAR: r_linkarquivo.

    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).
    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory istream = if_stream document = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.
        CASE if_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = if_node->get_name( ).

            CASE tag_name.
              WHEN 'respostaArquivoConferencia'.
                filho         = if_node->get_children( ).
                iterator2     = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'codigoMensagem'.
                      e_codigo = valor_dom.
                    WHEN 'mensagem'.
                      TRANSLATE valor_dom TO UPPER CASE.
                      e_msg = valor_dom.
                    WHEN 'linkArquivo'.
                      r_linkarquivo = valor_dom.
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.

    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_consultar_rota.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_document      TYPE REF TO if_ixml_document,

          if_node          TYPE REF TO if_ixml_node,
          if_map           TYPE REF TO if_ixml_named_node_map,
          if_attr          TYPE REF TO if_ixml_node,

          iterator         TYPE REF TO if_ixml_node_iterator,
          iterator_pracas  TYPE REF TO if_ixml_node_iterator,
          iterator_pracas2 TYPE REF TO if_ixml_node_iterator,
          tag_name         TYPE string,
          name_dom         TYPE string,
          count_dom        TYPE i,
          index_dom        TYPE i,
          prefix_dom       TYPE string,
          valor_dom        TYPE string,

          filho            TYPE REF TO if_ixml_node_list,
          if_node_filho    TYPE REF TO if_ixml_node,
          if_map_filho     TYPE REF TO if_ixml_named_node_map,
          count_dom_filho  TYPE i,
          valor_filho      TYPE string,

          filho2           TYPE REF TO if_ixml_node_list,
          if_node_filho2   TYPE REF TO if_ixml_node,
          if_map_filho2    TYPE REF TO if_ixml_named_node_map,
          count_dom_filho2 TYPE i,
          valor_filho2     TYPE string,

          wa_rotas         TYPE zlest0101,
          wa_pracas        TYPE zlest0102,
          wa_j_1bbranch    TYPE j_1bbranch,
*          wa_j_1btreg_city TYPE j_1btreg_city. "Incluindo nova seleção na tabela devido estar buscando mais de um valor domicio fiscal. US 109147 / AOENNING.
          wa_j_1btxjur     TYPE j_1btxjur. "Comentado devido altaração na seleção esta buscando mais de um valor domicio fiscal. US 109147 / AOENNING.
    CLEAR: e_rotas, e_pracas.

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
      if_node  = iterator->get_next( ).

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

                "VALOR_DOM.
              ENDDO.

              CASE tag_name.
                WHEN: 'msg'.
                  e_msg = if_node->get_value( ).
                WHEN: 'cnpjContratante'.
                  CLEAR: wa_rotas.
                  valor_dom = if_node->get_value( ).
                  SELECT SINGLE * INTO wa_j_1bbranch
                    FROM j_1bbranch
                   WHERE stcd1 EQ valor_dom.
                  IF sy-subrc IS INITIAL.
                    wa_rotas-bukrs  = wa_j_1bbranch-bukrs.
                    wa_rotas-branch = wa_j_1bbranch-branch.
                  ENDIF.
                WHEN: 'codigoRota'.
                  wa_rotas-id_rota_adm = if_node->get_value( ).
                WHEN: 'codigoCidadeOrigem'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  CLEAR: wa_j_1btxjur.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country       = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_origem = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'codigoCidadeDestino'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  CLEAR: wa_j_1btxjur.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country        = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_destino = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'retornoOrigem'.
                  wa_rotas-tp_rota_perc = if_node->get_value( ).
                  APPEND wa_rotas TO e_rotas.
                WHEN: 'pracasPedagio'.
                  filho           = if_node->get_children( ).
                  iterator_pracas = filho->create_iterator( ).
                  if_node_filho   = iterator_pracas->get_next( ).

                  WHILE NOT if_node_filho IS INITIAL.
                    filho2           = if_node_filho->get_children( ).
                    iterator_pracas2 = filho2->create_iterator( ).
                    if_node_filho2   = iterator_pracas2->get_next( ).

                    WHILE NOT if_node_filho2 IS INITIAL.
                      tag_name = if_node_filho2->get_name( ).
                      CASE if_node_filho2->get_type( ).
                        WHEN: if_ixml_node=>co_node_element.
                          tag_name     = if_node_filho2->get_name( ).
                          if_map_filho2 = if_node_filho2->get_attributes( ).
                          IF NOT ( if_map_filho2 IS INITIAL ).
                            CASE tag_name.
                              WHEN: 'codigoPracaAilog'.
                                valor_dom = if_node_filho2->get_value( ).
                                CLEAR: wa_pracas.
                                wa_pracas-id_rota_adm = wa_rotas-id_rota_adm.
                                MOVE valor_dom TO wa_pracas-id_praca.
                              WHEN 'codigoPracaSemParar'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-id_praca_sem_par.
                              WHEN: 'nomePraca'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-nm_praca.
                              WHEN: 'eixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo1.
                              WHEN: 'eixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo2.
                              WHEN: 'eixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo3.
                              WHEN: 'eixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo4.
                              WHEN: 'eixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo5.
                              WHEN: 'eixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo6.
                              WHEN: 'eixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo7.
                              WHEN: 'eixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo8.
                              WHEN: 'eixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo9.
                              WHEN: 'eixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo10.
                              WHEN: 'precisaoEixo'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_precisao.
                                APPEND wa_pracas TO e_pracas.
*** Inicio - Rubenilson - 17.12.24 - US160867
                              WHEN: 'tageixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo1.
                              WHEN: 'tageixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo2.
                              WHEN: 'tageixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo3.
                              WHEN: 'tageixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo4.
                              WHEN: 'tageixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo5.
                              WHEN: 'tageixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo6.
                              WHEN: 'tageixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo7.
                              WHEN: 'tageixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo8.
                              WHEN: 'tageixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo9.
                              WHEN: 'tageixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
                            ENDCASE.
                          ENDIF.
                      ENDCASE.
                      if_node_filho2 = iterator_pracas2->get_next( ).
                    ENDWHILE.
                    if_node_filho = iterator_pracas->get_next( ).
                  ENDWHILE.
              ENDCASE.
            ENDIF.
        ENDCASE.
        if_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.

*<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
*<retornoConsultaRota>
*    <msg>Sucesso</msg>
*    <cnpjContratante>77294254001670</cnpjContratante>
*    <codigoRota>1007</codigoRota>
*    <codigoCidadeOrigem>5107925</codigoCidadeOrigem>
*    <codigoCidadeDestino>5107602</codigoCidadeDestino>
*    <retornoOrigem>0</retornoOrigem>
*    <pracasPedagio>
*        <praca>
*            <codigoPraca>849</codigoPraca>
*            <nomePraca>Lucas do Rio Verde</nomePraca>
*            <eixo1>430</eixo1>
*            <eixo2>860</eixo2>
*            <eixo3>1290</eixo3>
*            <eixo4>1720</eixo4>
*            <eixo5>2150</eixo5>
*            <eixo6>2580</eixo6>
*            <eixo7>3010</eixo7>
*            <eixo8>3440</eixo8>
*            <eixo9>3870</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>850</codigoPraca>
*            <nomePraca>Nova Mutum</nomePraca>
*            <eixo1>330</eixo1>
*            <eixo2>660</eixo2>
*            <eixo3>990</eixo3>
*            <eixo4>1320</eixo4>
*            <eixo5>1650</eixo5>
*            <eixo6>1980</eixo6>
*            <eixo7>2310</eixo7>
*            <eixo8>2640</eixo8>
*            <eixo9>2970</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>851</codigoPraca>
*            <nomePraca>Diamantino</nomePraca>
*            <eixo1>410</eixo1>
*            <eixo2>820</eixo2>
*            <eixo3>1230</eixo3>
*            <eixo4>1640</eixo4>
*            <eixo5>2050</eixo5>
*            <eixo6>2460</eixo6>
*            <eixo7>2870</eixo7>
*            <eixo8>3280</eixo8>
*            <eixo9>3690</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>852</codigoPraca>
*            <nomePraca>Jangada</nomePraca>
*            <eixo1>490</eixo1>
*            <eixo2>980</eixo2>
*            <eixo3>1470</eixo3>
*            <eixo4>1960</eixo4>
*            <eixo5>2450</eixo5>
*            <eixo6>2940</eixo6>
*            <eixo7>3430</eixo7>
*            <eixo8>3920</eixo8>
*            <eixo9>4410</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>828</codigoPraca>
*            <nomePraca>Rondonópolis</nomePraca>
*            <eixo1>450</eixo1>
*            <eixo2>900</eixo2>
*            <eixo3>1350</eixo3>
*            <eixo4>1800</eixo4>
*            <eixo5>2250</eixo5>
*            <eixo6>2700</eixo6>
*            <eixo7>3150</eixo7>
*            <eixo8>3600</eixo8>
*            <eixo9>4050</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>829</codigoPraca>
*            <nomePraca>Campo Verde</nomePraca>
*            <eixo1>370</eixo1>
*            <eixo2>740</eixo2>
*            <eixo3>1110</eixo3>
*            <eixo4>1480</eixo4>
*            <eixo5>1850</eixo5>
*            <eixo6>2220</eixo6>
*            <eixo7>2590</eixo7>
*            <eixo8>2960</eixo8>
*            <eixo9>3330</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>830</codigoPraca>
*            <nomePraca>Santo Antônio de Leverger</nomePraca>
*            <eixo1>360</eixo1>
*            <eixo2>720</eixo2>
*            <eixo3>1080</eixo3>
*            <eixo4>1440</eixo4>
*            <eixo5>1800</eixo5>
*            <eixo6>2160</eixo6>
*            <eixo7>2520</eixo7>
*            <eixo8>2880</eixo8>
*            <eixo9>3240</eixo9>
*        </praca>
*    </pracasPedagio>
*</retornoConsultaRota>



  ENDMETHOD.


  METHOD ler_xml_criar_rota.

    DATA: xml_ret    TYPE REF TO cl_xml_document,
          xml_node   TYPE REF TO if_ixml_node,
          v_tamanhoi TYPE i,
          v_valor    TYPE string.

    CREATE OBJECT xml_ret.

    v_tamanhoi = xml_ret->parse_string( i_xml ).

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'msg'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      MOVE v_valor TO e_msg.
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'codigoRota'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      MOVE v_valor TO e_id_rota_adm.
    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_rotas.

    TYPES: BEGIN OF ty_rotas,
             bukrs         TYPE zlest0084-bukrs,
             branch        TYPE zlest0084-branch,
             id_rota       TYPE zlest0084-id_rota,
             cat_veiculo   TYPE zlest0084-cat_veiculo,
             dt_vigencia   TYPE zlest0084-dt_vigencia,
             munic_origem  TYPE zlest0084-munic_origem,
             munic_destino TYPE zlest0084-munic_destino,
             distancia     TYPE zlest0084-distancia,
             vlr_pedagio   TYPE zlest0084-vlr_pedagio,
             descr_rota    TYPE zlest0084-descr_rota,
           END OF ty_rotas.

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


    DATA: gt_rotas     TYPE TABLE OF ty_rotas,
          gw_rotas     TYPE ty_rotas,
          gw_zlest0084 TYPE zlest0084.

    DATA: obj_zcl_rota    TYPE REF TO zcl_rota,
          obj_zcl_rota_db TYPE REF TO zcl_rota_db.



    FIELD-SYMBOLS: <fs_rotas> TYPE ty_rotas.

    SELECT SINGLE * INTO @DATA(wa_zlest0159)
      FROM zlest0159
     WHERE ds_grupo EQ @i_grupo.

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

                gw_rotas-id_rota = valor_dom.

                IF NOT ( gw_rotas-id_rota IS INITIAL ).
                  APPEND gw_rotas TO gt_rotas.
                ENDIF.

              ENDDO.

              CASE tag_name.
                WHEN: 'categoriaVeiculo'.

                  IF NOT ( gw_rotas-id_rota IS INITIAL ).
                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-cat_veiculo = if_node->get_value( ).
                    ENDLOOP.
                    UNASSIGN <fs_rotas>.
                  ENDIF.

                WHEN: 'valorPedagio'.

                  IF NOT ( gw_rotas-id_rota IS INITIAL ).
                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-vlr_pedagio = if_node->get_value( ).
                    ENDLOOP.

                    UNASSIGN <fs_rotas>.
                  ENDIF.


                WHEN: 'percurso'.

                  IF NOT ( gw_rotas-id_rota IS INITIAL ).
                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-descr_rota = if_node->get_value( ).
                    ENDLOOP.
                    UNASSIGN <fs_rotas>.
                  ENDIF.


                WHEN: 'distanciaPercurso'.

                  IF NOT ( gw_rotas-id_rota IS INITIAL ).
                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-distancia = if_node->get_value( ).
                    ENDLOOP.
                  ENDIF.

                WHEN: 'inicioVigencia'.

                  IF NOT ( gw_rotas-id_rota IS INITIAL ).
                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-dt_vigencia = if_node->get_value( ).
                    ENDLOOP.
                    UNASSIGN <fs_rotas>.
                  ENDIF.

                WHEN: 'municipios'.

                  IF NOT ( gw_rotas-id_rota IS INITIAL ).

                    node_filho    = if_node->get_first_child( ).
                    valor_filho   = node_filho->get_value( ).

                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-munic_origem = valor_filho.
                    ENDLOOP.
                    UNASSIGN <fs_rotas>.

                    CLEAR: node_filho, valor_filho.

                    node_filho  = if_node->get_last_child( ).
                    valor_filho = node_filho->get_value( ).

                    LOOP AT gt_rotas ASSIGNING <fs_rotas> WHERE id_rota EQ gw_rotas-id_rota.
                      <fs_rotas>-munic_destino = valor_filho.
                    ENDLOOP.
                    UNASSIGN <fs_rotas>.

                    CLEAR: valor_filho.
                  ENDIF.
              ENDCASE.
            ENDIF.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.

    IF NOT ( gt_rotas[] IS INITIAL ).

      LOOP AT gt_rotas INTO gw_rotas.

        FREE: obj_zcl_rota,
              obj_zcl_rota_db.

        CREATE OBJECT: obj_zcl_rota,
                       obj_zcl_rota_db.

        SELECT SINGLE * FROM zlest0084 INTO gw_zlest0084 WHERE id_rota     EQ gw_rotas-id_rota
                                                           AND cat_veiculo EQ gw_rotas-cat_veiculo
                                                           AND bukrs       EQ gw_rotas-bukrs
                                                           AND branch      EQ gw_rotas-branch.
        CASE sy-subrc.
          WHEN: 0.

            obj_zcl_rota->set_bukrs( wa_zlest0159-bukrs ).
            obj_zcl_rota->set_branch( wa_zlest0159-branch ).
            obj_zcl_rota->set_id_rota( gw_rotas-id_rota ).
            obj_zcl_rota->set_cat_veiculo( gw_rotas-cat_veiculo ).
            obj_zcl_rota->set_dt_vigencia( gw_rotas-dt_vigencia ).
            obj_zcl_rota->set_vlr_pedagio( gw_rotas-vlr_pedagio ).
            obj_zcl_rota->set_descr_rota( gw_rotas-descr_rota ).

            "Atualizar as Informações no Banco de Dados.
            obj_zcl_rota_db->zif_rota_db~atualizar( obj_zcl_rota ).

          WHEN OTHERS.

            obj_zcl_rota->set_bukrs( wa_zlest0159-bukrs ).
            obj_zcl_rota->set_branch( wa_zlest0159-branch ).
            obj_zcl_rota->set_id_rota( gw_rotas-id_rota  ).
            obj_zcl_rota->set_cat_veiculo( gw_rotas-cat_veiculo ).
            obj_zcl_rota->set_dt_vigencia( gw_rotas-dt_vigencia ).
            obj_zcl_rota->set_munic_origem( gw_rotas-munic_origem ).
            obj_zcl_rota->set_munic_destino( gw_rotas-munic_destino ).
            obj_zcl_rota->set_distancia( gw_rotas-distancia ).
            obj_zcl_rota->set_vlr_pedagio( gw_rotas-vlr_pedagio ).
            obj_zcl_rota->set_descr_rota( gw_rotas-descr_rota ).

            "Inserir no Banco de Dados.
            obj_zcl_rota_db->zif_rota_db~inserir( obj_zcl_rota ).


        ENDCASE.
      ENDLOOP.

    ELSE.
      "RAISE EXCEPTION TYPE ZESCX_WEBSERVICE EXPORTING TEXTID = ZESCX_WEBSERVICE=>SEM_ROTA_UPDATE.
    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_situacao_transportador.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_document      TYPE REF TO if_ixml_document,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_node          TYPE REF TO if_ixml_node,
          iterator         TYPE REF TO if_ixml_node_iterator.

    DATA: tag_name      TYPE string,
          valor_dom     TYPE string,
          filho         TYPE REF TO if_ixml_node_list,
          iterator2     TYPE REF TO if_ixml_node_iterator,
          if_node_filho TYPE REF TO if_ixml_node.

    CLEAR: e_zlest0135.

    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).
    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory istream = if_stream document = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.
        CASE if_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = if_node->get_name( ).

            CASE tag_name.
              WHEN 'transportador'.
                filho         = if_node->get_children( ).
                iterator2     = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'razaoSocial'.
                      e_zlest0135-ds_razao_social = valor_dom.
                    WHEN 'tipo'.
                      TRANSLATE valor_dom TO UPPER CASE.
                      CASE valor_dom.
                        WHEN 'TAC'.
                          e_zlest0135-tp_transportador = '1'.
                        WHEN 'ETC'.
                          e_zlest0135-tp_transportador = '2'.
                        WHEN 'CTC'.
                          e_zlest0135-tp_transportador = '3'.
                      ENDCASE.
                    WHEN 'validadeRntrc'.
                      IF valor_dom IS NOT INITIAL.
                        CONCATENATE valor_dom+6(4) valor_dom+3(2) valor_dom(2) INTO e_zlest0135-dt_validade_rntrc.
                      ENDIF.
                    WHEN 'rntrcAtivo'.
                      TRANSLATE valor_dom TO UPPER CASE.
                      CASE valor_dom.
                        WHEN 'FALSE'.
                          e_zlest0135-ck_rntrc_ativo = abap_false.
                        WHEN 'TRUE'.
                          e_zlest0135-ck_rntrc_ativo = abap_true.
                      ENDCASE.
                    WHEN 'equiparadoTac'.
                      TRANSLATE valor_dom TO UPPER CASE.
                      CASE valor_dom.
                        WHEN 'FALSE'.
                          e_zlest0135-ck_etc_equiparado = abap_false.
                        WHEN 'TRUE'.
                          e_zlest0135-ck_etc_equiparado = abap_true.
                      ENDCASE.
                    WHEN 'msg'.
                      e_zlest0135-ds_msg_transportador = valor_dom.
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
              WHEN 'veiculoTransportador'.
                filho     = if_node->get_children( ).
                iterator2 = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'descricao'.
                      e_zlest0135-ds_veiculo = valor_dom.
                    WHEN 'eixo'.
                      e_zlest0135-qt_eixos = valor_dom.
                    WHEN 'proprietario'.
                      e_zlest0135-ds_proprietario = valor_dom.
                    WHEN 'tag'.
                      e_zlest0135-nr_tag = valor_dom.
                    WHEN 'msg'.
                      e_zlest0135-ds_msg_veiculo = valor_dom.
                      TRANSLATE valor_dom TO UPPER CASE.
                      IF valor_dom = '[WS SEMPARAR] - SUCESSO'.
                        e_zlest0135-ck_sem_parar = abap_true.
                      ELSE.
                        e_zlest0135-ck_sem_parar = abap_false.
                      ENDIF.
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.

    ENDIF.

  ENDMETHOD.


  METHOD processar_arquivo_cobranca.

    TYPES: BEGIN OF ty_lines,
              line(500),
            END OF ty_lines.

    TYPES BEGIN OF ty_valores.
    TYPES: cd_ciot   TYPE zciot,
            docnum    TYPE j_1bdocnum,
            vl_perda  TYPE kwert,
            vl_quebra TYPE kwert.
    TYPES END OF ty_valores.

    DATA: it_lines_arq       TYPE ZESDE_linha_txt_1000_t,
           it_lines_arq_ped   TYPE ZESDE_linha_txt_1000_t,
           it_lines           TYPE STANDARD TABLE OF ty_lines,
           wa_0062            TYPE zlest0062,
           it_0062            TYPE TABLE OF zlest0062,
           arquivo            TYPE REF TO zescl_tip_frete_aq_cobranca,
           "LC_NR_LOTE_ADM    TYPE ZPFE_NR_LOTE_ADM,
           it_reg_cabecalho   TYPE TABLE OF zpfe_lote,
           it_reg_itens       TYPE TABLE OF zpfe_lote_item,
           it_reg_itens_ad    TYPE TABLE OF zpfe_lote_item,
           it_reg_itens_aux   TYPE TABLE OF zpfe_lote_item,
           it_arquivo	        TYPE TABLE OF zpfe_arquivo,
           vg_lote            TYPE  i,
           vg_nm_lote_item    TYPE zpfe_numero_lote,
           vg_tipcontabil     TYPE ztipcontabil,
           vg_tipcontabil_itm TYPE ztipcontabil,
           st_lote            TYPE zpfe_numero_lote,
           st_lote_aux        TYPE zpfe_numero_lote,
           wa_valores         TYPE ty_valores,
           it_valores         TYPE TABLE OF ty_valores,
           vg_diferenca       TYPE j_1bnetqty,
           vg_toleravel       TYPE j_1bnetqty.

    DEFINE insert_line_log.

       CLEAR: wa_0062.
       wa_0062-nr_lote_adm = &1.
       wa_0062-nr_lote     = &2.
       wa_0062-chvid       = &3.
       wa_0062-nucontrato  = &4.
       wa_0062-id_tipo     = &5.
       wa_0062-msg_erro    = &6.
       wa_0062-linha       = &7.
       APPEND wa_0062 TO it_0062.

       IF &8 EQ 'E'.
         MODIFY zlest0062 FROM wa_0062.
       ENDIF.

    END-OF-DEFINITION.

    CLEAR: it_reg_cabecalho, it_reg_cabecalho[], it_reg_itens, it_reg_itens[], e_lotes[].

    DATA: url TYPE string.
    DATA: t_url TYPE string.
    DATA: client TYPE REF TO if_http_client.
    DATA: c_xml TYPE string.
    DATA: c_separador TYPE c LENGTH 1.

    IF i_filename IS NOT INITIAL.
*-US 130492-08.04.2024-JT-inicio
      IF i_filename(4) = 'http'.
        url = i_filename.

        cl_http_client=>create_by_url(
           EXPORTING
             url                = url
*           PROXY_HOST         =
*           PROXY_SERVICE      =
*           SSL_ID             =
*           SAP_USERNAME       =
*           SAP_CLIENT         =
           IMPORTING
             client             = client
           EXCEPTIONS
             argument_not_found = 1
             plugin_not_active  = 2
             internal_error     = 3
             OTHERS             = 4 ).

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zescx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
        ENDIF.

        client->send(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             http_invalid_timeout       = 4
             OTHERS                     = 5
         ).

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zescx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
        ENDIF.

        client->receive(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             OTHERS                     = 4
         ).

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zescx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
        ENDIF.

        c_xml = client->response->get_cdata( ).
        client->close( ).
      ELSE.
        c_xml = zescl_string=>base64_to_string( i_texto = CONV #( i_content_filename ) ).
      ENDIF.
*-US 130492-08.04.2024-JT-fim

      SPLIT c_xml AT cl_abap_char_utilities=>newline INTO: TABLE it_lines_arq.
    ENDIF.

    IF i_filename_pedagio IS NOT INITIAL.
*-US 130492-08.04.2024-JT-inicio
      IF i_filename_pedagio(4) = 'http'.
        url = i_filename_pedagio.

        cl_http_client=>create_by_url(
           EXPORTING
             url                = url
*           PROXY_HOST         =
*           PROXY_SERVICE      =
*           SSL_ID             =
*           SAP_USERNAME       =
*           SAP_CLIENT         =
           IMPORTING
             client             = client
           EXCEPTIONS
             argument_not_found = 1
             plugin_not_active  = 2
             internal_error     = 3
             OTHERS             = 4 ).

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zescx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
        ENDIF.

        client->send(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             http_invalid_timeout       = 4
             OTHERS                     = 5
         ).

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zescx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
        ENDIF.

        client->receive(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             OTHERS                     = 4
         ).

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zescx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
        ENDIF.

        c_xml = client->response->get_cdata( ).
        client->close( ).
      ELSE.
        c_xml = zescl_string=>base64_to_string( i_texto = CONV #( i_content_filename_pedagio ) ).
      ENDIF.
*-US 130492-08.04.2024-JT-fim

      SPLIT c_xml AT cl_abap_char_utilities=>newline INTO: TABLE it_lines_arq_ped.
    ENDIF.

    LOOP AT it_lines_arq INTO DATA(wa_line).
      APPEND wa_line TO it_arquivo.
    ENDLOOP.

    vg_lote = 0.

    CALL FUNCTION 'Z_PFE_ARQUIVO_REGISTOS'
       TABLES
         t_arquivo           = it_arquivo
         t_reg_cabecalho     = it_reg_cabecalho
         t_reg_itens         = it_reg_itens
       CHANGING
         vg_lote             = vg_lote
       EXCEPTIONS
         nao_administradora  = 1
         nao_local_negocio   = 2
         nao_chave_historico = 3
         nao_chave_hist_info = 4
         nao_ciot            = 5
         nao_ciot_info       = 6
         OTHERS              = 7.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zescx_erro_arquivo
         EXPORTING
           textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
           msgid  = sy-msgid
           msgno  = sy-msgno
           msgty  = 'E'
           msgv1  = sy-msgv1
           msgv2  = sy-msgv2
           msgv3  = sy-msgv3
           msgv4  = sy-msgv4.
    ENDIF.

    CHECK it_reg_itens IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_zlest0025)
       FROM zlest0025.

    MOVE it_reg_itens[] TO it_reg_itens_ad[].
    DELETE it_reg_itens    WHERE chvid EQ '1'.
    DELETE it_reg_itens_ad WHERE chvid NE '1'.
    MOVE it_reg_itens[] TO it_reg_itens_aux[].

    SORT it_reg_cabecalho BY nr_lote_adm.

    DATA(it_reg_copia) = it_reg_cabecalho[].
    SORT it_reg_copia BY nr_lote_adm.
    DELETE ADJACENT DUPLICATES FROM it_reg_copia COMPARING nr_lote_adm.
    LOOP AT it_reg_copia INTO DATA(wa_reg_copia).
      SELECT SINGLE * INTO @DATA(wa_zpfe_lote) FROM zpfe_lote WHERE nr_lote_adm EQ @wa_reg_copia-nr_lote_adm.
      IF sy-subrc IS INITIAL.
        DELETE it_reg_cabecalho WHERE nr_lote_adm = wa_reg_copia-nr_lote_adm .
      ENDIF.
    ENDLOOP.


     "Adiantamento
    vg_nm_lote_item = 1.
    LOOP AT it_reg_cabecalho INTO DATA(wa_reg_cabecalho).

      READ TABLE it_arquivo INDEX 1 INTO DATA(wa_arquivo).
      CLEAR: vg_tipcontabil.

      READ TABLE it_reg_itens_ad WITH KEY nm_lote = wa_reg_cabecalho-nm_lote TRANSPORTING NO FIELDS.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      wa_reg_cabecalho-vl_total_lote = 0.
      wa_reg_cabecalho-vl_confi_lote = 0.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
         EXPORTING
           p_dt_posicao  = wa_reg_cabecalho-dt_posicao
         IMPORTING
           p_tipcontabil = vg_tipcontabil.

      CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
           nr_range_nr             = '01'
           object                  = 'ZPFELOTE'
         IMPORTING
           number                  = st_lote
         EXCEPTIONS
           interval_not_found      = 1
           number_range_not_intern = 2
           object_not_found        = 3
           quantity_is_0           = 4
           quantity_is_not_1       = 5
           interval_overflow       = 6
           buffer_overflow         = 7
           OTHERS                  = 8.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zescx_erro_arquivo
           EXPORTING
             textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
             msgid  = sy-msgid
             msgno  = sy-msgno
             msgty  = 'E'
             msgv1  = sy-msgv1
             msgv2  = sy-msgv2
             msgv3  = sy-msgv3
             msgv4  = sy-msgv4.
      ENDIF.

      st_lote_aux = wa_reg_cabecalho-nm_lote.

      wa_reg_cabecalho-status  = 'I'.
      wa_reg_cabecalho-nm_lote = st_lote.
      IF it_reg_itens_ad[] IS NOT INITIAL.

        SELECT *
           FROM zpfe_lote_item
           INTO TABLE it_reg_itens_aux
            FOR ALL ENTRIES IN it_reg_itens_ad
            WHERE nucontrato EQ it_reg_itens_ad-nucontrato
              AND chvid      EQ it_reg_itens_ad-chvid.

        SELECT  *
           FROM zcte_ciot
           INTO TABLE @DATA(it_zcte_ciot)
            FOR ALL ENTRIES IN @it_reg_itens_ad
            WHERE nucontrato EQ @it_reg_itens_ad-nucontrato.

        IF sy-subrc IS INITIAL.
          SELECT  *
             FROM zcte_identifica
             INTO TABLE @DATA(it_zcte_identifica)
             FOR ALL ENTRIES IN @it_zcte_ciot
              WHERE docnum EQ @it_zcte_ciot-docnum.
        ENDIF.
        SORT: it_reg_itens_aux BY nucontrato chvid.
        SORT: it_zcte_ciot BY nucontrato.
        SORT: it_zcte_identifica BY docnum.
      ENDIF.

      LOOP AT it_reg_itens_ad INTO DATA(wa_reg_itens) WHERE nm_lote EQ st_lote_aux.
        DATA(lc_tabix) = sy-tabix.
        ADD 1 TO lc_tabix.
        READ TABLE it_arquivo INDEX lc_tabix INTO wa_arquivo.

        READ TABLE it_reg_itens_aux INTO DATA(wa_reg_itens_aux)
         WITH KEY nucontrato = wa_reg_itens-nucontrato chvid = wa_reg_itens-chvid BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de existência de chave já importada'
                           wa_arquivo-linha
                           'E'.

           "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        wa_reg_itens-nm_lote      = st_lote.
        wa_reg_itens-status       = 'I'.
        wa_reg_itens-nm_lote_item = vg_nm_lote_item.

*        SELECT SINGLE * INTO WA_ZCTE_CIOT
*          FROM ZCTE_CIOT
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.
        READ TABLE it_zcte_ciot INTO DATA(wa_zcte_ciot) WITH KEY nucontrato = wa_reg_itens-nucontrato BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).
          IF wa_reg_itens-vl_transacao NE wa_zcte_ciot-vlr_adiantamento.
            insert_line_log wa_reg_cabecalho-nr_lote_adm
                             space
                             wa_reg_itens-chvid
                             wa_reg_itens-nucontrato
                             'F'
                             'Erro no adiantamento, valor da transação diferente do valor da cte'
                             wa_arquivo-linha
                             'E'.
            CONTINUE.
          ENDIF.

          READ TABLE it_zcte_identifica INTO DATA(wa_zcte_identifica) WITH KEY docnum = wa_zcte_ciot-docnum BINARY SEARCH.

          wa_reg_itens-tp_plano_administradora = wa_zcte_ciot-tp_plano_administradora.
          wa_reg_itens-cd_ciot                 = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot                 = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum                  = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum                   = wa_zcte_ciot-tknum.
          wa_reg_itens-ctenum                  = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie                = wa_zcte_identifica-serie.
          wa_reg_cabecalho-vl_total_lote       = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

          wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
          wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
          wa_reg_itens-ck_conferido    = ''.
          wa_reg_itens-ds_usuario_conf = sy-uname.

          wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_transacao.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_reg_itens-nm_lote_item
             IMPORTING
               output = wa_reg_itens-nm_lote_item.

          CASE wa_reg_itens-tp_plano_administradora.
            WHEN zescl_ciot=>st_tp_plano_pre_pago.
              vg_tipcontabil_itm = 'FP'.
            WHEN OTHERS.
              vg_tipcontabil_itm = vg_tipcontabil.
          ENDCASE.

          MODIFY zpfe_lote_item FROM wa_reg_itens.
          vg_nm_lote_item = vg_nm_lote_item + 1.

          insert_line_log wa_reg_itens-nr_lote_adm
                           wa_reg_itens-nm_lote
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'A linha do arquivo foi importado com sucesso'
                           space
                           'S'.
        ELSE.
          insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de Contrato não encontrado'
                           wa_arquivo-linha
                           'E'.
           "Erro de Contrato não encontrado

        ENDIF.
      ENDLOOP.

      wa_reg_cabecalho-tplote = 'A'.

      MODIFY zpfe_lote FROM wa_reg_cabecalho.
      APPEND wa_reg_cabecalho-nm_lote TO e_lotes.

      insert_line_log wa_reg_cabecalho-nr_lote_adm
                       wa_reg_cabecalho-nm_lote
                       space
                       space
                       'F'
                       'A linha do cabeçalho do arquivo foi importado com sucesso'
                       space
                       'S'.

    ENDLOOP.

    IF it_reg_itens[] IS NOT INITIAL.
      SELECT * INTO TABLE it_reg_itens_aux
         FROM zpfe_lote_item
          FOR ALL ENTRIES IN it_reg_itens
        WHERE nucontrato EQ it_reg_itens-nucontrato
          AND chvid      EQ it_reg_itens-chvid.

      SELECT * INTO TABLE it_zcte_ciot
         FROM zcte_ciot
          FOR ALL ENTRIES IN it_reg_itens
        WHERE nucontrato EQ it_reg_itens-nucontrato.

      IF sy-subrc IS INITIAL.
        SELECT * INTO TABLE it_zcte_identifica
           FROM zcte_identifica
            FOR ALL ENTRIES IN it_zcte_ciot
          WHERE docnum EQ it_zcte_ciot-docnum.
      ENDIF.

      SORT: it_reg_itens_aux BY nucontrato chvid.
      SORT: it_zcte_ciot BY nucontrato.
      SORT: it_zcte_identifica BY docnum.
    ENDIF.

     "Resto de Chaves
    vg_nm_lote_item = 1.
    LOOP AT it_reg_cabecalho INTO wa_reg_cabecalho.

      CLEAR: vg_tipcontabil.

      READ TABLE it_reg_itens WITH KEY nm_lote = wa_reg_cabecalho-nm_lote TRANSPORTING NO FIELDS.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      wa_reg_cabecalho-vl_total_lote = 0.
      wa_reg_cabecalho-vl_confi_lote = 0.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
         EXPORTING
           p_dt_posicao  = wa_reg_cabecalho-dt_posicao
         IMPORTING
           p_tipcontabil = vg_tipcontabil.

      CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
           nr_range_nr             = '01'
           object                  = 'ZPFELOTE'
         IMPORTING
           number                  = st_lote
         EXCEPTIONS
           interval_not_found      = 1
           number_range_not_intern = 2
           object_not_found        = 3
           quantity_is_0           = 4
           quantity_is_not_1       = 5
           interval_overflow       = 6
           buffer_overflow         = 7
           OTHERS                  = 8.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zescx_erro_arquivo
           EXPORTING
             textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
             msgid  = sy-msgid
             msgno  = sy-msgno
             msgty  = 'E'
             msgv1  = sy-msgv1
             msgv2  = sy-msgv2
             msgv3  = sy-msgv3
             msgv4  = sy-msgv4.
      ENDIF.

      st_lote_aux = wa_reg_cabecalho-nm_lote.

      wa_reg_cabecalho-status  = 'I'.
      wa_reg_cabecalho-nm_lote = st_lote.

       "Verificações de Saldo de Frete
      LOOP AT it_reg_itens INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux AND chvid EQ '2'.

        CASE wa_reg_itens-tp_plano_administradora.
          WHEN zescl_ciot=>st_tp_plano_pre_pago.
            vg_tipcontabil_itm = 'FP'.
          WHEN OTHERS.
            vg_tipcontabil_itm = vg_tipcontabil.
        ENDCASE.

        lc_tabix = sy-tabix.
        ADD 1 TO lc_tabix.
        READ TABLE it_arquivo INDEX lc_tabix INTO wa_arquivo.
        READ TABLE it_zlest0025 WITH KEY chvid = wa_reg_itens-chvid INTO DATA(wa_zlest0025).
        IF ( wa_reg_itens-chvid IS INITIAL ) OR ( NOT sy-subrc IS INITIAL ).
          CONTINUE.
        ENDIF.

        READ TABLE it_reg_itens_aux INTO wa_reg_itens_aux
         WITH KEY nucontrato = wa_reg_itens-nucontrato chvid = wa_reg_itens-chvid BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de existência de chave já importada'
                           wa_arquivo-linha
                           'E'.
           "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        wa_reg_itens-nm_lote      = st_lote.
        wa_reg_itens-status       = 'I'.
        wa_reg_itens-nm_lote_item = vg_nm_lote_item.

        READ TABLE it_zcte_ciot INTO wa_zcte_ciot
         WITH KEY nucontrato = wa_reg_itens-nucontrato BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

          READ TABLE it_zcte_identifica INTO wa_zcte_identifica WITH KEY docnum = wa_zcte_ciot-docnum BINARY SEARCH.

          wa_valores-cd_ciot   = wa_zcte_ciot-cd_ciot.
          wa_valores-docnum    = wa_zcte_ciot-docnum.
          wa_valores-vl_quebra = 0.
          wa_valores-vl_perda  = 0.

          wa_reg_itens-cd_ciot     = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot     = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum       = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum       = wa_zcte_ciot-tknum.
          wa_reg_itens-peso_origem = wa_zcte_ciot-quantidade.
          wa_reg_itens-ctenum      = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie    = wa_zcte_identifica-serie.
          wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

          IF vg_tipcontabil_itm EQ 'FC'.
            wa_reg_itens-peso_chegada = wa_reg_itens-peso_importado.
            wa_zcte_ciot-vlr_frete = wa_zcte_ciot-vlr_frete - wa_zcte_ciot-vlr_adiantamento - wa_zcte_ciot-vlr_seguro - wa_zcte_ciot-vlr_impostos.

            IF wa_reg_itens-peso_chegada GT wa_reg_itens-peso_origem.
              wa_reg_itens-peso_chegada = wa_reg_itens-peso_origem.
            ENDIF.
            wa_reg_itens-vl_pago_lote = wa_zcte_ciot-vlr_frete.

            IF ( wa_reg_itens-peso_chegada LT wa_reg_itens-peso_origem ) OR
                ( wa_reg_itens-vl_transacao GT wa_zcte_ciot-vlr_frete ).
              vg_diferenca = wa_reg_itens-peso_origem - wa_reg_itens-peso_chegada.

               "Valor de Quebra
              wa_valores-vl_quebra  = ( vg_diferenca * ( wa_zcte_ciot-vlr_unit_frete / 1000 ) ).
               "Valor da Perda
              vg_toleravel          = wa_reg_itens-peso_origem * ( wa_zcte_ciot-perc_tolerancia / 100 ).
              IF vg_diferenca GT vg_toleravel.
                wa_valores-vl_perda = ( ( vg_diferenca - vg_toleravel ) * wa_zcte_ciot-vlr_unit_merc  ).
              ENDIF.
            ENDIF.
          ELSEIF vg_tipcontabil_itm EQ 'FS' OR vg_tipcontabil_itm EQ 'FP'.
            wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
            wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
            wa_reg_itens-peso_chegada    = wa_reg_itens-peso_importado.
            wa_reg_itens-ck_conferido    = 'X'.
            wa_reg_itens-ds_usuario_conf = sy-uname.
          ENDIF.

          APPEND wa_valores TO it_valores.
          IF wa_zlest0025-naturezachvid EQ 'S'.
            wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_pago_lote * ( -1 ).
          ENDIF.

          wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_reg_itens-nm_lote_item
             IMPORTING
               output = wa_reg_itens-nm_lote_item.

          MODIFY zpfe_lote_item FROM wa_reg_itens.
          vg_nm_lote_item = vg_nm_lote_item + 1.
          insert_line_log wa_reg_itens-nr_lote_adm
                           wa_reg_itens-nm_lote
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'A linha do arquivo foi importado com sucesso'
                           space
                           'S'.
        ELSE.
          insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de Contrato não encontrado'
                           wa_arquivo-linha
                           'E'.
        ENDIF.
      ENDLOOP.

      LOOP AT it_reg_itens INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux AND chvid NE '2'.
        lc_tabix = sy-tabix.
        ADD 1 TO lc_tabix.
        READ TABLE it_arquivo INDEX lc_tabix INTO wa_arquivo.

        READ TABLE it_zlest0025 WITH KEY chvid = wa_reg_itens-chvid INTO wa_zlest0025.
        IF ( wa_reg_itens-chvid IS INITIAL ) OR ( NOT sy-subrc IS INITIAL ).
          CONTINUE.
        ENDIF.

        READ TABLE it_reg_itens_aux INTO wa_reg_itens_aux
         WITH KEY nucontrato = wa_reg_itens-nucontrato chvid = wa_reg_itens-chvid BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de existência de chave já importada'
                           wa_arquivo-linha
                           'E'.
           "Erro de existência de chave já importada
          CONTINUE.
        ENDIF.

        wa_reg_itens-nm_lote      = st_lote.
        wa_reg_itens-status       = 'I'.
        wa_reg_itens-nm_lote_item = vg_nm_lote_item.

        READ TABLE it_zcte_ciot INTO wa_zcte_ciot WITH KEY nucontrato = wa_reg_itens-nucontrato BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

          READ TABLE it_zcte_identifica INTO wa_zcte_identifica WITH KEY docnum = wa_zcte_ciot-docnum BINARY SEARCH.
          wa_reg_itens-cd_ciot     = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot     = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum      = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum       = wa_zcte_ciot-tknum.
          wa_reg_itens-peso_origem = wa_zcte_ciot-quantidade.
          wa_reg_itens-ctenum      = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie    = wa_zcte_identifica-serie.
          wa_reg_itens-tp_plano_administradora = wa_zcte_ciot-tp_plano_administradora.

          IF wa_zlest0025-naturezachvid EQ 'S'.
            wa_reg_itens-vl_transacao = wa_reg_itens-vl_transacao * ( -1 ).
          ENDIF.
          wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

          CASE wa_reg_itens-tp_plano_administradora.
            WHEN zescl_ciot=>st_tp_plano_pre_pago.
              vg_tipcontabil_itm = 'FP'.
            WHEN OTHERS.
              vg_tipcontabil_itm = vg_tipcontabil.
          ENDCASE.

          IF vg_tipcontabil_itm EQ 'FC'.
            wa_reg_itens-vl_diferenca  = 0.

            CASE wa_reg_itens-chvid.
              WHEN '30'.
                READ TABLE it_valores INTO wa_valores
                 WITH KEY cd_ciot = wa_zcte_ciot-cd_ciot
                          docnum  = wa_zcte_ciot-docnum.
                IF sy-subrc IS INITIAL.
                  DATA(vg_tabix) = sy-tabix.
                  wa_reg_itens-vl_pago_lote = wa_valores-vl_quebra * -1.
                  wa_valores-vl_quebra      = 0.
                  MODIFY it_valores FROM wa_valores INDEX vg_tabix TRANSPORTING vl_quebra.
                ENDIF.
                 "Quebra
              WHEN '31'.
                 "Perda
                READ TABLE it_valores INTO wa_valores
                 WITH KEY cd_ciot = wa_zcte_ciot-cd_ciot
                          docnum  = wa_zcte_ciot-docnum.

                IF sy-subrc IS INITIAL.
                  vg_tabix = sy-tabix.
                  wa_reg_itens-vl_pago_lote = wa_valores-vl_perda * -1.
                  wa_valores-vl_perda       = 0.
                  MODIFY it_valores FROM wa_valores INDEX vg_tabix TRANSPORTING vl_perda.
                ENDIF.
              WHEN OTHERS.
                wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_transacao.
            ENDCASE.

          ELSEIF vg_tipcontabil_itm EQ 'FS' OR vg_tipcontabil_itm EQ 'FP'.
            wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
            wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
            wa_reg_itens-ck_conferido    = 'X'.
            wa_reg_itens-ds_usuario_conf = sy-uname.
          ENDIF.

          wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_reg_itens-nm_lote_item
             IMPORTING
               output = wa_reg_itens-nm_lote_item.

          MODIFY zpfe_lote_item FROM wa_reg_itens.
          vg_nm_lote_item = vg_nm_lote_item + 1.
          insert_line_log wa_reg_itens-nr_lote_adm
                           wa_reg_itens-nm_lote
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'A linha do arquivo foi importado com sucesso'
                           space
                           'S'.
        ELSE.
          insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de Contrato não encontrado'
                           wa_arquivo-linha
                           'E'.
           "Erro de Contrato não encontrado
        ENDIF.
      ENDLOOP.

      IF vg_tipcontabil EQ 'FC'.
        IF it_valores[] IS NOT INITIAL.
          SELECT *
             FROM zcte_ciot
             INTO TABLE it_zcte_ciot
              FOR ALL ENTRIES IN it_valores
              WHERE cd_ciot EQ it_valores-cd_ciot.

          SELECT *
             FROM zcte_identifica
             INTO TABLE it_zcte_identifica
             FOR ALL ENTRIES IN it_valores
              WHERE docnum EQ it_valores-docnum.

          SORT it_zcte_ciot BY cd_ciot.
          SORT it_zcte_identifica BY docnum.
        ENDIF.

        LOOP AT it_valores INTO wa_valores WHERE ( vl_quebra GT 0 OR vl_perda GT 0 ) .

          wa_reg_itens-nm_lote = st_lote.
          wa_reg_itens-status  = 'I'.

          READ TABLE it_zcte_ciot INTO wa_zcte_ciot WITH KEY cd_ciot = wa_valores-cd_ciot BINARY SEARCH.

          READ TABLE it_zcte_identifica INTO wa_zcte_identifica WITH KEY docnum = wa_valores-docnum BINARY SEARCH.

          wa_reg_itens-cd_ciot       = wa_zcte_ciot-cd_ciot.
          wa_reg_itens-nr_ciot       = wa_zcte_ciot-nr_ciot.
          wa_reg_itens-docnum         = wa_zcte_ciot-docnum.
          wa_reg_itens-tknum         = wa_zcte_ciot-tknum.
          wa_reg_itens-ctenum        = wa_zcte_identifica-nct.
          wa_reg_itens-cteserie      = wa_zcte_identifica-serie.

          wa_reg_itens-vl_pago_lote  = 0.
          wa_reg_itens-vl_transacao  = 0.
          wa_reg_itens-vl_diferenca  = 0.

          IF wa_valores-vl_quebra GT 0.
            READ TABLE it_zlest0025 WITH KEY chvid = '30' INTO wa_zlest0025.
            wa_reg_itens-vl_pago_lote = wa_valores-vl_quebra * ( -1 ).
            wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.
            wa_reg_itens-chvid = '30'.
            wa_reg_itens-nm_lote_item = vg_nm_lote_item.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = wa_reg_itens-nm_lote_item
               IMPORTING
                 output = wa_reg_itens-nm_lote_item.

            MODIFY zpfe_lote_item FROM wa_reg_itens.
            vg_nm_lote_item = vg_nm_lote_item + 1.
          ENDIF.
          IF wa_valores-vl_perda GT 0.
            READ TABLE it_zlest0025 WITH KEY chvid = '31' INTO wa_zlest0025.
            wa_reg_itens-vl_pago_lote = wa_valores-vl_perda * ( -1 ).
            wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.
            wa_reg_itens-chvid = '31'.
            wa_reg_itens-nm_lote_item = vg_nm_lote_item.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = wa_reg_itens-nm_lote_item
               IMPORTING
                 output = wa_reg_itens-nm_lote_item.

            MODIFY zpfe_lote_item FROM wa_reg_itens.
            vg_nm_lote_item = vg_nm_lote_item + 1.
            insert_line_log wa_reg_itens-nr_lote_adm
                             wa_reg_itens-nm_lote
                             wa_reg_itens-chvid
                             wa_reg_itens-nucontrato
                             'F'
                             'A linha do arquivo foi importado com sucesso'
                             space
                             'S'.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CLEAR: it_valores[].

      wa_reg_cabecalho-tplote = 'S'.
      MODIFY zpfe_lote FROM wa_reg_cabecalho.
      APPEND wa_reg_cabecalho-nm_lote TO e_lotes.

      insert_line_log wa_reg_cabecalho-nr_lote_adm
                       wa_reg_cabecalho-nm_lote
                       space
                       space
                       'F'
                       'A linha do cabeçalho do arquivo foi importado com sucesso'
                       space
                       'S'.
    ENDLOOP.

    COMMIT WORK AND WAIT.

    LOOP AT it_reg_cabecalho INTO wa_reg_cabecalho.
      CREATE OBJECT arquivo.
      arquivo->set_bukrs( i_bukrs = i_bukrs ).
      arquivo->set_branch( i_branch = i_branch  ).
      arquivo->set_ds_filename( i_ds_filename = i_filename ).
      arquivo->set_ds_filename_ped( i_ds_filename = i_filename_pedagio ).
      arquivo->set_corpo_arquivo( i_corpo_arquivo = it_lines_arq ).
      arquivo->set_corpo_arquivo_ped( i_corpo_arquivo = it_lines_arq_ped ).
      arquivo->set_nr_lote_adm( i_nr_lote_adm = wa_reg_cabecalho-nr_lote_adm ).
      arquivo->zesif_cadastro~gravar_registro( ).
      CLEAR: arquivo.
    ENDLOOP.

  ENDMETHOD.


  METHOD processar_arquivo_conferencia.

    TYPES: BEGIN OF ty_lines,
              line(500),
            END OF ty_lines.

    DATA: it_lines_arq        TYPE ZESDE_linha_txt_1000_t,
           it_lines            TYPE STANDARD TABLE OF ty_lines,
           wa_zpfe_lote_item   TYPE zpfe_lote_item,
           wa_zpfe_lote_item_c TYPE zpfe_lote_item,
           it_zpfe_lote_item   TYPE TABLE OF zpfe_lote_item,
           it_zpfe_lote_item_c TYPE TABLE OF zpfe_lote_item,
           wa_chvid_adm(10),
           wa_0062             TYPE zlest0062,
           it_0062             TYPE TABLE OF zlest0062,
           lc_sucesso          TYPE c LENGTH 1,
           arquivo             TYPE REF TO zcl_tip_frete_aq_conf,
           lc_nr_lote_adm	     TYPE zpfe_nr_lote_adm.

    DEFINE insert_line_log.

       CLEAR: WA_0062.
       WA_0062-NR_LOTE_ADM = &1.
       WA_0062-NR_LOTE     = &2.
       WA_0062-CHVID       = &3.
       WA_0062-NUCONTRATO  = &4.
       WA_0062-ID_TIPO     = &5.
       WA_0062-MSG_ERRO    = &6.
       WA_0062-LINHA       = &7.
       APPEND WA_0062 TO IT_0062.

       IF &8 EQ 'E'.
         MODIFY ZLEST0062 FROM WA_0062.
       ENDIF.

    END-OF-DEFINITION.

    CLEAR: it_zpfe_lote_item_c, it_zpfe_lote_item, it_zpfe_lote_item_c[], it_zpfe_lote_item[].

    DATA: url TYPE string.
    DATA: t_url TYPE string.
    DATA: client TYPE REF TO if_http_client.
    DATA: c_xml TYPE string.
    DATA: c_separador TYPE c LENGTH 1.

    url = i_filename.

    cl_http_client=>create_by_url(
       EXPORTING
         url                = url
*           PROXY_HOST         =
*           PROXY_SERVICE      =
*           SSL_ID             =
*           SAP_USERNAME       =
*           SAP_CLIENT         =
       IMPORTING
         client             = client
       EXCEPTIONS
         argument_not_found = 1
         plugin_not_active  = 2
         internal_error     = 3
         OTHERS             = 4 ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_erro_arquivo
         EXPORTING
           textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
           msgid  = sy-msgid
           msgno  = sy-msgno
           msgty  = 'E'
           msgv1  = sy-msgv1
           msgv2  = sy-msgv2
           msgv3  = sy-msgv3
           msgv4  = sy-msgv4.
    ENDIF.

    client->send(
       EXCEPTIONS
         http_communication_failure = 1
         http_invalid_state         = 2
         http_processing_failed     = 3
         http_invalid_timeout       = 4
         OTHERS                     = 5
     ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_erro_arquivo
         EXPORTING
           textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
           msgid  = sy-msgid
           msgno  = sy-msgno
           msgty  = 'E'
           msgv1  = sy-msgv1
           msgv2  = sy-msgv2
           msgv3  = sy-msgv3
           msgv4  = sy-msgv4.
    ENDIF.

    client->receive(
       EXCEPTIONS
         http_communication_failure = 1
         http_invalid_state         = 2
         http_processing_failed     = 3
         OTHERS                     = 4
     ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_erro_arquivo
         EXPORTING
           textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
           msgid  = sy-msgid
           msgno  = sy-msgno
           msgty  = 'E'
           msgv1  = sy-msgv1
           msgv2  = sy-msgv2
           msgv3  = sy-msgv3
           msgv4  = sy-msgv4.
    ENDIF.

    c_xml = client->response->get_cdata( ).

    SPLIT c_xml AT cl_abap_char_utilities=>newline INTO: TABLE it_lines_arq.

*     CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
*       EXPORTING
*         I_STRING         = C_XML
*         I_TABLINE_LENGTH = 1000
*       TABLES
*         ET_TABLE         = IT_LINES_ARQ.

    LOOP AT it_lines_arq INTO DATA(wa_lines_arq).
      CLEAR: wa_zpfe_lote_item,
              wa_zpfe_lote_item_c.

      SPLIT wa_lines_arq AT '|' INTO TABLE it_lines.
      READ TABLE it_lines INDEX 1 INTO DATA(wa_lines).

      CASE wa_lines-line.
        WHEN '1'.

          READ TABLE it_lines INDEX 4 INTO wa_lines.
          CONDENSE wa_lines-line NO-GAPS.

          CONCATENATE wa_lines-line+4(4) wa_lines-line+2(2) wa_lines-line(2) INTO DATA(wl_data).
          wa_zpfe_lote_item-dt_conf_adm = wl_data.

          READ TABLE it_lines INDEX 7 INTO wa_lines.

          CONDENSE wa_lines-line NO-GAPS.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_lines-line
             IMPORTING
               output = wa_zpfe_lote_item-nr_lote_adm.

          lc_nr_lote_adm = wa_zpfe_lote_item-nr_lote_adm.

        WHEN '2'.

          READ TABLE it_lines INDEX 12 INTO wa_lines.
          CONDENSE wa_lines-line NO-GAPS.
          IF wa_lines-line EQ 'A'.
            CLEAR: wa_lines.
            READ TABLE it_lines INDEX 13 INTO wa_lines.
            CONDENSE wa_lines-line NO-GAPS.
            IF wa_lines-line IS INITIAL.
**          Inclusao de nova linha na tabela e lote itens
              wa_zpfe_lote_item_c-nr_lote_adm = wa_zpfe_lote_item-nr_lote_adm.
              READ TABLE it_lines INDEX 8 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              wa_zpfe_lote_item_c-nr_ciot = wa_lines-line.

              READ TABLE it_lines INDEX 2 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              wa_zpfe_lote_item_c-nucontrato = wa_lines-line.

              READ TABLE it_lines INDEX 5 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              wa_zpfe_lote_item_c-ctenum = wa_lines-line.

              READ TABLE it_lines INDEX 3 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                 EXPORTING
                   input  = wa_lines-line
                 IMPORTING
                   output = wa_chvid_adm.

              SELECT SINGLE * INTO @DATA(wa_zpfe_lote_his_de)
                 FROM zpfe_lote_his_de
                WHERE chvid_adm EQ @wa_chvid_adm.

              IF sy-subrc IS INITIAL.
                wa_zpfe_lote_item_c-chvid = wa_zpfe_lote_his_de-chvid_emp.
              ENDIF.

              READ TABLE it_lines INDEX 4 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              CONCATENATE wa_lines-line+4(4) wa_lines-line+2(2) wa_lines-line(2) INTO wa_zpfe_lote_item_c-dt_transacao.

              READ TABLE it_lines INDEX 9 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              wa_zpfe_lote_item_c-peso_origem = wa_lines-line.

              READ TABLE it_lines INDEX 10 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              wa_zpfe_lote_item_c-peso_chegada = wa_zpfe_lote_item_c-peso_conf_adm = wa_lines-line.

              READ TABLE it_lines INDEX 11 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              CONCATENATE wa_lines-line+4(4) wa_lines-line+2(2) wa_lines-line(2) INTO  wa_zpfe_lote_item_c-dt_chegada.

              wa_zpfe_lote_item_c-dt_baixa = wa_zpfe_lote_item_c-dt_conf_adm = wa_zpfe_lote_item-dt_conf_adm.

              READ TABLE it_lines INDEX 7 INTO wa_lines.
              CONDENSE wa_lines-line NO-GAPS.
              IF NOT wa_lines-line IS INITIAL.
                DATA(lc_cont) = strlen( wa_lines-line ).
                SUBTRACT 2 FROM lc_cont.
                CONCATENATE wa_lines-line(lc_cont) '.' wa_lines-line+lc_cont INTO wa_lines-line.
              ELSE.
                wa_lines-line  = '0.00'.
              ENDIF.

              wa_zpfe_lote_item_c-vl_conf_adm = wa_zpfe_lote_item_c-vl_ajus_adm = wa_lines-line.

              SELECT SINGLE * INTO @DATA(wa_0025)
                 FROM zlest0025
                WHERE chvid EQ @wa_zpfe_lote_item_c-chvid.

              IF wa_0025-naturezachvid EQ 'S'.
                MULTIPLY wa_zpfe_lote_item_c-vl_ajus_adm BY -1.
              ENDIF.
              wa_zpfe_lote_item_c-ck_conf_adm = 'X'.

              APPEND wa_zpfe_lote_item_c TO it_zpfe_lote_item_c.
            ENDIF.
          ELSE.
            READ TABLE it_lines INDEX 2 INTO wa_lines.
            CONDENSE wa_lines-line NO-GAPS.
            wa_zpfe_lote_item-nucontrato = wa_lines-line.

            READ TABLE it_lines INDEX 3 INTO wa_lines.
            CONDENSE wa_lines-line NO-GAPS.
            SHIFT wa_lines-line LEFT DELETING LEADING '0'.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = wa_lines-line
               IMPORTING
                 output = wa_chvid_adm.

            SELECT SINGLE * INTO wa_zpfe_lote_his_de
               FROM zpfe_lote_his_de
              WHERE chvid_adm EQ wa_chvid_adm.

            IF sy-subrc IS INITIAL.
              wa_zpfe_lote_item-chvid = wa_zpfe_lote_his_de-chvid_emp.
            ENDIF.

            READ TABLE it_lines INDEX 7 INTO wa_lines.
            CONDENSE wa_lines-line NO-GAPS.

            CLEAR: lc_cont.
            lc_cont = strlen( wa_lines-line ).
            IF lc_cont GT 2.
              SUBTRACT 2 FROM lc_cont.
              CONCATENATE wa_lines-line(lc_cont) '.' wa_lines-line+lc_cont(2) INTO wa_lines-line.
              wa_zpfe_lote_item-vl_conf_adm = wa_lines-line.
            ENDIF.

            READ TABLE it_lines INDEX 10 INTO wa_lines.
            CONDENSE wa_lines-line NO-GAPS.
            wa_zpfe_lote_item-peso_conf_adm = wa_lines-line.

            APPEND wa_zpfe_lote_item TO it_zpfe_lote_item.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    IF it_zpfe_lote_item_c IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_lote)
         FROM zpfe_lote
          FOR ALL ENTRIES IN @it_zpfe_lote_item_c
        WHERE nr_lote_adm EQ @it_zpfe_lote_item_c-nr_lote_adm
          AND tplote      EQ 'S'.

      READ TABLE it_lote INDEX 1 INTO DATA(wa_lote).

      SELECT * INTO TABLE @DATA(it_itens)
         FROM zpfe_lote_item
          FOR ALL ENTRIES IN @it_zpfe_lote_item_c
        WHERE nr_lote_adm EQ @it_zpfe_lote_item_c-nr_lote_adm
          AND nm_lote     EQ @wa_lote-nm_lote.

      SELECT * INTO TABLE @DATA(it_zcte_ciot)
         FROM zcte_ciot
          FOR ALL ENTRIES IN @it_zpfe_lote_item_c
        WHERE nucontrato EQ @it_zpfe_lote_item_c-nucontrato.

      SORT it_itens BY nm_lote_item DESCENDING.

      LOOP AT it_zpfe_lote_item_c INTO wa_zpfe_lote_item_c.

        READ TABLE it_itens INTO DATA(wa_itens) WITH KEY nr_lote_adm = wa_zpfe_lote_item_c-nr_lote_adm.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: wa_itens.
        ENDIF.

        READ TABLE it_zcte_ciot INTO DATA(wa_zcte_ciot) WITH KEY nucontrato = wa_zpfe_lote_item_c-nucontrato.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: wa_zcte_ciot.
        ENDIF.

        wa_zpfe_lote_item_c-nm_lote_item = wa_itens-nm_lote_item + 1.
        wa_zpfe_lote_item_c-nm_lote      = wa_itens-nm_lote.
        wa_zpfe_lote_item_c-cd_ciot      = wa_zcte_ciot-cd_ciot.
        wa_zpfe_lote_item_c-docnum       = wa_zcte_ciot-docnum.
        wa_zpfe_lote_item_c-tknum        = wa_zcte_ciot-tknum.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
             input  = wa_zpfe_lote_item_c-nm_lote_item
           IMPORTING
             output = wa_zpfe_lote_item_c-nm_lote_item.

        lc_sucesso = 'X'.
        INSERT_LINE_LOG  WA_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
                          WA_ZPFE_LOTE_ITEM_C-NM_LOTE
                          WA_ZPFE_LOTE_ITEM_C-CHVID
                          WA_ZPFE_LOTE_ITEM_C-NUCONTRATO
                          'C'
                          'A linha do arquivo foi importado com sucesso'
                          SPACE
                          'S'.

        MODIFY zpfe_lote_item FROM wa_zpfe_lote_item_c.
      ENDLOOP.
      REFRESH: it_itens.
    ENDIF.

    IF it_zpfe_lote_item IS NOT INITIAL.

      SELECT * INTO TABLE it_itens
         FROM zpfe_lote_item
          FOR ALL ENTRIES IN it_zpfe_lote_item
        WHERE nr_lote_adm EQ it_zpfe_lote_item-nr_lote_adm
          AND nucontrato  EQ it_zpfe_lote_item-nucontrato
          AND chvid       EQ it_zpfe_lote_item-chvid.

      IF it_itens IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(it_zlest0025)
           FROM zlest0025
            FOR ALL ENTRIES IN @it_itens
          WHERE chvid EQ @it_itens-chvid.
      ENDIF.

      LOOP AT it_zpfe_lote_item INTO wa_zpfe_lote_item.
        DATA(lc_tabix) = sy-tabix.
        ADD 1 TO lc_tabix.

        READ TABLE it_lines_arq INDEX lc_tabix INTO wa_lines_arq.
        READ TABLE it_itens INTO wa_itens
           WITH KEY nr_lote_adm = wa_zpfe_lote_item-nr_lote_adm
                    nucontrato  = wa_zpfe_lote_item-nucontrato
                    chvid       = wa_zpfe_lote_item-chvid.

        IF sy-subrc IS INITIAL.
          IF wa_itens-ck_conf_adm IS INITIAL.
            lc_tabix = sy-tabix.

            READ TABLE it_zlest0025 INTO DATA(wa_zlest0025) WITH KEY chvid = wa_itens-chvid.
            IF sy-subrc IS INITIAL.
              MOVE: wa_zpfe_lote_item-dt_conf_adm   TO wa_itens-dt_conf_adm,
                     wa_zpfe_lote_item-vl_conf_adm   TO wa_itens-vl_conf_adm,
                     wa_zpfe_lote_item-peso_conf_adm TO wa_itens-peso_conf_adm,
                     'X'                             TO wa_itens-ck_conf_adm.

              IF wa_zlest0025-naturezachvid EQ 'S'.
                MULTIPLY wa_itens-vl_conf_adm  BY -1.
              ENDIF.

              MODIFY it_itens INDEX lc_tabix FROM wa_itens.
              lc_sucesso = 'X'.
              INSERT_LINE_LOG WA_ITENS-NR_LOTE_ADM
                               WA_ITENS-NM_LOTE
                               WA_ITENS-CHVID
                               WA_ITENS-NUCONTRATO
                               'C'
                               'A linha do arquivo foi importado com sucesso'
                               SPACE
                               'S'.
            ELSE.
              INSERT_LINE_LOG WA_ITENS-NR_LOTE_ADM
                               WA_ITENS-NM_LOTE
                               WA_ITENS-CHVID
                               WA_ITENS-NUCONTRATO
                               'C'
                               'A chave de identificação nao foi encontrada na tabela de "Controle chave de identificação de lote" ZLEST0025'
                               WA_LINES_ARQ
                               'E'.
            ENDIF.
          ELSE.
            INSERT_LINE_LOG WA_ITENS-NR_LOTE_ADM
                             WA_ITENS-NM_LOTE
                             WA_ITENS-CHVID
                             WA_ITENS-NUCONTRATO
                             'C'
                             'A linha já foi importada anteriormente. linha não importada'
                             WA_LINES_ARQ
                             'E'.

          ENDIF .
        ELSE.

          INSERT_LINE_LOG WA_ZPFE_LOTE_ITEM-NR_LOTE_ADM
                           SPACE
                           WA_ZPFE_LOTE_ITEM-CHVID
                           WA_ZPFE_LOTE_ITEM-NUCONTRATO
                           'C'
                           'A linha do documento financeiro não foi encontrado.'
                           WA_LINES_ARQ
                           'E'.
        ENDIF .

      ENDLOOP.

      IF lc_sucesso IS NOT INITIAL.
        IF it_itens[] IS NOT INITIAL.
          MODIFY zpfe_lote_item FROM TABLE it_itens.
        ENDIF.
      ENDIF.

      CREATE OBJECT arquivo.
      arquivo->set_bukrs( i_bukrs = i_bukrs ).
      arquivo->set_branch( i_branch = i_branch  ).
      arquivo->set_dt_inicial( i_dt_inicial = i_dt_inicial ).
      arquivo->set_dt_final( i_dt_final = i_dt_final ).
      arquivo->set_ds_filename( i_ds_filename = i_filename ).
      arquivo->set_corpo_arquivo( i_corpo_arquivo = it_lines_arq ).
      arquivo->set_nr_lote_adm( i_nr_lote_adm = lc_nr_lote_adm ).
      arquivo->zif_cadastro~gravar_registro( ).
    ENDIF.

  ENDMETHOD.


  METHOD salva_xml.

    TYPES: BEGIN OF ty_xml_viagem.
    TYPES:   xml TYPE string.
    TYPES: END OF ty_xml_viagem.

    DATA: wa_xml TYPE ty_xml_viagem,
          it_xml TYPE STANDARD TABLE OF ty_xml_viagem.

    CLEAR: it_xml.
    wa_xml-xml = i_xml.
    APPEND wa_xml TO it_xml.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = i_name_file
      TABLES
        data_tab                = it_xml
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD solicita_rota.

    DATA: e_chave       TYPE char32,
          lc_msg        TYPE string,
          lc_msg_adm    TYPE string,
          cx_exception  TYPE REF TO zescx_webservice,
          lc_xml        TYPE string,
          lc_http       TYPE REF TO if_http_client,
          wa_zlest0101  TYPE zlest0101,
          it_zlest0107  TYPE TABLE OF zlest0107,
          wa_zlest0107  TYPE zlest0107,
          it_zlest0107b TYPE zlest0107_t.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    SELECT SINGLE * INTO wa_zlest0101
      FROM zlest0101
     WHERE id_rota EQ i_rota
       AND bukrs   EQ i_bukrs
       AND branch  EQ i_branch.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e088(zles) WITH i_rota RAISING zwebservice.
    ELSEIF wa_zlest0101-id_rota_adm IS NOT INITIAL.
      MESSAGE e087(zles) RAISING zwebservice.
    ENDIF.

    SELECT * INTO TABLE it_zlest0107
      FROM zlest0107
     WHERE id_rota EQ i_rota
       AND bukrs   EQ i_bukrs
       AND branch  EQ i_branch
     ORDER BY id_rota_item.

    LOOP AT it_zlest0107 INTO wa_zlest0107.
      APPEND wa_zlest0107 TO it_zlest0107b.
    ENDLOOP.

    TRY .
        e_chave = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    lc_xml = xml_criar_rota( i_chave = e_chave i_zlest0101 = wa_zlest0101 i_zlest0107 = it_zlest0107b ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RO - Criar Rota.
        me->set_servico( EXPORTING i_servico = 'RO' ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'R').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        lc_http = me->url( ).

      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zesif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zesif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = lc_msg_adm
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    "LC_MSG_ADM = ME->ZESIF_WEBSERVICE~CONSULTAR( I_HTTP = LC_HTTP I_XML = LC_XML ).

    CLEAR: e_id_rota_adm, e_msg.

    CALL METHOD me->ler_xml_criar_rota
      EXPORTING
        i_xml         = lc_msg_adm
      IMPORTING
        e_msg         = e_msg
        e_id_rota_adm = e_id_rota_adm.

    IF e_id_rota_adm IS NOT INITIAL.
      wa_zlest0101-id_rota_adm = e_id_rota_adm.
      MODIFY zlest0101 FROM wa_zlest0101.
      COMMIT WORK.
    ELSE.
      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ENDIF.

  ENDMETHOD.


  METHOD xml_atualizar_rota.

    DATA: wa_j_1bbranch TYPE j_1bbranch.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs   EQ i_bukrs
       AND branch  EQ i_branch.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<atualizaRota>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<tipoConsulta>T</tipoConsulta>'.

    CONC_XML '</atualizaRota>'.

  ENDMETHOD.


  METHOD xml_atualiza_rota.

    CLEAR: e_xml. "Limpar a variavel de retorno.

    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    "Montar o arquivo XML para consultar das Rotas.
    CONC_XML '<atualizaRota>'.
    CONC_XML   '<chave>'.
    CONC_XML     I_CHAVE.
    CONC_XML   '</chave>'.
    CONC_XML   '<cnpjContratante>'.
    CONC_XML    I_CNPJCONTRATANTE.
    CONC_XML   '</cnpjContratante>'.
    CONC_XML   '<tipoConsulta>'.
    CONC_XML   'A'.
    CONC_XML   '</tipoConsulta>'.
    CONC_XML '</atualizaRota>'.

  ENDMETHOD.


  METHOD xml_autentica.
*****************
*  Descrição: Método para Montar a estrutura do arquivo XML de Autenticação.
*  Data: 07.04.2014 14:29:28
*  Developer: Victor Hugo Souza Nunes
*****************

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    "Montar o Arquivo XML da autenticação.
    CONC_XML '<autenticacao>'.
    CONC_XML  '<usuario>'.
    CONC_XML   I_USUARIO.
    CONC_XML  '</usuario>'.
    CONC_XML  '<senha>'.
    CONC_XML   I_SENHA.
    CONC_XML  '</senha>'.
    CONC_XML '</autenticacao>'.

  ENDMETHOD.


  METHOD xml_consultar_arq_cobranca.

    DATA: wa_j_1bbranch   TYPE j_1bbranch.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs  EQ i_bukrs
       AND branch EQ i_branch.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<arquivoCobrancaPedagio>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<codigoFatura>'.
    CONC_XML I_FATURA.
    CONC_XML '</codigoFatura>'.

    CONC_XML '</arquivoCobrancaPedagio>'.

  ENDMETHOD.


  METHOD xml_consultar_arq_conferencia.

    DATA: wa_j_1bbranch   TYPE j_1bbranch,
          lc_data_inicial TYPE c LENGTH 8,
          lc_data_final   TYPE c LENGTH 8.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs  EQ i_bukrs
       AND branch EQ i_branch.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<requisicaoArquivoConferencia>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONCATENATE i_dt_inicial+6(2) i_dt_inicial+4(2) i_dt_inicial(4) INTO lc_data_inicial.
    CONCATENATE i_dt_final+6(2)   i_dt_final+4(2)   i_dt_final(4)   INTO lc_data_final.

    CONC_XML '<dataConferenciaInicio>'.
    CONC_XML LC_DATA_INICIAL.
    CONC_XML '</dataConferenciaInicio>'.

    CONC_XML '<dataConferenciaFim>'.
    CONC_XML LC_DATA_FINAL.
    CONC_XML '</dataConferenciaFim>'.

    CONC_XML '</requisicaoArquivoConferencia>'.

  ENDMETHOD.


  METHOD xml_consultar_rota.

    DATA: wa_j_1bbranch TYPE j_1bbranch.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs   EQ i_zlest0101-bukrs
       AND branch  EQ i_zlest0101-branch.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<consultaRota>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<codigoRota>'.
    CONC_XML I_ZLEST0101-ID_ROTA_ADM.
    CONC_XML '</codigoRota>'.

    CONC_XML '</consultaRota>'.

  ENDMETHOD.


  METHOD xml_consulta_rota.

  ENDMETHOD.


  METHOD xml_consulta_transportador.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<situacaoTransportador>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML I_CONSULTA_RNTRC-CNPJ_CONTRATANTE.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<cnpj>'.
    CONC_XML I_CONSULTA_RNTRC-CNPJ_CONTRATADO.
    CONC_XML '</cnpj>'.

    CONC_XML '<rntrc>'.
    CONC_XML I_CONSULTA_RNTRC-RNTRC.
    CONC_XML '</rntrc>'.

    CONC_XML '<placa>'.
    CONC_XML I_CONSULTA_RNTRC-DS_PLACA.
    CONC_XML '</placa>'.

    CONC_XML '</situacaoTransportador>'.

  ENDMETHOD.


  METHOD xml_criar_rota.

    DATA: wa_j_1bbranch TYPE j_1bbranch,
          wa_zlest0107  TYPE zlest0107.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs   EQ i_zlest0101-bukrs
       AND branch  EQ i_zlest0101-branch.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<rota>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<retornoOrigem>'.
    CONC_XML I_ZLEST0101-TP_ROTA_PERC.
    CONC_XML '</retornoOrigem>'.

    "Cidades """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONC_XML '<cidades>'.
    CONC_XML '<codigoCidade>'.
    CONC_XML I_ZLEST0101-CD_CID_ORIGEM+3(7).
    CONC_XML '</codigoCidade>'.

    LOOP AT i_zlest0107 INTO wa_zlest0107.
      CONC_XML '<codigoCidade>'.
      CONC_XML WA_ZLEST0107-CD_CIDADE+3(7).
      CONC_XML '</codigoCidade>'.
    ENDLOOP.

    CONC_XML '<codigoCidade>'.
    CONC_XML I_ZLEST0101-CD_CID_DESTINO+3(7).
    CONC_XML '</codigoCidade>'.
    CONC_XML '</cidades>'.
    "Cidades """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CONC_XML '</rota>'.

  ENDMETHOD.


  METHOD check_data_envio_email.

    DATA: it_zlest0135 TYPE TABLE OF zlest0135,
          enviar_email TYPE char01,
          lv_data_lim  TYPE char14,
          lv_data_cons TYPE char14,
          lv_ret_date   TYPE tvpod-rudat,
          lv_ret_time   TYPE tvpod-rutim.


    DATA: lwa_zlest0135 TYPE zlest0135.

    lwa_zlest0135 = e_consultas.


*--------------------------------------------------------------------------------------------------------*
*  consultar a ultima atualização de envio email.
*--------------------------------------------------------------------------------------------------------*

    CLEAR: enviar_email.
    FREE: it_zlest0135.
    SELECT *
    FROM zlest0135 INTO TABLE it_zlest0135
    WHERE dt_tipbank_email EQ sy-datum.

    IF it_zlest0135 IS NOT INITIAL.
      SORT it_zlest0135 BY dt_tipbank_email hr_tipbank_email DESCENDING.
      READ TABLE it_zlest0135 INTO DATA(ws_zlest0135) INDEX 1.
      IF ws_zlest0135-dt_tipbank_email IS NOT INITIAL.

        "Verificar se hora que esta na tabela é maior que 6 hora, se for maior devera consultar novamente a API.
        CALL FUNCTION 'TSTR_CALC_TIME'
          EXPORTING
            iv_begin_datelocal_req   = sy-datum
            iv_begin_timelocal_req   = sy-uzeit
            iv_duration_integer      = 1800 "Data atual menos 30 minutos.
            iv_direction             = '-'
          IMPORTING
            ev_end_datelocal         = lv_ret_date
            ev_end_timelocal         = lv_ret_time
          EXCEPTIONS
            fatal_error              = 1
            time_invalid             = 2
            time_missing             = 3
            tstream_not_loadable     = 4
            tstream_generation_error = 5
            parameter_error          = 6
            unspecified_error        = 7
            OTHERS                   = 8.


        lv_data_lim   = |{ lv_ret_date }{ lv_ret_time }|.
        lv_data_cons  = |{ ws_zlest0135-dt_atualizacao }{ ws_zlest0135-hr_atualizacao }|.


        IF  lv_data_cons > lv_data_lim.
          enviar_email = abap_false.
        ELSE.
          enviar_email = abap_true.
        ENDIF.

      ELSE.
        enviar_email = abap_true.
      ENDIF.

    ELSE.
      enviar_email = abap_true.
    ENDIF.


*--------------------------------------------------------------------------------------------------------*
*   "Envio email/ Registrar ultimo envio.
*--------------------------------------------------------------------------------------------------------*
    IF enviar_email EQ abap_true.

      lwa_zlest0135-dt_tipbank_email = sy-datum.
      lwa_zlest0135-hr_tipbank_email = sy-uzeit.

      MODIFY  zlest0135 FROM lwa_zlest0135.
      COMMIT WORK.

      me->enviar_email_tip( e_consultas = lwa_zlest0135 ).

    ENDIF.

    CLEAR: ws_zlest0135, enviar_email.

  ENDMETHOD.


  METHOD cons_status_parceiro.

    DATA: lc_webservice TYPE REF TO zescl_webservice_tipcard.


    TYPES: BEGIN OF ty_dados,
             cd_transportador TYPE  lifnr,
             ds_placa         TYPE  zpc_veiculo,
             cnpj_contratante TYPE  stcd1,
             cnpj_contratado  TYPE  stcd1,
             cpf_contratado   TYPE  stcd1.
    TYPES END OF ty_dados.

    DATA: ws_dados         TYPE ZESDE_consulta_parceiro,
          it_dados         TYPE TABLE OF ZESDE_consulta_parceiro,
          e_chave          TYPE char32,
          cx_exception     TYPE REF TO zescx_webservice,
          lc_msg           TYPE string,
          lc_xml           TYPE string,
          lc_msg_adm       TYPE string,
          i_name_file      TYPE string,
          it_consultas     TYPE TABLE OF ZESDE_consulta_rntrc,
          wa_consultas     TYPE ZESDE_consulta_rntrc,
          wa_j_1bbranch    TYPE j_1bbranch,
          wa_lfa1          TYPE lfa1,
          it_lfa1          TYPE TABLE OF lfa1,
          lc_http          TYPE REF TO if_http_client,
          wa_zlest0135     TYPE zlest0135,
          wa_zlest0135_aux TYPE zlest0135,
          it_zlest0135     TYPE TABLE OF zlest0135,
          i_parceiro       TYPE j_1bparid.


    FREE: it_zlest0135.

*--------------------------------------------------------------------------------------------------------*
*   "Dados de acesso token.
*--------------------------------------------------------------------------------------------------------*

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs   EQ @i_bukrs
       AND branch  EQ @i_branch.

    CREATE OBJECT lc_webservice.

    TRY .
        e_chave = lc_webservice->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zescx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
    ENDTRY.

*--------------------------------------------------------------------------------------------------------*
*   "Dados da empresa contratante.
*--------------------------------------------------------------------------------------------------------*

    SELECT SINGLE * INTO wa_j_1bbranch FROM j_1bbranch
      WHERE bukrs  EQ i_bukrs
        AND branch EQ i_branch.

    IF sy-subrc EQ 0.

*--------------------------------------------------------------------------------------------------------*
*   "Dados da empresa contratada / proprietario do veiculo.
*--------------------------------------------------------------------------------------------------------*

      CLEAR: wa_lfa1.
      SELECT SINGLE * FROM lfa1 AS a
      INNER JOIN zlest0002 AS b ON b~proprietario EQ a~lifnr
      INTO CORRESPONDING FIELDS OF wa_lfa1
      WHERE pc_veiculo EQ i_placa.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e113(zles) WITH i_placa RAISING erro.
      ENDIF.

      ws_dados-cd_transportador = wa_lfa1-lifnr.
      ws_dados-ds_placa         = i_placa.
      ws_dados-cnpj_contratante = wa_j_1bbranch-stcd1.


      CASE wa_lfa1-stkzn.
        WHEN abap_true.
          ws_dados-cpf_contratado  = wa_lfa1-stcd2.
        WHEN abap_false.
          ws_dados-cnpj_contratado  = wa_lfa1-stcd1.
      ENDCASE.

      APPEND ws_dados TO it_dados.

*--------------------------------------------------------------------------------------------------------*
*   Dados do endpoint para realizar a consulta na API.
*--------------------------------------------------------------------------------------------------------*

      TRY .
          lc_webservice->set_servico( EXPORTING i_servico = 'SP' ).
        CATCH zescx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      lc_webservice->set_tipo( EXPORTING i_tipo = 'S').

      TRY .
          lc_http = lc_webservice->url( ).
        CATCH zescx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.


*--------------------------------------------------------------------------------------------------------*
*  Consultar a API.
*--------------------------------------------------------------------------------------------------------*
      CLEAR: ws_dados.
      LOOP AT it_dados INTO ws_dados.

        CLEAR: lc_msg_adm, wa_zlest0135_aux,  wa_zlest0135.
        SELECT SINGLE * FROM zlest0135 INTO wa_zlest0135
          WHERE ds_placa EQ ws_dados-ds_placa
            AND cd_transportador EQ ws_dados-cd_transportador.
        IF sy-subrc NE 0.
          wa_zlest0135-cd_transportador  = ws_dados-cd_transportador.
          wa_zlest0135-ds_placa          = ws_dados-ds_placa.
        ENDIF.

        lc_webservice->zesif_webservice~abrir_conexao( lc_http ).

        lc_xml = lc_webservice->xml_consulta_status_parceiro( EXPORTING i_chave = e_chave i_consulta = ws_dados ).

        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consultaParceiroReq.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_xml.
        ENDIF.


        CALL METHOD lc_webservice->zesif_webservice~consultar
          EXPORTING
            i_http                     = lc_http
            i_xml                      = lc_xml
          RECEIVING
            e_resultado                = lc_msg_adm
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING webservice.
        ENDIF.

*--------------------------------------------------------------------------------------------------------*
*  Retorno da API.
*--------------------------------------------------------------------------------------------------------*
        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consultaParceiroReq.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_msg_adm.
        ENDIF.

        wa_zlest0135_aux = lc_webservice->ler_xml_status_parceiro( EXPORTING i_xml = lc_msg_adm ).

        IF wa_zlest0135_aux-tp_transportador IS NOT INITIAL.
          CASE wa_zlest0135_aux-tp_transportador.
            WHEN '1'.
              wa_zlest0135-tp_transportador  = '1'.
              wa_zlest0135-ck_etc_equiparado = ''.  "vazio
            WHEN '2'.
              wa_zlest0135-tp_transportador  = '1'.
              wa_zlest0135-ck_etc_equiparado = 'X'.  "
            WHEN '3'.
              wa_zlest0135-tp_transportador  = '2'.
              wa_zlest0135-ck_etc_equiparado = ''.  "Vazio
            WHEN '4'.
              wa_zlest0135-tp_transportador  = '2'.
              wa_zlest0135-ck_etc_equiparado = 'X'.  "
            WHEN '5'.
              wa_zlest0135-tp_transportador  = '3'.
              wa_zlest0135-ck_etc_equiparado = ''.  "vazio
            WHEN '6'.
              wa_zlest0135-tp_transportador  = '3'.
              wa_zlest0135-ck_etc_equiparado = 'X'.  "
            WHEN OTHERS.
          ENDCASE.

          wa_zlest0135-dt_atualizacao  = sy-datum.
          wa_zlest0135-hr_atualizacao  = sy-uzeit.
          wa_zlest0135-consulta_base_tipbank  = abap_true.

          APPEND wa_zlest0135 TO it_zlest0135.
          CLEAR: wa_zlest0135, ws_dados.
        ELSE.
          APPEND VALUE #(
          cd_transportador  = ws_dados-cd_transportador
          ds_placa          = ws_dados-ds_placa ) TO e_consultas.
        ENDIF.
      ENDLOOP.

      IF it_zlest0135[] IS NOT INITIAL.
        MOVE it_zlest0135[] TO e_consultas[].
        MODIFY zlest0135 FROM TABLE it_zlest0135.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD enviar_email_tip.

    DATA: t_html TYPE string,
          t_host TYPE sy-host.

    DATA: lt_to        TYPE crmt_email_recipients,
          lt_copy      TYPE crmt_email_recipients,
          ls_recep     TYPE crms_email_recipient,
          lt_mail_body TYPE crmt_email_mime_struc,
          ls_mail_body TYPE crms_email_mime_struc,
          dt           TYPE char10,
          hr           TYPE char8,
          lv_activity  TYPE sysuuid_x.

    DATA: lo_create_mail TYPE REF TO cl_crm_email_data.
    CREATE OBJECT lo_create_mail.

    CLEAR: dt, hr.

    "Verificar endereço email.
    SELECT *
    FROM tvarvc INTO TABLE @DATA(it_stvarv)
    WHERE name = 'SIT_TRANSP_EMAIL'. "Sit_Transp_email

    CHECK it_stvarv IS NOT INITIAL.

    dt = |{ e_consultas-dt_tipbank_email+6(2) }.{ e_consultas-dt_tipbank_email+4(2) }.{ e_consultas-dt_tipbank_email+0(4) }|.
    hr = |{ e_consultas-hr_tipbank_email+0(2) }:{ e_consultas-hr_tipbank_email+2(2) }:{ e_consultas-hr_tipbank_email+4(2) }|.

    CLEAR t_html.
    CONCATENATE t_html '<html>'  INTO t_html.
    CONCATENATE t_html '<head>'  INTO t_html.
    CONCATENATE t_html '</head>' INTO t_html.
    CONCATENATE t_html '<body>'  INTO t_html.
    TRANSLATE t_host TO UPPER CASE.
    CONCATENATE t_html '<div><font face=Verdana size=4>' t_host '</font></div>'            INTO t_html.
    CONCATENATE t_html '<div><font face=Verdana size=3>Sistema de consulta da ANTT</font></div>' INTO t_html.
    CONCATENATE t_html '<br><br>'                               INTO t_html.
    CONCATENATE t_html '<align=left>&nbsp;</div>'               INTO t_html.
    CONCATENATE t_html '<div><p>O Sistema de consulta da ANTT entrou em contingencia desde' dt  'as ' hr '</p></div>' INTO t_html SEPARATED BY space.
    CONCATENATE t_html '<div><p>As consultas atuais serão em base ao histórico da TIPBANK, ate o retorno do Serviço.</p></div>' INTO t_html.
    CONCATENATE t_html '</body>'  INTO t_html.
    CONCATENATE t_html '</html>'  INTO t_html.

    lo_create_mail->subject =  'Sistema de consulta da ANTT'.

    CLEAR ls_mail_body.
    ls_mail_body-content_ascii = t_html.
    ls_mail_body-mime_type     = 'text/html'.
    APPEND ls_mail_body TO lt_mail_body.

    MOVE lt_mail_body TO lo_create_mail->body.

    LOOP AT it_stvarv INTO DATA(ws_end_email).
      CLEAR ls_recep.
      ls_recep-address = ws_end_email-low.
      APPEND ls_recep TO lt_to.
    ENDLOOP.

    MOVE lt_to TO lo_create_mail->to.

    ls_recep-address = 'suporte.sap@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.

    MOVE lt_copy TO lo_create_mail->copy.

    CLEAR ls_recep.
    ls_recep-address = ''.
    MOVE ls_recep TO lo_create_mail->from.

    CALL METHOD cl_crm_email_utility_base=>send_email
      EXPORTING
        iv_mail_data       = lo_create_mail
      RECEIVING
        ev_send_request_id = lv_activity.

    COMMIT WORK.
  ENDMETHOD.


  METHOD ler_xml_status_parceiro.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_document      TYPE REF TO if_ixml_document,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_node          TYPE REF TO if_ixml_node,
          iterator         TYPE REF TO if_ixml_node_iterator.

    DATA: tag_name      TYPE string,
          valor_dom     TYPE string,
          filho         TYPE REF TO if_ixml_node_list,
          iterator2     TYPE REF TO if_ixml_node_iterator,
          if_node_filho TYPE REF TO if_ixml_node.

    CLEAR: e_zlest0135.


    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).
    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory istream = if_stream document = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.
        CASE if_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = if_node->get_name( ).

            CASE tag_name.
              WHEN 'consultaParceiroRetorno'.
                filho         = if_node->get_children( ).
                iterator2     = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'tipoContratado'.
                      e_zlest0135-tp_transportador = valor_dom.
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.

    ENDIF.
  ENDMETHOD.


  METHOD xml_consulta_status_parceiro.

    CLEAR: e_xml. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE conc_xml.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

*    Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<consultaParceiro>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML I_CONSULTA-CNPJ_CONTRATANTE.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<numeroCNPJ>'.
    CONC_XML I_CONSULTA-CNPJ_CONTRATADO.
    CONC_XML '</numeroCNPJ>'.

    CONC_XML '<numeroCPF>'.
    CONC_XML I_CONSULTA-CPF_CONTRATADO.
    CONC_XML '</numeroCPF>'.

    CONC_XML '<tipo>'.
    CONC_XML '02'.
    CONC_XML '</tipo>'.

    CONC_XML '</consultaParceiro>'.
  ENDMETHOD.
ENDCLASS.