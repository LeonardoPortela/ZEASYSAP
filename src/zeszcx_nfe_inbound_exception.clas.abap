CLASS ZESZCX_NFE_INBOUND_EXCEPTION DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_sem_departamento,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_departamento .
    CONSTANTS:
      BEGIN OF zcx_tomador_nao_localizado,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tomador_nao_localizado .
    CONSTANTS:
      BEGIN OF zcx_fornecedor_nao_localizado,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_fornecedor_nao_localizado .
    CONSTANTS:
      BEGIN OF zcx_grupo_mercadoria,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_grupo_mercadoria .
    CONSTANTS:
      BEGIN OF zcx_estorna_romaneio_saida,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '046',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_estorna_romaneio_saida .
    CONSTANTS:
      BEGIN OF zcx_estorna_movimento_merc,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '047',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_estorna_movimento_merc .
    CONSTANTS:
      BEGIN OF zcx_pedido_nao_info_xml,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '048',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pedido_nao_info_xml .
    CONSTANTS:
      BEGIN OF zcx_permissao_acesso,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '045',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_permissao_acesso .
    CONSTANTS:
      BEGIN OF zcx_determinar_pedido,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '052',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_determinar_pedido .
    CONSTANTS:
      BEGIN OF zcx_determinar_iva,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '053',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'TRANSACAO',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_determinar_iva .
    CONSTANTS:
      BEGIN OF zcx_armazenagem_pendente,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '055',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_armazenagem_pendente .
    CONSTANTS:
      BEGIN OF zcx_sem_aceite_fiscal,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '056',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_aceite_fiscal .
    CONSTANTS:
      BEGIN OF zcx_possui_aceite_fisico,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '058',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_possui_aceite_fisico .
    CONSTANTS:
      BEGIN OF zcx_nao_possui_aceite_fisico,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '059',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_possui_aceite_fisico .
    CONSTANTS:
      BEGIN OF zcx_erro_estorno_migo,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '060',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_erro_estorno_migo .
    CONSTANTS:
      BEGIN OF zcx_possui_aviso_receb,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '061',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_possui_aviso_receb .
    CONSTANTS:
      BEGIN OF zcx_npossui_aviso_receb,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '062',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_npossui_aviso_receb .
    CONSTANTS:
      BEGIN OF zcx_obriga_lote,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '064',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obriga_lote .
    CONSTANTS:
      BEGIN OF zcx_obriga_lote_numero,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '065',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obriga_lote_numero .
    CONSTANTS:
      BEGIN OF zcx_obriga_lote_vencimento,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '066',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obriga_lote_vencimento .
    CONSTANTS:
      BEGIN OF zcx_nao_encontrado_lote,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '067',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_encontrado_lote .
    CONSTANTS:
      BEGIN OF zcx_item_sem_volume_disp,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '069',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_item_sem_volume_disp .
    CONSTANTS:
      BEGIN OF zcx_item_original,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '070',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_item_original .
    CONSTANTS:
      BEGIN OF zcx_sem_aceite_fisico,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '074',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_aceite_fisico .
    CONSTANTS:
      BEGIN OF zcx_possui_miro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '076',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_possui_miro .
    CONSTANTS:
      BEGIN OF zcx_nao_possui_miro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '077',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_possui_miro .
    CONSTANTS:
      BEGIN OF zcx_fase_obrigatorio,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '078',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_fase_obrigatorio .
    CONSTANTS:
      BEGIN OF zcx_bloqueio_pagamento,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '080',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bloqueio_pagamento .
    CONSTANTS:
      BEGIN OF zcx_bloqueio_parametrizado,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '081',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bloqueio_parametrizado .
    CONSTANTS:
      BEGIN OF zcx_volume_lote_errado,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '084',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_volume_lote_errado .
    CONSTANTS:
      BEGIN OF zcx_sem_saldo_particao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '085',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_saldo_particao .
    CONSTANTS:
      BEGIN OF zcx_nao_gerou_aviso,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '051',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_gerou_aviso .
    CONSTANTS:
      BEGIN OF zcx_retorno_avisos,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '086',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_retorno_avisos .
    CONSTANTS:
      BEGIN OF zcx_sem_saldo_particao_ped,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '136',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_saldo_particao_ped .
    CONSTANTS:
      BEGIN OF zcx_retorno_aviso,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '087',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_retorno_aviso .
    CONSTANTS:
      BEGIN OF zcx_possui_migo,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '088',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_possui_migo .
    CONSTANTS:
      BEGIN OF zcx_possui_fiscal,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '089',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_possui_fiscal .
    CONSTANTS:
      BEGIN OF zcx_iva_errado_pedido,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '091',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_iva_errado_pedido .
    CONSTANTS:
      BEGIN OF zcx_cfop_nao_encontrado,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '094',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cfop_nao_encontrado .
    CONSTANTS:
      BEGIN OF zcx_nao_gera_miro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '097',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_gera_miro .
    CONSTANTS:
      BEGIN OF zcx_nao_gera_migo,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '096',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_gera_migo .
    CONSTANTS:
      BEGIN OF zcx_tipo_pedido_nao_permitido,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '098',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tipo_pedido_nao_permitido .
    CONSTANTS:
      BEGIN OF zcx_nota_futura,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '100',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_futura .
    CONSTANTS:
      BEGIN OF zcx_nota_futura_nao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '099',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nota_futura_nao .
    CONSTANTS:
      BEGIN OF zcx_nfe_cancelada,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '105',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_cancelada .
    CONSTANTS:
      BEGIN OF zcx_nao_enc_nfe_aviso,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '107',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nao_enc_nfe_aviso .
    CONSTANTS:
      BEGIN OF zcx_nfe_escriturada,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '108',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_escriturada .
    CONSTANTS:
      BEGIN OF zcx_nfe_pedido_fornecedor,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '109',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_pedido_fornecedor .
    CONSTANTS:
      BEGIN OF zcx_nfe_pedido_empresa,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '110',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_pedido_empresa .
    CONSTANTS:
      BEGIN OF zcx_nfe_pedido_filial,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '111',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_pedido_filial .
    CONSTANTS:
      BEGIN OF zcx_nfe_pedido_dif_valor,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '112',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_pedido_dif_valor .
    CONSTANTS:
      BEGIN OF zcx_material_centro_ncm,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '114',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_material_centro_ncm .
    CONSTANTS:
      BEGIN OF zcx_material_centro_catcfop,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '115',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_material_centro_catcfop .
    CONSTANTS:
      BEGIN OF zcx_item_pedido_bloq,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '162',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_item_pedido_bloq .
    CONSTANTS:
      BEGIN OF zcx_material_centro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '039',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_material_centro .
    CONSTANTS:
      BEGIN OF zcx_erro_geral,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_erro_geral .
    CONSTANTS:
      BEGIN OF zcx_chave_nao_encontrada,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_chave_nao_encontrada .
    CONSTANTS:
      BEGIN OF zcx_sem_id_simetrya,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '121',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_id_simetrya .
    CONSTANTS:
      BEGIN OF zcx_sem_webservice_danfe,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '122',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_webservice_danfe .
    CONSTANTS:
      BEGIN OF zcx_objeto_autorizacao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '045',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_objeto_autorizacao .
    CONSTANTS:
      BEGIN OF zcx_banco_parceiro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '125',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_banco_parceiro .
    CONSTANTS:
      BEGIN OF zcx_possui_frete_erro_ped,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_possui_frete_erro_ped .
    CONSTANTS:
      BEGIN OF zcx_est_somente_aviso,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '131',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_est_somente_aviso .
    CONSTANTS:
      BEGIN OF zcx_sem_categoria_cfop,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '132',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_categoria_cfop .
    CONSTANTS:
      BEGIN OF zcx_sem_area_valiacao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '133',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_area_valiacao .
    CONSTANTS:
      BEGIN OF zcx_sem_utl_material,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '134',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_utl_material .
    CONSTANTS:
      BEGIN OF zcx_sem_org_material,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '135',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_org_material .
    CONSTANTS:
      BEGIN OF zcx_nfe_em_revisao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '137',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_em_revisao .
    CONSTANTS:
      BEGIN OF zcx_nfe_sem_revisao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '138',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_nfe_sem_revisao .
    CONSTANTS:
      BEGIN OF zcx_inf_data_vencimento,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '140',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_inf_data_vencimento .
    CONSTANTS:
      BEGIN OF zcx_inf_data_producao,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '139',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_inf_data_producao .
    CONSTANTS:
      BEGIN OF zcx_dt_producao_erro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '141',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dt_producao_erro .
    CONSTANTS:
      BEGIN OF zcx_dt_vencimento_erro,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '142',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_dt_vencimento_erro .
    CONSTANTS:
      BEGIN OF zcx_informar_armazem,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '148',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_informar_armazem .
    CONSTANTS:
      BEGIN OF zcx_sem_classe_zmm0111,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '149',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_sem_classe_zmm0111 .
    CONSTANTS:
      BEGIN OF zcx_existe_arm_dev,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '150',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_existe_arm_dev .
    CONSTANTS:
      BEGIN OF zcx_obriga_lote_fab,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_obriga_lote_fab.

    CONSTANTS:
      BEGIN OF zcx_zlicha_existe,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '149',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_zlicha_existe.

    CONSTANTS:
      BEGIN OF zcx_campos_obrig,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '166',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_campos_obrig .

    CONSTANTS:
      BEGIN OF zcx_transp_obrigatorio,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '164',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_transp_obrigatorio.

"ZMM0110 - bloq entrada nf com valor dif do ped #152648 - BG - INICIO
       CONSTANTS:
      BEGIN OF zcx_bloc_NF_VALOR_DIF_PEDIDO,
        msgid TYPE symsgid VALUE 'ZNFE_DISTRI',
        msgno TYPE symsgno VALUE '171',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bloc_NF_VALOR_DIF_PEDIDO.
"ZMM0110 - bloq entrada nf com valor dif do ped #152648 - BG - FIM
    DATA msgty TYPE syst_msgty .
    DATA msgno TYPE syst_msgno .
    DATA msgv1 TYPE syst_msgv .
    DATA msgv2 TYPE syst_msgv .
    DATA msgv3 TYPE syst_msgv .
    DATA msgv4 TYPE syst_msgv .
    DATA msgid TYPE syst_msgid .
    DATA transacao TYPE tcode .

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !msgty     TYPE syst_msgty OPTIONAL
        !msgno     TYPE syst_msgno OPTIONAL
        !msgv1     TYPE syst_msgv OPTIONAL
        !msgv2     TYPE syst_msgv OPTIONAL
        !msgv3     TYPE syst_msgv OPTIONAL
        !msgv4     TYPE syst_msgv OPTIONAL
        !msgid     TYPE syst_msgid OPTIONAL
        !transacao TYPE tcode OPTIONAL .
    METHODS published_erro
      IMPORTING
        !i_msgty         TYPE syst_msgty OPTIONAL
        !i_msgty_display TYPE syst_msgty OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZESZCX_NFE_INBOUND_EXCEPTION IMPLEMENTATION.


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
