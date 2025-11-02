FUNCTION z_grc_arquivo_doc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_CHAVE) TYPE  STRING OPTIONAL
*"     REFERENCE(I_GUID) TYPE  STRING OPTIONAL
*"     REFERENCE(I_TIPO) TYPE  STRING DEFAULT 'PDF'
*"     REFERENCE(I_ID_CCE) TYPE  NUM OPTIONAL
*"     REFERENCE(I_CNPJ) TYPE  ZESZXNFE_CNPJ OPTIONAL
*"     REFERENCE(I_DATA_EMISSAO) TYPE  SYDATUM OPTIONAL
*"     REFERENCE(I_NUMERO_NF) TYPE  DOCNUM OPTIONAL
*"     REFERENCE(I_TIPO_NF) TYPE  CHAR3 OPTIONAL
*"  EXPORTING
*"     REFERENCE(OUT) TYPE  XSTRING
*"     REFERENCE(E_NAME) TYPE  STRING
*"     REFERENCE(E_DOCNUM) TYPE  J_1BDOCNUM
*"  RAISING
*"      ZESZCX_DOC_ELETRONICO
*"----------------------------------------------------------------------

  "140872 - CS2024000421 API DE VALIDAÇÃO DE XML ZFIS63 NFPS - PSA 20/06/2024
*I_CNPJ
*I_DATA_EMISSAO
*I_NUMERO_NF
*I_TIPO_NF


  DATA: wa_active TYPE j_1bnfe_active.
  DATA: p_chave TYPE ZESZDE_CHAVE_NFE.
  DATA: tp_doc TYPE c LENGTH 3.
  DATA: t_zib_nfe_dist_ter TYPE TABLE OF ZESZIB_NFE_DIST_TER.

**********************************************************************
  "140872 - CS2024000421 API DE VALIDAÇÃO DE XML ZFIS63 NFPS - PSA 20/06/2024

  IF i_tipo_nf IS NOT INITIAL AND i_cnpj IS NOT INITIAL AND i_data_emissao IS NOT INITIAL AND i_numero_nf IS NOT INITIAL AND i_tipo IS NOT INITIAL.

    CASE i_tipo_nf.
      WHEN 'NFS'.
        SELECT SINGLE *
      FROM /tcsr/t_hd
      INTO @DATA(lw_t_hd)
        WHERE p_cnpj = @i_cnpj
      AND nfse_numero = @i_numero_nf
      AND dtemissao = @i_data_emissao.

* ----> BUG #179037 - MMSILVA - 13.05.2025 - Inicio <----
        IF lw_t_hd IS INITIAL.

          DATA: ls_valid_nfse TYPE char15.

          ls_valid_nfse = i_numero_nf.
          CONDENSE ls_valid_nfse NO-GAPS.
          ls_valid_nfse = |{ ls_valid_nfse ALPHA = IN }|.

          SELECT SINGLE *
            FROM /tcsr/t_hd
            INTO @lw_t_hd
            WHERE p_cnpj = @i_cnpj
            AND nfse_numero = @ls_valid_nfse
            AND dtemissao = @i_data_emissao.

          IF lw_t_hd IS INITIAL.

            ls_valid_nfse(4) = i_data_emissao(4).

            SELECT SINGLE *
              FROM /tcsr/t_hd
              INTO @lw_t_hd
              WHERE p_cnpj = @i_cnpj
              AND nfse_numero = @ls_valid_nfse
              AND dtemissao = @i_data_emissao.

          ENDIF.
        ENDIF.
* ----> BUG #179037 - MMSILVA - 13.05.2025 - Fim <----

        IF lw_t_hd-guid_header IS NOT INITIAL.
          IF lw_t_hd IS NOT INITIAL.
            SELECT SINGLE *
              FROM /tcsr/t_act
              INTO @DATA(lw_stepstatus03)
                WHERE guid_header = @lw_t_hd-guid_header.
*              AND last_stepstatus    = '03'. "BUG #179037 - MMSILVA - 13.05.2025 - Comentado devido não ser necessário olhar para o campo STATUS - Acordado com a Leila Mara

            IF lw_stepstatus03-guid_header IS NOT INITIAL.

              SELECT SINGLE en_docnum FROM zlest0034 WHERE guid_header = @lw_t_hd-guid_header INTO @DATA(w_en_docnum).

              IF w_en_docnum IS NOT INITIAL.
                SELECT SINGLE docnum FROM j_1bnfdoc WHERE docnum = @w_en_docnum AND cancel = '' INTO @e_docnum.
              ELSE.
                CLEAR: e_docnum.
              ENDIF.

              CASE i_tipo.
                WHEN 'XML'.
                  SELECT SINGLE xmlstring
            FROM /tcsr/t_xml
            INTO @DATA(lw_nfs_xml)
              WHERE guid_header = @lw_stepstatus03-guid_header.
                  IF sy-subrc EQ 0.
                    out = lw_nfs_xml.
                    "BUG #179037 - MMSILVA - 13.05.2025 - Inicio
                    IF ls_valid_nfse IS INITIAL.
                      e_name = |{ i_numero_nf }_{ i_cnpj }.xml|.
                    ELSE.
                      e_name = |{ ls_valid_nfse }_{ i_cnpj }.xml|.
                    ENDIF.
                    "BUG #179037 - MMSILVA - 13.05.2025 - Fim
                    "e_docnum = i_numero_nf.
                    EXIT.
                  ENDIF.
                WHEN 'PDF'.
                  SELECT SINGLE pdfstring
            FROM /tcsr/t_pdf
            INTO @DATA(lw_nfs_pdf)
              WHERE guid_header = @lw_stepstatus03-guid_header.
                  IF sy-subrc EQ 0.
                    out = lw_nfs_pdf.
                    "BUG #179037 - MMSILVA - 13.05.2025 - Inicio
                    IF ls_valid_nfse IS INITIAL.
                      e_name = |{ i_numero_nf }_{ i_cnpj }.pdf|.
                    ELSE.
                      e_name = |{ ls_valid_nfse }_{ i_cnpj }.pdf|.
                    ENDIF.
                    "BUG #179037 - MMSILVA - 13.05.2025 - Fim
                    "e_docnum = i_numero_nf.
                    EXIT.
                  ENDIF.
                WHEN OTHERS.
              ENDCASE.

            ENDIF.

          ENDIF.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    EXIT.
  ENDIF.

**********************************************************************

  IF i_docnum IS NOT INITIAL.


    SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
      FROM j_1bnfe_active
     WHERE docnum EQ @i_docnum.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
        EXPORTING
          textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno attr1 = CONV #( i_docnum ) )
          msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid
          msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno
          msgv1  = CONV #( i_docnum )
          msgty  = 'E'.

    ENDIF.

    CASE wa_j_1bnfe_active-form.
      WHEN space.
        tp_doc = 'IN'.
      WHEN OTHERS.
        tp_doc = 'OUT'.
    ENDCASE.

    p_chave = wa_j_1bnfe_active-regio &&
              wa_j_1bnfe_active-nfyear &&
              wa_j_1bnfe_active-nfmonth &&
              wa_j_1bnfe_active-stcd1 &&
              wa_j_1bnfe_active-model &&
              wa_j_1bnfe_active-serie &&
              wa_j_1bnfe_active-nfnum9 &&
              wa_j_1bnfe_active-docnum9 &&
              wa_j_1bnfe_active-cdv.

  ELSEIF i_chave IS NOT INITIAL.

    p_chave = i_chave.
    wa_active-regio     = p_chave(2).
    wa_active-nfyear    = p_chave+2(2).
    wa_active-nfmonth   = p_chave+4(2).
    wa_active-stcd1     = p_chave+6(14).
    wa_active-model     = p_chave+20(2).
    wa_active-serie     = p_chave+22(3).
    wa_active-nfnum9    = p_chave+25(9).
    wa_active-docnum9   = p_chave+34(9).
    wa_active-cdv       = p_chave+43(1).

    SELECT SINGLE * INTO @wa_j_1bnfe_active
      FROM j_1bnfe_active AS a
     WHERE regio    EQ @wa_active-regio
       AND nfyear   EQ @wa_active-nfyear
       AND nfmonth  EQ @wa_active-nfmonth
       AND stcd1    EQ @wa_active-stcd1
       AND model    EQ @wa_active-model
       AND serie    EQ @wa_active-serie
       AND nfnum9   EQ @wa_active-nfnum9
       AND docnum9  EQ @wa_active-docnum9
       AND cdv      EQ @wa_active-cdv
       AND cancel   NE @abap_true
       AND form     NE @space
       AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ @abap_true )
       AND NOT EXISTS ( SELECT * FROM ZESZIB_NFE_DIST_TER AS c WHERE c~chave_nfe EQ @i_chave ).

    IF sy-subrc IS NOT INITIAL.

      TRY .
          ZESZCL_DOC_ELETRONICO=>ZESZIF_DOC_ELETRONICO~get_xml_terceiro(
            EXPORTING
              i_chave            = CONV #( i_chave )
            IMPORTING
              e_xml_xstring      = DATA(e_xml_xstring)
              e_xml_string       = DATA(e_xml_string)
            RECEIVING
              r_xml              = DATA(r_xml) ).

          tp_doc = 'IN'.

        CATCH ZESZCX_DOC_ELETRONICO.

          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno attr1 = CONV #( p_chave ) )
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno
              msgv1  = CONV #( p_chave )
              msgty  = 'E'.
      ENDTRY.

    ELSE.
      tp_doc = 'OUT'.
    ENDIF.

    " 29.11.2022 - RAMON - PROJETO NFSE CS2021000988 -->
  ELSEIF i_guid IS NOT INITIAL.

    "Select PDF binary data
    SELECT SINGLE *
      FROM /tcsr/t_pdf
      INTO @DATA(lw_sit_pdf)
        WHERE guid_header = @i_guid.

    IF sy-subrc EQ 0.

      out = lw_sit_pdf-pdfstring.
      e_name = i_guid.

      e_name = e_name && '.pdf'.

      EXIT.

    ENDIF.

  ENDIF.
  " 29.11.2022 - RAMON - PROJETO NFSE CS2021000988 --<

  CASE tp_doc.
    WHEN 'OUT'.

      IF wa_j_1bnfe_active-cancel EQ abap_true.
        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancel-msgid
                              msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancel-msgno
                              attr1 = wa_j_1bnfe_active-nfnum9
                              attr2 = wa_j_1bnfe_active-serie
                              attr3 = wa_j_1bnfe_active-model
                              attr4 = wa_j_1bnfe_active-stcd1
                               )
            msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancel-msgid
            msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cancel-msgno
            msgv1  = CONV #( wa_j_1bnfe_active-nfnum9 )
            msgv2  = CONV #( wa_j_1bnfe_active-serie )
            msgv3  = CONV #( wa_j_1bnfe_active-model )
            msgv4  = CONV #( wa_j_1bnfe_active-stcd1 )
            msgty  = 'E'.

      ENDIF.

      IF i_id_cce IS NOT INITIAL. "Validações Carta de Correção

        SELECT SINGLE *
          FROM zcarta_correcao INTO @DATA(lwa_cce)
         WHERE docnum EQ @i_docnum
           AND id_cc  EQ @i_id_cce.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                attr1 = 'Registro da Carta de Correção não encontrado' )
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
              msgv1  = 'Registro da Carta de Correção não encontrado'
              msgty  = 'E'.
        ENDIF.

        IF lwa_cce-authcode IS INITIAL.
          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                attr1 = 'Carta de Correção não está autorizada' )
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
              msgv1  = 'Carta de Correção não está autorizada'
              msgty  = 'E'.
        ENDIF.

        SELECT *
          FROM zcarta_correcao INTO TABLE @DATA(lit_cce)
         WHERE docnum EQ @i_docnum.

        LOOP AT lit_cce INTO DATA(lwa_cce_ck) WHERE id_cc > lwa_cce-id_cc AND authcode IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                attr1 = 'Carta de Correção já foi substituída!' )
              msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
              msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
              msgv1  = 'Carta de Correção já foi substituída!'
              msgty  = 'E'.
        ENDLOOP.

      ENDIF.

      DATA: t_xml      TYPE  dcxmllines,
            out_binpdf TYPE  xstring,
            out_binxml TYPE  xstring.

      CALL FUNCTION 'ZFSD_BUSCA_DANFE'
        EXPORTING
          i_docnum   = wa_j_1bnfe_active-docnum
          i_chave    = p_chave
          i_tipo     = i_tipo
          i_id_cce   = i_id_cce
        IMPORTING
          t_xml      = t_xml
          out_binpdf = out_binpdf
          out_binxml = out_binxml.

    WHEN 'IN'.

      DATA: i_control_parameters TYPE ssfctrlop,
            i_output_options     TYPE ssfcompop,
            i_job_output_info    TYPE ssfcrescl.

      DATA(lv_model) = p_chave+20(2).
      i_output_options-tdnewid       = 'X'.
      i_output_options-tddelete      = 'X'.
      i_control_parameters-no_dialog = 'X'.
      i_control_parameters-device    = 'PRINTER'.
      i_control_parameters-preview   = ' '.
      i_control_parameters-getotf    = 'X'.

      IF i_chave IS NOT INITIAL.
        SELECT SINGLE docnum
          FROM zib_nfe_forn INTO e_docnum
         WHERE nu_chave EQ i_chave.
      ENDIF.

      CASE i_tipo.
        WHEN 'PDF'.

          DATA: lc_bin_filesize TYPE i,
                it_pdf_data     TYPE TABLE OF tline.

          CASE lv_model.
            WHEN '55'.


              IF e_xml_string IS NOT  INITIAL.
                CALL FUNCTION 'ZESZBRNFE_DANFE'
                  EXPORTING
                    i_xml                = e_xml_string
                    i_control_parameters = i_control_parameters
                    i_output_options     = i_output_options
                  IMPORTING
                    e_job_output_info    = i_job_output_info
                  EXCEPTIONS
                    nfe_nao_aprovada     = 1
                    OTHERS               = 2.


                CALL FUNCTION 'CONVERT_OTF'
                  EXPORTING
                    format                = 'PDF'
                    max_linewidth         = 132
                  IMPORTING
                    bin_filesize          = lc_bin_filesize
                    bin_file              = out_binpdf
                  TABLES
                    otf                   = i_job_output_info-otfdata[]
                    lines                 = it_pdf_data
                  EXCEPTIONS
                    err_max_linewidth     = 1
                    err_format            = 2
                    err_conv_not_possible = 3
                    err_bad_otf           = 4
                    OTHERS                = 5.

              ELSE.
                RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
                  EXPORTING
                    textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                      msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                      attr1 = 'chave nfe não encontrada' )
                    msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                    msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                    msgv1  = 'Chave não encontrada'
                    msgty  = 'E'.

              ENDIF.



            WHEN '57'.

              DATA: lva_par    TYPE syst_subrc,
                    lva_screen TYPE char1,
                    lit_otf    TYPE tsfotf.

              PERFORM entry3 IN PROGRAM zbrcte_dacte_nast_v_copy USING e_xml_string lva_par lva_screen CHANGING lit_otf.
              "  PERFORM entry3 IN PROGRAM zbrcte_dacte_nast USING e_xml_string lva_par lva_screen CHANGING lit_otf.

              IF lit_otf[] IS NOT INITIAL.


                TRY.
                    CALL FUNCTION 'CONVERT_OTF'
                      EXPORTING
                        format                = 'PDF'
                        max_linewidth         = 132
                      IMPORTING
                        bin_filesize          = lc_bin_filesize
                        bin_file              = out_binpdf
                      TABLES
                        otf                   = lit_otf
                        lines                 = it_pdf_data
                      EXCEPTIONS
                        err_max_linewidth     = 1
                        err_format            = 2
                        err_conv_not_possible = 3
                        err_bad_otf           = 4
                        OTHERS                = 5.

                  CATCH  ZESZCX_DOC_ELETRONICO.
                    RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
                      EXPORTING
                        textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid msgno = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno attr1 = CONV #( p_chave ) )
                        msgid  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgid
                        msgno  = ZESZCX_DOC_ELETRONICO=>zcx_doc_cabe_nao_enc-msgno
                        msgv1  = CONV #( p_chave )
                        msgty  = 'E'.
                ENDTRY.

              ENDIF.

          ENDCASE.

        WHEN 'XML'.
          out_binxml = e_xml_xstring.
      ENDCASE.

  ENDCASE.

  CASE i_tipo.
    WHEN 'PDF'.
      out = out_binpdf.
      e_name = zcl_string=>concat( EXPORTING s1 = 'Danfe' s2 = CONV #( p_chave ) sp = '-' ).
      e_name = zcl_string=>concat( EXPORTING s1 = e_name s2 = 'pdf' sp = '.' ).
    WHEN 'XML'.
      out = out_binxml.
      e_name = zcl_string=>concat( EXPORTING s1 = 'NFeXML' s2 = CONV #( p_chave ) sp = '-' ).
      e_name = zcl_string=>concat( EXPORTING s1 = e_name s2 = 'xml' sp = '.' ).
  ENDCASE.

ENDFUNCTION.
