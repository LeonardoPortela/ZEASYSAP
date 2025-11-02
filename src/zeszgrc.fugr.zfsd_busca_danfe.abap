FUNCTION zfsd_busca_danfe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_CHAVE) TYPE  ZESZDE_CHAVE_NFE OPTIONAL
*"     REFERENCE(I_TIPO) TYPE  STRING OPTIONAL
*"     REFERENCE(I_ID_CCE) TYPE  NUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(T_XML) TYPE  DCXMLLINES
*"     REFERENCE(OUT_BINPDF) TYPE  XSTRING
*"     REFERENCE(OUT_BINXML) TYPE  XSTRING
*"  RAISING
*"      ZESZCX_DOC_ELETRONICO
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_t685b,
           kappl TYPE t685b-kappl,
           kschl TYPE t685b-kschl,
         END OF ty_t685b,

         BEGIN OF ty_nast,
           kappl TYPE nast-kappl,
           objky TYPE nast-objky,
           kschl TYPE nast-kschl,
         END OF ty_nast.

  RANGES: r_kschl FOR nast-kschl.

  DATA lo_download   TYPE REF TO cl_j_1bnfe_xml_download.
  DATA lo_dom        TYPE REF TO if_ixml_document.
  DATA w_string      TYPE xstring.
  DATA w_size        TYPE i.

  SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc) WHERE docnum EQ @i_docnum.

  IF sy-subrc IS INITIAL.

    IF i_tipo IS INITIAL OR i_tipo = 'PDF'.

      "Ctgs.condição: dados adicionais p/envio de mensagens
      SELECT * FROM t685b INTO TABLE @DATA(lt_t685b) WHERE kappl EQ 'NF'.

      r_kschl-sign   = 'I'.
      r_kschl-option = 'EQ'.
      LOOP AT lt_t685b INTO DATA(wa_t685b).
        r_kschl-low = wa_t685b-kschl.
        APPEND r_kschl.
      ENDLOOP.

      "Determinação da Saída de Mensagem
      "LER TVARVC, ONDE NAME IGUAL ‘ZGRC_MSG_DANFE’ E TYPE IGUAL ‘P’.

      SELECT SINGLE * INTO @DATA(wa_tvarvc)
        FROM tvarvc
       WHERE type EQ 'P'
         AND name EQ 'ZGRC_MSG_DANFE'.

      SELECT SINGLE *
        FROM nast "Status da mensagem
        INTO @DATA(wa_nast)
        WHERE kappl  EQ 'NF'
          AND objky  EQ @wa_doc-docnum
          AND kschl  IN @r_kschl.

      IF sy-subrc IS NOT INITIAL.
        wa_nast-kappl = 'NF'.
        wa_nast-kschl = wa_tvarvc-low.
      ENDIF.

      "Determinação da Impressora Local
      "LER TVARVC, ONDE NAME IGUAL ‘ZGRC_MSG_IMPRE’ E TYPE IGUAL ‘P’.

      wa_nast-objky = wa_doc-docnum.
      wa_nast-spras = sy-langu.
      wa_nast-erdat = sy-datum.
      wa_nast-eruhr = sy-uzeit.
      wa_nast-nacha = 1.
      wa_nast-anzal = 1.
      wa_nast-vsztp = 1.
      wa_nast-ldest = wa_tvarvc-low.
      wa_nast-nauto = 'X'.
      wa_nast-delet = 'X'.

* Programa XXXXXXXXXX para geração da Saída de Mensagem

      DATA: gv_par      TYPE syst_subrc,
            gv_screen   TYPE char1,
            it_otf      TYPE tsfotf,
            it_pdf_data TYPE TABLE OF tline,
            lc_nast     TYPE nast.

      CLEAR: it_otf.

      CASE wa_doc-model.
        WHEN '55'.
          lc_nast-kappl = 'NF'.

          IF i_id_cce IS NOT INITIAL.
            lc_nast-objky = i_docnum && i_id_cce.
            PERFORM fm_entry2 IN PROGRAM zbrcce_nfe_nast USING gv_par gv_screen lc_nast CHANGING it_otf.
          ELSE.
            lc_nast-objky = i_docnum.
            PERFORM entry2 IN PROGRAM ZESZBRNFE_DANFE_NAST USING gv_par gv_screen lc_nast CHANGING it_otf.
          ENDIF.
        WHEN '57'.
          lc_nast-kappl = 'NF'.

          IF i_id_cce IS NOT INITIAL.
            lc_nast-objky = i_docnum && i_id_cce.
            PERFORM fm_entry2 IN PROGRAM zbrcce_cte_nast USING gv_par gv_screen lc_nast CHANGING it_otf.
          ELSE.
            lc_nast-objky = i_docnum.
            "PERFORM ENTRY2 IN PROGRAM ZBRCTE_DACTE_NAST USING GV_PAR GV_SCREEN LC_NAST CHANGING IT_OTF.
            PERFORM entry2 IN PROGRAM zbrcte_dacte_nast_v_copy USING gv_par gv_screen lc_nast CHANGING it_otf. "CS2020000061
          ENDIF.
        WHEN '58'.
          lc_nast-kappl = 'NF'.
          lc_nast-objky = i_docnum.
          PERFORM entry2 IN PROGRAM zbrmdfe_damdfe_nast USING gv_par gv_screen lc_nast CHANGING it_otf.
      ENDCASE.


      IF NOT it_otf[] IS INITIAL.
        DATA: lc_bin_filesize TYPE i.

        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            format                = 'PDF'
            max_linewidth         = 132
          IMPORTING
            bin_filesize          = lc_bin_filesize
            bin_file              = out_binpdf
          TABLES
            otf                   = it_otf[]
            lines                 = it_pdf_data
          EXCEPTIONS
            err_max_linewidth     = 1
            err_format            = 2
            err_conv_not_possible = 3
            err_bad_otf           = 4
            OTHERS                = 5.

        IF sy-subrc IS NOT INITIAL.

          RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
            EXPORTING
              textid = VALUE #( msgid = sy-msgid
                                msgno = sy-msgno
                                attr1 = sy-msgv1
                                attr2 = sy-msgv2
                                attr3 = sy-msgv3
                                attr4 = sy-msgv4  )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4
              msgty  = 'E'.

        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
          EXPORTING
            textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                              msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                              attr1 = 'Não Gerado OTF Smart Form' )
            msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
            msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
            msgv1  = 'Não Gerado OTF Smart Form'
            msgty  = 'E'.



      ENDIF.

    ENDIF.

    IF i_tipo IS INITIAL OR i_tipo = 'XML'.

*** XML NFe
* INICIO - 2000049672 - RRIBEIRO - 01.08.2025 - STEFANINI
    TRY.
* FIM - 2000049672 - RRIBEIRO - 01.08.2025 - STEFANINI
      " Retrieve key information
      SELECT SINGLE * FROM j_1bnfe_active INTO @DATA(gs_active) WHERE docnum EQ @i_docnum.
        IF sy-subrc EQ 0.
          IF gs_active-authcod IS INITIAL.

            RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
              EXPORTING
                textid = VALUE #( msgid = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                                  msgno = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                                  attr1 = 'XML não autorizado' )
                msgid  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgid
                msgno  = ZESZCX_DOC_ELETRONICO=>zcx_erro_geral-msgno
                msgv1  = 'XML não autorizado'
                msgty  = 'E'.


          ELSE.

            DATA: gv_access_key TYPE j_1b_nfe_access_key_dtel44.

            gv_access_key = gs_active-regio &&
                            gs_active-nfyear &&
                            gs_active-nfmonth &&
                            gs_active-stcd1 &&
                            gs_active-model &&
                            gs_active-serie &&
                            gs_active-nfnum9 &&
                            gs_active-docnum9 &&
                            gs_active-cdv.

*   Mesma lógica form get_rfc_destination do include j_1bnfe_monitor_f38
*            DATA: lv_rfcdest       TYPE rfcdest.
*            DATA: lv_xnfeactive    TYPE j_1bxnfeactive.

*            CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
*              EXPORTING
*                i_bukrs      = gs_active-bukrs
*                i_branch     = gs_active-branch
*                i_model      = gs_active-model
*              IMPORTING
*                e_rfcdest    = lv_rfcdest
*                e_xnfeactive = lv_xnfeactive
*              EXCEPTIONS
*                rfc_error    = 1
*                OTHERS       = 2.
*
            " IF sy-subrc IS INITIAL AND lv_xnfeactive = 'X'.




*            DATA: gv_rfcdest TYPE rfcdest.
*            gv_rfcdest = lv_rfcdest.

            " If connection was found
            "IF gv_rfcdest IS NOT INITIAL.
            " Instantiate download object
*                CREATE OBJECT lo_download
*                  EXPORTING
*                    iv_xml_key = gv_access_key
*                    iv_rfc     = gv_rfcdest.

            "Não é o tipo da nota fiscal que defini se é de outbound ou inbound e sim se tem ou não formulário
            "DATA(gv_direct_out) = COND string( WHEN wa_doc-form IS INITIAL THEN 'INBD' ELSE 'OUTB' ).

            CASE wa_doc-model.
              WHEN '55'.
                DATA(c_doctype) = 'NFE'.
              WHEN '57'.
                c_doctype = 'CTE'.
              WHEN '58'.
                c_doctype = 'MFE'.
            ENDCASE.

            " Load XML from GRC and store in the class attributes
*                CALL METHOD lo_download->load_xml_content
*                  EXPORTING
*                    iv_direction = CONV #( gv_direct_out )
*                    iv_doctype   = c_doctype.
*
*                TRY.
*                  DATA(o_xml_content) = lo_download->get_xml_content( ).
*                CATCH zcx_error INTO DATA(ex_erro_xml).
*                ENDTRY.

            DATA(lv_direcao) = COND string( WHEN wa_doc-form IS INITIAL THEN 'IN' ELSE 'OUT' ).

            zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = CONV #( gv_access_key )
                                                                   i_direcao = CONV #( lv_direcao )
                                                         IMPORTING e_xml_raw = DATA(o_xml_content) ).



            IF NOT o_xml_content IS INITIAL.
              " Converts raw string content into DOM object
              CALL FUNCTION 'SDIXML_XML_TO_DOM'
                EXPORTING
                  xml           = o_xml_content
                IMPORTING
                  document      = lo_dom
                EXCEPTIONS
                  invalid_input = 1
                  OTHERS        = 2.

              IF sy-subrc IS NOT INITIAL.

                RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
                  EXPORTING
                    textid = VALUE #( msgid = sy-msgid
                                      msgno = sy-msgno
                                      attr1 = sy-msgv1
                                      attr2 = sy-msgv2
                                      attr3 = sy-msgv3
                                      attr4 = sy-msgv4  )
                    msgid  = sy-msgid
                    msgno  = sy-msgno
                    msgv1  = sy-msgv1
                    msgv2  = sy-msgv2
                    msgv3  = sy-msgv3
                    msgv4  = sy-msgv4
                    msgty  = 'E'.

              ENDIF.

              " Convert DOM to XML doc (table)
              CALL FUNCTION 'SDIXML_DOM_TO_XML'
                EXPORTING
                  document      = lo_dom
                  pretty_print  = ' '
                IMPORTING
                  xml_as_string = out_binxml
                  size          = w_size
                TABLES
                  xml_as_table  = t_xml
                EXCEPTIONS
                  no_document   = 1
                  OTHERS        = 2.

              IF sy-subrc IS NOT INITIAL.

                RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
                  EXPORTING
                    textid = VALUE #( msgid = sy-msgid
                                      msgno = sy-msgno
                                      attr1 = sy-msgv1
                                      attr2 = sy-msgv2
                                      attr3 = sy-msgv3
                                      attr4 = sy-msgv4  )
                    msgid  = sy-msgid
                    msgno  = sy-msgno
                    msgv1  = sy-msgv1
                    msgv2  = sy-msgv2
                    msgv3  = sy-msgv3
                    msgv4  = sy-msgv4
                    msgty  = 'E'.

              ENDIF.

            ENDIF. "NOT o_xml_content IS INITIAL.
            "ENDIF. "gv_rfcdest IS NOT INITIAL.
*            ELSE.
*
*              RAISE EXCEPTION TYPE ZESZCX_DOC_ELETRONICO
*                EXPORTING
*                  textid = VALUE #( msgid = sy-msgid
*                                    msgno = sy-msgno
*                                    attr1 = sy-msgv1
*                                    attr2 = sy-msgv2
*                                    attr3 = sy-msgv3
*                                    attr4 = sy-msgv4  )
*                  msgid  = sy-msgid
*                  msgno  = sy-msgno
*                  msgv1  = sy-msgv1
*                  msgv2  = sy-msgv2
*                  msgv3  = sy-msgv3
*                  msgv4  = sy-msgv4
*                  msgty  = 'E'.
*
*            ENDIF. "sy-subrc IS INITIAL AND lv_xnfeactive = 'X'.

          ENDIF.
        ENDIF. "j_1bnfe_active
* INICIO - 2000049672 - RRIBEIRO - 01.08.2025 - STEFANINI
      CATCH ZESZCX_DOC_ELETRONICO INTO DATA(lx_doc).
      MESSAGE lx_doc->textid TYPE 'E'.
    ENDTRY.
* FIM - 2000049672 - RRIBEIRO - 01.08.2025 - STEFANINI
      ENDIF.
   ENDIF. "j_1bnfdoc

ENDFUNCTION.
