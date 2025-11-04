*----------------------------------------------------------------------*
***INCLUDE LZGRCF06.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_XML
*&---------------------------------------------------------------------*
FORM f_download_xml USING p_type.

  DATA: selected_folder	TYPE string,
        lt_xml          TYPE TABLE OF char80,
        tamanho         TYPE i.

  IF p_type EQ 'ARQXML'.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Pasta para salvar arquivos XML'
      CHANGING
        selected_folder      = selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4  ).
  ENDIF.

  CHECK sy-subrc IS INITIAL.

  LOOP AT gt_jdoc INTO wa_jdoc.

    CASE p_type.
      WHEN 'ARQXML'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_jdoc-docnum
              )->set_registro( EXPORTING i_docnum              = wa_jdoc-docnum
                                         i_sem_bloqueio        = abap_true
              )->get_xml(      IMPORTING e_xml                 = DATA(e_xml)
              )->get_registro( IMPORTING e_info_doc_eletronico = DATA(e_info_doc_eletronico)
              ).

            DATA(filename) = selected_folder && '\' &&
                             e_info_doc_eletronico-regio &&
                             e_info_doc_eletronico-nfyear &&
                             e_info_doc_eletronico-nfmonth &&
                             e_info_doc_eletronico-stcd1 &&
                             e_info_doc_eletronico-model &&
                             e_info_doc_eletronico-serie &&
                             e_info_doc_eletronico-nfnum9 &&
                             e_info_doc_eletronico-docnum9 &&
                             e_info_doc_eletronico-cdv &&
                             '.xml'.

            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = e_xml
              IMPORTING
                output_length = tamanho
              TABLES
                binary_tab    = lt_xml.

            cl_gui_frontend_services=>gui_download(
              EXPORTING
                bin_filesize              = tamanho
                filename                  = filename
                 filetype                  = 'BIN'
              CHANGING
                data_tab                  = lt_xml
              EXCEPTIONS
                file_write_error          = 1
                no_batch                  = 2
                gui_refuse_filetransfer   = 3
                invalid_type              = 4
                no_authority              = 5
                unknown_error             = 6
                header_not_allowed        = 7
                separator_not_allowed     = 8
                filesize_not_allowed      = 9
                header_too_long           = 10
                dp_error_create           = 11
                dp_error_send             = 12
                dp_error_write            = 13
                unknown_dp_error          = 14
                access_denied             = 15
                dp_out_of_memory          = 16
                disk_full                 = 17
                dp_timeout                = 18
                file_not_found            = 19
                dataprovider_exception    = 20
                control_flush_error       = 21
                not_supported_by_gui      = 22
                error_no_gui              = 23
                OTHERS                    = 24
            ).

            FREE: e_info_doc_eletronico,
                  e_xml,
                  lt_xml.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              RAISE error_open_file.
            ENDIF.

          CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
            RAISE error_open_file.
        ENDTRY.

      WHEN 'OPENXML'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_jdoc-docnum
              )->set_registro(  EXPORTING i_docnum       = wa_jdoc-docnum
                                          i_sem_bloqueio = abap_true
              )->get_urls_docs( IMPORTING e_link_xml     = DATA(e_link_xml)
              ).

            cl_gui_frontend_services=>execute(
              EXPORTING
                document               = e_link_xml
                 operation             = 'OPEN'
              EXCEPTIONS
                cntl_error             = 1
                error_no_gui           = 2
                bad_parameter          = 3
                file_not_found         = 4
                path_not_found         = 5
                file_extension_unknown = 6
                error_execute_failed   = 7
                synchronous_failed     = 8
                not_supported_by_gui   = 9
                OTHERS                 = 10
            ).

            FREE: e_link_xml.

          CATCH zcx_doc_eletronico INTO ex_doc_eletronico.    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
            RAISE error_open_file.
        ENDTRY.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_PSF
*&---------------------------------------------------------------------*
FORM f_download_pdf USING p_type.

  DATA: selected_folder	TYPE string,
        lt_pdf          TYPE TABLE OF char80,
        tamanho         TYPE i.


  IF p_type  EQ 'ARQPDF'.
    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Pasta para salvar arquivos XML'
      CHANGING
        selected_folder      = selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4  ).
  ENDIF.

  CHECK sy-subrc IS INITIAL.

  LOOP AT gt_jdoc INTO wa_jdoc.

    CASE p_type.
      WHEN 'ARQPDF'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_jdoc-docnum
              )->set_registro( EXPORTING i_docnum              = wa_jdoc-docnum
                                         i_sem_bloqueio        = abap_true
              )->get_pdf(      IMPORTING e_pdf                 = DATA(e_pdf)
              )->get_registro( IMPORTING e_info_doc_eletronico = DATA(e_info_doc_eletronico)
              ).

            DATA(filename) = selected_folder && '\' &&
                             e_info_doc_eletronico-regio &&
                             e_info_doc_eletronico-nfyear &&
                             e_info_doc_eletronico-nfmonth &&
                             e_info_doc_eletronico-stcd1 &&
                             e_info_doc_eletronico-model &&
                             e_info_doc_eletronico-serie &&
                             e_info_doc_eletronico-nfnum9 &&
                             e_info_doc_eletronico-docnum9 &&
                             e_info_doc_eletronico-cdv &&
                             '.pdf'.

            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = e_pdf
              IMPORTING
                output_length = tamanho
              TABLES
                binary_tab    = lt_pdf.

            cl_gui_frontend_services=>gui_download(
              EXPORTING
                bin_filesize              = tamanho
                filename                  = filename
                 filetype                  = 'BIN'
              CHANGING
                data_tab                  = lt_pdf
              EXCEPTIONS
                file_write_error          = 1
                no_batch                  = 2
                gui_refuse_filetransfer   = 3
                invalid_type              = 4
                no_authority              = 5
                unknown_error             = 6
                header_not_allowed        = 7
                separator_not_allowed     = 8
                filesize_not_allowed      = 9
                header_too_long           = 10
                dp_error_create           = 11
                dp_error_send             = 12
                dp_error_write            = 13
                unknown_dp_error          = 14
                access_denied             = 15
                dp_out_of_memory          = 16
                disk_full                 = 17
                dp_timeout                = 18
                file_not_found            = 19
                dataprovider_exception    = 20
                control_flush_error       = 21
                not_supported_by_gui      = 22
                error_no_gui              = 23
                OTHERS                    = 24
            ).

            FREE: e_info_doc_eletronico,
                  e_pdf,
                  lt_pdf.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              RAISE error_open_file.
            ENDIF.

          CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
            RAISE error_open_file.
        ENDTRY.

      WHEN 'OPENPDF'.

        TRY .
            zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_jdoc-docnum
              )->set_registro(  EXPORTING i_docnum       = wa_jdoc-docnum
                                          i_sem_bloqueio = abap_true
              )->get_urls_docs( IMPORTING e_link_pdf     = DATA(e_link_pdf)
              ).

            cl_gui_frontend_services=>execute(
              EXPORTING
                document               = e_link_pdf
                 operation              = 'OPEN'
              EXCEPTIONS
                cntl_error             = 1
                error_no_gui           = 2
                bad_parameter          = 3
                file_not_found         = 4
                path_not_found         = 5
                file_extension_unknown = 6
                error_execute_failed   = 7
                synchronous_failed     = 8
                not_supported_by_gui   = 9
                OTHERS                 = 10
            ).

            FREE: e_link_pdf.

          CATCH zcx_doc_eletronico INTO ex_doc_eletronico.    " .
            ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E').
            RAISE error_open_file.
        ENDTRY.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
