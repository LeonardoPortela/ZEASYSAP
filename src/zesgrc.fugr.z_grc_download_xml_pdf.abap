FUNCTION z_grc_download_xml_pdf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TYPE_FILE) TYPE  Z_GRC_TYPE_FILE
*"  TABLES
*"      T_DOCUMENTOS TYPE  ZSDS0040_T
*"  EXCEPTIONS
*"      ERROR_OPEN_FILE
*"----------------------------------------------------------------------

  FREE: gt_jdoc.

  CHECK t_documentos[] IS NOT INITIAL.

*------------------------------------
*-selecao documentos
*------------------------------------
  SELECT docnum
    FROM j_1bnfdoc
    INTO TABLE gt_jdoc
     FOR ALL ENTRIES IN t_documentos
   WHERE docnum = t_documentos-docnum.

  CHECK gt_jdoc[] IS NOT INITIAL.

*------------------------------------
*-processamento
*------------------------------------
  CASE i_type_file.
    WHEN 'ARQXML' OR 'OPENXML'.
      PERFORM f_download_xml USING i_type_file.
    WHEN 'ARQPDF' OR 'OPENPDF'.
      PERFORM f_download_pdf USING i_type_file.
  ENDCASE.

ENDFUNCTION.
