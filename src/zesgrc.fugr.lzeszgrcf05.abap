*----------------------------------------------------------------------*
***INCLUDE LZGRCF05.
*----------------------------------------------------------------------*

FORM F_ATRIB_NFE_ERRO  USING P_MSG_ERRO.

  DATA: WL_ZIB_DFE_ERRO     TYPE ZIB_DFE_ERRO,
        WL_ZIB_AUTORIT_GRC  TYPE ZIB_AUTORIT_GRC.

*------------------------------------------------------------------------------------------------------------*
* Atribuição ZIB_DFE_ERRO
*------------------------------------------------------------------------------------------------------------*

  CLEAR: WL_ZIB_DFE_ERRO.

  WL_ZIB_DFE_ERRO-CHAVE = GS_XML_SEFAZ-NFEPROC-PROTNFE-INFPROT-CHNFE.

  IF GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-EMIT-CNPJ IS NOT INITIAL.
    WL_ZIB_DFE_ERRO-CNPJ_CPF_EMISSOR = GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-EMIT-CNPJ.
  ELSE.
    WL_ZIB_DFE_ERRO-CNPJ_CPF_EMISSOR = GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-EMIT-CPF.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT     = WL_ZIB_DFE_ERRO-CNPJ_CPF_EMISSOR
    IMPORTING
      OUTPUT    = WL_ZIB_DFE_ERRO-CNPJ_CPF_EMISSOR.


  WL_ZIB_DFE_ERRO-MODELO            = GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-IDE-MOD.
  WL_ZIB_DFE_ERRO-SERIE             = GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-IDE-SERIE.
  WL_ZIB_DFE_ERRO-NUMERO            = GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-IDE-NNF.

  PERFORM ZF_GET_DATA_UTC USING GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-IDE-DHEMI CHANGING WL_ZIB_DFE_ERRO-DT_EMISSAO.

  WL_ZIB_DFE_ERRO-IE_EMISSOR        = GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-EMIT-IE.

  PERFORM ZF_BUSCAR_EMPRESA USING GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-DEST-CNPJ
                                  GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-DEST-IE
                                  GS_XML_SEFAZ-NFEPROC-NFE-INFNFE-DEST-INDIEDEST
                         CHANGING WL_ZIB_DFE_ERRO-BUKRS
                                  WL_ZIB_DFE_ERRO-BRANCH.

  WL_ZIB_DFE_ERRO-DS_ERRO = P_MSG_ERRO.

  MODIFY ZIB_DFE_ERRO FROM WL_ZIB_DFE_ERRO.

*------------------------------------------------------------------------------------------------------------*
* Atribuição ZIB_AUTORIT_GRC
*------------------------------------------------------------------------------------------------------------*

  CLEAR: WL_ZIB_AUTORIT_GRC.

  WL_ZIB_AUTORIT_GRC-CHNFE         = GS_XML_SEFAZ-NFEPROC-PROTNFE-INFPROT-CHNFE.
  WL_ZIB_AUTORIT_GRC-DT_REGISTRO   = SY-DATUM.
  WL_ZIB_AUTORIT_GRC-HR_REGISTRO   = SY-UZEIT.
  WL_ZIB_AUTORIT_GRC-US_REGISTRO   = SY-UNAME.
  WL_ZIB_AUTORIT_GRC-RG_ATUALIZADO = '0'.
  WL_ZIB_AUTORIT_GRC-DIRECTION     = 'INBD'.
  WL_ZIB_AUTORIT_GRC-XML_COM_ERRO  = ABAP_TRUE.
  WL_ZIB_AUTORIT_GRC-DS_ERRO       = P_MSG_ERRO.

  MODIFY ZIB_AUTORIT_GRC FROM WL_ZIB_AUTORIT_GRC.

  COMMIT WORK AND WAIT.


ENDFORM.

FORM F_ATRIB_CTE_ERRO  USING P_MSG_ERRO.

  DATA: WL_ZIB_DFE_ERRO TYPE ZIB_DFE_ERRO,
        V_CD_TOMADOR    TYPE C.

*------------------------------------------------------------------------------------------------------------*
* Atribuição ZIB_DFE_ERRO
*------------------------------------------------------------------------------------------------------------*

  CLEAR: WL_ZIB_DFE_ERRO.

  WL_ZIB_DFE_ERRO-DT_EMISSAO        = GWA_CTE_READ-DT_EMISSAO.
  WL_ZIB_DFE_ERRO-IE_EMISSOR        = GWA_CTE_READ-IE_EMISSOR.
  WL_ZIB_DFE_ERRO-CHAVE             = GWA_CTE_READ-CHAVE.
  WL_ZIB_DFE_ERRO-CNPJ_CPF_EMISSOR  = GWA_CTE_READ-CHAVE+6(14).
  WL_ZIB_DFE_ERRO-MODELO            = GWA_CTE_READ-CHAVE+20(02).
  WL_ZIB_DFE_ERRO-SERIE             = GWA_CTE_READ-CHAVE+22(03).
  WL_ZIB_DFE_ERRO-NUMERO            = GWA_CTE_READ-CHAVE+25(09).

  PERFORM ZF_GET_DADOS_TOMADOR CHANGING WL_ZIB_DFE_ERRO-BUKRS
                                        WL_ZIB_DFE_ERRO-BRANCH
                                        V_CD_TOMADOR.

  WL_ZIB_DFE_ERRO-DS_ERRO = P_MSG_ERRO.

  MODIFY ZIB_DFE_ERRO FROM WL_ZIB_DFE_ERRO.

ENDFORM.
