*&---------------------------------------------------------------------*
*& Report  ZESZGRC_SEND_EMAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZESZGRC_SEND_EMAIL MESSAGE-ID ZESZGRC.

TABLES: j_1bnfdoc.

SELECTION-SCREEN BEGIN OF BLOCK nfedata WITH FRAME TITLE text-001.
SELECT-OPTIONS: pdocnum FOR j_1bnfdoc-docnum,
                pnfnum9 FOR j_1bnfdoc-nfnum,
                pbukrs  FOR j_1bnfdoc-bukrs NO INTERVALS NO-EXTENSION MEMORY ID buk,
                pstdat  FOR j_1bnfdoc-pstdat ,
                pdirect FOR j_1bnfdoc-direct OBLIGATORY DEFAULT '2'.
SELECTION-SCREEN END OF BLOCK nfedata.

SELECTION-SCREEN BEGIN OF BLOCK nfeenvio WITH FRAME TITLE text-002.
PARAMETERS: pemail TYPE ad_smtpadr.
SELECTION-SCREEN END OF BLOCK nfeenvio.

PARAMETERS: pbatch TYPE sy-batch NO-DISPLAY.

TYPES BEGIN OF ty_link.
TYPES: docnum TYPE j_1bdocnum.
TYPES: reftyp TYPE j_1breftyp.
TYPES: link   TYPE char10.
TYPES: posnr  TYPE posnr_vf.
TYPES END OF ty_link.

TYPES BEGIN OF ty_parceiro.
TYPES: codigo TYPE lifnr.
TYPES END OF ty_parceiro.

DATA: it_link_bi TYPE TABLE OF ty_link.
DATA: it_link_zw TYPE TABLE OF ty_link.
DATA: it_link_ct TYPE TABLE OF ty_link.
DATA: it_parceiro TYPE TABLE OF ty_parceiro.
DATA: wa_link TYPE ty_link.
DATA: wa_parceiro TYPE ty_parceiro.

INITIALIZATION.

  IF sy-batch EQ abap_true.

    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.

  ENDIF.

  pbatch = sy-batch.

  SELECT SINGLE *
    FROM setleaf
    INTO @DATA(wa_setleaf)
   WHERE setname = 'MAGGI_GRC_SEND_MAIL'.

  IF sy-subrc IS INITIAL.
    DATA(ck_send_mail_grc) = abap_true.
  ELSE.
    ck_send_mail_grc = abap_false.
  ENDIF.

AT SELECTION-SCREEN.

*  IF SY-BATCH EQ ABAP_FALSE AND PDOCNUM IS INITIAL AND PNFNUM9 IS INITIAL.
*    SET CURSOR FIELD 'PDOCNUM-LOW'.
*    MESSAGE E002.
*  ENDIF.
*
*  IF SY-BATCH EQ ABAP_FALSE AND PNFNUM9 IS NOT INITIAL.
*    SET CURSOR FIELD 'PNFNUM9-LOW'.
*    MESSAGE E003.
*  ENDIF.

START-OF-SELECTION.

  DATA: lc_out       TYPE char01,
        lc_doc       TYPE j_1bnfdoc-docnum,
        lc_email     TYPE ad_smtpadr,
        lc_batch     TYPE syst_batch,
        lc_user_mail TYPE uname,
        lc_tabix     TYPE sy-tabix.

  lc_batch = pbatch.
  lc_user_mail = zcl_job=>get_user_job( ).

  PERFORM envia_email USING lc_doc lc_email lc_batch CHANGING lc_out.

  CLEAR: lc_doc, lc_email, lc_out, lc_batch.

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_POUTRODOC  text
*      <--P_OUTENVIADO  text
*----------------------------------------------------------------------*
FORM envia_email USING p_poutrodoc TYPE j_1bdocnum lc_email TYPE ad_smtpadr lc_batch TYPE syst_batch CHANGING p_outenviado TYPE char01.

  IF lc_batch EQ abap_true.
    pbatch = abap_true.
  ENDIF.

  IF lc_email IS NOT INITIAL.
    pemail = lc_email.
  ENDIF.

  IF p_poutrodoc IS NOT INITIAL.
    pdocnum[] = VALUE #( option = 'EQ' sign = 'I' ( low = p_poutrodoc high = p_poutrodoc  ) ).
  ENDIF.

  p_outenviado = abap_false.

  IF sy-batch EQ abap_true.

    CLEAR: pstdat[].

    DATA: e_data_limite TYPE datum.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 15
        months    = 0
        years     = 0
        signum    = '-'
      IMPORTING
        calc_date = e_data_limite.

    IF e_data_limite LT '20190924'.
      e_data_limite = '20190924'.
    ENDIF.

    pstdat[] = VALUE #( option = 'GE' sign = 'I' ( low = e_data_limite high = e_data_limite  ) ).

    SELECT j~docnum,
           j~nfenum,
           j~parvw,
           j~parid,
           j~bukrs,
           j~branch,
           j~crenam,
           j~chanam,
           j~model
      INTO TABLE @DATA(it_j_1bnfdoc)
      FROM j_1bnfdoc AS j
     WHERE j~docnum IN @pdocnum
       AND j~nfenum IN @pnfnum9
       AND j~bukrs  IN @pbukrs
       AND j~credat IN @pstdat
       AND j~form   NE @space
       AND j~model  EQ '55'
       AND NOT EXISTS ( SELECT * FROM zsdt0230 AS n WHERE n~docnum EQ j~docnum )
       AND EXISTS ( SELECT * FROM ZESZIB_NFE AS i WHERE i~docnum EQ j~docnum ).

  ELSE.

    SELECT j~docnum,
           j~nfenum,
           j~parvw,
           j~parid,
           j~bukrs,
           j~branch,
           j~crenam,
           j~chanam,
           j~model
      INTO TABLE @it_j_1bnfdoc
      FROM j_1bnfdoc AS j
     WHERE docnum IN @pdocnum
       AND direct IN @pdirect
       AND nfenum IN @pnfnum9
       AND bukrs  IN @pbukrs
       AND pstdat IN @pstdat
       AND form   NE @space
       AND model  EQ '55'
       AND EXISTS ( SELECT * FROM ZESZIB_NFE AS i WHERE i~docnum EQ j~docnum ).

  ENDIF.

  CHECK it_j_1bnfdoc[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_j_1bad_filial)
    FROM j_1bad
   WHERE partyp EQ 'B'.

  SELECT * INTO TABLE @DATA(it_j_1bad_fornecedor)
    FROM j_1bad
   WHERE partyp EQ 'V'.

  SELECT * INTO TABLE @DATA(it_j_1bad_cliente)
    FROM j_1bad
   WHERE partyp EQ 'C'.

  SELECT * INTO TABLE @DATA(it_zib_nfe)
    FROM ZESZIB_NFE
     FOR ALL ENTRIES IN @it_j_1bnfdoc
   WHERE docnum EQ @it_j_1bnfdoc-docnum.

  LOOP AT it_zib_nfe INTO DATA(wa_zib_nfe) WHERE ds_url_danfe CS 'SIMETRYA'.
    DELETE it_j_1bnfdoc WHERE docnum EQ wa_zib_nfe-docnum.
  ENDLOOP.

  SELECT docnum, reftyp, refkey, refitm
    INTO TABLE @DATA(it_j_1bnflin)
    FROM j_1bnflin
     FOR ALL ENTRIES IN @it_j_1bnfdoc
   WHERE docnum EQ @it_j_1bnfdoc-docnum.

  DATA(it_j_1bnflin_aux) = it_j_1bnflin[].
  DELETE it_j_1bnflin_aux WHERE reftyp NE 'BI'.

  CLEAR: it_link_bi[], it_link_zw[], it_parceiro[].

  IF it_j_1bnflin_aux[] IS NOT INITIAL.

    LOOP AT it_j_1bnflin_aux INTO DATA(wa_j_1bnflin_aux).
      CLEAR: wa_link.
      wa_link-docnum = wa_j_1bnflin_aux-docnum.
      wa_link-link   = wa_j_1bnflin_aux-refkey(10).
      wa_link-reftyp = wa_j_1bnflin_aux-reftyp.
      APPEND wa_link TO it_link_bi.
    ENDLOOP.

    DATA: rgparvw TYPE RANGE OF parvw.
    rgparvw[] = VALUE #( sign = 'I' option = 'EQ'
                          ( low = 'WE' high = 'WE' )
                          ( low = 'LR' high = 'LR' )
                          ( low = 'Z1' high = 'Z1' )
                          ( low = 'T1' high = 'T1' )
                          ( low = 'T2' high = 'T2' )
                          ( low = 'T3' high = 'T3' )
                          ( low = 'T4' high = 'T4' )
                          ( low = 'SP' high = 'SP' ) "CS2020000850
                        ).

    SELECT * INTO TABLE @DATA(it_vbpa)
      FROM vbpa
       FOR ALL ENTRIES IN @it_link_bi
     WHERE vbeln EQ @it_link_bi-link
       AND parvw IN @rgparvw.

*** CS2020000850 inicio
    SELECT * INTO TABLE @DATA(it_vbrp_aux)
     FROM vbrp
      FOR ALL ENTRIES IN @it_link_bi
    WHERE vbeln EQ @it_link_bi-link.

    SELECT *
      APPENDING TABLE @it_vbpa
      FROM vbpa
       FOR ALL ENTRIES IN @it_vbrp_aux
     WHERE vbeln EQ @it_vbrp_aux-vgbel
       AND parvw IN @rgparvw.

    SELECT * INTO TABLE @DATA(it_vbfa)
      FROM vbfa
       FOR ALL ENTRIES IN @it_vbrp_aux
      WHERE vbelv EQ @it_vbrp_aux-vgbel
       AND vbtyp_n = 'M'
       AND vbtyp_v = 'J'.

    LOOP AT it_vbpa INTO DATA(wa_vbpa_aux) WHERE parvw EQ 'SP'.
      lc_tabix = sy-tabix.
      READ TABLE it_vbfa INTO DATA(wa_vbfa) WITH KEY vbelv = wa_vbpa_aux-vbeln.
      IF sy-subrc = 0.
        IF wa_vbpa_aux-vbeln <> wa_vbfa-vbeln.
          wa_vbpa_aux-vbeln = wa_vbfa-vbeln.
          MODIFY it_vbpa FROM wa_vbpa_aux INDEX lc_tabix TRANSPORTING vbeln.
        ENDIF.
      ENDIF.
    ENDLOOP.

*** CS2020000850 fim

  ENDIF.

  it_j_1bnflin_aux = it_j_1bnflin[].
  DELETE it_j_1bnflin_aux WHERE reftyp NE 'ZW'.

  IF it_j_1bnflin_aux[] IS NOT INITIAL.

    LOOP AT it_j_1bnflin_aux INTO wa_j_1bnflin_aux.
      CLEAR: wa_link.
      wa_link-docnum = wa_j_1bnflin_aux-docnum.
      wa_link-link   = wa_j_1bnflin_aux-refkey(10).
      wa_link-reftyp = wa_j_1bnflin_aux-reftyp.
      APPEND wa_link TO it_link_zw.
    ENDLOOP.

    rgparvw[] = VALUE #( sign = 'I' option = 'EQ' ( low = 'WE' high = 'WE' ) ( low = 'LR' high = 'LR' ) ( low = 'Z1' high = 'Z1' ) ).

    SELECT * INTO TABLE @DATA(it_zfiwrt0015)
      FROM zfiwrt0015
       FOR ALL ENTRIES IN @it_link_zw
     WHERE seq_lcto EQ @it_link_zw-link
       AND parvw    IN @rgparvw.

  ENDIF.

  it_j_1bnflin_aux = it_j_1bnflin[].
  DELETE it_j_1bnflin_aux WHERE reftyp NE space.

  IF it_j_1bnflin_aux[] IS NOT INITIAL.

    rgparvw[] = VALUE #( sign = 'I' option = 'EQ' ( low = 'WE' high = 'WE' ) ( low = 'LR' high = 'LR' ) ( low = 'Z1' high = 'Z1' ) ).

    SELECT * INTO TABLE @DATA(it_j_1bnfnad)
      FROM j_1bnfnad
       FOR ALL ENTRIES IN @it_j_1bnflin_aux
     WHERE docnum EQ @it_j_1bnflin_aux-docnum
       AND parvw  IN @rgparvw.

  ENDIF.

  SORT it_j_1bnflin BY docnum.
  DELETE ADJACENT DUPLICATES FROM it_j_1bnflin COMPARING docnum.

  "Procurar Parceiros da CT-e """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "Procurar Parceiros da CT-e """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "Procurar Parceiros da CT-e """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DATA(it_j_1bnfdoc_cte) = it_j_1bnfdoc[].
  DELETE it_j_1bnfdoc_cte WHERE model NE '57'.

  it_j_1bnflin_aux = it_j_1bnflin[].
  DELETE it_j_1bnflin_aux WHERE reftyp EQ space.

  IF it_j_1bnfdoc_cte[] IS NOT INITIAL.

    LOOP AT it_j_1bnfdoc_cte INTO DATA(wa_j_1bnfdoc_cte).
      LOOP AT it_j_1bnflin_aux INTO wa_j_1bnflin_aux WHERE docnum EQ wa_j_1bnfdoc_cte-docnum.
        CLEAR: wa_link.
        wa_link-docnum = wa_j_1bnflin_aux-docnum.
        wa_link-link   = wa_j_1bnflin_aux-refkey(10).
        wa_link-reftyp = wa_j_1bnflin_aux-reftyp.
        wa_link-posnr  = wa_j_1bnflin_aux-refitm.
        APPEND wa_link TO it_link_ct.
      ENDLOOP.
    ENDLOOP.

    IF it_link_ct[] IS NOT INITIAL.

      "Fatura do Serviço
      SELECT * INTO TABLE @DATA(it_vbrp)
        FROM vbrp
         FOR ALL ENTRIES IN @it_link_ct
       WHERE vbeln EQ @it_link_ct-link
         AND posnr EQ @it_link_ct-posnr.

      IF it_vbrp[] IS NOT INITIAL.

        "Ordem de Venda
        SELECT * INTO TABLE @DATA(it_vbak)
          FROM vbak
           FOR ALL ENTRIES IN @it_vbrp
         WHERE vbeln EQ @it_vbrp-aubel.

        DELETE it_vbak WHERE tknum EQ space.

        IF it_vbak[] IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(it_vtpa)
            FROM vtpa
             FOR ALL ENTRIES IN @it_vbak
           WHERE vbeln EQ @it_vbak-tknum.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  IF pemail IS INITIAL.

    LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_doc>) WHERE parvw EQ 'BR'.
      <fs_doc>-parid = <fs_doc>-parid+4(4).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_doc>-parid
        IMPORTING
          output = <fs_doc>-parid.
    ENDLOOP.

    "Fornecedor """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(it_j_1bnfdoc_aux) = it_j_1bnfdoc[].

    LOOP AT it_j_1bad_cliente INTO DATA(wa_j_1bad_cliente).
      DELETE it_j_1bnfdoc_aux WHERE parvw EQ wa_j_1bad_cliente-parvw.
    ENDLOOP.

*    LOOP AT IT_J_1BAD_FILIAL INTO DATA(WA_J_1BAD_FILIAL).
*      DELETE IT_J_1BNFDOC_AUX WHERE PARVW EQ WA_J_1BAD_FILIAL-PARVW.
*    ENDLOOP.

    IF it_j_1bnfdoc_aux[] IS NOT INITIAL.
      SELECT lifnr, adrnr, ktokk
        INTO TABLE @DATA(it_lfa1)
        FROM lfa1
         FOR ALL ENTRIES IN @it_j_1bnfdoc_aux
       WHERE lifnr EQ @it_j_1bnfdoc_aux-parid.
    ENDIF.

    "Cliente """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    it_j_1bnfdoc_aux = it_j_1bnfdoc[].

    LOOP AT it_j_1bad_fornecedor INTO DATA(wa_j_1bad_fornecedor).
      DELETE it_j_1bnfdoc_aux WHERE parvw EQ wa_j_1bad_fornecedor-parvw.
    ENDLOOP.

    LOOP AT it_j_1bad_filial INTO DATA(wa_j_1bad_filial).
      DELETE it_j_1bnfdoc_aux WHERE parvw EQ wa_j_1bad_filial-parvw.
    ENDLOOP.

    IF it_j_1bnfdoc_aux[] IS NOT INITIAL.
      SELECT kunnr, adrnr, ktokd
        INTO TABLE @DATA(it_kna1)
        FROM kna1
         FOR ALL ENTRIES IN @it_j_1bnfdoc_aux
       WHERE kunnr EQ @it_j_1bnfdoc_aux-parid.
    ENDIF.

*    IT_J_1BNFDOC_AUX = IT_J_1BNFDOC[].
*    DELETE IT_J_1BNFDOC_AUX WHERE PARVW NE 'AG'.
*
*    IF IT_J_1BNFDOC_AUX[] IS NOT INITIAL.
*      SELECT KUNNR, ADRNR, KTOKD
*        APPENDING TABLE @IT_KNA1
*        FROM KNA1
*         FOR ALL ENTRIES IN @IT_J_1BNFDOC_AUX
*       WHERE KUNNR EQ @IT_J_1BNFDOC_AUX-PARID.
*    ENDIF.

    "" Busca de Clientes
    LOOP AT it_zfiwrt0015 INTO DATA(wa_zfiwrt0015) WHERE parvw EQ 'WE' OR parvw EQ 'LR'.
      CLEAR: wa_parceiro.
      wa_parceiro-codigo = wa_zfiwrt0015-parid.
      APPEND wa_parceiro TO it_parceiro.
    ENDLOOP.

    LOOP AT it_vbpa INTO DATA(wa_vbpa) WHERE parvw EQ 'WE' OR parvw EQ 'LR' OR parvw EQ 'T3' OR parvw EQ 'T4'.
      CLEAR: wa_parceiro.
      wa_parceiro-codigo = wa_vbpa-kunnr.
      APPEND wa_parceiro TO it_parceiro.
    ENDLOOP.

    LOOP AT it_vtpa INTO DATA(wa_vtpa) WHERE parvw EQ 'LR' OR parvw EQ 'SG'.
      CLEAR: wa_parceiro.
      wa_parceiro-codigo = wa_vtpa-kunnr.
      APPEND wa_parceiro TO it_parceiro.
    ENDLOOP.

    SORT it_parceiro BY codigo.
    DELETE ADJACENT DUPLICATES FROM it_parceiro COMPARING codigo.

    IF it_parceiro[] IS NOT INITIAL.
      SELECT kunnr, adrnr, ktokd
        APPENDING TABLE @it_kna1
        FROM kna1
         FOR ALL ENTRIES IN @it_parceiro
       WHERE kunnr EQ @it_parceiro-codigo.
    ENDIF.

    CLEAR: it_parceiro[].

    "" Busca de Fornecedor
    LOOP AT it_zfiwrt0015 INTO wa_zfiwrt0015 WHERE parvw EQ 'Z1'.
      CLEAR: wa_parceiro.
      wa_parceiro-codigo = wa_zfiwrt0015-parid.
      APPEND wa_parceiro TO it_parceiro.
    ENDLOOP.

    LOOP AT it_vbpa INTO wa_vbpa WHERE parvw EQ 'Z1' OR parvw EQ 'T1' OR parvw EQ 'T2' OR parvw EQ 'SP'.
      CLEAR: wa_parceiro.
      wa_parceiro-codigo = wa_vbpa-lifnr.
      APPEND wa_parceiro TO it_parceiro.
    ENDLOOP.

    LOOP AT it_vtpa INTO wa_vtpa WHERE parvw EQ 'MT' OR parvw EQ 'PC' OR parvw EQ 'PV' OR parvw EQ 'SP'.
      CLEAR: wa_parceiro.
      wa_parceiro-codigo = wa_vtpa-lifnr.
      APPEND wa_parceiro TO it_parceiro.
    ENDLOOP.

    SORT it_parceiro BY codigo.
    DELETE ADJACENT DUPLICATES FROM it_parceiro COMPARING codigo.

    IF it_parceiro[] IS NOT INITIAL.
      SELECT lifnr, adrnr, ktokk
        APPENDING TABLE @it_lfa1
        FROM lfa1
         FOR ALL ENTRIES IN @it_parceiro
       WHERE lifnr EQ @it_parceiro-codigo.
    ENDIF.

    DATA(it_j_1bnfnad_c) = it_j_1bnfnad[].
    DELETE it_j_1bnfnad_c WHERE partyp NE 'C'.
    IF it_j_1bnfnad_c[] IS NOT INITIAL.
      SELECT kunnr, adrnr
        APPENDING TABLE @it_kna1
        FROM kna1
         FOR ALL ENTRIES IN @it_j_1bnfnad_c
       WHERE kunnr EQ @it_j_1bnfnad_c-parid.
    ENDIF.

    DATA(it_j_1bnfnad_v) = it_j_1bnfnad[].
    DELETE it_j_1bnfnad_v WHERE partyp NE 'V'.
    IF it_j_1bnfnad_v[] IS NOT INITIAL.
      SELECT kunnr, adrnr
        APPENDING TABLE @it_lfa1
        FROM lfa1
         FOR ALL ENTRIES IN @it_j_1bnfnad_v
       WHERE lifnr EQ @it_j_1bnfnad_v-parid.
    ENDIF.

    IF it_lfa1[] IS NOT INITIAL.
      SELECT addrnumber,
             persnumber,
             date_from,
             smtp_addr
        INTO TABLE @DATA(it_adr6)
        FROM adr6
         FOR ALL ENTRIES IN @it_lfa1
       WHERE addrnumber  EQ @it_lfa1-adrnr
         AND date_from   LE @sy-datum.
    ENDIF.

    IF it_kna1[] IS NOT INITIAL.
      SELECT addrnumber,
             persnumber,
             date_from,
             smtp_addr
        APPENDING TABLE @it_adr6
        FROM adr6
         FOR ALL ENTRIES IN @it_kna1
       WHERE addrnumber  EQ @it_kna1-adrnr
         AND date_from   LE @sy-datum.
    ENDIF.

  ENDIF.

  SORT it_kna1 BY kunnr.
  SORT it_lfa1 BY lifnr.

  IF it_j_1bnfdoc[] IS INITIAL.
    MESSAGE s004 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  TYPES BEGIN OF ty_email.
  TYPES: email TYPE ad_smtpadr.
  TYPES END OF ty_email.

  DATA: gt_email TYPE TABLE OF ty_email.

  DATA: lt_contents   TYPE soli_tab,
        ls_contents   LIKE LINE OF lt_contents,
        lv_subject    TYPE so_obj_des,
        lv_binpdf     TYPE xstring,
        lv_binxml     TYPE xstring,
*        LO_DOCUMENT   TYPE REF TO CL_DOCUMENT_BCS,
*        WO_DOCUMENT   TYPE REF TO CL_BCS,
        lv_filename_d TYPE sood-objdes,
        lv_filename_x TYPE sood-objdes,
        lt_xml        TYPE dcxmllines,
        lt_pdf        TYPE TABLE OF tline,
        lt_attach_bin TYPE solix_tab,
        gr_recipient  TYPE REF TO if_recipient_bcs,
        lt_sval       TYPE TABLE OF sval,
        ls_sval       TYPE sval,
        lv_retcode    TYPE c,
        lv_len        TYPE i,
        lv_idx        TYPE i.

  DATA: lt_attach_txt  TYPE soli_tab,
        ls_content_txt TYPE soli.

  CHECK NOT it_j_1bnfdoc[] IS INITIAL.

  SELECT * INTO TABLE @DATA(it_j_1bbranch)
    FROM j_1bbranch
     FOR ALL ENTRIES IN @it_j_1bnfdoc
   WHERE bukrs  EQ @it_j_1bnfdoc-bukrs
     AND branch EQ @it_j_1bnfdoc-branch.

  SORT it_j_1bbranch BY bukrs branch.

  SELECT *
    INTO TABLE @DATA(it_adrc_branch)
    FROM adrc
     FOR ALL ENTRIES IN @it_j_1bbranch
   WHERE addrnumber  EQ @it_j_1bbranch-adrnr
     AND date_from   LE @sy-datum.

  SORT it_adrc_branch BY addrnumber.

  DATA: lv_rfcdest TYPE  rfcdest.

  LOOP AT it_j_1bnfdoc INTO DATA(ls_bnfdoc).

    CLEAR: lt_contents[], ls_contents.
    IF lv_rfcdest IS INITIAL.

      CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
        EXPORTING
          i_bukrs   = ls_bnfdoc-bukrs
          i_branch  = ls_bnfdoc-branch
          i_model   = ls_bnfdoc-model
        IMPORTING
          e_rfcdest = lv_rfcdest
        EXCEPTIONS
          rfc_error = 1
          OTHERS    = 2.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

    ENDIF.

    CASE ls_bnfdoc-model.
      WHEN '55'.
        lv_subject = 'DANFE E XML'. "Assunto
      WHEN '57'.
        lv_subject = 'DACTE E XML'. "Assunto
    ENDCASE.

    CASE sy-sysid.
      WHEN 'DEV'.
        ls_contents-line = 'Ambiente de Desenvolvimento'. "Body do E-mail
        APPEND ls_contents TO lt_contents.
        lv_subject = zcl_string=>concat( EXPORTING s1 = CONV #( lv_subject ) s2 = CONV #( ls_contents-line ) sp = ' - ' ) .
      WHEN 'QAS'.
        ls_contents-line = 'Ambiente de Homologação'. "Body do E-mail
        APPEND ls_contents TO lt_contents.
        lv_subject = zcl_string=>concat( EXPORTING s1 = CONV #( lv_subject ) s2 = CONV #( ls_contents-line ) sp = ' - ' ).
    ENDCASE.

    SELECT SINGLE * FROM j_1bnfe_active INTO @DATA(gs_active) WHERE docnum EQ @ls_bnfdoc-docnum.

    IF sy-subrc IS NOT INITIAL OR gs_active-docsta NE '1'.
      CONTINUE.
    ENDIF.

    DATA(gv_access_key) = gs_active-regio &&
                            gs_active-nfyear &&
                            gs_active-nfmonth &&
                            gs_active-stcd1 &&
                            gs_active-model &&
                            gs_active-serie &&
                            gs_active-nfnum9 &&
                            gs_active-docnum9 &&
                            gs_active-cdv.

*Anexo a este e-mail encontram-se a Nota Fiscal Eletrônica (NFe) e o Documento Auxiliar da Nota Fiscal Eletrônica (DANFe) de Chave de
*Acesso nº  51190800315457000780550000001227921594323144 .
*Atenciosamente,
*AGROPECUARIA MAGGI LTDA
*00.315.457/0007-80

    CASE ls_bnfdoc-model.
      WHEN '55'.
        ls_contents-line = 'Anexo a este e-mail encontram-se a Nota Fiscal Eletrônica (NFe) e o Documento Auxiliar da Nota Fiscal Eletrônica (DANFe) de Chave:' &&
        cl_abap_char_utilities=>cr_lf.
      WHEN '57'.
        ls_contents-line = 'Anexo a este e-mail encontram-se O Conhecimento de Transporte Eletrônico (CTe) e o Documento Auxiliar do Conhecimento de Transporte Eetrônico (DACTe) de Chave:' &&
        cl_abap_char_utilities=>cr_lf.
    ENDCASE.

    APPEND ls_contents TO lt_contents.
    ls_contents-line = gv_access_key && cl_abap_char_utilities=>cr_lf.
    APPEND ls_contents TO lt_contents.
    CLEAR ls_contents-line.
    APPEND ls_contents TO lt_contents.

    ls_contents-line = 'Atenciosamente,' && cl_abap_char_utilities=>cr_lf.
    APPEND ls_contents TO lt_contents.

    READ TABLE it_j_1bbranch INTO DATA(wa_j_1bbranch) WITH KEY bukrs = ls_bnfdoc-bukrs branch = ls_bnfdoc-branch BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE it_adrc_branch INTO DATA(wa_adrc_branch) WITH KEY addrnumber = wa_j_1bbranch-adrnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_contents-line = wa_adrc_branch-name1 && cl_abap_char_utilities=>cr_lf.
        APPEND ls_contents TO lt_contents.

        DATA: lc_cnpj TYPE c LENGTH 18.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_j_1bbranch-stcd1
          IMPORTING
            output = lc_cnpj.

        ls_contents-line = lc_cnpj && cl_abap_char_utilities=>cr_lf.
        APPEND ls_contents TO lt_contents.
      ENDIF.
    ENDIF.

    REFRESH: gt_email, gt_email[].

*Se ls_bnfdoc-parid PARVW diferente ‘SP’. Faça.
    IF pemail IS INITIAL.
      IF ls_bnfdoc-parvw NE 'SP' AND ls_bnfdoc-parvw NE 'BR' AND ls_bnfdoc-parvw NE 'LF'.
        READ TABLE it_kna1 WITH KEY kunnr = ls_bnfdoc-parid INTO DATA(wa_kna1) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT it_adr6 INTO DATA(wa_adr6) WHERE addrnumber EQ wa_kna1-adrnr AND smtp_addr IS NOT INITIAL.
            APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
          ENDLOOP.
        ENDIF.
      ELSE.
        READ TABLE it_lfa1 WITH KEY lifnr = ls_bnfdoc-parid INTO DATA(wa_lfa1) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_lfa1-adrnr AND smtp_addr IS NOT INITIAL.
            APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
          ENDLOOP.
        ENDIF.
      ENDIF.

      READ TABLE it_j_1bnflin WITH KEY docnum = ls_bnfdoc-docnum INTO wa_j_1bnflin_aux BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CASE wa_j_1bnflin_aux-reftyp.
          WHEN 'BI'.
            wa_link-link = wa_j_1bnflin_aux-refkey(10).
            LOOP AT it_vbpa INTO wa_vbpa WHERE vbeln EQ wa_link-link.
              CASE wa_vbpa-parvw.

                WHEN 'WE' OR 'LR' OR 'T3' OR 'T4'.
                  READ TABLE it_kna1 WITH KEY kunnr = wa_vbpa-kunnr INTO wa_kna1 BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_kna1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.

                WHEN 'Z1' OR 'T1' OR 'T2'.
                  READ TABLE it_lfa1 WITH KEY lifnr = wa_vbpa-lifnr INTO wa_lfa1 BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_lfa1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.
*** CS2020000850 Inicio
                WHEN 'SP'.
                  READ TABLE it_lfa1 WITH KEY lifnr = wa_vbpa-lifnr INTO wa_lfa1 BINARY SEARCH.
                  IF ( sy-subrc IS INITIAL ) AND ( wa_lfa1-ktokk <> 'ZFIC' ).
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_lfa1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.
*** CS2020000850 Fim
              ENDCASE.
            ENDLOOP.

            IF ls_bnfdoc-model EQ '57'.
              wa_link-link   = wa_j_1bnflin_aux-refkey(10).
              wa_link-posnr  = wa_j_1bnflin_aux-refitm.
              "Fatura
              LOOP AT it_vbrp INTO DATA(wa_vbrp)
                  WHERE vbeln EQ wa_link-link
                    AND posnr EQ wa_link-posnr.
                "Ordem de Venda
                LOOP AT it_vbak INTO DATA(wa_vbak) WHERE vbeln EQ wa_vbrp-aubel.
                  LOOP AT it_vtpa INTO wa_vtpa WHERE vbeln EQ wa_vbak-tknum.
                    CASE wa_vtpa-parvw.
                      WHEN 'LR' OR 'SG'.
                        READ TABLE it_kna1 WITH KEY kunnr = wa_vtpa-kunnr INTO wa_kna1 BINARY SEARCH.
                        IF sy-subrc IS INITIAL.
                          LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_kna1-adrnr AND smtp_addr IS NOT INITIAL.
                            APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                          ENDLOOP.
                        ENDIF.

                        IF wa_kna1-ktokd EQ 'ZCIC'.
                          CASE sy-sysid.
                            WHEN 'QAS'.
                              APPEND VALUE #( email = 'cte.homolog@amaggi.com.br' ) TO gt_email.
                            WHEN 'PRD'.
                              APPEND VALUE #( email = 'cte.fiscal@amaggi.com.br' ) TO gt_email.
                          ENDCASE.
                        ENDIF.

                      WHEN 'MT' OR 'PC' OR 'PV' OR 'SP'.
                        READ TABLE it_lfa1 WITH KEY lifnr = wa_vtpa-lifnr INTO wa_lfa1 BINARY SEARCH.
                        IF sy-subrc IS INITIAL.
                          LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_lfa1-adrnr AND smtp_addr IS NOT INITIAL.
                            APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                          ENDLOOP.
                        ENDIF.

                        IF wa_kna1-ktokd EQ 'ZFIC'.
                          CASE sy-sysid.
                            WHEN 'QAS'.
                              APPEND VALUE #( email = 'cte.homolog@amaggi.com.br' ) TO gt_email.
                            WHEN 'PRD'.
                              APPEND VALUE #( email = 'cte.fiscal@amaggi.com.br' ) TO gt_email.
                          ENDCASE.
                        ENDIF.

                    ENDCASE.
                  ENDLOOP.
                ENDLOOP.
              ENDLOOP.
            ENDIF.

          WHEN 'ZW'.
            wa_link-link = wa_j_1bnflin_aux-refkey(10).
            LOOP AT it_zfiwrt0015 INTO wa_zfiwrt0015 WHERE seq_lcto EQ wa_link-link.
              CASE wa_zfiwrt0015-parvw.
                WHEN 'WE' OR 'LR'.
                  READ TABLE it_kna1 WITH KEY kunnr = wa_zfiwrt0015-parid INTO wa_kna1 BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_kna1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.
                WHEN 'Z1'.
                  READ TABLE it_lfa1 WITH KEY lifnr = wa_zfiwrt0015-parid INTO wa_lfa1 BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_lfa1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.
              ENDCASE.
            ENDLOOP.
          WHEN space.

            LOOP AT it_j_1bnfnad INTO DATA(wa_nad) WHERE docnum EQ ls_bnfdoc-docnum.
              CASE wa_nad-partyp.
                WHEN 'C'.
                  READ TABLE it_kna1 WITH KEY kunnr = wa_nad-parid INTO wa_kna1 BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_kna1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.
                WHEN 'V'.
                  READ TABLE it_lfa1 WITH KEY lifnr = wa_nad-parid INTO wa_lfa1 BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    LOOP AT it_adr6 INTO wa_adr6 WHERE addrnumber EQ wa_lfa1-adrnr AND smtp_addr IS NOT INITIAL.
                      APPEND VALUE #( email = wa_adr6-smtp_addr ) TO gt_email.
                    ENDLOOP.
                  ENDIF.
              ENDCASE.
            ENDLOOP.

        ENDCASE.
      ENDIF.

    ELSE.
      APPEND VALUE #( email = pemail ) TO gt_email.
    ENDIF.

    IF gt_email[] IS NOT INITIAL.
      SORT gt_email BY email ASCENDING.
      DELETE ADJACENT DUPLICATES FROM gt_email COMPARING email.
    ELSE.
      MESSAGE s005 WITH ls_bnfdoc-parid DISPLAY LIKE 'E'.
      DATA: wa_enviado2 TYPE zsdt0230.
      wa_enviado2-docnum = ls_bnfdoc-docnum.
      MODIFY zsdt0230 FROM wa_enviado2.
      COMMIT WORK.
      CONTINUE.
    ENDIF.

*    CREATE OBJECT LO_DOCUMENT.
*    TRY .
*        LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*            I_TYPE       = 'HTM'
*            I_SUBJECT    = LV_SUBJECT
*            I_LANGUAGE   = SY-LANGU
*            I_IMPORTANCE = '1'
*            I_TEXT       = LT_CONTENTS ).

    CLEAR:   lv_binpdf,
             lv_binxml.

    REFRESH: lt_xml,
             lt_pdf,
             lt_attach_bin.

    TRY .
        CALL FUNCTION 'ZFSD_BUSCA_DANFE'
          EXPORTING
            i_docnum   = ls_bnfdoc-docnum
          IMPORTING
            t_xml      = lt_xml
            out_binpdf = lv_binpdf
            out_binxml = lv_binxml.

      CATCH cx_root.
        CONTINUE.
    ENDTRY.

    DATA(lv_xml_string) = zcl_string=>xstring_to_string( i_xstring = lv_binxml ).

    FREE: lt_attach_txt.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = lv_xml_string
      TABLES
        ftext_tab = lt_attach_txt.

    CASE ls_bnfdoc-model.
      WHEN '55'.
        lv_filename_d = 'Danfe.pdf'.
      WHEN '57'.
        lv_filename_d = 'Dacte.pdf'.
    ENDCASE.

****    PDF Attach
    lt_attach_bin = cl_document_bcs=>xstring_to_solix( ip_xstring = lv_binpdf ).
*    CALL METHOD LO_DOCUMENT->ADD_ATTACHMENT
*      EXPORTING
*        I_ATTACHMENT_TYPE    = 'PDF'
*        I_ATTACHMENT_SUBJECT = LV_FILENAME_D
*        I_ATT_CONTENT_HEX    = LT_ATTACH_BIN.

    CASE ls_bnfdoc-model.
      WHEN '55'.
        lv_filename_x = 'NFeXML.xml'.
      WHEN '57'.
        lv_filename_x = 'CTeXML.xml'.
    ENDCASE.

*    CALL METHOD LO_DOCUMENT->ADD_ATTACHMENT
*      EXPORTING
*        I_ATTACHMENT_TYPE    = 'XML'
*        I_ATTACHMENT_SUBJECT = LV_FILENAME_X
*        I_ATT_CONTENT_TEXT   = LT_CONTENT_TXT.
*
*        IF PBATCH EQ ABAP_FALSE.
*
*          DATA(GT_EMAIL_AUX) = GT_EMAIL[].
*
*          LOOP AT GT_EMAIL_AUX INTO DATA(LS_EMAIL).
*
*            CLEAR: LT_SVAL[].
*            LS_SVAL-TABNAME   = 'ADR6'.
*            LS_SVAL-FIELDNAME = 'SMTP_ADDR'.
*            LS_SVAL-VALUE     = LS_EMAIL-EMAIL.
*            APPEND LS_SVAL TO LT_SVAL.
*
**            CALL FUNCTION 'POPUP_GET_VALUES'
**              EXPORTING
**                POPUP_TITLE     = TEXT-006
**              IMPORTING
**                RETURNCODE      = LV_RETCODE
**              TABLES
**                FIELDS          = LT_SVAL
**              EXCEPTIONS
**                ERROR_IN_FIELDS = 1
**                OTHERS          = 2.
**
**            IF SY-SUBRC IS NOT INITIAL.
**              MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
**              DELETE GT_EMAIL WHERE EMAIL EQ LS_EMAIL-EMAIL.
**            ELSE.
**              IF LV_RETCODE = 'A'. "Cancel
**                DELETE GT_EMAIL WHERE EMAIL EQ LS_EMAIL-EMAIL.
**              ENDIF.
**            ENDIF.
*
*          ENDLOOP.
*
*        ENDIF.

    IF gt_email[] IS INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE gt_email INTO DATA(wa_email) INDEX 1.
    DATA: lc_string_mails TYPE string.
    CLEAR: lc_string_mails.

*        TRY .

*            WO_DOCUMENT = CL_BCS=>CREATE_PERSISTENT( ).
*            CALL METHOD WO_DOCUMENT->SET_DOCUMENT( LO_DOCUMENT ).
*
*            "Add sender to send request
*            CALL METHOD WO_DOCUMENT->SET_SENDER
*              EXPORTING
*                I_SENDER = CL_SAPUSER_BCS=>CREATE( ZCL_JOB=>GET_USER_JOB( ) ).

    DATA: it_email_rfc TYPE TABLE OF adr6,
          wa_email_rfc TYPE adr6.

    CLEAR: it_email_rfc[].

    LOOP AT gt_email INTO DATA(ls_email).

      IF ( pemail IS INITIAL ) OR ( pemail NE ls_email-email AND pemail IS NOT INITIAL ).
        CASE sy-sysid.
          WHEN 'DEV'.

            IF ls_bnfdoc-crenam NE zcl_job=>get_user_job( ).
              ls_email-email = zcl_user=>zif_user~get_mail_user( i_user = ls_bnfdoc-crenam ).
            ELSE.
              CLEAR: ls_email-email.
            ENDIF.

            IF ls_email-email IS INITIAL.
              ls_email-email = 'suporte.sap@amaggi.com.br'.
            ENDIF.

          WHEN 'QAS'.

            IF ls_bnfdoc-crenam NE zcl_job=>get_user_job( ).
              ls_email-email = zcl_user=>zif_user~get_mail_user( i_user = ls_bnfdoc-crenam ).
            ELSE.
              CLEAR: ls_email-email.
            ENDIF.

            IF ls_email-email IS INITIAL.
              ls_email-email = 'suporte.sap@amaggi.com.br'.
            ENDIF.

        ENDCASE.
      ENDIF.

      lc_string_mails = zcl_string=>concat( EXPORTING s1 = lc_string_mails s2 = CONV #( ls_email-email ) sp = ';' ).
      wa_email_rfc-smtp_addr = ls_email-email.
      APPEND wa_email_rfc TO it_email_rfc.

    ENDLOOP.

    DATA: i_type        TYPE  so_obj_tp,
          anexo_01_type TYPE  so_obj_tp,
          anexo_02_type TYPE  so_obj_tp,
          ck_anexo_01   TYPE  char01,
          ck_anexo_02   TYPE  char01.

    i_type = 'HTM'.
    anexo_01_type = 'PDF'.
    anexo_02_type = 'XML'.
    ck_anexo_01 = abap_true.
    ck_anexo_02 = abap_true.

    CASE ck_send_mail_grc.
      WHEN abap_true.

        CALL FUNCTION 'Z_SEND_MAIL' DESTINATION lv_rfcdest
          IMPORTING
            p_outenviado          = p_outenviado
          TABLES
            lt_contents           = lt_contents
            lt_smtpadr            = it_email_rfc
            anexo_02_txt          = lt_attach_txt
            anexo_01_hex          = lt_attach_bin
          CHANGING
            i_user                = lc_user_mail
            i_type                = i_type
            i_subject             = lv_subject
            ck_anexo_01           = ck_anexo_01
            anexo_01_type         = anexo_01_type
            anexo_01_subject      = lv_filename_d
            ck_anexo_02           = ck_anexo_02
            anexo_02_type         = anexo_02_type
            anexo_02_subject      = lv_filename_x
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2.

      WHEN abap_false.

        CALL FUNCTION 'Z_SEND_MAIL'
          IMPORTING
            p_outenviado     = p_outenviado
          TABLES
            lt_contents      = lt_contents
            lt_smtpadr       = it_email_rfc
            anexo_02_txt     = lt_attach_txt
            anexo_01_hex     = lt_attach_bin
          CHANGING
            i_user           = lc_user_mail
            i_type           = i_type
            i_subject        = lv_subject
            ck_anexo_01      = ck_anexo_01
            anexo_01_type    = anexo_01_type
            anexo_01_subject = lv_filename_d
            ck_anexo_02      = ck_anexo_02
            anexo_02_type    = anexo_02_type
            anexo_02_subject = lv_filename_x.

    ENDCASE.

    IF p_outenviado EQ abap_true.

      IF pemail IS INITIAL.
        DATA: wa_enviado TYPE zsdt0230.
        wa_enviado-docnum      = ls_bnfdoc-docnum.
        wa_enviado-email       = lc_string_mails.
        wa_enviado-dt_registro = sy-datum.
        wa_enviado-hr_registro = sy-uzeit.
        MODIFY zsdt0230 FROM wa_enviado.
      ENDIF.
      COMMIT WORK.

    ELSE.
      "IF SY-BATCH EQ ABAP_FALSE.
      "  WRITE: / 'Docnum ', LS_BNFDOC-DOCNUM, ' - Erro no envio do email'.
      "  WRITE: / 'Destinatários: ', WA_EMAIL-EMAIL.
      "ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
