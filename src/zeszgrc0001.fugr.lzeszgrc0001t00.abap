*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTNFE_ADD_RESP..................................*
DATA:  BEGIN OF STATUS_ZTNFE_ADD_RESP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTNFE_ADD_RESP                .
CONTROLS: TCTRL_ZTNFE_ADD_RESP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTNFE_ADD_RESP                .
TABLES: ZTNFE_ADD_RESP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
