*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZESZCTE_VIAGEM.....................................*
DATA:  BEGIN OF STATUS_ZCTE_VIAGEM                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCTE_VIAGEM                   .
CONTROLS: TCTRL_ZCTE_VIAGEM
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZESZCTE_VIAGEM                   .
TABLES: ZESZCTE_VIAGEM                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
