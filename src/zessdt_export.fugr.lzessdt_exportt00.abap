*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZESSDT_EXPORT.....................................*
DATA:  BEGIN OF STATUS_ZSDT_EXPORT                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT_EXPORT                   .
CONTROLS: TCTRL_ZSDT_EXPORT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZESSDT_EXPORT                   .
TABLES: ZESSDT_EXPORT                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
