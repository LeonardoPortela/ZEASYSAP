*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZESZIB_CTE_DIST_FLG................................*
DATA:  BEGIN OF STATUS_ZIB_CTE_DIST_FLG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_CTE_DIST_FLG              .
CONTROLS: TCTRL_ZIB_CTE_DIST_FLG
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZESZIB_CTE_DIST_GM.................................*
DATA:  BEGIN OF STATUS_ZIB_CTE_DIST_GM               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_CTE_DIST_GM               .
CONTROLS: TCTRL_ZIB_CTE_DIST_GM
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZESZIB_CTE_DIST_IVA................................*
DATA:  BEGIN OF STATUS_ZIB_CTE_DIST_IVA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_CTE_DIST_IVA              .
CONTROLS: TCTRL_ZIB_CTE_DIST_IVA
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZESZIB_CTE_DIST_PLD................................*
DATA:  BEGIN OF STATUS_ZIB_CTE_DIST_PLD              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_CTE_DIST_PLD              .
CONTROLS: TCTRL_ZIB_CTE_DIST_PLD
            TYPE TABLEVIEW USING SCREEN '0006'.
*.........table declarations:.................................*
TABLES: *ZESZIB_CTE_DIST_FLG              .
TABLES: *ZESZIB_CTE_DIST_GM               .
TABLES: *ZESZIB_CTE_DIST_IVA              .
TABLES: *ZESZIB_CTE_DIST_PLD              .
TABLES: ZESZIB_CTE_DIST_FLG               .
TABLES: ZESZIB_CTE_DIST_GM                .
TABLES: ZESZIB_CTE_DIST_IVA               .
TABLES: ZESZIB_CTE_DIST_PLD               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
