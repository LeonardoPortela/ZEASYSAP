*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZIB_CTE_DIST_TER
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZIB_CTE_DIST_TER   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
