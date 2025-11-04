PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TC_PROD_DET'
  MODULE tc_prod_det_active_tab_set.
  CALL SUBSCREEN tc_prod_det_sca
    INCLUDING g_tc_prod_det-prog g_tc_prod_det-subscreen.
  MODULE status_5100.
*
PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TC_PROD_DET'
  CALL SUBSCREEN tc_prod_det_sca.
  MODULE tc_prod_det_active_tab_get.
  MODULE user_command_5100.
