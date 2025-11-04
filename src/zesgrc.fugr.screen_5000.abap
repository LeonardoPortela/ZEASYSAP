PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'ZNFE'
  MODULE znfe_active_tab_set.
  CALL SUBSCREEN znfe_sca
    INCLUDING g_znfe-prog g_znfe-subscreen.
  MODULE status_5000.
*
PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'ZNFE'
  CALL SUBSCREEN znfe_sca.
  MODULE znfe_active_tab_get.
  MODULE user_command_5000.
