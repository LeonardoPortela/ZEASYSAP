*---------------------------------------------------------------------*
*    generated viewmaintenance function pool top
*---------------------------------------------------------------------*
FUNCTION-POOL ZESZCTE_DIST                  MESSAGE-ID ZESZCTE_DISTRI.

TABLES: ZESZIB_CTE_DIST_TER.

TYPES: BEGIN OF TY_INFO_FORNE.
TYPES:   BVTYP TYPE BVTYP,  "Tipo de banco do parceiro
         TEXTO TYPE CHAR50, "Fornecedor
         BANKL TYPE CHAR03, "Banco
         BANKA TYPE BANKA,  "Nome do Banco
         BANKN TYPE BANKN,  "Conta Corrente
         AGENC TYPE CHAR15. "Agencia
TYPES: END OF TY_INFO_FORNE.

CONSTANTS: OK_APROVAR   TYPE SY-UCOMM VALUE 'APROVAR',
           OK_RECUSAR   TYPE SY-UCOMM VALUE 'RECUSAR',
           OK_CANCEL    TYPE SY-UCOMM VALUE 'CANCEL',
           OK_CONFIRMAR TYPE SY-UCOMM VALUE 'CONFIRMAR',
           OK_CANCELAR  TYPE SY-UCOMM VALUE 'CANCELAR',
           OK_BTN_NOTAS TYPE SY-UCOMM VALUE 'BTN_NOTAS'.

CONSTANTS: OK_SALVAR    TYPE SY-UCOMM VALUE 'SALVAR',
           OK_VERIFICAR TYPE SY-UCOMM VALUE 'VERIFICAR',
           OK_PRIMEIRO  TYPE SY-UCOMM VALUE 'PRIMEIRO',
           OK_ANTERIOR  TYPE SY-UCOMM VALUE 'ANTERIOR',
           OK_PROXIMO   TYPE SY-UCOMM VALUE 'PROXIMO',
           OK_ULTIMO    TYPE SY-UCOMM VALUE 'ULTIMO'.

INCLUDE LSVIMDAT                                . "general data decl.
INCLUDE LZCTE_DISTT00                           . "view rel. data dcl.
