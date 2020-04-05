FUNCTION Z_FM_TEST_DATA_TEST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMPORT) TYPE  STRING OPTIONAL
*"     REFERENCE(BAPIRETURN) TYPE  BAPIRETURN OPTIONAL
*"  EXPORTING
*"     REFERENCE(EXPORT) TYPE  STRING
*"  TABLES
*"      TABLES_ STRUCTURE  SFLIGHT OPTIONAL
*"  CHANGING
*"     REFERENCE(CHANGING) TYPE  STRING OPTIONAL
*"     REFERENCE(A234567890_234567890_234567890) TYPE  STRING OPTIONAL
*"----------------------------------------------------------------------
  changing = 'after call'.

  export = 'after call'.

  APPEND INITIAL LINE TO tables_.


ENDFUNCTION.
