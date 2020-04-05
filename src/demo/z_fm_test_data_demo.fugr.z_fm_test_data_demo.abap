FUNCTION Z_FM_TEST_DATA_DEMO.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMPORT) TYPE  STRING OPTIONAL
*"     REFERENCE(BAPIRETURN) TYPE  BAPIRETURN OPTIONAL
*"     REFERENCE(WAIT_2_SECONDS) TYPE  FLAG DEFAULT ' '
*"     REFERENCE(RAISE_EXCEPTION) TYPE  FLAG DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(EXPORT) TYPE  STRING
*"  TABLES
*"      TABLES_ STRUCTURE  SFLIGHT OPTIONAL
*"  CHANGING
*"     REFERENCE(CHANGING) TYPE  STRING OPTIONAL
*"     REFERENCE(A234567890_234567890_234567890) TYPE  STRING
*"         OPTIONAL
*"  EXCEPTIONS
*"      A
*"      FAILED
*"--------------------------------------------------------------------
  changing = 'after call'.

  export = 'after call'.

  APPEND VALUE sflight( carrid = 'AZ' connid = '525' ) TO tables_.

  IF wait_2_seconds = abap_true.
    " Test of the "duration" internal field in test data
    WAIT UP TO 2 SECONDS.
  ENDIF.

  IF raise_exception = abap_true.
    MESSAGE e001(00) WITH 'Error' RAISING failed.
  ENDIF.


ENDFUNCTION.
