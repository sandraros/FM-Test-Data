FUNCTION Z_FM_TEST_DATA_LOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FM_NAME) TYPE  TFDIR-FUNCNAME
*"     VALUE(ID) TYPE  EUFUNC-NUMMER
*"  EXPORTING
*"     VALUE(TEST_DATA_XML) TYPE  STRING
*"  EXCEPTIONS
*"      NON_LOCAL_RFC_CALL
*"      LOAD_ERROR
*"----------------------------------------------------------------------
  " CALLED BY zcl_fm_test_data=>load and zcl_fm_test_data=>load_as_xml
  " when argument isolated_run = abap_true.

  DATA caller_in_same_system TYPE answer.

  CALL FUNCTION 'RFC_WITHIN_SAME_SYSTEM'
    IMPORTING
      caller_in_same_system     = caller_in_same_system
    EXCEPTIONS
      system_call_not_supported = 1
      no_rfc_communication      = 2
      internal_error            = 3.

  IF sy-subrc <> 0 OR caller_in_same_system = abap_false.
    RAISE non_local_rfc_call.
  ENDIF.

  TRY.
      " §§ IMPORTANT §§ isolated_run must be = abap_false otherwise endless loop
      " because method LOAD calls again Z_FM_TEST_DATA_LOAD when isolated_run = abap_true.
      DATA(test_data) = zcl_fm_test_data=>load( fm_name = fm_name id = id isolated_run = abap_false ).
      test_data_xml = zcl_fm_test_data_serialize=>serialize( test_data ).
    CATCH zcx_fm_test_data INTO DATA(error).
      RAISE load_error.
  ENDTRY.

ENDFUNCTION.
