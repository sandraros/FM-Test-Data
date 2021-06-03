# FM-Test-Data
Utility class `ZCL_FM_TEST_DATA` to read and save Function Module Test Data

Four methods are proposed:
- `LOAD`
  - Input:
    - Function module name
    - Test ID
  - Output:
    - param_bindings_pbo : Values of arguments passed to the function module (call)
    - param_bindings_pai : Values of parameters returned after the function module call
  - NB: it's allowed that the function module does not exist anymore, or it has been moved to another function group.
- `CREATE_WITHOUT_EXECUTION`
- `EXECUTE_AND_CREATE`
- `DELETE`

Example program (of type "executable") to duplicate test data in the same function module:
```
REPORT.
PARAMETERS fm_name TYPE tfdir-funcname DEFAULT 'STRING_REVERSE'.
PARAMETERS nummer TYPE i DEFAULT 1.

START-OF-SELECTION.
  DATA exc_fm_test_data TYPE REF TO zcx_fm_test_data.
  DATA(param_bindings) = VALUE abap_func_parmbind_tab( ).

  TRY.
      zcl_fm_test_data=>load(
        EXPORTING
          fm_name       = fm_name
          test_id       = nummer
        IMPORTING
          datadir_entry = DATA(datadir_entry)
          attributes    = DATA(attributes)
        CHANGING
          param_bindings_pbo = param_bindings ).
    CATCH zcx_fm_test_data INTO exc_fm_test_data.
      MESSAGE exc_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  TRY.
      zcl_fm_test_data=>create_without_execution(
          fm_name        = fm_name
          title          = |Copy of test data { nummer }|
          param_bindings = param_bindings ).
      COMMIT WORK.

      MESSAGE 'Test data successfully created' TYPE 'I' DISPLAY LIKE 'S'.

    CATCH zcx_fm_test_data INTO exc_fm_test_data.
      MESSAGE exc_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
```

Dependencies:
- https://github.com/sandraros/Export-Import-Tables.git
- https://github.com/sandraros/FM-params-RTTS.git
