# FM-Test-Data
Utility to read and save Function Module Test Data

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
