# FM-Test-Data
API (class `ZCL_FM_TEST_DATA`) to create Function Module Test Data in transaction code SE37.

Example program:
```
REPORT zdemo.

START-OF-SELECTION.
  DATA(test_data) = zcl_fm_test_data=&gt;create( fm_name = 'DATE_GET_WEEK' title = 'demo' ).
  test_data-&gt;set_input_parameters( VALUE #( ( name = 'DATE' value = REF #( sy-datum ) ) ) ).
  test_data-&gt;save( ).
  COMMIT WORK.
  MESSAGE |Test data { condense( test_data-&gt;id ) } created| type 'I'.
```

Dependencies:
- https://github.com/sandraros/Export-Import-Tables.git
- https://github.com/sandraros/FM-params-RTTS.git
- https://github.com/sandraros/S-RTTI.git

Installation via [abapGit](https://github.com/abapGit/abapGit)
