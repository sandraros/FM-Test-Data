CLASS zcl_fm_test_data_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apack_manifest.

    METHODS: constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FM_TEST_DATA_APACK IMPLEMENTATION.


  METHOD constructor.
    if_apack_manifest~descriptor = VALUE #(
        group_id     = 'github.com/sandraros'
        artifact_id  = 'FM-Test-Data'
        version      = '1.0'
        repository_type = 'abapGit'
        git_url      = 'https://github.com/sandraros/FM-Test-Data.git'
        dependencies = VALUE #(
            ( group_id       = 'github.com/sandraros'
              artifact_id    = 'Export-Import-Tables'
              version        = '1.0'
              git_url        = 'https://github.com/sandraros/Export-Import-Tables.git'
              target_package = '' )
            ( group_id       = 'github.com/sandraros'
              artifact_id    = 'FM-params-RTTS'
              version        = '1.0'
              git_url        = 'https://github.com/sandraros/FM-params-RTTS.git'
              target_package = '' )
            ( group_id       = 'github.com/sandraros'
              artifact_id    = 'S-RTTI'
              version        = '1.0'
              git_url        = 'https://github.com/sandraros/S-RTTI.git'
              target_package = '' ) ) ).
  ENDMETHOD.
ENDCLASS.
