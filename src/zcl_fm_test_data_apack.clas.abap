CLASS zcl_fm_test_data_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_apack_manifest.

    METHODS: constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_fm_test_data_apack IMPLEMENTATION.

  METHOD constructor.
    zif_apack_manifest~descriptor = VALUE #(
        group_id     = 'github.com/sandraros'
        artifact_id  = 'FM-Test-Data'
        version      = '0.2'
        repository_type = 'abapGit'
        git_url      = 'https://github.com/sandraros/FM-Test-Data.git'
        dependencies = VALUE #(
            ( group_id       = 'github.com/sandraros'
              artifact_id    = 'Export-Import-Tables'
              version        = '0.2'
              git_url        = 'https://github.com/sandraros/Export-Import-Tables.git'
              target_package = '' )
            ( group_id       = 'github.com/sandraros'
              artifact_id    = 'FM-params-RTTS'
              version        = '0.2'
              git_url        = 'https://github.com/sandraros/FM-params-RTTS.git'
              target_package = '' ) ) ).
  ENDMETHOD.

ENDCLASS.
