*&---------------------------------------------------------------------*
*& Report z_fm_test_data_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fm_test_data_export.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_fm_names TYPE RANGE OF tfdir-funcname,
           ty_nummers  TYPE RANGE OF i.

    METHODS constructor
      IMPORTING
        fm_names TYPE ty_fm_names
        nummers  TYPE ty_nummers
        to_file  TYPE csequence.

    METHODS main
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS download
      IMPORTING
        i_filename TYPE csequence
        i_string   TYPE string.

    DATA: fm_names TYPE ty_fm_names READ-ONLY,
          nummers  TYPE ty_nummers READ-ONLY,
          to_file  TYPE string READ-ONLY.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.
    me->fm_names = fm_names.
    me->nummers = nummers.
    me->to_file = to_file.
  ENDMETHOD.

  METHOD main.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab,
          param_bindings_pai TYPE abap_func_parmbind_tab,
          test_data_string   TYPE string.

    SELECT * FROM eufunc
    WHERE name   IN @fm_names
      AND nummer IN @nummers
      AND nummer <> '999'
    INTO TABLE @DATA(eufunc_s).

    DATA(file_string) = |<?xml version="1.0"?><testDataFolder>|.

    LOOP AT eufunc_s REFERENCE INTO DATA(eufunc).

      CLEAR: param_bindings_pbo,
             param_bindings_pai.

      zcl_fm_test_data=>load(
        EXPORTING
          fm_name        = eufunc->name
          test_id        = CONV decfloat34( eufunc->nummer )
        IMPORTING
          datadir_entry  = DATA(datadir_entry)
          attributes     = DATA(attributes)
        CHANGING
          param_bindings_pbo = param_bindings_pbo
          param_bindings_pai = param_bindings_pai ).

      CALL TRANSFORMATION id
        SOURCE
          fm_name       = eufunc->name
          test_id       = eufunc->nummer
          datadir_entry = datadir_entry
          attributes    = attributes
          pbo           = param_bindings_pbo
          pai           = param_bindings_pai
        RESULT
          XML test_data_string
        OPTIONS
          xml_header = 'no'.

      file_string = |{ file_string }<testData>{ test_data_string }</testData>|.

    ENDLOOP.

    file_string = |{ file_string }</testDataFolder>|.

    download( i_filename = to_file
              i_string   = file_string ).

  ENDMETHOD.

  METHOD download.

    DATA: l_length   TYPE i,
          lt_string  TYPE TABLE OF string,
          l_filename TYPE string.

    APPEND i_string TO lt_string.

    l_filename = i_filename.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename = l_filename
        filetype = 'ASC' "#EC NOTEXT
      CHANGING
        data_tab = lt_string
      EXCEPTIONS
        OTHERS   = 17.

  ENDMETHOD.

ENDCLASS.

DATA: fm_name TYPE tfdir-funcname,
      nummer  TYPE i.

SELECT-OPTIONS fm_names FOR fm_name DEFAULT 'STRING_REVERSE'.
SELECT-OPTIONS nummers FOR nummer DEFAULT 1 TO 99.
PARAMETERS to_file TYPE string DEFAULT 'C:\temp\testdata.xml' LOWER CASE.

START-OF-SELECTION.
  TRY.
      NEW lcl_app(
        fm_names = fm_names[]
        nummers  = nummers[]
        to_file  = to_file
        )->main( ).
    CATCH zcx_fm_test_data INTO DATA(lx_fm_test_data).
      MESSAGE lx_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
