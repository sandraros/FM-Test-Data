*&---------------------------------------------------------------------*
*& Report z_fm_test_data_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fm_test_data_export.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_fm_names TYPE RANGE OF tfdir-funcname,
           ty_nummers  TYPE RANGE OF i,
           ty_dates    TYPE RANGE OF sy-datum.

    METHODS constructor
      IMPORTING
        fm_names TYPE ty_fm_names
        nummers  TYPE ty_nummers
        dates    TYPE ty_dates
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
          dates    TYPE ty_dates READ-ONLY,
          to_file  TYPE string READ-ONLY.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

    me->fm_names = fm_names.
    me->nummers = nummers.
    me->dates = dates.
    me->to_file = to_file.

  ENDMETHOD.

  METHOD main.

    DATA: param_bindings_pbo    TYPE abap_func_parmbind_tab,
          param_bindings_pai    TYPE abap_func_parmbind_tab,
          test_data_header      TYPE string,
          arguments_before_call TYPE string,
          arguments_after_call  TYPE string,
          generic               TYPE abap_trans_srcbind_tab.

    SELECT * FROM eufunc
    WHERE name   IN @fm_names
      AND nummer IN @nummers
      AND nummer <> '999'
      AND seqid  =  ' ' " Don't extract test sequences
      AND datum  IN @dates
    INTO TABLE @DATA(eufunc_s).

    DATA(file_string) = |<?xml version="1.0"?><testDataRepository>|.

    LOOP AT eufunc_s REFERENCE INTO DATA(eufunc).

      CLEAR: param_bindings_pbo,
             param_bindings_pai.

      zcl_fm_test_data=>load(
        EXPORTING
          fugr_name      = CONV #( eufunc->gruppe )
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
        RESULT
          XML test_data_header
        OPTIONS
          xml_header = 'no'.

      generic = VALUE abap_trans_srcbind_tab(
          FOR <pbo> IN param_bindings_pbo
          ( name  = <pbo>-name
            value = <pbo>-value ) ).

      CALL TRANSFORMATION id
        SOURCE
          (generic)
        RESULT
          XML arguments_before_call
        OPTIONS
          xml_header = 'no'.

      generic = VALUE abap_trans_srcbind_tab(
          FOR <pai> IN param_bindings_pai
          ( name  = <pai>-name
            value = <pai>-value ) ).

      CALL TRANSFORMATION id
        SOURCE
          (generic)
        RESULT
          XML arguments_after_call
        OPTIONS
          xml_header = 'no'.

      file_string = |{ file_string }<testData>{ test_data_header
                    }<argumentsBeforeCall>{ arguments_before_call }</argumentsBeforeCall>| &&
                    |<argumentsAfterCall>{ arguments_after_call }</argumentsAfterCall>| &&
                    |</testData>|.

    ENDLOOP.

    file_string = |{ file_string }</testDataRepository>|.

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
      nummer  TYPE i,
      date    TYPE sy-datum.

SELECT-OPTIONS fm_names FOR fm_name DEFAULT 'STRING_REVERSE'.
SELECT-OPTIONS nummers FOR nummer DEFAULT 1 TO 99.
SELECT-OPTIONS dates FOR date.
PARAMETERS to_file TYPE string DEFAULT 'C:\temp\testdata.xml' LOWER CASE.

START-OF-SELECTION.
  TRY.
      NEW lcl_app(
        fm_names = fm_names[]
        nummers  = nummers[]
        dates  = dates[]
        to_file  = to_file
        )->main( ).
    CATCH zcx_fm_test_data INTO DATA(lx_fm_test_data).
      MESSAGE lx_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
