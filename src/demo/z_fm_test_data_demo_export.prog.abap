*&---------------------------------------------------------------------*
*& Report Z_FM_TEST_DATA_DEMO_EXPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fm_test_data_demo_export.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_fm_names    TYPE RANGE OF tfdir-funcname,
           ty_nummers     TYPE RANGE OF i,
           ty_dates       TYPE RANGE OF sy-datum,
           ty_xml_pattern TYPE RANGE OF rsparamsl_255-low.

    METHODS constructor
      IMPORTING
        fm_names    TYPE ty_fm_names
        nummers     TYPE ty_nummers
        dates       TYPE ty_dates
        to_file     TYPE csequence
        xml_pattern TYPE ty_xml_pattern.

    METHODS main
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS download
      IMPORTING
        i_filename TYPE csequence
        i_string   TYPE string.

    DATA: fm_names    TYPE ty_fm_names READ-ONLY,
          nummers     TYPE ty_nummers READ-ONLY,
          dates       TYPE ty_dates READ-ONLY,
          to_file     TYPE string READ-ONLY,
          xml_pattern TYPE ty_xml_pattern READ-ONLY.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

    me->fm_names = fm_names.
    me->nummers = nummers.
    me->dates = dates.
    me->to_file = to_file.
    me->xml_pattern = xml_pattern.

  ENDMETHOD.

  METHOD main.

    TYPES: BEGIN OF ty_test_data_item,
             error   TYPE string,
             content TYPE xsdany,
           END OF ty_test_data_item,
           ty_test_data_items TYPE STANDARD TABLE OF ty_test_data_item WITH EMPTY KEY.

    SELECT DISTINCT name, nummer FROM eufunc
        WHERE name   IN @fm_names
          AND nummer IN @nummers
          AND nummer <> '999'
          AND seqid  =  ' ' " Don't extract test sequences
          AND datum  IN @dates
        INTO TABLE @DATA(eufunc_lines).

    DATA(test_data_items) = VALUE ty_test_data_items( ).

    LOOP AT eufunc_lines REFERENCE INTO DATA(eufunc).

      DATA(test_data_item) = VALUE ty_test_data_item( ).
      TRY.
          DATA(test_data) = zcl_fm_test_data=>load( fm_name = eufunc->name id = eufunc->nummer isolated_run = abap_true ).
          DATA(test_data_serialized_xml) = zcl_fm_test_data_serialize=>serialize( test_data ).

          IF test_data_serialized_xml IN xml_pattern.
            test_data_item-content = cl_abap_codepage=>convert_to( test_data_serialized_xml ).
          ENDIF.

        CATCH zcx_fm_test_data INTO DATA(error).
          test_data_item-error = error->get_text( ).
      ENDTRY.

      APPEND test_data_item TO test_data_items.
    ENDLOOP.

    DATA(file_string) = VALUE string( ).
    CALL TRANSFORMATION id
        SOURCE test_data_items = test_data_items
        RESULT XML file_string.

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
      date    TYPE sy-datum,
      pattern TYPE rsparamsl_255-low.

SELECT-OPTIONS fm_names FOR fm_name DEFAULT 'Z_FM_TEST_DATA_DEMO'.
SELECT-OPTIONS nummers FOR nummer DEFAULT 1 TO 99.
SELECT-OPTIONS dates FOR date.
SELECT-OPTIONS xmlpatrn FOR pattern LOWER CASE.
PARAMETERS to_file TYPE string DEFAULT 'C:\temp\testdata.xml' LOWER CASE.

START-OF-SELECTION.
  TRY.
      NEW lcl_app(
        fm_names    = fm_names[]
        nummers     = nummers[]
        dates       = dates[]
        to_file     = to_file
        xml_pattern = xmlpatrn[]
        )->main( ).
    CATCH zcx_fm_test_data INTO DATA(lx_fm_test_data).
      MESSAGE lx_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
