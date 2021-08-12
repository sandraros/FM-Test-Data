*&---------------------------------------------------------------------*
*& Report Z_FM_TEST_DATA_DEMO_IMPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fm_test_data_demo_import.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_fm_names    TYPE RANGE OF tfdir-funcname,
           ty_nummers     TYPE RANGE OF i,
           ty_xml_pattern TYPE RANGE OF rsparamsl_255-low.

    METHODS constructor
      IMPORTING
        from_file   TYPE csequence
        fm_names    TYPE ty_fm_names
        nummers     TYPE ty_nummers
        xml_pattern TYPE ty_xml_pattern.

    METHODS main
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS upload
      IMPORTING
        i_filename    TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    DATA: fm_names    TYPE ty_fm_names READ-ONLY,
          nummers     TYPE ty_nummers READ-ONLY,
          from_file   TYPE string READ-ONLY,
          xml_pattern TYPE ty_xml_pattern READ-ONLY.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

    me->fm_names = fm_names.
    me->nummers = nummers.
    me->from_file = from_file.
    me->xml_pattern = xml_pattern.

  ENDMETHOD.

  METHOD main.

    TYPES: BEGIN OF ty_test_data_item,
             error   TYPE string,
             content TYPE xsdany,
           END OF ty_test_data_item,
           ty_test_data_items TYPE STANDARD TABLE OF ty_test_data_item WITH EMPTY KEY.


    DATA(xml) = upload( from_file ).

    DATA(test_data_items) = VALUE ty_test_data_items( ).
    CALL TRANSFORMATION id
        SOURCE XML xml
        RESULT test_data_items = test_data_items.

    DELETE test_data_items WHERE error IS NOT INITIAL.

    LOOP AT test_data_items REFERENCE INTO DATA(test_data_item).

      DATA(test_data_xml) = VALUE string( ).
      CALL TRANSFORMATION id
          SOURCE XML test_data_item->content
          RESULT XML test_data_xml.

      DATA(test_data_deserialized_xml) = cl_abap_codepage=>convert_from( test_data_item->content ).

      IF test_data_deserialized_xml IN xml_pattern.

        DATA(test_data_structure) = zcl_fm_test_data_serialize=>deserialize_into_structure( test_data_deserialized_xml ).

        IF test_data_structure-header-fm_name IN fm_names AND CONV i( test_data_structure-header-id ) IN nummers.

          DATA(test_data) = zcl_fm_test_data_serialize=>deserialize( test_data_deserialized_xml ).
          test_data->save( ).
          COMMIT WORK.

          WRITE / |Test data created: { test_data->fm_name } #{ condense( test_data->id ) }|.

        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD upload.

    DATA: l_length   TYPE i,
          lt_string  TYPE TABLE OF string,
          l_filename TYPE string.

    l_filename = i_filename.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = l_filename
        filetype = 'ASC' "#EC NOTEXT
      CHANGING
        data_tab = lt_string
      EXCEPTIONS
        OTHERS   = 17.

    result = concat_lines_of( sep = |\n| table = lt_string ).

  ENDMETHOD.

ENDCLASS.

DATA: fm_name TYPE tfdir-funcname,
      nummer  TYPE i,
      pattern TYPE rsparamsl_255-low.

SELECT-OPTIONS fm_names FOR fm_name DEFAULT 'Z_FM_TEST_DATA_DEMO'.
SELECT-OPTIONS nummers FOR nummer.
SELECT-OPTIONS xmlpatrn FOR pattern LOWER CASE.
PARAMETERS file TYPE string DEFAULT 'C:\temp\testdata.xml' LOWER CASE.

START-OF-SELECTION.
  TRY.
      NEW lcl_app(
        from_file   = file
        fm_names    = fm_names[]
        nummers     = nummers[]
        xml_pattern = xmlpatrn[]
        )->main( ).
    CATCH zcx_fm_test_data INTO DATA(lx_fm_test_data).
      MESSAGE lx_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
