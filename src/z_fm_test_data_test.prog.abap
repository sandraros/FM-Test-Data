*&---------------------------------------------------------------------*
*& Report z_fm_test_data_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fm_test_data_test.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        fm_name TYPE tfdir-funcname
        nummer  TYPE i
        option  TYPE i.

    METHODS main
      RAISING
        zcx_fm_test_data.

    METHODS create_without_execution
      RAISING
        zcx_fm_test_data.

    METHODS execute_and_create
      RAISING
        zcx_fm_test_data.

    METHODS copy_without_execution
      RAISING
        zcx_fm_test_data.

    METHODS display
      RAISING
        zcx_fm_test_data.

    METHODS delete
      RAISING
        zcx_fm_test_data.

    METHODS display_raw_internal_format
      RAISING
        zcx_fm_test_data.

    DATA:
      fm_name TYPE tfdir-funcname READ-ONLY,
      nummer  TYPE i READ-ONLY,
      option  TYPE i READ-ONLY.
    CONSTANTS:
      c_test_fm_name   TYPE tfdir-funcname VALUE 'Z_FM_TEST_DATA_TEST'.

  PRIVATE SECTION.

    METHODS get_xml
      IMPORTING
        data_name  TYPE csequence
        data_ref   TYPE REF TO data
      RETURNING
        VALUE(xml) TYPE string.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.
    me->fm_name = fm_name.
    me->nummer = nummer.
    me->option = option.
  ENDMETHOD.


  METHOD main.

    CASE option.
      WHEN 0.
        create_without_execution( ).
      WHEN 1.
        execute_and_create( ).
      WHEN 2.
        copy_without_execution( ).
      WHEN 3.
        display( ).
      WHEN 4.
        delete( ).
      WHEN 5.
        display_raw_internal_format( ).
    ENDCASE.

  ENDMETHOD.


  METHOD create_without_execution.

    TYPES ty_tables_ TYPE STANDARD TABLE OF sflight WITH EMPTY KEY.

    DATA(test_data_id) = zcl_fm_test_data=>create_without_execution(
      EXPORTING
        fm_name        = c_test_fm_name
        title          = 'test data'
        param_bindings = VALUE #(
                      ( name = 'IMPORT'   value = NEW string( |AA| ) )
                      ( name = 'BAPIRETURN' value = NEW bapireturn( code = 'FL317' ) )
                      ( name = 'CHANGING' value = NEW string( |BB| ) )
                      ( name = 'A234567890_234567890_234567890' value = NEW string( |CC| ) )
                      ( name = 'TABLES_'  value = NEW ty_tables_(
                          ( carrid = 'LH' connid = 123 )
                          ( carrid = 'AF' connid = 125 ) ) ) ) ).

    COMMIT WORK.

    MESSAGE |Test data "{ test_data_id }" created| TYPE 'I'.

  ENDMETHOD.


  METHOD execute_and_create.

    DATA(param_bindings_pbo) = VALUE abap_func_parmbind_tab( ).
    zcl_fm_test_data=>load(
      EXPORTING
        fm_name        = fm_name
        test_data_id   = nummer
      IMPORTING
        datadir_entry  = DATA(datadir_entry)
        attributes     = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo ).

    DATA(test_data_id) = zcl_fm_test_data=>execute_and_create(
      EXPORTING
        fm_name        = fm_name
        title          = |Copy of test data "{ nummer }" including FM result|
        param_bindings = param_bindings_pbo ).

    COMMIT WORK.

    MESSAGE |Test data "{ test_data_id }" created| TYPE 'I'.

  ENDMETHOD.


  METHOD copy_without_execution.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab.

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name        = fm_name
        test_data_id   = nummer
      IMPORTING
        datadir_entry  = DATA(datadir_entry)
        attributes     = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo ).

    DATA(test_data_id) = zcl_fm_test_data=>create_without_execution(
      EXPORTING
        fm_name        = fm_name
        title          = |Copy of test data "{ nummer }"|
        param_bindings = param_bindings_pbo
        lower_case     = attributes-lower_case ).

    COMMIT WORK.

    MESSAGE |Test data "{ test_data_id }" created| TYPE 'I'.

  ENDMETHOD.


  METHOD display.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab,
          param_bindings_pai TYPE abap_func_parmbind_tab.

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name            = fm_name
        test_data_id       = nummer
      IMPORTING
        datadir_entry      = DATA(datadir_entry)
        attributes         = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo
        param_bindings_pai = param_bindings_pai ).

    DATA(xml) = concat_lines_of( sep = || table = VALUE string_table(
        ( |<root>| )
        ( |<PBO>| )
        ( LINES OF VALUE #(
            FOR <param_binding> IN param_bindings_pbo
            ( get_xml( data_name = <param_binding>-name data_ref = <param_binding>-value ) ) ) )
        ( |</PBO>| )
        ( |<PAI>| )
        ( LINES OF VALUE #(
            FOR <param_binding> IN param_bindings_pai
            ( get_xml( data_name = <param_binding>-name data_ref = <param_binding>-value ) ) ) )
        ( |</PAI>| )
        ( |</root>| ) ) ).

    cl_demo_output=>display_xml( xml ).

  ENDMETHOD.


  METHOD delete.

    zcl_fm_test_data=>delete(
        fm_name      = fm_name
        test_data_id = nummer ).

    COMMIT WORK.

    MESSAGE |Test data "{ nummer }" deleted| TYPE 'I'.

  ENDMETHOD.


  METHOD display_raw_internal_format.

    DATA: eufunc          TYPE eufunc,
          lx_expimp_table TYPE REF TO zcx_expimp_table.

    SELECT SINGLE pname FROM tfdir WHERE funcname = @fm_name INTO @DATA(pname).
    DATA(fugr_name) = replace( val = pname sub = 'SAPL' with = `` ).

    eufunc = VALUE eufunc(
        relid   = 'FL'
        gruppe  = fugr_name
        name    = fm_name
        nummer  = nummer ).

    TRY.
        zcl_expimp_table=>import_all(
          EXPORTING
            tabname  = 'EUFUNC'
            area     = 'FL'
            id_new   = eufunc
          IMPORTING
            tab_cpar = DATA(tab_cpar) ).
      CATCH zcx_expimp_table INTO lx_expimp_table.
        MESSAGE lx_expimp_table TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    eufunc = VALUE eufunc(
        relid   = 'FL'
        gruppe  = fugr_name
        name    = fm_name
        nummer  = '999' ).

    TRY.
        zcl_expimp_table=>import_all(
          EXPORTING
            tabname  = 'EUFUNC'
            area     = 'FL'
            id_new   = eufunc
          IMPORTING
            tab_cpar = DATA(tab_cpar_999) ).
      CATCH zcx_expimp_table INTO lx_expimp_table.
        MESSAGE lx_expimp_table TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    DATA datadir_entries TYPE zcl_fm_test_data=>ty_datadir.
    FIELD-SYMBOLS <datadir_entries> TYPE STANDARD TABLE.
    ASSIGN tab_cpar_999[ name = 'TE_DATADIR' ] TO FIELD-SYMBOL(<cpar_datadir>).
    ASSIGN <cpar_datadir>-dref->* TO <datadir_entries>.
    datadir_entries = <datadir_entries>.

    DATA(xml) = concat_lines_of( sep = || table = VALUE string_table(
        ( |<root>| )
        ( |<parameters>| )
        ( LINES OF VALUE #(
            FOR <cpar> IN tab_cpar
            ( get_xml( data_name = <cpar>-name data_ref = <cpar>-dref ) ) ) )
        ( |</parameters>| )
        ( get_xml( data_name = 'DATADIR_ENTRY' data_ref = REF #( datadir_entries[ dataid = nummer ] ) ) )
        ( |</root>| ) ) ).

    cl_demo_output=>display_xml( xml ).

  ENDMETHOD.


  METHOD get_xml.

    ASSIGN data_ref->* TO FIELD-SYMBOL(<value>).
    TRY.
        CALL TRANSFORMATION id
            SOURCE
                data = <value>
            RESULT
                XML DATA(xml_xstring)
            OPTIONS
                technical_types = 'ignore'
                initial_components = 'suppress'.
      CATCH cx_transformation_error INTO DATA(lx_transformation_error).
        MESSAGE lx_transformation_error TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    DATA lo_doc TYPE REF TO if_ixml_document.
    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = xml_xstring
      IMPORTING
        document = lo_doc
      EXCEPTIONS
        OTHERS   = 1.
    DATA(lo_node) = lo_doc->find_from_path_ns( default_uri = '' path =
           |/"http://www.sap.com/abapxml:abap"|
        && |/"http://www.sap.com/abapxml:values"|
        && |/DATA| ).
    xml = xml && |<element name="{ escape( val = data_name format = cl_abap_format=>e_html_attr ) }">|.
    DATA(lo_children) = lo_node->get_children( ).
    DO lo_node->num_children( ) TIMES.
      DATA(new_xml_string) = VALUE string( ).
      lo_children->get_item( index = sy-index - 1 )->render( cl_ixml=>create( )->create_stream_factory( )->create_ostream_cstring( new_xml_string ) ).
      xml = xml && new_xml_string.
    ENDDO.
    xml = xml && |</element>|.

  ENDMETHOD.


ENDCLASS.

TABLES sscrfields.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS block1 RADIOBUTTON GROUP rb2 default 'X' USER-COMMAND ENTER.
SELECTION-SCREEN COMMENT (79) txt_blk1 FOR FIELD block1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt0 RADIOBUTTON GROUP rb3 DEFAULT 'X' MODIF ID b1.
PARAMETERS opt00 RADIOBUTTON GROUP rb3 MODIF ID hid.
SELECTION-SCREEN COMMENT (79) txt_opt0 FOR FIELD opt0 MODIF ID b1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS block2 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN COMMENT (79) txt_blk2 FOR FIELD block2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) fm_text.
PARAMETERS fm_name TYPE tfdir-funcname DEFAULT 'Z_FM_TEST_DATA_TEST' MODIF ID b2.
SELECTION-SCREEN PUSHBUTTON (20) txt_se37 USER-COMMAND se37 MODIF ID b2.
SELECTION-SCREEN END OF LINE.
PARAMETERS nummer TYPE eudatadir-nummer MODIF ID b2.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt1 RADIOBUTTON GROUP rb1 MODIF ID b2.
SELECTION-SCREEN COMMENT (79) txt_opt1 FOR FIELD opt1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt2 RADIOBUTTON GROUP rb1 MODIF ID b2.
SELECTION-SCREEN COMMENT (79) txt_opt2 FOR FIELD opt2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt3 RADIOBUTTON GROUP rb1 MODIF ID b2.
SELECTION-SCREEN COMMENT (79) txt_opt3 FOR FIELD opt3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt4 RADIOBUTTON GROUP rb1 MODIF ID b2.
SELECTION-SCREEN COMMENT (79) txt_opt4 FOR FIELD opt4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt5 RADIOBUTTON GROUP rb1 MODIF ID b2.
SELECTION-SCREEN COMMENT (79) txt_opt5 FOR FIELD opt5.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b02.

INITIALIZATION.
  txt_blk1 = 'Operations on function module Z_FM_TEST_DATA_TEST only'.
  txt_opt0 = 'Create fixed test data'.

  txt_blk2 = 'Operations on any function module and any test data'.
  fm_text  = 'Function module'.
  txt_opt1 = 'Execute given test data and create a new one including FM output data'.
  txt_opt2 = 'Copy given function module test data'.
  txt_opt3 = 'Display'.
  txt_opt4 = 'Delete'.
  txt_opt5 = 'Display raw internal format'.

  txt_se37 = 'SE37 Test Tool'(009).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR nummer.
  zcl_fm_test_data=>f4_help_test_data_id(
      dyname                  = sy-repid
      dynumb                  = sy-dynnr
      fm_field_name           = 'FM_NAME'
      test_data_id_field_name = 'NUMMER' ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'HID'.
        screen-active = '0'.
      WHEN 'B1'.
        IF block2 = 'X'.
          screen-input = '0'.
        ELSE.
          screen-input = '1'.
        ENDIF.
      WHEN 'B2'.
        IF block1 = 'X'.
          screen-input = '0'.
        ELSE.
          screen-input = '1'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
  IF block1 = 'X'.
    SET CURSOR FIELD 'OPT0'.
  ELSE.
    SET CURSOR FIELD 'FM_NAME'.
  ENDIF.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      CASE sscrfields-ucomm.
        WHEN 'SE37'.
          SUBMIT rs_testframe_call WITH funcn = fm_name AND RETURN.
      ENDCASE.
  ENDCASE.

START-OF-SELECTION.
  IF block1 = abap_true.
    CLEAR: opt1, opt2, opt3, opt4, opt5.
  ELSE.
    CLEAR: opt0.
  endif.
  DATA(options) = VALUE trext_c1( ( opt0 ) ( opt1 ) ( opt2 ) ( opt3 ) ( opt4 ) ( opt5 ) ).
  CONCATENATE LINES OF options INTO DATA(options2) RESPECTING BLANKS.
  TRY.
      NEW lcl_app(
        fm_name = fm_name
        nummer  = CONV #( nummer )
        option  = find( val = options2 sub = 'X' )
        )->main( ).
    CATCH zcx_fm_test_data INTO DATA(lx_fm_test_data).
      MESSAGE lx_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
