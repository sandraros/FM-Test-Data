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
    METHODS copy_execute_and_create
      RAISING
        zcx_fm_test_data.
    METHODS import_all
      RAISING
        zcx_fm_test_data.
    METHODS query
      RAISING
        zcx_fm_test_data.
    METHODS delete
      RAISING
        zcx_fm_test_data.
    METHODS recovery
      RAISING
        zcx_fm_test_data.

    DATA: fm_name TYPE tfdir-funcname READ-ONLY,
          nummer  TYPE i READ-ONLY,
          option  TYPE i READ-ONLY.
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
        copy_execute_and_create( ).
      WHEN 4.
        import_all( ).
      WHEN 5.
        query( ).
      WHEN 6.
        delete( ).
      WHEN 7.
        recovery( ).
    ENDCASE.

  ENDMETHOD.

  METHOD create_without_execution.

    TYPES ty_tables_ TYPE STANDARD TABLE OF sflight WITH EMPTY KEY.

    zcl_fm_test_data=>create_without_execution(
      EXPORTING
        fm_name        = fm_name
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

  ENDMETHOD.

  METHOD execute_and_create.
    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab.

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name        = fm_name
        test_id        = nummer
      IMPORTING
        datadir_entry  = DATA(datadir_entry)
        attributes     = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo ).

    zcl_fm_test_data=>execute_and_create(
      EXPORTING
        fm_name        = fm_name
        title          = datadir_entry-title
        param_bindings = param_bindings_pbo ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD copy_without_execution.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab.

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name        = 'Z_FM_TEST_DATA_TEST'
        test_id        = nummer
      IMPORTING
        datadir_entry  = DATA(datadir_entry)
        attributes     = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo ).

    zcl_fm_test_data=>create_without_execution(
      EXPORTING
        fm_name        = fm_name
        title          = datadir_entry-title
        param_bindings = param_bindings_pbo ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD copy_execute_and_create.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab.

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name        = fm_name
        test_id        = nummer
      IMPORTING
        datadir_entry  = DATA(datadir_entry)
        attributes     = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo ).

    zcl_fm_test_data=>execute_and_create(
      EXPORTING
        fm_name        = fm_name
        title          = datadir_entry-title
        param_bindings = param_bindings_pbo ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD import_all.

    SELECT SINGLE * FROM tfdir WHERE funcname = @fm_name INTO @DATA(fugr).
    DATA(eufunc) = VALUE eufunc(
        relid   = 'FL'
        gruppe  = fugr
        name    = fm_name
        nummer  = nummer ).

    TRY.
        zcl_expimp_table=>import_all(
          EXPORTING
            tabname  = 'EUFUNC'
            area     = 'FL'
          IMPORTING
            tab_cpar = DATA(tab_cpar)
            wa       = eufunc ).
      CATCH zcx_expimp_table INTO DATA(lx).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    TRY.
        CALL TRANSFORMATION id
            SOURCE
                data = tab_cpar
            RESULT
                XML DATA(xstring)
            OPTIONS
                data_refs = 'heap-or-create'
                technical_types = 'ignore'
                initial_components = 'suppress'.
      CATCH cx_transformation_error INTO DATA(lx_transformation_error).
        MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    cl_demo_output=>display_xml( xstring ).

  ENDMETHOD.

  METHOD query.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab,
          param_bindings_pai TYPE abap_func_parmbind_tab.

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name            = fm_name
        test_id            = nummer
      IMPORTING
        datadir_entry      = DATA(datadir_entry)
        attributes         = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo
        param_bindings_pai = param_bindings_pai ).

    TRY.
        CALL TRANSFORMATION id
            SOURCE
                datadir_entry = datadir_entry
                attributes    = attributes
                pbo           = param_bindings_pbo
                pai           = param_bindings_pai
            RESULT
                XML DATA(xstring)
            OPTIONS
                data_refs = 'heap-or-create'
                technical_types = 'ignore'
                initial_components = 'suppress'.
      CATCH cx_transformation_error INTO DATA(lx_transformation_error).
    ENDTRY.

    cl_demo_output=>display_xml( xstring ).

  ENDMETHOD.

  METHOD delete.

    zcl_fm_test_data=>delete(
      EXPORTING
        fm_name = fm_name
        test_id = nummer ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD recovery.

    TYPES: BEGIN OF ty_arguments,
             import     TYPE string,
             bapireturn TYPE bapireturn,
             export     TYPE string,
             changing   TYPE string,
             tables_    TYPE STANDARD TABLE OF sflight WITH EMPTY KEY,
           END OF ty_arguments.
    TYPES: BEGIN OF ty_results,
             import   TYPE string,
             bapiret1 TYPE bapiret1,
             export   TYPE string,
             changing TYPE string,
             tables_  TYPE STANDARD TABLE OF sflight WITH EMPTY KEY,
           END OF ty_results.

    DATA(from_fm_name) = CONV tfdir-funcname( 'Z_FM_TEST_DATA_TEST' ).
    DATA(to_fm_name) = CONV tfdir-funcname( 'Z_FM_TEST_DATA_TEST_2' ).

    DATA(source) = VALUE ty_arguments( ).
    DATA(param_bindings_pbo) = VALUE abap_func_parmbind_tab(
          ( name = 'IMPORT'     value = REF #( source-import ) )
          ( name = 'BAPIRETURN' value = REF #( source-bapireturn ) )
          ( name = 'EXPORT'     value = REF #( source-export ) )
          ( name = 'CHANGING'   value = REF #( source-changing ) )
          ( name = 'TABLES_'    value = REF #( source-tables_ ) ) ).

    zcl_fm_test_data=>load(
      EXPORTING
        fm_name       = from_fm_name
        test_id       = nummer
      IMPORTING
        datadir_entry = DATA(datadir_entry)
        attributes    = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo ).

    DATA(results) = CORRESPONDING ty_results( source ).
    results-bapiret1 = VALUE #(
        BASE CORRESPONDING #( source-bapireturn )
        id     = source-bapireturn-code(2)
        number = source-bapireturn-code+2 ).

    zcl_fm_test_data=>create_without_execution(
      EXPORTING
        fm_name        = to_fm_name
        title          = datadir_entry-title
        param_bindings = VALUE #(
          ( name = 'IMPORT'     value = REF #( results-import ) )
          ( name = 'BAPIRET1'   value = REF #( results-bapiret1 ) )
          ( name = 'EXPORT'     value = REF #( results-export ) )
          ( name = 'CHANGING'   value = REF #( results-changing ) )
          ( name = 'TABLES_'    value = REF #( results-tables_ ) ) ) ).

    COMMIT WORK.

  ENDMETHOD.

ENDCLASS.

PARAMETERS fm_name TYPE tfdir-funcname DEFAULT 'Z_FM_TEST_DATA_TEST'.
PARAMETERS nummer TYPE i DEFAULT 1.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt0 RADIOBUTTON GROUP rb1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (80) txt_opt0.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt1 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt3 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt4 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt4.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt5 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt5.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt6 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt6.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS opt7 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (80) txt_opt7.
SELECTION-SCREEN END OF LINE.

INITIALIZATION.
  txt_opt0 = 'Create without execution in Z_FM_TEST_DATA_TEST'.
  txt_opt1 = 'Execute Z_FM_TEST_DATA_TEST and create'.
  txt_opt2 = 'Copy without execution'.
  txt_opt3 = 'Copy, execute and create'.
  txt_opt4 = 'Import all'.
  txt_opt5 = 'Query'.
  txt_opt6 = 'Delete'.
  txt_opt7 = 'Copy from Z_FM_TEST_DATA_TEST to Z_FM_TEST_DATA_TEST_2'.

START-OF-SELECTION.
  DATA(options) = VALUE trext_c1( ( opt0 ) ( opt1 ) ( opt2 ) ( opt3 ) ( opt4 ) ( opt5 ) ( opt6 ) ( opt7 ) ).
  CONCATENATE LINES OF options INTO DATA(options2) RESPECTING BLANKS.
  TRY.
      NEW lcl_app(
        fm_name = fm_name
        nummer  = nummer
        option  = find( val = options2 sub = 'X' )
        )->main( ).
    CATCH zcx_fm_test_data INTO DATA(lx_fm_test_data).
      MESSAGE lx_fm_test_data TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
