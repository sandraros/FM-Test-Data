"! <p class="shorttext synchronized" lang="en">Utility for Function Module Test Data</p>
CLASS zcl_fm_test_data DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_datadir_entry,
        dataid TYPE eufunc-nummer,    "Schlüsselnr. für Testdatenimp.
        "! <ul>
        "! <li>PBO: Test data contains only parameter values before call</li>
        "! <li>PAI: Test data contains parameter values before and after call</li>
        "! </ul>
        stepid TYPE c LENGTH 3,
        "! <ul>
        "! <li>' ': Not a test sequence</li>
        "! <li>'S': Test sequence (not supported by ZCL_FM_TEST_DATA)</li>
        "! </ul>
        seqid  TYPE eufunc-seqid,
        datum  TYPE sy-datum,
        uzeit  TYPE sy-uzeit,
        title  TYPE c LENGTH 40,
      END OF ty_datadir_entry .
    TYPES:
      ty_datadir TYPE STANDARD TABLE OF ty_datadir_entry WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_fdesc_entry,
        "! Field name
        name    TYPE c LENGTH 30,
        "! Table name
        table   TYPE c LENGTH 40,
        "! Data type
        type    TYPE c LENGTH 1,
        "! Length
        length  TYPE c LENGTH 5,
        "! Input length
        ilength TYPE c LENGTH 5,
        "! Input length
        hlength TYPE p LENGTH 8 DECIMALS 0,
        "! Parameter type<ul>
        "! <li>I: IMPORT</li>
        "! <li>E: EXPORT</li>
        "! <li>C: CHANGING </li>
        "! <li>S: STRUCTURE</li>
        "! <li>Y: TYPE</li>
        "! <li>T: TABLE</li>
        "! </ul>
        ftype   TYPE c LENGTH 3,
        "! Reference, wessen Struktur
        number  TYPE i,
      END OF ty_fdesc_entry .
    TYPES:
      ty_fdesc TYPE STANDARD TABLE OF ty_fdesc_entry WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_fdesc_copy_entry,
        line TYPE ty_fdesc_entry,
      END OF ty_fdesc_copy_entry .
    TYPES:
      ty_fdesc_copy TYPE STANDARD TABLE OF ty_fdesc_copy_entry WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_test_attr,
        "! Author
        author         TYPE syuname,
        "! Date
        datum          TYPE d,
        "! Time
        zeit           TYPE t,
        "! Version
        version        TYPE c LENGTH 4,
        "! Duration in microseconds (TIME1)
        duration       TYPE p LENGTH 8 DECIMALS 0,
        "! Return code (V_RC)
        rc             TYPE i,
        "! Exception name (VEXCEPTION)
        exception_name TYPE c LENGTH 30,
        "! Upper case (G_UPPER)
        lower_case     TYPE c LENGTH 1,
      END OF ty_test_attr .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter title | <p class="shorttext synchronized" lang="en">Title of test data</p>
    "! @parameter param_bindings | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter lower_case | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter test_data_id | <p class="shorttext synchronized" lang="en">ID of the test data created</p>
    CLASS-METHODS create_without_execution
      IMPORTING
        !fm_name            TYPE tfdir-funcname
        !title              TYPE ty_datadir_entry-title
        !param_bindings     TYPE abap_func_parmbind_tab
        !lower_case         TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(test_data_id) TYPE eufunc-nummer
      RAISING
        zcx_fm_test_data .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter title | <p class="shorttext synchronized" lang="en">Title of test data</p>
    "! @parameter param_bindings | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter lower_case | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter test_data_id | <p class="shorttext synchronized" lang="en">ID of the test data created</p>
    CLASS-METHODS execute_and_create
      IMPORTING
        !fm_name            TYPE tfdir-funcname
        !title              TYPE ty_datadir_entry-title
        !param_bindings     TYPE abap_func_parmbind_tab
        !lower_case         TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(test_data_id) TYPE eufunc-nummer
      RAISING
        zcx_fm_test_data .
    "! NB: LOAD supports the fact that the function module does not exist anymore, or
    "! has been moved to another function group since then.
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter test_data_id | <p class="shorttext synchronized" lang="en">ID of the test data created</p>
    "! @parameter datadir_entry | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter attributes | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter param_bindings_pbo | <p class="shorttext synchronized" lang="en"></p>
    "!      | Values of arguments passed to the function module (call)
    "! @parameter param_bindings_pai | <p class="shorttext synchronized" lang="en"></p>
    "!      | Values of parameters returned after the function module call
    CLASS-METHODS load
      IMPORTING
        VALUE(fugr_name)    TYPE tlibg-area OPTIONAL
        !fm_name            TYPE tfdir-funcname
        !test_data_id       TYPE numeric
      EXPORTING
        !datadir_entry      TYPE ty_datadir_entry
        !attributes         TYPE ty_test_attr
      CHANGING
        !param_bindings_pbo TYPE abap_func_parmbind_tab OPTIONAL
        !param_bindings_pai TYPE abap_func_parmbind_tab OPTIONAL
      RAISING
        zcx_fm_test_data .

    CLASS-METHODS load_as_xml
      IMPORTING
        VALUE(fugr_name) TYPE tlibg-area OPTIONAL
        !fm_name         TYPE tfdir-funcname
        !test_data_id    TYPE numeric
      RETURNING
        VALUE(xml)       TYPE string
      RAISING
        zcx_fm_test_data .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter test_data_id | <p class="shorttext synchronized" lang="en">ID of the test data created</p>
    CLASS-METHODS delete
      IMPORTING
        !fm_name      TYPE tfdir-funcname
        !test_data_id TYPE numeric
      RAISING
        zcx_fm_test_data .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_field_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter test_data_id_field_name | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS f4_help_test_data_id
      IMPORTING
        VALUE(dyname)           TYPE syrepid
        VALUE(dynumb)           TYPE sydynnr
        fm_field_name           TYPE csequence
        test_data_id_field_name TYPE csequence.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_context,
             te_datadir            TYPE ty_datadir,
             fdesc_copy            TYPE ty_fdesc_copy,
             struc_info_table_copy TYPE nf2ty_struc_info_table,
             g_no_save             TYPE c LENGTH 1,
             d102_fname            TYPE rs38l_fnam,
           END OF ty_context,
           BEGIN OF ty_us_rtts,
             value TYPE REF TO data,
             name  TYPE string,
           END OF ty_us_rtts,
           ty_ut_rtts TYPE STANDARD TABLE OF ty_us_rtts WITH DEFAULT KEY.

    CLASS-METHODS load_test_context
      IMPORTING
        fm_name        TYPE tfdir-funcname
      RETURNING
        VALUE(context) TYPE ty_context
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS save_test_context
      IMPORTING
        fm_name TYPE tfdir-funcname
        context TYPE ty_context.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter fugr_name | <p class="shorttext synchronized" lang="en">Function group or empty if the function module doesn't exist</p>
    CLASS-METHODS get_fugr_name
      IMPORTING
        fm_name          TYPE tfdir-funcname
      RETURNING
        VALUE(fugr_name) TYPE eufunc-gruppe.

    CLASS-METHODS get_free_test_data_id
      IMPORTING
        fm_name             TYPE tfdir-funcname
      RETURNING
        VALUE(test_data_id) TYPE eufunc-nummer.

    CLASS-METHODS save
      IMPORTING
        fm_name             TYPE tfdir-funcname
        title               TYPE ty_datadir_entry-title
        stepid              TYPE ty_datadir_entry-stepid
        new_param_bindings  TYPE abap_func_parmbind_tab
        param_bindings_pbo  TYPE abap_func_parmbind_tab
        attributes          TYPE ty_test_attr
      RETURNING
        VALUE(test_data_id) TYPE eufunc-nummer
      RAISING
        zcx_fm_test_data.

    "! Receives PARAM_BINDINGS = table of values of function module parameters, except those in the
    "!   FM signature which are of category Exporting. Parameters to which initial values are to be
    "!   passed don't need to be defined in the table.
    "! Returns:
    "!   1) NEW_PARAM_BINDINGS = the lines from PARAM_BINDINGS + all missing parameters of the
    "!      given function module, whatever their categories (Importing, Exporting, Changing, Tables).
    "!   2) PARAM_BINDINGS_PBO =
    CLASS-METHODS complete_param_bindings
      IMPORTING
        fm_name            TYPE tfdir-funcname
        stepid             TYPE ty_datadir_entry-stepid
        param_bindings     TYPE abap_func_parmbind_tab
      EXPORTING
        new_param_bindings TYPE abap_func_parmbind_tab
        param_bindings_pbo TYPE abap_func_parmbind_tab
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS get_fm_params_rtts
      IMPORTING
        funcname           TYPE tfdir-funcname
      RETURNING
        VALUE(params_rtts) TYPE zcl_fm_params_rtts=>ty_params_rtts
      RAISING
        zcx_fm_test_data.

ENDCLASS.



CLASS zcl_fm_test_data IMPLEMENTATION.


  METHOD complete_param_bindings.

    DATA: ref_parameter     TYPE REF TO data,
          ref_parameter_pbo TYPE REF TO data.
    FIELD-SYMBOLS:
      <param_binding> TYPE abap_func_parmbind.

    DATA(params_rtts) = get_fm_params_rtts( funcname = fm_name ).

    new_param_bindings = VALUE #( ).
    param_bindings_pbo = VALUE #( ).

    LOOP AT params_rtts REFERENCE INTO DATA(param_rtts).

      UNASSIGN <param_binding>.
      IF param_rtts->call_function_kind <> abap_func_importing.
        " All those values to be passed to the function module
        " with Call Function '...' exporting ... tables ... changing ...
        ASSIGN param_bindings[ name = param_rtts->name ] TO <param_binding>.
        IF sy-subrc = 0 AND <param_binding>-value IS BOUND.
          param_bindings_pbo = VALUE #( BASE param_bindings_pbo
                ( <param_binding> ) ).
        ELSE.
          CREATE DATA ref_parameter_pbo TYPE HANDLE param_rtts->type.
          param_bindings_pbo = VALUE #( BASE param_bindings_pbo
                ( name  = param_rtts->name
                  value = ref_parameter_pbo ) ).
        ENDIF.
      ENDIF.

      " All parameters
      " for Call Function '...' exporting ... importing ... tables ... changing ...
      CREATE DATA ref_parameter TYPE HANDLE param_rtts->type.
      IF stepid = 'PAI' AND <param_binding> IS ASSIGNED AND param_rtts->call_function_kind <> abap_func_importing.
        ASSIGN <param_binding>-value->* TO FIELD-SYMBOL(<input_parameter>).
        ASSIGN ref_parameter->* TO FIELD-SYMBOL(<parameter_value>).
        <parameter_value> = <input_parameter>.
      ENDIF.
      new_param_bindings = VALUE #( BASE new_param_bindings
          ( name  = param_rtts->name
            kind  = param_rtts->call_function_kind
            value = ref_parameter ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD create_without_execution.

    complete_param_bindings(
      EXPORTING
        fm_name            = fm_name
        stepid             = 'PBO'
        param_bindings     = param_bindings
      IMPORTING
        new_param_bindings = DATA(new_param_bindings)
        param_bindings_pbo = DATA(param_bindings_pbo) ).

    test_data_id = save(
                    fm_name            = fm_name
                    title              = title
                    stepid             = 'PBO'
                    new_param_bindings = new_param_bindings
                    param_bindings_pbo = param_bindings_pbo
                    attributes         = VALUE #( lower_case = lower_case ) ).

  ENDMETHOD.


  METHOD delete.

    DATA(not_found) = 0.

    DATA(context) = load_test_context( fm_name ).

    DELETE context-te_datadir WHERE dataid = test_data_id.
    IF sy-subrc <> 0.
      not_found = not_found + 1.
    ENDIF.

    save_test_context( fm_name = fm_name context = context ).

    DATA(fugr_name) = get_fugr_name( fm_name ).
    DELETE FROM eufunc
        WHERE relid   = 'FL'
          AND gruppe  = fugr_name
          AND name    = fm_name
          AND nummer  = test_data_id.
    IF sy-subrc <> 0.
      not_found = not_found + 1.
    ENDIF.

    IF not_found = 2.
      RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Test data "{ test_data_id }" not found for "{ fm_name }"|.
    ENDIF.

  ENDMETHOD.


  METHOD execute_and_create.

    TYPES: ty_p TYPE p LENGTH 8 DECIMALS 0.
    DATA: time1 TYPE ty_p,
          time2 TYPE ty_p.

    " PARAMETERS
    complete_param_bindings(
      EXPORTING
        fm_name            = fm_name
        stepid             = 'PAI'
        param_bindings     = param_bindings
      IMPORTING
        new_param_bindings = DATA(new_param_bindings)
        param_bindings_pbo = DATA(param_bindings_pbo) ).

    " EXCEPTIONS
    SELECT parameter FROM fupararef
        WHERE funcname  = @fm_name
          AND paramtype = 'X'
        INTO TABLE @DATA(fm_exceptions).
    DATA(exceptions) = VALUE abap_func_excpbind_tab(
        ( LINES OF VALUE #( FOR <fm_exception> IN fm_exceptions INDEX INTO row_number
        ( name = <fm_exception>-parameter value = row_number ) ) )
        ( name = 'ERROR_MESSAGE' value = lines( fm_exceptions ) + 1 )
        ( name = 'OTHERS' value = lines( fm_exceptions ) + 2 ) ).

    GET RUN TIME FIELD time1.

    CALL FUNCTION fm_name
      PARAMETER-TABLE
      new_param_bindings
      EXCEPTION-TABLE
      exceptions.

    DATA(subrc) = sy-subrc.
    GET RUN TIME FIELD time2.

    time1 = time2 - time1.

    save( fm_name            = fm_name
          title              = title
          stepid             = 'PAI'
          new_param_bindings = new_param_bindings
          param_bindings_pbo = param_bindings_pbo
          attributes         = VALUE #(
                duration        = time1
                rc              = subrc
                exception_name  = COND #( WHEN subrc <> 0 THEN exceptions[ value = subrc ]-name )
                lower_case      = lower_case ) ).

  ENDMETHOD.


  METHOD f4_help_test_data_id.

    TYPES: tt_datadir TYPE STANDARD TABLE OF eudatadir WITH EMPTY KEY.

    DATA(lt_dynpfield) = VALUE dynpread_tabtype( ( fieldname = fm_field_name ) ).
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = dyname
        dynumb     = dynumb
      TABLES
        dynpfields = lt_dynpfield
      EXCEPTIONS
        OTHERS     = 9.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_datadir) = VALUE tt_datadir( ).
    CALL FUNCTION 'RS_TESTDATA_DIRECTORY_GET'
      EXPORTING
        functionname       = CONV rs38l-name( lt_dynpfield[ fieldname = fm_field_name ]-fieldvalue )
      TABLES
        te_datadir         = lt_datadir
      EXCEPTIONS
        function_not_found = 1
        no_data            = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = test_data_id_field_name
        retfield    = 'NUMMER' " column name in VALUE_TAB
        value_org   = 'S'
      TABLES
        value_tab   = lt_datadir.

  ENDMETHOD.


  METHOD get_fm_params_rtts.

    TRY.
        params_rtts = zcl_fm_params_rtts=>get( funcname = funcname ).
      CATCH zcx_fm_params_rtts.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Interface error for "{ funcname }"|.
    ENDTRY.

    " Replace generic types
    LOOP AT params_rtts REFERENCE INTO DATA(param_rtts).
      IF NOT param_rtts->type->is_instantiatable( ).
        CASE param_rtts->type->type_kind.
          WHEN cl_abap_typedescr=>typekind_char
                OR cl_abap_typedescr=>typekind_clike
                OR cl_abap_typedescr=>typekind_csequence
                OR cl_abap_typedescr=>typekind_any.
            param_rtts->type = cl_abap_elemdescr=>get_c( p_length = 200 ).
          WHEN cl_abap_typedescr=>typekind_packed.
            param_rtts->type = cl_abap_elemdescr=>get_p( p_length = 16 p_decimals = 0 ).
          WHEN cl_abap_typedescr=>typekind_hex.
            param_rtts->type = cl_abap_elemdescr=>get_x( p_length = 200 ).
          WHEN cl_abap_typedescr=>typekind_num.
            param_rtts->type = cl_abap_elemdescr=>get_n( p_length = 200 ).
          WHEN cl_abap_typedescr=>typekind_decfloat.
            param_rtts->type = cl_abap_elemdescr=>get_decfloat34( ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Unsupported data type "{ param_rtts->type->type_kind }" for "{ funcname }"|.
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_free_test_data_id.

    " TODO simulate the standard, which looks for gaps between numbers.

    TYPES ty_p TYPE p LENGTH 10 DECIMALS 0.

    DATA(fugr_name) = get_fugr_name( fm_name ).

    SELECT MAX( nummer )
        FROM eufunc
        WHERE relid  = 'FL'
          AND gruppe = @fugr_name
          AND name   = @fm_name
          AND nummer <> '999'
        INTO @test_data_id.

    DATA(test_data_id_p) = CONV ty_p( test_data_id + 1 ).
    test_data_id = test_data_id_p.

  ENDMETHOD.


  METHOD get_fugr_name.

    SELECT SINGLE pname
        FROM tfdir
        WHERE funcname = @fm_name
        INTO @fugr_name.
    REPLACE 'SAPL' IN fugr_name WITH ``.

  ENDMETHOD.


  METHOD load.

    DATA: parameter_name TYPE string,
          cpar           TYPE REF TO cpar.
    FIELD-SYMBOLS:
      <param_binding>       TYPE abap_func_parmbind,
      <cpar_value>          TYPE any,
      <param_binding_value> TYPE any.


    IF fugr_name IS INITIAL.
      fugr_name = get_fugr_name( fm_name ).
    ENDIF.

    " DATAID: number formatted like I = right-aligned and sign character at rightmost position
    DATA(dataid) = CONV eufunc-nummer( CONV i( test_data_id ) ).

    IF fugr_name IS NOT INITIAL.
      SELECT COUNT(*) FROM eufunc
          WHERE relid  = 'FL'
            AND gruppe = @fugr_name
            AND name   = @fm_name
            AND nummer = @dataid.
      IF sy-subrc <> 0.
        CLEAR fugr_name.
      ENDIF.
    ENDIF.

    IF fugr_name IS INITIAL.
      SELECT SINGLE gruppe FROM eufunc
          WHERE relid  = 'FL'
            AND name   = @fm_name
            AND nummer = @dataid
          INTO @fugr_name.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Test data "{ dataid }" not found for "{ fm_name }"|.
      ENDIF.
    ENDIF.

    TRY.
        DATA(eufunc) = VALUE eufunc( ).
        zcl_expimp_table=>import_all(
          EXPORTING
            tabname  = 'EUFUNC'
            area     = 'FL'
            id       = VALUE functdir(
                        area   = fugr_name
                        progid = fm_name
                        dataid = dataid )
          IMPORTING
            tab_cpar = DATA(tab_cpar)
            wa       = eufunc ).
      CATCH zcx_expimp_table INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Export to EUFUNC failed| previous = lx.
    ENDTRY.

    attributes = VALUE ty_test_attr(
            author  = eufunc-autor
            datum   = eufunc-datum
            zeit    = eufunc-zeit
            version = eufunc-version ).

    FIELD-SYMBOLS <cpar> TYPE cpar.
    ASSIGN tab_cpar[ name = 'TIME1' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<time1>).
    ASSIGN tab_cpar[ name = 'V_RC' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<v_rc>).
    ASSIGN tab_cpar[ name = 'VEXCEPTION' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<vexception>).
    ASSIGN tab_cpar[ name = 'G_UPPER' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<g_upper>).
    attributes = VALUE ty_test_attr(
        BASE attributes
        duration       = <time1>
        rc             = <v_rc>
        exception_name = <vexception>
        lower_case     = xsdbool( <g_upper> = abap_false ) ).
    DELETE tab_cpar
        WHERE name = 'TIME1'
           OR name = 'V_RC'
           OR name = 'VEXCEPTION'
           OR name = 'G_UPPER'.

    SORT tab_cpar BY name.

    LOOP AT tab_cpar REFERENCE INTO cpar.
      IF cpar->name CP '%_I*'.
        INSERT VALUE abap_func_parmbind(
                kind  = COND #( WHEN line_exists( tab_cpar[ name = |%_O{ cpar->name+3 }| ] )
                                  THEN abap_func_changing
                                WHEN line_exists( tab_cpar[ name = |%_V{ cpar->name+3 }| ] )
                                  THEN abap_func_tables
                                ELSE abap_func_exporting )
                name  = cpar->name+3
                value = cpar->dref )
            INTO TABLE param_bindings_pbo.
      ELSEIF cpar->name CP '%_V*'.
        INSERT VALUE abap_func_parmbind(
                kind  = COND #( WHEN line_exists( tab_cpar[ name = |%_O{ cpar->name+3 }| ] )
                                  THEN abap_func_changing
                                WHEN line_exists( tab_cpar[ name = |%_I{ cpar->name+3 }| ] )
                                  THEN abap_func_importing
                                ELSE abap_func_tables )
                name  = cpar->name+3
                value = cpar->dref )
            INTO TABLE param_bindings_pai.
      ENDIF.
    ENDLOOP.

    TRY.
        DATA(eufunc_dir) = VALUE eufunc( ).
        zcl_expimp_table=>import_all(
          EXPORTING
            tabname  = 'EUFUNC'
            area     = 'FL'
            id       = VALUE functdir(
                        area   = fugr_name
                        progid = fm_name
                        dataid = '999' )
          IMPORTING
            tab_cpar = DATA(tab_cpar_dir) ).
      CATCH zcx_expimp_table INTO lx.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Export to EUFUNC failed| previous = lx.
    ENDTRY.

    ASSIGN tab_cpar_dir[ name = 'TE_DATADIR' ] TO <cpar>.
    FIELD-SYMBOLS <te_datadir> TYPE STANDARD TABLE.
    ASSIGN <cpar>-dref->* TO <te_datadir>.
    datadir_entry = <te_datadir>[ ('CMP00001') = dataid ].

  ENDMETHOD.


  METHOD load_as_xml.

    DATA: param_bindings_pbo TYPE abap_func_parmbind_tab,
          param_bindings_pai TYPE abap_func_parmbind_tab.
    FIELD-SYMBOLS:
      <param_binding>       TYPE abap_func_parmbind,
      <cpar_value>          TYPE any,
      <param_binding_value> TYPE any.

    load(
      EXPORTING
        fugr_name          = fugr_name
        fm_name            = fm_name
        test_data_id       = test_data_id
      IMPORTING
        datadir_entry      = DATA(datadir_entry)
        attributes         = DATA(attributes)
      CHANGING
        param_bindings_pbo = param_bindings_pbo
        param_bindings_pai = param_bindings_pai ).

  ENDMETHOD.


  METHOD load_test_context.

    DATA(local_params_rtts) = get_fm_params_rtts( fm_name ).

    DATA(params_rtts) = VALUE ty_ut_rtts( ).
    LOOP AT local_params_rtts REFERENCE INTO DATA(local_param_rtts).
      DATA(param_rtts) = VALUE ty_us_rtts( name = local_param_rtts->name ).
      CREATE DATA param_rtts-value TYPE HANDLE local_param_rtts->type.
      APPEND param_rtts TO params_rtts.
    ENDLOOP.

    DATA(fugr_name) = get_fugr_name( fm_name ).

    DATA(d102n_exportkey) = VALUE functdir(
        area   = fugr_name
        progid = fm_name
        dataid = '999'
        seqid  = ' ' ).

    IMPORT  te_datadir            TO context-te_datadir
            fdesc_copy            TO context-fdesc_copy
            struc_info_table_copy TO context-struc_info_table_copy
            g_no_save             TO context-g_no_save
            d102_fname            TO context-d102_fname
        FROM DATABASE eufunc(fl)
        ID d102n_exportkey.

    IF sy-subrc <> 0.
      " FDESC is initialized by subroutine IN_DESCRIBE_INTERFACE in SAPLSEUJ, for
      " all parameters and exceptions.
      " The list of parameters are initially retrieved via function module FUNCTION_IMPORT_DOKU.
      DATA(fdesc) = VALUE ty_fdesc( ).
      " STRUC_INFO_TABLE is initialized by subroutine IN_DESCRIBE_INTERFACE in SAPLSEUJ,
      " by calling the function module RS_COMPLEX_OBJECT_TYPEINFO_GET for all parameters
      " except EXPORTING parameters.
      context-struc_info_table_copy = VALUE #( ).
      NEW lcl_saplseuj_redef( )->in_describe_interface(
        EXPORTING
          p_fname               = fm_name
          params_rtts           = params_rtts
        IMPORTING
          struc_info_table_copy = context-struc_info_table_copy
          fdesc2                = fdesc ).
      context-fdesc_copy = fdesc.
      " G_NO_SAVE = ' ' if test data is possible, = 'X' if test data is not possible.
      context-g_no_save = ' '.
      " D102_FNAME
      context-d102_fname = fm_name.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    DATA(fugr_name) = get_fugr_name( fm_name ).

    test_data_id = get_free_test_data_id( fm_name ).

    DATA(tab_cpar) = VALUE tab_cpar(
        " 1) Values before calling the function modules
        "    call function '...' exporting ... importing ... tables ... changing ...
        "                            X                          X           X
        ( LINES OF VALUE #(
            FOR <param_binding2> IN param_bindings_pbo
            ( name = '%_I' && <param_binding2>-name dref = <param_binding2>-value ) ) )
        " 2) Values after calling the function modules
        "    call function '...' exporting ... importing ... tables ... changing ...
        "                                          X            X           X
        ( LINES OF COND #( WHEN stepid = 'PAI' THEN VALUE #(
            ( LINES OF VALUE #(
                FOR <param_binding2> IN new_param_bindings
                WHERE ( kind = abap_func_changing )
                ( name = '%_O' && <param_binding2>-name dref = <param_binding2>-value )
                ( name = '%_V' && <param_binding2>-name dref = <param_binding2>-value ) ) )
            ( LINES OF VALUE #(
                FOR <param_binding2> IN new_param_bindings
                WHERE ( kind = abap_func_importing )
                ( name = '%_V' && <param_binding2>-name dref = <param_binding2>-value ) ) )
            ( LINES OF VALUE #(
                FOR <param_binding2> IN new_param_bindings
                WHERE ( kind = abap_func_tables )
                ( name = '%_V' && <param_binding2>-name dref = <param_binding2>-value ) ) ) ) ) )
        ( name = 'TIME1'      dref = NEW ty_test_attr-duration( attributes-duration ) )
        ( name = 'V_RC'       dref = NEW ty_test_attr-rc( attributes-rc ) )
        ( name = 'VEXCEPTION' dref = NEW ty_test_attr-exception_name( attributes-exception_name ) )
        ( name = 'G_UPPER'    dref = NEW ty_test_attr-lower_case( xsdbool( attributes-lower_case = abap_false ) ) ) ).

    TRY.
        zcl_expimp_table=>export_all(
          EXPORTING
            tabname  = 'EUFUNC'
            area     = 'FL'
            id       = VALUE functdir(
                        area    = fugr_name
                        progid  = fm_name
                        dataid  = test_data_id )
            wa       = VALUE eufunc(
                        langu   = ' '
                        autor   = sy-uname
                        datum   = sy-datum
                        zeit    = sy-uzeit
                        version = '  1 ' )
            tab_cpar = tab_cpar ).
      CATCH zcx_expimp_table INTO DATA(lx).
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Export to EUFUNC failed| previous = lx.
    ENDTRY.


    DATA(context) = load_test_context( fm_name ).

    DELETE context-te_datadir WHERE dataid = test_data_id.
    context-te_datadir = VALUE #(
        BASE context-te_datadir
        ( VALUE #(
          dataid = test_data_id
          stepid = stepid
          seqid  = ''
          datum  = sy-datum
          uzeit  = sy-uzeit
          title  = title ) ) ).

    save_test_context( fm_name = fm_name context = context ).

  ENDMETHOD.


  METHOD save_test_context.

    DATA(fugr_name) = get_fugr_name( fm_name ).

    DATA(d102n_exportkey) = VALUE functdir(
        area   = fugr_name
        progid = fm_name
        dataid = '999'
        seqid  = ' ' ).

    EXPORT
        te_datadir            FROM context-te_datadir
        fdesc_copy            FROM context-fdesc_copy
        struc_info_table_copy FROM context-struc_info_table_copy
        g_no_save             FROM context-g_no_save
        d102_fname            FROM context-d102_fname
      TO DATABASE eufunc(fl)
      ID d102n_exportkey.


  ENDMETHOD.
ENDCLASS.
