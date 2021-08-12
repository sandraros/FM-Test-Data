"! <p class="shorttext synchronized" lang="en">API for operations on Function Module Test Data</p>
CLASS zcl_fm_test_data DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_datadir_entry,
        dataid TYPE eufunc-nummer,
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
      ty_duration TYPE p LENGTH 8 DECIMALS 0,
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
        duration       TYPE ty_duration,
        "! Return code (V_RC)
        rc             TYPE i,
        "! Exception name (VEXCEPTION)
        exception_name TYPE c LENGTH 30,
        "! Upper case (G_UPPER)
        lower_case     TYPE c LENGTH 1,
      END OF ty_test_attr .
    TYPES:
      BEGIN OF ty_call_arguments,
        parameters TYPE abap_func_parmbind_tab,
        exceptions TYPE abap_func_excpbind_tab,
      END OF ty_call_arguments .

    DATA:
      fm_name           TYPE tfdir-funcname READ-ONLY,
      fugr_name         TYPE tlibg-area READ-ONLY,
      id                TYPE eufunc-nummer READ-ONLY,
      title             TYPE ty_datadir_entry-title READ-ONLY,
      input_parameters  TYPE abap_func_parmbind_tab READ-ONLY,
      result_parameters TYPE abap_func_parmbind_tab READ-ONLY,
      "! Author
      author            TYPE syuname READ-ONLY,
      "! Date
      date              TYPE d READ-ONLY,
      "! Time
      time              TYPE t READ-ONLY,
      "! Version
      version           TYPE c LENGTH 4 READ-ONLY,
      "! Duration in microseconds (TIME1)
      duration          TYPE p LENGTH 8 DECIMALS 0 READ-ONLY,
      "! Return code (V_RC)
      rc                TYPE i READ-ONLY,
      "! Exception name (VEXCEPTION)
      exception_name    TYPE c LENGTH 30 READ-ONLY,
      "! Upper case (G_UPPER)
      lower_case        TYPE c LENGTH 1 READ-ONLY,
      "! <ul>
      "! <li>PBO: Test data contains only parameter values before call</li>
      "! <li>PAI: Test data contains parameter values before and after call</li>
      "! </ul>
      stepid            TYPE c LENGTH 3 READ-ONLY.


    METHODS constructor.

    CLASS-METHODS create
      IMPORTING
        !fm_name         TYPE tfdir-funcname
        !title           TYPE ty_datadir_entry-title
      RETURNING
        VALUE(test_data) TYPE REF TO zcl_fm_test_data
      RAISING
        zcx_fm_test_data .

    CLASS-METHODS load
      IMPORTING
        fm_name          TYPE tfdir-funcname
        id               TYPE eufunc-nummer
        isolated_run     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(test_data) TYPE REF TO zcl_fm_test_data
      RAISING
        zcx_fm_test_data .

    METHODS cleanup_after_execution
      RAISING
        zcx_fm_test_data .

    METHODS execute
      RAISING
        zcx_fm_test_data .

    METHODS get_call_arguments
      RETURNING
        VALUE(call_arguments) TYPE ty_call_arguments
      RAISING
        zcx_fm_test_data .

    METHODS set_input_parameters
      IMPORTING
        !parameters TYPE abap_func_parmbind_tab .

    METHODS set_title
      IMPORTING
        title TYPE ty_datadir_entry-title .

    METHODS set_lower_case
      IMPORTING
        lower_case TYPE abap_bool DEFAULT abap_true .

    METHODS save
      RAISING
        zcx_fm_test_data .

    CLASS-METHODS delete
      IMPORTING
        !fm_name TYPE tfdir-funcname
        !id      TYPE eufunc-nummer
      RAISING
        zcx_fm_test_data .

    CLASS-METHODS f4_help_test_data_id
      IMPORTING
        VALUE(dyname)            TYPE syrepid
        VALUE(dynumb)            TYPE sydynnr
        !fm_field_name           TYPE csequence
        !test_data_id_field_name TYPE csequence .

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_us_rtts,
             value TYPE REF TO data,
             name  TYPE string,
           END OF ty_us_rtts,
           ty_ut_rtts TYPE STANDARD TABLE OF ty_us_rtts WITH DEFAULT KEY.

    CLASS-METHODS _deserialize
      IMPORTING
        test_data_xml TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_fm_test_data
      RAISING
        zcx_fm_test_data.

    METHODS _load
      RAISING
        zcx_fm_test_data.

    METHODS get_free_test_data_id
      RETURNING
        VALUE(test_data_id) TYPE eufunc-nummer.

    METHODS _delete
      RAISING
        zcx_fm_test_data .

    DATA:
      dep TYPE REF TO lif_dep.

ENDCLASS.



CLASS zcl_fm_test_data IMPLEMENTATION.


  METHOD cleanup_after_execution.

    CLEAR result_parameters.
    stepid = 'PBO'.

  ENDMETHOD.


  METHOD create.

    test_data = NEW zcl_fm_test_data( ).
    test_data->fm_name = fm_name.
    test_data->title = title.
    test_data->fugr_name = test_data->dep->get_fugr_name( fm_name ).
    test_data->cleanup_after_execution( ).

  ENDMETHOD.


  METHOD delete.

    DATA(test_data) = zcl_fm_test_data=>load( fm_name = fm_name id = id ).
    test_data->_delete( ).

  ENDMETHOD.


  METHOD _delete.

    DATA(not_found) = 0.

    DATA(context) = dep->load_test_context( fm_name ).

    DELETE context-te_datadir WHERE dataid = id.
    IF sy-subrc <> 0.
      not_found = not_found + 1.
    ENDIF.

    dep->save_test_context( fm_name = fm_name context = context ).

    DELETE FROM eufunc
        WHERE relid   = 'FL'
          AND gruppe  = fugr_name
          AND name    = fm_name
          AND nummer  = id.
    IF sy-subrc <> 0.
      not_found = not_found + 1.
    ENDIF.

    IF not_found = 2.
      RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Test data "{ id }" not found for "{ fm_name }"|.
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    DATA(call_arguments) = get_call_arguments( ).


    GET RUN TIME FIELD DATA(start).

    rc = dep->dynamic_function_call( call_arguments ).

    GET RUN TIME FIELD DATA(end).


    duration = dep->get_duration( runtime_start = start runtime_end = end ).
    stepid = 'PAI'.
    result_parameters = VALUE #(
        FOR <argument> IN call_arguments-parameters
        WHERE ( kind <> abap_func_exporting )
        ( <argument> ) ).

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


  METHOD get_call_arguments.

    DATA: ref_parameter     TYPE REF TO data.
    FIELD-SYMBOLS:
      <param_binding_pbo> TYPE abap_func_parmbind.

    "=================
    " PARAMETERS
    "=================

    DATA(params_rtts) = dep->get_fm_params_rtts( funcname = fm_name ).
    LOOP AT params_rtts REFERENCE INTO DATA(param_rtts).
      CREATE DATA ref_parameter TYPE HANDLE param_rtts->type.
      ASSIGN input_parameters[ name = param_rtts->name ] TO <param_binding_pbo>.
      IF sy-subrc = 0 AND <param_binding_pbo>-value IS BOUND.
        " All those values to be passed to the function module
        " with Call Function '...' exporting ... tables ... changing ...
        ASSIGN <param_binding_pbo>-value->* TO FIELD-SYMBOL(<input_parameter>).
        ASSIGN ref_parameter->* TO FIELD-SYMBOL(<parameter_value>).
        <parameter_value> = <input_parameter>.
      ENDIF.
      INSERT VALUE #(
            name  = param_rtts->name
            kind  = param_rtts->call_function_kind
            value = ref_parameter
          ) INTO TABLE call_arguments-parameters.
    ENDLOOP.

    "=================
    " EXCEPTIONS
    "=================

    DATA(fm_exceptions) = dep->get_fm_exceptions( ).

    DATA(exceptions) = VALUE abap_func_excpbind_tab(
        ( LINES OF VALUE #( FOR <fm_exception> IN fm_exceptions INDEX INTO row_number
        ( name = <fm_exception> value = row_number ) ) )
        ( name = 'ERROR_MESSAGE' value = lines( fm_exceptions ) + 1 )
        ( name = 'OTHERS' value = lines( fm_exceptions ) + 2 ) ).

  ENDMETHOD.


  METHOD get_free_test_data_id.

    TYPES ty_p TYPE p LENGTH 10 DECIMALS 0.

    DATA(fugr_name) = dep->get_fugr_name( fm_name ).
    DATA(test_data_ids) = dep->select_existing_test_data_ids(
          im_name     = fm_name
          i_fugr_name = fugr_name ).

    SORT test_data_ids BY table_line.

    DATA(test_data_id_numeric) = 1.
    test_data_id = CONV ty_p( 0 ). " in case TEST_DATA_IDS is empty
    LOOP AT test_data_ids INTO test_data_id.
      IF test_data_id <> CONV eu_name( test_data_id_numeric ).
        test_data_id = CONV eu_name( test_data_id_numeric ) - 1.
        EXIT.
      ENDIF.
      ADD 1 TO test_data_id_numeric.
    ENDLOOP.
    test_data_id = CONV ty_p( test_data_id ) + 1.

  ENDMETHOD.


  METHOD load.

    DATA: test_data_xml TYPE string.

    IF isolated_run = abap_true.

      CALL FUNCTION 'Z_FM_TEST_DATA_LOAD'
        DESTINATION 'NONE'
        EXPORTING
          fm_name               = fm_name
          id                    = id
        IMPORTING
          test_data_xml         = test_data_xml
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Error while loading the test data "{ condense( id ) }" of { fm_name } (RC { sy-subrc })|.
      ENDIF.

      test_data = _deserialize( test_data_xml ).

    ELSE.

      test_data = NEW zcl_fm_test_data( ).
      test_data->fm_name = fm_name.
      test_data->fugr_name = test_data->dep->get_fugr_name( fm_name ).
      test_data->id = id.
      test_data->_load( ).

    ENDIF.

  ENDMETHOD.


  METHOD save.

    " Create a new Test Data in all cases! (edit is not allowed)
    id = get_free_test_data_id( ).

    DATA(tab_cpar) = VALUE tab_cpar(
        " 1) Values before calling the function modules
        "    call function '...' exporting ... importing ... tables ... changing ...
        "                            X                          X           X
        ( LINES OF VALUE #(
            FOR <param_binding2> IN input_parameters "param_bindings_pbo
            WHERE ( kind <> abap_func_importing )
            ( name = '%_I' && <param_binding2>-name dref = <param_binding2>-value ) ) )
        " 2) Values after calling the function modules
        "    call function '...' exporting ... importing ... tables ... changing ...
        "                                          X            X           X
        ( LINES OF VALUE #(
            FOR <new_param_binding> IN result_parameters "param_bindings_pai
            ( LINES OF SWITCH #( <new_param_binding>-kind
                WHEN abap_func_changing THEN VALUE #(
                    ( name = '%_O' && <new_param_binding>-name dref = <new_param_binding>-value )
                    ( name = '%_V' && <new_param_binding>-name dref = <new_param_binding>-value ) )
                WHEN abap_func_importing THEN VALUE #(
                    ( name = '%_V' && <new_param_binding>-name dref = <new_param_binding>-value ) )
                WHEN abap_func_tables THEN VALUE #(
                    ( name = '%_V' && <new_param_binding>-name dref = <new_param_binding>-value ) ) ) ) ) )
        ( name = 'TIME1'      dref = NEW ty_test_attr-duration( duration ) )
        ( name = 'V_RC'       dref = NEW ty_test_attr-rc( rc ) )
        ( name = 'VEXCEPTION' dref = NEW ty_test_attr-exception_name( exception_name ) )
        ( name = 'G_UPPER'    dref = NEW ty_test_attr-lower_case( xsdbool( lower_case = abap_false ) ) ) ).


    DATA(context) = dep->load_test_context( fm_name ).

    DELETE context-te_datadir WHERE dataid = id.
    context-te_datadir = VALUE #(
        BASE context-te_datadir
        ( VALUE #(
          dataid = id
          stepid = stepid
          seqid  = ''
          datum  = sy-datum
          uzeit  = sy-uzeit
          title  = title ) ) ).

    dep->eufunc_export( i_tab_cpar = tab_cpar ).
    dep->save_test_context( fm_name = fm_name context = context ).

  ENDMETHOD.


  METHOD set_input_parameters.

    me->input_parameters = parameters.

  ENDMETHOD.


  METHOD set_lower_case.

    me->lower_case = xsdbool( lower_case <> abap_false ).

  ENDMETHOD.


  METHOD set_title.

    me->title = title.

  ENDMETHOD.


  METHOD _load.

    DATA: cpar          TYPE REF TO cpar,
          datadir_entry TYPE ty_datadir_entry.
    FIELD-SYMBOLS:
      <cpar>       TYPE cpar,
      <te_datadir> TYPE STANDARD TABLE.


    fugr_name = dep->get_fugr_name( fm_name ).


    dep->eufunc_import(
          EXPORTING
            test_data_id = id
          IMPORTING
            e_eufunc   = DATA(eufunc)
            e_tab_cpar = DATA(tab_cpar) ).


    author  = eufunc-autor.
    date    = eufunc-datum.
    time    = eufunc-zeit.
    version = eufunc-version.

    ASSIGN tab_cpar[ name = 'TIME1' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<time1>).
    ASSIGN tab_cpar[ name = 'V_RC' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<v_rc>).
    ASSIGN tab_cpar[ name = 'VEXCEPTION' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<vexception>).
    ASSIGN tab_cpar[ name = 'G_UPPER' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO FIELD-SYMBOL(<g_upper>).

    duration       = <time1>.
    rc             = <v_rc>.
    exception_name = <vexception>.
    lower_case     = xsdbool( <g_upper> = abap_false ).

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
            INTO TABLE input_parameters. "param_bindings_pbo.
      ELSEIF cpar->name CP '%_V*'.
        INSERT VALUE abap_func_parmbind(
                kind  = COND #( WHEN line_exists( tab_cpar[ name = |%_O{ cpar->name+3 }| ] )
                                  THEN abap_func_changing
                                WHEN line_exists( tab_cpar[ name = |%_I{ cpar->name+3 }| ] )
                                  THEN abap_func_importing
                                ELSE abap_func_tables )
                name  = cpar->name+3
                value = cpar->dref )
            INTO TABLE result_parameters. "param_bindings_pai.
      ENDIF.
    ENDLOOP.


    dep->eufunc_import(
          EXPORTING
            test_data_id = '999'
          IMPORTING
            e_tab_cpar   = DATA(tab_cpar_dir) ).


    ASSIGN tab_cpar_dir[ name = 'TE_DATADIR' ] TO <cpar>.
    ASSIGN <cpar>-dref->* TO <te_datadir>.
    datadir_entry = <te_datadir>[ ('CMP00001') = id ].

    title = datadir_entry-title.
    stepid = datadir_entry-stepid.

  ENDMETHOD.


  METHOD _deserialize.

    DATA: test_data_structure TYPE zcl_fm_test_data_serialize=>ty_test_data,
          error               TYPE REF TO cx_root.


    TRY.
        CALL TRANSFORMATION z_fm_test_data
            SOURCE XML test_data_xml
            RESULT root = test_data_structure.
      CATCH cx_root INTO error.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING previous = error.
    ENDTRY.

    IF test_data_structure-error IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = test_data_structure-error.
    ENDIF.

    DATA(test_data) = NEW zcl_fm_test_data( ).
    test_data->fm_name = test_data_structure-header-fm_name.
    test_data->fugr_name = test_data_structure-header-fugr_name.
    test_data->id = test_data_structure-header-id.
    test_data->title = test_data_structure-header-title.
    test_data->author = test_data_structure-header-author.
    test_data->date = test_data_structure-header-date.
    test_data->time = test_data_structure-header-time.
    test_data->lower_case = test_data_structure-header-lower_case.
    test_data->duration = test_data_structure-header-duration.
    test_data->rc = test_data_structure-header-rc.
    test_data->exception_name = test_data_structure-header-exception_name.
    test_data->stepid = test_data_structure-header-stepid.
    test_data->version = test_data_structure-header-version.

    test_data->set_input_parameters(
        parameters = VALUE #(
                        FOR <parameter> IN test_data_structure-values_before_call
                        ( zcl_fm_test_data_serialize=>deserialize_parameter(
                            parameter       = <parameter>
                            parameter_types = test_data_structure-parameter_types-input ) ) ) ).

    result = test_data.

  ENDMETHOD.


  METHOD constructor.

    dep = lcl_dep_factory=>create( me ).

  ENDMETHOD.


ENDCLASS.
