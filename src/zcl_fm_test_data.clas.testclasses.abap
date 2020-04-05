*"* use this source file for your ABAP unit test classes
CLASS save_determine_id DEFINITION DEFERRED.
CLASS save DEFINITION DEFERRED.
CLASS zcl_fm_test_data DEFINITION LOCAL FRIENDS save_determine_id save.

CLASS lcl_compare DEFINITION FOR TESTING FINAL.

  PUBLIC SECTION.

    INTERFACES if_constraint.

    CLASS-METHODS create
      IMPORTING
        expected_value TYPE any
      RETURNING
        VALUE(result)  TYPE REF TO lcl_compare.

    DATA: ref_expected_value TYPE REF TO data READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS compare
      IMPORTING
        act  TYPE any
        exp  TYPE any
        path TYPE string.

    METHODS compare_type
      IMPORTING
        act           TYPE any
        exp           TYPE any
        path          TYPE string
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_typedescr.

    METHODS compare_elem
      IMPORTING
        act  TYPE any
        exp  TYPE any
        path TYPE string.

    METHODS compare_ref
      IMPORTING
        act  TYPE REF TO data
        exp  TYPE REF TO data
        path TYPE string.

    METHODS compare_ref_to_object
      IMPORTING
        act  TYPE REF TO object
        exp  TYPE REF TO object
        path TYPE string.

    "! Compare components by position
    "! @parameter act | .
    "! @parameter exp | .
    METHODS compare_struct
      IMPORTING
        act  TYPE any
        exp  TYPE any
        path TYPE string.

    METHODS compare_table
      IMPORTING
        act  TYPE INDEX TABLE
        exp  TYPE INDEX TABLE
        path TYPE string.

    DATA: messages TYPE string_table.
ENDCLASS.



CLASS lcl_compare IMPLEMENTATION.


  METHOD create.

    CREATE OBJECT result.
    CREATE DATA result->ref_expected_value LIKE expected_value.
    ASSIGN result->ref_expected_value->* TO FIELD-SYMBOL(<expected_value>).
    <expected_value> = expected_value.

  ENDMETHOD.


  METHOD if_constraint~get_description.

    result = messages.

  ENDMETHOD.


  METHOD if_constraint~is_valid.

    ASSIGN me->ref_expected_value->* TO FIELD-SYMBOL(<expected_value>).
    compare( act = data_object exp = <expected_value> path = || ).
    IF messages IS INITIAL.
      result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD compare.

    DATA(rtti) = compare_type( act = act exp = exp path = path ).

    IF rtti IS BOUND.
      CASE rtti->kind.
        WHEN cl_abap_typedescr=>kind_elem.
          compare_elem( act = act exp = exp path = path ).
        WHEN cl_abap_typedescr=>kind_ref.
          CASE rtti->type_kind.
            WHEN cl_abap_typedescr=>typekind_dref.
              compare_ref( act = act exp = exp path = path ).
            WHEN OTHERS.
              compare_ref_to_object( act = act exp = exp path = path ).
          ENDCASE.
        WHEN cl_abap_typedescr=>kind_struct.
          compare_struct( act = act exp = exp path = path ).
        WHEN cl_abap_typedescr=>kind_table.
          compare_table( act = act exp = exp path = path ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD compare_type.

    DATA(act_rtti) = cl_abap_typedescr=>describe_by_data( act ).
    DATA(exp_rtti) = cl_abap_typedescr=>describe_by_data( exp ).

    IF act_rtti->type_kind <> exp_rtti->type_kind.
      APPEND |{ path }: Actual type is { act_rtti->type_kind } / Expected type is { exp_rtti->type_kind }| TO messages.
    ELSE.
      result = act_rtti.
    ENDIF.

  ENDMETHOD.


  METHOD compare_elem.

    IF act <> exp.
      APPEND |{ path }: Actual value is "{ act }" / Expected value is "{ exp }"| TO messages.
    ENDIF.

  ENDMETHOD.


  METHOD compare_ref.

    IF boolc( act IS BOUND ) <> boolc( exp IS BOUND ).
      APPEND |{ path }: Actual dref is { COND #( WHEN act IS BOUND THEN |BOUND| ELSE |NOT bound| ) } / Expected dref is { COND #( WHEN exp IS BOUND THEN |BOUND| ELSE |NOT bound| ) }| TO messages.
    ENDIF.
    IF act IS BOUND AND exp IS BOUND.
      ASSIGN act->* TO FIELD-SYMBOL(<act_value>).
      ASSIGN exp->* TO FIELD-SYMBOL(<exp_value>).
      compare( act = <act_value> exp = <exp_value> path = path && '->*' ).
    ENDIF.

  ENDMETHOD.


  METHOD compare_ref_to_object.

    IF boolc( act IS BOUND ) <> boolc( exp IS BOUND ).
      APPEND |{ path }: Actual oref is { COND #( WHEN act IS BOUND THEN |BOUND| ELSE |NOT bound| ) } / Expected oref is { COND #( WHEN exp IS BOUND THEN |BOUND| ELSE |NOT bound| ) }| TO messages.
    ENDIF.
    IF act IS BOUND AND exp IS BOUND.
      " TODO : how to compare ?
      DATA(act_rtti) = cl_abap_typedescr=>describe_by_object_ref( act ).
      DATA(exp_rtti) = cl_abap_typedescr=>describe_by_object_ref( exp ).
    ENDIF.

  ENDMETHOD.


  METHOD compare_struct.

    DO.
      DATA(comp_number) = sy-index.
      ASSIGN COMPONENT comp_number OF STRUCTURE act TO FIELD-SYMBOL(<act_field>).
      ASSIGN COMPONENT comp_number OF STRUCTURE exp TO FIELD-SYMBOL(<exp_field>).
      CASE |{ boolc( <act_field> IS ASSIGNED ) }{ boolc( <exp_field> IS ASSIGNED ) }|.
        WHEN `  `.
          EXIT.
        WHEN `X `.
          APPEND |{ path }: Actual structure has more components than Expected structure| TO messages.
          EXIT.
        WHEN ` X`.
          APPEND |{ path }: Actual structure has less components than Expected structure| TO messages.
          EXIT.
        WHEN `XX`.
          compare( act = <act_field> exp = <exp_field> path = path && |\{{ comp_number }\}| ).
      ENDCASE.
      UNASSIGN <act_field>.
      UNASSIGN <exp_field>.
    ENDDO.

  ENDMETHOD.


  METHOD compare_table.

    " Lines which are both in actual and expected results
    LOOP AT act TO lines( exp ) ASSIGNING FIELD-SYMBOL(<act_line>).
      DATA(line_number) = sy-tabix.
      READ TABLE exp INDEX line_number ASSIGNING FIELD-SYMBOL(<exp_line>).
      ASSERT sy-subrc = 0.
      compare( act = <act_line> exp = <exp_line> path = path && |/item[{ line_number }]| ).
    ENDLOOP.

    " Lines which are in actual result but are not expected
    LOOP AT act FROM lines( exp ) + 1 ASSIGNING <act_line>.
      APPEND |{ path }: Actual table has more lines than Expected table| TO messages.
      EXIT.
    ENDLOOP.

    " Lines which are expected but are not in actual result
    LOOP AT exp FROM lines( act ) + 1 ASSIGNING <exp_line>.
      APPEND |{ path }: Actual table has more lines than Expected table| TO messages.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lth_dep DEFINITION FOR TESTING.

  PUBLIC SECTION.

    INTERFACES lif_dep.

    TYPES: BEGIN OF ty_tb_line,
             comp1 TYPE string,
             comp2 TYPE string,
           END OF ty_tb_line,
           ty_tb TYPE STANDARD TABLE OF ty_tb_line WITH DEFAULT KEY,
           BEGIN OF ty_fm_call_result,
             parameters TYPE abap_func_parmbind_tab,
             duration   TYPE i,
             exception  TYPE i,
           END OF ty_fm_call_result.

    METHODS inject
      IMPORTING
        fm_name        TYPE tfdir-funcname
*        fms            TYPE ty_fms
        tab_cpar       TYPE tab_cpar OPTIONAL
        fm_call_result TYPE ty_fm_call_result OPTIONAL
        test_data_ids  TYPE lif_dep=>ty_test_data_ids OPTIONAL.

    CLASS-METHODS class_constructor.

    DATA: fm_name        TYPE tfdir-funcname READ-ONLY,
          tab_cpar       TYPE tab_cpar READ-ONLY,
          fm_call_result TYPE ty_fm_call_result READ-ONLY,
          test_data_ids  TYPE lif_dep=>ty_test_data_ids READ-ONLY.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_fm,
             fm_name       TYPE tfdir-funcname,
             fugr_name     TYPE tlibg-area,
             params_rtts   TYPE zcl_fm_params_rtts=>ty_params_rtts,
             fm_exceptions TYPE lif_dep=>ty_fm_exceptions,
*             test_data_ids  TYPE lif_dep=>ty_test_data_ids,
*             tab_cpar       TYPE tab_cpar,
*             fm_call_result TYPE ty_fm_call_result,
           END OF ty_fm,
           ty_fms TYPE HASHED TABLE OF ty_fm WITH UNIQUE KEY fm_name.

    CLASS-DATA: fms TYPE ty_fms.

ENDCLASS.


CLASS save_determine_id DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS first_time FOR TESTING RAISING cx_static_check.
    METHODS no_gap FOR TESTING RAISING cx_static_check.
    METHODS gap_1_not_free FOR TESTING RAISING cx_static_check.
    METHODS gap_1_free FOR TESTING RAISING cx_static_check.

    METHODS create_test_data
      IMPORTING
        test_data_ids TYPE lif_dep=>ty_test_data_ids
      RETURNING
        VALUE(result) TYPE REF TO zcl_fm_test_data
      RAISING
        zcx_fm_test_data.

ENDCLASS.


CLASS save DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES ty_tb TYPE lth_dep=>ty_tb.

    METHODS parameters_before_call FOR TESTING RAISING cx_static_check.
    METHODS parameters_after_call FOR TESTING RAISING cx_static_check.
    METHODS lower_case_off FOR TESTING RAISING cx_static_check.
    METHODS lower_case_on FOR TESTING RAISING cx_static_check.
    METHODS exception FOR TESTING RAISING cx_static_check.

    METHODS create_test_data
      IMPORTING
        in             TYPE i OPTIONAL
        tb             TYPE ty_tb OPTIONAL
        ch             TYPE string OPTIONAL
        fm_call_result TYPE lth_dep=>ty_fm_call_result OPTIONAL
      RETURNING
        VALUE(result)  TYPE REF TO zcl_fm_test_data
      RAISING
        zcx_fm_test_data.

    "! Clone data object with CREATE DATA to avoid dumps to Freed References
    "! @parameter dobj | Data object
    "! @parameter result | Data reference of cloned data object
    METHODS clone
      IMPORTING
        dobj          TYPE any
      RETURNING
        VALUE(result) TYPE REF TO data.

ENDCLASS.


CLASS lth_dep IMPLEMENTATION.

  METHOD class_constructor.
    DATA: dummy_tb TYPE ty_tb.

    fms = VALUE #(
    ( fm_name = 'FM1'
      fugr_name = 'A'
      params_rtts = VALUE #(
        ( name = 'IN' call_function_kind = abap_func_exporting function_kind = abap_func_importing type = cl_abap_elemdescr=>get_i( ) )
        ( name = 'TB' call_function_kind = abap_func_tables    function_kind = abap_func_tables    type = CAST #( cl_abap_typedescr=>describe_by_data( dummy_tb ) ) )
        ( name = 'CH' call_function_kind = abap_func_changing  function_kind = abap_func_changing  type = cl_abap_elemdescr=>get_string( ) ) ) )
    ( fm_name = 'FM2'
      fugr_name = 'A'
      params_rtts = VALUE #( )
      fm_exceptions = VALUE #( ( 'ERR1' ) ( 'ERR2' ) ) ) ).

  ENDMETHOD.

  METHOD inject.

    me->fm_name = fm_name.
    me->tab_cpar = tab_cpar.
    me->fm_call_result = fm_call_result.
    me->test_data_ids = test_data_ids.

  ENDMETHOD.

  METHOD lif_dep~get_fugr_name.

    fugr_name = fms[ fm_name = fm_name ]-fugr_name.

  ENDMETHOD.

  METHOD lif_dep~select_existing_test_data_ids.

    r_test_data_ids = test_data_ids.

  ENDMETHOD.

  METHOD lif_dep~load_test_context.

  ENDMETHOD.

  METHOD lif_dep~save_test_context.

  ENDMETHOD.

  METHOD lif_dep~get_fm_params_rtts.

    params_rtts = fms[ fm_name = funcname ]-params_rtts.

  ENDMETHOD.

  METHOD lif_dep~eufunc_export.

    me->tab_cpar = i_tab_cpar.

  ENDMETHOD.

  METHOD lif_dep~eufunc_import.

    e_tab_cpar = tab_cpar.

  ENDMETHOD.

  METHOD lif_dep~dynamic_function_call.

    LOOP AT i_call_arguments-parameters REFERENCE INTO DATA(parameter)
        WHERE kind <> abap_func_exporting.

      ASSIGN parameter->value->* TO FIELD-SYMBOL(<parameter_value>).
      CHECK sy-subrc = 0.
      ASSIGN fm_call_result-parameters[ name = parameter->name ] TO FIELD-SYMBOL(<fm_call_result_param>).
      CHECK sy-subrc = 0.
      ASSIGN <fm_call_result_param>-value->* TO FIELD-SYMBOL(<fm_call_result_param_value>).

      <parameter_value> = <fm_call_result_param_value>.
    ENDLOOP.

    result = me->fm_call_result-exception.

  ENDMETHOD.

  METHOD lif_dep~get_fm_exceptions.

    result = fms[ fm_name = fm_name ]-fm_exceptions.

  ENDMETHOD.

  METHOD lif_dep~get_duration.

    result = fm_call_result-duration.

  ENDMETHOD.

ENDCLASS.


CLASS save_determine_id IMPLEMENTATION.

  METHOD first_time.

    " given
    "   there are no test data yet for this Function Module
    DATA(test_data) = create_test_data( VALUE #( ) ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   id 1 is assigned
    cl_abap_unit_assert=>assert_equals( exp = '                            1' act = test_data->id ).

  ENDMETHOD.

  METHOD no_gap.

    " given
    "   existing test data for this Function Module has ID 1
    DATA(test_data) = create_test_data( VALUE #( ( '                            1' ) ) ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   id 2 is assigned
    cl_abap_unit_assert=>assert_equals( exp = '                            2' act = test_data->id ).
  ENDMETHOD.

  METHOD gap_1_not_free.

    " given
    "   existing IDs for this Function Module are 1 and 3
    DATA(test_data) = create_test_data( VALUE #( ( '                            1' ) ( '                            3' ) ) ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   id 2 is assigned
    cl_abap_unit_assert=>assert_equals( exp = '                            2' act = test_data->id ).

  ENDMETHOD.

  METHOD gap_1_free.
    " given
    "   existing ID for this Function Module has ID 2
    DATA(test_data) = create_test_data( VALUE #( ( '                            2' ) ) ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   id 1 is assigned
    cl_abap_unit_assert=>assert_equals( exp = '                            1' act = test_data->id ).

  ENDMETHOD.


  METHOD create_test_data.

    DATA(dep) = NEW lth_dep( ).
    dep->inject( fm_name = 'FM1' test_data_ids = test_data_ids ).
    lcl_dep_factory=>inject( dep ).
    result  = zcl_fm_test_data=>create( fm_name = 'FM1' title = '' ).

  ENDMETHOD.


ENDCLASS.


CLASS save IMPLEMENTATION.


  METHOD parameters_before_call.

    " given
    "   test data is created for function module 'x' with parameters
    "   TABLES tb = table with 2 lines [ { comp1 = 1 comp2 = 2 } , { comp1 = 3 comp2 = 4 } ]
    "   and CHANGING ch = ch (with value 'A')
    "   with lower case option switched off
    DATA(test_data) = create_test_data(
            in = 1
            tb = VALUE #( ( comp1 = 1 comp2 = 2 ) ( comp1 = 3 comp2 = 4 ) )
            ch = 'A' ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   EUFUNC FL should contain in data cluster format (EXPORT id1 = value1 [id2 = value2 ...] TO ...)
    "     ID          VALUE
    "     ------      ----------
    "     %_IIN       1
    "     %_ICH       A
    "     %_ITB       table with 2 lines [ { comp1 = 1 comp2 = 2 } , { comp1 = 3 comp2 = 4 } ]
    "     TIME1       0
    "     V_RC        0
    "     VEXCEPTION
    "     G_UPPER     X
    cl_abap_unit_assert=>assert_that(
        act = CAST lth_dep( test_data->dep )->tab_cpar
        exp = lcl_compare=>create( VALUE tab_cpar(
            ( name = '%_IIN'      dref = REF #( 1 ) )
            ( name = '%_ITB'      dref = NEW ty_tb( ( comp1 = 1 comp2 = 2 ) ( comp1 = 3 comp2 = 4 ) ) )
            ( name = '%_ICH'      dref = REF #( `A` ) )
            ( name = 'TIME1'      dref = NEW zcl_fm_test_data=>ty_duration( 0 ) )
            ( name = 'V_RC'       dref = REF #( 0 ) )
            ( name = 'VEXCEPTION' dref = REF #( '' ) )
            ( name = 'G_UPPER'    dref = REF #( 'X' ) ) ) ) ).

  ENDMETHOD.


  METHOD parameters_after_call.

    " given
    "   test data is created for function module 'x' with parameters
    "   TABLES tb = table with 2 lines [ { comp1 = 1 comp2 = 2 } , { comp1 = 3 comp2 = 4 } ]
    "   and CHANGING ch = ch (with value 'A')
    "   with lower case option switched off
    " and it's executed
    "   whose duration is exactly 2 seconds and 2139 µs, and no exception happened,
    "   and returns data     out = 5
    "                        tb  = table with 1 line [ { comp1 = 1 comp2 = 2 } ]
    "                        ch  = 'B'
    DATA(test_data) = create_test_data(
        in = 1
        tb = VALUE #( ( comp1 = 1 comp2 = 2 ) ( comp1 = 3 comp2 = 4 ) )
        ch = 'A'
        fm_call_result = VALUE #( duration = 2002139 exception = '' parameters = VALUE #(
            ( name = 'OUT' value = NEW i( 5 ) )
            ( name = 'TB'  value = NEW ty_tb( ( comp1 = 1 comp2 = 2 ) ) )
            ( name = 'CH'  value = NEW string( 'B' ) ) ) ) ).
    test_data->execute( ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   EUFUNC FL should contain in data cluster format (EXPORT id1 = value1 [id2 = value2 ...] TO ...)
    "     ID      VALUE
    "     ------  ----------
    "     %_IIN   1
    "     %_VOUT  5
    "     %_ICH   A
    "     %_OCH   B
    "     %_VCH   B
    "     %_ITB   table with 2 lines [ { comp1 = 1 comp2 = 2 } , { comp1 = 3 comp2 = 4 } ]
    "     %_VTB   table with 1 line [ { comp1 = 1 comp2 = 2 } ]
    "     TIME1       2002139
    "     V_RC        0
    "     VEXCEPTION
    "     G_UPPER     X
    cl_abap_unit_assert=>assert_that(
        act = CAST lth_dep( test_data->dep )->tab_cpar
        exp = lcl_compare=>create( VALUE tab_cpar(
            ( name = '%_IIN'      dref = NEW i( 1 ) )
            ( name = '%_ITB'      dref = NEW ty_tb( ( comp1 = 1 comp2 = 2 ) ( comp1 = 3 comp2 = 4 ) ) )
            ( name = '%_ICH'      dref = REF #( `A` ) )
            ( name = '%_VTB'      dref = NEW ty_tb( ( comp1 = 1 comp2 = 2 ) ) )
            ( name = '%_OCH'      dref = REF #( `B` ) )
            ( name = '%_VCH'      dref = REF #( `B` ) )
            ( name = 'TIME1'      dref = NEW zcl_fm_test_data=>ty_duration( 2002139 ) )
            ( name = 'V_RC'       dref = REF #( 0 ) )
            ( name = 'VEXCEPTION' dref = REF #( '' ) )
            ( name = 'G_UPPER'    dref = REF #( 'X' ) ) ) ) ).

  ENDMETHOD.


  METHOD lower_case_off.

    " given
    "   test data is set for call function 'x' changing ch = ch, with ch = 'a' and lower case option switched off,
    DATA(test_data) = create_test_data( ch = 'a' ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   EUFUNC FL should contain in data cluster format (EXPORT id1 = value1 [id2 = value2 ...] TO ...)
    "     ID      VALUE
    "     ------  ----------
    "     %_IIN   A
    "     TIME1       0
    "     V_RC        0
    "     VEXCEPTION
    "     G_UPPER     X
    cl_abap_unit_assert=>assert_that(
        act = CAST lth_dep( test_data->dep )->tab_cpar
        exp = lcl_compare=>create( VALUE tab_cpar(
            ( name = '%_IIN'      dref = NEW i( ) )
            ( name = '%_ITB'      dref = NEW ty_tb( ) )
            ( name = '%_ICH'      dref = REF #( `a` ) )
            ( name = 'TIME1'      dref = NEW zcl_fm_test_data=>ty_duration( 0 ) )
            ( name = 'V_RC'       dref = REF #( 0 ) )
            ( name = 'VEXCEPTION' dref = REF #( '' ) )
            ( name = 'G_UPPER'    dref = REF #( 'X' ) ) ) ) ).

  ENDMETHOD.


  METHOD lower_case_on.

    " given
    "   test data is set for call function 'x' changing ch = ch, with ch = 'a' and lower case option switched on,
    DATA(test_data) = create_test_data( ch = 'a' ).
    test_data->set_lower_case( abap_true ).

    " when
    "   test data is saved
    test_data->save( ).

    " then
    "   EUFUNC FL should contain in data cluster format (EXPORT id1 = value1 [id2 = value2 ...] TO ...)
    "     ID      VALUE
    "     ------  ----------
    "     %_IIN   a
    "     TIME1       0
    "     V_RC        0
    "     VEXCEPTION
    "     G_UPPER
    cl_abap_unit_assert=>assert_that(
        act = CAST lth_dep( test_data->dep )->tab_cpar
        exp = lcl_compare=>create( VALUE tab_cpar(
            ( name = '%_IIN'      dref = NEW i( ) )
            ( name = '%_ITB'      dref = NEW ty_tb( ) )
            ( name = '%_ICH'      dref = REF #( `a` ) )
            ( name = 'TIME1'      dref = NEW zcl_fm_test_data=>ty_duration( 0 ) )
            ( name = 'V_RC'       dref = REF #( 0 ) )
            ( name = 'VEXCEPTION' dref = REF #( '' ) )
            ( name = 'G_UPPER'    dref = REF #( '' ) ) ) ) ).

  ENDMETHOD.


  METHOD exception.

    DATA(dep) = NEW lth_dep( ).
    dep->inject( fm_name = 'FM2' fm_call_result = VALUE #( duration = 1500 exception = 2 ) ).
    lcl_dep_factory=>inject( dep ).
    DATA(test_data) = zcl_fm_test_data=>create( fm_name = 'FM2' title = '' ).
    test_data->execute( ).
    test_data->save( ).

    cl_abap_unit_assert=>assert_that(
        act = CAST lth_dep( test_data->dep )->tab_cpar
        exp = lcl_compare=>create( VALUE tab_cpar(
            ( name = 'TIME1'      dref = NEW zcl_fm_test_data=>ty_duration( 1500 ) )
            ( name = 'V_RC'       dref = REF #( 2 ) )
            ( name = 'VEXCEPTION' dref = REF #( '' ) )
            ( name = 'G_UPPER'    dref = REF #( 'X' ) ) ) ) ).

  ENDMETHOD.


  METHOD create_test_data.

    DATA(dep) = NEW lth_dep( ).
    dep->inject( fm_name = 'FM1' fm_call_result = fm_call_result ).
    lcl_dep_factory=>inject( dep ).
    result = zcl_fm_test_data=>create( fm_name = 'FM1' title = '' ).

    result->set_input_parameters( parameters = VALUE #(
                ( name = 'IN' kind = abap_func_exporting value = clone( in ) )
                ( name = 'TB' kind = abap_func_tables    value = clone( tb ) )
                ( name = 'CH' kind = abap_func_changing  value = clone( ch ) ) ) ).

  ENDMETHOD.


  METHOD clone.

    CREATE DATA result LIKE dobj.
    ASSIGN result->* TO FIELD-SYMBOL(<any>).
    <any> = dobj.

  ENDMETHOD.


ENDCLASS.
