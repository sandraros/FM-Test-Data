CLASS zcl_fm_test_data_serialize DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS ZCL_fm_test_data.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_parameter,
             name                      TYPE string,
             kind                      LIKE abap_func_importing,
             dynamically_defined_value TYPE xsdany,
           END OF ty_parameter,
           BEGIN OF ty_parameter_type,
             name                      TYPE string,
             dynamically_defined_value TYPE xsdany, " srtti_type TYPE REF TO zcl_srtti_typedescr
           END OF ty_parameter_type,
           ty_parameter_types TYPE STANDARD TABLE OF ty_parameter_type WITH EMPTY KEY,
           BEGIN OF ty_test_data,
             BEGIN OF header,
               fm_name        TYPE tfdir-funcname,
               fugr_name      TYPE tlibg-area,
               id             TYPE eufunc-nummer,
               title          TYPE zcl_fm_test_data=>ty_datadir_entry-title,
               author         TYPE syuname,
               date           TYPE d,
               time           TYPE t,
               lower_case     TYPE abap_bool,
               duration       TYPE i,
               rc             TYPE i,
               exception_name TYPE c LENGTH 30,
               stepid         TYPE zcl_fm_test_data=>ty_datadir_entry-stepid,
               version        TYPE c LENGTH 4,
             END OF header,
             values_before_call TYPE STANDARD TABLE OF ty_parameter WITH EMPTY KEY,
             values_after_call  TYPE STANDARD TABLE OF ty_parameter WITH EMPTY KEY,
             BEGIN OF parameter_types,
               input  TYPE ty_parameter_types,
               result TYPE ty_parameter_types,
             END OF parameter_types,
             error              TYPE string,
           END OF ty_test_data.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter test_data | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter test_data_xml | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_fm_test_data | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS serialize
      IMPORTING
        test_data            TYPE REF TO zcl_fm_test_data
*        parameter_types      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(test_data_xml) TYPE string
      RAISING
        zcx_fm_test_data .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter test_data_xml | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_fm_test_data | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS deserialize
      IMPORTING
        test_data_xml TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_fm_test_data
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS deserialize_into_structure
      IMPORTING
        test_data_xml TYPE string
      RETURNING
        VALUE(result) TYPE ty_test_data
      RAISING
        zcx_fm_test_data.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_parameter
      IMPORTING
        parameter     TYPE abap_func_parmbind
      RETURNING
        VALUE(result) TYPE ty_parameter
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS get_parameter_type
      IMPORTING
        parameter     TYPE abap_func_parmbind
      RETURNING
        VALUE(result) TYPE ty_parameter_type
      RAISING
        zcx_fm_test_data.

    CLASS-METHODS serialize_parameter_types
      IMPORTING
        parameters TYPE abap_func_parmbind_tab
      RETURNING
        VALUE(xml) TYPE string.

    CLASS-METHODS deserialize_parameter
      IMPORTING
        parameter       TYPE ty_parameter
        parameter_types TYPE ty_parameter_types
      RETURNING
        VALUE(result)   TYPE abap_func_parmbind
      RAISING
        zcx_fm_test_data.

ENDCLASS.



CLASS zcl_fm_test_data_serialize IMPLEMENTATION.


  METHOD serialize.

    DATA(serializable_test_data) = VALUE ty_test_data(
        header             = VALUE #(
                                fm_name        = test_data->fm_name
                                fugr_name      = test_data->fugr_name
                                id             = test_data->id
                                title          = test_data->title
                                author         = test_data->author
                                date           = test_data->date
                                time           = test_data->time
                                lower_case     = test_data->lower_case
                                duration       = test_data->duration
                                rc             = test_data->rc
                                exception_name = test_data->exception_name
                                stepid         = test_data->stepid
                                version        = test_data->version )
        values_before_call = VALUE #(
                                FOR <parameter> IN test_data->input_parameters
                                ( get_parameter( <parameter> ) ) )
        values_after_call  = VALUE #(
                                FOR <parameter> IN test_data->result_parameters
                                ( get_parameter( <parameter> ) ) )
        parameter_types    = VALUE #(
                                input = VALUE #(
                                        FOR <type> IN test_data->input_parameters
                                        ( get_parameter_type( <type> ) ) )
                                result = VALUE #(
                                        FOR <type> IN test_data->result_parameters
                                        ( get_parameter_type( <type> ) ) ) ) ).

    TRY.
        CALL TRANSFORMATION z_fm_test_data
            SOURCE root = serializable_test_data
            RESULT XML test_data_xml
            OPTIONS xml_header = 'no'.
      CATCH cx_root INTO DATA(error).
        serializable_test_data-error = error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize.

    DATA: parameter TYPE REF TO ty_parameter.


    DATA(test_data_temp) = deserialize_into_structure( test_data_xml ).

    IF test_data_temp-error IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = test_data_temp-error.
    ENDIF.

    DATA(test_data) = zcl_fm_test_data=>create( fm_name = test_data_temp-header-fm_name
                                                title   = test_data_temp-header-title ).
*    test_data->fugr_name = test_data_temp-header-fugr_name.
*    test_data->id = test_data_temp-header-id.
*    test_data->author = test_data_temp-header-author.
*    test_data->date = test_data_temp-header-date.
*    test_data->time = test_data_temp-header-time.
    test_data->set_lower_case( test_data_temp-header-lower_case ).
*    test_data->duration = test_data_temp-header-duration.
*    test_data->rc = test_data_temp-header-rc.
*    test_data->exception_name = test_data_temp-header-exception_name.
*    test_data->stepid = test_data_temp-header-stepid.
*    test_data->version = test_data_temp-header-version.

    test_data->set_input_parameters(
        parameters = VALUE #(
                        FOR <parameter> IN test_data_temp-values_before_call
                        ( deserialize_parameter( parameter       = <parameter>
                                                 parameter_types = test_data_temp-parameter_types-input ) ) ) ).

    result = test_data.

  ENDMETHOD.


  METHOD deserialize_into_structure.

    DATA: error TYPE REF TO cx_root.

    TRY.
        CALL TRANSFORMATION z_fm_test_data
            SOURCE XML test_data_xml
            RESULT root = result.
      CATCH cx_root INTO error.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize_parameter.

    DATA: "test_data_temp TYPE ty_xx,
      srtti_type TYPE REF TO zcl_srtti_typedescr,
      error      TYPE REF TO cx_root,
      dref       TYPE REF TO data.


    DATA(parameter_type) = REF #( parameter_types[ name = parameter-name ] OPTIONAL ).
    IF parameter_type IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING text = |Internal error - Parameter type was not serialized|.
    ENDIF.

    TRY.
        CALL TRANSFORMATION id
            SOURCE XML parameter_type->dynamically_defined_value
            RESULT data = srtti_type.
      CATCH cx_root INTO error.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING previous = error.
    ENDTRY.

    DATA(rtti) = CAST cl_abap_datadescr( srtti_type->get_rtti( ) ).
    CREATE DATA dref TYPE HANDLE rtti.
    ASSIGN dref->* TO FIELD-SYMBOL(<parameter_value>).

    " store the value
    TRY.
        CALL TRANSFORMATION id
            SOURCE XML parameter-dynamically_defined_value
            RESULT data = <parameter_value>.
      CATCH cx_root INTO error.
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING previous = error.
    ENDTRY.

    result = VALUE #(
        name  = parameter-name
        kind  = parameter-kind
        value = dref ).

  ENDMETHOD.


  METHOD serialize_parameter_types.

    DATA: parameter  TYPE REF TO abap_func_parmbind,
          srtti_type TYPE REF TO zcl_srtti_typedescr.
    FIELD-SYMBOLS <parameter_value> TYPE any.

    LOOP AT parameters REFERENCE INTO parameter.
      ASSIGN parameter->value->* TO <parameter_value>.
      srtti_type = zcl_srtti_typedescr=>create_by_data_object( <parameter_value> ).
      TRY.
          " to omit the BOM, get RESULT into type XSTRING, but drawback is that it's UTF-8 to be later converted to string.
          CALL TRANSFORMATION id
            SOURCE data = srtti_type
            RESULT XML DATA(serialized_srtti_type)
            OPTIONS initial_components = 'suppress' "data_refs = 'heap-or-create'
                    xml_header    = 'no'.
        CATCH cx_root INTO DATA(error).
      ENDTRY.
      xml = xml && |<element name="{ parameter->name }">{ cl_abap_codepage=>convert_from( serialized_srtti_type ) }</element>|.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_parameter.

    FIELD-SYMBOLS <parameter_value> TYPE any.

    result-name = parameter-name.
    result-kind = parameter-kind.

    ASSIGN parameter-value->* TO <parameter_value>.
    TRY.
        " to omit the BOM, get RESULT into type XSTRING, but drawback is that it's UTF-8 to be later converted to string.
        CALL TRANSFORMATION id
          SOURCE data = <parameter_value>
          RESULT XML result-dynamically_defined_value
          OPTIONS initial_components = 'suppress' "data_refs = 'heap-or-create'
                  xml_header    = 'no'.
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_parameter_type.

    FIELD-SYMBOLS <parameter_value> TYPE any.

    result-name = parameter-name.

    ASSIGN parameter-value->* TO <parameter_value>.
    DATA(srtti_type) = zcl_srtti_typedescr=>create_by_data_object( <parameter_value> ).
    TRY.
        " to omit the BOM, get RESULT into type XSTRING, but drawback is that it's UTF-8 to be later converted to string.
        CALL TRANSFORMATION id
          SOURCE data = srtti_type
          RESULT XML result-dynamically_defined_value
          OPTIONS initial_components = 'suppress' "data_refs = 'heap-or-create'
                  xml_header    = 'no'.
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_fm_test_data EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
