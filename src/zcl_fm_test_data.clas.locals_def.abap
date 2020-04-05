*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

interface lif_dep.

    TYPES: ty_test_data_ids TYPE STANDARD TABLE OF eufunc-nummer WITH EMPTY KEY,
           BEGIN OF ty_context,
             te_datadir            TYPE zcl_fm_test_data=>ty_datadir,
             fdesc_copy            TYPE zcl_fm_test_data=>ty_fdesc_copy,
             struc_info_table_copy TYPE nf2ty_struc_info_table,
             g_no_save             TYPE c LENGTH 1,
             d102_fname            TYPE rs38l_fnam,
           END OF ty_context.
    TYPES: ty_fm_exceptions TYPE STANDARD TABLE OF fupararef-parameter with EMPTY KEY.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter fugr_name | <p class="shorttext synchronized" lang="en">Function group or empty if the function module doesn't exist</p>
    METHODS get_fugr_name
      IMPORTING
        fm_name          TYPE tfdir-funcname
      RETURNING
        VALUE(fugr_name) TYPE eufunc-gruppe.

    METHODS select_existing_test_data_ids
      IMPORTING
        im_name                TYPE tfdir-funcname
        i_fugr_name            TYPE eufunc-gruppe
      RETURNING
        VALUE(r_test_data_ids) TYPE ty_test_data_ids.

    METHODS load_test_context
      IMPORTING
        fm_name        TYPE tfdir-funcname
      RETURNING
        VALUE(context) TYPE ty_context
      RAISING
        zcx_fm_test_data.

    METHODS save_test_context
      IMPORTING
        fm_name TYPE tfdir-funcname
        context TYPE ty_context.

    METHODS get_fm_params_rtts
      IMPORTING
        funcname           TYPE tfdir-funcname
      RETURNING
        VALUE(params_rtts) TYPE zcl_fm_params_rtts=>ty_params_rtts
      RAISING
        zcx_fm_test_data.

    METHODS eufunc_import
      IMPORTING
        test_data_id TYPE eufunc-nummer
      EXPORTING
        e_eufunc   TYPE eufunc
        e_tab_cpar TYPE tab_cpar
      RAISING
        zcx_fm_test_data.

    METHODS eufunc_export
      IMPORTING
        i_tab_cpar TYPE tab_cpar
      RAISING
        zcx_fm_test_data.

    METHODS dynamic_function_call
      IMPORTING
        i_call_arguments TYPE zcl_fm_test_data=>ty_call_arguments
      returning
        value(result) type i.

    METHODS get_fm_exceptions
      RETURNING
        VALUE(result) TYPE ty_fm_exceptions.

    METHODS get_duration
      IMPORTING
        RUNTIME_start type i
        RUNTIME_end type i
      RETURNING
        VALUE(result) TYPE i.

    data main_object TYPE REF TO zcl_fm_test_data.

ENDINTERFACE.


class lcl_dep_factory definition.

  PUBLIC SECTION.

    class-methods create
      IMPORTING
        main_object type ref to zcl_fm_test_data
      RETURNING
        VALUE(result) type ref to lif_dep.

    class-methods inject
      IMPORTING
        dep type ref to lif_dep.

   CLASS-DATA injected_dep type ref to lif_dep.

endclass.


class lcl_dep definition.

  PUBLIC SECTION.

    interfaces lif_dep.

    methods constructor
      IMPORTING
        main_object TYPE REF TO zcl_fm_test_data.

    aliases main_object for lif_dep~main_object.
    aliases ty_context for lif_dep~ty_context.

  PRIVATE SECTION.

    METHODS dereference_and_copy
      IMPORTING
        dref   TYPE ref to data
      EXPORTING
        target TYPE any.

ENDCLASS.
