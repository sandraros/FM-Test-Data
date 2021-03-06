"! <p class="shorttext synchronized" lang="en">Utility for Function Module Test Data</p>
CLASS zcl_fm_test_data DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_datadir_entry,
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
           END OF ty_datadir_entry,
           ty_datadir TYPE STANDARD TABLE OF ty_datadir_entry WITH EMPTY KEY,

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
           END OF ty_fdesc_entry,
           ty_fdesc TYPE STANDARD TABLE OF ty_fdesc_entry WITH EMPTY KEY,

           BEGIN OF ty_fdesc_copy_entry,
             line TYPE ty_fdesc_entry,
           END OF ty_fdesc_copy_entry,
           ty_fdesc_copy TYPE STANDARD TABLE OF ty_fdesc_copy_entry WITH EMPTY KEY,

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
           END OF ty_test_attr.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter title | <p class="shorttext synchronized" lang="en">Title of test data</p>
    "! @parameter param_bindings | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter lower_case | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter test_id | <p class="shorttext synchronized" lang="en">ID of the test data created</p>
    CLASS-METHODS create_without_execution
      IMPORTING
        fm_name        TYPE tfdir-funcname
        title          TYPE ty_datadir_entry-title
        param_bindings TYPE abap_func_parmbind_tab
        lower_case     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(test_id) TYPE eufunc-nummer
      RAISING
        zcx_fm_test_data.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter title | <p class="shorttext synchronized" lang="en">Title of test data</p>
    "! @parameter param_bindings | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter lower_case | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter test_id | <p class="shorttext synchronized" lang="en">ID of the test data created</p>
    CLASS-METHODS execute_and_create
      IMPORTING
        fm_name        TYPE tfdir-funcname
        title          TYPE ty_datadir_entry-title
        param_bindings TYPE abap_func_parmbind_tab
        lower_case     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(test_id) TYPE eufunc-nummer
      RAISING
        zcx_fm_test_data.

    "! NB: LOAD supports the fact that the function module does not exist anymore, or
    "! has been moved to another function group since then.
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter test_id | <p class="shorttext synchronized" lang="en">ID of the test data</p>
    "! @parameter datadir_entry | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter attributes | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter param_bindings_pbo | <p class="shorttext synchronized" lang="en"></p>
    "!      | Values of arguments passed to the function module (call)
    "! @parameter param_bindings_pai | <p class="shorttext synchronized" lang="en"></p>
    "!      | Values of parameters returned after the function module call
    CLASS-METHODS load
      IMPORTING
        VALUE(fugr_name)   TYPE tlibg-area OPTIONAL
        fm_name            TYPE tfdir-funcname
        test_id            TYPE numeric
      EXPORTING
        datadir_entry      TYPE ty_datadir_entry
        attributes         TYPE ty_test_attr
      CHANGING
        param_bindings_pbo TYPE abap_func_parmbind_tab OPTIONAL
        param_bindings_pai TYPE abap_func_parmbind_tab OPTIONAL
      RAISING
        zcx_fm_test_data.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Name of function module</p>
    "! @parameter test_id | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS delete
      IMPORTING
        fm_name TYPE tfdir-funcname
        test_id TYPE numeric
      RAISING
        zcx_fm_test_data.

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

    CLASS-METHODS in_describe_interface
      IMPORTING
        p_fname               TYPE funcname
      EXPORTING
        struc_info_table_copy TYPE nf2ty_struc_info_table
        fdesc2                TYPE ty_fdesc.

    CLASS-METHODS in_describe_fields
      IMPORTING
        p_fname               TYPE funcname
        p_parameter           TYPE parameter
        p_typ                 TYPE rs38l_typ
        p_struc               TYPE likefield
        p_kind                TYPE char1
      CHANGING
        struc_info_table_copy TYPE nf2ty_struc_info_table
        fdesc2                TYPE ty_fdesc.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter fugr_name | <p class="shorttext synchronized" lang="en">Function group or empty if the function module doesn't exist</p>
    CLASS-METHODS get_fugr_name
      IMPORTING
        fm_name          TYPE tfdir-funcname
      RETURNING
        VALUE(fugr_name) TYPE eufunc-gruppe.

    CLASS-METHODS get_free_test_id
      IMPORTING
        fm_name        TYPE tfdir-funcname
      RETURNING
        VALUE(test_id) TYPE eufunc-nummer.

    CLASS-METHODS save
      IMPORTING
        fm_name            TYPE tfdir-funcname
        title              TYPE ty_datadir_entry-title
        stepid             TYPE ty_datadir_entry-stepid
        new_param_bindings TYPE abap_func_parmbind_tab
        param_bindings_pbo TYPE abap_func_parmbind_tab
        attributes         TYPE ty_test_attr
      RAISING
        zcx_fm_test_data.

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

    CLASS-DATA: params_rtts TYPE ty_ut_rtts."zcl_fm_params_rtts=>ty_params_rtts.

ENDCLASS.



CLASS zcl_fm_test_data IMPLEMENTATION.


  METHOD complete_param_bindings.

    DATA: ref_parameter     TYPE REF TO data,
          ref_parameter_pbo TYPE REF TO data.

    DATA(params_rtts) = get_fm_params_rtts( funcname = fm_name ).

    new_param_bindings = VALUE #( ).
    param_bindings_pbo = VALUE #( ).
    LOOP AT params_rtts REFERENCE INTO DATA(param_rtts).
      CREATE DATA ref_parameter TYPE HANDLE param_rtts->type.
      new_param_bindings = VALUE #( BASE new_param_bindings
          ( name  = param_rtts->name
            kind  = param_rtts->call_function_kind
            value = ref_parameter ) ).
      IF param_rtts->call_function_kind <> abap_func_importing.
        ASSIGN param_bindings[ name = param_rtts->name ] TO FIELD-SYMBOL(<param_binding>).
        IF sy-subrc = 0.
          IF stepid = 'PAI'.
            ASSIGN <param_binding>-value->* TO FIELD-SYMBOL(<input_parameter>).
            ASSIGN ref_parameter->* TO FIELD-SYMBOL(<parameter_value>).
            <parameter_value> = <input_parameter>.
          ENDIF.
          param_bindings_pbo = VALUE #( BASE param_bindings_pbo
                ( name = param_rtts->name value = <param_binding>-value ) ).
        ELSE.
          CREATE DATA ref_parameter_pbo TYPE HANDLE param_rtts->type.
          param_bindings_pbo = VALUE #( BASE param_bindings_pbo
                ( name = param_rtts->name value = ref_parameter_pbo ) ).
        ENDIF.
      ENDIF.
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

    save( fm_name            = fm_name
          title              = title
          stepid             = 'PBO'
          new_param_bindings = new_param_bindings
          param_bindings_pbo = param_bindings_pbo
          attributes         = VALUE #( lower_case = lower_case ) ).

  ENDMETHOD.


  METHOD delete.

    DATA(context) = load_test_context( fm_name ).

    DELETE context-te_datadir WHERE dataid = test_id.

    save_test_context( fm_name = fm_name context = context ).

    DATA(fugr_name) = get_fugr_name( fm_name ).
    DELETE FROM eufunc
        WHERE relid   = 'FL'
          AND gruppe  = fugr_name
          AND name    = fm_name
          AND nummer  = test_id.

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
        FOR <fm_exception> IN fm_exceptions INDEX INTO row_number
        ( name  = <fm_exception>-parameter
          value = row_number ) ).

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


  METHOD get_free_test_id.

    " TODO simulate the standard, which looks for gaps between numbers.

    TYPES ty_p TYPE p LENGTH 10 DECIMALS 0.

    DATA(fugr_name) = get_fugr_name( fm_name ).

    SELECT MAX( nummer )
        FROM eufunc
        WHERE relid  = 'FL'
          AND gruppe = @fugr_name
          AND name   = @fm_name
          AND nummer <> '999'
        INTO @test_id.

    DATA(test_id_p) = CONV ty_p( test_id + 1 ).
    test_id = test_id_p.

  ENDMETHOD.


  METHOD get_fugr_name.

    SELECT SINGLE pname
        FROM tfdir
        WHERE funcname = @fm_name INTO @fugr_name.
    REPLACE 'SAPL' IN fugr_name WITH ``.

  ENDMETHOD.


  METHOD in_describe_fields.

    " This method is inspired from subroutine IN_DESCRIBE_FIELDS in SAPLSEUJ.

    DATA: l_sline TYPE nf2ty_info_entry,
          fdesc   TYPE ty_fdesc_entry.

*  call function 'SFCS_FA_PARAMETER_DESCRIBE'
*    exporting
*      function         = p_fname
*      parameter        = p_parameter
*      cb_program       = 'SAPLSEUJ'
*    exceptions
*      generation_error = 1
*      others           = 2.
    READ TABLE params_rtts ASSIGNING FIELD-SYMBOL(<param_rtts>)
          WITH KEY name = p_parameter.
    ASSERT sy-subrc = 0.

    DATA(l_type_info) = VALUE nf2ty_struc_info( ).
    ASSIGN <param_rtts>-value->* TO FIELD-SYMBOL(<value>).
    CALL FUNCTION 'RS_COMPLEX_OBJECT_TYPEINFO_GET'
      EXPORTING
        object_name = p_parameter
        object      = <value>
      IMPORTING
        type_info   = l_type_info
      EXCEPTIONS
        OTHERS      = 0.

    struc_info_table_copy = VALUE #( BASE struc_info_table_copy
        ( param_name = p_parameter
          type_info  = l_type_info ) ).

    CLEAR fdesc.
    fdesc-name = p_parameter.
    IF NOT p_typ IS INITIAL.
      fdesc-table = p_typ.
      CASE p_typ.
        WHEN 'I' OR 'C' OR 'N' OR 'P' OR 'F' OR 'b' OR 's' OR 'T'
             OR 'D' OR 'X' OR 'STRING' OR 'XSTRING' OR cl_abap_typedescr=>typekind_int8.
*           or cl_abap_typedescr=>typekind_dtday
*           or cl_abap_typedescr=>typekind_tsecond
*           or cl_abap_typedescr=>typekind_utcsecond
*           or cl_abap_typedescr=>typekind_utclong.
          CONCATENATE '''' p_typ '''' INTO fdesc-table.
*  In Hochkommata, damit man erkennt, daß es ein elemntarer Typ ist.
* wird es noch benötigt, wenn ftype Y wegfällt?
        WHEN 'TABLE' OR 'STANDARD TABLE' OR 'HASHED TABLE' OR
             'SORTED TABLE' OR 'INDEX TABLE'.
        WHEN 'ANY'.
          CLEAR fdesc-table.
      ENDCASE.
    ELSE.
      fdesc-table = p_struc.
    ENDIF.
    READ TABLE l_type_info INDEX 1 INTO l_sline.
    fdesc-type = l_sline-struc_type.
    IF l_sline-vlength > 200.
      l_sline-vlength = 200.
    ENDIF.
    IF fdesc-type = 'y' OR fdesc-type = 'g'.
*   Strings auf char 200
      l_sline-vlength = 200.
    ENDIF.
    fdesc-length = l_sline-vlength.
    fdesc-hlength = l_sline-vlength.
    fdesc-ilength = l_sline-vlength.
*    IF l_sline-vlength > max_field_length.
*      max_field_length = l_sline-vlength.
*      max_field_length_pai = l_sline-vlength.
*    ENDIF.
    IF p_struc IS INITIAL.
      CASE fdesc-type.
***JR 14.9.98
        WHEN 'h' OR 'v' OR if_wb_constants=>flag_struc OR 'u'.
*      when 'h' or 'v' or 'T' or IF_WB_CONSTANTS=>flag_struc or 'u'.
          CONCATENATE p_kind fdesc-type INTO fdesc-ftype.
        WHEN OTHERS.
          fdesc-ftype = p_kind.
      ENDCASE.
    ELSE.
      CONCATENATE p_kind 'S' INTO fdesc-ftype.
    ENDIF.
    APPEND fdesc TO fdesc2.
  ENDMETHOD.


  METHOD in_describe_interface.

    " This method is inspired from subroutine IN_DESCRIBE_INTERFACE in SAPLSEUJ.

    DATA: if_import     TYPE TABLE OF rsimp,
          if_change     TYPE TABLE OF rscha,
          if_export     TYPE TABLE OF rsexp,
          if_tables     TYPE TABLE OF rstbl,
          if_except     TYPE TABLE OF rsexc,
          documentation TYPE TABLE OF funct.

    REFRESH: if_import, if_export, if_change, if_tables, if_except,
             documentation.

    CALL FUNCTION 'FUNCTION_IMPORT_DOKU'
      EXPORTING
        funcname           = p_fname
        with_enhancements  = 'X'
      TABLES
        exception_list     = if_except
        export_parameter   = if_export
        import_parameter   = if_import
        changing_parameter = if_change
        tables_parameter   = if_tables
        dokumentation      = documentation
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.

    LOOP AT if_import INTO DATA(f_import).
      in_describe_fields(
        EXPORTING
          p_fname               = p_fname
          p_parameter           = f_import-parameter
          p_typ                 = f_import-typ
          p_struc               = f_import-dbfield
          p_kind                = 'I'
        CHANGING
          struc_info_table_copy = struc_info_table_copy
          fdesc2                = fdesc2 ).
    ENDLOOP.

* --Changefelder : Name, Länge und Typ bestimmen
    LOOP AT if_change INTO DATA(f_change).
      in_describe_fields(
        EXPORTING
          p_fname               = p_fname
          p_parameter           = f_change-parameter
          p_typ                 = f_change-typ
          p_struc               = f_change-dbfield
          p_kind                = 'C'
        CHANGING
          struc_info_table_copy = struc_info_table_copy
          fdesc2                = fdesc2 ).
    ENDLOOP.

* --Exportfelder : Name, Länge und Typ bestimmen
    LOOP AT if_export INTO DATA(f_export).                   "keine neue Beschreibung wenn
      READ TABLE fdesc2 WITH KEY name = f_export-parameter INTO DATA(fdesc).  "schon IMPORT
      IF sy-subrc = 0.                   "Schon Input-Parameter ?
        IF fdesc-ftype = 'I'.            "nur Typ entsprechend ändern
          fdesc-ftype = 'IO'.
        ELSEIF fdesc-ftype = 'IS'.
          fdesc-ftype = 'IOS'.
        ENDIF.
        MODIFY fdesc2 FROM fdesc INDEX sy-tabix.
      ELSE.
        in_describe_fields(
          EXPORTING
            p_fname               = p_fname
            p_parameter           = f_export-parameter
            p_typ                 = f_export-typ
            p_struc               = f_export-dbfield
            p_kind                = 'O'
          CHANGING
            struc_info_table_copy = struc_info_table_copy
            fdesc2                = fdesc2 ).
      ENDIF.
    ENDLOOP.

* --Tablesfelder : Name, Länge und Typ bestimmen
    LOOP AT if_tables INTO DATA(f_tables).
      in_describe_fields(
        EXPORTING
          p_fname               = p_fname
          p_parameter           = f_tables-parameter
          p_typ                 = f_tables-typ
          p_struc               = f_tables-dbstruct
          p_kind                = 'T'
        CHANGING
          struc_info_table_copy = struc_info_table_copy
          fdesc2                = fdesc2 ).
    ENDLOOP.

* --Exceptions : Nix zu Bestimmen
    LOOP AT if_except INTO DATA(f_except).
      CLEAR fdesc.
      fdesc-name = f_except-exception.
      APPEND fdesc TO fdesc2.
    ENDLOOP.

* Abtrennen
    CLEAR fdesc.
    fdesc-name = '*'.
    APPEND fdesc TO fdesc2.

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
    DATA(dataid) = CONV eufunc-nummer( CONV i( test_id ) ).

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
        RAISE EXCEPTION TYPE zcx_fm_test_data.
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
      CATCH zcx_expimp_table.
        RAISE EXCEPTION TYPE zcx_fm_test_data.
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

*    DATA(params_rtts) = get_fm_params_rtts( funcname = fm_name ).

    SORT tab_cpar BY name.

    LOOP AT tab_cpar REFERENCE INTO cpar WHERE name CP '%_+*'.

      INSERT VALUE abap_func_parmbind(
              kind  = SWITCH #( cpar->name(3) WHEN '%_I' THEN '' )
              name  = cpar->name+3
              value = cpar->dref )
          INTO TABLE param_bindings_pbo.

    ENDLOOP.

*    LOOP AT params_rtts REFERENCE INTO DATA(param_rtts).
*
*      IF param_rtts->call_function_kind <> abap_func_importing.
*        parameter_name = '%_I' && param_rtts->name.
*        READ TABLE tab_cpar WITH KEY name = parameter_name REFERENCE INTO cpar.
*        IF sy-subrc = 0.
*          ASSIGN param_bindings_pbo[
*                  kind = param_rtts->call_function_kind
*                  name = param_rtts->name ]
*              TO <param_binding>.
*          IF sy-subrc <> 0.
*            ASSIGN param_bindings_pbo[
*                    kind = 0
*                    name = param_rtts->name ]
*                TO <param_binding>.
*          ENDIF.
*          IF sy-subrc = 0.
*            ASSIGN cpar->dref->* TO <cpar_value>.
*            ASSIGN <param_binding>-value->* TO <param_binding_value>.
*            <param_binding_value> = <cpar_value>.
*          ELSE.
*            INSERT VALUE abap_func_parmbind( kind = param_rtts->call_function_kind name = param_rtts->name )
*                INTO TABLE param_bindings_pbo
*                ASSIGNING <param_binding>.
*            <param_binding>-value = cpar->dref.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      IF param_rtts->call_function_kind <> abap_func_exporting.
*        parameter_name = '%_V' && param_rtts->name.
*        READ TABLE tab_cpar WITH KEY name = parameter_name REFERENCE INTO cpar.
*        IF sy-subrc = 0.
*          ASSIGN param_bindings_pai[
*                  kind = param_rtts->call_function_kind
*                  name = param_rtts->name ]
*              TO <param_binding>.
*          IF sy-subrc <> 0.
*            ASSIGN param_bindings_pai[
*                    kind = 0
*                    name = param_rtts->name ]
*                TO <param_binding>.
*          ENDIF.
*          IF sy-subrc = 0.
*            ASSIGN cpar->dref->* TO <cpar_value>.
*            ASSIGN <param_binding>-value->* TO <param_binding_value>.
*            <param_binding_value> = <cpar_value>.
*          ELSE.
*            INSERT VALUE abap_func_parmbind( kind = param_rtts->call_function_kind name = param_rtts->name )
*                INTO TABLE param_bindings_pai
*                ASSIGNING <param_binding>.
*            <param_binding>-value = cpar->dref.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.


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
      CATCH zcx_expimp_table.
        RAISE EXCEPTION TYPE zcx_fm_test_data.
    ENDTRY.

    ASSIGN tab_cpar_dir[ name = 'TE_DATADIR' ] TO <cpar>.
    FIELD-SYMBOLS <te_datadir> TYPE STANDARD TABLE.
    ASSIGN <cpar>-dref->* TO <te_datadir>.
    datadir_entry = <te_datadir>[ ('CMP00001') = dataid ].

  ENDMETHOD.


  METHOD load_test_context.

    DATA(local_params_rtts) = get_fm_params_rtts( fm_name ).

    params_rtts = VALUE #( ).
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
      in_describe_interface(
        EXPORTING
          p_fname               = fm_name
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

    DATA(test_id) = get_free_test_id( fm_name ).

    DATA(tab_cpar) = VALUE tab_cpar(
        ( LINES OF VALUE #(
            FOR <param_binding2> IN param_bindings_pbo
            ( name = '%_I' && <param_binding2>-name dref = <param_binding2>-value ) ) )
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
            ( name = '%_V' && <param_binding2>-name dref = <param_binding2>-value ) ) )
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
                dataid  = test_id )
            wa       = VALUE eufunc(
                langu   = ' '
                autor   = sy-uname
                datum   = sy-datum
                zeit    = sy-uzeit
                version = '  1 ' )
            tab_cpar = tab_cpar ).
      CATCH zcx_expimp_table.
        RAISE EXCEPTION TYPE zcx_fm_test_data.
    ENDTRY.


    DATA(context) = load_test_context( fm_name ).

    context-te_datadir = VALUE #(
        BASE context-te_datadir
        ( VALUE #(
          dataid = test_id
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

  METHOD get_fm_params_rtts.

    TRY.
        params_rtts = zcl_fm_params_rtts=>get( funcname = funcname ).
      CATCH zcx_fm_params_rtts.
        RAISE EXCEPTION TYPE zcx_fm_test_data.
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
            RAISE EXCEPTION TYPE zcx_fm_test_data.
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
