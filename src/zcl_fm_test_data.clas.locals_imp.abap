*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_saplseuj_redef DEFINITION DEFERRED.
CLASS zcl_fm_test_data DEFINITION LOCAL FRIENDS lcl_saplseuj_redef.

CLASS lcl_saplseuj_redef DEFINITION.

  PUBLIC SECTION.

    METHODS in_describe_interface
      IMPORTING
        p_fname               TYPE funcname
        params_rtts           TYPE zcl_fm_test_data=>ty_ut_rtts
      EXPORTING
        struc_info_table_copy TYPE nf2ty_struc_info_table
        fdesc2                TYPE zcl_fm_test_data=>ty_fdesc.

    METHODS in_describe_fields
      IMPORTING
        p_fname               TYPE funcname
        p_parameter           TYPE parameter
        p_typ                 TYPE rs38l_typ
        p_struc               TYPE likefield
        p_kind                TYPE char1
        params_rtts           TYPE zcl_fm_test_data=>ty_ut_rtts
      CHANGING
        struc_info_table_copy TYPE nf2ty_struc_info_table
        fdesc2                TYPE zcl_fm_test_data=>ty_fdesc.

    DATA: app TYPE REF TO zcl_fm_test_data READ-ONLY.

ENDCLASS.

CLASS lcl_saplseuj_redef IMPLEMENTATION.

  METHOD in_describe_fields.

    " This method is copied and adapted from subroutine IN_DESCRIBE_FIELDS in SAPLSEUJ (version 7.52 SP 0):
    " 1) instead of global variables, two new parameters are returned.
    " 2) Modifications at beginning and end are marked between *===========> and *<===========.

    DATA: l_sline TYPE nf2ty_info_entry.

*===========>
    DATA: fdesc                TYPE zcl_fm_test_data=>ty_fdesc_entry,
          max_field_length     TYPE i,
          max_field_length_pai TYPE i.

    READ TABLE params_rtts ASSIGNING FIELD-SYMBOL(<param_rtts>)
          WITH KEY name = p_parameter.
    ASSERT sy-subrc = 0.

    DATA(struc_info_table_entry) = VALUE nf2ty_struc_info_table_entry( ).
    ASSIGN <param_rtts>-value->* TO FIELD-SYMBOL(<value>).
    CALL FUNCTION 'RS_COMPLEX_OBJECT_TYPEINFO_GET'
      EXPORTING
        object_name = p_parameter
        object      = <value>
      IMPORTING
        type_info   = struc_info_table_entry-type_info
      EXCEPTIONS
        OTHERS      = 0.

    struc_info_table_copy = VALUE #( BASE struc_info_table_copy
        ( param_name = p_parameter
          type_info  = struc_info_table_entry-type_info ) ).

*  call function 'SFCS_FA_PARAMETER_DESCRIBE'
*    exporting
*      function         = p_fname
*      parameter        = p_parameter
*      cb_program       = 'SAPLSEUJ'
*    exceptions
*      generation_error = 1
*      others           = 2.
*<===========
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
    READ TABLE struc_info_table_entry-type_info INDEX 1 INTO l_sline.
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
    IF l_sline-vlength > max_field_length.
      max_field_length = l_sline-vlength.
      max_field_length_pai = l_sline-vlength.
    ENDIF.
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
*===========>
    APPEND fdesc TO fdesc2.
*    APPEND fdesc.
*<===========

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
          params_rtts           = params_rtts
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
          params_rtts           = params_rtts
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
            params_rtts           = params_rtts
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
          params_rtts           = params_rtts
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

ENDCLASS.
