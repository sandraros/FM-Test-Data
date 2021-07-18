CLASS zcx_fm_test_data DEFINITION
      INHERITING FROM cx_static_check
      PUBLIC
      FINAL
      CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        text     TYPE clike
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL.
    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.
    DATA: text TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_FM_TEST_DATA IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
        textid   = textid
        previous = previous ).
    me->text = text.
  ENDMETHOD.


  METHOD get_longtext.
    result = text.
  ENDMETHOD.


  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.
