"! <p class="shorttext synchronized">Exception</p>
CLASS zcx_updownci_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA iv_text TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        textid    LIKE textid   OPTIONAL
        !previous LIKE previous OPTIONAL
        iv_text   TYPE string   OPTIONAL.

    METHODS get_text REDEFINITION.
ENDCLASS.


CLASS zcx_updownci_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
        textid   = textid
        previous = previous ).
    me->iv_text = iv_text.
  ENDMETHOD.

  METHOD get_text.
    IF iv_text IS NOT INITIAL.
      result = iv_text.
    ELSE.
      result = super->get_text( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
