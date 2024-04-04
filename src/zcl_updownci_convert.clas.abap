"! <p class="shorttext synchronized">Conversions</p>
"! <p>
"! This class is mostly copied from ZCL_ABAPGIT_CONVERT:
"! https://github.com/abapGit/abapGit
"! </p>
CLASS zcl_updownci_convert DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        iv_string         TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_updownci_exception.

ENDCLASS.


CLASS zcl_updownci_convert IMPLEMENTATION.
  METHOD string_to_xstring_utf8.
    rv_xstring = lcl_out=>convert( iv_string ).
  ENDMETHOD.
ENDCLASS.
