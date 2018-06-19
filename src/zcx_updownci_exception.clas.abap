class ZCX_UPDOWNCI_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data IV_TEXT type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !IV_TEXT type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_UPDOWNCI_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->IV_TEXT = IV_TEXT .
  endmethod.
ENDCLASS.
