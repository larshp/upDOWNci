class ZCX_UPDOWNCI_TEST_NOT_FOUND definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_UPDOWNCI_TEST_NOT_FOUND IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.
ENDCLASS.
