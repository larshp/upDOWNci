CLASS zcl_updownci_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS add_redirect
      IMPORTING
        !iv_from TYPE seoclsname
        !iv_to   TYPE seoclsname .
    CLASS-METHODS find_include
      IMPORTING
        !iv_class         TYPE seoclsname
      RETURNING
        VALUE(rv_include) TYPE program
      RAISING
        zcx_updownci_exception .
    CLASS-METHODS attributes
      IMPORTING
        !iv_class            TYPE seoclsname
      RETURNING
        VALUE(rt_attributes) TYPE seo_attributes
      RAISING
        zcx_updownci_exception.
  PROTECTED SECTION.

    TYPES: BEGIN OF ty_redirect,
             from TYPE seoclsname,
             to   TYPE seoclsname,
           END OF ty_redirect.

    CLASS-DATA: gt_redirect TYPE STANDARD TABLE OF ty_redirect WITH DEFAULT KEY.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_updownci_class IMPLEMENTATION.


  METHOD add_redirect.

    FIELD-SYMBOLS: <ls_redirect> LIKE LINE OF gt_redirect.

    APPEND INITIAL LINE TO gt_redirect ASSIGNING <ls_redirect>.
    <ls_redirect>-from = iv_from.
    <ls_redirect>-to   = iv_to.

  ENDMETHOD.


  METHOD attributes.

    DATA: lt_attributes         TYPE seo_attributes,
          lv_super              TYPE seoclsname,
          ls_redirect           LIKE LINE OF gt_redirect,
          lo_class              TYPE REF TO cl_oo_class,
          lo_class_not_existent TYPE REF TO cx_class_not_existent.


    READ TABLE gt_redirect INTO ls_redirect WITH KEY from = iv_class.

    TRY.
        IF ls_redirect IS NOT INITIAL.
          lo_class ?= cl_oo_class=>get_instance( ls_redirect-to ).
        ELSE.
          lo_class ?= cl_oo_class=>get_instance( iv_class ).
        ENDIF.
      CATCH cx_class_not_existent INTO lo_class_not_existent.
        RAISE EXCEPTION TYPE zcx_updownci_exception
          EXPORTING
            iv_text  = 'Class does not exist'
            previous = lo_class_not_existent.
    ENDTRY.

    rt_attributes = lo_class->get_attributes( ).

    lv_super = lo_class->get_superclass( ).
    IF NOT lv_super IS INITIAL.
      lt_attributes = attributes( lv_super ).
      APPEND LINES OF lt_attributes TO rt_attributes.
    ENDIF.

  ENDMETHOD.                    "attributes


  METHOD find_include.

    DATA: lo_class              TYPE REF TO cl_oo_class,
          lv_super              TYPE seoclsname,
          ls_redirect           LIKE LINE OF gt_redirect,
          ls_mtdkey             TYPE seocpdkey,
          lo_class_not_existent TYPE REF TO cx_class_not_existent.


    READ TABLE gt_redirect INTO ls_redirect WITH KEY from = iv_class.
    IF sy-subrc = 0.
      ls_mtdkey-clsname = ls_redirect-to.
    ELSE.
      ls_mtdkey-clsname = iv_class.
    ENDIF.
    ls_mtdkey-cpdname = 'PUT_ATTRIBUTES'.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = rv_include
      EXCEPTIONS
        class_not_existing  = 1
        method_not_existing = 2
        OTHERS              = 3 ).
    IF sy-subrc = 2.
      TRY.
          lo_class ?= cl_oo_class=>get_instance( iv_class ).
        CATCH cx_class_not_existent INTO lo_class_not_existent.
          RAISE EXCEPTION TYPE zcx_updownci_exception
            EXPORTING
              iv_text  = 'Class does not exist'
              previous = lo_class_not_existent.
      ENDTRY.
      lv_super = lo_class->get_superclass( ).
      rv_include = find_include( lv_super ).
    ELSEIF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error while finding method include'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
