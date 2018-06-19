class ZCL_UPDOWNCI_CLASS definition
  public
  final
  create public .

public section.

  class-methods FIND_INCLUDE
    importing
      !IV_CLASS type SEOCLSNAME
    returning
      value(RV_INCLUDE) type PROGRAM
    raising
      CX_STATIC_CHECK .
  class-methods ATTRIBUTES
    importing
      !IV_CLASS type SEOCLSNAME
    returning
      value(RT_ATTRIBUTES) type SEO_ATTRIBUTES
    raising
      CX_STATIC_CHECK .
ENDCLASS.



CLASS ZCL_UPDOWNCI_CLASS IMPLEMENTATION.


  METHOD attributes.

    DATA: lt_attributes TYPE seo_attributes,
          lv_super      TYPE seoclsname,
          lo_class      TYPE REF TO cl_oo_class.


    lo_class ?= cl_oo_class=>get_instance( iv_class ).

    rt_attributes = lo_class->get_attributes( ).

    lv_super = lo_class->get_superclass( ).
    IF NOT lv_super IS INITIAL.
      lt_attributes = attributes( lv_super ).
      APPEND LINES OF lt_attributes TO rt_attributes.
    ENDIF.

  ENDMETHOD.                    "attributes


  METHOD find_include.

    DATA: lo_class  TYPE REF TO cl_oo_class,
          lv_super  TYPE seoclsname,
          ls_mtdkey TYPE seocpdkey.


    ls_mtdkey-clsname = iv_class.
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
      lo_class ?= cl_oo_class=>get_instance( iv_class ).
      lv_super = lo_class->get_superclass( ).
      rv_include = find_include( lv_super ).
    ELSEIF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error while finding method include'.
    ENDIF.

  ENDMETHOD.                    "find_include
ENDCLASS.
