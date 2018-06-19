class ZCL_UPDOWNCI_VARIANT definition
  public
  create public .

public section.

  class-methods VERSION
    importing
      !IV_TESTNAME type SCI_TSTVAL-TESTNAME
    returning
      value(RV_VERSION) type SCI_TSTVAL-VERSION
    raising
      ZCX_UPDOWNCI_TEST_NOT_FOUND .
  class-methods HAS_ATTRIBUTES
    importing
      !IV_TESTNAME type SCI_TSTVAL-TESTNAME
    returning
      value(RV_HAS_ATTRIBUTES) type SYCHAR01
    raising
      ZCX_UPDOWNCI_TEST_NOT_FOUND .
  class-methods READ
    importing
      !IV_USER type SCI_USER
      !IV_NAME type SCI_CHKV
      !IV_CREATE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_VARIANT) type ref to CL_CI_CHECKVARIANT
    raising
      ZCX_UPDOWNCI_EXCEPTION .
  PRIVATE SECTION.
    CLASS-METHODS:
      create_object
        IMPORTING iv_testname   TYPE sci_tstval-testname
        RETURNING VALUE(ro_obj) TYPE REF TO object
        RAISING   zcx_updownci_test_not_found.

ENDCLASS.



CLASS ZCL_UPDOWNCI_VARIANT IMPLEMENTATION.


  METHOD create_object.

    TRY.
        CREATE OBJECT ro_obj TYPE (iv_testname).
      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE zcx_updownci_test_not_found.
    ENDTRY.

  ENDMETHOD.                    "create_object


  METHOD has_attributes.

    DATA: lo_obj TYPE REF TO object.

    FIELD-SYMBOLS: <lv_has_attributes> TYPE sychar01.


    lo_obj = create_object( iv_testname ).

    ASSIGN lo_obj->('HAS_ATTRIBUTES') TO <lv_has_attributes>.
    ASSERT sy-subrc = 0.

    rv_has_attributes = <lv_has_attributes>.

  ENDMETHOD.                    "has_attributes


  METHOD read.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user            = iv_user
        p_name            = iv_name
      RECEIVING
        p_ref             = ro_variant
      EXCEPTIONS
        chkv_not_exists   = 1
        missing_parameter = 2
        OTHERS            = 3 ).
    IF sy-subrc = 0.
      ro_variant->get_info(
        EXCEPTIONS
          could_not_read_variant = 1
          OTHERS                 = 2 ).
    ELSEIF sy-subrc = 1 AND iv_create = abap_true.
      cl_ci_checkvariant=>create(
        EXPORTING
          p_user              = iv_user
          p_name              = iv_name
        RECEIVING
          p_ref               = ro_variant
        EXCEPTIONS
          chkv_already_exists = 1
          locked              = 2
          error_in_enqueue    = 3
          not_authorized      = 4
          OTHERS              = 5 ).
    ENDIF.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error reading/creating variant'.
    ENDIF.

  ENDMETHOD.                    "read


  METHOD version.

    DATA: lo_obj TYPE REF TO object.

    FIELD-SYMBOLS: <lv_version> TYPE sci_vers.


    lo_obj = create_object( iv_testname ).

    ASSIGN lo_obj->('VERSION') TO <lv_version>.
    ASSERT sy-subrc = 0.

    rv_version = <lv_version>.

  ENDMETHOD.                    "version
ENDCLASS.
