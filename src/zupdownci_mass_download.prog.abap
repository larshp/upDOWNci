REPORT zupdownci_mass_download.

TABLES scichkv_hd.

SELECT-OPTIONS s_user FOR scichkv_hd-ciuser.
SELECT-OPTIONS s_name FOR scichkv_hd-checkvname.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS run.

  PRIVATE SECTION.
    TYPES: BEGIN OF gy_check_variant,
             ciuser     TYPE scichkv_hd-ciuser,
             checkvname TYPE scichkv_hd-checkvname,
           END OF gy_check_variant.
    TYPES gy_check_variants TYPE TABLE OF gy_check_variant WITH KEY ciuser checkvname.

    CLASS-METHODS read_check_variants_from_db
      RETURNING
        VALUE(rt_check_variants) TYPE gy_check_variants.

    CLASS-METHODS add_variant_to_zip
      IMPORTING
        io_zip           TYPE REF TO cl_abap_zip
        is_check_variant TYPE gy_check_variant
      RAISING
        zcx_updownci_exception.

    CLASS-METHODS download_zip
      IMPORTING
        io_zip TYPE REF TO cl_abap_zip
      RAISING
        cx_bcs
        zcx_updownci_exception.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    DATA lt_check_variants TYPE gy_check_variants.
    DATA lo_zip TYPE REF TO cl_abap_zip.
    DATA lx_static TYPE REF TO cx_static_check.
    DATA lx_exception TYPE REF TO zcx_updownci_exception.
    FIELD-SYMBOLS <ls_check_variant> TYPE gy_check_variant.

    lt_check_variants = read_check_variants_from_db( ).
    lo_zip = NEW cl_abap_zip( ).

    TRY.
        LOOP AT lt_check_variants ASSIGNING <ls_check_variant>.
          add_variant_to_zip(
              io_zip           = lo_zip
              is_check_variant = <ls_check_variant> ).
        ENDLOOP.

        download_zip( lo_zip ).
      CATCH zcx_updownci_exception INTO lx_exception.
        MESSAGE lx_exception->iv_text TYPE 'E'.
      CATCH cx_static_check INTO lx_static.
        MESSAGE lx_static TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD download_zip.
    DATA lv_zip_xstring TYPE xstring.

    lv_zip_xstring = io_zip->save( ).

    zcl_updownci_file=>download_xstring(
        iv_default_file_name = 'variants'
        iv_default_extension = 'zip'
        iv_content           = lv_zip_xstring ).
  ENDMETHOD.

  METHOD read_check_variants_from_db.
    SELECT ciuser checkvname
      FROM scichkv_hd
      INTO TABLE rt_check_variants
      WHERE ciuser     IN s_user
        AND checkvname IN s_name.
  ENDMETHOD.

  METHOD add_variant_to_zip.
    DATA lv_xml TYPE string.
    DATA lv_path TYPE string.
    DATA lv_filename TYPE string.
    DATA lv_name TYPE string.
    DATA lv_data TYPE xstring.
    DATA lx_exception TYPE REF TO zcx_updownci_exception.

    TRY.
        lv_xml = zcl_updownci=>build_xml( iv_name = is_check_variant-checkvname
                                                iv_user = is_check_variant-ciuser ).
        lv_path = `/`.

        IF is_check_variant-ciuser IS NOT INITIAL.
          " Place user-specific variants in a folder named after the user
          lv_path = |{ lv_path }{ is_check_variant-ciuser }/|.
        ENDIF.

        lv_filename = cl_http_utility=>escape_url( |{ is_check_variant-checkvname }.xml| ).

        lv_name = |{ lv_path }{ lv_filename }|.

        lv_data = zcl_updownci_convert=>string_to_xstring_utf8( lv_xml ).

        io_zip->add( name    = lv_name
                     content = lv_data ).
      CATCH zcx_updownci_exception INTO lx_exception.
        RAISE EXCEPTION TYPE zcx_updownci_exception
          EXPORTING
            iv_text  = |Failed to process variant { is_check_variant-ciuser } { is_check_variant-checkvname }|
            previous = lx_exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).
