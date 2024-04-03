REPORT zupdownci_mass_download.

CLASS lcl_program DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS run.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS gc_xml_file_extension TYPE string VALUE `.xml` ##NO_TEXT.
    CONSTANTS gc_slash_replacement TYPE char01 VALUE '#'.

    METHODS read_check_variants_from_db
      RETURNING
        VALUE(rt_check_variants) TYPE zcl_ci2git_staging=>gy_check_variants.


ENDCLASS.


CLASS lcl_program IMPLEMENTATION.
  METHOD run.
    DATA lt_check_variants TYPE zcl_ci2git_staging=>gy_check_variants.
    FIELD-SYMBOLS <ls_check_variant> TYPE zcl_ci2git_staging=>gy_check_variant.

    lt_check_variants = read_check_variants_from_db( ).

    DATA(lo_zip) = NEW cl_abap_zip( ).

    LOOP AT lt_check_variants ASSIGNING <ls_check_variant>.
      TRY.
          DATA(lv_xml) = zcl_updownci=>build_xml( iv_name = <ls_check_variant>-checkvname
                                                  iv_user = <ls_check_variant>-ciuser ).
        CATCH zcx_updownci_exception.
          CONTINUE.
      ENDTRY.

      DATA(lv_path) = `/`.

      IF <ls_check_variant>-ciuser IS NOT INITIAL.
        lv_path = |{ lv_path }{ <ls_check_variant>-ciuser }/|.
      ENDIF.

      DATA(lv_filename) = replace( val  = |{ <ls_check_variant>-checkvname }{ gc_xml_file_extension }|
                                   sub  = '/'
                                   with = gc_slash_replacement
                                   occ  = 0 ).

      DATA(lv_name) = |{ lv_path }{ lv_filename }|.

      DATA(lv_data) = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml ).

      lo_zip->add( name    = lv_name
                   content = lv_data ).
    ENDLOOP.

    DATA(lv_zip_xstring) = lo_zip->save( ).

    zcl_updownci_file=>download_xstring(
        iv_default_file_name = 'variants'
        iv_default_extension = 'zip'
        iv_content           = lv_zip_xstring ).
  ENDMETHOD.

  METHOD read_check_variants_from_db.
    SELECT ciuser checkvname
      FROM scichkv_hd
      INTO TABLE rt_check_variants.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_program( )->run( ).
