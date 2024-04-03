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

    METHODS download
      IMPORTING
        iv_xstring TYPE xstring
      RAISING
        cx_bcs zcx_updownci_exception.
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

*    CL_BCS_CONVERT=>xstring_to_string(
*    EXPORTING
*      iv_xstr   = LV_ZIP_XSTRING
*      iv_cp     =  1100                " SAP character set identification
*    RECEIVING
*      rv_string = DATA(lv_string)
*  ).

    " data lv_size type i.
    "
    "     CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    "   EXPORTING
    "     BUFFER        = LV_ZIP_XSTRING
    "   IMPORTING
    "     OUTPUT_LENGTH = lv_size
    "   TABLES
    "     BINARY_TAB    = ME->PT_ZIP_BIN_DATA.
  ENDMETHOD.

  METHOD read_check_variants_from_db.
    SELECT ciuser checkvname
      FROM scichkv_hd
      INTO TABLE rt_check_variants.
  ENDMETHOD.

  METHOD download.
    DATA lt_rawdata TYPE solix_tab.
    DATA lv_action TYPE i.
*    DATA lv_so_obj_len TYPE so_obj_len.
    DATA lv_size TYPE i.
    DATA lv_filename TYPE string.
    DATA lv_path TYPE string.
    DATA lv_fullpath TYPE string.

    cl_gui_frontend_services=>file_save_dialog( EXPORTING  default_extension    = 'zip'
                                                           default_file_name    = 'export'
                                                CHANGING   filename             = lv_filename
                                                           path                 = lv_path
                                                           fullpath             = lv_fullpath
                                                           user_action          = lv_action
                                                EXCEPTIONS cntl_error           = 1
                                                           error_no_gui         = 2
                                                           not_supported_by_gui = 3
                                                           OTHERS               = 4 ) ##NO_TEXT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception
        EXPORTING
          iv_text = 'error from file_save_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstring
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_rawdata.

*    cl_bcs_convert=>string_to_solix(
*      EXPORTING
*        iv_string = iv_xml
*      IMPORTING
*        et_solix  = lt_rawdata
*        ev_size   = lv_so_obj_len ).
*    lv_size = lv_so_obj_len.

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize            = lv_size
                                                       filename                = lv_fullpath
                                                       filetype                = 'BIN'
                                            CHANGING   data_tab                = lt_rawdata
                                            EXCEPTIONS file_write_error        = 1
                                                       no_batch                = 2
                                                       gui_refuse_filetransfer = 3
                                                       invalid_type            = 4
                                                       no_authority            = 5
                                                       unknown_error           = 6
                                                       header_not_allowed      = 7
                                                       separator_not_allowed   = 8
                                                       filesize_not_allowed    = 9
                                                       header_too_long         = 10
                                                       dp_error_create         = 11
                                                       dp_error_send           = 12
                                                       dp_error_write          = 13
                                                       unknown_dp_error        = 14
                                                       access_denied           = 15
                                                       dp_out_of_memory        = 16
                                                       disk_full               = 17
                                                       dp_timeout              = 18
                                                       file_not_found          = 19
                                                       dataprovider_exception  = 20
                                                       control_flush_error     = 21
                                                       not_supported_by_gui    = 22
                                                       error_no_gui            = 23
                                                       OTHERS                  = 24 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception
        EXPORTING
          iv_text = 'error from gui_download'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_program( )->run( ).
