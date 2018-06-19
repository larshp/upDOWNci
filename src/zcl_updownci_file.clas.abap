CLASS zcl_updownci_file DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      download
        IMPORTING iv_default TYPE string
                  iv_xml     TYPE string
        RAISING   cx_bcs zcx_updownci_exception,
      upload
        RETURNING VALUE(rv_xml) TYPE string
        RAISING   cx_bcs zcx_updownci_exception.

ENDCLASS.



CLASS ZCL_UPDOWNCI_FILE IMPLEMENTATION.


  METHOD download.

    DATA: lt_rawdata    TYPE solix_tab,
          lv_action     TYPE i,
          lv_so_obj_len TYPE so_obj_len,
          lv_size       TYPE i,
          lv_filename   TYPE string,
          lv_path       TYPE string,
          lv_fullpath   TYPE string.


    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension    = 'xml'
        default_file_name    = iv_default
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_fullpath
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).                         "#EC NOTEXT
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error from file_save_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    cl_bcs_convert=>string_to_solix(
      EXPORTING
        iv_string = iv_xml
      IMPORTING
        et_solix  = lt_rawdata
        ev_size   = lv_so_obj_len ).
    lv_size = lv_so_obj_len.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
      bin_filesize = lv_size
        filename                  = lv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error from gui_download'.
    ENDIF.

  ENDMETHOD.


  METHOD upload.

    DATA: lv_action     TYPE i,
          lt_data       TYPE TABLE OF text255,
          lv_filename   TYPE string,
          lt_file_table TYPE filetable,
          ls_file_table LIKE LINE OF lt_file_table,
          lv_rc         TYPE i.


    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_extension       = 'xml'
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error from file_open_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    ASSERT lines( lt_file_table ) = 1.
    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    lv_filename = ls_file_table-filename.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_filename
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error from gui_upload'.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xml.

  ENDMETHOD.                    "upload
ENDCLASS.
