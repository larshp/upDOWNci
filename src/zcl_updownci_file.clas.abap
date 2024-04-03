CLASS zcl_updownci_file DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS download_string
      IMPORTING
        iv_default_file_name TYPE string
        iv_default_extension TYPE string
        iv_content           TYPE string
      RAISING
        cx_bcs zcx_updownci_exception.

    CLASS-METHODS download_xstring
      IMPORTING
        iv_default_file_name TYPE string
        iv_default_extension TYPE string
        iv_content           TYPE xstring
      RAISING
        cx_bcs zcx_updownci_exception.

    CLASS-METHODS upload
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        cx_bcs zcx_updownci_exception.

  PRIVATE SECTION.
    CLASS-METHODS download_to_client
      IMPORTING
        iv_size           TYPE i
        iv_fullpath       TYPE string
        VALUE(it_rawdata) TYPE solix_tab
      RAISING
        zcx_updownci_exception.

    CLASS-METHODS file_save_dialog
      IMPORTING
        iv_default_file_name TYPE string
        iv_default_extension TYPE string
      EXPORTING
        ev_fullpath          TYPE string
        ev_size              TYPE i
        ev_so_obj_len        TYPE so_obj_len
        ev_action            TYPE i
        et_rawdata           TYPE solix_tab
      RAISING
        zcx_updownci_exception.
ENDCLASS.


CLASS zcl_updownci_file IMPLEMENTATION.
  METHOD download_string.
    DATA lv_fullpath TYPE string.
    DATA lv_size TYPE i.
    DATA lv_so_obj_len TYPE so_obj_len.
    DATA lv_action TYPE i.
    DATA lt_rawdata TYPE solix_tab.

    file_save_dialog(
      EXPORTING
        iv_default_file_name = iv_default_file_name
        iv_default_extension = iv_default_extension
      IMPORTING
        ev_fullpath          = lv_fullpath
        ev_action            = lv_action
        et_rawdata           = lt_rawdata ).

    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    cl_bcs_convert=>string_to_solix(
      EXPORTING
        iv_string = iv_content
      IMPORTING
        et_solix  = lt_rawdata
        ev_size   = lv_so_obj_len ).

    lv_size = lv_so_obj_len.

    download_to_client(
        iv_size     = lv_size
        iv_fullpath = lv_fullpath
        it_rawdata  = lt_rawdata ).
  ENDMETHOD.

  METHOD file_save_dialog.
    DATA lv_filename TYPE string.
    DATA lv_path TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension    = iv_default_extension
        default_file_name    = iv_default_file_name
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = ev_fullpath
        user_action          = ev_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ) ##NO_TEXT.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error from file_save_dialog'.
    ENDIF.
  ENDMETHOD.

  METHOD download_to_client.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = iv_size
        filename                  = iv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = it_rawdata
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

  METHOD download_xstring.
    DATA lv_fullpath TYPE string.
    DATA lv_action TYPE i.
    DATA lt_rawdata TYPE solix_tab.

    file_save_dialog(
      EXPORTING
        iv_default_file_name = iv_default_file_name
        iv_default_extension = iv_default_extension
      IMPORTING
        ev_fullpath          = lv_fullpath
        ev_action            = lv_action
        et_rawdata           = lt_rawdata ).

    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_content ).

    download_to_client(
        iv_size     = xstrlen( iv_content )
        iv_fullpath = lv_fullpath
        it_rawdata  = lt_rawdata ).
  ENDMETHOD.

  METHOD upload.
    DATA lv_action TYPE i.
    DATA lt_data TYPE TABLE OF text255.
    DATA lv_filename TYPE string.
    DATA lt_file_table TYPE filetable.
    DATA ls_file_table LIKE LINE OF lt_file_table.
    DATA lv_rc TYPE i.

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
  ENDMETHOD.
ENDCLASS.
