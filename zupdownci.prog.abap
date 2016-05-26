REPORT zupdownci.

*********************************************************************************
*
* Upload and download SAP Code Inspector variants in XML format
* see https://github.com/larshp/upDOWNci
*
*
*********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2015 Lars Hvam
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*********************************************************************************

TABLES: seoclass.

PARAMETERS: p_user TYPE sci_user DEFAULT sy-uname,
            p_name TYPE sci_chkv DEFAULT 'DEFAULT' OBLIGATORY.

SELECT-OPTIONS: s_class FOR seoclass-clsname.

PARAMETERS: p_down TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_up   TYPE c RADIOBUTTON GROUP g1,
            p_test TYPE c AS CHECKBOX DEFAULT abap_true.

DEFINE _raise.
  raise exception type lcx_exception
    exporting
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

TYPES: BEGIN OF ty_wdy_ci_test_conventions ##needed, " used dynamically
         init_done       TYPE abap_bool,
         db_access       TYPE abap_bool,
         file_access     TYPE abap_bool,
         obsolete_screen TYPE abap_bool,
         program_flow    TYPE abap_bool,
         system_call     TYPE abap_bool,
         param_check     TYPE abap_bool,
         include_source  TYPE abap_bool,
         call_me         TYPE abap_bool,
       END OF ty_wdy_ci_test_conventions.

*----------------------------------------------------------------------*
*       CLASS lcx_exception DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text TYPE string.

ENDCLASS.                    "lcx_exception DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_exception IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcx_test_not_found DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_test_not_found DEFINITION INHERITING FROM cx_static_check FINAL.

ENDCLASS.                    "lcx_test_not_found DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_test_not_found IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_test_not_found IMPLEMENTATION.

ENDCLASS.                    "lcx_test_not_found IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      find_include
        IMPORTING iv_class          TYPE seoclsname
        RETURNING value(rv_include) TYPE program
        RAISING   cx_static_check,
      attributes
        IMPORTING iv_class             TYPE seoclsname
        RETURNING value(rt_attributes) TYPE seo_attributes
        RAISING   cx_static_check.

ENDCLASS.                    "lcl_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class IMPLEMENTATION.

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
      _raise 'error while finding method include'.
    ENDIF.

  ENDMETHOD.                    "find_include

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

ENDCLASS.                    "lcl_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_xml TYPE string OPTIONAL,
      write_variant
        IMPORTING ig_data     TYPE data OPTIONAL
                  iv_testname TYPE sci_tstval-testname
                  iv_version  TYPE sci_tstval-version
        RAISING   lcx_exception,
      read_variant
        EXPORTING ei_attributes TYPE REF TO if_ixml_node
                  ev_testname   TYPE sci_tstval-testname
                  ev_version    TYPE sci_tstval-version
        RAISING   lcx_exception,
      read_attributes
        IMPORTING ii_attributes TYPE REF TO if_ixml_node
        CHANGING  cg_data       TYPE data
        RAISING   lcx_exception,
      render
        RETURNING value(rv_xml) TYPE string.

  PRIVATE SECTION.

    DATA:
      mi_ixml     TYPE REF TO if_ixml,
      mi_root     TYPE REF TO if_ixml_node,
      mi_iterator TYPE REF TO if_ixml_node_iterator,
      mi_xml_doc  TYPE REF TO if_ixml_document.

    METHODS:
      find_node
        IMPORTING ii_node        TYPE REF TO if_ixml_node
                  iv_name        TYPE string
        RETURNING value(ri_node) TYPE REF TO if_ixml_node,
      structure_export
        IMPORTING ig_data   TYPE data
                  ii_parent TYPE REF TO if_ixml_element
        RAISING   lcx_exception,
      structure_import
        IMPORTING ii_parent TYPE REF TO if_ixml_node
        CHANGING  cg_data   TYPE data
        RAISING   lcx_exception,
      table_export
        IMPORTING it_data   TYPE STANDARD TABLE
                  ii_parent TYPE REF TO if_ixml_element
        RAISING   lcx_exception,
      table_import
        IMPORTING ii_parent TYPE REF TO if_ixml_node
        CHANGING  ct_data   TYPE STANDARD TABLE
        RAISING   lcx_exception,
      simple_export
        IMPORTING iv_data   TYPE simple
                  ii_parent TYPE REF TO if_ixml_element,
      simple_import
        IMPORTING ii_parent TYPE REF TO if_ixml_node
        CHANGING  cv_data   TYPE simple,
      error
        IMPORTING ii_parser TYPE REF TO if_ixml_parser.

ENDCLASS.                    "lcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.

  METHOD constructor.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser.


    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).

    IF iv_xml IS SUPPLIED.
      li_stream_factory = mi_ixml->create_stream_factory( ).
      li_istream = li_stream_factory->create_istream_string( iv_xml ).
      li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                          istream        = li_istream
                                          document       = mi_xml_doc ).
      IF li_parser->parse( ) <> 0.
        error( li_parser ).
      ENDIF.

      li_istream->close( ).

      mi_root = mi_xml_doc->get_first_child( ).
      mi_iterator = mi_root->get_children( )->create_iterator( ).
    ELSE.
      mi_root = mi_xml_doc->create_element( 'VARIANT' ).
      mi_xml_doc->append_child( mi_root ).
    ENDIF.

  ENDMETHOD.                    "constructor

  METHOD error.

    DATA: lv_error TYPE i,
          lv_txt1  TYPE string,
          lv_txt2  TYPE string,
          lv_txt3  TYPE string,
          lv_times TYPE i,
          li_error TYPE REF TO if_ixml_parse_error.


    IF ii_parser->num_errors( ) <> 0.
      lv_times = ii_parser->num_errors( ).
      DO lv_times TIMES.
        lv_error = sy-index - 1.
        li_error = ii_parser->get_error( lv_error ).

        lv_txt1 = li_error->get_column( ).
        CONCATENATE 'Column:' lv_txt1 INTO lv_txt1.         "#EC NOTEXT
        lv_txt2 = li_error->get_line( ).
        CONCATENATE 'Line:' lv_txt2 INTO lv_txt2.           "#EC NOTEXT
        lv_txt3 = li_error->get_reason( ).

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Error from XML parser'                 "#EC NOTEXT
            txt1  = lv_txt1
            txt2  = lv_txt2
            txt3  = lv_txt3.
      ENDDO.
    ENDIF.

  ENDMETHOD.                    "error

  METHOD write_variant.

    DATA: lv_name   TYPE string,
          lv_string TYPE string,
          li_test   TYPE REF TO if_ixml_element,
          li_text   TYPE REF TO if_ixml_text,
          li_vers   TYPE REF TO if_ixml_element,
          li_attr   TYPE REF TO if_ixml_element.


    lv_name = iv_testname.
    li_test = mi_xml_doc->create_element( lv_name ).
    mi_root->append_child( li_test ).

    li_vers = mi_xml_doc->create_element( 'VERSION' ).
    lv_string = iv_version.
    li_text = mi_xml_doc->create_text( lv_string ).
    li_vers->append_child( li_text ).
    li_test->append_child( li_vers ).

    li_attr = mi_xml_doc->create_element( 'ATTRIBUTES' ).
    li_test->append_child( li_attr ).

    IF ig_data IS SUPPLIED.
      structure_export( ig_data   = ig_data
                        ii_parent = li_attr ).
    ENDIF.

  ENDMETHOD.                    "write_variant

  METHOD read_variant.

    DATA: li_child TYPE REF TO if_ixml_node.


    CLEAR ev_testname.
    CLEAR ei_attributes.
    CLEAR ev_version.

    li_child = mi_iterator->get_next( ).
    IF li_child IS INITIAL.
      RETURN.
    ENDIF.

    ev_testname = li_child->get_name( ).
    ev_version = find_node( ii_node = li_child
                            iv_name = 'VERSION' )->get_value( ).
    ei_attributes = find_node( ii_node = li_child
                                iv_name = 'ATTRIBUTES' ).

  ENDMETHOD.                    "read_variant

  METHOD find_node.

    DATA: li_node     TYPE REF TO if_ixml_node,
          li_iterator TYPE REF TO if_ixml_node_iterator.


    li_iterator = ii_node->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      IF li_node->get_type( ) = if_ixml_node=>co_node_element
          AND li_node->get_name( ) = iv_name.
        ri_node = li_node.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.                    "find_node

  METHOD read_attributes.

    structure_import(
      EXPORTING
        ii_parent = ii_attributes
      CHANGING
        cg_data   = cg_data ).

  ENDMETHOD.                    "read_attributes

  METHOD render.

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( ).
    li_renderer->render( ).

* this is the wrong way to do it, but it makes it easier to debug
* when its possible to see the XML as text in the debugger
    REPLACE FIRST OCCURRENCE
      OF '<?xml version="1.0" encoding="utf-16"?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.

  ENDMETHOD.                    "render

  METHOD table_export.

    DATA: lv_kind        TYPE abap_typecategory,
          li_parent      TYPE REF TO if_ixml_element,
          lo_data_descr  TYPE REF TO cl_abap_datadescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( it_data ).
    lo_data_descr = lo_table_descr->get_table_line_type( ).
    lv_kind = lo_data_descr->kind.

    LOOP AT it_data ASSIGNING <lg_line>.
      li_parent = mi_xml_doc->create_element( 'ROW' ).
      ii_parent->append_child( li_parent ).

      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_export( it_data   = <lg_line>
                        ii_parent = li_parent ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_export( ig_data   = <lg_line>
                            ii_parent = li_parent ).
        WHEN cl_abap_typedescr=>kind_elem.
          simple_export( iv_data   = <lg_line>
                         ii_parent = li_parent ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "table_export

  METHOD table_import.

    DATA: lv_kind        TYPE abap_typecategory,
          li_node        TYPE REF TO if_ixml_node,
          lo_data_descr  TYPE REF TO cl_abap_datadescr,
          li_children    TYPE REF TO if_ixml_node_list,
          li_iterator    TYPE REF TO if_ixml_node_iterator,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    CLEAR ct_data.

    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( ct_data ).
    lo_data_descr = lo_table_descr->get_table_line_type( ).
    lv_kind = lo_data_descr->kind.

    li_children = ii_parent->get_children( ).
    IF li_children IS INITIAL.
      RETURN.
    ENDIF.
    li_iterator = li_children->create_iterator( ).
    li_node = li_iterator->get_next( ).

    WHILE NOT li_node IS INITIAL.
      APPEND INITIAL LINE TO ct_data ASSIGNING <lg_line>.

      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_import( EXPORTING ii_parent = li_node
                        CHANGING ct_data   = <lg_line> ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_import( EXPORTING ii_parent = li_node
                            CHANGING cg_data   = <lg_line> ).
        WHEN cl_abap_typedescr=>kind_elem.
          simple_import( EXPORTING ii_parent = li_node
                         CHANGING cv_data   = <lg_line> ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.                    "table_import

  METHOD simple_export.

    DATA: lv_string TYPE string,
          li_text   TYPE REF TO if_ixml_text.


    lv_string = iv_data.
    li_text = mi_xml_doc->create_text( lv_string ).
    ii_parent->append_child( li_text ).

  ENDMETHOD.                    "simple_export

  METHOD simple_import.

    DATA: lv_string TYPE string.


    IF ii_parent IS BOUND.
      lv_string = ii_parent->get_value( ).
      cv_data = lv_string.
    ELSE.
      CLEAR cv_data.
    ENDIF.

  ENDMETHOD.                    "simple_import

  METHOD structure_export.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          li_parent      TYPE REF TO if_ixml_element,
          lv_name        TYPE string,
          lo_typedescr   TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lo_structdescr->components,
                   <lg_any>  TYPE any.


    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( ig_data ).

    LOOP AT lo_structdescr->components ASSIGNING <ls_comp>.
      lv_name = <ls_comp>-name.
      li_parent = mi_xml_doc->create_element( lv_name ).
      ii_parent->append_child( li_parent ).
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ig_data TO <lg_any>.
      ASSERT sy-subrc = 0.

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <lg_any> ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_export( it_data   = <lg_any>
                        ii_parent = li_parent ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_export( ig_data   = <lg_any>
                            ii_parent = li_parent ).
        WHEN cl_abap_typedescr=>kind_elem.
          simple_export( iv_data   = <lg_any>
                         ii_parent = li_parent ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "structure_export

  METHOD structure_import.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          li_parent      TYPE REF TO if_ixml_node,
          lv_name        TYPE string,
          lo_typedescr   TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lo_structdescr->components,
                   <lg_any>  TYPE any.


    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( cg_data ).

    LOOP AT lo_structdescr->components ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE cg_data TO <lg_any>.
      ASSERT sy-subrc = 0.

      lv_name = <ls_comp>-name.
      li_parent = find_node( ii_node = ii_parent
                             iv_name = lv_name ).
      IF li_parent IS INITIAL.
        CONTINUE.
      ENDIF.

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <lg_any> ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_import( EXPORTING ii_parent = li_parent
                        CHANGING ct_data    = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_import( EXPORTING ii_parent = li_parent
                            CHANGING cg_data    = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_elem.
          simple_import( EXPORTING ii_parent = li_parent
                         CHANGING cv_data    = <lg_any> ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "structure_import

ENDCLASS.                    "lcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_file DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      download
        IMPORTING iv_xml TYPE string
        RAISING   cx_bcs lcx_exception,
      upload
        RETURNING value(rv_xml) TYPE string
        RAISING   cx_bcs lcx_exception.

ENDCLASS.                    "lcl_file DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file IMPLEMENTATION.

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
      _raise 'error from file_open_dialog'.
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
      _raise 'error from gui_upload'.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xml.

  ENDMETHOD.                    "upload

  METHOD download.

    DATA: lt_rawdata    TYPE solix_tab,
          lv_action     TYPE i,
          lv_so_obj_len TYPE so_obj_len,
          lv_size       TYPE i,
          lv_filename   TYPE string,
          lv_default    TYPE string,
          lv_path       TYPE string,
          lv_fullpath   TYPE string.


    lv_default = p_name.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension    = 'xml'
        default_file_name    = lv_default
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
      _raise 'error from file_save_dialog'.
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
      _raise 'error from gui_download'.
    ENDIF.

  ENDMETHOD.                    "download

ENDCLASS.                    "lcl_file IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_variant DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_variant DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      version
        IMPORTING iv_testname       TYPE sci_tstval-testname
        RETURNING value(rv_version) TYPE sci_tstval-version
        RAISING   lcx_test_not_found,
      has_attributes
        IMPORTING iv_testname              TYPE sci_tstval-testname
        RETURNING value(rv_has_attributes) TYPE sychar01
        RAISING   lcx_test_not_found,
      read
        IMPORTING iv_create         TYPE abap_bool DEFAULT abap_false
        RETURNING value(ro_variant) TYPE REF TO cl_ci_checkvariant
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      create_object
        IMPORTING iv_testname   TYPE sci_tstval-testname
        RETURNING value(ro_obj) TYPE REF TO object
        RAISING   lcx_test_not_found.

ENDCLASS.                    "lcl_variant DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_variant IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_variant IMPLEMENTATION.

  METHOD create_object.

    TRY.
        CREATE OBJECT ro_obj TYPE (iv_testname).
      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE lcx_test_not_found.
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

  METHOD version.

    DATA: lo_obj TYPE REF TO object.

    FIELD-SYMBOLS: <lv_version> TYPE sci_vers.


    lo_obj = create_object( iv_testname ).

    ASSIGN lo_obj->('VERSION') TO <lv_version>.
    ASSERT sy-subrc = 0.

    rv_version = <lv_version>.

  ENDMETHOD.                    "version

  METHOD read.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user            = p_user
        p_name            = p_name
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
          p_user              = p_user
          p_name              = p_name
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
      _raise 'error reading/creating variant'.
    ENDIF.

  ENDMETHOD.                    "read

ENDCLASS.                    "lcl_variant IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_updownci DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_updownci DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      download
        RAISING cx_static_check,
      upload
        RAISING cx_static_check,
      initialize.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_parameter,
        name  TYPE string,
        value TYPE string,
      END OF ty_parameter.

    TYPES: ty_parameter_tt TYPE STANDARD TABLE OF ty_parameter WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_type,
        parameter TYPE string,
        name      TYPE string,
        type      TYPE string,
        field     TYPE string,
      END OF ty_type.

    TYPES: ty_type_tt TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY.

    CLASS-DATA:
      go_xml TYPE REF TO lcl_xml.

    CLASS-METHODS:
      download_attributes
        IMPORTING iv_class      TYPE seoclsname
                  iv_attributes TYPE xstring
                  iv_version    TYPE sci_tstval-version
        RAISING   cx_static_check,
      build_memory
        IMPORTING iv_class  TYPE seoclsname
        EXPORTING er_data   TYPE REF TO data
                  et_memory TYPE ty_parameter_tt
        RAISING   cx_static_check,
      upload_attributes
        IMPORTING
                  iv_testname   TYPE sci_tstval-testname
                  ii_attributes TYPE REF TO if_ixml_node
        CHANGING
                  cv_attributes TYPE xstring
        RAISING   cx_static_check,
      update_variant
        IMPORTING
                  iv_testname   TYPE sci_tstval-testname
                  ii_attributes TYPE REF TO if_ixml_node
        CHANGING
                  ct_variant    TYPE sci_tstvar
        RAISING   cx_static_check,
      read_source
        IMPORTING iv_include       TYPE program
        RETURNING value(rt_source) TYPE abaptxt255_tab,
      parse
        IMPORTING it_source            TYPE abaptxt255_tab
        RETURNING value(rt_parameters) TYPE ty_parameter_tt
        RAISING   lcx_exception,
      find_types
        IMPORTING iv_class        TYPE seoclsname
                  it_parameters   TYPE ty_parameter_tt
        RETURNING value(rt_types) TYPE ty_type_tt
        RAISING   cx_static_check,
      show_progress
        IMPORTING iv_current TYPE i
                  iv_total   TYPE i
                  iv_class   TYPE seoclsname,
      create_structure
        IMPORTING it_types       TYPE ty_type_tt
        RETURNING value(rr_data) TYPE REF TO data
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_updownci DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_updownci IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_updownci IMPLEMENTATION.

  METHOD upload_attributes.

    DATA: lt_export     TYPE ty_parameter_tt,
          lv_attributes TYPE xstring,
          lr_data       TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>     TYPE data,
                   <lg_data_old> TYPE data.


    build_memory( EXPORTING iv_class  = iv_testname
                  IMPORTING er_data   = lr_data
                            et_memory = lt_export ).
    ASSIGN lr_data->* TO <lg_data>.
    ASSERT sy-subrc = 0.

    build_memory( EXPORTING iv_class  = iv_testname
                  IMPORTING er_data   = lr_data ).
    ASSIGN lr_data->* TO <lg_data_old>.
    ASSERT sy-subrc = 0.

    IF NOT cv_attributes IS INITIAL.
      IMPORT (lt_export) FROM DATA BUFFER cv_attributes.
      IF sy-subrc <> 0.
        _raise 'IMPORT error'.
      ENDIF.
      <lg_data_old> = <lg_data>.
    ENDIF.

    CLEAR <lg_data>.
    go_xml->read_attributes(
      EXPORTING
        ii_attributes = ii_attributes
      CHANGING
        cg_data       = <lg_data> ).

    EXPORT (lt_export) TO DATA BUFFER lv_attributes.
    IF <lg_data> <> <lg_data_old>.
      WRITE: / 'Attributes mismatch' COLOR 6.               "#EC NOTEXT
      cv_attributes = lv_attributes.
    ELSE.
      WRITE: / 'Attributes match'.                          "#EC NOTEXT
    ENDIF.

  ENDMETHOD.                    "upload_attributes

  METHOD initialize.

    FIELD-SYMBOLS: <ls_class> LIKE LINE OF s_class.


    APPEND INITIAL LINE TO s_class ASSIGNING <ls_class>.
    <ls_class>-sign   = 'I'.
    <ls_class>-option = 'CP'.
    <ls_class>-low    = 'CL_*'.

    APPEND INITIAL LINE TO s_class ASSIGNING <ls_class>.
    <ls_class>-sign   = 'I'.
    <ls_class>-option = 'CP'.
    <ls_class>-low    = 'ZCL_AOC_*'.

  ENDMETHOD.                    "initialize

  METHOD run.

    DATA: lx_static    TYPE REF TO cx_static_check,
          lx_exception TYPE REF TO lcx_exception.


    TRY.
        IF p_down = abap_true.
          lcl_updownci=>download( ).
        ELSE.
          lcl_updownci=>upload( ).
        ENDIF.
      CATCH lcx_exception INTO lx_exception.
        MESSAGE lx_exception->mv_text TYPE 'E'.
      CATCH cx_static_check INTO lx_static.
        MESSAGE lx_static TYPE 'E'.
    ENDTRY.

  ENDMETHOD.                    "run

  METHOD download.

    DATA: lv_count   TYPE i,
          lv_xml     TYPE string,
          lo_variant TYPE REF TO cl_ci_checkvariant.

    FIELD-SYMBOLS: <ls_variant> LIKE LINE OF lo_variant->variant.


    lo_variant = lcl_variant=>read( ).

    CREATE OBJECT go_xml.

    LOOP AT lo_variant->variant ASSIGNING <ls_variant>
        WHERE testname IN s_class.                      "#EC CI_HASHSEQ

      lv_count = lv_count + 1.
      show_progress( iv_current = lv_count
                     iv_total   = lines( lo_variant->variant )
                     iv_class   = <ls_variant>-testname ).

      download_attributes( iv_class      = <ls_variant>-testname
                           iv_attributes = <ls_variant>-attributes
                           iv_version    = <ls_variant>-version ).

    ENDLOOP.

    lv_xml = go_xml->render( ).
    lcl_file=>download( lv_xml ).

  ENDMETHOD.                    "download

  METHOD upload.

    DATA: lt_variant    TYPE sci_tstvar,
          lv_xml        TYPE string,
          lo_variant    TYPE REF TO cl_ci_checkvariant,
          li_attributes TYPE REF TO if_ixml_node,
          lv_testname   TYPE sci_tstval-testname,
          lv_version    TYPE sci_tstval-version.


    lv_xml = lcl_file=>upload( ).
    IF lv_xml IS INITIAL.
      RETURN.
    ENDIF.

    lo_variant = lcl_variant=>read( abap_true ).
    lo_variant->enter_change( ).
    lt_variant = lo_variant->variant.

    CREATE OBJECT go_xml
      EXPORTING
        iv_xml = lv_xml.

    go_xml->read_variant(
      IMPORTING
        ei_attributes = li_attributes
        ev_testname   = lv_testname
        ev_version    = lv_version ).
    WHILE NOT lv_testname IS INITIAL.
      show_progress( iv_current = 1
                     iv_total   = 100
                     iv_class   = lv_testname ).

      WRITE: / lv_testname, lv_version.
      TRY.
          IF lcl_variant=>version( lv_testname ) <> lv_version.
            WRITE: / 'Version mismatch' COLOR 6.            "#EC NOTEXT
          ELSE.
            WRITE: / 'Version match'.                       "#EC NOTEXT

            update_variant(
              EXPORTING
                iv_testname   = lv_testname
                ii_attributes = li_attributes
              CHANGING
                ct_variant    = lt_variant ).
          ENDIF.
        CATCH lcx_test_not_found.
          WRITE: / 'Test not found, skip'.                  "#EC NOTEXT
      ENDTRY.
      WRITE: /.

      go_xml->read_variant(
        IMPORTING
          ei_attributes = li_attributes
          ev_testname   = lv_testname
          ev_version    = lv_version ).
    ENDWHILE.

    IF p_test = abap_false.
      lo_variant->save( lt_variant ).
    ELSE.
      lo_variant->leave_change( ).
    ENDIF.

  ENDMETHOD.                    "upload

  METHOD update_variant.

    DATA: ls_variant LIKE LINE OF ct_variant.

    FIELD-SYMBOLS: <ls_variant>  LIKE LINE OF ct_variant.


    READ TABLE ct_variant ASSIGNING <ls_variant> WITH KEY testname = iv_testname.
    IF sy-subrc = 0.
      WRITE: / 'Found'.                                     "#EC NOTEXT
    ELSE.
      WRITE: / 'Not found, creating' COLOR 6.               "#EC NOTEXT

      CLEAR ls_variant.
      ls_variant-testname = iv_testname.
      ls_variant-version = lcl_variant=>version( iv_testname ).
      INSERT ls_variant INTO TABLE ct_variant.

      READ TABLE ct_variant ASSIGNING <ls_variant> WITH KEY testname = iv_testname.
      ASSERT sy-subrc = 0.
    ENDIF.

    IF lcl_variant=>has_attributes( iv_testname ) = abap_true.
      upload_attributes(
        EXPORTING
          iv_testname   = iv_testname
          ii_attributes = ii_attributes
        CHANGING
          cv_attributes = <ls_variant>-attributes ).
    ELSE.
      WRITE: / 'No attributes'.                             "#EC NOTEXT
    ENDIF.

  ENDMETHOD.                    "update_variant

  METHOD build_memory.

    DATA: lv_include    TYPE program,
          lt_parameters TYPE ty_parameter_tt,
          lt_types      TYPE ty_type_tt,
          lt_source     TYPE abaptxt255_tab.

    FIELD-SYMBOLS: <ls_type>   LIKE LINE OF lt_types,
                   <ls_memory> LIKE LINE OF et_memory.

    CLEAR et_memory.

    lv_include    = lcl_class=>find_include( iv_class ).
    lt_source     = read_source( lv_include ).
    lt_parameters = parse( lt_source ).
    lt_types      = find_types( iv_class      = iv_class
                                it_parameters = lt_parameters ).

    er_data = create_structure( lt_types ).

    LOOP AT lt_types ASSIGNING <ls_type>.
      APPEND INITIAL LINE TO et_memory ASSIGNING <ls_memory>.
      <ls_memory>-name = <ls_type>-parameter.
      IF <ls_type>-field IS INITIAL.
        CONCATENATE '<LG_DATA>' <ls_type>-name
          INTO <ls_memory>-value SEPARATED BY '-'.
      ELSE.
        CONCATENATE '<LG_DATA>' <ls_type>-name <ls_type>-field
          INTO <ls_memory>-value SEPARATED BY '-'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "build_memory

  METHOD download_attributes.

    DATA: lt_import TYPE ty_parameter_tt,
          lr_data   TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data> TYPE data.


    IF iv_attributes IS INITIAL.
      go_xml->write_variant( iv_testname = iv_class
                             iv_version  = iv_version ).
    ELSE.
      build_memory(
        EXPORTING
          iv_class  = iv_class
        IMPORTING
          er_data   = lr_data
          et_memory = lt_import ).
      ASSIGN lr_data->* TO <lg_data>.

      IMPORT (lt_import) FROM DATA BUFFER iv_attributes.
      IF sy-subrc <> 0.
        _raise 'IMPORT error'.
      ENDIF.

      go_xml->write_variant( iv_testname = iv_class
                             iv_version  = iv_version
                             ig_data     = <lg_data> ).
    ENDIF.

  ENDMETHOD.                    "download_attributes

  METHOD create_structure.

    DATA: lt_components  TYPE cl_abap_structdescr=>component_table,
          lo_structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components,
                   <ls_type>      LIKE LINE OF it_types.


    IF it_types IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_types ASSIGNING <ls_type>.
      APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
      <ls_component>-name = <ls_type>-name.
      <ls_component>-type ?= cl_abap_typedescr=>describe_by_name( <ls_type>-type ).
    ENDLOOP.

    SORT lt_components BY name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_components COMPARING name.

    TRY.
        lo_structdescr = cl_abap_structdescr=>create( lt_components ).
        CREATE DATA rr_data TYPE HANDLE lo_structdescr.
      CATCH cx_sy_struct_comp_name.
        _raise 'error creating structure'.
    ENDTRY.

  ENDMETHOD.                    "create_structure

  METHOD read_source.

    DATA: lv_source LIKE LINE OF rt_source.


    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = iv_include
        with_lowercase   = abap_true
      TABLES
        source_extended  = rt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.                             "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    LOOP AT rt_source INTO lv_source.
      IF lv_source(1) = '*'.
        DELETE rt_source INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "read_source

  METHOD parse.

    DATA: lv_offset TYPE i,
          lt_split  TYPE TABLE OF string,
          lv_source TYPE string.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF rt_parameters.


    CONCATENATE LINES OF it_source INTO lv_source.

    FIND FIRST OCCURRENCE OF 'import' IN lv_source
      IGNORING CASE MATCH OFFSET lv_offset.                 "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise '"import" not found'.
    ENDIF.
    lv_offset = lv_offset + 6.
    lv_source = lv_source+lv_offset.

    FIND FIRST OCCURRENCE OF 'from data buffer p_attributes' IN lv_source
      IGNORING CASE MATCH OFFSET lv_offset.                 "#EC NOTEXT
    IF sy-subrc <> 0.
      FIND FIRST OCCURRENCE OF 'from data buffer  p_attributes' IN lv_source
        IGNORING CASE MATCH OFFSET lv_offset.               "#EC NOTEXT
    ENDIF.
    IF sy-subrc <> 0.
      _raise '"from data buffer" not found'.
    ENDIF.
    lv_source = lv_source(lv_offset).

    SPLIT lv_source AT space INTO TABLE lt_split.
    LOOP AT lt_split INTO lv_source.
      IF lv_source IS INITIAL OR lv_source = '='.
        CONTINUE.
      ENDIF.

      IF NOT <ls_parameter> IS ASSIGNED.
        APPEND INITIAL LINE TO rt_parameters ASSIGNING <ls_parameter>.
      ENDIF.
      IF <ls_parameter>-name IS INITIAL.
        <ls_parameter>-name = lv_source.
        TRANSLATE <ls_parameter>-name TO UPPER CASE.
      ELSE.
        <ls_parameter>-value = lv_source.
        TRANSLATE <ls_parameter>-value TO UPPER CASE.
        UNASSIGN <ls_parameter>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "parse

  METHOD find_types.

    DATA: lv_name  TYPE seocmpname,
          lv_field TYPE string,
          lv_type  TYPE string,
          lt_attr  TYPE seo_attributes.

    FIELD-SYMBOLS: <ls_type>      LIKE LINE OF rt_types,
                   <ls_attr>      LIKE LINE OF lt_attr,
                   <ls_parameter> LIKE LINE OF it_parameters.


    lt_attr = lcl_class=>attributes( iv_class ).

    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      IF <ls_parameter>-value CA '-'.
        SPLIT <ls_parameter>-value AT '-' INTO lv_name lv_field.
      ELSE.
        lv_name = <ls_parameter>-value.
        CLEAR lv_field.
      ENDIF.

      READ TABLE lt_attr ASSIGNING <ls_attr> WITH KEY cmpname = lv_name.
      IF sy-subrc = 0.
        lv_type = <ls_attr>-type.
      ENDIF.

      IF lv_type IS INITIAL.
        IF iv_class = 'CL_WDY_CI_TEST_CONVENTIONS'.
          lv_type = 'TY_WDY_CI_TEST_CONVENTIONS'.
        ELSE.
          lv_type = 'ABAP_BOOL'.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO rt_types ASSIGNING <ls_type>.
      <ls_type>-parameter = <ls_parameter>-name.
      <ls_type>-name = lv_name.
      <ls_type>-type = lv_type.
      <ls_type>-field = lv_field.

    ENDLOOP.

  ENDMETHOD.                    "find_types

  METHOD show_progress.

    DATA: lv_f   TYPE f,
          lv_pct TYPE i.


    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_class.

  ENDMETHOD.                    "show_progress

ENDCLASS.                    "lcl_updownci IMPLEMENTATION

INITIALIZATION.
  lcl_updownci=>initialize( ).

START-OF-SELECTION.
  lcl_updownci=>run( ).