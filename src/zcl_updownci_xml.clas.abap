CLASS zcl_updownci_xml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_xml TYPE string OPTIONAL
        RAISING   zcx_updownci_exception,
      write_variant
        IMPORTING ig_data     TYPE data OPTIONAL
                  iv_testname TYPE sci_tstval-testname
                  iv_version  TYPE sci_tstval-version
        RAISING   zcx_updownci_exception,
      read_variant
        EXPORTING ei_attributes TYPE REF TO if_ixml_node
                  ev_testname   TYPE sci_tstval-testname
                  ev_version    TYPE sci_tstval-version
        RAISING   zcx_updownci_exception,
      read_attributes
        IMPORTING ii_attributes TYPE REF TO if_ixml_node
        CHANGING  cg_data       TYPE data
        RAISING   zcx_updownci_exception,
      render
        RETURNING VALUE(rv_xml) TYPE string.

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
        RETURNING VALUE(ri_node) TYPE REF TO if_ixml_node,
      structure_export
        IMPORTING ig_data   TYPE data
                  ii_parent TYPE REF TO if_ixml_element
        RAISING   zcx_updownci_exception,
      structure_import
        IMPORTING ii_parent TYPE REF TO if_ixml_node
        CHANGING  cg_data   TYPE data
        RAISING   zcx_updownci_exception,
      table_export
        IMPORTING it_data   TYPE STANDARD TABLE
                  ii_parent TYPE REF TO if_ixml_element
        RAISING   zcx_updownci_exception,
      table_import
        IMPORTING ii_parent TYPE REF TO if_ixml_node
        CHANGING  ct_data   TYPE STANDARD TABLE
        RAISING   zcx_updownci_exception,
      simple_export
        IMPORTING iv_data   TYPE simple
                  ii_parent TYPE REF TO if_ixml_element,
      simple_import
        IMPORTING ii_parent TYPE REF TO if_ixml_node
        CHANGING  cv_data   TYPE simple,
      error
        IMPORTING ii_parser TYPE REF TO if_ixml_parser.

ENDCLASS.



CLASS ZCL_UPDOWNCI_XML IMPLEMENTATION.


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
        RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'XML error'.
      ENDIF.

      li_istream->close( ).

      mi_root = mi_xml_doc->get_first_child( ).
      mi_iterator = mi_root->get_children( )->create_iterator( ).
    ELSE.
      mi_root = mi_xml_doc->create_element( 'VARIANT' ).
      mi_xml_doc->append_child( mi_root ).
    ENDIF.

  ENDMETHOD.


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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD read_attributes.

    structure_import(
      EXPORTING
        ii_parent = ii_attributes
      CHANGING
        cg_data   = cg_data ).

  ENDMETHOD.


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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD simple_export.

    DATA: lv_string TYPE string,
          li_text   TYPE REF TO if_ixml_text.


    lv_string = iv_data.
    li_text = mi_xml_doc->create_text( lv_string ).
    ii_parent->append_child( li_text ).

  ENDMETHOD.


  METHOD simple_import.

    DATA: lv_string TYPE string.


    IF ii_parent IS BOUND.
      lv_string = ii_parent->get_value( ).
      cv_data = lv_string.
    ELSE.
      CLEAR cv_data.
    ENDIF.

  ENDMETHOD.


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
          RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text ='unknown kind'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


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
          RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text ='unknown kind'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


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
          RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text ='unknown kind'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


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
          RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text ='unknown kind'.
      ENDCASE.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


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

  ENDMETHOD.
ENDCLASS.
