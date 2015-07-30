REPORT zupdownci.

* see https://github.com/larshp/upDOWNci

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
            p_name TYPE sci_chkv DEFAULT 'DEFAULT'.

SELECT-OPTIONS: s_class FOR seoclass-clsname.

PARAMETERS: p_down TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_up TYPE c RADIOBUTTON GROUP g1.

CLASS lcl_class DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      find_include
        IMPORTING iv_class TYPE seoclsname
        RETURNING VALUE(rv_include) TYPE program
        RAISING cx_static_check,
      attributes
        IMPORTING iv_class TYPE seoclsname
        RETURNING VALUE(rt_attributes) TYPE seo_attributes
        RAISING cx_static_check.

ENDCLASS.

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
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.

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

  ENDMETHOD.

ENDCLASS.

CLASS lcl_xml DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      export
        IMPORTING ig_any TYPE any
        RETURNING VALUE(rv_xml) TYPE string,
      import
        IMPORTING iv_xml TYPE string
        CHANGING cv_any TYPE any.

ENDCLASS.

CLASS lcl_xml IMPLEMENTATION.

  METHOD export.

* todo
    BREAK-POINT.

  ENDMETHOD.

  METHOD import.

* todo
    BREAK-POINT.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_updownci DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        RAISING cx_static_check,
      initialize.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_parameter,
        name TYPE string,
        value TYPE string,
      END OF ty_parameter.

    TYPES: ty_parameter_tt TYPE STANDARD TABLE OF ty_parameter WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_type,
        parameter TYPE string,
        name TYPE string,
        type TYPE string,
        field TYPE string,
      END OF ty_type.

    TYPES: ty_type_tt TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY.

    CLASS-METHODS:
      handle
        IMPORTING iv_class TYPE seoclsname
                  iv_attributes TYPE xstring
        RAISING cx_static_check,
      read_source
        IMPORTING iv_include TYPE program
        RETURNING VALUE(rt_source) TYPE abaptxt255_tab,
      parse
        IMPORTING it_source TYPE abaptxt255_tab
        RETURNING VALUE(rt_parameters) TYPE ty_parameter_tt,
      find_types
        IMPORTING iv_class TYPE seoclsname
                  it_parameters TYPE ty_parameter_tt
        RETURNING VALUE(rt_types) TYPE ty_type_tt
        RAISING cx_static_check,
      show_progress
        IMPORTING iv_current TYPE i
                  iv_total TYPE i
                  iv_class TYPE seoclsname,
      create_structure
        IMPORTING it_types TYPE ty_type_tt
        RETURNING VALUE(rr_data) TYPE REF TO data.

ENDCLASS.

CLASS lcl_updownci IMPLEMENTATION.

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

  ENDMETHOD.

  METHOD run.

    DATA: lv_count   TYPE i,
          lo_variant TYPE REF TO cl_ci_checkvariant.

    FIELD-SYMBOLS: <ls_variant> LIKE LINE OF lo_variant->variant.


    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user            = p_user
        p_name            = p_name
      RECEIVING
        p_ref             = lo_variant
      EXCEPTIONS
        chkv_not_exists   = 1
        missing_parameter = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    lo_variant->get_info(
      EXCEPTIONS
        could_not_read_variant = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    LOOP AT lo_variant->variant ASSIGNING <ls_variant>
        WHERE testname IN s_class.

*    <ls_variant>-testname
*    <ls_variant>-attributes
*    <ls_variant>-version

      lv_count = lv_count + 1.
      show_progress( iv_current = lv_count
                     iv_total = lines( lo_variant->variant )
                     iv_class = <ls_variant>-testname ).

      IF NOT <ls_variant>-attributes IS INITIAL.
        handle( iv_class      = <ls_variant>-testname
                iv_attributes = <ls_variant>-attributes ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD handle.

    DATA: lv_include    TYPE program,
          lt_parameters TYPE ty_parameter_tt,
          lt_import TYPE ty_parameter_tt,
          lt_types      TYPE ty_type_tt,
          lr_data       TYPE REF TO data,
          lt_source     TYPE abaptxt255_tab.

    FIELD-SYMBOLS: <ls_type> LIKE LINE OF lt_types,
                   <ls_import> LIKE LINE OF lt_import,
                   <lg_any> TYPE any.


    lv_include    = lcl_class=>find_include( iv_class ).
    lt_source     = read_source( lv_include ).
    lt_parameters = parse( lt_source ).
    lt_types      = find_types( iv_class = iv_class
                                it_parameters = lt_parameters ).

    lr_data = create_structure( lt_types ).
    ASSIGN lr_data->* TO <lg_any>.

    LOOP AT lt_types ASSIGNING <ls_type>.
      APPEND INITIAL LINE TO lt_import ASSIGNING <ls_import>.
      <ls_import>-name = <ls_type>-parameter.
      CONCATENATE '<LG_ANY>' <ls_import>-name
        INTO <ls_import>-value SEPARATED BY '-'.
    ENDLOOP.

    IMPORT (lt_import) FROM DATA BUFFER iv_attributes.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.

  METHOD create_structure.

    DATA: lt_components  TYPE cl_abap_structdescr=>component_table,
          lr_structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components,
                   <ls_type> LIKE LINE OF it_types.


    LOOP AT it_types ASSIGNING <ls_type>.
      APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
      <ls_component>-name = <ls_type>-name.
      <ls_component>-type ?= cl_abap_typedescr=>describe_by_name( <ls_type>-type ).
    ENDLOOP.

    SORT lt_components BY name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_components COMPARING name.

    TRY.
        lr_structdescr = cl_abap_structdescr=>create( lt_components ).
        CREATE DATA rr_data TYPE HANDLE lr_structdescr.
      CATCH cx_sy_struct_comp_name.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD.

  METHOD read_source.

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
        OTHERS           = 4.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD parse.

    DATA: lv_offset TYPE i,
          lt_split  TYPE TABLE OF string,
          lv_source TYPE string.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF rt_parameters.


    CONCATENATE LINES OF it_source INTO lv_source.

    FIND FIRST OCCURRENCE OF 'import' IN lv_source
      IGNORING CASE MATCH OFFSET lv_offset.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.
    lv_offset = lv_offset + 6.
    lv_source = lv_source+lv_offset.

    FIND FIRST OCCURRENCE OF 'from data buffer p_attributes' IN lv_source
      IGNORING CASE MATCH OFFSET lv_offset.
    IF sy-subrc <> 0.
      BREAK-POINT.
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

  ENDMETHOD.

  METHOD find_types.

    DATA: lv_name  TYPE seocmpname,
          lv_field TYPE string,
          lv_type  TYPE string,
          lt_attr  TYPE seo_attributes.

    FIELD-SYMBOLS: <ls_type> LIKE LINE OF rt_types,
                   <ls_attr> LIKE LINE OF lt_attr,
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
      IF sy-subrc <> 0 AND lv_name = 'TRANSPORT_CHECK'.
* special case
        lv_type = 'ABAP_BOOL'.
      ELSEIF sy-subrc <> 0.
        BREAK-POINT.
      ELSE.
        lv_type = <ls_attr>-type.
      ENDIF.
      IF lv_type IS INITIAL.
* special case
        lv_type = 'ABAP_BOOL'.
      ENDIF.

      APPEND INITIAL LINE TO rt_types ASSIGNING <ls_type>.
      <ls_type>-parameter = <ls_parameter>-name.
      <ls_type>-name = lv_name.
      <ls_type>-type = lv_type.
      <ls_type>-field = lv_field.

    ENDLOOP.

  ENDMETHOD.

  METHOD show_progress.

    DATA: lv_f TYPE f,
          lv_pct TYPE i.


    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_class.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  lcl_updownci=>initialize( ).

START-OF-SELECTION.
  lcl_updownci=>run( ).
