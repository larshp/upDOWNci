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

CLASS lcl_class DEFINITION.

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

CLASS lcl_updownci DEFINITION.

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
        name TYPE string,
        type TYPE string,
        field TYPE string,
      END OF ty_type.

    TYPES: ty_type_tt TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY.

    CLASS-METHODS:
      find_parameters
        IMPORTING iv_class TYPE seoclsname
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
                  iv_class TYPE seoclsname.

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

    DATA: lv_count TYPE i,
          lo_variant TYPE REF TO cl_ci_checkvariant.


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

    LOOP AT lo_variant->variant ASSIGNING FIELD-SYMBOL(<ls_variant>)
        WHERE testname IN s_class.

*    <ls_variant>-testname
*    <ls_variant>-attributes
*    <ls_variant>-version

      lv_count = lv_count + 1.
      show_progress( iv_current = lv_count
                     iv_total = lines( lo_variant->variant )
                     iv_class = <ls_variant>-testname ).

      IF NOT <ls_variant>-attributes IS INITIAL.
        find_parameters( <ls_variant>-testname ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD find_parameters.

    DATA: lv_include    TYPE program,
          lt_parameters TYPE ty_parameter_tt,
          lt_types      TYPE ty_type_tt,
          lt_source     TYPE abaptxt255_tab.


    lv_include    = lcl_class=>find_include( iv_class ).
    lt_source     = read_source( lv_include ).
    lt_parameters = parse( lt_source ).
    lt_types      = find_types( iv_class = iv_class
                                it_parameters = lt_parameters ).

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

    DATA: lv_name TYPE string,
          lv_field TYPE string,
          lt_attributes TYPE seo_attributes.

    FIELD-SYMBOLS: <ls_attr> LIKE LINE OF lt_attributes,
                   <ls_parameter> LIKE LINE OF it_parameters.


    lt_attributes = lcl_class=>attributes( iv_class ).

    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      IF <ls_parameter>-value CA '-'.
        SPLIT <ls_parameter>-value AT '-' INTO lv_name lv_field.
      ELSE.
        lv_name = <ls_parameter>-value.
        CLEAR lv_field.
      ENDIF.

      READ TABLE lt_attributes ASSIGNING <ls_attr> WITH KEY cmpname = lv_name.
      IF sy-subrc <> 0 AND lv_name = 'TRANSPORT_CHECK'.
* special case
      ELSEIF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.
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
