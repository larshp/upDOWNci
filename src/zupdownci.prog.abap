REPORT zupdownci.

*********************************************************************************
*
* Upload and download SAP Code Inspector variants in XML format
* see https://github.com/larshp/upDOWNci
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

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      initialize.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

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

    DATA: lv_xml       TYPE string,
          lx_static    TYPE REF TO cx_static_check,
          lx_exception TYPE REF TO zcx_updownci_exception,
          lv_default   TYPE string.


    lv_default = p_name.
    REPLACE ALL OCCURRENCES OF '/' IN lv_default WITH 'x'.

    TRY.
        IF p_down = abap_true.
          lv_xml = zcl_updownci=>build_xml(
            iv_name  = p_name
            iv_user  = p_user
            it_class = s_class[] ).

          zcl_updownci_file=>download_string(
            iv_default_file_name = lv_default
            iv_default_extension = 'xml'
            iv_content           = lv_xml ).
        ELSE.
          lv_xml = zcl_updownci_file=>upload( ).
          IF lv_xml IS INITIAL.
            RETURN.
          ENDIF.

          zcl_updownci=>create_from_xml(
            iv_xml  = lv_xml
            iv_name = p_name
            iv_user = p_user
            iv_test = p_test ).
        ENDIF.
      CATCH zcx_updownci_exception INTO lx_exception.
        MESSAGE lx_exception->iv_text TYPE 'E'.
      CATCH cx_static_check INTO lx_static.
        MESSAGE lx_static TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  lcl_app=>initialize( ).

START-OF-SELECTION.
  lcl_app=>run( ).
