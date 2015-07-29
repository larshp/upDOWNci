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

PARAMETERS: p_user TYPE sci_user DEFAULT sy-uname,
            p_name TYPE sci_chkv DEFAULT 'DEFAULT'.

*select-options: class name

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lo_variant TYPE REF TO cl_ci_checkvariant.


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

  LOOP AT lo_variant->variant ASSIGNING FIELD-SYMBOL(<ls_variant>).

*    <ls_variant>-testname
*    <ls_variant>-attributes
*    <ls_variant>-version

    IF NOT <ls_variant>-attributes IS INITIAL.
      PERFORM find_parameters USING <ls_variant>-testname.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM find_parameters USING pv_class TYPE seoclsname.

  DATA: lv_include TYPE program.


  PERFORM get_include USING pv_class CHANGING lv_include.

  WRITE: / pv_class, lv_include.

ENDFORM.

FORM get_include USING pv_class TYPE seoclsname CHANGING cv_include TYPE program.

  DATA: lo_class TYPE REF TO cl_oo_class,
        lv_class TYPE seoclsname,
        ls_mtdkey TYPE seocpdkey.


  ls_mtdkey-clsname = pv_class.
  ls_mtdkey-cpdname = 'PUT_ATTRIBUTES'.

  cl_oo_classname_service=>get_method_include(
    EXPORTING
      mtdkey              = ls_mtdkey
    RECEIVING
      result              = cv_include
    EXCEPTIONS
      class_not_existing  = 1
      method_not_existing = 2
      OTHERS              = 3 ).
  IF sy-subrc = 2.
* try super class
    lo_class ?= cl_oo_class=>get_instance( pv_class ).
    lv_class = lo_class->get_superclass( ).
    PERFORM get_include USING lv_class CHANGING cv_include.
  ELSEIF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

ENDFORM.
