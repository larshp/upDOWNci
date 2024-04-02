CLASS zcl_updownci DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_class_range TYPE RANGE OF seoclsname .
    TYPES:
      BEGIN OF ty_wdy_ci_test_conventions ##needed, " used dynamically
        init_done       TYPE abap_bool,
        db_access       TYPE abap_bool,
        file_access     TYPE abap_bool,
        obsolete_screen TYPE abap_bool,
        program_flow    TYPE abap_bool,
        system_call     TYPE abap_bool,
        param_check     TYPE abap_bool,
        include_source  TYPE abap_bool,
        call_me         TYPE abap_bool,
      END OF ty_wdy_ci_test_conventions .
    TYPES:
      BEGIN OF ty_t_case ##needed, " used dynamically
        none      TYPE sychar01,
        lower     TYPE sychar01,
        upper     TYPE sychar01,
        key_lower TYPE sychar01,
        key_upper TYPE sychar01,
      END OF ty_t_case.
    CLASS-METHODS build_xml
      IMPORTING
        !iv_user      TYPE sci_user
        !iv_name      TYPE sci_chkv
        !it_class     TYPE ty_class_range OPTIONAL
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_updownci_exception.
    CLASS-METHODS create_from_xml
      IMPORTING
        !iv_xml  TYPE string
        !iv_user TYPE sci_user
        !iv_name TYPE sci_chkv
        !iv_test TYPE abap_bool
      RAISING
        zcx_updownci_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_parameter,
        name  TYPE string,
        value TYPE string,
      END OF ty_parameter .
    TYPES:
      ty_parameter_tt TYPE STANDARD TABLE OF ty_parameter WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_type,
        parameter TYPE string,
        name      TYPE string,
        type      TYPE string,
        field     TYPE string,
      END OF ty_type .
    TYPES:
      ty_type_tt TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY .

    CLASS-DATA go_xml TYPE REF TO zcl_updownci_xml .

    CLASS-METHODS download_attributes
      IMPORTING
        !iv_class      TYPE seoclsname
        !iv_attributes TYPE xstring
        !iv_version    TYPE sci_tstval-version
      RAISING
        zcx_updownci_exception.
    CLASS-METHODS build_memory
      IMPORTING
        !iv_class  TYPE seoclsname
      EXPORTING
        !er_data   TYPE REF TO data
        !et_memory TYPE ty_parameter_tt
      RAISING
        zcx_updownci_exception.
    CLASS-METHODS upload_attributes
      IMPORTING
        !iv_testname   TYPE sci_tstval-testname
        !ii_attributes TYPE REF TO if_ixml_node
      CHANGING
        !cv_attributes TYPE xstring
      RAISING
        zcx_updownci_exception.
    CLASS-METHODS update_variant
      IMPORTING
        !iv_testname   TYPE sci_tstval-testname
        !ii_attributes TYPE REF TO if_ixml_node
      CHANGING
        !ct_variant    TYPE sci_tstvar
      RAISING
        zcx_updownci_exception
        zcx_updownci_test_not_found.
    CLASS-METHODS read_source
      IMPORTING
        !iv_include      TYPE program
      RETURNING
        VALUE(rt_source) TYPE abaptxt255_tab .
    CLASS-METHODS parse
      IMPORTING
        !it_source           TYPE abaptxt255_tab
      RETURNING
        VALUE(rt_parameters) TYPE ty_parameter_tt
      RAISING
        zcx_updownci_exception .
    CLASS-METHODS find_types
      IMPORTING
        !iv_class       TYPE seoclsname
        !it_parameters  TYPE ty_parameter_tt
      RETURNING
        VALUE(rt_types) TYPE ty_type_tt
      RAISING
        zcx_updownci_exception .
    CLASS-METHODS show_progress
      IMPORTING
        !iv_current TYPE i
        !iv_total   TYPE i
        !iv_class   TYPE seoclsname .
    CLASS-METHODS create_structure
      IMPORTING
        !iv_class      TYPE seoclsname
        !it_types      TYPE ty_type_tt
      RETURNING
        VALUE(rr_data) TYPE REF TO data
      RAISING
        zcx_updownci_exception .
ENDCLASS.



CLASS ZCL_UPDOWNCI IMPLEMENTATION.


  METHOD build_memory.

    DATA: lv_include    TYPE program,
          lt_parameters TYPE ty_parameter_tt,
          lt_types      TYPE ty_type_tt,
          lt_source     TYPE abaptxt255_tab.

    FIELD-SYMBOLS: <ls_type>   LIKE LINE OF lt_types,
                   <ls_memory> LIKE LINE OF et_memory.

    CLEAR et_memory.

    lv_include    = zcl_updownci_class=>find_include( iv_class ).
    lt_source     = read_source( lv_include ).
    lt_parameters = parse( lt_source ).
    lt_types      = find_types( iv_class      = iv_class
                                it_parameters = lt_parameters ).

    er_data = create_structure( iv_class = iv_class
                                it_types = lt_types ).

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


  METHOD build_xml.

    DATA: lv_count   TYPE i,
          lo_variant TYPE REF TO cl_ci_checkvariant.

    FIELD-SYMBOLS: <ls_variant> LIKE LINE OF lo_variant->variant.


    lo_variant = zcl_updownci_variant=>read(
      iv_user = iv_user
      iv_name = iv_name ).

    CREATE OBJECT go_xml.

    LOOP AT lo_variant->variant ASSIGNING <ls_variant>
        WHERE testname IN it_class.                     "#EC CI_HASHSEQ

      lv_count = lv_count + 1.
      show_progress( iv_current = lv_count
                     iv_total   = lines( lo_variant->variant )
                     iv_class   = <ls_variant>-testname ).

      download_attributes( iv_class      = <ls_variant>-testname
                           iv_attributes = <ls_variant>-attributes
                           iv_version    = <ls_variant>-version ).

    ENDLOOP.

    rv_xml = go_xml->render( ).

  ENDMETHOD.


  METHOD create_from_xml.

    DATA: lt_variant    TYPE sci_tstvar,
          lo_variant    TYPE REF TO cl_ci_checkvariant,
          li_attributes TYPE REF TO if_ixml_node,
          lv_testname   TYPE sci_tstval-testname,
          lv_version    TYPE sci_tstval-version.


    lo_variant = zcl_updownci_variant=>read(
      iv_user   = iv_user
      iv_name   = iv_name
      iv_create = abap_true ).
    lo_variant->enter_change( ).
    lt_variant = lo_variant->variant.

    CREATE OBJECT go_xml
      EXPORTING
        iv_xml = iv_xml.

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
          IF zcl_updownci_variant=>version( lv_testname ) <> lv_version.
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
        CATCH zcx_updownci_test_not_found.
          WRITE: / 'Test not found, skip'.                  "#EC NOTEXT
      ENDTRY.
      WRITE: /.

      go_xml->read_variant(
        IMPORTING
          ei_attributes = li_attributes
          ev_testname   = lv_testname
          ev_version    = lv_version ).
    ENDWHILE.

    IF iv_test = abap_false.
      lo_variant->save( lt_variant ).
    ELSE.
      lo_variant->leave_change( ).
    ENDIF.

  ENDMETHOD.                    "upload


  METHOD create_structure.

    DATA: lt_components  TYPE cl_abap_structdescr=>component_table,
          lo_type        TYPE REF TO cl_abap_typedescr,
          lo_structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components,
                   <ls_type>      LIKE LINE OF it_types.


    IF it_types IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_types ASSIGNING <ls_type>.
      APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
      <ls_component>-name = <ls_type>-name.

      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = <ls_type>-type
        RECEIVING
          p_descr_ref    = lo_type
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2 ).
      <ls_component>-type ?= lo_type.
      IF sy-subrc <> 0.
        <ls_component>-type ?= cl_abap_typedescr=>describe_by_name( |{ iv_class }=>{ <ls_type>-type }| ).
      ENDIF.

    ENDLOOP.

    SORT lt_components BY name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_components COMPARING name.

    TRY.
        lo_structdescr = cl_abap_structdescr=>create( lt_components ).
        CREATE DATA rr_data TYPE HANDLE lo_structdescr.
      CATCH cx_sy_struct_comp_name.
        RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'error creating structure'.
    ENDTRY.

  ENDMETHOD.                    "create_structure


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
        RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'IMPORT error'.
      ENDIF.

      go_xml->write_variant( iv_testname = iv_class
                             iv_version  = iv_version
                             ig_data     = <lg_data> ).
    ENDIF.

  ENDMETHOD.


  METHOD find_types.

    DATA: lv_name  TYPE seocmpname,
          lv_field TYPE string,
          lv_type  TYPE string,
          lt_attr  TYPE seo_attributes.

    FIELD-SYMBOLS: <ls_type>      LIKE LINE OF rt_types,
                   <ls_attr>      LIKE LINE OF lt_attr,
                   <ls_parameter> LIKE LINE OF it_parameters.


    lt_attr = zcl_updownci_class=>attributes( iv_class ).

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

      IF lv_name = 'M_OPTION' AND lv_field = 'SCAN_WEB_DYNPRO'.
* special handling for CL_CI_TEST_ABAP_NAMING_NEW
        lv_type = 'ABAP_BOOL'.
      ENDIF.

      IF lv_name = 'CASE'.
* special handling for CL_CI_TEST_PRETTY_PRINT
        lv_type = 'TY_T_CASE'.
      ENDIF.

      IF iv_class(8) = 'Y_CHECK_'.
* Special handling for code pal for ABAP - https://github.com/SAP/code-pal-for-abap
        lv_type = 'Y_IF_CLEAN_CODE_MANAGER=>CHECK_CONFIGURATION'.
      ENDIF.

      APPEND INITIAL LINE TO rt_types ASSIGNING <ls_type>.
      <ls_type>-parameter = <ls_parameter>-name.
      <ls_type>-name = lv_name.
      <ls_type>-type = lv_type.
      <ls_type>-field = lv_field.

    ENDLOOP.

  ENDMETHOD.                    "find_types


  METHOD parse.

    DATA: lv_offset TYPE i,
          lt_split  TYPE TABLE OF string,
          lv_source TYPE string.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF rt_parameters.


    CONCATENATE LINES OF it_source INTO lv_source.

    FIND FIRST OCCURRENCE OF 'import' IN lv_source
      IGNORING CASE MATCH OFFSET lv_offset.                 "#EC NOTEXT
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = '"import" not found'.
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
      RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = '"from data buffer" not found'.
    ENDIF.
    lv_source = lv_source(lv_offset).

    SPLIT lv_source AT space INTO TABLE lt_split.
    LOOP AT lt_split INTO lv_source.
      IF lv_source IS INITIAL OR lv_source = '='.
        CONTINUE.
      ENDIF.

* special handling for CL_SAUNIT_LEGACY_CI_CHECK
      REPLACE FIRST OCCURRENCE OF 'me->' IN lv_source WITH ''.

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
      ls_variant-version = zcl_updownci_variant=>version( iv_testname ).
      INSERT ls_variant INTO TABLE ct_variant.

      READ TABLE ct_variant ASSIGNING <ls_variant> WITH KEY testname = iv_testname.
      ASSERT sy-subrc = 0.
    ENDIF.

    IF zcl_updownci_variant=>has_attributes( iv_testname ) = abap_true.
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
        RAISE EXCEPTION TYPE zcx_updownci_exception EXPORTING iv_text = 'IMPORT error'.
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
ENDCLASS.
