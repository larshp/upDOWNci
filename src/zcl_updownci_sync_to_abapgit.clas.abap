"! <p class="shorttext synchronized">Synchronization to abapGit</p>
CLASS zcl_updownci_sync_to_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF gy_check_variant,
             ciuser     TYPE scichkv_hd-ciuser,
             checkvname TYPE scichkv_hd-checkvname,
           END OF gy_check_variant.
    TYPES gy_check_variants TYPE TABLE OF gy_check_variant WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        io_repository  TYPE REF TO zcl_abapgit_repo_online
        iv_folder_name TYPE string
        ii_abapgit_log TYPE REF TO zif_abapgit_log.

    METHODS stage_deletion_of_variants
      IMPORTING
        it_check_variants TYPE gy_check_variants
        io_stage          TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception.

    METHODS stage_new_or_changed_variants
      IMPORTING
        it_check_variants TYPE gy_check_variants
        io_stage          TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS gc_xml_file_extension TYPE string VALUE `.xml` ##NO_TEXT.
    CONSTANTS gc_slash_replacement TYPE char01 VALUE '#'.

    DATA mo_repository TYPE REF TO zcl_abapgit_repo_online.
    DATA mv_path TYPE string.
    DATA mi_abapgit_log TYPE REF TO zif_abapgit_log.

    METHODS build_xml_file
      IMPORTING
        is_check_variant TYPE gy_check_variant
      RETURNING
        VALUE(rs_file)   TYPE zif_abapgit_git_definitions=>ty_file
      RAISING
        zcx_updownci_exception.

    METHODS get_variant_for_file
      IMPORTING
        is_file                        TYPE zif_abapgit_git_definitions=>ty_file
      RETURNING
        VALUE(rs_check_variant_search) TYPE gy_check_variant.
ENDCLASS.


CLASS zcl_updownci_sync_to_abapgit IMPLEMENTATION.
  METHOD constructor.
    mo_repository = io_repository.
    mv_path = |/{ iv_folder_name }/|.
    mi_abapgit_log = ii_abapgit_log.
  ENDMETHOD.

  METHOD stage_new_or_changed_variants.
    DATA(lt_files) = mo_repository->get_files_remote( ).

    LOOP AT it_check_variants ASSIGNING FIELD-SYMBOL(<ls_check_variant>).
      TRY.
          DATA(ls_file) = build_xml_file( <ls_check_variant> ).
        CATCH zcx_updownci_exception INTO DATA(lo_build_xml_error).
          mi_abapgit_log->add_error( lo_build_xml_error->iv_text ).
          mi_abapgit_log->add_error( |Skipping { <ls_check_variant>-ciuser } { <ls_check_variant>-checkvname }| ).
          CONTINUE.
      ENDTRY.

      IF line_exists( lt_files[ KEY file_path
                                path     = ls_file-path
                                filename = ls_file-filename
                                data     = ls_file-data ] ).
        " The file in Git doesn't differ from the variant
        CONTINUE.
      ENDIF.

      io_stage->add( iv_path     = ls_file-path
                     iv_filename = ls_file-filename
                     iv_data     = ls_file-data ).
    ENDLOOP.
  ENDMETHOD.

  METHOD stage_deletion_of_variants.
    DATA(lt_files) = mo_repository->get_files_remote( ).

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>)
         WHERE path CP |{ mv_path }*|.
      DATA(ls_check_variant_search) = get_variant_for_file( <ls_file> ).

      IF NOT line_exists( it_check_variants[ table_line = ls_check_variant_search ] ).
        io_stage->rm( iv_path     = <ls_file>-path
                      iv_filename = <ls_file>-filename ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_xml_file.
    DATA(lv_xml) = zcl_updownci=>build_xml( iv_name = is_check_variant-checkvname
                                            iv_user = is_check_variant-ciuser ).

    TRY.
        rs_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml ).
      CATCH zcx_abapgit_exception INTO DATA(lo_abapgit_exception).
        RAISE EXCEPTION TYPE zcx_updownci_exception
          EXPORTING
            iv_text  = 'Error converting from string to xstring'
            previous = lo_abapgit_exception.
    ENDTRY.

    rs_file-path = mv_path.

    IF is_check_variant-ciuser IS NOT INITIAL.
      rs_file-path = |{ rs_file-path }{ is_check_variant-ciuser }/|.
    ENDIF.

    rs_file-filename = replace( val  = |{ is_check_variant-checkvname }{ gc_xml_file_extension }|
                                sub  = '/'
                                with = gc_slash_replacement
                                occ  = 0 ).
  ENDMETHOD.

  METHOD get_variant_for_file.
    DATA(lv_extension_length) = strlen( gc_xml_file_extension ).

    rs_check_variant_search = VALUE gy_check_variant( ).

    rs_check_variant_search-ciuser = substring_before( val = substring_after( val = is_file-path
                                                                              sub = mv_path )
                                                       sub = `/` ).

    DATA(lv_variant_name_length) = strlen( is_file-filename ) - lv_extension_length.

    rs_check_variant_search-checkvname = replace( val  = is_file-filename
                                                  sub  = gc_slash_replacement
                                                  with = '/'
                                                  occ  = 0 ).

    rs_check_variant_search-checkvname = substring( val = rs_check_variant_search-checkvname
                                                    len = lv_variant_name_length ).
  ENDMETHOD.
ENDCLASS.
