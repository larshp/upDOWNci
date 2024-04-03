" SPDX-License-Identifier: MIT
REPORT zupdownci_push_to_abapgit.

INCLUDE zabapgit_password_dialog.

" Required for password dialog:
TABLES sscrfields.

" TODO: Add UI texts
PARAMETERS p_repo TYPE string LOWER CASE.
PARAMETERS p_comm TYPE string DEFAULT `Update variants` LOWER CASE.


CLASS lcl_program DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_program.

    METHODS run.

  PRIVATE SECTION.
    METHODS get_repository
      RETURNING
        VALUE(ro_repository) TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception
        zcx_updownci_exception.

    METHODS build_comment
      RETURNING
        VALUE(rs_comment) TYPE zif_abapgit_git_definitions=>ty_comment.

    METHODS read_check_variants_from_db
      RETURNING
        VALUE(rt_check_variants) TYPE zcl_updownci_sync_to_abapgit=>gy_check_variants.
ENDCLASS.


CLASS lcl_program IMPLEMENTATION.
  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD run.
    DATA li_abapgit_log TYPE REF TO zif_abapgit_log.
    DATA lo_stage TYPE REF TO zcl_abapgit_stage.
    DATA lo_repository TYPE REF TO zcl_abapgit_repo_online.
    DATA lt_check_variants TYPE zcl_updownci_sync_to_abapgit=>gy_check_variants.
    DATA lo_abapgit_sync TYPE REF TO zcl_updownci_sync_to_abapgit.
    DATA ls_comment TYPE zif_abapgit_git_definitions=>ty_comment.
    DATA lo_abapgit_exception TYPE REF TO zcx_abapgit_exception.
    DATA lo_updownci_exception TYPE REF TO zcx_updownci_exception.

    CREATE OBJECT li_abapgit_log TYPE zcl_abapgit_log.
    CREATE OBJECT lo_stage.

    TRY.
        zcl_abapgit_background=>enqueue( ).

        lo_repository = get_repository( ).

        lt_check_variants = read_check_variants_from_db( ).

        CREATE OBJECT lo_abapgit_sync
          EXPORTING
            io_repository  = lo_repository
            iv_folder_name = `variant`
            ii_abapgit_log = li_abapgit_log.

        lo_abapgit_sync->stage_new_or_changed_variants( it_check_variants = lt_check_variants
                                                        io_stage          = lo_stage ).

        lo_abapgit_sync->stage_deletion_of_variants( it_check_variants = lt_check_variants
                                                     io_stage          = lo_stage ).

        IF lo_stage->count( ) > 0.
          ls_comment = build_comment( ).

          lo_repository->push( is_comment = ls_comment
                               io_stage   = lo_stage ).

          COMMIT WORK AND WAIT.

          li_abapgit_log->add_info( `Commit and push successful` ).
        ELSE.
          li_abapgit_log->add_info( `Nothing to commit` ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO lo_abapgit_exception.
        li_abapgit_log->add_exception( lo_abapgit_exception ).

      CATCH zcx_updownci_exception INTO lo_updownci_exception.
        li_abapgit_log->add_error( lo_updownci_exception->iv_text ).
    ENDTRY.

    zcl_abapgit_background=>dequeue( ).

    zcl_abapgit_log_viewer=>show_log( li_abapgit_log ).
  ENDMETHOD.

  METHOD read_check_variants_from_db.
    SELECT ciuser checkvname
      FROM scichkv_hd
      INTO TABLE rt_check_variants.
  ENDMETHOD.

  METHOD get_repository.
    DATA lt_repositories TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA lo_repository TYPE REF TO zif_abapgit_repo.
    lt_repositories = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repositories INTO lo_repository.
      IF lo_repository->is_offline( ) = abap_true.
        CONTINUE.
      ENDIF.

      IF lo_repository->get_name( ) = p_repo.
        ro_repository ?= lo_repository.
        RETURN.
      ENDIF.
    ENDLOOP.

    RAISE EXCEPTION TYPE zcx_updownci_exception
      EXPORTING
        iv_text = 'Requested online repository not found'.
  ENDMETHOD.

  METHOD build_comment.
    DATA lo_user_record TYPE REF TO zcl_abapgit_user_record.
    lo_user_record = zcl_abapgit_user_record=>get_instance( sy-uname ).

    rs_comment-comment = p_comm.
    rs_comment-committer-name  = lo_user_record->get_name( ).
    rs_comment-committer-email = lo_user_record->get_email( ).
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.

START-OF-SELECTION.
  lcl_program=>new( )->run( ).
