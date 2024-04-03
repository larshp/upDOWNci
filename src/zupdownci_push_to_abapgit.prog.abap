" SPDX-License-Identifier: MIT
REPORT zupdownci_push_to_abapgit.

INCLUDE zabapgit_password_dialog.

" Required for password dialog:
TABLES sscrfields.

PARAMETERS p_repo TYPE string LOWER CASE.
PARAMETERS p_comm TYPE string DEFAULT `Update variants` LOWER CASE.


CLASS lcl_program DEFINITION.

  PUBLIC SECTION.
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
  METHOD run.
    DATA li_abapgit_log TYPE REF TO zif_abapgit_log.
    li_abapgit_log = NEW zcl_abapgit_log( ).

    TRY.
        zcl_abapgit_background=>enqueue( ).

        DATA(lo_repository) = get_repository( ).

        DATA(lt_check_variants) = read_check_variants_from_db( ).

        DATA(lo_abapgit_sync) = NEW zcl_updownci_sync_to_abapgit( io_repository  = lo_repository
                                                                  iv_folder_name = `variant`
                                                                  ii_abapgit_log = li_abapgit_log ).

        DATA(lo_stage) = NEW zcl_abapgit_stage( ).

        lo_abapgit_sync->stage_new_or_changed_variants( it_check_variants = lt_check_variants
                                                      io_stage          = lo_stage ).

        lo_abapgit_sync->stage_deletion_of_variants( it_check_variants = lt_check_variants
                                                      io_stage          = lo_stage ).

        IF lo_stage->count( ) > 0.
          DATA(ls_comment) = build_comment( ).

          lo_repository->push( is_comment = ls_comment
                               io_stage   = lo_stage ).

          COMMIT WORK AND WAIT.

          li_abapgit_log->add_info( `Commit and push successful` ).
        ELSE.
          li_abapgit_log->add_info( `Nothing to commit` ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(lo_abapgit_exception).
        li_abapgit_log->add_exception( lo_abapgit_exception ).

      CATCH zcx_updownci_exception INTO DATA(lo_updownci_exception).
        li_abapgit_log->add_error( lo_updownci_exception->iv_text ).
    ENDTRY.

    zcl_abapgit_background=>dequeue( ).

    zcl_abapgit_log_viewer=>show_log( li_abapgit_log ).
  ENDMETHOD.

  METHOD read_check_variants_from_db.
    SELECT
      FROM scichkv_hd
      FIELDS ciuser, checkvname
      INTO TABLE @rt_check_variants.
  ENDMETHOD.

  METHOD get_repository.
    DATA(lt_repositories) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repositories INTO DATA(lo_repository).
      IF lo_repository->is_offline( ).
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
    DATA(lo_user_record) = zcl_abapgit_user_record=>get_instance( sy-uname ).

    rs_comment = VALUE #( comment   = p_comm
                          committer = VALUE #( name  = lo_user_record->get_name( )
                                               email = lo_user_record->get_email( ) ) ).
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
  NEW lcl_program( )->run( ).
