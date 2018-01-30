*&---------------------------------------------------------------------*
*& Include zabapgit_object_pdts
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_pdts DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_pdts DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_task,
             short_text                 TYPE hr_mcshort,
             plvar                      TYPE plvar,
             wi_text                    TYPE witext,
             method                     TYPE hrs1201,
             method_binding             TYPE hrsmtbind,
             starting_events            TYPE hrsevtab,
             starting_events_binding    TYPE hrsevbind,
             terminating_events         TYPE hrsetmtab,
             terminating_events_binding TYPE hrsevbind,
             descriptions               TYPE wstexts,
           END OF ty_task.

    CONSTANTS: co_subty_task_description TYPE hr_s_subty VALUE '0120',
               co_object_type_task       TYPE hr_sotype  VALUE 'TS'.

    DATA: mv_objid TYPE hrobject-objid.

ENDCLASS.                    "lcl_object_pdts DEFINITION

CLASS lcl_object_pdts_helper DEFINITION
                             INHERITING FROM cl_workflow_general_task_def.

  PUBLIC SECTION.
    CLASS-METHODS:
      set_objid
        IMPORTING
          iv_objid TYPE hrobject-objid
          io_task  TYPE REF TO cl_workflow_general_task_def,

      set_container_id
        IMPORTING
          iv_id   TYPE guid_32
          io_task TYPE REF TO cl_workflow_general_task_def.

ENDCLASS.

CLASS lcl_object_pdts_helper IMPLEMENTATION.

  METHOD set_objid.

    io_task->objid = iv_objid.

  ENDMETHOD.


  METHOD set_container_id.

    FIELD-SYMBOLS: <object> TYPE REF TO if_swf_cnt_container.

    ASSIGN ('IO_TASK->CONTAINER') TO <object>.
    ASSERT sy-subrc = 0.

    CALL METHOD <object>->('SET_GUID')
      EXPORTING
        guid_32 = iv_id.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_pdts IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_pdts IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_objid = ms_item-obj_name.

  ENDMETHOD.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~exists.

    DATA: lv_endda TYPE plog-endda.

    lv_endda = '99991231'.

    CALL FUNCTION 'RH_READ_OBJECT'
      EXPORTING
        plvar     = '01'
        otype     = co_object_type_task
        objid     = mv_objid
        istat     = '1'
        begda     = sy-datum
        endda     = lv_endda
        ointerval = 'X'
        read_db   = 'X'
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~serialize.

    DATA: ls_task          TYPE ty_task,
          lo_inst          TYPE REF TO cl_workflow_task_ts,
          lo_first_element TYPE REF TO if_ixml_element,
          lo_xml_dom       TYPE REF TO if_ixml_document.

    FIELD-SYMBOLS: <ls_description>             TYPE hrs1002,
                   <ls_method_binding>          LIKE LINE OF ls_task-method_binding,
                   <ls_starting_events_binding> TYPE hrs1212.

    cl_workflow_factory=>create_ts(
      EXPORTING
        objid                        = mv_objid
      RECEIVING
        ts_inst                      = lo_inst
      EXCEPTIONS
        standard_task_does_not_exist = 1
        object_could_not_be_locked   = 2
        objid_not_given              = 3
        OTHERS                       = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CREATE_TS { sy-subrc }| ).
    ENDIF.

    ls_task-wi_text                    = lo_inst->wi_text.
    ls_task-short_text                 = lo_inst->short_text.
    ls_task-plvar                      = lo_inst->plvar.
    ls_task-method                     = lo_inst->method.
    ls_task-method_binding             = lo_inst->method_binding.
    ls_task-starting_events            = lo_inst->starting_events.
    ls_task-starting_events_binding    = lo_inst->starting_events_binding.
    ls_task-terminating_events         = lo_inst->terminating_events.
    ls_task-terminating_events_binding = lo_inst->terminating_events_binding.
    ls_task-descriptions               = lo_inst->descriptions.

    lo_inst->container->to_xml(
      EXPORTING
        include_null_values        = abap_true
        include_initial_values     = abap_true
        include_typenames          = abap_true
        include_change_data        = abap_true
        include_texts              = abap_true
        include_extension_elements = abap_true
        save_delta_handling_info   = abap_true
        use_xslt                   = abap_false
      IMPORTING
        xml_dom                    = lo_xml_dom
      EXCEPTIONS
        conversion_error           = 1
        OTHERS                     = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CREATE_TS { sy-subrc }| ).
    ENDIF.

    CLEAR: ls_task-method-aedtm,
           ls_task-method-uname.

    LOOP AT ls_task-method_binding ASSIGNING <ls_method_binding>.

      CLEAR: <ls_method_binding>-aedtm,
             <ls_method_binding>-uname.

    ENDLOOP.

    LOOP AT ls_task-starting_events_binding ASSIGNING <ls_starting_events_binding>.

      CLEAR: <ls_starting_events_binding>-aedtm,
             <ls_starting_events_binding>-uname.

    ENDLOOP.

    LOOP AT ls_task-descriptions ASSIGNING <ls_description>.

      CLEAR: <ls_description>-aedtm,
             <ls_description>-uname.

    ENDLOOP.

    io_xml->add( iv_name = 'PDTS'
                 ig_data = ls_task ).

    lo_first_element ?= lo_xml_dom->get_first_child( ).

    io_xml->add_xml( iv_name = 'CONTAINER'
                     ii_xml  = lo_first_element ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: ls_task      TYPE ty_task,
          lv_message   TYPE string,
          ls_hrsobject TYPE hrsobject,
          lo_inst      TYPE REF TO cl_workflow_task_ts,
          lo_gen_task  TYPE REF TO cl_workflow_general_task_def.

    FIELD-SYMBOLS: <ls_method_binding> TYPE hrs1214.

    io_xml->read(
      EXPORTING
        iv_name = 'PDTS'
      CHANGING
        cg_data = ls_task ).

    cl_workflow_factory=>create_new_ts(
      EXPORTING
        short_text          = |{ ls_task-short_text }|
        text                = |{ ls_task-wi_text }|
      RECEIVING
        task_object         = lo_inst
      EXCEPTIONS
        text_exists_already = 1
        OTHERS              = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CREATE_NEW_TS { sy-subrc }| ).
    ENDIF.

    lo_gen_task ?= lo_inst.

    lcl_object_pdts_helper=>set_objid( iv_objid = mv_objid
                                       io_task  = lo_gen_task ).

    lcl_object_pdts_helper=>set_container_id( iv_id    = |{ co_object_type_task }{ mv_objid }|
                                              io_task  = lo_gen_task ).

    lo_inst->change_wi_text(
      EXPORTING
        new_wi_text        = ls_task-wi_text
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_WI_TEXT { sy-subrc }| ).
    ENDIF.

    lo_inst->change_method(
      EXPORTING
        new_method                   = ls_task-method    " New Method or Settings
      EXCEPTIONS
        no_changes_allowed           = 1
        problem_method_web_enabling  = 2
        problem_method_phon_enabling = 3
        OTHERS                       = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_METHOD { sy-subrc }| ).
    ENDIF.

    LOOP AT ls_task-method_binding ASSIGNING <ls_method_binding>.

      lo_inst->change_method_binding(
        EXPORTING
          binding                       = <ls_method_binding>
          delete                        = abap_false
          insert                        = abap_true
        EXCEPTIONS
          no_changes_allowed            = 1
          desired_action_not_clear      = 2
          ts_cnt_element_does_not_exist = 3
          binding_could_not_be_deleted  = 4
          OTHERS                        = 5 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |error from CHANGE_METHOD_BINDING { sy-subrc }| ).
      ENDIF.

    ENDLOOP.

    lo_inst->change_start_events_complete(
      EXPORTING
        starting_events    = ls_task-starting_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_START_EVENTS_COMPLETE { sy-subrc }| ).
    ENDIF.

    lo_inst->change_start_evt_bind_complete(
      EXPORTING
        new_bindings       = ls_task-starting_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_START_EVT_BIND_COMPLETE { sy-subrc }| ).
    ENDIF.

    lo_inst->change_term_events_complete(
      EXPORTING
        terminating_events = ls_task-terminating_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_TEXT { sy-subrc }| ).
    ENDIF.

    lo_inst->change_term_evt_bind_complete(
      EXPORTING
        new_bindings       = ls_task-terminating_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_TERM_EVENTS_COMPLETE { sy-subrc }| ).
    ENDIF.

    lo_inst->change_text(
      EXPORTING
        subty              = co_subty_task_description
        new_text           = ls_task-descriptions
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from CHANGE_TEXT { sy-subrc }| ).
    ENDIF.

    ls_hrsobject-otype = co_object_type_task.
    ls_hrsobject-objid = mv_objid.
    INSERT hrsobject FROM ls_hrsobject.

    lo_inst->save_standard_task(
      EXPORTING
        development_class          = iv_package
      EXCEPTIONS
        no_changes_allowed         = 1
        no_client_indep_maint      = 2
        update_error               = 3
        insert_error_new_ts        = 4
        new_ts_could_not_be_locked = 5
        save_abort_by_user         = 6
        OTHERS                     = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from SAVE_STANDARD_TASK { sy-subrc }| ).
    ENDIF.

    DATA: lo_xml_element TYPE REF TO if_ixml_element,
          xml_string     TYPE xstring.

    DATA(li_document) = io_xml->get_raw( ).

    DATA(lo_container_element) = li_document->find_from_name_ns( 'CONTAINER' ).

    IF lo_container_element IS BOUND.

      li_document = cl_ixml=>create( )->create_document( ).

      DATA(li_stream) = cl_ixml=>create( )->create_stream_factory( )->create_ostream_xstring( xml_string ).

      li_document->append_child( lo_container_element ).

      cl_ixml=>create( )->create_renderer(
          document = li_document
          ostream  = li_stream
      )->render( ).

      lo_inst->container->import_from_xml(
        EXPORTING
*          xml_dom        = li_document
          xml_stream     = xml_string
        IMPORTING
          exception_list = DATA(exception_list) ).

    ENDIF.

    tadir_insert( iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'RH_TASK_DELETE'
      EXPORTING
        act_otype           = co_object_type_task
        act_objid           = mv_objid
        act_plvar           = '01'
        act_istat           = '1'
      EXCEPTIONS
        no_active_plvar     = 1
        task_not_found      = 2
        task_not_deleted    = 3
        task_not_enqueued   = 4
        task_type_not_valid = 5
        OTHERS              = 6.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from RH_TASK_DELETE { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS_REMOTE'
      STARTING NEW TASK 'GIT'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_pdts IMPLEMENTATION
