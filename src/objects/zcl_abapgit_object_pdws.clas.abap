CLASS zcl_abapgit_object_pdws DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .

    METHODS:
      constructor
        IMPORTING
          !is_item     TYPE zif_abapgit_definitions=>ty_item
          !iv_language TYPE spras.

  PRIVATE SECTION.
    DATA:
      mv_objid  TYPE hrobject-objid,
      mv_wfd_id TYPE swd_wfd_id.

ENDCLASS.


CLASS zcl_abapgit_object_pdws IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_objid = ms_item-obj_name.
    mv_wfd_id = |WS{ mv_objid }|.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'RH_TASK_DELETE'
      EXPORTING
        act_otype           = 'WS'
        act_objid           = mv_objid
        act_plvar           = '01'
      EXCEPTIONS
        no_active_plvar     = 1
        task_not_found      = 2
        task_not_deleted    = 3
        task_not_enqueued   = 4
        task_type_not_valid = 5
        OTHERS              = 6.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from RH_TASK_DELETE RC={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_wfd_key                 TYPE swd_wfdkey,
          lo_document                TYPE REF TO if_ixml_document,
          lo_pdws                    TYPE REF TO if_ixml_element,
          lo_converter               TYPE REF TO if_swf_pdef_import,
          lo_document_base           TYPE REF TO cl_xml_document_base,
          im_task_already_created    TYPE abap_bool,
          ex_return                  TYPE swd_return,
          ex_errors                  TYPE swd_terror,
          ex_activation_not_possible TYPE sytabix,
          ls_head                    TYPE swd_ahead,
          lv_rc                      TYPE sysubrc.

    io_xml->read(
      EXPORTING
        iv_name = 'KEY'
      CHANGING
        cg_data = ls_wfd_key ).

    lo_document ?= io_xml->get_raw( ).

    lo_pdws = lo_document->find_from_name_ns( name  = 'PDTS' ).

    CREATE OBJECT lo_converter TYPE cl_wfd_convert_ixml_to_def.

    CREATE OBJECT lo_document_base .
    lo_document_base->create_with_node( lo_pdws ).


    PERFORM ssc_wd_create IN PROGRAM saplswdd
      USING
        im_task_already_created
      CHANGING
        ex_return.

    lv_rc = lo_converter->convert( xml_document = lo_document_base
                                   language     = sy-langu ).

    CALL FUNCTION 'SWD_INTERN_GET_HEADER'
      IMPORTING
        head                = ls_head
*       global              =
*       properties          =     " Properties
*       containers          =     " Container
*       local_events        =     " Local Event
*       events              =     " Workflow Definition: Table of Events in WF Builder
*      TABLES
*       binding             =     " Workflow Definition: Binding for Steps (Temporary)
*       functions           =     " Functions
*       tasks               =     " Tasks Referenced Directly in Workflow Buffer
      EXCEPTIONS
        buffer_not_occupied = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from SWD_WORKFLOW_STORE RC={ sy-subrc }| ).
    ENDIF.

    ls_head-wfd_id  = ls_wfd_key-wfd_id.
    ls_head-version = ls_wfd_key-version.
    ls_head-exetyp  = ls_wfd_key-exetyp.

    CALL FUNCTION 'SWD_INTERN_SET_BUFFER'
      EXPORTING
        wfd_header = ls_head.

    CALL FUNCTION 'SWD_WORKFLOW_STORE'
      EXPORTING
        im_okcode               = 'SAVE'
        im_package              = iv_package
        im_force_gen            = abap_true
*      IMPORTING
*       ex_task                 = mv_wfd_id
*       ex_wfdkey               =
      EXCEPTIONS
        internal_database_error = 1
        internal_no_definition  = 2
        action_canceled         = 3
        OTHERS                  = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from SWD_WORKFLOW_STORE RC={ sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'SWD_WORKFLOW_ACTIVATE'
      EXPORTING
        im_force_gen               = abap_true
        im_package                 = iv_package
      IMPORTING
        ex_errors                  = ex_errors
        ex_activation_not_possible = ex_activation_not_possible
        ex_return                  = ex_return.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.


    DATA: lv_endda TYPE plog-endda.

    lv_endda = '99991231'.

    CALL FUNCTION 'RH_READ_OBJECT'
      EXPORTING
        plvar     = '01'    " Plan Version
        otype     = 'WS'    " Object Type
        objid     = mv_objid    " Object ID
        istat     = '1'    " Status for Reading Text
        begda     = sy-datum    " Start Date for Reading Text
        endda     = lv_endda    " End Date for Reading Text
        ointerval = 'X'    " Return Object Validity Period ' '/X/I
        read_db   = 'X'    " X = Object is Read From Database (Buffer Replaced)
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    rv_bool = boolc( sy-subrc = 0 ).


  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS_REMOTE'
      STARTING NEW TASK 'GIT'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.


    DATA: ls_wfd_key    TYPE swd_wfdkey,
          first_node    TYPE REF TO if_ixml_element,
          lo_converter  TYPE REF TO if_swf_pdef_export,
          lo_document   TYPE REF TO cl_xml_document_base,
          ls_swd_header TYPE swd_header.

    CREATE OBJECT lo_converter TYPE cl_wfd_convert_def_to_ixml.

    SELECT * FROM swd_header
             INTO ls_swd_header
             WHERE wfd_id = mv_wfd_id
             ORDER BY PRIMARY KEY.
    ENDSELECT.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |No active version of workflow { mv_objid } found| ).
    ENDIF.

    ls_wfd_key-wfd_id  = ls_swd_header-wfd_id.
    ls_wfd_key-version = ls_swd_header-version.
    ls_wfd_key-exetyp  = 'S'. "ls_swd_header-exetyp.

    lo_document = lo_converter->convert( language     = sy-langu
                                         load_from_db = abap_true
                                         wfd_key      = ls_wfd_key ).

    IF lo_document IS BOUND.

      first_node ?= lo_document->get_first_node( ).

      io_xml->add( iv_name = 'KEY'
                   ig_data = ls_wfd_key ).

      io_xml->add_xml(
        EXPORTING
          iv_name = 'PDTS'
          ii_xml  = first_node ).

    ENDIF.


  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.

ENDCLASS.
