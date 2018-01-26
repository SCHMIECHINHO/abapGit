*&---------------------------------------------------------------------*
*& Include zabapgit_object_pdws
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_pdws DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_pdws DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PRIVATE SECTION.
    DATA: mv_objid  TYPE hrobject-objid,
          mv_wfd_id TYPE swd_wfd_id.

ENDCLASS.                    "lcl_object_pdws DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_pdws IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_pdws IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_objid = ms_item-obj_name.
    mv_wfd_id = |WS{ mv_objid }|.

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

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~serialize.

    DATA: ls_wfd_key    TYPE swd_wfdkey,
          retcode       TYPE i,
          stream        TYPE xstring,
          size          TYPE sytabix,
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


*    DATA: ex_document TYPE REF TO cl_xml_document_base,
*          ex_errors   TYPE swd_terror,
*          retcode TYPE i,
*          stream TYPE xstring,
*          size TYPE sytabix.

*    DATA: ls_workflow TYPE ty_workflow.
*
*    cl_workflow_factory=>create_ws(
*      EXPORTING
*        objid                      = mv_objid
**        no_changes_allowed         = 'X'
**        enqueue_already_done       = SPACE
*      RECEIVING
*        ws_inst                    = DATA(lo_inst)
*      EXCEPTIONS
*        workflow_does_not_exist    = 1
*        object_could_not_be_locked = 2
*        objid_not_given            = 3
*        OTHERS                     = 4 ).
*
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( |error from CREATE_WS { sy-subrc }| ).
*    ENDIF.

*    DATA(lo_wf_export) = NEW cl_wfd_wizard_export_intern( |XML-EXPORT| ).
*
*    lo_wf_export->execute(
*      IMPORTING
*        ex_document         = ex_document    " XML Document - Management (Basis Class)
*        ex_errors           = ex_errors
*      EXCEPTIONS
*        export_not_possible = 1
*        OTHERS              = 2 ).
*
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    ex_document->render_2_xstring(
*      EXPORTING
*        pretty_print = 'X'
*      IMPORTING
*        retcode      = retcode
*        stream       = stream
*        size         = size ).

*    io_xml->add( iv_name = 'PDWS'
*                 ig_data = ls_task ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

*    DATA: lv_mode   TYPE c LENGTH 1,
*          ls_tpara  TYPE tpara,
*          ls_tparat TYPE tparat.
*
*
*    SELECT SINGLE * FROM tpara INTO ls_tpara
*      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
*    IF sy-subrc = 0.
*      lv_mode = 'M'.
*    ELSE.
*      lv_mode = 'I'.
*    ENDIF.
*
*    io_xml->read( EXPORTING iv_name = 'TPARA'
*                  CHANGING cg_data = ls_tpara ).
*    io_xml->read( EXPORTING iv_name = 'TPARAT'
*                  CHANGING cg_data = ls_tparat ).
*
*    CALL FUNCTION 'RS_CORR_INSERT'
*      EXPORTING
*        object              = ms_item-obj_name
*        object_class        = 'PARA'
*        mode                = lv_mode
*        global_lock         = abap_true
*        devclass            = iv_package
*        master_language     = mv_language
*      EXCEPTIONS
*        cancelled           = 1
*        permission_failure  = 2
*        unknown_objectclass = 3
*        OTHERS              = 4.
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT, PARA' ).
*    ENDIF.
*
*    MODIFY tpara FROM ls_tpara.                           "#EC CI_SUBRC
*    ASSERT sy-subrc = 0.
*
*    MODIFY tparat FROM ls_tparat.                         "#EC CI_SUBRC
*    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~delete.

*    DATA: lv_paramid TYPE tpara-paramid.
*
*
*    lv_paramid = ms_item-obj_name.
*    CALL FUNCTION 'RS_PARAMETER_DELETE'
*      EXPORTING
*        objectname = lv_paramid
*      EXCEPTIONS
*        cancelled  = 1
*        OTHERS     = 2.
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'error from RS_PRAMETER_DELETE' ).
*    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_pdws IMPLEMENTATION
