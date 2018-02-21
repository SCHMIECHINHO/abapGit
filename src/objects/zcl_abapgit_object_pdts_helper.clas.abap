CLASS zcl_abapgit_object_pdts_helper DEFINITION
  PUBLIC
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      set_objid
        IMPORTING
          !iv_objid TYPE hrobject-objid
          !io_task  TYPE REF TO cl_workflow_general_task_def,

      set_container_id
        IMPORTING
          !iv_id   TYPE guid_32
          !io_task TYPE REF TO cl_workflow_general_task_def .

ENDCLASS.



CLASS zcl_abapgit_object_pdts_helper IMPLEMENTATION.


  METHOD set_container_id.

    FIELD-SYMBOLS: <object> TYPE REF TO if_swf_cnt_container.

    ASSIGN ('IO_TASK->CONTAINER') TO <object>.
    ASSERT sy-subrc = 0.

    CALL METHOD <object>->('SET_GUID')
      EXPORTING
        guid_32 = iv_id.

  ENDMETHOD.


  METHOD set_objid.

    io_task->objid = iv_objid.

  ENDMETHOD.

ENDCLASS.
