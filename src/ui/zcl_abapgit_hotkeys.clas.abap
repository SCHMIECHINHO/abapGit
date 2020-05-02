CLASS zcl_abapgit_hotkeys DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_hotkey_ctl,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_renderable.

    CONSTANTS:
      c_showhotkeys_action TYPE string VALUE `showHotkeys` ##NO_TEXT.

    CLASS-METHODS:
      get_all_default_hotkeys
        RETURNING
          VALUE(rt_hotkey_actions) TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr
        RAISING
          zcx_abapgit_exception,

      merge_hotkeys_with_settings
        CHANGING
          ct_hotkey_actions TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr
        RAISING
          zcx_abapgit_exception.

    CLASS-METHODS:
      should_show_hint
        RETURNING
          VALUE(rv_yes) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_hotkey_providers TYPE TABLE OF REF TO zif_abapgit_gui_hotkeys.

    CLASS-DATA:
      gv_hint_was_shown            TYPE abap_bool,
      gt_interface_implementations TYPE saboo_iimpt.

    CLASS-METHODS:
      filter_relevant_classes
        IMPORTING
          it_classes TYPE seo_relkeys
        RETURNING
          VALUE(rt_classes) TYPE seo_relkeys,

      get_class_package
        IMPORTING
          iv_class_name TYPE seoclsname
        RETURNING
          VALUE(rv_package) TYPE devclass,

      get_referred_class_name
        IMPORTING
          io_ref TYPE any
        RETURNING
          VALUE(rv_name) TYPE seoclsname,

      get_hotkeys_by_class_name
        IMPORTING
          iv_class_name TYPE seoclsname
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr,

      get_hotkeys_from_global_intf
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr
        RAISING
          zcx_abapgit_exception,

      get_hotkeys_from_local_intf
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr
        RAISING
          zcx_abapgit_exception,

      get_local_intf_implementations
        RETURNING
          VALUE(rt_interface_implementations) TYPE saboo_iimpt
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_HOTKEYS IMPLEMENTATION.


  METHOD filter_relevant_classes.

    DATA lv_this_class_name TYPE seoclsname.
    DATA lv_this_class_pkg TYPE devclass.
    DATA lv_class_pkg TYPE devclass.
    DATA lo_dummy TYPE REF TO zcl_abapgit_hotkeys.

    FIELD-SYMBOLS <ls_class> LIKE LINE OF it_classes.

    lv_this_class_name = get_referred_class_name( lo_dummy ).
    lv_this_class_pkg = get_class_package( lv_this_class_name ).

    LOOP AT it_classes ASSIGNING <ls_class>.
      lv_class_pkg = get_class_package( <ls_class>-clsname ).
      IF lv_class_pkg = lv_this_class_pkg.
        APPEND <ls_class> TO rt_classes.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_all_default_hotkeys.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      rt_hotkey_actions = get_hotkeys_from_local_intf( ).
    ELSE.
      rt_hotkey_actions = get_hotkeys_from_global_intf( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_class_package.

    SELECT SINGLE devclass FROM tadir
      INTO rv_package
      WHERE pgmid = 'R3TR'
      AND object = 'CLAS'
      AND obj_name = iv_class_name.

  ENDMETHOD.


  METHOD get_hotkeys_by_class_name.

    CALL METHOD (iv_class_name)=>zif_abapgit_gui_hotkeys~get_hotkey_actions
      RECEIVING
        rt_hotkey_actions = rt_hotkeys.

  ENDMETHOD.


  METHOD get_hotkeys_from_global_intf.

    DATA: lt_hotkey_actions LIKE rt_hotkeys,
          lo_interface      TYPE REF TO cl_oo_interface,
          li_dummy          TYPE REF TO zif_abapgit_gui_hotkeys,
          lt_classes        TYPE seo_relkeys.

    FIELD-SYMBOLS: <ls_class> LIKE LINE OF lt_classes.

    TRY.
        lo_interface ?= cl_oo_class=>get_instance( get_referred_class_name( li_dummy ) ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    lt_classes = lo_interface->get_implementing_classes( ).
    lt_classes = filter_relevant_classes( lt_classes ). " For security reasons

    LOOP AT lt_classes ASSIGNING <ls_class>.
      lt_hotkey_actions = get_hotkeys_by_class_name( <ls_class>-clsname ).
      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_hotkeys_from_local_intf.

    DATA: lt_hotkey_actions            LIKE rt_hotkeys,
          lt_interface_implementations TYPE saboo_iimpt.

    FIELD-SYMBOLS: <ls_intf_implementation> TYPE vseoimplem.

    lt_interface_implementations = get_local_intf_implementations( ).

    LOOP AT lt_interface_implementations ASSIGNING <ls_intf_implementation>.
      lt_hotkey_actions = get_hotkeys_by_class_name( <ls_intf_implementation>-clsname ).
      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_local_intf_implementations.

    DATA: ls_type_infos             TYPE saboo_vseot,
          lt_method_implementations TYPE saboo_method_impl_tab,
          lt_source                 TYPE saboo_sourt.

    IF gt_interface_implementations IS INITIAL.

      READ REPORT sy-cprog INTO lt_source.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Cannot read { sy-cprog }| ).
      ENDIF.

      CALL FUNCTION 'SCAN_ABAP_OBJECTS_CLASSES'
        CHANGING
          vseo_tabs                   = ls_type_infos
          method_impls                = lt_method_implementations
          sourc_tab                   = lt_source
        EXCEPTIONS
          scan_abap_source_error      = 1
          scan_abap_src_line_too_long = 2
          OTHERS                      = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      gt_interface_implementations = ls_type_infos-iimpl_tab.

    ENDIF.

    rt_interface_implementations = gt_interface_implementations.

  ENDMETHOD.


  METHOD get_referred_class_name.

    DATA lo_ref TYPE REF TO cl_abap_refdescr.
    lo_ref ?= cl_abap_typedescr=>describe_by_data( io_ref ).
    rv_name = lo_ref->get_referenced_type( )->get_relative_name( ).

  ENDMETHOD.


  METHOD merge_hotkeys_with_settings.

    DATA lt_user_defined_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey.
    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF ct_hotkey_actions.
    FIELD-SYMBOLS <ls_user_defined_hotkey> LIKE LINE OF lt_user_defined_hotkeys.

    lt_user_defined_hotkeys = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_hotkeys( ).

    LOOP AT ct_hotkey_actions ASSIGNING <ls_hotkey>.
      READ TABLE lt_user_defined_hotkeys ASSIGNING <ls_user_defined_hotkey>
        WITH TABLE KEY action COMPONENTS
          ui_component = <ls_hotkey>-ui_component
          action       = <ls_hotkey>-action.
      IF sy-subrc = 0.
        <ls_hotkey>-hotkey = <ls_user_defined_hotkey>-hotkey.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey LIKE LINE OF rt_hotkey_actions.

    ls_hotkey-ui_component  = 'Hotkeys'.
    ls_hotkey-action        = c_showhotkeys_action.
    ls_hotkey-description   = 'Show hotkeys help'.
    ls_hotkey-hotkey = '?'.
    INSERT ls_hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys.

    DATA li_hotkey_provider      LIKE LINE OF mt_hotkey_providers.
    DATA lt_hotkey_portion       LIKE rt_registered_hotkeys.
    DATA lt_user_defined_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey.
    DATA lo_settings             TYPE REF TO zcl_abapgit_settings.
    DATA ls_hotkey_to_add        LIKE LINE OF lt_hotkey_portion.

    FIELD-SYMBOLS <ls_user_defined_hotkey> LIKE LINE OF lt_user_defined_hotkeys.

    lo_settings             = zcl_abapgit_persist_settings=>get_instance( )->read( ).
    lt_user_defined_hotkeys = lo_settings->get_hotkeys( ).

    LOOP AT mt_hotkey_providers INTO li_hotkey_provider.
      lt_hotkey_portion = li_hotkey_provider->get_hotkey_actions( ).

      LOOP AT lt_hotkey_portion INTO ls_hotkey_to_add.
        READ TABLE lt_user_defined_hotkeys ASSIGNING <ls_user_defined_hotkey>
          WITH TABLE KEY action COMPONENTS
            ui_component = ls_hotkey_to_add-ui_component
            action       = ls_hotkey_to_add-action.
        IF sy-subrc = 0.
          ls_hotkey_to_add-hotkey = <ls_user_defined_hotkey>-hotkey.
        ENDIF.

        READ TABLE rt_registered_hotkeys TRANSPORTING NO FIELDS
          WITH TABLE KEY action COMPONENTS
            ui_component = ls_hotkey_to_add-ui_component
            action       = ls_hotkey_to_add-action.
        IF sy-subrc = 0.
          DELETE rt_registered_hotkeys INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      APPEND LINES OF lt_hotkey_portion TO rt_registered_hotkeys.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~register_hotkeys.
    IF ii_hotkeys IS BOUND.
      APPEND ii_hotkeys TO mt_hotkey_providers.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~reset.
    CLEAR mt_hotkey_providers.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA:
      lv_hint                 TYPE string,
      lt_registered_hotkeys   TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr,
      lv_hotkey               TYPE string.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_registered_hotkeys.

    zif_abapgit_gui_hotkey_ctl~register_hotkeys( me ). " TODO refactor ? don't do this directly ?

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_registered_hotkeys = zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT lt_registered_hotkeys BY description.

    " Render hotkeys
    ri_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_registered_hotkeys ASSIGNING <ls_hotkey>.
      ri_html->add( |<li>|
        && |<span class="key-id">{ <ls_hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <ls_hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.
    ri_html->add( '</ul>' ).

    " Wrap
    CLEAR: lv_hotkey.

*    READ TABLE lt_registered_hotkeys ASSIGNING <ls_hotkey>
*      WITH TABLE KEY action COMPONENTS
*      action = zcl_abapgit_gui_page=>c_global_page_action-showhotkeys.
*    IF sy-subrc = 0.
*      lv_hotkey = <ls_hotkey>-hotkey.
*    ENDIF.
*
*    IF lv_hotkey IS NOT INITIAL.
*      lv_hint = |Close window with '{ <ls_hotkey>-hotkey }' or upper right corner 'X'|.
*    ENDIF.

    ri_html = zcl_abapgit_gui_chunk_lib=>render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = lv_hint
      iv_hide       = abap_true
      iv_scrollable = abap_false
      io_content    = ri_html ).

*    IF <ls_hotkey> IS ASSIGNED AND zcl_abapgit_hotkeys=>should_show_hint( ) = abap_true.
*      ro_html->add( |<div id="hotkeys-hint" class="corner-hint">|
*        && |Press '{ <ls_hotkey>-hotkey }' to get keyboard shortcuts list|
*        && |</div>| ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
