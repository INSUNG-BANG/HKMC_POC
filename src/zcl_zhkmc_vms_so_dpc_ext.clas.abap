class ZCL_ZHKMC_VMS_SO_DPC_EXT definition
  public
  inheriting from ZCL_ZHKMC_VMS_SO_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZHKMC_VMS_SO_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY.

DATA : lt_deep_entity  TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ty_deep_entity, "Deep Entity Type
           ls_deep_entity  TYPE zcl_zhkmc_vms_so_mpc_ext=>ty_deep_entity,          "Deep Entity Type
*       ls_item        TYPE zcl_zhkmc_vms_so_mpc_ext=>ts_so_item,
           lt_item         TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ts_so_item,
*           ls_schedules    TYPE zcl_zhkmc_vms_so_mpc_ext=>ts_so_schedules,
           lt_schedules    TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ts_so_schedules,
*           ls_partners     TYPE zcl_zhkmc_vms_so_mpc_ext=>ts_so_partners,
           lt_partners     TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ts_so_partners,
*           ls_config_ref   TYPE zcl_zhkmc_vms_so_mpc_ext=>ts_so_config_ref,
           lt_config_ref   TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ts_so_config_ref,
*           ls_config_inst  TYPE zcl_zhkmc_vms_so_mpc_ext=>ts_so_config_inst,
           lt_config_inst  TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ts_so_config_inst,
*           ls_config_value TYPE zcl_zhkmc_vms_so_mpc_ext=>ts_so_config_value,
           lt_config_value TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ts_so_config_value.

    DATA: order_headers_out     TYPE TABLE OF bapisdhd,
          order_items_out       TYPE TABLE OF  bapisdit,
          order_schedules_out   TYPE  TABLE OF bapisdhedu,
          order_partners_out    TYPE  TABLE OF  bapisdpart,
          order_address_out     TYPE TABLE OF bapisdcoad,
          order_cfgs_curefs_out TYPE TABLE OF bapicurefm,
          order_cfgs_cuins_out  TYPE TABLE OF bapicuinsm,
          order_cfgs_cuvals_out TYPE TABLE OF  bapicuvalm.



    IF iv_entity_set_name = 'SO_HEADER'.


    DATA : it_vbeln TYPE  TABLE OF sales_key,
           wa_vbeln TYPE sales_key.

  LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_vbeln>).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_vbeln>-value
        IMPORTING
          output = wa_vbeln.
      .
      APPEND wa_vbeln TO it_vbeln.
    ENDLOOP.


      DATA:  ls_ord_view       TYPE  order_view.

      ls_ord_view-header      = abap_true.
      ls_ord_view-item        = abap_true.
      ls_ord_view-sdschedule  = abap_true.
*      ls_ord_view-business    = abap_true.
      ls_ord_view-partner     = abap_true.
      ls_ord_view-address     = abap_true.
*      ls_ord_view-status_h    = abap_true.
*      ls_ord_view-status_i    = abap_true.
*      ls_ord_view-sdcond      = abap_true.
*      ls_ord_view-sdcond_add  = abap_true.
*      ls_ord_view-contract    = abap_true.
*      ls_ord_view-text        = abap_true.
*      ls_ord_view-flow        = abap_true.
*      ls_ord_view-billplan    = abap_true.
      ls_ord_view-configure   = abap_true.
*      ls_ord_view-credcard    = abap_true.
*      ls_ord_view-incomp_log  = abap_true.


      CALL FUNCTION 'BAPISDORDER_GETDETAILEDLIST'
        EXPORTING
          i_bapi_view           = ls_ord_view
*         I_MEMORY_READ         =
*         I_WITH_HEADER_CONDITIONS       = ' '
        TABLES
          sales_documents       = it_vbeln
          order_headers_out     = order_headers_out
          order_items_out       = order_items_out
          order_schedules_out   = order_schedules_out
*         ORDER_BUSINESS_OUT    =
          order_partners_out    = order_partners_out
          order_address_out     = order_address_out
*         ORDER_STATUSHEADERS_OUT        =
*         ORDER_STATUSITEMS_OUT =
*         ORDER_CONDITIONS_OUT  =
*         ORDER_COND_HEAD       =
*         ORDER_COND_ITEM       =
*         ORDER_COND_QTY_SCALE  =
*         ORDER_COND_VAL_SCALE  =
*         ORDER_CONTRACTS_OUT   =
*         ORDER_TEXTHEADERS_OUT =
*         ORDER_TEXTLINES_OUT   =
*         ORDER_FLOWS_OUT       =
          order_cfgs_curefs_out = order_cfgs_curefs_out
*         ORDER_CFGS_CUCFGS_OUT =
          order_cfgs_cuins_out  = order_cfgs_cuins_out
*         ORDER_CFGS_CUPRTS_OUT =
          order_cfgs_cuvals_out = order_cfgs_cuvals_out
*         ORDER_CFGS_CUBLBS_OUT =
*         ORDER_CFGS_CUVKS_OUT  =
*         ORDER_BILLINGPLANS_OUT         =
*         ORDER_BILLINGDATES_OUT         =
*         ORDER_CREDITCARDS_OUT =
*         EXTENSIONOUT          =
        .

 IF lines( order_headers_out ) = 0.
      DATA: message TYPE string.
      CONCATENATE 'Sales Order ''' <fs_vbeln>-value  ''' not exist' INTO message.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
          message_unlimited = message.
      exit.
    ENDIF.

    LOOP AT order_headers_out INTO DATA(ls_order_header).
      "header
      ls_deep_entity = CORRESPONDING #( ls_order_header ).

      "item
    ls_deep_entity-headertoitem = VALUE #( FOR ls_item IN  order_items_out WHERE ( doc_number = ls_order_header-doc_number )
           ( doc_number = ls_item-doc_number
             itm_number = ls_item-itm_number
             material = ls_item-material
             plant = ls_item-plant
             store_loc  = ls_item-stge_loc
             itemtoschedule = VALUE #( FOR <FS_SCHEDULE> IN order_schedules_out WHERE ( doc_number =  ls_item-doc_number  AND itm_number = LS_ITEM-itm_number )
                                        (    DOC_NUMBER =  <FS_SCHEDULE>-doc_number
                                             ITM_NUMBER =  <FS_SCHEDULE>-itm_number
                                             SCHED_LINE =  <FS_SCHEDULE>-sched_line
                                             REQ_DATE   =  <FS_SCHEDULE>-req_date
                                             REQ_QTY    =  <FS_SCHEDULE>-req_qty
                                             CONFIR_QTY =  <FS_SCHEDULE>-confir_qty
                                             SALES_UNIT =  <FS_SCHEDULE>-sales_unit
                                             AVAIL_CON  =  <FS_SCHEDULE>-avail_con
                                             TP_DATE =  <FS_SCHEDULE>-tp_date
                                             LOAD_DATE =  <FS_SCHEDULE>-load_date
                                             GI_DATE =  <FS_SCHEDULE>-gi_date ) )
             itemtopartner = VALUE #( FOR <FS_PARTNER> IN order_partners_out WHERE ( sd_doc =  ls_item-doc_number )
                                      FOR <fs_ADDRESS> IN order_address_out WHERE ( doc_number =  ls_item-doc_number AND  address = <FS_PARTNER>-address )
                                        ( doc_number = <FS_PARTNER>-sd_doc
                                        partn_role =  <FS_PARTNER>-partn_role
                                         partn_numb = <FS_PARTNER>-customer
                                         itm_number = <FS_PARTNER>-itm_number
                                         name = <fs_ADDRESS>-name ) )
             itemtoconfig = VALUE #( for <FS_REF> IN  order_cfgs_curefs_out WHERE ( sd_doc = ls_item-doc_number AND posex = ls_item-itm_number  )
                                    for <FS_INST> IN  order_cfgs_cuins_out WHERE ( sd_doc = ls_item-doc_number AND inst_id = <FS_REF>-inst_id and config_id = <FS_REF>-config_id )
                                      ( POSEX = <FS_REF>-posex
                                       CONFIG_ID = <FS_INST>-config_id
                                       DOC_NUMBER = <FS_INST>-sd_doc
                                       INST_ID = <FS_INST>-inst_id
                                       OBJ_TYPE = <FS_INST>-obj_type
                                       CLASS_TYPE = <FS_INST>-class_type
                                       OBJ_KEY = <FS_INST>-obj_key
                                       QUANTITY = <FS_INST>-quantity
                                       QUANTITY_UNIT = <FS_INST>-quantity_unit
                                       configtovalue = VALUE #(  for <FS_VALUE> IN order_cfgs_cuvals_out WHERE ( sd_doc = ls_order_header-doc_number AND inst_id = <FS_INST>-inst_id AND config_id =  <FS_INST>-config_id  )
                                                              ( DOC_NUMBER =  <FS_VALUE>-sd_doc
                                                                config_id =  <FS_VALUE>-config_id
                                                                INST_ID =  <FS_VALUE>-inst_id
                                                                CHARC =  <FS_VALUE>-charc
                                                                CHARC_TXT =  <FS_VALUE>-CHARC_TXT
                                                                VALUE =  <FS_VALUE>-value
                                                                VALUE_TXT =  <FS_VALUE>-value_txt
                                                                VALCODE =  <FS_VALUE>-valcode ) )
              ) ) ) ).
*       = CORRESPONDING #( lt_item[] ).
*
*      "schedule line
*      lt_schedules[] =  VALUE #( FOR ls_schedules IN
*          (  doc_number = ls_schedules-doc_number
*          itm_number =  ls_schedules-itm_number
*            sched_line = ls_schedules-sched_line
*            req_date = ls_schedules-req_date
*            req_qty = ls_schedules-req_qty ) ).
*      ls_deep_entity-itemtoschedule = CORRESPONDING #(   lt_schedules[] ).
*
*      "partners
*      lt_partners[] =  VALUE #( FOR <fs> IN  order_partners_out WHERE ( sd_doc = ls_order_header-doc_number )
*                                FOR <fs2> IN order_address_out WHERE ( doc_number = ls_order_header-doc_number
*                                                                 AND  address = <fs>-address )
*          ( doc_number = <fs>-sd_doc
*          partn_role =  <fs>-partn_role
*           partn_numb = <fs>-customer
*           itm_number = <fs>-itm_number
*           name = <fs2>-name ) ).
*      ls_deep_entity-itemtopartner = CORRESPONDING #(  lt_partners[] ).
*
*      "configuration ref.
*      lt_config_ref[] = VALUE #( FOR <fs_ref>  IN  order_cfgs_curefs_out WHERE ( sd_doc = ls_order_header-doc_number )
*     ( doc_number = <fs_Ref>-sd_doc
*     posex = <fs_ref>-posex
*       config_id = <fs_ref>-config_id
*       root_id = <fs_ref>-inst_id ) ) .
*      ls_deep_entity-itemtoconfigref = CORRESPONDING #( lt_config_ref[] ).
*
*      "config inst.
*      lt_config_inst[] = VALUE #( FOR <fs_inst> IN order_cfgs_cuins_out WHERE ( sd_doc = ls_order_header-doc_number )
*      ( doc_number = <fs_inst>-sd_doc
*        config_id = <fs_inst>-config_id
*        inst_id = <fs_inst>-inst_id
*        obj_type = <fs_inst>-obj_type
*        class_type = <fs_inst>-class_type
*        obj_key = <fs_inst>-obj_key
*        quantity = <fs_inst>-quantity
*        quantity_unit = <fs_inst>-quantity_unit ) ) .
*      ls_deep_entity-configreftoinst = CORRESPONDING #( lt_config_inst[] ).
*
**             *config value.
*      lt_config_value[] = VALUE #( FOR <fs_value> IN order_cfgs_cuvals_out WHERE ( sd_doc = ls_order_header-doc_number )
*        ( doc_number = <fs_value>-sd_doc
*         config_id = <fs_value>-config_id
*            inst_id = <fs_value>-inst_id
*           charc = <fs_value>-charc
*           charc_txt = <fs_value>-charc_txt
*           value = <fs_value>-value
*           value_txt = <fs_value>-value_txt ) ) .
*      ls_deep_entity-configinsttovalue = CORRESPONDING #(  lt_config_value[] ).

      APPEND ls_deep_entity TO lt_deep_entity.


    ENDLOOP.


      CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref
        EXPORTING
          is_data = ls_deep_entity
        CHANGING
          cr_data = er_entity.
    ENDIF.

  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET.
DATA : lt_deep_entity  TYPE TABLE OF zcl_zhkmc_vms_so_mpc_ext=>ty_deep_entity, "Deep Entity Type
           ls_deep_entity  TYPE zcl_zhkmc_vms_so_mpc_ext=>ty_deep_entity.         "Deep Entity Type

    DATA: order_headers_out     TYPE TABLE OF bapisdhd,
          order_items_out       TYPE TABLE OF  bapisdit,
          order_schedules_out   TYPE  TABLE OF bapisdhedu,
          order_partners_out    TYPE  TABLE OF  bapisdpart,
          order_address_out     TYPE TABLE OF bapisdcoad,
          order_cfgs_curefs_out TYPE TABLE OF bapicurefm,
          order_cfgs_cuins_out  TYPE TABLE OF bapicuinsm,
          order_cfgs_cuvals_out TYPE TABLE OF  bapicuvalm.



    IF iv_entity_set_name = 'SO_HEADER'.


    DATA : it_vbeln TYPE  TABLE OF sales_key,
           wa_vbeln TYPE sales_key.

*  LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_vbeln>).
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = <fs_vbeln>-value
*        IMPORTING
*          output = wa_vbeln.
*      .
*      APPEND wa_vbeln TO it_vbeln.
*    ENDLOOP.

 SELECT vbeln FROM vbak
         UP TO 10 ROWS
         INTO TABLE it_vbeln ORDER BY erdat DESCENDING.

      DATA:  ls_ord_view       TYPE  order_view.

      ls_ord_view-header      = abap_true.
      ls_ord_view-item        = abap_true.
      ls_ord_view-sdschedule  = abap_true.
*      ls_ord_view-business    = abap_true.
      ls_ord_view-partner     = abap_true.
      ls_ord_view-address     = abap_true.
*      ls_ord_view-status_h    = abap_true.
*      ls_ord_view-status_i    = abap_true.
*      ls_ord_view-sdcond      = abap_true.
*      ls_ord_view-sdcond_add  = abap_true.
*      ls_ord_view-contract    = abap_true.
*      ls_ord_view-text        = abap_true.
*      ls_ord_view-flow        = abap_true.
*      ls_ord_view-billplan    = abap_true.
      ls_ord_view-configure   = abap_true.
*      ls_ord_view-credcard    = abap_true.
*      ls_ord_view-incomp_log  = abap_true.


      CALL FUNCTION 'BAPISDORDER_GETDETAILEDLIST'
        EXPORTING
          i_bapi_view           = ls_ord_view
*         I_MEMORY_READ         =
*         I_WITH_HEADER_CONDITIONS       = ' '
        TABLES
          sales_documents       = it_vbeln
          order_headers_out     = order_headers_out
          order_items_out       = order_items_out
          order_schedules_out   = order_schedules_out
*         ORDER_BUSINESS_OUT    =
          order_partners_out    = order_partners_out
          order_address_out     = order_address_out
*         ORDER_STATUSHEADERS_OUT        =
*         ORDER_STATUSITEMS_OUT =
*         ORDER_CONDITIONS_OUT  =
*         ORDER_COND_HEAD       =
*         ORDER_COND_ITEM       =
*         ORDER_COND_QTY_SCALE  =
*         ORDER_COND_VAL_SCALE  =
*         ORDER_CONTRACTS_OUT   =
*         ORDER_TEXTHEADERS_OUT =
*         ORDER_TEXTLINES_OUT   =
*         ORDER_FLOWS_OUT       =
          order_cfgs_curefs_out = order_cfgs_curefs_out
*         ORDER_CFGS_CUCFGS_OUT =
          order_cfgs_cuins_out  = order_cfgs_cuins_out
*         ORDER_CFGS_CUPRTS_OUT =
          order_cfgs_cuvals_out = order_cfgs_cuvals_out
*         ORDER_CFGS_CUBLBS_OUT =
*         ORDER_CFGS_CUVKS_OUT  =
*         ORDER_BILLINGPLANS_OUT         =
*         ORDER_BILLINGDATES_OUT         =
*         ORDER_CREDITCARDS_OUT =
*         EXTENSIONOUT          =
        .

* IF lines( order_headers_out ) = 0.
*      DATA: message TYPE string.
*      CONCATENATE 'Sales Order ''' <fs_vbeln>-value  ''' not exist' INTO message.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
*          message_unlimited = message.
*    ENDIF.

    LOOP AT order_headers_out INTO DATA(ls_order_header).
      "header
      ls_deep_entity = CORRESPONDING #( ls_order_header ).

      "item
    ls_deep_entity-headertoitem = VALUE #( FOR ls_item IN  order_items_out WHERE ( doc_number = ls_order_header-doc_number )
           ( doc_number = ls_item-doc_number
             itm_number = ls_item-itm_number
             material = ls_item-material
             plant = ls_item-plant
             store_loc  = ls_item-stge_loc
             itemtoschedule = VALUE #( FOR <FS_SCHEDULE> IN order_schedules_out WHERE ( doc_number =  ls_item-doc_number  AND itm_number = LS_ITEM-itm_number )
                                        (    DOC_NUMBER =  <FS_SCHEDULE>-doc_number
                                             ITM_NUMBER =  <FS_SCHEDULE>-itm_number
                                             SCHED_LINE =  <FS_SCHEDULE>-sched_line
                                             REQ_DATE   =  <FS_SCHEDULE>-req_date
                                             REQ_QTY    =  <FS_SCHEDULE>-req_qty
                                             CONFIR_QTY =  <FS_SCHEDULE>-confir_qty
                                             SALES_UNIT =  <FS_SCHEDULE>-sales_unit
                                             AVAIL_CON  =  <FS_SCHEDULE>-avail_con
                                             TP_DATE =  <FS_SCHEDULE>-tp_date
                                             LOAD_DATE =  <FS_SCHEDULE>-load_date
                                             GI_DATE =  <FS_SCHEDULE>-gi_date ) )
             itemtopartner = VALUE #( FOR <FS_PARTNER> IN order_partners_out WHERE ( sd_doc =  ls_item-doc_number )
                                      FOR <fs_ADDRESS> IN order_address_out WHERE ( doc_number =  ls_item-doc_number AND  address = <FS_PARTNER>-address )
                                        ( doc_number = <FS_PARTNER>-sd_doc
                                        partn_role =  <FS_PARTNER>-partn_role
                                         partn_numb = <FS_PARTNER>-customer
                                         itm_number = <FS_PARTNER>-itm_number
                                         name = <fs_ADDRESS>-name ) )
             itemtoconfig = VALUE #( for <FS_REF> IN  order_cfgs_curefs_out WHERE ( sd_doc = ls_item-doc_number AND posex = ls_item-itm_number  )
                                    for <FS_INST> IN  order_cfgs_cuins_out WHERE ( sd_doc = ls_item-doc_number AND inst_id = <FS_REF>-inst_id and config_id = <FS_REF>-config_id )
                                      ( POSEX = <FS_REF>-posex
                                       CONFIG_ID = <FS_INST>-config_id
                                       DOC_NUMBER = <FS_INST>-sd_doc
                                       INST_ID = <FS_INST>-inst_id
                                       OBJ_TYPE = <FS_INST>-obj_type
                                       CLASS_TYPE = <FS_INST>-class_type
                                       OBJ_KEY = <FS_INST>-obj_key
                                       QUANTITY = <FS_INST>-quantity
                                       QUANTITY_UNIT = <FS_INST>-quantity_unit
                                       configtovalue = VALUE #(  for <FS_VALUE> IN order_cfgs_cuvals_out WHERE ( sd_doc = ls_order_header-doc_number AND inst_id = <FS_INST>-inst_id AND config_id =  <FS_INST>-config_id  )
                                                              ( DOC_NUMBER =  <FS_VALUE>-sd_doc
                                                                config_id =  <FS_VALUE>-config_id
                                                                INST_ID =  <FS_VALUE>-inst_id
                                                                CHARC =  <FS_VALUE>-charc
                                                                CHARC_TXT =  <FS_VALUE>-CHARC_TXT
                                                                VALUE =  <FS_VALUE>-value
                                                                VALUE_TXT =  <FS_VALUE>-value_txt
                                                                VALCODE =  <FS_VALUE>-valcode ) )
              ) ) ) ).
*

      APPEND ls_deep_entity TO lt_deep_entity.
             clear ls_deep_entity.

    ENDLOOP.


      CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref
        EXPORTING
          is_data = lt_deep_entity
        CHANGING
          cr_data = er_entityset.
    ENDIF.
  endmethod.
ENDCLASS.
