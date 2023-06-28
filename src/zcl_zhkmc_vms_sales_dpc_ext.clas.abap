class ZCL_ZHKMC_VMS_SALES_DPC_EXT definition
  public
  inheriting from ZCL_ZHKMC_VMS_SALES_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods SO_CREATE_SIMPLE_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHKMC_VMS_SALES_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.

    DATA : lt_deep_entity TYPE TABLE OF zcl_zhkmc_vms_sales_mpc_ext=>ty_deep_entity, "Deep Entity Type
           ls_deep_entity TYPE zcl_zhkmc_vms_sales_mpc_ext=>ty_deep_entity.         "Deep Entity Type

    DATA: order_headers_out     TYPE TABLE OF bapisdhd,
          order_items_out       TYPE TABLE OF  bapisdit,
          order_schedules_out   TYPE  TABLE OF bapisdhedu,
          order_partners_out    TYPE  TABLE OF  bapisdpart,
          order_address_out     TYPE TABLE OF bapisdcoad,
          order_cfgs_curefs_out TYPE TABLE OF bapicurefm,
          order_cfgs_cuins_out  TYPE TABLE OF bapicuinsm,
          order_cfgs_cuvals_out TYPE TABLE OF  bapicuvalm.

    DATA : it_vbeln    TYPE TABLE OF sales_key,
           s_vbeln     TYPE  RANGE OF vbak-vbeln,
           wa_vbeln    LIKE LINE OF s_vbeln,
           s_matnr     TYPE RANGE OF mara-matnr,
           wa_matnr    LIKE LINE OF s_matnr,
           s_sp_kunnr  TYPE RANGE OF vbpa-kunnr,
           wa_sp_kunnr LIKE LINE OF s_sp_kunnr,
           s_vkorg     TYPE RANGE OF vbak-vkorg,
           wa_vkorg    LIKE LINE OF s_vkorg.

    IF iv_entity_set_name = 'SO_SEARCH'.


      DATA(lt_filter_select_options) = io_tech_request_context->get_filter( )->get_filter_select_options( ).


      IF lines(  lt_filter_select_options ) = 0.
        DATA: message TYPE string VALUE `Please specific $filter=Material eq 'xxx' and SalesOrg eq 'xxx' and SoldToParty = 'xxxx' `.
*      CONCATENATE 'Corresponding Sales Orders not exist' INTO message.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
            message_unlimited = message.
      ENDIF.

      LOOP AT  lt_filter_select_options ASSIGNING FIELD-SYMBOL(<fs_filter>) .
        LOOP AT <fs_filter>-select_options ASSIGNING FIELD-SYMBOL(<fs_select_op>).

          CASE <fs_filter>-property.

            WHEN 'MATERIAL'.
              wa_matnr-option = 'EQ'.
              wa_matnr-sign = 'I'.
              wa_matnr-low = <fs_select_op>-low.
              APPEND wa_matnr TO s_matnr.

            WHEN 'SALESORG'.
              wa_vkorg-option = 'EQ'.
              wa_vkorg-sign = 'I'.
              wa_vkorg-low = <fs_select_op>-low.
              APPEND wa_vkorg TO s_vkorg.

            WHEN 'SOLDTOPARTY'.
              wa_sp_kunnr-option = 'EQ'.
              wa_sp_kunnr-sign = 'I'.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_select_op>-low
                IMPORTING
                  output = wa_sp_kunnr-low.
**      .
              APPEND wa_sp_kunnr TO s_sp_kunnr.
            WHEN 'DOC_NUMBER'.

              wa_vbeln-option = 'EQ'.
              wa_vbeln-sign = 'I'.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_select_op>-low
                IMPORTING
                  output = wa_vbeln-low.

              APPEND wa_vbeln TO s_vbeln.

          ENDCASE.

        ENDLOOP.

      ENDLOOP.


      SELECT h~vbeln FROM vbak AS h JOIN vbap AS i ON h~vbeln = i~vbeln
                                  JOIN vbpa AS p ON h~vbeln = p~vbeln
                  INTO TABLE it_vbeln
                   WHERE i~matnr IN s_matnr
                     AND p~kunnr IN s_sp_kunnr
                     AND p~parvw = 'AG' "SOLD-TO-PARTY
                     AND h~vkorg IN s_vkorg
                     AND h~vbeln IN s_vbeln.



*    ELSEIF iv_entity_set_name = 'SO_HEADER'.


*      lt_filter_select_options = io_tech_request_context->get_filter( )->get_filter_select_options( ).
*
*      IF lines(  lt_filter_select_options ) = 0.
*        message =  `Please specific $filter=DocNumber eq 'xxxxx' `.
**      CONCATENATE 'Corresponding Sales Orders not exist' INTO message.
*        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*          EXPORTING
*            textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
*            message_unlimited = message.
*      ENDIF.


*      LOOP AT  lt_filter_select_options ASSIGNING <fs_filter> WHERE property = 'DOC_NUMBER'.
*        LOOP AT <fs_filter>-select_options ASSIGNING FIELD-SYMBOL(<fs_vbeln>).
*
**
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = <fs_vbeln>-low
*            IMPORTING
*              output = wa_vbeln.
***      .
*          APPEND wa_vbeln TO it_vbeln.
*        ENDLOOP.
*
*      ENDLOOP.



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

sy-langu = 'EN'.

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
        message = 'Corresponding Sales Orders not exist'.
*      CONCATENATE 'Corresponding Sales Orders not exist' INTO message.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
            message_unlimited = message.
      ENDIF.
      DATA: s_atnam  TYPE RANGE OF cabn-atnam,
            wa_atnam LIKE LINE OF s_atnam.

      LOOP AT order_cfgs_cuvals_out ASSIGNING FIELD-SYMBOL(<fs_config>).
        wa_atnam-low = <fs_config>-charc.
        APPEND wa_atnam TO s_atnam.
      ENDLOOP.

      IF s_atnam[] IS NOT INITIAL.

        SELECT a~atnam, at~atbez, b~atwrt, c~atwtb
         FROM cabn AS a
          JOIN cabnt AS at
          ON  a~atinn = at~atinn
          AND a~adzhl = at~adzhl
          JOIN cawn AS b
         ON   a~atinn = b~atinn
         AND  a~adzhl = b~adzhl
         JOIN cawnt AS c
          ON  b~atinn = c~atinn
         AND  b~atzhl = c~atzhl
         INTO TABLE @DATA(lt_char_text)
         FOR ALL ENTRIES IN @s_atnam
         WHERE a~atnam = @s_atnam-low
         AND  at~spras = 'E'
         AND   c~spras = 'E'
         AND   c~lkenz = @space.
      ENDIF.

      SELECT parvw, vtext FROM tpart INTO TABLE @DATA(it_partner_role) WHERE spras = 'E'.



      "header
      ls_deep_entity-doc_number = wa_vbeln-low.
      ls_deep_entity-material = wa_matnr-low.
      ls_deep_entity-salesorg = wa_vkorg-low.
      ls_deep_entity-soldtoparty = wa_sp_kunnr-low.

      ls_deep_entity-searchtoheader = CORRESPONDING #( order_headers_out ).

      LOOP AT  ls_deep_entity-searchtoheader ASSIGNING FIELD-SYMBOL(<fs_header>).
        <fs_header>-headertoitem = VALUE #( FOR ls_item IN  order_items_out WHERE ( doc_number = <fs_header>-doc_number )
                                          ( doc_number = ls_item-doc_number
                                            itm_number = ls_item-itm_number
                                            material = ls_item-material
                                            plant = ls_item-plant
                                            store_loc  = ls_item-stge_loc
                                            itemtoschedule = VALUE #( FOR <fs_schedule> IN order_schedules_out WHERE ( doc_number =  ls_item-doc_number  AND itm_number = ls_item-itm_number )
                                            (    doc_number =  <fs_schedule>-doc_number
                                                 itm_number =  <fs_schedule>-itm_number
                                                 sched_line =  <fs_schedule>-sched_line
                                                 req_date   =  <fs_schedule>-req_date
                                                 req_qty    =  <fs_schedule>-req_qty
                                                 confir_qty =  <fs_schedule>-confir_qty
                                                 sales_unit =  <fs_schedule>-sales_unit
                                                 avail_con  =  <fs_schedule>-avail_con
                                                 tp_date =  <fs_schedule>-tp_date
                                                 load_date =  <fs_schedule>-load_date
                                                                         gi_date =  <fs_schedule>-gi_date ) )
                                          itemtopartner = VALUE #( FOR <fs_partner> IN order_partners_out WHERE ( sd_doc =  ls_item-doc_number )
                                                                   FOR <fs_address> IN order_address_out WHERE ( doc_number =  ls_item-doc_number AND  address = <fs_partner>-address )
                                                                   FOR <fs_partner_role_txt> IN it_partner_role WHERE ( parvw =  <fs_partner>-partn_role )
                                                                     ( doc_number = <fs_partner>-sd_doc
                                                                      partn_role = <fs_partner_role_txt>-vtext
                                                                      partn_numb = <fs_partner>-customer
                                                                      itm_number = <fs_partner>-itm_number
                                                                      name = <fs_address>-name ) )
                                          itemtoconfig = VALUE #( FOR <fs_ref> IN  order_cfgs_curefs_out WHERE ( sd_doc = ls_item-doc_number AND posex = ls_item-itm_number  )
                                                                 FOR <fs_inst> IN  order_cfgs_cuins_out WHERE ( sd_doc = ls_item-doc_number AND inst_id = <fs_ref>-inst_id AND config_id = <fs_ref>-config_id )
                                                                   ( posex = <fs_ref>-posex
                                                                    config_id = <fs_inst>-config_id
                                                                    doc_number = <fs_inst>-sd_doc
                                                                    root_id = <fs_inst>-inst_id
                                                                    obj_type = <fs_inst>-obj_type
                                                                    class_type = <fs_inst>-class_type
                                                                    obj_key = <fs_inst>-obj_key
                                                                    quantity = <fs_inst>-quantity
                                                                    quantity_unit = <fs_inst>-quantity_unit
                                                                    configtovalue = VALUE #(  FOR <fs_value> IN order_cfgs_cuvals_out WHERE ( sd_doc = <fs_header>-doc_number AND inst_id = <fs_inst>-inst_id AND config_id =  <fs_inst>-config_id  )
                                                                                              FOR <fs_char_text> IN lt_char_text WHERE ( atnam = <fs_value>-charc AND atwrt = <fs_value>-value )
                                                                                           ( doc_number =  <fs_value>-sd_doc
                                                                                             config_id =  <fs_value>-config_id
                                                                                             inst_id =  <fs_value>-inst_id
                                                                                             charc =  <fs_value>-charc
                                                                                             charc_txt =  <fs_char_text>-atbez
                                                                                             value =  <fs_value>-value
                                                                                             value_txt =  <fs_char_text>-atwtb ) )
*                                                                    valcode =  <fs_value>-valcode ) )
                 )  ) ) ) .
*



      ENDLOOP.

      IF order_headers_out[] IS NOT INITIAL.
        SELECT b~vbeln, a~vguid,a~vhcle, a~endcu,c~name_first FROM vlccuorder AS b JOIN  vlcvehicle AS  a ON b~vguid = a~vguid
                                                                          JOIN but000 AS c ON a~endcu = c~partner
       INTO TABLE @DATA(it_vehicle) FOR ALL ENTRIES IN  @order_headers_out
          WHERE b~vbeln = @order_headers_out-doc_number .
      ENDIF.

      DATA: wa_so_all TYPE zcl_zhkmc_vms_sales_mpc_ext=>ts_so_all,
            it_so_all TYPE zcl_zhkmc_vms_sales_mpc_ext=>tt_so_all.

      LOOP AT  order_headers_out ASSIGNING FIELD-SYMBOL(<ls_header>).
        wa_so_all-doc_number =  <ls_header>-doc_number.
        wa_so_all-sales_org = <ls_header>-sales_org.
        wa_so_all-distr_chan = <ls_header>-distr_chan.
        wa_so_all-division = <ls_header>-division.
        wa_so_all-sales_grp =  <ls_header>-sales_grp.
        wa_so_all-purch_no =  <ls_header>-purch_no.
        wa_so_all-req_date_h =  <ls_header>-req_date_h.

        LOOP AT it_vehicle ASSIGNING FIELD-SYMBOL(<fs_vehicle>) WHERE ( vbeln =  <ls_header>-doc_number ).
          wa_so_all-endcustomer = <fs_vehicle>-endcu.
          wa_so_all-endcustomer_name = <fs_vehicle>-name_first.
           wa_so_all-vehicleguid =  <fs_vehicle>-VGUID.
           wa_so_all-vin_number = <fs_vehicle>-vhcle.
        ENDLOOP.


        LOOP AT  order_items_out INTO DATA(fs_item) WHERE ( doc_number = <ls_header>-doc_number ).
          wa_so_all-itm_number = fs_item-itm_number.
          wa_so_all-material = fs_item-material.
          wa_so_all-material_text = fs_item-short_text.


          LOOP AT  order_partners_out ASSIGNING FIELD-SYMBOL(<fs_partner2>) WHERE ( sd_doc =  fs_item-doc_number AND partn_role = 'AG' ).
            LOOP AT  order_address_out ASSIGNING FIELD-SYMBOL(<fs_address2>) WHERE ( doc_number =  fs_item-doc_number AND  address = <fs_partner2>-address ).
              wa_so_all-soldtoparty = <fs_partner2>-customer.
              wa_so_all-soldtoparty_name = <fs_address2>-name.
            ENDLOOP.
          ENDLOOP.

          LOOP AT order_cfgs_curefs_out ASSIGNING FIELD-SYMBOL(<fs_ref2>) WHERE ( sd_doc = fs_item-doc_number AND posex = fs_item-itm_number  ).
            LOOP AT  order_cfgs_cuins_out ASSIGNING FIELD-SYMBOL(<fs_inst2>) WHERE ( sd_doc = fs_item-doc_number AND inst_id = <fs_ref2>-inst_id AND config_id = <fs_ref2>-config_id ).
              LOOP AT order_cfgs_cuvals_out ASSIGNING FIELD-SYMBOL(<fs_value2>) WHERE ( sd_doc =  fs_item-doc_number AND inst_id = <fs_inst2>-inst_id AND config_id =  <fs_inst2>-config_id  ).
                LOOP AT  lt_char_text ASSIGNING FIELD-SYMBOL(<fs_char_text2>) WHERE ( atnam = <fs_value2>-charc AND atwrt = <fs_value2>-value ).
                  CASE <fs_value2>-charc.
                    WHEN 'DOOR'.
                      wa_so_all-door_value = <fs_value2>-value.
                      wa_so_all-door_text =  <fs_char_text2>-atwtb.
                    WHEN 'DRIVE_TRAIN'.
                      wa_so_all-drivetrain_value = <fs_value2>-value.
                      wa_so_all-drivetrain_text  = <fs_char_text2>-atwtb.
                    WHEN 'EMISSION'.
                      wa_so_all-emission_value                = <fs_value2>-value.
                      wa_so_all-emission_text              = <fs_char_text2>-atwtb.
                    WHEN 'ENGINE_TYPE'.
                      wa_so_all-enginetype_value  = <fs_value2>-value.
                      wa_so_all-enginetype_text   = <fs_char_text2>-atwtb.
                    WHEN 'FAMILY'.
                      wa_so_all-family_value = <fs_value2>-value.
                      wa_so_all-family_text = <fs_char_text2>-atwtb.

                    WHEN 'MODEL_YEAR'.
                      wa_so_all-model_year =  <fs_value2>-value..

                    WHEN 'OPTION_GROUP'.
                      wa_so_all-optiongroup_value = <fs_value2>-value.
                      wa_so_all-optiongroup_text = <fs_char_text2>-atwtb.

                    WHEN 'SERIES'.
                      wa_so_all-series_value   = <fs_value2>-value.
                      wa_so_all-series_text   = <fs_char_text2>-atwtb.
                    WHEN 'TRANSMISSION'.
                      wa_so_all-transmission_value     = <fs_value2>-value.
                      wa_so_all-transmission_text = <fs_char_text2>-atwtb.
                    WHEN 'TRIM'.
                      wa_so_all-trim_value =  <fs_value2>-value.
                      wa_so_all-trim_text = <fs_char_text2>-atwtb.

                  ENDCASE.
                ENDLOOP.
              ENDLOOP.
            ENDLOOP.
            ENDLOOP.


          ENDLOOP.

           APPEND wa_so_all TO IT_SO_ALL.

           ENDLOOP.
           ls_deep_entity-searchtoso_all = IT_SO_ALL[].

*          ls_deep_entity-searchtoso_all = VALUE #( FOR <ls_header> IN order_headers_out
*                                                   FOR ls_item IN order_items_out WHERE ( doc_number = <ls_header>-doc_number )
*                                                   FOR <fs_ref> IN  order_cfgs_curefs_out WHERE ( sd_doc = ls_item-doc_number AND posex = ls_item-itm_number  )
*                                                   FOR <fs_inst> IN  order_cfgs_cuins_out WHERE ( sd_doc = ls_item-doc_number AND inst_id = <fs_ref>-inst_id AND config_id = <fs_ref>-config_id )
*                                                   FOR <fs_value> IN order_cfgs_cuvals_out WHERE ( sd_doc = <fs_header>-doc_number AND inst_id = <fs_inst>-inst_id AND config_id =  <fs_inst>-config_id  )
*                                                   FOR <fs_char_text> IN lt_char_text WHERE ( atnam = <fs_value>-charc AND atwrt = <fs_value>-value )
*                                                   FOR <fs_partner> IN order_partners_out WHERE ( sd_doc =  ls_item-doc_number AND partn_role = 'AG' )
*                                                   FOR <fs_address> IN order_address_out WHERE ( doc_number =  ls_item-doc_number AND  address = <fs_partner>-address )
**
*                                                  ( doc_number =  <ls_header>-doc_number
**                                                itm_number = ls_item-itm_number
*                                                   sales_org = <ls_header>-sales_org
*                                                   distr_chan = <ls_header>-distr_chan
*                                                   division = <ls_header>-division
*                                                   sales_grp =  <ls_header>-sales_grp
*                                                   purch_no =  <ls_header>-purch_no
*                                                   req_date_h =  <ls_header>-req_date_h
*                                                   material = ls_item-material
*                                                   soldtoparty = <fs_partner>-customer
*                                                   soldtoparty_name = <fs_address>-name
**                                               endcustomer = <fs_vehicle>-endcu
**                                               endcustomer_name = <FS_VEHICLE>-name_first
**                                               Door_Value
**                                               Door_Text
**                                               DriveTrain_Value
**                                               DriveTrain_Text
**                                               Emission_Value
**                                               Emission_Text
**                                               EngineType_Value
**                                               EngineType_Text
**                                               Family_Value
**                                               Family_Text
**                                               Model_Year
**                                               OptionGroup_Value
**                                               OptionGroup_Text
**                                               Series_Value
**                                               Series_Text
**                                               Transmission_Value
**                                               Transmission_Text
**                                               Trim_Value
**                                               Trim_Text





          APPEND ls_deep_entity TO lt_deep_entity.
          CLEAR ls_deep_entity.




          CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref
            EXPORTING
              is_data = lt_deep_entity
            CHANGING
              cr_data = er_entityset.
        ENDIF.

      ENDMETHOD.


  METHOD so_create_simple_create_entity.

    DATA : sales_order_input LIKE er_entity.


    io_data_provider->read_entry_data( IMPORTING es_data = sales_order_input ).

    DATA : lo_msg     TYPE REF TO /iwbep/if_message_container,
           ls_error   TYPE /iwbep/if_message_container=>ty_s_error_detail,
           rt_message TYPE /iwbep/t_message_container.



    CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
      RECEIVING
        ro_message_container = lo_msg.

    CALL METHOD lo_msg->reset( ).

    DATA message TYPE bapi_msg.
*
    FIELD-SYMBOLS: <fs_fieldvalue> TYPE any,
                   <fs_structure>  TYPE any.
    DATA: name TYPE string.

    DEFINE check_input_validation.

      CLEAR name.
      name = &1.

       ASSIGN sales_order_input TO <fs_structure>.

           ASSIGN COMPONENT name OF STRUCTURE <fs_structure>
            TO  <fs_fieldvalue>.

        IF <fs_fieldvalue> IS INITIAL.

       CLEAR message.
       CONCATENATE 'Enter' `'` &1 '`' 'Value'  INTO message .

       CALL METHOD lo_msg->add_message_text_only
       EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = message.

       ENDIF.
    END-OF-DEFINITION.


    check_input_validation : 'VEHICLEGUID', 'SALESORG', 'DISTRIBUTION', 'DIVISION', 'SOLDTOPARTY', 'ENDCUSTOMER',
                                       'CUSTREFRENCE' .

    rt_message = lo_msg->get_messages( iv_provide_text = '' ).

*” Raising Exception
    IF rt_message  IS NOT INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.


    DATA: gw_head  TYPE bapivehiactiondata_head,
          gw_head1 TYPE bapivehiactiondata_head,
          gw_headx TYPE bapivehiactiondata_headx,
          gw_id    TYPE bapivehicle_id_type,
          gt_item  TYPE STANDARD TABLE OF bapivehiactiondata_item,
          gw_item  TYPE bapivehiactiondata_item,
          gt_ref   TYPE STANDARD TABLE OF bapiconfig_references,
          gw_ref   TYPE bapiconfig_references,
          gt_ret   TYPE STANDARD TABLE OF bapiret2,
          gw_ret   TYPE bapiret2,
          lv_mess  TYPE string,
          gw_itemx TYPE bapivehiactiondata_itemx.

    DATA: config_references TYPE TABLE OF  bapiconfig_references,
          config_value      TYPE TABLE OF   bapicuval,
          vehicle_data      TYPE TABLE OF bapivehicle,
          it_return         TYPE TABLE OF  bapiret2,
          additional_param  TYPE TABLE OF bapivehicleaddparams,
          config_data       TYPE TABLE OF bapicucfg,
          config_inst       TYPE TABLE OF  bapicuins.


    SELECT vguid FROM vlcvehicle INTO TABLE @DATA(it_vehicle) WHERE vguid = @sales_order_input-vehicleguid .

    CALL FUNCTION 'BAPI_VEHICLE_GETLIST'
      EXPORTING
        requestedtablex   = 'XX'
      TABLES
        vehicleguid       = it_vehicle
*       VEHICLEGUID32     =
*       VEHICLENUMBER     =
*       VEHICLECUOBJ      =
*       VEHICLEIDENTNUMB  =
*       VEHICLEEXTERNNUM  =
*       VEHICLEADDPARAMS  =
        vehicledata       = vehicle_data
        config_references = config_references
        config_data       = config_data
        config_instance   = config_inst
*       CONFIG_INFO       =
        config_charsval   = config_value
*       CONFIG_KEY        =
*       VEHICLE_QUALIFIERS       =
*       EXTENSIONOUT      =
*       RETURN            = it_return
      .
    IF lines( vehicle_data ) = 0.
      DATA: no_message TYPE string VALUE `Corresponding Vehicle Guid not exist `.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
          message_unlimited = no_message.
    ENDIF.
*
*gw_head-plant = '1710'. "gw_vehicle-plant'.
**gw_head-material = '2023644H2AT501'.
    gw_head-salesorg = sales_order_input-salesorg.          " '1710'.
    gw_head-distr_chan = sales_order_input-distribution. "'10'.
    gw_head-division =  sales_order_input-division. " '00'.
    gw_head-doc_type = 'TA'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sales_order_input-soldtoparty
      IMPORTING
        output = gw_head-sold_to.

*    gw_head-purch_no = 'ABC1234'.
*    gw_head-payer = '0000002200'.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sales_order_input-endcustomer
      IMPORTING
        output = gw_head-endcustomer.


    gw_head-req_dat_h = sy-datum.
    gw_head-purch_no = sales_order_input-custrefrence.

    READ TABLE vehicle_data INTO DATA(ls_vehicle) INDEX 1.
    gw_head-vehicleidentnumb =  ls_vehicle-vehicleguid32.


*    gw_head-material_version = gw_vehicle-materialnumber_version.

    MOVE 'X' TO: gw_headx-plant,gw_headx-material,gw_headx-salesorg,
                 gw_headx-distr_chan,gw_headx-division,gw_headx-doc_type,
                 gw_headx-sold_to,gw_headx-endcustomer, gw_headx-req_dat_h,
                 gw_headx-vehicleidentnumb, gw_headx-purch_no.



    gw_item-vehicleguid = ls_vehicle-vehicleguid.
    gw_item-vehicleguid32 = ls_vehicle-vehicleguid32.
    gw_item-vehiclenumber = ls_vehicle-vehiclenumber.
    gw_item-endcustomer  = gw_head-endcustomer .
    gw_item-vehicleusage = '10'.
    APPEND gw_item TO gt_item.

    MOVE 'X' TO: gw_itemx-vehicleguid,gw_itemx-vehicleguid32,gw_itemx-vehiclenumber, gw_itemx-endcustomer, gw_itemx-vehicleusage.

    gw_ref-vehicleidentifier = ls_vehicle-vehicleguid32.

    gw_ref-config_id = '1'."    gw_config-config_id.
    gw_ref-inst_id   = '00000001'. "gw_config-root_id.
    APPEND gw_ref TO gt_ref.
    gw_id-vehicle_id_type = 'G'.
*BREAK-POINT.

    CALL FUNCTION 'BAPI_VEHICLE_CHANGE_MULTIPLE'
      EXPORTING
        vehicleaction        = 'CUOR'
        vehiactiondata_head  = gw_head
        requestedtablex      = ' X'
        vehiactiondata_headx = gw_headx
        vehiactiondata_itemx = gw_itemx
        vehicle_id_type      = gw_id
* IMPORTING
*       CHANGED_VEHIACTIONDATA_HEAD       =
      TABLES
        vehiactiondata_item  = gt_item
        config_references    = config_references
        config_data          = config_data
        config_instances     = config_inst
        config_charsvalues   = config_value
*       VEHICLE_QUALIFIERS   =
*       EXTENSIONIN          =
*       CHANGED_VEHIACTIONDATA_ITEM       =
*       CHANGED_VEHICLE      =
*       CHANGED_CONFIG_DATA  =
*       CHANGED_CONFIG_INSTANCE           =
*       CHANGED_CONFIG_CHARSVAL           =
*       EXTENSIONOUT         =
        return               = it_return
*       CONFIG_VARCONDNKEYS  =
*       CHANGED_CONFIG_VARCONDNKEYS       =
*       VEHICLE_DATA         =
      .
    READ TABLE it_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CALL METHOD lo_msg->add_messages_from_bapi
        EXPORTING
          it_bapi_messages = it_return.   " Return parameter table



      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>business_error
          message_container = lo_msg.

    ELSE.
*   Commit BAPI
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.

      READ TABLE it_return WITH KEY number  = '311' INTO DATA(wa_result).

      CONCATENATE 'Sales Order' '''' wa_result-message_v2 '''' 'created Successfully' INTO message .
      lo_msg->add_message_text_only(
              EXPORTING
                iv_msg_type               = 'S'
                iv_msg_text               = message
                iv_add_to_response_header = abap_true ).

      sales_order_input-docnumber = wa_result-message_v2  .

      MOVE-CORRESPONDING sales_order_input TO er_entity.

    ENDIF.



























*(아래 로직은 Configuration으로 SALES ORDER를 만드는 로직이고 Vehicle을 통해 sales order를 만드는 로직이 아니기에 폐기함


*    DATA : lo_msg     TYPE REF TO /iwbep/if_message_container,
*           ls_error   TYPE /iwbep/if_message_container=>ty_s_error_detail,
*           rt_message TYPE /iwbep/t_message_container.
*
*
*
*    CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
*      RECEIVING
*        ro_message_container = lo_msg.
*
*    CALL METHOD lo_msg->reset( ).
*
*    DATA message TYPE bapi_msg.
**
*    FIELD-SYMBOLS: <fs_fieldvalue> TYPE any,
*                   <fs_structure>  TYPE any.
*    DATA: name TYPE string.
*
*    DEFINE check_input_validation.
*
*      CLEAR name.
*      name = &1.
*
*       ASSIGN sales_order_input TO <fs_structure>.
*
*           ASSIGN COMPONENT name OF STRUCTURE <fs_structure>
*            TO  <fs_fieldvalue>.
*
*        IF <fs_fieldvalue> IS INITIAL.
*
*       CLEAR message.
*       CONCATENATE 'Enter' `'` &1 '`' 'Value'  INTO message .
*
*       CALL METHOD lo_msg->add_message_text_only
*       EXPORTING
*        iv_msg_type = 'E'
*        iv_msg_text = message.
*
*       ENDIF.
*    END-OF-DEFINITION.
*
*
*    check_input_validation : 'DOOR', 'DRIVE_TRAIN', 'EMISSION', 'ENGINE_TYPE',
*                              'FAMILY', 'MATERIAL'  , 'MODEL_YEAR', 'OPTION_GROUP',
*                              'SALESORG', 'SOLDTOPARTY', 'SERIES' , 'TRANSMISSION' ,'TRIM' .
*
*    rt_message = lo_msg->get_messages( iv_provide_text = '' ).
*
**” Raising Exception
*    IF rt_message  IS NOT INITIAL.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_msg.
*    ENDIF.
*
*    DATA: wa_order_header_in    TYPE  bapisdhd1,
*          wa_order_header_inx   TYPE bapisdhd1x,
*          i_order_items_in      TYPE TABLE OF bapisditm,
*          i_order_items_inx     TYPE TABLE OF bapisditmx,
*          i_order_schedules_in  TYPE TABLE OF bapischdl,
*          i_order_schedules_inx TYPE TABLE OF bapischdlx,
*          i_order_partners      TYPE TABLE OF bapiparnr,
*          i_order_cfgs_ref      TYPE TABLE OF  bapicucfg,
*          i_order_cfgs_inst     TYPE TABLE OF  bapicuins,
*          i_order_cfgs_value    TYPE TABLE OF  bapicuval,
*          l_sales               TYPE bapivbeln-vbeln,
*          i_return              TYPE TABLE OF bapiret2.
*
*
*    wa_order_header_in-doc_type    = 'TA'.    "Document Type
**    wa_order_header_in-purch_no_c  = 'VMS and BTP PoC'.  "PO Numer
*    wa_order_header_in-sales_org   = sales_order_input-salesorg.    "Sales Organization
**    wa_order_header_in-distr_chan  = sales_order_input-distribution.   "Dristibution Channel
**    wa_order_header_in-division    = sales_order_input-division.      "Division
*    wa_order_header_in-req_date_h  = sales_order_input-reqdate_h.   "Required Date
*
************** Populate order header flags,WA_ORDER_HEADER_INX
**    wa_order_header_inx-updateflag  = c_init.
*    wa_order_header_inx-sales_org   = 'X'.
**    wa_order_header_inx-distr_chan  = 'X'.
**    wa_order_header_inx-division    = 'X'.
**    wa_order_header_inx-purch_no_c  = 'X'.
*    wa_order_header_inx-req_date_h  = 'X'.
*
*    i_order_items_in = VALUE #( ( itm_number = '10'
*                                  po_itm_no = '10'
*                                  material   = sales_order_input-material ) ).
*
*
*    i_order_items_inx = VALUE #( ( itm_number = '10'
*                                   po_itm_no  = 'X'
*                                   material   = 'X'
*                                    ) ).
*
*    i_order_schedules_in = VALUE #( ( itm_number = '10'
*                                      req_qty    =  sales_order_input-quantity
*                                      sched_line = '1' ) ).
*
*    i_order_schedules_inx = VALUE #( ( itm_number = '10'
*                                        req_qty = 'X'
*                                        sched_line  =  '1' ) ).
*
*    i_order_cfgs_ref = VALUE #( ( posex = '10'
*                                  config_id = '000001'
*                                  root_id = '00000001'
*                                  complete = 'T'
*                                  consistent = 'T' ) ).
*
*    i_order_cfgs_inst = VALUE #( (
*                                  config_id     =  '000001'
*                                  inst_id       =  '00000001'
*                                  obj_type      =  'MARA'
*                                  class_type    =  '300'
*                                  obj_key       = sales_order_input-material
*                                  quantity      = sales_order_input-quantity
*                                  quantity_unit = 'EA'
*                                  complete = 'T'
*                                  consistent = 'T' ) ).
*
*
*    i_order_cfgs_value  = VALUE #( (  config_id = '000001'
*                                      inst_id = '00000001'
*                                      charc = 'DOOR'
*                                      value = sales_order_input-door )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'DRIVE_TRAIN'
*                                    value = sales_order_input-drive_train )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'EMISSION'
*                                    value = sales_order_input-emission )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'ENGINE_TYPE'
*                                    value = sales_order_input-engine_type )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'FAMILY'
*                                    value = sales_order_input-family )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'MODEL_YEAR'
*                                    value = sales_order_input-model_year )
*
*                                  (  config_id = '000001'
*                                     inst_id = '00000001'
*                                     charc = 'OPTION_GROUP'
*                                     value = sales_order_input-option_group )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'SERIES'
*                                    value = sales_order_input-series )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'TRANSMISSION'
*                                    value = sales_order_input-transmission )
*
*                                  ( config_id = '000001'
*                                    inst_id = '00000001'
*                                    charc = 'TRIM'
*                                    value = sales_order_input-trim ) ).
*
*
*
*
********************************* Populate partner details********************************
*
*    i_order_partners = VALUE #( ( partn_role = 'AG'
*                                  partn_numb = sales_order_input-soldtoparty ) ).
*
*
*    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
*      EXPORTING
*        order_header_in     = wa_order_header_in
*        order_header_inx    = wa_order_header_inx
*      IMPORTING
*        salesdocument       = l_sales
*      TABLES
*        return              = i_return
*        order_items_in      = i_order_items_in
*        order_items_inx     = i_order_items_inx
*        order_partners      = i_order_partners
*        order_schedules_in  = i_order_schedules_in
*        order_schedules_inx = i_order_schedules_inx
*        order_cfgs_ref      = i_order_cfgs_ref
*        order_cfgs_inst     = i_order_cfgs_inst
*        order_cfgs_value    = i_order_cfgs_value.
*
*
*    READ TABLE i_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      CALL METHOD lo_msg->add_messages_from_bapi
*        EXPORTING
*          it_bapi_messages = i_return.   " Return parameter table
*
*
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          textid            = /iwbep/cx_mgw_busi_exception=>business_error
*          message_container = lo_msg.
*
*    ELSE.
**   Commit BAPI
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*
*      CONCATENATE 'Sales Order' '`' l_sales '`' 'created Successfully' INTO message .
*      lo_msg->add_message_text_only(
*              EXPORTING
*                iv_msg_type               = 'S'
*                iv_msg_text               = message
*                iv_add_to_response_header = abap_true ).
*
*      sales_order_input-docnumber = l_sales.
*
*      MOVE-CORRESPONDING sales_order_input to er_entity.
*
*
*    ENDIF.  폐기 )


  ENDMETHOD.
ENDCLASS.
