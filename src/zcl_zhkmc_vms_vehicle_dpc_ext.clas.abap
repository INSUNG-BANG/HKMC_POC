class ZCL_ZHKMC_VMS_VEHICLE_DPC_EXT definition
  public
  inheriting from ZCL_ZHKMC_VMS_VEHICLE_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZHKMC_VMS_VEHICLE_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
**  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
**    it_key_tab              =
**    it_navigation_path      =
**    io_tech_request_context =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_filter_select_options =
**    it_order                 =
**    is_paging                =
**    it_navigation_path       =
**    it_key_tab               =
**    iv_filter_string         =
**    iv_search_string         =
**    io_tech_request_context  =
**  IMPORTING
**    er_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_key_tab               =
**    it_navigation_path       =
**    io_expand                =
**    io_tech_request_context  =
**  IMPORTING
**    er_entity                =
**    es_response_context      =
**    et_expanded_clauses      =
**    et_expanded_tech_clauses =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
  endmethod.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.

    DATA : lt_deep_entity TYPE TABLE OF zcl_zhkmc_vms_vehicle_mpc_ext=>ty_deep_entity, "Deep Entity Type
               ls_deep_entity TYPE zcl_zhkmc_vms_vehicle_mpc_ext=>ty_deep_entity.

    DATA: config_references TYPE TABLE OF  bapiconfig_references,
          config_value      TYPE TABLE OF   bapicuval,
          vehicle_data      TYPE TABLE OF bapivehicle,
          it_return         TYPE TABLE OF  bapiret2.

    IF iv_entity_set_name = 'VEHICLE_DATA'.

      SELECT vguid FROM vlcvehicle INTO TABLE @DATA(it_vehicle).

      CALL FUNCTION 'BAPI_VEHICLE_GETLIST'
        EXPORTING
          requestedtablex   = 'XX'
        TABLES
          vehicleguid       = it_vehicle
*         VEHICLEGUID32     =
*         VEHICLENUMBER     =
*         VEHICLECUOBJ      =
*         VEHICLEIDENTNUMB  =
*         VEHICLEEXTERNNUM  =
*         VEHICLEADDPARAMS  =
          vehicledata       = vehicle_data
          config_references = config_references
*         CONFIG_DATA       =
*         CONFIG_INSTANCE   =
*         CONFIG_INFO       =
          config_charsval   = config_value
*         CONFIG_KEY        =
*         VEHICLE_QUALIFIERS       =
*         EXTENSIONOUT      =
          return            = it_return.


      READ TABLE it_return ASSIGNING FIELD-SYMBOL(<fs_message>) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        DATA: message TYPE string.
        CONCATENATE 'Error Message: ' <fs_message>-message INTO message.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
            message_unlimited = message.
        EXIT.
      ENDIF.

 loop at vehicle_data ASSIGNING FIELD-SYMBOL(<fs_vehicle>).

   LS_DEEP_ENTITY = CORRESPONDING #( <fs_vehicle> ) .

   LS_DEEP_ENTITY-VehicleToConfig = VALUE #( FOR <FS_CONFIG_REF> IN config_references WHERE ( VEHICLEIDENTIFIER = <fs_vehicle>-vehicleguid32 )
                                                                         ( vehicleidentifier = <FS_CONFIG_REF>-vehicleidentifier
                                                                           CONFIG_ID = <FS_CONFIG_REF>-config_id
                                                                           INST_ID = <FS_CONFIG_REF>-inst_id
                                                                           configtovalue = VALUE #( FOR <FS_CONFIG_VALUE> IN CONFIG_VALUE WHERE ( config_id = <FS_CONFIG_REF>-config_id AND inst_id = <FS_CONFIG_REF>-inst_id )
                                                                                                               ( config_id = <fs_config_value>-config_id
                                                                                                                 inst_id = <fs_config_value>-inst_id
                                                                                                                 charc = <fs_config_value>-charc
                                                                                                                 charc_txt = <fs_config_value>-charc_txt
                                                                                                                 value = <fs_config_value>-value
                                                                                                                 VALUE_TXT = <fs_config_value>-value_txt )

                                                                                          ) ) ).

    APPEND ls_deep_entity TO lt_deep_entity.
             clear ls_deep_entity.

endloop.


      CALL METHOD /iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref
        EXPORTING
          is_data = lt_deep_entity
        CHANGING
          cr_data = er_entityset.
    ENDIF.



  ENDMETHOD.
ENDCLASS.
