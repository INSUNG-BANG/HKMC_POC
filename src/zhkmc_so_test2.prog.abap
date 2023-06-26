*&---------------------------------------------------------------------*
*& Report ZHKMC_SO_TEST2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhkmc_so_test2.

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

PARAMETER: p_vguid TYPE vlcvehicle-vguid.

SELECT vguid FROM vlcvehicle INTO TABLE @DATA(it_vehicle) WHERE vguid = @p_vguid. .

CALL FUNCTION 'BAPI_VEHICLE_GETLIST'
  EXPORTING
    requestedtablex   = 'XX'
  TABLES
    vehicleguid       = it_vehicle
*   VEHICLEGUID32     =
*   VEHICLENUMBER     =
*   VEHICLECUOBJ      =
*   VEHICLEIDENTNUMB  =
*   VEHICLEEXTERNNUM  =
*   VEHICLEADDPARAMS  =
    vehicledata       = vehicle_data
    config_references = config_references
    config_data       = config_data
    config_instance   = config_inst
*   CONFIG_INFO       =
    config_charsval   = config_value
*   CONFIG_KEY        =
*   VEHICLE_QUALIFIERS       =
*   EXTENSIONOUT      =
*   RETURN            = it_return
  .
*MOVE-CORRESPONDING vehicle_Data to gw_head.
*MOve-CORRESPONDING vehicle_Data[] to gt_item[].

*gw_head-plant = '1710'. "gw_vehicle-plant'.
*gw_head-material = '2023644H2AT501'.
gw_head-salesorg = '1710'.
gw_head-distr_chan = '10'.
gw_head-division = '00'.
gw_head-doc_type = 'OR'.
gw_head-sold_to = '0017100001'.
*    gw_head-purch_no = 'ABC1234'.
*    gw_head-payer = '0000002200'.
gw_head-endcustomer = '0001000251'.
gw_head-req_dat_h = sy-datum.
READ TABLE vehicle_data INTO DATA(ls_vehicle) INDEX 1.
*gw_head-vehicleidentnumb =  ls_vehicle-vehicleguid32.

*    gw_head-material_version = gw_vehicle-materialnumber_version.

MOVE 'X' TO: gw_headx-plant,gw_headx-material,gw_headx-salesorg,
             gw_headx-distr_chan,gw_headx-division,gw_headx-doc_type,
             gw_headx-sold_to,gw_headx-endcustomer, gw_headx-req_dat_h,
             gw_headx-vehicleidentnumb.



gw_item-vehicleguid = ls_vehicle-vehicleguid.
*gw_item-vehicleguid32 = ls_vehicle-vehicleguid32.
gw_item-vehiclenumber = ls_vehicle-vehiclenumber.
gw_item-endcustomer  = '0001000251'.
gw_item-vehicleusage = '10'.
APPEND gw_item TO gt_item.

MOVE 'X' TO: gw_itemx-vehicleguid,gw_itemx-vehicleguid32,gw_itemx-vehiclenumber, gw_itemx-endcustomer, gw_itemx-vehicleusage.
*                     gw_itemx-po_number.
*  READ TABLE gt_qual INTO gw_qual INDEX 1.
gw_ref-vehicleidentifier = ls_vehicle-vehicleguid32.
*    READ TABLE gt_config1 INTO gw_config INDEX 1.
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
*   CHANGED_VEHIACTIONDATA_HEAD       =
  TABLES
    vehiactiondata_item  = gt_item
    config_references    = config_references
    config_data          = config_data
    config_instances     = config_inst
    config_charsvalues   = config_value
*   VEHICLE_QUALIFIERS   =
*   EXTENSIONIN          =
*   CHANGED_VEHIACTIONDATA_ITEM       =
*   CHANGED_VEHICLE      =
*   CHANGED_CONFIG_DATA  =
*   CHANGED_CONFIG_INSTANCE           =
*   CHANGED_CONFIG_CHARSVAL           =
*   EXTENSIONOUT         =
    return               = it_return
*   CONFIG_VARCONDNKEYS  =
*   CHANGED_CONFIG_VARCONDNKEYS       =
*   VEHICLE_DATA         =
  .
READ TABLE it_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*   IMPORTING
*     RETURN        =
    .
  BREAK-POINT.

ELSE.
  READ TABLE it_return WITH KEY NUMBER  = '311' INTO DATA(wa_result).
*   Commit BAPI
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
  WRITE : wa_result-message_v2.
ENDIF.
