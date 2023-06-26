*&---------------------------------------------------------------------*
*& Report ZHKMC_SO_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHKMC_SO_TEST.

DATA:  wa_order_header_in TYPE  BAPISDHD1,
       wa_order_header_inx TYPE bapisdhd1x,
       I_order_items_in type bapisditm OCCURS 0 WITH HEADER LINE,
       i_order_items_inx type bapisditmx OCCURS 0 WITH HEADER LINE,
       i_order_schedules_in TYPE STANDARD TABLE OF bapischdl WITH HEADER LINE,
       i_order_schedules_inx TYPE STANDARD TABLE OF bapischdlx WITH HEADER LINE,
       i_order_partners TYPE BAPIPARNR  occurs 0 with header line,
       i_ORDER_CFGS_REF LIKE  BAPICUCFG occurs 0 WITH HEADER LINE,
       i_ORDER_CFGS_INST  LIKE  BAPICUINS occurs 0 WITH HEADER LINE,
       i_ORDER_CFGS_VALUE LIKE  BAPICUVAL occurs 0 WITH HEADER LINE,
       L_SALES TYPE BAPIVBELN-VBELN,
       i_return type BAPIRET2  occurs 0 WITH HEADER LINE.


*PARAMETERS : PUR_NO TYPE BAPISDHD1-purch_no_c,
*             SALES_ORG TYPE BAPISDHD1-sales_org ,
*             distr_chan


    wa_order_header_in-doc_type    = 'TA'.    "Document Type
    wa_order_header_in-purch_no_c  = 'VMS and BTP PoC'.  "PO Numer
    wa_order_header_in-sales_org   = '1710'.    "Sales Organization
    wa_order_header_in-distr_chan  = '10'.   "Dristibution Channel
    wa_order_header_in-division    = '00'.      "Division
    wa_order_header_in-req_date_h  = sy-datum.   "Required Date

************* Populate order header flags,WA_ORDER_HEADER_INX
*    wa_order_header_inx-updateflag  = c_init.
    wa_order_header_inx-sales_org   = 'X'.
    wa_order_header_inx-distr_chan  = 'X'.
    wa_order_header_inx-division    = 'X'.
    wa_order_header_inx-purch_no_c  = 'X'.
    wa_order_header_inx-req_date_h  = 'X'.

******************** Populate Order item details and flags*****************


*loop at

      i_order_items_in-itm_number = '10'.                         "Item Number
      i_order_items_in-po_itm_no  = '10'.                         "POSEX required entry during configuration
      i_order_items_in-material   = '2023644H2AT501'.                  "Populate material number
      i_order_items_in-target_qty = '1'.
      APPEND i_order_items_in .
      CLEAR i_order_items_in.

      i_order_items_inx-itm_number = '10'.                      "Item Number
      i_order_items_inx-po_itm_no  = 'X'.
      i_order_items_inx-material   = 'X'.                          "Populate material update flag
      i_order_items_INX-target_qty = 'X'.
      APPEND i_order_items_inx.
      CLEAR i_order_items_inx.

*      i_order_schedules_in-itm_number = '10'.
*      i_order_schedules_in-req_qty    = '1'.                  "Populate quantity
*      i_order_schedules_in-SCHED_LINE = '1'.
*      APPEND i_order_schedules_in TO i_order_schedules_in.
*      CLEAR i_order_schedules_in.
*
*      i_order_schedules_inx-itm_number = '10'.
*      i_order_schedules_inx-req_qty = 'X'.                         "Populate quantity flag
*      i_order_schedules_inx-SCHED_LINE  =  '1'.
*      APPEND i_order_schedules_inx TO i_order_schedules_inx.
*      CLEAR i_order_schedules_inx.


*********Filling ORDER_CFGS_REF  of type BAPICUCFG with work area i_order_cfgS_ref
      i_order_cfgS_ref-posex = '10'.
      i_order_cfgS_ref-config_id = '000001'.
      i_order_cfgS_ref-root_id = '00000001'.
      i_order_cfgS_ref-COMPLETE = 'T'.
      i_order_cfgS_ref-CONSISTENT = 'T'.
      APPEND i_order_cfgS_ref TO i_order_cfgS_ref.
      CLEAR i_order_cfgS_ref.

***ORDER_CFGS_INST  of type BAPICUINS with work area i_ORDER_CFGS_INST
      i_ORDER_CFGS_INST-config_id     =  '000001'.                              "External configuration ID (temporary)
      i_ORDER_CFGS_INST-inst_id       =  '00000001'.                              "Instance number in the configuration
      i_ORDER_CFGS_INST-obj_type      =  'MARA'.                           "Object type
      i_ORDER_CFGS_INST-class_type    =  '300'.                            "Class type
      i_ORDER_CFGS_INST-obj_key       = '2023644H2AT501'.       " Object key
      i_ORDER_CFGS_INST-quantity      = '1'.     "Instance Quantity
      i_ORDER_CFGS_INST-QUANTITY_UNIT = 'EA'.
      i_ORDER_CFGS_INST-COMPLETE = 'T'.
      i_ORDER_CFGS_INST-CONSISTENT = 'T'.
      APPEND i_ORDER_CFGS_INST TO i_ORDER_CFGS_INST.
      CLEAR i_ORDER_CFGS_INST.

*********ORDER_CFGS_VALUE  of type BAPICUVAL with work area i_ORDER_CFGS_VALUE

      CLEAR i_ORDER_CFGS_VALUE.
      i_ORDER_CFGS_VALUE-config_id = '000001'.                             "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                              "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'DOOR'.                  "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '4' .                                  "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                     "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
      CLEAR i_ORDER_CFGS_VALUE.


      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'DRIVE_TRAIN'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = 'A'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'EMISSION'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '5'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'ENGINE_TYPE'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = 'T'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.


      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'FAMILY'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '4'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'MODEL_YEAR'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '2023'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'OPTION_GROUP'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '1'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'SERIES'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '6'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

      i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'TRANSMISSION'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = '2'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.

          i_ORDER_CFGS_VALUE-config_id = '000001'.                          "External configuration ID (temporary)
      i_ORDER_CFGS_VALUE-inst_id = '00000001'.                          "Instance number in the configuration
      i_ORDER_CFGS_VALUE-charc = 'TRIM'.               "Characteristic Name
      i_ORDER_CFGS_VALUE-value = 'H'.                              "Characteristic Value
      i_ORDER_CFGS_VALUE-VALCODE = '1'.                                  "Value Type: Interval Limits - Single Values
      APPEND i_ORDER_CFGS_VALUE TO i_ORDER_CFGS_VALUE.
       CLEAR i_ORDER_CFGS_VALUE.
*    ENDLOOP.

******************************** Populate partner details********************************

    i_order_partners-partn_role = 'WE'.
    i_order_partners-partn_numb = '0017100001'.

    APPEND i_order_partners TO i_order_partners.
    CLEAR i_order_partners.

*   i_order_partners-partn_role = 'AG'.
*    i_order_partners-partn_numb = '0017100001'.
*
*    APPEND i_order_partners TO i_order_partners.
*    CLEAR i_order_partners.



********Create a sales order using BAPI_SALESORDER_CREATEFROMDAT2***********

CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
    ORDER_HEADER_IN               = WA_ORDER_HEADER_IN
    ORDER_HEADER_INX              = WA_ORDER_HEADER_INX
  IMPORTING
    SALESDOCUMENT                 = L_SALES
  TABLES
    RETURN                        = I_RETURN
    ORDER_ITEMS_IN                = I_ORDER_ITEMS_IN
    ORDER_ITEMS_INX               = I_ORDER_ITEMS_INX
    ORDER_PARTNERS                = I_ORDER_PARTNERS
    ORDER_SCHEDULES_IN            = I_ORDER_SCHEDULES_IN
    ORDER_SCHEDULES_INX           = I_ORDER_SCHEDULES_INX
    ORDER_CFGS_REF                = i_order_cfgS_ref
    ORDER_CFGS_INST               = i_ORDER_CFGS_INST
    ORDER_CFGS_VALUE              = i_ORDER_CFGS_VALUE.
*    ORDER_CFGS_VK                 = I_CFG_VK
*    ORDER_CFGS_REFINST            = I_CFG_REFINST.

READ TABLE i_return with key TYPE = 'E'.
IF SY-SUBRC = 0.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*   IMPORTING
*     RETURN        =
            .
       BREAK-POINT.

  ELSE.
*   Commit BAPI
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
      WAIT = 'X'.
      write : l_sales , 'created'.
      ENDIF.
*
*      CUXI_INITIALIZER
*      SD_SALES_DOCUMENT_INIT
*      RV_MESSAGES_REFRESH
