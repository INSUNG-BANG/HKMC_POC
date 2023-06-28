class ZCL_ZHKMC_VMS_SEARCH_H_DPC_EXT definition
  public
  inheriting from ZCL_ZHKMC_VMS_SEARCH_H_DPC
  create public .

public section.
protected section.

  methods SOLDTOPARTY_GET_ENTITYSET
    redefinition .
  methods ZVEHICLEMATNRSET_GET_ENTITYSET
    redefinition .
  methods SALESORG_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHKMC_VMS_SEARCH_H_DPC_EXT IMPLEMENTATION.


  method SALESORG_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->SALESORG_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.

    select * INTO TABLE ET_ENTITYSET from  TVKOT WHERE vkorg = '1710' and SPRAS = 'E' .
  endmethod.


  method SOLDTOPARTY_GET_ENTITYSET.
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
DATA lo_filter TYPE  REF TO /iwbep/if_mgw_req_filter.
DATA lt_filter_select_options TYPE /iwbep/t_mgw_select_option.
DATA lv_filter_str TYPE string.
DATA lv_max_hits TYPE i.
DATA ls_paging TYPE /iwbep/s_mgw_paging.
DATA ls_converted_keys LIKE LINE OF et_entityset.
DATA ls_message TYPE bapiret2.
DATA lt_selopt TYPE ddshselops.
DATA ls_selopt LIKE LINE OF lt_selopt.
DATA ls_filter TYPE /iwbep/s_mgw_select_option.
DATA ls_filter_range TYPE /iwbep/s_cod_select_option.
DATA lr_loevm LIKE RANGE OF ls_converted_keys-loevm.
DATA ls_loevm LIKE LINE OF lr_loevm.
DATA lr_kunnr LIKE RANGE OF ls_converted_keys-kunnr.
DATA ls_kunnr LIKE LINE OF lr_kunnr.
DATA lr_mcod1 LIKE RANGE OF ls_converted_keys-mcod1.
DATA ls_mcod1 LIKE LINE OF lr_mcod1.
DATA lr_mcod3 LIKE RANGE OF ls_converted_keys-mcod3.
DATA ls_mcod3 LIKE LINE OF lr_mcod3.
DATA lr_pstlz LIKE RANGE OF ls_converted_keys-pstlz.
DATA ls_pstlz LIKE LINE OF lr_pstlz.
DATA lr_land1 LIKE RANGE OF ls_converted_keys-land1.
DATA ls_land1 LIKE LINE OF lr_land1.
DATA lr_sortl LIKE RANGE OF ls_converted_keys-sortl.
DATA ls_sortl LIKE LINE OF lr_sortl.
DATA lt_result_list TYPE /iwbep/if_sb_gendpc_shlp_data=>tt_result_list.
DATA lv_next TYPE i VALUE 1.
DATA ls_entityset LIKE LINE OF et_entityset.
DATA ls_result_list_next LIKE LINE OF lt_result_list.
DATA ls_result_list LIKE LINE OF lt_result_list.

*-------------------------------------------------------------
*  Map the runtime request to the Search Help select option - Only mapped attributes
*-------------------------------------------------------------
* Get all input information from the technical request context object
* Since DPC works with internal property names and runtime API interface holds external property names
* the process needs to get the all needed input information from the technical request context object
* Get filter or select option information
lo_filter = io_tech_request_context->get_filter( ).
lt_filter_select_options = lo_filter->get_filter_select_options( ).
lv_filter_str = lo_filter->get_filter_string( ).

* Check if the supplied filter is supported by standard gateway runtime process
IF  lv_filter_str            IS NOT INITIAL
AND lt_filter_select_options IS INITIAL.
  " If the string of the Filter System Query Option is not automatically converted into
  " filter option table (lt_filter_select_options), then the filtering combination is not supported
  " Log message in the application log
  me->/iwbep/if_sb_dpc_comm_services~log_message(
    EXPORTING
      iv_msg_type   = 'E'
      iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
      iv_msg_number = 025 ).
  " Raise Exception
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
    EXPORTING
      textid = /iwbep/cx_mgw_tech_exception=>internal_error.
ENDIF.

* Get key table information
io_tech_request_context->get_converted_source_keys(
  IMPORTING
    es_key_values  = ls_converted_keys ).

ls_paging-top = io_tech_request_context->get_top( ).
ls_paging-skip = io_tech_request_context->get_skip( ).

" Calculate the number of max hits to be fetched from the function module
" The lv_max_hits value is a summary of the Top and Skip values
IF ls_paging-top > 0.
  lv_max_hits = is_paging-top + is_paging-skip.
ENDIF.

* Maps filter table lines to the Search Help select option table
LOOP AT lt_filter_select_options INTO ls_filter.

  CASE ls_filter-property.
    WHEN 'LOEVM'.              " Equivalent to 'Loevm' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_loevm ).

      LOOP AT lr_loevm INTO ls_loevm.
        ls_selopt-sign = ls_loevm-sign.
        ls_selopt-option = ls_loevm-option.
        ls_selopt-low = ls_loevm-low.
        ls_selopt-high = ls_loevm-high.
        ls_selopt-shlpfield = 'LOEVM'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'KUNNR'.              " Equivalent to 'Kunnr' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_kunnr ).

      LOOP AT lr_kunnr INTO ls_kunnr.
        ls_selopt-sign = ls_kunnr-sign.
        ls_selopt-option = ls_kunnr-option.
        ls_selopt-low = ls_kunnr-low.
        ls_selopt-high = ls_kunnr-high.
        ls_selopt-shlpfield = 'KUNNR'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'MCOD1'.              " Equivalent to 'Mcod1' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_mcod1 ).

      LOOP AT lr_mcod1 INTO ls_mcod1.
        ls_selopt-sign = ls_mcod1-sign.
        ls_selopt-option = ls_mcod1-option.
        ls_selopt-low = ls_mcod1-low.
        ls_selopt-high = ls_mcod1-high.
        ls_selopt-shlpfield = 'MCOD1'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'MCOD3'.              " Equivalent to 'Mcod3' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_mcod3 ).

      LOOP AT lr_mcod3 INTO ls_mcod3.
        ls_selopt-sign = ls_mcod3-sign.
        ls_selopt-option = ls_mcod3-option.
        ls_selopt-low = ls_mcod3-low.
        ls_selopt-high = ls_mcod3-high.
        ls_selopt-shlpfield = 'MCOD3'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'PSTLZ'.              " Equivalent to 'Pstlz' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_pstlz ).

      LOOP AT lr_pstlz INTO ls_pstlz.
        ls_selopt-sign = ls_pstlz-sign.
        ls_selopt-option = ls_pstlz-option.
        ls_selopt-low = ls_pstlz-low.
        ls_selopt-high = ls_pstlz-high.
        ls_selopt-shlpfield = 'PSTLZ'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'LAND1'.              " Equivalent to 'Land1' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_land1 ).

      LOOP AT lr_land1 INTO ls_land1.
        ls_selopt-sign = ls_land1-sign.
        ls_selopt-option = ls_land1-option.
        ls_selopt-low = ls_land1-low.
        ls_selopt-high = ls_land1-high.
        ls_selopt-shlpfield = 'LAND1'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'SORTL'.              " Equivalent to 'Sortl' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_sortl ).

      LOOP AT lr_sortl INTO ls_sortl.
        ls_selopt-sign = ls_sortl-sign.
        ls_selopt-option = ls_sortl-option.
        ls_selopt-low = ls_sortl-low.
        ls_selopt-high = ls_sortl-high.
        ls_selopt-shlpfield = 'SORTL'.
        ls_selopt-shlpname = 'DEBIA'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.

    WHEN OTHERS.
      " Log message in the application log
      me->/iwbep/if_sb_dpc_comm_services~log_message(
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
          iv_msg_number = 020
          iv_msg_v1     = ls_filter-property ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
  ENDCASE.
ENDLOOP.

*-------------------------------------------------------------
*  Call to Search Help get values mechanism
*-------------------------------------------------------------
* Get search help values
me->/iwbep/if_sb_gendpc_shlp_data~get_search_help_values(
  EXPORTING
    iv_shlp_name = 'DEBIA'
    iv_maxrows = lv_max_hits
    iv_sort = 'X'
    iv_call_shlt_exit = 'X'
    it_selopt = lt_selopt
  IMPORTING
    et_return_list = lt_result_list
    es_message = ls_message ).

*-------------------------------------------------------------
*  Map the Search Help returned results to the caller interface - Only mapped attributes
*-------------------------------------------------------------
IF ls_message IS NOT INITIAL.
* Call RFC call exception handling
  me->/iwbep/if_sb_dpc_comm_services~rfc_save_log(
    EXPORTING
      is_return      = ls_message
      iv_entity_type = iv_entity_name
      it_key_tab     = it_key_tab ).
ENDIF.

CLEAR et_entityset.

LOOP AT lt_result_list INTO ls_result_list
  WHERE record_number > ls_paging-skip.

  " Move SH results to GW request responce table
  lv_next = sy-tabix + 1. " next loop iteration
  CASE ls_result_list-field_name.
    WHEN 'KUNNR'.
      ls_entityset-kunnr = ls_result_list-field_value.
    WHEN 'LAND1'.
      ls_entityset-land1 = ls_result_list-field_value.
    WHEN 'LOEVM'.
      ls_entityset-loevm = ls_result_list-field_value.
    WHEN 'MCOD1'.
      ls_entityset-mcod1 = ls_result_list-field_value.
    WHEN 'MCOD3'.
      ls_entityset-mcod3 = ls_result_list-field_value.
    WHEN 'PSTLZ'.
      ls_entityset-pstlz = ls_result_list-field_value.
    WHEN 'SORTL'.
      ls_entityset-sortl = ls_result_list-field_value.
  ENDCASE.

  " Check if the next line in the result list is a new record
  READ TABLE lt_result_list INTO ls_result_list_next INDEX lv_next.
  IF sy-subrc <> 0
  OR ls_result_list-record_number <> ls_result_list_next-record_number.
    " Save the collected SH result in the GW request table
    APPEND ls_entityset TO et_entityset.
    CLEAR: ls_result_list_next, ls_entityset.
  ENDIF.

ENDLOOP.

delete et_entityset where kunnr NE '0017100001'.

  endmethod.


  method ZVEHICLEMATNRSET_GET_ENTITYSET.
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
DATA lo_filter TYPE  REF TO /iwbep/if_mgw_req_filter.
DATA lt_filter_select_options TYPE /iwbep/t_mgw_select_option.
DATA lv_filter_str TYPE string.
DATA lv_max_hits TYPE i.
DATA ls_paging TYPE /iwbep/s_mgw_paging.
DATA ls_converted_keys LIKE LINE OF et_entityset.
DATA ls_message TYPE bapiret2.
DATA lt_selopt TYPE ddshselops.
DATA ls_selopt LIKE LINE OF lt_selopt.
DATA ls_filter TYPE /iwbep/s_mgw_select_option.
DATA ls_filter_range TYPE /iwbep/s_cod_select_option.
DATA lr_maktx LIKE RANGE OF ls_converted_keys-maktx.
DATA ls_maktx LIKE LINE OF lr_maktx.
DATA lr_matnr LIKE RANGE OF ls_converted_keys-matnr.
DATA ls_matnr LIKE LINE OF lr_matnr.
DATA lt_result_list TYPE /iwbep/if_sb_gendpc_shlp_data=>tt_result_list.
DATA lv_next TYPE i VALUE 1.
DATA ls_entityset LIKE LINE OF et_entityset.
DATA ls_result_list_next LIKE LINE OF lt_result_list.
DATA ls_result_list LIKE LINE OF lt_result_list.

*-------------------------------------------------------------
*  Map the runtime request to the Search Help select option - Only mapped attributes
*-------------------------------------------------------------
* Get all input information from the technical request context object
* Since DPC works with internal property names and runtime API interface holds external property names
* the process needs to get the all needed input information from the technical request context object
* Get filter or select option information
lo_filter = io_tech_request_context->get_filter( ).
lt_filter_select_options = lo_filter->get_filter_select_options( ).
lv_filter_str = lo_filter->get_filter_string( ).

* Check if the supplied filter is supported by standard gateway runtime process
IF  lv_filter_str            IS NOT INITIAL
AND lt_filter_select_options IS INITIAL.
  " If the string of the Filter System Query Option is not automatically converted into
  " filter option table (lt_filter_select_options), then the filtering combination is not supported
  " Log message in the application log
  me->/iwbep/if_sb_dpc_comm_services~log_message(
    EXPORTING
      iv_msg_type   = 'E'
      iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
      iv_msg_number = 025 ).
  " Raise Exception
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
    EXPORTING
      textid = /iwbep/cx_mgw_tech_exception=>internal_error.
ENDIF.

* Get key table information
io_tech_request_context->get_converted_source_keys(
  IMPORTING
    es_key_values  = ls_converted_keys ).

ls_paging-top = io_tech_request_context->get_top( ).
ls_paging-skip = io_tech_request_context->get_skip( ).

" Calculate the number of max hits to be fetched from the function module
" The lv_max_hits value is a summary of the Top and Skip values
IF ls_paging-top > 0.
  lv_max_hits = is_paging-top + is_paging-skip.
ENDIF.

* Maps filter table lines to the Search Help select option table
LOOP AT lt_filter_select_options INTO ls_filter.

  CASE ls_filter-property.
    WHEN 'MAKTX'.              " Equivalent to 'Maktx' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_maktx ).

      LOOP AT lr_maktx INTO ls_maktx.
        ls_selopt-high = ls_maktx-high.
        ls_selopt-low = ls_maktx-low.
        ls_selopt-option = ls_maktx-option.
        ls_selopt-sign = ls_maktx-sign.
        ls_selopt-shlpfield = 'MAKTX'.
        ls_selopt-shlpname = 'Z_VEHICLE_MATNR'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.
    WHEN 'MATNR'.              " Equivalent to 'Matnr' property in the service
      lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lr_matnr ).

      LOOP AT lr_matnr INTO ls_matnr.
        ls_selopt-high = ls_matnr-high.
        ls_selopt-low = ls_matnr-low.
        ls_selopt-option = ls_matnr-option.
        ls_selopt-sign = ls_matnr-sign.
        ls_selopt-shlpfield = 'MATNR'.
        ls_selopt-shlpname = 'Z_VEHICLE_MATNR'.
        APPEND ls_selopt TO lt_selopt.
        CLEAR ls_selopt.
      ENDLOOP.

    WHEN OTHERS.
      " Log message in the application log
      me->/iwbep/if_sb_dpc_comm_services~log_message(
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
          iv_msg_number = 020
          iv_msg_v1     = ls_filter-property ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
  ENDCASE.
ENDLOOP.

*-------------------------------------------------------------
*  Call to Search Help get values mechanism
*-------------------------------------------------------------
* Get search help values
me->/iwbep/if_sb_gendpc_shlp_data~get_search_help_values(
  EXPORTING
    iv_shlp_name = 'Z_VEHICLE_MATNR'
    iv_maxrows = lv_max_hits
    iv_sort = 'X'
    iv_call_shlt_exit = 'X'
    it_selopt = lt_selopt
  IMPORTING
    et_return_list = lt_result_list
    es_message = ls_message ).

*-------------------------------------------------------------
*  Map the Search Help returned results to the caller interface - Only mapped attributes
*-------------------------------------------------------------
IF ls_message IS NOT INITIAL.
* Call RFC call exception handling
  me->/iwbep/if_sb_dpc_comm_services~rfc_save_log(
    EXPORTING
      is_return      = ls_message
      iv_entity_type = iv_entity_name
      it_key_tab     = it_key_tab ).
ENDIF.

CLEAR et_entityset.

LOOP AT lt_result_list INTO ls_result_list
  WHERE record_number > ls_paging-skip.

  " Move SH results to GW request responce table
  lv_next = sy-tabix + 1. " next loop iteration
  CASE ls_result_list-field_name.
    WHEN 'MAKTX'.
      ls_entityset-maktx = ls_result_list-field_value.
    WHEN 'MATNR'.
      ls_entityset-matnr = ls_result_list-field_value.
  ENDCASE.

  " Check if the next line in the result list is a new record
  READ TABLE lt_result_list INTO ls_result_list_next INDEX lv_next.
  IF sy-subrc <> 0
  OR ls_result_list-record_number <> ls_result_list_next-record_number.
    " Save the collected SH result in the GW request table
    APPEND ls_entityset TO et_entityset.
    CLEAR: ls_result_list_next, ls_entityset.
  ENDIF.

ENDLOOP.


    delete ADJACENT DUPLICATES FROM et_entityset.

  endmethod.
ENDCLASS.
