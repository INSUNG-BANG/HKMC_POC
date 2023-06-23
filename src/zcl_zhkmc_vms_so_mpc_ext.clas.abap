class ZCL_ZHKMC_VMS_SO_MPC_EXT definition
  public
  inheriting from ZCL_ZHKMC_VMS_SO_MPC
  create public .

public section.


types : begin of ty_Configurations.

         include type  ts_so_config_inst.
 TYPES : POSEX type C length 6,
         ConfigToValue type table of ts_so_config_value WITH DEFAULT KEY.
TYPEs: end of ty_Configurations.

TYPES : BEGIN OF TY_HeaderToItem.
           INCLUDE TYPE TS_SO_ITEM.
 TYPES:  ItemToPartner    type TABLE OF ts_so_partners WITH DEFAULT KEY,
         ItemToSchedule   TYPE TABLE OF ts_so_schedules WITH DEFAULT KEY,
         ItemToConfig     Type TABLE OF ty_Configurations WITH DEFAULT KEY,
         end of TY_HeaderToItem.

types:
    BEGIN OF ty_deep_entity,
             doc_number           TYPE ts_so_header-doc_number,
             REC_DATE             type ts_so_header-rec_date,
             REC_TIME             TYPE ts_so_header-rec_time,
             created_by           TYPE ts_so_header-created_by,
             doc_type             TYPE ts_so_header-doc_type,
             sales_org            TYPE ts_so_header-sales_org,
             distr_chan           TYPE ts_so_header-distr_chan,
             division             TYPE ts_so_header-division,
             sales_grp            TYPE ts_so_header-sales_grp,
             sales_off            TYPE ts_so_header-sales_off,
             req_date_h           TYPE ts_so_header-req_date_h,
             purch_no             TYPE ts_so_header-purch_no,
             HeaderToItem         TYPE TABLE OF TY_HeaderToItem WITH DEFAULT KEY,
           END OF ty_deep_entity .

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZHKMC_VMS_SO_MPC_EXT IMPLEMENTATION.


  method DEFINE.


         super->define( ).
 	 DATA:
*      lo_annotation     TYPE REF TO /iwbep/if_mgw_odata_annotation,
     lo_entity_type    TYPE REF TO /iwbep/if_mgw_odata_entity_typ.
*     lo_complex_type   TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,
*      lo_property       TYPE REF TO /iwbep/if_mgw_odata_property,
*      lo_entity_set     TYPE REF TO /iwbep/if_mgw_odata_entity_set.


* ******************************************************************  *****************************************************************
*	*   ENTITY - Deep Entity
* ******************************************************************  *****************************************************************

 	 lo_entity_type = model->get_entity_type( iv_entity_name = 'SO_HEADER' ). "#EC NOTEXT
   lo_entity_type->bind_structure( iv_structure_name  =  'ZCL_ZHKMC_VMS_SO_MPC_EXT=>TY_DEEP_ENTITY' )."#EC NOTEXT
  endmethod.
ENDCLASS.
