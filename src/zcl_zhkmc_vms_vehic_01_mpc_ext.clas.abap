class ZCL_ZHKMC_VMS_VEHIC_01_MPC_EXT definition
  public
  inheriting from ZCL_ZHKMC_VMS_VEHIC_01_MPC
  create public .

public section.


TYPES begin of ty_config.
  INCLUDE TYPE ts_config_ref.
  types: ConfigToValue TYPE TABLE OF ts_config_values WITH DEFAULT KEY,
  end of ty_config.

  types:
    BEGIN OF TY_DEEP_ENTITY.
     iNCLUDE type ts_vehicle_data.
     TYPES : VehicleToConfig type TABLE of ty_config WITH DEFAULT KEY,
      END OF TY_DEEP_ENTITY .

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZHKMC_VMS_VEHIC_01_MPC_EXT IMPLEMENTATION.


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

 	 lo_entity_type = model->get_entity_type( iv_entity_name = 'VEHICLE_DATA' ). "#EC NOTEXT
   lo_entity_type->bind_structure( iv_structure_name  =  'ZCL_ZHKMC_VMS_VEHIC_01_MPC_EXT=>TY_DEEP_ENTITY' )."#EC NOTEXT


  endmethod.
ENDCLASS.
