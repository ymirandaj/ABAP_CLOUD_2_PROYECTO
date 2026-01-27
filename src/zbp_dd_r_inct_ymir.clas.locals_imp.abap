CLASS lhc_Indicent DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Incident RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Incident RESULT result.
    METHODS setInitialValues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Incident~setInitialValues.
    METHODS createInitialHistory FOR DETERMINE ON SAVE
      IMPORTING keys FOR Incident~createInitialHistory.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Incident RESULT result.

    METHODS changeStatus FOR MODIFY
      IMPORTING keys FOR ACTION Incident~changeStatus RESULT result.



    "Métodos Helpers
    METHODS get_next_history_id
      IMPORTING iv_inc_uuid      TYPE sysuuid_x16
      RETURNING VALUE(rv_his_id) TYPE zde_his_id_ymir.


ENDCLASS.

CLASS lhc_Indicent IMPLEMENTATION.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.


  ENDMETHOD.

  METHOD setInitialValues.
    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
       ENTITY Incident
         FIELDS ( IncidentId Status CreationDate )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_incidents).


    DELETE lt_incidents WHERE IncidentId IS NOT INITIAL.
    CHECK lt_incidents IS NOT INITIAL.


    SELECT SINGLE MAX( incident_id ) FROM zdt_inct_ymir INTO @DATA(lv_max_id).


    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).

    MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
        UPDATE FIELDS ( IncidentId Status CreationDate )
        WITH VALUE #( FOR ls_inc IN lt_incidents INDEX INTO i (
                           %tky         = ls_inc-%tky
                           IncidentId   = lv_max_id + i
                           Status       = 'OP'
                           CreationDate = lv_today
                        ) )
      REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD createInitialHistory.
    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
    ENTITY Incident
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(incidents).

    MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
    ENTITY Incident
      CREATE BY \_History
        FIELDS ( HisId NewStatus Text )
        WITH VALUE #( FOR incident IN incidents (
           %tky    = incident-%tky
           %target = VALUE #( (
                       %cid      = 'INIT_' && incident-IncidentId
                       IncUuid = incident-IncUuid
                       HisId    = get_next_history_id( incident-IncUUID )
                       NewStatus = incident-Status
                       Text      = 'First Incident'
                     ) )
        ) )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD get_next_history_id.
    DATA: lv_max_id TYPE zde_his_id_ymir.


    SELECT SINGLE MAX( his_id )
      FROM zdt_inct_h_ymir
      WHERE inc_uuid = @iv_inc_uuid
      INTO @lv_max_id.

    IF sy-subrc = 0.
      rv_his_id = lv_max_id + 1.
    ELSE.
      rv_his_id = 1.
    ENDIF.
  ENDMETHOD.



  METHOD get_instance_features.
  ENDMETHOD.

  METHOD changeStatus.

    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
        FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(incidents).

    LOOP AT incidents ASSIGNING FIELD-SYMBOL(<incident>).


     DATA(ls_popup_data) = keys[ KEY id %tky = <incident>-%tky ]-%param.


      MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
        ENTITY Incident
          UPDATE FIELDS ( Status )
          WITH VALUE #( ( %tky   = <incident>-%tky
                          Status = ls_popup_data-status ) ).


      MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
        ENTITY Incident
          CREATE BY \_History
          FIELDS ( PreviousStatus NewStatus Text HisId )
          WITH VALUE #( ( %tky    = <incident>-%tky
                          %target = VALUE #( ( %cid           = 'NEW_HIST_ENTRY'
                                               HisId    = get_next_history_id( <incident>-IncUUID )
                                               PreviousStatus = <incident>-Status
                                               NewStatus      = ls_popup_data-status
                                               Text           = ls_popup_data-text ) ) ) ).
    ENDLOOP.


    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(updated_incidents).

    result = VALUE #( FOR inc IN updated_incidents
                       ( %tky = inc-%tky %param = inc ) ).
  ENDMETHOD.

ENDCLASS.
