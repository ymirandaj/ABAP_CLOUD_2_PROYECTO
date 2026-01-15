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
   READ ENTITIES OF ZDD_R_INCT_YMIR IN LOCAL MODE
      ENTITY Incident
        FIELDS ( IncidentId Status CreationDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_incidents).

    " Filtrar para procesar solo los que aún no tienen valores (evitar re-procesar)
    DELETE lt_incidents WHERE IncidentId IS NOT INITIAL.
    CHECK lt_incidents IS NOT INITIAL.

    " 2. Obtener el último Incident ID de la tabla persistente
    SELECT SINGLE MAX( incident_id ) FROM zdt_inct_ymir INTO @DATA(lv_max_id).

    " 3. Preparar los nuevos valores
    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).

    MODIFY ENTITIES OF ZDD_R_INCT_YMIR IN LOCAL MODE
      ENTITY Incident
        UPDATE FIELDS ( IncidentId Status CreationDate )
        WITH VALUE #( FOR ls_inc IN lt_incidents INDEX INTO i (
                           %tky         = ls_inc-%tky
                           IncidentId   = lv_max_id + i
                           Status       = 'OP'      " Valor inicial por defecto
                           CreationDate = lv_today    " Fecha actual del sistema
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

ENDCLASS.
