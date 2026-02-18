

CLASS lhc_Indicent DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    "AUTORIZACIONES POR INSTANCIA
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Incident RESULT result.

    "AUTORIZACIONES GLOBALES
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Incident RESULT result.

    "MÉTODO PARA DAR LOS VALORES INICIALES AL MOMENTO DE CREAR UN INCIDENTE
    METHODS setInitialValues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Incident~setInitialValues.

    "ESTE MÉTODO SE EJECUTA CUANDO SE CREA EL INCIDENTE Y AGREGA AL HISTORIAL SU INICIO
    METHODS createInitialHistory FOR DETERMINE ON SAVE
      IMPORTING keys FOR Incident~createInitialHistory.

    "MÉTODO QUE SIRVE PARA DAR CIERTAS CONDICIONES COMO EJEMPLO PORNER EN READONLY EN BASE A UNA CONDICIÓN
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Incident RESULT result.

    "METODO QUE PERMITE CAMBIAR EL ESTADO DEL INCIDENTE
    METHODS changeStatus FOR MODIFY
      IMPORTING keys FOR ACTION Incident~changeStatus RESULT result.
    "VALIDACIONES PARA CREAR O MODIFICAR EL INCIDENTE
    METHODS validateFields FOR VALIDATE ON SAVE
      IMPORTING keys FOR Incident~validateFields.




    "SE CREA EL MÉTODO get_next_history_id PARA OBTENER SIGUIENTE EL ID DEL HISTORIAL DEL INCIDENTE
    METHODS get_next_history_id
      IMPORTING iv_inc_uuid      TYPE sysuuid_x16
      RETURNING VALUE(rv_his_id) TYPE zde_his_id_ymir.
    "SE CREA EL MÉTODO get_next_incident_id PARA OBTENER EL SIGUIENTE ID DEL INCIDENTE
    METHODS get_next_incident_id
      RETURNING VALUE(rv_next_id) TYPE zdt_inct_ymir-incident_id.
    "SE CREA PARA OBTENER LA FECHA LOCAL
    METHODS get_local_date RETURNING VALUE(rv_local_date) TYPE zde_creation_date_ymir.


ENDCLASS.

CLASS lhc_Indicent IMPLEMENTATION.

  METHOD get_instance_authorizations.

    " OBTENER EL USUARIO DESDE LA APLICACIÓN
    DATA(current_user) = cl_abap_context_info=>get_user_technical_name( ).


    CONSTANTS cv_admin TYPE string VALUE 'CB9980000302'.


    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      FIELDS ( LocalCreatedBy )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_incidents).




    LOOP AT lt_incidents INTO DATA(ls_incident).

      "CONDICIÓN QUE PERMITE DETERMINAR SI UN USUARIO PUEDE ACTUALIZAR LOS REGISTROS
      DATA(lv_is_authorized) = COND #(
          WHEN current_user = cv_admin
          THEN if_abap_behv=>auth-allowed
          ELSE if_abap_behv=>auth-unauthorized ).


      "CONDICIÓN QUE PERMITE SI UN USUARIO PUEDE ELIMINAR LOS REGISTROS
      DATA(lv_delete_allowed) = COND #(
          WHEN lv_is_authorized = if_abap_behv=>auth-allowed AND ls_incident-Status = zcl_inct_status_constants_ymir=>status-open
          THEN if_abap_behv=>auth-allowed
          ELSE if_abap_behv=>auth-unauthorized ).


      " SE AGREGA LAS AUTORIZACIONES PARA EL UPDATE Y DELETE EN BASE A LAS CONDICIONES
      APPEND VALUE #( %tky    = ls_incident-%tky
                      %update = lv_is_authorized
                      %delete = lv_delete_allowed
                    ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_global_authorizations.


    DATA(current_user) = cl_abap_context_info=>get_user_technical_name( ).
    CONSTANTS cv_admin TYPE string VALUE 'CB9980000302'.


    IF requested_authorizations-%update      = if_abap_behv=>mk-on .

      IF current_user = cv_admin.
        result-%update      = if_abap_behv=>auth-allowed.
      ELSE.
        result-%update      = if_abap_behv=>auth-allowed.
      ENDIF.
    ENDIF.

  ENDMETHOD.



  METHOD setInitialValues.

    " SE LEE LOS INCIDENTES
    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      FIELDS ( IncidentId Status CreationDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(incidents).

    DELETE incidents WHERE IncidentId IS NOT INITIAL.

    CHECK incidents IS NOT INITIAL.


    "SE ASIGNA LOS VALORES INICIALES PARA CUANDO SE QUIERE CREAR UN INCIDENTE, DONDE SE ASIGNA EL ID DEL INCIDENTE,
    "SU ESTADO Y FECHA DE CREACIÓN
    MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      UPDATE FIELDS ( IncidentId Status CreationDate )
      WITH VALUE #( FOR inc IN incidents (
        %tky         = inc-%tky
        IncidentId   = get_next_incident_id( )
        Status       = zcl_inct_status_constants_ymir=>status-open
        CreationDate = get_local_date( )
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


    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
        FIELDS ( IncidentId )
        WITH CORRESPONDING #( keys )
      RESULT DATA(incidents).



    DATA(lt_incidents_to_check) = incidents.
    DELETE lt_incidents_to_check WHERE IncidentId IS INITIAL.

    DATA(lt_existing_ids_in_db) = lt_incidents_to_check.




    IF lt_existing_ids_in_db IS NOT INITIAL.
      SELECT incident_id
        FROM zdt_inct_ymir
        FOR ALL ENTRIES IN @lt_existing_ids_in_db
        WHERE incident_id = @lt_existing_ids_in_db-IncidentId
        INTO TABLE @DATA(lt_found_in_db).
    ENDIF.


    result = VALUE #( FOR incident IN incidents
      ( %tky = incident-%tky

        %field-CreationDate = COND #(

            WHEN incident-IncidentId IS INITIAL THEN
                if_abap_behv=>fc-f-mandatory

            WHEN line_exists( lt_found_in_db[ incident_id = incident-IncidentId ] ) THEN
                if_abap_behv=>fc-f-read_only

            ELSE
                if_abap_behv=>fc-f-mandatory
        )

        %field-ChangedDate  = COND #(

            WHEN incident-IncidentId IS INITIAL THEN
                if_abap_behv=>fc-f-read_only

            WHEN line_exists( lt_found_in_db[ incident_id = incident-IncidentId ] ) THEN
                if_abap_behv=>fc-f-unrestricted

            ELSE
                if_abap_behv=>fc-f-read_only
        )

        %action-changeStatus = COND #(

            WHEN incident-IncidentId IS INITIAL
                 OR NOT line_exists( lt_found_in_db[ incident_id = incident-IncidentId ] )
            THEN if_abap_behv=>fc-o-disabled


            ELSE if_abap_behv=>fc-o-enabled
        )

      )



    ).
  ENDMETHOD.


  METHOD changeStatus.

    DATA: lv_has_error TYPE abap_boolean.


    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      FIELDS ( Status IncUUID IncidentId )
      WITH CORRESPONDING #( keys )
      RESULT DATA(incidents).


    CHECK incidents IS NOT INITIAL.


    LOOP AT incidents ASSIGNING FIELD-SYMBOL(<incident>).
      CLEAR lv_has_error.


      APPEND VALUE #(
        %tky        = <incident>-%tky
        %state_area = 'VALIDATE_INCIDENT'
      ) TO reported-incident.

      TRY.
          DATA(ls_status_param) = keys[ KEY id %tky = <incident>-%tky ]-%param.
        CATCH cx_sy_itab_line_not_found.
          APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
          APPEND VALUE #(
            %tky        = <incident>-%tky
            %state_area = 'VALIDATE_INCIDENT'
            %msg        = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = 'Parámetros de acción no encontrados' )
          ) TO reported-incident.
          CONTINUE.
      ENDTRY.

      IF ls_status_param-status IS INITIAL.
        lv_has_error = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky        = <incident>-%tky
          %state_area = 'VALIDATE_INCIDENT'
          %msg        = NEW zcx_inct_messages_ymir(
            textid          = zcx_inct_messages_ymir=>field_required
            severity        = if_abap_behv_message=>severity-error
            inc_uuid        = <incident>-IncUUID
            incident_id     = CONV #( <incident>-IncidentId )
            previous_status = |{ <incident>-Status }|
            new_status      = |{ ls_status_param-status }| )
        ) TO reported-incident.
        CONTINUE.
      ENDIF.


      IF <incident>-Status = ls_status_param-status.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky        = <incident>-%tky
          %state_area = 'VALIDATE_INCIDENT'
          %msg        = NEW zcx_inct_messages_ymir(
            textid      = zcx_inct_messages_ymir=>same_status
            severity    = if_abap_behv_message=>severity-error
            inc_uuid    = <incident>-IncUUID
            incident_id = CONV #( <incident>-IncidentId ) )
        ) TO reported-incident.
        CONTINUE.
      ENDIF.


      IF zcl_inct_status_constants_ymir=>is_valid_status( ls_status_param-status ) = abap_false.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky        = <incident>-%tky
          %state_area = 'VALIDATE_INCIDENT'
          %msg        = NEW zcx_inct_messages_ymir(
            textid      = zcx_inct_messages_ymir=>invalid_status
            status        = |{ ls_status_param-status }|
            severity    = if_abap_behv_message=>severity-error
            inc_uuid    = <incident>-IncUUID
            incident_id = CONV #( <incident>-IncidentId ) )

        ) TO reported-incident.
        CONTINUE.
      ENDIF.


      IF zcl_inct_status_constants_ymir=>is_final_status( <incident>-Status ) = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky        = <incident>-%tky
          %state_area = 'VALIDATE_INCIDENT'
          %msg        = NEW zcx_inct_messages_ymir(
            textid      = zcx_inct_messages_ymir=>state_pending_validate
            severity    = if_abap_behv_message=>severity-error

            inc_uuid    = <incident>-IncUUID
            incident_id = CONV #( <incident>-IncidentId ) )
        ) TO reported-incident.
        CONTINUE.
      ENDIF.

      IF zcl_inct_status_constants_ymir=>is_valid_transition(
           iv_current_status = <incident>-Status
           iv_new_status     = ls_status_param-status ) = abap_false.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky        = <incident>-%tky
          %state_area = 'VALIDATE_INCIDENT'
          %msg        = NEW zcx_inct_messages_ymir(
            textid      = zcx_inct_messages_ymir=>invalid_status_transition
            new_status     = |{ ls_status_param-status }|
            previous_status = |{ <incident>-Status }|
            severity    = if_abap_behv_message=>severity-error
            inc_uuid    = <incident>-IncUUID

            incident_id = CONV #( <incident>-IncidentId ) )
        ) TO reported-incident.
        CONTINUE.
      ENDIF.


      IF ls_status_param-text IS INITIAL.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky        = <incident>-%tky
          %state_area = 'VALIDATE_INCIDENT'
          %msg        = NEW zcx_inct_messages_ymir(
            textid      = zcx_inct_messages_ymir=>history_text_required
            severity    = if_abap_behv_message=>severity-error
            inc_uuid    = <incident>-IncUUID
            incident_id = CONV #( <incident>-IncidentId ) )
        ) TO reported-incident.
        CONTINUE.
      ENDIF.


      MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
        ENTITY Incident
          UPDATE FIELDS ( Status )
          WITH VALUE #( (
            %tky   = <incident>-%tky
            Status = ls_status_param-status
          ) )

        ENTITY Incident
          CREATE BY \_History
          FIELDS ( PreviousStatus NewStatus Text HisId )
          WITH VALUE #( (
            %tky    = <incident>-%tky
            %target = VALUE #( (
              %cid           = |HIST_{ sy-tabix }|
              HisId          = get_next_history_id( <incident>-IncUUID )
              PreviousStatus = <incident>-Status
              NewStatus      = ls_status_param-status
              Text           = ls_status_param-text
            ) )
          ) )
        MAPPED   DATA(lt_mapped_local)
        FAILED   DATA(lt_failed_local)
        REPORTED DATA(lt_reported_local).


      INSERT LINES OF lt_failed_local-incident   INTO TABLE failed-incident.
      INSERT LINES OF lt_reported_local-incident INTO TABLE reported-incident.
      INSERT LINES OF lt_mapped_local-history    INTO TABLE mapped-history.

    ENDLOOP.


    CHECK failed-incident IS INITIAL.

    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(updated_incidents).

    result = VALUE #( FOR inc IN updated_incidents
                      ( %tky   = inc-%tky
                        %param = inc ) ).

  ENDMETHOD.



  METHOD get_next_incident_id.

    SELECT SINGLE MAX( incident_id ) FROM zdt_inct_ymir INTO @DATA(lv_max_incident).


    SELECT SINGLE MAX( incidentid ) FROM zdtinct_ymir_d INTO @DATA(lv_max_incident_draft).


    rv_next_id = COND #( WHEN lv_max_incident >= lv_max_incident_draft
                         THEN lv_max_incident
                         ELSE lv_max_incident_draft ) + 1.
  ENDMETHOD.



  METHOD validateFields.

    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      FIELDS ( Title Description Priority CreationDate IncUUID IncidentId ChangedDate   )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_incidents).

    CLEAR: failed-incident, reported-incident.




    SELECT PriorityCode
   FROM zdd_priority_vh_ymir
   INTO TABLE @DATA(lt_priorities).
    SORT lt_priorities BY PriorityCode.


    LOOP AT lt_incidents INTO DATA(ls_incident).


      DATA(lv_is_create) = abap_true.


      SELECT SINGLE FROM zdd_inct_h_ymir
      FIELDS @if_abap_behv=>mk-on
      WHERE IncUuid = @ls_incident-IncUUID
      INTO @DATA(lv_exists_in_db).


      IF sy-subrc = 0.
        lv_is_create = abap_false.
      ENDIF.


      APPEND VALUE #(
          %tky        = ls_incident-%tky
          %state_area = 'VALIDATE_INCIDENT'
      ) TO reported-incident.


      IF ls_incident-Title IS INITIAL.

        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.


        APPEND VALUE #(
          %tky               = ls_incident-%tky
          %state_area        = 'VALIDATE_INCIDENT'
          %msg               = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>enter_title
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
          )
          %element-title     = if_abap_behv=>mk-on
        ) TO reported-incident.

      ELSEIF strlen( ls_incident-Title ) < 5.

        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky               = ls_incident-%tky
          %state_area        = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg               = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>title_too_short
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
            min_length   = '5'
          )
          %element-title     = if_abap_behv=>mk-on
        ) TO reported-incident.
      ENDIF.


      IF ls_incident-Description IS INITIAL.
        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = ls_incident-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                   = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>enter_description
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
          )
          %element-description   = if_abap_behv=>mk-on
        ) TO reported-incident.

      ELSEIF strlen( ls_incident-Description ) < 5.

        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = ls_incident-%tky
          %state_area            = 'VALIDATE_INCIDENT'
          %msg                   = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>description_too_short
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
            min_length   = '5'
          )
          %element-description   = if_abap_behv=>mk-on
        ) TO reported-incident.
      ENDIF.




      IF lv_is_create = abap_true .

        IF ls_incident-CreationDate IS INITIAL.
          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                   = ls_incident-%tky
            %state_area            = 'VALIDATE_INCIDENT'
            %msg                   = NEW zcx_inct_messages_ymir(
              textid       = zcx_inct_messages_ymir=>enter_creation_date
              severity     = if_abap_behv_message=>severity-error
              inc_uuid     = ls_incident-IncUUID
              incident_id  = CONV #( ls_incident-IncidentId )
            )
            %element-creationDate   = if_abap_behv=>mk-on
          ) TO reported-incident.

        ELSEIF ls_incident-CreationDate > get_local_date( ).

          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                   = ls_incident-%tky
            %state_area            = 'VALIDATE_INCIDENT'
            %msg                   = NEW zcx_inct_messages_ymir(
              textid       = zcx_inct_messages_ymir=>creation_date_invalid
              severity     = if_abap_behv_message=>severity-error
              inc_uuid     = ls_incident-IncUUID
              incident_id  = CONV #( ls_incident-IncidentId )
              creation_date = ls_incident-CreationDate
              changed_date = get_local_date( )
            )
            %element-creationDate   = if_abap_behv=>mk-on
          ) TO reported-incident.
        ENDIF.
      ELSE.

        IF ls_incident-ChangedDate IS INITIAL.
          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                   = ls_incident-%tky
            %state_area            = 'VALIDATE_INCIDENT'
            %msg                   = NEW zcx_inct_messages_ymir(
              textid       = zcx_inct_messages_ymir=>enter_changed_date
              severity     = if_abap_behv_message=>severity-error
              inc_uuid     = ls_incident-IncUUID
              incident_id  = CONV #( ls_incident-IncidentId )
            )
            %element-changedDate   = if_abap_behv=>mk-on
          ) TO reported-incident.

        ELSEIF ls_incident-ChangedDate > get_local_date( ).

          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                   = ls_incident-%tky
            %state_area            = 'VALIDATE_INCIDENT'
            %msg                   = NEW zcx_inct_messages_ymir(
              textid       = zcx_inct_messages_ymir=>changed_date_invalid
              severity     = if_abap_behv_message=>severity-error
              inc_uuid     = ls_incident-IncUUID
              incident_id  = CONV #( ls_incident-IncidentId )
              creation_date = ls_incident-CreationDate
              changed_date = get_local_date( )
            )
            %element-changedDate   = if_abap_behv=>mk-on
          ) TO reported-incident.

        ELSEIF ls_incident-ChangedDate < ls_incident-CreationDate.

          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                   = ls_incident-%tky
            %state_area            = 'VALIDATE_INCIDENT'
            %msg                   = NEW zcx_inct_messages_ymir(
              textid       = zcx_inct_messages_ymir=>changed_date_bcd
              severity     = if_abap_behv_message=>severity-error
              inc_uuid     = ls_incident-IncUUID
              incident_id  = CONV #( ls_incident-IncidentId )
              creation_date = ls_incident-CreationDate
              changed_date = get_local_date( )
            )
            %element-changedDate   = if_abap_behv=>mk-on
          ) TO reported-incident.

        ENDIF.

      ENDIF.

      IF ls_incident-Priority IS INITIAL.
        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky                 = ls_incident-%tky
          %state_area          = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                 = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>enter_priority
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
          )
          %element-priority    = if_abap_behv=>mk-on
        ) TO reported-incident.

      ELSE.

        READ TABLE lt_priorities
        WITH KEY PriorityCode = ls_incident-Priority
        TRANSPORTING NO FIELDS.

        IF sy-subrc <> 0.

          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                 = ls_incident-%tky
            %state_area          = 'VALIDATE_INCIDENT'
            %msg                 = NEW zcx_inct_messages_ymir(
              textid       = zcx_inct_messages_ymir=>invalid_priority
              severity     = if_abap_behv_message=>severity-error
              inc_uuid     = ls_incident-IncUUID
              incident_id  = CONV #( ls_incident-IncidentId )
            )
            %element-priority    = if_abap_behv=>mk-on
          ) TO reported-incident.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_local_date.

    GET TIME STAMP FIELD DATA(lv_timestamp).

    CONVERT TIME STAMP lv_timestamp
            TIME ZONE 'EST'
            INTO DATE DATA(lv_local_date).
    rv_local_date = lv_local_date.
  ENDMETHOD.

ENDCLASS.
