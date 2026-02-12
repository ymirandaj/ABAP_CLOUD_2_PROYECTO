

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
    METHODS validateFields FOR VALIDATE ON SAVE
      IMPORTING keys FOR Incident~validateFields.




    "Métodos Helpers
    METHODS get_next_history_id
      IMPORTING iv_inc_uuid      TYPE sysuuid_x16
      RETURNING VALUE(rv_his_id) TYPE zde_his_id_ymir.

    METHODS get_next_incident_id
      RETURNING VALUE(rv_next_id) TYPE zdt_inct_ymir-incident_id.

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
       RESULT DATA(incidents).


    DELETE incidents WHERE IncidentId IS NOT INITIAL.
    CHECK incidents IS NOT INITIAL.







    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).



    MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
        UPDATE FIELDS ( IncidentId Status CreationDate )
        WITH VALUE #( FOR inc IN incidents INDEX INTO i (
                           %tky         = inc-%tky
                           IncidentId   = get_next_incident_id(  )
                           Status       = 'OP' " 'Open'
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
    " 1. Leer los datos de la instancia actual
    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
        FIELDS ( IncidentId )
        WITH CORRESPONDING #( keys )
      RESULT DATA(incidents).

    " 2. Recopilar los IDs para verificar si existen en la base de datos
    "    Nos interesan solo los que ya tienen valor asignado
    DATA(lt_incidents_to_check) = incidents.
    DELETE lt_incidents_to_check WHERE IncidentId IS INITIAL.

    DATA(lt_existing_ids_in_db) = lt_incidents_to_check.

    " 3. Consultar en BD: ¿Cuáles de estos IDs ya están guardados físicamente?
    IF lt_existing_ids_in_db IS NOT INITIAL.
      SELECT incident_id
        FROM zdt_inct_ymir
        FOR ALL ENTRIES IN @lt_existing_ids_in_db
        WHERE incident_id = @lt_existing_ids_in_db-IncidentId
        INTO TABLE @DATA(lt_found_in_db).
    ENDIF.

    " 4. Construir el resultado integrando la lógica dentro del COND
    result = VALUE #( FOR incident IN incidents
      ( %tky = incident-%tky

        %field-CreationDate = COND #(
            " Caso A: No tiene ID -> Estamos creando desde cero
            WHEN incident-IncidentId IS INITIAL THEN
                if_abap_behv=>fc-f-mandatory

            " Caso B: Tiene ID y está en BD -> Estamos editando uno existente
            WHEN line_exists( lt_found_in_db[ incident_id = incident-IncidentId ] ) THEN
                if_abap_behv=>fc-f-read_only

            " Caso C: Tiene ID pero NO está en BD -> Es un Draft nuevo con ID pre-asignado
            ELSE
                if_abap_behv=>fc-f-mandatory
        )

        %field-ChangedDate  = COND #(
            " Caso A: No tiene ID -> Creando: Fecha cambio deshabilitada
            WHEN incident-IncidentId IS INITIAL THEN
                if_abap_behv=>fc-f-read_only

            " Caso B: Tiene ID y está en BD -> Editando: Fecha cambio habilitada
            WHEN line_exists( lt_found_in_db[ incident_id = incident-IncidentId ] ) THEN
                if_abap_behv=>fc-f-unrestricted

            " Caso C: Tiene ID pero NO está en BD -> Creando: Fecha cambio deshabilitada
            ELSE
                if_abap_behv=>fc-f-read_only
        )

        " --- NUEVA LÓGICA PARA LA ACCIÓN ---
        " fc-o-disabled: Botón visible pero gris/deshabilitado
        " fc-o-enabled: Botón habilitado y clicable
        %action-changeStatus = COND #(
            " Si NO tiene ID OR (Tiene ID PERO NO está en BD) -> Es NUEVO -> Deshabilitar
            WHEN incident-IncidentId IS INITIAL
                 OR NOT line_exists( lt_found_in_db[ incident_id = incident-IncidentId ] )
            THEN if_abap_behv=>fc-o-disabled

            " Si tiene ID Y Está en BD -> Es EXISTENTE -> Habilitar
            ELSE if_abap_behv=>fc-o-enabled
        )

      )



    ).
  ENDMETHOD.



*  METHOD changeStatus.
*    " 1. Leer los incidentes actuales
*    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
*      ENTITY Incident
*      FIELDS ( Status IncUUID ) " IncUUID es necesario para tu método get_next_history_id
*      WITH CORRESPONDING #( keys )
*      RESULT DATA(incidents).
*
*    LOOP AT incidents ASSIGNING FIELD-SYMBOL(<incident>).
*      " Obtener parámetros de la acción para esta clave específica
*      DATA(ls_status_param) = keys[ KEY id %tky = <incident>-%tky ]-%param.
*
*      " 2. Actualizar estatus y Crear Historial en una sola llamada MODIFY (Más eficiente)
*      MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
*        ENTITY Incident
*          UPDATE FIELDS ( Status )
*          WITH VALUE #( ( %tky = <incident>-%tky Status = ls_status_param-status ) )
*        ENTITY Incident
*          CREATE BY \_History
*          FIELDS ( PreviousStatus NewStatus Text HisId )
*          WITH VALUE #( ( %tky    = <incident>-%tky
*                          %target = VALUE #( (
*                                    " CID único usando el índice del loop para evitar errores
*                                    %cid           = |HIST_{ sy-tabix }|
*                                    HisId          = get_next_history_id( <incident>-IncUUID )
*                                    PreviousStatus = <incident>-Status
*                                    NewStatus      = ls_status_param-status
*                                    Text           = ls_status_param-text ) ) ) )
*        MAPPED DATA(lt_mapped_local)
*        FAILED DATA(lt_failed_local)
*        REPORTED DATA(lt_reported_local).
*
*      " 3. Mapear errores y respuestas
*      " Importante: recolectar de ambas entidades (Incident y History)
*      INSERT LINES OF lt_failed_local-incident   INTO TABLE failed-incident.
*      INSERT LINES OF lt_failed_local-history    INTO TABLE failed-history.
*      INSERT LINES OF lt_reported_local-incident INTO TABLE reported-incident.
*      INSERT LINES OF lt_reported_local-history  INTO TABLE reported-history.
*
*      " Mapear el historial creado para el framework
*      INSERT LINES OF lt_mapped_local-history    INTO TABLE mapped-history.
*    ENDLOOP.
*
*    " 4. Leer los datos actualizados para devolver el resultado a la UI
*    IF failed-incident IS INITIAL.
*      READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
*        ENTITY Incident
*        ALL FIELDS WITH CORRESPONDING #( keys )
*        RESULT DATA(updated_incidents).
*
*      result = VALUE #( FOR inc IN updated_incidents
*                       ( %tky = inc-%tky %param = inc ) ).
*    ENDIF.
*  ENDMETHOD.

  METHOD changeStatus.
    " 1. Leer los datos actuales (Incluimos campos necesarios para validar)
    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      FIELDS ( Status IncUUID )
      WITH CORRESPONDING #( keys )
      RESULT DATA(incidents).

    LOOP AT incidents ASSIGNING FIELD-SYMBOL(<incident>).
      " Limpiar mensajes previos de esta acción
      APPEND VALUE #( %tky = <incident>-%tky %state_area = 'VALIDATE_INCIDENT' ) TO reported-incident.

      " Obtener parámetros de la acción
      DATA(ls_status_param) = keys[ KEY id %tky = <incident>-%tky ]-%param.

      " ==========================================================
      " BLOQUE DE VALIDACIONES
      " ==========================================================
      DATA(lv_has_error) = abap_false.

      if <incident>-Status = 'PE' AND ( ls_status_param-status = 'CO' OR ls_status_param-status = 'CL'  ).

      lv_has_error = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky          = <incident>-%tky
          %state_area   = 'VALIDATE_INCIDENT'
          %msg          = NEW zcx_inct_messages_ymir(
                            textid      = zcx_inct_messages_ymir=>state_pending_validate " Debes crear esta constante
                            severity    = if_abap_behv_message=>severity-error
                            inc_uuid    = <incident>-IncUUID
                            incident_id = CONV #( <incident>-IncidentId ) )
        ) TO reported-incident.
      ENDIF.
      IF lv_has_error = abap_true.
        CONTINUE.
      ENDIF.



      IF <incident>-Status = 'CL' OR <incident>-Status = 'CN' OR <incident>-Status = 'CO'.
        lv_has_error = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky ) TO failed-incident.
        APPEND VALUE #(
          %tky          = <incident>-%tky
          %state_area   = 'VALIDATE_INCIDENT'
          %msg          = NEW zcx_inct_messages_ymir(
                            textid      = zcx_inct_messages_ymir=>invalid_status_transition " Debes crear esta constante
                            severity    = if_abap_behv_message=>severity-error
                            inc_uuid    = <incident>-IncUUID
                            incident_id = CONV #( <incident>-IncidentId ) )
        ) TO reported-incident.
      ENDIF.
      IF lv_has_error = abap_true.
        CONTINUE.
      ENDIF.

      " Validación 2: El texto de justificación es obligatorio
      IF ls_status_param-status IS INITIAL.
        lv_has_error = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky  ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = <incident>-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                   = NEW zcx_inct_messages_ymir(
          previous_status = |{ <incident>-Status }|
          new_status = |{ ls_status_param-status  }|
            textid       = zcx_inct_messages_ymir=>field_required
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = <incident>-IncUUID
            incident_id  = CONV #( <incident>-IncidentId

             )

          )

        ) TO reported-incident.

      ELSEIF <incident>-Status = ls_status_param-status.
        lv_has_error = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky  ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = <incident>-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                   = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>same_status
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = <incident>-IncUUID
            incident_id  = CONV #( <incident>-IncidentId )

          )

        ) TO reported-incident.




      ENDIF.


      IF lv_has_error = abap_true.
        CONTINUE.
      ENDIF.

      " Validación 1: El nuevo estatus no puede ser igual al actual


      " Validación 2: El texto de justificación es obligatorio
      IF ls_status_param-text IS INITIAL.
        lv_has_error = abap_true.
        APPEND VALUE #( %tky = <incident>-%tky  ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = <incident>-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                   = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>history_text_required
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = <incident>-IncUUID
            incident_id  = CONV #( <incident>-IncidentId )

          )

        ) TO reported-incident.
      ENDIF.

      " Si hay errores, saltamos al siguiente incidente sin ejecutar el MODIFY
      IF lv_has_error = abap_true.
        CONTINUE.
      ENDIF.




      " ==========================================================
      " PROCESO DE ACTUALIZACIÓN (Solo si no hay errores)
      " ==========================================================
      MODIFY ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
        ENTITY Incident
          UPDATE FIELDS ( Status )
          WITH VALUE #( ( %tky = <incident>-%tky Status = ls_status_param-status ) )
        ENTITY Incident
          CREATE BY \_History
          FIELDS ( PreviousStatus NewStatus Text HisId )
          WITH VALUE #( ( %tky    = <incident>-%tky
                          %target = VALUE #( (
                                    %cid           = |HIST_{ sy-tabix }|
                                    HisId          = get_next_history_id( <incident>-IncUUID )
                                    PreviousStatus = <incident>-Status
                                    NewStatus      = ls_status_param-status
                                    Text           = ls_status_param-text ) ) ) )
        MAPPED DATA(lt_mapped_local)
        FAILED DATA(lt_failed_local)
        REPORTED DATA(lt_reported_local).

      " Recolectar resultados del MODIFY
      INSERT LINES OF lt_failed_local-incident   INTO TABLE failed-incident.
      INSERT LINES OF lt_reported_local-incident INTO TABLE reported-incident.
      INSERT LINES OF lt_mapped_local-history    INTO TABLE mapped-history.
    ENDLOOP.

    " 4. Devolver el resultado a la UI
    IF failed-incident IS INITIAL.
      READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
        ENTITY Incident
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(updated_incidents).

      result = VALUE #( FOR inc IN updated_incidents
                       ( %tky = inc-%tky %param = inc ) ).
    ENDIF.
  ENDMETHOD.



  METHOD get_next_incident_id.

    SELECT SINGLE MAX( incident_id ) FROM zdt_inct_ymir INTO @DATA(lv_max_incident).


    SELECT SINGLE MAX( incidentid ) FROM zdtinct_ymir_d INTO @DATA(lv_max_incident_draft).


    rv_next_id = COND #( WHEN lv_max_incident >= lv_max_incident_draft
                         THEN lv_max_incident
                         ELSE lv_max_incident_draft ) + 1.
  ENDMETHOD.



  METHOD validateFields.
    " Leer los datos actuales
    READ ENTITIES OF zdd_r_inct_ymir IN LOCAL MODE
      ENTITY Incident
      FIELDS ( Title Description Priority CreationDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_incidents).

    CLEAR: failed-incident, reported-incident.


    LOOP AT lt_incidents INTO DATA(ls_incident).

      " 1. PASO CRUCIAL: Limpiar el área de estado SIEMPRE para este registro
      APPEND VALUE #(
          %tky        = ls_incident-%tky
          %state_area = 'VALIDATE_INCIDENT'
      ) TO reported-incident.

      " ====================================================================
      " Validación de Title
      " ====================================================================
      IF ls_incident-Title IS INITIAL.
        " Agregar a failed
        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        " Agregar mensaje específico a reported con %state_area
        APPEND VALUE #(
          %tky               = ls_incident-%tky
          %state_area        = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg               = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>enter_title
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
          )
          %element-title     = if_abap_behv=>mk-on
        ) TO reported-incident.

      ELSEIF strlen( ls_incident-Title ) < 5.
        " Título muy corto
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

      " ====================================================================
      " Validación de Description
      " ====================================================================
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
        " Descripción muy corta
        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = ls_incident-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
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
      DATA(lv_today) = cl_abap_context_info=>get_system_date( ).

      " 1. Obtener el timestamp actual del sistema (UTC)
      GET TIME STAMP FIELD DATA(lv_timestamp).

      " 2. Convertirlo a tu zona horaria específica
      CONVERT TIME STAMP lv_timestamp
              TIME ZONE 'EST'  " O 'America/Guayaquil'
              INTO DATE DATA(lv_local_date).


      IF ls_incident-CreationDate IS INITIAL.
        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = ls_incident-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                   = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>enter_creation_date
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
          )
          %element-creationDate   = if_abap_behv=>mk-on
        ) TO reported-incident.

      ELSEIF ls_incident-CreationDate > lv_local_date.
        " Descripción muy corta
        APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

        APPEND VALUE #(
          %tky                   = ls_incident-%tky
          %state_area            = 'VALIDATE_INCIDENT'   " <-- AGREGADO
          %msg                   = NEW zcx_inct_messages_ymir(
            textid       = zcx_inct_messages_ymir=>creation_date_invalid
            severity     = if_abap_behv_message=>severity-error
            inc_uuid     = ls_incident-IncUUID
            incident_id  = CONV #( ls_incident-IncidentId )
            creation_date = ls_incident-CreationDate
            changed_date = lv_local_date
          )
          %element-creationDate   = if_abap_behv=>mk-on
        ) TO reported-incident.
      ENDIF.

      " ====================================================================
      " Validación de Priority
      " ====================================================================
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
        " Validar que el código de prioridad exista
        SELECT SINGLE FROM zdd_priority_vh_ymir
          FIELDS PriorityCode
          WHERE PriorityCode = @ls_incident-Priority
          INTO @DATA(lv_priority_exists).

        IF sy-subrc <> 0.
          " Priority no válida
          APPEND VALUE #( %tky = ls_incident-%tky ) TO failed-incident.

          APPEND VALUE #(
            %tky                 = ls_incident-%tky
            %state_area          = 'VALIDATE_INCIDENT'   " <-- AGREGADO
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



ENDCLASS.
