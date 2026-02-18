"! <p class="shorttext synchronized" lang="es">Esta clase sirve para los mensajes de los incidentes</p>
"! <p class="longtext synchronized" lang="es">Esta clase se utiliza para manejar los mensajes de error y validaciones relacionados con los incidentes en el sistema.
"! Contiene constantes para diferentes tipos de mensajes, como campos requeridos, longitud mínima y máxima, prioridades inválidas, entre otros. Además, tiene un constructor
"! que permite inicializar los atributos del mensaje y establecer la severidad del mismo.</p>
CLASS zcx_inct_messages_ymir DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.
    INTERFACES if_abap_behv_message.

    CONSTANTS:
      gc_msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',


      BEGIN OF field_required,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_FIELD_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_required,

      BEGIN OF title_too_short,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MIN_LENGTH',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF title_too_short,

      BEGIN OF description_too_short,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_MIN_LENGTH',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF description_too_short,

      BEGIN OF invalid_priority,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_priority,

      BEGIN OF invalid_characters,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_characters,

      BEGIN OF description_too_long,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_MAX_LENGTH',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF description_too_long,

      BEGIN OF description_warning,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF description_warning,

      BEGIN OF incident_validated,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_INCIDENT_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF incident_validated,

      BEGIN OF validating_incident,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MV_INCIDENT_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF validating_incident,

      BEGIN OF invalid_status,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MV_STATUS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_status,

      BEGIN OF enter_title,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF enter_title,

      BEGIN OF enter_description,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF enter_description,

      BEGIN OF enter_priority,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF enter_priority,

      BEGIN OF invalid_creation_date,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_creation_date,

      BEGIN OF invalid_changed_date,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_changed_date,

      BEGIN OF not_authorized,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_authorized,

      BEGIN OF incident_not_found,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MV_INCIDENT_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF incident_not_found,

      BEGIN OF history_text_required,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF history_text_required,

      BEGIN OF same_status,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF same_status,

      BEGIN OF invalid_status_transition,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MV_PREVIOUS_STATUS',
        attr2 TYPE scx_attrname VALUE 'MV_NEW_STATUS',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_status_transition,

 BEGIN OF creation_date_invalid,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'MV_CREATION_DATE',
        attr2 TYPE scx_attrname VALUE 'MV_CHANGED_DATE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF creation_date_invalid,
      BEGIN OF enter_creation_date,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF enter_creation_date,
      BEGIN OF state_pending_validate,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF state_pending_validate,
      BEGIN OF enter_changed_date,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF enter_changed_date,
      BEGIN OF changed_date_invalid,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF changed_date_invalid,
      BEGIN OF changed_date_bcd,
        msgid TYPE symsgid VALUE 'ZMC_MESSAGE_INC',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF changed_date_bcd.

    METHODS constructor
      IMPORTING
        textid             LIKE if_t100_message=>t100key OPTIONAL
        attr1              TYPE string OPTIONAL
        attr2              TYPE string OPTIONAL
        attr3              TYPE string OPTIONAL
        attr4              TYPE string OPTIONAL
        previous           LIKE previous OPTIONAL
        inc_uuid           TYPE sysuuid_x16 OPTIONAL
        incident_id        TYPE string OPTIONAL
        title              TYPE string OPTIONAL
        description        TYPE string OPTIONAL
        status             TYPE string OPTIONAL
        priority           TYPE string OPTIONAL
        previous_status    TYPE string OPTIONAL
        new_status         TYPE string OPTIONAL
        field_name         TYPE string OPTIONAL
        min_length         TYPE string OPTIONAL
        max_length         TYPE string OPTIONAL
        creation_date      TYPE datum OPTIONAL
        changed_date       TYPE datum OPTIONAL
        severity           TYPE if_abap_behv_message=>t_severity OPTIONAL
        uname              TYPE syuname OPTIONAL.

    DATA:
      mv_attr1           TYPE string,
      mv_attr2           TYPE string,
      mv_attr3           TYPE string,
      mv_attr4           TYPE string,
      mv_inc_uuid        TYPE sysuuid_x16,
      mv_incident_id     TYPE string,
      mv_title           TYPE string,
      mv_description     TYPE string,
      mv_status          TYPE string,
      mv_priority        TYPE string,
      mv_previous_status TYPE string,
      mv_new_status      TYPE string,
      mv_field_name      TYPE string,
      mv_min_length      TYPE string,
      mv_max_length      TYPE string,
      mv_creation_date   TYPE datum,
      mv_changed_date    TYPE datum,
      mv_uname           TYPE syuname.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_INCT_MESSAGES_YMIR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->mv_attr1           = attr1.
    me->mv_attr2           = attr2.
    me->mv_attr3           = attr3.
    me->mv_attr4           = attr4.
    me->mv_inc_uuid        = inc_uuid.
    me->mv_incident_id     = incident_id.
    me->mv_title           = title.
    me->mv_description     = description.
    me->mv_status          = status.
    me->mv_priority        = priority.
    me->mv_previous_status = previous_status.
    me->mv_new_status      = new_status.
    me->mv_field_name      = field_name.
    me->mv_min_length      = min_length.
    me->mv_max_length      = max_length.
    me->mv_creation_date   = creation_date.
    me->mv_changed_date    = changed_date.
    me->mv_uname           = uname.

    if_abap_behv_message~m_severity = severity.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
