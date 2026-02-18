CLASS zcl_inct_status_constants_ymir DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:

      BEGIN OF status,
        open        TYPE ZDE_STATUS_YMIR VALUE 'OP',  " Open
        pending    TYPE ZDE_STATUS_YMIR VALUE 'PE',  " Pending
        in_progress TYPE ZDE_STATUS_YMIR VALUE 'IP',  " In Process


        completed  TYPE ZDE_STATUS_YMIR VALUE 'CO',  " Completed
        closed     TYPE ZDE_STATUS_YMIR VALUE 'CL',  " Closed
        cancelled  TYPE ZDE_STATUS_YMIR VALUE 'CN',  " Cancelled
      END OF status.


    CLASS-METHODS:

      is_final_status
        IMPORTING
          iv_status        TYPE ZDE_STATUS_YMIR
        RETURNING
          VALUE(rv_result) TYPE abap_boolean,


      is_valid_transition
        IMPORTING
          iv_current_status TYPE ZDE_STATUS_YMIR
          iv_new_status     TYPE ZDE_STATUS_YMIR
        RETURNING
          VALUE(rv_result)  TYPE abap_boolean,

      is_valid_status
      impoRTING iv_status type zde_status_ymir
      RETURNING
          VALUE(rv_result) TYPE abap_boolean,


      get_status_description
        IMPORTING
          iv_status        TYPE ZDE_STATUS_YMIR
        RETURNING
          VALUE(rv_desc)   TYPE ZDE_STATUS_DESC.

ENDCLASS.



CLASS ZCL_INCT_STATUS_CONSTANTS_YMIR IMPLEMENTATION.


  METHOD is_final_status.

    rv_result = COND #(
      WHEN iv_status = status-completed OR
           iv_status = status-closed OR
           iv_status = status-cancelled
      THEN abap_true
      ELSE abap_false ).
  ENDMETHOD.


  METHOD is_valid_transition.

    IF is_final_status( iv_current_status ) = abap_true.
      rv_result = abap_false.
      RETURN.
    ENDIF.


    IF iv_current_status = status-pending AND
       ( iv_new_status = status-completed OR
         iv_new_status = status-closed ).
      rv_result = abap_false.
      RETURN.
    ENDIF.


    rv_result = abap_true.
  ENDMETHOD.


  METHOD get_status_description.

    SELECT SINGLE status_description
      FROM zdt_status_ymir
      WHERE status_code = @iv_status
      INTO @rv_desc.

    IF sy-subrc <> 0.
      rv_desc = |Estado desconocido: { iv_status }|.
    ENDIF.
  ENDMETHOD.


  METHOD is_valid_status.

   SELECT SINGLE FROM zdd_status_vh_ymir
          FIELDS StatusCode
          WHERE StatusCode = @iv_status
          INTO @DATA(lv_status).

     IF sy-subrc <> 0.
        rv_result = abap_false.
        return.
     endIF.

    rv_result = abap_true.

  ENDMETHOD.
ENDCLASS.
