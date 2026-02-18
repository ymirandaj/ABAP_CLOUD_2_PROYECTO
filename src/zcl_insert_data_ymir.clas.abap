CLASS zcl_insert_data_ymir DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: insert_statuses IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      insert_priorities IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out .

ENDCLASS.



CLASS ZCL_INSERT_DATA_YMIR IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    "insert_statuses( out ).
    "insert_priorities( out ).

    "delete from  zdtinct_h_ymir_d.
    "delete from  zdtinct_ymir_d.



  ENDMETHOD.


  METHOD insert_priorities.

    DATA lt_priority TYPE STANDARD TABLE OF zdt_priority_ym.


    TRY.
        lt_priority = VALUE #(
                       ( priority_code = 'H' priority_description = 'High' )
                       ( priority_code = 'M' priority_description = 'Medium' )
                       ( priority_code = 'L' priority_description = 'Low' )

                      ).

        MODIFY zdt_priority_ym FROM TABLE @lt_priority.

        IF sy-subrc = 0.
          io_out->write( | Se ingresaron { sy-dbcnt } prioridades| ).
        ELSE.
          io_out->write( | No se registraron las prioridades.| ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        io_out->write( |Error: { lx_root->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD insert_statuses.
    DATA lt_status TYPE STANDARD TABLE OF zdt_status_ymir.


    TRY.
        lt_status = VALUE #(
                       ( status_code = 'OP' status_description = 'Open' )
                       ( status_code = 'IP' status_description = 'In Progress' )
                       ( status_code = 'PE' status_description = 'Pending' )
                       ( status_code = 'CO' status_description = 'Completed' )
                       ( status_code = 'CL' status_description = 'Closed' )
                       ( status_code = 'CN' status_description = 'Canceled' )
                      ).

        MODIFY zdt_status_ymir FROM TABLE @lt_status.

        IF sy-subrc = 0.
          io_out->write( | Se ingresaron { sy-dbcnt } estados| ).
        ELSE.
          io_out->write( | No se registraron los estados.| ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        io_out->write( |Error: { lx_root->get_text( ) }| ).
    ENDTRY.




  ENDMETHOD.
ENDCLASS.
