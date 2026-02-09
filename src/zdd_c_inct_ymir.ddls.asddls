@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PROYECCIÓN DE CONSUMO DEL INCIDENTE'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZDD_C_INCT_YMIR
provider contract transactional_query 
as projection on ZDD_R_INCT_YMIR
{
    
    key IncUuid,
    IncidentId,
    Title,
    Description,
    
    @ObjectModel.text.element: [ 'StatusDescription' ]
    Status,
    
    _Status.StatusDescription  as StatusDescription,
    @ObjectModel.text.element: [ 'PriorityDescription' ]
    Priority,
    
    _Priority.PriorityDescription as PriorityDescription,
    CreationDate,
    ChangedDate,
    LocalCreatedBy,
    LocalCreatedAt,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,
    /* Associations */

    _History:redirected to composition child ZDD_C_INCT_H_YMIR,
    _Priority,
    _Status
}
