@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface del data definicion del root'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZDD_I_INCT_YMIR  
provider contract transactional_interface
as projection on ZDD_R_INCT_YMIR

{
    key IncUuid,
    IncidentId,
    Title,
    Description,
    Status,
    Priority,
    CreationDate,
    ChangedDate,
     @Semantics.user.createdBy: true
    LocalCreatedBy,
     @Semantics.systemDateTime.createdAt: true
    LocalCreatedAt,
    @Semantics.user.localInstanceLastChangedBy: true
    LocalLastChangedBy,
    @Semantics.systemDateTime.localInstanceLastChangedAt: true
    LocalLastChangedAt,
    @Semantics.systemDateTime.lastChangedAt: true
    LastChangedAt,
    /* Associations */
    _History,
    _Priority,
    _Status
}
