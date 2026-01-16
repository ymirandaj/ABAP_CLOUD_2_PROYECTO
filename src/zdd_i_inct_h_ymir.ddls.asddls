@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'interface de proyección del historial'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZDD_I_INCT_H_YMIR
provider contract transactional_interface 
as projection on ZDD_INCT_H_YMIR
{
    key HisUuid,
     IncUuid,
    HisId,
    PreviousStatus,
    NewStatus,
    Text,
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
    _Incident,
    _StatusNext,
    _StatusPrevius
}
