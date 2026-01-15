@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'interface de proyección del historial'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZDD_I_INCT_H_YMIR
provider contract transactional_interface 
as projection on ZDD_INCT_H_YMIR
{
    key HisUuid,
    key IncUuid,
    HisId,
    PreviousStatus,
    NewStatus,
    Text,
    LocalCreatedBy,
    LocalCreatedAt,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,
    /* Associations */
    _Incident,
    _StatusNext,
    _StatusPrevius
}
