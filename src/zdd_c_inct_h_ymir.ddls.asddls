@AccessControl.authorizationCheck:#NOT_REQUIRED
@EndUserText.label: 'PROYECCION DEL CONSUMO DEL HISTORIAL'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZDD_C_INCT_H_YMIR
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
    _Incident : redirected to parent ZDD_C_INCT_YMIR,
    _StatusNext,
    _StatusPrevius
}
