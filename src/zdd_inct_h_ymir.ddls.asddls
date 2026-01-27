@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS history incidents'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZDD_INCT_H_YMIR as select from zdt_inct_h_ymir
association to parent ZDD_R_INCT_YMIR as _Incident on _Incident.IncUuid = $projection.IncUuid
association [1..1] to ZDD_STATUS_VH_YMIR as _StatusPrevius on _StatusPrevius.StatusCode = $projection.PreviousStatus
association [1..1] to ZDD_STATUS_VH_YMIR as _StatusNext on _StatusNext.StatusCode = $projection.NewStatus
{
    key his_uuid as HisUuid,
    inc_uuid as IncUuid,
    his_id as HisId,
    previous_status as PreviousStatus,
    new_status as NewStatus,
    text as Text,
     @Semantics.user.createdBy: true
    local_created_by as LocalCreatedBy,
    @Semantics.systemDateTime.createdAt: true
    local_created_at as LocalCreatedAt,
    @Semantics.user.localInstanceLastChangedBy: true
    local_last_changed_by as LocalLastChangedBy,
    @Semantics.systemDateTime.localInstanceLastChangedAt: true
    local_last_changed_at as LocalLastChangedAt,
    @Semantics.systemDateTime.lastChangedAt: true
    last_changed_at as LastChangedAt,
    _Incident,
    _StatusNext,
    _StatusPrevius
    
}
