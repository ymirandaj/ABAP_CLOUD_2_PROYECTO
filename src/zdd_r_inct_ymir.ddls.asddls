@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS root incidents'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZDD_R_INCT_YMIR as select from zdt_inct_ymir
composition [0..*] of  ZDD_INCT_H_YMIR as _History 
association [0..1] to ZDD_STATUS_VH_YMIR  as _Status on _Status.StatusCode = $projection.Status 
association [0..1] to ZDD_PRIORITY_VH_YMIR as _Priority on _Priority.PriorityCode = $projection.Priority
{
    key inc_uuid as IncUuid,
    incident_id as IncidentId,
    title as Title,
    description as Description,
    status as Status,
    priority as Priority,
    creation_date as CreationDate,
    changed_date as ChangedDate,
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
    _History,
    _Status,
    _Priority
}
