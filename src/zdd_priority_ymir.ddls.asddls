@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Priority'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZDD_PRIORITY_YMIR as select from zdt_priority_ym
{
    key priority_code as PriorityCode,
    priority_description as PriorityDescription
}
