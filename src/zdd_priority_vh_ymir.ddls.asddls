@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS para la prioridades'
@Metadata.ignorePropagatedAnnotations: true

//configuración para la ayuda de búsqueda
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #MASTER
}
@ObjectModel.representativeKey: 'PriorityCode'
@ObjectModel.dataCategory: #VALUE_HELP
@Search.searchable: true

@Consumption.ranked: true
@VDM.viewType: #BASIC


define view entity ZDD_PRIORITY_VH_YMIR as select from zdt_priority_ym
{
    @ObjectModel.text.element: [ 'PriorityDescription' ]
    key priority_code as PriorityCode,
    
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Semantics.text: true
    priority_description as PriorityDescription
}
