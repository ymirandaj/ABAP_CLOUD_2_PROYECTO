@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status'
@Metadata.ignorePropagatedAnnotations: true

//configuración para la ayuda de búsqueda
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #MASTER
}
@ObjectModel.representativeKey: 'StatusCode'
@ObjectModel.dataCategory: #VALUE_HELP
@Search.searchable: true

@Consumption.ranked: true
@VDM.viewType: #BASIC
define view entity ZDD_STATUS_VH_YMIR as select from zdt_status_ymir
{
    
    @ObjectModel.text.element: [ 'StatusDescription' ]
    key status_code as StatusCode,
    
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Semantics.text: true
    status_description as StatusDescription
}
