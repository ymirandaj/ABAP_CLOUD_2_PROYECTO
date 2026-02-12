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

// Mejora: Agregar metadatos para UI semántica
@UI.headerInfo: {
    typeName: 'State',
    typeNamePlural: 'States'
}
define view entity ZDD_STATUS_VH_YMIR as select from zdt_status_ymir
{
    @UI.facet: [ { id: 'status', type: #IDENTIFICATION_REFERENCE } ]
    @ObjectModel.text.element: [ 'StatusDescription' ]
    @UI.lineItem: [ { position: 10, label: 'Status ID' } ]
    @UI.identification: [ { position: 10 } ]
    key status_code as StatusCode,
    
    @UI.lineItem: [ { position: 20, label: 'Status Description' } ]
    @UI.identification: [ { position: 20, label: 'Status Description' } ]
    @EndUserText.label: 'Status Description'
    @Search.defaultSearchElement: true
    //@Search.fuzzinessThreshold: 0.8
    @Semantics.text: true
    status_description as StatusDescription
    
 
      
     
  

   
     
}
