@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS para la prioridades'
@Metadata.ignorePropagatedAnnotations: true

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

//Vista que se utiliza para el valor de ayuda de las prioridades, se basa en la tabla ZDT_PRIORITY_YM, donde se encuentran los datos de las prioridades
define view entity ZDD_PRIORITY_VH_YMIR
  as select from zdt_priority_ym
{
      @ObjectModel.text.element: [ 'PriorityDescription' ]
  key priority_code        as PriorityCode,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      priority_description as PriorityDescription
}
