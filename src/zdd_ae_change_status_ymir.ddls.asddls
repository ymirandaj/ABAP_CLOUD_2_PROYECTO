@EndUserText.label: 'Change State'
define abstract entity ZDD_AE_CHANGE_STATUS_YMIR
{
  
  @Consumption.valueHelpDefinition: [ {
                    entity.name: 'zdd_status_vh_ymir',
                    entity.element: 'StatusCode',
                    useForValidation: true
  } ]
    status : zde_status_ymir;

  @EndUserText.label: 'Add Observation Text'
  @UI.multiLineText: true 
  text   : zde_text_ymir;
}
