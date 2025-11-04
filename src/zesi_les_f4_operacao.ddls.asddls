@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AJUDA F4 OPERAÇÃO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZESZI_LES_F4_OPERACAO as select from dd07t
{
    ddtext,
    domvalue_l
}
where domname = 'ZESZDM_OPER_AQUAV'
and ( domvalue_l = 'IM' or domvalue_l = 'VI' )
