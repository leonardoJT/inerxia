	��V@<eMt8  � �                                              � 387400F0utf-8 MAIN D:\SPS\soportes\fodun\Prog\dsolprofina.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fHndle,widget-handle, TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,Agencia integer 0 0,NombreAgencia character 1 0,Ciudad character 2 0,NombreCiudad character 3 0,Fec_UltActualiza date 4 0,TpoCliente integer 5 0,OtroTpoCliente character 6 0,CptcionClccion character 7 0,PrdctoSlctar integer 8 0,OtroPrdctoSlctar character 9 0,Monto decimal 10 0,Plazo integer 11 0,Grntia character 12 0,Linea character 13 0,reestrctrcion logical 14 0,FrmaPgo character 15 0,dstncion character 16 0,Cuota decimal 17 0,RowNum integer 18 0,RowIdent character 19 0,RowMod character 20 0,RowIdentIdx character 21 0,RowUserProp character 22 0,ChangedFields character 23 0       r              �[             JN r  ��              ��              �K     +   |� t  W   �� D  X   4� �  Y   �   [   �   \   � 0  ]   �   ^   #    `   ? 0$ *  ISO8859-1                                                                           Xq   " �                                      �                   <�                �q      <   �   ��              ��  �   �q      �q                                                       PROGRESS                         �           
    
                    �              �                                                                                                     
  T         �          \  �S  �   �T     e  ���LV  a                     (           �'      �                bdcentral                        PROGRESS                         (     �        �                         X�vL            �  �                              �  �                      �  �  �      AGENCIANOMBREDIRECCIONTELEFONOFAXMODEMCIUDADZONAMENSAJEFEC_CREACIONFEC_RETIROESTADOFEC_TRABAJOID_CIERREFEC_CIERRENOM_LOGICOTIP_AGENCIAENTIDADVALMAX_INVERSIONESPORMAX_CONCENTRACIONINVVALMAX_CAJAVALMAX_BANCOSNIT_DIRECTOREMAILPERNOM_ACTUALDATAFONOUSRDATAFONO                                                             	          
                                                                                                                                                                                                           �        �                         X�vL            �  L�                              �  �                      �  �        UBICACIONNOMBRETIPOESTADOCOMUNA                                                   �     �        �                         X�vL            �  ��                              �  �                      8  �  �w     FEC_INGRESONITFEC_RETIROCOD_PROFESIONFEC_NACIMIENTOTEL_RESIDENCIALUGAR_RESIDENCIALUGAR_NACIMIENTOFEC_ULTACTUALIZACOD_ZONAEST_CIVILDIR_CORRESPONDENCIAESTRATOCOD_RETIRONIV_EDUCATIVOMED_PUBLICITARIOID_RETENCIONPER_ACARGOCALIFICACIONNUM_HIJOSREPORTADO_SUPERCOD_INGRESOCODIGO_CIIUFEC_INISANCIONREESTRUCTURADOSANCIONADOCOD_SEGMENTOGRAN_CONTRIBUYENTEAUT_CENTRALRIESGOESTADOUSUARIODIAS_SANCIONFEC_CALIFICACIONNOMBREAPELLIDO1APELLIDO2DIR_COMERCIALTEL_COMERCIALLUGAR_COMERCIALLUGAR_EXPEDICIONFEC_EXPEDICIONEMAILTIPO_VINCULOAGENCIAREPORTADO_FISCALIAREPORTADO_PROCREDITOCARNETCOD_CARGOFEC_INGEMPRESAING_ARRIENDOSCOD_EMPRESASALARIOTIP_CONTRATOING_HONORARIOSING_FINANCIEROSGTO_FAMILIARSDO_OBLIGACIONESACT_CASAACT_VEHICULOACT_INVERSIONTIPO_IDENTIFICACIONSEXOTIPO_CLIENTEGRUPOSUBGRUPOGARANTIACONOCIMIENTO_CLIENTEMORA_COMERCIALTIPO_ACTIVIDADRESPALDO_PATRIMGTO_ARRIENDOTIPO_VIVIENDAFEC_ASOCIACIONDIR_RESIDENCIACELULARFOTOGRAFIAFIRMAFEC_ACTFOTOFEC_ACTFIRMAULT_USUIMAGENGTO_OBLIGACIONNOM_ARRENDATARIOTEL_ARRENDATARIOING_OTROSCON_SOSPECHOSASEDADTIEMPO_ASOCIACIONTIEMPO_EMPRESAID_PREEXISTENTESGTOFINANC_INDIRENDEUD_INDIRECTOCAPACIDAD_PAGOPUNTAJEDESTINOID_PRIVILEGIADOID_PUEDECODEUDARCOD_ANTERIORANTERIOR_NITFEC_MODNITFEC_FALLECIDOID_EXONERADOSIPLAFECINI_NOSIPLAFECFIN_NOSIPLASUPERUNIDADDECAPTURASUPERSUBCUENTAITPCOD_ACTIVIDADID_CAB_FAMILIAID_MON_EXTGRADOJORNADACOLEGIOID_MICROSITUACION_ACTUALFUERZA_MAYORFEC_ULT_ACTTIPO_EMPRESANIV_EDUCOPNUM_ACTACOP                                                                                                                                                                                                                                                                       %          &          '          )          +          .          /          4          5          6          7          8          9          <          =          >          @          B          C          D          E          F          G          I          J          K          L          M          N          O          Q          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          � 	        �          �          �          x  �	      �  
    
                  �  �             d                                                                                          �	          
  $  �	      �  
    
                  �  T                                                                                                       �	          
  �  
      L  
    
                  8     	           �                                                                                          
          
  |  
      �  
    
                  �  �  
           h                                                                                          
          
  (  #
      �  
    
                  �  X                                                                                                       #
          
  �  5
      P  
    
                  <               �                                                                                          5
          
  �  J
      �  
    
                  �  �             l                                                                                          J
          
  ,  `
      �  
    
                  �  \                                                                                                       `
          
  �  n
      T                         @               �                                                                                          n
            �  {
                               �  �             p                                                                                          {
            0  �
      �  
    
                  �  `                                                                                                       �
          
  �  �
      X  
    
                  D               �                                                                                          �
          
  �  �
        
    
                  �  �             t                                                                                          �
          
  4  �
      �                        �  d                                                                                                        �
            �  �
      \                        H               �                                                                                          �
            �  �
                              �  �             x                                                                                          �
                �
      �                        �  8             $                                                                                          �
            �         �       p  X  Pc  .   �c  p  ��      �c         p             �W          hY      �              �       �  X  lp  /   �p  �   �      �p         �         �    8d          ,f      �                 ��                                               ��          �  �  L lp                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                 F                  G                  H                  I                  J                  K                  L                  M                  N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                                 �8  �8  �8  �8              �8             �8  �8  �8  �8              �8              9  ,9  09  T9  @9          X9             |9  �9  �9  �9              �9             �9  �9  �9  �9              �9             �9  :  :  :              :              D:  L:  X:  l:              p:             �:  �:  �:  �:                              �:  �:  �:  �:              �:             ;  ;  ;  8;  (;          <;              T;  `;  l;  �;  �;          �;              �;  �;  �;  �;              �;              <  (<  4<  L<              P<              l<  x<  �<  �<              �<              �<  �<  �<   =  =          $=              X=  h=  t=  �=              �=              �=  �=  �=  �=                              �=  >  >  ,>              0>              d>  p>  t>  �>              �>             �>  �>  �>  �>  �>          �>              ?  ?  ?  ,?              0?              \?  d?  l?  t?              x?              �?  �?  �?  �?              �?              �?  �?  �?  @              @            (@  0@  H@  \@              `@             �@  �@  �@  �@              �@            �@  �@  A  A               A             DA  TA  lA  �A              �A             �A  �A  �A  �A              �A             B  B  ,B  @B              DB             `B  lB  �B  �B              �B             �B  �B  �B  �B               C             C  $C  8C  PC              TC             pC  �C  �C  �C              �C             �C  �C  �C  �C              �C              D  $D  ,D  8D                              <D  HD  XD  dD                             hD  tD  |D  �D                              �D  �D  �D  �D                             �D  �D  �D  �D              �D              ,E  4E  @E  TE              XE              �E  �E  �E  �E              �E             �E  �E  �E  F              F              @F  LF  dF  pF              tF             �F  �F  �F  �F              �F              �F  �F  �F  �F              �F              G  G   G  (G              ,G             @G  PG  TG  dG                              hG  xG  �G  �G  �G          �G             �G  �G  �G  H  �G          H              0H  @H  DH  PH              TH              �H  �H  �H  �H              �H              �H  �H  I  $I              (I             PI  `I  dI  tI                              xI  �I  �I  �I                              �I  �I  �I  �I              �I              J  $J  (J  <J                              @J  TJ  \J  pJ                              tJ  �J  �J  �J                              �J  �J  �J  �J              �J              �J  �J  K  K              K             @K  LK  TK  `K                              dK  pK  xK  �K                              �K  �K  �K  �K                              �K  �K  �K  �K                              �K  �K  �K  �K                              �K  �K  �K  L                              L  L  $L  (L                              ,L  8L  PL  `L              dL             �L  �L  �L  �L              �L             �L  �L  �L  M              M             PM  `M  tM  �M              �M             �M  �M  �M  �M               N              <N  PN  \N  pN                             tN  �N  �N  �N                             �N  �N  �N  �N                              �N  �N  �N  O                             O  O   O  @O  4O          DO              TO  dO  lO  �O  |O          �O              �O  �O  �O  �O              �O             �O  P  P  0P  (P          4P             PP  \P  tP  �P  �P          �P             �P  �P  �P  �P  �P          �P             Q  Q  4Q  DQ  <Q          HQ             dQ  pQ  xQ  �Q              �Q              �Q  �Q  �Q  �Q  �Q                          �Q  �Q  �Q  R  R                           R  ,R  8R  DR                              HR  PR  XR  `R                              dR  xR  �R  �R                              �R  �R  �R  �R                              �R  �R  �R  �R                              �R  �R  �R   S              S              S   S  0S  <S                             @S  LS  \S  hS                             lS  xS  �S  �S              �S              �S  �S  �S  �S              �S                                                         Agencia 999 Agencia 0   Agencia de radicaci�n del credito   Nit X(12)   Nit     N�mero de identificaci�n del cliente    Cod_Credito 999 Codigo Producto C�digo de Producto  0   Linea a la que pertenece el cr�dito Num_Credito 999999999   Num_Credito 0   N�mero de Cr�dito   Tip_Credito 9   Tipo de Producto Cs/Cm/H    1   Clase de Cr�dito    Pagare  X(14)   Pagare  ?   n�mero de pagar� que respalda el cr�dito    Tasa    >>9.999999  Tasa del Credito    0   tasa de inter�s de desembolso   Fec_Aprobacion  99/99/9999  Aprobaci�n  ?   Fec_Desembolso  99/99/9999  Fecha Aprobacion    ?   Fecha desembolso    Fec_Pago    99/99/9999  Fecha Pago  Fecha de Pago   ?   Fecha de pago de cuota  Fec_UltPago 99/99/9999  Fecha Ultima Pago   Fecha de Ultimo Pago    ?   fecha �ltimo pago Cuota Fec_Calificacion    99/99/9999  Fecha Calificaci�n  ?   Fecha Ultima calificaci�n de Cartera    Fec_Reestructurado  99/99/9999  Fecha de Reestructurado ?   Fecha de reestructuraci�n   Fec_PagAnti 99/99/9999  Fecha Limite Renueva Liquidaci�n    ?   Fecha l�miite para renovar cobro Inter�s    Fec_DifCobro    99/99/9999  Fecha de Dificil Cobro  Fecha de Dif Cobro  ?   fecha en que el cr�dito pas� a ser de dif�cil cobro Fec_CanceTotal  99/99/9999  Fecha de Cancelaci�n Total  ?   Fecha cancelaci�n total del cr�dito Fec_UltLiquidacion  99/99/9999  Fec_UltLiquidacion  ?   Fec_ProxLiquidacion 99/99/9999  Fecha Inicio Periodiciad    ?   fecha de inicio del per�odo para cobro de inter�s   For_Pago    9   Forma de Pago   1   Forma de pago de los abonos del cr�dito For_Interes 9   Periodo Deducci�n   Tipo de Interes V/A 1   Ingrese el per�odo de deducci�n Per_Pago    9   Periodo de Pago 1   periodicidad de pago de cuota del cr�dito   Plazo   >>>9    Plazo   0   plazo  del cr�dito  Usuario X(4)    Usuario     c�digo del usurio que matricul� el cr�dito  Cuota   ->>>>>,>>>,>>>,>>9.99   Cuota del Credito   0   Cuota Mensual del cr�dito   Monto   ->>>>>,>>>,>>>,>>9.99   Monto de Credito    0   Valor de Desembolso del cr�dito Sdo_Capital ->>>>>,>>>,>>>,>>9.99   Saldo Capital   0   Saldo de capital actual de deuda    Int_Corrientes  ->>>>>,>>>,>>>,>>9.99   Intereses Corrientes    0   Valor de los intereses corrientes   Int_Anticipado  ->>>>>,>>>,>>>,>>9.99   Interes Anticipado  0   Interes anticipado  Int_MorCobrar   ->>>>>,>>>,>>>,>>9.99   Interes de Mora por Cobrar  0   Valor de inter�s de mora por cobrar Int_DifCobro    ->>>>>,>>>,>>>,>>9.99   Interes Dif. Cobro  0   Inter�s de dificil cobro    Sdo_CapPag  ->>>>>,>>>,>>>,>>9.99   Acumulado de Capital Pago   0   Capital pagado al cr�dito   Sdo_IntPag  ->>>>>,>>>,>>>,>>9.99   Saldo Intereses pagados 0   Saldo intereses pagados Sdo_IntMor  ->>>,>>>,>>>,>>9    Saldo Intereses Mora    0   Saldo de intereses de mora  Sdo_Proyectado  ->>>>>,>>>,>>>,>>9.99   Sdo Proyectado  0   Ingrese el valor proyectado del cr�dito Cuo_Pagadas 999 Cuotas Pagadas  0   Cuotas pagadas por el cr�dito   Cuo_Atraso  9999    Cuo_Atraso  0   Val_Atraso  >>,>>>,>>>,>>9  Val_Atraso  0   Dias_Atraso 99999   Dias_Atraso 0   Provision   >>>,>>>,>>9 Provision   0   Cod_Califica    99999   Calificacion del Cr�dito    00001   Categor�a dada por la calificaci�n de cartera de cr�ditos   Poliza  >>>>>>>9    Numero de Poliza    0   Ingrese el n�mero de p�liza que tiene el cr�dito    Deducible   ->>>>>,>>>,>>>,>>9.99   Deducibles  0   Sumatoria de deducibles al cr�dito  Observaciones   X(50)   Observaciones       Ingrese las observaciones que tiene el cr�dito  Incremento  ->>>>>,>>>,>>>,>>9.99   Incremento  0   Ingrese el incremento   Destino 99999   Destino 0   Ingrese el destino del cr�dito  Sistema 99999   Sistema Liquidaci�n 0   Ingrese el sistema de liquidaci�n   Estado  9   Estado  1   Estado del cr�dito  Detalle_Estado  99  Detalle_Estado  1   Num_Solicitud   99999999    Nro Solicitud   N�mero de Solicitud 0   Ingrese el n�mero de solicitud de cr�dito   Per_Gracia  >>>9    Per.Gracia  Peri�do de Gracia   0   Ingrese el per�odo de gracia    Id_Adicionales  9   Deducciones 1   Si el valor adicional se suma o no al total del pr�stamo.   Reestructurado  9   Reestructurado  2   Si el cr�dito ha sido reestructurado    Int_AntDesembolso   ->>>>>,>>>,>>>,>>9.99   Interes Ant. Desembolso 0   Intereses por correr la primera Cuota   Age_Desembolso  999 Age_Desembolso  0   Cod_Desembolso  999 Cod_Desembolso  0   Cue_Desembolso  X(14)   Cuenta de Ahorros       Ingrese la Cuenta de ahorros donde se consigna el valor credito Age_DebAutomatico   999 Age_DebAutomatico   0   Cue_DebAutomatico   X(14)   Cue_DebAutomatico       Cod_DebAutomatico   999 Cod_DebAutomatico   0   Desembolso  9   Tipo de Desembolso  2   Forma del Desembolso del Cr�dito    Costas  ->>>>>,>>>,>>>,>>9.99       0   Ingrese el valor aprobado del cr�dito   Nit_Juzgado X(14)   Nit_Juzgado     Nom_Juzgado X(50)   Nom_Juzgado     Abogado yes/no  Abogado no  Honorarios  >>>,>>>,>>9 Honorarios  0   Polizas >>>,>>>,>>9 Polizas 0   Categoria   X   Categoria       Cta_Contable    X(14)           Sdo_Anuales ->>>>>,>>>,>>>,>>9.99   Saldos!Anuales  0   Ingrese los saldo mensuales Capital_Acum    ->>>,>>>,>>>,>>9.99 Capital Acumulado   0   Capital Acumulado cumplido  Int_LiqAcum ->>>,>>>,>>>,>>9.99 Int_Liquidado Acumulado 0   Int_Liquidado Acumulado hasta el ultimo periodo cumplido    Int_MoraDifCob  ->,>>>,>>>,>>9.99   Mora Dif.Cobro por Cobrar   0   Valor de inter�s de mora por cobrar(Dif.Cobro)  Cod_CalificaMes 99999   Calificacion del Cr�dito    00001   Categor�a dada por la calificaci�n de cartera de cr�ditos   Provision_Interes   >>>,>>>,>>9 Provision_Interes   0   Provision_Otros >>>,>>>,>>9 Costas Polizas y Honorarios 0   CategoriaMes    X   Categoria       Val_Desembolso  >>>>,>>>,>>9.99 Val_Desembolso  0   Fec_Bloqueo 99/99/9999  Fecha de Bloqueo    Fec_Bloqueo ?   Fecha Bloqueo   Usuario_gestor  X(4)    Usuario Gestor      ?   Usuario que hizo la negociaci�n Tasa_Desembolso >>9.999999  Tasa anual nominal  0   tasa de inter�s de desembolso   Seg_Cartera ->>>>>,>>>,>>>,>>9.99   Seg.cartera SegCart 0   Valor del seguro de cartera Com_Bruta   ->>>>>,>>>,>>>,>>9.99   Com.Bruta   ComBruta    0   Valor de la Comisi�n Bruta  Com_Adicional   ->>>>>,>>>,>>>,>>9.99   Com.Adic    ComAdicional    0   Valor de la Comisi�n Adicional  GmfxC   ->>>>>,>>>,>>>,>>9.99   GmfxC   GmfxC   0   Valor de la Cuota de Manejo TarjetaDB   X(16)   TarjetaDB       Numero de la Tarjeta D�bito Fec_BloqueoTdb  99/99/9999  Fec.Bloqueo FecBloqueo  ?   Fec_CreaTdb 99/99/9999  Creacion de Tarjeta DB  aper.TarjetaDB  ?   Fec_CancTdb 99/99/9999  FecCancTdb  ?   diapago >>>,>>9 diapago 0   cod_calificareest   >>>,>>9 cod_calificareest   0   cod_calant  >9  cod_calant  0   cod_calact  >9  cod_calact  0   respaldoaportes yes/no  respaldoaportes no  respaldoaportes Comision    ->>>,>>>,>>9.99 Comision    0   seg_vida    ->>>,>>>,>>9    seg_vida    0   Num_Control 999999999   Num_Control 0   N�mero de Cr�dito   Tasa_Ant    >>9.999999  Tasa del Credito    0   tasa de inter�s de desembolso   �    3 D Y y ��  ���b������    � �����������                                         �             �       ��      ���             �)        �)        �)        �)        �)        �)        �)        �)        �)        �)        *                �     i  i  i     i  i     i  i  i  i     i  i  i  i      i  i  i      i  i  i     i   i  i  i     i  i     i     i   i 	 i     i   i 
 i     	 	2 	 	0 	
 	 	 	 	 	 	    #   +   /   ;   G   S   Z   _   n   }   �   �   �   �   �   �   �   �         #  )  1  7  =  I  X  g  u  �  �  �  �  �  �  �  �  �  �  �  �      !  )  1  8  G  U  `  o  ~  �  �  �  �  �  �  �  �        %  0  8  B  O  [  h  t  �  �  �  �  �  �  �  �  �         &  0  ?  K  W  _  q  |  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                             �]  �]  �]  �]  �]          �]             �]  �]  �]  �]  �]          �]             ^  ^  ^  $^  ^          (^             0^  @^  H^  h^  X^          l^             �^  �^  �^  �^  �^          �^             _  _  _  8_  (_          <_             L_  \_  d_  �_  x_          �_             �_  �_  �_  �_  �_                         �_  `  `  <`  $`          @`             X`  l`  t`  �`  �`          �`             �`  �`  �`  �`  �`          �`              a  a  a   a  a          $a             <a  Da  La  da  Xa          ha             �a  �a  �a  �a  �a                         �a  �a  �a  b  �a                         b  b  b  8b  (b                         <b  Hb  Pb  hb  \b                         lb  tb  �b  �b  �b          �b             �b  �b  �b  �b                             �b  �b  �b  �b                              �b  �b  �b  c                             c  c  c  (c                             ,c  8c  @c  Lc                                                                          Agencia 999 Agencia Agencia 0   Agencia NombreAgencia   X(40)   Nombre Agencia  Nombre!Agencia      Nombre De La Agencia    Ciudad  X(8)    Ciudad  Ciudad      Ciudad  NombreCiudad    X(40)   Nombre Ciudad   Nombre!Ciudad       Ingrese el nombre de la ubicaci�n   Fec_UltActualiza    99/99/9999  Ultima Actualizaci�n    Ultima!Actualizaci�n    TODAY   Fecha de actualizaci�n de datos TpoCliente  999 Tipo Cliente    Tipo!Cliente    0   Tipo De Cliente OtroTpoCliente  X(50)   Otro Tipo Cliente   Otro Tipo Cliente       Ingrese las observaciones que tiene el cr�dito  CptcionClccion  X   Clase Producto  Clase!Producto      PrdctoSlctar    9   Producto A Solicitar    Producto!A Solicitar    1   Producto A Solicitar    OtroPrdctoSlctar    X(14)   Otro Producto A Solicitar   Otro Producto!A Solicitar   ?   Cual Otro Producto? Monto   ->>>>>,>>>,>>>,>>9.99   Monto   Monto   0   Monto   Plazo   >>>9    Plazo   Plazo   0   Producto A Solicitar    Grntia  X(14)   Garant�a    Garant�a        Ingrese la Cuenta de ahorros donde se consigna el valor credito Linea   X(14)   L�nea   L�nea       reestrctrcion   S/N Reestructuraci�n    Reestructuraci�n    no  FrmaPgo X   Forma De Pago   Forma De Pago       dstncion    X(14)   Destinaci�n Destinaci�n     Cuota   ->>>>>,>>>,>>>,>>9.99   Cuota   Cuota   0   Cuota   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �   �  ���������    �   �                   1(        A(        H(                �     i     i     i     	 	 	    #   �'  �'  Q&  ^&  �'  �'  �'  �'  �'  7  #  (  (  (   (  ((  1  1(  8(  A(  H(  T(                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                               xj  �j  �j  �j  �j          �j             �j  �j  �j  �j  �j          �j             �j  �j  k  k  k          k              k  0k  8k  Xk  Hk          \k             �k  �k  �k  �k  �k          �k             �k  l  l  (l  l          ,l             <l  Ll  Tl  |l  hl          �l             �l  �l  �l  �l  �l                         �l  �l  �l  ,m  m          0m             Hm  \m  dm  �m  �m          �m             �m  �m  �m  �m  �m          �m             �m  �m   n  n  n          n             ,n  4n  <n  Tn  Hn          Xn             �n  �n  �n  �n  �n                         �n  �n  �n  �n  �n                         �n  o  o  (o  o                         ,o  8o  @o  Xo  Lo                         \o  do  |o  �o  �o          �o             �o  �o  �o  �o                             �o  �o  �o  �o                              �o  �o  �o  �o                             �o  p  p  p                             p  (p  0p  <p                              @p  Pp  Xp  hp                                                                          Agencia 999 Agencia Agencia 0   Agencia NombreAgencia   X(40)   Nombre Agencia  Nombre!Agencia      Nombre De La Agencia    Ciudad  X(8)    Ciudad  Ciudad      Ciudad  NombreCiudad    X(40)   Nombre Ciudad   Nombre!Ciudad       Ingrese el nombre de la ubicaci�n   Fec_UltActualiza    99/99/9999  Ultima Actualizaci�n    Ultima!Actualizaci�n    TODAY   Fecha de actualizaci�n de datos TpoCliente  999 Tipo Cliente    Tipo!Cliente    0   Tipo De Cliente OtroTpoCliente  X(50)   Otro Tipo Cliente   Otro Tipo Cliente       Ingrese las observaciones que tiene el cr�dito  CptcionClccion  X   Clase Producto  Clase!Producto      PrdctoSlctar    9   Producto A Solicitar    Producto!A Solicitar    1   Producto A Solicitar    OtroPrdctoSlctar    X(14)   Otro Producto A Solicitar   Otro Producto!A Solicitar   ?   Cual Otro Producto? Monto   ->>>>>,>>>,>>>,>>9.99   Monto   Monto   0   Monto   Plazo   >>>9    Plazo   Plazo   0   Producto A Solicitar    Grntia  X(14)   Garant�a    Garant�a        Ingrese la Cuenta de ahorros donde se consigna el valor credito Linea   X(14)   L�nea   L�nea       reestrctrcion   S/N Reestructuraci�n    Reestructuraci�n    no  FrmaPgo X   Forma De Pago   Forma De Pago       dstncion    X(14)   Destinaci�n Destinaci�n     Cuota   ->>>>>,>>>,>>>,>>9.99   Cuota   Cuota   0   Cuota   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �   �  ���������    �   �                   1(        A(        H(                �     i     i     i     	 	 	    #   �'  �'  Q&  ^&  �'  �'  �'  �'  �'  7  #  (  (  (   (  ((  1  1(  8(  A(  H(  T(  `(    ��                            ����                            /'    ��                    %�    *   ��                    ��    undefined                                                               �       ��  x   `   ��  ��                    �����               �cq        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /     �                                3   ����       $      8  ���                       8      
                       � ߱        x  �      D       �     @          �  �   H      �  9   J      �                         � ߱          $   K   �  ���                       fHndle  ��     �   $  �          4   ����                �                      ��                  �   �                   �$>           �   4  �  	  �   �                                        3   ����$      O   �   ��  ��  0  batchServices                               �  p      ��                  �  �  �              <�=        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            clientSendRows                              �  �      ��                  �  �  �              @>        O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��   �             P               ��   �             x               ��                  �           ��                            ����                            commitTransaction                               �  |      ��                  �  �  �              t*>        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             �  p      ��                  �  �  �              (+>        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              ��=        O   ����    e�          O   ����    R�          O   ����    ��            ��                   	           ��                            ����                            describeSchema                              �	  �	      ��                  �  �  
              4�=        O   ����    e�          O   ����    R�          O   ����    ��            ��   T
              
               �� 
          �       H
  
         ��                            ����                            destroyServerObject                             <  $      ��                  �  �  T              $�/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T              ȍ/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              0        ��                  �  �  H              ��/        O   ����    e�          O   ����    R�          O   ����    ��            ��                  `           ��                            ����                            fetchFirst                              L  4      ��                  �  �  d              Ԋ/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               @  (      ��                  �  �  X              ��/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               4        ��                  �  �  L              h�/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               (        ��                  �  �  @              ��/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                                       ��                  �  �  8              ��/        O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            home                                8         ��                  �  �  P              |�/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                4        ��                  �  �  L              �/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              4        ��                  �  �  L              ؈        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             ,        ��                  �  �  D              �؈        O   ����    e�          O   ����    R�          O   ����    ��            ��                  \           ��                            ����                            printToCrystal                              L  4      ��                  �  �  d              h��        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                  �           ��                            ����                            refreshRow                              �  �      ��                  �  �  �              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              �  �      ��                  �  �  �              �d8        O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��   <                            ��   d             0               ��   �             X               ��   �             �               ��   �             �               �� 
        �       �  
             ��                  �           ��                            ����                            restartServerObject                             �  �      ��                  �  �                <��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              x��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            saveContextAndDestroy                                 �      ��                  �  �                �˵        O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            serverSendRows                              $          ��                      <               ���        O   ����    e�          O   ����    R�          O   ����    ��            ��   �              T                ��   �              |                ��   �              �                ��    !             �                ��   (!             �                �� 
          �       !  
         ��                            ����                            serverFetchRowObjUpdTable                               "   "      ��                      0"              �(�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       H"  
         ��                            ����                            setPropertyList                             8#   #      ��                      P#              -�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  h#           ��                            ����                            serverSendRows                              X$  @$      ��                      p$              p1�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �$             �$               ��   �$             �$               ��   %             �$               ��   4%              %               ��   \%             (%               �� 
          �       P%  
         ��                            ����                            startServerObject                               D&  ,&      ��                       \&              �@�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                <'  $'      ��                  "  %  T'              �E�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �'             l'               ��                  �'           ��                            ����                            submitForeignKey                                �(  p(      ��                  '  +  �(              �A�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��   )             �(               ��                  )           ��                            ����                            submitValidation                                �)  �)      ��                  -  0  *              �Q�        O   ����    e�          O   ����    R�          O   ����    ��            ��   `*             ,*               ��                  T*           ��                            ����                            synchronizeProperties                               L+  4+      ��                  2  5  d+              �W�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             |+               ��                  �+           ��                            ����                            transferToExcel                             �,  |,      ��                  ?  D  �,              D^�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��    -             �,               ��   H-             -               ��                  <-           ��                            ����                            undoTransaction                             ,.  .      ��                  F  G  D.              �g�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             (/  /      ��                  I  L  @/              \j�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �/             X/               ��                  �/           ��                            ����                            updateQueryPosition                             t0  \0      ��                  N  O  �0              �p�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             h1  P1      ��                  Q  S  �1              �s�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �1           ��                            ����                            addRow           2      (2    H      CHARACTER,INPUT pcViewColList CHARACTER cancelRow   2      P2      |2   	 O      CHARACTER,  canNavigate \2      �2      �2    Y      LOGICAL,    closeQuery  �2      �2      �2   
 e      LOGICAL,    columnProps �2      �2      $3    p      CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   3      d3      �3   	 |      CHARACTER,INPUT pcViewColList CHARACTER copyRow p3      �3      �3    �      CHARACTER,INPUT pcViewColList CHARACTER createRow   �3      4      44   	 �      LOGICAL,INPUT pcValueList CHARACTER deleteRow   4      X4      �4  	 	 �      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    d4      �4      �4  
  �      CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   �4      5      @5    �      CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow  5      �5      �5    �      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    �5      �5       6    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds �5      X6      �6    �      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  d6      �6      �6    �      CHARACTER,  hasForeignKeyChanged    �6      �6      (7    �      LOGICAL,    openDataQuery   7      47      d7          LOGICAL,INPUT pcPosition CHARACTER  openQuery   D7      �7      �7   	       LOGICAL,    prepareQuery    �7      �7      �7          LOGICAL,INPUT pcQuery CHARACTER rowAvailable    �7      8      @8    +      LOGICAL,INPUT pcDirection CHARACTER rowValues    8      d8      �8   	 8      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   p8      �8      9   	 B      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   �8      X9      �9   	 L      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   d9      �9      �9    V      CHARACTER,  assignDBRow                             �:  l:      ��                  9  ;  �:              ,��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �:  
         ��                            ����                            bufferCopyDBToRO                                �;  �;      ��                  =  B  �;              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <             �;  
             �� 
  4<              <  
             ��   \<             (<               ��                  P<           ��                            ����                            compareDBRow                                @=  (=      ��                  D  E  X=              h��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             8>   >      ��                  G  I  P>              Ȧ�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  h>           ��                            ����                            dataAvailable                               X?  @?      ��                  K  M  p?              0��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �?           ��                            ����                            fetchDBRowForUpdate                             |@  d@      ��                  O  P  �@              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              pA  XA      ��                  R  S  �A              4��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               dB  LB      ��                  U  V  |B              t��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               XC  @C      ��                  X  Y  pC              |��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               LD  4D      ��                  [  \  dD              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              LE  4E      ��                  ^  `  dE              0��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 |E  
         ��                            ����                            initializeObject                                pF  XF      ��                  b  c  �F              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                hG  PG      ��                  e  g  �G              �Ð        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �G  
         ��                            ����                            releaseDBRow                                �H  pH      ��                  i  j  �H              Hʐ        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             |I  dI      ��                  l  m  �I              �ʐ        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               tJ  \J      ��                  o  r  �J              �ː        O   ����    e�          O   ����    R�          O   ����    ��            ��   �J             �J               ��                  �J           ��                            ����                            addQueryWhere   �9      4K      dK    w      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    DK      �K      �K    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO �K      HL      |L    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   \L      �L      M    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  �L      XM      �M    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  hM      �M      �M    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �M       N      8N    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable N      \N      �N     �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    hN      �N      �N  !  �      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �N       O      4O  "        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  O      \O      �O  #        HANDLE,INPUT pcColumn CHARACTER excludeColumns  lO      �O      �O  $  ,      CHARACTER,INPUT iTable INTEGER  getDataColumns  �O      �O      ,P  %  ;      CHARACTER,  getForeignValues    P      8P      lP  &  J      CHARACTER,  getQueryPosition    LP      xP      �P  '  [      CHARACTER,  getQuerySort    �P      �P      �P  (  l      CHARACTER,  getQueryString  �P      �P      $Q  )  y      CHARACTER,  getQueryWhere   Q      0Q      `Q  *  �      CHARACTER,  getTargetProcedure  @Q      lQ      �Q  +  �      HANDLE, indexInformation    �Q      �Q      �Q  ,  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �Q      8R      lR  -  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  LR      �R      �R  .  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    �R      �S      �S  /  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �S      0T      `T  0  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  @T      �T      �T  1  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �T      $U      TU  2        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    4U      |U      �U  3        LOGICAL,    removeQuerySelection    �U      �U      �U  4  )      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   �U      4V      dV  5  >      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  DV      �V      �V  6 
 L      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �V      �V      W  7  W      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    �V      dW      �W  8  f      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    xW      �W      �W  9  w      LOGICAL,INPUT pcSort CHARACTER  setQueryString  �W      X      <X  :  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   X      dX      �X  ;  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   tX      �X      �X  <  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �Y  xY      ��                      �Y              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  pZ      ��                      �Z              X�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �[  l[      ��                      �[              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �\  h\      ��                      �\              \�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �]  h]      ��                      �]              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             |^  d^      ��                      �^              p�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             t_  \_      ��                       �_              ��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �_  
         ��                            ����                            startServerObject                               �`  �`      ��                  "  #  �`              4�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �a  xa      ��                  %  '  �a              T�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �a           ��                            ����                            getAppService   �X      (b      Xb  =  �      CHARACTER,  getASBound  8b      db      �b  > 
 �      LOGICAL,    getAsDivision   pb      �b      �b  ?  �      CHARACTER,  getASHandle �b      �b      c  @  �      HANDLE, getASHasStarted �b      c      <c  A  �      LOGICAL,    getASInfo   c      Hc      tc  B 	 �      CHARACTER,  getASInitializeOnRun    Tc      �c      �c  C   	      LOGICAL,    getASUsePrompt  �c      �c      �c  D  	      LOGICAL,    getServerFileName   �c       d      4d  E  $	      CHARACTER,  getServerOperatingMode  d      @d      xd  F  6	      CHARACTER,  runServerProcedure  Xd      �d      �d  G  M	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �d      �d      ,e  H  `	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   e      Te      �e  I  n	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle de      �e      �e  J  |	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �e      �e       f  K 	 �	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun     f      @f      xf  L  �	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  Xf      �f      �f  M  �	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �f      �f       g  N  �	      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode   g      Dg      |g  O  �	      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             ,h  h      ��                  �  �  Dh              �S�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �h             \h  
             ��   �h             �h               �� 
                 �h  
         ��                            ����                            addMessage                              �i  �i      ��                  �  �  �i              �b�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �i             �i               ��   $j             �i               ��                  j           ��                            ����                            adjustTabOrder                              k  �j      ��                  �  �   k              4j�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  lk             8k  
             �� 
  �k             `k  
             ��                  �k           ��                            ����                            applyEntry                              tl  \l      ��                  �  �  �l              �q�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �l           ��                            ����                            changeCursor                                �m  |m      ��                       �m              0v�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �m           ��                            ����                            createControls                              �n  �n      ��                      �n              �z�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �o  �o      ��                      �o              �}�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �p  �p      ��                  
    �p              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �q  �q      ��                      �q              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �r  �r      ��                      �r              0��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �s  ts      ��                      �s              ̄�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �t  pt      ��                      �t              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �u  lu      ��                      �u              Ј�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �u             �u  
             ��   v             �u               ��   8v             v               ��                  ,v           ��                            ����                            modifyUserLinks                             w  w      ��                     $  4w              H��        O   ����    e�          O   ����    R�          O   ����    ��            ��   �w             Lw               ��   �w             tw               �� 
                 �w  
         ��                            ����                            removeAllLinks                              �x  tx      ��                  &  '  �x              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �y  hy      ��                  )  -  �y              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �y             �y  
             ��   z             �y               �� 
                  z  
         ��                            ����                            repositionObject                                �z  �z      ��                  /  2  {              L��        O   ����    e�          O   ����    R�          O   ����    ��            ��   X{             ${               ��                  L{           ��                            ����                            returnFocus                             8|   |      ��                  4  6  P|              P��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 h|  
         ��                            ����                            showMessageProcedure                                `}  H}      ��                  8  ;  x}              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��   �}             �}               ��                  �}           ��                            ����                            toggleData                              �~  �~      ��                  =  ?  �~              |��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �~           ��                            ����                            viewObject                              �  �      ��                  A  B  �              <��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  \g      0�      \�  P 
 -      LOGICAL,    assignLinkProperty  <�      h�      ��  Q  8      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   |�      �      $�  R  K      CHARACTER,  getChildDataKey �      0�      `�  S  Y      CHARACTER,  getContainerHandle  @�      l�      ��  T  i      HANDLE, getContainerHidden  ��      ��      ܁  U  |      LOGICAL,    getContainerSource  ��      �      �  V  �      HANDLE, getContainerSourceEvents    ��      $�      `�  W  �      CHARACTER,  getContainerType    @�      l�      ��  X  �      CHARACTER,  getDataLinksEnabled ��      ��      ��  Y  �      LOGICAL,    getDataSource   ��      �      �  Z  �      HANDLE, getDataSourceEvents ��      $�      X�  [  �      CHARACTER,  getDataSourceNames  8�      d�      ��  \        CHARACTER,  getDataTarget   x�      ��      ԃ  ]        CHARACTER,  getDataTargetEvents ��      ��      �  ^  #      CHARACTER,  getDBAware  �       �      L�  _ 
 7      LOGICAL,    getDesignDataObject ,�      X�      ��  `  B      CHARACTER,  getDynamicObject    l�      ��      ̄  a  V      LOGICAL,    getInstanceProperties   ��      ؄      �  b  g      CHARACTER,  getLogicalObjectName    ��      �      T�  c  }      CHARACTER,  getLogicalVersion   4�      `�      ��  d  �      CHARACTER,  getObjectHidden t�      ��      Ѕ  e  �      LOGICAL,    getObjectInitialized    ��      ܅      �  f  �      LOGICAL,    getObjectName   �       �      P�  g  �      CHARACTER,  getObjectPage   0�      \�      ��  h  �      INTEGER,    getObjectParent l�      ��      Ȇ  i  �      HANDLE, getObjectVersion    ��      І      �  j  �      CHARACTER,  getObjectVersionNumber  �      �      H�  k        CHARACTER,  getParentDataKey    (�      T�      ��  l        CHARACTER,  getPassThroughLinks h�      ��      ȇ  m  .      CHARACTER,  getPhysicalObjectName   ��      ԇ      �  n  B      CHARACTER,  getPhysicalVersion  �      �      L�  o  X      CHARACTER,  getPropertyDialog   ,�      X�      ��  p  k      CHARACTER,  getQueryObject  l�      ��      Ȉ  q  }      LOGICAL,    getRunAttribute ��      Ԉ      �  r  �      CHARACTER,  getSupportedLinks   �      �      D�  s  �      CHARACTER,  getTranslatableProperties   $�      P�      ��  t  �      CHARACTER,  getUIBMode  l�      ��      ĉ  u 
 �      CHARACTER,  getUserProperty ��      Љ       �  v  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ��      (�      `�  w  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles @�      ��      ��  x  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      ؊      �  y        CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �      D�      p�  z        CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   P�      ܋      �  {        CHARACTER,INPUT piMessage INTEGER   propertyType    �      0�      `�  |  +      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  @�      ��      ��  }  8      CHARACTER,  setChildDataKey ��      Č      �  ~  G      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  Ԍ      �      P�    W      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  0�      p�      ��  �  j      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      č       �  �  }      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ��      $�      X�  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   8�      ��      ��  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      Ў      �  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      ,�      `�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   @�      ��      ��  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      ܏      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      4�      `�  � 
       LOGICAL,INPUT lAware LOGICAL    setDesignDataObject @�      ��      ��  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      ܐ      �  �         LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      ,�      d�  �  1      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    D�      ��      ��  �  G      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      ܑ      �  �  \      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      4�      d�  �  n      LOGICAL,INPUT pcName CHARACTER  setObjectParent D�      ��      ��  �  |      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      Ԓ      �  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      0�      d�  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks D�      ��      ��  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ��      �  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      8�      l�  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute L�      ��      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      @�      |�  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  \�      ��      ̕  � 
 '      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �  2      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      \�      ��  �  B      LOGICAL,INPUT pcMessage CHARACTER   Signature   h�      ��      ؖ  � 	 N      CHARACTER,INPUT pcName CHARACTER    ��     X  �  ��          4   ����D                ��                      ��                  Y  �                  ��           Y  $�         Z  ��  �          4   ����T                ,�                      ��                  [  �                  0�           [  ��  �     r  D�  ��          4   ����h                Ę                      ��                  ~  �                  ��           ~  T�                                           �     
                    � ߱        H�  $   �  �  ���                           $   �  t�  ���                                                 � ߱        ��     �  ��  (�          4   ����0                8�                      ��                  �  O	                  p�           �  ș  l�  o   �      ,                                 Ě  $   �  ��  ���                       �  @         �              � ߱        ؚ  �   �  �      �  �   �  8       �  �   �  �      �  �   �         (�  �   �  �      <�  �   �        P�  �   �  �      d�  �   �  �      x�  �   �  4      ��  �   �  �      ��  �   �  $	      ��  �   �  �	      ț  �   �  
      ܛ  �   �  X
      �  �   �  �
      �  �   �  H      �  �   �  �      ,�  �   �  �      @�  �   �  4      T�  �   �  �      h�  �   �        |�  �   �  �      ��  �   �        ��  �   �  �      ��  �   �        ̜  �   �  x      ��  �   �  �      ��  �   �  (      �  �   �  �      �  �   �  �      0�  �   �  L      D�  �   �  �      X�  �   �  �      l�  �   �         ��  �   �  <      ��  �   �  �      ��  �   �  �      ��  �   �  0      Н  �   �  l      �  �   �  �      ��  �   �  �      �  �   �          �  �   �  \      4�  �   �  �          �   �  �                      T�          ��  ��      ��                  v	  �	  ؞              ���        O   ����    e�          O   ����    R�          O   ����    ��      D     
                �                     �                         � ߱        ��  $  �	  �  ���                           O   �	  ��  ��                 �          ܟ  �    ̟                                             ��                            ����                                �9      H�      ��     V     ��                       �  V                     @�     �	  ��  �          4   ����                (�                      ��                  �	  K
                  ���           �	  ��  <�  �   �	  |      P�  �   �	  �      d�  �   �	  l      x�  �   �	  �      ��  �   �	  d      ��  �   �	  �      ��  �   �	  T      ȡ  �   �	  �      ܡ  �   �	  L      �  �   �	  �      �  �   �	  <      �  �   �	  �      ,�  �   �	  4          �   �	  �      ��     {
  X�  Ȣ          4   ����                 آ                      ��                  |
                    41�           |
  h�  �  �   ~
  �       �  �   
  �      �  �   �
  x      (�  �   �
  �      <�  �   �
  h       P�  �   �
  �       d�  �   �
  !      x�  �   �
  �!      ��  �   �
   "      ��  �   �
  <"      ��  �   �
  x"      ȣ  �   �
  �"      ܣ  �   �
  (#      �  �   �
  �#      �  �   �
   $      �  �   �
  �$      ,�  �   �
  %      @�  �   �
  �%      T�  �   �
  &      h�  �   �
  �&      |�  �   �
   '      ��  �   �
  <'      ��  �   �
  x'      ��  �   �
  �'      ̤  �   �
  h(      �  �   �
  �(      ��  �   �
  `)      �  �   �
  �)      �  �   �
  X*      0�  �   �
  �*      D�  �   �
  +      X�  �   �
  �+      l�  �   �
   ,      ��  �   �
  <,      ��  �   �
  �,      ��  �   �
  ,-      ��  �   �
  �-      Х  �   �
  .      �  �   �
  �.      ��  �   �
  /      �  �   �
  �/       �  �   �
  �/      4�  �   �
  p0      H�  �   �
  �0      \�  �   �
  `1      p�  �   �
  �1      ��  �   �
  X2          �   �
  �2      L�     (  ��   �          4   �����2                0�                      ��                  )  �                  4 �           )  ��  D�  �   -  \3      X�  �   .  �3      l�  �   /  L4      ��  �   0  �4      ��  �   1  45      ��  �   2  �5      ��  �   3  $6      Ч  �   4  �6      �  �   5  7      ��  �   6  �7      �  �   7  8       �  �   8  �8      4�  �   9  9      H�  �   :  �9      \�  �   ;  �9      p�  �   <  h:      ��  �   =  �:      ��  �   >  `;      ��  �   ?  �;      ��  �   @  H<      Ԩ  �   A  �<      �  �   B   =      ��  �   C  t=      �  �   D  �=      $�  �   E  l>      8�  �   F  �>          �   G  T?      P�     �  d�  ԩ          4   �����?  	              �                      ��             	     �  g                  ԋ�           �  t�  ��  �   �  $@      �  �   �  �@       �  �   �  A      4�  �   �  �A      H�  �   �  B      \�  �   �  �B      p�  �   �  C      ��  �   �  �C      ��  �   �  �C      ��  �   �  pD      ��  �   �  �D      Ԫ  �   �  hE      �  �   �  �E      ��  �   �  XF      �  �   �  �F      $�  �   �  PG      8�  �   �  �G      L�  �   �  HH      `�  �   �  �H      t�  �   �  0I      ��  �   �  �I      ��  �   �  (J      ��  �   �  dJ      ī  �   �  �J      ث  �   �  \K      �  �   �  �K       �  �   �  TL      �  �   �  �L          �   �  DM      getRowObjUpdStatic  deleteRecordStatic  �       h�  x�          4   �����M      /     ��     ��                          3   �����M            Ԭ                      3   �����M  ��     &  ��  l�  ��      4   ���� N  
              |�                      ��             
     '  �                  ��           '  �  ��  �   +  `N      �  $   ,  ��  ���                       �N     
                    � ߱        ��  �   -  �N      T�  $   /  (�  ���                       �N  @         �N              � ߱        �  $   2  ��  ���                       (O       	       	           � ߱        8P     
                �P                     R  @        
 �Q              � ߱        ��  V   <  ��  ���                        R       	       	       DR       
       
       �R       	       	           � ߱        0�  $   X  <�  ���                       @S     
                �S                     U  @        
 �T              � ߱            V   j  ̯  ���                                      ��                      ��                  �  (                  �           �  \�  U     
                �U                     �V  @        
 �V          LW  @        
 W          �W  @        
 lW          X  @        
 �W              � ߱            V   �  ̰  ���                        adm-clone-props 4�  ��              �     W     4                          0  M                     start-super-proc    ��  �  �           �     X                                  n                      �     @  ��  ��          4   �����[      /   A  �     �                          3   �����[            �                      3   �����[  x�  $   [  L�  ���                       �[                         � ߱         �     k  ��   �  ��      4   ����\                t�                      ��                  l  p                  �           l  ��  \                     ,\                     @\                         � ߱            $   m  �  ���                              q  ��  ��          4   ����X\  x\                         � ߱            $   r  ȴ  ���                       �     y  8�  H�  ��      4   �����\      $   z  t�  ���                       �\                         � ߱            �   �  �\       ]     
                |]                     �^  @        
 �^              � ߱        D�  V   �  ��  ���                        X�  �   �  �^      P�     ]  p�  ��          4   ����_      /   ^  ��     ��                          3   ����(_            ܶ                      3   ����H_  h_     
                �_                     4a  @        
 �`              � ߱        �  V   j  �  ���                        �a     
                �a                     Lc  @        
 c              � ߱        �  V   �  |�  ���                        |�       $�  ��          4   ����`c                ��                      ��                                      ��             4�  �  /     и     �                          3   ����pc             �                      3   �����c      /     <�     L�                          3   �����c            l�                      3   �����c  ��  /  {  ��          d                      3   �����c  initProps   0�  ��              �     Y     $             d             :'  	                                   �          ��  ��      ��                �  �  ��               ��        O   ����    e�          O   ����    R�          O   ����    ��      D'                      Ⱥ          ��  p   �  l�  �      �  ��  |�     x�                ��                      ��                  �                    ���           �  �  ��  :                    $     л  ���                       ��                         � ߱        �  l�     ��                |�                      ��                    !                  l��             �  ��  :                   $     ��  ���                       ��                         � ߱        ܽ  \�     Ѕ                l�                      ��                  "  >                  ԙ�           "  ��  ��  :  :                 $   ;  ��  ���                       �                         � ߱        ̾  L�     ��                \�                      ��                  ?  [                  <��           ?  �  t�  :  W                 $   X  ��  ���                       �                         � ߱        L�  <�     (�                                        ��                  \  x                  Ȑ�           \  ܾ  ̿  ��     <�                                        ��                  y  �                  ���           y  \�  L�  <�     P�                                        ��                  �  �                  h��           �  ܿ  ��  ��     d�                                        ��                  �  �                  8��           �  \�  L�  <�     x�  	                                      ��             	     �  �                  P��           �  ��  ��  ��     ��  
                                      ��             
     �  	                   ��           �  \�  L�  <�     ��                                        ��                  
  &                  �           
  ��  ��  ��     ��                                        ��                  '  C                  ���           '  \�  L�  <�     Ȇ                                        ��                  D  `                  ���           D  ��  ��  ��     ܆                                        ��                  a  }                  ���           a  \�  L�  <�     ��                                        ��                  ~  �                  l��           ~  ��  ��  ��     �                                        ��                  �  �                  <��           �  \�  L�  <�     �                                        ��                  �  �                  ��           �  ��      ��     ,�                                        ��                  �  �                  ��           �  \�      O   �  ��  ��  @�               P�          8�  D�   , �                                                       �     ��                            ����                            Ĺ  <�  Ժ   �      ��     Z     X�                      � T�  V'                     ��     
  �  |�          4   ����L�                ��                      ��                                      d��             �  ��  /     ��     ��                          3   ����\�            ��                      3   ����|�  d�  /     $�     4�                          3   ������            T�                      3   ������  ��  /     ��     ��                          3   ����Ї            ��                      3   ������      /     ��     �                          3   �����            ,�                      3   ����0�  �     
                ��                     Њ  @        
 ��              � ߱        ��  V   {  <�  ���                        ��  $   �  ��  ���                       �                         � ߱         �     
                |�                     ̌  @        
 ��              � ߱        ��  V   �  $�  ���                        p�  $   �  ��  ���                       ،     
                    � ߱        �     
                h�                     ��  @        
 x�              � ߱        ��  V   �  �  ���                        X�  $   �  ��  ���                       Ď     
                    � ߱        ؎     
                T�                     ��  @        
 d�              � ߱        ��  V   �  ��  ���                        @�  $     ��  ���                       ��                         � ߱        �     
                `�                     ��  @        
 p�              � ߱        l�  V     ��  ���                        ��  �   &  Ȓ      <�  $   '  ��  ���                       �     
                    � ߱        ��     
                x�                     Ȕ  @        
 ��              � ߱        h�  V   1  ��  ���                        ��  $   K  ��  ���                       Ԕ     
                    � ߱        ��  �   e  �      ,�  $   o   �  ���                       (�     
                    � ߱        @�  �   �  <�      ��  $   �  l�  ���                       |�                         � ߱               �  ��  ��          4   ������      /   �  ��     ��                          3   ������  ,�     
   �                      3   ����ؕ  \�        L�                      3   ������  ��        |�                      3   ������            ��                      3   �����  pushRowObjUpdTable  ��  ��  �                   [      �                               �(                     pushTableAndValidate    ��  ,�  �           p     \     �                          �  )                     remoteCommit    D�  ��  �           d      ]     �                          �  a)                     serverCommit    ��  �  �           `    ! ^     �                          �  n)                                      �          ��  ��      ��                  �  �  �              ��        O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  @�    ��                            ����                            �  (�      x�              _      8�                      
�     {)                     disable_UI  x�  ��                      `      �                               �)  
                                   ��          ��  ��      ��                  Y  a  ��              <�        O   ����    e�          O   ����    R�          O   ����    ��          O   _  ��  ��  p�    ��                            ����                            ��        <�              a      ��                      
      �)                      �  �    ����  �       ��           ��  8   ����   ��  8   ����   �  8   ����   �  8   ����   $�  8   ����   4�  8   ����   D�  8   ����   T�  8   ����   d�  8   ����   t�  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  H�  T�      returnFocus ,INPUT hTarget HANDLE   8�  |�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    l�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ,�  <�      removeAllLinks  ,   �  P�  `�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE @�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  D�  P�      hideObject  ,   4�  d�  p�      exitObject  ,   T�  ��  ��      editInstanceProperties  ,   t�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  4�  @�      applyEntry  ,INPUT pcField CHARACTER    $�  l�  |�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER \�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  8�  @�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  �      disconnectObject    ,   ��  0�  @�      destroyObject   ,    �  T�  `�      bindServer  ,   D�  t�  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  d�  ��  ��      startFilter ,   ��  ��  ��      releaseDBRow    ,   ��  �  �      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ��  H�  `�      filterContainerHandler  ,INPUT phFilterContainer HANDLE 8�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ��  �      compareDBRow    ,   ��  �  0�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   �  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  ��  ��      updateState ,INPUT pcState CHARACTER    ��  �  (�      updateQueryPosition ,   �  <�  P�      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ,�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  H�  `�      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   8�  ��  ��      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��   �  4�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  �  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ��  �      startServerObject   ,   ��  $�  4�      setPropertyList ,INPUT pcProperties CHARACTER   �  d�  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    T�  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  |�  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER l�  ��  ��      rowObjectState  ,INPUT pcState CHARACTER    ��  ��  �      retrieveFilter  ,   ��   �  4�      restartServerObject ,   �  H�  X�      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   8�  P�  \�      refreshRow  ,   @�  p�  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  `�  ��  ��      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  ��  $�  <�      initializeServerObject  ,   �  P�  d�      initializeObject    ,   @�  x�  ��      home    ,   h�  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  ��  ��      fetchPrev   ,   ��  ��  ��      fetchNext   ,   ��  �  �      fetchLast   ,    �  0�  <�      fetchFirst  ,    �  P�  \�      fetchBatch  ,INPUT plForwards LOGICAL   @�  ��  ��      endClientDataRequest    ,   x�  ��  ��      destroyServerObject ,   ��  ��  ��      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  8�  H�      dataAvailable   ,INPUT pcRelative CHARACTER (�  t�  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE d�  ��  ��      commitTransaction   ,   ��  ��   �      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    ��  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 p%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %              %              %              %              %              %              %              %              %              %              %              %              %              %              %              %              %       	       %              %              %              %              %              %              %              %              %               %              %              %              %              %              %              %               �     }        ��    G   %               � 
"    
 v%              h �P  \         (          
�                          
�            � X   �
"    
 o
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 p�           �    1� h  
 p� s   v%               o%   o           � x    q
"   
 q�           ,    1� y   q� s   v%               o%   o           � �   p
"   
 o�           �    1� �  
 o� s   v%               o%   o           � �   q
"   
 p�               1� �   p� s   v%               o%   o           � x    o
"   
 ~�           �    1� �   ~� s   v%               o%   o           � �   p
"   
 q�           �    1� �   q� �   v%               o%   o           %               
"   
 v�          x    1� �   v� �     
"   
 q�           �    1�    q� s   v%               o%   o           �   p
"   
 o�           (    1�    o� s   v%               o%   o           � *  S q
"   
 p�           �    1� ~   p� �   v%               o%   o           %               
"   
 o�           	    1� �   o� �   v%               o%   o           %               
"   
 q�           �	    1� �   q� �   v%               o%   o           %              
"   
 v�          
    1� �   v� �     
"   
 q�           L
    1� �  
 q� �   v%               o%   o           %               
"   
 o�           �
    1� �   o� s   v%               o%   o           � x    q
"   
 v�          <    1� �   v� �     
"   
 o�           x    1� �   o� s   v%               o%   o           � �  t p
"   
 v�          �    1� j  
 v� �     
"   
 p�           (    1� u   p� s   v%               o%   o           � �  � q
"   
 o�           �    1�    o� s   v%               o%   o           � x    p
"   
 o�               1� *  
 o� 5   v%               o%   o           %               
"   
 ~�           �    1� 9   ~� �   v%               o%   o           %              
"   
 o�               1� A   o� s   v%               o%   o           � x    ~
"   
 o�           |    1� R   o� s   v%               o%   o           o%   o           
"   
 p�           �    1� b  
 p� s   v%               o%   o           � x    q
"   
 p�           l    1� m   p� ~  	 v%               o%   o           � �  / p
"   
 v�          �    1� �   v� ~  	   
"   
 o�               1� �   o� ~  	 vo%   o           o%   o           � x    o
"   
 v�          �    1� �   v� ~  	   
"   
 o�           �    1� �   o� ~  	 vo%   o           o%   o           � x    o
"   
 v�          @    1� �   v� �     
"   
 v�          |    1� 
   v� ~  	   
"   
 v�          �    1�    v� ~  	   
"   
 v�          �    1� $   v� ~  	   
"   
 p�           0    1� 2   p� �   vo%   o           o%   o           %              
"   
 v�          �    1� C   v� ~  	   
"   
 v�          �    1� Q  
 v� \     
"   
 v�          $    1� d   v� ~  	   
"   
 v�          `    1� s   v� ~  	   
"   
 v�          �    1� �   v� ~  	   
"   
 v�          �    1� �   v� ~  	   
"   
 v�              1� �  	 v� ~  	   
"   
 v�          P    1� �   v� ~  	   
"   
 v�          �    1� �   v� ~  	   
"   
 p�           �    1� �   p� s   v%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 j
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    �      
"   
 �� @  , 
�       �    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 o�           p    1�   
 o� s   v%               o%   o           � x    o
"   
 o�           �    1� "  
 o� s   v%               o%   o           o%   o           
"   
 p�           `    1� -   p� �   v%               o%   o           o%   o           
"   
 q�           �    1� 6   q� �   v%               o%   o           %               
"   
 o�           X    1� E   o� �   v%               o%   o           %               
"   
 p�           �    1� R   p� s   v%               o%   o           � x    o
"   
 o�           H    1� Y   o� �   v%               o%   o           %              
"   
 o�           �    1� k   o� �   v%               o%   o           o%   o           
"   
 p�           @    1� w   p� s   v%               o%   o           o%   o           
"   
 q�           �    1� �  	 q� s   v%               o%   o           � x    ~
"   
 q�           0    1� �   q� s   v%               o%   o           o%   o           
"   
 o�           �    1� �   o� s   v%               o%   o           o%   o           
"   
 q�           (    1� �   q� �   v%               o%   o           %               
"   
 q�           �    1� �   q� �   v%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 o�           t    1� �  
 o� �   v%               o%   o           %              
"   
 o�           �    1� �   o� s   v%               o%   o           o%   o           
"   
 o�           l    1� �   o� s   v%               o%   o           � x    p
"   
 o�           �    1� �   o� s   v%               o%   o           o%   o           
"   
 v�          \     1� �   v� �     
"   
 ~�           �     1�    ~� s   v%               o%   o           �   ! q
"   
 o�           !    1� A   o� s   v%               o%   o           � x    ~
"   
 q�           �!    1� N   q� s   v%               o%   o           � a   o
"   
 v�          �!    1� p   v� }     
"   
 v�          0"    1� �   v� �     
"   
 q�           l"    1� �   q� s   v%               o%   o           � x    o
"   
 v�          �"    1� �  
 v� �     
"   
 ~�           #    1� �   ~� �   v%               o%   o           o%   o           
"   
 p�           �#    1� �   p� �   v%               o%   o           %               
"   
 q�           $    1� �   q� �   v%               o%   o           %               
"   
 o�           �$    1� �   o� s   v%               o%   o           � x    q
"   
 o�           %    1� �   o� s   v%               o%   o           o%   o           
"   
 ~�           �%    1� �   ~� �   v%               o%   o           %              
"   
 p�           �%    1�    p� �   v%               o%   o           %               
"   
 p�           x&    1�    p� �   v%               o%   o           %               
"   
 v�          �&    1� $   v� �     
"   
 v�          0'    1� 1   v� s     
"   
 o�           l'    1� >   o� 5   v%               o%   o           o%   o           
"   
 ~�           �'    1� J   ~� s   v%               o%   o           � x    o
"   
 ~�           \(    1� X   ~� s   v%               o%   o           o%   o           
"   
 o�           �(    1� f   o� �   vo%   o           o%   o           o%   o           
"   
 o�           T)    1� {   o� ~  	 v%               o%   o           o%   o           
"   
 q�           �)    1� �   q� s   v%               o%   o           o%   o           
"   
 q�           L*    1� �  
 q� 5   v%               o%   o           o%   o           
"   
 v�          �*    1� �   v� s     
"   
 p�           +    1� �   p� s   v%               o%   o           � �  4 p
"   
 q�           x+    1�   
 q� �   v%               o%   o           %              
"   
 v�          �+    1�    v� �     
"   
 ~�           0,    1�    ~� s   v%               o%   o           � x    o
"   
 o�           �,    1� +   o� �   v%               o%   o           %              
"   
 p�            -    1� :   p� s   v%               o%   o           � x    o
"   
 o�           �-    1� G   o� s   v%               o%   o           � x    p
"   
 q�           .    1� U   q� s   v%               o%   o           � x    o
"   
 o�           |.    1� a   o� �   v%               o%   o           %               
"   
 o�           �.    1� p  	 o� �   v%               o%   o           o%   o           
"   
 o�           t/    1� z   o� s   v%               o%   o           � �  	 ~
"   
 p�           �/    1� �   p� 5   v%               o%   o           %       �       
"   
 o�           d0    1� �   o� s   v%               o%   o           � x    p
"   
 o�           �0    1� �   o� �   vo%   o           o%   o           %              
"   
 o�           T1    1� �   o� �   v%               o%   o           %               
"   
 o�           �1    1� �   o� s   v%               o%   o           o%   o           
"   
 o�           L2    1� �   o� ~  	 v%               o%   o           � x    ~
"   
 v�          �2    1� �   v� ~  	   P �L 
�H T   %              �     }        �GG %              
"   
 o�           P3    1� �  
 o� s   v%               o%   o           � x    o
"   
 p�           �3    1� 	   p� �   v%               o%   o           %               
"   
 o�           @4    1�   	 o� s   v%               o%   o           � x    p
"   
 q�           �4    1�     q� s   v%               o%   o           � x    o
"   
 ~�           (5    1� .   ~� �   v%               o%   o           %               
"   
 q�           �5    1� >   q� s   v%               o%   o           � x    ~
"   
 q�           6    1� Q   q� s   v%               o%   o           o%   o           
"   
 o�           �6    1� Y   o� s   v%               o%   o           o%   o           
"   
 p�           7    1� f   p� �   v%               o%   o           o%   o           
"   
 o�           �7    1� t   o� �   v%               o%   o           o%   o           
"   
 q�           8    1� �   q� �   v%               o%   o           o%   o           
"   
 p�           �8    1� �   p� s   v%               o%   o           o%   o           
"   
 o�            9    1� �  	 o� ~  	 v%               o%   o           � x    q
"   
 ~�           t9    1� �  
 ~� ~  	 v%               o%   o           � x    o
"   
 q�           �9    1� �   q� s   v%               o%   o           � x    ~
"   
 q�           \:    1� �   q� s   v%               o%   o           o%   o           
"   
 o�           �:    1� �   o� s   v%               o%   o           o%   o           
"   
 q�           T;    1� �   q� s   v%               o%   o           � x    p
"   
 o�           �;    1� �   o� s   v%               o%   o           � x    q
"   
 o�           <<    1�    o� ~  	 v%               o%   o           o%   o           
"   
 v�          �<    1�    v� �     
"   
 o�           �<    1� %   o� s   v%               o%   o           � x    o
"   
 o�           h=    1� 3   o� s   v%               o%   o           o%   o           
"   
 p�           �=    1� F   p� �   v%               o%   o           o%   o           
"   
 ~�           `>    1� X  
 ~� s   v%               o%   o           � x    q
"   
 p�           �>    1� c   p� s   v%               o%   o           � x    ~
"   
 q�           H?    1� {   q� �   v%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 q�           @    1� �  	 q� �   v%               o%   o           o%   o           
"   
 q�           �@    1� �   q� �   v%               o%   o           o%   o           
"   
 p�           A    1� �   p� �   v%               o%   o           o%   o           
"   
 ~�           �A    1� �   ~� �   v%               o%   o           %              
"   
 p�           B    1� �   p� s   v%               o%   o           � �  M ~
"   
 o�           |B    1� /   o� �   v%               o%   o           %              
"   
 o�           �B    1� @   o� �   v%               o%   o           %               
"   
 p�           tC    1� T   p� �   v%               o%   o           %               
"   
 q�           �C    1� k   q� ~  	 v%               o%   o           � y   p
"   
 ~�           dD    1� �   ~� �   v%               o%   o           %               
"   
 ~�           �D    1� �   ~� ~  	 v%               o%   o           o%   o           
"   
 p�           \E    1� �   p� �   vo%   o           o%   o           %              
"   
 o�           �E    1� �   o� ~  	 vo%   o           o%   o           � x    o
"   
 o�           LF    1� �   o� �   vo%   o           o%   o           o%   o           
"   
 o�           �F    1� �   o� �   vo%   o           o%   o           o%   o           
"   
 o�           DG    1� �   o� ~  	 vo%   o           o%   o           o%   o           
"   
 o�           �G    1� �   o� �   vo%   o           o%   o           o%   o           
"   
 o�           <H    1�    o� ~  	 vo%   o           o%   o           �    o
"   
 p�           �H    1�    p� ~  	 vo%   o           o%   o           � &   p
"   
 q�           $I    1� 2   q� �   v%               o%   o           %               
"   
 q�           �I    1� F   q� �   v%               o%   o           %               
"   
 v�          J    1� Z   v� ~  	   
"   
 ~�           XJ    1� n   ~� �   v%               o%   o           %               
"   
 ~�           �J    1� z   ~� s   v%               o%   o           o%   o           
"   
 p�           PK    1� �   p� s   v%               o%   o           o%   o           
"   
 o�           �K    1� �   o� �   v%               o%   o           o%   o           
"   
 q�           HL    1� �   q� s   v%               o%   o           � x    p
"   
 o�           �L    1� �   o� �   v%               o%   o           %               
"   
 o�           8M    1� �  	 o� �   v%               o%   o           %                "    v%     start-super-proc v%     adm2/smart.p 0�P �L 
�H T   %              �     }        �GG %              
"   
   �       TN    6� �     
"   
   
�        �N    8
"   
   �        �N    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      �P    � $         � �          
�    �    �
"   
 �p� @  , 
�       �Q    ��    �p�               �L"  	  , �   �    o�    v�     }        �A      |    "  	    �    o%              (<   \ (    |    �     }        �A�    �A"  
  o    "  	  �"  
  o  < "  	  �"  
  o(    |    �     }        �A�    �A"  
  o
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8      �S    � $         � �          
�    �    �
"   
 �p� @  , 
�       �T    �� h  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        dU    �� �   � P   �        pU    �@    
� @  , 
�       |U    �� �     p�               �L
�    %              � 8      �U    � $         � �          
�    �      
"   
 �p� @  , 
�       �V    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�        W    �� �     p�               �L%               
"   
  p� @  , 
�       `W    �� �    p�               �L%               
"   
  p� @  , 
�       �W    �� �    p�               �L(        � x      � x      � x      �     }        �A
�H T   %              �     }        �GG %              
"   
 p (   � 
"   
 �    �        �X    �� �   �
"   
   � 8      �X    � $         � �          
�    �    �
"   
   �        DY    �
"   
   �       dY    /
"   
   
"   
   �       �Y    6� �     
"   
   
�        �Y    8
"   
   �        �Y    �
"   
   �       �Y    �
"   
   p�    � <   o
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �Z    �A"    �A
"   
   
�        [    �@ � 
"   
 p"      �       }        �
"   
 v%              %                "    v%     start-super-proc v%     adm2/appserver.p xo�    � �     
�    �     }        �%               %      Server  - �     }        �    "    o� x    v%                   "    o� x    v%      NONE    p�,  8         $     "    p        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        L]    �� �   � P   �        X]    �@    
� @  , 
�       d]    �� �   �p�               �L
�    %              � 8      p]    � $         � �          
�    �    �
"   
 �p� @  , 
�       �^    �� �   �p�               �L"    , p�,  8         $     "    p        � �   �
�     "    v%     start-super-proc v%     adm2/dataquery.p #q
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
 �(�  L ( l       �        �_    �� �   � P   �        �_    �@    
� @  , 
�       �_    �� �   �p�               �L
�    %              � 8      �_    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       �`    �� u   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
 �(�  L ( l       �        �a    �� �   � P   �        �a    �@    
� @  , 
�       �a    �� �   �p�               �L
�    %              � 8      �a    � $         � �   �     
�    �    �
"   
 �p� @  , 
�        c    �� �   �p�               �L%               "    v%     start-super-proc v%     adm2/query.p 0�%     start-super-proc v%     adm2/queryext.p % 	    initProps �
�    %� �   FOR EACH tCreditos NO-LOCK,       EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK,       EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK,       EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION �   � &!     � (!     � *!  %   
�     	         �G
"   
 q�        Te    �G
"   
   
"   
    x    (0 4      �        te    �G%                   �        �e    �GG %              � 2"    �� 3"         %              %                   "      %              
"   
       "      �        pf    �
"   
   
�       �f    �"       \      H   "    �((       "      %              � x      � &!   �     
"   
   
"   
 v \      H   "      ((       "      %              � x     � &!   q�        Hg    �%                   %              %                   "  (    %                  "  (      
"   
 �
"   
 q0 T       m � "  (  q�        Ph    �A @   "      $         � "  (  q�    v�        \h    �� "  (    
"   
 v \ H     H   "      ((       "    �%              � x    v� &!     (        "  !  �� x    q�        i    �"  !  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
   
"   
   (�  L ( l       �        j    �� �   � P   �        j    �@    
� @  , 
�        j    �� �     p�               �L
�    %              � 8      ,j    � $         � �          
�    �      
"   
 �p� @  , 
�       <k    �� �   �p�               �L%               
"   
   p� @  , 
�       �k    �� �     p�               �L"    , �,  8         $     "    �L        � :"  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 �
"   
 �(�  L ( l       �        �l    �� �   � P   �        �l    �@    
� @  , 
�       �l    �� �   �p�               �L
�    %              � 8      �l    � $         � �   �     
�    �      
"   
 �p� @  , 
�       �m    ��    �p�               �L
"   
 , 
"   
   p� @  , 
�       n    �� �     p�               �L"    , 
"   
  p� @  , 
�       dn    �� �    p�               �L"    ,     "    q� x    v%   OPEN QUERY Query-Main FOR EACH tCreditos NO-LOCK,       EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK,       EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK,       EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION.     "    ed� \#   CK((        "    WH%                   "    ia� b#     "    bi (   "           "    ia%              @ �,  8         $     "    �        � n#    
�    p�,  8         $     � {#   o        � }#   �
�    %,#  rowObject.NombreAgencia = Agencias.Nombre  rowObject.NombreCiudad = Ubicacion.Nombre  rowObject.TpoCliente = tCreditos.Cod_Credito  rowObject.OtroTpoCliente = tCreditos.Observaciones  rowObject.CptcionClccion = tCreditos.Categoria  rowObject.PrdctoSlctar = tCreditos.Per_Pago  rowObject.OtroPrdctoSlctar = tCreditos.Pagare  rowObject.Grntia = tCreditos.Cue_Desembolso  rowObject.Linea = tCreditos.Cue_DebAutomatico  rowObject.reestrctrcion = tCreditos.Abogado  rowObject.FrmaPgo = tCreditos.CategoriaMes  rowObject.dstncion = tCreditos.Nit_Juzgado �    "      � (!         %              %                   "      %                  "      "      T(        "    p%              "    p� (!   v"      �       "    ��    "    p�    v� x      �    ��    "     �     S    "      "    v    "    o%                � @    �     t T     P   4       v"      (0       4       �"      � x      � x    �� &!   �T ,  %              T   "    �"    v� (!     �    �� &!   �T    �    "    ��    v"      �    �"      %                   %              %                   "      %                  "      �     "       \      H   "      ((       "    �%              � x    v� �%     4  q     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �w    �� �   � P   �        �w    �@    
� @  , 
�       �w    �� �   �p�               �L
�    %              � 8      �w    � $         � �          
�    �    �
"   
 �p� @  , 
�       �x    �� �  
 �p�               �L"    ,       "  
  ��    � �%  � �� (!   v      "  	    �    � �%  � v� (!   ��   � &!     � (!     � �%  � ��   � &!     � (!   �� �%  � �      "  
  ��    � x    �� (!   v      "  	    �    � <&   v� (!   �   ,        "    �� �%   ��   � &!   �� (!   �� x    v   ,        "      � �%     �   � &!   �� (!   v� <&   �      "  
  ��    � x    p� (!   v      "  	    �    � Q&   v� (!   p   ,        "    �� �%   p�   � &!   �� (!   p� x    v   ,        "      � �%     �   � &!   p� (!   v� Q&   �      "  
  p�    � x    o� (!   v      "  	    �    � ^&   v� (!   o   ,        "    �� �%   o�   � &!   �� (!   o� x    v   ,        "      � �%     �   � &!   o� (!   v� ^&   p�   � &!     � (!     � o&  �   
�H T   %              �     }        �GG %              
"   
 v
"   
 �
"   
 v
"   
 v(�  L ( l       �        �}    �� �   � P   �        �}    �@    
� @  , 
�       �}    �� �   vp�               �L
�    %              � 8      �}    � $         � �          
�    �      
"   
 �p� @  , 
�       �~    �� �   �p�               �L"    , 
"   
   p� @  , 
�       (    �� c     p�               �L"    , 
"   
  p� @  , 
�       �    �� >    p�               �L"    ,     %              %                   "      %                  "      �     "      4 (        "  
    �    � �%  �   � (!         "  	  q�     "    �T    "      "      @ A,    �   � &!   v� �%     "    �"       T      @   "    v(        "      � x    �� x      � &!   �"    o     "  	   %              D H   @ A,    �   � &!   �� �%     "    �"    �,    S   "    �� �%  � �� (!   v%                T      @   "    v(        "      � x    �� x      � &!   �"    p     "  
   %                         "    v� �%     "    �           "      � �%   �"      
�H T   %              �     }        �GG %              
"   
 q
"   
   
"   
 q
"   
 �(�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   qp�               �L
�    %              � 8      ��    � $         � �   �     
�    �    v
"   
 �p� @  , 
�       ��    �� c   �p�               �L"    , 
"   
   p� @  , 
�       �    �� >     p�               �L"    , "      %               �     }        �%               �     }        �%               �     }        �%               �     }        �%              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    v%     start-super-proc v%     adm2/data.p %     start-super-proc v%     adm2/dataext.p %     start-super-proc v%     adm2/dataextcols.p %     start-super-proc v%     adm2/dataextapi.p o%              %              %              %              %              %              %              %              %       	       
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
 �(�  L ( l       �        P�    �� �   � P   �        \�    �@    
� @  , 
�       h�    �� �   �p�               �L
�    %              � 8      t�    � $         � �   �     
�    �    �
"   
 �p� @  , 
�       ��    �� n   �p�               �L%               %     "dsolprofina.i" 
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        L�    �� �   � P   �        X�    �@    
� @  , 
�       d�    �� �   �p�               �L
�    %              � 8      p�    � $         � �          
�    �    �
"   
 �p� @  , 
�       ��    �� k   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        8�    �� �   � P   �        D�    �@    
� @  , 
�       P�    �� �   �p�               �L
�    %              � 8      \�    � $         � �          
�    �    �
"   
 �p� @  , 
�       l�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        $�    �� �   � P   �        0�    �@    
� @  , 
�       <�    �� �   �p�               �L
�    %              � 8      H�    � $         � �          
�    �    �
"   
 �p� @  , 
�       X�    �� p  	 �p�               �L
"   
 , 
"   
 v     � �(  	   �        ��    �
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        0�    �� �   � P   �        <�    �@    
� @  , 
�       H�    �� �   �p�               �L
�    %              � 8      T�    � $         � �          
�    �    �
"   
 �p� @  , 
�       d�    �� �   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        H�    �� �   � P   �        T�    �@    
� @  , 
�       `�    �� �   �p�               �L
�    %              � 8      l�    � $         � �          
�    �    �
"   
 �p� @  , 
�       |�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 �        � �(   �
�    
�             �Gp�,  8         $     
"   
 �        � �(   �
�    �    � �(     
�        "    �� x    v%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "       %     bufferCommit    
�    "       "       �    � J)     
�    %               %     bufferCommit    
�    " !     " !     
�     
        �G�     }        �
�    
�     
        �G                �           x   `       ��                 �  �  �               <�        O   ����    e�          O   ����    R�          O   ����    ��         $   �  �   ���                       TX     
                    � ߱               �    �          4   �����X                �                      ��                  �  �                  �           �  (  �  �  �  �X             �  �  4          4   ����PY                D                      ��                  �  �                  ��           �  �  x  o   �      ,                                 �  �   �  pY      �  �   �  �Y      �  $   �  �  ���                       �Y     
                    � ߱          �   �  �Y         �   �  Z      4  �   �  (Z          $   �  `  ���                       XZ  @         DZ              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 �  !  �               <�        O   ����    e�          O   ����    R�          O   ����    ��      ]                      �          �  $   �  �   ���                       �Z     
                    � ߱                  �  �                      ��                   �  �                  �"�          �  (      4   �����Z      $   �  �  ���                       [     
                    � ߱        d     �    (          4   ����,[      /  �  T                               3   ����@[  x  �     L[          O     ��  ��  �[               �          �  �   , �                          
                               �      ��                            ����                                            �           x   `       ��            	     �  �  �               ��        O   ����    e�          O   ����    R�          O   ����    ��         $   �  �   ���                       d                         � ߱        �  $   �  ,  ���                       e                         � ߱        @e     
                �e  @         `e              � ߱        ,  $   �  X  ���                         <      �  `                      ��        0          �  �                  �J�         �  �      $   �  h  ���                       f                         � ߱        �  $   �  �  ���                       Hf                         � ߱            4   ����|f  �f     
                �f                     Tg                         � ߱        �  $   �  �  ���                                     �                      ��                  �  �                  lN�    �     �  �  �  $   �  (  ���                       �g       !       !           � ߱          �        �                      ��        0          �  �                  lR�           �  T      $   �  �  ���                       �g       (       (           � ߱        t  $   �  H  ���                       h       (       (           � ߱            4   ����8h         �  �            4   ����hh                                      ��                  �  �                  tV�           �  �  t  $   �  H  ���                       �h       !       !           � ߱            O   �  �� ��          $   �  �  ���                       i                         � ߱        �i     
                8j                     �k  @        
 Hk          �k  @        
 �k          �k                     4l     
                �l                      n  @        
 �m          Xn  @        
 n          �n  @        
 pn              � ߱        8  V   �  �  ���                        	     �  P  �          4   �����n  �n                      p                      p                     |p                         � ߱            $   �  `  ���                       H	     �  $	  4	          4   �����p      �   �  �p      �	  $   �  t	  ���                       <q                         � ߱        X
  $   �  �	  ���                       ls                         � ߱          h
      �
  �                      ��        0          �  �                  |Z�    p     �  �	      $   �  �
  ���                       �s                         � ߱          $   �  �
  ���                       �s                         � ߱            4   �����s  t                     Lt                     Xt                     �t                     �t                         � ߱        �  $   �  (  ���                              �              4   �����t      $   �  D  ���                       u          <v             � ߱        (  $   �  �  ���                       Hv                         � ߱          8      �  �                      ��        0          �  �                   ��    �     �  �      $   �  d  ���                       \v                         � ߱        �  $   �  �  ���                       �v                         � ߱            4   �����v      $   �  $  ���                       �v                         � ߱        \w     
                �w                     (y  @        
 �x              � ߱        `  V   	  P  ���                        4y       
       
       hy       	       	       �y                     �y                         � ߱          $   P  �  ���                       �y       
       
       (z       	       	       \z                     �z                         � ߱        �  $   w  �  ���                       {       
       
       8{       	       	       l{                     �{                         � ߱        d  $   �  8  ���                       |       
       
       H|       	       	       ||                     �|                         � ߱        �  $   �  �  ���                       �  $   �  �  ���                       $}                         � ߱        P}     
                �}                       @        
 �~          t  @        
 4          �  @        
 �              � ߱          V      �  ���                                 x  �                      ��        0     	     d  y                  |��    �     d  �      $   d  L  ���                       �                         � ߱        �  $   d  �  ���                       �                         � ߱        �  4   ����0�      4   ����X�  H  $   i    ���                       ��                         � ߱        T     k  `  �          4   ����܀                (                      ��                  l  p                  ��           l  p   �                     ��       	       	           � ߱            $   m  �  ���                              r  l  �          4   ������  	              4                      ��             	     t  x                  ć�           t  |  D�                     ��       
       
           � ߱            $   u  �  ���                       Ԃ                     �                         � ߱        T  $     `  ���                       <�     
                ��                     �  @        
 Ȅ          `�  @        
  �              � ߱            V   �  �  ���                                    J �          D    � X�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            /'                          %�                                x   `       ��                  I  T  �               ���        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           x   `       ��                  ^  m  �               ���        O   ����    e�          O   ����    R�          O   ����    ��      )       �              �                  $                  X  /  j       (  H�                      3   ����,�            H                      3   ����P�      O   k  ��  ��  \�               �          �  �    �                                             ��                            ����                                            <          x   `       ��                  w  �  �               � �        O   ����    e�          O   ����    R�          O   ����    ��      +)        �              �          �       $                  5)                      �          @)                                �  /  �  h     x  ��                      3   ����`�            �                      3   ������  �  /  �  �     �  ��                      3   ������  l                            3   ������      $   �  @  ���                                                    � ߱                  �  �                  3   ����Ȗ      $   �  �  ���                                                    � ߱        L  $   �     ���                       Ԗ                          � ߱            O   �  ��  ��  �                �          �  �   @ �                                                              0              0            ��                            ����                                                      x   `       ��                  �  �  �               ��        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  5)   !    �              �          @)   !                   �              /  �  @     P   �                      3   �����  �        p  �                  3   ����(�      $   �  �  ���                                !                   � ߱                  �                    3   ����4�      $   �  4  ���                                !                   � ߱                   !  �          �  �   , �                                                            !     ��                            ����                                            �           x   `       ��                  C  N  �               ��        O   ����    e�          O   ����    R�          O   ����    ��             M  �   �           4   ����T�      �   M  h�    ��                            ����                            TXS appSrvUtils tCreditos Creditos Agencia Nit Cod_Credito Num_Credito Tip_Credito Pagare Tasa Fec_Aprobacion Fec_Desembolso Fec_Pago Fec_UltPago Fec_Calificacion Fec_Reestructurado Fec_PagAnti Fec_DifCobro Fec_CanceTotal Fec_UltLiquidacion Fec_ProxLiquidacion For_Pago For_Interes Per_Pago Plazo Usuario Cuota Monto Sdo_Capital Int_Corrientes Int_Anticipado Int_MorCobrar Int_DifCobro Sdo_CapPag Sdo_IntPag Sdo_IntMor Sdo_Proyectado Cuo_Pagadas Cuo_Atraso Val_Atraso Dias_Atraso Provision Cod_Califica Poliza Deducible Observaciones Incremento Destino Sistema Estado Detalle_Estado Num_Solicitud Per_Gracia Id_Adicionales Reestructurado Int_AntDesembolso Age_Desembolso Cod_Desembolso Cue_Desembolso Age_DebAutomatico Cue_DebAutomatico Cod_DebAutomatico Desembolso Costas Nit_Juzgado Nom_Juzgado Abogado Honorarios Polizas Categoria Cta_Contable Sdo_Anuales Capital_Acum Int_LiqAcum Int_MoraDifCob Cod_CalificaMes Provision_Interes Provision_Otros CategoriaMes Val_Desembolso Fec_Bloqueo Usuario_gestor Tasa_Desembolso Seg_Cartera Com_Bruta Com_Adicional GmfxC TarjetaDB Fec_BloqueoTdb Fec_CreaTdb Fec_CancTdb diapago cod_calificareest cod_calant cod_calact respaldoaportes Comision seg_vida Num_Control Tasa_Ant Agencias Agencias Ubicacion Configura Ubicacion Clientes Clientes D:\SPS\soportes\fodun\Prog\dsolprofina.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "dsolprofina.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH tCreditos NO-LOCK,       EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK,       EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK,       EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION ,   tCreditos Agencias Ubicacion Clientes hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH tCreditos NO-LOCK,       EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK,       EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK,       EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage rowObject.NombreAgencia = Agencias.Nombre  rowObject.NombreCiudad = Ubicacion.Nombre  rowObject.TpoCliente = tCreditos.Cod_Credito  rowObject.OtroTpoCliente = tCreditos.Observaciones  rowObject.CptcionClccion = tCreditos.Categoria  rowObject.PrdctoSlctar = tCreditos.Per_Pago  rowObject.OtroPrdctoSlctar = tCreditos.Pagare  rowObject.Grntia = tCreditos.Cue_Desembolso  rowObject.Linea = tCreditos.Cue_DebAutomatico  rowObject.reestrctrcion = tCreditos.Abogado  rowObject.FrmaPgo = tCreditos.CategoriaMes  rowObject.dstncion = tCreditos.Nit_Juzgado ; Agencia TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo Grntia Linea reestrctrcion FrmaPgo dstncion Cuota NombreAgencia Ciudad NombreCiudad Fec_UltActualiza Agencia NombreAgencia Ciudad NombreCiudad Fec_UltActualiza TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo Grntia Linea reestrctrcion FrmaPgo dstncion Cuota Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p NombreAgencia Ciudad TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Grntia Linea reestrctrcion FrmaPgo dstncion RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI FHNDLE Icr_nits IdAgeEst Idxcre idxcre1 IdxNitCodNum idxrec Idx_Creditos ITipAge skNumCrdto XIE3Creditos XIE4Creditos qDataQuery t  T=  �  �K      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   �	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �      !           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    6   Y   �                            initProps   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  P  w  �  �  �     d  i  k  l  m  p  r  t  u  x  y    �  �                  lRet                       piTableIndex    �  t  0   Z   �        `                  deleteRecordStatic  �  �               !  "  :  ;  >  ?  W  X  [  \  x  y  �  �  �  �  �  �  �  �  	  
  &  '  C  D  `  a  }  ~  �  �  �  �  �  �  �  �  �  �                 !       0  �     [       4      |                  pushRowObjUpdTable  T  �        �        pcValType                  $       L       \       �                         pushTableAndValidate    j  k  m  H         <        pcContext   `             $       �         x        pcMessages             �        pcUndoIds   �  �     ]       $      �                  remoteCommit    �  �  �  �  �               $       8  !      ,        pcMessages      !      P        pcUndoIds   �  �     ^       �      �                  serverCommit    �  �  \  �     _               �                  getRowObjUpdStatic  �  �  �  ,     `                                  disable_UI  M  N  �  l     a               d                  fHndle  _  a  4  �.       �(      .                      X$  �  �  a   tCreditos   L         T         X         d         p         |         �         �         �         �         �         �         �         �         �                             ,          @          L          X          d          l          t          |          �          �          �          �          �          �          �          �          �          !         !         !         (!         4!         @!         P!         X!         d!         t!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         "         "         ("         <"         P"         d"         p"         x"         �"         �"         �"         �"         �"         �"         �"        �"         �"         �"          #         #         $#         4#         D#         T#         `#         p#         �#         �#         �#         �#         �#         �#         �#         �#         �#         �#          $         $         $         ($         4$         @$         L$         Agencia Nit Cod_Credito Num_Credito Tip_Credito Pagare  Tasa    Fec_Aprobacion  Fec_Desembolso  Fec_Pago    Fec_UltPago Fec_Calificacion    Fec_Reestructurado  Fec_PagAnti Fec_DifCobro    Fec_CanceTotal  Fec_UltLiquidacion  Fec_ProxLiquidacion For_Pago    For_Interes Per_Pago    Plazo   Usuario Cuota   Monto   Sdo_Capital Int_Corrientes  Int_Anticipado  Int_MorCobrar   Int_DifCobro    Sdo_CapPag  Sdo_IntPag  Sdo_IntMor  Sdo_Proyectado  Cuo_Pagadas Cuo_Atraso  Val_Atraso  Dias_Atraso Provision   Cod_Califica    Poliza  Deducible   Observaciones   Incremento  Destino Sistema Estado  Detalle_Estado  Num_Solicitud   Per_Gracia  Id_Adicionales  Reestructurado  Int_AntDesembolso   Age_Desembolso  Cod_Desembolso  Cue_Desembolso  Age_DebAutomatico   Cue_DebAutomatico   Cod_DebAutomatico   Desembolso  Costas  Nit_Juzgado Nom_Juzgado Abogado Honorarios  Polizas Categoria   Cta_Contable    Sdo_Anuales Capital_Acum    Int_LiqAcum Int_MoraDifCob  Cod_CalificaMes Provision_Interes   Provision_Otros CategoriaMes    Val_Desembolso  Fec_Bloqueo Usuario_gestor  Tasa_Desembolso Seg_Cartera Com_Bruta   Com_Adicional   GmfxC   TarjetaDB   Fec_BloqueoTdb  Fec_CreaTdb Fec_CancTdb diapago cod_calificareest   cod_calant  cod_calact  respaldoaportes Comision    seg_vida    Num_Control Tasa_Ant    �&  h$  t$     RowObject   �%         �%         �%         �%         �%         �%         �%         �%         �%         &         &         $&         ,&         4&         <&         L&         T&         `&         h&         p&         |&         �&         �&         Agencia NombreAgencia   Ciudad  NombreCiudad    Fec_UltActualiza    TpoCliente  OtroTpoCliente  CptcionClccion  PrdctoSlctar    OtroPrdctoSlctar    Monto   Plazo   Grntia  Linea   reestrctrcion   FrmaPgo dstncion    Cuota   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �&  �&     RowObjUpd   �'         �'         �'         �'         (         (         ((         8(         H(         X(         l(         t(         |(         �(         �(         �(         �(         �(         �(         �(         �(         �(         �(         �(         Agencia NombreAgencia   Ciudad  NombreCiudad    Fec_UltActualiza    TpoCliente  OtroTpoCliente  CptcionClccion  PrdctoSlctar    OtroPrdctoSlctar    Monto   Plazo   Grntia  Linea   reestrctrcion   FrmaPgo dstncion    Cuota   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   )          )  
   appSrvUtils D)       0)     xiRocketIndexLimit  l)        X)  
   gshAstraAppserver   �)        �)  
   gshSessionManager   �)  	 	     �)  
   gshRIManager    �)  
 
     �)  
   gshSecurityManager  *        �)  
   gshProfileManager   4*        *  
   gshRepositoryManager    `*        H*  
   gshTranslationManager   �*        t*  
   gshWebManager   �*        �*     gscSessionId    �*        �*     gsdSessionObj   �*        �*  
   gshFinManager   +        +  
   gshGenManager   8+        (+  
   gshAgnManager   \+        L+     gsdTempUniqueID |+        p+     gsdUserObj  �+        �+     gsdRenderTypeObj    �+        �+     gsdSessionScopeObj  �+       �+  
   ghProp  ,       �+  
   ghADMProps  ,,       ,  
   ghADMPropsBuf   T,       @,     glADMLoadFromRepos  p,       h,     glADMOk �,       �,  
   ghContainer �,    	   �,     cObjectName �,    
   �,     iStart  �,       �,     cAppService -        -     cASDivision 8-        -     cServerOperatingMode    \-       L-     cContainerType  �-       p-     cQueryString    �-       �-  
   hRowObject  �-       �-  
   hDataQuery  �-       �-     cColumns             �-     cDataFieldDefs   .    \  .  tCreditos   <.       0.  Agencias    X.       L.  Ubicacion   t.       h.  Clientes    �.    X  �.  RowObject         X  �.  RowObjUpd            @   H   J   K   �   �   �   �   X  Y  Z  [  r  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  O	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  K
  {
  |
  ~
  
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
    (  )  -  .  /  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  @  A  B  C  D  E  F  G  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  g      &  '  +  ,  -  /  2  <  X  j  �  �  �  (  @  A  [  k  l  m  p  q  r  y  z  �  �  �  ]  ^  j  �            {  
              {  �  �  �  �  �  �      &  '  1  K  e  o  �  �  �  �      ��  C:\Progress\OpenEdge\src\adm2\data.i �2  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    3  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i P3  7S , .\dsolprofina.i  �3  �  C:\Progress\OpenEdge\src\adm2\query.i    �3  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �3  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  4  F� ) C:\Progress\OpenEdge\gui\fnarg   <4   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   h4  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �4  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �4  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    $5  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   \5  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �5  Ds % C:\Progress\OpenEdge\gui\fn  �5  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �5  Q. # C:\Progress\OpenEdge\gui\set <6  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i d6  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �6  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �6  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i   7  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i T7  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �7   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �7  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   8  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   X8  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �8  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �8  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    9  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i \9  �j  C:\Progress\OpenEdge\gui\get �9  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �9  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �9  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i @:  Su  C:\Progress\OpenEdge\src\adm2\globals.i  t:  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �:  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �:  �  C:\Progress\OpenEdge\src\adm2\appsprto.i ,;  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   `;  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �;  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �;  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  ,<  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   `<  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �<  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �<  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   =  ��    D:\SPS\soportes\fodun\Prog\dsolprofina.w     �   �      �=  [  �     �=     �  %   �=  �        �=     �  .   �=  �   �     �=     �     �=  �   �     �=     q  #   >  �   o     >     M  #   (>  �   K     8>     )  #   H>  �   &     X>       #   h>  �        x>     �  #   �>  �   �     �>     �  #   �>  �   �     �>     �  #   �>  �   �     �>     s  #   �>  �   f     �>     N  -   ?  �   J     ?     7  ,   (?  k   �     8?  �  �     H?     �  +   X?  �  �     h?     �  +   x?  �  �     �?     �  +   �?  �  �     �?     �  +   �?  �  }     �?     c  +   �?  �  `     �?     F  +   �?  �  C     @     )  +   @  �  &     (@       +   8@  �  	     H@     �  +   X@  �  �     h@     �  +   x@  �  �     �@     �  +   �@  �  �     �@     �  +   �@  �  �     �@     {  +   �@  �  x     �@     ^  +   �@  �  [     A     A  +   A  �  >     (A     $  +   8A  �  !     HA       +   XA  �       hA     �  +   xA  �  �     �A     �  #   �A  �  �     �A     �  #   �A  j  `     �A     >  #   �A  i  =     �A       #   �A  h       B     �  #   B  ^  �     (B     �  *   8B  ]  �     HB     �  *   XB  \  �     hB     z  *   xB  [  y     �B     S  *   �B  Z  R     �B     ,  *   �B  Y  +     �B       *   �B  X       �B     �  *   �B  W  �     C     �  *   C  V  �     (C     �  *   8C  U  �     HC     i  *   XC  T  h     hC     B  *   xC  S  A     �C       *   �C  R       �C     �  *   �C  Q  �     �C     �  *   �C  P  �     �C     �  *   �C  O  �     D       *   D  N  ~     (D     X  *   8D  M  W     HD     1  *   XD  ?  #     hD       #   xD  	  �     �D     �  )   �D  �   �     �D     �  #   �D  �   �     �D     s  #   �D  �   r     �D     P  #   �D  �   O     E     -  #   E  �   ,     (E     
  #   8E  �   	     HE     �  #   XE  �   w     hE       (   xE  g        �E  a   �      �E     �  '   �E  _   �      �E     �  #   �E  ]   �      �E     b  #   �E  I   N      �E  �   E  !   F     �  &   F  �   �  !   (F     �  #   8F  �   �  !   HF     �  #   XF  �   �  !   hF       #   xF  g   e  !   �F     F     �F  O   .  !   �F  �   �  "   �F     �  %   �F  �   �  "   �F     .  $   �F  �   #  "   �F       #   G  �      "   G     �  #   (G  �   �  "   8G     �  #   HG  �   �  "   XG     �  #   hG  �   �  "   xG     b  #   �G  }   V  "   �G     4  #   �G     �  "   �G     j  !   �G     "      �G     �     �G     p     �G  �   g     H  O   Y     H     H     (H     �     8H  �   �     HH  �   �     XH  O   �     hH     �     xH     K     �H  y   #     �H  �     	   �H  G        �H     �
     �H     �
     �H  c   S
  	   �H  x   K
     �H  M   6
     I     %
     I     �	     (I  a   �	     8I  �  �	     HI     �	     XI  �  O	     hI  O   A	     xI     0	     �I     �     �I  �        �I     �     �I     3     �I  x   -     �I          �I     �     �I     �     J     �     J     l     (J  Q   \     8J           HJ     �     XJ     �     hJ     �     xJ  ]   �  	   �J     �     �J     D  	   �J     6  
   �J     "  	   �J  Z        �J     /     �J     �     �J     �     K     �     K  c   �     (K     ~     8K     6     HK     "     XK          hK     �      xK     !       �K           