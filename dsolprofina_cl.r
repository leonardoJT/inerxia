	��VA<eMX8   �                                              �l 385800EFutf-8 MAIN D:\SPS\soportes\fodun\Prog\dsolprofina_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,Agencia integer 0 0,NombreAgencia character 1 0,Ciudad character 2 0,NombreCiudad character 3 0,Fec_UltActualiza date 4 0,TpoCliente integer 5 0,OtroTpoCliente character 6 0,CptcionClccion character 7 0,PrdctoSlctar integer 8 0,OtroPrdctoSlctar character 9 0,Monto decimal 10 0,Plazo integer 11 0,Grntia character 12 0,Linea character 13 0,reestrctrcion logical 14 0,FrmaPgo character 15 0,dstncion character 16 0,Cuota decimal 17 0,RowNum integer 18 0,RowIdent character 19 0,RowMod character 20 0,RowIdentIdx character 21 0,RowUserProp character 22 0,ChangedFields character 23 0       ,b              �H              % ,b  ��               �              �G     +   �� t  W   l� D  X   �� �  Y   0�   [   4�   \   @� 0  ]   p�   ^   ��    `   ? �� |'  ISO8859-1                                                                           �a    �                                      �                   p�                �a  @    t   &)   ��              T�  �   �a       b                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          \  0D  �   �D     e  ���L<F  a                     `                �   �  �	      ,  
    
                    �             �                                                                                          �	          
  \  �	      �  
    
                  �  �             H                                                                                          �	          
    �	      �  
    
                  p  8             �                                                                                          �	          
  �  �	      0  
    
                    �             �                                                                                          �	          
  `  �	      �  
    
                  �  �             L                                                                                          �	          
    �	      �  
    
                  t  <  	           �                                                                                          �	          
  �  �	      4  
    
                     �  
           �                                                                                          �	          
  d  
      �  
    
                  �  �             P                                                                                          
          
    
      �                         x  @             �                                                                                          
            �   
      8                        $  �             �                                                                                           
            h	  .
      �  
    
                  �  �	             T	                                                                                          .
          
  
  <
      �	  
    
                  |	  D
              
                                                                                          <
          
  �
  J
      <
  
    
                  (
  �
             �
                                                                                          J
          
  l  X
      �
                        �
  �             X                                                                                          X
              h
      �                        �  H                                                                                                       h
            �  s
      @                        ,  �             �                                                                                          s
                �
      �                        �  p             \                                                                                          �
            �         �         X  �S  .   �S    ��      T                      �G          �I      �              �       1  X  �`  /   �`  1   �      0a         1         �    pT          dV      �                 ��                                               ��          �    L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                 F                  G                  H                  I                  J                  K                  L                  M                  N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                                 �(  �(  �(  �(              �(             )   )  ()  ,)              0)             X)  d)  h)  �)  x)          �)             �)  �)  �)  �)              �)             �)  �)   *  *               *             4*  <*  D*  L*              P*              |*  �*  �*  �*              �*             �*  �*  �*  �*                              �*  +  +  $+              (+             <+  H+  T+  p+  `+          t+              �+  �+  �+  �+  �+          �+              �+   ,  ,   ,              $,              L,  `,  l,  �,              �,              �,  �,  �,  �,              �,              -   -  ,-  X-  D-          \-              �-  �-  �-  �-              �-              �-  .  .  $.                              (.  <.  H.  d.              h.              �.  �.  �.  �.              �.             �.  �.  �.   /  /          $/              D/  P/  T/  d/              h/              �/  �/  �/  �/              �/              �/  �/  �/  �/              �/              0  0  ,0  @0              D0            `0  h0  �0  �0              �0             �0  �0  �0  �0              �0            1  $1  <1  T1              X1             |1  �1  �1  �1              �1             �1  �1  �1  2              2             <2  L2  d2  x2              |2             �2  �2  �2  �2              �2             �2  3  3  43              83             P3  \3  p3  �3              �3             �3  �3  �3  �3              �3             4  4  4  ,4              04              P4  \4  d4  p4                              t4  �4  �4  �4                             �4  �4  �4  �4                              �4  �4  �4  �4                             �4  �4  5   5              (5              d5  l5  x5  �5              �5              �5  �5  �5  �5              �5             6  ,6  46  D6              H6              x6  �6  �6  �6              �6             �6  �6  �6  �6              �6               7  7  7  $7              (7              L7  T7  X7  `7              d7             x7  �7  �7  �7                              �7  �7  �7  �7  �7          �7             8  8  $8  D8  08          H8              h8  x8  |8  �8              �8              �8  �8  �8  �8              �8              9  ,9  D9  \9              `9             �9  �9  �9  �9                              �9  �9  �9  �9                              �9  �9  �9  :              :              H:  \:  `:  t:                              x:  �:  �:  �:                              �:  �:  �:  �:                              �:  �:  �:   ;              ;              (;  0;  H;  L;              P;             x;  �;  �;  �;                              �;  �;  �;  �;                              �;  �;  �;  �;                              �;  �;  �;   <                              <  <  <   <                              $<  0<  4<  @<                              D<  T<  \<  `<                              d<  p<  �<  �<              �<             �<  �<  �<  �<              �<             =  =  0=  H=              L=             �=  �=  �=  �=              �=             �=  >  >  0>              8>              t>  �>  �>  �>                             �>  �>  �>  �>                             �>  �>  �>  ?                              ?  ?  ,?  <?                             @?  L?  X?  x?  l?          |?              �?  �?  �?  �?  �?          �?              �?  �?  �?  @              @             0@  <@  T@  h@  `@          l@             �@  �@  �@  �@  �@          �@             �@  �@  A  (A  A          ,A             LA  TA  lA  |A  tA          �A             �A  �A  �A  �A              �A              �A  �A  �A  B  B                          B   B  ,B  TB  DB                          XB  dB  pB  |B                              �B  �B  �B  �B                              �B  �B  �B  �B                              �B  �B  �B  �B                              �B  �B   C  C                              C   C  (C  8C              <C              LC  XC  hC  tC                             xC  �C  �C  �C                             �C  �C  �C  �C              �C              �C  �C  �C  D              D                                                         Agencia 999 Agencia 0   Agencia de radicaci�n del credito   Nit X(12)   Nit     N�mero de identificaci�n del cliente    Cod_Credito 999 Codigo Producto C�digo de Producto  0   Linea a la que pertenece el cr�dito Num_Credito 999999999   Num_Credito 0   N�mero de Cr�dito   Tip_Credito 9   Tipo de Producto Cs/Cm/H    1   Clase de Cr�dito    Pagare  X(14)   Pagare  ?   n�mero de pagar� que respalda el cr�dito    Tasa    >>9.999999  Tasa del Credito    0   tasa de inter�s de desembolso   Fec_Aprobacion  99/99/9999  Aprobaci�n  ?   Fec_Desembolso  99/99/9999  Fecha Aprobacion    ?   Fecha desembolso    Fec_Pago    99/99/9999  Fecha Pago  Fecha de Pago   ?   Fecha de pago de cuota  Fec_UltPago 99/99/9999  Fecha Ultima Pago   Fecha de Ultimo Pago    ?   fecha �ltimo pago Cuota Fec_Calificacion    99/99/9999  Fecha Calificaci�n  ?   Fecha Ultima calificaci�n de Cartera    Fec_Reestructurado  99/99/9999  Fecha de Reestructurado ?   Fecha de reestructuraci�n   Fec_PagAnti 99/99/9999  Fecha Limite Renueva Liquidaci�n    ?   Fecha l�miite para renovar cobro Inter�s    Fec_DifCobro    99/99/9999  Fecha de Dificil Cobro  Fecha de Dif Cobro  ?   fecha en que el cr�dito pas� a ser de dif�cil cobro Fec_CanceTotal  99/99/9999  Fecha de Cancelaci�n Total  ?   Fecha cancelaci�n total del cr�dito Fec_UltLiquidacion  99/99/9999  Fec_UltLiquidacion  ?   Fec_ProxLiquidacion 99/99/9999  Fecha Inicio Periodiciad    ?   fecha de inicio del per�odo para cobro de inter�s   For_Pago    9   Forma de Pago   1   Forma de pago de los abonos del cr�dito For_Interes 9   Periodo Deducci�n   Tipo de Interes V/A 1   Ingrese el per�odo de deducci�n Per_Pago    9   Periodo de Pago 1   periodicidad de pago de cuota del cr�dito   Plazo   >>>9    Plazo   0   plazo  del cr�dito  Usuario X(4)    Usuario     c�digo del usurio que matricul� el cr�dito  Cuota   ->>>>>,>>>,>>>,>>9.99   Cuota del Credito   0   Cuota Mensual del cr�dito   Monto   ->>>>>,>>>,>>>,>>9.99   Monto de Credito    0   Valor de Desembolso del cr�dito Sdo_Capital ->>>>>,>>>,>>>,>>9.99   Saldo Capital   0   Saldo de capital actual de deuda    Int_Corrientes  ->>>>>,>>>,>>>,>>9.99   Intereses Corrientes    0   Valor de los intereses corrientes   Int_Anticipado  ->>>>>,>>>,>>>,>>9.99   Interes Anticipado  0   Interes anticipado  Int_MorCobrar   ->>>>>,>>>,>>>,>>9.99   Interes de Mora por Cobrar  0   Valor de inter�s de mora por cobrar Int_DifCobro    ->>>>>,>>>,>>>,>>9.99   Interes Dif. Cobro  0   Inter�s de dificil cobro    Sdo_CapPag  ->>>>>,>>>,>>>,>>9.99   Acumulado de Capital Pago   0   Capital pagado al cr�dito   Sdo_IntPag  ->>>>>,>>>,>>>,>>9.99   Saldo Intereses pagados 0   Saldo intereses pagados Sdo_IntMor  ->>>,>>>,>>>,>>9    Saldo Intereses Mora    0   Saldo de intereses de mora  Sdo_Proyectado  ->>>>>,>>>,>>>,>>9.99   Sdo Proyectado  0   Ingrese el valor proyectado del cr�dito Cuo_Pagadas 999 Cuotas Pagadas  0   Cuotas pagadas por el cr�dito   Cuo_Atraso  9999    Cuo_Atraso  0   Val_Atraso  >>,>>>,>>>,>>9  Val_Atraso  0   Dias_Atraso 99999   Dias_Atraso 0   Provision   >>>,>>>,>>9 Provision   0   Cod_Califica    99999   Calificacion del Cr�dito    00001   Categor�a dada por la calificaci�n de cartera de cr�ditos   Poliza  >>>>>>>9    Numero de Poliza    0   Ingrese el n�mero de p�liza que tiene el cr�dito    Deducible   ->>>>>,>>>,>>>,>>9.99   Deducibles  0   Sumatoria de deducibles al cr�dito  Observaciones   X(50)   Observaciones       Ingrese las observaciones que tiene el cr�dito  Incremento  ->>>>>,>>>,>>>,>>9.99   Incremento  0   Ingrese el incremento   Destino 99999   Destino 0   Ingrese el destino del cr�dito  Sistema 99999   Sistema Liquidaci�n 0   Ingrese el sistema de liquidaci�n   Estado  9   Estado  1   Estado del cr�dito  Detalle_Estado  99  Detalle_Estado  1   Num_Solicitud   99999999    Nro Solicitud   N�mero de Solicitud 0   Ingrese el n�mero de solicitud de cr�dito   Per_Gracia  >>>9    Per.Gracia  Peri�do de Gracia   0   Ingrese el per�odo de gracia    Id_Adicionales  9   Deducciones 1   Si el valor adicional se suma o no al total del pr�stamo.   Reestructurado  9   Reestructurado  2   Si el cr�dito ha sido reestructurado    Int_AntDesembolso   ->>>>>,>>>,>>>,>>9.99   Interes Ant. Desembolso 0   Intereses por correr la primera Cuota   Age_Desembolso  999 Age_Desembolso  0   Cod_Desembolso  999 Cod_Desembolso  0   Cue_Desembolso  X(14)   Cuenta de Ahorros       Ingrese la Cuenta de ahorros donde se consigna el valor credito Age_DebAutomatico   999 Age_DebAutomatico   0   Cue_DebAutomatico   X(14)   Cue_DebAutomatico       Cod_DebAutomatico   999 Cod_DebAutomatico   0   Desembolso  9   Tipo de Desembolso  2   Forma del Desembolso del Cr�dito    Costas  ->>>>>,>>>,>>>,>>9.99       0   Ingrese el valor aprobado del cr�dito   Nit_Juzgado X(14)   Nit_Juzgado     Nom_Juzgado X(50)   Nom_Juzgado     Abogado yes/no  Abogado no  Honorarios  >>>,>>>,>>9 Honorarios  0   Polizas >>>,>>>,>>9 Polizas 0   Categoria   X   Categoria       Cta_Contable    X(14)           Sdo_Anuales ->>>>>,>>>,>>>,>>9.99   Saldos!Anuales  0   Ingrese los saldo mensuales Capital_Acum    ->>>,>>>,>>>,>>9.99 Capital Acumulado   0   Capital Acumulado cumplido  Int_LiqAcum ->>>,>>>,>>>,>>9.99 Int_Liquidado Acumulado 0   Int_Liquidado Acumulado hasta el ultimo periodo cumplido    Int_MoraDifCob  ->,>>>,>>>,>>9.99   Mora Dif.Cobro por Cobrar   0   Valor de inter�s de mora por cobrar(Dif.Cobro)  Cod_CalificaMes 99999   Calificacion del Cr�dito    00001   Categor�a dada por la calificaci�n de cartera de cr�ditos   Provision_Interes   >>>,>>>,>>9 Provision_Interes   0   Provision_Otros >>>,>>>,>>9 Costas Polizas y Honorarios 0   CategoriaMes    X   Categoria       Val_Desembolso  >>>>,>>>,>>9.99 Val_Desembolso  0   Fec_Bloqueo 99/99/9999  Fecha de Bloqueo    Fec_Bloqueo ?   Fecha Bloqueo   Usuario_gestor  X(4)    Usuario Gestor      ?   Usuario que hizo la negociaci�n Tasa_Desembolso >>9.999999  Tasa anual nominal  0   tasa de inter�s de desembolso   Seg_Cartera ->>>>>,>>>,>>>,>>9.99   Seg.cartera SegCart 0   Valor del seguro de cartera Com_Bruta   ->>>>>,>>>,>>>,>>9.99   Com.Bruta   ComBruta    0   Valor de la Comisi�n Bruta  Com_Adicional   ->>>>>,>>>,>>>,>>9.99   Com.Adic    ComAdicional    0   Valor de la Comisi�n Adicional  GmfxC   ->>>>>,>>>,>>>,>>9.99   GmfxC   GmfxC   0   Valor de la Cuota de Manejo TarjetaDB   X(16)   TarjetaDB       Numero de la Tarjeta D�bito Fec_BloqueoTdb  99/99/9999  Fec.Bloqueo FecBloqueo  ?   Fec_CreaTdb 99/99/9999  Creacion de Tarjeta DB  aper.TarjetaDB  ?   Fec_CancTdb 99/99/9999  FecCancTdb  ?   diapago >>>,>>9 diapago 0   cod_calificareest   >>>,>>9 cod_calificareest   0   cod_calant  >9  cod_calant  0   cod_calact  >9  cod_calact  0   respaldoaportes yes/no  respaldoaportes no  respaldoaportes Comision    ->>>,>>>,>>9.99 Comision    0   seg_vida    ->>>,>>>,>>9    seg_vida    0   Num_Control 999999999   Num_Control 0   N�mero de Cr�dito   Tasa_Ant    >>9.999999  Tasa del Credito    0   tasa de inter�s de desembolso   �    3 D Y y ��  ���b������    � �����������                                         �             �       ��      ���             '        '        '        '        #'        0'        7'        D'        L'        W'        d'                �     i  i  i     i  i     i  i  i  i     i  i  i  i      i  i  i      i  i  i     i   i  i  i     i  i     i     i   i 	 i     i   i 
 i     	 	2 	 	0 	
 	 	 	 	 	 	    #   +   /   ;   G   S   Z   _   n   }   �   �   �   �   �   �   �   �         #  )  1  7  =  I  X  g  u  �  �  �  �  �  �  �  �  �  �  �  �      !  )  1  8  G  U  `  o  ~  �  �  �  �  �  �  �  �        %  0  8  B  O  [  h  t  �  �  �  �  �  �  �  �  �         &  0  ?  K  W  _  q  |  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                             �M  �M  �M  �M  �M          �M             �M  �M   N   N  N          $N             <N  DN  LN  \N  TN          `N             hN  xN  �N  �N  �N          �N             �N  �N  �N  O   O           O             @O  LO  PO  pO  `O          tO             �O  �O  �O  �O  �O          �O             �O  P  P  ,P  P                         0P  @P  DP  tP  \P          xP             �P  �P  �P  �P  �P          �P             �P  Q  Q  ,Q  $Q          0Q             8Q  @Q  HQ  XQ  PQ          \Q             tQ  |Q  �Q  �Q  �Q          �Q             �Q  �Q  �Q   R  �Q                         R  R  R  @R  ,R                         DR  LR  PR  pR  `R                         tR  �R  �R  �R  �R                         �R  �R  �R  �R  �R          �R             �R  �R  �R  �R                              S  S  S   S                              $S  ,S  4S  <S                             @S  LS  TS  `S                             dS  pS  xS  �S                                                                          Agencia 999 Agencia Agencia 0   Agencia NombreAgencia   X(40)   Nombre Agencia  Nombre!Agencia      Nombre De La Agencia    Ciudad  X(8)    Ciudad  Ciudad      Ciudad  NombreCiudad    X(40)   Nombre Ciudad   Nombre!Ciudad       Ingrese el nombre de la ubicaci�n   Fec_UltActualiza    99/99/9999  Ultima Actualizaci�n    Ultima!Actualizaci�n    TODAY   Fecha de actualizaci�n de datos TpoCliente  999 Tipo Cliente    Tipo!Cliente    0   Tipo De Cliente OtroTpoCliente  X(50)   Otro Tipo Cliente   Otro Tipo Cliente       Ingrese las observaciones que tiene el cr�dito  CptcionClccion  X   Clase Producto  Clase!Producto      PrdctoSlctar    9   Producto A Solicitar    Producto!A Solicitar    1   Producto A Solicitar    OtroPrdctoSlctar    X(14)   Otro Producto A Solicitar   Otro Producto!A Solicitar   ?   Cual Otro Producto? Monto   ->>>>>,>>>,>>>,>>9.99   Monto   Monto   0   Monto   Plazo   >>>9    Plazo   Plazo   0   Producto A Solicitar    Grntia  X(14)   Garant�a    Garant�a        Ingrese la Cuenta de ahorros donde se consigna el valor credito Linea   X(14)   L�nea   L�nea       reestrctrcion   S/N Reestructuraci�n    Reestructuraci�n    no  FrmaPgo X   Forma De Pago   Forma De Pago       dstncion    X(14)   Destinaci�n Destinaci�n     Cuota   ->>>>>,>>>,>>>,>>9.99   Cuota   Cuota   0   Cuota   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �   �  ���������    �   �                   �%        �%        �%                �     i     i     i     	 	 	    #   %   %  �#  �#  '%  2%  A%  P%  ]%  7  #  n%  u%  {%  �%  �%  1  �%  �%  �%  �%  �%                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                               �Z  �Z  �Z  �Z  �Z          �Z             �Z  �Z  �Z  [   [          [             ,[  4[  <[  L[  D[          P[             X[  h[  p[  �[  �[          �[             �[  �[  �[  \  �[          \             0\  <\  @\  `\  P\          d\             t\  �\  �\  �\  �\          �\             �\  �\  �\  ]  ]                          ]  0]  4]  d]  L]          h]             �]  �]  �]  �]  �]          �]             �]  �]  ^  ^  ^           ^             (^  0^  8^  H^  @^          L^             d^  l^  t^  �^  �^          �^             �^  �^  �^  �^  �^                         �^  _  _  0_  _                         4_  <_  @_  `_  P_                         d_  p_  x_  �_  �_                         �_  �_  �_  �_  �_          �_             �_  �_  �_  �_                             �_  �_  `  `                              `  `  $`  ,`                             0`  <`  D`  P`                             T`  ``  h`  t`                              x`  �`  �`  �`                                                                          Agencia 999 Agencia Agencia 0   Agencia NombreAgencia   X(40)   Nombre Agencia  Nombre!Agencia      Nombre De La Agencia    Ciudad  X(8)    Ciudad  Ciudad      Ciudad  NombreCiudad    X(40)   Nombre Ciudad   Nombre!Ciudad       Ingrese el nombre de la ubicaci�n   Fec_UltActualiza    99/99/9999  Ultima Actualizaci�n    Ultima!Actualizaci�n    TODAY   Fecha de actualizaci�n de datos TpoCliente  999 Tipo Cliente    Tipo!Cliente    0   Tipo De Cliente OtroTpoCliente  X(50)   Otro Tipo Cliente   Otro Tipo Cliente       Ingrese las observaciones que tiene el cr�dito  CptcionClccion  X   Clase Producto  Clase!Producto      PrdctoSlctar    9   Producto A Solicitar    Producto!A Solicitar    1   Producto A Solicitar    OtroPrdctoSlctar    X(14)   Otro Producto A Solicitar   Otro Producto!A Solicitar   ?   Cual Otro Producto? Monto   ->>>>>,>>>,>>>,>>9.99   Monto   Monto   0   Monto   Plazo   >>>9    Plazo   Plazo   0   Producto A Solicitar    Grntia  X(14)   Garant�a    Garant�a        Ingrese la Cuenta de ahorros donde se consigna el valor credito Linea   X(14)   L�nea   L�nea       reestrctrcion   S/N Reestructuraci�n    Reestructuraci�n    no  FrmaPgo X   Forma De Pago   Forma De Pago       dstncion    X(14)   Destinaci�n Destinaci�n     Cuota   ->>>>>,>>>,>>>,>>9.99   Cuota   Cuota   0   Cuota   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �   �  ���������    �   �                   �%        �%        �%                �     i     i     i     	 	 	    #   %   %  �#  �#  '%  2%  A%  P%  ]%  7  #  n%  u%  {%  �%  �%  1  �%  �%  �%  �%  �%  �%    ��                            ����                            q'    ��                    ��    undefined                                                               �       ��  x   `    �  ��                    �����               �cq        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /      �                                3   ����       $       8  ���                       8      
                       � ߱        x  �   "   D       �     E          �  �   M      �  9   O      �                         � ߱          $   P   �  ���                       ��     �     �          4   ����                �                      ��                  �   �                   ��=           �   ,  �  	  �   �                                        3   ����$      O   �   ��  ��  0  batchServices                               �  h      ��                  �  �  �              �>        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            clientSendRows                              �  �      ��                  �  �  �              >        O   ����    e�          O   ����    R�          O   ����    ��            ��   ,             �               ��   T                             ��   |             H               ��   �             p               ��                  �           ��                            ����                            commitTransaction                               �  t      ��                  �  �  �              L�=        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             �  h      ��                  �  �  �               �=        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              \>        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  �  �   
              >        O   ����    e�          O   ����    R�          O   ����    ��            ��   L
             
               �� 
          �       @
  
         ��                            ����                            destroyServerObject                             4        ��                  �  �  L              �׈        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                4        ��                  �  �  L              `؈        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              (        ��                  �  �  @              و        O   ����    e�          O   ����    R�          O   ����    ��            ��                  X           ��                            ����                            fetchFirst                              D  ,      ��                  �  �  \              h��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8         ��                  �  �  P              䥉        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               ,        ��                  �  �  D              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                                        ��                  �  �  8              p$�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                                       ��                  �  �  0              %�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  H           ��                            ����                            home                                0        ��                  �  �  H              L�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                ,        ��                  �  �  D              ��/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              ,        ��                  �  �  D               �/        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             $        ��                  �  �  <              ؎/        O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            printToCrystal                              D  ,      ��                  �  �  \              ��/        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             t               ��   �             �               ��                  �           ��                            ����                            refreshRow                              �  �      ��                  �  �  �              `��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              �  �      ��                  �  �  �              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��   4                             ��   \             (               ��   �             P               ��   �             x               ��   �             �               �� 
  �      �       �  
             ��                  �           ��                            ����                            restartServerObject                             �  �      ��                  �  �  �              �˵        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �e8        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              �Y8        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            saveContextAndDestroy                               �  �      ��                  �                   ^8        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ,           ��                            ����                            serverSendRows                                        ��                  
    4               ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   �              L                ��   �              t                ��   �              �                ��   �              �                ��    !             �                �� 
          �       !  
         ��                            ����                            serverFetchRowObjUpdTable                               "  �!      ��                      ("              )�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       @"  
         ��                            ����                            setPropertyList                             0#  #      ��                      H#              H-�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  `#           ��                            ����                            serverSendRows                              P$  8$      ��                    "  h$              �1�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �$             �$               ��   �$             �$               ��   %             �$               ��   ,%             �$               ��   T%              %               �� 
          �       H%  
         ��                            ����                            startServerObject                               <&  $&      ��                  $  %  T&              A�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                4'  '      ��                  '  *  L'              �A�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �'             d'               ��                  �'           ��                            ����                            submitForeignKey                                �(  h(      ��                  ,  0  �(              PJ�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��   )             �(               ��                   )           ��                            ����                            submitValidation                                �)  �)      ��                  2  5  *              �Q�        O   ����    e�          O   ����    R�          O   ����    ��            ��   X*             $*               ��                  L*           ��                            ����                            synchronizeProperties                               D+  ,+      ��                  7  :  \+              tX�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             t+               ��                  �+           ��                            ����                            transferToExcel                             �,  t,      ��                  D  I  �,              l^�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��   -             �,               ��   @-             -               ��                  4-           ��                            ����                            undoTransaction                             $.  .      ��                  K  L  <.              h�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                              /  /      ��                  N  Q  8/              �j�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �/             P/               ��                  x/           ��                            ����                            updateQueryPosition                             l0  T0      ��                  S  T  �0              �p�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             `1  H1      ��                  V  X  x1              �s�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �1           ��                            ����                            addRow          �1       2     �      CHARACTER,INPUT pcViewColList CHARACTER cancelRow    2      H2      t2   	 �      CHARACTER,  canNavigate T2      �2      �2    �      LOGICAL,    closeQuery  �2      �2      �2   
 
      LOGICAL,    columnProps �2      �2      3          CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   �2      \3      �3   	 !      CHARACTER,INPUT pcViewColList CHARACTER copyRow h3      �3      �3    +      CHARACTER,INPUT pcViewColList CHARACTER createRow   �3       4      ,4   	 3      LOGICAL,INPUT pcValueList CHARACTER deleteRow   4      P4      |4   	 =      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    \4      �4      �4  	  G      CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   �4      5      85  
  P      CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow 5      |5      �5    ^      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    �5      �5      �5    f      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds �5      P6      |6    s      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  \6      �6      �6          CHARACTER,  hasForeignKeyChanged    �6      �6       7    �      LOGICAL,    openDataQuery    7      ,7      \7    �      LOGICAL,INPUT pcPosition CHARACTER  openQuery   <7      �7      �7   	 �      LOGICAL,    prepareQuery    �7      �7      �7    �      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    �7      8      88    �      LOGICAL,INPUT pcDirection CHARACTER rowValues   8      \8      �8   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   h8      �8      9   	 �      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   �8      P9      |9   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   \9      �9      �9    �      CHARACTER,  assignDBRow                             |:  d:      ��                  >  @  �:              \��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �:  
         ��                            ����                            bufferCopyDBToRO                                �;  �;      ��                  B  G  �;              ��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <             �;  
             �� 
  ,<             �;  
             ��   T<              <               ��                  H<           ��                            ����                            compareDBRow                                8=   =      ��                  I  J  P=              h��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             0>  >      ��                  L  N  H>               ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  `>           ��                            ����                            dataAvailable                               P?  8?      ��                  P  R  h?              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �?           ��                            ����                            fetchDBRowForUpdate                             t@  \@      ��                  T  U  �@              ԰�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              hA  PA      ��                  W  X  �A              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               \B  DB      ��                  Z  [  tB              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               PC  8C      ��                  ]  ^  hC              (��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               DD  ,D      ��                  `  a  \D               ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DE  ,E      ��                  c  e  \E              D��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tE  
         ��                            ����                            initializeObject                                hF  PF      ��                  g  h  �F              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                `G  HG      ��                  j  l  xG              @Ɛ        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �G  
         ��                            ����                            releaseDBRow                                �H  hH      ��                  n  o  �H              �ʐ        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             tI  \I      ��                  q  r  �I              4ː        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               lJ  TJ      ��                  t  w  �J              Pΐ        O   ����    e�          O   ����    R�          O   ����    ��            ��   �J             �J               ��                  �J           ��                            ����                            addQueryWhere   �9      ,K      \K          LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    <K      �K      �K    *      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO �K      @L      tL    ?      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   TL      �L      M    S      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  �L      PM      �M    e      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  `M      �M      �M    t      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �M      �M      0N    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable N      TN      �N    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    `N      �N      �N     �      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �N      �N      ,O  !  �      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  O      TO      �O  "  �      HANDLE,INPUT pcColumn CHARACTER excludeColumns  dO      �O      �O  #  �      CHARACTER,INPUT iTable INTEGER  getDataColumns  �O      �O      $P  $  �      CHARACTER,  getForeignValues    P      0P      dP  %  �      CHARACTER,  getQueryPosition    DP      pP      �P  &         CHARACTER,  getQuerySort    �P      �P      �P  '        CHARACTER,  getQueryString  �P      �P      Q  (        CHARACTER,  getQueryWhere   �P      (Q      XQ  )  -      CHARACTER,  getTargetProcedure  8Q      dQ      �Q  *  ;      HANDLE, indexInformation    xQ      �Q      �Q  +  N      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �Q      0R      dR  ,  _      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  DR      �R      �R  -  p      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    �R      |S      �S  .        CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �S      (T      XT  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  8T      |T      �T  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �T      U      LU  1  �      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    ,U      tU      �U  2  �      LOGICAL,    removeQuerySelection    �U      �U      �U  3  �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   �U      ,V      \V  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  <V      �V      �V  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �V      �V       W  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    �V      \W      �W  7        LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    pW      �W      �W  8        LOGICAL,INPUT pcSort CHARACTER  setQueryString  �W      X      4X  9  )      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   X      \X      �X  :  8      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   lX      �X      �X  ;  F      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �Y  pY      ��                      �Y              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  hZ      ��                      �Z              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             |[  d[      ��                      �[              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                x\  `\      ��                      �\              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              x]  `]      ��                      �]              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             t^  \^      ��                     !  �^              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             l_  T_      ��                  #  %  �_              �        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �_  
         ��                            ����                            startServerObject                               �`  x`      ��                  '  (  �`              t�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �a  pa      ��                  *  ,  �a              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �a           ��                            ����                            getAppService   �X       b      Pb  <  X      CHARACTER,  getASBound  0b      \b      �b  = 
 f      LOGICAL,    getAsDivision   hb      �b      �b  >  q      CHARACTER,  getASHandle �b      �b      �b  ?        HANDLE, getASHasStarted �b      c      4c  @  �      LOGICAL,    getASInfo   c      @c      lc  A 	 �      CHARACTER,  getASInitializeOnRun    Lc      xc      �c  B  �      LOGICAL,    getASUsePrompt  �c      �c      �c  C  �      LOGICAL,    getServerFileName   �c      �c      ,d  D  �      CHARACTER,  getServerOperatingMode  d      8d      pd  E  �      CHARACTER,  runServerProcedure  Pd      |d      �d  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �d      �d      $e  G  	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   e      Le      |e  H  	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle \e      �e      �e  I  !	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �e      �e      f  J 	 -	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �e      8f      pf  K  7	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  Pf      �f      �f  L  L	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �f      �f      g  M  [	      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �f      <g      tg  N  m	      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             $h  h      ��                  �  �  <h              �S�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �h             Th  
             ��   �h             |h               �� 
                 �h  
         ��                            ����                            addMessage                              �i  xi      ��                  �  �  �i              �b�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �i             �i               ��   j             �i               ��                  j           ��                            ����                            adjustTabOrder                               k  �j      ��                  �  �  k              �j�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  dk             0k  
             �� 
  �k             Xk  
             ��                  �k           ��                            ����                            applyEntry                              ll  Tl      ��                      �l              Hr�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �l           ��                            ����                            changeCursor                                �m  tm      ��                      �m              �v�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �m           ��                            ����                            createControls                              �n  �n      ��                  	  
  �n              �z�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �o  �o      ��                      �o              �}�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �p  �p      ��                      �p              Ѐ�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �q  �q      ��                      �q              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �r  xr      ��                      �r              h��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �s  ls      ��                      �s              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �t  ht      ��                      �t              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              |u  du      ��                    #  �u              ��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �u             �u  
             ��   v             �u               ��   0v             �u               ��                  $v           ��                            ����                            modifyUserLinks                             w  �v      ��                  %  )  ,w              蘑        O   ����    e�          O   ����    R�          O   ����    ��            ��   xw             Dw               ��   �w             lw               �� 
                 �w  
         ��                            ����                            removeAllLinks                              �x  lx      ��                  +  ,  �x              L��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              xy  `y      ��                  .  2  �y              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �y             �y  
             ��   z             �y               �� 
                 �y  
         ��                            ����                            repositionObject                                �z  �z      ��                  4  7  {              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   P{             {               ��                  D{           ��                            ����                            returnFocus                             0|  |      ��                  9  ;  H|              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 `|  
         ��                            ����                            showMessageProcedure                                X}  @}      ��                  =  @  p}              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   �}             �}               ��                  �}           ��                            ����                            toggleData                              �~  �~      ��                  B  D  �~              4��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �~           ��                            ����                            viewObject                              �  �      ��                  F  G  �              l��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  Tg      (�      T�  O 
 �
      LOGICAL,    assignLinkProperty  4�      `�      ��  P  �
      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   t�      �      �  Q  �
      CHARACTER,  getChildDataKey ��      (�      X�  R  �
      CHARACTER,  getContainerHandle  8�      d�      ��  S        HANDLE, getContainerHidden  x�      ��      ԁ  T  !      LOGICAL,    getContainerSource  ��      ��      �  U  4      HANDLE, getContainerSourceEvents    �      �      X�  V  G      CHARACTER,  getContainerType    8�      d�      ��  W  `      CHARACTER,  getDataLinksEnabled x�      ��      ؂  X  q      LOGICAL,    getDataSource   ��      �      �  Y  �      HANDLE, getDataSourceEvents �      �      P�  Z  �      CHARACTER,  getDataSourceNames  0�      \�      ��  [  �      CHARACTER,  getDataTarget   p�      ��      ̃  \  �      CHARACTER,  getDataTargetEvents ��      ؃      �  ]  �      CHARACTER,  getDBAware  �      �      D�  ^ 
 �      LOGICAL,    getDesignDataObject $�      P�      ��  _  �      CHARACTER,  getDynamicObject    d�      ��      Ą  `  �      LOGICAL,    getInstanceProperties   ��      Є      �  a        CHARACTER,  getLogicalObjectName    �      �      L�  b  "      CHARACTER,  getLogicalVersion   ,�      X�      ��  c  7      CHARACTER,  getObjectHidden l�      ��      ȅ  d  I      LOGICAL,    getObjectInitialized    ��      ԅ      �  e  Y      LOGICAL,    getObjectName   �      �      H�  f  n      CHARACTER,  getObjectPage   (�      T�      ��  g  |      INTEGER,    getObjectParent d�      ��      ��  h  �      HANDLE, getObjectVersion    ��      Ȇ      ��  i  �      CHARACTER,  getObjectVersionNumber  ܆      �      @�  j  �      CHARACTER,  getParentDataKey     �      L�      ��  k  �      CHARACTER,  getPassThroughLinks `�      ��      ��  l  �      CHARACTER,  getPhysicalObjectName   ��      ̇      �  m  �      CHARACTER,  getPhysicalVersion  �      �      D�  n  �      CHARACTER,  getPropertyDialog   $�      P�      ��  o        CHARACTER,  getQueryObject  d�      ��      ��  p  "      LOGICAL,    getRunAttribute ��      ̈      ��  q  1      CHARACTER,  getSupportedLinks   ܈      �      <�  r  A      CHARACTER,  getTranslatableProperties   �      H�      ��  s  S      CHARACTER,  getUIBMode  d�      ��      ��  t 
 m      CHARACTER,  getUserProperty ��      ȉ      ��  u  x      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ؉       �      X�  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles 8�      ��      ��  w  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      Њ       �  x  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      <�      h�  y  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   H�      ԋ      �  z  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �      (�      X�  {  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  8�      ��      ��  |  �      CHARACTER,  setChildDataKey ��      ��      �  }  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ̌      �      H�  ~  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  (�      h�      ��          LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    |�      ��      ��  �  "      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ؍      �      P�  �  ;      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   0�      x�      ��  �  O      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      Ȏ      ��  �  ]      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ܎      $�      X�  �  q      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   8�      ��      ��  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      ԏ      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �      ,�      X�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject 8�      x�      ��  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      Ԑ      �  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      $�      \�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    <�      ��      ��  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      ԑ      �  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      ,�      \�  �        LOGICAL,INPUT pcName CHARACTER  setObjectParent <�      |�      ��  �  !      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      ̒       �  �  1      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      (�      \�  �  B      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks <�      ��      ��  �  S      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ؓ      �  �  g      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      0�      d�  �  }      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute D�      ��      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      ��      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      8�      t�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  T�      ��      ĕ  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      T�      ��  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   `�      ��      Ж  � 	 �      CHARACTER,INPUT pcName CHARACTER    ��     ]  �  |�          4   ����D                ��                      ��                  ^  �                  ��           ^  �         _  ��  �          4   ����T                $�                      ��                  `  �                  8�           `  ��  �     w  <�  ��          4   ����h                ��                      ��                  �  �                  ��           �  L�         �                                  �     
                    � ߱        @�  $   �  �  ���                           $   �  l�  ���                                                 � ߱        ��     �  ��   �          4   ����0                0�                      ��                  �  T	                  p�           �  ��  d�  o   �      ,                                 ��  $   �  ��  ���                       �  @         �              � ߱        К  �   �  �      �  �   �  8      ��  �   �  �      �  �   �          �  �   �  �      4�  �   �        H�  �   �  �      \�  �   �  �      p�  �   �  4      ��  �   �  �      ��  �   �  $	      ��  �   �  �	      ��  �   �  
      ԛ  �   �  X
      �  �   �  �
      ��  �   �  H      �  �   �  �      $�  �   �  �      8�  �   �  4      L�  �   �  �      `�  �   �        t�  �   �  �      ��  �   �        ��  �   �  �      ��  �   �        Ĝ  �   �  x      ؜  �   �  �      �  �   �  (       �  �   �  �      �  �   �  �      (�  �   �  L      <�  �   �  �      P�  �   �  �      d�  �   �         x�  �   �  <      ��  �   �  �      ��  �   �  �      ��  �   �  0      ȝ  �   �  l      ܝ  �   �  �      �  �   �  �      �  �   �         �  �   �  \      ,�  �   �  �          �   �  �                      L�          ��  ��      ��                  {	  �	  О              ��        O   ����    e�          O   ����    R�          O   ����    ��      D     
                �                     �                         � ߱        x�  $  �	  �  ���                           O   �	  ��  ��                 �          ԟ  ܟ    ğ                                             ��                            ����                                �9      @�      ��     V     �                       �  �                     8�     �	  ��  �          4   ����                 �                      ��                  �	  P
                  ���           �	  ��  4�  �   �	  |      H�  �   �	  �      \�  �   �	  l      p�  �   �	  �      ��  �   �	  d      ��  �   �	  �      ��  �   �	  T      ��  �   �	  �      ԡ  �   �	  L      �  �   �	  �      ��  �   �	  <      �  �   �	  �      $�  �   �	  4          �   �	  �      ��     �
  P�  ��          4   ����                 Т                      ��                  �
                    41�           �
  `�  �  �   �
  �      ��  �   �
  �      �  �   �
  x       �  �   �
  �      4�  �   �
  h       H�  �   �
  �       \�  �   �
  !      p�  �   �
  �!      ��  �   �
   "      ��  �   �
  <"      ��  �   �
  x"      ��  �   �
  �"      ԣ  �   �
  (#      �  �   �
  �#      ��  �   �
   $      �  �   �
  �$      $�  �   �
  %      8�  �   �
  �%      L�  �   �
  &      `�  �   �
  �&      t�  �   �
   '      ��  �   �
  <'      ��  �   �
  x'      ��  �   �
  �'      Ĥ  �   �
  h(      ؤ  �   �
  �(      �  �   �
  `)       �  �   �
  �)      �  �   �
  X*      (�  �   �
  �*      <�  �   �
  +      P�  �   �
  �+      d�  �   �
   ,      x�  �   �
  <,      ��  �   �
  �,      ��  �   �
  ,-      ��  �   �
  �-      ȥ  �   �
  .      ܥ  �   �
  �.      �  �   �
  /      �  �   �
  �/      �  �   �
  �/      ,�  �   �
  p0      @�  �   �
  �0      T�  �   �
  `1      h�  �   �
  �1      |�  �   �
  X2          �   �
  �2      D�     -  ��  �          4   �����2                (�                      ��                  .  �                  d �           .  ��  <�  �   2  \3      P�  �   3  �3      d�  �   4  L4      x�  �   5  �4      ��  �   6  45      ��  �   7  �5      ��  �   8  $6      ȧ  �   9  �6      ܧ  �   :  7      �  �   ;  �7      �  �   <  8      �  �   =  �8      ,�  �   >  9      @�  �   ?  �9      T�  �   @  �9      h�  �   A  h:      |�  �   B  �:      ��  �   C  `;      ��  �   D  �;      ��  �   E  H<      ̨  �   F  �<      �  �   G   =      ��  �   H  t=      �  �   I  �=      �  �   J  l>      0�  �   K  �>          �   L  T?      H�     �  \�  ̩          4   �����?  	              ܩ                      ��             	     �  l                  ��           �  l�  �  �   �  $@      �  �   �  �@      �  �   �  A      ,�  �   �  �A      @�  �   �  B      T�  �   �  �B      h�  �   �  C      |�  �   �  �C      ��  �   �  �C      ��  �   �  pD      ��  �   �  �D      ̪  �   �  hE      �  �   �  �E      ��  �   �  XF      �  �   �  �F      �  �   �  PG      0�  �   �  �G      D�  �   �  HH      X�  �   �  �H      l�  �   �  0I      ��  �   �  �I      ��  �   �  (J      ��  �   �  dJ      ��  �   �  �J      Ы  �   �  \K      �  �   �  �K      ��  �   �  TL      �  �   �  �L          �   �  DM      getRowObjUpdStatic  deleteRecordStatic  ܬ     !  `�  p�          4   �����M      /   "  ��     ��                          3   �����M            ̬                      3   �����M  ��     +  ��  d�  ��      4   ���� N  
              t�                      ��             
     ,  �                  ��           ,  �  ��  �   0  `N      �  $   1  ��  ���                       �N     
                    � ߱        ��  �   2  �N      L�  $   4   �  ���                       �N  @         �N              � ߱        �  $   7  x�  ���                       (O       	       	           � ߱        8P     
                �P                     R  @        
 �Q              � ߱        ��  V   A  ��  ���                        R       	       	       DR       
       
       �R       	       	           � ߱        (�  $   ]  4�  ���                       @S     
                �S                     U  @        
 �T              � ߱            V   o  į  ���                                      |�                      ��                  �  -                  8�           �  T�  U     
                �U                     �V  @        
 �V          LW  @        
 W          �W  @        
 lW          X  @        
 �W              � ߱            V   �  İ  ���                        adm-clone-props ,�  ��              �     W     4                          0  �                     start-super-proc    ��  �  �           �     X                                                       �     E  ��  ��          4   �����[      /   F  ز     �                          3   �����[            �                      3   �����[  p�  $   `  D�  ���                       �[                         � ߱        D�     p  ��  ��  ��      4   ����\                l�                      ��                  q  u                  l�           q  ��  \                     ,\                     @\                         � ߱            $   r  �  ���                              v  ��  �          4   ����X\  x\                         � ߱            $   w  ��  ���                       �\                         � ߱        p�  $   {  �  ���                       h�     ~  ��  ��  �      4   �����\      $     ĵ  ���                       �\                         � ߱            �   �  �\      ]     
                �]                     �^  @        
 �^              � ߱        ��  V   �  �  ���                        ��  �   �  �^      ��     b  ��  ж          4   ����,_      /   c  ��     �                          3   ����<_            ,�                      3   ����\_  |_     
                �_                     Ha  @        
 a              � ߱        0�  V   o  <�  ���                        �a     
                b                     `c  @        
  c              � ߱        \�  V   �  ̷  ���                        ̹       t�  �          4   ����tc                ��                      ��                                      <�             ��  `�  /      �     0�                          3   �����c            P�                      3   �����c      /     ��     ��                          3   �����c            ��                      3   �����c  ��  /  �  ��         d                      3   �����c  initProps   (�  �              H     Y     @                          <  �$  	                                   @�          �  к      ��                 5  N   �              TO�        O   ����    e�          O   ����    R�          O   ����    ��      �$                      �          \�  p   @  |{  \�      K  ܻ  ̻     �{                                        ��                  A  ]                  �X�           A  l�  \�  L�     �{                                        ��                  ^  z                  �Y�           ^  �  ܼ  ̼     �{                                        ��                  {  �                  \Z�           {  l�  \�  L�     �{                                        ��                  �  �                  \S�           �  �  ܽ  ̽     �{                                        ��                  �  �                  $T�           �  l�  \�  L�     �{                                        ��                  �  �                  �T�           �  �  ܾ  ̾      |                                        ��                  �                    �U�           �  l�  \�  L�     |                                        ��                    (                  �V�             �  ܿ  ̿     (|  	                                      ��             	     )  E                  �[�           )  l�  \�  L�     <|  
                                      ��             
     F  b                  x\�           F  �  ��  ��     P|                                        ��                  c                    H]�           c  l�  \�  L�     d|                                        ��                  �  �                  ^�           �  ��  ��  ��     x|                                        ��                  �  �                  t_�           �  l�  \�  L�     �|                                        ��                  �  �                  �_�           �  ��  ��  ��     �|                                        ��                  �  �                  �`�           �  l�  \�  L�     �|                                        ��                  �                    �a�           �  ��  ��  ��     �|                                        ��                    -                  `b�             l�      L�     �|                                        ��                  .  J                  |c�           .  ��      O   M  ��  ��  �|               ��          ��  ��   , ��                                                       �     ��                            ����                            �  4�  $�  p�      t�     Z     ��                      � ��  �$                     0�     c  ��  �          4   �����|                �                      ��                  d  x                  �d�           d  ��  ��  /   e  H�     X�                          3   ����}            x�                      3   ����,}  ��  /   f  ��     ��                          3   ����D}            ��                      3   ����d}  `�  /   k   �     0�                          3   �����}            P�                      3   �����}      /   q  ��     ��                          3   �����}            ��                      3   �����}  �~     
                0                     ��  @        
 @�              � ߱        \�  V   �  ��  ���                        �  $   �  ��  ���                       ��                         � ߱        ��     
                ,�                     |�  @        
 <�              � ߱        D�  V   �  ��  ���                         �  $     p�  ���                       ��     
                    � ߱        ��     
                �                     h�  @        
 (�              � ߱        ,�  V     ��  ���                        ��  $   7  X�  ���                       t�     
                    � ߱        ��     
                �                     T�  @        
 �              � ߱        �  V   A  ��  ���                        ��  $   [  @�  ���                       l�                         � ߱        ��     
                �                     `�  @        
  �              � ߱        ��  V   e  l�  ���                        �  �     x�      ��  $   �  <�  ���                       ��     
                    � ߱        ��     
                (�                     x�  @        
 8�              � ߱        ��  V   �  h�  ���                        P�  $   �  $�  ���                       ��     
                    � ߱        d�  �   �  ��      ��  $   �  ��  ���                       ؊     
                    � ߱        ��  �   �  �      (�  $     ��  ���                       ,�                         � ߱                 @�  P�          4   ����H�      /     |�     ��                          3   ����h�  ��     
   ��                      3   ������  ��        ��                      3   ������  �        �                      3   ������            <�                      3   ������  pushRowObjUpdTable  (�  L�  �                   [      �                               b&                     pushTableAndValidate    `�  ��  �           p     \     �                          �  &                     remoteCommit    ��  0�  �           d     ]     �                          �  �&                     serverCommit    @�  ��  �           `     ^     �                          �  �&                                     ��          ��  h�      ��                  3  @  ��              0Β        O   ����    e�          O   ����    R�          O   ����    ��          O   >  ��  ��  ��    ��                            ����                            ��   �      �              _      ��                      
�     �&                     disable_UI  �  d�                      `      �                               �&  
                    �  �    ����  �       ��       �  8   ����    �  8   ����   0�  8   ����   @�  8   ����       8   ����       8   ����       `�  l�      viewObject  ,   P�  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    p�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  �   �      returnFocus ,INPUT hTarget HANDLE   �  H�  \�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    8�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��  �      removeAllLinks  ,   ��  �  ,�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    t�  �  �      hideObject  ,    �  0�  <�      exitObject  ,    �  P�  h�      editInstanceProperties  ,   @�  |�  ��      displayLinks    ,   l�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��   �  �      applyEntry  ,INPUT pcField CHARACTER    ��  8�  H�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER (�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  `�  p�      unbindServer    ,INPUT pcMode CHARACTER P�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      disconnectObject    ,   ��  ��  �      destroyObject   ,   ��   �  ,�      bindServer  ,   �  @�  P�      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  0�  ��  ��      startFilter ,   ��  ��  ��      releaseDBRow    ,   ��  ��  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ��  �  ,�      filterContainerHandler  ,INPUT phFilterContainer HANDLE �  \�  p�      fetchDBRowForUpdate ,   L�  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL t�  ��  ��      compareDBRow    ,   ��  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   ��  p�  |�      assignDBRow ,INPUT phRowObjUpd HANDLE   `�  ��  ��      updateState ,INPUT pcState CHARACTER    ��  ��  ��      updateQueryPosition ,   ��  �  �      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��  `�  p�      undoTransaction ,   P�  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  t�  �  ,�      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   �  ��  ��      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   |�  ��   �      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  ��  t�  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  d�  ��  ��      startServerObject   ,   ��  ��   �      setPropertyList ,INPUT pcProperties CHARACTER   ��  0�  L�      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd     �  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    p�  H�  `�      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER 8�  ��  ��      rowObjectState  ,INPUT pcState CHARACTER    |�  ��  ��      retrieveFilter  ,   ��  ��   �      restartServerObject ,   ��  �  $�      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   �  �  (�      refreshRow  ,   �  <�  L�      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ,�  ��  ��      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  ��  ��  �      initializeServerObject  ,   ��  �  0�      initializeObject    ,   �  D�  L�      home    ,   4�  `�  p�      genContextList  ,OUTPUT pcContext CHARACTER P�  ��  ��      fetchPrev   ,   ��  ��  ��      fetchNext   ,   ��  ��  ��      fetchLast   ,   ��  ��  �      fetchFirst  ,   ��  �  (�      fetchBatch  ,INPUT plForwards LOGICAL   �  T�  l�      endClientDataRequest    ,   D�  ��  ��      destroyServerObject ,   p�  ��  ��      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  �  �      dataAvailable   ,INPUT pcRelative CHARACTER ��  @�  L�      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE 0�  ��  ��      commitTransaction   ,   ��  ��  ��      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    ��  d�  t�      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 o%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %              %              %              %              %              %              %              %              %              %              %              %              %              %              %              %              %       	       %              %              %              %              %              %              %              %              %               %              %              %              %              %              %              %               �     }        �� �  .   %               � 
" 	   
 v%              h �P  \         (          
�                          
�            � �   �
" 	   
 o
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 p�           �    1�   
 p�    v%               o%   o           �     o
"   
 q�           ,    1�    q�    v%               o%   o           � ,   p
"   
 q�           �    1� 3  
 q�    v%               o%   o           � >   q
"   
 o�               1� N   o�    v%               o%   o           �     q
"   
 ~�           �    1� \   ~�    v%               o%   o           � k   o
"   
 o�           �    1� �   o� �   v%               o%   o           %               
"   
 v�          x    1� �   v� �     
"   
 q�           �    1� �   q�    v%               o%   o           � �  p
"   
 q�           (    1� �   q�    v%               o%   o           � �  S q
"   
 o�           �    1� #   o� �   v%               o%   o           %               
"   
 p�           	    1� 3   p� �   v%               o%   o           %               
"   
 o�           �	    1� E   o� �   v%               o%   o           %              
"   
 v�          
    1� R   v� �     
"   
 q�           L
    1� a  
 q� �   v%               o%   o           %               
"   
 q�           �
    1� l   q�    v%               o%   o           �     q
"   
 v�          <    1� t   v� �     
"   
 o�           x    1� �   o�    v%               o%   o           � �  t o
"   
 v�          �    1�   
 v� �     
"   
 p�           (    1�    p�    v%               o%   o           � +  � o
"   
 q�           �    1� �   q�    v%               o%   o           �     p
"   
 q�               1� �  
 q� �   v%               o%   o           %               
"   
 ~�           �    1� �   ~� �   v%               o%   o           %              
"   
 o�               1� �   o�    v%               o%   o           �     ~
"   
 o�           |    1� �   o�    v%               o%   o           o%   o           
"   
 p�           �    1�   
 p�    v%               o%   o           �     o
"   
 o�           l    1�    o� #  	 v%               o%   o           � -  / p
"   
 v�          �    1� ]   v� #  	   
"   
 q�               1� o   q� #  	 vo%   o           o%   o           �     q
"   
 v�          �    1� �   v� #  	   
"   
 o�           �    1� �   o� #  	 vo%   o           o%   o           �     o
"   
 v�          @    1� �   v� �     
"   
 v�          |    1� �   v� #  	   
"   
 v�          �    1� �   v� #  	   
"   
 v�          �    1� �   v� #  	   
"   
 p�           0    1� �   p� �   vo%   o           o%   o           %              
"   
 v�          �    1� �   v� #  	   
"   
 v�          �    1� �  
 v�      
"   
 v�          $    1� 	   v� #  	   
"   
 v�          `    1�    v� #  	   
"   
 v�          �    1� +   v� #  	   
"   
 v�          �    1� @   v� #  	   
"   
 v�              1� O  	 v� #  	   
"   
 v�          P    1� Y   v� #  	   
"   
 v�          �    1� l   v� #  	   
"   
 o�           �    1� �   o�    v%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 j
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�       �    �� 3  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 o�           p    1� �  
 o�    v%               o%   o           �     o
"   
 o�           �    1� �  
 o�    v%               o%   o           o%   o           
"   
 o�           `    1� �   o� �   v%               o%   o           o%   o           
"   
 o�           �    1� �   o� �   v%               o%   o           %               
"   
 p�           X    1� �   p� �   v%               o%   o           %               
"   
 p�           �    1� �   p�    v%               o%   o           �     p
"   
 q�           H    1� �   q� �   v%               o%   o           %              
"   
 q�           �    1�    q� �   v%               o%   o           o%   o           
"   
 o�           @    1�    o�    v%               o%   o           o%   o           
"   
 o�           �    1� *  	 o�    v%               o%   o           �     ~
"   
 o�           0    1� 4   o�    v%               o%   o           o%   o           
"   
 p�           �    1� H   p�    v%               o%   o           o%   o           
"   
 o�           (    1� W   o� �   v%               o%   o           %               
"   
 o�           �    1� g   o� �   v%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 o�           t    1� s  
 o� �   v%               o%   o           %              
"   
 o�           �    1� ~   o�    v%               o%   o           o%   o           
"   
 p�           l    1� �   p�    v%               o%   o           �     p
"   
 p�           �    1� �   p�    v%               o%   o           o%   o           
"   
 v�          \     1� �   v� �     
"   
 ~�           �     1� �   ~�    v%               o%   o           � �  ! q
"   
 o�           !    1� �   o�    v%               o%   o           �     ~
"   
 o�           �!    1� �   o�    v%               o%   o           �    o
"   
 v�          �!    1�    v� "     
"   
 v�          0"    1� (   v� �     
"   
 q�           l"    1� <   q�    v%               o%   o           �     q
"   
 v�          �"    1� H  
 v� �     
"   
 ~�           #    1� S   ~� �   v%               o%   o           o%   o           
"   
 p�           �#    1� a   p� �   v%               o%   o           %               
"   
 o�           $    1� n   o� �   v%               o%   o           %               
"   
 p�           �$    1�    p�    v%               o%   o           �     o
"   
 p�           %    1� �   p�    v%               o%   o           o%   o           
"   
 ~�           �%    1� �   ~� �   v%               o%   o           %              
"   
 o�           �%    1� �   o� �   v%               o%   o           %               
"   
 p�           x&    1� �   p� �   v%               o%   o           %               
"   
 v�          �&    1� �   v� �     
"   
 v�          0'    1� �   v�      
"   
 p�           l'    1� �   p� �   v%               o%   o           o%   o           
"   
 ~�           �'    1� �   ~�    v%               o%   o           �     o
"   
 ~�           \(    1� �   ~�    v%               o%   o           o%   o           
"   
 q�           �(    1�    q� �   vo%   o           o%   o           o%   o           
"   
 q�           T)    1�     q� #  	 v%               o%   o           o%   o           
"   
 q�           �)    1� 1   q�    v%               o%   o           o%   o           
"   
 o�           L*    1� >  
 o� �   v%               o%   o           o%   o           
"   
 v�          �*    1� I   v�      
"   
 p�           +    1� Z   p�    v%               o%   o           � q  4 o
"   
 o�           x+    1� �  
 o� �   v%               o%   o           %              
"   
 v�          �+    1� �   v� �     
"   
 ~�           0,    1� �   ~�    v%               o%   o           �     o
"   
 p�           �,    1� �   p� �   v%               o%   o           %              
"   
 o�            -    1� �   o�    v%               o%   o           �     p
"   
 q�           �-    1� �   q�    v%               o%   o           �     o
"   
 o�           .    1� �   o�    v%               o%   o           �     q
"   
 o�           |.    1�    o� �   v%               o%   o           %               
"   
 o�           �.    1�   	 o� �   v%               o%   o           o%   o           
"   
 p�           t/    1�    p�    v%               o%   o           � .  	 ~
"   
 o�           �/    1� 8   o� �   v%               o%   o           %       �       
"   
 q�           d0    1� D   q�    v%               o%   o           �     o
"   
 q�           �0    1� K   q� �   vo%   o           o%   o           %              
"   
 o�           T1    1� ]   o� �   v%               o%   o           %               
"   
 o�           �1    1� t   o�    v%               o%   o           o%   o           
"   
 p�           L2    1� �   p� #  	 v%               o%   o           �     ~
"   
 v�          �2    1� �   v� #  	   P �L 
�H T   %              �     }        �GG %              
"   
 q�           P3    1� �  
 q�    v%               o%   o           �     q
"   
 p�           �3    1� �   p� �   v%               o%   o           %               
"   
 o�           @4    1� �  	 o�    v%               o%   o           �     p
"   
 o�           �4    1� �   o�    v%               o%   o           �     o
"   
 ~�           (5    1� �   ~� �   v%               o%   o           %               
"   
 o�           �5    1� �   o�    v%               o%   o           �     ~
"   
 o�           6    1� �   o�    v%               o%   o           o%   o           
"   
 q�           �6    1� �   q�    v%               o%   o           o%   o           
"   
 p�           7    1�    p� �   v%               o%   o           o%   o           
"   
 o�           �7    1�    o� �   v%               o%   o           o%   o           
"   
 o�           8    1� )   o� �   v%               o%   o           o%   o           
"   
 o�           �8    1� :   o�    v%               o%   o           o%   o           
"   
 p�            9    1� I  	 p� #  	 v%               o%   o           �     q
"   
 ~�           t9    1� S  
 ~� #  	 v%               o%   o           �     p
"   
 o�           �9    1� ^   o�    v%               o%   o           �     ~
"   
 o�           \:    1� m   o�    v%               o%   o           o%   o           
"   
 q�           �:    1� {   q�    v%               o%   o           o%   o           
"   
 q�           T;    1� �   q�    v%               o%   o           �     o
"   
 p�           �;    1� �   p�    v%               o%   o           �     q
"   
 p�           <<    1� �   p� #  	 v%               o%   o           o%   o           
"   
 v�          �<    1� �   v� �     
"   
 q�           �<    1� �   q�    v%               o%   o           �     o
"   
 q�           h=    1� �   q�    v%               o%   o           o%   o           
"   
 o�           �=    1� �   o� �   v%               o%   o           o%   o           
"   
 ~�           `>    1� �  
 ~�    v%               o%   o           �     o
"   
 p�           �>    1�    p�    v%               o%   o           �     ~
"   
 o�           H?    1�     o� �   v%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 o�           @    1� 1  	 o� �   v%               o%   o           o%   o           
"   
 q�           �@    1� ;   q� �   v%               o%   o           o%   o           
"   
 o�           A    1� J   o� �   v%               o%   o           o%   o           
"   
 ~�           �A    1� Y   ~� �   v%               o%   o           %              
"   
 p�           B    1� m   p�    v%               o%   o           � �  M ~
"   
 p�           |B    1� �   p� �   v%               o%   o           %              
"   
 o�           �B    1� �   o� �   v%               o%   o           %               
"   
 o�           tC    1� �   o� �   v%               o%   o           %               
"   
 o�           �C    1�    o� #  	 v%               o%   o           �    o
"   
 ~�           dD    1� .   ~� �   v%               o%   o           %               
"   
 ~�           �D    1� =   ~� #  	 v%               o%   o           o%   o           
"   
 p�           \E    1� J   p� �   vo%   o           o%   o           %              
"   
 p�           �E    1� Z   p� #  	 vo%   o           o%   o           �     p
"   
 o�           LF    1� m   o� �   vo%   o           o%   o           o%   o           
"   
 o�           �F    1� }   o� �   vo%   o           o%   o           o%   o           
"   
 o�           DG    1� �   o� #  	 vo%   o           o%   o           o%   o           
"   
 o�           �G    1� �   o� �   vo%   o           o%   o           o%   o           
"   
 o�           <H    1� �   o� #  	 vo%   o           o%   o           � �   o
"   
 o�           �H    1� �   o� #  	 vo%   o           o%   o           � �   o
"   
 o�           $I    1� �   o� �   v%               o%   o           %               
"   
 q�           �I    1� �   q� �   v%               o%   o           %               
"   
 v�          J    1� �   v� #  	   
"   
 ~�           XJ    1�    ~� �   v%               o%   o           %               
"   
 ~�           �J    1�    ~�    v%               o%   o           o%   o           
"   
 o�           PK    1� 3   o�    v%               o%   o           o%   o           
"   
 q�           �K    1� G   q� �   v%               o%   o           o%   o           
"   
 q�           HL    1� Y   q�    v%               o%   o           �     p
"   
 p�           �L    1� h   p� v   v%               o%   o           %               
"   
 o�           8M    1� ~  	 o� �   v%               o%   o           %                "    v%     start-super-proc v%     adm2/smart.p Q�P �L 
�H T   %              �     }        �GG %              
"   
   �       TN    6� �     
"   
   
�        �N    8
"   
   �        �N    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      �P    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �Q    �� �   �p�               �L"  	  , �   � �   q� �   v�     }        �A      |    "  	    � �   p%              (<   \ (    |    �     }        �A� �   �A"  
  q    "  	  �"  
  q  < "  	  �"  
  q(    |    �     }        �A� �   �A"  
  q
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8      �S    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �T    ��   
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        dU    �� �   � P   �        pU    �@    
� @  , 
�       |U    �� �     p�               �L
�    %              � 8      �U    � $         � �          
�    � �     
"   
 �p� @  , 
�       �V    �� 3  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�        W    �� N     p�               �L%               
"   
  p� @  , 
�       `W    �� �    p�               �L%               
"   
  p� @  , 
�       �W    �� o    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 o (   � 
"   
 �    �        �X    �� �   �
"   
   � 8      �X    � $         � �          
�    � �   �
"   
   �        DY    �
"   
   �       dY    /
"   
   
"   
   �       �Y    6� �     
"   
   
�        �Y    8
"   
   �        �Y    �
"   
   �       �Y    �
"   
   p�    � �   p
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �Z    �A"    �A
"   
   
�        [    �@ � 
"   
 o"      �       }        �
"   
 v%              %                "    v%     start-super-proc v%     adm2/appserver.p vp�    � b     
�    �     }        �%               %      Server  - �     }        �    "    o�     v%               %      Client      "    o�     v%      NONE    p�,  8         $     "    q        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        `]    �� �   � P   �        l]    �@    
� @  , 
�       x]    �� �   �p�               �L
�    %              � 8      �]    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �^    �� 4   �p�               �L"    , p�,  8         $     "    q        � �   �
�     "    v%     start-super-proc v%     adm2/dataquery.p 5p
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
 �(�  L ( l       �        �_    �� �   � P   �        �_    �@    
� @  , 
�       �_    �� �   �p�               �L
�    %              � 8      �_    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       �`    ��    �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
 �(�  L ( l       �        �a    �� �   � P   �        �a    �@    
� @  , 
�       �a    �� �   �p�               �L
�    %              � 8      b    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       c    �� �   �p�               �L%               "    v%     start-super-proc v%     adm2/query.p P�%     start-super-proc v%     adm2/queryext.p % 	    initProps �
�    %� �   FOR EACH tCreditos NO-LOCK,       EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK,       EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK,       EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION �   � �      � �      � �   %       "    q� �     v
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �e    �� �   � P   �        �e    �@    
� @  , 
�       �e    �� �   �p�               �L
�    %              � 8      �e    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �f    �� �  	 �p�               �L"    , %,#  rowObject.NombreAgencia = Agencias.Nombre  rowObject.NombreCiudad = Ubicacion.Nombre  rowObject.TpoCliente = tCreditos.Cod_Credito  rowObject.OtroTpoCliente = tCreditos.Observaciones  rowObject.CptcionClccion = tCreditos.Categoria  rowObject.PrdctoSlctar = tCreditos.Per_Pago  rowObject.OtroPrdctoSlctar = tCreditos.Pagare  rowObject.Grntia = tCreditos.Cue_Desembolso  rowObject.Linea = tCreditos.Cue_DebAutomatico  rowObject.reestrctrcion = tCreditos.Abogado  rowObject.FrmaPgo = tCreditos.CategoriaMes  rowObject.dstncion = tCreditos.Nit_Juzgado �    "      � �          %              %                   "      %                  "      "      T(        "    o%              "    o� �    v"      �       "    ��    "    o� �   v�       � �   ��    "     � �    S    "      "    v    "    o%                � @    �     t T     P   4       v"      (0       4       �"      �       �     �� �    �T ,  %              T   "    �"    v� �      � �   �� �    �T    �    "    �� �   v"      � �   �"      %                   %              %                   "      %                  "      �     "       \      H   "      ((       "    �%              �     v� !#     4  p     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �m    �� �   � P   �        �m    �@    
� @  , 
�       �m    �� �   �p�               �L
�    %              � 8      �m    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �n    �� �  
 �p�               �L"    ,       "  
  p�    � ##  � o� �    v      "  	    �    � ##  � v� �    o�   � �      � �      � ##  � ��   � �      � �    �� ##  � o      "  
  ~�    �     o� �    v      "  	    �    � �#   v� �    o   ,        "    �� !#   o�   � �    �� �    o�     v   ,        "      � !#     �   � �    o� �    v� �#   ~      "  
  o�    �     o� �    v      "  	    �    � �#   v� �    o   ,        "    �� !#   o�   � �    �� �    o�     v   ,        "      � !#     �   � �    o� �    v� �#   o      "  
  o�    �     p� �    v      "  	    �    � �#   v� �    p   ,        "    �� !#   p�   � �    �� �    p�     v   ,        "      � !#     �   � �    p� �    v� �#   o�   � �      � �      � �#  �   
�H T   %              �     }        �GG %              
"   
 v
"   
 �
"   
 v
"   
 v(�  L ( l       �        �s    �� �   � P   �        �s    �@    
� @  , 
�       �s    �� �   vp�               �L
�    %              � 8      �s    � $         � �          
�    � �     
"   
 �p� @  , 
�       �t    �� <   �p�               �L"    , 
"   
   p� @  , 
�       8u    ��      p�               �L"    , 
"   
  p� @  , 
�       �u    �� �    p�               �L"    ,     %              %                   "      %                  "      �     "      4 (        "  
    �    � ##  �   � �          "  	  p�     "    qT    "      "      @ A,    �   � �    v� !#     "    �"       T      @   "    v(        "      �     ��       � �    �"    p     "  	   %              D H   @ A,    �   � �    �� !#     "    �"    ~,    S   "    �� ##  � ~� �    v%                T      @   "    v(        "      �     ��       � �    �"    o     "  
   %                         "    v� !#     "    �           "      � !#   �"      
�H T   %              �     }        �GG %              
"   
 �
"   
   
"   
 �
"   
 �(�  L ( l       �        �y    �� �   � P   �        �y    �@    
� @  , 
�       �y    �� �   �p�               �L
�    %              � 8      �y    � $         � �   �     
�    � �   v
"   
 �p� @  , 
�       �z    ��    �p�               �L"    , 
"   
   p� @  , 
�       ${    �� �     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    v%     start-super-proc v%     adm2/data.p %     start-super-proc v%     adm2/dataext.p %     start-super-proc v%     adm2/dataextcols.p %     start-super-proc v%     adm2/dataextapi.p p%              %              %              %              %              %              %              %              %       	       
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
 �(�  L ( l       �             �� �   � P   �            �@    
� @  , 
�           �� �   �p�               �L
�    %              � 8      $    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       4�    ��    �p�               �L%               %     "dsolprofina.i" 
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8       �    � $         � �          
�    � �   �
"   
 �p� @  , 
�       0�    ��    �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�        �    �� �   �p�               �L
�    %              � 8      �    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �    �� H  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        Ԅ    �� �   � P   �        ��    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      ��    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �    ��   	 �p�               �L
"   
 , 
"   
 v     � &  	   �        `�    �
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        �    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      �    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �    ��    �p�               �L"    , 
"   
   �       l�    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 v
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      �    � $         � �          
�    � �   �
"   
 �p� @  , 
�       ,�    �� 1  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 o        � &   �
�    
�             �Gp�,  8         $     
"   
 o        � '&   �
�    �    � 9&     
�        "    o�     v%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �&     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           x   `       ��                 �  �  �               |�        O   ����    e�          O   ����    R�          O   ����    ��         $   �  �   ���                       TX     
                    � ߱               �    �          4   �����X                �                      ��                  �  �                  ��           �  (  �  �  �  �X             �  �  4          4   ����PY                D                      ��                  �  �                  X�           �  �  x  o   �      ,                                 �  �   �  pY      �  �   �  �Y      �  $   �  �  ���                       �Y     
                    � ߱          �   �  �Y         �   �  Z      4  �   �  (Z          $   �  `  ���                       XZ  @         DZ              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 �  &  �               ��        O   ����    e�          O   ����    R�          O   ����    ��                            �          �  $   �  �   ���                       �Z     
                    � ߱                  �  �                      ��                   �  �                  "�          �  (      4   �����Z      $   �  �  ���                       [     
                    � ߱        d     �    (          4   ����,[      /  �  T                               3   ����@[  x  �     L[          O   $  ��  ��  �[               �          �  �   , �                          
                               �      ��                            ����                                            �           x   `       ��                 �  (  �               P	�        O   ����    e�          O   ����    R�          O   ����    ��         $   �  �   ���                       d                         � ߱        X  $   �  ,  ���                       (e                         � ߱               p  �          4   ����Te  te     
                �e                     @g  @        
  g              � ߱            V     �  ���                        h  $   4  <  ���                       Lg                         � ߱           $   5  �  ���                       |i                         � ߱          0      �  �                      ��        0          7  M                  D>�    8     7  �      $   7  \  ���                       �i                         � ߱        �  $   7  �  ���                       �i                         � ߱            4   �����i  j                     \j                     hj                     �j                     �j                         � ߱        �  $   8  �  ���                              E  �  �          4   �����j      $   F    ���                        k          Ll             � ߱        �  $   P  d  ���                       Xl                         � ߱                 X  �                      ��        0          R  W                  �B�    |     R  �      $   R  ,  ���                       ll                         � ߱        �  $   R  �  ���                       �l                         � ߱            4   �����l      $   T  �  ���                       �l                         � ߱        lm     
                �m                     8o  @        
 �n              � ߱        (  V   b    ���                        Do       
       
       xo       	       	       �o                     �o                         � ߱        �  $   �  �  ���                       p       
       
       8p       	       	       lp                     �p                         � ߱        �	  $   �  T  ���                       q       
       
       Hq       	       	       |q                     �q                         � ߱        ,
  $   �   	  ���                       $r       
       
       Xr       	       	       �r                     �r                         � ߱        X
  $     �	  ���                       L  $   M  �
  ���                       4s                         � ߱        `s     
                �s                     ,u  @        
 �t          �u  @        
 Du          �u  @        
 �u              � ߱        �  V   Y  �
  ���                          �      @  �                      ��        0          �  �                  �L�    p     �  x      $   �    ���                       �u                         � ߱        �  $   �  l  ���                       v                         � ߱        �  4   ����@v      4   ����hv    $   �  �  ���                       �v                         � ߱             �  (  �          4   �����v                �                      ��                  �  �                  M�           �  8  0w                     �w       	       	           � ߱            $   �  �  ���                              �  4  �          4   �����w                �                      ��                  �  �                  �M�           �  D  Tx                     �x       
       
           � ߱            $   �  �  ���                       �x                     y                         � ߱          $   �  (  ���                       Ly     
                �y                     {  @        
 �z          p{  @        
 0{              � ߱            V   �  �  ���                                    7           �  �  � |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        x   `       ��                  �  �  �               $��        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           x   `       ��                  �  �  �               ���        O   ����    e�          O   ����    R�          O   ����    ��      u&       �              �                  $                  X  /  �       (  ��                      3   ����܋            H                      3   ���� �      O   �  ��  ��  �               �          �  �    �                                             ��                            ����                                            <          x   `       ��                  �  �  �               ,��        O   ����    e�          O   ����    R�          O   ����    ��      �&       �              �          �       $                  �&                     �          �&                               �  /  �  h     x  4�                      3   �����            �                      3   ����<�  �  /  �  �     �  d�                      3   ����H�  l                            3   ����l�      $   �  @  ���                                                   � ߱                  �  �                  3   ����x�      $   �  �  ���                                                   � ߱        L  $   �     ���                       ��                         � ߱            O   �  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                      x   `       ��                    &  �               DȒ        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �&       �              �          �&                      �              /  #  @     P  Ќ                      3   ������  �        p  �                  3   ����،      $   #  �  ���                                                   � ߱                  �                    3   �����      $   #  4  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           x   `       ��                  �  �  �               �ϒ        O   ����    e�          O   ����    R�          O   ����    ��             �  �   �           4   �����      �   �  �    ��                            ����                            TXS appSrvUtils tCreditos Creditos Agencia Nit Cod_Credito Num_Credito Tip_Credito Pagare Tasa Fec_Aprobacion Fec_Desembolso Fec_Pago Fec_UltPago Fec_Calificacion Fec_Reestructurado Fec_PagAnti Fec_DifCobro Fec_CanceTotal Fec_UltLiquidacion Fec_ProxLiquidacion For_Pago For_Interes Per_Pago Plazo Usuario Cuota Monto Sdo_Capital Int_Corrientes Int_Anticipado Int_MorCobrar Int_DifCobro Sdo_CapPag Sdo_IntPag Sdo_IntMor Sdo_Proyectado Cuo_Pagadas Cuo_Atraso Val_Atraso Dias_Atraso Provision Cod_Califica Poliza Deducible Observaciones Incremento Destino Sistema Estado Detalle_Estado Num_Solicitud Per_Gracia Id_Adicionales Reestructurado Int_AntDesembolso Age_Desembolso Cod_Desembolso Cue_Desembolso Age_DebAutomatico Cue_DebAutomatico Cod_DebAutomatico Desembolso Costas Nit_Juzgado Nom_Juzgado Abogado Honorarios Polizas Categoria Cta_Contable Sdo_Anuales Capital_Acum Int_LiqAcum Int_MoraDifCob Cod_CalificaMes Provision_Interes Provision_Otros CategoriaMes Val_Desembolso Fec_Bloqueo Usuario_gestor Tasa_Desembolso Seg_Cartera Com_Bruta Com_Adicional GmfxC TarjetaDB Fec_BloqueoTdb Fec_CreaTdb Fec_CancTdb diapago cod_calificareest cod_calant cod_calact respaldoaportes Comision seg_vida Num_Control Tasa_Ant .\dsolprofina.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "dsolprofina.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH tCreditos NO-LOCK,       EACH Agencias WHERE Agencias.Agencia = tCreditos.Agencia NO-LOCK,       EACH Ubicacion WHERE Ubicacion.Ubicacion = Agencias.Ciudad NO-LOCK,       EACH Clientes WHERE Clientes.Nit = tCreditos.Nit NO-LOCK INDEXED-REPOSITION ,   tCreditos Agencias Ubicacion Clientes  rowObject.NombreAgencia = Agencias.Nombre  rowObject.NombreCiudad = Ubicacion.Nombre  rowObject.TpoCliente = tCreditos.Cod_Credito  rowObject.OtroTpoCliente = tCreditos.Observaciones  rowObject.CptcionClccion = tCreditos.Categoria  rowObject.PrdctoSlctar = tCreditos.Per_Pago  rowObject.OtroPrdctoSlctar = tCreditos.Pagare  rowObject.Grntia = tCreditos.Cue_Desembolso  rowObject.Linea = tCreditos.Cue_DebAutomatico  rowObject.reestrctrcion = tCreditos.Abogado  rowObject.FrmaPgo = tCreditos.CategoriaMes  rowObject.dstncion = tCreditos.Nit_Juzgado ; Agencia TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo Grntia Linea reestrctrcion FrmaPgo dstncion Cuota NombreAgencia Ciudad NombreCiudad Fec_UltActualiza Agencia NombreAgencia Ciudad NombreCiudad Fec_UltActualiza TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Monto Plazo Grntia Linea reestrctrcion FrmaPgo dstncion Cuota INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p NombreAgencia Ciudad TpoCliente OtroTpoCliente CptcionClccion PrdctoSlctar OtroPrdctoSlctar Grntia Linea reestrctrcion FrmaPgo dstncion RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI Icr_nits IdAgeEst Idxcre idxcre1 IdxNitCodNum idxrec Idx_Creditos ITipAge skNumCrdto XIE3Creditos XIE4Creditos qDataQuery �   :  �  �G      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   �	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �    $  &           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  $   Y   �          �                  initProps   �  �      4  5  7  8  E  F  M  P  R  T  W  b  �  �  �    M  Y  �  �  �  �  �  �  �  �  �  �  �  �  �  (            l     lRet              �        piTableIndex    �  �  (   Z   X  t      �                  deleteRecordStatic  @  A  ]  ^  z  {  �  �  �  �  �  �  �  �      (  )  E  F  b  c    �  �  �  �  �  �  �  �  �      -  .  J  K  M  N                 !       �  �     [       �      �                  pushRowObjUpdTable  �          �        pcValType                  $       �  d     \       �      L                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     4     ]       p      $                  remoteCommit    �  �  �  �  �  `             $       �        x        pcMessages            �        pcUndoIds   �  �     ^       H      �                  serverCommit    #  &  �  4     _                                  getRowObjUpdStatic  >  @  �  x     `               l                  disable_UI  �  �  <  d+       &      +                      d!  �  �  a   tCreditos   X         `         d         p         |         �         �         �         �         �         �         �         �         �                           $         8         L         X         d         p         x         �         �         �         �         �         �         �         �         �         �                                     (         4         @         L         \         d         p         �         �         �         �         �         �         �         �         �         �                           $         4         H         \         p         |         �         �         �         �         �         �         �         �        �         �         �                             0          @          P          `          l          |          �          �          �          �          �          �          �          �          �          �          !         !         $!         4!         @!         L!         X!         Agencia Nit Cod_Credito Num_Credito Tip_Credito Pagare  Tasa    Fec_Aprobacion  Fec_Desembolso  Fec_Pago    Fec_UltPago Fec_Calificacion    Fec_Reestructurado  Fec_PagAnti Fec_DifCobro    Fec_CanceTotal  Fec_UltLiquidacion  Fec_ProxLiquidacion For_Pago    For_Interes Per_Pago    Plazo   Usuario Cuota   Monto   Sdo_Capital Int_Corrientes  Int_Anticipado  Int_MorCobrar   Int_DifCobro    Sdo_CapPag  Sdo_IntPag  Sdo_IntMor  Sdo_Proyectado  Cuo_Pagadas Cuo_Atraso  Val_Atraso  Dias_Atraso Provision   Cod_Califica    Poliza  Deducible   Observaciones   Incremento  Destino Sistema Estado  Detalle_Estado  Num_Solicitud   Per_Gracia  Id_Adicionales  Reestructurado  Int_AntDesembolso   Age_Desembolso  Cod_Desembolso  Cue_Desembolso  Age_DebAutomatico   Cue_DebAutomatico   Cod_DebAutomatico   Desembolso  Costas  Nit_Juzgado Nom_Juzgado Abogado Honorarios  Polizas Categoria   Cta_Contable    Sdo_Anuales Capital_Acum    Int_LiqAcum Int_MoraDifCob  Cod_CalificaMes Provision_Interes   Provision_Otros CategoriaMes    Val_Desembolso  Fec_Bloqueo Usuario_gestor  Tasa_Desembolso Seg_Cartera Com_Bruta   Com_Adicional   GmfxC   TarjetaDB   Fec_BloqueoTdb  Fec_CreaTdb Fec_CancTdb diapago cod_calificareest   cod_calant  cod_calact  respaldoaportes Comision    seg_vida    Num_Control Tasa_Ant    �#  t!  �!     RowObject   �"         �"         �"         �"         �"         �"         �"         �"         #         #         (#         0#         8#         @#         H#         X#         `#         l#         t#         |#         �#         �#         �#         Agencia NombreAgencia   Ciudad  NombreCiudad    Fec_UltActualiza    TpoCliente  OtroTpoCliente  CptcionClccion  PrdctoSlctar    OtroPrdctoSlctar    Monto   Plazo   Grntia  Linea   reestrctrcion   FrmaPgo dstncion    Cuota   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �#  �#     RowObjUpd   �$         �$         �$         %         %         (%         4%         D%         T%         d%         x%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         Agencia NombreAgencia   Ciudad  NombreCiudad    Fec_UltActualiza    TpoCliente  OtroTpoCliente  CptcionClccion  PrdctoSlctar    OtroPrdctoSlctar    Monto   Plazo   Grntia  Linea   reestrctrcion   FrmaPgo dstncion    Cuota   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   (&          &  
   appSrvUtils P&       <&     xiRocketIndexLimit  x&        d&  
   gshAstraAppserver   �&        �&  
   gshSessionManager   �&        �&  
   gshRIManager    �&        �&  
   gshSecurityManager  '         '  
   gshProfileManager   @'  	 	     ('  
   gshRepositoryManager    l'  
 
     T'  
   gshTranslationManager   �'        �'  
   gshWebManager   �'        �'     gscSessionId    �'        �'     gsdSessionObj   �'        �'  
   gshFinManager    (        (  
   gshGenManager   D(        4(  
   gshAgnManager   h(        X(     gsdTempUniqueID �(        |(     gsdUserObj  �(        �(     gsdRenderTypeObj    �(        �(     gsdSessionScopeObj  �(       �(  
   ghProp  )       )  
   ghADMProps  8)       ()  
   ghADMPropsBuf   `)       L)     glADMLoadFromRepos  |)       t)     glADMOk �)       �)  
   ghContainer �)    	   �)     cObjectName �)    
   �)     iStart  �)       �)     cAppService *       *     cASDivision D*       ,*     cServerOperatingMode    h*       X*     cContainerType  �*       |*     cQueryString    �*       �*  
   hRowObject  �*       �*  
   hDataQuery  �*       �*     cColumns              +     cDataFieldDefs  ,+    \   +  tCreditos   H+    X  <+  RowObject         X  X+  RowObjUpd          "   E   M   O   P   �   �   �   �   ]  ^  _  `  w  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  T	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  P
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
  �
  �
  �
  �
    -  .  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  @  A  B  C  D  E  F  G  H  I  J  K  L  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  l  !  "  +  ,  0  1  2  4  7  A  ]  o  �  �  �  -  E  F  `  p  q  r  u  v  w  {  ~    �  �  �  b  c  o  �            �  c  d  e  f  k  q  x  �  �  �      7  A  [  e    �  �  �  �  �  �            ��  .\dsolprofina.w  �/  ��  C:\Progress\OpenEdge\src\adm2\data.i �/  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �/  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i (0  7S , .\dsolprofina.i  \0  �   C:\Progress\OpenEdge\src\adm2\query.i    x0  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �0  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �0   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   1  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    T1  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �1  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    �1  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   2  I� # C:\Progress\OpenEdge\src\adm2\smart.i    L2  Ds & C:\Progress\OpenEdge\gui\fn  �2  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �2  Q. $ C:\Progress\OpenEdge\gui\set �2  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i 3  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    D3  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �3  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �3  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i  4  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i @4   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �4  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �4  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   5  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i L5  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �5  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �5  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i 6  �j  C:\Progress\OpenEdge\gui\get <6  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    d6  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �6  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �6  Su  C:\Progress\OpenEdge\src\adm2\globals.i   7  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i T7  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �7  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �7  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   8  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    T8  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �8  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  �8  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   9  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i P9  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �9  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �9  8�    D:\SPS\soportes\fodun\Prog\dsolprofina_cl.w         �      8:  �   H     H:  [  �     X:     �  &   h:  �   n     x:       .   �:  �        �:     �     �:  �   �     �:     �  $   �:  �   �     �:     �  $   �:  �   �     �:     �  $   ;  �        ;     ]  $   (;  �   [     8;     9  $   H;  �   6     X;       $   h;  �        x;     �  $   �;  �   �     �;     �  $   �;  �   �     �;     �  -   �;  �   �     �;     �  ,   �;  k   V     �;  �  J      <     0  +   <  �  -      (<       +   8<  �        H<     �  +   X<  �  �      h<     �  +   x<  �  �      �<     �  +   �<  �  �      �<     �  +   �<  �  �      �<     �  +   �<  �        �<     e  +   �<  �  b      =     H  +   =  �  E      (=     +  +   8=  �  (      H=       +   X=  �        h=     �  +   x=  �  �      �=     �  +   �=  �  �      �=     �  +   �=  �  �      �=     �  +   �=  �  �      �=     }  +   �=  �  z      >     `  +   >  �  ]      (>     C  +   8>  �  #      H>       $   X>  �         h>     �  $   x>  j  �      �>     �  $   �>  i  �      �>     t  $   �>  h  s      �>     Q  $   �>  ^  G      �>     !  *   �>  ]         ?     �  *   ?  \  �      (?     �  *   8?  [  �      H?     �  *   X?  Z  �      h?     �  *   x?  Y  �      �?     ^  *   �?  X  ]      �?     7  *   �?  W  6      �?       *   �?  V        �?     �  *   �?  U  �      @     �  *   @  T  �      (@     �  *   8@  S  �      H@     t  *   X@  R  s      h@     M  *   x@  Q  L      �@     &  *   �@  P  %      �@     �  *   �@  O  �      �@     �  *   �@  N  �      �@     �  *   �@  M  �      A     �  *   A  ?  |      (A     Z  $   8A    )      HA       $   XA  �   |      hA     #  )   xA  g         �A  a   �  !   �A     �  (   �A  _   �  !   �A     �  $   �A  ]   �  !   �A     g  $   �A  I   S  !   �A  �   J  "   B     �  '   B  �   �  "   (B     �  $   8B  �   �  "   HB     �  $   XB  �   �  "   hB     �  $   xB  g   j  "   �B     K     �B  O   3  "   �B  �   �  #   �B     �  &   �B  �   �  #   �B     3  %   �B  �   (  #   �B       $   C  �     #   C     �  $   (C  �   �  #   8C     �  $   HC  �   �  #   XC     �  $   hC  �   �  #   xC     g  $   �C  }   [  #   �C     9  $   �C     �  #   �C     o  "   �C     '  !   �C     �      �C     u     �C  �   l     D  O   ^     D     M     (D     �     8D  �   �     HD  �   �     XD  O   �     hD     �     xD     P     �D  y   (     �D  �     
   �D  G   	     �D     �
     �D     �
     �D  c   X
  
   �D  x   P
     �D  M   ;
     E     *
     E     �	     (E  a   �	     8E  �  �	     HE     �	     XE  �  T	     hE  O   F	     xE     5	     �E     �     �E  �        �E     �     �E     8     �E  x   2     �E          �E     �     �E     �     F     �     F     q     (F  Q   a     8F          HF     �     XF     �     hF     �     xF  ]   �  
   �F     �     �F     I  
   �F     ;     �F     '  
   �F  Z        �F     4  	   �F     �     �F     �     G     �     G  c   �     (G     �     8G     ;     HG     '     XG          hG     �      xG     &      �G           �G           