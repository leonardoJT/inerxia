	��V�YM|;   �                                              [ 3B7C00EFutf-8 MAIN U:\Prog\dAct_Fijo_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,Agencia integer 0 0,Anos_Adepreciar integer 1 0,Cen_Costos integer 2 0,Codigo character 3 0,Cos_Historico decimal 4 0,Descripcion character 5 0,Estado integer 6 0,Fec_Avaluo date 7 0,Fec_Compra date 8 0,Fec_Contrato date 9 0,Fec_debaja date 10 0,Fec_Garantia date 11 0,Fec_IniDepre date 12 0,Fec_Retiro date 13 0,Fec_Venta date 14 0,Grupo integer 15 0,Mejoras decimal 16 0,Neto decimal 17 0,Nit_Proveedor character 18 0,Nit_Responsable character 19 0,Fec_Asignacion date 20 0,Nit_Seguro character 21 0,Nombre character 22 0,Nro_Factura character 23 0,Nro_Seguro character 24 0,Ord_Compra character 25 0,Per_Depreciado integer 26 0,Sdo_Depre decimal 27 0,ValDepAcum decimal 28 0,ValDepMes decimal 29 0,Val_Compra decimal 30 0,Val_Garantia decimal 31 0,Vto_Seguro date 32 0,Val_Avaluo decimal 33 0,Val_Comercial decimal 34 0,Val_Provision decimal 35 0,Val_Valorizacion decimal 36 0,Sdo_Provision decimal 37 0,Id_Prestamo logical 38 0,Apellido1Prov character 39 0,Apellido2Prov character 40 0,NitProv character 41 0,Nombre-2Prov character 42 0,Apellido-2Ase character 43 0,Apellido-3Ase character 44 0,Nit-2Ase character 45 0,Nombre-3Ase character 46 0,Apellido-4Res character 47 0,Apellido-5Res character 48 0,Nit-3Res character 49 0,Nombre-4Res character 50 0,RowNum integer 51 0,RowIdent character 52 0,RowMod character 53 0,RowIdentIdx character 54 0,RowUserProp character 55 0,ChangedFields character 56 0      8W              L=             � 8W  �              <�              XD     +   �� t  W   �� D  X   @� �  Y   ��   [   ��   \   �� 0  ]    �   ^   �    `   ? 4� �'  ISO8859-1                                                                           �V    �                                      �                   ��                �V  �       *b   8�              ��  �    W      W                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �  �        
    
                    �             �                                                                                          �          
  L        �  
    
                  �  |             8                                                                                                    
  �        t  
    
                  `  (             �                                                                                                    
  �  *         
    
                    �             �                                                                                          *          
  P  ?      �  
    
                  �  �  	           <                                                                                          ?          
  �  U      x  
    
                  d  ,  
           �                                                                                          U          
  �  c      $                           �             �                                                                                          c            T  p      �                        �  �             @                                                                                          p             	  ~      |  
    
                  h  0	             �                                                                                          ~          
  �	  �      (	  
    
                  	  �	             �	                                                                                          �          
  X
  �      �	  
    
                  �	  �
             D
                                                                                          �          
    �      �
                        l
  4             �
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       e  X  �1  T   �1  e  .      02  8       e             �          l      �              �       �  X  U  U   \U  �  �      �U  9       �         �    3          �7      �                 T�                                               X�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                                 8  @  D  T  L          X             �  �  �  �  �          �             �  �  �                             <  D  L  d  \          h             �  �  �  �              �             �  �  �      �                        D   L   P   X               \              �   �   �   �   �           �              �   �   �   !  !           !             P!  `!  l!  �!  �!          �!             �!  �!  �!  �!  �!          �!             ("  8"  D"  T"              \"             �"  �"  �"  �"  �"          �"             #  #   #  0#              4#             `#  l#  x#  �#              �#             �#  �#  �#  �#  �#          �#              $  $  $  ,$              0$             d$  l$  �$  �$              �$             �$  �$  �$  �$  �$          �$             %  $%  ,%  8%              <%             x%  �%  �%  �%  �%          �%             �%  �%   &  &  &          &             P&  X&  `&  p&  h&          t&             �&  �&  �&  �&  �&          �&             �&  �&  �&   '  �&          '             4'  @'  H'  h'  X'          l'             �'  �'  �'  �'              �'             �'  �'   (  (  (          (             H(  T(  h(  �(              �(             �(  �(  �(  �(              �(             )  )  ()  @)  4)          D)             d)  t)  �)  �)  �)          �)             �)  �)  �)  �)               *             8*  D*  X*  p*  d*          t*             �*  �*  �*  �*  �*          �*             �*  +  +  4+  $+          8+             h+  |+  �+  �+  �+          �+             �+  �+  ,  ,,  ,          0,             `,  l,  t,  �,  �,          �,             �,  �,  �,  �,  �,           -             -  ,-  4-  d-  L-          h-             �-  �-  �-  �-  �-          �-             �-  �-  �-  .  �-          .              .  0.  8.  `.  L.          d.             �.  �.  �.  �.  �.          �.             �.  �.   /  /  /          /             8/  D/  L/  d/  X/          h/             |/  �/  �/  �/  �/          �/             �/  �/  �/  $0  0          (0             H0  T0  \0  l0  d0          p0             �0  �0  �0  �0  �0          �0             �0  �0  �0  �0                             �0  1  1  1                              1  $1  ,1  41                             81  D1  L1  X1                             \1  h1  p1  |1                                                                          Agencia 999 Oficina Oficina 0   C�digo de la oficina donde se crea el activo    Anos_Adepreciar 9999    Vida �til   Vida �til   0   Vida �til en a�os del activo fijo   Cen_Costos  999 Centro de Costos    0   Centro de costos donde se encuentra el activo fijo  Codigo  X(15)   C�digo Activo   C�digo      C�digo del activo fijo  Cos_Historico   >>>,>>>,>>>,>>9 Costo Hist�rico 0   Costo hist�rico del activo fijo Descripcion X(100)  Descripci�n Descripci�n     Descripci�n del activo fijo. Ejemplo: Modelo, Serie, Marca..    Estado  9   Estado  1   Estado del registro del activo fijo Fec_Avaluo  99/99/9999  Fecha de Avaluo Fec.Avaluo  ?   D�a, mes y a�o del aval�o del activo fijo   Fec_Compra  99/99/9999  Fecha de Compra Fec.de Compra   ?   D�a, mes y a�o en que se compra el activo fijo  Fec_Contrato    99/99/9999  Fecha de Contrato   Fec.de Contrato ?   Ingrese la Fecha del Contrato de Arriendo   Fec_debaja  99/99/9999  Fecha de Baja   Fecha de Baja   ?   D�a, mes y a�o de la baja del activo fijo   Fec_Garantia    99/99/9999  Fecha Garant�a  TODAY   D�a, mes y a�o de la garant�a del activo    Fec_IniDepre    99/99/9999  Depreciaci�n    Fec.Depreciaci�n    ?   D�a, mes y a�o de inicio de depreciaci�n del activo fijo    Fec_Retiro  99/99/9999  Fecha de Retiro ?   D�a, mes y a�o del retiro del activo fijo   Fec_Venta   99/99/9999  Fecha de Venta  ?   D�a, mes y a�o de la venta del activo fijo  Grupo   99999   Grupo   Grupo   1   Grupo al que pertenece el activo    Mejoras >>>,>>>,>>>,>>9 Mejoras y Adiciones 0   Valor del incremento del activo fijo por mejoras    Neto    ->>>,>>>,>>>,>>9    Neto    0   Representa el valor actual del activo   Nit_Proveedor   X(12)   Proveedor   Proveedor       Documento de identificaci�n del proveedor   Nit_Responsable X(12)   Responsable     N�mero de identificaci�n del responsable del activo fijo    Fec_Asignacion  99/99/9999  Fec. de Asignaci�n  Fec. Asignaci�n ?   D�a, mes y a�o de la asignaci�n del activo fijo Nit_Seguro  X(12)   Seguro  Seguro      N�mero de identificaci�n de la aseguradora del activo fijo  Nombre  X(40)   Nombre  Nombre      Nombre del activo fijo  Nro_Factura X(10)   Factura Factura     N�mero de la factura del activo fijo    Nro_Seguro  X(10)   P�liza  P�liza      N�mero de la p�liza del seguro del activo fijo  Ord_Compra  X(10)   Ord. de Compra  Orden de Compra     Orden de compra del activo fijo Per_Depreciado  9999    Periodos depreciados    0   Meses depreciados del activo fijo   Sdo_Depre   >>>,>>>,>>>,>>9 Sdo.Depre   Sdo.Depre   0   Ingrese el Saldo Depreciaci�n del Activo    ValDepAcum  ->>>,>>>,>>>,>>9    Vr.Depreciaci�n Acumulada   0   Valor de la depreciaci�n acumulada  ValDepMes   ->>>,>>>,>>>,>>9    Vr.Depreciaci�n Mensual 0   Valor de la depreciaci�n mensual    Val_Compra  >>>,>>>,>>>,>>9 Vr. Compra  Vr. Compra  0   Valor de compra del activo fijo Val_Garantia    >>>,>>>,>>>,>>9 Vr. Garant�a    Vr. Garant�a    0   Valor de la garant�a del activo fijo    Vto_Seguro  99/99/9999  Vencimiento Seguro  ?   D�a, mes y a�o del vencimiento de la p�liza de seguro   Val_Avaluo  >>>,>>>,>>>,>>9.99  Vr. Avaluo  Vr. Avaluo  0   Valor de aval�o del activo  Val_Comercial   >>>,>>>,>>>,>>9 Vr. Comercial   Vr. Comercial   0   Valor comercial del activo fijo Val_Provision   >>>,>>>,>>>,>>9 Valor Provision Valor Provision 0   Ingrese el Valor de Provisi�n del Activo Fijo   Val_Valorizacion    >>>,>>>,>>>,>>9 Val Valorizacion    val.Valorizacion    0   Ingrese el Valor de Valorizaci�n del Activo Fijo    Sdo_Provision   >>>,>>>,>>>,>>9 Sdo Provision   Sdo Provision   0   Ingrese el Saldo de Provisi�n del Activo Fijo   Id_Prestamo Si/No   Prestado    Prestado    No  Indique si el Activo esta Prestado  Apellido1Prov   X(15)   Primer Apellido Prov    Primer Apellido Prov        Primer apellido del cliente Apellido2Prov   X(15)   Segundo Apellido Prov   Segundo Apellido Prov       Segundo apellido del cliente    NitProv X(12)   Nit Prov    Nit Prov        N�mero documento de identificaci�n  Nombre-2Prov    X(40)   Nombre Prov Nombre Prov     Nombre del cliente  Apellido-2Ase   X(15)   Primer Apellido Ase Primer Apellido Ase     Primer apellido del cliente Apellido-3Ase   X(15)   Segundo Apellido Ase    Segundo Apellido Ase        Segundo apellido del cliente    Nit-2Ase    X(12)   Nit Ase Nit Ase     N�mero documento de identificaci�n  Nombre-3Ase X(40)   Nombre Ase  Nombre Ase      Nombre del cliente  Apellido-4Res   X(15)   Primer Apellido Res Primer Apellido Res     Primer apellido del cliente Apellido-5Res   X(15)   Segundo Apellido Res    Segundo Apellido Res        Segundo apellido del cliente    Nit-3Res    X(12)   Nit Res Nit Res     N�mero documento de identificaci�n  Nombre-4Res X(40)   Nombre Res  Nombre Res      Nombre del cliente  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �    1 A�  ���9������      ��������    �           �                           &        .&        5&                �     i     i     i    5 	7 	8 	    �#  �#  �#  �#  �#  $  $  $  "$  -$  :$  E$  R$  _$  j$  t$  z$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  %  %  %  !%  .%  9%  D%  R%  `%  q%  %  �%  �%  �%  �%  �%  �%  �%  �%  �%  �%  	&  &  &  %&  .&  5&  A&                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                                 �A  �A  �A  �A  �A          �A             �A  �A  �A  B  B          B             8B  DB  HB  \B              `B             �B  �B  �B  �B  �B          �B             �B  �B  �B  C              C             ,C  8C  @C  XC  LC          \C             �C  �C  �C  �C              �C             �C  �C  �C  D   D          D             <D  HD  TD  tD  dD          xD             �D  �D  �D  �D  �D          �D             E  $E  0E  PE  @E          TE             �E  �E  �E  �E              �E             �E  �E  �E   F  F          $F             `F  lF  xF  �F              �F             �F  �F  �F  �F              �F             G  G   G  0G  (G          4G             XG  `G  pG  �G              �G             �G  �G  �G  �G              �G             H  H  $H  <H  0H          @H             lH  |H  �H  �H              �H             �H  �H  �H  I   I          I             DI  PI  XI  hI  `I          lI             �I  �I  �I  �I  �I          �I             �I  �I  �I  J   J          J             4J  @J  HJ  XJ  PJ          \J             �J  �J  �J  �J  �J          �J             �J  �J  �J  K              K             <K  HK  XK  pK  dK          tK             �K  �K  �K  �K              �K             L  L  $L  <L              @L             dL  pL  �L  �L  �L          �L             �L  �L  �L  �L  �L           M             (M  4M  @M  TM              XM             �M  �M  �M  �M  �M          �M             �M  �M  N  (N  N          ,N             LN  \N  lN  �N  |N          �N             �N  �N  �N  O  �N          O             DO  TO  dO  �O  tO          �O             �O  �O  �O  �O  �O          �O             P  P  $P  TP  <P          XP             tP  �P  �P  �P  �P          �P             �P  �P  �P  Q  �P          Q             0Q  @Q  HQ  `Q  TQ          dQ             xQ  �Q  �Q  �Q  �Q          �Q             �Q  �Q  �Q   R  R          $R             DR  PR  XR  hR  `R          lR             �R  �R  �R  �R  �R          �R             �R  �R  �R  S   S          S             4S  DS  LS  |S  dS          �S             �S  �S  �S  �S  �S          �S             �S  �S   T  T  T          T             0T  8T  DT  LT                             PT  \T  dT  pT                              tT  |T  �T  �T                             �T  �T  �T  �T                             �T  �T  �T  �T                              �T  �T  �T   U                                                                          Agencia 999 Oficina Oficina 0   C�digo de la oficina donde se crea el activo    Anos_Adepreciar 9999    Vida �til   Vida �til   0   Vida �til en a�os del activo fijo   Cen_Costos  999 Centro de Costos    0   Centro de costos donde se encuentra el activo fijo  Codigo  X(15)   C�digo Activo   C�digo      C�digo del activo fijo  Cos_Historico   >>>,>>>,>>>,>>9 Costo Hist�rico 0   Costo hist�rico del activo fijo Descripcion X(100)  Descripci�n Descripci�n     Descripci�n del activo fijo. Ejemplo: Modelo, Serie, Marca..    Estado  9   Estado  1   Estado del registro del activo fijo Fec_Avaluo  99/99/9999  Fecha de Avaluo Fec.Avaluo  ?   D�a, mes y a�o del aval�o del activo fijo   Fec_Compra  99/99/9999  Fecha de Compra Fec.de Compra   ?   D�a, mes y a�o en que se compra el activo fijo  Fec_Contrato    99/99/9999  Fecha de Contrato   Fec.de Contrato ?   Ingrese la Fecha del Contrato de Arriendo   Fec_debaja  99/99/9999  Fecha de Baja   Fecha de Baja   ?   D�a, mes y a�o de la baja del activo fijo   Fec_Garantia    99/99/9999  Fecha Garant�a  TODAY   D�a, mes y a�o de la garant�a del activo    Fec_IniDepre    99/99/9999  Depreciaci�n    Fec.Depreciaci�n    ?   D�a, mes y a�o de inicio de depreciaci�n del activo fijo    Fec_Retiro  99/99/9999  Fecha de Retiro ?   D�a, mes y a�o del retiro del activo fijo   Fec_Venta   99/99/9999  Fecha de Venta  ?   D�a, mes y a�o de la venta del activo fijo  Grupo   99999   Grupo   Grupo   1   Grupo al que pertenece el activo    Mejoras >>>,>>>,>>>,>>9 Mejoras y Adiciones 0   Valor del incremento del activo fijo por mejoras    Neto    ->>>,>>>,>>>,>>9    Neto    0   Representa el valor actual del activo   Nit_Proveedor   X(12)   Proveedor   Proveedor       Documento de identificaci�n del proveedor   Nit_Responsable X(12)   Responsable     N�mero de identificaci�n del responsable del activo fijo    Fec_Asignacion  99/99/9999  Fec. de Asignaci�n  Fec. Asignaci�n ?   D�a, mes y a�o de la asignaci�n del activo fijo Nit_Seguro  X(12)   Seguro  Seguro      N�mero de identificaci�n de la aseguradora del activo fijo  Nombre  X(40)   Nombre  Nombre      Nombre del activo fijo  Nro_Factura X(10)   Factura Factura     N�mero de la factura del activo fijo    Nro_Seguro  X(10)   P�liza  P�liza      N�mero de la p�liza del seguro del activo fijo  Ord_Compra  X(10)   Ord. de Compra  Orden de Compra     Orden de compra del activo fijo Per_Depreciado  9999    Periodos depreciados    0   Meses depreciados del activo fijo   Sdo_Depre   >>>,>>>,>>>,>>9 Sdo.Depre   Sdo.Depre   0   Ingrese el Saldo Depreciaci�n del Activo    ValDepAcum  ->>>,>>>,>>>,>>9    Vr.Depreciaci�n Acumulada   0   Valor de la depreciaci�n acumulada  ValDepMes   ->>>,>>>,>>>,>>9    Vr.Depreciaci�n Mensual 0   Valor de la depreciaci�n mensual    Val_Compra  >>>,>>>,>>>,>>9 Vr. Compra  Vr. Compra  0   Valor de compra del activo fijo Val_Garantia    >>>,>>>,>>>,>>9 Vr. Garant�a    Vr. Garant�a    0   Valor de la garant�a del activo fijo    Vto_Seguro  99/99/9999  Vencimiento Seguro  ?   D�a, mes y a�o del vencimiento de la p�liza de seguro   Val_Avaluo  >>>,>>>,>>>,>>9.99  Vr. Avaluo  Vr. Avaluo  0   Valor de aval�o del activo  Val_Comercial   >>>,>>>,>>>,>>9 Vr. Comercial   Vr. Comercial   0   Valor comercial del activo fijo Val_Provision   >>>,>>>,>>>,>>9 Valor Provision Valor Provision 0   Ingrese el Valor de Provisi�n del Activo Fijo   Val_Valorizacion    >>>,>>>,>>>,>>9 Val Valorizacion    val.Valorizacion    0   Ingrese el Valor de Valorizaci�n del Activo Fijo    Sdo_Provision   >>>,>>>,>>>,>>9 Sdo Provision   Sdo Provision   0   Ingrese el Saldo de Provisi�n del Activo Fijo   Id_Prestamo Si/No   Prestado    Prestado    No  Indique si el Activo esta Prestado  Apellido1Prov   X(15)   Primer Apellido Prov    Primer Apellido Prov        Primer apellido del cliente Apellido2Prov   X(15)   Segundo Apellido Prov   Segundo Apellido Prov       Segundo apellido del cliente    NitProv X(12)   Nit Prov    Nit Prov        N�mero documento de identificaci�n  Nombre-2Prov    X(40)   Nombre Prov Nombre Prov     Nombre del cliente  Apellido-2Ase   X(15)   Primer Apellido Ase Primer Apellido Ase     Primer apellido del cliente Apellido-3Ase   X(15)   Segundo Apellido Ase    Segundo Apellido Ase        Segundo apellido del cliente    Nit-2Ase    X(12)   Nit Ase Nit Ase     N�mero documento de identificaci�n  Nombre-3Ase X(40)   Nombre Ase  Nombre Ase      Nombre del cliente  Apellido-4Res   X(15)   Primer Apellido Res Primer Apellido Res     Primer apellido del cliente Apellido-5Res   X(15)   Segundo Apellido Res    Segundo Apellido Res        Segundo apellido del cliente    Nit-3Res    X(12)   Nit Res Nit Res     N�mero documento de identificaci�n  Nombre-4Res X(40)   Nombre Res  Nombre Res      Nombre del cliente  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �    1 A�  ���:������      ��������    �           �                               &        .&        5&                �     i     i     i    5 	7 	8 	    �#  �#  �#  �#  �#  $  $  $  "$  -$  :$  E$  R$  _$  j$  t$  z$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  �$  %  %  %  !%  .%  9%  D%  R%  `%  q%  %  �%  �%  �%  �%  �%  �%  �%  �%  �%  �%  	&  &  &  %&  .&  5&  A&  M&    ��                            ����                            �'    t�                    ��    undefined                                                               �       x�  x   `   ��  ��                    �����               �w        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /      �                                3   ����       $       8  ���                       8      
                       � ߱        x  �   "   D       �     U          |�     �   �            4   ����d                 $                      ��                  �   �                   ��g           �   �  h  	  �   X                                        3   ����|       O   �   ��  ��  �   batchServices                                 �      ��                  �  �                 ��|        O   ����    e�          O   ����    R�          O   ����    ��            ��   l             8               ��                  `           ��                            ����                            clientSendRows                              P  8      ��                  �  �  h              �        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��   �             �               ��                �               ��   ,             �               ��                              ��                            ����                            commitTransaction                                 �      ��                  �  �  ,              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                               �      ��                  �  �                 ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   l             8               �� 
                 `  
         ��                            ����                            dataAvailable                               P  8      ��                  �  �  h               O�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              p	  X	      ��                  �  �  �	              dS�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �	             �	               �� 
          �       �	  
         ��                            ����                            destroyServerObject                             �
  �
      ��                  �  �  �
              �Y�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                �  �      ��                  �  �  �              ,Z�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              �  �      ��                  �  �  �              �Z�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            fetchFirst                              �  �      ��                  �  �  �              �@�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �  �      ��                  �  �  �              �C�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �  �      ��                  �  �  �              tD�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �  �      ��                  �  �  �              �G�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              �  �      ��                  �  �  �              H�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  �      ��                  �  �  �              |H�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �  �  �              I�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �O�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �P�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              \q        O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��   X             $               ��                  L           ��                            ����                            refreshRow                              8         ��                  �  �  P              �p�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              0        ��                    
  H              �s�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             `               ��   �             �               ��   �             �               ��                �               ��   4                             ��   \             (               �� 
  �      �       P  
             ��                  x           ��                            ����                            restartServerObject                             l  T      ��                      �              t��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              d  L      ��                      |              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              \  D      ��                      t              h��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  l      ��                      �              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            serverSendRows                              �  �      ��                  "  )  �              H��        O   ����    e�          O   ����    R�          O   ����    ��            ��                 �               ��   0              �               ��   X              $                ��   �              L                ��   �              t                �� 
          �       �   
         ��                            ����                            serverFetchRowObjUpdTable                               �!  �!      ��                  +  -  �!              �n        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       �!  
         ��                            ����                            setPropertyList                             �"  �"      ��                  /  1  �"              8n        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �"           ��                            ����                            serverSendRows                              �#  �#      ��                  3  :  �#              |�m        O   ����    e�          O   ����    R�          O   ����    ��            ��   <$             $               ��   d$             0$               ��   �$             X$               ��   �$             �$               ��   �$             �$               �� 
          �       �$  
         ��                            ����                            startServerObject                               �%  �%      ��                  <  =  �%              ��m        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                �&  �&      ��                  ?  B  �&              |�m        O   ����    e�          O   ����    R�          O   ����    ��            ��    '             �&               ��                  '           ��                            ����                            submitForeignKey                                (  �'      ��                  D  H   (              |�m        O   ����    e�          O   ����    R�          O   ����    ��            ��   l(             8(               ��   �(             `(               ��                  �(           ��                            ����                            submitValidation                                |)  d)      ��                  J  M  �)              �Z        O   ����    e�          O   ����    R�          O   ����    ��            ��   �)             �)               ��                  �)           ��                            ����                            synchronizeProperties                               �*  �*      ��                  O  R  �*              �`        O   ����    e�          O   ����    R�          O   ����    ��            ��   0+             �*               ��                  $+           ��                            ����                            transferToExcel                             ,  �+      ��                  \  a  ,,              Tg        O   ����    e�          O   ����    R�          O   ����    ��            ��   x,             D,               ��   �,             l,               ��   �,             �,               ��                  �,           ��                            ����                            undoTransaction                             �-  �-      ��                  c  d  �-              �p        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �.  �.      ��                  f  i  �.              Ts        O   ����    e�          O   ����    R�          O   ����    ��            ��   /             �.               ��                   /           ��                            ����                            updateQueryPosition                             �/  �/      ��                  k  l  0              4�D        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �0  �0      ��                  n  p   1              |�D        O   ����    e�          O   ����    R�          O   ����    ��            ��                  1           ��                            ����                            addRow          �1      �1     =       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   �1      �1      �1   	 D       CHARACTER,  canNavigate �1      2      42    N       LOGICAL,    closeQuery  2      @2      l2   
 Z       LOGICAL,    columnProps L2      x2      �2    e       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   �2      �2      3   	 q       CHARACTER,INPUT pcViewColList CHARACTER copyRow �2      83      `3    {       CHARACTER,INPUT pcViewColList CHARACTER createRow   @3      �3      �3   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �3      �3      4   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �3      (4      T4  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   44      �4      �4  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �4      5      ,5    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    5      P5      �5    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds `5      �5      6    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �5      ,6      d6    �       CHARACTER,  hasForeignKeyChanged    D6      p6      �6    �       LOGICAL,    openDataQuery   �6      �6      �6    �       LOGICAL,INPUT pcPosition CHARACTER  openQuery   �6      7      47   	 	      LOGICAL,    prepareQuery    7      @7      p7          LOGICAL,INPUT pcQuery CHARACTER rowAvailable    P7      �7      �7           LOGICAL,INPUT pcDirection CHARACTER rowValues   �7      �7      8   	 -      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �7      l8      �8   	 7      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   x8      �8      9   	 A      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �8      D9      t9    K      CHARACTER,  assignDBRow                             :  �9      ��                  V  X  :              �B�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4:  
         ��                            ����                            bufferCopyDBToRO                                (;  ;      ��                  Z  _  @;              C�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �;             X;  
             �� 
  �;             �;  
             ��   �;             �;               ��                  �;           ��                            ����                            compareDBRow                                �<  �<      ��                  a  b  �<              �`�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �=  �=      ��                  d  f  �=              �b�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �=           ��                            ����                            dataAvailable                               �>  �>      ��                  h  j  �>              Lg�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ?           ��                            ����                            fetchDBRowForUpdate                             �?  �?      ��                  l  m  @              �l�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              �@  �@      ��                  o  p  A              <o�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �A  �A      ��                  r  s  �A              Ă�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �B  �B      ��                  u  v  �B              ̃�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �C  �C      ��                  x  y  �C              �k        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              �D  �D      ��                  {  }  �D              ��k        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �D  
         ��                            ����                            initializeObject                                �E  �E      ��                    �  F              ԑk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                �F  �F      ��                  �  �   G              �k        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 G  
         ��                            ����                            releaseDBRow                                H  �G      ��                  �  �   H              ��k        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �H  �H      ��                  �  �  I              $�k        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �I  �I      ��                  �  �  J              ̚k        O   ����    e�          O   ����    R�          O   ����    ��            ��   XJ             $J               ��                  LJ           ��                            ����                            addQueryWhere   T9      �J      �J    l      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    �J      8K      pK    z      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO PK      �K      �K    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �K      hL      �L    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  |L      �L      M    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �L      ,M      \M    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    <M      �M      �M    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable �M      �M      N    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �M      ,N      \N     �      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    <N      �N      �N  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  �N      �N      O  "        HANDLE,INPUT pcColumn CHARACTER excludeColumns  �N      ,O      \O  #  !      CHARACTER,INPUT iTable INTEGER  getDataColumns  <O      |O      �O  $  0      CHARACTER,  getForeignValues    �O      �O      �O  %  ?      CHARACTER,  getQueryPosition    �O      �O      ,P  &  P      CHARACTER,  getQuerySort    P      8P      hP  '  a      CHARACTER,  getQueryString  HP      tP      �P  (  n      CHARACTER,  getQueryWhere   �P      �P      �P  )  }      CHARACTER,  getTargetProcedure  �P      �P       Q  *  �      HANDLE, indexInformation     Q      (Q      \Q  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    <Q      �Q      �Q  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �Q      DR      tR  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    TR      S      8S  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   S      �S      �S  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  �S      T      4T  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident T      �T      �T  1  �      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    �T      �T      0U  2        LOGICAL,    removeQuerySelection    U      <U      tU  3        LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   TU      �U      �U  4  3      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  �U      V      4V  5 
 A      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  V      XV      �V  6  L      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    hV      �V      W  7  [      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �V      <W      lW  8  l      LOGICAL,INPUT pcSort CHARACTER  setQueryString  LW      �W      �W  9  y      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   �W      �W      X  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �W      4X      hX  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              Y  �X      ��                  )  *  (Y              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               Z  �Y      ��                  ,  -   Z              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             [  �Z      ��                  /  0  [              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                 \  �[      ��                  2  3  \              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                               ]  �\      ��                  5  6  ]              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �]  �]      ��                  8  9  ^              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �^  �^      ��                  ;  =  _              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 $_  
         ��                            ����                            startServerObject                               `   `      ��                  ?  @  0`              ̹�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                a  �`      ��                  B  D  (a              亟        O   ����    e�          O   ����    R�          O   ����    ��            ��                  @a           ��                            ����                            getAppService   HX      �a      �a  <  �      CHARACTER,  getASBound  �a      �a      b  = 
 �      LOGICAL,    getAsDivision   �a      b      Lb  >  �      CHARACTER,  getASHandle ,b      Xb      �b  ?  �      HANDLE, getASHasStarted db      �b      �b  @  �      LOGICAL,    getASInfo   �b      �b      �b  A 	 �      CHARACTER,  getASInitializeOnRun    �b       c      8c  B  �      LOGICAL,    getASUsePrompt  c      Dc      tc  C  
      LOGICAL,    getServerFileName   Tc      �c      �c  D        CHARACTER,  getServerOperatingMode  �c      �c      �c  E  +      CHARACTER,  runServerProcedure  �c      d      8d  F  B      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   d      |d      �d  G  U      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �d      �d      e  H  c      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �d      (e      Te  I  q      LOGICAL,INPUT phASHandle HANDLE setASInfo   4e      te      �e  J 	 }      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �e      �e      �e  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �e      f      Lf  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   ,f      lf      �f  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �f      �f      �f  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �g  �g      ��                      �g              ��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  h             �g  
             ��   8h             h               �� 
                 ,h  
         ��                            ����                            addMessage                              i   i      ��                      0i              @.�        O   ����    e�          O   ����    R�          O   ����    ��            ��   |i             Hi               ��   �i             pi               ��                  �i           ��                            ����                            adjustTabOrder                              �j  pj      ��                      �j              ��\        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �j             �j  
             �� 
  k             �j  
             ��                  k           ��                            ����                            applyEntry                              �k  �k      ��                      l              $]        O   ����    e�          O   ����    R�          O   ����    ��            ��                  $l           ��                            ����                            changeCursor                                m  �l      ��                      ,m              X]        O   ����    e�          O   ����    R�          O   ����    ��            ��                  Dm           ��                            ����                            createControls                              4n  n      ��                  !  "  Ln              �]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               ,o  o      ��                  $  %  Do              �]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                $p  p      ��                  '  (  <p              �]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              $q  q      ��                  *  +  <q              ()]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              r   r      ��                  -  .  0r              �)]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              s  �r      ��                  0  1  $s              0*]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                t  �s      ��                  3  4   t              (-]        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              u  �t      ��                  6  ;  u              (.]        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  hu             4u  
             ��   �u             \u               ��   �u             �u               ��                  �u           ��                            ����                            modifyUserLinks                             �v  �v      ��                  =  A  �v              �ܧ        O   ����    e�          O   ����    R�          O   ����    ��            ��    w             �v               ��   (w             �v               �� 
                 w  
         ��                            ����                            removeAllLinks                              x  �w      ��                  C  D  $x              <�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               y  �x      ��                  F  J  y              ��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  dy             0y  
             ��   �y             Xy               �� 
                 �y  
         ��                            ����                            repositionObject                                tz  \z      ��                  L  O  �z              t�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��                  �z           ��                            ����                            returnFocus                             �{  �{      ��                  Q  S  �{              P��        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �{  
         ��                            ����                            showMessageProcedure                                �|  �|      ��                  U  X  �|              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��   D}             }               ��                  8}           ��                            ����                            toggleData                              $~  ~      ��                  Z  \  <~              |��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  T~           ��                            ����                            viewObject                              @  (      ��                  ^  _  X              4�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �f      �      �  O 
 "      LOGICAL,    assignLinkProperty  �      �      �  P  -      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      t�      ��  Q  @      CHARACTER,  getChildDataKey ��      ��      ��  R  N      CHARACTER,  getContainerHandle  ��      �       �  S  ^      HANDLE, getContainerHidden   �      (�      \�  T  q      LOGICAL,    getContainerSource  <�      h�      ��  U  �      HANDLE, getContainerSourceEvents    |�      ��      ��  V  �      CHARACTER,  getContainerType    ��      �       �  W  �      CHARACTER,  getDataLinksEnabled  �      ,�      `�  X  �      LOGICAL,    getDataSource   @�      l�      ��  Y  �      HANDLE, getDataSourceEvents |�      ��      ؂  Z  �      CHARACTER,  getDataSourceNames  ��      �      �  [  �      CHARACTER,  getDataTarget   ��      $�      T�  \  
      CHARACTER,  getDataTargetEvents 4�      `�      ��  ]        CHARACTER,  getDBAware  t�      ��      ̃  ^ 
 ,      LOGICAL,    getDesignDataObject ��      ؃      �  _  7      CHARACTER,  getDynamicObject    �      �      L�  `  K      LOGICAL,    getInstanceProperties   ,�      X�      ��  a  \      CHARACTER,  getLogicalObjectName    p�      ��      Ԅ  b  r      CHARACTER,  getLogicalVersion   ��      ��      �  c  �      CHARACTER,  getObjectHidden �       �      P�  d  �      LOGICAL,    getObjectInitialized    0�      \�      ��  e  �      LOGICAL,    getObjectName   t�      ��      Ѕ  f  �      CHARACTER,  getObjectPage   ��      ܅      �  g  �      INTEGER,    getObjectParent �      �      H�  h  �      HANDLE, getObjectVersion    (�      P�      ��  i  �      CHARACTER,  getObjectVersionNumber  d�      ��      Ȇ  j  �      CHARACTER,  getParentDataKey    ��      Ԇ      �  k        CHARACTER,  getPassThroughLinks �      �      H�  l  #      CHARACTER,  getPhysicalObjectName   (�      T�      ��  m  7      CHARACTER,  getPhysicalVersion  l�      ��      ̇  n  M      CHARACTER,  getPropertyDialog   ��      ؇      �  o  `      CHARACTER,  getQueryObject  �      �      H�  p  r      LOGICAL,    getRunAttribute (�      T�      ��  q  �      CHARACTER,  getSupportedLinks   d�      ��      Ĉ  r  �      CHARACTER,  getTranslatableProperties   ��      Ј      �  s  �      CHARACTER,  getUIBMode  �      �      D�  t 
 �      CHARACTER,  getUserProperty $�      P�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    `�      ��      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      �      4�  w  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �      X�      ��  x  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry h�      Ċ      ��  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   Њ      \�      ��  z  	      CHARACTER,INPUT piMessage INTEGER   propertyType    l�      ��      ��  {   	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      �      8�  |  -	      CHARACTER,  setChildDataKey �      D�      t�  }  <	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  T�      ��      Ќ  ~  L	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      ��      $�    _	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �      D�      ��  �  r	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled `�      ��      ؍  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��       �      0�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �      P�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  d�      ��      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      �      8�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      \�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  p�      ��      ��  � 
 �	      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��       �      4�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �      \�      ��  �  
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   p�      ��      �  �  &
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    Đ      �      @�  �  <
      LOGICAL,INPUT c CHARACTER   setLogicalVersion    �      \�      ��  �  Q
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   p�      ��      �  �  c
      LOGICAL,INPUT pcName CHARACTER  setObjectParent đ      �      4�  �  q
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �      T�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    h�      ��      �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks Ē      �      @�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName    �      `�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  x�      ��      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ̓      �      @�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks    �      h�      ��  �  �
      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   |�      ��      ��  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ܔ       �      L�  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty ,�      l�      ��  �  '      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage |�      ܕ      �  �  7      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      ,�      X�  � 	 C      CHARACTER,INPUT pcName CHARACTER     �     u  ��  �          4   �����                 �                      ��                  v  �                  �X           v  ��         w  ,�  ��          4   �����                 ��                      ��                  x  �                  `X           x  <�  ��     �  ė  4�          4   �����                 D�                      ��                  �  �                  �$X           �  ԗ         �                                  ,     
                    � ߱        Ș  $   �  p�  ���                           $   �  ��  ���                       x                         � ߱        �     �  8�  ��          4   �����                ��                      ��                  �  l	                  �%X           �  H�  �  o   �      ,                                 D�  $   �  �  ���                       �  @         �              � ߱        X�  �   �        l�  �   �  �      ��  �   �        ��  �   �  x      ��  �   �  �      ��  �   �  `      К  �   �  �      �  �   �        ��  �   �  �      �  �   �          �  �   �  |      4�  �   �  �      H�  �   �  t      \�  �   �  �      p�  �   �  ,      ��  �   �  �      ��  �   �  �      ��  �   �  P	      ��  �   �  �	      ԛ  �   �   
      �  �   �  t
      ��  �   �  �
      �  �   �  l      $�  �   �  �      8�  �   �  \      L�  �   �  �      `�  �   �  D      t�  �   �  �      ��  �   �  �      ��  �   �  0      ��  �   �  �      Ĝ  �   �  �      ؜  �   �        �  �   �  X       �  �   �  �      �  �   �        (�  �   �  L      <�  �   �  �      P�  �   �  �      d�  �   �         x�  �   �  <      ��  �   �  x      ��  �   �  �      ��  �   �  �          �   �  ,                      Ԟ          @�  (�      ��                  �	  �	  X�              40k        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱         �  $  �	  p�  ���                           O   �	  ��  ��  h               l�          \�  d�    L�                                             ��                            ����                                D9      ȝ      �     V     t�                       p�  K                     ��     �	  (�  ��          4   ����t                ��                      ��                  �	  h
                  hs           �	  8�  ��  �   �	  �      Р  �   �	  H      �  �   �	  �      ��  �   �	  @      �  �   �	  �       �  �   �	  8      4�  �   �	  �      H�  �   �	  (      \�  �   �	  �      p�  �   �	         ��  �   �	  �      ��  �   �	        ��  �   �	  �          �   �	        �     �
  ء  H�          4   ����x                X�                      ��                  �
  7                  ���           �
  �  l�  �   �
  �      ��  �   �
  T      ��  �   �
  �      ��  �   �
  D      ��  �   �
  �      Т  �   �
  �      �  �   �
  p      ��  �   �
  �      �  �   �
  X       �  �   �
  �      4�  �   �
  �      H�  �   �
  D       \�  �   �
  �       p�  �   �
  �       ��  �   �
  x!      ��  �   �
  �!      ��  �   �
  h"      ��  �   �
  �"      ԣ  �   �
  `#      �  �   �
  �#      ��  �   �
  X$      �  �   �
  �$      $�  �   �
  �$      8�  �   �
  L%      L�  �   �
  �%      `�  �   �
  <&      t�  �   �
  �&      ��  �   �
  4'      ��  �   �
  �'      ��  �   �
  ,(      Ĥ  �   �
  h(      ؤ  �   �
  �(      �  �   �
  X)       �  �   �
  �)      �  �   �
  *      (�  �   �
  �*      <�  �   �
  �*      P�  �   �
  l+      d�  �   �
  �+      x�  �   �
  \,      ��  �   �
  �,      ��  �   �
  L-      ��  �   �
  �-      ȥ  �   �
  <.      ܥ  �   �
  �.      �  �   �
  4/      �  �   �
  �/          �   �
  $0      ̨     E  0�  ��          4   ����T0                ��                      ��                  F  �                  �            F  @�  Ħ  �   J  �0      ئ  �   K  (1      �  �   L  �1       �  �   M  2      �  �   N  �2      (�  �   O  3      <�  �   P  |3      P�  �   Q  �3      d�  �   R  t4      x�  �   S  �4      ��  �   T  l5      ��  �   U  �5      ��  �   V  d6      ȧ  �   W  �6      ܧ  �   X  L7      �  �   Y  �7      �  �   Z  <8      �  �   [  �8      ,�  �   \  ,9      @�  �   ]  �9      T�  �   ^  :      h�  �   _  X:      |�  �   `  �:      ��  �   a  H;      ��  �   b  �;      ��  �   c  8<          �   d  �<      Ы     �  �  T�          4   ����=  	              d�                      ��             	     �  �                  <<�           �  ��  x�  �   �  |=      ��  �   �  �=      ��  �   �  t>      ��  �   �  �>      ȩ  �   �  l?      ܩ  �   �  �?      �  �   �  \@      �  �   �  �@      �  �   �  TA      ,�  �   �  �A      @�  �   �  DB      T�  �   �  �B      h�  �   �  <C      |�  �   �  �C      ��  �   �  ,D      ��  �   �  �D      ��  �   �  $E      ̪  �   �  �E      �  �   �  F      ��  �   �  �F      �  �   �  G      �  �     �G      0�  �     �G      D�  �     8H      X�  �     �H      l�  �     0I      ��  �     �I      ��  �      J          �     �J      getRowObjUpdStatic  deleteRecordStatic  d�     9  �  ��          4   ����K      /   :  $�     4�                          3   ����K            T�                      3   ����<K  �     C  |�  �  <�      4   ����XK  
              ��                      ��             
     D  �                  !�           D  ��  �  �   H  �K      h�  $   I  <�  ���                       �K     
                    � ߱        |�  �   J  L      ԭ  $   L  ��  ���                       ,L  @         L              � ߱        ��  $   O   �  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱         �  V   Y  ,�  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        ��  $   u  ��  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   �  L�  ���                                      �                      ��                  �  E                  H"�           �  ܯ  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   �  L�  ���                        adm-clone-props ��  0�              �     W     4                          0  @                     start-super-proc    @�  ��  �           �     X                                  a                     ��     ]  $�  4�          4   �����X      /   ^  `�     p�                          3   ���� Y            ��                      3   ���� Y  ��  $   x  ̲  ���                       @Y                         � ߱        ̴     �  �  ��   �      4   ����\Y                ��                      ��                  �  �                  ���           �   �  pY                     �Y                     �Y                         � ߱            $   �  ��  ���                              �  8�  t�          4   �����Y  �Y                         � ߱            $   �  H�  ���                       �Y                         � ߱        ��  $   �  ��  ���                       �     �  �   �  x�      4   �����Y      $   �  L�  ���                       Z                         � ߱            �   �  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱        �  V   �  ��  ���                        0�  �   �  D\      (�     z  H�  X�          4   �����\      /   {  ��     ��                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   �  Ķ  ���                        �^     
                h_                     �`  @        
 x`              � ߱        �  V   �  T�  ���                        T�     -  ��  l�          4   �����`                |�                      ��                  .  6                  T��           .  �  �  /   /  ��     ��                          3   �����`            ظ                      3   �����`      /   0  �     $�                          3   ����a            D�                      3   ����8a  �  /  �  ��         la                      3   ����Ta  initProps   ��  ��              H     Y     @                          <  ]#  	                                   Ⱥ          p�  X�      ��                 M  f  ��              <YZ        O   ����    e�          O   ����    R�          O   ����    ��      g#                      ��          ��  p   X  y  �      c  d�  T�     y                                        ��                  Y  u                  �Z           Y  ��  �  Ի     0y                                        ��                  v  �                  H�Z           v  t�  d�  T�     Dy                                        ��                  �  �                  �Z           �  ��  �  Լ     Xy                                        ��                  �  �                  �Z           �  t�  d�  T�     ly                                        ��                  �  �                  ��Z           �  ��  �  Խ     �y                                        ��                  �                    ܃Z           �  t�  d�  T�     �y                                        ��                    #                  ��Z             ��  �  Ծ     �y                                        ��                  $  @                  \�Z           $  t�  d�  T�     �y  	                                      ��             	     A  ]                  ,�Z           A  ��  �  Կ     �y  
                                      ��             
     ^  z                  ��Z           ^  t�  d�  T�     �y                                        ��                  {  �                  L�Z           {  ��  ��  ��     �y                                        ��                  �  �                  �Z           �  t�  d�  T�     z                                        ��                  �  �                  �Z           �  ��  ��  ��      z                                        ��                  �  �                  ��Z           �  t�  d�  T�     4z                                        ��                  �                    ��Z           �  ��  ��  ��     Hz                                        ��                    (                  ��Z             t�  d�  T�     \z                                        ��                  )  E                  `�Z           )  ��      ��     pz                                        ��                  F  b                  0�Z           F  t�      O   e  ��  ��  �z               h�          P�  \�   , 0�                                                       �     ��                            ����                            ��  ��  ��  ��      ��     Z     p�                      � l�  y#                     ��     {  $�  ��          4   �����z                ��                      ��                  |  �                  ��Z           |  4�  �  /   }  ��     ��                          3   �����z             �                      3   �����z  |�  /   ~  <�     L�                          3   �����z            l�                      3   �����z  ��  /   �  ��     ��                          3   ����{            ��                      3   ����4{      /   �  �     $�                          3   ����T{            D�                      3   ����t{  �{     
                L|                     �}  @        
 \}              � ߱        ��  V     T�  ���                        ��  $   '  �  ���                       �}                         � ߱        �}     
                H~                     �  @        
 X              � ߱        ��  V   1  <�  ���                        ��  $   K  ��  ���                       �     
                    � ߱        �     
                4�                     ��  @        
 D�              � ߱        ��  V   U  $�  ���                        p�  $   p  ��  ���                       ��     
                    � ߱        ��     
                 �                     p�  @        
 0�              � ߱        ��  V   z  �  ���                        X�  $   �  ��  ���                       ��                         � ߱        ��     
                ,�                     |�  @        
 <�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      T�  $   �  ��  ���                       ��     
                    � ߱        ȅ     
                D�                     ��  @        
 T�              � ߱        ��  V   �  ��  ���                        ��  $   �  ��  ���                       ��     
                    � ߱        ��  �   �  ��      D�  $     �  ���                       �     
                    � ߱        X�  �     �      ��  $   =  ��  ���                       H�                         � ߱               H  ��  ��          4   ����d�      /   I  �     �                          3   ������  D�     
   4�                      3   ������  t�        d�                      3   ������  ��        ��                      3   ������            ��                      3   ����܈  pushRowObjUpdTable  ��  ��  �                   [      �                               �&                     pushTableAndValidate    ��  D�  �           p     \     �                          �  '                     remoteCommit    \�  ��  �           d     ]     �                          �  N'                     serverCommit    ��  $�  �           `     ^     �                          �  ['                                     8�          �  ��      ��                  l  y   �              0C�        O   ����    e�          O   ����    R�          O   ����    ��          O   w  ��  ��  �    ��                            ����                            4�  ��      ��              _      P�                      
�     h'                     disable_UI  ��  ��                      `      �                               {'  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��   �  8�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  |�  ��      returnFocus ,INPUT hTarget HANDLE   l�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��   �  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  `�  p�      removeAllLinks  ,   P�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE t�  ��   �      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  x�  ��      hideObject  ,   h�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  ,�  <�      changeCursor    ,INPUT pcCursor CHARACTER   �  h�  t�      applyEntry  ,INPUT pcField CHARACTER    X�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  l�  t�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE \�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��   �  �      runServerObject ,INPUT phAppService HANDLE  ��  <�  P�      disconnectObject    ,   ,�  d�  t�      destroyObject   ,   T�  ��  ��      bindServer  ,   x�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  ,�      releaseDBRow    ,   �  @�  P�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   0�  |�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE l�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ,�  <�      compareDBRow    ,   �  P�  d�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   @�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER     �  H�  \�      updateQueryPosition ,   8�  p�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    `�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  |�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   l�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  T�  h�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  D�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  0�  D�      startServerObject   ,    �  X�  h�      setPropertyList ,INPUT pcProperties CHARACTER   H�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��  �      rowObjectState  ,INPUT pcState CHARACTER    ��  0�  @�      retrieveFilter  ,    �  T�  h�      restartServerObject ,   D�  |�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   l�  ��  ��      refreshRow  ,   t�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  (�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  X�  p�      initializeServerObject  ,   H�  ��  ��      initializeObject    ,   t�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  �  �      fetchPrev   ,   ��  $�  0�      fetchNext   ,   �  D�  P�      fetchLast   ,   4�  d�  p�      fetchFirst  ,   T�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   t�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �   �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema     �  l�  |�      dataAvailable   ,INPUT pcRelative CHARACTER \�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��  $�  4�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 l%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        ��    ,   %               � 
"    
 � %              h �P  \         (          
�                          
�            � M   �
"    
 X
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��               1� ]  
 �� h   � %               o%   o           � m    �
"   
 ��           �    1� n   �� h   � %               o%   o           � |   �
"   
 ��           �    1� �  
 �� h   � %               o%   o           � �   �
"   
 ��           l    1� �   �� h   � %               o%   o           � m    �
"   
 ��           �    1� �   �� h   � %               o%   o           � �   �
"   
 ��           T    1� �   �� �   � %               o%   o           %               
"   
 � �          �    1� �   � � �     
"   
 ��               1� �   �� h   � %               o%   o           �   �
"   
 ��           �    1�    �� h   � %               o%   o           �   S �
"   
 ��           �    1� s   �� �   � %               o%   o           %               
"   
 ��           p    1� �   �� �   � %               o%   o           %               
"   
 ��           �    1� �   �� �   � %               o%   o           %              
"   
 � �          h    1� �   � � �     
"   
 ��           �    1� �  
 �� �   � %               o%   o           %               
"   
 ��                1� �   �� h   � %               o%   o           � m    �
"   
 � �          �    1� �   � � �     
"   
 ��           �    1� �   �� h   � %               o%   o           � �  t �
"   
 � �          D	    1� _  
 � � �     
"   
 ��           �	    1� j   �� h   � %               o%   o           � {  � �
"   
 ��           �	    1�    �� h   � %               o%   o           � m    �
"   
 ��           h
    1�   
 �� *   � %               o%   o           %               
"   
 �           �
    1� .   � �   � %               o%   o           %              
"   
 �           `    1� 6   � h   � %               o%   o           � m    
"   
 �           �    1� G   � h   � %               o%   o           o%   o           
"   
 X�           P    1� W  
 X� h   � %               o%   o           � m    
"   
 �           �    1� b   � s  	 � %               o%   o           � }  / X
"   
 � �          8    1� �   � � s  	   
"   
 �           t    1� �   � s  	 � o%   o           o%   o           � m    
"   
 � �          �    1� �   � � s  	   
"   
 �           $    1� �   � s  	 � o%   o           o%   o           � m    
"   
 � �          �    1� �   � � �     
"   
 � �          �    1� �   � � s  	   
"   
 � �              1�    � � s  	   
"   
 � �          L    1�    � � s  	   
"   
 X�           �    1� '   X� �   � o%   o           o%   o           %              
"   
 � �              1� 8   � � s  	   
"   
 � �          @    1� F  
 � � Q     
"   
 � �          |    1� Y   � � s  	   
"   
 � �          �    1� h   � � s  	   
"   
 � �          �    1� {   � � s  	   
"   
 � �          0    1� �   � � s  	   
"   
 � �          l    1� �  	 � � s  	   
"   
 � �          �    1� �   � � s  	   
"   
 � �          �    1� �   � � s  	   
"   
 �                1� �   � h   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         � �          
�    � 	     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1�   
 � h   � %               o%   o           � m    
"   
 �           <    1�   
 � h   � %               o%   o           o%   o           
"   
 �           �    1� "   � �   � %               o%   o           o%   o           
"   
 �           4    1� +   � �   � %               o%   o           %               
"   
 �           �    1� :   � �   � %               o%   o           %               
"   
 l�           ,    1� G   l� h   � %               o%   o           � m    
"   
 X�           �    1� N   X� �   � %               o%   o           %              
"   
 X�               1� `   X� �   � %               o%   o           o%   o           
"   
 X�           �    1� l   X� h   � %               o%   o           o%   o           
"   
 �               1� z  	 � h   � %               o%   o           � m    
"   
 �           �    1� �   � h   � %               o%   o           o%   o           
"   
 �               1� �   � h   � %               o%   o           o%   o           
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 �           �    1� �   � �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 � �   � %               o%   o           %              
"   
 �           H    1� �   � h   � %               o%   o           o%   o           
"   
 �           �    1� �   � h   � %               o%   o           � m    X
"   
 �           8    1� �   � h   � %               o%   o           o%   o           
"   
 � �          �    1� �   � � �     
"   
 X�           �    1�    X� h   � %               o%   o           �   ! 
"   
 �           d    1� 6   � h   � %               o%   o           � m    X
"   
 �           �    1� C   � h   � %               o%   o           � V   
"   
 � �          L    1� e   � � r     
"   
 � �          �    1� x   � � �     
"   
 �           �    1� �   � h   � %               o%   o           � m    
"   
 � �          8     1� �  
 � � �     
"   
 l�           t     1� �   l� �   � %               o%   o           o%   o           
"   
 X�           �     1� �   X� �   � %               o%   o           %               
"   
 �           l!    1� �   � �   � %               o%   o           %               
"   
 X�           �!    1� �   X� h   � %               o%   o           � m    
"   
 X�           \"    1� �   X� h   � %               o%   o           o%   o           
"   
 �           �"    1� �   � �   � %               o%   o           %              
"   
 �           T#    1� �   � �   � %               o%   o           %               
"   
 l�           �#    1� 	   l� �   � %               o%   o           %               
"   
 � �          L$    1�    � � �     
"   
 � �          �$    1� &   � � h     
"   
 X�           �$    1� 3   X� *   � %               o%   o           o%   o           
"   
 X�           @%    1� ?   X� h   � %               o%   o           � m    
"   
 X�           �%    1� M   X� h   � %               o%   o           o%   o           
"   
 �           0&    1� [   � �   � o%   o           o%   o           o%   o           
"   
 �           �&    1� p   � s  	 � %               o%   o           o%   o           
"   
 �           ('    1� �   � h   � %               o%   o           o%   o           
"   
 �           �'    1� �  
 � *   � %               o%   o           o%   o           
"   
 � �           (    1� �   � � h     
"   
 X�           \(    1� �   X� h   � %               o%   o           � �  4 
"   
 �           �(    1� �  
 � �   � %               o%   o           %              
"   
 � �          L)    1�    � � �     
"   
 �           �)    1�    � h   � %               o%   o           � m    l
"   
 �           �)    1�     � �   � %               o%   o           %              
"   
 X�           x*    1� /   X� h   � %               o%   o           � m    
"   
 �           �*    1� <   � h   � %               o%   o           � m    X
"   
 �           `+    1� J   � h   � %               o%   o           � m    
"   
 �           �+    1� V   � �   � %               o%   o           %               
"   
 �           P,    1� e  	 � �   � %               o%   o           o%   o           
"   
 l�           �,    1� o   l� h   � %               o%   o           � ~  	 
"   
 �           @-    1� �   � *   � %               o%   o           %       �       
"   
 �           �-    1� �   � h   � %               o%   o           � m    
"   
 �           0.    1� �   � �   � o%   o           o%   o           %              
"   
 �           �.    1� �   � �   � %               o%   o           %               
"   
 �           (/    1� �   � h   � %               o%   o           o%   o           
"   
 �           �/    1� �   � s  	 � %               o%   o           � m    
"   
 � �          0    1� �   � � s  	   P �L 
�H T   %              �     }        �GG %              
"   
 l�           �0    1� �  
 l� h   � %               o%   o           � m    l
"   
 �           1    1� �   � �   � %               o%   o           %               
"   
 �           �1    1�   	 � h   � %               o%   o           � m    
"   
 X�           2    1�    X� h   � %               o%   o           � m    
"   
 �           �2    1� #   � �   � %               o%   o           %               
"   
 �           �2    1� 3   � h   � %               o%   o           � m    
"   
 �           p3    1� F   � h   � %               o%   o           o%   o           
"   
 �           �3    1� N   � h   � %               o%   o           o%   o           
"   
 �           h4    1� [   � �   � %               o%   o           o%   o           
"   
 l�           �4    1� i   l� �   � %               o%   o           o%   o           
"   
 �           `5    1� y   � �   � %               o%   o           o%   o           
"   
 �           �5    1� �   � h   � %               o%   o           o%   o           
"   
 �           X6    1� �  	 � s  	 � %               o%   o           � m    X
"   
 X�           �6    1� �  
 X� s  	 � %               o%   o           � m    
"   
 �           @7    1� �   � h   � %               o%   o           � m    X
"   
 �           �7    1� �   � h   � %               o%   o           o%   o           
"   
 �           08    1� �   � h   � %               o%   o           o%   o           
"   
 �           �8    1� �   � h   � %               o%   o           � m    
"   
 �            9    1� �   � h   � %               o%   o           � m    
"   
 �           �9    1� �   � s  	 � %               o%   o           o%   o           
"   
 � �          :    1�    � � �     
"   
 �           L:    1�    � h   � %               o%   o           � m    
"   
 �           �:    1� (   � h   � %               o%   o           o%   o           
"   
 l�           <;    1� ;   l� �   � %               o%   o           o%   o           
"   
 �           �;    1� M  
 � h   � %               o%   o           � m    
"   
 �           ,<    1� X   � h   � %               o%   o           � m    
"   
 X�           �<    1� p   X� �   � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 X�           p=    1� �  	 X� �   � %               o%   o           o%   o           
"   
 X�           �=    1� �   X� �   � %               o%   o           o%   o           
"   
 �           h>    1� �   � �   � %               o%   o           o%   o           
"   
 l�           �>    1� �   l� �   � %               o%   o           %              
"   
 �           `?    1� �   � h   � %               o%   o           � �  M l
"   
 �           �?    1� $   � �   � %               o%   o           %              
"   
 �           P@    1� 5   � �   � %               o%   o           %               
"   
 �           �@    1� I   � �   � %               o%   o           %               
"   
 �           HA    1� `   � s  	 � %               o%   o           � n   
"   
 �           �A    1� |   � �   � %               o%   o           %               
"   
 �           8B    1� �   � s  	 � %               o%   o           o%   o           
"   
 �           �B    1� �   � �   � o%   o           o%   o           %              
"   
 l�           0C    1� �   l� s  	 � o%   o           o%   o           � m    l
"   
 �           �C    1� �   � �   � o%   o           o%   o           o%   o           
"   
 �            D    1� �   � �   � o%   o           o%   o           o%   o           
"   
 �           �D    1� �   � s  	 � o%   o           o%   o           o%   o           
"   
 �           E    1� �   � �   � o%   o           o%   o           o%   o           
"   
 �           �E    1� �   � s  	 � o%   o           o%   o           �    
"   
 �           F    1� 
   � s  	 � o%   o           o%   o           �    
"   
 �           |F    1� %   � �   � %               o%   o           %               
"   
 �           �F    1� 9   � �   � %               o%   o           %               
"   
 � �          tG    1� M   � � s  	   
"   
 X�           �G    1� a   X� �   � %               o%   o           %               
"   
 X�           ,H    1� m   X� h   � %               o%   o           o%   o           
"   
 �           �H    1� �   � h   � %               o%   o           o%   o           
"   
 �           $I    1� �   � �   � %               o%   o           o%   o           
"   
 �           �I    1� �   � h   � %               o%   o           � m    
"   
 �           J    1� �   � �   � %               o%   o           %               
"   
 �           �J    1� �  	 � �   � %               o%   o           %                "    � %     start-super-proc u� %     adm2/smart.p }�P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   �p�               �L
�    %              � 8       N    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       O    �� �   �p�               �L"  	  , �   �    �    � �     }        �A      |    "  	    �    %              (<   \ (    |    �     }        �A�    �A"  
      "  	  �"  
    < "  	  �"  
  (    |    �     }        �A�    �A"  
  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      Q    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       R    �� ]  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         � �          
�    � 	     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � m      � m      � m      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        �U    �� �   �
"   
   � 8      DV    � $         � �          
�    � 	   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � /   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 "      �       }        �
"   
 � %              %                "    � %     start-super-proc u� %     adm2/appserver.p ��    � �     
�    �     }        �%               %      Server  - �     }        �    "    � m    � %               %      Client      "    � m    � %      NONE    p�,  8         $     "            � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "            � �   �
�     "    � %     start-super-proc t� %     adm2/dataquery.p k
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   �p�               �L
�    %              � 8      D]    � $         � �   �     
�    � 	   �
"   
 �p� @  , 
�       T^    �� j   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   �p�               �L
�    %              � 8      \_    � $         � �   �     
�    � 	   �
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    � %     start-super-proc s� %     adm2/query.p |�%     start-super-proc s� %     adm2/queryext.p % 	    initProps �
�    %0&   FOR EACH Act_Fijo NO-LOCK,       EACH CliProveedor WHERE CliProveedor.Nit = Act_Fijo.Nit_Proveedor NO-LOCK,       EACH CliAseguradora WHERE CliAseguradora.Nit = Act_Fijo.Nit_Seguro NO-LOCK,       EACH CliResponsable WHERE CliResponsable.Nit = Act_Fijo.Nit_Responsable NO-LOCK INDEXED-REPOSITION i�   � G     � I     � K  3       "    �     � 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        @c    �� �   � P   �        Lc    �@    
� @  , 
�       Xc    �� �   �p�               �L
�    %              � 8      dc    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       td    ��   	 �p�               �L"    , %@60  rowObject.Apellido1Prov = CliProveedor.Apellido1  rowObject.Apellido2Prov = CliProveedor.Apellido2  rowObject.NitProv = CliProveedor.Nit  rowObject.Nombre-2Prov = CliProveedor.Nombre  rowObject.Apellido-2Ase = CliAseguradora.Apellido1  rowObject.Apellido-3Ase = CliAseguradora.Apellido2  rowObject.Nit-2Ase = CliAseguradora.Nit  rowObject.Nombre-3Ase = CliAseguradora.Nombre  rowObject.Apellido-4Res = CliResponsable.Apellido1  rowObject.Apellido-5Res = CliResponsable.Apellido2  rowObject.Nit-3Res = CliResponsable.Nit  rowObject.Nombre-4Res = CliResponsable.Nombre d�    "      � I         %              %                   "      %                  "      "      T(        "    %              "    � I   � "      �       "    ��    "    �    � � m      �    ��    "     �     S    "      "    �     "    %                � @    �     t T     P   4       � "      (0       4       "      � m      � m    �� G   T ,  %              T   "    "    � � I     �    �� G   T    �    "    �    � "      �    �"      %                   %              %                   "      %                  "      �     "       \      H   "      ((       "    �%              � m    � � �     4       "      
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        Lk    �� �   � P   �        Xk    �@    
� @  , 
�       dk    �� �   �p�               �L
�    %              � 8      pk    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       �l    �� �  
 �p�               �L"    ,       "  
  �    � �  �X� I   �       "  	    �    � �  �� � I   X�   � G     � I     � �  ���   � G     � I   �� �  �X      "  
  �    � x   0 �� I   �       "  	    �    � x   0 � � I   �   ,        "    �� �   ��   � G   �� I   �� x   0 �    ,        "      � �     �   � G   �� I   � � x   0       "  
  ��    � �   0 �� I   �       "  	    �    � �   0 � � I   �   ,        "    �� �   ��   � G   �� I   �� �   0 �    ,        "      � �     �   � G   �� I   � � �   0 �      "  
  ��    � �   0 � I   �       "  	    �    � �   0 � � I      ,        "    �� �   �   � G   �� I   � �   0 �    ,        "      � �     �   � G   � I   � � �   0 ��   � G     � I     � !  Q  
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        @q    �� �   � P   �        Lq    �@    
� @  , 
�       Xq    �� �   � p�               �L
�    %              � 8      dq    � $         � �          
�    � 	     
"   
 �p� @  , 
�       tr    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �r    �� X     p�               �L"    , 
"   
  p� @  , 
�       $s    �� 3    p�               �L"    ,     %              %                   "      %                  "      �     "      4 (        "  
    �    � !  Q  � I         "  	  �     "    XT    "      "      @ A,    �   � G   � � �     "    �"       T      @   "    � (        "      � m    �� m      � G   �"         "  	   %              D H   @ A,    �   � G   �� �     "    �"    ,    S   "    �� !  Q� I   � %                T      @   "    � (        "      � m    �� m      � G   �"         "  
   %                         "    � � �     "    �           "      � �   �"      
�H T   %              �     }        �GG %              
"   
 �
"   
   
"   
 �
"   
 �(�  L ( l       �        ,w    �� �   � P   �        8w    �@    
� @  , 
�       Dw    �� �   �p�               �L
�    %              � 8      Pw    � $         � �   �     
�    � 	   � 
"   
 �p� @  , 
�       `x    �� X   �p�               �L"    , 
"   
   p� @  , 
�       �x    �� 3     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc s� %     adm2/data.p %     start-super-proc s� %     adm2/dataext.p %     start-super-proc s� %     adm2/dataextcols.p %     start-super-proc s� %     adm2/dataextapi.p %              %              %              
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        |    �� �   � P   �        (|    �@    
� @  , 
�       4|    �� �   �p�               �L
�    %              � 8      @|    � $         � �   �     
�    � 	   �
"   
 �p� @  , 
�       P}    �� a   �p�               �L%               %     "dAct_Fijo.i"   
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ~    �� �   � P   �        $~    �@    
� @  , 
�       0~    �� �   �p�               �L
�    %              � 8      <~    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       L    �� `   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      (�    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       8�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      �    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       $�    �� e  	 �p�               �L
"   
 , 
"   
 �      � �&  	   �        |�    �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8       �    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       0�    �� �   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �         �    �@    
� @  , 
�       ,�    �� �   �p�               �L
�    %              � 8      8�    � $         � �          
�    � 	   �
"   
 �p� @  , 
�       H�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
         � �&   �
�    
�             �Gp�,  8         $     
"   
         � �&   �
�    �    � �&     
�        "    � m    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � 7'     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           x   `       ��                 �  �  �               d�        O   ����    e�          O   ����    R�          O   ����    ��         $   �  �   ���                       �U     
                    � ߱               �    �          4   ����V                �                      ��                  �  �                  ��           �  (  �  �  �  PV             �  �  4          4   �����V                D                      ��                  �  �                  \�           �  �  x  o   �      ,                                 �  �   �  �V      �  �   �  �V      �  $   �  �  ���                        W     
                    � ߱          �   �  @W         �   �  `W      4  �   �  �W          $   �  `  ���                       �W  @         �W              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 �  >  �               ��        O   ����    e�          O   ����    R�          O   ����    ��      P                      �          �  $     �   ���                       X     
                    � ߱                  �  �                      ��                                        �            (      4   ����$X      $     �  ���                       pX     
                    � ߱        d         (          4   �����X      /    T                               3   �����X  x  �   /  �X          O   <  ��  ��  �X               �          �  �   , �                          
                               �      ��                            ����                                            �           x   `       ��                 �  @  �               h��        O   ����    e�          O   ����    R�          O   ����    ��         $   �  �   ���                       ta                         � ߱        X  $   �  ,  ���                       �b                         � ߱               p  �          4   �����b  �b     
                pc                     �d  @        
 �d              � ߱            V   '  �  ���                        h  $   L  <  ���                       �d                         � ߱           $   M  �  ���                       g                         � ߱          0      �  �                      ��        0          O  e                  �%Z    8     O  �      $   O  \  ���                       0g                         � ߱        �  $   O  �  ���                       `g                         � ߱            4   �����g  �g                     �g                     �g                     Lh                     lh                         � ߱        �  $   P  �  ���                              ]  �  �          4   �����h      $   ^    ���                       �h          �i             � ߱        �  $   h  d  ���                       �i                         � ߱                 X  �                      ��        0          j  o                  �(Z    |     j  �      $   j  ,  ���                        j                         � ߱        �  $   j  �  ���                       0j                         � ߱            4   ����Xj      $   l  �  ���                       �j                         � ߱         k     
                |k                     �l  @        
 �l              � ߱        (  V   z    ���                        �l       
       
       m       	       	       @m                     lm                         � ߱        �  $   �  �  ���                       �m       
       
       �m       	       	        n                     Tn                         � ߱        �	  $   �  T  ���                       �n       
       
       �n       	       	       o                     do                         � ߱        ,
  $      	  ���                       �o       
       
       �o       	       	        p                     tp                         � ߱        X
  $   6  �	  ���                       L  $   e  �
  ���                       �p                         � ߱        �p     
                pq                     �r  @        
 �r          s  @        
 �r          ps  @        
 0s              � ߱        �  V   q  �
  ���                          �      @  �                      ��        0          �  �                  �-Z    p     �  x      $   �    ���                       |s                         � ߱        �  $   �  l  ���                       �s                         � ߱        �  4   �����s      4   �����s    $   �  �  ���                       `t                         � ߱             �  (  �          4   �����t                �                      ��                  �  �                  LWZ           �  8  �t                     ,u       	       	           � ߱            $   �  �  ���                              �  4  �          4   ����Tu                �                      ��                  �  �                  �WZ           �  D  �u                     Pv       
       
           � ߱            $   �  �  ���                       xv                     �v                         � ߱          $   �  (  ���                       �v     
                \w                     �x  @        
 lx          y  @        
 �x              � ߱            V   �  �  ���                                    7           �  �  � |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        x   `       ��                  �  �  �               D�y        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           x   `       ��                  �  �  �               D�y        O   ����    e�          O   ����    R�          O   ����    ��      �&       �              �                  $                  X  /  �       (  �                      3   ������            H                      3   �����      O   �  ��  ��  (�               �          �  �    �                                             ��                            ����                                            <          x   `       ��                  	  4  �               D�y        O   ����    e�          O   ����    R�          O   ����    ��      '       �              �          �       $                  "'                     �          -'                               �  /  (  h     x  P�                      3   ����,�            �                      3   ����X�  �  /  *  �     �  ��                      3   ����d�  l                            3   ������      $   *  @  ���                                                   � ߱                  �  �                  3   ������      $   *  �  ���                                                   � ߱        L  $   .     ���                       ��                         � ߱            O   2  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                      x   `       ��                  >  _  �               D�y        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  "'       �              �          -'                      �              /  \  @     P  �                      3   ����Љ  �        p  �                  3   �����      $   \  �  ���                                                   � ߱                  �                    3   ���� �      $   \  4  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           x   `       ��                    "  �               �C�        O   ����    e�          O   ����    R�          O   ����    ��             !  �   �           4   ���� �      �   !  4�    ��                            ����                            TXS appSrvUtils .\dAct_Fijo.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "dAct_Fijo.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Act_Fijo NO-LOCK,       EACH CliProveedor WHERE CliProveedor.Nit = Act_Fijo.Nit_Proveedor NO-LOCK,       EACH CliAseguradora WHERE CliAseguradora.Nit = Act_Fijo.Nit_Seguro NO-LOCK,       EACH CliResponsable WHERE CliResponsable.Nit = Act_Fijo.Nit_Responsable NO-LOCK INDEXED-REPOSITION ,   Act_Fijo CliProveedor CliAseguradora CliResponsable  rowObject.Apellido1Prov = CliProveedor.Apellido1  rowObject.Apellido2Prov = CliProveedor.Apellido2  rowObject.NitProv = CliProveedor.Nit  rowObject.Nombre-2Prov = CliProveedor.Nombre  rowObject.Apellido-2Ase = CliAseguradora.Apellido1  rowObject.Apellido-3Ase = CliAseguradora.Apellido2  rowObject.Nit-2Ase = CliAseguradora.Nit  rowObject.Nombre-3Ase = CliAseguradora.Nombre  rowObject.Apellido-4Res = CliResponsable.Apellido1  rowObject.Apellido-5Res = CliResponsable.Apellido2  rowObject.Nit-3Res = CliResponsable.Nit  rowObject.Nombre-4Res = CliResponsable.Nombre ; Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable Fec_Asignacion Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia Vto_Seguro Val_Avaluo Val_Comercial Val_Provision Val_Valorizacion Sdo_Provision Id_Prestamo Apellido1Prov Apellido2Prov NitProv Nombre-2Prov Apellido-2Ase Apellido-3Ase Nit-2Ase Nombre-3Ase Apellido-4Res Apellido-5Res Nit-3Res Nombre-4Res Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable Fec_Asignacion Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia Vto_Seguro Val_Avaluo Val_Comercial Val_Provision Val_Valorizacion Sdo_Provision Id_Prestamo Apellido1Prov Apellido2Prov NitProv Nombre-2Prov Apellido-2Ase Apellido-3Ase Nit-2Ase Nombre-3Ase Apellido-4Res Apellido-5Res Nit-3Res Nombre-4Res INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p Agencia Anos_Adepreciar Cen_Costos Codigo Cos_Historico Descripcion Estado Fec_Avaluo Fec_Compra Fec_Contrato Fec_debaja Fec_Garantia Fec_IniDepre Fec_Retiro Fec_Venta Grupo Mejoras Neto Nit_Proveedor Nit_Responsable Fec_Asignacion Nit_Seguro Nombre Nro_Factura Nro_Seguro Ord_Compra Per_Depreciado Sdo_Depre ValDepAcum ValDepMes Val_Compra Val_Garantia Vto_Seguro Val_Avaluo Val_Comercial Val_Provision Val_Valorizacion Sdo_Provision Id_Prestamo Apellido1Prov Apellido2Prov NitProv Nombre-2Prov Apellido-2Ase Apellido-3Ase Nit-2Ase Nombre-3Ase Apellido-4Res Apellido-5Res Nit-3Res Nombre-4Res RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery �  �6  �  HD      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   �	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc                /  <  >           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  $   Y   �          �                  initProps   �  �    '  L  M  O  P  ]  ^  e  h  j  l  o  z  �  �    6  e  q  �  �  �  �  �  �  �  �  �  �  �  �  �  @            l     lRet              �        piTableIndex    �  �  (   Z   X  t      �                  deleteRecordStatic  X  Y  u  v  �  �  �  �  �  �  �  �      #  $  @  A  ]  ^  z  {  �  �  �  �  �  �  �  �      (  )  E  F  b  c  e  f                 !       �  �     [       �      �                  pushRowObjUpdTable  �          �        pcValType                  $       �  d     \       �      L                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     4     ]       p      $                  remoteCommit    (  *  .  2  4  `             $       �        x        pcMessages            �        pcUndoIds   �  �     ^       H      �                  serverCommit    \  _  �  4     _                                  getRowObjUpdStatic  w  y  �  x     `               l                  disable_UI  !  "  <  $(       �"      �'                      <  �  �  8   RowObject   l         t         �         �         �         �         �         �         �         �         �         �                                     (         0         8         @         P         `         p         |         �         �         �         �         �         �         �         �         �         �                                     0         D         T         `         p         �         �         �         �         �         �         �         �         �         �                                    $         0         Agencia Anos_Adepreciar Cen_Costos  Codigo  Cos_Historico   Descripcion Estado  Fec_Avaluo  Fec_Compra  Fec_Contrato    Fec_debaja  Fec_Garantia    Fec_IniDepre    Fec_Retiro  Fec_Venta   Grupo   Mejoras Neto    Nit_Proveedor   Nit_Responsable Fec_Asignacion  Nit_Seguro  Nombre  Nro_Factura Nro_Seguro  Ord_Compra  Per_Depreciado  Sdo_Depre   ValDepAcum  ValDepMes   Val_Compra  Val_Garantia    Vto_Seguro  Val_Avaluo  Val_Comercial   Val_Provision   Val_Valorizacion    Sdo_Provision   Id_Prestamo Apellido1Prov   Apellido2Prov   NitProv Nombre-2Prov    Apellido-2Ase   Apellido-3Ase   Nit-2Ase    Nombre-3Ase Apellido-4Res   Apellido-5Res   Nit-3Res    Nombre-4Res RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     L  X  9   RowObjUpd                                 (          0          @          L          T          `          l          |          �          �          �          �          �          �          �          �          �          �          !         !         !         (!         4!         @!         P!         \!         h!         t!         �!         �!         �!         �!         �!         �!         �!         �!         �!         "         "          "         0"         @"         P"         \"         h"         x"         �"         �"         �"         �"         �"         �"         �"         �"         Agencia Anos_Adepreciar Cen_Costos  Codigo  Cos_Historico   Descripcion Estado  Fec_Avaluo  Fec_Compra  Fec_Contrato    Fec_debaja  Fec_Garantia    Fec_IniDepre    Fec_Retiro  Fec_Venta   Grupo   Mejoras Neto    Nit_Proveedor   Nit_Responsable Fec_Asignacion  Nit_Seguro  Nombre  Nro_Factura Nro_Seguro  Ord_Compra  Per_Depreciado  Sdo_Depre   ValDepAcum  ValDepMes   Val_Compra  Val_Garantia    Vto_Seguro  Val_Avaluo  Val_Comercial   Val_Provision   Val_Valorizacion    Sdo_Provision   Id_Prestamo Apellido1Prov   Apellido2Prov   NitProv Nombre-2Prov    Apellido-2Ase   Apellido-3Ase   Nit-2Ase    Nombre-3Ase Apellido-4Res   Apellido-5Res   Nit-3Res    Nombre-4Res RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   #          �"  
   appSrvUtils ,#       #     xiRocketIndexLimit  T#        @#  
   gshAstraAppserver   |#        h#  
   gshSessionManager   �#        �#  
   gshRIManager    �#        �#  
   gshSecurityManager  �#        �#  
   gshProfileManager   $        $  
   gshRepositoryManager    H$  	 	     0$  
   gshTranslationManager   l$  
 
     \$  
   gshWebManager   �$        �$     gscSessionId    �$        �$     gsdSessionObj   �$        �$  
   gshFinManager   �$        �$  
   gshGenManager    %        %  
   gshAgnManager   D%        4%     gsdTempUniqueID d%        X%     gsdUserObj  �%        x%     gsdRenderTypeObj    �%        �%     gsdSessionScopeObj  �%       �%  
   ghProp  �%       �%  
   ghADMProps  &       &  
   ghADMPropsBuf   <&       (&     glADMLoadFromRepos  X&       P&     glADMOk x&       l&  
   ghContainer �&    	   �&     cObjectName �&    
   �&     iStart  �&       �&     cAppService �&       �&     cASDivision  '       '     cServerOperatingMode    D'       4'     cContainerType  h'       X'     cQueryString    �'       |'  
   hRowObject  �'       �'  
   hDataQuery  �'       �'     cColumns             �'     cDataFieldDefs  (    X  �'  RowObject         X  (  RowObjUpd          "   U   �   �   �   �   u  v  w  x  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  l	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  h
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
  7  E  F  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  d  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                  �  9  :  C  D  H  I  J  L  O  Y  u  �  �  �  �  E  ]  ^  x  �  �  �  �  �  �  �  �  �  �  �  �  z  {  �  �  -  .  /  0  6  �  {  |  }  ~  �  �  �    '  1  K  U  p  z  �  �  �  �  �  �  �      =  H  I      %:  .\dAct_Fijo.w    P,  ��  C:\Progress\OpenEdge\src\adm2\data.i l,  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �,  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �,  $ , .\dAct_Fijo.i    -  �   C:\Progress\OpenEdge\src\adm2\query.i    ,-  z + C:\Progress\OpenEdge\src\adm2\delrecst.i `-  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �-   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �-  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    .  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   @.  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    �.  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �.  I� # C:\Progress\OpenEdge\src\adm2\smart.i     /  Ds & C:\Progress\OpenEdge\gui\fn  4/  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   \/  Q. $ C:\Progress\OpenEdge\gui\set �/  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    <0  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �0  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �0  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �0   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    41  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   p1  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �1  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i  2  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    42  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    x2  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �2  �j  C:\Progress\OpenEdge\gui\get �2  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    3  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    \3  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �3  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �3  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i 4  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   H4  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �4  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �4  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    5  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   D5  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  �5  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �5  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i 6  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    86  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   |6  ��    U:\Prog\dAct_Fijo_cl.w          (      �6  �   �     �6  [  1     �6     /  &   7  �   �     7     Q  .   (7  �   G     87     (     H7  �   %     X7       $   h7  �        x7     �  $   �7  �   �     �7     �  $   �7  �   �     �7     �  $   �7  �   �     �7     r  $   �7  �   o     �7     M  $   8  �   K     8     )  $   (8  �   '     88       $   H8  �   �     X8     �  -   h8  �   �     x8     �  ,   �8  k   n     �8  �  b      �8     H  +   �8  �  E      �8     +  +   �8  �  (      �8       +   �8  �        9     �  +   9  �  �      (9     �  +   89  �  �      H9     �  +   X9  �  �      h9     �  +   x9  �  �      �9     }  +   �9  �  z      �9     `  +   �9  �  ]      �9     C  +   �9  �  @      �9     &  +   �9  �  #      :     	  +   :  �        (:     �  +   8:  �  �      H:     �  +   X:  �  �      h:     �  +   x:  �  �      �:     �  +   �:  �  �      �:     x  +   �:  �  u      �:     [  +   �:  �  ;      �:       $   �:  �        ;     �  $   ;  j  �      (;     �  $   8;  i  �      H;     �  $   X;  h  �      h;     i  $   x;  ^  _      �;     9  *   �;  ]  8      �;       *   �;  \        �;     �  *   �;  [  �      �;     �  *   �;  Z  �      <     �  *   <  Y  �      (<     v  *   8<  X  u      H<     O  *   X<  W  N      h<     (  *   x<  V  '      �<       *   �<  U         �<     �  *   �<  T  �      �<     �  *   �<  S  �      �<     �  *   �<  R  �      =     e  *   =  Q  d      (=     >  *   8=  P  =      H=       *   X=  O        h=     �  *   x=  N  �      �=     �  *   �=  M  �      �=     �  *   �=  ?  �      �=     r  $   �=    A      �=       $   �=  �   �      >     ;  )   >  g         (>  a     !   8>     �  (   H>  _   �  !   X>     �  $   h>  ]   �  !   x>       $   �>  I   k  !   �>  �   b  "   �>     
  '   �>  �     "   �>     �  $   �>  �   �  "   �>     �  $   �>  �   �  "   ?     �  $   ?  g   �  "   (?     c     8?  O   K  "   H?  �   �  #   X?     �  &   h?  �   �  #   x?     K  %   �?  �   @  #   �?       $   �?  �     #   �?     �  $   �?  �   �  #   �?     �  $   �?  �   �  #   �?     �  $   @  �   �  #   @       $   (@  }   s  #   8@     Q  $   H@     �  #   X@     �  "   h@     ?  !   x@     �      �@     �     �@  �   �     �@  O   v     �@     e     �@          �@  �   �     �@  �   �     �@  O   �     A     �     A     h     (A  y   @     8A  �   6  
   HA  G   !     XA          hA     �
     xA  c   p
  
   �A  x   h
     �A  M   S
     �A     B
     �A     �	     �A  a   �	     �A  �  �	     �A     �	     �A  �  l	     B  O   ^	     B     M	     (B     �     8B  �   )     HB     �     XB     P     hB  x   J     xB     1     �B     �     �B     �     �B     �     �B     �     �B  Q   y     �B          �B     �     �B     �     C     �     C  ]   �  
   (C     �     8C     a  
   HC     S     XC     ?  
   hC  Z         xC     L  	   �C          �C     �     �C     �     �C  c   �     �C     �     �C     S     �C     ?     �C     %     D          D     &      (D           8D           