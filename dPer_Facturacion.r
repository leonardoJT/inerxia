	��V���K47  j �                                              �> 373400F0utf-8 MAIN \\192.168.101.9\desarrollo\prg\dPer_Facturacion.w,, PROCEDURE preTransactionValidate,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,Per_Factura integer 0 0,Fec_Inicial date 1 0,Fec_Final date 2 0,Fec_LimPago date 3 0,Estado integer 4 0,RowNum integer 5 0,RowIdent character 6 0,RowMod character 7 0,RowIdentIdx character 8 0,RowUserProp character 9 0,ChangedFields character 10 0        h              P             
  h  ��              ��              �>  	   +   � �  W   �� h  X   � <  Y   X�   [   l�   \   �� @  ]   �� $  ^   �� 4  `    �   a   ? (� �   ISO8859-1                                                                        $  �     �                                      �                  x�  	             �  �        D   ��            ��  �   0      <          �                                             PROGRESS                         $           
        
                    �              �                                                                                                     
                   bdcentral                        PROGRESS                              !   �          C                      ΎJ               �t                              �  t                      �  �  ?      PER_FACTURAFEC_INICIALFEC_FINALFEC_LIMPAGOESTADOFEC_LIMPAGOCLTE                                                             X     !   �      !                          ΎJ            ?   �t                              �  �                      �  �  ?      PER_FACTURAFEC_INICIALFEC_FINALFEC_LIMPAGOESTADOFEC_LIMPAGOCLTE                                                             �  7      P  
        
                  <               �                                                                                          7          
      �  I        
        
                  �  �             t                                                                                          I          
      @  [      �  
        
                  �  t             (                                                                                          [          
      �  h      l  
        
                  X  (             �                                                                                          h          
      �  {         
        
                    �  	           �                                                                                          {          
      \  �      �  
        
                  �  �  
           D                                                                                          �          
      	  �      �  
        
                  t  D	             �                                                                                          �          
      �	  �      <	  
        
                  (	  �	             �	                                                                                          �          
      x
  �      �	                             �	  �
             `
                                                                                          �                ,  �      �
                            �
  `                                                                                                       �                �  �      X  
        
                  D               �                                                                                          �          
      �  �        
        
                  �  �             |                                                                                          �          
      H  �      �  
        
                  �  |             0                                                                                          �          
      �        t                            `  0             �                                                                                                          �        (                              �             �                                                                                                          d  &      �                            �  �             L                                                                                          &                    7      �                            |                                                                                                          7                �         �       �  X  �       �  ��      d  
       �                       �      �              �       �  X  �     �  �  N      X         �         �    �          |      �                 ��                                               ��          p  �  L lP                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                   �  �  �  �  �          �      @           ,  <  4          @             l  x  �  �  �          �             �  �  �  �  �          �                   (             ,             D  L  X  `                             d  p  x  �                              �  �  �  �                             �  �  �  �                             �  �  �  �                                                                          Per_Factura >>>>>>9 Per�odo Per�odo 0   N�mero de Periodo de Facturaci�n    Fec_Inicial 99/99/9999  Fec.Ini Fec.Ini ?   Fecha Inicial del periodo de facturaci�n    Fec_Final   99/99/9999  Fec.Fin Fec.Fin ?   Fecha Final del periodo de facturaci�n  Fec_LimPago 99/99/9999  Fec.Pago    Fec.Pago    ?   Fecha L�mite de Pago    Estado  9   Estado  Estado  1   Estado de la Factura    RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ��������� ���         ?        O        V                �     i     i     i     		 	
 	    
    "  ,  8  ?  F  O  V  b                                                                                                                                     	                  
                                                                     �  �  �  �  �          �      @      �  �  �     �                       0  <  H  X  P          \             �  �  �  �  �          �             �  �  �  �  �          �                   $                             (  4  <  H                              L  T  \  d                             h  t  |  �                             �  �  �  �                              �  �  �  �                                                                          Per_Factura >>>>>>9 Per�odo Per�odo 0   N�mero de Periodo de Facturaci�n    Fec_Inicial 99/99/9999  Fec.Ini Fec.Ini ?   Fecha Inicial del periodo de facturaci�n    Fec_Final   99/99/9999  Fec.Fin Fec.Fin ?   Fecha Final del periodo de facturaci�n  Fec_LimPago 99/99/9999  Fec.Pago    Fec.Pago    ?   Fecha L�mite de Pago    Estado  9   Estado  Estado  1   Estado de la Factura    RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ��������� ���             ?        O        V                �     i     i     i     		 	
 	    
    "  ,  8  ?  F  O  V  b  n    ��                            ����                            �    ��                    v�    �    ��                    ��    �    v�    ?         �    m     undefined                                                               �       ��  �   p   �  ���                  �����               p�f                        O   ����    e�          O   ����    R�          O   ����    ��      x       �   �              4   ����      /                                    3   ����       $     L  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  <      d       4   ����d                 L                      ��                  �   �                   �^�                           �   �  �  	  �   �                                        3   ����|       O   �   ��  ��  �   batchServices                               @  (      ��                  R  U  X              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             p               ��                  �           ��                            ����                            clientSendRows                              �  �      ��                  W  ]  �              ��9                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��   $             �               ��   L                            ��   t             @               ��                  h           ��                            ����                            commitTransaction                               l  T      ��                  _  `  �              d��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             p  X      ��                  b  e  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  g  i  �              ح�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  k  n  
              �J                        O   ����    e�          O   ����    R�          O   ����    ��            ��   \
             (
               �� 
          �       P
  
         ��                            ����                            destroyServerObject                             T  <      ��                  p  q  l              �L'                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                d  L      ��                  s  t  |              �M'                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              h  P      ��                  v  x  �              TN'                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            fetchFirst                              �  |      ��                  z  {  �              �:V                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �  �      ��                  }  ~  �              $;V                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �  �      ��                  �  �  �              �;V                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �  �      ��                  �  �  �              (F�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              �  �      ��                  �  �  �              �F�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  �      ��                  �  �  �              (G�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �  �  �              8M�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �                �M�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �                dN�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  $           ��                            ����                            printToCrystal                              $        ��                  �  �  <              �6�                         O   ����    e�          O   ����    R�          O   ����    ��            ��   �             T               ��   �             |               ��                  �           ��                            ����                            refreshRow                              �  �      ��                  �  �  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              �  �      ��                  �  �  �              (�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��   4                             ��   \             (               ��   �             P               ��   �             x               ��   �             �               �� 
  �      �       �  
             ��                  �           ��                            ����                            restartServerObject                             �  �      ��                  �  �                L��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �                ,:�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                                �      ��                  �  �                c�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            saveContextAndDestroy                               <   $       ��                  �  �  T               ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  l            ��                            ����                            serverSendRows                              l!  T!      ��                  �  �  �!              @*                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �!             �!               ��   �!             �!               ��    "             �!               ��   H"             "               ��   p"             <"               �� 
          �       d"  
         ��                            ����                            serverFetchRowObjUpdTable                               p#  X#      ��                  �  �  �#              Ĳ�                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       �#  
         ��                            ����                            setPropertyList                             �$  �$      ��                  �  �  �$              4�Z                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �$           ��                            ����                            serverSendRows                              �%  �%      ��                  �  �  �%              ��Z                        O   ����    e�          O   ����    R�          O   ����    ��            ��   4&              &               ��   \&             (&               ��   �&             P&               ��   �&             x&               ��   �&             �&               �� 
          �       �&  
         ��                            ����                            startServerObject                               �'  �'      ��                  �  �  �'              �}X                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                �(  �(      ��                  �  �  �(              X~X                        O   ����    e�          O   ����    R�          O   ����    ��            ��   8)             )               ��                  ,)           ��                            ����                            submitForeignKey                                0*  *      ��                  �  �  H*              HW�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �*             `*               ��   �*             �*               ��                  �*           ��                            ����                            submitValidation                                �+  �+      ��                  �  �  �+              ��*                        O   ����    e�          O   ����    R�          O   ����    ��            ��   ,             �+               ��                  ,           ��                            ����                            synchronizeProperties                               -  �,      ��                  �  �  ,-              �Ȃ                        O   ����    e�          O   ����    R�          O   ����    ��            ��   x-             D-               ��                  l-           ��                            ����                            transferToExcel                             l.  T.      ��                    	  �.              ��J                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �.             �.               ��   �.             �.               ��    /             �.               ��                  /           ��                            ����                            undoTransaction                             0  �/      ��                      ,0              ��v                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                              1  1      ��                      81              D�v                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �1             P1               ��                  x1           ��                            ����                            updateQueryPosition                             |2  d2      ��                      �2              Ċv                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �3  h3      ��                      �3              (�v                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �3           ��                            ����                            addRow          4      @4     �       CHARACTER,INPUT pcViewColList CHARACTER cancelRow    4      h4      �4   	 �       CHARACTER,  canNavigate t4      �4      �4    �       LOGICAL,    closeQuery  �4      �4      5   
 �       LOGICAL,    columnProps �4      5      <5    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   5      |5      �5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �5      �5      �5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   �5       6      L6   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   ,6      p6      �6   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    |6      �6      �6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   �6      (7      X7  
        CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow 87      �7      �7          LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    �7      �7      8          LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds �7      p8      �8    &      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  |8      �8      �8    2      CHARACTER,  hasForeignKeyChanged    �8      9      @9    I      LOGICAL,    openDataQuery    9      L9      |9    ^      LOGICAL,INPUT pcPosition CHARACTER  openQuery   \9      �9      �9   	 l      LOGICAL,    prepareQuery    �9      �9      :    v      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    �9      (:      X:    �      LOGICAL,INPUT pcDirection CHARACTER rowValues   8:      |:      �:   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �:      ;      0;   	 �      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   ;      p;      �;   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   |;      �;      <    �      CHARACTER,  assignDBRow                             �<  �<      ��                  �     �<               ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �<  
         ��                            ����                            bufferCopyDBToRO                                �=  �=      ��                      �=              �u�                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D>             >  
             �� 
  l>             8>  
             ��   �>             `>               ��                  �>           ��                            ����                            compareDBRow                                �?  p?      ��                  	  
  �?              $��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �@  x@      ��                      �@              Љ�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �@           ��                            ����                            dataAvailable                               �A  �A      ��                      �A              T��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            fetchDBRowForUpdate                             �B  �B      ��                      C              @�W                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              �C  �C      ��                      D              ��W                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �D  �D      ��                      E              ��H                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                                F  �E      ��                      F              0�H                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               G  �F      ��                     !  G              ��H                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              H  �G      ��                  #  %  ,H              ���                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 DH  
         ��                            ����                            initializeObject                                HI  0I      ��                  '  (  `I              ���                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                PJ  8J      ��                  *  ,  hJ              ���                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �J  
         ��                            ����                            releaseDBRow                                �K  hK      ��                  .  /  �K              xu�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �L  lL      ��                  1  2  �L              v�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �M  tM      ��                  4  7  �M              Hy�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �M             �M               ��                  �M           ��                            ����                            addQueryWhere   �;      LN      |N    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    \N      �N      O    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO �N      `O      �O    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   tO       P      4P          CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  P      pP      �P          CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �P      �P      �P    '      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �P      Q      PQ    6      CHARACTER,INPUT pcColumn CHARACTER  columnTable 0Q      tQ      �Q    K      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �Q      �Q      �Q     W      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �Q      R      LR  !  d      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  ,R      tR      �R  "  u      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �R      �R      �R  #  �      CHARACTER,INPUT iTable INTEGER  getDataColumns  �R      S      DS  $  �      CHARACTER,  getForeignValues    $S      PS      �S  %  �      CHARACTER,  getQueryPosition    dS      �S      �S  &  �      CHARACTER,  getQuerySort    �S      �S       T  '  �      CHARACTER,  getQueryString  �S      T      <T  (  �      CHARACTER,  getQueryWhere   T      HT      xT  )  �      CHARACTER,  getTargetProcedure  XT      �T      �T  *  �      HANDLE, indexInformation    �T      �T      �T  +        CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �T      PU      �U  ,        CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  dU      �U      V  -  #      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    �U      �V      �V  .  2      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �V      HW      xW  /  C      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  XW      �W      �W  0  Q      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �W      <X      lX  1  `      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    LX      �X      �X  2  p      LOGICAL,    removeQuerySelection    �X      �X      Y  3  �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   �X      LY      |Y  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  \Y      �Y      �Y  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �Y      �Y       Z  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition     Z      |Z      �Z  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Z      �Z      [  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString  �Z      $[      T[  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   4[      |[      �[  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �[      �[       \  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �\  �\      ��                  �  �  �\              ,��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �]  �]      ��                  �  �  �]              ���                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �^  �^      ��                  �  �  �^              T�E                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �_  �_      ��                  �  �  �_              l�E                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �`  �`      ��                  �  �   a              |�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �a  �a      ��                  �  �  b              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �b  �b      ��                  �  �  c              ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,c  
         ��                            ����                            startServerObject                               0d  d      ��                  �  �  Hd              l�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                8e   e      ��                  �  �  Pe              \��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  he           ��                            ����                            getAppService   �[      �e       f  <        CHARACTER,  getASBound  �e      f      8f  = 
       LOGICAL,    getAsDivision   f      Df      tf  >  $      CHARACTER,  getASHandle Tf      �f      �f  ?  2      HANDLE, getASHasStarted �f      �f      �f  @  >      LOGICAL,    getASInfo   �f      �f      g  A 	 N      CHARACTER,  getASInitializeOnRun    �f      (g      `g  B  X      LOGICAL,    getASUsePrompt  @g      lg      �g  C  m      LOGICAL,    getServerFileName   |g      �g      �g  D  |      CHARACTER,  getServerOperatingMode  �g      �g       h  E  �      CHARACTER,  runServerProcedure   h      ,h      `h  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   @h      �h      �h  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �h      �h      ,i  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle i      Pi      |i  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   \i      �i      �i  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �i      �i       j  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt   j      Dj      tj  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Tj      �j      �j  M        LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �j      �j      $k  N         LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �k  �k      ��                  �  �  �k              쁋                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Hl             l  
             ��   pl             <l               �� 
                 dl  
         ��                            ����                            addMessage                              `m  Hm      ��                  �  �  xm              �z�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �m             �m               ��   �m             �m               ��                  �m           ��                            ����                            adjustTabOrder                              �n  �n      ��                  �  �  �n              |��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Do             o  
             �� 
  lo             8o  
             ��                  `o           ��                            ����                            applyEntry                              \p  Dp      ��                  �  �  tp              �`                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            changeCursor                                �q  tq      ��                  �  �  �q              �`                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �q           ��                            ����                            createControls                              �r  �r      ��                  �  �  �r              |�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �s  �s      ��                  �  �  �s              l�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �t  �t      ��                  �  �  �t              d�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �u  �u      ��                  �  �  �u              h�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �v  �v      ��                  �  �  �v              �                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �w  �w      ��                  �  �  �w              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �x  �x      ��                  �  �  y              D�T                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �y  �y      ��                  �  �  z              d�T                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `z             ,z  
             ��   �z             Tz               ��   �z             |z               ��                  �z           ��                            ����                            modifyUserLinks                             �{  �{      ��                  �  �  �{              X��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   |             �{               ��   0|             �{               �� 
                 $|  
         ��                            ����                            removeAllLinks                              $}  }      ��                  �  �  <}              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              (~  ~      ��                  �  �  @~              ���                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �~             X~  
             ��   �~             �~               �� 
                 �~  
         ��                            ����                            repositionObject                                �  �      ��                  �  �  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            returnFocus                              �  �      ��                  �  �  �              P�                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 0�  
         ��                            ����                            showMessageProcedure                                8�   �      ��                  �     P�              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             h�               ��                  ��           ��                            ����                            toggleData                              ��  t�      ��                      ��              ��F                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ��           ��                            ����                            viewObject                              ��  ��      ��                      Є              ��F                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  k      (�      T�  O 
 �      LOGICAL,    assignLinkProperty  4�      `�      ��  P  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   t�      �      �  Q  �      CHARACTER,  getChildDataKey ��      (�      X�  R  �      CHARACTER,  getContainerHandle  8�      d�      ��  S  �      HANDLE, getContainerHidden  x�      ��      Ԇ  T  �      LOGICAL,    getContainerSource  ��      ��      �  U  �      HANDLE, getContainerSourceEvents    �      �      X�  V  �      CHARACTER,  getContainerType    8�      d�      ��  W        CHARACTER,  getDataLinksEnabled x�      ��      ؇  X  $      LOGICAL,    getDataSource   ��      �      �  Y  8      HANDLE, getDataSourceEvents �      �      P�  Z  F      CHARACTER,  getDataSourceNames  0�      \�      ��  [  Z      CHARACTER,  getDataTarget   p�      ��      ̈  \  m      CHARACTER,  getDataTargetEvents ��      ؈      �  ]  {      CHARACTER,  getDBAware  �      �      D�  ^ 
 �      LOGICAL,    getDesignDataObject $�      P�      ��  _  �      CHARACTER,  getDynamicObject    d�      ��      ĉ  `  �      LOGICAL,    getInstanceProperties   ��      Љ      �  a  �      CHARACTER,  getLogicalObjectName    �      �      L�  b  �      CHARACTER,  getLogicalVersion   ,�      X�      ��  c  �      CHARACTER,  getObjectHidden l�      ��      Ȋ  d  �      LOGICAL,    getObjectInitialized    ��      Ԋ      �  e        LOGICAL,    getObjectName   �      �      H�  f  !      CHARACTER,  getObjectPage   (�      T�      ��  g  /      INTEGER,    getObjectParent d�      ��      ��  h  =      HANDLE, getObjectVersion    ��      ȋ      ��  i  M      CHARACTER,  getObjectVersionNumber  ܋      �      @�  j  ^      CHARACTER,  getParentDataKey     �      L�      ��  k  u      CHARACTER,  getPassThroughLinks `�      ��      ��  l  �      CHARACTER,  getPhysicalObjectName   ��      ̌      �  m  �      CHARACTER,  getPhysicalVersion  �      �      D�  n  �      CHARACTER,  getPropertyDialog   $�      P�      ��  o  �      CHARACTER,  getQueryObject  d�      ��      ��  p  �      LOGICAL,    getRunAttribute ��      ̍      ��  q  �      CHARACTER,  getSupportedLinks   ܍      �      <�  r  �      CHARACTER,  getTranslatableProperties   �      H�      ��  s  	      CHARACTER,  getUIBMode  d�      ��      ��  t 
  	      CHARACTER,  getUserProperty ��      Ȏ      ��  u  +	      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ؎       �      X�  v  ;	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles 8�      ��      ��  w  P	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      Џ       �  x  \	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      <�      h�  y  i	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   H�      Ԑ      �  z  u	      CHARACTER,INPUT piMessage INTEGER   propertyType    �      (�      X�  {  �	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  8�      ��      ��  |  �	      CHARACTER,  setChildDataKey ��      ��      �  }  �	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ̑      �      H�  ~  �	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  (�      h�      ��    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    |�      ��      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ؒ      �      P�  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   0�      x�      ��  �  
      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ȓ      ��  �  
      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ܓ      $�      X�  �  $
      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   8�      ��      ��  �  7
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      Ԕ      �  �  E
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �      ,�      X�  � 
 Y
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject 8�      x�      ��  �  d
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      ԕ      �  �  x
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      $�      \�  �  �
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    <�      ��      ��  �  �
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      Ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      ,�      \�  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent <�      |�      ��  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      ̗       �  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      (�      \�  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks <�      ��      ��  �        LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ؘ      �  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      0�      d�  �  0      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute D�      ��      ��  �  C      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      ��      �  �  S      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      8�      t�  �  e      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  T�      ��      Ě  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      T�      ��  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   `�      ��      Л  � 	 �      CHARACTER,INPUT pcName CHARACTER    Ԟ      �  ��      �       4   �����                 ��                      ��                    K                  L�2                              �          ��  <�      �       4   �����                 L�                      ��                     J                  ��2                              ̜  P�    7  h�  �      �       4   �����                 ��                      ��                  C  E                  T�2                           C  x�         D                                  ,     
                    � ߱        |�  $  G  $�  ���                           $  I  ��  ���                       x                         � ߱        �    O  �  p�      �      4   �����                ��                      ��                  P  	                  �2                           P   �  ��  o   S      ,                                 �  $   T  ��  ���                       �  @         �              � ߱         �  �   U        4�  �   V  �      H�  �   X        \�  �   Z  x      p�  �   \  �      ��  �   ^  `      ��  �   _  �      ��  �   `        ��  �   c  �      Ԡ  �   e         �  �   f  |      ��  �   h  �      �  �   i  t      $�  �   j  �      8�  �   k  ,      L�  �   l  �      `�  �   r  �      t�  �   t  P	      ��  �   z  �	      ��  �   |   
      ��  �   ~  t
      ġ  �     �
      ء  �   �  l      �  �   �  �       �  �   �  \      �  �   �  �      (�  �   �  D      <�  �   �  �      P�  �   �  �      d�  �   �  0      x�  �   �  �      ��  �   �  �      ��  �   �        ��  �   �  X      Ȣ  �   �  �      ܢ  �   �        �  �   �  L      �  �   �  �      �  �   �  �      ,�  �   �         @�  �   �  <      T�  �   �  x      h�  �   �  �      |�  �   �  �          �   �  ,                      ��          �   �      ��                  ;	  i	  0�               3                        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        ؤ  $ O	  H�  ���                           O   g	  ��  ��  h               D�          4�  <�    $�                                             ��                            ����                                �;      ��      �     V     L�                       H�  �                     ��    �	  �  ��      t      4   ����t                ��                      ��                  �	  
                   ��                           �	  �  ��  �   �	  �      ��  �   �	  H      Ц  �   �	  �      �  �   �	  @      ��  �   �	  �      �  �   �	  8       �  �   �	  �      4�  �   �	  (      H�  �   �	  �      \�  �   �	         p�  �   �	  �      ��  �   �	        ��  �   �	  �          �   �	        �    @
  ȧ  H�      x      4   ����x                X�                      ��                  A
  �
                  ��                           A
  ا  l�  �   C
  �      ��  �   D
  T      ��  �   E
  �      ��  �   F
  D      ��  �   G
  �      Ш  �   H
  �      �  �   J
  p      ��  �   K
  �      �  �   L
  X       �  �   M
  �      4�  �   N
  �      H�  �   O
  D       \�  �   P
  �       p�  �   Q
  �       ��  �   R
  x!      ��  �   S
  �!      ��  �   T
  h"      ��  �   U
  �"      ԩ  �   V
  `#      �  �   W
  �#      ��  �   X
  X$      �  �   Y
  �$      $�  �   Z
  �$      8�  �   [
  L%      L�  �   \
  �%      `�  �   ]
  <&      t�  �   ^
  �&      ��  �   _
  4'      ��  �   `
  �'      ��  �   a
  ,(      Ī  �   b
  h(      ت  �   d
  �(      �  �   e
  X)       �  �   f
  �)      �  �   g
  *      (�  �   h
  �*      <�  �   i
  �*      P�  �   j
  l+      d�  �   k
  �+      x�  �   l
  \,      ��  �   m
  �,      ��  �   n
  L-      ��  �   o
  �-      ȫ  �   p
  <.      ܫ  �   q
  �.      �  �   r
  4/      �  �   s
  �/          �   t
  $0      ��    �
  4�  ��      T0      4   ����T0                Ĭ                      ��                  �
                    �«                           �
  D�  ج  �   �
  �0      �  �   �
  (1       �  �   �
  �1      �  �   �
  2      (�  �   �
  �2      <�  �   �
  3      P�  �   �
  |3      d�  �   �
  �3      x�  �   �
  t4      ��  �   �
  �4      ��  �   �
  l5      ��  �   �
  �5      ȭ  �   �
  d6      ܭ  �   �
  �6      �  �      L7      �  �     �7      �  �     <8      ,�  �     �8      @�  �     ,9      T�  �     �9      h�  �     :      |�  �     X:      ��  �     �:      ��  �   	  H;      ��  �   
  �;      ̮  �     8<      �  �     �<          �     (=      �    �  �  ��      �=      4   �����=  	              ��                      ��             	     �  .                  ,��                           �   �  ��  �   �  �=      ȯ  �   �  t>      ܯ  �   �  �>      �  �   �  l?      �  �   �  �?      �  �   �  \@      ,�  �   �  �@      @�  �   �  TA      T�  �   �  �A      h�  �   �  DB      |�  �   �  �B      ��  �   �  <C      ��  �   �  �C      ��  �   �  ,D      ̰  �   �  �D      �  �   �  $E      ��  �   �  �E      �  �   �  F      �  �   �  �F      0�  �   �  G      D�  �   �  �G      X�  �   �  �G      l�  �   �  8H      ��  �   �  �H      ��  �   �  0I      ��  �   �  �I      ��  �   �  (J      б  �   �  �J          �   �  K      getRowObjUpdStatic  deleteRecordStatic  ��    �  (�  8�      �K      4   �����K      /   �  d�     t�                          3   �����K            ��                      3   �����K  p�    �  ��  @�  ��  �K      4   �����K  
              P�                      ��             
     �  P                  �&{                           �  в  d�  �   �  4L      ��  $  �  ��  ���                       `L     
                    � ߱        г  �   �  �L      (�  $   �  ��  ���                       �L  @         �L              � ߱        �  $  �  T�  ���                       �L       	       	           � ߱        N     
                �N                     �O  @        
 �O              � ߱        t�  V     ��  ���                        �O       	       	       P       
       
       TP       	       	           � ߱        �  $    �  ���                       Q     
                �Q                     �R  @        
 �R              � ߱            V   1  ��  ���                                      h�                      ��                  R  �                  �'{                           R  0�  �R     
                hS                     �T  @        
 xT           U  @        
 �T          �U  @        
 @U          �U  @        
 �U              � ߱            V   g  ��  ���                        adm-clone-props ��  ��              �     W     l                          h  �                     start-super-proc    ��   �  �           �     X     (                          $  �                     �      ��  ��      lY      4   ����lY      /     ȸ     ظ                          3   ����|Y            ��                      3   �����Y  `�  $   "  4�  ���                       �Y                         � ߱         �    2  |�  ��  ��  �Y      4   �����Y                p�                      ��                  3  7                   ${                           3  ��  �Y                      Z                     Z                         � ߱            $  4  �  ���                             8  ��  ��      ,Z      4   ����,Z  LZ                         � ߱            $  9  Ⱥ  ���                       �    @  <�  L�  ��  `Z      4   ����`Z      $  A  x�  ���                       �Z                         � ߱            �   ^  �Z      �Z     
                P[                     �\  @        
 `\              � ߱        H�  V   r  ��  ���                        \�  �   �  �\      X�    $  x�  ��      �\      4   �����\      /   %  ��     ļ                          3   �����\            �                      3   ����]  <]     
                �]                     _  @        
 �^              � ߱        �  V   1  ��  ���                        T_     
                �_                      a  @        
 �`              � ߱        �  V   U  ��  ���                        ��    �  0�  ��      4a      4   ����4a                ��                      ��                  �  �                  ��&                           �  @�  ,�  /   �  �     ��                          3   ����Da            �                      3   ����da      /   �  X�     h�                          3   �����a            ��                      3   �����a  ��  /  B  Ŀ         �a                      3   �����a  initProps   �  Կ              4     Y     �                       �  �  	                                   �          ��  ��      ��                �  �  ��              h��                        O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p   �  �|  8�      �  8�  ��     �|                ��                      ��                  �  �                  ܟ�                           �  H�  ��  :  �                 $  �  �  ���                       �|                         � ߱        ��  ��     �|                                        ��                  �  �                  ԫ�                           �  H�  X�  H�      }                                        ��                  �                    ���                           �  ��  ��  ��     }                                        ��                    #                  d��                             h�  x�  h�     (}                                        ��                  $  @                  ��                           $  ��  �  ��     <}                                        ��                  A  ]                  d�                           A  ��  ��  ��     P}                                        ��                  ^  z                  ,�                           ^  �  (�  �     d}                                        ��                  {  �                  ��                           {  ��  ��  ��     x}  	                                      ��             	     �  �                  ��                           �  8�  H�  8�     �}  
                                      ��             
     �  �                  ��                           �  ��  ��  ��     �}                                        ��                  �  �                  ��                           �  X�  h�  X�     �}                                        ��                  �                    H�                           �  ��  ��  ��     �}                                        ��                    (                  �                             x�  ��  x�     �}                                        ��                  )  E                  ��                           )  �  �  �     �}                                        ��                  F  b                   (�                           F  ��  ��  ��     ~                                        ��                  c                    �(�                           c  (�  8�  (�     ~                                        ��                  �  �                  �)�                           �  ��      ��     ,~                                        ��                  �  �                  x*�                           �  H�      O   �  ��  ��  @~               L�          4�  @�   , �                                                       �     ��                            ����                            �  ��   �  <�      ��     Z     T�                      � P�  �                     ��    �  �  ��      L~      4   ����L~                ��                      ��                  �  �                  ,ī                           �  �  �  /   �  ��     ��                          3   ����\~            ��                      3   ����|~  t�  /   �  4�     D�                          3   �����~            d�                      3   �����~  ��  /   �  ��     ��                          3   �����~            ��                      3   �����~      /   �  �     �                          3   ����            <�                      3   ����0  �     
                �                     X�  @        
 �              � ߱        ��  V   6  L�  ���                        ��  $  P  �  ���                       l�                         � ߱        ��     
                (�                     x�  @        
 8�              � ߱        ��  V   Z  4�  ���                        ��  $  t  ��  ���                       ��     
                    � ߱        ��     
                �                     d�  @        
 $�              � ߱        ��  V   ~  �  ���                        h�  $  �  ��  ���                       p�     
                    � ߱        ��     
                 �                     P�  @        
 �              � ߱        ��  V   �  �  ���                        P�  $  �  ��  ���                       h�                         � ߱        ��     
                �                     \�  @        
 �              � ߱        |�  V   �  ��  ���                        ��  �   �  t�      L�  $  �  ��  ���                       ��     
                    � ߱        ��     
                $�                     t�  @        
 4�              � ߱        x�  V   �  ��  ���                        ��  $    ��  ���                       ��     
                    � ߱        ��  �      ��      <�  $  *  �  ���                       ԋ     
                    � ߱        P�  �   D  �      ��  $   f  |�  ���                       (�                         � ߱              q  ��  ��      D�      4   ����D�      /   r   �     �                          3   ����d�  @�     
   0�                      3   ������  p�        `�                      3   ������  ��        ��                      3   ������            ��                      3   ������  pushRowObjUpdTable  ��  ��  �                   [      �                                                    pushTableAndValidate    ��  @�  �           �     \     �                          �  $                     remoteCommit    X�  ��  �           t     ]                                �  o                     serverCommit    ��   �  �           p     ^     �                          �  |                                     D�          �  ��      ��                  �  �  ,�              �Y                        O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            0�  �      ��              _      \�                      
�     �                     disable_UI  ��  ��                      `      �                               �  
                   preTransactionValidate  �  `�                      a      x                              �                       �  �    ����  �       ��          ,�  8   ����   <�  8   ����   $�          L�  8   ����   \�  8   ����   l�  8   ����   |�  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  P�  \�      returnFocus ,INPUT hTarget HANDLE   @�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    t�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  4�  D�      removeAllLinks  ,   $�  X�  h�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE H�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  L�  X�      hideObject  ,   <�  l�  x�      exitObject  ,   \�  ��  ��      editInstanceProperties  ,   |�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��   �  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  <�  H�      applyEntry  ,INPUT pcField CHARACTER    ,�  t�  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER d�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  @�  H�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE 0�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  $�      disconnectObject    ,    �  8�  H�      destroyObject   ,   (�  \�  h�      bindServer  ,   L�  |�  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  l�  ��  ��      startFilter ,   ��  ��   �      releaseDBRow    ,   ��  �  $�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   �  P�  h�      filterContainerHandler  ,INPUT phFilterContainer HANDLE @�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��   �  �      compareDBRow    ,   ��  $�  8�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   �  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  ��  ��      updateState ,INPUT pcState CHARACTER    ��  �  0�      updateQueryPosition ,   �  D�  X�      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    4�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  h�      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   @�  ��  ��      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  (�  <�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  �  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  �  �      startServerObject   ,   ��  ,�  <�      setPropertyList ,INPUT pcProperties CHARACTER   �  l�  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    \�  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER t�  ��  ��      rowObjectState  ,INPUT pcState CHARACTER    ��  �  �      retrieveFilter  ,   ��  (�  <�      restartServerObject ,   �  P�  `�      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   @�  X�  d�      refreshRow  ,   H�  x�  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  h�  ��  ��      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  ��  ,�  D�      initializeServerObject  ,   �  X�  l�      initializeObject    ,   H�  ��  ��      home    ,   p�  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  ��  ��      fetchPrev   ,   ��  ��  �      fetchNext   ,   ��  �  $�      fetchLast   ,   �  8�  D�      fetchFirst  ,   (�  X�  d�      fetchBatch  ,INPUT plForwards LOGICAL   H�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  ��  ��      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  @�  P�      dataAvailable   ,INPUT pcRelative CHARACTER 0�  |�  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE l�  ��  ��      commitTransaction   ,   ��  ��  �      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    ��  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
   %     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� O   P   %               � 
" 
   
   %              h �P  \         (          
�                          
�            � �   `
" 
   
   
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
   �               1� �  
   � �   � %               o%   o           � �    O
"   
   �           �    1� �     � �   � %               o%   o           � �   O
"   
   �           �    1� �  
   � �   � %               o%   o           � �   O
"   
   �           l    1�      � �   � %               o%   o           � �    O
"   
   �           �    1�      � �   � %               o%   o           �    O
"   
   �           T    1� 3     � ?   � %               o%   o           %               
"   
   �          �    1� G     � W     
"   
   �               1� ^     � �   � %               o%   o           � q  O
"   
   �           �    1� s     � �   � %               o%   o           � �  S O
"   
   �           �    1� �     � ?   � %               o%   o           %               
"   
   �           p    1� �     � ?   � %               o%   o           %               
"   
   �           �    1� �     � ?   � %               o%   o           %              
"   
   �          h    1�      � ?     
"   
   �           �    1�   
   � ?   � %               o%   o           %               
"   
   �                1�      � �   � %               o%   o           � �    O
"   
   �          �    1� '     � W     
"   
   �           �    1� 7     � �   � %               o%   o           � M  t O
"   
   �          D	    1� �  
   � W     
"   
   �           �	    1� �     � �   � %               o%   o           � �  � O
"   
   �           �	    1� k     � �   � %               o%   o           � �    O
"   
   �           h
    1� �  
   � �   � %               o%   o           %               
"   
   �           �
    1� �     � ?   � %               o%   o           %              
"   
   �           `    1� �     � �   � %               o%   o           � �    �
"   
   �           �    1� �     � �   � %               o%   o           o%   o           
"   
   �           P    1� �  
   � �   � %               o%   o           � �    2
"   
   �           �    1� �     � �  	 � %               o%   o           � �  / �
"   
   �          8    1�      � �  	   
"   
   �           t    1� "     � �  	 � o%   o           o%   o           � �    �
"   
   �          �    1� 5     � �  	   
"   
   �           $    1� D     � �  	 � o%   o           o%   o           � �    �
"   
   �          �    1� T     � ?     
"   
   �          �    1� b     � �  	   
"   
   �              1� o     � �  	   
"   
   �          L    1� |     � �  	   
"   
   �           �    1� �     � ?   � o%   o           o%   o           %              
"   
   �              1� �     � �  	   
"   
   �          @    1� �  
   � �     
"   
   �          |    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          0    1� �     � �  	   
"   
   �          l    1�   	   � �  	   
"   
   �          �    1�      � �  	   
"   
   �          �    1�      � �  	   
"   
   �                1� 6     � �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    �� B   � P   �        �    �@    
� @  , 
�            �� K     p�               �L
�    %              � 8          � $         � R          
�    � l     
"   
   � @  , 
�           �� �  
   p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� o  
   � �   � %               o%   o           � �    �
"   
   �           <    1� z  
   � �   � %               o%   o           o%   o           
"   
   �           �    1� �     � W   � %               o%   o           o%   o           
"   
   �           4    1� �     � ?   � %               o%   o           %               
"   
   �           �    1� �     � ?   � %               o%   o           %               
"   
   �           ,    1� �     � �   � %               o%   o           � �    �
"   
   �           �    1� �     � ?   � %               o%   o           %              
"   
   �               1� �     � ?   � %               o%   o           o%   o           
"   
   �           �    1� �     � �   � %               o%   o           o%   o           
"   
   �               1� �  	   � �   � %               o%   o           � �    �
"   
   �           �    1� �     � �   � %               o%   o           o%   o           
"   
   �               1� �     � �   � %               o%   o           o%   o           
"   
   �           �    1� 
     � ?   � %               o%   o           %               
"   
   �           �    1�      � ?   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� &  
   � ?   � %               o%   o           %              
"   
   �           H    1� 1     � �   � %               o%   o           o%   o           
"   
   �           �    1� =     � �   � %               o%   o           � �    2
"   
   �           8    1� K     � �   � %               o%   o           o%   o           
"   
   �          �    1� W     � W     
"   
   �           �    1� d     � �   � %               o%   o           � w  ! �
"   
   �           d    1� �     � �   � %               o%   o           � �    �
"   
   �           �    1� �     � �   � %               o%   o           � �   � 
"   
   �          L    1� �     � �     
"   
   �          �    1� �     � W     
"   
   �           �    1� �     � �   � %               o%   o           � �    �
"   
   �          8     1� �  
   � W     
"   
   �           t     1�      � ?   � %               o%   o           o%   o           
"   
   �           �     1�      � ?   � %               o%   o           %               
"   
   �           l!    1� !     � ?   � %               o%   o           %               
"   
   �           �!    1� 2     � �   � %               o%   o           � �    2
"   
   �           \"    1� B     � �   � %               o%   o           o%   o           
"   
   �           �"    1� N     � ?   � %               o%   o           %              
"   
   �           T#    1� _     � ?   � %               o%   o           %               
"   
   �           �#    1� l     � ?   � %               o%   o           %               
"   
   �          L$    1� |     � W     
"   
   �          �$    1� �     � �     
"   
   �           �$    1� �     � �   � %               o%   o           o%   o           
"   
   �           @%    1� �     � �   � %               o%   o           � �    �
"   
   �           �%    1� �     � �   � %               o%   o           o%   o           
"   
   �           0&    1� �     � ?   � o%   o           o%   o           o%   o           
"   
   �           �&    1� �     � �  	 � %               o%   o           o%   o           
"   
   �           ('    1� �     � �   � %               o%   o           o%   o           
"   
   �           �'    1� �  
   � �   � %               o%   o           o%   o           
"   
   �           (    1� �     � �     
"   
   �           \(    1�      � �   � %               o%   o           � $  4 �
"   
   �           �(    1� Y  
   � ?   � %               o%   o           %              
"   
   �          L)    1� d     � W     
"   
   �           �)    1� u     � �   � %               o%   o           � �    �
"   
   �           �)    1� �     � ?   � %               o%   o           %              
"   
   �           x*    1� �     � �   � %               o%   o           � �    � 
"   
   �           �*    1� �     � �   � %               o%   o           � �    �
"   
   �           `+    1� �     � �   � %               o%   o           � �    �
"   
   �           �+    1� �     � ?   � %               o%   o           %               
"   
   �           P,    1� �  	   � W   � %               o%   o           o%   o           
"   
   �           �,    1� �     � �   � %               o%   o           � �  	 �
"   
   �           @-    1� �     � �   � %               o%   o           %       �       
"   
   �           �-    1� �     � �   � %               o%   o           � �    �
"   
   �           0.    1� �     � ?   � o%   o           o%   o           %              
"   
   �           �.    1�      � ?   � %               o%   o           %               
"   
   �           (/    1� '     � �   � %               o%   o           o%   o           
"   
   �           �/    1� 8     � �  	 � %               o%   o           � �    �
"   
   �          0    1� I     � �  	   P �L 
�H T   %              �     }        �GG %              
"   
   �           �0    1� V  
   � �   � %               o%   o           � �    2
"   
   �           1    1� a     � ?   � %               o%   o           %               
"   
   �           �1    1� n  	   � �   � %               o%   o           � �    �
"   
   �           2    1� x     � �   � %               o%   o           � �    2
"   
   �           �2    1� �     � ?   � %               o%   o           %               
"   
   �           �2    1� �     � �   � %               o%   o           � �    �
"   
   �           p3    1� �     � �   � %               o%   o           o%   o           
"   
   �           �3    1� �     � �   � %               o%   o           o%   o           
"   
   �           h4    1� �     � ?   � %               o%   o           o%   o           
"   
   �           �4    1� �     � ?   � %               o%   o           o%   o           
"   
   �           `5    1� �     � ?   � %               o%   o           o%   o           
"   
   �           �5    1� �     � �   � %               o%   o           o%   o           
"   
   �           X6    1� �  	   � �  	 � %               o%   o           � �    �
"   
   �           �6    1�   
   � �  	 � %               o%   o           � �    �
"   
   �           @7    1�      � �   � %               o%   o           � �    2
"   
   �           �7    1�       � �   � %               o%   o           o%   o           
"   
   �           08    1� .     � �   � %               o%   o           o%   o           
"   
   �           �8    1� ;     � �   � %               o%   o           � �    �
"   
   �            9    1� P     � �   � %               o%   o           � �    �
"   
   �           �9    1� _     � �  	 � %               o%   o           o%   o           
"   
   �          :    1� q     � W     
"   
   �           L:    1� }     � �   � %               o%   o           � �    �
"   
   �           �:    1� �     � �   � %               o%   o           o%   o           
"   
   �           <;    1� �     � ?   � %               o%   o           o%   o           
"   
   �           �;    1� �  
   � �   � %               o%   o           � �    2
"   
   �           ,<    1� �     � �   � %               o%   o           � �    �
"   
   �           �<    1� �     � ?   � %               o%   o           %               
"   
   �           =    1� �     � �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �=    1� �  	   � W   � %               o%   o           o%   o           
"   
   �           h>    1�       � W   � %               o%   o           o%   o           
"   
   �           �>    1�      � W   � %               o%   o           o%   o           
"   
   �           `?    1�      � ?   � %               o%   o           %              
"   
   �           �?    1� 2     � �   � %               o%   o           � K  M �
"   
   �           P@    1� �     � ?   � %               o%   o           %              
"   
   �           �@    1� �     � ?   � %               o%   o           %               
"   
   �           HA    1� �     � ?   � %               o%   o           %               
"   
   �           �A    1� �     � �  	 � %               o%   o           � �  3 2
"   
   �           8B    1�      � ?   � %               o%   o           %               
"   
   �           �B    1� &     � �  	 � %               o%   o           o%   o           
"   
   �           0C    1� 3     � ?   � o%   o           o%   o           %              
"   
   �           �C    1� C     � �  	 � o%   o           o%   o           � �    �
"   
   �            D    1� V     � W   � o%   o           o%   o           o%   o           
"   
   �           �D    1� f     � W   � o%   o           o%   o           o%   o           
"   
   �           E    1� v     � �  	 � o%   o           o%   o           o%   o           
"   
   �           �E    1� �     � W   � o%   o           o%   o           o%   o           
"   
   �           F    1� �     � �  	 � o%   o           o%   o           � �   � 
"   
   �           �F    1� �     � �  	 � o%   o           o%   o           � �   �
"   
   �           �F    1� �     � ?   � %               o%   o           %               
"   
   �           tG    1� �     � ?   � %               o%   o           %               
"   
   �          �G    1� �     � �  	   
"   
   �           ,H    1� �     � ?   � %               o%   o           %               
"   
   �           �H    1�      � �   � %               o%   o           o%   o           
"   
   �           $I    1�      � �   � %               o%   o           o%   o           
"   
   �           �I    1� 0     � ?   � %               o%   o           o%   o           
"   
   �           J    1� B     � �   � %               o%   o           � �    � 
"   
   �           �J    1� Q     � _   � %               o%   o           %               
"   
   �           K    1� g  	   � ?   � %               o%   o           %                "      %     start-super-proc �� %     adm2/smart.p N`P �L 
�H T   %              �     }        �GG %              
"   
   �       (L    6� B     
"   
   
�        TL    8
"   
   �        tL    ��     }        �G 4              
"   
   G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        XN    �� B   � P   �        dN    �@    
� @  , 
�       pN    �� K     p�               �L
�    %              � 8      |N    � $         � R          
�    � l   `
"   
   p� @  , 
�       �O    �� ^     p�               �L"  	    �   � �   �� �   � �     }        �A      |    "  	    � �   �%              (<   \ (    |    �     }        �A� �   �A"  
        "  	    "  
      < "  	    "  
    (    |    �     }        �A� �   �A"  
    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        `Q    �� B   � P   �        lQ    �@    
� @  , 
�       xQ    �� K     p�               �L
�    %              � 8      �Q    � $         � R          
�    � l   `
"   
   p� @  , 
�       �R    �� �  
   p�               �L"  	    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        8S    �� B   � P   �        DS    �@    
� @  , 
�       PS    �� K     p�               �L
�    %              � 8      \S    � $         � R          
�    � l     
"   
   p� @  , 
�       lT    �� �  
   p�               �L%     SmartDataObject 
"   
   p� @  , 
�       �T    ��      p�               �L%               
"   
   p� @  , 
�       4U    �� D     p�               �L%               
"   
   p� @  , 
�       �U    �� "     p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
    (   � 
"   
       �        tV    �� B   �
"   
   � 8      �V    � $         � R          
�    � l   `
"   
   �        W    �
"   
   �       8W    /
"   
   
"   
   �       dW    6� B     
"   
   
�        �W    8
"   
   �        �W    �
"   
   �       �W    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
   G %              G %              
�     }        �
"   
    (   � 
"   
       �        �X    �A"      
"   
   
�        �X    �@ � 
"   
   "      �       }        �
"   
   %              %                "      %     start-super-proc �� %     adm2/appserver.p ���    � K     
�    �     }        �%               %      Server  - �     }        �    "      � �    � %                   "      � �    � %      NONE    p�,  8         $     "              � e   `
�    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �         [    �� B   � P   �        ,[    �@    
� @  , 
�       8[    �� K     p�               �L
�    %              � 8      D[    � $         � R          
�    � l   `
"   
   p� @  , 
�       T\    �� �     p�               �L"      p�,  8         $     "              � s   `
�     "      %     start-super-proc �� %     adm2/dataquery.p ��
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �]    �� B   � P   �        �]    �@    
� @  , 
�       �]    �� K     p�               �L
�    %              � 8      �]    � $         � R   `     
�    � l   `
"   
   p� @  , 
�       �^    �� �     p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �_    �� B   � P   �        �_    �@    
� @  , 
�       �_    �� K     p�               �L
�    %              � 8      �_    � $         � R   `     
�    � l   `
"   
   p� @  , 
�       �`    �� 3     p�               �L%               "      %     start-super-proc �� %     adm2/query.p N`%     start-super-proc �� %     adm2/queryext.p % 	    initProps `
�    %< 3 ,   FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION �   � �     � �     � !      
�     	         �G
"   
   �        \b    �G
"   
   
"   
    x    (0 4      �        |b    �G%                   �        �b    �GG %              � �    `� �         %              %                   "      %              
"   
       "      �        xc    �
"   
   �        �c    �
"   
   
�       �c    �"       \      H   "      ((       "      %              � �      � �   `     
"   
   
"   
    \      H   "      ((       "      %              � �     � �   W�        pd    �%                   %              %                   "  (    %                  "  (        
"   
   
"   
   0 T       m � "  (    �        |e    �A @   "       $         � "  (    � �   � �        �e    �� "  (    
"   
    \ H     H   "      ((       "      %              � �    � � �     (        "  !    � �    W�        0f    �"  !    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        4g    �� B   � P   �        @g    �@    
� @  , 
�       Lg    �� K     p�               �L
�    %              � 8      Xg    � $         � R          
�    � l     
"   
   p� @  , 
�       hh    ��      p�               �L%               
"   
   p� @  , 
�       �h    �� =     p�               �L"      �,  8         $     "              � �  
  
�    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �i    �� B   � P   �        �i    �@    
� @  , 
�       �i    �� K     p�               �L
�    %              � 8      �i    � $         � R   `     
�    � l     
"   
   p� @  , 
�       �j    �� q     p�               �L
"   
   
"   
   p� @  , 
�       8k    �� P     p�               �L"      
"   
   p� @  , 
�       �k    �� �     p�               �L"          "      � �    � %T J D   OPEN QUERY Query-Main FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION.     "      � ,    N((        "      %                   "      � 2     "       (   "           "      %              @ �,  8         $     "              � >    
�    p�,  8         $     � K   W        � M   `
�    %               �    "      � �         %              %                   "      %                  "      "      "      T(        "      %              "      � �   � "      �       "      �    "      � �   � � �      � �   `�    "      � �    S    "      "          "      %                � @    �     t T     P   4         "      (0       4         "      � �      � �    `� �   WT ,  %              T   "      "      � �     � �   `� �   WT    �    "      � �   � "      � �   `"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "      %              � �    � � Y     4         "      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        r    �� B   � P   �        r    �@    
� @  , 
�       $r    �� K     p�               �L
�    %              � 8      0r    � $         � R          
�    � l   `
"   
   p� @  , 
�       @s    �� V  
   p�               �L"            "  
    �    � [  4 W� �   �       "  	    �    � [  4 � � �   W�   � �     � �     � [  4 `�   � �     � �   `� [  4 W�   � �     � �     � [  4   
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �t    �� B   � P   �        �t    �@    
� @  , 
�       �t    �� K     p�               �L
�    %              � 8      �t    � $         � R          
�    � l     
"   
   p� @  , 
�       v    �� �     p�               �L"      
"   
   p� @  , 
�       \v    �� �     p�               �L"      
"   
   p� @  , 
�       �v    �� �     p�               �L"          %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � [  4   � �         "  	    �     "      T    "      "      @ A,    �   � �   � � Y     "      "       T      @   "      (        "      � �    `� �      � �   `"           "  	    %              D H   @ A,    �   � �   `� Y     "      "      ,    S   "      � [  4 I� �   � %                T      @   "      (        "      � �    `� �      � �   `"           "  
    %                         "      � Y     "                 "      � Y   `"      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �z    �� B   � P   �        �z    �@    
� @  , 
�       �z    �� K     p�               �L
�    %              � 8      �z    � $         � R   `     
�    � l   � 
"   
   p� @  , 
�       |    �� �     p�               �L"      
"   
   p� @  , 
�       \|    �� �     p�               �L"      "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "      %     start-super-proc �� %     adm2/data.p %     start-super-proc �� %     adm2/dataext.p %     start-super-proc �� %     adm2/dataextcols.p %     start-super-proc �� %     adm2/dataextapi.p 2%              %              %              
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    �� B   � P   �        �    �@    
� @  , 
�       �    �� K     p�               �L
�    %              � 8      �    � $         � R   `     
�    � l   `
"   
   p� @  , 
�       �    �� �     p�               �L%               %< 3 ,   "\\192.168.101.9\desarrollo\prg\dPer_Facturacion.i" 
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ��    �� B   � P   �        �    �@    
� @  , 
�       �    �� K     p�               �L
�    %              � 8      �    � $         � R          
�    � l   `
"   
   p� @  , 
�       ,�    �� �     p�               �L"      
�     	        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    �� B   � P   �        ��    �@    
� @  , 
�       ��    �� K     p�               �L
�    %              � 8      �    � $         � R          
�    � l   `
"   
   p� @  , 
�       �    �� �  
   p�               �L
"   
   
�     
        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        Ѕ    �� B   � P   �        ܅    �@    
� @  , 
�       �    �� K     p�               �L
�    %              � 8      �    � $         � R          
�    � l   `
"   
   p� @  , 
�       �    �� �  	   p�               �L
"   
   
"   
        � �  	   �        \�    �
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        ܇    �� B   � P   �        �    �@    
� @  , 
�       �    �� K     p�               �L
�    %              � 8       �    � $         � R          
�    � l   `
"   
   p� @  , 
�       �    �� 2     p�               �L"      
"   
   �       h�    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    �� B   � P   �         �    �@    
� @  , 
�       �    �� K     p�               �L
�    %              � 8      �    � $         � R          
�    � l   `
"   
   p� @  , 
�       (�    �� �  	   p�               �L
"   
   
�             �Gp�,  8         $     
"   
           � �   `
�    
�             �Gp�,  8         $     
"   
           � �   `
�    �    � �     
�        "      � �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � X     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�    %      SUPER   *         "      %              � �   �%                   S    "  	    &    &         S    "  	    � �     %               %              %                  "      "          "      &    &    &    $        %              &    &     *    � �  4        S    "  	    � �     %                   "      %              � �  A   � (   x (   H (             "      "          "      "          "      %                  "      %                  "      %              � *   9   "          "      &    H               "      "          "      "          "      "      � *   9        S    "  	    � d      %               o%   o           o%   o           o%   o           "      "      "          "      "          "      "          "      &       "      &       "      &   �    �    �    t    d    @            "      &        "      &        "      &    &    &    & 	   & 
   &    *    � h   G   "      %                              �           �   p       ��                 _  �  �               �W                        O   ����    e�          O   ����    R�          O   ����    ��        $  n  �   ���                       (V     
                    � ߱              o  ,  �      �V      4   �����V                �                      ��                  p  �                  ��B                           p  <  �  �  q  �V            s  �  l      $W      4   ����$W                |                      ��                  t  �                  0�B                           t  �  �  o   u      ,                                 �  �   v  DW      �  �   w  pW      0  $  x    ���                       �W     
                    � ߱        D  �   y  �W      X  �   z  �W      l  �   }  �W          $   �  �  ���                       ,X  @         X              � ߱                     `          8  L   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   p       ��                 �  �  �               ��B                        O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �X     
                    � ߱                  �  �                      ��                   �  �                  �&                          �  8      4   �����X      $  �  �  ���                       �X     
                    � ߱        �    �  <  L       Y      4   ���� Y      /  �  x                               3   ����Y  �  �   �   Y          O   �  ��  ��  XY                               , �                          
                               �      ��                            ����                                            �           �   p       ��            	     N  �  �               D{                        O   ����    e�          O   ����    R�          O   ����    ��        $  n  �   ���                       �a                         � ߱        �  $  o  <  ���                       b                         � ߱        Hb     
                �b  @         hb              � ߱        L  $   �  h  ���                         \      �  �                      ��        0         �  �                  t�H      �c         `     �  �      $  �  �  ���                        c                         � ߱          $  �  �  ���                       Pc                         � ߱            4   �����c  �c     
                �c                     |d                         � ߱          $  �    ���                                     ,                      ��                  �  �                  d I                    �     �  �  �  $  �  X  ���                       �d       !       !           � ߱                \  �                      ��        0         �  �                  TI     ( xe                �  �      $  �  0  ���                       e       (       (           � ߱        �  $  �  �  ���                       8e       (       (           � ߱            4   ����`e        �  �  `      �e      4   �����e                p                      ��                  �  �                  �I                           �  �  �  $  �  �  ���                        f       !       !           � ߱            O   �  �� ��          $  �    ���                       <f                         � ߱        �f     
                dg                     �h  @        
 th          i  @        
 �h           i                     `i     
                �i                     ,k  @        
 �j          �k  @        
 Dk          �k  @        
 �k              � ߱        �  V   �  8  ���                        d	    �  �  8	      �k      4   �����k  l                     `l                     �l                     �l                         � ߱            $  �  �  ���                       �	    �  �	  �	      m      4   ����m      �   �  \m      �	  $  �  �	  ���                       �m                         � ߱        �
  $  �  (
  ���                       �m                         � ߱          �
      ,  0                      ��        0         �  �                  �I      Hn         �     �  T
      $  �     ���                       �m                         � ߱        �  $  �  X  ���                        n                         � ߱            4   ����(n  Tn                     �n                     �n                     �n                     o                         � ߱        \  $  �  �  ���                             �  x  �      8o      4   ����8o      $  �  �  ���                       `o          �p             � ߱        �  $  �    ���                       �p                         � ߱          �        x                      ��        0         �  �                  �I      ,q         4     �  8      $  �  �  ���                       �p                         � ߱        h  $  �  <  ���                       �p                         � ߱            4   ����q      $  �  �  ���                       @q                         � ߱        �q     
                <r                     �s  @        
 Ls              � ߱        �  V   �  �  ���                        �s       
       
       �s       	       	        t                     ,t                         � ߱          $    `  ���                          $  �  8  ���                       Xt                         � ߱        �t     
                 u                     Pv  @        
 v          �v  @        
 hv           w  @        
 �v              � ߱        �  V   �  d  ���                          �        |                      ��        0    	     ,  A                  ���      �w         \     ,  ,      $  ,  �  ���                       w                         � ߱        \  $  ,  0  ���                       <w                         � ߱        l  4   ����dw      4   �����w  �  $  1  �  ���                       x                         � ߱        �    3  �  p      $x      4   ����$x                �                      ��                  4  8                  ���                           4     hx                     �x       	       	           � ߱            $  5  �  ���                             :    �      �x      4   �����x  	              �                      ��             	     <  @                  ��                           <     �y                     �y       
       
           � ߱            $  =  �  ���                       z                     Pz                         � ߱          $  G    ���                       �z     
                 {                     P|  @        
 |          �|  @        
 h|              � ߱            V   U  �  ���                                    J �          �  �  � Xh                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            �                          v�                                �   p       ��                      �               ̺�                        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   p       ��                    (  �               4��                        O   ����    e�          O   ����    R�          O   ����    ��             �              �                  $                  h  /  %  (     8  �                      3   ����،            X                      3   ������      O   &  ��  ��  �               �          �  �    �                                             ��                            ����                                            L          �   p       ��                  2  ]  �               ���                        O   ����    e�          O   ����    R�          O   ����    ��      9       �              �                $                  C       0             �          N                      $         �  /  Q  x     �  0�                      3   �����            �                      3   ����8�    /  S  �     �  `�                      3   ����D�  |          $                  3   ����h�      $   S  P  ���                                                   � ߱                  �  �                  3   ����t�      $   S  �  ���                                                   � ߱        \  $   W  0  ���                       ��                         � ߱            O   [  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                            $          �   p       ��                  g  �  �               ���                        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  C                    �          N                      �              /  �  P     `  ̍                      3   ������  �        �  �                  3   ����ԍ      $   �  �  ���                                                   � ߱                                      3   ������      $   �  D  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   p       ��                  �  �  �               �A:                        O   ����    e�          O   ����    R�          O   ����    ��            �  �   �        �      4   ���� �      �   �  �    ��                            ����                                            �           �   p       ���               �  /  �               �B:                        O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   �����  �  B   �      D   ��                                                                 �  �                                   @            `   p    �    �  �  �      0�      4   ����0�  8�                         � ߱            $  �  �  ���                             �      L                  ��                  �  .  4              ��                           �  $      �          ��                            7   ����         ��               ��    �            p                  6   �       �   ��         �  ��    �            p                                                        `�   l�                   �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��             h  �      ��      4   ������                �                      ��                                       �f:                              x  �  B         h   ��         H  \�                                         �   ��   �   0�                   �  �           L�           T�         �            �   �            �  �      ��      4   ������      O     ��  ��  ��   
      $  �      ��      4   ������                �                      ��                                      �j:                             4        	  �  �  h  �      4   �����      O   
  ��  ��  �                x                      ��                                       k:                             �  ,      �  �      �      4   �����      O     ��  ��  �        <      �	          �	  �	      ��                      �	              �k:                             �      h  �       ��                            7   ����          ��               �    �            	                  6           8	   ��         ,	  �    �            	                                                        ��                 �	  t	                                   @            T	   d	        O   ����  e�          O   ����  R�          O   ����  ��              �	  
      $�      4   ����$�      O     ��  ��  ��          <
  �
      ��      4   ������                �
                      ��                    ,                  �n:                             L
  �  B         d   ��           В                                         ܑ   �   �   �   $�   0�   <�   \�  	 |�  
 ��   ��                 �  �                                   @            �   �          '  �  �  L  ��      4   ������      O   (  ��  ��  ��  ��                     ��                         � ߱            $  *     �                         ��                             ��                             ��                            ����                                       TXS appSrvUtils BPer_Facturacion Per_Facturacion viPer_Factura Per_Facturacion \\192.168.101.9\desarrollo\prg\dPer_Facturacion.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource WordIndexedFields RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "\\192.168.101.9\desarrollo\prg\dPer_Facturacion.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH Per_Facturacion NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; Per_Factura Fec_Inicial Fec_Final Fec_LimPago Estado Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p Per_Factura Fec_Inicial Fec_Final Fec_LimPago Estado RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI A,C,D,U D �nicamente Permite Eliminar �ltimo Per�odo Futuro(3) U No Es Posible Actualizar Las Fechas de facturaci�n Del Periodo... No Es Posible Actualizar Las Fechas. Revise las Fechas... A,C Revisar Las Fechas del Nuevo Per�odo de Facturaci�n.Intente de Nuevo... PRETRANSACTIONVALIDATE qDataQuery idx_Per idx_Est �  X0     �>      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   O	  g	  i	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props n  o  p  q  s  t  u  v  w  x  y  z  }  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   n  o  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  ,  1  3  4  5  8  :  <  =  @  A  G  U  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �      #  $  @  A  ]  ^  z  {  �  �  �  �  �  �  �  �      (  )  E  F  b  c    �  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable    �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate    %  &  (  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    Q  S  W  [  ]  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    �  �  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI  �  �  �  X     a               @                  preTransactionValidate  �  �  �  �  �               	  
                        '  (  *  ,  .  /    �"             H"                             
   RowObject   �         �         �         �         �         �         �         �         �         �         Per_Factura Fec_Inicial Fec_Final   Fec_LimPago Estado  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp            RowObjUpd   �         �         �         �         �         �         �         �         �                            Per_Factura Fec_Inicial Fec_Final   Fec_LimPago Estado  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   <          0  
   appSrvUtils `       P     viPer_Factura   �       t     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager    $          
   gshSecurityManager  L  	 	     8  
   gshProfileManager   x  
 
     `  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId                  gsdSessionObj   4        $  
   gshFinManager   X        H  
   gshGenManager   |        l  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj             �     gsdSessionScopeObj  ,        $   
   ghProp  L        @   
   ghADMProps  p        `   
   ghADMPropsBuf   �        �      glADMLoadFromRepos  �        �      glADMOk �        �   
   ghContainer �     	   �      cObjectName !    
   !     iStart  0!       $!     cAppService P!       D!     cASDivision |!       d!     cServerOperatingMode    �!       �!     cContainerType  �!       �!     cQueryString    �!       �!  
   hRowObject  "       �!  
   hDataQuery  $"       "     cColumns             8"     cDataFieldDefs  l"     C  X"  BPer_Facturacion    �"       |"  Per_Facturacion �"    X  �"  RowObject         X  �"  RowObjUpd            9   �   �   �   �            7  C  D  E  G  I  J  K  O  P  S  T  U  V  X  Z  \  ^  _  `  c  e  f  h  i  j  k  l  r  t  z  |  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  @
  A
  C
  D
  E
  F
  G
  H
  J
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  d
  e
  f
  g
  h
  i
  j
  k
  l
  m
  n
  o
  p
  q
  r
  s
  t
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
                     	  
          �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  .  �  �  �  �  �  �  �  �  �      1  P  R  g  �      "  2  3  4  7  8  9  @  A  ^  r  �  $  %  1  U  �  �  �  �  �  B  �  �  �  �  �  �  �  6  P  Z  t  ~  �  �  �  �  �  �  �       *  D  f  q  r      ��  c:\progress10\src\adm2\data.i    �&  �) . c:\progress10\src\adm2\custom\datacustom.i   '  �� - c:\progress10\src\adm2\robjflds.i    T'  �^  , \\192.168.101.9\desarrollo\prg\dPer_Facturacion.i    �'  �:  c:\progress10\src\adm2\query.i   �'  z + c:\progress10\src\adm2\delrecst.i    �'  `W * c:\progress10\src\adm2\tblprep.i  (  F� ) c:\progress10\gui\fnarg  L(   ( c:\progress10\src\adm2\custom\querycustom.i  p(  �   c:\progress10\src\adm2\dataquery.i   �(  �Z ' c:\progress10\src\adm2\custom\dataquerycustom.i  �(  �< ! c:\progress10\src\adm2\appserver.i   )  �� & c:\progress10\src\adm2\custom\appservercustom.i  D)  I� " c:\progress10\src\adm2\smart.i   �)  Ds % c:\progress10\gui\fn �)  tw $ c:\progress10\src\adm2\custom\smartcustom.i  �)  Q. # c:\progress10\gui\set    *  �>  c:\progress10\src\adm2\dataprop.i    (*  ��  c:\progress10\src\adm2\custom\datapropcustom.i   X*  ��  c:\progress10\src\adm2\custom\dataprtocustom.i   �*  YO  c:\progress10\src\adm2\qryprop.i �*  -�  c:\progress10\src\adm2\custom\qrypropcustom.i    �*  ��  c:\progress10\src\adm2\custom\qryprtocustom.i    8+   	 c:\progress10\src\adm2\dataqueryprop.i   t+  �d  c:\progress10\src\adm2\custom\dataquerypropcustom.i  �+  ��  c:\progress10\src\adm2\custom\dataqueryprtocustom.i  �+  �l  c:\progress10\src\adm2\appsprop.i    (,  ɏ  c:\progress10\src\adm2\custom\appspropcustom.i   X,  V  c:\progress10\src\adm2\custom\appsprtocustom.i   �,  i$  c:\progress10\src\adm2\smrtprop.i    �,  �j  c:\progress10\gui\get     -  �  c:\progress10\src\adm2\custom\smrtpropcustom.i   $-  ��  c:\progress10\src\adm2\custom\smrtprtocustom.i   `-  ��  c:\progress10\src\adm2\smrtprto.i    �-  Su  c:\progress10\src\adm2\globals.i �-  M�  c:\progress10\src\adm2\custom\globalscustom.i    �-  )a  c:\progress10\src\adm2\custom\smartdefscustom.i  4.  �  c:\progress10\src\adm2\appsprto.i    p.  ��  c:\progress10\src\adm2\custom\appserverdefscustom.i  �.  ��  c:\progress10\src\adm2\dataqueryprto.i   �.  ª 
 c:\progress10\src\adm2\custom\dataquerydefscustom.i  /  ��  c:\progress10\src\adm2\qryprto.i T/  �  c:\progress10\src\adm2\custom\querydefscustom.i  �/  �`  c:\progress10\src\adm2\dataprto.i    �/  �  c:\progress10\src\adm2\custom\datadefscustom.i   �/  e�  c:\progress10\gui\adecomm\appserv.i  (0  F    \\192.168.101.9\desarrollo\prg\dPer_Facturacion.w        �   �      �0  [  Z     �0     X  %   �0  �   �     �0     z  .   �0  �   p     �0     Q     �0  �   N     1     ,  #   1  �   *     (1       #   81  �        H1     �  #   X1  �   �     h1     �  #   x1  �   �     �1     �  #   �1  �   �     �1     v  #   �1  �   t     �1     R  #   �1  �   P     �1     .  #   �1  �   !     2     	  -   2  �        (2     �  ,   82  k   �     H2  �  �     X2     �  +   h2  �  �     x2     �  +   �2  �       �2     e  +   �2  �  b     �2     H  +   �2  �  E     �2     +  +   �2  �  (     �2       +   3  �       3     �  +   (3  �  �     83     �  +   H3  �  �     X3     �  +   h3  �  �     x3     �  +   �3  �  �     �3     }  +   �3  �  z     �3     `  +   �3  �  ]     �3     C  +   �3  �  @     �3     &  +   4  �  #     4     	  +   (4  �       84     �  +   H4  �  �     X4     �  +   h4  �  �     x4     �  +   �4  �  �     �4     p  #   �4  �  o     �4     M  #   �4  k  (     �4       #   �4  j       �4     �  #   5  i  �     5     �  #   (5  _  �     85     �  *   H5  ^  �     X5     i  *   h5  ]  h     x5     B  *   �5  \  A     �5       *   �5  [       �5     �  *   �5  Z  �     �5     �  *   �5  Y  �     �5     �  *   6  X  �     6       *   (6  W  ~     86     X  *   H6  V  W     X6     1  *   h6  U  0     x6     
  *   �6  T  	     �6     �  *   �6  S  �     �6     �  *   �6  R  �     �6     �  *   �6  Q  �     �6     n  *   7  P  m     7     G  *   (7  O  F     87        *   H7  N       X7     �  *   h7  @  �     x7     �  #   �7  	  �     �7     �  )   �7  �        �7     ]  #   �7  �   \     �7     :  #   �7  �   9     �7       #   8  �        8     �  #   (8  �   �     88     �  #   H8  �   �     X8     �  #   h8  �   >     x8     �  (   �8  g   �     �8  a   �      �8     q  '   �8  _   o      �8     M  #   �8  ]   K      �8     )  #   �8  I         9  �     !   9     �  &   (9  �   �  !   89     �  #   H9  �   �  !   X9     j  #   h9  �   h  !   x9     F  #   �9  g   ,  !   �9          �9  O   �  !   �9  �     "   �9     }  %   �9  �   M  "   �9     �  $   �9  �   �  "   :     �  #   :  �   �  "   (:     �  #   8:  �   �  "   H:     �  #   X:  �   �  "   h:     _  #   x:  �   K  "   �:     )  #   �:  }     "   �:     �  #   �:       "   �:     1  !   �:     �      �:     �     �:     7     ;  �   .     ;  O         (;          8;     �     H;  �   �     X;  �        h;  O   q     x;     `     �;          �;  y   �
     �;  �   �
  	   �;  G   �
     �;     �
     �;     x
     �;  c   
  	   �;  x   
     <  M   �	     <     �	     (<     �	     8<  a   �	     H<  �  f	     X<     G	     h<  �  	     x<  O   	     �<     �     �<     �     �<  �   �     �<     �     �<     �     �<  x   �     �<     �     �<     b     =     ^     =     J     (=     1     8=  Q   !     H=     �     X=     �     h=     {     x=     a     �=  ]   [  	   �=     Q     �=     	  	   �=     �  
   �=     �  	   �=  Z   �     �=     �     �=     �     >     �     >     �     (>  c   e     8>     C     H>     �      X>     �      h>     �      x>     �      �>     !       �>           