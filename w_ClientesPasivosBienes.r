	��V��K�5   �              _                                �P 35D00110utf-8 MAIN \\192.168.101.9\desarrollo\prg\w_ClientesPasivosBienes.w,,INPUT P_Nit CHARACTER,OUTPUT P_Pas DECIMAL,OUTPUT P_VVeh DECIMAL,OUTPUT P_VBie DECIMAL,OUTPUT P_VOtros DECIMAL PROCEDURE updaterecord,, PROCEDURE updatemode,,INPUT c CHARACTER PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE copyrecord,, PROCEDURE cancelrecord,, PROCEDURE calculos_bienes,, PROCEDURE adm-create-objects,, PROCEDURE addrecord,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      �              P�              & �  |�              <�              �*    +   �� �  7   8� h  8   �� �  B   �� `F  C   �� t  D   d� �  E   T� �  F   D� �  G   �� �  H   L� (  I   t� �  J   \� t  K   �� �  L           �� �  ? L� �.  ISO8859-1                                                                        $      �                                       �              �  �                   t     �   �v    ��  t    �  (�  �   �      �          `                                             PROGRESS                         l           
        
                    �              �                                                                                                     
      �       �             �         �                     �         �       (             �         �       P                      �                      D            �
      �  
        
                  �  T                                                                                                       �
          
      �  �
      L  
        
                  8               �                                                                                          �
          
      �  �
         
        
                  �  �             p                                                                                          �
          
      <  �
      �  
        
                  �  p             $                                                                                          �
          
      �  �
      h  
        
                  T  $             �                                                                                          �
          
      �  �
        
        
                    �             �                                                                                          �
          
      X        �  
        
                  �  �  	           @                                                                                                    
        %      �  
        
                  p  @  
           �                                                                                          %          
      �  3      8                             $  �             �                                                                                          3                t	  @      �                            �  �	             \	                                                                                          @                (
  N      �	  
        
                  �	  \
             
                                                                                          N          
      �
  \      T
  
        
                  @
               �
                                                                                          \          
      �  j        
        
                  �
  �             x                                                                                          j          
      D  x      �                            �  x             ,                                                                                          x                �  �      p                            \  ,             �                                                                                          �                �  �      $                              �             �                                                                                          �                    �      �                            �  �             H                                                                                          �                             bdcentral                        PROGRESS                                e-  `      e-                         ΎJ            e-  v�                              �  0                      �  @  �      MODELONOMBRENITCOD_RELACIONCOD_TIPOPLACAMATRICULA_INMOBILIARIADETALLEPRENDA_HIPOTECALUGAR_BIENESDIR_BIENESVAL_COMERCIALVAL_CUOTAVAL_SALDO                                                                         	          
                                                                          t�                                              * |�          �  �  � ��                          
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
                                         
                                                                                                               $ �   �   �   �   �   �   �   �       ,  <  L  \  l  |  �  �  �  �  �  �  �  �      ,  <  L  \  l  |  �  �     $ �   �   �   �   �   �   �   �      ,  <  L  \  l  |  �  �  �  �  �  �  �  �      ,  <  L  \  l  |  �  �    ��                                               �          ����                            �.   ��    CierraClientesPasivosBienes undefined                                                               �       ��  �   p   ��                        �����               �h�                        O   ����    e�          O   ����    R�          O   ����    ��      x       �   �              4   ����      /                                    3   ����       $     L  ���                       8      
                       � ߱        �  �      D       t
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      (    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         `      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget x      �           L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget        D      p    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  P      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            8    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         \      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    p      �           �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �      $      \  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    <      �      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �             ,   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       P      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    `      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            L    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  ,      p      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    |      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            D    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank   $      d      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused t      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      <	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      `	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue p	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	       
      0
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    K  �
  �
  d  �       4   �����       o   L       �
                              �  �   NA  �   �  �   �  �      �          $    8  '  L  (  `    t    �    �  `  �  
`  �  $  �    �            $  _  �  ���                            
                    � ߱        Ă    �  �  X            4   ����                h                      ��                  �  �                  8�3                           �  �  �    �  �  �      P      4   ����P      $  �  �  ���                       �  @         �              � ߱              �          �      4   �����      $  �  D  ���                       8  @         $              � ߱        assignPageProperty                                �      ��                      $              L                         O   ����    e�          O   ����    R�          O   ����    ��            ��   p             <               ��                  d           ��                            ����                            changePage                              `  H      ��                      x              $                         O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             d  L      ��                       |              4                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  |      ��                  "  '  �              T�                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                �  
             ��   H                            �� 
                 <  
         ��                            ����                            createObjects                               <  $      ��                  )  *  T              ��s                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              @  (      ��                  ,  .  X               �s                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            destroyObject                               p  X      ��                  0  1  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                t  \      ��                  3  5  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  7  8  �              �n_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  :  ;  �              �o_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  =  ?  �              xr_                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  A  C                ��P                        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            passThrough                                      ��                  E  H  0              �P                        O   ����    e�          O   ����    R�          O   ����    ��            ��   |             H               ��                  p           ��                            ����                            removePageNTarget                               t  \      ��                  J  M  �              ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  O  Q  �              T�4                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  S  U                 �"w                        O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            viewObject                              !  !      ��                  W  X  4!              �&w                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                 "  "      ��                  Z  \  8"              �'w                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  P"           ��                            ����                            disablePagesInFolder    
      �"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �"      #      P#          LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  0#      |#      �#    '      HANDLE, getCallerWindow �#      �#      �#    :      HANDLE, getContainerMode    �#      �#      $$    J      CHARACTER,  getContainerTarget  $      0$      d$    [      CHARACTER,  getContainerTargetEvents    D$      p$      �$    n      CHARACTER,  getCurrentPage  �$      �$      �$    �      INTEGER,    getDisabledAddModeTabs  �$      �$      ,%     �      CHARACTER,  getDynamicSDOProcedure  %      8%      p%  !  �      CHARACTER,  getFilterSource P%      |%      �%  "  �      HANDLE, getMultiInstanceActivated   �%      �%      �%  #  �      LOGICAL,    getMultiInstanceSupported   �%      �%      8&  $  �      LOGICAL,    getNavigationSource &      D&      x&  %        CHARACTER,  getNavigationSourceEvents   X&      �&      �&  &        CHARACTER,  getNavigationTarget �&      �&       '  '  6      HANDLE, getOutMessageTarget �&      '      <'  (  J      HANDLE, getPageNTarget  '      D'      t'  )  ^      CHARACTER,  getPageSource   T'      �'      �'  *  m      HANDLE, getPrimarySdoTarget �'      �'      �'  +  {      HANDLE, getReEnableDataLinks    �'      �'      ,(  ,  �      CHARACTER,  getRunDOOptions (      8(      h(  -  �      CHARACTER,  getRunMultiple  H(      t(      �(  .  �      LOGICAL,    getSavedContainerMode   �(      �(      �(  /  �      CHARACTER,  getSdoForeignFields �(      �(      ()  0  �      CHARACTER,  getTopOnly  )      4)      `)  1 
 �      LOGICAL,    getUpdateSource @)      l)      �)  2  �      CHARACTER,  getUpdateTarget |)      �)      �)  3        CHARACTER,  getWaitForObject    �)      �)      *  4        HANDLE, getWindowTitleViewer    �)       *      X*  5  )      HANDLE, getStatusArea   8*      `*      �*  6  >      LOGICAL,    pageNTargets    p*      �*      �*  7  L      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �*      +      4+  8  Y      LOGICAL,INPUT h HANDLE  setCallerProcedure  +      L+      �+  9  i      LOGICAL,INPUT h HANDLE  setCallerWindow `+      �+      �+  :  |      LOGICAL,INPUT h HANDLE  setContainerMode    �+      �+      ,  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      <,      p,  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  P,      �,      �,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �,      �,      -  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      H-      �-  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource `-      �-      �-  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �-      �-      $.  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   .      D.      �.  B        LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   `.      �.      �.  C  *      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �.      /      P/  D  D      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   0/      t/      �/  E  X      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �/      �/      0  F  r      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      (0      \0  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  <0      |0      �0  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �0      �0       1  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0       1      T1  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    41      |1      �1  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �1      �1      2  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      02      `2  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  @2      �2      �2  N         LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �2      �2      3  O        LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      <3      p3  P  %      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  P3      �3      �3  Q 
 9      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �3      �3      4  R  D      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      <4      l4  S  T      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    L4      �4      �4  T  d      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �4      �4      5  U  u      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      <5      l5  V  �      CHARACTER,  setStatusArea   L5      x5      �5  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             `6  H6      ��                  �  �  x6              |�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               h7  P7      ��                  �  �  �7              �                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                p8  X8      ��                  �  �  �8              H�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                |9  d9      ��                  �  �  �9              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �:  l:      ��                  �  �  �:              ��                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �:           ��                            ����                            getAllFieldHandles  �5      ;      P;  X  �      CHARACTER,  getAllFieldNames    0;      \;      �;  Y  �      CHARACTER,  getCol  p;      �;      �;  Z  �      DECIMAL,    getDefaultLayout    �;      �;      <  [  �      CHARACTER,  getDisableOnInit    �;      <      D<  \  �      LOGICAL,    getEnabledObjFlds   $<      P<      �<  ]  �      CHARACTER,  getEnabledObjHdls   d<      �<      �<  ^        CHARACTER,  getHeight   �<      �<      �<  _ 	       DECIMAL,    getHideOnInit   �<      =      8=  `  !      LOGICAL,    getLayoutOptions    =      D=      x=  a  /      CHARACTER,  getLayoutVariable   X=      �=      �=  b  @      CHARACTER,  getObjectEnabled    �=      �=      �=  c  R      LOGICAL,    getObjectLayout �=      >      4>  d  c      CHARACTER,  getRow  >      @>      h>  e  s      DECIMAL,    getWidth    H>      t>      �>  f  z      DECIMAL,    getResizeHorizontal �>      �>      �>  g  �      LOGICAL,    getResizeVertical   �>      �>       ?  h  �      LOGICAL,    setAllFieldHandles   ?      ,?      `?  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    @?      �?      �?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �?      �?      @  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �?      ,@      `@  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   @@      �@      �@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �@      �@      A  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �@      (A      XA  o  	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal 8A      |A      �A  p  	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �A      �A      B  q  2	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �A      8B      lB  r  D	      LOGICAL,    getObjectSecured    LB      xB      �B  s  X	      LOGICAL,    createUiEvents  �B      �B      �B  t  i	      LOGICAL,    bindServer                              �C  pC      ��                  �  �  �C              P�#                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �D  xD      ��                  �  �  �D              �#                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �E  �E      ��                  �  �  �E              ��                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �F  �F      ��                  �  �  �F              @�                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �G  �G      ��                  �  �  �G              h�#                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �H  �H      ��                  �  �  �H              �#                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �I  �I      ��                  �  �  �I              H�#                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            startServerObject                                K  �J      ��                  �  �  K              x�#                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                L  �K      ��                  �  �   L               �#                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  8L           ��                            ����                            getAppService   �B      �L      �L  u  x	      CHARACTER,  getASBound  �L      �L      M  v 
 �	      LOGICAL,    getAsDivision   �L      M      DM  w  �	      CHARACTER,  getASHandle $M      PM      |M  x  �	      HANDLE, getASHasStarted \M      �M      �M  y  �	      LOGICAL,    getASInfo   �M      �M      �M  z 	 �	      CHARACTER,  getASInitializeOnRun    �M      �M      0N  {  �	      LOGICAL,    getASUsePrompt  N      <N      lN  |  �	      LOGICAL,    getServerFileName   LN      xN      �N  }  �	      CHARACTER,  getServerOperatingMode  �N      �N      �N  ~  �	      CHARACTER,  runServerProcedure  �N      �N      0O    
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   O      tO      �O  �  %
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �O      �O      �O  �  3
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �O       P      LP  �  A
      LOGICAL,INPUT phASHandle HANDLE setASInfo   ,P      lP      �P  � 	 M
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    xP      �P      �P  �  W
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �P      Q      DQ  �  l
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   $Q      dQ      �Q  �  {
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  xQ      �Q      �Q  �  �
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �R  �R      ��                  �  �  �R              ��                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  S             �R  
             ��   @S             S               �� 
                 4S  
         ��                            ����                            addMessage                              0T  T      ��                  �  �  HT              tH4                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �T             `T               ��   �T             �T               ��                  �T           ��                            ����                            adjustTabOrder                              �U  �U      ��                  �  �  �U              P4                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  V             �U  
             �� 
  <V             V  
             ��                  0V           ��                            ����                            applyEntry                              ,W  W      ��                  �  �  DW              �W4                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  \W           ��                            ����                            changeCursor                                \X  DX      ��                  �  �  tX              �z_                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �X           ��                            ����                            createControls                              �Y  tY      ��                  �  �  �Y              $_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  |Z      ��                  �  �  �Z              �_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �[  �[      ��                  �  �  �[              �_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �\  �\      ��                  �  �  �\              ă_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �]  �]      ��                  �  �  �]              ��_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �^  �^      ��                  �  �  �^              T�_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �_  �_      ��                  �  �  �_               �_                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �`  �`      ��                  �  �  �`              L�_                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0a             �`  
             ��   Xa             $a               ��   �a             La               ��                  ta           ��                            ����                            modifyUserLinks                             tb  \b      ��                  �  �  �b              �w                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �b             �b               ��    c             �b               �� 
                 �b  
         ��                            ����                            removeAllLinks                              �c  �c      ��                  �  �  d              ��7                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �d  �d      ��                  �  �  e              ��7                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  \e             (e  
             ��   �e             Pe               �� 
                 xe  
         ��                            ����                            repositionObject                                |f  df      ��                  �  �  �f              0�7                        O   ����    e�          O   ����    R�          O   ����    ��            ��   �f             �f               ��                  �f           ��                            ����                            returnFocus                             �g  �g      ��                  �  �  �g              ��7                        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  h  
         ��                            ����                            showMessageProcedure                                i  �h      ��                  �  �   i              �7                        O   ����    e�          O   ����    R�          O   ����    ��            ��   li             8i               ��                  `i           ��                            ����                            toggleData                              \j  Dj      ��                  �  �  tj              t�                        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �j           ��                            ����                            viewObject                              �k  pk      ��                  �  �  �k              ؈                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �Q      �k      $l  � 
 �      LOGICAL,    assignLinkProperty  l      0l      dl  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Dl      �l      �l  �        CHARACTER,  getChildDataKey �l      �l      (m  �        CHARACTER,  getContainerHandle  m      4m      hm  �  .      HANDLE, getContainerHidden  Hm      pm      �m  �  A      LOGICAL,    getContainerSource  �m      �m      �m  �  T      HANDLE, getContainerSourceEvents    �m      �m      (n  �  g      CHARACTER,  getContainerType    n      4n      hn  �  �      CHARACTER,  getDataLinksEnabled Hn      tn      �n  �  �      LOGICAL,    getDataSource   �n      �n      �n  �  �      HANDLE, getDataSourceEvents �n      �n       o  �  �      CHARACTER,  getDataSourceNames   o      ,o      `o  �  �      CHARACTER,  getDataTarget   @o      lo      �o  �  �      CHARACTER,  getDataTargetEvents |o      �o      �o  �  �      CHARACTER,  getDBAware  �o      �o      p  � 
 �      LOGICAL,    getDesignDataObject �o       p      Tp  �        CHARACTER,  getDynamicObject    4p      `p      �p  �        LOGICAL,    getInstanceProperties   tp      �p      �p  �  ,      CHARACTER,  getLogicalObjectName    �p      �p      q  �  B      CHARACTER,  getLogicalVersion   �p      (q      \q  �  W      CHARACTER,  getObjectHidden <q      hq      �q  �  i      LOGICAL,    getObjectInitialized    xq      �q      �q  �  y      LOGICAL,    getObjectName   �q      �q      r  �  �      CHARACTER,  getObjectPage   �q      $r      Tr  �  �      INTEGER,    getObjectParent 4r      `r      �r  �  �      HANDLE, getObjectVersion    pr      �r      �r  �  �      CHARACTER,  getObjectVersionNumber  �r      �r      s  �  �      CHARACTER,  getParentDataKey    �r      s      Ps  �  �      CHARACTER,  getPassThroughLinks 0s      \s      �s  �  �      CHARACTER,  getPhysicalObjectName   ps      �s      �s  �        CHARACTER,  getPhysicalVersion  �s      �s      t  �        CHARACTER,  getPropertyDialog   �s       t      Tt  �  0      CHARACTER,  getQueryObject  4t      `t      �t  �  B      LOGICAL,    getRunAttribute pt      �t      �t  �  Q      CHARACTER,  getSupportedLinks   �t      �t      u  �  a      CHARACTER,  getTranslatableProperties   �t      u      Tu  �  s      CHARACTER,  getUIBMode  4u      `u      �u  � 
 �      CHARACTER,  getUserProperty lu      �u      �u  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �u      �u      (v  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles v      Pv      |v  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    \v      �v      �v  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �v      w      8w  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   w      �w      �w  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �w      �w      (x  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  x      Px      �x  �  �      CHARACTER,  setChildDataKey `x      �x      �x  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �x      �x      y  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �x      8y      ly  �  /      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    Ly      �y      �y  �  B      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �y      �y       z  �  [      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource    z      Hz      xz  �  o      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents Xz      �z      �z  �  }      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �z      �z      ({  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   {      P{      �{  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents `{      �{      �{  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �{      �{      (|  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject |      H|      ||  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    \|      �|      �|  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �|      �|      ,}  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    }      P}      �}  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   h}      �}      �}  �  !      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �}      �}      ,~  �  3      LOGICAL,INPUT pcName CHARACTER  setObjectParent ~      L~      |~  �  A      LOGICAL,INPUT phParent HANDLE   setObjectVersion    \~      �~      �~  �  Q      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �~      �~      ,  �  b      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks       T      �  �  s      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   h      �      �  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �       �      4�  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      X�      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   h�      ��      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   Ā      �      D�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  $�      h�      ��  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty t�      ��      �  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ā      $�      P�  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   0�      t�      ��  � 	       CHARACTER,INPUT pcName CHARACTER    ��      ��  `�      h      4   ����h                p�                      ��                    B                  4gA                             ��          ��  �      x      4   ����x                �                      ��                    A                  �gA                             ��   �    .  8�  ��      �      4   �����                Ȅ                      ��                  :  <                  DkA                           :  H�         ;                                  (     
                    � ߱        L�  $  >  �  ���                           $  @  x�  ���                       t                         � ߱        ��    F  ��  @�      �      4   �����                P�                      ��                  G  	                  �kA                           G  Ѕ  ��  o   J      ,                                 ܆  $   K  ��  ���                       �  @         �              � ߱        ��  �   L        �  �   M  �      �  �   O         ,�  �   Q  t      @�  �   S  �      T�  �   U  \      h�  �   V  �      |�  �   W        ��  �   Z  �      ��  �   \  �      ��  �   ]  x	      ̇  �   _  �	      ��  �   `  p
      �  �   a  �
      �  �   b  (      �  �   c  �      0�  �   i  �      D�  �   k  L      X�  �   q  �      l�  �   s  �      ��  �   u  p      ��  �   v  �      ��  �   |  h      ��  �   }  �      Ј  �   ~  X      �  �     �      ��  �   �  @      �  �   �  |       �  �   �  �      4�  �   �  ,      H�  �   �  �      \�  �   �  �      p�  �   �        ��  �   �  T      ��  �   �  �      ��  �   �        ��  �   �  H      ԉ  �   �  �      �  �   �  �      ��  �   �  �      �  �   �  8      $�  �   �  t      8�  �   �  �      L�  �   �  �          �   �  (                      |�          �  Њ      ��                  2	  `	   �              lO9                        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     $                         � ߱        ��  $ F	  �  ���                           O   ^	  ��  ��  d               �          �  �    �                                             ��                            ����                                <5      `�      ��     6     �                      V �  �                     |�    �	  Ԍ  T�      p      4   ����p                d�                      ��                  �	  
                  `T9                           �	  �  x�  �   �	  �      ��  �   �	  D      ��  �   �	  �      ��  �   �	  <      ȍ  �   �	  �      ܍  �   �	  4      ��  �   �	  �      �  �   �	  $      �  �   �	  �      ,�  �   �	        @�  �   �	  �      T�  �   �	        h�  �   �	  �          �   �	        X�    
  ��  �      t      4   ����t                (�                      ��                  
  �
                  ,V9                           
  ��  <�  �   
  �      P�  �   
  H      d�  �   
  �      x�  �   
  8       ��  �   
  �       ��  �   
   !      ��  �   
  �!      ȏ  �   
  "      ܏  �   
  �"      ��  �   
  �"      �  �   
  t#      �  �    
  �#      ,�  �   !
  \$      @�  �   "
  �$      T�  �   #
  T%      h�  �   $
  �%      |�  �   %
  L&      ��  �   &
  �&      ��  �   '
  D'      ��  �   (
  �'      ̐  �   )
  <(      ��  �   *
  �(      ��  �   +
  4)      �  �   ,
  �)      �  �   -
  ,*      0�  �   .
  �*      D�  �   /
  $+          �   0
  �+      x�    �
  t�  ��      ,      4   ����,                �                      ��                  �
  _                  4�#                           �
  ��  �  �   �
  h,      ,�  �   �
  �,      @�  �   �
  `-      T�  �   �
  �-      h�  �   �
  H.      |�  �   �
  �.      ��  �   �
  0/      ��  �   �
  l/      ��  �   �
  �/      ̒  �   �
  0      ��  �   �
  X0      ��  �   �
  �0      �  �   �
  @1      �  �   �
  �1      0�  �   �
  02      D�  �   �
  �2      X�  �   �
  3      l�  �   �
  �3      ��  �   �
  4      ��  �   �
  L4      ��  �   �
  �4      ��  �   �
  45      Г  �   �
  �5      �  �   �
  �5      ��  �   �
   6      �  �   �
  �6       �  �   �
  �6      4�  �   �
  7      H�  �   �
  P7      \�  �   �
  �7      p�  �   �
  �7      ��  �   �
  8      ��  �   �
  @8      ��  �   �
  �8      ��  �   �
  �8      Ԕ  �   �
  ,9      �  �   �
  h9      ��  �   �
  �9      �  �   �
  �9      $�  �   �
  :      8�  �   �
  X:      L�  �   �
  �:      `�  �   �
  @;      t�  �   �
  �;      ��  �   �
  (<      ��  �   �
  �<      ��  �   �
   =      ĕ  �   �
  �=      ؕ  �   �
  >      �  �   �
  �>       �  �   �
  ?      �  �   �
  L?      (�  �   �
  �?      <�  �   �
  @      P�  �   �
  @@      d�  �   �
  |@          �   �
  �@      Ж  $  k  ��  ���                       XA     
                    � ߱        h�    �  �  ��      dA      4   ����dA      /   �  (�     8�                          3   ����tA            X�                      3   �����A  ĝ    �  ��  �  ��  �A      4   �����A  	              �                      ��             	     �  3                  lXv                           �  ��  (�  �   �  B      ��  $  �  T�  ���                       <B     
                    � ߱        ��  �   �  \B      �  $   �  ��  ���                       �B  @         pB              � ߱        ��  $  �  �  ���                       �B                         � ߱        LC     
                �C                     E  @        
 �D              � ߱        8�  V   �  D�  ���                        $E                     XE                     �E                         � ߱        Ț  $  �  ԙ  ���                       TF     
                �F                      H  @        
 �G              � ߱        X�  V   �  d�  ���                        ,H     
                �H                     �I  @        
 �I              � ߱            V     ��  ���                        
              ��                      ��             
     5  �                  �                            5  ��  J     
                �J                     �K  @        
 �K          4L  @        
 �K          �L  @        
 TL          �L  @        
 �L              � ߱            V   J  �  ���                        adm-clone-props \�  �              �     7     l                          h  ]                     start-super-proc    ��  T�  �           �     8     (                          $  ~                     \�    �  ��  �      �P      4   �����P      /   �  �     ,�                          3   �����P            L�                      3   �����P  ��  $     ��  ���                       �P                         � ߱        t�      О  P�  �  �P      4   �����P                ğ                      ��                                      7                             ��   Q                     Q                       (Q       !       !           � ߱            $    `�  ���                               �  H�      @Q      4   ����@Q  `Q                         � ߱            $    �  ���                       p�    #  ��  ��  ��  tQ      4   ����tQ      $  $  ̠  ���                       �Q       !       !           � ߱            �   A  �Q      �Q     
                dR                     �S  @        
 tS              � ߱        ��  V   U  �  ���                        ��  �   �  �S      H�    
  ̡  ܡ       T      4   ���� T      /     �     �                          3   ����T            8�                      3   ����0T  �  $    t�  ���                       LT       "       "           � ߱        xT     
                �T                     DV  @        
 V              � ߱        0�  V     ��  ���                        �    �  L�  ̣      PV      4   ����PV                ܣ                      ��                  �  �                  �"7                           �  \�      g   �  ��          ���                           ��          ��  x�      ��                  �      ��              ��,                        O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     ��  xV                      3   ����`V  ,�     
   �                      3   �����V         
   L�                      3   �����V    ��                              ��        �                  ����                                        �              9      \�                      g                               $�  g   �  0�           �	ȧ                           ��          ̦  ��      ��                  �  �  �              L�,                        O   ����    e�          O   ����    R�          O   ����    ��          /  �  (�     8�  �V                      3   �����V            X�                      3   �����V    ��                              ��        �                  ����                                        D�              :      h�                      g                               0�  g   �  <�           �	ԩ                           �          ب  ��      ��                  �  �  �              ��,                        O   ����    e�          O   ����    R�          O   ����    ��          /  �  4�     D�  �V                      3   �����V            d�                      3   �����V    ��                              ��        �                  ����                                        P�              ;      t�                      g                               ��    �  L�  ̪      W      4   ����W                ܪ                      ��                  �  �                  h6                           �  \�  H�  /   �  �     �                          3   ����$W            8�                      3   ����DW  D�  /  �  t�     ��  �W                      3   ����`W  ��     
   ��                      3   �����W  �        ԫ                      3   �����W  �        �                      3   �����W            4�                      3   �����W  l�    �  `�  p�      �W      4   �����W      /  �  ��     ��  tX                      3   ����TX  ܬ     
   ̬                      3   ����|X  �        ��                      3   �����X  <�        ,�                      3   �����X            \�                      3   �����X        �  ��  ��      �X      4   �����X      /  �  ĭ     ԭ  0Y                      3   ����Y  �     
   ��                      3   ����8Y  4�        $�                      3   ����@Y  d�        T�                      3   ����TY            ��                      3   ����pY  \�    �  ��  0�      �Y      4   �����Y                @�                      ��                  �  �                  h:                           �  ��      g   �  X�          � �        �Y                  $�          ��  ܯ      ��                  �      �              �:                        O   ����    e�          O   ����    R�          O   ����    ��          /  �  P�     `�  �Y                      3   �����Y  ��     
   ��                      3   �����Y         
   ��                      3   �����Y    ��                            ����                                        l�              <      ��                      g                               ��     �  �Y                                     �Y     
                tZ                     �[  @        
 �[              � ߱        ��  V   Q  ��  ���                        �[     
                T\                     �]  @        
 d]              � ߱        ��  V   x   �  ���                        4�    �  ̲  ܲ      �]      4   �����]      $   �  �  ���                       ^  @         ^              � ߱        �  g   �  L�          ���        ,^   ���        8^                  ,�          ��  �      ��                  �  �  �              (�O                        O   ����    e�          O   ����    R�          O   ����    ��            �  H�  X�      D^      4   ����D^      O  �  ������  X^    ��                            ����                                        t�              =      p�                      g                               ��  g   �  $�          60�         l^                              ��  ��      ��                  �  �  ص              ��O                        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                        8�              >      �                      g                               ��  g   �  ��          "<�                            p�          @�  (�      ��                  �  �  X�              H�O                        O   ����    e�          O   ����    R�          O   ����    ��      ��  /   �  ��                                 3   ����x^  ķ  �  �  �^                �  �^  }        ��                              ��        �                  ����                                        ��              ?      ܷ                      g                                     �  ��  4�      �^      4   �����^                ��                      ��                  �  &                  �3                           �  ĸ  �^  @                     �^  @         �^          _  @         _              � ߱        Թ  $   �  D�  ���                       Ի  g     �          nx�      }                      ��          ��  p�      ��                      ��              t�3                        O   ����    e�          O   ����    R�          O   ����    ��      ��  /    �                                 3   ����$_          �   �      @_      4   ����@_      O    ������  t_    ��                            ����                                         �              @      8�                      g                               ��  g     �          !P�         �_                  �          ��  p�      ��                      ��               �3                        O   ����    e�          O   ����    R�          O   ����    ��      �_  @                         � ߱            $    ��  ���                         ��                            ����                                         �              A      �                      g                               �  /     ؽ                                 3   �����_          �  ��      �_      4   �����_                �                      ��                    $                  ж_                             �                D�          ,�  �      ��                   "                  4�_                             ��      O       ��          O       ��      ��  /     p�                                 3   �����_           ��  ��      �_      4   �����_      k   !  ȿ              }      �n        �   addrecord   h�  �                      B      �                                	                   adm-create-objects  �  H�              �E     C      F                          �E  K-                     calculos_bienes \�  ��          x         D     �                          �  q-                     cancelrecord    ��  $�                      E      �                              �-                     copyrecord  4�  ��                      F      �                              �-  
                   disable_UI  ��  ��                      G      @                              �-  
                   enable_UI   �  `�                      H      (                              �-  	                   exitObject  l�  ��                      I      �                               �-  
                   initializeObject    ��  0�                      J      �                              �.                     updatemode  D�  ��  �           �     K                                 �.  
                   updaterecord    ��  �                      L      �                              �.                      �     ��     �����������������������  �             8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  (�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL   �  l�  x�      returnFocus ,INPUT hTarget HANDLE   \�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  P�  `�      removeAllLinks  ,   @�  t�  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE d�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  h�  t�      hideObject  ,   X�  ��  ��      editInstanceProperties  ,   x�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  8�  D�      applyEntry  ,INPUT pcField CHARACTER    (�  p�  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER `�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  <�  D�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ,�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      startServerObject   ,   ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  4�  H�      restartServerObject ,   $�  \�  t�      initializeServerObject  ,   L�  ��  ��      disconnectObject    ,   x�  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  ��  �      processAction   ,INPUT pcAction CHARACTER   ��  4�  D�      enableObject    ,   $�  X�  h�      disableObject   ,   H�  |�  ��      applyLayout ,   l�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  (�  4�      selectPage  ,INPUT piPageNum INTEGER    �  `�  t�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER P�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  �  �      notifyPage  ,INPUT pcProc CHARACTER ��  8�  D�      initPages   ,INPUT pcPageList CHARACTER (�  p�  ��      initializeVisualContainer   ,   `�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  ��      destroyObject   ,   ��  ��  �      deletePage  ,INPUT piPageNum INTEGER    ��  4�  D�      createObjects   ,   $�  X�  h�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE H�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  �  $�      changePage  ,   �  8�  L�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
   %     adecomm/as-utils.w  
"   
   �    }        �
"     
   %              %                  �     }        �G� �   �G%              � �     %       	 %       %        %       	%        %       	%               %               %               %               %              %              %              %              %               %              
�        
"   
   
�    
"   
   
"   
       �        8     �        D    
"   
   �        �         �     }        �%              
"   
   
"   
       �        �     �        �    
"   
   �                 �     }        �%              � 
"    
   %              � �  �         X      $              
�    �    �      
"   
                         
�            �    �
"    
   
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
   �               1� /  
   � :   � %               o%   o           � ?    O
"   
   �           �    1� @     � :   � %               o%   o           � N   O
"   
   �           �    1� U  
   � :   � %               o%   o           � `   O
"   
   �           h    1� l     � :   � %               o%   o           � z   O
"   
   �           �    1� �     � :   � %               o%   o           � �   O
"   
   �           P    1� �     � �   � %               o%   o           %               
"   
   �          �    1� �     � �     
"   
   �               1� �     � :   � %               o%   o           � �  e O
"   
   �           |    1� K     � :   � %               o%   o           � Z  [ O
"   
   �           �    1� �     � �   � %               o%   o           %               
"   
   �           l	    1� �     � �   � %               o%   o           %               
"   
   �           �	    1� �     � �   � %               o%   o           %              
"   
   �          d
    1� �     � �     
"   
   �           �
    1� �  
   � �   � %               o%   o           %               
"   
   �               1� �     � :   � %               o%   o           � ?    O
"   
   �          �    1�      � �     
"   
   �           �    1�      � :   � %               o%   o           � -  t O
"   
   �          @    1� �  
   � �     
"   
   �           |    1� �     � :   � %               o%   o           � �  � O
"   
   �           �    1� K     � :   � %               o%   o           � ?    O
"   
   �           d    1� b  
   � m   � %               o%   o           %               
"   
   �           �    1� q     � �   � %               o%   o           %               
"   
   �           \    1� y     � :   � %               o%   o           � ?    
"   
   �           �    1� �     � :   � %               o%   o           o%   o           
"   
   �           L    1� �  
   � :   � %               o%   o           � ?    v
"   
   �           �    1� �     � �  	 � %               o%   o           � �  / 
"   
   �          4    1� �     � �  	   
"   
   �           p    1�      � �  	 � o%   o           o%   o           � ?    
"   
   �          �    1�      � �  	   
"   
   �                1� $     � �  	 � o%   o           o%   o           � ?    
"   
   �          �    1� 4     � �     
"   
   �          �    1� B     � �  	   
"   
   �              1� O     � �  	   
"   
   �          H    1� \     � �  	   
"   
   �           �    1� j     � �   � o%   o           o%   o           %              
"   
   �               1� {     � �  	   
"   
   �          <    1� �  
   � �     
"   
   �          x    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          ,    1� �     � �  	   
"   
   �          h    1� �  	   � �  	   
"   
   �          �    1� �     � �  	   
"   
   �          �    1� �     � �  	   
"   
   �               1�      � :   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �    �� "   � P   �        �    �@    
� @  , 
�       �    �� +     p�               �L
�    %              � 8          � $         � 2          
�    � L     
"   
   � @  , 
�           �� U  
   p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1� O  
   � :   � %               o%   o           � ?    O
"   
   �           8    1� Z  
   � :   � %               o%   o           o%   o           
"   
   �           �    1� e     � �   � %               o%   o           o%   o           
"   
   �           0    1� n     � �   � %               o%   o           %               
"   
   �           �    1� }     � �   � %               o%   o           %               
"   
   �           (    1� �     � :   � %               o%   o           � ?    
"   
   �           �    1� �     � �   � %               o%   o           %              
"   
   �               1� �     � �   � %               o%   o           o%   o           
"   
   �           �    1� �     � :   � %               o%   o           o%   o           
"   
   �               1� �  	   � :   � %               o%   o           � ?    9
"   
   �           �    1� �     � :   � %               o%   o           o%   o           
"   
   �                1� �     � :   � %               o%   o           o%   o           
"   
   �           |    1� �     � �   � %               o%   o           %               
"   
   �           �    1� �     � �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
   �           �    1�      � �  	 � %               o%   o           � ?    9
"   
   �           <    1�      � �  	 � %               o%   o           � ?    9
"   
   �           �    1� !     � �   � %               o%   o           %               
"   
   �           ,     1� /     � �  	 � %               o%   o           � ?    
"   
   �           �     1� >     � �  	 � %               o%   o           � ?    
"   
   �           !    1� L     � �   � %               o%   o           %               
"   
   �           �!    1� Z     � �  	 � %               o%   o           � ?    
"   
   �           "    1� i     � �  	 � %               o%   o           � ?    9
"   
   �           x"    1� x     � �  	 � %               o%   o           � ?    9
"   
   �           �"    1� �     � �  	 � %               o%   o           o%   o           
"   
   �           h#    1� �     � �  	 � %               o%   o           � ?    9
"   
   �           �#    1� �     � �  	 � %               o%   o           � ?    
"   
   �           P$    1� �  	   � �   � %               o%   o           %               
"   
   �           �$    1� �     � �   � %               o%   o           %               
"   
   �           H%    1� �     � �   � %               o%   o           o%   o           
"   
   �           �%    1� �     � �   � %               o%   o           o%   o           
"   
   �           @&    1� �     � �   � %               o%   o           %               
"   
   �           �&    1� �     � �   � %               o%   o           %               
"   
   �           8'    1�      � �   � %               o%   o           %               
"   
   �           �'    1�      � %   � %               o%   o           %       
       
"   
   �           0(    1� -     � %   � %               o%   o           o%   o           
"   
   �           �(    1� 9     � %   � %               o%   o           %              
"   
   �           ()    1� E     � %   � %               o%   o           o%   o           
"   
   �           �)    1� Q     � %   � %               o%   o           %              
"   
   �            *    1� ^     � %   � %               o%   o           o%   o           
"   
   �           �*    1� k     � %   � %               o%   o           %              
"   
   �           +    1� s     � %   � %               o%   o           o%   o           
"   
   �           �+    1� {     � �  	 � %               o%   o           � ?    vP �L 
�H T   %              �     }        �GG %              
"   
   �           \,    1� �     � m   � %               o%   o           %               
"   
   �           �,    1� �     � m   � %               o%   o           o%   o           
"   
   �           T-    1� �     � :   � %               o%   o           � ?    O
"   
   �           �-    1� �     � :   � %               o%   o           � �  - 9
"   
   �           <.    1� �     � :   � %               o%   o           � ?    
"   
   �           �.    1�      � :   � %               o%   o           � -   9
"   
   �          $/    1� K     � �     
"   
   �           `/    1� \     � :   � %               o%   o           � ?    
"   
   �          �/    1� h  
   � �     
"   
   �          0    1� s     � �     
"   
   �           L0    1� �     � �  	 � %               o%   o           � ?    
"   
   �           �0    1� �     � :   � %               o%   o           � ?    
"   
   �           41    1� �     � �   � %               o%   o           o%   o           
"   
   �           �1    1� �     � :   � %               o%   o           � �  ! 
"   
   �           $2    1� �     � :   � %               o%   o           � ?    O
"   
   �           �2    1� �     � :   � %               o%   o           � �   9
"   
   �           3    1�   	   � m   � %               o%   o           o%   o           
"   
   �           �3    1�      � �   � %               o%   o           %               
"   
   �          4    1� !     � �     
"   
   �           @4    1� /     � :   � %               o%   o           � C   9
"   
   �           �4    1� R     � �  	 � %               o%   o           � ?    
"   
   �           (5    1� _     � �  	 � %               o%   o           � ?    
"   
   �          �5    1� o     � �     
"   
   �          �5    1� �     � �  	   
"   
   �           6    1� �     � �   � o%   o           o%   o           %               
"   
   �          �6    1� �     � �     
"   
   �          �6    1� �     � �  	   
"   
   �          7    1� �     � �  	   
"   
   �          D7    1� �     � �  	   
"   
   �          �7    1� �     � �  	   
"   
   �          �7    1�      � �  	   
"   
   �          �7    1�      � �     
"   
   �           48    1� '     � :   � %               o%   o           � >  4 9
"   
   �          �8    1� s     � �     
"   
   �          �8    1� �     � �     
"   
   �           9    1� �     � �     
"   
   �          \9    1� �     � �  	   
"   
   �          �9    1� �     � �  	   
"   
   �          �9    1� �     � �  	   
"   
   �          :    1� �     � �     
"   
   �           L:    1� �     � �  	 � %               o%   o           � ?    O
"   
   �           �:    1� �     � �  	 � %               o%   o           � ?    9
"   
   �           4;    1� �     � �  	 � %               o%   o           � ?    
"   
   �           �;    1�      � �  	 � %               o%   o           � ?    9
"   
   �           <    1� &     � �   � %               o%   o           %               
"   
   �           �<    1� 4     � �   � %               o%   o           o%   o           
"   
   �           =    1� F     � �   � %               o%   o           %               
"   
   �           �=    1� V     � �   � %               o%   o           %               
"   
   �           >    1� b     � �   � %               o%   o           o%   o           
"   
   �           �>    1� }     � �   � %               o%   o           %               
"   
   �          ?    1� �     � �  	   
"   
   �           @?    1� �     � �   � %               o%   o           %              
"   
   �          �?    1� �     � �  	   
"   
   �          �?    1� �     � �  	   
"   
   �          4@    1� �  
   � �  	   
"   
   �           p@    1� �     � �  	 � %               o%   o           � &   
"   
   �           �@    1� �     � �  	 � %               o%   o           � ?    9
"   
    "      %     start-super-proc �� %     adm2/smart.p  �P �L 
�H T   %              �     }        �GG %              
"   
   �       B    6� "     
"   
   
�        0B    8
"   
   �        PB    ��     }        �G 4              
"   
   G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �C    �� "   � P   �        �C    �@    
� @  , 
�       �C    �� +     p�               �L
�    %              � 8      �C    � $         � 2          
�    � L   �
"   
   p� @  , 
�       �D    �� �     p�               �L"      �   �    � !   � �     }        �A      |    "      �    %              (<   \ (    |    �     }        �A� #   �A"          "      "        < "      "      (    |    �     }        �A� #   �A"      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �F    �� "   � P   �        �F    �@    
� @  , 
�       �F    �� +     p�               �L
�    %              � 8      �F    � $         � 2          
�    � L   �
"   
   p� @  , 
�       �G    �� /  
   p�               �L"      
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        xH    �� "   � P   �        �H    �@    
� @  , 
�       �H    �� +     p�               �L
�    %              � 8      �H    � $         � 2          
�    � L   �
"   
   p� @  , 
�       �I    �� �     p�               �L
"   
   
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        PJ    �� "   � P   �        \J    �@    
� @  , 
�       hJ    �� +     p�               �L
�    %              � 8      tJ    � $         � 2          
�    � L     
"   
   p� @  , 
�       �K    �� U  
   p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� l     p�               �L%      WINDOW  
"   
   p� @  , 
�       HL    �� $     p�               �L%               
"   
   p� @  , 
�       �L    ��      p�               �L(        � ?      � ?      � ?      �     }        �A
�H T   %              �     }        �GG %              
"   
    (   � 
"   
       �        �M    �� "   �
"   
   � 8      �M    � $         � 2          
�    � L   �
"   
   �        ,N    �
"   
   �       LN    /
"   
   
"   
   �       xN    6� "     
"   
   
�        �N    8
"   
   �        �N    �
"   
   �       �N    �
"   
   p�    � L   
�    �     }        �G 4              
"   
   G %              G %              
�     }        �
"   
    (   � 
"   
       �        �O    �A"      
"   
   
�        �O    �@ � 
"   
   "      �       }        �
"   
   %              %                "      %     start-super-proc �� %     adm2/appserver.p �O�    � �     
�    �     }        �%               %      Server  - �     }        �    "      � ?    � %                   "       � ?    � %      NONE    p�,  8         $     "               � �   �
�    
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        4R    �� "   � P   �        @R    �@    
� @  , 
�       LR    �� +     p�               �L
�    %              � 8      XR    � $         � 2          
�    � L   �
"   
   p� @  , 
�       hS    �� �     p�               �L"  !    p�,  8         $     "              � �   �
�     "      %     start-super-proc �� %     adm2/visual.p ��   �      �      � �  	   
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        �T    �� "   � P   �        �T    �@    
� @  , 
�       �T    �� +     p�               �L
�    %              � 8      �T    � $         � 2          
�    � L   �
"   
   p� @  , 
�       �U    �� Z     p�               �L"  "    � 
"    
   %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP  �%     processAction   
�    %     CTRL-PAGE-DOWN  "      %     start-super-proc �� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � a   
�    � s   � A    �    � a     
�    �    � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � a   � 
�    � �    %     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
   
"   
   %     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        DZ    �� "   � P   �        PZ    �@    
� @  , 
�       \Z    �� +     p�               �L
�    %              � 8      hZ    � $         � 2   �     
�    � L   � 
"   
   p� @  , 
�       x[    �� o     p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
   
"   
   
"   
   (�  L ( l       �        $\    �� "   � P   �        0\    �@    
� @  , 
�       <\    �� +     p�               �L
�    %              � 8      H\    � $         � 2   �     
�    � L   �
"   
   p� @  , 
�       X]    �� &     p�               �L%              (        �     }        �G� �   �G� 
"   
   
"   
   �        �]    �%              
"   
   
"   
   �     }        �%               
"   
   %     Calculos_bienes � �     %      CLOSE   � 
"   
   
"   
   
"   
   �        �^    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �  	   %               
"   
   
�    %     createObjects    �     }        �%     initializeObject ��  �     }        ��              %               �     "      %               %     constructObject %      dact_pasivos.wDB-AWARE 
�             �G%PE@  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes   
"   
   %     repositionObject �� 
"   
   %            %           %     constructObject %      dact_pasivos.wDB-AWARE 
�             �G%PE@  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes �
"   
   %     repositionObject �� 
"   
   %            %            %     constructObject %      dact_pasivos.wDB-AWARE 
�             �G%PE@  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes �
"   
   %     repositionObject �� 
"   
   %          %           %     constructObject % 
    vpas-act.w 
�             �G%� � �   EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �� 
"   
   %            %           %     constructObject %      dact_pasivos.wDB-AWARE 
�             �G%PE@  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes �
"   
   %     repositionObject �� 
"   
   %         %            %     constructObject %     folder.w  �
�             �G           � b!     � p!  & �� �!  L �
"   
   %     repositionObject �� 
"   
   %       	  %            %     resizeObject    
"   
   %         %        	    %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Page    
�    %     adjustTabOrder  
"   
   
"   
   %      AFTER   %              %     constructObject %     vclientespasivos.w 
�             �G%� � �   EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �� 
"   
   %          %           %     constructObject %     adm2/dynbrowser.w 9
�             �G%���  DisplayedFieldsNombre,Val_Comercial,Val_Cuota,Val_SaldoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout �� 
"  	 
   %     repositionObject �� 
"  	 
   %       	  %            %     resizeObject    
"  	 
   %         %        	    %     constructObject % 	    pupdsav.w �
�             �G%$  AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout �
"   
   %     repositionObject �� 
"   
   %            %        	   %     resizeObject    
"   
   %           %           %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Update  
"   
   %      addLink 
"   
   %      TableIO 
"   
   %      addLink 
"   
   %      Data    
"  	 
   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"  	 
   
"   
   %      AFTER   %              %     constructObject %      vclientesvehiculos.w �� 
�             �G%� � �   EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �� 
"   
   %          %           %     constructObject %     adm2/dynbrowser.w 
�             �G%��  DisplayedFieldsNombre,Modelo,Placa,Val_Comercial,Prenda_HipotecaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout t
"  
 
   %     repositionObject �� 
"  
 
   %       	  %            %     resizeObject    
"  
 
   %         %        	    %     constructObject % 	    pupdsav.w �
�             �G%$  AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout �
"   
   %     repositionObject �� 
"   
   %            %        	   %     resizeObject    
"   
   %        %           %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Update  
"   
   %      addLink 
"   
   %      TableIO 
"   
   %      addLink 
"   
   %      Data    
"  
 
   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"  
 
   
"   
   %      AFTER   %              %     constructObject %     vclientesbienes.w 
�             �G%� � �   EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �� 
"   
   %          %           %     constructObject %     adm2/dynbrowser.w v
�             �G%$  DisplayedFieldsNombre,Dir_Bienes,Lugar_Bienes,Val_Comercial,Matricula_inmobiliaria,Prenda_HipotecaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �� 
"   
   %        %            %     resizeObject    
"   
   %       	  %        	   %     constructObject % 	    pupdsav.w �
�             �G%$  AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout t 
"   
   %     repositionObject �� 
"   
   %            %        	   %     resizeObject    
"   
   %           %           %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Update  
"   
   %      addLink 
"   
   %      TableIO 
"   
   %      addLink 
"   
   %      Data    
"   
   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %              %     constructObject %     vclientesotros.w �
�             �G%� � �   EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �� 
"   
   %          %           %     constructObject %     adm2/dynbrowser.w 
�             �G%���  DisplayedFieldsNombre,Val_Comercial,DetalleEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout nIn
"   
   %     repositionObject �� 
"   
   %         %            %     resizeObject    
"   
   %        %        	   %     constructObject % 	    pupdsav.w �
�             �G%$  AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout t 
"   
   %     repositionObject �� 
"   
   %            %        	   %     resizeObject    
"   
   %           %           %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Update  
"   
   %      addLink 
"   
   %      TableIO 
"   
   %      addLink 
"   
   %      Data    
"   
   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER       "      %               % 
    selectPage 
�    %              %               %              "      &    &        %                  "      &         "      "      "      %               %              "      &    &        %                  "      &         "      "      "      %               %              "      &    &        %                  "      &         "      "      "      %               %              "      &    &        %                  "      &         "      "      "      �              %              �              %               (        �     }        �G� �   �G� 
"   
   
"   
   �     }        �
�    
"   
   
"   
   %      CLOSE   %               % 	    initPages �%      1,2,3,4 %      SUPER   � �-  	   � �-  
   � �-     � �-     � �-  
   %     Busqueda_Nit    
"   
   "      %     Actualizacion_nit � 
"   
   "      %     Actualizacion_nit � 
"   
   "      %     Actualizacion_nit � 
"   
   "      %     Actualizacion_nit � 
"   
   "      %     Cod_Relacion    
"   
   %              %     Cod_Relacion    
"   
   %              %     Cod_Relacion    
"   
   %              %     Cod_Relacion    
"   
   %              p�    � �-  
 
"   
   p�T  `         L                � .   "      � .             � !.   � 
"   
   p�    � /.  	 
"   
   p�    � �-  
 
"   
   p�T  `         L                � .   "      � 9.             � !.   � 
"   
   p�    � /.  	 
"   
   p�    � �-  
 
"   
   p�T  `         L                � .   "      � L.             � !.   � 
"   
   p�    � /.  	 
"   
   p�    � �-  
 
"   
   p�T  `         L                � .   "      � _.             � !.   � 
"   
   p�    � /.  	 
"   
   p�,  8         $     � r.  	         � |.   �
"  	 
   % 
    selectPage %              % 
    selectPage %              % 
    selectPage %              % 
    selectPage %              %     pEscndeDrccion  
"   
   �              %               �              %                              �           �   p       ��                 B  f  �               d�                         O   ����    e�          O   ����    R�          O   ����    ��        $  Q  �   ���                       <M     
                    � ߱              R  ,  �      �M      4   �����M                �                      ��                  S  e                  �                           S  <  �  �  T  �M            V  �  l      8N      4   ����8N                |                      ��                  W  d                  �                           W  �  �  o   X      ,                                 �  �   Y  XN      �  �   Z  �N      0  $  [    ���                       �N     
                    � ߱        D  �   \  �N      X  �   ]  �N      l  �   `  O          $   c  �  ���                       @O  @         ,O              � ߱                     `          8  L   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   p       ��                 �  �  �               �                        O   ����    e�          O   ����    R�          O   ����    ��      m                      �          �  $  �    ���                       �O     
                    � ߱                  �  �                      ��                   �  �                  �1                          �  8      4   �����O      $  �  �  ���                        P     
                    � ߱        �    �  <  L      P      4   ����P      /  �  x                               3   ����(P  �  �   �  4P          O   �  ��  ��  lP                               , �                          
                               �      ��                            ����                                            (          �   p       ��                 1  ;  �               �_                        O   ����    e�          O   ����    R�          O   ����    ��                    8                      ��                  8  :                  ؾ_                           8  �       $   9  d  ���                       `  @         `              � ߱          ��                              ��        �                  ����                                            �           �   p       ��                 A  2  �               ��_                        O   ����    e�          O   ����    R�          O   ����    ��      0`                         � ߱          $  I  �   ���                       E  p   K  8`  ,      -  \  �     D`                �                      ��                  M  �                  ��                           M  <    /   N  �     �                          3   ����X`  (                              3   ����t`  X     
   H                      3   �����`  �        x                      3   �����`         
   �  �                  3   ���� b      $   N  �  ���                               
                    � ߱        �  /	  S  <     L  ,b                      3   ����b  |        l                      3   ����8b            �                      3   ����Lb     /   V  �     �                          3   ����`b                                3   ����|b  H     
   8                      3   �����b  x        h                      3   �����b         
   �  �                  3   ����d      $   V  �  ���                               
                    � ߱        �  /	  [  ,     <  4d                      3   ����d  l        \                      3   ����@d            �                      3   ����Td  �  /   ^  �     �                          3   ����hd          �                      3   �����d  8     
   (                      3   �����d  h        X                      3   �����d         
   �  �                  3   ����f      $   ^  �  ���                               
                    � ߱        �  /	  c       ,  <f                      3   ����f  \        L                      3   ����Hf            |                      3   ����\f  �  /   f  �     �                          3   ����pf  �        �                      3   �����f  (     
                         3   �����f  X        H                      3   �����f         
   x  �                  3   �����g      $   f  �  ���                               
                    � ߱        |	  /	  k  	     	  �g                      3   �����g  L	        <	                      3   �����g            l	                      3   �����g  �
  /   n  �	     �	                          3   �����g  �	        �	                      3   ����h  
     
   
                      3   ����<h  H
        8
                      3   ����Ph         
   h
  x
                  3   �����i      $   n  �
  ���                               
                    � ߱        l  /	  s  �
       �i                      3   �����i  <        ,                      3   �����i            \                      3   �����i  �  /   v  �     �                          3   ����j  �        �                      3   ���� j       
   �                      3   ����8j  8        (                      3   ����Lj         
   X  h                  3   �����j      $   v  �  ���                               
                    � ߱        \  /	  {  �     �  �j                      3   �����j  ,                              3   �����j            L                      3   �����j  �  /	  |  �     �  �j                      3   �����j  �        �                      3   ����k            �                      3   ����k  �  /     $     4                          3   ����0k  d     
   T                      3   ����Dk  �        �                      3   ����Pk         
   �                      3   ����dk  �  /   �  �                                3   ����pk  0     
                          3   �����k  `        P                      3   �����k         
   �                      3   �����k      /   �  �     �                          3   �����k  �     
   �                      3   �����k  ,     
                         3   �����k            L                      3   �����k  �  �     �k                �                      ��                  �  �                  0�_                           �  l  @  /   �       (                          3   ����l  X        H                      3   ����$l  �     
   x                      3   ����Dl  �        �                      3   ����Xl         
   �  �                  3   ����@m      $   �    ���                               
                    � ߱        �  /	  �  l     |  lm                      3   ����Lm  �        �                      3   ����xm            �                      3   �����m  0  /   �                                 3   �����m  H        8                      3   �����m  x     
   h                      3   �����m  �        �                      3   �����m         
   �  �                  3   �����o      $   �    ���                               
  	       	           � ߱        �  /	  �  \     l  p                      3   �����o  �        �                      3   ����(p            �                      3   ����<p  h  /	  �  �       lp                      3   ����Pp  8        (                      3   ����xp            X                      3   �����p  �  /   �  �     �                          3   �����p  �        �                      3   �����p       
   �                      3   �����p  4        $                      3   �����p         
   T  d                  3   ����s      $   �  �  ���                               
                    � ߱        X  /	  �  �     �  <s                      3   ����s  (                              3   ����Hs            H                      3   ����\s  �  /	  �  �     �  �s                      3   ����ps  �        �                      3   �����s            �                      3   �����s  �  /   �        0                          3   �����s  `     
   P                      3   �����s  �        �                      3   �����s         
   �                      3   �����s  �  /   �  �     �                          3   ���� t  ,     
                         3   ����t  \        L                      3   ���� t         
   |                      3   ����4t  X  /   �  �     �                          3   ����@t  �     
   �                      3   ����Tt  (                              3   ����`t         
   H                      3   ����tt  $  /   �  �     �                          3   �����t  �     
   �                      3   �����t  �        �                      3   �����t         
                         3   �����t  �  /   �  P     `                          3   �����t  �     
   �                      3   �����t  �     
   �                      3   �����t            �                      3   �����t  �  /   �       ,                          3   ����u  \     
   L                      3   ����$u  �     
   |                      3   ����0u            �                      3   ����<u      /   �  �     �                          3   ����Pu  (     
                         3   ����lu  X     
   H                      3   ����xu            x                      3   �����u  �*       �u                                      ��                  �  �                  �_                           �  �  l  /   �  D     T                          3   �����u  �        t                      3   �����u  �     
   �                      3   �����u  �        �                      3   ���� v         
                       3   �����v      $   �  @  ���                               
                    � ߱           /	  �  �     �  w                      3   �����v  �        �                      3   ���� w            �                      3   ����4w  \!  /   �  4      D                           3   ����Hw  t         d                       3   ����dw  �      
   �                       3   �����w  �         �                       3   �����w         
   �   !                  3   �����y      $   �  0!  ���                               
  
       
           � ߱        �!  /	  �  �!     �!  �y                      3   �����y  �!        �!                      3   �����y            �!                      3   �����y  �"  /	  �  $"     4"  z                      3   ���� z  d"        T"                      3   ����(z            �"                      3   ����<z  �#  /   �  �"     �"                          3   ����Pz   #        �"                      3   ����lz  0#     
    #                      3   �����z  `#        P#                      3   �����z         
   �#  �#                  3   �����|      $   �  �#  ���                               
                    � ߱        �$  /	  �  $     $$  �|                      3   �����|  T$        D$                      3   �����|            t$                      3   ����}   %  /	  �  �$     �$  <}                      3   ���� }  �$        �$                      3   ����H}            %                      3   ����\}  �%  /   �  L%     \%                          3   ����p}  �%     
   |%                      3   �����}  �%        �%                      3   �����}         
   �%                      3   �����}  �&  /   �  &     (&                          3   �����}  X&     
   H&                      3   �����}  �&        x&                      3   �����}         
   �&                      3   �����}  �'  /   �  �&     �&                          3   �����}  $'     
   '                      3   ����~  T'        D'                      3   ����~         
   t'                      3   ����$~  P(  /   �  �'     �'                          3   ����0~  �'     
   �'                      3   ����D~   (        (                      3   ����P~         
   @(                      3   ����d~  )  /   �  |(     �(                          3   ����p~  �(     
   �(                      3   �����~  �(     
   �(                      3   �����~            )                      3   �����~  �)  /   �  H)     X)                          3   �����~  �)     
   x)                      3   �����~  �)     
   �)                      3   �����~            �)                      3   �����~      /   �  *     $*                          3   ����   T*     
   D*                      3   ����  �*     
   t*                      3   ����(            �*                      3   ����4  �7  4+     H                D+                      ��                  �                    з                           �  �*  �,  /   �  p+     �+                          3   ����\  �+        �+                      3   ����x  �+     
   �+                      3   �����  ,         ,                      3   �����         
   0,  @,                  3   ������      $   �  l,  ���                               
                    � ߱        4-  /	  �  �,     �,  ��                      3   ������  -        �,                      3   ����̀            $-                      3   ������  �.  /   �  `-     p-                          3   �����  �-        �-                      3   �����  �-     
   �-                      3   ����0�   .        �-                      3   ����D�         
    .  0.                  3   ����l�      $   �  \.  ���                               
                    � ߱        $/  /	  �  �.     �.  ��                      3   ����x�  �.        �.                      3   ������            /                      3   ������  �/  /	  �  P/     `/  �                      3   ����̃  �/        �/                      3   �����            �/                      3   �����  1  /   �  �/     �/                          3   �����  ,0        0                      3   ����8�  \0     
   L0                      3   ����P�  �0        |0                      3   ����d�         
   �0  �0                  3   ������      $   �  �0  ���                               
                    � ߱        �1  /	  �  @1     P1  ��                      3   ������  �1        p1                      3   ����Ć            �1                      3   ����؆  L2  /	  �  �1     �1  �                      3   �����  2        2                      3   �����            <2                      3   ����(�  3  /   �  x2     �2                          3   ����<�  �2     
   �2                      3   ����P�  �2        �2                      3   ����\�         
   3                      3   ����p�  �3  /   �  D3     T3                          3   ����|�  �3     
   t3                      3   ������  �3        �3                      3   ������         
   �3                      3   ������  �4  /   �  4      4                          3   ������  P4     
   @4                      3   ����Ї  �4        p4                      3   ����܇         
   �4                      3   ������  |5  /   �  �4     �4                          3   ������  5     
   5                      3   �����  L5        <5                      3   �����         
   l5                      3   ����0�  H6  /   �  �5     �5                          3   ����<�  �5     
   �5                      3   ����X�  6     
   6                      3   ����d�            86                      3   ����p�  7  /   �  t6     �6                          3   ������  �6     
   �6                      3   ������  �6     
   �6                      3   ������            7                      3   ������      /      @7     P7                          3   ����̈  �7     
   p7                      3   �����  �7     
   �7                      3   �����            �7                      3   ���� �      `8     �                p8                      ��                    +                  ��                             �7  �9  /     �8     �8                          3   ����(�  �8        �8                      3   ����D�  9     
   �8                      3   ����d�  <9        ,9                      3   ����x�         
   \9  l9                  3   ����`�      $     �9  ���                               
                    � ߱        `:  /	  	  �9      :  ��                      3   ����l�  0:         :                      3   ������            P:                      3   ������  �;  /     �:     �:                          3   ������  �:        �:                      3   ����܊  �:     
   �:                      3   ������  ,;        ;                      3   �����         
   L;  \;                  3   �����      $     �;  ���                               
                    � ߱        P<  /	    �;     �;  0�                      3   �����   <        <                      3   ����<�            @<                      3   ����P�  �<  /	    |<     �<  ��                      3   ����d�  �<        �<                      3   ������            �<                      3   ������  @>  /     =     (=                          3   ������  X=        H=                      3   ����Ѝ  �=     
   x=                      3   �����  �=        �=                      3   ������         
   �=  �=                  3   ����$�      $     >  ���                               
                    � ߱        �>  /	    l>     |>  P�                      3   ����0�  �>        �>                      3   ����\�            �>                      3   ����p�  x?  /	    ?     ?  ��                      3   ������  H?        8?                      3   ������            h?                      3   ������  D@  /     �?     �?                          3   ����Ԑ  �?     
   �?                      3   �����  @        @                      3   ������         
   4@                      3   �����  A  /     p@     �@                          3   �����  �@     
   �@                      3   ����(�  �@        �@                      3   ����4�         
    A                      3   ����H�  �A  /     <A     LA                          3   ����T�  |A     
   lA                      3   ����h�  �A        �A                      3   ����t�         
   �A                      3   ������  �B  /   "  B     B                          3   ������  HB     
   8B                      3   ������  xB        hB                      3   ������         
   �B                      3   ����ȑ  tC  /   %  �B     �B                          3   ����ԑ  C     
   C                      3   �����  DC     
   4C                      3   ������            dC                      3   �����  @D  /   '  �C     �C                          3   �����  �C     
   �C                      3   ����8�  D     
    D                      3   ����D�            0D                      3   ����P�      /   )  lD     |D                          3   ����d�  �D     
   �D                      3   ������  �D     
   �D                      3   ������            �D                      3   ������        /  (E  8E      ��      4   ������      /  0  dE     tE  �                      3   ����Ԓ            �E                      3   ������               �E          �E  �E    �E                                             ��                              ��        �                  ����                                            �           �   p       ��                 8  g  �               q                        O   ����    e�          O   ����    R�          O   ����    ��      �                         � ߱        �  $  @  �   ���                             �      8            �      ��                  A  F                 hqD                    �     A        �         ��                            7   ����          ��               L�    �            \                  6   A        �   ��         �  L�    �            \                                                        �   0�                   �  �           <�           D�         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��          $  E  d  ���                       ��                         � ߱        ��                         � ߱          $  G  �  ���                       ��                         � ߱        �  $  I  �  ���                             �      h          8         ��                  J  O  P              XsD                    �     J  @      �  <       ��                            7   ����          ��               ��    �            �                  6   J        �   ��         �  ��    �            �                                                        ē   ؓ                                 �           �         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��          $  N  �  ���                       ,�                         � ߱        L�                         � ߱        D  $  P  �  ���                       X�                         � ߱        �  $  R    ���                             �      �	          h	  P	      ��                  S  X  �	              H�                    
     S  p        l       ��                            7   ����          ��               ��    �            �                  6   S        �   ��         �  ��    �            �                                                        l�   ��                   <	  0	           ��           ��         �            	    	        O   ����  e�          O   ����  R�          O   ����  ��          $  W  �	  ���                       Ԕ                         � ߱        ��                         � ߱        t
  $  Y  �	  ���                        �                         � ߱          $  [  H
  ���                                    �          �  �      ��                  \  a  �              �                    L     \  �
      L  �       ��                            7   ����          ��               D�    �            �                  6   \        $   ��           D�    �            �                                                        �   (�                   l  `           4�           <�         �            @   P        O   ����  e�          O   ����  R�          O   ����  ��          $  `  �  ���                       |�                         � ߱        ��                         � ߱            $  b     ���                                     �                                           ��                             ��                             ��                             ��                             ��                            ����                                            (          �   p       ��                 m  w  �               p(                        O   ����    e�          O   ����    R�          O   ����    ��                    8                      ��                  s  u                  �(                           s  �       $   t  d  ���                       ��  @         ��              � ߱          ��                              ��        �                  ����                                            (          �   p       ��                 }  �  �               �)                        O   ����    e�          O   ����    R�          O   ����    ��                    8                      ��                  �  �                  h,                           �  �       $   �  d  ���                       �  @         Е              � ߱          ��                              ��        �                  ����                                            �           �   p       ��                  �  �  �               8-                        O   ����    e�          O   ����    R�          O   ����    ��          �  �   �       ��      4   ������      n   �     �          8�        �     0      D�      4   ����D�      �   �  X�    ��                            ����                                            �           �   p       ��                  �  �  �               T6y                        O   ����    e�          O   ����    R�          O   ����    ��          �               � ߱          h   �  �    �        `�                  
   �  ��               l�    ��                              ��        �                  ����                                            �           �   p       ��                  �  �  �               ,7y                        O   ����    e�          O   ����    R�          O   ����    ��      �     �  x�  }          O   �  ��  ��  ��    ��                            ����                                            �           �   p       ��                  �    �               �:y                        O   ����    e�          O   ����    R�          O   ����    ��      $  /   �  �      �                           3   ������                                  3   ������  `  /   �  P                                3   ����̖  |  �  �      ��          �  �  �      �          �  �  �      ��          �  �  �      �          �  �  �      �          X  /  �       (  8�                      3   �����            H                      3   ����D�  �  /  �  �     �  p�                      3   ����P�            �                      3   ����|�  0  /  �  �        ��                      3   ������                                   3   ������  �  /  �  \     l  ��                      3   ������            �                      3   �����    /  �  �     �  �                      3   ������            �                      3   ����$�  t  /  �  4     D  L�                      3   ����0�            d                      3   ����X�  �  /  �  �     �  ��                      3   ����l�            �                      3   ������  L  /  �         Ę                      3   ������            <                      3   ����И  �  /  �  x     �   �                      3   �����            �                      3   �����  �  �   �   �      �  �   �  @�      �  �   �  ��        �   �  ̙        �   �  �      0  �   �  X�      D  �   �  x�      X  �   �  ��      l  �   �  �      �  �   �  $�      �  �   �  D�      �  �   �  ��      �  �      Л      (  /     �     �                          3   �����                                  3   ����,�  �  /     T     d                          3   ����@�            �                      3   ����X�     /     �     �                          3   ����l�            �                      3   ������  l  /     ,     <                          3   ������            \                      3   ������      /    �         ��                      3   ����Ĝ    ��                            ����                                            P          �   p       ��                     �               LD                        O   ����    e�          O   ����    R�          O   ����    ��      �.                      �                        `                      ��                                      �D                             �       $     �  ���                        �  @         �              � ߱                               �      �                                             ��                              ��        �                  ����                                            (          �   p       ��                   '  �               �D                        O   ����    e�          O   ����    R�          O   ����    ��                    8                      ��                  #  %                  $D                           #  �       $   $  d  ���                       (�  @         �              � ߱          ��                              ��        �                  ����                                  d d     �   ���2��2  � �                                               �                     x                                                  d     D                                                                 \  �#�L�              d                   �                 �.                h       D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST P_Nit P_Pas P_VVeh P_VBie P_VOtros cDrccion hDrccion wWin h_dact_pasivos1 h_dact_pasivos2 h_dact_pasivos3 h_dact_pasivos4 h_dynbrowser1 h_dynbrowser2 h_dynbrowser3 h_dynbrowser4 h_folder h_pupdsav h_pupdsav-2 h_pupdsav-3 h_pupdsav-4 h_vclientesbienes-2 h_vclientesotros h_vclientespasivos h_vclientesvehiculos h_vpas-act Btn_Salir fMain GUI Detalle Activos y Pasivos DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CierraClientesPasivosBienes CLOSE iStartPage ADM-ERROR ADDRECORD currentPage dact_pasivos.wDB-AWARE AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_pasivosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes vpas-act.w EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout folder.w FolderLabels Pasivos|Veh�culos|Bienes|Otros Activos FolderTabWidth17FolderFont5HideOnInitnoDisableOnInitnoObjectLayout Data Page AFTER vclientespasivos.w EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout adm2/dynbrowser.w DisplayedFieldsNombre,Val_Comercial,Val_Cuota,Val_SaldoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout pupdsav.w AddFunctionOne-RecordPanelTypeUpdateAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout Update TableIO vclientesvehiculos.w DisplayedFieldsNombre,Modelo,Placa,Val_Comercial,Prenda_HipotecaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout vclientesbienes.w DisplayedFieldsNombre,Dir_Bienes,Lugar_Bienes,Val_Comercial,Matricula_inmobiliaria,Prenda_HipotecaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout vclientesotros.w DisplayedFieldsNombre,Val_Comercial,DetalleEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_pasivosUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout ADM-CREATE-OBJECTS v-suma Act_Pasivos CALCULOS_BIENES CANCELRECORD COPYRECORD DISABLE_UI ENABLE_UI EXITOBJECT 1,2,3,4 addrecord copyrecord updaterecord cancelrecord updatemode closeQuery nit = ' ' AND Cod_Tipo = 4 setQueryWhere openQuery ' AND Cod_Tipo = 2 ' AND Cod_Tipo = 1 ' AND Cod_Tipo = 3 'Entidad' setBrowseColumnLabels INITIALIZEOBJECT c UPDATEMODE UPDATERECORD &Salir Icli_nit |  �#      �*      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   F	  ^	  `	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props Q  R  S  T  V  W  X  Y  Z  [  \  ]  `  c  d  e  f              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  T	  �	     =                                   �  �  �	  �	     >                                   �  �	  $
     ?                                   �  �  �  �  �	  d
     @                                           4
  �
     A                                       t
  �
     B               �
                  addrecord   8  9  :  ;                 currentPage �
  \  \   C   �
          H                  adm-create-objects  I  K  M  N  S  V  [  ^  c  f  k  n  s  v  {  |    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �           	                    "  %  '  )  +  -  /  0  2            �     v-suma    (     D   �                            calculos_bienes @  A  E  F  G  I  J  N  O  P  R  S  W  X  Y  [  \  `  a  b  g  �  �     E               �                  cancelrecord    s  t  u  w  |       F               �                  copyrecord  �  �  �  �  �  T     G               H                  disable_UI  �  �  �  �    �     H               �                  enable_UI   �  �  �  d  �     I               �                  exitObject  �  �  �  �  8  #   J               $                  initializeObject    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                           �        c   �       K       �                        updatemode          �  l     L               \                  updaterecord    #  $  %  '  ,  �       �  �  �                      �          �  
   appSrvUtils �       �     cDrccion              
   hDrccion    (          
   wWin    L       <  
   h_dact_pasivos1 p       `  
   h_dact_pasivos2 �       �  
   h_dact_pasivos3 �       �  
   h_dact_pasivos4 �    	   �  
   h_dynbrowser1        
   �  
   h_dynbrowser2   $         
   h_dynbrowser3   H       8  
   h_dynbrowser4   h       \  
   h_folder    �       |  
   h_pupdsav   �       �  
   h_pupdsav-2 �       �  
   h_pupdsav-3 �       �  
   h_pupdsav-4        �  
   h_vclientesbienes-2 8       $  
   h_vclientesotros    `       L  
   h_vclientespasivos  �       t  
   h_vclientesvehiculos    �       �  
   h_vpas-act  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager              
   gshRIManager    H        4  
   gshSecurityManager  p        \  
   gshProfileManager   �        �  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager   �  
 
     �  
   gshWebManager                 gscSessionId    4        $     gsdSessionObj   X        H  
   gshFinManager   |        l  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj          �     gsdRenderTypeObj    4              gsdSessionScopeObj  P       H  
   ghProp  p       d  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer             cObjectName 4       ,     iStart  T       H     cAppService t        h     cASDivision �    !   �     cServerOperatingMode    �    "   �     cFields       #   �     iStartPage  �       �        P_Nit                  P_Pas   <       4        P_VVeh  \       T        P_VBie           t        P_VOtros             �  Act_Pasivos          7   K  L  _  �  �  �  �  �  �  �          .  :  ;  <  >  @  A  B  F  G  J  K  L  M  O  Q  S  U  V  W  Z  \  ]  _  `  a  b  c  i  k  q  s  u  v  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  !
  "
  #
  $
  %
  &
  '
  (
  )
  *
  +
  ,
  -
  .
  /
  0
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
  _  k  �  �  �  �  �  �  �  �  �  �  �  �    3  5  J  �  �  �                #  $  A  U  �  
        �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  Q  x  �  �  �  �  �  �  �                 !  "  $  &      H� $ c:\progress10\src\adm2\windowmn.i    �  f!  c:\progress10\src\adm2\containr.i    �  � # c:\progress10\src\adm2\custom\containrcustom.i     ��  c:\progress10\src\adm2\visual.i  H  # " c:\progress10\src\adm2\custom\visualcustom.i t  �<  c:\progress10\src\adm2\appserver.i   �  �� ! c:\progress10\src\adm2\custom\appservercustom.i  �  I�  c:\progress10\src\adm2\smart.i     Ds   c:\progress10\gui\fn D  tw  c:\progress10\src\adm2\custom\smartcustom.i  d  Q.  c:\progress10\gui\set    �  ��  c:\progress10\src\adm2\cntnprop.i    �  ��  c:\progress10\src\adm2\custom\cntnpropcustom.i   �  P  c:\progress10\src\adm2\custom\cntnprtocustom.i   ,  F>  c:\progress10\src\adm2\visprop.i h  �I  c:\progress10\src\adm2\custom\vispropcustom.i    �  ��  c:\progress10\src\adm2\custom\visprtocustom.i    �  �l 
 c:\progress10\src\adm2\appsprop.i       ɏ  c:\progress10\src\adm2\custom\appspropcustom.i   <   V  c:\progress10\src\adm2\custom\appsprtocustom.i   x   i$  c:\progress10\src\adm2\smrtprop.i    �   �j  c:\progress10\gui\get    �   �  c:\progress10\src\adm2\custom\smrtpropcustom.i   !  ��  c:\progress10\src\adm2\custom\smrtprtocustom.i   D!  ��  c:\progress10\src\adm2\smrtprto.i    �!  Su  c:\progress10\src\adm2\globals.i �!  M�  c:\progress10\src\adm2\custom\globalscustom.i    �!  )a  c:\progress10\src\adm2\custom\smartdefscustom.i  "  �  c:\progress10\src\adm2\appsprto.i    T"  ��  c:\progress10\src\adm2\custom\appserverdefscustom.i  �"  �X 	 c:\progress10\src\adm2\visprto.i �"  !�  c:\progress10\src\adm2\custom\visualdefscustom.i �"  n�  c:\progress10\src\adm2\cntnprto.i    ,#  ;  c:\progress10\src\adm2\custom\containrdefscustom.i   \#  ~�  c:\progress10\src\adm2\widgetprto.i  �#  e�  c:\progress10\gui\adecomm\appserv.i  �#  �<    \\192.168.101.9\desarrollo\prg\w_ClientesPasivosBienes.w       )      @$     �  $   P$  �   �      `$  �   �     p$     p     �$  �   k     �$     I     �$  �   A     �$     �  #   �$  �   �     �$     �      �$  �   �     �$     �       %  �   �     %     �       %  r   �     0%  n   �     @%     8  "   P%  i   3     `%          p%  P   �     �%  �   �     �%     �  !   �%  �   �     �%     p     �%  �   o     �%     M     �%  �   K     �%     )      &  g        &     �      &  O   �     0&  �   b     @&     `      P&  �   0     `&     �     p&  �   �     �&     �     �&  �   �     �&     �     �&  �   �     �&     e     �&  �   d     �&     B     �&  �   1      '          '  �         '     �     0'  }   �     @'     �     P'     @     `'     �     p'     �     �'  7   h     �'  �   _     �'  O   Q     �'     @     �'     �
     �'  �   �
     �'  �   �
     �'  O   �
      (     �
     (     4
      (  �   
     0(  x   
  
   @(  M   �	     P(     �	     `(     �	     p(  a   ~	  
   �(  �  ]	     �(     >	     �(  �  	     �(  O   �     �(     �     �(     �     �(  �   �     �(     �      )     �     )  x   �      )     �     0)     Y     @)     U     P)     A     `)     (     p)  Q     
   �)     �     �)     �  
   �)     r     �)     X  
   �)  f   -     �)     �  	   �)  "   �     �)     t      *     S     *  Z         *     
     0*     �     @*     �     P*     �     `*     g     p*  '   �       �*     @      �*            �*           