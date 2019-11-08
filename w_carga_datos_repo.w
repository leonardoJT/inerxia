&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


DEF VAR hSper001 AS HANDLE NO-UNDO.
RUN repo-super001.p PERSISTENT SET hSper001.
SESSION:ADD-SUPER-PROCEDURE(hSper001).
DEF BUFFER brepo FOR repositorio.repositorio.
DEF BUFFER bconf FOR repositorio.conf_repositorio.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEF VAR W_FecMes AS DATE NO-UNDO.
W_FecMes = TODAY.
DEF VAR iPriodoi AS INTEGER NO-UNDO INITIAL 999999.
DEF VAR iPriodof AS INTEGER NO-UNDO.

DEF VAR cOpcion AS CHAR NO-UNDO INITIAL "Total".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-314 COMBO-BOX-1 daFchaPrcso ~
COMBO-BOX-2 BUTTON-5 BUTTON-1 Btn_Termina 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-1 daFchaPrcso COMBO-BOX-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLlnaRpstrio wWin 
FUNCTION fLlnaRpstrio RETURNS CHARACTER
  (daFchaPrcso AS DATE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_drepo_control_cargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-repo-info AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-1 
     LABEL "Procesar" 
     SIZE 10 BY 1.62 TOOLTIP "Continuar - Procesar".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     LIST-ITEM-PAIRS "Todos","0"
     DROP-DOWN-LIST
     SIZE 48.72 BY 1 TOOLTIP "Agencias Disponibles Con Por Lo Menos Un Crédito" NO-UNDO.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Total","0",
                     "Los Desembolsados En El Mes","1"
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Opciones Del Repositorio" NO-UNDO.

DEFINE VARIABLE daFchaPrcso AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Fecha Del Proceso" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 11.31.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 6.73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     COMBO-BOX-1 AT ROW 1.81 COL 17.29 COLON-ALIGNED HELP
          "Seleccione La Agencia" WIDGET-ID 16
     daFchaPrcso AT ROW 3.15 COL 17.29 COLON-ALIGNED WIDGET-ID 4
     COMBO-BOX-2 AT ROW 3.15 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     BUTTON-5 AT ROW 5.31 COL 70.43 WIDGET-ID 24
     BUTTON-1 AT ROW 7.19 COL 70.43 WIDGET-ID 2
     Btn_Termina AT ROW 9.88 COL 72 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 28
     "Proceso Carga Repositorio:" VIEW-AS TEXT
          SIZE 25 BY .62 TOOLTIP "Proceso Carga Repositorio" AT ROW 1 COL 3.29 WIDGET-ID 8
          FGCOLOR 7 
     RECT-1 AT ROW 1.27 COL 1.29 WIDGET-ID 6
     RECT-314 AT ROW 5.04 COL 69.29 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.57 BY 11.69
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "REPOSITORIO - Carga Masiva De Datos"
         HEIGHT             = 11.69
         WIDTH              = 82.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* REPOSITORIO - Carga Masiva De Datos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* REPOSITORIO - Carga Masiva De Datos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
/*   APPLY "CLOSE":U TO THIS-PROCEDURE. */
/*   RETURN NO-APPLY.                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina wWin
ON CHOOSE OF Btn_Termina IN FRAME fMain /* Terminar Consulta */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Procesar */
DO:
    MESSAGE "USTED ESTA GENERANDO SOBRE LA BASE DE DATOS: " skip
            "'" PDBNAME(2) "'" SKIP
            "LA OPCION DE CREDITOS: '" 
            cOpcion "'"
            SKIP(2)
            "DESEA CONTINUAR?"
            VIEW-AS ALERT-BOX  QUESTION 
            BUTTONS YES-NO TITLE "P R E G U N T A" UPDATE lsino AS LOGICAL.
    IF NOT lsino THEN RETURN NO-APPLY.
    SESSION:set-WAIT("general").
    fLlnaRpstrio(dafchaprcso).
    SESSION:set-WAIT("").
    DYNAMIC-FUNCTION('closeQuery':U IN h_drepo_control_cargas).
    DYNAMIC-FUNCTION('openQuery':U IN h_drepo_control_cargas).
    MESSAGE "FIN DEL PROCESO"
        VIEW-AS ALERT-BOX INFORMATION.
/*
    DEFINE VAR Listado     AS CHARACTER INITIAL "".
    ASSIGN Listado = W_PathSpl + "-InfInteg-" + W_Usuario.

    {INCLUIDO\Imprimir.I "Listado"} 
*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME fMain /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-1 wWin
ON VALUE-CHANGED OF COMBO-BOX-1 IN FRAME fMain /* Agencia */
DO:
    PUBLISH "AgnciaSlccionda" (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-2 wWin
ON VALUE-CHANGED OF COMBO-BOX-2 IN FRAME fMain
DO:
    cOpcion = entry(lookup(self:SCREEN-VALUE,SELF:LIST-ITEM-PAIRS) - 1,SELF:LIST-ITEM-PAIRS,",").
    PUBLISH "SeguimientoUOtorgamiento" (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchaPrcso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchaPrcso wWin
ON LEAVE OF daFchaPrcso IN FRAME fMain /* Fecha Proceso */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&SELF-NAME}.  
        IF daFchaPrcso = ?
        THEN DO:
            MESSAGE "Indique La Fecha Del Proceso"
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'drepo_control_cargas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedrepo_control_cargasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_drepo_control_cargas ).
       RUN repositionObject IN h_drepo_control_cargas ( 1.81 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsperiodo,TotalRegistros,fechaCarga,cHra,usuario,Nombre,agencia,Nombre-2EnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdrepo_control_cargasUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 4.50 , 3.29 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 7.81 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'w-repo-info.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-repo-info ).
       /* Position in AB:  ( 1.81 , 72.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_drepo_control_cargas , 'Data':U , h_dynbrowser ).
       RUN addLink ( h_dynbrowser , 'Update':U , h_drepo_control_cargas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             COMBO-BOX-2:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY COMBO-BOX-1 daFchaPrcso COMBO-BOX-2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-314 COMBO-BOX-1 daFchaPrcso COMBO-BOX-2 BUTTON-5 BUTTON-1 
         Btn_Termina 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Control wWin 
PROCEDURE Imp_Control :
/*------------------------------------------------------------------------------
  Purpose: Imp_Control    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
    
    DISPLAY STRING(W_Nom_Entidad,"X(40)") + "   -  Integridad Aplicativo" SKIP
    "                              Fecha : " + STRING(W_FecMes,"99/99/9999") FORM "X(80)"
    SKIP (1)
    WITH WIDTH 150 NO-LABELS.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    DEF VAR cLsta AS CHAR NO-UNDO.
    RUN SUPER.
    DYNAMIC-FUNCTION('setBGColor':U IN h_dynbrowser,17).
    DYNAMIC-FUNCTION('setFont':U IN h_dynbrowser,5).
        
    DO WITH FRAME {&FRAME-NAME}:
        COMBO-BOX-2:SCREEN-VALUE = "0".
        daFchaPrcso = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
        daFchaPrcso:SCREEN-VALUE = STRING(daFchaPrcso).
        cLsta = " Todos,0,".
        FOR EACH agencias NO-LOCK
            WHERE
                CAN-FIND(FIRST creditos 
                            WHERE creditos.agencia = agencias.agencia
                        AND NOT (creditos.estado > 2 AND creditos.estado < 5)):
            cLsta = cLsta + agencias.nombre + "," + string(agencias.agencia)+ ",". 
        END.
        cLsta = TRIM(clsta,",").
        COMBO-BOX-1:LIST-ITEM-PAIRS = cLsta NO-ERROR.
        COMBO-BOX-1:SCREEN-VALUE = "0".
        APPLY "entry" TO COMBO-BOX-1.
    END.
    PUBLISH "AgnciaSlccionda" ("0").
    PUBLISH "UsuarioActual" (w_usuario).
    PUBLISH "AgenciaActual" (w_agencia).
    PUBLISH "SeguimientoUOtorgamiento" ("0").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:  ProcesoImprimir   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* {Incluido\RepEncabezado.I} */

  /*ASSIGN W_Reporte    = "Reporte   : Detalle Crediticio Por C/Obligaciòn                       Fecha : " +
                         STRING(W_Fecha,"99/99/9999") + "   Hora :" + STRING(TIME,"HH:MM:SS")
  W_EncColumna = */
        
  RUN Imp_Control.  /*Imprime El Informe*/
    
  DEF VAR cl AS CHAR NO-UNDO.
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
  
  PUT UNFORMATTED FILL("-",128) SKIP.
  cl = "".
  OVERLAY(cl,1,12) = "TABLA".
  OVERLAY(cl,14,20) = "CAMPO ORIGEN".
  OVERLAY(cl,35,12) = "INDICE".
  OVERLAY(cl,48,12) = "TIPO DATO".
  OVERLAY(cl,61,12) = "CAM. DESTINO".
  OVERLAY(cl,74,12) = "NOM.CAM.CAL.".
  OVERLAY(cl,87,20) = "DES. CAM. CALCULADO".
  OVERLAY(cl,108,20) = "PROCEDIMIENTO".
  PUT UNFORMATTED cl SKIP.
  PUT UNFORMATTED FILL("-",128) SKIP(1).
  FOR EACH repositorio._file NO-LOCK
      WHERE
          repositorio._file._file-name = "repositorio":
      FOR EACH repositorio._field NO-LOCK
          WHERE
              repositorio._field._file-recid = RECID(repositorio._file)
          AND (   repositorio._field._field-name BEGINS "char"
               OR repositorio._field._field-name BEGINS "int"
               OR repositorio._field._field-name BEGINS "dec"
               OR repositorio._field._field-name BEGINS "dat"
               OR repositorio._field._field-name BEGINS "log")
          AND NOT repositorio._field._field-name = repositorio._field._label
          BY 
              repositorio._field._order:

          cl = "".
          OVERLAY(cl,1,12) = entry(1,repositorio._field._label,".").
          OVERLAY(cl,14,20) = entry(2,repositorio._field._label,".") NO-ERROR.
          FIND bconf NO-LOCK
              WHERE
                  bconf.BaseDestino = "repositorio"
              AND bconf.Tabladestino = "repositorio"
              AND bconf.campodestino = repositorio._field._field-name NO-ERROR.
          OVERLAY(cl,35,12) = IF AVAILABLE bconf THEN (IF NOT bconf.indice = 0 THEN string(bconf.indice,"99") ELSE "") ELSE "".
          OVERLAY(cl,48,12) = repositorio._field._data-type NO-ERROR.
          OVERLAY(cl,61,12) = repositorio._field._field-name NO-ERROR.
          OVERLAY(cl,74,12) = IF AVAILABLE bconf THEN bconf.NomCampoCal ELSE "".
          OVERLAY(cl,87,20) = IF AVAILABLE bconf THEN bconf.desCampoCal ELSE "".
          OVERLAY(cl,108,20) = IF AVAILABLE bconf THEN bconf.procCampoCal ELSE "".
          PUT UNFORMATTED cl SKIP.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLlnaRpstrio wWin 
FUNCTION fLlnaRpstrio RETURNS CHARACTER
  (daFchaPrcso AS DATE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(dafchaprcso).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

