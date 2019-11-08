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
{Incluido\variable.i "shared"}
        

DEF VAR W_Nuevo        AS LOG NO-UNDO.
DEF VAR v-cod-factor1  AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FProSarlaft

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Salir RECT-2 BUTTON-1 Btn_Imprimir ~
Btn_Consulta BtnMtrizRiesgos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dPSarlaft AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpSarlaft AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnMtrizRiesgos 
     LABEL "M Riesgos" 
     SIZE 10 BY 1.62 TOOLTIP "Matriz De Riesgos".

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 10" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Salir DEFAULT  NO-FOCUS
     LABEL "&Salir" 
     SIZE 9 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11.86 BY 7.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FProSarlaft
     Btn_Salir AT ROW 21.04 COL 119.86 WIDGET-ID 12
     BUTTON-1 AT ROW 1.62 COL 119 WIDGET-ID 6
     Btn_Imprimir AT ROW 3.38 COL 119 WIDGET-ID 4
     Btn_Consulta AT ROW 5.15 COL 119 WIDGET-ID 2
     BtnMtrizRiesgos AT ROW 6.92 COL 119 WIDGET-ID 16
     RECT-2 AT ROW 1.35 COL 118 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 146.14 BY 26.42
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
         TITLE              = "Factores Riesgo"
         HEIGHT             = 21.73
         WIDTH              = 129.29
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
/* SETTINGS FOR FRAME FProSarlaft
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Factores Riesgo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Factores Riesgo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnMtrizRiesgos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnMtrizRiesgos wWin
ON CHOOSE OF BtnMtrizRiesgos IN FRAME FProSarlaft /* M Riesgos */
DO:
    RUN pMtrizRiesgosSarlaft.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME FProSarlaft /* Button 10 */
DO:
  IF W_Nuevo THEN
     RETURN.
    
  RUN wconsproSarlaft.w.  

/*   APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Sarlaft. */
/*                                                            */
/*   FRAME F_Consulta:HIDDEN = NO.                            */
     /*RUN Consulta IN h_vpSarlaft
    ( INPUT v-cod-factor1 /* INTEGER */).*/
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME FProSarlaft /* Button 8 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "L-FacRiesgo.lst".
  {Incluido\IMPRIMIR.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME FProSarlaft /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME FProSarlaft /* Button 1 */
DO:
  RUN W-InfDia.w NO-ERROR.
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
             INPUT  'dPSarlaft.wDB-AWARE':U ,
             INPUT  FRAME FProSarlaft:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedSarlaftOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch20RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dPSarlaft ).
       RUN repositionObject IN h_dPSarlaft ( 19.04 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 3.73 , 17.00 ) */

       RUN constructObject (
             INPUT  'vpSarlaft.w':U ,
             INPUT  FRAME FProSarlaft:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNamevpSarlaftLogicalObjectNamevpSarlaftPhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpSarlaft ).
       RUN repositionObject IN h_vpSarlaft ( 1.54 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 19.65 , 116.00 ) */

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME FProSarlaft:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav ).
       RUN repositionObject IN h_pupdsav ( 8.81 , 118.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav ( 11.85 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vpSarlaft. */
       RUN addLink ( h_dPSarlaft , 'Data':U , h_vpSarlaft ).
       RUN addLink ( h_vpSarlaft , 'Update':U , h_dPSarlaft ).
       RUN addLink ( h_pupdsav , 'TableIO':U , h_vpSarlaft ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_pupdsav ,
             BtnMtrizRiesgos:HANDLE IN FRAME FProSarlaft , 'AFTER':U ).
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
  ENABLE Btn_Salir RECT-2 BUTTON-1 Btn_Imprimir Btn_Consulta BtnMtrizRiesgos 
      WITH FRAME FProSarlaft IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FProSarlaft}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  SUBSCRIBE "ProSarlaft" ANYWHERE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR Linea         AS CHARACTER FORMAT "X(120)" INITIAL "".
    DEF VAR W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEF VAR Raya          AS CHARACTER INITIAL "-".
    DEF VAR v-EveFacto    AS CHAR NO-UNDO.
    DEF VAR v-Clase_ROperativo AS CHAR NO-UNDO.
    
    
    DEFINE FRAME F-Encabezado
        HEADER
        "ENTIDAD : "                         AT 2
        W_Nom_Entidad                        AT 14 FORMAT "X(30)"
        "Página:"                            AT 75 PAGE-NUMBER FORMAT ">>>9"
        "Agencia : "                         AT 2
        TRIM(W_Nom_Agencia)                  AT 14 FORMAT "X(30)" SKIP
        "Factores Sarlaft"  AT 35
         FILL("_",200)   FORMAT "x(200)"     AT 1  SKIP
        "No."                                AT 3
        "Proceso"                            AT 9
        "SubProceso"                         AT 30
        "Actividad"                          AT 61
        "Factor de"                          AT 92
        "Riesgo Operativos"                  AT 108
        "Descripcion como puede (ó pudo)"    AT 134
        "Descripcion de "                    AT 185
        "Severidad y/o"                      AT 236   
       /* "Frecuencia y/o"                     
        "Calificación inicial"
        "Descripción de la"
        "Descripción de"
        "Tipo"
        "Responsable"
        "Cargo Responsable"*/
        SKIP
        "Riesgo"
        "Riesgo"             AT 95
        "Suceder el riesgo"  AT 134
        "consecuencias"      AT 185
        "consecuencias"      AT 236
        "Probabilidad"       
        SKIP(0)
        FILL("_",200) FORMAT "x(200)" AT 1
        WITH DOWN WIDTH 255 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.
          

    DEFINE FRAME F-Piepagina
    HEADER 
        "FECHA:"                AT 2 
        TODAY                   AT 10 FORMAT "99/99/9999"
        W_Nom_Agencia           AT 40 FORMAT "X(30)" FONT 9
        "HORA:"                 AT 70 STRING(TIME,"HH:MM AM")
    WITH DOWN WIDTH 132 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
       
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Piepagina.
        
    FOR EACH pro_Sarlaft NO-LOCK:
      IF pro_Sarlaft.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo".         
      
      /*Factor de riesgo Cod_EveFacto tipo=37 */
      FIND FIRST varios WHERE varios.tipo   = 39 
                          AND varios.codigo = pro_Sarlaft.Cod_EveFacto
      NO-LOCK NO-ERROR.
      IF AVAILABLE varios THEN
        v-EveFacto = varios.descripcion.

      /*Clase de Riesgo operativo Clase_ROperativo  tipo = 35*/
      FIND FIRST varios WHERE varios.tipo   = 40
                          AND varios.codigo = pro_Sarlaft.Clase_ROperativo
      NO-LOCK NO-ERROR.
      IF AVAILABLE varios THEN
        v-Clase_ROperativo = varios.descripcion.

      
      DISPLAY pro_Sarlaft.Cod_Factor 
              pro_Sarlaft.proceso          AT 9   FORMAT "X(20)"
              pro_Sarlaft.Subproceso       AT 30  FORMAT "X(30)"
              pro_Sarlaft.Actividad        AT 61  FORMAT "X(30)"
              v-eveFacto                   AT 92  FORMAT "X(15)"
              v-Clase_ROperativo           AT 108 FORMAT "X(25)"
              pro_Sarlaft.nombre           AT 134 FORMAT "X(50)"
              Pro_Sarlaft.Descripcion_Con  AT 185 FORMAT "X(50)"
      WITH FRAME F-PSarlaft  DOWN WIDTH 250 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
      DOWN WITH FRAME F-PSarlaft.
    END.  
    PAGE.  
                
    /*
    Cod_Factor Proceso Subproceso Actividad Cod_EveFactor Clase_ROperativo
     Cargo_Resp Cod_Controles   
    Cod_Probabilidad Cod_Severidad Cod_UbFactor Descripcion_Con Des_Controles 
     Nombre  Responsable_Cont  
     Tipo Fec_Creacion Fec_Retiro Estado 
    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProSarlaft wWin 
PROCEDURE ProSarlaft :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     ProSarlaft  
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    MESSAGE c
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF NOT c = "0" 
    THEN DO:
        DYNAMIC-FUNCTION('closeQuery':U IN h_dPSarlaft).
        DYNAMIC-FUNCTION('setQueryWhere':U IN h_dPSarlaft,'cod_Factor = ' + c).  
        DYNAMIC-FUNCTION('openQuery':U IN h_dPSarlaft).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

