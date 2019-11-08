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
&Scoped-define FRAME-NAME FProsaro

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-293 Btn_Salir BUTTON-1 ~
Btn_Imprimir Btn_Consulta BtnMtrizEvntos BtnMtrizRiesgos daFchaI daFchaF 
&Scoped-Define DISPLAYED-OBJECTS daFchaI daFchaF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dPsaro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pupdsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpsaro AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnMtrizEvntos 
     LABEL "M Eventos" 
     SIZE 10 BY 1.62 TOOLTIP "Matriz De Eventos".

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

DEFINE VARIABLE daFchaF AS DATE FORMAT "99/99/99":U 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fecha Fnal" NO-UNDO.

DEFINE VARIABLE daFchaI AS DATE FORMAT "99/99/99":U 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fecha Inicial" NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11.86 BY 8.54.

DEFINE RECTANGLE RECT-293
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 42 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FProsaro
     Btn_Salir AT ROW 22.12 COL 120.86 WIDGET-ID 12
     BUTTON-1 AT ROW 1.54 COL 120 WIDGET-ID 6
     Btn_Imprimir AT ROW 3.08 COL 120 WIDGET-ID 4
     Btn_Consulta AT ROW 4.69 COL 120 WIDGET-ID 2
     BtnMtrizEvntos AT ROW 6.31 COL 120 WIDGET-ID 14
     BtnMtrizRiesgos AT ROW 7.92 COL 120 WIDGET-ID 16
     daFchaI AT ROW 22.54 COL 44 COLON-ALIGNED WIDGET-ID 18
     daFchaF AT ROW 22.54 COL 64 COLON-ALIGNED WIDGET-ID 20
     "Rango Fechas Matriz Eventos:" VIEW-AS TEXT
          SIZE 27 BY .62 TOOLTIP "Rango De Fechas Para La Matriz De Eventos" AT ROW 22 COL 40 WIDGET-ID 24
     RECT-2 AT ROW 1.27 COL 119 WIDGET-ID 8
     RECT-293 AT ROW 22.27 COL 39 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 130.72 BY 23.12
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
         HEIGHT             = 22.88
         WIDTH              = 130.43
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
/* SETTINGS FOR FRAME FProsaro
   FRAME-NAME                                                           */
ASSIGN 
       RECT-293:PRIVATE-DATA IN FRAME FProsaro     = 
                "Rango Fechas".

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


&Scoped-define SELF-NAME BtnMtrizEvntos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnMtrizEvntos wWin
ON CHOOSE OF BtnMtrizEvntos IN FRAME FProsaro /* M Eventos */
DO:
    DEF VAR l AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN daFchaI daFchaf.
    END.
    MESSAGE "Se Generará La Matriz De Eventos Entre Las Fechas:" SKIP
            STRING(daFchaI,"99/99/9999") " - " STRING(daFchaf,"99/99/9999") SKIP
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l.
    IF NOT l THEN RETURN NO-APPLY.
    IF NOT (daFchai <= daFchaF AND NOT dafchai = ? AND NOT dafchaf = ?) 
    THEN DO:
        MESSAGE "ERROR: Rango De Fechas Para Generación De Matriz De Eventos Incorrecto"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'ENTRY' TO daFchaI.
        RETURN NO-APPLY.
    END.
    SESSION:SET-WAIT('general').
    RUN pMtrizEvntos.p(STRING(daFchai) + CHR(1) + STRING(daFchaf)).    
    SESSION:SET-WAIT('').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnMtrizRiesgos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnMtrizRiesgos wWin
ON CHOOSE OF BtnMtrizRiesgos IN FRAME FProsaro /* M Riesgos */
DO:
    RUN pMtrizRiesgos.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME FProsaro /* Button 10 */
DO:
  IF W_Nuevo THEN
     RETURN.
    
  RUN wconsprosaroa.w.  

/*   APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Saro. */
/*                                                            */
/*   FRAME F_Consulta:HIDDEN = NO.                            */
     /*RUN Consulta IN h_vpsaro
    ( INPUT v-cod-factor1 /* INTEGER */).*/
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME FProsaro /* Button 8 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "L-FacRiesgo.lst".
  {Incluido\IMPRIMIR.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME FProsaro /* Salir */
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
ON CHOOSE OF BUTTON-1 IN FRAME FProsaro /* Button 1 */
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
             INPUT  'dPsaro.wDB-AWARE':U ,
             INPUT  FRAME FProsaro:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedsaroOpenOnInitnoPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dPsaro ).
       RUN repositionObject IN h_dPsaro ( 22.35 , 110.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vpsaro.w':U ,
             INPUT  FRAME FProsaro:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpsaro ).
       RUN repositionObject IN h_vpsaro ( 3.69 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 18.04 , 116.00 ) */

       RUN constructObject (
             INPUT  'pupdsav.w':U ,
             INPUT  FRAME FProsaro:HANDLE ,
             INPUT  'AddFunctionOne-RecordPanelTypeSaveAddFunctionOne-RecordEdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksTableIO-SourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionhorizontalLogicalObjectNameDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pupdsav ).
       RUN repositionObject IN h_pupdsav ( 9.88 , 119.00 ) NO-ERROR.
       RUN resizeObject IN h_pupdsav ( 11.85 , 12.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vpsaro. */
       RUN addLink ( h_dPsaro , 'Data':U , h_vpsaro ).
       RUN addLink ( h_vpsaro , 'Update':U , h_dPsaro ).
       RUN addLink ( h_pupdsav , 'TableIO':U , h_vpsaro ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vpsaro ,
             Btn_Imprimir:HANDLE IN FRAME FProsaro , 'AFTER':U ).
       RUN adjustTabOrder ( h_pupdsav ,
             BtnMtrizRiesgos:HANDLE IN FRAME FProsaro , 'AFTER':U ).
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
  DISPLAY daFchaI daFchaF 
      WITH FRAME FProsaro IN WINDOW wWin.
  ENABLE RECT-2 RECT-293 Btn_Salir BUTTON-1 Btn_Imprimir Btn_Consulta 
         BtnMtrizEvntos BtnMtrizRiesgos daFchaI daFchaF 
      WITH FRAME FProsaro IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FProsaro}
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
    SUBSCRIBE "ProSaro" ANYWHERE.
    DO WITH FRAME  {&FRAME-NAME}:
        daFchaI = date(month(TODAY),1,YEAR(TODAY)).
        daFchaF = TODAY - 1.
        DISPLAY daFchaI daFchaF.
        daFchaI:TOOLTIP = "Fecha Inicial En Formato " + SESSION:DATE-FORMAT.
        daFchaf:TOOLTIP = "Fecha Final En Formato " + SESSION:DATE-FORMAT.
    END.
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
        "Factores SARO"  AT 35
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
        
    FOR EACH pro_saro NO-LOCK:
      IF pro_saro.Estado = 1 THEN
        ASSIGN W_EstadoInf = "Activo".
      ELSE
        ASSIGN W_EstadoInf = "Inactivo".         
      
      /*Factor de riesgo Cod_EveFacto tipo=37 */
      FIND FIRST varios WHERE varios.tipo   = 37 
                          AND varios.codigo = pro_saro.Cod_EveFacto
      NO-LOCK NO-ERROR.
      IF AVAILABLE varios THEN
        v-EveFacto = varios.descripcion.

      /*Clase de Riesgo operativo Clase_ROperativo  tipo = 35*/
      FIND FIRST varios WHERE varios.tipo   = 35
                          AND varios.codigo = pro_saro.Clase_ROperativo
      NO-LOCK NO-ERROR.
      IF AVAILABLE varios THEN
        v-Clase_ROperativo = varios.descripcion.

      
      DISPLAY pro_saro.Cod_Factor 
              pro_saro.proceso          AT 9   FORMAT "X(20)"
              pro_saro.Subproceso       AT 30  FORMAT "X(30)"
              pro_saro.Actividad        AT 61  FORMAT "X(30)"
              v-eveFacto                AT 92  FORMAT "X(15)"
              v-Clase_ROperativo        AT 108 FORMAT "X(25)"
              pro_saro.nombre           AT 134 FORMAT "X(50)"
              Pro_Saro.Descripcion_Con  AT 185 FORMAT "X(50)"
      WITH FRAME F-PSaro  DOWN WIDTH 250 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
      DOWN WITH FRAME F-PSaro.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProSaro wWin 
PROCEDURE ProSaro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DYNAMIC-FUNCTION('closeQuery':U IN h_dPsaro).
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dPsaro,'cod_Factor = ' + c).  
    DYNAMIC-FUNCTION('openQuery':U IN h_dPsaro).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

