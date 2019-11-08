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

{Incluido\VARIABLE.I "SHARED"}

DEFINE VARIABLE vcString AS CHARACTER INITIAL "" NO-UNDO. /*cadenas de caracteres varios*/
DEFINE VARIABLE vlestado AS LOGICAL     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-153 Btn_Imp BtnDone 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Archivo 
       MENU-ITEM m_Informacin   LABEL "Información"   
       MENU-ITEM m_Salir        LABEL "Salir"         .

DEFINE SUB-MENU m_Informes 
       MENU-ITEM m_Informes2    LABEL "Informes"      .

DEFINE MENU MENU-BAR-wWin MENUBAR
       SUB-MENU  m_Archivo      LABEL "Archivo"       
       SUB-MENU  m_Informes     LABEL "Informes"      .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bmov_ahorros AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bmov_creditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bTaquilla AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dahorros AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dclientes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcreditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dhoja_vida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dmov_ahorros AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dmov_creditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drelaciones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtaquilla AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttagencias AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttinstancias AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttmov_inssipla AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttusuarios AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Age AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Ahorros AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Creditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Hoja_Vida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Inst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Mov_ins AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-rela AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-Usu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientes_segmentacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vclientes_ubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vhoja_vida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vttmov_header AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vttmov_inssipla AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wclientes_cons AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 7 BY 1.65
     FONT 8.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65
     FONT 8.

DEFINE BUTTON BUTTON-exitF 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-Update 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Actualizar" 
     SIZE 8 BY 1.62 TOOLTIP "Actualizar".

DEFINE VARIABLE Fec_Fin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Estado AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Gesionadas", 1,
"No Gestionadas", 2,
"Todas", 3
     SIZE 14 BY 2.69 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-153 AT ROW 2.62 COL 119 WIDGET-ID 28
     Btn_Imp AT ROW 4.23 COL 119 WIDGET-ID 2
     BtnDone AT ROW 5.88 COL 119 WIDGET-ID 30
     "INSTANCIAS" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.42 COL 23 WIDGET-ID 22
     "USUARIOS" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 6.69 COL 85 WIDGET-ID 26
     "AGENCIAS" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 6.69 COL 25 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.72 BY 24.35
         BGCOLOR 17  WIDGET-ID 100.

DEFINE FRAME F_Filtros
     Fec_Ini AT ROW 1.54 COL 5 COLON-ALIGNED
     Fec_Fin AT ROW 2.62 COL 5 COLON-ALIGNED
     R_Estado AT ROW 1.54 COL 24 NO-LABEL
     BUTTON-Update AT ROW 1.27 COL 42 WIDGET-ID 2
     BUTTON-exitF AT ROW 2.88 COL 42
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 68 ROW 2.62
         SIZE 50 BY 4.58
         BGCOLOR 17 FONT 4
         TITLE "Filtros" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Movimientos SIPLA"
         HEIGHT             = 24.35
         WIDTH              = 125.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-wWin:HANDLE.
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
/* REPARENT FRAME */
ASSIGN FRAME F_Filtros:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME F_Filtros
   Custom                                                               */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE
       FRAME F_Filtros:SELECTABLE       = TRUE
       FRAME F_Filtros:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Movimientos SIPLA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Movimientos SIPLA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain
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


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME fMain /* Button 149 */
DO:
    RUN wInfSipla.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME fMain /* Button 153 */
DO:
     RUN changePage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-exitF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-exitF wWin
ON CHOOSE OF BUTTON-exitF IN FRAME F_Filtros /* Button 163 */
DO:
  HIDE FRAME F_Filtros.
  RUN viewpage(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Update wWin
ON CHOOSE OF BUTTON-Update IN FRAME F_Filtros /* Actualizar */
DO:     
    APPLY "VALUE-CHANGED":U TO R_Estado IN FRAME F_Filtros.

    RUN creaTTMov_insSipla IN h_dttinstancias
      ( INPUT 0,
        INPUT 0,
        INPUT DATE(STRING(Fec_ini:SCREEN-VALUE IN FRAME F_Filtros)),
        INPUT DATE(STRING(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros)),
        INPUT vlestado).

    RUN creaTTMov_insSipla IN h_dttAgencias
      ( INPUT 0,
        INPUT 0,
        INPUT DATE(STRING(Fec_ini:SCREEN-VALUE IN FRAME F_Filtros)),
        INPUT DATE(STRING(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros)),
        INPUT vlestado).


  RUN creaTTMov_insSipla IN h_dttusuarios
    ( INPUT 0,
      INPUT 0,
      INPUT DATE(STRING(Fec_ini:SCREEN-VALUE IN FRAME F_Filtros)),
      INPUT DATE(STRING(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros)),
        INPUT vlestado).

  RUN creaTTMov_InsSipla IN h_dttmov_inssipla
    ( INPUT 0,
      INPUT 0,
      INPUT DATE(STRING(Fec_ini:SCREEN-VALUE IN FRAME F_Filtros)),
      INPUT DATE(STRING(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros)),
        INPUT vlestado).

    ASSIGN vcString = "Fec_Transaccion >= " + STRING(Fec_ini:SCREEN-VALUE IN FRAME F_Filtros) + " AND " +
      "Fec_Transaccion <= " + STRING(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros).

    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dtaquilla, INPUT vcString).

    
    ASSIGN vcString = "Fecha >= " + STRING(Fec_ini:SCREEN-VALUE) + " AND " + 
      "Fecha <= " + STRING(Fec_fin:SCREEN-VALUE).
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dmov_ahorros, INPUT vcString).
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dmov_creditos, INPUT vcString).


    DYNAMIC-FUNCTION('openQuery':U IN h_dttinstancias).
    DYNAMIC-FUNCTION('openQuery':U IN h_dttAgencias).
    DYNAMIC-FUNCTION('openQuery':U IN h_dttusuarios).
    DYNAMIC-FUNCTION('openQuery':U IN h_dttmov_inssipla).
    DYNAMIC-FUNCTION('openQuery':U IN h_dtaquilla).

    DYNAMIC-FUNCTION('openQuery':U IN h_dmov_ahorros).
    DYNAMIC-FUNCTION('openQuery':U IN h_dmov_creditos).

  APPLY "CHOOSE":U TO BUTTON-exitF IN FRAME F_Filtros.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fec_Fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_Fin wWin
ON LEAVE OF Fec_Fin IN FRAME F_Filtros /* Hasta */
DO:
   ASSIGN Fec_Fin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Informacin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Informacin wWin
ON CHOOSE OF MENU-ITEM m_Informacin /* Información */
DO:
    RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Informes2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Informes2 wWin
ON CHOOSE OF MENU-ITEM m_Informes2 /* Informes */
DO:
    RUN wInfSipla.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Salir wWin
ON CHOOSE OF MENU-ITEM m_Salir /* Salir */
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


&Scoped-define SELF-NAME R_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Estado wWin
ON VALUE-CHANGED OF R_Estado IN FRAME F_Filtros
DO:
    CASE SELF:SCREEN-VALUE:
        WHEN "1" THEN
            ASSIGN vlestado = TRUE.
        WHEN "2" THEN
            ASSIGN vlestado = FALSE.
        OTHERWISE
            ASSIGN vlestado = ?.
    END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
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
             INPUT  'dttinstancias.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedttinstanciasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dttinstancias ).
       RUN repositionObject IN h_dttinstancias ( 2.35 , 9.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dttagencias.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsInstancia,InstanciaObjectNamedttagenciasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dttagencias ).
       RUN repositionObject IN h_dttagencias ( 2.35 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dttusuarios.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsInstancia,Instancia,Agencia,AgenciaObjectNamedttusuariosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dttusuarios ).
       RUN repositionObject IN h_dttusuarios ( 3.69 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dttmov_inssipla.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsInstancia,Instancia,Agencia,Agencia,UsuGestiona,UsuGestionaObjectNamedttmov_inssiplaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dttmov_inssipla ).
       RUN repositionObject IN h_dttmov_inssipla ( 5.04 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 8.00 ) */

       RUN constructObject (
             INPUT  'vttmov_header.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vttmov_header ).
       RUN repositionObject IN h_vttmov_header ( 1.00 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.58 , 124.00 ) */

       RUN constructObject (
             INPUT  'dhoja_vida.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsHoja_Vida.Nit,NitObjectNamedhoja_vidaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dhoja_vida ).
       RUN repositionObject IN h_dhoja_vida ( 2.62 , 44.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dclientes.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsClientes.Nit,NitObjectNamedclientesOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dclientes ).
       RUN repositionObject IN h_dclientes ( 6.38 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 7.00 ) */

       RUN constructObject (
             INPUT  'dtaquilla.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsTaquilla.Nit,NitObjectNamedtaquillaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dtaquilla ).
       RUN repositionObject IN h_dtaquilla ( 2.62 , 57.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dmov_creditos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsMov_Creditos.agencia,Agencia,Mov_Creditos.Nit,NitObjectNamedmov_creditosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dmov_creditos ).
       RUN repositionObject IN h_dmov_creditos ( 2.62 , 36.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dmov_ahorros.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsMov_Ahorros.Agencia,Agencia,Mov_Ahorros.Nit,NitObjectNamedmov_ahorrosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dmov_ahorros ).
       RUN repositionObject IN h_dmov_ahorros ( 2.62 , 27.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vclientes_segmentacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientes_segmentacion ).
       RUN repositionObject IN h_vclientes_segmentacion ( 3.42 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 8.62 , 118.00 ) */

       RUN constructObject (
             INPUT  'drelaciones.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsRelaciones.Nit,NitObjectNamedrelacionesOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_drelaciones ).
       RUN repositionObject IN h_drelaciones ( 7.73 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dahorros.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsAhorros.Nit,NitObjectNamedahorrosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dahorros ).
       RUN repositionObject IN h_dahorros ( 9.08 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'dcreditos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsCreditos.Nit,NitObjectNamedcreditosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcreditos ).
       RUN repositionObject IN h_dcreditos ( 10.69 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsIUsuGest,ICantEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdttusuariosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Usu ).
       RUN repositionObject IN h_dynbrowser-Usu ( 7.19 , 61.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Usu ( 4.85 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsIAgencia,ICantEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdttagenciasUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Age ).
       RUN repositionObject IN h_dynbrowser-Age ( 7.23 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Age ( 4.85 , 54.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsIInstancia,ICantEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdttinstanciasUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Inst ).
       RUN repositionObject IN h_dynbrowser-Inst ( 3.96 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Inst ( 2.46 , 54.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Mov. Instancias|Det. Instancias|Transacciones|Datos Cliente|Relaciones|Ahorros|Créditos|Hoja de Vida' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 12.58 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 12.38 , 122.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2':U) NO-ERROR.

       /* Links to SmartDataObject h_dttagencias. */
       RUN addLink ( h_dttinstancias , 'Data':U , h_dttagencias ).

       /* Links to SmartDataObject h_dttusuarios. */
       RUN addLink ( h_dttagencias , 'Data':U , h_dttusuarios ).

       /* Links to SmartDataObject h_dttmov_inssipla. */
       RUN addLink ( h_dttusuarios , 'Data':U , h_dttmov_inssipla ).

       /* Links to SmartDataViewer h_vttmov_header. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_vttmov_header ).
       RUN addLink ( h_vttmov_inssipla , 'GroupAssign':U , h_vttmov_header ).

       /* Links to SmartDataObject h_dhoja_vida. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_dhoja_vida ).

       /* Links to SmartDataObject h_dclientes. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_dclientes ).

       /* Links to SmartDataObject h_dtaquilla. */
       RUN addLink ( h_dclientes , 'Data':U , h_dtaquilla ).

       /* Links to SmartDataObject h_dmov_creditos. */
       RUN addLink ( h_dclientes , 'Data':U , h_dmov_creditos ).

       /* Links to SmartDataObject h_dmov_ahorros. */
       RUN addLink ( h_dclientes , 'Data':U , h_dmov_ahorros ).

       /* Links to SmartDataViewer h_vclientes_segmentacion. */
       RUN addLink ( h_dclientes , 'Data':U , h_vclientes_segmentacion ).

       /* Links to SmartDataObject h_drelaciones. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_drelaciones ).

       /* Links to SmartDataObject h_dahorros. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_dahorros ).

       /* Links to SmartDataObject h_dcreditos. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_dcreditos ).

       /* Links to SmartDataBrowser h_dynbrowser-Usu. */
       RUN addLink ( h_dttusuarios , 'Data':U , h_dynbrowser-Usu ).

       /* Links to SmartDataBrowser h_dynbrowser-Age. */
       RUN addLink ( h_dttagencias , 'Data':U , h_dynbrowser-Age ).

       /* Links to SmartDataBrowser h_dynbrowser-Inst. */
       RUN addLink ( h_dttinstancias , 'Data':U , h_dynbrowser-Inst ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vttmov_header ,
             FRAME F_Filtros:HANDLE , 'BEFORE':U ).
       RUN adjustTabOrder ( h_vclientes_segmentacion ,
             BUTTON-153:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-Inst ,
             h_vclientes_segmentacion , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-Usu ,
             BtnDone:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynbrowser-Age ,
             h_dynbrowser-Usu , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_dynbrowser-Age , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFCliente,FVNUM,FVNUD,Estado,Fecha_Transaccion,FHorTrans,Fecha_Gestion,FHorGestion,FUsu_reporta,FUsu_GestionaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdttmov_inssiplaUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Mov_ins ).
       RUN repositionObject IN h_dynbrowser-Mov_ins ( 13.65 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Mov_ins ( 10.50 , 118.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-Mov_ins. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_dynbrowser-Mov_ins ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-Mov_ins ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vttmov_inssipla.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vttmov_inssipla ).
       RUN repositionObject IN h_vttmov_inssipla ( 14.19 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 10.23 , 117.00 ) */

       /* Links to SmartDataViewer h_vttmov_inssipla. */
       RUN addLink ( h_dttmov_inssipla , 'Data':U , h_vttmov_inssipla ).
       RUN addLink ( h_vttmov_inssipla , 'Update':U , h_dttmov_inssipla ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vttmov_inssipla ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'bTaquilla.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bTaquilla ).
       RUN repositionObject IN h_bTaquilla ( 14.19 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_bTaquilla ( 4.58 , 118.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bmov_creditos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bmov_creditos ).
       RUN repositionObject IN h_bmov_creditos ( 19.31 , 67.00 ) NO-ERROR.
       RUN resizeObject IN h_bmov_creditos ( 4.58 , 56.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bmov_ahorros.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bmov_ahorros ).
       RUN repositionObject IN h_bmov_ahorros ( 19.31 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_bmov_ahorros ( 4.58 , 61.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bTaquilla. */
       RUN addLink ( h_dtaquilla , 'Data':U , h_bTaquilla ).

       /* Links to SmartDataBrowser h_bmov_creditos. */
       RUN addLink ( h_dmov_creditos , 'Data':U , h_bmov_creditos ).

       /* Links to SmartDataBrowser h_bmov_ahorros. */
       RUN addLink ( h_dmov_ahorros , 'Data':U , h_bmov_ahorros ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bTaquilla ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_bmov_ahorros ,
             h_bTaquilla , 'AFTER':U ).
       RUN adjustTabOrder ( h_bmov_creditos ,
             h_bmov_ahorros , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'wclientes_cons.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wclientes_cons ).
       /* Position in AB:  ( 1.00 , 1.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'vclientes_ubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vclientes_ubicacion ).
       RUN repositionObject IN h_vclientes_ubicacion ( 13.92 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 9.42 , 118.00 ) */

       /* Links to SmartWindow h_wclientes_cons. */
       RUN addLink ( h_dclientes , 'Data':U , h_wclientes_cons ).
       RUN addLink ( h_folder , 'Page':U , h_wclientes_cons ).

       /* Links to SmartDataViewer h_vclientes_ubicacion. */
       RUN addLink ( h_dclientes , 'Data':U , h_vclientes_ubicacion ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vclientes_ubicacion ,
             h_folder , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFPersonaRelacion,FRelacion,FDirecRelacion,FTelRelacion,Clase_Producto,Cod_Producto,Aprobada,Fec_Ingreso,FEstado,Fec_InactividadEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdrelacionesUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-rela ).
       RUN repositionObject IN h_dynbrowser-rela ( 14.46 , 8.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-rela ( 9.96 , 112.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-rela. */
       RUN addLink ( h_drelaciones , 'Data':U , h_dynbrowser-rela ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-rela ,
             h_folder , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFAporte,Tasa,Plazo,Cuota,Fec_Apertura,Sdo_Disponible,Sdo_Canje,FAgenciaEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdahorrosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Ahorros ).
       RUN repositionObject IN h_dynbrowser-Ahorros ( 14.46 , 6.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Ahorros ( 9.96 , 116.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-Ahorros. */
       RUN addLink ( h_dahorros , 'Data':U , h_dynbrowser-Ahorros ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-Ahorros ,
             h_folder , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFCredito,Tasa,Plazo,Cuota,Sdo_Capital,Val_Atraso,Fec_DesembolsoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdcreditosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Creditos ).
       RUN repositionObject IN h_dynbrowser-Creditos ( 14.46 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Creditos ( 9.96 , 114.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-Creditos. */
       RUN addLink ( h_dcreditos , 'Data':U , h_dynbrowser-Creditos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-Creditos ,
             h_folder , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFCliente,FHVEncabezado,Asunto_Cumplido,FInstancia,Fec_Grabacion,Hora_Grabacion,Fec_Limite,DoctoReferEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdhoja_vidaUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-Hoja_Vida ).
       RUN repositionObject IN h_dynbrowser-Hoja_Vida ( 14.46 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-Hoja_Vida ( 3.77 , 114.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vhoja_vida.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vhoja_vida ).
       RUN repositionObject IN h_vhoja_vida ( 18.50 , 7.00 ) NO-ERROR.
       /* Size in AB:  ( 5.65 , 114.00 ) */

       /* Links to SmartDataBrowser h_dynbrowser-Hoja_Vida. */
       RUN addLink ( h_dhoja_vida , 'Data':U , h_dynbrowser-Hoja_Vida ).

       /* Links to SmartDataViewer h_vhoja_vida. */
       RUN addLink ( h_dhoja_vida , 'Data':U , h_vhoja_vida ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-Hoja_Vida ,
             h_folder , 'AFTER':U ).
       RUN adjustTabOrder ( h_vhoja_vida ,
             h_dynbrowser-Hoja_Vida , 'AFTER':U ).
    END. /* Page 8 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DEFINE VARIABLE viPagina AS INTEGER INITIAL 0 NO-UNDO.

  ASSIGN viPagina =   getCurrentPage().
  CASE viPagina:
      WHEN 0 THEN DO:
          RUN hideObject IN h_vclientes_segmentacion.
          VIEW FRAME F_Filtros.
      END.
      WHEN 1 THEN DO:
          RUN hideObject IN h_vclientes_segmentacion.
          VIEW FRAME F_Filtros.
      END.
      OTHERWISE DO:
           RUN viewObject IN h_vclientes_segmentacion.
           HIDE FRAME F_Filtros.
      END.
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
  ENABLE BUTTON-153 Btn_Imp BtnDone 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY Fec_Ini Fec_Fin R_Estado 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE Fec_Ini Fec_Fin R_Estado BUTTON-Update BUTTON-exitF 
      WITH FRAME F_Filtros IN WINDOW wWin.
  VIEW FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
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

  RUN hideObject IN h_vclientes_segmentacion.
  HIDE FRAME F_Filtros.
  
  RUN PutValorInicial.

/*   RUN viewpage(1). */
  VIEW FRAME F_Filtros.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PutValorInicial wWin 
PROCEDURE PutValorInicial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    ASSIGN
        Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros = STRING(TODAY - DAY(TODAY - 1)) 
        Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros = STRING(TODAY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

