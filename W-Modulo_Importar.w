&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WImportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WImportar 
     
/* oakley */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  {Incluido/Variable.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE NEW SHARED VAR j AS DECIMAL.
DEFINE NEW SHARED VAR E AS DECIMAL.
/*DEFINE NEW SHARED VARIABLE WRegistro AS DECIMAL FORMAT ">>>,>>>,>>9".*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Importar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS STablas Btn_Importar EErrores TBorra ~
W_PlanPag THabTri Btn_Salir 
&Scoped-Define DISPLAYED-OBJECTS STablas EErrores TBorra W_PlanPag THabTri ~
RI 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WImportar AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Errores 
     LABEL "Ver Errores >>" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_Importar 
     LABEL "Importar" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 15 BY 1.65.

DEFINE VARIABLE EErrores AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 51 BY 11.31
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE RI AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Reg.Importados" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE STablas AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 32 BY 7.54
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TBorra AS LOGICAL INITIAL no 
     LABEL "Borrar datos antes de importar" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .77 NO-UNDO.

DEFINE VARIABLE THabTri AS LOGICAL INITIAL no 
     LABEL "Habilitar Triggers" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.72 BY .81 NO-UNDO.

DEFINE VARIABLE W_PlanPag AS LOGICAL INITIAL no 
     LABEL "Generar Plan Pagos desde Créditos" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.72 BY .77 TOOLTIP "Marque solo para Generar Control_pagos a Todos los Créditos Importados" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Importar
     STablas AT ROW 2.35 COL 4 NO-LABEL
     Btn_Importar AT ROW 2.35 COL 38
     EErrores AT ROW 2.35 COL 57 NO-LABEL
     Btn_Errores AT ROW 4.23 COL 38
     TBorra AT ROW 10.15 COL 5
     W_PlanPag AT ROW 11.19 COL 5
     THabTri AT ROW 12.15 COL 5.29
     Btn_Salir AT ROW 13.38 COL 37
     RI AT ROW 13.65 COL 16 COLON-ALIGNED
     "Escoja la tabla a Importar" VIEW-AS TEXT
          SIZE 23 BY .88 AT ROW 1.27 COL 4.57
          FGCOLOR 7 
     "Listado de errores ocurridos durante la importación" VIEW-AS TEXT
          SIZE 48 BY .88 AT ROW 1.27 COL 58
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.14 BY 14.27
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WImportar ASSIGN
         HIDDEN             = YES
         TITLE              = "Módulo de Importación"
         HEIGHT             = 14.27
         WIDTH              = 53.29
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB WImportar 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WImportar
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Importar
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Errores IN FRAME F_Importar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RI IN FRAME F_Importar
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WImportar)
THEN WImportar:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WImportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WImportar WImportar
ON END-ERROR OF WImportar /* Módulo de Importación */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WImportar WImportar
ON WINDOW-CLOSE OF WImportar /* Módulo de Importación */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Errores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Errores WImportar
ON CHOOSE OF Btn_Errores IN FRAME F_Importar /* Ver Errores >> */
DO:
  IF SELF:LABEL EQ "Ver Errores >>" THEN DO:
    WImportar:WIDTH = 109.14.
    SELF:LABEL = "<< Ocultar".
  END.
  ELSE DO:
    WImportar:WIDTH = 54.
    SELF:LABEL = "Ver Errores >>".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Importar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Importar WImportar
ON CHOOSE OF Btn_Importar IN FRAME F_Importar /* Importar */
DO:
  ASSIGN FRAME F_Importar TBorra THabTri.
  ASSIGN rI:SCREEN-VALUE = "0".
  DISABLE Btn_Errores WITH FRAME F_Importar.
  IF STablas:SCREEN-VALUE EQ ? THEN DO:
     MESSAGE "No se ha escogido ninguna tabla para" SKIP
             "La importación de datos." SKIP(1)
             "Escoja una tabla de la lista de la izquierda"
             VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Btn_Importar.
     RETURN NO-APPLY.
  END.
  DEFINE VAR OkPressed AS LOGICAL.
  DEFINE VAR Archivo AS CHARACTER FORMAT "X(50)".
  SYSTEM-DIALOG GET-FILE Archivo
         TITLE      "Escoja la fuente de datos ..."
         INITIAL-DIR W_Pathspl
         FILTERS    "Archivos Texto (*.txt)"   "*.txt",
                    "Fuente de Datos (*.d)"   "*.d"
         MUST-EXIST
         USE-FILENAME
         UPDATE OKpressed.
  INPUT FROM VALUE(Archivo).
  IF OkPressed AND Archivo NE "" THEN DO:
     RUN _SetCurs.r ("WAIT").
     RUN Prc_importar.p STablas:SCREEN-VALUE TBorra:SCREEN-VALUE THabTri:SCREEN-VALUE.
     RUN _SetCurs.r ("ARROW").
  END.
  INPUT CLOSE.
  W_ok = Eerrores:READ-FILE(W_Pathspl + "\errores.e") IN FRAME F_Importar.   
  IF Eerrores:SCREEN-VALUE NE "" THEN DO:
     ENABLE Btn_Errores WITH FRAME F_Importar.
  END.
  ASSIGN Ri:SCREEN-VALUE = STRING(j).
  MESSAGE "Proceso Culminado" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir WImportar
ON CHOOSE OF Btn_Salir IN FRAME F_Importar /* Salir */
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


&Scoped-define SELF-NAME W_PlanPag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_PlanPag WImportar
ON MOUSE-SELECT-CLICK OF W_PlanPag IN FRAME F_Importar /* Generar Plan Pagos desde Créditos */
DO:
  ASSIGN W_PlanPag.
  IF W_PlanPag THEN DO:
     MESSAGE "Esta opción solo es para Créditos Importados..." SKIP
             "Está Segura(o) de generar Control_pagos desde Créditos..."
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO                
           TITLE "Confirmar Generar Control_pagos" UPDATE W_RptaPP AS LOGICAL.
     
     IF W_RptaPP THEN
        RUN Util_Creditos.R.

     ASSIGN W_PlanPag:SCREEN-VALUE = "No"
            W_PlanPag.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WImportar 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects WImportar  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WImportar  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WImportar)
  THEN DELETE WIDGET WImportar.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WImportar  _DEFAULT-ENABLE
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
  DISPLAY STablas EErrores TBorra W_PlanPag THabTri RI 
      WITH FRAME F_Importar IN WINDOW WImportar.
  ENABLE STablas Btn_Importar EErrores TBorra W_PlanPag THabTri Btn_Salir 
      WITH FRAME F_Importar IN WINDOW WImportar.
  {&OPEN-BROWSERS-IN-QUERY-F_Importar}
  VIEW WImportar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject WImportar 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject WImportar 
PROCEDURE initializeObject :
.

  RUN SUPER.
  FOR EACH _File:
    IF SUBSTRING(_File-Name,1,1) NE "_" AND
       SUBSTRING(_File-Name,1,3) NE "SYS" THEN
        W_Ok = STablas:ADD-LAST(_File-Name) IN FRAME F_Importar.
  END.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

