&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-2 BUTTON-1 BUTTON-3 FIni FFin RECT-1 ~
RECT-10 RECT-11 RECT-13 
&Scoped-Define DISPLAYED-OBJECTS FIni FFin T_Transacciones T_Consignaciones ~
T_Retiros 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 R4 R1 R2 R3 Id_Procesando 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Cambiar Valores Predeterminados" 
     SIZE 25 BY 1.08.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-3 
     LABEL "Ejecutar" 
     SIZE 12 BY 1.08.

DEFINE VARIABLE FFin AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FIni AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE T_Consignaciones AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Consignaciones" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE T_Retiros AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total de Retiros" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE T_Transacciones AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Número de Transacciones" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 85 BY 1.88.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 1.35.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY 1.35.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 1.35.

DEFINE VARIABLE Id_Procesando AS CHARACTER FORMAT "X(256)":U INITIAL "Procesando..." 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     FGCOLOR 7 FONT 4 NO-UNDO.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 7" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE R_Eleccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Agencias", 1,
"Segmentos", 2,
"Clientes", 3
     SIZE 38 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Consulta
     BUTTON-2 AT ROW 1.27 COL 103
     BUTTON-1 AT ROW 1.81 COL 48
     BUTTON-3 AT ROW 1.81 COL 74
     FIni AT ROW 1.92 COL 10.57 COLON-ALIGNED
     FFin AT ROW 1.92 COL 32.43 COLON-ALIGNED
     T_Transacciones AT ROW 4 COL 20 COLON-ALIGNED
     T_Consignaciones AT ROW 4 COL 57 COLON-ALIGNED
     T_Retiros AT ROW 4 COL 89 COLON-ALIGNED
     RECT-1 AT ROW 1.27 COL 2
     RECT-10 AT ROW 3.65 COL 2
     RECT-11 AT ROW 3.65 COL 41
     RECT-13 AT ROW 3.65 COL 77
     "Consolidado de Toda la Entidad" VIEW-AS TEXT
          SIZE 29 BY .54 AT ROW 3.35 COL 3.57
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.38
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Predeterminados
     R_Eleccion AT ROW 1.27 COL 24 NO-LABEL
     BUTTON-7 AT ROW 6.12 COL 57
     "Informe Filtrado por" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 1.27 COL 5
          FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 5.31
         SIZE 67 BY 7.81
         BGCOLOR 17 FONT 5
         TITLE "Valores Predeterminados".

DEFINE FRAME F_Conteo
     Id_Procesando AT ROW 1.27 COL 1 COLON-ALIGNED NO-LABEL
     R4 AT ROW 2.08 COL 12
     R1 AT ROW 2.08 COL 3
     R2 AT ROW 2.08 COL 6
     R3 AT ROW 2.08 COL 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 87 ROW 1.25
         SIZE 15 BY 1.88
         BGCOLOR 17 .


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
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Consulta General de Transacciones"
         HEIGHT             = 21.38
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
ASSIGN FRAME F_Conteo:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Predeterminados:FRAME = FRAME F_Consulta:HANDLE.

/* SETTINGS FOR FRAME F_Consulta
                                                                        */
/* SETTINGS FOR FILL-IN T_Consignaciones IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T_Retiros IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T_Transacciones IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Conteo
                                                                        */
/* SETTINGS FOR FILL-IN Id_Procesando IN FRAME F_Conteo
   NO-DISPLAY NO-ENABLE 1                                               */
/* SETTINGS FOR RECTANGLE R1 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R1:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR RECTANGLE R2 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R2:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR RECTANGLE R3 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R3:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR RECTANGLE R4 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R4:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR FRAME F_Predeterminados
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Predeterminados:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Consulta General de Transacciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Consulta General de Transacciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Consulta /* Cambiar Valores Predeterminados */
DO:
  VIEW FRAME F_Predeterminados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Predeterminados
&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON CHOOSE OF BUTTON-7 IN FRAME F_Predeterminados /* Button 7 */
DO:
  HIDE FRAME F_Predeterminados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
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
  DISPLAY FIni FFin T_Transacciones T_Consignaciones T_Retiros 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE BUTTON-2 BUTTON-1 BUTTON-3 FIni FFin RECT-1 RECT-10 RECT-11 RECT-13 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  VIEW FRAME F_Conteo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Conteo}
  DISPLAY R_Eleccion 
      WITH FRAME F_Predeterminados IN WINDOW wWin.
  ENABLE R_Eleccion BUTTON-7 
      WITH FRAME F_Predeterminados IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Predeterminados}
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
  ASSIGN FIni = TODAY
         FFin = TODAY.
  RUN SUPER.
  APPLY "entry" TO FIni IN FRAME F_Consulta.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

