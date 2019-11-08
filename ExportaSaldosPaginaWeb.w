&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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


DEFINE VARIABLE k AS INTEGER INITIAL 0    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Ilogo Fini Ffin BtnDone BtnExportar 
&Scoped-Define DISPLAYED-OBJECTS Fini Ffin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "Cancelar" 
     SIZE 21 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnExportar 
     LABEL "Exportar Saldos" 
     SIZE 22 BY 1.12.

DEFINE VARIABLE Ffin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Fini AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE IMAGE Ilogo
     FILENAME "adeicon/blank":U
     SIZE 28 BY 2.42.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY .54
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Main
     Fini AT ROW 6.12 COL 15 COLON-ALIGNED WIDGET-ID 2
     Ffin AT ROW 6.12 COL 45 COLON-ALIGNED WIDGET-ID 4
     BtnDone AT ROW 8.27 COL 17 WIDGET-ID 16
     BtnExportar AT ROW 8.27 COL 39 WIDGET-ID 10
     "Exportar movimientos de ahorro comprendidos en las fechas:" VIEW-AS TEXT
          SIZE 62 BY .62 AT ROW 4.77 COL 4 WIDGET-ID 6
     Ilogo AT ROW 1.54 COL 4 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 9.08
         BGCOLOR 15 
         DEFAULT-BUTTON BtnExportar CANCEL-BUTTON BtnDone WIDGET-ID 100.

DEFINE FRAME F_Progreso
     R1 AT ROW 6.92 COL 3.57
     R2 AT ROW 6.38 COL 3.57
     R3 AT ROW 5.85 COL 3.57
     R4 AT ROW 5.31 COL 3.57
     R5 AT ROW 4.77 COL 3.57
     R6 AT ROW 4.23 COL 3.57
     R7 AT ROW 3.69 COL 3.57
     R8 AT ROW 3.15 COL 3.57
     R9 AT ROW 2.62 COL 3.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 61.57 ROW 2.73
         SIZE 11.43 BY 6.73
         BGCOLOR 15  WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Exportar Saldos"
         HEIGHT             = 9.08
         WIDTH              = 72
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 93
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 93
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Progreso:FRAME = FRAME F_Main:HANDLE.

/* SETTINGS FOR FRAME F_Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Exportar Saldos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Exportar Saldos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME F_Main /* Cancelar */
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


&Scoped-define SELF-NAME BtnExportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExportar C-Win
ON CHOOSE OF BtnExportar IN FRAME F_Main /* Exportar Saldos */
DO:

    DEFINE VARIABLE Resp AS LOGICAL     NO-UNDO.

    IF date(Fini:SCREEN-VALUE IN FRAME f_main) > date(Ffin:SCREEN-VALUE IN FRAME f_main)THEN
        DO:
            MESSAGE "La fecha inicial no puede ser mayor a la fecha final"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    ELSE
        DO:
            IF date(Fini:SCREEN-VALUE) = ? OR date(Ffin:SCREEN-VALUE) = ? THEN
                DO:
                    MESSAGE "Fecha inválida"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
             ELSE
                 DO:
                     MESSAGE "Está a punto de exportar saldos desde " fini:SCREEN-VALUE " hasta " ffin:SCREEN-VALUE
                              SKIP
                             "Está seguro de continuar?"
                              VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE Resp.

                     IF resp THEN DO:

                         VIEW FRAME F_Progreso.

                         DISABLE BtnExportar BtnDone.
                         
                         RUN ExportaAhorrosSubMejorado2.r (INPUT date(fini:SCREEN-VALUE), INPUT date(FFin:SCREEN-VALUE)).
                         RUN progreso.
                         RUN ExportaAhorros.r.
                         RUN progreso.
                         RUN ExportaAportes.r.
                         RUN progreso.
                         RUN ExportaCreditos.r.
                           
                         HIDE FRAME F_Progreso.

                         MESSAGE "Proceso concluido con éxito, archivos generados en C:\Info_Cooprudea"
                             VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        
                         ENABLE BtnExportar BtnDone.

                     END.   /* Cierra Condición Respuesta */

                 END.       /* Cierra Si no */
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

Ilogo:LOAD-IMAGE("Imagenes/LogoCooprudea.jpg").
ASSIGN ffin:SCREEN-VALUE IN FRAME f_main = STRING(TODAY).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY Fini Ffin 
      WITH FRAME F_Main IN WINDOW C-Win.
  ENABLE Ilogo Fini Ffin BtnDone BtnExportar 
      WITH FRAME F_Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Main}
  ENABLE R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso C-Win 
PROCEDURE Progreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DO WITH FRAME F_Progreso:
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN R1:BGCOL = 18.
          WHEN 2 THEN R2:BGCOL = 18.
          WHEN 3 THEN R3:BGCOL = 18.
          WHEN 4 THEN R4:BGCOL = 18.
          WHEN 5 THEN R5:BGCOL = 18.
          WHEN 6 THEN R6:BGCOL = 18.
          WHEN 7 THEN R7:BGCOL = 18.
          WHEN 8 THEN R8:BGCOL = 18.
          WHEN 9 THEN R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

