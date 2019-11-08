&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido\variable.i "shared"}

DEFINE VARIABLE w_ok AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE tmp-conteo
    FIELD tage LIKE tarjetas.agencia
    FIELD tnomage AS CHARACTER FORMAT "X(20)"
    FIELD tact AS DECIMAL INITIAL 0
    FIELD tblo AS DECIMAL INITIAL 0
    FIELD tdis AS DECIMAL INITIAL 0
    FIELD trem AS DECIMAL INITIAL 0
    FIELD trec AS DECIMAL INITIAL 0
    FIELD tnew AS DECIMAL INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm-Main
&Scoped-define BROWSE-NAME Brw-Consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tmp-Conteo

/* Definitions for BROWSE Brw-Consulta                                  */
&Scoped-define FIELDS-IN-QUERY-Brw-Consulta Tmp-Conteo.Tage Tmp-Conteo.TNomage Tmp-Conteo.Tnew Tmp-Conteo.Tact Tmp-Conteo.Trem Tmp-Conteo.Trec Tmp-Conteo.Tdis Tmp-Conteo.Tblo Tmp-Conteo.Tact + Tmp-conteo.Tblo + Tmp-conteo.Tdis + Tmp-Conteo.Tnew + Tmp-Conteo.Trem + Tmp-Conteo.Trec   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Consulta   
&Scoped-define SELF-NAME Brw-Consulta
&Scoped-define QUERY-STRING-Brw-Consulta FOR EACH Tmp-Conteo
&Scoped-define OPEN-QUERY-Brw-Consulta OPEN QUERY {&SELF-NAME} FOR EACH Tmp-Conteo.
&Scoped-define TABLES-IN-QUERY-Brw-Consulta Tmp-Conteo
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Consulta Tmp-Conteo


/* Definitions for FRAME Frm-Main                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm-Main ~
    ~{&OPEN-QUERY-Brw-Consulta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-339 Btn-Procesar Btn-Visualizar ~
Btn_Done W-FecIni W-FecFin Brw-Consulta 
&Scoped-Define DISPLAYED-OBJECTS W-FecIni W-FecFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn-Visualizar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Procesar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/exit01.ico":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE W-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-339
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 2.65
     FGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Consulta FOR 
      Tmp-Conteo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Consulta C-Win _FREEFORM
  QUERY Brw-Consulta DISPLAY
      Tmp-Conteo.Tage            COLUMN-LABEL "CodAge"      FORMAT "zz9"   
     Tmp-Conteo.TNomage         COLUMN-LABEL "Agencia"     FORMAT "X(20)"
     Tmp-Conteo.Tnew COLUMN-LABEL "Nuevas" FORMAT ">>>,>>9"
     Tmp-Conteo.Tact            COLUMN-LABEL "Activas"     FORMAT "zz,zzz,zz9"
     Tmp-Conteo.Trem COLUMN-LABEL "Remisión" FORMAT ">>>,>>9"
     Tmp-Conteo.Trec COLUMN-LABEL "Recepción" FORMAT ">>>,>>9"
     Tmp-Conteo.Tdis            COLUMN-LABEL "Disponibles" FORMAT "zz,zzz,zz9"
     Tmp-Conteo.Tblo            COLUMN-LABEL "Bloqueadas"  FORMAT "zz,zzz,zz9"     
     Tmp-Conteo.Tact +
     Tmp-conteo.Tblo +
     Tmp-conteo.Tdis +
     Tmp-Conteo.Tnew +
     Tmp-Conteo.Trem +
     Tmp-Conteo.Trec COLUMN-LABEL "Total" FORMAT "zz,zzz,zz9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 7.81
         FGCOLOR 0  ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     Btn-Procesar AT ROW 2.08 COL 28 WIDGET-ID 86
     Btn-Visualizar AT ROW 2.08 COL 36 WIDGET-ID 166
     Btn_Done AT ROW 2.08 COL 44 WIDGET-ID 20
     W-FecIni AT ROW 2.19 COL 9.72 COLON-ALIGNED WIDGET-ID 64
     W-FecFin AT ROW 3.35 COL 9.57 COLON-ALIGNED WIDGET-ID 66
     Brw-Consulta AT ROW 4.77 COL 2 WIDGET-ID 200
     "[dd/mm/aaaa]" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 1.35 COL 12.86 WIDGET-ID 70
          FGCOLOR 12 
     " Fechas:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1.35 COL 3.86 WIDGET-ID 68
          FGCOLOR 12 
     RECT-339 AT ROW 1.85 COL 2.57 WIDGET-ID 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.86 BY 11.88 WIDGET-ID 100.


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
         TITLE              = "Conteo de Tarjetas - W-Imp_Conteo.w"
         HEIGHT             = 11.85
         WIDTH              = 101.86
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB Brw-Consulta W-FecFin Frm-Main */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME Frm-Main
   6                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Consulta
/* Query rebuild information for BROWSE Brw-Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tmp-Conteo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Brw-Consulta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Conteo de Tarjetas - W-Imp_Conteo.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Conteo de Tarjetas - W-Imp_Conteo.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME Frm-Main /* Procesar */
DO:
  RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME Frm-Main /* Button 2 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
     DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
     listado = W_PathSpl + "L_Usuar.Lst".
     {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME Frm-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W-FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-FecFin C-Win
ON LEAVE OF W-FecFin IN FRAME Frm-Main /* Hasta */
DO:
  ASSIGN w-FecFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W-FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-FecIni C-Win
ON LEAVE OF W-FecIni IN FRAME Frm-Main /* Desde */
DO:
  ASSIGN W-FecIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-Consulta
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
  ASSIGN W-FecIni:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha - DAY(w_fecha) + 1)
         W-FecFin:SCREEN-VALUE IN FRAME Frm-main = STRING(W_Fecha)
         W-FecIni W-FecFin.

  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY W-FecIni W-FecFin 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  ENABLE RECT-339 Btn-Procesar Btn-Visualizar Btn_Done W-FecIni W-FecFin 
         Brw-Consulta 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializar_variables C-Win 
PROCEDURE inicializar_variables :
DEFINE VAR w_dis AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR w_act AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR w_blo AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR w_rem AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR w_rec AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR w_new AS DECIMAL INITIAL 0 NO-UNDO.

FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.

DISABLE Btn-Visualizar.

EMPTY TEMP-TABLE tmp-Conteo.

FOR EACH tarjetas WHERE (tarjetas.fec_activacion >= W-Fecini AND tarjetas.fec_activacion <= W-Fecini)
                     OR (tarjetas.fec_cargue >= W-Fecini AND tarjetas.fec_cargue <= W-Fecini)
                     OR (tarjetas.fec_bloqueo >= W-Fecini AND tarjetas.fec_bloqueo <= W-Fecini) NO-LOCK BREAK BY tarjetas.agencia:
    IF FIRST-OF(Tarjetas.agencia) THEN DO:
        CREATE tmp-conteo.
        ASSIGN tage = Tarjetas.agencia.
        FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias) THEN
            ASSIGN Tmp-Conteo.TNomAge = Agencias.nombre.
    END.

    CASE tarjetas.estado:
        WHEN "00" THEN  /* Nueva */
           ASSIGN tmp-conteo.tnew = tmp-conteo.tnew + 1
                  w_new = w_new + 1.
        WHEN "01" THEN  /* Activas */
            ASSIGN tmp-conteo.tact = tmp-conteo.tact + 1
                   w_act = w_act + 1.
        WHEN "02" THEN  /* Remisión */
            ASSIGN tmp-conteo.trem = tmp-conteo.trem + 1
                   w_rem = w_rem + 1.
        WHEN "03" THEN  /* Recepción */
            ASSIGN tmp-conteo.trec = tmp-conteo.trec + 1
                   w_rec = w_rec + 1.
        WHEN "04" THEN  /* Disponibles */
            ASSIGN tmp-conteo.tdis = tmp-conteo.tdis + 1
                   w_dis = w_dis + 1.
        OTHERWISE   /* Bloqueadas */
           ASSIGN tmp-conteo.tblo = tmp-conteo.tblo + 1
                  w_blo = w_blo + 1.
    END CASE.
END.

CREATE Tmp-Conteo.
ASSIGN Tmp-Conteo.Tnomage = "Total x Agencia: "
       Tmp-Conteo.Tdis    = W_Dis
       Tmp-Conteo.Tact    = W_act
       Tmp-Conteo.TBlo    = W_Blo
       Tmp-Conteo.Tnew = w_new
       Tmp-Conteo.Trem = w_rem
       Tmp-Conteo.Trec = w_rec.

OPEN QUERY Brw-Consulta FOR EACH Tmp-Conteo NO-LOCK.
APPLY "CHOOSE" TO btn-Visualizar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}.
DEFINE VAR tot AS DECIMAL.
DEFINE FRAME F-mov1
Tmp-Conteo.Tage         AT 1                    
Tmp-Conteo.Tnomage      AT 5  FORMAT "X(15)"    
Tmp-Conteo.Tnew      AT 22 FORMAT "zzz,zz9"
Tmp-Conteo.Tact      AT 36 FORMAT "zzz,zz9"
Tmp-Conteo.Trem      AT 50 FORMAT "zzz,zz9"
Tmp-Conteo.Trec      AT 64 FORMAT "zzz,zz9"
Tmp-Conteo.Tdis      AT 78 FORMAT "zzz,zz9"
Tmp-Conteo.Tblo      AT 92 FORMAT "zzz,zz9"
    tot             AT 106 FORMAT "zzz,zz9"

    WITH 10 DOWN SIZE 170 BY 10 FRAME F-mov1 USE-TEXT no-box NO-LABEL STREAM-IO.




/*  DEBUGGER:INITIATE().   */
/*  DEBUGGER:SET-BREAK().  */
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 W_Reporte    = "REPORTE   : Conteo de Tarjetas " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Cod Agencia          Nuevas        Activas       Remisión     Recepción    Disponibles    Bloqueadas     Total".
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.
 FOR EACH Tmp-conteo NO-LOCK :
     tot = Tmp-Conteo.Tact +
           Tmp-conteo.Tblo +
           Tmp-conteo.Tdis +
           Tmp-Conteo.Tnew +
           Tmp-Conteo.Trem +
           Tmp-Conteo.Trec.

   DISPLAY Tmp-Conteo.Tage     
           Tmp-Conteo.Tnomage  
           Tmp-Conteo.Tnew 
           Tmp-Conteo.Tact 
           Tmp-Conteo.Trem 
           Tmp-Conteo.Trec 
           Tmp-Conteo.Tdis 
           Tmp-Conteo.Tblo 
       tot
           
       WITH FRAME F-mov1 /*USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 300 NO-LABELS*/.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

