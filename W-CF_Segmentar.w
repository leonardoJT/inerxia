&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE TEMP-TABLE Pscoring LIKE pro_scoring.
DEFINE VARIABLE W_Cr AS LOGICAL INIT FALSE.
DEFINE VAR Puntero AS ROWID.
DEFINE VAR PunteroActual AS ROWID.

DEFINE NEW SHARED VARIABLE Wvble like pro_scoring.VARIABLE.
DEFINE VARIABLE FileTmp AS CHARACTER FORMAT "X(30)".
DEFINE TEMP-TABLE Tmp
  FIELD TVariable    LIKE Pro_Scoring.VARIABLE
  FIELD TDescripcion AS CHARACTER FORMAT "X(50)"
  FIELD TTipoCampo   AS CHARACTER FORMAT "X(9)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Procesar
&Scoped-define BROWSE-NAME Br_ProScorin

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pro_scoring

/* Definitions for BROWSE Br_ProScorin                                  */
&Scoped-define FIELDS-IN-QUERY-Br_ProScorin Pro_scoring.Codigo Pro_scoring.Tabla Pro_scoring.Variable Pro_scoring.Rango_inicial Pro_scoring.Rango_final Pro_scoring.puntaje   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ProScorin   
&Scoped-define SELF-NAME Br_ProScorin
&Scoped-define QUERY-STRING-Br_ProScorin FOR EACH Pro_scoring NO-LOCK                                  BY Codigo BY VARIABLE
&Scoped-define OPEN-QUERY-Br_ProScorin OPEN QUERY Br_ProScorin FOR EACH Pro_scoring NO-LOCK                                  BY Codigo BY VARIABLE.
&Scoped-define TABLES-IN-QUERY-Br_ProScorin Pro_scoring
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ProScorin Pro_scoring


/* Definitions for FRAME F_Scoring                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Scoring ~
    ~{&OPEN-QUERY-Br_ProScorin}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R1 BUTTON-146 BUTTON-145 
&Scoped-Define DISPLAYED-OBJECTS R1 F_Nit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_Codigos Cmb_Tablas Cmb_Campos 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-145 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 145" 
     SIZE 8 BY 1.65.

DEFINE BUTTON BUTTON-146 
     LABEL "Procesar" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE F_Nit AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Procesar Todos los Nits", 1,
"Procesar un Solo Nit", 2
     SIZE 40 BY .81 NO-UNDO.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 12 BY 1.54.

DEFINE BUTTON Btn_Eliminar 
     LABEL "&Eliminar" 
     SIZE 12 BY 1.73.

DEFINE BUTTON Btn_Ingresar 
     LABEL "&Ingresar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 12 BY 1.54.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-137 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 137" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-139 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 139" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-140 
     IMAGE-UP FILE "imagenes/fizq.bmp":U
     LABEL "Button 140" 
     SIZE 5 BY 1.12.

DEFINE BUTTON BUTTON-141 
     IMAGE-UP FILE "imagenes/fder.bmp":U
     LABEL "Button 141" 
     SIZE 5 BY 1.12.

DEFINE BUTTON BUTTON-142 
     LABEL "Llena Scoring" 
     SIZE 14 BY 1.35.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 143" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-144 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 144" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE Cmb_Campos AS CHARACTER FORMAT "X(250)":U INITIAL "No Asignado" 
     LABEL "Variable" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "No Asignado" 
     DROP-DOWN-LIST
     SIZE 80 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Cmb_Codigos AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - No Asignado" 
     LABEL "Codigos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 56 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tablas AS CHARACTER FORMAT "X(50)":U INITIAL "No Asignado" 
     LABEL "Archivo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "No Asignado" 
     DROP-DOWN-LIST
     SIZE 56 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Tipos AS CHARACTER FORMAT "X(16)":U 
     LABEL "Tipos de Campo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Caracter","2 - Fecha","3 - Entero/Decimal" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE NomTipoCampo AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tipo de Campo" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14 BY 8.77.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14 BY 5.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_ProScorin FOR 
      Pro_scoring SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_ProScorin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ProScorin wWin _FREEFORM
  QUERY Br_ProScorin NO-LOCK DISPLAY
      Pro_scoring.Codigo
      Pro_scoring.Tabla
      Pro_scoring.Variable
      Pro_scoring.Rango_inicial COLUMN-LABEL "R.Inicial" WIDTH 13
      Pro_scoring.Rango_final COLUMN-LABEL "R.Final" WIDTH 13
      Pro_scoring.puntaje COLUMN-LABEL "Puntos" WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95.86 BY 7.19
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .54 EXPANDABLE TOOLTIP "Con click sube el registro a los campos para ser actualizados.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Scoring
     BUTTON-139 AT ROW 1.81 COL 100
     BUTTON-144 AT ROW 3.42 COL 100
     Cmb_Codigos AT ROW 3.69 COL 15 COLON-ALIGNED
     Cmb_Tablas AT ROW 4.77 COL 15 COLON-ALIGNED
     BUTTON-143 AT ROW 5.04 COL 100
     Cmb_Campos AT ROW 6.92 COL 15 COLON-ALIGNED
     Cmb_Tipos AT ROW 8.23 COL 15.43 COLON-ALIGNED
     Pro_scoring.Rango_inicial AT ROW 8.27 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 34.72 BY .81
          BGCOLOR 15 
     BUTTON-142 AT ROW 9 COL 99
     Pro_scoring.Rango_final AT ROW 9.35 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 34.72 BY .81
          BGCOLOR 15 
     BUTTON-140 AT ROW 10 COL 39
     BUTTON-141 AT ROW 10 COL 44
     NomTipoCampo AT ROW 10.23 COL 14.86 COLON-ALIGNED
     Pro_scoring.Puntaje AT ROW 10.42 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 11.54 COL 100.29
     Pro_scoring.Fec_Creacion AT ROW 11.62 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Pro_scoring.observacion AT ROW 12.58 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 59.29 BY 2.15
          BGCOLOR 15 
     Btn_Ingresar AT ROW 13.19 COL 100.29
     Btn_Eliminar AT ROW 14.88 COL 100.29
     Br_ProScorin AT ROW 15.08 COL 1.72
     Btn_Cancelar AT ROW 16.69 COL 100.29
     Btn_Salir AT ROW 18.31 COL 100.29
     BUTTON-137 AT ROW 20.62 COL 105
     RECT-281 AT ROW 11.35 COL 99
     RECT-282 AT ROW 1.54 COL 99
     "Observación" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 12 COL 2.14
          FGCOLOR 7 
     "Label del Campo" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 6.12 COL 18
          FGCOLOR 7 
     "Nombre del Campo" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 6.12 COL 49
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Procesar
     R1 AT ROW 1.27 COL 4 NO-LABEL
     F_Nit AT ROW 2.35 COL 26 COLON-ALIGNED
     BUTTON-146 AT ROW 3.69 COL 4
     BUTTON-145 AT ROW 3.69 COL 37
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 2.08
         SIZE 46 BY 5.65
         BGCOLOR 17 FONT 4
         TITLE "Llenar Scoring".


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
         TITLE              = "SFG - Configuracion variables Scoring, Programa W-Cf_Segmentar.W"
         HEIGHT             = 21.38
         WIDTH              = 114.29
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
ASSIGN FRAME F_Procesar:FRAME = FRAME F_Scoring:HANDLE.

/* SETTINGS FOR FRAME F_Procesar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Procesar:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F_Nit IN FRAME F_Procesar
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Scoring
                                                                        */
/* BROWSE-TAB Br_ProScorin Btn_Eliminar F_Scoring */
/* SETTINGS FOR BUTTON BUTTON-140 IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-141 IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-143 IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Campos IN FRAME F_Scoring
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Codigos IN FRAME F_Scoring
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Tablas IN FRAME F_Scoring
   1                                                                    */
/* SETTINGS FOR FILL-IN Pro_scoring.Fec_Creacion IN FRAME F_Scoring
   NO-ENABLE                                                            */
ASSIGN 
       Pro_scoring.Fec_Creacion:READ-ONLY IN FRAME F_Scoring        = TRUE.

/* SETTINGS FOR FILL-IN NomTipoCampo IN FRAME F_Scoring
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ProScorin
/* Query rebuild information for BROWSE Br_ProScorin
     _START_FREEFORM
OPEN QUERY Br_ProScorin FOR EACH Pro_scoring NO-LOCK
                                 BY Codigo BY VARIABLE.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_ProScorin */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Procesar
/* Query rebuild information for FRAME F_Procesar
     _Query            is NOT OPENED
*/  /* FRAME F_Procesar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Scoring
/* Query rebuild information for FRAME F_Scoring
     _Query            is NOT OPENED
*/  /* FRAME F_Scoring */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Configuracion variables Scoring, Programa W-Cf_Segmentar.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Configuracion variables Scoring, Programa W-Cf_Segmentar.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_ProScorin
&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME Br_ProScorin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_ProScorin wWin
ON MOUSE-SELECT-CLICK OF Br_ProScorin IN FRAME F_Scoring
DO:
  IF AVAIL(Pro_Scoring) THEN DO:
     W_Cr = FALSE.
  
     ENABLE Btn_Ingresar.
     
     RUN Mostrar_Registro.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Scoring /* Cancelar */
DO:
  DO WITH FRAME F_Scoring:
     DISABLE Btn_Salvar.
     RUN Inicializar_Campos.
     IF AVAILABLE Pscoring THEN
       ASSIGN Pro_scoring.Puntaje               = Pscoring.Puntaje
              Pro_scoring.rango_inicial         = Pscoring.rango_inicial
              Pro_scoring.rango_final           = Pscoring.rango_final
              Pro_scoring.Fec_creacion          = Pscoring.Fec_creacion
              Pro_scoring.tabla                 = Pscoring.tabla
              pro_scoring.VARIABLE              = Pscoring.VARIABLE.
     FOR EACH Pscoring: DELETE Pscoring. END.
     ENABLE Btn_Ingresar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Eliminar wWin
ON CHOOSE OF Btn_Eliminar IN FRAME F_Scoring /* Eliminar */
DO:
  IF AVAIL(Pro_scoring) THEN DO:
     MESSAGE "Esta Segura(o) de Eliminar el Registro en Pantalla...?" SKIP
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Eliminar" 
           UPDATE W_SiConf AS LOGICAL.
     IF NOT W_SiConf THEN
        RETURN.

     FIND CURRENT Pro_scoring NO-ERROR.
     DELETE Pro_scoring.

     CLOSE QUERY Br_ProScorin.
     OPEN QUERY Br_ProScorin FOR EACH Pro_Scoring NO-LOCK BY Codigo BY VARIABLE.

     APPLY "Choose" TO Btn_Ingresar.
  END.
  ELSE 
     MESSAGE "No hay registro en pantalla para la operacion."
         VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Scoring /* Ingresar */
DO:
  DO WITH FRAME F_Scoring:
     W_Cr = TRUE.

     IF AVAILABLE(pro_scoring) THEN 
        BUFFER-COPY pro_scoring TO Pscoring.
     
     RELEASE pro_scoring.

     RUN Inicializar_Campos.

     ENABLE Btn_Salvar.
     DISABLE Btn_Ingresar.

     APPLY "entry" TO Cmb_Codigos.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Scoring /* Salir */
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


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Scoring /* Salvar */
DO:
  DEFI VAR W_RowidPS AS ROWID.

  DO WITH FRAME F_Scoring:
   IF W_Cr AND AVAILABLE(pro_scoring) THEN DO:
      MESSAGE "Si esta ingresando no debe haber Registro en pantalla."
          VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   IF W_Cr THEN DO: 
      CREATE Pro_scoring.
      W_Cr = FALSE.
   END.
   ELSE FIND CURRENT Pro_scoring NO-ERROR.

   ASSIGN Pro_scoring.Puntaje       = DECIMAL(Pro_scoring.Puntaje:SCREEN-VALUE)       
         Pro_scoring.Fec_creacion   = DATE(Pro_scoring.Fec_creacion:SCREEN-VALUE)            
         Pro_scoring.tabla          = Cmb_Tablas:SCREEN-VALUE                                
         pro_scoring.VARIABLE       = SUBSTRING(Cmb_Campos:SCREEN-VALUE,34,30)               
         pro_scoring.Titulo         = SUBSTRING(Cmb_Campos:SCREEN-VALUE,1,30)                
         Pro_Scoring.Codigo         = INTEGER(SUBSTRING(Cmb_Codigos:SCREEN-VALUE,1,5))       
         Pro_Scoring.TipoCampo      = INTEGER(SUBSTRING(Cmb_Tipos:SCREEN-VALUE,1,1)).        

   ASSIGN Pro_scoring.rango_inicial = Pro_scoring.rango_inicial:SCREEN-VALUE NO-ERROR.
   ASSIGN Pro_scoring.rango_final   = Pro_scoring.rango_final:SCREEN-VALUE   NO-ERROR.

    /*RUN Mostrar_Registro*/

   FIND CURRENT Pro_scoring NO-LOCK NO-ERROR.
   ENABLE Btn_Ingresar.

   ASSIGN W_RowidPS = ROWID (Pro_Scoring).

   CLOSE QUERY Br_ProScorin.
   OPEN QUERY Br_ProScorin FOR EACH Pro_Scoring NO-LOCK BY Codigo BY VARIABLE.
   REPOSITION Br_ProScorin TO ROWID W_RowidPS.
   
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-139
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-139 wWin
ON CHOOSE OF BUTTON-139 IN FRAME F_Scoring /* Button 139 */
DO:
  RUN W-InfDia.R NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-140
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-140 wWin
ON CHOOSE OF BUTTON-140 IN FRAME F_Scoring /* Button 140 */
DO:
  FIND NEXT pro_scoring NO-ERROR.
  IF NOT AVAILABLE(pro_scoring) THEN 
     FIND FIRST pro_scoring NO-ERROR.
  RUN mostrar_registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-141
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-141 wWin
ON CHOOSE OF BUTTON-141 IN FRAME F_Scoring /* Button 141 */
DO:
  FIND PREV pro_scoring NO-ERROR.
  IF NOT AVAILABLE(pro_scoring) THEN 
     FIND LAST pro_scoring NO-ERROR.
  RUN mostrar_registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-142
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-142 wWin
ON CHOOSE OF BUTTON-142 IN FRAME F_Scoring /* Llena Scoring */
DO:
VIEW FRAME F_Procesar.
DO WITH FRAME F_Procesar:
  DISABLE F_Nit.
  ASSIGN R1:SCREEN-VALUE    = "1"
         F_Nit:SCREEN-VALUE = "".
  APPLY "entry" TO R1.
  RETURN NO-APPLY.
END.              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F_Scoring /* Button 143 */
DO:
  FileTmp = Cmb_Tablas:SCREEN-VALUE IN FRAME F_Scoring.
  DEFINE VARIABLE P_Codigo   LIKE Pro_Scoring.Codigo.
  DEFINE VARIABLE P_Tabla    LIKE Pro_Scoring.Tabla.
  DEFINE VARIABLE P_Variable LIKE Pro_Scoring.VARIABLE.
  DEFINE VARIABLE PunteroS AS ROWID.
  ASSIGN PunteroS = ?
         W_Cr     = NO.
  RUN C-Scoring.r (OUTPUT PunteroS).
  PunteroActual = PunteroS.
  ENABLE Btn_Ingresar.
  FIND Pro_Scoring WHERE ROWID(Pro_Scoring) EQ PunteroS NO-ERROR.
  /*Pro_Scoring.Codigo EQ P_Codigo AND
                         Pro_Scoring.Tabla  EQ P_Tabla  AND
                         Pro_Scoring.VARIABLE EQ P_Variable AND NO-LOCK NO-ERROR*/.
  IF AVAILABLE Pro_Scoring THEN 
     RUN Mostrar_Registro.
  ELSE DO:
     MESSAGE "No encontro el registro de pro_scoring".
     APPLY "Choose" TO Btn_Ingresar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-144
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-144 wWin
ON CHOOSE OF BUTTON-144 IN FRAME F_Scoring /* Button 144 */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
   Listado = W_PathSpl + "Scoring.LST".
  {Incluido\Imprimir.I "Listado"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Procesar
&Scoped-define SELF-NAME BUTTON-145
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-145 wWin
ON CHOOSE OF BUTTON-145 IN FRAME F_Procesar /* Button 145 */
DO:
  HIDE FRAME F_Procesar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-146
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-146 wWin
ON CHOOSE OF BUTTON-146 IN FRAME F_Procesar /* Procesar */
DO:
  ASSIGN FRAME F_Procesar F_Nit.
  F_Nit = '"' + F_Nit + '"'.
  FOR EACH pro_scoring :
      ASSIGN Wvble = pro_scoring.VARIABLE.
      RUN Prc_Scoring.p Tabla WVble rango_inicial rango_final puntaje 1 F_Nit Pro_Scoring.Codigo.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME Cmb_Campos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Campos wWin
ON VALUE-CHANGED OF Cmb_Campos IN FRAME F_Scoring /* Variable */
DO:
  Pro_Scoring.Observacion:SCREEN-VALUE = "".
  FIND Tmp WHERE Tmp.TVariable EQ SUBSTRING(Cmb_Campos:SCREEN-VALUE,34,30) NO-ERROR.
  IF AVAILABLE Tmp THEN
     ASSIGN Pro_Scoring.Observacion:SCREEN-VALUE = Tmp.TDescripcion.
     CASE Tmp.TTipoCampo:
       WHEN "character" THEN
         NomTipoCampo:SCREEN-VALUE = "character".
       WHEN "date" THEN
         NomTipoCampo:SCREEN-VALUE = "date".
       OTHERWISE
         NomTipoCampo:SCREEN-VALUE = "integer o decimal".
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Tablas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tablas wWin
ON VALUE-CHANGED OF Cmb_Tablas IN FRAME F_Scoring /* Archivo */
DO:
  Cmb_Campos:LIST-ITEMS = "".
  W_Ok = Cmb_Campos:ADD-LAST("No Asignado").
  Cmb_Campos:SCREEN-VALUE = Cmb_Campos:ENTRY(1).
  FIND FIRST _file WHERE _file._file-name = Cmb_Tablas:SCREEN-VALUE no-error.
  RUN Llenar_CmbCampos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Procesar
&Scoped-define SELF-NAME R1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R1 wWin
ON VALUE-CHANGED OF R1 IN FRAME F_Procesar
DO:
DO WITH FRAME F_Procesar:
  ASSIGN FRAME F_Procesar R1.
  IF R1 EQ 2 THEN DO: 
     ENABLE F_Nit.
     APPLY "entry" TO F_Nit.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     DISABLE F_Nit.
     F_Nit:SCREEN-VALUE = "".
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME Pro_scoring.Rango_inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pro_scoring.Rango_inicial wWin
ON LEAVE OF Pro_scoring.Rango_inicial IN FRAME F_Scoring /* Rango Inicial */
DO:
  /*Puntero = ROWID(Pro_Scoring).
DO WITH FRAME F_Scoring:
  FIND Pro_Scoring WHERE 
       Pro_Scoring.Codigo EQ INTEGER(SUBSTRING(Cmb_Codigos:SCREEN-VALUE,1,5)) AND
       Pro_Scoring.Tabla  EQ Cmb_Tablas:SCREEN-VALUE AND
       Pro_Scoring.VARIABLE EQ Cmb_Campos:SCREEN-VALUE NO-ERROR.
  IF AVAILABLE Pro_Scoring THEN DO:
     IF Pro_Scoring.Rango_Inicial EQ SELF:SCREEN-VALUE THEN DO:
         MESSAGE "Para este segmento, esta tabla y este campo" SKIP
                 "ya existe este rango inicial." SKIP(2)
                 "el rango existente es:" SKIP
                 "Rango Inicial: " Pro_Scoring.Rango_Inicial SKIP
                 "Rango Final  : " Pro_Scoring.Rango_Final SKIP
                 "debe crear un rango diferente a este!!!"
                 VIEW-AS ALERT-BOX WARNING.
         SELF:SCREEN-VALUE = "".
         APPLY "entry" TO Cmb_Campos.
         RETURN NO-APPLY.
     END.
  END.
  ELSE FIND Pro_Scoring WHERE ROWID(Pro_Scoring) EQ PunteroActual NO-ERROR.
END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Procesar
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
  DISPLAY Cmb_Codigos Cmb_Tablas Cmb_Campos Cmb_Tipos NomTipoCampo 
      WITH FRAME F_Scoring IN WINDOW wWin.
  IF AVAILABLE Pro_scoring THEN 
    DISPLAY Pro_scoring.Rango_inicial Pro_scoring.Rango_final Pro_scoring.Puntaje 
          Pro_scoring.Fec_Creacion Pro_scoring.observacion 
      WITH FRAME F_Scoring IN WINDOW wWin.
  ENABLE BUTTON-139 BUTTON-144 Cmb_Codigos Cmb_Tablas Cmb_Campos Cmb_Tipos 
         Pro_scoring.Rango_inicial BUTTON-142 Pro_scoring.Rango_final 
         Pro_scoring.Puntaje Btn_Salvar Pro_scoring.observacion Btn_Ingresar 
         Btn_Eliminar Br_ProScorin Btn_Cancelar Btn_Salir BUTTON-137 RECT-281 
         RECT-282 
      WITH FRAME F_Scoring IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Scoring}
  DISPLAY R1 F_Nit 
      WITH FRAME F_Procesar IN WINDOW wWin.
  ENABLE R1 BUTTON-146 BUTTON-145 
      WITH FRAME F_Procesar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Procesar}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Campos wWin 
PROCEDURE Inicializar_Campos :
DO WITH FRAME F_Scoring:
   ASSIGN Pro_scoring.Puntaje:SCREEN-VALUE        = " "
          Pro_scoring.rango_inicial:SCREEN-VALUE  = " "
          Pro_scoring.rango_final:SCREEN-VALUE    =  " "
          Cmb_Campos:SCREEN-VALUE                 = Cmb_Campos:ENTRY(1)
          Cmb_Tablas:SCREEN-VALUE                 = Cmb_Tablas:ENTRY(1)
          Pro_scoring.Fec_creacion:SCREEN-VALUE   = STRING(W_Fecha).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
HIDE FRAME F_Procesar.
DO WITH FRAME F_Scoring:
  FOR EACH Varios WHERE Varios.Tipo EQ 19 NO-LOCK:
    W_Ok = Cmb_Codigos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
  END.
  FOR EACH _file  WHERE NOT _FILE._HIDDEN:
     W_Ok = Cmb_Tablas:ADD-LAST(STRING(_file._FILE-NAME)).
     IF AVAILABLE Pro_Scoring AND STRING(_file._FILE-NAME) EQ Pro_Scoring.Tabla THEN DO:
        Cmb_Tablas:SCREEN-VALUE = _file._FILE-NAME.
        RUN Llenar_CmbCampos.
     END.
  END.
  FIND FIRST Pro_Scoring NO-ERROR.
  IF AVAILABLE Pro_Scoring THEN DO:
    FIND Varios WHERE Varios.Tipo EQ 19 AND Varios.Codigo EQ Pro_Scoring.Codigo NO-LOCK.
    IF AVAILABLE Varios THEN
       Cmb_Codigos:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
    RUN Mostrar_Registro.
  END.
  ELSE DO:
    ASSIGN Cmb_Tablas:SCREEN-VALUE = Cmb_Tablas:ENTRY(1)
           Cmb_Campos:SCREEN-VALUE = Cmb_Campos:ENTRY(1).
    RUN Inicializar_Campos.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_CmbCampos wWin 
PROCEDURE Llenar_CmbCampos :
FOR EACH Tmp: DELETE Tmp. END.

FOR EACH _field OF _file:
      W_Ok = Cmb_Campos:ADD-LAST(STRING(_Field._Label,"X(30)") + " - " + STRING(_field._field-name,"x(30)")) IN FRAME F_Scoring.
      CREATE Tmp.
      ASSIGN TVariable = STRING(_field._field-name)
             TDescripcion = STRING(_Field._Desc)
             TTipoCampo   = STRING(_data-type).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llena_Scoring wWin 
PROCEDURE Llena_Scoring :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* llena la tabla de scoring con puntajes */
/*
  FOR EACH TABLA:
     IF CAMPO GE RANGOINICIAL AND CAMPO LE RANGOFINAL THEN
        LLENA SCORING.
  END.
*/
/*
DEFINE VARIABLE Wage LIKE Agencias.agencia.
DEFINE VARIABLE WNit LIKE clientes.nit.
/*DEFINE SHARED VARIABLE  Wvble LIKE scoring.VARIABLE.*/

FOR EACH {1}:
    ASSIGN Wage = Agencia
           Wnit = nit.
   
    IF {2} GE {3} AND {2} LE {4} THEN DO:
       CREATE SCORING.
       ASSIGN SCORING.agencia  = wage    
              SCORING.Nit             = Wnit 
              SCORING.Num_Solicitud   = {6}
              SCORING.Fec_Solitud     = TODAY   
              SCORING.VARIABLE        = wvble   
              SCORING.Valor_Variable  =  {2}
              SCORING.puntaje         = {5}
              SCORING.Cod_Producto    = 1.
      END.
END.

  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
DEFINE VAR temow AS CHARACTER FORMAT "X(125)".
DO WITH FRAME F_Scoring:
   ENABLE Btn_Salvar.

   IF Pro_Scoring.Tabla NE FileTmp THEN DO:
      FIND FIRST _file WHERE _file._file-name = Pro_Scoring.Tabla no-error.
      RUN Llenar_CmbCampos.
   END.

   ASSIGN Pro_scoring.Puntaje:SCREEN-VALUE         = STRING(Pro_scoring.Puntaje)
          Pro_scoring.rango_inicial:SCREEN-VALUE   = string(Pro_scoring.rango_inicial)
          Pro_scoring.rango_final:SCREEN-VALUE     = string(Pro_scoring.rango_final)
          Pro_scoring.Fec_creacion:SCREEN-VALUE    = STRING(Pro_scoring.Fec_creacion)
          Cmb_Tablas:SCREEN-VALUE                  = Pro_Scoring.Tabla.
          Cmb_Campos:SCREEN-VALUE                  = STRING(Pro_Scoring.Titulo,"X(30)") + " - " + STRING(Pro_Scoring.VARIABLE,"X(30)").
          Pro_Scoring.Observacion:SCREEN-VALUE     = Pro_Scoring.Observacion.
   CASE Pro_Scoring.TipoCampo:
     WHEN 1 THEN
       ASSIGN NomTipoCampo:SCREEN-VALUE = "character"
              Cmb_Tipos:SCREEN-VALUE    = "1 - Caracter".
     WHEN 2 THEN
       ASSIGN NomTipoCampo:SCREEN-VALUE = "date"
              Cmb_Tipos:SCREEN-VALUE    = "2 - Fecha".
     OTHERWISE
       ASSIGN NomTipoCampo:SCREEN-VALUE = "integer o decimal"
              Cmb_Tipos:SCREEN-VALUE    = "3 - Entero/Decimal".
   END CASE.

   FIND Varios WHERE Varios.Tipo EQ 19 AND Varios.Codigo EQ Pro_Scoring.Codigo NO-LOCK.
   IF AVAILABLE Varios THEN
      Cmb_Codigos:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
   ELSE
      Cmb_Codigos:SCREEN-VALUE = Cmb_Codigos:ENTRY(1).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    

  DEFINE VAR Tot_Tabla    AS DECIMAL FORMAT ">>,>>9".
  DEFINE VAR Tot_Segmento AS DECIMAL FORMAT ">>,>>9".
  
  W_Reporte    = "REPORTE   : CONFIGURACION DEL SCORING - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " BANCO           NOMBRE".
  /*" BANCO           NOMBRE                 CJ.LOC D.REM.COB D.REM.NEG  DED.REM.COB DED.REM.NEG      ".*/
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Pro_Scoring NO-LOCK BREAK BY Pro_Scoring.Codigo BY Pro_Scoring.Tabla BY Pro_Scoring.Rango_Inicial:
      IF FIRST-OF(Pro_Scoring.Codigo) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 19 AND Varios.Codigo EQ Pro_Scoring.Codigo NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            DISPLAY "Segmento :  "      AT 1 
                    Varios.Descripcion  AT 15 WITH FRAME F_Seg WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
      END.
      IF FIRST-OF(Pro_Scoring.Tabla) THEN DO:
         DISPLAY "Tabla    :  " AT 1
                 Pro_Scoring.Tabla AT 15 WITH FRAME F_Tab WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
      END.
      ASSIGN Tot_Tabla    = Tot_Tabla + Pro_Scoring.Puntaje
             Tot_Segmento = Tot_Segmento + Pro_Scoring.Puntaje.
      DISPLAY Pro_scoring.Titulo        AT 1   FORMAT "X(25)"
              Pro_scoring.VARIABLE      AT 30  FORMAT "X(25)"
              Pro_scoring.Rango_inicial AT 60  FORMAT "X(15)"
              Pro_scoring.Rango_final   AT 80  FORMAT "X(15)"
              Pro_scoring.Puntaje       AT 100 FORMAT "999"
      WITH FRAME F-Sc DOWN WIDTH 132 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
      DOWN WITH FRAME F-Sc.
      IF LAST-OF(Pro_Scoring.Tabla) THEN DO:
         DISPLAY "Total Tabla     : " AT 60
                 Tot_Tabla            AT 97 WITH FRAME T_Tab USE-TEXT NO-BOX WIDTH 132 NO-LABELS.
         Tot_Tabla = 0.
      END.
      IF LAST-OF(Pro_Scoring.Codigo) THEN DO:
         DISPLAY "Total Segmento  : " AT 60
                 Tot_Segmento         AT 97 WITH FRAME T_Seg USE-TEXT NO-BOX WIDTH 132 NO-LABELS.
         Tot_Segmento = 0.
      END.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

