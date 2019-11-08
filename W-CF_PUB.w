&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

   /*{Incluido\VARIABLE.I "SHARED"}*/

DEFINE VAR W_New AS LOGICAL INITIAL NO.
DEFINE VAR Puntero AS ROWID.
DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
DEFINE SHARED VAR W_Manija         AS   HANDLE.
DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.

DEFINE VAR li AS INTEGER FORMAT 9.
DEFINE VAR lf AS INTEGER FORMAT 9.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME B_PUB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PUB

/* Definitions for BROWSE B_PUB                                         */
&Scoped-define FIELDS-IN-QUERY-B_PUB PUB.Ente PUB.Clase PUB.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_PUB   
&Scoped-define SELF-NAME B_PUB
&Scoped-define QUERY-STRING-B_PUB FOR EACH PUB BY PUB.Ente INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_PUB OPEN QUERY {&SELF-NAME} FOR EACH PUB BY PUB.Ente INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_PUB PUB
&Scoped-define FIRST-TABLE-IN-QUERY-B_PUB PUB


/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain PUB.Ente PUB.Nombre PUB.Estado ~
PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Usuario PUB.Clase 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain PUB.Ente PUB.Nombre PUB.Estado ~
PUB.Clase 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain PUB
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain PUB
&Scoped-define QUERY-STRING-fMain FOR EACH PUB SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH PUB SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain PUB
&Scoped-define FIRST-TABLE-IN-QUERY-fMain PUB


/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-B_PUB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS PUB.Ente PUB.Nombre PUB.Estado PUB.Clase 
&Scoped-define ENABLED-TABLES PUB
&Scoped-define FIRST-ENABLED-TABLE PUB
&Scoped-Define ENABLED-OBJECTS EDes BUTTON-49 BUTTON-1 BUTTON-2 B_CrSalvar ~
B_CRIngresar BCancelar B_CRBorrar BtnDone RECT-301 
&Scoped-Define DISPLAYED-FIELDS PUB.Ente PUB.Nombre PUB.Estado ~
PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Usuario PUB.Clase 
&Scoped-define DISPLAYED-TABLES PUB
&Scoped-define FIRST-DISPLAYED-TABLE PUB
&Scoped-Define DISPLAYED-OBJECTS EDes Nom_Usuario 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 EDes PUB.Ente PUB.Nombre PUB.Estado 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BCancelar 
     LABEL "Cancelar" 
     SIZE 13 BY 1.65.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 1" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 13 BY 1.54.

DEFINE BUTTON B_CRBorrar 
     LABEL "Borrar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CRIngresar 
     LABEL "Ingresar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CrSalvar 
     LABEL "Salvar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON C_CRDeshacer 
     LABEL "Deshacer" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE EDes AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 52 BY 4.31
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Usuario AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 1.35.

DEFINE BUTTON BUTTON-60 
     LABEL "Ocultar" 
     SIZE 10 BY 1.12.

DEFINE VARIABLE RClases AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Perspectivas", 1,
"Subgerencias", 2,
"Grupos", 3,
"Usuarios", 4,
"Todo", 5
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE REstados AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 20 BY .54 NO-UNDO.

DEFINE RECTANGLE RECT-328
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 1.35.

DEFINE RECTANGLE RECT-329
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.27.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_PUB FOR 
      PUB SCROLLING.

DEFINE QUERY fMain FOR 
      PUB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_PUB wWin _FREEFORM
  QUERY B_PUB NO-LOCK DISPLAY
      PUB.Ente WIDTH 08 PUB.Clase PUB.Nombre
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 53 BY 9.96
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     EDes AT ROW 9.88 COL 26 NO-LABEL
     PUB.Ente AT ROW 4.77 COL 24 COLON-ALIGNED
          LABEL "Ente"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     PUB.Nombre AT ROW 5.85 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY .81
          BGCOLOR 15 
     PUB.Estado AT ROW 7.19 COL 26 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 19 BY .81
     BUTTON-49 AT ROW 3.15 COL 99
     BUTTON-1 AT ROW 5.08 COL 99
     BUTTON-2 AT ROW 6.69 COL 99
     PUB.Fec_Ingreso AT ROW 6.92 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     PUB.Fec_Retiro AT ROW 8 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     B_CrSalvar AT ROW 8.85 COL 99
     C_CRDeshacer AT ROW 10.46 COL 99
     B_CRIngresar AT ROW 12.08 COL 99
     BCancelar AT ROW 13.69 COL 99
     B_CRBorrar AT ROW 15.31 COL 99
     PUB.Usuario AT ROW 15.54 COL 24 COLON-ALIGNED
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Nom_Usuario AT ROW 15.54 COL 36 COLON-ALIGNED NO-LABEL
     BtnDone AT ROW 16.92 COL 99
     PUB.Clase AT ROW 3.69 COL 26 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Perspectivas":U, 1,
"Subgerencias":U, 2,
"Grupos":U, 3,
"Usuarios":U, 4
          SIZE 46 BY .62
     RECT-301 AT ROW 15.27 COL 19
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 20.31
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Consulta
     B_PUB AT ROW 1.27 COL 2
     RClases AT ROW 11.77 COL 4 NO-LABEL
     BUTTON-60 AT ROW 13.12 COL 45
     REstados AT ROW 13.38 COL 4 NO-LABEL
     RECT-328 AT ROW 11.5 COL 2
     RECT-329 AT ROW 12.92 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 5.58
         SIZE 56 BY 14.27
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Entes PUB".


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
         TITLE              = "Configuración de los Entes PUB"
         HEIGHT             = 20.31
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
ASSIGN FRAME F_Consulta:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Consulta:MOVE-AFTER-TAB-ITEM (EDes:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME F_Consulta:MOVE-BEFORE-TAB-ITEM (PUB.Ente:HANDLE IN FRAME fMain)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON C_CRDeshacer IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDes IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN PUB.Ente IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR RADIO-SET PUB.Estado IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN PUB.Fec_Ingreso IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PUB.Fec_Retiro IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PUB.Nombre IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN Nom_Usuario IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PUB.Usuario IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Consulta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_PUB RECT-329 F_Consulta */
ASSIGN 
       FRAME F_Consulta:HIDDEN           = TRUE
       FRAME F_Consulta:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_PUB
/* Query rebuild information for BROWSE B_PUB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH PUB BY PUB.Ente INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_PUB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.PUB"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Configuración de los Entes PUB */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Configuración de los Entes PUB */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCancelar wWin
ON CHOOSE OF BCancelar IN FRAME fMain /* Cancelar */
DO:
  IF Puntero NE ? THEN DO:
     FIND PUB WHERE ROWID(PUB) EQ Puntero NO-LOCK NO-ERROR.
     IF AVAILABLE PUB THEN DO:
        DISPLAY PUB.Ente PUB.Estado PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Nombre PUB.Clase PUB.Usuario
                WITH FRAME FMain.
        DISABLE PUB.Ente WITH FRAME FMain.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
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
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancias.Lst".
  {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
    OPEN QUERY B_PUB FOR EACH PUB
     NO-LOCK BY PUB.Ente INDEXED-REPOSITION.
  VIEW FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME fMain /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BUTTON-60
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-60 wWin
ON CHOOSE OF BUTTON-60 IN FRAME F_Consulta /* Ocultar */
DO:
  HIDE FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B_CRBorrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRBorrar wWin
ON CHOOSE OF B_CRBorrar IN FRAME fMain /* Borrar */
DO:
  /*ASSIGN PUB.Fec_Retiro = TODAY
         PUB.Estado     = 2.*/
    FIND CURRENT pub.
    DELETE PUB.
    FIND FIRST Pub NO-LOCK NO-ERROR.
    IF AVAILABLE Pub THEN RUN Mostrar_Registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CRIngresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRIngresar wWin
ON CHOOSE OF B_CRIngresar IN FRAME fMain /* Ingresar */
DO:
  W_New = YES.
  IF AVAILABLE PUB THEN Puntero = ROWID(PUB).
  ASSIGN PUB.Ente:SCREEN-VALUE      = "" 
         PUB.Fec_Ingreso:SCREEN-VALUE = STRING(TODAY)
         PUB.Fec_Retiro:SCREEN-VALUE  = "?"
         PUB.Nombre:SCREEN-VALUE      = ""
         PUB.Clase:SCREEN-VALUE        = "1"
         PUB.Usuario:SCREEN-VALUE     = "10"
         Nom_Usuario:SCREEN-VALUE     = "Cambiar a incluido"
         EDes:SCREEN-VALUE            = "".
  ENABLE {&List-1} WITH FRAME FMain.
  APPLY "entry" TO PUB.Clase.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CrSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CrSalvar wWin
ON CHOOSE OF B_CrSalvar IN FRAME fMain /* Salvar */
DO: 
IF W_New THEN DO:
   FIND PUB WHERE
        PUB.Ente EQ PUB.Ente:SCREEN-VALUE NO-ERROR.
   IF AVAILABLE PUB THEN DO:
      MESSAGE "Ya existe un Ente PUB con este código" SKIP
              "se cancela la operacion de salvado del" SKIP
              "nuevo registro" VIEW-AS ALERT-BOX.
      RUN Mostrar_Registro.
      APPLY "entry" TO PUB.Nombre.
   END.
   ELSE DO:
     CREATE PUB.
     W_New = NO.
   END.
END.
ELSE
  FIND CURRENT PUB.
ASSIGN FRAME {&FRAME-NAME} PUB.Ente PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Nombre PUB.Clase PUB.Estado EDes.
ASSIGN PUB.Descripcion = EDes.
PUB.Usuario = "10".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_PUB
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_PUB wWin
ON MOUSE-SELECT-DBLCLICK OF B_PUB IN FRAME F_Consulta
DO:
  DISPLAY PUB.Ente PUB.Estado PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Nombre PUB.Clase PUB.Usuario
          WITH FRAME FMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME C_CRDeshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_CRDeshacer wWin
ON CHOOSE OF C_CRDeshacer IN FRAME fMain /* Deshacer */
DO:
 /* RUN Deshacer_Cambios_Creacion.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PUB.Ente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PUB.Ente wWin
ON LEAVE OF PUB.Ente IN FRAME fMain /* Ente */
DO:
  FIND PUB WHERE
       PUB.Ente EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE PUB THEN DO:
     MESSAGE "El Ente PUB ya existe, a continuación" SKIP
             "se mostrara la información de este Ente" VIEW-AS ALERT-BOX INFORMATION.
     DISPLAY PUB.Ente PUB.Estado PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Nombre PUB.Clase PUB.Usuario
             WITH FRAME FMain.
     DISABLE PUB.Ente WITH FRAME FMain.
  END.
  CASE PUB.Clase:SCREEN-VALUE:
    WHEN "1" THEN DO:
        IF LENGTH(SELF:SCREEN-VALUE) NE 2 THEN DO:
           MESSAGE "Si el Clase de Ente es PERSPECTIVA," SKIP
                   "solo puede ser de 2 caracteres." SKIP(1)
                   "Se cancela el ingreso!" VIEW-AS ALERT-BOX WARNING. 
           APPLY "choose" TO BCancelar.
        END.
    END.
    WHEN "2" THEN DO:
        IF LENGTH(SELF:SCREEN-VALUE) NE 4 THEN DO:
           MESSAGE "Si el Clase de Ente es SUBGERENCIA," SKIP
                   "solo puede ser de 4 caracteres." SKIP(1)
                   "Los dos primeros identifican la PERSPECTIVA" SKIP
                   "a la que pertenece la SUBGERENCIA. " SKIP
                   "Los dos digitos siguientes identificaran la" SKIP
                   "SUBGERENCIA." SKIP(1)
                   "Se cancela el ingreso!!!" VIEW-AS ALERT-BOX WARNING. 
           APPLY "choose" TO BCancelar.
        END.
        FIND FIRST PUB WHERE PUB.Ente EQ SUBSTRING(SELF:SCREEN-VALUE,1,2) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE PUB THEN DO:
           MESSAGE "No ha sido creada la PERSPECTIVA " SUBSTRING(SELF:SCREEN-VALUE,1,2) SKIP
                   "para esta SUBGERENCIA," SKIP(1)
                   "Se cancela el ingreso!!!." VIEW-AS ALERT-BOX ERROR.
           APPLY "choose" TO BCancelar.
        END.
    END.
    WHEN "3" THEN DO:
        IF LENGTH(SELF:SCREEN-VALUE) NE 6 THEN DO:
           MESSAGE "Si el Clase de Ente es AGENCIA," SKIP
                   "solo puede ser de 6 caracteres." SKIP(1)
                   "Los 4 primeros identifican la PERSPECTIVA y" SKIP
                   "la SUBGERENCIA. " SKIP
                   "Los dos digitos siguientes identificaran la" SKIP
                   "AGENCIA." SKIP(1)
                   "Se cancela el ingreso!!!" VIEW-AS ALERT-BOX WARNING. 
           APPLY "choose" TO BCancelar.
        END.
        FIND FIRST PUB WHERE PUB.Ente EQ SUBSTRING(SELF:SCREEN-VALUE,1,4) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE PUB THEN DO:
           MESSAGE "No ha sido creada el Ente PUB mayor " SUBSTRING(SELF:SCREEN-VALUE,1,4) SKIP
                   "que contega la PERSPECTIVA Y SUBGERENCIA" SKIP(1)
                   "Se cancela el ingreso!!!" VIEW-AS ALERT-BOX ERROR.
           APPLY "choose" TO BCancelar.
        END.
    END.
    WHEN "4" THEN DO:
        IF LENGTH(SELF:SCREEN-VALUE) NE 8 THEN DO:
           MESSAGE "Si el Clase de Ente es USUARIO," SKIP
                   "solo puede ser de 8 caracteres." SKIP(1)
                   "Los 6 primeros identifican la PERSPECTIVA," SKIP
                   "la SUBGERENCIA y la AGENCIA " SKIP
                   "Los dos digitos siguientes identificaran al" SKIP
                   "USUARIO." SKIP(1)
                   "Se cancela el ingreso!!!" VIEW-AS ALERT-BOX WARNING. 
           SELF:SCREEN-VALUE = "".
           APPLY "choose" TO BCancelar.
        END.
        FIND FIRST PUB WHERE PUB.Ente EQ SUBSTRING(SELF:SCREEN-VALUE,1,6) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE PUB THEN DO:
           MESSAGE "No ha sido creado el Ente PUB mayor " SUBSTRING(SELF:SCREEN-VALUE,1,6) SKIP
                   "que contega la PERSPECTIVA, SUBGERENCIA" SKIP
                   "y la AGENCIA para este USUARIO" SKIP(1)
                   "Se cancela el ingreso!!!" VIEW-AS ALERT-BOX ERROR.
           APPLY "choose" TO BCancelar.
        END.
    END.
  END CASE.
  IF LENGTH(SELF:SCREEN-VALUE) EQ 0 THEN DO:
     MESSAGE "No se puede crear un Ente PUB" SKIP
             "que no tenga identificación." SKIP(1)
             "Se cancela el ingreso!!!" VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO BCancelar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PUB.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PUB.Estado wWin
ON VALUE-CHANGED OF PUB.Estado IN FRAME fMain /* Estado */
DO:
  /*Puntero = ROWID(Pub).
  FIND PUB WHERE
       PUB.Ente BEGINS PUB.Ente:SCREEN-VALUE AND
       LENGTH(PUB.Ente) GT LENGTH(PUB.Ente:SCREEN-VALUE) AND
       PUB.Estado EQ 1 AND PUB.Fec_Retiro EQ ? NO-ERROR.
  IF AVAILABLE PUB THEN DO:
     MESSAGE "No se puede inactivar un Ente PUB que tenga" SKIP
             "activo alguno de sus subniveles" VIEW-AS ALERT-BOX.
     SELF:SCREEN-VALUE = "1".
  END.
  ELSE DO:
      FIND PUB WHERE ROWID(pub) EQ Puntero.
      ASSIGN PUB.Estado     = 2.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME RClases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RClases wWin
ON VALUE-CHANGED OF RClases IN FRAME F_Consulta
DO:
  ASSIGN FRAME F_Consulta REstados RClases.
  IF RClases EQ 5 THEN ASSIGN li = 1 lf = 4.
  ELSE ASSIGN li = RClases lf = RClases.

  OPEN QUERY B_PUB FOR EACH PUB WHERE Pub.Clase GE li AND 
                                      Pub.Clase LE lf AND
                                      Pub.Estado EQ REstados
     NO-LOCK BY PUB.Ente INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME REstados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstados wWin
ON VALUE-CHANGED OF REstados IN FRAME F_Consulta
DO:
  APPLY "value-changed" TO RClases.
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY EDes Nom_Usuario 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE PUB THEN 
    DISPLAY PUB.Ente PUB.Nombre PUB.Estado PUB.Fec_Ingreso PUB.Fec_Retiro 
          PUB.Usuario PUB.Clase 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE EDes PUB.Ente PUB.Nombre PUB.Estado BUTTON-49 BUTTON-1 BUTTON-2 
         B_CrSalvar B_CRIngresar BCancelar B_CRBorrar BtnDone PUB.Clase 
         RECT-301 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY RClases REstados 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-328 RECT-329 B_PUB RClases BUTTON-60 REstados 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
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
RUN SUPER.
FIND FIRST PUB NO-LOCK NO-ERROR.
IF AVAILABLE PUB THEN DO:
   RUN Mostrar_Registro.
   DISABLE PUB.Ente WITH FRAME FMain.
   APPLY "entry" TO PUB.Nombre IN FRAME FMain.
END.
ELSE
   DISABLE {&List-1} WITH FRAME FMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
EDes = PUB.Descripcion.
 DISPLAY PUB.Ente PUB.Estado PUB.Fec_Ingreso PUB.Fec_Retiro PUB.Nombre PUB.Clase PUB.Usuario EDes
         WITH FRAME FMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
 W_Reporte   = "REPORTE   : ENTES PUB - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Clase    Orden    Instancia     Nombre                         TipPdt   Producto".

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 FOR EACH PUB WHERE Pub.Estado EQ 1 NO-LOCK BREAK BY Pub.Ente BY Pub.Clase:
     DISPLAY PUB.Clase PUB.Ente PUB.Nombre WITH FRAME f-mov STREAM-IO NO-LABELS USE-TEXT NO-BOX.
 END.

 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

