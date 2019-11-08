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

  {Incluido\VARIABLE.I "SHARED"}

DEFINE VAR W_New AS LOGICAL INITIAL NO.
DEFINE VAR Puntero AS ROWID.

DEFINE VAR WNomCodFila AS CHARACTER FORMAT "X(50)".
DEFINE VAR WNomEntFila AS CHARACTER FORMAT "X(50)".

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
&Scoped-define INTERNAL-TABLES Cfg_PUB Alm_Pub

/* Definitions for BROWSE B_PUB                                         */
&Scoped-define FIELDS-IN-QUERY-B_PUB Cfg_PUB.Ente WNomEntFila Cfg_PUB.Codigo WNomCodFila   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_PUB   
&Scoped-define SELF-NAME B_PUB
&Scoped-define QUERY-STRING-B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2 BY Cfg_PUB.Ente BY Cfg_PUB.Codigo  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_PUB OPEN QUERY {&SELF-NAME} FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2 BY Cfg_PUB.Ente BY Cfg_PUB.Codigo  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_PUB Cfg_PUB
&Scoped-define FIRST-TABLE-IN-QUERY-B_PUB Cfg_PUB


/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Alm_Pub.Ente Alm_Pub.Codigo ~
Alm_Pub.Fecha Alm_Pub.Acumulado Alm_Pub.Usuario 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Alm_Pub.Acumulado 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Alm_Pub
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Alm_Pub
&Scoped-define QUERY-STRING-fMain FOR EACH Alm_Pub SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Alm_Pub SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Alm_Pub
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Alm_Pub


/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-B_PUB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Alm_Pub.Acumulado 
&Scoped-define ENABLED-TABLES Alm_Pub
&Scoped-define FIRST-ENABLED-TABLE Alm_Pub
&Scoped-Define ENABLED-OBJECTS BUTTON-49 BUTTON-2 B_CrSalvar Btn_Ingresar ~
BtnDone 
&Scoped-Define DISPLAYED-FIELDS Alm_Pub.Ente Alm_Pub.Codigo Alm_Pub.Fecha ~
Alm_Pub.Acumulado Alm_Pub.Usuario 
&Scoped-define DISPLAYED-TABLES Alm_Pub
&Scoped-define FIRST-DISPLAYED-TABLE Alm_Pub
&Scoped-Define DISPLAYED-OBJECTS NomUsuario Nom_CodPUB Nom_IndPUB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 13 BY 1.54.

DEFINE BUTTON B_CrSalvar 
     LABEL "Salvar" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_CodPUB AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_IndPUB AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WBusca AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RBusca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ente", 1,
"Indicador", 2,
"Todos", 3
     SIZE 35.14 BY .81
     FONT 5 NO-UNDO.

DEFINE VARIABLE RSel AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 0,
"Perspectivas", 1,
"Subgerencias", 2,
"Grupos", 3,
"Usuarios", 4
     SIZE 60 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_PUB FOR 
      Cfg_PUB SCROLLING.

DEFINE QUERY fMain FOR 
      Alm_Pub SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_PUB wWin _FREEFORM
  QUERY B_PUB NO-LOCK DISPLAY
      Cfg_PUB.Ente     COLUMN-LABEL "Ente PUB" 
      WNomEntFila         COLUMN-LABEL "Nom.Ente" WIDTH 20
      Cfg_PUB.Codigo   COLUMN-LABEL "Codigo"
      WNomCodFila         COLUMN-LABEL "Nom.Codigo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 7.81
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     NomUsuario AT ROW 15.85 COL 27.43 COLON-ALIGNED NO-LABEL
     Nom_CodPUB AT ROW 13.65 COL 27.57 COLON-ALIGNED NO-LABEL
     Nom_IndPUB AT ROW 14.73 COL 27.57 COLON-ALIGNED NO-LABEL
     BUTTON-49 AT ROW 1.54 COL 72
     BUTTON-2 AT ROW 3.15 COL 72
     B_CrSalvar AT ROW 5.31 COL 72
     Btn_Ingresar AT ROW 6.92 COL 72
     BtnDone AT ROW 8.54 COL 72
     Alm_Pub.Ente AT ROW 13.65 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 15 
     Alm_Pub.Codigo AT ROW 14.73 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 15 
     Alm_Pub.Fecha AT ROW 16.88 COL 27.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Alm_Pub.Acumulado AT ROW 17.96 COL 27.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.57 BY .81
          BGCOLOR 15 
     Alm_Pub.Usuario AT ROW 15.81 COL 18 COLON-ALIGNED
          LABEL "Usuario que Alimenta"
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.57 BY 18.19
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Consulta
     B_PUB AT ROW 1.27 COL 2
     RSel AT ROW 9.62 COL 5 NO-LABEL
     RBusca AT ROW 11.08 COL 6.86 NO-LABEL
     WBusca AT ROW 11.12 COL 41 COLON-ALIGNED NO-LABEL
     RECT-302 AT ROW 10.69 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.27
         SIZE 69 BY 12.12
         BGCOLOR 17 FONT 4.


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
         HEIGHT             = 18.19
         WIDTH              = 86.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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

ASSIGN XXTABVALXX = FRAME F_Consulta:MOVE-BEFORE-TAB-ITEM (NomUsuario:HANDLE IN FRAME fMain)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN Alm_Pub.Codigo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Alm_Pub.Ente IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Alm_Pub.Fecha IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_CodPUB IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_IndPUB IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Alm_Pub.Usuario IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Consulta
                                                                        */
/* BROWSE-TAB B_PUB RECT-302 F_Consulta */
ASSIGN 
       FRAME F_Consulta:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_PUB
/* Query rebuild information for BROWSE B_PUB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2 BY Cfg_PUB.Ente BY Cfg_PUB.Codigo  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_PUB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Alm_Pub"
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


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME fMain /* Ingresar */
DO:
  W_New = YES.
 /* IF AVAILABLE Ind_PUB THEN DO:
      Puntero = ROWID(Ind_PUB).*/
      DO WITH FRAME FMain:
         ASSIGN  Alm_Pub.Fecha:SCREEN-VALUE       = STRING(TODAY)
                 Alm_Pub.Usuario:SCREEN-VALUE     = W_Usuario
                 Alm_Pub.Acumulado:SCREEN-VALUE   = "0".
      END.
  /*END.*/
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.
  ENABLE {&List-1} WITH FRAME FMain.
  APPLY "entry" TO Alm_PUB.Acumulado.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
    OPEN QUERY B_PUB FOR EACH Cfg_PUB 
     NO-LOCK BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
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


&Scoped-define SELF-NAME B_CrSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CrSalvar wWin
ON CHOOSE OF B_CrSalvar IN FRAME fMain /* Salvar */
DO: 
IF W_New THEN DO:
   FIND Alm_PUB WHERE
        Alm_PUB.Codigo EQ INTEGER(Alm_PUB.Codigo:SCREEN-VALUE) AND
        Alm_PUB.Ente EQ Alm_PUB.Ente:SCREEN-VALUE NO-ERROR.
   IF AVAILABLE Alm_PUB THEN DO:
      MESSAGE "Ya el registro de acumulado de balance" SKIP 
              "se cancela la operacion de salvado del" SKIP
              "nuevo registro" SKIP
              "ahora se mostrara la informacion y se" SKIP
              "podra modificar el campo acumulado!" VIEW-AS ALERT-BOX.
      RUN Mostrar_Registro.
      APPLY "Entry" TO Alm_Pub.Acumulado.
   END.
   ELSE DO:
     CREATE Alm_PUB.
     W_New = NO.
   END.
END.
ELSE 
  FIND CURRENT Alm_PUB.
ASSIGN FRAME FMain Alm_PUB.Codigo Alm_PUB.Ente 
        Alm_PUB.Fecha Alm_PUB.Usuario Alm_Pub.Acumulado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_PUB
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME B_PUB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_PUB wWin
ON MOUSE-SELECT-DBLCLICK OF B_PUB IN FRAME F_Consulta
DO:
 FIND Alm_Pub WHERE
      Alm_Pub.Ente   EQ Cfg_Pub.Ente   AND
      Alm_Pub.Codigo EQ Cfg_Pub.Codigo AND
      Alm_Pub.Fecha  EQ W_Fecha NO-LOCK NO-ERROR.
 IF AVAILABLE Alm_Pub THEN DO:
     RUN Mostrar_Registro.
     APPLY "Entry" TO Alm_Pub.Acumulado IN FRAME FMain.
 END.
 ELSE DO:
    ASSIGN Alm_Pub.Ente:SCREEN-VALUE IN FRAME FMain = Cfg_Pub.Ente
           Alm_Pub.Codigo:SCREEN-VALUE              = STRING(Cfg_Pub.Codigo).
    APPLY "choose" TO Btn_Ingresar IN FRAME FMain.
 END.
 ASSIGN Nom_CodPub:SCREEN-VALUE IN FRAME FMain = WNomEntFila:SCREEN-VALUE IN BROWSE B_Pub
        Nom_IndPub:SCREEN-VALUE = WNomCodFila:SCREEN-VALUE IN BROWSE B_Pub.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_PUB wWin
ON ROW-DISPLAY OF B_PUB IN FRAME F_Consulta
DO:
  FIND PUB WHERE PUB.Ente EQ Cfg_PUB.Ente NO-LOCK NO-ERROR.
  IF AVAILABLE PUB THEN
     WNomEntFila = PUB.Nombre.
  ELSE
     WNomEntFila = "No Existe en el PUB".
  FIND Ind_PUB WHERE Ind_PUB.Codigo EQ Cfg_PUB.Codigo NO-LOCK NO-ERROR.
  IF AVAILABLE Ind_PUB THEN
     WNomCodFila = Ind_PUB.Nombre.
  ELSE
     WNomCodFila = "No Existe en  Indicadores PUB".
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RBusca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RBusca wWin
ON VALUE-CHANGED OF RBusca IN FRAME F_Consulta
DO:

  IF INTEGER(SELF:SCREEN-VALUE) EQ 3 THEN DO:
     DISABLE WBusca WITH FRAME F_Consulta.
     WBusca:SCREEN-VALUE = "".
     IF RSel EQ 0 THEN
        OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
             BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
     ELSE
        OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND LENGTH(Cfg_Pub.Ente) EQ (RSel * 2) AND Cfg_Pub.Id_Alimentacion EQ 2
             BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.

  END.
  ELSE DO:
     ENABLE WBusca WITH FRAME F_Consulta.
     APPLY "Entry" TO WBusca.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RSel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RSel wWin
ON VALUE-CHANGED OF RSel IN FRAME F_Consulta
DO:
    ASSIGN FRAME F_Consulta RSel RBusca WBusca.
    CASE RSel:
        WHEN 0 THEN DO:
            IF RBusca EQ 1 THEN
               OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
                 BY Cfg_PUB.Codigo BY Cfg_PUB.Ente INDEXED-REPOSITION.
            ELSE
               OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
                 BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
        END.
        WHEN 1 THEN DO:
               OPEN QUERY B_PUB FOR EACH Cfg_PUB 
                    WHERE LENGTH(Cfg_Pub.Ente) EQ 2 AND Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
                    BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
        END.
        WHEN 2 THEN DO:
               OPEN QUERY B_PUB FOR EACH Cfg_PUB 
                    WHERE LENGTH(Cfg_Pub.Ente) EQ 4 AND Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
                    BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
        END.
        WHEN 3 THEN DO:
               OPEN QUERY B_PUB FOR EACH Cfg_PUB 
                    WHERE LENGTH(Cfg_Pub.Ente) EQ 6 AND Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
                    BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
        END.
        WHEN 4 THEN DO:
               OPEN QUERY B_PUB FOR EACH Cfg_PUB 
                    WHERE LENGTH(Cfg_Pub.Ente) EQ 8 AND Cfg_Pub.Estado EQ 1 AND Cfg_Pub.Id_Alimentacion EQ 2
                    BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
        END.
    END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME WBusca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WBusca wWin
ON LEAVE OF WBusca IN FRAME F_Consulta
DO:
  ASSIGN FRAME F_Consulta RBusca.
  IF RBusca EQ 1 THEN
     OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_PUB.Ente BEGINS SELF:SCREEN-VALUE AND Cfg_Pub.Id_Alimentacion EQ 2
          BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.
  ELSE
     OPEN QUERY B_PUB FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 AND Cfg_PUB.Codigo EQ INTEGER(SELF:SCREEN-VALUE) AND Cfg_Pub.Id_Alimentacion EQ 2
          BY Cfg_PUB.Ente BY Cfg_PUB.Codigo INDEXED-REPOSITION.

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
  DISPLAY NomUsuario Nom_CodPUB Nom_IndPUB 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Alm_Pub THEN 
    DISPLAY Alm_Pub.Ente Alm_Pub.Codigo Alm_Pub.Fecha Alm_Pub.Acumulado 
          Alm_Pub.Usuario 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-49 BUTTON-2 B_CrSalvar Btn_Ingresar BtnDone Alm_Pub.Acumulado 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY RSel RBusca WBusca 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-302 B_PUB RSel RBusca WBusca 
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
FIND FIRST Alm_PUB NO-LOCK NO-ERROR.
IF AVAILABLE Alm_PUB THEN DO:
   RUN Mostrar_Registro.
   DISABLE Alm_PUB.Codigo WITH FRAME FMain.
   APPLY "entry" TO Alm_PUB.Ente IN FRAME FMain.
END.
ELSE DO:
  DISABLE {&List-1} WITH FRAME FMain.
  DISABLE {&List-2} WITH FRAME FMetas.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
DEFINE VAR Ctg AS CHARACTER FORMAT "X(12)".
  ASSIGN Alm_Pub.Ente:SCREEN-VALUE IN FRAME FMain = Cfg_Pub.Ente
         Alm_Pub.Codigo:SCREEN-VALUE              = STRING(Cfg_Pub.Codigo).
  DISPLAY Alm_PUB.Fecha Alm_PUB.Usuario Alm_Pub.Acumulado WITH FRAME FMain.
  FIND Ind_PUB WHERE
       Ind_PUB.Codigo EQ INTEGER(Alm_PUB.Codigo:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Ind_PUB THEN DO:
     ASSIGN Nom_IndPUB:SCREEN-VALUE = Ind_PUB.Nombre.
  END.
  FIND PUB WHERE
       PUB.Ente EQ Alm_PUB.Ente:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE PUB THEN DO:
     CASE LENGTH(Alm_PUB.Ente):
         WHEN 2 THEN Ctg = "Perspectiva:".
         WHEN 4 THEN Ctg = "Subgerencia:".
         WHEN 6 THEN Ctg = "Agencia    :".
         WHEN 8 THEN Ctg = "Usuario    :".
     END CASE.
     Nom_CodPUB:SCREEN-VALUE = Ctg + " " + PUB.Nombre.
  END.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN NomUsuario:SCREEN-VALUE = Usuarios.Nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
 W_Reporte   = "REPORTE   : CONFIGURACION BALANCE - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Cod Nombre Indicador                                   Ente     Nombre Ente".
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 FOR EACH Alm_PUB NO-LOCK BREAK BY Alm_pub.Codigo BY Alm_pub.Ente:
   FIND PUB WHERE PUB.Ente EQ Alm_PUB.Ente NO-LOCK NO-ERROR.
   IF AVAILABLE PUB THEN
      WNomEntFila = PUB.Nombre.
   ELSE
      WNomEntFila = "No Existe en el PUB".
   FIND Ind_PUB WHERE Ind_PUB.Codigo EQ Alm_PUB.Codigo NO-LOCK NO-ERROR.
   IF AVAILABLE Ind_PUB THEN
      WNomCodFila = Ind_PUB.Nombre.
   ELSE
      WNomCodFila = "No Existe en  Indicadores PUB".
   DISPLAY Alm_Pub.Codigo
           WNomCodFila
           Alm_Pub.Ente
           WNomEntFila
       WITH FRAME Fmov WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
 END.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

