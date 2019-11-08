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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VAR W_Cr AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Con
&Scoped-define BROWSE-NAME B_Con

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Documentos

/* Definitions for BROWSE B_Con                                         */
&Scoped-define FIELDS-IN-QUERY-B_Con Documentos.Nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Con   
&Scoped-define SELF-NAME B_Con
&Scoped-define QUERY-STRING-B_Con FOR EACH Documentos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Con OPEN QUERY {&SELF-NAME} FOR EACH Documentos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Con Documentos
&Scoped-define FIRST-TABLE-IN-QUERY-B_Con Documentos


/* Definitions for FRAME F_Con                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Con ~
    ~{&OPEN-QUERY-B_Con}

/* Definitions for FRAME F_Doc                                          */
&Scoped-define FIELDS-IN-QUERY-F_Doc Documentos.Nombre ~
Documentos.Encabezado Documentos.Cuerpo Documentos.Pie_Pagina ~
Documentos.Id_Contabiliza Documentos.Cod_Operacion Documentos.Id_Creditos ~
Documentos.Id_GarAdmisible Documentos.Id_Cliente Documentos.Id_Ahorros ~
Documentos.Id_Atrasos Documentos.Id_GarPersonal Documentos.Id_EnviaCodeudor 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Doc Documentos.Nombre ~
Documentos.Encabezado Documentos.Cuerpo Documentos.Pie_Pagina ~
Documentos.Id_Contabiliza Documentos.Cod_Operacion Documentos.Id_Creditos ~
Documentos.Id_GarAdmisible Documentos.Id_Cliente Documentos.Id_Ahorros ~
Documentos.Id_Atrasos Documentos.Id_GarPersonal Documentos.Id_EnviaCodeudor 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Doc Documentos
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Doc Documentos
&Scoped-define QUERY-STRING-F_Doc FOR EACH Documentos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Doc OPEN QUERY F_Doc FOR EACH Documentos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Doc Documentos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Doc Documentos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B_Con BUTTON-61 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-61 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 61" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BtnDone-2 DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 13 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-60 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 60" 
     SIZE 4 BY 1.12.

DEFINE BUTTON B_CRBorrar-2 
     LABEL "Inactivar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON B_CRCancelar-2 
     LABEL "Cancelar" 
     SIZE 13 BY 1.65.

DEFINE BUTTON B_CRIngresar-2 
     LABEL "Ingresar" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE W_NomOperacion AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 59 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 4.31.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 5.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 10.23.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Con FOR 
      Documentos SCROLLING.

DEFINE QUERY F_Doc FOR 
      Documentos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Con
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Con wWin _FREEFORM
  QUERY B_Con NO-LOCK DISPLAY
      Documentos.Nombre FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 8.88
         BGCOLOR 15 FONT 5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Doc
     Documentos.Nombre AT ROW 1.27 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 63 BY 1
          BGCOLOR 15 
     BUTTON-1 AT ROW 1.54 COL 99
     BUTTON-2 AT ROW 3.15 COL 99
     Documentos.Encabezado AT ROW 3.42 COL 5 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 87 BY 2.96
          BGCOLOR 15 FONT 4
     BUTTON-3 AT ROW 4.77 COL 99
     Documentos.Cuerpo AT ROW 7.19 COL 5 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 87 BY 4.04
          BGCOLOR 15 FONT 4
     Btn_Salvar AT ROW 10.42 COL 99
     Documentos.Pie_Pagina AT ROW 12.04 COL 5 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 87 BY 2.42
          BGCOLOR 15 FONT 4
     Btn_Deshacer AT ROW 12.04 COL 99
     B_CRIngresar-2 AT ROW 13.65 COL 99
     Documentos.Id_Contabiliza AT ROW 14.73 COL 5
          LABEL "El Certificado tiene un Costo para el Cliente?"
          VIEW-AS TOGGLE-BOX
          SIZE 43 BY .77
     B_CRCancelar-2 AT ROW 15.27 COL 99
     Documentos.Cod_Operacion AT ROW 15.81 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.43 BY .81
          BGCOLOR 15 
     W_NomOperacion AT ROW 15.81 COL 31 COLON-ALIGNED NO-LABEL
     B_CRBorrar-2 AT ROW 16.88 COL 99
     Documentos.Id_Creditos AT ROW 18.23 COL 6
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .77
     Documentos.Id_GarAdmisible AT ROW 18.23 COL 34
          VIEW-AS TOGGLE-BOX
          SIZE 22.86 BY .77
     Documentos.Id_Cliente AT ROW 18.23 COL 62
          VIEW-AS TOGGLE-BOX
          SIZE 23.86 BY .77
     BtnDone-2 AT ROW 18.5 COL 99
     Documentos.Id_Ahorros AT ROW 19.04 COL 62
          VIEW-AS TOGGLE-BOX
          SIZE 21.57 BY .77
     Documentos.Id_Atrasos AT ROW 19.31 COL 6
          VIEW-AS TOGGLE-BOX
          SIZE 21.43 BY .77
     Documentos.Id_GarPersonal AT ROW 19.31 COL 34
          VIEW-AS TOGGLE-BOX
          SIZE 23.14 BY .77
     Documentos.Id_EnviaCodeudor AT ROW 20.38 COL 6
          LABEL "Enviar copia a Codeudores?"
          VIEW-AS TOGGLE-BOX
          SIZE 28.57 BY .77
     BUTTON-60 AT ROW 20.38 COL 104
     RECT-1 AT ROW 17.42 COL 4
     RECT-2 AT ROW 1.27 COL 98
     RECT-3 AT ROW 10.15 COL 98
     "Cuerpo del Documento" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 6.38 COL 5
          FGCOLOR 7 
     "Encabezado del Documento" VIEW-AS TEXT
          SIZE 26 BY .88 AT ROW 2.46 COL 5
          FGCOLOR 7 
     "Pie de Pagina del Documento" VIEW-AS TEXT
          SIZE 27 BY .81 AT ROW 11.23 COL 5
          FGCOLOR 7 
     "Escoja el Tipo de Información de Contenido del Documento" VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 17.15 COL 5
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 21.31
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Con
     B_Con AT ROW 1.27 COL 4
     BUTTON-61 AT ROW 10.42 COL 40
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 48 ROW 3.69
         SIZE 49 BY 12.12
         BGCOLOR 17 
         TITLE "Consulta de Documentos".


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
         TITLE              = "SFG - Configuración de Documentos del Sistema"
         HEIGHT             = 21.31
         WIDTH              = 114
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
ASSIGN FRAME F_Con:FRAME = FRAME F_Doc:HANDLE.

/* SETTINGS FOR FRAME F_Con
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Con 1 F_Con */
ASSIGN 
       FRAME F_Con:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Doc
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME F_Doc
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Documentos.Id_Contabiliza IN FRAME F_Doc
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Documentos.Id_EnviaCodeudor IN FRAME F_Doc
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomOperacion IN FRAME F_Doc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Con
/* Query rebuild information for BROWSE B_Con
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Documentos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Con */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Doc
/* Query rebuild information for FRAME F_Doc
     _TblList          = "bdCentral.Documentos"
     _Query            is OPENED
*/  /* FRAME F_Doc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Configuración de Documentos del Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Configuración de Documentos del Sistema */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Doc
&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME F_Doc /* Salir */
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


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Doc /* Deshacer */
DO:
DO WITH FRAME F_Doc:
  IF AVAILABLE Documentos THEN DO:
    ASSIGN Documentos.Cuerpo:SCREEN-VALUE       = Documentos.Cuerpo       
           Documentos.Encabezado:SCREEN-VALUE   = Documentos.Encabezado
           Documentos.Id_Ahorros:SCREEN-VALUE   = STRING(Documentos.Id_Ahorros)
           Documentos.Id_Atrasos:SCREEN-VALUE   = STRING(Documentos.Id_Atrasos)
           Documentos.Id_Cliente:SCREEN-VALUE   = STRING(Documentos.Id_Cliente)
           Documentos.Id_Creditos:SCREEN-VALUE  = STRING(Documentos.Id_Creditos)
           Documentos.Id_GarAdmisible:SCREEN-VALUE = STRING(Documentos.Id_GarAdmisible)
           Documentos.Id_GarPersonal:SCREEN-VALUE  = STRING(Documentos.Id_GarPersonal)
           Documentos.Nombre:SCREEN-VALUE       = Documentos.Nombre
           Documentos.Pie_Pagina:SCREEN-VALUE   = Documentos.Pie_Pagina
           Documentos.Id_Contabiliza:SCREEN-VALUE = STRING(Documentos.Id_Contabiliza)
           Documentos.Cod_Operacion:SCREEN-VALUE  = STRING(Documentos.Cod_Operacion)
           Documentos.Id_EnviaCodeudor:SCREEN-VALUE = STRING(Documentos.Id_EnviaCodeudor).
    IF NOT Documentos.Id_Contabiliza THEN DO: 
       DISABLE Documentos.Cod_Operacion WITH FRAME F_Doc.
       W_NomOperacion:SCREEN-VALUE = "".
    END.
    ELSE DO:
       ENABLE Documentos.Cod_Operacion WITH FRAME F_Doc.
       FIND Operacion WHERE Operacion.Cod_Operacion EQ Documentos.Cod_Operacion NO-LOCK NO-ERROR.
       IF AVAILABLE Operacion THEN W_NomOperacion:SCREEN-VALUE = Operacion.Nom_Operacion.
    END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Doc /* Salvar */
DO: 
DO WITH FRAME F_Doc:
  FIND Documentos WHERE Documentos.Nombre EQ Documentos.Nombre:SCREEN-VALUE NO-ERROR.
  IF NOT AVAILABLE Documentos THEN CREATE Documentos.
  RUN Salvar.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME F_Doc /* Button 3 */
DO:
  OPEN QUERY B_Con FOR EACH Documentos NO-LOCK INDEXED-REPOSITION.
  VIEW FRAME F_Con.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Con
&Scoped-define SELF-NAME BUTTON-61
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-61 wWin
ON CHOOSE OF BUTTON-61 IN FRAME F_Con /* Button 61 */
DO:
  HIDE FRAME F_Con.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Con
&Scoped-define SELF-NAME B_Con
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Con wWin
ON MOUSE-SELECT-DBLCLICK OF B_Con IN FRAME F_Con
DO:
  APPLY "choose" TO Btn_Deshacer IN FRAME F_Doc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Doc
&Scoped-define SELF-NAME B_CRCancelar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRCancelar-2 wWin
ON CHOOSE OF B_CRCancelar-2 IN FRAME F_Doc /* Cancelar */
DO:
DO WITH FRAME F_Doc:
  FIND LAST Documentos NO-ERROR.
  IF AVAILABLE Documentos THEN
    ASSIGN Documentos.Cuerpo:SCREEN-VALUE       = Documentos.Cuerpo       
           Documentos.Encabezado:SCREEN-VALUE   = Documentos.Encabezado
           Documentos.Id_Ahorros:SCREEN-VALUE   = STRING(Documentos.Id_Ahorros)
           Documentos.Id_Atrasos:SCREEN-VALUE   = STRING(Documentos.Id_Atrasos)
           Documentos.Id_Cliente:SCREEN-VALUE   = STRING(Documentos.Id_Cliente)
           Documentos.Id_Creditos:SCREEN-VALUE  = STRING(Documentos.Id_Creditos)
           Documentos.Id_GarAdmisible:SCREEN-VALUE = STRING(Documentos.Id_GarAdmisible)
           Documentos.Id_GarPersonal:SCREEN-VALUE  = STRING(Documentos.Id_GarPersonal)
           Documentos.Nombre:SCREEN-VALUE       = Documentos.Nombre
           Documentos.Pie_Pagina:SCREEN-VALUE   = Documentos.Pie_Pagina.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_CRIngresar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_CRIngresar-2 wWin
ON CHOOSE OF B_CRIngresar-2 IN FRAME F_Doc /* Ingresar */
DO:
DO WITH FRAME F_Doc:
    ASSIGN Documentos.Cuerpo:SCREEN-VALUE       = ""
           Documentos.Encabezado:SCREEN-VALUE   = ""
           Documentos.Id_Ahorros:SCREEN-VALUE   = "no"
           Documentos.Id_Atrasos:SCREEN-VALUE   = "no"
           Documentos.Id_Cliente:SCREEN-VALUE   = "no"
           Documentos.Id_Creditos:SCREEN-VALUE  = "no"
           Documentos.Id_GarAdmisible:SCREEN-VALUE = "no"
           Documentos.Id_GarPersonal:SCREEN-VALUE  = "no"
           Documentos.Nombre:SCREEN-VALUE       = ""
           Documentos.Pie_Pagina:SCREEN-VALUE   = "".
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Documentos.Cod_Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Cod_Operacion wWin
ON LEAVE OF Documentos.Cod_Operacion IN FRAME F_Doc /* Código Operación */
DO:
  DEFINE VAR P_Codigo LIKE Operacion.Cod_Operacion.
  DEFINE VAR P_Nombre LIKE Operacion.Nom_Operacion.
  IF Documentos.Id_Contabiliza:SCREEN-VALUE EQ "yes" AND (DECIMAL(SELF:SCREEN-VALUE) EQ 0 OR
     Documentos.Cod_Operacion:SCREEN-VALUE EQ "?") THEN DO:
     RUN C-Operaciones.r (OUTPUT P_Codigo, OUTPUT P_Nombre).
     ASSIGN SELF:SCREEN-VALUE = STRING(P_Codigo)
            W_NomOperacion:SCREEN-VALUE = P_Nombre.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Documentos.Cuerpo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Cuerpo wWin
ON ENTRY OF Documentos.Cuerpo IN FRAME F_Doc /* Cuerpo */
DO:
  ON return return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Cuerpo wWin
ON LEAVE OF Documentos.Cuerpo IN FRAME F_Doc /* Cuerpo */
DO:
  ON return tab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Documentos.Encabezado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Encabezado wWin
ON ENTRY OF Documentos.Encabezado IN FRAME F_Doc /* Encabezado */
DO:
  ON return return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Encabezado wWin
ON LEAVE OF Documentos.Encabezado IN FRAME F_Doc /* Encabezado */
DO:
  ON return tab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Documentos.Id_Contabiliza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Id_Contabiliza wWin
ON VALUE-CHANGED OF Documentos.Id_Contabiliza IN FRAME F_Doc /* El Certificado tiene un Costo para el Cliente? */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
     ENABLE Documentos.Cod_Operacion WITH FRAME F_Doc.
     APPLY "entry" TO Documentos.Cod_Operacion.
  END.
  ELSE DO:
     Documentos.Cod_Operacion:SCREEN-VALUE = "0".
     DISABLE Documentos.Cod_Operacion WITH FRAME F_Doc.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Documentos.Pie_Pagina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Pie_Pagina wWin
ON ENTRY OF Documentos.Pie_Pagina IN FRAME F_Doc /* Pie_Pagina */
DO:
  ON return return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Documentos.Pie_Pagina wWin
ON LEAVE OF Documentos.Pie_Pagina IN FRAME F_Doc /* Pie_Pagina */
DO:
  ON return tab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Con
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

  {&OPEN-QUERY-F_Doc}
  GET FIRST F_Doc.
  DISPLAY W_NomOperacion 
      WITH FRAME F_Doc IN WINDOW wWin.
  IF AVAILABLE Documentos THEN 
    DISPLAY Documentos.Nombre Documentos.Encabezado Documentos.Cuerpo 
          Documentos.Pie_Pagina Documentos.Id_Contabiliza 
          Documentos.Cod_Operacion Documentos.Id_Creditos 
          Documentos.Id_GarAdmisible Documentos.Id_Cliente Documentos.Id_Ahorros 
          Documentos.Id_Atrasos Documentos.Id_GarPersonal 
          Documentos.Id_EnviaCodeudor 
      WITH FRAME F_Doc IN WINDOW wWin.
  ENABLE Documentos.Nombre BUTTON-1 Documentos.Encabezado BUTTON-3 
         Documentos.Cuerpo Btn_Salvar Documentos.Pie_Pagina Btn_Deshacer 
         B_CRIngresar-2 Documentos.Id_Contabiliza B_CRCancelar-2 
         Documentos.Cod_Operacion B_CRBorrar-2 Documentos.Id_Creditos 
         Documentos.Id_GarAdmisible Documentos.Id_Cliente BtnDone-2 
         Documentos.Id_Ahorros Documentos.Id_Atrasos Documentos.Id_GarPersonal 
         Documentos.Id_EnviaCodeudor BUTTON-60 RECT-1 RECT-2 RECT-3 
      WITH FRAME F_Doc IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Doc}
  ENABLE B_Con BUTTON-61 
      WITH FRAME F_Con IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Con}
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

  HIDE FRAME F_Con.
  FIND FIRST Documentos NO-ERROR.
  IF AVAILABLE Documentos THEN APPLY "choose" TO Btn_Deshacer IN FRAME F_Doc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salvar wWin 
PROCEDURE Salvar :
ASSIGN FRAME F_Doc
      Documentos.Cuerpo
      Documentos.Encabezado
      Documentos.Id_Ahorros
      Documentos.Id_Atrasos
      Documentos.Id_Cliente
      Documentos.Id_Creditos
      Documentos.Id_GarAdmisible
      Documentos.Id_GarPersonal
      Documentos.Nombre
      Documentos.Pie_Pagina
      Documentos.Id_Contabiliza
      Documentos.Cod_Operacion
      Documentos.Id_EnviaCodeudor.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

