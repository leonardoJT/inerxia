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
{Incluido/Variable.I "SHARED"}
DEFINE VAR W_ok AS LOGICAL.
DEFINE VAR i    AS INTEGER.

DEFINE VARIABLE P_Cod   LIKE Empresas.Cod_Empresa.
DEFINE VARIABLE P_AgeEmp LIKE Agencias.Agencia.

DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
DEFINE VARIABLE P_CliAge   LIKE Clientes.Agencia.

DEFINE VARIABLE W_Puntero AS ROWID.
DEFINE VARIABLE W_Crear   AS LOGICAL.

  /*para buscar cuentas destino y cuentas debito automatico*/
  DEFINE VAR W_Age  LIKE Ahorros.Agencia.
  DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEFINE VAR W_Nit  LIKE Ahorros.Nit.
  DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Plastico
&Scoped-define BROWSE-NAME BPlastico

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Plastico

/* Definitions for BROWSE BPlastico                                     */
&Scoped-define FIELDS-IN-QUERY-BPlastico Plastico.Agencia Plastico.Num_Plastico Plastico.Nit Plastico.Cod_Ahorro Plastico.Cue_Ahorros   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BPlastico   
&Scoped-define SELF-NAME BPlastico
&Scoped-define QUERY-STRING-BPlastico FOR EACH Plastico
&Scoped-define OPEN-QUERY-BPlastico OPEN QUERY {&SELF-NAME} FOR EACH Plastico.
&Scoped-define TABLES-IN-QUERY-BPlastico Plastico
&Scoped-define FIRST-TABLE-IN-QUERY-BPlastico Plastico


/* Definitions for FRAME FConsulta                                      */

/* Definitions for FRAME F_Plastico                                     */
&Scoped-define FIELDS-IN-QUERY-F_Plastico Plastico.Num_Plastico ~
Plastico.Nit Plastico.Cod_ahorro Plastico.Cue_Ahorros 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Plastico Plastico.Num_Plastico 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Plastico Plastico
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Plastico Plastico
&Scoped-define QUERY-STRING-F_Plastico FOR EACH Plastico SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Plastico OPEN QUERY F_Plastico FOR EACH Plastico SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Plastico Plastico
&Scoped-define FIRST-TABLE-IN-QUERY-F_Plastico Plastico


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Plastico.Num_Plastico 
&Scoped-define ENABLED-TABLES Plastico
&Scoped-define FIRST-ENABLED-TABLE Plastico
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-229 Btn_Informacion ~
BUTTON-3 BUTTON-4 Cmb_Agencia Btn_Salvar Btn_Deshacer BUTTON-123 BUTTON-124 ~
Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir BUTTON-11 
&Scoped-Define DISPLAYED-FIELDS Plastico.Num_Plastico Plastico.Nit ~
Plastico.Cod_ahorro Plastico.Cue_Ahorros 
&Scoped-define DISPLAYED-TABLES Plastico
&Scoped-define FIRST-DISPLAYED-TABLE Plastico
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencia WNomNit WNomPro 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Informacion BUTTON-3 BUTTON-4 Btn_Salvar ~
Btn_Deshacer Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir BUTTON-11 
&Scoped-define List-3 Cmb_Agencia 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-55 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 55" 
     SIZE 11 BY 1.88.

DEFINE VARIABLE Cmb_Agencia2 AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las agencias" 
     DROP-DOWN-LIST
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RAsignadas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todas", 1,
"Asignadas", 2,
"No Asignadas", 3
     SIZE 39 BY .81 NO-UNDO.

DEFINE BUTTON Btn_Borrar 
     LABEL "Borrar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Informacion 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.62
     FGCOLOR 0 .

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-123 
     LABEL "Buscar Cuenta de Ahorros" 
     SIZE 22 BY .81.

DEFINE BUTTON BUTTON-124 
     LABEL "Reinicializar Valores" 
     SIZE 25 BY .81.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 3" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 4" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 64 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomNit AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomPro AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82 BY 4.31.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 10.23.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BPlastico FOR 
      Plastico SCROLLING.

DEFINE QUERY F_Plastico FOR 
      Plastico SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BPlastico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BPlastico wWin _FREEFORM
  QUERY BPlastico DISPLAY
      Plastico.Agencia COLUMN-LABEL "Agencia"
 Plastico.Num_Plastico FORMAT ">>>>>>>>>>>>>>>>9"
 Plastico.Nit
 Plastico.Cod_Ahorro
 Plastico.Cue_Ahorros
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 3.23
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Plastico
     Btn_Informacion AT ROW 1.81 COL 100
     BUTTON-3 AT ROW 3.42 COL 100
     BUTTON-4 AT ROW 5.04 COL 100
     Cmb_Agencia AT ROW 6.38 COL 24 COLON-ALIGNED
     Plastico.Num_Plastico AT ROW 7.46 COL 24 COLON-ALIGNED FORMAT ">>>>>>>>>>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 64 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 8.81 COL 100
     Plastico.Nit AT ROW 9.62 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     WNomNit AT ROW 9.62 COL 41 COLON-ALIGNED NO-LABEL
     Btn_Deshacer AT ROW 10.42 COL 100
     Plastico.Cod_ahorro AT ROW 10.69 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     WNomPro AT ROW 10.69 COL 41 COLON-ALIGNED NO-LABEL
     Plastico.Cue_Ahorros AT ROW 11.77 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     BUTTON-123 AT ROW 11.77 COL 43
     BUTTON-124 AT ROW 11.77 COL 66
     Btn_Ingresar AT ROW 12.04 COL 100
     Btn_Borrar AT ROW 13.65 COL 100
     Btn_Cancelar AT ROW 15.27 COL 100
     Btn_Salir AT ROW 16.88 COL 100
     BUTTON-11 AT ROW 19.04 COL 104
     RECT-2 AT ROW 1.54 COL 99
     RECT-3 AT ROW 8.54 COL 99
     RECT-229 AT ROW 8.81 COL 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.12
         BGCOLOR 17 FONT 4.

DEFINE FRAME FConsulta
     BPlastico AT ROW 1.54 COL 3
     Cmb_Agencia2 AT ROW 5.04 COL 9 COLON-ALIGNED
     BUTTON-55 AT ROW 5.04 COL 61
     RAsignadas AT ROW 6.12 COL 11 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 15 ROW 13.65
         SIZE 73 BY 7.27
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Plastico".


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
         TITLE              = "Mantenimiento de Plastico"
         HEIGHT             = 21.12
         WIDTH              = 113.86
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
ASSIGN FRAME FConsulta:FRAME = FRAME F_Plastico:HANDLE.

/* SETTINGS FOR FRAME FConsulta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BPlastico 1 FConsulta */
ASSIGN 
       FRAME FConsulta:HIDDEN           = TRUE
       FRAME FConsulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Plastico
                                                                        */
/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Informacion IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salir IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-11 IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-3 IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME F_Plastico
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F_Plastico
   3                                                                    */
/* SETTINGS FOR FILL-IN Plastico.Cod_ahorro IN FRAME F_Plastico
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Plastico.Cue_Ahorros IN FRAME F_Plastico
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Plastico.Nit IN FRAME F_Plastico
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Plastico.Num_Plastico IN FRAME F_Plastico
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN WNomNit IN FRAME F_Plastico
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomPro IN FRAME F_Plastico
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BPlastico
/* Query rebuild information for BROWSE BPlastico
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Plastico.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BPlastico */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FConsulta
/* Query rebuild information for FRAME FConsulta
     _Query            is NOT OPENED
*/  /* FRAME FConsulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Plastico
/* Query rebuild information for FRAME F_Plastico
     _TblList          = "bdcentral.Plastico"
     _Query            is NOT OPENED
*/  /* FRAME F_Plastico */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Mantenimiento de Plastico */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Mantenimiento de Plastico */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BPlastico
&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME BPlastico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BPlastico wWin
ON MOUSE-SELECT-CLICK OF BPlastico IN FRAME FConsulta
DO:
  RUN Mostrar_Plastico.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Plastico
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Plastico /* Cancelar */
DO:
  W_Crear = NO.
  DISABLE {&List-3} WITH FRAME F_Plastico.
  ENABLE Btn_Salvar Btn_Salir Btn_Ingresar Btn_Borrar WITH FRAME F_Plastico.
  RUN Inicializar_Pantalla.
  IF W_Puntero NE ? THEN DO:
     FIND Plastico WHERE ROWID(Plastico) EQ W_Puntero NO-ERROR.
     IF AVAILABLE Plastico THEN DO:
        RUN Mostrar_Plastico.
     END.
  END.

  Button-4:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Plastico /* Deshacer */
DO:
  W_Crear = NO.
  IF AVAILABLE Plastico THEN
     RUN Mostrar_Plastico.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Plastico /* Ingresar */
DO:
DO WITH FRAME F_Plastico:
  IF AVAILABLE Plastico THEN
     W_Puntero = ROWID(Plastico).
     
  ENABLE {&List-2}.
  DISABLE Btn_Ingresar Btn_Borrar Btn_Salir.
  /*valida usuarios.all_agencias para habilitar cmb_agencia*/
  W_Crear = YES.

  RUN Inicializar_Pantalla.
  Plastico.Num_Plastico:SCREEN-VALUE IN FRAME F_Plastico = "".

  RELEASE Plastico.

  Button-4:SENSITIVE = FALSE.

  APPLY "entry" TO Plastico.Nit.
  RETURN NO-APPLY.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Plastico /* Salir */
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
ON CHOOSE OF Btn_Salvar IN FRAME F_Plastico /* Salvar */
DO:
  IF DECIMAL(Plastico.Num_Plastico:SCREEN-VALUE) LE 0 THEN DO:
     MESSAGE "Falta Codigo del Plastico ... No se permite la operaciòn." VIEW-AS ALERT-BOX.
     RETURN.
  END.
    
  IF W_Crear THEN DO:
     CREATE Plastico.
     ASSIGN Plastico.Agencia      = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
            Plastico.Num_Plastico = DECIMAL(Plastico.Num_Plastico:SCREEN-VALUE).
  END.
  ELSE FIND CURRENT Plastico NO-ERROR.

  RUN Grabar.  

  ASSIGN W_Crear            = NO
         Button-4:SENSITIVE = TRUE.

  ENABLE Btn_Ingresar WITH FRAME F_Plastico.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-123
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-123 wWin
ON CHOOSE OF BUTTON-123 IN FRAME F_Plastico /* Buscar Cuenta de Ahorros */
DO:
  RUN C-Ahorros.r (INPUT "", OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
  ASSIGN Plastico.Nit:SCREEN-VALUE = W_Nit
         Plastico.Cod_Ahorro:SCREEN-VALUE = STRING(W_Pro)
         Plastico.Cue_Ahorros:SCREEN-VALUE = W_Cue.
  FIND Clientes WHERE Clientes.Nit EQ W_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN
     WNomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_Pro NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Ahorros THEN
     WNomPro:SCREEN-VALUE = Pro_Ahorros.Nom_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-124
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-124 wWin
ON CHOOSE OF BUTTON-124 IN FRAME F_Plastico /* Reinicializar Valores */
DO:
  ASSIGN Plastico.Nit:SCREEN-VALUE = ""
         WNomNit:SCREEN-VALUE      = ""
         Plastico.Cod_Ahorro:SCREEN-VALUE = "0"
         Plastico.Cue_Ahorros:SCREEN-VALUE = ""
         WNomPro:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME F_Plastico /* Button 3 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Plastico.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME F_Plastico /* Button 4 */
DO:
  OPEN QUERY BPlastico FOR EACH Plastico.
  VIEW FRAME FConsulta.
/*  WWin:SENSITIVE = FALSE.

  
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP ( ).*/
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME BUTTON-55
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-55 wWin
ON CHOOSE OF BUTTON-55 IN FRAME FConsulta /* Button 55 */
DO:
  HIDE FRAME FConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Plastico
&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia wWin
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F_Plastico /* Agencia */
DO:
  ASSIGN FRAME F_Plastico Cmb_Agencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME Cmb_Agencia2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia2 wWin
ON VALUE-CHANGED OF Cmb_Agencia2 IN FRAME FConsulta /* Agencia */
DO:
  DEFINE VAR AgeIni LIKE Agencias.Agencia.
  DEFINE VAR AgeFin LIKE Agencias.Agencia.
  ASSIGN FRAME FConsulta Cmb_Agencia2 RAsignadas.
  IF INTEGER(SUBSTRING(Cmb_Agencia2,1,3)) EQ 0 THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE 
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencia2,1,3)) AgeFin = AgeIni.


  CASE RAsignadas:
      WHEN 1 THEN
          OPEN QUERY BPlastico FOR EACH Plastico 
               WHERE Plastico.Agencia GE AgeIni AND
                     Plastico.Agencia LE AgeFin.
      WHEN 2 THEN
          OPEN QUERY BPlastico FOR EACH Plastico 
               WHERE Plastico.Agencia GE AgeIni AND
                     Plastico.Agencia LE AgeFin AND
                     Plastico.Nit NE "".
      WHEN 3 THEN
          OPEN QUERY BPlastico FOR EACH Plastico 
               WHERE Plastico.Agencia GE AgeIni AND
                     Plastico.Agencia LE AgeFin AND
                     Plastico.Nit EQ "".
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAsignadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAsignadas wWin
ON VALUE-CHANGED OF RAsignadas IN FRAME FConsulta
DO:
  APPLY "value-changed" TO Cmb_Agencia2 IN FRAME Fconsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Plastico
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
  DISPLAY Cmb_Agencia WNomNit WNomPro 
      WITH FRAME F_Plastico IN WINDOW wWin.
  IF AVAILABLE Plastico THEN 
    DISPLAY Plastico.Num_Plastico Plastico.Nit Plastico.Cod_ahorro 
          Plastico.Cue_Ahorros 
      WITH FRAME F_Plastico IN WINDOW wWin.
  ENABLE RECT-2 RECT-3 RECT-229 Btn_Informacion BUTTON-3 BUTTON-4 Cmb_Agencia 
         Plastico.Num_Plastico Btn_Salvar Btn_Deshacer BUTTON-123 BUTTON-124 
         Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir BUTTON-11 
      WITH FRAME F_Plastico IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Plastico}
  DISPLAY Cmb_Agencia2 RAsignadas 
      WITH FRAME FConsulta IN WINDOW wWin.
  ENABLE BPlastico Cmb_Agencia2 BUTTON-55 RAsignadas 
      WITH FRAME FConsulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConsulta}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar wWin 
PROCEDURE Grabar :
DO WITH FRAME F_Plastico:
   ASSIGN Plastico.Agencia              = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
          Plastico.Nit                  
          Plastico.Cue_Ahorros             
          Plastico.Cod_Ahorro NO-ERROR.          
   Plastico.Num_Plastico = DECIMAL(Plastico.Num_Plastico:SCREEN-VALUE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Pantalla wWin 
PROCEDURE Inicializar_Pantalla :
DO WITH FRAME F_Plastico:
   FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
      Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
   ASSIGN Plastico.Num_Plastico:SCREEN-VALUE  = "0"
          Plastico.Nit:SCREEN-VALUE           = ""
          Plastico.Cue_Ahorros:SCREEN-VALUE   = ""
          Plastico.Cod_Ahorro:SCREEN-VALUE    = "0".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_Plastico:
  RUN SUPER.
  FOR EACH Agencias NO-LOCK:
    W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
    W_Ok = Cmb_Agencia2:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME FConsulta.
  END.
  FIND FIRST Plastico WHERE Plastico.Agencia EQ W_Agencia NO-ERROR.
  IF AVAILABLE Plastico THEN
     RUN Mostrar_Plastico.
  ELSE DO:
    FIND FIRST Plastico NO-ERROR.
    IF AVAILABLE Plastico THEN
      RUN Mostrar_Plastico.
    ELSE DO:
      DISABLE ALL WITH FRAME F_Plastico.
      ENABLE {&List-1} WITH FRAME F_Plastico.
      RUN Inicializar_Pantalla.
    END.
  END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Plastico wWin 
PROCEDURE Mostrar_Plastico :
DO WITH FRAME F_Plastico:
   FIND Agencias WHERE Agencias.Agencia EQ Plastico.Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
      Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

   ASSIGN Plastico.Num_Plastico:SCREEN-VALUE   = STRING(Plastico.Num_Plastico)
          Plastico.Nit:SCREEN-VALUE            = Plastico.Nit
          Plastico.Cod_Ahorro:SCREEN-VALUE     = STRING(Plastico.Cod_Ahorro)
          Plastico.Cue_Ahorros:SCREEN-VALUE    = STRING(Plastico.Cue_Ahorros).
    
    FIND Clientes WHERE Clientes.Nit EQ Plastico.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
       ASSIGN WNomNit:SCREEN-VALUE             = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE
       ASSIGN WNomNit:SCREEN-VALUE             = "No Asignado".
    FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Plastico.Cod_Ahorro NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Ahorros THEN
       WNomPro:SCREEN-VALUE = CAPS(Pro_Ahorros.Nom_Producto).
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
{INCLUIDO\RepEncabezado.I}    
  W_Reporte    = "REPORTE   : Plastico DEL SISTEMA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " Cod  Age  Nit            Descripcion                      Period     Nit Pagad   Nit Rep.Legal".
  DEFINE VAR Des AS CHARACTER FORMAT "X(10)".
    
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Plastico:
     DISPLAY Plastico.Agencia         
             Plastico.Num_Plastico    
             Plastico.Nit            
             Plastico.Cod_Ahorro
             Plastico.Cue_Ahorro
     WITH FRAME F-Detalle WIDTH 132 NO-BOX.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

