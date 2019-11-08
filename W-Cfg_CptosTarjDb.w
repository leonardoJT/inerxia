&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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
DEFI VAR W_Ok       AS LOGICAL INITIAL FALSE.
DEFI VAR w-tipoOper AS INTEGER INITIAL 0.
DEFI VAR W-Clase    LIKE Cfg_CptosTarjDb.Clase.
DEFI VAR W-Tipo     LIKE Cfg_CptosTarjDB.TipoTrans.
DEFI VAR W-CodCre   LIKE Cfg_CptosTarjDB.Cod_Cre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cfg_CptosTarjDB.Gmf_AsumeCoop ~
Cfg_CptosTarjDB.Db_Monto Cfg_CptosTarjDB.Cr_Monto Cfg_CptosTarjDB.GMF ~
Cfg_CptosTarjDB.TopeGmf Cfg_CptosTarjDB.Db_Gmf Cfg_CptosTarjDB.Cr_Gmf ~
Cfg_CptosTarjDB.Db_GmfCBco Cfg_CptosTarjDB.Cr_GmfCBco ~
Cfg_CptosTarjDB.Db_IngCoop Cfg_CptosTarjDB.Cr_IngCoop ~
Cfg_CptosTarjDB.Cr_IngxCom Cfg_CptosTarjDB.Cr_IvaxCom ~
Cfg_CptosTarjDB.Cod_Operacion Cfg_CptosTarjDB.Cr_TrasAge 
&Scoped-define ENABLED-TABLES Cfg_CptosTarjDB
&Scoped-define FIRST-ENABLED-TABLE Cfg_CptosTarjDB
&Scoped-Define ENABLED-OBJECTS R-Tipo Cmb_Operaciones Btn-Cancelar-2 ~
Btn-Salvar-2 Btn_Done W-NomCta RECT-344 RECT-345 RECT-346 RECT-347 RECT-348 ~
RECT-350 
&Scoped-Define DISPLAYED-FIELDS Cfg_CptosTarjDB.Gmf_AsumeCoop ~
Cfg_CptosTarjDB.Db_Monto Cfg_CptosTarjDB.Cr_Monto Cfg_CptosTarjDB.GMF ~
Cfg_CptosTarjDB.TopeGmf Cfg_CptosTarjDB.Db_Gmf Cfg_CptosTarjDB.Cr_Gmf ~
Cfg_CptosTarjDB.Db_GmfCBco Cfg_CptosTarjDB.Cr_GmfCBco ~
Cfg_CptosTarjDB.Db_IngCoop Cfg_CptosTarjDB.Cr_IngCoop ~
Cfg_CptosTarjDB.Cr_IngxCom Cfg_CptosTarjDB.Cr_IvaxCom ~
Cfg_CptosTarjDB.Db_SegCartera Cfg_CptosTarjDB.Cr_SegCartera ~
Cfg_CptosTarjDB.Cod_Operacion Cfg_CptosTarjDB.Cr_TrasAge 
&Scoped-define DISPLAYED-TABLES Cfg_CptosTarjDB
&Scoped-define FIRST-DISPLAYED-TABLE Cfg_CptosTarjDB
&Scoped-Define DISPLAYED-OBJECTS R-Tipo Cmb_Operaciones W-NomCta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Cancelar-2 
     LABEL "Cancela" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn-Salvar-2 
     LABEL "Salvar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE W-NomCta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-Tipo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuenta de Ahorros", 1,
"Cupo Rotativo", 2
     SIZE 20 BY 1.62
     BGCOLOR 15 FGCOLOR 3  NO-UNDO.

DEFINE RECTANGLE RECT-344
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 34 BY 2.42
     BGCOLOR 3 FGCOLOR 15 .

DEFINE RECTANGLE RECT-345
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 34 BY 2.42
     BGCOLOR 3 FGCOLOR 8 .

DEFINE RECTANGLE RECT-346
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 35 BY 7.81
     BGCOLOR 3 FGCOLOR 15 .

DEFINE RECTANGLE RECT-347
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 71 BY 1.35
     BGCOLOR 3 FGCOLOR 15 .

DEFINE RECTANGLE RECT-348
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 72 BY 2.42
     BGCOLOR 3 FGCOLOR 15 .

DEFINE RECTANGLE RECT-350
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 14.54.

DEFINE VARIABLE Cmb_Operaciones AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 50.29 BY 1.81
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Cfg_CptosTarjDB.Gmf_AsumeCoop AT ROW 8.19 COL 33.86 WIDGET-ID 154
          VIEW-AS TOGGLE-BOX
          SIZE 2 BY .81 TOOLTIP "Marcar si desea que la entidad asuma el Gmf"
          BGCOLOR 3 
     R-Tipo AT ROW 1.35 COL 3.72 NO-LABEL WIDGET-ID 4
     Cmb_Operaciones AT ROW 1.27 COL 23.72 NO-LABEL WIDGET-ID 8
     Cfg_CptosTarjDB.Db_Monto AT ROW 3.69 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_Monto AT ROW 3.69 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.GMF AT ROW 6.15 COL 15.14 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.TopeGmf AT ROW 7.15 COL 15.14 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Db_Gmf AT ROW 9.15 COL 15.14 COLON-ALIGNED NO-LABEL WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_Gmf AT ROW 10.15 COL 15.14 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Db_GmfCBco AT ROW 11.31 COL 15.14 COLON-ALIGNED NO-LABEL WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_GmfCBco AT ROW 12.35 COL 15.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Db_IngCoop AT ROW 5.81 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_IngCoop AT ROW 6.73 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_IngxCom AT ROW 9.42 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_IvaxCom AT ROW 10.38 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Db_SegCartera AT ROW 14.15 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 142
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_SegCartera AT ROW 15.23 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 144
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cod_Operacion AT ROW 14.15 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Cfg_CptosTarjDB.Cr_TrasAge AT ROW 15.15 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 FGCOLOR 3 
     Btn-Cancelar-2 AT ROW 9 COL 76.57 WIDGET-ID 128
     Btn-Salvar-2 AT ROW 11 COL 76.57 WIDGET-ID 130
     Btn_Done AT ROW 13.04 COL 76.57 WIDGET-ID 134
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.72 BY 15.65
         BGCOLOR 17 FGCOLOR 0  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     W-NomCta AT ROW 11.42 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     "Cr Tras Age:" VIEW-AS TEXT
          SIZE 11.57 BY .81 AT ROW 15.15 COL 39 WIDGET-ID 94
          BGCOLOR 3 FGCOLOR 15 
     "Db GMF:" VIEW-AS TEXT
          SIZE 9 BY .69 AT ROW 9.19 COL 7.86 WIDGET-ID 56
          BGCOLOR 3 FGCOLOR 15 
     "Tope GMF:" VIEW-AS TEXT
          SIZE 10.57 BY .69 AT ROW 7.19 COL 6 WIDGET-ID 54
          BGCOLOR 3 FGCOLOR 15 
     "Cr Cuenta:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 3.69 COL 41 WIDGET-ID 50
          BGCOLOR 3 FGCOLOR 15 
     "% GMF:" VIEW-AS TEXT
          SIZE 8 BY .69 AT ROW 6.23 COL 9 WIDGET-ID 52
          BGCOLOR 3 FGCOLOR 15 
     "Db Cuenta:" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 3.69 COL 3.72 WIDGET-ID 48
          BGCOLOR 3 FGCOLOR 15 
     "  Manejo de GMF" VIEW-AS TEXT
          SIZE 17.29 BY .81 AT ROW 5.19 COL 11.72 WIDGET-ID 88
          BGCOLOR 3 FGCOLOR 15 
     " Ingresos Comisión Adicional" VIEW-AS TEXT
          SIZE 27.43 BY .62 AT ROW 5 COL 43.57 WIDGET-ID 70
          BGCOLOR 3 FGCOLOR 15 
     "Cr IVA:" VIEW-AS TEXT
          SIZE 6.43 BY .69 AT ROW 10.27 COL 42.57 WIDGET-ID 82
          BGCOLOR 3 FGCOLOR 15 
     "Cr:" VIEW-AS TEXT
          SIZE 3 BY .69 AT ROW 9.46 COL 46 WIDGET-ID 80
          BGCOLOR 3 FGCOLOR 15 
     " Ingresos Comisión Bruta" VIEW-AS TEXT
          SIZE 25 BY .81 AT ROW 8.5 COL 44.86 WIDGET-ID 76
          BGCOLOR 3 FGCOLOR 15 
     "Db:" VIEW-AS TEXT
          SIZE 4 BY .81 AT ROW 5.81 COL 46 WIDGET-ID 72
          BGCOLOR 3 FGCOLOR 15 
     "Cr:" VIEW-AS TEXT
          SIZE 3 BY .54 AT ROW 6.88 COL 46 WIDGET-ID 74
          BGCOLOR 3 FGCOLOR 15 
     "Cr GMF Bco:" VIEW-AS TEXT
          SIZE 12 BY .69 AT ROW 12.38 COL 4.57 WIDGET-ID 66
          BGCOLOR 3 FGCOLOR 15 
     "Db SegCart:" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 14.15 COL 5 WIDGET-ID 146
          BGCOLOR 3 FGCOLOR 15 
     "Gmf Asumida por la Entidad:" VIEW-AS TEXT
          SIZE 27 BY .81 AT ROW 8.15 COL 6.43 WIDGET-ID 152
          BGCOLOR 3 FGCOLOR 15 
     "Cr SegCart:" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 15.19 COL 5.43 WIDGET-ID 148
          BGCOLOR 3 FGCOLOR 15 
     "Db GMF Bco:" VIEW-AS TEXT
          SIZE 13 BY .69 AT ROW 11.31 COL 3.86 WIDGET-ID 64
          BGCOLOR 3 FGCOLOR 15 
     "Cod.Oper:" VIEW-AS TEXT
          SIZE 9.14 BY .81 AT ROW 14.27 COL 41 WIDGET-ID 62
          BGCOLOR 3 FGCOLOR 15 
     "Cr GMF:" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 10.31 COL 8.57 WIDGET-ID 58
          BGCOLOR 3 FGCOLOR 15 
     RECT-344 AT ROW 5.27 COL 41 WIDGET-ID 68
     RECT-345 AT ROW 8.92 COL 41 WIDGET-ID 84
     RECT-346 AT ROW 5.58 COL 3 WIDGET-ID 86
     RECT-347 AT ROW 3.42 COL 3 WIDGET-ID 90
     RECT-348 AT ROW 13.88 COL 3 WIDGET-ID 92
     RECT-350 AT ROW 2.08 COL 2 WIDGET-ID 140
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.72 BY 15.65
         BGCOLOR 17 FGCOLOR 0  WIDGET-ID 100.


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
         TITLE              = "Configuración de conceptos de Tarjeta Débito  W-Cfg_CptosTarjDb.W"
         HEIGHT             = 15.65
         WIDTH              = 84.57
         MAX-HEIGHT         = 19.27
         MAX-WIDTH          = 136.43
         VIRTUAL-HEIGHT     = 19.27
         VIRTUAL-WIDTH      = 136.43
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
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN Cfg_CptosTarjDB.Cr_SegCartera IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_CptosTarjDB.Db_SegCartera IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       W-NomCta:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración de conceptos de Tarjeta Débito  W-Cfg_CptosTarjDb.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración de conceptos de Tarjeta Débito  W-Cfg_CptosTarjDb.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancelar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancelar-2 C-Win
ON CHOOSE OF Btn-Cancelar-2 IN FRAME F-Main /* Cancela */
DO:
  RUN inicializar_vbles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salvar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salvar-2 C-Win
ON CHOOSE OF Btn-Salvar-2 IN FRAME F-Main /* Salvar */
DO:
  DO WITH FRAME F-Main:
    FIND CURRENT Cfg_CptosTarjDb  NO-ERROR.
    ASSIGN FRAME F-Main  Cfg_CptosTarjDB.Cod_Operacion 
                         Cfg_CptosTarjDB.Cr_Gmf 
                         Cfg_CptosTarjDB.Cr_GmfCBco
                         Cfg_CptosTarjDB.Cr_IngCoop 
                         Cfg_CptosTarjDB.Cr_IngxCom
                         Cfg_CptosTarjDB.Cr_IvaxCom 
                         Cfg_CptosTarjDB.Cr_Monto 
                         Cfg_CptosTarjDB.Cr_TrasAge 
                         Cfg_CptosTarjDB.Db_Gmf 
                         Cfg_CptosTarjDB.Db_GmfCBco
                         Cfg_CptosTarjDB.Db_IngCoop 
                         Cfg_CptosTarjDB.Db_Monto 
                         Cfg_CptosTarjDB.GMF 
                         Cfg_CptosTarjDB.TopeGmf
                         Cfg_CptosTarjDB.Db_SegCartera
                         Cfg_CptosTarjDB.Cr_SegCartera
                         Cfg_CptosTarjDB.Gmf_AsumeCoop.    
    RELEASE Cfg_CptosTarjDB.
    FIND FIRST Cfg_CptosTarjDB WHERE Cfg_CptosTarjDB.Clase     EQ W-Clase AND
                                     Cfg_CptosTarjDB.TipoTrans EQ W-Tipo  NO-LOCK NO-ERROR.
  END.
  RUN inicializar_vbles. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Operaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Operaciones C-Win
ON VALUE-CHANGED OF Cmb_Operaciones IN FRAME F-Main
DO:
    ASSIGN w-tipo = INTEGER(SUBSTRING(Cmb_Operaciones:SCREEN-VALUE,1,2)).

    IF r-tipo EQ 2 THEN
        W-CodCre = INTEGER(SUBSTRING(Cmb_Operaciones:SCREEN-VALUE,15,3)).
    ELSE
        W-CodCre = 0.

    RUN Inicializar_vbles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cod_Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cod_Operacion C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cod_Operacion IN FRAME F-Main /* CodOper */
DO:
  ASSIGN R-Tipo W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ INT64(Cfg_CptosTarjDb.Cod_Operacion:SCREEN-VALUE IN FRAME F-Main)
                         AND Operacion.Tipo_Producto EQ R-Tipo
                         AND Operacion.Estado        EQ 1 NO-LOCK NO-ERROR. 
  IF AVAIL(Operacion) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Operacion.Nom_Operacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cod_Operacion C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cod_Operacion IN FRAME F-Main /* CodOper */
DO:
  ASSIGN R-Tipo W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ INT64(Cfg_CptosTarjDb.Cod_Operacion:SCREEN-VALUE IN FRAME F-Main)
                       AND Operacion.Tipo_Producto   EQ R-Tipo
                       AND Operacion.Estado EQ 1
      NO-LOCK NO-ERROR. 
  IF AVAIL(Operacion) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Operacion.Nom_Operacion.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_Gmf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_Gmf C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_Gmf IN FRAME F-Main /* CrGmf */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_Gmf:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_Gmf C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_Gmf IN FRAME F-Main /* CrGmf */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_Gmf:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_GmfCBco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_GmfCBco C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_GmfCBco IN FRAME F-Main /* Cr_GmfCBco */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_GmfCBco:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_GmfCBco C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_GmfCBco IN FRAME F-Main /* Cr_GmfCBco */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_GmfCBco:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_IngCoop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_IngCoop C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_IngCoop IN FRAME F-Main /* Cr_IngCoop */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_IngCoop:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_IngCoop C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_IngCoop IN FRAME F-Main /* Cr_IngCoop */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_IngCoop:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_IngxCom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_IngxCom C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_IngxCom IN FRAME F-Main /* Cr_IngxCom */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_IngxCom:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_IngxCom C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_IngxCom IN FRAME F-Main /* Cr_IngxCom */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_IngxCom:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_IvaxCom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_IvaxCom C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_IvaxCom IN FRAME F-Main /* Cr_IvaxCom */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_IvaxCom:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_IvaxCom C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_IvaxCom IN FRAME F-Main /* Cr_IvaxCom */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_IvaxCom:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_Monto C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_Monto IN FRAME F-Main /* CrM */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_Monto:SCREEN-VALUE
                         AND Cuentas.Tipo EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
        ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_Monto C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_Monto IN FRAME F-Main /* CrM */
DO:
    W-NomCta:SCREEN-VALUE = "".
    
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_Monto:SCREEN-VALUE
                         AND Cuentas.Tipo EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
        ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
    ELSE
        APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_SegCartera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_SegCartera C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_SegCartera IN FRAME F-Main /* Cr_SegCar */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_segCartera:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_SegCartera C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_SegCartera IN FRAME F-Main /* Cr_SegCar */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_SegCartera:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Cr_TrasAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_TrasAge C-Win
ON ENTRY OF Cfg_CptosTarjDB.Cr_TrasAge IN FRAME F-Main /* Cr_TrasAge */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_TrasAge:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Cr_TrasAge C-Win
ON LEAVE OF Cfg_CptosTarjDB.Cr_TrasAge IN FRAME F-Main /* Cr_TrasAge */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Cr_TrasAge:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Db_Gmf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_Gmf C-Win
ON ENTRY OF Cfg_CptosTarjDB.Db_Gmf IN FRAME F-Main /* DbGmf */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_Gmf:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_Gmf C-Win
ON LEAVE OF Cfg_CptosTarjDB.Db_Gmf IN FRAME F-Main /* DbGmf */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_Gmf:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Db_GmfCBco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_GmfCBco C-Win
ON ENTRY OF Cfg_CptosTarjDB.Db_GmfCBco IN FRAME F-Main /* Db_GmfCBco */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_GmfCBco:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_GmfCBco C-Win
ON LEAVE OF Cfg_CptosTarjDB.Db_GmfCBco IN FRAME F-Main /* Db_GmfCBco */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_GmfCBco:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Db_IngCoop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_IngCoop C-Win
ON ENTRY OF Cfg_CptosTarjDB.Db_IngCoop IN FRAME F-Main /* Db_IngCoop */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_IngCoop:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_IngCoop C-Win
ON LEAVE OF Cfg_CptosTarjDB.Db_IngCoop IN FRAME F-Main /* Db_IngCoop */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_IngCoop:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Db_Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_Monto C-Win
ON ENTRY OF Cfg_CptosTarjDB.Db_Monto IN FRAME F-Main /* DbM */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_Monto:SCREEN-VALUE
                         AND Cuentas.Tipo EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_Monto C-Win
ON LEAVE OF Cfg_CptosTarjDB.Db_Monto IN FRAME F-Main /* DbM */
DO:
    W-NomCta:SCREEN-VALUE = "".

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_Monto:SCREEN-VALUE
                         AND Cuentas.Tipo EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
        ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
    ELSE
        APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_CptosTarjDB.Db_SegCartera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_SegCartera C-Win
ON ENTRY OF Cfg_CptosTarjDB.Db_SegCartera IN FRAME F-Main /* Db_SegCar */
DO:
    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_segCartera:SCREEN-VALUE
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN
       ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_CptosTarjDB.Db_SegCartera C-Win
ON LEAVE OF Cfg_CptosTarjDB.Db_SegCartera IN FRAME F-Main /* Db_SegCar */
DO:
  W-NomCta:SCREEN-VALUE = "".
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Db_SegCartera:SCREEN-VALUE
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
     ASSIGN W-NomCta:SCREEN-VALUE = Cuentas.Nombre.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-Tipo C-Win
ON VALUE-CHANGED OF R-Tipo IN FRAME F-Main
DO:
    ASSIGN R-Tipo.
    IF R-Tipo = 1 THEN DO:
        ASSIGN Cmb_Operaciones:LIST-ITEMS = "".
        
        ENABLE Gmf_AsumeCoop WITH FRAME f-main.

        DISABLE Db_SegCartera
                Cr_SegCartera
            WITH FRAME f-Main.

        FOR EACH Cfg_cptosTarjDb WHERE Cfg_cptosTarjDb.clase = 10 NO-LOCK BREAK BY Cfg_cptosTarjDb.TipoTrans:
            W_Ok = Cmb_Operaciones:ADD-LAST( STRING(Cfg_cptosTarjDb.TipoTrans,"99") + " - " + Cfg_cptosTarjDb.descrip ).

            IF FIRST(Cfg_cptosTarjDb.TipoTrans) THEN
                ASSIGN Cmb_Operaciones:SCREEN-VALUE = STRING(Cfg_cptosTarjDb.TipoTrans,"99") + " - " + Cfg_cptosTarjDb.descrip
                       W-Clase = Cfg_cptosTarjDb.clase  W-Tipo = Cfg_cptosTarjDb.TipoTrans
                       W-CodCre = 0.
        END.
    END.
    ELSE DO:
        ASSIGN Cmb_Operaciones:LIST-ITEMS = "".

        ENABLE Db_SegCartera
               Cr_SegCartera
            WITH FRAME f-Main.

        DISABLE Gmf_AsumeCoop WITH FRAME f-main.

        FOR EACH Cfg_cptosTarjDb WHERE Cfg_cptosTarjDb.clase = 20 NO-LOCK BREAK BY Cfg_cptosTarjDb.TipoTrans:
            W_Ok = Cmb_Operaciones:ADD-LAST( STRING(Cfg_cptosTarjDb.TipoTrans,"99") + " - CodCupo: " + STRING(Cfg_CptosTarjDb.Cod_Credito,"999") + " - " + Cfg_cptosTarjDb.descrip).

            IF FIRST(Cfg_cptosTarjDb.TipoTrans) THEN
                ASSIGN Cmb_Operaciones:SCREEN-VALUE = STRING(Cfg_cptosTarjDb.TipoTrans,"99") + " - CodCupo: " + STRING(Cfg_CptosTarjDb.Cod_Credito) + " - " + Cfg_cptosTarjDb.descrip
                       W-Clase = Cfg_cptosTarjDb.clase  W-Tipo = Cfg_cptosTarjDb.TipoTrans
                       W-CodCre = Cfg_cptosTarjDb.Cod_Credito.
        END.
    END.

    RUN inicializar_Vbles.
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
  ASSIGN Cmb_Operaciones:LIST-ITEMS = "".
  FOR EACH Cfg_cptosTarjDb WHERE Cfg_cptosTarjDb.clase = 10 NO-LOCK BREAK BY Cfg_cptosTarjDb.TipoTrans:
      W_Ok = Cmb_Operaciones:ADD-LAST(STRING(Cfg_cptosTarjDb.TipoTrans,"99") + " - " + Cfg_cptosTarjDb.descrip).
      IF FIRST(Cfg_cptosTarjDb.TipoTrans) THEN
          ASSIGN Cmb_Operaciones:SCREEN-VALUE = STRING(Cfg_cptosTarjDb.TipoTrans,"99") + " - " + Cfg_cptosTarjDb.descrip
                 W-Clase = Cfg_cptosTarjDb.clase  W-Tipo = Cfg_cptosTarjDb.TipoTrans.
  END.

  RUN inicializar_Vbles.

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
  DISPLAY R-Tipo Cmb_Operaciones W-NomCta 
      WITH FRAME F-Main IN WINDOW C-Win.
  IF AVAILABLE Cfg_CptosTarjDB THEN 
    DISPLAY Cfg_CptosTarjDB.Gmf_AsumeCoop Cfg_CptosTarjDB.Db_Monto 
          Cfg_CptosTarjDB.Cr_Monto Cfg_CptosTarjDB.GMF Cfg_CptosTarjDB.TopeGmf 
          Cfg_CptosTarjDB.Db_Gmf Cfg_CptosTarjDB.Cr_Gmf 
          Cfg_CptosTarjDB.Db_GmfCBco Cfg_CptosTarjDB.Cr_GmfCBco 
          Cfg_CptosTarjDB.Db_IngCoop Cfg_CptosTarjDB.Cr_IngCoop 
          Cfg_CptosTarjDB.Cr_IngxCom Cfg_CptosTarjDB.Cr_IvaxCom 
          Cfg_CptosTarjDB.Db_SegCartera Cfg_CptosTarjDB.Cr_SegCartera 
          Cfg_CptosTarjDB.Cod_Operacion Cfg_CptosTarjDB.Cr_TrasAge 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE Cfg_CptosTarjDB.Gmf_AsumeCoop R-Tipo Cmb_Operaciones 
         Cfg_CptosTarjDB.Db_Monto Cfg_CptosTarjDB.Cr_Monto Cfg_CptosTarjDB.GMF 
         Cfg_CptosTarjDB.TopeGmf Cfg_CptosTarjDB.Db_Gmf Cfg_CptosTarjDB.Cr_Gmf 
         Cfg_CptosTarjDB.Db_GmfCBco Cfg_CptosTarjDB.Cr_GmfCBco 
         Cfg_CptosTarjDB.Db_IngCoop Cfg_CptosTarjDB.Cr_IngCoop 
         Cfg_CptosTarjDB.Cr_IngxCom Cfg_CptosTarjDB.Cr_IvaxCom 
         Cfg_CptosTarjDB.Cod_Operacion Cfg_CptosTarjDB.Cr_TrasAge 
         Btn-Cancelar-2 Btn-Salvar-2 Btn_Done W-NomCta RECT-344 RECT-345 
         RECT-346 RECT-347 RECT-348 RECT-350 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Vbles C-Win 
PROCEDURE Inicializar_Vbles :
FIND FIRST Cfg_CptosTarjDb WHERE Cfg_CptosTarjDb.Clase EQ W-Clase
                             AND Cfg_CptosTarjDb.TipoTrans EQ W-Tipo
                             AND Cfg_CptosTarjDb.Cod_Credito EQ W-CodCre NO-LOCK NO-ERROR.
DO WITH FRAME F-Main:
    ASSIGN Cfg_CptosTarjDb.Cod_Operacion:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cod_Operacion)
           Cfg_CptosTarjDb.Cr_Gmf:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_Gmf)
           Cfg_CptosTarjDb.Cr_GmfCBco:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_GmfCBco)
           Cfg_CptosTarjDb.Cr_IngCoop:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_IngCoop)
           Cfg_CptosTarjDb.Cr_IngxCom:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_IngxCom)
           Cfg_CptosTarjDb.Cr_IvaxCom:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_IvaxCom)
           Cfg_CptosTarjDb.Cr_Monto:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_Monto)
           Cfg_CptosTarjDb.Cr_TrasAge:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_TrasAge)
           Cfg_CptosTarjDb.Db_Gmf:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Db_Gmf)
           Cfg_CptosTarjDb.Db_GmfCBco:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Db_GmfCBco)
           Cfg_CptosTarjDb.Db_IngCoop:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Db_IngCoop)
           Cfg_CptosTarjDb.Db_Monto:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Db_Monto)
           Cfg_CptosTarjDb.GMF:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.GMF,"99.999999")
           Cfg_CptosTarjDb.TopeGmf:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.TopeGmf,"zzz,zzz,zz9")
           Cfg_CptosTarjDb.Db_SegCartera:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Db_segCartera)
           Cfg_CptosTarjDb.Cr_SegCartera:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Cr_segCartera)
           Cfg_CptosTarjDb.Gmf_AsumeCoop:SCREEN-VALUE = STRING(Cfg_CptosTarjDb.Gmf_AsumeCoop).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

