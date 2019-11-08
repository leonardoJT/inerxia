&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Importar_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Importar_Productos 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{incluido\Variable.i "SHARED"}

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VARIABLE W_Rpta AS LOGICAL.
DEFINE VAR W_Cbt AS INTEGER INITIAL 21.
DEFINE VAR W_CtaDeb AS CHARACTER.
DEFINE VAR W_CtaCre AS CHARACTER.
DEFINE VAR j AS INTEGER.
DEFINE VAR k AS INTEGER.

DEFINE TEMP-TABLE Tmp
  FIELD CAge AS INTEGER FORMAT "999"
  FIELD CNit AS CHARACTER FORMAT "X(11)"
  FIELD CCuo AS DECIMAL FORMAT "9999999999".

DEFINE TEMP-TABLE TmpMov LIKE Mov_Contable.
DEFINE TEMP-TABLE TmpAho LIKE Mov_Ahorros.

DEFINE VAR TDeb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9". 
DEFINE VAR TCre AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR procname AS CHARACTER.
DEFINE VAR OKpressed AS LOGICAL INITIAL TRUE.

/* oakley */

DEFINE VAR NatAnex_Det    LIKE Cuentas.Naturaleza. 
DEFINE VAR VARCtaAnex_Tot LIKE Cuentas.Cuenta.     
DEFINE VAR VARNatAnex_Tot LIKE Cuentas.Naturaleza. 
DEFINE VAR CtaAnex_Tot    LIKE cuentas.cuenta.
DEFINE VAR W_Nat          LIKE Cuentas.Natur.
DEFINE VAR NatAnex_Tot    LIKE Cuentas.Natur.
DEFINE VAR W_Ctr          LIKE Cuentas.Ctr_Natur.
DEFINE VAR W_TX AS CHARACTER FORMAT "X(25)".
DEFINE VAR w_doc AS INTEGER INITIAL 0.

DEFINE VAR W_NoCargue AS INTEGER INITIAL 0.
DEFINE VAR W_VlrNoC   AS DECIMAL INITIAL 0.00.

DEFINE VAR W_OKTmp AS LOGICAL INITIAL YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-323 RECT-324 Cmb_agencia Cmb_TProducto ~
Cmb_Cptes Rs_DbCr W_CtaContab W_CtaContra BUTTON-26 BUTTON-3 Preview ~
Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS Cmb_agencia Cmb_TProducto Cmb_Productos ~
Cmb_Cptes Rs_DbCr W_CtaContab NomCta1 W_CtaContra NomCta2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Importar_Productos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Conta 
     LABEL "Contabilizar" 
     SIZE 50 BY 1.12.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-26 
     LABEL "Revisar Estructura Archivo Plano" 
     SIZE 50 BY 1.08.

DEFINE BUTTON BUTTON-3 
     LABEL "Leer Archivo Plano" 
     SIZE 50 BY 1.12.

DEFINE BUTTON Preview 
     LABEL "Revisar comprobante sin contabilzar" 
     SIZE 50 BY 1.12.

DEFINE VARIABLE Cmb_agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia de Cargue" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Cptes AS CHARACTER FORMAT "X(30)":U 
     LABEL "Comprobante" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 24.86 BY 1 TOOLTIP "Seleccione el Comprobante para los Asientos Contables"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 33.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TProducto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipos de Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 34 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta1 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45.14 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCta2 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaContab AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Db/Cr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .85 TOOLTIP "Con Doble click consulta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaContra AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Contrapartida" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_DbCr AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Valores Débito", 1,
"Valores Crédito", 2
     SIZE 33.43 BY .58 NO-UNDO.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY .81.

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 3.77.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 2 BY 1.08
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Cmb_agencia AT ROW 1.88 COL 25.14 COLON-ALIGNED
     Cmb_TProducto AT ROW 2.96 COL 25.43 COLON-ALIGNED
     Cmb_Productos AT ROW 4.04 COL 25.43 COLON-ALIGNED
     Cmb_Cptes AT ROW 6.46 COL 20.72 COLON-ALIGNED
     Rs_DbCr AT ROW 7.77 COL 23 NO-LABEL
     W_CtaContab AT ROW 8.69 COL 20.72 COLON-ALIGNED
     NomCta1 AT ROW 8.73 COL 35.86 COLON-ALIGNED NO-LABEL
     W_CtaContra AT ROW 9.77 COL 20.86 COLON-ALIGNED
     NomCta2 AT ROW 9.81 COL 36 COLON-ALIGNED NO-LABEL
     BUTTON-26 AT ROW 11.23 COL 17 WIDGET-ID 2
     BUTTON-3 AT ROW 12.54 COL 17.14
     Preview AT ROW 13.85 COL 17.14
     Btn_Conta AT ROW 15.15 COL 17.14
     Btn_Done AT ROW 16.54 COL 69.14
     "Naturaleza" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 7.69 COL 11.14
     "Descripción del Cargue" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.27 COL 7
          FGCOLOR 7 
     RECT-323 AT ROW 7.62 COL 22.43
     RECT-324 AT ROW 1.54 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.86 BY 21.19
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.27 COL 22
     R1 AT ROW 1.27 COL 3.14
     R2 AT ROW 1.27 COL 5.14
     R3 AT ROW 1.27 COL 7.14
     R4 AT ROW 1.27 COL 9.14
     R5 AT ROW 1.27 COL 11.14
     R6 AT ROW 1.27 COL 13.14
     R7 AT ROW 1.27 COL 15.14
     R8 AT ROW 1.27 COL 17.14
     R9 AT ROW 1.27 COL 19.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24.14 ROW 16.69
         SIZE 26 BY 1.88
         BGCOLOR 17 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Importar_Productos ASSIGN
         HIDDEN             = YES
         TITLE              = "Importa movimientos de Productos de Ahorro"
         HEIGHT             = 18.77
         WIDTH              = 85.29
         MAX-HEIGHT         = 27.23
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.23
         VIRTUAL-WIDTH      = 146.29
         DROP-TARGET        = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Importar_Productos 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Importar_Productos
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Progreso:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Conta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Productos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCta2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Importar_Productos)
THEN W-Importar_Productos:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Importar_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Importar_Productos W-Importar_Productos
ON END-ERROR OF W-Importar_Productos /* Importa movimientos de Productos de Ahorro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Importar_Productos W-Importar_Productos
ON WINDOW-CLOSE OF W-Importar_Productos /* Importa movimientos de Productos de Ahorro */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Conta W-Importar_Productos
ON CHOOSE OF Btn_Conta IN FRAME F-Main /* Contabilizar */
DO:
  ASSIGN j = 0.
  RUN Generar_Movimiento.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Importar_Productos
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


&Scoped-define SELF-NAME BUTTON-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-26 W-Importar_Productos
ON CHOOSE OF BUTTON-26 IN FRAME F-Main /* Revisar Estructura Archivo Plano */
DO:
    MESSAGE "Archivo CSV Separado por ';' " SKIP(2)
        "Agencia ;" SKIP
        "Nit;" SKIP
        "Valor"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Importar_Productos
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Leer Archivo Plano */
DO:
  ASSIGN J = 0.
  VIEW FRAME F_Progreso.
  FOR EACH Tmp: 
      DELETE Tmp. 
  END.

  EMPTY TEMP-TABLE Tmp.
  EMPTY TEMP-TABLE TmpMov.
  EMPTY TEMP-TABLE TmpAho.

  SYSTEM-DIALOG GET-FILE procname
        TITLE      "Choose Procedure to Run ..."
        FILTERS    "Source Files (*.txt)"   "*.txt",
                   "R-code Files (*.csv)"   "*.csv"
        INITIAL-DIR "C:\info_fodun\"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
  RUN _SetCurs.r ("WAIT").     
    IF OKpressed = TRUE THEN
        RUN Generar_Temporal.
  RUN _SetCurs.r ("ARROW").
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_agencia W-Importar_Productos
ON VALUE-CHANGED OF Cmb_agencia IN FRAME F-Main /* Agencia de Cargue */
DO:
  ASSIGN cmb_agencia
         Cmb_Cptes:LIST-ITEMS = "".
  FOR EACH Comprobantes WHERE Comprobantes.Agencia             EQ INTEGER(SUBSTRING(Cmb_Agencia,1,3))
                              AND Comprobantes.Estado          EQ 1 NO-LOCK:
    Cmb_Cptes:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + "-" + 
                             STRING(Comprobantes.Nombre,"X(25)")).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Cptes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Cptes W-Importar_Productos
ON VALUE-CHANGED OF Cmb_Cptes IN FRAME F-Main /* Comprobante */
DO:
  ASSIGN cmb_cptes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_TProducto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TProducto W-Importar_Productos
ON VALUE-CHANGED OF Cmb_TProducto IN FRAME F-Main /* Tipos de Producto */
DO:
 ASSIGN  Cmb_TProducto
         Cmb_Productos:LIST-ITEMS = "".
 FOR EACH Pro_Ahorros WHERE 
     Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
     Pro_Ahorros.Estado     EQ 1 NO-LOCK:
     W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).      
 END.
 Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
 Cmb_Productos:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Preview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Preview W-Importar_Productos
ON CHOOSE OF Preview IN FRAME F-Main /* Revisar comprobante sin contabilzar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  /*{Imprimir.I "listado"}*/   



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_DbCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_DbCr W-Importar_Productos
ON MOUSE-SELECT-CLICK OF Rs_DbCr IN FRAME F-Main
DO:
  ASSIGN Rs_DbCr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaContab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContab W-Importar_Productos
ON LEAVE OF W_CtaContab IN FRAME F-Main /* Cuenta Db/Cr */
DO:
  ASSIGN W_CtaContab
         NomCta1:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContab
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN
  DO:
     ASSIGN NomCta1:SCREEN-VALUE = Cuentas.Nombre
            NatAnex_Det = Cuentas.Naturaleza.
     IF Rs_DbCr EQ 1 THEN    
        W_CtaDeb = Cuentas.Cuenta.         
     ELSE                    
        W_CtaCre = Cuentas.Cuenta.         
     END.                    
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContab W-Importar_Productos
ON MOUSE-SELECT-DBLCLICK OF W_CtaContab IN FRAME F-Main /* Cuenta Db/Cr */
DO:
     ASSIGN W-Importar_Productos:SENSITIVE   = FALSE
            NomCta1:SCREEN-VALUE     = ""
            W_CtaContab:SCREEN-VALUE = ""
            W_CtaContab.

     RUN C-Cuentas.R (OUTPUT W_CtaContab,OUTPUT NomCta1, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                      INPUT  2).  

     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContab
                         AND Cuentas.Tipo   EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN
        ASSIGN NomCta1:SCREEN-VALUE     = Cuentas.Nombre
               W_CtaContab:SCREEN-VALUE = W_CtaContab
               NomCta1:SCREEN-VALUE = Cuentas.Nombre
               NatAnex_Det = Cuentas.Naturaleza.
     ELSE W_CtaContab = "".

     ASSIGN W-importar_productos:SENSITIVE = TRUE.  
     W-Importar_Productos:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaContra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContra W-Importar_Productos
ON LEAVE OF W_CtaContra IN FRAME F-Main /* Cuenta Contrapartida */
DO:
  ASSIGN W_CtaContra
         NomCta2:SCREEN-VALUE = "".

  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Cuentas) THEN 
  DO:
     ASSIGN NomCta2:SCREEN-VALUE = Cuentas.Nombre
            CtaAnex_Tot = Cuentas.Cuenta
            NatAnex_Tot = Cuentas.Naturaleza.
     IF Rs_DbCr EQ 1 THEN    
        W_CtaCre = Cuentas.Cuenta.         
     ELSE                    
        W_CtaDeb = Cuentas.Cuenta.         
  END.
  ELSE APPLY "Mouse-Select-DblClick" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContra W-Importar_Productos
ON MOUSE-SELECT-DBLCLICK OF W_CtaContra IN FRAME F-Main /* Cuenta Contrapartida */
DO:
     ASSIGN W-Importar_Productos:SENSITIVE   = FALSE
            NomCta2:SCREEN-VALUE      = ""
            W_CtaContra:SCREEN-VALUE = ""
            W_CtaContra.

     RUN C-Cuentas.R (OUTPUT W_CtaContra,OUTPUT NomCta2, OUTPUT W_Nat, OUTPUT W_Ctr,                         
                      INPUT  2).  

     FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                         AND Cuentas.Tipo    EQ 2
                         AND Cuentas.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN
        ASSIGN NomCta2:SCREEN-VALUE     = Cuentas.Nombre
               W_CtaContra:SCREEN-VALUE = W_CtaContra.
     ELSE W_CtaContra = "".

     ASSIGN W-Importar_Productos:SENSITIVE = TRUE. 
     W-Importar_Productos:MOVE-TO-TOP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Importar_Productos 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Importar_Productos  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Importar_Productos  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Importar_Productos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Importar_Productos)
  THEN DELETE WIDGET W-Importar_Productos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Importar_Productos  _DEFAULT-ENABLE
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
  DISPLAY Cmb_agencia Cmb_TProducto Cmb_Productos Cmb_Cptes Rs_DbCr W_CtaContab 
          NomCta1 W_CtaContra NomCta2 
      WITH FRAME F-Main IN WINDOW W-Importar_Productos.
  ENABLE RECT-323 RECT-324 Cmb_agencia Cmb_TProducto Cmb_Cptes Rs_DbCr 
         W_CtaContab W_CtaContra BUTTON-26 BUTTON-3 Preview Btn_Done 
      WITH FRAME F-Main IN WINDOW W-Importar_Productos.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW W-Importar_Productos.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW W-Importar_Productos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_MovConTmp W-Importar_Productos 
PROCEDURE Generar_MovConTmp :
/*debito*/
  CREATE TmpMov.
  ASSIGN TmpMov.Agencia         = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
         TmpMov.Cen_Costos      = 999
         TmpMov.Cuenta          = W_CtaDeb
         TmpMov.Comprobante     = W_Cbt /*nota contable*/
         TmpMov.Fec_Contable    = W_Fecha
         TmpMov.Fec_Grabacion   = W_Fecha
         TmpMov.Num_Documento   = W_Doc
         TmpMov.Doc_Referencia  = STRING(W_Doc)
         TmpMov.Nit             = TRIM(Tmp.CNit)
         TmpMov.Comentario      = "Cargue de Productos (Db)"
         TmpMov.Usuario         = W_Usuario. 
         TmpMov.DB              = Tmp.CCuo.
  /*credito*/
  CREATE TmpMov.
  ASSIGN TmpMov.Agencia         = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
         TmpMov.Cen_Costos      = 999
         TmpMov.Cuenta          = W_CtaCre
         TmpMov.Comprobante     = W_Cbt /*nota contable*/
         TmpMov.Fec_Contable    = W_Fecha
         TmpMov.Fec_Grabacion   = W_Fecha
         TmpMov.Num_Documento   = W_Doc
         TmpMov.Doc_Referencia  = STRING(W_Doc)
         TmpMov.Nit             = TRIM(Tmp.CNit)
         TmpMov.Comentario      = "Cargue de Productos (Cr)"
         TmpMov.Usuario         = W_Usuario. 
         TmpMov.CR              = Tmp.CCuo.

   CREATE TmpAho.
   ASSIGN TmpAho.Agencia         = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) 
          TmpAho.Age_Destino     = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) 
          TmpAho.Age_Fuente      = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
          TmpAho.Cod_Ahorro      = Ahorros.Cod_Ahorro
          TmpAho.Cod_Operacion   = 010101001
          TmpAho.Cpte            = INTEGER(SUBSTRING(Cmb_Cptes,1,2))
          TmpAho.Cue_Ahorros     = ahorros.cue_ahorros
          TmpAho.Descrip         = "Importacion de Productos"
          TmpAho.Fecha           = W_Fecha
          TmpAho.Hora            = TIME
          TmpAho.Nit             = Ahorros.nit
          TmpAho.Num_Documento   = STRING(W_Doc)
          TmpAho.Sdo_Disponible  = Ahorros.Sdo_disponible + Tmp.CCuo
          TmpAho.Usuario         = W_Usuario
          TmpAho.Val_Efectivo    = Tmp.CCuo
          TmpAho.Cedula_Trans    = Tmp.CNit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Movimiento W-Importar_Productos 
PROCEDURE Generar_Movimiento :

DO TRANSACTION:
 MESSAGE "Agencia" INTEGER(SUBSTRING(Cmb_Agencia,1,3)) SKIP
         "Comprobante"  INTEGER(SUBSTRING(Cmb_Cptes,1,2))
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
 FIND FIRST comprobantes WHERE comprobantes.agencia   EQ INTEGER(SUBSTRING(Cmb_Agencia,1,3)) AND
                         comprobantes.comprobante     EQ INTEGER(SUBSTRING(Cmb_Cptes,1,2)) AND 
                              Comprobantes.Estado     EQ 1 NO-ERROR.
 ASSIGN Comprobantes.Secuencia    = Comprobantes.Secuencia    + 1
        W_Doc = comprobantes.secuencia.

 FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
 IF W_Doc EQ 0 THEN DO:
   MESSAGE "El numero de documento esta en 0" VIEW-AS ALERT-BOX.
   RETURN ERROR.
 END.

 FOR EACH TmpMov:
   ASSIGN j = j + 1.
   RUN Progreso.
   CREATE Mov_Contable.
   BUFFER-COPY TmpMov TO Mov_Contable NO-ERROR.
   ASSIGN Mov_contable.Num_Documento  = W_Doc
          Mov_contable.Doc_Referencia = STRING(W_Doc).

   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Error al crear la partida contable para" SKIP
              "Cuenta: " TmpMov.Cuenta SKIP
              "Nit: " TmpMov.Nit SKIP
              "Valor Db: " TmpMov.Db " Valor Cr: " TmpMov.Cr SKIP(2)
              "Se cancela el proceso !!!"
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.

 END.
 FOR EACH TmpAho:
   ASSIGN j = j + 1.
   RUN Progreso.
   IF TmpAho.Cod_Ahorro EQ INTEGER(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F-Main,1,3)) THEN DO:
      FIND FIRST Ahorros WHERE
           Ahorros.Agencia    EQ TmpAho.Agencia AND
           Ahorros.Tip_Ahorro EQ INTEGER(SUBSTR(Cmb_TProducto:SCREEN-VALUE IN FRAME F-Main,1,1)) AND
           Ahorros.Cod_Ahorro EQ INTEGER(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
           Ahorros.Nit        EQ TmpAho.Nit /*AND 
           ahorros.sdo_disponible GT 0*/ EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(Ahorros) THEN DO:
         ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + TmpAho.Val_Efectivo
                Ahorros.Fec_UltTransaccion = W_Fecha.
         
         CREATE Mov_Ahorros.
         ASSIGN Mov_Ahorros.Agencia         = Ahorros.agencia 
                Mov_Ahorros.Age_Destino     = Ahorros.agencia 
                Mov_Ahorros.Age_Fuente      = Ahorros.agencia 
                Mov_Ahorros.Cod_Ahorro      = Ahorros.Cod_Ahorro
                Mov_Ahorros.Cod_Operacion   = 010101001
                Mov_Ahorros.Cpte            = INTEGER(SUBSTRING(Cmb_Cptes,1,2))
                Mov_Ahorros.Cue_Ahorros     = ahorros.cue_ahorros
                Mov_Ahorros.Descrip         = "Importacion de Productos"
                Mov_Ahorros.Fecha           = W_Fecha
                Mov_Ahorros.Hora            = TIME
                Mov_Ahorros.Nit             = Ahorros.nit
                Mov_Ahorros.Num_Documento   = STRING(W_Doc)
                Mov_Ahorros.Sdo_Disponible  = Ahorros.Sdo_disponible
                Mov_Ahorros.Usuario         = W_Usuario
                Mov_Ahorros.Val_Efectivo    = tmpAho.Val_Efectivo
                mov_ahorros.Cedula_Trans    = Ahorros.nit.
      END.
      ELSE DO:
          MESSAGE "El cliente identifiado con nit: " TmpAho.Nit SKIP
                  "no tiene cuenta seleccionada" SKIP(2)
                  "se cancela el proceso " VIEW-AS ALERT-BOX.
          RETURN ERROR.
      END.
   END.
   RELEASE Ahorros.
 END.
END. /* TRANSACTION.*/
RUN Rutina_Imprimir (INPUT W_Cbt, INPUT INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)),
                     INPUT W_Doc, INPUT W_Doc).

MESSAGE "Contabilización Realizada" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Temporal W-Importar_Productos 
PROCEDURE Generar_Temporal :
DEFINE VAR datos AS CHARACTER FORMAT "X(80)".
DEFINE VARIABLE xtot AS DECIMAL.
INPUT FROM VALUE(Procname).
REPEAT:
   create tmp.
   IMPORT DELIMITER ";" tmp.
   ASSIGN xtot = xtot + ccuo.
END.
INPUT CLOSE.
MESSAGE STRING(xtot,">>>,>>>,>>>,>>9.99") VIEW-AS ALERT-BOX.
ASSIGN xtot = 0.

FOR EACH TmpMov: DELETE TmpMov. END.

ASSIGN j      = 0.


DEFINE VAR w_dirtempo AS CHARACTER FORMAT "X(80)".
w_dirtempo = W_PathSpl + "\" + STRING(W_agencia,"999") + "-" + SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F-Main,1,3) + "-" +
            STRING(W_Fecha,"999999") + ".txt".

OUTPUT TO VALUE(w_dirtempo).
FOR EACH Tmp WHERE
         Tmp.CAge NE ? AND
         Tmp.CCuo NE ?:
  ASSIGN j = j + 1
         W_OkTmp = YES.
  RUN Progreso.
  FIND Clientes WHERE Clientes.Nit EQ TRIM(Tmp.CNit) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Clientes) AND Tmp.CNit NE "" THEN DO:
      MESSAGE "El Nit: " Tmp.Cnit "No existe. Rectifique!" VIEW-AS ALERT-BOX.
      W_OkTmp = NO.
  END.
  FIND FIRST Ahorros WHERE
       Ahorros.Agencia    EQ Tmp.CAge AND
       Ahorros.Tip_Ahorro EQ INTEGER(SUBSTR(Cmb_TProducto:SCREEN-VALUE IN FRAME F-Main,1,1)) AND
       Ahorros.Cod_Ahorro EQ INTEGER(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
       Ahorros.Nit        EQ Tmp.CNit and
       ahorros.sdo_disponible GE 0 NO-LOCK NO-ERROR.

  IF NOT AVAILABLE(Ahorros) THEN DO:
     DISPLAY "Nit: " Tmp.CNit "no tiene cuenta Seleccionada -Vlr " Tmp.CCuo WITH FRAME j1 WITH WIDTH 120 NO-LABELS USE-TEXT.
     W_OkTmp = NO.
  END.
  /*
  IF (Ahorros.sdo_disponible + Ahorros.sdo_canje) LE 0 OR ahorros.estado EQ 2 THEN DO:
      DISPLAY "Inconsistencia Nit " Tmp.CNit  " con Vlr " Tmp.CCuo " -Producto con estado Inactivo"  WITH FRAME j2 WITH WIDTH 120 NO-LABELS USE-TEXT.
      W_OkTmp = NO.
  END.

  IF Tmp.CCuo EQ 0 THEN DO:
     DISPLAY "Valor en Cero para el cargue Nit " Tmp.CNit  " con Vlr " Tmp.CCuo " Con valor Cero"  WITH FRAME j3 WITH WIDTH 120 NO-LABELS USE-TEXT.
     W_OkTmp = NO.
  END.*/
  IF NOT W_OkTmp THEN DO:
     ASSIGN W_NoCargue = W_NoCargue + 1
            W_VlrNoc   = W_VlrNoc + Tmp.CCuo.
     MESSAGE "no existe " Tmp.CNit VIEW-AS ALERT-BOX.
     NEXT.
  END.
  ELSE
     RUN Generar_MovConTmp.
      ASSIGN xtot = xtot + ccuo.
END.

DISPLAY "Registros No cargados " W_NoCargue SKIP
        "Valor No cargado      " W_VlrNoc   FORMAT "zzz,zzz,zz9.99" WITH FRAME j6 WITH WIDTH 120 NO-LABELS.
OUTPUT CLOSE.
ASSIGN TCre = 0 TDeb = 0 W_OkTmp = YES.
FOR EACH TmpMov:
    IF TmpMov.CR NE 0 THEN
       TCre = TCre + TmpMov.CR.
    ELSE
       TDeb = TDeb + TmpMov.DB.
END.

IF TCre NE TDeb THEN DO:
   MESSAGE "Débitos y Créditos Diferentes" SKIP
           "Débitos : " TDeb SKIP
           "Créditos: " TCre VIEW-AS ALERT-BOX.
   W_OkTmp = NO.
END.

IF W_OkTmp THEN DO:
    MESSAGE "El temporal fue creado correctamente" VIEW-AS ALERT-BOX INFORMATION.
    ENABLE Btn_Conta WITH FRAME F-main.
END.
ELSE DO:
    DISABLE Btn_Conta WITH FRAME F-Main.
    APPLY "choose" TO preview IN FRAME F-Main.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Importar_Productos 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Importar_Productos 
PROCEDURE local-initialize :
ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_TX = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_OK = Cmb_Agencia:ADD-LAST(W_TX).
      IF W_Agencia EQ Agencias.Agencia THEN
         Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre.
  END.

ASSIGN Cmb_TProducto:LIST-ITEMS IN FRAME {&FRAME-NAME} = ""
         W_Ok = Cmb_TProducto:ADD-LAST("1 - A la Vista")
         W_Ok = Cmb_TProducto:ADD-LAST("2 - Contractual")        
         W_Ok = Cmb_TProducto:ADD-LAST("3 - A Término")        
         W_Ok = Cmb_TProducto:ADD-LAST("4 - Aportes")          
         Cmb_TProducto:SCREEN-VALUE = Cmb_TProducto:ENTRY(1).  
RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Importar_Productos 
PROCEDURE ProcesoImprimir :
/*CAge/CNit/CCue/CNat/Cval/CCom*/

    DEFINE VAR IDeb LIKE Mov_Contable.CR.
    DEFINE VAR ICre LIKE Mov_Contable.CR.
    DEFINE VAR TIDeb LIKE Mov_Contable.CR.
    DEFINE VAR TICre LIKE Mov_Contable.CR.
    DEFINE VAR INom AS CHARACTER FORMAT "X(18)".

{INCLUIDO\RepEncabezado.I}    
    W_Reporte    = "REPORTE   : PRUEBA CONTABILIZACION CARGUE DE PRODUCTO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "CUENTA          COMENTARIO                     NIT          NOMBRE               DEBITO              CREDITO".          

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH TmpMov:
    FIND Clientes WHERE Clientes.Nit EQ TmpMov.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN INom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE INom = "Nit no existe en clientes".
    IF TmpMov.Db GT 0 THEN
       ASSIGN IDeb = TmpMov.Db
              TIDeb = TIDeb + IDeb 
              ICre = 0.
    ELSE
       ASSIGN ICre = TmpMov.Cr
              TICre = TICre + ICre 
              IDeb = 0.
    DISPLAY TmpMov.Cuenta      AT 1
            TmpMov.Comentario  AT 17 FORMAT "X(29)"
            TmpMov.Nit         AT 48 FORMAT "X(12)"
            INom        AT 61 FORMAT "X(18)"
            IDeb        AT 82 FORMAT ">>>,>>>,>>>,>>9.99"
            ICre        AT 102 FORMAT ">>>,>>>,>>>,>>9.99"
    WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
  END.
  DISPLAY "-------------------------------------------" AT 82
          TIDeb         AT 82
          TICre         AT 102
  WITH FRAME F-Tot USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso W-Importar_Productos 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 2 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN
             R1:BGCOL = 18.
          WHEN 2 THEN
             R2:BGCOL = 18.
          WHEN 3 THEN
             R3:BGCOL = 18.
          WHEN 4 THEN
             R4:BGCOL = 18.
          WHEN 5 THEN
             R5:BGCOL = 18.
          WHEN 6 THEN
             R6:BGCOL = 18.
          WHEN 7 THEN
             R7:BGCOL = 18.
          WHEN 8 THEN
             R8:BGCOL = 18.
          WHEN 9 THEN
             R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rutina_Imprimir W-Importar_Productos 
PROCEDURE Rutina_Imprimir :
DEFINE INPUT PARAMETER P_CompAct       LIKE Comprobantes.comprobante.
    DEFINE INPUT PARAMETER P_OfiOrigen     LIKE Agencias.Agencia.
    DEFINE INPUT PARAMETER P_DocIni        LIKE Comprobantes.Secuencia.
    DEFINE INPUT PARAMETER P_DocFin        LIKE Comprobantes.Secuencia.
  
    FIND Comprobantes WHERE Comprobantes.Comprobante EQ P_CompAct 
                        AND Comprobantes.Agencia     EQ P_OfiOrigen
                    NO-LOCK NO-ERROR.        
    IF AVAILABLE Comprobantes THEN DO:
       FIND Formatos WHERE Formatos.Agencia     EQ P_OfiOrigen
                       AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato 
                   NO-LOCK NO-ERROR.
       IF AVAILABLE(Formatos) THEN
          RUN VALUE(Formatos.Nom_Proceso) (INPUT P_CompAct,
                                           INPUT P_DocIni, INPUT P_DocFin,
                                           INPUT P_OfiOrigen).
       ELSE
         RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
    END.
    ELSE 
      RUN MostrarMensaje IN W_Manija (INPUT 268, OUTPUT W_Rpta).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Importar_Productos  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Importar_Productos 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

