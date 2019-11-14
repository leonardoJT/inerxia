&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

DEFINE VAR W_Ok AS LOGICAL.
  
{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFI TEMP-TABLE TImp
    FIELD Reng AS CHAR FORM "X(140)".

DEFINE VAR P_FecIni AS DATE.
DEFINE VAR P_FecFin AS DATE.
DEFINE VAR TCon AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TRet AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR i AS INTEGER.
DEFINE VARIABLE W_Metodo AS LOGICAL.
DEFINE VAR Linea AS CHARACTER FORMAT "X(140)".
DEFINE VARIABLE WImagen AS CHARACTER FORMAT "X(40)".
DEFINE VARIABLE WAno AS INTEGER FORMAT "9999".
DEFINE VARIABLE WMes AS INTEGER FORMAT "99".
DEFINE VARIABLE WFec AS DATE.
DEFINE VARIABLE MesIni AS INTEGER FORMAT "99".
DEFINE VARIABLE MesFin AS INTEGER FORMAT "99".
DEFINE VARIABLE AgeIni AS INTEGER FORMAT "999".
DEFINE VARIABLE AgeFin AS INTEGER FORMAT "999".
DEFINE VARIABLE ProIni AS INTEGER FORMAT "999".
DEFINE VARIABLE ProFin AS INTEGER FORMAT "999".
DEFINE VARIABLE CueIni AS CHARACTER FORMAT "X(14)".
DEFINE VARIABLE CueFin AS CHARACTER FORMAT "X(14)".
DEFINE VARIABLE NitIni AS CHARACTER FORMAT "X(14)".
DEFINE VARIABLE NitFin AS CHARACTER FORMAT "X(14)".
DEFINE VARIABLE P_linea AS INTEGER.

DEFINE TEMP-TABLE Imp
    FIELD Reg AS INTEGER
    FIELD Lin AS CHARACTER FORMAT "X(160)".

DEFINE TEMP-TABLE ext_tarDB
    FIELD nro_auditoria AS CHARACTER
    FIELD fecha AS DATE
    FIELD num_documento AS CHARACTER
    FIELD cpte AS INTEGER

      /* oakley */

        FIELD descrip        LIKE mov_ahorros.descrip
        FIELD val_consigna   LIKE mov_ahorros.val_efectivo
        FIELD val_retiro     LIKE mov_ahorros.val_efectivo
        FIELD age_fuente     LIKE mov_ahorros.age_fuente
        INDEX IdxTarDB nro_auditoria fecha num_documento cpte.

DEFINE VAR flagInformeLineas AS LOGICAL INITIAL FALSE.

DEFINE TEMP-TABLE TmpDoc
    FIELD Age LIKE Agencias.Agencia
    FIELD Com LIKE Comprobantes.Comprobante
    FIELD Doc LIKE Mov_Contable.Num_Documento
    FIELD Cta LIKE Mov_Contable.Cuenta
    FIELD nit AS CHARACTER.

DEFINE TEMP-TABLE TT_Movimientos
    FIELD tipo AS CHARACTER
    FIELD consecutivo AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD num_doc AS INTEGER
    FIELD fecha AS DATE
    FIELD doc_ref AS CHARACTER
    FIELD enlace AS CHARACTER
    FIELD sdo_inicial AS DECIMAL
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    FIELD sdo_final AS DECIMAL
    FIELD detalle AS CHARACTER.

DEFINE TEMP-TABLE cuentasProductos
    FIELD cuenta AS CHARACTER.

DEFINE TEMP-TABLE cuentasCreditos
    FIELD cuenta AS CHARACTER.

DEFINE TEMP-TABLE cuentasAhorros
    FIELD cuenta AS CHARACTER.

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
&Scoped-Define ENABLED-OBJECTS W_CodOpe BUTTON-120 BUTTON-121 BUTTON-123 ~
BUTTON-125 F_Buscar W_Rseleccion BUTTON-118 W_Rproducto Btn_Impresion ~
Btn_Nota Cmb_Agencia Cmb_Producto SExtracto Btn_Done Btn_Ayuda IMAGE-1 ~
Img_MesF Img_MesI RECT-15 RECT-217 RECT-282 RECT-283 RECT-284 RECT-285 
&Scoped-Define DISPLAYED-OBJECTS W_CodOpe AnoFin AnoIni F_Buscar ~
W_Rseleccion W_Rproducto F_Nota SExtracto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "" 
     SIZE 4 BY 1.08
     FONT 9.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 11 BY 1.62
     FONT 15.

DEFINE BUTTON Btn_Nota 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 119" 
     SIZE 5 BY .81.

DEFINE BUTTON BUTTON-118 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 118" 
     SIZE 11 BY 1.65.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BUTTON-123 
     IMAGE-UP FILE "imagenes/cancel.bmp":U
     LABEL "Button 123" 
     SIZE 4 BY .96.

DEFINE BUTTON BUTTON-125 
     LABEL "Consultar" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(20)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 55 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Producto AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ahorros" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 55 BY 1 TOOLTIP "Productos Disponibles"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Buscar AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F_Nota AS CHARACTER FORMAT "X(82)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE W_CodOpe AS INTEGER FORMAT "999999999":U INITIAL 0 
     LABEL "Cod_Operacion" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "imagenes/dinero.bmp":U
     SIZE 5 BY 1.35.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11.29 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE VARIABLE W_Rproducto AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Por Cuenta", 1,
"Por Nit", 2
     SIZE 23 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE W_Rseleccion AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ahorros", 1,
"Créditos", 2,
"Movimientos", 3
     SIZE 39 BY 1.08
     BGCOLOR 17 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 1.62.

DEFINE RECTANGLE RECT-217
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 1.88.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 1.88.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 4 GRAPHIC-EDGE    
     SIZE 27 BY 2.65
     BGCOLOR 15 FGCOLOR 7 .

DEFINE VARIABLE SExtracto AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 111.72 BY 9.42
     BGCOLOR 15 FONT 3 NO-UNDO.

DEFINE BUTTON BUTTON-124 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 124" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE F_Ope AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE T_Ope AS LOGICAL INITIAL no 
     LABEL "Sacar las Ultimas" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_CodOpe AT ROW 9.92 COL 85.43 COLON-ALIGNED
     AnoFin AT ROW 6.54 COL 54 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 6.54 COL 29 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 6.54 COL 37
     BUTTON-121 AT ROW 6.54 COL 62
     BUTTON-123 AT ROW 4 COL 70
     BUTTON-125 AT ROW 4.77 COL 100
     F_Buscar AT ROW 4.08 COL 53 COLON-ALIGNED HELP
          "Ingrese el Nit Inicial para la consulta"
     W_Rseleccion AT ROW 1.85 COL 13 HELP
          "Seleccione el producto para el Extracto" NO-LABEL
     BUTTON-118 AT ROW 1.54 COL 100
     W_Rproducto AT ROW 4.08 COL 12.57 HELP
          "La Consulta la desea por Cuenta o por Nit." NO-LABEL
     Btn_Impresion AT ROW 3.15 COL 100 HELP
          "Imprime la Información que desea consultar"
     Btn_Nota AT ROW 7.77 COL 65
     F_Nota AT ROW 8.69 COL 13 COLON-ALIGNED NO-LABEL
     Cmb_Agencia AT ROW 9.65 COL 13 COLON-ALIGNED
     Cmb_Producto AT ROW 10.73 COL 13 COLON-ALIGNED HELP
          "Seleccione el Producto que desea consultar"
     SExtracto AT ROW 11.77 COL 1.72 NO-LABEL
     Btn_Done AT ROW 6.92 COL 100
     Btn_Ayuda AT ROW 9.08 COL 104
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 5.73 COL 20
          BGCOLOR 17 FGCOLOR 7 
     "          Consulta General Por:" VIEW-AS TEXT
          SIZE 25.86 BY .62 AT ROW 8.85 COL 71.86
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 5.77 COL 45
          BGCOLOR 17 FGCOLOR 7 
     "Click en el Boton para Escribir una Nota Adjunta al Extracto" VIEW-AS TEXT
          SIZE 50 BY .88 AT ROW 7.77 COL 15
          FGCOLOR 7 
     IMAGE-1 AT ROW 1.81 COL 68.14
     Img_MesF AT ROW 6.54 COL 44.72
     Img_MesI AT ROW 6.54 COL 19.86
     RECT-15 AT ROW 3.62 COL 11
     RECT-217 AT ROW 1.5 COL 11
     RECT-282 AT ROW 5.62 COL 15
     RECT-283 AT ROW 5.62 COL 42
     RECT-284 AT ROW 1.27 COL 99
     RECT-285 AT ROW 8.73 COL 71.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.12
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.

DEFINE FRAME F_Operaciones
     BUTTON-124 AT ROW 1.27 COL 44
     T_Ope AT ROW 1.65 COL 3
     F_Ope AT ROW 1.65 COL 20 COLON-ALIGNED NO-LABEL
     "Operaciones" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 1.65 COL 28
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14.86 ROW 11.65
         SIZE 55 BY 2.96
         BGCOLOR 17 FONT 5
         TITLE "Restringir el extracto a Varias Operaciones".


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
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Generación de Extractos"
         HEIGHT             = 21.12
         WIDTH              = 113.14
         MAX-HEIGHT         = 21.12
         MAX-WIDTH          = 113.14
         VIRTUAL-HEIGHT     = 21.12
         VIRTUAL-WIDTH      = 113.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Operaciones:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX Cmb_Producto IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN F_Nota IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Operaciones
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Operaciones:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F_Ope IN FRAME F_Operaciones
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Generación de Extractos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Generación de Extractos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME} DO:
   SYSTEM-HELP "\AYUDAS\AHORRO.HLP" CONTEXT 154.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Win
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


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-Win
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
    EMPTY TEMP-TABLE Imp.

    DO i = 1 TO SExtracto:NUM-ITEMS:
        CREATE Imp.
        ASSIGN Imp.Reg = i
               Imp.Lin = SExtracto:ENTRY(i).
    END.

    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_PathSpl + W_Usuario + "_EXTRACTO.LST".
    {incluido/Imprimir_LG.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nota W-Win
ON CHOOSE OF Btn_Nota IN FRAME F-Main /* Button 119 */
DO:
  IF F_Nota:SENSITIVE THEN DO:
     W_Ok = Btn_Nota:LOAD-IMAGE("imagenes\arwdwn.gif") NO-ERROR.
     DISABLE F_Nota WITH FRAME {&FRAME-NAME}.
     F_Nota:SCREEN-VALUE = "".
     F_Nota:BGCOLOR = 17.
  END.
  ELSE DO:
    W_Ok = Btn_Nota:LOAD-IMAGE("imagenes\arwUp.gif") NO-ERROR.
    ENABLE F_Nota WITH FRAME {&FRAME-NAME}.
    F_Nota:BGCOLOR = 15.
    F_Nota:SCREEN-VALUE = "".
    APPLY "entry" TO F_Nota.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-118
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-118 W-Win
ON CHOOSE OF BUTTON-118 IN FRAME F-Main /* Button 118 */
DO:
  RUN W-InfDia.R NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 W-Win
ON CHOOSE OF BUTTON-120 IN FRAME F-Main /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(WAno)
         AnoIni = WAno
         MesIni = WMes
         P_FecIni = DATE(MesIni,01,AnoIni).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 W-Win
ON CHOOSE OF BUTTON-121 IN FRAME F-Main /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin   = WAno
         MesFin   = WMes
         P_FecFin = WFec.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-123
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-123 W-Win
ON CHOOSE OF BUTTON-123 IN FRAME F-Main /* Button 123 */
DO:
  VIEW FRAME F_Operaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Operaciones
&Scoped-define SELF-NAME BUTTON-124
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-124 W-Win
ON CHOOSE OF BUTTON-124 IN FRAME F_Operaciones /* Button 124 */
DO:
  HIDE FRAME F_Operaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-125
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-125 W-Win
ON CHOOSE OF BUTTON-125 IN FRAME F-Main /* Consultar */
DO:
    DEFINE VAR i AS INTEGER.

    DO i = 1 TO 100 BY 1:
        W_Ok = SExtracto:DELETE(1).
    END.

    ASSIGN FRAME F-Main W_RSeleccion
                        F_Nota
                        W_RProducto
                        F_Buscar
                        Cmb_Agencia
                        Cmb_Producto
                        AnoIni
                        AnoFin.

    IF SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN ProIni = 0
               ProFin = 999.

    IF W_CodOpe GT 0 THEN DO:
        IF W_Rseleccion = 1 THEN
            RUN Movtos_PorOperAho.

        IF W_Rseleccion = 2 THEN
            RUN Movtos_PorOperCre.

        ASSIGN W_CodOpe = 0
               W_CodOpe:SCREEN-VALUE = "0".

        APPLY "CHOOSE" TO Btn_Impresion.

        RETURN.
    END.

    IF F_Buscar EQ "" THEN DO:
        MESSAGE "Debe digitarse la cuenta, pagare o Nit" SKIP
                "Al cual se le genera el extracto." SKIP(1)
                "Rectifique!"
            VIEW-AS ALERT-BOX.

        APPLY "entry" TO F_Buscar.
        RETURN NO-APPLY.
    END.

    IF W_Rseleccion <> 3 THEN DO:
        IF W_RProducto EQ 1 THEN DO:
            ASSIGN NitIni = "00000000000000"
                   NitFin = "99999999999999"
                   CueIni = F_Buscar
                   CueFin = F_Buscar.

            IF SUBSTRING(Cmb_Producto:SCREEN-VALUE,1,3) = "000" THEN DO:
                MESSAGE "Si la busqueda es por cuenta se debe" SKIP
                        "Escoger el producto en la cual esta matriculada" SKIP (1)
                        "Rectifique el producto al cual pertenece la cuenta!"
                    VIEW-AS ALERT-BOX.

                APPLY "entry" TO Cmb_Producto.

                RETURN NO-APPLY.
            END.

            IF W_RSeleccion EQ 1 THEN DO:
                FIND FIRST Ahorros WHERE Ahorros.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia,1,3))
                                     AND Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Producto,1,3))
                                     AND Ahorros.Cue_Ahorros EQ F_Buscar NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ahorros THEN DO:
                    MESSAGE "La Cuenta no existe para la Agencia o El producto" SKIP
                            "Rectifique!"
                        VIEW-AS ALERT-BOX.

                    APPLY "Entry" TO F_Buscar.

                    RETURN NO-APPLY.
                END.
                ELSE
                    RUN Extracto_Ahorros.
            END.

            IF W_RSeleccion EQ 2 THEN DO:
                FIND FIRST Creditos WHERE Creditos.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia,1,3))
                                      AND Creditos.Cod_Credito EQ INTEGER(SUBSTRING(Cmb_Producto,1,3))
                                      AND Creditos.Num_Credito EQ INTEGER(F_Buscar) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Creditos THEN DO:
                    MESSAGE "El Número de Credito no existe para la Agencia o El producto" SKIP
                            "Rectifique!"
                        VIEW-AS ALERT-BOX.

                    APPLY "Entry" TO F_Buscar.

                    RETURN NO-APPLY.
                END.
                ELSE
                    RUN Extracto_Creditos.
            END.
        END.
        ELSE DO:
            ASSIGN NitIni = F_Buscar
                   NitFin = F_Buscar
                   CueIni = "00000000000000"
                   CueFin = "99999999999999".

            FIND FIRST Clientes WHERE Clientes.Nit EQ F_Buscar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Clientes THEN DO:
                MESSAGE "El Cliente no Existe." SKIP
                        "Rectifique!"
                    VIEW-AS ALERT-BOX.

                APPLY "Entry" TO F_Buscar.

                RETURN NO-APPLY.
            END.

            IF W_RSeleccion EQ 1 THEN
                RUN Extracto_Ahorros.
            ELSE
                RUN Extracto_Creditos.
        END.
    END.
    ELSE DO:
        NitIni = F_Buscar.
        NitFin = F_Buscar.
    END.

    APPLY "CHOOSE" TO Btn_Impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia W-Win
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F-Main /* Agencia */
DO:
  ASSIGN FRAME F-Main Cmb_Agencia W_RProducto.
  ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencia,1,3))
         AgeFin = INTEGER(SUBSTRING(Cmb_Agencia,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Producto W-Win
ON VALUE-CHANGED OF Cmb_Producto IN FRAME F-Main /* Ahorros */
DO:
  ASSIGN FRAME F-Main Cmb_Producto.
  ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Producto,1,3))
         ProFin = INTEGER(SUBSTRING(Cmb_Producto,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Operaciones
&Scoped-define SELF-NAME T_Ope
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T_Ope W-Win
ON VALUE-CHANGED OF T_Ope IN FRAME F_Operaciones /* Sacar las Ultimas */
DO:
 DO WITH FRAME F_Operaciones:
  ASSIGN FRAME F_Operaciones T_Ope.
  IF T_Ope THEN DO: 
     ENABLE F_Ope.
     APPLY "entry" TO F_Ope.
  END.
  ELSE DO: 
     F_Ope:SCREEN-VALUE = "000".
     DISABLE F_Ope.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME W_CodOpe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodOpe W-Win
ON LEAVE OF W_CodOpe IN FRAME F-Main /* Cod_Operacion */
DO:
  ASSIGN W_CodOpe.

  IF W_CodOpe GT 0 THEN
     APPLY "Choose" TO BUTTON-125.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rproducto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rproducto W-Win
ON VALUE-CHANGED OF W_Rproducto IN FRAME F-Main
OR RETURN OF W_Rproducto DO:
 DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_RProducto W_RSeleccion Cmb_Agencia.
     IF W_Rproducto EQ 1 THEN DO:
        ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencia,1,3))
               AgeFin = INTEGER(SUBSTRING(Cmb_Agencia,1,3))
               NitIni = "00000000000000"
               NitFin = "99999999999999".
        IF W_RSeleccion EQ 1 THEN F_Buscar:LABEL = "Cuenta". 
        ELSE F_Buscar:LABEL = "Num.Crédito". 
        ENABLE Cmb_Agencia Cmb_Producto.
        APPLY "entry" TO Cmb_Agencia.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        ASSIGN AgeIni = 0
               AgeFin = 999
               ProIni = 0
               ProFin = 999
               CueIni = "00000000000000"
               CueFin = "99999999999999".
        F_Buscar:LABEL = "Nit".
        DISPLAY F_Buscar.
        DISABLE Cmb_Agencia Cmb_Producto.
        Cmb_Agencia:HIDDEN = YES.
        Cmb_Producto:HIDDEN = YES.
        APPLY "entry" TO F_Buscar.
        RETURN NO-APPLY.
     END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Rseleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Rseleccion W-Win
ON VALUE-CHANGED OF W_Rseleccion IN FRAME F-Main
OR RETURN OF W_Rseleccion OR TAB OF W_Rseleccion DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN W_Rseleccion W_RProducto.

        IF W_Rseleccion = 1 THEN DO:
            IF W_RProducto EQ 1 THEN
                F_Buscar:LABEL = "Cuenta".
            ELSE
                F_Buscar:LABEL = "Nit".

            ASSIGN Cmb_Producto:LIST-ITEMS = ""
                   Cmb_Producto:LABEL = "Ahorros".

            W_Ok = Image-1:LOAD-IMAGE("imagenes\Dinero.Bmp") NO-ERROR.

            Cmb_Producto:ADD-LAST("000 - Escoja el producto de Ahorros de la cuenta").

            FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK BREAK BY Pro_Ahorros.Cod_Ahorro:
                IF FIRST-OF(Pro_Ahorros.Cod_Ahorro) THEN
                    Cmb_Producto:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
            END.
        END.

        IF W_Rseleccion = 2 THEN DO:
            IF W_RProducto EQ 1 THEN
                F_Buscar:LABEL = "Pagare".
            ELSE
                F_Buscar:LABEL = "Nit".

            W_Ok = Image-1:LOAD-IMAGE("imagenes\DineroRojo.Bmp") NO-ERROR.

            ASSIGN Cmb_Producto:LIST-ITEMS = ""
                   Cmb_Producto:LABEL = "Créditos".

            Cmb_Producto:ADD-LAST("000 - Escoja el producto de Creditos del Pagare").

            FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK BREAK BY Pro_Creditos.Cod_Credito:
                IF FIRST-OF(Pro_Creditos.Cod_Credito) THEN
                    Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
            END.
        END.

        IF W_Rseleccion = 3 THEN DO:
            F_Buscar:LABEL = "Cédula".
            Cmb_Producto:SENSITIVE = FALSE.
            W_Rproducto:SENSITIVE = FALSE.
            Cmb_agencia:SENSITIVE = FALSE.
        END.
        ELSE DO:
            Cmb_Producto:SENSITIVE = TRUE.
            W_Rproducto:SENSITIVE = TRUE.
            Cmb_agencia:SENSITIVE = TRUE.
        END.

        ASSIGN Cmb_Producto:SCREEN-VALUE = Cmb_Producto:ENTRY(1).
    END.

    APPLY "VALUE-CHANGED" TO Cmb_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ASIGNA_LINEA W-Win 
PROCEDURE ASIGNA_LINEA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN P_linea = creditos.cod_credito.
CASE P_linea:
     WHEN 705 THEN ASSIGN P_linea = 5.
     WHEN 710 THEN ASSIGN P_linea = 10.
     WHEN 711 THEN ASSIGN P_linea = 11.
     WHEN 715 THEN ASSIGN P_linea = 15.
     WHEN 720 THEN ASSIGN P_linea = 20.
     WHEN 722 THEN ASSIGN P_linea = 22.
     WHEN 725 THEN ASSIGN P_linea = 25.
     WHEN 730 THEN ASSIGN P_linea = 30.
     WHEN 733 THEN ASSIGN P_linea = 33.
     WHEN 735 THEN ASSIGN P_linea = 35.
     WHEN 740 THEN ASSIGN P_linea = 40.
     WHEN 745 THEN ASSIGN P_linea = 45.
     WHEN 748 THEN ASSIGN P_linea = 48.
     WHEN 749 THEN ASSIGN P_linea = 49.
     WHEN 750 THEN ASSIGN P_linea = 50.
     WHEN 751 THEN ASSIGN P_linea = 51.
     WHEN 752 THEN ASSIGN P_linea = 52.
     WHEN 753 THEN ASSIGN P_linea = 53.
     WHEN 770 THEN ASSIGN P_linea = 70.
     WHEN 775 THEN ASSIGN P_linea = 75.
     WHEN 780 THEN ASSIGN P_linea = 80.
     WHEN 785 THEN ASSIGN P_linea = 85.
     WHEN 790 THEN ASSIGN P_linea = 90.
     WHEN 795 THEN ASSIGN P_linea = 95.
     WHEN 821 THEN ASSIGN P_linea = 521.
     WHEN 822 THEN ASSIGN P_linea = 522.
     WHEN 823 THEN ASSIGN P_linea = 523.
     WHEN 824 THEN ASSIGN P_linea = 524.
     WHEN 825 THEN ASSIGN P_linea = 525.
     WHEN 826 THEN ASSIGN P_linea = 526.
     WHEN 827 THEN ASSIGN P_linea = 527.
     WHEN 829 THEN ASSIGN P_linea = 529.
     WHEN 830 THEN ASSIGN P_linea = 530.
     WHEN 831 THEN ASSIGN P_linea = 531.
     WHEN 832 THEN ASSIGN P_linea = 532.
     WHEN 833 THEN ASSIGN P_linea = 533.
     WHEN 834 THEN ASSIGN P_linea = 534.
     WHEN 835 THEN ASSIGN P_linea = 535.
     WHEN 836 THEN ASSIGN P_linea = 536.
     WHEN 837 THEN ASSIGN P_linea = 537.
     WHEN 838 THEN ASSIGN P_linea = 538.
     WHEN 839 THEN ASSIGN P_linea = 539.
     WHEN 840 THEN ASSIGN P_linea = 540.
     WHEN 841 THEN ASSIGN P_linea = 541.
     WHEN 842 THEN ASSIGN P_linea = 542.
     WHEN 856 THEN ASSIGN P_linea = 556.
     WHEN 857 THEN ASSIGN P_linea = 557.
     WHEN 858 THEN ASSIGN P_linea = 558.
     WHEN 859 THEN ASSIGN P_linea = 559.
     WHEN 860 THEN ASSIGN P_linea = 560.
     WHEN 861 THEN ASSIGN P_linea = 561.
     WHEN 870 THEN ASSIGN P_linea = 570.
     WHEN 871 THEN ASSIGN P_linea = 571.
     WHEN 872 THEN ASSIGN P_linea = 572.
     WHEN 873 THEN ASSIGN P_linea = 573.
     WHEN 874 THEN ASSIGN P_linea = 574.
     WHEN 875 THEN ASSIGN P_linea = 575.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Operacion W-Win 
PROCEDURE Buscar_Operacion :
DEFINE INPUT  PARAMETER CodOpe LIKE Operacion.Cod_Operacion.
DEFINE OUTPUT PARAMETER NomOpe LIKE Operacion.Nom_Operacion.
DEFINE OUTPUT PARAMETER TipOpe LIKE Operacion.Tipo_Operacion.
NomOpe = "Operacion " + string(CodOpe) + " No Existe".
FIND Operacion WHERE Operacion.Cod_Operacion EQ CodOpe NO-LOCK NO-ERROR.
IF AVAILABLE Operacion THEN
   ASSIGN NomOpe = Operacion.Nom_Operacion
          TipOpe = Operacion.Tipo_Operacion.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY W_CodOpe AnoFin AnoIni F_Buscar W_Rseleccion W_Rproducto F_Nota 
          SExtracto 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE W_CodOpe BUTTON-120 BUTTON-121 BUTTON-123 BUTTON-125 F_Buscar 
         W_Rseleccion BUTTON-118 W_Rproducto Btn_Impresion Btn_Nota Cmb_Agencia 
         Cmb_Producto SExtracto Btn_Done Btn_Ayuda IMAGE-1 Img_MesF Img_MesI 
         RECT-15 RECT-217 RECT-282 RECT-283 RECT-284 RECT-285 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY T_Ope F_Ope 
      WITH FRAME F_Operaciones IN WINDOW W-Win.
  ENABLE BUTTON-124 T_Ope 
      WITH FRAME F_Operaciones IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Operaciones}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Encabezado W-Win 
PROCEDURE Encabezado :
DEFINE INPUT PARAMETER EncNit LIKE Clientes.Nit.

DEFINE VARIABLE Tipo_Extracto AS CHARACTER FORMAT "X(30)".
DEFINE VARIABLE Age_Actual AS CHARACTER FORMAT "X(30)".
DEFINE VARIABLE Age_Pertenece AS CHARACTER FORMAT "X(30)".

IF W_RSeleccion EQ 1 THEN 
   Tipo_Extracto = "                                                  EXTRACTO DE AHORRO".
ELSE 
   Tipo_Extracto = "                                                  EXTRACTO DE CREDITO".

FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
IF AVAILABLE Agencias THEN Age_Actual = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

DO WITH FRAME F-Main:
  SExtracto:LIST-ITEMS = "".
  FIND Clientes WHERE Clientes.Nit EQ EncNit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
     FIND Agencias WHERE Agencias.Agencia EQ Clientes.Agencia NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        Age_Pertenece = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     ASSIGN W_Ok = SExtracto:ADD-LAST(Tipo_Extracto)
            W_Ok = SExtracto:ADD-LAST("")
            W_Ok = SExtracto:ADD-LAST("Agencia  Genera  Extracto :  " + Age_Actual)
            W_Ok = SExtracto:ADD-LAST("Agencia  Origen  Cliente  :  " + Age_Pertenece)
            W_Ok = SExtracto:ADD-LAST("Identificación del Cliente:  " + TRIM(STRING(EncNit,"X(14)")) + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
            W_Ok = SExtracto:ADD-LAST("Telefono Residencia       :  " + STRING(Clientes.Tel_Residencia))
            W_Ok = SExtracto:ADD-LAST("Telefono Trabajo          :  " + STRING(Clientes.Tel_Comercial)).
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Encabezado_Ahorro W-Win 
PROCEDURE Encabezado_Ahorro :
DEFINE VAR WTas LIKE Ahorros.Tasa.
DO WITH FRAME F-Main: END.

FIND Pro_Ahorros WHERE 
     Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
IF AVAILABLE Pro_Ahorros THEN DO:
   IF Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
      FIND Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador NO-LOCK NO-ERROR.
      IF AVAILABLE Indicadores THEN WTas = Indicadores.Tasa * 100.
   END.
   ELSE WTas = Ahorros.Tasa * 100.

   WTas = Ahorros.Tasa.

   FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
   CREATE TImp.
   ASSIGN TImp.Reng = "Producto           : " + STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + STRING(Pro_Ahorros.Nom_Producto,"X(40)").

   CREATE TImp.       
   ASSIGN TImp.Reng = "Agencia de Apertura: " + STRING(Ahorros.Agencia,"999") + " - " + STRING(Agencias.Nombre,"X(20)").

   CREATE TImp.   
   IF Ahorros.TarjetaDB = " " THEN
       ASSIGN TImp.Reng = "Número de Cuenta   : " + STRING(Ahorros.Cue_Ahorros).
   ELSE
       ASSIGN TImp.Reng = "Número de Cuenta   : " + STRING(Ahorros.Cue_Ahorros) + "               TARJETA DEBITO: " + TRIM(Ahorros.tarjetaDB).

   CREATE TImp.       
   ASSIGN TImp.Reng = "Plazo              : " + STRING(Ahorros.Plazo).

   CREATE TImp.       
   ASSIGN TImp.Reng = "Tasa               : " + TRIM(STRING(WTas,">>9.99")) + "%".

   CREATE TImp.       
   ASSIGN TImp.Reng = "Fecha de Apertura  : " + STRING(Fec_Apertura,"99/99/9999").

   CREATE TImp.
   ASSIGN TImp.reng = "                                              Movimiento Desde 01/" + STRING(MesIni,"99") + "/" + STRING(AnoIni,"9999") + " Hasta " + STRING(P_FecFin,"99/99/9999").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Encabezado_Creditos W-Win 
PROCEDURE Encabezado_Creditos :
DEFINE VAR WTas LIKE Ahorros.Tasa.
DEFINE VAR Valor AS CHARACTER FORMAT "X(15)".

DO WITH FRAME F-Main:
END.

FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito
                          AND Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
IF AVAILABLE Pro_Creditos THEN DO:
    IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa NO-LOCK NO-ERROR.
        IF AVAILABLE Indicadores THEN
            WTas = Indicadores.Tasa * 100.
    END.
    ELSE
        WTas = Creditos.Tasa * 100.

    WTas = (Creditos.Tasa / 12).

    Valor = REPLACE(STRING(Creditos.Cuota,">>>,>>>,>>9"),",",".").
    
    FIND FIRST Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.

    CREATE TImp.
    ASSIGN TImp.reng = "Producto           : " + STRING(Pro_Creditos.Cod_Credito,"999") + " - " + STRING(Pro_Creditos.Nom_Producto,"X(40)").
    
    CREATE TImp.
    ASSIGN TImp.Reng = "Agencia de Apertura: " + STRING(Creditos.Agencia,"999") + " - " + STRING(Agencias.Nombre,"X(20)").

    CREATE TImp.
    ASSIGN TImp.Reng = "Número de crédito  : " + STRING(Creditos.Num_Credito).

    CREATE TImp.
    ASSIGN TImp.reng = "Pagaré #           : " + STRING(Creditos.Pagare).

    CREATE TImp.
    ASSIGN TImp.Reng = "Plazo              : " + STRING(Creditos.Plazo).

    CREATE TImp.
    ASSIGN TImp.reng = "Tasa Mensual       : " + TRIM(STRING(WTas,">>9.99")) + "%".

    IF creditos.cod_credito = 123 THEN DO:
        CREATE TImp.
        ASSIGN TImp.Reng = "Fecha límite pago  : " + STRING(Fec_pago,"99/99/9999").
    END.
    ELSE DO:
        CREATE TImp.       
        ASSIGN TImp.Reng = "Fecha de desembolso: " + STRING(Fec_Desembolso,"99/99/9999").
    END.

    CREATE TImp.
    ASSIGN TImp.reng = "Cuota del Credito  : " + TRIM(Valor).

    CREATE TImp.
    ASSIGN TImp.reng = "                                          Movimiento desde 01/" + STRING(MesIni,"99") + "/" + STRING(AnoIni,"9999") + " hasta " + STRING(P_FecFin,"99/99/9999").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Extracto_Ahorros W-Win 
PROCEDURE Extracto_Ahorros PRIVATE :
DEFINE VARIABLE TotalInteres AS INTEGER NO-UNDO.

IF ProIni = 3 AND ProFin = 3 THEN DO:
    FIND FIRST Ahorros WHERE Ahorros.Agencia GE AgeIni
                         AND Ahorros.Agencia LE AgeFin
                         AND Ahorros.Cod_Ahorro GE ProIni
                         AND Ahorros.Cod_Ahorro LE ProFin
                         AND Ahorros.Cue_Ahorros GE CueIni
                         AND Ahorros.Cue_Ahorros LE CueFin
                         AND Ahorros.Nit GE NitIni
                         AND Ahorros.Nit LE NitFin NO-LOCK NO-ERROR.
    IF AVAILABLE(ahorros) THEN
        RUN Extracto_TarjetaDB.

    RETURN.
END.

DEFINE VAR Enc AS LOGICAL INITIAL NO.
DEFINE VAR NomOpe AS CHARACTER FORMAT "X(35)".
DEFINE VAR TipOpe LIKE Operacion.Tipo_Operacion.
DEFINE VAR WDescrip AS CHARACTER FORMAT "X(35)".
DEFI VAR W_VrTx LIKE Mov_Ahorros.Val_Efectivo.
DEFI VAR TotDoc LIKE Mov_Ahorros.Sdo_Dispon.
DEFINE VAR TCon AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TRet AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SFin AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SIni AS DECIMAL FORMAT "->>>>>>,>>>,>>9".

EMPTY TEMP-TABLE TImp.
EMPTY TEMP-TABLE Imp.
    
DO WITH FRAME F-Main:
END.

FOR EACH Ahorros WHERE Ahorros.Agencia GE AgeIni
                   AND Ahorros.Agencia LE AgeFin
                   AND Ahorros.Cod_Ahorro GE ProIni
                   AND Ahorros.Cod_Ahorro LE ProFin
                   AND Ahorros.Cue_Ahorros GE CueIni
                   AND Ahorros.Cue_Ahorros LE CueFin
                   AND Ahorros.Nit GE NitIni
                   AND Ahorros.Nit LE NitFin NO-LOCK BREAK BY Ahorros.Nit
                                                           BY Ahorros.Cod_Ahorro
                                                           BY Ahorros.Cue_Ahorros:
    IF FIRST-OF(Ahorros.Nit) THEN
        RUN Encabezado (INPUT Ahorros.Nit).

    CREATE TImp.

    ASSIGN TImp.Reng = "*********************************************************************************************************************"
           TCon = 0
           TRet = 0.

    RUN Encabezado_Ahorro.

    CREATE TImp.
    ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------------------------------".

    CREATE TImp.
    ASSIGN TImp.Reng = " Fecha Operacion                                    N.Transc  Consignacion      Retiros   Sdo_Disp+Canje Of.O NitTran".

    CREATE TImp.
    ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------------------------------"
           P_FecIni = DATE(MesIni,01,AnoIni)
           SFin = Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.

    CREATE TImp.

    FIND FIRST Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia
                             AND Mov_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                             AND Mov_Ahorros.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                             AND Mov_Ahorros.Fecha GE P_FecIni
                             AND Mov_Ahorros.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
    IF AVAIL(Mov_Ahorro) THEN DO:
        RUN Buscar_Operacion(INPUT Mov_Ahorros.Cod_Operacion,
                             OUTPUT NomOpe,
                             OUTPUT TipOpe).

        IF TipOpe EQ 2 THEN
            SIni = Mov_Ahorros.Sdo_disponible + (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque).
        ELSE
            SIni = Mov_Ahorros.Sdo_disponible - (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque).

        ASSIGN TImp.Reng = "                                                                         Saldo Inicial $" + STRING(SIni,"->>>,>>>,>>>,>>9")
               SFin = SIni.
    END.
    ELSE DO:
        FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia
                               AND Mov_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                               AND Mov_Ahorros.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                               AND Mov_Ahorros.Fecha LT P_FecIni
                               AND Mov_Ahorros.Nit EQ Ahorros.Nit NO-LOCK BY Mov_Ahorros.Fecha DESCEND
                                                                          BY Mov_Ahorros.Hora DESCEND:
            ASSIGN TImp.Reng = "                                                                         Saldo Inicial $" + STRING(Mov_Ahorros.Sdo_disponible,"->>>,>>>,>>>,>>9")
                   SFin = Mov_Ahorros.Sdo_disponible.

            LEAVE.
        END.
    END.

    FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia
                           AND Mov_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                           AND Mov_Ahorros.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                           AND Mov_Ahorros.Fecha GE P_FecIni
                           AND Mov_Ahorros.Fecha LE P_FecFin
                           AND Mov_Ahorros.Nit EQ Ahorros.Nit NO-LOCK BY Mov_Ahorros.Fecha
                                                                      BY ROWID(mov_ahorros):
        IF (Mov_Ahorros.Cpte EQ 5) THEN DO:
            IF ((Mov_Ahorros.cod_ahorro EQ 3 OR mov_ahorros.cod_ahorro EQ 216) AND substring(Mov_Ahorros.descrip, 1, 9) NE "Dest CDAT") THEN
                NEXT.
        END.

        RUN Buscar_Operacion(INPUT Mov_Ahorros.Cod_Operacion,
                             OUTPUT NomOpe,
                             OUTPUT TipOpe).

        IF Mov_Ahorros.Descrip EQ "" THEN
            Linea = STRING(Mov_Ahorros.Fecha,"999999") + " " + STRING(NomOpe,"X(45)") + STRING(Mov_Ahorros.Num_Documento,"X(8)").
        ELSE
            ASSIGN WDescrip = REPLACE(Mov_Ahorros.Descrip,",",".")
                   Linea = STRING(Mov_Ahorros.Fecha,"999999") + " " + STRING(WDescrip,"X(45)") + STRING(Mov_Ahorros.Num_Documento,"X(8)").

        IF (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque) NE 0 THEN DO:
            IF TipOpe EQ 1 THEN
                ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                       Linea = Linea + "  " + STRING(W_VrTx,"->>>,>>>,>>9") + "              "
                       TCon = TCon + (Val_Efectivo + Val_Cheque).
            ELSE
                ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                       Linea = Linea + "               " + STRING(W_VrTx,"->>>,>>>,>>9") + " "
                       TRet = TRet + (Val_Efectivo + Val_Cheque).
            
            Linea = Linea + "  " + STRING(Mov_Ahorros.Sdo_disponible,"->,>>>,>>>,>>9") + STRING(Mov_Ahorros.Age_Fuente,">>999") + " " + TRIM(Mov_Ahorros.Cedula_Transac).

            CREATE TImp.
            ASSIGN TImp.Reng = Linea
                   SFin = Mov_Ahorros.Sdo_disponible.
        END.
    END.

    /* Acumula los intereses por cada Ahorro - William Martinez 22-12-2008 - Mod2. Nelson 06-01-2009 */
    TotalInteres = 0.

    ASSIGN TCon = 0
           TRet = 0
           TotalInteres = 0.

    FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia
                           AND Mov_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                           AND Mov_Ahorros.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                           AND Mov_Ahorros.Fecha GE P_FecIni
                           AND Mov_Ahorros.Fecha LE P_FecFin
                           AND Mov_Ahorros.Nit EQ Ahorros.Nit
                           AND Mov_Ahorros.Cpte EQ 5 NO-LOCK BY Mov_Ahorros.Fecha
                                                             BY Mov_Ahorros.Hora:
        IF (Mov_Ahorros.cod_ahorro NE 3 AND mov_ahorros.cod_ahorro NE 216) OR (substring(Mov_Ahorros.descrip, 1, 9) EQ "Dest CDAT") THEN
            NEXT.

        RUN Buscar_Operacion(INPUT Mov_Ahorros.Cod_Operacion,
                             OUTPUT NomOpe,
                             OUTPUT TipOpe).

        IF (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque) GT 0 THEN DO:
            IF TipOpe EQ 1 THEN
                TCon  = TCon + (Val_Efectivo + Val_Cheque).
            ELSE
                TRet = TRet + (Val_Efectivo + Val_Cheque).
        END.
    END.

    IF (TCon - TRet) GT 0 THEN DO:
        WDescrip = "Intereses liquidados periodo".
        Linea = "      " + " " + STRING(WDescrip,"X(45)") + STRING("        ","X(8)").
        Linea = Linea + "  " + STRING(TCon,">,>>>,>>>,>>9") + "              ".

        CREATE TImp.
        ASSIGN TImp.Reng = Linea.

        WDescrip = "Retención en la Fuente".
        Linea = "      " + " " + STRING(WDescrip,"X(45)") + STRING("        ","X(8)").
        Linea = Linea + "               " + STRING(TRet,">,>>>,>>>,>>9") + " ".

        CREATE TImp.
        ASSIGN TImp.Reng = Linea.
    END.

    IF P_FecFin GE W_Fecha THEN
        SFin = Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.

    CREATE TImp.
    ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------------------------------".

    CREATE TImp.
    ASSIGN TImp.Reng = "                                       Total Movimientos :  " + STRING(TCon,">>>>>>,>>>,>>9") + " " + STRING(TRet,">>>,>>>,>>>,>>9").

    CREATE Timp.

    CREATE TImp.
    ASSIGN TImp.Reng = "                                                                            Saldo Final $" + STRING(SFin,"->>,>>>,>>>,>>9").

    CREATE TImp.
    ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------------------------------".

    CREATE TImp.
END.

CREATE TImp.
ASSIGN TImp.Reng = "Nota: " + F_Nota.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Extracto_Creditos W-Win 
PROCEDURE Extracto_Creditos :
DEFINE VAR Enc AS LOGICAL.
DEFINE VAR SFin AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SIni AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

EMPTY TEMP-TABLE TImp.
EMPTY TEMP-TABLE Imp.

DO WITH FRAME F-Main:
END.

FOR EACH Creditos WHERE Creditos.Agencia >= AgeIni
                    AND Creditos.Agencia <= AgeFin
                    AND Creditos.Nit >= NitIni
                    AND Creditos.Nit <= NitFin
                    AND Creditos.Num_Credito >= DECIMAL(CueIni)
                    AND Creditos.Num_Credito <= DECIMAL(CueFin)
                    AND Creditos.Cod_Credito >= ProIni
                    AND Creditos.Cod_Credito <= ProFin NO-LOCK BREAK BY Creditos.Agencia
                                                                     BY Creditos.Nit
                                                                     BY Creditos.Num_Credito:
    IF FIRST-OF(Creditos.Nit) THEN
        RUN Encabezado (INPUT Creditos.Nit).

    CREATE TImp.
    TImp.Reng = "***************************************************************************************************************************".
    
    RUN Encabezado_Creditos.

    CREATE TImp.
    TImp.Reng = "------------------------------------------------------------------------------------------------------------------------".

    CREATE TImp.
    TImp.Reng = "  FECHA   |       DETALLE DE LA OPERACIÓN          |# TRANS | AVANCE/DES | PAGO/ABONO |TOTAL TRANSAC|SDO_CAPITAL|OF-CPTE".

    CREATE TImp.
    ASSIGN TImp.Reng = "------------------------------------------------------------------------------------------------------------------------"
           TCon = 0
           TRet = 0.

    RUN P_Movtos (OUTPUT SFin).

    CREATE TImp.
    TImp.Reng = "------------------------------------------------------------------------------------------------------------------------".

    CREATE TImp.
    TImp.Reng = "                                           Total Movimientos:" + STRING(TRet,"$->>,>>>,>>9") + " " + STRING(TCon,"$->>,>>>,>>9").

    CREATE TImp.

    CREATE TImp.
    TImp.Reng = "                                                                                         Saldo Final: " + STRING(SFin,"$>>>,>>>,>>9").

    CREATE TImp.
END.

CREATE TImp.      
ASSIGN TImp.Reng = "Nota: " + F_Nota. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Extracto_tarjetaDB W-Win 
PROCEDURE Extracto_tarjetaDB :
DEFINE VAR wclave_tmp AS CHARACTER FORMAT "X(25)".
DEFINE VAR zswini AS LOGICAL INITIAL FALSE.
DEFINE VAR wclave_ant AS CHARACTER FORMAT "X(25)" INITIAL "XXXXXXXX".
DEFINE VAR wAcum AS DECIMAL FORMAT "->>>>>>,>>>,>>9" INITIAL 0.0.
DEFINE VAR Enc AS LOGICAL INITIAL NO.
DEFINE VAR NomOpe AS CHARACTER FORMAT "X(35)".
DEFINE VAR TipOpe LIKE Operacion.Tipo_Operacion.
DEFINE VAR WDescrip AS CHARACTER FORMAT "X(35)".
DEFI VAR W_VrTx LIKE Mov_Ahorros.Val_Efectivo.
DEFI VAR TotDoc LIKE Mov_Ahorros.Sdo_Dispon.
DEFI VAR W_SiAho AS LOGICAL INITIAL FALSE.
DEFINE VAR TCon AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TRet AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SFin AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SIni AS DECIMAL FORMAT "->>>>>>,>>>,>>9".

EMPTY TEMP-TABLE TImp.
EMPTY TEMP-TABLE Imp.

DO WITH FRAME F-Main:
END.

FOR EACH Ahorros WHERE Ahorros.Agencia GE AgeIni
                   AND Ahorros.Agencia LE AgeFin
                   AND Ahorros.Cod_Ahorro GE ProIni
                   AND Ahorros.Cod_Ahorro LE ProFin
                   AND Ahorros.Cue_Ahorros GE CueIni
                   AND Ahorros.Cue_Ahorros LE CueFin
                   AND Ahorros.Nit GE NitIni
                   AND Ahorros.Nit LE NitFin NO-LOCK BREAK BY Ahorros.Nit
                                                           BY Ahorros.Cod_Ahorro
                                                           BY Ahorros.Cue_Ahorros:
    IF FIRST-OF(Ahorros.Nit) THEN
        RUN Encabezado (INPUT Ahorros.Nit).

    CREATE TImp.
    ASSIGN TImp.Reng = "=========================================================================================================================".
           
    TCon = 0.
    TRet = 0.

    RUN Encabezado_Ahorro.

    CREATE TImp.
    ASSIGN TImp.Reng = "___________________________________________________________________________________________________________________________".

    CREATE TImp.
    ASSIGN TImp.Reng = "   FECHA  DOCUMENTO #AUDITOR        DESCRIPCION                              CONSIGNACION      RETIROS  SDO_DISP+CANJE Of.O".
    
    CREATE TImp.
    ASSIGN TImp.Reng = "___________________________________________________________________________________________________________________________".

    P_FecIni  = DATE(MesIni,01,AnoIni).
    SFin = Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.

    CREATE TImp.

    EMPTY TEMP-TABLE ext_TarDB.
    
    FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia
                           AND Mov_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                           AND Mov_Ahorros.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                           AND Mov_Ahorros.Fecha LT P_FecIni
                           AND Mov_Ahorros.Nit EQ Ahorros.Nit NO-LOCK BY Mov_Ahorros.Fecha DESCEND
                                                                      BY Mov_Ahorros.Hora DESCEND:
        ASSIGN TImp.Reng = "                                                               Saldo Inicial $" + STRING(Mov_Ahorros.Sdo_disponible,"->>>>>>,>>>,>>9")
               SFin = Mov_Ahorros.Sdo_disponible.

        wacum = sfin.
        zswini = TRUE.

        LEAVE.
    END.

    FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia
                           AND Mov_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                           AND Mov_Ahorros.Cue_Ahorros EQ Ahorros.Cue_Ahorros
                           AND Mov_Ahorros.Fecha GE P_FecIni
                           AND Mov_Ahorros.Fecha LE P_FecFin
                           AND Mov_Ahorros.Nit EQ Ahorros.Nit NO-LOCK BY Mov_Ahorros.Fecha
                                                                      BY Mov_Ahorros.Hora:
        IF wacum = 0 THEN
            wacum = mov_ahorros.sdo_disponible.

        wclave_tmp = SUBSTRING(mov_ahorros.nro_auditoria,9,4) + STRING(mov_ahorros.fecha).

        IF wclave_tmp NE wclave_ant THEN DO:
            FOR EACH ext_TarDB:
                ASSIGN WDescrip = REPLACE(ext_TarDB.Descrip,",",".")
                       Linea = STRING(ext_TarDB.Fecha,"99/99/9999") + " " + STRING(ext_TarDB.Num_Documento,"X(8)") + " " + STRING(ext_TarDB.nro_auditoria,"X(12)") + " " + STRING(WDescrip,"X(41)").

                IF ext_TarDB.val_consigna > 0 THEN
                    ASSIGN Linea = Linea + "  " + STRING(val_consigna,">>>>,>>>,>>9") + "              "
                           TCon = TCon + val_consigna.
                ELSE
                    ASSIGN Linea  = Linea + "               " + STRING(val_retiro,">>>>,>>>,>>9") + " "
                           TRet = TRet + Val_retiro.

                wacum = mov_ahorros.sdo_disponible.

                IF zswini THEN
                    ASSIGN /*Wacum = Wacum + ext_TarDB.val_consigna - ext_TarDB.val_retiro*/
                           Linea = Linea + "  " + STRING(Wacum,"->>>>,>>>,>>9") + STRING(ext_TarDB.Age_Fuente,">>999").
                ELSE
                    ASSIGN Linea = Linea + "  " + STRING(Wacum,"->>>>,>>>,>>9") + STRING(ext_TarDB.Age_Fuente,">>999")
                           zswini = TRUE.

                CREATE TImp.
                ASSIGN TImp.Reng = Linea
                       SFin = wacum.
            END.

            EMPTY TEMP-TABLE ext_tarDB.
                     
            wclave_ant = wclave_tmp.
        END.

        RUN Buscar_Operacion(INPUT Mov_Ahorros.Cod_Operacion,
                             OUTPUT NomOpe,
                             OUTPUT TipOpe).

        IF Mov_Ahorros.Descrip EQ "" THEN
            Linea = STRING(Mov_Ahorros.Fecha,"99/99/9999") + " " + STRING(Mov_Ahorros.Num_Documento,"X(8)" + " " + STRING(Mov_Ahorros.nro_auditoria,"X(12)") + " " + STRING(NomOpe,"X(41)")).
        ELSE
            ASSIGN WDescrip = REPLACE(Mov_Ahorros.Descrip,",",".")
                   Linea = STRING(Mov_Ahorros.Fecha,"99/99/9999") + " " + STRING(Mov_Ahorros.Num_Documento,"X(8)") + " " + STRING(Mov_Ahorros.nro_auditoria,"X(12)") + " " + STRING(WDescrip,"X(41)").

        IF (SUBSTRING(Mov_Ahorros.Descrip,1,3) EQ "CA " OR SUBSTRING(Mov_Ahorros.Descrip,1,3) EQ "CB ") OR SUBSTRING(Mov_Ahorros.Descrip,1,6) EQ "GMF - "  THEN DO:
            IF SUBSTRING(Mov_Ahorros.Descrip,1,3) EQ "CA " OR SUBSTRING(Mov_Ahorros.Descrip,1,3) EQ "CB " THEN
                FIND FIRST ext_tarDB WHERE ext_tarDB.nro_auditoria EQ mov_ahorros.nro_auditoria
                                       AND ext_tarDB.fecha EQ mov_ahorros.fecha
                                       AND ext_tarDB.num_documento EQ mov_ahorros.num_documento
                                       AND ext_tarDB.cpte EQ mov_ahorros.cpte
                                       AND (SUBSTRING(ext_tarDB.Descrip,1,3) EQ "CA " OR SUBSTRING(ext_tarDB.Descrip,1,3) EQ "CB ") NO-LOCK NO-ERROR.
            ELSE
                FIND FIRST ext_tarDB WHERE ext_tarDB.nro_auditoria EQ mov_ahorros.nro_auditoria
                                       AND ext_tarDB.fecha EQ mov_ahorros.fecha
                                       AND ext_tarDB.num_documento EQ mov_ahorros.num_documento
                                       AND ext_tarDB.cpte EQ mov_ahorros.cpte
                                       AND SUBSTRING(ext_tarDB.Descrip,1,6) EQ "GMF - "NO-LOCK NO-ERROR.
                
            IF NOT AVAILABLE(ext_tarDB) THEN DO:
                CREATE ext_tarDB.
                ASSIGN ext_tarDB.nro_auditoria = mov_ahorros.nro_auditoria
                       ext_tarDB.fecha = mov_ahorros.fecha
                       ext_tarDB.num_documento = mov_ahorros.num_documento
                       ext_tarDB.cpte = mov_ahorros.cpte
                       ext_tarDB.descrip = mov_ahorros.descrip
                       ext_tarDB.age_fuente = mov_ahorros.age_fuente.

                IF TipOpe EQ 1 THEN
                    ASSIGN ext_tarDB.val_consigna = mov_ahorros.val_efectivo
                           ext_tarDB.val_retiro = 0.
                ELSE
                    ASSIGN ext_tarDB.val_retiro = mov_ahorros.val_efectivo
                           ext_tarDB.val_consigna = 0.
            END.
            ELSE DO:
                IF TipOpe EQ 1 THEN
                    ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                           ext_tarDB.val_consigna = ext_tarDB.val_consigna + W_VrTx.
                ELSE
                    ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                           ext_tarDB.val_retiro = ext_tarDB.val_retiro + W_VrTx.
            END.
        END.
        ELSE DO:
            IF (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque) GT 0 THEN DO:
                IF TipOpe EQ 1 THEN
                    ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                           Linea = Linea + "  " + STRING(W_VrTx,">>>>,>>>,>>9") + "              "
                           TCon = TCon   + (Val_Efectivo + Val_Cheque)
                           wacum = wacum + (Val_Efectivo + Val_Cheque).
                ELSE
                    ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                           Linea = Linea + "               " + STRING(W_VrTx,">>>>,>>>,>>9") + " "
                           TRet = TRet  + (Val_Efectivo + Val_Cheque)
                           wacum = wacum - (Val_Efectivo + Val_Cheque).

                IF NOT zswini THEN DO:
                    IF TipOpe EQ 1 THEN
                        wacum = wacum - (Val_Efectivo + Val_Cheque).
                    ELSE
                        wacum = wacum + (Val_Efectivo + Val_Cheque).

                    zswini = TRUE.
                END.
            END.

            wacum = mov_ahorros.sdo_disponible.

            Linea = Linea + "  " + STRING(wacum,"->>>>,>>>,>>9") + STRING(Mov_Ahorros.Age_Fuente,">>999").

            CREATE TImp.
            ASSIGN TImp.Reng = Linea
                   SFin = wacum.
        END.
    END.

    FOR EACH ext_TarDB:
        ASSIGN WDescrip = REPLACE(ext_TarDB.Descrip,",",".")
               Linea = STRING(ext_TarDB.Fecha,"99/99/9999") + " " + STRING(ext_TarDB.Num_Documento,"X(8)") + " " + STRING(ext_TarDB.nro_auditoria,"X(12)") + " " + STRING(WDescrip,"X(41)").

        IF ext_TarDB.val_consigna > 0 THEN
            ASSIGN Linea = Linea + "  " + STRING(val_consigna,">>>>,>>>,>>9") + "              "
                   TCon = TCon + val_consigna.
        ELSE
            ASSIGN Linea = Linea + "               " + STRING(val_retiro,">>>>,>>>,>>9") + " "
                   TRet = TRet + Val_retiro.

        wacum = wacum + ext_TarDB.val_consigna - ext_TarDB.val_retiro.

        Linea = Linea + "  " + STRING(Wacum ,"->>>>,>>>,>>9") + STRING(ext_TarDB.Age_Fuente,">>999").

        CREATE TImp.
        ASSIGN TImp.Reng = Linea
               SFin = wacum.
    END.

    EMPTY TEMP-TABLE ext_TarDB.

    IF P_FecFin GE W_Fecha THEN
        SFin = Ahorros.Sdo_disponible + Ahorros.Sdo_Canje.

    CREATE TImp.
    ASSIGN TImp.Reng = "________________________________________________________________________________________________________________________".

    CREATE TImp.
    ASSIGN TImp.Reng = "                            Total Movimientos :                         " + STRING(TCon,">>>>>>,>>>,>>9") + " " + STRING(TRet,">>>>>>,>>>,>>9").

    CREATE TImp.
    ASSIGN TImp.Reng = "                                                                                          Saldo Final $" + STRING(SFin,"->>>>>,>>>,>>9").

    CREATE TImp.
    ASSIGN TImp.Reng = "________________________________________________________________________________________________________________________".
    
    CREATE TImp.
END.

CREATE TImp.
ASSIGN TImp.Reng = "Nota: " + F_Nota.

/*Mayo 30/06 Gaer, Extractos sin cta-ahorros */
FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Nit GE NitIni
                       AND Mov_Ahorros.Nit LE NitFin
                       AND Mov_Ahorros.Cod_Ahorro GE ProIni
                       AND Mov_Ahorros.Cod_Ahorro LE ProFin NO-LOCK BREAK BY Mov_Ahorros.Nit
                                                                          BY Mov_Ahorros.Cue_Ahorros
                                                                          BY Mov_Ahorros.Fecha
                                                                          BY Mov_Ahorros.Hora:
    IF FIRST-OF(Mov_Ahorros.Cue_Ahorros) THEN
        FIND FIRST Ahorros WHERE Ahorros.Agencia EQ Mov_Ahorros.Agencia
                             AND Ahorros.Cod_Ahorro EQ Mov_Ahorros.Cod_Ahorro
                             AND Ahorros.Cue_Ahorros EQ Mov_Ahorros.Cue_Ahorros
                             AND Ahorros.Nit EQ Mov_Ahorros.Nit NO-LOCK NO-ERROR.
    
    IF AVAIL(Ahorros) THEN
        NEXT.    /*Continua solo para los que no existe la Cta-Ahorros*/

    IF FIRST-OF(Mov_Ahorros.Cue_Ahorros) THEN DO:
        RUN Encabezado (INPUT Mov_Ahorros.Nit).

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Mov_Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.

        CREATE TImp.
        ASSIGN TImp.Reng = "Producto   : " + STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + STRING(Pro_Ahorros.Nom_Producto,"X(40)").

        CREATE TImp.
        ASSIGN TImp.Reng = "Num.Cuenta : " + STRING(Mov_Ahorros.Cue_Ahorros) + "   Tarjeta Debito : " + ahorros.TarjetaDB.

        CREATE TImp.
        ASSIGN TImp.Reng = "========================================================================================================================"
               TCon = 0
               TRet = 0.

        CREATE TImp.
        ASSIGN TImp.Reng = "___________________________________________________________________________________________________________________________".
        
        CREATE TImp.
        ASSIGN TImp.Reng = "   FECHA  DOCUMENTO #AUDITOR        DESCRIPCION                              CONSIGNACION      RETIROS  SDO_DISP+CANJE Of.O".
        
        CREATE TImp.
        ASSIGN TImp.Reng = "___________________________________________________________________________________________________________________________".

        P_FecIni = DATE(MesIni,01,AnoIni).
        SFin = 0.

        CREATE TImp.
    END.

    RUN Buscar_Operacion(INPUT Mov_Ahorros.Cod_Operacion,
                         OUTPUT NomOpe,
                         OUTPUT TipOpe).

    IF Mov_Ahorros.Descrip EQ "" THEN
        Linea = STRING(Mov_Ahorros.Fecha,"99/99/9999") + " " + STRING(Mov_Ahorros.Num_Documento,"X(8)") + " " + STRING(Mov_Ahorros.Nro_auditoria,"X(12)") + " " + STRING(NomOpe,"X(41)").
    ELSE
        ASSIGN WDescrip = REPLACE(Mov_Ahorros.Descrip,",",".")
               Linea = STRING(Mov_Ahorros.Fecha,"99/99/9999") + " " + STRING(Mov_Ahorros.Num_Documento,"X(8)") + " " + STRING(Mov_Ahorros.Nro_auditoria,"X(12)") + " " + STRING(WDescrip,"X(41)").

    IF (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque) NE 0 THEN DO:
        IF TipOpe EQ 1 THEN
            ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                   Linea = Linea + "  " + STRING(W_VrTx,"->>>>,>>>,>>9") + "              "
                   TCon = TCon + (Val_Efectivo + Val_Cheque).
        ELSE
            ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                   Linea = Linea + "               " + STRING(W_VrTx,"->>>>,>>>,>>9") + " "
                   TRet = TRet + (Val_Efectivo + Val_Cheque).
    END.

    Linea = Linea + "  " + STRING(Mov_Ahorros.Sdo_disponible,"->>>>,>>>,>>9") + STRING(Mov_Ahorros.Age_Fuente,">>999").

    CREATE TImp.
    ASSIGN TImp.Reng = Linea
           SFin = Mov_Ahorros.Sdo_disponible.

    IF LAST-OF(Mov_Ahorros.Cue_Ahorros) THEN DO:
        CREATE TImp.
        ASSIGN TImp.Reng = "________________________________________________________________________________________________________________________".

        CREATE TImp.
        ASSIGN TImp.Reng = "                            Total Movimientos :                         " + STRING(TCon,"->>>>>>,>>>,>>9") + " " + STRING(TRet,"->>>>>>,>>>,>>9").

        CREATE TImp.
        ASSIGN TImp.Reng = "                                                                                      Saldo Final $ 0.00".

        CREATE TImp.
        ASSIGN TImp.Reng = "________________________________________________________________________________________________________________________".

        CREATE TImp.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirMovimientos W-Win 
PROCEDURE ImprimirMovimientos :
{INCLUIDO\RepEncabezado.I}

DEFINE VAR totalDB AS DECIMAL.
DEFINE VAR totalCR AS DECIMAL.

EMPTY TEMP-TABLE cuentasCreditos.
EMPTY TEMP-TABLE cuentasAhorros.

W_Reporte = "REPORTE   : Auxiliares por Línea " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = " NUM_DOC  FECHA    ENLACE    DOC_REF    SALDO INICIAL          DÉBITOS         CRÉDITOS      SALDO FINAL DESCRIPCION/DETALLE".

DEFINE VAR TotDb AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotCr AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

W_Linea = FILL(W_Raya,170).

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

FOR EACH pro_ahorros NO-LOCK:
    FOR EACH cortoLargo WHERE cortoLargo.clase_producto = 1
                          AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro
                          AND cortoLargo.agencia = 1 NO-LOCK:
        FIND FIRST cuentasAhorros WHERE cuentasAhorros.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentasAhorros THEN DO:
            CREATE cuentasAhorros.
            ASSIGN cuentasAhorros.cuenta = cortoLargo.cta_AsoAd.

            FOR EACH TT_Movimientos WHERE TT_Movimientos.tipo = "AH"
                                      AND TT_Movimientos.cuenta = cortoLargo.cta_AsoAd
                                      AND TT_Movimientos.db + TT_Movimientos.cr > 0 NO-LOCK BY TT_Movimientos.consecutivo:
                IF TT_Movimientos.consecutivo = 1 THEN
                    DISPLAY pro_ahorros.nom_producto FORMAT "X(30)" NO-LABEL "-" TT_Movimientos.cuenta FORMAT "X(20)" NO-LABEL.
                
                DISPLAY TT_Movimientos.num_doc FORMAT ">>>>>>>>"
                        TT_Movimientos.fecha
                        TT_Movimientos.enlace FORMAT "X(8)"
                        TT_Movimientos.doc_ref FORMAT "X(8)"
                        TT_Movimientos.sdo_inicial FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.db FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.cr FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.sdo_final FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.detalle FORMAT "X(50)"
                    WITH FRAME FMvtoAH WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

                ASSIGN TotDb = TotDb + TT_Movimientos.Db
                       TotCr = TotCr + TT_Movimientos.Cr.
            END.
        END.
    END.

    IF totDB + totCR > 0 THEN DO:
        DISPLAY "                                                       ______________   ______________" SKIP
                TotDb AT 53 FORMAT ">>,>>>,>>>,>>9.99"
                TotCr AT 70 FORMAT ">>,>>>,>>>,>>9.99" SKIP(1)
            WITH FRAME FtotdcAH WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    END.

    ASSIGN totalDB = totalDB + totDB
           TotDb = 0
           totalCR = totalCR + totCR
           TotCr = 0.
END.

FOR EACH pro_creditos NO-LOCK:
    FOR EACH cortoLargo WHERE cortoLargo.clase_producto = 2
                          AND cortoLargo.cod_producto = pro_creditos.cod_credito
                          AND cortoLargo.agencia = 1 NO-LOCK:
        FIND FIRST cuentasCreditos WHERE cuentasCreditos.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentasCreditos THEN DO:
            CREATE cuentasCreditos.
            ASSIGN cuentasCreditos.cuenta = cortoLargo.cta_AsoAd.

            FOR EACH TT_Movimientos WHERE TT_Movimientos.tipo = "CR"
                                      AND TT_Movimientos.cuenta = cortoLargo.cta_AsoAd
                                      AND TT_Movimientos.db + TT_Movimientos.cr > 0 NO-LOCK BY TT_Movimientos.consecutivo:
                IF TT_Movimientos.consecutivo = 1 THEN
                    DISPLAY pro_creditos.nom_producto FORMAT "X(30)" NO-LABEL "-" TT_Movimientos.cuenta FORMAT "X(20)" NO-LABEL.

                DISPLAY TT_Movimientos.num_doc FORMAT ">>>>>>>>"
                        TT_Movimientos.fecha
                        TT_Movimientos.enlace FORMAT "X(8)"
                        TT_Movimientos.doc_ref FORMAT "X(8)"
                        TT_Movimientos.sdo_inicial FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.db FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.cr FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.sdo_final FORMAT "$->>>,>>>,>>9.99"
                        TT_Movimientos.detalle FORMAT "X(50)"
                    WITH FRAME FMvtoCR WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

                ASSIGN TotDb = TotDb + TT_Movimientos.Db
                       TotCr = TotCr + TT_Movimientos.Cr.
            END.
        END.

        IF totDB + totCR > 0 THEN DO:
            DISPLAY "                                                       ______________   ______________" SKIP
                    TotDb AT 53 FORMAT ">>,>>>,>>>,>>9.99"
                    TotCr AT 70 FORMAT ">>,>>>,>>>,>>9.99" SKIP(1)
                WITH FRAME FtotdcCR WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.

        ASSIGN totalDB = totalDB + totDB
               TotDb = 0
               totalCR = totalCR + totCR
               TotCr = 0.

        IF cortoLargo.cta_AsoAd <> cortoLargo.cta_NoaAd THEN DO:
            FIND FIRST cuentasCreditos WHERE cuentasCreditos.cuenta = cortoLargo.cta_NoaAd NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cuentasCreditos THEN DO:
                CREATE cuentasCreditos.
                ASSIGN cuentasCreditos.cuenta = cortoLargo.cta_NoaAd.

                FOR EACH TT_Movimientos WHERE TT_Movimientos.tipo = "CR"
                                          AND TT_Movimientos.cuenta = cortoLargo.cta_NoaAd
                                          AND TT_Movimientos.db + TT_Movimientos.cr > 0 NO-LOCK BY TT_Movimientos.consecutivo:
                    IF TT_Movimientos.consecutivo = 1 THEN
                        DISPLAY pro_creditos.nom_producto FORMAT "X(30)" NO-LABEL "-" TT_Movimientos.cuenta FORMAT "X(20)" NO-LABEL.

                    DISPLAY TT_Movimientos.num_doc FORMAT ">>>>>>>>"
                            TT_Movimientos.fecha
                            TT_Movimientos.enlace FORMAT "X(8)"
                            TT_Movimientos.doc_ref FORMAT "X(8)"
                            TT_Movimientos.sdo_inicial FORMAT "$->>>,>>>,>>9.99"
                            TT_Movimientos.db FORMAT "$->>>,>>>,>>9.99"
                            TT_Movimientos.cr FORMAT "$->>>,>>>,>>9.99"
                            TT_Movimientos.sdo_final FORMAT "$->>>,>>>,>>9.99"
                            TT_Movimientos.detalle FORMAT "X(50)"
                        WITH FRAME FMvtoCR2 WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

                    ASSIGN TotDb = TotDb + TT_Movimientos.Db
                           TotCr = TotCr + TT_Movimientos.Cr.
                END.
            END.

            IF totDB + totCR > 0 THEN DO:
                DISPLAY "                                                       ______________   ______________" SKIP
                        TotDb AT 53 FORMAT ">>,>>>,>>>,>>9.99"
                        TotCr AT 70 FORMAT ">>,>>>,>>>,>>9.99" SKIP(1)
                    WITH FRAME FtotdcCR2 WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            END.

            ASSIGN totalDB = totalDB + totDB
                   TotDb = 0
                   totalCR = totalCR + totCR
                   TotCr = 0.
        END.
    END.
END.

FOR EACH TT_Movimientos WHERE TT_Movimientos.tipo = "CONT"
                          AND TT_Movimientos.db + TT_Movimientos.cr > 0 NO-LOCK BREAK BY TT_Movimientos.cuenta
                                                                                      BY TT_Movimientos.consecutivo:
    IF FIRST-OF(TT_Movimientos.cuenta) THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = TT_Movimientos.cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas THEN
            DISPLAY cuentas.nombre FORMAT "X(30)" NO-LABEL "-" cuentas.cuenta FORMAT "X(20)" NO-LABEL.
    END.

    DISPLAY TT_Movimientos.num_doc FORMAT ">>>>>>>>"
            TT_Movimientos.fecha
            TT_Movimientos.enlace FORMAT "X(8)"
            TT_Movimientos.doc_ref FORMAT "X(8)"
            TT_Movimientos.sdo_inicial FORMAT "$->>>,>>>,>>9.99"
            TT_Movimientos.db FORMAT "$->>>,>>>,>>9.99"
            TT_Movimientos.cr FORMAT "$->>>,>>>,>>9.99"
            TT_Movimientos.sdo_final FORMAT "$->>>,>>>,>>9.99"
            TT_Movimientos.detalle FORMAT "X(50)"
        WITH FRAME FMvtoCONT WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

    ASSIGN TotDb = TotDb + TT_Movimientos.Db
           TotCr = TotCr + TT_Movimientos.Cr.

    IF LAST-OF(TT_Movimientos.cuenta) THEN DO:
        IF totDB + totCR > 0 THEN DO:
            DISPLAY "                                                       ______________   ______________" SKIP
                    TotDb AT 53 FORMAT ">>,>>>,>>>,>>9.99"
                    TotCr AT 70 FORMAT ">>,>>>,>>>,>>9.99" SKIP(1)
                WITH FRAME FtotdcCONT WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

            ASSIGN totalDB = totalDB + totDB
                   TotDb = 0
                   totalCR = totalCR + totCR
                   TotCr = 0.
        END.
    END.
END.

DISPLAY "                                              ______________   ______________" SKIP
        totalDB AT 44 FORMAT ">>,>>>,>>>,>>9.99"
        totalCR AT 61 FORMAT ">>,>>>,>>>,>>9.99" SKIP(1)
    WITH FRAME FtotdcTOTAL WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

ASSIGN TotDb = 0
       TotCr = 0.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK:
         ASSIGN W_Metodo = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + STRING(Agencias.Nombre,"X(16)")).    
         IF Agencias.Agencia EQ W_Agencia THEN
            ASSIGN Cmb_Agencia:SCREEN-VALUE = Cmb_Agencia:ENTRY(W_Agencia).
     END.
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ).
     ASSIGN W_Rproducto:SCREEN-VALUE = "2"
            F_Buscar:LABEL = "Nit"
            Cmb_Producto:LIST-ITEMS  = ""
            Cmb_Producto:LABEL       = "Ahorros".

     ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
            W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
            AnoIni:SCREEN-VALUE = STRING(YEAR(TODAY))
            AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
            MesIni = MONTH(TODAY)
            MesFin = MONTH(TODAY)
            AgeIni = 000
            AgeFin = 999
            ProIni = 000
            ProFin = 999
            P_FecFin = W_Fecha.

     Cmb_Producto:ADD-LAST("000 - Escoja el producto de Ahorros de la cuenta").
     FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK
                          BREAK BY Pro_Ahorros.Cod_Ahorro:
         IF FIRST-OF(Pro_Ahorros.Cod_Ahorro) THEN DO:
            Cmb_Producto:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
         END.
     END.
     ASSIGN Cmb_Producto:SCREEN-VALUE = Cmb_Producto:ENTRY(1).
     APPLY "VALUE-CHANGED" TO Cmb_Agencia.

     APPLY "ENTRY" TO F_Buscar.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movtos_PorOperAho W-Win 
PROCEDURE Movtos_PorOperAho :
DEFINE VAR Enc AS LOGICAL INITIAL NO.
DEFINE VAR NomOpe AS CHARACTER FORMAT "X(35)".
DEFINE VAR TipOpe LIKE Operacion.Tipo_Operacion.
DEFINE VAR WDescrip AS CHARACTER FORMAT "X(35)".
DEFI VAR W_VrTx LIKE Mov_Ahorros.Val_Efectivo.
DEFI VAR TotDoc LIKE Mov_Ahorros.Sdo_Dispon.
DEFINE VAR TCon AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TRet AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SFin AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SIni AS DECIMAL FORMAT "->>>>>>,>>>,>>9".

EMPTY TEMP-TABLE TImp.
EMPTY TEMP-TABLE Imp.
    
DO WITH FRAME F-Main:
END.

CREATE TImp.
ASSIGN TImp.Reng = "--------------------------------------------------------------------------------------------".

CREATE TImp.
ASSIGN TImp.Reng = "Ag.FechaOp Ced/Nit    Cue_ahorros N.Transc  Consignacion      Retiros   Sdo_Disp+Canje Of.O".

CREATE TImp.
ASSIGN TImp.Reng = "--------------------------------------------------------------------------------------------"
       P_FecIni = DATE(MesIni,01,AnoIni)
       SFin = 0.

FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia GE AgeIni
                       AND Mov_Ahorros.Agencia LE AgeFin
                       AND Mov_Ahorros.Cod_ahorro EQ ProIni
                       AND Mov_Ahorros.Fecha GE P_FecIni
                       AND Mov_Ahorros.Fecha LE P_FecFin
                       AND Mov_Ahorros.Cod_Oper EQ W_CodOpe NO-LOCK BY Mov_Ahorros.Fecha
                                                                    BY Mov_Ahorros.Hora:
    RUN Buscar_Operacion(INPUT Mov_Ahorros.Cod_Operacion,
                         OUTPUT NomOpe,
                         OUTPUT TipOpe).

    Linea = STRING(Mov_Ahorros.Agenc,"999") + " " + STRING(Mov_Ahorros.Fecha,"999999") + " " + STRING(Mov_Ahorros.Nit,"X(12)") + " " + STRING(Mov_Ahorros.Cue_Ahorro,"X(12)") + " " + STRING(Mov_Ahorros.Num_Documento,"X(8)").

    IF (Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque) GT 0 THEN DO:
        IF TipOpe EQ 1 THEN
            ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                   Linea = Linea + "  " + STRING(W_VrTx,">>>>,>>>,>>9") + "              "
                   TCon = TCon   + (Val_Efectivo + Val_Cheque).
        ELSE
            ASSIGN W_VrTx = Mov_Ahorros.Val_Efectivo + Mov_Ahorros.Val_Cheque
                   Linea = Linea + "               " + STRING(W_VrTx,">>>>,>>>,>>9") + " "
                   TRet = TRet   + (Val_Efectivo + Val_Cheque).
    END.

    Linea = Linea + "  " + STRING(Mov_Ahorros.Sdo_disponible,"->>>>,>>>,>>9") + STRING(Mov_Ahorros.Age_Fuente,">>999").

    CREATE TImp.
    ASSIGN TImp.Reng = Linea.
END.

CREATE TImp.
ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------".

CREATE TImp.
ASSIGN TImp.Reng = "                             Total Movimientos :  " + STRING(TCon,">>>>>>,>>>,>>9") + " " + STRING(TRet,">>>>>>,>>>,>>9").

CREATE TImp.
ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------".

CREATE TImp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movtos_PorOperCre W-Win 
PROCEDURE Movtos_PorOperCre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR Enc    AS LOGICAL INITIAL NO.
DEFINE VAR NomOpe AS CHARACTER FORMAT "X(35)".
DEFINE VAR TipOpe LIKE Operacion.Tipo_Operacion.
DEFINE VAR WDescrip AS CHARACTER FORMAT "X(35)".
DEFI   VAR W_VrTx   LIKE Mov_Ahorros.Val_Efectivo.
DEFI   VAR TotDoc   LIKE Mov_Ahorros.Sdo_Dispon.

DEFINE VAR TCon   AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TRet   AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SFin   AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR SIni   AS DECIMAL FORMAT "->>>>>>,>>>,>>9".

FOR EACH TImp: DELETE TImp. END.
FOR EACH Imp:  DELETE Imp.  END.

DO WITH FRAME F-Main: END.

   CREATE TImp.      
   ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------".

   CREATE TImp.      
   ASSIGN TImp.Reng = "Ag. FechaOp Ced/Nit      No.Pagaré  N.Transc  Consignacion      Retiros   Saldo Final    Of.O".

   CREATE TImp.      
   ASSIGN TImp.Reng = "----------------------------------------------------------------------------------------------"
          P_FecIni  = DATE(MesIni,01,AnoIni)
          SFin      = 0.
   
   FOR EACH Mov_Creditos WHERE 
             Mov_Creditos.Agencia       GE AgeIni AND
             Mov_Creditos.Agencia       LE AgeFin AND
             Mov_Creditos.Cod_Credito   EQ ProIni AND
             Mov_Creditos.Fecha        GE P_FecIni             AND
             Mov_Creditos.Fecha        LE P_FecFin             AND
             Mov_Creditos.Cod_Oper     EQ W_CodOpe NO-LOCK 
          BY Mov_Creditos.Fecha BY Mov_Creditos.Hora:
      
      RUN Buscar_Operacion(INPUT Mov_Creditos.Cod_Operacion,OUTPUT NomOpe, OUTPUT TipOpe).
      
      
      Linea = STRING(Mov_Creditos.Agenc,"999") + " " + STRING(Mov_Creditos.Fecha,"999999") + " " + STRING(Mov_Creditos.Nit,"X(12)") + " " 
              + STRING(Mov_Creditos.Pagare,"X(12)") 
              + " " + STRING(Mov_Creditos.Num_Documento,"X(8)").
      
      IF (Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque) GT 0 THEN DO:         
         IF TipOpe EQ 1 THEN
            ASSIGN W_VrTx = Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque
                   Linea  = Linea + "  " + STRING(W_VrTx,">>>>,>>>,>>9") + "              "         
                   TCon   = TCon   + (Val_Efectivo + Val_Cheque).
         ELSE
            ASSIGN W_VrTx = Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque
                   Linea  = Linea + "               " + STRING(W_VrTx,">>>>,>>>,>>9") + " "
                   TRet   = TRet   + (Val_Efectivo + Val_Cheque).                   
      END.

      Linea = Linea + "  " + STRING(Mov_Creditos.Sdo_Capital,"->>>>,>>>,>>9") + STRING(Mov_Creditos.Ofi_Fuente,">>999").

      CREATE TImp.
      ASSIGN TImp.Reng = Linea.             
   END.
      
   CREATE TImp.
   ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------".
   
   CREATE TImp.      
   ASSIGN TImp.Reng = "                             Total Movimientos :  " + 
                      STRING(TCon,">>>>>>,>>>,>>9") + " " + STRING(TRet,">>>>>>,>>>,>>9").

   CREATE TImp.       
   ASSIGN TImp.Reng = "---------------------------------------------------------------------------------------------".

   CREATE TImp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
DEFINE VAR ProIni AS INTEGER FORMAT "999".
DEFINE VAR ProFin AS INTEGER FORMAT "999".
DEFINE VAR TipSel AS INTEGER FORMAT "9".
DEFINE VAR RanIni AS CHARACTER FORMAT "X(14)".
DEFINE VAR RanFin AS CHARACTER FORMAT "X(14)".
DEFINE VAR FecIni AS DATE FORMAT "99/99/9999".
DEFINE VAR FecFin AS DATE FORMAT "99/99/9999".
DEFINE VAR Produc AS INTEGER FORMAT "999".
DEFINE VAR Nota AS CHARACTER FORMAT "X(82)".
DEFINE VAR pSaldoInicial AS DECIMAL.
DEFINE VAR mesAnterior AS INTEGER.
DEFINE VAR cont AS INTEGER.
DEFINE VAR cont1 AS INTEGER.
DEFINE VAR pNaturaleza AS CHARACTER.

EMPTY TEMP-TABLE TT_Movimientos.
EMPTY TEMP-TABLE cuentasProductos.
EMPTY TEMP-TABLE cuentasCreditos.

mesAnterior = MONTH(P_FecIni) - 1.

IF mesAnterior = 0 THEN
    mesAnterior = 12.

DO WITH FRAME {&FRAME-NAME}:
    IF W_Rseleccion = 1 OR W_Rseleccion = 2 THEN DO:
        FOR EACH Imp:
            DISPLAY Lin AT 1 FORM "X(150)"
                WITH FRAME FC WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.

        FOR EACH TImp:
            DISPLAY Reng AT 1 FORM "X(150)"
                WITH FRAME Fdet WIDTH 180 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.
    END.

    IF W_Rseleccion = 3 THEN DO:
        /* Ahorros */
        FOR EACH pro_ahorros NO-LOCK:
            pSaldoInicial = 0.

            FOR EACH cortoLargo WHERE cortoLargo.clase_producto = 1
                                  AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro
                                  AND cortoLargo.agencia = 1 NO-LOCK:
                FIND FIRST cuentasAhorros WHERE cuentasAhorros.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentasAhorros THEN DO:
                    CREATE cuentasAhorros.
                    ASSIGN cuentasAhorros.cuenta = cortoLargo.cta_AsoAd.

                    cont = 0.

                    FIND FIRST ahorros WHERE ahorros.cod_ahorro = pro_ahorros.cod_ahorro
                                         AND ahorros.nit = NitIni
                                         AND ahorros.plazo >= plazo_inicial
                                         AND ahorros.plazo <= plazo_final NO-LOCK NO-ERROR.
                    IF AVAILABLE ahorros THEN DO:
                        FOR EACH ahorros WHERE ahorros.Cod_Ahorro EQ pro_ahorros.Cod_Ahorro
                                           AND ahorros.nit EQ NitIni
                                           AND ahorros.plazo >= plazo_inicial
                                           AND ahorros.plazo <= plazo_final NO-LOCK:
                            pSaldoInicial = pSaldoInicial + ahorros.sdo_Anuales[mesAnterior].
                        END.

                        FOR EACH mov_ahorros WHERE mov_ahorros.nit = NitIni
                                               AND mov_ahorros.fecha >= P_FecIni
                                               AND mov_ahorros.fecha <= P_FecFin
                                               AND mov_ahorros.cod_ahorro = pro_ahorros.cod_ahorro NO-LOCK BY mov_ahorros.fecha
                                                                                                           BY mov_ahorros.hora:
                            IF cont = 0 THEN DO:
                                FIND FIRST cuentas WHERE cuentas.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
                                IF AVAILABLE cuentas THEN DO:
                                    CREATE TT_Movimientos.
                                    ASSIGN TT_Movimientos.cuenta = cuentas.cuenta + " - " + cuentas.nombre.
                                END.
                            END.

                            CREATE TT_Movimientos.
                            ASSIGN TT_Movimientos.tipo = "AH"
                                   TT_Movimientos.consecutivo = cont + 1
                                   TT_Movimientos.cuenta = cortoLargo.cta_AsoAd
                                   TT_Movimientos.num_doc = INTEGER(mov_ahorros.num_documento)
                                   TT_Movimientos.enlace = mov_ahorros.cue_ahorros
                                   TT_Movimientos.fecha = mov_ahorros.fecha
                                   TT_Movimientos.sdo_inicial = pSaldoInicial
                                   TT_Movimientos.detalle = mov_ahorros.descrip.

                            FIND FIRST cuentasProductos WHERE cuentasProductos.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE cuentasProductos THEN DO:
                                CREATE cuentasProductos.
                                ASSIGN cuentasProductos.cuenta = cortoLargo.cta_AsoAd.
                            END.

                            FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                            IF AVAILABLE operacion THEN DO:
                                IF operacion.tipo_operacion = 1 THEN
                                    TT_Movimientos.cr = mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                            
                                IF operacion.tipo_operacion = 2 THEN
                                    TT_Movimientos.db = mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                            END.

                            TT_Movimientos.sdo_final = pSaldoInicial + TT_Movimientos.cr - TT_Movimientos.db.
                            pSaldoInicial = TT_Movimientos.sdo_final.
                            cont = cont + 1.
                        END.
                    END.
                END.
            END.
        END.

        /* Créditos */
        FOR EACH pro_creditos NO-LOCK:
            pSaldoInicial = 0.

            FOR EACH cortoLargo WHERE cortoLargo.clase_producto = 2
                                  AND cortoLargo.cod_producto = pro_creditos.cod_credito
                                  AND cortoLargo.agencia = 1 NO-LOCK:
                FIND FIRST cuentasCreditos WHERE cuentasCreditos.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentasCreditos THEN DO:
                    CREATE cuentasCreditos.
                    ASSIGN cuentasCreditos.cuenta = cortoLargo.cta_AsoAd.

                    cont = 0.
                    
                    FOR EACH creditos WHERE creditos.Cod_credito EQ pro_creditos.Cod_credito
                                       AND creditos.nit EQ NitIni NO-LOCK:
                        pSaldoInicial = pSaldoInicial + creditos.sdo_Anuales[mesAnterior].
                    END.
                    
                    FOR EACH Mov_Contable WHERE Mov_Contable.Cuenta = cortoLargo.cta_AsoAd
                                            AND Mov_Contable.Fec_Contable >= P_FecIni
                                            AND Mov_Contable.Fec_Contable <= p_fecFin
                                            AND mov_contable.nit = NitIni NO-LOCK BREAK BY mov_contable.cuenta
                                                                                        BY Mov_Contable.fec_contable
                                                                                        BY ROWID(mov_contable):
                        IF FIRST-OF(mov_contable.cuenta) THEN DO:
                            FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
                            IF AVAILABLE cuentas THEN DO:
                                CREATE TT_Movimientos.
                                ASSIGN TT_Movimientos.cuenta = cuentas.cuenta + " - " + cuentas.nombre.
                            END.
                        END.
                    
                        CREATE TT_Movimientos.
                        ASSIGN TT_Movimientos.tipo = "CR"
                               TT_Movimientos.consecutivo = cont + 1
                               TT_Movimientos.cuenta = mov_contable.cuenta
                               TT_Movimientos.num_doc = mov_contable.num_documento
                               TT_Movimientos.fecha = mov_contable.fec_contable
                               TT_Movimientos.doc_ref = mov_contable.doc_ref
                               TT_Movimientos.enlace = mov_contable.enlace
                               TT_Movimientos.sdo_inicial = pSaldoInicial
                               TT_Movimientos.DB = mov_contable.db
                               TT_Movimientos.CR = mov_contable.cr
                               TT_Movimientos.sdo_final = pSaldoInicial - mov_contable.cr + mov_contable.db
                               TT_Movimientos.detalle = mov_contable.comentario.
                    
                        FIND FIRST cuentasProductos WHERE cuentasProductos.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE cuentasProductos THEN DO:
                            CREATE cuentasProductos.
                            ASSIGN cuentasProductos.cuenta = cortoLargo.cta_AsoAd.
                        END.
                    
                        cont = cont + 1.
                    
                        pSaldoInicial = pSaldoInicial - mov_contable.cr + mov_contable.db.
                    END.
                END.

                IF cortoLargo.cta_AsoAd <> cortoLargo.cta_NoaAd THEN DO:
                    FIND FIRST cuentasCreditos WHERE cuentasCreditos.cuenta = cortoLargo.cta_NoaAd NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentasCreditos THEN DO:
                        CREATE cuentasCreditos.
                        ASSIGN cuentasCreditos.cuenta = cortoLargo.cta_NoaAd.

                        cont = 0.

                        FOR EACH Mov_Contable WHERE Mov_Contable.Cuenta = cortoLargo.cta_NoaAd
                                                AND Mov_Contable.Fec_Contable >= P_FecIni
                                                AND Mov_Contable.Fec_Contable <= p_fecFin
                                                AND mov_contable.nit = NitIni NO-LOCK BREAK BY mov_contable.cuenta
                                                                                            BY Mov_Contable.fec_contable
                                                                                            BY ROWID(mov_contable):
                            IF FIRST-OF(mov_contable.cuenta) THEN DO:
                                FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
                                IF AVAILABLE cuentas THEN DO:
                                    CREATE TT_Movimientos.
                                    ASSIGN TT_Movimientos.cuenta = cuentas.cuenta + " - " + cuentas.nombre.
                                END.
                            END.
                    
                            CREATE TT_Movimientos.
                            ASSIGN TT_Movimientos.tipo = "CR"
                                   TT_Movimientos.consecutivo = cont + 1
                                   TT_Movimientos.cuenta = mov_contable.cuenta
                                   TT_Movimientos.num_doc = mov_contable.num_documento
                                   TT_Movimientos.fecha = mov_contable.fec_contable
                                   TT_Movimientos.doc_ref = mov_contable.doc_ref
                                   TT_Movimientos.enlace = mov_contable.enlace
                                   TT_Movimientos.sdo_inicial = pSaldoInicial
                                   TT_Movimientos.DB = mov_contable.db
                                   TT_Movimientos.CR = mov_contable.cr
                                   TT_Movimientos.sdo_final = pSaldoInicial - mov_contable.cr + mov_contable.db
                                   TT_Movimientos.detalle = mov_contable.comentario.
                    
                            FIND FIRST cuentasProductos WHERE cuentasProductos.cuenta = cortoLargo.cta_NoaAd NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE cuentasProductos THEN DO:
                                CREATE cuentasProductos.
                                ASSIGN cuentasProductos.cuenta = cortoLargo.cta_NoaAd.
                            END.
                    
                            cont = cont + 1.
                    
                            pSaldoInicial = pSaldoInicial - mov_contable.cr + mov_contable.db.
                        END.
                    END.
                END.
            END.
        END.

        /* Cuentas contables */
        FOR EACH mov_contable WHERE mov_contable.nit = NitIni
                                AND mov_contable.fec_contable >= p_FecIni
                                AND mov_contable.fec_contable <= p_FecFin BREAK BY mov_contable.cuenta:
            FIND FIRST cuentasProductos WHERE cuentasProductos.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
            IF AVAILABLE cuentasProductos THEN
                NEXT.

            IF FIRST-OF(mov_contable.cuenta) THEN DO:
                pSaldoInicial = 0.

                FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
                IF AVAILABLE cuentas THEN DO:
                    CREATE TT_Movimientos.
                    ASSIGN TT_Movimientos.cuenta = cuentas.cuenta + " - " + cuentas.nombre.
                END.

                FOR EACH anexos WHERE anexos.cuenta = mov_contable.cuenta
                                  AND anexos.ano = YEAR(p_fecIni) NO-LOCK:
                    FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
                    IF AVAILABLE cuentas THEN DO:
                        pSaldoInicial = anexos.sdo_inicial.
                        pNaturaleza = cuentas.naturaleza.
                    END.

                    IF MONTH(p_fecIni) > 1 THEN DO:
                        DO cont1 = 1 TO MONTH(p_fecIni) - 1:
                            IF pNaturaleza = "DB" THEN
                                pSaldoInicial = pSaldoInicial + anexos.db[cont1] - anexos.cr[cont1].
                            ELSE
                                pSaldoInicial = pSaldoInicial - anexos.db[cont1] + anexos.cr[cont1].
                        END.
                    END.
                END.
            END.

            /*FIND FIRST TT_Movimientos WHERE TT_Movimientos.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TT_Movimientos THEN DO:*/
                CREATE TT_Movimientos.
                ASSIGN TT_Movimientos.tipo = "CONT"
                       TT_Movimientos.consecutivo = cont + 1
                       TT_Movimientos.cuenta = mov_contable.cuenta
                       TT_Movimientos.num_doc = mov_contable.num_documento
                       TT_Movimientos.fecha = mov_contable.fec_contable
                       TT_Movimientos.doc_ref = mov_contable.doc_ref
                       TT_Movimientos.enlace = mov_contable.enlace
                       TT_Movimientos.sdo_inicial = pSaldoInicial
                       TT_Movimientos.DB = mov_contable.db
                       TT_Movimientos.CR = mov_contable.cr
                       TT_Movimientos.detalle = mov_contable.comentario.

                cont = cont + 1.

                IF pNaturaleza = "DB" THEN
                    TT_Movimientos.sdo_final = pSaldoInicial - mov_contable.cr + mov_contable.db.
                ELSE
                    TT_Movimientos.sdo_final = pSaldoInicial + mov_contable.cr - mov_contable.db.

                pSaldoInicial = TT_Movimientos.sdo_final.
            /*END.*/
        END.

        RUN ImprimirMovimientos.

        MESSAGE "Desea exportar el informe a Excel?"
            VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Exportar a Excel" UPDATE flagExportarExcel AS LOGICAL.

        IF flagExportarExcel = TRUE THEN DO:
            OUTPUT TO VALUE("C:\INFO_Fodun\MovimientoExportado_" + w_usuario + "_" + REPLACE(STRING(w_fecha),"/","") + ".csv").
                EXPORT DELIMITER ";" "CUENTA"
                                     "NUM_DOC"
                                     "FECHA"
                                     "ENLACE"
                                     "DOC_REFERENCIA"
                                     "SDO_INICIAL"
                                     "DB"
                                     "CR"
                                     "SDO_FINAL"
                                     "DETALLE".

                FOR EACH TT_Movimientos NO-LOCK BREAK /*BY TT_Movimientos.cuenta*/:
                    EXPORT DELIMITER ";" TT_Movimientos.cuenta
                                         TT_Movimientos.num_doc
                                         TT_Movimientos.fecha
                                         TT_Movimientos.enlace
                                         TT_Movimientos.doc_ref
                                         TT_movimientos.sdo_inicial
                                         TT_Movimientos.db
                                         TT_Movimientos.cr
                                         TT_movimientos.sdo_final
                                         TT_Movimientos.detalle.
                END.
            OUTPUT CLOSE.

            MESSAGE "El informe fue exportado a la siguiente ubicación:" SKIP
                    "C:\Info_Fodun\"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE P_Movtos W-Win 
PROCEDURE P_Movtos :
DEFI OUTPUT PARAMETER P_SdoFin AS DECIMAL.

DEFINE VAR NomOpe AS CHARACTER FORMAT "X(35)".
DEFINE VAR TipOpe AS INTEGER.
DEFINE VAR WDescrip AS CHARACTER.
DEFI VAR TotDoc AS DECIMAL.
DEFINE VAR pFecha AS CHARACTER.
DEFINE VAR pSdoCapital AS CHARACTER.
DEFINE VAR pCpteAg AS CHARACTER.
DEFINE VAR pNumDoc AS CHARACTER.

P_FecIni = DATE(MesIni,01,AnoIni).

RUN Asigna_linea.

FOR EACH Mov_Creditos WHERE Mov_Creditos.Agencia = Creditos.Agencia
                        AND (Mov_Creditos.Cod_Credito = p_linea OR Mov_Creditos.Cod_Credito = creditos.cod_credito)
                        AND Mov_Creditos.Num_Credito = Creditos.Num_Credito
                        AND Mov_Creditos.Fecha < P_FecIni
                        AND Mov_Creditos.Nit = Creditos.Nit NO-LOCK BREAK BY Mov_Creditos.Fecha DESCEND
                                                                          BY Mov_Creditos.Hora DESCEND:
    P_SdoFin = Mov_Creditos.Sdo_Capital.  /*Inicia con este, en caso de no hay movimientos*/
    LEAVE.
END.

FOR EACH Mov_Creditos WHERE Mov_Creditos.Agencia = Creditos.Agencia
                        AND (Mov_Creditos.Cod_Credito = p_linea OR Mov_Creditos.Cod_Credito = creditos.cod_credito)
                        AND Mov_Creditos.Num_Credito = Creditos.Num_Credito
                        AND Mov_Creditos.Fecha >= P_FecIni
                        AND Mov_Creditos.Fecha <= P_FecFin
                        AND Mov_Creditos.Nit = Creditos.Nit NO-LOCK BREAK BY Mov_Creditos.Fecha
                                                                          /*BY Mov_Creditos.Cpte*/
                                                                          BY Mov_Creditos.Hora
                                                                          BY Mov_Creditos.Num_Documento:
    pSdoCapital = "".
    pFecha = "".
    pCpteAg = "".
    pNumDoc = "".
    
    IF FIRST-OF(mov_creditos.num_documento) THEN
        ASSIGN pFecha = STRING(mov_creditos.fecha,"99/99/9999")
               pCpteAg = STRING(mov_creditos.agencia,"99") + "-" + STRING(mov_creditos.cpte,"99")
               pNumDoc = STRING(mov_creditos.num_documento).
    
    IF LAST-OF(mov_creditos.num_documento) THEN
        pSdoCapital = STRING(mov_creditos.sdo_capital,"$->>>,>>>,>>9").
    
        
               
    RUN Buscar_Operacion(INPUT Mov_Creditos.Cod_Operacion,
                         OUTPUT NomOpe,
                         OUTPUT TipOpe).

    IF Mov_Creditos.Descrip <> "" THEN
        ASSIGN WDescrip = REPLACE(Mov_Creditos.Descrip,",",".")
               Linea = STRING(pFecha,"X(10)") + "| " + STRING(WDescrip,"X(39)") + "|" + STRING(pNumDoc,"X(8)") + "|".
    ELSE
        Linea = STRING(pFecha,"X(10)") + "| " + STRING(NomOpe,"X(39)") + "|" + STRING(pNumDoc,"X(8)") + "|".

    IF TipOpe = 2 THEN
        ASSIGN Linea = Linea + STRING(Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque,"$->>,>>>,>>9") + "|            |"
               TRet = TRet + (Val_Efectivo + Val_Cheque)
               TotDoc = TotDoc - (Val_Efectivo + Val_Cheque).
    ELSE
        ASSIGN Linea = Linea + "            |" + STRING(Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque,"$->>,>>>,>>9") + "|"
               TCon = TCon + (Val_Efectivo + Val_Cheque)
               TotDoc = TotDoc + (Val_Efectivo + Val_Cheque).

    ASSIGN Linea = Linea + (IF LAST-OF(Mov_Creditos.Num_Documento) THEN STRING(TotDoc,"$->>,>>>,>>9") + "|" ELSE "            |")
                         + STRING(pSdoCapital,"X(13)") + " |"
                         + STRING(pCpteAg,"X(5)").

    P_SdoFin = Mov_Creditos.Sdo_Capital.

    IF mov_creditos.cod_operacion = 999999999 THEN
        Linea = STRING(pFecha,"X(10)") + "| " + STRING(WDescrip,"X(100)").


    CREATE TImp.
    TImp.Reng = Linea.

    IF LAST-OF(Mov_Creditos.Num_Documento) THEN DO:
        CREATE TImp.
        ASSIGN TImp.Reng = FILL("-",120)
               /*TImp.Reng = FILL(" ",52) + FILL("-",66)*/
               TotDoc = 0.
    END.
END.

DELETE TImp.

IF P_FecFin GE W_Fecha THEN
    P_SdoFin = Creditos.Sdo_Capital.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

