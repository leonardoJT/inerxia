&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

/* Connected Databases 
          bdcentral        PROGRESS
*/

&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 

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

/* oakley */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli LIKE Clientes.Nit.
DEFINE INPUT PARAMETER P_CodCre LIKE Creditos.Cod_Credito.
DEFINE INPUT PARAMETER P_TipCre LIKE Creditos.Tip_Credito.
DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER P_CodOpe AS CHARACTER FORMAT "X(9)".
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

DEFINE VAR W_Canje  LIKE Bancos.Dia_Canje.
DEFINE VAR W_ValChe LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValEfe LIKE Creditos.Sdo_Capital.

DEFINE VAR W_Ok AS LOGICAL.

DEFINE VAR P_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VrCapital LIKE Creditos.Sdo_Capital.

DEFINE VAR W_Cbte      LIKE Comprobantes.Comprobante.
DEFINE VAR W_CtaCorCre LIKE Cuentas.Cuenta.
DEFINE VAR W_Caja      LIKE Cuentas.Cuenta.
DEFINE VAR W_NumCbt    LIKE Comprobantes.Secuencia.
DEFINE VAR W_CtaSya_Des    LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaSya_Fte    LIKE Cuentas.Cuenta.

DEFINE VARIABLE W_NumSeq      AS INTEGER FORMAT "9999999".
DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Cre

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Creditos

/* Definitions for FRAME F_Cre                                          */
&Scoped-define FIELDS-IN-QUERY-F_Cre Creditos.Fec_Desembolso ~
Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion Creditos.Int_MorCobrar ~
Creditos.Fec_UltPago Creditos.Int_Corrientes Creditos.Fec_UltLiquidacion ~
Creditos.Int_Anticipado Creditos.Int_DifCobro Creditos.Sdo_Proyectado ~
Creditos.Sdo_Capital Creditos.Cuo_Pagadas Creditos.Tasa Creditos.Val_Atraso ~
Creditos.Cuota Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision ~
Creditos.Fec_Reestructurado 
&Scoped-define QUERY-STRING-F_Cre FOR EACH Creditos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cre OPEN QUERY F_Cre FOR EACH Creditos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cre Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cre Creditos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-296 RECT-297 R_TipCon W_AboMor ~
Btn_Grabar Btn_Salir 
&Scoped-Define DISPLAYED-FIELDS Creditos.Fec_Desembolso Creditos.Fec_Pago ~
Creditos.Fec_ProxLiquidacion Creditos.Int_MorCobrar Creditos.Fec_UltPago ~
Creditos.Int_Corrientes Creditos.Fec_UltLiquidacion Creditos.Int_Anticipado ~
Creditos.Int_DifCobro Creditos.Sdo_Proyectado Creditos.Sdo_Capital ~
Creditos.Cuo_Pagadas Creditos.Tasa Creditos.Val_Atraso Creditos.Cuota ~
Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision ~
Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS R_TipCon W_AboMor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar Operación" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE W_AboMor AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Abono a Mora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NumBan AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NumChe AS CHARACTER FORMAT "X(10)":U 
     LABEL "Número de Cheque" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_TipCon AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Efectivo", 1,
"Cheque", 2
     SIZE 24 BY 1.08
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 11.31.

DEFINE RECTANGLE RECT-297
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 8.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Cre FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cre
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Aprobación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     R_TipCon AT ROW 2.35 COL 66 NO-LABEL
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_ProxLiquidacion AT ROW 3.69 COL 14 COLON-ALIGNED
          LABEL "Prox. Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_MorCobrar AT ROW 3.69 COL 44 COLON-ALIGNED
          LABEL "Interés de Mora"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 14 FGCOLOR 0 
     W_AboMor AT ROW 3.69 COL 72 COLON-ALIGNED
     Creditos.Fec_UltPago AT ROW 4.5 COL 14 COLON-ALIGNED
          LABEL "Último Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 4.77 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NumChe AT ROW 4.77 COL 72 COLON-ALIGNED
     Creditos.Fec_UltLiquidacion AT ROW 5.31 COL 14 COLON-ALIGNED
          LABEL "Última Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Anticipado AT ROW 5.85 COL 44 COLON-ALIGNED
          LABEL "Interés Anticipado"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NumBan AT ROW 5.85 COL 72 COLON-ALIGNED
     Creditos.Int_DifCobro AT ROW 6.12 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 6.92 COL 14 COLON-ALIGNED
          LABEL "Saldo Proyectado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 6.92 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Grabar AT ROW 7.19 COL 74
     Creditos.Cuo_Pagadas AT ROW 7.73 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 8 COL 44 COLON-ALIGNED
          LABEL "Tasa"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Val_Atraso AT ROW 8.54 COL 14 COLON-ALIGNED
          LABEL "Valor Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Cuota AT ROW 9.08 COL 44 COLON-ALIGNED
          LABEL "Cuota"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salir AT ROW 9.08 COL 74
     Creditos.Dias_Atraso AT ROW 9.35 COL 14 COLON-ALIGNED
          LABEL "Días de Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Cuo_Atraso AT ROW 10.15 COL 14 COLON-ALIGNED
          LABEL "Cuotas Atrasadas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Provision AT ROW 10.96 COL 14 COLON-ALIGNED
          LABEL "Provisión"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 12.12
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     Creditos.Fec_Reestructurado AT ROW 11.77 COL 14 COLON-ALIGNED
          LABEL "Reestructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información del Crédito" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 1.15 COL 3
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 1.54 COL 2
     RECT-297 AT ROW 2.08 COL 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 12.12
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
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         COLUMN             = 3.43
         ROW                = 10.27
         HEIGHT             = 12.12
         WIDTH              = 97.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Wwin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Cre
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_ProxLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_MorCobrar IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_NumBan IN FRAME F_Cre
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NumBan:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR FILL-IN W_NumChe IN FRAME F_Cre
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NumChe:HIDDEN IN FRAME F_Cre           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _TblList          = "bdCentral.Creditos"
     _Query            is OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Cre /* Grabar Operación */
DO:
    vTime = TIME.

 DO WITH FRAME F_Cre:
   ASSIGN FRAME F_Cre W_AboMor R_TipCon W_NumChe W_NumBan.
   
      
   IF R_TipCon EQ 2 AND (W_NumChe EQ "" OR W_NumBan EQ 0) THEN DO:
      MESSAGE "Debe digitarse el número del cheque y del banco. Rectifique!"
               VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO W_NumChe.
      RETURN NO-APPLY.
   END.
   IF R_TipCon EQ 2 THEN DO: /*verifica que exista el banco en BANCOS*/
      W_Canje = 0.
      FIND Bancos WHERE Bancos.Cod_Compensa EQ W_NumBan NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Bancos THEN DO:
         MESSAGE "No existe el banco dentro de la tabla de Bancos" SKIP
                 "Cominiquese con el Administrador para que lo cree" SKIP
                  VIEW-AS ALERT-BOX ERROR.
         APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
      END.
      ELSE W_Canje = Bancos.Dia_Canje.
   END.
   IF W_AboMor EQ 0 THEN DO:
      MESSAGE "Digite el Valor del Abono a Capital" SKIP
              "El valor no puede ser cero" VIEW-AS ALERT-BOX.
      APPLY "entry" TO W_AboMor.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      RUN Transaccion NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         MESSAGE "Error al Realizar la Transaccion" SKIP
                 "consulte con el administrador!" VIEW-AS ALERT-BOX ERROR.
         
      APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
   END.
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Btn_Salir */
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


&Scoped-define SELF-NAME R_TipCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipCon Wwin
ON VALUE-CHANGED OF R_TipCon IN FRAME F_Cre
DO:
  IF SELF:SCREEN-VALUE EQ "2" THEN DO:
     ENABLE W_NumChe W_NumBan WITH FRAME F_Cre.
  END.
  ELSE DO:
    ASSIGN W_NumChe:SCREEN-VALUE = ""
           W_NumChe:HIDDEN = YES
           W_NumBan:SCREEN-VALUE = ""
           W_NumBan:HIDDEN = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AboMor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AboMor Wwin
ON LEAVE OF W_AboMor IN FRAME F_Cre /* Abono a Mora */
DO:
  IF DECIMAL(SELF:SCREEN-VALUE) GT Creditos.INT_MorCobrar THEN DO:
    MESSAGE "No se puede abonar mas del Saldo Mora presente" SKIP
            "El cual es: $"  Creditos.Sdo_Capital VIEW-AS ALERT-BOX.
    SELF:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Wwin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar Wwin 
PROCEDURE Contabilizar :
FIND Comprobantes WHERE 
     Comprobantes.Agencia     EQ W_Agencia AND
     Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
IF NOT AVAILABLE Comprobantes THEN DO:
   MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
           "Rectifique con el Administrador!"
           VIEW-AS ALERT-BOX ERROR. 
   RETURN ERROR.
END.
IF AVAILABLE Comprobantes THEN
   ASSIGN W_Cbte    = Comprobantes.Comprobante.
          /*W_NumCbt  = Comprobantes.Secuencia + 1
          Comprobantes.Secuencia = Comprobantes.Secuencia + 1*/

FOR EACH Taquilla WHERE Taquilla.Usuario EQ W_Usuario AND
                        Taquilla.Contabiliza EQ NO BREAK BY Taquilla.Nro_Transaccion:
  IF FIRST-OF(Taquilla.Nro_Transaccion) THEN DO:
     FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
     IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
       W_Cbte = Operacion.Comprobante.
     ELSE DO:
       RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Ok).
       RETURN ERROR.
     END.
  END.
  RUN Contabilizar_Partidas NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
     RETURN ERROR.
  ELSE
     Taquilla.Contabiliza = YES.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas Wwin 
PROCEDURE Contabilizar_Partidas :
DEFINE VAR WComentario AS CHARACTER FORMAT "X(30)".
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Taquilla.Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Cuenta         = Taquilla.Cuenta
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Abono a Int.Mora"
         Mov_Contable.Usuario        = Taquilla.Usuario
         Mov_contable.Nit            = Taquilla.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = Taquilla.Age_Destino
         Mov_Contable.Num_Documento  = INTEGER(Taquilla.Nro_Transaccion)
         Mov_Contable.Doc_Referencia = Taquilla.Num_Retcheque
         Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion NO-ERROR.
  IF Taquilla.Naturaleza EQ "DB" THEN 
     ASSIGN Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
  ELSE
     ASSIGN Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /*contrapartida*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Taquilla.Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Cuenta         = Taquilla.Cta_Contra
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Abono a Int.Mora"
         Mov_Contable.Usuario        = Taquilla.Usuario
         Mov_contable.Nit            = Taquilla.Nit
         Mov_Contable.Destino        = Taquilla.Age_Destino
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Num_Documento  = INTEGER(Taquilla.Nro_Transaccion)
         Mov_Contable.Doc_Referencia = Taquilla.Num_Retcheque
         Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion  NO-ERROR.
  IF Taquilla.Naturaleza EQ "DB" THEN 
     ASSIGN Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
  ELSE
     ASSIGN Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CortoLargoCreditos Wwin 
PROCEDURE CortoLargoCreditos :
FIND CortoLargo WHERE CortoLargo.Agencia        EQ Creditos.Agencia
                 AND   CortoLargo.Clase_Producto EQ 2
                 AND   CortoLargo.Cod_Producto   EQ Creditos.Cod_Credito
                 AND   CortoLargo.Cta_ContingenteDB NE "" 
                 AND   CortoLargo.Cta_ContingenteCR NE "" 
                 AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
              
 IF AVAILABLE(CortoLargo) THEN DO:
    /*W_Cbte = CortoLargo.Comprobante.*/
    W_CtaSyA_Des  = CortoLargo.Cta_SYA.
    IF W_CtaSyA_Des EQ "" THEN DO:
       MESSAGE "No esta configurada la cuenta de sucursales y agencias" SKIP
               "para la agencia de trabajo actual" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
    /*busca cta en tabla liqint*/
    W_CtaCorCre = "".
    FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2 AND
                         Liqui_Int.Cod_Producto   EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
    IF AVAILABLE Liqui_Int THEN W_CtaCorCre = Liqui_Int.CtaDB_MoraAso.
    IF W_CtaCorCre EQ "" THEN DO:
       MESSAGE "La cuenta para el pago de intereses de mora no esta configurada" SKIP
               "para el producto. se cancela la operación" SKIP
               "de desembolso!" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
 END.
 ELSE RETURN ERROR.
 IF Creditos.Agencia NE W_Agencia THEN DO:
   FIND CortoLargo WHERE CortoLargo.Agencia        EQ W_Agencia
                   AND   CortoLargo.Clase_Producto EQ 2
                   AND   CortoLargo.Cod_Producto   EQ Creditos.Cod_Credito
                   AND   CortoLargo.Cta_ContingenteDB NE ""
                   AND   CortoLargo.Cta_ContingenteCR NE ""
                   AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
     IF AVAILABLE(CortoLargo) THEN W_CtaSYA_FTE = CortoLargo.Cta_SYA.
     ELSE DO:
       MESSAGE "No se encontra la cuenta de sucursales y agencias" SKIP
               "de la Agencia de donde es el crédito" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-F_Cre}
  GET FIRST F_Cre.
  DISPLAY R_TipCon W_AboMor 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_Desembolso Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion 
          Creditos.Int_MorCobrar Creditos.Fec_UltPago Creditos.Int_Corrientes 
          Creditos.Fec_UltLiquidacion Creditos.Int_Anticipado 
          Creditos.Int_DifCobro Creditos.Sdo_Proyectado Creditos.Sdo_Capital 
          Creditos.Cuo_Pagadas Creditos.Tasa Creditos.Val_Atraso Creditos.Cuota 
          Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision 
          Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 RECT-297 R_TipCon W_AboMor Btn_Grabar Btn_Salir 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject Wwin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_CheTransito Wwin 
PROCEDURE Gra_CheTransito :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER C_Banco   LIKE Che_Transito.Cod_Compensa.
   DEFINE INPUT PARAMETER C_Cheque  LIKE Che_Transito.Cheque.
   DEFINE INPUT PARAMETER C_CodPto  LIKE Che_Transito.Cod_Producto.
   DEFINE INPUT PARAMETER C_Estado  LIKE Che_Transito.Estado.
   DEFINE INPUT PARAMETER C_Cuenta  LIKE Che_Transito.Num_Cuenta.
   DEFINE INPUT PARAMETER C_Agencia LIKE Che_Transito.Agencia.
   DEFINE INPUT PARAMETER C_TipPto  LIKE Che_Transito.Tip_Producto.
   DEFINE INPUT PARAMETER C_Valor   LIKE Che_Transito.Valor.
   DEFINE INPUT PARAMETER C_Canje   LIKE Che_Transito.Tip_Remesa.
   
   FIND Che_Transito WHERE Che_Transito.Agencia      EQ C_Agencia
                     AND   Che_Transito.Cod_Compensa EQ C_Banco
                     AND   Che_Transito.Cheque       EQ C_Cheque
                     AND   Che_Transito.Cod_Producto EQ C_CodPto
                     AND   Che_Transito.Num_Cuenta   EQ C_Cuenta EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(Che_Transito) THEN DO:
      ASSIGN Che_Transito.Valor = Che_Transito.Valor + C_Valor.
   END.
   ELSE DO:
      CREATE Che_Transito.
      ASSIGN Che_Transito.Cod_Compensa     = C_Banco
             Che_Transito.Cheque           = C_Cheque
             Che_Transito.Cod_Producto     = C_CodPto
             Che_Transito.Estado           = C_Estado
             Che_Transito.Fec_Canje        = TODAY
             Che_Transito.Fec_Confirmacion = TODAY
             Che_Transito.Int_Generado     = 0
             Che_Transito.Num_Cuenta       = C_Cuenta
             Che_Transito.Agencia          = C_Agencia
             Che_Transito.Ofi_Destino      = W_Agencia
             Che_Transito.Tip_Producto     = C_TipPto
             Che_Transito.Valor            = C_Valor
             Che_Transito.Tip_Remesa       = C_Canje NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al Grabar en Chequesen Transito... "
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
         TITLE "Error en Taquilla".
         RETURN ERROR.
      END.
   END.
   RELEASE Che_Transito.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovCreditos Wwin 
PROCEDURE Gra_MovCreditos :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos.       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER C_CodOper LIKE Mov_Creditos.Cod_Operacion.
  DEFINE INPUT PARAMETER C_CodPto  LIKE Mov_creditos.cod_credito.
  DEFINE INPUT PARAMETER C_NumCre  LIKE Mov_Creditos.Num_Credito.
  DEFINE INPUT PARAMETER C_Dto     LIKE Mov_Creditos.Num_Documento.
  DEFINE INPUT PARAMETER C_Agencia LIKE Mov_Creditos.Agencia.
  DEFINE INPUT PARAMETER C_OfiFte  LIKE Mov_Creditos.Ofi_Fuente.
  DEFINE INPUT PARAMETER C_OfiDest LIKE Mov_Creditos.Ofi_Destino.
  DEFINE INPUT PARAMETER C_Usuario LIKE Mov_Creditos.Usuario.
  DEFINE INPUT PARAMETER C_VlrChe  LIKE Mov_Creditos.Val_Cheque.
  DEFINE INPUT PARAMETER C_VlrEfe  LIKE Mov_Creditos.Val_Efectivo.
  
  CREATE Mov_Creditos.
  ASSIGN Mov_Creditos.Cod_Operacion  = C_CodOper
         Mov_creditos.cod_credito    = C_CodPto
         Mov_Creditos.Nit            = Creditos.nit
         Mov_Creditos.Fecha          = TODAY
         Mov_Creditos.Hora           = vTime
         Mov_Creditos.Pagare         = Creditos.Pagare
         Mov_Creditos.Num_Documento  = C_Dto
         Mov_Creditos.Agencia        = C_Agencia
         Mov_Creditos.Ofi_Fuente     = C_OfiFte
         Mov_Creditos.Ofi_Destino    = C_OfiDest
         Mov_Creditos.Num_Credito    = C_NumCre
         Mov_Creditos.Usuario        = C_Usuario
         Mov_Creditos.Val_Cheque     = C_VlrChe
         Mov_Creditos.Val_Efectivo   = C_VlrEfe
         Mov_Creditos.Sdo_Capital    = Creditos.Sdo_Capital
         Mov_Creditos.INT_Corrientes = Creditos.INT_Corrientes
         Mov_Creditos.INT_MorCobrar  = Creditos.INT_MorCobrar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Taquilla Wwin 
PROCEDURE Gra_Taquilla :
/*------------------------------------------------------------------------------
  OBSERVACIONES: Permite Almacenar el Registro en Taquilla.       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER T_Autorizo  LIKE Taquilla.Autorizo.
   DEFINE INPUT PARAMETER T_Banco     LIKE Taquilla.Cod_Compensa.
   DEFINE INPUT PARAMETER T_CodOper   LIKE Taquilla.Cod_Operacion.
   DEFINE INPUT PARAMETER T_CodPto    LIKE Taquilla.Cod_Producto.
   DEFINE INPUT PARAMETER T_Cuenta    LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_CtraCta   LIKE Taquilla.Cuenta.
   DEFINE INPUT PARAMETER T_Nat       LIKE Taquilla.Naturaleza.
   DEFINE INPUT PARAMETER T_Nit       LIKE Taquilla.Nit.
   DEFINE INPUT PARAMETER T_Nrocuenta LIKE Taquilla.Nro_cuenta.
   DEFINE INPUT PARAMETER T_NumDto    LIKE Taquilla.Num_Documento.
   DEFINE INPUT PARAMETER T_NumRetche LIKE Taquilla.Num_Retcheque.
   DEFINE INPUT PARAMETER T_Agencia   LIKE Taquilla.Agencia.
   DEFINE INPUT PARAMETER T_OfiDes    LIKE Taquilla.Age_Destino.
   DEFINE INPUT PARAMETER T_OfiFue    LIKE Taquilla.Age_Fuente.
   DEFINE INPUT PARAMETER T_TipPto    LIKE Taquilla.Tip_Producto.
   DEFINE INPUT PARAMETER T_Usuario   LIKE Taquilla.Usuario.
   DEFINE INPUT PARAMETER T_ValChe    LIKE Taquilla.Val_Cheque.
   DEFINE INPUT PARAMETER T_ValEfec   LIKE Taquilla.Val_Efectivo.
   DEFINE INPUT PARAMETER T_Segmento  LIKE Clientes.Cod_Segmento.
   
   CREATE Taquilla.
   ASSIGN Taquilla.Autorizo         = T_Autorizo
          Taquilla.Nro_Transaccion  = W_NumSeq
          Taquilla.Cod_Compensa     = T_Banco
          Taquilla.Cod_Operacion    = T_CodOper
          Taquilla.Cod_Producto     = T_CodPto
          Taquilla.Contabilizar     = FALSE
          Taquilla.Cuenta           = T_Cuenta
          Taquilla.Cta_Contra       = T_CtraCta
          Taquilla.Duracion         = 0
          Taquilla.Est_Linea        = 0
          Taquilla.Fec_Transaccion  = TODAY
          Taquilla.Hora_Transaccion = TIME
          Taquilla.Naturaleza       = T_Nat
          Taquilla.Nit              = T_Nit
          Taquilla.Nro_cuenta       = T_Nrocuenta
          Taquilla.Num_Documento    = T_NumDto
          Taquilla.Num_Retcheque    = T_NumRetche
          Taquilla.Agencia          = T_Agencia
          Taquilla.Age_Destino      = T_OfiDes
          Taquilla.Age_Fuente       = T_OfiFue
          Taquilla.Tip_Producto     = T_TipPto
          Taquilla.Usuario          = T_Usuario
          Taquilla.Val_Cheque       = T_ValChe
          Taquilla.Val_Efectivo     = T_ValEfec
          Taquilla.Estacion         = W_Estacion
          Taquilla.Cod_Segmento     = T_Segmento.
          Taquilla.Descripcion      = "Abono a Int.Mora".
   RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
  Wwin:TITLE = P_NomIns.
  FIND Operacion WHERE Operacion.Cod_Operacion EQ INTEGER(P_CodOpe) NO-LOCK NO-ERROR.
  IF AVAILABLE Operacion THEN W_Cbte = Operacion.Comprobante.
  ELSE DO:
    MESSAGE "No se ha encontrado la operación de créditos" SKIP
            "Comuniquese con el Administrador!" SKIP
            "La Operacion pasada fue: " P_CodOpe VIEW-AS ALERT-BOX ERROR.
    DISABLE Btn_Grabar WITH FRAME F_Cre.
  END.
  
  FIND Creditos WHERE
       Creditos.Tip_Credito EQ P_TipCre AND
       Creditos.Cod_Credito EQ P_CodCre AND
       Creditos.Num_Credito EQ P_NumCre AND
       Creditos.Nit         EQ P_NitCli EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.          
     FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
          Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN RUN CortoLargoCreditos NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
        DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
     FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                        AND   Cuentas.Car_Efectivo  EQ  2
                        AND   Cuentas.Estado        EQ  1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cuentas THEN W_Caja = Cuentas.Cuenta.
     ELSE DO:
       MESSAGE "No se ha encontrado la cuenta de caja efectivo" SKIP
               "para realizar la operacion. " SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
     END.
     RUN Mostrar_Credito.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
          Creditos.Fec_ProxLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_ProxLiquidacion)
          Creditos.Fec_UltLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_UltLiquidacion)
          Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
          Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
          Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
          Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado)
          Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado)
          Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
          Creditos.INT_MorCobrar:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir Wwin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    ASSIGN FRAME F_Cre W_AboMor.
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    ASSIGN W_Cliente = Creditos.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
 
    W_EncColumna = "Cliente Deudor Crédito      :   " + W_Cliente.
    W_Reporte   = "ABONO A CAPITAL    : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DISPLAY 
     "=============================================DATOS GENERALES DEL CREDITO=====================================" AT 1
     "Agencia de Radicación       : " AT 1
     Creditos.Agencia                 AT 33
     "Número del Crédito          : " AT 65
     Creditos.Num_Credito             AT 98
     "Fecha de Aprobación         : " AT 1
     Creditos.Fec_Aprobacion          AT 33
     "Producto de Crédito         : " AT 65
     Pro_Creditos.Nom_Producto        AT 98 FORMAT "X(20)"
     "Tipo de Producto            : " AT 1
     Pro_Creditos.Tip_Credito         AT 33 
     "Usuario Actualmente Procesa : " AT 65
     W_Usuario                        AT 98   
     "Forma de Pago de la Cuota   : " AT 1
     Creditos.FOR_Pago                AT 33 
     "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
     "Monto a Prestar             : " AT 1
     Creditos.Monto                   AT 33 
     "Tasa Efectiva Anual         : " AT 65
     Creditos.Tasa                    AT 98 SKIP(2)
     "INFORMACION DEL ABONO A CAPITAL" AT 1
     "Valor Abono        :"           AT 1
     W_AboMor                         AT 25
     "Fecha Abono        :"           AT 1
     W_Fecha                          AT 25
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taquilla Wwin 
PROCEDURE Taquilla :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = W_AboMor W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = W_AboMor.


W_NumSeq = NEXT-VALUE(Sec_Taquilla).
RUN Gra_MovCreditos(INPUT P_CodOpe, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT P_CodOpe, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaCorCre,      INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0).
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT P_CodOpe, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0).
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT P_CodOpe, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaCorCre,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0).
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT W_AboMor,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion Wwin 
PROCEDURE Transaccion :
DO TRANSACTION:
      RUN Taquilla NO-ERROR.
      ASSIGN Creditos.INT_MorCobrar = Creditos.INT_MorCobrar - W_AboMor.
      RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").
      MESSAGE "Aliste papel para Segunda Copia ".
      RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").
END.
DO TRANSACTION:
      RUN Contabilizar NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al contabiliar" VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

