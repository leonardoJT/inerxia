&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

/* Connected Databases 
          bdcentral        PROGRESS
*/

&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE VAR CtrLavado AS LOGICAL INITIAL YES.

/* oakley */

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
DEFI   VAR W_RowIdCr AS ROWID.

DEFINE VAR W_Ok AS LOGICAL.
DEFI   VAR W_DocContab LIKE Comprobantes.Secuencia.

DEFINE VAR P_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VrCapital LIKE Creditos.Sdo_Capital.

DEFINE VAR W_Cbte      LIKE Comprobantes.Comprobante.
DEFINE VAR W_CtaCorCre LIKE Cuentas.Cuenta.
DEFINE VAR W_Caja      LIKE Cuentas.Cuenta.
DEFINE VAR W_Banco     LIKE Cuentas.Cuenta.
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_AboCap R_TipCon Btn_Grabar Btn_Salir ~
RECT-296 RECT-297 RECT-321 
&Scoped-Define DISPLAYED-FIELDS Creditos.Int_MorCobrar ~
Creditos.Fec_Desembolso Creditos.Fec_Pago Creditos.Sdo_Capital ~
Creditos.Fec_UltPago Creditos.Tasa Creditos.Int_Anticipado ~
Creditos.Int_DifCobro Creditos.Sdo_Proyectado Creditos.Int_Corrientes ~
Creditos.Cuo_Pagadas Creditos.Cuota Creditos.Val_Atraso Creditos.Plazo ~
Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision ~
Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS W_AboCap W_VrCheque R_TipCon W_VrEfec 

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

DEFINE BUTTON Btn_Validadora 
     LABEL "Validar" 
     SIZE 6 BY 1.12 TOOLTIP "Botòn Oculto, para independizar Impresiòn de la Tx.".

DEFINE VARIABLE W_AboCap AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Abono a Capital" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81 TOOLTIP "Abono Extra en este período Independiente de la cuota normal"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NumBan AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NumChe AS CHARACTER FORMAT "X(10)":U 
     LABEL "Número de Cheque" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrCheque AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor del Cheque" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrEfec AS DECIMAL FORMAT "->>>>,>>>,>>9":U INITIAL 0 
     LABEL "Paga en Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_TipCon AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Efectivo", 1,
"Efec/Cheque", 2
     SIZE 20.72 BY .81
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 11.31.

DEFINE RECTANGLE RECT-297
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 10.77.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.43 BY 1.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cre
     Creditos.Int_MorCobrar AT ROW 5.31 COL 14 COLON-ALIGNED
          LABEL "Interés por Mora" FORMAT "->>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_AboCap AT ROW 4.08 COL 71.86 COLON-ALIGNED
     W_NumBan AT ROW 5.04 COL 72 COLON-ALIGNED
     W_NumChe AT ROW 6.04 COL 72 COLON-ALIGNED
     W_VrCheque AT ROW 7.04 COL 72 COLON-ALIGNED
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     R_TipCon AT ROW 2.35 COL 71.72 NO-LABEL
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          LABEL "Fecha Prox.Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 3.69 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 14 FGCOLOR 0 
     Creditos.Fec_UltPago AT ROW 4 COL 13.86 COLON-ALIGNED
          LABEL "Último Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 4.77 COL 44 COLON-ALIGNED
          LABEL "Tasa"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Anticipado AT ROW 5.85 COL 44 COLON-ALIGNED
          LABEL "Interés Anticipado"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_DifCobro AT ROW 6.12 COL 14 COLON-ALIGNED
          LABEL "Interés Contingente"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 6.92 COL 14 COLON-ALIGNED
          LABEL "Saldo Proyectado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 6.92 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 7.73 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuota AT ROW 8 COL 44 COLON-ALIGNED
          LABEL "Cuota"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Val_Atraso AT ROW 8.54 COL 14 COLON-ALIGNED
          LABEL "Valor Vencido"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Plazo AT ROW 9.04 COL 44 COLON-ALIGNED
          LABEL "Plazo" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_VrEfec AT ROW 9.04 COL 72 COLON-ALIGNED
     Creditos.Dias_Atraso AT ROW 9.35 COL 14 COLON-ALIGNED
          LABEL "Días Vencidos"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Cuo_Atraso AT ROW 10.15 COL 14 COLON-ALIGNED
          LABEL "Cuotas Vencidas"
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
     Btn_Grabar AT ROW 10.96 COL 53.43
     Btn_Salir AT ROW 10.96 COL 76
     Btn_Validadora AT ROW 11.23 COL 35 HELP
          "Botòn Oculto, para independizar Impresiòn de la Tx."
     Creditos.Fec_Reestructurado AT ROW 11.77 COL 14 COLON-ALIGNED
          LABEL "ReEstructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información del Crédito" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 1.15 COL 3
          FGCOLOR 7 FONT 5
     "Solo Abono Extra (Total)" VIEW-AS TEXT
          SIZE 17 BY .5 AT ROW 3.58 COL 71.57
          BGCOLOR 14 FGCOLOR 0 
     "Abono Extra en este período Independiente de la cuota normal" VIEW-AS TEXT
          SIZE 43.14 BY .5 AT ROW 1.58 COL 30.29
          BGCOLOR 14 FGCOLOR 0 
     RECT-296 AT ROW 1.54 COL 1.29
     RECT-297 AT ROW 2.08 COL 30
     RECT-321 AT ROW 2.23 COL 71.29
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
         COLUMN             = 8
         ROW                = 11.69
         HEIGHT             = 12.12
         WIDTH              = 97.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON Btn_Validadora IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Validadora:HIDDEN IN FRAME F_Cre           = TRUE.

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
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_MorCobrar IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
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

/* SETTINGS FOR FILL-IN W_VrCheque IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrEfec IN FRAME F_Cre
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _Query            is NOT OPENED
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
   ASSIGN FRAME F_Cre W_AboCap R_TipCon W_NumChe W_NumBan.

   IF  (Creditos.Sdo_Capital - W_AboCap) LE 0
   AND ((Creditos.Int_Corriente + Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob +
         Creditos.Int_DifCobro  + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios) GT 0
         OR Creditos.Int_Anticipado NE 0) THEN DO:
       MESSAGE "El Sdo-Capital no puede Cancelarse mientras Hallan Otros Saldos..." SKIP
                  "                                   Revise por favor..."
                  VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END. 
      
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
         RETURN NO-APPLY.
      END.
      ELSE W_Canje = Bancos.Dia_Canje.
   END.
   
   IF W_AboCap LE 0 THEN DO:
      MESSAGE "Digite el Valor del Abono a Capital" SKIP
              "El valor no puede ser cero o negativo." VIEW-AS ALERT-BOX.
      APPLY "Entry" TO W_AboCap.
      RETURN NO-APPLY.
   END.
   ELSE DO:
/*       IF W_AboCap LT 200000 OR ((Creditos.Sdo_Capital * .10) GT W_AboCap) THEN DO: */
      IF W_AboCap LT creditos.cuota THEN DO:
         MESSAGE "El Valor del Abono a Capital NO CUMPLE el tope establecido." SKIP
                 "                             Operación Rechazada." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.

      IF W_AboCap - (W_VrEfec + W_VrCheque) NE 0
      OR W_VrEfec   LT 0 
      OR W_VrCheque LT 0 THEN DO:
         MESSAGE "El Valor del Abono a Capital (Efectivo + Cheque) Errado." SKIP
                 "                             Operación Rechazada." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY. 
      END.

      MESSAGE "El valor Total del Abono a Capital es de $" W_AboCap    SKIP
              "En Efectivo $" W_VrEfec "      En Cheque $" W_VrCheque  SKIP
              "                               Continúe solo si está Segura(o)...?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Abono"
          UPDATE W_SiK AS LOGICAL. 
      IF NOT W_SiK THEN
         RETURN NO-APPLY.

      FIND Comprobantes WHERE Comprobantes.Agencia     EQ W_Agencia AND
                              Comprobantes.Comprobante EQ W_Cbte NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Comprobantes THEN DO:
         MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
           "Rectifique con el Administrador!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN W_Cbte = Comprobantes.Comprobante
                W_RowIdCr = ROWID(Creditos).
   
      RUN Transaccion NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al Realizar la Transacción" SKIP
                 "consulte con el administrador!" VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.
         
      APPLY "choose" TO Btn_Validadora. 
   END.
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Btn_Salir */
DO: 
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Validadora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Validadora Wwin
ON CHOOSE OF Btn_Validadora IN FRAME F_Cre /* Validar */
DO:
  /*MESSAGE "Impresora(SI)   Ò    Validadora(NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS 
       YES-NO  TITLE "Medio de Impresiòn" UPDATE RpaVI AS LOGICAL.                   
                                                                                  
  IF RpaVI THEN DO:                                                                  
     RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").                                    
     MESSAGE "Aliste papel para Segunda Copia ".                                     
     RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").                                    
  END.                                                                               
  ELSE*/  

  RUN Imp_Validadora (INPUT W_NumSeq, INPUT "").          
       
  APPLY "choose" TO Btn_Salir IN FRAME F_Cre.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_TipCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipCon Wwin
ON VALUE-CHANGED OF R_TipCon IN FRAME F_Cre
DO:
  ASSIGN R_TipCon.

  IF SELF:SCREEN-VALUE EQ "2" THEN DO:
     ENABLE W_NumChe W_NumBan W_VrCheque WITH FRAME F_Cre.
  END.
  ELSE DO:
    ASSIGN W_NumChe:SCREEN-VALUE   = ""
           W_NumChe:HIDDEN         = YES
           W_NumBan:SCREEN-VALUE   = ""
           W_NumBan:HIDDEN         = YES
           W_VrCheque:SCREEN-VALUE = "0"
           W_VrCheque:HIDDEN       = YES
           W_VrEfec                = W_AboCap
           W_VrEfec:SCREEN-VALUE   = STRING(W_AboCap).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AboCap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AboCap Wwin
ON LEAVE OF W_AboCap IN FRAME F_Cre /* Abono a Capital */
DO:
  IF DECIMAL(SELF:SCREEN-VALUE) GT Creditos.Sdo_Capital THEN DO:
    MESSAGE "No se puede abonar mas del Saldo Capital presente" SKIP
            "El cual es: $"  Creditos.Sdo_Capital VIEW-AS ALERT-BOX.
    SELF:SCREEN-VALUE = STRING(Creditos.Sdo_Capital).
  END.
  
  ASSIGN W_AboCap.

  IF R_TipCon EQ 1 THEN
     ASSIGN W_VrEfec                = W_AboCap
            W_VrEfec:SCREEN-VALUE   = STRING(W_AboCap)
            W_VrCheque              = 0
            W_VrCheque:SCREEN-VALUE = "0".
  ELSE 
     ASSIGN W_VrEfec                = W_AboCap - W_VrCheque
            W_VrEfec:SCREEN-VALUE   = STRING(W_VrEfec).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrCheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrCheque Wwin
ON LEAVE OF W_VrCheque IN FRAME F_Cre /* Valor del Cheque */
DO:
  ASSIGN W_VrCheque
         W_VrEfec              = W_AboCap - W_VrCheque
         W_VrEfec:SCREEN-VALUE = STRING(W_VrEfec).

  IF W_VrCheque GT W_AboCap THEN DO:
     MESSAGE "El valor del cheque no puede ser superior al Abono-Total..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN W_VrCheque              = W_AboCap  
            W_VrCheque:SCREEN-VALUE = STRING(W_AboCap)
            W_VrEfec                = 0           
            W_VrEfec:SCREEN-VALUE   = "0". 
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
FOR EACH Taquilla WHERE Taquilla.Usuario         EQ W_Usuario AND
                         Taquilla.Fec_Transaccion EQ W_Fecha   AND
                         Taquilla.Contabiliza     EQ NO        AND   
                         Taquilla.Nro_Transaccion EQ W_NumSeq:
     RUN Contabilizar_Partidas NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error grabando Mov_contable...Revise por favor."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.
     ELSE
        Taquilla.Contabiliza = YES.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas Wwin 
PROCEDURE Contabilizar_Partidas :
/*
  ------------------------------------------------------------------*/
  DEFINE VAR WComentario AS CHARACTER FORMAT "X(30)".

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Taquilla.Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Cuenta         = Taquilla.Cuenta
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Abono a Capital"
         Mov_Contable.Usuario        = Taquilla.Usuario
         Mov_contable.Nit            = Taquilla.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = W_Agencia
         Mov_Contable.Num_Documento  = W_DocContab
         Mov_Contable.Doc_Referencia = Taquilla.Num_Retcheque
         Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion.

  IF  Taquilla.Agencia EQ W_Agencia
  AND (SUBSTRING(Taquilla.Cuenta,1,4) EQ "1904" OR SUBSTRING(Taquilla.Cuenta,1,4) EQ "2705") THEN DO:
      FIND Creditos WHERE ROWID(Creditos) EQ W_RowIdCr NO-LOCK NO-ERROR.    
      ASSIGN Mov_contable.Nit = STRING(Creditos.Agencia,"999").
  END.

  IF Taquilla.Naturaleza EQ "DB" THEN 
     ASSIGN Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo.
  ELSE
     ASSIGN Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo.

  IF  Taquilla.Agencia EQ W_Agencia
  AND (SUBSTRING(Taquilla.Cuenta,1,4) EQ "1904" OR SUBSTRING(Taquilla.Cuenta,1,4) EQ "2705") THEN DO:
      FIND Creditos WHERE ROWID(Creditos) EQ W_RowIdCr NO-LOCK NO-ERROR.    
  END.

 /* IF ERROR-STATUS:ERROR THEN RETURN ERROR.*/
  
  /*contrapartida*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Taquilla.Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Cuenta         = Taquilla.Cta_Contra
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Abono a Capital"
         Mov_Contable.Usuario        = Taquilla.Usuario
         Mov_contable.Nit            = Taquilla.Nit
         Mov_Contable.Destino        = W_Agencia
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Num_Documento  = W_DocContab
         Mov_Contable.Doc_Referencia = Taquilla.Num_Retcheque
         Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion NO-ERROR.

  IF  Taquilla.Agencia NE W_Agencia
  AND (SUBSTRING(Mov_Contable.Cuenta,1,4) EQ "1904" OR SUBSTRING(Mov_Contable.Cuenta,1,4) EQ "2705") THEN DO:
       ASSIGN Mov_contable.Nit = STRING(W_Agencia,"999") NO-ERROR.
  END.

  IF Taquilla.Naturaleza EQ "DB" THEN 
     ASSIGN Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo.
  ELSE
     ASSIGN Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo.

  /*IF ERROR-STATUS:ERROR THEN RETURN ERROR.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlLavado Wwin 
PROCEDURE ControlLavado :
DEFINE VAR RT_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR RT_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR PCod LIKE BorradorSipla.CodAutoriza.
DEFINE VAR PUsu LIKE W_Usuario.
DEFINE VAR MenSipla  AS CHARACTER FORMAT "X(5)".
DEFINE VAR MenSiplaM AS CHARACTER FORMAT "X(5)".

FIND LAST ControlSipla WHERE
          ControlSipla.Nit          EQ Creditos.Nit AND
          MONTH(ControlSipla.Fecha) EQ MONTH(W_Fecha) NO-ERROR.
IF AVAILABLE ControlSipla THEN DO:
   IF ControlSipla.Fecha EQ W_Fecha THEN
      ASSIGN CS_ValSiplaDia = ControlSipla.CS_TotalDia
             AB_ValSiplaDia = ControlSipla.AB_TotalDia
             RT_ValSiplaDia = ControlSipla.RT_TotalDia.
   ASSIGN CS_ValSiplaMes = ControlSipla.CS_TotalMes
          AB_ValSiplaMes = ControlSipla.AB_TotalMes
          RT_ValSiplaMes = ControlSipla.RT_TotalDia.
END.
ELSE DO:
    CREATE ControlSipla.
    ASSIGN ControlSipla.Nit         = Creditos.Nit
           ControlSipla.Fecha       = W_Fecha.
END.
IF W_ValEfe + W_ValChe GT 0 THEN DO: /*es consignacion*/
   IF CS_ValSiplaDia + AB_ValSiplaDia + W_ValEfe + W_ValChe GT Entidad.MaxOp_Efectivo_Dia THEN 
      ASSIGN MenSipla  = "CSDIA". 
   IF CS_ValSiplaMes + AB_ValSiplaMes + W_ValEfe + W_ValChe  GT Entidad.MaxOp_Efectivo_Mes THEN
      ASSIGN MenSiplaM = "CSMES".
END.
ASSIGN PCod = 0
       CtrLavado = NO.

IF MenSipla NE "" OR MenSiplaM NE "" THEN 
   RUN C-ControlSipla.w (INPUT Creditos.Nit, INPUT MenSipla, INPUT MenSiplaM, OUTPUT PCod).
ELSE DO: 
   CtrLavado = YES.
   /*MESSAGE controlsipla.fecha w_fecha VIEW-AS ALERT-BOX.*/
   IF ControlSipla.Fecha NE W_Fecha THEN DO:
       CREATE ControlSipla.
       ASSIGN ControlSipla.Nit         = Creditos.Nit
              ControlSipla.Fecha       = W_Fecha.
   END.
   ASSIGN ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + W_ValEfe + W_ValChe
          ControlSipla.AB_TotalDia = ControlSipla.AB_TotalDia + W_ValEfe + W_ValChe.
END.

IF PCod NE 0 THEN DO: /*graba registro del dia de la transaccion*/
   CtrLavado = YES.
   IF ControlSipla.Fecha LT W_Fecha THEN DO:
       FIND ControlSipla       WHERE
            ControlSipla.Nit   EQ Creditos.Nit AND
            ControlSipla.Fecha EQ W_Fecha NO-ERROR.
       IF NOT AVAILABLE ControlSipla THEN DO:
          CREATE ControlSipla.
          ASSIGN ControlSipla.Nit         = Creditos.Nit
                 ControlSipla.Fecha       = W_Fecha
                 ControlSipla.CS_TotalMes = ControlSipla.CS_TotalMes + CS_ValSiplaMes
                 ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + AB_ValSiplaMes
                 ControlSipla.RT_TotalMes = ControlSipla.RT_TotalMes + RT_ValSiplaMes.
       END.
   END.
   IF MenSipla  NE "" THEN ControlSipla.Id_NUD = YES.
   IF MenSiplaM NE "" THEN ControlSipla.Id_NUM = YES.
   ASSIGN ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + W_ValEfe + W_ValChe
          ControlSipla.AB_TotalDia = ControlSipla.AB_TotalDia + W_ValEfe + W_ValChe.
   /*crea la instancia de mov_inssipla para el encargado de la agencia*/

   FIND Instancias WHERE
        Instancias.Tipo_Instancia EQ 6   AND 
        Instancias.Primera        EQ YES AND
        Instancias.Estado         EQ 1   NO-LOCK NO-ERROR.
   IF AVAILABLE Instancias THEN DO:
      FIND FIRST Cfg_Instancias WHERE 
           Cfg_Instancias.Agencia   EQ Creditos.Agencia /*W_Agencia*/ AND /*para que la instancia quede*/
           Cfg_Instancias.Instancia EQ Instancias.Instancia AND          /*en la agencia de la cuenta*/
           Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE Cfg_Instancias THEN DO:
         FIND LAST BorradorSipla WHERE
              BorradorSipla.Agencia      EQ W_Agencia   AND
              BorradorSipla.Nit          EQ Creditos.Nit AND
              BorradorSipla.Fecha        EQ W_Fecha     AND
              /*BorradorSipla.Id_AhoCre    EQ 2           AND*/
              BorradorSipla.CodAutoriza  EQ PCod NO-ERROR.
         FIND FIRST Mov_InsSipla WHERE
              Mov_InsSipla.Agencia           EQ Creditos.Agencia AND
              Mov_InsSipla.Fecha_Transaccion EQ W_Fecha          AND
              Mov_InsSipla.Instancia         EQ Cfg_Instancias.Instancia AND
              Mov_InsSipla.Nit               EQ Creditos.Nit NO-ERROR.
         IF NOT AVAILABLE Mov_InsSipla THEN DO:
             CREATE Mov_InsSipla.
             ASSIGN Mov_InsSipla.Agencia           = Creditos.Agencia /*W_Agencia*/
                    Mov_InsSipla.Fecha_Transaccion = W_Fecha
                    Mov_InsSipla.Hora_Transaccion  = TIME
                    Mov_InsSipla.Instancia         = Cfg_Instancias.Instancia
                    Mov_InsSipla.Nit               = Creditos.Nit
                    Mov_InsSipla.UsuCajero         = W_Usuario
                    Mov_InsSipla.UsuGestiona       = Cfg_Instancias.Usuario.
                  /*Mov_InsSipla.AgeOrigen         = W_Agencia Ahorros.Agencia*/
         END.
         IF AVAILABLE BorradorSipla THEN DO:
             BorradorSipla.Id_AhoCre = 2.
             ASSIGN Mov_InsSipla.UsuReporta        = BorradorSipla.Usuario
                    Mov_InsSipla.Id_Exonerada      = BorradorSipla.Id_Exonerada
                    Mov_InsSipla.Id_NUD            = BorradorSipla.Id_NUD
                    Mov_InsSipla.Id_NUM            = BorradorSipla.Id_NUM
                    Mov_InsSipla.Id_RepUIAF        = BorradorSipla.Id_RepFiscalia
                    Mov_InsSipla.Id_Sospechosa     = BorradorSipla.Id_Sospechosa
                    Mov_InsSipla.CodAutoriza       = PCod.
             IF Creditos.Agencia NE W_Agencia THEN 
                Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + 
                                                 " Ag:" + STRING(W_Agencia,"99") + " - " +
                                                 BorradorSipla.Descripcion.
             ELSE
                 Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + " . " +
                                                  BorradorSipla.Descripcion.
         END.
         ELSE DO:
            MESSAGE "No se encontrado el borrador sipla que permita" SKIP
                    "identificar quien manejo al cliente en la agencia"
                    VIEW-AS ALERT-BOX.
            CtrLavado = NO.
            RETURN ERROR.
         END.
         IF MenSipla  NE "" THEN Mov_InsSipla.Id_NUD = YES.
         IF MenSiplaM NE "" THEN Mov_InsSipla.Id_NUM = YES.
      END.
      ELSE DO:
         MESSAGE "No se ha encontrado un usuario para asignarle" SKIP
                 "la primera instancia del control de lavado de" SKIP
                 "activos.  No  se podra realizar la operacion," SKIP
                 "hasta que no se asigne la instancia a un usuario" VIEW-AS ALERT-BOX.
         CtrLavado = NO.
      END.
   END.
   ELSE DO:
       MESSAGE "No se ha encontrado la primera instancia" SKIP
               "del control de lavado de activos." SKIP
               "la transaccion no se podra realizar!!!" SKIP(1)
               "comuniquese con el depto de sistemas" VIEW-AS ALERT-BOX ERROR.
       CtrLavado = NO.
   END.
END.
RELEASE ControlSipla.
RELEASE Mov_InsSipla.
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
                 AND   CortoLargo.Comprobante       NE 0  NO-LOCK NO-ERROR.
                 
 IF AVAILABLE(CortoLargo) THEN DO: 
    W_CtaSyA_Des  = CortoLargo.Cta_SYA.
    IF W_CtaSyA_Des EQ "" THEN DO:
       MESSAGE "No esta configurada la cuenta de sucursales y agencias" SKIP
               "para la agencia de trabajo actual" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.

    /*busca si hay garantias admisibles*/
    FIND FIRST Garantias WHERE 
         Garantias.Num_Credito EQ Creditos.Num_Credito  AND 
         Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Garantias THEN DO:
      IF Clientes.Tipo_Vinculo EQ 1 THEN 
         W_CtaCorCre = CortoLargo.Cta_AsoAd.
      ELSE 
         W_CtaCorCre = CortoLargo.Cta_NoaAd.
    END.
    ELSE DO:
      IF Clientes.Tipo_Vinculo EQ 1 THEN 
         W_CtaCorCre = CortoLargo.Cta_AsoNa.
      ELSE 
         W_CtaCorCre = CortoLargo.Cta_NoaNa.
    END.
    IF W_CtaCorCre EQ "" THEN DO:
       MESSAGE "La cuenta de corto y largo no esta configurada" SKIP
               "para el producto de crédito. se cancela la operación" SKIP
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
  DISPLAY W_AboCap W_VrCheque R_TipCon W_VrEfec 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Int_MorCobrar Creditos.Fec_Desembolso Creditos.Fec_Pago 
          Creditos.Sdo_Capital Creditos.Fec_UltPago Creditos.Tasa 
          Creditos.Int_Anticipado Creditos.Int_DifCobro Creditos.Sdo_Proyectado 
          Creditos.Int_Corrientes Creditos.Cuo_Pagadas Creditos.Cuota 
          Creditos.Val_Atraso Creditos.Plazo Creditos.Dias_Atraso 
          Creditos.Cuo_Atraso Creditos.Provision Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE W_AboCap R_TipCon Btn_Grabar Btn_Salir RECT-296 RECT-297 RECT-321 
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
         MESSAGE "Error al Grabar en Cheques en Transito... "
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
         TITLE "Error en Taquilla".
         RETURN ERROR.
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovCreditos Wwin 
PROCEDURE Gra_MovCreditos :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos.
  y en el PlanPagos.       
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
         Mov_Creditos.Cpte           = W_Cbte
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
         Mov_Creditos.INT_MorCobrar  = Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob
         Mov_Creditos.Descrip        = "Abono Capital-Efectivo".
           
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
          Taquilla.Fec_Transaccion  = W_Fecha
          Taquilla.Hora_Transaccion = TIME
          Taquilla.Naturaleza       = T_Nat
          Taquilla.Nit              = T_Nit
          Taquilla.Nro_cuenta       = T_Nrocuenta
          Taquilla.Num_Documento    = T_NumDto
          Taquilla.Num_Retcheque    = T_NumRetche
          Taquilla.Agencia          = T_Agencia
          Taquilla.Age_Destino      = T_OfiDes
          Taquilla.Age_Fuente       = T_OfiFue
          Taquilla.Tip_Producto     = 2
          Taquilla.Usuario          = T_Usuario
          Taquilla.Val_Cheque       = T_ValChe
          Taquilla.Val_Efectivo     = T_ValEfec
          Taquilla.Estacion         = W_Estacion
          Taquilla.Cod_Segmento     = T_Segmento
          Taquilla.Descripcion      = "Extra-Efectivo".
          
 IF Taquilla.Val_Cheque GT 0 THEN
    Taquilla.Descripcion = "Extra-Cheque".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Validadora Wwin 
PROCEDURE Imp_Validadora :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER P_transa   LIKE Taquilla.Nro_Transaccion.
 DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)".

 DEFINE VAR W_NomPcto          AS   CHARACTER FORMAT "X(15)".
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_TVrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
 DEFINE VAR W_TVrEfec          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
 DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".
 DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99" INIT 0.
 DEFINE VAR W_VrConsig         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrRetiro         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(32)" INITIAL "".
 DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_totopera         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR Total_Debito       LIKE Mov_Contable.db.
 DEFINE VAR Total_Credito      LIKE Mov_Contable.db.
 DEFINE VAR W_NomUsu           LIKE Usuarios.Nombre.
 DEFINE VAR W_Rpta             AS   LOGICAL. 


FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa NO-LOCK
                  BREAK BY Taquilla.Cod_Producto:
      IF  Taquilla.Naturaleza            EQ "Cr" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "1904"   THEN
         ASSIGN W_TVrEfec   = W_TVrEfec   + Taquilla.Val_Efectivo
                W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.
      
      IF LAST-OF(Taquilla.Cod_Producto) THEN DO:
         ASSIGN W_DescOpe = "Operación no Existe"
                W_NomPcto = ""
                W_NomCli  = "".

         FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) THEN
            W_DescOpe = Operacion.Nom_Operacion.

         FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN 
            W_NomPcto = TRIM(Pro_Creditos.Nom_Producto).

         FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN 
            W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     
         W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto)).
         IF Taquilla.Descripcion NE "" THEN
            W_Comentario = Taquilla.Descripcion.

         RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto,
                                          Taquilla.Nro_Cuenta,
                                          W_Comentario, W_Nomcli, W_NomPcto,
                                          W_DocContab, W_TVrEfec,W_TVrCheque," ").
         ASSIGN W_TVrEfec   = 0
                W_TVrCheque = 0.
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_ValidadoraAnt Wwin 
PROCEDURE Imp_ValidadoraAnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*  Comentariado Mayo 19/05 así estaba en el Procedimiento Imp_Validadora
 
 DEFINE INPUT PARAMETER P_transa   LIKE Taquilla.Nro_Transaccion.
 DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)".

 DEFINE VAR W_NomPcto          AS   CHARACTER FORMAT "X(15)".
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_TvrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99".
 DEFINE VAR W_TvrEfec          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99".
 DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".
 DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrConsig         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrRetiro         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(32)" INITIAL "".
 DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_totopera         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR Total_Debito       LIKE Mov_Contable.db.
 DEFINE VAR Total_Credito      LIKE Mov_Contable.db.
 DEFINE VAR W_NomUsu           LIKE Usuarios.Nombre.
 DEFINE VAR W_Rpta             AS   LOGICAL. 

FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa NO-LOCK:
      ASSIGN W_DescOpe = "Operación no Existe"
             W_NomPcto = ""
             w_NomCli  = "".

      FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
      IF AVAILABLE(Operacion) THEN
         W_DescOpe = Operacion.Nom_Operacion.

      FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
      IF AVAILABLE(Pro_Creditos) THEN 
         W_NomPcto = TRIM(Pro_Creditos.Nom_Producto).
    
      IF Taquilla.Naturaleza = "Cr" THEN DO:                            
        ASSIGN w_VrOpera = 0.                                               
        IF Taquilla.Val_Cheque GT 0 THEN                                    
          ASSIGN w_VrOpera = Taquilla.Val_Cheque                            
                 W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.           
        ELSE                                                                
          ASSIGN w_VrOpera = Taquilla.Val_Efectivo                          
                 W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.            
        Total_Credito  = Total_Credito + w_VrOpera.                         
      END.                                                                  
      ELSE DO:                                                              
        IF Taquilla.Naturaleza = "Db" THEN DO:                              
          ASSIGN w_VrOpera = 0.                                             
          IF Taquilla.Val_Cheque GT 0 THEN                                  
            ASSIGN w_VrOpera = Taquilla.Val_Cheque                          
                   W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.         
          ELSE                                                              
            ASSIGN w_VrOpera = Taquilla.Val_Efectivo                        
                   W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.          
          Total_Debito  = Total_Debito + w_VrOpera.                         
        END.                                                                
      END.                                                                  
      
      FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN 
        W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).

     
      W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto)).
      IF Taquilla.Descripcion NE "" THEN
         W_Comentario = Taquilla.Descripcion.

      RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto, Taquilla.Nro_Cuenta,
                              W_Comentario, W_Nomcli, W_NomPcto,
                              P_transa, W_VrOpera,Taquilla.Val_Cheque," ").

END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
/*
  -------------------------------------------------------------------------------*/
  FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  RUN SUPER.
  Wwin:TITLE = P_NomIns.

  FIND Operacion WHERE Operacion.Cod_Operacion EQ 020101001 NO-LOCK NO-ERROR.
  IF AVAILABLE Operacion THEN 
     ASSIGN W_Cbte   = Operacion.Comprobante
            P_CodOpe = STRING(020101001).
  ELSE DO:
    MESSAGE "No se ha encontrado la operación de créditos :" 020101001 SKIP
            "Comuniquese con el Administrador!"
           VIEW-AS ALERT-BOX ERROR.
    DISABLE Btn_Grabar WITH FRAME F_Cre.
  END.

  FIND FIRST Creditos WHERE Creditos.Nit         EQ P_NitCli
                        AND Creditos.Sdo_Capital GT 0
                        AND Creditos.Val_Atraso  GT 10000 NO-LOCK NO-ERROR.
  IF AVAIL(Creditos) THEN DO:
     MESSAGE "El Cliente tiene Creditos con Capital vencido, NO se permite la Operacion."
         VIEW-AS ALERT-BOX ERROR.
     DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
  END.
    
  FIND Creditos WHERE
       Creditos.Tip_Credito EQ P_TipCre AND
       Creditos.Cod_Credito EQ P_CodCre AND
       Creditos.Num_Credito EQ P_NumCre AND
       Creditos.Nit         EQ P_NitCli NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.          
     FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
                             Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN 
        RUN CortoLargoCreditos NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
        DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
        
     /*busca cuenta de caja-cheque*/
     FIND FIRST Cuentas WHERE       Cuentas.Cuenta  GT "0"
                        AND   Cuentas.Cod_FlujoEfec EQ "D"
                        AND   Cuentas.Car_Efectivo  EQ  3
                        AND   Cuentas.Estado        EQ  1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN 
        W_Banco = Cuentas.Cuenta.
     ELSE 
       MESSAGE "No se ha encontrado la cuenta de Caja cheques" SKIP
               "para realizar la operacion. " SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.

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
          Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro + Creditos.INT_MoraDifCob)
          Creditos.INT_MorCobrar:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar)
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
          Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
          Creditos.Tasa:SCREEN-VALUE = STRING(Creditos.Tasa)
          Creditos.Fec_UltPago:SCREEN-VALUE = STRING(Creditos.Fec_UltPago).
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
    ASSIGN FRAME F_Cre W_AboCap.
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
     Creditos.Val_Desembolso          AT 33 
     "Tasa Efectiva Anual         : " AT 65
     Creditos.Tasa                    AT 98 SKIP(2)
     "INFORMACION DEL ABONO A CAPITAL" AT 1
     "Valor Abono        :"           AT 1
     W_AboCap                         AT 25
     "Fecha Abono        :"           AT 1
     W_Fecha                          AT 25
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taquilla Wwin 
PROCEDURE Taquilla :
/*Mayo 19/05 GAER, Se agregó grabar Efectivo y/o cheque.
------------------------------------------------------*/
DEFI VAR W_CtaPpal LIKE Cuentas.Cuenta.


IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe  = W_AboCap - W_VrEfec
          W_ValEfe  = W_VrEfec
          W_CtaPpal = W_Banco.
ELSE ASSIGN W_ValChe  = 0 
            W_ValEfe  = W_AboCap
            W_CtaPpal = W_Caja.

RUN ControlLavado NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN ERROR.
IF NOT CtrLavado THEN RETURN ERROR.

RUN Gra_MovCreditos(INPUT P_CodOpe, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_DocContab,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).
IF R_TipCon EQ 2 THEN DO:
   ASSIGN Mov_Creditos.Val_Cheque   = W_ValChe 
          Mov_Creditos.Val_Efectivo = 0
          Mov_Creditos.Descrip      = "Abono Capital-Cheque".

   IF W_ValEfe GT 0 THEN DO:
      RUN Gra_MovCreditos(INPUT P_CodOpe, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_DocContab,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT 0, 
                    INPUT W_ValEfe).
      Mov_Creditos.Descrip = "Abono Capital-Efectivo".
   END.
END.

IF Creditos.Agencia EQ W_Agencia THEN DO:
   RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT P_CodOpe, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaCorCre,      INPUT W_CtaPpal,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0).
   IF R_TipCon EQ 2 AND W_ValEfe GT 0 THEN DO:
      ASSIGN Taquilla.Val_Efectivo = 0.

      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT P_CodOpe, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaCorCre,      INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT 0,   INPUT W_ValEfe,
                       INPUT 0).
   END.

END.
ELSE DO:                                                                                               
    RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT P_CodOpe,                                           
                     INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_CtaPpal,                   
                     INPUT "CR",            INPUT Creditos.Nit,INPUT STRING(Creditos.Num_Credito),    
                     INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,                        
                     INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",                            
                     INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,                             
                     INPUT 0). 

    IF R_TipCon EQ 2 AND W_ValEfe GT 0 THEN DO:
       ASSIGN Taquilla.Val_Efectivo = 0.

       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT P_CodOpe,                                           
                     INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,                   
                     INPUT "CR",            INPUT Creditos.Nit,INPUT STRING(Creditos.Num_Credito),    
                     INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,                        
                     INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",                            
                     INPUT W_Usuario,       INPUT 0,   INPUT W_ValEfe,                             
                     INPUT 0). 
    END.

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
                        INPUT 2,        INPUT W_ValChe,  INPUT W_Canje) NO-ERROR.                         
                                                                                                       
RELEASE Taquilla.
RELEASE Che_Transito.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion Wwin 
PROCEDURE Transaccion :
DO TRANSACTION ON ERROR UNDO:
      FIND Comprobantes WHERE Comprobantes.Agencia     EQ W_Agencia AND
                              Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
      ASSIGN W_NumSeq               = NEXT-VALUE(Sec_Taquilla)
             Comprobantes.Secuencia = Comprobantes.Secuencia + 1
             W_DocContab            = Comprobantes.Secuencia.

      FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

      FIND CURRENT Creditos NO-ERROR.
      ASSIGN Creditos.Sdo_Capital = Creditos.Sdo_Capital - W_AboCap
             Creditos.Sdo_CapPag  = Creditos.Sdo_CapPag  + W_AboCap
             Creditos.Fec_UltPag  = W_Fecha.

      FIND LAST PlanPagos WHERE PlanPagos.Agencia  EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1  NO-ERROR.
                        
      ASSIGN PlanPagos.Cuota = PlanPagos.Cuota + W_AboCap WHEN Creditos.Sistema NE 2.
         
      ASSIGN PlanPagos.Pagos_CapitalAcum = PlanPagos.Pagos_CapitalAcum + W_AboCap
             PlanPagos.Pagos_CapitalPdo  = PlanPagos.Pagos_CapitalPdo  + W_AboCap.
         
      FIND CURRENT PlanPagos NO-LOCK NO-ERROR.

      RUN Taquilla NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al grabar en taquilla" VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
      
      FIND CURRENT Creditos NO-LOCK NO-ERROR.
             
      RUN Contabilizar NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Error al contabilizar" VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

