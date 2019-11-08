&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

/* Connected Databases 
          bdcentral        PROGRESS
*/

&Scoped-define WINDOW-NAME WCos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WCos 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}

    /* oakley */     

DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli LIKE Clientes.Nit.
DEFINE INPUT PARAMETER P_CodCre LIKE Creditos.Cod_Credito.
DEFINE INPUT PARAMETER P_TipCre LIKE Creditos.Tip_Credito.
DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER P_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".
DEFINE VAR   ROWID_taq          AS ROWID.
DEFINE VARIABLE W_SecuenciaImprimir      AS INTEGER FORMAT "9999999".
DEFINE VAR   CtaHon LIKE Cuentas.Cuenta.

DEFINE VAR P_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VrCapital LIKE Creditos.Sdo_Capital.

     DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
     DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
     DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
     DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
     DEFINE VARIABLE P_TipDoc   LIKE Clientes.Tipo_Identificacion.


DEFINE VAR W_Canje  LIKE Bancos.Dia_Canje.
DEFINE VAR W_ValChe LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValEfe LIKE Creditos.Sdo_Capital.

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_Cbte      LIKE Comprobantes.Comprobante.
DEFINE VAR CtaDeb      LIKE Cuentas.Cuenta.
DEFINE VAR CtaCre      LIKE Cuentas.Cuenta.
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-296 RECT-298 Cmb_Tipo W_NitAbo W_FecAbo ~
W_DocAbo W_Valor Btn_Acepta_Terminos Btn_Salir 
&Scoped-Define DISPLAYED-FIELDS Creditos.Fec_Desembolso Creditos.Fec_Pago ~
Creditos.Fec_ProxLiquidacion Creditos.Fec_UltPago ~
Creditos.Fec_UltLiquidacion Creditos.Sdo_Proyectado Creditos.Cuo_Pagadas ~
Creditos.Val_Atraso Creditos.Dias_Atraso Creditos.Cuo_Atraso ~
Creditos.Provision Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo W_NitAbo W_NomAbo W_FecAbo ~
W_DocAbo W_SdoDeuda W_Valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WCos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Acepta_Terminos 
     LABEL "Aceptar Nuevos Términos" 
     SIZE 21 BY 1.62.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 176" 
     SIZE 21 BY 1.62.

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(25)":U INITIAL "Cargo de Costas" 
     LABEL "Tipo de Operación a Realizar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Cargo de Costas","Cargo de Honorarios","Cargo de Polizas" 
     DROP-DOWN-LIST
     SIZE 25 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_DocAbo AS CHARACTER FORMAT "X(10)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecAbo AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitAbo AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Abogado" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomAbo AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoDeuda AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Saldo Deuda" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 12 FGCOLOR 15 .

DEFINE VARIABLE W_Valor AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Costas" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 11.31.

DEFINE RECTANGLE RECT-298
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 5.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cre
     Cmb_Tipo AT ROW 1.81 COL 68 COLON-ALIGNED
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Aprobación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          LABEL "Fecha Prox.Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NitAbo AT ROW 3.42 COL 39 COLON-ALIGNED
     W_NomAbo AT ROW 3.42 COL 55 COLON-ALIGNED NO-LABEL
     Creditos.Fec_ProxLiquidacion AT ROW 3.69 COL 14 COLON-ALIGNED
          LABEL "Prox. Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_UltPago AT ROW 4.5 COL 14 COLON-ALIGNED
          LABEL "Último Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_FecAbo AT ROW 4.5 COL 39 COLON-ALIGNED
     Creditos.Fec_UltLiquidacion AT ROW 5.31 COL 14 COLON-ALIGNED
          LABEL "Última Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_DocAbo AT ROW 5.58 COL 39 COLON-ALIGNED
     W_SdoDeuda AT ROW 6.12 COL 14 COLON-ALIGNED
     W_Valor AT ROW 6.65 COL 39 COLON-ALIGNED
     Creditos.Sdo_Proyectado AT ROW 6.92 COL 14 COLON-ALIGNED
          LABEL "Saldo Proyectado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 7.73 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Val_Atraso AT ROW 8.54 COL 14 COLON-ALIGNED
          LABEL "Valor Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Btn_Acepta_Terminos AT ROW 9.08 COL 74
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
     Btn_Salir AT ROW 10.96 COL 74
     Creditos.Fec_Reestructurado AT ROW 11.77 COL 14 COLON-ALIGNED
          LABEL "Reestructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información del Crédito" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 1.15 COL 3
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 1.54 COL 2
     RECT-298 AT ROW 2.88 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 12.08
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
  CREATE WINDOW WCos ASSIGN
         HIDDEN             = YES
         TITLE              = "Cargos por Cobro Jurídico, Programa W-ProRec_Costas.W"
         COLUMN             = 1.57
         ROW                = 6.46
         HEIGHT             = 12.08
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB WCos 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WCos
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Cre
   FRAME-NAME                                                           */
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
/* SETTINGS FOR FILL-IN Creditos.Fec_ProxLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_NomAbo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoDeuda IN FRAME F_Cre
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WCos)
THEN WCos:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _Query            is NOT OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WCos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WCos WCos
ON END-ERROR OF WCos /* Cargos por Cobro Jurídico, Programa W-ProRec_Costas.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WCos WCos
ON WINDOW-CLOSE OF WCos /* Cargos por Cobro Jurídico, Programa W-ProRec_Costas.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Acepta_Terminos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acepta_Terminos WCos
ON CHOOSE OF Btn_Acepta_Terminos IN FRAME F_Cre /* Aceptar Nuevos Términos */
DO:
    vTime = TIME.

    DO WITH FRAME F_Cre:
        Wcos:ALWAYS-ON-TOP = NO.
        ASSIGN FRAME F_Cre W_NitAbo W_FecAbo W_DocAbo W_Valor Cmb_Tipo.

        IF CtaCre EQ ? OR CtaDeb EQ ? THEN DO:
            MESSAGE "No se han encontrado las cuentas Débito y Crédito" SKIP
                    "para el cargo de referencia!"
                VIEW-AS ALERT-BOX ERROR.
            RELEASE Creditos.
            APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
        END.

        IF W_NitAbo:SCREEN-VALUE EQ "" OR W_FecAbo:SCREEN-VALUE EQ "?" OR W_DocAbo:SCREEN-VALUE EQ "" THEN DO:
            MESSAGE "El nit/documento/fecha se encuentra se encuentran en blanco" SKIP
                    "no se aceptan los nuevos terminos"
                VIEW-AS ALERT-BOX.
            APPLY "entry" TO W_Valor IN FRAME F_Cre.
            RETURN NO-APPLY.
        END.

        IF W_Valor EQ 0 THEN DO:
            MESSAGE "El valor de " Cmb_Tipo SKIP
                    "No puede ser cero. Rectifique"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO W_Valor IN FRAME F_Cre.
            RETURN NO-APPLY.
        END.

        IF AVAIL(Creditos) THEN
            FIND CURRENT Creditos NO-ERROR.
        ELSE
            RETURN NO-APPLY.

        RUN Transaccion NO-ERROR.

        /* Imprimir en pantalla */
        DEFINE VAR Listado AS CHARACTER INITIAL "".
        DEFINE VAR Tamano  AS INTEGER   INITIAL 2.

        listado = W_PathSpl + "L_Usuar.Lst".
        {Incluido\Imprimir.i "Listado" Tamano}

        Wcos:ALWAYS-ON-TOP = YES.
        FIND CURRENT Creditos NO-LOCK NO-ERROR.
        APPLY "choose" TO Btn_Salir.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir WCos
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Button 176 */
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


&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo WCos
ON MOUSE-SELECT-CLICK OF Cmb_Tipo IN FRAME F_Cre /* Tipo de Operación a Realizar */
DO:
  ASSIGN Cmb_Tipo.
  CtaHon = "".
  CASE SELF:SCREEN-VALUE:
    WHEN "Cargo de Costas" THEN 
         ASSIGN P_CodOpe      = 020102005
                W_Valor:LABEL = "Costas"
                CtaDeb        = CortoLargo.Cta_CostasDB
                CtaCre        = Cta_CostasCR.
    WHEN "Cargo de Honorarios" THEN DO:
         ASSIGN P_CodOpe      = 020102003
                W_Valor:LABEL = "Honorarios"
                CtaDeb        = CortoLargo.Cta_HonorariosDB
                CtaCre        = CortoLargo.Cta_HonorariosCR.
    END.
    WHEN "Cargo de Polizas" THEN 
         ASSIGN P_CodOpe      = 020102004
                W_Valor:LABEL = "Polizas"
                CtaDeb        = CortoLargo.Cta_PolizasDB
                CtaCre        = CortoLargo.Cta_PolizasCR.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitAbo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitAbo WCos
ON LEAVE OF W_NitAbo IN FRAME F_Cre /* Nit Abogado */
DO:
  FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE (Clientes) THEN
     ASSIGN W_NomAbo:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  ELSE
  DO:
    RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
    ASSIGN W_NomAbo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = P_Nombre
           W_NitAbo:SCREEN-VALUE    IN FRAME {&FRAME-NAME} = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WCos 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects WCos  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Local WCos 
PROCEDURE Contabilizar_Local :
CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Creditos.Agencia
       Mov_Contable.Comprobante = W_Cbte
       Mov_Contable.Cuenta = CtaDeb
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = Cmb_Tipo
       Mov_Contable.Usuario = W_Usuario
       Mov_contable.Nit = Creditos.Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Num_Documento = W_NumCbt
       Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
       Mov_contable.Enlace = STRING(W_NumCbt)
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.DB = W_ValEfe NO-ERROR.

ASSIGN ROWID_TAQ = ROWID(mov_contable).
W_SecuenciaImprimir = W_NumCbt.

IF ERROR-STATUS:ERROR THEN do:
    MESSAGE "Error Contabilizando."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ERROR.
END.

/*contrapartida*/
CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Creditos.Agencia
       Mov_Contable.Comprobante = W_Cbte
       Mov_Contable.Cuenta = CtaCre
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = Cmb_Tipo
       Mov_Contable.Usuario = W_Usuario
       Mov_contable.Nit = W_NitAbo
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Num_Documento = W_NumCbt
       Mov_Contable.Doc_Referencia = W_DocAbo
       Mov_contable.Enlace = STRING(W_NumCbt)
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.CR = W_ValEfe NO-ERROR.

IF ERROR-STATUS:ERROR THEN do:
    MESSAGE "Error Contabilizando."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ERROR.
END.
/*ELSE MESSAGE "Comprobante:" mov_contable.num_documento mov_contable.agencia
      mov_contable.fec_contable VIEW-AS ALERT-BOX.*/
/*ELSE
     RUN formatos.r (INPUT "NOTA2MUL", ROWID_taq, 0, 0, "CARGOS POR " + Cmb_Tipo:SCREEN-VALUE IN FRAME F_Cre, w_secuenciaimprimir, 0).*/

/*IF W_Agencia NE 10 THEN RUN Contabilizar_SucAge.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_SucAge WCos 
PROCEDURE Contabilizar_SucAge :
/*DEFINE VAR AgeDes LIKE Agencias.Agencia.
DEFINE VAR Valor  LIKE Agencias.Agencia.  
DEFINE VAR ValAux LIKE Ahorros.Sdo_Disponible.
ASSIGN FRAME F_Cre Cmb_Tipo.

  ASSIGN AgeDes = 10.
  FIND Comprobantes WHERE
       Comprobantes.Agencia     EQ Creditos.Agencia AND
       Comprobantes.Comprobante EQ 4 NO-ERROR.
  IF NOT AVAILABLE Comprobantes THEN DO:                           
     IF LOCKED Comprobantes THEN RETRY.                                                     
     RUN MostrarMensaje IN W_Manija (INPUT 72, OUTPUT W_Eleccion). 
     RETURN ERROR.                                                 
  END. 
  ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1                 
         Comprobantes.Secuencia = Comprobantes.Secuencia + 1.   
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR. 

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Creditos.Agencia
         Mov_Contable.Comprobante    = 4
         Mov_Contable.Cuenta         = CtaCre
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Traslado Costas"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = W_NitAbo
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = 10
         Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
         Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
         Mov_contable.Enlace         = ""
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.DB             = W_Valor NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  ASSIGN ROWID_TAQ = ROWID(mov_contable).

  /*contrapartida*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Creditos.Agencia
         Mov_Contable.Comprobante    = 4
         Mov_Contable.Cuenta         = "27059501"
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Sucursales y Agencias"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = "010"
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = 10
         Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
         Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
         Mov_contable.Enlace         = ""
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.CR             = W_Valor NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
 /* ELSE MESSAGE "Comprobante:" mov_contable.num_documento mov_contable.agencia
      mov_contable.fec_contable VIEW-AS ALERT-BOX.*/

  ELSE RUN formatos.r (INPUT "NOTA2MUL", ROWID_taq, 0, 0, "TRASLADO DE " + Cmb_Tipo:SCREEN-VALUE IN FRAME F_Cre, W_NumCbt, 0).
/**/
  FIND Comprobantes WHERE
       Comprobantes.Agencia     EQ 10 AND
       Comprobantes.Comprobante EQ 4 NO-ERROR.
  IF NOT AVAILABLE Comprobantes THEN DO:                           
     IF LOCKED Comprobantes THEN RETRY.                                                     
     RUN MostrarMensaje IN W_Manija (INPUT 72, OUTPUT W_Eleccion). 
     RETURN ERROR.                                                 
  END. 
  ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1                 
         Comprobantes.Secuencia = Comprobantes.Secuencia + 1.   
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR. 

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = 10
         Mov_Contable.Comprobante    = 4
         Mov_Contable.Cuenta         = "27059501"
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Sucursales y Agencias"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = STRING(Creditos.Agencia,"999")
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = 010
         Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
         Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
         Mov_contable.Enlace         = ""
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.DB             = W_Valor NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  ASSIGN ROWID_TAQ = ROWID(mov_contable).
  IF Cmb_Tipo = "Cargo de Honorarios" THEN DO:
      FIND Clientes WHERE Clientes.Nit EQ W_NitAbo NO-LOCK NO-ERROR.
      IF AVAILABLE(Clientes) THEN DO:
         IF Clientes.Tipo_Cliente LT 3 THEN
            ASSIGN CtaHon = "24451501"
                   ValAux = W_Valor * 0.1.
         ELSE
            ASSIGN CtaHon = "24451502"
                   ValAux = W_Valor * 0.11.
      END.
      ELSE DO:
         MESSAGE "El Nit que identifica el crédito" SKIP
                 "no existe en la tabla de clientes" SKIP
                 "verifique esta inconsistencia." VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      END.
      /*contrapartida*/
      CREATE Mov_Contable.
      ASSIGN Mov_Contable.Agencia        = 10
             Mov_Contable.Comprobante    = 4
             Mov_Contable.Cuenta         = CtaHon
             Mov_Contable.Fec_Contable   = W_Fecha
             Mov_Contable.Comentario     = "Traslado Cos/Hon/Pol"
             Mov_Contable.Usuario        = W_Usuario
             Mov_contable.Nit            = W_NitAbo
             Mov_Contable.Cen_Costos     = 999
             Mov_Contable.Destino        = 010
             Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
             Mov_Contable.Doc_Referencia = W_DocAbo
             Mov_contable.Enlace         = ""
             Mov_Contable.Fec_Grabacion  = TODAY
             Mov_Contable.Hora           = TIME
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.CR             = ValAux NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
      CREATE Mov_Contable.
      ASSIGN Mov_Contable.Agencia        = 10
             Mov_Contable.Comprobante    = 4
             Mov_Contable.Cuenta         = CtaCre
             Mov_Contable.Fec_Contable   = W_Fecha
             Mov_Contable.Comentario     = "Traslado Cos/Hon/Pol"
             Mov_Contable.Usuario        = W_Usuario
             Mov_contable.Nit            = W_NitAbo
             Mov_Contable.Cen_Costos     = 999
             Mov_Contable.Destino        = 10
             Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
             Mov_Contable.Doc_Referencia = W_DocAbo
             Mov_contable.Enlace         = ""
             Mov_Contable.Fec_Grabacion  = TODAY
             Mov_Contable.Hora           = TIME
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.CR             = W_Valor - ValAux NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.
  ELSE DO:
      /*contrapartida*/
      CREATE Mov_Contable.
      ASSIGN Mov_Contable.Agencia        = 10
             Mov_Contable.Comprobante    = 4
             Mov_Contable.Cuenta         = CtaCre
             Mov_Contable.Fec_Contable   = W_Fecha
             Mov_Contable.Comentario     = "Traslado Cos/Hon/Pol"
             Mov_Contable.Usuario        = W_Usuario
             Mov_contable.Nit            = W_NitAbo
             Mov_Contable.Cen_Costos     = 999
             Mov_Contable.Destino        = 10
             Mov_Contable.Num_Documento  = INTEGER(W_NumCbt)
             Mov_Contable.Doc_Referencia = W_DocAbo
             Mov_contable.Enlace         = ""
             Mov_Contable.Fec_Grabacion  = TODAY
             Mov_Contable.Hora           = TIME
             Mov_Contable.Estacion       = W_Estacion
             Mov_Contable.CR             = W_Valor NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
      /*ELSE
          MESSAGE "Comprobante:" mov_contable.num_documento mov_contable.agencia
              mov_contable.fec_contable VIEW-AS ALERT-BOX.*/
  END.
/*  MESSAGE "Comprobante:" mov_contable.num_documento mov_contable.agencia
      mov_contable.fec_contable VIEW-AS ALERT-BOX.*/

  RUN formatos.r (INPUT "NOTA2MUL", ROWID_taq, 0, 0, "TRASLADO DE " + Cmb_Tipo:SCREEN-VALUE IN FRAME F_Cre, W_NumCbt, 0).
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CortoLargoCreditos WCos 
PROCEDURE CortoLargoCreditos :
FIND CortoLargo WHERE CortoLargo.Agencia         EQ Creditos.Agencia
                 AND   CortoLargo.Clase_Producto EQ 2
                 AND   CortoLargo.Cod_Producto   EQ Creditos.Cod_Credito
                 AND   CortoLargo.Cta_ContingenteDB NE "" 
                 AND   CortoLargo.Cta_ContingenteCR NE "" 
                 AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
              
 IF AVAILABLE(CortoLargo) THEN DO:
    ASSIGN W_Cbte       = CortoLargo.Comprobante
           CtaDeb       = CortoLargo.Cta_CostasDB
           CtaCre       = CortoLargo.Cta_CostasCR
           W_CtaSyA_Des = CortoLargo.Cta_SYA.
           
    IF W_CtaSyA_Des EQ "" THEN DO:
       MESSAGE "No esta configurada la cuenta de sucursales y agencias" SKIP
               "para la agencia de trabajo actual" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WCos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WCos)
  THEN DELETE WIDGET WCos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WCos  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Tipo W_NitAbo W_NomAbo W_FecAbo W_DocAbo W_SdoDeuda W_Valor 
      WITH FRAME F_Cre IN WINDOW WCos.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_Desembolso Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion 
          Creditos.Fec_UltPago Creditos.Fec_UltLiquidacion 
          Creditos.Sdo_Proyectado Creditos.Cuo_Pagadas Creditos.Val_Atraso 
          Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision 
          Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW WCos.
  ENABLE RECT-296 RECT-298 Cmb_Tipo W_NitAbo W_FecAbo W_DocAbo W_Valor 
         Btn_Acepta_Terminos Btn_Salir 
      WITH FRAME F_Cre IN WINDOW WCos.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  VIEW WCos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject WCos 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovCreditos WCos 
PROCEDURE Gra_MovCreditos :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos y en
                  PlanPagos.       
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
         Mov_Creditos.Descrip        = Cmb_Tipo
         Mov_Creditos.Hora           = vTime
         Mov_Creditos.Pagare         = Creditos.Pagare
         Mov_Creditos.Cpte           = W_Cbte
         Mov_Creditos.Num_Documento  = C_Dto
         Mov_Creditos.Agencia        = C_Agencia
         Mov_Creditos.Ofi_Fuente     = C_OfiFte
         Mov_Creditos.Ofi_Destino    = C_OfiDest
         Mov_Creditos.Num_Credito    = C_NumCre
         Mov_Creditos.Usuario        = C_Usuario
         Mov_Creditos.Val_Cheque     = 0
         Mov_Creditos.Val_Efectivo   = C_VlrEfe
         Mov_Creditos.Sdo_Capital    = Creditos.Sdo_Capital
         Mov_Creditos.INT_Corrientes = Creditos.INT_Corrientes
         Mov_Creditos.INT_MorCobrar  = Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob.

  FIND LAST PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1  NO-ERROR.
                        
  ASSIGN PlanPagos.Cargos_Acum = PlanPagos.Cargos_Acum + C_VlrEfe
         PlanPagos.Cargos_Pdo  = PlanPagos.Cargos_Pdo  + C_VlrEfe.
         
  FIND CURRENT PlanPagos NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject WCos 
PROCEDURE initializeObject :
RUN SUPER.
  ASSIGN WCos:TITLE = P_NomIns
         P_CodOpe   = 020102005
         W_FecAbo:SCREEN-VALUE IN FRAME F_Cre = STRING(W_FEcha). /*operacion inicia en costas*/
  
  FIND Creditos WHERE
       Creditos.Tip_Credito EQ P_TipCre AND
       Creditos.Cod_Credito EQ P_CodCre AND
       Creditos.Num_Credito EQ P_NumCre AND
       Creditos.Nit         EQ P_NitCli NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     /*IF Creditos.Agencia NE W_Agencia THEN DO:
        MESSAGE "No se puede realizar el cargo de Costas/Honorarios/Polizas" SKIP
                "No se encuentra en la agencia a la cual pertenece el crédito" 
                VIEW-AS ALERT-BOX ERROR.
       DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
     END.                                            */
     
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.          
     FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
                             Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN 
        RUN CortoLargoCreditos NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito WCos 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
          Creditos.Fec_ProxLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_ProxLiquidacion)
          Creditos.Fec_UltLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_UltLiquidacion)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
          Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
          Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
          Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado)
          W_SdoDeuda:SCREEN-VALUE = STRING(Creditos.Honorarios     + Creditos.Costas + Creditos.Polizas +
                                           Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob +
                                           Creditos.Int_DifCobro   + Creditos.Int_Corrientes +
                                           Creditos.Sdo_Capital    - Creditos.Int_Anticipado).
          
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir WCos 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    
    ASSIGN FRAME F_Cre W_NitAbo W_NomAbo W_FecAbo W_DocAbo W_Valor.
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    ASSIGN W_Cliente    = Creditos.Nit + " - " + Clientes.Nombre + " " + 
                          Clientes.Apellido1 + " " + Clientes.Apellido2
           W_EncColumna = "Cliente Solicitante         :   " + W_Cliente
           W_Reporte    = "COSTAS JURIDICAS   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DISPLAY 
     "=============================================DATOS GENERALES DEL CREDITO==============================================" AT 1
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
     
     "INFORMACION DE LA COSTA ASENTADA" AT 1
     "Nit Abogado        :"           AT 1
     W_NitAbo                         AT 25
     "Nombre             :"           AT 1
     W_NomAbo                         AT 25
     "Fecha Costa        :"           AT 1
     W_FecAbo                         AT 25
     "Documento          :"           AT 1
     W_DocAbo                         AT 25
     "Valor Costa        :"           AT 1
     W_Valor                         AT 25
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion WCos 
PROCEDURE Transaccion :
DO TRANSACTION:   /* ON ERROR UNDO*/
    ASSIGN W_ValEfe = W_Valor.

    FIND Comprobantes WHERE Comprobante.Agencia EQ W_Agencia
                        AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
    IF AVAILABLE Comprobantes THEN
        ASSIGN W_NumCbt = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

    RUN Gra_MovCreditos(INPUT P_CodOpe,
                        INPUT Creditos.Cod_Credito,
                        INPUT Creditos.Num_Credito,
                        INPUT W_NumCbt,
                        INPUT Creditos.Agencia,
                        INPUT W_Agencia,
                        INPUT Creditos.Agencia,
                        INPUT W_Usuario,
                        INPUT 0,
                        INPUT W_ValEfe).

    CASE Cmb_Tipo:
        WHEN "Cargo de Costas" THEN Creditos.Costas = Creditos.Costas + W_ValEfe.
        WHEN "Cargo de Honorarios" THEN Creditos.Honorarios = Creditos.Honorarios + W_ValEfe.
        WHEN "Cargo de Polizas" THEN Creditos.Polizas = Creditos.Polizas + W_ValEfe.
    END CASE.

    RUN Contabilizar_Local NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al contabilizar"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

