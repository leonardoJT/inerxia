&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{Incluido\VARIABLE.I "SHARED"}
{Incluido\VARCON.I "SHARED"}
{Incluido\pdf_inc.i "NOT SUPER"}

DEFINE VAR WK_CtaProDeb AS CHARACTER.
DEFINE VAR WK_CtaProIntGto AS CHARACTER.
DEFINE VAR WK_CtaProCosGto AS CHARACTER.
DEFINE VAR i AS INTEGER FORMAT "999".
DEFINE VAR W_IntMora AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR W_Aportes AS DECIMAL.
DEFINE VAR W_ApoDesc AS DECIMAL.

DEFI TEMP-TABLE CopMov_Inst LIKE Mov_Instancias
    FIELD W_RowidMI AS ROWID
    INDEX IxPpal Nit Cuenta estado Num_Solicitud.

DEFINE TEMP-TABLE TSdos
    FIELD Age AS INTEGER
    FIELD Cta AS CHARACTER
    FIELD Aju AS CHARACTER
    FIELD Id  AS CHARACTER FORMAT "X(2)"
    FIELD Sdo AS DECIMAL
    FIELD SiC AS LOGICAL.

DEFINE VAR Op_IntAnt AS CHARACTER INITIAL "020101005".
DEFINE VAR TA_IntCte AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR TA_IntAnt AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR TA_DifCob AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR TA_IntMor AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR TA_MorDif AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR secuenciaContable AS INTEGER.
DEFINE VAR codComprobante AS INTEGER.
DEFINE VAR tasaLiquidacion AS DECIMAL.
DEFINE VAR tasaUsura AS DECIMAL.
DEFINE VAR dias_aLiquidar AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_Error AS LOGICAL.
DEFINE VAR W_HayAlgunErr AS LOGICAL.
DEFINE VAR W_SiProceso AS LOGICAL.
DEFINE VAR W_DiaDdc AS INTEGER.

DEFINE TEMP-TABLE Tmp-Conta
    FIELD WC_Age AS INTEGER
    FIELD WC_CodPdt AS INTEGER
    FIELD WC_ForPag AS INTEGER
    FIELD WC_Nit AS CHARACTER
    FIELD WC_IntCte AS DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
    FIELD WC_IntAnt AS DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
    FIELD WC_DifCob AS DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
    FIELD WC_IntMor AS DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
    INDEX idx1 WC_Age WC_CodPdt WC_ForPag.

/* oakley */

DEFINE TEMP-TABLE Tmp-CalifInt
    FIELD C_Age LIKE Agencias.Agencia
    FIELD C_Tipo LIKE Creditos.Tip_Credito
    FIELD C_CtaCal LIKE Cuentas.Cuenta
    FIELD C_CodVar LIKE Varios.Codigo
    FIELD C_Valor AS DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

DEFINE TEMP-TABLE Tmp-CalifConting
    FIELD C_Age LIKE Agencias.Agencia
    FIELD C_Tipo LIKE Creditos.Tip_Credito

    /* oakley */

   FIELD C_CtaCal          LIKE Cuentas.Cuenta
   FIELD Cruce             LIKE Cuentas.Cuenta
   FIELD C_CodVar          LIKE Varios.Codigo
   FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

  DEFINE TEMP-TABLE Tmp-CalifCos
   FIELD C_Age             LIKE Agencias.Agencia
   FIELD C_Tipo            LIKE Creditos.Tip_Credito
   FIELD C_CtaCal          LIKE Cuentas.Cuenta
   FIELD C_CodVar          LIKE Varios.Codigo
   FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

/*tablas temporales para la provision*/
  DEFINE TEMP-TABLE Tmp-Provis
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_Tipo            LIKE Creditos.Tip_Credito
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD Cruce             LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

  DEFINE TEMP-TABLE Tmp-ProvisDet
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_CodCre          LIKE Creditos.Cod_Credito
   FIELD P_NumCre          LIKE Creditos.Num_Credito
   FIELD P_Nit             LIKE Clientes.Nit
   FIELD P_CodCal          LIKE Creditos.Cod_Califica
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".


  DEFINE TEMP-TABLE Tmp-ProvisInt
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_Tipo            LIKE Creditos.Tip_Credito
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD P_CtaGto          LIKE Cuentas.Cuenta
   FIELD P_CtaIng          LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

  DEFINE TEMP-TABLE Tmp-ProvisCos
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_Tipo            LIKE Creditos.Tip_Credito
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD P_CtaIng          LIKE Cuentas.Cuenta
   FIELD P_CtaGto          LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".


  DEFINE TEMP-TABLE Tmpi
   FIELD T_Age              LIKE Agencias.Agencia
   FIELD T_Tpd              AS INT FORM 9
   FIELD T_Pdt              LIKE Creditos.Cod_Credito
   FIELD T_NCr              LIKE Creditos.Num_Credito
   FIELD T_Pagare           LIKE Creditos.Pagare
   FIELD T_Aso              LIKE Clientes.Tipo_Vinculo
   FIELD T_Nit              LIKE Clientes.Nit
   FIELD T_CtaCre           LIKE Cuentas.Cuenta
   FIELD T_CtaCal           LIKE Cuentas.Cuenta
   FIELD T_DiaAtr           LIKE Creditos.Dias_Atraso FORM "-9999"
   FIELD T_Catego           LIKE Creditos.Categoria
   FIELD T_ValGar           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_SdoCap           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_Provis           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_IntCte           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_IntAnt           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"   
   FIELD T_DifCob           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_IntMor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_IntMorAmor       AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_IMorDifC         AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_BaseLiq          AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
   FIELD T_TasaLiq          AS   DECIMAL FORMAT "->>>>>9.999999"
   FIELD T_TasaDes          AS   DECIMAL FORMAT "->>>>>9.999999"
   FIELD T_BaseMora         AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

/*variable para manejar la instancia en la que debe quedar un credito*/
DEFINE VAR W_AsigInst      LIKE Instancias.Instancia.

DEFINE TEMP-TABLE TCbtes
  FIELD CAge LIKE Agencias.Agencia
  FIELD CNum LIKE Comprobantes.Secuencia.

/*DEFINE TEMP-TABLE TPlan LIKE PlanPagos.*/

/*para calificacion y provision*/
DEFINE TEMP-TABLE Pro
    FIELD Agencia     LIKE Creditos.Agencia
    FIELD Nit         LIKE Creditos.Nit
    FIELD Cod_Credito LIKE Creditos.Cod_Credito
    FIELD Pagare      LIKE Creditos.Pagare
    FIELD Num_Credito LIKE Creditos.Num_Credito
    FIELD Sdo_Capital LIKE Creditos.Sdo_Capital
    FIELD Provision   LIKE Creditos.Provision
    FIELD ProvInt     LIKE Creditos.Provision
    FIELD ProvCos     LIKE Creditos.Provision
    FIELD Cal_Credito LIKE Creditos.Cod_Califica
    FIELD Cal_Cliente LIKE Clientes.Calificacion
    FIELD ApoDistribu LIKE Ahorros.Sdo_Disponible
    FIELD ValDefecto  LIKE Ahorros.Sdo_Disponible
    FIELD TotAportes  LIKE Ahorros.Sdo_Disponible
    FIELD Reestru     LIKE Clientes.Reestructurado
    FIELD ValGarant   LIKE Ahorros.Sdo_Disponible
    FIELD diamora     LIKE Creditos.Dias_atraso
    FIELD CuentaCon   LIKE Cuentas.Cuenta.


DEFINE TEMP-TABLE TotCre
    FIELD Nit LIKE Creditos.Nit
    FIELD Tot LIKE Creditos.Sdo_Capital.

DEFINE VARIABLE W_UsuDes LIKE Usuarios.Usuario.                
DEFINE VAR diasPeriodo AS INTEGER.
def var P_NMeses as int.
def var Periodos as INT.
def var P_NomPer as CHAR.
DEF VAR w_TasaEA AS DEC FORMAT "->>>>>>9.99999999".
DEF VAR w_TasaNA AS DEC FORMAT "->>>>>>9.99999999".
DEFINE VAR vTime AS INTEGER.

DEFINE TEMP-TABLE tt_correos
    FIELD nit AS CHARACTER
    FIELD email AS CHARACTER.

/************************************************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Lq

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-290 Cmb_Agencias BUTTON-166 ~
Btn_Ejecutar BtnDone 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias WDia WMes WAno wproc 

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
     SIZE 14 BY 1.38
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ejecutar 
     LABEL "Ejecutar" 
     SIZE 14 BY 1.38.

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 166" 
     SIZE 14 BY 1.38.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(35)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WAno AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WDia AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Día" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WMes AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wproc AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Procesando la Agencia" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 6.19.

DEFINE VARIABLE P_Age AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE P_Nit AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE P_NroCre AS INTEGER FORMAT "999999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Mensaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Lq
     Cmb_Agencias AT ROW 1.54 COL 10 COLON-ALIGNED
     WDia AT ROW 2.88 COL 10 COLON-ALIGNED
     WMes AT ROW 2.88 COL 19 COLON-ALIGNED
     WAno AT ROW 2.88 COL 29 COLON-ALIGNED
     wproc AT ROW 3.96 COL 36 COLON-ALIGNED
     BUTTON-166 AT ROW 4.77 COL 55
     Btn_Ejecutar AT ROW 6.15 COL 55
     BtnDone AT ROW 8.81 COL 55
     "Revisión 04/04/2013" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 3.15 COL 50.29 WIDGET-ID 2
          FONT 3
     RECT-290 AT ROW 4.23 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69 BY 9.85
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Progreso
     P_Age AT ROW 2.04 COL 5 NO-LABEL
     P_NroCre AT ROW 2.04 COL 8 COLON-ALIGNED NO-LABEL
     P_Nit AT ROW 2.04 COL 19.72 COLON-ALIGNED NO-LABEL
     W_Mensaje AT ROW 3.12 COL 3 COLON-ALIGNED NO-LABEL
     "Age" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.23 COL 6
     "Nro.Credito" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.23 COL 10.29
     "Nit Cliente" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.23 COL 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 5.85
         SIZE 36.57 BY 4.04
         BGCOLOR 17 
         TITLE "Progreso".


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
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Proceso de Liquidacion de Créditos"
         HEIGHT             = 9.85
         WIDTH              = 69
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
ASSIGN FRAME F_Progreso:FRAME = FRAME F_Lq:HANDLE.

/* SETTINGS FOR FRAME F_Lq
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN WAno IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WDia IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WMes IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wproc IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN P_Age IN FRAME F_Progreso
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       P_Age:HIDDEN IN FRAME F_Progreso           = TRUE.

/* SETTINGS FOR FILL-IN P_Nit IN FRAME F_Progreso
   NO-ENABLE                                                            */
ASSIGN 
       P_Nit:HIDDEN IN FRAME F_Progreso           = TRUE.

/* SETTINGS FOR FILL-IN P_NroCre IN FRAME F_Progreso
   NO-ENABLE                                                            */
ASSIGN 
       P_NroCre:HIDDEN IN FRAME F_Progreso           = TRUE.

/* SETTINGS FOR FILL-IN W_Mensaje IN FRAME F_Progreso
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Proceso de Liquidacion de Créditos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Proceso de Liquidacion de Créditos */
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
ON CHOOSE OF BtnDone IN FRAME F_Lq /* Salir */
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


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME F_Lq /* Ejecutar */
DO:
    vTime = TIME.

    /* Validamos que el proceso haya sido realizado el día anterior y que no se haya realizado ya para este día */
    FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia GE W_OfiIni
                        AND Agencias.Agencia LE W_OfiFin NO-LOCK:
        FIND FIRST ProcDia WHERE ProcDia.Agencia EQ Agencias.Agencia
                             AND ProcDia.Cod_Proceso EQ 1
                             AND ProcDia.Fecha_Proc EQ W_Fecha - 1
                             AND ProcDia.Estado EQ 2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(ProcDia) THEN DO:
            MESSAGE "Este proceso no ha sido ejecutado para el día anterior en la agencia:" Agencias.Agencia
                VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

            RETURN.
        END.
    END.

    FOR EACH agencias WHERE agencias.estado <> 3
                        AND agencias.agencia >= w_ofiIni
                        AND agencias.agencia <= w_ofiFin NO-LOCK:
        FIND FIRST ProcDia WHERE ProcDia.Agencia EQ Agencias.Agencia
                             AND ProcDia.Cod_Proceso EQ 1
                             AND ProcDia.Fecha_Proc EQ w_fecha
                             AND ProcDia.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(ProcDia) THEN DO:
            MESSAGE "Este proceso ya fue ejecutado para este día en la agencia:" Agencias.Agencia SKIP
                    "o no está matriculado... Revise por favor. No se permite la operación."
                VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

            RETURN.
        END.
    END.

    ASSIGN FRAME F_Lq
        Cmb_Agencias
        WMes
        WAno.

    W_Error = FALSE.

    SESSION:SET-WAIT-STATE("General").

    FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia GE W_OfiIni
                        AND Agencias.Agencia LE W_OfiFin NO-LOCK:    
        RUN Verificar_PRC_dia(INPUT agencias.agencia,
                              INPUT WMes,
                              INPUT WAno,
                              INPUT 8,
                              INPUT 1,
                              INPUT W_Fecha,
                              INPUT FALSE,
                              OUTPUT codComprobante,
                              OUTPUT W_Error).
        IF W_HayAlgunErr THEN DO:
            MESSAGE "El proceso Presenta errores de configuracion " skip
                    "Por lo tanto no se podra correr el proceso " 
                VIEW-AS ALERT-BOX TITLE "Validación el calendario".

            SESSION:SET-WAIT-STATE("").
            HIDE FRAME F_Procesados.

            RETURN.
        END.
    END.

    RUN Chequeo_Contable. /* oakley */
    IF W_HayAlgunErr THEN DO:
        MESSAGE "El proceso Presenta errores de configuracion " skip
                "Por lo tanto no se podra correr el proceso "
            VIEW-AS ALERT-BOX TITLE "Validación el calendario".
        
        SESSION:SET-WAIT-STATE("").
        HIDE W_Mensaje IN FRAME F_Progreso.
        
        RETURN.
    END.

    W_SiProceso = FALSE.

    /* busco tasa Usura.*/
    FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
    IF AVAILABLE Entidad THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                                 AND Indicadores.Estado EQ 1
                                 AND Indicadores.FecVcto GE W_Fecha NO-LOCK NO-ERROR.
        IF AVAILABLE Indicadores THEN
            tasaUsura = Indicadores.Tasa.
        ELSE DO:
            FIND LAST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                                    AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE Indicadores THEN
                tasaUsura = Indicadores.Tasa.
            ELSE
                tasaUsura = 0.
        END.

        IF tasaUsura GT 0 THEN DO:
            RUN EFNV IN W_ManFin  (INPUT tasaUsura / 100, 12, OUTPUT tasaUsura).
            tasaUsura = tasaUsura * 1200.
        END.
    END.

    ASSIGN dias_aLiquidar = 1  /*Inicia con un día para todos y el dia 30 fin mes*/
           W_Aportes = 0.

    IF DAY(W_Fecha + 1) EQ 1 THEN DO:   /*Mayo 25/05 GAER, Procesando Ultimo dia de mes*/
        IF DAY(W_Fecha) EQ 31 THEN        /*No liquida*/
            dias_aLiquidar = 0.
        ELSE
            IF DAY(W_Fecha) EQ 28 THEN   /*28 de Febrero liquida 3 dias*/
                dias_aLiquidar = 3.
            ELSE
                IF DAY(W_Fecha) EQ 29 THEN   /*29 de Febrero liquida 2 dias*/
                    dias_aLiquidar = 2.
    END.

    FOR EACH TCbtes:
        DELETE TCbtes.
    END.

    /* Créditos Rotativos con saldo 0 y sin aportes, se cancelan */
    RUN cancelarRotativos.

    RUN Liquidacion NO-ERROR.
    SESSION:SET-WAIT-STATE("").
    
    IF W_SiProceso THEN
        MESSAGE "Fin Proceso. Ok" W_siproceso
            VIEW-AS ALERT-BOX.
    ELSE
        MESSAGE "Proceso con Errorres, Debe corregirlo y Reprocesar."
            VIEW-AS ALERT-BOX.

    HIDE W_Mensaje.
    
    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME F_Lq /* Agencia */
DO:
    IF INTEGER(SUBSTRING(cmb_agencias:SCREEN-VALUE,1,3)) = 0 THEN DO:
        w_ofiIni = 0.
        w_ofiFin = 999.
    END.
    ELSE DO:
        w_OfiIni = INTEGER(SUBSTRING(cmb_agencias:SCREEN-VALUE,1,3)).
        w_OfiFin = INTEGER(SUBSTRING(cmb_agencias:SCREEN-VALUE,1,3)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActualizarDiasDeAtraso wWin 
PROCEDURE ActualizarDiasDeAtraso :
DEFINE VAR saldoProyectado AS DECIMAL.
DEFINE VAR w_fechaTr AS DATE.
DEFINE VAR fecFinal AS DATE. /* Fecha donde ponemos la fecha de finalización del crédito */

w_fechaTr = w_fecha + 1.
creditos.val_atraso = 0.
creditos.cuo_atraso = 0.
creditos.dias_atraso = 0.

/* Días de atraso */
IF creditos.fec_pago < w_fechaTr THEN
    creditos.dias_atraso = w_fechaTr - creditos.fec_pago.
ELSE
    creditos.dias_atraso = 0.

/* Valor del atraso - Cuotas atrasadas */
IF creditos.dias_atraso > 0 THEN DO:
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2
                             AND CONTROL_pagos.fec_Vcto <= w_fecha NO-LOCK:
        creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
        creditos.cuo_atraso = creditos.cuo_atraso + 1.
    END.

    IF creditos.val_atraso = 0 THEN DO:
        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito NO-LOCK BY CONTROL_pagos.fec_vcto DESC:
            fecFinal = CONTROL_pagos.fec_vcto.
            LEAVE.
        END.

        IF fecFinal <= w_fecha THEN
            creditos.val_atraso = creditos.sdo_Capital.
    END.

    IF creditos.val_atraso > creditos.sdo_capital THEN
        creditos.val_atraso = creditos.sdo_capital.
END.

/* Saldo proyectado */
saldoProyectado = creditos.monto.

FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                         AND CONTROL_pagos.num_credito = creditos.num_credito
                         AND CONTROL_pagos.fec_vcto <= w_fecha NO-LOCK BY CONTROL_pagos.fec_vcto:
    saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.
END.

IF saldoProyectado < creditos.sdo_capital OR creditos.dias_atraso > 0 THEN
    creditos.sdo_proyectado = saldoProyectado.
ELSE
    creditos.sdo_proyectado = creditos.sdo_capital.

IF creditos.dias_atraso > 30 THEN
    RUN reportarListaSuspendidos.

RUN Calificar NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActualizarDiasDeAtrasoRotativos wWin 
PROCEDURE ActualizarDiasDeAtrasoRotativos :
creditos.val_atraso = 0.
creditos.cuo_atraso = 0.
creditos.dias_atraso = 0.

FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                       AND facturacion.num_credito = creditos.num_credito
                       AND facturacion.fec_pago <= w_fecha
                       AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago DESCENDING:
    creditos.fec_pago = facturacion.fec_pago.
    creditos.val_atraso = creditos.val_atraso + facturacion.capital - facturacion.pago_capital.

    IF creditos.val_atraso < 0 THEN
        creditos.val_atraso = 0.
    
    creditos.cuo_atraso = creditos.cuo_atraso + 1.
    creditos.sdo_proyectado = creditos.sdo_Capital - creditos.val_atraso.
END.

IF creditos.fec_pago < w_fecha THEN
    creditos.dias_atraso = w_fecha - creditos.fec_pago + 1.
ELSE
    creditos.sdo_proyectado = creditos.sdo_capital.

FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                         AND facturacion.num_credito = creditos.num_credito
                         AND facturacion.estado = 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE facturacion THEN DO:
    /* Corremos la fecha de pago en caso que sea el vencimiento y el crédito no tenga facturas pendientes */
    IF w_fecha >= creditos.fec_pago THEN DO:
        IF DAY(w_fecha) <= 16 THEN
            creditos.fec_pago = ADD-INTERVAL(w_fecha,1,"months").
        ELSE
            creditos.fec_pago = ADD-INTERVAL(w_fecha,2,"months").
    END.

    creditos.val_atraso = 0.
    creditos.cuo_atraso = 0.
    creditos.dias_atraso = 0.
END.

IF creditos.dias_atraso > 30 THEN
    RUN reportarListaSuspendidos.

RUN Calificar NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna_Agen11 wWin 
PROCEDURE Asigna_Agen11 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFI VAR W_UsuAsig LIKE Usuarios.Usuario.

 FOR EACH cfg_Instancias WHERE /*busca la instancia del rango de mora */
           Cfg_Instancias.Agencia        EQ 11                     AND
           Cfg_Instancias.Tipo_Instancia EQ 2                      AND
           Cfg_Instancias.Plazo_Minimo   LE Creditos.Dias_Atraso   AND
           Cfg_Instancias.Plazo_Maximo   GE Creditos.Dias_Atraso   AND
           Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
           Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND
           Cfg_Instancias.Estado         EQ 1              NO-LOCK:

      FIND FIRST CopMov_Inst WHERE CopMov_Inst.Nit           EQ Creditos.Nit                 AND
                                   CopMov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) AND
                                   CopMov_Inst.Estado        EQ NO                           AND
                                   CopMov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud       AND
                                   CopMov_Inst.Instancia     EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
      IF  AVAIL(CopMov_Inst) 
      AND CopMov_Inst.Usuario EQ Cfg_Instancias.Usuario
      AND CopMov_Inst.Agencia EQ 11 THEN              
          RETURN.   /*Retorna por Estar correctamente asignado.*/

      FIND FIRST Solicitud WHERE Solicitud.Num_Solicitud EQ Creditos.Num_Solicitud
                             AND Solicitud.Nit           EQ Creditos.Nit NO-LOCK NO-ERROR.
      IF AVAIL(Solicitud) THEN DO:
         FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Solicitud.Usuario
                               AND Usuarios.Estado  EQ 1
                               AND Usuarios.Agencia EQ 11 NO-LOCK NO-ERROR.
         IF AVAIL(CopMov_Inst) AND AVAIL(Usuarios) THEN DO:   /* asigna al de la solicitud.*/
            FIND FIRST Mov_Instancias WHERE ROWID(Mov_Instancias) EQ CopMov_Inst.W_RowidMI NO-ERROR.
            ASSIGN Mov_Instancias.Usuario = Usuarios.Usuario.
            RETURN.
         END.
      END.
      
      IF AVAIL(CopMov_Inst) THEN DO: /*De lo contrario si existe, asigna al configurado.*/
         FIND FIRST Mov_Instancias WHERE ROWID(Mov_Instancias) EQ CopMov_Inst.W_RowidMI NO-ERROR.
         ASSIGN Mov_Instancias.Usuario = Cfg_Instancias.Usuario.
         RETURN.
      END.

      /*y crea la nueva*/
      CREATE Mov_Instancias.                                             
      ASSIGN Mov_Instancias.Fec_Ingreso   = W_Fecha                      
             Mov_Instancias.Hora_Ingreso  = TIME                         
             Mov_Instancias.Nit           = Creditos.Nit                 
             Mov_Instancias.Num_Solicitud = Creditos.Num_Solicitud       
             Mov_Instancias.Usuario       = Cfg_Instancias.Usuario
             Mov_Instancias.Instancia     = Cfg_Instancias.Instancia         
             Mov_Instancias.Cuenta        = STRING(Creditos.Num_Credito) 
             Mov_Instancias.Agencia       = 11.  

      IF AVAIL(Solicitud) AND AVAIL(Usuarios) THEN
         ASSIGN Mov_Instancias.Usuario = Usuarios.Usuario.


      RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarSdoCta wWin 
PROCEDURE BuscarSdoCta :
/*   Halla los Sdos de Balance*/                                        
 DEFINE INPUT  PARAMETER CAge LIKE Agencias.Agencia.
 DEFINE INPUT  PARAMETER CCta LIKE Cuentas.Cuenta.
 DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial INIT 0.

 DEFINE VAR I AS INTEGER.

 FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Cuenta  EQ CCta
                         AND Sal_Cuenta.Ano     EQ YEAR(W_Fecha)        
                         AND Sal_Cuenta.Agencia EQ CAge NO-LOCK NO-ERROR.
 IF AVAILABLE Sal_Cuenta THEN DO:
    FIND Cuentas WHERE Cuentas.Cuenta EQ Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.   
    SFin = Sal_Cuenta.Sal_Inicial.
    DO I = 1 TO MONTH(W_Fecha) BY 1:
       IF Cuentas.Naturaleza EQ "DB" THEN
          SFin  = SFin + Sal_Cuenta.DB[I] - Sal_Cuenta.Cr[I].
       ELSE
          SFin  = SFin - Sal_Cuenta.DB[I] + Sal_Cuenta.Cr[I].
    END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calificar wWin 
PROCEDURE Calificar :
FIND FIRST CarteraVencida WHERE CarteraVencida.Cod_Producto = Creditos.Cod_Credito
                            AND CarteraVencida.Per_Inicial <= Creditos.Dias_Atraso
                            AND CarteraVencida.Per_Final >= Creditos.Dias_Atraso NO-LOCK NO-ERROR.
IF AVAILABLE carteraVencida THEN DO:
    Creditos.Cod_Califica = CarteraVencida.Cod_Califica.
    Creditos.Categoria = CarteraVencida.Categoria.
END.
ELSE DO:
    MESSAGE "No se encuentra la configuración en Cartera Vencida para la morosidad de" creditos.dias_atraso "que presenta el crédito #" STRING(creditos.num_credito) "del Asociado" 
            creditos.nit "." SKIP(2)
            "La calificación el crédito no se modifica."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cal_SdoProyectado wWin 
PROCEDURE Cal_SdoProyectado :
IF Creditos.Val_Atraso GT Creditos.Sdo_capital THEN
    Creditos.Val_Atraso = Creditos.Sdo_capital.

IF (Creditos.Sistema EQ 2 OR Creditos.Plazo EQ 1) AND Creditos.Val_Atraso GT 0 THEN
    Creditos.Cuo_Atraso = 1.

/*calculo de calificacion y provision*/
RUN Calificar NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    RETURN ERROR.
END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelarRotativos wWin 
PROCEDURE cancelarRotativos :
FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.sdo_capital = 0
                    AND creditos.estado = 2:
    FIND FIRST ahorros WHERE ahorros.nit = creditos.nit
                         AND ahorros.tip_ahorro = 4
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ahorros THEN DO:
        creditos.estado = 3.
        creditos.fec_cancetotal = w_fecha.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Chequeo_Contable wWin 
PROCEDURE Chequeo_Contable :
DEFINE VAR W_AuxCodPdt AS INTEGER INITIAL 0.

W_HayAlgunErr = FALSE.

FOR EACH Pro_Creditos FIELDS (Cod_Credito
                              Id_Asociado) WHERE pro_creditos.tip_credito <= 4
                                                         AND Pro_Creditos.Estado = 1
                                                         AND W_HayAlgunErr = FALSE NO-LOCK BREAK BY Pro_Creditos.Cod_Credito:
    /* oakley */
    IF Pro_Creditos.Cod_Credito NE W_AuxCodPdt THEN DO:
        W_AuxCodPdt = Pro_Creditos.Cod_Credito.

        FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                         AND Liqui_Int.Cod_Producto EQ Pro_Creditos.Cod_Credito NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Liqui_Int) THEN DO:
            MESSAGE "El Producto: " + STRING(Pro_Creditos.Cod_Credito) + " no tiene Configurado Cuentas de Liquidacion de Intereses." SKIP
                    "Por favor configurelas y corra el proceso nuevamente"
                VIEW-AS ALERT-BOX TITLE "Verificación de Liquidación de Intereses".

            W_HayAlgunErr = TRUE.
        END.
        ELSE DO:
            IF Id_Asociado = 1 THEN DO:
                RUN Ctas_Asociado.
                IF NOT W_HayAlgunErr THEN
                    RUN Ctas_NoAsociado.
                
                RUN Verif_Error.
            END.
            ELSE DO:
                IF Id_Asociado = 2 THEN DO:
                    RUN Ctas_Asociado.
                    RUN Verif_Error.
                END.
                ELSE DO:
                    RUN Ctas_NoAsociado.
                    RUN Verif_Error.
                END.
            END.
        END.  
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CieMes_HastaFeb1506 wWin 
PROCEDURE CieMes_HastaFeb1506 :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar wWin 
PROCEDURE Contabilizar :
DEFINE VAR SdoCta AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR ValCod AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR CtaDeb LIKE Cuentas.Cuenta.
DEFINE VAR CtaCre LIKE Cuentas.Cuenta.
DEFINE VAR W_Sw AS LOGICAL.
DEFINE VAR W_Des AS CHARACTER FORMAT "X(30)".

FIND FIRST Varios WHERE Varios.Tipo EQ 8
                    AND Varios.Codigo EQ 1 NO-LOCK NO-ERROR.
IF AVAILABLE Varios THEN
    codComprobante = Varios.Comprobante.
ELSE DO:
    MESSAGE "No se encontro la configuracion del proceso" SKIP
            "en la tabla varios"
        VIEW-AS ALERT-BOX ERROR.
        
    RETURN ERROR.
END.

FOR EACH Tmp-Conta BREAK BY Tmp-Conta.WC_Age:
    IF FIRST-OF(Tmp-Conta.WC_Age) THEN DO:
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ Tmp-Conta.WC_Age
                                  AND Comprobantes.Comprobante EQ codComprobante NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            IF LOCKED Comprobantes THEN
                RETRY.

            RUN MostrarMensaje IN W_Manija (INPUT 72, OUTPUT W_Eleccion).

            RETURN ERROR.
        END.

        ASSIGN secuenciaContable = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    END.

    FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                           AND Liqui_Int.Cod_Producto EQ tmp-conta.WC_CodPdt NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Liqui_Int THEN DO:
        MESSAGE "No se encontró la configuración de liquidación de interes" SKIP
                "para el producto: " tmp-conta.WC_CodPdt SKIP
                "Se devuelve el proceso"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    IF Tmp-Conta.WC_ForPag EQ 2 THEN DO: /*Contabilizacion del Interes Anticipado*/
        IF tmp-conta.WC_IntAnt GT 0 THEN
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaInt_AntAso,
                                      INPUT Liqui_Int.CtaCR_LiqAso,
                                      INPUT Op_IntAnt,
                                      INPUT "Int. Anticipados x Nomina",
                                      INPUT codComprobante,
                                      INPUT tmp-conta.WC_IntAnt,
                                      INPUT tmp-conta.WC_Nit).

        IF tmp-conta.WC_DifCob GT 0 THEN /*Contabilizacion del Interes de Dificil cobro*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaDB_DifCobAso,
                                      INPUT Liqui_Int.CtaCR_DifCobAso,
                                      INPUT Op_IntAnt,
                                      INPUT "Int.Dificil Cobro x Nomina",
                                      INPUT codComprobante,
                                      INPUT tmp-conta.WC_DifCob,
                                      INPUT tmp-conta.WC_Nit).

        IF Tmp-Conta.WC_IntMor GT 0 THEN /*Contabilizacion del Interes de Mora por Cobrar*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaDB_MoraAso,
                                      INPUT Liqui_Int.CtaCR_MoraAso,
                                      INPUT Op_IntAnt,
                                      INPUT "Int.Mor.Cobrar x Nomina",
                                      INPUT codComprobante,
                                      INPUT tmp-conta.WC_IntMor,
                                      INPUT tmp-conta.WC_Nit).

        IF Tmp-Conta.WC_IntCte GT 0 THEN /*Contabilizacion del Interes Corriente*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaDB_LiqAso,
                                      INPUT Liqui_Int.CtaCR_LiqAso,
                                      INPUT Op_IntAnt,
                                      INPUT "Int.Corriente x Nomina",
                                      INPUT codComprobante,
                                      INPUT tmp-conta.WC_IntCte,
                                      INPUT tmp-conta.WC_Nit).
    END.
    ELSE DO:
        IF tmp-conta.WC_IntAnt GT 0 THEN /*Contabilizacion del Interes Anticipado*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaInt_Ant,
                                      INPUT Liqui_Int.CtaCR_Liq,
                                      INPUT Op_IntAnt,
                                      INPUT "Int. Anticipados x Caja",
                                      INPUT codComprobante,
                                      INPUT tmp-conta.WC_IntAnt,
                                      INPUT tmp-conta.WC_Nit).

        IF tmp-conta.WC_DifCob GT 0 THEN /*Contabilizacion del Interes de Dificil cobro*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaDB_DifCob,
                                      INPUT Liqui_Int.CtaCR_DifCob,
                                      INPUT Op_IntAnt,
                                      INPUT "Int.Dificil Cobro x Caja",
                                      INPUT codComprobante,
                                      INPUT tmp-conta.WC_DifCob,
                                      INPUT tmp-conta.WC_Nit).

        IF Tmp-Conta.WC_IntMor GT 0 THEN /*Contabilizacion del Interes de Mora por Cobrar*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaDB_Mora,
                                      INPUT Liqui_Int.CtaCR_Mora,
                                      INPUT Op_IntAnt,
                                      INPUT "Int.Mor.Cobrar x Caja",
                                      INPUT codComprobante,
                                      INPUT Tmp-Conta.WC_IntMor,
                                      INPUT tmp-conta.WC_Nit).

        IF Tmp-Conta.WC_IntCte GT 0 THEN /*Contabilizacion del Interes Corriente*/
            RUN Contabilizar_Partidas(INPUT Tmp-Conta.WC_Age,
                                      INPUT Liqui_Int.CtaDB_Liq,
                                      INPUT Liqui_Int.CtaCR_Liq,
                                      INPUT Op_IntAnt,
                                      INPUT "Int.Corriente x Caja",
                                      INPUT codComprobante,
                                      INPUT Tmp-Conta.WC_IntCte,
                                      INPUT tmp-conta.WC_Nit).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas wWin 
PROCEDURE Contabilizar_Partidas :
DEFINE INPUT PARAMETER W_AgeCon LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER W_CtaDeb LIKE Cuentas.Cuenta.
DEFINE INPUT PARAMETER W_CtaCre LIKE Cuentas.Cuenta.
DEFINE INPUT PARAMETER W_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER W_Descripcion AS CHARACTER FORMAT "X(30)".
DEFINE INPUT PARAMETER W_Combte LIKE Comprobantes.Comprobante.
DEFINE INPUT PARAMETER W_Valor AS DECIMAL.
DEFINE INPUT PARAMETER W_Nit AS CHARACTER.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = W_AgeCon
       Mov_Contable.Comprobante = W_Combte
       Mov_Contable.Cuenta = W_CtaDeb
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = W_Descripcion
       Mov_Contable.Usuario = W_Usuario
       /*Mov_contable.Nit = ""*/
       mov_contable.nit = W_Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Num_Documento = secuenciaContable
       Mov_Contable.Doc_Referencia = ""
       Mov_contable.Enlace = ""
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.DB = W_Valor NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = W_AgeCon
       Mov_Contable.Comprobante = W_Combte
       Mov_Contable.Cuenta = W_CtaCre
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = W_Descripcion
       Mov_Contable.Usuario = W_Usuario
       /*Mov_contable.Nit = ""*/
       mov_contable.nit = W_Nit
       Mov_Contable.Cen_Costos = 999
       Mov_Contable.Destino = 0
       Mov_Contable.Num_Documento = secuenciaContable
       Mov_Contable.Doc_Referencia = ""
       Mov_contable.Enlace = ""
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.CR = W_Valor NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Conting wWin 
PROCEDURE Contab_Calificar_Conting :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La ReClasificacion de Intereses-Contingentes.  
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Marzo 7/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.
        
  FOR EACH Tmp-CalifConting BREAK BY Tmp-CalifConting.C_Age BY Tmp-CalifConting.C_CtaCal:
                                                          /*Contab.de Clasific.Interes-Contingente*/
      IF FIRST-OF(Tmp-CalifConting.C_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-CalifConting.C_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de Contingentes para la agencia: " Tmp-CalifConting.C_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-CalifConting.C_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "ReClasif.Int-Contingentes"                                                 
                     codComprobante = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-CalifConting.C_Age AND                                           
                                 Comprobantes.Comprobante EQ codComprobante NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-CalifConting.C_Age SKIP                                                 
                       "Comprobante: " codComprobante VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN secuenciaContable               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-CalifConting.C_Valor.

      IF LAST-OF(Tmp-CalifConting.C_CtaCal) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-CalifConting.C_Age
                               AND TSdos.Cta     EQ Tmp-CalifConting.C_CtaCal
                               AND TSdos.Id      EQ "IC"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TSdos.Sdo GT TCalif THEN        /*Debe restar de Sdos*/
               ASSIGN CtaCre    = Tmp-CalifConting.C_CtaCal
                      SdoCta    = TCalif - TSdos.Sdo   /*Para que el Ajuste sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TSdos.Sdo LT TCalif THEN   /*Debe sumar al Sdo*/
                ASSIGN CtaCre    = Tmp-CalifConting.C_CtaCal
                       SdoCta    = TCalif - TSdos.Sdo   /*Ajuste Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN                /*Son iguales no hay asiento*/
                TSdos.SiC = TRUE. 
            ELSE IF NOT AVAIL(TSdos) THEN            /*Ajuste Se debe crear al DEBE */
               ASSIGN CtaCre = Tmp-CalifConting.C_CtaCal
                      SdoCta = TCalif.
           
            IF SdoCta NE 0 THEN DO:
               RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
               RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,Tmp-CalifConting.Cruce,W_Des,SdoCta * -1) NO-ERROR.
            END.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-CalifConting.C_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Sdos, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-CalifConting.C_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "IC"                   AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,TSdos.Cta,"Ajuste Calif.Conting.",TSdos.Sdo * -1) NO-ERROR.
             RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,Tmp-CalifConting.Cruce,"Ajuste Calif.Conting",TSdos.Sdo) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "IC" AND TSdos.Sdo NE 0:
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final Conting",TSdos.Sdo * -1) NO-ERROR.             
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Aju,"Ajuste Final Conting",TSdos.Sdo) NO-ERROR.        
      ASSIGN TSdos.SiC = TRUE.                                                                            
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Costas wWin 
PROCEDURE Contab_Calificar_Costas :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La ReClasificacion de las Costas.  
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.16/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.
        
  FOR EACH Tmp-CalifCos BREAK BY Tmp-CalifCos.C_Age BY Tmp-CalifCos.C_CtaCal: /*Contab.de la Clasific.Costas*/
      IF FIRST-OF(Tmp-CalifCos.C_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-CalifCos.C_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-CalifCos.C_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-CalifCos.C_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "ReClasif.Costas"                                                 
                     codComprobante = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-CalifCos.C_Age AND                                           
                                 Comprobantes.Comprobante EQ codComprobante NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-CalifCos.C_Age SKIP                                                 
                       "Comprobante: " codComprobante VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN secuenciaContable               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-CalifCos.C_Valor.

      IF LAST-OF(Tmp-CalifCos.C_CtaCal) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-CalifCos.C_Age
                               AND TSdos.Cta     EQ Tmp-CalifCos.C_CtaCal
                               AND TSdos.Id      EQ "CO"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TSdos.Sdo GT TCalif THEN
               ASSIGN CtaCre    = Tmp-CalifCos.C_CtaCal
                      SdoCta    = TCalif - TSdos.Sdo   /*Para que el Ajuste sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TSdos.Sdo LT TCalif THEN
                ASSIGN CtaCre    = Tmp-CalifCos.C_CtaCal
                       SdoCta    = TCalif - TSdos.Sdo /*Ajuste Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN                /*Son iguales no hay asiento*/
                TSdos.SiC = TRUE. 
            ELSE IF NOT AVAIL(TSdos) THEN           /*Ajuste Se debe crear al DEBE */
               ASSIGN CtaCre = Tmp-CalifCos.C_CtaCal
                      SdoCta = TCalif.
           
            IF SdoCta NE 0 THEN 
               RUN Contab_PartNuevas (INPUT Tmp-CalifCos.C_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-CalifCos.C_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Sdo-Costas, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-CalifCos.C_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "CO"               AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-CalifCos.C_Age,TSdos.Cta,"Ajuste Reclasif.Costas",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "CO" AND TSdos.Sdo NE 0:
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste sin Costas",TSdos.Sdo * -1) NO-ERROR.
      ASSIGN TSdos.SiC = TRUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Interes wWin 
PROCEDURE Contab_Calificar_Interes :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La ReClasificacion de Intereses.  
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.16/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.
        
  FOR EACH Tmp-CalifInt BREAK BY Tmp-CalifInt.C_Age BY Tmp-CalifInt.C_CtaCal: /*Contab.de la Clasific.Interes*/
      IF FIRST-OF(Tmp-CalifInt.C_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-CalifInt.C_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-CalifInt.C_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-CalifInt.C_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "ReClasif.Intereses"                                                 
                     codComprobante = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-CalifInt.C_Age AND                                           
                                 Comprobantes.Comprobante EQ codComprobante NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-CalifInt.C_Age SKIP                                                 
                       "Comprobante: " codComprobante VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN secuenciaContable               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-CalifInt.C_Valor.

      IF LAST-OF(Tmp-CalifInt.C_CtaCal) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-CalifInt.C_Age
                               AND TSdos.Cta     EQ Tmp-CalifInt.C_CtaCal
                               AND TSdos.Id      EQ "CI"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TSdos.Sdo GT TCalif THEN
               ASSIGN CtaCre    = Tmp-CalifInt.C_CtaCal
                      SdoCta    = TCalif - TSdos.Sdo   /*Para que el Ajuste sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TSdos.Sdo LT TCalif THEN
                ASSIGN CtaCre    = Tmp-CalifInt.C_CtaCal
                       SdoCta    = TCalif - TSdos.Sdo   /*Ajuste Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN                /*Son iguales no hay asiento*/
                TSdos.SiC = TRUE. 
            ELSE IF NOT AVAIL(TSdos) THEN           /*Ajuste Se debe crear al DEBE */
               ASSIGN CtaCre = Tmp-CalifInt.C_CtaCal
                      SdoCta = TCalif.
           
            IF SdoCta NE 0 THEN 
               RUN Contab_PartNuevas (INPUT Tmp-CalifInt.C_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-CalifInt.C_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Sdo-Intereses, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-CalifInt.C_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "CI"               AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-CalifInt.C_Age,TSdos.Cta,"Ajuste Reclasif.Int",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "CI" AND TSdos.Sdo NE 0:
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste sin Int.",TSdos.Sdo * -1) NO-ERROR.
      ASSIGN TSdos.SiC = TRUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_PartNuevas wWin 
PROCEDURE Contab_PartNuevas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER W_AgeCon      LIKE Agencias.Agencia.
  DEFINE INPUT PARAMETER W_Cta         LIKE Cuentas.Cuenta.
  DEFINE INPUT PARAMETER W_Descripcion AS CHARACTER FORMAT "X(30)".
  DEFINE INPUT PARAMETER W_Valor       LIKE Creditos.Sdo_Capital.

  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = W_AgeCon
         Mov_Contable.Comprobante    = codComprobante
         Mov_Contable.Cuenta         = W_Cta
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = W_Descripcion
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = ""
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = W_Agencia
         Mov_Contable.Num_Documento  = INTEGER(secuenciaContable)
         Mov_Contable.Doc_Referencia = ""
         Mov_contable.Enlace         = ""
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion
         Mov_Contable.DB             = W_Valor NO-ERROR.
  IF W_Valor LT 0 THEN
     ASSIGN Mov_Contable.DB = 0
            Mov_Contable.Cr = W_Valor * -1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Provision_Capital wWin 
PROCEDURE Contab_Provision_Capital :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La Provision de capital
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.16/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.

  FOR EACH Tmp-Provis BREAK BY Tmp-Provis.P_Age BY Tmp-Provis.P_CtaPro:   /*Contab.de la Provision K*/
      IF FIRST-OF(Tmp-Provis.P_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-Provis.P_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-Provis.P_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-Provis.P_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "Provision-Capital"                                                 
                     codComprobante = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-Provis.P_Age AND                                           
                                 Comprobantes.Comprobante EQ codComprobante NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-Provis.P_Age SKIP                                                 
                       "Comprobante: " codComprobante VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN secuenciaContable               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-Provis.P_Valor.

      IF LAST-OF(Tmp-Provis.P_CtaPro) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-Provis.P_Age
                               AND TSdos.Cta     EQ Tmp-Provis.P_CtaPro
                               AND TSdos.Id      EQ "PC"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TCalif GT TSdos.Sdo THEN
               ASSIGN CtaCre    = Tmp-Provis.P_CtaPro        /*Debe aumentar la provision*/
                      SdoCta    = TSdos.Sdo - TCalif   /*Para que sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TCalif LT TSdos.Sdo THEN
                ASSIGN CtaCre    = Tmp-Provis.P_CtaPro   /*Se debe disminuir la provision*/
                       SdoCta    = TSdos.Sdo - TCalif     /*Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN          /*Igual no hay asiento*/
                TSdos.SiC = TRUE.
            ELSE IF NOT AVAIL(TSdos) THEN           /*Se debe crear al Haber */
               ASSIGN CtaCre = Tmp-Provis.P_CtaPro
                      SdoCta = TCalif * - 1.
           
            IF SdoCta NE 0 THEN DO:
               RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
               RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,Tmp-Provis.Cruce,W_Des,SdoCta * -1) NO-ERROR.
            END.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-Provis.P_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Provision, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-Provis.P_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "PC"             AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,TSdos.Cta,"Ajuste Prov.K",TSdos.Sdo) NO-ERROR.
             RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,Tmp-Provis.Cruce,"Ajuste Prov.K",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "PC" AND TSdos.Sdo NE 0:
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final P.K",TSdos.Sdo) NO-ERROR.             
      RUN Contab_PartNuevas (INPUT TSdos.Age,WK_CtaProDeb,"Ajuste Final P.K",TSdos.Sdo * -1) NO-ERROR.        
      ASSIGN TSdos.SiC = TRUE.                                                                            
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Provision_Costas wWin 
PROCEDURE Contab_Provision_Costas :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La Provision de Costas(Otras Ctas x Cobrar del Credito).
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.17/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.

  FOR EACH Tmp-ProvisCos BREAK BY Tmp-ProvisCos.P_Age BY Tmp-ProvisCos.P_CtaPro:  /*Contab.de la Prov.Costas*/
      IF FIRST-OF(Tmp-ProvisCos.P_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-ProvisCos.P_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-ProvisCos.P_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-ProvisCos.P_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "Provision-Costas"                                                 
                     codComprobante = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-ProvisCos.P_Age AND                                           
                                 Comprobantes.Comprobante EQ codComprobante NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-ProvisCos.P_Age SKIP                                                 
                       "Comprobante: " codComprobante VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN secuenciaContable               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-ProvisCos.P_Valor.

      IF LAST-OF(Tmp-ProvisCos.P_CtaPro) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-ProvisCos.P_Age
                               AND TSdos.Cta     EQ Tmp-ProvisCos.P_CtaPro
                               AND TSdos.Id      EQ "PO"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TCalif GT TSdos.Sdo THEN
               ASSIGN CtaCre    = Tmp-ProvisCos.P_CtaPro        /*Debe aumentar la provision*/
                      SdoCta    = TSdos.Sdo - TCalif   /*Para que sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TCalif LT TSdos.Sdo THEN
                ASSIGN CtaCre    = Tmp-ProvisCos.P_CtaPro   /*Se debe disminuir la provision*/
                       SdoCta    = TSdos.Sdo - TCalif     /*Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN           /*Igual no hay asiento*/
                TSdos.SiC = TRUE.
            ELSE IF NOT AVAIL(TSdos) THEN           /*Se debe crear al Haber */
               ASSIGN CtaCre = Tmp-ProvisCos.P_CtaPro
                      SdoCta = TCalif * - 1.
           
            IF SdoCta NE 0 THEN DO:
               RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
               RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,Tmp-ProvisCos.P_CtaGto,W_Des,SdoCta * -1) NO-ERROR.
            END.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-ProvisCos.P_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Provision, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-ProvisCos.P_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "PO"                AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,TSdos.Cta,"Ajuste Prov.Costas",TSdos.Sdo) NO-ERROR.
             RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,Tmp-ProvisCos.P_CtaGto,"Ajuste Prov.Costas",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "PO" AND TSdos.Sdo NE 0:                                
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final P.O.",TSdos.Sdo) NO-ERROR.                  
      RUN Contab_PartNuevas (INPUT TSdos.Age,WK_CtaProCosGto,"Ajuste Final P.O.",TSdos.Sdo * -1) NO-ERROR.
      ASSIGN TSdos.SiC = TRUE.                                                                             
  END.                                                                                                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Provision_Interes wWin 
PROCEDURE Contab_Provision_Interes :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La Provision de Intereses
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.17/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.

  FOR EACH Tmp-ProvisInt BREAK BY Tmp-ProvisInt.P_Age BY Tmp-ProvisInt.P_CtaPro:  /*Contab.de la Prov.Intereses*/
      IF FIRST-OF(Tmp-ProvisInt.P_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-ProvisInt.P_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-ProvisInt.P_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-ProvisInt.P_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "Provision-Interes"                                                 
                     codComprobante = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-ProvisInt.P_Age AND                                           
                                 Comprobantes.Comprobante EQ codComprobante NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-ProvisInt.P_Age SKIP                                                 
                       "Comprobante: " codComprobante VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN secuenciaContable               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-ProvisInt.P_Valor.

      IF LAST-OF(Tmp-ProvisInt.P_CtaPro) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-ProvisInt.P_Age
                               AND TSdos.Cta     EQ Tmp-ProvisInt.P_CtaPro
                               AND TSdos.Id      EQ "PI"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TCalif GT TSdos.Sdo THEN
               ASSIGN CtaCre    = Tmp-ProvisInt.P_CtaPro        /*Debe aumentar la provision*/
                      SdoCta    = TSdos.Sdo - TCalif   /*Para que sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TCalif LT TSdos.Sdo THEN
                ASSIGN CtaCre    = Tmp-ProvisInt.P_CtaPro   /*Se debe disminuir la provision*/
                       SdoCta    = TSdos.Sdo - TCalif     /*Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN            /*Igual no hay asiento*/
                TSdos.SiC = TRUE.
            ELSE IF NOT AVAIL(TSdos) THEN           /*Se debe crear al Haber */
               ASSIGN CtaCre = Tmp-ProvisInt.P_CtaPro
                      SdoCta = TCalif * - 1.
           
            IF SdoCta NE 0 THEN DO:
               RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
               RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,Tmp-ProvisInt.P_CtaGto,W_Des,SdoCta * -1) NO-ERROR.
            END.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-ProvisInt.P_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Provision, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-ProvisInt.P_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "PI"                AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,TSdos.Cta,"Ajuste Prov.I.",TSdos.Sdo) NO-ERROR.
             RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,Tmp-ProvisInt.P_CtaGto,"Ajuste Prov.I.",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "PI" AND TSdos.Sdo NE 0:                                
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final P.I.",TSdos.Sdo) NO-ERROR.                  
      RUN Contab_PartNuevas (INPUT TSdos.Age,Wk_CtaProIntGto,"Ajuste Final P.I.",TSdos.Sdo * -1) NO-ERROR.
      ASSIGN TSdos.SiC = TRUE.                                                                             
  END.                                                                                                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CorteRotativos wWin 
PROCEDURE CorteRotativos :
DEFINE VAR flagAtraso AS LOGICAL.
DEFINE VAR atrasoInteres AS DECIMAL.
DEFINE VAR atrasoInteresDifCobro AS DECIMAL.
DEFINE VAR atrasoMora AS DECIMAL.
DEFINE VAR fechaPago AS DATE.
DEFINE VAR cuotaCapital AS DECIMAL.

IF cfg_tarjetaDB.diaPagoCupoRotativo > cfg_tarjetaDB.diaCorteCupoRotativo THEN
    fechaPago = DATE(MONTH(w_fecha), cfg_tarjetaDB.diaPagoCupoRotativo, YEAR(w_fecha)).
ELSE DO:
    fechaPago = ADD-INTERVAL(w_fecha,1,"months").
    fechaPago = DATE(MONTH(fechaPago), cfg_tarjetaDB.diaPagoCupoRotativo, YEAR(fechaPago)).
END.

/* Calculamos los valores pendientes */
FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                       AND facturacion.num_credito = creditos.num_credito
                       AND facturacion.fec_pago < w_fecha
                       AND facturacion.estado = 1 NO-LOCK:
    atrasoInteres = atrasoInteres + (Facturacion.int_corriente - Facturacion.pago_intCorriente).
    atrasoInteresDifCobro = atrasoInteresDifCobro + (facturacion.INT_difCobro - facturacion.pago_intDifCobro).
    atrasoMora = atrasoMora + (Facturacion.int_mora - Facturacion.pago_mora).
    flagAtraso = TRUE.
END.

/* Creamos la nueva factura */
CREATE facturacion.
ASSIGN facturacion.nit = creditos.nit
       facturacion.num_credito = creditos.num_credito
       facturacion.fec_pago = fechaPago
       facturacion.INT_corriente = creditos.INT_corriente - atrasoInteres
       facturacion.INT_difCobro = creditos.INT_difCobro - atrasoInteresDifCobro
       facturacion.int_mora = creditos.INT_morCobrar + creditos.Int_MoraDifCob - atrasoMora
       facturacion.estado = 1.

IF facturacion.INT_corriente < 0 THEN facturacion.INT_corriente = 0.
IF facturacion.INT_difCobro < 0 THEN facturacion.INT_difCobro = 0.
IF facturacion.INT_mora < 0 THEN facturacion.INT_mora = 0.

/* Recorremos mov_Creditos para identificar si es necesario refinanciar */
FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                        AND mov_creditos.num_credito = creditos.num_credito
                        AND mov_creditos.fecha > ADD-INTERVAL(w_fecha,-1,"months")
                        AND mov_Creditos.cod_operacion <> 020102008 NO-LOCK BY mov_creditos.fecha DESCENDING:
    FIND FIRST operacion WHERE operacion.cod_operacion = mov_creditos.cod_operacion NO-LOCK NO-ERROR.
    IF AVAILABLE operacion THEN DO:
        IF operacion.tipo_operacion = 2 THEN DO:
            facturacion.capital = ROUND(creditos.sdo_capital / 36,0).
            creditos.cuo_pagadas = 0.

            FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.
            IF AVAILABLE cfg_tarjetaDB THEN
                creditos.plazo = cfg_tarjetaDB.plazoCupo.

            LEAVE.
        END.
    END.
END.

IF creditos.sdo_capital > 0 THEN
    facturacion.capital = ROUND(creditos.sdo_capital / (creditos.plazo - creditos.cuo_pagadas),0).

facturacion.cuota = facturacion.capital + facturacion.INT_corriente + facturacion.INT_difCobro + facturacion.INT_mora.

IF facturacion.cuota = 0 THEN DO:
    /*facturacion.INT_corriente = creditos.INT_corriente.
    facturacion.INT_difCobro = creditos.INT_difCobro.
    facturacion.INT_mora = creditos.INT_mora.
    facturacion.cuota = facturacion.capital + facturacion.INT_corriente + facturacion.INT_difCobro + facturacion.INT_mora.*/
    DELETE facturacion.
    LEAVE.
END.

IF facturacion.capital > 0 THEN
    facturacion.cuota = TRUNCATE((facturacion.cuota + 100) / 100,0) * 100.

creditos.cuota = 0.

FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                       AND facturacion.num_credito = creditos.num_credito
                       AND facturacion.estado = 1 NO-LOCK:
    creditos.cuota = creditos.cuota + facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_Mora.
END.

IF flagAtraso = FALSE THEN
    creditos.fec_pago = fechaPago.

CREATE Mov_Creditos.
ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
       Mov_Creditos.Nit = Creditos.Nit
       Mov_Creditos.Num_Credito = Creditos.Num_Credito
       Mov_Creditos.Cod_Operacion = 999999999
       Mov_Creditos.Ofi_Destino = Creditos.Agencia
       Mov_Creditos.Ofi_Fuente = W_Agencia
       Mov_Creditos.Pagare = Creditos.Pagare
       Mov_Creditos.Fecha = W_Fecha
       Mov_Creditos.Hora = vTime
       Mov_Creditos.Usuario = W_Usuario
       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
       Mov_Creditos.Cpte = 20
       Mov_Creditos.Descrip = "Corte/Asignación de Cuota --> " + STRING(creditos.cuota,"$>>>,>>>,>>9.99").

RUN Rp_FacturaCupoRotativo.r (INPUT creditos.nit,
                              INPUT creditos.num_credito,
                              INPUT w_fecha) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ctas_Asociado wWin 
PROCEDURE Ctas_Asociado :
FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_LiqAso
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Débito.Liquidación Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidacion Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_LiqAso
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Crédito.Liquidación Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidación Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_MoraAso
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Débito.Mora Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidacion Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_MoraAso
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Crédito.Mora Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidacion Intereses".    
    RETURN.
  END.

  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaInt_AntAso
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Int.Anticipados Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidacion Intereses".    
    RETURN.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ctas_NoAsociado wWin 
PROCEDURE Ctas_NoAsociado :
FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_Liq
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Débito.Liquidación no Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidación Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_Liq
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Crédito.Liquidación no Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidación Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_Mora
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.      
    MESSAGE "La cuenta que presenta problemas es Cta Débito Mora no Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidación Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_Mora
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Cta.Crédito Mora no Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidación Intereses".    
    RETURN.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaInt_Ant
                 AND Cuentas.Estado EQ 1
                 AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Cuentas) THEN DO:
    W_HayAlgunErr = TRUE.
    MESSAGE "La cuenta que presenta problemas es Int.Anticipados no Asociado. Verifique.."
            VIEW-AS ALERT-BOX TITLE "Verificación Liquidación Intereses".    
    RETURN.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dias_DificilCobro wWin 
PROCEDURE Dias_DificilCobro :
W_DiaDdc = 0.

FOR EACH Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                     AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                     AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                     AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud NO-LOCK:
    IF Garantias.Nit EQ Creditos.Nit AND Garantias.Num_Credito EQ Creditos.Num_Credito AND Garantias.Estado EQ 1 THEN DO:
        ASSIGN W_DiaDdc = Pro_Creditos.Per_GarRea.
        LEAVE.
    END.
END.

IF W_DiaDdc LE 0 THEN
    W_DiaDdc = Pro_Creditos.Per_GarPer.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayTot wWin 
PROCEDURE DisplayTot :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DISPLAY 
          "Total Int. Liquidados" AT 25
          "Int. Corriente"        AT 55
          "Int. Anticipados"      AT 70
          "Int. Dif.Cobro"        AT 88
          "Int. Mora"             AT 109
          "I.Mora-DifC"           AT 123 SKIP
          "Total Agencia: " AT 1
          Tmpi.T_Age        AT 17
          TA_IntCte + TA_IntAnt + TA_DifCob + TA_IntMor 
                            AT 35  FORMAT ">>>>,>>>,>>9"                            
          TA_IntCte         AT 58  FORMAT ">>>,>>>,>>9"          
          TA_IntAnt         AT 75  FORMAT ">>>,>>>,>>9"          
          TA_DifCob         AT 91  FORMAT ">>>,>>>,>>9"          
          TA_IntMor         AT 107 FORMAT ">>>,>>>,>>9"
          TA_MorDif         AT 123 FORMAT ">>>,>>>,>>9"
      WITH DOWN WIDTH 240 FRAME F-TAgencia USE-TEXT STREAM-IO NO-LABELS NO-BOX.
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
  DISPLAY Cmb_Agencias WDia WMes WAno wproc 
      WITH FRAME F_Lq IN WINDOW wWin.
  ENABLE RECT-290 Cmb_Agencias BUTTON-166 Btn_Ejecutar BtnDone 
      WITH FRAME F_Lq IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Lq}
  DISPLAY P_Age P_NroCre P_Nit W_Mensaje 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fin_Liquidacion wWin 
PROCEDURE Fin_Liquidacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 HIDE FRAME F_Progreso.

 ASSIGN P_Age:SCREEN-VALUE IN FRAME F_Progreso    = "0"
        P_NroCre:SCREEN-VALUE IN FRAME F_Progreso = "0"
        P_Nit:SCREEN-VALUE IN FRAME F_Progreso    = ""
        W_Mensaje:SCREEN-VALUE IN FRAME F_Progreso = "".

 /*Grabación del listado de liquidacion*/
  DEFINE VAR Listado AS CHARACTER INITIAL "".

  listado = W_PathSpl + "\000-" + STRING(W_Fecha,"999999") + "CpLCre.Lst".
  {incluido/ImpArch.I "Listado"} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_Informes wWin 
PROCEDURE Graba_Informes :
/*------------------------------------------------------------------------------
  Purpose:  Graba los informes en disco
  Feb.16/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR TotProvision AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
  DEFINE VAR TotProAge    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
  DEFINE VAR ListadoPro AS CHARACTER INITIAL "".
  
  ASSIGN ListadoPro = W_PathSpl + "\000-" + STRING(W_Fecha,"999999") + "Provis.Lst".
         W_Mensaje:SCREEN-VALUE IN FRAME F_Progreso = "InfProv:" + ListadoPro.

OUTPUT TO VALUE(ListadoPro).
  DISPLAY "AGE NIT          CRE NUMCREDIT           SDO.CAPITAL   PROVISION    PRO.INTERES    PRO.COSTAS   CACRE CACLI        APORTESDISTRIB            VALDEFECTO         TOTAL APORTES  REESTRUCT   PAGARE   DIAMORA   GARANTIA"
        WITH FRAME FTI WIDTH 300.
  FOR EACH Pro BREAK BY Pro.Agencia BY Pro.Nit:
      DISPLAY 
         Pro.Agencia Pro.Nit FORMAT "x(11)" Pro.Cod_Credito Pro.Num_Credito Pro.Sdo_Capital Pro.Provision   
         Pro.Provint Pro.ProvCos Pro.Cal_Credito Pro.Cal_Cliente Pro.ApoDistribu Pro.ValDefecto  Pro.TotAportes  
         Pro.Reestru Pro.CuentaCon Pro.diamora Pro.ValGarant  
        WITH FRAME Fp WIDTH 300 NO-LABELS USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE.
     
      ASSIGN TotProAge    = TotProAge + Pro.Provision.
      IF LAST-OF(Pro.Agencia) THEN DO:
         DISPLAY "Total Provision Agencia : " Pro.Agencia " - $ " TotProaGE WITH NO-LABELS.
         ASSIGN TotProvision = TotProvision  + TotProAge
                TotProAge = 0.
      END.
  END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_Saldos wWin 
PROCEDURE Halla_Saldos :
/*------------------------------------------------------------------------------
  Purpose: Desde las configuraciones trae TODOS los saldos por agencia.    
  Notes:   Feb.15/06 GAER.    
------------------------------------------------------------------------------*/
  FOR EACH TSdos: DELETE TSdos. END.

  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK:
      FOR EACH CarteraVencida NO-LOCK:
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoAdDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN
             CREATE TSdos.
          ASSIGN TSdos.Age = Agencias.Agencia
                 Tsdos.Cta = CarteraVencida.Cta_AsoAdDB
                 Tsdos.Id  = "CC".

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoAAdDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN           
             CREATE TSdos.                   
          ASSIGN TSdos.Age = Agencias.Agencia
                 Tsdos.Cta = CarteraVencida.Cta_NoAAdDB
                 Tsdos.Id  = "CC".

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoNaDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoNaDB
                 Tsdos.Id  = "CC".

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoANaDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoANaDB
                 Tsdos.Id  = "CC".

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.CtaCal_Interes 
                             AND Tsdos.Id  EQ "CI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.CtaCal_Interes
                 Tsdos.Id  = "CI".   

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia            /*Reclasif.Int-contingente*/
                             AND TSdos.Cta EQ CarteraVencida.Cta_IntContingDb 
                             AND Tsdos.Id  EQ "IC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_IntContingDb
                 TSdos.Aju = CarteraVencida.Cta_IntContingCr
                 Tsdos.Id  = "IC".   

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.CtaCal_Costas 
                             AND Tsdos.Id  EQ "CO" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.CtaCal_Costas
                 Tsdos.Id  = "CO". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoPrvAdCr    /*Cta_AsoPrvAdCr*/
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoPrvAdCr   
                 Tsdos.Id  = "PC". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoPrvNaCr   /*Cta_AsoPrvNaCr*/
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoPrvNaCr              
                 Tsdos.Id  = "PC". 
            
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaPrvAdCr 
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaPrvAdCr
                 Tsdos.Id  = "PC". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaPrvNaCr 
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaPrvNaCr
                 Tsdos.Id  = "PC".   

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoIntAdCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoIntAdCr
                 Tsdos.Id  = "PI". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoIntNaCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoIntNaCr
                 Tsdos.Id  = "PI". 
            
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaIntAdCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaIntAdCr
                 Tsdos.Id  = "PI". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaIntNaCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaIntNaCr
                 Tsdos.Id  = "PI". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_CostasCR 
                             AND Tsdos.Id  EQ "PO" NO-ERROR.   /*Costas*/
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_CostasCR
                 Tsdos.Id  = "PO". 
      END.
  END.

  FOR EACH TSdos BY TSdos.Age BY TSdos.Cta:
      RUN BuscarSdoCta (INPUT TSdos.Age, INPUT TSdos.Cta, OUTPUT TSdos.Sdo).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DEFINE VAR i AS INTEGER INITIAL 1.
RUN SUPER.

IF W_OfiIni = 0 THEN
    W_Ok = Cmb_Agencias:ADD-LAST("000 - Todas las Agencias") IN FRAME F_Lq.

FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia GE W_OfiIni
                        AND Agencias.Agencia LE W_OfiFin NO-LOCK:
    W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
    cmb_agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
END.

IF W_OfiIni = 0 THEN
    Cmb_Agencias:SCREEN-VALUE = "000 - Todas las Agencias".


ASSIGN WDia:SCREEN-VALUE = STRING(DAY(w_fecha))
       WMes:SCREEN-VALUE = STRING(MONTH(w_fecha))
       WAno:SCREEN-VALUE = STRING(YEAR(w_fecha)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IntCorriente_CupoRotativo wWin 
PROCEDURE IntCorriente_CupoRotativo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR W_SumaCapFac  AS DECIMAL INITIAL 0 NO-UNDO.
   DEF VAR W_VlrCausInt  AS DECIMAL INITIAL 0 NO-UNDO. 
   DEF VAR W_FacInt      AS DECIMAL INITIAL 0 NO-UNDO.
   
/*    MESSAGE "Calculo del interes para el nit = " Creditos.nit SKIP  */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                          */
   FOR EACH Mov_Factura WHERE Mov_Factura.Nit         EQ Creditos.Nit AND 
                              Mov_Factura.Num_credito EQ Creditos.Num_credito 
                      BREAK BY Mov_Factura.Fec_Inicial DESC:
      ASSIGN Mov_Factura.Nuevo_Saldo = Mov_Factura.Cargos - Mov_Factura.Pagos
             W_SumaCapFac = W_SumaCapFac + Mov_Factura.Nuevo_Saldo.
      IF W_SumaCapFac GT Creditos.Sdo_capital THEN
         ASSIGN W_VlrCausInt = Creditos.Sdo_capital - (W_SumaCapFac -  Mov_Factura.Nuevo_Saldo).
      ELSE
         ASSIGN W_VlrCausInt = Mov_Factura.Nuevo_Saldo.

      IF Mov_Factura.Tasa GT tasaUsura THEN DO:
          ASSIGN W_FacInt = ROUND(W_VlrCausInt * (tasaUsura  / 360),0) * dias_aLiquidar
                 W_FacInt = ROUND(W_FacInt / 100,0).
/*           MESSAGE "Se liquida con la tasa usura porque tasa usura = " SKIP                        */
/*                   "Tasa Cupo Rotativo Mes/Año " Mov_Factura.IndMes " Tasa: " Mov_Factura.Tasa SKIP         */
/*                   "Valor del interes: " W_FacInt SKIP                                                      */
/*                   "Aplicar formula intereses = ROUND(W_VlrCausInt * ( / 360),0) * " SKIP */
/*                   "Donde W_VlrCausInt = " W_VlrCausInt SKIP                                                */
/*                   "         = "                                                         */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                           */
      END.
      ELSE DO:                      
          ASSIGN W_FacInt = ROUND(W_VlrCausInt * (Mov_Factura.Tasa / 360),0) * dias_aLiquidar
                 W_FacInt = ROUND(W_FacInt / 100,0).
/*           MESSAGE "Se liquida con la Tasa Cupo Rotativo Mes/Año " Mov_Factura.IndMes " Tasa: " Mov_Factura.Tasa SKIP */
/*                   "Tasa de Usura    = " SKIP                                                                */
/*                   "Valor del interes: " W_FacInt SKIP                                                                */
/*                   "Aplicar formula intereses = ROUND(W_VlrCausInt * (Mov_Factura.Tasa  / 360),0) * " SKIP   */
/*                   "Donde W_VlrCausInt = " W_VlrCausInt SKIP                                                          */
/*                   "         = "                                                                   */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                     */
      END.
      ASSIGN Mov_Factura.Int_Corrientes = Mov_Factura.Int_Corrientes + W_FacInt.
      
      IF W_SumaCapFac GE Creditos.Sdo_capital THEN
         LEAVE.
   END.
   IF W_SumaCapFac LT Creditos.Sdo_capital THEN DO:
       ASSIGN W_FacInt  = ROUND((Creditos.Sdo_capital - W_SumaCapFac) * (tasaLiquidacion  / 360),0) * dias_aLiquidar
              W_FacInt  = ROUND(W_FacInt / 100,0).
              
/*        MESSAGE "Liquidacion de interes Faltante "                                                                                */
/*                "Se liquida con la Tasa Actual Tasa: " SKIP                                                    */
/*                "Tasa de Usura = " SKIP                                                                                  */
/*                "Valor del interes: " W_FacInt SKIP                                                                               */
/*                "Aplicar formula intereses = ROUND((Creditos.Sdo_capital - W_SumaCapFac) * (  / 360),0) * " SKIP */
/*                "Donde W_VlrCausInt = " W_VlrCausInt SKIP                                                                         */
/*                "      (Creditos.Sdo_capital - W_SumaCapFac) = " (Creditos.Sdo_capital - W_SumaCapFac) SKIP                       */
/*                "         = "                                                                                  */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                    */
   END.
/*    MESSAGE "Total Intereses para la cedula " Creditos.nit SKIP */
/*            "Valor total intereses: " SKIP            */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidacion wWin 
PROCEDURE Liquidacion :
DEFINE VAR interesDia AS DECIMAL.
DEFINE VAR basetem AS DECIMAL.
DEFINE VAR flagDebitar AS LOGICAL.
DEFINE VAR sumaDebitar AS DECIMAL.
DEFINE VAR valorCxC AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR vPath AS CHARACTER.

Liquidando:
DO TRANSACTION ON ERROR UNDO Liquidando:
    FIND FIRST cfg_creditos NO-LOCK NO-ERROR.
    FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.

    IF DAY(w_fecha) = cfg_tarjetaDB.diaCorteCupoRotativo THEN
        MESSAGE "Se realizará la generación de facturas de cupos rotativos." SKIP
                "Recomendamos borrar el contenido de la carpeta donde se" SKIP
                "encuentran ubicados los históricos antes de cerrar esta" SKIP
                "ventana..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin NO-LOCK:
        
        /* 1. Hacemos el débito automático para los Créditos atrasados con más de 85 días, normalizando todos los demás créditos */
        FOR EACH creditos WHERE creditos.agencia = agencias.agencia
                            AND creditos.tip_credito <= 4
                            AND creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.int_MoradifCob > 0
                            AND creditos.fec_pago <= w_fecha
                            AND creditos.abogado = NO
                            AND creditos.detalle_Estado <> 2 BREAK BY creditos.nit
                                                                   BY creditos.dias_atraso DESCENDING:
            IF FIRST-OF(creditos.nit) THEN DO:
                /* Lo primero que hacemos es revisar que con los ahorros se alcance a cancelar todo el valor de la deuda.*/
                /* De lo contrario, no se realiza ningún débito */
                FOR EACH ahorros WHERE ahorros.agencia = creditos.agencia
                                   AND ahorros.nit = creditos.nit
                                   AND (ahorros.cod_ahorro = 9 OR
                                        ahorro.cod_ahorro = 4 OR
                                        ahorros.cod_ahorro = 3)
                                   AND ahorros.sdo_disponible > 0 NO-LOCK:
                    sumaDebitar = sumaDebitar + ahorros.sdo_disponible - (ahorros.sdo_disponible * 0.004).
                END.

                IF creditos.dias_atraso >= 85 AND sumaDebitar >= creditos.val_atraso + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar THEN
                    flagDebitar = TRUE.
                ELSE
                    flagDebitar = FALSE.
            END.

            IF flagDebitar = TRUE THEN DO:
                IF creditos.cod_credito = 123 THEN DO:
                    FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                                             AND facturacion.num_credito = creditos.num_credito
                                             AND facturacion.estado = 1 NO-LOCK NO-ERROR.
                    IF AVAILABLE facturacion THEN DO:
                        RUN p-debitoAutomaticoRotativo.r (INPUT creditos.nit,
                                                          INPUT creditos.cod_credito,
                                                          INPUT creditos.num_credito,
                                                          INPUT TRUE /* Debita incluso del Ahorro Permanente */).
                    END.
                END.
                ELSE
                    RUN p-debitoAutomaticoCreditos.r (INPUT creditos.nit,
                                                      INPUT creditos.cod_credito,
                                                      INPUT creditos.num_credito).
            END.
        END.
        /* ---------------------------- */

        /* 2. Hacemos el débito automático para los Créditos Rotativos con mora menor a 85 días */
        FOR EACH creditos WHERE creditos.agencia = agencias.agencia
                            AND creditos.tip_credito <= 4
                            AND creditos.cod_credito = 123
                            AND creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.int_MoradifCob > 0
                            AND creditos.fec_pago < w_fecha
                            AND creditos.dias_atraso < 85
                            AND creditos.abogado = NO
                            AND creditos.detalle_Estado <> 2:
            FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                                     AND facturacion.num_credito = creditos.num_credito
                                     AND facturacion.estado = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE facturacion THEN
                RUN p-debitoAutomaticoRotativo.r (INPUT creditos.nit,
                                                  INPUT creditos.cod_credito,
                                                  INPUT creditos.num_credito,
                                                  INPUT FALSE /* No debita del Ahorro Permanente */).
        END.
        /* ---------------------------- */

        /* 3. Hacemos el DB_Aut para las CxC por auxilios */
        FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                          AND anexos.ano = YEAR(w_fecha)
                          AND anexos.cuenta = "16605004" NO-LOCK:
            valorCxC = anexos.sdo_inicial.

            DO cont = 1 TO MONTH(w_fecha):
                valorCxC = valorCxC + anexos.db[cont] - anexos.cr[cont].
            END.
    
            IF valorCxC > 0 THEN
                RUN p-DebitoAutomaticoCxC.r(INPUT anexos.nit,
                                            INPUT "16605004",
                                            INPUT anexos.cen_costos,
                                            INPUT agencias.agencia,
                                            INPUT valorCxC) NO-ERROR.
        END.

        /* Inicia la liquidación */
        FOR EACH Creditos WHERE Creditos.Agencia = agencias.agencia
                            AND Creditos.Tip_Credito <= 4
                            AND creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.int_MoradifCob > 0
                            AND Creditos.Estado = 2
                            AND creditos.detalle_estado <> 2 BREAK BY Creditos.Cod_Credito
                                                                   BY Creditos.Nit
                                                                   BY creditos.dias_atraso:
            /* Llenamos la tabla con los históricos de saldos para los pagos con fechas anteriores */
            CREATE historico_saldos_cr.
            historico_saldos_cr.fecha = w_fecha.
            historico_saldos_cr.cliente_id = creditos.nit.
            historico_saldos_cr.cod_credito = creditos.cod_credito.
            historico_saldos_cr.num_credito = creditos.num_credito.
            historico_saldos_cr.INT_corriente = creditos.INT_corriente.
            historico_saldos_cr.INT_difCobro = creditos.int_difCobro.
            historico_saldos_cr.int_mora = creditos.INT_morCobrar.
            historico_saldos_cr.int_moraDifCobro = creditos.INT_moraDifCob.
            
            /* Revisamos que exista control_pagos. Si no existe, se crea */
            IF creditos.fec_pago = ? AND creditos.cod_credito <> 123 THEN DO:
                FIND FIRST control_Pagos WHERE control_Pagos.nit = creditos.nit
                                           AND control_Pagos.num_credito = creditos.num_credito
                                           AND control_Pagos.id_pdoMes <> 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE control_Pagos THEN
                    RUN CrearControlPagos.r(INPUT creditos.nit,
                                            INPUT creditos.num_credito).
            END.

            FIND FIRST Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE(clientes) THEN DO:
                IF clientes.fec_fallecido <> ? THEN /* Si es un cliente fallecido no se liquida interés - El crédito se queda parqueado */
                    NEXT.
            END.

            IF FIRST-OF(Creditos.Nit) THEN
                W_Aportes = 0.

            IF FIRST-OF(Creditos.Cod_Credito) THEN DO:
                FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito = Creditos.Tip_Credito
                                          AND Pro_Creditos.Cod_Credito = Creditos.Cod_Credito
                                          AND Pro_Creditos.Estado = 1 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Pro_Creditos THEN DO:
                    MESSAGE "No fue encontrado el producto de créditos para la" SKIP
                            "liquidación del crédito Nro." Creditos.Num_Credito SKIP
                            "Comunique esta inconsistencia al Administrador!"
                        VIEW-AS ALERT-BOX.

                    RETURN ERROR.
                END.
            END.

            interesDia = 0.
            diasPeriodo = 0.

            RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                           INPUT Creditos.Plazo,
                                           OUTPUT diasPeriodo,
                                           OUTPUT P_NMeses,
                                           OUTPUT Periodos,
                                           OUTPUT P_NomPer).

            CREATE Tmpi.
            ASSIGN Tmpi.T_Age = Creditos.Agencia
                   Tmpi.T_Tpd = Creditos.Tip_Credito
                   Tmpi.T_Pdt = Creditos.Cod_Credito
                   Tmpi.T_NCr = Creditos.Num_Credito
                   Tmpi.T_Pagare = Creditos.Pagare
                   Tmpi.T_Nit = Creditos.Nit
                   Tmpi.T_CtaCre = Creditos.Cta_Contable
                   Tmpi.T_SdoCap = Creditos.Sdo_Capital.

            /* Actualizar los días de atraso */
            IF creditos.cod_credito <> 123 THEN
                RUN ActualizarDiasDeAtraso.
            ELSE
                RUN ActualizarDiasDeAtrasoRotativos. /* oakley */

            FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_TasaMax
                                     AND Indicadores.Estado EQ 1
                                     AND Indicadores.FecVcto GE W_Fecha NO-LOCK NO-ERROR.
            IF AVAILABLE Indicadores THEN
                tasaUsura = Indicadores.Tasa.
            ELSE DO:
                FIND LAST Indicadores WHERE Indicadores.Indicador  EQ Entidad.Ind_Usura
                                        AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAILABLE Indicadores THEN
                    tasaUsura = Indicadores.Tasa.
                ELSE
                    tasaUsura = 0.
            END.

            IF tasaUsura GT 0 THEN DO:
                RUN EFNV IN W_ManFin (INPUT tasaUsura / 100,
                                      INPUT periodos,
                                      OUTPUT tasaUsura).

                tasaUsura = tasaUsura * 100 * periodos.
            END.

            ASSIGN P_Age:SCREEN-VALUE IN FRAME F_Progreso = STRING(Creditos.Agencia,"999")
                   P_NroCre:SCREEN-VALUE IN FRAME F_Progreso = STRING(Creditos.Num_Credito,"999999999")
                   P_Nit:SCREEN-VALUE IN FRAME F_Progreso = Creditos.Nit
                   W_Mensaje:SCREEN-VALUE IN FRAME F_Progreso = "Liquidando Creditos"
                   W_IntMora = 0.
            
            FIND FIRST Tmp-Conta WHERE Tmp-Conta.WC_Age EQ Creditos.Agencia
                                   AND Tmp-Conta.WC_CodPdt EQ Creditos.Cod_Credito
                                   AND Tmp-Conta.WC_ForPag EQ Creditos.FOR_Pago
                                   AND Tmp-Conta.WC_Nit = creditos.nit NO-ERROR.
            
            IF NOT AVAILABLE Tmp-Conta THEN DO:
                CREATE Tmp-Conta.
                ASSIGN Tmp-Conta.WC_Age = Creditos.Agencia
                       Tmp-Conta.WC_CodPdt = Creditos.Cod_Credito
                       Tmp-Conta.WC_ForPag = Creditos.FOR_Pago
                       Tmp-Conta.WC_Nit = creditos.nit.
            END.

            IF Creditos.Tasa GT tasaUsura THEN         /*Tasas para liquid.respetando la Proyecc del PlanPagos*/
                ASSIGN tasaLiquidacion = tasaUsura.
            ELSE
                ASSIGN tasaLiquidacion = Creditos.Tasa.
            
            RUN Dias_DificilCobro.

            IF creditos.cod_credito = 108 OR creditos.cod_credito = 113 THEN DO:
                basetem = creditos.sdo_capital + creditos.INT_corriente.

                IF basetem >= creditos.cuota THEN
                    basetem = 0.
            END.
            ELSE
                basetem = creditos.sdo_capital - creditos.val_atraso.

            ASSIGN interesDia = ROUND(basetem * (tasaLiquidacion / 360),0) * dias_aLiquidar
                   interesDia = ROUND(interesDia / 100,0)
                   Tmpi.T_BaseLiq = basetem 
                   Tmpi.T_TasaLiq = (tasaLiquidacion / 360)
                   Tmpi.T_TasaDes = (Creditos.Tasa / 360).

            /* Para todas las periodicidades, excepto la semanal, no se liquida interés el 31 */
            IF creditos.per_pago = 1 AND dias_aLiquidar = 0 THEN
                ASSIGN interesDia = ROUND(basetem * (tasaLiquidacion / 360),0) * 1
                       interesDia = ROUND(interesDia / 100,0)
                       Tmpi.T_BaseLiq = basetem 
                       Tmpi.T_TasaLiq = (tasaLiquidacion  / 360)
                       Tmpi.T_TasaDes = (Creditos.Tasa / 360).
            
            /* Crédito Rotatorios a 01 cuota (1 mes) no tienen interès */
            IF creditos.cod_credito = 123 AND diasPeriodo = 30 AND creditos.plazo = 1 THEN
                interesDia = 0.

            IF interesDia > 0 THEN DO:
                /* Revisamos que el interés no sobrepase el interés ya calculado para los créditos de cuota única (108-113) */
                IF (creditos.cod_credito = 108 OR creditos.cod_credito = 113 OR creditos.plazo = 1) AND creditos.sdo_capital + creditos.INT_corriente + interesDia > creditos.cuota THEN
                    interesDia = creditos.cuota - (creditos.sdo_capital + creditos.INT_corriente).

                IF Creditos.Dias_Atraso > W_Diaddc THEN
                    ASSIGN Creditos.Int_DifCobro = Creditos.Int_DifCobro + interesDia /* Contingente */
                           tmp-conta.WC_DifCob = tmp-conta.WC_DifCob + interesDia
                           Tmpi.T_DifCob = interesDia.
                ELSE
                    ASSIGN tmp-conta.WC_IntCte = tmp-conta.WC_IntCte + interesDia
                           Tmpi.T_IntCte = interesDia
                           Creditos.Int_Corrientes = Creditos.Int_Corrientes + interesDia.

                /* Acumulamos el interés corriente en el respectivo registro del Plan de Pagos */
                FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                           AND CONTROL_pagos.num_credito = creditos.num_credito
                                           AND CONTROL_pagos.fec_Vcto > w_fecha
                                           AND CONTROL_pagos.Id_PdoMes < 2 USE-INDEX ppal4 NO-ERROR.
                IF AVAILABLE CONTROL_pagos THEN
                    CONTROL_pagos.causacion = CONTROL_pagos.causacion + interesDia.
            END.

            IF Creditos.Val_Atraso GT 0 AND Creditos.Dias_Atraso GT 0 THEN DO:
                ASSIGN W_IntMora = ROUND(Creditos.Val_Atraso * (tasaUsura / 360),0) * dias_aLiquidar
                       W_IntMora = ROUND(W_IntMora / 100 ,0)
                       Tmpi.T_BaseMora = Creditos.Val_Atraso
                       Tmpi.T_DiaAtr = W_Fecha - Creditos.Fec_Pago - 1
                       Tmpi.T_DiaAtr = (W_Fecha + 1) - Creditos.Fec_Pago.

                /* A partir de parámetros, se determina si se liquida o no interés de mora */
                IF cfg_creditos.liquidaMora = NO THEN
                    W_IntMora = 0.
                /* ----------------------------------------------------------------------- */

                IF Creditos.Int_Anticipado GT 0 AND W_IntMora GT 0 THEN DO:  /*Anticipados con Int-mora por si se DA*/
                    IF Creditos.Int_Anticipado GE W_IntMora THEN
                        ASSIGN Creditos.Int_Anticipado = Creditos.Int_Anticipado - W_IntMora
                               Tmpi.T_IntAnt = Tmpi.T_IntAnt + W_IntMora
                               Tmpi.T_IntMorAmor = W_IntMora
                               Tmp-Conta.WC_IntAnt = tmp-conta.WC_IntAnt + W_IntMora
                               W_IntMora = 0.
                    ELSE
                        ASSIGN W_IntMora = W_IntMora - Creditos.Int_Anticipado
                               Tmpi.T_IntAnt = Tmpi.T_IntAnt + Creditos.INT_Anticipado
                               Tmpi.T_IntMorAmor = Creditos.INT_Anticipado
                               tmp-conta.WC_IntAnt = tmp-conta.WC_IntAnt + Creditos.INT_Anticipado

                               /* oakley */

                               Creditos.Int_Anticipado = 0.
                END.

                /* Control para los créditos rotativos con dos días de mora: Deben liquidar dos días de interés */
                IF creditos.cod_credito = 123 AND creditos.dias_atraso = 2 THEN
                    w_intMora = w_intMora * 2.
                /* ----------------------- */

                ASSIGN Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + W_IntMora
                       tmp-conta.WC_IntMor = tmp-conta.WC_IntMor + W_IntMora
                       Tmpi.T_IntMor = W_IntMora.

                FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                         AND CONTROL_pagos.num_credito = creditos.num_credito
                                         AND CONTROL_pagos.Id_PdoMes < 2 BY CONTROL_pagos.fec_vcto:
                    CONTROL_pagos.INT_mora = creditos.INT_morCobrar.
                    LEAVE.
                END.
            END.

            ASSIGN Creditos.Fec_UltLiquidacion = W_Fecha
                   Creditos.Fec_ProxLiquidacion = W_Fecha + 1
                   Tmpi.T_Catego = Creditos.Categoria.

            IF creditos.cod_credito = 123 AND creditos.fec_pago = w_fecha THEN
                creditos.dias_atraso = 0.
        END.
        
        /* Corte Rotativos */
        IF DAY(w_fecha) = cfg_tarjetaDB.diaCorteCupoRotativo THEN DO:
            FOR EACH Creditos WHERE Creditos.Agencia = agencias.agencia
                                AND creditos.cod_credito = 123
                                AND creditos.sdo_Capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.Int_MoraDifCob > 0
                                AND Creditos.Estado = 2
                                AND creditos.detalle_estado <> 2:
                FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
                CREATE tt_correos.
                tt_correos.nit = creditos.nit.
                tt_correos.email = clientes.email.

                RUN corteRotativos.
            END.
        END.
        /* --------------- */
    END.

    
    IF DAY(w_fecha) = cfg_tarjetaDB.diaCorteCupoRotativo THEN DO:
        OUTPUT TO Reportes\FacturasCupos\correos.csv.
        EXPORT DELIMITER ";" "Num_Id" "email".
        FOR EACH tt_correos NO-LOCK:
            EXPORT DELIMITER ";" tt_correos.
        END.
        OUTPUT CLOSE.
    END.
    
    IF ERROR-STATUS:ERROR THEN DO:
        /*RETURN ERROR.*/
    END.

    RUN Contabilizar NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error en Contabilizacion"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    W_SiProceso = TRUE.

    FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia GE W_OfiIni
                        AND Agencias.Agencia LE W_OfiFin NO-LOCK:
        FIND FIRST ProcDia WHERE ProcDia.Agencia EQ agencias.agencia
                             AND ProcDia.Cod_Proceso EQ 1
                             AND ProcDia.Fecha_Proc EQ W_Fecha NO-ERROR.
        IF AVAILABLE(ProcDia) AND ProcDia.Estado = 1 THEN
            ProcDia.Estado = 2.
    END.
END.  /*Fin Tx*/

RUN Fin_Liquidacion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Listar_Comprobantes wWin 
PROCEDURE Listar_Comprobantes :
/* {Incluido\VARIABLE.I "SHARED"}
 RUN Rutinas.r PERSISTENT SET W_Manija.*/

 DEFINE INPUT PARAMETER W_Cbt1   LIKE Comprobantes.Comprobante.
 DEFINE INPUT PARAMETER W_Doc1   LIKE Mov_Contable.Num_Documento.
 DEFINE INPUT PARAMETER W_Doc2   LIKE Mov_Contable.Num_Documento.
 DEFINE INPUT PARAMETER W_Ofi1   LIKE Mov_Contable.Agencia.

 DEFINE VAR W_NroDoc           AS   INTEGER INITIAL 0.
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_PrimerCom        AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario2      AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Nomofi           AS  CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS  CHARACTER FORMAT "X(30)".
 DEFINE VAR Movimiento_Debito  LIKE Mov_Contable.DB.
 DEFINE VAR Movimiento_Credito LIKE Mov_Contable.DB.
 DEFINE VAR Total_Debito       LIKE Mov_Contable.DB.
 DEFINE VAR Total_Credito      LIKE Mov_Contable.DB.
 DEFINE VAR W_NomCbt           LIKE Comprobantes.Nombre.
 DEFINE VAR W_NomUsu           LIKE Usuarios.Nombre.
 DEFINE VAR W_Rpta             AS   LOGICAL. 

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".

  W_NroDoc = W_Doc2 - W_Doc1.
  FIND Comprobantes WHERE 
       Comprobantes.Comprobante = W_Cbt1 AND
       Comprobantes.Agencia     = W_Ofi1 NO-LOCK NO-ERROR.
  IF AVAILABLE(Comprobantes) THEN
        W_NomCbt = Comprobantes.Nombre.

  FIND FIRST Mov_Contable WHERE Mov_Contable.Agencia         EQ W_Ofi1
                            AND Mov_Contable.Comprobante     EQ W_Cbt1
                            AND ( Mov_Contable.Num_Documento GE W_Doc1 
                            AND   Mov_Contable.Num_Documento LE W_Doc2 )
                        NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Mov_Contable) THEN DO:
    /*  RUN MostrarMensaje IN W_Manija (INPUT 64, OUTPUT W_Rpta).*/
     RETURN.
  END.
  w_NomCli = "".
  FIND FIRST Clientes WHERE Clientes.nit EQ Mov_contable.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN 
    W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

  FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Contable.Usuario 
                  AND Usuarios.Agencia EQ W_Ofi1
              NO-LOCK NO-ERROR.
  IF AVAILABLE(Usuarios) THEN
    W_NomUsu = Usuarios.Nombre.
  ELSE
    W_NomUsu = "No encontro".
  w_NomOfi = "".
  FIND Agencias WHERE Agencias.Agencia = Mov_Contable.Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE(Agencias) THEN DO:
   W_NomOfi = Agencias.Nombre.
   FIND Entidad WHERE Entidad.Entidad EQ Agencia.Entidad NO-LOCK NO-ERROR.
   IF AVAILABLE(Entidad) THEN 
    ASSIGN W_NomEntidad   = Entidad.Nombre 
           W_NitEnti   = Entidad.Nit
           w_ConcatEnti = TRIM(W_NomEntidad) + " " + "  Nit: " + w_NitEnti.
  END.

  DEFINE FRAME F-Encabezado
    HEADER
        W_ConcatEnti                  AT COL  3 ROW 1
        "Comprobante de       :"      AT COL  3 ROW 2
        W_NomCbt                      AT COL 26 ROW 2
        "Fecha:"                      AT COL 98 ROW 2
        TODAY                         AT COL 104 ROW 2
        "Agencia Nro          :"      AT COL  3 ROW 3
        Mov_contable.Agencia          AT COL 26 ROW 3
        W_NomOfi                      AT COL 31 ROW 3
        "Hora :"                      AT COL 98 ROW 3 STRING(TIME,"HH:MM:SS") SKIP
        "Pagina:"                     AT COL 98 ROW 4 PAGE-NUMBER FORMAT ">9"
        "Documento Nro        :"      AT COL  3 ROW 4
        Mov_Contable.Num_documento    AT COL 26 ROW 4
        "Fecha Contabilizacion:"      AT COL  3 ROW 5
        Mov_Contable.Fec_Contable     AT COL 26 ROW 5
        "Usuario Digitador    :"      AT COL  3 ROW 6
        W_NomUsu                      AT COL 26 ROW 6
        W_NomCli                      AT COL  3 ROW 7 FORMAT "X(100)"
        "Cuenta"                      AT COL  1 ROW 9
        "Nit"                         AT COL 16 ROW 9
        "Nombre"                      AT COL 35 ROW 9
        "Débito"                      AT COL 85 ROW 9
        "Crédito"                     AT COL 102 ROW 9 skip(1)
  WITH DOWN WIDTH 300 FRAME F-Encabezado NO-LABEL NO-BOX NO-UNDERLINE PAGE-TOP 
                              USE-TEXT.

  DEFINE FRAME F_Movimiento
     Mov_Contable.Cuenta      AT 1
     Mov_Contable.Nit         AT 16
     Mov_Contable.Comentario  AT 32  FORMAT "X(30)"
     Movimiento_Debito        AT 75
     Movimiento_Credito       AT 95
  WITH 10 DOWN size 150 by 10 FRAME F_Movimiento USE-TEXT NO-BOX NO-LABEL STREAM-IO.

  DEFINE FRAME F-Totales
    HEADER
    "Sumas Iguales: "                       AT COL 55 ROW 2
    Total_Debito                            AT COL 75 ROW 2
    Total_Credito                           AT COL 95 ROW 2
    "Elaboró :__________________"           AT COL 2  ROW 4
    "Firma: ______________________"         AT COL 42 ROW 4
    "C.C. NIT."                             AT COL 48 ROW 5
  WITH WIDTH 150 FRAME F-Totales PAGE-BOTTOM NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT.

/*  Listado = W_PathSpl + "L_Ingreso.Lst".*/
  listado = W_PathSpl + "\" + STRING(W_Ofi1,"999") + "-" +
            STRING(W_Fecha,"999999") + "CpLCre.Lst".

  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".  
/*0702  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado, INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.*/
  OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 44.

/*  CASE W_Dispositivo:
 *     WHEN "I" THEN
 *       OUTPUT TO PRINTER        NO-ECHO PAGE-SIZE 28.
 *     OTHERWISE 
 *       OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 28.
 *   END CASE.*/

  /* Inicio a armar Comentario */
  W_Comentario = "".
  IF W_NroDoc = 0 THEN DO:
    w_comentario = "Detalle Transacción: ".
    FOR EACH Mov_Contable FIELD (Agencia Comprobante Num_Documento DB CR
                                 Cuenta Cen_Costos Comentario Nit Fec_contable)
                          WHERE Mov_Contable.Agencia         EQ W_Ofi1
                            AND Mov_Contable.Comprobante     EQ W_Cbt1
                            AND ( Mov_Contable.Num_Documento GE W_Doc1
                            AND Mov_Contable.Num_Documento   LE W_Doc2)
                        NO-LOCK BREAK BY Mov_contable.comprobante
                                      BY Mov_contable.Num_documento
                                      BY ROWID(Mov_Contable):
        IF FIRST-OF(Mov_Contable.Num_documento) THEN DO:
          ASSIGN W_Comentario = W_Comentario + TRIM(Mov_Contable.Comentario) + ", "
                 W_PrimerCom  = TRIM(Mov_Contable.Comentario).
        END.
        IF TRIM(Mov_Contable.Comentario) NE W_PrimerCom THEN 
           W_Comentario = W_Comentario + TRIM(Mov_Contable.Comentario) + " ".
    END.
  END.
  /* Fin Armar Comentario */
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Totales.  
  FOR EACH Mov_Contable FIELD (Agencia Comprobante Num_Documento DB CR
                               Cuenta Cen_Costos Comentario Nit Fec_contable)
                        WHERE Mov_Contable.Agencia         EQ W_Ofi1
                          AND Mov_Contable.Comprobante     EQ W_Cbt1
                          AND ( Mov_Contable.Num_Documento GE W_Doc1
                          AND Mov_Contable.Num_Documento   LE W_Doc2)
                      NO-LOCK BREAK BY Mov_contable.comprobante
                                    BY Mov_contable.Num_documento
                                    BY ROWID(Mov_Contable):
      IF FIRST-OF(Mov_Contable.Num_documento) THEN
        ASSIGN w_Comentario2 = ""
               Total_Debito = 0 
               Total_Credito = 0
               W_Comentario2 = "Detalle Transaccion: "
               W_Comentario2 = W_Comentario2 + TRIM(Mov_Contable.Comentario) + ", "
               W_PrimerCom  = TRIM(Mov_Contable.Comentario).
      IF Mov_Contable.DB GT 0 THEN
         ASSIGN Movimiento_Debito = Mov_Contable.DB
                Movimiento_Credito= 0
                Total_Debito  = Total_Debito + Mov_Contable.DB.
      ELSE
         ASSIGN Movimiento_Credito = Mov_Contable.CR
                Movimiento_Debito  = 0
                Total_Credito = Total_Credito + Mov_Contable.CR.

      IF TRIM(Mov_Contable.Comentario) NE W_PrimerCom THEN 
        W_Comentario2 = W_Comentario2 + TRIM(Mov_Contable.Comentario) + " ".
      DISPLAY 
         Mov_Contable.Cuenta
         Mov_contable.Nit
         Mov_Contable.Comentario
         Movimiento_Debito   
         Movimiento_Credito  
      WITH /*10 down*/ FRAME F_Movimiento.
/*      IF LAST-OF(Mov_Contable.Num_documento) THEN DO:
 *        IF W_NroDoc GT 0 THEN 
 *          DISPLAY 
 *            W_Comentario2          AT 3
 *          WITH WIDTH 150 FRAME F-Comentario2 NO-LABEL NO-BOX USE-TEXT.
 *       END.*/
  END.  
  OUTPUT CLOSE.
/*  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT listado).
  IF W_Dispositivo = "I" THEN
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT listado,INPUT 2,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT W_Rpta).
  ELSE
    IF W_Dispositivo <> "A" THEN
       OS-DELETE VALUE(LISTADO).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*  Llamado por el \Incluido\ImpArch.I
 ----------------------------------------------------------------------------------*/                                      
    {INCLUIDO\RepEncabezado.I}    

    DEFINE VAR W_NomCli AS CHARACTER FORMAT "X(30)".
    
    DEFINE VAR TT_IntCte AS DECIMAL FORMAT ">>>,>>>,>>9.99".
    DEFINE VAR TT_IntAnt AS DECIMAL FORMAT ">>>,>>>,>>9.99".
    DEFINE VAR TT_DifCob AS DECIMAL FORMAT ">>>,>>>,>>9.99".
    DEFINE VAR TT_IntMor AS DECIMAL FORMAT ">>>,>>>,>>9.99".
    DEFINE VAR TT_MorDif AS DECIMAL FORMAT ">>>,>>>,>>9.99".
    
    ASSIGN TA_IntCte = 0 TA_IntAnt = 0 TA_DifCob = 0 TA_IntMor = 0 TA_MorDif = 0.
    
    W_Reporte = "REPORTE   : LIQUIDACION DE CREDITOS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
                         /*  1         2         3         4         5         6         7         8         9         0         1         2
                    1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
    /*W_EncColumna = "AGE TP COD CAT NUM.CREDITO Nro.PAGARE  NIT       BASE LIQUIDAC. T-DESEM     TASA INT.CORRIEN INT.ANTICIP. I.DIF-COBR DMORA BASE-CAP.VDO INT.X MORA".*/
    W_EncColumna = "".
     
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  
  FOR EACH Tmpi NO-LOCK BREAK BY Tmpi.T_Age BY Tmpi.T_Tpd BY Tmpi.T_Pdt:
      /*FIND Clientes WHERE Clientes.Nit EQ Tmpi.T_Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN W_NomCli = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
      ELSE W_NomCli = "No Existe".*/
      
      DISPLAY 
        Tmpi.T_Age       FORMAT "999"          LABEL "AG."
        Tmpi.T_Tpd       FORMAT "9"            LABEL "TP"
        Tmpi.T_Pdt       FORMAT "999"          LABEL "PTO"
        Tmpi.T_Catego    FORMAT "X(3)"         LABEL "CAT"
        Tmpi.T_NCr       FORMAT "999999999"    LABEL "N.CREDITO"
        Tmpi.T_Pagare    FORMAT "X(12)"        LABEL "N.PAGARE"
        Tmpi.T_Nit       FORMAT "X(12)"        LABEL "CED/NIT"
        Tmpi.T_BaseLiq   FORMAT "->>>>>>>,>>9" LABEL "BASE LIQUID."
        Tmpi.T_TasaLiq   FORMAT ">9.999999"     LABEL "TASA-LIQ"
        Tmpi.T_TasaDes   FORMAT ">9.999999"     LABEL "T-DESEMB"
        Tmpi.T_IntCte    FORMAT "->>,>>>,>>9"  LABEL "I-CORRIENTE"
        Tmpi.T_IntAnt    FORMAT "->>,>>>,>>9"  LABEL "AMOR.ANTIC."
        Tmpi.T_DifCob    FORMAT "->>,>>>,>>9"  LABEL "I.DIF.COBRO"
        Tmpi.T_DiaAtr    FORMAT "->>>9"        LABEL "DMORA"
        Tmpi.T_BaseMora  FORMAT "->>>>>>,>>9"  LABEL "BASE-CAP.VDO"
        Tmpi.T_IntMor    FORMAT "->>,>>>,>>9"  LABEL "INT.X MORA" 
        Tmpi.T_IntMorAmor     FORMAT "->>,>>>,>>9"  LABEL "IMORA-Amortiz"

        Tmpi.T_IMorDifC  FORMAT "->>,>>>,>>9"  LABEL "IMORA-DIF.C"    SKIP(0)
      WITH DOWN WIDTH 240 FRAME F-informe USE-TEXT NO-LABELS STREAM-IO NO-BOX.
      
      ASSIGN TA_IntCte = TA_IntCte + Tmpi.T_IntCte
             TA_IntAnt = TA_IntAnt + Tmpi.T_IntAnt
             TA_DifCob = TA_DifCob + Tmpi.T_DifCob
             TA_IntMor = TA_IntMor + Tmpi.T_IntMor
             TA_MorDif = TA_MorDif + Tmpi.T_IMorDifC.
             
      ASSIGN TT_IntCte = TT_IntCte + Tmpi.T_IntCte
             TT_IntAnt = TT_IntAnt + Tmpi.T_IntAnt
             TT_DifCob = TT_DifCob + Tmpi.T_DifCob
             TT_IntMor = TT_IntMor + Tmpi.T_IntMor
             TT_MorDif = TT_MorDif + Tmpi.T_IMorDifC.
             
      IF LAST-OF(Tmpi.T_Age) THEN DO:
         RUN DisplayTot.
         
         ASSIGN TA_IntCte = 0 TA_IntAnt = 0 TA_DifCob = 0 TA_IntMor = 0 TA_MorDif = 0.
      END.
  END. 
   
  DISPLAY "Total Entidad: Total Liq." AT 1
           TT_IntCte + TT_IntAnt + TT_DifCob + TT_IntMor 
                            AT 35  FORMAT ">>>>,>>>,>>9"
           TT_IntCte        AT 58  FORMAT ">>>,>>>,>>9"
           TT_IntAnt        AT 75  FORMAT ">>>,>>>,>>9"
           TT_DifCob        AT 91  FORMAT ">>>,>>>,>>9"
           TT_IntMor        AT 107 FORMAT ">>>,>>>,>>9"
           TT_MorDif        AT 123 FORMAT ">>>,>>>,>>9"
  WITH DOWN WIDTH 240 FRAME F-TEntidad USE-TEXT STREAM-IO NO-LABELS NO-BOX.

  ASSIGN TT_IntCte = 0 
         TT_IntAnt = 0 
         TT_DifCob = 0 
         TT_IntMor = 0 
         TT_MorDif = 0.

  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reportarListaSuspendidos wWin 
PROCEDURE reportarListaSuspendidos :
FIND FIRST listaNegra WHERE listaNegra.nit = creditos.nit
                        AND (listaNegra.estado = 1 OR listaNegra.estado = 3) NO-ERROR.
IF NOT AVAILABLE listaNegra THEN DO:
    FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK.

    CREATE listaNegra.
    ASSIGN listaNegra.codigo = 4
           listaNegra.nit = creditos.nit
           listaNegra.nombre = clientes.nombre
           listaNegra.apellido1 = clientes.apellido1
           listaNegra.apellido2 = clientes.apellido2
           listaNegra.usuario = w_usuario.
END.

listaNegra.observacion = STRING(creditos.cod_credito,"zzz") + " - " + STRING(creditos.fec_pago,"99/99/9999").
listaNegra.fec_actualizacion = w_fecha + 1.
listaNegra.fec_exclusion = listaNegra.fec_actualizacion + (creditos.dias_atraso * 3).
listaNegra.estado = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Temporal_MovInst wWin 
PROCEDURE Temporal_MovInst :
DEFI VAR J AS INTEG FORM "999".

VIEW FRAME F_Progreso.

FIND FIRST CopMov_Inst NO-LOCK NO-ERROR.
IF AVAIL(CopMov_Inst) THEN
    RETURN.

W_Mensaje:SCREEN-VALUE IN FRAME F_Progreso = "Generando Temporal Mov-Inst.".

FOR EACH Mov_Instancias:
    CREATE CopMov_Inst.
    BUFFER-COPY Mov_Instancias TO CopMov_Inst.
    ASSIGN CopMov_Inst.W_RowidMI = ROWID(Mov_Instancias).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValCta wWin 
PROCEDURE ValCta :
DEFINE INPUT PARAMETER Cta1 LIKE Cuentas.Cuenta.
DEFINE INPUT PARAMETER Cta2 LIKE Cuentas.Cuenta.
DEFINE INPUT PARAMETER Mesj AS CHARACTER FORMAT "X(30)".

FIND Cuentas WHERE Cuentas.Cuenta EQ Cta1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Cuentas THEN
   MESSAGE Cta1 " No exite en cuentas" SKIP
           Mesj VIEW-AS ALERT-BOX.
FIND Cuentas WHERE Cuentas.Cuenta EQ Cta2 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Cuentas THEN
   MESSAGE Cta2 " No exite en cuentas" SKIP
           Mesj VIEW-AS ALERT-BOX.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Varios wWin 
PROCEDURE Varios :
DEFINE  INPUT PARAMETER P_Agencia LIKE Agencias.Agencia.
  DEFINE  INPUT PARAMETER P_Tipo    LIKE Varios.Tipo.
  DEFINE  INPUT PARAMETER P_Codigo  LIKE Varios.Codigo.
  DEFINE OUTPUT PARAMETER P_Cbte    LIKE Comprobantes.Comprobante.
  DEFINE OUTPUT PARAMETER P_Error   AS   LOGICAL.
  DEFINE VAR W_Rpta                 AS   LOGICAL.

  FIND Varios WHERE Varios.Tipo   EQ P_Tipo 
              AND   Varios.Codigo EQ P_Codigo NO-LOCK NO-ERROR.
  IF AVAILABLE(Varios) THEN DO:
     ASSIGN P_cbte  = Varios.comprobante
            P_Error = FALSE.
     FIND Comprobantes WHERE Comprobantes.Agencia        EQ P_Agencia
                       AND   Comprobantes.Comprobante    EQ P_Cbte
                       AND   Comprobantes.Estado         EQ 1
                       AND   Comprobantes.Fec_Retiro     EQ ?
                       /*AND  (Comprobantes.Id_Consecutivo EQ 1
                       OR    Comprobantes.Id_Consecutivo EQ 2)*/ NO-LOCK NO-ERROR.
     IF NOT AVAILABLE (comprobantes) THEN DO:
        MESSAGE "El proceso comprobante no definido para el proceso " skip
                 "para la Agencia " P_agencia skip
                 "Por lo tanto no se podra correr el proceso " P_codigo
           VIEW-AS ALERT-BOX TITLE "Validación la configuracion de varios".
        P_error = TRUE.
     END.            
  END.
  ELSE DO:
    P_Error = TRUE.
    MESSAGE "El Proceso No Se Puede Ejecutar. No Hay Regidtro" SKIP
            "En Varios Para el Comprobante de la Agencia " P_Agencia
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Validación de Procesos".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_PRC_Dia wWin 
PROCEDURE Verificar_PRC_Dia :
DEFINE INPUT PARAMETER P_OfiIni LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_Mes AS INTEGER FORMAT "99".
DEFINE INPUT PARAMETER P_Ano AS INTEGER FORMAT "99".
DEFINE INPUT PARAMETER P_Tipo LIKE Varios.Tipo.
DEFINE INPUT PARAMETER P_Codigo LIKE Varios.Codigo.
DEFINE INPUT PARAMETER p_Wfecha LIKE Mov_Contable.Fec_Contable.
DEFINE INPUT PARAMETER P_VerifCie AS LOGICAL.
DEFINE OUTPUT PARAMETER P_Cbte LIKE Comprobantes.Comprobante.
DEFINE OUTPUT PARAMETER P_Error AS LOGICAL INITIAL FALSE.

DEFINE VAR W_OfiTbajo LIKE Agencias.Agencia.
DEFINE VAR W_Rpta AS LOGICAL.
DEFINE VAR W_MesA LIKE P_Mes.
DEFINE VAR W_AnoA LIKE P_Ano.

IF P_Mes = 1 THEN
    ASSIGN W_MesA = 12
           W_AnoA = P_Ano - 1.
ELSE
    ASSIGN W_MesA = P_Mes - 1
           W_AnoA = P_Ano.

/* verifica estado de agencia debe estar en 1 normal */
W_OfiTbajo = P_OfiIni.

/*verifica si ya fue ejecutado el cierre diario el dia no debe estar cerrado = 1 */
FIND ProcDia WHERE ProcDia.Agencia EQ W_OfiTBajo
               AND ProcDia.Cod_Proceso EQ 6
               AND ProcDia.Fecha_Proc EQ P_WFecha NO-LOCK NO-ERROR.
IF AVAILABLE (ProcDia) THEN DO:
    IF ProcDia.Estado = 2 THEN DO:
        MESSAGE "El proceso de cierre diario ya fue ejecutado " skip
                "para la Agencia " W_OfiTbajo skip
                "Por lo tanto no se podra correr el proceso " P_codigo
            VIEW-AS ALERT-BOX TITLE "Validación del Proceso Cierre diario".

        P_ERROR = TRUE.
        RETURN ERROR.
    END.
END.

/*verifica si el proceso de liquidacion ya fue ejecutado para el dia*/
FIND ProcDia WHERE ProcDia.Agencia EQ W_OfiTbajo
               AND ProcDia.Cod_Proceso EQ P_Codigo
               AND ProcDia.Fecha_Proc EQ p_WFecha NO-LOCK NO-ERROR.
IF AVAILABLE(ProcDia) THEN DO:
    IF ProcDia.Estado = 2 THEN DO:
        MESSAGE "El Proceso ya fue ejecutado para este día. El Estado en la Tabla" SKIP
                "ProcDia Para La Agencia " W_OfiTbajo " Es de Ejecutado."
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Validación de Procesos".
            
        P_Error = TRUE.
        RETURN ERROR.
    END.
END.
ELSE DO:
    MESSAGE "El proceso no esta configurado para este día, a la Agencia " P_OfiIni SKIP
        VIEW-AS ALERT-BOX TITLE "Liquidacion de Interes de Credito".

    P_Error = TRUE.
    RETURN ERROR.
END.

/* verifica que se haya procesado el dia anterior */
FIND ProcDia WHERE ProcDia.Agencia EQ W_OfiTbajo
               AND ProcDia.Cod_Proceso EQ P_Codigo
               AND ProcDia.Fecha_Proc EQ (p_WFecha - 1) NO-LOCK NO-ERROR.
IF AVAILABLE(ProcDia) THEN DO:
    IF ProcDia.Estado NE 2 THEN DO:
        MESSAGE "El Proceso no se ha ejecutado para el día anterior. " SKIP
                "ProcDia Para La Agencia " W_OfiTbajo
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK TITLE "Validación de Procesos".
            
        P_Error = TRUE.
        RETURN ERROR.
    END.
END.
ELSE DO:
    MESSAGE "El proceso no fue ejecutado el dia anterior a la agencia: " P_OfiIni SKIP
        VIEW-AS ALERT-BOX TITLE "Liquidacion de Interes de Credito".
    
    P_Error = TRUE.
    RETURN ERROR.
END.

/*verifica que el mes actual no este cerrado*/
FIND ProcDia WHERE ProcDia.Agencia EQ W_OfiTBajo
               AND ProcDia.Cod_Proceso EQ 7
               AND MONTH(ProcDia.Fecha_Proc) EQ MONTH(P_WFecha)
               AND YEAR(ProcDia.Fecha_Proc) EQ YEAR(P_WFecha) NO-LOCK NO-ERROR.
IF AVAILABLE(ProcDia) THEN DO:
    IF ProcDia.Estado EQ 2 THEN DO:
        MESSAGE "El mes se encuentra cerrado"
            VIEW-AS ALERT-BOX TITLE "Validación de Procesos".
            
        P_Error = TRUE.
        RETURN ERROR.
    END.
END.

RUN Varios (INPUT W_OfiTbajo,
            INPUT P_Tipo,
            INPUT P_Codigo,
            OUTPUT P_Cbte,
            OUTPUT P_Error).
IF P_Error THEN
    RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verif_Error wWin 
PROCEDURE Verif_Error :
IF W_HayAlgunErr THEN DO:
       MESSAGE "Hay Problemas en la Configuración de Cuentas de Liquidación." SKIP
               "Verifique y corra el proceso nuevamente" SKIP
               "El Producto de trabajo es: " Pro_Creditos.Cod_Credito
       VIEW-AS ALERT-BOX TITLE "Verificación de Liquidación de Intereses".
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

