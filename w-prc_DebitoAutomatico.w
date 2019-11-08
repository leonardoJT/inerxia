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
         TITLE              = "Proceso de Débito Automático"
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
ON END-ERROR OF wWin /* Proceso de Débito Automático */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de Débito Automático */
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

    ASSIGN FRAME F_Lq
        Cmb_Agencias
        WMes
        WAno.

    W_Error = FALSE.

    SESSION:SET-WAIT-STATE("General").

    W_SiProceso = FALSE.

    RUN Liquidacion NO-ERROR.
    SESSION:SET-WAIT-STATE("").
    
    IF W_SiProceso THEN
        MESSAGE "Fin Proceso. Ok" W_siproceso
            VIEW-AS ALERT-BOX.
    ELSE
        MESSAGE "Proceso con Errorres, Debe corregirlo y Reprocesar."
            VIEW-AS ALERT-BOX.

    HIDE W_Mensaje IN FRAME F_Progreso.
    
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

W_ofiIni = 0.
W_ofiFin = 9999.

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
    MESSAGE W_OfiIni W_OfiFin
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin NO-LOCK:
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
    END.
    
    W_SiProceso = TRUE.

END.  /*Fin Tx*/

RUN Fin_Liquidacion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

