&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_LiqIntAhorro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_LiqIntAhorro 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFINE VAR W_TtReTfte AS DECIMAL.
DEFINE VAR W_VrReTfte AS DECIMAL.
DEFINE VAR W_Val AS DECIMAL.
DEFINE VAR W_Cta AS CHARACTER.
DEFINE VAR W_Nat AS CHARACTER.
DEFINE VAR W-DiasLiq AS INTEGER.
DEFINE VAR W-DiasALiq AS INTEGER.
DEFINE VAR FProxLiq AS DATE.
DEFINE VAR W_SiLiq AS LOGICAL.
DEFINE VAR wnitEmp AS CHARACTER.
DEFINE VAR W_TasaRango AS DECIMAL.
DEFINE VAR FecLiqMes AS DATE.
DEFINE VAR FecLiqTri AS DATE.
DEFINE VAR FProxLiqM AS DATE.
DEFINE VAR FProxLiqT AS DATE.

DEFINE TEMP-TABLE TempCtas
    FIELD Agen AS INTEGER
    FIELD Pto AS INTEGER

       /* oakley */

        FIELD CtaAho LIKE Cuentas.Cuenta
        FIELD CtaCau LIKE Cuentas.Cuenta
        FIELD CtaGto LIKE Cuentas.Cuenta
        FIELD CtaLiq LIKE Cuentas.Cuenta
        FIELD CtaRF  LIKE Cuentas.Cuenta
        FIELD Oper   LIKE Liqui_Int.Cod_Operacion
        FIELD CtaSyA LIKE Cuentas.Cuenta.

   DEFI TEMP-TABLE TempLiq 
        FIELD Agen   LIKE Ahorros.Agencia
        FIELD CDocum LIKE Comprobantes.Secuencia
        FIELD NitA   LIKE Clientes.Nit
        FIELD TipA   LIKE Ahorros.Tip_Ahorro
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
        FIELD Cta    LIKE Ahorros.Cue_Ahorro
        FIELD FAper  LIKE Ahorros.Fec_Apertura
        FIELD FVcto  LIKE Ahorros.Fec_Apertura 
        FIELD FPror  LIKE Ahorros.Fec_Apertura
        FIELD FLiq   LIKE Ahorros.Fec_Apertura
        FIELD Base   LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD Tasa   LIKE Ahorros.Tasa             INIT 0
        FIELD Cuota  LIKE Ahorros.Cuota            INIT 0
        FIELD VrLiq  LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD SdoDis LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD SdoCan LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD NvoSdo LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD DiasL  AS INTEG FORM "99999"         INIT 0
        FIELD VrCau  LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD AcCau  LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD AcCauI LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD DifLC  LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD VrRf   LIKE Ahorros.Sdo_Disponible   INIT 0
        FIELD CtaXP  AS INTEG FORM "9" INIT 1
        FIELD AgeD   LIKE Ahorros.Agencia
        FIELD PtoD   LIKE Ahorros.Cod_Ahorro
        FIELD CtaD   LIKE Ahorros.Cue_Ahorro
        FIELD CtaL   LIKE Cuentas.Cuenta
              INDEX Ix_AgPto Agen Pto
              INDEX Ix_IdCxP CtaxP VrLiq
              INDEX Ix_VrAgP AgeD  VrLiq.

DEFINE VAR diasDeInteres AS INTEGER.
DEFINE VAR tasaInteresAhorroAlaVista AS DECIMAL.

DEFINE TEMP-TABLE sinAhorroAlaVista
    FIELD nit AS CHARACTER
    FIELD agencia AS INTEGER
    FIELD cue_ahorros AS CHARACTER
    FIELD FOR_liquidacion AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 W_CmbOfi Btn_Proc MesSaldo ~
MaxFecApertura fechaAplicar Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi MesSaldo tasaLiquidacion ~
MaxFecApertura fechaAplicar W_FecMes W_Cont Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Prc_LiqIntAhorro AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 10 BY 2
     BGCOLOR 8 .

DEFINE BUTTON Btn_Proc 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "&Procesar" 
     SIZE 10 BY 2 TOOLTIP "Procesar los Diferidos".

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 43.14 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fechaAplicar AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de aplicación del interés:" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE MaxFecApertura AS DATE FORMAT "99/99/9999":U 
     LABEL "Asociados hasta el:" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE MesSaldo AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Liquidar con saldos al mes de:" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 62.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tasaLiquidacion AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Tasa de Liquidación (TNA):" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecMes AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81 TOOLTIP "Fecha Corte para el Proceso"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.14 BY 8.35.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     W_CmbOfi AT ROW 1.35 COL 19 COLON-ALIGNED
     Btn_Proc AT ROW 1.62 COL 67
     MesSaldo AT ROW 2.35 COL 59 COLON-ALIGNED WIDGET-ID 2
     tasaLiquidacion AT ROW 3.38 COL 56 COLON-ALIGNED WIDGET-ID 4
     MaxFecApertura AT ROW 4.35 COL 50.29 COLON-ALIGNED WIDGET-ID 6
     fechaAplicar AT ROW 5.35 COL 50.29 COLON-ALIGNED WIDGET-ID 8
     Btn_Done AT ROW 7.08 COL 66.86 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_FecMes AT ROW 7.73 COL 14.29 COLON-ALIGNED
     W_Cont AT ROW 7.81 COL 48.43 COLON-ALIGNED NO-LABEL
     Msaje AT ROW 8.85 COL 2 NO-LABEL
     "Regist.Proceso Perm." VIEW-AS TEXT
          SIZE 20 BY .88 AT ROW 7.73 COL 30
     RECT-314 AT ROW 1.27 COL 65.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.57 BY 8.88
         BGCOLOR 17 FGCOLOR 0 FONT 5.


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
  CREATE WINDOW W-Prc_LiqIntAhorro ASSIGN
         HIDDEN             = YES
         TITLE              = "Liquidación Trimestral de Ahorro Permanente"
         HEIGHT             = 8.88
         WIDTH              = 77.57
         MAX-HEIGHT         = 30.46
         MAX-WIDTH          = 164.57
         VIRTUAL-HEIGHT     = 30.46
         VIRTUAL-WIDTH      = 164.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW W-Prc_LiqIntAhorro
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Proc
   NO-ENABLE ALIGN-L 2                                                  */
/* SETTINGS FOR FILL-IN tasaLiquidacion IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecMes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_LiqIntAhorro)
THEN W-Prc_LiqIntAhorro:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Prc_LiqIntAhorro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_LiqIntAhorro W-Prc_LiqIntAhorro
ON END-ERROR OF W-Prc_LiqIntAhorro /* Liquidación Trimestral de Ahorro Permanente */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_LiqIntAhorro W-Prc_LiqIntAhorro
ON WINDOW-CLOSE OF W-Prc_LiqIntAhorro /* Liquidación Trimestral de Ahorro Permanente */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Prc_LiqIntAhorro
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-Prc_LiqIntAhorro
ON CHOOSE OF Btn_Proc IN FRAME F_Proc /* Procesar */
DO:
    ASSIGN maxFecApertura
           MesSaldo
           fechaAplicar.

    diasDeInteres = w_fecha - fechaAplicar.

    IF mesSaldo < 1 OR mesSaldo > 12 THEN DO:
        MESSAGE "El mes para el saldo a liquidar debe estar entre 1 (enero) y 12 (diciembre)." SKIP
                "Corrija por favor!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.
        
    IF mesSaldo <> 2 AND mesSaldo <> 5 AND mesSaldo <> 8 AND mesSaldo <> 11 THEN DO:
        MESSAGE "El mes debe ser el segundo de trimestre (Febrero, Mayo, Agosto o Noviembre)"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.
        
    EMPTY TEMP-TABLE TempLiq.
    EMPTY TEMP-TABLE TempCtas.

    FIND FIRST entidad NO-LOCK NO-ERROR.
    
    wnitEmp = REPLACE(entidad.nit,"-","").

    FIND FIRST Varios WHERE Varios.Tipo EQ 8
                        AND Varios.Codigo EQ 2 NO-LOCK NO-ERROR.
    IF AVAIL (varios) THEN
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                                  AND Comprobantes.Comprob EQ Varios.Comprobante
                                  AND Comprobantes.Estado EQ 1 NO-LOCK NO-ERROR.
    
    IF NOT AVAIL(Comprobantes) OR NOT AVAIL(varios) THEN DO:
        MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en Tipos y en Comprobantes" SKIP
                "                          No se acepta la Operación."
            VIEW-AS ALERT-BOX.

        RETURN.
    END.

    SESSION:SET-WAIT-STATE("GENERAL").

    RUN Valida NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        SESSION:SET-WAIT-STATE("").

        Msaje:SCREEN-VALUE = "El Proceso Liquidaciòn-Causaciòn NO se efectuò...Revise por favor".

        RETURN.
    END.
    ELSE
        RUN Procesar.

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fechaAplicar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fechaAplicar W-Prc_LiqIntAhorro
ON LEAVE OF fechaAplicar IN FRAME F_Proc /* Fecha de aplicación del interés: */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MaxFecApertura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MaxFecApertura W-Prc_LiqIntAhorro
ON LEAVE OF MaxFecApertura IN FRAME F_Proc /* Asociados hasta el: */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MesSaldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MesSaldo W-Prc_LiqIntAhorro
ON LEAVE OF MesSaldo IN FRAME F_Proc /* Liquidar con saldos al mes de: */
DO:
    ASSIGN mesSaldo.

    IF MONTH(w_fecha) >= mesSaldo THEN
        fechaAplicar:SCREEN-VALUE = STRING(ADD-INTERVAL(DATE(mesSaldo,1,YEAR(w_fecha)),2,"months")).
    ELSE
        fechaAplicar:SCREEN-VALUE = STRING(ADD-INTERVAL(DATE(mesSaldo,1,YEAR(w_fecha) - 1),2,"months")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Prc_LiqIntAhorro
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
    IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                              /*AND Agencias.Estado EQ 2*/ NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            W_OfiIni = Agencias.Agencia.

        FIND LAST Agencias WHERE Agencias.Agencia GT 0
                             /*AND Agencias.Estado EQ 2*/ NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            W_OfiFin = Agencias.Agencia.
    END.
    ELSE
        ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
               W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecMes W-Prc_LiqIntAhorro
ON LEAVE OF W_FecMes IN FRAME F_Proc /* Fecha Proceso */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Prc_LiqIntAhorro 


ASSIGN CURRENT-WINDOW = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
    
ON CLOSE OF THIS-PROCEDURE
    RUN disable_UI.
    
PAUSE 0 BEFORE-HIDE.
    
MAIN-BLOCK:
DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
        
    W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
        
    ASSIGN W_OfiIni = W_Agencia
           W_OfiFin = W_Agencia
           W_FecMes:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Fecha)
           W_FecMes.

    FOR EACH Agencias WHERE /*Agencias.Estado EQ 2
                        AND*/ Agencias.Agencia GT 0 NO-LOCK:
        W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).
    END.

    W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".

    APPLY "VALUE-CHANGED" TO W_CmbOfi.

    FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro = 2
                             AND pro_ahorros.cod_ahorro = 3 NO-LOCK NO-ERROR.
    IF AVAILABLE pro_ahorros THEN DO:
        FIND FIRST indicadores WHERE indicadores.indicador = pro_ahorros.indicador NO-LOCK NO-ERROR.
        IF AVAILABLE indicadores THEN
            ASSIGN tasaLiquidacion:SCREEN-VALUE = STRING(indicadores.tasa,">>9.99").
    END.

    MaxFecApertura:SCREEN-VALUE = STRING(w_fecha).
    fechaAplicar:SCREEN-VALUE = STRING(w_fecha).

    FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro = 1
                             AND pro_ahorros.cod_ahorro = 4 NO-LOCK NO-ERROR.
    IF AVAILABLE pro_ahorros THEN DO:
        FIND FIRST indicadores WHERE indicadores.indicador = pro_ahorros.indicador NO-LOCK NO-ERROR.
        IF AVAILABLE indicadores THEN
            tasaInteresAhorroAlaVista = indicadores.tasa.
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AbonaADestino W-Prc_LiqIntAhorro 
PROCEDURE AbonaADestino :
FIND FIRST Ahorros WHERE Ahorros.Agencia EQ TempLiq.AgeD
                     AND Ahorros.Cod_Ahorro EQ TempLiq.PtoD
                     AND Ahorros.Cue_Ahorro EQ TempLiq.CtaD NO-ERROR.
IF NOT AVAIL(Ahorros) THEN DO:
    FIND FIRST Ahorros WHERE Ahorros.Cod_Ahorro EQ TempLiq.PtoD
                         AND Ahorros.Cue_Ahorro EQ TempLiq.CtaD NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        TempLiq.AgeD = ahorros.agencia.
    END.
END.
    
IF NOT AVAILABLE ahorros THEN DO:
    MESSAGE "La Cuenta Destino : " TempLiq.CtaD SKIP
            "Del Pdcto ahorros : " TempLiq.PtoD SKIP
            "En la Agencia     : " TempLiq.AgeD "      ...De la Ced./Nit : " TempLiq.Nit SKIP
            "               No existe....Revise por favor."
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

IF Ahorros.Estado NE 1 THEN
    Ahorros.Estado = 1.

ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + TempLiq.VrLiq
       Ahorros.Sdo_Minimo = Ahorros.Sdo_Minimo + TempLiq.VrLiq.

RUN MovAhorrro.

Mov_Ahorros.Descrip = "Dest AhPerm Abono Liq.Interés".

FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.AgeD
                      AND TempCtas.Pto EQ TempLiq.PtoD NO-ERROR.

ASSIGN TempLiq.CtaL = TempCtas.CtaAho WHEN AVAIL(TempCtas).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BaseRf W-Prc_LiqIntAhorro 
PROCEDURE BaseRf :
/*-------------------------------------------------------------------------------*/    
    DEFI VAR W_BasRet  LIKE Liqui_Int.Base.
    DEFI VAR W_Porce   LIKE Base_Ret.Porcentaje.
    DEFI VAR W_Dialiq  LIKE TempLiq.DiasL.
     
    ASSIGN W_Porce    = 0
           W_BasRet   = 0
           W_VrReTfte = 0
           W_Dialiq   = TempLiq.DiasL.
    
    FIND Liqui_Int WHERE Liqui_Int.Cod_Producto   EQ Ahorros.Cod_ahorro
                     AND Liqui_Int.Clase_Producto EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Liqui_Int THEN DO:
       ASSIGN W_BasRet  = Liqui_Int.Base. 
              
       FIND Base_Ret WHERE Base_Ret.Cod_base EQ Liqui_Int.Cod_base NO-LOCK NO-ERROR.
    END.
    
    IF AVAILABLE(Base_Ret) THEN
       W_Porce = Base_Ret.Porcentaje. 

    IF TempLiq.TipA EQ 2 THEN DO:  /*Contractuales respetan lo causado, Vble TempLiq.DiasL siempre es 1*/
       IF Ahorros.Per_Liquidac EQ 2 THEN DO:     /*Debe tomar los dias de per_liq.*/
          W_Dialiq = 30.

          IF MONTH(Fec_Apertura) EQ MONTH(W_Fecha) AND YEAR(Fec_Apertura) EQ YEAR(W_Fecha) THEN
             W_Dialiq = 30 - DAY(Fec_Apertura).
          ELSE IF MONTH(Fec_Vencimiento) EQ MONTH(W_Fecha) AND YEAR(Fec_Vencimiento) EQ YEAR(W_Fecha) THEN
             W_Dialiq = DAY(W_Fecha).

          IF W_Dialiq LE 0 THEN
              W_DiaLiq = 1.
       END.
       ELSE IF Ahorros.Per_Liquidac EQ 6 THEN
          W_Dialiq = Ahorros.Plazo.

       ASSIGN TempLiq.DiasL = W_Dialiq.
    END.

    FIND FIRST clientes WHERE clientes.nit EQ ahorros.nit NO-LOCK NO-ERROR.

    IF (TempLiq.VrLiq / W_Dialiq) GE W_BasRet AND W_BasRet GT 0 AND W_Porce GT 0 AND Clientes.Id_Retencion EQ YES THEN 
        ASSIGN W_VrReTfte   = ROUND(TempLiq.VrLiq * W_Porce / 100,0)
               TempLiq.VrRF = ROUND(TempLiq.VrLiq * W_Porce / 100,0)
               W_TtRetFte   = W_TtRetFte + W_VrReTfte.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CausaDia W-Prc_LiqIntAhorro 
PROCEDURE CausaDia :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR BaseLiq  LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR W_TasaD  LIKE Ahorros.Tasa       INIT 0.

  /*Falta base Promedio Pro_Ahorros.Bas_Calculo EQ 2*/

  IF      Pro_Ahorros.Bas_Calculo EQ 1 AND Ahorros.Sdo_Minimo     GE Pro_Ahorros.Mon_MinLiqidacion THEN
     BaseLiq = Ahorros.Sdo_Minimo.
  ELSE IF Pro_Ahorros.Bas_Calculo EQ 2 AND Ahorros.Sdo_Minimo     GE Pro_Ahorros.Mon_MinLiqidacion THEN
     BaseLiq = Ahorros.Sdo_Minimo.
  ELSE IF Pro_Ahorros.Bas_Calculo EQ 3 AND Ahorros.Sdo_Disponible GE Pro_Ahorros.Mon_MinLiqidacion THEN
     BaseLiq = Ahorros.Sdo_Disponible.

  IF Ahorros.Tip_Ahorro EQ 1 AND Pro_Ahorros.Per_liquidacion EQ 3 THEN DO:
     IF (MONTH(W_Fecha) EQ 1 OR MONTH(W_Fecha) EQ 4 OR MONTH(W_Fecha) EQ 7 OR MONTH(W_Fecha) EQ 10) 
     AND DAY(W_Fecha)   LE Pro_Ahorros.Dia_Gracia 
     AND Ahorros.Sdo_Disponible GE Pro_Ahorros.Mon_MinLiqidacion THEN
         BaseLiq = Ahorros.Sdo_Disponible.
  END.

  IF Ahorros.Sdo_Disponible LE 0 THEN  /*Feb.25/05 GAER*/
     BaseLiq = 0.

  IF BaseLiq GT 0 AND Ahorros.Tasa GT 0 THEN DO:
     /*RUN TasaPerNom IN W_ManFin (INPUT 365,Ahorros.Tasa,1,OUTPUT W_TasaD).
      W_TasaD = (W_TasaD * 100).*/
     ASSIGN W_TasaD = Ahorros.Tasa / 360.
     IF Ahorros.Tip_Ahorro EQ 1 AND Pro_Ahorros.Per_liquidacion EQ 1 THEN
        ASSIGN W_TasaD = Ahorros.Tasa / 365.
        
     RUN CrearTempL.

     ASSIGN TempLiq.Base   = BaseLiq 
            TempLiq.Tasa   = W_TasaD
            TempLiq.VrLiq  = 0
            TempLiq.DiasL  = 1
            Ahorros.Fec_UltCausa = W_Fecha
            TempLiq.VrCau  = ROUND((BaseLiq * W_TasaD) / 100,0)   /*Causan todos 1 día*/
            TempLiq.AcCauI = Ahorros.Int_Causado + TempLiq.VrCau.      /*Solo para el informe*/

     /*IF Ahorros.Fec_Prorroga NE ? THEN
        TempLiq.FAper = Ahorros.Fec_Prorroga. Hay otro campo para Fec_prorroga*/
  END.
  ELSE IF Ahorros.Int_Causado GT 0 THEN    /*Gaer Feb.3/05 control para los a la vista*/
          RUN Temp_ReversaCausa.           /*Los debe reversar, ya no hay causación*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contable W-Prc_LiqIntAhorro 
PROCEDURE Contable :
CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = TempLiq.Agen
       Mov_Contable.Cuenta = W_Cta
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = "Liq.Int.Ahorros"
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Cen_Costos = W_Cencosgral
       Mov_Contable.Destino = TempLiq.AgeD
       Mov_Contable.Comprobante = Varios.Comprobante
       Mov_Contable.Num_Documento = TempLiq.CDocum
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion.

IF W_Nat EQ "DB" THEN
    Mov_Contable.Db = W_Val.
ELSE
    Mov_Contable.Cr = W_Val.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContabRFte W-Prc_LiqIntAhorro 
PROCEDURE ContabRFte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.Agen                                            
                         AND TempCtas.Pto EQ TempLiq.Pto NO-ERROR. 

   ASSIGN W_Cta = TempCtas.CtaRF                                                                 
          W_Val = TempLiq.VrRF                                                                           
          W_Nat = "CR".                                                                           
                                                                                                        
   RUN Contable.                                                                                  
   ASSIGN Mov_Contable.Nit    = TempLiq.NitA
          Mov_Contable.Coment = "RetFuente x Liq.Interés"
          W_Cta               = TempCtas.CtaGto /*Es gasto,Porque lo abonado y Cont.en Gto, ya tiene restada la RetFuente*/
          W_Nat               = "DB". 

          /*W_Cta               = TempCtas.CtaAho   Estaba hasta Feb.8/05 Gaer*/

   /*IF TempLiq.CtaxP EQ 3 AND TempLiq.CtaL GT "0" THEN      /*Es la CxP pero de la Cta-Destino*/
      ASSIGN W_Cta = TempLiq.CtaL.                                                             
   ELSE IF TempLiq.CtaxP EQ 2 THEN                         /*Es la de CxP x Liquidac.*/        
      ASSIGN W_Cta = TempCtas.CtaLiq.                                                      
   Estaba hasta Feb.8/05 Gaer*/

   RUN Contable. 
   ASSIGN Mov_Contable.Nit    = TempLiq.NitA
          Mov_Contable.Coment = "RetFuente x Liq.Interés".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CrearTempL W-Prc_LiqIntAhorro 
PROCEDURE CrearTempL :
/*------------------------------------------------------------------------------
  Gaer Feb.3/05.
------------------------------------------------------------------------------*/
  CREATE TempLiq.
  ASSIGN TempLiq.Agen   = Ahorros.Agencia
         TempLiq.CDocum = Comprobantes.Secuencia
         TempLiq.NitA   = Ahorros.Nit                                        
         TempLiq.Pto    = Ahorros.Cod_Ahorro  
         TempLiq.TipA   = Ahorros.Tip_Ahorro
         TempLiq.Cta    = Ahorros.Cue_Ahorros                                
         TempLiq.FAper  = Ahorros.Fec_Apertura                               
         TempLiq.FVcto  = Ahorros.Fec_Vencimiento  
         TempLiq.FLiq   = Ahorros.Fec_ProLiquidacion
         TempLiq.Cuota  = Ahorros.Cuota                                      
         TempLiq.SdoDis = Ahorros.Sdo_Disponible                             
         TempLiq.SdoCan = Ahorros.Sdo_Canje                                  
         TempLiq.NvoSdo = Ahorros.Sdo_Disponible                             
         TempLiq.CtaXP  = 1
         TempLiq.Base   = 0                                            
         TempLiq.Tasa   = 0                                            
         TempLiq.VrLiq  = 0    
         TempLiq.DiasL  = W-DiasALiq                                         
         TempLiq.VrCau  = 0    
         TempLiq.AcCauI = Ahorros.INT_Causado
         W_Cont         = W_Cont + 1                                         
         W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont). 

  IF Ahorros.Tip_Ahorro LE 2 THEN DO:
     ASSIGN TempLiq.FPror = ?.

     IF Ahorros.Tip_Ahorro EQ 1 THEN
        ASSIGN TempLiq.FVcto = ?.
  END.
  ELSE 
     ASSIGN TempLiq.FPror = Ahorros.Fec_Prorroga.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Prc_LiqIntAhorro  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_LiqIntAhorro)
  THEN DELETE WIDGET W-Prc_LiqIntAhorro.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Prc_LiqIntAhorro  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi MesSaldo tasaLiquidacion MaxFecApertura fechaAplicar W_FecMes 
          W_Cont Msaje 
      WITH FRAME F_Proc IN WINDOW W-Prc_LiqIntAhorro.
  ENABLE RECT-314 W_CmbOfi Btn_Proc MesSaldo MaxFecApertura fechaAplicar 
         Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Prc_LiqIntAhorro.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Prc_LiqIntAhorro.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaFecProxLiq W-Prc_LiqIntAhorro 
PROCEDURE HallaFecProxLiq :
DEFI VAR NMes AS INTEG FORMAT "99".
DEFI VAR WDia AS INTEG FORMAT "99".

ASSIGN FProxLiq = W_Fecha
       WDia = DAY(Ahorros.Fec_Apertura)
       NMes = MONTH(W_Fecha) + 1.        /*Inicia en mensual*/

IF WDia GT 30 THEN
    WDia = 30.

IF Ahorros.Per_liquidacion EQ 3 THEN          /*Liq.Trimestral*/
    NMes = MONTH(W_Fecha) + 3.
ELSE
    IF Ahorros.Per_liquidacion EQ 4 THEN     /*Liq.Semestral*/
        NMes = MONTH(W_Fecha) + 6.
    ELSE
        IF Ahorros.Per_liquidacion EQ 5 THEN       /*Liq.Anual*/
            NMes = MONTH(W_Fecha) + 12.

IF NMes GE 13 THEN DO:
    NMes = NMes - 12.

    IF NMes EQ 2 AND WDia GT 28 THEN
        WDia = 28.

    FProxLiq = DATE(NMes,WDia,YEAR(FProxLiq) + 1).
END.
ELSE DO:
    IF NMes EQ 2 AND WDia GT 28 THEN
        WDia = 28.

    FProxLiq = DATE(NMes,WDia,YEAR(FProxLiq)).
END.

/*Agosto 2/05 Gaer, Control para el dia de apertura 1o.de cada mes, FProxLiq que al regresar le resta 1 dia*/
IF WDia EQ 1 THEN DO:
    NMes = NMes + 1.

    IF NMes GE 13 THEN
        ASSIGN NMes = NMes - 12
               FProxLiq = DATE(NMes,WDia,YEAR(FProxLiq) + 1).
    ELSE
        FProxLiq = DATE(NMes,WDia,YEAR(FProxLiq)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaFVcto W-Prc_LiqIntAhorro 
PROCEDURE HallaFVcto :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR NMes      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR FProxVcto AS DATE.
  DEFI VAR NroM      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR WDia      AS INTEG FORMAT "99".
 
  ASSIGN FProxVcto = W_Fecha - 1 + Ahorros.Plazo 
         NMes      = MONTH(W_Fecha)
         NroM      = ROUND(Ahorros.Plazo / 30,0)
         WDia      = DAY(Ahorros.Fec_Apertura).

  IF WDia GT 30 THEN
     WDia = 30.

  IF Ahorros.Plazo GT 31 THEN DO:        
     ASSIGN NMes = Nmes + NroM.

     IF NMes GE 25 THEN DO:                                                            
        ASSIGN NMes = NMes - 24.
        IF NMes EQ 2 AND WDia GT 28 THEN
           WDia = 28.
        FProxVcto = DATE(NMes,WDia,YEAR(W_Fecha) + 2).            
     END.
     ELSE IF NMes GE 13 THEN DO:                                                            
        ASSIGN NMes = NMes - 12.
        IF NMes EQ 2 AND WDia GT 28 THEN
           WDia = 28.
        FProxVcto = DATE(NMes,WDia,YEAR(W_Fecha) + 1).  
     END.
     ELSE DO:                                                                          
        IF NMes EQ 2 AND WDia GT 28 THEN
           WDia = 28.
        FProxVcto = DATE(NMes,WDia,YEAR(W_Fecha)). 
     END.

     ASSIGN Ahorros.Fec_Vencimiento = FProxVcto.     
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaTasa_VistaYContrac W-Prc_LiqIntAhorro 
PROCEDURE HallaTasa_VistaYContrac :
DEFI VAR W_PlazoVC LIKE Ahorros.Plazo.

FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador
                         AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
IF AVAIL(Indicadores) THEN DO:
    ASSIGN W_TasaRango = Indicadores.Tasa
           W_PlazoVC = Ahorros.Plazo.

    IF Pro_Ahorros.Tip_Ahorro EQ 1 THEN
        W_PlazoVC = 999.

    IF Indicadores.Rango THEN DO:
        FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador EQ Indicadores.Indicador
                                   AND Ahorros.Sdo_Disponible GE Ran_Intereses.Val_Inicial
                                   AND Ahorros.Sdo_Disponible LE Ran_Intereses.Val_Final
                                   AND W_PlazoVC GE Ran_Intereses.Pla_Inicial
                                   AND W_PlazoVC LE Ran_Intereses.Pla_Final
                                   AND Ran_Interes.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Ran_Intereses) THEN
            W_TasaRango = W_TasaRango + Ran_Intereses.Puntos_Asoc.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-Prc_LiqIntAhorro 
PROCEDURE ImpCpte :
/*------------------------------------------------------------------------------
      Invocado desde ProcesoImprimir.
  Purpose:     Imprime el resumen contable de la liquidaciòn.
  ------------------------------------------------------------------------------*/
  DEFI VAR TotD   AS DECIMAL INIT 0.
  DEFI VAR TotC   AS DECIMAL INIT 0.
  DEFI VAR TTotD  AS DECIMAL INIT 0.
  DEFI VAR TTotC  AS DECIMAL INIT 0.

  RETURN.  /*Abril 6/05 GAER No necesario, lo imprimen por aparte*/

  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte = "Cpte Resumen : Liquidación-Causación Intereses de Ahorros     Fecha :" +
                      STRING(W_FecMes,"99/99/9999") + "      Hora :" + STRING(TIME,"HH:MM:SS").

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  DISPLAY STRING(Comprobantes.Comprobante,"99") + "-" + 
          STRING(Comprobantes.Secuencia,"99999999") FORMAT "X(120)"  
      WITH DOWN WIDTH 140 FRAME FCpte USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
/*  
  DISPLAY SKIP(2)
            "  "
            STRING(W_Nom_Entidad,"X(45)") +
            "   "  +
            TRIM(W_Nom_Agencia,"X(25)")          FORMAT "X(120)" SKIP(1)
            "             Comprobante Resumen Liquidación-Causación Int-Ahorros     Fecha del Cpte: " +
            STRING(W_FecMes,"99/99/9999")        FORMAT "X(120)"  SKIP
            "                   " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                                   STRING(Comprobantes.Secuencia,"99999999") FORMAT "X(120)"  
            SKIP(1)
            "--------------------------------------------------------------------------------------------------------"
       WITH DOWN WIDTH 140 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
*/

  FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                          AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                          AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta:
      ASSIGN TotD  = TotD  + Mov_Contable.Db
             TTotD = TTotD + Mov_Contable.Db
             TotC  = TotC  + Mov_Contable.Cr
             TTotC = TTotC + Mov_Contable.Cr.

      IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                              /*AND Cuentas.Estado EQ 1*/ NO-LOCK NO-ERROR.
         DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                 Mov_Contable.Cuenta    LABEL "Cta-Contable"
                 Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                 TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                 TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN TotD  = 0
                TotC  = 0.
      END.

  END.

  DISPLAY SKIP(1)
          "                     TOTAL FINAL------------>               ------------------ ------------------"
          SKIP
          "                                                           "
          TTotD      FORM "->>>>>>,>>>,>>9.99"
          TTotC      FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquida W-Prc_LiqIntAhorro 
PROCEDURE Liquida :
DEFI VAR BaseLiq LIKE Ahorros.Sdo_Dispon INIT 0.
DEFI VAR W_TasaD LIKE Ahorros.Tasa INIT 0.
DEFI VAR NroA AS INTEG FORM "999" INIT 0.
DEFI VAR W_PdoAno AS INTEG FORM "9" INIT 0.

W_VrReTfte = 0.

FIND FIRST rep_ahorros WHERE rep_ahorros.fecCorte = DATE(mesSaldo + 1,1,YEAR(w_fecha)) - 1
                         AND rep_ahorros.nit = ahorros.nit
                         AND rep_ahorros.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
IF AVAILABLE rep_ahorros THEN
    /*BaseLiq = ahorros.sdo_anuales[mesSaldo].*/
    baseLiq = rep_ahorros.sdo_disponible.

IF BaseLiq GT 0 AND Ahorros.Tasa GT 0 THEN DO:
    ASSIGN W_TasaD = Ahorros.Tasa / 360
           W_SiLiq = TRUE.

    RUN CrearTempL.

    ASSIGN TempLiq.Base = BaseLiq
           TempLiq.Tasa = W_TasaD
           TempLiq.VrLiq = ROUND((BaseLiq * W_TasaD * W-DiasALiq) / 100,0)
           Ahorros.Sdo_UltLiquidacion = BaseLiq.

    /* Se calcula el interés para este valor a partir de la fecha en que se debió asentar, hasta la fecha de aplicación (hoy) */
    IF diasDeInteres > 0 THEN
        TempLiq.VrLiq = TempLiq.VrLiq + ROUND((((tasaInteresAhorroAlaVista / 100) / 360) * TempLiq.VrLiq * diasDeInteres),0).

    ASSIGN TempLiq.DifLC = TempLiq.VrLiq
           TempLiq.VrLiq = TempLiq.VrLiq + Ahorros.Int_Causado  /*Porque adiciona solo el ultimo día*/
           TempLiq.AcCau = Ahorros.Int_Causado
           TempLiq.VrCau = 0.

    RUN MovAhorrro.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE llenaDestinoIntereses W-Prc_LiqIntAhorro 
PROCEDURE llenaDestinoIntereses :
DEFINE BUFFER bfrAhorros FOR ahorros.
DEFINE VAR ageDestino AS INTEGER.
DEFINE VAR cueDestino AS CHARACTER.
DEFINE VAR vCuenta AS CHARACTER.

EMPTY TEMP-TABLE sinAhorroAlaVista.

FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                   AND ahorros.cod_ahorro = 3
                   AND ahorros.estado = 1:
    FIND FIRST bfrAhorros WHERE bfrAhorros.agencia = ahorros.agencia
                            AND bfrAhorros.nit = ahorros.nit
                            AND bfrAhorros.cod_ahorro = 4
                            AND bfrAhorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfrAhorros THEN DO:
        FIND FIRST bfrAhorros WHERE bfrAhorros.agencia = ahorros.agencia
                                AND bfrAhorros.nit = ahorros.nit
                                AND bfrAhorros.cod_ahorro = 4 NO-ERROR.
        IF AVAILABLE bfrAhorros THEN DO:
            bfrAhorros.estado = 1.
            bfrAhorros.fec_cancelacion = ?.

            CREATE hoja_vida.
            hoja_vida.Asunto_Cumplido = TRUE.
            hoja_vida.Codigo = 8.
            hoja_vida.DoctoRefer = INTEGER(bfrAhorros.cue_ahorros).
            hoja_vida.Fec_Grabacion = TODAY.
            hoja_vida.Hora_Grabacion = TIME.
            hoja_vida.Nit = bfrAhorros.nit.
            hoja_vida.Observacion = "Reactivación de Ahorro a la Vista (Proceso de liquidación Trimestral de Ahorro Permanente".
            hoja_vida.Tipo = 18.
            hoja_vida.Usuario = w_usuario.
        END.
    END.
    
    IF AVAILABLE bfrAhorros THEN DO:
        Ahorros.Agencia_Destino = ahorros.agencia.
        Ahorros.Pro_Destino = 4.
        Ahorros.Cue_Destino = bfrAhorros.cue_ahorros.
        ahorros.des_interes = 1.
    END.
    IF NOT AVAILABLE bfrAhorros THEN DO:
        FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro EQ 1
                                     AND pro_ahorros.cod_ahorro EQ 4 NO-ERROR.
        IF AVAILABLE pro_ahorros THEN DO:
            IF Pro_Ahorros.Id_Consecutivo THEN DO:
                pro_ahorros.num_consecutivo = pro_ahorros.num_consecutivo + 1.
                vCuenta = STRING(pro_ahorros.num_consecutivo).
            END.
            ELSE
                vCuenta = ahorros.nit.

            CREATE sinAhorroAlaVista.
            ASSIGN sinAhorroAlaVista.nit = ahorros.nit
                   sinAhorroAlaVista.agencia = ahorros.agencia
                   sinAhorroAlaVista.cue_ahorros = vCuenta
                   sinAhorroAlaVista.FOR_liquidacion = pro_ahorros.FOR_liquidacion.

            Ahorros.Agencia_Destino = ahorros.agencia.
            Ahorros.Pro_Destino = 4.
            Ahorros.Cue_Destino = vCuenta.
            ahorros.des_interes = 1.
        END.
        ELSE DO:
            ahorros.cue_destino = "".
            ahorros.agencia_destino = ?.
            ahorros.pro_destino = ?.
            ahorros.Des_Intereses = 3.
        END.
    END.
END.

/* Creamos los ahorro a la vista que faltan */
FIND FIRST indicadores WHERE indicadores.indicador EQ pro_ahorros.indicador NO-LOCK NO-ERROR.

FOR EACH sinAhorroAlaVista NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = sinAhorroAlaVista.nit NO-LOCK NO-ERROR.

    CREATE ahorros.
    Ahorros.Cue_Ahorros = sinAhorroAlaVista.cue_ahorros.
    Ahorros.Tip_Ahorro = 1.
    Ahorros.Cod_ahorro = 4.
    Ahorros.Nit = sinAhorroAlaVista.nit.
    Ahorros.Detalle_Estado = 1.
    Ahorros.Agencia = sinAhorroAlaVista.agencia.
    Ahorros.FOR_Pago = 1.
    Ahorros.Monto_Apertura = 0.
    Ahorros.For_Liquidacion = sinAhorroAlaVista.FOR_liquidacion.
    Ahorros.Estado = 1.
    Ahorros.Tasa = Indicadores.Tasa.
    Ahorros.Usu_Creacion = W_Usuario.
    Ahorros.IdNombre = Clientes.Nombre.
    Ahorros.IdApellido1 = Clientes.Apellido1.
    Ahorros.Cuota = 0.
    Ahorros.Plazo = 9999.
    Ahorros.Per_Deduccion = 1.
    Ahorros.per_Liquidacion = 6.
    Ahorros.Fec_Apertura = TODAY.
    ahorros.fec_cancelacion = ?.

    CREATE hoja_vida.
    hoja_vida.Asunto_Cumplido = TRUE.
    hoja_vida.Codigo = 8.
    hoja_vida.DoctoRefer = INTEGER(sinAhorroAlaVista.cue_ahorros).
    hoja_vida.Fec_Grabacion = TODAY.
    hoja_vida.Hora_Grabacion = TIME.
    hoja_vida.Nit = sinAhorroAlaVista.nit.
    hoja_vida.Observacion = "Apertura de Ahorro a la Vista (Proceso de liquidación Trimestral de Ahorro Permanente".
    hoja_vida.Tipo = 18.
    hoja_vida.Usuario = w_usuario.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovAhorrro W-Prc_LiqIntAhorro 
PROCEDURE MovAhorrro :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  FIND FIRST TempCtas WHERE TempCtas.Age EQ Ahorros.Agencia
                        AND TempCtas.Pto EQ Ahorros.Cod_Ahorro NO-ERROR.
  
  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Agencia        = Ahorros.Agencia
         Mov_Ahorros.Age_Destino    = Ahorros.Agencia                   
         Mov_Ahorros.Age_Fuente     = TempLiq.Agen                          
         Mov_Ahorros.Cod_Ahorro     = Ahorros.Cod_Ahorro                        
         Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorro                 
         Mov_Ahorros.Fecha          = W_Fecha                            
         Mov_Ahorros.Hora           = TIME                               
         Mov_Ahorros.Nit            = Ahorros.Nit                        
         Mov_Ahorros.Num_Documento  = STRING(TempLiq.CDocum)     
         Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
         Mov_Ahorros.Usuario        = W_Usuario                          
         Mov_Ahorros.Val_Efectivo   = TempLiq.VrLiq
         Mov_Ahorros.Cod_Operacion  = TempCtas.Oper
         Mov_Ahorros.Descrip        = "Abono Liq.Interés"
         Mov_Ahorros.Cpte           = Varios.Comprobante.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesar W-Prc_LiqIntAhorro 
PROCEDURE Procesar :
DEFI VAR Listado AS CHAR FORM "X(40)".
DEFI VAR W_Rpta AS LOGICAL.
DEFI VAR W_ArchTR AS CHAR FORM "X(40)".

W_VrReTfte = 0.

Liquidacion:
DO TRANSACTION ON ERROR UNDO Liquidacion:
    FIND FIRST Varios WHERE Varios.Tipo EQ 8
                        AND Varios.Codigo EQ 2 NO-LOCK NO-ERROR.

    ASSIGN listado = W_PathSpl + "\" + SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME F_Proc,1,3) + "-" + STRING(W_Fecha,"999999") + "CpLAhoContTrim.Lst"
           Msaje:SCREEN-VALUE IN FRAME F_Proc = "Espere un momento por favor, Procesando Liquidaciòn-Causaciòn...".

    W_ArchTR = W_PathSpl + "\" + "TasasRenovContTrim" + STRING(W_Fecha,"999999") + ".Txt".

    OUTPUT TO VALUE(W_ArchTR).
        DISPLAY "Ag. Ced./Nit     No.Titulo         Tasa-Ant    Tasa-Act Fec-Apert. Fec-Vcto.  Fec-PxLiq PdoL Plazo" SKIP
                "--- ------------ -------------- ----------- ----------- ---------- ---------- --------- ---- -----"
            WITH DOWN WIDTH 120 FRAME FTitnot USE-TEXT STREAM-IO NO-LABELS NO-BOX.

        FOR EACH Agencias WHERE /*Agencias.Estado EQ 2
                            AND*/ Agencias.Agencia GE W_OfiIni
                            AND Agencias.Agencia LE W_OfiFin NO-LOCK:
            FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ Agencias.Agencia
                                      AND Comprobantes.Comprob EQ Varios.Comprobante
                                      AND Comprobantes.Estado EQ 1 NO-ERROR.
            IF NOT AVAIL(Comprobantes) THEN DO:
                MESSAGE "Falta El Cpte : " Varios.Comprobante SKIP
                        "Para la agencia : " Agencias.Agencia SKIP
                        "                   Proceso cancelado"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.

            ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                   Msaje:SCREEN-VALUE IN FRAME F_Proc = "Espere por favor, Proceso Liquidac.-Causaciòn...Agenc." + STRING(Agencias.Agencia,"999").

            FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

            RUN Proceso1 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                Msaje:SCREEN-VALUE = "El Proceso Liquidaciòn-Causaciòn NO se efectuò...Revise por favor".

                RETURN ERROR.
            END.
        END.
    OUTPUT CLOSE.

    IF NOT ERROR-STATUS:ERROR THEN DO:
        RUN Proceso2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            Msaje:SCREEN-VALUE = "El Proceso Liquidaciòn-Causaciòn NO se efectuò...Revise por favor".

            RETURN ERROR.
        END.

        {Incluido\ImpArch.I "listado"}

        ASSIGN Msaje:SCREEN-VALUE = "Proceso Liq-Causaciòn Ahorros Contractuales Trimestrales Terminó Exitosamente...".

        MESSAGE "Proceso terminado con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso1 W-Prc_LiqIntAhorro 
PROCEDURE Proceso1 :
DEFI VAR P_ImpAplic LIKE Ahorros.Sdo_Dispon.
DEFI VAR TotLiq LIKE TempLiq.VrLiq.
DEFI VAR FContVcto AS DATE.
DEFI VAR W_RowidInd AS ROWID.
DEFINE VARIABLE vlCambiaFecha AS LOGICAL NO-UNDO. /*Gcamacho - Verifica si se modifica fecha de vencimiento para producto 221 */
DEFINE VAR fechaRetiro AS DATE.
DEFINE BUFFER bfrAhorros FOR ahorros.

RUN llenaDestinoIntereses. /* Se actualiza el destino de los intereses de todos los ahorros permanentes, para que lo lleven al Ahorro a la Vista */

FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = 2
                       AND pro_ahorros.cod_ahorro = 3
                       AND Pro_Ahorros.Estado EQ 1 NO-LOCK BY Pro_Ahorros.Tip_Ahorro
                                                           BY Pro_Ahorros.Cod_Ahorro:
    IF Pro_Ahorros.Id_PerLiquidacion EQ 1 THEN DO: /*Pdo.de Liquidación x Pdcto */
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador
                                 AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Indicadores) THEN DO:  /*Siempre se Liquida o causa con el Indic.Vigente*/
            W_TasaRango = Indicadores.Tasa.

            IF Indicadores.Rango THEN DO:
                FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador EQ Indicadores.Indicador
                                           AND Ran_Intereses.Puntos_Asoc NE 0 NO-LOCK NO-ERROR.
                IF AVAIL(Ran_Intereses) THEN
                    W_TasaRango = W_TasaRango + Ran_Intereses.Puntos_Asoc.
            END.

            IF W_TasaRango LE 0 THEN DO:
                MESSAGE "El Indicador de Tasa : " Pro_Ahorros.Indicador SKIP
                        "Para el Pdcto : " Pro_Ahorros.Cod_Ahorro " ,Tiene Tasa Menor O Igual a Cero(0)," SKIP
                        "Contractuales y Ala-Vista toman la Tasa del Indicador Con o sin Rangos...?" SKIP
                        "                Continue solo si Está Segura(0), " SKIP
                        "                de lo contrario Teclee NO y el Proceso será cancelado."
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR Liquidar con TASA 0" UPDATE W_RptaTasaCA AS LOGICAL.

                IF NOT W_RptaTasaCA THEN
                    RETURN ERROR.
            END.
        END.
        ELSE DO:
            IF NOT AVAIL(Indicadores) THEN DO:
                MESSAGE "Falta Indicador-Tasa y activo : " Pro_Ahorros.Indicador SKIP
                        "Para el Pdcto : " Pro_Ahorros.Cod_Ahorro  SKIP
                        "Contractuales y Ala-Vista toman la Tasa desde el Indicador..." SKIP
                        "                        Proceso cancelado."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.
    END.

    FOR EACH Ahorros WHERE Ahorros.Agencia = Agencias.Agencia
                       AND ahorros.tip_ahorro = pro_ahorros.tip_ahorro
                       AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro
                       AND ((ahorros.fec_vencimiento >= w_fecha OR ahorros.fec_vencimiento = ?) OR
                            (ahorros.fec_vencimiento < w_fecha AND ahorros.fec_vencimiento <> ?)) BY Ahorros.Nit
                                                                                                  BY ahorros.cue_ahorros:
        FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE clientes THEN
            NEXT.

        /* El asociado debe haber permanecido todo el trimestre */
        IF clientes.fec_Retiro <> ? THEN DO:
            CASE mesSaldo:
                WHEN 2 THEN fechaRetiro = DATE(3,31,YEAR(w_fecha)).
                WHEN 5 THEN fechaRetiro = DATE(6,30,YEAR(w_fecha)).
                WHEN 8 THEN fechaRetiro = DATE(9,30,YEAR(w_fecha)).
                WHEN 11 THEN fechaRetiro = DATE(12,31,YEAR(w_fecha) - 1).
            END CASE.
        END.

        IF clientes.fec_retiro < fechaRetiro AND ahorros.estado = 2 THEN
            NEXT.
        
        ASSIGN Ahorros.Num_RetDia = 0
               Ahorros.Val_RetDia = 0
               Ahorros.Num_DepDia = 0
               Ahorros.Val_DepDia = 0
               Ahorros.Num_RetDiaCheq = 0
               Ahorros.Val_RetDiaCheq = 0
               W-DiasLiq = 1              /*Inicia a liq.o causar 1 Día*/
               FProxLiq = W_Fecha + 1   /*Liq.Diaria-Inicia para Todos Mañana*/
               Ahorros.Tip_Ahorro = Pro_Ahorros.Tip_Ahorro  /*Lo trae desde config.pdctos*/
               FContVcto = /*Ahorros.Fec_Vencimiento*/ ?.

        IF Pro_Ahorros.Id_PerLiquidacion EQ 1 THEN DO:    /*El pdo.de Liquid.es Por Pdcto */
            Ahorros.Per_Liquidacion = Pro_Ahorros.Per_Liquidacion. /*Los trae desde Config.del Pdcto*/

            IF Pro_Ahorros.Per_liquidacion EQ 1 THEN        /*Liq.Diaria*/
                ASSIGN Ahorros.Fec_ProLiquidacion = W_Fecha
                       FProxLiq = W_Fecha + 1
                       W-DiasLiq = 1.
            ELSE
                IF Pro_Ahorros.Per_liquidacion EQ 2 THEN        /*Liq.Mensual*/
                    ASSIGN Ahorros.Fec_ProLiquidacion = FecLiqMes
                           FProxLiq = FProxLiqM
                           W-DiasLiq = 30.
                ELSE
                    IF Pro_Ahorros.Per_liquidacion EQ 3 THEN       /*Liq.Trimestral*/
                        ASSIGN FProxLiq = FProxLiqT
                               W-DiasLiq = 90.
                    ELSE
                        IF Pro_Ahorros.Per_liquidacion EQ 4 THEN       /*Liq.Semestral*/
                            W-DiasLiq = 180.
                        ELSE
                            IF Pro_Ahorros.Per_liquidacion EQ 5 THEN        /*Liq.Anual*/
                                W-DiasLiq = 360.
                            ELSE
                                IF Pro_Ahorros.Per_liquidacion EQ 6 THEN        /*Al-Vcto*/
                                    ASSIGN Ahorros.Fec_ProLiquidacion = Ahorros.Fec_Vencimiento - 1
                                           W-DiasLiq = Ahorros.Plazo
                                           FProxLiq = W_Fecha + Ahorros.Plazo.

            RUN HallaTasa_VistaYContrac.

            Ahorros.Tasa = W_TasaRango.  /*Siempre se Liquida o causa con el Indic.Vigente*/
        END.

        W-DiasALiq = W-DiasLiq.

        ahorros.fec_ProLiquidacion = w_fecha. /* Para obligar a liquidar hoy */

        /*Pregunta para Liquidar hoy*/
        IF (Ahorros.Fec_ProLiquidacion EQ W_Fecha AND (FContVcto GE W_Fecha OR FContVcto = ?)) THEN DO: /*O los que se vencen Mañana*/
            W_SiLiq = FALSE.

            RUN Liquida NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "liquidacion " cue_ahorro AHORROS.nit.
                
                RETURN ERROR.
            END.

            IF NOT W_SiLiq THEN DO:
                Ahorros.Sdo_Minimo = Ahorros.Sdo_Disponible.

                RELEASE TempLiq.

                NEXT.
            END.

            IF TempLiq.VrLiq GT 0 THEN DO:
                TotLiq = TotLiq + TempLiq.VrLiq.

                RUN BaseRF.  /*RetFuente x Intereses */
            END.
            ELSE
                W_VrReTfte = 0.  /*---> Gaer Feb.3/05*/

            ASSIGN Ahorros.Fec_ProLiquidacion = FProxLiq     /*Inicia para todos*/
                   Ahorros.Fec_UltLiquidacion = W_Fecha
                   Ahorros.Int_Causado = 0
                   TempLiq.VrLiq = TempLiq.VrLiq - W_VrReTfte
                   Ahorros.Val_RetAcum = Ahorros.Val_RetAcum + W_VrReTfte.

            IF Ahorros.Per_liquidacion EQ 6 /*Todos los que liq.al vencimiento*/ AND Ahorros.Fec_Vencimiento LT FProxLiq THEN /*La fecha de vcto es más rápida que la de prox.liq.*/
                Ahorros.Fec_ProLiquidacion = Ahorros.Fec_Vencimiento - 1.

            IF Ahorros.Des_Intereses EQ 3 THEN
                ASSIGN Ahorros.INT_Pagar = Ahorros.INT_Pagar + TempLiq.VrLiq
                       TempLiq.CtaXP = 2.              /*A la CtaxP de Liq.*/
            ELSE
                ASSIGN TempLiq.CtaXP = 3         /*Resto Para Hallar La Cta-Destino en c/agencia*/
                       TempLiq.AgeD = Ahorros.Agencia_Destino
                       TempLiq.PtoD = Ahorros.Pro_Destino
                       TempLiq.CtaD = Ahorros.Cue_Destino.

            IF TempLiq.VrRF GT 0 THEN DO:
                RUN MovAhorrro.

                ASSIGN Mov_Ahorros.Val_Efectivo = TempLiq.VrRf
                       Mov_Ahorros.Descrip = "RetFuente X Liq.Interés"
                       Mov_Ahorros.Cod_Operacion = 010102001 /*se adiciona para que coja bien el informe*/
                       Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
                       TempLiq.DifLC = TempLiq.DifLC - TempLiq.VrRf.
            END.
        END.

        ASSIGN TempLiq.NvoSdo = Ahorros.Sdo_Disponible WHEN AVAIL(TempLiq).

        RELEASE TempLiq.

        /* GCamacho - May06/08 - Implementacion Ahorro Permanente. */
        IF vlCambiaFecha THEN /* corresponde a cambio en prod. 221. cambió fecha de vencimiento. se debe dejar la de W_fecha + 2 para que nunca venza*/
            UPDATE Ahorros.Fec_Vencimiento = W_fecha + 2
                   Ahorros.Fec_ProLiquidacion = FecLiqTri.
    END.  /*Fin Recorrido Ahorros*/

    IF (TotLiq - W_TtRetFte) GT 0 THEN DO:  /*Estan restados la liq.intereses x CDATs(ATermino)*/
        /**GMF a cargo de la entidad x Intereses, Tot x Agencia y Pdcto*/
        RUN RutGMF.R (INPUT TRUE,
                      INPUT W_Agencia,
                      INPUT Agencias.Agencia,
                      INPUT 1,
                      INPUT Pro_Ahorros.Cod_Ahorro,
                      INPUT wnitEmp,
                      INPUT wnitEmp,
                      INPUT 010302001,
                      INPUT TotLiq - W_TtRetFte,
                      INPUT Comprobantes.Comprobante,
                      INPUT STRING(Comprobantes.Secuencia),
                      INPUT "LiqInt.Ahorro",
                      INPUT 1,
                      INPUT 0,
                      OUTPUT P_ImpAplic) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "El programa RutGMF.P...Retornó ERROR, no se permite la operación." SKIP
                    TotLiq W_TtRetFte
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

    ASSIGN TotLiq = 0
           W_TTRetFte = 0.
END.  /*Fin recorre Pro_Ahorros*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso2 W-Prc_LiqIntAhorro 
PROCEDURE Proceso2 :
DEFI VAR TotA LIKE Ahorros.Sdo_disponible INIT 0.
DEFI VAR TotC LIKE Ahorros.Sdo_disponible INIT 0.
DEFI VAR TotD LIKE Ahorros.Sdo_disponible INIT 0.
DEFI VAR Totl LIKE Ahorros.Sdo_disponible INIT 0.
DEFI VAR TotAcC LIKE TempLiq.AcCau.

FOR EACH TempLiq WHERE TempLiq.CtaxP EQ 3 AND TempLiq.VrLiq GT 0:    /*Abona intereses a la Cta-Destino*/
    RUN AbonaADestino NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.

Msaje:SCREEN-VALUE IN FRAME F_Proc = "Espere un momento por favor, Procesando Contabilización...".

FOR EACH TempLiq BREAK BY TempLiq.Agen
                       BY tempLiq.nitA
                       BY TempLiq.Pto:  /*Ajusta Liq.vs Causación y causa lo de hoy*/
    ASSIGN TotC = TotC + TempLiq.VrCau    /*Causados del día*/
           TotD = TotD + TempLiq.DifLC    /*Diferencias entre Liq.y causación*/
           TotAcC = TotAcC + TempLiq.AcCau.   /*Tot.Acumulados para reversar*/

    IF LAST-OF(TempLiq.Pto) THEN DO:
        FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.Agen
                              AND TempCtas.Pto EQ TempLiq.Pto NO-ERROR.

        IF TotC GT 0 THEN DO:                     /*Contab los Nuevos Causados de hoy*/
            ASSIGN W_Cta = TempCtas.CtaGto         /*Debe al Gasto*/
                   W_Val = TotC
                   W_Nat = "DB".

            RUN Contable.
            mov_contable.nit = TempLiq.NitA.

            ASSIGN W_Cta = TempCtas.CtaCau        /*Haber Cta-Causación*/
                   W_Val = TotC
                   W_Nat = "Cr".

            RUN Contable.
        END.

        /*Ajusta Liq.con causados por Dif.*/
        IF TotD GT 0 THEN DO:                     /*Mayor Liq.Dif.al Debe del gasto,TotD GT 0 */
            ASSIGN W_Cta = TempCtas.CtaGto
                   W_Val = TotD
                   W_Nat = "DB".

            RUN Contable.   /*La contrapartida es la C x P(Código más abajo) que está en el total liquidado-*/
            mov_contable.nit = TempLiq.NitA.
        END.
        ELSE
            IF TotD LT 0 THEN DO:                /*Dif.al Haber del gasto,TotD LT 0 */
                ASSIGN W_Cta = TempCtas.CtaGto
                       W_Val = TotD * -1
                       W_Nat = "Cr".

                RUN Contable. /*La contrapartida está en el Debe del Acum.Causados-*/

                mov_contable.nit = TempLiq.NitA.
            END.

        IF TotAcC GT 0 THEN DO:  /*Es el total ya causado que se debe reversar en la origen*/
            ASSIGN W_Cta = TempCtas.CtaCau        /*Debe Cta-Causación*/
                   W_Val = TotAcC
                   W_Nat = "Db".

            RUN Contable.
            mov_contable.nit = TempLiq.NitA.
        END.

        ASSIGN TotD = 0
               TotC = 0
               TotAcC = 0.
    END.
END.

FOR EACH TempLiq WHERE (TempLiq.AgeD EQ 0 OR TempLiq.AgeD EQ TempLiq.Agen)
                   AND TempLiq.VrLiq GT 0 BREAK BY TempLiq.Agen
                                                BY TempLiq.Pto
                                                BY TempLiq.CtaxP
                                                BY tempLiq.nitA
                                                BY TempLiq.CtaL:  /*Contab las CxP (Solo misma Agencia*/

    TotL = TotL + TempLiq.VrLiq.

    IF TempLiq.VrRF GT 0 THEN
        RUN ContabRFte.   /*Retefuente Individual en Contab*/

    IF LAST-OF(TempLiq.CtaL) THEN DO:
        FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.Agen
                              AND TempCtas.Pto EQ TempLiq.Pto NO-ERROR.

        ASSIGN W_Cta = TempCtas.CtaAho         /*La CxP de lo Liquidado- Inicia con la ContableAhorros*/
               W_Val = TotL
               W_Nat = "Cr".

        IF TempLiq.CtaxP EQ 3 AND TempLiq.CtaL GT "0" THEN      /*Es la CxP pero de la Cta-Destino*/
            W_Cta = TempLiq.CtaL.
        ELSE
            IF TempLiq.CtaxP EQ 2 THEN                         /*Es la de CxP x Liquidac.*/
                W_Cta = TempCtas.CtaLiq.

        RUN Contable.
        mov_contable.nit = TempLiq.NitA.

        TotL = 0.
    END.
END.

FOR EACH TempLiq WHERE (TempLiq.AgeD GT 0 AND TempLiq.AgeD NE TempLiq.Agen)
                   AND TempLiq.VrLiq GT 0 BREAK BY TempLiq.Agen
                                                BY TempLiq.Pto
                                                BY TempLiq.AgeD
                                                BY tempLiq.nitA
                                                BY TempLiq.CtaL:  /*Contab las CxP(Solo Agen-Diferentes-Son Ctas-Destino*/
    TotA = TotA + TempLiq.VrLiq.

    IF TempLiq.VrRF GT 0 THEN
        RUN ContabRFte.   /*Retefuente Individual en Contab*/

    IF LAST-OF(TempLiq.CtaL) THEN DO:
        FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.Agen           /*Pdcto Origen*/
                              AND TempCtas.Pto EQ TempLiq.Pto NO-ERROR.

        ASSIGN W_Cta = TempCtas.CtaSyA         /*CxP a la Cta.SyA en la Origen*/
               W_Val = TotA
               W_Nat = "Cr".

        RUN Contable.
        Mov_Contable.Nit = STRING(TempLiq.AgeD,"999").

        ASSIGN W_Cta = TempCtas.CtaSyA         /*Corresponde Cta.SyA en la Destino*/
               W_Val = TotA
               W_Nat = "Db".

        RUN Contable.

        ASSIGN Mov_Contable.Agencia = TempLiq.AgeD
               Mov_Contable.Destino = TempLiq.Agen
               Mov_Contable.Nit = STRING(TempLiq.Agen,"999").

        ASSIGN W_Cta = TempCtas.CtaL           /*La CxP en la Destino*/
               W_Val = TotA
               W_Nat = "Cr".

        RUN Contable.
        mov_contable.nit = tempLiq.nitA.
        
        ASSIGN Mov_Contable.Agencia = TempLiq.AgeD
               Mov_Contable.Destino = TempLiq.Agen.

        ASSIGN TotA = 0.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Prc_LiqIntAhorro 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
 Purpose:  Imprime Informe con Cpte.resumen Contable y el soporte de la
           Liquidación-Causación.
 ------------------------------------------------------------------------------*/
 /*RUN ImpCpte.  No necesario, lo imprimen por aparte   ...Abril 6/05 GAER*/

 DEFI VAR TLiq     LIKE TempLiq.VrLiq INIT 0.                                                                  
 DEFI VAR TCau     LIKE TempLiq.VrLiq INIT 0.
 DEFI VAR TDif     LIKE TempLiq.VrLiq INIT 0.
 DEFI VAR TLiqA    LIKE TempLiq.VrLiq INIT 0.                                                                  
 DEFI VAR TCauA    LIKE TempLiq.VrLiq INIT 0.
 DEFI VAR TDifA    LIKE TempLiq.VrLiq INIT 0.
 DEFI VAR TLiqG    LIKE TempLiq.VrLiq INIT 0.                                                                  
 DEFI VAR TCauG    LIKE TempLiq.VrLiq INIT 0.
 DEFI VAR TDifG    LIKE TempLiq.VrLiq INIT 0.
 DEFI VAR TRevC    LIKE TempLiq.VrLiq INIT 0 EXTENT 3.
 DEFI VAR RetFte   LIKE TempLiq.VrLiq INIT 0 EXTENT 3.
                                                                                                                                                   
 {Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Reporte   : Liquidación-Causación Intereses de Ahorros     Fecha del Informe: " +
                    STRING(W_FecMes,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS").

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 FOR EACH TempLiq BREAK BY TempLiq.Agen BY TempLiq.Pto BY TempLiq.Nit:                                                                                                                                    
     ASSIGN TLiq  = TLiq  + TempLiq.VrLiq
            TLiqA = TLiqA + TempLiq.VrLiq
            TLiqG = TLiqG + TempLiq.VrLiq
            TCau  = TCau  + TempLiq.VrCau
            TCauA = TCauA + TempLiq.VrCau
            TCauG = TCauG + TempLiq.VrCau
            TDif  = TDif  + TempLiq.DifLC
            TDifA = TDifA + TempLiq.DifLC
            TDifG = TDifG + TempLiq.DifLC
            RetFte [1] = RetFte [1] + TempLiq.VrRf
            RetFte [2] = RetFte [2] + TempLiq.VrRf
            RetFte [3] = RetFte [3] + TempLiq.VrRf
            TRevC  [1] = TRevC  [1] + TempLiq.AcCau
            TRevC  [2] = TRevC  [2] + TempLiq.AcCau
            TRevC  [3] = TRevC  [3] + TempLiq.AcCau.

     IF FIRST-OF(TempLiq.Pto) THEN DO:
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ TempLiq.Pto
                                 AND Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
        DISPLAY SKIP(1)
                "Producto : "
                TempLiq.Pto
                " - "
                Pro_Ahorros.Nom_Prod 
           WITH DOWN WIDTH 240 FRAME FPto USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
     END.
                                                                                                                                                  
     DISPLAY STRING(TempLiq.Agen,"999") + " " + STRING(TempLiq.Pto,"999")    LABEL "Ag. Pto"      
                                                            FORM "X(7)"
             STRING(TempLiq.Nit,"X(12)") + " " + STRING(TempLiq.Cta,"X(14)")
                             LABEL "Ced./Nit    Cta-Ahorros" FORM "X(27)"
             TempLiq.FAper   LABEL "Apertura"  FORM "99/99/99"
             TempLiq.FPror   LABEL "Prorroga"  FORM "99/99/99"
             TempLiq.FLiq    LABEL "PxLiquid"  FORM "99/99/99"
             TempLiq.FVcto   LABEL "Vencimto"  FORM "99/99/99"                                                         
             TempLiq.cuota   LABEL "Vr.Cuota"                FORM "->>,>>>,>>>"
             TempLiq.DiasL   LABEL "No.Dias"
             TempLiq.Base    LABEL "Base-Liquidac."          FORM "->>>>,>>>,>>9"
             TempLiq.Tasa    LABEL "% Tasa"                  FORM ">9.9999999"
             TempLiq.VrRf + TempLiq.VrLi LABEL "Tot.Liquid"  FORM "->>>>,>>>,>>9"
             TempLiq.VrRf    LABEL "ReteFuente"              FORM "->>>>,>>>,>>9"
             TempLiq.VrLiq   LABEL "Vr.Liq.-Interés"         FORM "->>>>,>>>,>>9"
             TempLiq.VrCau   LABEL "V.Int-Causado"           FORM "->>>>,>>>,>>9" 
             TempLiq.DifLC   LABEL "Diferenc.L - C"          FORM "->>>>,>>>,>>9"
             TempLiq.AcCauI  LABEL "Acumul.Causado"          FORM "->>>>,>>>,>>9"
             TempLiq.SdoDis  LABEL "Sdo-Disponible"          FORM "->>>>,>>>,>>9"
             TempLiq.SdoCan  LABEL "Saldo-Canje"             FORM "->>>>,>>>,>>9"
             TempLiq.NvoSdo  LABEL "Nuevo Sdo-Disp"          FORM "->>>>,>>>,>>9"
             TempLiq.CtaXP   LABEL "D"
             TempLiq.CtaD    LABEL "Cta-AhoDestino"          FORM "X(14)"
             /*STRING(TempLiq.CtaXP,"9")  + " "   + STRING(TempLiq.AgeD,"999") + "  " +   
             STRING(TempLiq.PtoD,"999") + "   " + STRING(TempLiq.CtaD,"X(14)")
                             LABEL "D Ag.D Pto.D Cta-AhoDestino"  FORM "X(31)"      */

             SKIP(0)                                                                                                                                       
        WITH DOWN WIDTH 320 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

     IF LAST-OF(TempLiq.Pto) THEN DO:
        DISPLAY SKIP(0)    
           "          TOTALES Producto ----> " 
           TempLiq.Pto
           " - "
           Pro_Ahorros.Nom_Prod   FORM "X(38)"
           "        "
           RetFte [1]  FORM "->>>>,>>>,>>9"    LABEL "Vr.Ret.Fuente"
           "   "
           Tliq        FORM "->>>>,>>>,>>9"    LABEL "Vlr. Liquidac"
           "   "
           TCau        FORM "->>>>>,>>>,>>9"   LABEL "Valor Causado"
           "  "
           TDif        FORM "->>>>>,>>>,>>9"   LABEL "Difer.Liq-Caus" 
           " " 
           TRevC  [1]  FORM "->>>>,>>>,>>9"    LABEL "Reversa Causac."
           SKIP(1)                                                                                                   
          WITH DOWN WIDTH 250 FRAME F4 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

        ASSIGN Tliq = 0
               TCau = 0
               TDif = 0
               RetFte [1] = 0
               TRevC  [1] = 0.
     END.

     IF LAST-OF(TempLiq.Agen) THEN DO:
        DISPLAY SKIP(1)                                                                                                                                      
           "                                                          AGENCIA TOTALES-----> "     
           "        "
           RetFte [2]   FORM "->>>>,>>>,>>9"  LABEL "Vr.Ret.Fuente"   
           "   "                                                     
           TliqA        FORM "->>>>,>>>,>>9"  LABEL "Vlr. Liquidac"  
           "   "                                                     
           TCauA        FORM "->>>>>,>>>,>>9" LABEL "Valor Causado" 
           "  "                                                      
           TDifA        FORM "->>>>>,>>>,>>9" LABEL "Difer.Liq-Caus"  
           " " 
           TRevC  [2]   FORM "->>>>,>>>,>>9"  LABEL "Reversa Causac."
           SKIP(2)                                                                                                   
          WITH DOWN WIDTH 250 FRAME F5 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

        ASSIGN TliqA = 0
               TCauA = 0
               TDifA = 0
               RetFte [2] = 0
               TRevC  [2] = 0.
     END.
 END.                                                                                                                                                 
                                                                                                                                                   
 DISPLAY SKIP(1)                                                                                                                                      
         "                                                          GENERAL TOTALES----->" 
         "        "
         RetFte [3]   FORM "->>>>,>>>,>>9"   LABEL "Vr.Ret.Fuente"  
         "   "                                                      
         TliqG        FORM "->>>>>,>>>,>>9"  LABEL "Vlr. Liquidac"  
         "   "                                                      
         TCauG        FORM "->>>>>,>>>,>>9"  LABEL "Valor Causado" 
         "  "                                                        
         TDifG        FORM "->>>>>,>>>,>>9"  LABEL "Difer.Liq-Caus" 
         " " 
         TRevC  [3]   FORM "->>>>,>>>,>>9"   LABEL "Reversa Causac."
     SKIP(2)                                                                                                   
      WITH DOWN WIDTH 250 FRAME FTg USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

 ASSIGN Tliqg = 0
        TCaug = 0
        TDifg = 0
        RetFte [3] = 0
        TRevC  [3] = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Renovacion_Ahorros_Renovables W-Prc_LiqIntAhorro 
PROCEDURE Renovacion_Ahorros_Renovables :
/*------------------------------------------------------------------------------
  Purpose: Renovar tasa, vencimiento y fecha de prorrogas para los ahorros que han vencido el dia anterior
  -------------------------------------------------------------------------------------------------------*/
  DEFI VAR BaseLiq  LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR W_TasaD  LIKE Ahorros.Tasa       INIT 0.
  DEFI VAR NroA       AS INTEG   FORMAT "999"   INIT 0.
  DEFI VAR WDia       AS INTEG   FORMAT "99".
  DEFI VAR W_DiaPer   AS INTEGER FORMAT "9999".
  DEFI VAR W_Per      AS DEC FORMAT "9999.99".
  DEFI VAR W_PerLiqui AS INTEGER FORMAT "9999".
  DEFI VAR W_TasaAnt  LIKE Ahorros.Tasa       INIT 0.

  DEFINE VAR W_Monto   LIKE Ahorros.Monto.
  DEFINE VAR W_Tasa    LIKE Indicadores.Tasa.
  DEFINE VAR W_TasPer  LIKE Indicadores.Tasa.

  W_TasaAnt = Ahorros.Tasa.


  /*Falta base Promedio Pro_Ahorros.Bas_Calculo EQ 2*/

  IF      Pro_Ahorros.Bas_Calculo EQ 1 AND Ahorros.Sdo_Minimo     GE Pro_Ahorros.Mon_MinLiqidacion THEN
     BaseLiq = Ahorros.Sdo_Minimo.
  ELSE IF Pro_Ahorros.Bas_Calculo EQ 2 AND Ahorros.Sdo_Minimo     GE Pro_Ahorros.Mon_MinLiqidacion THEN
     BaseLiq = Ahorros.Sdo_Minimo.
  ELSE IF Pro_Ahorros.Bas_Calculo EQ 3 AND Ahorros.Sdo_Disponible GE Pro_Ahorros.Mon_MinLiqidacion THEN
     BaseLiq = Ahorros.Sdo_Disponible.    
        
  IF  Pro_Ahorros.Id_Tasa    EQ 1   AND                                                                                 
      Pro_Ahorros.Tip_Ahorro EQ 3   AND                                                                                
      Pro_Ahorros.Id_RenVencimiento AND                                                                                
/*       Ahorros.Fec_Vencimiento EQ W_Fecha - 1 THEN DO:   /*Por Pdcto trae Nueva tasa desde Indicadores*/ */
      Ahorros.Fec_Vencimiento EQ W_Fecha - 4 THEN DO:   /*Por Pdcto trae Nueva tasa desde Indicadores*/    /* giocam 02/12/08 */
                                                                                                                       
      FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador AND                                  
                                   Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.                                        
      IF AVAIL(Indicadores) THEN DO:                                                                                   
         ASSIGN W_Tasa = Indicadores.Tasa + Ahorros.Pun_TasVariable.                                             
                                                                                                               
         IF Indicadores.Rango THEN DO:                                                                                 
            FIND FIRST Ran_Intereses WHERE                                                                             
                 Ran_Intereses.Indicador  EQ Indicadores.Indicador      AND                                            
                 BaseLiq                  GE Ran_Intereses.Val_Inicial  AND                                            
                 BaseLiq                  LE Ran_Intereses.Val_Final    AND                                            
                 Ahorros.Plazo            GE Ran_Intereses.Pla_Inicial  AND                                            
                 Ahorros.Plazo            LE Ran_Intereses.Pla_Final    AND                                            
                 Ran_Interes.Estado       EQ 1 NO-LOCK NO-ERROR.                                                       
                                                                                                               
            IF AVAIL(Ran_Intereses) THEN                                                                               
               W_Tasa = W_Tasa + Ran_Intereses.Puntos_Asoc.                                                
         END.                                                                                                          
      END.                                                                                                             
      ELSE DO:                                                                                                         
        MESSAGE "Para Tasa x Producto es Indispensable el Cod-Indicador y que exista en la configuraciòn." SKIP        
                "Mensaje en Procedimiento Renovacion_Ahorros_Renovables"                                               
            VIEW-AS ALERT-BOX.                                                                                         
        RETURN ERROR.                                                                                                  
      END.                                                                                                             
  END.                                                                                                                 
                                                                                                               
  IF  Pro_Ahorros.Id_RenVencimiento AND                                                                                
      Ahorros.Per_Liquidacion NE 1  AND                                                                                
/*       Ahorros.Fec_Vencimiento EQ W_Fecha - 1 THEN DO:     /* se aumenta renovacion de 1 a 3 */ */
      Ahorros.Fec_Vencimiento EQ W_Fecha - 4 THEN DO:     /* se aumenta renovacion de 1 a 3 */                                                             
      FProxLiq = Ahorros.Fec_ProLiquidacion.                                                                           
      ASSIGN Ahorros.Fec_Vencimiento = FProxLiq + 1                                                                       
             Ahorros.Fec_Prorroga    = W_Fecha.                                                                        
                                                                                                                                                                              
      IF Ahorros.Per_Liquidacion EQ 6 THEN
         ASSIGN Ahorros.Fec_ProLiquidacion = Ahorros.Fec_Vencimiento + Ahorros.Plazo
                Ahorros.Fec_Vencimiento    = Ahorros.Fec_ProLiquidacion + 1.
      ELSE IF (Ahorros.Per_Liquidacion EQ 5 AND Ahorros.Plazo GT 365) THEN DO:                                            
         ASSIGN NroA = ROUND(Ahorros.Plazo / 360,0) 
                WDia = DAY(FProxLiq).
         IF WDia GT 30 THEN
            WDia = 30.

         IF MONTH(FProxLiq) EQ 2 AND WDia GT 28 THEN
            WDia = 28.

         Ahorros.Fec_Vencimiento = DATE(MONTH(FProxLiq),WDia,(YEAR(FProxLiq) + NroA)).                 
      END.
      ELSE                                                                                                             
         IF ((  Ahorros.Per_Liquidacion EQ 2 AND Ahorros.Plazo NE 30)                                                 
            OR (Ahorros.Per_Liquidacion EQ 3 AND Ahorros.Plazo NE 90)                                                  
            OR (Ahorros.Per_Liquidacion EQ 4 AND Ahorros.Plazo NE 180))                                                
            AND Ahorros.Plazo GT 31 THEN                                                                               
                RUN HallaFVcto.                                                                                            
  END.   

  CASE Ahorros.Per_Liquidacion:
     WHEN 1 THEN
         ASSIGN W_Per      = 365
                W_Perliqui = 0
                W_DiaPer   = 1.
     WHEN 2 THEN
         ASSIGN W_Per      = 12
                W_Perliqui = 4
                W_DiaPer   = 30.
     WHEN 3 THEN
         ASSIGN W_Per      = 4
                W_Perliqui = 6
                W_DiaPer   = 90.
     WHEN 4 THEN
         ASSIGN W_Per      = 2
                W_Perliqui = 8
                W_DiaPer   = 180.
     WHEN 5 THEN
         ASSIGN W_Per      = 1
                W_Perliqui = 9
                W_DiaPer   = 360.
     WHEN 6 THEN 
         ASSIGN W_Per      = (360 / Ahorros.Plazo)
                W_Perliqui = 10
                W_DiaPer   = Ahorros.Plazo.     
  END CASE.
  
  IF Ahorros.For_Liquidacion EQ 2 THEN
     RUN EFNV IN W_ManFin (INPUT W_Tasa / 100, INPUT W_Per, OUTPUT W_TasPer).
  ELSE
     RUN EFNA IN W_ManFin (INPUT W_Tasa / 100, INPUT W_Per, OUTPUT W_TasPer).

  ASSIGN Ahorros.Tasa = (W_TasPer * W_Per) * 100.    

  DISPLAY Ahorros.Agencia              
          Ahorros.Nit                  
          Ahorros.Cue_ahorros          
          W_TasaAnt                    
          Ahorros.Tasa                 
          Ahorros.Fec_Apertura         
          Ahorros.Fec_Vencimiento      
          Ahorros.Fec_ProLiqui  VIEW-AS FILL-IN FORM "99/99/9999"       
          Ahorros.Per_Liqui     VIEW-AS FILL-IN FORM "99"          
          Ahorros.Plazo         VIEW-AS FILL-IN FORM "99999"          
      WITH DOWN WIDTH 120 FRAME Fnot USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Temp_ReversaCausa W-Prc_LiqIntAhorro 
PROCEDURE Temp_ReversaCausa :
/*------------------------------------------------------------------------------
  Gaer Feb.3/05.
  Purpose:  Crear la temporal con la diferencia(Causados) para reversa contable en 
            procedimiento proceso2. Solo los a la vista.
  ------------------------------------------------------------------------------*/
  IF Ahorros.Tip_Ahorro NE 1 THEN
     RETURN.
  
  RUN CrearTempL.

  ASSIGN TempLiq.AcCau  = 0                         
         TempLiq.VrCau  = 0  
         TempLiq.AcCau  = Ahorros.Int_Causado     /*La reversa de lo ya causado*/
         TempLiq.AcCauI = Ahorros.Int_Causado             /*Solo para el informe*/
         TempLiq.DifLC  = Ahorros.Int_Causado * - 1    /*Para que reste del gasto*/                           
         W_SiLiq        = TRUE.                                          
                                                                     
  RUN MovAhorrro.                                                       
  ASSIGN Mov_Ahorros.Val_Efectivo   = Ahorros.Int_Causado               
         Mov_Ahorros.Descrip        = "Reversa Causados x No-Liq."      
         Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
         Mov_Ahorros.Cod_Operacion  = 010302001
         Ahorros.Int_Causado        = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W-Prc_LiqIntAhorro 
PROCEDURE Valida :
FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro <> 4
                       AND Pro_Ahorros.Estado = 1 NO-LOCK:
    FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto = 1
                          AND CortoLargo.Cod_Producto = Pro_Ahorros.Cod_Ahorro
                          AND CortoLargo.Plazo_Inicial >= 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                          BY CortoLargo.Cod_Producto
                                                                          BY CortoLargo.Plazo_Inicial:
        IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.Cuenta = CortoLargo.Cta_AsoAd
                                 AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
            IF AVAILABLE(Cuentas) THEN
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = CortoLargo.Cta_SyA
                                     AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
            
            /*IF NOT AVAILABLE(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir Activas en Cuentas..." SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro SKIP
                        "De la Agencia : " CortoLargo.Agencia
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.*/

            CREATE TempCtas.
            ASSIGN TempCtas.Age = CortoLargo.Agencia
                   TempCtas.Pto = CortoLargo.Cod_Producto
                   TempCtas.CtaAho = CortoLargo.Cta_AsoAd
                   TempCtas.CtaSyA = CortoLargo.Cta_SyA.

            FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto = 1
                                   AND Liqui_Int.Cod_Producto = CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Liqui_Int) THEN DO:
                MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            FIND FIRST Cuentas WHERE Cuentas.Cuenta = Liqui_Int.CtaCr_LiqAso
                                 AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
            IF AVAILABLE(Cuentas) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = Liqui_Int.CtaDb_LiqAso
                                     AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
                IF AVAILABLE(Cuentas) THEN
                    FIND FIRST Cuentas WHERE Cuentas.Cuenta = Liqui_Int.Cta_CauCr
                                         AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
            END.

            /*IF NOT AVAILABLE(Cuentas) THEN DO:
                MESSAGE "En Liqui_Int las CtaCr_LiqAso,CtaDb_LiqAso y Cta_CauCr : " CtaCr_LiqAso "," CtaDb_LiqAso "y" Cta_CauCr SKIP
                        "Deben existir Activas en Plan de Cuentas..." SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.*/

            FIND FIRST Operacion WHERE Operacion.Cod_Operac = Liqui_Int.Cod_Operacion NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Operacion) THEN DO:
                MESSAGE "En Liqui_Int El Cod_Operación para la Liquid.Intereses debe existir." SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            ASSIGN TempCtas.CtaCau = Liqui_Int.Cta_CauCr
                   TempCtas.CtaGto = Liqui_Int.CtaDb_LiqAso
                   TempCtas.CtaLiq = Liqui_Int.CtaCr_LiqAso
                   TempCtas.Oper = Liqui_Int.Cod_Operacion
                   TempCtas.CtaRF = Liqui_Int.CtaCr_Ret.

            /*TempCtas.CtaGto = "61752001".
            TempCtas.CtaRF = "24453501".*/
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

