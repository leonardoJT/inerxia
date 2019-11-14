&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_LiqIntAhorro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_LiqIntAhorro 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

    /* oakley */

DEFI VAR W_TtReTfte AS DECIMAL.



   DEFI VAR W_VrReTfte LIKE Ahorros.INT_pagar.
   DEFI VAR W_Val      LIKE Mov_Contable.Db INIT 0.
   DEFI VAR W_Cta      LIKE Cuentas.Cuenta.
   DEFI VAR W_Nat      LIKE Cuentas.Naturaleza.
   DEFI VAR W-DiasLiq  AS INTEG FORM "99999" INIT 0.
   DEFI VAR W-DiasALiq AS INTEG FORM "99999" INIT 0.
   DEFI VAR FProxLiq   LIKE Ahorros.Fec_ProLiquidacion.
   DEFI VAR W_OpCgoAho LIKE Operacion.Cod_Operacion.
   DEFI VAR W_SiLiq     AS LOG INIT FALSE.
   DEFI VAR W_DiasLiq   AS DEC FORM "9999".
   DEFI VAR wnitEmp     LIKE entidad.nit.
   DEFI VAR W_TasaRango LIKE Ahorros.Tasa.

   DEFI VAR FecLiqMes  AS DATE.   /*Vble para fecha Fin-mes actual*/
   DEFI VAR FecLiqTri  AS DATE.   /*Vble para fecha Fin-trimestre que está corriendo*/
   DEFI VAR W_RowidCal AS ROWID.
   DEFI VAR ProxAno    AS INTEG FORMAT "9999". 
   DEFI VAR ProxMes    AS INTEG FORMAT "99".
   DEFI VAR ProxATri   AS INTEG FORMAT "9999". 
   DEFI VAR ProxMTri   AS INTEG FORMAT "99".
   
   DEFI VAR FProxLiqM  AS DATE INIT ?.        /*Inicia ?, Prox.fin mes diferente al actual*/
   DEFI VAR FProxLiqT  AS DATE INIT ?.        /*Inicia ?, Prox.fin trimestre diferente al actual */
   DEFI VAR FLiqAct    AS DATE INIT ?.         /*Inicia ?, Fecha de Prox.Liquid(Actual)*/   

   ASSIGN W_OpCgoAho = 010302001.      /*Retiros para Ahorros, solo inf para ir a RutGMF.P*/ 

   DEFI TEMP-TABLE TempCtas
        FIELD Agen   LIKE Ahorros.Agencia
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 W_CmbOfi BUTTON-5 Btn_Proc Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_FecMes W_Cont Msaje 

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
     LABEL "&Salir" 
     SIZE 10 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar" 
     SIZE 10 BY 1.46 TOOLTIP "Procesar los Diferidos".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 43.14 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 62.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

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
     SIZE 15 BY 6.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     W_CmbOfi AT ROW 1.35 COL 19 COLON-ALIGNED
     BUTTON-5 AT ROW 1.77 COL 68.29
     Btn_Proc AT ROW 3.73 COL 68.29
     W_FecMes AT ROW 4.35 COL 19 COLON-ALIGNED
     W_Cont AT ROW 5.31 COL 48.43 COLON-ALIGNED NO-LABEL
     Btn_Done AT ROW 5.54 COL 68.14 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Msaje AT ROW 6.54 COL 2 NO-LABEL
     "Regist.Proceso Perm." VIEW-AS TEXT
          SIZE 20 BY .88 AT ROW 4.5 COL 45
     RECT-314 AT ROW 1.27 COL 65.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.72 BY 6.88
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
         TITLE              = "Liquidaciòn-Causaciòn Diaria Int-Ahorros, Prog.W-Prc_LiqIntAhorro.W"
         HEIGHT             = 6.88
         WIDTH              = 82.72
         MAX-HEIGHT         = 20.31
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.31
         VIRTUAL-WIDTH      = 114.29
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
ON END-ERROR OF W-Prc_LiqIntAhorro /* Liquidaciòn-Causaciòn Diaria Int-Ahorros, Prog.W-Prc_LiqIntAhorro.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_LiqIntAhorro W-Prc_LiqIntAhorro
ON WINDOW-CLOSE OF W-Prc_LiqIntAhorro /* Liquidaciòn-Causaciòn Diaria Int-Ahorros, Prog.W-Prc_LiqIntAhorro.W */
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
    EMPTY TEMP-TABLE TempLiq.
    EMPTY TEMP-TABLE TempCtas.

    FIND FIRST entidad NO-LOCK NO-ERROR.

    wnitEmp = REPLACE(entidad.nit,"-","").

    FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin
                        AND Agencias.Estado <> 3 NO-LOCK:
        /* Revisar si el proceso fue realizado para el día anterior */
        FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                             AND ProcDia.Cod_Proceso = 2
                             AND ProcDia.Fecha_Proc = W_FecMes - 1
                             AND ProcDia.Estado = 2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(ProcDia) THEN DO:
            MESSAGE "Este proceso no fue ejecutado para el día Anterior en la agencia" STRING(Agencias.Agencia) + "." SKIP
                    "No se permiet la operación."
                VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

            RETURN.
        END.
        /* -------------------------------------------------------- */

        FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                             AND ProcDia.Cod_Proceso = 2
                             AND ProcDia.Fecha_Proc = W_FecMes
                             AND ProcDia.Estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(ProcDia) THEN DO:
            MESSAGE "Este proceso ya fue ejecutado para este día en la Agencia:" Agencias.Agencia SKIP
                    "O no está matriculado...Revise por favor. No se permite la operación."
                VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

            RETURN.
        END.
    END.

    FIND FIRST Varios WHERE Varios.Tipo = 8
                        AND Varios.Codigo = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE(varios) THEN
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia = W_Agencia
                                  AND Comprobantes.Comprobante = Varios.Comprobante
                                  AND Comprobantes.Estado = 1 NO-LOCK NO-ERROR.

    IF NOT AVAILABLE(Comprobantes) OR NOT AVAILABLE(varios) THEN DO:
        MESSAGE "El Comprobante Contable para el proceso debe existir en Tipos y en Comprobantes." SKIP
                "No se acepta la operación."
            VIEW-AS ALERT-BOX.

        RETURN.
    END.

    SESSION:SET-WAIT-STATE("GENERAL").

    RUN Valida NO-ERROR. /* oakley */
    IF ERROR-STATUS:ERROR THEN DO:
        SESSION:SET-WAIT-STATE("").
        Msaje:SCREEN-VALUE = "El proceso Liquidación de intereses de ahorros NO se efectuó... Revise por favor".
        RETURN.
    END.
    ELSE
        RUN Procesar.

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Prc_LiqIntAhorro
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
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

    IF W_OfiIni = 0 THEN
        W_CmbOfi:ADD-LAST("000 - Todas las agencias").

    ASSIGN W_FecMes:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Fecha)
           W_FecMes.

    FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin NO-LOCK:
        W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + STRING(Agencias.Nombre,"X(25)")).
        W_CmbOfi:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
    END.

    IF w_OfiIni = 0 THEN
        W_CmbOfi:SCREEN-VALUE = "000 - Todas las agencias".

    /*APPLY "VALUE-CHANGED" TO W_CmbOfi.*/

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
    MESSAGE "La cuenta destino:" TempLiq.CtaD SKIP
            "Del producto ahorros:" TempLiq.PtoD SKIP
            "En la agencia:" TempLiq.AgeD "... de la Cédula / Nit:" TempLiq.Nit SKIP
            "no existe... Revise por favor."
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

IF Ahorros.Estado NE 1 THEN
    Ahorros.Estado = 1.

ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + TempLiq.VrLiq
       Ahorros.Sdo_Minimo = Ahorros.Sdo_Minimo + TempLiq.VrLiq.

RUN MovAhorrro.

Mov_Ahorros.Descrip = "Dest CDAT Abono Liq.Interés". /* Agregado William Martínez 06-01-2009 */

FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.AgeD
                      AND TempCtas.Pto EQ TempLiq.PtoD NO-ERROR.

ASSIGN TempLiq.CtaL = TempCtas.CtaAho WHEN AVAIL(TempCtas).  /*Nva Contable para CxP en la Destino.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BaseRf W-Prc_LiqIntAhorro 
PROCEDURE BaseRf :
DEFINE VAR W_BasRet AS DECIMAL.
DEFINE VAR W_Porce AS DECIMAL.
DEFINE VAR W_Dialiq AS INTEGER.

W_Porce = 0.
W_BasRet = 0.
W_VrReTfte = 0.
W_Dialiq = TempLiq.DiasL.

FIND FIRST Liqui_Int WHERE Liqui_Int.Cod_Producto = Ahorros.Cod_ahorro
                       AND Liqui_Int.Clase_Producto = 1 NO-LOCK NO-ERROR.
IF AVAILABLE Liqui_Int THEN DO:
    W_BasRet = Liqui_Int.Base.

    FIND FIRST Base_Ret WHERE Base_Ret.Cod_base = Liqui_Int.Cod_base NO-LOCK NO-ERROR.
END.

IF AVAILABLE(Base_Ret) THEN
    W_Porce = Base_Ret.Porcentaje.

IF TempLiq.TipA = 2 THEN DO:  /*Contractuales respetan lo causado, Vble TempLiq.DiasL siempre es 1*/
    IF Ahorros.Per_Liquidac = 2 THEN DO:     /*Debe tomar los dias de per_liq.*/
        W_Dialiq = 30.

        IF MONTH(Fec_Apertura) = MONTH(W_Fecha) AND YEAR(Fec_Apertura) = YEAR(W_Fecha) THEN
            W_Dialiq = 30 - DAY(Fec_Apertura).
        ELSE DO:
            IF MONTH(Fec_Vencimiento) = MONTH(W_Fecha) AND YEAR(Fec_Vencimiento) = YEAR(W_Fecha) THEN
                W_Dialiq = DAY(W_Fecha).
        END.

        IF W_Dialiq <= 0 THEN
            W_DiaLiq = 1.
    END.
    ELSE DO:
        IF Ahorros.Per_Liquidac = 6 THEN
            W_Dialiq = Ahorros.Plazo.
    END.

    TempLiq.DiasL = W_Dialiq.
END.

FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.

IF (TempLiq.VrLiq / W_Dialiq) >= W_BasRet AND W_BasRet > 0 AND W_Porce > 0 AND Clientes.Id_Retencion = YES THEN DO:
    W_VrReTfte = ROUND(TempLiq.VrLiq * W_Porce / 100,0).
    TempLiq.VrRF = ROUND(TempLiq.VrLiq * W_Porce / 100,0).
    W_TtRetFte = W_TtRetFte + W_VrReTfte.
END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CausaDia W-Prc_LiqIntAhorro 
PROCEDURE CausaDia :
DEFINE VAR BaseLiq AS DECIMAL.
DEFINE VAR W_TasaD AS DECIMAL.

IF Pro_Ahorros.Bas_Calculo = 1 AND Ahorros.Sdo_Minimo >= Pro_Ahorros.Mon_MinLiqidacion THEN
    BaseLiq = Ahorros.Sdo_Minimo.
ELSE DO:
    IF Pro_Ahorros.Bas_Calculo = 2 AND Ahorros.Sdo_Minimo >= Pro_Ahorros.Mon_MinLiqidacion THEN
        BaseLiq = Ahorros.Sdo_Minimo.
    ELSE DO:
        IF Pro_Ahorros.Bas_Calculo = 3 AND Ahorros.Sdo_Disponible >= Pro_Ahorros.Mon_MinLiqidacion THEN
            BaseLiq = Ahorros.Sdo_Disponible.
    END.
END.

IF Ahorros.Tip_Ahorro = 1 AND Pro_Ahorros.Per_liquidacion = 3 THEN DO:
    IF (MONTH(W_Fecha) = 1 OR MONTH(W_Fecha) = 4 OR MONTH(W_Fecha) = 7 OR MONTH(W_Fecha) = 10) AND DAY(W_Fecha) <= Pro_Ahorros.Dia_Gracia AND Ahorros.Sdo_Disponible >= Pro_Ahorros.Mon_MinLiqidacion THEN
        BaseLiq = Ahorros.Sdo_Disponible.
END.

IF Ahorros.Sdo_Disponible <= 0 THEN
    BaseLiq = 0.

IF BaseLiq > 0 AND Ahorros.Tasa > 0 THEN DO:
    W_TasaD = Ahorros.Tasa / 360.

    IF Ahorros.Tip_Ahorro = 1 AND Pro_Ahorros.Per_liquidacion = 1 THEN
        W_TasaD = Ahorros.Tasa / 365.

    RUN CrearTempL.

    TempLiq.Base = BaseLiq.
    TempLiq.Tasa = W_TasaD.
    TempLiq.VrLiq = 0.
    TempLiq.DiasL = 1.
    Ahorros.Fec_UltCausa = W_Fecha.
    TempLiq.VrCau = ROUND((BaseLiq * W_TasaD) / 100,0).
    TempLiq.AcCauI = Ahorros.Int_Causado + TempLiq.VrCau.
END.
ELSE DO:
    IF Ahorros.Int_Causado > 0 THEN
        RUN Temp_ReversaCausa.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contable W-Prc_LiqIntAhorro 
PROCEDURE Contable :
/*------------------------------------------------------------------------------
  Purpose:     
-----------------------------------------------------------------------------*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia       = TempLiq.Agen             
        Mov_Contable.Cuenta         = W_Cta         
        Mov_Contable.Fec_Contable   = W_Fecha                         
        Mov_Contable.Comentario     = "Liq.Int.Ahorros"           
        Mov_Contable.Usuario        = W_Usuario                       
        Mov_Contable.Cen_Costos     = W_Cencosgral                    
        Mov_Contable.Destino        = TempLiq.AgeD                       
        Mov_Contable.Comprobante    = Varios.Comprobante        
        Mov_Contable.Num_Documento  = TempLiq.CDocum          
        Mov_Contable.Fec_Grabacion  = TODAY                           
        Mov_Contable.Hora           = TIME                            
        Mov_Contable.Estacion       = W_Estacion.
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
          /*W_Cta               = TempCtas.CtaAho   Estaba hasta Feb.8/05 Gaer*/
          W_Cta               = TempCtas.CtaGto /*Es gasto,Porque lo abonado y Cont.en Gto, ya tiene restada la RetFuente*/
          W_Nat               = "DB". 

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
CREATE TempLiq.
TempLiq.Agen = Ahorros.Agencia.
TempLiq.CDocum = Comprobantes.Secuencia.
TempLiq.NitA = Ahorros.Nit.
TempLiq.Pto = Ahorros.Cod_Ahorro.
TempLiq.TipA = Ahorros.Tip_Ahorro.
TempLiq.Cta = Ahorros.Cue_Ahorros.
TempLiq.FAper = Ahorros.Fec_Apertura.
TempLiq.FVcto = Ahorros.Fec_Vencimiento.
TempLiq.FLiq = Ahorros.Fec_ProLiquidacion.
TempLiq.Cuota = Ahorros.Cuota.
TempLiq.SdoDis = Ahorros.Sdo_Disponible.
TempLiq.SdoCan = Ahorros.Sdo_Canje.
TempLiq.NvoSdo = Ahorros.Sdo_Disponible.
TempLiq.CtaXP = 1.
TempLiq.Base = 0.
TempLiq.Tasa = 0.
TempLiq.VrLiq = 0.
TempLiq.DiasL = W-DiasALiq.
TempLiq.VrCau = 0.
TempLiq.AcCauI = Ahorros.INT_Causado.
W_Cont = W_Cont + 1.
W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

IF Ahorros.Tip_Ahorro <= 2 THEN DO:
    TempLiq.FPror = ?.

    IF Ahorros.Tip_Ahorro = 1 THEN
        TempLiq.FVcto = ?.
END.
ELSE
    TempLiq.FPror = Ahorros.Fec_Prorroga.
  
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
  DISPLAY W_CmbOfi W_FecMes W_Cont Msaje 
      WITH FRAME F_Proc IN WINDOW W-Prc_LiqIntAhorro.
  ENABLE RECT-314 W_CmbOfi BUTTON-5 Btn_Proc Btn_Done 
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
DEFI VAR NMes AS INTEG FORMAT "99" INIT 0.
DEFI VAR FProxVcto AS DATE.
DEFI VAR NroM AS INTEG FORMAT "99" INIT 0.
DEFI VAR WDia AS INTEG FORMAT "99".

ASSIGN FProxVcto = W_Fecha - 1 + Ahorros.Plazo
       NMes = MONTH(W_Fecha)
       NroM = ROUND(Ahorros.Plazo / 30,0)
       WDia = DAY(Ahorros.Fec_Apertura).

IF WDia GT 30 THEN
    WDia = 30.

IF Ahorros.Plazo >= 30 THEN DO:
    /*ahorros.fec_vencimiento = ADD-INTERVAL(ahorros.fec_vencimiento,nroM,"months").

    IF MONTH(ahorros.fec_vencimiento) <> 2 AND DAY(ahorros.fec_apertura) > DAY(ahorros.fec_vencimiento) THEN DO:
    
    ahorros.plazo = NroM * 30.
        
    END.*/

    NMes = Nmes + NroM.

    IF NMes >= 25 THEN DO:
        NMes = NMes - 24.

        IF NMes = 2 AND WDia > 28 THEN
            WDia = 28.

        FProxVcto = DATE(NMes,WDia,YEAR(W_Fecha) + 2).
    END.
    ELSE
        IF NMes >= 13 THEN DO:
            NMes = NMes - 12.

            IF NMes = 2 AND WDia > 28 THEN
                WDia = 28.

            FProxVcto = DATE(NMes,WDia,YEAR(W_Fecha) + 1).
        END.
        ELSE DO:
            IF NMes = 2 AND WDia > 28 THEN
                WDia = 28.

            FProxVcto = DATE(NMes,WDia,YEAR(W_Fecha)). 
        END.

    Ahorros.Fec_Vencimiento = FProxVcto.
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
  DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

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
                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
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
DEFINE VAR BaseLiq AS DECIMAL.
DEFINE VAR W_TasaD AS DECIMAL.
DEFINE VAR W_PdoAno AS INTEGER.

W_VrReTfte = 0.

IF Pro_Ahorros.Bas_Calculo = 1 AND Ahorros.Sdo_Minimo >= Pro_Ahorros.Mon_MinLiqidacion THEN
    BaseLiq = Ahorros.Sdo_Minimo.
ELSE DO:
    IF Pro_Ahorros.Bas_Calculo = 2 AND Ahorros.Sdo_Minimo >= Pro_Ahorros.Mon_MinLiqidacion THEN
        BaseLiq = Ahorros.Sdo_Minimo.
    ELSE
        IF Pro_Ahorros.Bas_Calculo EQ 3 AND Ahorros.Sdo_Disponible >= Pro_Ahorros.Mon_MinLiqidacion THEN
            BaseLiq = Ahorros.Sdo_Disponible.
END.

IF Ahorros.Sdo_Disponible <= 0 THEN
    BaseLiq = 0.

IF ahorros.cod_ahorro = 3 THEN /* Ahorro Pemanente / No se liquida */
    BaseLiq = 0.

IF BaseLiq > 0 AND Ahorros.Tasa > 0 THEN DO:
    W_TasaD = Ahorros.Tasa / 365.
    W_SiLiq = TRUE.

    IF Ahorros.Tip_Ahorro = 1 AND Ahorros.Per_liquidacion = 1 THEN  /* Ahorro a la Vista - Periodicidad de liquidación diaria */
        W_TasaD = Ahorros.Tasa / 365.

    RUN CrearTempL.

    TempLiq.Base = BaseLiq.
    TempLiq.Tasa = W_TasaD.
    TempLiq.VrLiq = ROUND((BaseLiq * W_TasaD * W-DiasALiq) / 100,0).
    Ahorros.Sdo_UltLiquidacion = BaseLiq.

    IF Pro_Ahorros.Tip_Ahorro = 3 AND Ahorros.Per_liquidacion >= 2 AND Ahorros.Per_liquidacion <= 5 THEN DO:
        W_PdoAno = 12.  /*Inicia Mensual*/

        CASE ahorros.per_liquidacion:
            WHEN 3 THEN w_pdoAno = 4.   /* Trimestral */
            WHEN 4 THEN w_pdoAno = 2.   /* Semestral */
            WHEN 5 THEN w_pdoAno = 1.   /* Anual */
        END CASE.
        
        W_TasaD = ROUND(Ahorros.Tasa,2).
        TempLiq.VrLiq = ROUND((BaseLiq * W_TasaD) / (W_PdoAno * 100),0).
        TempLiq.Tasa = W_TasaD / W_PdoAno.
        W_TasaD = Ahorros.Tasa / 360.  /* Deja la variable diaria */
    END.

    IF Pro_Ahorros.Tip_Ahorro = 2 THEN DO:  /* Contractuales */
        TempLiq.Base = ahorros.sdo_disponible.
        TempLiq.DifLC = TempLiq.VrLiq.
        TempLiq.VrLiq = TempLiq.VrLiq + Ahorros.Int_Causado.
        TempLiq.AcCau = Ahorros.Int_Causado.
        TempLiq.VrCau = 0.
    END.
    ELSE DO:
        IF Ahorros.Per_Liquidacion = 1 THEN DO:  /*Todos los de Liq.Diaria-No causan*/
            TempLiq.AcCau = 0.
            TempLiq.DifLC = TempLiq.VrLiq.   /*Para Contab.valor del gasto*/
        END.
        ELSE DO:
            IF Ahorros.Per_Liquidacion <> 1 AND Ahorros.Int_Causado <> 0 THEN DO:     /*Resto A la vista y A-Tno*/
                TempLiq.AcCau = Ahorros.Int_Causado.   /*Para reversa de todo lo causado*/
                TempLiq.DifLC = TempLiq.VrLiq - Ahorros.Int_Causado.  /*Si causados mayor que Liq.-->Negativos,Reversa gasto*/
            END.
            ELSE DO:
                IF (Pro_Ahorros.Tip_Ahorro = 3 OR Pro_Ahorros.Tip_Ahorro = 1) AND Ahorros.Per_Liquidacion <> 1 AND Ahorros.Int_Causado = 0 THEN    /* Le cancelaron los causados*/
                    TempLiq.DifLC = TempLiq.VrLiq.
            END.
        END.
    END.

    IF TempLiq.VrLiq > 0 THEN
        RUN MovAhorrro.
END.
ELSE DO:
    IF Ahorros.Int_Causado > 0 THEN    /*Los debe reversar, no hubo liquidación*/
        RUN Temp_ReversaCausa.           /*Solo los a la vista, porque si es diaria no deben tener causados*/
END.                                    /*Y si es Fin mes o fin trimestre no tenían sdo_mínimo*/
                                        
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
DEFINE VAR Listado AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_ArchTR AS CHARACTER FORMAT "X(40)".

W_VrReTfte = 0.

Liquidacion:
DO TRANSACTION ON ERROR UNDO Liquidacion:
    FIND FIRST Varios WHERE Varios.Tipo = 8
                        AND Varios.Codigo = 2 NO-LOCK NO-ERROR.

    ASSIGN listado = W_PathSpl + "\" + SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME F_Proc,1,3) + "-" + STRING(W_Fecha,"999999") + "CpLAho.Lst"
           Msaje:SCREEN-VALUE IN FRAME F_Proc = "Espere un momento por favor... Proceso en ejecución!".

    W_ArchTR = W_PathSpl + "\" + "TasasRenov" + STRING(W_Fecha,"999999") + ".Txt".

    OUTPUT TO VALUE(W_ArchTR).
    DISPLAY "Ag. Ced./Nit     No.Titulo         Tasa-Ant    Tasa-Act Fec-Apert. Fec-Vcto.  Fec-PxLiq PdoL Plazo" SKIP
            "--- ------------ -------------- ----------- ----------- ---------- ---------- --------- ---- -----"
        WITH DOWN WIDTH 120 FRAME FTitnot USE-TEXT STREAM-IO NO-LABELS NO-BOX.

    FOR EACH Agencias WHERE Agencias.Estado <> 3
                        AND Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin NO-LOCK:
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia = Agencias.Agencia
                                  AND Comprobantes.Comprobante = Varios.Comprobante
                                  AND Comprobantes.Estado = 1 NO-ERROR.
        IF NOT AVAILABLE(Comprobantes) THEN DO:
            MESSAGE "Falta el comprobante:" Varios.Comprobante SKIP
                    "para la agencia:" Agencias.nombre SKIP
                    "Proceso cancelado!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.

        Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
        Msaje:SCREEN-VALUE IN FRAME F_Proc = "Espere por favor... Proceso en ejecución para la agencia " + agencias.nombre.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

        RUN Proceso1 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            Msaje:SCREEN-VALUE = "El Proceso Liquidaciòn-Causaciòn NO se efectuò...Revise por favor".

                RETURN ERROR.
            END.
        END.
        OUTPUT CLOSE.

        IF NOT ERROR-STATUS:ERROR THEN DO:
            /* oakley */
            RUN Proceso2 NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            Msaje:SCREEN-VALUE = "El Proceso Liquidaciòn-Causaciòn NO se efectuò...Revise por favor".

            RETURN ERROR.
        END.

        FOR EACH Agencias WHERE Agencias.Estado EQ 2
                            AND Agencias.Agencia GE W_OfiIni
                            AND Agencias.Agencia LE W_OfiFin NO-LOCK:
            FIND FIRST ProcDia WHERE ProcDia.Agencia EQ Agencias.Agencia
                                 AND ProcDia.Cod_Proceso EQ 2
                                 AND ProcDia.Fecha_Proc EQ W_FecMes
                                 AND ProcDia.Estado EQ 1 NO-ERROR.

            ProcDia.Estado = 2.
        END.

        {Incluido\ImpArch.I "listado"}

        ASSIGN Msaje:SCREEN-VALUE = "Proceso Liq-Causaciòn Ahorros Terminó Exitosamente...".
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso1 W-Prc_LiqIntAhorro 
PROCEDURE Proceso1 :
DEFINE VAR P_ImpAplic AS DECIMAL.
DEFINE VAR TotLiq AS DECIMAL.
DEFINE VAR FContVcto AS DATE.
DEFINE VAR W_RowidInd AS ROWID.

RUN ValidaCalendario NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Valida calendario - Error!"
        VIEW-AS ALERT-BOX.

    RETURN ERROR.
END.

FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro <= 3
                       AND Pro_Ahorros.Estado = 1 NO-LOCK BY Pro_Ahorros.Tip_Ahorro
                                                          BY Pro_Ahorros.Cod_Ahorro:
    /* Omitimos los Ahorros Contractuales de liquidación trimestral */
    IF pro_ahorros.tip_ahorro = 2 AND pro_ahorros.cod_ahorro = 3 THEN
        NEXT.
    /* ---------- */

    IF Pro_Ahorros.Id_PerLiquidacion = 1 AND Pro_Ahorros.Tip_Ahorro <= 2 THEN DO: /* Contractuales y A la Vista */
        FIND FIRST Indicadores WHERE Indicadores.Indicador = Pro_Ahorros.Indicador
                                 AND Indicadores.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Indicadores THEN DO:  /* Siempre se liquida o causa con el Indicador vigente */
            W_TasaRango = Indicadores.Tasa.

            IF Indicadores.Rango THEN DO:
                FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador = Indicadores.Indicador
                                           AND Ran_Intereses.Puntos_Asoc <> 0 NO-LOCK NO-ERROR.
                IF AVAILABLE Ran_Intereses THEN
                    W_TasaRango = W_TasaRango + Ran_Intereses.Puntos_Asoc.
            END.
        END.
        ELSE DO:
            MESSAGE "No se encuentra tasa de interés configurada para" SKIP
                    "la liquidación/causación de intereses del producto" Pro_Ahorros.Cod_Ahorro SKIP
                    "Proceso cancelado...!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

    FOR EACH Ahorros WHERE Ahorros.Agencia = Agencias.Agencia
                       AND Ahorros.Cod_Ahorro = Pro_Ahorros.Cod_Ahorro BY Ahorros.Nit
                                                                       BY Ahorros.Cue_Ahorro:
        IF Ahorros.Tip_Ahorro = 1 AND Pro_Ahorros.Per_liquidacion = 3 /*A la vista trimestrales*/ AND Pro_Ahorros.Bas_calculo = 1 /*Base de Liq.Sdo_mínimo*/ THEN DO:
            IF Ahorros.Sdo_Minimo > Ahorros.Sdo_Disponible THEN
                Ahorros.Sdo_Minimo = Ahorros.Sdo_Disponible.

            IF Ahorros.Sdo_Minimo < 0 THEN
                Ahorros.Sdo_Minimo = 0.
        END.

        Ahorros.Num_RetDia = 0.
        Ahorros.Val_RetDia = 0.
        Ahorros.Num_DepDia = 0.
        Ahorros.Val_DepDia = 0.
        Ahorros.Num_RetDiaCheq = 0.
        Ahorros.Val_RetDiaCheq = 0.
        W-DiasLiq = 1.
        FLiqAct = W_Fecha.
        FProxLiq = W_Fecha + 1.
        Ahorros.Tip_Ahorro = Pro_Ahorros.Tip_Ahorro.
        FContVcto = Ahorros.Fec_Vencimiento.

        IF Pro_Ahorros.Tip_Ahorro = 1 THEN  /* A la vista */
            FContVcto = W_Fecha + 2.    /* El Ahorro a la Vista no maneja fecha de vencimiento */

        IF Calendario.Cierre THEN DO:
            Ahorros.Num_DepMes = 0.
            Ahorros.Val_DepMes = 0.
            Ahorros.Num_RetMes = 0.
            Ahorros.Val_RetMes = 0.
        END.

        IF Ahorros.Estado = 1 AND
           Ahorros.Detalle_Estado <> 4 AND
           Ahorros.Detalle_Estado <> 8 AND
           Pro_Ahorros.Id_BloInactividad AND
           Pro_Ahorros.Tie_Inactividad > 0 AND
           (W_Fecha - Ahorros.Fec_UltTrans) > Pro_Ahorros.Tie_Inactividad THEN
            Ahorros.Detalle_Estado = 4. /* Bloqueo */

        IF Ahorros.Tip_Ahorro = 2 AND Ahorros.Fec_ProLiquidacion = ? AND Ahorros.Fec_Vencimiento <= W_Fecha THEN
            NEXT.   /*Contractuales, Ya se vencieron y fueron liquidados*/

        IF (Ahorros.Estado <> 1 OR Ahorros.Sdo_Disponible <= 0) /*Inactivos o sin saldo*/ AND Ahorros.INT_Causado <= 0 THEN  /* Causados gt 0 para poderlos reversar */
            NEXT.  /*Sgte solo para los sin causados*/

        IF Ahorros.Fec_Apertura = W_Fecha AND Ahorros.Tip_Ahorro <> 2 THEN DO: /*Apertura hoy*/
            IF Ahorros.Per_liquidacion = 1 THEN     /* Diferentes a contractuales no causa, ni liquida hoy */
                Ahorros.Fec_ProLiquidacion = W_Fecha + 1.

            NEXT.
        END.
        
        IF Pro_Ahorros.Id_RenVencimiento AND Ahorros.Fec_ProLiquidacion <> W_Fecha AND Ahorros.Fec_Vencimiento = W_Fecha - 4 THEN DO:
            W_RowidInd = ROWID(Indicadores).
            RUN Renovacion_Ahorros_Renovables.
            FIND FIRST Indicadores WHERE ROWID(Indicadores) = W_RowidInd NO-LOCK NO-ERROR.
            NEXT.
        END.

        IF Pro_Ahorros.Id_PerLiquidacion = 1 THEN DO:    /*El pdo.de Liquid.es Por Pdcto */
            Ahorros.Per_Liquidacion = Pro_Ahorros.Per_Liquidacion. /*Los trae desde Config.del Pdcto*/

            IF Pro_Ahorros.Per_liquidacion = 1 THEN        /*Liq.Diaria*/
                ASSIGN FLiqAct = W_Fecha
                       Ahorros.Fec_ProLiquidacion = W_Fecha
                       FProxLiq = W_Fecha + 1
                       W-DiasLiq = 1.
            ELSE
                IF Pro_Ahorros.Per_liquidacion = 2 THEN        /*Liq.Mensual*/
                    ASSIGN FLiqAct = FecLiqMes
                           Ahorros.Fec_ProLiquidacion = FecLiqMes
                           FProxLiq = FProxLiqM
                           W-DiasLiq = 30.
                ELSE
                    IF Pro_Ahorros.Per_liquidacion = 3 THEN       /*Liq.Trimestral*/
                        ASSIGN FLiqAct = FecLiqTri
                               FProxLiq = FProxLiqT
                               W-DiasLiq = 90.
                    ELSE
                        IF Pro_Ahorros.Per_liquidacion = 4 THEN       /*Liq.Semestral*/
                            W-DiasLiq = 180.
                        ELSE
                            IF Pro_Ahorros.Per_liquidacion = 5 THEN        /*Liq.Anual*/
                                W-DiasLiq = 360.
                            ELSE
                                IF Pro_Ahorros.Per_liquidacion = 6 THEN        /*Al-Vcto*/
                                    ASSIGN FLiqAct = Ahorros.Fec_Vencimiento - 1
                                           Ahorros.Fec_ProLiquidacion = Ahorros.Fec_Vencimiento - 1
                                           W-DiasLiq = Ahorros.Plazo
                                           FProxLiq = W_Fecha + Ahorros.Plazo.

            IF Ahorros.Tip_Ahorro <= 2 THEN DO:     /*Contractuales y a la vista*/
                RUN HallaTasa_VistaYContrac.

                RUN EFNV IN w_manfin (INPUT W_TasaRango / 100,
                                      INPUT 360,
                                      OUTPUT w_tasaRango).

                Ahorros.Tasa = W_TasaRango * 360 * 100.  /*Siempre se Liquida o causa con el Indic.Vigente*/

                IF Ahorros.Tip_Ahorro = 1 /*A la Vista*/ AND
                   Ahorros.Per_liquidacion <> 1 /*No son Liq.diaria*/ AND
                   W_Fecha - Ahorros.Fec_Apertura < W-DiasLiq   /*Apertura posterior*/ AND
                   Ahorros.Fec_ProLiquidacion = W_Fecha THEN  /*Liquidan HOY*/
                    W-DiasLiq = W_Fecha - Ahorros.Fec_Apertura.  /*Para Liquidar menos dias*/

                IF Ahorros.Tip_Ahorro = 2 THEN DO: /*Contractuales, porque causa diario */
                    IF FContVcto - 1 = W_Fecha THEN      /*Se vencen Mañana-Los deja listos hoy*/
                        Ahorros.Fec_ProLiquidacion = W_Fecha.
                END.
            END. 
            ELSE /*Los A término config.Pdo.Liq. por Pdcto*/
                IF FContVcto - 1 = W_Fecha THEN DO:     /*Se vencen Mañana-Los deja listos hoy*/
                    Ahorros.Fec_ProLiquidacion = W_Fecha.
                    FProxLiq = W_Fecha + Ahorros.Plazo.

                    /* Para Fodun, todos los intereses de CDAT son capitalizables. Entonces obligamos que el producto quede configurado con destino de intereses el mismo título */
                    IF ahorros.tip_ahorro = 3 THEN DO:
                        ahorros.des_interes = 1.
                        ahorros.agencia_destino = ahorros.agencia.
                        ahorros.pro_Destino = ahorros.cod_ahorro.
                        ahorros.cue_destino = ahorros.cue_ahorros.
                    END.
                    /* ---------- */
                END.
        END.
        ELSE DO:      /*Liquidan x Pdo.de C/título(Cuenta)*/
            ASSIGN W-DiasLiq = 1
                   FProxLiq  = W_Fecha + 1.  /*Inicia Diaria*/

            IF Ahorros.Per_liquidacion = 2 THEN            /*Liq.Mensual*/
                W-DiasLiq = 30.
            ELSE
                IF Ahorros.Per_liquidacion = 3 THEN           /*Liq.Trimestral*/
                    W-DiasLiq = 90.
                ELSE
                    IF Ahorros.Per_liquidacion = 4 THEN            /*Liq.Semestral*/
                        W-DiasLiq = 180.
                    ELSE
                        IF Ahorros.Per_liquidacion = 5 THEN           /*Liq.Anual*/
                            W-DiasLiq = 360.
                        ELSE
                            IF Ahorros.Per_liquidacion = 6 THEN           /*Liq.Al Vencimiento*/
                                ASSIGN W-DiasLiq = Ahorros.Plazo
                                       FProxLiq = W_Fecha + Ahorros.Plazo.
                                
            IF Ahorros.Per_liquidacion > 1 AND Ahorros.Per_liquidacion < 6 THEN
                RUN HallaFecProxLiq.

            IF FContVcto - 1 = W_Fecha THEN DO:    /*Se vencen Mañana-Los deja listos hoy*/
                Ahorros.Fec_ProLiquidacion = W_Fecha.

                /* Para Fodun, todos los intereses de CDAT son capitalizables. Entonces obligamos que el producto quede configurado con destino de intereses el mismo título */
                IF ahorros.tip_ahorro = 3 THEN DO:
                    ahorros.des_interes = 1.
                    ahorros.agencia_destino = ahorros.agencia.
                    ahorros.pro_Destino = ahorros.cod_ahorro.
                    ahorros.cue_destino = ahorros.cue_ahorros.
                END.
            END.
        END.

        IF Ahorros.Tasa <= 0 THEN
            NEXT.

        W-DiasALiq = W-DiasLiq.

        /*Pregunta para Liquidar hoy*/
        IF (Ahorros.Fec_ProLiquidacion = W_Fecha AND FContVcto >= W_Fecha) /*Los que se deben Liquidar*/ OR (FContVcto - 1 = W_Fecha) THEN DO: /*O los que se vencen Mañana*/
            W_SiLiq = FALSE.

            RUN Liquida NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error en la liquidación de intereses para la cuenta" cue_ahorro "del titular" AHORROS.nit
                    VIEW-AS ALERT-BOX.

                RETURN ERROR.
            END.

            IF NOT W_SiLiq THEN DO:
                Ahorros.Sdo_Minimo = Ahorros.Sdo_Disponible.
                RELEASE TempLiq.
                NEXT.
            END.

            IF TempLiq.VrLiq > 0 THEN DO:
                TotLiq = TotLiq + TempLiq.VrLiq.

                RUN BaseRF.  /*RetFuente x Intereses */

                IF Ahorros.Tip_Ahorro = 3 AND Ahorros.Des_Intereses = 3 THEN
                    TotLiq = TotLiq - TempLiq.VrLiq.
            END.
            ELSE
                W_VrReTfte = 0.

            Ahorros.Fec_ProLiquidacion = FProxLiq.
            Ahorros.Fec_UltLiquidacion = W_Fecha.
            Ahorros.Int_Causado = 0.
            TempLiq.VrLiq = TempLiq.VrLiq - W_VrReTfte.
            Ahorros.Val_RetAcum = Ahorros.Val_RetAcum + W_VrReTfte.

            IF Ahorros.Tip_Ahorro = 3 THEN
                Ahorros.Fec_ProLiquidacion = FProxLiq - 1.

            IF Ahorros.Per_liquidacion = 6 /*Todos los que liq.al vencimiento*/ AND Ahorros.Fec_Vencimiento < FProxLiq THEN
                Ahorros.Fec_ProLiquidacion = Ahorros.Fec_Vencimiento - 1.

            IF Ahorros.Tip_Ahorro = 1 THEN DO:
                Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon + TempLiq.VrLiq.
                Ahorros.Sdo_Minimo = Ahorros.Sdo_Disponible.
                TempLiq.CtaXP = 1.           /*A la misma CtaAho*/

                ASSIGN Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje WHEN AVAIL(Mov_Ahorros).

                /* Contractuales que se vencen mañana, no vuelven a liquidar */
                IF Ahorros.Tip_Ahorro = 2 AND FContVcto - 1 = W_Fecha THEN
                    Ahorros.Fec_ProLiquidacion = ?.
            END.
            ELSE DO:
                IF (Pro_Ahorros.Tip_Ahorro = 3 OR pro_ahorros.tip_ahorro = 2) AND (Ahorros.Des_Intereses = 3) THEN DO:
                    Ahorros.INT_Pagar = Ahorros.INT_Pagar + TempLiq.VrLiq.
                    TempLiq.CtaXP = 2.              /*A la CtaxP de Liq.*/
                END.
                ELSE DO:
                    TempLiq.CtaXP = 3.         /*Resto Para Hallar La Cta-Destino en c/agencia*/
                    TempLiq.AgeD = Ahorros.Agencia_Destino.
                    TempLiq.PtoD = Ahorros.Pro_Destino.
                    TempLiq.CtaD = Ahorros.Cue_Destino.
                END.
            END.

            IF TempLiq.VrRF > 0 THEN DO:
                RUN MovAhorrro.

                Mov_Ahorros.Val_Efectivo = TempLiq.VrRf.
                Mov_Ahorros.Descrip = "RetFuente X Liq.Interés".
                Mov_Ahorros.Cod_Operacion = 010102001.
                Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
                TempLiq.DifLC = TempLiq.DifLC - TempLiq.VrRf.
            END.
        END.
        ELSE DO:
            IF Ahorros.Per_liquidacion <> 1 /*Los que no liquidan diario, ni se han vencido*/ AND FContVcto > W_Fecha THEN DO: /*Deben causar*/
                IF Pro_Ahorros.Tip_Ahorro = 3 AND DAY(W_Fecha) = 31 THEN.  /*Sept.2/05 GAER,Dia 31 no causan los CDATS*/
                ELSE DO:  /*El resto SI*/
                    RUN CausaDia NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN DO:
                        MESSAGE "Error en el proceso de causación de intereses de ahorros" SKIP
                                "para el producto" ahorros.cue_ahorros "del titular" ahorros.nit
                            VIEW-AS ALERT-BOX.

                        RETURN ERROR.
                    END.

                    IF AVAILABLE(TempLiq) THEN DO:
                        TempLiq.SdoDis = Ahorros.Sdo_Disponible.
                        TempLiq.SdoCan = Ahorros.Sdo_Canje.
                        TempLiq.NvoSdo = Ahorros.Sdo_Disponible.
                        Ahorros.Int_Causado = Ahorros.Int_Causado + TempLiq.VrCau.
                    END.

                    IF Ahorros.Tip_Ahorro = 1 AND Pro_Ahorros.Per_liquidacion = 3 THEN DO:
                        IF (MONTH(W_Fecha) = 1 OR MONTH(W_Fecha) = 4 OR MONTH(W_Fecha) = 7 OR MONTH(W_Fecha) = 10) AND DAY(W_Fecha) = Pro_Ahorros.Dia_Gracia THEN
                            Ahorros.Sdo_Minimo = Ahorros.Sdo_Disponible.
                    END.
                END.
            END.
            ELSE DO:
                IF Ahorros.Int_Causado > 0 THEN
                    RUN Temp_ReversaCausa.
            END.
        END.

        ASSIGN TempLiq.NvoSdo = Ahorros.Sdo_Disponible WHEN AVAILABLE(TempLiq).

        RELEASE TempLiq.
    END.

    IF (TotLiq - W_TtRetFte) GT 0 THEN DO:  /*Estan restados la liq.intereses x CDATs(ATermino)*/
        /**GMF a cargo de la entidad x Intereses, Tot x Agencia y Pdcto*/
        RUN RutGMF.R (INPUT TRUE,
                      INPUT W_Agencia,
                      INPUT Agencias.Agencia,
                      INPUT 1,
                      INPUT Pro_Ahorros.Cod_Ahorro,
                      INPUT wnitEmp,
                      INPUT wnitEmp,
                      INPUT W_OpCgoAho,
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
DEFINE VAR TotA AS DECIMAL.
DEFINE VAR TotC AS DECIMAL.
DEFINE VAR TotD AS DECIMAL.
DEFINE VAR Totl AS DECIMAL.
DEFINE VAR TotAcC AS DECIMAL.

FOR EACH TempLiq WHERE TempLiq.CtaxP = 3
                   AND TempLiq.VrLiq > 0:    /*Abona intereses a la Cta-Destino*/
    RUN AbonaADestino NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.

Msaje:SCREEN-VALUE IN FRAME F_Proc = "Espere un momento por favor, Procesando Contabilización...".

FOR EACH TempLiq BREAK BY TempLiq.Agen
                       BY tempLiq.NitA
                       BY TempLiq.Pto:  /*Ajusta Liq.vs Causación y causa lo de hoy*/
    ASSIGN TotC = TotC + TempLiq.VrCau    /*Causados del día*/
           TotD = TotD + TempLiq.DifLC    /*Diferencias entre Liq.y causación*/
           TotAcC = TotAcC + TempLiq.AcCau.   /*Tot.Acumulados para reversar*/

    IF LAST-OF(TempLiq.Pto) THEN DO:
    /*IF LAST-OF(TempLiq.NitA) THEN DO:*/
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
            mov_contable.nit = TempLiq.NitA.
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
                                                BY tempLiq.NitA
                                                BY TempLiq.CtaL:  /*Contab las CxP (Solo misma Agencia*/
    TotL = TotL + TempLiq.VrLiq.

    IF TempLiq.VrRF GT 0 THEN
        RUN ContabRFte.   /*Retefuente Individual en Contab*/

    IF LAST-OF(TempLiq.CtaL) THEN DO:
    /*IF LAST-OF(TempLiq.nitA) THEN DO:*/
        FIND FIRST TempCtas WHERE TempCtas.Age EQ TempLiq.Agen
                              AND TempCtas.Pto EQ TempLiq.Pto NO-ERROR.

        ASSIGN W_Cta = TempCtas.CtaAho         /*La CxP de lo Liquidado- Inicia con la ContableAhorros*/
               W_Val = TotL
               W_Nat = "Cr".

        IF TempLiq.CtaxP EQ 3 AND TempLiq.CtaL GT "0" THEN DO:      /*Es la CxP pero de la Cta-Destino*/
            W_Cta = TempLiq.CtaL.
        END.
        ELSE DO:
            IF TempLiq.CtaxP EQ 2 THEN DO:                         /*Es la de CxP x Liquidac.*/
                W_Cta = TempCtas.CtaLiq.
            END.
        END.

        RUN Contable.
        mov_contable.nit = tempLiq.nitA.
        
        TotL = 0.
    END.
END.

FOR EACH TempLiq WHERE (TempLiq.AgeD  GT 0 AND TempLiq.AgeD NE TempLiq.Agen)
                   AND TempLiq.VrLiq  GT 0 BREAK BY TempLiq.Agen
                                                 BY TempLiq.Pto
                                                 BY TempLiq.AgeD
                                                 BY tempLiq.nitA
                                                 BY TempLiq.CtaL:  /*Contab las CxP(Solo Agen-Diferentes-Son Ctas-Destino*/
    TotA = TotA + TempLiq.VrLiq.

    IF TempLiq.VrRF GT 0 THEN
        RUN ContabRFte.   /*Retefuente Individual en Contab*/

    IF LAST-OF(TempLiq.CtaL) THEN DO:
    /*IF LAST-OF(TempLiq.NitA) THEN DO:*/
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
DEFI VAR BaseLiq LIKE Ahorros.Sdo_Dispon INIT 0.
DEFI VAR W_TasaD LIKE Ahorros.Tasa INIT 0.
DEFI VAR NroA AS INTEG FORMAT "999" INIT 0.
DEFI VAR WDia AS INTEG FORMAT "99".
DEFI VAR W_DiaPer AS INTEGER FORMAT "9999".
DEFI VAR W_Per AS DEC FORMAT "9999.99".
DEFI VAR W_PerLiqui AS INTEGER FORMAT "9999".
DEFI VAR W_TasaAnt LIKE Ahorros.Tasa INIT 0.
DEFINE VAR W_Monto LIKE Ahorros.Monto.
DEFINE VAR W_Tasa LIKE Indicadores.Tasa.
DEFINE VAR W_TasPer LIKE Indicadores.Tasa.
DEFINE VAR fechaVencimientoAnterior AS DATE.

W_TasaAnt = Ahorros.Tasa.

IF Pro_Ahorros.Bas_Calculo EQ 1 AND Ahorros.Sdo_Minimo GE Pro_Ahorros.Mon_MinLiqidacion THEN
    BaseLiq = Ahorros.Sdo_Minimo.
ELSE
    IF Pro_Ahorros.Bas_Calculo EQ 2 AND Ahorros.Sdo_Minimo GE Pro_Ahorros.Mon_MinLiqidacion THEN
        BaseLiq = Ahorros.Sdo_Minimo.
    ELSE
        IF Pro_Ahorros.Bas_Calculo EQ 3 AND Ahorros.Sdo_Disponible GE Pro_Ahorros.Mon_MinLiqidacion THEN
            BaseLiq = Ahorros.Sdo_Disponible.

IF Pro_Ahorros.Id_Tasa EQ 1 AND Pro_Ahorros.Tip_Ahorro EQ 3 AND Pro_Ahorros.Id_RenVencimiento AND Ahorros.Fec_Vencimiento EQ W_Fecha - 4 THEN DO:
    FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador
                             AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(Indicadores) THEN DO:
        W_Tasa = Indicadores.Tasa + Ahorros.Pun_TasVariable.

        IF Indicadores.Rango THEN DO:
            FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador EQ Indicadores.Indicador
                                       AND BaseLiq GE Ran_Intereses.Val_Inicial
                                       AND BaseLiq LE Ran_Intereses.Val_Final
                                       AND Ahorros.Plazo GE Ran_Intereses.Pla_Inicial
                                       AND Ahorros.Plazo LE Ran_Intereses.Pla_Final
                                       AND Ran_Interes.Estado EQ 1 NO-LOCK NO-ERROR.
            IF AVAIL(Ran_Intereses) THEN
                W_Tasa = W_Tasa + Ran_Intereses.Puntos_Asoc.
        END.
    END.
    ELSE DO:
        MESSAGE "No se encontró un Indicador (Tasa de Interés) vigente para la renovación del producto" SKIP
                "de ahorro #" ahorros.cue_ahorros ", para el Asociado" ahorros.nit ". Revise por favor la configuración." SKIP
                "El producto se renueva con la misma tasa que tiene en el momento..."
            VIEW-AS ALERT-BOX.

        w_tasa = ahorros.tasa.
            
        /*RETURN ERROR. */
    END.
END.

IF Pro_Ahorros.Id_RenVencimiento AND Ahorros.Per_Liquidacion NE 1 AND Ahorros.Fec_Vencimiento EQ W_Fecha - 4 THEN DO:
    FProxLiq = Ahorros.Fec_ProLiquidacion.

    ASSIGN Ahorros.Fec_Vencimiento = FProxLiq + 1
           Ahorros.Fec_Prorroga = W_Fecha.

    IF Ahorros.Per_Liquidacion EQ 6 THEN DO:
        /* Para los CDAT se mueven periodos completos */
        IF ahorros.tip_ahorro = 3 THEN DO:
            fechaVencimientoAnterior = ahorros.fec_vencimiento.

            IF ahorros.cod_ahorro = 5 THEN /* CDAT a 3 meses */
                ahorros.fec_vencimiento = ADD-INTERVAL(ahorros.fec_vencimiento,3,"months").

            IF ahorros.cod_ahorro = 6 THEN /* CDAT a 6 meses */
                ahorros.fec_vencimiento = ADD-INTERVAL(ahorros.fec_vencimiento,6,"months").

            ahorros.plazo = INTERVAL(ahorros.fec_vencimiento, fechaVencimientoAnterior, "days").
            ahorros.fec_proLiquidacion = ahorros.fec_vencimiento - 1.
        END.
        ELSE
            ASSIGN Ahorros.Fec_ProLiquidacion = Ahorros.Fec_Vencimiento + Ahorros.Plazo
                   Ahorros.Fec_Vencimiento = Ahorros.Fec_ProLiquidacion + 1.
    END.
    ELSE
        IF (Ahorros.Per_Liquidacion EQ 5 AND Ahorros.Plazo GT 365) THEN DO:
            ASSIGN NroA = ROUND(Ahorros.Plazo / 360,0)
                   WDia = DAY(FProxLiq).

            IF WDia GT 30 THEN
                WDia = 30.

            IF MONTH(FProxLiq) EQ 2 AND WDia GT 28 THEN
                WDia = 28.

            Ahorros.Fec_Vencimiento = DATE(MONTH(FProxLiq),WDia,(YEAR(FProxLiq) + NroA)).
        END.
        ELSE
            IF ((Ahorros.Per_Liquidacion EQ 2 AND Ahorros.Plazo NE 30) OR
                (Ahorros.Per_Liquidacion EQ 3 AND Ahorros.Plazo NE 90) OR
                (Ahorros.Per_Liquidacion EQ 4 AND Ahorros.Plazo NE 180)) AND Ahorros.Plazo GT 31 THEN
                RUN HallaFVcto.
END.

CASE Ahorros.Per_Liquidacion:
    WHEN 1 THEN
        ASSIGN W_Per = 365
               W_Perliqui = 0
               W_DiaPer = 1.

    WHEN 2 THEN
        ASSIGN W_Per = 12
               W_Perliqui = 4
               W_DiaPer = 30.

    WHEN 3 THEN
        ASSIGN W_Per = 4
               W_Perliqui = 6
               W_DiaPer = 90.

    WHEN 4 THEN
        ASSIGN W_Per = 2
               W_Perliqui = 8
               W_DiaPer = 180.

    WHEN 5 THEN
        ASSIGN W_Per = 1
               W_Perliqui = 9
               W_DiaPer = 360.

    WHEN 6 THEN
        ASSIGN W_Per = (360 / Ahorros.Plazo)
               W_Perliqui = 10
               W_DiaPer = Ahorros.Plazo.
END CASE.

IF Ahorros.For_Liquidacion EQ 2 THEN
    RUN EFNV IN W_ManFin (INPUT W_Tasa / 100, INPUT W_Per, OUTPUT W_TasPer).
ELSE
    RUN EFNA IN W_ManFin (INPUT W_Tasa / 100, INPUT W_Per, OUTPUT W_TasPer).

Ahorros.Tasa = (W_TasPer * W_Per) * 100.

DISPLAY Ahorros.Agencia
        Ahorros.Nit
        Ahorros.Cue_ahorros
        W_TasaAnt
        Ahorros.Tasa
        Ahorros.Fec_Apertura
        Ahorros.Fec_Vencimiento
        Ahorros.Fec_ProLiqui VIEW-AS FILL-IN FORM "99/99/9999"
        Ahorros.Per_Liqui VIEW-AS FILL-IN FORM "99"
        Ahorros.Plazo VIEW-AS FILL-IN FORM "99999"
    WITH DOWN WIDTH 120 FRAME Fnot USE-TEXT STREAM-IO NO-LABELS NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Temp_ReversaCausa W-Prc_LiqIntAhorro 
PROCEDURE Temp_ReversaCausa :
IF Ahorros.Tip_Ahorro <> 1 THEN
    RETURN.

RUN CrearTempL.
TempLiq.AcCau = 0.
TempLiq.VrCau = 0.
TempLiq.AcCau = Ahorros.Int_Causado.
TempLiq.AcCauI = Ahorros.Int_Causado.
TempLiq.DifLC = Ahorros.Int_Causado * -1.
W_SiLiq = TRUE.

RUN MovAhorrro.
Mov_Ahorros.Val_Efectivo = Ahorros.Int_Causado.
Mov_Ahorros.Descrip = "Reversa Causados x No-Liq.".
Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
Mov_Ahorros.Cod_Operacion = 010302001.
Ahorros.Int_Causado = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W-Prc_LiqIntAhorro 
PROCEDURE Valida :
FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro <> 4 /* 4 - Aportes */
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

            IF NOT AVAILABLE(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir en el Catálogo de Cuentas" SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                        "De la Agencia:" CortoLargo.Agencia
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            CREATE TempCtas.
            ASSIGN TempCtas.Age = CortoLargo.Agencia
                   TempCtas.Pto = CortoLargo.Cod_Producto
                   TempCtas.CtaAho = CortoLargo.Cta_AsoAd
                   TempCtas.CtaSyA = CortoLargo.Cta_SyA.

            FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto = 1
                                   AND Liqui_Int.Cod_Producto = CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Liqui_Int) THEN DO:
                MESSAGE "Falta Liqui_Int para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro
                    VIEW-AS ALERT-BOX ERROR.
                
                RETURN ERROR.
            END.

            /* Para todas las líneas de ahorro (excepto 'Devoluciones') validamos la cuenta del costo parea la causación de los intereses */
            IF pro_ahorros.cod_ahorro <> 8 THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = Liqui_Int.CtaDb_LiqAso
                                     AND Cuentas.Tipo = 2
                                     AND cuentas.estado = 1 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(Cuentas) THEN DO:
                    MESSAGE "En la configuración de las cuentas para liquidación de intereses deben estar registradas" SKIP
                            "las cuentas contables para el débito de la liquidación de intereses (cuenta del costo)" SKIP
                            "para el producto" STRING(pro_ahorros.cod_ahorro,"99") + " - " + pro_ahorros.nom_producto + "." SKIP(2)
                            "Se cancela la operación hasta tanto se corrija la configuración contable para la" SKIP
                            "liquidación de intereses!"
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.
            END.

            IF pro_ahorros.tip_ahorro <> 1 AND pro_ahorros.cod_ahorro <> 3 /* Ahorro permanente */ THEN DO: /* Para todos los ahorros distintos a Ahorro a la Vista se debe tener configurada una cuenta contable para los intereses liquidados, en caso que el destino de ellos sea una CxP */
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = Liqui_Int.Cta_CauCr
                                     AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentas THEN DO:
                    MESSAGE "En la configuración de las cuentas para liquidación de intereses deben estar" SKIP
                            "registtadas las cuentas contables para el destino de los intereses causados (CxP)." SKIP
                            "para el producto" STRING(pro_ahorros.cod_ahorro,"99") + " - " + pro_ahorros.nom_producto + "." SKIP(2)
                            "Se cancela la operación hasta tanto se corrija la configuración contable para la" SKIP
                            "liquidación de intereses!"
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.
            END.

            IF pro_ahorros.tip_ahorro <> 1 AND pro_ahorros.cod_ahorro <> 3 /* Ahorro permanente */ THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = Liqui_Int.CtaCr_LiqAso
                                     AND Cuentas.Tipo = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentas THEN DO:
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "En la configuración de las cuentas para liquidación de intereses deben estar" SKIP
                                "registradas las cuentas contables para el destino de los intereses liquidados (CxP)." SKIP
                                "para el producto" STRING(pro_ahorros.cod_ahorro,"99") + " - " + pro_ahorros.nom_producto + "." SKIP(2)
                                "Se cancela la operación hasta tanto se corrija la configuración contable para la" SKIP
                                "liquidación de intereses!"
                            VIEW-AS ALERT-BOX ERROR.

                        RETURN ERROR.
                    END.

                END.
            END.

            FIND FIRST Operacion WHERE Operacion.Cod_Operac = Liqui_Int.Cod_Operacion NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(Operacion) THEN DO:
                MESSAGE "En Liqui_Int El Cod_Operación para la Liquidación de Intereses debe existir" SKIP
                        "para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            ASSIGN TempCtas.CtaCau = Liqui_Int.Cta_CauCr
                   TempCtas.CtaGto = Liqui_Int.CtaDb_LiqAso
                   TempCtas.CtaLiq = Liqui_Int.CtaCr_LiqAso
                   TempCtas.Oper = Liqui_Int.Cod_Operacion
                   TempCtas.CtaRF = Liqui_Int.CtaCr_Ret.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaCalendario W-Prc_LiqIntAhorro 
PROCEDURE ValidaCalendario :
FIND FIRST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                        AND Calendario.Ano = YEAR(W_Fecha)
                        AND Calendario.Mes >= MONTH(W_Fecha)
                        AND Calendario.Trimestre_Cierre NO-LOCK NO-ERROR.
IF AVAILABLE calendario THEN
    FecLiqtri = DATE(Calendario.Mes,Calendario.Dia,Calendario.Ano).
ELSE
    MESSAGE "No se ha configurado la fecha para el cierre de trimestre." SKIP
            "(Mensaje informativo)"
        VIEW-AS ALERT-BOX ERROR.


FIND LAST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                       AND Calendario.Ano = YEAR(W_Fecha)
                       AND Calendario.Mes = MONTH(W_Fecha)
                       AND Calendario.Dia > 0
                       AND Calendario.Cierre NO-LOCK NO-ERROR.
IF AVAILABLE calendario THEN
    FecLiqMes = DATE(Calendario.Mes,Calendario.Dia,Calendario.Ano).
ELSE
    MESSAGE "No se ha configurado la fecha para el cierre de mes." SKIP
            "(Mensaje informativo)"
        VIEW-AS ALERT-BOX ERROR.


FIND FIRST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                        AND Calendario.Ano = YEAR(W_Fecha)
                        AND Calendario.Mes = MONTH(W_Fecha)
                        AND Calendario.Dia = DAY(W_Fecha) NO-LOCK NO-ERROR.

ASSIGN W_RowidCal = ROWID(Calendario)
       ProxAno = YEAR(W_Fecha)
       ProxMes = MONTH(W_Fecha) + 1
       ProxATri = ProxAno
       ProxMTri = ProxMes + 2.

IF ProxMes = 13 THEN
    ASSIGN ProxMes = 1
           ProxMTri = 3
           ProxAno = YEAR(W_Fecha) + 1
           ProxATri = ProxAno.
ELSE
    IF ProxMTri > 12 THEN
        ASSIGN ProxMTri = ProxMTri - 12
               ProxATri = YEAR(W_Fecha) + 1.

IF Calendario.Cierre THEN DO:
    FIND LAST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                           AND Calendario.Ano = ProxAno
                           AND Calendario.Mes = ProxMes
                           AND Calendario.Dia > 0
                           AND Calendario.Cierre NO-LOCK NO-ERROR.
    IF AVAILABLE Calendario THEN
        FProxLiqM = DATE(ProxMes,Calendario.Dia,ProxAno).
    ELSE
        MESSAGE "No se ha configurado la fecha para el cierre del próximo mes." SKIP
                "(Mensaje informativo)"
            VIEW-AS ALERT-BOX ERROR.
END.


FIND FIRST Calendario WHERE ROWID(Calendario) = W_RowidCal NO-LOCK NO-ERROR.

IF Calendario.Trimestre_Cierre THEN DO:
    FIND LAST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                           AND Calendario.Ano = ProxATri
                           AND Calendario.Mes = ProxMTri
                           AND Calendario.Dia > 0
                           AND Calendario.Trimestre_Cierre NO-LOCK NO-ERROR.
    IF AVAILABLE Calendario THEN
        FProxLiqT = DATE(ProxMTri,Calendario.Dia,ProxATri).
    ELSE
        MESSAGE "No se ha configurado la fecha para el cierre del próximo trimestre." SKIP
                "(Mensaje informativo)"
            VIEW-AS ALERT-BOX ERROR.
END.

FIND FIRST Calendario WHERE ROWID(Calendario) = W_RowidCal NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

