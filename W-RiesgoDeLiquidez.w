&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME AGENCIAW-Integridad_PC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS AGENCIAW-Integridad_PC 
CREATE WIDGET-POOL.

DEFINE VARIABLE W_OfiIni AS INTEGER NO-UNDO.
DEFINE VARIABLE W_OfiFin AS INTEGER NO-UNDO.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
DEFI TEMP-TABLE TempCtas NO-UNDO
    /*FIELD TipP AS CHAR FORM "X(1)"*/

       /* oakley */

        FIELD ClaP   AS INTEG FORM "9"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
        FIELD NomPro AS CHAR FORM "X(20)"
        FIELD CtaCtable LIKE Cuentas.Cuenta
        FIELD CtaIng LIKE Cuentas.Cuenta        /*Para Ahorros Los Por Pagar*/
        FIELD CtaLiq LIKE Cuentas.Cuenta        /*Para Ahorros Los Causados*/
        FIELD IntAnt LIKE Cuentas.Cuenta        
        FIELD IntMor LIKE Cuentas.Cuenta
        FIELD MorIng LIKE Liqui_Int.CtaCr_MoraAso
        FIELD DifCoD LIKE Cuentas.Cuenta
        FIELD DifCoH LIKE Cuentas.Cuenta
        FIELD CtaPol LIKE Cuentas.Cuenta
        FIELD CtaHon LIKE Cuentas.Cuenta
        FIELD CtaCos LIKE Cuentas.Cuenta
        FIELD CtaGar LIKE CortoLargo.Cta_VigGarAd
        FIELD CtaCGa LIKE CortoLargo.Cta_ContrapartidaGar
        FIELD Calif  AS INTEG FORM "99"
        FIELD GL     AS INTEG FORM "9"
       FIELD  CtaProd LIKE Cuentas.Cuenta   COLUMN-LABEL "Cuenta_Producc."
        INDEX idx clap pto CtaCtable.  /*1 Ad con LIb, 2 Ad sin Lib, 3 NA con Lib, 4 Na sin Lib*/
DEF VAR htempctas AS HANDLE NO-UNDO.
DEFINE VARIABLE vcCtaProd AS CHARACTER INITIAL "" NO-UNDO. /*giocam Dic/10/07*/
htempctas = TEMP-TABLE tempctas:HANDLE.    

   DEFI TEMP-TABLE TCon_Sdos NO-UNDO
        FIELD Ag     LIKE Agencias.Agencia
        FIELD TipP   AS CHAR  FORM "X(1)" 
        FIELD ClaP   AS INTEG FORM "9"
        FIELD Cpto   AS CHAR  FORM "X(4)"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro     
        FIELD CtaCtable LIKE Cuentas.Cuenta 
        FIELD CtaProd LIKE Cuentas.Cuenta 
        FIELD NomPro AS CHAR FORM "X(20)"
        FIELD SdoPro LIKE Ahorros.Sdo_dispon INIT 0
        FIELD SdoCta LIKE Ahorros.Sdo_dispon INIT 0
        FIELD Calif  AS INTEG FORM "99"
        FIELD GL     AS INTEG FORM "9"
        INDEX idx ag tipp clap cpto pto.
DEF VAR htcon_sdos AS HANDLE NO-UNDO.
htcon_sdos = TEMP-TABLE tcon_sdos:HANDLE.    
    
   DEFI VAR Ag_Proc LIKE Agencias.Agencia NO-UNDO.
   DEFI VAR Ag_Nom  LIKE Agencias.Nombre NO-UNDO. 
    
   DEFI VAR W_Gtia  AS INTEG FORM "9" NO-UNDO.
   /*DEFI VAR W_TsdoK LIKE Creditos.Sdo_Capital INIT 0 EXTENT 4.*/
   DEFI VAR Tot_Pto LIKE Ahorros.Sdo_dispon   INIT 0 EXTENT 9 NO-UNDO.
   DEFI VAR Listado AS CHAR FORM "X(35)" NO-UNDO.
   
   DEFI VAR TotCtaX_Pdcto LIKE Ahorros.Sdo_Dispon INIT 0 NO-UNDO.
   DEFI VAR TotCtaX_Cta   LIKE Ahorros.Sdo_Dispon INIT 0 NO-UNDO.

DEF VAR iAgncia AS INTEGER NO-UNDO.
/*
/* para determinar las cuentas */
DEF TEMP-TABLE tcrtolrgo NO-UNDO
    FIELD cdgo_prdcto AS INTEGER
    FIELD cuenta AS CHAR
    FIELD nmnco AS CHAR
    FIELD orden AS INTEGER
    FIELD sldo AS DECIMAL
    INDEX cuenta cuenta.
DEF VAR iano AS INTEGER NO-UNDO.
DEF VAR imes AS INTEGER NO-UNDO.
DEF VAR cLstaNmncos AS CHAR NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO.    
cLstaNmncos = "SCAP,SHON,SPOL,SCOS,ICAU,IXPG,IMOR,IDIF".
FOR EACH cortolargo NO-LOCK
        WHERE
            cortolargo.agencia = 1
        AND cortolargo.clase_producto = 2
    BREAK 
        BY cortolargo.agencia
        BY cortolargo.clase_producto
        BY cortolargo.cod_producto:
    IF FIRST-OF(cortolargo.cod_producto) 
    THEN DO:
        FIND FIRST Liqui_Int NO-LOCK
            WHERE 
                Liqui_Int.Clase_Producto = cortolargo.clase_producto
            AND Liqui_Int.Cod_Producto   = CortoLargo.Cod_Producto NO-ERROR.
        DO i = 1 TO NUM-ENTRIES(cLstaNmncos,","):
            CREATE tcrtolrgo.
            ASSIGN  
                    tcrtolrgo.cdgo_prdcto = cortolargo.cod_producto
                    tcrtolrgo.cuenta = 
                        IF i = 1 
                        THEN cortolargo.cta_asoad 
                        ELSE 
                        IF i = 2 
                        THEN cortolargo.cta_honorariosdb
                        ELSE
                        IF i = 3
                        THEN cortolargo.cta_polizasdb
                        ELSE
                        IF i = 4
                        THEN cortolargo.cta_costasdb
                        ELSE
                        IF i = 5
                        THEN (IF AVAILABLE liqui_int THEN liqui_int.ctadb_liqaso ELSE "")
                        ELSE
                        IF i = 6
                        THEN (IF AVAILABLE liqui_int THEN liqui_int.ctaint_antaso ELSE "")
                        ELSE
                        IF i = 7
                        THEN (IF AVAILABLE liqui_int THEN liqui_int.ctadb_moraaso ELSE "")
                        ELSE (IF AVAILABLE liqui_int THEN liqui_int.ctadb_difcobaso ELSE "")
                        /* cLstaNmncos = "SCAP,SHON,SPOL,SCOS,ICAU,IXPG,IMOR,IDIF". */
                    tcrtolrgo.nmnco = ENTRY(i,cLstaNmncos,",")
                    tcrtolrgo.orden = i.
                                                                                                        
        END.
    END.
END.    /* FOR EACH cortolargo */
*/

DEFINE TEMP-TABLE errores
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cod_producto AS INTEGER
    FIELD cue_ahorros AS CHARACTER
    FIELD sdo_disponible AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD INT_causado AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD INT_pagar AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD tError AS CHARACTER.

DEFINE VAR totalCreditos AS DECIMAL.
DEFINE VAR totalcreditosN AS DECIMAL.
DEFINE VAR totalCreditosC AS DECIMAL.
DEFINE VAR totalIntCorriente AS DECIMAL.
DEFINE VAR totalIntCorrienteN AS DECIMAL.
DEFINE VAR totalIntCorrienteC AS DECIMAL.
DEFINE VAR totalCuentaCap AS DECIMAL.
DEFINE VAR totalCuentaCapN AS DECIMAL.
DEFINE VAR totalCuentaCapC AS DECIMAL.
DEFINE VAR totalCuentaintCorriente AS DECIMAL.
DEFINE VAR totalCuentaintCorrienteN AS DECIMAL.
DEFINE VAR totalCuentaintCorrienteC AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalIntMora AS DECIMAL.
DEFINE VAR totalIntMoraN AS DECIMAL.
DEFINE VAR totalIntMoraC AS DECIMAL.
DEFINE VAR totalCuentaintMora AS DECIMAL.
DEFINE VAR totalCuentaintMoraC AS DECIMAL.
DEFINE VAR totalCuentaintMoraN AS DECIMAL.

DEFINE VAR totalIntContingente AS DECIMAL.
DEFINE VAR totalIntContingenteN AS DECIMAL.
DEFINE VAR totalIntContingenteC AS DECIMAL.
DEFINE VAR totalCuentaintContingente AS DECIMAL.
DEFINE VAR totalCuentaintContingenteN AS DECIMAL.
DEFINE VAR totalCuentaintContingenteC AS DECIMAL.

DEFINE VAR totalProvisionCapital AS DECIMAL.
DEFINE VAR totalProvisionCapitalN AS DECIMAL.
DEFINE VAR totalProvisionCapitalC AS DECIMAL.
DEFINE VAR totalCuentaProvisionCapital AS DECIMAL.
DEFINE VAR totalCuentaProvisionCapitalN AS DECIMAL.
DEFINE VAR totalCuentaProvisionCapitalC AS DECIMAL.

DEFINE TEMP-TABLE totalesCreditos
    FIELD agencia AS INTEGER
    FIELD cod_credito AS INTEGER
    FIELD intCorriente AS DECIMAL
    FIELD intCorrienteN AS DECIMAL
    FIELD intCorrienteC AS DECIMAL
    FIELD intMora AS DECIMAL
    FIELD intMoraN AS DECIMAL
    FIELD intMoraC AS DECIMAL
    FIELD intContingente AS DECIMAL
    FIELD intContingenteN AS DECIMAL
    FIELD intContingenteC AS DECIMAL
    FIELD provisionCapital AS DECIMAL
    FIELD provisionCapitalN AS DECIMAL
    FIELD provisionCapitalC AS DECIMAL
    FIELD provisionInteres AS DECIMAL.

DEFINE TEMP-TABLE ttCuentas
    FIELD cuenta AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Integ

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_CmbOfi Btn_Imp 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCuentasDeAhorros AGENCIAW-Integridad_PC 
FUNCTION fCuentasDeAhorros RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSldoCuenta AGENCIAW-Integridad_PC 
FUNCTION fSldoCuenta RETURNS CHARACTER
  (iagncia AS integer,ccuenta AS CHAR,iano AS integer,imes AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR AGENCIAW-Integridad_PC AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 9.72 BY 1.77.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     DROP-DOWN-LIST
     SIZE 43.14 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Integ
     W_CmbOfi AT ROW 1.54 COL 9 COLON-ALIGNED
     Btn_Imp AT ROW 2.73 COL 44.14
     Msaje AT ROW 4.77 COL 3 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 54.43 BY 5.62
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Seleccione la Agencia para el Control de Integridad".


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
  CREATE WINDOW AGENCIAW-Integridad_PC ASSIGN
         HIDDEN             = YES
         TITLE              = "Integridad Productos Vs Cuentas, Programa W-Integridad_PC.W"
         HEIGHT             = 5.62
         WIDTH              = 54.43
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
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
/* SETTINGS FOR WINDOW AGENCIAW-Integridad_PC
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Integ
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Integ
   NO-ENABLE ALIGN-L 2                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(AGENCIAW-Integridad_PC)
THEN AGENCIAW-Integridad_PC:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Integ
/* Query rebuild information for FRAME F_Integ
     _Query            is NOT OPENED
*/  /* FRAME F_Integ */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME AGENCIAW-Integridad_PC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AGENCIAW-Integridad_PC AGENCIAW-Integridad_PC
ON END-ERROR OF AGENCIAW-Integridad_PC /* Integridad Productos Vs Cuentas, Programa W-Integridad_PC.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AGENCIAW-Integridad_PC AGENCIAW-Integridad_PC
ON WINDOW-CLOSE OF AGENCIAW-Integridad_PC /* Integridad Productos Vs Cuentas, Programa W-Integridad_PC.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp AGENCIAW-Integridad_PC
ON CHOOSE OF Btn_Imp IN FRAME F_Integ /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_PathSpl + "-InfInteg-" + W_Usuario.

    {INCLUIDO\Imprimir.I "Listado"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi AGENCIAW-Integridad_PC
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Integ /* Agencia */
DO:
    IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
        W_OfiIni = 0.
        W_OfiFin = 999.
    END.
    ELSE
        ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
               W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK AGENCIAW-Integridad_PC 


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

    W_CmbOfi:ADD-LAST("000 - CONSOLIDADO").

    ASSIGN W_OfiIni = W_Agencia
           W_OfiFin = W_Agencia.

    FOR EACH Agencias WHERE Agencias.Estado NE 3
                        AND Agencias.Agencia GT 0 NO-LOCK:
        W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + STRING(Agencias.Nombre,"X(25)")).
    END.

    FIND FIRST agencias WHERE agencias.agencia = w_agencia NO-LOCK NO-ERROR.
    IF AVAILABLE agencias THEN
        W_CmbOfi:SCREEN-VALUE = STRING(agencias.agencia,"999") + " - " + STRING(Agencias.Nombre,"X(25)").

    APPLY "VALUE-CHANGED" TO W_CmbOfi.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ahorros AGENCIAW-Integridad_PC 
PROCEDURE Ahorros :
DEFINE VAR totalAhorros AS DECIMAL.
DEFINE VAR totalIntCausado AS DECIMAL.
DEFINE VAR totalIntPagar AS DECIMAL.
DEFINE VAR totalCuentaCap AS DECIMAL.
DEFINE VAR totalCuentaCau AS DECIMAL.
DEFINE VAR totalCuentaInt AS DECIMAL.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE errores.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                             AHORROS                      " SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK:
    totalAhorros = 0.
    totalIntCausado = 0.
    totalIntPagar = 0.
    totalCuentaCap = 0.
    totalCuentaCau = 0.
    totalCuentaInt = 0.

    /* 1. Sumo el saldo de ahorros */
    FOR EACH ahorros WHERE ahorros.agencia >= w_ofiIni
                       AND ahorros.agencia <= W_OfiFin
                       AND ahorros.cod_ahorro = pro_ahorros.cod_ahorro
                       AND ahorros.estado = 1 NO-LOCK:
        totalAhorros = totalAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
        totalIntCausado = totalIntCausado + ahorros.INT_causado.
        totalIntPagar = totalIntPagar + ahorros.INT_pagar.

        IF ahorros.sdo_disponible < 0 THEN DO:
            CREATE errores.
            ASSIGN errores.agencia = ahorros.agencia
                   errores.nit = ahorros.nit
                   errores.cod_producto = ahorros.cod_ahorro
                   errores.cue_ahorros = ahorros.cue_ahorros
                   errores.sdo_disponible = ahorros.sdo_disponible
                   errores.tError = "Producto con saldo negativo".
        END.
    END.

    FOR EACH ahorros WHERE ahorros.agencia >= w_ofiIni
                       AND ahorros.agencia <= W_OfiFin
                       AND ahorros.cod_ahorro = pro_ahorros.cod_ahorro
                       AND ahorros.estado <> 1
                       AND (ahorros.sdo_disponible <> 0 OR
                            ahorros.INT_causado <> 0 OR
                            ahorros.INT_pagar <> 0) NO-LOCK:
        totalAhorros = totalAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
        totalIntCausado = totalIntCausado + ahorros.INT_causado.
        totalIntPagar = totalIntPagar + ahorros.INT_pagar.

        CREATE errores.
        ASSIGN errores.agencia = ahorros.agencia
               errores.nit = ahorros.nit
               errores.cod_producto = ahorros.cod_ahorro
               errores.cue_ahorros = ahorros.cue_ahorros
               errores.sdo_disponible = ahorros.sdo_disponible
               errores.INT_causado = ahorros.INT_causado
               errores.INT_pagar = ahorros.INT_pagar
               errores.tError = "Producto inactivo con saldo".
    END.

    /* Reviso la configuracion para capturar la cuenta contable */
    FIND FIRST cortoLargo WHERE cortoLargo.agencia = 1
                            AND cortoLargo.clase_producto = 1 
                            AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
    IF AVAILABLE cortoLargo THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = cortoLargo.Cta_AsoAd
                              AND sal_cuenta.ano = YEAR(w_fecha)
                              AND sal_cuenta.agencia >= w_OfiIni
                              AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
            totalCuentaCap = totalCuentaCap + sal_cuenta.sal_inicial.

            FIND FIRST cuentas WHERE cuentas.cuenta = cortoLargo.cta_AsoAd NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cuentas THEN DO:
                MESSAGE "La cuenta contable configurada para el producto" pro_ahorros.nom_producto "(" cortoLargo.cta_AsoAd ") no existe en el Plan de Cuentas. Revise por favor."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.

            DO cont = 1 TO MONTH(w_fecha):
                IF cuentas.naturaleza = "DB" THEN
                    totalCuentaCap = totalCuentaCap + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    totalCuentaCap = totalCuentaCap - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.
    END.

    DISPLAY cortoLargo.Cta_AsoAd + " - " + STRING(pro_ahorros.cod_ahorro,"99") + "-" + pro_ahorros.nom_producto FORMAT "X(34)" AT 1
            totalAhorros FORMAT "$->>,>>>,>>>,>>9.99" AT 36
            totalCuentaCap FORMAT "$->>,>>>,>>>,>>9.99" AT 60
            totalAhorros - totalCuentaCap FORMAT "$->>,>>>,>>>,>>9.99" AT 86
        WITH DOWN WIDTH 170 FRAME framAhorros NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    
    FIND FIRST liqui_int WHERE liqui_int.clase_producto = 1
                           AND liqui_int.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
    IF AVAILABLE liqui_int THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = liqui_int.cta_cauCr
                              AND sal_cuenta.ano = YEAR(w_fecha)
                              AND sal_cuenta.agencia >= w_OfiIni
                              AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
            totalCuentaCau = totalCuentaCau + sal_cuenta.sal_inicial.

            FIND FIRST cuentas WHERE cuentas.cuenta = liqui_int.cta_cauCr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cuentas THEN DO:
                MESSAGE "La cuenta contable configurada para el producto" pro_ahorros.nom_producto "(" liqui_int.cta_cauCr ") no existe en el Plan de Cuentas. Revise por favor."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.

            DO cont = 1 TO MONTH(w_fecha):
                IF cuentas.naturaleza = "DB" THEN
                    totalCuentaCau = totalCuentaCau + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    totalCuentaCau = totalCuentaCau - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.

        FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = liqui_int.ctaCr_LiqAso
                              AND sal_cuenta.ano = YEAR(w_fecha)
                              AND sal_cuenta.agencia >= w_OfiIni
                              AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
            totalCuentaInt = totalCuentaInt + sal_cuenta.sal_inicial.

            FIND FIRST cuentas WHERE cuentas.cuenta = liqui_int.ctaCr_LiqAso NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cuentas THEN DO:
                MESSAGE "La cuenta contable configurada para el producto" pro_ahorros.nom_producto "(" liqui_int.ctaCr_LiqAso ") no existe en el Plan de Cuentas. Revise por favor."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.

            DO cont = 1 TO MONTH(w_fecha):
                IF cuentas.naturaleza = "DB" THEN
                    totalCuentaInt = totalCuentaInt + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    totalCuentaInt = totalCuentaInt - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.
    END.

    IF AVAILABLE liqui_int THEN DO:
        DISPLAY liqui_int.cta_cauCr + " - " + STRING(pro_ahorros.cod_ahorro,"99") + "-Interés Causado" FORMAT "X(34)" AT 1
                totalIntCausado FORMAT "$->>,>>>,>>>,>>9.99" AT 36
                totalCuentaCau FORMAT "$->>,>>>,>>>,>>9.99" AT 60
                totalIntCausado - totalCuentaCau FORMAT "$->>,>>>,>>>,>>9.99" AT 86
            WITH DOWN WIDTH 170 FRAME frmInteres NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        DISPLAY liqui_int.ctaCr_LiqAso + " - " + STRING(pro_ahorros.cod_ahorro,"99") + "-Interés x Pagar" FORMAT "X(34)" AT 1
                totalIntPagar FORMAT "$->>,>>>,>>>,>>9.99" AT 36
                totalCuentaInt FORMAT "$->>,>>>,>>>,>>9.99" AT 60
                totalIntPagar - totalCuentaInt FORMAT "$->>,>>>,>>>,>>9.99" AT 86
                SKIP(1)
            WITH DOWN WIDTH 170 FRAME frmInteres NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    END.
    ELSE
        DISPLAY SKIP.

    IF AVAILABLE errores THEN DO:
        DISPLAY "AG NIT        PRO CUENTA      SDO_DISPONIBLE     INT_CAUSADO       INT_PAGAR ERROR" SKIP
                "-- ---------- --- ---------- --------------- --------------- --------------- ---------------------------" SKIP
            WITH DOWN WIDTH 170 FRAME frmErrores NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        FOR EACH errores NO-LOCK:
            DISPLAY errores.agencia FORMAT "99"
                    errores.nit FORMAT "X(10)"
                    errores.cod_producto FORMAT "999"
                    errores.cue_ahorros FORMAT "X(10)"
                    errores.sdo_disponible
                    errores.INT_Causado
                    errores.INT_pagar
                    errores.tError FORMAT "X(36)"
                WITH DOWN WIDTH 170 FRAME frmErrores NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.

        DISPLAY SKIP.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Creditos AGENCIAW-Integridad_PC 
PROCEDURE Creditos :
DEFINE VAR flagDistintas AS LOGICAL.

EMPTY TEMP-TABLE errores.
EMPTY TEMP-TABLE totalesCreditos.
EMPTY TEMP-TABLE ttCuentas.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                             CREDITOS                     " SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    totalCreditos = 0.
    totalCreditosC = 0.
    totalCreditosN = 0.
    totalCuentaCap = 0.
    totalCuentaCapN = 0.
    totalCuentaCapC = 0.
    totalIntMora = 0.
    totalIntMoraN = 0.
    totalIntMoraC = 0.
    flagDistintas = FALSE.

    CREATE totalesCreditos.
    ASSIGN totalesCreditos.cod_credito = pro_creditos.cod_credito.
    
    /* Saldo de Capital */
    FIND FIRST cortoLargo WHERE cortoLargo.agencia = 1
                            AND cortoLargo.clase_producto = 2
                            AND cortoLargo.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
    
    /* 1. Sumo el saldo de creditos */
    FOR EACH creditos WHERE creditos.agencia >= w_ofiIni
                        AND creditos.agencia <= W_OfiFin
                        AND creditos.cod_credito = pro_creditos.cod_credito
                        AND (creditos.estado = 2 OR (creditos.estado <> 2 AND (creditos.sdo_capital <> 0 OR creditos.INT_corriente <> 0))) NO-LOCK:
        FIND FIRST totalesCreditos WHERE totalesCreditos.cod_credito = creditos.cod_credito NO-ERROR.

        CASE creditos.FOR_pago:
            WHEN 1 THEN DO:
                totalCreditosC = totalCreditosC + creditos.sdo_capital.
                totalesCreditos.intCorrienteC = totalesCreditos.intCorrienteC + creditos.INT_corriente.
                totalesCreditos.intContingenteC = totalesCreditos.intContingenteC + creditos.INT_difCobro.
                totalesCreditos.intMoraC = totalesCreditos.intMoraC + creditos.INT_morCobrar.
                totalesCreditos.provisionCapitalC = totalesCreditos.provisionCapitalC + creditos.provision.
            END.

            WHEN 2 THEN DO:
                totalCreditosN = totalCreditosN + creditos.sdo_capital.
                totalesCreditos.intCorrienteN = totalesCreditos.intCorrienteN + creditos.INT_corriente.
                totalesCreditos.intContingenteN = totalesCreditos.intContingenteN + creditos.INT_difCobro.
                totalesCreditos.intMoraN = totalesCreditos.intMoraN + creditos.INT_morCobrar.
                totalesCreditos.provisionCapitalN = totalesCreditos.provisionCapitalN + creditos.provision.
            END.
        END.

        totalCreditos = totalCreditos + creditos.sdo_capital.
        totalesCreditos.intCorriente = totalesCreditos.intCorriente + creditos.INT_corriente.
        totalesCreditos.intContingente = totalesCreditos.intContingente + creditos.INT_difCobro.
        totalesCreditos.intMora = totalesCreditos.intMora + creditos.INT_morCobrar.
        totalesCreditos.provisionCapital = totalescreditos.provisionCapital + creditos.provision.
        totalesCreditos.provisionInteres = totalesCreditos.provisionInteres + creditos.provision_interes.


        IF creditos.sdo_capital < 0 THEN DO:
            CREATE errores.
            ASSIGN errores.agencia = creditos.agencia
                   errores.nit = creditos.nit
                   errores.cod_producto = creditos.cod_credito
                   errores.cue_ahorros = STRING(creditos.num_credito)
                   errores.sdo_disponible = creditos.sdo_capital
                   errores.tError = "Producto con saldo negativo".
        END.

        IF creditos.estado <> 2 AND (creditos.sdo_capital <> 0 OR creditos.INT_corriente <> 0) THEN DO:
            CREATE errores.
            ASSIGN errores.agencia = creditos.agencia
                   errores.nit = creditos.nit
                   errores.cod_producto = creditos.cod_credito
                   errores.cue_ahorros = STRING(creditos.num_credito)
                   errores.sdo_disponible = creditos.sdo_capital
                   errores.INT_causado = creditos.INT_corriente + creditos.INT_difCobro
                   errores.INT_pagar = creditos.INT_morCobrar + creditos.INT_moraDifCob
                   errores.tError = "Producto inactivo con saldo".
        END.
    END.
    /* ---------------------------- */

    FOR EACH carteraVencida WHERE carteraVencida.cod_producto = pro_creditos.cod_credito NO-LOCK:
        IF carteraVencida.cta_AsoAddb = carteraVencida.cta_Noaaddb THEN DO:
            FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_AsoAddb NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCuentas THEN DO:
                CREATE ttCuentas.
                ttCuentas.cuenta = carteraVencida.cta_AsoAddb.

                FOR EACH sal_cuenta WHERE (sal_cuenta.cuenta = carteraVencida.cta_AsoAddb OR sal_cuenta.cuenta = carteraVencida.cta_Noaaddb)
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                    totalCuentaCap = totalCuentaCap + sal_cuenta.sal_inicial.
    
                    FIND FIRST cuentas WHERE cuentas.cuenta = carteraVencida.cta_AsoAddb NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" carteraVencida.cta_AsoAddb ") no existe en el Plan de Cuentas. Revise por favor."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                        RETURN NO-APPLY.
                    END.
    
                    DO cont = 1 TO MONTH(w_fecha):
                        IF cuentas.naturaleza = "DB" THEN
                            totalCuentaCap = totalCuentaCap + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                        ELSE
                            totalCuentaCap = totalCuentaCap - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                    END.
                END.
            END.
        END.
        ELSE DO:
            flagDistintas = TRUE.

            FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_AsoAddb NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCuentas THEN DO:
                CREATE ttCuentas.
                ttCuentas.cuenta = carteraVencida.cta_AsoAddb.

                FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = carteraVencida.cta_AsoAddb
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                    totalCuentaCapN = totalCuentaCapN + sal_cuenta.sal_inicial.
    
                    FIND FIRST cuentas WHERE cuentas.cuenta = carteraVencida.cta_AsoAddb NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" carteraVencida.cta_AsoAddb ") no existe en el Plan de Cuentas. Revise por favor."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                        RETURN NO-APPLY.
                    END.

                    DO cont = 1 TO MONTH(w_fecha):
                        IF cuentas.naturaleza = "DB" THEN
                            totalCuentaCapN = totalCuentaCapN + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                        ELSE
                            totalCuentaCapN = totalCuentaCapN - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                    END.
                END.
            END.

            FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_Noaaddb NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCuentas THEN DO:
                CREATE ttCuentas.
                ttCuentas.cuenta = carteraVencida.cta_Noaaddb.

                FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = carteraVencida.cta_Noaaddb
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                    totalCuentaCapC = totalCuentaCapC + sal_cuenta.sal_inicial.
    
                    FIND FIRST cuentas WHERE cuentas.cuenta = carteraVencida.cta_Noaaddb NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" carteraVencida.cta_Noaaddb ") no existe en el Plan de Cuentas. Revise por favor."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                        RETURN NO-APPLY.
                    END.
    
                    DO cont = 1 TO MONTH(w_fecha):
                        IF cuentas.naturaleza = "DB" THEN
                            totalCuentaCapC = totalCuentaCapC + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                        ELSE
                            totalCuentaCapC = totalCuentaCapC - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                    END.
                END.
            END.
        END.
    END.

    IF flagDistintas = FALSE THEN DO:
        DISPLAY STRING(cortoLargo.Cta_AsoAd,"X(8)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                totalCreditos FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                totalCuentaCap FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                totalCreditos - totalCuentaCap FORMAT "$->>>,>>>,>>>,>>9.99" AT 85 SKIP(2)
            WITH DOWN WIDTH 170 FRAME frmCreditos1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    END.
    ELSE DO:
        DISPLAY STRING(cortoLargo.Cta_AsoAd,"X(8)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                totalCreditosN FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                totalCuentaCapN FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                totalCreditosN - totalCuentaCapN FORMAT "$->>>,>>>,>>>,>>9.99" AT 85
                STRING(cortoLargo.Cta_NoaAd,"X(8)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                totalCreditosC FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                totalCuentaCapC FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                totalCreditosC - totalCuentaCapC FORMAT "$->>>,>>>,>>>,>>9.99" AT 85
                "                                                                                              ----------"
                "                                                                TOTAL DIFERENCIA -->" (totalCreditosN - totalCuentaCapN) + (totalCreditosC - totalCuentaCapC) FORMAT "->>>,>>>,>>>,>>9.99" SKIP(2)
            WITH DOWN WIDTH 170 FRAME frmCreditos2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    END.

    IF AVAILABLE errores THEN DO:
        DISPLAY SKIP(2)
                "AG NIT        PRO CUENTA      SDO_DISPONIBLE     INT_CAUSADO       INT_PAGAR ERROR" SKIP
                "-- ---------- --- ---------- --------------- --------------- --------------- ---------------------------" SKIP
            WITH DOWN WIDTH 170 FRAME frmErrores NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        FOR EACH errores NO-LOCK:
            DISPLAY errores.agencia FORMAT "99"
                    errores.nit FORMAT "X(10)"
                    errores.cod_producto FORMAT "999"
                    errores.cue_ahorros FORMAT "X(10)"
                    errores.sdo_disponible
                    errores.INT_Causado
                    errores.INT_pagar
                    errores.tError FORMAT "X(36)" SKIP
                WITH DOWN WIDTH 170 FRAME frmErrores NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.

        DISPLAY SKIP.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI AGENCIAW-Integridad_PC  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(AGENCIAW-Integridad_PC)
  THEN DELETE WIDGET AGENCIAW-Integridad_PC.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI AGENCIAW-Integridad_PC  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi Msaje 
      WITH FRAME F_Integ IN WINDOW AGENCIAW-Integridad_PC.
  ENABLE W_CmbOfi Btn_Imp 
      WITH FRAME F_Integ IN WINDOW AGENCIAW-Integridad_PC.
  {&OPEN-BROWSERS-IN-QUERY-F_Integ}
  VIEW AGENCIAW-Integridad_PC.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InteresContingente AGENCIAW-Integridad_PC 
PROCEDURE InteresContingente :
DEFINE VAR flagDistintas AS LOGICAL.
DEFINE VAR sumaProducto AS DECIMAL.
DEFINE VAR cuentaUnica AS CHARACTER.

EMPTY TEMP-TABLE ttcuentas.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                               INTERÉS DIFÍCIL COBRO      " SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    totalCuentaintContingente = 0.
    totalCuentaintContingenteN = 0.
    totalCuentaintContingenteC = 0.
    flagDistintas = FALSE.

    FIND FIRST totalesCreditos WHERE totalesCreditos.cod_credito = pro_creditos.cod_credito NO-LOCK NO-ERROR.

    FIND FIRST liqui_int WHERE liqui_int.clase_producto = 2
                           AND liqui_int.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
    IF AVAILABLE liqui_int THEN DO:
        cuentaUnica = Liqui_Int.CtaDb_DifCobAso.

        FIND FIRST ttcuentas WHERE ttcuentas.cuenta = Liqui_Int.CtaDb_DifCob NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCuentas THEN DO:
            CREATE ttcuentas.
            ttcuentas.cuenta = Liqui_Int.CtaDb_DifCob.
        END.

        FIND FIRST ttcuentas WHERE ttcuentas.cuenta = Liqui_Int.CtaDb_DifCobAso NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCuentas THEN DO:
            CREATE ttcuentas.
            ttcuentas.cuenta = Liqui_Int.CtaDb_DifCobAso.
        END.

        DISPLAY STRING(Liqui_Int.CtaDb_DifCobAso,"X(10)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                totalesCreditos.intContingenteN FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                STRING(Liqui_Int.CtaDb_DifCob,"X(10)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                totalesCreditos.intContingenteC FORMAT "$->>>,>>>,>>>,>>9.99" AT 35 SKIP
            WITH DOWN WIDTH 170 FRAME frmCreditos2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        sumaProducto = sumaProducto + totalesCreditos.intContingenteN + totalesCreditos.intContingenteC.
    END.
END.

FOR EACH ttCuentas NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = ttCuentas.cuenta NO-LOCK NO-ERROR.

    FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = ttcuentas.cuenta
                          AND sal_cuenta.ano = YEAR(w_fecha)
                          AND sal_cuenta.agencia >= w_OfiIni
                          AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
        totalCuentaintContingente = totalCuentaintContingente + sal_cuenta.sal_inicial.

        FIND FIRST cuentas WHERE cuentas.cuenta = Liqui_Int.CtaDb_DifCobAso NO-LOCK NO-ERROR.
        
        DO cont = 1 TO MONTH(w_fecha):
            IF cuentas.naturaleza = "DB" THEN
                totalCuentaintContingente = totalCuentaintContingente + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
            ELSE
                totalCuentaintContingente = totalCuentaintContingente - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
        END.
    END.
END.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "SdoBalance" + " - " + cuentas.nombre FORMAT "X(33)" AT 1
        sumaProducto FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
        totalCuentaIntContingente FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
        sumaProducto - totalCuentaIntContingente FORMAT "$->>>,>>>,>>>,>>9.99" AT 85 SKIP
    WITH DOWN WIDTH 170 FRAME frmIntContingente2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE interesCorriente AGENCIAW-Integridad_PC 
PROCEDURE interesCorriente :
DEFINE VAR flagDistintas AS LOGICAL.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                               INTERÉS CORRIENTE          " SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    totalIntCorriente = 0.
    totalIntCorrienteN = 0.
    totalIntCorrienteC = 0.
    totalCuentaintCorriente = 0.
    totalCuentaintCorrienteN = 0.
    totalCuentaintCorrienteC = 0.
    flagDistintas = FALSE.

    /* Interés corriente */
    FIND FIRST totalesCreditos WHERE totalesCreditos.cod_credito = pro_creditos.cod_credito NO-LOCK NO-ERROR.

    FIND FIRST liqui_int WHERE liqui_int.clase_producto = 2
                           AND liqui_int.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.

    FOR EACH carteraVencida WHERE carteraVencida.cod_producto = pro_creditos.cod_credito NO-LOCK BREAK BY CarteraVencida.CtaCal_Interes:
        IF FIRST-OF(CarteraVencida.CtaCal_Interes) THEN DO:
            FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = CarteraVencida.CtaCal_Interes
                                  AND sal_cuenta.ano = YEAR(w_fecha)
                                  AND sal_cuenta.agencia >= w_OfiIni
                                  AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                totalCuentaIntCorriente = totalCuentaIntCorriente + sal_cuenta.sal_inicial.

                FIND FIRST cuentas WHERE cuentas.cuenta = CarteraVencida.CtaCal_Interes NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentas THEN DO:
                    MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" liqui_Int.ctadb_liqAso ") no existe en el Plan de Cuentas. Revise por favor."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN NO-APPLY.
                END.

                DO cont = 1 TO MONTH(w_fecha):
                    IF cuentas.naturaleza = "DB" THEN
                        totalCuentaIntCorriente = totalCuentaIntCorriente + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                    ELSE
                        totalCuentaIntCorriente = totalCuentaIntCorriente - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                END.
            END.
        END.
    END.

    DISPLAY STRING(liqui_Int.ctadb_liqAso,"X(10)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
            totalesCreditos.intCorriente FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
            totalCuentaIntCorriente FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
            totalesCreditos.intCorriente - totalCuentaIntCorriente FORMAT "$->>>,>>>,>>>,>>9.99" AT 85 SKIP
        WITH DOWN WIDTH 170 FRAME frmIntCorriente1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE interesMora AGENCIAW-Integridad_PC 
PROCEDURE interesMora :
DEFINE VAR flagDistintas AS LOGICAL.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                               INTERÉS DE MORA" SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    totalIntMora = 0.
    totalIntMoraN = 0.
    totalIntMoraC = 0.
    totalCuentaintMora = 0.
    totalCuentaintMoraN = 0.
    totalCuentaIntMoraC = 0.
    flagDistintas = FALSE.

    /* Interés de Mora */
    FIND FIRST totalesCreditos WHERE totalesCreditos.cod_credito = pro_creditos.cod_credito NO-LOCK NO-ERROR.

    FIND FIRST liqui_int WHERE liqui_int.clase_producto = 2
                           AND liqui_int.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
    IF AVAILABLE liqui_int THEN DO:
        IF liqui_int.ctaDb_Mora = liqui_int.ctaDb_MoraAso THEN DO:
            FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = liqui_int.ctaDb_MoraAso
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                totalCuentaIntMora = totalCuentaIntMora + sal_cuenta.sal_inicial.

                FIND FIRST cuentas WHERE cuentas.cuenta = liqui_int.ctaDb_MoraAso NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentas THEN DO:
                    MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" liqui_Int.ctadb_MoraAso ") no existe en el Plan de Cuentas. Revise por favor."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN NO-APPLY.
                END.

                DO cont = 1 TO MONTH(w_fecha):
                    IF cuentas.naturaleza = "DB" THEN
                        totalCuentaIntMora = totalCuentaIntMora + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                    ELSE
                        totalCuentaIntMora = totalCuentaIntMora - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                END.
            END.
        END.
        ELSE DO:
            flagDistintas = TRUE.

            FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = liqui_int.ctaDb_MoraAso
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                totalCuentaIntMoraN = totalCuentaIntMoraN + sal_cuenta.sal_inicial.

                FIND FIRST cuentas WHERE cuentas.cuenta = liqui_int.ctaDb_MoraAso NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentas THEN DO:
                    MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" liqui_Int.ctadb_MoraAso ") no existe en el Plan de Cuentas. Revise por favor."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN NO-APPLY.
                END.

                DO cont = 1 TO MONTH(w_fecha):
                    IF cuentas.naturaleza = "DB" THEN
                        totalCuentaIntMoraN = totalCuentaIntMoraN + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                    ELSE
                        totalCuentaIntMoraN = totalCuentaIntMoraN - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                END.
            END.

            FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = liqui_int.ctaDb_Mora
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                totalCuentaIntMoraC = totalCuentaIntMoraC + sal_cuenta.sal_inicial.

                FIND FIRST cuentas WHERE cuentas.cuenta = liqui_int.ctaDb_Mora NO-LOCK NO-ERROR.
                IF NOT AVAILABLE cuentas THEN DO:
                    MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" liqui_Int.ctadb_Mora ") no existe en el Plan de Cuentas. Revise por favor."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN NO-APPLY.
                END.

                DO cont = 1 TO MONTH(w_fecha):
                    IF cuentas.naturaleza = "DB" THEN
                        totalCuentaIntMoraC = totalCuentaIntMoraC + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                    ELSE
                        totalCuentaIntMoraC = totalCuentaIntMoraC - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                END.
            END.
        END.

        IF flagDistintas = FALSE THEN
            DISPLAY STRING(liqui_int.ctaDb_MoraAso,"X(10)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                    totalesCreditos.intMora FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                    totalCuentaIntMora FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                    totalesCreditos.intMora - totalCuentaIntMora FORMAT "$->>>,>>>,>>>,>>9.99" AT 85 SKIP(2)
                WITH DOWN WIDTH 170 FRAME frmCreditos1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        ELSE
            DISPLAY STRING(liqui_int.ctaDb_MoraAso,"X(10)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                    totalesCreditos.intMoraN FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                    totalCuentaIntMoraN FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                    totalesCreditos.intMoraN - totalCuentaIntMoraN FORMAT "$->>>,>>>,>>>,>>9.99" AT 85
                    STRING(liqui_int.ctaDb_Mora,"X(10)") + " - " + STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
                    totalesCreditos.intMoraC FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                    totalCuentaIntMoraC FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                    totalesCreditos.intMoraC - totalCuentaIntMoraC FORMAT "$->>>,>>>,>>>,>>9.99" AT 85
                    "                                                                                              ----------"
                    "                                                                TOTAL DIFERENCIA -->" (totalesCreditos.intMoraN - totalCuentaIntMoraN) + (totalesCreditos.intMoraC - totalCuentaIntMoraC) FORMAT "->>>,>>>,>>>,>>9.99" SKIP(2)
                WITH DOWN WIDTH 170 FRAME frmCreditos2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir AGENCIAW-Integridad_PC 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.I}

DISPLAY STRING(W_Nom_Entidad,"X(40)") FORMAT "X(100)" SKIP
        "INFORME DE INTEGRIDAD - PRODUCTOS Vs. CONTABILIDAD" SKIP
        "Fecha:" STRING(W_fecha,"99/99/9999") FORM "X(80)" SKIP
        "Agencia:" W_CmbOfi:SCREEN-VALUE IN FRAME F_Integ FORMAT "X(80)" SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP(1)
    WITH WIDTH 150 NO-LABELS.

RUN Ahorros.
RUN Creditos.
RUN InteresCorriente.
RUN interesMora.
RUN InteresContingente.
RUN Provision.
RUN ProvisionInteres.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Provision AGENCIAW-Integridad_PC 
PROCEDURE Provision :
DEFINE VAR flagDistintas AS LOGICAL.
DEFINE VAR totalDiferencia AS DECIMAL.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                                PROVISIÓN CAPITAL           " SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    totalProvisionCapital = 0.
    totalProvisionCapitalC = 0.
    totalCuentaProvisionCapital = 0.
    totalCuentaProvisionCapitalN = 0.
    totalCuentaProvisionCapitalC = 0.
    flagDistintas = FALSE.

    /* Provisión Capital */
    FIND FIRST totalesCreditos WHERE totalesCreditos.cod_credito = pro_creditos.cod_credito NO-LOCK NO-ERROR.

    FOR EACH carteraVencida WHERE carteraVencida.cod_producto = pro_creditos.cod_credito NO-LOCK:
        IF carteraVencida.cta_AsoPrvAdCr = carteraVencida.cta_NoaPrvAdCr THEN DO:
            FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_AsoPrvAdCr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCuentas THEN DO:
                CREATE ttCuentas.
                ttCuentas.cuenta = carteraVencida.cta_AsoPrvAdCr.

                FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = carteraVencida.cta_AsoPrvAdCr
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                    totalCuentaProvisionCapital = totalCuentaProvisionCapital + sal_cuenta.sal_inicial.

                    FIND FIRST cuentas WHERE cuentas.cuenta = carteraVencida.cta_AsoPrvAdCr NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" carteraVencida.cta_AsoPrvAdCr ") no existe en el Plan de Cuentas. Revise por favor."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                        RETURN NO-APPLY.
                    END.
    
                    DO cont = 1 TO MONTH(w_fecha):
                        IF cuentas.naturaleza = "DB" THEN
                            totalCuentaProvisionCapital = totalCuentaProvisionCapital + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                        ELSE
                            totalCuentaProvisionCapital = totalCuentaProvisionCapital - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                    END.
                END.
            END.
        END.
        ELSE DO:
            flagDistintas = TRUE.

            FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_AsoPrvAdCr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCuentas THEN DO:
                CREATE ttCuentas.
                ttCuentas.cuenta = carteraVencida.cta_AsoPrvAdCr.

                FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = carteraVencida.cta_AsoPrvAdCr
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                    totalCuentaProvisionCapitalN = totalCuentaProvisionCapitalN + sal_cuenta.sal_inicial.
    
                    FIND FIRST cuentas WHERE cuentas.cuenta = carteraVencida.cta_AsoPrvAdCr NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" carteraVencida.cta_AsoPrvAdCr ") no existe en el Plan de Cuentas. Revise por favor."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                        RETURN NO-APPLY.
                    END.

                    DO cont = 1 TO MONTH(w_fecha):
                        IF cuentas.naturaleza = "DB" THEN
                            totalCuentaProvisionCapitalN = totalCuentaProvisionCapitalN + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                        ELSE
                            totalCuentaProvisionCapitalN = totalCuentaProvisionCapitalN - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                    END.
                END.
            END.

            FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_NoaPrvAdCr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCuentas THEN DO:
                CREATE ttCuentas.
                ttCuentas.cuenta = carteraVencida.cta_NoaPrvAdCr.

                FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = carteraVencida.cta_NoaPrvAdCr
                                      AND sal_cuenta.ano = YEAR(w_fecha)
                                      AND sal_cuenta.agencia >= w_OfiIni
                                      AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
                    totalCuentaProvisionCapitalC = totalCuentaProvisionCapitalC + sal_cuenta.sal_inicial.
    
                    FIND FIRST cuentas WHERE cuentas.cuenta = carteraVencida.cta_NoaPrvAdCr NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE cuentas THEN DO:
                        MESSAGE "La cuenta contable configurada para el producto" pro_creditos.nom_producto "(" carteraVencida.cta_NoaPrvAdCr ") no existe en el Plan de Cuentas. Revise por favor."
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
                        RETURN NO-APPLY.
                    END.
    
                    DO cont = 1 TO MONTH(w_fecha):
                        IF cuentas.naturaleza = "DB" THEN
                            totalCuentaProvisionCapitalC = totalCuentaProvisionCapitalC + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                        ELSE
                            totalCuentaProvisionCapitalC = totalCuentaProvisionCapitalC - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                    END.
                END.
            END.
        END.
    END.

    IF flagDistintas = FALSE THEN DO:
        DISPLAY STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto + " - " + "" FORMAT "X(33)" AT 1
                totalesCreditos.provisionCapital FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                totalCuentaProvisionCapital * -1 FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                totalesCreditos.provisionCapital + totalCuentaProvisionCapital FORMAT "$->>>,>>>,>>>,>>9.99" AT 85 SKIP(2)
            WITH DOWN WIDTH 170 FRAME frmCreditos1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        totalDiferencia = totalDiferencia + (totalesCreditos.provisionCapital + totalCuentaProvisionCapital).
    END.
    ELSE DO:
        DISPLAY STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto + " - " + "Nómina" FORMAT "X(33)" AT 1
                totalesCreditos.provisionCapitalN FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                totalCuentaProvisionCapitalN * -1 FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                totalesCreditos.provisionCapitalN + totalCuentaProvisionCapitalN FORMAT "$->>>,>>>,>>>,>>9.99" AT 85
                STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto + " - " + "Caja" FORMAT "X(33)" AT 1
                totalesCreditos.provisionCapitalC FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
                totalCuentaProvisionCapitalC * -1 FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
                totalesCreditos.provisionCapitalC + totalCuentaProvisionCapitalC FORMAT "$->>>,>>>,>>>,>>9.99" AT 85 SKIP(2)
            WITH DOWN WIDTH 170 FRAME frmCreditos2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        totalDiferencia = totalDiferencia + (totalesCreditos.provisionCapitalN + totalCuentaProvisionCapitalN) + (totalesCreditos.provisionCapitalC + totalCuentaProvisionCapitalC).
    END.

    IF AVAILABLE errores THEN DO:
        DISPLAY SKIP(2)
                "AG NIT        PRO CUENTA      SDO_DISPONIBLE     INT_CAUSADO       INT_PAGAR ERROR" SKIP
                "-- ---------- --- ---------- --------------- --------------- --------------- ---------------------------" SKIP
            WITH DOWN WIDTH 170 FRAME frmErrores NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        FOR EACH errores NO-LOCK:
            DISPLAY errores.agencia FORMAT "99"
                    errores.nit FORMAT "X(10)"
                    errores.cod_producto FORMAT "999"
                    errores.cue_ahorros FORMAT "X(10)"
                    errores.sdo_disponible
                    errores.INT_Causado
                    errores.INT_pagar
                    errores.tError FORMAT "X(36)" SKIP
                WITH DOWN WIDTH 170 FRAME frmErrores NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.

        DISPLAY SKIP.
    END.
END.

DISPLAY "---------------------------------------------------------------------------------------------------------" SKIP
        "                                                                TOTAL DIFERENCIA -->" totalDiferencia FORMAT "->>>,>>>,>>>,>>9.99" SKIP(2)
            WITH DOWN WIDTH 170 FRAME frmDiferencia NO-LABELS NO-BOX USE-TEXT STREAM-IO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProvisionInteres AGENCIAW-Integridad_PC 
PROCEDURE ProvisionInteres :
DEFINE VAR totalDiferencia AS DECIMAL.
DEFINE VAR totalProvisionInteres AS DECIMAL.
DEFINE VAR totalCuentaProvisionInteres AS DECIMAL.

EMPTY TEMP-TABLE ttcuentas.

DISPLAY "--------------------------------------------------------------------------------------------------------" SKIP
        "                                PROVISIÓN INTERÉS           " SKIP
        "--------------------------------------------------------------------------------------------------------" SKIP
        "NOMBRE DEL PRODUCTO                          PRODUCTOS                 CONTABLE               DIFERENCIA" SKIP
        "---------------------                        ---------                 --------               ----------"
    WITH WIDTH 150 NO-LABELS.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    totalProvisionInteres = 0.
    totalCuentaProvisionInteres = 0.
    
    FIND FIRST totalesCreditos WHERE totalesCreditos.cod_credito = pro_creditos.cod_credito NO-LOCK NO-ERROR.

    FOR EACH carteraVencida WHERE carteraVencida.cod_producto = pro_creditos.cod_credito NO-LOCK:
        FOR EACH sal_cuenta WHERE (sal_cuenta.cuenta = carteraVencida.cta_AsoIntAdCr OR
                                   sal_cuenta.cuenta = carteraVencida.cta_AsoIntNaCr OR
                                   sal_cuenta.cuenta = carteraVencida.cta_NoaIntAdCr OR
                                   sal_cuenta.cuenta = carteraVencida.cta_NoaIntNaCr)
                              AND sal_cuenta.ano = YEAR(w_fecha)
                              AND sal_cuenta.agencia >= w_OfiIni
                              AND sal_cuenta.agencia <= W_OfiFin NO-LOCK:
            FIND FIRST ttcuentas WHERE ttcuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttcuentas THEN DO:
                CREATE ttcuentas.
                ttcuentas.cuenta = sal_cuenta.cuenta.

                totalCuentaProvisionInteres = totalCuentaProvisionInteres + sal_cuenta.sal_inicial.

                FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

                DO cont = 1 TO MONTH(w_fecha):
                    IF cuentas.naturaleza = "DB" THEN
                        totalCuentaProvisionInteres = totalCuentaProvisionInteres + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                    ELSE
                        totalCuentaProvisionInteres = totalCuentaProvisionInteres - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
                END.
            END.
        END.
    END.

    DISPLAY STRING(pro_creditos.cod_credito,"999") + "-" + pro_creditos.nom_producto FORMAT "X(33)" AT 1
            totalesCreditos.provisionInteres FORMAT "$->>>,>>>,>>>,>>9.99" AT 35
            totalCuentaProvisionInteres * -1 FORMAT "$->>>,>>>,>>>,>>9.99" AT 60
            totalesCreditos.provisionInteres + totalCuentaProvisionInteres FORMAT "$->>>,>>>,>>>,>>9.99" AT 85
        WITH DOWN WIDTH 170 FRAME frmProvisionInteres NO-LABELS NO-BOX USE-TEXT STREAM-IO.

    totalDiferencia = totalDiferencia + (totalesCreditos.provisionInteres + totalCuentaProvisionInteres).
END.

DISPLAY "---------------------------------------------------------------------------------------------------------" SKIP
        "                                                                TOTAL DIFERENCIA -->" totalDiferencia FORMAT "->>>,>>>,>>>,>>9.99" SKIP(2)
            WITH DOWN WIDTH 170 FRAME frmProvisionInteres2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tot2 AGENCIAW-Integridad_PC 
PROCEDURE Tot2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DISPLAY SKIP(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida AGENCIAW-Integridad_PC 
PROCEDURE Valida :
Msaje:SCREEN-VALUE IN FRAME F_Integ = "Generando Temporal con Configuraciones...".

FOR EACH Pro_Ahorros FIELDS(cod_ahorro tip_ahorro nom_prod) WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK BY Pro_Ahorros.Tip_Ahorro BY Pro_Ahorros.Cod_Ahorro:
    FOR EACH CortoLargo FIELDS(cod_producto cta_asoad)
                        WHERE CortoLargo.Agencia EQ 1
                          AND CortoLargo.Clase_Producto EQ 1
                          AND CortoLargo.Cod_Producto EQ Pro_Ahorros.Cod_Ahorro NO-LOCK BY CortoLargo.Plazo_Inicial:
        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Ahorros.Tip_Ahorro
                              AND TempCtas.Pto = CortoLargo.Cod_Producto
                              AND TempCtas.CtaCtable = CortoLargo.Cta_AsoAd NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            CREATE TempCtas.
            ASSIGN TempCtas.ClaP = Pro_Ahorros.Tip_Ahorro
                   TempCtas.NomPro = Pro_Ahorros.Nom_prod
                   TempCtas.Pto = CortoLargo.Cod_Producto
                   TempCtas.CtaCtable = CortoLargo.Cta_AsoAd.
        END.

        IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN
            NEXT.

        FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                               AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
        IF NOT AVAIL(Liqui_Int) THEN DO:
            MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        ASSIGN TempCtas.CtaLiq = Liqui_Int.Cta_CauCr         /*Los Causados*/
               TempCtas.IntAnt = Liqui_Int.CtaCr_LiqAso.     /*Los Por Pagar*/
    END.
END.

FOR EACH Pro_Creditos FIELDS (Cod_Credito Tip_Credito Nom_prod) WHERE Pro_Creditos.Estado EQ 1 NO-LOCK BY Pro_Creditos.Tip_Credito BY Pro_Creditos.Cod_Credito:
    FOR EACH CortoLargo FIELDS(Cta_AsoAd Cta_HonorariosDB Cta_PolizasDB Cta_CostasDB Cta_VigGarAd Cta_ContrapartidaGar Cod_Producto)
                        WHERE CortoLargo.Agencia EQ 1
                          AND CortoLargo.Clase_Producto EQ 2
                          AND CortoLargo.Cod_Producto   EQ Pro_Creditos.Cod_Credito NO-LOCK BY CortoLargo.Plazo_Inicial:
        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                              AND TempCtas.Pto = CortoLargo.Cod_Producto
                              AND TempCtas.CtaCtable = CortoLargo.Cta_AsoAd NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            CREATE TempCtas.
            ASSIGN TempCtas.ClaP = Pro_Creditos.Tip_Credito
                   TempCtas.Pto = Pro_Creditos.Cod_Credito
                   TempCtas.NomPro = Pro_Creditos.Nom_prod
                   TempCtas.CtaCtable = CortoLargo.Cta_AsoAd
                   TempCtas.CtaProd = CortoLargo.Cta_AsoAd
                   TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB
                   TempCtas.CtaPol = CortoLargo.Cta_PolizasDB
                   TempCtas.CtaCos = CortoLargo.Cta_CostasDB
                   TempCtas.CtaGar = CortoLargo.Cta_VigGarAd
                   TempCtas.CtaCGa = CortoLargo.Cta_ContrapartidaGar
                   TempCtas.Calif = 1
                   TempCtas.GL = 4.
        END.

        FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                               AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
        IF NOT AVAIL(Liqui_Int) THEN DO:
            MESSAGE "Falta Liqui_Int Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        ASSIGN TempCtas.CtaLiq = Liqui_Int.CtaDb_LiqAso
               TempCtas.IntAnt = Liqui_Int.CtaInt_AntAso
               TempCtas.IntMor = Liqui_Int.CtaDb_MoraAso
               TempCtas.DifCoD = Liqui_Int.CtaDb_DifCobAso
               TempCtas.DifCoH = Liqui_Int.CtaCr_DifCobAso
               TempCtas.MorIng = Liqui_Int.CtaCr_MoraAso.

        FOR EACH Cuentas fields(cuenta)
                         WHERE SUBSTRING(Cuentas.Cuenta,1,4) EQ "8120" /*Int-DifCobro*/
                           AND Cuentas.Estado EQ 1
                           AND Cuentas.Tipo EQ 2 NO-LOCK:
            FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                                  AND TempCtas.Pto = Pro_Creditos.Cod_Credito
                                  AND TempCtas.DifCoD = Cuentas.Cuenta NO-ERROR.
            IF NOT AVAIL(TempCtas) THEN DO:
                RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/

                ASSIGN TempCtas.Calif = 1
                       TempCtas.DifCoD = Cuentas.Cuenta
                       TempCtas.GL = 4.
            END.
        END.
    END.

    FOR EACH CarteraVencida fields(Cod_Producto Cta_AsoAdDb Cod_Califica Cta_NoaAdDb CtaCal_Costas Cta_AsoNaDb Cta_NoaNaDb CtaCal_Interes)
                            WHERE CarteraVencida.Cod_Producto EQ Pro_Creditos.Cod_Credito NO-LOCK BY Cod_Califica:
        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                              AND TempCtas.Pto = CarteraVencida.Cod_Producto
                              AND TempCtas.CtaCtable = CarteraVencida.Cta_AsoAdDb NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/

            ASSIGN TempCtas.Calif = CarteraVencida.Cod_Califica
                   TempCtas.CtaCtable = CarteraVencida.Cta_AsoAdDb
                   TempCtas.GL = 1.
        END.

        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                              AND TempCtas.Pto = CarteraVencida.Cod_Producto
                              AND TempCtas.CtaCtable = CarteraVencida.Cta_NoaAdDb NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/

            ASSIGN TempCtas.Calif = CarteraVencida.Cod_Califica
                   TempCtas.CtaCtable = CarteraVencida.Cta_NoaAdDb
                   TempCtas.GL = 2.
        END.

        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaCtable = CarteraVencida.Cta_AsoNaDb NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
            ASSIGN TempCtas.Calif = CarteraVencida.Cod_Califica
                   TempCtas.CtaCtable = CarteraVencida.Cta_AsoNaDb
                   TempCtas.GL = 3.
        END.

        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                              AND TempCtas.Pto = CarteraVencida.Cod_Producto
                              AND TempCtas.CtaCtable = CarteraVencida.Cta_NoaNaDb NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/

            ASSIGN TempCtas.Calif = CarteraVencida.Cod_Califica
                   TempCtas.CtaCtable = CarteraVencida.Cta_NoaNaDb
                   TempCtas.GL = 4.
        END.

        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaLiq = CarteraVencida.CtaCal_Interes NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/

            ASSIGN TempCtas.Calif = CarteraVencida.Cod_Califica
                   TempCtas.CtaLiq = CtaCal_Interes
                   TempCtas.GL = 4.
        END.

        FIND FIRST TempCtas WHERE TempCtas.ClaP = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaCos = CarteraVencida.CtaCal_Costas NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/

            ASSIGN TempCtas.Calif = CarteraVencida.Cod_Califica
                   TempCtas.CtaCos = CarteraVencida.CtaCal_Costas
                   TempCtas.GL = 4.
        END.
    END.
END.

END PROCEDURE.

PROCEDURE CtasCred:
    CREATE TempCtas.
    ASSIGN TempCtas.ClaP = Pro_Creditos.Tip_Credito
           TempCtas.Pto = Pro_Creditos.Cod_Credito
           TempCtas.NomPro = Pro_Creditos.Nom_prod
           TempCtas.CtaProd = TempCtas.CtaCtable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE x_valida AGENCIAW-Integridad_PC 
PROCEDURE x_valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Pro_Ahorros FIELDS(cod_ahorro tip_ahorro nom_prod)
        WHERE 
            Pro_Ahorros.Estado EQ 1 NO-LOCK 
        BY Pro_Ahorros.Tip_Ahorro 
        BY Pro_Ahorros.Cod_Ahorro:
        FOR EACH CortoLargo FIELDS(cod_producto cta_asoad)
            WHERE 
                CortoLargo.Agencia        EQ 1
            AND CortoLargo.Clase_Producto EQ 1
            AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro NO-LOCK
            BY CortoLargo.Plazo_Inicial:
            FIND FIRST TempCtas 
                WHERE   
                    TempCtas.ClaP   = Pro_Ahorros.Tip_Ahorro
            AND TempCtas.Pto    = CortoLargo.Cod_Producto
            AND TempCtas.CtaCtable = CortoLargo.Cta_AsoAd NO-ERROR.
            IF NOT AVAIL(TempCtas) 
            THEN DO:
                CREATE TempCtas.
                ASSIGN  TempCtas.ClaP   = Pro_Ahorros.Tip_Ahorro
                        TempCtas.NomPro = Pro_Ahorros.Nom_prod
                        TempCtas.Pto    = CortoLargo.Cod_Producto
                        TempCtas.CtaCtable = CortoLargo.Cta_AsoAd. 
            END.
            IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN NEXT.
            FIND FIRST Liqui_Int
                WHERE 
                    Liqui_Int.Clase_Producto EQ 1
                AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
            IF NOT AVAIL(Liqui_Int) 
            THEN DO:
                MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro   SKIP VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
            END.
            ASSIGN  TempCtas.CtaLiq = Liqui_Int.Cta_CauCr         /*Los Causados*/   
                    TempCtas.IntAnt = Liqui_Int.CtaCr_LiqAso.     /*Los Por Pagar*/
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCuentasDeAhorros AGENCIAW-Integridad_PC 
FUNCTION fCuentasDeAhorros RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    /*  DESCRIPCION: DEVUELVE UNA LISTA DE LAS CUENTAS DE AHORRO CON SEPARADOR chr(1)
            1ra entrada: CortoLargo.Cta_AsoAd
            2da entrada: Liqui_Int.Cta_CauCr
            3ra entrada: Liqui_Int.CtaCr_LiqAso
        LOG: CREADA - EMM - 1 oct 2007
        */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    /* OJO: se carga la configuracion de cuentas más reciente 
            no se programa diversas cuentas por cambio de parametrización */
    FOR LAST CortoLargo FIELDS(cod_producto cta_asoad) 
        WHERE 
            CortoLargo.Agencia        EQ 1
        AND CortoLargo.Clase_Producto EQ 1
        AND CortoLargo.Cod_Producto   EQ Ahorros.Cod_Ahorro NO-LOCK:

        cRtrno = CortoLargo.Cta_AsoAd.
        FIND FIRST Liqui_Int NO-LOCK
            WHERE 
                Liqui_Int.Clase_Producto EQ 1
            AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-ERROR.
        crtrno = crtrno + CHR(1) + IF AVAILABLE Liqui_Int THEN Liqui_Int.Cta_CauCr ELSE "" + CHR(1) + IF AVAILABLE Liqui_Int THEN Liqui_Int.CtaCr_LiqAso ELSE "".
    END.
    RETURN crtrno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSldoCuenta AGENCIAW-Integridad_PC 
FUNCTION fSldoCuenta RETURNS CHARACTER
  (iagncia AS integer,ccuenta AS CHAR,iano AS integer,imes AS INTEGER /* parameter-definitions */ ) :
/*  DESCRIPCION: DEVUELVE EL SALDO DE LA CUENTA 
LOG: CREADA - EMM - 2 oct 2007
*/
    DEF VAR deSldo AS DECIMAL NO-UNDO.
    DEF VAR k AS INTEGER NO-UNDO.
    FOR EACH Sal_Cuenta fields(sal_inicial cuenta db cr) NO-LOCK
        WHERE 
            Sal_Cuenta.Agencia EQ iagncia                                 
        AND Sal_Cuenta.Cuenta  EQ ccuenta                                  
        AND Sal_Cuenta.Ano     EQ iano:
        deSldo = deSldo + Sal_Cuenta.Sal_Inicial.
        DO k = 1 TO imes BY 1:                                                                  
            FIND CUENTAS no-lock
                WHERE 
                    CUENTAS.CUENTA EQ Sal_Cuenta.Cuenta NO-ERROR.                        
            IF AVAILABLE cuentas
            THEN DO:
                deSldo = deSldo + IF Cuentas.Natur EQ "Db" THEN (Db[K] - Cr[K]) ELSE (Cr[K] - Db[K]).
            END.
        END.
    END.
    RETURN STRING(deSldo).
END FUNCTION.
/*fin fSldoCuenta */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

