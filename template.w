&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-CentralesDeRiesgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-CentralesDeRiesgo 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
DEFI TEMP-TABLE TempCtas NO-UNDO

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
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbCentralDeRiesgo Btn_Imp 
&Scoped-Define DISPLAYED-OBJECTS cmbCentralDeRiesgo Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCuentasDeAhorros W-CentralesDeRiesgo 
FUNCTION fCuentasDeAhorros RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSldoCuenta W-CentralesDeRiesgo 
FUNCTION fSldoCuenta RETURNS CHARACTER
  (iagncia AS integer,ccuenta AS CHAR,iano AS integer,imes AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-CentralesDeRiesgo AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 9.72 BY 1.77.

DEFINE VARIABLE cmbCentralDeRiesgo AS CHARACTER FORMAT "X(30)":U 
     LABEL "Central de Riesgo" 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "CIFIN" 
     DROP-DOWN-LIST
     SIZE 22 BY .92 TOOLTIP "Centrales de Riesgo"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbCentralDeRiesgo AT ROW 1.54 COL 2.71
     Btn_Imp AT ROW 2.73 COL 31.29
     Msaje AT ROW 4.77 COL 3 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 41.72 BY 5.62
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
  CREATE WINDOW W-CentralesDeRiesgo ASSIGN
         HIDDEN             = YES
         TITLE              = "Centrales de Riesgo"
         HEIGHT             = 5.19
         WIDTH              = 41.29
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
/* SETTINGS FOR WINDOW W-CentralesDeRiesgo
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cmbCentralDeRiesgo IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Msaje IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L 2                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-CentralesDeRiesgo)
THEN W-CentralesDeRiesgo:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-CentralesDeRiesgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-CentralesDeRiesgo W-CentralesDeRiesgo
ON END-ERROR OF W-CentralesDeRiesgo /* Centrales de Riesgo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-CentralesDeRiesgo W-CentralesDeRiesgo
ON WINDOW-CLOSE OF W-CentralesDeRiesgo /* Centrales de Riesgo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-CentralesDeRiesgo
ON CHOOSE OF Btn_Imp IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_PathSpl + "-InfInteg-" + W_Usuario.

    {INCLUIDO\Imprimir.I "Listado"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCentralDeRiesgo
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-CentralesDeRiesgo 


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

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-CentralesDeRiesgo  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-CentralesDeRiesgo)
  THEN DELETE WIDGET W-CentralesDeRiesgo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-CentralesDeRiesgo  _DEFAULT-ENABLE
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
  DISPLAY cmbCentralDeRiesgo Msaje 
      WITH FRAME DEFAULT-FRAME IN WINDOW W-CentralesDeRiesgo.
  ENABLE cmbCentralDeRiesgo Btn_Imp 
      WITH FRAME DEFAULT-FRAME IN WINDOW W-CentralesDeRiesgo.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW W-CentralesDeRiesgo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-CentralesDeRiesgo 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.I}

DISPLAY STRING(W_Nom_Entidad,"X(40)") FORMAT "X(100)" SKIP
        "INFORME DE INTEGRIDAD - PRODUCTOS Vs. CONTABILIDAD" SKIP
        "Fecha:" STRING(W_fecha,"99/99/9999") FORM "X(80)" SKIP
        "Agencia:" string(w_agencia, "X(80)") SKIP
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tot2 W-CentralesDeRiesgo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE x_valida W-CentralesDeRiesgo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCuentasDeAhorros W-CentralesDeRiesgo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSldoCuenta W-CentralesDeRiesgo 
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

