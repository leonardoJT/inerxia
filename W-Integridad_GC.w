&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Integridad_PC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Integridad_PC 
/*---------------------------------------------------------------------------
  File:        W-Integridad_PC.W
  Description: Informe con Sdos.de pdctos comparados con Sdos.Ctas-Contables.
               Tablas: Ahorros, Creditos y Sal_Cuenta consultando las config.
  Author:      GAER
  Created:     Mayo 4/05

----------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

    ON RETURN TAB.

   {Incluido/Variable.I "SHARED"}
   {Incluido/VARCON.I   "SHARED"}
        
   DEFI TEMP-TABLE TempCtas NO-UNDO
        FIELD TipP   AS CHAR  FORM "X(1)"
        FIELD ClaP   AS INTEG FORM "9"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
        FIELD NomPro AS CHAR FORM "X(20)"
        FIELD CtaPro LIKE Cuentas.Cuenta
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
        INDEX idx tipp clap pto ctapro.  /*1 Ad con LIb, 2 Ad sin Lib, 3 NA con Lib, 4 Na sin Lib*/
DEF VAR htempctas AS HANDLE NO-UNDO.
htempctas = TEMP-TABLE tempctas:HANDLE.    

   DEFI TEMP-TABLE TCon_Sdos NO-UNDO
        FIELD Ag     LIKE Agencias.Agencia
        FIELD TipP   AS CHAR  FORM "X(1)" 
        FIELD ClaP   AS INTEG FORM "9"
        FIELD Cpto   AS CHAR  FORM "X(4)"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro     
        FIELD CtaPro LIKE Cuentas.Cuenta 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Integ

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 BUTTON-5 W_CmbOfi BTN-Exportar ~
Btn_Proc Btn_Imp W_FecMes Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_FecMes W_Cont Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCuentasDeAhorros W-Integridad_PC 
FUNCTION fCuentasDeAhorros RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSldoCuenta W-Integridad_PC 
FUNCTION fSldoCuenta RETURNS CHARACTER
  (iagncia AS integer,ccuenta AS CHAR,iano AS integer,imes AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Integridad_PC AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Exportar 
     LABEL "Exportar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imp 
     LABEL "Imprimir" 
     SIZE 9.72 BY 1.35.

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar" 
     SIZE 10 BY 1.46 TOOLTIP "Procesar los Productos y las Cuentas de Balance".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     DROP-DOWN-LIST
     SIZE 43.14 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 57.43 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>>,>>9":U INITIAL 0 
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
     SIZE 15 BY 8.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Integ
     BUTTON-5 AT ROW 1.46 COL 68.86
     W_CmbOfi AT ROW 1.5 COL 19.29 COLON-ALIGNED
     BTN-Exportar AT ROW 3.15 COL 46 WIDGET-ID 2
     Btn_Proc AT ROW 3.42 COL 68.86
     Btn_Imp AT ROW 5.27 COL 69
     W_FecMes AT ROW 5.46 COL 19.72 COLON-ALIGNED
     W_Cont AT ROW 6.42 COL 49.14 COLON-ALIGNED NO-LABEL
     Msaje AT ROW 7.65 COL 7.86 NO-LABEL
     Btn_Done AT ROW 8 COL 69 HELP
          "Sale del proceso de Depreciación y Ajustes"
     "Regist.Proceso" VIEW-AS TEXT
          SIZE 13.72 BY .88 AT ROW 5.5 COL 51.14
     RECT-314 AT ROW 1.27 COL 66.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.72 BY 9.65
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
  CREATE WINDOW W-Integridad_PC ASSIGN
         HIDDEN             = YES
         TITLE              = "Integridad Productos Vs Cuentas, Programa W-Integridad_PC.W"
         HEIGHT             = 9.65
         WIDTH              = 82.72
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
/* SETTINGS FOR WINDOW W-Integridad_PC
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Integ
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Integ
   NO-ENABLE ALIGN-L 2                                                  */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Integ
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Integridad_PC)
THEN W-Integridad_PC:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Integ
/* Query rebuild information for FRAME F_Integ
     _Query            is NOT OPENED
*/  /* FRAME F_Integ */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Integridad_PC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Integridad_PC W-Integridad_PC
ON END-ERROR OF W-Integridad_PC /* Integridad Productos Vs Cuentas, Programa W-Integridad_PC.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Integridad_PC W-Integridad_PC
ON WINDOW-CLOSE OF W-Integridad_PC /* Integridad Productos Vs Cuentas, Programa W-Integridad_PC.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Exportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Exportar W-Integridad_PC
ON CHOOSE OF BTN-Exportar IN FRAME F_Integ /* Exportar */
DO:
    OUTPUT TO "c:\ConSaldos.txt".
/*     PUT "Age; Tipo; ClaProd; Concep; Pto; Cuenta; NomProd; saldoProd; SaldoCta; Calif; GL" */
/*         SKIP.                                                                              */
    FOR EACH TCon_Sdos NO-LOCK BY Ag BY TipP BY ClaP:
        FORM
            Ag      COLUMN-LABEL "Age"          
            TipP    COLUMN-LABEL "TipP"
            ClaP    COLUMN-LABEL "CalseP"
            Cpto    COLUMN-LABEL "Concepto"
            Pto     COLUMN-LABEL "Pto"
            CtaPro  COLUMN-LABEL "CuentaPro"
            NomPro  COLUMN-LABEL "Producto"
            SdoPro  COLUMN-LABEL "Saldo Pro"        FORMAT ">>>,>>>,>>>,>>9.99" 
            SdoCta  COLUMN-LABEL "Saldo Cta"        FORMAT ">>>,>>>,>>>,>>9.99" 
            Calif   COLUMN-LABEL "Calif"
            GL      COLUMN-LABEL "GL"
            WITH FRAME a DOWN COLUMN 1 WIDTH 250                     
                NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.


        DISPLAY 
            Ag     
            TipP   
            ClaP   
            Cpto   
            Pto    
            CtaPro 
            NomPro 
            SdoPro 
            SdoCta 
            Calif  
            GL     
            WITH FRAME a.
        DOWN WITH FRAME a.

/*         EXPORT DELIMITER ";"                                        */
/* /*         PUT */                                                   */
/*             Ag          /* ";" */                                   */
/*             TipP        /* ";" */                                   */
/*             ClaP        /* ";" */                                   */
/*             Cpto        /* ";" */                                   */
/*             Pto         /* ";" */                                   */
/*             CtaPro      /* ";" */                                   */
/*             NomPro      /* ";" */                                   */
/*             SdoPro      /* ";" */       FORMAT ">>>,>>>,>>>,>>9.99" */
/*             SdoCta      /* ";" */       FORMAT ">>>,>>>,>>>,>>9.99" */
/*             Calif       /* ";" */                                   */
/*             GL                                                      */
/*             SKIP.                                                   */
    END.
    OUTPUT CLOSE.

    MESSAGE "Export Finalizado"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Integridad_PC
ON CHOOSE OF Btn_Done IN FRAME F_Integ /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-Integridad_PC
ON CHOOSE OF Btn_Imp IN FRAME F_Integ /* Imprimir */
DO:
        
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
  ASSIGN Listado = W_PathSpl + "-InfInteg-" + W_Usuario.
         

  {INCLUIDO\Imprimir.I "Listado"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-Integridad_PC
ON CHOOSE OF Btn_Proc IN FRAME F_Integ /* Procesar */
DO:
/*
    FOR EACH TempCtas:  DELETE TempCtas.  END.
    FOR EACH TCon_Sdos: DELETE TCon_Sdos. END.
*/
    DELETE OBJECT htempctas.        
    DELETE OBJECT htcon_sdos. 
        
  SESSION:SET-WAIT-STATE("GENERAL").
  RUN Valida NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     SESSION:SET-WAIT-STATE("").

     Msaje:SCREEN-VALUE = "El Proceso de INTEGRIDAD NO se efectuò...Revise por favor".

     RETURN.     
  END.
  ELSE DO:
     RUN SdosXCta.  /*Halla todos los Sdos-Contables*/

     RUN Procesar.  /*Halla los Sdos de pdctos*/
     Listado = "C:\info_fodun\INTEG_" + SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME F_Integ,1,3)
                         + STRING(W_Fecha,"999999") + ".Txt".

     {Incluido\ImpArch.I "listado"}
     /*RUN Guarda_Control.*/   /*Graba el informe en Disco*/

     /*Listado = W_PathSpl + "INTEG_CtaPdcto.Txt".
     {Incluido\Imprimir.I "listado"}  */
  END.
        
  /*ASSIGN W_Cont = 0
         W_Cont:SCREEN-VALUE IN FRAME F_Integ = STRING(W_Cont). */

/*     iano = YEAR(W_FecMes).                         */
/*     imes = MONTH(W_FecMes).                        */
/*     SESSION:SET-WAIT-STATE("").                    */
/*     RUN ahrros.                                    */
/*     MESSAGE "Proceso Terminado" VIEW-AS ALERT-BOX. */
    APPLY "choose" TO Btn_Imp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Integridad_PC
ON CHOOSE OF BUTTON-5 IN FRAME F_Integ /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Integridad_PC
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Integ /* Agencia */
DO:
  IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
     FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                           AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiIni = Agencias.Agencia.
        
     FIND LAST Agencias WHERE Agencias.Agencia GT 0
                          AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiFin = Agencias.Agencia.
  END.
  ELSE ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
              W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
    iAgncia = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecMes W-Integridad_PC
ON LEAVE OF W_FecMes IN FRAME F_Integ /* Fecha Proceso */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Integridad_PC 


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

  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni                               = W_Agencia
         W_OfiFin                               = W_Agencia
         W_FecMes:SCREEN-VALUE IN FRAME F_Integ = STRING(W_Fecha)
         W_FecMes.
                  
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).      
  END.              
     
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
  APPLY "VALUE-CHANGED" TO W_CmbOfi.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ahrros W-Integridad_PC 
PROCEDURE ahrros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
OUTPUT TO c:\tmp\exito.txt.    
DEF VAR clsta AS CHAR NO-UNDO.    
DEF VAR clsta1 AS CHAR NO-UNDO.    
DEF VAR j AS INTEGER NO-UNDO.    
DEF VAR deScap AS DECIMAL NO-UNDO.
DEF VAR deIcau AS DECIMAL NO-UNDO.
DEF VAR deIxpg AS DECIMAL NO-UNDO.
DEF VAR iAgncia AS INTEGER NO-UNDO.
    iAgncia = INTEGER(SUBSTRING(w_cmbofi:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)).
    
    FOR EACH ahorros FIELDS(agencia tip_ahorro cod_ahorro sdo_canje sdo_disponible INT_causado Int_Pagar) NO-LOCK
        WHERE
            (IF iAgncia = 0 THEN TRUE ELSE ahorros.agencia = iAgncia)
        BREAK
            BY ahorros.agencia
            BY ahorros.tip_ahorro
            BY Ahorros.Cod_Ahorro:
        IF FIRST-OF(ahorros.agencia) 
        THEN DO:
            FIND agencias NO-LOCK
                WHERE
                    agencias.agencia = ahorros.agencia NO-ERROR.
            FORM HEADER 
                "JURISCOOP" AT 1
                "Fecha: " AT 1 string(TODAY,"99/99/9999")
                "Agencia Del Control : " AT 1 agencias.Nombre 
                "-----------------------------------------------------------------------------------------------------------" AT 1
                "Ag. T C Pcto Descrip.Producto     Cpto     Sdo-del Pdcto  Saldo Cta-Contab Cta-Contable          Diferencia" AT 1
                "-----------------------------------------------------------------------------------------------------------" AT 1
                WITH STREAM-IO PAGE-TOP FRAME fAgncia WIDTH 132.
            VIEW FRAME fAgncia.
        END. /* IF FIRST-OF(ahorros.agencia) */
    
        ACCUMULATE (ahorros.sdo_canje + ahorros.sdo_disponible) (TOTAL BY ahorros.agencia BY ahorros.tip_ahorro BY ahorros.cod_ahorro).
        ACCUMULATE ahorros.INT_causado  (TOTAL BY ahorros.agencia BY ahorros.tip_ahorro BY ahorros.cod_ahorro).
        ACCUMULATE ahorros.Int_Pagar  (TOTAL BY ahorros.agencia BY ahorros.tip_ahorro BY ahorros.cod_ahorro).
    
        IF LAST-OF(Ahorros.Cod_Ahorro)
        THEN DO:
            cLsta = fCuentasDeAhorros(). /* lista de las cuentas a buscar saldo */
            cLsta1 = fill(chr(1),NUM-ENTRIES(cLsta,CHR(1)) - 1). /* lista deparada por chr(1) de los saldos */
            DO j = 1 TO NUM-ENTRIES(cLsta,CHR(1)):
                 ENTRY(j,clsta1,CHR(1)) = fSldoCuenta(ahorros.agencia,ENTRY(j,clsta,CHR(1)),iano,imes).
            END.
            FIND pro_ahorros NO-LOCK
                WHERE
                    pro_ahorros.cod_ahorro = ahorros.cod_ahorro
                AND pro_ahorros.tip_ahorro = ahorros.tip_ahorro NO-ERROR.
            deScap = DECIMAL(ENTRY(1,cLsta1,CHR(1))) NO-ERROR.
            deicau = DECIMAL(ENTRY(2,cLsta1 + CHR(1),CHR(1))) NO-ERROR.
            deixpg = DECIMAL(ENTRY(3,cLsta1 + CHR(1) + CHR(1),CHR(1))) NO-ERROR.
            ACCUMULATE descap (TOTAL BY ahorros.agencia BY ahorros.tip_ahorro BY ahorros.cod_ahorro).
            ACCUMULATE deicau (TOTAL BY ahorros.agencia BY ahorros.tip_ahorro BY ahorros.cod_ahorro).
            ACCUMULATE deixpg (TOTAL BY ahorros.agencia BY ahorros.tip_ahorro BY ahorros.cod_ahorro).
            DISPLAY SKIP(1) ahorros.agencia NO-LABEL
                    "A" NO-LABEL
                    ahorros.tip_ahorro NO-LABEL
                    ahorros.cod_ahorro NO-LABEL
                    pro_ahorros.nom_producto FORMAT "x(20)" WHEN AVAILABLE pro_ahorros NO-LABEL
                    "SCAP" NO-LABEL AT 35
                    ACCUM TOTAL BY ahorros.cod_ahorro (ahorros.sdo_canje + ahorros.sdo_disponible) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL
                    deScap FORMAT "$->>>,>>>,>>>,>>9" COLUMN-LABEL "Saldo Cta-Contab"
                    ENTRY(1,cLsta,CHR(1)) FORMAT "x(14)" COLUMN-LABEL "Cta-Contable"
                    (ACCUM TOTAL BY ahorros.cod_ahorro (ahorros.sdo_canje + ahorros.sdo_disponible)) - deScap FORMAT "$->>>,>>>,>>>,>>9" COLUMN-LABEL "Diferencia"
                    SKIP
                    "ICAU" AT 35 NO-LABEL
                    ACCUM TOTAL BY ahorros.cod_ahorro ahorros.INT_causado FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL
                    deicau FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL
                    ENTRY(2,cLsta + CHR(1),CHR(1)) FORMAT "x(14)" NO-LABEL
                    (ACCUM TOTAL BY ahorros.cod_ahorro ahorros.INT_causado) - deicau FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL
                    SKIP
                    "IXPG" AT 35 NO-LABEL
                    ACCUM TOTAL BY ahorros.cod_ahorro ahorros.INT_pagar FORMAT "$->>>,>>>,>>>,>>9"  NO-LABEL
                    deixpg FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL
                    ENTRY(3,cLsta + CHR(1) + CHR(1),CHR(1)) FORMAT "x(14)" NO-LABEL
                    (ACCUM TOTAL BY ahorros.cod_ahorro ahorros.INT_pagar) - deixpg FORMAT "$->>>,>>>,>>>,>>9"  NO-LABEL
                    
                WITH WIDTH 132 WITH STREAM-IO  FRAME fdtlle DOWN NO-LABELS.
        END. /* IF LAST-OF(Ahorros.Cod_Ahorro) */
    
    
        IF LAST-OF(ahorros.cod_ahorro)
        THEN DO:
        END.
    
        IF LAST-OF(ahorros.tip_ahorro)
        THEN DO:
            DISPLAY 
                    ahorros.agencia NO-LABEL
                    "A" NO-LABEL 
                    ahorros.tip_ahorro NO-LABEL
                    "TOTAL CLASE-PRODUCTO" FORMAT "x(20)" NO-LABEL
                    "SCAP" NO-LABEL at 35
                    ACCUM TOTAL BY ahorros.tip_ahorro (ahorros.sdo_canje + ahorros.sdo_disponible) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                    ACCUM TOTAL BY ahorros.tip_ahorro deScap FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                    (ACCUM TOTAL BY ahorros.tip_ahorro (ahorros.sdo_canje + ahorros.sdo_disponible)) - (ACCUM TOTAL BY ahorros.tip_ahorro deScap) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                    "ICAU" NO-LABEL at 35
                    ACCUM TOTAL BY ahorros.tip_ahorro ahorros.INT_causado FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                    ACCUM TOTAL BY ahorros.tip_ahorro deicau FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                    (ACCUM TOTAL BY ahorros.tip_ahorro ahorros.INT_causado) - (ACCUM TOTAL BY ahorros.tip_ahorro deicau) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                    "IXPG" NO-LABEL at 35
                    ACCUM TOTAL BY ahorros.tip_ahorro ahorros.INT_pagar  FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                    ACCUM TOTAL BY ahorros.tip_ahorro deixpg FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                    (ACCUM TOTAL BY ahorros.tip_ahorro ahorros.INT_pagar) - (ACCUM TOTAL BY ahorros.tip_ahorro deixpg) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                    SKIP(2)
                WITH FRAME tcodAhorro STREAM-IO WIDTH 132.
        END.
    
        IF LAST-OF(ahorros.agencia)
        THEN DO:
            DISPLAY 
                    "TOTAL AGENCIA" FORMAT "x(20)" NO-LABEL AT 9
                    "SCAP" NO-LABEL at 35
                    ACCUM TOTAL BY ahorros.agencia (ahorros.sdo_canje + ahorros.sdo_disponible) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                    ACCUM TOTAL BY ahorros.agencia deScap FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                    (ACCUM TOTAL BY ahorros.agencia (ahorros.sdo_canje + ahorros.sdo_disponible)) - (ACCUM TOTAL BY ahorros.agencia deScap) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                    "ICAU" NO-LABEL at 35
                    ACCUM TOTAL BY ahorros.agencia ahorros.INT_causado FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                    ACCUM TOTAL BY ahorros.agencia deicau FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                    (ACCUM TOTAL BY ahorros.agencia ahorros.INT_causado) - (ACCUM TOTAL BY ahorros.agencia deicau) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                    "IXPG" NO-LABEL at 35
                    ACCUM TOTAL BY ahorros.agencia ahorros.INT_pagar  FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                    ACCUM TOTAL BY ahorros.agencia deixpg FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                    (ACCUM TOTAL BY ahorros.agencia ahorros.INT_pagar) - (ACCUM TOTAL BY ahorros.agencia deixpg) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                    SKIP(2)
                WITH FRAME tagncia STREAM-IO WIDTH 132.
            /* RUN Crdtos(iAgncia). */
            PAGE.
        END.
        /*    
        IF LAST(ahorros.agencia)
        THEN DO:
            DISPLAY 
                "T O T A L" FORMAT "x(20)" NO-LABEL AT 9
                "SCAP" NO-LABEL at 35
                ACCUM TOTAL (ahorros.sdo_canje + ahorros.sdo_disponible) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                ACCUM TOTAL deScap FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                (ACCUM TOTAL (ahorros.sdo_canje + ahorros.sdo_disponible)) - (ACCUM TOTAL deScap) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                "ICAU" NO-LABEL at 35
                ACCUM TOTAL ahorros.INT_causado FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                ACCUM TOTAL deicau FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                (ACCUM TOTAL ahorros.INT_causado) - (ACCUM TOTAL  deicau) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                "IXPG" NO-LABEL at 35
                ACCUM TOTAL ahorros.INT_pagar  FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 40
                ACCUM TOTAL deixpg FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL 
                (ACCUM TOTAL ahorros.INT_pagar) - (ACCUM TOTAL deixpg) FORMAT "$->>>,>>>,>>>,>>9" NO-LABEL AT 91
                SKIP(2)
            WITH FRAME fttal STREAM-IO WIDTH 132.
            PAGE.
        END. */
    END. /* FOR EACH ahorros */
    OUTPUT CLOSE.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crdtos W-Integridad_PC 
PROCEDURE Crdtos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* CREDITOS */
/*    
DEF INPUT PARAMETER iAgncia AS INTEGER NO-UNDO.    
DEF VAR desldocta AS DECIMAL NO-UNDO.
DEF VAR k AS INTEGER NO-UNDO.
DEF VAR desldos AS DECIMAL EXTENT 8 NO-UNDO.
DEF VAR iorden AS INTEGER NO-UNDO.
DEF VAR clcuentas AS CHAR NO-UNDO.
DEF VAR desldo AS DECIMAL NO-UNDO.
FOR EACH creditos 
    FIELDS (tip_credito agencia Sdo_Capital INT_corrientes cod_credito INT_anticipado int_morcobrar INT_difcobro INT_moradifcob honorarios polizas costas) NO-LOCK
    WHERE
        (IF iagncia = 0 THEN TRUE ELSE creditos.agencia = iAgncia)
    AND creditos.estado = 2
    BREAK
        BY creditos.agencia 
        BY creditos.tip_credito
        BY creditos.cod_credito: 

    IF FIRST-OF(creditos.agencia)
    THEN DO:
        /* calcula los saldos de cada cuenta involucrada por agencia */
        FOR EACH tcrtolrgo
            BREAK
            BY tcrtolrgo.cuenta:
            tcrtolrgo.sldo = 0.
            IF FIRST-OF(tcrtolrgo.cuenta)
            THEN DO:
                tcrtolrgo.sldo = decimal(fSldoCuenta(creditos.agencia,tcrtolrgo.cuenta,iano,imes)).
            END.
        END. /* calcula los saldos de cada cuenta involucrada por agencia */
    END. /* IF FIRST-OF(creditos.agencia) */
    IF FIRST-OF(creditos.tip_credito)
    THEN DO:
        clcuentas = "".
    END.

    ACCUMULATE Creditos.Sdo_Capital (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.INT_corrientes (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.INT_anticipado (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.int_morcobrar (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.INT_difcobro (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.INT_moradifcob (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.honorarios (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.polizas (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    ACCUMULATE Creditos.costas (TOTAL BY creditos.agencia BY creditos.tip_credito BY creditos.cod_credito).
    

    IF LAST-OF(creditos.cod_credito)
    THEN DO:
        desldo = 0.
        FOR EACH tcrtolrgo
            WHERE
                tcrtolrgo.cdgo_prdcto = creditos.cod_credito:
            desldo = desldo + tcrtolrgo.sldo.
            clcuentas = clcuentas + tcrtolrgo.cuenta + "|" + string(tcrtolrgo.sldo) + CHR(10).
        END.
        ACCUMULATE desldo (TOTAL BY creditos.agencia BY creditos.tip_credito).
    END.
    IF LAST-OF(creditos.tip_credito)
    THEN DO:
        DISPLAY creditos.agencia
                "C"
                creditos.tip_credito
                "total " + string(tip_credito) FORMAT "x(20)"
                ACCUM TOTAL BY creditos.tip_credito credito.sdo_capital FORMAT "$->>>,>>>,>>>,>>9" AT 40
                ACCUM TOTAL BY creditos.tip_credito desldo FORMAT "$->>>,>>>,>>>,>>9"  AT 58
                (ACCUM TOTAL BY creditos.tip_credito credito.sdo_capital) - (ACCUM TOTAL BY creditos.tip_credito desldo) FORMAT "$->>>,>>>,>>>,>>9"  AT 91
                WITH STREAM-IO NO-LABELS WIDTH 132 FRAME a DOWN.
        PUT UNFORMATTED clcuentas SKIP. 
    END. /* IF LAST-OF(creditos.tip_credito) */
    IF LAST-OF(creditos.agencia)
    THEN DO:
        DISPLAY creditos.agencia 
                "C"
                " " 
                ACCUM TOTAL BY creditos.agencia credito.sdo_capital FORMAT "$->>>,>>>,>>>,>>9" AT 40
                ACCUM TOTAL BY creditos.agencia desldo FORMAT "$->>>,>>>,>>>,>>9"  AT 58
                (ACCUM TOTAL BY creditos.agencia credito.sdo_capital) - (ACCUM TOTAL BY creditos.agencia desldo) FORMAT "$->>>,>>>,>>>,>>9" AT 91
                WITH STREAM-IO NO-LABELS WIDTH 132 frame b DOWN.
    END.

    IF LAST(creditos.agencia)
    THEN DO:
        DISPLAY "T O T A L " 
                ACCUM TOTAL credito.sdo_capital FORMAT "$->>>,>>>,>>>,>>9" AT 40
                ACCUM TOTAL desldo FORMAT "$->>>,>>>,>>>,>>9" AT 58
                (ACCUM TOTAL credito.sdo_capital) - (ACCUM TOTAL BY creditos.agencia desldo) FORMAT "$->>>,>>>,>>>,>>9" AT 91
                WITH STREAM-IO NO-LABELS WIDTH 132 frame c DOWN.
    END.
END.

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Integridad_PC  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Integridad_PC)
  THEN DELETE WIDGET W-Integridad_PC.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Integridad_PC  _DEFAULT-ENABLE
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
      WITH FRAME F_Integ IN WINDOW W-Integridad_PC.
  ENABLE RECT-314 BUTTON-5 W_CmbOfi BTN-Exportar Btn_Proc Btn_Imp W_FecMes 
         Btn_Done 
      WITH FRAME F_Integ IN WINDOW W-Integridad_PC.
  {&OPEN-BROWSERS-IN-QUERY-F_Integ}
  VIEW W-Integridad_PC.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Guarda_Control W-Integridad_PC 
PROCEDURE Guarda_Control :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_GtiaLib W-Integridad_PC 
PROCEDURE Halla_GtiaLib :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_Gtia = 4.  /*Inicia NoAd Sin LIb*/

  FOR EACH Garantias fields(Num_credito Estado Val_Bien)
        WHERE Garantias.Agencia     EQ Creditos.Agencia
                       AND Garantias.Cod_credito EQ Creditos.Cod_credito NO-LOCK:
        IF  Garantias.Num_credito EQ Creditos.Num_credito
        AND Garantias.Estado      EQ 1
        AND Garantias.Val_Bien    GT 0 THEN DO:
            W_Gtia = 1.
            LEAVE.
        END.
  END.

  IF      Creditos.FOR_pago EQ 2 AND W_Gtia = 1 THEN  /*Ad con Lib*/
          W_Gtia = 1.
  ELSE IF Creditos.FOR_pago NE 2 AND W_Gtia = 1 THEN  /*Ad sinm Lib*/
          W_Gtia = 2.
  ELSE IF Creditos.FOR_pago EQ 2 AND W_Gtia NE 4 THEN   /*No Ad. con Lib.*/
          W_Gtia = 3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Control W-Integridad_PC 
PROCEDURE Imp_Control :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR T_SdoKP LIKE Creditos.Sdo_Capital INIT 0 EXTENT 4.
  DEFI VAR T_SdoKC LIKE Creditos.Sdo_Capital INIT 0 EXTENT 4.
  
  DISPLAY STRING(W_Nom_Entidad,"X(40)") + "   -  Integridad Aplicativo" SKIP
          "                              Fecha : " + STRING(W_FecMes,"99/99/9999") FORM "X(80)"
         SKIP (1)
     WITH WIDTH 150 NO-LABELS.
    
  FOR EACH TCon_Sdos WHERE SdoPro NE 0 OR SdoCta NE 0
                     BREAK BY Ag BY TipP BY ClaP BY Cpto BY CtaPro BY Pto:
      IF FIRST-OF(TCon_Sdos.Ag) THEN DO:
         FIND FIRST Agencias WHERE Agencias.Agencia EQ TCon_Sdos.Ag NO-LOCK NO-ERROR.
      
         DISPLAY
          "Agencia del Control : "
          Agencias.Nombre 
          SKIP (1)
          "Ag. T C Pcto Descrip.Producto     Cpto Cf-G Sdo-del Pdcto   Saldo Cta-Contab Cta-Contable   Diferencia" 
          SKIP
          "--- - - ---- -------------------- ---- ------------------ ------------------ -------------- ---------------"
              WITH WIDTH 150 NO-LABELS.
      END.

      ASSIGN TotCtaX_Pdcto = TotCtaX_Pdcto + SdoPro
             TotCtaX_Cta   = TotCtaX_Cta   + SdoCta.

      /*IF Cpto EQ "SCAP" THEN*/
      ASSIGN T_SdoKP[1] = T_SdoKP[1] + SdoPro
             T_SdoKC[1] = T_SdoKC[1] + SdoCta
             T_SdoKP[2] = T_SdoKP[2] + SdoPro 
             T_SdoKC[2] = T_SdoKC[2] + SdoCta
             T_SdoKP[3] = T_SdoKP[3] + SdoPro
             T_SdoKC[3] = T_SdoKC[3] + SdoCta
             T_SdoKP[4] = T_SdoKP[4] + SdoPro
             T_SdoKC[4] = T_SdoKC[4] + SdoCta. 

      IF LAST-OF(Pto) THEN DO:
         IF TipP EQ "A" THEN
            DISPLAY TCon_Sdos.Ag    FORM "999"
                        TipP        FORM "X(1)"
                        ClaP        FORM "9" 
                        Pto         FORM "Z999"
                        NomPro      FORM "X(20)"
                        Cpto        FORM "X(6)"
                        /*Calif       FORM "99" 
                        GL          FORM "9"*/
                        T_SdoKP[1]  FORM "->>>,>>>,>>>,>>9"
                        
                        T_SdoKC[1]  FORM "->>>>>,>>>,>>>,>>9"
                        " "
                        CtaPro      FORM "X(12)"
                        /*(SdoPro - SdoCta) /*LABEL "Diferencia"*/ FORM "->>>>>>,>>>,>>9"*/  SKIP(0)                        
             WITH DOWN WIDTH 170 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

         ASSIGN T_SdoKP[1] = 0
                T_SdoKC[1] = 0.
      END.

      IF LAST-OF(CtaPro) THEN DO:
         IF TipP EQ "A" THEN 
            RUN Tot1.
      
         ASSIGN TotCtaX_Pdcto = 0
                TotCtaX_Cta   = 0.
      END.

      IF LAST-OF(Cpto) THEN DO:           
         IF TipP EQ "A" THEN
            DISPLAY SKIP(0)
              TCon_Sdos.Ag       FORM "999"  
                     TipP     FORM "X(1)" 
                     ClaP     FORM "9"    
                     
                     "TOTAL CONCEPTO      "      FORM "X(20)"
                     Cpto                        FORM "X(4)" 
                     Calif                       FORM "99"   
                     GL                          FORM "9"    
              T_SdoKP[2]        FORM "->,>>>,>>>,>>>,>>9"                                           
              T_SdoKC[2]        FORM "->,>>>,>>>,>>>,>>9"                                                
              "              "                                                    
              (T_SdoKP[2] - T_SdoKC[2]) FORM "->>>>>>,>>>,>>9"  SKIP(1)                                                                                 
              WITH DOWN WIDTH 150 FRAME FTK2 NO-LABELS NO-BOX USE-TEXT STREAM-IO. 

         ASSIGN T_SdoKP[2] = 0
                T_SdoKC[2] = 0.
      END.

      IF LAST-OF(ClaP) THEN DO:           
         DISPLAY SKIP(0)
                     "                   "
                     "                     --------------------   ---------------------    ------------------" SKIP
           TCon_Sdos.Ag        FORM "999"  
                     TipP      FORM "X(1)" 
                     ClaP      FORM "9"    
                     "TOT.CLASE-PRODUCTO  "      FORM "X(20)"
                     "   "                       FORM "X(4)" 
                     Calif                       FORM "99"   
                     GL                          FORM "9"    
           T_SdoKP[3]      FORM "->,>>>,>>>,>>>,>>9"                                         
           T_SdoKC[3]      FORM "->,>>>,>>>,>>>,>>9"                                                 
           "              "                                                    
           (T_SdoKP[3] - T_SdoKC[3]) FORM "->>>>>>,>>>,>>9"  SKIP(2)                                                                                 
          WITH DOWN WIDTH 150 FRAME FTK3 NO-LABELS NO-BOX USE-TEXT STREAM-IO. 

        ASSIGN T_SdoKP[3] = 0
               T_SdoKC[3] = 0.
      END.

      IF LAST-OF(TipP) THEN DO:
         DISPLAY SKIP(1)
           "                   TOTAL SDOS X PDTO :"
           T_SdoKP[4]    FORM "->,>>>,>>>,>>>,>>9"                                           
           T_SdoKC[4]    FORM "->,>>>,>>>,>>>,>>9"                                                   
           "              "                                                    
           (T_SdoKP[4] - T_SdoKC[4]) FORM "->>>>>>,>>>,>>9"  SKIP(2)                                                                                 
          WITH DOWN WIDTH 150 FRAME FTK4 NO-LABELS NO-BOX USE-TEXT STREAM-IO.             

         ASSIGN T_SdoKP[4] = 0
                T_SdoKC[4] = 0.
      END.

      IF LAST-OF(TCon_Sdos.Ag) AND TipP EQ "C" THEN
         RUN Resumen_Creditos(INPUT TCon_Sdos.Ag).
  END. 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesar W-Integridad_PC 
PROCEDURE Procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Agencias FIELDS(agencia nombre)
        WHERE Agencias.Estado  NE 3
                      AND Agencias.Agencia GE W_OfiIni
                      AND Agencias.Agencia LE W_OfiFin NO-LOCK
                       BY Agencias.Agencia:
      ASSIGN Ag_Proc = Agencias.Agencia
             Ag_Nom  = Agencias.Nombre.
             Tot_Pto = 0.
      RUN Proc_Ahorros.
        
        
      RUN Proc_Creditos.                         
  END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Integridad_PC 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {Incluido\RepEncabezado.I}

  /*ASSIGN W_Reporte    = "Reporte   : Detalle Crediticio Por C/Obligaciòn                       Fecha : " +
                         STRING(W_Fecha,"99/99/9999") + "   Hora :" + STRING(TIME,"HH:MM:SS")
  W_EncColumna = */
        
  RUN Imp_Control.  /*Imprime El Informe*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc_Ahorros W-Integridad_PC 
PROCEDURE Proc_Ahorros :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Integ = "Generando Temporal con Sdos-Ahorros..."
         Tot_Pto = 0.   

  FOR EACH Ahorros FIELDS(sdo_dispon sdo_canje INT_causado INT_pagar tip_ahorro cod_ahorro)
        WHERE Ahorros.Agencia EQ Ag_Proc 
                     AND Ahorros.Estado  EQ 1                 NO-LOCK
                     BREAK BY Ahorros.Tip_Ahorro BY Ahorros.Cod_Ahorro:
      ASSIGN Tot_Pto [1] = Tot_Pto [1] + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
             Tot_Pto [2] = Tot_Pto [2] + Ahorros.INT_Causado
             Tot_Pto [3] = Tot_Pto [3] + Ahorros.INT_Pagar
             W_Cont = W_Cont + 1.
             /* W_Cont:SCREEN-VALUE IN FRAME F_Integ = STRING(W_Cont). */

      IF LAST-OF(Ahorros.Cod_Ahorro) THEN DO:
         IF Tot_Pto [1] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc              
                                   AND TCon_Sdos.TipP    = "A"                           
                                   AND TCon_Sdos.ClaP    = Ahorros.Tip_Ahorro            
                                   AND TCon_Sdos.Cpto    = "SCAP"
                                   AND TCon_Sdos.Pto     = Ahorros.Cod_Ahorro NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoP (INPUT "SCAP").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [1].                    
         END.

         IF Tot_Pto [2] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag   = Ag_Proc
                                AND TCon_Sdos.TipP    = "A"
                                AND TCon_Sdos.ClaP    = Ahorros.Tip_Ahorro  
                                AND TCon_Sdos.Pto     = Ahorros.Cod_Ahorro
                                AND TCon_Sdos.Cpto    = "ICAU" NO-ERROR.
            IF NOT AVAIL(TCon_Sdos) THEN 
               RUN Crea_SdoP (INPUT "ICAU").     /*Al final de este mismo Procedimiento*/
                                   
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [2].
         END.

         IF Tot_Pto [3] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag   = Ag_Proc
                                AND TCon_Sdos.TipP    = "A"
                                AND TCon_Sdos.ClaP    = Ahorros.Tip_Ahorro  
                                AND TCon_Sdos.Pto     = Ahorros.Cod_Ahorro
                                AND TCon_Sdos.Cpto    = "IXPG" NO-ERROR.
            IF NOT AVAIL(TCon_Sdos) THEN 
               RUN Crea_SdoP (INPUT "IXPG").     /*Al final de este mismo Procedimiento*/
                                   
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [3].
         END.
                                   
         Tot_Pto = 0.                      
      END.
  END.
END PROCEDURE.

PROCEDURE Crea_SdoP:
  DEFI INPUT PARAMET W_Cpto LIKE TCon_Sdos.Cpto.

  DEFI VAR W_Cta LIKE TCon_Sdos.CtaPro INIT "Sin Config".

  IF Ahorros.Cod_Ahorro NE 10 THEN
     FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag  = Ag_Proc              
                         AND TCon_Sdos.TipP   = "A"                           
                         AND TCon_Sdos.ClaP   = Ahorros.Tip_Ahorro
                         AND TCon_Sdos.Cpto   = W_Cpto
                         AND TCon_Sdos.Pto    GT 0 NO-ERROR.  
  ELSE
     FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag  = Ag_Proc              
                         AND TCon_Sdos.TipP   = "A"                           
                         AND TCon_Sdos.ClaP   = Ahorros.Tip_Ahorro
                         AND TCon_Sdos.Cpto   = W_Cpto
                         AND TCon_Sdos.Pto    EQ 5 NO-ERROR.

  IF NOT AVAIL(TCon_Sdos) THEN
     FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag   = Ag_Proc              
                            AND TCon_Sdos.TipP = "A"                           
                            AND TCon_Sdos.ClaP = Ahorros.Tip_Ahorro
                            AND TCon_Sdos.Cpto = "SCAP" 
                            AND TCon_Sdos.Pto  GT 0 NO-ERROR.

  IF AVAIL(TCon_Sdos) THEN 
     W_Cta = TCon_Sdos.CtaPro.
  
  FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                           AND Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
  CREATE TCon_Sdos.                                 
  ASSIGN TCon_Sdos.Ag     = Ag_Proc                 
         TCon_Sdos.TipP   = "A" 
         TCon_Sdos.ClaP   = Ahorros.Tip_Ahorro
         TCon_Sdos.Cpto   = W_Cpto
         TCon_Sdos.Pto    = Ahorros.Cod_Ahorro      
         TCon_Sdos.CtaPro = W_Cta
         TCon_Sdos.SdoCta = 0.

  ASSIGN TCon_Sdos.NomPro = Pro_Ahorros.Nom_producto WHEN AVAIL(Pro_Ahorros). 
END PROCE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc_Creditos W-Integridad_PC 
PROCEDURE Proc_Creditos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_TsdoK LIKE Creditos.Sdo_Capital INIT 0 EXTENT 4.
  DEFI VAR K       AS INTEG FORM "9".

  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Integ = "Generando Temporal con Sdos-Creditos..."
         Tot_Pto = 0
         W_TsdoK = 0. 

  FOR EACH Creditos FIELDS(sdo_capital INT_corrientes INT_anticipado INT_morcobrar INT_difcobro INT_moradifcob honorarios polizas costas tip_credito cod_credito cod_califica FOR_Pago agencia num_credito)
        WHERE Creditos.Agencia EQ Ag_Proc 
                      AND Creditos.Estado  EQ 2 NO-LOCK
                     BREAK BY Creditos.Tip_Credito  BY Creditos.Cod_Credito
                           BY Creditos.Cod_Califica BY Creditos.FOR_Pago:
      RUN Halla_GtiaLib.

      IF      W_Gtia EQ 1 THEN
         W_TsdoK [1] = W_TsdoK[1] + Creditos.Sdo_Capital.
      ELSE IF W_Gtia EQ 2 THEN
         W_TsdoK [2] = W_TsdoK[2] + Creditos.Sdo_Capital.
      ELSE IF W_Gtia EQ 3 THEN
         W_TsdoK [3] = W_TsdoK[3] + Creditos.Sdo_Capital.
      ELSE 
         W_TsdoK [4] = W_TsdoK[4] + Creditos.Sdo_Capital.

      ASSIGN /*W_TsdoK     = W_TsdoK + Creditos.Sdo_Capital*/
             Tot_Pto [2] = Tot_Pto [2] + Creditos.INT_Corrientes
             Tot_Pto [3] = Tot_Pto [3] + Creditos.INT_Anticipado
             Tot_Pto [4] = Tot_Pto [4] + Creditos.INT_MorCobrar
             Tot_Pto [5] = Tot_Pto [5] + Creditos.INT_DifCobro + Creditos.INT_MoraDifCob
             Tot_Pto [6] = Tot_Pto [6] + Creditos.Honorarios
             Tot_Pto [7] = Tot_Pto [7] + Creditos.Polizas
             Tot_Pto [8] = Tot_Pto [8] + Creditos.Costas
             W_Cont = W_Cont + 1.
             /* W_Cont:SCREEN-VALUE IN FRAME F_Integ = STRING(W_Cont). */

      IF LAST-OF(Creditos.Cod_Califica) THEN DO:   /*Solo K*/
         DO K = 1 TO 4:
            IF W_TsdoK[K] NE 0 THEN DO:
               FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag    = Ag_Proc             
                                    AND TCon_Sdos.TipP    = "C"                           
                                    AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                    AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                    AND TCon_Sdos.Calif   = Creditos.Cod_Califica
                                    AND TCon_Sdos.GL      = W_Gtia
                                    AND TCon_Sdos.Cpto    = "SCAP" NO-ERROR.              
               IF NOT AVAIL(TCon_Sdos) THEN DO:                                                
                  RUN Crea_SdoCre (INPUT "SCAP").     /*Al final de este mismo Procedimiento*/
                  ASSIGN TCon_Sdos.Calif = Creditos.Cod_Califica
                         TCon_Sdos.GL    = W_Gtia.
               END.

               ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + W_TsdoK[K].                    
            END.
         END.

         W_TsdoK = 0.
      END.

      IF LAST-OF(Creditos.Cod_Credito) THEN DO:  /*Menos K*/
         IF Tot_Pto [2] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "ICAU" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "ICAU").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [2].                    
         END.

         IF Tot_Pto [3] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "IXPG" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "IXPG").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [3].                    
         END.
         
         IF Tot_Pto [4] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "IMOR" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "IMOR").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [4].                    
         END.

         IF Tot_Pto [5] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "IDIF" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "IDIF").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [5].                    
         END.

         IF Tot_Pto [6] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "SHON" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "SHON").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [6].                    
         END.

         IF Tot_Pto [7] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "SPOL" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "SPOL").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [7].                    
         END.
         
         IF Tot_Pto [8] NE 0 THEN DO:
            FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Ag_Proc             
                                   AND TCon_Sdos.TipP    = "C"                           
                                   AND TCon_Sdos.ClaP    = Creditos.Tip_Credito            
                                   AND TCon_Sdos.Pto     = Creditos.Cod_Credito
                                   AND TCon_Sdos.Cpto    = "SCOS" NO-ERROR.              
            IF NOT AVAIL(TCon_Sdos) THEN                                                 
               RUN Crea_SdoCre (INPUT "SCOS").     /*Al final de este mismo Procedimiento*/
                                                                                         
            ASSIGN TCon_Sdos.SdoPro = TCon_Sdos.SdoPro + Tot_Pto [8].                    
         END.  

         ASSIGN Tot_Pto = 0. 
      END.
  END.
END PROCEDURE.

PROCEDURE Crea_SdoCre:
  DEFI INPUT PARAMET W_Cpto LIKE TCon_Sdos.Cpto.

  DEFI VAR W_Cta LIKE TCon_Sdos.CtaPro INIT "Sin Config".

  FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag    = Ag_Proc              
                         AND TCon_Sdos.TipP  = "C"                           
                         AND TCon_Sdos.ClaP  = Creditos.Tip_Credito
                         AND TCon_Sdos.Calif = Creditos.Cod_Califica
                         AND TCon_Sdos.Cpto  = W_Cpto NO-ERROR.              
  IF NOT AVAIL(TCon_Sdos) THEN
     FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag = Ag_Proc              
                         AND TCon_Sdos.TipP  = "C"                           
                         AND TCon_Sdos.ClaP  = Creditos.Tip_Credito
                         AND TCon_Sdos.Cpto  = W_Cpto NO-ERROR.

  IF NOT AVAIL(TCon_Sdos) THEN
     FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag   = Ag_Proc              
                            AND TCon_Sdos.TipP = "C"                           
                            AND TCon_Sdos.ClaP = Creditos.Tip_Credito
                            AND TCon_Sdos.Cpto NE "SCAP" NO-ERROR.
  IF AVAIL(TCon_Sdos) THEN
     W_Cta = TCon_Sdos.CtaPro.

  FIND FIRST Pro_Credito WHERE Pro_Credito.Cod_Credito EQ Creditos.Cod_Credito
                           AND Pro_Credito.Estado      EQ 1 NO-LOCK NO-ERROR.
  CREATE TCon_Sdos.                                 
  ASSIGN TCon_Sdos.Ag     = Ag_Proc                 
         TCon_Sdos.TipP   = "C" 
         TCon_Sdos.ClaP   = Creditos.Tip_Credito
         TCon_Sdos.Cpto   = W_Cpto
         TCon_Sdos.Pto    = Creditos.Cod_Credito      
         TCon_Sdos.CtaPro = W_Cta
         TCon_Sdos.SdoCta = 0.

  ASSIGN TCon_Sdos.NomPro = Pro_Credito.Nom_Producto WHEN AVAIL(Pro_Creditos).
END PROCE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_Creditos W-Integridad_PC 
PROCEDURE Resumen_Creditos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI INPUT PARAM W_AgCr LIKE TCon_Sdos.Ag.

  DEFI VAR T_SdoKP LIKE Creditos.Sdo_Capital INIT 0 EXTENT 2.
  DEFI VAR T_SdoKC LIKE Creditos.Sdo_Capital INIT 0 EXTENT 2.

  DISPLAY "Resumen Créditos: " WITH FRAME Cred1 NO-LABELS NO-BOX.
  FOR EACH TCon_Sdos WHERE TCon_Sdos.Ag      EQ W_AgCr AND
                                     TipP    EQ "C"    AND 
                                     (SdoPro NE 0 OR SdoCta NE 0)
                     BREAK BY Ag BY TipP BY Cpto BY CtaPro BY Calif BY Pto:
      ASSIGN T_SdoKP[1] = T_SdoKP[1] + SdoPro
             T_SdoKC[1] = T_SdoKC[1] + SdoCta.

      IF LAST-OF(Cpto) THEN DO:           
         DISPLAY 
           TCon_Sdos.Ag        FORM "999"  
                     TipP      FORM "X(1)" 
                     ClaP      FORM "9"    
                     "TOTAL CONCEPTO      "      FORM "X(20)"
                     Cpto                        FORM "X(4)" 
                     Calif                       FORM "99"   
                     GL                          FORM "9"    
           T_SdoKP[1]    FORM "->,>>>,>>>,>>>,>>9"                                           
           T_SdoKC[1]    FORM "->,>>>,>>>,>>>,>>9"                                                    
           "              "                                                    
           (T_SdoKP[1] - T_SdoKC[1]) FORM "->>>>>>,>>>,>>9"  SKIP(0)                                                                                 
          WITH DOWN WIDTH 150 FRAME O_FTK2 NO-LABELS NO-BOX USE-TEXT STREAM-IO. 

        ASSIGN T_SdoKP[1] = 0
               T_SdoKC[1] = 0.
      END.
  END.
  DISPLAY SKIP(3) WITH FRAME Cred2 NO-LABELS NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SdosXCta W-Integridad_PC 
PROCEDURE SdosXCta :
/*------------------------------------------------------------------------------
  Purpose:                        
  Parameters:  <none>             
  Notes:                          
------------------------------------------------------------------------------*/
  Msaje:SCREEN-VALUE IN FRAME F_Integ = "Generando Temporal con Sdos-Contables...".  
        
  FOR EACH Agencias FIELDS(agencia)
        WHERE Agencias.Estado  NE 3
                      AND Agencias.Agencia GE W_OfiIni
                      AND Agencias.Agencia LE W_OfiFin NO-LOCK
                       BY Agencias.Agencia:
      FOR EACH TempCtas WHERE TempCtas.CtaPro GT " " BREAK BY TempCtas.CtaPro:
          IF FIRST-OF(TempCtas.CtaPro) THEN
             RUN Halla_CtaPro (INPUT TempCtas.CtaPro,"SCAP").   /*Al final de este mismo procedimiento*/
      END.

      FOR EACH TempCtas WHERE TempCtas.CtaLiq GT " " BREAK BY TempCtas.CtaLiq:
          IF FIRST-OF(TempCtas.CtaLiq) THEN
             RUN Halla_CtaPro (INPUT TempCtas.CtaLiq,"ICAU").   
      END.

      FOR EACH TempCtas WHERE TempCtas.IntAnt GT " " BREAK BY TempCtas.IntAnt:
          IF FIRST-OF(TempCtas.IntAnt) THEN
             RUN Halla_CtaPro (INPUT TempCtas.IntAnt,"IXPG").   
      END.

      FOR EACH TempCtas WHERE TempCtas.IntMor GT " " BREAK BY TempCtas.IntMor:
          IF FIRST-OF(TempCtas.IntMor) THEN
             RUN Halla_CtaPro (INPUT TempCtas.IntMor,"IMOR").   
      END.

      FOR EACH TempCtas WHERE TempCtas.DifCoD GT " " BREAK BY TempCtas.DifCoD:
          IF FIRST-OF(TempCtas.DifCoD) THEN
             RUN Halla_CtaPro (INPUT TempCtas.DifCoD,"IDIF").   
      END.

      FOR EACH TempCtas WHERE TempCtas.CtaHon GT " " BREAK BY TempCtas.CtaHon:
          IF FIRST-OF(TempCtas.CtaHon) THEN
             RUN Halla_CtaPro (INPUT TempCtas.CtaHon,"SHON").   
      END.

      FOR EACH TempCtas WHERE TempCtas.CtaPol GT " " BREAK BY TempCtas.CtaPol:
          IF FIRST-OF(TempCtas.CtaPol) THEN
             RUN Halla_CtaPro (INPUT TempCtas.CtaPol,"SPOL").   
      END.

      FOR EACH TempCtas WHERE TempCtas.CtaCos GT " " BREAK BY TempCtas.CtaCos:
          IF FIRST-OF(TempCtas.CtaCos) THEN
             RUN Halla_CtaPro (INPUT TempCtas.CtaCos,"SCOS").   
      END.
  END.  
    
END PROCEDURE.

PROCEDURE Halla_CtaPro:
  DEFI INPUT PARAMET W_Cta  LIKE Cuentas.Cuenta.
  DEFI INPUT PARAMET W_Cpto LIKE TCon_Sdos.Cpto.

  DEFI VAR P_SdoAct LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI VAR K        AS INTEG FORM "99".
    
  ASSIGN W_Cont = W_Cont + 1
         /* W_Cont:SCREEN-VALUE IN FRAME F_Integ = STRING(W_Cont) */. 

  FIND FIRST TCon_Sdos WHERE TCon_Sdos.Ag      = Agencias.Agencia
                         AND TCon_Sdos.TipP    = TempCtas.TipP 
                         AND TCon_Sdos.ClaP    = TempCtas.ClaP
                         AND TCon_Sdos.CtaPro  = W_Cta NO-ERROR.        
  IF NOT AVAIL(TCon_Sdos) THEN DO:            
     CREATE TCon_Sdos.                                                                                              
     ASSIGN TCon_Sdos.Ag     = Agencias.Agencia                                                         
            TCon_Sdos.TipP   = TempCtas.TipP                                                            
            TCon_Sdos.ClaP   = TempCtas.ClaP                                                            
            TCon_Sdos.Cpto   = W_Cpto                                                                   
            TCon_Sdos.Pto    = TempCtas.Pto                                                             
            TCon_Sdos.CtaPro = W_Cta                                                          
            TCon_Sdos.NomPro = TempCtas.NomPro                                                          
            TCon_Sdos.SdoPro = 0
            TCon_Sdos.Calif  = TempCtas.Calif
            TCon_Sdos.GL     = TempCtas.GL 
            P_SdoAct         = 0.
                                                                                                        
     FOR EACH Sal_Cuenta fields(sal_inicial cuenta db cr)
            WHERE Sal_Cuenta.Agencia EQ Agencias.Agencia                                 
                           AND Sal_Cuenta.Cuenta  EQ W_Cta                                  
                           AND Sal_Cuenta.Ano     EQ YEAR(W_FecMes) NO-LOCK:
         ASSIGN TCon_Sdos.SdoCta = TCon_Sdos.SdoCta + Sal_Cuenta.Sal_Inicial.

         DO K = 1 TO MONTH(W_FecMes) BY 1:                                                                  
            FIND CUENTAS WHERE CUENTAS.CUENTA EQ Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.                        
            IF Cuentas.Natur EQ "Db" THEN                                                                   
               ASSIGN P_SdoAct = P_SdoAct + (Db[K] - Cr[K]).
            ELSE                                                                                            
               ASSIGN P_SdoAct = P_SdoAct + (Cr[K] - Db[K]).
         END.
     END.

     ASSIGN TCon_Sdos.SdoCta = TCon_Sdos.SdoCta + P_SdoAct.
  END.  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tot1 W-Integridad_PC 
PROCEDURE Tot1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DISPLAY TCon_Sdos.Ag        FORM "999"       
                    TipP      FORM "X(1)" 
                    ClaP      FORM "9"    
                    " TOT.CUENTA-CONTABLE"        FORM "X(20)"
                    Cpto                          FORM "X(4)" 
                    Calif                         FORM "99"   
                    GL                            FORM "9"    
                    TotCtaX_Pdcto     FORM "->,>>>,>>>,>>>,>>9"                                          
                    TotCtaX_Cta       FORM "->,>>>,>>>,>>>,>>9"                                                
                    "              "                                                    
                    (TotCtaX_Pdcto - TotCtaX_Cta) FORM "->>>>>>,>>>,>>9"              
                    SKIP(1)                                                           
      WITH DOWN WIDTH 150 FRAME F2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tot2 W-Integridad_PC 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W-Integridad_PC 
PROCEDURE Valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
    Msaje:SCREEN-VALUE IN FRAME F_Integ = "Generando Temporal con Configuraciones...". 
    FOR EACH Pro_Ahorros FIELDS(cod_ahorro tip_ahorro nom_prod)
        WHERE 
            Pro_Ahorros.Estado EQ 1 NO-LOCK 
                          BY Pro_Ahorros.Tip_Ahorro BY Pro_Ahorros.Cod_Ahorro:
        FOR EACH CortoLargo FIELDS(cod_producto cta_asoad)
                WHERE 
                    CortoLargo.Agencia        EQ 1
                            AND CortoLargo.Clase_Producto EQ 1
                            AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro NO-LOCK
                                BY CortoLargo.Plazo_Inicial:
            FIND FIRST TempCtas WHERE TempCtas.TipP   = "A"
                                AND TempCtas.ClaP   = Pro_Ahorros.Tip_Ahorro
                                AND TempCtas.Pto    = CortoLargo.Cod_Producto
                                AND TempCtas.CtaPro = CortoLargo.Cta_AsoAd NO-ERROR.
            IF NOT AVAIL(TempCtas) THEN DO:
                CREATE TempCtas.
                ASSIGN TempCtas.TipP   = "A"
                        TempCtas.ClaP   = Pro_Ahorros.Tip_Ahorro
                        TempCtas.NomPro = Pro_Ahorros.Nom_prod
                        TempCtas.Pto    = CortoLargo.Cod_Producto
                        TempCtas.CtaPro = CortoLargo.Cta_AsoAd. 
            END.

            IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN NEXT.

            FIND FIRST Liqui_Int
                    WHERE Liqui_Int.Clase_Producto EQ 1
                                 AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
          IF NOT AVAIL(Liqui_Int) THEN DO:
             MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro   SKIP
                        VIEW-AS ALERT-BOX ERROR.
             RETURN ERROR.
          END.

          ASSIGN TempCtas.CtaLiq = Liqui_Int.Cta_CauCr         /*Los Causados*/   
                 TempCtas.IntAnt = Liqui_Int.CtaCr_LiqAso.     /*Los Por Pagar*/
      END.
  END.

  CREATE TempCtas.
  ASSIGN TempCtas.TipP   = "A"
         TempCtas.ClaP   = 4            
         TempCtas.NomPro = "Capital Min.Irred."           
         TempCtas.Pto    = 99                      
         TempCtas.CtaPro = "31050512".               
    
  FOR EACH Pro_Creditos FIELDS (Cod_Credito Tip_Credito Nom_prod)
        WHERE Pro_Creditos.Estado EQ 1 NO-LOCK 
                           BY Pro_Creditos.Tip_Credito BY Pro_Creditos.Cod_Credito:
      FOR EACH CortoLargo FIELDS(Cta_AsoAd Cta_HonorariosDB Cta_PolizasDB Cta_CostasDB Cta_VigGarAd Cta_ContrapartidaGar Cod_Producto)
            WHERE CortoLargo.Agencia        EQ 1
                            AND CortoLargo.Clase_Producto EQ 2
                            AND CortoLargo.Cod_Producto   EQ Pro_Creditos.Cod_Credito NO-LOCK
                                BY CortoLargo.Plazo_Inicial:
          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CortoLargo.Cod_Producto
                                AND TempCtas.CtaPro = CortoLargo.Cta_AsoAd NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             CREATE TempCtas.                                        
             ASSIGN TempCtas.TipP   = "C"                            
                    TempCtas.ClaP   = Pro_Creditos.Tip_Credito       
                    TempCtas.Pto    = Pro_Creditos.Cod_Credito       
                    TempCtas.NomPro = Pro_Creditos.Nom_prod          
                    TempCtas.CtaPro = CortoLargo.Cta_AsoAd           
                    TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB    
                    TempCtas.CtaPol = CortoLargo.Cta_PolizasDB       
                    TempCtas.CtaCos = CortoLargo.Cta_CostasDB        
                    TempCtas.CtaGar = CortoLargo.Cta_VigGarAd        
                    TempCtas.CtaCGa = CortoLargo.Cta_ContrapartidaGar
                    TempCtas.Calif  = 1
                    TempCtas.GL     = 4.
          END.             

          FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2                                        
                                 AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.   
          IF NOT AVAIL(Liqui_Int) THEN DO:                                                                   
             MESSAGE "Falta Liqui_Int Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito   SKIP   
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
                WHERE SUBSTRING(Cuentas.Cuenta,1,4)    EQ "8120" AND  /*Int-DifCobro*/
                                           Cuentas.Estado         EQ 1      AND
                                           Cuentas.Tipo           EQ 2 NO-LOCK:
              FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                    AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                    AND TempCtas.Pto    = Pro_Creditos.Cod_Credito
                                    AND TempCtas.DifCoD = Cuentas.Cuenta NO-ERROR.
              IF NOT AVAIL(TempCtas) THEN DO:
                 RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
                 ASSIGN TempCtas.Calif  = 1
                        TempCtas.DifCoD = Cuentas.Cuenta
                        TempCtas.GL     = 4.
              END.                              
          END.
      END.
      FOR EACH CarteraVencida fields(Cod_Producto Cta_AsoAdDb Cod_Califica Cta_NoaAdDb CtaCal_Costas Cta_AsoNaDb Cta_NoaNaDb CtaCal_Interes)
            WHERE CarteraVencida.Cod_Producto EQ Pro_Creditos.Cod_Credito NO-LOCK
                                 BY Cod_Califica:
          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaPro = CarteraVencida.Cta_AsoAdDb NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
             ASSIGN TempCtas.Calif  = CarteraVencida.Cod_Califica
                    TempCtas.CtaPro = CarteraVencida.Cta_AsoAdDb
                    TempCtas.GL     = 1.
          END.

          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaPro = CarteraVencida.Cta_NoaAdDb NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
             ASSIGN TempCtas.Calif  = CarteraVencida.Cod_Califica
                    TempCtas.CtaPro = CarteraVencida.Cta_NoaAdDb
                    TempCtas.GL     = 2.
          END.

          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaPro = CarteraVencida.Cta_AsoNaDb NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
             ASSIGN TempCtas.Calif  = CarteraVencida.Cod_Califica
                    TempCtas.CtaPro = CarteraVencida.Cta_AsoNaDb
                    TempCtas.GL     = 3.
          END.

          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaPro = CarteraVencida.Cta_NoaNaDb NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
             ASSIGN TempCtas.Calif  = CarteraVencida.Cod_Califica
                    TempCtas.CtaPro = CarteraVencida.Cta_NoaNaDb
                    TempCtas.GL     = 4.
          END.

          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaLiq = CarteraVencida.CtaCal_Interes NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
             ASSIGN TempCtas.Calif  = CarteraVencida.Cod_Califica
                    TempCtas.CtaLiq = CtaCal_Interes
                    TempCtas.GL     = 4.
          END.

          FIND FIRST TempCtas WHERE TempCtas.TipP   = "C"
                                AND TempCtas.ClaP   = Pro_Creditos.Tip_Credito
                                AND TempCtas.Pto    = CarteraVencida.Cod_Producto
                                AND TempCtas.CtaCos = CarteraVencida.CtaCal_Costas NO-ERROR.
          IF NOT AVAIL(TempCtas) THEN DO:
             RUN CtasCred.   /*Procedim.abajo de este mismo Procedim.*/
             ASSIGN TempCtas.Calif  = CarteraVencida.Cod_Califica
                    TempCtas.CtaCos = CarteraVencida.CtaCal_Costas
                    TempCtas.GL     = 4.
          END.
      END.
  END.    

END PROCEDURE.

PROCEDURE CtasCred:
  CREATE TempCtas.                                        
  ASSIGN TempCtas.TipP   = "C"                            
         TempCtas.ClaP   = Pro_Creditos.Tip_Credito       
         TempCtas.Pto    = Pro_Creditos.Cod_Credito        
         TempCtas.NomPro = Pro_Creditos.Nom_prod.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE x_valida W-Integridad_PC 
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
                    TempCtas.TipP   = "A"
            AND TempCtas.ClaP   = Pro_Ahorros.Tip_Ahorro
            AND TempCtas.Pto    = CortoLargo.Cod_Producto
            AND TempCtas.CtaPro = CortoLargo.Cta_AsoAd NO-ERROR.
            IF NOT AVAIL(TempCtas) 
            THEN DO:
                CREATE TempCtas.
                ASSIGN  TempCtas.TipP   = "A"
                        TempCtas.ClaP   = Pro_Ahorros.Tip_Ahorro
                        TempCtas.NomPro = Pro_Ahorros.Nom_prod
                        TempCtas.Pto    = CortoLargo.Cod_Producto
                        TempCtas.CtaPro = CortoLargo.Cta_AsoAd. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCuentasDeAhorros W-Integridad_PC 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSldoCuenta W-Integridad_PC 
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

