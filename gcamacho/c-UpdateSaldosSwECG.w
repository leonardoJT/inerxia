&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


/* {Incluido/Variable.I "SHARED"} */

DEFINE VARIABLE W_Fecha AS DATE  INITIAL TODAY NO-UNDO.
DEFINE VARIABLE W_usuario AS CHARACTER INITIAL "1041" NO-UNDO. /*Operador Nocturno*/
DEFINE VARIABLE W_PathSpl AS CHARACTER INITIAL "c:\info_juriscoop\"  NO-UNDO.

DEFINE VARIABLE viNumReg AS INTEGER     NO-UNDO.
DEFINE VARIABLE Contador AS INTEGER     NO-UNDO.
DEFINE VARIABLE Hora1    AS INTEGER     NO-UNDO.
DEFINE VARIABLE vdtHorIni AS DATETIME   NO-UNDO.
DEFINE VARIABLE vdtHorFin AS DATETIME   NO-UNDO.

DEFINE VARIABLE vcCodEntidad  AS CHARACTER INITIAL "00000005"  NO-UNDO.
DEFINE VARIABLE vdaFecAct AS DATE INITIAL TODAY NO-UNDO.

DEFINE VARIABLE viCntClientes   AS INTEGER     NO-UNDO. /*Contador registros a cargar Clientes */
DEFINE VARIABLE viCntCuentas    AS INTEGER     NO-UNDO. /*Contador registros a cargar Cuentas  */
DEFINE VARIABLE viCntCupos      AS INTEGER     NO-UNDO. /*Contador registros a cargar Cupos X canal- Cuenta  */
DEFINE VARIABLE viCntTarjetas   AS INTEGER     NO-UNDO. /*Contador registros a cargar Tarjetas */
DEFINE VARIABLE viCntTarCuenta  AS INTEGER     NO-UNDO. /*Contador registros a cargar Tarjeta Vs. Cuenta*/


DEFINE TEMP-TABLE TPrint
    FIELDS  tipo    AS INTEGER
    FIELDS  cadena  AS CHARACTER FORMAT "X(500)"
    FIELDS  fecha   AS CHARACTER FORMAT "99999999"
    INDEX idx tipo.

DEFINE TEMP-TABLE TClientes
    FIELDS  Signo       AS CHARACTER    INITIAL "+"
    FIELDS  nit         LIKE    clientes.nit
    FIELDS  tpNit       AS INTEGER /*0. Cédula, 1. Ced. Extranjeria, 2. Tarj. Identidad 3. Pasaporte*/
    FIELDS  nombre      LIKE clientes.nombre
    FIELDS  nombre2     LIKE clientes.nombre
    FIELDS  apellido1   LIKE clientes.apellido1
    FIELDS  apellido2   LIKE clientes.apellido2
    FIELDS  dirCasa     AS CHARACTER
    FIELDS  dirTrabajo  AS CHARACTER
    FIELDS  telCasa     AS INT64
    FIELDS  telTrabajo  AS INT64
    FIELDS  telMovil    AS INT64
    FIELDS  fecNacim    AS CHARACTER
    FIELDS  genero      AS CHARACTER /*F o M*/
    FIELDS  correoe     AS CHARACTER
    FIELDS  paisResid   AS CHARACTER
    FIELDS  DeptoResid  AS CHARACTER
    FIELDS  CiudadResid AS CHARACTER
    FIELDS  paisNacim   AS CHARACTER
    FIELDS  DeptoNacim  AS CHARACTER
    FIELDS  CiudadNacim AS CHARACTER
    FIELDS  nivelAcceso AS CHARACTER INITIAL "0000"
    FIELDS  fecAct      AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX  nit IS UNIQUE nit.

DEFINE TEMP-TABLE TCuentas
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER 
    FIELDS nit          LIKE ahorros.nit
    FIELDS cuenta       AS CHARACTER FORMAT "9999999999999"
    FIELDS tpProd       AS INTEGER /*10. Ahorros; 50. Cupo Rotativo*/
    FIELDS CtaDefault   AS INTEGER  /*0. No; 1. Si*/
    FIELDS Constante    AS INTEGER INITIAL 170
    FIELDS sdoTotal     AS CHARACTER 
    FIELDS sdoDispon    AS CHARACTER
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit cuenta.

DEFINE TEMP-TABLE TCupos
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER 
    FIELDS nit          LIKE ahorros.nit
    FIELDS cuenta       AS CHARACTER FORMAT "999999999999"
    FIELDS canal        AS CHARACTER
    FIELDS NumTxCanal   AS CHARACTER
    FIELDS ValTxCanal   AS CHARACTER
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit cuenta.

DEFINE TEMP-TABLE TTarjetas
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER INITIAL "00000005"
    FIELDS nit          LIKE ahorros.nit
    FIELDS tarjetaDB    LIKE ahorros.tarjetaDB
    FIELDS estadoTDB    AS CHARACTER INITIAL "00"       
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit tarjetaDB .

DEFINE TEMP-TABLE TTDBCta
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER INITIAL "00000005"
    FIELDS nit          LIKE ahorros.nit
    FIELDS tarjetaDB    LIKE ahorros.tarjetaDB
    FIELDS cuenta       AS CHARACTER FORMAT "999999999999"
    FIELDS tpProd       AS INTEGER /*10. Ahorros; 50. Cupo Rotativo*/
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit tarjetaDB .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-274 RECT-1 RECT-278 BUTTON-1 F-fecha ~
B-Proceso BtnDone-2 BUTTON-95 
&Scoped-Define DISPLAYED-OBJECTS F-fecha F-Estado F-Hora1 F-Hora2 NumReg ~
F-Tiempo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 BUTTON-1 Btn_Consulta BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD decToChar C-Win 
FUNCTION decToChar RETURNS CHARACTER
    (INPUT ipdeValor AS DECIMAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFecToInt C-Win 
FUNCTION getFecToInt RETURNS INT64
    (INPUT ipdaFecha    AS DATE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Proceso 
     LABEL "Procesar" 
     SIZE 13 BY 1.65 TOOLTIP "Procesar".

DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 58.72 BY 1.12.

DEFINE BUTTON BtnDone-2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir" 
     SIZE 13 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Consultar" 
     SIZE 13 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Información" 
     SIZE 13 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE F-Estado AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 58 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Cierre" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE F-Hora1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hora Inicio" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Hora2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hora Final" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Tiempo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tiempo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NumReg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Reg.Procesados" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 12.38.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 12.38.

DEFINE RECTANGLE RECT-278
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 7.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN-Titulo AT ROW 1.31 COL 14.43 WIDGET-ID 68
     BUTTON-1 AT ROW 3.15 COL 88 WIDGET-ID 58
     Btn_Consulta AT ROW 4.77 COL 88 WIDGET-ID 54
     F-fecha AT ROW 5.58 COL 29 COLON-ALIGNED WIDGET-ID 134
     B-Proceso AT ROW 6.38 COL 88 WIDGET-ID 56
     F-Estado AT ROW 11.77 COL 17 COLON-ALIGNED WIDGET-ID 92
     F-Hora1 AT ROW 12.85 COL 17 COLON-ALIGNED WIDGET-ID 94
     F-Hora2 AT ROW 12.85 COL 40 COLON-ALIGNED WIDGET-ID 96
     NumReg AT ROW 12.85 COL 66 COLON-ALIGNED WIDGET-ID 102
     BtnDone-2 AT ROW 13.12 COL 88 WIDGET-ID 50
     F-Tiempo AT ROW 13.92 COL 17 COLON-ALIGNED WIDGET-ID 132
     BUTTON-95 AT ROW 15.27 COL 93 WIDGET-ID 60
     "EXPORTAR SALDOS PRODUCTOS TARJETA DEBITO" VIEW-AS TEXT
          SIZE 48 BY .81 AT ROW 1.46 COL 19.72 WIDGET-ID 138
          BGCOLOR 11 FONT 1
     RECT-274 AT ROW 2.62 COL 87 WIDGET-ID 66
     RECT-1 AT ROW 2.62 COL 1.29 WIDGET-ID 2
     RECT-278 AT ROW 3.15 COL 19 WIDGET-ID 136
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.57 BY 15.62
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Exportar Saldos a Switch"
         HEIGHT             = 15.62
         WIDTH              = 101.57
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 109.72
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 109.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME DEFAULT-FRAME
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN F-Estado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Hora1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Hora2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Tiempo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NumReg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Exportar Saldos a Switch */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Exportar Saldos a Switch */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Proceso C-Win
ON CHOOSE OF B-Proceso IN FRAME DEFAULT-FRAME /* Procesar */
DO:

    ASSIGN F-Hora1:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(TIME,"HH:MM:SS")
        Hora1 = TIME
        vdtHorIni = NOW
        F-Tiempo:SCREEN-VALUE = "0".    
/*     DISPLAY F-Hora1 WITH FRAME fmain. */

    EMPTY TEMP-TABLE TCuentas.
    EMPTY TEMP-TABLE TClientes.
    EMPTY TEMP-TABLE TCupos.    
    EMPTY TEMP-TABLE TTarjetas. 
    EMPTY TEMP-TABLE TTDBCta.   



    RUN GeneraTemp.
    RUN creaFiles.

    ASSIGN F-Hora2:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(TIME,"HH:MM:SS").
    MESSAGE "Proceso Terminado."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 C-Win
ON CHOOSE OF BtnDone-2 IN FRAME DEFAULT-FRAME /* Salir */
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Información */
DO:
    RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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

  ASSIGN F-fecha:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(TODAY)
        vdaFecAct = TODAY.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaFiles C-Win 
PROCEDURE creaFiles :
DEFINE VARIABLE vcNomPath AS CHARACTER INITIAL "c:\info_Juriscoop\" NO-UNDO.
    DEFINE VARIABLE vcNomFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcNomFileCl     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileTar    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileCta    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileCupos  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileTarCta AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileCtrl   AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE vcComandoFiles  AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE VARIABLE vcComando       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.


    ASSIGN vcNomFile = /*vcNomPath +*/ STRING(getFecToInt(vdaFecAct)) + "." + vcCodEntidad + "."
        vcNomFileCl     = vcNomFile + "FCL"
        vcNomFileTar    = vcNomFile + "FCA"
        vcNomFileCta    = vcNomFile + "FAC"
        vcNomFileCupos  = vcNomFile + "FACH"
        vcNomFileTarCta = vcNomFile + "FCVA"
        vcNomFileCtrl   = vcNomFile + "F".

OUTPUT TO VALUE(vcNomPath + vcNomFileCl).
    FOR EACH  TClientes NO-LOCK:
        ASSIGN F-Estado:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "Creando Tabla Temporal Clientes: " + STRING(TClientes.nit).
        CREATE TPrint.
        UPDATE tipo = 1
               cadena = TRIM(STRING(Signo))         + "," +
                        TRIM(STRING(nit,"x(15)"))   + "," +
                        TRIM(STRING(tpNit))         + "," +
                        TRIM(REPLACE(STRING(nombre),    ",", " "))        + "," +
                        TRIM(REPLACE(STRING(nombre2),   ",", " "))       + "," +
                        TRIM(REPLACE(STRING(apellido1), ",", " "))     + "," +
                        TRIM(REPLACE(STRING(apellido2), ",", " "))     + "," +
                        TRIM(STRING(dirCasa))       + "," +
                        TRIM(STRING(dirTrabajo))    + "," +
                        TRIM(STRING(telCasa))       + "," +
                        TRIM(STRING(telTrabajo))    + "," +
                        TRIM(STRING(telMovil))      + "," +
                        TRIM(STRING(fecNacim))      + "," +
                        TRIM(STRING(genero))        + "," +
                        TRIM(STRING(correoe))       + "," +
                        TRIM(STRING(paisResid))     + "," +
                        TRIM(STRING(DeptoResid))    + "," +
                        TRIM(STRING(CiudadResid))   + "," +
                        TRIM(STRING(paisNacim))     + "," +
                        TRIM(STRING(DeptoNacim))    + "," +
                        TRIM(STRING(CiudadNacim))   + "," +
                        TRIM(STRING(nivelAcceso))   + "," +
                        TRIM(STRING(fecAct)).
        PUT UNFORMATTED TPrint.cadena SKIP.
        ASSIGN viCntClientes = viCntClientes + 1.
        RUN verDisplay.
    END.
    OUTPUT CLOSE.

    


    OUTPUT TO VALUE(vcNomPath + vcNomFileCta).
    FOR EACH TCuentas NO-LOCK:
        ASSIGN F-Estado:SCREEN-VALUE = "Creando Tabla Temporal Cuentas: " + STRING(TCuentas.nit).
        CREATE TPrint.
        UPDATE tipo = 2
            cadena = TRIM(STRING(TCuentas.Signo))        + "," +
                     TRIM(STRING(TCuentas.codEntidad))   + "," +
                     TRIM(STRING(TCuentas.nit))          + "," +
                     TRIM(STRING(TCuentas.cuenta))       + "," +
                     TRIM(STRING(TCuentas.tpProd))       + "," +
                     TRIM(STRING(TCuentas.CtaDefault))   + "," +
                     TRIM(STRING(TCuentas.Constante))    + "," +
                     TRIM(STRING(TCuentas.sdoTotal))     + "," +
                     TRIM(STRING(TCuentas.sdoDispon))    + "," +
                     TRIM(STRING(TCuentas.fecAct)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntCuentas = viCntCuentas + 1.
        RUN verDisplay.
    END.
    OUTPUT CLOSE.

    /*Cupos*/
    OUTPUT TO VALUE(vcNomPath + vcNomFileCupos).
    FOR EACH TCupos NO-LOCK:
        ASSIGN F-Estado:SCREEN-VALUE = "Creando Tabla Temporal Cupos: " + STRING(TCupos.nit).
        CREATE TPrint.
        UPDATE tipo = 2
            cadena = TRIM(STRING(TCupos.Signo))        + "," +
                     TRIM(STRING(TCupos.codEntidad))   + "," +
                     TRIM(STRING(TCupos.nit))          + "," +
                     TRIM(STRING(TCupos.cuenta))       + "," +
                     TRIM(STRING(TCupos.canal))       + "," +
                     TRIM(STRING(TCupos.NumTxCanal))   + "," +
                     TRIM(STRING(TCupos.ValTxCanal))    + "," +
                     TRIM(STRING(TCupos.fecAct)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntCupos = viCntCupos + 1.
        RUN verDisplay.
    END.
    OUTPUT CLOSE.


    OUTPUT TO VALUE(vcNomPath + vcNomFileTar).
    FOR EACH TTarjetas NO-LOCK:
        ASSIGN F-Estado:SCREEN-VALUE = "Creando Tabla Temporal Tarjetas: " + STRING(TTarjetas.nit).
        CREATE TPrint.
        UPDATE  tipo = 3
                cadena = TRIM(STRING(TTarjetas.Signo))      + "," +
                         TRIM(STRING(TTarjetas.codEntidad)) + "," +
                         TRIM(STRING(TTarjetas.nit))        + "," +
                         TRIM(STRING(TTarjetas.tarjetaDB))  + "," +
                         TRIM(STRING(TTarjetas.estadoTDB))  + "," +
                         TRIM(STRING(TTarjetas.fecAct)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntTarjetas = viCntTarjetas + 1.
        RUN verDisplay.
    END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(vcNomPath + vcNomFileTarCta).
    FOR EACH TTDBCta NO-LOCK:
        ASSIGN F-Estado:SCREEN-VALUE = "Creando Tabla Temporal Tarjetas - Cuentas : " + STRING(TTDBCta.nit).
        CREATE TPrint.
        UPDATE  tipo = 4 
                cadena = TRIM(STRING(TTDBCta.Signo))        + "," +
                         TRIM(STRING(TTDBCta.codEntidad))   + "," +
                         TRIM(STRING(TTDBCta.nit))          + "," +
                         TRIM(STRING(TTDBCta.tarjetaDB))    + "," +
                         TRIM(STRING(TTDBCta.cuenta))       + "," +
                         TRIM(STRING(TTDBCta.tpProd))       + "," +
                         TRIM(STRING(TTDBCta.fecAct)).  

        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntTarCuenta = viCntTarCuenta + 1.        
        RUN verDisplay.
    END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(vcNomPath + vcNomFileCtrl).
        CREATE TPrint.
        UPDATE tipo = 5
                cadena = TRIM(STRING(vcNomFileCl))      + "," + TRIM(STRING(viCntClientes)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        CREATE TPrint.
        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileCta))     + "," + TRIM(STRING(viCntCuentas)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        CREATE TPrint.
        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileTar))     + "," + TRIM(STRING(viCntTarjetas)).            
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        CREATE TPrint.
        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileTarCta))  + "," + TRIM(STRING(viCntTarCuenta)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.

        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileCupos))  + "," + TRIM(STRING(viCntCupos)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        
     OUTPUT CLOSE.


     ASSIGN F-Estado:SCREEN-VALUE = "Creando Archivo comprimido".
     ASSIGN  vcNomFile = STRING(getFecToInt(vdaFecAct)) + "." + vcCodEntidad + "."
             vcNomFileCl     = vcNomFile + "FCL"
             vcNomFileTar    = vcNomFile + "FCA"
             vcNomFileCta    = vcNomFile + "FAC"
             vcNomFileCupos  = vcNomFile + "FACH"
             vcNomFileTarCta = vcNomFile + "FCVA"
             vcNomFileCtrl   = vcNomFile + "F"
             vcComandoFiles[1]  = vcNomFileCl
             vcComandoFiles[2]  = vcNomFileTar
             vcComandoFiles[3]  = vcNomFileCta
             vcComandoFiles[4]  = vcNomFileCupos
             vcComandoFiles[5]  = vcNomFileTarCta
             vcComandoFiles[6]  = vcNomFileCtrl.


     /*Comprimir archivos de carga*/
     DO i = 1 TO 6:
        ASSIGN vcComando = "7z a " + vcNomPath + vcNomFile + "7z " + vcNomPath + vcComandoFiles[i].
        OS-COMMAND SILENT VALUE(vcComando).        
     END.

     /*Conectar unidad de red*/
     ASSIGN vcComando = "net use K: \\172.16.31.170\c$\ECGInp /User:KIRDI\administrador dgsis01".
     OS-COMMAND SILENT VALUE(vcComando).

     /*Copiar archivo comprimido en servidor de switch ECG*/
     ASSIGN vcComando = "COPY " + vcNomPath + vcNomFile + "7z  K:".
     OS-COMMAND SILENT VALUE(vcComando).

     /*Desconectar Unidad de red*/
     ASSIGN vcComando = "net use K: /DELETE".
     OS-COMMAND SILENT VALUE(vcComando).


     MESSAGE "Archivo Comprimido " SKIP 
         vcNomFile "7z" SKIP
         "Ha sido generado. y copiado en" SKIP
         "Servidor de switch Transaccional." 
         VIEW-AS ALERT-BOX INFO BUTTONS OK .

     MESSAGE "Puede continuar con la carga" SKIP
         "desde el switch Transaccional"
         VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Continuar carga".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY F-fecha F-Estado F-Hora1 F-Hora2 NumReg F-Tiempo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-274 RECT-1 RECT-278 BUTTON-1 F-fecha B-Proceso BtnDone-2 
         BUTTON-95 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GeneraTemp C-Win 
PROCEDURE GeneraTemp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    DEFINE VARIABLE viNumTXATM AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.
    DEFINE VARIABLE viValTXATM AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.
    DEFINE VARIABLE viNumTXPOS AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.
    DEFINE VARIABLE viValTXPOS AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.

    DEFINE VARIABLE vdeSdoDisProd AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE vdeSdoTotProd AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE pcnumcuenta   AS CHARACTER   NO-UNDO.


    FOR EACH ahorros WHERE ahorros.tarjetaDB NE "" AND tarjetaDB BEGINS "5907" AND estado EQ 1 AND 
/*         ahorros.agencia EQ 8 AND      */
/*         ahorros.nit EQ "80374147" AND */
        TRUE NO-LOCK:  

        IF LENGTH(ahorros.tarjetaDB) NE 16 THEN NEXT.

        ASSIGN F-Estado:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "Creando Tabla de saldos. Tarjeta : " + STRING(Ahorros.tarjetaDB).

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.tip_ahorro EQ ahorros.tip_ahorro AND Pro_Ahorros.cod_ahorro EQ Ahorros.cod_ahorro NO-LOCK NO-ERROR.
        ASSIGN 
                        vdeSdoDisProd = Ahorros.Sdo_Disponible - Pro_Ahorros.Val_SdoMinimo - Ahorros.AjusteTDB.
                IF Ahorros.Ajuste EQ 1 THEN
                        vdeSdoDisProd = Ahorros.Sdo_Disponible - Ahorros.AjusteTDB.
                IF Ahorros.num_convenio NE 0 THEN DO:
                        FIND FIRST Convenios WHERE Convenios.num_convenio = Ahorros.num_convenio NO-LOCK NO-ERROR.
                        IF AVAILABLE Convenios THEN
                                ASSIGN
                                        vdeSdoDisProd = Ahorros.Sdo_Disponible - Ahorros.AjusteTDB - Convenios.sdo_minimo_Benef.                                    
                END. /*IF Ahorros.num_convenio NE 0*/
                
                ASSIGN vdeSdoTotProd = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.

         IF LENGTH(STRING(ahorros.cod_ahorro)) > 6 THEN
            pcnumcuenta = STRING(INTEGER(ahorros.cue_ahorro),"9999999").
        ELSE 
            pcnumcuenta = STRING(INTEGER(ahorros.cue_ahorro),"999999").

        /*Cuenta Ahorros*/
        CREATE TCuentas.
        BUFFER-COPY ahorros TO TCuentas.
        UPDATE  TCuentas.CodEntidad     = vcCodEntidad
                TCuentas.cuenta         = STRING(ahorros.agencia,"999") + STRING(ahorros.cod_ahorro,"999") + pcnumcuenta
                TCuentas.tpProd         = 10
                TCuentas.CtaDefault     = 1
                TCuentas.sdoTotal       = TRIM(STRING(decToChar(vdeSdoTotProd)))
                TCuentas.sdoDispon      = TRIM(STRING(decToChar(vdeSdoDisProd)))
                TCuentas.fecAct         = getFecToInt(vdaFecAct).

        IF DEC(TCuentas.sdoDispon) < 0 THEN
           TCuentas.sdoDispon = "0".

        IF DEC(TCuentas.sdoTotal) < 0 THEN
           TCuentas.sdoTotal = "0".

        /*Cuenta Credito Rotativo*/
        FIND FIRST creditos WHERE 
                   creditos.nit EQ ahorros.nit AND 
                  (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND 
                  creditos.estado EQ 2 NO-LOCK NO-ERROR.
        
        /*Modificaco : Maria Caceres
             Enviar 7 digitos solo para los cupos rotativos que el numero de cuenta sea mayor a 6 digitos   
        */

        IF AVAILABLE creditos THEN DO:
            IF LENGTH(STRING(creditos.num_credito)) > 6 THEN
                pcnumcuenta = STRING(INTEGER(creditos.num_credito),"9999999").
            ELSE 
                pcnumcuenta = STRING(INTEGER(creditos.num_credito),"999999").                      

            CREATE TCuentas.
            UPDATE TCuentas.codEntidad  = vcCodEntidad
                   TCuentas.nit         = ahorros.nit
                   /*TCuentas.cuenta      = STRING(creditos.agencia,"999") + STRING(creditos.cod_credito,"999") + STRING(creditos.num_credito,"9999999")*/
                   TCuentas.cuenta      = STRING(creditos.agencia,"999") + STRING(creditos.cod_credito,"999") + pcnumcuenta
                   TCuentas.tpProd      = 50
                   TCuentas.CtaDefault  = 0
                   TCuentas.sdoTotal    = TRIM(STRING(decToChar(Creditos.Monto - Creditos.Sdo_Capital)))
                   TCuentas.sdoDispon   = TRIM(STRING(decToChar(Creditos.Monto - Creditos.Sdo_Capital)))
                   TCuentas.fecAct      = getFecToInt(vdaFecAct).
            
            IF DEC(TCuentas.sdoDispon) < 0 THEN
               TCuentas.sdoDispon = "0".

            IF  DEC(TCuentas.sdoTotal) < 0 THEN
                TCuentas.sdoTotal = "0".

            RUN verDisplay.

        END. /*IF AVAILABLE creditos THEN DO:*/

    RUN verDisplay.

    END. /*FOR EACH ahorros */
    
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verDisplay C-Win 
PROCEDURE verDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN viNumReg = viNumReg + 1
      Contador = Contador + 1
      vdtHorFin = NOW.
  IF viNumReg = 1 OR Contador = 200 THEN DO:
     ASSIGN F-Hora2:SCREEN-VALUE IN FRAME DEFAULT-FRAME  = STRING(TIME, "HH:MM:SS") 
            F-Hora1:SCREEN-VALUE IN FRAME DEFAULT-FRAME  = STRING(Hora1,"HH:MM:SS") 
            NumReg:SCREEN-VALUE IN FRAME DEFAULT-FRAME   = STRING(viNumReg)
            F-Tiempo:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(INTERVAL(vdtHorFin,vdtHorIni,"seconds"),"HH:MM:SS")
            Contador = 0.
/*      DISPLAY F-Estado NumReg F-Hora1 F-Hora2 F-Tiempo WITH FRAME DEFAULT-FRAME */
/*          IN WINDOW C-Win */
/*          .               */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION decToChar C-Win 
FUNCTION decToChar RETURNS CHARACTER
    (INPUT ipdeValor AS DECIMAL):
    /*------------------------------------------------------------------------------
      Purpose:  Convertir un decimal de dos decimales en character
        Notes:  toma un decimal y lo convierte a character, dejando en los dos ultimos
                caracteres, los decimales
                Eje: 12345.78  --> queda como --> "1234578"
    --------------------------------------------- ---- -----------------------------*/
    
        DEFINE VARIABLE vcValor AS CHARACTER NO-UNDO.

        ASSIGN 
                vcValor = REPLACE(STRING(TRUNCATE(ipdeValor,2),"->>>>>>>>>>>9.99"),".","").

        RETURN vcValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFecToInt C-Win 
FUNCTION getFecToInt RETURNS INT64
    (INPUT ipdaFecha    AS DATE):

    DEFINE VARIABLE vi6Fecha AS INT64       NO-UNDO.

    ASSIGN vi6Fecha = INT64(STRING(YEAR(ipdaFecha),"9999") + STRING(MONTH(ipdaFecha),"99") + STRING(DAY(ipdaFecha),"99")).

    RETURN vi6Fecha.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

