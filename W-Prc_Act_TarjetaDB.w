&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
  DEFINE VARIABLE W_Ok       AS LOGICAL.

DEF  VAR  ObjRecordSet    AS COM-HANDLE  NO-UNDO .
DEF  VAR  ObjConnection   AS COM-HANDLE  NO-UNDO .
DEF  VAR  ObjCommand      AS COM-HANDLE  NO-UNDO .
DEF  VAR  ODBC-DSN        AS CHARACTER   NO-UNDO .
DEF  VAR  ODBC-SERVER     AS CHARACTER   NO-UNDO .
DEF  VAR  ODBC-USERID     AS CHARACTER   NO-UNDO .
DEF  VAR  ODBC-PASSWD     AS CHARACTER   NO-UNDO.
DEF  VAR  ODBC-QUERY      AS CHARACTER   NO-UNDO .
DEF  VAR  ODBC-STATUS     AS CHARACTER   NO-UNDO .
DEF  VAR  ODBC-RECCOUNT   AS INTEGER     NO-UNDO .
DEF  VAR  ODBC-NULL       AS CHARACTER   NO-UNDO .
DEF  VAR  ODBC-CURSOR     AS INTEGER     NO-UNDO .
DEF  VAR  wcuenta         AS CHARACTER FORMAT "x(20)".
DEF  VAR  wtarAct         AS INTEGER INITIAL  0.
DEF  VAR  WStrAccess      AS CHARACTER    NO-UNDO.
DEF  VAR  IRetorna        AS CHARACTER    NO-UNDO.
DEF  VAR  wsdomin         AS DECIMAL INITIAL 0. 
DEF  VAR  wsdo_act        AS DECIMAL INITIAL 0 FORMAT "-zzzzzzzzz9.99".
DEF  VAR  wsdo_cta        AS DECIMAL INITIAL 0 FORMAT "-zzzzzzzzz9.99".
DEF  VAR  wcodigo         AS CHARACTER FORMAT "X(10)".
DEF  VAR  wtel            AS CHARACTER FORMAT "X(10)".
DEF  VAR  wnroconv        AS CHARACTER INITIAL "252".
DEF  VAR  wcantAho        AS INTEGER INITIAL 0.
DEF  VAR  wcantCup        AS INTEGER INITIAL 0.
    
/* Definicion de Tabla temporal */ 
DEFINE TEMP-TABLE DatosAccess
    FIELD cli_docide     AS CHARACTER FORMAT "X(20)"
    FIELD cli_tarjeta    AS CHARACTER FORMAT "X(20)"
    FIELD cli_cuenta     AS CHARACTER FORMAT "X(20)"
    FIELD cli_Tipocuenta AS INTEGER
    INDEX DatosDocCtaTipo cli_DocIde Cli_Cuenta Cli_TipoCuenta.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-285 RECT-287 T-Concurrente ~
T-Inf_ahorros T-Inf_Creditos tgl-NumeracionTdb btn-Salida 
&Scoped-Define DISPLAYED-OBJECTS T-Concurrente T-Inf_ahorros T-Inf_Creditos ~
tgl-NumeracionTdb 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     LABEL "Procesar" 
     SIZE 18 BY 1.35.

DEFINE BUTTON btn-Salida 
     LABEL "Salir" 
     SIZE 18 BY 1.35.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39.72 BY 3.08.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 2.15.

DEFINE VARIABLE T-Concurrente AS LOGICAL INITIAL no 
     LABEL "Activar" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE T-Inf_ahorros AS LOGICAL INITIAL no 
     LABEL "Cuentas de Ahorros a la Vista" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.43 BY .81 NO-UNDO.

DEFINE VARIABLE T-Inf_Creditos AS LOGICAL INITIAL no 
     LABEL "Linea de Créditos Cupo Rotativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.43 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-NumeracionTdb AS LOGICAL INITIAL no 
     LABEL "Numeración de Tarjetas" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     T-Concurrente AT ROW 1.73 COL 28.86
     T-Inf_ahorros AT ROW 4.27 COL 4.57
     T-Inf_Creditos AT ROW 5.08 COL 4.57
     tgl-NumeracionTdb AT ROW 5.85 COL 4.72 WIDGET-ID 2
     Btn-Procesar AT ROW 7.46 COL 2
     btn-Salida AT ROW 7.46 COL 24
     "Concurrente" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.23 COL 4
     RECT-285 AT ROW 3.88 COL 2.29
     RECT-287 AT ROW 1.46 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 43 BY 8.54
         BGCOLOR 17 .


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
         TITLE              = "Actualización Tarjeta Débito"
         HEIGHT             = 8.54
         WIDTH              = 42.86
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn-Procesar IN FRAME f-main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME f-main:HANDLE
       ROW             = 1.81
       COLUMN          = 3
       HEIGHT          = 1.35
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(T-Concurrente:HANDLE IN FRAME f-main).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Actualización Tarjeta Débito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Actualización Tarjeta Débito */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME f-main /* Procesar */
DO:
  ASSIGN FRAME F-MAIN  T-Concurrente.

  /* Create the connection object for the link to SQL */
  CREATE "ADODB.Connection" ObjConnection.
  /* Create a recordset object ready to return the data */
  CREATE "ADODB.RecordSet" ObjRecordSet.
  /* Create a command object for sending the SQL statement */
  CREATE "ADODB.Command" ObjCommand.

  /* Change the below values as necessary */ 
  ASSIGN ODBC-DSN = "Enpacto" /* The ODB
  C DSN */
  ODBC-SERVER = "172.16.31.152" /* The name of the server hosting the SQL DB
  and DSN */
  ODBC-USERID = "" /* The user id for access to the SQL Database */
  ODBC-PASSWD = "" /* Password required by above user-id */
  ODBC-QUERY = "SELECT cli_docide, cli_tarjeta, cli_cuenta, Cli_tipocuenta FROM clientes ".
  /* Open up the connecti.on to the ODBC Layer */
  ObjConnection:Open ( "data source=" + ODBC-DSN + ";server=" +
  ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) NO-ERROR. 
  /* Check for connection errors */
  If ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN  DO:
      ODBC-STATUS = "Error: No pudo establecer conexion.".
     DISPLAY "NO SE PUDO HACER CONEXION" SKIP(0).
     RETURN.
  END.
  ELSE DO:
      ASSIGN  ObjCommand:ActiveConnection = ObjConnection
              ObjCommand:CommandText = ODBC-QUERY
              ObjCommand:CommandType = 1 /* adCmdText */
              ObjConnection:CursorLocation = 3 /* adUseClient */
              ObjRecordSet:CursorType = 3 /* adOpenStatic */
              ObjRecordSet = ObjCommand:Execute ( output ODBC-NULL, "", 32 )
              ODBC-RECCOUNT = ObjRecordSet:RecordCount.

          /* Have we returned any rows ? */
          If ( ODBC-RECCOUNT > 0 ) AND NOT  ( ODBC-RECCOUNT = ? ) THEN DO:
              DISPLAY "Registros con tarjeta debito " ODBC-RECCOUNT NO-LABEL SKIP(0).
              ObjRecordSet:MoveFirst NO-ERROR.
              Do WHILE ODBC-CURSOR < ODBC-RECCOUNT:
                /* Display the data from the query (or create a Progress temp-table
                for future use) */
                /* Display ObjRecordSet:Fields ("name"):Value format "x(20)". */
                    CREATE DatosAccess.
                    ASSIGN DatosAccess.cli_docide     = trim(ObjRecordSet:Fields ("cli_docide"):Value)       
                           DatosAccess.cli_tarjeta    = trim(ObjRecordSet:Fields ("cli_tarjeta"):Value)      
                           DatosAccess.cli_cuenta     = trim(ObjRecordSet:Fields ("cli_cuenta"):Value) 
                           DatosAccess.Cli_tipocuenta = INTEGER(trim(ObjRecordSet:Fields ("Cli_tipocuenta"):Value)) .            
                ASSIGN  ODBC-CURSOR = ODBC-CURSOR + 1.
                ObjRecordSet:MoveNext NO-ERROR .
              END . /* retrieved a single data row */
          END. /* retrieved all data rows */
      ELSE 
          ASSIGN ODBC-STATUS = "No se encontraron registros.".


  ObjRecordSet:CLOSE NO-ERROR.
          ASSIGN  ObjCommand:ActiveConnection = ObjConnection
                  ObjCommand:CommandText = ODBC-QUERY
                  ObjCommand:CommandType = 1 /* adCmdText */
                  ObjConnection:CursorLocation = 3 /* adUseClient */
                  ObjRecordSet:CursorType = 3 /* adOpenStatic */
                  ObjRecordSet = ObjCommand:Execute ( output ODBC-NULL, "", 32 )
                  ODBC-RECCOUNT = ObjRecordSet:RecordCount.
           
             /* Have we returned any rows ? */
            If ( ODBC-RECCOUNT > 0 ) AND NOT  ( ODBC-RECCOUNT = ? ) THEN DO:
                ObjRecordSet:MoveFirst NO-ERROR.
                Do WHILE ODBC-CURSOR < ODBC-RECCOUNT:
                    /* Display the data from the query (or create a Progress temp-table
                    for future use) */
                    /* Display ObjRecordSet:Fields ("name"):Value format "x(20)". */
                        CREATE DatosAccess.
                        ASSIGN DatosAccess.cli_docide     = trim(ObjRecordSet:Fields ("cli_docide"):Value)       
                               DatosAccess.cli_tarjeta    = trim(ObjRecordSet:Fields ("cli_tarjeta"):Value)      
                               DatosAccess.cli_cuenta     = trim(ObjRecordSet:Fields ("cli_cuenta"):Value) 
                               DatosAccess.Cli_tipocuenta = INTEGER(trim(ObjRecordSet:Fields ("Cli_tipocuenta"):Value)).            
                    ASSIGN  ODBC-CURSOR = ODBC-CURSOR + 1.
                    ObjRecordSet:MoveNext NO-ERROR .
                END . /* retrieved a single data row */
            END. /* retrieved all data rows */
        ELSE 
            ASSIGN ODBC-STATUS = "No records found.".
  /* Close the ADO connection */
  ObjConnection:CLOSE NO-ERROR.
  END. /* The connection opened correctly */

  /* Don't forget to release the memory!! */
  RELEASE OBJECT ObjConnection NO-ERROR.
  RELEASE OBJECT ObjCommand NO-ERROR.
  RELEASE OBJECT ObjRecordSet NO-ERROR.
  ASSIGN  ObjConnection = ? ObjCommand = ? ObjRecordSet = ?.

TransGrabacion:
  DO TRANSACTION ON ERROR  UNDO, LEAVE TransGrabacion
                 ON ENDKEY UNDO, LEAVE transGrabacion
                 ON STOP   UNDO, LEAVE transGrabacion:   
     DISPLAY "Ingreso a TransGrabacion" SKIP(0).
     /* OUTPUT TO c:\cuentas_actualizar.txt. */
     IF tgl-NumeracionTdb THEN DO:
         FOR EACH DatosAccess WHERE DatosAccess.Cli_Tipocuenta = 10:
             wcuenta = SUBSTRING(DatosAccess.cli_cuenta,4,16).
             IF DatosAccess.cli_tipocuenta EQ 20 AND LENGTH(TRIM(wcuenta)) NE 0 THEN DO:  /* Cupo Rotativo */
                wcuenta = TRIM(STRING(DECIMAL(cli_cuenta) - 2000000000)).
             END.
             ELSE DO: /* Cuenta de Ahorros a la vista */
                /* ACTUALIZACION DE NUMERACION DE TARJETAS DEBITO */
                IF TRIM(DatosAccess.cli_tarjeta) NE ? THEN DO: 
                    FIND FIRST ahorros WHERE ahorros.nit         = DatosAccess.Cli_Docide AND
                                             ahorros.cod_ahorro  = 3                AND
                                             ahorros.cue_ahorros = wcuenta  NO-LOCK NO-ERROR.
                    IF AVAILABLE(ahorros) THEN DO:
                       IF LENGTH(TRIM(DatosAccess.cli_tarjeta)) = 16 AND trim(ahorros.tarjetadb) NE trim(DatosAccess.cli_tarjeta) THEN DO:
                          FIND CURRENT ahorros  NO-ERROR.
                          ASSIGN ahorros.tarjetadb = TRIM(DatosAccess.cli_tarjeta).  
                          FIND CURRENT ahorros NO-LOCK NO-ERROR.
                          DISPLAY DatosAccess.cli_docide wcuenta  DatosAccess.cli_tarjeta Ahorros.tarjetadb NO-LABEL SKIP(0).
                          wtarAct = wtarAct + 1.
                       END.
                    END.
                END. 
             END.
         END.
         DISPLAY "Nro. Tarjetas Actualizadas  : " wtarAct FORMAT "zzz,zz9" NO-LABEL SKIP(0).
         RELEASE ahorros.
     END.
     /* OUTPUT CLOSE. */
     IF t-Inf_Ahorros THEN DO:
         FIND FIRST pro_ahorros WHERE pro_ahorros.Tip_ahorro = Ahorros.Tip_ahorro AND
                                      pro_ahorros.Cod_ahorro = Ahorros.Cod_ahorro NO-LOCK NO-ERROR.
         IF AVAILABLE(pro_ahorros) THEN
           wsdomin = pro_ahorros.val_sdominimo.

         FOR EACH ahorros WHERE ahorros.cod_ahorro = 3 AND ahorros.Fec_CreaTdb = TODAY  NO-LOCK:
           IF ahorros.tarjetadb = "TARJ.DB.SOLICIT." OR ahorros.tarjetadb = " " THEN DO:
              FIND FIRST DatosAccess WHERE DatosAccess.Cli_DocIde     = ahorros.nit AND 
                                           DatosAccess.Cli_TipoCuenta = 10 AND 
                                           DatosAccess.Cli_cuenta         = (wnroconv + TRIM(ahorros.cue_ahorros)) NO-LOCK NO-ERROR.
              IF NOT AVAILABLE(DatosAccess) THEN DO:
                FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
                ASSIGN wsdo_act = round(ahorros.sdo_disponible - wsdomin,0)
                       wsdo_cta = round(ahorros.sdo_disponible + ahorros.sdo_canje,0)
                       wcodigo  = STRING(RANDOM(1,9999999999),"9999999999").
                IF wsdo_act LT 1 THEN wsdo_act = 0.
                IF wsdo_cta LT 1 THEN wsdo_cta = 0.

                IF    clientes.tel_residencia NE " " THEN wtel = clientes.tel_residencia.
                ELSE  wtel = clientes.tel_comercial.
                IF wtel = " " OR LENGTH(TRIM(wtel)) LT 7 THEN wtel = '3487300'.

                WstrAccess = "INSERT INTO clientes(cli_nombre, cli_Saldo_actual, Cli_Saldo_Cta, " 
                           + "Cli_fecha_saldo, Cli_Tel, Cli_DocIde, Cli_cuenta, cli_codigo, cli_comentarios, cli_TipoCuenta) " 
                           + "VALUES ('" + TRIM(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2) + "', "
                           + TRIM(STRING(wsdo_act)) + ", " + TRIM(STRING(wsdo_cta)) + ", " 
                           + "('" + STRING(DAY(TODAY),"99") + '/' + STRING(MONTH(TODAY),"99") + '/' + STRING(YEAR(TODAY),'9999') + "'), '"
                           + TRIM(wtel) + "', '" + TRIM(ahorros.nit) + "', '" + wnroconv  + TRIM(ahorros.cue_ahorros)  + "', '"  
                           + TRIM(wcodigo) + "', 'Administrador TarjDB', '10') ".
                RUN CnxAccessClientes(INPUT WStrAccess, OUTPUT IRetorna).   /* Crear cliente */
                DISPLAY "Nuevas Ctas  de Ahorro      : " ahorros.agencia FORMAT "z9"    NO-LABEL 
                                                         ahorros.nit     FORMAT "X(12)" NO-LABEL 
                                                         ahorros.cue_ahorros FORMAT "X(8)" NO-LABEL wsdo_act NO-LABELS SKIP(0).
                wcantAho = wcantAho + 1.
              END.
           END.
         END.   
         DISPLAY "Nuevos Tarj DB Ahorros      : " wcantAho FORMAT "zzz,zz9" NO-LABEL SKIP(0).
     END.
     /* Creacion de Cupo Rotativo */ 
     IF t-Inf_Creditos THEN DO:
         FOR EACH creditos WHERE creditos.cod_credito = 570 AND Creditos.Fec_aprobacion = TODAY NO-LOCK:
              FIND FIRST DatosAccess WHERE DatosAccess.Cli_DocIde     = creditos.nit AND 
                                           DatosAccess.Cli_TipoCuenta = 20          AND 
                                           DatosAccess.Cli_cuenta     = (wnroconv + TRIM(STRING(creditos.num_credito + 2000000000))) NO-LOCK NO-ERROR.
              IF NOT AVAILABLE(DatosAccess) THEN DO:

                FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
                ASSIGN wsdo_act = round(creditos.Monto - creditos.sdo_capital,0)
                       wsdo_cta = round(creditos.Monto - Creditos.sdo_capital,0)
                       wcodigo  = STRING(RANDOM(1,9999999999),"9999999999").
                IF wsdo_act LT 1 THEN wsdo_act = 0.
                IF wsdo_cta LT 1 THEN wsdo_cta = 0.

                IF    clientes.tel_residencia NE " " THEN wtel = clientes.tel_residencia.
                ELSE  wtel = clientes.tel_comercial.
                IF wtel = " " OR LENGTH(TRIM(wtel)) LT 7 THEN wtel = '3487300'.

                WstrAccess = "INSERT INTO clientes(cli_nombre, cli_Saldo_actual, Cli_Saldo_Cta, " 
                           + "Cli_fecha_saldo, Cli_Tel, Cli_DocIde, Cli_cuenta, cli_codigo, cli_comentarios, cli_TipoCuenta) " 
                           + "VALUES ('" + TRIM(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2) + "', "
                           + TRIM(STRING(wsdo_act)) + ", " + TRIM(STRING(wsdo_cta)) + ", " 
                           + "('" + STRING(DAY(TODAY),"99") + '/' + STRING(MONTH(TODAY),"99") + '/' + STRING(YEAR(TODAY),'9999') + "'), '"
                           + TRIM(wtel) + "', '" + TRIM(creditos.nit) + "', '" + (wnroconv + TRIM(STRING(creditos.num_credito + 2000000000)))  + "', '"  
                           + TRIM(wcodigo) + "', 'Administrador TarjDB', '20') ".
                RUN CnxAccessClientes(INPUT WStrAccess, OUTPUT IRetorna).   /* Crear cliente */ 
                DISPLAY "Nuevas Ctas Cupo Rotativo   : " creditos.agencia " " creditos.nit " " creditos.Num_credito " " wsdo_act NO-LABEL SKIP(0).
                wcantCup = wcantCup + 1.
              END.
           END.    
           DISPLAY "Nuevos Tarj DB Cupo Rotativo: " wcantCup FORMAT "zzz,zz9" NO-LABEL SKIP(0).
           DISPLAY "Salida del programa " STRING(TIME,"HH:MM:SS") SKIP(1).
     END.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Salida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Salida C-Win
ON CHOOSE OF btn-Salida IN FRAME f-main /* Salir */
DO:
   /* OS-DELETE VALUE(Listado).*/
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


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
IF T-Concurrente:SCREEN-VALUE IN FRAME F-main EQ "YES" THEN DO:
  IF FRAME F-main:VISIBLE THEN DO:
     APPLY "choose" TO btn-procesar IN FRAME F-main.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Concurrente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Concurrente C-Win
ON VALUE-CHANGED OF T-Concurrente IN FRAME f-main /* Activar */
DO:
  ASSIGN t-concurrente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Inf_ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Inf_ahorros C-Win
ON VALUE-CHANGED OF T-Inf_ahorros IN FRAME f-main /* Cuentas de Ahorros a la Vista */
DO:
  ASSIGN T-Inf_ahorros T-Inf_Creditos.
  IF T-Inf_Ahorros OR T-Inf_Creditos THEN
     Btn-Procesar:SENSITIVE = true.
  ELSE
     Btn-Procesar:SENSITIVE = false.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Inf_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Inf_Creditos C-Win
ON VALUE-CHANGED OF T-Inf_Creditos IN FRAME f-main /* Linea de Créditos Cupo Rotativo */
DO:
    ASSIGN T-Inf_Ahorros T-Inf_Creditos.
    IF T-Inf_Ahorros OR T-Inf_Creditos THEN 
       Btn-Procesar:SENSITIVE = true.
    ELSE
       Btn-Procesar:SENSITIVE = false.
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
  RUN InitializeObject.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CnxAccessClientes C-Win 
PROCEDURE CnxAccessClientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PCStrSql AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER PIRetorna AS CHARACTER NO-UNDO.

    DEF  VAR  ObjRecordSet AS  COM-HANDLE  NO-UNDO .
    DEF  VAR  ObjConnection AS  COM-HANDLE  NO-UNDO .
    DEF  VAR  ObjCommand AS  COM-HANDLE  NO-UNDO .

    DEF  VAR  ODBC-DSN AS  CHARACTER  NO-UNDO .
    DEF  VAR  ODBC-SERVER AS  CHARACTER  NO-UNDO .
    DEF  VAR  ODBC-USERID AS  CHARACTER  NO-UNDO .
    DEF  VAR  ODBC-PASSWD AS  CHARACTER  NO-UNDO .
    DEF  VAR  ODBC-STATUS AS  CHARACTER  NO-UNDO .
    DEF  VAR  ODBC-RECCOUNT AS  INTEGER  NO-UNDO .
    DEF  VAR  ODBC-NULL AS  CHARACTER  NO-UNDO .
    DEF  VAR  ODBC-CURSOR AS  INTEGER  NO-UNDO .
    
    /* If not executing against a sports2000 like database this temp table
    will need to be redefined */ 
    
    /* Create the connection object for the link to SQL */
    CREATE  "ADODB.Connection" ObjConnection.
    /* Create a recordset object ready to return the data */
    CREATE  "ADODB.RecordSet" ObjRecordSet.
    /* Create a command object for sending the SQL statement */
    CREATE  "ADODB.Command" ObjCommand.
    ASSIGN ODBC-DSN = "Enpacto" /* The ODBC DSN */
    ODBC-SERVER = "172.16.31.152" /* The name of the server hosting the SQL DB
    and DSN */
    ODBC-USERID = "" /* The user id for access to the SQL Database */
    ODBC-PASSWD = "". /* Password required by above user-id */
    /* ODBC-QUERY = "SELECT cli_docide, cli_tarjeta, cli_cuenta, Cli_tipocuenta FROM clientes ".*/
    /* Open up the connecti.on to the ODBC Layer */
    ObjConnection:Open ( "data source=" + ODBC-DSN + ";server=" +
    ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) NO-ERROR. 
    /* Check for connection errors */

    If ( ERROR-STATUS:NUM-MESSAGES > 0 ) then 
        ODBC-STATUS = "Error: Could not establish connection.".
    ELSE
    DO:
        ASSIGN  ObjCommand:ActiveConnection = ObjConnection
                ObjCommand:CommandText = PCStrSql
                ObjCommand:CommandType = 1 /* adCmdText */
                ObjConnection:CursorLocation = 3 /* adUseClient */
                ObjRecordSet:CursorType = 3 /* adOpenStatic */
                ObjRecordSet = ObjCommand:EXECUTE ( OUTPUT ODBC-NULL, "", 32 )
                ODBC-RECCOUNT = ObjRecordSet:RecordCount NO-ERROR.
        /*IF ERROR THEN*/
            /* Have we returned any rows ? */
            If ( ODBC-RECCOUNT > 0 ) and NOT  ( ODBC-RECCOUNT = ? ) THEN DO:
                ObjRecordSet:MoveFirst NO-ERROR.
                DO WHILE ODBC-CURSOR < ODBC-RECCOUNT:
                    /* Display the data from the query (or create a Progress temp-table
                    for future use) */
                    /* Display ObjRecordSet:Fields ("name"):Value format "x(20)". */
                    PIRetorna = ObjRecordSet:Fields (0):Value.
                    
                    Assign ODBC-CURSOR = ODBC-CURSOR + 1.
                    ObjRecordSet:MoveNext no-error.
                END. /* retrieved a single data row */
            End. /* retrieved all data rows */
            ELSE 
                ASSIGN ODBC-STATUS = "No records found.".
            /* Close the ADO connection */
            ObjConnection:Close NO-ERROR.
    END. /* The connection opened correctly */
    /* Don't forget to release the memory!! */
    RELEASE OBJECT  ObjConnection NO-ERROR .
    RELEASE OBJECT  ObjCommand NO-ERROR .
    RELEASE OBJECT  ObjRecordSet NO-ERROR .
    
    ASSIGN ObjConnection = ? ObjCommand = ? ObjRecordSet = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Prc_Act_TarjetaDB.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Prc_Act_TarjetaDB.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY T-Concurrente T-Inf_ahorros T-Inf_Creditos tgl-NumeracionTdb 
      WITH FRAME f-main IN WINDOW C-Win.
  ENABLE RECT-285 RECT-287 T-Concurrente T-Inf_ahorros T-Inf_Creditos 
         tgl-NumeracionTdb btn-Salida 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

