&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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

{Incluido\variable.i "shared"}
/* Local Variable Definitions ---                                       */
  DEF VAR P_Nit       LIKE Clientes.Nit        NO-UNDO.
  DEF VAR p_Nombre    LIKE Clientes.Nombre     NO-UNDO.
  DEF VAR P_Apellido  LIKE Clientes.Apellido1  NO-UNDO.
  DEF VAR P_AgeCli    LIKE Clientes.Agencia    NO-UNDO.
  DEF VAR w_ok        AS LOGICAL               NO-UNDO.
  DEF VAR w-tarjetaDB LIKE Ahorros.TarjetaDb   NO-UNDO.
  DEF VAR choice      AS LOGICAL INITIAL FALSE NO-UNDO.
  DEF VAR znroctas    AS INTEGER INITIAL 0     NO-UNDO.
  DEF VAR W-Cupotot   AS DECIMAL INITIAL 0     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-337 Btn_Cancelar Cmb-TarjetaDb ~
Cmb-CueCredito w-cedula Btn_Done 
&Scoped-Define DISPLAYED-FIELDS Tarjetas.OperMaxCaj Tarjetas.MontoMaxCaj ~
Tarjetas.MontoMaxPos Tarjetas.OperMaxPos 
&Scoped-define DISPLAYED-TABLES Tarjetas
&Scoped-define FIRST-DISPLAYED-TABLE Tarjetas
&Scoped-Define DISPLAYED-OBJECTS Cmb-TarjetaDb Cmb-CueCredito w-cedula ~
W_NomTitular 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar Btn_Salvar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Visualizar 
     LABEL "Visualizar" 
     SIZE 8 BY .69 TOOLTIP "Visualizar por Pantalla / Impresora".

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 8 BY 1.92
     FONT 4.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar 
     LABEL "&Salvar" 
     SIZE 8 BY 1.92
     FONT 4.

DEFINE VARIABLE w-cedula AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 68 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-337
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 2.42.

DEFINE VARIABLE Cmb-CueCredito AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23.57 BY 2.69 NO-UNDO.

DEFINE VARIABLE Cmb-TarjetaDb AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 22 BY 2.69 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Cancelar AT ROW 1.04 COL 71 WIDGET-ID 16
     Cmb-TarjetaDb AT ROW 1.88 COL 21.72 NO-LABEL WIDGET-ID 36
     Cmb-CueCredito AT ROW 1.88 COL 45.57 NO-LABEL WIDGET-ID 32
     w-cedula AT ROW 2 COL 2 NO-LABEL WIDGET-ID 12
     Btn_Salvar AT ROW 2.92 COL 71 WIDGET-ID 18
     W_NomTitular AT ROW 4.77 COL 2 NO-LABEL WIDGET-ID 14
     Btn_Done AT ROW 4.85 COL 71 WIDGET-ID 20
     Tarjetas.OperMaxCaj AT ROW 6.62 COL 56.14 COLON-ALIGNED NO-LABEL WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 5 BY .81 TOOLTIP "Número Máximo de Retiros en Cajeros"
     Tarjetas.MontoMaxCaj AT ROW 6.69 COL 20.14 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 16 BY .81 TOOLTIP "Monto Máximo a Retirar en Cajeros"
     Btn-Visualizar AT ROW 6.92 COL 71 WIDGET-ID 8
     Tarjetas.MontoMaxPos AT ROW 7.65 COL 20 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .81 TOOLTIP "Monto Máximo a Pagar en POS"
     Tarjetas.OperMaxPos AT ROW 7.69 COL 56.14 COLON-ALIGNED NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 5 BY .81 TOOLTIP "Número Máximo de Pagos en Pos"
     " Configuración de Valores" VIEW-AS TEXT
          SIZE 25.14 BY .81 AT ROW 5.81 COL 6 WIDGET-ID 52
          FGCOLOR 1 
     "Nro.Tarjeta Débito" VIEW-AS TEXT
          SIZE 18 BY .73 AT ROW 1.08 COL 21.57 WIDGET-ID 28
     "Cédula" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.12 COL 7 WIDGET-ID 22
     "Nro. Cuenta Cupo Rotativo" VIEW-AS TEXT
          SIZE 25.57 BY .73 AT ROW 1.08 COL 45.43 WIDGET-ID 34
     "Nro.Oper.Pos:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 7.77 COL 41.43 WIDGET-ID 48
     "Nro.Oper.Cajero:" VIEW-AS TEXT
          SIZE 16.14 BY .81 AT ROW 6.62 COL 41.14 WIDGET-ID 46
     RECT-337 AT ROW 6.38 COL 5.14 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 8.08 WIDGET-ID 100.


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
         TITLE              = "Activar Plásticos Cupo - W-ActivarPlasticos_Cupo.w"
         HEIGHT             = 8
         WIDTH              = 78.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 81.29
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 81.29
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
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn-Visualizar:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Tarjetas.MontoMaxCaj IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tarjetas.MontoMaxPos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tarjetas.OperMaxCaj IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tarjetas.OperMaxPos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-cedula IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Activar Plásticos Cupo - W-ActivarPlasticos_Cupo.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Activar Plásticos Cupo - W-ActivarPlasticos_Cupo.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME F-Main /* Visualizar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F-Main /* Cancelar */
DO:
  RUN Inicializar_Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Salir */
DO:
  MESSAGE "Esta seguro de SALIR de el registro del Cupo Rotativo ?" VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
  IF NOT choice THEN 
     RETURN.

  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME F-Main /* Salvar */
DO:
    W-Cupotot = 0.

    DO WITH FRAME F-main:
        ASSIGN Cmb-tarjetaDb
               W-Cedula
               Cmb-CueCredito.

        IF Cmb-TarjetaDb EQ ? OR W-Cedula EQ ?
                              OR Cmb-CueCredito EQ ? THEN DO:
            MESSAGE "Recuerde seleccionar uno de estos campos " SKIP
                    "Cédula    :" W-Cedula SKIP
                    "Tarjeta   :" Cmb-TarjetaDb SKIP
                    "NroCrédito:" Cmb-CueCredito SKIP
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.

        IF znroctas GT 1 THEN DO:
            MESSAGE "Esta seguro de ACTIVAR el cupo en el Nro.Crédito " Cmb-CueCredito " ?"
                VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
            
            IF NOT choice THEN
                RETURN.
        END.

        FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.
        
        Grabando:
        DO TRANSACTION ON ERROR UNDO Grabando:
            FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-ERROR.
            IF AVAILABLE(tarjetas) THEN DO:
                ASSIGN Tarjetas.Num_Credito = int64(Cmb-CueCredito)
                       Tarjetas.Cta_Cupo = Cfg_tarjetaDb.NroConvTarjDB + TRIM(STRING(int64(Cmb-CueCredito) + 2000000000))
                       Tarjetas.Fec_ActCupo = TODAY
                       Tarjetas.Hora_ActCupo = TIME
                       Tarjetas.Estado = '01'
                       Tarjetas.MontoMaxCaj
                       Tarjetas.MontoMaxPos
                       Tarjetas.OperMaxCaj
                       Tarjetas.OperMaxPos
                       Tarjetas.Usuario = W_Usuario.

                CREATE Hoja_Vida.
                ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                       Hoja_Vida.Codigo = 2
                       Hoja_Vida.DoctoRefer = DECIMAL(Cmb-CueCredito)
                       Hoja_Vida.Fec_Grabacion = W_Fecha
                       Hoja_Vida.Hora_Grabacion = TIME
                       Hoja_Vida.Nit = W-Cedula
                       Hoja_Vida.Observacion = "Tarjeta DB: " + TRIM(tarjetas.TarjetaDB)
                       Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_Tipo
                       Hoja_Vida.Usuario = W_Usuario.

                CREATE Bitacora_Tdb.
                ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_Tipo
                    Bitacora_Tdb.Codigo =  2
                    Bitacora_Tdb.Nit = W-Cedula
                    Bitacora_Tdb.Usuario = W_Usuario
                    Bitacora_Tdb.Observacion = TRIM(Cmb-CueCredito)
                    Bitacora_Tdb.Fecha = W_Fecha
                    Bitacora_Tdb.Hora = STRING(TIME,"HH:MM:SS")
                    Bitacora_Tdb.Cue_ahorros = Tarjetas.Cue_ahorros
                    Bitacora_Tdb.Num_credito = DECIMAL(Cmb-CueCredito)
                    Bitacora_Tdb.Usu_Inicial = W_usuario
                    Bitacora_Tdb.Usu_Final = W_usuario
                    Bitacora_Tdb.Age_Inicial = Tarjetas.agencia
                    Bitacora_Tdb.Age_Final = Tarjetas.agencia
                    Bitacora_Tdb.Tar_Inicial = tarjetas.TarjetaDB
                    Bitacora_Tdb.Tar_Final = tarjetas.TarjetaDB
                    Bitacora_Tdb.Cantidad = 1.

                FIND FIRST Creditos WHERE Creditos.cod_Credito = Cfg_Tarjeta.Cod_Credito
                                      AND Creditos.nit = w-Cedula
                                      AND Creditos.Num_credito = int64(Cmb-CueCredito) NO-ERROR.
                IF NOT AVAILABLE(Creditos) THEN
                    FIND FIRST Creditos WHERE Creditos.cod_Credito = Cfg_Tarjeta.Cod_Credito2
                                          AND Creditos.nit = w-Cedula
                                          AND Creditos.Num_credito = int64(Cmb-CueCredito) NO-ERROR.

                IF AVAILABLE(Creditos) THEN DO:
                    ASSIGN Creditos.tarjetadb = Cmb-TarjetaDb
                           Creditos.Fec_Creatdb = TODAY
                           Creditos.Fec_desembolso = w_Fecha
                           Creditos.Estado = 2
                           W-Cupotot = Creditos.Monto.
                END.

                RELEASE Creditos.
            END.
            ELSE 
                MESSAGE "No se encontró tarjeta Débito Nro: " Cmb-tarjetaDb
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RELEASE tarjetas.
            APPLY "CHOOSE" TO btn-visualizar.
        END.

        RUN inicializar_variables.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-CueCredito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-CueCredito C-Win
ON VALUE-CHANGED OF Cmb-CueCredito IN FRAME F-Main
DO:
    ASSIGN Cmb-CueCredito.
    
    IF TRIM(Cmb-CueCredito:SCREEN-VALUE) NE "" AND TRIM(Cmb-TarjetaDb:SCREEN-VALUE) NE "" THEN
        ENABLE btn_salvar WITH FRAME f-main.
    ELSE
        DISABLE btn_Salvar  WITH FRAME f-main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-TarjetaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarjetaDb C-Win
ON VALUE-CHANGED OF Cmb-TarjetaDb IN FRAME F-Main
DO:
    ASSIGN Cmb-TarjetaDb.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tarjetas.MontoMaxCaj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tarjetas.MontoMaxCaj C-Win
ON LEAVE OF Tarjetas.MontoMaxCaj IN FRAME F-Main /* Monto MaxCaj */
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) > cfg_tarjetaDB.MontoMaxCaj OR DECIMAL(SELF:SCREEN-VALUE) < cfg_tarjetaDB.MontoMinCaj THEN DO:
        MESSAGE "Este valor no está dentro de los rangos permitidos." SKIP
                "Por favor, verifique y corrija!!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        SELF:SCREEN-VALUE = STRING(0).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tarjetas.MontoMaxPos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tarjetas.MontoMaxPos C-Win
ON LEAVE OF Tarjetas.MontoMaxPos IN FRAME F-Main /* Monto MaxPos */
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) > cfg_tarjetaDB.MontoMaxCaj OR DECIMAL(SELF:SCREEN-VALUE) < cfg_tarjetaDB.MontoMinCaj THEN DO:
        MESSAGE "Este valor no está dentro de los rangos permitidos." SKIP
                "Por favor, verifique y corrija!!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        SELF:SCREEN-VALUE = STRING(0).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tarjetas.OperMaxCaj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tarjetas.OperMaxCaj C-Win
ON LEAVE OF Tarjetas.OperMaxCaj IN FRAME F-Main /* Operacion Caj */
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) > cfg_tarjetaDB.MontoMaxCaj OR DECIMAL(SELF:SCREEN-VALUE) < cfg_tarjetaDB.MontoMinCaj THEN DO:
        MESSAGE "Este valor no está dentro de los rangos permitidos." SKIP
                "Por favor, verifique y corrija!!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        SELF:SCREEN-VALUE = STRING(0).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tarjetas.OperMaxPos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tarjetas.OperMaxPos C-Win
ON LEAVE OF Tarjetas.OperMaxPos IN FRAME F-Main /* Operacion Pos */
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) > cfg_tarjetaDB.MontoMaxCaj OR DECIMAL(SELF:SCREEN-VALUE) < cfg_tarjetaDB.MontoMinCaj THEN DO:
        MESSAGE "Este valor no está dentro de los rangos permitidos." SKIP
                "Por favor, verifique y corrija!!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        SELF:SCREEN-VALUE = STRING(0).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-cedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cedula C-Win
ON LEAVE OF w-cedula IN FRAME F-Main
DO:
    DO WITH FRAME F-main:
        DISABLE btn_Salvar  WITH FRAME f-main.

        FIND Clientes WHERE Clientes.Nit EQ w-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ELSE DO:
            RUN C-Clientes.R(INPUT 2,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            ASSIGN w-cedula:SCREEN-VALUE = P_Nit
                   W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).

            FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
        END.

        IF Clientes.Estado EQ 2 AND Clientes.Fec_Retiro NE ? THEN DO:
            MESSAGE "No se pueden crear cuentas de ahorro para clientes Retirados" SKIP
                VIEW-AS ALERT-BOX WARNING.
            APPLY "choose" TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.

        IF Clientes.Tipo_Vinculo GT 2 THEN DO:
            MESSAGE W_NomTitular " No es un cliente de la Cooperativa" SKIP
                    "La persona o empresa debe estar matriculado como" SKIP
                    "Cliente o Asociado. Rectifique!"
                VIEW-AS ALERT-BOX.
            APPLY 'choose' TO btn_Cancelar.
            RETURN NO-APPLY.
        END.

        ASSIGN Cmb-CueCredito:LIST-ITEMS = ""
               Cmb-TarjetaDb:LIST-ITEMS = ""
               znroctas = 0.

        /* La adjudicación es para la agencia dónde se encuentre la cuenta */
        FOR EACH Creditos WHERE Creditos.agencia EQ W_Agencia
                            AND (Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito OR
                                 Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito2)
                            AND Creditos.nit EQ Clientes.Nit NO-LOCK:
            FIND FIRST tarjetas WHERE Tarjetas.estado = "01"
                                  AND Tarjetas.num_credito = Creditos.Num_credito NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(tarjetas) THEN DO:
                W_Ok = Cmb-CueCredito:ADD-LAST(TRIM(STRING(Creditos.Num_Credito))).
                znroctas = znroctas + 1.
            END.
        END.

        FOR EACH tarjetas WHERE Tarjetas.estado EQ "01"
                            AND Tarjetas.Nit EQ Clientes.Nit
                            AND (Tarjetas.Num_credito EQ 0 OR
                                 Tarjetas.Num_credito EQ ?) NO-LOCK:
            W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).
        END.
    END.
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
  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY Cmb-TarjetaDb Cmb-CueCredito w-cedula W_NomTitular 
      WITH FRAME F-Main IN WINDOW C-Win.
  IF AVAILABLE Tarjetas THEN 
    DISPLAY Tarjetas.OperMaxCaj Tarjetas.MontoMaxCaj Tarjetas.MontoMaxPos 
          Tarjetas.OperMaxPos 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE RECT-337 Btn_Cancelar Cmb-TarjetaDb Cmb-CueCredito w-cedula Btn_Done 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.

ASSIGN W-Cedula:SCREEN-VALUE IN FRAME f-main  = "" W-Cedula 
       W-TarjetaDb = ""
       Cmb-CueCredito:LIST-ITEMS = ""
       Cmb-CueCredito
       w_NomTitular:SCREEN-VALUE = ""
       W_NomTitular
       Cmb-TarjetaDb:LIST-ITEMS = ""
       Cmb-TarjetaDb.

/*IF Cfg_Tarjeta.Rep_cupo = 1 THEN DO: /* Particular */
    ASSIGN Tarjetas.MontoMaxCaj:SCREEN-VALUE = STRING(Tarjetas.MontoMaxCaj)
           Tarjetas.MontoMaxPos:SCREEN-VALUE = STRING(Tarjetas.MontoMaxPos)
           Tarjetas.OperMaxCaj:SCREEN-VALUE = STRING(Tarjetas.OperMaxCaj) 
           Tarjetas.OperMaxPos:SCREEN-VALUE = STRING(Tarjetas.OperMaxPos).

    ENABLE Tarjetas.MontoMaxCaj
           Tarjetas.MontoMaxPos
           Tarjetas.OperMaxCaj
           Tarjetas.OperMaxPos WITH FRAME f-main.
END.
ELSE DO:
    ASSIGN Tarjetas.MontoMaxCaj:SCREEN-VALUE = STRING(Cfg_Tarjeta.MontoMaxCaj)
           Tarjetas.MontoMaxPos:SCREEN-VALUE = STRING(Cfg_Tarjeta.MontoMaxPos)
           Tarjetas.OperMaxCaj:SCREEN-VALUE = STRING(Cfg_Tarjeta.OperMaxCaj)
           Tarjetas.OperMaxPos:SCREEN-VALUE = STRING(Cfg_Tarjeta.OperMaxPos).

    DISABLE Tarjetas.MontoMaxCaj
            Tarjetas.MontoMaxPos
            Tarjetas.OperMaxCaj
            Tarjetas.OperMaxPos WITH FRAME f-main.
END.*/

ASSIGN Tarjetas.MontoMaxCaj:SCREEN-VALUE = STRING(Cfg_Tarjeta.MontoMaxCaj)
       Tarjetas.MontoMaxPos:SCREEN-VALUE = STRING(Cfg_Tarjeta.MontoMaxPos)
       Tarjetas.OperMaxCaj:SCREEN-VALUE = STRING(Cfg_Tarjeta.OperMaxCaj)
       Tarjetas.OperMaxPos:SCREEN-VALUE = STRING(Cfg_Tarjeta.OperMaxPos).

ENABLE Tarjetas.MontoMaxCaj
       Tarjetas.MontoMaxPos
       Tarjetas.OperMaxCaj
       Tarjetas.OperMaxPos WITH FRAME f-main.

DISABLE Btn_Salvar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
 {INCLUIDO\RepEncabezado.I}.  
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 W_Reporte    = "REPORTE   : RELACION DE PLASTICOS : CUPO ROTATIVO  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.
 IF AVAILABLE(tarjetas) THEN DO:
     W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Tarjetas.tarjetaDb) + "           Activada el " + STRING(Tarjetas.Fec_activacion,"99/99/9999").
     VIEW FRAME F-Encabezado.
     VIEW FRAME F-Ftr.
     FIND FIRST Creditos WHERE Creditos.Cod_Credito = Cfg_TarjetaDb.cod_Credito AND
                               Creditos.nit         = Tarjetas.nit          AND
                               Creditos.Num_credito = Tarjetas.Num_Credito NO-LOCK NO-ERROR.
     FIND FIRST agencias WHERE agencias.agencia = Creditos.agencia  NO-LOCK NO-ERROR.
     FIND FIRST clientes WHERE clientes.nit     = Tarjetas.nit NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN DO:
         DISPLAY "Número de Crédito#: " + TRIM(STRING(Creditos.Num_Credito))  AT 1  FORMAT "X(80)"
                 "Agencia de Crédito: " + TRIM(Agencias.nombre)               AT 1  FORMAT "X(80)"    
                 "Cupo Otorgado     : " + STRING(W-Cupotot,">>>,>>>,>>9")     AT 1  FORMAT "X(80)"
                 "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Tarjetas.Nit) AT 1  FORMAT "X(80)"
                 "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                 WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

    END.                                                                                                                             
     PUT " " SKIP(2).
     DISPLAY Cfg_tarjetaDb.descripcion VIEW-AS EDITOR SIZE 105 BY 3 AT 3
            WITH FRAME fcode WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

     PUT " " SKIP(5).
     DISPLAY "Firma del Asociado:   _________________________________________"  WITH FRAME fcode2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

     DISPLAY "                      " + TRIM(Clientes.Nombre) + " " + TRIM(clientes.apellido1)     + " " + TRIM(Clientes.apellido2) AT 1 FORMAT "X(80)"
             "                      " + Clientes.Tipo_Identificacion +  ": "  AT 1  FORMAT "X(80)"
             WITH FRAME F-mov3 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
     PUT " " SKIP(2).
     PAGE.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

