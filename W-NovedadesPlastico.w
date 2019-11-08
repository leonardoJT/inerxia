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
  DEFINE VAR P_Nit       LIKE Clientes.Nit       NO-UNDO.
  DEFINE VAR p_Nombre    LIKE Clientes.Nombre    NO-UNDO.
  DEFINE VAR P_Apellido  LIKE Clientes.Apellido1 NO-UNDO.
  DEFINE VAR P_AgeCli    LIKE Clientes.Agencia   NO-UNDO.
  DEFINE VAR w_ok        AS LOGICAL              NO-UNDO.
  DEFINE VAR w-tarjetaDB LIKE Ahorros.TarjetaDb  NO-UNDO.

DEFINE TEMP-TABLE tmp-tarjetadb LIKE tarjetadebito.

/* Variables Para Imprimir en Excel */
DEFINE VAR InputFile AS CHARACTER NO-UNDO.
DEFINE VAR SwExiste AS CHARACTER NO-UNDO.
DEFINE VAR chExcelApp AS COM-HANDLE NO-UNDO.
DEFINE VAR hWorkBooks AS COM-HANDLE NO-UNDO.
DEFINE VAR ValCol AS CHARACTER NO-UNDO.
DEFINE VAR Dato AS CHARACTER NO-UNDO.
DEFINE VAR PrinterName AS CHARACTER NO-UNDO.
DEFINE VAR chWorksheet AS COM-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb-cedula Cmb-TarjetaDb R-Operacion ~
Btn_Cancelar Btn_Done RECT-338 RECT-339 RECT-337 
&Scoped-Define DISPLAYED-FIELDS Tarjetas.Cue_Ahorros Tarjetas.Num_Credito ~
Tarjetas.Fec_Activacion Tarjetas.Fec_ActCupo Tarjetas.MontoMaxCaj ~
Tarjetas.OperMaxCaj Tarjetas.MontoMaxPos Tarjetas.OperMaxPos 
&Scoped-define DISPLAYED-TABLES Tarjetas
&Scoped-define FIRST-DISPLAYED-TABLE Tarjetas
&Scoped-Define DISPLAYED-OBJECTS Cmb-cedula Cmb-TarjetaDb W_NomTitular ~
R-Operacion W_NomAgencia 

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
     IMAGE-UP FILE "imagenes/warning.gif":U
     LABEL "&Cancelar" 
     SIZE 8 BY 1.92
     FONT 4.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/exit01.ico":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "&Salvar" 
     SIZE 8 BY 1.92
     FONT 4.

DEFINE VARIABLE Cmb-cedula AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomAgencia AS CHARACTER FORMAT "X(60)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81 TOOLTIP "Agencia de matrícula"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .81 TOOLTIP "Nombre del Titular de la Tarjeta Débito"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-Operacion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Bloquear", 1,
"Cambiar Cuenta Ahorro", 2,
"Cambiar Cuenta Cupo", 3,
"Cambiar Montos", 4,
"Anular", 5
     SIZE 24 BY 3.04 NO-UNDO.

DEFINE RECTANGLE RECT-337
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.65.

DEFINE RECTANGLE RECT-338
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 3.62.

DEFINE RECTANGLE RECT-339
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 3.46.

DEFINE VARIABLE Cmb-TarjetaDb AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 22 BY 3.27 TOOLTIP "Tarjetas Débitos Existentes" NO-UNDO.

DEFINE VARIABLE Cmb-NewCupo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nueva Cuenta Cupo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE Cmb-NewCta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nueva Cuenta Ahorro" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 18.29 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Cmb-cedula AT ROW 2 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     Cmb-TarjetaDb AT ROW 2.04 COL 27 NO-LABEL WIDGET-ID 36
     W_NomTitular AT ROW 6 COL 3 NO-LABEL WIDGET-ID 14
     Tarjetas.Cue_Ahorros AT ROW 7.08 COL 46.14 COLON-ALIGNED WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 14 BY .81 TOOLTIP "Cuenta de Ahorro"
          BGCOLOR 18 FGCOLOR 15 
     Tarjetas.Num_Credito AT ROW 8.12 COL 46.14 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     R-Operacion AT ROW 2.12 COL 52 NO-LABEL WIDGET-ID 88
     Btn_Cancelar AT ROW 3.88 COL 77.43 WIDGET-ID 16
     Btn_Salvar AT ROW 1.92 COL 77.43 WIDGET-ID 18
     Tarjetas.Fec_Activacion AT ROW 7.08 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .81 TOOLTIP "Fecha de Activación de la cuenta de Ahorro"
          BGCOLOR 18 FGCOLOR 15 
     W_NomAgencia AT ROW 7.15 COL 10.43 COLON-ALIGNED WIDGET-ID 66
     Tarjetas.Fec_ActCupo AT ROW 8.08 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 82
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Done AT ROW 5.85 COL 77.43 WIDGET-ID 20
     Btn-Visualizar AT ROW 7.85 COL 77.43 WIDGET-ID 8
     Tarjetas.MontoMaxCaj AT ROW 10.73 COL 16.14 COLON-ALIGNED WIDGET-ID 96
          VIEW-AS FILL-IN 
          SIZE 16 BY .81 TOOLTIP "Monto Máximo a Retirar en Cajeros"
     Tarjetas.OperMaxCaj AT ROW 10.65 COL 68.43 COLON-ALIGNED NO-LABEL WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 5 BY .81 TOOLTIP "Número Máximo de Retiros en Cajeros"
     Tarjetas.MontoMaxPos AT ROW 11.69 COL 16 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .81 TOOLTIP "Monto Máximo a Pagar en POS"
     Tarjetas.OperMaxPos AT ROW 11.73 COL 68.43 COLON-ALIGNED NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 5 BY .81 TOOLTIP "Número Máximo de Pagos en Pos"
     "Tipo de Acción" VIEW-AS TEXT
          SIZE 17.72 BY .81 AT ROW 1.12 COL 50.86 WIDGET-ID 98
     "Fec Activación" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 6 COL 62.29 WIDGET-ID 84
     " Configuración de Valores" VIEW-AS TEXT
          SIZE 25.14 BY .81 AT ROW 9.88 COL 3 WIDGET-ID 52
          FGCOLOR 1 
     "Nro.Oper.Pos:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 11.85 COL 56 WIDGET-ID 48
     "Nro.Oper.Cajero:" VIEW-AS TEXT
          SIZE 16.14 BY .81 AT ROW 10.77 COL 53.86 WIDGET-ID 46
     "Cédula" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.12 COL 3 WIDGET-ID 22
     "Nro.Tarjeta Débito" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 1.08 COL 28.14 WIDGET-ID 28
     RECT-338 AT ROW 5.73 COL 2 WIDGET-ID 72
     RECT-339 AT ROW 1.92 COL 51 WIDGET-ID 92
     RECT-337 AT ROW 10.19 COL 2 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85 BY 17.5 WIDGET-ID 100.

DEFINE FRAME Frm-NuevaCta
     Cmb-NewCta AT ROW 1.15 COL 20.43 COLON-ALIGNED WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 34.57 ROW 6.54
         SIZE 41.14 BY 1.35 WIDGET-ID 200.

DEFINE FRAME Frm-Creditos
     Cmb-NewCupo AT ROW 1.19 COL 20.57 COLON-ALIGNED WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 34.57 ROW 7.88
         SIZE 41.14 BY 1.27 WIDGET-ID 300.


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
         TITLE              = "Novedades Plástico - W-NovedadesPlastico.w"
         HEIGHT             = 12.12
         WIDTH              = 84.86
         MAX-HEIGHT         = 17.5
         MAX-WIDTH          = 91.86
         VIRTUAL-HEIGHT     = 17.5
         VIRTUAL-WIDTH      = 91.86
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn-Visualizar:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Tarjetas.Cue_Ahorros IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Tarjetas.Cue_Ahorros:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN Tarjetas.Fec_ActCupo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Tarjetas.Fec_ActCupo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN Tarjetas.Fec_Activacion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Tarjetas.Fec_Activacion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN Tarjetas.MontoMaxCaj IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tarjetas.MontoMaxPos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tarjetas.Num_Credito IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Tarjetas.Num_Credito:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN Tarjetas.OperMaxCaj IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tarjetas.OperMaxPos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomAgencia IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       W_NomAgencia:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME Frm-Creditos
                                                                        */
ASSIGN 
       FRAME Frm-Creditos:HIDDEN           = TRUE
       FRAME Frm-Creditos:SENSITIVE        = FALSE.

/* SETTINGS FOR FRAME Frm-NuevaCta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frm-NuevaCta:HIDDEN           = TRUE
       FRAME Frm-NuevaCta:SENSITIVE        = FALSE.

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
ON END-ERROR OF C-Win /* Novedades Plástico - W-NovedadesPlastico.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Novedades Plástico - W-NovedadesPlastico.w */
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

  RUN ImprimirExcelReporteNovedades.
   /*
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  */
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
    DO WITH FRAME F-main:
        ASSIGN Cmb-tarjetaDb
               Cmb-Cedula
               R-Operacion.

        FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.

        IF Cmb-cedula NE "" AND Cmb-TarjetaDb NE "" THEN DO:
            Grabando:
            DO TRANSACTION ON ERROR UNDO Grabando:
                FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-ERROR.
                IF AVAILABLE(tarjetas) THEN DO:
                    IF R-operacion = 1 THEN DO: /* Bloqueo de cuenta */
                        FIND FIRST Cfg_tarjetadb NO-LOCK NO-ERROR.
                        FIND FIRST ahorros WHERE ahorros.nit = tarjetas.nit
                                             AND ahorros.cue_ahorros = Tarjetas.cue_ahorro NO-ERROR.
                        IF AVAILABLE(ahorros) THEN DO:
                            ASSIGN ahorros.tarjetadb = ""
                                   ahorros.Fec_Creatdb = ?
                                   ahorros.Fec_BloqueoTdb = W_Fecha.

                            RELEASE ahorros.

                            ASSIGN Tarjetas.Fec_Bloqueo = W_Fecha
                                   Tarjetas.Usuario_Bloqueo = W_Usuario
                                   Tarjetas.Estado = "41"  /* Bloqueada  */
                                   R-Operacion = 1
                                   R-Operacion:SCREEN-VALUE = STRING(R-Operacion).

                            CREATE Hoja_Vida.
                            ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                                   Hoja_Vida.Codigo = 5
                                   Hoja_Vida.DoctoRefer = DECIMAL(Tarjetas.Cue_ahorros)
                                   Hoja_Vida.Fec_Grabacion = W_Fecha
                                   Hoja_Vida.Hora_Grabacion = TIME
                                   Hoja_Vida.Nit = Tarjetas.Nit
                                   Hoja_Vida.Observacion = "Nov.Bloqueo Plástico - " + TRIM(STRING(Tarjetas.TarjetaDB))
                                   Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_Tipo
                                   Hoja_Vida.Usuario = W_Usuario.

                            CREATE Bitacora_Tdb.
                            ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_Tipo
                                   Bitacora_Tdb.Codigo = 5  /* Bloqueo Plasticos */
                                   Bitacora_Tdb.Nit = Tarjetas.nit
                                   Bitacora_Tdb.Usuario = W_Usuario
                                   Bitacora_Tdb.Observacion = "Libreria Webcaja " + Tarjetas.TarjetaDb
                                   Bitacora_Tdb.Fecha = W_Fecha
                                   Bitacora_Tdb.Hora = STRING(TIME,"HH:MM:SS")
                                   Bitacora_Tdb.Cue_ahorros = Tarjetas.Cue_Ahorros
                                   Bitacora_Tdb.Num_credito = Tarjetas.Num_credito
                                   Bitacora_Tdb.Usu_Inicial = Tarjetas.Usuario
                                   Bitacora_Tdb.Usu_Final = Tarjetas.Usuario
                                   Bitacora_Tdb.Age_Inicial = Tarjetas.Agencia
                                   Bitacora_Tdb.Age_Final = Tarjetas.Agencia
                                   Bitacora_Tdb.Tar_Inicial = Tarjetas.TarjetaDb
                                   Bitacora_Tdb.Tar_Final = Tarjetas.TarjetaDb
                                   Bitacora_Tdb.Cantidad = 1.

                            /*RUN WC_BlqPan.r (INPUT Tarjetas.TarjetaDb). /* Marca como bloqueada en el Switch */*/
                        END.
                    END.
                    
                    IF R-operacion = 2 THEN DO: /* Bloqueo de cuenta */
                        /* Cambio de Cuenta de Ahorros */
                        /* Liberar anterior Cuenta */
                        FIND FIRST Ahorros WHERE Ahorros.Cod_Ahorro = Cfg_TarjetaDB.Cod_Ahorro
                                             AND Ahorros.Cue_Ahorro = Tarjetas.Cue_ahorros
                                             AND Ahorros.nit = Cmb-Cedula NO-ERROR.
                        IF AVAILABLE(Ahorros) THEN
                            ASSIGN Ahorros.TarjetaDb = ""
                                   Ahorros.Fec_CancTdb = W_Fecha
                                   Ahorros.Fec_CreaTdb = ?.

                        ASSIGN Tarjetas.Novedad = 02  /* Llevar como novedad en Tarjetas */
                               Tarjetas.Cta_AhorrosAnt = Tarjetas.Cta_Ahorros
                               Tarjetas.Cue_ahorros = TRIM(Cmb-NewCta)
                               Tarjetas.Cta_ahorros = TRIM(Cfg_tarjetaDb.NroConvTarjDB) + TRIM(Cmb-NewCta).
                        
                        /* Crear Nueva Cuenta TarjetaDB */
                        FIND FIRST Ahorros WHERE Ahorros.Cod_Ahorro = Cfg_TarjetaDB.Cod_Ahorro
                                             AND Ahorros.Cue_Ahorro = Tarjetas.Cue_ahorros
                                             AND Ahorros.nit = Cmb-Cedula NO-ERROR.
                        IF AVAILABLE(Ahorros) THEN
                            ASSIGN Ahorros.TarjetaDb = Tarjetas.TarjetaDB
                                   Ahorros.Fec_CreaTdb = W_Fecha
                                   Ahorros.Fec_CancTdb = ?.

                        CREATE Hoja_Vida.
                        ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                               Hoja_Vida.Codigo = 6
                               Hoja_Vida.DoctoRefer = DECIMAL(Tarjetas.Cue_ahorros)
                               Hoja_Vida.Fec_Grabacion = W_Fecha
                               Hoja_Vida.Hora_Grabacion = TIME
                               Hoja_Vida.Nit = Tarjetas.Nit
                               Hoja_Vida.Observacion = "Plástico" + Tarjetas.TarjetaDB + " - Nueva - " + TRIM(Tarjetas.Cue_ahorros) + " " + " Anterior - " + TRIM(STRING(Tarjetas.Cta_AhorrosAnt))
                               Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_Tipo
                               Hoja_Vida.Usuario = W_Usuario.

                        CREATE Bitacora_Tdb.
                        ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_Tipo
                               Bitacora_Tdb.Codigo =  6  /* Cargar y Relacionar Plasticos */
                               Bitacora_Tdb.Nit = Tarjetas.Nit
                               Bitacora_Tdb.Usuario = W_Usuario
                               Bitacora_Tdb.Observacion = "Anterior: " + TRIM(STRING(Tarjetas.Cta_AhorrosAnt)) + " " + "Nueva: " + TRIM(Tarjetas.Cue_ahorros)
                               Bitacora_Tdb.Fecha = W_Fecha
                               Bitacora_Tdb.Hora = STRING(TIME,"HH:MM:SS")
                               Bitacora_Tdb.Cue_ahorros = Tarjetas.Cue_ahorros
                               Bitacora_Tdb.Num_credito = DECIMAL(Tarjetas.Cta_AhorrosAnt)
                               Bitacora_Tdb.Usu_Inicial = Tarjetas.Usuario
                               Bitacora_Tdb.Usu_Final = Tarjetas.Usuario
                               Bitacora_Tdb.Age_Inicial = Tarjetas.agencia
                               Bitacora_Tdb.Age_Final = Tarjetas.agencia
                               Bitacora_Tdb.Tar_Inicial = Tarjetas.TarjetaDb
                               Bitacora_Tdb.Tar_Final = Tarjetas.TarjetaDb
                               Bitacora_Tdb.Cantidad = 1.
                    END.

                    IF R-operacion = 3 THEN DO: /* Bloqueo de cuenta */
                        FIND FIRST Creditos WHERE Creditos.Cod_Credito EQ Cfg_TarjetaDB.Cod_Credito
                                              AND Creditos.Num_credito EQ Tarjetas.Num_Credito
                                              AND Creditos.nit EQ Cmb-Cedula NO-ERROR.
                        IF NOT AVAILABLE(Creditos) THEN
                            FIND FIRST Creditos WHERE Creditos.Cod_Credito EQ Cfg_TarjetaDB.Cod_Credito2
                                                  AND Creditos.Num_credito EQ Tarjetas.Num_Credito
                                                  AND Creditos.nit EQ Cmb-Cedula NO-ERROR.

                        IF AVAILABLE(Creditos) THEN
                            ASSIGN Creditos.TarjetaDb = ""
                                   Creditos.Fec_CancTdb = W_Fecha
                                   Creditos.Fec_CreaTdb = ?.

                        ASSIGN Tarjetas.NovedadCupo = 02  /* Llevar como novedad en Tarjetas */
                               Tarjetas.Cta_CupoAnt = Tarjetas.Cta_Cupo
                               Tarjetas.Num_Credito = INT64(Cmb-NewCupo)
                               Tarjetas.Cta_Cupo = TRIM(Cfg_tarjetaDb.NroConvTarjDB) + TRIM(STRING(int64(Cmb-NewCupo) + 2000000000)).

                        RELEASE creditos.

                        /* Crear Nueva Cuenta TarjetaDB */
                        FIND FIRST Creditos WHERE Creditos.Cod_Credito EQ Cfg_TarjetaDB.Cod_Credito
                                              AND Creditos.Num_Credito EQ Tarjetas.Num_Credito
                                              AND Creditos.Nit EQ Cmb-Cedula NO-ERROR.
                        IF NOT AVAILABLE(Creditos) THEN
                            FIND FIRST Creditos WHERE Creditos.Cod_Credito EQ Cfg_TarjetaDB.Cod_Credito2
                                                  AND Creditos.Num_Credito EQ Tarjetas.Num_Credito
                                                  AND Creditos.Nit EQ Cmb-Cedula NO-ERROR.

                        IF AVAILABLE(Creditos) THEN
                            ASSIGN Creditos.TarjetaDb = Tarjetas.TarjetaDB
                                   Creditos.Fec_CreaTdb = W_Fecha
                                   Creditos.Fec_CancTdb = ?.

                        CREATE Hoja_Vida.
                        ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                               Hoja_Vida.Codigo = 9
                               Hoja_Vida.DoctoRefer = Tarjetas.Num_Credito
                               Hoja_Vida.Fec_Grabacion = W_Fecha
                               Hoja_Vida.Hora_Grabacion = TIME
                               Hoja_Vida.Nit = Tarjetas.Nit
                               Hoja_Vida.Observacion = "Plástico" + Tarjetas.TarjetaDB + " - Nueva - " + TRIM(STRING(Tarjetas.Num_Credito)) + " " + " Anterior - " + TRIM(STRING(Tarjetas.Cta_CupoAnt))
                               Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_Tipo
                               Hoja_Vida.Usuario = W_Usuario.

                        CREATE Bitacora_Tdb.
                        ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_Tipo
                               Bitacora_Tdb.Codigo = 9  /* Cargar y Relacionar Plasticos */
                               Bitacora_Tdb.Nit = Tarjetas.Nit
                               Bitacora_Tdb.Usuario = W_Usuario
                               Bitacora_Tdb.Observacion = "Anterior: " + TRIM(STRING(Tarjetas.Cta_CupoAnt)) + " " + "Nueva: " + TRIM(STRING(Tarjetas.Num_credito))
                               Bitacora_Tdb.Fecha = W_Fecha
                               Bitacora_Tdb.Hora = STRING(TIME,"HH:MM:SS")
                               Bitacora_Tdb.Cue_ahorros = STRING(Tarjetas.Num_credito)
                               /*Bitacora_Tdb.Num_credito   = DECIMAL(Tarjetas.Cta_CupoAnt)*/
                               Bitacora_Tdb.Usu_Inicial = Tarjetas.Usuario
                               Bitacora_Tdb.Usu_Final = Tarjetas.Usuario
                               Bitacora_Tdb.Age_Inicial = Tarjetas.agencia
                               Bitacora_Tdb.Age_Final = Tarjetas.agencia
                               Bitacora_Tdb.Tar_Inicial = Tarjetas.TarjetaDb
                               Bitacora_Tdb.Tar_Final = Tarjetas.TarjetaDb
                               Bitacora_Tdb.Cantidad = 1.
                    END.

                    IF R-Operacion = 4 THEN DO:
                        tarjetas.MontoMaxCaj = DECIMAL(tarjetas.MontoMaxCaj:SCREEN-VALUE).
                        tarjetas.MontoMaxPos = DECIMAL(tarjetas.MontoMaxPos:SCREEN-VALUE).
                        tarjetas.OperMaxCaj = DECIMAL(tarjetas.OperMaxCaj:SCREEN-VALUE).
                        tarjetas.OperMaxPos = DECIMAL(tarjetas.OperMaxPos:SCREEN-VALUE).
                        tarjetas.fec_actMonto = w_fecha.
                        /*tarjetas.hora_actMonto = TIME.*/
                        tarjetas.usuario_ActMonto = w_usuario.

                        CREATE Hoja_Vida.
                        ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                               Hoja_Vida.Codigo = 10
                               Hoja_Vida.DoctoRefer = DECIMAL(Tarjetas.Cue_ahorros)
                               Hoja_Vida.Fec_Grabacion = W_Fecha
                               Hoja_Vida.Hora_Grabacion = TIME
                               Hoja_Vida.Nit = Tarjetas.Nit
                               Hoja_Vida.Observacion = "Cambio de Monto - " + TRIM(STRING(Tarjetas.TarjetaDB))
                               Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_Tipo
                               Hoja_Vida.Usuario = W_Usuario.
                    END.

                    IF R-Operacion = 5 THEN DO:
                        FIND FIRST Cfg_tarjetadb NO-LOCK NO-ERROR.
                        FIND FIRST ahorros WHERE ahorros.cod_ahorro = cfg_tarjetaDb.Cod_Ahorro
                                             AND ahorros.nit = tarjetas.nit
                                             AND ahorros.cue_ahorros = Tarjetas.cue_ahorro NO-ERROR.
                        IF AVAILABLE(ahorros) THEN DO:
                            ASSIGN ahorros.tarjetadb = ""
                                   ahorros.Fec_Creatdb = ?
                                   ahorros.Fec_BloqueoTdb = W_Fecha.

                            RELEASE ahorros.

                            ASSIGN Tarjetas.Fec_Bloqueo = W_Fecha
                                   Tarjetas.Usuario_Bloqueo = W_Usuario
                                   Tarjetas.Estado = "99"  /* Anulada */
                                   R-Operacion = 1
                                   R-Operacion:SCREEN-VALUE = STRING(R-Operacion).
                        
                            CREATE Hoja_Vida.
                            ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                                   Hoja_Vida.Codigo = 5
                                   Hoja_Vida.DoctoRefer = DECIMAL(Tarjetas.Cue_ahorros)
                                   Hoja_Vida.Fec_Grabacion = W_Fecha
                                   Hoja_Vida.Hora_Grabacion = TIME
                                   Hoja_Vida.Nit = Tarjetas.Nit
                                   Hoja_Vida.Observacion = "Nov.Anula Plástico - " + TRIM(STRING(Tarjetas.TarjetaDB))
                                   Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_Tipo
                                   Hoja_Vida.Usuario = W_Usuario.
                        END.
                    END.

                    RELEASE Creditos.
                    RELEASE Ahorros.
                    RELEASE Tarjetas.
                END.
                ELSE
                    MESSAGE "No se encontró tarjeta Débito Nro: " Cmb-tarjetaDb " Para la Operación"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                APPLY "CHOOSE" TO btn-visualizar.
            END.

            RUN inicializar_variables.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-cedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-cedula C-Win
ON LEAVE OF Cmb-cedula IN FRAME F-Main
DO:
    DO WITH FRAME F-main:
        DISABLE btn_Salvar  WITH FRAME f-main.

        FIND Clientes WHERE Clientes.Nit EQ Cmb-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ELSE DO:
            RUN C-Clientes.R(INPUT 2,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            ASSIGN Cmb-cedula:SCREEN-VALUE = P_Nit
                   W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).

            FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
        END.

        IF Clientes.Estado EQ 2 AND Clientes.Fec_Retiro NE ? THEN DO:
            MESSAGE "No se pueden crear cuentas de ahorro para clientes Retirados" SKIP
                    VIEW-AS ALERT-BOX WARNING.
            APPLY "choose" TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.

        ASSIGN W_NomAgencia:SCREEN-VALUE = ""
               Tarjetas.Fec_Activacion:SCREEN-VALUE = ""
               Tarjetas.Fec_ActCupo:SCREEN-VALUE = ""
               Tarjetas.Cue_Ahorros:SCREEN-VALUE = ""
               Tarjetas.Num_credito:SCREEN-VALUE = "".

        IF Clientes.Tipo_Vinculo GT 2 THEN DO:
            MESSAGE W_NomTitular " No es un cliente de la Cooperativa" SKIP
                    "La persona o empresa debe estar matriculado como" SKIP
                    "Cliente o Asociado. Rectifique!"
                VIEW-AS ALERT-BOX.
            APPLY 'choose' TO btn_Cancelar.
            RETURN NO-APPLY.
        END.

        ASSIGN Cmb-TarjetaDb:LIST-ITEMS = "".

        FOR EACH tarjetas WHERE /*Tarjetas.estado EQ "01"
                            AND*/ Tarjetas.Nit EQ Clientes.Nit
                            AND Tarjetas.Cue_Ahorros NE "" NO-LOCK BY Tarjetas.Nit:
            FIND FIRST ahorros WHERE ahorros.nit = tarjetas.nit
                                 AND ahorros.tarjetaDB = tarjetas.tarjetaDB NO-LOCK NO-ERROR.
            IF AVAILABLE ahorros THEN DO:
                W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).

                FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.

                ASSIGN W_NomAgencia:SCREEN-VALUE = STRING(agencias.agencia,"999") + " - " + TRIM(agencias.nombre)
                       Tarjetas.Fec_Activacion:SCREEN-VALUE = STRING(tarjetas.Fec_Activacion)
                       Tarjetas.Fec_ActCupo:SCREEN-VALUE = STRING(Tarjetas.Fec_ActCupo)
                       Tarjetas.Cue_Ahorros:SCREEN-VALUE = Tarjetas.Cue_ahorros
                       Tarjetas.Num_credito:SCREEN-VALUE = STRING(Tarjetas.Num_credito).
            END.
        END.

        DISABLE R-Operacion WITH FRAME F-Main.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm-NuevaCta
&Scoped-define SELF-NAME Cmb-NewCta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-NewCta C-Win
ON VALUE-CHANGED OF Cmb-NewCta IN FRAME Frm-NuevaCta /* Nueva Cuenta Ahorro */
DO:
  ASSIGN Cmb-NewCta.
  IF Cmb-NewCta NE "" THEN        
    ENABLE  btn_Salvar   WITH FRAME F-Main.
  ELSE  
    DISABLE  btn_Salvar  WITH FRAME F-Main.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Cmb-TarjetaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarjetaDb C-Win
ON VALUE-CHANGED OF Cmb-TarjetaDb IN FRAME F-Main
DO:
    ASSIGN Cmb-TarjetaDb
           Cmb-Cedula.

    FIND FIRST Tarjetas WHERE tarjetas.TarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.
    
    ASSIGN Cmb-Cedula:SCREEN-VALUE = Tarjetas.nit
           Cmb-Cedula.

    FIND Clientes WHERE Clientes.Nit EQ Cmb-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
    FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.

    IF AVAILABLE(Tarjetas) THEN DO:
        ASSIGN W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
               W_NomAgencia:SCREEN-VALUE = STRING(agencias.agencia,"999") + " - " + TRIM(agencias.nombre)
               Tarjetas.Fec_Activacion:SCREEN-VALUE = STRING(tarjetas.Fec_Activacion)
               Tarjetas.Fec_ActCupo:SCREEN-VALUE = STRING(Tarjetas.Fec_ActCupo)
               Tarjetas.Cue_Ahorros:SCREEN-VALUE = Tarjetas.Cue_ahorros
               Tarjetas.Num_credito:SCREEN-VALUE = STRING(Tarjetas.Num_credito).

        Tarjetas.MontoMaxCaj:SCREEN-VALUE = STRING(tarjetas.MontoMaxCaj).
        Tarjetas.MontoMaxPos:SCREEN-VALUE = STRING(tarjetas.MontoMaxPos).
        Tarjetas.OperMaxCaj:SCREEN-VALUE = STRING(tarjetas.OperMaxCaj).
        Tarjetas.OperMaxPos:SCREEN-VALUE = STRING(tarjetas.OperMaxPos).
    END.
    ELSE
        MESSAGE "No encontró " Tarjetas.TarjetaDb
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF Cmb-TarjetaDb NE "" AND Cmb-Cedula NE "" THEN DO:
        ENABLE Btn_salvar WITH FRAME F-Main.
        ENABLE R-Operacion WITH FRAME F-Main.
    END.
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
    IF DECIMAL(SELF:SCREEN-VALUE) > cfg_tarjetaDB.MontoMaxPos OR DECIMAL(SELF:SCREEN-VALUE) < cfg_tarjetaDB.MontoMinPos THEN DO:
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
    IF DECIMAL(SELF:SCREEN-VALUE) > cfg_tarjetaDB.OperMaxCaj OR DECIMAL(SELF:SCREEN-VALUE) < cfg_tarjetaDB.OperMinCaj THEN DO:
        MESSAGE "Este valor no está dentro de los rangos permitidos." SKIP
                "Por favor, verifique y corrija!!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        SELF:SCREEN-VALUE = STRING(0).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-Operacion C-Win
ON VALUE-CHANGED OF R-Operacion IN FRAME F-Main
DO:
    DEFI VAR w-habilitar  AS LOGICAL INITIAL FALSE.

    FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.
    FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-ERROR.

    tarjetas.MontoMaxCaj:SENSITIVE = FALSE.
    tarjetas.MontoMaxPos:SENSITIVE = FALSE.
    tarjetas.OperMaxCaj:SENSITIVE = FALSE.
    tarjetas.OperMaxPos:SENSITIVE = FALSE.

    tarjetas.MontoMaxCaj:SCREEN-VALUE = STRING(tarjetas.MontoMaxCaj).
    tarjetas.MontoMaxPos:SCREEN-VALUE = STRING(tarjetas.MontoMaxPos).
    tarjetas.OperMaxCaj:SCREEN-VALUE = STRING(tarjetas.OperMaxCaj).
    tarjetas.OperMaxPos:SCREEN-VALUE = STRING(tarjetas.OperMaxPos).

    ASSIGN R-Operacion.

    IF R-Operacion = 1 THEN
        ENABLE btn_Salvar WITH FRAME F-Main.

    IF R-Operacion = 2 THEN DO:
        ASSIGN Cmb-NewCta:LIST-ITEMS IN FRAME Frm-NuevaCta = "".

        IF AVAILABLE(tarjetas) THEN DO:
            FOR EACH ahorros WHERE Ahorros.Cod_Ahorro = Cfg_TarjetaDB.Cod_Ahorro
                               AND Ahorros.Nit = Tarjetas.nit
                               AND Ahorros.estado = 1
                               AND Ahorros.TarjetaDB = "" NO-LOCK:
                ASSIGN W_Ok = Cmb-NewCta:ADD-LAST(TRIM(Ahorros.Cue_Ahorros))
                       Cmb-NewCta:SCREEN-VALUE = TRIM(Ahorros.Cue_Ahorros)
                       w-Habilitar = TRUE.

                IF NOT W-Habilitar THEN DO:
                    MESSAGE "Esta Cédula " TRIM(Cmb-Cedula) " No tiene cuentas de ahorros" SKIP
                            "habilitadas para adjudicarle una Tarjeta Débito"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    DISABLE btn_Salvar WITH FRAME F-Main.

                    ASSIGN FRAME Frm-NuevaCta:VISIBLE = FALSE
                           FRAME Frm-NuevaCta:SENSITIVE = FALSE.
                END.
                ELSE DO:
                    ASSIGN FRAME Frm-NuevaCta:VISIBLE = TRUE
                           FRAME Frm-NuevaCta:SENSITIVE = TRUE.
                END.
            END.
        END.
    END.

    IF R-Operacion = 3 THEN DO:
        ASSIGN Cmb-NewCupo:LIST-ITEMS IN FRAME Frm-Creditos = "".

        IF AVAILABLE(tarjetas) THEN DO:
            FOR EACH Creditos WHERE (Creditos.Cod_Credito EQ Cfg_TarjetaDB.Cod_Credito OR
                                     Creditos.Cod_Credito EQ Cfg_TarjetaDB.Cod_Credito2)
                                AND Creditos.Nit EQ Tarjetas.nit
                                AND Creditos.TarjetaDB EQ "" NO-LOCK:
                ASSIGN W_Ok = Cmb-NewCupo:ADD-LAST(TRIM(STRING(Creditos.Num_Credito)))
                       Cmb-NewCupo:SCREEN-VALUE = TRIM(STRING(Creditos.Num_Credito))
                       w-Habilitar = TRUE.
            END.
        END.

        IF NOT W-Habilitar THEN DO:
            MESSAGE "Esta Cédula " TRIM(Cmb-Cedula) " No tiene cuentas de Cupo Rotativo" SKIP
                    "habilitadas para adjudicarle una Tarjeta Débito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            DISABLE btn_Salvar WITH FRAME F-Main.

            ASSIGN FRAME Frm-Creditos:VISIBLE = FALSE
                   FRAME Frm-Creditos:SENSITIVE = FALSE.
        END.
    END.

    IF R-Operacion = 4 THEN DO:
        tarjetas.MontoMaxCaj:SENSITIVE = TRUE.
        tarjetas.MontoMaxPos:SENSITIVE = TRUE.
        tarjetas.OperMaxCaj:SENSITIVE = TRUE.
        tarjetas.OperMaxPos:SENSITIVE = TRUE.
    END.

    IF R-Operacion = 5 THEN DO:
        tarjetas.MontoMaxCaj:SENSITIVE = FALSE.
        tarjetas.MontoMaxPos:SENSITIVE = FALSE.
        tarjetas.OperMaxCaj:SENSITIVE = FALSE.
        tarjetas.OperMaxPos:SENSITIVE = FALSE.
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
  DISPLAY Cmb-cedula Cmb-TarjetaDb W_NomTitular R-Operacion W_NomAgencia 
      WITH FRAME F-Main IN WINDOW C-Win.
  IF AVAILABLE Tarjetas THEN 
    DISPLAY Tarjetas.Cue_Ahorros Tarjetas.Num_Credito Tarjetas.Fec_Activacion 
          Tarjetas.Fec_ActCupo Tarjetas.MontoMaxCaj Tarjetas.OperMaxCaj 
          Tarjetas.MontoMaxPos Tarjetas.OperMaxPos 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE Cmb-cedula Cmb-TarjetaDb R-Operacion Btn_Cancelar Btn_Done RECT-338 
         RECT-339 RECT-337 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY Cmb-NewCta 
      WITH FRAME Frm-NuevaCta IN WINDOW C-Win.
  ENABLE Cmb-NewCta 
      WITH FRAME Frm-NuevaCta IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-NuevaCta}
  FRAME Frm-NuevaCta:SENSITIVE = NO.
  DISPLAY Cmb-NewCupo 
      WITH FRAME Frm-Creditos IN WINDOW C-Win.
  ENABLE Cmb-NewCupo 
      WITH FRAME Frm-Creditos IN WINDOW C-Win.
  VIEW FRAME Frm-Creditos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Creditos}
  FRAME Frm-Creditos:SENSITIVE = NO.
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirExcelReporteNovedades C-Win 
PROCEDURE ImprimirExcelReporteNovedades :
IF R-Operacion <> 4 THEN DO:
    InputFile = "Formatos\AV - 319.xls".

    SwExiste = SEARCH(InputFile).

    IF SwExiste EQ ? THEN DO:
        MESSAGE InputFile "no encontrado."
            VIEW-AS ALERT-BOX.
        RETURN.
    END.

    FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
    FIND FIRST Clientes WHERE Clientes.Nit = cmb-cedula:SCREEN-VALUE IN FRAME F-Main NO-LOCK NO-ERROR.
    FIND FIRST Agencias WHERE Agencias.Agencia = w_agencia NO-LOCK NO-ERROR.

    CREATE "Excel.Application" chExcelApp.

    hWorkBooks = chExcelApp:WorkBooks:OPEN(SwExiste,,TRUE,).

    IF hWorkBooks THEN DO:
        chExcelApp:Visible = TRUE.
        chWorkSheet = chExcelApp:Sheets:Item(1).
    END.
    ELSE
        SwExiste = ?.

    ASSIGN ValCol = "A5"
           Dato = Agencias.Nombre
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "O5"
           Dato = Cmb-TarjetaDB
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "V5"
           Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "Y26"
           Dato = Clientes.nit
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "B27"
           Dato = usuarios.nombre
           chWorkSheet:Range(ValCol):VALUE = Dato.
END.
ELSE DO:
    InputFile = "Formatos\AV - 323.xls".

    SwExiste = SEARCH(InputFile).

    IF SwExiste EQ ? THEN DO:
        MESSAGE InputFile "no encontrado."
            VIEW-AS ALERT-BOX.
        RETURN.
    END.

    FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
    FIND FIRST Clientes WHERE Clientes.Nit = cmb-cedula:SCREEN-VALUE IN FRAME F-Main NO-LOCK NO-ERROR.
    FIND FIRST Agencias WHERE Agencias.Agencia = w_agencia NO-LOCK NO-ERROR.
    FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-ERROR.

    CREATE "Excel.Application" chExcelApp.

    hWorkBooks = chExcelApp:WorkBooks:OPEN(SwExiste,,TRUE,).

    IF hWorkBooks THEN DO:
        chExcelApp:Visible = TRUE.
        chWorkSheet = chExcelApp:Sheets:Item(1).
    END.
    ELSE
        SwExiste = ?.

    ASSIGN ValCol = "D5"
           Dato = Agencias.Nombre
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "B15"
           Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "AE15"
           Dato = Clientes.nit
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "D26"
           Dato = Clientes.dir_residencia
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "AA17"
           Dato = Cmb-TarjetaDB
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "W21"
           Dato = STRING(tarjetas.MontoMaxCaj)
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "AF21"
           Dato = STRING(tarjetas.OperMaxCaj)
           chWorkSheet:Range(ValCol):VALUE = Dato.
    
    ASSIGN ValCol = "K23"
           Dato = STRING(tarjetas.MontoMaxPos)
           chWorkSheet:Range(ValCol):VALUE = Dato.

    ASSIGN ValCol = "T23"
           Dato = STRING(tarjetas.OperMaxPos)
           chWorkSheet:Range(ValCol):VALUE = Dato.
    
    ASSIGN ValCol = "AI31"
           Dato = usuarios.nombre
           chWorkSheet:Range(ValCol):VALUE = Dato.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.

ASSIGN Cmb-Cedula:SCREEN-VALUE IN FRAME f-main = "" Cmb-cedula
       W-TarjetaDb = ""
       w_NomTitular:SCREEN-VALUE = ""
       W_NomTitular
       Cmb-TarjetaDb:LIST-ITEMS = ""
       Cmb-TarjetaDb
       W_NomAgencia:SCREEN-VALUE = ""
       Tarjetas.Fec_Activacion:SCREEN-VALUE = ""
       Tarjetas.Fec_ActCupo:SCREEN-VALUE = ""
       Tarjetas.Cue_Ahorros:SCREEN-VALUE = ""
       Tarjetas.Num_credito:SCREEN-VALUE = ""
       R-Operacion:SCREEN-VALUE = "1"
       R-Operacion.

       ASSIGN FRAME Frm-NuevaCta:VISIBLE = FALSE
              FRAME Frm-NuevaCta:SENSITIVE = FALSE
              Cmb-NewCta:LIST-ITEMS = "".

       ASSIGN FRAME Frm-Creditos:VISIBLE = FALSE
              FRAME Frm-Creditos:SENSITIVE = FALSE
              Cmb-NewCupo:LIST-ITEMS = "".

       DISABLE R-Operacion WITH FRAME F-Main.
       DISABLE Btn_Salvar WITH FRAME F-Main.

       ASSIGN Cmb-TarjetaDb:LIST-ITEMS = "".
/*        FOR EACH tarjetas WHERE Tarjetas.estado         EQ "01"         AND                     */
/*                                Tarjetas.Cue_Ahorros    NE ""          /* AND                   */
/*                                TArjetas.Fec_Activacion EQ W_Fecha */ NO-LOCK BY Tarjetas.Nit:  */
/*           W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).                     */
/*        END.                                                                                    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}.  
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 IF R-Operacion =  1 THEN DO:
     W_Reporte    = "REPORTE   : BLOQUEO DE PLASTICOS :  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
     FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.
     IF AVAILABLE(tarjetas) THEN DO:
         W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Tarjetas.tarjetaDb) + "           Bloqueada el " + STRING(Tarjetas.Fec_Bloqueo,"99/99/9999").
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-Ftr.
         FIND FIRST clientes WHERE clientes.nit        = Tarjetas.nit NO-LOCK NO-ERROR.
         IF AVAILABLE(Clientes) THEN
             DISPLAY "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Tarjetas.Nit) AT 1  FORMAT "X(80)"
                     "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                     WITH FRAME F-mov0 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
         FIND FIRST Ahorros  WHERE Ahorros.Cod_Ahorro  = Cfg_TarjetaDb.cod_Ahorro AND
                                   Ahorros.nit         = Tarjetas.nit             AND
                                   Ahorros.Cue_ahorro  = Tarjetas.Cue_ahorro      NO-LOCK NO-ERROR.
         IF AVAILABLE(Ahorros) THEN DO:
           FIND FIRST agencias WHERE agencias.agencia = Ahorros.agencia  NO-LOCK NO-ERROR.
           DISPLAY "Cuenta de Ahorros#: " + TRIM(STRING(Ahorros.Cue_ahorros))  AT 1  FORMAT "X(80)"
                   "Agencia Cta Ahorro: " + TRIM(Agencias.nombre) AT 1  FORMAT "X(80)"    
                    WITH FRAME F-mov1 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

         END.                                                                                                                             
         IF Tarjetas.Num_Credito NE 0  THEN DO:
           FIND FIRST Creditos WHERE  Creditos.Cod_Credito EQ Cfg_TarjetaDb.cod_Credito AND
                                      Creditos.nit         EQ Tarjetas.nit              AND
                                      Creditos.Num_credito EQ Tarjetas.Num_Credito NO-LOCK NO-ERROR.
           IF NOT AVAILABLE(Creditos) THEN
             FIND FIRST Creditos WHERE  Creditos.Cod_Credito EQ Cfg_TarjetaDb.cod_Credito2 AND
                                        Creditos.nit         EQ Tarjetas.nit               AND
                                        Creditos.Num_credito EQ Tarjetas.Num_Credito NO-LOCK NO-ERROR.

           IF AVAILABLE(Creditos) THEN DO:
             FIND FIRST agencias WHERE agencias.agencia = Creditos.agencia  NO-LOCK NO-ERROR.
             DISPLAY "Número de Crédito#: " + TRIM(STRING(Creditos.Num_Credito))  AT 1  FORMAT "X(80)"
                     "Agencia de Crédito: " + TRIM(Agencias.nombre) AT 1  FORMAT "X(80)"    
                      WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

           END.                                                                                                                             
         END.
          PUT " " SKIP(2).
         PAGE.
     END.
 END.
 ELSE DO:
     W_Reporte    = "REPORTE   : CAMBIO DE CUENTA DE AHORRO :  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
     FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.
     IF AVAILABLE(tarjetas) THEN DO:
         W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Tarjetas.tarjetaDb) + "           Cambia Cta Ahorros " + STRING(W_Fecha,"99/99/9999").
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-Ftr.
         FIND FIRST clientes WHERE clientes.nit        = Tarjetas.nit NO-LOCK NO-ERROR.
         IF AVAILABLE(Clientes) THEN
             DISPLAY "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Tarjetas.Nit) AT 1  FORMAT "X(80)"
                     "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                     WITH FRAME F-mov3 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
         FIND FIRST Ahorros  WHERE Ahorros.Cod_Ahorro  = Cfg_TarjetaDb.cod_Ahorro AND
                                   Ahorros.nit         = Tarjetas.nit             AND
                                   Ahorros.Cue_ahorro  = Tarjetas.Cue_ahorro      NO-LOCK NO-ERROR.
         IF AVAILABLE(Ahorros) THEN DO:
           FIND FIRST agencias WHERE agencias.agencia = Ahorros.agencia  NO-LOCK NO-ERROR.
           DISPLAY "Cuenta de Ahorros# : " + TRIM(STRING(Ahorros.Cue_ahorros))  AT 1  FORMAT "X(80)"
                   "Agencia Cta Ahorro : " + TRIM(Agencias.nombre) AT 1  FORMAT "X(80)"    
                   "Anterior Cta Ahorro: " + SUBSTRING(Tarjetas.Cta_AhorrosAnt,4,14) AT 1 FORMAT "X(80)"
                    WITH FRAME F-mov4 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

         END.                                                                                                                             
         PUT " " SKIP(2).
         PAGE.
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

