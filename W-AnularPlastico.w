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
  DEFINE VARIABLE P_Nit       LIKE Clientes.Nit        NO-UNDO.
  DEFINE VARIABLE p_Nombre    LIKE Clientes.Nombre     NO-UNDO.
  DEFINE VARIABLE P_Apellido  LIKE Clientes.Apellido1  NO-UNDO.
  DEFINE VARIABLE P_AgeCli    LIKE Clientes.Agencia    NO-UNDO.
  DEFINE VARIABLE w_ok        AS LOGICAL               NO-UNDO.
  DEFINE VARIABLE w-tarjetaDB LIKE Ahorros.TarjetaDb   NO-UNDO.
  DEFINE VARIABLE choice      AS LOGICAL INITIAL FALSE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-338 Btn_Cancelar Cmb-Cedula ~
Cmb-TarjetaDb W_NomAgencia Btn_Done R-Anular 
&Scoped-Define DISPLAYED-FIELDS Tarjetas.Cue_Ahorros ~
Tarjetas.Fec_Activacion Tarjetas.Fec_ActCupo Tarjetas.Num_Credito 
&Scoped-define DISPLAYED-TABLES Tarjetas
&Scoped-define FIRST-DISPLAYED-TABLE Tarjetas
&Scoped-Define DISPLAYED-OBJECTS Cmb-Cedula Cmb-TarjetaDb W_NomTitular ~
W_NomAgencia R-Anular 

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

DEFINE VARIABLE W_NomAgencia AS CHARACTER FORMAT "X(60)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81 TOOLTIP "Agencia de matrícula"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .81 TOOLTIP "Nombre del Titular de la Tarjeta Débito"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-Anular AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Anular Cta Ahorro", 1,
"Anular Cupo Rotativo", 2
     SIZE 48.72 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-338
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 3.58.

DEFINE VARIABLE Cmb-Cedula AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24.29 BY .81 TOOLTIP "Nro. Cédula del Asociado" NO-UNDO.

DEFINE VARIABLE Cmb-TarjetaDb AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 22 BY .81 TOOLTIP "Tarjetas Débitos Existentes" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Cancelar AT ROW 1.04 COL 77.43 WIDGET-ID 16
     Cmb-Cedula AT ROW 2.08 COL 2.72 NO-LABEL WIDGET-ID 76
     Cmb-TarjetaDb AT ROW 2.08 COL 29.72 NO-LABEL WIDGET-ID 36
     Btn_Salvar AT ROW 2.92 COL 77.43 WIDGET-ID 18
     W_NomTitular AT ROW 3.42 COL 3 NO-LABEL WIDGET-ID 14
     Tarjetas.Cue_Ahorros AT ROW 4.5 COL 46.14 COLON-ALIGNED WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 14 BY .81 TOOLTIP "Cuenta de Ahorro"
          BGCOLOR 18 FGCOLOR 15 
     Tarjetas.Fec_Activacion AT ROW 4.5 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .81 TOOLTIP "Fecha de Activación de la cuenta de Ahorro"
          BGCOLOR 18 FGCOLOR 15 
     W_NomAgencia AT ROW 4.58 COL 10.43 COLON-ALIGNED WIDGET-ID 66
     Btn_Done AT ROW 4.85 COL 77.43 WIDGET-ID 20
     Tarjetas.Fec_ActCupo AT ROW 5.5 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 82
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Tarjetas.Num_Credito AT ROW 5.54 COL 46.14 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     R-Anular AT ROW 6.81 COL 2.29 NO-LABEL WIDGET-ID 78
     Btn-Visualizar AT ROW 6.92 COL 77.43 WIDGET-ID 8
     "Fec Activación" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 3.42 COL 62.29 WIDGET-ID 84
     "Cédula" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.27 COL 3 WIDGET-ID 22
     "Nro.Tarjeta Débito" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 1.23 COL 29.72 WIDGET-ID 28
     RECT-338 AT ROW 3.15 COL 2 WIDGET-ID 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85 BY 6.77 WIDGET-ID 100.


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
         TITLE              = "Anular Plástico - W-AnularPlastico.w"
         HEIGHT             = 6.73
         WIDTH              = 85
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 86
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 86
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

/* SETTINGS FOR FILL-IN Tarjetas.Num_Credito IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Tarjetas.Num_Credito:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Anular Plástico - W-AnularPlastico.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Anular Plástico - W-AnularPlastico.w */
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
               R-Anular.

        FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.

        MESSAGE "Esta seguro de ANULAR la cuenta seleccionada ?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.

        IF choice THEN DO:
            Grabando:
            DO TRANSACTION ON ERROR UNDO Grabando:
                FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-ERROR.
                IF AVAILABLE(tarjetas) THEN DO:
                    IF R-Anular = 1 THEN DO:
                        FIND FIRST ahorros WHERE Ahorros.cod_ahorro = Cfg_Tarjeta.Cod_Ahorro
                                             AND Ahorros.nit = Cmb-Cedula
                                             AND Ahorros.cue_ahorros = Tarjetas.Cue_Ahorro NO-ERROR.
                        IF AVAILABLE(ahorros) THEN DO:
                            ASSIGN ahorros.tarjetadb = ""
                                   ahorros.Fec_Creatdb = ?.

                            CREATE Hoja_Vida.
                            ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                                   Hoja_Vida.Codigo = 7
                                   Hoja_Vida.DoctoRefer = DECIMAL(Tarjetas.Cue_Ahorro)
                                   Hoja_Vida.Fec_Grabacion = W_Fecha
                                   Hoja_Vida.Hora_Grabacion = TIME
                                   Hoja_Vida.Nit = Cmb-Cedula
                                   Hoja_Vida.Observacion = "Tarjeta Db: " + TRIM(Cmb-TarjetaDb)
                                   Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_tipo
                                   Hoja_Vida.Usuario = W_Usuario.

                            CREATE Bitacora_Tdb.
                            ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_tipo
                                   Bitacora_Tdb.Codigo = 7  /* Anulación Plastico: Cta-Ahorro */
                                   Bitacora_Tdb.Nit = Tarjetas.nit
                                   Bitacora_Tdb.Usuario = W_Usuario
                                   Bitacora_Tdb.Observacion = TRIM(Tarjetas.Cue_Ahorros)
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
                        END.

                        RELEASE Ahorros.
                    END.

                    IF R-Anular = 2 THEN DO:
                        FIND FIRST Creditos WHERE Creditos.cod_Credito = Cfg_Tarjeta.Cod_Credito
                                              AND Creditos.nit = Cmb-Cedula
                                              AND Creditos.Num_credito = Tarjetas.Num_credito NO-ERROR.
                        IF NOT AVAILABLE(Creditos) THEN
                            FIND FIRST Creditos WHERE Creditos.cod_Credito = Cfg_Tarjeta.Cod_Credito2
                                                  AND Creditos.nit = Cmb-Cedula
                                                  AND Creditos.Num_credito = Tarjetas.Num_credito NO-ERROR.
                        
                        IF AVAILABLE(Creditos) THEN DO:
                            ASSIGN Creditos.tarjetadb = ""
                                   Creditos.Fec_Creatdb = ?
                                   Creditos.Estado = 1
                                   Creditos.Fec_Desembolso = ?.
                        END.

                        RELEASE Creditos.
                        
                        CREATE Hoja_Vida.
                        ASSIGN Hoja_Vida.Asunto_Cumplido = TRUE
                               Hoja_Vida.Codigo = 8
                               Hoja_Vida.DoctoRefer = Tarjetas.Num_Credito
                               Hoja_Vida.Fec_Grabacion = W_Fecha
                               Hoja_Vida.Hora_Grabacion = TIME
                               Hoja_Vida.Nit = Cmb-Cedula
                               Hoja_Vida.Observacion = "Tarjeta Db: " + TRIM(Tarjetas.TarjetaDb)
                               Hoja_Vida.Tipo = Cfg_TarjetaDb.Varios_tipo
                               Hoja_Vida.Usuario = W_Usuario.

                        CREATE Bitacora_Tdb.
                        ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_tipo
                               Bitacora_Tdb.Codigo = 8  /* Anulación Plastico: Cta-Cupo */
                               Bitacora_Tdb.Nit = Tarjetas.nit
                               Bitacora_Tdb.Usuario = W_Usuario
                               Bitacora_Tdb.Observacion = TRIM(string(Tarjetas.Num_credito))
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
                    END.

                    ASSIGN tarjetas.nit = "" WHEN R-Anular = 1
                           tarjetas.Num_Credito = 0
                           Tarjetas.Cta_Cupo = ""
                           Tarjetas.Fec_ActCupo = ?
                           Tarjetas.Hora_ActCupo = 0
                           Tarjetas.Fec_Activacion = ? WHEN R-Anular = 1
                           Tarjetas.Hora_activacion = 0 WHEN R-Anular = 1
                           Tarjetas.Estado = '00' WHEN R-Anular = 1
                           Tarjetas.Usuario = W_Usuario
                           Tarjetas.Nombres = "" WHEN R-Anular = 1.
                END.
                ELSE
                    MESSAGE "No se encontró tarjeta Débito Nro: " Cmb-tarjetaDb
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RELEASE tarjetas.

                FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.
                
                APPLY "CHOOSE" TO btn-visualizar.
            END.

            RUN inicializar_variables.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-Cedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-Cedula C-Win
ON VALUE-CHANGED OF Cmb-Cedula IN FRAME F-Main
DO:
    DO WITH FRAME F-main:
        DISABLE btn_Salvar WITH FRAME f-main.

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
                    "Cliente o Asociado. Rectifique!" VIEW-AS ALERT-BOX.
            APPLY 'choose' TO btn_Cancelar.
            RETURN NO-APPLY.
        END.

        ASSIGN Cmb-TarjetaDb:LIST-ITEMS = "".

        FOR EACH tarjetas WHERE Tarjetas.estado EQ "01"
                            AND Tarjetas.Nit EQ Clientes.Nit
                            AND Tarjetas.Cue_Ahorros NE ""
                            AND (Tarjetas.Fec_Activacion EQ W_Fecha OR
                                 Tarjetas.Fec_ActCupo EQ W_Fecha) NO-LOCK:
            W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).

            FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.
            
            ASSIGN W_NomAgencia:SCREEN-VALUE = STRING(agencias.agencia,"999") + " - " + TRIM(agencias.nombre)
                   Tarjetas.Fec_Activacion:SCREEN-VALUE = STRING(tarjetas.Fec_Activacion)
                   Tarjetas.Fec_ActCupo:SCREEN-VALUE = STRING(Tarjetas.Fec_ActCupo)
                   Tarjetas.Cue_Ahorros:SCREEN-VALUE = Tarjetas.Cue_ahorros
                   Tarjetas.Num_credito:SCREEN-VALUE = STRING(Tarjetas.Num_credito).

            IF Tarjetas.Fec_ActCupo NE ? THEN
                ASSIGN R-Anular:SCREEN-VALUE = "2" R-anular.
            ELSE
                ASSIGN R-Anular:SCREEN-VALUE = "1" R-anular.
        END.

        DISABLE R-Anular WITH FRAME F-Main.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-TarjetaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarjetaDb C-Win
ON VALUE-CHANGED OF Cmb-TarjetaDb IN FRAME F-Main
DO:
    ASSIGN Cmb-TarjetaDb.

    ENABLE btn_Salvar WITH FRAME F-Main.

    FIND FIRST Tarjetas WHERE tarjetas.TarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.

    ASSIGN Cmb-Cedula:SCREEN-VALUE = Tarjetas.nit
           Cmb-Cedula.

    FIND Clientes WHERE Clientes.Nit EQ Cmb-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
    FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.
    
    IF AVAILABLE(Tarjetas) THEN
        ASSIGN W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
               W_NomAgencia:SCREEN-VALUE   = STRING(agencias.agencia,"999") + " - " + TRIM(agencias.nombre)
               Tarjetas.Fec_Activacion:SCREEN-VALUE = STRING(tarjetas.Fec_Activacion)
               Tarjetas.Fec_ActCupo:SCREEN-VALUE = STRING(Tarjetas.Fec_ActCupo)
               Tarjetas.Cue_Ahorros:SCREEN-VALUE = Tarjetas.Cue_ahorros
               Tarjetas.Num_credito:SCREEN-VALUE = STRING(Tarjetas.Num_credito).
    ELSE
        MESSAGE "No encontró " Tarjetas.TarjetaDb
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF Cmb-TarjetaDb NE "" AND Cmb-Cedula NE "" THEN
        ENABLE R-Anular WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-Anular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-Anular C-Win
ON VALUE-CHANGED OF R-Anular IN FRAME F-Main
DO:
  ASSIGN R-anular.
  ENABLE btn_Salvar  WITH FRAME F-Main.
  IF R-anular = 1 AND (Tarjetas.Fec_Activacion NE W_Fecha  OR Tarjetas.Fec_ActCupo EQ W_Fecha ) THEN
    ASSIGN  R-Anular = 2  R-Anular:SCREEN-VALUE = "2".
  IF R-Anular = 2 AND decimal(Tarjetas.Num_credito:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Sólo se anula relación en caso de tener Cupo Rotativo"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN R-Anular = 1  R-Anular:SCREEN-VALUE = "1".
     RETURN.
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
  DISPLAY Cmb-Cedula Cmb-TarjetaDb W_NomTitular W_NomAgencia R-Anular 
      WITH FRAME F-Main IN WINDOW C-Win.
  IF AVAILABLE Tarjetas THEN 
    DISPLAY Tarjetas.Cue_Ahorros Tarjetas.Fec_Activacion Tarjetas.Fec_ActCupo 
          Tarjetas.Num_Credito 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE RECT-338 Btn_Cancelar Cmb-Cedula Cmb-TarjetaDb W_NomAgencia Btn_Done 
         R-Anular 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.

ASSIGN Cmb-Cedula:LIST-ITEMS IN FRAME f-main = "" Cmb-cedula
       W-TarjetaDb = ""
       w_NomTitular:SCREEN-VALUE = ""
       W_NomTitular
       Cmb-TarjetaDb:LIST-ITEMS = ""
       Cmb-TarjetaDb
       W_NomAgencia:SCREEN-VALUE = ""
       Tarjetas.Fec_Activacion:SCREEN-VALUE = ""
       Tarjetas.Fec_ActCupo:SCREEN-VALUE = ""
       Tarjetas.Cue_Ahorros:SCREEN-VALUE = ""
       Tarjetas.Num_credito:SCREEN-VALUE = "".

DISABLE R-Anular WITH FRAME F-Main.
DISABLE Btn_Salvar WITH FRAME F-Main.

ASSIGN Cmb-TarjetaDb:LIST-ITEMS = ""
       Cmb-Cedula = "".

FOR EACH tarjetas WHERE Tarjetas.estado EQ "01"
                    AND Tarjetas.Cue_Ahorros NE ""
                    AND (TArjetas.Fec_Activacion EQ W_Fecha OR
                         Tarjetas.Fec_Actcupo EQ W_FEcha) NO-LOCK:
    W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).
    W_Ok = Cmb-Cedula:ADD-LAST(TRIM(Tarjetas.Nit)).
END.

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
 DO WITH FRAME F-MAIN:
     IF R-anular = 1 THEN DO: /* Cuenta de Ahorros  */
         W_Reporte    = "REPORTE   : ANULACION DE PLASTICOS : CTA AHORROS  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
         FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb:SCREEN-VALUE NO-LOCK NO-ERROR.
         IF AVAILABLE(tarjetas) THEN DO:
             W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Tarjetas.TarjetaDb) + "  -   Anulada el " + STRING(w_fecha,"99/99/9999").
             VIEW FRAME F-Encabezado.
             VIEW FRAME F-Ftr.
             FIND FIRST Ahorros  WHERE Ahorros.Cod_Ahorro  = Cfg_TarjetaDb.cod_Ahorro  AND
                                       Ahorros.nit         = Cmb-cedula:SCREEN-VALUE   AND
                                       Ahorros.cue_ahorros = Tarjetas.Cue_ahorros:SCREEN-VALUE NO-LOCK NO-ERROR.
             FIND FIRST agencias WHERE agencias.agencia = Ahorros.agencia             NO-LOCK NO-ERROR.
             FIND FIRST clientes WHERE clientes.nit     = Cmb-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF AVAILABLE(Clientes) THEN DO:
                 DISPLAY "Número Cta Ahorro#: " + TRIM(STRING(Ahorros.Cue_ahorros))  AT 1  FORMAT "X(80)"
                         "Agencia CtaAhorro : " + TRIM(Agencias.nombre)              AT 1  FORMAT "X(80)"    
                         "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Clientes.nit) AT 1  FORMAT "X(80)"
                         "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                         WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

            END.                                                                                                                             
            PUT " Tarjeta ha sido liberada de la relación por Cuenta de Ahorros" SKIP(2).
            PAGE.
         END.

     END.
     ELSE DO:
         W_Reporte    = "REPORTE   : ANULACION DE PLASTICOS : CUPO ROTATIVO  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
         FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb:SCREEN-VALUE NO-LOCK NO-ERROR.
         IF AVAILABLE(tarjetas) THEN DO:                                                                     
             W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Cmb-TarjetaDb:SCREEN-VALUE) + "     Anulada el " + STRING(w_fecha,"99/99/9999").
             VIEW FRAME F-Encabezado.
             VIEW FRAME F-Ftr.
             FIND FIRST Creditos WHERE Creditos.Cod_Credito = Cfg_TarjetaDb.cod_Credito  AND
                                       Creditos.nit         = Cmb-cedula:SCREEN-VALUE    AND
                                       Creditos.Num_credito = INT64(TARJETAS.Num_Credito:SCREEN-VALUE) NO-LOCK NO-ERROR.
             FIND FIRST agencias WHERE agencias.agencia = Creditos.agencia  NO-LOCK NO-ERROR.
             FIND FIRST clientes WHERE clientes.nit     = Cmb-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF AVAILABLE(Clientes) THEN DO:
                 DISPLAY "Número de Crédito#: " + TRIM(STRING(Creditos.Num_Credito))  AT 1  FORMAT "X(80)"
                         "Agencia de Crédito: " + TRIM(Agencias.nombre) AT 1  FORMAT "X(80)"    
                         "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Clientes.Nit) AT 1  FORMAT "X(80)"
                         "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                         WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

            END.                                                                                                                             
            PUT " Tarjeta ha sido liberada de la relación por Cupo Rotativo" SKIP(2).
            PAGE.
         END.
     END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

