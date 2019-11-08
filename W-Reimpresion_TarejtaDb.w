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

{Incluido\variable.i "shared"}
/* Local Variable Definitions ---                                       */
  DEFINE VARIABLE P_Nit       LIKE Clientes.Nit       NO-UNDO.
  DEFINE VARIABLE p_Nombre    LIKE Clientes.Nombre    NO-UNDO.
  DEFINE VARIABLE P_Apellido  LIKE Clientes.Apellido1 NO-UNDO.
  DEFINE VARIABLE P_AgeCli    LIKE Clientes.Agencia   NO-UNDO.
  DEFINE VARIABLE w_ok        AS LOGICAL              NO-UNDO.
  DEFINE VARIABLE w-tarjetaDB LIKE Ahorros.TarjetaDb  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cancelar R-tipo w-cedula Cmb-TarjetaDb ~
Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS R-tipo w-cedula Cmb-TarjetaDb W_NomTitular 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Visualizar 
     LABEL "Imprimir" 
     SIZE 8 BY 1.92 TOOLTIP "Visualizar por Pantalla / Impresora".

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 8 BY 1.92
     FONT 4.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE w-cedula AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 68 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuenta de Ahorro", 1,
"Cupo Rotativo", 2
     SIZE 20 BY 1.35 NO-UNDO.

DEFINE VARIABLE Cmb-TarjetaDb AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Cancelar AT ROW 1.04 COL 71 WIDGET-ID 16
     R-tipo AT ROW 1.54 COL 49 NO-LABEL WIDGET-ID 56
     w-cedula AT ROW 2.23 COL 2 NO-LABEL WIDGET-ID 12
     Cmb-TarjetaDb AT ROW 2.23 COL 21.72 NO-LABEL WIDGET-ID 36
     Btn-Visualizar AT ROW 3 COL 71 WIDGET-ID 8
     W_NomTitular AT ROW 3.31 COL 2 NO-LABEL WIDGET-ID 14
     Btn_Done AT ROW 5 COL 71 WIDGET-ID 20
     "Nro.Tarjeta Débito" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 1.27 COL 21.57 WIDGET-ID 28
     "Cédula" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.31 COL 7 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 5.96 WIDGET-ID 100.


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
         TITLE              = "Reimpresión Relación Plásticos - W-Reimpresion_TarejtaDb.w"
         HEIGHT             = 5.92
         WIDTH              = 78.43
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
/* SETTINGS FOR FILL-IN w-cedula IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Reimpresión Relación Plásticos - W-Reimpresion_TarejtaDb.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Reimpresión Relación Plásticos - W-Reimpresion_TarejtaDb.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME F-Main /* Imprimir */
DO:
    IF R-Tipo = 1 THEN DO:
        FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb
                              AND TRIM(Tarjetas.cue_AhorroS) NE "" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(tarjetas) THEN DO:
            MESSAGE "Esta tarjeta no tiene CUENTA DE AHORRO asignada"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END.
    ELSE DO:
        FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb
                              AND Tarjetas.Num_Credito NE 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(tarjetas) THEN DO:
            MESSAGE "Esta tarjeta no tiene CUPO ROTATIVO asignada"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END.

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


&Scoped-define SELF-NAME Cmb-TarjetaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarjetaDb C-Win
ON VALUE-CHANGED OF Cmb-TarjetaDb IN FRAME F-Main
DO:
    ASSIGN Cmb-TarjetaDb
           w-cedula.

    IF TRIM(Cmb-TarjetaDb:SCREEN-VALUE) NE "" AND w-cedula NE "" THEN
        ENABLE btn-Visualizar WITH FRAME f-main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-tipo C-Win
ON VALUE-CHANGED OF R-tipo IN FRAME F-Main
DO:
  ASSIGN R-Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-cedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cedula C-Win
ON LEAVE OF w-cedula IN FRAME F-Main
DO:
    DO WITH FRAME F-main:
        FIND Clientes WHERE Clientes.Nit EQ w-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ELSE DO:
            RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
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

        Cmb-TarjetaDb:LIST-ITEMS = "".
        FOR EACH tarjetas WHERE Tarjetas.estado = "01"
                            AND Tarjetas.Nit = Clientes.Nit NO-LOCK:
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
  DISPLAY R-tipo w-cedula Cmb-TarjetaDb W_NomTitular 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE Btn_Cancelar R-tipo w-cedula Cmb-TarjetaDb Btn_Done 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.
ASSIGN W-Cedula:SCREEN-VALUE IN FRAME f-main  = "" W-Cedula 
       W-TarjetaDb = ""
       w_NomTitular:SCREEN-VALUE = "" W_NomTitular
       Cmb-TarjetaDb:LIST-ITEMS = ""  Cmb-TarjetaDb.
    DISABLE Btn-Visualizar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}.

FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.

IF R-Tipo = 1 THEN DO:
    W_Reporte = "REPORTE   : REIMPRESION RELACION DE PLASTICOS : Cuenta de Ahorros  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

    FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb
                          AND TRIM(Tarjetas.cue_AhorroS) NE "" NO-LOCK NO-ERROR.
    IF AVAILABLE(tarjetas) THEN DO:
        W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Tarjetas.tarjetaDb) + "           Activada el " + STRING(Tarjetas.Fec_activacion,"99/99/9999").

        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.

        FIND FIRST ahorros WHERE Ahorros.Cod_ahorro = Cfg_TarjetaDb.cod_Ahorro
                             AND Ahorros.nit = Tarjetas.nit
                             AND Ahorros.cue_ahorros = Tarjetas.Cue_ahorros NO-LOCK NO-ERROR.
        FIND FIRST agencias WHERE agencias.agencia = Ahorros.agencia  NO-LOCK NO-ERROR.
        FIND FIRST clientes WHERE clientes.nit     = Tarjetas.nit NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN DO:
            DISPLAY "Cuenta de Ahorro #: " + Ahorros.Cue_Ahorros AT 1  FORMAT "X(80)"
                    "Agencia Cta Ahorro: " + TRIM(Agencias.nombre) AT 1  FORMAT "X(80)"
                    "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Tarjetas.Nit) AT 1  FORMAT "X(80)"
                    "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                WITH FRAME F-mov1 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
        END.

        PUT " " SKIP(2).

        DISPLAY Cfg_tarjetaDb.descripcion VIEW-AS EDITOR SIZE 105 BY 3 AT 3
            WITH FRAME fcode2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        PUT " " SKIP(5).

        DISPLAY "Firma del Asociado:   _________________________________________"  WITH FRAME fcode3 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        DISPLAY "                      " + TRIM(Clientes.Nombre) + " " + TRIM(clientes.apellido1)     + " " + TRIM(Clientes.apellido2) AT 1 FORMAT "X(80)"
                "                      " + Clientes.Tipo_Identificacion +  ": "  AT 1  FORMAT "X(80)"
            WITH FRAME F-mov4 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

        PUT " " SKIP(2).

        PAGE.
    END.
    ELSE
        MESSAGE "Esta tarjeta no tiene CUENTA DE AHORROS asignada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:
    W_Reporte = "REPORTE   : REIMPRESION RELACION DE PLASTICOS : Cupo Rotativo  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

    FIND FIRST tarjetas WHERE tarjetas.tarjetaDb = Cmb-TarjetaDb
                          AND Tarjetas.Num_credito NE 0 NO-LOCK NO-ERROR.
    IF AVAILABLE(tarjetas) THEN DO:
        W_EncColumna = "Tarjeta Débito Nro.: " + TRIM(Tarjetas.tarjetaDb) + "           Activada el " + STRING(Tarjetas.Fec_activacion,"99/99/9999").

        VIEW FRAME F-Encabezado.
        VIEW FRAME F-Ftr.

        FIND FIRST Creditos WHERE Creditos.Cod_Credito EQ Cfg_TarjetaDb.cod_Credito
                              AND Creditos.nit EQ Tarjetas.nit
                              AND Creditos.Num_credito EQ Tarjetas.Num_Credito NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(creditos) THEN
            FIND FIRST Creditos WHERE Creditos.Cod_Credito EQ Cfg_TarjetaDb.cod_Credito2
                                  AND Creditos.nit EQ Tarjetas.nit
                                  AND Creditos.Num_credito EQ Tarjetas.Num_Credito NO-LOCK NO-ERROR.
        
        FIND FIRST agencias WHERE agencias.agencia EQ Creditos.agencia  NO-LOCK NO-ERROR.
        FIND FIRST clientes WHERE clientes.nit EQ Tarjetas.nit NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN DO:
            DISPLAY "Número de Crédito#: " + TRIM(STRING(Creditos.Num_Credito))  AT 1  FORMAT "X(80)"
                    "Agencia de Crédito: " + TRIM(Agencias.nombre) AT 1  FORMAT "X(80)"
                    "Identificación Nro: " + Clientes.Tipo_Identificacion +  ": " + TRIM(Tarjetas.Nit) AT 1  FORMAT "X(80)"
                    "Nombres Completos : " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2) + " " + TRIM(Clientes.Nombre) AT 1 FORMAT "X(80)"
                WITH FRAME F-mov5 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
        END.

        PUT " " SKIP(2).

        DISPLAY Cfg_tarjetaDb.descripcion VIEW-AS EDITOR SIZE 105 BY 3 AT 3
            WITH FRAME fcode6 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        PUT " " SKIP(5).
        
        DISPLAY "Firma del Asociado:   _________________________________________"  WITH FRAME fcode7 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        DISPLAY "                      " + TRIM(Clientes.Nombre) + " " + TRIM(clientes.apellido1)     + " " + TRIM(Clientes.apellido2) AT 1 FORMAT "X(80)"
                "                      " + Clientes.Tipo_Identificacion +  ": "  AT 1  FORMAT "X(80)"
            WITH FRAME F-mov8 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

        PUT " " SKIP(2).
        
        PAGE.
    END.
    ELSE
        MESSAGE "Esta tarjeta no tiene CUPO ROTATIVO asignado"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

