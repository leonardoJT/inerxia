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
  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

  DEFINE VARIABLE AgeOrigen   AS INTEGER FORMAT "999" INITIAL 0   NO-UNDO.
  DEFINE VARIABLE AgeDestino  AS INTEGER FORMAT "999" INITIAL 999 NO-UNDO.
  DEFINE VARIABLE UsuOrigen  LIKE usuarios.usuario                NO-UNDO.
  DEFINE VARIABLE UsuDestino  LIKE usuarios.usuario               NO-UNDO.
  DEFINE VAR W_Ok AS LOGICAL                                      NO-UNDO.
  DEFINE VAR w-CantLim AS INTEGER INITIAL 0    NO-UNDO.
  DEFINE VAR w-RanTdb1 AS DECIMAL INITIAL 0    NO-UNDO.
  DEFINE VAR w-RanTdb2 AS DECIMAL INITIAL 0    NO-UNDO.
  DEFINE VAR w-tarIni  LIKE tarjetas.tarjetadb NO-UNDO.
  DEFINE VAR w-tarfin  LIKE tarjetas.tarjetadb NO-UNDO.
  DEFINE VAR choice    AS LOGICAL INITIAL NO   NO-UNDO.

  DEFINE VAR oficinaInicial AS INTEGER.
  DEFINE VAR oficinaFinal AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS w-nrotarjfin w-nrotarjini Cmb_AgeOrigen ~
Btn-Cancelar Btn-Salvar Btn_Done RECT-332 
&Scoped-Define DISPLAYED-OBJECTS w-nrotarjfin w-nrotarjini Cmb_AgeOrigen ~
w-CantTarj w-usuorigen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Cancelar 
     LABEL "Cancela" 
     SIZE 9 BY 1.92.

DEFINE BUTTON Btn-Salvar 
     LABEL "Bloquear" 
     SIZE 9 BY 1.92.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 9 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE Cmb_AgeOrigen AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w-CantTarj AS INTEGER FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81 TOOLTIP "Cantidad de tarjetas Disponibles" NO-UNDO.

DEFINE VARIABLE w-usuorigen AS CHARACTER FORMAT "X(60)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81 TOOLTIP "Usuario quién posee las tarjetas"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-332
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52.29 BY 11.04.

DEFINE VARIABLE w-nrotarjfin AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 26 BY 2.96 NO-UNDO.

DEFINE VARIABLE w-nrotarjini AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 26 BY 2.96 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     w-nrotarjfin AT ROW 7.77 COL 19.29 NO-LABEL WIDGET-ID 162
     w-nrotarjini AT ROW 3.42 COL 19.29 NO-LABEL WIDGET-ID 160
     Cmb_AgeOrigen AT ROW 2.27 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     w-CantTarj AT ROW 6.77 COL 17.29 COLON-ALIGNED WIDGET-ID 134
     w-usuorigen AT ROW 11.23 COL 15 COLON-ALIGNED WIDGET-ID 158
     Btn-Cancelar AT ROW 5.77 COL 55.14 WIDGET-ID 128
     Btn-Salvar AT ROW 7.85 COL 55.14 WIDGET-ID 42
     Btn_Done AT ROW 10 COL 55.29 WIDGET-ID 38
     " Agencia Origen" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 1.35 COL 3.72 WIDGET-ID 140
          FGCOLOR 1 
     "Nro.Tarj.Inicial:" VIEW-AS TEXT
          SIZE 14 BY .81 TOOLTIP "Tarjeta para el rango inicial" AT ROW 3.42 COL 4.72 WIDGET-ID 164
     "Nro.Tarj.Final:" VIEW-AS TEXT
          SIZE 14 BY .81 TOOLTIP "Tarjeta del rango final" AT ROW 7.73 COL 5 WIDGET-ID 166
     RECT-332 AT ROW 1.81 COL 1 WIDGET-ID 136
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 63.29 BY 12.04 WIDGET-ID 100.


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
         TITLE              = "Bloquear Plásticos Disponibles -  w-BloquearPlast_Disponible.w"
         HEIGHT             = 12.12
         WIDTH              = 63.29
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 146.29
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FRAME F-MAIN
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN w-CantTarj IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-usuorigen IN FRAME F-MAIN
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bloquear Plásticos Disponibles -  w-BloquearPlast_Disponible.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bloquear Plásticos Disponibles -  w-BloquearPlast_Disponible.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancelar C-Win
ON CHOOSE OF Btn-Cancelar IN FRAME F-MAIN /* Cancela */
DO:
  RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salvar C-Win
ON CHOOSE OF Btn-Salvar IN FRAME F-MAIN /* Bloquear */
DO:
    DO WITH FRAME F-MAIN:
        ASSIGN Cmb_AgeOrigen
               w-nrotarjIni
               w-nrotarjFin
               w-cantTarj
               w-usuorigen.

        FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.

        MESSAGE "Esta seguro de BLOQUEAR las tarjetas seleccionadas ?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.

        DEFINE VAR w-cont AS INTEGER INITIAL 0.

        IF choice THEN DO:
            IF w-cantTarj LE 0 THEN DO:
                MESSAGE "No se puede grabar Bloqueo, dado que la cantidad es cero" SKIP
                    VIEW-AS ALERT-BOX.
                RETURN NO-apply.
            END.

            FOR EACH tarjetas WHERE tarjetas.agencia = ageorigen
                                AND tarjetas.usuario = w_usuario
                                AND tarjetas.estado = "00"
                                AND substring(tarjetas.tarjetadb,1,15) GE substring(w-nrotarjIni,1,15)
                                AND substring(tarjetas.tarjetadb,1,15) LE substring(w-nrotarjFin,1,15):
                ASSIGN Tarjetas.Estado = "41".
            END.

/*          CREATE Hoja_Vida.                                                                        */
/*          ASSIGN Hoja_Vida.Asunto_Cumplido  = TRUE                                                 */
/*                 Hoja_Vida.Codigo           = 3  /* Bloqueo de plástico Nuevo */                   */
/*                 Hoja_Vida.DoctoRefer       = DECIMAL(w-nrotarjIni)                                */
/*                 Hoja_Vida.Fec_Grabacion    = W_Fecha                                              */
/*                 Hoja_Vida.Hora_Grabacion   = TIME                                                 */
/*                 Hoja_Vida.Observacion      = "Cantidad: " + STRING(w-cantTarj,"->>>,>>9") + " " + */
/*                                              "Tarj_Ini: " + TRIM(w-nrotarjIni) + " " +            */
/*                                              "Tarj_Fin: " + TRIM(W-NroTarjFin)                    */
/*                 Hoja_Vida.Tipo             = Cfg_TarjetaDb.Varios_Tipo                            */
/*                 Hoja_Vida.Usuario          = W_Usuario.                                           */

            CREATE Bitacora_Tdb.
            ASSIGN Bitacora_Tdb.Tipo = Cfg_TarjetaDb.Varios_Tipo
                   Bitacora_Tdb.Codigo = 3  /* Bloqueo de plástico Nuevo */
                   Bitacora_Tdb.Nit = ""
                   Bitacora_Tdb.Usuario = W_Usuario
                   Bitacora_Tdb.Observacion = "* Malas Condiciones"
                   Bitacora_Tdb.Fecha = W_Fecha
                   Bitacora_Tdb.Hora = STRING(TIME,"HH:MM:SS")
                   Bitacora_Tdb.Cue_ahorros = ""
                   Bitacora_Tdb.Num_credito = 0
                   Bitacora_Tdb.Usu_Inicial = SUBSTRING(W-UsuOrigen:SCREEN-VALUE,1,4)
                   Bitacora_Tdb.Usu_Final = SUBSTRING(W-UsuOrigen:SCREEN-VALUE,1,4)
                   Bitacora_Tdb.Age_Inicial = INTEGER(SUBSTRING(Cmb_AgeOrigen,1,3))
                   Bitacora_Tdb.Age_Final = INTEGER(SUBSTRING(Cmb_AgeOrigen,1,3))
                   Bitacora_Tdb.Tar_Inicial = W-NrotarjIni
                   Bitacora_Tdb.Tar_Final = W-NrotarjFin
                   Bitacora_Tdb.Cantidad = W-CantTarj.
        END.
    END.

    RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-MAIN /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_AgeOrigen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_AgeOrigen C-Win
ON VALUE-CHANGED OF Cmb_AgeOrigen IN FRAME F-MAIN
DO:
    ASSIGN FRAME F-main Cmb_Ageorigen.

    AgeOrigen = INTEGER(SUBSTRING(Cmb_AgeOrigen,1,3)).

    DEFINE VAR w-ntarini  LIKE tarjetas.tarjetadb NO-UNDO.
    DEFINE VAR w-ntarfin  LIKE tarjetas.tarjetadb NO-UNDO.

    ASSIGN W-NroTarjIni:LIST-ITEMS = ""
           W-NroTarjFin:LIST-ITEMS = "".

    FIND FIRST tarjetas WHERE tarjetas.estado = "00"
                          AND tarjetas.agencia EQ ageorigen USE-INDEX IdxTarj NO-LOCK NO-ERROR.
    IF AVAILABLE(tarjetas) THEN DO:
        ASSIGN W-UsuOrigen = tarjetas.usuario
               W-CantTarj = 0
               w-ntarini = tarjetas.tarjetadb.

        FOR EACH tarjetas WHERE tarjetas.estado = "00"
                            AND tarjetas.usuario = w_usuario
                            AND tarjetas.agencia EQ ageorigen USE-INDEX IdxTarj
                            /* AND Tarjetas.Usuario = W-usuOrigen */ NO-LOCK:
            ASSIGN W-CantTarj = W-CantTarj + 1
                   w-ntarfin = tarjetas.tarjetadb.
            
            W_Ok = W-NroTarjIni:ADD-LAST(Tarjetas.TarjetaDb).
            W_Ok = W-NroTarjFin:ADD-LAST(Tarjetas.TarjetaDb).
        END.

        ASSIGN w-NroTarjIni:SCREEN-VALUE = w-ntarini    w-NroTarjIni
               w-NroTarjFin:SCREEN-VALUE = w-ntarfin    w-NroTarjFin.

        FIND FIRST usuarios WHERE usuarios.usuario = w-usuOrigen NO-LOCK NO-ERROR.
        IF AVAILABLE(usuarios) THEN
            ASSIGN w-usuOrigen = w-usuorigen + " - " + TRIM(Usuarios.nombre)
                   W-UsuOrigen:SCREEN-VALUE = W-UsuOrigen.

        ASSIGN w-cantLim = W-CantTarj
               w-RanTdb1 = DECIMAL(SUBSTRING(W-NroTarjIni,1,15))
               W-RanTdb2 = DECIMAL(SUBSTRING(W-NroTarjFin,1,15))
               W-CantTarj:SCREEN-VALUE = "1" W-CantTarj
               W-NroTarjIni:SCREEN-VALUE = W-NTarIni W-NroTarjIni
               W-NroTarjFin:SCREEN-VALUE = W-NTarIni W-NroTarjFin
               w-tarini = W-NroTarjIni.
    END.
    ELSE DO:
        ASSIGN w-nrotarjini:SCREEN-VALUE = ""   w-nrotarjini = ""
               w-canttarj:SCREEN-VALUE = ""     w-canttarj = 0
               w-nrotarjfin:SCREEN-VALUE = ""   w-nrotarjfin = ""
               w-usuorigen:SCREEN-VALUE = ""    w-usuorigen = ""
               w-cantLim = 0
               w-RanTdb1 = 0
               W-RanTdb2 = 0
               w-tarini  = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-CantTarj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-CantTarj C-Win
ON LEAVE OF w-CantTarj IN FRAME F-MAIN /* Cantidad */
DO:
    ASSIGN w-nrotarjini
           w-nrotarjfin
           W-CantTarj.

    DEFINE VAR i AS integer INITIAL 0.

    FIND LAST TARJETAS WHERE tarjetas.estado = "00"
                         AND tarjetas.agencia = ageorigen
                         AND tarjetas.usuario = w_usuario NO-LOCK NO-ERROR.
    IF AVAILABLE(tarjetas) THEN
        ASSIGN w-nrotarjFin:SCREEN-VALUE = tarjetas.tarjetadb   w-nrotarjfin.

    IF DECIMAL(SUBSTRING(TRIM(w-nrotarjFin:SCREEN-VALUE),1,15)) GE w-RanTdb1 AND
       DECIMAL(SUBSTRING(TRIM(W-NroTarjFin:SCREEN-VALUE),1,15)) LE W-RanTdb2 THEN DO:
        IF DECIMAL(SUBSTRING(TRIM(W-NroTarjFin:SCREEN-VALUE),1,15)) LT DECIMAL(SUBSTRING(TRIM(W-NroTarjIni:SCREEN-VALUE),1,15)) THEN
            ASSIGN W-NroTarjIni:SCREEN-VALUE = W-NroTarjFin:SCREEN-VALUE
                   W-NroTarjIni.

        FOR EACH tarjetas WHERE tarjetas.estado = "00"
                            AND tarjetas.agencia = ageorigen
                            AND tarjetas.usuario = w_usuario
                            AND tarjetas.tarjetadb GE w-nrotarjini
                            AND tarjetas.tarjetadb LE w-nrotarjfin NO-LOCK:
            ASSIGN w-nrotarjfin:SCREEN-VALUE = tarjetadb  w-nrotarjfin.

            i = i + 1.
            
            IF (i + 1) GT w-canttarj THEN DO:
                ASSIGN W-CantTarj = i   W-CantTarj:SCREEN-VALUE = STRING(i).
                RETURN.
            END.
        END.

        IF i NE w-cantTarj THEN
            ASSIGN W-CantTarj = i   W-CantTarj:SCREEN-VALUE = STRING(i).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-nrotarjfin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-nrotarjfin C-Win
ON VALUE-CHANGED OF w-nrotarjfin IN FRAME F-MAIN
DO:
    ASSIGN w-nrotarjini
           w-nrotarjfin
           W-CantTarj = 0.

    IF DECIMAL(SUBSTRING(TRIM(w-nrotarjFin:SCREEN-VALUE),1,15)) GE w-RanTdb1 AND
       DECIMAL(SUBSTRING(TRIM(W-NroTarjFin:SCREEN-VALUE),1,15)) LE W-RanTdb2 THEN DO:
        IF DECIMAL(SUBSTRING(TRIM(W-NroTarjFin:SCREEN-VALUE),1,15)) LT DECIMAL(SUBSTRING(TRIM(W-NroTarjIni:SCREEN-VALUE),1,15)) THEN
            ASSIGN W-NroTarjIni:SCREEN-VALUE = W-NroTarjFin:SCREEN-VALUE
                   W-NroTarjIni.

        FOR EACH tarjetas WHERE tarjetas.estado = "00"
                            AND tarjetas.agencia = ageorigen
                            AND tarjetas.usuario = w_usuario
                            AND tarjetas.tarjetadb GE w-nrotarjini
                            AND tarjetas.tarjetadb LE w-nrotarjfin USE-INDEX IdxTarj NO-LOCK:
            ASSIGN W-CantTarj = W-CantTarj + 1
                   W-CantTarj:SCREEN-VALUE = STRING(W-CantTarj).
        END.

        /*       APPLY "choose" TO w-canttarj. */
    END.
    ELSE DO:
        MESSAGE "La tarjeta Inicial debe estar dentro de los rangos" SKIP
                "Rango Inicial: " W-RanTdb1 SKIP
                "Rango Final  : " W-RanTdb2
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN w-nrotarjini = w-TarIni   w-nrotarjini:SCREEN-VALUE = w-tarini.

        APPLY "leave" TO w-nrotarjini.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-nrotarjini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-nrotarjini C-Win
ON VALUE-CHANGED OF w-nrotarjini IN FRAME F-MAIN
DO:
    ASSIGN w-nrotarjini
           w-nrotarjfin
           w-canttarj = 0.

    IF DECIMAL(SUBSTRING(TRIM(w-nrotarjini:SCREEN-VALUE),1,15)) GE w-RanTdb1 AND
       DECIMAL(SUBSTRING(TRIM(W-NroTarjIni:SCREEN-VALUE),1,15)) LE W-RanTdb2 THEN DO:
        IF DECIMAL(SUBSTRING(TRIM(W-NroTarjFin:SCREEN-VALUE),1,15)) LT DECIMAL(SUBSTRING(TRIM(W-NroTarjIni:SCREEN-VALUE),1,15)) THEN
            ASSIGN W-NroTarjFin:SCREEN-VALUE = W-NroTarjIni:SCREEN-VALUE W-NroTarjFin.

        FOR EACH tarjetas WHERE tarjetas.estado = "00"
                            AND tarjetas.agencia EQ ageorigen
                            AND tarjetas.usuario = w_usuario
                            AND tarjetas.tarjetadb GE w-nrotarjini
                            AND tarjetas.tarjetadb LE w-nrotarjfin USE-INDEX IdxTarj NO-LOCK:
            ASSIGN W-CantTarj = W-CantTarj + 1
                   W-CantTarj:SCREEN-VALUE = STRING(W-CantTarj).
        END.

        ENABLE w-CantTarj WITH FRAME f-main.
    END.
    ELSE DO:
        MESSAGE "La tarjeta Inicial debe estar dentro de los rangos" SKIP
                "Rango Inicial: " W-RanTdb1 SKIP
                "Rango Final  : " W-RanTdb2
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN w-nrotarjini = w-TarIni   w-nrotarjini:SCREEN-VALUE = w-tarini.

        APPLY "leave" TO w-nrotarjini.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cantidad C-Win 
PROCEDURE Cantidad :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
DEFINE VAR I AS INTEGER INITIAL 0.
FIND FIRST tarjetas WHERE tarjetas.estado = "00" AND tarjetas.tarjetadb = w-nrotarjini NO-LOCK NO-ERROR.
FOR EACH tarjetas WHERE tarjetas.estado = "00" AND tarjetas.agencia = AgeOrigen AND tarjetas.tarjetadb GE w-nrotarjini NO-LOCK:
  IF i = w-canttarj THEN
    ASSIGN w-nrotarjfin:SCREEN-VALUE IN FRAME f-main = tarjetas.tarjetadb  w-NroTarjFin.
  ELSE
     IF i GT w-canttarj THEN RETURN.
  ASSIGN i = i + 1.
END.
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
  DISPLAY w-nrotarjfin w-nrotarjini Cmb_AgeOrigen w-CantTarj w-usuorigen 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE w-nrotarjfin w-nrotarjini Cmb_AgeOrigen Btn-Cancelar Btn-Salvar 
         Btn_Done RECT-332 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
DO WITH FRAME f-main:
    DEFINE VAR w-ntarini LIKE tarjetas.tarjetadb NO-UNDO.

    DEFINE VAR w-ntarfin LIKE tarjetas.tarjetadb NO-UNDO.

    ageorigen = w_agencia.

    FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.
    IF w_usuario = cfg_tarjetaDB.usuario THEN DO:
        oficinaInicial = 0.
        oficinaFinal = 999.
    END.
    ELSE DO:
        oficinaInicial = w_agencia.
        oficinaFinal = w_agencia.
    END.

    ASSIGN Cmb_AgeOrigen:LIST-ITEMS = ""
           W-CantTarj:SCREEN-VALUE = "0"
           w-cantTarj
           W-NroTarjIni:LIST-ITEMS = ""
           W-NroTarjFin:LIST-ITEMS = "".

    FOR EACH Agencias WHERE agencia.agencia >= oficinaInicial
                        AND agencias.agencia <= oficinaFinal
                        AND Agencias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_AgeOrigen:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).

        IF Agencias.Agencia EQ W_Agencia THEN
            ASSIGN Cmb_AgeOrigen:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
    END.

    FIND FIRST tarjetas WHERE tarjetas.estado = "00"
                          AND tarjetas.usuario = w_usuario
                          AND tarjetas.agencia EQ w_agencia USE-INDEX IdxTarj NO-LOCK NO-ERROR.

    IF AVAILABLE(tarjetas) THEN DO:
        ASSIGN W-UsuOrigen = tarjetas.usuario
               W-CantTarj = 0
               w-ntarini = tarjetas.tarjetadb.

        FOR EACH tarjetas WHERE tarjetas.estado = "00"
                            AND tarjetas.usuario = w_usuario
                            AND tarjetas.agencia EQ w_agencia USE-INDEX IdxTarj /* AND Tarjetas.Usuario = W-usuOrigen */ NO-LOCK:
            ASSIGN W-CantTarj = W-CantTarj + 1
                   w-ntarfin = tarjetas.tarjetadb.
                   W_Ok = W-NroTarjIni:ADD-LAST(Tarjetas.TarjetaDb).
                   W_Ok = W-NroTarjFin:ADD-LAST(Tarjetas.TarjetaDb).
        END.

        ASSIGN w-NroTarjIni:SCREEN-VALUE = w-ntarini
               w-NroTarjFin:SCREEN-VALUE = w-ntarfin.

        FIND FIRST usuarios WHERE usuarios.usuario = w-usuOrigen NO-LOCK NO-ERROR.
        IF AVAILABLE(usuarios) THEN
            ASSIGN w-usuOrigen = w-usuorigen + " - " + TRIM(Usuarios.nombre)
                   W-UsuOrigen:SCREEN-VALUE = W-UsuOrigen.

        ASSIGN W-NroTarjIni:SCREEN-VALUE = W-NTarIni
               W-NroTarjIni
               W-NroTarjFin:SCREEN-VALUE = W-Ntarfin
               W-NroTarjFin
               w-cantLim = W-CantTarj
               w-RanTdb1 = DECIMAL(SUBSTRING(W-NroTarjIni,1,15))
               W-RanTdb2 = DECIMAL(SUBSTRING(W-NroTarjFin,1,15))
               W-NroTarjFin:SCREEN-VALUE = W-NtarIni
               W-NroTarjFin
               W-CantTarj:SCREEN-VALUE = "1"
               W-CantTarj
               w-tarini = W-NroTarjIni.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

