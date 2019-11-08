&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
  DEFINE VAR W_Evento AS CHAR INITIAL " SALVAR ". 
 
  DEFINE VAR W_Cont   AS INTEGER FORMAT "9" INITIAL 0 NO-UNDO.
  DEFINE VAR W_Entrada AS CHARACTER.
  
  DEFINE SHARED VAR W_Agencia LIKE Agencias.Agencia.
  DEFINE SHARED VAR W_Cadena AS CHARACTER.
  DEFINE SHARED VAR W_Manija AS HANDLE.
  DEFINE SHARED VAR W_Eleccion AS LOGICAL.
  DEFINE SHARED VAR W_Fecha AS DATE.
  DEFINE SHARED VAR W_Clave Like Usuarios.Clave.
  DEFINE SHARED VAR W_Usuario Like Usuarios.Usuario.
  DEFINE SHARED VAR P-Valida  AS LOGICAL INITIAL FALSE.

  DEFINE VARIABLE I       AS INTEGER   FORMAT 99.
  DEFINE VARIABLE J       AS INTEGER   FORMAT 99.
  DEFINE VARIABLE W_Car1  AS CHARACTER FORMAT "X".
  DEFINE VARIABLE W_Car2  AS CHARACTER FORMAT "X".
  DEFINE VARIABLE W_dCont AS INTEGER   FORMAT 9.
  DEFINE VARIABLE W_Dig   AS LOGICAL.
  DEFINE VARIABLE W_Num   AS LOGICAL.

  DEFINE VARIABLE W_Ok AS LOGICAL INITIAL YES.
  DEFINE VARIABLE X_Ok AS LOGICAL INITIAL YES.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_ClaAct W_Clanva Btn_Salvar Btn_Cancel ~
IMAGE-4 
&Scoped-Define DISPLAYED-OBJECTS W_ClaAct W_Clanva 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "Cancelar" 
     SIZE 14.29 BY 1.12.

DEFINE BUTTON Btn_Salvar 
     LABEL "Aceptar" 
     SIZE 14.29 BY 1.12.

DEFINE VARIABLE W_ClaAct AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contraseña" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Clanva AS CHARACTER FORMAT "X(256)":U 
     LABEL "Confirmar" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "imagenes/cambiarpassword.jpg":U
     STRETCH-TO-FIT
     SIZE 12 BY 4.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     W_ClaAct AT ROW 2.62 COL 25 COLON-ALIGNED BLANK 
     W_Clanva AT ROW 3.96 COL 25 COLON-ALIGNED BLANK 
     Btn_Salvar AT ROW 5.27 COL 32.72
     Btn_Cancel AT ROW 5.31 COL 17.57
     "INGRESE SU NUEVA CONTRASEÑA" VIEW-AS TEXT
          SIZE 32 BY .81 AT ROW 1.27 COL 8
          FONT 0
     IMAGE-4 AT ROW 2.35 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 5.


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
         TITLE              = "SFG - Cambio de Contraseña"
         HEIGHT             = 5.81
         WIDTH              = 47.72
         MAX-HEIGHT         = 22.08
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.08
         VIRTUAL-WIDTH      = 114.29
         MIN-BUTTON         = no
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
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Cambio de Contraseña */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Cambio de Contraseña */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME DEFAULT-FRAME /* Aceptar */
DO:
    ASSIGN W_ClaAct.

    FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario NO-ERROR NO-WAIT.
    IF ENCODE(W_ClaAct + W_Cadena) = Usuarios.Clave THEN DO:
        W_Cont = W_Cont + 1.

        RUN Mostrarmensaje IN W_Manija (INPUT 346, OUTPUT W_Eleccion).

        RUN Intentos.

        ASSIGN W_ClaAct = ""
               W_ClaAct:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

        APPLY "ENTRY" TO W_ClaAct.

        RETURN NO-APPLY.
    END.

    W_Clanva = TRIM(W_Clanva).

    ASSIGN W_Entrada = TRIM(W_ClaNva + W_Cadena)
           W_Entrada = ENCODE(W_Entrada)
           Usuarios.Clave = W_Entrada
           Usuarios.Pedir_Clave = NO
           W_Clave = W_ClaNva
           Usuarios.Fec_UltCam = TODAY NO-ERROR.

    RUN P-GraLog IN W_Manija (INPUT STRING(W_fecha) + " " + W_Evento + " Cambio Clave").
    
    FIND CURRENT Usuarios NO-LOCK NO-ERROR.

    APPLY "CHOOSE" TO Btn_Cancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON ENTRY OF Btn_Salvar IN FRAME DEFAULT-FRAME /* Aceptar */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_ClaAct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_ClaAct C-Win
ON ENTRY OF W_ClaAct IN FRAME DEFAULT-FRAME /* Contraseña */
DO:
  ON RETURN TAB. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Clanva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Clanva C-Win
ON LEAVE OF W_Clanva IN FRAME DEFAULT-FRAME /* Confirmar */
DO:
    ASSIGN FRAME {&FRAME-NAME} W_ClaNva W_ClaAct.

    IF LENGTH(W_ClaNva) LT 8 THEN DO:
        MESSAGE "La clave debe tener una longitud mínima de o caracteres!!!"
            VIEW-AS ALERT-BOX WARNING TITLE "Longitud inválida".

        APPLY "ENTRY" TO W_ClaAct.
        RETURN NO-APPLY.
    END.

    IF W_ClaAct <> W_Clanva THEN DO:
        W_Cont = W_Cont + 1.

        RUN Intentos.
        RUN Mostrarmensaje IN W_Manija (INPUT 105, OUTPUT W_Eleccion).

        w_Clanva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
        APPLY "ENTRY" TO W_ClaAct.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        RUN Verificar_Caracteres&Numeros(OUTPUT X_Ok).
        RUN Verificar_Repeticion_Digitos(OUTPUT W_Ok).

        IF W_Ok AND X_Ok THEN DO:
            ENABLE Btn_Salvar WITH FRAME {&FRAME-NAME}.
            APPLY "ENTRY" TO Btn_Salvar.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            IF NOT W_Ok THEN DO:
                MESSAGE "No se permite repetir caracteres de forma consecutiva." skip(2)
                    VIEW-AS ALERT-BOX ERROR TITLE "Digito Repetido".

                ASSIGN w_Clanva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
                APPLY "ENTRY" TO W_ClaAct.
                RETURN NO-APPLY.
            END.
            
            IF NOT X_Ok THEN DO:
                MESSAGE "La clave debe contener caracteres y números." skip(2)
                    VIEW-AS ALERT-BOX ERROR TITLE "Digitos de un solo tipo".

                ASSIGN w_Clanva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
                APPLY "ENTRY" TO W_ClaAct.
                RETURN NO-APPLY.
            END.
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
    FIND FIRST Usuarios WHERE Usuarios.Agencia = W_Agencia
                          AND UsuarioS.Usuario = W_Usuario NO-LOCK NO-ERROR.

    RUN enable_UI.

    APPLY "ENTRY" TO W_ClaAct IN FRAME {&FRAME-NAME}.

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
  DISPLAY W_ClaAct W_Clanva 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE W_ClaAct W_Clanva Btn_Salvar Btn_Cancel IMAGE-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Intentos C-Win 
PROCEDURE Intentos :
IF W_Cont > 4 THEN DO:
    QUIT.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Caracteres&Numeros C-Win 
PROCEDURE Verificar_Caracteres&Numeros :
DEFINE OUTPUT PARAMETER P_Ok AS LOGICAL.

DEFINE VARIABLE W_Verificar AS INTEGER FORMAT 9.

ASSIGN P_Ok = YES
       W_Dig = NO
       W_Num = NO.

DO I = 1 TO LENGTH(W_Clanva) BY 1:
    W_Verificar = INTEGER(SUBSTRING(W_ClaNva,I,1)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        W_Dig = YES.
    ELSE
        W_Num = YES.
END.

IF NOT W_Dig OR NOT W_Num THEN
    P_Ok = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Repeticion_Digitos C-Win 
PROCEDURE Verificar_Repeticion_Digitos :
DEFINE OUTPUT PARAMETER P_Ok AS LOGICAL.

P_Ok = YES.

DO I = 1 TO LENGTH(W_Clanva) BY 1:
    ASSIGN W_dCont = 0
           W_Car1 = SUBSTRING(W_Clanva,I,1).

    DO J = 1 TO LENGTH(W_Clanva) BY 1:
        W_Car2 = SUBSTRING(W_Clanva,J,1).

        IF W_Car1 EQ W_Car2 THEN
            W_dCont = W_dCont + 1.

        IF W_dCont GT 3 THEN
            NEXT.
    END.

    IF W_dCont GT 3 THEN DO:
        P_Ok = NO.
        NEXT.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

