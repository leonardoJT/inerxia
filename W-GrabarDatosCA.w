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
{incluido\Variable.i "SHARED"}
/* Local Variable Definitions ---                                       */
/* Local Variable Definitions ---                                       */
DEFI VAR w_ok     AS LOGICAL.
DEFI VAR wcta     AS INTEGER INITIAL 10.
DEFI VAR W-Tage     LIKE Comprobantes.agencia               NO-UNDO.
DEFI VAR W-Tnro     LIKE Comprobantes.secuencia             NO-UNDO.
DEFI VAR W-TCbte    LIKE Comprobantes.Comprobante           NO-UNDO.
DEFI VAR W-Tmarca   LIKE Comprobantes.Id_Consecutivo        NO-UNDO.
DEFI VAR W-Tsaldo   LIKE Comprobantes.num_inicial           NO-UNDO.
DEFI VAR W-DocAgeO  LIKE Mov_Contable.Num_Documento         NO-UNDO.
DEFI VAR w-DocAgeD  LIKE Mov_Contable.Num_Documento         NO-UNDO.
DEFI VAR W-AgeOrig  LIKE Mov_Contable.Agencia               NO-UNDO.
DEFI VAR W-AgeDest  LIKE Mov_Contable.Agencia               NO-UNDO.
DEFI VAR W-RegDet   AS CHARACTER FORMAT "X(220)" INITIAL "" NO-UNDO. 
DEFI VAR W-SwAvance AS LOGICAL EXTENT 10 INITIAL FALSE      NO-UNDO.
DEFI VAR W-UsrData  LIKE Usuarios.Usuario  INITIAL "998"    NO-UNDO. /* Usuario CA */

/* Necesarios para el proceso de importacion */
DEFI VAR W-Porc        AS  INTEGER INITIAL 0  NO-UNDO.
DEFI VAR W-NroReg      AS  INTEGER INITIAL 0  NO-UNDO.
DEFI VAR W-TotReg      AS  INTEGER INITIAL 0  NO-UNDO.
DEFI VAR zcadena       AS  CHAR FORMAT "X(40)"             NO-UNDO.
DEFI VAR z-Regist      AS  CHAR FORMAT "X(220)" INITIAL "" NO-UNDO.


DEFINE TEMP-TABLE tmpMiCbte NO-UNDO
    FIELD Tage     LIKE Comprobantes.agencia
    FIELD Tnro     LIKE Comprobantes.secuencia
    FIELD TCbte    LIKE Comprobantes.Comprobante
    FIELD Tmarca   LIKE Comprobantes.Id_Consecutivo    
    FIELD Tsaldo   LIKE Comprobantes.num_inicial
    INDEX IdxAgeCbte Tage TCbte.

FIND FIRST Cfg_TarjetaDb NO-LOCK NO-ERROR.
DECLARE MiCbte CURSOR FOR
    SELECT a.agencia LABEL 'Age', a.secuencia LABEL  'nro',
                        a.comprobante LABEL 'cbte',
                        a.id_consecutivo LABEL 'marca', a.num_inicial LABEL 'saldo'  
                        FROM   comprobantes a 
                        WHERE (comprobante = Cfg_TarjetaDb.CbteAhorro OR
                               comprobante = Cfg_TarjetaDb.CbteCupo)
       ORDER BY a.agencia, a.comprobante
            WITH FRAME MiCbte WIDTH 210 NO-LABELS.

DEFINE TEMP-TABLE TmpDatafono NO-UNDO
    FIELD Tage   LIKE agencias.agencia
    FIELD Tdata  AS CHARACTER FORMAT "X(8)"
    FIELD Tusr   LIKE Usuarios.Usuario    
    INDEX IdxData Tdata.

DEFINE TEMP-TABLE tmovgra NO-UNDO
    FIELD greg  AS CHARACTER FORMAT "X(220)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS I-Fondo I-Texto 
&Scoped-Define DISPLAYED-OBJECTS W-Cedula W-FecBco W-Comentario W-TxtAvance 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Visualizar 
     IMAGE-UP FILE "Imagenes/impresora2.bmp":U
     LABEL "Visualizar" 
     SIZE 7 BY 1.35 TOOLTIP "Visualizar por Pantalla / Impresora".

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 7 BY 1.35 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Procesar 
     LABEL "Procesar" 
     SIZE 14 BY .81.

DEFINE VARIABLE W-Cedula AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 4  NO-UNDO.

DEFINE VARIABLE W-Comentario AS CHARACTER FORMAT "X(90)":U 
     VIEW-AS FILL-IN 
     SIZE 40.29 BY .81
     BGCOLOR 4  NO-UNDO.

DEFINE VARIABLE W-FecBco AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 4  NO-UNDO.

DEFINE VARIABLE W-TxtAvance AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81 TOOLTIP "Nro de registros procesados"
     BGCOLOR 4  NO-UNDO.

DEFINE IMAGE I-Avance10
     FILENAME "imagenes/avance10.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance100
     FILENAME "imagenes/avance100.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance20
     FILENAME "imagenes/avance20.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance30
     FILENAME "imagenes/avance30.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance40
     FILENAME "imagenes/avance40.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance50
     FILENAME "imagenes/avance50.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance60
     FILENAME "imagenes/avance60.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance70
     FILENAME "imagenes/avance70.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance80
     FILENAME "imagenes/avance80.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Avance90
     FILENAME "imagenes/avance90.jpg":U
     SIZE 36.57 BY .62.

DEFINE IMAGE I-Fondo
     FILENAME "imagenes/logoca.jpg":U
     SIZE 73 BY 4.04.

DEFINE IMAGE I-Texto
     FILENAME "imagenes/avance0.jpg":U
     SIZE 36.72 BY .73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W-Cedula AT ROW 1.81 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 8 NO-TAB-STOP 
     W-FecBco AT ROW 1.81 COL 57.43 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     W-Comentario AT ROW 2.69 COL 2.72 NO-LABEL WIDGET-ID 10
     W-TxtAvance AT ROW 2.69 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     Btn-Visualizar AT ROW 3.62 COL 58.86 WIDGET-ID 46
     Btn_Done AT ROW 3.62 COL 66 WIDGET-ID 42
     Btn_Procesar AT ROW 3.69 COL 3 WIDGET-ID 2
     "Fecha:" VIEW-AS TEXT
          SIZE 7 BY .81 TOOLTIP "Fecha Archivo Enviado por el Banco" AT ROW 1.81 COL 52.14 WIDGET-ID 38
          BGCOLOR 4 
     I-Fondo AT ROW 1 COL 1 WIDGET-ID 6
     I-Texto AT ROW 3.58 COL 17.86 WIDGET-ID 12
     I-Avance10 AT ROW 4.38 COL 18 WIDGET-ID 14
     I-Avance100 AT ROW 4.38 COL 18 WIDGET-ID 16
     I-Avance20 AT ROW 4.38 COL 18 WIDGET-ID 18
     I-Avance30 AT ROW 4.38 COL 18 WIDGET-ID 20
     I-Avance40 AT ROW 4.38 COL 18 WIDGET-ID 22
     I-Avance50 AT ROW 4.38 COL 18 WIDGET-ID 24
     I-Avance60 AT ROW 4.35 COL 18 WIDGET-ID 28
     I-Avance70 AT ROW 4.38 COL 18 WIDGET-ID 30
     I-Avance80 AT ROW 4.38 COL 18 WIDGET-ID 32
     I-Avance90 AT ROW 4.38 COL 18 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.14 BY 4.15 WIDGET-ID 100.


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
         TITLE              = "Procesar Resumen Diario Tarjeta Débito - W-GrabarDatosCA.w"
         HEIGHT             = 4
         WIDTH              = 72.86
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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

/* SETTINGS FOR BUTTON Btn_Done IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Done:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Procesar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Procesar:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance10 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance10:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance100 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance100:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance20 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance20:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance30 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance30:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance40 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance40:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance50 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance50:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance60 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance60:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance70 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance70:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance80 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance80:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR IMAGE I-Avance90 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       I-Avance90:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN W-Cedula IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       W-Cedula:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W-Comentario IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       W-Comentario:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W-FecBco IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       W-FecBco:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W-TxtAvance IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       W-TxtAvance:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Procesar Resumen Diario Tarjeta Débito - W-GrabarDatosCA.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Procesar Resumen Diario Tarjeta Débito - W-GrabarDatosCA.w */
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
  /*{Imprimir.I "listado"}*/   
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
      RETURN.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Procesar C-Win
ON CHOOSE OF Btn_Procesar IN FRAME F-Main /* Procesar */
RUN importar.

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
    DEFI INPUT PARAMETER W-FecEnviada AS DATE FORMAT "99/99/9999".
    RUN enable_UI.
/*     MESSAGE W-FecEnviada                   */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   APPLY "choose" TO Btn_Procesar.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avance C-Win 
PROCEDURE Avance :
DEFI INPUT PARAMETER W-Porc AS INTEGER.

IF W-Porc > 9 AND W-Porc < 20 THEN DO:
    IF NOT W-SwAvance[1] THEN
        ASSIGN I-Avance10:VISIBLE IN FRAME F-Main = TRUE
               W-SwAvance[1] = TRUE.
    RETURN.
END.
ELSE
    IF W-Porc > 19 AND W-Porc < 29 THEN DO:
        IF NOT W-SwAvance[2] THEN
            ASSIGN I-Avance10:VISIBLE = FALSE
                   I-Avance20:VISIBLE = TRUE
                   W-SwAvance[2] = TRUE.
        RETURN.
    END.
    ELSE
        IF W-Porc > 29 AND W-Porc < 40 THEN DO:
            IF NOT W-SwAvance[3]  THEN
                ASSIGN I-Avance20:VISIBLE = FALSE
                       I-Avance30:VISIBLE = TRUE
                       W-SwAvance[3] = TRUE.
            RETURN.
        END.
        ELSE
            IF W-Porc > 39 AND W-Porc < 50 THEN DO:
                IF NOT W-SwAvance[4] THEN
                    ASSIGN I-Avance30:VISIBLE = FALSE
                           I-Avance40:VISIBLE = TRUE
                           W-SwAvance[4] = TRUE.
                RETURN.
            END.
            ELSE
                IF W-Porc > 49 AND W-Porc < 60 THEN DO:
                    IF NOT W-SwAvance[5] THEN
                        ASSIGN I-Avance40:VISIBLE = FALSE
                               I-Avance50:VISIBLE = TRUE
                               W-SwAvance[5] = TRUE.
                    RETURN.
                END.
                ELSE
                    IF W-Porc > 59 AND W-Porc < 70 THEN DO:
                        IF NOT W-SwAvance[6] THEN
                            ASSIGN I-Avance50:VISIBLE = FALSE
                                   I-Avance60:VISIBLE = TRUE
                                   W-SwAvance[6] = TRUE.
                        RETURN.
                    END.
                    ELSE
                        IF W-Porc > 69 AND W-Porc < 80 THEN DO:
                            IF NOT W-SwAvance[7] THEN
                                ASSIGN I-Avance60:VISIBLE = FALSE
                                       I-Avance70:VISIBLE = TRUE
                                       W-SwAvance[7] = TRUE.
                            RETURN.
                        END.
                        ELSE
                            IF W-Porc > 79 AND W-Porc < 90 THEN DO:
                                IF NOT W-SwAvance[8] THEN
                                    ASSIGN I-Avance70:VISIBLE = FALSE
                                           I-Avance80:VISIBLE = TRUE
                                           W-SwAvance[8] = TRUE.
                                RETURN.
                            END.
                            ELSE
                                IF W-Porc > 89 AND W-Porc < 99 THEN DO:
                                    IF NOT W-SwAvance[9] THEN
                                        ASSIGN I-Avance80:VISIBLE = FALSE
                                               I-Avance90:VISIBLE = TRUE
                                               W-SwAvance[9] = TRUE.
                                    RETURN.
                                END.
                                ELSE
                                    IF W-Porc = 100 THEN DO:
                                        IF NOT W-SwAvance[10] THEN
                                            ASSIGN I-Avance90:VISIBLE = FALSE
                                                   I-Avance100:VISIBLE = TRUE
                                                   W-SwAvance[10] = TRUE.
                                        RETURN.
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
  DISPLAY W-Cedula W-FecBco W-Comentario W-TxtAvance 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE I-Fondo I-Texto 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_incons C-Win 
PROCEDURE Graba_incons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wregtra  AS CHARACTER FORMAT "X(256)" INITIAL "".
ASSIGN wregtra    = STRING(DECIMAL(Mov_Tarjetas.Nit),"zzz,zzz,zzz,zz9") + " "   +
                    STRING(Mov_Tarjetas.TarjetaDb,"9999999999999999")   + " "   +
                    STRING(Mov_Tarjetas.TipoCuenta,"99")           + " "   + 
                    STRING(Mov_Tarjetas.Num_Cuenta,"X(11)")             + " "   + 
                    STRING(Mov_Tarjetas.NroTerminal,"X(8)")             + " "   +
                    STRING(Mov_Tarjetas.Monto,"-zzz,zzz,zzz")           + "   " +
                    STRING(Mov_Tarjetas.Comision,"-zzz,zzz,zzz")        + "   " +
                    STRING(Mov_Tarjetas.ComAdicional,"-zzz,zzz,zzz")    + "   " +
                    STRING(Mov_Tarjetas.OtrosCargos,"-zzz,zzz,zzz")     + "   " +
                    "NO APLICADO        " + Mov_Tarjetas.lugar          + " "   + 
                    Mov_Tarjetas.Naud.
CREATE Tmovgra.
ASSIGN Tmovgra.greg = wregtra.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importar C-Win 
PROCEDURE Importar :
W-Comentario:SCREEN-VALUE IN FRAME F-Main = "Bajando Información : Tarjeta Débito".

ASSIGN W-Porc = 0
       W-NroReg = 0
       W-TotReg = 0
       zcadena = ""
       z-Regist = "".

FIND FIRST Calend_TarjDb WHERE Calend_TarjDB.Fec_ProBco EQ W-FecEnviada NO-ERROR.
IF AVAILABLE(Calend_TarjDb) THEN DO:
    IF NOT calend_tarjdb.aplicado THEN DO:
        RUN llenar_Tablas.

        ASSIGN W-TotReg = Calend_TarjDb.NroReg.

        FOR EACH mov_tarjeta WHERE mov_tarjetas.Fec_ProBco EQ W-FecEnviada
                               AND NOT mov_tarjetas.aplicado:
            ASSIGN W-FecBco:SCREEN-VALUE = STRING(Calend_Tarjdb.Fec_ProBco,"99/99/9999")
                   W-FecBco = Calend_Tarjdb.Fec_ProBco W-RegDet = "".

            IF Mov_Tarjetas.Num_Cuenta EQ "" OR Mov_Tarjetas.Nit EQ ""
                                             OR Mov_Tarjetas.TarjetaDb EQ ""
                                             OR (ABS(Mov_Tarjetas.Monto) +
                                                 ABS(Mov_Tarjetas.Comision) +
                                                 ABS(Mov_Tarjetas.ComAdicional) +
                                                 ABS(Mov_Tarjetas.OtrosCargos)) EQ 0 THEN DO:
                ASSIGN Z-Regist = STRING(DECIMAL(Mov_Tarjetas.Nit),"zzz,zzz,zzz,zz9") + " " +
                                  STRING(Mov_Tarjetas.TarjetaDb,"9999999999999999") + " " +
                                  STRING(Mov_Tarjetas.TipoCuenta,"99") + " " +
                                  STRING(Mov_Tarjetas.Num_Cuenta,"X(11)") + " " +
                                  STRING(Mov_Tarjetas.NroTerminal,"X(8)") + " " +
                                  STRING(Mov_Tarjetas.Monto,"-zzz,zzz,zzz") + "   " +
                                  STRING(Mov_Tarjetas.Comision,"-zzz,zzz,zzz") + "   " +
                                  STRING(Mov_Tarjetas.ComAdicional,"-zzz,zzz,zzz") + "   " +
                                  STRING(Mov_Tarjetas.OtrosCargos,"-zzz,zzz,zzz") + "   " +
                                  "NO APLICADO        " + Mov_Tarjetas.lugar + " " +
                                  Mov_Tarjetas.Naud + " " + STRING(Mov_Tarjetas.Fec_Transac,"99/99/9999").

                CREATE Tmovgra.
                ASSIGN Tmovgra.greg = Z-Regist
                       Mov_Tarjeta.Inconsistencia = "Alguno de los valores es nulo"
                       W-NroReg = W-NroReg + 1
                       W-Porc = TRUNCATE(W-NroReg / W-TotReg * 100,0)
                       W-Cedula:SCREEN-VALUE = Mov_Tarjetas.Nit
                       W-TxtAvance:SCREEN-VALUE = "RegProc: " + TRIM(STRING(W-NroReg,"zzz,zz9")) + " / " +  TRIM(STRING(W-TotReg,"zzz,zz9")).

                RUN avance(INPUT W-Porc).

                NEXT.
            END.

            RUN transaccion.






        ASSIGN W-NroReg = W-NroReg + 1
               W-Porc = TRUNCATE(W-NroReg / W-TotReg * 100,0)
               W-Cedula:SCREEN-VALUE = Mov_Tarjetas.Nit.
        W-TxtAvance:SCREEN-VALUE = "RegProc: " + TRIM(STRING(W-NroReg,"zzz,zz9")) + " / " +  TRIM(STRING(W-TotReg,"zzz,zz9")). 
        RUN avance(INPUT W-Porc).
      END.
      ASSIGN calend_tarjdb.aplicado = TRUE.
  END.
  ELSE 
     MESSAGE "La información ya fué procesada, revise esta fecha " W-FecEnviada
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:
   MESSAGE "No se encontraron Datos para esta fecha " W-FecEnviada
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ENABLE Btn-Visualizar btn_Done WITH FRAME f-main.
zcadena = STRING(TIME,"HH:MM:SS").
zcadena = "Grab_TarjDb_" + STRING(TODAY,"99999999") + "_" + SUBSTRING(zcadena,1,2) + SUBSTRING(zcadena,4,2) + SUBSTRING(zcadena,7,2).
OUTPUT TO VALUE("C:\enpacto\Informes\" + zcadena).
PUT "REPORTE   : Movimiento Banco de Bogotá - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am") SKIP(2).
PUT "      Cédula    #Tarjeta Débito  TC Nro.Cuenta  NroTerm    Vlr. Monto   Comisión Bco   Comisión Coop. Otros Cargos   Operación          Lugar                          #Audit  Fecha_Trans" SKIP(1).
FOR EACH tmovgra:
   PUT greg FORMAT "X(220)" SKIP(0).
END.
OUTPUT CLOSE.
APPLY "CHOOSE" TO btn-visualizar.
RELEASE tmpMiCbte.
RELEASE TmpDatafono.
RELEASE tmovgra.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_Tablas C-Win 
PROCEDURE Llenar_Tablas :
EMPTY TEMP-TABLE TmpDatafono.

FOR EACH agencias NO-LOCK:
    IF agencias.datafono[1] NE "" THEN DO:
        CREATE TmpDatafono.
        ASSIGN TmpDatafono.TAge = Agencias.agencia
               TmpDatafono.TData = Agencias.Datafono[1]
               TmpDatafono.Tusr = Agencias.UsrDatafono[1].
    END.

    IF agencias.datafono[2] NE "" THEN DO:
        CREATE TmpDatafono.
        ASSIGN TmpDatafono.TAge = Agencias.agencia
               TmpDatafono.TData = Agencias.Datafono[2]
               TmpDatafono.Tusr = Agencias.UsrDatafono[2].
    END.

    IF agencias.datafono[3] NE "" THEN DO:
        CREATE TmpDatafono.
        ASSIGN TmpDatafono.TAge = Agencias.agencia
               TmpDatafono.TData = Agencias.Datafono[3]
               TmpDatafono.Tusr = Agencias.UsrDatafono[3].
    END.

    IF agencias.datafono[4] NE "" THEN DO:
        CREATE TmpDatafono.
        ASSIGN TmpDatafono.TAge = Agencias.agencia
               TmpDatafono.TData = Agencias.Datafono[4]
               TmpDatafono.Tusr = Agencias.UsrDatafono[4].
    END.
END.

EMPTY TEMP-TABLE TmpMiCbte.
/* Búsqueda de Comprobantes a ser utilizados */
OPEN MiCbte.

REPEAT:
    FETCH MiCbte INTO W-Tage, W-Tnro, W-TCbte, W-Tmarca, W-Tsaldo.
    CREATE TmpMiCbte.
    ASSIGN TmpMiCbte.Tage = W-Tage
           TmpMiCbte.Tnro = 0
           TmpMiCbte.TCbte = W-TCbte
           TmpMiCbte.Tmarca = W-Tmarca
           TmpMiCbte.Tsaldo = W-Tsaldo.
END.

CLOSE Micbte.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Num_Documento C-Win 
PROCEDURE Num_Documento :
FIND FIRST TmpMiCbte WHERE TmpMiCbte.TAge  = W-AgeOrig
                       AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-ERROR.
IF W-DocAgeO NE 0 AND TmpMiCbte.Tnro EQ 0 THEN
    ASSIGN TmpMiCbte.Tnro = W-DocAgeO TmpMiCbte.Tmarca = 1.

IF W-AgeOrig NE W-AgeDest THEN DO:
    FIND FIRST TmpMiCbte WHERE TmpMiCbte.TAge  = W-AgeDest
                           AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-ERROR.
    IF W-DocAgeD NE 0 AND TmpMiCbte.Tnro EQ 0 THEN
        ASSIGN TmpMiCbte.Tnro = W-DocAgeD TmpMiCbte.Tmarca = 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Tel  : 311 322 54 58 - Casa 253 49 71 
  Notes: impresión de los datos almacenados en el sistema SFG
------------------------------------------------------------------------------*/
{INCLUIDO\RepEncabezado.I}.  
    W_Reporte    = "REPORTE   : Movimiento Banco de Bogotá - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "      Cédula    #Tarjeta Débito  TC Nro.Cuenta  NroTerm    Vlr. Monto   Comisión Bco   Comisión Coop. Otros Cargos   Detalle Operación                                  #Auditoria  Fecha_Tran". 
/*  W_EncColumna = "      Cédula    #Tarjeta Débito  TC Nro.Cuenta  NroTerm    Vlr. Monto   Comisión Bco   Comisión Coop. Otros Cargos   Operación          Lugar                          #Audit    Fec_Trans". */
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
/*     PUT W_EncColumna SKIP(2).  */
    FOR EACH tmovgra:
        DISPLAY greg FORMAT "X(220)"
                WITH FRAME F-movgra USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 232 NO-LABELS.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion C-Win 
PROCEDURE Transaccion :
IF Mov_Tarjetas.TipoTransaccion EQ 0  THEN DO: /* Cobros de Plastico, cuotas adm */
    FIND FIRST ahorros WHERE Ahorros.Nit EQ Mov_Tarjetas.Nit
                         AND Ahorros.Cod_Ahorro EQ Cfg_TarjetaDb.Cod_Ahorro
                         AND Ahorros.Cue_Ahorros EQ Mov_Tarjetas.Num_Cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE(Ahorros) THEN DO:
        FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge  = Ahorros.Agencia
                               AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
        IF AVAILABLE(TmpMiCbte) THEN DO:
            ASSIGN W-DocAgeO = TmpMiCbte.Tnro
                   W-DocAgeD = TmpMiCbte.Tnro
                   W-AgeOrig = Ahorros.agencia
                   W-AgeDest = Ahorros.agencia.

            RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                 INPUT-OUTPUT w-DocAgeD, INPUT Mov_Tarjetas.TarjetaDB, INPUT Mov_Tarjetas.TipoCuenta,
                                 INPUT Ahorros.Cue_Ahorros, INPUT Mov_Tarjetas.TipoTransaccion, INPUT Mov_Tarjetas.Monto,
                                 INPUT Mov_Tarjetas.Comision, INPUT Mov_Tarjetas.ComAdicional, INPUT Mov_Tarjetas.Lugar,
                                 INPUT Mov_Tarjetas.Naud, INPUT Mov_Tarjetas.NroTerminal, INPUT Mov_Tarjetas.Fec_Transac,
                                 INPUT Mov_Tarjetas.Hora_Transac, INPUT Mov_Tarjetas.Nit, INPUT Mov_Tarjetas.TipoRed,
                                 INPUT Mov_Tarjetas.OtrosCargos, INPUT "998", /* Usuario */ INPUT TRUE /* Datafono */,
                                 INPUT 0, /* jjmp 28/08/2008 */ OUTPUT W-RegDet).

            ASSIGN mov_tarjetas.aplicado = TRUE.

            RUN Num_Documento.

            CREATE Tmovgra.
            ASSIGN Tmovgra.greg = W-RegDet.
        END.
    END.
END.
ELSE DO:
    IF Mov_Tarjetas.TipoTransaccion EQ 1 OR  /* Consulta de Datáfono/Cajero */ Mov_Tarjetas.TipoTransaccion EQ 4 /* Declinada de Datáfono/Cajero  */ THEN DO:
        IF Mov_Tarjetas.TipoCuenta EQ 10 THEN DO:
            FIND FIRST ahorros WHERE Ahorros.Nit EQ Mov_Tarjetas.Nit
                                 AND Ahorros.Cod_Ahorro  EQ Cfg_TarjetaDb.Cod_Ahorro
                                 AND Ahorros.Cue_Ahorros EQ Mov_Tarjetas.Num_Cuenta NO-LOCK NO-ERROR.
            IF AVAILABLE(Ahorros) THEN DO:
                W-AgeOrig = Ahorros.agencia.

                FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Ahorros.agencia
                                       AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
                IF AVAILABLE(TmpMiCbte) THEN
                    ASSIGN W-DocAgeO = TmpMiCbte.Tnro.

                FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Cfg_tarjetaDb.AgeAdmon
                                       AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
                IF AVAILABLE(TmpMiCbte) THEN
                    ASSIGN W-DocAgeD = TmpMiCbte.Tnro W-AgeDest = Cfg_tarjetaDb.AgeAdmon.

                RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                     INPUT-OUTPUT W-DocAgeD, INPUT Mov_Tarjeta.TarjetaDB, INPUT Mov_Tarjetas.TipoCuenta,
                                     INPUT Ahorros.Cue_Ahorros, INPUT Mov_Tarjeta.TipoTransaccion, INPUT Mov_Tarjeta.Monto,
                                     INPUT Mov_Tarjeta.Comision, INPUT Mov_Tarjeta.ComAdicional, INPUT Mov_Tarjeta.Lugar,
                                     INPUT Mov_Tarjeta.Naud, INPUT Mov_Tarjeta.NroTerminal, INPUT Mov_Tarjeta.Fec_Transac,
                                     INPUT Mov_Tarjeta.Hora_Transac, INPUT Mov_Tarjeta.Nit, INPUT Mov_Tarjeta.TipoRed,
                                     INPUT Mov_Tarjeta.OtrosCargos, INPUT "998", /* Usuario */ INPUT TRUE /* Datafono */,
                                     INPUT 0, /* jjmp 28/08/2008 */ OUTPUT W-RegDet).

                ASSIGN mov_tarjetas.aplicado = TRUE.

                RUN Num_Documento.

                CREATE Tmovgra.
                ASSIGN Tmovgra.greg = W-RegDet.
            END.
            ELSE 
                RUN Graba_Incons.
        END.
        ELSE DO: /* Cupo Rotativo */
            FIND FIRST Creditos WHERE Creditos.Nit EQ Mov_Tarjetas.Nit
                                  AND (Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito OR /* jjmp 28/08/2008 */
                                       Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito2)
                                  AND Creditos.Num_credito EQ INT64(MOV_Tarjetas.Num_Cuenta) NO-LOCK NO-ERROR.
            IF AVAILABLE(Creditos) THEN DO:
                W-AgeOrig = Creditos.agencia.
                FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Creditos.agencia
                                       AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteCupo NO-LOCK NO-ERROR.
                IF AVAILABLE(TmpMiCbte) THEN
                    ASSIGN W-DocAgeO = TmpMiCbte.Tnro.

                FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Cfg_tarjetaDb.AgeAdmon
                                       AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteCupo NO-LOCK NO-ERROR.
                IF AVAILABLE(TmpMiCbte) THEN
                    ASSIGN W-DocAgeD = TmpMiCbte.Tnro W-AgeDest = Cfg_tarjetaDb.AgeAdmon.

                IF AVAILABLE(TmpMiCbte) THEN
                    ASSIGN W-DocAgeD = TmpMiCbte.Tnro W-AgeDest = Cfg_tarjetaDb.AgeAdmon.

                RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                     INPUT-OUTPUT W-DocAgeD, INPUT Mov_Tarjeta.TarjetaDB, INPUT Mov_Tarjetas.TipoCuenta,
                                     INPUT TRIM(STRING(Creditos.Num_Credito)), INPUT Mov_Tarjeta.TipoTransaccion, INPUT Mov_Tarjeta.Monto,
                                     INPUT Mov_Tarjeta.Comision, INPUT Mov_Tarjeta.ComAdicional, INPUT Mov_Tarjeta.Lugar,
                                     INPUT Mov_Tarjeta.Naud, INPUT Mov_Tarjeta.NroTerminal, INPUT Mov_Tarjeta.Fec_Transac,
                                     INPUT Mov_Tarjeta.Hora_Transac, INPUT Mov_Tarjeta.Nit, INPUT Mov_Tarjeta.TipoRed,
                                     INPUT Mov_Tarjeta.OtrosCargos, INPUT "998", /* Usuario */ INPUT TRUE /* Datafono */,
                                     INPUT Creditos.Cod_Credito, /* jjmp 28/08/2008 */ OUTPUT W-RegDet).

                ASSIGN mov_tarjetas.aplicado = TRUE.

                RUN Num_Documento.

                CREATE Tmovgra.
                ASSIGN Tmovgra.greg = W-RegDet.
            END.
            ELSE 
                RUN Graba_Incons.
        END.
    END.
    ELSE DO:
        FIND FIRST TmpDatafono WHERE TmpDatafono.TData = Mov_Tarjetas.NroTerminal NO-LOCK NO-ERROR.
        IF AVAILABLE(TmpDatafono) THEN DO:
            ASSIGN W-AgeOrig = TmpDatafono.Tage
                   W-AgeDesT = TmpDatafono.Tage
                   W-UsrData = TmpDatafono.TUsr.

            IF W-UsrData EQ "" OR W-UsrData = ? THEN
                W-UsrData = '998'.

            FIND FIRST tarjetas WHERE Tarjetas.tarjetadb = Mov_Tarjetas.TarjetaDb NO-LOCK NO-ERROR.
            IF AVAILABLE(tarjetas) THEN DO:
                IF Mov_Tarjetas.TipoCuenta = 10 AND Tarjetas.Cue_Ahorros = Mov_Tarjetas.Num_Cuenta THEN DO:
                    FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = TmpDatafono.Tage
                                           AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
                    IF AVAILABLE(TmpMiCbte) THEN
                        ASSIGN W-DocAgeO = TmpMiCbte.Tnro
                               W-DocAgeD = TmpMiCbte.Tnro.
                    ELSE
                        ASSIGN W-DocAgeO = 0
                               W-DocAgeD = 0.

                    FIND FIRST ahorros WHERE Ahorros.Nit EQ Mov_Tarjetas.Nit
                                         AND Ahorros.Cod_Ahorro EQ Cfg_TarjetaDb.Cod_Ahorro
                                         AND Ahorros.Cue_Ahorros EQ Tarjetas.Cue_Ahorros NO-LOCK NO-ERROR.
                    IF AVAILABLE(Ahorros) THEN DO:
                        IF TmpDatafono.Tage NE Ahorros.agencia THEN DO:
                            W-AgeDesT = Ahorros.agencia.

                            FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Ahorros.agencia
                                                   AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
                            IF AVAILABLE(TmpMiCbte) THEN
                                ASSIGN W-DocAgeD = TmpMiCbte.Tnro.
                        END.

                        ASSIGN W-AgeOrig = TmpDatafono.Tage
                               W-AgeDest = Ahorros.agencia.

                        RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                             INPUT-OUTPUT W-DocAgeD, INPUT Mov_Tarjeta.TarjetaDB, INPUT Mov_Tarjetas.TipoCuenta,
                                             INPUT Ahorros.Cue_Ahorros, INPUT Mov_Tarjeta.TipoTransaccion, INPUT Mov_Tarjeta.Monto,
                                             INPUT Mov_Tarjeta.Comision, INPUT Mov_Tarjeta.ComAdicional, INPUT Mov_Tarjeta.Lugar,
                                             INPUT Mov_Tarjeta.Naud, INPUT Mov_Tarjeta.NroTerminal, INPUT Mov_Tarjeta.Fec_Transac,
                                             INPUT Mov_Tarjeta.Hora_Transac, INPUT Mov_Tarjeta.Nit, INPUT Mov_Tarjeta.TipoRed,
                                             INPUT Mov_Tarjeta.OtrosCargos, INPUT W-UsrData, /* Usuario */ INPUT TRUE /* Datafono */,
                                             INPUT 0, /* jjmp 28/08/2008 */ OUTPUT  W-RegDet).

                        ASSIGN mov_tarjetas.aplicado = TRUE.

                        RUN Num_Documento.

                        CREATE Tmovgra.
                        ASSIGN Tmovgra.greg = W-RegDet.
                    END.
                END.
                ELSE DO: /* Cupo Rotivo */
                    FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = TmpDatafono.Tage
                                           AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteCupo NO-LOCK NO-ERROR.
                    IF AVAILABLE(TmpMiCbte) THEN
                        ASSIGN W-DocAgeO = TmpMiCbte.Tnro
                               W-DocAgeD = TmpMiCbte.Tnro.
                    ELSE
                        ASSIGN W-DocAgeO = 0
                               W-DocAgeD = 0.

                    FIND FIRST Creditos WHERE Creditos.Nit EQ Mov_Tarjetas.Nit
                                          AND (Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito OR
                                               Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito2)
                                          AND /* 28/08/2008 */ Creditos.Num_Credito EQ INT64(MOV_Tarjetas.Num_Cuenta) NO-LOCK NO-ERROR.
                    IF AVAILABLE(Creditos) THEN DO:
                        IF TmpDatafono.Tage NE Creditos.agencia THEN DO:
                            W-AgeDesT = Creditos.agencia.

                            FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Creditos.agencia
                                                   AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteCupo NO-LOCK NO-ERROR.
                            IF AVAILABLE(TmpMiCbte) THEN
                                ASSIGN W-DocAgeD = TmpMiCbte.Tnro.
                        END.

                        ASSIGN W-AgeOrig = TmpDatafono.Tage   W-AgeDest = Creditos.agencia.

                        RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                             INPUT-OUTPUT W-DocAgeD, INPUT Mov_Tarjeta.TarjetaDB, INPUT Mov_Tarjetas.TipoCuenta,
                                             INPUT TRIM(STRING(Creditos.Num_credito)), INPUT Mov_Tarjeta.TipoTransaccion, INPUT Mov_Tarjeta.Monto,
                                             INPUT Mov_Tarjeta.Comision, INPUT Mov_Tarjeta.ComAdicional, INPUT Mov_Tarjeta.Lugar,
                                             INPUT Mov_Tarjeta.Naud, INPUT Mov_Tarjeta.NroTerminal, INPUT Mov_Tarjeta.Fec_Transac,
                                             INPUT Mov_Tarjeta.Hora_Transac, INPUT Mov_Tarjeta.Nit, INPUT Mov_Tarjeta.TipoRed,
                                             INPUT Mov_Tarjeta.OtrosCargos, INPUT W-UsrData, /* Usuario */ INPUT TRUE /* Datafono */,
                                             INPUT Creditos.Cod_Credito, /* jjmp 28/08/2008 */ OUTPUT  W-RegDet).

                        ASSIGN mov_tarjetas.aplicado = TRUE.

                        RUN Num_Documento.

                        CREATE Tmovgra.
                        ASSIGN Tmovgra.greg = W-RegDet.
                    END.
                    ELSE
                        RUN Graba_Incons.
                END.
            END.
        END.
        ELSE DO: /* Cuando es de otras redes, TipoTrans: 2, 3  */
            IF Mov_Tarjetas.TipoCuenta = 10 THEN DO:
                FIND FIRST ahorros WHERE Ahorros.Nit EQ Mov_Tarjetas.Nit
                                     AND Ahorros.Cod_Ahorro EQ Cfg_TarjetaDb.Cod_Ahorro
                                     AND Ahorros.Cue_Ahorros EQ Mov_Tarjetas.Num_Cuenta NO-LOCK NO-ERROR.
                IF AVAILABLE(Ahorros) THEN DO:
                    ASSIGN W-AgeOrig = Ahorros.agencia.
                    FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Ahorros.agencia
                                           AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
                    IF AVAILABLE(TmpMiCbte) THEN
                        ASSIGN W-DocAgeO = TmpMiCbte.Tnro.

                    /* Llevar contrapartida a la agencia administrativa */
                    FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Cfg_tarjetaDb.AgeAdmon
                                           AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteAhorro NO-LOCK NO-ERROR.
                    IF AVAILABLE(TmpMiCbte) THEN
                        ASSIGN W-DocAgeD = TmpMiCbte.Tnro
                               W-AgeDest = Cfg_tarjetaDb.AgeAdmon.
                    ELSE
                        ASSIGN W-DocAgeD = 0
                               W-AgeDest = Cfg_tarjetaDb.AgeAdmon.

                    RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                         INPUT-OUTPUT W-DocAgeD, INPUT Mov_Tarjeta.TarjetaDB, INPUT Mov_Tarjeta.TipoCuenta,
                                         INPUT Ahorros.Cue_Ahorros, INPUT Mov_Tarjeta.TipoTransaccion, INPUT Mov_Tarjeta.Monto,
                                         INPUT Mov_Tarjeta.Comision, INPUT Mov_Tarjeta.ComAdicional, INPUT Mov_Tarjeta.Lugar,
                                         INPUT Mov_Tarjeta.Naud, INPUT Mov_Tarjeta.NroTerminal, INPUT Mov_Tarjeta.Fec_Transac,
                                         INPUT Mov_Tarjeta.Hora_Transac, INPUT Mov_Tarjeta.Nit, INPUT Mov_Tarjeta.TipoRed,
                                         INPUT Mov_Tarjeta.OtrosCargos, INPUT "998", /* Usuario */ INPUT FALSE,
                                         INPUT 0, OUTPUT W-RegDet).

                    ASSIGN mov_tarjetas.aplicado = TRUE.
                    
                    RUN Num_Documento.

                    CREATE Tmovgra.
                    ASSIGN Tmovgra.greg = W-RegDet.
                END.
                ELSE 
                    RUN Graba_Incons.
            END.
            ELSE DO: /* Cupo Rotivo */
                FIND FIRST Creditos WHERE Creditos.Nit EQ Mov_Tarjetas.Nit
                                      AND (Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito OR
                                           Creditos.Cod_Credito EQ Cfg_TarjetaDb.Cod_Credito2)
                                      AND /* jjmp 28/08/2008 */ Creditos.Num_Credito EQ INT64(MOV_Tarjetas.Num_Cuenta) NO-LOCK NO-ERROR.
                IF AVAILABLE(Creditos) THEN DO:
                    ASSIGN W-AgeOrig = Creditos.agencia.
                    FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge EQ Creditos.agencia
                                           AND TmpMiCbte.Tcbte EQ Cfg_TarjetaDb.CbteCupo NO-LOCK NO-ERROR.
                    IF AVAILABLE(TmpMiCbte) THEN
                        ASSIGN W-DocAgeO = TmpMiCbte.Tnro.

                    /* Llevar contrapartida a la agencia administrativa */
                    FIND FIRST TmpMiCbte WHERE TmpMiCbte.tAge = Cfg_tarjetaDb.AgeAdmon
                                           AND TmpMiCbte.Tcbte = Cfg_TarjetaDb.CbteCupo NO-LOCK NO-ERROR.
                    IF AVAILABLE(TmpMiCbte) THEN
                        ASSIGN W-DocAgeD = TmpMiCbte.Tnro
                               W-AgeDest = Cfg_tarjetaDb.AgeAdmon.
                    ELSE
                        ASSIGN W-DocAgeD = 0
                               W-AgeDest = Cfg_tarjetaDb.AgeAdmon.

                    RUN TipoTransaCoop.r(INPUT W-AgeOrig, INPUT W-AgeDest, INPUT-OUTPUT W-DocAgeO,
                                         INPUT-OUTPUT W-DocAgeD, INPUT Mov_Tarjeta.TarjetaDB, INPUT Mov_Tarjeta.TipoCuenta,
                                         INPUT TRIM(STRING(Creditos.Num_credito)), INPUT Mov_Tarjeta.TipoTransaccion, INPUT Mov_Tarjeta.Monto,
                                         INPUT Mov_Tarjeta.Comision, INPUT Mov_Tarjeta.ComAdicional, INPUT Mov_Tarjeta.Lugar,
                                         INPUT Mov_Tarjeta.Naud, INPUT Mov_Tarjeta.NroTerminal, INPUT Mov_Tarjeta.Fec_Transac,
                                         INPUT Mov_Tarjeta.Hora_Transac, INPUT Mov_Tarjeta.Nit, INPUT Mov_Tarjeta.TipoRed,
                                         INPUT Mov_Tarjeta.OtrosCargos, INPUT "998", /* Usuario */ INPUT FALSE,
                                         Creditos.Cod_Credito, /* jjmp 28/08/2008 */ OUTPUT W-RegDet).

                    ASSIGN mov_tarjetas.aplicado = TRUE.
                    
                    RUN Num_Documento.

                    CREATE Tmovgra.
                    ASSIGN Tmovgra.greg = W-RegDet.
                END.
                ELSE
                    RUN Graba_Incons.
            END.
        END.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

