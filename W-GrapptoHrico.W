&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-PptGen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-PptGen 
/*------------------------------------------------------------------------

  ARCHIVO: W-PPTGEN.W

  Description: Genera el presupuesto automatico
 ------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

 {Incluido\VARIABLE.I "SHARED"}

/* Local Variable Definitions ---                                       */

  DEFINE VAR Cont       AS   INTEGER.
  DEFINE VAR W_Error    AS   LOGICAL.
  DEFINE VAR W_HayInfo  AS   LOGICAL.  
  DEFINE VAR W_Cencos   LIKE Cen_Costos.Cen_Costos.
  DEFINE VAR W_SdoAux   LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SdoAcu   LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SdoAct   LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SdoCre   LIKE Sal_Presupuesto.Sdo_Final.
  DEFINE VAR W_SdoDeb   LIKE Sal_Presupuesto.Sdo_Final.

  DEFINE BUFFER Tmp-SalPpto FOR Sal_Presupuesto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_PptGen

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-70 W_AnoPre Btn_Ver R_Tipo W_Ano ~
W_Incremento Btn_Generar R_Forma Btn_Salir Btn_Ayuda RECT-36 RECT-38 ~
RECT-74 RECT-75 
&Scoped-Define DISPLAYED-OBJECTS W_OfiPre W_NomOfi W_AnoPre W_Generada ~
R_Tipo W_Ano W_Incremento R_Forma 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-PptGen AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4.86 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Generar 
     LABEL "&Generar" 
     SIZE 11 BY 1.88.

DEFINE BUTTON Btn_Salir AUTO-END-KEY DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ver 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "&Ver Cuentas" 
     SIZE 11 BY 1.88.

DEFINE BUTTON BUTTON-70 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 70" 
     SIZE 11 BY 1.92.

DEFINE VARIABLE W_Ano AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año de Referencia" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_AnoPre AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año a Crear" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_Generada AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cuenta Generada" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Incremento AS DECIMAL FORMAT ">,>>9.99":U INITIAL 0 
     LABEL "Porcentaje Incremento" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomOfi AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OfiPre AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "agencia" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE R_Forma AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mes a Mes", 1,
"Final / 12", 2
     SIZE 36 BY .54
     BGCOLOR 18 FONT 6 NO-UNDO.

DEFINE VARIABLE R_Tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Saldos Presupuesto", 1,
"Saldos Cuentas", 2
     SIZE 44 BY .54
     BGCOLOR 18 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 1.35.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 5.12.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 1.08.

DEFINE RECTANGLE RECT-75
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 1.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_PptGen
     BUTTON-70 AT ROW 3.69 COL 98
     W_OfiPre AT ROW 5.31 COL 23 COLON-ALIGNED HELP
          "Ingrese la agencia Inicial para la Creación del Presupuesto"
     W_NomOfi AT ROW 5.31 COL 29 COLON-ALIGNED NO-LABEL
     W_AnoPre AT ROW 6.65 COL 23 COLON-ALIGNED HELP
          "Ingrese el Año a crear para el Nuevo Presupuesto"
     W_Generada AT ROW 7.73 COL 23 COLON-ALIGNED
     Btn_Ver AT ROW 8.81 COL 98 HELP
          "Permite la Consulta de Presupuestos Existentes"
     R_Tipo AT ROW 10.15 COL 26 HELP
          "Seleccione el tipo de Prespuesto" NO-LABEL
     W_Ano AT ROW 11.5 COL 34 COLON-ALIGNED HELP
          "Ingrese el Año apartir del cual se toman los datos de Ppto."
     W_Incremento AT ROW 11.5 COL 58 COLON-ALIGNED HELP
          "Ingrese el Porcentaje de Incremento"
     Btn_Generar AT ROW 12.31 COL 98 HELP
          "Permite Generar el Nuevo Presupuesto"
     R_Forma AT ROW 13.12 COL 28 HELP
          "Seleccione la Manera para generar el nuevo Ppto." NO-LABEL
     Btn_Salir AT ROW 14.46 COL 98 HELP
          "Permite Regresar a la Opción Anterior"
     Btn_Ayuda AT ROW 19.04 COL 101 HELP
          "Permite Obtener la Ayuda de esta Panalla"
     RECT-36 AT ROW 11.23 COL 21
     RECT-38 AT ROW 11.77 COL 96
     RECT-74 AT ROW 9.88 COL 21
     RECT-75 AT ROW 12.85 COL 21
     "Datos Generales para la Creación del presupuesto." VIEW-AS TEXT
          SIZE 77 BY .62 AT ROW 3.96 COL 9
          BGCOLOR 18 FGCOLOR 15 FONT 6
     "Información Adicional." VIEW-AS TEXT
          SIZE 77 BY .62 AT ROW 8.81 COL 9
          BGCOLOR 18 FGCOLOR 15 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.43 BY 21.04
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-PptGen ASSIGN
         HIDDEN             = YES
         TITLE              = "Presupuesto sobre saldo historico"
         HEIGHT             = 21.08
         WIDTH              = 112.43
         MAX-HEIGHT         = 21.08
         MAX-WIDTH          = 112.43
         VIRTUAL-HEIGHT     = 21.08
         VIRTUAL-WIDTH      = 112.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 17
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
/* SETTINGS FOR WINDOW W-PptGen
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_PptGen
                                                                        */
/* SETTINGS FOR FILL-IN W_Generada IN FRAME F_PptGen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomOfi IN FRAME F_PptGen
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_OfiPre IN FRAME F_PptGen
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-PptGen)
THEN W-PptGen:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-PptGen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-PptGen W-PptGen
ON END-ERROR OF W-PptGen /* Presupuesto sobre saldo historico */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-PptGen W-PptGen
ON WINDOW-CLOSE OF W-PptGen /* Presupuesto sobre saldo historico */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-PptGen
ON CHOOSE OF Btn_Ayuda IN FRAME F_PptGen /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONTABIL" CONTEXT 8.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Generar W-PptGen
ON CHOOSE OF Btn_Generar IN FRAME F_PptGen /* Generar */
DO:
  ASSIGN R_Tipo W_Ano W_Incremento R_Forma.
  RUN Validar-Variables.
  IF W_Error THEN
     RETURN.
  RUN _SetCurs.r ("WAIT").
  DISPLAY "Espere" @ W_Generada WITH FRAME F_PptGen.
  CASE R_Tipo:
    WHEN 1 THEN
       RUN Presupuesto.
    WHEN 2 THEN 
       RUN Movimiento.
  END CASE.
  RUN _SetCurs.r ("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-PptGen
ON CHOOSE OF Btn_Salir IN FRAME F_PptGen /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ver W-PptGen
ON CHOOSE OF Btn_Ver IN FRAME F_PptGen /* Ver Cuentas */
DO:
  RUN W-PptDet.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-70
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-70 W-PptGen
ON CHOOSE OF BUTTON-70 IN FRAME F_PptGen /* Button 70 */
DO:
  RUN W-InfDia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Forma W-PptGen
ON LEAVE OF R_Forma IN FRAME F_PptGen
DO:
  APPLY "VALUE-CHANGED" TO R_Forma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Forma W-PptGen
ON MOUSE-SELECT-CLICK OF R_Forma IN FRAME F_PptGen
DO:
  APPLY "VALUE-CHANGED" TO R_Forma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Forma W-PptGen
ON VALUE-CHANGED OF R_Forma IN FRAME F_PptGen
OR RETURN OF R_Forma DO:
  ASSIGN R_Forma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tipo W-PptGen
ON LEAVE OF R_Tipo IN FRAME F_PptGen
DO:
  APPLY "VALUE-CHANGED" TO R_tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tipo W-PptGen
ON MOUSE-SELECT-CLICK OF R_Tipo IN FRAME F_PptGen
DO:
  APPLY "VALUE-CHANGED" TO R_tipo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tipo W-PptGen
ON VALUE-CHANGED OF R_Tipo IN FRAME F_PptGen
OR RETURN OF R_tipo DO:
  ASSIGN R_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Ano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Ano W-PptGen
ON LEAVE OF W_Ano IN FRAME F_PptGen /* Año de Referencia */
DO:
  ASSIGN W_Ano.
  IF W_Ano = 0 THEN
    RETURN.
  IF R_Tipo = 1 THEN DO:
    FIND FIRST Sal_presupuesto WHERE Sal_Presupuesto.Ano = W_Ano NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Sal_Presupuesto) THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 290, OUTPUT W_Eleccion).
      APPLY "ENTRY" TO Btn_Salir.
      RETURN NO-APPLY.
    END.
  END.
  ELSE DO:
    FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Ano = W_Ano NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 290, OUTPUT W_Eleccion).
      APPLY "ENTRY" TO Btn_Salir.
      RETURN NO-APPLY.
    END.   
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AnoPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AnoPre W-PptGen
ON LEAVE OF W_AnoPre IN FRAME F_PptGen /* Año a Crear */
DO:
  ASSIGN W_AnoPre.
  IF W_AnoPre < YEAR(W_Fecha) THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 96, OUTPUT W_Eleccion).
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Incremento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Incremento W-PptGen
ON LEAVE OF W_Incremento IN FRAME F_PptGen /* Porcentaje Incremento */
DO:
  ASSIGN W_Incremento.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OfiPre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiPre W-PptGen
ON LEAVE OF W_OfiPre IN FRAME F_PptGen /* agencia */
DO:
  ASSIGN W_OfiPre.
  FIND agencias WHERE 
       agencias.Estado = 1 AND agencias.agencia = W_OfiPre NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(agencias) THEN
   DO:
     RUN P-BrwOfi.r (INPUT W_OfiPre,OUTPUT W_NomOfi).
     IF W_OfiPre = 0 THEN
      DO:
        RUN MostrarMensaje IN W_Manija (INPUT 18,OUTPUT W_Eleccion).
        APPLY "ENTRY" TO W_OfiPre.
        RETURN NO-APPLY.
      END.
   END.
  DISPLAY W_OfiPre W_NomOfi WITH FRAME F_PptGen.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-PptGen 


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
  W_Ano = YEAR(TODAY) - 1.
  IF W_UbiDatos = "C" THEN
     ENABLE W_OfiPre WITH FRAME F_PptGen.
  ASSIGN  W_OfiPre     = W_agencia
          W_NomOfi     = TRIM(W_Nom_agencia)
          W_AnoPre     = YEAR(TODAY) + 1
          W_Ano        = (W_AnoPre - 1).

  RUN enable_UI.
  DISPLAY W_AnoPre W_Ano
          WITH FRAME F_PptGen.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borrar-Presupuesto W-PptGen 
PROCEDURE Borrar-Presupuesto :
/*------------------------------------------------------------------------------
  Objetivo:     Borra los saldos de presupuesto de esa agencia y centro de costos
                para el a¤o especificado en la pantalla
------------------------------------------------------------------------------*/
  RUN MostrarMensaje IN W_Manija (INPUT 344, OUTPUT W_Eleccion).
  IF NOT W_Eleccion THEN 
    RETURN ERROR.

  FOR EACH Sal_Presupuesto  WHERE Sal_Presupuesto.agencia   EQ W_OfiPre 
                              AND Sal_Presupuesto.Ano       EQ W_AnoPre:
    DELETE Sal_Presupuesto.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcula-Saldo W-PptGen 
PROCEDURE Calcula-Saldo :
/*------------------------------------------------------------------------------
  objetivo  :   Crea el ppto desde sal_cuentas mes por mes   
------------------------------------------------------------------------------*/
 FOR EACH Cuentas FIELDS ( Cuentas.Tipo Cuentas.Id_Presupuesto
             Cuentas.Estado Cuentas.Cod_Presupuesto Cuentas.Cuenta Cuentas.Naturaleza )
                          WHERE Cuentas.Tipo            EQ 2
                            AND Cuentas.Id_Presupuesto  EQ TRUE 
                            AND Cuentas.Estado          EQ 1
                        NO-LOCK:
    DISPLAY Cuentas.Cod_Presupuesto @ W_Generada WITH FRAME F_PptGen.

    FOR EACH Cen_Costos WHERE Cen_Costos.agencia = W_OfiPre
                          AND Cen_Costos.Estado  = 1
                      NO-LOCK:
           W_HayInfo = FALSE.
           DO Cont = 1 TO 12 BY 1:
             W_SdoAcu = 0.

             FIND Sal_Cuenta WHERE Sal_Cuenta.Cuenta     EQ Cuentas.Cuenta 
                               AND Sal_Cuenta.agencia    EQ W_OfiPre 
                               AND Sal_Cuenta.Cen_Costos EQ Cen_Costos.Cen_Costos
                               AND Sal_cuenta.Ano        EQ W_Ano 
                               AND Sal_Cuenta.Mes        EQ Cont
                           NO-LOCK NO-ERROR.

             IF AVAILABLE(Sal_Cuenta) THEN DO:
               W_HayInfo = TRUE.
               IF Cuentas.Naturaleza = "DB" THEN
                 W_SdoAcu = W_SdoAcu + (Sal_Cuenta.Sal_Debito - Sal_Cuenta.Sal_Credito).
               ELSE
                 W_SdoAcu = W_SdoAcu + (Sal_Cuenta.Sal_Credito - Sal_cuenta.Sal_Debito).
             END.                             /* end del If */

             IF W_HayInfo THEN
               RUN Ppto_MesXMesMov.

           END.  /* End del do Cont */
       END.                                /* End de centro de centro costos */
    END. /* End de centro de cuentas */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-PptGen  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-PptGen)
  THEN DELETE WIDGET W-PptGen.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-PptGen  _DEFAULT-ENABLE
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
  DISPLAY W_OfiPre W_NomOfi W_AnoPre W_Generada R_Tipo W_Ano W_Incremento 
          R_Forma 
      WITH FRAME F_PptGen IN WINDOW W-PptGen.
  ENABLE BUTTON-70 W_AnoPre Btn_Ver R_Tipo W_Ano W_Incremento Btn_Generar 
         R_Forma Btn_Salir Btn_Ayuda RECT-36 RECT-38 RECT-74 RECT-75 
      WITH FRAME F_PptGen IN WINDOW W-PptGen.
  {&OPEN-BROWSERS-IN-QUERY-F_PptGen}
  VIEW W-PptGen.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Final/12-Movimiento W-PptGen 
PROCEDURE Final/12-Movimiento :
/*------------------------------------------------------------------------------
  Objetivo:   Suma todos los saldos del año a tomar y le asigna a cada mes del
              año a generar este valor / 12
------------------------------------------------------------------------------*/
    FOR EACH Cuentas FIELDS ( Cuentas.Tipo Cuentas.Id_Presupuesto
                              Cuentas.Estado Cuentas.Cod_Presupuesto Cuentas.Cuenta 
                              Cuentas.Naturaleza )
                      WHERE Cuentas.Tipo            EQ 2
                        AND Cuentas.Id_Presupuesto  EQ TRUE 
                        AND Cuentas.Estado          EQ 1
                    NO-LOCK:
       DISPLAY Cuentas.Cod_Presupuesto @ W_Generada WITH FRAME F_PptGen.

       FOR EACH Cen_Costos WHERE Cen_Costos.agencia = W_OfiPre
                             AND Cen_Costos.Estado  = 1
                         NO-LOCK:
           ASSIGN W_HayInfo = FALSE
                  W_SdoAcu  = 0.
           DO Cont = 1 TO 12 BY 1:
             FIND Sal_Cuenta WHERE Sal_Cuenta.Cuenta     EQ Cuentas.Cuenta 
                               AND Sal_Cuenta.agencia    EQ W_OfiPre 
                               AND Sal_Cuenta.Cen_Costos EQ Cen_Costos.Cen_Costos
                               AND Sal_cuenta.Ano        EQ W_Ano 
                               AND Sal_Cuenta.Mes        EQ Cont
                           NO-LOCK NO-ERROR.

             IF AVAILABLE(Sal_Cuenta) THEN DO:
               W_HayInfo = TRUE.
               IF Cuentas.Naturaleza = "DB" THEN
                 W_SdoAcu = W_SdoAcu + (Sal_Cuenta.Sal_Debito - Sal_Cuenta.Sal_Credito).
               ELSE
                 W_SdoAcu = W_SdoAcu + (Sal_Cuenta.Sal_Credito - Sal_cuenta.Sal_Debito).
             END.                             /* end del If */
           END.  /* End del do Cont */

           IF W_HayInfo THEN DO:
              W_SdoAcu = W_SdoAcu / 12.
              DO Cont = 1 TO 12 BY 1:
                RUN Ppto_MesxMesMov.
              END.
           END.   /* End de w_hayinfo */
       END.     /* End de centro de centro costos */
    END. /* End de centro de cuentas */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Final/12-Presupuesto W-PptGen 
PROCEDURE Final/12-Presupuesto :
/*------------------------------------------------------------------------------
  Objetivo:   Suma todos los saldos del a¤o a tomar y le asigna a cada mes del
              a¤o a generar este valor / 12
------------------------------------------------------------------------------*/
  W_SdoAux = 0.
  FOR EACH Cen_Costos WHERE Cen_Costos.agencia = W_OfiPre
                        AND Cen_Costos.Estado  = 1
                    NO-LOCK:
    ASSIGN W_SdoAux  = 0
           W_Cencos  = Cen_Costos.Cen_Costos.
           W_HayInfo = FALSE.
     RUN Ppto_Fin/12.
  END.   /* End del each cen_costos */
  ASSIGN W_SdoAux = 0
         W_Cencos = 0.
  RUN Ppto_Fin/12.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MesXMes-Presupuesto W-PptGen 
PROCEDURE MesXMes-Presupuesto :
/*------------------------------------------------------------------------------
  Objetivo:     Crear el presupuesto mes por mes segun los saldos de presupuesto
                o los saldos de movimiento del a¤o a tomar.
------------------------------------------------------------------------------*/  
  DO Cont = 1 TO 12 BY 1:
   FOR EACH Cen_Costos WHERE Cen_Costos.agencia EQ W_OfiPre
                         AND Cen_Costos.Estado  EQ 1
                     NO-LOCK:
     W_CenCos = Cen_costos.Cen_costos.
     RUN Ppto_MesXMes.
   END.  /*  End del centro de costos */

   W_CenCos = 0.
   RUN Ppto_MesXMes.

  END.  /* end del do */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movimiento W-PptGen 
PROCEDURE Movimiento :
/*------------------------------------------------------------------------------
  Objetivo:    Crea el presupuesto segun las cuentas del Movimiento del a¤o
               especificado y con el incremento sugerido.
------------------------------------------------------------------------------*/
 RUN Borrar-Presupuesto.
 CASE R_Forma:
    WHEN 1 THEN 
      RUN Calcula-Saldo.  /*Calcula mesxmes movimiento */
    WHEN 2 THEN
      RUN Final/12-Movimiento.
 END CASE.
 RUN MostrarMensaje IN W_Manija (INPUT 98, OUTPUT W_Eleccion).
 DISPLAY "" @ W_Generada WITH FRAME F_PptGen.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ppto_Fin/12 W-PptGen 
PROCEDURE Ppto_Fin/12 :
/*------------------------------------------------------------------------------
  Purpose:  presupuesto se genera tomando saldos de presupuesto por cada codigo, y
            dividirlo entre 12, esto determina el valor para cada mes.   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Cod_Presupuesto EQ Presupuesto.Cod_Presupuesto
                               AND Sal_Presupuesto.agencia         EQ W_OfiPre
                               AND Sal_Presupuesto.Cen_Costos      EQ W_CenCos
                               AND Sal_Presupuesto.Ano             EQ W_Ano
                           NO-LOCK:
       W_HayInfo = TRUE.
       W_SdoAux = W_SdoAux + Sal_Presupuesto.Sdo_Final.
    END. /* end del each sal_presupuesto */
 
    IF W_HayInfo THEN DO:
      W_SdoAux = W_SdoAux / 12.
      DO Cont = 1 TO 12 BY 1:
        CREATE Sal_Presupuesto.
        ASSIGN Sal_Presupuesto.Cod_Presupuesto = Presupuesto.Cod_Presupuesto
               Sal_Presupuesto.agencia         = W_OfiPre
               Sal_Presupuesto.Cen_Costos      = W_CenCos
               Sal_Presupuesto.Ano             = W_AnoPre
               Sal_Presupuesto.Mes             = Cont.
        IF W_Incremento <> 0 THEN 
           Sal_Presupuesto.Sdo_Final = W_SdoAux + (W_SdoAux * (W_Incremento / 100)).
        ELSE
           Sal_Presupuesto.Sdo_Final = W_SdoAux.
      END.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ppto_MesXMes W-PptGen 
PROCEDURE Ppto_MesXMes :
/*------------------------------------------------------------------------------
  Purpose: crea ppto mes a mes desde saldos de presupuesto.   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     FOR EACH Sal_Presupuesto WHERE Sal_Presupuesto.Cod_Presupuesto EQ Presupuesto.Cod_Presupuesto
                                AND Sal_Presupuesto.agencia         EQ W_OfiPre
                                AND Sal_Presupuesto.Ano             EQ W_Ano 
                                AND Sal_Presupuesto.Mes             EQ Cont 
                                AND Sal_Presupuesto.Cen_Costos      EQ W_CenCos
                            NO-LOCK:
        W_SdoAux = Sal_Presupuesto.Sdo_Final.
        FIND Tmp-SalPpto WHERE Tmp-Salppto.Cod_Presupuesto EQ Presupuesto.Cod_Presupuesto
                           AND Tmp-Salppto.agencia         EQ W_OfiPre
                           AND Tmp-Salppto.Ano             EQ W_AnoPre
                           AND Tmp-Salppto.Cen_Costos      EQ Sal_Presupuesto.Cen_Costos 
                           AND Tmp-Salppto.Mes             EQ Cont NO-ERROR. 
        IF AVAILABLE(Tmp-Salppto) THEN DO:
          ASSIGN Tmp-Salppto.Cod_Presupuesto = Presupuesto.Cod_Presupuesto
                 Tmp-Salppto.agencia         = W_OfiPre
                 Tmp-Salppto.Cen_Costos      = Sal_Presupuesto.Cen_Costos 
                 Tmp-Salppto.Ano             = W_AnoPre
                 Tmp-Salppto.Mes             = Cont.
          IF W_Incremento <> 0 THEN
            Tmp-Salppto.Sdo_Final = Tmp-Salppto.Sdo_Final + W_SdoAux + 
                                    (W_SdoAux * W_Incremento / 100).
          ELSE
            Tmp-Salppto.Sdo_Final = W_SdoAux.
        END.
        ELSE DO:
          CREATE Tmp-Salppto.
          ASSIGN Tmp-Salppto.Cod_Presupuesto = Presupuesto.Cod_Presupuesto
                 Tmp-Salppto.agencia         = W_OfiPre
                 Tmp-Salppto.Cen_Costos      = Sal_Presupuesto.Cen_Costos 
                 Tmp-Salppto.Mes             = Cont
                 Tmp-Salppto.Sdo_Final       = 0
                 Tmp-Salppto.Ano             = W_AnoPre.
          IF W_Incremento <> 0 THEN
            Tmp-Salppto.Sdo_Final =  W_SdoAux + (W_SdoAux * W_Incremento / 100).
          ELSE
            Tmp-Salppto.Sdo_Final = W_SdoAux.
        END.     
     END. /* end del each */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ppto_MesXMesMov W-PptGen 
PROCEDURE Ppto_MesXMesMov :
/*------------------------------------------------------------------------------
  Purpose: Crea el presupuesto mes por mes desde movimiento    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND Sal_Presupuesto WHERE Sal_Presupuesto.agencia         = W_OfiPre 
                         AND Sal_Presupuesto.Cen_Costos      = Cen_Costos.Cen_costos
                         AND Sal_Presupuesto.Cod_Presupuesto = Cuentas.Cod_Presupuesto
                         AND Sal_Presupuesto.Ano             = W_AnoPre
                         AND Sal_Presupuesto.Mes             = Cont NO-ERROR.
  IF NOT AVAILABLE(Sal_Presupuesto) THEN DO:
    CREATE Sal_Presupuesto.
    ASSIGN Sal_Presupuesto.Cod_Presupuesto = Cuentas.Cod_Presupuesto
           Sal_Presupuesto.agencia         = W_OfiPre
           Sal_Presupuesto.Cen_Costos      = Cen_Costos.Cen_costos
           Sal_Presupuesto.Ano             = W_AnoPre
           Sal_Presupuesto.Mes             = Cont.
    IF W_Incremento <> 0 THEN
       Sal_Presupuesto.Sdo_Final = W_SdoAcu + (W_SdoAcu * W_Incremento / 100).
    ELSE
       Sal_Presupuesto.Sdo_Final = W_SdoAcu.
  END.
  ELSE DO:
    IF W_Incremento <> 0 THEN
       Sal_Presupuesto.Sdo_Final = Sal_Presupuesto.Sdo_Final + W_SdoAcu + 
                                   ( W_SdoAcu * W_Incremento / 100 ).
    ELSE
       Sal_Presupuesto.Sdo_Final = Sal_Presupuesto.Sdo_Final + W_SdoAcu.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Presupuesto W-PptGen 
PROCEDURE Presupuesto :
/*------------------------------------------------------------------------------
  Objetivo :   Crea el presupuesto segun las cuentas del presupuesto del a¤o
               especificado y con el incremento sugerido.
------------------------------------------------------------------------------*/
 RUN Borrar-Presupuesto.
 
 FOR EACH Presupuesto WHERE Presupuesto.Estado = 1 
                        AND Presupuesto.Fec_Retiro = ? NO-LOCK:
    CASE R_Forma:
      WHEN 1 THEN
        RUN MesXMes-Presupuesto.
      WHEN 2 THEN
        RUN Final/12-Presupuesto.
    END CASE.
    DISPLAY Presupuesto.Cod_Presupuesto @ W_Generada WITH FRAME F_PptGen.
 END.
 RUN MostrarMensaje IN W_Manija (INPUT 98, OUTPUT W_Eleccion).
 DISPLAY "" @ W_Generada WITH FRAME F_PptGen.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar-Variables W-PptGen 
PROCEDURE Validar-Variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  W_Error = TRUE.
  FIND agencias WHERE 
       agencias.Estado = 1 AND agencias.agencia = W_OfiPre NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(agencias) THEN
   DO:
     RUN MostrarMensaje IN W_Manija (INPUT 18,OUTPUT W_Eleccion).
     RETURN ERROR.
   END.

  FIND FIRST Sal_presupuesto WHERE Sal_Presupuesto.Ano = W_Ano NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Sal_Presupuesto) THEN
   DO:
     RUN MostrarMensaje IN W_Manija (INPUT 290, OUTPUT W_Eleccion).
     APPLY "ENTRY" TO Btn_Salir IN FRAME F_pptGen.
     RETURN NO-APPLY.
   END.
  W_Error = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

