&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_CierreDia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_CierreDia 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE TEMP-TABLE Total_Dia LIKE Total_Agencia
    FIELD TasaXSdo AS DECIMAL
    FIELD TTasa AS DECIMAL.

DEFINE TEMP-TABLE TOTAL_Empresa
    FIELD Cod_empresa AS INTEGER
    FIELD Valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

/* Cuentas para cierre de impuestos mensual */
DEFINE TEMP-TABLE cierreCuentas
    FIELD cuenta AS CHARACTER.

CREATE cierreCuentas.   cierreCuentas.cuenta = "24300501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24301001".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24301002".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24302001".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24309501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24350501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24351501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24351502".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24351503".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24351504".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352502".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352503".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352504".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352505".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352506".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352507".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24352508".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353001".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353002".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353502".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353503".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353504".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353507".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353508".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353509".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24353510".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24354001".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24354002".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24358501".
CREATE cierreCuentas.   cierreCuentas.cuenta = "24358502".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-120 RECT-314 BUTTON-5 W_CmbOfi ~
Btn_Procesar Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_DiaC W_MesC W_Nmes W_AnoC ~
W_mensaje W_Cont 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 W_MesC W_AnoC 
&Scoped-define List-2 W_mensaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Prc_CierreDia AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.27
     BGCOLOR 8 .

DEFINE BUTTON Btn_Procesar 
     LABEL "&Procesar" 
     SIZE 10 BY 1.46 TOOLTIP "Realiza el Proceso de Cierre".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 33.86 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_AnoC AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Dia" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.14 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_MesC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nmes AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-120
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.57 BY 6.73.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.29 BY 6.73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     BUTTON-5 AT ROW 3.42 COL 62.72
     W_CmbOfi AT ROW 3.88 COL 12.72 COLON-ALIGNED
     Btn_Procesar AT ROW 5.81 COL 62.86 HELP
          "Permite Realizar el Proceso de Cierre"
     W_DiaC AT ROW 6.31 COL 12.72 COLON-ALIGNED
     W_MesC AT ROW 6.31 COL 20.72 COLON-ALIGNED
     W_Nmes AT ROW 6.31 COL 24 COLON-ALIGNED NO-LABEL
     W_AnoC AT ROW 6.31 COL 42.14 COLON-ALIGNED
     Btn_Done AT ROW 8 COL 62.86 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_mensaje AT ROW 8.73 COL 4.86 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 8.73 COL 39 COLON-ALIGNED NO-LABEL
     "RegistrosTotales" VIEW-AS TEXT
          SIZE 11.43 BY .5 AT ROW 8.19 COL 41
          BGCOLOR 1 
     RECT-120 AT ROW 2.85 COL 6.57
     RECT-314 AT ROW 2.85 COL 60.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.86 BY 11.35
         BGCOLOR 17 FGCOLOR 0 FONT 4.


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
  CREATE WINDOW W-Prc_CierreDia ASSIGN
         HIDDEN             = YES
         TITLE              = "Proceso de Cierre Diario, Programa W-Prc_CierreDia.W"
         HEIGHT             = 11.35
         WIDTH              = 80.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 85.72
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 85.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW W-Prc_CierreDia
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_AnoC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaC IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_mensaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_MesC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Nmes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_CierreDia)
THEN W-Prc_CierreDia:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Prc_CierreDia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_CierreDia W-Prc_CierreDia
ON END-ERROR OF W-Prc_CierreDia /* Proceso de Cierre Diario, Programa W-Prc_CierreDia.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_CierreDia W-Prc_CierreDia
ON WINDOW-CLOSE OF W-Prc_CierreDia /* Proceso de Cierre Diario, Programa W-Prc_CierreDia.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Prc_CierreDia
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Procesar W-Prc_CierreDia
ON CHOOSE OF Btn_Procesar IN FRAME F_Proc /* Procesar */
DO:
    RUN _SetCurs.r("WAIT").

    RUN Proceso NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        W_Mensaje:SCREEN-VALUE = "El Proceso tiene errores...Verifique por favor.".
        RUN _SetCurs.r("ARROW").
        RETURN.
    END.
    ELSE
        W_Mensaje:SCREEN-VALUE = "Generado el Proceso de Cierre Diario!...!".

    RUN borrarMovAhorros.
    RUN borrarMovCreditos.
    RUN borrarMovContable.

    MESSAGE "El proceso de cierre se realizó con éxito"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RUN _SetCurs.r("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Prc_CierreDia
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Prc_CierreDia
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
  IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
     FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                           AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiIni = Agencias.Agencia.
        
     FIND LAST Agencias WHERE Agencias.Agencia GT 0
                          AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiFin = Agencias.Agencia.
  END.
  ELSE ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
              W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Prc_CierreDia 


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

   W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
   ASSIGN W_OfiIni                            = W_Agencia
          W_OfiFin                            = W_Agencia
          W_MesC:SCREEN-VALUE IN FRAME F_Proc = STRING(MONTH(W_Fecha))
          W_DiaC:SCREEN-VALUE                 = STRING(DAY(W_Fecha))
          W_AnoC:SCREEN-VALUE                 = STRING(YEAR(W_Fecha)).
                  
   FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                       AND Agencias.Agencia GT 0 NO-LOCK:
       W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).
       IF W_OfiCierre NE 0 AND Agencias.Agencia EQ W_OfiCierre THEN
          ASSIGN W_CmbOfi:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + "-" + 
                                         STRING(Agencias.Nombre,"X(25)").
   END.              
     
   IF W_OfiCierre EQ 0 THEN
      ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
   APPLY "VALUE-CHANGED" TO W_CmbOfi.
    
   CASE MONTH(W_Fecha):
        WHEN 1 THEN
          W_NMes = "Enero".
        WHEN 2 THEN
          W_NMes = "Febrero".
        WHEN 3 THEN
          W_NMes = "Marzo".
        WHEN 4 THEN
          W_NMes = "Abril".
        WHEN 5 THEN
          W_NMes = "Mayo".
        WHEN 6 THEN
          W_NMes = "Junio".
        WHEN 7 THEN
          W_NMes = "Julio".
        WHEN 8 THEN
          W_NMes = "Agosto".
        WHEN 9 THEN
          W_NMes = "Septiembre".
        WHEN 10 THEN
          W_NMes = "Octubre".
        WHEN 11 THEN
          W_NMes = "Noviembre".
        WHEN 12 THEN
          W_NMes = "Diciembre".
   END CASE. 

   W_NMes:SCREEN-VALUE = W_NMes.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BorrarMovAhorros W-Prc_CierreDia 
PROCEDURE BorrarMovAhorros :
/*DISABLE TRIGGERS FOR LOAD OF mov_ahorros.

FOR EACH ahorros WHERE ahorros.estado = 2 NO-LOCK:
    FOR EACH mov_ahorros WHERE mov_ahorros.agencia = ahorros.agencia
                           AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                           AND mov_ahorros.cue_ahorro = ahorros.cue_ahorro
                           AND mov_ahorros.fecha <= ADD-INTERVAL(w_fecha,-5,"years")
                           AND mov_ahorros.nit = ahorros.nit:
        DELETE mov_ahorros.
    END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BorrarMovContable W-Prc_CierreDia 
PROCEDURE BorrarMovContable :
/*DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE fec_contable <= ADD-INTERVAL(w_fecha,-5,"years"):
    DELETE mov_contable.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BorrarMovCreditos W-Prc_CierreDia 
PROCEDURE BorrarMovCreditos :
/*DISABLE TRIGGERS FOR LOAD OF mov_creditos.

FOR EACH creditos WHERE creditos.estado = 3 NO-LOCK:
    FOR EACH mov_creditos WHERE mov_creditos.agencia = creditos.agencia
                            AND mov_creditos.cod_credito = creditos.cod_credito
                            AND mov_creditos.num_credito = creditos.num_credito
                            AND mov_creditos.fecha <= ADD-INTERVAL(w_fecha,-5,"years")
                            AND mov_creditos.nit = creditos.nit:
        DELETE mov_creditos.
    END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Prc_CierreDia  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_CierreDia)
  THEN DELETE WIDGET W-Prc_CierreDia.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Prc_CierreDia  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi W_DiaC W_MesC W_Nmes W_AnoC W_mensaje W_Cont 
      WITH FRAME F_Proc IN WINDOW W-Prc_CierreDia.
  ENABLE RECT-120 RECT-314 BUTTON-5 W_CmbOfi Btn_Procesar Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Prc_CierreDia.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Prc_CierreDia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W-Prc_CierreDia 
PROCEDURE Proceso :
DEFINE VAR W_FecCorte AS DATE.
DEFINE VAR K AS INTEGER.
DEFINE VAR id AS LOGICAL.
DEFINE VAR pSaldoCuenta AS DECIMAL.
DEFINE VAR vSecuencia AS INTEGER.

/*Validaciones*/
FIND FIRST Agencias WHERE Agencias.Agencia >= W_OfiIni
                      AND Agencias.Agencia <= W_OfiFin
                      AND Agencias.Estado = 2 NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Agencias) THEN DO:
    MESSAGE "No existe Agencia en estado de Cierre...?" SKIP
            "                     Revise por favor. No se permite la operación."
        VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

    RETURN ERROR.
END.

FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                    AND Agencias.Agencia <= W_OfiFin
                    AND Agencias.Estado = 2 NO-LOCK:   /*Solo Agencias con estado Cerradas*/
    FIND FIRST Usuarios WHERE Usuarios.Agencia EQ Agencias.Agencia
                          AND Usuarios.Usuario NE W_Usuario
                          AND Usuarios.Estado EQ 1
                          AND Usuarios.Id_Entrada NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN DO:
        MESSAGE "El Usuario    :" Usuarios.Usuario "-" Usuarios.Nombre SKIP
                "De la Agencia :" Agencias.Agencia "-" Agencias.Nombre SKIP
                "está conectado al Aplicativo... Todos los usuarios deben estar" skip
                "desconectados antes de realizar este proceso.."
            VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".
        
        W-Prc_CierreDia:SENSITIVE = FALSE.

        RUN W-Control_Usuarios.r.

        W-Prc_CierreDia:SENSITIVE = TRUE.
        
        W-Prc_CierreDia:MOVE-TO-TOP().

        FIND FIRST Usuarios WHERE Usuarios.Agencia EQ Agencias.Agencia
                              AND Usuarios.Usuario NE W_Usuario
                              AND Usuarios.Estado EQ 1
                              AND Usuarios.Id_Entrada NO-LOCK NO-ERROR.
        IF AVAILABLE(Usuarios) THEN DO:
            MESSAGE "Aun existen Usuarios conectados al Aplicativo."
                    "No se permite la realización del proceso..."
                VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

            RETURN ERROR.
        END.
    END.

    FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                         AND ProcDia.Fecha_Proc = W_Fecha
                         AND ProcDia.Cod_Proceso = 6
                         AND ProcDia.Estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(ProcDia) THEN DO:
        MESSAGE "Este proceso ya fue ejecutado para este día en la Agencia:" Agencias.Agencia SKIP
                "o no está matriculado... Revise por favor. No se permite la operación." SKIP
            VIEW-AS ALERT-BOX TITLE "Confirmar proceso".

        RETURN ERROR.
    END.

    FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                         AND ProcDia.Fecha_Proc = W_Fecha
                         AND ProcDia.Cod_Proceso <> 6
                         AND ProcDia.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAIL(ProcDia) THEN DO:
        MESSAGE "El Proceso:" ProcDia.Cod_Proceso "no se ejecutó para este día en la Agencia:" Agencias.Agencia SKIP
                "Revise por favor... No se permite la operación." SKIP
            VIEW-AS ALERT-BOX TITLE "Confirmar proceso".

        RETURN ERROR.
    END.
END.

DO TRANSACTION ON ERROR UNDO:
    ASSIGN W_Mensaje:SCREEN-VALUE IN FRAME F_Proc = "Revisando Listas de suspendidos...".

    FOR EACH listaNegra WHERE listaNegra.fec_exclusion <= w_fecha
                          AND listaNegra.estado = 1:
        listaNegra.estado = 2.
    END.

    FOR EACH listaNegra WHERE listaNegra.estado = 3:
        listaNegra.estado = 1.
    END.

    FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin
                        AND Agencias.Estado = 2:
        FIND FIRST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                                AND Calendario.Ano = YEAR(W_Fecha)
                                AND Calendario.Mes = MONTH(W_Fecha)
                                AND Calendario.Dia = DAY(W_Fecha) NO-ERROR.
        EMPTY TEMP-TABLE TOTAL_Dia.
        
        Calendario.Estado = 2.
        W_FecCorte = W_Fecha.

        /* Se revisa si es cierre de mes para reiniciar los comprobantes que asi se encuentren configurados */
        IF calendario.Cierre = TRUE THEN DO:
            FOR EACH comprobantes WHERE comprobantes.agencia = agencias.agencia
                                    AND comprobantes.ReiniciaCierre = TRUE:
                comprobantes.secuenciaCierre = comprobantes.secuencia.
                comprobantes.secuencia = 0.
            END.
        END.
        /* ----------------------------------------------------------------------------------------------- */

        RUN ProcTotales.     /*Genera tabla Temp Total_Agencia, diaria*/
            
        ASSIGN W_Mensaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Total_Agencia...Espere por Favor.".

        FOR EACH TOTAL_Dia WHERE TOTAL_Dia.Agencia EQ Agencias.Agencia:   /*Pasa la Temporal a la BD*/
            CREATE TOTAL_Agencia.
            BUFFER-COPY TOTAL_Dia TO TOTAL_Agencia.
        END.

        DO K = 1 TO 7:    /*Halla en los Pròximos 7 dìas el primero hàbil*/
            W_FecCorte = W_FecCorte + 1.

            FIND FIRST Calendario WHERE Calendario.Agencia = Agencias.Agencia
                                    AND Calendario.Ano = YEAR(W_FecCorte)
                                    AND Calendario.Mes = MONTH(W_FecCorte)
                                    AND Calendario.Dia = DAY(W_FecCorte)
                                    AND Calendario.Habil = TRUE NO-ERROR.
            IF AVAILABLE(Calendario) THEN DO:
                Calendario.Estado = 1.
                LEAVE.
            END.
        END.

        FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                             AND ProcDia.Fecha_Proc = W_Fecha
                             AND ProcDia.Cod_Proceso = 6
                             AND ProcDia.Estado = 1 NO-ERROR.

        ProcDia.Estado = 2.
        Agencias.Estado = 1.
    END.

    /* Llenado del repositorio */
    IF DAY(w_fecha + 1) = 1 THEN DO:
        FOR EACH ahorros WHERE ahorros.estado = 1 NO-LOCK:
            CREATE rep_ahorros.
            rep_ahorros.fecCorte = w_fecha.
            BUFFER-COPY ahorros TO rep_ahorros.
        END.

        FOR EACH creditos WHERE creditos.estado = 2 OR creditos.estado = 5 OR creditos.fec_canceTotal >= ADD-INTERVAL(w_fecha,-1,"months") + 1 NO-LOCK:
            CREATE rep_creditos.
            rep_creditos.fecCorte = w_fecha.
            BUFFER-COPY creditos TO rep_creditos.

            /* Ajustes para el reporte de CIFIN */
            IF rep_creditos.cuota = 0 THEN
                rep_creditos.cuota = INTEGER(rep_creditos.sdo_capital / rep_creditos.plazo) + rep_creditos.INT_corriente + rep_creditos.INT_difCobro + rep_credito.INT_morCobrar.

            IF rep_credito.cuota = ? THEN
                rep_creditos.cuota = 0.

            IF rep_creditos.sdo_capital = 0 THEN DO:
                rep_creditos.dias_atraso = 0.
                rep_creditos.val_atraso = 0.
                rep_creditos.cuo_atraso = 0.
            END.
            /* -------------------------------- */
        END.

        FOR EACH activosFijos NO-LOCK:
            CREATE rep_activosFijos.
            rep_activosFijos.fecCorte = w_fecha.
            BUFFER-COPY activosFijos TO rep_activosFijos.
        END.
    END.

    IF DAY(w_fecha + 1) = 1 THEN
        RUN trasladarCuentasDeImpuestos.
    
    /* Se llenan los Hábiles para tema Alojamientos desde la página web */
    FOR EACH habiles:
        DELETE habiles.
    END.

    FOR EACH ahorros WHERE ahorros.tip_ahorro = 4
                       AND ahorros.estado = 1 NO-LOCK BREAK BY ahorros.nit:
        IF FIRST-OF(ahorros.nit) THEN DO:
            FIND FIRST listaNegra WHERE listaNegra.nit = ahorros.nit
                                    AND listaNegra.estado = 1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE listaNegra THEN DO:
                FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    CREATE habiles.
                    Habiles.agencia = clientes.agencia.
                    Habiles.nit = clientes.nit.
                    Habiles.apellido1 = clientes.apellido1.
                    Habiles.apellido2 = clientes.apellido2.
                    Habiles.nombre = clientes.nombre.

                    FIND FIRST facultades WHERE facultades.agencia = clientes.agencia
                                            AND facultades.tipo = "F"
                                            AND facultades.codigo = STRING(Clientes.facultad,"99") NO-LOCK NO-ERROR.
                    IF AVAILABLE facultades THEN
                        habiles.facultad = facultades.nombre.
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotales W-Prc_CierreDia 
PROCEDURE ProcTotales :
FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK:
    CREATE TOTAL_Dia.
    ASSIGN TOTAL_Dia.Agencia = Agencias.Agencia
           TOTAL_Dia.Fecha = W_Fecha
           TOTAL_Dia.Clase_Producto = Pro_Ahorros.Tip_Ahorro
           TOTAL_Dia.Codigo = STRING(Pro_Ahorros.Cod_Ahorro,"999")
           TOTAL_Dia.Tipo_Producto = 1
           W_Cont = W_Cont + 1
           W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

    FOR EACH Ahorros WHERE Ahorros.Agencia EQ Agencias.Agencia
                       AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro:
        IF Calendario.Cierre THEN
            ASSIGN Ahorros.Sdo_AnualPerAnt[MONTH(W_Fecha)] = Ahorros.Sdo_Anuales [MONTH(W_Fecha)]
                   Ahorros.Sdo_Anuales [MONTH(W_Fecha)] = (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje)
                   Ahorros.Num_DepMes = 0
                   Ahorros.Num_RetMes = 0
                   Ahorros.Val_DepMes = 0
                   Ahorros.Val_RetMes = 0.

        ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado + Ahorros.Val_DepDia
               TOTAL_Dia.Vr_Retirado = TOTAL_Dia.Vr_Retirado + Ahorros.Val_RetDia
               Ahorros.Val_DepDia = 0
               Ahorros.Val_RetDia = 0
               Ahorros.Num_DepDia = 0
               Ahorros.Num_RetDia = 0.

        IF (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) EQ 0 AND Ahorros.Fec_Apertura NE W_Fecha AND Ahorros.Fec_Cancelac NE W_Fecha THEN
            NEXT.

        ASSIGN TOTAL_Dia.Cta_totales = TOTAL_Dia.Cta_totales + 1
               TOTAL_Dia.Sdo_dia = TOTAL_Dia.Sdo_dia + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje)
               TOTAL_Dia.TasaXSdo = TOTAL_Dia.TasaXSdo + ((Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) * Ahorros.Tasa)
               TOTAL_Dia.TTasa = TOTAL_Dia.TTasa + Ahorros.Tasa.

        IF Ahorros.Fec_Apertura EQ W_Fecha THEN
            TOTAL_Dia.Cta_nuevas = TOTAL_Dia.Cta_nuevas + 1.

        IF Ahorros.Fec_Cancelac EQ W_Fecha THEN
            TOTAL_Dia.Cta_Retiradas = TOTAL_Dia.Cta_Retiradas + 1.
    END.

    ASSIGN TOTAL_Dia.Tasa_Ponderada = TOTAL_Dia.TasaXSdo / TOTAL_Dia.Sdo_dia
           TOTAL_Dia.Tasa_promedio = TOTAL_Dia.TTasa / TOTAL_Dia.Cta_totales.
END.

RUN ProcTotCreditos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotContab W-Prc_CierreDia 
PROCEDURE ProcTotContab :
/*------------------------------------------------------------------------------
  Purpose:     Totales Contables desde Cuentas.Id_Total
 ------------------------------------------------------------------------------*/
 DEFI VAR K AS INTEG FORM "99".
 
 FOR EACH Cuentas WHERE Cuentas.Estado EQ 1
                    AND Cuentas.Tipo   EQ 2
                    AND Cuentas.Id_Total    NO-LOCK:
     CREATE TOTAL_Dia.
     ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia      
            TOTAL_Dia.Fecha          = W_Fecha                
            TOTAL_Dia.Clase_Producto = 3 
            TOTAL_Dia.Codigo         = Cuentas.Cuenta 
            TOTAL_Dia.Tipo_Producto  = 3
            W_Cont                   = W_Cont + 1
            W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont). 
        
     FIND FIRST Sal_Cuenta WHERE  Sal_Cuenta.Agencia EQ Agencias.Agencia AND
                                  Sal_Cuenta.Ano     EQ YEAR(W_Fecha)    AND
                                  Sal_Cuenta.Cuenta  EQ Cuentas.Cuenta
                                  NO-LOCK NO-ERROR.
     IF AVAIL(Sal_Cuenta) THEN DO:
        ASSIGN TOTAL_Dia.Sdo_Dia = Sal_Cuenta.Sal_Inicial. 
             
        DO K = 1 TO MONTH(W_Fecha):
           IF Cuentas.Natur EQ "DB" THEN
              ASSIGN TOTAL_Dia.Sdo_Dia = TOTAL_Dia.Sdo_Dia + (Sal_Cuenta.Db[K] - Sal_Cuenta.Cr[K]).
           ELSE 
              ASSIGN TOTAL_Dia.Sdo_Dia = TOTAL_Dia.Sdo_Dia + (Sal_Cuenta.Cr[K] - Sal_Cuenta.Db[K]).    
        END.
     END.
        
     FOR EACH Mov_Contable WHERE Mov_Contable.Agencia    EQ Agencias.Agencia
                            AND  Mov_Contable.Cuenta     EQ Cuentas.Cuenta
                            AND  Mov_Contable.Fec_Contab EQ W_Fecha NO-LOCK:
          
         ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado + Mov_Contable.Db
                TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado   + Mov_Contable.Cr.
     END.      
 END.

 RUN ProcTotInver.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotCreditos W-Prc_CierreDia 
PROCEDURE ProcTotCreditos :
/*------------------------------------------------------------------------------
  Purpose:   Totales de crèditos e Invoca a ProcTotContab.  
------------------------------------------------------------------------------*/
  FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK:
      CREATE TOTAL_Dia.
      ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia
             TOTAL_Dia.Fecha          = W_Fecha
             TOTAL_Dia.Clase_Producto = Pro_Creditos.Tip_Credito
             TOTAL_Dia.Codigo         = STRING(Pro_Creditos.Cod_Credito,"999")
             TOTAL_Dia.Tipo_Producto  = 2
             W_Cont                   = W_Cont + 1
             W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).
        
      FOR EACH Creditos WHERE Creditos.Agencia    EQ Agencias.Agencia
                         AND Creditos.Cod_Credito EQ Pro_Creditos.Cod_Credito:
          IF Calendario.Cierre THEN                 
             Creditos.Sdo_Anuales [MONTH(W_Fecha)] = Creditos.Sdo_Capital.

          /*actualiza el cupo actual en la tabla empresa, para informes estadisticos*/
          IF Creditos.Sdo_Capital NE 0 AND Creditos.FOR_Pago EQ 2 THEN DO:
             FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
             IF AVAILABLE Clientes THEN DO:
                FIND Total_Empresa WHERE Total_Empresa.Cod_Empresa EQ Clientes.Cod_Empresa NO-ERROR.
                IF NOT AVAILABLE(TOTAL_Empresa) THEN DO:
                   CREATE TOTAL_Empresa.
                   ASSIGN TOTAL_Empresa.Cod_Empresa = Clientes.Cod_Empresa.
                END.
                TOTAL_Empresa.Valor = TOTAL_Empresa.Valor + Creditos.Sdo_Capital.
             END.
          END.

          IF  Creditos.Sdo_Capital   EQ 0
          AND Creditos.Fec_Desemb    NE W_Fecha
          AND Creditos.Fec_CanceTot  NE W_Fecha THEN NEXT.
            
          ASSIGN TOTAL_Dia.Cta_totales = TOTAL_Dia.Cta_totales + 1   
                 TOTAL_Dia.Sdo_dia     = TOTAL_Dia.Sdo_dia + Creditos.Sdo_Capital
                 TOTAL_Dia.TasaXSdo    = TOTAL_Dia.TasaXSdo + 
                                         (Creditos.Sdo_Capital * Creditos.Tasa)
                 TOTAL_Dia.TTasa       = TOTAL_Dia.TTasa + Creditos.Tasa
                 Total_Dia.Val_Atraso  = Total_Dia.Val_Atraso + Creditos.Val_Atraso.

          IF Creditos.Fec_Desemb EQ W_Fecha THEN
             TOTAL_Dia.Cta_nuevas = TOTAL_Dia.Cta_nuevas + 1.
             
          IF Creditos.Fec_CanceTot EQ W_Fecha THEN
             TOTAL_Dia.Cta_Retiradas = TOTAL_Dia.Cta_Retiradas + 1.
      END.
        
      ASSIGN TOTAL_Dia.Tasa_Ponderada = TOTAL_Dia.TasaXSdo / TOTAL_Dia.Sdo_dia
             TOTAL_Dia.Tasa_promedio  = TOTAL_Dia.TTasa    / TOTAL_Dia.Cta_totales.
            
      FOR EACH Taquilla WHERE Taquilla.Agencia   EQ Agencias.Agencia
                         AND  Taquilla.Fec_Trans EQ W_Fecha NO-LOCK:
          IF  Taquilla.Tip_Prod EQ 2
          AND Taquilla.Cod_Prod EQ Pro_Creditos.Cod_Credito THEN DO:
              IF Taquilla.Naturaleza EQ "CR" THEN
                 ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado +
                                                  (Taquilla.Val_Cheque + Taquilla.Val_Efectivo).
              ELSE 
                 ASSIGN TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado +
                                                  (Taquilla.Val_Cheque + Taquilla.Val_Efectivo).
          END.
      END.
                
  END.

  RUN ProcTotContab.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotInver W-Prc_CierreDia 
PROCEDURE ProcTotInver :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
 FOR EACH Pro_Inversiones NO-LOCK BY Pro_Inversiones.Categoria
                                  BY Pro_Inversiones.Cod_Producto:
     CREATE TOTAL_Dia.
     ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia      
            TOTAL_Dia.Fecha          = W_Fecha                
            TOTAL_Dia.Clase_Producto = Pro_Inversiones.Categoria
            TOTAL_Dia.Codigo         = STRING(Pro_Inversiones.Cod_Producto,"999")
            TOTAL_Dia.Tipo_Producto  = 4
            W_Cont                   = W_Cont + 1
            W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont). 
              
     FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Agencia      EQ Agencias.Agencia
                               AND Inversion_Sdos.Cod_Producto EQ Pro_Inversiones.Cod_Producto
                                   NO-LOCK :
         IF  Inversion_Sdos.Sdo_Actual   EQ 0
         AND Inversion_Sdos.Fec_Apertura NE W_Fecha
         AND Inversion_Sdos.Fec_Canc     NE W_Fecha THEN NEXT.
            
         ASSIGN TOTAL_Dia.Cta_Totales = TOTAL_Dia.Cta_Totales + 1   
                TOTAL_Dia.Sdo_dia     = TOTAL_Dia.Sdo_dia + Inversion_Sdos.Sdo_Actual
                TOTAL_Dia.TasaXSdo    = TOTAL_Dia.TasaXSdo + 
                                        Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual
                TOTAL_Dia.TTasa       = TOTAL_Dia.TTasa + Inversion_Sdos.Tasa_NomiAnual.

         IF Inversion_Sdos.Fec_Apertura EQ W_Fecha THEN
            TOTAL_Dia.Cta_nuevas = TOTAL_Dia.Cta_nuevas + 1.
             
         IF Inversion_Sdos.Fec_Canc EQ W_Fecha THEN
            TOTAL_Dia.Cta_Retiradas = TOTAL_Dia.Cta_Retiradas + 1.
     END.
          
     ASSIGN TOTAL_Dia.Tasa_Ponderada = TOTAL_Dia.TasaXSdo / TOTAL_Dia.Sdo_dia
            TOTAL_Dia.Tasa_promedio  = TOTAL_Dia.TTasa    / TOTAL_Dia.Cta_totales.

     FOR EACH Mov_Inversion WHERE Mov_Inversion.Agencia    EQ Agencias.Agencia
                              AND Mov_Inversion.Cod_Produc EQ Pro_Inversiones.Cod_Producto
                              AND Mov_Inversion.Fecha      EQ W_Fecha NO-LOCK:
         ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado + Mov_Inversion.Vr_Consig
                TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado   + Mov_Inversion.Vr_Retiro.
     END.    
 END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trasladarCuentasDeImpuestos W-Prc_CierreDia 
PROCEDURE trasladarCuentasDeImpuestos :
DEFINE VAR vSecuencia AS INTEGER.
DEFINE VAR vTotal2430 AS DECIMAL.
DEFINE VAR vTotal2435 AS DECIMAL.
DEFINE VAR flagContabiliza AS LOGICAL.
DEFINE VAR pSaldoCuenta AS DECIMAL.
    
FOR EACH agencias WHERE agencias.agencia <> 1 NO-LOCK:
    vTotal2430 = 0.
    vTotal2435 = 0.
    flagContabiliza = FALSE.

    FOR EACH cierreCuentas NO-LOCK:
        vTotal2430 = 0.
        vTotal2435 = 0.

        FOR EACH cen_costos NO-LOCK:
            RUN hallarSaldoCuenta IN w_manija (INPUT agencias.agencia,
                                               INPUT cen_costos.cen_costo,
                                               INPUT cierreCuentas.cuenta,
                                               INPUT YEAR(w_fecha),
                                               INPUT MONTH(w_fecha),
                                               OUTPUT pSaldoCuenta) NO-ERROR.
            IF pSaldoCuenta <> 0 THEN DO:
                IF flagContabiliza = FALSE THEN DO:
                    FIND FIRST comprobantes WHERE comprobantes.agencia = agencias.agencia AND comprobantes.comprobante = 20 NO-ERROR.
                    comprobantes.secuencia = comprobantes.secuencia + 1.
                    vSecuencia = comprobantes.secuencia.

                    FIND CURRENT comprobantes NO-LOCK.
                    flagContabiliza = TRUE.
                END.

                CREATE mov_contable.
                Mov_Contable.agencia = agencias.agencia.
                Mov_Contable.Cen_Costos = cen_costos.cen_costo.
                Mov_Contable.Comentario = "Traslado automático".
                Mov_Contable.Comprobante = comprobantes.comprobante.
                ASSIGN Mov_Contable.Db = pSaldoCuenta WHEN pSaldoCuenta > 0.
                ASSIGN Mov_Contable.Cr = pSaldoCuenta * -1 WHEN pSaldoCuenta < 0.
                Mov_Contable.Cuenta = cierreCuentas.cuenta.
                Mov_Contable.Destino = 1.
                Mov_Contable.Estacion = "000005".
                Mov_Contable.Fec_Contable = w_fecha.
                Mov_Contable.Fec_Grabacion = TODAY.
                Mov_Contable.Hora = TIME.
                Mov_Contable.Nit = "800197268".
                Mov_Contable.Num_Documento = vSecuencia.
                Mov_Contable.Usuario = w_usuario.

                ASSIGN vTotal2430 = vTotal2430 + pSaldoCuenta WHEN SUBSTRING(cierreCuentas.cuenta,1,4) = "2430".
                ASSIGN vTotal2435 = vTotal2435 + pSaldoCuenta WHEN SUBSTRING(cierreCuentas.cuenta,1,4) = "2435".
            END.
        END.

        IF vTotal2430 <> 0 THEN DO:
            CREATE mov_contable.
            Mov_Contable.agencia = agencias.agencia.
            Mov_Contable.Cen_Costos = 999.
            Mov_Contable.Comentario = "Traslado automático".
            Mov_Contable.Comprobante = comprobantes.comprobante.
            ASSIGN Mov_Contable.cr = vTotal2430 WHEN vTotal2430 > 0.
            ASSIGN Mov_Contable.db = vTotal2430 * -1 WHEN vTotal2430 < 0.
            Mov_Contable.Cuenta = "27059501".
            Mov_Contable.Destino = 1.
            Mov_Contable.Estacion = "000005".
            Mov_Contable.Fec_Contable = w_fecha.
            Mov_Contable.Fec_Grabacion = TODAY.
            Mov_Contable.Hora = TIME.
            Mov_Contable.Nit = "001".
            Mov_Contable.Num_Documento = vSecuencia.
            Mov_Contable.Usuario = w_usuario.

            CREATE mov_contable.
            Mov_Contable.agencia = 1.
            Mov_Contable.Cen_Costos = 999.
            Mov_Contable.Comentario = "Traslado automático".
            Mov_Contable.Comprobante = comprobantes.comprobante.
            ASSIGN Mov_Contable.Db = vTotal2430 WHEN vTotal2430 > 0.
            ASSIGN Mov_Contable.Cr = vTotal2430 * -1 WHEN vTotal2430 < 0.
            Mov_Contable.Cuenta = "27059501".
            Mov_Contable.Destino = 1.
            Mov_Contable.Estacion = "000005".
            Mov_Contable.Fec_Contable = w_fecha.
            Mov_Contable.Fec_Grabacion = TODAY.
            Mov_Contable.Hora = TIME.
            Mov_Contable.Nit = STRING(agencias.agencia,"999").
            Mov_Contable.Num_Documento = vSecuencia.
            Mov_Contable.Usuario = w_usuario.

            CREATE mov_contable.
            Mov_Contable.agencia = 1.
            Mov_Contable.Cen_Costos = 999.
            Mov_Contable.Comentario = "Traslado automático".
            Mov_Contable.Comprobante = comprobantes.comprobante.
            ASSIGN Mov_Contable.cr = vTotal2430 WHEN vTotal2430 > 0.
            ASSIGN Mov_Contable.db = vTotal2430 * -1 WHEN vTotal2430 < 0.
            Mov_Contable.Cuenta = cierreCuentas.cuenta.
            Mov_Contable.Destino = 1.
            Mov_Contable.Estacion = "000005".
            Mov_Contable.Fec_Contable = w_fecha.
            Mov_Contable.Fec_Grabacion = TODAY.
            Mov_Contable.Hora = TIME.
            Mov_Contable.Nit = "800197268".
            Mov_Contable.Num_Documento = vSecuencia.
            Mov_Contable.Usuario = w_usuario.
        END.

        IF vTotal2435 <> 0 THEN DO:
            CREATE mov_contable.
            Mov_Contable.agencia = agencias.agencia.
            Mov_Contable.Cen_Costos = 999.
            Mov_Contable.Comentario = "Traslado automático".
            Mov_Contable.Comprobante = comprobantes.comprobante.
            ASSIGN Mov_Contable.cr = vTotal2435 WHEN vTotal2435 > 0.
            ASSIGN Mov_Contable.db = vTotal2435 * -1 WHEN vTotal2435 < 0.
            Mov_Contable.Cuenta = "27054501".
            Mov_Contable.Destino = 1.
            Mov_Contable.Estacion = "000005".
            Mov_Contable.Fec_Contable = w_fecha.
            Mov_Contable.Fec_Grabacion = TODAY.
            Mov_Contable.Hora = TIME.
            Mov_Contable.Nit = "001".
            Mov_Contable.Num_Documento = vSecuencia.
            Mov_Contable.Usuario = w_usuario.

            CREATE mov_contable.
            Mov_Contable.agencia = 1.
            Mov_Contable.Cen_Costos = 999.
            Mov_Contable.Comentario = "Traslado automático".
            Mov_Contable.Comprobante = comprobantes.comprobante.
            ASSIGN Mov_Contable.Db = vTotal2435 WHEN vTotal2435 > 0.
            ASSIGN Mov_Contable.Cr = vTotal2435 * -1 WHEN vTotal2435 < 0.
            Mov_Contable.Cuenta = "27054501".
            Mov_Contable.Destino = 1.
            Mov_Contable.Estacion = "000005".
            Mov_Contable.Fec_Contable = w_fecha.
            Mov_Contable.Fec_Grabacion = TODAY.
            Mov_Contable.Hora = TIME.
            Mov_Contable.Nit = STRING(agencias.agencia,"999").
            Mov_Contable.Num_Documento = vSecuencia.
            Mov_Contable.Usuario = w_usuario.

            CREATE mov_contable.
            Mov_Contable.agencia = 1.
            Mov_Contable.Cen_Costos = 999.
            Mov_Contable.Comentario = "Traslado automático".
            Mov_Contable.Comprobante = comprobantes.comprobante.
            ASSIGN Mov_Contable.cr = vTotal2435 WHEN vTotal2435 > 0.
            ASSIGN Mov_Contable.db = vTotal2435 * -1 WHEN vTotal2435 < 0.
            Mov_Contable.Cuenta = cierreCuentas.cuenta.
            Mov_Contable.Destino = 1.
            Mov_Contable.Estacion = "000005".
            Mov_Contable.Fec_Contable = w_fecha.
            Mov_Contable.Fec_Grabacion = TODAY.
            Mov_Contable.Hora = TIME.
            Mov_Contable.Nit = "800197268".
            Mov_Contable.Num_Documento = vSecuencia.
            Mov_Contable.Usuario = w_usuario.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

