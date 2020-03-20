&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_ActivosFijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_ActivosFijos 
CREATE WIDGET-POOL.

/* oakley */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-120 RECT-314 Btn_Procesar W_CmbOfi ~
Btn_Done 
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
DEFINE VAR W-Prc_ActivosFijos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.81
     BGCOLOR 8 .

DEFINE BUTTON Btn_Procesar 
     LABEL "&Procesar" 
     SIZE 10 BY 1.88 TOOLTIP "Realiza el Proceso de Cierre".

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
     SIZE 47.57 BY 2.69.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 4.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     Btn_Procesar AT ROW 1.54 COL 51 HELP
          "Permite Realizar el Proceso de Cierre"
     W_CmbOfi AT ROW 1.69 COL 12 COLON-ALIGNED
     W_DiaC AT ROW 2.69 COL 11.57 COLON-ALIGNED
     W_MesC AT ROW 2.69 COL 19.57 COLON-ALIGNED
     W_Nmes AT ROW 2.69 COL 22.86 COLON-ALIGNED NO-LABEL
     W_AnoC AT ROW 2.69 COL 41 COLON-ALIGNED
     Btn_Done AT ROW 3.69 COL 51 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_mensaje AT ROW 4.73 COL 1 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 4.73 COL 35.14 COLON-ALIGNED NO-LABEL
     "RegistrosTotales" VIEW-AS TEXT
          SIZE 11.43 BY .5 AT ROW 4.19 COL 37.14
          BGCOLOR 1 
     RECT-120 AT ROW 1.27 COL 2
     RECT-314 AT ROW 1.27 COL 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.57 BY 4.85
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
  CREATE WINDOW W-Prc_ActivosFijos ASSIGN
         HIDDEN             = YES
         TITLE              = "Depreciación de Activos Fijos"
         HEIGHT             = 4.85
         WIDTH              = 62.57
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
/* SETTINGS FOR WINDOW W-Prc_ActivosFijos
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_ActivosFijos)
THEN W-Prc_ActivosFijos:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Prc_ActivosFijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_ActivosFijos W-Prc_ActivosFijos
ON END-ERROR OF W-Prc_ActivosFijos /* Depreciación de Activos Fijos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_ActivosFijos W-Prc_ActivosFijos
ON WINDOW-CLOSE OF W-Prc_ActivosFijos /* Depreciación de Activos Fijos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Prc_ActivosFijos
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Procesar W-Prc_ActivosFijos
ON CHOOSE OF Btn_Procesar IN FRAME F_Proc /* Procesar */
DO:
    RUN _SetCurs.r("WAIT").

    RUN Proceso NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        W_Mensaje:SCREEN-VALUE = "El Proceso tiene errores...Verifique por favor.".
        RUN _SetCurs.r("ARROW").
        RETURN.
    END.
    ELSE DO:
        MESSAGE "El proceso de depreciación y valorización" SKIP
                "de Activos Fijos ha sido realizado de forma" SKIP
                "exitosa."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        W_Mensaje:SCREEN-VALUE = "Generado el proceso de depreciación de activos...!".
    END.

    RUN _SetCurs.r("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Prc_ActivosFijos
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
    IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,2)) = 0 THEN DO:
        ASSIGN w_ofiIni = 0
               w_ofiFin = 999.
    END.
    ELSE
        ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,2)) 
               W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,2)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Prc_ActivosFijos 


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

    W_CmbOfi:ADD-LAST("00 - CONSOLIDADO").

    ASSIGN W_OfiIni = W_Agencia
           W_OfiFin = W_Agencia
           W_MesC:SCREEN-VALUE IN FRAME F_Proc = STRING(MONTH(W_Fecha))
           W_DiaC:SCREEN-VALUE = STRING(DAY(W_Fecha))
           W_AnoC:SCREEN-VALUE = STRING(YEAR(W_Fecha)).

    FOR EACH Agencias WHERE Agencias.Estado <> 3 NO-LOCK:
        W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"99") + " - " + STRING(Agencias.Nombre,"X(25)")).

        IF W_OfiCierre <> 0 AND Agencias.Agencia EQ W_OfiCierre THEN
            W_CmbOfi:SCREEN-VALUE = STRING(Agencias.Agencia,"99") + " - " + STRING(Agencias.Nombre,"X(25)").
    END.

    IF W_OfiCierre = 0 THEN
        W_CmbOfi:SCREEN-VALUE = "00 - CONSOLIDADO".

    APPLY "VALUE-CHANGED" TO W_CmbOfi.

    CASE MONTH(W_Fecha):
        WHEN 1 THEN W_NMes = "Enero".
        WHEN 2 THEN W_NMes = "Febrero".
        WHEN 3 THEN W_NMes = "Marzo".
        WHEN 4 THEN W_NMes = "Abril".
        WHEN 5 THEN W_NMes = "Mayo".
        WHEN 6 THEN W_NMes = "Junio".
        WHEN 7 THEN W_NMes = "Julio".
        WHEN 8 THEN W_NMes = "Agosto".
        WHEN 9 THEN W_NMes = "Septiembre".
        WHEN 10 THEN W_NMes = "Octubre".
        WHEN 11 THEN W_NMes = "Noviembre".
        WHEN 12 THEN W_NMes = "Diciembre".
    END CASE.

    W_NMes:SCREEN-VALUE = W_NMes.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Depreciacion W-Prc_ActivosFijos 
PROCEDURE Depreciacion :
DEFINE VAR valDepreciar AS DECIMAL.
DEFINE VAR flagContabilizo AS LOGICAL INITIAL FALSE.

FIND FIRST comprobantes WHERE comprobantes.agencia = agencias.agencia
                          AND comprobantes.comprobante = 10 NO-ERROR.

comprobantes.secuencia = comprobantes.secuencia + 1.

FOR EACH activosFijos WHERE activosFijos.agencia = agencias.agencia
                        AND activosFijos.valorActual > 0
                        /*AND activosFijos.mesesDepreciar > 0*/
                        AND activosFijos.estado = 1
                        AND activosFijos.contabilizado = TRUE
                        AND activosFijos.depreciable = YES:
    FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_activosFijos THEN DO:
        flagContabilizo = TRUE.

        IF activosFijos.mesesDepreciar > 0 THEN
            valDepreciar = ROUND(activosFijos.valorActual / activosFijos.mesesDepreciar,2).
        ELSE
            valDepreciar = activosFijos.valorActual.

        CREATE mov_contable.
        mov_contable.agencia = activosFijos.agencia.
        mov_contable.Cen_Costos = activosFijos.cen_costo.
        mov_contable.Comentario = "Proceso Depreciación".
        mov_contable.Comprobante = comprobantes.comprobante.
        mov_contable.db = valDepreciar.
        mov_contable.Cuenta = cfg_activosFijos.gasto.
        mov_contable.Fec_Contable = w_fecha.
        mov_contable.Fec_Grabacion = TODAY.
        mov_contable.Hora = TIME.
        mov_contable.Nit = activosFijos.idActivo.
        mov_contable.Num_Documento = comprobantes.secuencia.
        mov_contable.Usuario = w_usuario.

        CREATE mov_contable.
        mov_contable.agencia = activosFijos.agencia.
        mov_contable.Cen_Costos = activosFijos.cen_costo.
        mov_contable.Comentario = "Proceso Depreciación".
        mov_contable.Comprobante = comprobantes.comprobante.
        mov_contable.cr = valDepreciar.
        mov_contable.Cuenta = cfg_activosFijos.depreciacion.
        mov_contable.Fec_Contable = w_fecha.
        mov_contable.Fec_Grabacion = TODAY.
        mov_contable.Hora = TIME.
        mov_contable.Nit = activosFijos.idActivo.
        mov_contable.Num_Documento = comprobantes.secuencia.
        mov_contable.Usuario = w_usuario.

        activosFijos.valorActual = activosFijos.valorActual - valDepreciar.
        activosFijos.valorDepreciado = activosFijos.valorDepreciado + valDepreciar.
        ASSIGN activosFijos.mesesDepreciar = activosFijos.mesesDepreciar - 1 WHEN activosFijos.mesesDepreciar >= 1.
        activosFijos.fecUltDepreciacion = TODAY.
        activosFijos.anotacion = activosFijos.anotacion + "Última Depreciación: " + STRING(w_fecha,"99/99/9999") + " - " + STRING(valDepreciar,"$zzz,zzz,zz9.99") + " - ".
    END.
END.

IF flagContabilizo = FALSE THEN
    comprobantes.secuencia = comprobantes.secuencia - 1.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Prc_ActivosFijos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_ActivosFijos)
  THEN DELETE WIDGET W-Prc_ActivosFijos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Prc_ActivosFijos  _DEFAULT-ENABLE
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
      WITH FRAME F_Proc IN WINDOW W-Prc_ActivosFijos.
  ENABLE RECT-120 RECT-314 Btn_Procesar W_CmbOfi Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Prc_ActivosFijos.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Prc_ActivosFijos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W-Prc_ActivosFijos 
PROCEDURE Proceso :
DEFI VAR W_FecCorte LIKE W_Fecha.
DEFI VAR K AS INTEG FORM "9".
DEFI VAR id AS LOGICAL INITIAL NO.

/* Validaciones */
FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                    AND Agencias.Agencia <= W_OfiFin NO-LOCK:
    FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                         AND MONTH(ProcDia.Fecha_Proc) = MONTH(W_Fecha)
                         AND YEAR(procDia.fecha_proc) = YEAR(w_fecha)
                         AND ProcDia.Cod_Proceso = 10
                         AND ProcDia.Estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(ProcDia) THEN DO:
        MESSAGE "Este proceso ya fue ejecutado para este mes en la agencia" agencias.nombre SKIP
                "o no se encuentra matriculado. Revise por favor..." SKIP
            VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".

        RETURN ERROR.
    END.
END.
/* ----- */

DO TRANSACTION ON ERROR UNDO:
    FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin NO-LOCK:
        W_Mensaje:SCREEN-VALUE IN FRAME F_Proc = "Realizando proceso de depreciación de Activos Fijos...".

        RUN Depreciacion.
        RUN Valorizacion.

        FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                             AND MONTH(ProcDia.Fecha_Proc) = MONTH(W_Fecha)
                             AND YEAR(procDia.fecha_proc) = YEAR(w_fecha)
                             AND ProcDia.Cod_Proceso EQ 10
                             AND ProcDia.Estado EQ 1 NO-ERROR.
        ProcDia.Estado = 2.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valorizacion W-Prc_ActivosFijos 
PROCEDURE Valorizacion :
DEFINE VAR flagContabilizo AS LOGICAL INITIAL FALSE.
DEFINE VAR oldValorizacion AS DECIMAL.
DEFINE VAR newValorizacion AS DECIMAL.
DEFINE VAR ajuste AS DECIMAL.

FIND FIRST comprobantes WHERE comprobantes.agencia = agencias.agencia
                          AND comprobantes.comprobante = 10 NO-ERROR.

comprobantes.secuencia = comprobantes.secuencia + 1.

FOR EACH activosFijos WHERE activosFijos.agencia = agencias.agencia
                        AND activosFijos.avaluo > 0
                        AND activosFijos.valorizacion <> activosFijos.avaluo - activosFijos.valorActual
                        AND activosFijos.estado = 1
                        AND activosFijos.contabilizado = TRUE:
    FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_activosFijos THEN DO:
        flagContabilizo = TRUE.

        oldValorizacion = activosFijos.valorizacion.
        newValorizacion = activosFijos.avaluo - activosFijos.valorActual.
        ajuste = newValorizacion - oldValorizacion.

        FIND FIRST cfg_activosFijos WHERE cfg_ActivosFijos.tipoActivo = activosFijos.tipoActivo NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_activosFijos THEN DO:
            /*IF cfg_ActivosFijos.valorizacionDB <> "" AND cfg_activosFijos.valorizacionDB <> ? AND cfg_ActivosFijos.valorizacionCR <> "" AND cfg_activosFijos.valorizacionCR <> ? THEN DO:
                activosFijos.valorizacion = newValorizacion.
                activosFijos.anotacion = activosFijos.anotacion + " / " + "Última Valorización: " + STRING(w_fecha,"99/99/9999") + " --> " + STRING(newValorizacion,"$->>>,>>>,>>>,>>9.99").

                CREATE mov_contable.
                mov_contable.agencia = activosFijos.agencia.
                mov_contable.Cen_Costos = activosFijos.cen_costo.
                mov_contable.Comentario = "Proceso Valorización".
                mov_contable.Comprobante = comprobantes.comprobante.
                mov_contable.db = ajuste.
                mov_contable.Cuenta = cfg_activosFijos.valorizacionDB.
                mov_contable.Fec_Contable = w_fecha.
                mov_contable.Fec_Grabacion = TODAY.
                mov_contable.Hora = TIME.
                mov_contable.Nit = activosFijos.idActivo.
                mov_contable.Num_Documento = comprobantes.secuencia.
                mov_contable.Usuario = w_usuario.

                CREATE mov_contable.
                mov_contable.agencia = activosFijos.agencia.
                mov_contable.Cen_Costos = activosFijos.cen_costo.
                mov_contable.Comentario = "Proceso Valorización".
                mov_contable.Comprobante = comprobantes.comprobante.
                mov_contable.cr = ajuste.
                mov_contable.Cuenta = cfg_activosFijos.valorizacionCR.
                mov_contable.Fec_Contable = w_fecha.
                mov_contable.Fec_Grabacion = TODAY.
                mov_contable.Hora = TIME.
                mov_contable.Nit = activosFijos.idActivo.
                mov_contable.Num_Documento = comprobantes.secuencia.
                mov_contable.Usuario = w_usuario.
            END.
            ELSE
                MESSAGE "No se encuentra la cuenta contable para la valorización del activo" activosFijos.nombre
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        END.
    END.
END.

IF flagContabilizo = FALSE THEN
    comprobantes.secuencia = comprobantes.secuencia - 1.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

