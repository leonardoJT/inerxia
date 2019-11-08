&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE VAR flag1 AS LOG.    /* Variable que dice si existen registros principales por replicar */

DEFINE TEMP-TABLE ttMovAhorros LIKE BD_WEB.mov_ahorros.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME MainFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cont BtnDone 
&Scoped-Define DISPLAYED-OBJECTS cont 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Exit" 
     SIZE 29 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE cont AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME MainFrame
     cont AT ROW 3.58 COL 9 COLON-ALIGNED NO-LABEL
     BtnDone AT ROW 4.81 COL 6
     "Registros replicados:" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.92 COL 11.14
     "REPLICACIÓN" VIEW-AS TEXT
          SIZE 17 BY .96 AT ROW 1.46 COL 13
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.8 BY 15.38.


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
         TITLE              = "Replicación Web - Fodun"
         HEIGHT             = 5.58
         WIDTH              = 40
         MAX-HEIGHT         = 25.92
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 25.92
         VIRTUAL-WIDTH      = 160
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
/* SETTINGS FOR FRAME MainFrame
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Replicación Web - Fodun */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON ENTRY OF C-Win /* Replicación Web - Fodun */
DO:
    ASSIGN cont = 0.

    /* Se desabilitan los triggers para las tablas a replicar. */
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.agencias.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Ahorros.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Clientes.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Creditos.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.habiles.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Mov_Ahorros.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Mov_creditos.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Pro_Ahorros.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.Pro_creditos.
    DISABLE TRIGGERS FOR LOAD OF BD_WEB.ubicacion.

    /* Borramos el movimiento anterior a 6 meses */
    FOR EACH BD_WEB.mov_creditos WHERE BD_WEB.mov_creditos.fecha <= ADD-INTERVAL(TODAY,-6,"months"):
        DELETE BD_WEB.mov_creditos.
    END.

    FOR EACH BD_WEB.mov_ahorros WHERE BD_WEB.mov_ahorros.fecha <= ADD-INTERVAL(TODAY,-6,"months"):
        DELETE BD_WEB.mov_ahorros.
    END.

    /* Se actualiza la tabla de Hábiles */
    FOR EACH BD_WEB.habiles:
        DELETE BD_WEB.habiles.
    END.

    FOR EACH bdcentral.habiles NO-LOCK:
        CREATE BD_WEB.habiles.
        BUFFER-COPY bdcentral.habiles TO BD_WEB.habiles.
    END.
    
    /* Se revisa toda la tabla de replicación y se van pasando uno a uno los registros que no se hayan transferido a la BD de WEB. */
    REPEAT:
        cont:SCREEN-VALUE IN FRAME MainFrame = STRING(cont).

        /* Desconecta y cierra la base de datos a las 11:30 p.m. */
        IF TIME > 84600 THEN DO:
            DISCONNECT bdcentral.
            DISCONNECT bd_web.
            QUIT.
        END.

        /* Borra los registros ya replicados de la tabla replication */
        IF cont >= 3000 THEN DO:
            FOR EACH bdcentral.replication WHERE bdcentral.replication.replicado = YES:
                DELETE bdcentral.replication.
            END.

            FOR EACH bd_web.clientes WHERE bd_web.clientes.estado <> 1:
                DELETE bd_web.clientes.
            END.

            FOR EACH bd_web.ahorros WHERE bd_web.ahorros.sdo_disponible = 0 AND bd_web.ahorros.estado = 2:
                DELETE bd_web.ahorros.
            END.

            FOR EACH bd_web.creditos WHERE bd_web.creditos.sdo_capital = 0 AND bd_web.creditos.estado <> 2:
                DELETE bd_web.creditos.
            END.

            cont = 0.
        END.

        /* Comienza la replicación */
        FOR EACH bdcentral.Replication WHERE bdcentral.Replication.Replicado = NO BY entry-id:
            IF TIME > 84600 OR cont >= 3000 THEN
                LEAVE. /* Valido las condiciones de salida y de borrado de los registros ya replicados */

            CASE bdcentral.Replication.table-name:
                WHEN "Agencias" THEN RUN ReplicarAgencias.
                WHEN "Ahorros" THEN RUN ReplicarAhorros.
                WHEN "Clientes" THEN RUN ReplicarClientes.
                WHEN "Creditos" THEN RUN ReplicarCreditos.
                WHEN "Mov_Ahorros" THEN RUN ReplicarMovAhorros.
                WHEN "Mov_creditos" THEN RUN ReplicarMovCreditos.
                WHEN "Pro_Ahorros" THEN RUN ReplicarProAhorros.
                WHEN "Pro_creditos" THEN RUN ReplicarProCreditos.
                WHEN "Ubicacion" THEN RUN ReplicarUbicacion.
            END CASE.

            ASSIGN cont = cont + 1.
            cont:SCREEN-VALUE IN FRAME MainFrame = STRING(cont).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Replicación Web - Fodun */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MainFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MainFrame C-Win
ON END-ERROR OF FRAME MainFrame
DO:
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME MainFrame /* Exit */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cont C-Win
ON END-ERROR OF cont IN FRAME MainFrame
DO:
  QUIT.
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
  DISPLAY cont 
      WITH FRAME MainFrame IN WINDOW C-Win.
  ENABLE cont BtnDone 
      WITH FRAME MainFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-MainFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarAgencias C-Win 
PROCEDURE ReplicarAgencias :
FIND FIRST BD_WEB.agencias WHERE BD_WEB.agencias.agencia = bdcentral.Replication.key-1 NO-ERROR.
IF AVAILABLE BD_WEB.agencias THEN
    DELETE BD_WEB.agencias.

CREATE BD_WEB.agencias.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.agencias.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarAhorros C-Win 
PROCEDURE ReplicarAhorros :
FIND FIRST BD_WEB.Ahorros WHERE BD_WEB.Ahorros.agencia = bdcentral.Replication.key-1
                            AND BD_WEB.Ahorros.Cod_ahorro = bdcentral.Replication.key-3
                            AND BD_WEB.Ahorros.Cue_Ahorros = bdcentral.Replication.key-4 NO-ERROR.
IF AVAILABLE BD_WEB.Ahorros THEN
    DELETE BD_WEB.Ahorros.

CREATE BD_WEB.Ahorros.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Ahorros.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarAtrasos C-Win 
PROCEDURE ReplicarAtrasos :
/*FIND FIRST WEB.Atrasos WHERE WEB.Atrasos.Oficina            = Datos.Replication.key-1
                                  AND WEB.Atrasos.Cod_producto       = Datos.Replication.key-3
                                  AND WEB.Atrasos.Pagare             = Datos.Replication.key-2
                                  AND STRING(WEB.Atrasos.Fecha)      = Datos.Replication.key-4
                                  AND STRING(WEB.Atrasos.Fec_atraso) = Datos.Replication.key-5
                                  AND STRING(WEB.Atrasos.Nro_Cuota)  = Datos.Replication.key-6 NO-ERROR.
IF AVAILABLE WEB.Atrasos THEN DELETE WEB.Atrasos.
CREATE WEB.Atrasos.
RAW-TRANSFER Datos.Replication.record TO WEB.Atrasos.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarAuxilios C-Win 
PROCEDURE ReplicarAuxilios :
/*FIND FIRST WEB.Auxilios WHERE WEB.Auxilios.Oficina     = Datos.Replication.key-1
                                   AND WEB.Auxilios.Cod_auxilio = Datos.Replication.key-3 NO-ERROR.
IF AVAILABLE WEB.Auxilios THEN DELETE WEB.Auxilios.
CREATE WEB.Auxilios.
RAW-TRANSFER Datos.Replication.record TO WEB.Auxilios.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarBancos C-Win 
PROCEDURE ReplicarBancos :
/*FIND FIRST WEB.Bancos WHERE WEB.Bancos.Cod_compensa = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Bancos THEN DELETE WEB.Bancos.
CREATE WEB.Bancos.
RAW-TRANSFER Datos.Replication.record TO WEB.Bancos.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarBaseRet C-Win 
PROCEDURE ReplicarBaseRet :
/*FIND FIRST WEB.Base_Ret WHERE WEB.Base_Ret.Cod_Base           = Datos.Replication.key-2
                                   AND WEB.Base_Ret.Nombre             = Datos.Replication.key-4
                                   AND STRING(WEB.Base_Ret.Porcentaje) = Datos.Replication.key-5
                                   AND STRING(WEB.Base_Ret.Fecha)      = Datos.Replication.key-6
                                   AND WEB.Base_Ret.Estado             = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Base_Ret THEN DELETE WEB.Base_Ret.
CREATE WEB.Base_Ret.
RAW-TRANSFER Datos.Replication.record TO WEB.Base_Ret.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarBenAutAho C-Win 
PROCEDURE ReplicarBenAutAho :
/*FIND FIRST WEB.BenAut_Aho WHERE WEB.BenAut_Aho.Oficina      = Datos.Replication.key-1
                                     AND WEB.BenAut_Aho.Cod_Producto = Datos.Replication.key-3
                                     AND WEB.BenAut_Aho.Cue_Ahorros  = Datos.Replication.key-2
                                     AND WEB.BenAut_Aho.Cedula       = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.BenAut_Aho THEN DELETE WEB.BenAut_Aho.
CREATE WEB.BenAut_Aho.
RAW-TRANSFER Datos.Replication.record TO WEB.BenAut_Aho.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarBienes C-Win 
PROCEDURE ReplicarBienes :
/*FIND FIRST WEB.Bienes WHERE WEB.Bienes.Nit       = Datos.Replication.key-2
                                 AND WEB.Bienes.Secuencia = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Bienes THEN DELETE WEB.Bienes.
CREATE WEB.Bienes.
RAW-TRANSFER Datos.Replication.record TO WEB.Bienes.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCalendario C-Win 
PROCEDURE ReplicarCalendario :
/*FIND FIRST WEB.Calendario WHERE WEB.Calendario.Oficina     = Datos.Replication.key-1
                                     AND WEB.Calendario.Ano         = Datos.Replication.key-3
                                     AND STRING(WEB.Calendario.Mes) = Datos.Replication.key-2
                                     AND STRING(WEB.Calendario.Dia) = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.Calendario THEN DELETE WEB.Calendario.
CREATE WEB.Calendario.
RAW-TRANSFER Datos.Replication.record TO WEB.Calendario.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCapacitacion C-Win 
PROCEDURE ReplicarCapacitacion :
/*FIND FIRST WEB.Capacitacion WHERE WEB.Capacitacion.Nit       = Datos.Replication.key-2
                                       AND WEB.Capacitacion.Secuencia = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Capacitacion THEN DELETE WEB.Capacitacion.
CREATE WEB.Capacitacion.
RAW-TRANSFER Datos.Replication.record TO WEB.Capacitacion.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCarteraVencida C-Win 
PROCEDURE ReplicarCarteraVencida :
/*FIND FIRST WEB.CarteraVencida WHERE WEB.CarteraVencida.Cod_Producto         = Datos.Replication.key-1
                                         AND WEB.CarteraVencida.Per_inicial          = Datos.Replication.key-3
                                         AND STRING(WEB.CarteraVencida.Per_final)    = Datos.Replication.key-2
                                         AND STRING(WEB.CarteraVencida.Cod_califica) = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.CarteraVencida THEN DELETE WEB.CarteraVencida.
CREATE WEB.CarteraVencida.
RAW-TRANSFER Datos.Replication.record TO WEB.CarteraVencida.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCausaInte C-Win 
PROCEDURE ReplicarCausaInte :
/*FIND FIRST WEB.Causa_Inte WHERE WEB.Causa_Inte.Clase_Producto         = Datos.Replication.key-1
                                     AND WEB.Causa_Inte.Cod_Producto           = Datos.Replication.key-3 NO-ERROR.
IF AVAILABLE WEB.Causa_Inte THEN DELETE WEB.Causa_Inte.
CREATE WEB.Causa_Inte.
RAW-TRANSFER Datos.Replication.record TO WEB.Causa_Inte.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCenCostos C-Win 
PROCEDURE ReplicarCenCostos :
/*FIND FIRST WEB.Cen_Costos WHERE WEB.Cen_Costos.Oficina    = Datos.Replication.key-1
                                     AND WEB.Cen_Costos.Cen_Costos = Datos.Replication.key-3 NO-ERROR.
IF AVAILABLE WEB.Cen_Costos THEN DELETE WEB.Cen_Costos.
CREATE WEB.Cen_Costos.
RAW-TRANSFER Datos.Replication.record TO WEB.Cen_Costos.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCfgActivo C-Win 
PROCEDURE ReplicarCfgActivo :
/*FIND FIRST WEB.Cfg_Activo WHERE WEB.Cfg_Activo.Clase = Datos.Replication.key-2
                                     AND WEB.Cfg_Activo.Grupo = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Cfg_Activo THEN DELETE WEB.Cfg_Activo.
CREATE WEB.Cfg_Activo.
RAW-TRANSFER Datos.Replication.record TO WEB.Cfg_Activo.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCfgCieAnual C-Win 
PROCEDURE ReplicarCfgCieAnual :
/*FIND FIRST WEB.Cfg_CieAnual WHERE WEB.Cfg_CieAnual.CuentaIni   = Datos.Replication.key-2
                                       AND WEB.Cfg_CieAnual.CuentaFinal = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.Cfg_CieAnual THEN DELETE WEB.Cfg_CieAnual.
CREATE WEB.Cfg_CieAnual.
RAW-TRANSFER Datos.Replication.record TO WEB.Cfg_CieAnual.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCfgCtasDifer C-Win 
PROCEDURE ReplicarCfgCtasDifer :
/*FIND FIRST WEB.Cfg_CtasDifer WHERE WEB.Cfg_CtasDifer.Clase = Datos.Replication.key-2
                                        AND WEB.Cfg_CtasDifer.Grupo = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Cfg_CtasDifer THEN DELETE WEB.Cfg_CtasDifer.
CREATE WEB.Cfg_CtasDifer.
RAW-TRANSFER Datos.Replication.record TO WEB.Cfg_CtasDifer.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCfgScor C-Win 
PROCEDURE ReplicarCfgScor :
/*FIND FIRST WEB.Cfg_Scor WHERE WEB.Cfg_Scor.Tipo_Var = Datos.Replication.key-2
                                   AND WEB.Cfg_Scor.Variable = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.Cfg_Scor THEN DELETE WEB.Cfg_Scor.
CREATE WEB.Cfg_Scor.
RAW-TRANSFER Datos.Replication.record TO WEB.Cfg_Scor.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCfgVarios C-Win 
PROCEDURE ReplicarCfgVarios :
/*FIND FIRST WEB.Cfg_Varios WHERE WEB.Cfg_Varios.Tipo = Datos.Replication.key-2 NO-ERROR.
IF AVAILABLE WEB.Cfg_Varios THEN DELETE WEB.Cfg_Varios.
CREATE WEB.Cfg_Varios.
RAW-TRANSFER Datos.Replication.record TO WEB.Cfg_Varios.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCheBlo C-Win 
PROCEDURE ReplicarCheBlo :
/*FIND FIRST WEB.Che_Blo WHERE WEB.Che_Blo.Oficina               = Datos.Replication.key-1
                                  AND WEB.Che_Blo.Cod_Producto          = Datos.Replication.key-3
                                  AND WEB.Che_Blo.Cue_ahorros           = Datos.Replication.key-2
                                  AND STRING(WEB.Che_Blo.Numero_Cheque) = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.Che_Blo THEN DELETE WEB.Che_Blo.
CREATE WEB.Che_Blo.
RAW-TRANSFER Datos.Replication.record TO WEB.Che_Blo.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCheTransito C-Win 
PROCEDURE ReplicarCheTransito :
/*FIND FIRST WEB.Che_Transito WHERE WEB.Che_Transito.Oficina              = Datos.Replication.key-1
                                       AND WEB.Che_Transito.Cod_Compensa         = Datos.Replication.key-3
                                       AND WEB.Che_Transito.Cheque               = Datos.Replication.key-2
                                       AND STRING(WEB.Che_Transito.Cod_Producto) = Datos.Replication.key-6
                                       AND WEB.Che_Transito.Tip_Producto         = Datos.Replication.key-4
                                       AND WEB.Che_Transito.Num_Cuenta           = Datos.Replication.key-5 NO-ERROR.
IF AVAILABLE WEB.Che_Transito THEN DELETE WEB.Che_Transito.
CREATE WEB.Che_Transito.
RAW-TRANSFER Datos.Replication.record TO WEB.Che_Transito.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCiiu C-Win 
PROCEDURE ReplicarCiiu :
/*FIND FIRST WEB.Ciiu WHERE WEB.Ciiu.Codigo_Ciiu = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Ciiu THEN DELETE WEB.Ciiu.
CREATE WEB.Ciiu.
RAW-TRANSFER Datos.Replication.record TO WEB.Ciiu.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarClientes C-Win 
PROCEDURE ReplicarClientes :
FIND FIRST BD_WEB.Clientes WHERE BD_WEB.Clientes.Nit = bdcentral.Replication.key-2 NO-ERROR.
IF AVAILABLE BD_WEB.Clientes THEN
    DELETE BD_WEB.Clientes.

CREATE BD_WEB.Clientes.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Clientes NO-ERROR.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarComprobantes C-Win 
PROCEDURE ReplicarComprobantes :
/*FIND FIRST WEB.Comprobantes WHERE WEB.Comprobantes.Oficina = Datos.Replication.key-1
                                       AND WEB.Comprobantes.Comprobante = Datos.Replication.key-3 NO-ERROR.
IF AVAILABLE WEB.Comprobantes THEN DELETE WEB.Comprobantes.
CREATE WEB.Comprobantes.
RAW-TRANSFER Datos.Replication.record TO WEB.Comprobantes.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarConciliacion C-Win 
PROCEDURE ReplicarConciliacion :
/*FIND FIRST WEB.Conciliacion WHERE WEB.Conciliacion.Oficina       = Datos.Replication.key-1
                                       AND WEB.Conciliacion.Cuenta        = Datos.Replication.key-2
                                       AND WEB.Conciliacion.Reg_MovExtrac = Datos.Replication.key-4
                                       AND WEB.Conciliacion.Reg_CtaConta  = Datos.Replication.key-5 NO-ERROR.
IF AVAILABLE WEB.Conciliacion THEN DELETE WEB.Conciliacion.
CREATE WEB.Conciliacion.
RAW-TRANSFER Datos.Replication.record TO WEB.Conciliacion.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCortoLargo C-Win 
PROCEDURE ReplicarCortoLargo :
/*FIND FIRST WEB.CortoLargo WHERE WEB.CortoLargo.Clase_Producto = Datos.Replication.key-1
                                     AND WEB.CortoLargo.Cod_Producto = Datos.Replication.key-3
                                     AND STRING(WEB.CortoLargo.Plazo_Inicial) = Datos.Replication.key-2
                                     AND STRING(WEB.CortoLargo.Plazo_Final) = Datos.Replication.key-4 NO-ERROR.
IF AVAILABLE WEB.CortoLargo THEN DELETE WEB.CortoLargo.
CREATE WEB.CortoLargo.
RAW-TRANSFER Datos.Replication.record TO WEB.CortoLargo.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCreditos C-Win 
PROCEDURE ReplicarCreditos :
FIND FIRST BD_WEB.Creditos WHERE BD_WEB.Creditos.agencia = bdcentral.Replication.key-1
                             AND BD_WEB.Creditos.Nit = bdcentral.Replication.key-2
                             AND BD_WEB.Creditos.num_credito = bdcentral.Replication.key-3
                             AND BD_WEB.Creditos.num_solicitud = bdcentral.Replication.key-5 NO-ERROR.
IF AVAILABLE BD_WEB.Creditos THEN
    DELETE BD_WEB.Creditos.

CREATE BD_WEB.Creditos.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Creditos.
ASSIGN bdcentral.Replication.Replicado = YES.

IF bd_web.creditos.dias_atraso > 0 THEN DO:
    IF bd_web.creditos.cod_credito <> 123 THEN DO:
        bd_web.creditos.val_atraso = bd_web.creditos.INT_morCobrar + bd_web.creditos.INT_moraDifCob.

        FOR EACH bdcentral.control_pagos WHERE bdcentral.CONTROL_pagos.nit = bd_web.creditos.nit
                                           AND bdcentral.CONTROL_pagos.num_credito = bd_web.creditos.num_credito
                                           AND bdcentral.CONTROL_pagos.id_pdoMes < 2
                                           AND bdcentral.CONTROL_pagos.fec_Vcto <= TODAY + 1 NO-LOCK:
            bd_web.creditos.val_atraso = bd_web.creditos.val_atraso + bdcentral.CONTROL_pagos.cuota - bdcentral.CONTROL_pagos.cap_pagado.
        END.
    END.
    ELSE DO:
        bd_web.creditos.val_atraso = 0.

        FOR EACH bdcentral.facturacion WHERE bdcentral.facturacion.nit = bd_web.creditos.nit
                                         AND bdcentral.facturacion.num_credito = bd_web.creditos.num_credito
                                         AND bdcentral.facturacion.fec_pago <= TODAY + 1
                                         AND bdcentral.facturacion.estado = 1 NO-LOCK BY bdcentral.facturacion.fec_pago DESCENDING:
            bd_web.creditos.val_atraso = bd_web.creditos.val_atraso + (bdcentral.facturacion.cuota - bdcentral.Facturacion.pago_capital - bdcentral.Facturacion.pago_intCorriente - bdcentral.Facturacion.pago_intDifCobro - bdcentral.Facturacion.pago_mora).
        END.

        IF bd_web.creditos.val_atraso < 0 THEN
            bd_web.creditos.val_atraso = 0.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarCtrProcesos C-Win 
PROCEDURE ReplicarCtrProcesos :
/*FIND FIRST WEB.Ctr_Procesos WHERE WEB.Ctr_Procesos.Oficina             = Datos.Replication.key-1
                                       AND WEB.Ctr_Procesos.Tip_proceso         = Datos.Replication.key-3
                                       AND STRING(WEB.Ctr_Procesos.Fec_proceso) = Datos.Replication.key-2 NO-ERROR.
IF AVAILABLE WEB.Ctr_Procesos THEN DELETE WEB.Ctr_Procesos.
CREATE WEB.Ctr_Procesos.
RAW-TRANSFER Datos.Replication.record TO WEB.Ctr_Procesos.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarEspeciales C-Win 
PROCEDURE ReplicarEspeciales :
/*FIND FIRST WEB.Especiales WHERE WEB.Especiales.Oficina      = Datos.Replication.key-1
                                     AND WEB.Especiales.Cod_Producto = Datos.Replication.key-3
                                     AND WEB.Especiales.Nit          = Datos.Replication.key-2 NO-ERROR.
IF AVAILABLE WEB.Especiales THEN DELETE WEB.Especiales.
CREATE WEB.Especiales.
RAW-TRANSFER Datos.Replication.record TO WEB.Especiales.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarGarantias C-Win 
PROCEDURE ReplicarGarantias :
/*FIND FIRST WEB.Garantias WHERE WEB.Garantias.oficina = Datos.Replication.key-1
                           AND WEB.Garantias.Cod_Producto = Datos.Replication.key-3
                           AND STRING(WEB.Garantias.num_solicitud) = Datos.Replication.key-4
                           AND STRING(WEB.Garantias.Cod_bien) = Datos.Replication.key-5
                           AND WEB.Garantias.Nit_codeudor = Datos.Replication.key-2 NO-ERROR.
IF AVAILABLE WEB.Garantias THEN DELETE WEB.Garantias.
CREATE WEB.Garantias.
RAW-TRANSFER Datos.Replication.record TO WEB.Garantias.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarHabiles C-Win 
PROCEDURE ReplicarHabiles :
FIND FIRST BD_WEB.habiles WHERE BD_WEB.habiles.nit = bdcentral.Replication.key-2 NO-ERROR.
IF AVAILABLE BD_WEB.habiles THEN
    DELETE BD_WEB.habiles.

CREATE BD_WEB.habiles.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.habiles.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarHojaDeVida C-Win 
PROCEDURE ReplicarHojaDeVida :
/*CREATE THoja_Vida.
RAW-TRANSFER Datos.Replication.record TO THoja_Vida.

FIND FIRST WEB.Hoja_Vida WHERE WEB.Hoja_Vida.Nit            = THoja_Vida.Nit
                                    AND WEB.Hoja_Vida.Clase_Asunto   = THoja_Vida.Clase_Asunto
                                    AND WEB.Hoja_Vida.Codigo_Asunto  = THoja_Vida.Codigo_Asunto
                                    AND WEB.Hoja_Vida.DoctoRefer     = THoja_Vida.DoctoRefer
                                    AND WEB.Hoja_Vida.Fec_Grabacion  = THoja_Vida.Fec_Grabacion 
                                    AND WEB.Hoja_Vida.Hora_Grabacion = THoja_Vida.Hora_Grabacion NO-ERROR.
IF AVAILABLE WEB.Hoja_Vida THEN DELETE WEB.Hoja_Vida.
CREATE WEB.Hoja_Vida.
RAW-TRANSFER Datos.Replication.record TO WEB.Hoja_Vida.
DELETE THoja_Vida.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarInfLaboral C-Win 
PROCEDURE ReplicarInfLaboral :
/*FIND FIRST WEB.Inf_Laboral WHERE WEB.Inf_Laboral.Nit = Datos.Replication.key-2
                             AND WEB.Inf_Laboral.Cod_Empresa = Datos.Replication.key-1 NO-ERROR.
IF AVAILABLE WEB.Inf_Laboral THEN
    DELETE WEB.Inf_Laboral.

CREATE WEB.Inf_Laboral.
RAW-TRANSFER Datos.Replication.record TO WEB.Inf_Laboral.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarMovAhorros C-Win 
PROCEDURE ReplicarMovAhorros :
CREATE ttMovAHorros.
RAW-TRANSFER bdcentral.Replication.record TO ttMovAhorros.

IF LENGTH(ttMovAhorros.descrip) > 50 THEN
    ttMovAhorros.descrip = SUBSTRING(ttMovAhorros.descrip,1,50).

ASSIGN bdcentral.Replication.Replicado = YES.

/* Si no es ahorro a la vista, lo descartamos */
IF ttMovAhorros.cod_ahorro <> 4 AND ttMovAhorros.cod_ahorro <> 8 THEN
    LEAVE.

IF ttMovAhorros.cod_operacion = 010101003 AND ttmovahorros.descrip = "Abono Liq.Interés" THEN DO:
    FIND FIRST BD_WEB.mov_ahorros WHERE BD_WEB.mov_ahorros.nit = ttMovAhorros.nit
                                    AND BD_WEB.mov_ahorros.cue_ahorros = ttMovAhorros.cue_ahorros
                                    AND BD_WEB.mov_ahorros.cod_operacion = 010101003
                                    AND MONTH(BD_WEB.mov_ahorros.fecha) = MONTH(ttMovAhorros.fecha)
                                    AND YEAR(BD_WEB.mov_ahorros.fecha) = YEAR(ttMovAhorros.fecha)
                                    AND BD_WEB.mov_ahorros.descrip = "Abono Liq.Interés" NO-ERROR.
    IF AVAILABLE BD_WEB.mov_ahorros THEN DO:
        ttMovAhorros.val_efectivo = ttMovAhorros.val_efectivo + BD_WEB.mov_ahorros.val_efectivo.

        DELETE BD_WEB.mov_ahorros.
    END.
END.

CREATE BD_WEB.Mov_Ahorros.
BUFFER-COPY ttmovAhorros TO BD_WEB.mov_ahorros.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarMovContable C-Win 
PROCEDURE ReplicarMovContable :
/*CREATE WEB.Mov_Contable.
RAW-TRANSFER Datos.Replication.record TO WEB.Mov_Contable.
ASSIGN Datos.Replication.Replicado = YES.*/

/*
FIND WEB.Cuentas WHERE WEB.Cuentas.cuenta = WEB.Mov_Contable.cuenta NO-LOCK.
IF WEB.Mov_Contable.cuenta <> "" THEN DO:
    REPEAT:
        FIND WEB.Sal_Cuenta WHERE WEB.Sal_Cuenta.Oficina    = WEB.Mov_Contable.Oficina
                                       AND WEB.Sal_Cuenta.Cuenta     = WEB.Mov_Contable.Cuenta
                                       AND WEB.Sal_Cuenta.Mes        = MONTH(WEB.Mov_Contable.Fec_Contable)
                                       AND WEB.Sal_Cuenta.Ano        = YEAR(WEB.Mov_Contable.Fec_Contable)
                                       AND WEB.Sal_Cuenta.Cen_Costos = WEB.Mov_Contable.Cen_Costos EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE WEB.Sal_Cuenta THEN DO:
            IF LOCKED(WEB.Sal_Cuenta) THEN NEXT.
            ELSE DO:
                CREATE WEB.Sal_cuenta.
                ASSIGN WEB.Sal_Cuenta.Oficina = WEB.Mov_Contable.Oficina
                       WEB.Sal_Cuenta.Cuenta  = WEB.Mov_Contable.Cuenta
                       WEB.Sal_Cuenta.Mes     = MONTH(WEB.Mov_Contable.Fec_Contable)
                       WEB.Sal_Cuenta.Ano     = YEAR(WEB.Mov_Contable.Fec_Contable)
                       WEB.Sal_Cuenta.Cen_Costos = WEB.Mov_Contable.Cen_Costos.
            END.
        END. /* Fin IF NOT AVAILABLE */
                                            
        IF WEB.Mov_Contable.Naturaleza = "DB" THEN WEB.Sal_Cuenta.Sal_Debito = WEB.Sal_Cuenta.Sal_Debito + WEB.Mov_Contable.Valor.
        ELSE WEB.Sal_Cuenta.Sal_Credito = WEB.Sal_Cuenta.Sal_Credito + WEB.Mov_Contable.Valor.
        
        IF WEB.Cuentas.Naturaleza = "DB" THEN
            IF WEB.Mov_Contable.Naturaleza = "DB" THEN ASSIGN WEB.Sal_cuenta.Sdo_Final = WEB.Sal_cuenta.Sdo_Final + WEB.Mov_Contable.Valor. 
            ELSE ASSIGN WEB.Sal_cuenta.Sdo_Final = WEB.Sal_cuenta.Sdo_Final - WEB.Mov_Contable.Valor.
        ELSE
            IF WEB.Mov_contable.Naturaleza = "CR" THEN ASSIGN WEB.Sal_cuenta.Sdo_Final = WEB.Sal_cuenta.Sdo_Final + WEB.Mov_Contable.Valor.
            ELSE ASSIGN WEB.Sal_cuenta.Sdo_Final = WEB.Sal_cuenta.Sdo_Final - WEB.Mov_Contable.Valor.
        RELEASE WEB.Sal_Cuenta.
        LEAVE.
    END.
END.

IF WEB.Mov_Contable.Nit <> "" THEN DO:
    REPEAT:
        FIND WEB.Anexos WHERE WEB.Anexos.Oficina    = WEB.Mov_Contable.Oficina
                                   AND WEB.Anexos.Cen_Costos = WEB.Mov_Contable.Cen_Costos
                                   AND WEB.Anexos.Cuenta     = WEB.Mov_Contable.Cuenta
                                   AND WEB.Anexos.Nit        = WEB.Mov_Contable.Nit
                                   AND WEB.Anexos.Ano        = YEAR(WEB.Mov_Contable.Fec_Contable)
                                   AND WEB.Anexos.Mes        = MONTH(WEB.Mov_Contable.Fec_Contable) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE WEB.Anexos THEN
            IF LOCKED WEB.Anexos THEN NEXT.
            ELSE DO:
                CREATE WEB.Anexos.
                ASSIGN WEB.Anexos.Oficina    = WEB.Mov_Contable.Oficina
                      WEB.Anexos.Nit        = WEB.Mov_Contable.Nit
                      WEB.Anexos.Cuenta     = WEB.Mov_Contable.Cuenta
                    WEB.Anexos.Mes        = MONTH(WEB.Mov_Contable.Fec_Contable)
                     WEB.Anexos.Ano        = YEAR(WEB.Mov_Contable.Fec_Contable)
                      WEB.Anexos.Cen_Costos = WEB.Mov_Contable.Cen_Costos.
            END. /* Fin ELSE DO */
                                            
        ASSIGN WEB.Anexos.Sdo_Base = WEB.Anexos.Sdo_Base + WEB.Mov_Contable.Base.
        
        IF WEB.Mov_Contable.Naturaleza = "DB" THEN WEB.Anexos.Sal_Debito = WEB.Anexos.Sal_Debito + WEB.Mov_Contable.Valor.
        ELSE WEB.Anexos.Sal_Credito = WEB.Anexos.Sal_Credito + WEB.Mov_Contable.Valor.
                                            
        IF WEB.Cuentas.Naturaleza = "DB" THEN
            IF WEB.Mov_Contable.Naturaleza = "DB" THEN ASSIGN WEB.Anexos.Sdo_Final = WEB.Anexos.Sdo_Final + WEB.Mov_Contable.Valor.
            ELSE ASSIGN WEB.Anexos.Sdo_Final = WEB.Anexos.Sdo_Final - WEB.Mov_Contable.Valor.
        ELSE
            IF WEB.Mov_Contable.Naturaleza = "CR" THEN ASSIGN WEB.Anexos.Sdo_Final = WEB.Anexos.Sdo_Final + WEB.Mov_Contable.Valor.
            ELSE ASSIGN WEB.Anexos.Sdo_Final = WEB.Anexos.Sdo_Final - WEB.Mov_Contable.Valor.
        RELEASE WEB.Anexos.
        LEAVE.
    END. /* Fin REPEAT */
END. /* IF WEB.Mov_Contable.Nit <> "" THEN DO */
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarMovCreditos C-Win 
PROCEDURE ReplicarMovCreditos :
CREATE BD_WEB.Mov_creditos.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Mov_creditos.


IF LENGTH(BD_web.mov_creditos.descrip) > 50 THEN
    BD_web.mov_creditos.descrip = SUBSTRING(BD_web.mov_creditos.descrip,1,50).

ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarProAhorros C-Win 
PROCEDURE ReplicarProAhorros :
FIND FIRST BD_WEB.Pro_Ahorros WHERE BD_WEB.Pro_Ahorros.cod_ahorro = bdcentral.Replication.key-1
                                AND BD_WEB.Pro_Ahorros.tip_ahorro = bdcentral.Replication.key-3 NO-ERROR.
IF AVAILABLE BD_WEB.Pro_Ahorros THEN
    DELETE BD_WEB.Pro_Ahorros.

CREATE BD_WEB.Pro_Ahorros.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Pro_Ahorros.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarProCreditos C-Win 
PROCEDURE ReplicarProCreditos :
FIND FIRST BD_WEB.Pro_creditos WHERE BD_WEB.Pro_creditos.cod_credito = bdcentral.Replication.key-1
                                 AND BD_WEB.Pro_creditos.tip_credito = bdcentral.Replication.key-3 NO-ERROR.
IF AVAILABLE BD_WEB.Pro_creditos THEN
    DELETE BD_WEB.Pro_credito.

CREATE BD_WEB.Pro_creditos.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.Pro_creditos.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarSalCuenta C-Win 
PROCEDURE ReplicarSalCuenta :
/*FIND FIRST WEB.Sal_Cuenta WHERE WEB.Sal_Cuenta.Oficina     = Datos.Replication.key-1
                                     AND WEB.Sal_Cuenta.Cen_Costos  = Datos.Replication.key-3
                                     AND WEB.Sal_Cuenta.Cuenta      = Datos.Replication.key-2
                                     AND STRING(WEB.Sal_Cuenta.Ano) = Datos.Replication.key-4
                                     AND STRING(WEB.Sal_Cuenta.Mes) = Datos.Replication.key-5 NO-ERROR.
IF AVAILABLE WEB.Sal_Cuenta THEN DELETE WEB.Sal_Cuenta.
CREATE WEB.Sal_Cuenta.
RAW-TRANSFER Datos.Replication.record TO WEB.Sal_Cuenta.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarTaquilla C-Win 
PROCEDURE ReplicarTaquilla :
/*FIND FIRST WEB.Taquilla WHERE WEB.Taquilla.Oficina                 = Datos.Replication.key-1
                                   AND WEB.Taquilla.Usuario                 = Datos.Replication.key-4
                                   AND STRING(WEB.Taquilla.Fec_transaccion) = Datos.Replication.key-2
                                   AND WEB.Taquilla.Hora_transaccion        = Datos.Replication.key-3 NO-ERROR.
IF AVAILABLE WEB.Taquilla THEN DELETE WEB.Taquilla.
CREATE WEB.Taquilla.
RAW-TRANSFER Datos.Replication.record TO WEB.Taquilla.
ASSIGN Datos.Replication.Replicado = YES.*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarTerceros C-Win 
PROCEDURE ReplicarTerceros :
/*FIND FIRST WEB.Terceros WHERE WEB.Terceros.Nit = Datos.Replication.key-2 NO-ERROR.
IF AVAILABLE WEB.Terceros THEN DELETE WEB.Terceros.
CREATE WEB.Terceros.
RAW-TRANSFER Datos.Replication.record TO WEB.Terceros.
ASSIGN Datos.Replication.Replicado = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReplicarUbicacion C-Win 
PROCEDURE ReplicarUbicacion :
FIND FIRST BD_WEB.ubicacion WHERE BD_WEB.ubicacion.ubicacion = bdcentral.Replication.key-2 NO-ERROR.
IF AVAILABLE BD_WEB.ubicacion THEN
    DELETE BD_WEB.ubicacion.

CREATE BD_WEB.ubicacion.
RAW-TRANSFER bdcentral.Replication.record TO BD_WEB.ubicacion.
ASSIGN bdcentral.Replication.Replicado = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

