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

CREATE WIDGET-POOL.
{Incluido\variable.i "shared"}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  DEFINE VAR W_TX AS CHARACTER FORMAT "X(25)".
  DEFINE VAR W_OK AS LOGICAL INITIAL YES.
  DEFINE VARIABLE W_Rpta AS LOGICAL.
  DEFINE VAR wTcupo LIKE Ahorros.Sdo_disponible INITIAL 0.
  DEFINE VAR Listado  AS CHARACTER INITIAL "".
  DEFINE VAR WListado AS CHARACTER INITIAL "". 
  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VAR W_sw          AS LOGICAL. 

  DEFINE VAR zswsalida AS LOGICAL INITIAL NO.
  DEFINE VARIABLE procname AS CHARACTER NO-UNDO.
  DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.
  DEFINE VAR W_Pan AS INTEGER INITIAL 1. /*1-pantalla 2 pantalla2*/
  
  DEFINE TEMP-TABLE Tmp
  FIELD TAge     LIKE Agencias.Agencia
  FIELD TNit     LIKE clientes.nit
  FIELD TCupo    LIKE creditos.Monto
    INDEX Cage Tage Tnit.

  DEFINE TEMP-TABLE tmpincons
    FIELD tdanger AS LOGICAL INITIAL FALSE
    FIELD terror  AS CHARACTER FORMAT "X(80)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Imp-3 BUTTON-3 Btn_Grabar Btn_Done 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 10 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar" 
     SIZE 43 BY 1.12.

DEFINE BUTTON Btn_Imp-3 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Btn_imp 3" 
     SIZE 10 BY 1.62
     FONT 8.

DEFINE BUTTON BUTTON-3 
     LABEL "Leer Archivo Plano" 
     SIZE 43 BY 1.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     Btn_Imp-3 AT ROW 1.27 COL 48 WIDGET-ID 16
     BUTTON-3 AT ROW 1.54 COL 2.14 WIDGET-ID 4
     Btn_Grabar AT ROW 2.88 COL 2 WIDGET-ID 6
     Btn_Done AT ROW 2.88 COL 48 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.57 BY 5.31
         BGCOLOR 17  WIDGET-ID 100.


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
         TITLE              = "Carga Masiva de Cupo Rotativo - w-CargaMasiva_CupoRotativo.r"
         HEIGHT             = 3.58
         WIDTH              = 58.72
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Carga Masiva de Cupo Rotativo - w-CargaMasiva_CupoRotativo.r */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Carga Masiva de Cupo Rotativo - w-CargaMasiva_CupoRotativo.r */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME f-main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar C-Win
ON CHOOSE OF Btn_Grabar IN FRAME f-main /* Grabar */
DO:
  IF NOT zswsalida THEN
     RUN Generar_CupoRotativo.
  ELSE
     MESSAGE "Corrija Primero las inconsistencias e intente de nuevo!!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp-3 C-Win
ON CHOOSE OF Btn_Imp-3 IN FRAME f-main /* Btn_imp 3 */
DO:
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    ASSIGN WListado = Listado.
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
  OS-RENAME VALUE(WListado) VALUE(Listado).
  /*MESSAGE listado VIEW-AS ALERT-BOX.*/
/*    IF W_Dispositivo = "" THEN
      RETURN.*/
    IF W_Dispositivo = "P" THEN DO: 
        IF W_Pan EQ 1 THEN RUN Pantalla IN W_Manija (INPUT Listado).
        IF W_Pan EQ 2 THEN RUN Pantalla2 IN W_Manija (INPUT Listado).
    END.
      
    ELSE                                                  
      IF W_Dispositivo = "I" THEN DO:
        IF W_Pan EQ 1 THEN 
            RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  2,INPUT  1,INPUT  1,
                                           INPUT  99999,OUTPUT W_sw).
         IF W_Pan EQ 2 THEN
             RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  8,INPUT  1,INPUT  1,
                                            INPUT  99999,OUTPUT W_sw).
      END.
    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(Listado).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME f-main /* Leer Archivo Plano */
DO:
  FOR EACH Tmp: DELETE Tmp. END.
  SYSTEM-DIALOG GET-FILE procname
        TITLE      "Choose Procedure to Run ..."
        FILTERS    "Source Files (*.txt)"   "*.txt",
                   "R-code Files (*.prn)"   "*.prn"
        INITIAL-DIR "C:\sicobel\"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
  RUN _SetCurs.r ("WAIT").     
    IF OKpressed = TRUE THEN
        RUN Generar_Temporal.
  RUN _SetCurs.r ("ARROW").
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
   DISABLE Btn_GRabar WITH FRAME F-plano.
  RUN enable_UI.
  DISABLE Btn_GRabar WITH FRAME F-plano.
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
  ENABLE Btn_Imp-3 BUTTON-3 Btn_Grabar Btn_Done 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_CupoRotativo C-Win 
PROCEDURE Generar_CupoRotativo :
/* ASSIGN FRAME F-plano. */

IF WTcupo = 0 THEN DO:
   MESSAGE "No se puede crear Cupo Rotativo, porque el cupo es igual a cero" SKIP
            VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.

DEFINE VAR wcrage     like creditos.agencia.
DEFINE VAR wcrnit     LIKE creditos.nit.
DEFINE VAR wcrcodcre  LIKE creditos.cod_credito.
DEFINE VAR wcrnumcre  LIKE creditos.num_credito.
DEFINE VAR wtasa      LIKE creditos.tasa.

DEFINE VAR wantcupo AS DECIMAL INITIAL 0.


FIND FIRST pro_creditos WHERE (pro_credito.cod_credito = 570 OR 
                               pro_credito.cod_credito = 870) NO-LOCK NO-ERROR.
IF AVAILABLE(pro_creditos) THEN
   FIND  FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.
   IF AVAILABLE(indicadores) THEN DO:
      wtasa =  (((EXP( (indicadores.tasa / 100) + 1,1 / 12)) - 1 )  * 100) * 12.
   END.
   ELSE Do: 
     MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
     DISABLE btn_grabar.
     RETURN.
   END.

DEFINE VAR zconta  AS LOGICAL INITIAL FALSE.

Grabarcupo:
DO TRANSACTION ON ERROR UNDO Grabarcupo:
  FOR EACH Tmp WHERE Tmp.tnit NE "" BREAK BY Tmp.Tage:
    FIND FIRST creditos WHERE creditos.nit EQ Tmp.Tnit AND 
               (creditos.cod_credito = 570 OR creditos.cod_credito = 870) 
                AND creditos.estado = 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(creditos)  THEN DO:

        CREATE creditos.
        ASSIGN creditos.fec_aprobacion = TODAY
               creditos.agencia        = Tmp.Tage
               creditos.nit            = Tmp.Tnit
               creditos.cod_credito    = 570
               creditos.tip_credito    = 1
               creditos.num_credito    = NEXT-VALUE(sec_creditos)
               creditos.pagare         = STRING(CURRENT-VALUE(sec_creditos))
               creditos.tasa           = wtasa
               creditos.fec_desembolso = w_fecha
               creditos.plazo          = 36
               creditos.usuario        = w_usuario
               creditos.monto          = tmp.Tcupo
               creditos.estado         = 2
               creditos.detalle_estado = 1.

       ASSIGN wcrage     = creditos.agencia
              wcrnit     = creditos.nit
              wcrcodcre  = creditos.cod_credito
              wcrnumcre  = creditos.num_credito.
       RELEASE creditos.
       FIND FIRST creditos NO-LOCK NO-ERROR.
       RUN w-planpagosCupoRotativo.r (INPUT wcrage, wcrnit,wcrcodcre, wcrnumcre).
       CREATE Hoja_Vida.
              ASSIGN Hoja_Vida.Tipo             = 1 
                     Hoja_Vida.Codigo           = 1  
                     Hoja_Vida.Nit              = Tmp.Tnit
                     Hoja_Vida.Usuario          = W_Usuario
                     Hoja_Vida.Fec_Grabacion    = W_fecha
                     Hoja_Vida.Hora_Grabacion   = TIME
                     Hoja_Vida.Observacion      = "Creacion de Cupo Rotativo al nit: " + Tmp.Tnit + 
                                                  " NroCre : " + TRIM(STRING( wcrnumcre)) + " Cupo: " +  string(tmp.Tcupo).
       RELEASE creditos.     
       FIND FIRST creditos NO-LOCK NO-ERROR.
       btn_grabar:SENSITIVE IN FRAME f-main = FALSE.
       /* RUN inicializar_variables */.
    END.
    /*ELSE
       PUT "c.c. " Tmp.Tnit " Ya tiene cupo de " Creditos.monto " en la agencia: " creditos.agencia " No se graba " SKIP(0). */

  END.
END.
MESSAGE " Termino la creación del Cupo Rotativo exitosamente, revise por favor " SKIP(1)
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF zconta THEN MESSAGE "" VIEW-AS ALERT-BOX.
FOR EACH Tmp:
  DELETE tmp.
END.
/* DISABLE Btn_Conta WITH FRAME F-Main. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_temporal C-Win 
PROCEDURE Generar_temporal :
DEFINE VAR  wcon_linea   AS INTEGER INITIAL 1.
FOR EACH tmpincons:
  DELETE tmpincons.
END.
INPUT FROM VALUE(Procname).

wtcupo = 0.
REPEAT:
   CREATE Tmp.
   IMPORT DELIMITER "," tmp.
   IF Tmp.Tage = 0 OR Tmp.Tnit = "" THEN DO:
      DELETE Tmp.
      NEXT.
   END.
   FIND FIRST creditos WHERE 
       (creditos.cod_credito = 570 OR creditos.cod_credito = 870) AND
        creditos.nit = Tmp.Tnit NO-LOCK NO-ERROR.
   IF AVAILABLE(creditos) THEN DO:
     CREATE tmpincons.
     ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cupo Rotativo YA EXISTE c.c.: " + string(Tmp.Tnit,"X(12)") + "  En la agencia: " + STRING(creditos.agencia,"999").
     ASSIGN wcon_linea = wcon_linea + 1.
     DELETE Tmp.
     NEXT.
   END.
   FIND FIRST ahorros WHERE ahorros.tip_ahorro = 4 AND 
                            ahorros.cod_ahorro = 5 AND 
                            ahorros.nit = Tmp.Tnit AND ahorros.estado = 1 NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE(ahorros) THEN DO:
     CREATE tmpincons.
     ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cliente Sin aportes Obligatorios c.c.: " + string(Tmp.Tnit,"X(12)") + "  En la agencia: " + STRING(Tmp.Tage,"999").
     ASSIGN wcon_linea = wcon_linea + 1.
     DELETE Tmp.
     NEXT.
   END.
   ELSE
     IF (ahorros.sdo_disponible + ahorros.sdo_canje) LE 0 THEN DO:
         CREATE tmpincons.
         ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cliente Sin Saldo en Aportes Obligatorios c.c.: " + string(Tmp.Tnit,"X(12)") + "  En la agencia: " + STRING(Tmp.Tage,"999") + " Sdo: " + STRING(ahorros.sdo_disponible + ahorros.sdo_canje, "->>>,>>9.99").
         ASSIGN wcon_linea = wcon_linea + 1.
         DELETE Tmp.
         NEXT.
     END.


   FIND FIRST agencias WHERE agencias.agencia EQ Tmp.Tage NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(agencias) THEN DO:
     CREATE tmpincons.
     ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cod. Agencia: " + string(Tmp.Tage,"999") + " No Encontrada  "
            tmpincons.Tdanger =  TRUE.
   END.
   IF TRIM(Tmp.TNit) = "" THEN DO:
       CREATE tmpincons.
       ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cedula/Nit en blanco,  Agencia " + STRING(Tmp.Tage,"999")
              tmpincons.Tdanger =  TRUE.
   END.
   FIND FIRST Clientes WHERE Clientes.Nit EQ TRIM(Tmp.TNit) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) AND Tmp.TNit NE "" THEN DO:
      CREATE tmpincons.
      ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cedula/Nit : " + string(Tmp.Tnit,"X(12)") + " No Encontrada ,  Agencia " + STRING(Tmp.Tage,"999")
             tmpincons.Tdanger =  TRUE.
   END.
   IF Tmp.Tcupo = 0  THEN DO:
      CREATE tmpincons.
      ASSIGN tmpincons.Terror  = "Linea " + STRING(wcon_linea,">>>,>>9") + " NO Tiene valor de Cupo Asignado,  Agencia " + STRING(Tmp.Tage,"999")
             tmpincons.Tdanger =  TRUE.
   END.
   IF Tmp.Tcupo < 0 THEN DO:
      CREATE tmpincons.
      ASSIGN tmpincons.Terror  = "Linea " + STRING(wcon_linea,">>>,>>9") + " NO Tiene Cupo Positivos  Cupo: " + STRING(Tmp.Tcupo,"->>>,>>9.99") 
             tmpincons.Tdanger =  TRUE.
   END.
   IF Tmp.Tcupo GT 0 THEN
      WTcupo = WTcupo + Tmp.Tcupo.
      
   ASSIGN wcon_linea = wcon_linea + 1.
END.
INPUT CLOSE.

/*CAge/CNit/CCue/CNat/Cval/CCom*/

ASSIGN wcon_linea = 0.
FOR EACH Tmpincons:
  IF tmpincons.Tdanger THEN DO: 
     ASSIGN wcon_linea = wcon_linea + 1
            zswsalida  = TRUE.
  END.
END.
IF zswsalida THEN DO:
   MESSAGE "no se permite la actualización de datos " SKIP(0)
           "se encontraron " + STRING(wcon_linea,">>>,>>9") " registros con error"
            VIEW-AS ALERT-BOX. /* QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice AS LOGICAL. IF CHOICE THEN quit.*/
   DISABLE btn_Grabar WITH FRAME f-plano.
END.
ELSE 
  IF WTcupo GT 0  THEN 
     ENABLE Btn_GRabar WITH FRAME F-plano.

RUN rutina_imprimir.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rutina_imprimir C-Win 
PROCEDURE Rutina_imprimir :
Listado = W_PathSpl + "CargueMasivo-" + W_Usuario + STRING(RANDOM(2000,10000)) +  ".Lst".
OS-DELETE VALUE(Listado).

{Incluido\RepEncabezado.i}
IF W_Pan EQ 1 THEN
   OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
IF W_Pan EQ 2 THEN
   OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 57.
 
 W_Reporte   = "REPORTE   : CREDITOS MASIVOS CUPO ROTATIVO - FECHA: " + STRING(w_fecha) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Cod  Agencia                                 Nit          Nombres completos                           Cupo A otorgar".
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.
 
 FOR EACH tmpincons:
    DISPLAY tmpincons.Terror WITH FRAME FINFO WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
    zswsalida = TRUE.
 END.

 PUT SKIP(2).
 DEFINE VAR znombre     AS CHARACTER FORMAT "X(40)" INITIAL "".
 DEFINE VAR ztotalcupo  AS DECIMAL   FORMAT "zz,zzz,zzz,zz9.99" INITIAL 0.

 FOR EACH TMP:
    IF tmp.Tnit EQ "" OR tmp.tcupo = 0 OR tmp.tage = 0 THEN NEXT.
    FIND FIRST agencias    WHERE agencias.agencia = tmp.Tage NO-LOCK NO-ERROR.
    FIND FIRST clientes    WHERE clientes.nit     = tmp.Tnit NO-LOCK NO-ERROR.
    znombre = TRIM(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2).
    PUT Tmp.Tage " " agencias.nombre " " Tmp.Tnit " " znombre " " Tmp.Tcupo FORMAT "zz,zzz,zzz,zz9.99" SKIP(0).
    ztotalcupo = ztotalcupo + Tmp.Tcupo.
 END.
 IF ztotalcupo GT 0 THEN
    PUT SKIP(1) "Total Cupo a otorgar                         :" ztotalcupo FORMAT "zz,zzz,zzz,zz9.99".
 VIEW FRAME F-Ftr.
 PAGE.
 
 IF zswsalida THEN
    DISABLE btn_grabar.
 ELSE
    ENABLE btn_grabar.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

