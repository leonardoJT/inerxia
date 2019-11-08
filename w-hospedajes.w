&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido\VARIABLE.I "SHARED"}

/* DEFINE VARIABLE W_Agencia AS INTEGER  INITIAL 1 NO-UNDO. */
/* DEFINE VARIABLE W_Usuario AS CHARACTER   NO-UNDO.        */

    /* oakley */

DEFINE VARIABLE vicomprobante AS INTEGER INITIAL 6 NO-UNDO.


DEFINE VARIABLE vcCCosto AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-cc f-Nit B-Contabiliza f-NumFact f-valor ~
btnFactura f-iva B-ocupacion BtnDone 
&Scoped-Define DISPLAYED-OBJECTS CB-cc f-Nit f-cliente f-NumFact f-valor ~
f-iva 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Contabiliza 
     LABEL "&Contabilizar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON B-ocupacion 
     LABEL "Ver Ocupacion" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON btnFactura 
     LABEL "Factura" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-4 
     LABEL "" 
     SIZE 30 BY .81.

DEFINE VARIABLE CB-cc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Centro de Costo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1","1"
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-cliente AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 18 FGCOLOR 0 FONT 1 NO-UNDO.

DEFINE VARIABLE f-iva AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Iva" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-Nit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-NumFact AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Num. Factura" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-valor AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-4 AT ROW 2.12 COL 27.57 WIDGET-ID 22
     CB-cc AT ROW 3.69 COL 12 COLON-ALIGNED WIDGET-ID 2
     f-Nit AT ROW 4.77 COL 12 COLON-ALIGNED WIDGET-ID 6
     f-cliente AT ROW 4.77 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     B-Contabiliza AT ROW 5.81 COL 53.57 WIDGET-ID 12
     f-NumFact AT ROW 5.85 COL 12 COLON-ALIGNED WIDGET-ID 32
     f-valor AT ROW 6.92 COL 12 COLON-ALIGNED WIDGET-ID 10
     btnFactura AT ROW 6.92 COL 53.57 WIDGET-ID 34
     f-iva AT ROW 8 COL 12 COLON-ALIGNED WIDGET-ID 30
     B-ocupacion AT ROW 8.04 COL 53.57 WIDGET-ID 36
     BtnDone AT ROW 9.15 COL 53.57 WIDGET-ID 14
     "FACTURACION ALOJAMIENTOS" VIEW-AS TEXT
          SIZE 29 BY .5 AT ROW 2.27 COL 28.14 WIDGET-ID 26
          BGCOLOR 8 FONT 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.43 BY 10.15
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


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
         TITLE              = "Hospedajes"
         HEIGHT             = 10.15
         WIDTH              = 72.43
         MAX-HEIGHT         = 17.54
         MAX-WIDTH          = 91.86
         VIRTUAL-HEIGHT     = 17.54
         VIRTUAL-WIDTH      = 91.86
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
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-cliente IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Hospedajes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Hospedajes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Contabiliza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Contabiliza C-Win
ON CHOOSE OF B-Contabiliza IN FRAME DEFAULT-FRAME /* Contabilizar */
DO:
    DEFINE VARIABLE viNumDocCont AS INTEGER NO-UNDO.
    DEFINE VARIABLE vcComentario AS CHARACTER NO-UNDO.
    DEFINE VARIABLE viAgeCCosto AS INTEGER NO-UNDO.
    
    IF f-Nit:SCREEN-VALUE = "" THEN DO:
        MESSAGE "El número de identificación es obligatorio." SKIP
                "Por favor rectifique."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    Contabiliza:
    DO TRANSACTION ON ERROR UNDO Contabiliza:
        FIND FIRST cen_costo WHERE cen_costo.cen_costo EQ INTEGER(CB-cc:SCREEN-VALUE).
        
        viAgeCCosto = cen_costo.agencia.
        vcComentario = "Alojamiento".
        
        RUN numDocContable (INPUT 1,
                            INPUT vicomprobante,
                            OUTPUT viNumDocCont).
        
        IF INTEGER(f-valor:SCREEN-VALUE) > 0 THEN DO:
            CREATE mov_contable.
            UPDATE Mov_Contable.agencia = W_Agencia
                   Mov_Contable.Cen_Costos = INTEGER(CB-cc:SCREEN-VALUE)
                   Mov_Contable.Comprobante = vicomprobante
                   Mov_Contable.Num_Documento = viNumDocCont
                   Mov_Contable.Nit = f-Nit:SCREEN-VALUE
                   Mov_Contable.Cuenta = "16150501"
                   Mov_Contable.Fec_Contable = w_fecha
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.Usuario = W_Usuario
                   Mov_Contable.Db = INTEGER(f-valor:SCREEN-VALUE)
                   Mov_Contable.Cr = 0
                   Mov_Contable.Comentario = vcComentario
                   Mov_Contable.Destino = W_agencia
                   Mov_Contable.Doc_Referencia = f-NumFact:SCREEN-VALUE.
        END.
        
        /*IVA*/
        IF INTEGER(f-iva:SCREEN-VALUE) > 0 THEN DO:
            CREATE mov_contable.
            UPDATE Mov_Contable.agencia = 1
                   Mov_Contable.Cen_Costos = 999
                   Mov_Contable.Comprobante = vicomprobante
                   Mov_Contable.Num_Documento = viNumDocCont
                   Mov_Contable.Nit = f-Nit:SCREEN-VALUE
                   Mov_Contable.Cuenta = "24401003"
                   Mov_Contable.Fec_Contable = w_fecha
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.Usuario = W_Usuario
                   Mov_Contable.Db = 0
                   Mov_Contable.Cr = INTEGER(f-iva:SCREEN-VALUE)
                   Mov_Contable.Comentario = "IVA - " + vcComentario
                   Mov_Contable.Destino = W_agencia
                   Mov_Contable.Doc_Referencia = f-NumFact:SCREEN-VALUE.
        
        
            IF W_Agencia NE 1 THEN DO:
                /*IVA*/
                CREATE mov_contable.
                UPDATE Mov_Contable.agencia = W_Agencia
                       Mov_Contable.Cen_Costos = 999
                       Mov_Contable.Comprobante = vicomprobante
                       Mov_Contable.Num_Documento = viNumDocCont
                       Mov_Contable.Nit = "001"
                       Mov_Contable.Cuenta = "27054001"
                       Mov_Contable.Fec_Contable = w_fecha
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Hora = TIME
                       Mov_Contable.Usuario = W_Usuario
                       Mov_Contable.Db = 0
                       Mov_Contable.Cr = INTEGER(f-iva:SCREEN-VALUE)
                       Mov_Contable.Comentario = "IVA - " + vcComentario
                       Mov_Contable.Destino = W_agencia
                       Mov_Contable.Doc_Referencia = f-NumFact:SCREEN-VALUE.
        
        
                /*S&A - IVA*/
                CREATE mov_contable.
                UPDATE Mov_Contable.agencia = 1
                       Mov_Contable.Cen_Costos = 999
                       Mov_Contable.Comprobante = vicomprobante
                       Mov_Contable.Num_Documento = viNumDocCont
                       Mov_Contable.Nit = STRING(W_Agencia,"999")
                       Mov_Contable.Cuenta = "27054001"
                       Mov_Contable.Fec_Contable = w_fecha
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Hora = TIME
                       Mov_Contable.Usuario = W_Usuario
                       Mov_Contable.Db = INTEGER(f-iva:SCREEN-VALUE)
                       Mov_Contable.Cr = 0
                       Mov_Contable.Comentario = vcComentario
                       Mov_Contable.Destino = W_agencia
                       Mov_Contable.Doc_Referencia = f-NumFact:SCREEN-VALUE.
            END.
        END.
        
        IF INTEGER(f-valor:SCREEN-VALUE) - INTEGER(f-iva:SCREEN-VALUE) > 0 THEN DO:
            CREATE mov_contable.
            UPDATE Mov_Contable.agencia = IF W_Agencia EQ viAgeCCosto THEN W_Agencia ELSE viAgeCCosto
                   Mov_Contable.Cen_Costos = INTEGER(CB-cc:SCREEN-VALUE)
                   Mov_Contable.Comprobante = vicomprobante
                   Mov_Contable.Num_Documento = viNumDocCont
                   Mov_Contable.Nit = f-Nit:SCREEN-VALUE
                   Mov_Contable.Cuenta = "41401001"
                   Mov_Contable.Fec_Contable = w_fecha
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.Usuario = W_Usuario
                   Mov_Contable.Db = 0
                   Mov_Contable.Cr = INTEGER(f-valor:SCREEN-VALUE) - INTEGER(f-iva:SCREEN-VALUE)
                   Mov_Contable.Comentario = vcComentario
                   Mov_Contable.Destino = W_agencia
                   Mov_Contable.Doc_Referencia = f-NumFact:SCREEN-VALUE.
        
            IF W_Agencia NE viAgeCCosto THEN DO: /*S&A*/
                CREATE mov_contable.
                UPDATE Mov_Contable.agencia = viAgeCCosto
                       Mov_Contable.Cen_Costos = INTEGER(CB-cc:SCREEN-VALUE)
                       Mov_Contable.Comprobante = vicomprobante
                       Mov_Contable.Num_Documento = viNumDocCont
                       Mov_Contable.Nit = STRING(W_agencia,"999")
                       Mov_Contable.Cuenta = "27054001"
                       Mov_Contable.Fec_Contable = w_fecha
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Hora = TIME
                       Mov_Contable.Usuario = W_Usuario
                       Mov_Contable.Db = INTEGER(f-valor:SCREEN-VALUE) - INTEGER(f-iva:SCREEN-VALUE)
                       Mov_Contable.Cr = 0
                       Mov_Contable.Comentario = "S&A - " + vcComentario
                       Mov_Contable.Destino = W_agencia
                       Mov_Contable.Doc_Referencia   = f-NumFact:SCREEN-VALUE.
        
                CREATE mov_contable.
                UPDATE Mov_Contable.agencia = W_Agencia
                       Mov_Contable.Cen_Costos = INTEGER(CB-cc:SCREEN-VALUE)
                       Mov_Contable.Comprobante = vicomprobante
                       Mov_Contable.Num_Documento = viNumDocCont
                       Mov_Contable.Nit = STRING(viAgeCCosto,"999")
                       Mov_Contable.Cuenta = "27054001"
                       Mov_Contable.Fec_Contable = w_fecha
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Hora = TIME
                       Mov_Contable.Usuario = W_Usuario
                       Mov_Contable.Db = 0
                       Mov_Contable.Cr = INTEGER(f-valor:SCREEN-VALUE) - INTEGER(f-iva:SCREEN-VALUE)
                       Mov_Contable.Comentario = "S&A - " + vcComentario
                       Mov_Contable.Destino = W_agencia
                       Mov_Contable.Doc_Referencia = f-NumFact:SCREEN-VALUE.
            END.
        END.
    END.

    IF viAgeCCosto <> w_agencia THEN
        RUN f-ingreso.p (INPUT vicomprobante,
                         INPUT viNumDocCont,
                         INPUT viNumDocCont,
                         INPUT viAgeCCosto,
                         INPUT w_fecha).

    RUN f-ingreso.p (INPUT vicomprobante,
                     INPUT viNumDocCont,
                     INPUT viNumDocCont,
                     INPUT W_Agencia,
                     INPUT w_fecha). 

    RUN limpiarPantalla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Salir */
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


&Scoped-define SELF-NAME btnFactura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFactura C-Win
ON CHOOSE OF btnFactura IN FRAME DEFAULT-FRAME /* Factura */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    listado = W_PathSpl + "FacturaAlojamiento_" + w_usuario + ".Lst".

    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw AS LOGICAL.

    RUN P-DisPos IN W_Manija (INPUT-OUTPUT listado,
                              INPUT-OUTPUT W_Dispositivo).

    IF W_Dispositivo = "" THEN
        RETURN.

    RUN _SetCurs.r ("WAIT").

    OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 90.
        RUN ProcesoImprimir.
    OUTPUT CLOSE.

    RUN _SetCurs.r ("ARROW").
    IF W_Dispositivo = "P" THEN
        RUN Pantalla IN W_Manija (INPUT listado).
    ELSE
        IF W_Dispositivo = "I" THEN
            RUN adecomm/_osprint.r (INPUT ?,
                                    INPUT Listado,
                                    INPUT 8,
                                    INPUT 1,
                                    INPUT 1,
                                    INPUT 99999,
                                    OUTPUT W_sw).

    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(listado).

    IF W_Dispositivo = "E" THEN
        RUN Imprimir_Excel.

    /*DEFINE VAR valorEnLetras AS CHARACTER.

    RUN MontoEsc.p (INPUT ROUND(DECIMAL(f-valor:SCREEN-VALUE),0),INPUT 0,OUTPUT valorEnLetras).

    FIND FIRST clientes WHERE clientes.nit = f-nit:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE clientes AND f-nit:SCREEN-VALUE <> "" THEN DO:
        RUN facturaAlojamiento.r(INPUT clientes.nit,
                                 INPUT clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,
                                 INPUT clientes.tel_residencia,
                                 INPUT clientes.dir_residencia,
                                 INPUT STRING(DAY(w_fecha),"99"),
                                 INPUT STRING(MONTH(w_fecha),"99"),
                                 INPUT STRING(YEAR(W_fecha),"9999"),
                                 INPUT f-numFact:SCREEN-VALUE,
                                 INPUT "Pago Alojamiento Sede " + SUBSTRING(CB-cc,13),
                                 INPUT DECIMAL(f-valor:SCREEN-VALUE) - DECIMAL(f-iva:SCREEN-VALUE),
                                 INPUT DECIMAL(f-iva:SCREEN-VALUE),
                                 INPUT DECIMAL(f-valor:SCREEN-VALUE),
                                 INPUT valorEnLetras) NO-ERROR.
    END.
    ELSE DO:
        MESSAGE "El Cliente no se encuentra registrado como Asociado ni como Tercero." SKIP
                "No es posible emitir una factura en estas condiciones. Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Nit C-Win
ON LEAVE OF f-Nit IN FRAME DEFAULT-FRAME /* Nit */
DO:
    FIND FIRST clientes WHERE clientes.nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        ASSIGN f-cliente:SCREEN-VALUE = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
    ELSE DO:
        MESSAGE "Asociado no existe" SKIP
            "Por favor revisar"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-valor C-Win
ON LEAVE OF f-valor IN FRAME DEFAULT-FRAME /* Valor */
DO:
    f-iva:SCREEN-VALUE = STRING(INTEGER(f-valor:SCREEN-VALUE) * 19 / 119).
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
    
  CB-cc:DELETE(1).
  FOR EACH Cen_Costos WHERE Cen_Costos.Cen_Costos NE 999 AND Cen_Costos.estado EQ 1 NO-LOCK:
      ASSIGN vcCCosto = STRING(Cen_Costos.agencia,"999") + " - " + STRING(Cen_Costos.Cen_Costos,"999") + " - " + Cen_Costos.Nombre.
      CB-cc:ADD-LAST(vcCCosto,STRING(Cen_Costos.Cen_Costos)).
  END.
  CB-cc:MOVE-TO-TOP ( ).

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
  DISPLAY CB-cc f-Nit f-cliente f-NumFact f-valor f-iva 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB-cc f-Nit B-Contabiliza f-NumFact f-valor btnFactura f-iva 
         B-ocupacion BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE limpiarPantalla C-Win 
PROCEDURE limpiarPantalla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN f-Nit:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "" 
        f-cliente:SCREEN-VALUE = ""
        f-NumFact:SCREEN-VALUE = "0" 
        f-valor:SCREEN-VALUE = "0"
        f-iva:SCREEN-VALUE = "0" .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numDocContable C-Win 
PROCEDURE numDocContable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

        DEFINE INPUT  PARAMETER ipiAgencia AS INTEGER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiComprobante AS INTEGER NO-UNDO.
        DEFINE OUTPUT PARAMETER opiNumDocumento AS INTEGER NO-UNDO.     
        FIND FIRST bdcentral.Comprobantes WHERE bdcentral.Comprobantes.agencia EQ ipiAgencia
                AND bdcentral.Comprobantes.Comprobante EQ ipiComprobante EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE(Comprobantes) THEN DO:
                ASSIGN 
                        bdcentral.Comprobantes.Secuencia = bdcentral.Comprobantes.Secuencia + 1
                    opiNumDocumento        = bdcentral.Comprobantes.Secuencia.          
                FIND CURRENT  bdcentral.Comprobantes NO-LOCK NO-ERROR.
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
DEFINE VAR vCenCosto AS CHARACTER.

FIND FIRST cen_costos WHERE cen_costos.cen_costos = INTEGER(CB-cc:SCREEN-VALUE IN FRAME default-frame) NO-LOCK NO-ERROR.
IF AVAILABLE cen_costos THEN
    vCenCosto = Cen_Costos.Nombre.

DISPLAY SKIP(12)
        f-cliente:SCREEN-VALUE IN FRAME default-frame AT 20 FORMAT "X(50)" SKIP(1)
        f-nit:SCREEN-VALUE                            AT 24 FORMAT "X(25)"
        clientes.tel_residencia                       AT 62                
        STRING(DAY(w_fecha),"99")                     AT 105 FORMAT "X(2)"
        STRING(MONTH(w_fecha),"99")                   AT 111 FORMAT "X(2)"
        STRING(YEAR(w_fecha),"9999")                  AT 115 FORMAT "X(4)" SKIP (1)
        clientes.DIR_residencia                       AT 20 FORMAT "X(50)" SKIP(3)
        STRING(INTEGER(CB-cc:SCREEN-VALUE),"999")     AT 15 FORMAT "X(3)"
        vCenCosto                                     AT 27 FORMAT "X(30)"
    WITH WIDTH 132 FRAME factura USE-TEXT NO-BOX STREAM-IO NO-LABELS.

/*DEFINE VAR TotalRet AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR WKEnt AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKDoc AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCer AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKHem AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKPie AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCta AS CHARACTER FORMAT "X(50)".
DEFINE VAR totalBase AS DECIMAL.

ASSIGN FRAME FCer Wano.

FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.

FOR EACH TNit NO-LOCK:
    DISPLAY SKIP(10)
            "                                  CERTIFICADO DE RETENCION EN LA FUENTE" SKIP
            "                                            AÑO GRAVABLE" STRING(WAno,"9999") SKIP(3)                
            "                En  cumplimiento  de  las  disposiciones  fiscales  vigentes  (artículo 381 del" SKIP
            "                estatuto tributario), el Fondo de Empleados Docentes de la Universidad Nacional" SKIP
            "                de  Colombia  -  FODUN,  con  NIT 800112808-7,  practicó Retención en la Fuente" SKIP
            "                durante el año " TRIM(STRING(WAno,"9999")) FORMAT "X(4)" "a:" CAPS(TNit.Nom) FORMAT "X(40)" ", identificado" SKIP
            "                con documento #" TRIM(TNit.Nit) FORMAT "X(12)" ", valores que fueron consignados en BOGOTÁ:" SKIP(2)
            "       -------------------------------------------------------------------------------" AT 10
            /*         1         2         3         4         5         6         7         8*/
            /*12346578901234657890123465789012346578901234657890123465789012346578901234657890*/
            "       CONCEPTO                                             BASE        VALOR RETENIDO" AT 10
            "       -------------------------------------------------------------------------------" AT 10
        WITH WIDTH 132 FRAME FEnca /*PAGE-TOP*/ USE-TEXT NO-BOX STREAM-IO NO-LABELS.

    
    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "ICA"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       ICA".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "RENDIMIENTOS FINANCIEROS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       RENDIMIENTOS FINANCIEROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret3 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "HONORARIOS Y/O SERVICIOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       HONORARIOS Y/O SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret4 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "SERVICIOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "COMPRAS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       COMPRA ACTIVOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) /*AND totalBase > 0*/ THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "DEMAS COSTOS Y DEDUCCIONES"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       OTROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret7 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "ARRENDAMIENTOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       ARRENDAMIENTOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret8 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "RETENCIÓN EN LA FUENTE"  NO-LOCK BREAK BY TCer.nit:
        WKCta = "       TOTAL RETENCIÓN EN LA FUENTE".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) THEN
            DISPLAY "       -------------------------------------------------------------------------------" AT 10
                    WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>>"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret9 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.


    DISPLAY SKIP(2)
            /*"                                                              Total Retenido: " TotRet*/
            /*"                                                              Sin Retención : " TNit.totalSinRet*/
            SKIP(3)
            "                 El  100% de los Rendimientos Financieros percibidos durante el año gravable" SKIP
            "                " STRING(WAno,"9999") ", no constituye Renta ni Ganancia Ocasional" /* (Decreto 0563 de Marzo 16" SKIP
            "                 de 2011)."*/
            /*"                 De conformidad con el Decreto 0652 de abril 05 de 2013, no constituye Renta ni"*/ SKIP
            /*"                 Ganancia  Ocasional  por  el  año gravable" STRING(WAno,"9999") + STRING(",","X(1)") "el 41.71% del valor de los"*/ skip
            /*"                 Rendimientos  Financieros  percibidos   por  personas  naturales  y sucesiones"*/ SKIP
            /*"                 ilíquidas no obligadas a llevar libros de contabilidad."*/
            SKIP(3)
            "                 Este Certificado no requiere firma autógrafa según Artículo 10 del Decreto 836" SKIP
            "                 de 1991."
        WITH FRAME FPieCer /*PAGE-BOTTOM*/ USE-TEXT WIDTH 132 NO-BOX STREAM-IO NO-LABELS.

    PAGE.
END.*/

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

