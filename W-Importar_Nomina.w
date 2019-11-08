&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/* oakley */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{incluido\Variable.i "SHARED"}

DEFINE VARIABLE W_Rpta AS LOGICAL.

DEFINE TEMP-TABLE Tmp
    FIELD CAge AS INTEGER
    FIELD CCue AS CHARACTER FORMAT "X(12)"
    FIELD CCom AS CHARACTER
    FIELD CNit AS CHARACTER
    FIELD CDocR AS CHARACTER
    FIELD CDB AS DECIMAL
    FIELD CCR AS DECIMAL
    FIELD ccos AS INTEGER
    INDEX Cage Ccue.

DEFINE VAR TDeb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9". 
DEFINE VAR TCre AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE procname AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.
DEFINE VAR zswsalida AS LOGICAL INITIAL NO.

DEFINE TEMP-TABLE tmpincons
    FIELD tdanger AS LOGICAL INITIAL FALSE
    FIELD terror AS CHARACTER FORMAT "X(80)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-228 RECT-322 BUTTON-3 BUTTON-174 ~
tg-RepGMF Btn_Done BUTTON-175 
&Scoped-Define DISPLAYED-OBJECTS DebPan CrePan tg-RepGMF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Conta 
     LABEL "Contabilizar" 
     SIZE 43 BY 1.12.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-174 
     LABEL "Revisar comprobante sin contabilzar" 
     SIZE 43 BY 1.12.

DEFINE BUTTON BUTTON-175 
     LABEL "Ver estructura" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-3 
     LABEL "Leer Archivo Plano" 
     SIZE 43 BY 1.12.

DEFINE VARIABLE CrePan AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Créditos" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE DebPan AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Débitos" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE DifPan AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Diferencia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 0    
     SIZE 28 BY .12
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.72 BY 1.35.

DEFINE VARIABLE tg-RepGMF AS LOGICAL INITIAL no 
     LABEL "No actualizar Reporte GMF" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-3 AT ROW 2.88 COL 4
     DebPan AT ROW 4.27 COL 28.86 COLON-ALIGNED
     CrePan AT ROW 5.35 COL 28.86 COLON-ALIGNED
     DifPan AT ROW 6.96 COL 28.86 COLON-ALIGNED
     BUTTON-174 AT ROW 8.19 COL 4
     tg-RepGMF AT ROW 9.35 COL 5 WIDGET-ID 2
     Btn_Conta AT ROW 10.23 COL 4
     Btn_Done AT ROW 11.58 COL 39
     BUTTON-175 AT ROW 12.04 COL 21 WIDGET-ID 4
     "           Comprobante de Cargue de Nómina: (4 - Nota)" VIEW-AS TEXT
          SIZE 48.57 BY 1.08 AT ROW 1.35 COL 2
          BGCOLOR 18 FGCOLOR 15 
     RECT-228 AT ROW 6.5 COL 19
     RECT-322 AT ROW 1.19 COL 1.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.14 BY 17
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Importar Archivo plano de Nómina"
         HEIGHT             = 12.69
         WIDTH              = 50.86
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Conta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CrePan IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DebPan IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DifPan IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Importar Archivo plano de Nómina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Importar Archivo plano de Nómina */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Conta W-Win
ON CHOOSE OF Btn_Conta IN FRAME F-Main /* Contabilizar */
DO:
  IF NOT zswsalida THEN
     RUN Generar_Movimiento.
  ELSE
     MESSAGE "Corrija Primero las inconsistencias e intente de nuevo!!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Win
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


&Scoped-define SELF-NAME BUTTON-174
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-174 W-Win
ON CHOOSE OF BUTTON-174 IN FRAME F-Main /* Revisar comprobante sin contabilzar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  /*{Imprimir.I "listado"}*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-175
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-175 W-Win
ON CHOOSE OF BUTTON-175 IN FRAME F-Main /* Ver estructura */
DO:
MESSAGE "Agencia" SKIP
        "cuenta  " SKIP
        "Comentario "  SKIP
        "nit        "      SKIP
        "doc_referencia "      SKIP
        "db              "         SKIP
        "cr" SKIP
        "cada campo separado con ; "view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Leer Archivo Plano */
DO:
    ASSIGN CrePan = 0
           DebPan = 0
           DifPan = 0.

    DISPLAY CrePan
            DebPan
            DifPan
        WITH FRAME F-Main.

    EMPTY TEMP-TABLE Tmp.

    SYSTEM-DIALOG GET-FILE procname
        TITLE "Choose Procedure to Run ..."
        FILTERS "Planos (*.csv)"   "*.csv", "Texto (*.txt)"   "*.txt"
        INITIAL-DIR "C:\info_fodun\"
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


&Scoped-define SELF-NAME tg-RepGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-RepGMF W-Win
ON VALUE-CHANGED OF tg-RepGMF IN FRAME F-Main /* No actualizar Reporte GMF */
DO:
  ASSIGN tg-repGMF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY DebPan CrePan tg-RepGMF 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-228 RECT-322 BUTTON-3 BUTTON-174 tg-RepGMF Btn_Done BUTTON-175 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Movimiento W-Win 
PROCEDURE Generar_Movimiento :
ASSIGN FRAME F-Main CrePan DebPan.
DEFINE VAR w_doc LIKE mov_contable.num_documento.
DEFINE VAR w_cbt LIKE mov_contable.comprobante.

IF CrePan NE DebPan  AND (Crepan + Debpan > 0) THEN DO:
   MESSAGE "No se puede grabar un movimiento si el débito y el crédito no son iguales" SKIP
           "consulte el informe sin contabilizar!." VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.

/*IF W_Doc EQ 0 THEN DO:
   MESSAGE "El numero de documento está en 0" VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.*/

DEFINE VAR zconta  AS LOGICAL INITIAL FALSE.
Grabar:
DO TRANSACTION ON ERROR UNDO Grabar:
    FOR EACH Tmp WHERE Tmp.CCue NE "" BREAK BY Tmp.Cage:
    
      IF FIRST-OF(Tmp.Cage) THEN DO:
      
          FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ 4
                                   AND Comprobantes.Agencia     EQ Tmp.Cage
                                   AND Comprobantes.Estado      EQ 1 NO-ERROR.
          IF AVAILABLE Comprobantes THEN DO:
          
          ASSIGN W_Cbt = 4
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               W_Doc = Comprobantes.Secuencia.
         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
         END.
      END.

      CREATE Mov_Contable.
      ASSIGN Mov_Contable.Agencia         = Tmp.Cage /* INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) */
             Mov_Contable.Cen_Costos      = tmp.ccos
             Mov_Contable.Cuenta          = Tmp.CCue
             Mov_Contable.Comprobante     = W_Cbt   /*nota contable*/
             Mov_Contable.Fec_Contable    = W_Fecha
             Mov_Contable.Fec_Grabacion   = W_Fecha
             Mov_Contable.Num_Documento   = W_Doc
             Mov_Contable.Doc_Referencia  = STRING(Tmp.Cdoc)
             Mov_Contable.Nit             = TRIM(Tmp.CNit)
             Mov_Contable.Comentario      = Tmp.CCom
             Mov_Contable.Usuario         = W_Usuario.
      
      zconta = TRUE.

      IF Tmp.Cdb GT 0 THEN DO:
          Mov_Contable.DB         = Tmp.CDb.
      END.
      ELSE DO:
          Mov_Contable.CR         = Tmp.CCR.
      END.

      IF mov_contable.cuenta BEGINS '2430' AND NOT tg-RepGMF THEN
          RUN Graba_RepGMF.

      /*
      IF LAST-OF(Tmp.Cage) THEN DO:
          RUN Rutina_Imprimir (INPUT W_Cbt, Tmp.Cage),
                               W_Doc, W_Doc).
      END. */
    END.
    IF zconta THEN MESSAGE "Contabilización Realizada" VIEW-AS ALERT-BOX.
    FOR EACH Tmp:
      DELETE tmp.
    END.
END.
/* DISABLE Btn_Conta WITH FRAME F-Main. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Temporal W-Win 
PROCEDURE Generar_Temporal :
DEFINE VAR wcon_linea AS INTEGER INITIAL 0.

EMPTY TEMP-TABLE tmpincons.

zswsalida = FALSE.

DISABLE Btn_Conta WITH FRAME F-main.

INPUT FROM VALUE(Procname).
REPEAT:
    wcon_linea = wcon_linea + 1.

    CREATE Tmp.
    IMPORT DELIMITER ";" tmp.

    IF Tmp.Cage = 0 OR ( Tmp.Cdb = 0 AND Tmp.Ccr = 0 )  THEN DO:
        DELETE Tmp.
        NEXT.
    END.

    FIND FIRST agencias WHERE agencias.agencia EQ Tmp.Cage NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(agencias) THEN DO:
        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cod. Agencia: " + string(Tmp.Cage,"999") + " no Encontrada  ".
    END.

    FIND FIRST Clientes WHERE Clientes.Nit EQ TRIM(Tmp.CNit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Clientes) AND Tmp.CNit NE "" THEN DO:
        /*MESSAGE wcon_linea Tmp.cage tmp.CCue tmp.CCom tmp.CNit tmp.CDocR tmp.CDB tmp.CCR tmp.ccos
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cedula/Nit: " + string(Tmp.Cnit,"X(12)") + " no Encontrada, Agencia " + STRING(Tmp.Cage,"999")
               tmpincons.Tdanger = TRUE.
    END.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Tmp.CCue NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Cuentas) THEN DO:
        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta: " + string(Tmp.Ccue,"X(12)") + " no Encontrada, Agencia " + STRING(Tmp.Cage,"999")
               tmpincons.Tdanger = TRUE.
    END.
    ELSE DO:
        IF Cuentas.Id_nit AND Tmp.CNit EQ "" THEN DO:
            CREATE tmpincons.
            ASSIGN tmpincons.Terror  = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta: " + string(Tmp.Ccue,"X(12)") + " exige nit, el nit no puede ser blancos, Agencia " + STRING(Tmp.Cage,"999")
                   tmpincons.Tdanger = TRUE.
        END.

        IF Cuentas.Tipo NE 2 THEN DO:
            CREATE tmpincons.
            ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta: " + string(Tmp.Ccue,"X(12)") + " debe ser cuenta de Movimiento, Agencia " + STRING(Tmp.Cage,"999")
                   tmpincons.Tdanger = TRUE.
        END.  

        IF Cuentas.id_NoMvto THEN DO:
            CREATE tmpincons.
            ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta: " + string(Tmp.Ccue,"X(12)") + " sólo se maneja por taquilla, Agencia " + STRING(Tmp.Cage,"999")
                   tmpincons.Tdanger = TRUE.
        END.

        IF cuentas.estado = 2 THEN DO:
            CREATE tmpincons.
            ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta: " + string(Tmp.Ccue,"X(12)") + " se encuentra inactiva, Agencia " + STRING(Tmp.Cage,"999")
                   tmpincons.Tdanger = TRUE.
        END.

        IF cuentas.id_cenCostos = TRUE THEN DO:
            FIND FIRST cen_costos WHERE cen_costos.cen_costo = tmp.ccos
                                    AND cen_costos.agencia = tmp.cage NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cen_costos THEN DO:
                CREATE tmpincons.
                ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + ". El Centro de Costos " + string(Tmp.ccos,"999") + " no se encuentra configurado para la agencia " + STRING(Tmp.Cage,"999") + "."
                       tmpincons.Tdanger = TRUE.
            END.
            ELSE DO:
                IF cen_costos.estado = 2 THEN DO:
                    CREATE tmpincons.
                    ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + ". El Centro de Costos " + string(Tmp.ccos,"999") + " se encuentra inactivo para la agencia " + STRING(Tmp.Cage,"999") + "."
                           tmpincons.Tdanger = TRUE.
                END.
            END.
        END.
    END.

    IF Tmp.Cdb = 0 AND Tmp.Ccr = 0 THEN DO:
        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " no tiene valores Dédito ni Crédito, Agencia " + STRING(Tmp.Cage,"999")
               tmpincons.Tdanger = TRUE.
    END.

    IF Tmp.Cdb < 0 OR Tmp.Ccr < 0 THEN DO:
        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " no tiene valores Positivos - CR: " + STRING(Tmp.Ccr,"->>>,>>9.99") + "  DB: " + STRING(Tmp.Cdb,"->>>,>>9.99") + "  Agencia " + STRING(Tmp.Cage,"999")
               tmpincons.Tdanger = TRUE.
    END.

    IF Tmp.Cdb > 0 AND Tmp.Ccr > 0 THEN DO:
        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Tiene valores DB: " + STRING(Tmp.Cdb,"->>>,>>9.99") +  " y CR: " +  STRING(Tmp.Ccr,"->>>,>>9.99")  + ", Agencia " + STRING(Tmp.Cage,"999")
               tmpincons.Tdanger = TRUE.
    END.

    IF Cdb GT 0 THEN
        TDeb = TDeb + Tmp.Cdb.
    ELSE
        IF Ccr GT 0 THEN
            TCre = TCre + Tmp.Ccr.

    /* Control para Activos Fijos */
    IF SUBSTRING(Tmp.CCue,1,2) = "17" THEN DO:
        CREATE tmpincons.
        ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>>>9") + " Tiene una cuenta 17. Esta es una cuenta de Activos Fijos y solo puede usarse con el comprobante 10. No se permite la carga"
               tmpincons.Tdanger = TRUE.

        tmpincons.Tdanger = TRUE.
    END.
END.

IF TDeb NE TCre THEN DO:
    CREATE tmpincons.
    ASSIGN tmpincons.Terror = "DB: " + STRING(Tdeb,"->>,>>>,>>>,>>9.99") + " no Cuadran con los CR: " + STRING(Tcre,"->>,>>>,>>>,>>9.99")
           tmpincons.Tdanger = TRUE.
END.

INPUT CLOSE.

OUTPUT TO "c:\info_fodun\errorCargaContab.txt".
wcon_linea = 0.

FOR EACH Tmpincons:
    IF tmpincons.Tdanger THEN DO:
        ASSIGN wcon_linea = wcon_linea + 1
               zswsalida  = TRUE.

        EXPORT Tmpincons.
    END.
END.
OUTPUT CLOSE.

IF zswsalida THEN DO:
    MESSAGE "No se permite la carga del movimiento contable." SKIP(0)
            "Se encontraron " + STRING(wcon_linea,">>>,>>9") " registros con error" SKIP (2)
            "Generado archivo info_fodun\errorCargaContab.txt"
        VIEW-AS ALERT-BOX.

    DISABLE btn_Conta WITH FRAME f-main.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_RepGMF W-Win 
PROCEDURE Graba_RepGMF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR zimpto  AS  DECIMAL INITIAL 0.00. 
  DEFI VAR zvlr    AS  DECIMAL INITIAL 0.00. 
  CREATE Mov_GMF.
  
  IF mov_contable.cr GT 0 THEN ASSIGN zimpto = mov_contable.cr.
  ELSE ASSIGN zimpto = mov_contable.db * (-1).
  zvlr = ROUND((((zimpto) * 1000) / 4),2).

  ASSIGN Mov_GMF.Agencia             = mov_contable.agencia
         Mov_GMF.Agencia_Tx          = mov_contable.agencia
         Mov_GMF.Documento           = string(mov_contable.num_documento)
         Mov_GMF.Fecha               = mov_contable.fec_contable
         Mov_GMF.Hora                = TIME
         Mov_GMF.Descrip             = "GMF Importar Archivos Planos"
         Mov_GMF.Id_EfChTras         = 2
         Mov_GMF.Nit                 = mov_contable.nit
         Mov_GMF.Porc_Impto          = 0
         Mov_GMF.Renglon             = 00
         Mov_GMF.Tipo_Pdcto          = 3
         Mov_GMF.VrAcum_RetMes       = 0
         Mov_GMF.Cod_Pdcto           = INTEGER(SUBSTRING(mov_contable.doc_referencia,1,3))
         Mov_GMF.Cpte                = mov_contable.comprobante 
         Mov_GMF.CtaDb_ImptoCliente  = ''
         Mov_GMF.CtaDb_ImptoEntidad  = mov_contable.cuenta
         Mov_GMF.Cta_ContableCr      = mov_contable.cuenta
         Mov_GMF.Cta_PdctoOContab    = SUBSTRING(mov_contable.doc_referencia,4,14)
         Mov_GMF.VrBase_Cliente      = 0
         Mov_GMF.VrBase_Entidad      = zvlr
         Mov_GMF.VrBase_Exenta       = 0
         Mov_GMF.VrImpto_Cliente     = 0
         Mov_GMF.VrImpto_Entidad     = zImpto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_NumCbt W-Win 
PROCEDURE Llenar_NumCbt :
/*
FIND Comprobantes WHERE 
     Comprobantes.Agencia EQ Tmp.Cage AND
     Comprobantes.Comprobante EQ W_Cbt NO-LOCK NO-ERROR.
IF AVAILABLE Comprobantes THEN DO:
   IF Comprobantes.Id_Consecutivo LT 3 THEN DO:
       W_Doc:SCREEN-VALUE = "00000000".
       DISABLE W_Doc WITH FRAME F-Main.
   END.
   ELSE 
      ENABLE W_Doc WITH FRAME F-Main.
END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_TX = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_OK = Cmb_Agencia:ADD-LAST(W_TX).
      IF W_Agencia EQ Agencias.Agencia THEN
         Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre.
  END. 

  RUN Llenar_NumCBT.
*/
  DISABLE Btn_Conta WITH FRAME F-main.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  APPLY "Leave" TO BUTTON-3 IN FRAME f-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*CAge/CNit/CCue/CNat/Cval/CCom*/
    DEFINE VAR WswAge AS LOGICAL INITIAL FALSE.
    DEFINE VAR wmsg   AS CHARACTER FORMAT "X(80)".
    DEFINE VAR IDeb LIKE Mov_Contable.CR.
    DEFINE VAR ICre LIKE Mov_Contable.CR.
    DEFINE VAR TIDeb LIKE Mov_Contable.CR.
    DEFINE VAR TICre LIKE Mov_Contable.CR.
    DEFINE VAR INom  AS CHARACTER FORMAT "X(18)".
    DEFINE VAR wage  AS CHARACTER FORMAT "X(25)".

{INCLUIDO\RepEncabezado.I}.    
    IF zswsalida THEN DO:
      W_Reporte    = "REPORTE   : INCONSISTENCIAS IMPORTACION   - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = "DETALLE DEL ERROR ENCONTRADO EN EL ARCHIVO PLANO - VALIDE LA LINEA DEL ARCHIVO PLANO ".
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-Ftr.
      FOR EACH Tmpincons:
         DISPLAY Tmpincons.Terror WITH FRAME F-ERR USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS. 
      END.
      PUT " " SKIP(2).
      DISABLE Btn_Conta WITH FRAME F-main.
      PAGE.
    END.
    ELSE
      ENABLE Btn_Conta WITH FRAME F-main.

  W_Reporte    = "REPORTE   : PRUEBA CONTABILIZACION NOMINA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE CUENTA          COMENTARIO                     NIT          NOMBRE               DEBITO              CREDITO".
  wmsg = "Descuadre en Agencias: ".
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  DEFINE VAR WAdb AS DECIMAL INITIAL 0.
  DEFINE VAR WAcr AS DECIMAL INITIAL 0.
  DEFINE VAR wlin AS INTEGER INITIAL 0.
  FOR EACH Tmp BREAK BY Tmp.Cage:
    wlin = wlin + 1.
    IF FIRST-OF(tmp.Cage) THEN
       ASSIGN WAdb = 0    WAcr = 0.

    IF Tmp.CDb NE 0 THEN
       ASSIGN IDeb  = Tmp.Cdb
              TIDeb = TIDeb + IDeb 
              ICre  = 0
              WAdb  = WAdb + Tmp.Cdb.
    ELSE
        ASSIGN ICre = Tmp.Ccr
               TICre = TICre + ICre 
               IDeb = 0
               WAcr  = WAcr + Tmp.Ccr.
    FIND FIRST clientes WHERE clientes.nit = Tmp.Cnit NO-LOCK NO-ERROR.
    IF AVAILABLE(clientes) THEN  Inom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE Inom = "No Encontro Nombre".

    DISPLAY Tmp.Cage    AT 1   FORMAT "999"
            Tmp.CCue    AT 5   FORMAT "X(12)"
            Tmp.CCom    AT 20  FORMAT "X(29)"
            Tmp.CNit    AT 52  FORMAT "X(12)"
            INom        AT 66  FORMAT "X(18)"
            IDeb        AT 87  FORMAT "->>>,>>>,>>9.99"
            ICre        AT 104 FORMAT "->>>,>>>,>>9.99"
    WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.

    IF LAST-OF(Tmp.Cage) THEN DO:
        FIND FIRST agencia WHERE agencias.agencia = tmp.Cage NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias) THEN wage = agencias.nombre.
        ELSE wage = " No identificada".
             
        DISPLAY "Total Agencia : " wage   AT 10
                "-------------------------------------------" AT 82
                WAdb         AT 82  FORMAT "->>,>>>,>>>,>>9.99" 
                WAcr         AT 102 FORMAT "->>,>>>,>>>,>>9.99" 
            WITH FRAME F-Totage USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
            PUT " " SKIP(1).
        IF WAdb NE WAcr THEN DO:
          ASSIGN WswAge = TRUE
                 Wmsg = wmsg + "  Ag: " + STRING(Tmp.Cage,"999").
          PUT "Descuadre agencia: " STRING(Tmp.Cage,"999") "  Valor Descuadre: " (WAdb - WAcr) FORMAT "-zzz,zzz,zzz,zz9.99" SKIP(2).
        END.



    END.

  END.
  DISPLAY "-------------------------------------------" AT 82
          TIDeb         AT 82   FORMAT "->>,>>>,>>>,>>9.99" 
          TICre         AT 102  FORMAT "->>,>>>,>>>,>>9.99" 
  WITH FRAME F-Tot USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
  PAGE.

  ASSIGN CrePan = TCre
         DebPan = TDeb
         DifPan = TCre - TDeb
         CrePan:SCREEN-VALUE IN FRAME f-main = string(TCre)
         DebPan:SCREEN-VALUE = string(TDeb)
         DifPan:SCREEN-VALUE = string(TCre - TDeb).
  DISPLAY Crepan DebPan DifPan WITH FRAME F-Main.
  IF DifPan = 0 THEN btn_Conta:VISIBLE = TRUE.
  IF wswage THEN DO: 
     DISABLE Btn_Conta WITH FRAME F-main.
     MESSAGE "Existen descuadres por agencia ... NO SE DEBE REALIZAR LA IMPORTACION"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE
     IF NOT zswsalida THEN
        ENABLE Btn_Conta WITH FRAME F-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rutina_Imprimir W-Win 
PROCEDURE Rutina_Imprimir :
DEFINE INPUT PARAMETER P_CompAct       LIKE Comprobantes.comprobante.
    DEFINE INPUT PARAMETER P_OfiOrigen     LIKE Agencias.Agencia.
    DEFINE INPUT PARAMETER P_DocIni        LIKE Comprobantes.Secuencia.
    DEFINE INPUT PARAMETER P_DocFin        LIKE Comprobantes.Secuencia.
    FIND Comprobantes WHERE Comprobantes.Comprobante EQ P_CompAct 
                        AND Comprobantes.Agencia     EQ P_OfiOrigen
                    NO-LOCK NO-ERROR.        
    IF AVAILABLE Comprobantes THEN DO:
       FIND Formatos WHERE Formatos.Agencia     EQ P_OfiOrigen
                       AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato 
                   NO-LOCK NO-ERROR.
       IF AVAILABLE(Formatos) THEN
          RUN VALUE(Formatos.Nom_Proceso) (INPUT P_CompAct,
                                           INPUT P_DocIni, INPUT P_DocFin,
                                           INPUT P_OfiOrigen).
       ELSE
         RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
    END.
    ELSE 
      RUN MostrarMensaje IN W_Manija (INPUT 268, OUTPUT W_Rpta).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

