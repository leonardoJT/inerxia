&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{incluido\Variable.i "SHARED"}

DEFINE VARIABLE W_Rpta AS LOGICAL.
DEFINE VAR W_Cbt AS INTEGER INITIAL 4.
DEFINE VAR W_CtaDeb AS CHARACTER INITIAL "27209501".
DEFINE VAR W_CtaCre AS CHARACTER INITIAL "31050501".
DEFINE VAR W_Cont AS INTEGER.
DEFINE VAR j AS INTEGER.
DEFINE VAR k AS INTEGER.

DEFINE TEMP-TABLE Tmp
    FIELD CAge AS INTEGER
    FIELD CNit AS CHARACTER FORMAT "X(11)"
    FIELD CCuo AS DECIMAL.

DEFINE TEMP-TABLE TmpMov LIKE Mov_Contable.
DEFINE TEMP-TABLE TmpAho LIKE Mov_Ahorros.

DEFINE VAR TDeb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9". 
DEFINE VAR TCre AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE procname AS CHARACTER.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.
DEFINE VAR W_TX AS CHARACTER FORMAT "X(25)".
DEFINE VAR W_OK AS LOGICAL.
DEFINE VAR W_OKTmp AS LOGICAL INITIAL YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* oakley */

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_agencia BUTTON-3 Preview Btn_Done ~
RECT-322 
&Scoped-Define DISPLAYED-OBJECTS Cmb_agencia W_Doc 

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
     SIZE 50 BY 1.12.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-3 
     LABEL "Leer Archivo Plano" 
     SIZE 50 BY 1.12.

DEFINE BUTTON Preview 
     LABEL "Revisar comprobante sin contabilzar" 
     SIZE 50 BY 1.12.

DEFINE VARIABLE Cmb_agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia de Cargue" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Doc AS INTEGER FORMAT "9999999":U INITIAL 0 
     LABEL "Número de documento a contabilizar" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 52 BY 1.42
     BGCOLOR 18 .

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Cmb_agencia AT ROW 2.88 COL 27 COLON-ALIGNED
     W_Doc AT ROW 3.96 COL 43 COLON-ALIGNED
     BUTTON-3 AT ROW 5.04 COL 4
     Preview AT ROW 6.46 COL 4
     Btn_Conta AT ROW 8 COL 4
     Btn_Done AT ROW 9.35 COL 46
     RECT-322 AT ROW 1.27 COL 3
     "Cbte Traslado de Aportes Diferidos a Aportes Ordinarios" VIEW-AS TEXT
          SIZE 49.29 BY .92 AT ROW 1.54 COL 4.86
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.14 BY 21.19
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.27 COL 21
     R1 AT ROW 1.27 COL 2
     R2 AT ROW 1.27 COL 4
     R3 AT ROW 1.27 COL 6
     R4 AT ROW 1.27 COL 8
     R5 AT ROW 1.27 COL 10
     R6 AT ROW 1.27 COL 12
     R7 AT ROW 1.27 COL 14
     R8 AT ROW 1.27 COL 16
     R9 AT ROW 1.27 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 9.35
         SIZE 26 BY 1.88
         BGCOLOR 17 .


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
         HEIGHT             = 10.58
         WIDTH              = 56.43
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 112.43
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 112.43
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
/* REPARENT FRAME */
ASSIGN FRAME F_Progreso:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON Btn_Conta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Doc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

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
  ASSIGN j = 0.
  RUN Generar_Movimiento.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
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


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Leer Archivo Plano */
DO:
  ASSIGN J = 0.
  VIEW FRAME F_Progreso.
  FOR EACH Tmp: DELETE Tmp. END.
  ASSIGN FRAME F-MAIN W_Doc.
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
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Preview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Preview W-Win
ON CHOOSE OF Preview IN FRAME F-Main /* Revisar comprobante sin contabilzar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  /*{Imprimir.I "listado"}*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Doc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Doc W-Win
ON LEAVE OF W_Doc IN FRAME F-Main /* Número de documento a contabilizar */
DO:
  FIND FIRST Mov_Contable WHERE
             Mov_Contable.Agencia     EQ INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)) AND
             Mov_Contable.Comprobante EQ W_Cbt AND
             Mov_Contable.Num_Documento EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE(Mov_Contable) THEN DO:
     MESSAGE "Este número de documento ya existe dentro" SKIP
             "de la contabilidad. Intente otro número" VIEW-AS ALERT-BOX.
     SELF:SCREEN-VALUE = "00000000".
     RETURN NO-APPLY.
  END.
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
  DISPLAY Cmb_agencia W_Doc 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE Cmb_agencia BUTTON-3 Preview Btn_Done RECT-322 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_MovConTmp W-Win 
PROCEDURE Generar_MovConTmp :
/*debito*/
CREATE TmpMov.
ASSIGN TmpMov.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
       TmpMov.Cen_Costos = 999
       TmpMov.Cuenta = W_CtaDeb
       TmpMov.Comprobante = W_Cbt /*nota contable*/
       TmpMov.Fec_Contable = W_Fecha
       TmpMov.Fec_Grabacion = W_Fecha
       TmpMov.Num_Documento = W_Doc
       TmpMov.Doc_Referencia = STRING(W_Doc)
       TmpMov.Nit = TRIM(Tmp.CNit)
       TmpMov.Comentario = "Traslado a Ap.Sociales"
       TmpMov.Usuario = W_Usuario. 
       TmpMov.DB = Tmp.CCuo.

/*credito*/
CREATE TmpMov.
ASSIGN TmpMov.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
       TmpMov.Cen_Costos = 999
       TmpMov.Cuenta = W_CtaCre
       TmpMov.Comprobante = W_Cbt /*nota contable*/
       TmpMov.Fec_Contable = W_Fecha
       TmpMov.Fec_Grabacion = W_Fecha
       TmpMov.Num_Documento = W_Doc
       TmpMov.Doc_Referencia = STRING(W_Doc)
       TmpMov.Nit = TRIM(Tmp.CNit)
       TmpMov.Comentario = "Traslado desde Ap.Diferidos"
       TmpMov.Usuario = W_Usuario. 
       TmpMov.CR = Tmp.CCuo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Movimiento W-Win 
PROCEDURE Generar_Movimiento :

IF W_Doc EQ 0 THEN DO:
   MESSAGE "El numero de docuemento esta en 0" VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.

DO TRANSACTION:
    FOR EACH TmpMov:
        j = j + 1.

        RUN Progreso.
        
        CREATE Mov_Contable.
        BUFFER-COPY TmpMov TO Mov_Contable NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error al crear la partida contable para" SKIP
                    "Cuenta: " TmpMov.Cuenta SKIP
                    "Nit: " TmpMov.Nit SKIP
                    "Valor Db: " TmpMov.Db " Valor Cr: " TmpMov.Cr SKIP(2)
                    "Se cancela el proceso de traslado de aportes!!!"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

 FOR EACH TmpAho:
   ASSIGN j = j + 1.
   RUN Progreso.
   IF TmpAho.Cod_Ahorro EQ 7 THEN DO:
      FIND Ahorros WHERE
           Ahorros.Agencia    EQ TmpAho.Agencia AND
           Ahorros.Tip_Ahorro EQ 4 AND
           Ahorros.Cod_Ahorro EQ 7 AND
           Ahorros.Nit        EQ TmpAho.Nit EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(Ahorros) THEN
         ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - TmpAho.Val_Efectivo
                Ahorros.Fec_UltTransaccion = W_Fecha.
      ELSE DO:
          MESSAGE "El cliente identifiado con nit: " TmpAho.Nit SKIP
                  "no tiene cuenta de aportes diferidos." SKIP(2)
                  "se cancela el proceso de subida de traslados de aportes" VIEW-AS ALERT-BOX.
          RETURN ERROR.
      END.
   END.
   RELEASE Ahorros.
   ASSIGN j = j + 1.
   RUN Progreso.
   IF TmpAho.Cod_Ahorro EQ 5 THEN DO:
         FIND Ahorros WHERE
              Ahorros.Agencia    EQ TmpAho.Agencia AND
              Ahorros.Tip_Ahorro EQ 4 AND
              Ahorros.Cod_Ahorro EQ 5 AND
              Ahorros.Nit        EQ TmpAho.Nit EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE(Ahorros) THEN
            ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + TmpAho.Val_Efectivo
                   Ahorros.Fec_UltTransaccion = W_Fecha.
         ELSE DO:
             MESSAGE "El cliente identifiado con nit: " TmpAho.Nit SKIP
                     "no tiene cuenta de aportes sociales." SKIP(2)
                     "se cancela el proceso de subida de traslados de aportes" VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
   END.
   RELEASE Ahorros.
   CREATE Mov_Ahorros.
   BUFFER-COPY TmpAho TO Mov_Ahorros.
 END.
END. /* TRANSACTION.*/
RUN Rutina_Imprimir (INPUT W_Cbt, INPUT INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)),
                     INPUT W_Doc, INPUT W_Doc).

MESSAGE "Contabilización Realizada" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Temporal W-Win 
PROCEDURE Generar_Temporal :
INPUT FROM VALUE(Procname).
REPEAT:
   CREATE Tmp.
   IMPORT DELIMITER " " tmp.
   IF Tmp.CAge NE INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) THEN DO:
      MESSAGE "La información que se trata de subir pertenece a la agencia: " Tmp.CAge SKIP
              "La agencia seleccionada en el Combo no es igual a la del archivo" SKIP(2)
              "Se cancela el proceso de subida de datos!" VIEW-AS ALERT-BOX.
      FOR EACH Tmp: DELETE Tmp. END.
      RETURN ERROR.
   END.
END.
INPUT CLOSE.

/*CAge/CNit/CCuo*/

EMPTY TEMP-TABLE TmpMov.

ASSIGN W_Cont = 0
       j      = 0.

FOR EACH Tmp WHERE
         Tmp.CAge NE ? AND
         Tmp.CCuo NE ?:
  ASSIGN W_Cont = W_Cont + 1
         j = j + 1.
  RUN Progreso.
  FIND Clientes WHERE Clientes.Nit EQ TRIM(Tmp.CNit) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Clientes) AND Tmp.CNit NE "" THEN DO:
      MESSAGE "El Nit: " Tmp.Cnit "No existe. Rectifique!" VIEW-AS ALERT-BOX.
      W_OkTmp = NO.
  END.
  FIND Ahorros WHERE
       Ahorros.Agencia    EQ Tmp.CAge AND
       Ahorros.Tip_Ahorro EQ 4 AND
       Ahorros.Cod_Ahorro EQ 7 AND
       Ahorros.Nit        EQ Tmp.CNit NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Ahorros) THEN DO:
     MESSAGE "El cliente identifiado con nit: " Tmp.CNit SKIP
             "no tiene cuenta de aportes diferidos." SKIP
             "Linea: " W_Cont " del archivo plano" VIEW-AS ALERT-BOX.
     W_OkTmp = NO.
  END.
  ELSE DO:
     IF Ahorros.Sdo_Disponible EQ 0 THEN DO:
        MESSAGE "El asociado identificado con nit: " Tmp.CNit SKIP
                "no tiene saldo disponible para la transacción" SKIP(1)
                "No se subira este registro al temporal" VIEW-AS ALERT-BOX WARNING.
        NEXT.
     END.
     ELSE
       IF Ahorros.Sdo_Disponible LE Tmp.CCuo THEN
          Tmp.CCuo = Ahorros.Sdo_Disponible.
     RUN GN_MovAhoApDif.
  END.
  FIND Ahorros WHERE
       Ahorros.Agencia    EQ Tmp.CAge AND
       Ahorros.Tip_Ahorro EQ 4 AND
       Ahorros.Cod_Ahorro EQ 5 AND
       Ahorros.Nit        EQ Tmp.CNit NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Ahorros) THEN DO:
     MESSAGE "El cliente identifiado con nit: " Tmp.CNit SKIP
             "no tiene cuenta de aportes sociales." SKIP
             "Linea: " W_Cont " del archivo plano" VIEW-AS ALERT-BOX.
     W_OkTmp = NO.
  END.
  ELSE RUN GN_MovAhoApSoc.
  IF Tmp.CCuo EQ 0 THEN DO:
      MESSAGE "El cliente identifiado con nit: " Tmp.CNit SKIP
              "viene con cuota 0" SKIP
              "Linea: " W_Cont " del archivo plano" VIEW-AS ALERT-BOX.
      W_OkTmp = NO.
  END.
  RUN Generar_MovConTmp.
END.

/* oakley - Para revisar esta parte */

ASSIGN TCre = 0 TDeb = 0.
FOR EACH TmpMov:
    IF TmpMov.CR NE 0 THEN
       TCre = TCre + TmpMov.CR.
    ELSE
       TDeb = TDeb + TmpMov.DB.
END.

IF TCre NE TDeb THEN DO:
   MESSAGE "Débitos y Créditos Diferentes" SKIP
           "Débitos : " TDeb SKIP
           "Créditos: " TCre VIEW-AS ALERT-BOX.
   W_OkTmp = NO.
END.

IF W_OkTmp THEN DO:
    MESSAGE "El temporal fue creado correctamente" VIEW-AS ALERT-BOX INFORMATION.
    ENABLE Btn_Conta WITH FRAME F-main.
END.
ELSE DO:
    DISABLE Btn_Conta WITH FRAME F-Main.
    APPLY "choose" TO preview IN FRAME F-Main.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GN_MovAhoApDif W-Win 
PROCEDURE GN_MovAhoApDif :
/*movimiento debito a apdiferidos x tralado*/
  CREATE TmpAho.
  ASSIGN TmpAho.Cod_Operacion = 010302001
         TmpAho.cod_ahorro    = 7
         TmpAho.Cue_Ahorros   = Ahorros.Cue_Ahorros
         TmpAho.nit           = Tmp.CNit
         TmpAho.Fecha         = W_Fecha
         TmpAho.Hora          = TIME
         TmpAho.Num_Documento = STRING(W_Doc)
         TmpAho.Cpte          = W_Cbt
         TmpAho.Agencia       = Tmp.CAge
         TmpAho.Age_Fuente    = Tmp.CAge
         TmpAho.Age_Destino   = Tmp.CAge
         TmpAho.Usuario       = W_Usuario
         TmpAho.Val_Cheque    = 0
         TmpAho.Val_Efectivo  = Tmp.CCuo
         TmpAho.NomApell      = Clientes.Nombre + " " + Clientes.Apellido1
         TmpAho.Cedula_Trans  = Tmp.CNit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GN_MovAhoApSoc W-Win 
PROCEDURE GN_MovAhoApSoc :
/*movimiento credito a apsociales x tralado*/
  CREATE TmpAho.
  ASSIGN TmpAho.Cod_Operacion = 010301001
         TmpAho.cod_ahorro    = 5
         TmpAho.Cue_Ahorros   = Ahorros.Cue_Ahorros
         TmpAho.nit           = Tmp.CNit
         TmpAho.Fecha         = W_Fecha
         TmpAho.Hora          = TIME
         TmpAho.Num_Documento = STRING(W_Doc)
         TmpAho.Cpte          = W_Cbt
         TmpAho.Agencia       = Tmp.CAge
         TmpAho.Age_Fuente    = Tmp.CAge
         TmpAho.Age_Destino   = Tmp.CAge
         TmpAho.Usuario       = W_Usuario
         TmpAho.Val_Cheque    = 0
         TmpAho.Val_Efectivo  = Tmp.CCuo
         TmpAho.NomApell      = Clientes.Nombre + " " + Clientes.Apellido1
         TmpAho.Cedula_Trans  = Tmp.CNit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_NumCbt W-Win 
PROCEDURE Llenar_NumCbt :
FIND Comprobantes WHERE 
     Comprobantes.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
     Comprobantes.Comprobante EQ W_Cbt NO-LOCK NO-ERROR.
IF AVAILABLE Comprobantes THEN DO:
   IF Comprobantes.Id_Consecutivo LT 3 THEN DO:
       W_Doc:SCREEN-VALUE = "00000000".
       DISABLE W_Doc WITH FRAME F-Main.
   END.
   ELSE 
      ENABLE W_Doc WITH FRAME F-Main.
END.
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
ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_TX = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_OK = Cmb_Agencia:ADD-LAST(W_TX).
      IF W_Agencia EQ Agencias.Agencia THEN
         Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre.
  END.

  RUN Llenar_NumCBT.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*CAge/CNit/CCue/CNat/Cval/CCom*/

    DEFINE VAR IDeb LIKE Mov_Contable.CR.
    DEFINE VAR ICre LIKE Mov_Contable.CR.
    DEFINE VAR TIDeb LIKE Mov_Contable.CR.
    DEFINE VAR TICre LIKE Mov_Contable.CR.
    DEFINE VAR INom AS CHARACTER FORMAT "X(18)".

{INCLUIDO\RepEncabezado.I}    
    W_Reporte    = "REPORTE   : PRUEBA CONTABILIZACION NOMINA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "CUENTA          COMENTARIO                     NIT          NOMBRE               DEBITO              CREDITO".          

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH TmpMov:
    FIND Clientes WHERE Clientes.Nit EQ TmpMov.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN INom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE INom = "Nit no existe en clientes".
    IF TmpMov.Db GT 0 THEN
       ASSIGN IDeb = TmpMov.Db
              TIDeb = TIDeb + IDeb 
              ICre = 0.
    ELSE
       ASSIGN ICre = TmpMov.Cr
              TICre = TICre + ICre 
              IDeb = 0.
    DISPLAY TmpMov.Cuenta      AT 1
            TmpMov.Comentario  AT 17 FORMAT "X(29)"
            TmpMov.Nit         AT 48 FORMAT "X(12)"
            INom        AT 61 FORMAT "X(18)"
            IDeb        AT 82 FORMAT ">>>,>>>,>>>,>>9.99"
            ICre        AT 102 FORMAT ">>>,>>>,>>>,>>9.99"
    WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
  END.
  DISPLAY "-------------------------------------------" AT 82
          TIDeb         AT 82
          TICre         AT 102
  WITH FRAME F-Tot USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso W-Win 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 2 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN
             R1:BGCOL = 18.
          WHEN 2 THEN
             R2:BGCOL = 18.
          WHEN 3 THEN
             R3:BGCOL = 18.
          WHEN 4 THEN
             R4:BGCOL = 18.
          WHEN 5 THEN
             R5:BGCOL = 18.
          WHEN 6 THEN
             R6:BGCOL = 18.
          WHEN 7 THEN
             R7:BGCOL = 18.
          WHEN 8 THEN
             R8:BGCOL = 18.
          WHEN 9 THEN
             R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.

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

