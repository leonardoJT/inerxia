&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

DEFINE VAR W_Ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN
&Scoped-define BROWSE-NAME brwConceptos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cfg_retenciones

/* Definitions for BROWSE brwConceptos                                  */
&Scoped-define FIELDS-IN-QUERY-brwConceptos cfg_retenciones.cuenta cfg_retenciones.concepto ~
cfg_retenciones.tipo cfg_retenciones.base cfg_retenciones.porcentaje 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwConceptos 
&Scoped-define QUERY-STRING-brwConceptos FOR EACH cfg_retenciones SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwConceptos OPEN QUERY brwConceptos FOR EACH cfg_retenciones SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwConceptos cfg_retenciones
&Scoped-define FIRST-TABLE-IN-QUERY-brwConceptos cfg_retenciones


/* Definitions for FRAME F-MAIN                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-MAIN ~
    ~{&OPEN-QUERY-brwConceptos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cuenta_fill Btn_Deshacer Btn-Guardar ~
Btn-Eliminar Tipo_fill Concepto_fill Base_fill Porcentaje_fill Btn-Salvar ~
Btn_Done Btn-Editar RECT-325 
&Scoped-Define DISPLAYED-OBJECTS Cuenta_fill Tipo_fill Concepto_fill ~
Base_fill Porcentaje_fill 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Editar 
     IMAGE-UP FILE "imagenes/modify.jpg":U
     LABEL "btnagregar 2" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn-Eliminar 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/eliminar.jpg":U
     LABEL "Eliminar" 
     SIZE 8 BY 2.15.

DEFINE BUTTON Btn-Guardar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "" 
     SIZE 7 BY 1.62.

DEFINE BUTTON Btn-Salvar 
     IMAGE-UP FILE "imagenes/agregar.jpg":U
     LABEL "Salvar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Deshacer 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Prog/Imagenes/deshacer.bmp":U
     LABEL "Cancelar" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE Tipo_fill AS CHARACTER FORMAT "x(25)" 
     LABEL "Tipo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Salarios","SAL",
                     "Honorarios","HON",
                     "Servicios","SRV",
                     "Arrendamientos","ARR",
                     "Redimientos financieros","RENFIN",
                     "Compras","CMP",
                     "Impuesto a las ventas","IVA",
                     "Industria y Comercio","INDCOM",
                     "Otras","OTR"
     DROP-DOWN-LIST
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE Base_fill AS DECIMAL FORMAT ">>>,>>>,>>9.99" INITIAL 0 
     LABEL "Base" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Concepto_fill AS CHARACTER FORMAT "x(30)" 
     LABEL "Concepto" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE Cuenta_fill AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cuenta contable" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.08 NO-UNDO.

DEFINE VARIABLE Porcentaje_fill AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     LABEL "Porcentaje" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110.14 BY 26.92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwConceptos FOR 
      cfg_retenciones SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwConceptos C-Win _STRUCTURED
  QUERY brwConceptos SHARE-LOCK NO-WAIT DISPLAY
      cfg_retenciones.cuenta COLUMN-LABEL "Cuenta" FORMAT "x(15)":U
            LABEL-BGCOLOR 10
      cfg_retenciones.concepto COLUMN-LABEL "Concepto" FORMAT "x(30)":U
            LABEL-BGCOLOR 10
      cfg_retenciones.tipo COLUMN-LABEL "Tipo" FORMAT "x(25)":U
            LABEL-BGCOLOR 10 VIEW-AS COMBO-BOX INNER-LINES 15
                      LIST-ITEM-PAIRS "Salarios"," SAL",
                                      "Honorarios"," HON",
                                      "Servicios"," SRV",
                                      "Arrendamientos"," ARR",
                                      "Redimientos financieros"," RENFIN",
                                      "Compras"," CMP",
                                      "Impuesto a las ventas"," IVA",
                                      "Industria y Comercio"," INDCOM",
                                      "Otras"," OTR"
                      DROP-DOWN-LIST 
      cfg_retenciones.base COLUMN-LABEL "Base" FORMAT ">>>,>>>,>>9.99":U
            LABEL-BGCOLOR 10
      cfg_retenciones.porcentaje COLUMN-LABEL "Porcentaje" FORMAT ">>9.9999":U
            LABEL-BGCOLOR 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 21.54 ROW-HEIGHT-CHARS .61 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     brwConceptos AT ROW 8.27 COL 3 WIDGET-ID 200
     Cuenta_fill AT ROW 3.69 COL 40 COLON-ALIGNED WIDGET-ID 148
     Btn_Deshacer AT ROW 6.96 COL 113.86 WIDGET-ID 146
     Btn-Guardar AT ROW 3.96 COL 101 WIDGET-ID 144
     Btn-Eliminar AT ROW 8.92 COL 113.86 WIDGET-ID 142
     Tipo_fill AT ROW 6.38 COL 17 COLON-ALIGNED WIDGET-ID 140
     Concepto_fill AT ROW 5.04 COL 17 COLON-ALIGNED WIDGET-ID 130
     Base_fill AT ROW 3.69 COL 81 COLON-ALIGNED WIDGET-ID 134
     Porcentaje_fill AT ROW 5.04 COL 81 COLON-ALIGNED WIDGET-ID 136
     Btn-Salvar AT ROW 3.12 COL 113.86 WIDGET-ID 42
     Btn_Done AT ROW 27.88 COL 113.86 WIDGET-ID 38
     Btn-Editar AT ROW 5.08 COL 113.86 WIDGET-ID 16
     "               Administración de la configuración para la Retención en la Fuente" VIEW-AS TEXT
          SIZE 120.72 BY 1.08 AT ROW 1.46 COL 2 WIDGET-ID 8
          BGCOLOR 3 
     RECT-325 AT ROW 2.88 COL 1.86 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123 BY 28.81 WIDGET-ID 100.


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
         TITLE              = "Configuración Parámeros Generales de Créditos"
         HEIGHT             = 28.81
         WIDTH              = 123
         MAX-HEIGHT         = 32.35
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 32.35
         VIRTUAL-WIDTH      = 146.29
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FRAME F-MAIN
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brwConceptos 1 F-MAIN */
/* SETTINGS FOR BROWSE brwConceptos IN FRAME F-MAIN
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwConceptos
/* Query rebuild information for BROWSE brwConceptos
     _TblList          = "bdcentral.cfg_retenciones"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.cfg_retenciones.concepto
"cfg_retenciones.concepto" "Concepto" ? "character" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.cfg_retenciones.tipo
"cfg_retenciones.tipo" "Tipo" ? "character" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "DROP-DOWN-LIST" "," ? "Salarios, SAL,Honorarios, HON,Servicios, SRV,Arrendamientos, ARR,Redimientos financieros, RENFIN,Compras, CMP,Impuesto a las ventas, IVA,Industria y Comercio, INDCOM,Otras, OTR" 15 no 0 no no
     _FldNameList[3]   > bdcentral.cfg_retenciones.base
"cfg_retenciones.base" "Base" ? "decimal" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.cfg_retenciones.porcentaje
"cfg_retenciones.porcentaje" "Porcentaje" ? "decimal" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwConceptos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración Parámeros Generales de Créditos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración Parámeros Generales de Créditos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Editar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Editar C-Win
ON CHOOSE OF Btn-Editar IN FRAME F-MAIN /* btnagregar 2 */
DO:
  APPLY "Mouse-Select-Click" TO brwConceptos.
  IF AVAIL(cfg_retenciones) THEN DO:
     ASSIGN Concepto_fill:SCREEN-VALUE = STRING(cfg_retenciones.concepto)
           Tipo_fill:SCREEN-VALUE = STRING(cfg_retenciones.tipo)
           Base_fill:SCREEN-VALUE = STRING(cfg_retenciones.base)
           Porcentaje_fill:SCREEN-VALUE = STRING(cfg_retenciones.porcentaje)
           Cuenta_fill:SCREEN-VALUE = STRING(cfg_retenciones.cuenta).
     ASSIGN 
       Btn-Guardar:HIDDEN IN FRAME F-MAIN           = FALSE.
       DISABLE Btn-Salvar WITH FRAME F-MAIN.
  END.
  ELSE MESSAGE "No existe el registro seleccionado para modificar" SKIP
               VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Eliminar C-Win
ON CHOOSE OF Btn-Eliminar IN FRAME F-MAIN /* Eliminar */
DO:
  APPLY "Mouse-Select-Click" TO brwConceptos.
  IF AVAIL(cfg_retenciones) THEN DO:
     MESSAGE "¿Desea eliminar el registro seleccionado?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar eliminación"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN. 
     
     DELETE cfg_retenciones.
     OPEN QUERY brwConceptos FOR EACH cfg_retenciones SHARE-LOCK INDEXED-REPOSITION.
  END.
  ELSE MESSAGE "No existe el registro seleccionado para eliminarlo" SKIP
               VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Guardar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Guardar C-Win
ON CHOOSE OF Btn-Guardar IN FRAME F-MAIN
DO:
    IF AVAIL(cfg_retenciones) THEN DO:
        RUN Validar_campos.
        MESSAGE "¿Desea cambiar el registro seleccionado?" SKIP
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmación de cambio"
         UPDATE W_RptaRet AS LOGICAL.
        IF NOT W_RptaRet THEN RETURN.
        ASSIGN cfg_retenciones.concepto = STRING(Concepto_fill:SCREEN-VALUE)
           cfg_retenciones.tipo = STRING(Tipo_fill:SCREEN-VALUE)
           cfg_retenciones.base = dec(Base_fill:SCREEN-VALUE)
           cfg_retenciones.porcentaje = dec(Porcentaje_fill:SCREEN-VALUE)
           cfg_retenciones.cuenta = STRING(Cuenta_fill:SCREEN-VALUE).
       ASSIGN Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE.
       ENABLE Btn-Salvar WITH FRAME F-MAIN.
       RUN Inicializar_Variables.
       OPEN QUERY brwConceptos FOR EACH cfg_retenciones SHARE-LOCK INDEXED-REPOSITION.
    END.
    ELSE MESSAGE "No existe el registro seleccionado para modificar" SKIP
               VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salvar C-Win
ON CHOOSE OF Btn-Salvar IN FRAME F-MAIN /* Salvar */
DO: 
    CREATE cfg_retenciones.
    RUN Validar_campos.
    MESSAGE "¿Desea guardar el registro ingresado?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar registro"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN.

    ASSIGN cfg_retenciones.concepto = STRING(Concepto_fill:SCREEN-VALUE)
           cfg_retenciones.tipo = STRING(Tipo_fill:SCREEN-VALUE)
           cfg_retenciones.base = dec(Base_fill:SCREEN-VALUE)
           cfg_retenciones.porcentaje = dec(Porcentaje_fill:SCREEN-VALUE)
           cfg_retenciones.cuenta = STRING(Cuenta_fill:SCREEN-VALUE).
        
    OPEN QUERY brwConceptos FOR EACH cfg_retenciones SHARE-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer C-Win
ON CHOOSE OF Btn_Deshacer IN FRAME F-MAIN /* Cancelar */
DO:
  RUN Inicializar_Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-MAIN /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwConceptos
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

    brwConceptos:SENSITIVE = TRUE.
    ASSIGN 
       Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE.

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
  DISPLAY Cuenta_fill Tipo_fill Concepto_fill Base_fill Porcentaje_fill 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE Cuenta_fill Btn_Deshacer Btn-Guardar Btn-Eliminar Tipo_fill 
         Concepto_fill Base_fill Porcentaje_fill Btn-Salvar Btn_Done Btn-Editar 
         RECT-325 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
VIEW FRAME F-Main.

DO WITH FRAME f-main:
     ASSIGN Concepto_fill:SCREEN-VALUE = ""
           Tipo_fill:SCREEN-VALUE = "OTR"
           Base_fill:SCREEN-VALUE = ""
           Porcentaje_fill:SCREEN-VALUE = "".
     ASSIGN Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE.
     ENABLE Btn-Salvar WITH FRAME F-MAIN.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_campos C-Win 
PROCEDURE Validar_campos :
DO WITH FRAME f-main:
        IF Concepto_fill:SCREEN-VALUE = "" OR Tipo_fill:SCREEN-VALUE = " " OR dec(Base_fill:SCREEN-VALUE) = 0 OR dec(Porcentaje_fill:SCREEN-VALUE) = 0 OR Cuenta_fill:SCREEN-VALUE = "" THEN DO:
            MESSAGE "Debe ingresar todos los campos de la tabla."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN ERROR.
        END.
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = STRING(Cuenta_fill:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL(Cuentas) THEN DO:
            MESSAGE "El número de cuenta no existe. Verifique."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN ERROR.
        END.
        
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

