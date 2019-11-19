&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProCre 
CREATE WIDGET-POOL.

/* ***************************  Definitions  *******************************
 *  Programa: "W_Panest.I                                                  *
 *  Descripcion: Se encarga de mostrar la pantalla de estadísticas para    *
 *               clientes o para productos.                                *
 *                                                                         *
 *  Parámetros: {1} toma el valor de 0 no producto, diferente 0 Pide Pctos *
 ***************************************************************************/



/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  DEFINE VAR W_IOfi    LIKE Oficinas.Oficina.
  DEFINE VAR W_FOfi    LIKE Oficinas.Oficina.
  DEFINE VAR W_Ifec    AS DATE FORMAT "99/99/9999".
  DEFINE VAR W_Ffec    AS DATE FORMAT "99/99/9999".
  DEFINE VAR W_Ilin    LIKE Pro_Creditos.Cod_Producto.  
  DEFINE VAR W_Flin    LIKE Pro_Creditos.Cod_Producto.
  DEFINE VAR W_OpcInforme AS CHARACTER.
  DEFINE VAR Procname AS CHARACTER.


  DEFINE VAR W_WidSel             AS HANDLE.
  DEFINE VAR W_RoficnaAnt         AS INTEGER FORMAT "9" INITIAL 0.
  DEFINE VAR W_Rpta               AS LOGICAL.
  DEFINE VAR W_Ofini              LIKE Oficina.Oficina INITIAL 0.
  DEFINE VAR W_Offin              LIKE Oficina.Oficina INITIAL 0.
  DEFINE VAR W_Linini             LIKE Pro_Creditos.Cod_Producto INITIAL 0.
  DEFINE VAR W_Linfin             LIKE Pro_Creditos.Cod_Producto INITIAL 0.
  DEFINE VAR W_Metodo             AS   LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 W_cmbOfini W_CmbOffin W_Fecini ~
W_FecFin W_Cmblinini W_cmbLinFin RECT-17 RECT-5 Btn_Cancelar Btn_Impresion ~
Btn_Ayuda 
&Scoped-Define DISPLAYED-OBJECTS W_TitInf W_cmbOfini W_CmbOffin W_Fecini ~
W_FecFin W_Cmblinini W_cmbLinFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProCre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_f-titulo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "adeicon/cuehelp":U
     LABEL "" 
     SIZE 4 BY 1.08
     FONT 9.

DEFINE BUTTON Btn_Cancelar 
     LABEL "S&alir" 
     SIZE 11 BY 1.08.

DEFINE BUTTON Btn_Consulta 
     LABEL "Consultar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON Btn_Impresion 
     LABEL "I&mprimir" 
     SIZE 11 BY 1.08
     FONT 15.

DEFINE VARIABLE W_cmbLinFin AS CHARACTER FORMAT "X(35)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE W_Cmblinini AS CHARACTER FORMAT "X(35)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbOffin AS CHARACTER FORMAT "X(25)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE W_cmbOfini AS CHARACTER FORMAT "X(25)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE W_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE W_Fecini AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE W_TitInf AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .81
     BGCOLOR 3 FGCOLOR 15 FONT 12 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 9.42.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 1.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_TitInf AT ROW 3.15 COL 5 COLON-ALIGNED NO-LABEL
     W_cmbOfini AT ROW 6.12 COL 23 COLON-ALIGNED HELP
          "Seleccione el Rango Inicial para la consulta"
     W_CmbOffin AT ROW 6.12 COL 54 COLON-ALIGNED HELP
          "Seleccione el Rango Final para la consulta"
     W_Fecini AT ROW 8.54 COL 29 COLON-ALIGNED HELP
          "Seleccione el Rango Inicial para la consulta" AUTO-RETURN 
     W_FecFin AT ROW 8.54 COL 54 COLON-ALIGNED HELP
          "Seleccione el Rango Final para la consulta" AUTO-RETURN 
     W_Cmblinini AT ROW 11.23 COL 22 COLON-ALIGNED HELP
          "Seleccione el Rango Inicial para la consulta"
     W_cmbLinFin AT ROW 11.23 COL 52 COLON-ALIGNED HELP
          "Seleccione el Rango Final para la consulta"
     Btn_Cancelar AT ROW 16.35 COL 11 HELP
          "Permite Regresar a la opción anterior"
     Btn_Consulta AT ROW 16.35 COL 60
     Btn_Impresion AT ROW 16.35 COL 71 HELP
          "Permite Generar e Imprimir informes"
     Btn_Ayuda AT ROW 16.35 COL 82 HELP
          "Muestra la Ayuda pertinente a la pantalla"
     RECT-16 AT ROW 4.77 COL 17
     "Selección de Rangos de Oficinas" VIEW-AS TEXT
          SIZE 57 BY .62 AT ROW 5.04 COL 18
          BGCOLOR 7 FGCOLOR 15 
     "Rango de Fechas" VIEW-AS TEXT
          SIZE 57 BY .62 AT ROW 7.46 COL 18
          BGCOLOR 7 FGCOLOR 15 
     "Lineas de Crédito" VIEW-AS TEXT
          SIZE 57 BY .62 AT ROW 10.15 COL 18
          BGCOLOR 7 FGCOLOR 15 
     "Text 4" VIEW-AS TEXT
          SIZE 57 BY .54 AT ROW 13.12 COL 18
          BGCOLOR 7 FGCOLOR 7 
     RECT-17 AT ROW 16.08 COL 10
     RECT-5 AT ROW 16.08 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.43 BY 17.62
         FONT 4
         CANCEL-BUTTON Btn_Cancelar.


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
  CREATE WINDOW W-ProCre ASSIGN
         HIDDEN             = YES
         TITLE              = "Procesos Estadísticos - Parámetros"
         HEIGHT             = 16.69
         WIDTH              = 91.43
         MAX-HEIGHT         = 17.62
         MAX-WIDTH          = 91.43
         VIRTUAL-HEIGHT     = 17.62
         VIRTUAL-WIDTH      = 91.43
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProCre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TitInf IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
THEN W-ProCre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-ProCre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON END-ERROR OF W-ProCre /* Procesos Estadísticos - Parámetros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON WINDOW-CLOSE OF W-ProCre /* Procesos Estadísticos - Parámetros */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ProCre
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
   RUN Ayuda.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-ProCre
ON CHOOSE OF Btn_Cancelar IN FRAME F-Main /* Salir */
DO:
  W_OpcInforme = "".
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  IF W_Ofini > W_Offin THEN
   DO:
     RUN MostrarMensaje IN W_Manija (INPUT 68, OUTPUT W_Eleccion).
     APPLY "ENTRY" TO W_CmbOfIni.
     RETURN NO-APPLY.
   END.
  IF INPUT W_FecIni > INPUT W_FecFin THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 68, OUTPUT W_Eleccion).
    APPLY "ENTRY" TO W_FecIni.
    RETURN NO-APPLY.
  END.
  IF W_Linini > W_Linfin THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 68, OUTPUT W_Eleccion).
    APPLY "ENTRY" TO W_CmbLinIni.
    RETURN NO-APPLY.
  END.

  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Procname,INPUT-OUTPUT W_OpcInforme).

  IF W_OpcInforme = "" THEN
     RETURN.

  IF W_OpcInforme = "A" THEN DO:
     SYSTEM-DIALOG GET-FILE Procname
     TITLE "Escoja el archivo"
     FILTERS "Todos los Archivos (*.*)" "*.Txt"
     INITIAL-DIR "w_pathspl".
  END.  
  ASSIGN W_Iofi = W_OfIni
         W_Fofi = W_OfFin
         W_IFec = W_FecIni
         W_FFec = W_FecFin
         W_ILin = W_LinIni
         W_FLin = W_LinFin.
  ASSIGN W_Metodo = SESSION:SET-WAIT-STATE ("GENERAL").
  RUN procesa.
  ASSIGN W_Metodo = SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON ENTRY OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON LEAVE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_cmbLinFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_cmbLinFin W-ProCre
ON LEAVE OF W_cmbLinFin IN FRAME F-Main /* Final */
DO:
 W_WidSel = LAST-EVENT:WIDGET-ENTER.
 IF W_WidSel:NAME = "Btn_Cancelar" THEN
   RETURN.
 IF W_Linini > W_Linfin THEN
  DO:
   RUN MostrarMensaje IN W_Manija (INPUT 68, OUTPUT W_Eleccion).
   RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_cmbLinFin W-ProCre
ON VALUE-CHANGED OF W_cmbLinFin IN FRAME F-Main /* Final */
DO:
  W_Linfin = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.
  ASSIGN W_Linfin = INTEGER(SUBSTRING(SELF:ENTRY(W_Linfin),1,4)).
  IF W_LinIni GT W_LinFin THEN 
     ASSIGN W_LinIni                 = W_LinFin
            W_CmbLinIni:SCREEN-VALUE = W_CmbLinFin:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cmblinini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cmblinini W-ProCre
ON VALUE-CHANGED OF W_Cmblinini IN FRAME F-Main /* Inicial */
DO:
  W_Linini = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.
  ASSIGN W_Linini = INTEGER(SUBSTRING(SELF:ENTRY(W_LinIni),1,4)).
  IF W_Linini GT W_LinFin THEN 
     ASSIGN W_LinFin                 = W_LinIni
            W_CmbLinFin:SCREEN-VALUE = W_CmbLinIni:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOffin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOffin W-ProCre
ON LEAVE OF W_CmbOffin IN FRAME F-Main /* Final */
DO:
 W_WidSel = LAST-EVENT:WIDGET-ENTER.
 IF W_WidSel:NAME = "Btn_Cancelar" THEN
   RETURN.
 IF W_Ofini > W_Offin THEN
  DO:
   RUN MostrarMensaje IN W_Manija (INPUT 68, OUTPUT W_Eleccion).
   RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOffin W-ProCre
ON VALUE-CHANGED OF W_CmbOffin IN FRAME F-Main /* Final */
DO:
  W_OfFin = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.
  ASSIGN W_Offin = INTEGER(SUBSTRING(SELF:ENTRY(W_OfFin),1,3)).
  IF W_OfIni GT W_OfFin THEN 
     ASSIGN W_OfIni                 = W_OfFin
            W_CmbOfIni:SCREEN-VALUE = W_CmbOfFin:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_cmbOfini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_cmbOfini W-ProCre
ON VALUE-CHANGED OF W_cmbOfini IN FRAME F-Main /* Inicial */
DO:
  W_OfIni = SELF:LOOKUP(SELF:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.
  ASSIGN W_OfIni = INTEGER(SUBSTRING(SELF:ENTRY(W_OfIni),1,3)).
  IF W_OfIni GT W_OfFin THEN 
     ASSIGN W_OfFin                 = W_OfIni
            W_CmbOfFin:SCREEN-VALUE = W_CmbOfIni:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFin W-ProCre
ON LEAVE OF W_FecFin IN FRAME F-Main /* Hasta */
DO:
  ASSIGN W_FecFin.
  IF W_FecIni GT W_FecFin THEN 
     ASSIGN W_FecIni                 = W_FecFin
            W_FecIni:SCREEN-VALUE    = W_FecFin:SCREEN-VALUE.
  W_WidSel = LAST-EVENT:WIDGET-ENTER.
  IF W_WidSel:NAME = "Btn_Cancelar" THEN
   RETURN.
  IF INPUT W_FecIni > INPUT W_FecFin THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 68, OUTPUT W_Eleccion).
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Fecini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Fecini W-ProCre
ON LEAVE OF W_Fecini IN FRAME F-Main /* Desde */
DO:
  ASSIGN W_FecIni.
  IF W_FecIni GT W_FecFin THEN 
       ASSIGN W_FecFin                 = W_FecIni
              W_FecFin:SCREEN-VALUE    = W_FecIni:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProCre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-ProCre _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'f-titulo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-titulo ).
       RUN set-position IN h_f-titulo ( 1.00 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 88.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-titulo ,
             W_TitInf:HANDLE IN FRAME F-Main , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-ProCre _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProCre _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
  THEN DELETE WIDGET W-ProCre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProCre _DEFAULT-ENABLE
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
  DISPLAY W_TitInf W_cmbOfini W_CmbOffin W_Fecini W_FecFin W_Cmblinini 
          W_cmbLinFin 
      WITH FRAME F-Main IN WINDOW W-ProCre.
  ENABLE RECT-16 W_cmbOfini W_CmbOffin W_Fecini W_FecFin W_Cmblinini 
         W_cmbLinFin RECT-17 RECT-5 Btn_Cancelar Btn_Impresion Btn_Ayuda 
      WITH FRAME F-Main IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-ProCre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-ProCre 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-ProCre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}: 
        W_CmbOfIni:LIST-ITEMS = "".
        FOR EACH Oficinas WHERE Oficinas.Estado <> 2 NO-LOCK:
             W_Rpta = W_CmbOfini:ADD-LAST(STRING(Oficinas.Oficina,"999") + " " + SUBSTRING(Oficinas.nombre,1,20)).
        END.
      
        W_CmbOfFin:LIST-ITEMS = "".
        FOR EACH Oficinas WHERE Oficinas.Estado <> 2 NO-LOCK:
             W_Rpta = W_CmbOffin:ADD-LAST(STRING(Oficinas.Oficina,"999") + " " + SUBSTRING(Oficinas.nombre,1,20)).
        END.
      
        W_CmbLinIni:LIST-ITEMS = "".
        FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado <> 2 NO-LOCK:
             W_Rpta = W_CmbLinIni:ADD-LAST(STRING(Pro_Creditos.Cod_Producto,"9999") + " " + 
                                    SUBSTRING(Pro_Creditos.nom_Producto,1,20)).
        END.
      
        W_CmbLinFin:LIST-ITEMS = "".
        FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado <> 2 NO-LOCK:
             W_Rpta = W_CmbLinfin:ADD-LAST(STRING(Pro_Creditos.Cod_Producto,"9999") + " " + 
                                    SUBSTRING(Pro_Creditos.nom_Producto,1,20)).
        END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}: 
    IF W_CmbOffin:NUM-ITEMS > 0 THEN 
      ASSIGN W_CmbOfFin:SCREEN-VALUE =  W_CmbOfFin:ENTRY(1)
             W_OfFin  = INTEGER(SUBSTRING(W_CmbofFin:SCREEN-VALUE,1,3)).
  
    IF W_CmbOfIni:NUM-ITEMS > 0 THEN 
      ASSIGN W_CmbOfIni:SCREEN-VALUE  =  W_CmbOfIni:ENTRY(1)
             W_Ofini  = INTEGER(SUBSTRING(W_Cmbofini:SCREEN-VALUE,1,3)).
  
    IF W_CmbLinfin:NUM-ITEMS > 0 THEN 
      ASSIGN W_CmbLinFin:SCREEN-VALUE = W_CmbLinFin:ENTRY(1).
             W_LinFin = INTEGER(SUBSTRING(W_CmbLinFin:SCREEN-VALUE,1,4)).    
  
    IF W_CmbLinIni:NUM-ITEMS > 0 THEN 
      ASSIGN W_CmbLinIni:SCREEN-VALUE = W_CmbLinIni:ENTRY(1)
             W_Linini = INTEGER(SUBSTRING(W_CmbLinIni:SCREEN-VALUE,1,4)).
  
    DISPLAY TODAY          @ W_Fecini. 
    DISPLAY TODAY          @ W_FecFin. 

    ASSIGN W_FecIni = Today
           W_FecFin = Today. 
    IF {1} = 0 THEN
      DISABLE W_CmbLinIni W_CmbLinFin.
    ELSE
     ENABLE W_CmbLinIni   w_CmbLinFin.
    w_TitInf = "{2}".
    DISPLAY W_TitInf WITH FRAME {&FRAME-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-ProCre _ADM-SEND-RECORDS
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-ProCre 
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


