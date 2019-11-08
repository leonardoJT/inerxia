&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_NomPdt AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_Nit    AS CHARACTER FORMAT "X(14)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define BROWSE-NAME B_Valora

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tasas_Mercado

/* Definitions for BROWSE B_Valora                                      */
&Scoped-define FIELDS-IN-QUERY-B_Valora Tasas_Mercado.Cod_Producto W_NomPdt Tasas_Mercado.Nom_TasaUnid Tasas_Mercado.Fec_Crea Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final Tasas_Mercado.Tasa Tasas_Mercado.Valor_Unidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Valora Tasas_Mercado.Tasa Tasas_Mercado.Valor_Unidad   
&Scoped-define ENABLED-TABLES-IN-QUERY-B_Valora Tasas_Mercado
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-B_Valora Tasas_Mercado
&Scoped-define SELF-NAME B_Valora
&Scoped-define OPEN-QUERY-B_Valora FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE, ~
      1, ~
      2)) NO-LOCK NO-ERROR. OPEN QUERY {&SELF-NAME}   FOR EACH Tasas_Mercado WHERE            Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND            Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)   NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Valora Tasas_Mercado
&Scoped-define FIRST-TABLE-IN-QUERY-B_Valora Tasas_Mercado


/* Definitions for FRAME F_Nuevo                                        */
&Scoped-define FIELDS-IN-QUERY-F_Nuevo Tasas_Mercado.Nom_TasaUnid ~
Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Nuevo Tasas_Mercado.Nom_TasaUnid ~
Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Nuevo Tasas_Mercado
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Nuevo Tasas_Mercado
&Scoped-define QUERY-STRING-F_Nuevo FOR EACH Tasas_Mercado SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Nuevo OPEN QUERY F_Nuevo FOR EACH Tasas_Mercado SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Nuevo Tasas_Mercado
&Scoped-define FIRST-TABLE-IN-QUERY-F_Nuevo Tasas_Mercado


/* Definitions for FRAME F_Val                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Val ~
    ~{&OPEN-QUERY-B_Valora}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Tasas_Mercado.Nom_TasaUnid ~
Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final 
&Scoped-define ENABLED-TABLES Tasas_Mercado
&Scoped-define FIRST-ENABLED-TABLE Tasas_Mercado
&Scoped-Define ENABLED-OBJECTS Cmb_TInversiones Cmb_Productos Btn_Salvar ~
BUTTON-62 
&Scoped-Define DISPLAYED-FIELDS Tasas_Mercado.Nom_TasaUnid ~
Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final 
&Scoped-define DISPLAYED-TABLES Tasas_Mercado
&Scoped-define FIRST-DISPLAYED-TABLE Tasas_Mercado
&Scoped-Define DISPLAYED-OBJECTS Cmb_TInversiones Cmb_Productos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 12 BY 1.65.

DEFINE BUTTON BUTTON-62 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 62" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TInversiones AS CHARACTER FORMAT "X(256)":U INITIAL "1 - Negociables" 
     LABEL "Tipos de Inversiones" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Negociables","2 - Permanentes","3 - Disponibles" 
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BtnDone-2 DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 13 BY 1.54.

DEFINE BUTTON BUTTON-50 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 50" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-51 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 51" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-60 
     LABEL "Nuevo" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE Cmb_Entidades AS CHARACTER FORMAT "X(256)":U 
     LABEL "Entidad" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Pcto AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Captaciones", 1,
"Colocaciones", 2,
"Inversiones", 3
     SIZE 46 BY 1.08 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Valora FOR 
      Tasas_Mercado SCROLLING.

DEFINE QUERY F_Nuevo FOR 
      Tasas_Mercado SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Valora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Valora wWin _FREEFORM
  QUERY B_Valora NO-LOCK DISPLAY
      Tasas_Mercado.Cod_Producto FORMAT "999":U    LABEL "CodPdt"
      W_NomPdt FORMAT "X(30)" LABEL "Nombre Pdt"
      Tasas_Mercado.Nom_TasaUnid FORMAT "X(30)":U  LABEL "Descripción"
      Tasas_Mercado.Fec_Crea FORMAT "99/99/9999":U LABEL "Fecha"
      Tasas_Mercado.Monto_Final FORMAT "->>>,>>>,>>>,>>9":U LABEL "Monto"
      Tasas_Mercado.Pla_Final FORMAT ">>,>>9":U    LABEL "Plazo"
      Tasas_Mercado.Tasa FORMAT ">>9.99":U    LABEL "Tasa"
      Tasas_Mercado.Valor_Unidad FORMAT "->>>,>>>,>>9.9999":U  LABEL "Valor"
      ENABLE Tasas_Mercado.Tasa Tasas_Mercado.Valor_Unidad
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 6.73
         BGCOLOR 15 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Val
     Cmb_Entidades AT ROW 1.27 COL 10 COLON-ALIGNED
     R_Pcto AT ROW 1.27 COL 49 NO-LABEL
     BUTTON-49 AT ROW 1.5 COL 98
     B_Valora AT ROW 2.62 COL 3
     BUTTON-50 AT ROW 3 COL 98
     BUTTON-51 AT ROW 4.54 COL 98
     BUTTON-60 AT ROW 6.12 COL 98
     BtnDone-2 AT ROW 7.73 COL 98
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 16.5
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Nuevo
     Cmb_TInversiones AT ROW 1.27 COL 26 COLON-ALIGNED
     Cmb_Productos AT ROW 2.35 COL 26 COLON-ALIGNED
     Tasas_Mercado.Nom_TasaUnid AT ROW 3.42 COL 26 COLON-ALIGNED
          LABEL "Nombre de la Tasa o Unidad"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     Tasas_Mercado.Monto_Final AT ROW 4.5 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 4.77 COL 58
     Tasas_Mercado.Pla_Final AT ROW 5.58 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     BUTTON-62 AT ROW 6.65 COL 58
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 8.54
         SIZE 71 BY 8.35
         BGCOLOR 17 FONT 5
         TITLE "Nuevo Item de Valoración".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de las Valoraciones"
         HEIGHT             = 16.5
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Nuevo:FRAME = FRAME F_Val:HANDLE.

/* SETTINGS FOR FRAME F_Nuevo
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Nuevo:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Tasas_Mercado.Nom_TasaUnid IN FRAME F_Nuevo
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME F_Val
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Nuevo:MOVE-AFTER-TAB-ITEM (BtnDone-2:HANDLE IN FRAME F_Val)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB B_Valora BUTTON-49 F_Val */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Valora
/* Query rebuild information for BROWSE B_Valora
     _START_FREEFORM
FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
OPEN QUERY {&SELF-NAME}
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)
  NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Valora */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Nuevo
/* Query rebuild information for FRAME F_Nuevo
     _TblList          = "bdCentral.Tasas_Mercado"
     _Query            is OPENED
*/  /* FRAME F_Nuevo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Configuración de las Valoraciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Configuración de las Valoraciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME F_Val /* Salir */
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


&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Nuevo /* Salvar */
DO:
DO WITH FRAME F_Nuevo:
   IF Cmb_Productos:SCREEN-VALUE EQ "" OR Tasas_Mercado.Nom_TasaUnid:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "No se puede grabar la información" SKIP
              "debe entrarse el Producto y el nombre de la valoración"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO Tasas_Mercado.Nom_TasaUnid.
      RETURN NO-APPLY.
   END.
   IF Tasas_Mercado.Pla_Final:SCREEN-VALUE EQ "" OR 
      Tasas_Mercado.Monto_Final:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "No se puede grabar la información" SKIP
              "debe digitarse el Monto y el Plazo"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO Tasas_Mercado.Monto_Final.
      RETURN NO-APPLY.
   END.
   CREATE Tasas_Mercado.
   ASSIGN Tasas_Mercado.Cod_Producto  = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3))
          Tasas_Mercado.Fec_Crea      = TODAY
          Tasas_Mercado.Nit_Entidad   = W_Nit
          Tasas_Mercado.Nom_TasaUnid  = Tasas_Mercado.Nom_TasaUnid:SCREEN-VALUE
          Tasas_Mercado.Pdcto         = INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)
          Tasas_Mercado.Monto_Final   = DECIMAL(Tasas_Mercado.Monto_Final:SCREEN-VALUE)
          Tasas_Mercado.Pla_Final     = DECIMAL(Tasas_Mercado.Pla_Final:SCREEN-VALUE)
          Tasas_Mercado.Tipo          = INTEGER(SUBSTRING(Cmb_TInversiones:SCREEN-VALUE,1,1)).
FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
OPEN QUERY B_Valora
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto       EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)
  NO-LOCK INDEXED-REPOSITION.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME F_Val /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-50
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-50 wWin
ON CHOOSE OF BUTTON-50 IN FRAME F_Val /* Button 50 */
DO:
/*  DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancias.Lst".
  {Incluido\Imprimir.I "listado"}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-51
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-51 wWin
ON CHOOSE OF BUTTON-51 IN FRAME F_Val /* Button 51 */
DO:
/*  OPEN QUERY Br_Asignacion FOR EACH CFG_Instancias 
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia BY CFG_Instancias.Orden INDEXED-REPOSITION.
  VIEW FRAME F_ConsAsignaciones.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-60
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-60 wWin
ON CHOOSE OF BUTTON-60 IN FRAME F_Val /* Nuevo */
DO:
  VIEW FRAME F_Nuevo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define SELF-NAME BUTTON-62
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-62 wWin
ON CHOOSE OF BUTTON-62 IN FRAME F_Nuevo /* Button 62 */
DO:
  HIDE FRAME F_Nuevo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Valora
&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME B_Valora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Valora wWin
ON ROW-DISPLAY OF B_Valora IN FRAME F_Val
DO:
  CASE R_Pcto:SCREEN-VALUE IN FRAME F_Val:
    WHEN "3" THEN DO:
      FIND Pro_Inversiones WHERE Pro_Inversiones.Categoria EQ Tasas_Mercado.Tipo AND
           Pro_Inversiones.Cod_Producto EQ Tasas_Mercado.Cod_Producto NO-LOCK NO-ERROR.
      IF AVAILABLE Pro_Inversiones THEN
         W_NomPdt = Pro_Inversiones.Nom_Producto.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Entidades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Entidades wWin
ON VALUE-CHANGED OF Cmb_Entidades IN FRAME F_Val /* Entidad */
DO:
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NO-LOCK.
  IF AVAILABLE Bancos THEN DO:
     FIND Clientes WHERE Clientes.Nit EQ Bancos.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN W_Nit = Clientes.Nit.
  END.
  APPLY "Value-Changed" TO R_Pcto IN FRAME F_Val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define SELF-NAME Cmb_TInversiones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TInversiones wWin
ON VALUE-CHANGED OF Cmb_TInversiones IN FRAME F_Nuevo /* Tipos de Inversiones */
DO:
  RUN Combo_Inversiones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME R_Pcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Pcto wWin
ON VALUE-CHANGED OF R_Pcto IN FRAME F_Val
DO:
DO WITH FRAME F_Val:
  CASE SELF:SCREEN-VALUE:
    WHEN "3" THEN RUN Combo_Inversiones.
  END CASE.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Nuevo
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Combo_Inversiones wWin 
PROCEDURE Combo_Inversiones :
DO WITH FRAME F_Nuevo:
   Cmb_Productos:LIST-ITEMS = "".
   FOR EACH Pro_Inversiones WHERE 
            Pro_Inversiones.Categoria EQ INTEGER(SUBSTRING(Cmb_TInversiones:SCREEN-VALUE,1,1))
            NO-LOCK:
            W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Inversiones.Cod_Producto,"999") + " - " + Pro_Inversiones.Nom_Producto).
   END.
   IF Cmb_Productos:NUM-ITEMS NE 0 THEN
      Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Entidades R_Pcto 
      WITH FRAME F_Val IN WINDOW wWin.
  ENABLE Cmb_Entidades R_Pcto BUTTON-49 B_Valora BUTTON-50 BUTTON-51 BUTTON-60 
         BtnDone-2 
      WITH FRAME F_Val IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Val}

  {&OPEN-QUERY-F_Nuevo}
  GET FIRST F_Nuevo.
  DISPLAY Cmb_TInversiones Cmb_Productos 
      WITH FRAME F_Nuevo IN WINDOW wWin.
  IF AVAILABLE Tasas_Mercado THEN 
    DISPLAY Tasas_Mercado.Nom_TasaUnid Tasas_Mercado.Monto_Final 
          Tasas_Mercado.Pla_Final 
      WITH FRAME F_Nuevo IN WINDOW wWin.
  ENABLE Cmb_TInversiones Cmb_Productos Tasas_Mercado.Nom_TasaUnid 
         Tasas_Mercado.Monto_Final Btn_Salvar Tasas_Mercado.Pla_Final BUTTON-62 
      WITH FRAME F_Nuevo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Nuevo}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
DO WITH FRAME F_Val:
  FOR EACH Bancos NO-LOCK BREAK BY Bancos.Cod_Compensa:
    FIND Clientes WHERE Clientes.Nit EQ Bancos.Nit NO-LOCK NO-ERROR.
    IF FIRST-OF(Bancos.Cod_Compensa) THEN W_Nit = Clientes.Nit.
    IF AVAILABLE Clientes THEN
       W_Ok = Cmb_Entidades:ADD-LAST(STRING(Bancos.Cod_Compensa,"99") + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
    ELSE
       W_Ok = Cmb_Entidades:ADD-LAST(STRING(Bancos.Cod_Compensa,"99") + " - " + "No esta en Clientes").       
  END.
  IF Cmb_Entidades:NUM-ITEMS NE 0 THEN
     Cmb_Entidades:SCREEN-VALUE = Cmb_Entidades:ENTRY(1).
END.
FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
OPEN QUERY B_Valora
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)
  NO-LOCK INDEXED-REPOSITION.
RUN Combo_Inversiones.
HIDE FRAME F_Nuevo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

