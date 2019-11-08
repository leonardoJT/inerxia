&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
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

{src/adm2/widgetprto.i}

  {Incluido\VARIABLE.I "SHARED"}
DEFINE VARIABLE Clr_Linea AS INTEGER FORMAT "99".
DEFINE VARIABLE CColor AS CHARACTER FORMAT "X".
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR Puntero AS ROWID.
DEFINE TEMP-TABLE Res
    FIELD Ent LIKE Cfg_pub.Ente
    FIELD NEn LIKE Pub.Nombre
    FIELD Cod LIKE Cfg_pub.Codigo
    FIELD NCd LIKE Ind_Pub.Nombre
    FIELD Cla LIKE Pub.Clase
    FIELD Tpo LIKE Ind_Pub.Tipo
    FIELD Mes AS INTEGER FORMAT "99"
    FIELD Tot AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD Met AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD Cum AS DECIMAL FORMAT "->>>,>>9.99"
    FIELD Clr AS INTEGER FORMAT "99"
    FIELD Est AS INTEGER.

DEFINE TEMP-TABLE TCon
    FIELD Ent LIKE Cfg_pub.Ente
    FIELD Nen LIKE Pub.Nombre
    FIELD Cla LIKE Pub.Clase.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME b_pub

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Res

/* Definitions for BROWSE b_pub                                         */
&Scoped-define FIELDS-IN-QUERY-b_pub Res.Ent Res.NEn Res.Cod Res.NCd Res.Tot Res.Met CColor Res.Cum   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b_pub   
&Scoped-define SELF-NAME b_pub
&Scoped-define QUERY-STRING-b_pub FOR EACH Res BY Res.Ent BY Res.Cod
&Scoped-define OPEN-QUERY-b_pub OPEN QUERY {&SELF-NAME} FOR EACH Res BY Res.Ent BY Res.Cod.
&Scoped-define TABLES-IN-QUERY-b_pub Res
&Scoped-define FIRST-TABLE-IN-QUERY-b_pub Res


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-b_pub}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b_pub BtnDone BUTTON-1 BUTTON-2 RTipo Cmes ~
RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS CEPub RTipo RCon Cmes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Cerrar_Nivel LABEL "Cerrar Nivel"  .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE CEPub AS CHARACTER FORMAT "X(256)":U 
     LABEL "Perspectivas" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmes AS CHARACTER FORMAT "99":U INITIAL "01" 
     LABEL "Mes" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "01","02","03","04","05","06","07","08","09","10","11","12" 
     DROP-DOWN-LIST
     SIZE 23 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RCon AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Perspectivas", 1,
"Subgerencias", 2,
"Grupos", 3,
"Usuarios", 4
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE RTipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Especifico", 2
     SIZE 24 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 110 BY 13.19
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 2.69.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 11 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY b_pub FOR 
      Res SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE b_pub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b_pub wWin _FREEFORM
  QUERY b_pub DISPLAY
      Res.Ent WIDTH 7
     Res.NEn COLUMN-LABEL "Ente PUB"
     Res.Cod COLUMN-LABEL "Cod"
     Res.NCd WIDTH 28 COLUMN-LABEL "Indicador"
     Res.Tot 
     Res.Met 
CColor COLUMN-LABEL "C" WIDTH 3
     Res.Cum COLUMN-LABEL "Cum"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 12.12
         BGCOLOR 11 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     b_pub AT ROW 7.19 COL 4
     BtnDone AT ROW 4.77 COL 94
     BUTTON-1 AT ROW 1.27 COL 94
     BUTTON-2 AT ROW 2.88 COL 94
     CEPub AT ROW 2.88 COL 39 COLON-ALIGNED
     RTipo AT ROW 1.54 COL 6 NO-LABEL
     RCon AT ROW 1.54 COL 31 NO-LABEL
     Cmes AT ROW 4.38 COL 40 COLON-ALIGNED
     RECT-1 AT ROW 6.65 COL 2
     RECT-2 AT ROW 1.27 COL 15
     RECT-3 AT ROW 1.27 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.86 BY 19.58
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Consulta de BALANCE"
         HEIGHT             = 19.58
         WIDTH              = 112.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME fMain
   Custom                                                               */
/* BROWSE-TAB b_pub 1 fMain */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

ASSIGN 
       b_pub:COLUMN-RESIZABLE IN FRAME fMain       = TRUE.

/* SETTINGS FOR COMBO-BOX CEPub IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RCon IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b_pub
/* Query rebuild information for BROWSE b_pub
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Res BY Res.Ent BY Res.Cod.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b_pub */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de BALANCE */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de BALANCE */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
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


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Instancias.Lst".
  {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME b_pub
&Scoped-define SELF-NAME b_pub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_pub wWin
ON MOUSE-SELECT-DBLCLICK OF b_pub IN FRAME fMain
DO:

  CASE Res.Cla:
      WHEN 1 THEN DO:
         OPEN QUERY b_pub 
             FOR EACH Res WHERE 
                     (Res.Cla EQ 1 OR Res.Cla EQ 2) OR
                      Res.Ent BEGINS Res.Ent:SCREEN-VALUE IN BROWSE B_Pub BY Res.Ent BY Res.Cod.
      END.
      WHEN 2 THEN DO:
         OPEN QUERY b_pub 
             FOR EACH Res WHERE 
                     (Res.Cla EQ 1 OR Res.Cla EQ 2 OR Res.Cla EQ 3) OR
                      Res.Ent BEGINS Res.Ent:SCREEN-VALUE IN BROWSE B_Pub BY Res.Ent BY Res.Cod.
      END.
      WHEN 3 THEN DO:
         OPEN QUERY b_pub 
             FOR EACH Res WHERE 
                     (Res.Cla EQ 1 OR Res.Cla EQ 2 OR Res.Cla EQ 3 OR Res.Cla EQ 4) OR
                      Res.Ent BEGINS Res.Ent:SCREEN-VALUE IN BROWSE B_Pub BY Res.Ent BY Res.Cod.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_pub wWin
ON ROW-DISPLAY OF b_pub IN FRAME fMain
DO:
   CASE Res.Cla:
       WHEN 1 THEN CColor:SCREEN-VALUE IN BROWSE B_Pub = "P".
       WHEN 2 THEN CColor:SCREEN-VALUE IN BROWSE B_Pub = "S".
       WHEN 3 THEN CColor:SCREEN-VALUE IN BROWSE B_Pub = "G".
       WHEN 4 THEN CColor:SCREEN-VALUE IN BROWSE B_Pub = "U".
   END CASE.
   CColor:BGCOL IN BROWSE B_pub = Res.Clr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CEPub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CEPub wWin
ON VALUE-CHANGED OF CEPub IN FRAME fMain /* Perspectivas */
DO:
    ASSIGN FRAME FMain CEPub.
/*  FOR EACH res WHERE Res.Ent EQ SUBSTRING(CEPub,1,8): MESSAGE res.ent.
  END.*/
    CASE RCon:
        WHEN 1 THEN
            OPEN QUERY B_Pub FOR EACH Res 
                 WHERE Res.Ent EQ SUBSTRING(CEPub:SCREEN-VALUE,1,2) AND
                       Res.Cla EQ RCon AND Res.Cod NE 0
                 BY Res.Ent BY Res.Cod.
        WHEN 2 THEN
            OPEN QUERY B_Pub FOR EACH Res 
                 WHERE SUBSTRING(Res.Ent,3,2) EQ SUBSTRING(CEPub:SCREEN-VALUE,1,2) AND
                       Res.Cla EQ RCon AND Res.Cod NE 0
                 BY Res.Ent BY Res.Cod.
        WHEN 3 THEN
            OPEN QUERY B_Pub FOR EACH Res
                 WHERE SUBSTRING(Res.Ent,5,2) EQ SUBSTRING(CEPub:SCREEN-VALUE,1,2) AND
                       Res.Cla EQ RCon AND Res.Cod NE 0
                 BY Res.Ent BY Res.Cod.
        WHEN 4 THEN
            OPEN QUERY B_Pub FOR EACH Res
                 WHERE SUBSTRING(Res.Ent,7,2) EQ SUBSTRING(CEPub:SCREEN-VALUE,1,2) AND
                       Res.Cla EQ RCon AND Res.Cod NE 0
                 BY Res.Ent BY Res.Cod.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmes wWin
ON VALUE-CHANGED OF Cmes IN FRAME fMain /* Mes */
DO:
  FOR EACH Res: DELETE Res. END.
  RUN inicializar_Res.
  RUN Calcular.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cerrar_Nivel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cerrar_Nivel wWin
ON CHOOSE OF MENU-ITEM m_Cerrar_Nivel /* Cerrar Nivel */
DO:
    CASE Res.Cla:
      WHEN 1 THEN DO:
         OPEN QUERY b_pub 
             FOR EACH Res WHERE 
                     Res.Cla EQ 1 BY Res.Ent BY Res.Cod.
      END.
      WHEN 2 THEN DO:
         OPEN QUERY b_pub 
             FOR EACH Res WHERE 
                      Res.Cla EQ 1 OR Res.Cla EQ 2 BY Res.Ent BY Res.Cod.
      END.
      WHEN 3 THEN DO:
         OPEN QUERY b_pub 
             FOR EACH Res WHERE 
                      Res.Cla EQ 1 OR Res.Cla EQ 2 OR Res.Cla EQ 3 BY Res.Ent BY Res.Cod.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RCon wWin
ON VALUE-CHANGED OF RCon IN FRAME fMain
DO:
  DEFINE VAR Wk AS CHARACTER FORMAT "X(2)".
  ASSIGN FRAME FMain RCon RTipo.
  ASSIGN CEPub:LIST-ITEMS = "". 
  FOR EACH TCon: DELETE TCon. END.

  FOR EACH Pub WHERE Pub.Clase EQ RCon AND Pub.Estado EQ 1 NO-LOCK BREAK BY Pub.Ente:
      CASE RCon:
          WHEN 1 THEN DO:
              ASSIGN Wk = SUBSTRING(Pub.Ente,1,2)
                     Cepub:LABEL = "Perspectivas".
              FIND TCon WHERE TCon.Ent EQ Wk AND TCon.Cla EQ 1 NO-ERROR.
              IF NOT AVAILABLE TCon THEN DO:
                 CREATE TCon.
                 ASSIGN TCon.Ent = Pub.Ente
                        TCon.Cla = Pub.Cla
                        TCon.Nen = Pub.Nombre.
              END.
          END.
          WHEN 2 THEN DO:
              ASSIGN Wk = SUBSTRING(Pub.Ente,3,2)
                     Cepub:LABEL = "Subgerencias".
              FIND TCon WHERE TCon.Ent EQ Wk AND TCon.Cla EQ 2 NO-ERROR.
              IF NOT AVAILABLE TCon THEN DO:
                 FIND FIRST Cfg_Pub WHERE Cfg_Pub.Ente EQ Pub.Ente AND Cfg_Pub.Codigo NE 0 NO-LOCK NO-ERROR.
                 IF AVAILABLE Cfg_Pub THEN DO:
                   CREATE TCon.
                   ASSIGN TCon.Ent = SUBSTRING(Pub.Ente,3,2)
                          TCon.Cla = Pub.Clase
                          TCon.Nen = Pub.Nombre.
                 END.
              END.
          END.
          WHEN 3 THEN DO:
              ASSIGN Wk = SUBSTRING(Pub.Ente,5,2)
                     Cepub:LABEL = "Grupos".
              FIND TCon WHERE TCon.Ent EQ Wk AND TCon.Cla EQ 3 NO-ERROR.
              IF NOT AVAILABLE TCon THEN DO:
                 CREATE TCon.
                 ASSIGN TCon.Ent = SUBSTRING(Pub.Ente,5,2)
                        TCon.Cla = Pub.Clase
                        TCon.Nen = Pub.Nombre.
              END.
          END.
          WHEN 4 THEN DO:
              ASSIGN Wk = SUBSTRING(Pub.Ente,7,2)
                     Cepub:LABEL = "Usuarios".

              FIND TCon WHERE TCon.Ent EQ Wk AND TCon.Cla EQ 4 NO-ERROR.
              IF NOT AVAILABLE TCon THEN DO:
                 CREATE TCon.
                 ASSIGN TCon.Ent = SUBSTRING(Pub.Ente,7,2)
                        TCon.Cla = Pub.Clase
                        TCon.Nen = Pub.Nombre.
              END.
          END.
      END CASE.
  END.
  FOR EACH TCon WHERE TCon.Cla EQ RCon BREAK BY TCon.Ent:
      W_Ok = CEpub:ADD-LAST(TCon.Ent + " - " + TCon.Nen).
  END.
  CEPub:SCREEN-VALUE = CEPub:ENTRY(1).
  APPLY "value-changed" TO CEpub.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RTipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RTipo wWin
ON VALUE-CHANGED OF RTipo IN FRAME fMain
DO:
ASSIGN FRAME FMain RTipo.
IF RTipo EQ 1 THEN DO:
   DISABLE RCon CEPub WITH FRAME FMain.
   OPEN QUERY B_Pub FOR EACH Res BY Res.Ent BY Res.Cod.
END.
ELSE DO:
    ENABLE RCon CEPub WITH FRAME FMain.
    APPLY "Value-changed" TO RCon.
    APPLY "Entry" TO CEPub.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcular wWin 
PROCEDURE Calcular :
DEFINE VARIABLE Tot  AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VARIABLE Met  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VARIABLE Cum  AS DECIMAL FORMAT ">>>>.99".

ASSIGN FRAME Fmain Cmes RCon.
FOR EACH Cfg_Pub WHERE
         Cfg_Pub.Estado EQ 1 NO-LOCK:
    FIND Ind_Pub WHERE Ind_Pub.Codigo EQ Cfg_Pub.Codigo NO-LOCK NO-ERROR.
    IF AVAILABLE Ind_Pub AND Ind_Pub.Prg NE "" THEN
       RUN VALUE(TRIM(Ind_Pub.Prg)) (INPUT Cfg_Pub.Codigo, INPUT Cfg_Pub.Ente, INPUT Cmes, INPUT Cfg_Pub.Cuenta,
                 OUTPUT Tot, OUTPUT Met, OUTPUT Cum).
    FIND Res WHERE Res.Ent EQ Cfg_Pub.Ente AND Res.Cod EQ Cfg_Pub.Codigo NO-ERROR.
    IF AVAILABLE Res THEN
       ASSIGN Res.Ent = Cfg_pub.ente
              Res.Cod = Cfg_pub.codigo
              Res.Mes = INTEGER(Cmes)
              Res.Tot = Tot
              Res.Met = Met
              Res.Cum = Cum * 100.
    RUN ColorLinea (INPUT Cfg_Pub.Codigo, INPUT Res.Cum).
    Clr_Linea = Res.Clr.
    RUN Pintar_Mayores.
    ASSIGN Tot = 0 Cum = 0 Met = 0.
END.  
OPEN QUERY B_Pub FOR EACH Res BY Res.Ent BY Res.Cod.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColorLinea wWin 
PROCEDURE ColorLinea :
DEFINE INPUT PARAMETER PCod LIKE Ind_Pub.Codigo.
DEFINE INPUT PARAMETER PCum AS DECIMAL FORMAT "->>9.99".
FIND Ind_Pub WHERE Ind_Pub.Codigo EQ PCod AND Ind_Pub.Estado EQ 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Ind_Pub THEN Res.Clr = 0.
ELSE DO:
   IF Ind_Pub.Id_Malo    AND Ind_Pub.Mi LE PCum AND Ind_Pub.Mf GE PCum THEN Res.Clr = 12.
   IF Ind_Pub.Id_Regular AND Ind_Pub.Ri LE PCum AND Ind_Pub.Rf GE PCum THEN Res.Clr = 4.
   IF Ind_Pub.Id_Bueno   AND Ind_Pub.Bi LE PCum AND Ind_Pub.Bf GE PCum THEN Res.Clr = 14.
   IF Ind_Pub.Id_Exitoso AND Ind_Pub.Ei LE PCum AND Ind_Pub.Ef GE PCum THEN Res.Clr = 10.
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
  DISPLAY CEPub RTipo RCon Cmes 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE b_pub BtnDone BUTTON-1 BUTTON-2 RTipo Cmes RECT-1 RECT-2 RECT-3 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Res wWin 
PROCEDURE Inicializar_Res :
FOR EACH Res: DELETE Res. END .
DEFINE VAR WPer AS CHARACTER FORMAT "X(15)".
DEFINE VAR WSub AS CHARACTER FORMAT "X(15)".
DEFINE VAR WGru AS CHARACTER FORMAT "X(15)".
DEFINE VAR WUsu AS CHARACTER FORMAT "X(15)".
DEFINE VAR i AS INTEGER.
  FOR EACH Cfg_PUB WHERE Cfg_Pub.Estado EQ 1 NO-LOCK BREAK BY Cfg_Pub.Ente:
      CREATE Res.
      ASSIGN Res.Ent = Cfg_pub.Ente
             Res.Cod = Cfg_pub.codigo
             Res.Mes = INTEGER(Cmes)
             Res.Tot = 0
             Res.Met = 0
             Res.Cum = 0
             Res.Clr = 15.
      FIND Pub WHERE Pub.Ente EQ Cfg_Pub.Ente NO-LOCK NO-ERROR.
      IF AVAILABLE Pub THEN DO: 
         ASSIGN Res.Cla = Pub.Clase
                Res.Est = Pub.Estado
                Res.Nen = Pub.Nombre.
      END.
      ELSE Res.NEn = "NE".
      FIND Ind_Pub WHERE Ind_Pub.Codigo EQ Cfg_Pub.Codigo NO-LOCK NO-ERROR.
      IF AVAILABLE Ind_Pub THEN ASSIGN Res.NCd = Ind_Pub.Nombre
                                       Res.Tpo = Ind_Pub.Tipo.
      ELSE Res.NEn = "NE".
  END.
  FOR EACH Pub WHERE Pub.Estado EQ 1 NO-LOCK:
      FIND FIRST Res WHERE Res.Ent EQ Pub.Ente NO-ERROR.
      IF NOT AVAILABLE Res THEN DO:
         CREATE Res.
         ASSIGN Res.Ent = Pub.Ente
                Res.Cla = Pub.Clase
                Res.Mes = INTEGER(Cmes)
                Res.NEn = Pub.Nombre
                Res.Tot = 0
                Res.Met = 0
                Res.Cum = 0
                Res.Clr = 15
                Res.Est = Pub.Estado.
      END.
  END.
  FOR EACH Res:
    CASE Res.Cla:
      WHEN 2 THEN Res.NEn = "     " + Res.NEn.
      WHEN 3 THEN Res.NEn = "          " + Res.NEn.
      WHEN 4 THEN Res.NEn = "               " + Res.NEn.
    END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN Inicializar_Res.
  RUN SUPER.
  RUN Calcular.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pintar_Mayores wWin 
PROCEDURE Pintar_Mayores :
FIND Res WHERE Res.Ent EQ SUBSTRING(Cfg_Pub.Ente,1,6) NO-ERROR.
  IF AVAILABLE Res THEN Res.Clr = Clr_Linea.
  FIND Res WHERE Res.Ent EQ SUBSTRING(Cfg_Pub.Ente,1,4) NO-ERROR.
  IF AVAILABLE Res THEN Res.Clr = Clr_Linea.
  FIND Res WHERE Res.Ent EQ SUBSTRING(Cfg_Pub.Ente,1,2) NO-ERROR.
  IF AVAILABLE Res THEN Res.Clr = Clr_Linea.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
 W_Reporte   = "REPORTE   : BALANCE SCORE CARD - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Cod      Nombre                         Cod Indicador                          Realizado          Meta          Cumplido".
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 FOR EACH Res WHERE Res.Cod NE 0 BREAK BY Res.Ent:
   DISPLAY Res.Ent  
           TRIM(Res.NEn) FORMAT "X(30)"
           Res.Cod 
           Res.NCd FORMAT "X(30)"
           Res.Tot FORMAT "->>>,>>>,>>>,>>9" 
           Res.Met FORMAT "->>>,>>>,>>>,>>9"
           Res.Cum FORMAT "->>>,>>9.99"
       WITH FRAME Fmov WIDTH 232 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
 END.
 PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

