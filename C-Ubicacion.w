&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER P_Ubi LIKE Ubicacion.Ubicacion.
DEFINE OUTPUT PARAMETER P_Nom AS CHA FORMAT "X(50)".

DEF VAR CADENA AS CHA NO-UNDO.
DEF VAR W_Ok   AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Deptos Cmb_Ciudades Cmb_Barrios ~
BUTTON-101 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Deptos Cmb_Ciudades 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-101 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 101" 
     SIZE 10 BY 1.85.

DEFINE VARIABLE Cmb_Barrios AS CHARACTER FORMAT "X(256)":U 
     LABEL "Barrios" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 50 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Ciudades AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ciudades" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 50 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Deptos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Departamentos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 50 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Cmb_Deptos AT ROW 1.31 COL 1
     Cmb_Ciudades AT ROW 2.35 COL 17 COLON-ALIGNED
     Cmb_Barrios AT ROW 3.42 COL 17 COLON-ALIGNED
     BUTTON-101 AT ROW 4.54 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.86 BY 5.77
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Escoja la Ubicacion"
         HEIGHT             = 5.77
         WIDTH              = 69.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX Cmb_Barrios IN FRAME fMain
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX Cmb_Deptos IN FRAME fMain
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Escoja la Ubicacion */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Escoja la Ubicacion */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-101
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-101 wWin
ON CHOOSE OF BUTTON-101 IN FRAME fMain /* Button 101 */
DO:
  ASSIGN FRAME {&FRAME-NAME} Cmb_Barrios Cmb_Ciudades Cmb_Deptos.
  IF Cmb_Barrios EQ "" OR SUBSTR(Cmb_Barrios,1,8) EQ "00000000" 
     THEN P_Ubi = SUBSTR(Cmb_Ciudades,19,8).
     ELSE P_Ubi = SUBSTR(Cmb_Barrios,24,8).    
  P_Nom = IF trim(SUBSTR(Cmb_Barrios,1,8)) BEGINS "00000000" THEN substr(Cmb_Ciudades,1,15) + " - " +  substr(Cmb_Deptos,1,15) ELSE 
          substr(Cmb_Barrios,1,15) + " - " + substr(Cmb_Ciudades,1,15) + " - " +  substr(Cmb_Deptos,1,15).
          /*desactivo harold
          IF INT(P_Ubi) = 0 THEN 
     MESSAGE "No se esta enviando ninguna ubicación" SKIP
             "al registro. Rectifique" VIEW-AS ALERT-BOX.
             */       
        /*  MESSAGE "p_nom=" P_nom "P_ubi=" P_Ubi  "cmb_barrios" cmb_barrios VIEW-AS ALERT-BOX INFORMATION. */
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


&Scoped-define SELF-NAME Cmb_Ciudades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Ciudades wWin
ON VALUE-CHANGED OF Cmb_Ciudades IN FRAME fMain /* Ciudades */
DO:
  Cmb_Barrios:LIST-ITEMS = "".
  W_Ok = Cmb_Barrios:ADD-LAST("00000000").
  FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Estado = 1 AND Ubicacion.Tipo = "B"
       AND SUBSTR( STRING (Ubicacion.Ubicacion),1,5) = SUBSTR(Cmb_Ciudades:SCREEN-VALUE,19,5)
        BY Ubicacion.Estado BY Ubicacion.Tipo BY Ubicacion.Nombre:
      Cadena =substring( TRIM(Ubicacion.Nombre)+ FILL(" ",20),1,20) + " - " + STRING(Ubicacion.Ubicacion) + 
               IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
      W_Ok = Cmb_Barrios:ADD-LAST(STRING(Cadena)).
  END.
  Cmb_Barrios:SCREEN-VALUE = Cmb_Barrios:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Deptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Deptos wWin
ON VALUE-CHANGED OF Cmb_Deptos IN FRAME fMain /* Departamentos */
DO:
  Cmb_Ciudades:LIST-ITEMS = "".
  FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Estado = 1 AND Ubicacion.Tipo = "C"
       AND SUBSTR(Ubicacion.Ubicacion,1,2) = SUBSTR(Cmb_Deptos:SCREEN-VALUE,19,2)
        BY Ubicacion.Estado BY Ubicacion.Tipo BY Ubicacion.Nombre:
      W_Ok = Cmb_Ciudades:ADD-LAST(substring(trim(Ubicacion.Nombre) + FILL(" ",15),1,15) + " - " + STRING(Ubicacion.Ubicacion)).
  END.
  Cmb_Ciudades:SCREEN-VALUE = Cmb_Ciudades:ENTRY(1).
  Cmb_Barrios:LIST-ITEMS = "".
  W_Ok = Cmb_Barrios:ADD-LAST("00000000").
  FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Estado = 1 AND Ubicacion.Tipo = "B"
       AND SUBSTR(STRING (Ubicacion.Ubicacion),1,5) = SUBSTR(Cmb_Ciudades:SCREEN-VALUE,19,5)
        BY Ubicacion.Estado BY Ubicacion.Tipo BY Ubicacion.Nombre:
      Cadena = substring(TRIM (Ubicacion.Nombre)+ FILL (" ",20),1,20) + " - " + STRING(Ubicacion.Ubicacion) +  
               IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
      W_Ok = Cmb_Barrios:ADD-LAST(STRING(Cadena)).
  END.
  Cmb_Barrios:SCREEN-VALUE = Cmb_Barrios:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
 {src/adm2/windowmn.i} 
/*{windows.i}*/

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
  DISPLAY Cmb_Deptos Cmb_Ciudades 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Cmb_Deptos Cmb_Ciudades Cmb_Barrios BUTTON-101 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
    FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Estado = 1 AND Ubicacion.Tipo = "D"
                       BY Ubicacion.Estado BY Ubicacion.Tipo BY Ubicacion.Nombre:
        W_Ok = Cmb_Deptos:ADD-LAST(substring(trim(Ubicacion.Nombre)+ FILL(" ",15),1,15) + " - " + STRING(Ubicacion.Ubicacion)) IN FRAME {&FRAME-NAME}.
        IF Ubicacion.Ubicacion = "41000000" THEN
           Cmb_Deptos:SCREEN-VALUE = SUBSTRING(trim(Ubicacion.Nombre)+ FILL(" ",15),1,15) + " - " + STRING(Ubicacion.Ubicacion).
    END.
    FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Estado = 1 AND Ubicacion.Tipo = "C"
         AND SUBSTR(Ubicacion.Ubicacion,1,2) = SUBSTR(Cmb_Deptos:SCREEN-VALUE,19,2)
          BY Ubicacion.Estado BY Ubicacion.Tipo BY Ubicacion.Nombre:      
          W_Ok = Cmb_Ciudades:ADD-LAST(SUBSTRING(trim(Ubicacion.Nombre)+ FILL(" ",15),1,15) + " - " + STRING(Ubicacion.Ubicacion)). 
        IF Ubicacion.Ubicacion = "41001000" THEN
           Cmb_Ciudades:SCREEN-VALUE = SUBSTRING(trim(Ubicacion.Nombre)+ FILL(" ",15),1,15) + " - " + STRING(Ubicacion.Ubicacion).
    END.
    W_Ok = Cmb_Barrios:ADD-LAST("00000000").
    FOR EACH Ubicacion NO-LOCK WHERE Ubicacion.Estado = 1 AND Ubicacion.Tipo = "B"
         AND SUBSTR(Ubicacion.Ubicacion,1,5) = SUBSTR(Cmb_Ciudades:SCREEN-VALUE,19,5)
          BY Ubicacion.Estado BY Ubicacion.Tipo BY Ubicacion.Nombre:
        Cadena = SUBSTRING( TRIM (Ubicacion.Nombre) + FILL(" ",20),1,20) + " - " + string(Ubicacion.Ubicacion) + 
                 IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
        W_Ok = Cmb_Barrios:ADD-LAST(STRING(Cadena)).
    END.
    Cmb_Barrios:SCREEN-VALUE = Cmb_Barrios:ENTRY(2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

