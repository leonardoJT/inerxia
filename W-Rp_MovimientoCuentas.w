&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
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
{Incluido/VARIABLE.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

  DEFINE VAR AgeIni   LIKE Agencias.Agencia.
  DEFINE VAR AgeFin   LIKE Agencias.Agencia.
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR P_Cuenta LIKE Cuentas.Cuenta.
  DEFINE VAR P_Nombre LIKE Cuentas.Nombre.
  DEFINE VAR P_NatTra LIKE Cuentas.Naturaleza.
  DEFINE VAR P_CtrNat LIKE Cuentas.Ctr_Naturaleza.

  DEFINE TEMP-TABLE TmpDoc
      FIELD Age LIKE Agencias.Agencia
      FIELD fec_contable AS DATE
      FIELD Com LIKE Comprobantes.Comprobante
      FIELD Doc LIKE Mov_Contable.Num_Documento
      FIELD Cta LIKE Mov_Contable.Cuenta
      INDEX idx1 age fec_contable com doc cta.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FMov

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Agencias CueIni CueFin FecIni FecFin ~
BUTTON-1 BUTTON-2 BtnDone 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias CueIni CueFin FecIni FecFin ~
NomCueIni NomCueFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 12 BY 1.38
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las agencias" 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las agencias" 
     DROP-DOWN-LIST
     SIZE 47 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CueFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CueIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCueFin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCueIni AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FMov
     Cmb_Agencias AT ROW 1.54 COL 11 COLON-ALIGNED
     CueIni AT ROW 2.62 COL 11 COLON-ALIGNED
     CueFin AT ROW 3.69 COL 11 COLON-ALIGNED
     FecIni AT ROW 4.77 COL 46 COLON-ALIGNED
     FecFin AT ROW 5.85 COL 46 COLON-ALIGNED
     BUTTON-1 AT ROW 1.27 COL 63
     NomCueIni AT ROW 2.62 COL 23 COLON-ALIGNED NO-LABEL
     BUTTON-2 AT ROW 2.88 COL 63
     NomCueFin AT ROW 3.69 COL 23 COLON-ALIGNED NO-LABEL
     BtnDone AT ROW 5.04 COL 63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75 BY 6.04
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
         TITLE              = "SFG - Documentos que contienen una o un rango de cuentas"
         HEIGHT             = 6.04
         WIDTH              = 75
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
/* SETTINGS FOR FRAME FMov
   Custom                                                               */
/* SETTINGS FOR FILL-IN NomCueFin IN FRAME FMov
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCueIni IN FRAME FMov
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Documentos que contienen una o un rango de cuentas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Documentos que contienen una o un rango de cuentas */
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
ON CHOOSE OF BtnDone IN FRAME FMov /* Salir */
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME FMov /* Button 1 */
DO:
  RUN W-Infdia.r NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME FMov /* Button 2 */
DO:
    IF CueIni:SCREEN-VALUE = "" OR
       CueFin:SCREEN-VALUE = "" OR
       FecIni:SCREEN-VALUE = "" OR
       FecFin:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Falta llenar alguno de los parámetros para el informe" SKIP
                "Rectifique la información entrada!!!"
            VIEW-AS ALERT-BOX.

        APPLY "entry" TO Cmb_Agencias.
    END.

    ASSIGN FRAME FMov
        CueIni
        CueFin
        FecIni
        FecFin
        Cmb_Agencias.

    IF SUBSTRING(Cmb_Agencias,1,3) = "000" THEN DO:
        AgeIni = 1.
        AgeFin = 999.
    END.
    ELSE DO: 
        AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
        AgeFin = AgeIni.
    END.

    EMPTY TEMP-TABLE TmpDoc.

    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia >= AgeIni
                            AND Mov_Contable.Agencia <= AgeFin
                            AND Mov_Contable.Cuenta >= CueIni
                            AND Mov_Contable.Cuenta <= CueFin
                            AND Mov_Contable.Fec_Contable >= FecIni
                            AND Mov_Contable.Fec_Contable <= fecFin NO-LOCK BREAK BY Mov_Contable.Cuenta:
        FIND FIRST TmpDoc WHERE TmpDoc.Age = Mov_Contable.Agencia
                            AND TmpDoc.fec_contable = mov_contable.fec_contable
                            AND TmpDoc.Com = Mov_Contable.Comprobante
                            AND TmpDoc.Doc = Mov_Contable.Num_Documento
                            AND TmpDoc.Cta = Mov_Contable.Cuenta NO-ERROR.
        IF NOT AVAILABLE TmpDoc THEN DO:
            CREATE TmpDoc.
            TmpDoc.Age = Mov_Contable.Agencia.
            TmpDoc.fec_contable = mov_contable.fec_contable.
            TmpDoc.Com = Mov_Contable.Comprobante.
            TmpDoc.Doc = Mov_Contable.Num_Documento.
            TmpDoc.Cta = Mov_Contable.Cuenta.
        END.
    END.

    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia >= AgeIni
                             AND Mov_Contable2.Agencia <= AgeFin
                             AND mov_contable2.comprobante >= 0
                             AND mov_contable2.comprobante <= 9999999999
                             AND mov_contable2.cen_costos >= 0
                             AND mov_contable2.cen_costos <= 9999999999
                             AND Mov_Contable2.Fec_Contable >= FecIni
                             AND Mov_Contable2.Fec_Contable <= fecFin
                             AND Mov_Contable2.Cuenta >= CueIni
                             AND Mov_Contable2.Cuenta <= CueFin NO-LOCK BREAK BY Mov_Contable2.Cuenta:
        FIND FIRST TmpDoc WHERE TmpDoc.Age = Mov_Contable2.Agencia
                            AND TmpDoc.fec_contable = mov_contable2.fec_contable
                            AND TmpDoc.Com = Mov_Contable2.Comprobante
                            AND TmpDoc.Doc = Mov_Contable2.Num_Documento
                            AND TmpDoc.Cta = Mov_Contable2.Cuenta NO-ERROR.
        IF NOT AVAILABLE TmpDoc THEN DO:
            CREATE TmpDoc.
            TmpDoc.Age = Mov_Contable2.Agencia.
            TmpDoc.fec_contable = mov_contable.fec_contable.
            TmpDoc.Com = Mov_Contable2.Comprobante.
            TmpDoc.Doc = Mov_Contable2.Num_Documento.
            TmpDoc.Cta = Mov_Contable2.Cuenta.
        END.
    END.

    ON return tab.

    DEFINE VAR Listado AS CHARACTER INITIAL "".
    listado = W_PathSpl + "L_DocCue.Lst".
    {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON ENTRY OF BUTTON-2 IN FRAME FMov /* Button 2 */
DO:
  ON return return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CueFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CueFin wWin
ON LEAVE OF CueFin IN FRAME FMov /* Cuenta Final */
DO:
    ASSIGN FRAME FMov CueFin.
    FIND Cuentas WHERE Cuentas.Cuenta EQ CueFin NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN DO:
        ASSIGN NomCueFin = Cuentas.Nombre.
        DISPLAY NomCueFin WITH FRAME FMov.
    END.
    ELSE DO:
      RUN C-Cuentas.r (  OUTPUT P_Cuenta, OUTPUT P_Nombre, OUTPUT P_NatTra,
                         OUTPUT P_CtrNat, INPUT  2).
      IF P_Cuenta NE "" THEN DO:
          ASSIGN SELF:SCREEN-VALUE = P_Cuenta
                 NomCueFin:SCREEN-VALUE = P_Nombre.
          APPLY "entry" TO FecIni.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CueIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CueIni wWin
ON LEAVE OF CueIni IN FRAME FMov /* Cuenta Inicial */
DO:
  ASSIGN FRAME FMov CueIni.
  FIND Cuentas WHERE Cuentas.Cuenta EQ CueIni NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN DO:
      ASSIGN NomCueIni = Cuentas.Nombre.
      DISPLAY NomCueIni WITH FRAME FMov.
  END.
  ELSE DO:
    RUN C-Cuentas.r (  OUTPUT P_Cuenta, OUTPUT P_Nombre, OUTPUT P_NatTra,
                       OUTPUT P_CtrNat, INPUT  2).
    IF P_Cuenta NE "" THEN DO:
        ASSIGN SELF:SCREEN-VALUE = P_Cuenta
               NomCueIni:SCREEN-VALUE = P_Nombre.
        APPLY "entry" TO CueFin.
    END.
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
  DISPLAY Cmb_Agencias CueIni CueFin FecIni FecFin NomCueIni NomCueFin 
      WITH FRAME FMov IN WINDOW wWin.
  ENABLE Cmb_Agencias CueIni CueFin FecIni FecFin BUTTON-1 BUTTON-2 BtnDone 
      WITH FRAME FMov IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FMov}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "Opción no disponible" VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Agencias NO-LOCK:
      W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME FMov.
  END.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}

DEFINE VAR W_Nombre AS CHARACTER FORMAT "X(30)".

W_Reporte = "REPORTE   : Documentos que Contienen Cuentas " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = "CB NUMDOC  CUENTA         DESCRIPCION                    NIT                    DEBITO          CREDITO".

DEFINE VAR TotDb AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotCr AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

W_Linea = FILL(W_Raya,132).

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.
  
FOR EACH TmpDoc BREAK BY TmpDoc.Age:
    IF FIRST-OF(TmpDoc.Age) THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia = TmpDoc.Age NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            DISPLAY "----------------------------------------------------------------------------------------------------------" AT 1 SKIP
                    "Agencia : " Agencias.Agencia " - " Agencias.Nombre SKIP
                    "----------------------------------------------------------------------------------------------------------" AT 1
                WITH FRAME Ft0 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
        ELSE
            DISPLAY "----------------------------------------------------------------------------------------------------------" AT 1
                    "Agencia : No Existe"
                    "----------------------------------------------------------------------------------------------------------" AT 1
                WITH FRAME Ft1.
    END.

    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante = TmpDoc.Com
                            AND mov_contable.fec_contable = TmpDoc.fec_contable
                            AND Mov_Contable.Agencia = TmpDoc.Age
                            AND Mov_Contable.Num_Documento = TmpDoc.Doc NO-LOCK BREAK BY Mov_Contable.Comprobante
                                                                                      BY Mov_Contable.Num_Documento:
        DISPLAY Mov_Contable.Comprobante
                Mov_Contable.Num_Documento
                Mov_Contable.Cuenta
                Mov_Contable.Comentario FORMAT "X(30)"
                Mov_Contable.Nit
                Mov_Contable.DB
                Mov_Contable.CR
            WITH FRAME FMvto WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        TotDb = TotDb + Mov_Contable.Db.
        TotCr = TotCr + Mov_Contable.Cr.

        IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
            /*        1         2         3         4         5         6         7         8         9         1
            "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
            "01 0048698 31050501       Consignación Ahorros Efectivo  21996467                    0           19,100*/
            DISPLAY "                                                                        ______________   ______________" SKIP
                    TotDb AT 73 FORMAT ">>,>>>,>>>,>>9"
                    TotCr AT 90 FORMAT ">>,>>>,>>>,>>9" SKIP(1)
                WITH FRAME Ftotdc WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

            TotDb = 0.
            TotCr = 0.
        END.
    END.

    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia = TmpDoc.Age
                             AND Mov_Contable2.Comprobante = TmpDoc.Com
                             AND mov_contable2.fec_contable = TmpDoc.fec_contable
                             AND Mov_Contable2.Num_Documento = TmpDoc.Doc NO-LOCK BREAK BY Mov_Contable2.Comprobante
                                                                                        BY Mov_Contable2.Num_Documento:
        DISPLAY Mov_Contable2.Comprobante
                Mov_Contable2.Num_Documento
                Mov_Contable2.Cuenta
                Mov_Contable2.Comentario FORMAT "X(30)"
                Mov_Contable2.cliente_id
                Mov_Contable2.DB
                Mov_Contable2.CR
            WITH FRAME FMvto2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

        TotDb = TotDb + Mov_Contable.Db.
        TotCr = TotCr + Mov_Contable.Cr.

        IF LAST-OF(Mov_Contable2.Num_Documento) THEN DO:
            /*        1         2         3         4         5         6         7         8         9         1
            "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
            "01 0048698 31050501       Consignación Ahorros Efectivo  21996467                    0           19,100*/
            DISPLAY "                                                                        ______________   ______________" SKIP
                    TotDb AT 73 FORMAT ">>,>>>,>>>,>>9"
                    TotCr AT 90 FORMAT ">>,>>>,>>>,>>9" SKIP(1)
                WITH FRAME Ftotdc2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

            TotDb = 0.
            TotCr = 0.
        END.
    END.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

