&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-ProcInv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProcInv 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE VAR DiasCau AS INTEGER FORMAT "99".
DEFINE VAR W_IntCau AS DECIMAL.
DEFINE VAR W_Valora AS DECIMAL.
DEFINE VAR W_Monto AS DECIMAL.

   /* oakley */

   DEFI VAR W_Provis  LIKE Inversion_Sdos.VrInvers_Inic INIT 0.
   DEFI VAR W_Valor   LIKE Inversion_Sdos.VrInvers_Inic INIT 0.
   DEFI VAR W_Coment  AS CHAR FORM "X(30)"              INIT "".
   DEFI VAR W_Cta1    LIKE Cuentas.Cuenta.
   DEFI VAR W_Cta2    LIKE Cuentas.Cuenta.
   DEFI VAR W_Cpte    AS LOG INIT FALSE.
  
   DEFI TEMP-TABLE TempLiq 
        FIELD AgeE   LIKE Inversion_Sdo.Agencia
        FIELD NitE   LIKE Clientes.Nit
        FIELD TitE   LIKE Inversion_Sdo.Nro_Titulo
        FIELD SecE   LIKE Inversion_Sdo.Secuencia
        FIELD DiaC   AS INTEG FORM "99"                 INIT 0
        FIELD DiaA   AS INTEG FORM "9999"               INIT 0
        FIELD SdoA   LIKE Inversion_Sdo.VrInvers_Inic   INIT 0
        FIELD TasI   LIKE Inversion_Sdos.Tasa_NomiAnual INIT 0
        FIELD TasM   LIKE Inversion_Sdos.Tasa_NomiAnual INIT 0
        FIELD Unid   LIKE Inversion_Sdos.Valor_Unid     INIT 0
        FIELD ICau   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD ACau   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD VVal   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD AVal   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD DVal   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD VPro   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD APro   LIKE Inversion_Sdo.VrInvers_Inic INIT 0
        FIELD DPro   LIKE Inversion_Sdo.VrInvers_Inic INIT 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-5 W_CmbOfi Btn_Proc Btn_Done RECT-314 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_FecMes Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProcInv AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.46
     BGCOLOR 8 .

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar" 
     SIZE 10 BY 1.46 TOOLTIP "Procesar la Causación, Valoración y Provisión de Inversiones".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 34.57 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecMes AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .81 TOOLTIP "Fecha Corte para el Proceso"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15.14 BY 6.08.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     BUTTON-5 AT ROW 3.46 COL 71
     W_CmbOfi AT ROW 3.5 COL 16.72 COLON-ALIGNED
     Btn_Proc AT ROW 5.5 COL 71
     W_FecMes AT ROW 6.54 COL 17 COLON-ALIGNED
     Btn_Done AT ROW 7.35 COL 71 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Msaje AT ROW 9.65 COL 17 COLON-ALIGNED NO-LABEL
     RECT-314 AT ROW 3.12 COL 68.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.29 BY 16
         BGCOLOR 17 FGCOLOR 0 FONT 4.


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
  CREATE WINDOW W-ProcInv ASSIGN
         HIDDEN             = YES
         TITLE              = "Inversiones -Proceso -, Programa W-Prc_Valoracion.W"
         HEIGHT             = 11.27
         WIDTH              = 87.29
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 87.29
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 87.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW W-ProcInv
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
                                                                        */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecMes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProcInv)
THEN W-ProcInv:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProcInv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProcInv W-ProcInv
ON END-ERROR OF W-ProcInv /* Inversiones -Proceso -, Programa W-Prc_Valoracion.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProcInv W-ProcInv
ON WINDOW-CLOSE OF W-ProcInv /* Inversiones -Proceso -, Programa W-Prc_Valoracion.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  ON RETURN RETURN.
  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-ProcInv
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-ProcInv
ON CHOOSE OF Btn_Proc IN FRAME F_Proc /* Procesar */
DO:
  DEFI   VAR Listado    AS CHAR FORM "X(40)".
  DEFINE VAR W_Rpta   AS LOGICAL.

  FOR EACH TempLiq: DELETE TempLiq. END.

  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GE W_OfiIni 
                      AND Agencias.Agencia LE W_OfiFin NO-LOCK:
      FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia
                     AND ProcDia.Cod_Proceso EQ 8
                     AND ProcDia.Fecha_Proc  EQ W_FecMes - 1
                     AND ProcDia.Estado      EQ 2  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(ProcDia) THEN DO:
         MESSAGE "Este proceso NO Fué Ejecutado para el Día Anterior en la Agencia: " Agencias.Agencia SKIP
                 VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".     
         RETURN. 
      END.

      FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia
                     AND ProcDia.Cod_Proceso EQ 8
                     AND ProcDia.Fecha_Proc  EQ W_FecMes
                     AND ProcDia.Estado      EQ 1  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(ProcDia) THEN DO:
         MESSAGE "Este proceso Ya Fué Ejecutado para este Día en la Agencia: " Agencias.Agencia SKIP
                 "O no està Matriculado...Revise por favor. No se permite la operación." SKIP
                 VIEW-AS ALERT-BOX /*QUESTION BUTTONS YES-NO*/ TITLE "Confirmar Proceso".     
         RETURN. 
      END.
  END.

  FIND FIRST Inversion_Sdo WHERE Inversion_Sdo.Estado EQ 0 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Inversion_Sdo) THEN DO:
     MESSAGE "No se existen Inversiones Vigentes para realizar el proceso." SKIP
             "          Revise por favor. No se permite la operación" SKIP
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Proceso".   
     RETURN. 
  END.
    
  FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Produc EQ Inversion_Sdo.Cod_Produc NO-LOCK NO-ERROR.
  IF NOT AVAIL(Pro_Inversiones) THEN DO:                                                
     MESSAGE "Falta configurar Pro_Inversiones para el pdcto: " Inversion_Sdo.Cod_Produc     
            VIEW-AS ALERT-BOX.                                                              
     RETURN.                                                                          
  END.                                                                                      

  FIND Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                      AND Comprobantes.Comprob EQ Pro_Inversiones.Cpte_Tras
                      AND Comprobantes.Estado  EQ 1 NO-ERROR.
  IF NOT AVAIL(Comprobantes) THEN DO:
     MESSAGE "El Comprobante-Fuente Contable: " Pro_Inversiones.Cpte_Tras SKIP
             "debe existir activo en esta Agencia." SKIP
             "                          No se acepta la Operación." VIEW-AS ALERT-BOX.
     RETURN.
  END.
      
  ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
         listado                = W_PathSpl + "ProcInver.Lst".
                      
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
 
  SESSION:SET-WAIT-STATE("GENERAL").
  ASSIGN Msaje:SCREEN-VALUE = "Espere un momento por favor, Procesando Inversiones...".
  
  RUN ProcesoDia NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     SESSION:SET-WAIT-STATE("").

     RETURN.     
  END.
  ELSE DO:              
     /*{Incluido\ImpArch.I "listado"} */
                
     FOR EACH Agencias WHERE Agencias.Estado  NE 3                                     
                         AND Agencias.Agencia GE W_OfiIni 
                         AND Agencias.Agencia LE W_OfiFin NO-LOCK:                            
         FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia                    
                        AND ProcDia.Cod_Proceso EQ 8                                   
                        AND ProcDia.Fecha_Proc  EQ W_FecMes                            
                        AND ProcDia.Estado      EQ 1 NO-ERROR.                         
                                                                                       
         ASSIGN ProcDia.Estado = 2.                                                    
     END.                                                                              
                                                                                           
     ASSIGN listado = W_PathSpl + "CpteInver.Lst".            
  
    {Incluido\Imprimir.I "listado"}

     /* FIND Formatos WHERE Formatos.Agencia     EQ W_Agencia
                     AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato NO-LOCK NO-ERROR.

     IF AVAILABLE(Formatos) THEN
        RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.comprobante,
                                         INPUT Comprobantes.Secuencia, INPUT Comprobantes.Secuencia,
                                         INPUT W_Agencia).
     ELSE
        RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
    */

     ASSIGN Msaje:SCREEN-VALUE = "        Proceso Inversiones Terminó Exitosamente...".


  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-ProcInv
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-ProcInv
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
  IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
     FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                           AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiIni = Agencias.Agencia.
        
     FIND LAST Agencias WHERE Agencias.Agencia GT 0
                          AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiFin = Agencias.Agencia.
  END.
  ELSE ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
              W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecMes W-ProcInv
ON LEAVE OF W_FecMes IN FRAME F_Proc /* Fecha Proceso */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProcInv 


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
  
  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni                              = W_Agencia
         W_OfiFin                              = W_Agencia
         W_FecMes:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Fecha)
         W_FecMes.
        
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).      
  END.              
     
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
  APPLY "VALUE-CHANGED" TO W_CmbOfi.
            
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabiliza W-ProcInv 
PROCEDURE Contabiliza :

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Inversion_Sdos.Agencia
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Comprobante = Comprobantes.Comprobante
       Mov_Contable.Num_Documento = Comprobantes.Secuencia
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Comentario = W_Coment
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Hora = TIME
       Mov_Contable.Cen_Costos = W_CenCosGral
       Mov_Contable.Nit = Inversion_Sdos.Nit_Emi
       Mov_Contable.Doc_Refer = Inversion_Sdos.Nro_Titulo
       Mov_Contable.Cuenta = W_Cta1
       Mov_Contable.Cr = W_Valor.

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Inversion_Sdos.Agencia
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Comprobante = Comprobantes.Comprobante
       Mov_Contable.Num_Documento = Comprobantes.Secuencia
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Comentario = W_Coment
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Hora = TIME
       Mov_Contable.Cen_Costos = W_CenCosGral
       Mov_Contable.Nit = Inversion_Sdos.Nit_Emi
       Mov_Contable.Doc_Refer = Inversion_Sdos.Nro_Titulo
       Mov_Contable.Cuenta = W_Cta2
       Mov_Contable.Db = W_Valor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProcInv  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProcInv)
  THEN DELETE WIDGET W-ProcInv.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProcInv  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi W_FecMes Msaje 
      WITH FRAME F_Proc IN WINDOW W-ProcInv.
  ENABLE BUTTON-5 W_CmbOfi Btn_Proc Btn_Done RECT-314 
      WITH FRAME F_Proc IN WINDOW W-ProcInv.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-ProcInv.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaMovInv W-ProcInv 
PROCEDURE GrabaMovInv :
/*------------------------------------------------------------------------------
     Invocado desde ProcesoDia.
  Purpose:   Graba Mov_Inversion las liquidaciones.  
------------------------------------------------------------------------------*/
  CREATE Mov_Inversion.
  ASSIGN Mov_Inversion.Agencia    = Inversion_Sdos.Agencia
         Mov_Inversion.Ofi_Origen = W_Agencia
         Mov_Inversion.Nit        = Inversion_Sdos.Nit_Emisor
         Mov_Inversion.Nro_Titulo = Inversion_Sdos.Nro_Titulo
         Mov_Inversion.Secuencia  = Inversion_Sdos.Secuencia
         Mov_Inversion.Fecha      = W_Fecha
         Mov_Inversion.Hora       = TIME
         Mov_Inversion.Usuario    = W_Usuario
         Mov_Inversion.Descrip    = W_Coment
         Mov_Inversion.Oper       = "L"
         Mov_Inversion.Comprobant = Comprobantes.Comprobante
         Mov_Inversion.Num_Docume = Comprobantes.Secuencia
         Mov_Inversion.Vr_Consig  = W_Valor
         Mov_Inversion.Valor_Unid     = Inversion_Sdos.Valor_Unid
         Mov_Inversion.Tasa_NomiAMcdo = Inversion_Sdos.Tasa_NomiAMcdo
         Mov_Inversion.Tasa_NomiAnual = Inversion_Sdos.Tasa_NomiAnual.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaTemp W-ProcInv 
PROCEDURE GrabaTemp :
/*------------------------------------------------------------------------------
     Invocado desde ProcesoDia.
  Purpose:  Graba temporal con liquidaciones.
 ------------------------------------------------------------------------------*/
  CREATE TempLiq. 
  ASSIGN TempLiq.AgeE  = Inversion_Sdos.Agencia
         TempLiq.NitE  = Inversion_Sdos.Nit_E
         TempLiq.TitE  = Inversion_Sdos.Nro_Titulo
         TempLiq.SecE  = Inversion_Sdos.Secuencia
         TempLiq.SdoA  = Inversion_Sdos.Sdo_Actual
         TempLiq.TasI  = Inversion_Sdos.Tasa_NomiAnual / 365
         TempLiq.TasM  = Inversion_Sdos.Tasa_NomiAMcdo / 365
         TempLiq.Unid  = Inversion_Sdos.Valor_Unid
         TempLiq.DiaC  = DiasCau
         TempLiq.ICau  = W_IntCau.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaTasasMcdo W-ProcInv 
PROCEDURE HallaTasasMcdo :
/*------------------------------------------------------------------------------
  Purpose:  Halla las tasas para liquidar  
 ------------------------------------------------------------------------------*/
  DEFI VAR W_Tasa  LIKE Tasas_Mercado.Tasa        INIT 0.

  FIND FIRST Tasas_Mercado WHERE Tasas_Mercado.Nit_Entidad  EQ Bancos.Nit 
                             AND Tasas_Mercado.Pdcto        EQ 3
                             AND Tasas_Mercado.Tipo         EQ Pro_Inversiones.Categoria
                             AND Tasas_Mercado.Cod_Producto EQ Pro_Inversiones.Cod_Produc
                             AND Tasas_Mercado.Pla_Final    GE Inversion_Sdos.Plazo
                             AND Tasas_Mercado.Monto_Final  GE Inversion_Sdos.Sdo_Actual                
                             AND Tasas_Mercado.Estado       EQ 1   NO-LOCK NO-ERROR.
      
  IF AVAIL(Tasas_Mercado) THEN DO:
     RUN TasaPerNom IN W_ManFin (INPUT 365,Tasas_Mercado.Tasa,1,OUTPUT W_Tasa).

     Inversion_Sdos.Tasa_NomiAMcdo = (W_Tasa * 36500).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaTasaVble W-ProcInv 
PROCEDURE HallaTasaVble :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR W_Tasa LIKE Indicadores.Tasa.

  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Inversion_Sdos.Indicador
                           AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Indicadores) THEN DO:
     ASSIGN W_Tasa = Indicadores.Tasa + Inversion_Sdos.Puntos_Adic.

     RUN TasaPerNom IN W_ManFin (INPUT 365,W_Tasa,1,OUTPUT W_Tasa).

     Inversion_Sdos.Tasa_NomiAnual = W_Tasa * 36500.
  END.
  ELSE 
     MESSAGE "Para Tasa variable es Indispensable el Cod-Indicador y que exista en la configuraciòn." SKIP
             "El Indicador : " Inversion_Sdos.Indicador " NO Existe activo en Indicadores." SKIP
          VIEW-AS ALERT-BOX TITLE "Informativo".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-ProcInv 
PROCEDURE ImpCpte :
DEFI VAR TotD AS DECIMAL INIT 0.
DEFI VAR TotC AS DECIMAL INIT 0.
DEFI VAR TTotD AS DECIMAL INIT 0.
DEFI VAR TTotC AS DECIMAL INIT 0.

{Incluido\RepEncabezado.I}

  ASSIGN W_Reporte = "Reporte   : Comprobante Resumen Liq.Inversiones     Fecha del Cpte: " +
                     STRING(W_FecMes,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.
  
  DISPLAY SKIP(0)
            "                   " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                                   STRING(Comprobantes.Secuencia,"99999999") FORMAT "X(120)"  
            SKIP(1)
            "--------------------------------------------------------------------------------------------------------"
       WITH DOWN WIDTH 140 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
  
  FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                          AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                          AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta BY Mov_Contable.Nit:
      ASSIGN TotD  = TotD  + Mov_Contable.Db
             TTotD = TTotD + Mov_Contable.Db
             TotC  = TotC  + Mov_Contable.Cr
             TTotC = TTotC + Mov_Contable.Cr.

      IF LAST-OF(Mov_Contable.Nit) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
         DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                 Mov_Contable.Cuenta    LABEL "Cta-Contable"
                 Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                 Mov_Contable.Nit       LABEL "Nit-Emisor"
                 TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                 TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN TotD  = 0
                TotC  = 0.
      END.

  END.

  DISPLAY SKIP(1)
          "                     TOTAL FINAL------------>                            ---------------- ----------------"
          SKIP
          "                                                                    "
          TTotD      FORM "->>>>>>,>>>,>>9.99"
          TTotC      FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoDia W-ProcInv 
PROCEDURE ProcesoDia :
/*------------------------------------------------------------------------------
      Invocado desde Triggers Btn_Proc.
  Purpose: Proceso Valoración y provisión diario.    
 ------------------------------------------------------------------------------*/
 DEFI VAR DiasFuturos LIKE Inversion_Sdos.Dias_Cau INIT 1.
 
DO TRANSACTION:  
  FOR EACH Inversion_Sdo WHERE Inversion_Sdo.Estado  EQ 0
                           AND Inversion_Sdo.Agencia GE W_OfiIni
                           AND Inversion_Sdo.Agencia LE W_OfiFin
                               BY Inversion_Sdo.Categoria BY Inversion_Sdo.Cod_Produc 
                               BY Inversion_Sdo.Agencia:
      ASSIGN DiasCau     = 1   /*Inicia Con 1 Dìa*/
             DiasFuturos = 1
             W_IntCau    = 0
             W_Valora    = 0
             W_Provis    = 0
             W_Valor     = 0.
        
      FIND FIRST Pro_Inversiones WHERE Pro_Inversiones.Cod_Produc EQ Inversion_Sdo.Cod_Produc NO-LOCK NO-ERROR.
      IF NOT AVAIL(Pro_Inversiones) THEN DO:
         MESSAGE "Falta configurar Pro_Inversiones para el pdcto: " Inversion_Sdo.Cod_Produc 
                VIEW-AS ALERT-BOX.
         RETURN ERROR.
      END.
        
      IF Pro_Inversiones.Id_TasaUnid   EQ 0 THEN DO:
         IF Inversion_Sdo.Fec_Apertura EQ W_Fecha
         OR Inversion_Sdo.Fec_Vcto     LT W_Fecha THEN
            DiasCau = 0.

         RUN HallaTasasMcdo.

         IF Inversion_Sdos.TipTasa_FV EQ 1 THEN 
            RUN HallaTasaVble.
      END.
      ELSE DO:               /*Liq.X Unidades*/
          FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Inversion_Sdos.Indicador
                                      AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
          IF AVAIL(Indicadores) THEN DO:
             ASSIGN Inversion_Sdos.Valor_Unid = Indicadores.Valor
                    W_Monto                   = Inversion_Sdos.Valor_Unid * Inversion_Sdos.Nro_Unid.

             IF W_Monto - (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado) NE 0 THEN DO:
                ASSIGN W_IntCau = W_Monto - (Inversion_Sdos.Sdo_Actual + Inversion_Sdos.Interes_Causado)
                       W_Coment = "Liquidación-Dia"
                       W_Cta1   = Pro_Inversiones.CtaRendimiento_CR                                                
                       W_Cta2   = Pro_Inversiones.CtaRendimiento_DB.

                IF W_IntCau GT 0 THEN
                   ASSIGN W_Valor                        = W_IntCau - Inversion_Sdos.Interes_Causado                                                                        
                          Inversion_Sdos.Interes_Causado = W_IntCau                  
                          Inversion_Sdos.Dias_Cau        = Inversion_Sdos.Dias_Cau + 1.  
                ELSE
                   ASSIGN W_Cta2                         = Pro_Inversiones.CtaRendimiento_CR                                                
                          W_Cta1                         = Pro_Inversiones.CtaRendimiento_DB
                          W_Valor                        = Inversion_Sdos.Interes_Causado - W_IntCau
                          Inversion_Sdos.Interes_Causado = W_IntCau                  
                          Inversion_Sdos.Dias_Cau        = Inversion_Sdos.Dias_Cau + 1.

                IF W_Valor LT 0 THEN W_Valor = W_Valor * -1.

                RUN Contabiliza.
                RUN GrabaMovInv.  

                IF W_IntCau LT 0 THEN
                   ASSIGN Mov_Inversion.Vr_Consig  = 0
                          Mov_Inversion.Vr_Retiro  = W_Valor.
             END.
          END.
      END.

      IF DiasCau GT 0 AND Pro_Inversiones.Id_TasaUnid EQ 0 THEN DO:      /*Liq.X tasa*/
         IF Inversion_Sdo.Fec_Vcto GT W_Fecha THEN
            DiasFuturos = Inversion_Sdo.Fec_Vcto - W_Fecha.

         RUN HPDF IN W_ManFin (INPUT (Inversion_Sdos.Sdo_Actual + 
                                     (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                                        365) * Inversion_Sdo.Plazo) / 100), 
         /*365 dias por 100*/  INPUT (Inversion_Sdos.Tasa_NomiAMcdo / 36500),
                               INPUT DiasFuturos,
                               OUTPUT W_Valora).

         W_IntCau = (W_Valora - Inversion_Sdos.Sdo_Actual).      /* Estaba con---> / DiasFuturos.*/

         IF W_IntCau GT 0 THEN DO:
            ASSIGN Inversion_Sdos.Int_Pactado = Inversion_Sdos.Int_Pactado +                                   
                                                (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                                                365) * DiasCau) / 100 

                   W_Coment                       = "Valoración-Dia"                                                                
                   /*W_Valor                        = W_IntCau*/
                   W_Valor                        = 0
                   W_Cta1                         = Pro_Inversiones.CtaRendimiento_CR                                                
                   W_Cta2                         = Pro_Inversiones.CtaRendimiento_DB                                                
                   /*Inversion_Sdos.Interes_Causado = Inversion_Sdos.Interes_Causado + W_IntCau */
                   Inversion_Sdos.Dias_Cau        = Inversion_Sdos.Dias_Cau + DiasCau.  

            IF W_IntCau GT Inversion_Sdos.Interes_Causado THEN
               W_Valor = W_IntCau - Inversion_Sdos.Interes_Causado.
            ELSE IF W_IntCau LT Inversion_Sdos.Interes_Causado THEN
               ASSIGN W_Valor = Inversion_Sdos.Interes_Causado - W_IntCau
                      W_Cta2  = Pro_Inversiones.CtaRendimiento_CR 
                      W_Cta1  = Pro_Inversiones.CtaRendimiento_DB.

            IF W_Valor GT 0 THEN DO:
               RUN Contabiliza.

               RUN GrabaMovInv. 

               IF W_IntCau LT Inversion_Sdos.Interes_Causado THEN
                  ASSIGN Mov_Inversion.Vr_Consig  = 0
                         Mov_Inversion.Vr_Retiro  = W_Valor.
            END.

            Inversion_Sdos.Interes_Causado = W_IntCau.

         END.
      END.
      
      RUN GrabaTemp.
      
      ASSIGN TempLiq.DiaA = Inversion_Sdos.Dias_Cau
             TempLiq.ACau = Inversion_Sdos.Interes_Causado
             DiasCau      = 0
             W_Valora     = Inversion_Sdos.Sdo_Actual.
             
      IF Inversion_Sdos.Calif_Invers NE "A " THEN DO:
         IF Inversion_Sdos.Calif_Invers EQ "B" THEN
            W_Provis = Inversion_Sdos.Sdo_Actual * .20.
         ELSE IF Inversion_Sdos.Calif_Invers EQ "C" THEN
            W_Provis = Inversion_Sdos.Sdo_Actual * .40.
         ELSE IF Inversion_Sdos.Calif_Invers EQ "D" THEN
            W_Provis = Inversion_Sdos.Sdo_Actual * .60.         
         ELSE 
            W_Provis = Inversion_Sdos.Sdo_Actual.                           
      END.          
         
      ASSIGN W_Valor      = 0
             W_Cta1       = Pro_Inversiones.CtaProvision_DB
             W_Cta2       = Pro_Inversiones.CtaProvision_CR.
             
      IF W_Provis GT Inversion_Sdos.Vr_Provis THEN
         W_Valor = W_Provis - Inversion_Sdos.Vr_Provision.
      ELSE
         IF W_Provis LT Inversion_Sdos.Vr_Provision THEN
            ASSIGN W_Valor = Inversion_Sdos.Vr_Provision - W_Provis
                   W_Cta2  = Pro_Inversiones.CtaProvision_DB
                   W_Cta1  = Pro_Inversiones.CtaProvision_CR.
      
      ASSIGN TempLiq.VPro                = W_Valor
             TempLiq.APro                = Inversion_Sdos.Vr_Provision
             Inversion_Sdos.Vr_Provision = W_Provis
             TempLiq.DPro                = W_Provis
             W_Coment                    = "Liquidación-Provisión".
      
      IF W_Valor GT 0 THEN DO:       
         RUN Contabiliza.
     
         ASSIGN W_Valor = W_Provis.
                
         RUN GrabaMovInv.
      END.
  END.
     
END.  /*Fin Tx*/
END PROCEDURE.

/* IF Inversion_Sdo.Fec_Vcto GE W_FecMes THEN DO:
         IF Inversion_Sdo.Fec_Apertura LT DATE(MONTH(W_FecMes),01,YEAR(W_FecMes)) THEN
            DiasCau = DAY(W_FecMes).
         ELSE 
            DiasCau = DAY(W_FecMes) - DAY(Inversion_Sdo.Fec_Apertura).      
      END.
      ELSE DO:
         IF Inversion_Sdo.Fec_Vcto GE DATE(MONTH(W_FecMes),01,YEAR(W_FecMes)) THEN DO:
            IF Inversion_Sdo.Fec_Apertura LT DATE(MONTH(W_FecMes),01,YEAR(W_FecMes)) THEN
               DiasCau = DAY(Inversion_Sdo.Fec_Vcto).
            ELSE 
               DiasCau = DAY(Inversion_Sdo.Fec_Vcto) - DAY(Inversion_Sdo.Fec_Apertura). 
         END.     
      END.*/




/*    IF Inversion_Sdo.Fec_Vcto GT W_FecMes THEN
         DiasCau = Inversion_Sdo.Fec_Vcto - W_FecMes.
         
      IF DiasCau GT 0 THEN   
         RUN HPDF IN W_ManFin (INPUT (Inversion_Sdos.Sdo_Actual + 
                                     (((Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual) /
                                        365) * (Inversion_Sdo.Fec_Vcto - Inversion_Sdo.Fec_Apertura)) / 100), 
         /*365 dias por 100*/  INPUT (Inversion_Sdos.Tasa_NomiAnual / 36500),
                               INPUT DiasCau,
                               OUTPUT W_Valora).
 
      ASSIGN W_Valor      = 0
             W_Cta1       = Pro_Inversiones.CtaValoracion_DB
             W_Cta2       = Pro_Inversiones.CtaValoracion_CR.
             
      IF W_Valora GT Inversion_Sdos.Vr_Valoracion THEN
         W_Valor = W_Valora - Inversion_Sdos.Vr_Valoracion.
      ELSE
         IF W_Valora LT Inversion_Sdos.Vr_Valoracion THEN
            ASSIGN W_Valor = Inversion_Sdos.Vr_Valoracion - W_Valora
                   W_Cta2  = Pro_Inversiones.CtaValoracion_DB
                   W_Cta1  = Pro_Inversiones.CtaValoracion_CR.
                   
      ASSIGN TempLiq.VVal                 = W_Valor
             TempLiq.AVal                 = Inversion_Sdos.Vr_Valoracion
             Inversion_Sdos.Vr_Valoracion = W_Valora         /*Nueva valoración en tabla inversiones*/
             TempLiq.DVal                 = W_Valora
             W_Coment                     = "Liquidación-Valoración"
             W_Provis                     = 0.
             
      IF W_Valor GT 0 THEN DO:       
         RUN Contabiliza.
   
         ASSIGN W_Valor  = W_Valora.
                
         RUN GrabaMovInv.
      END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ProcInv 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:   Detalle del proceso.
  ----------------------------------------------------------------------------*/
   RUN ImpCpte.
      
   DEFI VAR TInt   LIKE TempLiq.ICau init 0.
   DEFI VAR TProv  LIKE TempLiq.ICau init 0.
   DEFI VAR TVal   LIKE TempLiq.ICau init 0.

   {Incluido\RepEncabezado.I}

   ASSIGN W_Reporte = "Reporte   : Proceso Diario Liq.Inversiones     Fecha del Informe: " +
            STRING(W_FecMes,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").

   VIEW FRAME F-Encabezado.
   VIEW FRAME f-ftr.
  
   FOR EACH TempLiq:
       ASSIGN TInt  = TInt  + TempLiq.ICau
              TProv = TProv + TempLiq.VPro
              TVal  = TVal  + TempLiq.VVal.
       DISPLAY
            TempLiq.AgeE   LABEL "Ag."
            TempLiq.NitE   LABEL "Nit-Emisor"
            TempLiq.TitE   LABEL "Nro-Título"
            TempLiq.SecE   LABEL "Sec"
            TempLiq.DiaA   LABEL "D.Cau-Acum"
            TempLiq.DiaC   LABEL "D.Cau"
            TempLiq.SdoA   LABEL "Sdo-Inversiòn"         FORM "->>>>,>>>,>>9.99"
            TempLiq.TasI   LABEL "%Tasa-Dia"             FORM ">9.999"
            TempLiq.TasM   LABEL "%Mcdo-Dia"             FORM ">9.999"

            TempLiq.ICau   LABEL "Int.-Liquidado"        FORM "->>>>,>>>,>>9.99"
            TempLiq.ACau   LABEL "Acu.Int.Liquidado"     FORM "->>>>,>>>,>>9.99"
            TempLiq.Unid   LABEL "VR.Mcdo-Dia"           FORM ">>>>>>9.999"

            TempLiq.APro   LABEL "Provisión Anter."      FORM ">>>>,>>>,>>9.99"
            TempLiq.VPro   LABEL "Provisión Difer."      FORM ">>>>,>>>,>>9.99"
            TempLiq.DPro   LABEL "Provisión Actual"      FORM ">>>>,>>>,>>9.99"
            SKIP(0)
          WITH DOWN WIDTH 240 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
   END. 
   
   DISPLAY SKIP(1)
           "                                            TOTALES :                               "
           TInt          FORM "->>>>>,>>>,>>9.99"
           "                                              "
           TProv         FORM "->>>>>,>>>,>>9.99"
      WITH DOWN WIDTH 240 FRAME F3 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

