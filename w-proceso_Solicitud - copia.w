&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

DEFINE BUFFER BSolicitud FOR solicitud.
DEFINE VAR tasa1 AS DECIMAL.

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR choice AS LOGICAL.
DEFINE VAR W_NvaAdm AS LOGICAL.
DEFINE VAR W_NvaHV AS LOGICAL.
DEFINE VAR W_NvoCD AS LOGICAL.
DEFINE VAR Puntero AS ROWID.
DEFINE VAR Longitud AS DECIMAL.
DEFINE VAR W_Ultima AS INTEGER.
DEFINE VAR W_Primera AS INTEGER.
DEFINE VAR W_Negadas AS INTEGER.
DEFINE VAR W_SucAgen AS LOGICAL.
DEFINE VAR Id_Agregar AS CHARACTER.
DEFINE VAR W_TipoInforme AS CHARACTER.
DEFINE VAR Orden_InsPan AS INTEGER.
DEFINE VAR W_VigIns AS INTEGER.
DEFINE VAR Wk_PerPagEmp AS INTEGER.
DEFINE VAR control_grabar AS LOGICAL.
DEFINE VAR W_SiMInst AS LOGICAL.
DEFINE VAR W_RowIdTx AS ROWID.
DEFINE VAR Tas_Nominal AS DECIMAL.
DEFINE VAR P_Nit AS CHARACTER.
DEFINE VAR p_Nombre AS CHARACTER.
DEFINE VAR P_Apellido AS CHARACTER.
DEFINE VAR P_AgeCli AS CHARACTER.
DEFINE VAR W_Nuevo AS LOGICAL.
DEFINE VAR W_NroCodeu AS INTEGER.
DEFINE VAR W_NBien AS CHARACTER.
DEFINE VAR RowidGar_Global AS ROWID.
DEFINE VAR V_Cod AS INTEGER.
DEFINE VAR V_Nom AS CHARACTER.
DEFINE VAR PlaMinI AS DECIMAL.
DEFINE VAR PlaMaxI AS DECIMAL.
DEFINE VAR NN AS INTEGER.
DEFINE VAR P_ForPag AS INTEGER.
DEFINE VAR P_CedNit AS CHARACTER.
DEFINE VAR vcFormatoFecha AS CHARACTER.
DEFINE VAR W_Antiguedad AS DECIMAL.
DEFINE VAR k AS INTEGER FORMAT "9".
DEFINE VAR CodCreditoActivo AS INTEGER.
DEFINE VAR PDFImpreso AS LOGICAL INITIAL NO.
DEF VAR W_Sarlaft AS LOG INIT FALSE.

DEFINE TEMP-TABLE TCred_ACanc
    FIELD Agen AS INTEGER
    FIELD Pto AS INTEGER
    FIELD NumC AS INTEGER
    FIELD FecD AS DATE
    FIELD CuoC AS DECIMAL
    FIELD Tasa AS DECIMAL
    FIELD SdoTD AS DECIMAL
    FIELD CSiNo AS LOGICAL FORMAT "Si/No"
    FIELD valorAbono AS DECIMAL.

DEFINE BUFFER bfrTTAbonos FOR TCred_ACanc.

DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia AS INTEGER
    FIELD TP_Orden AS INTEGER
    FIELD TP_Instancia AS INTEGER
    FIELD TP_NomInstan AS CHARACTER
    FIELD TP_Usuario AS CHARACTER
    FIELD TP_NomUsuar AS CHARACTER
    FIELD TP_Cantidad AS INTEGER
    FIELD TP_Abogado AS LOGICAL.

DEFINE TEMP-TABLE TCode
    FIELD TC_AgeCode AS INTEGER
    FIELD TC_NitCode AS CHARACTER
    FIELD TC_NitDeud AS CHARACTER
    FIELD TC_NumSoli AS INTEGER
    FIELD TC_NomCode AS CHARACTER FORMAT "X(60)"
    FIELD TC_TelCdRs AS CHARACTER
    FIELD TC_TelCdCo AS CHARACTER
    FIELD TC_EmlCode AS CHARACTER
    FIELD TC_EstRela AS INTEGER
    FIELD TC_FecCrea AS DATE
    FIELD TC_FecReti AS DATE
    FIELD TC_Aprob AS LOGICAL.

DEFINE TEMP-TABLE TUXI
    FIELD Instanc AS INTEGER
    FIELD Agencia AS INTEGER
    FIELD Usuario AS CHARACTER
    FIELD Nombre AS CHARACTER
    FIELD Cantidad AS INTEGER
    FIELD Proceso AS LOGICAL.

DEFINE TEMP-TABLE TCerradas
    FIELD Instancia AS INTEGER
    FIELD INom_Instancia AS CHARACTER
    FIELD Fec_Ingreso AS DATE
    FIELD Fec_Retiro AS DATE
    FIELD Hora_Ingreso AS INTEGER
    FIELD Hora_Retiro AS INTEGER
    FIELD Estado AS LOGICAL
    FIELD Num_Solicitud AS INTEGER
    FIELD Usuario AS CHARACTER
    FIELD INom_Usuario AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion AS CHARACTER.

DEFINE VARIABLE A_Age AS INTEGER.
DEFINE VARIABLE A_Pro AS INTEGER.
DEFINE VARIABLE A_NitW AS CHARACTER.
DEFINE VARIABLE A_Cue AS CHARACTER.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE W_Ok AS LOGICAL.
DEFINE VARIABLE Dias AS DECIMAL.

DEFINE TEMP-TABLE TScoring
    FIELD CodS AS INTEGER
    FIELD TabS AS CHARACTER
    FIELD VarS AS CHARACTER
    FIELD VVaS AS CHARACTER
    FIELD PunS AS DECIMAL
    FIELD FecS AS DATE
    FIELD path AS CHARACTER.

DEFINE TEMP-TABLE TDeducc LIKE Deducible.

DEFINE TEMP-TABLE Consulta
    FIELD Num_Solicitud AS INTEGER
    FIELD AgeSolicitud AS INTEGER
    FIELD Nit AS CHARACTER
    FIELD Estado AS INTEGER
    FIELD Nombre AS CHARACTER FORMAT "X(40)"
    FIELD Fec_Ingreso AS DATE
    FIELD Hor_Ingreso AS CHARACTER FORMAT "X(15)"
    FIELD Orden AS INTEGER
    FIELD Monto AS DECIMAL FORMAT ">,>>>,>>>,>>9"
    FIELD Vigencia AS INTEGER.

DEFINE VAR vPeriodicidad AS INTEGER INITIAL 1.
DEFINE VAR Wk_Edad AS INTEGER.
DEFINE VAR WFactorCod AS INTEGER.

DEFINE TEMP-TABLE TmpI
    FIELD ILinea AS INTEGER FORMAT "99"
    FIELD ITexto AS CHARACTER FORMAT "X(125)".

DEFINE TEMP-TABLE CTmpI
    FIELD ILinea AS INTEGER FORMAT "99"
    FIELD ITexto AS CHARACTER FORMAT "X(50)"
    FIELD Ingr AS DECIMAL
    FIELD Egr AS DECIMAL
    FIELD Puntos AS DECIMAL
    FIELD Endeu AS DECIMAL.

DEFINE TEMP-TABLE CCTmpI
    FIELD ILinea AS INTEGER FORMAT "99"
    FIELD ITexto AS CHARACTER FORMAT "X(20)"
    FIELD Deud AS CHARACTER FORMAT "X(20)"
    FIELD Cod1 AS CHARACTER FORMAT "X(20)"
    FIELD Cod2 AS CHARACTER FORMAT "X(20)"
    FIELD Cod3 AS CHARACTER FORMAT "X(20)".

DEFINE BUFFER bfrAgencia FOR agencia.

DEFINE TEMP-TABLE tPro_Scoring LIKE pro_scoring
    FIELD NdoParent AS CHARACTER
    FIELD Ndo AS CHARACTER
    FIELD pth AS CHARACTER.

DEFINE VAR j AS INTEGER.

FOR EACH pro_scoring BREAK BY pro_scoring.codigo
                           BY pro_scoring.observacion
                           BY pro_scoring.VARIABLE:
    IF FIRST-OF(pro_scoring.variable) THEN DO:
        CREATE tPro_Scoring.
        BUFFER-COPY pro_scoring TO tPro_scoring.

        i = NUM-ENTRIES(pro_scoring.observacion,"|").

        DO j = 1 TO i:
            IF j = 1 THEN
                tpro_scoring.ndo = SUBSTRING(entry(j,pro_scoring.observacion,"|"),1,2).

            IF j > 1 THEN
                tpro_scoring.NdoParent = SUBSTRING(entry(j - 1,pro_scoring.observacion,"|"),1,2).

            tpro_scoring.pth = tpro_scoring.pth + SUBSTRING(entry(j,pro_scoring.observacion,"|"),1,2).
        END.
    END.
END.

DEFINE TEMP-TABLE tEscnrioEcnmco
    FIELD clfccion AS CHARACTER
    FIELD ps AS DECIMAL
    INDEX pk ps.

DEFINE VAR DECpcdadPgo AS DECIMAL.
DEFINE VARIABLE RutaFoto AS CHARACTER FORMAT "x(80)".

DEFINE VAR totalAbonoCreditos AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_CredACanc
&Scoped-define BROWSE-NAME BR_Admisible

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Garantias TCerradas TCode Hoja_Vida Consulta ~
TCred_ACanc TDeducc Extras TScoring Tuxi Anexos_Clientes Clientes

/* Definitions for BROWSE BR_Admisible                                  */
&Scoped-define FIELDS-IN-QUERY-BR_Admisible Garantias.Tipo_Garantia Garantias.Identificacion_Bien Garantias.Nom_Bien   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Admisible   
&Scoped-define SELF-NAME BR_Admisible
&Scoped-define OPEN-QUERY-BR_Admisible /*OPEN QUERY {&SELF-NAME} FOR EACH Garantias NO-LOCK INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-BR_Admisible Garantias
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Admisible Garantias


/* Definitions for BROWSE Br_Cerradas                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Cerradas TCerradas.Instancia TCerradas.INom_Instancia TCerradas.Usuario TCerradas.INom_Usuario TCerradas.Fec_Retiro TCerradas.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cerradas   
&Scoped-define SELF-NAME Br_Cerradas
&Scoped-define QUERY-STRING-Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cerradas OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cerradas TCerradas
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cerradas TCerradas


/* Definitions for BROWSE Br_Codeudores                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Codeudores TC_AgeCode TC_NitCode TC_NomCode TC_Aprob TC_TelCdRs TC_TelCdCo TC_emlCode   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Codeudores   
&Scoped-define SELF-NAME Br_Codeudores
&Scoped-define QUERY-STRING-Br_Codeudores FOR EACH TCode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Codeudores OPEN QUERY {&SELF-NAME} FOR EACH TCode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Codeudores TCode
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Codeudores TCode


/* Definitions for BROWSE Br_ConHV                                      */
&Scoped-define FIELDS-IN-QUERY-Br_ConHV Hoja_Vida.Fec_Grabacion Hoja_Vida.Observacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ConHV   
&Scoped-define SELF-NAME Br_ConHV
&Scoped-define QUERY-STRING-Br_ConHV FOR EACH Hoja_Vida WHERE Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud                                              AND Hoja_Vida.Tipo = 9                                              AND Hoja_Vida.Codigo = 1                                              AND Hoja_Vida.DoctoRef = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)                                              AND Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos, ~
      1, ~
      5))                                              AND Hoja_Vida.Asunto_Cumplido = NO                                              AND Hoja_Vida.Usuario = W_Usuario INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_ConHV OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud                                              AND Hoja_Vida.Tipo = 9                                              AND Hoja_Vida.Codigo = 1                                              AND Hoja_Vida.DoctoRef = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)                                              AND Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos, ~
      1, ~
      5))                                              AND Hoja_Vida.Asunto_Cumplido = NO                                              AND Hoja_Vida.Usuario = W_Usuario INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_ConHV Hoja_Vida
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ConHV Hoja_Vida


/* Definitions for BROWSE Br_Consulta                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Consulta Consulta.Num_Solicitud Consulta.AgeSolicitud Consulta.Estado Consulta.Nit Consulta.Nombre Consulta.Fec_Ingreso Consulta.Hor_Ingreso Consulta.Monto Consulta.Vigencia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Consulta   
&Scoped-define SELF-NAME Br_Consulta
&Scoped-define QUERY-STRING-Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Solicitud INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Consulta OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Solicitud INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Consulta Consulta
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Consulta Consulta


/* Definitions for BROWSE Br_Cred                                       */
&Scoped-define FIELDS-IN-QUERY-Br_Cred TCred_ACanc.Agen TCred_ACanc.Pto TCred_ACanc.Tasa TCred_ACanc.NumC TCred_ACanc.FecD TCred_ACanc.CuoC TCred_ACanc.SdoTD TCred_ACanc.CSiNo TCred_ACanc.valorAbono   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cred TCred_ACanc.CSiNo TCred_ACanc.valorAbono   
&Scoped-define ENABLED-TABLES-IN-QUERY-Br_Cred TCred_ACanc
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Br_Cred TCred_ACanc
&Scoped-define SELF-NAME Br_Cred
&Scoped-define QUERY-STRING-Br_Cred FOR EACH TCred_ACanc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cred OPEN QUERY {&SELF-NAME} FOR EACH TCred_ACanc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cred TCred_ACanc
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cred TCred_ACanc


/* Definitions for BROWSE Br_Deducibles                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Deducibles TDeducc.Nom_Deducible TDeducc.Valor TDeducc.Valor_Impuesto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Deducibles   
&Scoped-define SELF-NAME Br_Deducibles
&Scoped-define QUERY-STRING-Br_Deducibles FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Deducibles OPEN QUERY {&SELF-NAME} FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Deducibles TDeducc
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Deducibles TDeducc


/* Definitions for BROWSE Br_Extras                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Extras Extras.Agencia Extras.Cod_Credito Extras.Nit Extras.Num_Solicitud Extras.Nro_Cuota Extras.Vr_CuoExtra Extras.Fec_Vcto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Extras   
&Scoped-define SELF-NAME Br_Extras
&Scoped-define QUERY-STRING-Br_Extras FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit                                        AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BY Extras.Nro_Cuota INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Extras OPEN QUERY Br_Extras FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit                                        AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BY Extras.Nro_Cuota INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Extras Extras
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Extras Extras


/* Definitions for BROWSE BR_Scoring                                    */
&Scoped-define FIELDS-IN-QUERY-BR_Scoring VarS VVaS PunS   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR_Scoring   
&Scoped-define SELF-NAME BR_Scoring
&Scoped-define QUERY-STRING-BR_Scoring FOR EACH TScoring NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR_Scoring OPEN QUERY {&SELF-NAME} FOR EACH TScoring NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR_Scoring TScoring
&Scoped-define FIRST-TABLE-IN-QUERY-BR_Scoring TScoring


/* Definitions for BROWSE Br_Usuarios                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Usuarios Tuxi.Agencia Tuxi.Usuario Tuxi.Nombre Tuxi.Cantidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Usuarios   
&Scoped-define SELF-NAME Br_Usuarios
&Scoped-define QUERY-STRING-Br_Usuarios FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Usuarios OPEN QUERY {&SELF-NAME} FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Usuarios Tuxi
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Usuarios Tuxi


/* Definitions for FRAME F_Cerradas                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cerradas ~
    ~{&OPEN-QUERY-Br_Cerradas}

/* Definitions for FRAME F_Codeudores                                   */

/* Definitions for FRAME F_ConAdmisible                                 */

/* Definitions for FRAME F_Condicionada                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Condicionada ~
    ~{&OPEN-QUERY-Br_Usuarios}

/* Definitions for FRAME F_ConHV                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConHV ~
    ~{&OPEN-QUERY-Br_ConHV}

/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-Br_Consulta}

/* Definitions for FRAME F_CredACanc                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_CredACanc ~
    ~{&OPEN-QUERY-Br_Cred}

/* Definitions for FRAME F_Deducibles                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Deducibles ~
    ~{&OPEN-QUERY-Br_Deducibles}

/* Definitions for FRAME F_Extras                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Extras ~
    ~{&OPEN-QUERY-Br_Extras}

/* Definitions for FRAME F_ForPago                                      */
&Scoped-define FIELDS-IN-QUERY-F_ForPago Anexos_Clientes.Cam_Cat1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_ForPago Anexos_Clientes.Cam_Cat1 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_ForPago Anexos_Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_ForPago Anexos_Clientes
&Scoped-define QUERY-STRING-F_ForPago FOR EACH Anexos_Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_ForPago OPEN QUERY F_ForPago FOR EACH Anexos_Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_ForPago Anexos_Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_ForPago Anexos_Clientes


/* Definitions for FRAME F_Scoring                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Scoring ~
    ~{&OPEN-QUERY-BR_Scoring}

/* Definitions for FRAME F_Solicitud                                    */
&Scoped-define QUERY-STRING-F_Solicitud FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Solicitud OPEN QUERY F_Solicitud FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Solicitud Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_Solicitud Clientes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Br_Cred btnSalirAbonoCreditos 
&Scoped-Define DISPLAYED-OBJECTS W_TotCanc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Solicitud.Monto Cmb_Sistemas Solicitud.Plazo 
&Scoped-define List-2 Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Cancelar ~
BUTTON-9 
&Scoped-define List-6 Garantias.Estado Garantias.Nom_Bien 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fActualzaTposPrdctos wWin 
FUNCTION fActualzaTposPrdctos RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAhrros wWin 
FUNCTION fAhrros RETURNS LOGICAL (cNit AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBrdesRngo wWin 
FUNCTION fBrdesRngo RETURNS CHARACTER
  (crngo AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fchar2decimal wWin 
FUNCTION fchar2decimal RETURNS DECIMAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fColExcel wWin 
FUNCTION fColExcel RETURNS CHARACTER
  (j AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCpcdadPgo wWin 
FUNCTION fCpcdadPgo RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCrdtos wWin 
FUNCTION fCrdtos RETURNS LOGICAL (cnit AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFchaLtras wWin 
FUNCTION fFchaLtras RETURNS CHARACTER
  (dafcha AS DATE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fpath wWin 
FUNCTION fpath RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRngoSlrio wWin 
FUNCTION fRngoSlrio RETURNS DECIMAL
  (deSlrio AS DECIMAL,iNmroPrsnas AS INTEGER,cCiudad AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVldaSV wWin 
FUNCTION fVldaSV RETURNS DECIMAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_w-adm_garantias AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-proclientesnew AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Borra 
     LABEL "Borrar" 
     SIZE 12.57 BY 1.04.

DEFINE BUTTON Btn_CanAdm 
     LABEL "Cancelar" 
     SIZE 13.29 BY 1.04.

DEFINE BUTTON Btn_ConAdm 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 160" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_InaAdm 
     LABEL "Inactivar" 
     SIZE 13.57 BY 1.04.

DEFINE BUTTON Btn_IngAdm 
     LABEL "Ingresar" 
     SIZE 12.72 BY 1.04.

DEFINE BUTTON Btn_SalAdm 
     LABEL "Salvar" 
     SIZE 13 BY 1.04.

DEFINE BUTTON BUTTON-161 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 161" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Nom_Aseguradora AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_UsuGarantia AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 30.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CredAval AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DispGaran AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-293
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 5.81.

DEFINE RECTANGLE RECT-294
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 2.96.

DEFINE RECTANGLE RECT-295
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.86 BY 2.81.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.43 BY 1.04.

DEFINE RECTANGLE RECT-299
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19.86 BY 1.08.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 2.85.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE E_Agregar AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 990 SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_OutCerradas 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 143" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-154 
     LABEL "Ver solo Instancia y Descripción" 
     SIZE 25 BY 1.12.

DEFINE BUTTON Btn_Activas 
     LABEL "Inactivar" 
     SIZE 10.14 BY 1.12.

DEFINE BUTTON Btn_CanCod 
     LABEL "Cancelar" 
     SIZE 10.72 BY 1.12.

DEFINE BUTTON Btn_CreCod 
     LABEL "Crear" 
     SIZE 10 BY 1.12.

DEFINE BUTTON Btn_SalCod 
     LABEL "Salvar" 
     SIZE 10 BY 1.12.

DEFINE BUTTON BUTTON-155 
     LABEL "i" 
     SIZE 3.72 BY .81
     FONT 0.

DEFINE BUTTON BUTTON-165 
     LABEL "Inf.Econòmica" 
     SIZE 13.72 BY 1.04 TOOLTIP "Actualizaciòn Informaciòn Econòmica del Codeudor seleccionado".

DEFINE VARIABLE W_NitCodeudor AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Codeudor" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCodeudor AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 43.14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RActivas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 21.57 BY .77
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-297
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.43 BY 1.77.

DEFINE BUTTON Btn_OutConAdm 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE R_ConAdm AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Inactivas", 2
     SIZE 24 BY .88 NO-UNDO.

DEFINE BUTTON BUTTON-162 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 162" 
     SIZE 11 BY 1.88.

DEFINE VARIABLE Cmb_InsCon AS CHARACTER FORMAT "X(45)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE E_Condicion AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 48 BY 8.35
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_OutConHV 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_OutConsulta 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 133" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(30)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE VG_Alta AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .54
     BGCOLOR 12 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE VG_Media AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 28.29 BY .62
     BGCOLOR 14 FONT 4 NO-UNDO.

DEFINE VARIABLE VG_Normal AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .65
     BGCOLOR 10 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE R_Organizar AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Solicitud", 1,
"Agencia", 2,
"Nit", 3,
"Nombre", 4,
"Fecha", 5,
"Vigencia", 6
     SIZE 69 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-223
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 1.35.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 30 BY 1.08
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.08
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.08
     BGCOLOR 12 .

DEFINE BUTTON btnSalirAbonoCreditos 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 167" 
     SIZE 8 BY 1.35.

DEFINE VARIABLE W_TotCanc AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Saldos a Cancelar" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_Anexos 
     LABEL "Anexos" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "Cancelar" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 10" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_Deshacer 
     IMAGE-UP FILE "imagenes/volver2.bmp":U
     LABEL "Deshacer" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_ImpCA 
     LABEL "Impr.de Aprobación" 
     SIZE 19 BY .92.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_Ingresar 
     IMAGE-UP FILE "imagenes/agregar.jpg":U
     LABEL "Ingresar" 
     SIZE 10 BY 1.69.

DEFINE BUTTON Btn_ProcesarInstancia 
     LABEL "Procesar Instancia" 
     SIZE 34 BY 1.12.

DEFINE BUTTON Btn_Salvar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "Salvar" 
     SIZE 10 BY 1.69.

DEFINE BUTTON BUTTON-164 
     LABEL "I.Económica" 
     SIZE 9.86 BY 1.69 TOOLTIP "Actualizaciòn Informaciòn Econòmica del Deudor"
     FONT 4.

DEFINE BUTTON BUTTON-169 
     LABEL "Historial de Créditos" 
     SIZE 18.86 BY .92.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "Salir" 
     SIZE 10 BY 1.69.

DEFINE BUTTON BUTTON-9 
     IMAGE-UP FILE "imagenes/eliminar.jpg":U
     LABEL "Borrar" 
     SIZE 10 BY 1.69.

DEFINE BUTTON PDF 
     LABEL "PDF" 
     SIZE 6 BY 1.12.

DEFINE VARIABLE Cmb_Instancias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 34.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 35 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 19.38.

DEFINE RECTANGLE RECT-347
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY .12.

DEFINE VARIABLE T_Refresh AS LOGICAL INITIAL no 
     LABEL "Refrescar Automaticamente" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-101 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 101" 
     SIZE 9 BY 1.88.

DEFINE BUTTON BUTTON-104 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 104" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE W_NomAgeDesembolso AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomProDesembolso AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_AdExt 
     LABEL "&Salvar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON Btn_EliExt 
     LABEL "&Eliminar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON BUTTON-170 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 170" 
     SIZE 10.57 BY 1.62 TOOLTIP "Retorna a la Ventana Principal".

DEFINE VARIABLE W_PPExtra AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrExtra AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-106 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 106" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE W_NomAgeDebAutomatico AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCodDebAutomatico AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-230 
     LABEL "Cerrar" 
     SIZE 11.29 BY .81.

DEFINE IMAGE P_Foto
     FILENAME "adeicon/blank":U
     SIZE 18 BY 5.12.

DEFINE BUTTON Btn_OutGarantias 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 136" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE R_TipoGarantia AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Garantía Personal", 1,
"Garantía Admisible", 2,
"Garantía No Admisible", 3
     SIZE 68.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-226
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 95 BY 1.35
     BGCOLOR 18 .

DEFINE BUTTON Btn_NvoHv 
     LABEL "Ingresar" 
     SIZE 11 BY 1.35.

DEFINE BUTTON Btn_SalvaHV 
     LABEL "Salvar" 
     SIZE 11 BY 1.35.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 149" 
     SIZE 11 BY 1.65.

DEFINE BUTTON BUTTON-150 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 150" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-152 
     LABEL "Cancelar" 
     SIZE 11 BY 1.38.

DEFINE BUTTON BUTTON-108 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 108" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-156 
     LABEL "Ver Información Detallada" 
     SIZE 30 BY 1.65.

DEFINE VARIABLE S_InfoCliente AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP MAX-CHARS 999999999 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 48 BY 12.12
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_OutScoring 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 121" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE S_InfoProducto AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 51.57 BY 7.81
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_AgregarTxt 
     LABEL "Agregar Texto" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_GraInstancia 
     LABEL "Grabar" 
     SIZE 15 BY 1.65.

DEFINE BUTTON Btn_Imp 
     LABEL "&Imprimir" 
     SIZE 14.86 BY 1.5.

DEFINE BUTTON Btn_insVolver 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 135" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-142 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 142" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Vigencia AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tiempo vigente de la instancia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WHora_Ingreso AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Whora_Retiro AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_Instancia AS CHARACTER FORMAT "X(45)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UsuarioInstancia AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_OutProductos 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "x" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-131 
     LABEL "Salir sin escoger" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_Ejecutar 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Ejecutar" 
     SIZE 7.72 BY 1.69 TOOLTIP "Envía Repositorio Directamente A Excel".

DEFINE BUTTON BUT-IMP-Scoring 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 208" 
     SIZE 7.72 BY 1.69 TOOLTIP "Imprime Análisis Del Score".

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Siguiente" 
     SIZE 3 BY .81 TOOLTIP "Cliente/Codeudor - Siguiente".

DEFINE BUTTON BUTTON-99 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 99" 
     SIZE 7.72 BY 1.69.

DEFINE VARIABLE Nom_CteCod AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 38.14 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Total_Puntaje AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Puntaje" 
     VIEW-AS FILL-IN 
     SIZE 19.86 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_Concepto AS CHARACTER FORMAT "X(25)":U 
     LABEL "Concepto" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE M_Cliente AS LOGICAL INITIAL no 
     LABEL "Del Cliente" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.57 BY .58
     BGCOLOR 17  NO-UNDO.

DEFINE BUTTON btnFrmlrioAfliacion 
     LABEL "Afiliación" 
     SIZE 20 BY 1 TOOLTIP "Formulario De Afiliación".

DEFINE BUTTON btnGrntias 
     LABEL "Admin. Garantías" 
     SIZE 20 BY 1 TOOLTIP "Administracion de Garantias".

DEFINE BUTTON Btn_CancCred 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Cancelar Creditos con esta Solicitud de Ptmo" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Destino 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Destino" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Extras 
     LABEL "Cuotas extraordinarias" 
     SIZE 32 BY 1.

DEFINE BUTTON Btn_Gar 
     LABEL "Adicionar Garantías" 
     SIZE 20 BY 1 TOOLTIP "Adiciona Garantias a Una Solicitud".

DEFINE BUTTON Btn_HojaVida 
     LABEL "Pendientes" 
     SIZE 20 BY 1.

DEFINE BUTTON Btn_Liquidar 
     LABEL "Liquidar" 
     SIZE 20 BY 1.

DEFINE BUTTON Btn_Proyectar 
     LABEL "Proyección de Pagos" 
     SIZE 20 BY 1.

DEFINE BUTTON BUTTON-100 
     LABEL "Scoring" 
     SIZE 20 BY 1.

DEFINE BUTTON BUTTON-102 
     LABEL "Deducibles" 
     SIZE 20 BY 1.

DEFINE BUTTON BUTTON-103 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 103" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-105 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 105" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-107 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 107" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-120 
     LABEL "Producto" 
     SIZE 20 BY 1.

DEFINE BUTTON BUTTON-19 
     LABEL "i" 
     SIZE 3 BY .81
     FONT 0.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_PerPago AS CHARACTER FORMAT "X(25)":U INITIAL "4 - Mensual" 
     LABEL "Período" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "0 - Diario","1 - Semanal","2 - Decadal","3 - Quincenal","4 - Mensual","5 - Bimestral","6 - Trimestral","7 - Cuatrimestral","8 - Semestral","9 - Anual" 
     DROP-DOWN-LIST
     SIZE 18.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Sistemas AS CHARACTER FORMAT "X(60)":U 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 18.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Asociado AS DATE FORMAT "99/99/9999" 
     LABEL "Fec.Vinculación" 
     VIEW-AS FILL-IN 
     SIZE 16.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomIndicador AS CHARACTER FORMAT "X(30)":U 
     LABEL "Indicador" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Destino AS CHARACTER FORMAT "X(256)":U 
     LABEL "Destino" 
     VIEW-AS FILL-IN 
     SIZE 28.86 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Producto AS CHARACTER FORMAT "X(70)":U 
     LABEL "Linea" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .81
     BGCOLOR 3 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Texto1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY .69
     FGCOLOR 12 FONT 14 NO-UNDO.

DEFINE VARIABLE Texto2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY .69
     FGCOLOR 12 FONT 14 NO-UNDO.

DEFINE VARIABLE WAntiguedad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antiguedad" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WMonto AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Monto Máximo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WTasa AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Plena -" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .23
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WVeces AS CHARACTER FORMAT "X(256)":U 
     LABEL "# Veces" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WX_Edad AS INTEGER FORMAT "Z9":U INITIAL 0 
     LABEL "Edad" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Desembolso AS CHARACTER FORMAT "X(45)":U 
     LABEL "Desembolso" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ForPago AS CHARACTER FORMAT "X(45)":U 
     LABEL "Forma de Pago" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomPdo AS CHARACTER FORMAT "X(30)":U 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE W_TasaNominal AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Nominal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TasaPeriodo AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Período" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Tipo_Credito AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tipo" 
      VIEW-AS TEXT 
     SIZE 17.43 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotExt AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Extras" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrADesemb AS DECIMAL FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Vr.A Desembolsar Hoy" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrCredACanc AS DECIMAL FORMAT ">,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Deudas a Cancelar con esta Solicitud" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 3.73.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.73.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 1.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.57 BY 2.31.

DEFINE RECTANGLE RECT-330
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.23.

DEFINE RECTANGLE RECT-331
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.57 BY 4.88.

DEFINE RECTANGLE RECT-333
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.72 BY 4.27.

DEFINE RECTANGLE RECT-334
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.57 BY 7.54.

DEFINE BUTTON Btn_OutUltima 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_SalvaUltima 
     LABEL "Salvar" 
     SIZE 11 BY 1.35.

DEFINE VARIABLE Cmb_Negadas AS CHARACTER FORMAT "X(256)":U 
     LABEL "Motivo Negación" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00001 - Capacidad de Pago","00002 - Mora Comercial","00003 - Estabilidad Laboral","00004 - Inconsis. Informaci. ","00005 - Concepto de Agencia","00006 - Crédito Recién Desemb.","00007 - Verificación de Inf.","00008 - Edad","00009 - Codeudor","00010 - Falta Documentación","00011 - Pago Centrales de R.","00012 - Docum. Desactualiza.","00013 - Docum. Falsa","00014 - Garantía Insuficiente","00015 - Capac. Pago de Codeud.","00016 - Mora Comercial Codeud.","00017 - Estab. Laboral Codeud." 
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CptoScoring AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .77
     BGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_MenDes AS CHARACTER FORMAT "X(80)":U 
      VIEW-AS TEXT 
     SIZE 89 BY .62
     BGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-224
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 55 BY 1.88
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-225
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 91 BY .96
     BGCOLOR 0 .

DEFINE BUTTON Btn_FinVS 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outultima 2" 
     SIZE 11 BY 1.62 TOOLTIP "Salva y cierra esta ventana".

DEFINE VARIABLE cmbEstdoCrgo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado Cargo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Propiedad","Provisionalidad","Por Contrato","A termino","Otros Servidores" 
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE Cmb_ConCte AS CHARACTER FORMAT "X(25)":U 
     LABEL "Conocimiento Cliente" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","Sin Referencias","Buenas Referencias","Buena Experiencia CFB","Malas Referencias","Sin experiencia CFB","Buenas Referencias Comerciales" 
     DROP-DOWN-LIST
     SIZE 35.14 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_destino AS CHARACTER FORMAT "X(25)":U 
     LABEL "Destino del Crédito" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "","Consumo","Inv.Patrimonial","Actividad Pdtiva","Reposicion de Activos Fijos","Capital de Trabajo" 
     DROP-DOWN-LIST
     SIZE 35.57 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_Gtia AS CHARACTER FORMAT "X(25)":U 
     LABEL "Garantía" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "HIPOTECA/FUENTE LIQUIDA","PRENDA","CODEUDOR(LIBRANZA)","APORTES","OTRAS","MIXTA","CODEUDOR(SIN LIBRANZA)" 
     DROP-DOWN-LIST
     SIZE 35.43 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_MoraCial AS CHARACTER 
     LABEL "Histórico Pagos Externo Sin DELPHI" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEMS "AL DIA","MORA 30","MORA 60","CALIFICACION C-D-E","CARTERA CASTIGADA","SIN EXPERIENCIA","SIN CONSULTA" 
     DROP-DOWN
     SIZE 35.43 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_ResPat AS CHARACTER FORMAT "X(25)":U 
     LABEL "Respaldo Patrimonial" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "","Sin Patrimonio","Prop.con Hipoteca","Prop.sin Hipoteca","Vehìculo con Prenda","Vehìculo sin Prenda","Prop.y Vehìculo","Inversiones" 
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_TipAct AS CHARACTER FORMAT "X(50)":U 
     LABEL "Tipo de Actividad" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","Empleado","Estudiante","Ama de Casa","Jubilado","Independiente","Independiente con Experiencia > 3 anos","Independiente con Experiencia 1 y 3 anos","Independiente con Experiencia < 6 Meses" 
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_Ocupacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ocupación" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEM-PAIRS "Servidor Público","5",
                     "Término Indefinido","1",
                     "Término Fijo","2",
                     "Pensionado","3",
                     "Independiente","4"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE iDelphi AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Delphi" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 TOOLTIP "Delphi" NO-UNDO.

DEFINE VARIABLE Nom_CteCodeu AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 61.43 BY .81
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tot_Egresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Egresos" 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tot_Ingresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Ingresos" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_LiqDispon AS DECIMAL FORMAT "->>>>,>>>,>>9":U INITIAL 0 
     LABEL "Liquidez Disponible" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 19  NO-UNDO.

DEFINE VARIABLE W_VrCuota AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Cuota este Crédito" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .85
     BGCOLOR 19  NO-UNDO.

DEFINE RECTANGLE RECT-213
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.29 BY 6.35.

DEFINE RECTANGLE RECT-214
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.14 BY 8.19.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.43 BY 1.15.

DEFINE BUTTON Btn_IrProcesarInst 
     LABEL "Ir a procesar instancia" 
     SIZE 22.57 BY 1.58.

DEFINE BUTTON BUTTON-233 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "Salir" 
     SIZE 7 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR_Admisible FOR 
      Garantias SCROLLING.

DEFINE QUERY Br_Cerradas FOR 
      TCerradas SCROLLING.

DEFINE QUERY Br_Codeudores FOR 
      TCode SCROLLING.

DEFINE QUERY Br_ConHV FOR 
      Hoja_Vida SCROLLING.

DEFINE QUERY Br_Consulta FOR 
      Consulta SCROLLING.

DEFINE QUERY Br_Cred FOR 
      TCred_ACanc SCROLLING.

DEFINE QUERY Br_Deducibles FOR 
      TDeducc SCROLLING.

DEFINE QUERY Br_Extras FOR 
      Extras SCROLLING.

DEFINE QUERY BR_Scoring FOR 
      TScoring SCROLLING.

DEFINE QUERY Br_Usuarios FOR 
      Tuxi SCROLLING.

DEFINE QUERY F_ForPago FOR 
      Anexos_Clientes SCROLLING.

DEFINE QUERY F_Solicitud FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR_Admisible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Admisible wWin _FREEFORM
  QUERY BR_Admisible NO-LOCK DISPLAY
      Garantias.Tipo_Garantia COLUMN-LABEL "Tipo"
      Garantias.Identificacion_Bien FORMAT "X(12)":U
      Garantias.Nom_Bien FORMAT "X(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 6.73
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cerradas wWin _FREEFORM
  QUERY Br_Cerradas NO-LOCK DISPLAY
      TCerradas.Instancia FORMAT "999":U COLUMN-LABEL "Ins"
      TCerradas.INom_Instancia COLUMN-LABEL "Nombre Instancia" FORMAT "x(30)"
      TCerradas.Usuario FORMAT "X(12)":U COLUMN-LABEL "Usuario"
      TCerradas.INom_Usuario COLUMN-LABEL "Nombre Usuario"
      TCerradas.Fec_Retiro FORMAT "99/99/9999":U   COLUMN-LABEL "Fec.Retiro"
      TCerradas.Descripcion COLUMN-LABEL "Descripcion" FORMAT "X(200)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Codeudores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Codeudores wWin _FREEFORM
  QUERY Br_Codeudores NO-LOCK DISPLAY
      TC_AgeCode COLUMN-LABEL "Age"
    TC_NitCode COLUMN-LABEL "Id" FORMAT "X(14)"
    TC_NomCode COLUMN-LABEL "Nombre" FORMAT "X(45)"
    TC_Aprob COLUMN-LABEL "Acept/Neg"
    TC_TelCdRs COLUMN-LABEL "Tel.Residencia" FORMAT "X(14)"
    TC_TelCdCo COLUMN-LABEL "Tel.Comercial" FORMAT "X(14)"
    TC_emlCode COLUMN-LABEL "Correo Electrónico"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 5.65
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ConHV wWin _FREEFORM
  QUERY Br_ConHV NO-LOCK DISPLAY
      Hoja_Vida.Fec_Grabacion FORMAT "99/99/9999":U
      Hoja_Vida.Observacion FORMAT "X(400)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 6.73
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Consulta wWin _FREEFORM
  QUERY Br_Consulta NO-LOCK DISPLAY
      Consulta.Num_Solicitud FORMAT "99999999":U COLUMN-LABEL "Solicitud"
    Consulta.AgeSolicitud
    Consulta.Estado   COLUMN-LABEL "Est" 
    Consulta.Nit FORMAT "X(12)":U
    Consulta.Nombre  FORM "X(36)"
    Consulta.Fec_Ingreso FORMAT "99/99/9999":U COLUMN-LABEL "Fecha"
    Consulta.Hor_Ingreso COLUMN-LABEL "Hora"
    Consulta.Monto COLUMN-LABEL "Monto"
    Consulta.Vigencia COLUMN-LABEL "Vigen."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 92 BY 13.19
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cred wWin _FREEFORM
  QUERY Br_Cred NO-LOCK DISPLAY
      TCred_ACanc.Agen FORMAT "99":U COLUMN-LABEL "Ag."
      TCred_ACanc.Pto FORMAT "999":U COLUMN-LABEL "Pto"
      TCred_ACanc.Tasa FORMAT ">>9.9999":U COLUMN-LABEL "Tasa"
      TCred_ACanc.NumC FORMAT ">>>>>>>>9":U COLUMN-LABEL "numCredito"
      TCred_ACanc.FecD FORMAT "99/99/9999":U COLUMN-LABEL "Fec_desembolso"
      TCred_ACanc.CuoC FORMAT ">,>>>,>>>,>>9":U COLUMN-LABEL "Cuota"
      TCred_ACanc.SdoTD FORMAT ">,>>>,>>>,>>9":U COLUMN-LABEL "Saldo_total"
      TCred_ACanc.CSiNo COLUMN-LABEL "Cancela?"
      TCred_ACanc.valorAbono FORMAT ">,>>>,>>>,>>9":U COLUMN-LABEL "Valor_Abono"
      ENABLE TCred_ACanc.CSiNo TCred_ACanc.valorAbono
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 6.08
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN TOOLTIP "Con S/N Selecciona SI o NO es Credito a Cancelarlo con esta Solicitud".

DEFINE BROWSE Br_Deducibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Deducibles wWin _FREEFORM
  QUERY Br_Deducibles NO-LOCK DISPLAY
      TDeducc.Nom_Deducible FORMAT "X(20)":U COLUMN-LABEL "Nombre Deducible"
    TDeducc.Valor FORMAT ">>>,>>>,>>9.99999":U COLUMN-LABEL "Valor"
    TDeducc.Valor_Impuesto FORMAT ">>>,>>>,>>9.99999":U COLUMN-LABEL "Impuesto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 53 BY 7.81
         BGCOLOR 15 FONT 2 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Extras wWin _FREEFORM
  QUERY Br_Extras NO-LOCK DISPLAY
      Extras.Agencia FORMAT "999":U COLUMN-LABEL "Ag."
      Extras.Cod_Credito FORMAT "999":U COLUMN-LABEL "Pdcto"
      Extras.Nit FORMAT "X(12)":U COLUMN-LABEL "Ced./Nit"
      Extras.Num_Solicitud FORMAT "99999999":U COLUMN-LABEL "No.Solicitud"
      Extras.Nro_Cuota FORMAT "9999":U COLUMN-LABEL "Pdo.Pago"
      Extras.Vr_CuoExtra FORMAT "->>>>,>>>,>>9.99":U COLUMN-LABEL "Vr.Cuota Extra"
      Extras.Fec_Vcto FORMAT "99/99/9999":U COLUMN-LABEL "Fecha-Vcto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70.72 BY 7.88
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN TOOLTIP "Con CLICK selecciona la Extra para ser Modificada y/o Eliminarla".

DEFINE BROWSE BR_Scoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR_Scoring wWin _FREEFORM
  QUERY BR_Scoring NO-LOCK DISPLAY
      VarS FORMAT "x(60)" COLUMN-LABEL "Variable"
    VVaS COLUMN-LABEL "Valor" 
    PunS COLUMN-LABEL "Puntaje"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 10.62
         BGCOLOR 15 FONT 2 ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Usuarios wWin _FREEFORM
  QUERY Br_Usuarios NO-LOCK DISPLAY
      Tuxi.Agencia FORMAT "999":U COLUMN-LABEL "Agencia"
    Tuxi.Usuario FORMAT "X(4)":U COLUMN-LABEL "Usuario"
    Tuxi.Nombre FORMAT "X(35)":U COLUMN-LABEL "Nombre" 
    Tuxi.Cantidad FORMAT ">>9":U COLUMN-LABEL "Cantidad"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 43 BY 6.19
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_VblesS
     cmbEstdoCrgo AT ROW 10.69 COL 46 COLON-ALIGNED WIDGET-ID 8
     Rs_Ocupacion AT ROW 10.69 COL 10 COLON-ALIGNED WIDGET-ID 4
     iDelphi AT ROW 14.42 COL 25 COLON-ALIGNED WIDGET-ID 2
     W_VrCuota AT ROW 8.5 COL 18.57 COLON-ALIGNED
     Nom_CteCodeu AT ROW 9.62 COL 15.72 COLON-ALIGNED NO-LABEL
     Clientes.Sdo_Obligaciones AT ROW 4.35 COL 52.29 COLON-ALIGNED
          LABEL "Deuda DataCrédito" FORMAT ">,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     W_LiqDispon AT ROW 8.5 COL 52.29 COLON-ALIGNED
     Clientes.Capacidad_Pago AT ROW 7.5 COL 52.29 COLON-ALIGNED
          LABEL "Capacidad de Pago" FORMAT "->>9.99"
          VIEW-AS FILL-IN 
          SIZE 7.29 BY .81
          BGCOLOR 19 
     Clientes.Salario AT ROW 1.73 COL 15.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_arriendos AT ROW 2.62 COL 15.86 COLON-ALIGNED
          LABEL "Arriendos"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_financieros AT ROW 3.5 COL 15.86 COLON-ALIGNED
          LABEL "Financieros"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_Honorarios AT ROW 4.35 COL 15.86 COLON-ALIGNED
          LABEL "Honorarios"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_Otros AT ROW 5.27 COL 15.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Gto_Familiar AT ROW 1.73 COL 52.14 COLON-ALIGNED
          LABEL "Gtos Sostenimiento" FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .73
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Gto_Arriendo AT ROW 2.62 COL 52.29 COLON-ALIGNED
          LABEL "Arrendamientos" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Clientes.Gto_obligacion AT ROW 3.5 COL 52.29 COLON-ALIGNED
          LABEL "Deducciones Colilla" FORMAT ">>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Tot_Egresos AT ROW 6.35 COL 52 COLON-ALIGNED
     Tot_Ingresos AT ROW 6.35 COL 15.86 COLON-ALIGNED
     Clientes.GtoFinanc_Indir AT ROW 5.27 COL 52.43 COLON-ALIGNED
          LABEL "Centrales Riesgo" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .81
          BGCOLOR 15 
     Cmb_destino AT ROW 11.65 COL 24.43 COLON-ALIGNED
     Cmb_Gtia AT ROW 12.62 COL 24.57 COLON-ALIGNED
     Cmb_MoraCial AT ROW 13.54 COL 1
     Btn_FinVS AT ROW 14.73 COL 64
     Cmb_ConCte AT ROW 15.31 COL 12
     Cmb_TipAct AT ROW 16.31 COL 14.42
     Cmb_ResPat AT ROW 17.27 COL 12.14
     "  Ingresos Mensuales" VIEW-AS TEXT
          SIZE 19 BY .73 AT ROW 1 COL 4.57
          FGCOLOR 7 FONT 5
     "  Egresos Mensuales" VIEW-AS TEXT
          SIZE 18.86 BY .73 AT ROW 1 COL 41
          FGCOLOR 7 FONT 5
     "Salva y regresa" VIEW-AS TEXT
          SIZE 11 BY .58 AT ROW 14.19 COL 64
          BGCOLOR 1 FGCOLOR 15 
     RECT-213 AT ROW 1.31 COL 2.57
     RECT-214 AT ROW 1.31 COL 37.43
     RECT-321 AT ROW 10.5 COL 2.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 18.14 ROW 3.73
         SIZE 79.57 BY 18.27
         FONT 4
         TITLE "Actualización Información de análisis".

DEFINE FRAME F_Extras
     Btn_AdExt AT ROW 2.23 COL 40
     W_PPExtra AT ROW 2.27 COL 3.72 COLON-ALIGNED NO-LABEL
     W_VrExtra AT ROW 2.27 COL 17.43 COLON-ALIGNED NO-LABEL
     Btn_EliExt AT ROW 3.65 COL 40.14
     BUTTON-170 AT ROW 4.73 COL 58.86
     Br_Extras AT ROW 6.46 COL 1.72
     "Pdo.Pago              Valor Cuota Extra" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 1.38 COL 4.72
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36 ROW 4.62
         SIZE 72.86 BY 14.38
         FONT 5
         TITLE "Extras Pactadas".

DEFINE FRAME F_CredACanc
     Br_Cred AT ROW 1.27 COL 2
     btnSalirAbonoCreditos AT ROW 7.46 COL 73
     W_TotCanc AT ROW 7.65 COL 54 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14.14 ROW 5.81
         SIZE 81.43 BY 8.88
         TITLE "Deudas a Cancelar con esta Solicitud: S/N en Columna C.Si/No".

DEFINE FRAME F_Creditos
     Cmb_Instancias AT ROW 1.27 COL 10 COLON-ALIGNED
     BUTTON-169 AT ROW 1.27 COL 82
     Btn_Imprimir AT ROW 1.54 COL 109.72
     PDF AT ROW 1.81 COL 102 WIDGET-ID 6
     Btn_ProcesarInstancia AT ROW 2.35 COL 12
     T_Refresh AT ROW 2.35 COL 48
     Btn_ImpCA AT ROW 2.5 COL 82
     Btn_Consulta AT ROW 3.42 COL 109.72
     BUTTON-164 AT ROW 5.31 COL 109.72
     Btn_Anexos AT ROW 7.46 COL 109.72 WIDGET-ID 2
     Btn_Salvar AT ROW 9.62 COL 109.72
     Btn_Deshacer AT ROW 11.42 COL 109.72
     Btn_Ingresar AT ROW 13.23 COL 109.72
     Btn_Cancelar AT ROW 15.04 COL 109.72
     BUTTON-9 AT ROW 16.85 COL 109.72
     BUTTON-2 AT ROW 18.65 COL 109.72
     NomUsuario AT ROW 1.27 COL 45 COLON-ALIGNED NO-LABEL
     RECT-3 AT ROW 1.27 COL 108.72
     RECT-347 AT ROW 9.35 COL 109.14 WIDGET-ID 4
     SPACE(0.00) SKIP(14.15)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 5.

DEFINE FRAME F_Solicitud
     Cmb_Agencias AT ROW 1.04 COL 10 COLON-ALIGNED
     Solicitud.Num_Solicitud AT ROW 1.08 COL 74.57 COLON-ALIGNED
          LABEL "Solicitud"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 3 FGCOLOR 15 
     Solicitud.Fec_Solicitud AT ROW 1.08 COL 93.86 COLON-ALIGNED
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 3 FGCOLOR 15 
     NomNit AT ROW 2.19 COL 33 COLON-ALIGNED NO-LABEL
     BUTTON-19 AT ROW 2.19 COL 102.72
     Solicitud.Nit AT ROW 2.35 COL 14.43 COLON-ALIGNED
          LABEL "Cédula/Nit"
          VIEW-AS FILL-IN 
          SIZE 16.57 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Fec_Asociado AT ROW 3.19 COL 14.57 COLON-ALIGNED HELP
          "Fecha en que ingresa como asociado a la Organización" WIDGET-ID 354
     BUTTON-103 AT ROW 3.69 COL 101.43
     Nom_Producto AT ROW 3.77 COL 40.57 COLON-ALIGNED
     WX_Edad AT ROW 4.04 COL 27 COLON-ALIGNED
     Solicitud.DestinoF AT ROW 4.27 COL 100 COLON-ALIGNED NO-LABEL WIDGET-ID 346
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          FGCOLOR 17 
     Nom_Destino AT ROW 4.62 COL 70.57 COLON-ALIGNED WIDGET-ID 20
     Btn_Destino AT ROW 4.77 COL 101.43 WIDGET-ID 344
     WAntiguedad AT ROW 4.92 COL 27 COLON-ALIGNED WIDGET-ID 356
     WVeces AT ROW 5.77 COL 27 COLON-ALIGNED WIDGET-ID 358
     Texto1 AT ROW 5.81 COL 33.72 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     Texto2 AT ROW 6.5 COL 33.72 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     WMonto AT ROW 7.42 COL 17 COLON-ALIGNED WIDGET-ID 362
     btnGrntias AT ROW 7.46 COL 85.43 WIDGET-ID 6
     WTasa AT ROW 7.65 COL 82 RIGHT-ALIGNED WIDGET-ID 360
     Solicitud.For_Interes AT ROW 8 COL 37.72 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Vencido", 1,
"Anticipado", 2
          SIZE 23.29 BY .81
          FONT 5
     Solicitud.Tasa AT ROW 8.15 COL 71 COLON-ALIGNED
          LABEL "Efectiva" FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 3 FGCOLOR 15 
     Solicitud.Monto AT ROW 8.27 COL 17 COLON-ALIGNED
          LABEL "Monto solicitud"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 FONT 5
     Btn_Gar AT ROW 8.54 COL 85.43
     Cmb_Sistemas AT ROW 9.04 COL 42.57 COLON-ALIGNED NO-TAB-STOP 
     Solicitud.Plazo AT ROW 9.12 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 FONT 5
     W_TasaNominal AT ROW 9.12 COL 71 COLON-ALIGNED
     btnFrmlrioAfliacion AT ROW 9.62 COL 85.43 WIDGET-ID 4 NO-TAB-STOP 
     Solicitud.fec_desembolso AT ROW 10 COL 19 COLON-ALIGNED WIDGET-ID 390
          LABEL "Fecha Desembolso"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .85
          FONT 4
     Cmb_PerPago AT ROW 10.08 COL 42.57 COLON-ALIGNED NO-TAB-STOP 
     W_TasaPeriodo AT ROW 10.15 COL 71 COLON-ALIGNED
     Btn_Proyectar AT ROW 10.69 COL 85.43 NO-TAB-STOP 
     Solicitud.fec_primerPago AT ROW 10.88 COL 19 COLON-ALIGNED WIDGET-ID 392
          LABEL "Fecha primer pago"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .85
          FONT 4
     NomIndicador AT ROW 11.5 COL 43 COLON-ALIGNED
     BUTTON-120 AT ROW 11.77 COL 85.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 106 BY 20
         FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Solicitud
     Solicitud.Cuota AT ROW 11.92 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY .85
          BGCOLOR 3 FGCOLOR 15 FONT 5
     Solicitud.Deducible AT ROW 12.81 COL 19 COLON-ALIGNED
          LABEL "Valor Deducible"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 3 FGCOLOR 15 
     BUTTON-100 AT ROW 12.85 COL 85.43
     W_ForPago AT ROW 12.96 COL 49 COLON-ALIGNED
     BUTTON-107 AT ROW 12.96 COL 81
     Solicitud.Total_Prestamo AT ROW 13.65 COL 19 COLON-ALIGNED
          LABEL "Total a Prestar"
          VIEW-AS FILL-IN 
          SIZE 13 BY .77
          BGCOLOR 3 FGCOLOR 15 
     W_Desembolso AT ROW 13.73 COL 49 COLON-ALIGNED
     BUTTON-105 AT ROW 13.81 COL 81.14
     Btn_HojaVida AT ROW 13.92 COL 85.43
     Btn_Extras AT ROW 14.92 COL 2.14
     BUTTON-102 AT ROW 15 COL 85.43
     W_VrCredACanc AT ROW 15.31 COL 67.29 COLON-ALIGNED
     Btn_CancCred AT ROW 15.42 COL 81.14
     W_TotExt AT ROW 16.04 COL 18.86 COLON-ALIGNED
     W_VrADesemb AT ROW 16.08 COL 67.29 COLON-ALIGNED
     Btn_Liquidar AT ROW 16.12 COL 85.43
     W_Tipo_Credito AT ROW 4.65 COL 40.57 COLON-ALIGNED
     W_NomPdo AT ROW 12.04 COL 2 NO-LABEL
     "Tasas" VIEW-AS TEXT
          SIZE 5.57 BY .54 AT ROW 7.19 COL 65
     "Interés" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 7.58 COL 45.86 WIDGET-ID 350
     RECT-322 AT ROW 7.46 COL 36 WIDGET-ID 8
     RECT-323 AT ROW 7.46 COL 64 WIDGET-ID 10
     RECT-325 AT ROW 7.92 COL 36.57 WIDGET-ID 352
     RECT-327 AT ROW 14.81 COL 1.43 WIDGET-ID 368
     RECT-330 AT ROW 3.42 COL 35.57 WIDGET-ID 374
     RECT-331 AT ROW 2.04 COL 1.43 WIDGET-ID 376
     RECT-333 AT ROW 12.85 COL 36 WIDGET-ID 380
     RECT-334 AT ROW 7.19 COL 1.43 WIDGET-ID 388
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.62
         SIZE 106 BY 20
         FONT 5
         TITLE "Radicación de Solicitudes".

DEFINE FRAME F_HojaVida
     Hoja_Vida.Fec_Grabacion AT ROW 1.27 COL 70 COLON-ALIGNED
          LABEL "Fecha Grabación"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-150 AT ROW 1.27 COL 86
     Hoja_Vida.Asunto_Cumplido AT ROW 1.54 COL 4
          LABEL "Asunto Cumplido"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Hoja_Vida.Observacion AT ROW 2.62 COL 4 NO-LABEL
          VIEW-AS EDITOR LARGE
          SIZE 80 BY 7.08
          BGCOLOR 15 
     Btn_SalvaHV AT ROW 3.15 COL 86
     Btn_NvoHv AT ROW 4.5 COL 86
     BUTTON-152 AT ROW 5.85 COL 86
     BUTTON-149 AT ROW 8 COL 86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.04
         SIZE 98 BY 9.96
         FONT 5
         TITLE "Hoja de Vida".

DEFINE FRAME VisorSolicitud
     BUTTON-233 AT ROW 18.27 COL 100 WIDGET-ID 10
     Btn_IrProcesarInst AT ROW 18.31 COL 76.86 WIDGET-ID 14
     "Si es necesario, puede actualizar los datos del sistema y volver a generar PDF." VIEW-AS TEXT
          SIZE 74.86 BY .62 AT ROW 18.77 COL 2.14 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.42
         SIZE 107 BY 19.92
         TITLE "Visor de solicitud" WIDGET-ID 200.

DEFINE FRAME F_Consulta
     Br_Consulta AT ROW 2.08 COL 4
     R_Organizar AT ROW 15.54 COL 25 NO-LABEL
     Btn_OutConsulta AT ROW 16.73 COL 88
     Buscar AT ROW 17.15 COL 19 COLON-ALIGNED
     VG_Normal AT ROW 1.23 COL 3 COLON-ALIGNED NO-LABEL
     VG_Media AT ROW 1.27 COL 33.72 COLON-ALIGNED NO-LABEL
     VG_Alta AT ROW 1.27 COL 64 COLON-ALIGNED NO-LABEL
     "Organizada por..." VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 15.54 COL 7
          FGCOLOR 7 
     "" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 17.15 COL 42
          FGCOLOR 7 
     RECT-223 AT ROW 15.27 COL 4
     RECT-287 AT ROW 1 COL 35
     RECT-288 AT ROW 1 COL 4
     RECT-289 AT ROW 1 COL 65
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.69
         SIZE 98 BY 18.31
         FONT 5
         TITLE "Solicitudes Disponibles".

DEFINE FRAME F_Scoring
     BR_Scoring AT ROW 1.04 COL 1
     M_Cliente AT ROW 11.81 COL 54.72
     Nom_CteCod AT ROW 12.46 COL 27.14 COLON-ALIGNED NO-LABEL
     BUTTON-166 AT ROW 12.5 COL 26
     W_Concepto AT ROW 13.46 COL 36.57 COLON-ALIGNED
     Total_Puntaje AT ROW 14.5 COL 36.57 COLON-ALIGNED
     BUT-IMP-Scoring AT ROW 15.54 COL 25.86 WIDGET-ID 2
     Btn_Ejecutar AT ROW 15.54 COL 43 WIDGET-ID 36
     BUTTON-99 AT ROW 15.54 COL 60.14
     "Cada Codeudor" VIEW-AS TEXT
          SIZE 14.57 BY .62 AT ROW 11.77 COL 26.29
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.23
         SIZE 98 BY 17.23
         FONT 5
         TITLE "Datos del Scoring de Créditos".

DEFINE FRAME F_Producto
     Solicitud.Tip_Credito AT ROW 1 COL 3 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Consumo", 1,
"Comercial", 2,
"Hipotecario", 3,
"Microcrédito", 4
          SIZE 52 BY 1.08
     Cmb_Productos AT ROW 2.65 COL 10.29 COLON-ALIGNED
     Btn_OutProductos AT ROW 3.92 COL 43.86
     BUTTON-131 AT ROW 4.19 COL 11.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 28 ROW 10.96
         SIZE 57 BY 5.65
         FONT 5
         TITLE BGCOLOR 11 "Producto de la Solicitud".

DEFINE FRAME F_InfoProducto
     S_InfoProducto AT ROW 1.27 COL 1.72 NO-LABEL
     Btn_OutScoring AT ROW 9.15 COL 43.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 44.43 ROW 7.73
         SIZE 53.57 BY 10.77
         FGCOLOR 0 FONT 5
         TITLE "Información del Producto".

DEFINE FRAME F_InfoCliente
     S_InfoCliente AT ROW 1.27 COL 2 NO-LABEL WIDGET-ID 2
     BUTTON-156 AT ROW 13.65 COL 2
     BUTTON-108 AT ROW 13.65 COL 42
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 51 BY 15.35
         FONT 5
         TITLE "Información Financiera del Cliente".

DEFINE FRAME F_Garantias
     R_TipoGarantia AT ROW 1.27 COL 17.43 NO-LABEL
     Btn_OutGarantias AT ROW 12.31 COL 90
     RECT-226 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 6.65
         SIZE 98 BY 15.35
         FONT 5
         TITLE "Garantías".

DEFINE FRAME F_ConAdmisible
     BR_Admisible AT ROW 1.27 COL 2
     Btn_OutConAdm AT ROW 8.27 COL 85
     R_ConAdm AT ROW 8.54 COL 2.57 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 95 BY 9.96
         FONT 5
         TITLE BGCOLOR 3 "Consulta de Garantías Admisibles y No-Admisibles".

DEFINE FRAME F_Codeudores
     Relaciones.Aprobada AT ROW 1.27 COL 78.72 HELP
          "Aprobada/Negada" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Aceptado", yes,
"Rechazado", no
          SIZE 14.14 BY 1.35 TOOLTIP "Marque si el Codeudor es Aceptado Ò No."
     W_NitCodeudor AT ROW 1.54 COL 14 COLON-ALIGNED
     W_NomCodeudor AT ROW 1.54 COL 27 COLON-ALIGNED NO-LABEL
     BUTTON-155 AT ROW 1.58 COL 72.14
     Btn_CreCod AT ROW 2.62 COL 3
     Btn_CanCod AT ROW 2.62 COL 13.86
     Btn_Activas AT ROW 2.62 COL 25.43
     Btn_SalCod AT ROW 2.62 COL 36.29
     BUTTON-165 AT ROW 2.69 COL 50.86
     RActivas AT ROW 2.88 COL 73 NO-LABEL
     Br_Codeudores AT ROW 3.96 COL 3
     RECT-297 AT ROW 1 COL 77
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 95 BY 9.69
         FONT 5
         TITLE "Personal".

DEFINE FRAME F_Admisible
     Garantias.Descripcion_Bien2 AT ROW 4.46 COL 3 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 44 BY 1.62
          BGCOLOR 15 
     W_CredAval AT ROW 9.96 COL 45.86 COLON-ALIGNED NO-LABEL
     Garantias.Aprobada AT ROW 9.46 COL 74.86 HELP
          "Aprobada/Negada" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Aprobada", yes,
"Negada", no
          SIZE 19.43 BY .62 TOOLTIP "Marque si la Garantìa fue Ò no aprobada"
     Garantias.Tipo_Garantia AT ROW 1.04 COL 3 HELP
          "" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Propiedad", 1,
"Aportes", 6,
"Prenda", 2,
"Inversión", 3,
"Cdat-Contrac NoAd", 4,
"Seguro Deudor", 5,
"Otras", 6
          SIZE 91 BY .73
     Garantias.Estado AT ROW 2.88 COL 2 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 17.86 BY .81
     Garantias.Identificacion_Bien AT ROW 1.81 COL 30.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     Garantias.Nom_Bien AT ROW 1.81 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 48.43 BY .81
          BGCOLOR 15 
     Garantias.Val_Bien AT ROW 2.73 COL 30.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     Garantias.Fec_Creacion AT ROW 2.81 COL 56.43 COLON-ALIGNED
          LABEL "Ing de Garantía"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Fec_Retiro AT ROW 2.77 COL 78.72 COLON-ALIGNED
          LABEL "Fec Retiro"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Garantias.Descripcion_Bien AT ROW 4.46 COL 48 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 47 BY 1.62
          BGCOLOR 15 FONT 4
     Garantias.Nro_Seguro AT ROW 6.58 COL 16 COLON-ALIGNED
          LABEL "Número de Seguro"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Nit_Aseguradora AT ROW 7.46 COL 16 COLON-ALIGNED
          LABEL "Nit Aseguradora"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Nom_Aseguradora AT ROW 8.42 COL 1 COLON-ALIGNED NO-LABEL
     Garantias.Fec_IniSeguro AT ROW 9.31 COL 16 COLON-ALIGNED
          LABEL "Fecha Inicio Seguro"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Fec_FinSeguro AT ROW 10.19 COL 16 COLON-ALIGNED
          LABEL "Fecha Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Val_Asegurado AT ROW 11 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Nom_Impuesto AT ROW 6.65 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Fec_VctoImpuesto AT ROW 7.54 COL 46 COLON-ALIGNED
          LABEL "Fec.Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Val_Impuesto AT ROW 8.38 COL 46 COLON-ALIGNED
          LABEL "Valor"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 95 BY 13.46
         FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Admisible
     Garantias.Fec_ProxAvaluo AT ROW 6.58 COL 77 COLON-ALIGNED
          LABEL "Fec.Próx.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Fec_UltAvaluo AT ROW 7.42 COL 77 COLON-ALIGNED
          LABEL "Fec.Últ.Avaluo"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Garantias.Val_UltAvaluo AT ROW 8.23 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Nom_UsuGarantia AT ROW 10.96 COL 62 COLON-ALIGNED NO-LABEL
     Btn_ConAdm AT ROW 11.77 COL 79
     BUTTON-161 AT ROW 11.77 COL 87
     Btn_SalAdm AT ROW 12.31 COL 2
     Btn_CanAdm AT ROW 12.31 COL 16
     Btn_IngAdm AT ROW 12.31 COL 30.57
     Btn_InaAdm AT ROW 12.31 COL 44.43
     W_DispGaran AT ROW 11.23 COL 45.86 COLON-ALIGNED NO-LABEL
     Btn_Borra AT ROW 12.31 COL 59.43
     "Valor Crèditos Avalados + Vr.Solicitado" VIEW-AS TEXT
          SIZE 27 BY .5 AT ROW 9.42 COL 34.86
          FGCOLOR 7 
     "Descripción del Bien" VIEW-AS TEXT
          SIZE 18 BY .5 AT ROW 3.92 COL 3.14
          FGCOLOR 7 
     "Valor Disponible de esta Garantia" VIEW-AS TEXT
          SIZE 27 BY .5 AT ROW 10.73 COL 34.86
          FGCOLOR 7 
     "  Seguro" VIEW-AS TEXT
          SIZE 8 BY .58 AT ROW 6.08 COL 3.43
          FGCOLOR 7 
     "  Impuesto" VIEW-AS TEXT
          SIZE 8 BY .58 AT ROW 6.12 COL 35
          FGCOLOR 7 FONT 4
     "  Avaluo" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 6.12 COL 65
          FGCOLOR 7 FONT 4
     "Concepto Abogado:" VIEW-AS TEXT
          SIZE 14.14 BY .5 AT ROW 3.88 COL 48
          FGCOLOR 7 
     "La garantìa es :" VIEW-AS TEXT
          SIZE 9.57 BY .5 AT ROW 9.54 COL 64.29
     "Usuario que Ingreso" VIEW-AS TEXT
          SIZE 14.43 BY .5 AT ROW 10.42 COL 64
          FGCOLOR 7 
     RECT-293 AT ROW 6.38 COL 2
     RECT-294 AT ROW 6.38 COL 34
     RECT-295 AT ROW 6.38 COL 64
     RECT-296 AT ROW 9.23 COL 74.43
     RECT-299 AT ROW 2.77 COL 1.14
     RECT-301 AT ROW 9.35 COL 34
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 95 BY 13.46
         FONT 4
         TITLE "Garantías Admisibles".

DEFINE FRAME F_Foto
     BUTTON-230 AT ROW 6.12 COL 5 WIDGET-ID 4
     P_Foto AT ROW 1 COL 1.43 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 70 ROW 1
         SIZE 19 BY 6.73
         TITLE "Foto del Asociado"
         CANCEL-BUTTON BUTTON-230 WIDGET-ID 100.

DEFINE FRAME F_ForPago
     Solicitud.For_Pago AT ROW 1.27 COL 1 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Caja", 1,
"Nómina", 2,
"Déb Aut", 3,
"Nóm Crec", 4,
"Prima", 5
          SIZE 45 BY .81
     Anexos_Clientes.Cam_Cat1 AT ROW 2.35 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Capacidad Pago" FORMAT "x(15)"
          VIEW-AS COMBO-BOX SORT INNER-LINES 4
          LIST-ITEM-PAIRS ""," ",
                     "50%","f50%",
                     "SMMLV","fsmmlv",
                     "CAJA","fcapagocaja"
          DROP-DOWN-LIST
          SIZE 16 BY 1 TOOLTIP "Función Para Calcular La Capacidad De Pago"
     Solicitud.Age_DebAutomatico AT ROW 3.42 COL 15 COLON-ALIGNED
          LABEL "Agencia"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomAgeDebAutomatico AT ROW 3.42 COL 19 COLON-ALIGNED NO-LABEL
     Solicitud.Cod_DebAutomatico AT ROW 4.5 COL 15 COLON-ALIGNED
          LABEL "Producto"
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomCodDebAutomatico AT ROW 4.5 COL 19 COLON-ALIGNED NO-LABEL
     Solicitud.Cue_DebAutomatico AT ROW 5.58 COL 15 COLON-ALIGNED
          LABEL "Cuenta"
          VIEW-AS FILL-IN 
          SIZE 29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-106 AT ROW 6.65 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 18 ROW 10.42
         SIZE 46 BY 8.35
         FONT 5
         TITLE BGCOLOR 16 "Forma de Pago de la Cuota".

DEFINE FRAME F_Desembolso
     Solicitud.Desembolso AT ROW 1.27 COL 3 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Efectivo", 1,
"Cheque", 2,
"Cta Ahorros", 3,
"Orden a 3os", 4
          SIZE 52 BY .81
     Solicitud.Age_Desembolso AT ROW 2.35 COL 10 COLON-ALIGNED
          LABEL "Agencia"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     W_NomAgeDesembolso AT ROW 2.35 COL 20 COLON-ALIGNED NO-LABEL
     Solicitud.Cod_Desembolso AT ROW 3.42 COL 10 COLON-ALIGNED
          LABEL "Producto"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     W_NomProDesembolso AT ROW 3.42 COL 20 COLON-ALIGNED NO-LABEL
     Solicitud.Cue_Desembolso AT ROW 4.5 COL 10 COLON-ALIGNED
          LABEL "Cuenta"
          VIEW-AS FILL-IN 
          SIZE 41 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-104 AT ROW 5.58 COL 46
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 32 ROW 6.92
         SIZE 55 BY 7.27
         FONT 5
         TITLE "Destino del Desembolso".

DEFINE FRAME F_Deducibles
     Br_Deducibles AT ROW 1.27 COL 2
     BUTTON-101 AT ROW 9.35 COL 45
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 10.69
         SIZE 55 BY 11.31
         TITLE "Deducibles del Producto".

DEFINE FRAME F_ConHV
     Br_ConHV AT ROW 1.27 COL 3
     Btn_OutConHV AT ROW 8.27 COL 89
     "   Los Mensajes en Rojo estan pendientes por cumplirse" VIEW-AS TEXT
          SIZE 85 BY 1.08 AT ROW 8.54 COL 3
          BGCOLOR 0 FGCOLOR 15 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.04
         SIZE 98 BY 9.96
         FONT 4
         TITLE "Asuntos Pendientes".

DEFINE FRAME F_Ultima
     Btn_SalvaUltima AT ROW 1.27 COL 85
     Btn_OutUltima AT ROW 2.62 COL 85
     Solicitud.Estado AT ROW 3.27 COL 21 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "En estudio", 1,
"Aprobada", 2,
"Negada", 3,
"Condicionada", 4
          SIZE 56 BY .69
     Cmb_Negadas AT ROW 4.08 COL 53 COLON-ALIGNED
     W_CptoScoring AT ROW 4.23 COL 16 COLON-ALIGNED NO-LABEL
     W_MenDes AT ROW 5.31 COL 4 COLON-ALIGNED NO-LABEL
     "Se ha llegado a la última instancia en el proceso de solicitud," VIEW-AS TEXT
          SIZE 53 BY .81 AT ROW 1.54 COL 22
          BGCOLOR 0 FGCOLOR 15 
     "Concepto Scoring :" VIEW-AS TEXT
          SIZE 16.72 BY .62 AT ROW 4.23 COL 1
     "Cambie el estado de la Solicitud según el estudio realizado" VIEW-AS TEXT
          SIZE 53 BY .62 AT ROW 2.35 COL 22
          BGCOLOR 0 FGCOLOR 15 
     RECT-224 AT ROW 1.27 COL 21
     RECT-225 AT ROW 5.15 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         PAGE-TOP SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 98 BY 18.31
         FGCOLOR 15 FONT 5
         TITLE "Estado de la Solicitud".

DEFINE FRAME F_Condicionada
     Cmb_InsCon AT ROW 1.27 COL 8.43 COLON-ALIGNED
     Br_Usuarios AT ROW 2.08 COL 51
     E_Condicion AT ROW 3.15 COL 3 NO-LABEL
     BUTTON-162 AT ROW 9.88 COL 82
     "Condición" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 2.35 COL 3
     "  Usuarios Disponibles para recibir la Solicitud" VIEW-AS TEXT
          SIZE 40.29 BY .81 AT ROW 1.27 COL 52
          BGCOLOR 18 FGCOLOR 15 
     "Los Usuarios en Rojo han tenido la Solicitud asignada" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 8.81 COL 52
          FGCOLOR 15 FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 6.12
         SIZE 95 BY 12.38
         FGCOLOR 15 FONT 5
         TITLE "Instancia a la cual se devuelve la Solicitud Condicionada".

DEFINE FRAME F_Cerradas
     Br_Cerradas AT ROW 1.27 COL 3
     Btn_OutCerradas AT ROW 7.73 COL 89
     BUTTON-154 AT ROW 8 COL 62
     "La instancia activa se encuentra en letra color rojo" VIEW-AS TEXT
          SIZE 43 BY .81 AT ROW 7.19 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.69
         SIZE 98 BY 9.42
         FGCOLOR 0 FONT 4
         TITLE "Consulta de Instancias Procesadas y Actuales".

DEFINE FRAME F_Agregar
     E_Agregar AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-153 AT ROW 6.92 COL 48
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 11.5
         SIZE 57 BY 8.88
         TITLE "Texto a ser Agregado".

DEFINE FRAME F_Instancias
     BUTTON-142 AT ROW 1.27 COL 3
     Mov_Instancias.Fec_Ingreso AT ROW 1.27 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WHora_Ingreso AT ROW 1.27 COL 85 COLON-ALIGNED NO-LABEL
     W_Instancia AT ROW 1.42 COL 12 COLON-ALIGNED NO-LABEL
     W_UsuarioInstancia AT ROW 2.35 COL 12 COLON-ALIGNED NO-LABEL
     Mov_Instancias.Fec_Retiro AT ROW 2.35 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Whora_Retiro AT ROW 2.35 COL 85 COLON-ALIGNED NO-LABEL
     Vigencia AT ROW 3.42 COL 48 COLON-ALIGNED
     Mov_Instancias.Estado AT ROW 3.54 COL 3
          LABEL "Cerrar Instancia"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
     Mov_Instancias.Descripcion AT ROW 4.5 COL 3 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 1200 SCROLLBAR-VERTICAL LARGE
          SIZE 77 BY 9.69
          BGCOLOR 15 
     Btn_GraInstancia AT ROW 4.5 COL 82
     Btn_AgregarTxt AT ROW 6.38 COL 82
     Btn_Imp AT ROW 9.69 COL 82
     Btn_insVolver AT ROW 12.58 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.46
         SIZE 98 BY 14.54
         FONT 5
         TITLE "Procesar Instancias".


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
         TITLE              = "Proceso de Solicitudes"
         HEIGHT             = 22.69
         WIDTH              = 127.29
         MAX-HEIGHT         = 36.54
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.54
         VIRTUAL-WIDTH      = 182.86
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
ASSIGN FRAME F_Admisible:FRAME = FRAME F_Garantias:HANDLE
       FRAME F_Agregar:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Cerradas:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Codeudores:FRAME = FRAME F_Garantias:HANDLE
       FRAME F_ConAdmisible:FRAME = FRAME F_Garantias:HANDLE
       FRAME F_Condicionada:FRAME = FRAME F_Ultima:HANDLE
       FRAME F_ConHV:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Consulta:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Deducibles:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Desembolso:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_ForPago:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Foto:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Garantias:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_HojaVida:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_InfoCliente:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_InfoProducto:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Instancias:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Producto:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Scoring:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Solicitud:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Ultima:FRAME = FRAME F_Creditos:HANDLE
       FRAME VisorSolicitud:FRAME = FRAME F_Creditos:HANDLE.

/* SETTINGS FOR FRAME F_Admisible
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Admisible:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET Garantias.Aprobada IN FRAME F_Admisible
   EXP-HELP                                                             */
/* SETTINGS FOR RADIO-SET Garantias.Estado IN FRAME F_Admisible
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Garantias.Fec_Creacion IN FRAME F_Admisible
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_FinSeguro IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_IniSeguro IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_ProxAvaluo IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_Retiro IN FRAME F_Admisible
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Garantias.Fec_UltAvaluo IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Fec_VctoImpuesto IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nit_Aseguradora IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Nom_Aseguradora IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nom_Bien IN FRAME F_Admisible
   6 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Nom_UsuGarantia IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Garantias.Nro_Seguro IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Garantias.Tipo_Garantia IN FRAME F_Admisible
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Garantias.Val_Impuesto IN FRAME F_Admisible
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_CredAval IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DispGaran IN FRAME F_Admisible
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Agregar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Agregar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Cerradas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Cerradas TEXT-5 F_Cerradas */
ASSIGN 
       FRAME F_Cerradas:HIDDEN           = TRUE.

ASSIGN 
       Br_Cerradas:ALLOW-COLUMN-SEARCHING IN FRAME F_Cerradas = TRUE.

/* SETTINGS FOR FRAME F_Codeudores
                                                                        */
/* BROWSE-TAB Br_Codeudores RActivas F_Codeudores */
/* SETTINGS FOR RADIO-SET Relaciones.Aprobada IN FRAME F_Codeudores
   EXP-HELP                                                             */
/* SETTINGS FOR BUTTON Btn_CanCod IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_SalCod IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NitCodeudor IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCodeudor IN FRAME F_Codeudores
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_ConAdmisible
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BR_Admisible 1 F_ConAdmisible */
ASSIGN 
       FRAME F_ConAdmisible:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Condicionada
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Usuarios Cmb_InsCon F_Condicionada */
ASSIGN 
       FRAME F_Condicionada:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConHV
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_ConHV TEXT-12 F_ConHV */
ASSIGN 
       FRAME F_ConHV:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Consulta
                                                                        */
/* BROWSE-TAB Br_Consulta RECT-289 F_Consulta */
/* SETTINGS FOR FILL-IN VG_Alta IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VG_Media IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VG_Normal IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_CredACanc
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB Br_Cred 1 F_CredACanc */
ASSIGN 
       FRAME F_CredACanc:HIDDEN           = TRUE
       FRAME F_CredACanc:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN W_TotCanc IN FRAME F_CredACanc
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Creditos
   Size-to-Fit                                                          */
ASSIGN 
       FRAME F_Creditos:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Creditos
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Creditos
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME F_Creditos
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Creditos
   2                                                                    */
/* SETTINGS FOR BUTTON BUTTON-164 IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-9 IN FRAME F_Creditos
   2                                                                    */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON PDF IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Deducibles
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Deducibles 1 F_Deducibles */
ASSIGN 
       FRAME F_Deducibles:HIDDEN           = TRUE
       FRAME F_Deducibles:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Desembolso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Desembolso:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Solicitud.Age_Desembolso IN FRAME F_Desembolso
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Solicitud.Cod_Desembolso IN FRAME F_Desembolso
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Solicitud.Cue_Desembolso IN FRAME F_Desembolso
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_NomAgeDesembolso IN FRAME F_Desembolso
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomProDesembolso IN FRAME F_Desembolso
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Extras
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Extras BUTTON-170 F_Extras */
ASSIGN 
       FRAME F_Extras:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ForPago
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_ForPago:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Solicitud.Age_DebAutomatico IN FRAME F_ForPago
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX Anexos_Clientes.Cam_Cat1 IN FRAME F_ForPago
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Solicitud.Cod_DebAutomatico IN FRAME F_ForPago
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Solicitud.Cue_DebAutomatico IN FRAME F_ForPago
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_NomAgeDebAutomatico IN FRAME F_ForPago
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCodDebAutomatico IN FRAME F_ForPago
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Foto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Foto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Garantias
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Garantias:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_HojaVida
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_HojaVida:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.Fec_Grabacion IN FRAME F_HojaVida
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       Hoja_Vida.Observacion:READ-ONLY IN FRAME F_HojaVida        = TRUE.

/* SETTINGS FOR FRAME F_InfoCliente
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoCliente:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR S_InfoCliente IN FRAME F_InfoCliente
   NO-ENABLE                                                            */
ASSIGN 
       S_InfoCliente:READ-ONLY IN FRAME F_InfoCliente        = TRUE.

/* SETTINGS FOR FRAME F_InfoProducto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoProducto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Instancias
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Instancias:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR Mov_Instancias.Descripcion IN FRAME F_Instancias
   NO-ENABLE                                                            */
ASSIGN 
       Mov_Instancias.Descripcion:READ-ONLY IN FRAME F_Instancias        = TRUE.

/* SETTINGS FOR TOGGLE-BOX Mov_Instancias.Estado IN FRAME F_Instancias
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Mov_Instancias.Fec_Ingreso IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_Instancias.Fec_Retiro IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Vigencia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WHora_Ingreso IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Whora_Retiro IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Instancia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_UsuarioInstancia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Producto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Producto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Scoring
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BR_Scoring TEXT-22 F_Scoring */
ASSIGN 
       FRAME F_Scoring:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Nom_CteCod IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Total_Puntaje IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Concepto IN FRAME F_Scoring
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Solicitud
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Solicitud:HIDDEN           = TRUE
       FRAME F_Solicitud:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR BUTTON BUTTON-100 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Sistemas IN FRAME F_Solicitud
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Solicitud.Cuota IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Solicitud.Deducible IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Solicitud.DestinoF IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fec_Asociado IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Solicitud.fec_desembolso IN FRAME F_Solicitud
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Solicitud.fec_primerPago IN FRAME F_Solicitud
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Solicitud.Fec_Solicitud IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Solicitud.For_Interes IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Solicitud.Monto IN FRAME F_Solicitud
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN Solicitud.Nit IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN NomIndicador IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNit IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Destino IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Producto IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Solicitud.Num_Solicitud IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Solicitud.Plazo IN FRAME F_Solicitud
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Solicitud.Tasa IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Texto1 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Texto2 IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Solicitud.Total_Prestamo IN FRAME F_Solicitud
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN WAntiguedad IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WMonto IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WTasa IN FRAME F_Solicitud
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       WTasa:HIDDEN IN FRAME F_Solicitud           = TRUE.

/* SETTINGS FOR FILL-IN WVeces IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WX_Edad IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Desembolso IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ForPago IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomPdo IN FRAME F_Solicitud
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_TasaNominal IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TasaPeriodo IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Tipo_Credito IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotExt IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrADesemb IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrCredACanc IN FRAME F_Solicitud
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Ultima
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Ultima:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Negadas IN FRAME F_Ultima
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_Negadas:HIDDEN IN FRAME F_Ultima           = TRUE.

/* SETTINGS FOR FILL-IN W_CptoScoring IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MenDes IN FRAME F_Ultima
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_VblesS
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_VblesS:HIDDEN           = TRUE
       FRAME F_VblesS:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Capacidad_Pago IN FRAME F_VblesS
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR COMBO-BOX Cmb_ConCte IN FRAME F_VblesS
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX Cmb_MoraCial IN FRAME F_VblesS
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX Cmb_ResPat IN FRAME F_VblesS
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX Cmb_TipAct IN FRAME F_VblesS
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Clientes.GtoFinanc_Indir IN FRAME F_VblesS
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Gto_Arriendo IN FRAME F_VblesS
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Gto_Familiar IN FRAME F_VblesS
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Clientes.Gto_obligacion IN FRAME F_VblesS
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_arriendos IN FRAME F_VblesS
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Ing_financieros IN FRAME F_VblesS
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Ing_Honorarios IN FRAME F_VblesS
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Nom_CteCodeu IN FRAME F_VblesS
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Sdo_Obligaciones IN FRAME F_VblesS
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Tot_Egresos IN FRAME F_VblesS
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tot_Ingresos IN FRAME F_VblesS
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_LiqDispon IN FRAME F_VblesS
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrCuota IN FRAME F_VblesS
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME VisorSolicitud
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME VisorSolicitud:HIDDEN           = TRUE.

ASSIGN 
       Btn_IrProcesarInst:HIDDEN IN FRAME VisorSolicitud           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Admisible
/* Query rebuild information for BROWSE BR_Admisible
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Garantias NO-LOCK INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BR_Admisible */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cerradas
/* Query rebuild information for BROWSE Br_Cerradas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cerradas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Codeudores
/* Query rebuild information for BROWSE Br_Codeudores
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCode NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Codeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ConHV
/* Query rebuild information for BROWSE Br_ConHV
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Hoja_Vida WHERE Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                                             AND Hoja_Vida.Tipo = 9
                                             AND Hoja_Vida.Codigo = 1
                                             AND Hoja_Vida.DoctoRef = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                             AND Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                             AND Hoja_Vida.Asunto_Cumplido = NO
                                             AND Hoja_Vida.Usuario = W_Usuario INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_ConHV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Consulta
/* Query rebuild information for BROWSE Br_Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Solicitud INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cred
/* Query rebuild information for BROWSE Br_Cred
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCred_ACanc NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cred */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Deducibles
/* Query rebuild information for BROWSE Br_Deducibles
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Deducibles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Extras
/* Query rebuild information for BROWSE Br_Extras
     _START_FREEFORM
OPEN QUERY Br_Extras FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                                       AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BY Extras.Nro_Cuota INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Extras */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR_Scoring
/* Query rebuild information for BROWSE BR_Scoring
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TScoring NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BR_Scoring */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Usuarios
/* Query rebuild information for BROWSE Br_Usuarios
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Usuarios */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Admisible
/* Query rebuild information for FRAME F_Admisible
     _Query            is NOT OPENED
*/  /* FRAME F_Admisible */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Agregar
/* Query rebuild information for FRAME F_Agregar
     _Query            is NOT OPENED
*/  /* FRAME F_Agregar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cerradas
/* Query rebuild information for FRAME F_Cerradas
     _Query            is NOT OPENED
*/  /* FRAME F_Cerradas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Codeudores
/* Query rebuild information for FRAME F_Codeudores
     _Query            is NOT OPENED
*/  /* FRAME F_Codeudores */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ConAdmisible
/* Query rebuild information for FRAME F_ConAdmisible
     _Query            is NOT OPENED
*/  /* FRAME F_ConAdmisible */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Condicionada
/* Query rebuild information for FRAME F_Condicionada
     _Query            is NOT OPENED
*/  /* FRAME F_Condicionada */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Consulta
/* Query rebuild information for FRAME F_Consulta
     _Query            is NOT OPENED
*/  /* FRAME F_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_CredACanc
/* Query rebuild information for FRAME F_CredACanc
     _Query            is NOT OPENED
*/  /* FRAME F_CredACanc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Creditos
/* Query rebuild information for FRAME F_Creditos
     _Query            is NOT OPENED
*/  /* FRAME F_Creditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Deducibles
/* Query rebuild information for FRAME F_Deducibles
     _Query            is NOT OPENED
*/  /* FRAME F_Deducibles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Desembolso
/* Query rebuild information for FRAME F_Desembolso
     _Query            is NOT OPENED
*/  /* FRAME F_Desembolso */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Extras
/* Query rebuild information for FRAME F_Extras
     _Query            is NOT OPENED
*/  /* FRAME F_Extras */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_ForPago
/* Query rebuild information for FRAME F_ForPago
     _TblList          = "bdcentral.Anexos_Clientes"
     _Query            is NOT OPENED
*/  /* FRAME F_ForPago */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Garantias
/* Query rebuild information for FRAME F_Garantias
     _Query            is NOT OPENED
*/  /* FRAME F_Garantias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_HojaVida
/* Query rebuild information for FRAME F_HojaVida
     _Query            is NOT OPENED
*/  /* FRAME F_HojaVida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Instancias
/* Query rebuild information for FRAME F_Instancias
     _Query            is NOT OPENED
*/  /* FRAME F_Instancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Producto
/* Query rebuild information for FRAME F_Producto
     _Query            is NOT OPENED
*/  /* FRAME F_Producto */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Solicitud
/* Query rebuild information for FRAME F_Solicitud
     _TblList          = "bdcentral.Clientes"
     _Query            is NOT OPENED
*/  /* FRAME F_Solicitud */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ultima
/* Query rebuild information for FRAME F_Ultima
     _Query            is NOT OPENED
*/  /* FRAME F_Ultima */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_VblesS
/* Query rebuild information for FRAME F_VblesS
     _Query            is NOT OPENED
*/  /* FRAME F_VblesS */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Creditos:HANDLE
       ROW             = 2.23
       COLUMN          = 3
       HEIGHT          = 1.35
       WIDTH           = 5
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME VisorSolicitud:HANDLE
       ROW             = 1.81
       COLUMN          = 4
       HEIGHT          = 15.88
       WIDTH           = 101
       WIDGET-ID       = 4
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {CA8A9780-280D-11CF-A24D-444553540000} type: AcroPDF */
      CtrlFrame:MOVE-AFTER(PDF:HANDLE IN FRAME F_Creditos).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Proceso de Solicitudes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */

    IF THIS-PROCEDURE:PERSISTENT THEN
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de Solicitudes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Hoja_Vida.Asunto_Cumplido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Asunto_Cumplido wWin
ON VALUE-CHANGED OF Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida /* Asunto Cumplido */
DO:
    ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR_Admisible
&Scoped-define FRAME-NAME F_ConAdmisible
&Scoped-define SELF-NAME BR_Admisible
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Admisible wWin
ON MOUSE-SELECT-CLICK OF BR_Admisible IN FRAME F_ConAdmisible
DO:
    RowidGar_Global = ROWID(Garantias).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR_Admisible wWin
ON MOUSE-SELECT-DBLCLICK OF BR_Admisible IN FRAME F_ConAdmisible
DO:
    RowidGar_Global = ROWID(Garantias).
    APPLY "choose" TO Btn_OutConAdm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cerradas
&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME Br_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cerradas wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Cerradas IN FRAME F_Cerradas
DO:
    APPLY "choose" TO Btn_OutCerradas IN FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cerradas wWin
ON ROW-DISPLAY OF Br_Cerradas IN FRAME F_Cerradas
DO:
    IF TCerradas.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) OR TCerradas.Fec_Retiro = ? THEN DO:
        Tcerradas.Instancia:FGCOL IN BROWSE BR_Cerradas = 12.
        TCerradas.INom_Instancia:FGCOL IN BROWSE BR_Cerradas = 12.
        TCerradas.Usuario:FGCOL IN BROWSE BR_Cerradas = 12.
        TCerradas.INom_Usuario:FGCOL IN BROWSE BR_Cerradas = 12.
        TCerradas.Fec_Retiro:FGCOL IN BROWSE BR_Cerradas = 12.
        TCerradas.Descripcion:FGCOL IN BROWSE BR_Cerradas = 12.
    END.
    ELSE DO:
        Tcerradas.Instancia:FGCOL IN BROWSE BR_Cerradas = 0.
        TCerradas.INom_Instancia:FGCOL IN BROWSE BR_Cerradas = 0.
        TCerradas.Usuario:FGCOL IN BROWSE BR_Cerradas = 0.
        TCerradas.INom_Usuario:FGCOL IN BROWSE BR_Cerradas = 0.
        TCerradas.Fec_Retiro:FGCOL IN BROWSE BR_Cerradas = 0.
        TCerradas.Descripcion:FGCOL IN BROWSE BR_Cerradas = 0.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Codeudores
&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Br_Codeudores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Codeudores wWin
ON MOUSE-SELECT-CLICK OF Br_Codeudores IN FRAME F_Codeudores
DO:
    IF AVAILABLE(TCode) THEN
        ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
               W_NomCodeudor:SCREEN-VALUE = TC_NomCode
               Relaciones.Aprobada:SCREEN-VALUE = STRING(TC_Aprob)
               Btn_SalCod:SENSITIVE = TRUE
               W_NvoCD = FALSE
               W_NitCodeudor:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Codeudores wWin
ON VALUE-CHANGED OF Br_Codeudores IN FRAME F_Codeudores
DO:
    IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores <> 0 THEN
        ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
               W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_ConHV
&Scoped-define FRAME-NAME F_ConHV
&Scoped-define SELF-NAME Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_ConHV wWin
ON MOUSE-SELECT-DBLCLICK OF Br_ConHV IN FRAME F_ConHV
DO:
    APPLY "choose" TO Btn_OutConHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_ConHV wWin
ON ROW-DISPLAY OF Br_ConHV IN FRAME F_ConHV
DO:
    IF NOT Hoja_Vida.Asunto_Cumplido THEN DO:
        Hoja_Vida.Fec_Grabacion:FGCOL IN BROWSE Br_ConHV = 12.
        Hoja_Vida.Observacion:FGCOL IN BROWSE Br_ConHV = 12. 
    END.
    ELSE DO:
        Hoja_Vida.Fec_Grabacion:FGCOL IN BROWSE Br_ConHV = 0.
        Hoja_Vida.Observacion:FGCOL IN BROWSE Br_ConHV = 0.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Consulta
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Consulta IN FRAME F_Consulta
DO:
    APPLY "choose" TO Btn_OutConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON ROW-DISPLAY OF Br_Consulta IN FRAME F_Consulta
DO:
    IF Consulta.Estado = 4 THEN DO:
        ASSIGN Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.AgeSolicitud:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.

        ASSIGN Consulta.Num_Solicitud:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.AgeSolicitud:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Nit:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.
    END.

    IF Consulta.Vigencia <= (W_VigIns / 2) THEN DO:
        ASSIGN Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.AgeSolicitud:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 10
               Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 10.

        ASSIGN Consulta.Num_Solicitud:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.AgeSolicitud:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.Nit:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 0
               Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 0.
    END.

    IF Consulta.Vigencia > (W_VigIns / 2) AND Consulta.Vigencia <= W_VigIns THEN DO:
        ASSIGN Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.AgeSolicitud:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 14
               Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 14.
    END.

    IF Consulta.Vigencia > W_VigIns THEN DO:
        ASSIGN Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.AgeSolicitud:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
               Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.

        ASSIGN Consulta.Num_Solicitud:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.AgeSolicitud:FGCOL IN BROWSE Br_Consulta = 15 
               Consulta.Nit:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
               Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cred
&Scoped-define FRAME-NAME F_CredACanc
&Scoped-define SELF-NAME Br_Cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cred wWin
ON MOUSE-SELECT-CLICK OF Br_Cred IN FRAME F_CredACanc
DO:
    /*W_TotCanc = 0.

    FOR EACH TCred_ACanc WHERE TCred_ACanc.CSiNo OR TCred_ACanc.valorAbono > 0 NO-LOCK:
        IF TCred_ACanc.CSiNo = TRUE THEN DO:
            W_TotCanc = W_TotCanc + TCred_ACanc.SdoTD.
            TCred_ACanc.valorAbono:SCREEN-VALUE IN BROWSE br_cred = "0".
        END.
        ELSE
            W_TotCanc = W_TotCanc + TCred_ACanc.valorAbono.
    END.

    W_TotCanc:SCREEN-VALUE = STRING(W_TotCanc).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cred wWin
ON ROW-DISPLAY OF Br_Cred IN FRAME F_CredACanc
DO:
    IF TCred_ACanc.CSiNo = TRUE THEN
        TCred_ACanc.CSiNo:BGCOL IN BROWSE Br_Cred = 10.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cred wWin
ON ROW-ENTRY OF Br_Cred IN FRAME F_CredACanc
DO:
    totalAbonoCreditos = 0.

    FOR EACH bfrTTAbonos NO-LOCK:
        totalAbonoCreditos = totalAbonoCreditos + bfrTTAbonos.valorABono.
    END.

    W_TotCanc:SCREEN-VALUE IN FRAME F_CredACanc = STRING(totalAbonoCreditos).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cred wWin
ON ROW-LEAVE OF Br_Cred IN FRAME F_CredACanc
DO:
    totalAbonoCreditos = 0.

    FOR EACH bfrTTAbonos NO-LOCK:
        totalAbonoCreditos = totalAbonoCreditos + bfrTTAbonos.valorABono.
    END.

    W_TotCanc:SCREEN-VALUE IN FRAME F_CredACanc = STRING(totalAbonoCreditos).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Extras
&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Extras wWin
ON MOUSE-SELECT-CLICK OF Br_Extras IN FRAME F_Extras
DO:
    IF AVAILABLE(Extras) THEN
        ASSIGN W_PPExtra = Extras.Nro_Cuota
               W_PPExtra:SCREEN-VALUE = STRING(Extras.Nro_Cuota)
               W_VrExtra = Extras.Vr_CuoExtra
               W_VrExtra:SCREEN-VALUE = STRING(Extras.Vr_CuoExtra).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Usuarios
&Scoped-define FRAME-NAME F_Condicionada
&Scoped-define SELF-NAME Br_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Usuarios wWin
ON MOUSE-SELECT-CLICK OF Br_Usuarios IN FRAME F_Condicionada
DO:
    IF AVAILABLE(Tuxi) THEN
        W_RowIdTx = ROWID(Tuxi).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Usuarios wWin
ON ROW-DISPLAY OF Br_Usuarios IN FRAME F_Condicionada
DO:
    IF Tuxi.Proceso THEN DO:
        Tuxi.Agencia:FGCOL IN BROWSE Br_Usuarios = 12.
        Tuxi.Usuario:FGCOL IN BROWSE Br_Usuarios = 12.
        Tuxi.Nombre:FGCOL IN BROWSE Br_Usuarios = 12.
    END.
    ELSE DO:
        Tuxi.Agencia:FGCOL IN BROWSE Br_Usuarios = 0.
        Tuxi.Usuario:FGCOL IN BROWSE Br_Usuarios = 0.
        Tuxi.Nombre:FGCOL IN BROWSE Br_Usuarios = 0.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME btnFrmlrioAfliacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFrmlrioAfliacion wWin
ON CHOOSE OF btnFrmlrioAfliacion IN FRAME F_Solicitud /* Afiliación */
DO:
    RUN viewObject IN h_w-proclientesnew.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGrntias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGrntias wWin
ON CHOOSE OF btnGrntias IN FRAME F_Solicitud /* Admin. Garantías */
DO:
    RUN viewObject IN h_w-adm_garantias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_CredACanc
&Scoped-define SELF-NAME btnSalirAbonoCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalirAbonoCreditos wWin
ON CHOOSE OF btnSalirAbonoCreditos IN FRAME F_CredACanc /* Button 167 */
DO:
    FIND CURRENT Solicitud NO-ERROR.

    ASSIGN W_TotCanc.

    W_VrCredACanc = W_TotCanc.
    Solicitud.TOTAL_Prestamo = DECIMAL(Solicitud.TOTAL_Prestamo:SCREEN-VALUE IN FRAME f_solicitud).

    IF Solicitud.TOTAL_Prestamo - (W_TotCanc + Solicitud.Deducible ) < 0 THEN DO:
        MESSAGE "El valor solicitado es menor de lo requerido para cancelar los créditos." SKIP
                "No se permite la Operacion. Debe desmarcar créditos a cancelar..."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    FRAME F_CredACanc:VISIBLE = FALSE.
    FRAME F_Creditos:SENSITIVE = TRUE.

    FOR EACH TCred_ACanc NO-LOCK:
        FIND FIRST solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = solicitud.nit
                                              AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud
                                              AND solicitudes_pagoCreditos.num_credito = TCred_ACanc.numC NO-ERROR.
        IF NOT AVAILABLE solicitudes_pagoCreditos THEN DO:
            CREATE solicitudes_pagoCreditos.
            solicitudes_pagoCreditos.cliente_id = solicitud.nit.
            solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud.
            solicitudes_pagoCreditos.num_credito = TCred_ACanc.numC.
        END.
        
        solicitudes_pagoCreditos.pagoTotal = TCred_ACanc.CSiNo.
        solicitudes_pagoCreditos.valorAbono = TCred_ACanc.valorAbono.
        
        IF TCred_ACanc.valorAbono = 0 THEN
            DELETE solicitudes_pagoCreditos.
    END.

    W_VrADesemb = DECIMAL(Solicitud.Total_Prestamo:SCREEN-VALUE) - DECIMAL(Solicitud.Deducible:SCREEN-VALUE) - W_TotCanc.
    W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).
    W_VrCredACanc = W_TotCanc.
    W_VrCredACanc:SCREEN-VALUE = STRING(W_TotCanc).

    APPLY "leave" TO solicitud.PLAZO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_Activas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activas wWin
ON CHOOSE OF Btn_Activas IN FRAME F_Codeudores /* Inactivar */
DO:
    DO WITH FRAME F_Codeudores:
        IF Br_Codeudores:NUM-SELECTED-ROWS = 0 THEN DO:
            MESSAGE "Debe posicionarse en la relación a inactivar" SKIP
                    "mediante el mouse. Rectifique!!!"
                VIEW-AS ALERT-BOX INFORMATION.
        END.
        ELSE DO:
            FIND FIRST Relaciones WHERE Relaciones.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                                    AND Relaciones.Cod_Relacion = 11
                                    AND Relaciones.Clase_Producto = 2
                                    AND Relaciones.Cod_Producto = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
                                    AND Relaciones.Cuenta = Solicitud.Num_solicitud:SCREEN-VALUE IN FRAME F_Solicitud
                                    AND Relaciones.Nit_Relacion = TCode.TC_NitCode NO-ERROR.
            IF AVAILABLE(Relaciones) THEN DO:
                IF RActivas:SCREEN-VALUE = "1" THEN
                    ASSIGN Relaciones.Estado = 2
                           Relaciones.Fec_Inactividad = W_Fecha
                           TCode.TC_EstRela = 2
                           TCode.TC_FecReti = W_Fecha.
                ELSE
                    ASSIGN Relaciones.Estado = 1
                           Relaciones.Fec_Inactividad = ?
                           TCode.TC_EstRela = 1
                           TCode.TC_FecReti = ?.

                OPEN QUERY Br_Codeudores FOR EACH TCode WHERE TCode.TC_EstRel = INTEGER(RActivas:SCREEN-VALUE) NO-LOCK INDEXED-REPOSITION.

                IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores <> 0 THEN
                    ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
                           W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
                ELSE
                    ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
                           W_NomCodeudor:SCREEN-VALUE = "".
            END.

            RELEASE Relaciones.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Btn_AdExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AdExt wWin
ON CHOOSE OF Btn_AdExt IN FRAME F_Extras /* Salvar Extra */
DO:
    DEFINE VAR W_NroDias AS INTEGER.
    DEFINE VAR jj AS INTEGER.
    DEFINE VAR W_FecTra AS DATE.

    CASE solicitud.per_pago:
        WHEN 0 THEN W_NroDias = 1.
        WHEN 1 THEN W_NroDias = 7.
        WHEN 2 THEN W_NroDias = 10.
        WHEN 3 THEN W_NroDias = 15.
        WHEN 4 THEN W_NroDias = 30.
        WHEN 5 THEN W_NroDias = 60.
        WHEN 6 THEN W_NroDias = 90.
        WHEN 7 THEN W_NroDias = 120.
        WHEN 8 THEN W_NroDias = 180.
        WHEN 9 THEN W_NroDias = 360.
    END CASE.

    IF W_PPExtra <= 0 OR W_PPExtra > Solicitud.Plazo / W_NroDias THEN DO:
        MESSAGE "El período de pago de la cuota extra debe ser mayor que 0 y" SKIP
                "menor o igual el plazo total... No se acepta la operación."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    IF W_VrExtra <= 0 OR W_VrExtra >= Solicitud.Monto THEN DO:
        MESSAGE "El valor de la cuota extra debe ser mayor que 0 y" SKIP
                "menor al monto total... No se acepta la operación."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    W_TotExt = W_VrExtra.

    FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                      AND Extras.Num_Solicitud = Solicitud.Num_Solicitud:
        IF Extras.Estado = 1 AND Extras.Nro_Cuota <> W_PPExtra THEN
            W_TotExt = W_TotExt + Extras.Vr_CuoExtra.
        ELSE
            IF Extras.Estado <> 1 THEN
                DELETE Extras.
    END.

    IF W_TotExt >= Solicitud.Monto THEN DO:
        MESSAGE "El valor total de las cuotas extras + El valor de esta Extra" SKIP
                "debe ser menor al monto total... No se acepta la operaciòn."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    FIND FIRST Extras WHERE Extras.Nit = Solicitud.Nit
                        AND Extras.Num_Solicitud = Solicitud.Num_Solicitud
                        AND Extras.Nro_Cuota = W_PPExtra NO-ERROR.
    IF NOT AVAILABLE(Extras) THEN
        CREATE Extras.

    ASSIGN Extras.Agencia = Solicitud.Agencia
           Extras.Cod_Credito = Solicitud.Cod_Credito
           Extras.Nit = Solicitud.Nit
           Extras.Num_Solicitud = Solicitud.Num_Solicitud
           Extras.Nro_Cuota = W_PPExtra
           Extras.Vr_CuoExtra = W_VrExtra
           Extras.Estado = 1
           W_TotExt = 0
           W_FecTra = W_Fecha.

    DO JJ = 1 TO W_PPExtra:
        RUN Halla_FecVcto.R (INPUT DATE(solicitud.fec_desembolso:SCREEN-VALUE IN FRAME F_Solicitud),
                             INPUT W_NroDias,
                             INPUT W_FecTra,
                             OUTPUT W_FecTra).

        Extras.Fec_Vcto = W_FecTra.
    END.

    FIND CURRENT Extras NO-LOCK NO-ERROR.

    FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                      AND Extras.Num_Solicitud = Solicitud.Num_Solicitud
                      AND extras.estado = 1 NO-LOCK:
        ASSIGN W_TotExt = W_TotExt + Extras.Vr_CuoExtra
               W_TotExt:SCREEN-VALUE IN FRAME F_Solicitud = STRING(W_TotExt).
    END.

    CLOSE QUERY Br_Extras.
    OPEN QUERY Br_Extras FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                                           AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BY Extras.Nro_Cuota INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_AgregarTxt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AgregarTxt wWin
ON CHOOSE OF Btn_AgregarTxt IN FRAME F_Instancias /* Agregar Texto */
DO:
    IF NOT W_NvaHV THEN DO:
        Id_Agregar = "IN".
        E_Agregar:SCREEN-VALUE IN FRAME F_Agregar = "".
        ENABLE ALL WITH FRAME F_Agregar.
        VIEW FRAME F_Agregar.
        APPLY "entry" TO E_Agregar IN FRAME F_Agregar.

        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Anexos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Anexos wWin
ON CHOOSE OF Btn_Anexos IN FRAME F_Creditos /* Anexos */
DO:
    RUN VerFoto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_Borra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Borra wWin
ON CHOOSE OF Btn_Borra IN FRAME F_Admisible /* Borrar */
DO:
    IF AVAILABLE(Garantias) THEN DO:
        MESSAGE "Confirma la eliminación de la Garantía?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

        IF Choice THEN DO:
            FIND CURRENT Garantias EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF LOCKED(garantias) THEN DO:
                MESSAGE "ERROR: Eliminando Garantías. Registro en uso por otro usuario" SKIP "Intente más tarde"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.

            DELETE Garantias.

            MESSAGE "La Garantía fue eliminada."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            Garantias.Identificacion_Bien:SCREEN-VALUE = "".
            APPLY "CHOOSE" TO Btn_ConAdm.
        END.
    END.
    ELSE
        MESSAGE "No se ha seleccionado ninguna Garantía"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_CanAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanAdm wWin
ON CHOOSE OF Btn_CanAdm IN FRAME F_Admisible /* Cancelar */
DO:
    RUN Mostrar_Admisible.
    DISABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_CancCred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CancCred wWin
ON CHOOSE OF Btn_CancCred IN FRAME F_Solicitud /* Cancelar Creditos con esta Solicitud de Ptmo */
DO:
    DEFINE VAR interesProyectado AS DECIMAL.
    DEFINE VAR interesMoraProyectado AS DECIMAL.
    DEFINE VAR diasProyectar AS INTEGER.
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR tasaDiaria AS DECIMAL.
    DEFINE VAR tasaDiariaMora AS DECIMAL.
    DEFINE VAR baseLiquidarProyeccion AS DECIMAL.

    diasProyectar = DATE(solicitud.fec_desembolso:SCREEN-VALUE) - w_fecha.

    OPEN QUERY Br_Cred FOR EACH TCred_ACanc INDEXED-REPOSITION.

    IF NOT AVAILABLE(Solicitud) THEN DO:
        MESSAGE "Debe existir la solicitud ya grabada..."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = Solicitud.Cod_Credito NO-LOCK NO-ERROR.
    IF Pro_Creditos.Id_Extracto OR Pro_Creditos.Id_PerGracia THEN DO:
        MESSAGE "Credito transitorio o con periodo de gracia." SKIP
                "No se permite cancelar otros créditos."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    IF AVAILABLE(Solicitud) AND Solicitud.Nit = Solicitud.Nit:SCREEN-VALUE THEN DO:
        FRAME F_Creditos:SENSITIVE = FALSE.
        FRAME F_CredACanc:VISIBLE = TRUE.
        W_TotCanc:SCREEN-VALUE = STRING(W_VrCredACanc).

        FOR EACH Creditos WHERE Creditos.Nit = Solicitud.Nit
                            AND Creditos.Estado = 2 NO-LOCK:
            /*interesProyectado = 0.
            tasaDiaria = creditos.tasa / 36000.

            IF creditos.cod_credito = 108 OR creditos.cod_credito = 113 OR creditos.cod_credito = 114 THEN
                baseLiquidarProyeccion = creditos.sdo_capital + creditos.INT_corriente.
            ELSE
                baseLiquidarProyeccion = creditos.sdo_Capital.

            DO cont = 1 TO diasProyectar:
                IF creditos.cod_credito = 108 OR creditos.cod_credito = 113 OR creditos.cod_credito = 114 THEN DO:
                    interesProyectado = interesProyectado + ROUND((baseLiquidarProyeccion - creditos.val_atraso) * tasaDiaria,0).
                    baseLiquidarProyeccion = baseLiquidarProyeccion + interesProyectado.
                END.
                ELSE
                    interesProyectado = interesProyectado + ROUND((baseLiquidarProyeccion - creditos.val_atraso) * tasaDiaria,0).
            END.

            IF creditos.INT_morCobrar > 0 THEN DO:
                FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
                IF AVAILABLE pro_creditos THEN DO:
                    FIND FIRST Indicadores WHERE Indicadores.Indicador = pro_creditos.Cod_TasaMax
                                             AND Indicadores.Estado = 1
                                             AND Indicadores.FecVcto >= W_Fecha NO-LOCK NO-ERROR.
                    IF AVAILABLE Indicadores THEN
                        tasaDiariaMora = Indicadores.Tasa.
                    ELSE DO:
                        FIND LAST Indicadores WHERE Indicadores.Indicador = Entidad.Ind_Usura
                                                AND Indicadores.Estado = 1 NO-LOCK NO-ERROR.
                        IF AVAILABLE Indicadores THEN
                            tasaDiariaMora = Indicadores.Tasa.
                        ELSE
                            tasaDiariaMora = 0.
                    END.

                    IF tasaDiariaMora > 0 THEN DO:
                        RUN EFNV IN W_ManFin (INPUT tasaDiariaMora / 100,
                                              INPUT 12,
                                              OUTPUT tasaDiariaMora).

                        tasaDiariaMora = (tasaDiariaMora * 12) / 360.
                        interesProyectado = interesProyectado + (creditos.val_atraso * tasaDiariaMora * diasProyectar).
                    END.
                END.
            END.*/

            RUN p-proyectarLiquidacion.r(INPUT creditos.nit,
                                         INPUT creditos.cod_credito,
                                         INPUT creditos.num_credito,
                                         INPUT creditos.tasa,
                                         INPUT creditos.sdo_capital,
                                         INPUT creditos.INT_corriente,
                                         INPUT creditos.INT_morCobrar,
                                         INPUT creditos.fec_pago,
                                         INPUT DATE(solicitud.fec_desembolso:SCREEN-VALUE),
                                         OUTPUT interesProyectado,
                                         OUTPUT interesMoraProyectado) NO-ERROR.


            FIND FIRST TCred_ACanc WHERE TCred_ACanc.agen = creditos.agencia
                                     AND TCred_ACanc.Pto = creditos.cod_credito
                                     AND TCred_ACanc.numC = creditos.num_credito NO-ERROR.
            IF NOT AVAILABLE TCred_ACanc THEN DO:
                CREATE TCred_ACanc.
                TCred_ACanc.Agen = Creditos.Agencia.
                TCred_ACanc.Pto = Creditos.Cod_Credito.
                TCred_ACanc.NumC = Creditos.Num_Credito.
                TCred_ACanc.FecD = Creditos.Fec_Desembolso.
                TCred_ACanc.CuoC = Creditos.Cuota.
                TCred_ACanc.Tasa = Creditos.Tasa.
            END.

            /*TCred_ACanc.SdoTD = Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado + interesProyectado.*/
            TCred_ACanc.SdoTD = Creditos.Sdo_Capital + interesProyectado + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + interesMoraProyectado + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.

            FIND FIRST solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = creditos.nit
                                                  AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud
                                                  AND solicitudes_pagoCreditos.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
            IF AVAILABLE solicitudes_pagoCreditos THEN DO:
                TCred_ACanc.CSiNo = solicitudes_pagoCreditos.pagoTotal.

                IF solicitudes_pagoCreditos.pagoTotal = TRUE THEN
                    /*TCred_ACanc.valorAbono = Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado + interesProyectado.*/
                    TCred_ACanc.valorAbono = Creditos.Sdo_Capital + interesProyectado + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + interesMoraProyectado + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.
                ELSE
                    TCred_ACanc.valorAbono = solicitudes_pagoCreditos.valorAbono.
            END.
        END.
        
        FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = Solicitud.Cod_Credito NO-LOCK NO-ERROR.

        CLOSE QUERY Br_Cred.
        OPEN QUERY Br_Cred FOR EACH TCred_ACanc INDEXED-REPOSITION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Creditos /* Cancelar */
DO:
    W_Nuevo = NO.

    IF W_Primera = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN
        ENABLE Btn_Ingresar WITH FRAME F_Creditos.
    ELSE
        DISABLE Btn_Ingresar WITH FRAME F_Creditos.

    FIND FIRST Solicitud WHERE ROWID(Solicitud) = Puntero NO-LOCK NO-ERROR.
    IF AVAILABLE Solicitud THEN DO:
        RUN Mostrar_Solicitud.
        
        ENABLE Btn_Gar
               Btn_HojaVida
            WITH FRAME F_Solicitud.

        ENABLE PDF
            WITH FRAME F_Creditos.

        DISABLE Solicitud.Nit WITH FRAME F_Solicitud.
    END.
    ELSE DO:
        MESSAGE "No se ha encontrado la solicitud"
            VIEW-AS ALERT-BOX.

        APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_CanCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanCod wWin
ON CHOOSE OF Btn_CanCod IN FRAME F_Codeudores /* Cancelar */
DO:
    DISABLE W_NitCodeudor
            Btn_SalCod
            Btn_CanCod
        WITH FRAME F_Codeudores.

    ENABLE Btn_CreCod
        WITH FRAME F_Codeudores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_ConAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ConAdm wWin
ON CHOOSE OF Btn_ConAdm IN FRAME F_Admisible /* Button 160 */
DO:
    ASSIGN R_ConAdm:SCREEN-VALUE IN FRAME F_ConAdmisible = "1"
           R_ConAdm.

    RUN Consul_Gtias.

    ASSIGN FRAME F_Admisible:SENSITIVE = FALSE.
    VIEW FRAME F_ConAdmisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_Creditos /* Button 10 */
DO:
    IF W_Nuevo THEN
        RETURN.

    APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
    FRAME F_Consulta:HIDDEN = NO.

    ASSIGN W_Concepto = ""
           TOTAL_Puntaje = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_CreCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreCod wWin
ON CHOOSE OF Btn_CreCod IN FRAME F_Codeudores /* Crear */
DO:
    DO WITH FRAME F_Codeudores:
        W_NvoCD = YES.

        ENABLE Btn_SalCod
               Btn_CanCod
               W_NitCodeudor.

        DISABLE Btn_CreCod.

        ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
               W_NomCodeudor:SCREEN-VALUE = ""
               Relaciones.Aprobada:SCREEN-VALUE = "No".

        APPLY "entry" TO W_NitCodeudor.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Creditos /* Deshacer */
DO:
    FIND FIRST Solicitud WHERE ROWID(Solicitud) = puntero NO-LOCK NO-ERROR.
    IF AVAILABLE Solicitud THEN
        RUN Mostrar_Solicitud.

    IF W_Primera = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN
        ENABLE Btn_Ingresar WITH FRAME F_Creditos.
    ELSE
        DISABLE Btn_Ingresar WITH FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Destino wWin
ON CHOOSE OF Btn_Destino IN FRAME F_Solicitud /* Btn_Destino */
DO:
    RUN C-Varios.r (INPUT 38,
                    OUTPUT V_Cod,
                    OUTPUT V_Nom).

    IF V_Cod = 0 THEN
        MESSAGE "No se ha elegido ningún destino"
            VIEW-AS ALERT-BOX.
    ELSE
        ASSIGN Nom_Destino:SCREEN-VALUE = V_Nom
               Solicitud.Destinof:SCREEN-VALUE IN FRAME f_solicitud = STRING(V_Cod).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME F_Scoring /* Ejecutar */
DO:
    RUN InfrmeScringAExcel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Btn_EliExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EliExt wWin
ON CHOOSE OF Btn_EliExt IN FRAME F_Extras /* Eliminar Extra */
DO:
    FIND FIRST Extras WHERE Extras.Nit = Solicitud.Nit
                        AND Extras.Num_Solicitud = Solicitud.Num_Solicitud
                        AND Extras.Nro_Cuota = W_PPExtra NO-ERROR.
    IF AVAILABLE(Extras) THEN
        DELETE Extras.

    W_TotExt = 0.

    FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                      AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK:
        IF Extras.Estado = 1 THEN
            ASSIGN W_TotExt = W_TotExt + Extras.Vr_CuoExtra
                   W_TotExt:SCREEN-VALUE IN FRAME F_Solicitud = STRING(W_TotExt).
    END.

    CLOSE QUERY Br_Extras.
    OPEN QUERY Br_Extras FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                                           AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BY Extras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Extras wWin
ON CHOOSE OF Btn_Extras IN FRAME F_Solicitud /* Cuotas extraordinarias */
DO:
    IF Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud = "" THEN DO:
        MESSAGE "Para ingresar/consultar extras pactadas debe antes digitarse" SKIP
                "un número de identificación"
            VIEW-AS ALERT-BOX INFORMATION.

        APPLY "entry" TO Solicitud.Nit IN FRAME F_Solicitud.
        RETURN NO-APPLY.
    END.

    IF NOT AVAILABLE Solicitud THEN DO:
        MESSAGE "No hay una Solicitud disponible" 
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    ASSIGN FRAME F_Creditos:SENSITIVE = FALSE
           FRAME F_Extras:VISIBLE = TRUE.

    CLOSE QUERY Br_Extras.
    OPEN QUERY Br_Extras FOR EACH Extras WHERE Extras.Nit = Solicitud.Nit
                                           AND Extras.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BY Extras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Btn_FinVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinVS wWin
ON CHOOSE OF Btn_FinVS IN FRAME F_VblesS /* Btn_outultima 2 */
DO:
    DEFINE VAR W_GtosIndir AS DECIMAL.
    
    FOR EACH Relaciones WHERE Relaciones.Nit_Relacion = Clientes.Nit
                          AND Relaciones.Clase_Producto = 2
                          AND Relaciones.Cod_Relacion = 11
                          AND Relaciones.Estado = 1 NO-LOCK:
        FIND FIRST Creditos WHERE Creditos.Nit = Relaciones.Nit
                              AND Creditos.Num_Credito = INTEGER(Relaciones.Cuenta)
                              AND Creditos.Sdo_Capital > 0 NO-LOCK NO-ERROR.
        IF AVAILABLE(Creditos) THEN
            W_GtosIndir = W_GtosIndir + Creditos.Cuota.
    END.

    FIND CURRENT Clientes NO-ERROR.

    Clientes.GtoFinanc_Indir.
    Clientes.Ing_Arriendos.
    Clientes.Ing_Financieros.
    Clientes.Ing_Honorarios.
    Clientes.Ing_Otros.
    Clientes.Gto_Obligacion.
    Clientes.Sdo_Obligacion.
    Clientes.Salario.
    Clientes.Gto_Familiar.
    Clientes.Gto_Arriendo.
    Clientes.Fec_UltActualiza = W_Fecha.
    Clientes.Usuario = W_Usuario.
    Clientes.Tip_Contrato = INTEGER(Rs_Ocupacion:SCREEN-VALUE).
    Clientes.Endeud_Indirecto = ((Clientes.GtoFinanc_Indir + W_GtosIndir) / (Clientes.Ing_Arriendos  + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Ing_Otros + Clientes.Salario)) * 100.
    Clientes.Capacidad_Pago = DECIMAL(Clientes.Capacidad_Pago:SCREEN-VALUE).
    Clientes.Conocimiento_Cliente = Cmb_ConCte:SCREEN-VALUE.
    Clientes.Destino = Cmb_destino:SCREEN-VALUE.
    Clientes.Mora_Comercial = Cmb_MoraCial:SCREEN-VALUE.
    Clientes.Garantia = Cmb_Gtia:SCREEN-VALUE.
    Clientes.Respaldo_Patrim = Cmb_ResPat:SCREEN-VALUE.
    Clientes.Tipo_Actividad = Cmb_TipAct:SCREEN-VALUE NO-ERROR.

    FIND CURRENT Clientes NO-LOCK NO-ERROR.

    ASSIGN FRAME F_Creditos:SENSITIVE = TRUE.
    HIDE FRAME F_Vbless.

    IF Clientes.Nit = Solicitud.Nit THEN
        APPLY "CHOOSE" TO Btn_Salvar IN FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Gar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Gar wWin
ON CHOOSE OF Btn_Gar IN FRAME F_Solicitud /* Adicionar Garantías */
DO:
    IF Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud = "" THEN DO:
        MESSAGE "Para consultar las Garantías debe antes digitarse" SKIP
                "un número de identificación."
            VIEW-AS ALERT-BOX INFORMATION.

        APPLY "entry" TO Solicitud.Nit IN FRAME F_Solicitud.
        RETURN NO-APPLY.
    END.

    Garantias.Nom_bien:SCREEN-VALUE IN FRAME f_admisible = "".
    Garantias.Val_bien:SCREEN-VALUE IN FRAME f_admisible = "".
    Garantias.Identificacion_bien:SCREEN-VALUE IN FRAME f_admisible = "".

    HIDE FRAME F_Consultas.
    DISABLE ALL WITH FRAME F_Creditos.
    ENABLE ALL WITH FRAME F_Garantias.

    FIND FIRST garantias WHERE  garantias.nit = solicitud.nit NO-LOCK NO-ERROR.
    IF AVAILABLE(Garantias) THEN DO:
        RUN mostrar_admisible.
        W_NvaAdm = FALSE.
    END.
    ELSE DO:
        R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "1".
        APPLY "value-changed" TO R_TipoGarantia IN FRAME F_Garantias.
    END.

    puntero = ROWID(Clientes).
    
    EMPTY TEMP-TABLE TCode.

    IF AVAILABLE Solicitud THEN DO:
        RUN QUERY_Relaciones.

        IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores <> 0 THEN DO:
            W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode.
            W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
        END.
        ELSE DO:
            W_NitCodeudor:SCREEN-VALUE = "".
            W_NomCodeudor:SCREEN-VALUE = "".
        END.

        VIEW FRAME F_Garantias.
        HIDE FRAME F_ConAdmisible.
    END.
    ELSE
        MESSAGE "No hay una solicitud disponible."
            VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_GraInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_GraInstancia wWin
ON CHOOSE OF Btn_GraInstancia IN FRAME F_Instancias /* Grabar */
DO:
    DO TRANSACTION:
        IF Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = "" THEN DO:
            MESSAGE "No se puede grabar si no se ha entrado" SKIP
                    "el concepto de la instancia. Entre el Concepto!"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Mov_Instancias.Descripcion IN FRAME F_Instancias.
            RETURN NO-APPLY.
        END.

        MESSAGE "¿Está seguro de procesar la Instancia?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar salvar" UPDATE W_SiNos AS LOGICAL.

        IF NOT W_SiNoS THEN
            RETURN.

        FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                    AND Mov_Instancias.Nit = STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud)
                                    AND Mov_Instancias.Num_Solicitud = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                    AND Mov_Instancias.Estado = NO NO-ERROR.
        IF W_Negadas = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
            MESSAGE "La solicitud negada dejará de aparecer en las consultas"
                VIEW-AS ALERT-BOX.

            mov_Instancias.Fec_Retiro = W_Fecha.
            Mov_Instancias.Estado = YES.

            FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
            APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
            RELEASE Solicitud.
        END.
        ELSE DO:
            IF AVAILABLE Mov_Instancias THEN DO:
                ASSIGN FRAME F_Instancias
                    Mov_Instancias.Descripcion.

                FIND FIRST TCerradas WHERE TCerradas.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                       AND TCerradas.Num_Solicitud = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-ERROR.
                IF AVAILABLE TCerradas THEN
                    TCerradas.Descripcion = Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias.

                Mov_Instancias.Fec_Retiro = ?.
                Mov_Instancias.Hora_Retiro = 0.

                IF Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias = "YES" THEN DO:
                    FIND FIRST Hoja_Vida WHERE Hoja_Vida.Tipo = 9
                                           AND Hoja_Vida.Codigo = 1
                                           AND Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                           AND Hoja_Vida.DoctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                           AND Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                                           AND Hoja_Vida.Asunto_Cumplido = NO
                                           AND Hoja_Vida.Usuario = W_Usuario NO-ERROR.
                    IF AVAILABLE Hoja_Vida THEN DO:
                        Mov_Instancias.Estado = NO.

                        MESSAGE "La Solicitud aún tiene asuntos pendientes por resolver" SKIP
                                "en la hoja de vida. No se permite pasar a la siguiente" SKIP
                                "instancia si estos asuntos no se han cumplido." SKIP(1)
                                "Desea ver los asuntos por cumplir de la Solicitud...?"
                            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.

                        HIDE FRAME F_Instancias.
                        VIEW FRAME F_Solicitud.
                        ENABLE ALL WITH FRAME F_Creditos.
                        DISABLE NomUsuario WITH FRAME F_Creditos.

                        IF choice THEN DO:
                            APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
                            APPLY "entry" TO Hoja_Vida.Observacion IN FRAME F_HojaVida.
                            RETURN NO-APPLY.
                        END.
                        ELSE DO:
                            APPLY "entry" TO Btn_HojaVida IN FRAME F_Solicitud.
                            RETURN NO-APPLY.
                        END.
                    END.
                    ELSE DO:
                        HIDE FRAME F_Instancias.

                        IF W_Ultima <> INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                           W_Negadas <> INTEGER(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
                            RUN Asignar_Proxima_Instancia NO-ERROR.

                            IF ERROR-STATUS:ERROR THEN
                                UNDO.

                            FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.

                            FOR EACH Mov_Instancias WHERE Mov_Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                                      AND Mov_Instancias.Nit = STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud)
                                                      AND Mov_Instancias.Num_Solicitud = INT(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud):
                                Mov_Instancias.Fec_Retiro = W_Fecha.
                                Mov_Instancias.Hora_Retiro = TIME.
                                Mov_Instancias.Estado = YES.
                                Mov_Instancias.Agencia = INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)).
                            END.

                            FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
                            APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
                            RELEASE Solicitud.
                        END.
                        ELSE DO:
                            APPLY "value-changed" TO Solicitud.Estado IN FRAME F_Ultima.
                            ENABLE ALL WITH FRAME F_Ultima.
                            HIDE FRAME F_Condicionada.
                            VIEW FRAME F_Ultima.
                            W_CptoScoring:SCREEN-VALUE = Solicitud.Concepto.
                            DISABLE W_CptoScoring WITH FRAME F_ultima.
                            APPLY "entry" TO Solicitud.Estado IN FRAME F_Ultima.
                            RETURN NO-APPLY.
                        END.
                    END.
                END.
                PDF:HIDDEN IN FRAME F_Creditos = TRUE.
                FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
            END.
            ELSE
                MESSAGE "No se encontró la instancia" Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos SKIP
                        "Identificación:" Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud SKIP
                        "Número de Solicitud:" Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud
                    VIEW-AS ALERT-BOX ERROR.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_HojaVida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_HojaVida wWin
ON CHOOSE OF Btn_HojaVida IN FRAME F_Solicitud /* Pendientes */
DO:
    DISABLE ALL WITH FRAME F_Creditos.

    FIND FIRST Hoja_Vida WHERE Hoja_Vida.Tipo = 9
                           AND Hoja_Vida.Codigo = 1
                           AND Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                           AND Hoja_Vida.DoctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                           AND Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                           AND Hoja_Vida.Asunto_Cumplido = NO
                           AND Hoja_Vida.Usuario = W_Usuario NO-ERROR.

    DO WITH FRAME F_HojaVida:
        ENABLE ALL WITH FRAME F_HojaVida.
        DISABLE Hoja_Vida.Fec_Grabacion Btn_SalvaHV WITH FRAME F_HojaVida.

        IF AVAILABLE Hoja_Vida THEN DO:
            Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = STRING(Hoja_Vida.Asunto_Cumplido).
            Hoja_Vida.Observacion:SCREEN-VALUE = Hoja_Vida.Observacion.
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE = STRING(Hoja_Vida.Fec_Grabacion).

            ENABLE Hoja_Vida.Asunto_Cumplido
                   Hoja_Vida.Observacion.
        END.
        ELSE DO:
            Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = "no".
            Hoja_Vida.Observacion:SCREEN-VALUE = "".
            Hoja_Vida.Fec_Grabacion:SCREEN-VALUE = "".

            DISABLE Hoja_Vida.Asunto_Cumplido
                    Hoja_Vida.Observacion.
        END.
    END.

    VIEW FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME F_Instancias /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER FORM "X(35)" INITIAL "".

    ASSIGN Listado = W_Pathspl + "ComentInst2-" + W_Usuario + STRING(RANDOM(2000,10000)) + ".lst"
           W_SiMInst = TRUE.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                AND Mov_Instancias.Nit = STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud)
                                AND Mov_Instancias.Num_Solicitud = integer(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                AND Mov_Instancias.Usuario = W_Usuario
                                AND Mov_Instancias.Estado = NO NO-LOCK NO-ERROR.

    {Incluido/Imprimir.i "Listado"}.

    W_SiMInst = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_ImpCA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpCA wWin
ON CHOOSE OF Btn_ImpCA IN FRAME F_Creditos /* Impr.de Aprobación */
DO:
    IF NOT Instancias.Id_Scoring AND NOT Instancias.Id_Concepto THEN
        RETURN.

    W_TipoInforme = "Informe".

    RUN Informe_Imp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Creditos /* Button 8 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    Listado = W_PathSpl + "\HD_" + STRING(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) + W_Usuario + STRING(RANDOM(2000,10000)) + ".LST".
    {INCLUIDO\Imp_HoDes.I "Listado"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_InaAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_InaAdm wWin
ON CHOOSE OF Btn_InaAdm IN FRAME F_Admisible /* Inactivar */
DO:
    DO WITH FRAME F_Admisible:
        FIND CURRENT Garantias EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF LOCKED(garantias) THEN DO:
            MESSAGE "ERROR: Eliminando garantía. Registro en uso por otro usuario" SKIP
                    "Intente más tarde."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY.
        END.

        IF SELF:LABEL = "Inactivar" THEN DO:
            Garantias.Estado:SCREEN-VALUE = "2".
            Garantias.Fec_Retiro:SCREEN-VALUE = STRING(w_fecha).

            MESSAGE "Confirma la inactivación de la Garantía?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

            IF choice THEN DO:
                Garantias.Estado = 2.
                Garantias.Fec_Retiro = w_fecha.
            END.
            ELSE DO:
                Garantias.Estado:SCREEN-VALUE = "1".
                garantias.Fec_Retiro:SCREEN-VALUE = "?".
            END.
        END.
        ELSE DO:
            Garantias.Estado:SCREEN-VALUE = "1".
            Garantias.Fec_Retiro:SCREEN-VALUE = "?".

            MESSAGE "Confirma la activación de la Garantía?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

            IF choice THEN DO:
                Garantias.Estado = 1.
                Garantias.Fec_Retiro = ?.
            END.
            ELSE DO:
                Garantias.Estado:SCREEN-VALUE = "2".
                Garantias.Fec_Retiro:SCREEN-VALUE = STRING(Garantias.Fec_Retiro).
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_IngAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_IngAdm wWin
ON CHOOSE OF Btn_IngAdm IN FRAME F_Admisible /* Ingresar */
DO:
    DO WITH FRAME F_Admisible:
        DISABLE Btn_IngAdm WITH FRAME F_Amisible.
        ENABLE Btn_SalAdm WITH FRAME F_Admisible.

        RUN Inicializar_Admisible.

        W_NvaAdm = YES.

        IF R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "3" THEN
            Garantias.Tipo_Garantia:SCREEN-VALUE = "5".

        APPLY "entry" TO Garantias.Identificacion_Bien.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Creditos /* Ingresar */
DO:
    W_TotCanc = 0.
    W_TotCanc:SCREEN-VALUE IN FRAME F_CredACanc = "0".
    W_VrCredACanc = 0.

    DO WITH FRAME F_Solicitud:
        DISABLE Btn_Ingresar PDF WITH FRAME F_Creditos.
        DISABLE Btn_HojaVida Btn_Gar WITH FRAME F_Solicitud.
        FRAME F_Consulta:HIDDEN = YES.

        total_prestamo:SCREEN-VALUE = "0".

        IF FRAME F_Consulta:HIDDEN = YES THEN
            HIDE FRAME F_Consulta.

        IF FRAME F_Solicitud:HIDDEN = YES THEN
            VIEW FRAME F_Solicitud.

        IF AVAILABLE Solicitud THEN
            Puntero = ROWID(Solicitud).

        W_Nuevo = YES.

        RELEASE Solicitud.

        ENABLE Solicitud.Nit.
        ENABLE {&List-1} WITH FRAME F_Solicitud.
        
        RUN Inicializar_Variables.

        W_Concepto = "".
        TOTAL_Puntaje = 0.
        W_VrCredACanc:SCREEN-VALUE = "0".
        W_VrCredACanc.
        W_NomPdo:SCREEN-VALUE = "Cuota".
        W_TotExt = 0.
        W_TotExt:SCREEN-VALUE = "0".

        IF W_SucAgen THEN DO:
            ENABLE Cmb_Agencias WITH FRAME F_Solicitud.
            APPLY "entry" TO Cmb_Agencias.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISABLE Cmb_Agencias WITH FRAME F_Solicitud.
            APPLY "entry" TO Solicitud.Nit.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_insVolver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_insVolver wWin
ON CHOOSE OF Btn_insVolver IN FRAME F_Instancias /* Button 135 */
DO:
    ENABLE ALL WITH FRAME F_Creditos.
    DISABLE NomUsuario WITH FRAME F_Creditos.
    HIDE FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME VisorSolicitud
&Scoped-define SELF-NAME Btn_IrProcesarInst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_IrProcesarInst wWin
ON CHOOSE OF Btn_IrProcesarInst IN FRAME VisorSolicitud /* Ir a procesar instancia */
DO:
  VIEW FRAME F_Solicitud.
  APPLY "choose" TO Btn_ProcesarInstancia IN FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Liquidar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Liquidar wWin
ON CHOOSE OF Btn_Liquidar IN FRAME F_Solicitud /* Liquidar */
DO:
    APPLY "leave" TO solicitud.Monto.
    APPLY "leave" TO solicitud.Plazo.

    RUN Liquidar.

    DISABLE Btn_Liquidar WITH FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_NvoHv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_NvoHv wWin
ON CHOOSE OF Btn_NvoHv IN FRAME F_HojaVida /* Ingresar */
DO:
    ENABLE Btn_SalvaHV
           Hoja_Vida.Asunto_Cumplido
           Hoja_Vida.Observacion
        WITH FRAME F_HojaVida.

    DISABLE Btn_NvoHV WITH FRAME F_HojaVida.

    ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = "no"
           Hoja_Vida.Observacion:SCREEN-VALUE = ""
           Hoja_Vida.Fec_Grabacion:SCREEN-VALUE = STRING(w_fecha).

    HIDE FRAME F_ConHV.
    W_NvaHV = YES.
    Hoja_Vida.Observacion:READ-ONLY = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME Btn_OutCerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutCerradas wWin
ON CHOOSE OF Btn_OutCerradas IN FRAME F_Cerradas /* Button 143 */
DO:
    IF AVAILABLE TCerradas THEN DO:
        FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia = TCerradas.Instancia
                                    AND Mov_Instancias.Num_Solicitud = TCerradas.Num_Solicitud
                                    AND Mov_Instancias.Usuario = TCerradas.Usuario NO-LOCK NO-ERROR.
        IF AVAILABLE Mov_Instancias THEN DO:
            ASSIGN Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias = STRING(Mov_Instancias.Estado)
                   Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
                   WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
                   Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
                   Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
                   W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos
                   W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.

            Vigencia:SCREEN-VALUE = STRING(w_fecha - TCerradas.Fec_Ingreso) + " días".

            IF Mov_Instancias.Estado = YES THEN
                DISABLE Mov_Instancias.Estado
                        Btn_GraInstancia
                    WITH FRAME F_Instancias.
            ELSE
                ENABLE Mov_Instancias.Estado
                       Mov_Instancias.Descripcion
                       Btn_GraInstancia
                    WITH FRAME F_Instancias.
        END.
    END.

    HIDE FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConAdmisible
&Scoped-define SELF-NAME Btn_OutConAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConAdm wWin
ON CHOOSE OF Btn_OutConAdm IN FRAME F_ConAdmisible /* Button 163 */
DO:
    HIDE FRAME F_ConAdmisible.
    ASSIGN FRAME F_Admisible:SENSITIVE = TRUE.

    IF Br_Admisible:NUM-SELECTED-ROWS > 0 THEN DO:
        FIND FIRST Garantias WHERE ROWID(Garantias) = RowidGar_Global NO-ERROR.

        ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "3"
               R_TipoGarantia.

        IF Garantias.Tipo_Garantia <= 3 THEN
            ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "2"
                   R_TipoGarantia.

        RUN F_AdNad.
        RUN Mostrar_Admisible.

        W_NvaAdm = FALSE.

        IF Garantias.Estado = 1 THEN
            Btn_InaAdm:LABEL IN FRAME F_Admisible = "Inactivar".
        ELSE
            Btn_InaAdm:LABEL IN FRAME F_Admisible = "Activar".
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConHV
&Scoped-define SELF-NAME Btn_OutConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConHV wWin
ON CHOOSE OF Btn_OutConHV IN FRAME F_ConHV /* Button 153 */
DO:
    DO WITH FRAME F_HojaVida:
        IF AVAILABLE Hoja_Vida THEN DO:
            ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = STRING(Hoja_Vida.Asunto_Cumplido)
                   Hoja_Vida.Observacion:SCREEN-VALUE = Hoja_Vida.Observacion
                   Hoja_Vida.Fec_Grabacion:SCREEN-VALUE = STRING(Hoja_Vida.Fec_Grabacion).
        END.

        HIDE FRAME F_ConHV.

        IF Hoja_Vida.Asunto_Cumplido THEN
            DISABLE Hoja_Vida.Observacion WITH FRAME F_HojaVida.
        ELSE
            ENABLE Hoja_Vida.Observacion WITH FRAME F_HojaVida.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_OutConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConsulta wWin
ON CHOOSE OF Btn_OutConsulta IN FRAME F_Consulta /* Button 133 */
DO:
    IF AVAILABLE Consulta THEN DO:
        RUN ProSolNit IN h_w-proclientesnew(consulta.nit).
        RUN ProSolNit IN h_w-adm_garantias(consulta.nit).

        RELEASE Solicitud.

        FIND FIRST Solicitud WHERE Solicitud.Num_Solicitud = Consulta.Num_Solicitud
                               AND Solicitud.Estado = 3 NO-ERROR.
        IF AVAILABLE(Solicitud) THEN DO:
            MESSAGE "La Solicitud YA FUE NEGADA, Desea Reconsinderar el Estado...?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

            IF Choice THEN
                Solicitud.Estado = 1.
            ELSE
                RETURN.
        END.

        FIND FIRST Solicitud WHERE Solicitud.Num_Solicitud = Consulta.Num_Solicitud
                               AND Solicitud.Estado <> 3 NO-ERROR.
        IF LOCKED Solicitud THEN DO:
            MESSAGE "Esta Solicitud esta siendo accesada por otro" SKIP
                    "asesor. Por este motivo no podrá ser modificada."
                VIEW-AS ALERT-BOX INFORMATION.

            APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
        END.
        ELSE DO:
            IF AVAILABLE Solicitud THEN
                RUN Mostrar_Solicitud.
            ELSE
                RETURN.
        END.

        HIDE FRAME F_Consulta.
        VIEW FRAME F_Solicitud.

        ASSIGN BUTTON-100:SENSITIVE IN FRAME F_Solicitud = TRUE
               BUTTON-164:SENSITIVE = TRUE.

        FIND FIRST Instancias WHERE Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NO-LOCK NO-ERROR.
        IF AVAILABLE(Instancias) AND NOT Instancias.Id_Scoring AND NOT Instancias.Id_Concepto THEN DO:
            ASSIGN BUTTON-100:SENSITIVE IN FRAME F_Solicitud = FALSE
                   BUTTON-164:SENSITIVE = FALSE.
        END.

        EMPTY TEMP-TABLE TCred_ACanc.

        FOR EACH Creditos WHERE Creditos.Nit = Solicitud.Nit
                            AND Creditos.Estado = 2
                            AND Creditos.Sdo_Capital > 0 NO-LOCK:
            CREATE TCred_ACanc.
            ASSIGN TCred_ACanc.Agen = Creditos.Agencia
                   TCred_ACanc.Pto = Creditos.Cod_Credito
                   TCred_ACanc.NumC = Creditos.Num_Credito
                   TCred_ACanc.FecD = Creditos.Fec_Desembolso
                   TCred_ACanc.CuoC = Creditos.Cuota
                   TCred_ACanc.Tasa = Creditos.Tasa
                   TCred_ACanc.SdoTD = Creditos.Sdo_Capital + Creditos.Int_Corrientes + Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas + Creditos.Honorarios - Creditos.Int_Anticipado.

            FIND FIRST solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = creditos.nit
                                                  AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud
                                                  AND solicitudes_pagoCreditos.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
            IF AVAILABLE solicitudes_pagoCreditos THEN DO:
                TCred_ACanc.CSiNo = solicitudes_pagoCreditos.pagoTotal.
                TCred_ACanc.valorAbono = solicitudes.valorAbono.
            END.
        END.

        totalAbonoCreditos = 0.

        FOR EACH bfrTTAbonos NO-LOCK:
            totalAbonoCreditos = totalAbonoCreditos + bfrTTAbonos.valorABono.
        END.

        W_TotCanc = totalAbonoCreditos.
        W_TotCanc:SCREEN-VALUE IN FRAME F_CredACanc = STRING(totalAbonoCreditos).  
        W_VrADesemb = DECIMAL(Solicitud.Total_Prestamo:SCREEN-VALUE) - DECIMAL(Solicitud.Deducible:SCREEN-VALUE) - W_TotCanc.
        W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).
        W_VrCredACanc = W_TotCanc.
        W_VrCredACanc:SCREEN-VALUE = STRING(W_TotCanc).
    END.

    IF W_Negadas = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN
        DISABLE Btn_Salvar WITH FRAME F_Creditos.
    ELSE
        ENABLE Btn_Salvar WITH FRAME F_Creditos.

    OPEN QUERY Br_Cred FOR EACH TCred_ACanc INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Garantias
&Scoped-define SELF-NAME Btn_OutGarantias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutGarantias wWin
ON CHOOSE OF Btn_OutGarantias IN FRAME F_Garantias /* Button 136 */
DO:
    ENABLE ALL WITH FRAME F_Creditos.
    DISABLE NomUsuario WITH FRAME F_Creditos.

    IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) <> W_Primera THEN
        DISABLE Btn_Ingresar WITH FRAME F_Creditos.

    FIND FIRST Clientes WHERE ROWID(Clientes) EQ puntero NO-LOCK NO-ERROR.
    HIDE FRAME F_Garantias.
    FRAME F_Consulta:HIDDEN = YES.
    APPLY "entry" TO Btn_Gar IN FRAME F_Solicitud.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Btn_OutProductos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutProductos wWin
ON CHOOSE OF Btn_OutProductos IN FRAME F_Producto /* x */
DO:
    DO WITH FRAME F_Producto:
        Solicitud.Cuota:SCREEN-VALUE IN FRAME F_Solicitud = "0".

        ENABLE Btn_Liquidar WITH FRAME F_Solicitud.

        IF AVAILABLE Pro_Creditos THEN DO:
            IF NOT AVAILABLE solicitud THEN
                FIND FIRST clientes WHERE clientes.nit = solicitud.nit:SCREEN-VALUE NO-LOCK NO-ERROR.
            ELSE
                FIND FIRST clientes WHERE clientes.nit = solicitud.nit NO-LOCK NO-ERROR.

            IF NOT AVAILABLE Clientes THEN DO:
                MESSAGE "Antes de escoger un producto de crédito" SKIP
                        "se debe escoger un cliente para poder" SKIP
                        "saber si este puede acceder al producto" SKIP
                        "escogido." SKIP(1)
                        "A continuación Escoja un Cliente"
                    VIEW-AS ALERT-BOX WARNING.

                HIDE FRAME F_Producto.
                APPLY "entry" TO Solicitud.Nit IN FRAME F_Solicitud.
                RETURN NO-APPLY.
            END.

            IF Pro_Creditos.Id_Asociado GT 1 THEN DO:
                IF Pro_Creditos.Id_Asociado EQ 2 AND Clientes.Tipo_Vinculo <> 1 THEN DO:
                    MESSAGE "Este producto esta diseñado solo para Asociados." SKIP
                            "El cliente elejido no es un Asociado."
                        VIEW-AS ALERT-BOX WARNING.

                    APPLY "entry" TO Cmb_Productos.
                    RETURN NO-APPLY.
                END.
                
                IF Pro_Creditos.Id_Asociado EQ 3 AND Clientes.Tipo_Vinculo <> 5 THEN DO:
                    MESSAGE "Este producto esta diseñado solo para Empleados." SKIP
                            "El cliente elejido no es un Empleado."
                        VIEW-AS ALERT-BOX WARNING.

                    APPLY "entry" TO Cmb_Productos.
                    RETURN NO-APPLY.
                END.
            END.

            Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud = Cmb_Productos:SCREEN-VALUE.

            CASE Solicitud.Tip_Credito:SCREEN-VALUE:
                WHEN "1" THEN W_Tipo_Credito:SCREEN-VALUE IN FRAME F_Solicitud = "  Consumo".
                WHEN "2" THEN W_Tipo_Credito:SCREEN-VALUE IN FRAME F_Solicitud = "  Comercial".
                WHEN "3" THEN W_Tipo_Credito:SCREEN-VALUE IN FRAME F_Solicitud = "  Hipotecario".
                WHEN "4" THEN W_Tipo_Credito:SCREEN-VALUE IN FRAME F_Solicitud = "  Microcredito".
            END CASE.

            IF Pro_Creditos.Id_Tasa EQ 2 THEN
                ASSIGN Solicitud.Tasa:BGCOL = 15
                       Solicitud.Tasa:FGCOL = 0
                       Solicitud.Tasa:SENSITIVE = YES.
            ELSE
                ASSIGN Solicitud.Tasa:BGCOL = 18
                       Solicitud.Tasa:FGCOL = 15
                       Solicitud.Tasa:SENSITIVE = NO.
        END.
    END.

    HIDE FRAME F_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoProducto
&Scoped-define SELF-NAME Btn_OutScoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutScoring wWin
ON CHOOSE OF Btn_OutScoring IN FRAME F_InfoProducto /* Button 121 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_InfoProducto.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Btn_OutUltima
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutUltima wWin
ON CHOOSE OF Btn_OutUltima IN FRAME F_Ultima /* Button 153 */
DO:
  HIDE FRAME F_Ultima.
  HIDE FRAME F_Condicionada.
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario Btn_Ingresar WITH FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_ProcesarInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ProcesarInstancia wWin
ON CHOOSE OF Btn_ProcesarInstancia IN FRAME F_Creditos /* Procesar Instancia */
DO:
/* Modificado por: giocam - Feb/15/08 - optimizar find y for each colocando indices y ordenando where  */
   DEF VAR W_NroDias AS INTEG FORM "99999".
   IF FRAME F_Solicitud:HIDDEN EQ YES THEN DO:
      MESSAGE "Al momento no existe ninguna solicitud a la cual" SKIP
              "se le pueda procesar la instancia." SKIP(1)
              "Escoja de la lista de consulta una solicitud" SKIP
              "haciendo doble click sobre ella" VIEW-AS ALERT-BOX INFORMATION.
      APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
      RETURN NO-APPLY.
   END.
   OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Solicitud INDEXED-REPOSITION.
   IF Br_Consulta:NUM-ENTRIES IN FRAME F_Consulta EQ 0 THEN DO:
      MESSAGE "Al momento no existe ninguna solicitud a la cual" SKIP
              "se le pueda procesar la instancia." VIEW-AS ALERT-BOX INFORMATION.
      APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
      RETURN NO-APPLY.
   END.
   IF W_VrADesemb < 0 THEN DO:
      MESSAGE "El valor a desembolsar es negativo, debe revisar los valores a cancelar."
               VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   IF Pro_Credito.Id_Garantia THEN DO:
      FIND FIRST Relaciones WHERE Relaciones.Nit          EQ Solicitud.Nit
                              AND INT(Relaciones.Cuenta)  EQ Solicitud.Num_Solicitud
                              AND Relaciones.Cod_Relacion EQ 11
                              AND Relaciones.Estado       EQ 1 NO-LOCK NO-ERROR.
      IF NOT AVAIL Relaciones THEN
         FIND FIRST Garantias WHERE Garantias.Nit           EQ Solicitud.Nit
                                AND Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud
                                AND Garantias.Estado        EQ 1 NO-LOCK NO-ERROR.
      IF NOT AVAIL Relaciones AND NOT AVAIL Garantias THEN DO:
         MESSAGE "Garantias son obligatorias para esta solicitud" VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.
   IF Solicitud.Per_Pago EQ 0 THEN W_NroDias = 1.
   IF Solicitud.Per_Pago EQ 1 THEN W_NroDias = 7.
   IF Solicitud.Per_Pago EQ 2 THEN W_NroDias = 10.
   IF Solicitud.Per_Pago EQ 3 THEN W_NroDias = 15.
   IF Solicitud.Per_Pago EQ 4 THEN W_NroDias = 30.
   IF Solicitud.Per_Pago EQ 5 THEN W_NroDias = 60.
   IF Solicitud.Per_Pago EQ 6 THEN W_NroDias = 90.
   IF Solicitud.Per_Pago EQ 7 THEN W_NroDias = 120.
   IF Solicitud.Per_Pago EQ 8 THEN W_NroDias = 180.
   IF Solicitud.Per_Pago EQ 9 THEN W_NroDias = 360.
   
   FIND FIRST Relaciones WHERE Relaciones.Nit            EQ Solicitud.Nit
                           AND Relaciones.Clase_Producto EQ 2
                           AND Relaciones.Cod_Producto   EQ INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
                           AND Relaciones.Cuenta         EQ Solicitud.Num_Solicitud:SCREEN-VALUE  IN FRAME F_Solicitud
                           AND Relaciones.Cod_Relacion   EQ 11
                           AND Relaciones.Estado         EQ 1 NO-LOCK NO-ERROR.
/* Por Libranzas
   IF Pro_Creditos.Id_Sorteos THEN DO:
   /* Se pasa de 4 a 6 s.m.m.l.v  segun art.10 creditos con libranza 23 dic/2005 jjmp*/
      IF NOT AVAIL(Relaciones) AND Solicitud.Monto GT ROUND(Clientes.Salario * 6,0) THEN DO:
         MESSAGE "Para Creditos por Libranza con Montos Superiores a 6 Salarios del Solicitante..." SKIP
                 "Es Necesario Codeudor...No se permite la operaciòn."
                  VIEW-AS ALERT-BOX TITLE "Verificar Codeudores".
         APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
         RETURN NO-APPLY.
      END.
   END. */
   FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito USE-INDEX Ix-PpalUnico NO-LOCK NO-ERROR.
   FIND FIRST Clientes WHERE Clientes.Nit EQ Solicitud.Nit USE-INDEX Icli_nit NO-ERROR. 
   IF Clientes.Tip_Contrato EQ 1 OR Clientes.Tip_Contrato EQ 2 OR Clientes.Tip_Contrato EQ 5 THEN DO:
   /* IF NOT AVAIL(Relaciones) THEN DO:
         IF ROUND(W_SMLV * 2.0,0) GT Clientes.Salario
            AND INT(SUBSTR(Cmb_Productos:SCREEN-VALUE,1,3)) EQ 2 THEN DO:
            MESSAGE "Para Dependientes-Empleados, con salario menor que 2 SMMLV..." SKIP
                    "Es Necesario Codeudor...No se permite la operaciòn."
                     VIEW-AS ALERT-BOX TITLE "Verificar Codeudores".
            APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
            RETURN NO-APPLY.
         END.
      END.  Comentariado Enero 24/06 Gaer*/
   END.
   ELSE DO:
      IF Cfg_RegCredito.Agencia_Exigida EQ 11 AND NOT AVAIL(Relaciones) AND Solicitud.Monto GE 1000001 THEN DO:
         MESSAGE "Para MicroCredito es necesario codeudor para este monto..." SKIP 
                 "                         No se permite la operaciòn."
                  VIEW-AS ALERT-BOX TITLE "Verificar codeudores".
         APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
         RETURN NO-APPLY.
      END.
      ELSE
      IF NOT AVAIL(Relaciones) AND INT(SUBSTR(Cmb_Productos:SCREEN-VALUE,1,3)) EQ 2 AND Clientes.Tipo_Vivienda EQ 2 THEN DO:
         FIND FIRST Garantias WHERE Garantias.Agencia       EQ Solicitud.Agencia
                                AND Garantias.Cod_Credito   EQ Solicitud.Cod_Credito
                                AND Garantias.Tip_Credito   EQ Solicitud.Tip_Credito
                                AND Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud
                                AND Garantias.Nit           EQ Solicitud.Nit
                                AND Garantias.Estado        EQ 1 USE-INDEX idx_admisibles NO-ERROR.
         IF NOT AVAIL(garantias)  THEN DO:
            MESSAGE "Para independientes es necesario codeudor o una garantia admisible..." SKIP 
                    "Solicitante sin propiedad.                  No se permite la operaciòn."
                     VIEW-AS ALERT-BOX TITLE "Verificar Codeudores".
            APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
            RETURN NO-APPLY.
         END.
      END.
   END.
/* IF Cfg_RegCredito.Vr_MontoCodeud GT 0 AND Solicitud.Monto GE Cfg_RegCredito.Vr_MontoCodeud THEN DO:
      FIND FIRST Garantias WHERE Garantias.Agencia       EQ Solicitud.Agencia
                             AND Garantias.Nit           EQ Solicitud.Nit
                             AND Garantias.Tip_Credito   EQ Solicitud.Tip_Credito
                             AND Garantias.Cod_Credito   EQ Solicitud.Cod_Credito
                             AND Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud
                             AND Garantias.Estado        EQ 1 NO-LOCK NO-ERROR.                             
      IF NOT AVAIL(Garantias) THEN DO:
         MESSAGE "La Radicaciòn exige Garantìa..." SKIP
                 "                                No se permite la Operaciòn..."
                  VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
   END.*/
   IF NOT PDFImpreso AND W_Primera = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
       MESSAGE "Antes de procesar instancia debe imprimir un PDF con la solicitud para firma por el asociado. ¿Ir al PDF?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Imprimir solicitud" UPDATE W_SiNoI AS LOGICAL.

       IF W_SiNoI THEN DO:
           PDFImpreso = YES.
           APPLY "choose" TO PDF IN FRAME F_Creditos.
           RETURN NO-APPLY.
       END.
   END.
   
   HIDE FRAME F_Consultas.
   DISABLE ALL WITH FRAME F_Creditos.
   IF AVAIL Solicitud THEN DO:
      FIND FIRST Mov_Instancias WHERE 
           Mov_Instancias.Instancia EQ INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
           Mov_Instancias.Nit       EQ STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
           Mov_Instancias.Num_Solicitud EQ INT(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
           Mov_Instancias.Estado    EQ NO NO-LOCK NO-ERROR.
      IF AVAIL Mov_Instancias THEN DO:
         DO WITH FRAME F_Instancias:
            ASSIGN Mov_Instancias.Estado:SCREEN-VALUE = STRING(Mov_Instancias.Estado)
                   Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
                   WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
                   Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
                   Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
                   W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos
                   W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.
            Vigencia:SCREEN-VALUE = STRING(w_fecha - Mov_Instancias.Fec_Ingreso) + " Dias".
            ENABLE Mov_Instancias.Estado Mov_Instancias.Descripcion
                   Btn_GraInstancia Btn_InsVolver WITH FRAME F_Instancias.
            RUN Buscar_Instancias_Cerradas.
         END.
         VIEW FRAME F_Instancias.
         APPLY "entry" TO Mov_Instancias.Estado IN FRAME F_Instancias.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         MESSAGE "La Solicitud no esta disponible" SKIP
                 "o no se ha cerrado la instancia anterior" SKIP
                 "escoja una solicitud de la lista de " SKIP
                 "Solicitudes disponibles!" VIEW-AS ALERT-BOX WARNING.
         ENABLE ALL WITH FRAME F_Creditos.
         DISABLE NomUsuario WITH FRAME F_Creditos.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Btn_Proyectar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proyectar wWin
ON CHOOSE OF Btn_Proyectar IN FRAME F_Solicitud /* Proyección de Pagos */
DO:
    IF Btn_Liquidar:SENSITIVE IN FRAME F_Solicitud = YES THEN DO:
        MESSAGE "Para sacar la proyeccion es necesario" SKIP
                "haber liquidado. Haga Click en el boton" SKIP
                "de liquidar y vuelva a intentar sacar la" SKIP
                "la proyeccion del crédito solicitado!"
            VIEW-AS ALERT-BOX INFORMATION.

        APPLY "entry" TO Btn_Liquidar IN FRAME F_Solicitud.
        RETURN NO-APPLY.
    END.

    DEFINE VAR Listado AS CHARACTER INITIAL "".

    ASSIGN Listado = W_PathSpl + "Proyeccion" + W_Usuario + STRING(RANDOM(2000,10000)) + ".LST"
           W_TipoInforme = "Proyeccion".

    {INCLUIDO\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Btn_SalAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalAdm wWin
ON CHOOSE OF Btn_SalAdm IN FRAME F_Admisible /* Salvar */
DO:

    DEFI VAR W_RowIdGar AS ROWID.
    
    DO WITH FRAME F_Admisible:
        IF W_NvaAdm 
        AND (Garantias.Identificacion_Bien:SCREEN-VALUE EQ "" OR Garantias.Nom_Bien:SCREEN-VALUE EQ "" 
             OR Garantias.Val_Bien:SCREEN-VALUE EQ "0" )  AND  Garantias.tipo_garantia:SCREEN-VALUE NE "6"
        THEN DO:
            MESSAGE "Para poder salvar una nueva garantia admisible" SKIP
                    "debe digitarse al menos la siguiente información:" SKIP(1)
                    "  - Identificación del Bien" SKIP
                    "  - Nombre del Bien" SKIP
                    "  - Valor del Bien" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO Garantias.Identificacion_Bien.
            RETURN.
        END.
    
        DISABLE Btn_SalAdm WITH FRAME F_Admisible.
        ENABLE Btn_IngAdm WITH FRAME F_Admisible.
    
        IF W_NvaAdm 
        THEN    CREATE Garantias.
        ELSE    FIND CURRENT Garantias EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        DO WHILE LOCKED(garantias):
            MESSAGE "ERROR: Actualizando Garantía. Registro En Uso Por Otro Usuario" SKIP "Espere o CTRL-C Para Abortar La Transacción"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            FIND CURRENT Garantias EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        END.
        ASSIGN  Garantias.Agencia             = Solicitud.Agencia
                Garantias.Tip_Credito         = Solicitud.Tip_Credito
                Garantias.Cod_Credito         = Solicitud.Cod_Credito
                Garantias.Num_Solicitud       = Solicitud.Num_Solicitud
                Garantias.Tipo_Garantia       = INTEGER(Garantias.Tipo_Garantia:SCREEN-VALUE)
                Garantias.Identificacion_Bien = Garantias.Identificacion_Bien:SCREEN-VALUE
                Garantias.Nom_Bien            = Garantias.Nom_Bien:SCREEN-VALUE
                Garantias.Descripcion_Bien    = Garantias.Descripcion_Bien:SCREEN-VALUE
                Garantias.Descripcion_Bien2   = Garantias.Descripcion_Bien2:SCREEN-VALUE
                Garantias.Fec_Creacion        = DATE(Garantias.Fec_Creacion:SCREEN-VALUE)
                Garantias.Fec_Retiro          = DATE(Garantias.Fec_Retiro:SCREEN-VALUE)
                Garantias.Nit_Aseguradora     = Garantias.Nit_Aseguradora:SCREEN-VALUE
                Garantias.Nro_Seguro          = Garantias.Nro_Seguro:SCREEN-VALUE
                Garantias.Fec_IniSeguro       = DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE)
                Garantias.Fec_FinSeguro       = DATE(Garantias.Fec_FinSeguro:SCREEN-VALUE).
    
        ASSIGN  Garantias.Val_Asegurado       = DECIMAL(Garantias.Val_Asegurado:SCREEN-VALUE)
                Garantias.Usuario             = W_Usuario
                Garantias.Val_Bien            = DECIMAL(Garantias.Val_Bien:SCREEN-VALUE)
                Garantias.Nit                 = Solicitud.Nit
                Garantias.Nom_Impuesto        = Garantias.Nom_Impuesto:SCREEN-VALUE
                Garantias.Val_Impuesto        = DECIMAL(Garantias.Val_Impuesto:SCREEN-VALUE)
                Garantias.Fec_VctoImpuesto    = DATE(Garantias.Fec_VctoImpuesto:SCREEN-VALUE)
                Garantias.Fec_UltAvaluo       = DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE)
                Garantias.Fec_ProxAvaluo      = DATE(Garantias.Fec_ProxAvaluo:SCREEN-VALUE)
                Garantias.Val_UltAvaluo       = DECIMAL(Garantias.Val_UltAvaluo:SCREEN-VALUE)
                Garantias.Aprobada.
    
        IF Garantias.Tipo_Garantia EQ 4 
        THEN DO:
            FIND FIRST Ahorros 
                WHERE 
                    (Ahorros.Tip_Ahorro EQ 2 OR Ahorros.Tip_Ahorro EQ 3)   
                AND Ahorros.Nit EQ Garantias.Nit_Aseguradora 
                AND Ahorros.Cue_Ahorros EQ Garantias.Identificacion_Bien 
                AND Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
            ASSIGN Garantias.Cod_AhorroNA = Ahorros.Cod_Ahorro WHEN AVAIL(Ahorros).
            ASSIGN Garantias.Id_Interna   = TRUE WHEN AVAIL(Ahorros).
        END.
    
        ASSIGN  W_NvaAdm     = NO
                W_CredAval   = 0
                W_DispGaran  = 0
                W_CredAval:SCREEN-VALUE   = "0"
                W_DispGaran:SCREEN-VALUE  = "0"
                W_NBien                   = Garantias.Identificacion_Bien
                W_RowIdGar                = ROWID(Garantias).
    
    
        RUN Halla_DispGaran.
    
        FIND Garantias WHERE ROWID(Garantias) EQ W_RowidGar NO-LOCK NO-ERROR.
    
        /*Garantias.Identificacion_Bien:SENSITIVE = FALSE.*/
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME Btn_SalCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalCod wWin
ON CHOOSE OF Btn_SalCod IN FRAME F_Codeudores /* Salvar */
DO:
DEFI VAR W_RowIdR AS ROWID.

DO WITH FRAME F_Codeudores:
  IF W_NitCodeudor:SCREEN-VALUE LE "0" THEN DO:
     MESSAGE "El nit a relacionar no puede ser blancos-0" SKIP
             "Digite el nit a relacionar." VIEW-AS ALERT-BOX WARNING.
     APPLY "entry" TO W_NitCodeudor.
     RETURN NO-APPLY.
  END.

  FIND FIRST Relaciones WHERE 
        Relaciones.Nit          EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
        Relaciones.Cuenta       EQ STRING(Solicitud.Num_Solicitud:SCREEN-VALUE) AND
        Relaciones.Cod_Relacion EQ 11 AND
        Relaciones.Nit_Relacion EQ W_NitCodeudor:SCREEN-VALUE NO-ERROR.

  IF W_NvoCD AND NOT AVAIL(Relaciones) THEN DO:
     CREATE Relaciones.
     Relaciones.Estado = 1.
  END.

  ASSIGN Relaciones.Nit             = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
         Relaciones.Cod_Relacion    = 11
         Relaciones.Nit_Relacion    = W_NitCodeudor:SCREEN-VALUE
         Relaciones.Usuario         = W_Usuario
         Relaciones.Fec_Ingreso     = W_Fecha          
         Relaciones.Clase_Producto  = 2
         Relaciones.Cod_Producto    = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
         Relaciones.Cuenta          = Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud
         Relaciones.Aprobada.

  ASSIGN W_NvoCD = NO
         W_RowIdR = ROWID(Relaciones).

  FIND CURRENT Relaciones NO-LOCK NO-ERROR.

  RUN QUERY_Relaciones.
  ASSIGN W_NitCodeudor:SCREEN-VALUE = " "
         W_NomCodeudor:SCREEN-VALUE = "".
    
  DISABLE W_NitCodeudor Btn_SalCod Btn_CanCod.
  ENABLE Btn_CreCod Btn_Activas.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_SalvaHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaHV wWin
ON CHOOSE OF Btn_SalvaHV IN FRAME F_HojaVida /* Salvar */
DO:
    DISABLE Btn_SalvaHV WITH FRAME F_HojaVida.
    ENABLE  Btn_NvoHV WITH FRAME F_HojaVida.
    IF W_NvaHv 
    THEN DO:
        CREATE Hoja_Vida.
        ASSIGN  Hoja_Vida.Tipo = 9 
                Hoja_Vida.Codigo = 1  
                Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                Hoja_Vida.DoctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                Hoja_Vida.Nit        = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                Hoja_Vida.Usuario    = W_Usuario
                Hoja_Vida.Fec_Grabacion = w_fecha
                Hoja_Vida.Hora_Grabacion = TIME.
                W_NvaHv = NO.
                Hoja_Vida.Observacion:READ-ONLY = YES.
    END.
    ELSE FIND CURRENT Hoja_Vida EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    DO WHILE LOCKED(Hoja_Vida):
        MESSAGE "ERROR: Actualizando Hoja De Vida. Registro En Uso Por Otro Usuario" SKIP "Intente Más Tarde"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ASSIGN FRAME F_HojaVida Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion.
    IF AVAILABLE Hoja_Vida AND Hoja_Vida.Asunto_Cumplido 
    THEN APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Creditos /* Salvar */
DO:
    DEF VAR W_GtosIndir LIKE Clientes.Gto_obligacion INIT 0.
    DEF VAR W_GtoCoop LIKE Clientes.Gto_obligacion INIT 0.
    DEF VAR W_TotApor LIKE Ahorros.Sdo_disponib INIT 0.
    DEF VAR W_Cod_Anterior LIKE Solicitud.Cod_Credito.
    DEF VAR W_Valida AS LOG INIT FALSE.

    RUN validar_solicitud NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.

    DO WITH FRAME F_Solicitud:
        /* validaciones pantalla*/
        APPLY "leave" TO Solicitud.Monto.

        APPLY "leave" TO Solicitud.Plazo.

        RUN Liquidar.

        DISABLE Btn_Liquidar WITH FRAME F_Solicitud.
        
        IF Btn_Liquidar:SENSITIVE EQ YES THEN DO:
            MESSAGE "No se puede salvar la Solicitud ya que esta no se ha liquidado"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Btn_Liquidar.
            RETURN.
        END.

        IF Solicitud.Nit:SCREEN-VALUE EQ "" THEN DO:
            MESSAGE "No se puede Salvar la Solicitud. Falta ingresar el Nit"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Solicitud.Nit.
            RETURN.
        END.

        IF DEC(Solicitud.Monto:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede Salvar la Solicitud. Falta ingresar el Monto"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Solicitud.Monto.
            RETURN.
        END.

        IF INT(Solicitud.Plazo:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede Salvar la Solicitud. Falta ingresar el Plazo"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Solicitud.Plazo.
            RETURN.
        END.

        IF DEC(Solicitud.Tasa:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede Salvar la Solicitud. Falta la tasa, para esto debe entrar el plazo"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Btn_Liquidar.
            RETURN.
        END.

        IF DEC(Solicitud.Cuota:SCREEN-VALUE) EQ 0 THEN DO:
            MESSAGE "No se puede Salvar la Solicitud. Falta Liquidarla para que se calcule la Cuota"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Btn_Liquidar.
            RETURN.
        END.

        RUN Valida_Soli.R (INPUT Pro_Creditos.Cod_Credito,
                           INPUT Solicitud.Nit:SCREEN-VALUE,
                           INPUT DEC(Solicitud.Monto:SCREEN-VALUE),
                           INPUT INT(Solicitud.Plazo:SCREEN-VALUE),
                           INPUT INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE,1,3)),
                           INPUT INT(Solicitud.For_Pago:SCREEN-VALUE IN FRAME F_Forpago),
                           INPUT vPeriodicidad,
                           OUTPUT W_Valida) NO-ERROR.

        IF W_Valida THEN
            RETURN.
        /* fin validaciones pantalla*/

        
        IF W_Nuevo THEN DO:
            CREATE Solicitud.
            ASSIGN Solicitud.Num_Solicitud = NEXT-VALUE(Sec_Solicitud)
                   Solicitud.Estado = 1
                   Solicitud.Usuario = W_Usuario.

            Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud = STRING(Solicitud.Num_Solicitud).
        END.
        ELSE DO:
            FIND FIRST Solicitud WHERE Solicitud.Agencia EQ INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE,1,3))
                                   AND Solicitud.Num_Solicitud EQ INT(Solicitud.Num_Solicitud:SCREEN-VALUE)
                                   AND Solicitud.Nit EQ Solicitud.Nit:SCREEN-VALUE EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL Solicitud THEN DO:
                MESSAGE "No existe la Solicitud a la cual se le quieren hacer los cambios. Rectifique" SKIP
                        "la Solicitud consultándola y haciendo de nuevo los cambios!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
                RETURN.
            END.
        END.

        IF NOT CAN-FIND(Anexos_Clientes WHERE Anexos_Clientes.nit = Solicitud.nit) THEN DO:
            CREATE Anexos_Clientes.
            Anexos_Clientes.nit = Solicitud.nit.
        END.
        ELSE DO:
            FIND FIRST Anexos_Clientes EXCLUSIVE-LOCK WHERE Anexos_Clientes.nit = Solicitud.nit NO-ERROR.
            DO WHILE LOCKED Anexos_Clientes:
                MESSAGE "Registro En Uso Por Otro Usuario. Espere O CTRL-C Para Abortar"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                FIND FIRST Anexos_Clientes EXCLUSIVE-LOCK WHERE Anexos_Clientes.nit = Solicitud.nit NO-ERROR.
            END.
        END.

        ASSIGN FRAME F_Solicitud
            Solicitud.Nit
            Solicitud.Destinof
            Solicitud.Fec_Solicitud
            Solicitud.FOR_Interes
            Solicitud.Monto
            Solicitud.Cuota
            Solicitud.Deducible
            Solicitud.Total_Prestamo
            solicitud.tasa
            solicitud.fec_desembolso
            solicitud.fec_primerPago.

        ASSIGN Solicitud.Agencia = INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE,1,3))
               Solicitud.Per_Pago = INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1))
               Solicitud.Sistema = INT(SUBSTR(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5))
               Solicitud.Id_Adicionales = 2.

        IF NOT W_Nuevo THEN DO:
            FOR EACH Relaciones WHERE Relaciones.Nit EQ Solicitud.Nit
                                  AND Relaciones.Clase_Producto EQ 2
                                  AND Relaciones.Cod_Producto EQ Solicitud.Cod_Credito
                                  AND Relaciones.Cod_Relacion EQ 11
                                  AND Relaciones.Estado EQ 1 EXCLUSIVE-LOCK:
                Relaciones.Cod_producto = Pro_Creditos.Cod_Credito.
            END.
        END.

        DO WITH FRAME F_Producto:
            ASSIGN Solicitud.Tip_Credito
                   Solicitud.Cod_Credito = Pro_Creditos.Cod_Credito.
        END.

        ASSIGN Solicitud.Conocimiento_Cliente = Cmb_ConCte:SCREEN-VALUE IN FRAME F_VblesS
               Solicitud.Destino = Cmb_destino:SCREEN-VALUE
               Solicitud.Mora_Comercial = Cmb_MoraCial:SCREEN-VALUE
               Solicitud.Garantia = Cmb_Gtia:SCREEN-VALUE
               Solicitud.Respaldo_Patrim = Cmb_ResPat:SCREEN-VALUE
               Solicitud.Tipo_Actividad = Cmb_TipAct:SCREEN-VALUE.

        FIND FIRST Clientes WHERE Clientes.Nit EQ Solicitud.Nit NO-LOCK NO-ERROR.

        FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

        ASSIGN Solicitud.Endeud_Indirecto = Clientes.Endeud_Indirecto
               Solicitud.Capacidad_Pago = Clientes.Capacidad_pago.

        IF Cfg_RegCredito.Agencia_Exigida EQ 11 AND Pro_Creditos.Tip_Credito EQ 4 THEN DO: /*Enero 19/06 GAER*/
            IF W_LiqDispon LE Solicitud.Cuota THEN
                MESSAGE "La Liquidez Disponible es Menor o Igual al valor de la Cuota..."
                    VIEW-AS ALERT-BOX TITLE "      Alerta de Viabilidad".
        END.

        Solicitud.Puntaje  = Clientes.Puntaje.

        ASSIGN Solicitud.Concepto = W_Concepto WHEN W_Concepto GT " ".

        CASE SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1):
            WHEN "0" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 1.
            WHEN "1" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 7.
            WHEN "2" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 10.
            WHEN "3" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 15.
            WHEN "4" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 30.
            WHEN "5" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 60.
            WHEN "6" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 90.
            WHEN "7" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 120.
            WHEN "8" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 180.
            WHEN "9" THEN Solicitud.Plazo = INT(Solicitud.Plazo:SCREEN-VALUE) * 360.
        END CASE.

        DISABLE Solicitud.Nit WITH FRAME F_Solicitud.
    END.

    DO WITH FRAME F_Desembolso:
        ASSIGN Solicitud.Desembolso
               Solicitud.Age_Desembolso
               Solicitud.Cod_Desembolso
               Solicitud.Cue_Desembolso.
    END.

    DO WITH FRAME F_ForPago:
        ASSIGN FRAME F_ForPago
            Solicitud.FOR_Pago
            Solicitud.Age_DebAutomatico
            Solicitud.Cod_DebAutomatico
            Solicitud.Cue_DebAutomatico
            Anexos_Clientes.cam_cat1.
    END.

    IF W_Nuevo THEN DO:
        CREATE Mov_Instancias.
        ASSIGN Mov_Instancias.Fec_Ingreso = w_fecha
               Mov_Instancias.Hora_Ingreso = TIME
               Mov_Instancias.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
               Mov_Instancias.Num_Solicitud = DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE)
               Mov_Instancias.Usuario = W_Usuario.

        Mov_Instancias.Instancia = INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)).
        Mov_Instancias.Agencia = W_Agencia.

        CREATE Consulta.
        ASSIGN Consulta.AgeSolicitud = Solicitud.Agencia
               Consulta.Estado = Solicitud.Estado
               Consulta.Num_Solicitud = Mov_Instancias.Num_Solicitud
               Consulta.Fec_Ingreso = Mov_Instancias.Fec_Ingreso
               Consulta.Hor_Ingreso = STRING(Mov_Instancias.Hora_Ingreso,"HH:MM:SS am").

        FIND FIRST Clientes WHERE Clientes.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
        IF AVAIL Clientes THEN
            ASSIGN Consulta.Nit = Clientes.Nit
                   Consulta.Nombre = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    END.

    W_Nuevo = NO.

    ENABLE Btn_HojaVida Btn_Gar WITH FRAME F_Solicitud.
    ENABLE PDF WITH FRAME F_Creditos.

    IF W_Primera EQ INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN
        ENABLE Btn_Ingresar WITH FRAME F_Creditos.
    ELSE
        DISABLE Btn_Ingresar WITH FRAME F_Creditos.

    ASSIGN W_VrADesemb = Solicitud.TOTAL_Prestamo - Solicitud.Deducible - W_TotCanc
           W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).

    FIND CURRENT Anexos_Clientes NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Btn_SalvaUltima
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaUltima wWin
ON CHOOSE OF Btn_SalvaUltima IN FRAME F_Ultima /* Salvar */
DO:
    DO WITH FRAME F_Ultima:
        FIND FIRST Clientes WHERE Clientes.Nit EQ Solicitud.Nit
                              AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAIL(Clientes) THEN DO:
            MESSAGE "Falta el Cliente con Estado Activo para realizar la operación..."
                VIEW-AS ALERT-BOX ERROR.

            RETURN NO-APPLY.
        END.

        CASE Solicitud.Estado:SCREEN-VALUE:
            WHEN "1" THEN DO:
                MESSAGE "Como es la última instancia, se espera que se tome una" SKIP
                        "desición acerca de esta. La Solicitud No puede quedar" SKIP
                        "En Estudio. Rectifique su decisión!"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "entry" TO Solicitud.Estado.
                RETURN NO-APPLY.
            END.

            WHEN "2" THEN DO:
                MESSAGE "               Seguro de APROBAR la Solicitud...?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Aprobada" UPDATE W_SiConf AS LOGICAL.

                IF NOT W_SiConf THEN
                    RETURN.

                control_grabar = FALSE.

                RUN Verificar_relaciones_Garantias NO-ERROR.

                IF control_grabar THEN
                    RUN Grabar_Credito.
            END.

            WHEN "3" THEN DO:
                MESSAGE "               Seguro de NEGAR la Solicitud...?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Negada" UPDATE W_SiNeg AS LOGICAL.

                IF NOT W_SiNeg THEN
                    RETURN.

                RUN Negar_Solicitud.
            END.

            WHEN "4" THEN DO:
                FIND FIRST Tuxi WHERE ROWID(Tuxi) EQ W_RowIdTx NO-ERROR.
                IF NOT AVAILABLE Tuxi THEN DO:
                    MESSAGE "No se encuentra disponible el usuario" SKIP
                            "para condicionar la solicitud." SKIP
                            "No se permite la Operacion."
                        VIEW-AS ALERT-BOX.
                    
                    RETURN.
                END.

                IF E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada EQ "" THEN DO:
                    MESSAGE "Debe ingresarse algún comentario o condición para la" SKIP
                            "solicitud que se pasará a otro usuario" SKIP(1)
                            "Escriba la condición a cumplir"
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN.
                END.

                RUN Condicionar.
            END.
        END CASE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON LEAVE OF Buscar IN FRAME F_Consulta
DO:
  CASE R_Organizar:SCREEN-VALUE:
    WHEN "1" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Num_Solicitud EQ DECIMAL(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Num_Solicitud INDEXED-REPOSITION.
    END.
    WHEN "2" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.AgeSolicitud EQ DECIMAL(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.AgeSolicitud INDEXED-REPOSITION.
    END.
    WHEN "3" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Nit EQ SELF:SCREEN-VALUE
                NO-LOCK  BY Consulta.Nit INDEXED-REPOSITION.
    END.
    WHEN "4" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Nombre BEGINS SELF:SCREEN-VALUE
                NO-LOCK  BY Consulta.Nombre INDEXED-REPOSITION.
    END.
    WHEN "5" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Fec_Ingreso EQ DATE(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Fec_ingreso INDEXED-REPOSITION.
    END.
    WHEN "6" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Vigencia EQ INTEGER(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Vigencia INDEXED-REPOSITION.
    END.
  END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME BUT-IMP-Scoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUT-IMP-Scoring wWin
ON CHOOSE OF BUT-IMP-Scoring IN FRAME F_Scoring /* Button 208 */
DO:
    RUN ImprmeScring.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-100
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-100 wWin
ON CHOOSE OF BUTTON-100 IN FRAME F_Solicitud /* Scoring */
DO:
    IF NOT((solicitud.FOR_pago = 1 AND can-do("fcapagocaja",anexos_clientes.cam_cat1))
    OR (solicitud.FOR_pago = 2 AND can-do("f50%,fsmmlv",anexos_clientes.cam_cat1)))
    THEN DO:
        MESSAGE "Scoring No Disponible. Verifique Los Datos De Forma De Pago"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud EQ "" OR NOT AVAIL(Solicitud)     
    THEN DO:
        MESSAGE "Para Consultar el Scoring debe seleccionar una Solicitud, " SKIP
                VIEW-AS ALERT-BOX INFORMATION.
        APPLY "entry" TO Solicitud.Nit IN FRAME F_Solicitud.
        RETURN NO-APPLY.
    END.
    
    DISABLE ALL WITH FRAME F_Creditos.
    
    ENABLE ALL EXCEPT TOTAL_Puntaje W_Concepto WITH FRAME F_Scoring.
    
    ASSIGN  TOTAL_Puntaje              = 0
            W_Concepto                 = ""
            TOTAL_Puntaje:SCREEN-VALUE = "0"
            W_Concepto:SCREEN-VALUE    = "".
    APPLY "choose" TO BUTTON-164.    
    fCpcdadPgo(). 
    RUN Proceso_Scoring.
    VIEW FRAME F_Scoring.
    
    ASSIGN  TOTAL_Puntaje:VISIBLE = FALSE
            W_Concepto:VISIBLE    = FALSE.
    
    FIND FIRST Instancias 
        WHERE 
            Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NO-LOCK NO-ERROR.
    
    IF Instancias.Id_Scoring 
    THEN TOTAL_Puntaje:VISIBLE = TRUE.
    
    IF Instancias.Id_Concepto 
    THEN W_Concepto:VISIBLE = TRUE.   
    APPLY "choose" TO btn_ejecutar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Deducibles
&Scoped-define SELF-NAME BUTTON-101
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-101 wWin
ON CHOOSE OF BUTTON-101 IN FRAME F_Deducibles /* Button 101 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_Deducibles.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-102
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-102 wWin
ON CHOOSE OF BUTTON-102 IN FRAME F_Solicitud /* Deducibles */
DO:
  IF NOT AVAILABLE Pro_Creditos THEN DO:
     MESSAGE "Para ver los deducibles de un producto" SKIP
             "primero debe escoger este.!" VIEW-AS ALERT-BOX INFORMATION.
     VIEW FRAME F_Producto.
     APPLY "entry" TO Cmb_Productos IN FRAME F_Producto.
     RETURN NO-APPLY.
  END.
  OPEN QUERY Br_Deducibles FOR EACH TDeducc NO-LOCK INDEXED-REPOSITION.
  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_Deducibles.
  VIEW FRAME F_Deducibles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-103
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-103 wWin
ON CHOOSE OF BUTTON-103 IN FRAME F_Solicitud /* Button 103 */
DO:
    solicitud.tip_credito:SCREEN-VALUE IN FRAME f_producto = "1" .
    APPLY "Mouse-Select-Click" TO Solicitud.Tip_Credito IN FRAME F_Producto.
    /*Cmb_Producto:SCREEN-VALUE IN FRAME F_Producto = */
   /* DYNAMIC-FUNCTION('fActualzaTposPrdctos':U).*/
    VIEW FRAME F_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Desembolso
&Scoped-define SELF-NAME BUTTON-104
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-104 wWin
ON CHOOSE OF BUTTON-104 IN FRAME F_Desembolso /* Button 104 */
DO:
  DEFINE VARIABLE W_Des AS CHARACTER FORMAT "X(45)".
DO WITH FRAME F_Desembolso:
  CASE Solicitud.Desembolso:SCREEN-VALUE:
    WHEN "1" THEN W_Des = "Efectivo".
    WHEN "2" THEN W_Des = "Cheque".
    WHEN "3" THEN W_Des = "Cuenta de Ahorros".
    WHEN "4" THEN W_Des = "Orden a Terceros".
  END CASE.
  W_Desembolso:SCREEN-VALUE IN FRAME F_Solicitud = W_Des.
END.
  HIDE FRAME F_Desembolso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-105
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-105 wWin
ON CHOOSE OF BUTTON-105 IN FRAME F_Solicitud /* Button 105 */
DO:
  IF Solicitud.Nit:SCREEN-VALUE NE "" THEN
     VIEW FRAME F_Desembolso.
  ELSE DO:
     MESSAGE "Para la Solicitud se debe entrar primero" SKIP
             "el nit del cliente solicitante. Rectifique" VIEW-AS ALERT-BOX ERROR.
     IF Solicitud.Nit:SENSITIVE EQ YES THEN DO:
        APPLY "entry" TO Solicitud.Nit.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ForPago
&Scoped-define SELF-NAME BUTTON-106
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-106 wWin
ON CHOOSE OF BUTTON-106 IN FRAME F_ForPago /* Button 106 */
DO:
  DEFINE VAR W_FP AS CHARACTER FORMAT "X(25)".
DO WITH FRAME F_ForPago:
  CASE Solicitud.FOR_Pago:SCREEN-VALUE:
    WHEN "1" THEN W_Fp = "Caja".
    WHEN "2" THEN W_Fp = "Nomina".
    WHEN "3" THEN W_Fp = "Débito Automático".
    WHEN "4" THEN W_Fp = "Nomina Crecediario".
    WHEN "5" THEN W_Fp = "Prima".
  END CASE.
  W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = W_Fp.
END.
HIDE FRAME F_ForPago.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-107
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-107 wWin
ON CHOOSE OF BUTTON-107 IN FRAME F_Solicitud /* Button 107 */
DO:
  IF Solicitud.Nit:SCREEN-VALUE NE "" THEN DO:
     ASSIGN Btn_Liquidar:SENSITIVE = TRUE.
     
     VIEW FRAME F_ForPago.
  END.
  ELSE DO:
     MESSAGE "Para la solicitud se debe entrar primero" SKIP
             "el nit del cliente solicitante. Rectifique" VIEW-AS ALERT-BOX ERROR.
     IF Solicitud.Nit:SENSITIVE EQ YES THEN DO:
        APPLY "entry" TO Solicitud.Nit.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-108
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-108 wWin
ON CHOOSE OF BUTTON-108 IN FRAME F_InfoCliente /* Button 108 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_InfoCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Solicitud /* Producto */
DO:
 DO WITH FRAME F_Solicitud:
  IF Nom_Producto:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "Para mostrar la información del producto" SKIP
             "Primero se debe escoger uno. Rectifiqe!" VIEW-AS ALERT-BOX INFORMATION.
     VIEW FRAME F_Producto.
     APPLY "entry" TO Cmb_Productos.
     RETURN NO-APPLY.
  END.

  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_InfoProducto.
  VIEW FRAME F_InfoProducto.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME BUTTON-131
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-131 wWin
ON CHOOSE OF BUTTON-131 IN FRAME F_Producto /* Salir sin escoger */
DO:
  HIDE FRAME F_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME BUTTON-142
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-142 wWin
ON CHOOSE OF BUTTON-142 IN FRAME F_Instancias /* Button 142 */
DO:
  OPEN QUERY Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
  ENABLE ALL WITH FRAME F_Cerradas.
  VIEW FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_HojaVida /* Button 149 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_HojaVida.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_HojaVida /* Button 150 */
DO:
  ENABLE ALL WITH FRAME F_conHV.
  OPEN QUERY Br_ConHV FOR EACH Hoja_Vida WHERE
       Hoja_Vida.Tipo      EQ 9 AND Hoja_Vida.Codigo EQ 1 AND 
       Hoja_Vida.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
       Hoja_Vida.DoctoRef  EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
       Hoja_Vida.Nit       EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
       Hoja_Vida.Usuario   EQ W_Usuario 
 INDEXED-REPOSITION.
 VIEW FRAME F_ConHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-152
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-152 wWin
ON CHOOSE OF BUTTON-152 IN FRAME F_HojaVida /* Cancelar */
DO:
  DISABLE Btn_SalvaHV WITH FRAME F_HojaVida.
  APPLY "choose" TO Btn_HojaVida IN FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Agregar
&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  HIDE FRAME F_Agregar.
  IF E_Agregar:SCREEN-VALUE NE "" THEN DO:
     IF Id_Agregar EQ "HV" THEN DO:
        Hoja_Vida.Observacion:SCREEN-VALUE IN FRAME F_HojaVida = Hoja_Vida.Observacion:SCREEN-VALUE + 
        ". Fecha: " + STRING(w_fecha) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar. 
        ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
     END.    
     IF Id_Agregar EQ "IN" THEN DO:
        FIND FIRST Mov_Instancias WHERE 
                   Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                   Mov_Instancias.Nit       EQ STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
                   Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
                   Mov_Instancias.Usuario   EQ W_Usuario AND 
                   Mov_Instancias.Estado    EQ NO  NO-LOCK NO-ERROR.
           IF NOT available(mov_instancias) THEN 
             ASSIGN Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias =            
            "Fecha: " + STRING(W_Fecha) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar.
            ELSE
             ASSIGN   Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = 
            Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias + 
            ". Fecha: " + STRING(W_Fecha) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar.
          
       /* ASSIGN Mov_Instancias.Descripcion.    */  
        FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON ENTRY OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON LEAVE OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Cerradas /* Ver solo Instancia y Descripción */
DO:
  IF SELF:LABEL EQ "Ver solo Instancia y Descripción" THEN DO:
     TCerradas.Instancia:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.INom_Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Usuario:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.INom_Usuario:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.Fec_Retiro:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.Descripcion:VISIBLE IN BROWSE br_cerradas = YES.
     SELF:LABEL = "Todas las columnas".
  END.
  ELSE DO:
     TCerradas.Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.INom_Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Usuario:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.INom_Usuario:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Fec_Retiro:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Descripcion:VISIBLE IN BROWSE br_cerradas = YES.
     SELF:LABEL = "Ver solo Instancia y Descripción".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME BUTTON-155
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-155 wWin
ON CHOOSE OF BUTTON-155 IN FRAME F_Codeudores /* i */
DO:
  FIND Clientes WHERE Clientes.Nit EQ W_NitCodeudor:SCREEN-VALUE IN FRAME F_Codeudores NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN RUN Llenar_InfoCliente.
  DISABLE ALL WITH FRAME F_Creditos.
  ENABLE ALL WITH FRAME F_InfoCliente.
  VIEW FRAME F_InfoCliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-156
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-156 wWin
ON CHOOSE OF BUTTON-156 IN FRAME F_InfoCliente /* Ver Información Detallada */
DO:
  RUN W-ConsultaGeneral.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME BUTTON-161
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-161 wWin
ON CHOOSE OF BUTTON-161 IN FRAME F_Admisible /* Button 161 */
DO:
  HIDE FRAME F_Admisible.
  APPLY "choose" TO Btn_OutGarantias IN FRAME F_Garantias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Condicionada
&Scoped-define SELF-NAME BUTTON-162
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-162 wWin
ON CHOOSE OF BUTTON-162 IN FRAME F_Condicionada /* Button 162 */
DO:
  IF NOT AVAIL(Tuxi) THEN DO:
     MESSAGE "Usuario NO Asignado para Devolver a la Instancia Respectiva." SKIP
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     HIDE FRAME F_Condicionada.
  END.
  ELSE DO:
     IF E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada EQ "" THEN DO:
         MESSAGE "Debe ingresarse algún comentario o condición para la" SKIP
                 "solicitud que se pasará a otro usuario" SKIP(1)
                 "Escriba la condición a cumplir" VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO E_Condicion IN FRAME F_condicionada.
         RETURN NO-APPLY.
     END.

     HIDE FRAME F_Condicionada.
     APPLY "Choose" TO Btn_SalvaUltima IN FRAME F_Ultima.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-164
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-164 wWin
ON CHOOSE OF BUTTON-164 IN FRAME F_Creditos /* I.Económica */
DO:
    DEFI VAR W_Cuot LIKE Creditos.Cuota.
        
    IF NOT AVAIL(Solicitud) 
    THEN DO:
        MESSAGE "Debe seleccionar una solicitud. " VIEW-AS ALERT-BOX.
        RETURN.
    END.   
    
    IF NOT Instancias.Id_Scoring AND NOT Instancias.Id_Concepto THEN RETURN.
    
    APPLY "choose" TO btn_salvar.
    FIND Clientes WHERE Clientes.Nit EQ Solicitud.Nit NO-LOCK NO-ERROR. 
    IF NOT CAN-FIND(FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit)
    THEN DO:
        CREATE anexos_clientes.
        anexos_clientes.nit = clientes.nit.
    END.
    FIND FIRST anexos_clientes NO-LOCK
        WHERE
            anexos_clientes.nit = clientes.nit NO-ERROR.
    
    ASSIGN FRAME F_Creditos:SENSITIVE  = FALSE
    FRAME F_VBlesS:VISIBLE      = TRUE
    Clientes.GtoFinanc_Indir:SCREEN-VALUE IN FRAME F_VBlesS = STRING(Clientes.GtoFinanc_Indir)
    Clientes.Ing_Arriendos:SCREEN-VALUE      = STRING(Clientes.Ing_Arriendos)
    Clientes.Ing_Financieros:SCREEN-VALUE    = STRING(Clientes.Ing_Financieros)
    Clientes.Ing_Honorarios:SCREEN-VALUE     = STRING(Clientes.Ing_Honorarios)
    Clientes.Ing_Otros:SCREEN-VALUE          = STRING(Clientes.Ing_Otros)
    Clientes.Gto_obligacion:SCREEN-VALUE     = STRING(Clientes.Gto_obligacion)
    Clientes.Sdo_obligacion:SCREEN-VALUE     = STRING(Clientes.Sdo_obligacion)
    Clientes.Salario:SCREEN-VALUE            = STRING(Clientes.Salario)
    Clientes.Gto_Familiar:SCREEN-VALUE       = STRING(/*Clientes.Salario * .3*/ 0)
    Clientes.Gto_Arriendo:SCREEN-VALUE       = STRING(Clientes.Gto_Arriendo)
    Clientes.Capacidad_Pago:SCREEN-VALUE     = STRING(Clientes.Capacidad_Pago).
    
    ASSIGN  Rs_Ocupacion:SCREEN-VALUE                = STRING(Clientes.Tip_Contrato)
            Tot_Ingresos:SCREEN-VALUE = STRING(Clientes.Ing_Arriendos  + Clientes.Ing_Financieros +
            Clientes.Ing_Honorarios + Clientes.Ing_Otros       +
            Clientes.Salario)
            Tot_Egresos:SCREEN-VALUE  = STRING(Clientes.Gto_obligacion + Clientes.Salario * .3    +
            Clientes.Gto_Arriendo + Clientes.Sdo_obligacion)
            Tot_Ingresos
            Tot_Egresos
            W_LiqDispon              = Tot_Ingresos - Tot_Egresos
            W_LiqDispon:SCREEN-VALUE = STRING(Tot_Ingresos - Tot_Egresos)
            Cmb_ConCte:SENSITIVE   = TRUE
            Cmb_destino:SENSITIVE  = TRUE
            Cmb_MoraCial:SENSITIVE = TRUE
            Cmb_Gtia:SENSITIVE     = TRUE
            Nom_CteCodeu:SCREEN-VALUE = "DEUDOR : " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
            cmbEstdoCrgo:SCREEN-VALUE = anexos_clientes.estado_cargo.
    
    
    /* IF DEC(Clientes.Gto_obligacion:SCREEN-VALUE) LE 0 THEN 
    FOR EACH Creditos WHERE Creditos.Nit         EQ Clientes.Nit
    AND Creditos.Sdo_Capital GT 0 NO-LOCK:
    ASSIGN W_Cuot = W_Cuot + Creditos.Cuota
    Clientes.Gto_obligacion:SCREEN-VALUE = STRING(W_Cuot).
    END.*/
    ASSIGN  Cmb_ConCte:SCREEN-VALUE IN FRAME F_VblesS  = " "
            Cmb_destino:SCREEN-VALUE  = " "
            Cmb_MoraCial:SCREEN-VALUE = " "
            Cmb_Gtia:SCREEN-VALUE     = " "
            Cmb_ResPat:SCREEN-VALUE   = " "
            Cmb_TipAct:SCREEN-VALUE   = " "
            W_VrCuota:SCREEN-VALUE    = STRING(Solicitud.Cuota) NO-ERROR.
    
    ASSIGN  Cmb_ConCte:SCREEN-VALUE IN FRAME F_VblesS = Clientes.Conocimiento_Cliente
            Cmb_destino:SCREEN-VALUE  = Clientes.Destino        
            Cmb_MoraCial:SCREEN-VALUE = Clientes.Mora_Comercial
            Cmb_Gtia:SCREEN-VALUE     = Clientes.Garantia
            Cmb_ResPat:SCREEN-VALUE   = Clientes.Respaldo_Patrim          
            Cmb_TipAct:SCREEN-VALUE   = Clientes.Tipo_Actividad
            idelphi:SCREEN-VALUE      = string(anexos_clientes.delphi) NO-ERROR.
    DO WITH FRAME F_VblesS:
        FIND FIRST bfrAgencia NO-LOCK WHERE bfrAgencia.agencia = clientes.agencia NO-ERROR.
        clientes.gto_familiar:SCREEN-VALUE = 
            string(fRngoSlrio(
                decimal(tot_ingresos:SCREEN-VALUE IN FRAME F_VblesS),
                Clientes.Per_ACargo ,
                              bfrAgencia.ciudad)).        
    END.
    fCpcdadPgo().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME BUTTON-165
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-165 wWin
ON CHOOSE OF BUTTON-165 IN FRAME F_Codeudores /* Inf.Econòmica */
DO:
    DEFI VAR W_DestC LIKE Clientes.Destino.
    DEFI VAR W_GaraC LIKE Clientes.Garantia.
    FIND FIRST Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NO-LOCK NO-ERROR.
    
    IF NOT Instancias.Id_Scoring AND NOT Instancias.Id_Concepto THEN RETURN.
    
    IF NOT AVAIL(TCode) 
    THEN DO:
        MESSAGE "Debe seleccionar un codeudor. " VIEW-AS ALERT-BOX.
        RETURN.
    END.   
    
    FIND FIRST Clientes WHERE Clientes.Nit EQ Solicitud.Nit NO-LOCK NO-ERROR.
    ASSIGN  W_DestC = Clientes.Destino        
            W_GaraC = Clientes.Garantia.
    
    FIND Clientes WHERE Clientes.Nit EQ TCode.TC_NitCode NO-LOCK NO-ERROR. 
    IF NOT AVAIL Clientes THEN RETURN.
    IF NOT CAN-FIND(FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit)
    THEN DO:
        CREATE anexos_clientes.
        anexos_clientes.nit = clientes.nit.
    END.
    FIND FIRST anexos_clientes NO-LOCK
        WHERE
            anexos_clientes.nit = clientes.nit NO-ERROR.
    ASSIGN  FRAME F_codeudores:visible  = FALSE
            FRAME F_VBlesS:visible      = TRUE
            Clientes.GtoFinanc_Indir:SCREEN-VALUE IN FRAME F_VBlesS = STRING(Clientes.GtoFinanc_Indir)
            Clientes.Ing_Arriendos:SCREEN-VALUE      = STRING(Clientes.Ing_Arriendos)
            Clientes.Ing_Financieros:SCREEN-VALUE    = STRING(Clientes.Ing_Financieros)
            Clientes.Ing_Honorarios:SCREEN-VALUE     = STRING(Clientes.Ing_Honorarios)
            Clientes.Ing_Otros:SCREEN-VALUE          = STRING(Clientes.Ing_Otros)
            Clientes.Gto_obligacion:SCREEN-VALUE     = STRING(Clientes.Gto_obligacion)
            Clientes.Sdo_obligacion:SCREEN-VALUE     = STRING(Clientes.Sdo_obligacion)
            Clientes.Salario:SCREEN-VALUE            = STRING(Clientes.Salario)
            Clientes.Gto_Familiar:SCREEN-VALUE       = STRING(Clientes.Salario * .3)
            Clientes.Gto_Arriendo:SCREEN-VALUE       = STRING(Clientes.Gto_Arriendo)
            Clientes.Capacidad_Pago:SCREEN-VALUE     = STRING(Clientes.Capacidad_Pago).
    
    ASSIGN  Rs_Ocupacion:SCREEN-VALUE                = STRING(Clientes.Tip_Contrato)
            Tot_Ingresos:SCREEN-VALUE = STRING(Clientes.Ing_Arriendos  + Clientes.Ing_Financieros +
            Clientes.Ing_Honorarios + Clientes.Ing_Otros       +
            Clientes.Salario)
            Tot_Egresos:SCREEN-VALUE  = STRING(Clientes.Gto_obligacion + Clientes.Salario * .3    +
            Clientes.Gto_Arriendo + Clientes.Sdo_obligacion)
            Tot_Ingresos
            Tot_Egresos
            W_LiqDispon              = Tot_Ingresos - Tot_Egresos
            W_LiqDispon:SCREEN-VALUE = STRING(Tot_Ingresos - Tot_Egresos)
            Cmb_ConCte:SENSITIVE   = TRUE
            Cmb_destino:SENSITIVE  = TRUE
            Cmb_MoraCial:SENSITIVE = TRUE
            Cmb_Gtia:SENSITIVE     = TRUE
            Nom_CteCodeu:SCREEN-VALUE = "CODEUDOR : " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
            cmbEstdoCrgo:SCREEN-VALUE = anexos_clientes.estado_cargo.
    
    ASSIGN  Cmb_ConCte:SCREEN-VALUE IN FRAME F_VblesS  = " "
            Cmb_destino:SCREEN-VALUE  = " "
            Cmb_MoraCial:SCREEN-VALUE = " "            
            Cmb_Gtia:SCREEN-VALUE     = " "
            Cmb_ResPat:SCREEN-VALUE   = " "
            Cmb_TipAct:SCREEN-VALUE   = " "
            W_VrCuota:SCREEN-VALUE    = STRING(Solicitud.Cuota) NO-ERROR.
    
    ASSIGN  Cmb_ConCte:SCREEN-VALUE IN FRAME F_VblesS = Clientes.Conocimiento_Cliente
            Cmb_destino:SCREEN-VALUE  = W_DestC        
            Cmb_MoraCial:SCREEN-VALUE = Clientes.Mora_Comercial  
            Cmb_Gtia:SCREEN-VALUE     = W_GaraC
            Cmb_ResPat:SCREEN-VALUE   = Clientes.Respaldo_Patrim          
            Cmb_TipAct:SCREEN-VALUE   = Clientes.Tipo_Actividad
            idelphi:SCREEN-VALUE      = string(anexos_clientes.delphi) NO-ERROR.
    
    /*HIDE FRAME F_codeudores.
    VIEW FRAME F_VBlesS.
    APPLY "ENTRY" TO Clientes.Salario IN FRAME F_Vbless.*/
            

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME BUTTON-166
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-166 wWin
ON CHOOSE OF BUTTON-166 IN FRAME F_Scoring /* Siguiente */
DO:
  ASSIGN M_Cliente:SCREEN-VALUE = "No"
         M_Cliente
         Nom_CteCod:SCREEN-VALUE = " ".

  FOR EACH Relaciones WHERE Relaciones.Nit EQ Solicitud.Nit    AND
                 Relaciones.Clase_Producto EQ 2                AND
                 Relaciones.Cod_Relacion   EQ 11               AND
                 Relaciones.Estado         EQ 1                AND
                 Relaciones.Cod_Producto   EQ Solicitud.Cod_Credito AND
                 INTEG(Relaciones.Cuenta)  EQ Solicitud.Num_Solicitud NO-LOCK:
      RUN Prc_LlenarScoring.p (INPUT Relaciones.Nit_Relacion,
                               INPUT Solicitud.Num_Solicitud, INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME f_solicitud).

      RUN MostrarScor.

      FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-ERROR.

      ASSIGN Clientes.Puntaje = TOTAL_Puntaje
             Nom_CteCod:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " +
                                                               Clientes.Apellido2.

      FIND CURRENT Clientes NO-LOCK NO-ERROR.

      MESSAGE "Pasar al Siguiente..." VIEW-AS ALERT-BOX.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-169
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-169 wWin
ON CHOOSE OF BUTTON-169 IN FRAME F_Creditos /* Historial de Créditos */
DO:
  ASSIGN WWin:SENSITIVE = NO.
  RUN W-Hist_Creditos.r (INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,INPUT 999999999).
  ASSIGN WWin:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME BUTTON-170
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-170 wWin
ON CHOOSE OF BUTTON-170 IN FRAME F_Extras /* Button 170 */
DO:
  ASSIGN FRAME F_Extras:VISIBLE     = FALSE
         FRAME F_Creditos:SENSITIVE = TRUE.

  APPLY "Choose" TO Btn_Salvar IN FRAME F_Creditos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 wWin
ON CHOOSE OF BUTTON-19 IN FRAME F_Solicitud /* i */
DO:
  IF AVAILABLE Clientes THEN DO:
    DISABLE ALL WITH FRAME F_Creditos.
    ENABLE ALL WITH FRAME F_InfoCliente.
    RUN Llenar_InfoCliente.
    VIEW FRAME F_InfoCliente.
  END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Creditos /* Salir */
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


&Scoped-define FRAME-NAME F_Foto
&Scoped-define SELF-NAME BUTTON-230
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-230 wWin
ON CHOOSE OF BUTTON-230 IN FRAME F_Foto /* Cerrar */
DO:
  HIDE FRAME F_Foto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME VisorSolicitud
&Scoped-define SELF-NAME BUTTON-233
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-233 wWin
ON CHOOSE OF BUTTON-233 IN FRAME VisorSolicitud /* Salir */
DO:
    VIEW FRAME F_Solicitud.
    HIDE FRAME VisorSolicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME F_Creditos /* Borrar */
DO:
  IF AVAIL(Solicitud) THEN DO:
     MESSAGE "Está Seguro de Anular la Solicitud Seleccionada...?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR ANULACION" 
         UPDATE W_RpataAn AS LOGICAL.
     IF NOT W_RpataAn THEN
        RETURN.
     IF W_negadas = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) THEN DO:
        MESSAGE "Solicitud Negada, El proceso es informar al Asociado y pasar instancia"
        VIEW-AS ALERT-BOX  TITLE "No se Borra" .
        RETURN.
     END.

     /*dISABLE TRIGGERS FOR LOAD OF Solicitud.*/
     FIND CURRENT Solicitud NO-ERROR.
     /*Alexander Momçada enero 25 2005   se opto por borra el registro*/
     /*ASSIGN Solicitud.Estado     = 4
            Solicitud.Fec_Aprob  = W_Fecha
            Solicitud.Fec_Retiro = W_Fecha.*/


     FOR EACH Mov_Instancias WHERE 
         Mov_Instancias.Nit           EQ Solicitud.Nit AND
         Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud:
         DELETE Mov_Instancias.
     END.
     /*se borra la solicitud*/
     DELETE solicitud.
     MESSAGE "La solicitud fue Eliminada...El programa Finaliza y regresa al Menú."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "CHOOSE" TO BUTTON-2.
  END.
  ELSE MESSAGE "No Existe Solicitud Seleccionada para Anular...?"
         VIEW-AS ALERT-BOX.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME BUTTON-99
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-99 wWin
ON CHOOSE OF BUTTON-99 IN FRAME F_Scoring /* Button 99 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  DISABLE NomUsuario WITH FRAME F_Creditos.
  HIDE FRAME F_Scoring.  
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ForPago
&Scoped-define SELF-NAME Anexos_Clientes.Cam_Cat1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Cam_Cat1 wWin
ON VALUE-CHANGED OF Anexos_Clientes.Cam_Cat1 IN FRAME F_ForPago /* Capacidad Pago */
DO:
    DO WITH FRAME F_ForPago:
        CASE SELF:SCREEN-VALUE:
            WHEN "fcapagocaja"
            THEN DO:
                solicitud.FOR_pago:SCREEN-VALUE = "1".
                SELF:SENSITIVE = FALSE.
            END.
            WHEN " "
            THEN DO:
                solicitud.FOR_pago:SCREEN-VALUE = "3".
                SELF:SENSITIVE = FALSE.
            END.
        END CASE.
    END.
    fCpcdadPgo().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME cmbEstdoCrgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbEstdoCrgo wWin
ON VALUE-CHANGED OF cmbEstdoCrgo IN FRAME F_VblesS /* Estado Cargo */
DO:
    DO WITH FRAME F_VblesS:
        IF SELF:SCREEN-VALUE = "Otros Servidores"
        THEN do:
            ENABLE Rs_Ocupacion.
        END.
        ELSE do:
            Rs_Ocupacion:SCREEN-VALUE = "5".
            DISABLE Rs_Ocupacion.
        END.
    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON LEAVE OF Cmb_Agencias IN FRAME F_Solicitud /* Agencia */
DO:
IF W_Nuevo THEN DO:
  FIND Cfg_Instancias WHERE
       Cfg_instancias.Tipo_Instancia EQ 1 AND
       Cfg_Instancias.Agencia   EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
       Cfg_Instancias.Instancia EQ W_Primera AND
       Cfg_Instancias.Usuario   EQ W_Usuario AND
       Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_instancias THEN DO:
     MESSAGE "El usuario no tiene configurada la instancia" SKIP(1)
             Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos SKIP(1)
             "En la Agencia: " SELF:SCREEN-VALUE VIEW-AS ALERT-BOX ERROR.
     W_Nuevo = NO.
     HIDE FRAME F_Solicitud.
     APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Cmb_ConCte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ConCte wWin
ON LEAVE OF Cmb_ConCte IN FRAME F_VblesS /* Conocimiento Cliente */
DO:
  ASSIGN Cmb_ConCte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_destino wWin
ON LEAVE OF Cmb_destino IN FRAME F_VblesS /* Destino del Crédito */
DO:
  ASSIGN Cmb_Destino.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Gtia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Gtia wWin
ON LEAVE OF Cmb_Gtia IN FRAME F_VblesS /* Garantía */
DO:
  ASSIGN Cmb_Gtia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Condicionada
&Scoped-define SELF-NAME Cmb_InsCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_InsCon wWin
ON VALUE-CHANGED OF Cmb_InsCon IN FRAME F_Condicionada /* Instancias */
DO:    
  FOR EACH TUxi: DELETE Tuxi. END.
  CLOSE QUERY Br_Usuarios.

  FOR EACH Cfg_Instancias WHERE
            Cfg_Instancias.Agencia        EQ Solicitud.Agencia AND
            Cfg_Instancias.Tipo_Instancia EQ 1  AND
            Cfg_Instancias.Instancia      EQ INTEG(SUBSTRING(Cmb_InsCon:SCREEN-VALUE,1,5)) AND
            Cfg_Instancias.Estado         EQ 1 NO-LOCK:
                   
         FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario 
                         AND Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.
         IF AVAILABLE Usuarios THEN DO:
            CREATE TUxi.
            ASSIGN Tuxi.Agencia = Solicitud.Agencia
                   Tuxi.Usuario = Usuarios.Usuario
                   Tuxi.Nombre  = Usuarios.Nombre
                   Tuxi.Instanc = Cfg_Instancias.Instancia.

            FOR EACH Mov_Instancias WHERE
                  Mov_instancias.Instancia EQ Cfg_Instancias.Instancia AND
                  Mov_Instancias.Usuario   EQ Usuarios.Usuario AND
                  Mov_Instancias.Estado    EQ NO NO-LOCK:
                Tuxi.Cantidad = Tuxi.Cantidad + 1.
            END.

            FOR EACH Mov_Instancias WHERE 
                  Mov_Instancias.Instancia EQ Cfg_Instancias.Instancia AND
                  Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-LOCK:
                  FIND Tuxi WHERE TUxi.Usuario EQ Mov_Instancias.Usuario NO-ERROR.
                  IF AVAILABLE Tuxi THEN 
                     Tuxi.Proceso = YES.
            END.
         END.      
  END.

  OPEN QUERY Br_Usuarios FOR EACH Tuxi NO-LOCK INDEXED-REPOSITION.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME F_Creditos /* Instancias */
DO:
    ENABLE Btn_Salvar WITH FRAME F_Creditos.

    FIND FIRST Instancias WHERE Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN DO:
        ASSIGN Orden_InsPan = Instancias.Orden
               W_VigIns = Instancias.TMI
               BUTTON-100:SENSITIVE IN FRAME F_Solicitud = FALSE
               BUTTON-164:SENSITIVE = FALSE
               VG_Normal:SCREEN-VALUE IN FRAME F_Consulta = "Prioridad Normal hasta: " + STRING(Instancias.TMI / 2) + " Dias"
               VG_Media:SCREEN-VALUE IN FRAME F_Consulta = "Prioridad Media desde: " + STRING(Instancias.TMI / 2) + " Hasta : " + STRING(Instancias.TMI) + " Días"
               VG_Alta:SCREEN-VALUE IN FRAME F_Consulta = "Prioridad Alta desde: " + STRING(Instancias.TMI + 1) + " Días".

        IF Instancias.Instancia = W_Primera THEN
            ENABLE Btn_Ingresar WITH FRAME F_Creditos.
        ELSE
            DISABLE Btn_Ingresar WITH FRAME F_Creditos.

        IF Instancias.Id_Scoring OR Instancias.Id_Concepto THEN
            ASSIGN BUTTON-100:SENSITIVE IN FRAME F_Solicitud = TRUE
                   BUTTON-164:SENSITIVE = TRUE.
    END.

    RUN Solicitudes_X_Instancia.
    HIDE FRAME F_Solicitud.
    VIEW FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Cmb_MoraCial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_MoraCial wWin
ON LEAVE OF Cmb_MoraCial IN FRAME F_VblesS /* Histórico Pagos Externo Sin DELPHI */
DO:
  ASSIGN Cmb_MoraCial.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_MoraCial wWin
ON VALUE-CHANGED OF Cmb_MoraCial IN FRAME F_VblesS /* Histórico Pagos Externo Sin DELPHI */
DO:
    IF NOT SELF:SCREEN-VALUE = ?
    THEN do:
        iDelphi = 0.
        iDelphi:SCREEN-VALUE = "0".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_PerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerPago wWin
ON VALUE-CHANGED OF Cmb_PerPago IN FRAME F_Solicitud /* Período */
DO:
    DO WITH FRAME F_Solicitud:
        Solicitud.Cuota:SCREEN-VALUE = "0".

        ENABLE Btn_Liquidar.

        IF Solicitud.Plazo:SCREEN-VALUE NE "0" THEN DO:
            CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
                WHEN 0 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 1))
                           W_NomPdo:SCREEN-VALUE = "Cuota Diaria".

                WHEN 1 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 7))
                           W_NomPdo:SCREEN-VALUE = "Cuota Semanal".

                WHEN 2 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 10))
                           W_NomPdo:SCREEN-VALUE = "Cuota Decadal".

                WHEN 3 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 15))
                           W_NomPdo:SCREEN-VALUE = "Cuota Quincenal".

                WHEN 4 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 30))
                           W_NomPdo:SCREEN-VALUE = "Cuota Mensual".

                WHEN 5 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 60))
                           W_NomPdo:SCREEN-VALUE = "Cuota Bimensual".

                WHEN 6 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 90))
                           W_NomPdo:SCREEN-VALUE = "Cuota Trimestral".

                WHEN 7 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 120))
                           W_NomPdo:SCREEN-VALUE = "Cuota Cuatrimestral".

                WHEN 8 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 180))
                           W_NomPdo:SCREEN-VALUE = "Cuota Semestral".

                WHEN 9 THEN
                    ASSIGN Solicitud.Plazo:SCREEN-VALUE = STRING(ABS(Dias / 360))
                           W_NomPdo:SCREEN-VALUE = "Cuota Anual".
            END CASE.
        END.

        vPeriodicidad = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)).

        APPLY "leave" TO Solicitud.Plazo.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Producto /* Producto */
DO:
  FOR EACH TDeducc: DELETE TDeducc. END.
  
  ASSIGN Solicitud.Deducible:SCREEN-VALUE IN FRAME F_Solicitud = "0"
         Solicitud.TOTAL_Prestamo:SCREEN-VALUE = "0".
  
  
DO WITH FRAME F_Producto:

  IF SELF:SCREEN-VALUE NE "" THEN DO:
     FIND Pro_Creditos WHERE 
          Pro_Creditos.Tip_Credito  EQ INTEGER(Solicitud.Tip_Credito:SCREEN-VALUE) AND
          Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN DO:
        ASSIGN codCreditoActivo = pro_creditos.cod_credito.
        RUN Llenar_InfoProducto.
        FOR EACH Tdeducc: DELETE Tdeducc. END.
        DO i = 1 TO 10 BY 1:
           IF Pro_Creditos.Deducible[i] NE "" THEN DO:
              /*MESSAGE Pro_creditos.deducible[i].*/
              FIND Deducible WHERE Deducible.Cod_Deducible EQ Pro_Creditos.Deducible[i] NO-LOCK NO-ERROR.
              IF AVAILABLE Deducible THEN DO:
                 FIND TDeducc WHERE TDeducc.Cod_Deducible = Deducible.Cod_Deducible NO-ERROR.
                 IF AVAIL TDeducc THEN DELETE TDeducc.
                 CREATE TDeducc.
                 BUFFER-COPY Deducible TO TDeducc.                 
              END.
           END.
        END.
     END.
  END.
END.

FIND FIRST creditos WHERE creditos.nit EQ solicitud.nit:SCREEN-VALUE IN FRAME F_Solicitud AND creditos.cod_credito EQ codCreditoActivo AND
                            creditos.estado EQ 2 NO-LOCK NO-ERROR.
IF AVAILABLE creditos THEN
    MESSAGE "Cliente ya tiene un producto activo en esta linea." SKIP
            "Proceder a analizar antes de continuar."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

APPLY "CHOOSE" TO Btn_OutProductos.

IF DEC(Solicitud.Monto:SCREEN-VALUE IN FRAME F_Solicitud) GT 0 THEN
   APPLY "LEAVE" TO Solicitud.Monto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Cmb_ResPat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_ResPat wWin
ON LEAVE OF Cmb_ResPat IN FRAME F_VblesS /* Respaldo Patrimonial */
DO:
  ASSIGN Cmb_ConCte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Cmb_Sistemas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Sistemas wWin
ON VALUE-CHANGED OF Cmb_Sistemas IN FRAME F_Solicitud /* Sistema */
DO:
    DO WITH FRAME F_Solicitud:
        Solicitud.Cuota:SCREEN-VALUE IN FRAME F_Solicitud = "0".

        ENABLE Btn_Liquidar WITH FRAME F_Solicitud.

        DEFINE VARIABLE Sistema AS INTEGER FORMAT "99999".

        Sistema = INTEGER(SUBSTRING(Cmb_sistemas:SCREEN-VALUE,1,5)).

        APPLY "leave" TO Solicitud.Plazo IN FRAME F_Solicitud.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Cmb_TipAct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipAct wWin
ON LEAVE OF Cmb_TipAct IN FRAME F_VblesS /* Tipo de Actividad */
DO:
  ASSIGN Cmb_TipAct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
IF T_Refresh:SCREEN-VALUE IN FRAME F_Creditos EQ "YES" 
    THEN DO:
        IF FRAME F_Consulta:HIDDEN EQ NO 
        THEN APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
    END. 
    /*IF FRAME F_Consulta:HIDDEN EQ NO THEN
        APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Descripcion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien wWin
ON VALUE-CHANGED OF Garantias.Descripcion_Bien IN FRAME F_Admisible /* Descripcion_Bien */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Descripcion_Bien2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Descripcion_Bien2 wWin
ON VALUE-CHANGED OF Garantias.Descripcion_Bien2 IN FRAME F_Admisible /* Descripcion_Bien2 */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Desembolso
&Scoped-define SELF-NAME Solicitud.Desembolso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Desembolso wWin
ON VALUE-CHANGED OF Solicitud.Desembolso IN FRAME F_Desembolso /* Tipo de Desembolso */
DO:
DO WITH FRAME F_Desembolso:
  IF SELF:SCREEN-VALUE EQ "3" THEN DO:
     RUN C-Ahorros.r (INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud, OUTPUT A_Age, OUTPUT A_Pro, OUTPUT A_NitW, OUTPUT A_Cue).
     FIND Ahorros WHERE Ahorros.Agencia      EQ A_Age  AND
                        Ahorros.Cod_Ahorro   EQ A_Pro  AND
                        Ahorros.Nit          EQ A_NitW AND
                        Ahorros.Cue_Ahorros  EQ A_Cue NO-LOCK NO-ERROR.
     IF AVAILABLE Ahorros THEN DO:
        FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ A_Pro NO-LOCK NO-ERROR.
        IF Pro_Ahorros.Tip_Ahorro NE 1 THEN DO:
           MESSAGE "La cuenta seleccionada no es de tipo" SKIP
                   "A la Vista. Escoja una cuenta de este" SKIP
                   "Tipo para hacer el débito automático!" VIEW-AS ALERT-BOX WARNING.
           SELF:SCREEN-VALUE = "1".
        END.
        ELSE DO:
           ASSIGN Solicitud.Age_Desembolso:SCREEN-VALUE = STRING(A_Age)
                  Solicitud.Cod_Desembolso:SCREEN-VALUE = STRING(A_Pro)
                  Solicitud.Cue_Desembolso:SCREEN-VALUE = A_Cue.
        END.
     END.
  END.
  ELSE
     ASSIGN Solicitud.Age_Desembolso:SCREEN-VALUE = "0"
            Solicitud.Cod_Desembolso:SCREEN-VALUE = "0"
            Solicitud.Cue_Desembolso:SCREEN-VALUE = "".
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Solicitud.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Estado wWin
ON MOUSE-SELECT-CLICK OF Solicitud.Estado IN FRAME F_Ultima /* Estado */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Estado wWin
ON VALUE-CHANGED OF Solicitud.Estado IN FRAME F_Ultima /* Estado */
DO:
  /*HIDE FRAME F_Condicionada.*/
  DISABLE Cmb_Negadas WITH FRAME F_Ultima.
  Cmb_Negadas:HIDDEN IN FRAME F_Ultima = YES.
  
  CASE SELF:SCREEN-VALUE: 
    WHEN "1" THEN 
      W_MenDes:SCREEN-VALUE = "La Solicitud se encuentra en estudio por el usuario.".
    WHEN "2" THEN
      W_MenDes:SCREEN-VALUE = "La Solicitud pasara a ser un Credito sin desembolsar.".
    WHEN "3" THEN DO:
        W_MenDes:SCREEN-VALUE = "La Solicitud será negada y no podra cambirse su estado".
        Cmb_Negadas:HIDDEN IN FRAME F_Ultima = NO.
        ENABLE Cmb_Negadas WITH FRAME F_Ultima.
    END.
    WHEN "4" THEN DO:
      W_MenDes:SCREEN-VALUE = "La Solicitud será enviada a una Instancia Anterior, con una condición a cumplir".
      
      /*07/01/2005   Se llama una funcion donse se consultan los usuarios de la etapas anteriores.*/
      RUN Usuarios_X_Instancia.
      
      FIND FIRST Tuxi NO-ERROR.
      IF NOT AVAILABLE Tuxi THEN DO:
         MESSAGE "No existen usuarios disponibles en otras instancias" SKIP
                 "El usuario actual deberá verificar por si mismo" SKIP
                 "que las condiciones faltantes sean cumplidas" VIEW-AS ALERT-BOX INFORMATION.
      END.
      ELSE DO:
        VIEW FRAME F_Ultima.
        VIEW FRAME F_Condicionada.
        ENABLE ALL WITH FRAME F_Condicionada.
        ASSIGN E_Condicion:SCREEN-VALUE = ""
               Cmb_InsCon:SCREEN-VALUE  = Cmb_InsCon:ENTRY(1).
               
        APPLY "Value-Changed" TO Cmb_InsCon.
        APPLY "Entry" TO E_Condicion IN FRAME F_Condicionada.
        RETURN NO-APPLY.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Mov_Instancias.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_Instancias.Estado wWin
ON VALUE-CHANGED OF Mov_Instancias.Estado IN FRAME F_Instancias /* Cerrar Instancia */
OR ANY-KEY OF SELF DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
    APPLY "choose" TO Btn_AgregarTXT IN FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.fec_desembolso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.fec_desembolso wWin
ON LEAVE OF Solicitud.fec_desembolso IN FRAME F_Solicitud /* Fecha Desembolso */
DO:
    IF solicitud.fec_desembolso:SCREEN-VALUE = '' OR DATE(solicitud.fec_desembolso:SCREEN-VALUE) < w_fecha THEN DO:
        MESSAGE "La fecha de desembolso debe ser igual o posterior"
                "al día de hoy. Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        solicitud.fec_desembolso:SCREEN-VALUE = ''.
        APPLY 'entry' TO solicitud.fec_desembolso.
        RETURN NO-APPLY.
    END.

    RUN liquidar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Fec_FinSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON LEAVE OF Garantias.Fec_FinSeguro IN FRAME F_Admisible /* Fecha Vencimiento */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT DATE(Garantias.Fec_IniSeguro:SCREEN-VALUE) THEN DO:
     MESSAGE "La fecha de Vencimiento de Seguro no puede ser menor" SKIP
             "a la de inicio del seguro." VIEW-AS ALERT-BOX WARNING.
     SELF:SCREEN-VALUE = STRING(DATE(Garantias.Fec_Iniseguro:SCREEN-VALUE) + 365,"99/99/9999").
     APPLY "entry" TO Garantias.Fec_FinSeguro.
     RETURN NO-APPLY.
  END.

  IF DATE(SELF:SCREEN-VALUE) LT W_Fecha THEN DO:
     MESSAGE "La fecha de Vencimiento del Seguro no puede estar vencida" SKIP
             VIEW-AS ALERT-BOX ERROR.     
     APPLY "entry" TO Garantias.Fec_FinSeguro.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_FinSeguro wWin
ON VALUE-CHANGED OF Garantias.Fec_FinSeguro IN FRAME F_Admisible /* Fecha Vencimiento */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_IniSeguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_IniSeguro wWin
ON LEAVE OF Garantias.Fec_IniSeguro IN FRAME F_Admisible /* Fecha Inicio Seguro */
DO:
  IF DATE(SELF:SCREEN-VALUE) NE ? THEN
    Garantias.Fec_FinSeguro:SCREEN-VALUE = STRING(DATE(SELF:SCREEN-VALUE) + 365).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_IniSeguro wWin
ON VALUE-CHANGED OF Garantias.Fec_IniSeguro IN FRAME F_Admisible /* Fecha Inicio Seguro */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.fec_primerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.fec_primerPago wWin
ON LEAVE OF Solicitud.fec_primerPago IN FRAME F_Solicitud /* Fecha primer pago */
DO:
    IF solicitud.fec_primerPago:SCREEN-VALUE = '' OR DATE(solicitud.fec_primerPago:SCREEN-VALUE) <= w_fecha THEN DO:
        MESSAGE "La fecha de primer pago debe ser posterior"
                "al día de hoy. Revise por favor."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        solicitud.fec_primerPago:SCREEN-VALUE = ''.
        APPLY 'entry' TO solicitud.fec_primerPago.
        RETURN NO-APPLY.
    END.

    IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)) <> 2 AND
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 108 AND
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 113 AND
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 114 THEN DO:
        IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 62 AND DAY(DATE(solicitud.fec_primerPago:SCREEN-VALUE)) <> 10 THEN DO:
            MESSAGE "Por políticas del Fondo, las fechas de pago para" SKIP
                    "esta línea de crédito serán los días 10." SKIP
                    "No se permite esta fecha."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            solicitud.fec_primerPago:SCREEN-VALUE = "".
            APPLY 'entry' TO solicitud.fec_PrimerPago.
            RETURN NO-APPLY.
        END.

        IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) = 62 AND DAY(DATE(solicitud.fec_primerPago:SCREEN-VALUE) + 1) <> 1 THEN DO:
            MESSAGE "Por políticas del Fondo, las fechas de pago para esta"
                    "línea de crédito serán para los últimos días de mes." SKIP
                    "No se permite esta fecha."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            solicitud.fec_primerPago:SCREEN-VALUE = "".
            APPLY 'entry' TO solicitud.fec_PrimerPago.
            RETURN NO-APPLY.
        END.
    END.


    RUN liquidar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Fec_ProxAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON LEAVE OF Garantias.Fec_ProxAvaluo IN FRAME F_Admisible /* Fec.Próx.Avaluo */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) THEN DO:
     MESSAGE "La fecha de Pròximo Avaluo no puede ser menor" SKIP
             "a la del último avaluo." VIEW-AS ALERT-BOX WARNING.
     SELF:SCREEN-VALUE = STRING(DATE(Garantias.Fec_UltAvaluo:SCREEN-VALUE) + 365).
     APPLY "entry" TO Garantias.Fec_ProxAvaluo.
     RETURN NO-APPLY.
  END.

  IF DATE(SELF:SCREEN-VALUE) LT W_Fecha THEN DO:
     MESSAGE "La fecha de Pròximo Avaluo debe ser Futura..." SKIP
              VIEW-AS ALERT-BOX WARNING.
     APPLY "entry" TO Garantias.Fec_ProxAvaluo.
     /*RETURN NO-APPLY.*/
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_ProxAvaluo wWin
ON VALUE-CHANGED OF Garantias.Fec_ProxAvaluo IN FRAME F_Admisible /* Fec.Próx.Avaluo */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_UltAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_UltAvaluo wWin
ON LEAVE OF Garantias.Fec_UltAvaluo IN FRAME F_Admisible /* Fec.Últ.Avaluo */
DO:
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La fecha de Ult-Avaluo no puede futura" SKIP
             VIEW-AS ALERT-BOX WARNING.
     APPLY "entry" TO Garantias.Fec_UltAvaluo.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_UltAvaluo wWin
ON VALUE-CHANGED OF Garantias.Fec_UltAvaluo IN FRAME F_Admisible /* Fec.Últ.Avaluo */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Fec_VctoImpuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_VctoImpuesto wWin
ON LEAVE OF Garantias.Fec_VctoImpuesto IN FRAME F_Admisible /* Fec.Vencimiento */
DO:
  IF DATE(SELF:SCREEN-VALUE) LT (W_Fecha - 90) THEN DO:
     MESSAGE "La fecha de Vencimiento del Impuesto tiene màs de 90 dìas..." SKIP
             VIEW-AS ALERT-BOX ERROR.     
     APPLY "entry" TO SELF.
     /*RETURN NO-APPLY.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Fec_VctoImpuesto wWin
ON VALUE-CHANGED OF Garantias.Fec_VctoImpuesto IN FRAME F_Admisible /* Fec.Vencimiento */
DO:
   ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.For_Interes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.For_Interes wWin
ON VALUE-CHANGED OF Solicitud.For_Interes IN FRAME F_Solicitud /* Interes V/A */
DO:
  ENABLE Btn_Liquidar WITH FRAME F_Solicitud.
  Solicitud.Cuota:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ForPago
&Scoped-define SELF-NAME Solicitud.For_Pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.For_Pago wWin
ON MOUSE-SELECT-CLICK OF Solicitud.For_Pago IN FRAME F_ForPago /* Forma de Pago N/C/D */
DO:

    DO WITH FRAME F_ForPago:
        FIND CURRENT solicitud EXCLUSIVE-LOCK NO-ERROR.
        IF LOCKED solicitud
        THEN DO:
            MESSAGE "Registro De Solicitud En Uso Por Otro Usuario." SKIP
                    "Por Favor Intente Más Tarde."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        ASSIGN solicitud.FOR_pago.
        FIND CURRENT solicitud NO-LOCK NO-ERROR.
        FIND CURRENT anexos_clientes EXCLUSIVE-LOCK NO-ERROR.
        IF LOCKED(anexos_clientes)
        THEN DO:
            MESSAGE "Registro De Anexos Clientes En Uso Por Otro Usuario." SKIP
                    "Por Favor Intente Más Tarde."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        CASE SELF:SCREEN-VALUE:
            WHEN "1"
            THEN DO:
                anexos_clientes.cam_cat1 = "fcapagocaja".
                anexos_clientes.cam_cat1:SCREEN-VALUE = "fcapagocaja".
                anexos_clientes.cam_cat1:SENSITIVE = FALSE.
            END.
            WHEN "2"
            THEN DO:
                anexos_clientes.cam_cat1 = " ".
                anexos_clientes.cam_cat1:SCREEN-VALUE = " ".
                    /*HAROLD
                anexos_clientes.cam_cat1 = "f50%".
                anexos_clientes.cam_cat1:SCREEN-VALUE = "f50%".*/
                anexos_clientes.cam_cat1:SENSITIVE = TRUE.
                   
            END.
            WHEN "3"
            THEN DO:
                anexos_clientes.cam_cat1 = " ".
                anexos_clientes.cam_cat1:SCREEN-VALUE = " ".
                anexos_clientes.cam_cat1:SENSITIVE = FALSE.
            END.
        END.
    END.
    fCpcdadPgo().
    FIND CURRENT anexos_clientes NO-LOCK NO-ERROR.
    DO WITH FRAME F_Solicitud:
        IF SELF:SCREEN-VALUE EQ "3" 
        THEN DO:
            RUN C-Ahorros.r (INPUT Solicitud.Nit:SCREEN-VALUE, OUTPUT A_Age, OUTPUT A_Pro, OUTPUT A_NitW, OUTPUT A_Cue).
            FIND Ahorros 
                WHERE 
                    Ahorros.Agencia      EQ A_Age  
                AND Ahorros.Cod_Ahorro EQ A_Pro  
                AND Ahorros.Nit          EQ A_NitW 
                AND Ahorros.Cue_Ahorros  EQ A_Cue NO-LOCK NO-ERROR.
            IF AVAILABLE Ahorros 
            THEN DO:
                FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ A_Pro NO-LOCK NO-ERROR.
                IF Pro_Ahorros.Tip_Ahorro    NE 1 OR NOT Pro_Ahorros.Id_Debito   
                THEN DO:
                    MESSAGE "La cuenta seleccionada no es de tipo" SKIP
                            "A la Vista, O no permite Deb/Automàtico. Escoja una cuenta de este" SKIP
                            "Tipo para hacer el débito automático!" VIEW-AS ALERT-BOX WARNING.
                    SELF:SCREEN-VALUE = "1".
                END.
                ELSE DO:
                    ASSIGN  Solicitud.Age_DebAutomatico:SCREEN-VALUE = STRING(A_Age)
                            Solicitud.Cod_DebAutomatico:SCREEN-VALUE = STRING(A_Pro)
                            Solicitud.Cue_DebAutomatico:SCREEN-VALUE = A_Cue.
                    FIND Agencias WHERE Agencias.Agencia EQ A_Age NO-LOCK NO-ERROR.
                    W_NomAgeDebAutomatico:SCREEN-VALUE = Agencias.Nombre.
                    FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ A_Pro NO-LOCK NO-ERROR.
                    W_NomCodDebAutomatico:SCREEN-VALUE = Pro_Ahorros.Nom_producto.
                END.
            END.
        END.
        ELSE DO:
            ASSIGN  Solicitud.Age_DebAutomatico:SCREEN-VALUE = "0"
                    Solicitud.Cod_DebAutomatico:SCREEN-VALUE = "0"
                    Solicitud.Cue_DebAutomatico:SCREEN-VALUE = "".
        END.
        IF SELF:SCREEN-VALUE IN FRAME F_PerPago EQ "2" 
        THEN DO:
            IF Wk_PerPagEmp EQ 0 
            THEN DO:
                MESSAGE "La Solicitud no puede ser por nómina ya" SKIP
                        "el cliente no se encuentra matriculado a" SKIP
                        "ninguna empresa." SKIP(1)
                        "Se cambia la forma de pago a CAJA" VIEW-AS ALERT-BOX ERROR.
                Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "1".
                Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(5).
                /*DISABLE Cmb_PerPago WITH FRAME F_Solicitud.*/
            END.
            ELSE DO:
                Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(Wk_PerPagEmp).
                /*DISABLE Cmb_PerPago WITH FRAME F_Solicitud.*/
            END.
        END.
        ELSE DO:
            Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(5).
          /*  DISABLE Cmb_PerPago WITH FRAME F_Solicitud.*/
        END.

        APPLY "Value-Changed" TO Cmb_PerPago.
        
        RUN Hallar_Tasa_Nominal.
        RUN Hallar_Tasa_Periodo.       
    END.

    /* MODIFICADO 05/11/2008 WILLIAM MARTINEZ RUIZ - CAMBIA LA TASA SEGUN FORMA DE PAGO */
    APPLY "LEAVE" TO Solicitud.Monto IN FRAME F_Solicitud.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Clientes.GtoFinanc_Indir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.GtoFinanc_Indir wWin
ON LEAVE OF Clientes.GtoFinanc_Indir IN FRAME F_VblesS /* Centrales Riesgo */
DO:
        /*
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Gto_Arriendo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_Arriendo wWin
ON LEAVE OF Clientes.Gto_Arriendo IN FRAME F_VblesS /* Arrendamientos */
DO:
        /*
  ASSIGN Tot_Egresos = DEC(Clientes.Gto_obligacion:SCREEN-VALUE) + DEC(Clientes.Gto_Familiar:SCREEN-VALUE) + 
                       DEC(Clientes.Gto_Arriendo:SCREEN-VALUE)   + DEC(Clientes.Sdo_obligacion:SCREEN-VALUE)
         Tot_Egresos:SCREEN-VALUE  = STRING(Tot_Egresos).
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Gto_Familiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_Familiar wWin
ON LEAVE OF Clientes.Gto_Familiar IN FRAME F_VblesS /* Gtos Sostenimiento */
DO:
        /*
  ASSIGN Tot_Egresos = DEC(Clientes.Gto_obligacion:SCREEN-VALUE) + DEC(Clientes.Gto_Familiar:SCREEN-VALUE) + 
                       DEC(Clientes.Gto_Arriendo:SCREEN-VALUE)   + DEC(Clientes.Sdo_obligacion:SCREEN-VALUE)
         Tot_Egresos:SCREEN-VALUE  = STRING(Tot_Egresos).
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Gto_obligacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Gto_obligacion wWin
ON LEAVE OF Clientes.Gto_obligacion IN FRAME F_VblesS /* Deducciones Colilla */
DO:
        /*
  ASSIGN Tot_Egresos = DEC(Clientes.Gto_obligacion:SCREEN-VALUE) + DEC(Clientes.Gto_Familiar:SCREEN-VALUE) + 
                       DEC(Clientes.Gto_Arriendo:SCREEN-VALUE)   + DEC(Clientes.Sdo_obligacion:SCREEN-VALUE)
         Tot_Egresos:SCREEN-VALUE  = STRING(Tot_Egresos).
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iDelphi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iDelphi wWin
ON ENTRY OF iDelphi IN FRAME F_VblesS /* Delphi */
DO:
    IF NOT Cmb_MoraCial:SCREEN-VALUE = ?
    THEN DO:
        MESSAGE "Para Ingresar Datos En Esta Casilla, Seleccione BLANCO En La Casilla '"
                Cmb_MoraCial:LABEL "'"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO Cmb_MoraCial.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Identificacion_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Identificacion_Bien wWin
ON LEAVE OF Garantias.Identificacion_Bien IN FRAME F_Admisible /* Identificación del Bien */
DO:
  DEFI VAR W_RowIdGar  AS ROWID.
  IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "5"  THEN DO:
      garantias.nom_bien:SCREEN-VALUE = "SEGURO DEUDOR".
  END.
  IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4"  THEN DO:
     MESSAGE "Seleccione El Cdat/Contractual de Garantia."  
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
     FRAME F_Creditos:SENSITIVE = FALSE.
     RUN C-Ahorros.r (INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud, OUTPUT A_Age, OUTPUT A_Pro, OUTPUT A_NitW, OUTPUT A_Cue).
     FIND Ahorros WHERE Ahorros.Agencia      EQ A_Age  AND
                        Ahorros.Cod_Ahorro   EQ A_Pro  AND
                        Ahorros.Nit          EQ A_NitW AND
                        Ahorros.Cue_Ahorros  EQ A_Cue  AND
                        Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje GT 0 NO-LOCK NO-ERROR.
     FRAME F_Creditos:SENSITIVE = TRUE.

     IF (AVAILABLE(Ahorros) AND (Ahorros.Tip_Ahorro EQ 1 OR Ahorros.Tip_Ahorro EQ 4))
     OR NOT AVAILABLE(Ahorros) THEN DO:
        MESSAGE "La cuenta seleccionada Debe ser Cdat/Contractual y Activa," SKIP
                "Escoja una cuenta de este Tipo para la Garantia!" VIEW-AS ALERT-BOX WARNING.
        ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = ""
               Garantias.Val_Bien:SCREEN-VALUE            = "0".
     END.
     ELSE DO:
        ASSIGN Garantias.Identificacion_Bien:SCREEN-VALUE = Ahorros.Cue_Ahorros
               Garantias.Val_Bien:SCREEN-VALUE            = STRING(Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
               Garantias.Nom_Bien:SCREEN-VALUE = "Cod.Ahorro " + STRING(Ahorros.Cod_Ahorro,"99") +
                                                 " del Asociado con Ced/Nit : " + A_NitW
               Garantias.Fec_FinSeguro:SCREEN-VALUE       = STRING(Ahorros.Fec_Vencimiento)
               Garantias.Nit_Aseguradora:SCREEN-VALUE     = Ahorros.Nit. 
        APPLY "Entry" TO Garantias.Val_Bien.
        RETURN NO-APPLY.
     END.
  END.

  ASSIGN W_NBien    = Garantias.Identificacion_Bien:SCREEN-VALUE.
  ASSIGN W_RowIdGar = ROWID(Garantias) WHEN AVAIL(Garantias).

  RUN Halla_DispGaran.

  FIND Garantias WHERE ROWID(Garantias) EQ W_RowidGar NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Clientes.Ing_arriendos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_arriendos wWin
ON LEAVE OF Clientes.Ing_arriendos IN FRAME F_VblesS /* Arriendos */
DO:
/*        
  ASSIGN Tot_Ingresos = DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +   
                        DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) + DEC(Clientes.Ing_Otros:SCREEN-VALUE)       + 
                        DEC(Clientes.Salario:SCREEN-VALUE)
         Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos).  
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Ing_financieros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_financieros wWin
ON LEAVE OF Clientes.Ing_financieros IN FRAME F_VblesS /* Financieros */
DO:
        /*
  ASSIGN Tot_Ingresos = DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +   
                        DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) + DEC(Clientes.Ing_Otros:SCREEN-VALUE)       + 
                        DEC(Clientes.Salario:SCREEN-VALUE)
         Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos).
  RUN Halla_CapPago. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Ing_Honorarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_Honorarios wWin
ON LEAVE OF Clientes.Ing_Honorarios IN FRAME F_VblesS /* Honorarios */
DO:
        /*
  ASSIGN Tot_Ingresos = DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +   
                        DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) + DEC(Clientes.Ing_Otros:SCREEN-VALUE)       + 
                        DEC(Clientes.Salario:SCREEN-VALUE)
         Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos).
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Ing_Otros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Ing_Otros wWin
ON LEAVE OF Clientes.Ing_Otros IN FRAME F_VblesS /* Otros Ingresos */
DO:
        /*
  ASSIGN Tot_Ingresos = DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +   
                        DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) + DEC(Clientes.Ing_Otros:SCREEN-VALUE)       + 
                        DEC(Clientes.Salario:SCREEN-VALUE)
         Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos). 
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Monto wWin
ON LEAVE OF Solicitud.Monto IN FRAME F_Solicitud /* Monto solicitud */
DO:
    DEFINE BUFFER bfrAhorros FOR ahorros.
    DEFINE BUFFER bfrCreditos FOR creditos.
    DEFINE VAR valorMaximoPermanencia AS DECIMAL.

    DO WITH FRAME F_Solicitud:
        FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                                    AND Cfg_instancias.Tipo_Instancia = 1
                                    AND Cfg_Instancias.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                    AND Cfg_Instancias.Usuario = W_Usuario
                                    AND Cfg_Instancias.Estado = 1 USE-INDEX Idx_CFG_Instancias NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cfg_instancias THEN DO:
            MESSAGE "El usuario no tiene configurada la instancia" SKIP(1)
                    Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos SKIP(1)
                    "En la Agencia:" Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud
                VIEW-AS ALERT-BOX ERROR.
                
            APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
        END.

        IF DECIMAL(SELF:SCREEN-VALUE) > Cfg_Instancias.Monto_Maximo THEN DO:
            MESSAGE "El monto a prestar se sale del limite mayor de créditos" SKIP
                    "que usted tiene permitido radicar, el cual es:" STRING(Cfg_Instancias.Monto_Maximo,"$>>>,>>>,>>9") SKIP(1)
                VIEW-AS ALERT-BOX WARNING.

            ENABLE Btn_Liquidar.
        END.

        IF DECIMAL(SELF:SCREEN-VALUE) < Cfg_Instancias.Monto_Minimo THEN DO:
            MESSAGE "El monto a prestar se sale del limite menor de créditos" SKIP
                    "que usted tiene permitido radicar, el cual es:" STRING(Cfg_Instancias.Monto_Minimo,"$>>>,>>>,>>9") SKIP(1)
                VIEW-AS ALERT-BOX WARNING.

            ENABLE Btn_Liquidar.
        END.

        IF Nom_Producto:SCREEN-VALUE = "" THEN DO:
            MESSAGE "Para poder saber el valor a deducir" SKIP
                    "se debe escoger el producto de crédito" SKIP
                    "para la solicitud. rectifique!"
                VIEW-AS ALERT-BOX WARNING.

            Solicitud.Monto:SCREEN-VALUE = "0".
            VIEW FRAME F_Producto.
            APPLY "entry" TO Cmb_Productos IN FRAME F_Producto.
            RETURN NO-APPLY.
        END.

        IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) = 187 THEN DO:
            FOR EACH bfrAhorros WHERE bfrAhorros.nit = solicitud.nit:SCREEN-VALUE
                                  AND (bfrAhorros.tip_ahorro = 2 OR bfrAhorros.tip_ahorro = 4)
                                  AND bfrAhorros.estado = 1 NO-LOCK:
                valorMaximoPermanencia = valorMaximoPermanencia + bfrAhorros.sdo_disponible.
            END.

            FOR EACH bfrCreditos WHERE bfrCreditos.nit = solicitud.nit:SCREEN-VALUE
                                   AND bfrCreditos.estado = 2 NO-LOCK:
                valorMaximoPermanencia = valorMaximoPermanencia - bfrCreditos.sdo_capital.
            END.

            IF valorMaximoPermanencia < 0 THEN
                valorMaximoPermanencia = 0.

            IF valorMaximoPermanencia < DECIMAL(solicitud.monto:SCREEN-VALUE) THEN DO:
                MESSAGE "El valor máximo aprobado para este asociado en" SKIP
                        "esta línea de crédito es de" string(valorMaximoPermanencia,"$>>>,>>>,>>9,99")
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                solicitud.monto:SCREEN-VALUE = STRING(valorMaximoPermanencia).
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Monto wWin
ON VALUE-CHANGED OF Solicitud.Monto IN FRAME F_Solicitud /* Monto solicitud */
DO:
  Solicitud.Cuota:SCREEN-VALUE = "0".
  ENABLE Btn_Liquidar WITH FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Scoring
&Scoped-define SELF-NAME M_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL M_Cliente wWin
ON MOUSE-SELECT-CLICK OF M_Cliente IN FRAME F_Scoring /* Del Cliente */
DO: 
   ASSIGN M_Cliente.

   IF M_Cliente THEN DO:
      CLOSE QUERY Br_Scoring.
      RUN Proceso_Scoring.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Nit wWin
ON LEAVE OF Solicitud.Nit IN FRAME F_Solicitud /* Cédula/Nit */
DO:
    ASSIGN Fec_Asociado
           WAntiguedad:SCREEN-VALUE IN FRAME F_Solicitud = ""
           WVeces:SCREEN-VALUE IN FRAME F_Solicitud = ""
           WTasa:SCREEN-VALUE IN FRAME F_Solicitud = ""
           WMonto:SCREEN-VALUE IN FRAME F_Solicitud = "".

    IF Cmb_Agencias:SENSITIVE = NO THEN
        APPLY "leave" TO Cmb_Agencias.

    IF SELF:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Debe ingresarse el número de indentificación del posible deudor. Rectifique!!" SKIP(1)
                "Desea abrir la Consulta de Clientes?"
            VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice.

        IF choice THEN DO:
            RUN C-Clientes.R(INPUT 1,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            ASSIGN NomNit:SCREEN-VALUE = P_Nombre + " " + P_Apellido
                   SELF:SCREEN-VALUE = P_Nit.

            FIND FIRST Clientes WHERE Clientes.Nit = P_Nit NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            HIDE FRAME F_solicitud.
            W_Nuevo = NO.
            APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
        END.
    END.
    ELSE DO:
        FIND FIRST Clientes WHERE Clientes.Nit = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
            NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        ELSE DO:
            RUN C-Clientes.R(INPUT 1,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            ASSIGN NomNit:SCREEN-VALUE = P_Nombre + " " + P_Apellido
                   SELF:SCREEN-VALUE = P_Nit.

            FIND FIRST Clientes WHERE Clientes.Nit = P_Nit NO-LOCK NO-ERROR.

            RUN ProSolNit IN h_w-proclientesnew(Clientes.Nit).
            RUN ProSolNit IN h_w-adm_garantias(Clientes.Nit).
        END.

        IF clientes.tipo_cliente = 2 OR TODAY - date(Clientes.Fec_Nacimiento) < 6570 THEN DO:
            MESSAGE "Es menor de edad - No tiene acceso a Creditos!" SKIP
                    "Digite un número de cédula"
                VIEW-AS ALERT-BOX INFORMATION.

            RETURN NO-APPLY.
        END.
    END.

    FIND FIRST listaNegra WHERE listaNegra.nit = SELF:SCREEN-VALUE
                            AND listaNegra.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE listaNegra AND listaNegra.Fec_Exclusion >= TODAY THEN DO:
        MESSAGE "Asociado esta en lista de suspendidos hasta el día " listaNegra.Fec_Exclusion
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        solicitud.nit:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.

    /*RUN validarUsuarioSarlaft.R (INPUT SELF:SCREEN-VALUE, OUTPUT W_Sarlaft) NO-ERROR.*/
    IF W_Sarlaft THEN DO:
         solicitud.nit:SCREEN-VALUE = "".
         RETURN NO-APPLY.
    END.

    IF clientes.id_privilegiado = 1 THEN DO:
        ASSIGN Texto1 = ""
               Texto2 = "".

        ASSIGN Texto1 = "Solicitud debe ser aprobada por una instancia superior"
               Texto2 = "Solicitante es referido de un directivo de la Cooperativa".

        DISPLAY Texto1
                Texto2
            WITH FRAME F_Solicitud.
    END.

    FIND FIRST Relaciones WHERE Relaciones.Nit_Relacion = SELF:SCREEN-VALUE
                            AND Relaciones.Cod_Relacion = 1
                            AND Relaciones.Estado = 1 NO-LOCK NO-ERROR.

    ASSIGN Texto1 = ""
           Texto2 = "".

    IF AVAILABLE Relaciones THEN
        ASSIGN Texto1 = "Solicitud debe ser aprobada por una instancia superior"
               Texto2 = "Solicitante es referido de un directivo de la Cooperativa".

    DISPLAY Texto1
            Texto2
        WITH FRAME F_Solicitud.

    IF AVAILABLE Clientes THEN DO:
        RUN edad IN w_manija (INPUT clientes.fec_nacimiento,
                              OUTPUT wk_edad) NO-ERROR.

        /* oakley */

        ASSIGN WX_Edad = WK_Edad
               WX_Edad:SCREEN-VALUE = STRING(WX_Edad).

        IF W_Nuevo EQ YES AND Clientes.Fec_Retiro NE ? AND Clientes.Estado EQ 2 THEN DO:
            MESSAGE "No se pueden crear solicitudes de crédito a clientes retirados" SKIP(1)
                    "Se cancela el proceso de solicitud Actual"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "choose" TO Btn_Cancelar IN FRAME F_Creditos.
            RETURN NO-APPLY.
        END.

        IF (W_fecha - Clientes.Fec_ultActual) > 360 THEN DO:
            MESSAGE "El Asociado no tiene la informacion actualizada en el último año." SKIP
                    "No se podrá crear una solicitud hasta no actualizar la información!"
                VIEW-AS ALERT-BOX WARNING.

            ASSIGN Solicitud.Nit:SCREEN-VALUE = ""
                   NomNit:SCREEN-VALUE = "".

            APPLY "entry" TO Cmb_Agencias.
            RETURN NO-APPLY.
        END.

        FIND FIRST Ahorros WHERE Ahorros.Nit EQ Solicitud.Nit:SCREEN-VALUE
                             AND Ahorros.Tip_Ahorro EQ 4
                             AND Ahorros.Estado EQ 1
                             AND Ahorros.Sdo_Dispon + Sdo_Canje GT 0 NO-LOCK NO-ERROR.
        
        FIND FIRST Bsolicitud WHERE BSolicitud.Nit EQ Solicitud.Nit:SCREEN-VALUE
                                AND BSolicitud.Estado LE 1
                                AND BSolicitud.Num_solicitud NE INTEG(Solicitud.Num_solicitud:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAIL(Bsolicitud) THEN
            MESSAGE "Este cliente esta tramitando actualmente otra solicitud en la agencia: " BSolicitud.agencia
                VIEW-AS ALERT-BOX TITLE "PRECAUCION".

        DO:
            IF Clientes.Tipo_Cliente LE 2 THEN DO:
                ASSIGN vcFormatoFecha = SESSION:DATE-FORMAT.
                SESSION:DATE-FORMAT = "dmy".

                SESSION:DATE-FORMAT = vcFormatoFecha.
            END.

            FIND FIRST Creditos WHERE Creditos.Nit EQ P_Nit
                                  AND Creditos.Sdo_Capital GT 0
                                  AND Creditos.Fec_Pago LT W_Fecha NO-LOCK NO-ERROR.

            IF AVAIL Creditos THEN
                MESSAGE "El solicitante tiene Crèditos en Mora en la Cooperativa..."
                    VIEW-AS ALERT-BOX TITLE "Alerta por Morosidad".

            RUN Llenar_InfoCliente.
        END.
    END.

    IF AVAIL Clientes THEN DO:
        Wk_PerPagEmp = 5.

        FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
        IF AVAIL Empresas THEN
            Wk_PerPagEmp = Empresas.FOR_Pago + 1.

        IF W_Nuevo THEN DO:
            IF AVAIL Solicitud THEN DO:
                IF Solicitud.FOR_Pago NE 2 THEN DO:
                    Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "1".
                    W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Caja".
                    Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(5).

                    IF Wk_PerPagEmp NE 0 AND W_Nuevo THEN DO:
                        Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(Wk_PerPagEmp).
                        Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "2".
                        W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Nómina".
                    END.
                    ELSE DO:
                        Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(Solicitud.FOR_Pago).
                        Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = STRING(Solicitud.FOR_Pago).

                        CASE Solicitud.FOR_Pago:
                            WHEN 1 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Caja".
                            WHEN 2 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Nómina".
                            WHEN 3 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Deb. Automático".
                            WHEN 4 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Nomina Crecediario".
                            WHEN 5 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Prima".
                        END CASE.
                    END.
                END.
                ELSE DO:
                    CASE Solicitud.Per_Pago:
                        WHEN 0 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "0 - Diario". 
                        WHEN 1 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "1 - Semanal".
                        WHEN 2 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "2 - Decadal".
                        WHEN 3 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "3 - Quincenal".     
                        WHEN 4 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "4 - Mensual".
                        WHEN 5 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "5 - Bimensual".
                        WHEN 6 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "6 - Trimestral".
                        WHEN 7 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "7 - Cuatrimestral".
                        WHEN 8 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "8 - Semestral".
                        WHEN 9 THEN ASSIGN Cmb_PerPago:SCREEN-VALUE = "9 - Anual".
                    END CASE.
                    
                    Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = STRING(Solicitud.FOR_Pago).

                    CASE Solicitud.FOR_Pago:
                        WHEN 1 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Caja".
                        WHEN 2 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Nómina".
                        WHEN 3 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Deb. Automático".
                        WHEN 4 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Nomina Crecediario".
                        WHEN 5 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud = "Prima".
                    END CASE.
                END.
            END.
        END.

        W_Antiguedad = ROUND((TODAY - Clientes.Fec_Asociacion) / 365,2).

        ASSIGN Fec_Asociado:SCREEN-VALUE IN FRAME F_Solicitud = STRING(Clientes.Fec_Asociacion)
               WAntiguedad:SCREEN-VALUE IN FRAME F_Solicitud = STRING(W_Antiguedad).

        FIND FIRST Ahorros WHERE Ahorros.Nit = Clientes.nit AND Ahorros.Cod_Ahorro = 3 NO-LOCK NO-ERROR.
        IF NOT  AVAIL Ahorros THEN
            FIND FIRST Ahorros WHERE Ahorros.Nit = Clientes.nit AND Ahorros.Cod_Ahorro = 8 NO-LOCK NO-ERROR.
        IF NOT  AVAIL Ahorros THEN
            MESSAGE "Cliente No Posee Cuenta de Ahorro Permanente"
                VIEW-AS ALERT-BOX.
        ELSE DO:
            FIND FIRST Varios WHERE Varios.Tipo = 39 AND DEC(Varios.Codigo) >= W_Antiguedad NO-LOCK NO-ERROR.
            IF AVAIL Varios THEN DO:
                ASSIGN WVeces:SCREEN-VALUE IN FRAME F_Solicitud = STRING(Varios.Val_Final)
                       WTasa:SCREEN-VALUE  IN FRAME F_Solicitud = STRING(Varios.Val_Inicial)
                       WMonto:SCREEN-VALUE IN FRAME F_Solicitud = STRING(ROUND(Ahorros.Sdo_Disponible * Varios.Val_Final,0)).
            END.

            ASSIGN WVeces.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Nit_Aseguradora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON LEAVE OF Garantias.Nit_Aseguradora IN FRAME F_Admisible /* Nit Aseguradora */
DO:
DO WITH FRAME F_Admisible:
   IF SELF:SCREEN-VALUE EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud THEN DO:
      MESSAGE "No puede ser codeudor de si mismo" SKIP
              "Rectifique el nit del codeudor" SKIP
              "para el cliente: " Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.
   FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) THEN DO:
      RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
      ASSIGN Nom_Aseguradora:SCREEN-VALUE = P_Nombre + " " + P_Apellido
             SELF:SCREEN-VALUE = P_Nit.
   END.
   ELSE
      ASSIGN Nom_Aseguradora:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nit_Aseguradora wWin
ON VALUE-CHANGED OF Garantias.Nit_Aseguradora IN FRAME F_Admisible /* Nit Aseguradora */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nom_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nom_Impuesto wWin
ON VALUE-CHANGED OF Garantias.Nom_Impuesto IN FRAME F_Admisible /* Nom.Impuesto */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Nro_Seguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Nro_Seguro wWin
ON VALUE-CHANGED OF Garantias.Nro_Seguro IN FRAME F_Admisible /* Número de Seguro */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Hoja_Vida.Observacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON ENTRY OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  longitud = SELF:LENGTH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON LEAVE OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  IF Longitud NE SELF:LENGTH THEN ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON MOUSE-SELECT-CLICK OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  IF NOT W_NvaHV THEN DO:
     Id_Agregar = "HV".
     E_Agregar:SCREEN-VALUE IN FRAME F_Agregar = "".
     MESSAGE "No se permite el cambio del texto seleccionado" SKIP
             "Desea agregar algun mensaje de texto al asunto?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE choice.
     IF choice THEN DO:
        ENABLE ALL WITH FRAME F_Agregar.
        VIEW FRAME F_Agregar.
        APPLY "entry" TO E_Agregar IN FRAME F_Agregar.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON VALUE-CHANGED OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME PDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PDF wWin
ON CHOOSE OF PDF IN FRAME F_Creditos /* PDF */
DO:
  RUN pdfSolicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Plazo wWin
ON LEAVE OF Solicitud.Plazo IN FRAME F_Solicitud /* Plazo */
DO:
    DEFINE VAR fechaTemp AS DATE.

    ASSIGN Dias = 0
           WFactorCod = 1.

    FOR EACH Relaciones NO-LOCK WHERE Relaciones.Nit EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                                  AND Relaciones.Cod_Relacion EQ 11 AND Relaciones.Clase_Producto EQ 2
                                  AND Relaciones.Cod_Producto EQ INT(SUBSTR(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
                                  AND Relaciones.Cuenta EQ Solicitud.Num_solicitud:SCREEN-VALUE IN FRAME F_Solicitud
                                  AND Relaciones.estado = 1:
        WfactorCod = WfactorCod + 1.
    END.

    DO WITH FRAME F_Solicitud:
        FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Agencia EQ INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                                    AND Cfg_Instancias.Tipo_Instancia EQ 1
                                    AND Cfg_Instancias.Instancia EQ INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                    AND Cfg_Instancias.Usuario EQ W_Usuario
                                    AND Cfg_Instancias.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAIL Cfg_Instancias THEN DO:
            MESSAGE "La configuración de Instacia para este usuario" SKIP
                    "ya no esta disponible. Rectifique con el Administrador!"
                VIEW-AS ALERT-BOX INFORMATION.

            APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
        END.

        IF AVAIL Pro_Creditos THEN DO:
            ASSIGN Dias = DEC(SELF:SCREEN-VALUE)
                   vPeriodicidad = INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE,1,1)).

            RUN CalcularPlazo.

            IF NOT Pro_Creditos.Id_AprobAgencia THEN DO:
                IF DEC(SELF:SCREEN-VALUE) GT PlaMaxI THEN DO:
                    MESSAGE "El plazo establecido se sale del plazo mayor de creditos" SKIP
                            "que usted tiene permitido radicar, el cual es:" STRING(Cfg_Instancias.Plazo_Maximo,">,>>9")
                        VIEW-AS ALERT-BOX WARNING TITLE "Configuración de la Instancia".

                    APPLY "ENTRY" TO Solicitud.Plazo.
                    
                    RETURN.
                END.

                IF DEC(SELF:SCREEN-VALUE) LT PlaMinI THEN DO:
                    MESSAGE "El plazo establecido se sale del plazo menor de creditos" SKIP
                            "que usted tiene permitido radicar, el cual es:" STRING(Cfg_Instancias.Plazo_Minimo,">,>>9")
                        VIEW-AS ALERT-BOX WARNING TITLE "Configuración de la Instancia".

                    APPLY "ENTRY" TO Solicitud.Plazo.

                    RETURN.
                END.
            END.

            IF Solicitud.Plazo:SCREEN-VALUE NE "0" THEN
                RUN Buscar_Indicadores.

            RUN MostrarAportes.

            EMPTY TEMP-TABLE TDeducc.

            DO NN = 1 TO 10 BY 1:
                IF Pro_Creditos.Deducible[NN] NE "" AND Pro_Creditos.Deducible[NN] NE ? THEN DO:
                    FIND Deducible WHERE Deducible.Cod_Deducible EQ Pro_Creditos.Deducible[NN] NO-LOCK NO-ERROR.
                    IF AVAIL Deducible THEN DO:
                        FIND TDeducc WHERE TDeducc.Cod_Deducible = Deducible.Cod_Deducible NO-ERROR.
                        IF AVAIL TDeducc THEN
                            DELETE TDeducc.

                        CREATE TDeducc.
                        BUFFER-COPY Deducible TO TDeducc.
                    END.
                END.
            END.

            ASSIGN p_forpag = INTEG(Solicitud.For_Pago:SCREEN-VALUE IN FRAME F_Forpago)
                   P_CedNit  = (Solicitud.Nit:SCREEN-VALUE IN FRAME f_solicitud).

            RUN ProcesarDeducibles.

            ASSIGN Solicitud.Total_Prestamo:SCREEN-VALUE = Solicitud.Monto:SCREEN-VALUE
                   W_VrADesemb = DEC(Solicitud.Total_Prestamo:SCREEN-VALUE) - DEC(Solicitud.Deducible:SCREEN-VALUE) - W_VrCredACanc
                   W_VrADesemb:SCREEN-VALUE = STRING(W_VrADesemb).
        END.
        ELSE DO:
            MESSAGE "Se debe escoger un producto de crédito" SKIP
                    "antes de ingresar el plazo. Rectifique"
                VIEW-AS ALERT-BOX INFORMATION.

            VIEW FRAME F_Producto.
            APPLY "entry" TO Cmb_Productos IN FRAME F_Producto.
            RETURN.
        END.

        /*IF AVAIL Solicitud AND W_recoge = TRUE THEN DO:
            RUN Hallar_Tasa_Nominal.

            RUN Hallar_Tasa_Efectiva.
            RUN Hallar_Tasa_Periodo.
        END.*/
    END.

    /* Se asigna la fecha de desembolso y la fecha de primer pago como propuesta */
    IF solicitud.fec_desembolso:SCREEN-VALUE = '' /*OR DATE(solicitud.fec_desembolso:SCREEN-VALUE) < w_fecha*/ THEN DO:
        IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)) <> 2 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 108 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 113  AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 114 THEN
            ASSIGN solicitud.fec_desembolso:SCREEN-VALUE = solicitud.fec_solicitud:SCREEN-VALUE.
        ELSE
            ASSIGN solicitud.fec_desembolso:SCREEN-VALUE = solicitud.fec_solicitud:SCREEN-VALUE.
    END.

    IF solicitud.fec_primerPago:SCREEN-VALUE = '' /*OR DATE(solicitud.fec_primerPago:SCREEN-VALUE) <= w_fecha*/ THEN DO:
        IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)) <> 2 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 108 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 113 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 114 THEN DO:

            fechaTemp = ADD-INTERVAL(DATE(solicitud.fec_desembolso:SCREEN-VALUE),1,"months").

            IF DAY(fechaTemp) <= 15 THEN
                fechaTemp = DATE(MONTH(fechaTemp),10,YEAR(fechaTemp)).
            ELSE
                fechaTemp = DATE(MONTH(ADD-INTERVAL(fechaTemp,1,"months")),10,YEAR(ADD-INTERVAL(fechaTemp,1,"months"))).

            ASSIGN solicitud.fec_primerPago:SCREEN-VALUE = STRING(fechaTemp,"99/99/9999").
        END.
        ELSE
            ASSIGN solicitud.fec_primerPago:SCREEN-VALUE = STRING(ADD-INTERVAL(DATE(solicitud.fec_solicitud:SCREEN-VALUE),INTEGER(solicitud.plazo:SCREEN-VALUE),"months"),"99/99/9999").
    END.
    /* ------------------------------------------------------------------------- */
    
    CLOSE QUERY Br_Cred.
    OPEN  QUERY Br_Cred FOR EACH TCred_ACanc INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Plazo wWin
ON VALUE-CHANGED OF Solicitud.Plazo IN FRAME F_Solicitud /* Plazo */
DO:
  Solicitud.Cuota:SCREEN-VALUE = "0".
  ENABLE Btn_Liquidar WITH FRAME F_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME RActivas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RActivas wWin
ON VALUE-CHANGED OF RActivas IN FRAME F_Codeudores
DO:
  IF SELF:SCREEN-VALUE EQ "1" THEN
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE 
       TCode.TC_EstRela EQ 1 NO-LOCK INDEXED-REPOSITION.
  ELSE
    OPEN QUERY Br_Codeudores FOR EACH TCode WHERE 
       TCode.TC_EstRela EQ 2 NO-LOCK INDEXED-REPOSITION.
 
 IF Br_Codeudores:NUM-SELECTED-ROWS IN FRAME F_Codeudores NE 0 THEN
    ASSIGN W_NitCodeudor:SCREEN-VALUE = TCode.TC_NitCode
           W_NomCodeudor:SCREEN-VALUE = TCode.TC_NomCode.
 ELSE
    ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
           W_NomCodeudor:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Rs_Ocupacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Ocupacion wWin
ON VALUE-CHANGED OF Rs_Ocupacion IN FRAME F_VblesS /* Ocupación */
DO:
    DO WITH FRAME F_VblesS:
        IF SELF:SCREEN-VALUE = "5"
        THEN do:
            ENABLE cmbEstdoCrgo.
        END.
        ELSE do:
            cmbEstdoCrgo:SCREEN-VALUE = "Otros Servidores".
            DISABLE cmbEstdoCrgo.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConAdmisible
&Scoped-define SELF-NAME R_ConAdm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_ConAdm wWin
ON VALUE-CHANGED OF R_ConAdm IN FRAME F_ConAdmisible
DO:
  ASSIGN R_ConAdm.

  RUN Consul_Gtias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME R_Organizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Organizar wWin
ON VALUE-CHANGED OF R_Organizar IN FRAME F_Consulta
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Num_Solicitud INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Solicitud".
    END.
    WHEN "2" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.AgeSolicitud INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Agencia".
    END.
    WHEN "3" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Nit INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Nit".
    END.
    WHEN "4" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Nombre INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Nombre".
    END.
    WHEN "5" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Fec_ingreso INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Fec.Ingreso".
    END.
    WHEN "6" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Vigencia INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Vigencia".
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Garantias
&Scoped-define SELF-NAME R_TipoGarantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipoGarantia wWin
ON MOUSE-SELECT-CLICK OF R_TipoGarantia IN FRAME F_Garantias
DO:
  ASSIGN R_TipoGarantia.

  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipoGarantia wWin
ON VALUE-CHANGED OF R_TipoGarantia IN FRAME F_Garantias
DO:
  IF SELF:SCREEN-VALUE = "1" THEN DO:
    HIDE FRAME F_Admisible.
    VIEW FRAME F_Codeudores.
    ASSIGN Relaciones.Aprobada:SENSITIVE = TRUE.
    IF NOT Instancias.Id_Scoring AND NOT Instancias.Id_Concepto THEN
       ASSIGN Relaciones.Aprobada:SENSITIVE = FALSE.
  END.
  ELSE DO:
    RUN F_AdNad.
    IF R_TipoGarantia EQ 3 THEN DO:
           FIND FIRST Garantias WHERE Garantias.Agencia   EQ Solicitud.Agencia AND
                                  Garantias.Tip_Credito   EQ Solicitud.Tip_Credito AND
                                  Garantias.Cod_Credito   EQ Solicitud.Cod_Credito AND
                                  Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
                                  Garantias.Estado        EQ 1 AND
                                  Garantias.Tipo_Garantia GE 4   NO-ERROR. 
    END.
    ELSE
       FIND FIRST Garantias WHERE Garantias.Agencia       EQ Solicitud.Agencia AND
                                  Garantias.Tip_Credito   EQ Solicitud.Tip_Credito AND   
                                  Garantias.Cod_Credito   EQ Solicitud.Cod_Credito AND   
                                  Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND 
                                  Garantias.Estado        EQ 1 AND                       
                                  Garantias.Tipo_Garantia LE 3  OR 
                                  Garantias.Tipo_Garantia EQ 6 NO-ERROR.                 

    IF AVAILABLE Garantias THEN DO:
       RUN Mostrar_Admisible.
       W_NvaAdm = FALSE.
    END.
    ELSE DO:
       RUN Inicializar_Admisible.
       W_NvaAdm = YES.
    END.

    VIEW FRAME F_Admisible.

    ASSIGN Garantias.Aprobada:SENSITIVE         = TRUE
           Garantias.Descripcion_Bien:SENSITIVE = TRUE.

    IF NOT Instancias.Id_Scoring AND NOT Instancias.Id_Concepto THEN
       ASSIGN Garantias.Aprobada:SENSITIVE         = FALSE
              Garantias.Descripcion_Bien:SENSITIVE = FALSE.

    HIDE FRAME F_Codeudores.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_VblesS
&Scoped-define SELF-NAME Clientes.Salario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Salario wWin
ON LEAVE OF Clientes.Salario IN FRAME F_VblesS /* Salario */
DO:
/*        
  ASSIGN Tot_Ingresos = DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +   
                        DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) + DEC(Clientes.Ing_Otros:SCREEN-VALUE)       + 
                        DEC(Clientes.Salario:SCREEN-VALUE)
         Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos).
  RUN Halla_CapPago.
*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Sdo_Obligaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Sdo_Obligaciones wWin
ON LEAVE OF Clientes.Sdo_Obligaciones IN FRAME F_VblesS /* Deuda DataCrédito */
DO:
        /*
  ASSIGN Tot_Egresos = DEC(Clientes.Gto_obligacion:SCREEN-VALUE) + DEC(Clientes.Gto_Familiar:SCREEN-VALUE) + 
                       DEC(Clientes.Gto_Arriendo:SCREEN-VALUE)   + DEC(Clientes.Sdo_obligacion:SCREEN-VALUE)
         Tot_Egresos:SCREEN-VALUE  = STRING(Tot_Egresos).
  RUN Halla_CapPago.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Solicitud
&Scoped-define SELF-NAME Solicitud.Tasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Tasa wWin
ON LEAVE OF Solicitud.Tasa IN FRAME F_Solicitud /* Efectiva */
DO:
  RUN Hallar_Tasa_Nominal.
  RUN Hallar_Tasa_Periodo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Tasa wWin
ON VALUE-CHANGED OF Solicitud.Tasa IN FRAME F_Solicitud /* Efectiva */
DO:
 DO WITH FRAME F_Solicitud:
  ENABLE Btn_Liquidar.
  Solicitud.Cuota:SCREEN-VALUE = "0".
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Tipo_Garantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Tipo_Garantia wWin
ON MOUSE-SELECT-CLICK OF Garantias.Tipo_Garantia IN FRAME F_Admisible
DO:
    IF  garantias.tipo_garantia:SCREEN-VALUE EQ "6" THEN DO:
    
        FIND FIRST ahorros WHERE tip_ahorro = 4 AND ahorros.estado = 1 AND
         ahorros.nit = solicitud.nit:SCREEN-VALUE IN FRAME f_solicitud NO-LOCK NO-ERROR.
       IF NOT AVAILABLE(ahorros) THEN DO:
           MESSAGE "NO se encontro ninguna cuenta de aportes!" VIEW-AS ALERT-BOX INFO BUTTON OK.
           RETURN NO-APPLY.
       END.
       ELSE
           ASSIGN  garantias.val_bien:SCREEN-VALUE = string(sdo_disponible).
    END.
ELSE
  IF Garantias.Tipo_Garantia:SCREEN-VALUE EQ "4" THEN
     APPLY "Entry" TO Garantias.Identificacion_Bien.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Solicitud.Tip_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Tip_Credito wWin
ON MOUSE-SELECT-CLICK OF Solicitud.Tip_Credito IN FRAME F_Producto /* Tipo de Producto Cs/Cm/H */
DO:
 ASSIGN /*Cmb_PerPago:SENSITIVE IN FRAME F_Solicitud = FALSE*/
        Cmb_Productos:LIST-ITEMS                   = "".

 FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                             Pro_Creditos.Estado  EQ 1 NO-LOCK BY pro_creditos.cod_credito:
    
    W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
    IF AVAILABLE Solicitud AND Solicitud.Cod_Credito EQ Pro_Creditos.Cod_Credito THEN
       Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
 END.

 IF INTEGER(SELF:SCREEN-VALUE) EQ 4 THEN
    ASSIGN Cmb_PerPago:SENSITIVE IN FRAME F_Solicitud = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Solicitud.Tip_Credito wWin
ON VALUE-CHANGED OF Solicitud.Tip_Credito IN FRAME F_Producto /* Tipo de Producto Cs/Cm/H */
DO:
DO WITH FRAME F_Producto:
 Cmb_Productos:LIST-ITEMS = "".
 FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                             Pro_Creditos.Estado  EQ 1 NO-LOCK BY pro_creditos.cod_credito:
     
     W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
     IF AVAILABLE Solicitud AND Solicitud.Cod_Credito EQ Pro_Creditos.Cod_Credito THEN
         Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
 END.
 /*

 FIND FIRST anexos_clientes WHERE anexos_clientes.nit = solicitud.nit NO-LOCK NO-ERROR.
    IF available(anexos_clientes) AND Anexos_Clientes.Cli_AAA = YES THEN DO:
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                                  Pro_Creditos.cod_Credito LT 700 AND
                                  Pro_Creditos.Estado  EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
          IF AVAILABLE Solicitud AND Solicitud.Cod_Credito EQ Pro_Creditos.Cod_Credito THEN
              Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
      END.
    END. /* else de triple aaa */
    ELSE DO:
      FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INTEGER(SELF:SCREEN-VALUE) AND
                                  Pro_Creditos.cod_Credito LE 700 AND
                                  Pro_Creditos.Estado  EQ 1 NO-LOCK:
          W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
          IF AVAILABLE Solicitud AND Solicitud.Cod_Credito EQ Pro_Creditos.Cod_Credito THEN
              Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
      END.
    END. /* else triple aaa*/*/
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Admisible
&Scoped-define SELF-NAME Garantias.Val_Asegurado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Asegurado wWin
ON VALUE-CHANGED OF Garantias.Val_Asegurado IN FRAME F_Admisible /* Valor Asegurado */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Bien
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Bien wWin
ON LEAVE OF Garantias.Val_Bien IN FRAME F_Admisible /* Valor del Bien */
DO:
  DEFI VAR W_RowIdG AS ROWID.
  DEFI VAR W_TotAval LIKE Solicitud.TOTAL_Prestamo INIT 0.

  IF DEC(Garantias.Val_Bien:SCREEN-VALUE) LT Solicitud.TOTAL_Prestamo THEN 
     MESSAGE "El valor de la Garantìa es Inferior al Monto Solicitado..."
         VIEW-AS ALERT-BOX TITLE "INFORMATIVO".

  ASSIGN W_TotAval = Solicitud.TOTAL_Prestamo
         W_RowIdG  = ROWID(Garantias).

  FOR EACH Creditos WHERE Creditos.Nit         EQ Solicitud.Nit
                      AND Creditos.Sdo_Capital GT 0 NO-LOCK:
      FIND FIRST Garantias WHERE 
            Garantias.Agencia             EQ Creditos.Agencia       AND
            Garantias.Nit                 EQ Creditos.Nit           AND
            Garantias.Tip_Credito         EQ Creditos.Tip_Credito   AND
            Garantias.Cod_Credito         EQ Creditos.Cod_Credito   AND
            Garantias.Num_Credito         EQ Creditos.Num_Credito   AND
            Garantias.Identificacion_Bien EQ Garantias.Identificacion_Bien:SCREEN-VALUE AND
            Garantias.Estado              EQ 1 NO-LOCK NO-ERROR.
      IF AVAIL(Garantias) THEN 
         ASSIGN W_TotAval = W_TotAval + Creditos.Sdo_Capital.      
  END.

  FIND Garantias WHERE ROWID(Garantias) EQ W_RowidG NO-LOCK NO-ERROR.

  IF W_TotAval GT DEC(Garantias.Val_Bien:SCREEN-VALUE) THEN
     MESSAGE "El valor de la Garantìa Supera el Monto Solicitado + Otros Crèditos Avalados..."
         VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Bien wWin
ON VALUE-CHANGED OF Garantias.Val_Bien IN FRAME F_Admisible /* Valor del Bien */
DO:
  ENABLE Btn_SalAdm WITH FRAME F_Admisible.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_Impuesto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_Impuesto wWin
ON VALUE-CHANGED OF Garantias.Val_Impuesto IN FRAME F_Admisible /* Valor */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Garantias.Val_UltAvaluo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Garantias.Val_UltAvaluo wWin
ON VALUE-CHANGED OF Garantias.Val_UltAvaluo IN FRAME F_Admisible /* Valor Último Avaluo */
DO:
    ENABLE Btn_SalAdm WITH FRAME F_Admisible. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Codeudores
&Scoped-define SELF-NAME W_NitCodeudor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCodeudor wWin
ON LEAVE OF W_NitCodeudor IN FRAME F_Codeudores /* Nit Codeudor */
DO:
DO WITH FRAME F_Codeudores:
   IF SELF:SCREEN-VALUE EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud THEN DO:
      MESSAGE "No puede ser codeudor de si mismo" SKIP
              "Rectifique el nit del codeudor" SKIP
              "para el cliente: " Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.

   FIND FIRST Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) THEN DO:
       MESSAGE "El codeudor no existe como tercero" SKIP
               "Antes de continuar ingrese la información en Archivo de Nit." SKIP
               "No se podrá crear el Codeudor" SKIP
               "hasta no ingresar la información que se requiere!" VIEW-AS ALERT-BOX WARNING.
                RUN tercero_nvo.r.  /* PIDE AGREGAR EL TERCERO ANTES DE CONTINUAR */
       /*
       SELF:SCREEN-VALUE =  " ".
       DISABLE W_NitCodeudor Btn_SalCod.
       ENABLE Btn_CreCod Btn_Activas.
       APPLY "entry" TO Cmb_Agencias.
       RETURN NO-APPLY. */
   END.
   ELSE DO:
       IF (W_fecha - Clientes.Fec_ultActual) > 360 THEN DO:
           MESSAGE "El Codeudor no tiene la informacion actualizada en el último año." SKIP
                   "No se podrá ingresar el Codeudor hasta no se actaulice la información que se requiere!"
               VIEW-AS ALERT-BOX WARNING.

           /*                              /* HABILITA CODEUDOR DESACTUALIZADO 02/11/2008 */
            SELF:SCREEN-VALUE =  " ".
            DISABLE W_NitCodeudor Btn_SalCod.
            ENABLE Btn_CreCod Btn_Activas.
            APPLY "entry" TO Cmb_Agencias.
            RETURN NO-APPLY. */
      END.

      IF NOT Clientes.Id_PuedeCodeudar THEN DO:
         MESSAGE "El Codeudor està en Clientes con Id de No-Codeudar" SKIP
                       "No se permite como codeudor" VIEW-AS ALERT-BOX ERROR.
                  SELF:SCREEN-VALUE =  " ".
                  DISABLE W_NitCodeudor Btn_SalCod.
                  ENABLE Btn_CreCod Btn_Activas.
                  APPLY "entry" TO Cmb_Agencias.
                  RETURN NO-APPLY.
      END.

      /*IF (W_fecha - Clientes.Fec_nacimiento)  LE 6570 THEN DO:
                 MESSAGE "El Codeudor No puede ser un menor de edad" SKIP
                       "ingrese un NIT que no sea de un menor!" VIEW-AS ALERT-BOX WARNING.
                  SELF:SCREEN-VALUE =  " ".
                  DISABLE W_NitCodeudor Btn_SalCod.
                  ENABLE Btn_CreCod Btn_Activas.
                  APPLY "entry" TO Cmb_Agencias.
                  RETURN NO-APPLY.
      END.*/
   
     /* IF tip_contrato EQ 5 AND Clientes.Tipo_Vinculo NE 1 THEN DO:
          MESSAGE "Codeudor pensionado y no asociado" SKIP
           "No se podrá crear Codeudor "  VIEW-AS ALERT-BOX WARNING.
          SELF:SCREEN-VALUE =  " ".
          DISABLE W_NitCodeudor Btn_SalCod.
          ENABLE Btn_CreCod Btn_Activas.
          APPLY "entry" TO Cmb_Agencias.
          RETURN NO-APPLY.
      END.*/

      FIND FIRST Creditos WHERE Creditos.Nit        EQ SELF:SCREEN-VALUE
                           AND Creditos.Sdo_Capital GT 0
                           AND Creditos.Fec_Pago    LT W_Fecha NO-LOCK NO-ERROR.
      IF AVAIL(Creditos) THEN
         MESSAGE "El Codeudor tiene Crèditos en Mora en la Cooperativa..."
            VIEW-AS ALERT-BOX TITLE "            Alerta por Morosidad".
   END.

   ASSIGN W_NomCodeudor:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
       /* triangulacion */
   FIND FIRST Relaciones WHERE 
            Relaciones.Nit          EQ SELF:SCREEN-VALUE   AND
            Relaciones.estado       EQ 1                   AND
            Relaciones.Cod_Relacion EQ 11                  AND
            Relaciones.Nit_Relacion EQ Solicitud.Nit:SCREEN-VALUE  IN FRAME F_Solicitud NO-LOCK NO-ERROR.
   IF AVAILABLE(Relaciones) THEN DO:
       MESSAGE "No puede ser codeudor de alguien " SKIP
              "que lo esta codeudando . Triangulacion" SKIP
              "para el cliente: " Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
              VIEW-AS ALERT-BOX ERROR.
         /*  APPLY "entry" TO SELF.
           RETURN NO-APPLY. temporal*/
   END.
   FIND FIRST Relaciones WHERE 
        Relaciones.Nit          EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
        Relaciones.Cuenta       EQ STRING(Solicitud.Num_Solicitud:SCREEN-VALUE)    AND
        Relaciones.Estado       EQ 1                                               AND
        Relaciones.Cod_Relacion EQ 11                                              AND
        Relaciones.Nit_Relacion EQ SELF:SCREEN-VALUE                               AND
        Relaciones.Cod_Producto EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
        NO-LOCK NO-ERROR.
   IF AVAILABLE(Relaciones) THEN DO:
     ASSIGN W_NitCodeudor:SCREEN-VALUE = ""
            W_NomCodeudor:SCREEN-VALUE = "".
      MESSAGE "Esta Relacion ya existe!" SKIP
              "Se Cancela la Operacion de Creado" VIEW-AS ALERT-BOX WARNING.
      DISABLE W_NitCodeudor Btn_SalCod.
      ENABLE Btn_CreCod Btn_Activas.
   END.
   ELSE
       APPLY 'entry' TO Btn_SalCod.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME W_PPExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_PPExtra wWin
ON LEAVE OF W_PPExtra IN FRAME F_Extras
DO:
  ASSIGN W_PPExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrExtra wWin
ON LEAVE OF W_VrExtra IN FRAME F_Extras
DO:
  ASSIGN W_VrExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_CredACanc
&Scoped-define BROWSE-NAME BR_Admisible
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */


/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

ON 'value-changed':U,'leave' OF clientes.salario IN FRAME F_VblesS,
                                clientes.ing_arriendos IN FRAME F_VblesS,
                                clientes.gto_arriendo IN FRAME F_VblesS,
                                clientes.ing_financieros IN FRAME F_VblesS,
                                clientes.gto_obligacion IN FRAME F_VblesS,
                                clientes.ing_honorarios IN FRAME F_VblesS,
                                clientes.sdo_obligaciones IN FRAME F_VblesS,
                                clientes.ing_otros IN FRAME F_VblesS,
                                clientes.gtofinanc_indir IN FRAME F_VblesS,
                                clientes.gto_familiar IN FRAME F_VblesS DO:
    DO WITH FRAME F_VblesS:
        FIND FIRST bfrAgencia NO-LOCK WHERE bfrAgencia.agencia = clientes.agencia NO-ERROR.

        clientes.gto_familiar:SCREEN-VALUE = string(fRngoSlrio(decimal(tot_ingresos:SCREEN-VALUE IN FRAME F_VblesS),
                                                               Clientes.Per_ACargo,
                                                               bfrAgencia.ciudad)).

        fCpcdadPgo().

        ASSIGN tot_ingresos
               tot_egresos.

        w_liqDispon = tot_ingresos - tot_egresos.

        DISPLAY w_liqDispon.
    END.
END.


ON VALUE-CHANGED OF TCred_ACanc.CSiNo IN BROWSE Br_Cred DO:
    IF TCred_ACanc.CSiNo:SCREEN-VALUE IN BROWSE Br_Cred = "Si" THEN
        TCred_ACanc.valorAbono:SCREEN-VALUE IN BROWSE BR_Cred = Tcred_ACanc.SdoTD:SCREEN-VALUE.
    ELSE
        TCred_ACanc.valorAbono:SCREEN-VALUE IN BROWSE BR_Cred = '0'.
END.

ON VALUE-CHANGED OF TCred_ACanc.valorABono IN BROWSE Br_Cred DO:
    IF TCred_ACanc.CSiNo:SCREEN-VALUE IN BROWSE Br_Cred = "Si" OR DECIMAL(TCred_ACanc.valorAbono:SCREEN-VALUE) > DECIMAL(Tcred_ACanc.SdoTD:SCREEN-VALUE) THEN
        TCred_ACanc.valorAbono:SCREEN-VALUE IN BROWSE BR_Cred = Tcred_ACanc.SdoTD:SCREEN-VALUE.
END.

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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'w-adm_garantias.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-adm_garantias ).
       /* Position in AB:  ( 1.00 , 1.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'w-proclientesnew.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_w-proclientesnew ).
       /* Position in AB:  ( 4.62 , 1.00 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Negadas wWin 
PROCEDURE Asignar_Negadas :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.

IF Solicitud.Estado EQ 3 THEN DO:
  FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Instancia     NE INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
             Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
             Mov_instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
             Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     FIND  Instancias WHERE
           Instancias.Tipo_Instancia  EQ 1 AND
           Instancias.Instancia       EQ Mov_Instancias.Instancia AND
           Instancias.Tipo_Producto   EQ 2 AND
           Instancias.Estado          EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Instancias AND Instancias.Orden_Instancia LT Orden_InsPan THEN DO:
        MESSAGE "Para pasar a la siguiente instancia" SKIP
                "deben estar cerradas las instancias" SKIP
                "anteriores. al momento se encuentra" SKIP
                "abierta la instancia: " Mov_Instancias.Instancia SKIP
                "Vigente en el Usuario: " Mov_Instancias.Usuario
                 VIEW-AS ALERT-BOX ERROR.
        ENABLE ALL WITH FRAME F_Creditos.
        DISABLE NomUsuario WITH FRAME F_Creditos.
        ENABLE ALL WITH FRAME F_Consulta.
        OPEN QUERY Br_Consulta FOR EACH Consulta
                   NO-LOCK  BY Consulta.Num_Solicitud INDEXED-REPOSITION.
        Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
       RETURN ERROR.
     END.
  END.
  
  DO:
    FIND Instancias WHERE 
         Instancias.Tipo_Instancia EQ 1 AND
         Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
         Instancias.Tipo_Producto  EQ 2 AND
         Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN DO:
       W_ord = Instancias.Orden_Instancia.
       FIND Instancias WHERE
            Instancias.Tipo_Instancia  EQ 1 AND
            Instancias.Tipo_Producto   EQ 2 AND
            Instancias.Instancia       EQ W_Negadas AND
            Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
       IF AVAILABLE Instancias THEN DO: 
          FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

          FOR EACH TProIns: DELETE TProIns. END.
          /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
          FOR EACH Cfg_Instancias WHERE 
                   Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
                   Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                   Cfg_Instancias.Instancia      EQ Instancias.Instancia AND
                   Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                   Cfg_Instancias.Plazo_Minimo   LE Dias AND
                   Cfg_Instancias.Plazo_Maximo   GE Dias AND
                   Cfg_Instancias.Monto_Minimo   LE Solicitud.Monto AND
                   Cfg_Instancias.Monto_Maximo   GE Solicitud.Monto AND 
                   Cfg_Instancias.Estado         EQ 1 NO-LOCK:
               
               FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                     AND Usuarios.Agencia EQ Cfg_Instancias.Agencia 
                                     AND Usuarios.Estado EQ 1
                   NO-LOCK NO-ERROR.
               /*El pdcto pide aprobaciòn en la agencia*/
               /*IF  Pro_Creditos.Id_AprobAgencia AND AVAILABLE(Usuarios) AND Usuarios.Agencia NE W_Agencia THEN
                   NEXT.     /*para que Asigna solo a los de la misma agencia*/
               ELSE */
               /*  IF Pro_Creditos.Id_AprobAgencia AND NOT AVAIL(Usuarios) THEN NEXT.*/
               /*egm: valida si es el mismo usuario para los transitorios de asociados*/
              /* IF Pro_Creditos.Id_AprobAgencia AND AVAIL(Usuarios) AND 
                  Usuarios.Usuario NE W_Usuario AND Pro_Creditos.Cod_Credito EQ 7 THEN
                  NEXT.*/
               /* 2 */
               CREATE TProIns.
               ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                      TProIns.TP_Instancia = Cfg_Instancias.Instancia
                      TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                      TProIns.Tp_Cantidad  = 0
                      TProIns.TP_Agencia   = Cfg_Instancias.Agencia.

               FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
               IF AVAILABLE Instancias THEN 
                  TProIns.TP_NomInstan = Instancias.Nom_Instancia.
               ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

               IF AVAILABLE Usuarios THEN 
                  TProIns.TP_NomUsuar  = Usuario.Nombre.
               ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".

               ASSIGN TProIns.TP_Abogado = Instancias.Id_Abogado WHEN AVAIL(Instancias).
          END.

          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             MESSAGE "No se encontro ningún Usuario para asignar" SKIP
                     "la solicitud a la Proxima instancia." VIEW-AS ALERT-BOX.
             FIND Mov_Instancias WHERE 
                  Mov_Instancias.Instancia     EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
                  Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud NO-LOCK NO-ERROR.
             IF AVAILABLE Mov_Instancias THEN DO:
                ASSIGN Mov_Instancias.Estado = NO
                       Mov_Instancias.Fec_Retiro = ?.

                ENABLE ALL WITH FRAME F_Creditos.
                DISABLE NomUsuario WITH FRAME F_Creditos.
                ENABLE ALL WITH FRAME F_Consulta.
                APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
             END.
             RETURN ERROR.
          END.

          /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
          /*esto con el fin de escoger el que menos solicitudes este procesando y distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario  AND
                       Mov_Instancias.Estado    EQ NO    NO-LOCK:
                  TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
              END.
          END.

          CREATE Hoja_Vida.
          ASSIGN Hoja_Vida.Tipo       = 9 
                 Hoja_Vida.Codigo     = 1  
                 Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                 Hoja_Vida.DoctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                 Hoja_Vida.Nit        = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                 Hoja_Vida.Usuario    = W_Usuario
                 Hoja_Vida.Fec_Grabacion = w_fecha
                 Hoja_Vida.Hora_Grabacion = TIME + 100
                 Hoja_Vida.Observacion = 
                 "Se cierra la instancia: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos +
                 " - Procesada por Usuario: " + W_Usuario
                 Hoja_Vida.Asunto_Cumplido = YES.
          
          FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
              IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                 /*se crea registro en hoja de vida para usuario que procesa la isntancia actual*/
                 /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
                 CREATE Hoja_Vida.
                 ASSIGN Hoja_Vida.Tipo            = 9 
                        Hoja_Vida.Codigo          = 1  
                        Hoja_Vida.Instancia       = TProIns.TP_Instancia
                        Hoja_Vida.DoctoRefer      = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                        Hoja_Vida.Nit             = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                        Hoja_Vida.Usuario         = TProIns.TP_Usuario
                        Hoja_Vida.Fec_Grabacion   = W_Fecha
                        Hoja_Vida.Hora_Grabacion  = TIME
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 FIND FIRST Mov_Instancias WHERE
                      Mov_Instancias.Instancia     EQ TProIns.TP_Instancia AND
                      Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE) AND
                      Mov_Instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Mov_Instancias THEN DO:
                   IF TProIns.TP_Abogado THEN DO:
                      FIND FIRST Garantias WHERE 
                           Garantias.Agencia       EQ Solicitud.agencia AND
                           Garantias.Tip_Credito   EQ Solicitud.Tip_Credito AND
                           Garantias.Cod_Credito   EQ Solicitud.Cod_Credito AND
                           Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
                           Garantias.Estado        EQ 1 NO-LOCK NO-ERROR.
                      IF NOT AVAIL(Garantias) THEN
                         NEXT.
                   END.

                   CREATE Mov_Instancias.
                   ASSIGN Mov_Instancias.Fec_Ingreso   = w_fecha
                          Mov_Instancias.Hora_Ingreso  = TIME
                          Mov_Instancias.Nit           = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                          Mov_Instancias.Num_Solicitud = DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE)
                          Mov_Instancias.Usuario       = TProIns.TP_Usuario
                          Mov_Instancias.Instancia     = TProIns.TP_Instancia
                          Mov_Instancias.Agencia       = TProIns.TP_Agencia.
                 END.
              END.
          END.

/*          ENABLE ALL WITH FRAME F_Creditos.
          DISABLE NomUsuario WITH FRAME F_Creditos.
          ENABLE ALL WITH FRAME F_Consulta.
          APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
          Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".*/
       END.
    END.
  END.
END.

/*si la solicitud ha sido condicionada, para que se vaya a la ultima*/
IF Solicitud.Estado EQ 4 THEN DO:
  CREATE Hoja_Vida.
  ASSIGN Hoja_Vida.Tipo            = 9 
         Hoja_Vida.Codigo          = 1  
         Hoja_Vida.Instancia       = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
         Hoja_Vida.DoctoRefer      = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
         Hoja_Vida.Nit             = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
         Hoja_Vida.Usuario         = W_Usuario
         Hoja_Vida.Fec_Grabacion   = W_Fecha
         Hoja_Vida.Hora_Grabacion  = TIME
         Hoja_Vida.Asunto_Cumplido = YES.
   FIND Instancias WHERE 
        Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
        Instancias.Tipo_Instancia EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE Instancias THEN
         Hoja_Vida.Observacion = 
         "Se cierra la instancia condicionada: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos +
         " - Procesada por Usuario: " + W_Usuario + " - " + NomUsuario:SCREEN-VALUE IN FRAME F_Creditos.
         
  /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
 FIND FIRST Mov_Instancias WHERE
            Mov_Instancias.Instancia     EQ W_Ultima AND
            Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
            Mov_instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            NO-ERROR.
 IF NOT AVAILABLE Mov_Instancias THEN 
    CREATE Mov_Instancias.
 
 CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo         = 9 
         Hoja_Vida.Codigo      = 1  
         Hoja_Vida.Instancia   = W_Ultima
         Hoja_Vida.DoctoRefer  = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
         Hoja_Vida.Nit         = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
         Hoja_Vida.Usuario     = Mov_Instancias.Usuario
         Hoja_Vida.Fec_Grabacion = w_fecha
         Hoja_Vida.Hora_Grabacion = TIME
         Hoja_Vida.Asunto_Cumplido = YES.
 FIND Instancias WHERE Instancias.Tipo_Instancia  EQ 1 AND
                       Instancias.Id_Negadas      EQ YES AND
                       Instancias.Estado          EQ 1 AND
                       Instancias.Tipo_Producto   EQ 2 NO-LOCK NO-ERROR.
 IF AVAILABLE Instancias THEN DO:
     Hoja_Vida.Observacion = "Entra a negadas: " + STRING(Instancias.Instancia,"999") 
       + " - " + Instancias.Nom_Instancia +
       " - Usuario Responsable: " + STRING(Mov_Instancia.Usuario).
 END.
 
 ASSIGN Mov_Instancias.Fec_Ingreso   = W_Fecha.
        Mov_Instancias.Hora_Ingreso  = TIME.
        Mov_Instancias.Estado        = NO.
        Mov_Instancias.Fec_Retiro    = ?.
        Mov_Instancias.Hora_Retiro   = 0.
  
/* ENABLE ALL WITH FRAME F_Creditos.
 DISABLE NomUsuario WITH FRAME F_Creditos.
 ENABLE ALL WITH FRAME F_Consulta.
 APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.   */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Primera_Credito wWin 
PROCEDURE Asignar_Primera_Credito :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.
DEFINE VAR Asignado AS LOGICAL INITIAL NO.

IF Solicitud.Estado EQ 2 THEN DO:
  FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
             Mov_instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud AND
             Mov_Instancias.Estado EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     MESSAGE "Para pasar a la siguiente instancia" SKIP
             "deben estar cerradas las instancias" SKIP
             "anteriores. al momento se encuentra" SKIP
             "abierta la instancia: " Mov_Instancias.Instancia SKIP
             "Vigente en el Usuario: " Mov_Instancias.Usuario
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  
  FIND FIRST Pro_Creditos   WHERE Pro_Creditos.Cod_Credito   EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
  FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

  /* FIND Instancias WHERE Instancias.Tipo_Instancia EQ 3 AND   /* modific. 28/09/2007 */
                        Instancias.Primera        EQ YES AND
                        Instancias.Tipo_Producto  EQ 2 AND
                        Instancias.Estado         EQ 1 NO-LOCK NO-ERROR. */
  
  FIND FIRST Instancias WHERE Instancias.Tipo_Instancia EQ 3 AND /* Busca por orden  */
                          Instancias.Tipo_Producto      EQ 2 AND
                          Instancias.Estado             EQ 1 AND 
                          Instancia.Orden_Instancia     EQ 1 NO-LOCK NO-ERROR.

  IF AVAILABLE Instancias THEN DO:
          W_ord = Instancias.Orden_Instancia.
          FOR EACH TProIns: DELETE TProIns. END.
          /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
          FOR EACH Cfg_Instancias WHERE 
                   Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
                   Cfg_Instancias.Instancia      EQ Instancias.Instancia       AND
                   Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia  AND
                   Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                   Cfg_Instancias.Plazo_Minimo   LE Dias AND
                   Cfg_Instancias.Plazo_Maximo   GE Dias AND
                   Cfg_Instancias.Monto_Minimo   LE Solicitud.Monto AND
                   Cfg_Instancias.Monto_Maximo   GE Solicitud.Monto AND 
                   Cfg_Instancias.Estado         EQ 1 NO-LOCK:

              /* IF  Cfg_RegCredito.Agencia_Exigida EQ 11                 /*Dic.16/05 GAER*/
               AND Pro_Creditos.Tip_Credito       EQ 4 
               AND Pro_Creditos.Id_AprobAgencia 
               AND Cfg_Instancias.Usuario NE Solicitud.Usuario THEN
                   NEXT.*/
               
             /* Sept 27/09/2007   politicas de la empres */
           /*  IF INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) GT 3  AND
                INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) LT 11 THEN DO:
                IF  Cfg_Instancias.Usuario EQ W_Usuario THEN NEXT.
             END.
             ELSE */
               /* IF  Cfg_Instancias.Usuario NE W_Usuario THEN NEXT. */ /* Tomar mismo usuario */
          

              /* IF Solicitud.Cod_Credito EQ 7 AND Cfg_Instancias.Usuario EQ W_Usuario THEN 
                  MESSAGE "El crédito es un transitorio para asociados" SKIP
                          "se asignará automáticamente a la agencia y" SKIP
                          "al usuario que aprobó la solicitud!" VIEW-AS ALERT-BOX.              */
               /* 3 */
               CREATE TProIns.
               ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                      TProIns.TP_Instancia = Cfg_Instancias.Instancia
                      TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                      TProIns.Tp_Cantidad  = 0
                      TProIns.TP_Agencia   = CFG_Instancias.Agencia.

               FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
               IF AVAILABLE Instancias THEN 
                  TProIns.TP_NomInstan = Instancias.Nom_Instancia.
               ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

               FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
               IF AVAILABLE Usuarios THEN 
                  TProIns.TP_NomUsuar  = Usuario.Nombre.
               ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".

              /* IF  Cfg_RegCredito.Agencia_Exigida EQ 11                 /*Dic.16/05 GAER*/
               AND Pro_Creditos.Tip_Credito       EQ 4 
               AND Pro_Creditos.Id_AprobAgencia THEN
                   LEAVE.*/
          END.

          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             /*IF  Cfg_RegCredito.Agencia_Exigida EQ 11                 /*Dic.16/05 GAER*/
             AND Pro_Creditos.Tip_Credito       EQ 4 
             AND Pro_Creditos.Id_AprobAgencia THEN DO:
                 FIND FIRST Cfg_Instancias WHERE 
                   Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
                   Cfg_Instancias.Instancia      EQ Instancias.Instancia       AND
                   Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia  AND
                   Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                   Cfg_Instancias.Plazo_Minimo   LE Dias AND
                   Cfg_Instancias.Plazo_Maximo   GE Dias AND
                   Cfg_Instancias.Monto_Minimo   LE Solicitud.Monto AND
                   Cfg_Instancias.Monto_Maximo   GE Solicitud.Monto AND 
                   Cfg_Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
                 IF AVAIL(Cfg_Instancias) THEN DO:
                    CREATE TProIns.
                    ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden     
                           TProIns.TP_Instancia = Cfg_Instancias.Instancia 
                           TProIns.TP_Usuario   = Cfg_Instancias.Usuario   
                           TProIns.Tp_Cantidad  = 0                        
                           TProIns.TP_Agencia   = CFG_Instancias.Agencia.  
                 END.
             END. */
             
             FIND FIRST TProIns NO-ERROR.
             IF NOT AVAILABLE TProIns THEN DO:
                MESSAGE "No se ha encontrado Ninguna Configuración" SKIP
                     "donde un usuario tenga asignada la primera" SKIP
                     "instancia de Desembolso." SKIP(1)
                     "Se cancela la operación de aprobación"
                     VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
             END.
          END.

          /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
          /*esto con el fin de escoger el que menos solicitudes este procesando y distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario AND
                       Mov_Instancias.Estado    EQ NO NO-LOCK:
                       TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
              END.
              IF solicitud.usuario = tproins.Tp_Usuario  THEN TproIns.TP_Cantidad = 0. /*obliga el mismo que radico */
          END.

          FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
              IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                 CREATE Hoja_Vida.
                 ASSIGN Hoja_Vida.Tipo        = 9 
                        Hoja_Vida.Codigo      = 1  
                        Hoja_Vida.Instancia   = TProIns.TP_Instancia
                        Hoja_Vida.DoctoRefer  = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                        Hoja_Vida.Nit         = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                        Hoja_Vida.Usuario     = TProIns.TP_Usuario
                        Hoja_Vida.Fec_Grabacion = w_fecha
                        Hoja_Vida.Hora_Grabacion = TIME + 100
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 CREATE Mov_Instancias.
                 ASSIGN Mov_Instancias.Fec_Ingreso   = w_fecha
                        Mov_Instancias.Hora_Ingreso  = TIME
                        Mov_Instancias.Nit           = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                        Mov_Instancias.Num_Solicitud = DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE)
                        Mov_Instancias.Cuenta        = STRING(Creditos.Num_Credito)
                        Mov_Instancias.Usuario       = TProIns.TP_Usuario
                        Mov_Instancias.Instancia     = TProIns.TP_Instancia
                        Mov_Instancias.Agencia       = TPRoIns.TP_Agencia
                        Mov_Instancias.Estado        = NO
                        Asignado = YES.
                 LEAVE.  /*Nov.30/05 GAER*/
              END.
          END.

          IF NOT Asignado THEN DO:
              MESSAGE "No se ha encontrado la configuración de un usuario" SKIP
                      "en las instancias de desembolso!. Avise al administrador" SKIP
                      "para que configure la instacia de desembolso para un usuario" VIEW-AS ALERT-BOX.
              RETURN ERROR.
          END.
  END.  
  ELSE DO:
    MESSAGE "No existe La Config.de Instancia(Primera) para pasar al Desembolso."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Proxima_Instancia wWin 
PROCEDURE Asignar_Proxima_Instancia :
DEF VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEF VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.
DEF VAR W_AS            AS LOG INIT NO.
DEF VAR PMin            LIKE Cfg_Instancias.PLazo_Minimo INIT 0.
DEF VAR PMax            LIKE Cfg_Instancias.PLazo_Maximo INIT 999999999.
DEF VAR MMin            LIKE Cfg_Instancias.Monto_Minimo INIT 0.
DEF VAR MMax            LIKE Cfg_Instancias.Monto_Maximo INIT 999999999.
DEF VAR EndGlo          LIKE Solicitud.Monto INIT 0.
  EndGlo = EndGlo + Solicitud.Monto.
  FOR EACH Creditos WHERE Creditos.nit = Solicitud.Nit :
      IF Creditos.Estado NE 2 OR Creditos.TIP_CREDITO GT 4 THEN NEXT.
      IF Creditos.cod_credito = 570
         THEN EndGlo = EndGlo + Creditos.monto.
         ELSE EndGlo = EndGlo + Creditos.sdo_capital.
  END.
  IF Solicitud.Estado EQ 1 OR Solicitud.Estado EQ 4 THEN DO:
     FIND FIRST Mov_Instancias WHERE
                Mov_Instancias.Instancia     NE INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
            AND Mov_Instancias.Num_Solicitud EQ INT(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
            AND Mov_instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            AND Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
     IF AVAIL Mov_Instancias THEN DO:
        FIND Instancias WHERE Instancias.Tipo_Instancia EQ 1
                          AND Instancias.Instancia      EQ Mov_Instancias.Instancia
                          AND Instancias.Tipo_Producto  EQ 2
                          AND Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL Instancias AND Instancias.Orden_Instancia LT Orden_InsPan THEN DO:
           MESSAGE "Para pasar a la siguiente instancia" SKIP
                   "deben estar cerradas las instancias" SKIP
                   "anteriores. al momento se encuentra" SKIP
                   "abierta la instancia: " Mov_Instancias.Instancia SKIP
                   "Vigente en el Usuario: " Mov_Instancias.Usuario VIEW-AS ALERT-BOX ERROR.
           ENABLE ALL WITH FRAME F_Creditos.
           DISABLE NomUsuario WITH FRAME F_Creditos.
           ENABLE ALL WITH FRAME F_Consulta.
           OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Solicitud INDEXED-REPOSITION.
           Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
           RETURN ERROR.
        END.
     END.
   /*Cambio verificamos  la existencia de codeudores y Garantias   04/01/2005  John Moncada y Alexander Moncada*/ 
     FIND Clientes WHERE Clientes.Nit EQ Solicitud.Nit AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Clientes) THEN DO:
        MESSAGE "Falta el Cliente con Estado Activo para realizar la operaciòn..." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
     FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
     FIND FIRST Mov_Instancias WHERE
                Mov_Instancias.Instancia     NE INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
            AND Mov_Instancias.Num_Solicitud EQ INT(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
            AND Mov_instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
            AND Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
     IF AVAIL Mov_Instancias THEN DO:
        FIND Instancias WHERE Instancias.Tipo_Instancia  EQ 1
                          AND Instancias.Instancia       EQ Mov_Instancias.Instancia
                          AND Instancias.Tipo_Producto   EQ 2
                          AND Instancias.Estado          EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL instancias AND Instancias.Id_Scoring AND NOT Instancias.primera
           THEN RUN Verificar_relaciones_Garantias NO-ERROR.
     END.
     RELEASE clientes.
   /*Fin Cambio*/
     DO:
       FIND Instancias WHERE Instancias.Tipo_Instancia EQ 1 AND
            Instancias.Instancia      EQ INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
            Instancias.Tipo_Producto  EQ 2 AND Instancias.Estado EQ 1 NO-LOCK NO-ERROR.
       IF AVAIL Instancias THEN DO:
          W_ord = Instancias.Orden_Instancia.

          FIND NEXT Instancias WHERE Instancias.Tipo_Instancia  EQ 1
                                 AND Instancias.Orden_Instancia GT W_Ord
                                 AND Instancias.Tipo_Producto   EQ 2
                                 AND Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
          IF AVAIL Instancias THEN DO: 
             FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
             EMPTY TEMP-TABLE TProIns NO-ERROR.
           /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/

                
             IF NOT Pro_Creditos.Id_AprobAgencia THEN DO:

                FOR EACH Cfg_Instancias NO-LOCK WHERE 
                         Cfg_Instancias.Agencia        EQ INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                    AND  Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia
                    AND  Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia
                    AND  Cfg_Instancias.Plazo_Minimo   LE Dias
                    AND  Cfg_Instancias.Plazo_Maximo   GE Dias
                    AND  Cfg_Instancias.Monto_Minimo   LE EndGlo
                    AND  Cfg_Instancias.Monto_Maximo   GE EndGlo
                    AND  Cfg_Instancias.Estado         EQ 1:

   
                    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                          AND Usuarios.Agencia EQ Cfg_Instancias.Agencia 
                                          AND Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.

                     IF AVAIL Usuarios AND Usuarios.Agencia EQ Cfg_Instancias.Agencia THEN NEXT.
           /* 1 */  CREATE TProIns.
                    ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                           TProIns.TP_Instancia = Cfg_Instancias.Instancia
                           TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                           TProIns.Tp_Cantidad  = 0
                           TProIns.TP_Agencia   = Cfg_Instancias.Agencia.
                    FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
                    IF AVAIL Instancias THEN TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                                        ELSE TProIns.TP_NomInstan = "Instancia No encontrada".
                    IF AVAIL Usuarios THEN TProIns.TP_NomUsuar  = Usuario.Nombre.
                                      ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".
                    ASSIGN TProIns.TP_Abogado = Instancias.Id_Abogado WHEN AVAIL(Instancias).
                END.
             END.
             ELSE DO:
                FOR EACH Cfg_Instancias WHERE 
                         Cfg_Instancias.Agencia        EQ INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                    AND /*w_agencia INT(SUBSTR(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND */
                         Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia
                    AND  Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia
                    AND  Cfg_Instancias.Plazo_Minimo   LE Dias
                    AND  Cfg_Instancias.Plazo_Maximo   GE Dias
                    AND  Cfg_Instancias.Monto_Minimo   LE EndGlo
                    AND  Cfg_Instancias.Monto_Maximo   GE EndGlo
                    AND  Cfg_Instancias.Estado         EQ 1 NO-LOCK:

                   
                 /* para fabrica de credito*/
                 /* cuando se centralice todo se quita el if y se deja el else */
                    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario AND Usuarios.estado EQ 1 NO-LOCK NO-ERROR.
                 /* IF AVAIL(Usuarios) AND Usuarios.Usuario EQ W_Usuario THEN  NEXT.   */
                 /* este if impide que el usuario que sigue sea el mismo*/
                 /* SE CONDICIONA PARA QUE VALIDE SOLO LAS QUE NO SON DE BOGOTA */
                 /* CUANJDO SE IMPLEMENTE TODO EL PAIS QUEDA SIN EL IF SIGUIENTE */
                    IF AVAIL(usuarios) THEN DO: /* Agregue esta condicion antes de grabar */
                       
                       CREATE TProIns.
                       ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                              TProIns.TP_Instancia = Cfg_Instancias.Instancia
                              TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                              TProIns.Tp_Cantidad  = 0
                              TProIns.TP_Agencia   = Cfg_Instancias.Agencia.
                    END.
                    ELSE NEXT.
                    FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
                    IF AVAIL Instancias THEN TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                                        ELSE TProIns.TP_NomInstan = "Instancia No encontrada".
                    IF AVAIL Usuarios THEN TProIns.TP_NomUsuar  = Usuario.Nombre.
                                      ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".
                    ASSIGN TProIns.TP_Abogado = Instancias.Id_Abogado WHEN AVAIL(Instancias).
                END.
             END.
             FIND FIRST TProIns NO-ERROR.
             IF NOT AVAIL TProIns THEN DO:
                MESSAGE "No se encontro ningún Usuario fc" SKIP
                        "Para asignar la solicitud a la Proxima instancia." VIEW-AS ALERT-BOX.
                FIND Mov_Instancias WHERE Mov_Instancias.Instancia     EQ INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                      AND Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud NO-LOCK NO-ERROR.
                IF AVAIL Mov_Instancias THEN DO:
                   ASSIGN Mov_Instancias.Estado = NO Mov_Instancias.Fec_Retiro = ?.
                   ENABLE ALL WITH FRAME F_Creditos.
                   DISABLE NomUsuario WITH FRAME F_Creditos.
                   ENABLE ALL WITH FRAME F_Consulta.
                   APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
                END.
                RETURN ERROR.
             END.
           /*Por cada usuario encuentra el numero de Creditos que esta
             procesando para las instancias a seguir esto con el fin de
             escoger el que menos solicitudes este procesando y
             distribuir bien las solicitudes*/
             FOR EACH TproIns:
                 FOR EACH Mov_Instancias WHERE Mov_instancias.Instancia EQ TproIns.TP_Instancia
                      AND Mov_Instancias.Usuario   EQ TProIns.TP_Usuario
                      AND Mov_Instancias.Estado    EQ NO NO-LOCK:
                      TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
                 END.
             END.
             CREATE Hoja_Vida.
             ASSIGN Hoja_Vida.Tipo            = 9 
                    Hoja_Vida.Codigo          = 1  
                    Hoja_Vida.Instancia       = INT(SUBSTR(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                    Hoja_Vida.DoctoRefer      = INT(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                    Hoja_Vida.Nit             = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                    Hoja_Vida.Usuario         = W_Usuario
                    Hoja_Vida.Fec_Grabacion   = w_fecha
                    Hoja_Vida.Hora_Grabacion  = TIME + 100
                    Hoja_Vida.Observacion     = 
                   "Se cierra la instancia: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos +
                   " - Procesada por Usuario: " + W_Usuario
                    Hoja_Vida.Asunto_Cumplido = YES.
             FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
                 IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                  /*se crea registro en hoja de vida para usuario que procesa la isntancia actual*/
                  /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
                    CREATE Hoja_Vida.
                    ASSIGN Hoja_Vida.Tipo            = 9 
                           Hoja_Vida.Codigo          = 1  
                           Hoja_Vida.Instancia       = TProIns.TP_Instancia
                           Hoja_Vida.DoctoRefer      = INT(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                           Hoja_Vida.Nit             = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                           Hoja_Vida.Usuario         = TProIns.TP_Usuario
                           Hoja_Vida.Fec_Grabacion   = w_fecha
                           Hoja_Vida.Hora_Grabacion  = TIME
                           Hoja_Vida.Asunto_Cumplido = YES
                           Hoja_Vida.Observacion     = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                                               " - " + TProIns.TP_NomInstan  + " - Usuario Responsable: "
                                                     + TProIns.TP_Usuario    + " - " + TProIns.TP_NomUsuar.
                    FIND FIRST Mov_Instancias WHERE
                               Mov_Instancias.Instancia     EQ TProIns.TP_Instancia
                           AND Mov_Instancias.Num_Solicitud EQ INT(Solicitud.Num_Solicitud:SCREEN-VALUE)
                           AND Mov_Instancias.Nit           EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud NO-LOCK NO-ERROR.
                    IF NOT AVAIL Mov_Instancias OR (Solicitud.Estado EQ 4 AND Mov_Instancias.Estado EQ YES) THEN DO:
                       IF TProIns.TP_Abogado THEN DO:
                          FIND FIRST Garantias WHERE Garantias.Agencia       EQ Solicitud.agencia
                                                 AND Garantias.Tip_Credito   EQ Solicitud.Tip_Credito
                                                 AND Garantias.Cod_Credito   EQ Solicitud.Cod_Credito
                                                 AND Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud
                                                 AND Garantias.Estado        EQ 1 NO-LOCK NO-ERROR.
                          IF NOT AVAIL(Garantias) THEN NEXT.
                       END.
                    END.
                    CREATE Mov_Instancias.
                    ASSIGN Mov_Instancias.Nit           = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
                           Mov_Instancias.Num_Solicitud = DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE)
                           Mov_Instancias.Instancia     = TProIns.TP_Instancia
                           Mov_Instancias.Agencia       = TProIns.TP_Agencia
                           Mov_Instancias.Fec_Ingreso   = W_Fecha
                           Mov_Instancias.Hora_Ingreso  = TIME
                           Mov_Instancias.Usuario       = TProIns.TP_Usuario
                           Mov_Instancias.Estado        = NO.
                 END.
             END.
             ENABLE ALL WITH FRAME F_Creditos.
             DISABLE NomUsuario WITH FRAME F_Creditos.
             ENABLE ALL WITH FRAME F_Consulta.
             APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.
             W_AS = YES.
          END.
       END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Indicadores wWin 
PROCEDURE Buscar_Indicadores :
DEFINE BUFFER bfrCreditos FOR creditos.

DO WITH FRAME F_Solicitud:
    IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa
                                 AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAIL(Indicadores) THEN DO:
            MESSAGE "No exite un indicadores para el plazo, monto y linea"  SKIP
                    "de producto de crédito. Consulte con el Administrador" SKIP
                    "del sistema acerca de esta inconsistencia"
                VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".

            APPLY "ENTRY" TO Solicitud.Monto.

            RETURN NO-APPLY.
        END.

        IF Indicadores.FecVcto LT w_fecha THEN DO:
            MESSAGE "El indicador para este producto se encuentra Vencido" SKIP
                    "la tasa puede estar desactualizada. Consulte con" SKIP
                    "el administrador del sistema acerca de esta inconsistencia"
                VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".

            APPLY "ENTRY" TO Solicitud.Monto.

            RETURN NO-APPLY.
        END.

        NomIndicador:SCREEN-VALUE = Indicadores.Nombre.

        IF NOT Indicadores.Rangos THEN DO:
            IF Indicadores.Tasa EQ 0 THEN DO:
                MESSAGE "El indicador tiene tasa en 0. No se permite radicar la Solicitud"
                    VIEW-AS ALERT-BOX ERROR.

                APPLY "choose" TO Btn_Cancelar IN FRAME F_Creditos.
            END.

            Solicitud.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa).
        END.
        ELSE
            RUN Hallar_Rangos_Indicador.
    END.
    ELSE DO:
        IF Solicitud.Tasa:SCREEN-VALUE = "0" THEN DO:
            MESSAGE "EL producto de Credito permite que el asesor" SKIP
                    "entre la tasa para la solicitud." SKIP(1)
                    "Digite la Tasa para la solicitud en pantalla"
                VIEW-AS ALERT-BOX INFORMATION.

            ASSIGN Solicitud.Tasa:BGCOL = 15
                   Solicitud.Tasa:FGCOL = 0
                   Solicitud.Tasa:SENSITIVE = YES.

            APPLY "entry" TO Solicitud.Tasa IN FRAME F_Solicitud.

            RETURN NO-APPLY.
        END.
    END.

    /*IF pro_creditos.cod_credito = 17 OR pro_creditos.cod_credito = 22 OR pro_creditos.cod_credito = 27 THEN DO:
        FIND FIRST bfrCreditos WHERE bfrCreditos.nit = Solicitud.Nit:SCREEN-VALUE
                                 AND bfrCreditos.cod_credito = pro_creditos.cod_credito
                                 AND bfrcreditos.estado = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE bfrCreditos THEN DO:
            CASE pro_creditos.cod_credito:
                WHEN 17 THEN Solicitud.Tasa:SCREEN-VALUE = STRING(14.03).
                WHEN 22 THEN Solicitud.Tasa:SCREEN-VALUE = STRING(12.68).
                WHEN 27 THEN Solicitud.Tasa:SCREEN-VALUE = STRING(12.68).
            END CASE.
            
            Solicitud.Tasa:SENSITIVE = NO.
        END.
    END.*/

    RUN Hallar_Tasa_Nominal.
    RUN Hallar_Tasa_Periodo.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Instancias_Cerradas wWin 
PROCEDURE Buscar_Instancias_Cerradas :
/* FOR EACH TCerradas: DELETE TCerradas. END. */

EMPTY TEMP-TABLE TCerradas.

FOR EACH Mov_Instancias WHERE
        Mov_Instancias.Nit           EQ Solicitud.Nit AND
        Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud NO-LOCK:
    CREATE TCerradas.
    ASSIGN TCerradas.Instancia      = Mov_Instancias.Instancia
           TCerradas.Estado         = Mov_Instancias.Estado
           TCerradas.Num_Solicitud   = Mov_Instancias.Num_Solicitud
           TCerradas.Usuario        = Mov_Instancias.Usuario
           TCerradas.Fec_Ingreso    = Mov_Instancias.Fec_Ingreso
           TCerradas.Fec_Retiro     = Mov_Instancias.Fec_Retiro
           TCerradas.Hora_Ingreso   = Mov_Instancias.Hora_Ingreso
           TCerradas.Hora_Retiro    = Mov_Instancias.Hora_Retiro
           TCerradas.Descripcion    = Mov_Instancias.Descripcion.
    FIND FIRST Instancias WHERE Instancias.Instancia EQ Mov_Instancias.Instancia NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN 
        TCerradas.INom_Instancia = Instancias.Nom_Instancia.
    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE Usuario THEN 
        TCerradas.INom_Usuario = Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcularPlazo wWin 
PROCEDURE CalcularPlazo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)):
      WHEN 0 THEN ASSIGN Dias = Dias * 1
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 1)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 1)
                          W_NomPdo:SCREEN-VALUE = "Cuota Diaria".

       WHEN 1 THEN ASSIGN Dias = Dias * 7 
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 7)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 7)
                          W_NomPdo:SCREEN-VALUE = "Cuota semanal".
       WHEN 2 THEN ASSIGN Dias   = Dias * 10
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 10)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 10)
                          W_NomPdo:SCREEN-VALUE = "Cuota decadal" NO-ERROR.
       WHEN 3 THEN ASSIGN Dias   = Dias * 15
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 15)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 15)
                          W_NomPdo:SCREEN-VALUE = "Cuota quincenal" NO-ERROR.
       WHEN 4 THEN DO: 
            ASSIGN Dias   = Dias * 30
                   W_NomPdo:SCREEN-VALUE = "Cuota mensual" NO-ERROR.
            IF Cfg_Instancias.Plazo_Minimo GT 30 THEN PlaMinI = ROUND(Cfg_Instancias.Plazo_Minimo / 30,0).
                                                 ELSE PlaMaxI = 1.
            IF Cfg_Instancias.Plazo_Maximo GT 30 THEN PlaMaxI = ROUND(Cfg_Instancias.Plazo_Maximo / 30,0).
                                                 ELSE PlaMaxI = 1.
       END.
       WHEN 5 THEN ASSIGN Dias   = Dias * 60
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 60)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 60)
                          W_NomPdo:SCREEN-VALUE = "Cuota bimensual" NO-ERROR.
       WHEN 6 THEN ASSIGN Dias   = Dias * 90
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 90)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 90)
                          W_NomPdo:SCREEN-VALUE = "Cuota trimestral" NO-ERROR.
       WHEN 7 THEN ASSIGN Dias   = Dias * 120
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 120)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 120)
                          W_NomPdo:SCREEN-VALUE = "Cuota cuatrimestral" NO-ERROR.
       
       WHEN 8 THEN ASSIGN Dias   = Dias * 180
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 180)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 180)
                          W_NomPdo:SCREEN-VALUE = "Cuota Semestral" NO-ERROR.
       WHEN 9 THEN ASSIGN Dias   = Dias * 360
                          PlaMinI = ABS(Cfg_Instancias.Plazo_Minimo / 360)
                          PlaMaxI = ABS(Cfg_Instancias.Plazo_Maximo / 360)
                          W_NomPdo:SCREEN-VALUE = "Cuota Anual" NO-ERROR.

  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcular_Deducible wWin 
PROCEDURE Calcular_Deducible PRIVATE :
DEFI INPUT  PARAM P_CedNit   LIKE Creditos.Nit.
    DEFI INPUT  PARAM P_Linea    LIKE Pro_Creditos.Cod_Credito.
    DEFI INPUT  PARAM P_Monto    LIKE Creditos.Monto.
    DEFI INPUT  PARAM P_ForPag   LIKE Creditos.FOR_Pago.   
    DEFI OUTPUT PARAM Tot_Deduc  LIKE Solicitud.Monto.
    DEFI OUTPUT PARAM Tot_AjAho  LIKE Solicitud.Monto.
    DEFI OUTPUT PARAM Tot_FalApo LIKE Solicitud.Monto.    
    DEFI OUTPUT PARAM TOTWMpr    LIKE Solicitud.Monto.    

 MESSAGE "Ingreso a Clacular Deducible" SKIP
         "Paramentros " SKIP
         "P_CedNit    " P_CedNit  SKIP
         "P_Linea     " P_Linea   SKIP
         "P_Monto     " P_Monto   SKIP
         "P_ForPag    " P_ForPag  SKIP
         "Tot_Deduc   " Tot_Deduc SKIP
         "Tot_AjAho   " Tot_AjAho  SKIP
         "Tot_FalApo  " Tot_FalApo SKIP
         "TOTWMpr     " TOTWMpr    
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
 FOR EACH TDeducc WHERE TDeducc.Cod_Deducible NE ?:
    IF TDeducc.valor > 0 THEN
    CASE TDeducc.Cod_Deducible:
        WHEN "10" THEN DO: /* Seguro de Cartera    */ 
            IF P_Linea = 46  OR P_Linea = 5  OR P_Linea = 540 OR P_Linea = 541 OR P_Linea =  542 OR P_Linea = 10 OR
               P_Linea = 11  OR P_Linea = 12 OR P_Linea = 22 OR P_Linea = 524 OR P_Linea = 529 OR P_Linea =   55 OR P_Linea = 15 OR
               P_Linea = 20  OR P_Linea = 30 OR P_Linea = 35  OR P_Linea =  40 OR P_Linea =   45 OR P_Linea = 48 OR
               P_Linea = 49  OR P_Linea = 50 OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =   53 OR P_Linea = 526 OR
               P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR P_Linea = 65  OR P_Linea =   80 OR P_Linea = 85 OR
               P_Linea = 90  OR P_Linea = 95 OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523  OR 
               P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563  OR 
               P_Linea = 13  OR P_Linea = 14  THEN DO:
              Tot_Deduc = Tot_Deduc + ( P_Monto - (DECIMAL(W_TotCanc:SCREEN-VALUE IN FRAME f_CredAcanc) * TDeducc.Valor)).                                                              
            END.
        END.
       WHEN "16" THEN DO: /* Gasto de Papeleria */
           IF P_Linea = 46  OR P_Linea = 5  OR P_Linea = 540 OR P_Linea = 541 OR P_Linea =  542 OR P_Linea = 10 OR
              P_Linea = 11  OR P_Linea = 12 OR P_Linea = 22 OR P_Linea = 524 OR P_Linea = 529 OR P_Linea =   55 OR P_Linea = 15 OR
              P_Linea = 20  OR P_Linea = 30 OR P_Linea = 35  OR P_Linea =  40 OR P_Linea =   45 OR P_Linea = 48 OR
              P_Linea = 49  OR P_Linea = 50 OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =   53 OR P_Linea = 526 OR
              P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR P_Linea = 65  OR P_Linea =   80 OR P_Linea = 85 OR
              P_Linea = 90  OR P_Linea = 95 OR P_Linea = 521  OR P_Linea = 522 OR P_Linea = 523 OR 
              P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR 
              P_Linea = 13  OR P_Linea = 14  THEN DO:

             IF P_Monto LE (461500 * 10) THEN  /* Mayor A 10SM HASTA 30 SM (INDICADORES) = 4900 */
               Tot_Deduc = Tot_Deduc + 4900.                                                              
             ELSE
               IF P_Monto LE (461500 * 30) THEN
                  Tot_Deduc = Tot_Deduc + 9900.                                                                   
               ELSE  
                  Tot_Deduc = Tot_Deduc + 14900.
           END.
       END.
       WHEN "4" THEN DO: /* Gasto de Administracion */
           IF P_Linea = 5   OR P_Linea = 10 OR P_Linea = 11 OR P_Linea = 12 OR P_Linea = 22 OR P_Linea = 524 OR
              P_Linea = 529 OR P_Linea = 55 OR P_Linea = 15 OR P_Linea = 20 OR P_Linea =   30 OR 
              P_Linea = 35  OR P_Linea = 40 OR P_Linea = 45 OR P_Linea = 48 OR
              P_Linea = 49  OR P_Linea = 50 OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =   53 OR P_Linea = 526 OR
              P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60 OR P_Linea = 65  OR P_Linea =   80 OR P_Linea = 85 OR
              P_Linea = 90  OR P_Linea = 95 OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 OR 
              P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR
              P_Linea = 13  OR P_Linea = 14  THEN DO:
               IF P_Monto LE (461500 * 10) THEN  /* Mayor A 10SM HASTA 30 SM (INDICADORES) = 4900 */
                 Tot_Deduc = Tot_Deduc + 1000.                                                              
               ELSE
                 IF P_Monto LE (461500 * 30) THEN
                    Tot_Deduc = Tot_Deduc + 1500.                                                                   
                 ELSE  
                    Tot_Deduc = Tot_Deduc + 2000. /* IF P_Monto GE (461500 * 30) THEN   MAYOR 30 SM 14900 */
                    
           END.
       END.
       WHEN "2" THEN DO: /*  Delphy */
         IF  P_Linea = 25 OR  P_Linea = 5  OR  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 12 OR P_Linea = 22  OR P_Linea = 524 OR
             P_Linea = 529 OR P_Linea = 55 OR P_Linea =  15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
             P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
             P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
             P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
             P_Linea = 522 OR P_Linea = 523 OR 
             P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR 
             P_Linea = 13  OR P_Linea = 14 THEN DO:
             IF P_ForPag = 1 THEN 
                Tot_Deduc = Tot_Deduc + TDeducc.Valor.
         END.
       END.
       WHEN "5" THEN /* Sifin */
           IF  P_Linea = 25 OR  P_Linea = 5  OR  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 12 OR P_Linea = 22  OR P_Linea = 524 OR
               P_Linea = 529 OR P_Linea = 55 OR P_Linea =  15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
               P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
               P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
               P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
               P_Linea = 522 OR P_Linea = 523 OR 
               P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR 
               P_Linea = 13  OR P_Linea = 14 THEN DO:
               IF P_ForPag = 2 AND P_Monto GT (461500 * 30) THEN 
                  Tot_Deduc = Tot_Deduc + TDeducc.Valor.
           END.
     WHEN "15" THEN /* Cuota de Afiliacion */
         IF  P_Linea = 5  OR  P_Linea = 10 OR P_Linea = 11   OR P_Linea = 12 OR P_Linea = 22  OR P_Linea = 524 OR
             P_Linea = 529 OR P_Linea = 55 OR P_Linea = 15   OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
             P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48   OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
             P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526  OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
             P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
             P_Linea = 522 OR P_Linea = 523 OR 
             P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR 
             P_Linea = 13  OR P_Linea = 14 THEN DO:
             FIND FIRST creditos WHERE creditos.nit = P_CedNit NO-LOCK NO-ERROR.
             IF NOT AVAILABLE(creditos) THEN DO:
                FIND FIRST ahorros WHERE ahorros.nit = P_CedNit AND ahorros.tip_ahorro = 4 AND 
                     (ahorros.sdo_disponible + ahorros.sdo_canje) GT 0 NO-LOCK no-error.
                IF NOT AVAILABLE(ahorros) THEN
                   Tot_Deduc = Tot_Deduc + TDeducc.Valor.
             END.
       END.
        WHEN "6" THEN /* impuesto de timbre */
            IF  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 12 OR P_Linea = 22  OR P_Linea = 524 OR
                P_Linea = 529 OR P_Linea = 55 OR P_Linea = 15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
                P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
                P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
                P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
                P_Linea = 522 OR P_Linea = 523 OR 
                P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR 
                P_Linea = 13  OR P_Linea = 14 THEN DO:
                IF P_Monto GT 125844000 THEN
                   Tot_Deduc = Tot_Deduc + ROUND(P_Monto * TDeducc.Valor,0).     
            END.
        OTHERWISE DO:
            IF TDeducc.Id_Deducible EQ 0 THEN DO: /*financiados*/
                IF TDeducc.Cla_Deducible EQ 1 THEN DO: /*porcentaje*/
                   ASSIGN TOTWMpr = TOTWMpr + (P_Monto * TDeducc.Valor)
                          Tot_Deduc = Tot_Deduc + (P_Monto * TDeducc.Valor).               
                END.
                ELSE DO:
                    ASSIGN TOTWMpr = TOTWMpr + TDeducc.Valor
                           Tot_Deduc = Tot_Deduc + TDeducc.Valor.                
                END.
            END.
            ELSE DO: /*Descontados*/
                IF TDeducc.Cla_Deducible EQ 1 THEN /*porcentaje*/
                    ASSIGN TOTWMpr = TOTWMpr - (P_Monto * TDeducc.Valor)
                           Tot_Deduc = Tot_Deduc + (P_Monto * TDeducc.Valor).
                ELSE
                    ASSIGN TOTWMpr = TOTWMpr - TDeducc.Valor
                           Tot_Deduc = Tot_Deduc + TDeducc.Valor.
            END.        
        END.
    END CASE.
 END.
 ASSIGN Solicitud.Deducible:SCREEN-VALUE IN FRAME F_Solicitud = STRING(Tot_Deduc,">>,>>>,>>>,>>9").
 MESSAGE "Salida Clacular Deducible" SKIP
         "Paramentros " SKIP
         "P_CedNit    " P_CedNit  SKIP
         "P_Linea     " P_Linea   SKIP
         "P_Monto     " P_Monto   SKIP
         "P_ForPag    " P_ForPag  SKIP
         "Tot_Deduc   " Tot_Deduc SKIP
         "Tot_AjAho   " Tot_AjAho  SKIP
         "Tot_FalApo  " Tot_FalApo SKIP
         "TOTWMpr     " TOTWMpr    
          VIEW-AS ALERT-BOX INFO BUTTONS OK.


 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcular_Deduciblejj wWin 
PROCEDURE Calcular_Deduciblejj :
DEFI INPUT  PARAM P_CedNit   LIKE Creditos.Nit.
    DEFI INPUT  PARAM P_Linea    LIKE Pro_Creditos.Cod_Credito.
    DEFI INPUT  PARAM P_Monto    LIKE Creditos.Monto.
    DEFI INPUT  PARAM P_ForPag   LIKE Creditos.FOR_Pago.   
    DEFI OUTPUT PARAM Tot_Deduc  LIKE Solicitud.Monto INIT 0.
    DEFI OUTPUT PARAM Tot_AjAho  LIKE Solicitud.Monto INIT 0.
    DEFI OUTPUT PARAM Tot_FalApo LIKE Solicitud.Monto INIT 0.    
    DEFI OUTPUT PARAM TOTWMpr    LIKE Solicitud.Monto INIT 0.    

/*  MESSAGE "Ingreso a Clacular Deducible" SKIP */
/*          "Paramentros " SKIP                 */
/*          "P_CedNit    " P_CedNit  SKIP       */
/*          "P_Linea     " P_Linea   SKIP       */
/*          "P_Monto     " P_Monto   SKIP       */
/*          "P_ForPag    " P_ForPag  SKIP       */
/*          "Tot_Deduc   " Tot_Deduc SKIP       */
/*          "Tot_AjAho   " Tot_AjAho  SKIP      */
/*          "Tot_FalApo  " Tot_FalApo SKIP      */
/*          "TOTWMpr     " TOTWMpr              */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
 FOR EACH TDeducc WHERE TDeducc.Cod_Deducible NE ?:
    IF TDeducc.valor > 0 THEN
    CASE TDeducc.Cod_Deducible:
        WHEN "10" THEN DO: /* Seguro de Cartera    */ 
            IF P_Linea = 46  OR P_Linea = 5  OR P_Linea = 540 OR P_Linea = 541 OR P_Linea =  542 OR P_Linea = 10 OR
               P_Linea = 11  OR P_Linea = 22 OR P_Linea = 524 OR P_Linea = 529 OR P_Linea =   55 OR P_Linea = 15 OR
               P_Linea = 20  OR P_Linea = 30 OR P_Linea = 35  OR P_Linea =  40 OR P_Linea =   45 OR P_Linea = 48 OR
               P_Linea = 49  OR P_Linea = 50 OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =   53 OR P_Linea = 526 OR
               P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR P_Linea = 65  OR P_Linea =   80 OR P_Linea = 85 OR
               P_Linea = 90  OR P_Linea = 95 OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 THEN DO:
              
              Tot_Deduc = Tot_Deduc + ( P_Monto - (DECIMAL(W_TotCanc:SCREEN-VALUE IN FRAME f_CredAcanc) * TDeducc.Valor)).                                                              
            END.
        END.
       WHEN "16" THEN DO: /* Gasto de Papeleria */
           IF P_Linea = 46  OR P_Linea = 5  OR P_Linea = 540 OR P_Linea = 541 OR P_Linea =  542 OR P_Linea = 10 OR
              P_Linea = 11  OR P_Linea = 22 OR P_Linea = 524 OR P_Linea = 529 OR P_Linea =   55 OR P_Linea = 15 OR
              P_Linea = 20  OR P_Linea = 30 OR P_Linea = 35  OR P_Linea =  40 OR P_Linea =   45 OR P_Linea = 48 OR
              P_Linea = 49  OR P_Linea = 50 OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =   53 OR P_Linea = 526 OR
              P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR P_Linea = 65  OR P_Linea =   80 OR P_Linea = 85 OR
              P_Linea = 90  OR P_Linea = 95 OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 THEN DO:

             IF P_Monto LE (461500 * 10) THEN  /* Mayor A 10SM HASTA 30 SM (INDICADORES) = 4900 */
               Tot_Deduc = Tot_Deduc + 4900.                                                              
             ELSE
               IF P_Monto LE (461500 * 30) THEN
                  Tot_Deduc = Tot_Deduc + 9900.                                                                   
               ELSE  
                  Tot_Deduc = Tot_Deduc + 14900.
           END.
       END.
       WHEN "4" THEN DO: /* Gasto de Administracion */
           IF P_Linea = 5   OR P_Linea = 10 OR P_Linea = 11 OR P_Linea = 22 OR P_Linea = 524 OR
              P_Linea = 529 OR P_Linea = 55 OR P_Linea = 15 OR P_Linea = 20 OR P_Linea =   30 OR 
              P_Linea = 35  OR P_Linea = 40 OR P_Linea = 45 OR P_Linea = 48 OR
              P_Linea = 49  OR P_Linea = 50 OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =   53 OR P_Linea = 526 OR
              P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR P_Linea = 65  OR P_Linea =   80 OR P_Linea = 85 OR
              P_Linea = 90  OR P_Linea = 95 OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 THEN DO:
               IF P_Monto LE (461500 * 10) THEN  /* Mayor A 10SM HASTA 30 SM (INDICADORES) = 4900 */
                 Tot_Deduc = Tot_Deduc + 1000.                                                              
               ELSE
                 IF P_Monto LE (461500 * 30) THEN
                    Tot_Deduc = Tot_Deduc + 1500.                                                                   
                 ELSE  
                    Tot_Deduc = Tot_Deduc + 2000. /* IF P_Monto GE (461500 * 30) THEN   MAYOR 30 SM 14900 */
                    
           END.
       END.
       WHEN "2" THEN DO: /*  Delphy */
         IF  P_Linea = 25 OR  P_Linea = 5  OR  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 22  OR P_Linea = 524 OR
             P_Linea = 529 OR P_Linea = 55 OR P_Linea =  15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
             P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
             P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
             P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
             P_Linea = 522 OR P_Linea = 523 THEN DO:
             IF P_ForPag = 1 THEN 
                Tot_Deduc = Tot_Deduc + TDeducc.Valor.
         END.
       END.
       WHEN "5" THEN /* Sifin */
           IF  P_Linea = 25 OR  P_Linea = 5  OR  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 22  OR P_Linea = 524 OR
               P_Linea = 529 OR P_Linea = 55 OR P_Linea =  15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
               P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
               P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
               P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
               P_Linea = 522 OR P_Linea = 523 THEN DO:
               IF P_ForPag = 2 AND P_Monto GT (461500 * 30) THEN 
                  Tot_Deduc = Tot_Deduc + TDeducc.Valor.
           END.
     WHEN "15" THEN /* Cuota de Afiliacion */
         IF  P_Linea = 5  OR  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 22  OR P_Linea = 524 OR
             P_Linea = 529 OR P_Linea = 55 OR P_Linea = 15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
             P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
             P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
             P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
             P_Linea = 522 OR P_Linea = 523 THEN DO:
             FIND FIRST creditos WHERE creditos.nit = P_CedNit NO-LOCK NO-ERROR.
             IF NOT AVAILABLE(creditos) THEN DO:
                FIND FIRST ahorros WHERE ahorros.nit = P_CedNit AND ahorros.tip_ahorro = 4 AND 
                     (ahorros.sdo_disponible + ahorros.sdo_canje) GT 0 NO-LOCK no-error.
                IF NOT AVAILABLE(ahorros) THEN
                   Tot_Deduc = Tot_Deduc + TDeducc.Valor.
             END.
       END.
        WHEN "6" THEN /* impuesto de timbre */
            IF  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 22  OR P_Linea = 524 OR
                P_Linea = 529 OR P_Linea = 55 OR P_Linea = 15 OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
                P_Linea =  40 OR P_Linea = 45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
                P_Linea = 52  OR P_Linea = 53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
                P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90 OR P_Linea =  95 OR P_Linea = 521 OR
                P_Linea = 522 OR P_Linea = 523 THEN DO:
                IF P_Monto GT 125844000 THEN
                   Tot_Deduc = Tot_Deduc + ROUND(P_Monto * TDeducc.Valor,0).     
            END.
        OTHERWISE DO:
            IF TDeducc.Id_Deducible EQ 0 THEN DO: /*financiados*/
                IF TDeducc.Cla_Deducible EQ 1 THEN DO: /*porcentaje*/
                   ASSIGN TOTWMpr = TOTWMpr + (P_Monto * TDeducc.Valor)
                          Tot_Deduc = Tot_Deduc + (P_Monto * TDeducc.Valor).               
                END.
                ELSE DO:
                    ASSIGN TOTWMpr = TOTWMpr + TDeducc.Valor
                           Tot_Deduc = Tot_Deduc + TDeducc.Valor.                
                END.
            END.
            ELSE DO: /*Descontados*/
                IF TDeducc.Cla_Deducible EQ 1 THEN /*porcentaje*/
                    ASSIGN TOTWMpr = TOTWMpr - (P_Monto * TDeducc.Valor)
                           Tot_Deduc = Tot_Deduc + (P_Monto * TDeducc.Valor).
                ELSE
                    ASSIGN TOTWMpr = TOTWMpr - TDeducc.Valor
                           Tot_Deduc = Tot_Deduc + TDeducc.Valor.
            END.        
        END.
    END CASE.
 END.
 ASSIGN Solicitud.Deducible:SCREEN-VALUE IN FRAME F_Solicitud = STRING(Tot_Deduc,">>,>>>,>>>,>>9").
/*  MESSAGE "Salida Clacular Deducible" SKIP    */
/*          "Paramentros " SKIP                 */
/*          "P_CedNit    " P_CedNit  SKIP       */
/*          "P_Linea     " P_Linea   SKIP       */
/*          "P_Monto     " P_Monto   SKIP       */
/*          "P_ForPag    " P_ForPag  SKIP       */
/*          "Tot_Deduc   " Tot_Deduc SKIP       */
/*          "Tot_AjAho   " Tot_AjAho  SKIP      */
/*          "Tot_FalApo  " Tot_FalApo SKIP      */
/*          "TOTWMpr     " TOTWMpr              */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */


 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Condicionar wWin 
PROCEDURE Condicionar :
DEFINE VAR XINST LIKE INSTANCIAS.INSTANCIA.
ASSIGN XINST =  INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)). 
IF E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada EQ "" THEN DO:
    MESSAGE "Debe llenarse la condición para que" SKIP
            "el usuario que la reciba, pueda saber" SKIP
            "la acción a seguir. Rectifique!" VIEW-AS ALERT-BOX.
    RETURN.
 END.

 FIND FIRST Tuxi WHERE ROWID(Tuxi) EQ W_RowIdTx NO-ERROR.
 IF NOT AVAILABLE Tuxi THEN DO:
    MESSAGE "No se encuentra disponible el usuario" SKIP
            "para condicionar la solicitud." SKIP
            "No se permite la Operacion." VIEW-AS ALERT-BOX.    
    RETURN.
 END.

 FIND FIRST Mov_Instancias WHERE 
      Mov_Instancias.agencia      EQ Tuxi.Agencia AND
      Mov_Instancias.Instancia     EQ XINST AND
      Mov_Instancias.Usuario       EQ W_Usuario AND 
      Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud AND 
      Mov_Instancias.Nit           EQ Solicitud.Nit AND
      Mov_Instancias.Estado        EQ NO NO-ERROR.
 IF AVAILABLE Mov_Instancias THEN 
    ASSIGN Mov_Instancias.Fec_Retiro  = W_Fecha
           Mov_Instancias.Hora_Retiro = TIME
           Mov_Instancias.Estado      = YES
           Mov_Instancias.Descripcion = Mov_Instancias.Descripcion + 
            ". Fecha: " + STRING(W_Fecha) + " : " + E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada.
 ELSE DO:
    MESSAGE "No se encuentra el Mov_Instancias Actual para Cerrarla" VIEW-AS ALERT-BOX.
    RETURN. 
 END.

 FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.

 CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo       = 9 
        Hoja_Vida.Codigo     = 1  
        Hoja_Vida.Instancia  = Tuxi.Instanc
        Hoja_Vida.DoctoRefer = Solicitud.Num_Solicitud
        Hoja_Vida.Nit        = Solicitud.Nit
        Hoja_Vida.Usuario    = W_Usuario
        Hoja_Vida.Fec_Grabacion = W_Fecha
        Hoja_Vida.Hora_Grabacion = TIME
        Hoja_Vida.Observacion = 
        "Se Condiciona la aprobación: " + STRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos) +
        " - Procesada por Usuario: " + W_Usuario
        Hoja_Vida.Asunto_Cumplido = YES.
 /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
 CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo            = 9 
        Hoja_Vida.Codigo          = 1  
        Hoja_Vida.Instancia       = INTEGER(SUBSTRING(Cmb_InsCon:SCREEN-VALUE IN FRAME F_Condicionada,1,5))
        Hoja_Vida.DoctoRefer      = Solicitud.Num_Solicitud
        Hoja_Vida.Nit             = Solicitud.Nit
        Hoja_Vida.Usuario         = Tuxi.Usuario
        Hoja_Vida.Fec_Grabacion   = W_Fecha
        Hoja_Vida.Hora_Grabacion  = TIME
        Hoja_Vida.Asunto_Cumplido = NO
        Hoja_Vida.Observacion = "Condicion: " + E_Condicion:SCREEN-VALUE IN FRAME F_Condicionada 
                              + " - Usuario: " + W_Usuario.

 FIND FIRST Mov_Instancias WHERE
            Mov_Instancias.Instancia     EQ Tuxi.Instanc AND
            Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
            Mov_Instancias.Usuario       EQ Tuxi.Usuario AND
            Mov_instancias.Nit           EQ Solicitud.Nit NO-ERROR.
 IF NOT AVAILABLE Mov_Instancias THEN 
    CREATE Mov_Instancias.
 ASSIGN Mov_Instancias.Agencia       = Tuxi.Agencia
        Mov_Instancias.Fec_Ingreso   = W_Fecha
        Mov_Instancias.Hora_Ingreso  = TIME
        Mov_Instancias.Estado        = NO
        Mov_Instancias.Nit           = Solicitud.Nit
        Mov_Instancias.Num_Solicitud = Solicitud.Num_Solicitud
        Mov_Instancias.Usuario       = Tuxi.Usuario
        Mov_Instancias.Instancia     = Tuxi.Instanc
        Mov_Instancias.fec_retiro    = ?
        Mov_Instancias.Hora_retiro   = 0
        Mov_Instancias.estado        = NO.
        
 FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
 
 FIND CURRENT Solicitud NO-ERROR.
 Solicitud.Estado = 4.
 FIND CURRENT Solicitud NO-LOCK NO-ERROR.

 ENABLE ALL WITH FRAME F_Creditos.
 DISABLE NomUsuario Btn_Ingresar WITH FRAME F_Creditos.
 ENABLE ALL WITH FRAME F_Consulta.
 APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Consul_Gtias wWin 
PROCEDURE Consul_Gtias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OPEN QUERY Br_Admisible
       FOR EACH Garantias NO-LOCK WHERE
         Garantias.Agencia       EQ Solicitud.Agencia AND
         Garantias.Tip_Credito   EQ Solicitud.Tip_Credito AND
         Garantias.Cod_Credito   EQ Solicitud.Cod_Credito AND
         Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
         Garantias.Estado        EQ R_ConAdm  INDEXED-REPOSITION.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-proceso_Solicitud.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-proceso_Solicitud.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CpcdadPgoScoring wWin 
PROCEDURE CpcdadPgoScoring :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME F_VblesS:
        clientes.capacidad_pago:TOOLTIP         = ENTRY(1,c,CHR(1)).
        clientes.capacidad_pago:SCREEN-VALUE    = ENTRY(2,c,CHR(1)).
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
  DISPLAY Cmb_Instancias T_Refresh NomUsuario 
      WITH FRAME F_Creditos IN WINDOW wWin.
  ENABLE RECT-3 RECT-347 Cmb_Instancias BUTTON-169 Btn_Imprimir 
         Btn_ProcesarInstancia T_Refresh Btn_ImpCA Btn_Consulta Btn_Anexos 
         Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Cancelar BUTTON-9 BUTTON-2 
      WITH FRAME F_Creditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}
  ENABLE P_Foto BUTTON-230 
      WITH FRAME F_Foto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Foto}
  DISPLAY W_CredAval Nom_Aseguradora Nom_UsuGarantia W_DispGaran 
      WITH FRAME F_Admisible IN WINDOW wWin.
  IF AVAILABLE Garantias THEN 
    DISPLAY Garantias.Descripcion_Bien2 Garantias.Aprobada Garantias.Tipo_Garantia 
          Garantias.Estado Garantias.Identificacion_Bien Garantias.Nom_Bien 
          Garantias.Val_Bien Garantias.Fec_Creacion Garantias.Fec_Retiro 
          Garantias.Descripcion_Bien Garantias.Nro_Seguro 
          Garantias.Nit_Aseguradora Garantias.Fec_IniSeguro 
          Garantias.Fec_FinSeguro Garantias.Val_Asegurado Garantias.Nom_Impuesto 
          Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
          Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
          Garantias.Val_UltAvaluo 
      WITH FRAME F_Admisible IN WINDOW wWin.
  ENABLE Garantias.Descripcion_Bien2 Garantias.Aprobada Garantias.Tipo_Garantia 
         Garantias.Identificacion_Bien Garantias.Nom_Bien Garantias.Val_Bien 
         Garantias.Descripcion_Bien Garantias.Nro_Seguro 
         Garantias.Nit_Aseguradora Garantias.Fec_IniSeguro 
         Garantias.Fec_FinSeguro Garantias.Val_Asegurado Garantias.Nom_Impuesto 
         Garantias.Fec_VctoImpuesto Garantias.Val_Impuesto 
         Garantias.Fec_ProxAvaluo Garantias.Fec_UltAvaluo 
         Garantias.Val_UltAvaluo Btn_ConAdm BUTTON-161 Btn_SalAdm Btn_CanAdm 
         Btn_IngAdm Btn_InaAdm Btn_Borra RECT-293 RECT-294 RECT-295 RECT-296 
         RECT-299 RECT-301 
      WITH FRAME F_Admisible IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Admisible}
  DISPLAY W_NitCodeudor W_NomCodeudor RActivas 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  IF AVAILABLE Relaciones THEN 
    DISPLAY Relaciones.Aprobada 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  ENABLE RECT-297 Relaciones.Aprobada BUTTON-155 Btn_CreCod Btn_Activas 
         BUTTON-165 RActivas Br_Codeudores 
      WITH FRAME F_Codeudores IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Codeudores}
  DISPLAY R_ConAdm 
      WITH FRAME F_ConAdmisible IN WINDOW wWin.
  ENABLE BR_Admisible Btn_OutConAdm R_ConAdm 
      WITH FRAME F_ConAdmisible IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConAdmisible}
  ENABLE BUTTON-233 Btn_IrProcesarInst 
      WITH FRAME VisorSolicitud IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-VisorSolicitud}
  DISPLAY Cmb_Agencias NomNit Fec_Asociado Nom_Producto WX_Edad Nom_Destino 
          WAntiguedad WVeces Texto1 Texto2 WMonto Cmb_Sistemas W_TasaNominal 
          Cmb_PerPago W_TasaPeriodo NomIndicador W_ForPago W_Desembolso 
          W_VrCredACanc W_TotExt W_VrADesemb W_Tipo_Credito W_NomPdo 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  IF AVAILABLE Solicitud THEN 
    DISPLAY Solicitud.Num_Solicitud Solicitud.Fec_Solicitud Solicitud.Nit 
          Solicitud.DestinoF Solicitud.For_Interes Solicitud.Tasa 
          Solicitud.Monto Solicitud.Plazo Solicitud.fec_desembolso 
          Solicitud.fec_primerPago Solicitud.Cuota Solicitud.Deducible 
          Solicitud.Total_Prestamo 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  ENABLE RECT-322 RECT-323 RECT-325 RECT-327 RECT-330 RECT-331 RECT-333 
         RECT-334 BUTTON-19 BUTTON-103 Btn_Destino btnGrntias Btn_Gar 
         btnFrmlrioAfliacion Solicitud.fec_desembolso Cmb_PerPago Btn_Proyectar 
         Solicitud.fec_primerPago BUTTON-120 BUTTON-107 BUTTON-105 Btn_HojaVida 
         Btn_Extras BUTTON-102 Btn_CancCred Btn_Liquidar 
      WITH FRAME F_Solicitud IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Solicitud}
  DISPLAY R_Organizar Buscar VG_Normal VG_Media VG_Alta 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-223 RECT-287 RECT-288 RECT-289 Br_Consulta R_Organizar 
         Btn_OutConsulta Buscar 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY W_CptoScoring W_MenDes 
      WITH FRAME F_Ultima IN WINDOW wWin.
  IF AVAILABLE Solicitud THEN 
    DISPLAY Solicitud.Estado 
      WITH FRAME F_Ultima IN WINDOW wWin.
  ENABLE RECT-224 RECT-225 Btn_SalvaUltima Btn_OutUltima Solicitud.Estado 
      WITH FRAME F_Ultima IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Ultima}
  ENABLE Br_Cerradas Btn_OutCerradas BUTTON-154 
      WITH FRAME F_Cerradas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cerradas}
  DISPLAY cmbEstdoCrgo Rs_Ocupacion iDelphi W_VrCuota Nom_CteCodeu W_LiqDispon 
          Tot_Egresos Tot_Ingresos Cmb_destino Cmb_Gtia Cmb_MoraCial Cmb_ConCte 
          Cmb_TipAct Cmb_ResPat 
      WITH FRAME F_VblesS IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Sdo_Obligaciones Clientes.Capacidad_Pago Clientes.Salario 
          Clientes.Ing_arriendos Clientes.Ing_financieros 
          Clientes.Ing_Honorarios Clientes.Ing_Otros Clientes.Gto_Familiar 
          Clientes.Gto_Arriendo Clientes.Gto_obligacion Clientes.GtoFinanc_Indir 
      WITH FRAME F_VblesS IN WINDOW wWin.
  ENABLE cmbEstdoCrgo Rs_Ocupacion iDelphi Clientes.Sdo_Obligaciones 
         Clientes.Salario Clientes.Ing_arriendos Clientes.Ing_financieros 
         Clientes.Ing_Honorarios Clientes.Ing_Otros Clientes.Gto_Arriendo 
         Clientes.Gto_obligacion Clientes.GtoFinanc_Indir Cmb_destino Cmb_Gtia 
         Cmb_MoraCial Btn_FinVS Cmb_ConCte Cmb_TipAct Cmb_ResPat RECT-213 
         RECT-214 RECT-321 
      WITH FRAME F_VblesS IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_VblesS}
  DISPLAY M_Cliente Nom_CteCod W_Concepto Total_Puntaje 
      WITH FRAME F_Scoring IN WINDOW wWin.
  ENABLE BR_Scoring M_Cliente BUTTON-166 BUT-IMP-Scoring Btn_Ejecutar BUTTON-99 
      WITH FRAME F_Scoring IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Scoring}
  DISPLAY W_PPExtra W_VrExtra 
      WITH FRAME F_Extras IN WINDOW wWin.
  ENABLE Btn_AdExt W_PPExtra W_VrExtra Btn_EliExt BUTTON-170 Br_Extras 
      WITH FRAME F_Extras IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Extras}
  DISPLAY W_TotCanc 
      WITH FRAME F_CredACanc IN WINDOW wWin.
  ENABLE Br_Cred btnSalirAbonoCreditos 
      WITH FRAME F_CredACanc IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_CredACanc}
  DISPLAY Cmb_InsCon E_Condicion 
      WITH FRAME F_Condicionada IN WINDOW wWin.
  ENABLE Cmb_InsCon Br_Usuarios E_Condicion BUTTON-162 
      WITH FRAME F_Condicionada IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Condicionada}
  DISPLAY R_TipoGarantia 
      WITH FRAME F_Garantias IN WINDOW wWin.
  ENABLE RECT-226 R_TipoGarantia Btn_OutGarantias 
      WITH FRAME F_Garantias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Garantias}
  DISPLAY S_InfoCliente 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  ENABLE BUTTON-156 BUTTON-108 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoCliente}
  DISPLAY W_NomAgeDesembolso W_NomProDesembolso 
      WITH FRAME F_Desembolso IN WINDOW wWin.
  IF AVAILABLE Solicitud THEN 
    DISPLAY Solicitud.Desembolso Solicitud.Age_Desembolso Solicitud.Cod_Desembolso 
          Solicitud.Cue_Desembolso 
      WITH FRAME F_Desembolso IN WINDOW wWin.
  ENABLE Solicitud.Desembolso BUTTON-104 
      WITH FRAME F_Desembolso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Desembolso}
  DISPLAY WHora_Ingreso W_Instancia W_UsuarioInstancia Whora_Retiro Vigencia 
      WITH FRAME F_Instancias IN WINDOW wWin.
  IF AVAILABLE Mov_Instancias THEN 
    DISPLAY Mov_Instancias.Fec_Ingreso Mov_Instancias.Fec_Retiro 
          Mov_Instancias.Estado Mov_Instancias.Descripcion 
      WITH FRAME F_Instancias IN WINDOW wWin.
  ENABLE BUTTON-142 Mov_Instancias.Estado Btn_GraInstancia Btn_AgregarTxt 
         Btn_Imp Btn_insVolver 
      WITH FRAME F_Instancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Instancias}
  DISPLAY S_InfoProducto 
      WITH FRAME F_InfoProducto IN WINDOW wWin.
  ENABLE S_InfoProducto Btn_OutScoring 
      WITH FRAME F_InfoProducto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoProducto}
  DISPLAY W_NomAgeDebAutomatico W_NomCodDebAutomatico 
      WITH FRAME F_ForPago IN WINDOW wWin.
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.Cam_Cat1 
      WITH FRAME F_ForPago IN WINDOW wWin.
  IF AVAILABLE Solicitud THEN 
    DISPLAY Solicitud.For_Pago Solicitud.Age_DebAutomatico 
          Solicitud.Cod_DebAutomatico Solicitud.Cue_DebAutomatico 
      WITH FRAME F_ForPago IN WINDOW wWin.
  ENABLE Solicitud.For_Pago Anexos_Clientes.Cam_Cat1 BUTTON-106 
      WITH FRAME F_ForPago IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ForPago}
  ENABLE Br_Deducibles BUTTON-101 
      WITH FRAME F_Deducibles IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Deducibles}
  DISPLAY Cmb_Productos 
      WITH FRAME F_Producto IN WINDOW wWin.
  IF AVAILABLE Solicitud THEN 
    DISPLAY Solicitud.Tip_Credito 
      WITH FRAME F_Producto IN WINDOW wWin.
  ENABLE Solicitud.Tip_Credito Cmb_Productos Btn_OutProductos BUTTON-131 
      WITH FRAME F_Producto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Producto}
  DISPLAY E_Agregar 
      WITH FRAME F_Agregar IN WINDOW wWin.
  ENABLE E_Agregar BUTTON-153 
      WITH FRAME F_Agregar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Agregar}
  IF AVAILABLE Hoja_Vida THEN 
    DISPLAY Hoja_Vida.Fec_Grabacion Hoja_Vida.Asunto_Cumplido 
          Hoja_Vida.Observacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  ENABLE BUTTON-150 Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion Btn_SalvaHV 
         Btn_NvoHv BUTTON-152 BUTTON-149 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_HojaVida}
  ENABLE Br_ConHV Btn_OutConHV 
      WITH FRAME F_ConHV IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConHV}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE F_AdNad wWin 
PROCEDURE F_AdNad :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN FRAME F_Admisible:TITLE = "Garantias Admisibles".

    Garantias.Tipo_Garantia:ENABLE("Propiedad").
    Garantias.Tipo_Garantia:ENABLE("Prenda").
    Garantias.Tipo_Garantia:ENABLE("Inversión").
    Garantias.Tipo_Garantia:ENABLE("Aportes").
    Garantias.Tipo_Garantia:DISABLE("Seguro Deudor").
     Garantias.Tipo_Garantia:DISABLE("Otras").
    Garantias.Tipo_Garantia:DISABLE("Cdat-Contrac NoAd").

    IF R_TipoGarantia = 3 THEN DO:
       ASSIGN FRAME F_Admisible:TITLE = "Garantias No-Admisibles".

       Garantias.Tipo_Garantia:DISABLE("Propiedad").
       Garantias.Tipo_Garantia:DISABLE("Prenda").
       Garantias.Tipo_Garantia:DISABLE("Inversión").
       Garantias.Tipo_Garantia:ENABLE("Seguro Deudor").
       Garantias.Tipo_Garantia:ENABLE("Otras").
       Garantias.Tipo_Garantia:DISABLE("Aportes").
       Garantias.Tipo_Garantia:ENABLE("Cdat-Contrac NoAd").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Garantias_Imprimir wWin 
PROCEDURE Garantias_Imprimir :
/*codeudores a temporal de garantias*/
 DEFINE VARIABLE W_NomCode AS CHARACTER FORMAT "X(40)".
 DEFINE VARIABLE W AS INTEGER FORMAT "99" INITIAL 14.
 
 RUN TmpL (INPUT W, INPUT "").

 CREATE TmpI.
 ASSIGN TmpI.ILinea  = W
        TmpI.ITexto = "DEUDOR Y CODEUDORES                              ENDEUDAMIENTO       INGRESOS         EGRESOS     PUNTAJE".

 ASSIGN W_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
        W = W + 1.

 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 1
        CCTmpI.ITexto = "EDAD"
        CCTmpI.Deud   = STRING(Clientes.Edad).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 2
        CCTmpI.ITexto = "PERSONAS A CARGO"
        CCTmpI.Deud   = STRING(Clientes.Per_ACargo).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 3
        CCTmpI.ITexto = "TIPO ACTIVIDAD"
        CCTmpI.Deud   = STRING(Clientes.Tipo_Activid).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 4
        CCTmpI.ITexto = "TIPO CONTRATO"
        CCTmpI.Deud   = STRING(Clientes.Tip_Contrato).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 5
        CCTmpI.ITexto = "ENDEUD INDIRECTO"
        CCTmpI.Deud   = STRING(Clientes.Endeud_Indirecto).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 6
        CCTmpI.ITexto = "RESPALDO PATRIM."
        CCTmpI.Deud   = STRING(Clientes.Respaldo_Patrim).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 7
        CCTmpI.ITexto = "MORALIDAD COMERCIAL"
        CCTmpI.Deud   = STRING(Clientes.Mora_Comercial).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 8
        CCTmpI.ITexto = "ESTADO CIVIL"
        CCTmpI.Deud   = STRING( Clientes.Est_Civil).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 9
        CCTmpI.ITexto = "CONOCIMIENTO CLIENTE"
        CCTmpI.Deud   = STRING(Clientes.Conocimiento_Cliente).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 10
        CCTmpI.ITexto = "TIEMPO EN LA COOPERATIVA"
        CCTmpI.Deud   = STRING(ROUND((W_Fecha - Clientes.Fec_Ingreso) / 364,2)).     
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 11
        CCTmpI.ITexto = "GARANTÌA"
        CCTmpI.Deud   = STRING(Clientes.Garantia).
 CREATE CCTmpI.
 ASSIGN CCTmpI.ILinea = 12
        CCTmpI.ITexto = "TIEMPO EN LA EMPRESA"
        CCTmpI.Deud   = STRING(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 364,2)).
          
 CREATE CTmpI.                                                                                                                              
 ASSIGN CTmpI.ILinea = W                                                                                                                             
        CTmpI.ITexto = STRING(Clientes.Nit,"X(14)") + " - " + STRING(W_NomCode,"X(35)") + " Tel: " + STRING(Clientes.Tel_Residencia,"X(14)")          
        CTmpI.Ingr   = Clientes.Ing_Otros + Clientes.Ing_Honorarios + Clientes.Ing_financieros + Clientes.Ing_arriendos + Clientes.Salario           
        CTmpI.Egr    = Clientes.Gto_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Arriendo                                                       
        CTmpI.Puntos = Clientes.Puntaje                                                                                                              
        CTmpI.Endeu  = Clientes.Capacidad_Pago.    
 
 
 ASSIGN W_NroCodeu = 1.
 IF Solicitud.Estado NE 3 THEN DO:
     FOR EACH Relaciones WHERE
              Relaciones.Nit            EQ Creditos.Nit                   AND
              INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito           AND
              Relaciones.Clase_Producto EQ 2                              AND
              Relaciones.Cod_Producto   EQ Creditos.Cod_Credito           AND
              Relaciones.Cod_Relacion   EQ 11                             AND
              Relaciones.Estado         EQ 1  NO-LOCK:
          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN DO:
              W_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
              W = W + 1.
              CREATE CTmpI.
              ASSIGN CTmpI.ILinea = W
                     CTmpI.ITexto = STRING(Clientes.Nit,"X(14)") + " - " + STRING(W_NomCode,"X(35)") + " Tel: " + STRING(Clientes.Tel_Residencia,"X(14)") 
                     CTmpI.Ingr   = Clientes.Ing_Otros + Clientes.Ing_Honorarios + Clientes.Ing_financieros + Clientes.Ing_arriendos + Clientes.Salario  
                     CTmpI.Egr    = Clientes.Gto_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Arriendo
                     CTmpI.Puntos = Clientes.Puntaje        
                     CTmpI.Endeu  = Clientes.Capacidad_Pago.

              IF W_NroCodeu LE 3 THEN
                 RUN Vbles_XCodeud.  

              W_NroCodeu = W_NroCodeu  + 1.
          END.
     END.
 END.

 IF Solicitud.Estado EQ 3 THEN DO: /*los codeudores para la hoja de desicion salen en el informe por aca*/
     FOR EACH Relaciones WHERE
              Relaciones.Nit            EQ Solicitud.Nit                  AND
              INTEG(Relaciones.Cuenta)  EQ Solicitud.Num_Solicitud        AND
              Relaciones.Clase_Producto EQ 2                              AND
              Relaciones.Cod_Producto   EQ Solicitud.Cod_Credito           AND
              Relaciones.Cod_Relacion   EQ 11                             
              /*AND Relaciones.Estado EQ 1*/  NO-LOCK:
          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN DO:
              W_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
              W = W + 1.
              CREATE CTmpI.
              ASSIGN CTmpI.ILinea = W
                     CTmpI.ITexto = STRING(Clientes.Nit,"X(14)") + " - " + STRING(W_NomCode,"X(35)") + " Tel: " + STRING(Clientes.Tel_Residencia,"X(14)") 
                     CTmpI.Ingr   = Clientes.Ing_Otros + Clientes.Ing_Honorarios + Clientes.Ing_financieros + Clientes.Ing_arriendos + Clientes.Salario  
                     CTmpI.Egr    = Clientes.Gto_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Arriendo
                     CTmpI.Puntos = Clientes.Puntaje        
                     CTmpI.Endeu  = Clientes.Capacidad_Pago.

              IF W_NroCodeu LE 3 THEN
                 RUN Vbles_XCodeud.  

              W_NroCodeu = W_NroCodeu  + 1.
          END.
     END.
 END.

 /*
 /*ADMISIBLES a temporal de garantias           no x ahora en Informe*/
 DEFINE VAR W_NomGar AS CHARACTER FORMAT "X(15)".
 FIND FIRST Garantias WHERE 
            Garantias.Agencia     EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) AND
            Garantias.Tip_Credito EQ Solicitud.Tip_Credito AND
            Garantias.Cod_Credito EQ Solicitud.Cod_Credito AND
            Garantias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
            Garantias.Estado      EQ 1 AND
            Garantias.Fec_Retiro  EQ ? NO-LOCK NO-ERROR.
  IF AVAILABLE Garantias THEN DO:
    W = W + 1.
    RUN TmpL (INPUT W, INPUT "").
    W = W + 1.
    CREATE CTmpI.
    ASSIGN CTmpI.ILinea  = W
           CTmpI.ITexto = "GARANTIAS ADMISIBLES (Propiedades - Vehículos - Inversiones)".
     FOR EACH Garantias WHERE 
              Garantias.Agencia     EQ Solicitud.Agencia AND
              Garantias.Tip_Credito EQ Solicitud.Tip_Credito AND
              Garantias.Cod_Credito EQ Solicitud.Cod_Credito AND
              Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
              Garantias.Estado      EQ 1 AND
              Garantias.Fec_Retiro  EQ ? NO-LOCK:
        CASE Garantias.Tipo_Garantia:
          WHEN 1 THEN W_NomGar = "Propiedad".
          WHEN 2 THEN W_NomGar = "Vehículo".
          WHEN 3 THEN W_NomGar = "Inversion".
        END CASE.
        W = W + 1.
        CREATE CTmpI.
        ASSIGN CTmpI.ILinea  = W
               CTmpI.ITexto = STRING(W_NomGar,"X(10)") + " - " + STRING(Garantias.Nom_Bien,"X(35)") + "     Valor: $" + STRING(Garantias.Val_Bien,">>>,>>>,>>>,>>9").
     END.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Credito wWin 
PROCEDURE Grabar_Credito :
DEFINE VAR W_Cbte LIKE Comprobantes.Comprobante.
DEFINE VAR W_NumCbt LIKE Comprobantes.Secuencia.
DEFINE VAR SSistema LIKE Creditos.Sistema.

DO TRANSACTION:
    CREATE Hoja_Vida.
    ASSIGN Hoja_Vida.Tipo = 9
           Hoja_Vida.Codigo = 1
           Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
           Hoja_Vida.DoctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
           Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
           Hoja_Vida.Usuario = W_Usuario
           Hoja_Vida.Fec_Grabacion = w_fecha
           Hoja_Vida.Hora_Grabacion = TIME
           Hoja_Vida.Observacion = "Credito Aprobado por el usuario: " + W_Usuario + NomUsuario:SCREEN-VALUE IN FRAME F_Creditos
           Hoja_Vida.Asunto_Cumplido = YES.

    FIND CURRENT Solicitud NO-ERROR.

    ASSIGN Solicitud.Estado = 2
           Solicitud.Fec_Aprobacion = W_Fecha.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                AND Mov_Instancias.Nit EQ STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud)
                                AND Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
                                AND Mov_Instancias.Estado EQ NO NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN
        ASSIGN Mov_Instancias.Estado = YES
               Mov_Instancias.Fec_Retiro = W_Fecha
               Mov_Instancias.Hora_Retiro = TIME.
    ELSE DO:
        MESSAGE "No se encontro Mov_Instancias"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    SSistema = INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)).

    CREATE Creditos.
    ASSIGN Creditos.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
           Creditos.Fec_Aprobacion = W_Fecha
           Creditos.For_Interes = INTEGER(Solicitud.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud)
           Creditos.For_Pago = INTEGER(Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago)
           Creditos.Monto = DECIMAL(Solicitud.Monto)
           Creditos.Sistema = SSistema
           Creditos.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
           Creditos.Num_Solicitud = DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
           Creditos.Num_Credito = NEXT-VALUE(Sec_Creditos)
           Creditos.Per_Pago = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1))
           Creditos.Plazo = INTEGER(Solicitud.Plazo:SCREEN-VALUE IN FRAME F_Solicitud)
           Creditos.Cuota = DECIMAL(Solicitud.Cuota)
           Creditos.Sistema = Solicitud.Sistema
           Creditos.Tasa = DECIMAL(W_tasanominal:SCREEN-VALUE IN FRAME F_Solicitud)
           Creditos.Tip_Credito = INTEGER(Solicitud.Tip_Credito:SCREEN-VALUE IN FRAME F_Producto)
           Creditos.Cod_Credito = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
           Creditos.Usuario = W_Usuario
           Creditos.Desembolso = INTEGER(Solicitud.Desembolso:SCREEN-VALUE IN FRAME F_Desembolso)
           Creditos.Age_Desembolso = Solicitud.Age_Desembolso
           Creditos.Cod_Desembolso = Solicitud.Cod_Desembolso
           Creditos.Cue_Desembolso = Solicitud.Cue_Desembolso
           Creditos.Age_DebAutomatico = INTEGER(Solicitud.Age_DebAutomatico:SCREEN-VALUE IN FRAME F_ForPago)
           Creditos.Cod_DebAutomatico = INTEGER(Solicitud.Cod_DebAutomatico:SCREEN-VALUE IN FRAME F_ForPago)
           Creditos.Cue_DebAutomatico = STRING(Solicitud.Cue_DebAutomatico:SCREEN-VALUE IN FRAME F_ForPago)
           Creditos.Id_Adicionales = 2
           creditos.incremento = 0.

    Solicitud.Pagare = STRING(Creditos.Num_Credito).

    FIND CURRENT Solicitud NO-LOCK NO-ERROR.

    FOR EACH Garantias WHERE Garantias.Agencia EQ Creditos.Agencia
                         AND Garantias.Nit EQ Creditos.Nit
                         AND Garantias.Tip_Credito EQ Creditos.Tip_Credito
                         AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                         AND Garantias.Num_Credito EQ 0
                         AND Garantias.Num_Solicitud EQ Creditos.Num_Solicitud
                         AND Garantias.Estado EQ 1:
        Garantias.Num_Credito = Creditos.Num_Credito.
    END.

    FOR EACH Relaciones WHERE Relaciones.Nit EQ Creditos.Nit
                          AND INTEG(Relaciones.Cuenta) EQ Creditos.Num_Solicitud
                          AND Relaciones.Clase_Producto EQ 2
                          AND Relaciones.Cod_Producto EQ Creditos.Cod_Credito
                          AND Relaciones.Cod_Relacion EQ 11
                          AND Relaciones.Estado EQ 1:
        Relaciones.Cuenta = STRING(Creditos.Num_Credito).
    END.

    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar en la tabla de créditos" SKIP
                "consulte con el administrador"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    RUN Asignar_Primera_Credito NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    /*Contabilizacion*/
    FIND FIRST CortoLargo WHERE CortoLargo.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                            AND CortoLargo.Clase_Producto EQ 2
                            AND CortoLargo.Cod_Producto EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3))
                            AND CortoLargo.Cta_ContingenteDB NE ""
                            AND CortoLargo.Cta_ContingenteCR NE ""
                            AND CortoLargo.Comprobante NE 0 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CortoLargo THEN DO:
        MESSAGE "No se ha encontrado la configuración de Corto y largo" SKIP
                "o existe algun tipo de inconsistencia en su configuración" SKIP
                "Comunique esta inconsistencia al Administrador del Sistema"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
                              AND Comprobantes.Comprobante EQ CortoLargo.Comprobante NO-ERROR.
    IF NOT AVAILABLE Comprobantes THEN DO:
        MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
                "de la aprobación de la Solicitud. Rectifique con el Administrador!" SKIP
                "Agencia :" INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3)) SKIP
                "Comprobante :" CortoLargo.Comprobante
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
    ELSE DO:
        ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               W_Cbte = Comprobantes.Secuencia.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
               Mov_Contable.Comprobante = Comprobantes.Comprobante
               Mov_Contable.Cuenta = CortoLargo.Cta_ContingenteDB
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Cen_Costos = W_CenCosGral
               Mov_Contable.DB = DECIMAL(Solicitud.Monto)
               Mov_Contable.Comentario = "Aprobación de Solicitud"
               Mov_Contable.Usuario = W_Usuario
               Mov_Contable.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
               Mov_Contable.Doc_Referencia = Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud
               Mov_Contable.Num_Documento = W_Cbte
               Mov_Contable.Fec_Grabacion = w_fecha
               Mov_Contable.Hora = TIME NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error al contabilizar primera partida" SKIP
                    "consulte con el administrador"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))
               Mov_Contable.Comprobante = Comprobantes.Comprobante
               Mov_Contable.Cuenta = CortoLargo.Cta_ContingenteCR
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Cen_Costos = W_CenCosGral
               Mov_Contable.CR = DECIMAL(Solicitud.Monto)
               Mov_Contable.Comentario = "Aprobación de Solicitud"
               Mov_Contable.Nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
               Mov_Contable.Doc_Referencia = Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud
               Mov_Contable.Usuario = W_Usuario
               Mov_Contable.Num_Documento = W_Cbte
               Mov_Contable.Fec_Grabacion = w_fecha
               Mov_Contable.Hora = TIME NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error al contabilizar segunda partida" SKIP
                    "consulte con el administrador"
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.
END.

/*impresion de documentos*/
FIND FIRST Formatos WHERE Formatos.Agencia EQ Comprobantes.Agencia
                      AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato NO-LOCK NO-ERROR.
IF AVAILABLE(Formatos) THEN DO:
    /*RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.Comprobante,
                                     INPUT W_Cbte,
                                     INPUT W_Cbte,
                                     INPUT INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Solicitud,1,3))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al llamar la rutina de impresion" SKIP
                "consulte con el administrador!"
            VIEW-AS ALERT-BOX.

        RETURN ERROR.
    END.*/
END.
ELSE DO:
    MESSAGE "No se encuentra el formato de impresión" SKIP
            "consulte con el administrador!"
        VIEW-AS ALERT-BOX.

    RETURN ERROR.
END.

W_TipoInforme = "Informe".

RUN Informe_Imp.

FIND CURRENT Creditos NO-LOCK NO-ERROR.
FIND CURRENT Solicitud NO-LOCK NO-ERROR.
RELEASE Mov_Instancias.
RELEASE Hoja_Vida.

ENABLE ALL WITH FRAME F_Creditos.
DISABLE NomUsuario Btn_Ingresar WITH FRAME F_Creditos.
ENABLE ALL WITH FRAME F_Consulta.
APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Rangos_Indicador wWin 
PROCEDURE Hallar_Rangos_Indicador :
DO WITH FRAME F_Solicitud:
     DEF VAR wtotaportes AS DEC INITIAL 0.00.
     DEF VAR xfidel LIKE creditos.cod_credito.
  /* ASSIGN W_PunTos:SENSITIVE = FALSE W_PNegocia = 0.*/
     ASSIGN xfidel = INTEGER(SUBSTRING(Nom_producto:SCREEN-VALUE,1,3)).
     IF xfidel = 874 THEN xfidel = 574.
     IF xfidel NE 574 THEN
        FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador            EQ Indicador.Indicador 
                                   AND (DEC(Solicitud.Monto:SCREEN-VALUE) GE Ran_Intereses.Val_Inicial
                                   AND  DEC(Solicitud.Monto:SCREEN-VALUE) LE Ran_Intereses.Val_Final)
                                   AND (Dias                              GE Ran_Intereses.Pla_Inicial  
                                   AND  Dias                              LE Ran_Intereses.Pla_Final) 
                                   AND Ran_Interes.Estado                 EQ 1 NO-LOCK NO-ERROR.
     ELSE DO:
        FOR EACH ahorros NO-LOCK WHERE Ahorros.nit = solicitud.nit:SCREEN-VALUE
                             AND Ahorros.Tip_ahorro = 4 AND Ahorros.estado = 1:
            wtotaportes = wtotaportes + ahorros.sdo_disponible.
        END.
        IF wtotaportes LE 0 THEN DO:
           MESSAGE "Esta persona no tiene Aportes,  por tanto no puede tomar esta linea"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "Mouse-Select-Click" TO Solicitud.Tip_Credito IN FRAME F_Producto.
           RETURN NO-APPLY.
        END.
        FIND FIRST Ran_Intereses WHERE  Ran_Intereses.Indicador EQ Indicador.Indicador 
                                   AND (wtotaportes             GE Ran_Intereses.Val_Inicial
                                   AND  wtotaportes             LE Ran_Intereses.Val_Final)
                                   AND  Ran_Interes.Estado      EQ 1 NO-LOCK NO-ERROR.
/*      MESSAGE "Rango" SKIP "Apotes" wtotaportes SKIP "Puntos_Asoc" Puntos_Asoc SKIP */
/*              "Puntos" Puntos VIEW-AS ALERT-BOX INFO BUTTONS OK.                    */
     END.
     IF AVAIL(Ran_Intereses) THEN DO:
        IF W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud EQ "Nomina" OR 
           W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud EQ "Nomina Crecediario" OR
           W_ForPago:SCREEN-VALUE IN FRAME F_Solicitud EQ "Prima"
           THEN Solicitud.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa + Ran_Intereses.Puntos).
           ELSE Solicitud.Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa + Ran_Intereses.Puntos_Asoc).     /* Pago por Caja */
/*      MESSAGE "Tasa Plena (2)   : " Solicitud.Tasa:SCREEN-VALUE SKIP                           */
/*              "Tasa Preferencial: " DEC(Solicitud.Tasa:SCREEN-VALUE) - DEC(WTasa:SCREEN-VALUE) */
/*               VIEW-AS ALERT-BOX.                                                              */
        Solicitud.Tasa:SCREEN-VALUE = STRING(DEC(Solicitud.Tasa:SCREEN-VALUE) - DEC(WTasa:SCREEN-VALUE)).
     END.
     ELSE DO:
        IF xfidel NE 574 THEN DO:
           MESSAGE "El Indicador no tiene creado un rango que contemple" SKIP
                   "los valores de Plazo y/o Monto digitados por el usuario" SKIP(1)
                   "Consulte con el Administrador" VIEW-AS ALERT-BOX ERROR  TITLE "Configuración del Indicador".
           Solicitud.Plazo:SCREEN-VALUE IN FRAME F_Solicitud = "0".
           APPLY "entry" TO Solicitud.Monto IN FRAME F_Solicitud.
           RETURN NO-APPLY.
        END.
        ELSE DO:
           MESSAGE "El Indicador no tiene creado un  rango  que contemple" SKIP
                   "los valores de Aportes en ahorros: " wtotaportes  SKIP(0)
                    VIEW-AS ALERT-BOX ERROR  TITLE "Configuración del Indicador".
           APPLY "entry" TO Solicitud.Monto IN FRAME F_Solicitud.
           RETURN NO-APPLY.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Efectiva wWin 
PROCEDURE Hallar_Tasa_Efectiva :
DEF VAR tas_efe AS DEC.
DEF VAR Periodo AS INT FORMAT "999".
  DO WITH FRAME F_Solicitud:
     periodo = 12.
   /*  IF Solicitud.FOR_Interes:SCREEN-VALUE EQ "1"
        THEN RUN NVEF IN W_ManFin (INPUT(DEC(w_tasanominal:SCREEN-VALUE)/100),Periodo,OUTPUT tasa1).
        ELSE RUN NAEF IN W_ManFin (INPUT(DEC(w_tasanominal:SCREEN-VALUE)/100),Periodo,OUTPUT tasa1).*/
/*     ASSIGN Tas_efe                = ((tasa1 * Periodo) * 100)
            W_TasaNominal:SCREEN-VALUE = STRING(Tas_efe).*/
        RUN NVEF IN W_ManFin (INPUT(DEC(w_tasanominal:SCREEN-VALUE) / 100),Periodo,OUTPUT tasa1).
        solicitud.tasa:SCREEN-VALUE = string(tasa1 * 100).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Nominal wWin 
PROCEDURE Hallar_Tasa_Nominal :
DEF VAR Periodo AS INT FORMAT "999".
DEFI VAR PlazoW AS INTEG FORM "99".
DEFI VAR W_NroDias AS INTEG FORM "9999".
DEFI VAR P_NMeses AS DEC.
DEFI VAR P_NomPer AS CHAR FORMAT "X(15)".

DO WITH FRAME F_Solicitud:
    RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE,1,1)),
                                   INPUT  PlazoW,
                                   OUTPUT W_NroDias,
                                   OUTPUT P_NMeses,
                                   OUTPUT Periodo,
                                   OUTPUT P_NomPer).

    IF Solicitud.FOR_Interes:SCREEN-VALUE EQ "1" THEN
        RUN EFNV IN W_ManFin (INPUT(DEC(Solicitud.Tasa:SCREEN-VALUE) / 100),
                              INPUT Periodo,
                              OUTPUT Tas_Nominal).
    ELSE
        RUN EFNA IN W_ManFin (INPUT(DEC(Solicitud.Tasa:SCREEN-VALUE) / 100),
                              INPUT Periodo,
                              OUTPUT Tas_Nominal).

    ASSIGN Tas_Nominal = ((Tas_Nominal * Periodo) * 100)
           W_TasaNominal:SCREEN-VALUE = STRING(Tas_Nominal).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Periodo wWin 
PROCEDURE Hallar_Tasa_Periodo :
DEFINE VAR Tas_Periodo AS DECIMAL.

DO WITH FRAME F_Solicitud:
    RUN HallarTasPer IN W_ManFin (INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)),
                                  INPUT DECIMAL(Solicitud.Tasa:SCREEN-VALUE),
                                  INPUT INTEGER(Solicitud.FOR_Interes:SCREEN-VALUE),
                                  OUTPUT Tas_Periodo).

    /* Para los créditos Corto Plazo y Emergencia el periodo de liquidación es diario */
    IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) = 108 OR
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) = 113 OR
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) = 114 THEN DO:

        RUN HallarTasPer IN W_ManFin (INPUT 0, /* Periodo de liquidación diaria */
                                      INPUT DEC(Solicitud.Tasa:SCREEN-VALUE),
                                      INPUT INT(Solicitud.FOR_Interes:SCREEN-VALUE),
                                      OUTPUT Tas_Periodo).

        w_tasaNominal = tas_periodo * 36000.
        w_tasaNominal:SCREEN-VALUE = STRING(w_tasaNominal,">>9.999999").
    END.

    Tas_Periodo = (Tas_Periodo * 100).
    W_TasaPeriodo:SCREEN-VALUE = STRING(Tas_Periodo).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_CapPago wWin 
PROCEDURE Halla_CapPago :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  DEFI VAR W_GtosIndir LIKE Clientes.Gto_obligacion INIT 0.
  DEFI VAR W_GtoCoop   LIKE Clientes.Gto_obligacion INIT 0.
  DEFI VAR W_Capacidad_Pago   AS DECIMAL FORMAT "999.999" INIT 0.

  /*IF Clientes.Nit EQ Solicitud.Nit THEN*/
  /*W_GtoCoop = Solicitud.Cuota.*/
  CASE SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1):
    WHEN "1" THEN W_GtoCoop = Solicitud.Cuota * 4.3.
    WHEN "2" THEN W_GtoCoop = Solicitud.Cuota * 3.
    WHEN "3" THEN W_GtoCoop = Solicitud.Cuota * 2.
    WHEN "4" THEN W_GtoCoop = Solicitud.Cuota.
  END CASE.

  FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

  /*FOR EACH Relaciones WHERE Relaciones.Nit_Relacion    EQ Clientes.Nit    AND
                             Relaciones.Clase_Producto EQ 2                AND
                             Relaciones.Cod_Relacion   EQ 11               AND
                             Relaciones.Estado         EQ 1 NO-LOCK:
       FIND FIRST Creditos WHERE Creditos.Nit         EQ Relaciones.Nit
                             AND Creditos.Num_Credito EQ INTEG (Relaciones.Cuenta)
                             AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
       IF AVAIL(Creditos) THEN
          ASSIGN W_GtosIndir = W_GtosIndir + Creditos.Cuota.
  END.*/
    
  DO WITH FRAME F_VblesS. END.

 /* IF  Cfg_RegCredito.Agencia_Exigida EQ 11                 /*Enero 19/06 GAER*/
  AND Pro_Creditos.Tip_Credito       EQ 4 THEN DO:
      ASSIGN W_LiqDispon      = Tot_Ingresos - Tot_Egresos
             W_Capacidad_Pago = 0.

      IF W_LiqDispon GT Solicitud.Cuota * 2 THEN
         W_Capacidad_Pago = 40.
      ELSE IF W_LiqDispon EQ Solicitud.Cuota * 2 THEN
         W_Capacidad_Pago = 30.
      ELSE IF W_LiqDispon GT Solicitud.Cuota THEN
         W_Capacidad_Pago = 20.               
  END.*/
      
  ASSIGN /*W_Capacidad_Pago   = ((DEC(Clientes.Gto_Obligacion:SCREEN-VALUE) + W_GtoCoop) /                                          
                                   (DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +                      
                                   (DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) * .70) + (DEC(Clientes.Ing_Otros:SCREEN-VALUE) * .70) + 
                                    DEC(Clientes.Salario:SCREEN-VALUE)))  * 100*/
         W_Capacidad_Pago   = (Tot_Egresos / Tot_Ingresos)  * 100
         W_LiqDispon        = Tot_Ingresos - Tot_Egresos.

  ASSIGN Clientes.Capacidad_Pago:SCREEN-VALUE = STRING(W_Capacidad_Pago)
         W_LiqDispon:SCREEN-VALUE             = STRING(W_LiqDispon).
*/ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_DispGaran wWin 
PROCEDURE Halla_DispGaran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_CredAval = Solicitud.Monto.

  SESSION:SET-WAIT-STATE("General").

  FOR EACH Creditos WHERE Creditos.Nit         EQ Solicitud.Nit
                      AND Creditos.Estado EQ 2 NO-LOCK:
      FIND FIRST Garantias WHERE Garantias.Nit                     EQ Creditos.Nit
                           AND Garantias.Num_Credito               EQ Creditos.Num_Credito
                           AND trim(Garantias.Identificacion_Bien) EQ trim(W_NBien)
                           AND Garantias.Estado                    EQ 1
                           AND Garantias.Num_Solicitud             NE Solicitud.Num_Solicitud NO-LOCK NO-ERROR.
      IF AVAIL(Garantias) THEN
         ASSIGN W_CredAval = W_CredAval + Creditos.Sdo_capital. 
  END.

  SESSION:SET-WAIT-STATE("").

  ASSIGN W_CredAval:SCREEN-VALUE IN FRAME F_Admisible = STRING(W_CredAval)
         W_DispGaran                                  = DEC(Garantias.Val_Bien:SCREEN-VALUE) - W_CredAval
         W_DispGaran:SCREEN-VALUE                     = STRING(W_DispGaran).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Conceptos wWin 
PROCEDURE Imprimir_Conceptos :
DEFINE VAR i AS INTEGER.
DEFINE VAR Lin_Analista AS CHARACTER FORMAT "X(120)".

DISPLAY "--------------------------------------------------------------------------------------------------------------"
        "Conceptos emitidos en las Instancias" SKIP
        "--------------------------------------------------------------------------------------------------------------"
    WITH FRAME ftitins WIDTH 132 NO-LABELS USE-TEXT STREAM-IO NO-BOX.

FOR EACH Mov_Instancias WHERE Mov_Instancias.Nit = Solicitud.Nit
                          AND Mov_Instancias.Num_Solicitud = Solicitud.Num_Solicitud NO-LOCK BREAK BY Mov_Instancia.Instancia
                                                                                                   BY Mov_Instancia.Fec_Ingreso
                                                                                                   BY Hora_Ingreso:
    FIND FIRST Usuarios WHERE Usuarios.Usuario = Mov_Instancias.Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios THEN DO:
        FIND FIRST Instancias WHERE Instancias.Instancia = Mov_Instancias.Instancia
                                AND Instancias.Tipo_Instancia = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Instancias) THEN
            Lin_analista = " (" + Instancias.Nom_Instancia + ") :" + TRIM(Usuarios.Nombre).
    END.

    DISPLAY Lin_Analista SKIP
            mov_instancias.descripcion VIEW-AS EDITOR SIZE 105 BY 3 AT 3
        WITH FRAME fcode WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprmeScring wWin 
PROCEDURE ImprmeScring :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VAR W_sw          AS LOGICAL. 
  DEFI   VAR lista         AS CHAR FORM "X(35)".

  Lista = W_PathSpl + "Scoring-" + STRING(Solicitud.Num_solicitud).
  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Lista,INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.

  RUN _SetCurs.r ("WAIT").
  OUTPUT TO VALUE(Lista) NO-ECHO PAGED PAGE-SIZE 80.

  RUN InformeScring.

  OUTPUT CLOSE.        
  RUN _SetCurs.r ("ARROW").
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT Lista).
  ELSE                                                  
    IF W_Dispositivo = "I" THEN
       RUN adecomm/_osprint.r ( INPUT  ?, INPUT Lista,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
  /*IF W_Dispositivo <> "A" THEN
     OS-DELETE VALUE(Lista).*/

  IF W_Dispositivo = "E" THEN
     RUN Imprimir_Excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_MovInst wWin 
PROCEDURE Imp_MovInst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.

  DISPLAY SKIP (2)
          "Asociado  : "
          Solicitud.Nit
          " "
          NomNit:SCREEN-VALUE IN FRAME F_Solicitud  FORM "X(60)" SKIP(1)
          "Detalle de la Instancia : "  SKIP
          Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos FORM "X(40)" SKIP(1)
          "Usuario   : "
          Mov_Instancias.Usuario         FORM "X(8)"
          Usuarios.Nombre                FORM "X(35)"
          "          Estado        : "
          Mov_Instancias.Estado                     SKIP
          "Solicitud : "
          Mov_Instancias.Num_Solicitud
          "                                              Fecha Ingreso : "
          Mov_Instancias.Fec_Ingreso     FORM "99/99/9999"               SKIP
          "No.Credito: "
          Mov_Instancias.Cuenta
          "                                        Fecha Retiro  : "
          Mov_Instancias.Fec_Retiro      FORM "99/99/9999" SKIP(2)

          SUBSTRING(Mov_Instancias.Descrip,1,  100)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,101,200)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,201,300)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,301,400)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,401,500)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,501,600)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,601,700)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,701,800)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,801,900)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,901,990)        FORM "X(100)"
       WITH DOWN WIDTH 150 FRAME F_MInst NO-BOX NO-LABELS STREAM-IO USE-TEXT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informacion_Solicitud wWin 
PROCEDURE Informacion_Solicitud :
DEFINE VAR Linea AS CHARACTER FORMAT "X(1125)".
DEFINE VAR T_Plazo AS CHARACTER FORMAT "X(30)".
DEFINE VAR TDisp AS DECIMAL.
DEFINE VAR W_RowIdCte AS ROWID.
DEFINE VAR W_RowIdCodeu AS ROWID.
DEFINE VAR totalExtras AS DECIMAL.

FOR EACH extras WHERE extras.nit = clientes.nit
                  AND extras.num_solicitud = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-LOCK:
    totalExtras = totalExtras + extras.Vr_CuoExtra.
END.

FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.

DO WITH FRAME F_Solicitud:
    T_Plazo = Solicitud.Plazo:SCREEN-VALUE  + " (" + SUBSTRING(Cmb_PerPago:SCREEN-VALUE,5,15) + ")".

    Linea = clientes.nit + " - " + clientes.nombre + " " + clientes.apellido1 + " " + cliente.apellido2.
    RUN TmpL(INPUT 1,INPUT Linea).

    Linea = "=============================================DATOS GENERALES DE LA SOLICITUD===========================================".
    RUN TmpL (INPUT 2, INPUT Linea).

    Linea = "Agencia de Radicación       : " + STRING(Cmb_Agencias:SCREEN-VALUE,"X(30)") + "  Número de Solicitud         : " + STRING(Solicitud.Num_Solicitud:SCREEN-VALUE,"X(30)").
    RUN TmpL (INPUT 3, INPUT Linea).

    Linea = "Fecha de Radicación         : " + STRING(Solicitud.Fec_Solicitud:SCREEN-VALUE,"X(30)") + "  Producto de Crédito         : " + STRING(Nom_Producto:SCREEN-VALUE,"X(30)").
    RUN TmpL (INPUT 4, INPUT Linea).

    Linea = "Tipo de Producto            : " + STRING(TRIM(W_Tipo_Credito:SCREEN-VALUE),"X(30)") + "  Instancia Actual            : " + STRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,"X(30)").
    RUN TmpL (INPUT 5, INPUT Linea).

    Linea = "Usuario Actualmente Procesa : " + STRING(usuarios.nombre,"X(30)") + "  Forma de Pago de la Cuota   : " + STRING(W_ForPago:SCREEN-VALUE,"X(30)").
    RUN TmpL (INPUT 6, INPUT Linea).

    Linea = "Desembolso del Crédito      : " + STRING(W_Desembolso:SCREEN-VALUE,"X(30)") + "  Fecha de desembolso         : " + STRING(solicitud.fec_desembolso:SCREEN-VALUE).
    RUN TmpL (INPUT 7, INPUT Linea).

    Linea = "                                                              Fecha de primer pago        : " + STRING(solicitud.fec_primerPago:SCREEN-VALUE).
    RUN TmpL (INPUT 8, INPUT Linea).

   Linea = "=============================================DETALLE DE VALORES DE SOLICITUD===========================================".
   RUN TmpL (INPUT 9, INPUT Linea).

   Linea = "Monto a Prestar             : " + STRING(Solicitud.Monto:SCREEN-VALUE,"X(30)") + "  Tasa Efectiva Anual         : " + STRING(Solicitud.Tasa:SCREEN-VALUE,"X(30)").
   RUN TmpL (INPUT 10, INPUT Linea).

   Linea = "Plazo                       : " + STRING(T_Plazo,"X(30)") + "  Tasa Nomina Anual           : " + STRING(W_TasaNominal:SCREEN-VALUE,"X(30)").
   RUN TmpL (INPUT 11, INPUT Linea).         

   Linea = "Cuota del Período           : " + STRING(Solicitud.Cuota:SCREEN-VALUE,"X(30)") + "  Tasa del Período            : " + STRING(W_TasaPeriodo:SCREEN-VALUE,"X(30)").
   RUN TmpL (INPUT 12, INPUT Linea).

   Linea = "Valor cuotas extra          : " + STRING(TRIM(STRING(totalExtras,"$>>>,>>>,>>>.99")),"X(30)") + "  Abono a créditos            : " + STRING(W_TotCanc,">,>>>,>>>,>>9.99").
   RUN TmpL (INPUT 13, INPUT Linea).

   Linea = "Valor del Incremento        : " + STRING("0","X(30)") + "  Pago de Valor a Deducir     : Descontados".
   RUN TmpL (INPUT 14, INPUT Linea).

   Linea = "Valor Neto a Entregar       : " + STRING(W_VrADesemb:SCREEN-VALUE,"X(30)") + "  Destino                     : " + nom_destino:SCREEN-VALUE.
   RUN TmpL (INPUT 15, INPUT Linea).
   
   ASSIGN W_RowIdCte = ROWID(Clientes).

   FIND FIRST Relaciones WHERE Relaciones.Nit = Solicitud.Nit
                           AND (Relaciones.Cuenta = STRING(Solicitud.Num_Solicitud,"99999999") OR Relaciones.Cuenta = STRING(Creditos.Num_Credito))
                           AND Relaciones.Clase_Producto = 2
                           AND Relaciones.Cod_Producto = Solicitud.Cod_Credito
                           AND Relaciones.Cod_relacion = 11
                           AND Relaciones.Estado = 1 NO-LOCK NO-ERROR.
   IF AVAILABLE(Relaciones) THEN
       FIND FIRST Clientes WHERE Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.

   IF AVAILABLE(Relaciones) AND AVAILABLE(Clientes) THEN DO:
       Linea = "".
       RUN TmpL (INPUT 16, INPUT Linea).

       Linea = "=============================================INFORMACION DEL CODEUDOR==================================================".
       RUN TmpL (INPUT 20, INPUT Linea).

       Linea = Clientes.Nit + "-" + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2 + " (" + STRING((W_Fecha - Clientes.Fec_Nacimiento) / 360,"99") + " Años)".
       RUN TmpL (INPUT 21, INPUT Linea).

       Linea = "                                                              " + "Deudas DataCrédito          : " + STRING(STRING(Clientes.Sdo_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").

       RUN TmpL (INPUT 22, INPUT Linea).

       Linea = "Ingreso x Salarios          : " + STRING(STRING(Clientes.Salario,">>>,>>>,>>>,>>9"),"x(30)") + "  Deducciones de Colilla      : " + STRING(STRING(Clientes.Gto_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
       RUN TmpL (INPUT 23, INPUT Linea).

       Linea = "Ingreso x Arriendos         : " + STRING(STRING(Clientes.Ing_Arriendos,">>>,>>>,>>>,>>9"),"x(30)") + "  Gastos Familiares           : " + STRING(STRING(Clientes.Gto_Familiar,">>>,>>>,>>>,>>9"),"X(30)").
       RUN TmpL (INPUT 24, INPUT Linea).

       Linea = "Ingresos Financieros        : " + STRING(STRING(Clientes.Ing_Financieros,">>>,>>>,>>>,>>9"),"x(30)") + "  Gastos Fin. Indirectos      : " + STRING(STRING(Clientes.GtoFinanc_Indir,">>>,>>>,>>>,>>9"),"X(30)").
       RUN TmpL (INPUT 25, INPUT Linea).

       Linea = "Otros Ingresos              : " + STRING(STRING(Clientes.Ing_Otros,">>>,>>>,>>>,>>9"),"x(30)") + "  Gastos Arriendo             : " + STRING(STRING(Clientes.Gto_Arriendo,">>>,>>>,>>>,>>9"),"X(30)").
       RUN TmpL (INPUT 26, INPUT Linea).

       Linea = "Total Ingresos              : " + STRING(STRING(Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Arriendos + Clientes.Salario,">>>,>>>,>>>,>>9"),"x(30)")+ "  Total Gastos                : " + STRING(STRING(Clientes.Gto_Arriendo + Clientes.Sdo_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
       RUN TmpL (INPUT 27, INPUT Linea).

       ASSIGN Tdisp = Clientes.Ing_Otros +
                      Clientes.Ing_Financieros +
                      Clientes.Ing_Arriendos +
                      Clientes.Salario -
                      (Clientes.Gto_Arriendo + Clientes.Sdo_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Obligacion)
              Linea = "Porcentaje de Endeudamiento :        " + STRING(STRING(Clientes.Capacidad_Pago,"%-999.99"),"X(30)") + "  Total Disponible            : " + STRING(STRING(Tdisp,">>>,>>>,>>>,>>9"),"X(30)").
       RUN TmpL (INPUT 28, INPUT Linea).

       ASSIGN W_RowIdCodeu = ROWID(Relaciones).

       FIND FIRST Relaciones WHERE Relaciones.Nit = Solicitud.Nit
                               AND (Relaciones.Cuenta = STRING(Creditos.Num_Credito) OR Relaciones.Cuenta = STRING(Solicitud.Num_Solicitud,"99999999"))
                               AND Relaciones.Clase_Producto = 2
                               AND Relaciones.Cod_Producto = Solicitud.Cod_Credito
                               AND Relaciones.Cod_relacion = 11
                               AND Relaciones.Estado = 1
                               AND ROWID(Relaciones) <> W_RowIdCodeu NO-LOCK NO-ERROR.
       IF AVAILABLE(Relaciones) THEN
           FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.

       IF AVAILABLE(Relaciones) AND AVAILABLE(Clientes) THEN
           RUN Info_SoliCodeu.
   END.

   FIND FIRST Clientes WHERE ROWID(Clientes) EQ W_RowidCte NO-LOCK NO-ERROR.

   Linea = "=============================================ANALISIS DEL CREDITO======================================================".
   RUN TmpL (INPUT 36, INPUT Linea).

   Linea = "                                                              " + "Deudas DataCrédito          : " + STRING(STRING(Clientes.Sdo_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 37, INPUT Linea).

   Linea = "Ingreso x Salarios          : " + STRING(STRING(Clientes.Salario,">>>,>>>,>>>,>>9"),"x(30)") + "  Deducciones de Colilla      : " + STRING(STRING(Clientes.Gto_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 38, INPUT Linea).

   Linea = "Ingreso x Arriendos         : " + STRING(STRING(Clientes.Ing_Arriendos,">>>,>>>,>>>,>>9"),"x(30)") + "  Gastos Familiares           : " + STRING(STRING(Clientes.Gto_Familiar,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 39, INPUT Linea).

   Linea = "Ingresos Financieros        : " + STRING(STRING(Clientes.Ing_Financieros,">>>,>>>,>>>,>>9"),"x(30)") + "  Gastos Fin. Indirectos      : " + STRING(STRING(Clientes.GtoFinanc_Indir,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 40, INPUT Linea).

   Linea = "Otros Ingresos              : " + STRING(STRING(Clientes.Ing_Otros,">>>,>>>,>>>,>>9"),"x(30)") + "  Gastos Arriendo             : " + STRING(STRING(Clientes.Gto_Arriendo,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 41, INPUT Linea).

   Linea = "Total Ingresos              : " + STRING(STRING(Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Arriendos + Clientes.Salario,">>>,>>>,>>>,>>9"),"x(30)") + "  Total Gastos                : " + STRING(STRING(Clientes.Gto_Arriendo + Clientes.Sdo_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 42, INPUT Linea).

   ASSIGN Tdisp = Clientes.Ing_Otros +
                  Clientes.Ing_Financieros +
                  Clientes.Ing_Arriendos +
                  Clientes.Salario -
                  (Clientes.Gto_Arriendo + Clientes.Sdo_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Obligacion)
          Linea = "Porcentaje de Endeudamiento :        " + STRING(STRING(Clientes.Capacidad_Pago,"%-999.99"),"X(25)") + "Total Disponible            : " + STRING(STRING(Tdisp,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 43, INPUT Linea).

   Linea = "                                                              Total Cuota a Pagar $               " + STRING(Solicitud.Cuota:SCREEN-VALUE,"X(30)"). 
   RUN TmpL (INPUT 44, INPUT Linea).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe wWin 
PROCEDURE Informe :
{Incluido\RepEncabezado.i}

DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
DEFINE VAR W_AproNeg AS CHARACTER FORMAT "X(30)" INITIAL "E N  E S T U D I O".
DEFINE VAR W_Primero AS LOGICAL.

CASE solicitud.estado:
    WHEN 2 THEN W_AproNeg = "V E R I F I C A D A".
    WHEN 3 THEN W_AproNeg = "N  E  G  A  D  A".
END CASE.

ASSIGN W_Cliente = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud + " - " + NomNit:SCREEN-VALUE IN FRAME F_Solicitud
       W_Reporte = "REPORTE   : SOLICITUD DE CREDITO....CONCEPTO FINAL : " +  STRING(W_AproNeg,"X(20)") + "       FECHA: " + STRING(w_fecha) + " - " + STRING(TIME,"hh:mm am")
       W_EncColumna = "Cliente Solicitante         :   " + W_Cliente + " (" + STRING(Wk_edad,"99") + " Años)".

VIEW FRAME F-Encabezado.

EMPTY TEMP-TABLE TmpI.
EMPTY TEMP-TABLE CTmpI.
EMPTY TEMP-TABLE CCTmpI.

RUN Informacion_Solicitud.

FOR EACH TmpI BREAK BY TmpI.ILinea:
    DISPLAY TmpI.ITexto SKIP(0) WITH FRAME FINFO WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
END.

RUN Imprimir_Conceptos.
fAhrros(Solicitud.Nit).
fCrdtos(Solicitud.Nit).

DISPLAY SKIP(5)
        "Crédito y Cartera:__________________"      AT 2
        "Gerente Financiero:__________________" AT 40
        "Analista:__________________"        AT 79 SKIP
    WITH WIDTH 150 FRAME F-Totales NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT STREAM-IO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe3 wWin 
PROCEDURE Informe3 :
{Incluido\RepEncabezado.i}
    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    
    ASSIGN W_Cliente = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud + " - " + NomNit:SCREEN-VALUE IN FRAME F_Solicitud.
 
    W_Reporte   = "REPORTE   : SOLICITUD DE CREDITO - FECHA: " + STRING(w_fecha) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.


   DISPLAY 
  /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
     "=============================================DATOS GENERALES DE LA SOLICITUD==============================================" AT 1
     "Agencia de Radicación       : " AT 1
     Cmb_Agencias:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Número de Solicitud         : " AT 1
     STRING(Solicitud.Num_Solicitud:SCREEN-VALUE) AT 33
     "Fecha de Radicación         : " AT 65
     Solicitud.Fec_Solicitud:SCREEN-VALUE  AT 98  FORMAT "X(10)"
     "Producto de Crédito         : " AT 1
     Nom_Producto:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Tipo de Producto            : " AT 65
     TRIM(W_Tipo_Credito:SCREEN-VALUE) AT 98 FORMAT "X(30)"
     "Instancia Actual            : " AT 1
     Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos AT 33  FORMAT "X(30)"
     "Usuario Actualmente Procesa : " AT 65
     NomUsuario:SCREEN-VALUE IN FRAME F_Creditos AT 98  FORMAT "X(30)" 
     "Forma de Pago de la Cuota   : " AT 1
     W_ForPago:SCREEN-VALUE           AT 33 FORMAT "X(30)"
     "Desembolso del Crédito      : " AT 65
     W_Desembolso:SCREEN-VALUE        AT 98 FORMAT "X(30)" SKIP(1)
     "=============================================DETALLE DE VALORES DE SOLICITUD==============================================" AT 1
     "Monto a Prestar             : " AT 1
     Solicitud.Monto:SCREEN-VALUE     AT 33  FORMAT "X(30)"
     "Tasa Efectiva Anual         : " AT 65
     Solicitud.Tasa:SCREEN-VALUE      AT 98  FORMAT "X(30)"
     "Plazo                       : " AT 1
     T_Plazo                          AT 33  FORMAT "X(30)"
     "Tasa Nomina Anual           : " AT 65
     W_TasaNominal:SCREEN-VALUE       AT 98 FORMAT "X(30)"
     "Cuota del Período           : " AT 1
     Solicitud.Cuota:SCREEN-VALUE     AT 33 FORMAT "X(30)"
     "Tasa del Período            : " AT 65
     W_TasaPeriodo:SCREEN-VALUE       AT 98 FORMAT "X(30)"
     "Valor a Deducir             : " AT 1
     Solicitud.Deducible:SCREEN-VALUE AT 33 FORMAT "X(30)"
     "Pago de Valor a Deducir     : " AT 65
     T_Dedu                           AT 98 FORMAT "X(30)"
     "Valor del Incremento        : " AT 1
     "0" AT 33 FORMAT "X(30)" SKIP(1)
     "TOTAL A PRESTAR             : " AT 1
     Solicitud.TOTAL_Prestamo:SCREEN-VALUE AT 33 FORMAT "X(30)" SKIP(1)
     "==============================================DATOS COMPLEMENTARIOS=================================================" AT 1
   WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 /*impresion de codeudores*/
 DEFINE VARIABLE W_NomCode AS CHARACTER FORMAT "X(40)".
 FIND FIRST Relaciones WHERE
            Relaciones.Nit EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud  AND
            Relaciones.Cuenta EQ Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud AND
            Relaciones.Clase_Producto EQ 2 AND
            Relaciones.Cod_Producto EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) AND
            Relaciones.Cod_Relacion EQ 11 NO-LOCK NO-ERROR.
 IF AVAILABLE Relaciones THEN DO:
    DISPLAY SKIP(1)
            "CODEUDORES<<<<<<<<<<<<<<<<<<" AT 1
          /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
            "Nit           Nombre                                     Tel.Residencia  Tel.Comercial" AT 1
            WITH FRAME F_EncCode WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
    FOR EACH Relaciones WHERE
             Relaciones.Nit EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud  AND
             Relaciones.Cuenta EQ Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud AND
             Relaciones.Clase_Producto EQ 2 AND
             Relaciones.Cod_Producto EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) AND
             Relaciones.Cod_Relacion EQ 11 NO-LOCK:
       FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN DO:
          W_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          DISPLAY Clientes.Nit       AT 1
                  W_NomCode          AT 15 FORMAT "X(40)"
                  Clientes.Tel_Residencia AT 58
                  Clientes.Tel_Comercial  AT 80
          WITH FRAME F_Code2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO NO-UNDERLINE.
       END.
    END.
 END.
 /*IMPRESION DE GARANTIAS ADMISIBLES*/
 DEFINE VAR W_NomGar AS CHARACTER FORMAT "X(15)".
 FIND FIRST Garantias WHERE 
            Garantias.Agencia     EQ Solicitud.Agencia AND
            Garantias.Tip_Credito EQ Solicitud.Tip_Credito AND
            Garantias.Cod_Credito EQ Solicitud.Cod_Credito AND
            Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
            Garantias.Estado      EQ 1 AND
            Garantias.Fec_Retiro  EQ ? NO-LOCK NO-ERROR.
  IF AVAILABLE Garantias THEN DO:
     DISPLAY SKIP(1)
       "GARANTIAS ADMISIBLES <<<<<<<" AT 1
     /*           1         2         3         4         5         6         7         8
         1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        "Tipo Garantía  Descripción                                        Valor de la Garantía" AT 1
     WITH FRAME F_EncgAR WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
     FOR EACH Garantias WHERE 
              Garantias.Agencia     EQ Solicitud.Agencia AND
              Garantias.Tip_Credito EQ Solicitud.Tip_Credito AND
              Garantias.Cod_Credito EQ Solicitud.Cod_Credito AND
              Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
              Garantias.Estado      EQ 1 AND
              Garantias.Fec_Retiro  EQ ? NO-LOCK:
        CASE Garantias.Tipo_Garantia:
          WHEN 1 THEN W_NomGar = "Propiedad".
          WHEN 2 THEN W_NomGar = "Vehículo".
          WHEN 3 THEN W_NomGar = "Inversion".
        END CASE.
        DISPLAY W_NomGar   AT 1
                Garantias.Nom_Bien AT 16 FORMAT "X(50)"
                Garantias.Val_Bien AT 72
        WITH FRAME F_Gar WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
     END.
  END.
  
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InformeScring wWin 
PROCEDURE InformeScring :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR deSubtISD AS DECIMAL NO-UNDO.
    DEF VAR deCpcdadPgo AS DECIMAL NO-UNDO.
    DEF VAR deGrntia AS DECIMAL NO-UNDO.
    DEF VAR deMraCmrcial AS DECIMAL NO-UNDO.
    DEF VAR cLsta AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        cl = "ANALISIS DEL SCORE. Créditos Por Nómina".
        PUT UNFORMATTED cl SKIP.
        PUT SKIP(2).

        cl = "".
        overlay(cl,1)   = "FECHA".
        OVERLAY(cl,40)  = ffchaltras(bdcentral.Solicitud.Fec_Solicitud).
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,1)   = "NOMBRE DEL ASOCIADO".
        OVERLAY(cl,40)  = upper(NomNit:SCREEN-VALUE IN FRAME F_Solicitud).
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,1)   = "IDENTIFICACION".
        OVERLAY(cl,40)  = solicitud.nit:SCREEN-VALUE IN FRAME F_Solicitud.
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,1)   = "MONTO".
        OVERLAY(cl,40)  = solicitud.monto:SCREEN-VALUE IN FRAME F_Solicitud.
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,1)   = "PLAZO".
        OVERLAY(cl,40)  = solicitud.plazo:SCREEN-VALUE IN FRAME F_Solicitud.
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,1)   = "LINEA".
        OVERLAY(cl,40)  = upper(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud).
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,1)   = CHR(10) + "INFORMACION SOCIODEMOGRAFICA".
        PUT UNFORMATTED cl SKIP.

        cl = "".
        overlay(cl,11)   = "VARIABLE".
        overlay(cl,45)   = "DATOS".
        overlay(cl,70)   = "PUNTAJE".
        PUT UNFORMATTED cl SKIP.

        clsta = "!Capacidad_Pago,!garantia,!Mora_Comercial,*".
        deSubtISD = 0.
        FOR EACH tscoring
            WHERE
                CAN-DO(cLsta,tscoring.vars):
            cl = "".
            OVERLAY(cl,1) = upper(replace(tscoring.vars,"_"," ")).
            OVERLAY(cl,40) = tscoring.vvas.
            OVERLAY(cl,65) = string(tscoring.puns,"zz9.9").
            PUT UNFORMATTED cl SKIP.
            deSubtISD = deSubtISD + tscoring.puns.
        END.

        cl = "".
        OVERLAY(cl,1) = "SUBTOTAL".
        OVERLAY(cl,65) = string(deSubtISD,"zz9.9").
        PUT UNFORMATTED cl SKIP.

        cl = "".
        OVERLAY(cl,1) = "INFORMACION SOCIODEMOGRAFICA".
        OVERLAY(cl,65) = string(deSubtISD,"zz9.9").
        PUT UNFORMATTED cl SKIP.
         
        FOR EACH tscoring
            WHERE
                tscoring.vars = "Capacidad_Pago":
            cl = "".
            OVERLAY(cl,1) = upper(replace(tscoring.vars,"_"," ")).
            OVERLAY(cl,40) = tscoring.vvas.
            OVERLAY(cl,65) = string(tscoring.puns,"zz9.9").
            PUT UNFORMATTED chr(10) + cl SKIP.
            deCpcdadPgo = tscoring.puns.
        END.
        
        cl = "".
        OVERLAY(cl,1) = "HISTORICO DE PAGOS".
        PUT UNFORMATTED chr(10) + cl SKIP.
        FOR EACH tscoring
            WHERE
                tscoring.vars = "Mora_Comercial":
            cl = "".
            OVERLAY(cl,1) = "INTERNO".
            OVERLAY(cl,40) = tscoring.vvas.
            OVERLAY(cl,65) = string(tscoring.puns,"zz9.9").
            PUT UNFORMATTED cl SKIP.
            demracmrcial = tscoring.puns.
        END.
        cl = "".
        OVERLAY(cl,1) = "SUBTOTAL HISTORICO DE PAGOS".
        OVERLAY(cl,65) = string(demracmrcial,"zz9.9").
        PUT UNFORMATTED cl SKIP.
        
        FOR EACH tscoring
            WHERE
                tscoring.vars = "garantia":
            cl = "".
            OVERLAY(cl,1) = upper(replace(tscoring.vars,"_"," ")).
            OVERLAY(cl,40) = tscoring.vvas.
            OVERLAY(cl,65) = string(tscoring.puns,"zz9.9").
            PUT UNFORMATTED chr(10) + cl SKIP.
            deGrntia = tscoring.puns.
        END.

        cl = "".
        OVERLAY(cl,1) = "TIPO DE VIVIENDA".
        OVERLAY(cl,65) = string(0,"zz9.9").
        PUT UNFORMATTED chr(10) + cl SKIP.

        cl = "".
        OVERLAY(cl,1) = "FORMA DE PAGO".
        OVERLAY(cl,65) = string(0,"zz9.9").
        PUT UNFORMATTED chr(10) + cl SKIP.

        cl = "".
        OVERLAY(cl,1) = "TOTAL EVALUACION".
        OVERLAY(cl,65) = string(deGrntia + demracmrcial + deCpcdadPgo + deSubtISD,"zz9.9").
        PUT UNFORMATTED chr(10) + cl SKIP.

        cl = "".
        OVERLAY(cl,1) = "EVALUACION FINAL".
        OVERLAY(cl,65) = upper(w_concepto:SCREEN-VALUE IN FRAME F_Scoring).
        PUT UNFORMATTED chr(10) + cl SKIP.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Imp wWin 
PROCEDURE Informe_Imp :
DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VAR W_sw          AS LOGICAL. 
  DEFI   VAR lista         AS CHAR FORM "X(35)".

  Lista = W_PathSpl + "AprobNeg-" + STRING(Solicitud.Num_solicitud).
  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Lista,INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.

  RUN _SetCurs.r ("WAIT").
  OUTPUT TO VALUE(Lista) NO-ECHO PAGED PAGE-SIZE 80.

  RUN Informe.

  OUTPUT CLOSE.        
  RUN _SetCurs.r ("ARROW").
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT Lista).
  ELSE                                                  
    IF W_Dispositivo = "I" THEN
       RUN adecomm/_osprint.r ( INPUT  ?, INPUT Lista,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
  /*IF W_Dispositivo <> "A" THEN
     OS-DELETE VALUE(Lista).*/

  IF W_Dispositivo = "E" THEN
     RUN Imprimir_Excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Info_SoliCodeu wWin 
PROCEDURE Info_SoliCodeu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR Linea     AS CHARACTER FORMAT "X(1125)".
   DEFI VAR TDisp       LIKE Ahorros.Sdo_Disponible.

   Linea = "=============================================INFORMACION DEL CODEUDOR==================================================".
   RUN TmpL (INPUT 27, INPUT Linea).
   Linea = Clientes.Nit + "-" + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   RUN TmpL (INPUT 28, INPUT Linea).

   Linea = "                                                              "
         + "  Deudas DataCrédito          : " + STRING(STRING(Clientes.Sdo_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 29, INPUT Linea). 

   Linea = "Ingreso x Salarios          : " + STRING(STRING(Clientes.Salario,">>>,>>>,>>>,>>9"),"x(30)")
         + "  Deducciones de Colilla      : " + STRING(STRING(Clientes.Gto_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 30, INPUT Linea). 

   Linea = "Ingreso x Arriendos         : " + STRING(STRING(Clientes.Ing_Arriendos,">>>,>>>,>>>,>>9"),"x(30)")
         + "  Gastos Familiares           : " + STRING(STRING(Clientes.Gto_Familiar,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 31, INPUT Linea). 
      
   Linea = "Ingresos Financieros        : " + STRING(STRING(Clientes.Ing_Financieros,">>>,>>>,>>>,>>9"),"x(30)")
         + "  Gastos Fin. Indirectos      : " + STRING(STRING(Clientes.GtoFinanc_Indir,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 32, INPUT Linea). 
   
   Linea = "Otros Ingresos              : " + STRING(STRING(Clientes.Ing_Otros,">>>,>>>,>>>,>>9"),"x(30)")
         + "  Gastos Arriendo             : " + STRING(STRING(Clientes.Gto_Arriendo,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 33, INPUT Linea).

   Linea = "Total Ingresos              : " + STRING(STRING(Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Arriendos + Clientes.Salario,">>>,>>>,>>>,>>9"),"x(30)")
         + "  Total Gastos                : " + STRING(STRING(Clientes.Gto_Arriendo + Clientes.Sdo_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Obligacion,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 34, INPUT Linea).

   ASSIGN Tdisp = Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Arriendos + Clientes.Salario -
                  (Clientes.Gto_Arriendo + Clientes.Sdo_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Obligacion)
          Linea = "Porcentaje de Endeudamiento :        " + STRING(STRING(Clientes.Capacidad_Pago,"%-999.99"),"X(30)")
         + "  Total Disponible     : " + STRING(STRING(Tdisp,">>>,>>>,>>>,>>9"),"X(30)").
   RUN TmpL (INPUT 35, INPUT Linea).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfrmeScringAExcel wWin 
PROCEDURE InfrmeScringAExcel :
/*------------------------------------------------------------------------------
  Purpose: ENVIA SCORING A EXCEL     
  Parameters:  <none>
  Notes:     InfrmeScringAExcel  
  LOG: CREADO, 21 DIC 2007, ING. Edilberto Mariño Moya
------------------------------------------------------------------------------*/


    DEF VAR i AS INTEGER NO-UNDO.    
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR cRngo AS char NO-UNDO.
    DEF VAR cRngo1 AS char NO-UNDO.
    DEF VAR cvlor AS CHAR NO-UNDO.
    DEF VAR sw AS LOGICAL NO-UNDO.
    DEF VAR deTSocioDemo AS DECIMAL NO-UNDO.
    
    
    
/**************************************************************************************************************************************/
    
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR ct AS CHAR NO-UNDO.
    ct = FILL(CHR(1),3).
    DO WITH FRAME {&FRAME-NAME}:
        cl = "ANALISIS DEL SCORE. Créditos Por Nómina".
        fToExcelVlor("a1",cl).
        
        fToExcelVlor("a3","FECHA").
        fToExcelVlor("b3",ffchaltras(bdcentral.Solicitud.Fec_Solicitud)).
        

        fToExcelVlor("a4","NOMBRE DEL ASOCIADO").
        fToExcelVlor("b4",upper(NomNit:SCREEN-VALUE IN FRAME F_Solicitud)).

        fToExcelVlor("a5","IDENTIFICACION").
        fToExcelVlor("b5",solicitud.nit:SCREEN-VALUE IN FRAME F_Solicitud).

        fToExcelVlor("a6","MONTO").
        fToExcelVlor("b6",solicitud.monto:SCREEN-VALUE IN FRAME F_Solicitud).

        fToExcelVlor("a7","PLAZO").
        fToExcelVlor("b7",solicitud.plazo:SCREEN-VALUE IN FRAME F_Solicitud).

        fToExcelVlor("a8","LINEA").
        fToExcelVlor("b8",upper(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud)).

        crngo1 = "a10:".
        fToExcelVlor("a10","INFORMACION SOCIODEMOGRAFICA").

        ftoexcelvlor("a11","VARIABLE").
        ftoexcelvlor("b11","DATOS").
        ftoexcelvlor("c11","PUNTAJE").
        i = 12.
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "01" /* SOCIODEMOG */
            BREAK 
                BY tpro_scoring.pth:
            j = NUM-ENTRIES(tpro_scoring.observacion,"|").
            cvlor = substring(entry(j,tpro_scoring.observacion,"|"),4).
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                ftoexcelvlor("a" + STRING(i),cvlor).
                /* ftoexcelvlor("b" + STRING(i),tscoring.vvas). */
                ftoexcelvlor("b" + STRING(i),tscoring.vvas).
                ftoexcelvlor("c" + STRING(i),string(tscoring.puns)).
                ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
                i = i + 1.
            END.
        END.
        ftoexcelvlor("a" + string(i),"SUBTOTAL").
        ftoexcelvlor("c" + STRING(i),"=" + trim(ENTRY(1,ct,CHR(1)),"+")).
        ENTRY(1,ct,CHR(1)) = "".
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "0" + "+". 
        

        /* lee la suma desde excel */

        i = i + 1.
        ftoexcelvlor("a" + string(i),"INFORMACION SOCIODEMOGRAFICA").
        crngo1 = crngo1 + "c" + STRING(i).
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        
        i = i + 1.         
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "02" 
            BREAK 
                BY tpro_scoring.pth:
            i = i + 1.
            j = NUM-ENTRIES(tpro_scoring.observacion,"|").
            cvlor = substring(entry(j,tpro_scoring.observacion,"|"),4).
            ftoexcelvlor("a" + STRING(i),cvlor).
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                ftoexcelvlor("b" + string(i),tscoring.vvas).
                ftoexcelvlor("c" + string(i),string(tscoring.puns)).
            END.
            ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        END.
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        crngo = "a" + STRING(i) + ":".
        ftoexcelvlor("a" + string(i),"HISTORICO DE PAGOS").
        i = i + 1.
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "0301" /* historico de pago interno */
            BREAK 
                BY tpro_scoring.pth:
            j = NUM-ENTRIES(tpro_scoring.observacion,"|").
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                cvlor = substring(entry(j,tpro_scoring.observacion,"|"),4).
                ftoexcelvlor("a" + STRING(i),cvlor).
                ftoexcelvlor("b" + string(i),tscoring.vvas).
                ftoexcelvlor("c" + string(i),string(tscoring.puns)).
                ENTRY(2,ct,CHR(1)) = ENTRY(2,ct,CHR(1)) + "c" + STRING(i) + "+".
                i = i + 1.
            END.
        END.
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "0302" /* historico de pago externo */
            BREAK 
                BY tpro_scoring.pth:
            IF FIRST(tpro_scoring.pth) 
            THEN DO:
            END.
            sw = FALSE.
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                j = NUM-ENTRIES(tpro_scoring.observacion,"|").
                cvlor = substring(entry(j - 1,tpro_scoring.observacion,"|"),4) + " - " + substring(entry(j,tpro_scoring.observacion,"|"),4).
                ftoexcelvlor("a" + STRING(i),cvlor).
                ftoexcelvlor("b" + string(i),tscoring.vvas).
                ftoexcelvlor("c" + string(i),string(tscoring.puns)).
                sw = TRUE.
                ENTRY(2,ct,CHR(1)) = ENTRY(2,ct,CHR(1)) + "c" + STRING(i) + "+".
                i = i + 1.
            END.
            IF LAST(tpro_scoring.pth) 
            THEN DO:
                IF NOT sw  
                THEN DO:
                END.
            END.
        END.
        ftoexcelvlor("a" + string(i),"SUBTOTAL HISTORICO DE PAGOS").
        ftoexcelvlor("c" + STRING(i),"=" + TRIM(ENTRY(2,ct,CHR(1)),"+")).
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        crngo = crngo + "c" + STRING(i).
        fbrdesrngo(crngo).
        
        i = i + 2.
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "04" /* GARANTIA */
            BREAK 
                BY tpro_scoring.pth:
            j = NUM-ENTRIES(tpro_scoring.observacion,"|").
            cvlor = substring(entry(j,tpro_scoring.observacion,"|"),4).
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                ftoexcelvlor("a" + STRING(i),cvlor).
                ftoexcelvlor("b" + string(i),tscoring.vvas).
                ftoexcelvlor("c" + string(i),string(tscoring.puns)).
                ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
            END.
        END.
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "05" /* patrimonial vivienda */
            BREAK 
                BY tpro_scoring.pth:
            j = NUM-ENTRIES(tpro_scoring.observacion,"|").
            cvlor = substring(entry(j,tpro_scoring.observacion,"|"),4).
            
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                ftoexcelvlor("a" + STRING(i),cvlor).
                ftoexcelvlor("b" + string(i),tscoring.vvas).
                ftoexcelvlor("c" + string(i),string(tscoring.puns)).
                ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
            END.
            fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
            
        END.
/**/ 
        i = i + 2.
        FOR EACH tpro_scoring
            WHERE
                tpro_scoring.codigo = solicitud.FOR_pago
            AND tpro_scoring.pth BEGINS "06" /* forma de pago */
            BREAK 
                BY tpro_scoring.pth:
            j = NUM-ENTRIES(tpro_scoring.observacion,"|").
            cvlor = substring(entry(j,tpro_scoring.observacion,"|"),4).
            ftoexcelvlor("a" + STRING(i),cvlor).
            FOR EACH tscoring
                WHERE
                    trim(SUBSTRING(ENTRY(NUM-ENTRIES(tpro_scoring.observacion,"|"),tpro_scoring.observacion,"|"),4)) + "(" + trim(tpro_scoring.VARIABLE) + ")" = tscoring.VARs:
                ftoexcelvlor("b" + string(i),tscoring.vvas).
                ftoexcelvlor("c" + string(i),string(tscoring.puns)).
                ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
            END.
            fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
            LEAVE.
        END.

        i = i + 2.
        ftoexcelvlor("a" + string(i),"TOTAL EVALUACION").
        ftoexcelvlor("c" + STRING(i),"=" + trim(ENTRY(1,ct,CHR(1)),"+")).
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        
        
        i = i + 2.
        ftoexcelvlor("a" + string(i),"EVALUACION FINAL").
        ftoexcelvlor("c" + string(i),upper(w_concepto:SCREEN-VALUE IN FRAME F_Scoring)).
        crngo = "a" + STRING(i) + ":" + "c" + STRING(i).
        fbrdesrngo(crngo).
    END.

    fBrdesRngo("b3:b8").
    fBrdesRngo("a3:b8").
    fBrdesRngo(crngo1).

    /**************************************************************************************************************************************/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Admisible wWin 
PROCEDURE Inicializar_Admisible :
DO WITH FRAME F_Admisible:
 ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE       = "1"
        Garantias.Identificacion_Bien:SCREEN-VALUE = ""
        Garantias.Nom_Bien:SCREEN-VALUE            = ""
        Garantias.Descripcion_Bien:SCREEN-VALUE    = ""
        Garantias.Descripcion_Bien2:SCREEN-VALUE   = ""
        Garantias.Fec_Creacion:SCREEN-VALUE        = STRING(w_fecha)
        Garantias.Fec_Retiro:SCREEN-VALUE          = ""
        Garantias.Nit_Aseguradora:SCREEN-VALUE     = ""
        Garantias.Nro_Seguro:SCREEN-VALUE          = ""
        Garantias.Fec_IniSeguro:SCREEN-VALUE       = ""
        Garantias.Fec_FinSeguro:SCREEN-VALUE       = ""
        Garantias.Val_Asegurado:SCREEN-VALUE       = ""
        Nom_Aseguradora:SCREEN-VALUE               = ""
        Nom_UsuGarantia:SCREEN-VALUE               = ""
        Garantias.Val_Bien:SCREEN-VALUE            = "".

 ASSIGN W_CredAval                                 = 0
        W_CredAval:SCREEN-VALUE                    = "0"
        W_DispGaran                                = 0
        W_DispGaran:SCREEN-VALUE                   = "0"
        Garantias.Nom_Impuesto:SCREEN-VALUE        = ""
        Garantias.Val_Impuesto:SCREEN-VALUE        = ""
        Garantias.Fec_VctoImpuesto:SCREEN-VALUE    = ""
        Garantias.Fec_UltAvaluo:SCREEN-VALUE       = ""
        Garantias.Fec_ProxAvaluo:SCREEN-VALUE      = ""
        Garantias.Val_UltAvaluo:SCREEN-VALUE       = ""
        Garantias.Aprobada:SCREEN-VALUE            = "No"
        Garantias.Estado:SCREEN-VALUE              = "1"
        Garantias.Identificacion_Bien:SENSITIVE    = TRUE.

 IF R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias= "3" THEN
    ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE = "5".

 FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
 IF AVAILABLE Usuarios THEN
    ASSIGN Nom_UsuGarantia:SCREEN-VALUE = Usuario.Usuario + " - " + Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables wWin 
PROCEDURE Inicializar_Variables :
DO WITH FRAME F_Solicitud:
    FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
    IF AVAILABLE Agencias THEN DO:
        Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

        APPLY "value-changed" TO Cmb_Agencias.
    END.

    IF NOT W_Nuevo THEN
        Solicitud.Num_Solicitud:SCREEN-VALUE = "0".

    ASSIGN Solicitud.Nit:SCREEN-VALUE = ""
           NomNit:SCREEN-VALUE = ""
           Nom_Producto:SCREEN-VALUE = ""
           Solicitud.Fec_Solicitud:SCREEN-VALUE = STRING(w_fecha)
           Solicitud.FOR_Interes:SCREEN-VALUE = "1"
           Solicitud.Monto:SCREEN-VALUE = "0"
           Solicitud.Plazo:SCREEN-VALUE = "0"
           Solicitud.Cuota:SCREEN-VALUE = "0"
           Solicitud.Deducible:SCREEN-VALUE     = "0"
           W_VrADesemb:SCREEN-VALUE             = "0"
           W_VrADesemb
           Fec_Asociado:SCREEN-VALUE IN FRAME F_Solicitud = ""
           WAntiguedad:SCREEN-VALUE IN FRAME F_Solicitud = ""
           WVeces:SCREEN-VALUE IN FRAME F_Solicitud = ""
           WTasa:SCREEN-VALUE  IN FRAME F_Solicitud = ""
           WMonto:SCREEN-VALUE IN FRAME F_Solicitud = ""
           Cmb_Perpago:SCREEN-VALUE = "4 - Mensual"
           Solicitud.Tasa:SCREEN-VALUE = "0"
           W_TasaNominal:SCREEN-VALUE = "0"
           W_TasaPeriodo:SCREEN-VALUE = "0"
           W_Desembolso:SCREEN-VALUE = "Efectivo"
           W_ForPago:SCREEN-VALUE = "Caja"
           solicitud.fec_desembolso:SCREEN-VALUE = ''
           solicitud.fec_primerPago:SCREEN-VALUE = ''.

    FOR EACH TScoring:
        DELETE TScoring.
    END.

    OPEN QUERY Br_Scoring FOR EACH TScoring NO-LOCK INDEXED-REPOSITION.
END.

DO WITH FRAME F_Desembolso:
    ASSIGN Solicitud.Desembolso:SCREEN-VALUE = "1"
           Solicitud.Age_Desembolso:SCREEN-VALUE = "0"
           Solicitud.Cod_Desembolso:SCREEN-VALUE = "0"
           Solicitud.Cue_Desembolso:SCREEN-VALUE = "".
END.

DO WITH FRAME F_ForPago:
    ASSIGN Solicitud.FOR_Pago:SCREEN-VALUE = "1"
           Solicitud.Cue_DebAutomatico:SCREEN-VALUE = "".
END.

ASSIGN Cmb_ConCte:SCREEN-VALUE IN FRAME F_VblesS = " "
       Cmb_destino:SCREEN-VALUE = " "
       Cmb_MoraCial:SCREEN-VALUE = " "
       Cmb_Gtia:SCREEN-VALUE = " "
       Cmb_ResPat:SCREEN-VALUE = " "
       Cmb_TipAct:SCREEN-VALUE = " ".

EMPTY TEMP-TABLE TCred_ACanc.

W_TotCanc = 0.
W_TotCanc:SCREEN-VALUE IN FRAME F_CredACanc = "0".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
MESSAGE "Inicia proceso"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    /* oakley */

FOR EACH pro_scoring WHERE pro_scoring.observacion = "ESCENARIO ECONOMICO" NO-LOCK:
    CREATE tEscnrioEcnmco.
    ASSIGN tEscnrioEcnmco.clfccion = pro_scoring.titulo
           tEscnrioEcnmco.ps = fchar2decimal(pro_scoring.Rango_final).
END.

FOR EACH Varios WHERE Varios.Tipo = 26 NO-LOCK:
    W_Ok = Cmb_Negadas:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Ultima.
END.

FOR EACH Instancias WHERE Instancias.Tipo_Instancia = 1
                      AND Instancias.Estado = 1
                      AND Instancias.Tipo_Producto = 2 NO-LOCK BREAK BY Instancias.Orden:
    IF Instancias.Ultima THEN
        W_Ultima = Instancias.Instancia.

    IF Instancias.Primera THEN
        W_Primera = Instancias.Instancia.

    IF Instancias.Id_Negadas THEN
        W_Negadas = Instancias.Instancia.

    FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Tipo_Instancia = 1
                                AND Cfg_Instancias.Instancia = Instancias.Instancia
                                AND Cfg_Instancias.Usuario = W_Usuario
                                AND Cfg_Instancias.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Cfg_Instancias THEN DO:
        W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Creditos.
    END. 
    
    IF Instancias.Instancia < 50 THEN
        W_Ok = Cmb_InsCon:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Condicionada.
END.

IF W_Ultima = 0 THEN
    MESSAGE "No se ha definido la ultima instancia en el proceso de" SKIP
            "Solicitud. define una instancia con el parámetro último."
        VIEW-AS ALERT-BOX.

IF W_Primera = 0 THEN
    MESSAGE "No se ha definido la primera instancia en el proceso de" SKIP
            "Solicitud. define una instancia con el parametro primera"
        VIEW-AS ALERT-BOX.

FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.

NomUsuario = W_Usuario + " - " + Usuarios.Nombre.
W_SucAgen  = Usuarios.Id_OpeOfi.

DO WITH FRAME F_Solicitud:
    FOR EACH Agencias WHERE Agencia.Estado = 1 NO-LOCK:
        W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
    END.

    FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito = Solicitud.Tip_Credito
                            AND Pro_Creditos.Estado = 1 NO-LOCK:
        W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto) IN FRAME F_Producto.
    END.

    FOR EACH Varios WHERE Varios.Tipo = 20:
        W_Ok = Cmb_Sistemas:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
    END.

    Cmb_Sistemas:SCREEN-VALUE = Cmb_Sistemas:ENTRY(1).
    
    RUN Inicializar_Variables.
END.

RUN SUPER.

SUBSCRIBE "CpcdadPgoScoring" ANYWHERE.

Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1).

RUN Solicitudes_X_Instancia.
    APPLY "entry" TO Cmb_Instancias IN FRAME F_Creditos.
    RUN Inicializar_Variables.
    HIDE FRAME F_AsentarInstancia.
    HIDE FRAME F_Cerradas.
    HIDE FRAME F_CredACanc.
    HIDE FRAME F_Agregar.
    WWin:TITLE = "Proceso de Solicitudes - Agencia Actual: " + STRING(W_Agencia).
    Buscar:LABEL IN FRAME F_Consulta = "Buscar x Solicitud".
    APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
    fCpcdadPgo().
    RUN hideObject IN h_w-proclientesnew.
    RUN pParentHandle IN h_w-proclientesnew(THIS-PROCEDURE).
    RUN hideObject IN h_w-adm_garantias.
    RUN pParentHandle IN h_w-adm_garantias(THIS-PROCEDURE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar wWin 
PROCEDURE Liquidar :
DEFINE VARIABLE PlazoW LIKE Solicitud.Plazo.
DEFINE VARIABLE TotPtW LIKE Solicitud.Monto.
DEFINE VARIABLE CuotaW LIKE Solicitud.Cuota.
DEFINE VARIABLE TasaW LIKE Solicitud.Tasa.
DEFINE VARIABLE TinteW LIKE Solicitud.Monto.
DEFI VAR TVrPste LIKE Solicitud.Monto.
DEFI VAR VrPste LIKE Solicitud.Monto.
DEFI VAR Periodo AS INTEG FORM "99".
DEFI VAR W_NroDias AS INTEG FORM "9999".
DEFI VAR P_NMeses AS DEC.
DEFI VAR P_NomPer AS CHAR FORMAT "X(15)".
DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
DEFINE VAR Wimp_Coop AS INTEGER INITIAL 0.
DEFINE VAR interesPreInicio AS DECIMAL.
DEFINE VAR tasaPeriodo AS DECIMAL.
DEFINE VAR diasPreInicio AS INTEGER.
DEFINE VAR fechaInicio AS DATE.
DEFINE VAR cont AS INTEGER.
DEFINE VAR ultimaCuota AS DECIMAL.
DEFINE VAR nroUltimaCuota AS INTEGER.

DO WITH FRAME F_Solicitud:
    ASSIGN TotPtW = DECIMAL(Solicitud.Total_Prestamo:SCREEN-VALUE)
           TasaW = DECIMAL(Solicitud.Tasa:SCREEN-VALUE)
           PlazoW = DECIMAL(Solicitud.Plazo:SCREEN-VALUE)
           vPeriodicidad = INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1))
           TVrPste = 0
           tasaPeriodo = DECIMAL(w_tasaPeriodo:SCREEN-VALUE).

    IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia GT 0 THEN
        TotPtW = TotPtW + (((TotPtW * Tas_Nominal / 360) * Pro_Creditos.Dia_Gracia) / 100).

    RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE,1,1)),
                                   INPUT PlazoW,
                                   OUTPUT W_NroDias,
                                   OUTPUT P_NMeses,
                                   OUTPUT Periodo,
                                   OUTPUT P_NomPer).

    IF AVAILABLE solicitud THEN DO:
        IF W_TotExt GT 0 THEN DO:
            FOR EACH Extras WHERE Extras.Nit EQ Solicitud.Nit
                              AND Extras.Num_Solicitud EQ Solicitud.Num_Solicitud
                              AND extras.estado = 1 NO-LOCK BY Extras.Nro_Cuota:
                RUN HPDF IN W_ManFin (INPUT Extras.Vr_CuoExtra,
                                      INPUT (Tas_Nominal / (Periodo * 100)),
                                      INPUT Extras.Nro_Cuota,
                                      OUTPUT VrPste).

                TVrPste = TVrPste + VrPste.
            END.

            TotPtW = TotPtW - TVrPste.
        END.
    END.

    /* Esto lo hacemos con el fin de distribuir los intereses adicionales a un periodo de forma que aumentando la cuota se puedan amortizar en el transcurso del crédito */
    IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)) <> 2 AND
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 108 AND
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 113 AND
       INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 114 THEN DO:

        fechaInicio = ADD-INTERVAL(DATE(solicitud.fec_primerPago:SCREEN-VALUE),-1,"months").
        diasPreinicio = fechaInicio - DATE(solicitud.fec_desembolso:SCREEN-VALUE).

        ASSIGN interesPreInicio = ((DECIMAL(Solicitud.Total_Prestamo:SCREEN-VALUE) / 100) * (tasaPeriodo / 30) * diasPreinicio) WHEN diasPreinicio > 0.
        
        /*totPtw = totPtw + interesPreInicio.*/
    END.

    IF DATE(solicitud.fec_solicitud:SCREEN-VALUE) >= 02/12/2018 THEN DO:
        RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw,
                             INPUT-OUTPUT PlazoW,
                             INPUT-OUTPUT CuotaW,
                             INPUT-OUTPUT TInteW,
                             INPUT-OUTPUT TasaW,
                             INPUT 0,
                             INPUT 0,
                             INPUT vPeriodicidad,
                             INPUT 3,
                             INPUT INT(Solicitud.FOR_Interes:SCREEN-VALUE),
                             INPUT INT(SUBSTR(Cmb_Sistemas:SCREEN-VALUE,1,5))).

        IF INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)) <> 2 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 108 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 113 AND
           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) <> 114 THEN DO:
            RUN devuelveUltimaCuota.r(INPUT TotPtw /*DECIMAL(Solicitud.Monto:SCREEN-VALUE IN FRAME F_Solicitud)*/ ,
                                      INPUT PlazoW /*DECIMAL(Solicitud.Plazo:SCREEN-VALUE IN FRAME F_Solicitud)*/ ,
                                      INPUT-OUTPUT cuotaW,
                                      INPUT DATE(Solicitud.fec_desembolso:SCREEN-VALUE),
                                      INPUT DATE(Solicitud.fec_primerPago:SCREEN-VALUE),
                                      INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
                                      INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
                                      INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                                      INPUT INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)),
                                      INPUT DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)).
        END.
    
        IF CuotaW LE 0 THEN DO:
            MESSAGE "El Valor de la cuota debe ser mayor a cero. Rectifique!"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "ENTRY" TO Cmb_Sistemas IN FRAME F_Solicitud.
            RETURN ERROR.
        END.

        /* Redondeo a múltiplo de 100 */
        cuotaW = ROUND((cuotaW / 100) + 1,0) * 100.

        ASSIGN Solicitud.Cuota:SCREEN-VALUE = STRING(CuotaW).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCliente wWin 
PROCEDURE Llenar_InfoCliente :
DEFINE VARIABLE gtexto AS CHARACTER FORMAT "x(60)".
DEFINE VAR ENDGLOCLI LIKE Solicitud.Monto INITIAL 0.
DEFINE VARIABLE TTOTAL  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
DEFINE VARIABLE TDISPO  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".

IF AVAILABLE solicitud
THEN DO:
    EndGlocli = EndGlocli + Solicitud.Monto.
    FOR EACH creditos WHERE Creditos.nit = Solicitud.Nit:
        IF Creditos.Estado NE 2 THEN NEXT.
        IF CREDITOS.TIP_CREDITO GT 4 THEN NEXT.
        IF Creditos.cod_credito = 570 THEN ASSIGN EndGlocli = EndGlocli + creditos.monto.
        ELSE ASSIGN EndGlocli = EndGlocli + creditos.sdo_capital.
    END.   
END.


   DO i = 1 TO 100:
/*       W_Ok = S_InfoCliente_old:DELETE(1) IN FRAME F_InfoCliente. */
       ASSIGN S_InfoCliente:SCREEN-VALUE IN FRAME F_InfoCliente = "".
   END.
   IF AVAILABLE Clientes THEN DO:
      CASE Clientes.Tipo_Vinculo:
        WHEN 1 THEN DO:
          gTexto = "Tipo de Vinculo        : Asociado".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 2 THEN DO:
          gTexto = "Tipo de Vinculo        : Cliente No Asociado".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
          gTexto = "Tipo de Vinculo        : Tercero".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
          gTexto = "Tipo de Vinculo        : Proveedor".
          RUN SInfo (INPUT gtexto).
        END.
      END CASE.
      TTotal = Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Salario.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - INGRESOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Salario GT 0 THEN DO:
         

      

         gTexto = "Salario                : " + STRING(Clientes.Salario,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Ing_arriendos GT 0 THEN DO:
         gtexto = "Ingresos Financieros   : " + STRING(Clientes.Ing_Financieros,">>,>>>,>>>,>>9").
         RUN Sinfo (INPUT gtexto).
      END.
      IF Clientes.Ing_Honorarios GT 0 THEN DO:
         gtexto = "Ingresos por Honorarios: " + STRING(Clientes.Ing_Honorarios,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Ing_Otros GT 0 THEN DO:
         gtexto = "Otros Ingresos         : " + STRING(Clientes.Ing_Otros,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF TTotal GT 0 THEN DO:
         gtexto = "Total Ingresos---------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TDispo = TDispo + TTotal.
      TTotal = 0.
      
      TTotal = Clientes.Gto_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Arriendo.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - GASTOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Arriendo GT 0 THEN DO:
         gtexto = "Gastos Arriendo        : " + STRING(Clientes.Gto_Arriendo,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Familiar GT 0 THEN DO:
         gtexto = "Gastos Familiares      : " + STRING(Clientes.Gto_Familiar,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Obligacion GT 0 THEN DO:
         gtexto = "Obligaciones           : " + STRING(Clientes.Gto_Obligacion,">>,>>>,>>>,>>9").
         RUN Sinfo (INPUT gtexto).
      END.

      IF TTotal GT 0 THEN DO:
         gtexto = "Total Egresos----------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TDispo = TDispo - TTotal.
      IF TDispo GT 0 THEN DO:
         gTexto = "Disponible (Ing - Egre): " + STRING(TDispo,"->,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TTotal = 0.
      TDispo = 0.
      TTotal = Clientes.Act_Vehiculo + Clientes.Act_Inversion + Clientes.Act_Casa.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - ACTIVOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Casa GT 0 THEN DO:
         gTexto = "Valor en Propiedades   : " + STRING(Clientes.Act_casa,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Inversion GT 0 THEN DO:
         gtexto = "Valor Inversiones      : " + STRING(Clientes.Act_inversion,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Vehiculo GT 0 THEN DO:
         gtexto = "Valor Vehiculo         : " + STRING(Clientes.Act_Vehiculo,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF TTotal GT 0 THEN DO:
         gtexto = "".
         RUN SInfo (INPUT gtexto).
         gtexto = "Total Activos----------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
     /* IF Clientes.Sdo_Obligaciones GT 0 THEN DO:*/
         gTexto = "    -Endeudamiento Global -".
         RUN SInfo (INPUT gtexto).
         gTexto = "Endeudamiento Global     : " + STRING(EndGlocli,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      /*END.*/
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoProducto wWin 
PROCEDURE Llenar_InfoProducto :
DEFINE VAR gtexto AS CHARACTER FORMAT "X(50)".
DO WITH FRAME F_InfoProducto:
  DO i = 1 TO 100 BY 1:
     W_Ok = S_InfoProducto:DELETE(i) IN FRAME F_InfoProducto.
  END.

  ASSIGN W_Forpago:SCREEN-VALUE IN FRAME F_Solicitud        = "Caja".
         Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "1".

  IF Pro_Creditos.Id_MontoMinimo THEN DO:
     gtexto = "Monto Minimo       :  "  + STRING(Pro_Creditos.Val_MontoMinimo,">>>,>>>,>>>,>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
  END.
  IF Pro_Creditos.Id_MontoMaximo THEN DO:
     gtexto = "Monto Máximo       :  "  + STRING(Pro_Creditos.Val_MontoMaximo,">>>,>>>,>>>,>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
  END.
  IF Pro_Creditos.Id_Plazo THEN DO:
     gtexto = "Plazo Mínimo       :  "  + STRING(Pro_Creditos.Pla_Minimo / 30,">>>,>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
     gtexto = "Plazo Máximo       :  "  + STRING(Pro_Creditos.Pla_Maximo / 30,">>>,>>9").
     W_Ok = S_InfoProducto:ADD-LAST(gtexto).
  END.

  IF Pro_Creditos.Id_Sorteo THEN DO:
     ASSIGN W_Forpago:SCREEN-VALUE IN FRAME F_Solicitud        = "Nómina"
            Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "2".

     IF W_Nuevo THEN DO:
        /*              DESHABILITADO 04/11/2008 POR WILLIAM MARTINEZ
         MESSAGE "Forma de pago POR NOMINA, Revise el Pdo.de pago para el cálculo de la cuota."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "Mouse-Select-Click" TO Solicitud.For_Pago IN FRAME F_ForPago.
        */
     END.
  END.
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarAportes wWin 
PROCEDURE MostrarAportes :
FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarScor wWin 
PROCEDURE MostrarScor :
/*------------------------------------------------------------------------------
    Purpose:     De cada codeudor.
    ------------------------------------------------------------------------------*/
    FOR EACH TScoring: DELETE TScoring. END.
    CLOSE QUERY Br_Scoring.
    TOTAL_Puntaje = 0.
    
    FOR EACH Scoring 
        WHERE 
            Scoring.Nit           EQ Relaciones.Nit_Relacion 
        AND Scoring.Fec_Scoring   EQ W_Fecha
        AND Scoring.Num_Solicitud EQ Solicitud.Num_Solicitud NO-LOCK
            BY Scoring.Codigo:
        TOTAL_Puntaje = TOTAL_Puntaje + Scoring.Puntaje.
        CREATE TScoring.                                      
        ASSIGN  TScoring.VarS = Scoring.VARIABLE               
                TScoring.TabS = Scoring.Tabla                  
                TScoring.VVaS = Scoring.Valor_Variable         
                TScoring.PunS = Scoring.Puntaje                
                TScoring.FecS = Scoring.Fec_Scoring            
                TScoring.CodS = Scoring.Codigo.                
    END.
    
    OPEN QUERY Br_Scoring FOR EACH TScoring NO-LOCK INDEXED-REPOSITION.
    DISPLAY TOTAL_Puntaje WITH FRAME F_Scoring.
    
    /*
    FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

    IF  Cfg_RegCredito.Agencia_Exigida  EQ 11                 /*Enero 19/06 GAER*/
        AND Pro_Creditos.Tip_Credito        EQ 4 
    THEN FIND Varios WHERE Varios.Tipo   EQ 19 AND Varios.Codigo  EQ 2 NO-LOCK NO-ERROR.
    ELSE FIND Varios WHERE Varios.Tipo   EQ 19 AND Varios.Codigo EQ 1 NO-LOCK NO-ERROR.

    FIND FIRST PRO
    IF TOTAL_Puntaje LT Varios.Val_Inicial 
    THEN ASSIGN W_Concepto:SCREEN-VALUE = "Negada"
    W_Concepto.
    ELSE 
    IF TOTAL_Puntaje LT Varios.Val_Final 
    THEN ASSIGN W_Concepto:SCREEN-VALUE = "Franja de Anàlisis"
    W_Concepto.
    ELSE 
    IF TOTAL_Puntaje GE Varios.Val_Final 
    THEN ASSIGN W_Concepto:SCREEN-VALUE = "Aprobada"
    W_Concepto.  
    */
    FIND FIRST tEscnrioEcnmco NO-LOCK
        WHERE
            tEscnrioEcnmco.ps >= TOTAL_Puntaje NO-ERROR.
    IF AVAILABLE tEscnrioEcnmco
    THEN DO:
        ASSIGN  W_Concepto:SCREEN-VALUE = tEscnrioEcnmco.Clfccion
                W_Concepto. 
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Admisible wWin 
PROCEDURE Mostrar_Admisible :
DEFI VAR W_RowIdGar AS ROWID.

IF NOT AVAILABLE Garantias THEN DO:
   MESSAGE "Debe existir la garantia."
       VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
/*
ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "3"
       R_TipoGarantia.    
       */        

IF Garantias.Tipo_Garantia LE 3 OR Garantias.Tipo_Garantia EQ 6 THEN
   ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "2"
          R_TipoGarantia.
ELSE
    ASSIGN R_TipoGarantia:SCREEN-VALUE IN FRAME F_Garantias = "3"
          R_TipoGarantia.


DO WITH FRAME F_Admisible:
   ASSIGN Garantias.Tipo_Garantia:SCREEN-VALUE     = STRING(Garantias.Tipo_Garantia)
        Garantias.Identificacion_Bien:SCREEN-VALUE = Garantias.Identificacion_Bien
        Garantias.Nom_Bien:SCREEN-VALUE = Garantias.Nom_Bien
        Garantias.Descripcion_Bien:SCREEN-VALUE = Garantias.Descripcion_Bien
        Garantias.Descripcion_Bien2:SCREEN-VALUE = Garantias.Descripcion_Bien2
        Garantias.Fec_Creacion:SCREEN-VALUE = STRING(Garantias.Fec_Creacion)
        Garantias.Fec_Retiro:SCREEN-VALUE  = STRING(Garantias.Fec_Retiro)
        Garantias.Nit_Aseguradora:SCREEN-VALUE = Garantias.Nit_Aseguradora
        Garantias.Nro_Seguro:SCREEN-VALUE  = STRING(Garantias.Nro_Seguro)
        Garantias.Fec_IniSeguro:SCREEN-VALUE = STRING(Garantias.Fec_IniSeguro)
        Garantias.Fec_FinSeguro:SCREEN-VALUE = STRING(Garantias.Fec_FinSeguro)
        Garantias.Val_Asegurado:SCREEN-VALUE = STRING(Garantias.Val_Asegurado)
        Garantias.Val_Bien:SCREEN-VALUE      = STRING(Garantias.Val_Bien)
        Garantias.Nom_Impuesto:SCREEN-VALUE  = Garantias.Nom_Impuesto
        Garantias.Estado:SCREEN-VALUE        = STRING(Garantias.Estado).
 
   ASSIGN Garantias.Val_Impuesto:SCREEN-VALUE   = STRING(Garantias.Val_Impuesto)
        Garantias.Fec_VctoImpuesto:SCREEN-VALUE = STRING(Garantias.Fec_VctoImpuesto)
        Garantias.Fec_UltAvaluo:SCREEN-VALUE    = STRING(Garantias.Fec_UltAvaluo)
        Garantias.Fec_ProxAvaluo:SCREEN-VALUE   = STRING(Garantias.Fec_ProxAvaluo)
        Garantias.Val_UltAvaluo:SCREEN-VALUE    = STRING(Garantias.Val_UltAvaluo)
        Garantias.Aprobada:SCREEN-VALUE         = STRING(Garantias.Aprobada)
        Btn_SalAdm:SENSITIVE = TRUE
        W_CredAval:SCREEN-VALUE   = "0"
        W_DispGaran:SCREEN-VALUE  = "0"
        W_CredAval   = 0
        W_DispGaran  = 0
        W_NBien      = Garantias.Identificacion_Bien
        W_RowIdGar   = ROWID(Garantias).

   RUN Halla_DispGaran.

   FIND Garantias WHERE ROWID(Garantias) EQ W_RowidGar NO-LOCK NO-ERROR.

   FIND Clientes WHERE Clientes.Nit EQ Garantias.Nit_Aseguradora NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN 
      ASSIGN Nom_Aseguradora:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   ELSE
      ASSIGN Nom_Aseguradora:SCREEN-VALUE = "".

   FIND Usuarios WHERE Usuarios.Usuario EQ Garantias.Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuario THEN
      ASSIGN Nom_UsuGarantia:SCREEN-VALUE = Usuarios.Usuario + " - " + Usuarios.Nombre.
   ELSE
      ASSIGN Nom_UsuGarantia:SCREEN-VALUE = "".

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Solicitud wWin 
PROCEDURE Mostrar_Solicitud :
DEF VAR x1 AS CHA FORMAT "X(40)".
DEF VAR J AS INT FORM "9".

IF NOT CAN-FIND(FIRST Anexos_Clientes WHERE Anexos_Clientes.nit = Solicitud.nit) THEN DO:
    CREATE Anexos_Clientes.
    Anexos_Clientes.nit = Solicitud.nit.
END.

FIND FIRST Anexos_Clientes NO-LOCK WHERE Anexos_Clientes.nit = Solicitud.nit NO-ERROR.

IF Solicitud.Estado EQ 4 THEN
    MESSAGE "Esta Solicitud fue Retornada del Proceso Concepto Final, como Condicionada," SKIP
            "Por favor Priorice su proceso, si cumple los nuevos requisitos"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

DO WITH FRAME F_Producto:
    ASSIGN Solicitud.Tip_Credito:SCREEN-VALUE = STRING(Solicitud.Tip_Credito).

    APPLY "value-changed" TO Solicitud.Tip_Credito.

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Solicitud.Tip_Credito
                              AND Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
    IF AVAIL Pro_Creditos THEN
        Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
    ELSE
        MESSAGE "No se encuentra el producto de créditos de la Solicitud"
            VIEW-AS ALERT-BOX ERROR.

    APPLY "value-changed" TO Cmb_Productos.
    APPLY "choose" TO Btn_OutProductos.
END.

DO WITH FRAME F_Desembolso:
    ASSIGN Solicitud.Desembolso:SCREEN-VALUE = STRING(Solicitud.Desembolso)
           Solicitud.Age_Desembolso:SCREEN-VALUE = STRING(Solicitud.Age_Desembolso)
           Solicitud.Cod_Desembolso:SCREEN-VALUE = STRING(Solicitud.Cod_Desembolso)
           Solicitud.Cue_Desembolso:SCREEN-VALUE = Solicitud.Cue_Desembolso.

    IF Solicitud.Cue_Desembolso NE "" THEN DO:
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Solicitud.Cod_Desembolso NO-LOCK NO-ERROR.
        IF AVAIL Pro_Ahorros THEN
            W_NomProDesembolso:SCREEN-VALUE = Pro_Ahorros.Nom_Producto.

        FIND FIRST Agencias WHERE Agencias.Agencia EQ Solicitud.Age_Desembolso NO-LOCK NO-ERROR.
        IF AVAIL Agencias THEN
            W_NomAgeDesembolso:SCREEN-VALUE = Agencias.Nombre.
    END.
END.

DO WITH FRAME F_ForPago:
    ASSIGN Solicitud.FOR_Pago:SCREEN-VALUE = STRING(Solicitud.FOR_Pago)
           Solicitud.Age_DebAutomatico:SCREEN-VALUE = STRING(Solicitud.Age_DebAutomatico)
           Solicitud.Cod_DebAutomatico:SCREEN-VALUE = STRING(Solicitud.Cod_DebAutomatico)
           Solicitud.Cue_DebAutomatico:SCREEN-VALUE = Solicitud.Cue_DebAutomatico
           Anexos_cliente.cam_cat1:SCREEN-VALUE = Anexos_cliente.cam_cat1.

    CASE Solicitud.FOR_Pago:
        WHEN 1 OR
        WHEN 3 THEN Anexos_cliente.cam_cat1:SENSITIVE = FALSE.
    END CASE.

    CASE Solicitud.FOR_Pago:
        WHEN 1 THEN
            ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Solicitud = "Caja"
                   Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "1".

        WHEN 2 THEN
            ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Solicitud = "Nómina"
                   Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "2".
          
        WHEN 3 THEN
            ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Solicitud = "Débito Automático"
                   Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "3".
        
        WHEN 4 THEN
            ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Solicitud = "Nomina Crecediario"
                   Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "4".
          
        WHEN 5 THEN
            ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Solicitud = "Prima"
                   Solicitud.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "5".
    END CASE.

    IF Solicitud.Cue_DebAutomatico NE "" THEN DO:
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Solicitud.Cod_DebAutomatico NO-LOCK NO-ERROR.
        IF AVAIL Pro_Ahorros THEN
            W_NomCodDebAutomatico:SCREEN-VALUE = Pro_Ahorros.Nom_Producto.

        FIND FIRST Agencias WHERE Agencias.Agencia EQ Solicitud.Age_DebAutomatico NO-LOCK NO-ERROR.
        IF AVAIL Agencias THEN
            W_NomAgeDebAutomatico:SCREEN-VALUE = Agencias.Nombre.
    END.

    CASE Solicitud.Desembolso:
        WHEN 1 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Solicitud = "Efectivo".
        WHEN 2 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Solicitud = "Cheque".
        WHEN 3 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Solicitud = "Cuenta de Ahorros".
        WHEN 4 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Solicitud = "Orden a Terceros".
    END CASE.
END.

DO WITH FRAME F_Solicitud:
    FIND FIRST Agencias WHERE Agencias.Agencia EQ Solicitud.Agencia NO-LOCK NO-ERROR.
    IF AVAIL Agencias THEN
        Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

    DISPLAY Solicitud.Num_Solicitud
            Solicitud.Nit
            Solicitud.Fec_Solicitud
            Solicitud.FOR_Interes
            Solicitud.Monto
            Solicitud.Cuota
            Solicitud.Deducible
            Solicitud.Tasa
            Solicitud.Total_Prestamo
            solicitud.fec_desembolso
            solicitud.fec_primerPago.

    FIND FIRST Varios WHERE Varios.Tipo EQ 38
                        AND Varios.Codigo EQ Solicitud.Destinof NO-LOCK NO-ERROR.
    IF AVAIL Varios THEN
        Nom_Destino:SCREEN-VALUE = Varios.Descripcion.

    ASSIGN Cmb_Sistemas:SCREEN-VALUE = Cmb_Sistemas:ENTRY(Solicitud.Sistema)
           W_VrCredACanc:SCREEN-VALUE = "0"
           W_TotCanc:SCREEN-VALUE IN FRAME F_CredACanc = "0"
           W_VrADesemb:SCREEN-VALUE = "0"
           W_VrADesemb = 0
           W_VrCredACanc = 0.

    FOR EACH solicitudes_pagoCreditos WHERE solicitudes_pagoCreditos.cliente_id = solicitud.nit
                                        AND solicitudes_pagoCreditos.num_solicitud = solicitud.num_solicitud
                                        AND solicitudes_pagoCreditos.num_credito = TCred_ACanc.numC NO-LOCK:
        W_VrCredACanc = W_VrCredACanc + solicitudes_pagoCreditos.valorAbono.
    END.

    W_VrCredACanc:SCREEN-VALUE = STRING(W_VrCredACanc).

    RUN MostrarAportes.

    CASE Solicitud.Per_Pago:
        WHEN 0 THEN
            ASSIGN vPeriodicidad = 0
                   W_NomPdo:SCREEN-VALUE = "Cuota diaria"
                   Cmb_PerPago:SCREEN-VALUE = "0 - Diaria"
                   Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 360)
                   Dias = Solicitud.Plazo * 1.

            WHEN 1 THEN
                ASSIGN vPeriodicidad = 1
                       W_NomPdo:SCREEN-VALUE = "Cuota semanal"
                       Cmb_PerPago:SCREEN-VALUE = "1 - Semanal"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 7)
                       Dias = Solicitud.Plazo * 7.

            WHEN 2 THEN
                ASSIGN vPeriodicidad = 2
                       W_NomPdo:SCREEN-VALUE = "Cuota decadal"
                       Cmb_PerPago:SCREEN-VALUE = "2 - Decadal"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 10)
                       Dias = Solicitud.Plazo * 10.

            WHEN 3 THEN
                ASSIGN vPeriodicidad = 3
                       W_NomPdo:SCREEN-VALUE = "Cuota quincenal"
                       Cmb_PerPago:SCREEN-VALUE = "3 - Quincenal"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 15)
                       Dias = Solicitud.Plazo * 15.

            WHEN 4 THEN
                ASSIGN vPeriodicidad = 4
                       W_NomPdo:SCREEN-VALUE = "Cuota mensual"
                       Cmb_PerPago:SCREEN-VALUE = "4 - Mensual"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 30)
                       Dias = Solicitud.Plazo * 30.

            WHEN 5 THEN
                ASSIGN vPeriodicidad = 5
                       W_NomPdo:SCREEN-VALUE = "Cuota bimensual"
                       Cmb_PerPago:SCREEN-VALUE = "5 - Bimestral"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 60)
                       Dias = Solicitud.Plazo * 60.

            WHEN 6 THEN
                ASSIGN vPeriodicidad = 6
                       W_NomPdo:SCREEN-VALUE = "Cuota trimestral"
                       Cmb_PerPago:SCREEN-VALUE = "6 - Trimestral"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 90)
                       Dias = Solicitud.Plazo * 90.

            WHEN 7 THEN
                ASSIGN vPeriodicidad = 7
                       W_NomPdo:SCREEN-VALUE = "Cuota cuatrimestral"
                       Cmb_PerPago:SCREEN-VALUE = "7 - Cuatrimestral"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 120)
                       Dias = Solicitud.Plazo * 120.

            WHEN 8 THEN
                ASSIGN vPeriodicidad = 8
                       W_NomPdo:SCREEN-VALUE = "Cuota semestral"
                       Cmb_PerPago:SCREEN-VALUE = "8 - Semestral"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 180)
                       Dias = Solicitud.Plazo * 180.

            WHEN 9 THEN
                ASSIGN vPeriodicidad = 9
                       W_NomPdo:SCREEN-VALUE = "Cuota anual"
                       Cmb_PerPago:SCREEN-VALUE = "9 - Anual"
                       Solicitud.Plazo:SCREEN-VALUE = STRING(INTEGER(Solicitud.Plazo) / 360)
                       Dias = Solicitud.Plazo * 360.
        END CASE.

        APPLY "leave" TO Solicitud.Nit.
        APPLY "leave" TO Solicitud.Plazo.

        W_TotExt = 0.

        FOR EACH Extras NO-LOCK WHERE Extras.Nit EQ Solicitud.Nit
                                  AND Extras.Num_Solicitud EQ Solicitud.Num_Solicitud:
            IF Extras.Estado EQ 1 THEN
                ASSIGN W_TotExt = W_TotExt + Extras.Vr_CuoExtra
                       W_TotExt:SCREEN-VALUE IN FRAME F_Solicitud = STRING(W_TotExt).
        END.

        APPLY "value-changed" TO Cmb_agencias.
        DISABLE Btn_Liquidar.
        ENABLE {&List-1}.
        
        IF Solicitud.Tip_Credito EQ 4 THEN
            Cmb_PerPago:SENSITIVE = TRUE.

        IF W_VrADesemb LE 0 THEN
            MESSAGE "El Valor a Desembolsar es Negativo o Cero(0), Debe Revisar los Valores a Cancelar."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    APPLY "choose" TO Btn_Salvar IN FRAME F_Creditos.

    DO WITH FRAME F_VblesS:
        FIND FIRST bfrAgencia NO-LOCK WHERE bfrAgencia.agencia = Clientes.agencia NO-ERROR.

        Clientes.gto_familiar:SCREEN-VALUE = string(fRngoSlrio(decimal(tot_ingresos:SCREEN-VALUE IN FRAME F_VblesS),Clientes.Per_ACargo,bfrAgencia.ciudad)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Negar_Solicitud wWin 
PROCEDURE Negar_Solicitud :
IF SUBSTRING(Cmb_Negadas:SCREEN-VALUE IN FRAME F_Ultima,1,5) EQ "00000" THEN DO:
   MESSAGE "No se ha escogido el motivo de nagación de la" SKIP
           "solicitud. escoja el motivo y salve de nuevo" VIEW-AS ALERT-BOX INFORMATION.
   APPLY "entry" TO Cmb_Negadas IN FRAME F_Ultima.
   RETURN NO-APPLY.
END.
    
CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo           = 9 
        Hoja_Vida.Codigo         = 1  
        Hoja_Vida.Instancia      = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
        Hoja_Vida.DoctoRefer     = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud)
        Hoja_Vida.Nit            = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud
        Hoja_Vida.Usuario        = W_Usuario
        Hoja_Vida.Fec_Grabacion  = W_Fecha
        Hoja_Vida.Hora_Grabacion = TIME
        Hoja_Vida.Observacion = 
        "Solicitud Negada por el Usuario: " + W_Usuario.
        Hoja_Vida.Asunto_Cumplido = YES.
            
FIND Mov_Instancias WHERE 
      Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
      Mov_Instancias.Nit       EQ STRING(Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud) AND
/*      Mov_Instancias.Usuario   EQ W_Usuario AND */
      Mov_Instancias.Num_Solicitud EQ integer(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) AND
      Mov_Instancias.Estado    EQ NO NO-ERROR.            
IF AVAILABLE Mov_Instancias THEN 
   ASSIGN Mov_Instancias.Estado      = YES
          Mov_Instancias.Fec_Retiro  = W_Fecha
          Mov_Instancias.Hora_Retiro = TIME.
FIND CURRENT solicitud EXCLUSIVE-LOCK NO-ERROR.
ASSIGN Solicitud.Estado       = 3
       Solicitud.Fec_Retiro   = W_fecha
       Solicitud.Cod_Negacion = DECIMAL(SUBSTRING(Cmb_Negadas:SCREEN-VALUE IN FRAME F_Ultima,1,5)).

/*Abril 15/05 GAER Inactiva relaciones y garantias*/
FOR EACH Relaciones WHERE 
           Relaciones.Nit           EQ Solicitud.Nit:
    IF INTEG(Relaciones.Cuenta)      EQ Solicitud.Num_Solicitud AND
           Relaciones.Clase_Producto EQ 2                      AND
           Relaciones.Cod_Producto   EQ Solicitud.Cod_Credito  AND
           Relaciones.Cod_relacion   EQ 11 THEN
    ASSIGN Relaciones.Estado = 2.
END.

FOR EACH Garantias WHERE Garantias.Agencia       EQ Solicitud.Agencia
                     AND Garantias.Cod_Credito   EQ Solicitud.Cod_Credito 
                     AND Garantias.Tip_Credito   EQ Solicitud.Tip_Credito
                     AND Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud :
    IF Garantias.Nit EQ Solicitud.Nit THEN
       ASSIGN Garantias.Estado = 2.
END.
/*Fin Abril 15/05*/

RUN Asignar_Negadas.
/*para imprimir el concepto de las negadas*/
ASSIGN W_TipoInforme = "Informe".
       
APPLY "choose" TO Btn_Imprimir IN FRAME F_Creditos.

RELEASE solicitud.

ENABLE ALL WITH FRAME F_Creditos.
DISABLE NomUsuario WITH FRAME F_Creditos.
ENABLE ALL WITH FRAME F_Consulta.
APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Creditos.
/*FIND Solicitud WHERE ROWID(Solicitud) EQ Puntero NO-LOCK NO-ERROR.
APPLY "choose" TO Btn_Consulta IN FRAME F_Creditos.       */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE o_InfrmeScringAExcel wWin 
PROCEDURE o_InfrmeScringAExcel :
/*------------------------------------------------------------------------------
  Purpose: ENVIA SCORING A EXCEL     
  Parameters:  <none>
  Notes:     InfrmeScringAExcel  
  LOG: CREADO, 21 DIC 2007, ING. Edilberto Mariño Moya
------------------------------------------------------------------------------*/


    DEF VAR i AS INTEGER NO-UNDO.    
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR cRngo AS char NO-UNDO.
    DEF VAR cRngo1 AS char NO-UNDO.

    /**************************************************************************************************************************************/
    
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR ct AS CHAR NO-UNDO.
    ct = FILL(CHR(1),3).
    DO WITH FRAME {&FRAME-NAME}:
        cl = "ANALISIS DEL SCORE. Créditos Por Nómina".
        fToExcelVlor("a1",cl).
        
        
        
        fToExcelVlor("a3","FECHA").
        fToExcelVlor("b3",ffchaltras(bdcentral.Solicitud.Fec_Solicitud)).
        

        fToExcelVlor("a4","NOMBRE DEL ASOCIADO").
        fToExcelVlor("b4",upper(NomNit:SCREEN-VALUE IN FRAME F_Solicitud)).

        fToExcelVlor("a5","IDENTIFICACION").
        fToExcelVlor("b5",solicitud.nit:SCREEN-VALUE IN FRAME F_Solicitud).

        fToExcelVlor("a6","MONTO").
        fToExcelVlor("b6",solicitud.monto:SCREEN-VALUE IN FRAME F_Solicitud).

        fToExcelVlor("a7","PLAZO").
        fToExcelVlor("b7",solicitud.plazo:SCREEN-VALUE IN FRAME F_Solicitud).

        fToExcelVlor("a8","LINEA").
        fToExcelVlor("b8",upper(Nom_Producto:SCREEN-VALUE IN FRAME F_Solicitud)).

        crngo1 = "a10:".
        fToExcelVlor("a10","INFORMACION SOCIODEMOGRAFICA").

        ftoexcelvlor("a11","VARIABLE").
        ftoexcelvlor("b11","DATOS").
        ftoexcelvlor("c11","PUNTAJE").

        i = 12.
        FOR EACH tscoring
            WHERE
                :
            ftoexcelvlor("a" + STRING(i),upper(replace(tscoring.vars,"_"," "))).
            ftoexcelvlor("b" + STRING(i),tscoring.vvas).
            ftoexcelvlor("c" + STRING(i),string(tscoring.puns)).
            ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
            i = i + 1.
        END.
        ftoexcelvlor("a" + string(i),"SUBTOTAL").
        ftoexcelvlor("c" + STRING(i),"=" + trim(ENTRY(1,ct,CHR(1)),"+")).
        ENTRY(1,ct,CHR(1)) = "".
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".

        i = i + 1.
        ftoexcelvlor("a" + string(i),"INFORMACION SOCIODEMOGRAFICA").
        crngo1 = crngo1 + "c" + STRING(i).
        
        i = i + 1.         
        FOR EACH tscoring
            WHERE
                tscoring.vars = "Capacidad_Pago":
            i = i + 1.
            ftoexcelvlor("a" + string(i),upper(replace(tscoring.vars,"_"," "))).
            ftoexcelvlor("b" + string(i),tscoring.vvas).
            ftoexcelvlor("c" + string(i),string(tscoring.puns)).
            ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        END.
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        crngo = "a" + STRING(i) + ":".
        ftoexcelvlor("a" + string(i),"HISTORICO DE PAGOS").

        i = i + 1.
        FOR EACH tscoring
            WHERE
                tscoring.vars = "Mora_Comercial":
            ftoexcelvlor("a" + string(i),"INTERNO").
            ftoexcelvlor("b" + string(i),tscoring.vvas).
            ftoexcelvlor("c" + string(i),string(tscoring.puns)).
            ENTRY(2,ct,CHR(1)) = ENTRY(2,ct,CHR(1)) + "c" + STRING(i) + "+".
        END.

        i = i + 1.
        ftoexcelvlor("a" + string(i),"SUBTOTAL HISTORICO DE PAGOS").
        ftoexcelvlor("c" + STRING(i),"=" + TRIM(ENTRY(2,ct,CHR(1)),"+")).
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        crngo = crngo + "c" + STRING(i).
        fbrdesrngo(crngo).
        
        i = i + 2.
        FOR EACH tscoring
            WHERE
                tscoring.vars = "garantia":
            ftoexcelvlor("a" + string(i),upper(replace(tscoring.vars,"_"," "))).
            ftoexcelvlor("b" + string(i),tscoring.vvas).
            ftoexcelvlor("c" + string(i),string(tscoring.puns)).
            ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        END.
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        ftoexcelvlor("a" + string(i),"TIPO DE VIVIENDA").
        ftoexcelvlor("b" + string(i),"0"). /* FALTA DEFINIR O PARAMETRIZAR */ 
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        ftoexcelvlor("a" + string(i),"FORMA DE PAGO").
        ftoexcelvlor("b" + string(i),string(0)). /* FALTA DEFINIR O PARAMETRIZAR */
        ENTRY(1,ct,CHR(1)) = ENTRY(1,ct,CHR(1)) + "c" + STRING(i) + "+".
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        ftoexcelvlor("a" + string(i),"TOTAL EVALUACION").
        ftoexcelvlor("c" + STRING(i),"=" + trim(ENTRY(1,ct,CHR(1)),"+")).
        fbrdesrngo("a" + STRING(i) + ":" + "c" + STRING(i)).
        
        i = i + 2.
        ftoexcelvlor("a" + string(i),"EVALUACION FINAL").
        ftoexcelvlor("c" + string(i),upper(w_concepto:SCREEN-VALUE IN FRAME F_Scoring)).
        crngo = "a" + STRING(i) + ":" + "c" + STRING(i).
        fbrdesrngo(crngo).
        
    END.

    fBrdesRngo("b3:b8").
    fBrdesRngo("a3:b8").
    fBrdesRngo(crngo1).

    /**************************************************************************************************************************************/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pdfSolicitud wWin 
PROCEDURE pdfSolicitud :
DEFINE VAR hPdf AS COM-HANDLE NO-UNDO.
DEFINE VAR l-file AS CHARACTER.

VIEW FRAME VisorSolicitud.
HIDE FRAME F_Solicitud.

Btn_IrProcesarInst:HIDDEN IN FRAME VisorSolicitud = YES.
IF PDFImpreso THEN
    Btn_IrProcesarInst:HIDDEN IN FRAME VisorSolicitud = FALSE.

RUN Rp_Solicitud.R (INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                    INPUT Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud). 

l-file = "Solicitudes\" + Solicitud.Nit + "_" + STRING(integer(Solicitud.Num_Solicitud:SCREEN-VALUE)) + ".pdf".
hPdf = chCtrlFrame-3:acroPDF.
hPdf:LoadFile(l-file).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesarDeducibles wWin 
PROCEDURE ProcesarDeducibles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TDeducc WHERE TDeducc.Cod_Deducible NE ?:
        IF TDeducc.valor > 0 THEN 
           /*CASE TDeducc.Cod_Deducible:
                WHEN "10" THEN  /* Seguro de Cartera    */ 
                      IF P_Linea = 46  OR P_Linea = 5   OR P_Linea = 540 OR P_Linea = 541 OR P_Linea = 542 OR P_Linea =  10 OR
                         P_Linea = 11  OR P_Linea = 12  OR P_Linea = 22  OR P_Linea = 524 OR P_Linea = 529 OR P_Linea =  55 OR
                         P_Linea = 15  OR P_Linea = 20  OR P_Linea = 30  OR P_Linea = 35  OR P_Linea =  40 OR P_Linea =  45 OR
                         P_Linea = 48  OR P_Linea = 49  OR P_Linea = 50  OR P_Linea = 51  OR P_Linea = 52  OR P_Linea =  53 OR
                         P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR P_Linea = 65  OR P_Linea =  80 OR
                         P_Linea = 85  OR P_Linea = 90  OR P_Linea = 95  OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 OR
                         P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR P_Linea = 571 OR P_Linea = 572 OR P_Linea = 573 OR
                         P_Linea = 574 OR P_Linea = 13  OR P_Linea = 14  OR P_Linea = 33 OR P_Linea = 575 THEN DO:
                         WDed = WDed + ((DEC(Solicitud.Monto:SCREEN-VALUE IN FRAME F_Solicitud) -
                                         DEC(W_TotCanc:SCREEN-VALUE IN FRAME f_CredAcanc)) * TDeducc.Valor).                                                              .
                      END.
                WHEN "16" THEN  /* Gasto de Papeleria */
                      IF P_Linea =  46 OR P_Linea =   5 OR P_Linea = 540 OR P_Linea = 541 OR P_Linea = 542 OR P_Linea =  10 OR
                         P_Linea =  11 OR P_Linea =  12 OR P_Linea =  22 OR P_Linea = 524 OR P_Linea = 529 OR P_Linea =  55 OR
                         P_Linea =  15 OR P_Linea =  20 OR P_Linea =  30 OR P_Linea =  35 OR P_Linea =  40 OR P_Linea =  45 OR
                         P_Linea =  48 OR P_Linea =  49 OR P_Linea =  50 OR P_Linea =  51 OR P_Linea =  52 OR P_Linea =  53 OR
                         P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR P_Linea =  65 OR P_Linea =  80 OR
                         P_Linea =  85 OR P_Linea =  90 OR P_Linea =  95 OR P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 OR
                         P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR P_Linea = 571 OR P_Linea = 572 OR P_Linea = 573 OR
                         P_Linea = 574 OR P_Linea =  13 OR P_Linea =  14 OR P_Linea =  33 OR P_Linea = 575 THEN DO:
                      /* Mayor A 10SM HASTA 30 SM (INDICADORES) = 4900 */
                         IF DEC(Solicitud.Monto:SCREEN-VALUE) LE (461500 * 10) THEN ASSIGN WDed = WDed + 000 wvlres[2] = 000.
                         ELSE
                         IF DEC(Solicitud.Monto:SCREEN-VALUE) LE (461500 * 30) THEN ASSIGN WDed = WDed + 00  wvlres[2] =  00.
                         ELSE ASSIGN WDed = WDed + 00 wvlres[2] = 00.
                      END.
                WHEN "4" THEN  /* Gasto de Administracion */
                      IF P_Linea = 5   OR P_Linea = 10  OR P_Linea = 11  OR P_Linea = 12  OR P_Linea = 22  OR P_Linea = 524 OR
                         P_Linea = 529 OR P_Linea = 55  OR P_Linea = 15  OR P_Linea = 20  OR P_Linea = 30  OR P_Linea = 35  OR
                         P_Linea = 40  OR P_Linea = 45  OR P_Linea = 48  OR P_Linea = 49  OR P_Linea = 50  OR P_Linea = 51  OR
                         P_Linea = 52  OR P_Linea = 53  OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea = 60  OR
                         P_Linea = 65  OR P_Linea = 80  OR P_Linea = 85  OR P_Linea = 90  OR P_Linea = 95  OR P_Linea = 521 OR
                         P_Linea = 522 OR P_Linea = 523 OR P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR P_Linea = 571 OR
                         P_Linea = 572 OR P_Linea = 573 OR P_Linea = 574 OR P_Linea = 13  OR P_Linea = 14  OR P_Linea = 33  OR
                         P_Linea = 575 THEN DO:
                      /* Mayor A 10SM HASTA 30 SM (INDICADORES) = 4900 */
                         IF DEC(Solicitud.Monto:SCREEN-VALUE) LE (461500 * 10) THEN ASSIGN WDed = WDed + 00 wvlres[3] = 00.
                         ELSE
                         IF DEC(Solicitud.Monto:SCREEN-VALUE) LE (461500 * 30) THEN ASSIGN WDed = WDed + 00 wvlres[3] = 00.
                         ELSE ASSIGN WDed = WDed + 00 wvlres[3] = 00.
                      /* IF P_Monto GE (461500 * 30) THEN   MAYOR 30 SM 14900 */
                      END.
                WHEN "2" THEN  /*  Delphy */
                      IF P_Linea = 25  OR  P_Linea = 5  OR P_Linea =  10 OR P_Linea = 11  OR P_Linea = 12  OR P_Linea = 22  OR 
                         P_Linea = 524 OR P_Linea = 529 OR P_Linea =  55 OR P_Linea =  15 OR P_Linea =  20 OR P_Linea = 30  OR 
                         P_Linea =  35 OR P_Linea =  40 OR P_Linea =  45 OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR
                         P_Linea =  51 OR P_Linea = 52  OR P_Linea =  53 OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR 
                         P_Linea =  60 OR P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90  OR P_Linea =  95 OR 
                         P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 OR P_Linea = 571 OR P_Linea = 572 OR P_Linea = 573 OR 
                         P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR P_Linea = 574 OR P_Linea = 13  OR P_Linea = 14  OR
                         P_Linea = 33  OR P_Linea = 575 THEN DO:
                         IF P_ForPag = 1 THEN
                            ASSIGN WDed = WDed + (TDeducc.Valor * WfactorCod) wvlres[4] = (TDeducc.Valor * WfactorCod).
                      END.
                WHEN "5" THEN /* Sifin */
                      IF P_Linea = 25  OR  P_Linea = 5  OR P_Linea = 10  OR P_Linea = 11  OR P_Linea = 12  OR P_Linea = 22  OR 
                         P_Linea = 524 OR P_Linea = 529 OR P_Linea = 55  OR P_Linea =  15 OR P_Linea =  20 OR P_Linea = 30  OR
                         P_Linea =  35 OR P_Linea =  40 OR P_Linea = 45  OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR
                         P_Linea =  51 OR P_Linea = 52  OR P_Linea = 53  OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR 
                         P_Linea =  60 OR P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90  OR P_Linea =  95 OR 
                         P_Linea = 521 OR P_Linea = 522 OR P_Linea = 523 OR P_Linea = 571 OR P_Linea = 572 OR P_Linea = 573 OR 
                         P_Linea = 543 OR P_Linea = 544 OR P_Linea = 563 OR P_Linea = 574 OR P_Linea = 13  OR P_Linea = 14  OR 
                         P_Linea = 33  OR P_Linea = 575 THEN DO:
                         IF P_ForPag = 2 AND DEC(Solicitud.Monto:SCREEN-VALUE) GT (461500 * 30) THEN 
                            ASSIGN WDed = WDed + (TDeducc.Valor * WfactorCod) wvlres[5] = (TDeducc.Valor * WfactorCod).
                      END.
                WHEN "15" THEN /* Cuota de Afiliacion */
                      IF P_Linea = 5   OR  P_Linea = 10 OR P_Linea = 11  OR P_Linea = 12  OR P_Linea = 22  OR P_Linea = 524 OR
                         P_Linea = 529 OR P_Linea = 55  OR P_Linea = 15  OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR
                         P_Linea =  40 OR P_Linea = 45  OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR 
                         P_Linea = 52  OR P_Linea = 53  OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR 
                         P_Linea =  65 OR P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90  OR P_Linea =  95 OR P_Linea = 521 OR
                         P_Linea = 522 OR P_Linea = 523 OR P_Linea = 571 OR P_Linea = 572 OR P_Linea = 573 OR P_Linea = 543 OR
                         P_Linea = 544 OR P_Linea = 563 OR P_Linea = 574 OR P_Linea = 13  OR P_Linea = 14  OR P_Linea = 33  OR
                         P_Linea = 575 THEN DO:
                         FIND FIRST creditos WHERE creditos.nit = P_CedNit NO-LOCK NO-ERROR.
                         IF NOT AVAIL(creditos) THEN DO:
                            FIND FIRST  Ahorros WHERE Ahorros.nit = P_CedNit AND Ahorros.tip_ahorro = 4
                                   AND (Ahorros.sdo_disponible + Ahorros.sdo_canje) GT 0 NO-LOCK NO-ERROR.
                            IF NOT AVAIL(Ahorros) THEN ASSIGN WDed = WDed + TDeducc.Valor wvlres[6] = TDeducc.Valor.
                         END.
                      END.
                WHEN "6" THEN /* impuesto de timbre */
                      IF P_Linea = 10  OR P_Linea = 11  OR P_Linea = 12  OR P_Linea = 22  OR P_Linea = 524 OR P_Linea = 529 OR
                         P_Linea = 55  OR P_Linea = 15  OR P_Linea =  20 OR P_Linea = 30  OR P_Linea =  35 OR P_Linea =  40 OR
                         P_Linea = 45  OR P_Linea = 48  OR P_Linea =  49 OR P_Linea = 50  OR P_Linea =  51 OR P_Linea = 52  OR
                         P_Linea = 53  OR P_Linea = 526 OR P_Linea = 527 OR P_Linea = 528 OR P_Linea =  60 OR P_Linea =  65 OR
                         P_Linea =  80 OR P_Linea =  85 OR P_Linea = 90  OR P_Linea =  95 OR P_Linea = 521 OR P_Linea = 522 OR
                         P_Linea = 523 OR P_Linea = 571 OR P_Linea = 572 OR P_Linea = 573 OR P_Linea = 543 OR P_Linea = 544 OR
                         P_Linea = 563 OR P_Linea = 574 OR P_Linea = 13  OR P_Linea = 14  OR P_Linea = 33  OR P_Linea = 575 THEN DO:
                         IF DEC(Solicitud.Monto:SCREEN-VALUE) GT 125844000 THEN
                            ASSIGN WDed = WDed + ROUND(DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor,0)
                                   wvlres[7] = ROUND(DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor,0).
                      END.
                WHEN "3" THEN /* Estudio de Credito */
                      IF P_Linea = 575 THEN DO:
                         IF ROUND(DEC(Solicitud.Monto:SCREEN-VALUE) * 0.0,0) GT 100000
                         THEN ASSIGN wded = wded + 00 wvlres[9] = 00.
                         ELSE ASSIGN wded = wded + ROUND(DEC(Solicitud.Monto:SCREEN-VALUE) * 0.0,0)
                                     wvlres[9] = ROUND(DEC(Solicitud.Monto:SCREEN-VALUE) * 0.0,0).
                      END.
                OTHERWISE DO:
                      IF TDeducc.Id_Deducible EQ 0 THEN DO: /*financiados*/
                         IF TDeducc.Cla_Deducible EQ 1 THEN DO: /*porcentaje*/
                            ASSIGN TOTWMpr = TOTWMpr + (DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor)
                                   WDed = WDed + (DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor). 
                            wvlres[8] = (DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor).
                         END.
                         ELSE DO:
                            ASSIGN TOTWMpr = TOTWMpr + TDeducc.Valor WDed = WDed + TDeducc.Valor. 
                            wvlres[8] = TDeducc.Valor.
                         END.
                      END.
                      ELSE DO: /*Descontados*/
                         IF TDeducc.Cla_Deducible EQ 1 THEN /*porcentaje*/ DO:
                            ASSIGN TOTWMpr = TOTWMpr - (DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor)
                                   WDed = WDed + (DEC(Solicitud.Monto:SCREEN-VALUE) * TDeducc.Valor).
                            wvlres[8] = TDeducc.Valor.
                         END.
                         ELSE DO:
                            ASSIGN TOTWMpr = TOTWMpr - TDeducc.Valor WDed = WDed + TDeducc.Valor.
                            wvlres[8] = TDeducc.Valor.
                         END.
                      END.        
                END.
           END CASE.*/
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF W_SiMInst THEN DO:
    RUN Imp_MovInst.
    W_SiMInst = FALSE.
    RETURN.
END.

IF W_TipoInforme = "Proyeccion" THEN
    RUN Proyeccion.
ELSE
    RUN Informe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Scoring wWin 
PROCEDURE Proceso_Scoring :
DO WITH FRAME F_Scoring:
        TOTAL_Puntaje = 0.
        RUN Prc_LlenarScoring.p (INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                                 INPUT Solicitud.Num_Solicitud, 
                                 INPUT Solicitud.Nit:SCREEN-VALUE,
                                 decimal(w_vrcuota:SCREEN-VALUE IN FRAME F_VblesS),
                                 DECpcdadPgo).
        /*
        OUTPUT TO c:\tmp\exito.csv.
        EXPORT DELIMITER ";" "agencia" "Codigo" "Fec_Scoring" "Nit" "Num_Solicitud" "Puntaje" "Tabla" "Usuario" "Valor_variable" "Variable".
        FOR EACH scoring NO-LOCK
            WHERE
                scoring.nit = Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud:
            EXPORT DELIMITER ";" scoring.agencia Codigo Fec_Scoring scoring.Nit scoring.Num_Solicitud scoring.Puntaje Tabla scoring.Usuario Valor_variable Variable.
        END.
        QUIT.
        */
        FOR EACH TScoring: DELETE TScoring. END.
        CLOSE QUERY Br_Scoring.
        FOR EACH Scoring 
            WHERE 
                Scoring.Nit           EQ Solicitud.Nit:SCREEN-VALUE 
            AND Scoring.Fec_Scoring   EQ W_Fecha 
            AND Scoring.Num_Solicitud EQ Solicitud.Num_Solicitud NO-LOCK:
            TOTAL_Puntaje = TOTAL_Puntaje + Scoring.Puntaje.
            FIND FIRST pro_scoring NO-LOCK
                WHERE
                    pro_scoring.observacion MATCHES "*" + entry(1,scoring.VARIABLE,"(") NO-ERROR.
            CREATE TScoring.
            ASSIGN  TScoring.VarS = Scoring.VARIABLE
                    TScoring.TabS = Scoring.Tabla
                    TScoring.VVaS = Scoring.Valor_Variable
                    TScoring.PunS = Scoring.Puntaje
                    TScoring.FecS = Scoring.Fec_Scoring
                    TScoring.CodS = Scoring.Codigo 
                    tscoring.path = IF AVAILABLE pro_scoring THEN fpath(pro_scoring.observacion) ELSE "".
            /*
            IF Scoring.VARIABLE EQ "Tip_Contrato" 
            THEN DO:
                IF TScoring.VVaS EQ "0" 
                THEN TScoring.VVaS = "Ninguno-0".
                ELSE 
                IF TScoring.VVaS EQ "1" 
                THEN TScoring.VVaS = "T.Indef-1".
                ELSE 
                IF TScoring.VVaS EQ "2" 
                THEN TScoring.VVaS = "Fijo-2".
                ELSE 
                IF TScoring.VVaS EQ "3" 
                THEN TScoring.VVaS = "L.Contratada-3".
                ELSE 
                IF TScoring.VVaS EQ "4" 
                THEN TScoring.VVaS = "P.Servicios-4".
                ELSE 
                IF TScoring.VVaS EQ "5" 
                THEN TScoring.VVaS = "Pensionado-5".
            END.
            */
        END.
        
        OPEN QUERY Br_Scoring FOR EACH TScoring NO-LOCK BY path INDEXED-REPOSITION.
        DISPLAY TOTAL_Puntaje WITH FRAME F_Scoring.
        
        FIND Clientes exclusive-lock
            WHERE 
                Clientes.Nit = Solicitud.Nit NO-ERROR.
        DO WHILE LOCKED(clientes):
            MESSAGE "Cliente " clientes.nit " Está Siendo Usado Por Otro Usuario." SKIP
                    "Espere Por Favor" 
                VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "INFORMACION".
            FIND Clientes exclusive-lock
                WHERE 
                    Clientes.Nit = Solicitud.Nit NO-ERROR.
        END.
        
        ASSIGN  Clientes.Puntaje = TOTAL_Puntaje
                Nom_CteCod:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        
        FIND CURRENT Clientes NO-LOCK NO-ERROR.
        
        /*
        FIND FIRST Cfg_RegCredito 
            WHERE 
                Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
        IF  Cfg_RegCredito.Agencia_Exigida  EQ 11                 /*Enero 19/06 GAER*/
        AND Pro_Creditos.Tip_Credito        EQ 4 
        THEN FIND Varios WHERE  Varios.Tipo   EQ 19 AND Varios.Codigo  EQ 2 NO-LOCK NO-ERROR.
        ELSE FIND Varios WHERE Varios.Tipo   EQ 19  AND Varios.Codigo EQ 1 NO-LOCK NO-ERROR.
        
        IF TOTAL_Puntaje LT Varios.Val_Inicial 
        THEN ASSIGN W_Concepto:SCREEN-VALUE = "Negada"
                    W_Concepto.
        ELSE 
        IF TOTAL_Puntaje LT Varios.Val_Final 
        THEN ASSIGN W_Concepto:SCREEN-VALUE = "Franja de Anàlisis"
                    W_Concepto.
        ELSE 
        IF TOTAL_Puntaje GE Varios.Val_Final 
        THEN ASSIGN W_Concepto:SCREEN-VALUE = "Aprobada"
                    W_Concepto.           
        */
        FIND FIRST tEscnrioEcnmco NO-LOCK
            WHERE
                tEscnrioEcnmco.ps >= TOTAL_Puntaje NO-ERROR.
        IF AVAILABLE tEscnrioEcnmco
        THEN DO:
            ASSIGN  W_Concepto:SCREEN-VALUE = tEscnrioEcnmco.Clfccion
                    W_Concepto. 
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion wWin 
PROCEDURE Proyeccion :
DEFINE VAR W_NomPer AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_PdoAno AS INTEGER.
DEFINE VAR plazow AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR P_NMeses AS INTEGER.

RUN HallarPeriodo IN W_ManFin (INPUT INT(SUBSTR(Cmb_PerPago:SCREEN-VALUE IN FRAME f_Solicitud,1,1)),
                               INPUT PlazoW,
                               OUTPUT W_NroDias,
                               OUTPUT P_NMeses,
                               OUTPUT W_PdoAno,
                               OUTPUT W_NomPer).

RUN Proyeccion_Credito.R(INPUT DECIMAL(Solicitud.Monto:SCREEN-VALUE IN FRAME F_Solicitud),
                         INPUT DECIMAL(Solicitud.Plazo:SCREEN-VALUE IN FRAME F_Solicitud),
                         INPUT DECIMAL(Solicitud.Cuota:SCREEN-VALUE IN FRAME F_Solicitud),
                         INPUT DATE(Solicitud.fec_desembolso:SCREEN-VALUE),
                         INPUT DATE(Solicitud.fec_primerPago:SCREEN-VALUE),
                         INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
                         INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
                         INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)),
                         INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                         INPUT NomNit:SCREEN-VALUE IN FRAME F_Solicitud,
                         INPUT INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)),
                         INPUT SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,7,30),
                         INPUT "S",
                         INPUT DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud),
                         INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,9,30),
                         INPUT Solicitud.Monto).

W_TipoInforme = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Query_Relaciones wWin 
PROCEDURE Query_Relaciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TCode: DELETE TCode. END.
  CLOSE QUERY Br_Codeudores.

  FOR EACH Relaciones WHERE 
           Relaciones.Nit EQ Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud  AND
           Relaciones.Cuenta EQ Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud AND
           Relaciones.Clase_Producto EQ 2 AND
           Relaciones.Cod_Producto EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)) AND
           Relaciones.Cod_relacion EQ 11 NO-LOCK:
      FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN DO:
        CREATE TCode.
        ASSIGN TCode.TC_AgeCode = Clientes.Agencia
               TCode.TC_NitCode = Clientes.Nit
               TCode.TC_NitDeud = Relaciones.Nit
               TCode.TC_NumSoli = DECIMAL(Relaciones.Cuenta)
               TCode.TC_NomCode = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               TCode.TC_TelCdRs = Clientes.Tel_Residencia
               TCode.TC_TelCdCo = Clientes.Tel_Comercial
               TCode.TC_EmlCode = Clientes.email
               TCode.TC_EstRela = Relaciones.Estado
               TCode.TC_FecCrea = Relaciones.Fec_Ingreso
               TCode.TC_FecReti = Relaciones.Fec_Inactividad
               TCode.TC_Aprob   = Relaciones.Aprobada.
      END.
  END.

  OPEN QUERY Br_Codeudores FOR EACH TCode WHERE TCode.TC_EstRela EQ 1 NO-LOCK INDEXED-REPOSITION.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SInfo wWin 
PROCEDURE SInfo :
DEFINE INPUT PARAMETER texto AS CHARACTER FORMAT "X(60)".
/*     W_Ok = S_InfoCliente_old:ADD-LAST(texto) IN FRAME F_InfoCliente. */
    ASSIGN S_InfoCliente:SCREEN-VALUE IN FRAME F_InfoCliente = S_InfoCliente:SCREEN-VALUE + texto + CHR(10).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Solicitudes_X_Instancia wWin 
PROCEDURE Solicitudes_X_Instancia :
FOR EACH Consulta: DELETE Consulta. END.
    
  FOR EACH Mov_Instancias WHERE
           Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)) AND
           /*Mov_Instancias.Usuario   EQ W_Usuario AND*/
           Mov_Instancias.Agencia = W_agencia AND
           Mov_Instancias.Estado    EQ NO NO-LOCK:
     FIND Solicitud WHERE Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                          Solicitud.Nit           EQ Mov_Instancias.Nit           AND
                          Solicitud.Estado        NE 2 NO-LOCK NO-ERROR.
     IF AVAILABLE Solicitud THEN DO:
       CREATE Consulta.
       ASSIGN Consulta.AgeSolicitud  = Solicitud.Agencia
              Consulta.Estado        = Solicitud.Estado
              Consulta.Num_Solicitud = Mov_Instancias.Num_Solicitud
              Consulta.Fec_Ingreso   = Mov_Instancias.Fec_Ingreso
              Consulta.Hor_Ingreso   = STRING(Mov_Instancias.Hora_Ingreso,"HH:MM:SS am")
              Consulta.Vigencia      = w_fecha - Mov_Instancias.Fec_Ingreso
              Consulta.Monto         = Solicitud.Monto.
       FIND Clientes WHERE Clientes.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN
          ASSIGN Consulta.Nit         = Clientes.Nit
                 Consulta.Nombre      = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     END.
     ELSE DO:
       MESSAGE "El Movimiento de Instancias apunta a una Solicitud" SKIP
               "inexistente. Comunique esta anomalia al Administrador" SKIP
               "Num.Solicitud: " Mov_Instancias.Num_Solicitud SKIP
               "Nit: " Mov_Instancias.Nit 
               VIEW-AS ALERT-BOX.
     END.
  END.
  CASE R_Organizar:SCREEN-VALUE IN FRAME F_Consulta:
    WHEN "1" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Solicitud INDEXED-REPOSITION.
    WHEN "2" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.AgeSolicitud INDEXED-REPOSITION.
    WHEN "3" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Nit INDEXED-REPOSITION.
    WHEN "4" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Nombre INDEXED-REPOSITION.
    WHEN "5" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Fec_Ingreso INDEXED-REPOSITION.
    WHEN "6" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Vigencia DESCENDING INDEXED-REPOSITION.
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TmpL wWin 
PROCEDURE TmpL :
DEFINE INPUT PARAMETER Linea AS INTEGER FORMAT "99".
DEFINE INPUT PARAMETER Texto AS CHARACTER FORMAT "X(125)".

CREATE TmpI.
ASSIGN TmpI.ILinea = Linea
       TmpI.ITexto = Texto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ttlza wWin 
PROCEDURE Ttlza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME  F_VblesS:
        ASSIGN Tot_Ingresos =   DEC(Clientes.Ing_Arriendos:SCREEN-VALUE)  + 
                                DEC(Clientes.Ing_Financieros:SCREEN-VALUE) +   
                                DEC(Clientes.Ing_Honorarios:SCREEN-VALUE) + 
                                DEC(Clientes.Ing_Otros:SCREEN-VALUE)       + 
                                DEC(Clientes.Salario:SCREEN-VALUE)
               Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos).

        ASSIGN Tot_Egresos =    DEC(Clientes.Gto_obligacion:SCREEN-VALUE) + 
                                DEC(Clientes.Gto_Familiar:SCREEN-VALUE) + 
                                DEC(Clientes.Gto_Arriendo:SCREEN-VALUE)   + 
                                DEC(Clientes.Sdo_obligacion:SCREEN-VALUE) + 
                                DEC(Clientes.gtofinanc_indir:SCREEN-VALUE)  
               Tot_Egresos:SCREEN-VALUE  = STRING(Tot_Egresos).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Usuarios_ant_instancia wWin 
PROCEDURE Usuarios_ant_instancia :
/*------------------------------------------------------------------------------
  Purpose: ubica los usuarios que corresponden a la instancia anterior.     
  Parameters:  <none>
  Notes: Para identificar la instancia de analisis se opta por buscar la instancia donde se encuentra habilitada la opcion de scoring y esta diferente a ultima      
  fecha: 07/01/2005
------------------------------------------------------------------------------*/

FOR EACH TUxi: DELETE Tuxi. END.
  ASSIGN FRAME F_Condicionada Cmb_InsCon.
  find FIRST instancia  WHERE instancia.id_scoring AND NOT ultima NO-LOCK NO-ERROR.
  IF AVAIL(instancias) THEN DO:
      FOR EACH Cfg_Instancias WHERE
          Cfg_Instancias.Tipo_Instancia EQ 1  AND
          Cfg_Instancias.Instancia      EQ Instancias.instancia AND
          Cfg_Instancias.Estado         EQ 1 NO-LOCK:
  
        FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
          IF AVAILABLE Usuarios THEN DO:
               CREATE TUxi.
                ASSIGN  Tuxi.Agencia = Usuarios.Agencia
                        Tuxi.Usuario = Usuarios.Usuario
                        Tuxi.Nombre  = Usuarios.Nombre.
                FOR EACH Mov_Instancias WHERE
                         Mov_instancias.Instancia EQ Cfg_Instancias.Instancia AND
                         Mov_Instancias.Usuario   EQ Usuarios.Usuario AND
                         Mov_Instancias.Estado    EQ NO NO-LOCK:
                         Tuxi.Cantidad = Tuxi.Cantidad + 1.
                END.
                FOR EACH  Mov_Instancias WHERE 
                          Mov_Instancias.Instancia EQ Cfg_Instancias.Instancia AND
                          Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-LOCK:
                          FIND Tuxi WHERE TUxi.Usuario EQ Mov_Instancias.Usuario NO-ERROR.
                          IF AVAILABLE Tuxi THEN Tuxi.Proceso = YES.
                END.
        END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Usuarios_X_Instancia wWin 
PROCEDURE Usuarios_X_Instancia :
FOR EACH TUxi: DELETE Tuxi. END.
  ASSIGN FRAME F_Condicionada Cmb_InsCon.

  SESSION:SET-WAIT-STATE("General").

  FOR EACH Cfg_Instancias WHERE
            Cfg_Instancias.Agencia        EQ Solicitud.Agencia AND
            Cfg_Instancias.Tipo_Instancia EQ 1  AND
           (Cfg_Instancias.Instancia      EQ 20 OR Cfg_Instancias.Instancia EQ 30 OR
            Cfg_Instancias.Instancia      EQ 70) AND 
            Cfg_Instancias.Estado         EQ 1  NO-LOCK:

         FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario 
                        AND  Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.
         IF AVAILABLE Usuarios THEN DO:                                                                                         
            CREATE TUxi.                                                                                                        
            ASSIGN Tuxi.Agencia = Solicitud.Agencia                                                                              
                   Tuxi.Usuario = Usuarios.Usuario                                                                              
                   Tuxi.Nombre  = Usuarios.Nombre
                   Tuxi.Instanc = Cfg_Instancias.Instancia.
                                                                                                                                
            FOR EACH Mov_Instancias WHERE                                                                                       
                     Mov_instancias.Instancia EQ Cfg_Instancias.Instancia AND                                                   
                     Mov_Instancias.Usuario   EQ Usuarios.Usuario AND                                                           
                     Mov_Instancias.Estado    EQ NO NO-LOCK:                                                                    
               Tuxi.Cantidad = Tuxi.Cantidad + 1.                                                                               
            END.                                                                                                                
                                                                                                                                
            FOR EACH Mov_Instancias WHERE                                                                                       
                     Mov_Instancias.Instancia     EQ Cfg_Instancias.Instancia AND                                               
                     Mov_Instancias.Num_Solicitud EQ INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud) NO-LOCK:
                     FIND Tuxi WHERE TUxi.Usuario EQ Mov_Instancias.Usuario NO-ERROR.                                           
                     IF AVAILABLE Tuxi THEN                                                                                     
                        Tuxi.Proceso = YES.                                                                                     
            END.                                                                                                                
         END.        
  END.     

  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Solicitud wWin 
PROCEDURE Validar_Solicitud :
DEFINE VAR plazoMeses AS INTEGER.
DEFINE VAR vTotalDesembolsado AS DECIMAL.
DEFINE VAR pAutorizacion AS LOGICAL.
DEFINE VAR pUsuarioAutoriza AS CHARACTER.

CASE INTEGER(SUBSTRING(cmb_perPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)):
    WHEN 0 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE) / 30.
    WHEN 1 THEN plazoMeses = (DECIMAL(solicitud.plazo:SCREEN-VALUE) * 7) / 30.
    WHEN 2 THEN plazoMeses = (DECIMAL(solicitud.plazo:SCREEN-VALUE) * 10) / 30.
    WHEN 3 THEN plazoMeses = (DECIMAL(solicitud.plazo:SCREEN-VALUE) * 15) / 30.
    WHEN 4 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE).
    WHEN 5 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE) * 2.
    WHEN 6 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE) * 3.
    WHEN 7 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE) * 4.
    WHEN 8 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE) * 6.
    WHEN 9 THEN plazoMeses = DECIMAL(solicitud.plazo:SCREEN-VALUE) * 12.
END CASE.


IF AVAILABLE pro_creditos THEN DO:
    IF pro_creditos.id_montoMinimo = TRUE THEN DO:
        IF DECIMAL(solicitud.monto:SCREEN-VALUE) < pro_creditos.val_montoMinimo THEN DO:
            MESSAGE "El monto solicitado es inferior al monto mínimo permitido para este producto." STRING(pro_creditos.val_MontoMinimo,"$->>>,>>>,>>>,>>9.99") SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    IF pro_creditos.id_montoMaximo = TRUE THEN DO:
        IF DECIMAL(solicitud.monto:SCREEN-VALUE) > pro_creditos.val_montoMaximo THEN DO:
            MESSAGE "El monto solicitado es superior al monto máximo permitido para este producto." STRING(pro_creditos.val_montoMaximo,"$->>>,>>>,>>>,>>9.99") SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    IF pro_creditos.id_plazo = TRUE THEN DO:
        IF plazoMeses < pro_creditos.pla_minimo THEN DO:
            MESSAGE "El plazo (en meses) solicitado es inferior al plazo mínimo permitido para este producto." STRING(pro_creditos.pla_minimo,"$->>>,>>>,>>>,>>9.99") SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    IF pro_creditos.id_plazo = TRUE THEN DO:
        IF plazoMeses > pro_creditos.pla_maximo THEN DO:
            MESSAGE "EL plazo (en meses) solicitado es  superior al plazo máximo permitido para este producto." STRING(pro_creditos.pla_maximo,"$->>>,>>>,>>>,>>9.99")SKIP
                    "Revise las condiciones de la solicitud."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            RETURN ERROR.
        END.
    END.

    /* Control 9.000'000.000 para el crédito de Vivienda */
    IF pro_creditos.cod_credito = 189 THEN DO:
        FOR EACH creditos WHERE creditos.cod_credito = 189 NO-LOCK:
            vTotalDesembolsado = vTotalDesembolsado + creditos.monto.
        END.

        IF vTotalDesembolsado >= 9000000000 THEN DO:
            MESSAGE "El Fondo destinado para la línea" pro_creditos.Nom_Producto "se ha agotado." SKIP
                    "No se reciben más solicitudes para este concepto..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.
    END.

    /* Control 1.500'000.000 para el crédito de Alta Liquidez - 11/07/2019 */
    IF pro_creditos.cod_credito = 57 THEN DO:
        FOR EACH creditos WHERE creditos.cod_credito = 57 AND creditos.fec_desembolso >= 09/10/2019 NO-LOCK:
            vTotalDesembolsado = vTotalDesembolsado + creditos.monto.
        END.

        IF vTotalDesembolsado >= 2000000000 THEN DO:
            MESSAGE "El Fondo destinado para la línea" pro_creditos.Nom_Producto "se ha agotado." SKIP
                    "No se reciben más solicitudes para este concepto..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.
    END.

    /* Control para el crédito de Solidaridad - Si el asociado posee un crédito activo por esta línea, se solicita autorización para la grabación de la solicitud */
    IF pro_creditos.cod_credito <> 108 AND
       pro_creditos.cod_credito <> 113 AND
       pro_creditos.cod_credito <> 114 AND
       pro_creditos.cod_credito <> 190 THEN DO:
        FIND FIRST creditos WHERE creditos.nit = solicitud.nit:SCREEN-VALUE
                              AND creditos.cod_credito = 158
                              AND creditos.estado = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            FIND FIRST hoja_vida WHERE hoja_vida.nit = Solicitud.Nit:SCREEN-VALUE
                                   AND hoja_vida.tipo = 9
                                   AND hoja_vida.codigo = 1
                                   AND hoja_vida.doctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE)
                                   AND hoja_vida.instancia <= INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5))
                                   AND hoja_vida.usuario <> w_usuario
                                   AND hoja_vida.observacion = "Solicitud autorizada por " + hoja_vida.usuario NO-LOCK NO-ERROR.
            IF NOT AVAILABLE hoja_vida THEN DO:
                MESSAGE "El Asociado tiene un Crédito de Solidaridad vigente. por esta razón" SKIP
                        "no podrá solicitar ninguna otra línea de crédito (Resolución 7 de 2019)." SKIP
                        "Se requiere autorización para realizar esta operación."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RUN p-AutorizacionTransaccion IN w_manija (INPUT 6,
                                                           OUTPUT pAutorizacion,
                                                           OUTPUT pUsuarioAutoriza).

                IF pAutorizacion = TRUE THEN DO:
                    CREATE Hoja_Vida.
                    Hoja_Vida.Tipo = 9.
                    Hoja_Vida.Codigo = 1.
                    Hoja_Vida.Instancia = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,5)).
                    Hoja_Vida.DoctoRefer = INTEGER(Solicitud.Num_Solicitud:SCREEN-VALUE).
                    Hoja_Vida.Nit = Solicitud.Nit:SCREEN-VALUE.
                    Hoja_Vida.Usuario = W_Usuario.
                    Hoja_Vida.Fec_Grabacion = w_fecha.
                    Hoja_Vida.Hora_Grabacion = TIME.
                    Hoja_Vida.Asunto_Cumplido = YES.
                    Hoja_Vida.Observacion = "Solicitud autorizada por " + pUsuarioAutoriza.
                END.
                ELSE
                    RETURN ERROR.
            END.
        END.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Vbles_XCodeud wWin 
PROCEDURE Vbles_XCodeud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 1 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Edad).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Edad).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Edad).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 2 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Per_ACargo).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Per_ACargo).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Per_ACargo).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 3 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Tipo_Activid).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Tipo_Activid).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Tipo_Activid).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 4 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Tip_Contrato).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Tip_Contrato).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Tip_Contrato).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 5 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Endeud_Indirecto).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Endeud_Indirecto).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Endeud_Indirecto).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 6 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Respaldo_Patrim).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Respaldo_Patrim).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Respaldo_Patrim).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 7 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Mora_Comercial).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Mora_Comercial).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Mora_Comercial).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 8 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Est_Civil).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Est_Civil).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Est_Civil).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 9 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Conocimiento_Cliente).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Conocimiento_Cliente).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Conocimiento_Cliente).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 10 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(ROUND((W_Fecha - Clientes.Fec_Ingreso) / 364,2)).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(ROUND((W_Fecha - Clientes.Fec_Ingreso) / 364,2)).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(ROUND((W_Fecha - Clientes.Fec_Ingreso) / 364,2)).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 11 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(Clientes.Garantia).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(Clientes.Garantia).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(Clientes.Garantia).

 FIND FIRST CCTmpI WHERE CCTmpI.ILinea EQ 12 NO-ERROR.
 IF W_NroCodeu EQ 1 THEN
    ASSIGN CCTmpI.Cod1 = STRING(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 364,2)).
 ELSE IF W_NroCodeu EQ 2 THEN
    ASSIGN CCTmpI.Cod2 = STRING(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 364,2)).
 ELSE
    ASSIGN CCTmpI.Cod3 = STRING(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 364,2)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VerFoto wWin 
PROCEDURE VerFoto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN RutaFoto = "imagenes\fotos\0.jpg".
P_Foto:LOAD-IMAGE(RutaFoto) IN FRAME F_Foto.
IF Solicitud.Nit NE ? THEN
DO:

   ASSIGN RutaFoto = "imagenes\fotos\" + TRIM(Solicitud.Nit) + ".jpg".
   P_Foto:LOAD-IMAGE(RutaFoto) IN FRAME F_Foto.

   /*  
   IF ERROR-STATUS:ERROR THEN
     MESSAGE "No Existe la foto del asociado" VIEW-AS ALERT-BOX.
  ELSE
  */
     VIEW FRAME F_Foto.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verificar_Relaciones_Garantias wWin 
PROCEDURE Verificar_Relaciones_Garantias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   IF Pro_Creditos.Id_EXtracto THEN DO:  /*Nov.2/05 GAER, No valida para transitorios*/
      control_grabar = TRUE.
      RETURN.
   END.

   IF Clientes.Garantia EQ "Prenda" THEN DO: 
      FIND FIRST Garantias WHERE 
                  Garantias.Agencia             EQ Solicitud.Agencia        AND                      
                  Garantias.Nit                 EQ Solicitud.Nit            AND                      
                  Garantias.Tip_Credito         EQ Solicitud.Tip_Credito    AND                      
                  Garantias.Cod_Credito         EQ Solicitud.Cod_Credito    AND 
                  Garantias.Tipo_Garantia       EQ 2                        AND
                  Garantias.Num_Solicitud       EQ Solicitud.Num_Solicitud  AND
                  Garantias.Estado              EQ 1                        AND
                  Garantias.Aprobada                   NO-LOCK NO-ERROR.                             
      IF NOT AVAIL(Garantias) THEN DO:
         MESSAGE "La Aprobaciòn exige Garantìa PRENDA, Admisible debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                     VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
   END.

   IF Clientes.Garantia EQ "Inversion" THEN DO: 
      FIND FIRST Garantias WHERE 
                  Garantias.Agencia             EQ Solicitud.Agencia        AND                      
                  Garantias.Nit                 EQ Solicitud.Nit            AND                      
                  Garantias.Tip_Credito         EQ Solicitud.Tip_Credito    AND                      
                  Garantias.Cod_Credito         EQ Solicitud.Cod_Credito    AND 
                  Garantias.Tipo_Garantia       EQ 3                        AND
                  Garantias.Num_Solicitud       EQ Solicitud.Num_Solicitud  AND
                  Garantias.Estado              EQ 1                        AND
                  Garantias.Aprobada                   NO-LOCK NO-ERROR.                             
      IF NOT AVAIL(Garantias) THEN DO:
         MESSAGE "La Aprobaciòn exige Garantìa Inversion, Admisible debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                     VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
   END.

   FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
  /* IF Cfg_RegCredito.Vr_MontoCodeud GT 0 AND Solicitud.Monto GE Cfg_RegCredito.Vr_MontoCodeud THEN DO:
      FIND FIRST Garantias WHERE 
                  Garantias.Agencia             EQ Solicitud.Agencia        AND                      
                  Garantias.Nit                 EQ Solicitud.Nit            AND                      
                  Garantias.Tip_Credito         EQ Solicitud.Tip_Credito    AND                      
                  Garantias.Cod_Credito         EQ Solicitud.Cod_Credito    AND 
                  Garantias.Num_Solicitud       EQ Solicitud.Num_Solicitud  AND
                  Garantias.Estado              EQ 1                        AND
                  Garantias.Aprobada   NO-LOCK NO-ERROR.                             
      IF NOT AVAIL(Garantias) THEN DO:
         MESSAGE "La Aprobaciòn exige Garantìa debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                     VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
   END.*/

   IF Clientes.Garantia EQ "Con Codeudor(es)" 
   OR Clientes.Garantia EQ "Admisible y Codeudor" THEN DO:
      FIND FIRST Relaciones WHERE 
               Relaciones.Nit            EQ Solicitud.Nit                   AND
               INTEG(Relaciones.Cuenta)  EQ Solicitud.Num_Solicitud         AND
               Relaciones.Clase_Producto EQ 2                               AND
               Relaciones.Cod_Producto   EQ Solicitud.Cod_Credito           AND
               Relaciones.Cod_Relacion   EQ 11                              AND
               Relaciones.Estado         EQ 1                               AND
               Relaciones.Aprobada  NO-LOCK NO-ERROR.            
      IF NOT AVAIL(Relaciones) THEN DO:
         MESSAGE "La Aprobaciòn exige Codeudor debidamente Aceptado..." SKIP
                      "                       No se permite la Operaciòn..."
              VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.                                                                                                         
   END.  

   IF Clientes.Garantia EQ "Admisible y Codeudor"                                                       
   OR Clientes.Garantia EQ "Hipoteca" THEN DO:                                                                  
       IF Clientes.Garantia EQ "Hipoteca" THEN DO:
          FIND FIRST Garantias WHERE 
                  Garantias.Agencia             EQ Solicitud.Agencia        AND                      
                  Garantias.Nit                 EQ Solicitud.Nit            AND                      
                  Garantias.Tip_Credito         EQ Solicitud.Tip_Credito    AND                      
                  Garantias.Cod_Credito         EQ Solicitud.Cod_Credito    AND                      
                  Garantias.Num_Solicitud       EQ Solicitud.Num_Solicitud  AND
                  Garantias.Tipo_Garantia       EQ 1                        AND
                  Garantias.Estado              EQ 1                        AND
                  Garantias.Aprobada                   NO-LOCK NO-ERROR.                             
          IF NOT AVAIL(Garantias) THEN DO:
             MESSAGE "La Aprobaciòn exige Garantìa HIPOTECA debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                     VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
          END.
       END.
       ELSE DO:
            FIND FIRST Garantias WHERE 
                  Garantias.Agencia             EQ Solicitud.Agencia        AND                      
                  Garantias.Nit                 EQ Solicitud.Nit            AND                      
                  Garantias.Tip_Credito         EQ Solicitud.Tip_Credito    AND                      
                  Garantias.Cod_Credito         EQ Solicitud.Cod_Credito    AND 
                  Garantias.Tipo_Garantia       LE 3                        AND
                  Garantias.Num_Solicitud       EQ Solicitud.Num_Solicitud  AND
                  Garantias.Estado              EQ 1                        AND
                  Garantias.Aprobada                   NO-LOCK NO-ERROR.                             
            IF NOT AVAIL(Garantias) THEN DO:
                MESSAGE "La Aprobaciòn exige Garantìa Admisible debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                     VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
       END.
   END.

   IF Clientes.Garantia EQ "No Admisible" THEN DO: 
      FIND FIRST Garantias WHERE 
                  Garantias.Agencia             EQ Solicitud.Agencia        AND                      
                  Garantias.Nit                 EQ Solicitud.Nit            AND                      
                  Garantias.Tip_Credito         EQ Solicitud.Tip_Credito    AND                      
                  Garantias.Cod_Credito         EQ Solicitud.Cod_Credito    AND 
                  Garantias.Tipo_Garantia       GE 4                        AND
                  Garantias.Num_Solicitud       EQ Solicitud.Num_Solicitud  AND
                  Garantias.Estado              EQ 1                        AND
                  Garantias.Aprobada                   NO-LOCK NO-ERROR.                             
      IF NOT AVAIL(Garantias) THEN DO:
         MESSAGE "La Aprobaciòn exige Garantìa No-Admisible debidamente Aprobada..." SKIP
                        "                                No se permite la Operaciòn..."
                     VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
   END.

   control_grabar = TRUE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fActualzaTposPrdctos wWin 
FUNCTION fActualzaTposPrdctos RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  fActualzaTposPrdctos
------------------------------------------------------------------------------*/

    DO WITH FRAME F_Producto:
        Cmb_Productos:LIST-ITEMS = "".
        IF NOT CAN-FIND(FIRST anexos_clientes WHERE anexos_clientes.nit = solicitud.nit:SCREEN-VALUE IN FRAME f_solicitud)
        THEN RETURN FALSE.
        FIND FIRST anexos_clientes NO-LOCK
            WHERE 
                anexos_clientes.nit = solicitud.nit:SCREEN-VALUE IN FRAME f_solicitud NO-ERROR.
        IF NOT Anexos_Clientes.Cli_AAA
        THEN DO:
            FOR EACH Pro_Creditos NO-LOCK
                WHERE 
                    Pro_Creditos.Estado  = 1
                AND not(Pro_Creditos.Tip_Credito = 1 
                AND pro_creditos.cod_credito < 700):
                W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto) IN FRAME F_Producto.
            END.
        END.
        ELSE DO:
            FOR EACH Pro_Creditos NO-LOCK
                WHERE 
                    Pro_Creditos.Tip_Credito = 1 
                AND Pro_Creditos.Estado  = 1
                AND pro_creditos.cod_credito < 700:
                W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto) IN FRAME F_Producto.
            END.
        END.
    END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAhrros wWin 
FUNCTION fAhrros RETURNS LOGICAL (cNit AS CHARACTER) :

DEFINE VAR i AS INTEGER NO-UNDO.

PUT UNFORMATTED FILL(" ",42) + "SALDOS DE AHORRO" SKIP.

FOR EACH Ahorros NO-LOCK WHERE Ahorros.Nit = cNit
                           AND Ahorros.Estado = 1, FIRST Pro_Ahorros NO-LOCK WHERE Pro_Ahorros.Cod_Ahorro = Ahorros.Cod_ahorro BREAK BY Ahorros.Cod_Ahorro:
    i = i + 1.
    
    ACCUMULATE Ahorros.Sdo_Disponible (TOTAL BY Ahorros.Cod_Ahorro).
    ACCUMULATE Ahorros.Cuota (TOTAL BY Ahorros.Cod_Ahorro).
    ACCUMULATE Ahorros.Int_Pagar (TOTAL BY Ahorros.Cod_Ahorro).

    IF LAST-OF(Ahorros.Cod_Ahorro) THEN DO:
        DISPLAY Ahorros.Cod_Ahorro COLUMN-LABEL "CodProd"
                pro_ahorros.Nom_Producto COLUMN-LABEL "Nombre Producto"
                ACCUM TOTAL BY Ahorros.Cod_Ahorro Ahorros.Sdo_Disponible FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Saldo Disponible"
                ACCUM TOTAL BY Ahorros.Cod_Ahorro Ahorros.INT_pagar FORMAT ">>>,>>>,>>9" COLUMN-LABEL "IntPorPagar"
                ACCUM TOTAL BY Ahorros.Cod_Ahorro Ahorros.cuota FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Cuota"
            WITH USE-TEXT WIDTH 132 STREAM-IO DOWN FRAME DtlleAhrros.

        DOWN WITH FRAME DtlleAhrros.
    END.

    IF LAST(Ahorros.Cod_Ahorro) THEN DO:
        DISPLAY "Total:" AT 48
                ACCUM TOTAL Ahorros.Sdo_Disponible FORMAT ">>>,>>>,>>9"
                ACCUM TOTAL Ahorros.INT_pagar FORMAT ">>>,>>>,>>9"
                ACCUM TOTAL Ahorros.cuota FORMAT ">>>,>>>,>>9"
            WITH NO-LABELS USE-TEXT WIDTH 132 STREAM-IO FRAME TtalAhrros.
    END.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBrdesRngo wWin 
FUNCTION fBrdesRngo RETURNS CHARACTER
  (crngo AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fchar2decimal wWin 
FUNCTION fchar2decimal RETURNS DECIMAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
    Purpose: convierte a decimal un número almacenado en un campo tipo char  
    Notes:  fchar2decimal
    log: 9 ene 2008, Ing. Edilberto Mariño Moya        
------------------------------------------------------------------------------*/
    DEF VAR cs AS CHAR NO-UNDO.
    cs = IF SESSION:NUMERIC-DECIMAL-POINT = "." THEN "," ELSE ".".
    RETURN DECIMAL(REPLACE(c,cs,SESSION:NUMERIC-DECIMAL-POINT)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fColExcel wWin 
FUNCTION fColExcel RETURNS CHARACTER
  (j AS INTEGER /* parameter-definitions */ ) :
    /*  
        Descripcion: Devuelve en letras el equivalente a una columna de excel
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR k AS INTEGER NO-UNDO.
    DEF VAR cRt AS CHAR NO-UNDO.
    i = TRUNCATE(j / 26,0).
    k = j MOD 26.
    crt = CHR(i + 64) + CHR(1) + CHR(k + 64).
    IF ENTRY(2,crt,CHR(1)) = "@" 
    THEN DO:
        ENTRY(1,crt,CHR(1)) = CHR(ASC(ENTRY(1,crt,CHR(1))) - 1).
        ENTRY(2,crt,CHR(1)) = "Z".
    END.
    crt = REPLACE(crt,"@","").
    crt = TRIM(crt,CHR(1)).
    crt = REPLACE(crt,CHR(1),"").
    RETURN crt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCpcdadPgo wWin 
FUNCTION fCpcdadPgo RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR deVRtrno AS DECIMAL NO-UNDO.
    DO WITH FRAME F_VblesS:
        deVRtrno = 
            fVldaSV(Tot_Ingresos:SCREEN-VALUE)
        -   fVldaSV(clientes.gto_obligacion:SCREEN-VALUE) 
        -   fVldaSV(clientes.gtofinanc_indir:SCREEN-VALUE)
        -   fVldaSV(clientes.gto_arriendo:SCREEN-VALUE)
        -   fVldaSV(clientes.gto_familiar:SCREEN-VALUE)
        -   fVldaSV(W_VrCuota:SCREEN-VALUE).
        deVRtrno = 100 - (fVldaSV(clientes.gto_obligacion:SCREEN-VALUE) 
        +   fVldaSV(clientes.gtofinanc_indir:SCREEN-VALUE)
        +   fVldaSV(clientes.gto_arriendo:SCREEN-VALUE)
        +   fVldaSV(clientes.gto_familiar:SCREEN-VALUE)
        +   fVldaSV(W_VrCuota:SCREEN-VALUE)) / fVldaSV(Tot_Ingresos:SCREEN-VALUE) * 100.
        IF solicitud.FOR_pago:SCREEN-VALUE IN FRAME f_forpago = "1" 
        THEN DO:
            IF ABSOLUTE(devrtrno) < 100  
            THEN
            CLIENTE.capacidad_pago:SCREEN-VALUE = string(devrtrno).
            DECpcdadPgo = deVRtrno.
            cliente.gto_familiar:SENSITIVE = FALSE.
            cliente.gto_familiar:BGCOLOR = 18.
            cliente.gto_familiar:FGCOLOR = 15.
        END.
        ELSE DO:
            deVRtrno = 0.
            CLIENTE.capacidad_pago:SCREEN-VALUE = string(0).
            cliente.gto_familiar:SCREEN-VALUE = "0".
            DECpcdadPgo = deVRtrno. 
            cliente.gto_familiar:SENSITIVE = TRUE.
            cliente.gto_familiar:BGCOLOR = 15.
            cliente.gto_familiar:FGCOLOR = 0.
        END.
        RUN ttlza.        
    END.
    RETURN deVRtrno.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCrdtos wWin 
FUNCTION fCrdtos RETURNS LOGICAL (cnit AS CHAR) :

DEFINE VAR i AS INTEGER NO-UNDO.
DEFINE VAR cMnsje AS CHAR NO-UNDO.

PUT UNFORMATTED fill(chr(10),4) + fill(" ",30) + "INFORMACION DETALLADA DE LOS CREDITOS" SKIP.

FOR EACH Creditos NO-LOCK WHERE Creditos.Nit = cnit AND creditos.estado = 2, FIRST Pro_Creditos NO-LOCK WHERE Pro_Creditos.Cod_Credito = Creditos.Cod_Credito BREAK BY Creditos.Cod_Credito:
    ACCUMULATE Creditos.Sdo_Capital (TOTAL BY Creditos.Cod_Credito).
    ACCUMULATE Creditos.Cuota (TOTAL BY Creditos.Cod_Credito).
    ACCUMULATE Creditos.Int_morCobrar (TOTAL BY Creditos.Cod_Credito).
    
    i = i + 1.

    IF Creditos.Estado = 5 THEN
        cmnsje = cmnsje + "Crédito " + string(creditos.num_credito) + " Castigado".

    IF Creditos.Abogado THEN
        cMnsje = cMnsje + "Crédito " + string(creditos.num_credito) + " COBRO JURIDICO - Abogado: " + Creditos.Nom_Juzgado.

    IF LAST-OF(Creditos.Cod_Credito) THEN DO:
        DISPLAY Creditos.Cod_Credito COLUMN-LABEL "CodProducto"
                pro_creditos.Nom_Producto COLUMN-LABEL "Nombre Producto"
                ACCUM TOTAL BY Creditos.Cod_Credito Creditos.Sdo_Capital FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Saldo Capital"
                ACCUM TOTAL BY Creditos.Cod_Credito Creditos.Int_morCobrar FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Int Por Cobrar"
                ACCUM TOTAL BY Creditos.Cod_Credito Creditos.Cuota FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Cuota"
            WITH USE-TEXT STREAM-IO WIDTH 132 DOWN FRAME DtlleCrdto.

        IF NOT trim(cMnsje) = "" THEN
            PUT UNFORMATTED cMnsje SKIP.
    END.

    IF LAST(Creditos.cod_credito) THEN DO:
        DISPLAY "Total:" AT 49
                ACCUM TOTAL Creditos.Sdo_Capital FORMAT ">>>,>>>,>>9"
                ACCUM TOTAL Creditos.Int_morCobrar FORMAT ">>>,>>>,>>9" AT 71
                ACCUM TOTAL Creditos.Cuota FORMAT ">>>,>>>,>>9"
            WITH USE-TEXT NO-LABEL STREAM-IO WIDTH 132 FRAME CbzaCrdto.
    END.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFchaLtras wWin 
FUNCTION fFchaLtras RETURNS CHARACTER
  (dafcha AS DATE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR mes AS CHAR NO-UNDO EXTENT 12.
  mes[1] = "ene".
  mes[2] = "feb".
  mes[3] = "mar".
  mes[4] = "abr".
  mes[5] = "may".
  mes[6] = "jun".
  mes[7] = "jul".
  mes[8] = "ago".
  mes[9] = "sep".
  mes[10] = "oct".
  mes[11] = "nov".
  mes[12] = "dic".
  RETURN string(DAY(dafcha)) + "-" + mes[MONTH(dafcha)] + "-" + string(YEAR(dafcha)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fpath wWin 
FUNCTION fpath RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cx AS CHAR NO-UNDO.
    DO i = 1 TO NUM-ENTRIES(c,"|"):
        cx = cx + SUBSTRING(ENTRY(i,c,"|"),1,2).
    END.
    RETURN cx.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRngoSlrio wWin 
FUNCTION fRngoSlrio RETURNS DECIMAL
  (deSlrio AS DECIMAL,iNmroPrsnas AS INTEGER,cCiudad AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    /*
        Descripcion: Busca el salario en los rangos
        log: Creada 9 feb 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR cPrsnasACrgo AS CHAR NO-UNDO.
    DEF VAR deVRtrno AS DECIMAL NO-UNDO.
    DEF VAR deNto AS DECIMAL NO-UNDO.
    cPrsnasACrgo = IF iNmroPrsnas = 0 THEN "SIN PERSONAS A CARGO" ELSE "CON PERSONAS A CARGO".
    FOR FIRST pro_scoring NO-LOCK
        WHERE
            pro_scoring.codigo = 99
        AND DECIMAL(pro_scoring.rango_final) > deslrio
        AND CAN-DO(pro_scoring.rango_inicial,cCiudad)
        AND pro_scoring.titulo = cPrsnasACrgo
        BY (IF NOT pro_scoring.rango_inicial = "*" THEN 1 ELSE 99):
        deVRtrno = pro_scoring.puntaje.
    END.
    DO WITH FRAME F_VblesS:
        deNto = deSlrio - decimal(clientes.gto_obligacion:SCREEN-VALUE).
        deNto = deNto * deVRtrno / 100.
    END.
    deVRtrno = deNto.
    RETURN deVRtrno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fToExcelVlor wWin 
FUNCTION fToExcelVlor RETURNS LOGICAL
  (pCol AS char,pval AS char /* parameter-definitions */ ) :
    /*  
        Descripcion: Envia Valor A Excel
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN TRUE.    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVldaSV wWin 
FUNCTION fVldaSV RETURNS DECIMAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR DEvrTRNO AS DECIMAL NO-UNDO.  
    DEvrTRNO = IF c = ? THEN 0.0 ELSE DECIMAL(C) NO-ERROR.
    RETURN DEvrTRNO.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

