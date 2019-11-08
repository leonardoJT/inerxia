&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{Incluido\VARIABLE.I "SHARED"}
{Incluido\IDatacredito.I}
{Incluido\ISes.i}

DEFINE VAR z AS INTEGER.
DEF VAR i AS INTEGER.
DEF VAR C AS CHARACTER FORMAT "X(45)" INITIAL "                               ".
DEF VAR J AS INTEGER.
DEF VAR K AS INTEGER.
DEF VAR Acum AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEF VAR XAcum AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEF VAR FecAux AS DATE.
DEF VAR CAMPO AS CHARACTER FORMAT "X(15)".
DEF VAR CAMPO0 AS CHARACTER FORMAT "X(15)".
DEF VAR NumReg AS DECIMAL FORMAT "99999999".
DEF VAR NitAux LIKE Clientes.Nit.
define variable fecha1 as DATE.
define variable Xsumavalor as decimal initial 0 FORMAT "999,999,999,999".
define variable xsumatasa as decimal initial 0.
define variable xtitulos as integer initial 0.
DEFINE VAR CRegCon AS CHARACTER FORMAT "X(33)".

DEFINE TEMP-TABLE TmpDat
    FIELD INumCta AS CHARACTER FORMAT "X(18)"
    FIELD INumIde AS CHARACTER FORMAT "X(11)"
    FIELD INomCli AS CHARACTER FORMAT "X(45)"
    FIELD IFecNac AS CHARACTER FORMAT "X(6)"
    FIELD IFecApe AS DECIMAL FORMAT "999999"
    FIELD IFecVen AS DECIMAL FORMAT "999999"
    FIELD ICuoMes AS DECIMAL FORMAT "9999999999"
    FIELD INoveda AS DECIMAL INITIAL 01 FORMAT "99"
    FIELD IAdjeti AS DECIMAL FORMAT "99"
    FIELD ITipIde AS DECIMAL FORMAT "9"
    FIELD IValIni AS DECIMAL FORMAT "9999999999"
    FIELD ISdoCre AS DECIMAL FORMAT "9999999999"
    FIELD ISdoMor AS DECIMAL FORMAT "9999999999"
    FIELD IValDis AS DECIMAL FORMAT "9999999999"
    FIELD ITipMon AS DECIMAL FORMAT "9"
    FIELD ITipCre AS DECIMAL FORMAT "9"
    FIELD ITipGar AS DECIMAL FORMAT "9"
    FIELD ICalifi AS CHARACTER FORMAT "X"
    FIELD ICiuRes AS CHARACTER FORMAT "X(15)"
    FIELD IDirRes AS CHARACTER FORMAT "X(30)"
    FIELD ITelRes AS CHARACTER FORMAT "X(10)"
    FIELD ICiuLab AS CHARACTER FORMAT "X(15)"
    FIELD ITelLab AS character FORMAT "X(10)"
    FIELD ICiuCor AS CHARACTER FORMAT "X(15)"
    FIELD IDirCor AS CHARACTER FORMAT "X(30)"
    FIELD ICiiu AS DECIMAL FORMAT "999999"
    FIELD ITotCuo AS DECIMAL FORMAT "999"
    FIELD ICuoCan AS DECIMAL FORMAT "999"
    FIELD ICuoMor AS DECIMAL FORMAT "999"
    FIELD IFecPag AS DECIMAL FORMAT "99999999"
    FIELD IOfiRad AS CHARACTER FORMAT "X(15)"
    FIELD ICiuRad AS CHARACTER FORMAT "X(15)"
    FIELD IForPag AS INTEGER FORMAT "9"
    FIELD IPerPag AS INTEGER FORMAT "9" INITIAL 1
    FIELD IEdaMor AS INTEGER FORMAT "999"
    FIELD IFecAct AS DECIMAL FORMAT "99999999"
    FIELD IReclam AS DECIMAL FORMAT "9"

    /* oakley */

    FIELD IRespon AS DECIMAL FORMAT "99"
    FIELD IEstrat AS INTEGER FORMAT "9" INITIAL 3
    FIELD IFil AS CHARACTER FORMAT "X(14)" INITIAL "00000000000000"
    FIELD Registro AS DECIMAL FORMAT "9999999".

DEFINE TEMP-TABLE TmpDatCli
    FIELD ITipIde AS DECIMAL FORMAT "9"
    FIELD INumIde AS CHARACTER FORMAT "X(11)"
    FIELD INomCli AS CHARACTER FORMAT "X(45)"
    FIELD IFecExp AS CHARACTER FORMAT "X(8)"
    FIELD iLugarExp AS CHARACTER FORMAT "X(8)"
    FIELD IFecNac AS CHARACTER FORMAT "X(8)"
    FIELD IActividad AS INTEGER FORMAT "99"
    FIELD ICiiu AS INTEGER FORMAT "9999"
    FIELD IEstrat AS INTEGER FORMAT "99"
    FIELD IPer_Acargo AS INTEGER FORMAT "999999"
    FIELD IFecActAcargo AS CHARACTER FORMAT "x(8)"
    FIELD IEst_Civil AS INTEGER FORMAT "99"
    FIELD IFecActEstCivil AS CHARACTER FORMAT "x(8)"
    FIELD INivelEstudio AS INTEGER FORMAT "99"
    FIELD IFecActNivelEst AS CHARACTER FORMAT "x(8)"
    FIELD IIngresos AS DECIMAL FORMAT "9999999999999"
    FIELD IFecActIngresos AS CHARACTER FORMAT "x(8)"
    FIELD IActivos AS DECIMAL FORMAT "9999999999999"
    FIELD IPasivos AS DECIMAL FORMAT "9999999999999"
    FIELD IFecBalance AS CHARACTER FORMAT "x(8)"
    FIELD INitEmpleador AS DECIMAL FORMAT "99999999999"
    FIELD INomEmpleador AS CHARACTER FORMAT "x(45)"
    FIELD IFecIngEmpresa AS CHARACTER FORMAT "x(8)"
    FIELD IFecActEmpleador AS CHARACTER FORMAT "x(8)"
    FIELD ITipContrato AS INTEGER FORMAT "9"
    FIELD IFecActTipCont AS CHARACTER FORMAT "x(8)" 
    FIELD IOperacionesInt AS INTEGER FORMAT "9"
    FIELD IFecActOperaInt AS CHARACTER FORMAT "x(8)" 
    FIELD ISituacionTit AS INTEGER FORMAT "9"
    FIELD IFuerzaMayor AS INTEGER FORMAT "9"
    FIELD IFecActFuerzaMay AS CHARACTER FORMAT "x(8)" 
    FIELD IFiller AS CHARACTER FORMAT "x(72)".

DEFINE TEMP-TABLE TmpCod LIKE TmpDat.

/*REGISTRO DE FIN*/
DEFINE VAR CRegFIN AS CHARACTER FORMAT "X(42)".

/* ahorros fogacoop */
DEFINE TEMP-TABLE TmpAFg
    FIELD FA_TipIde AS CHARACTER FORMAT "X(1)"
    FIELD FA_CedNit AS CHARACTER FORMAT "X(12)"
    FIELD FA_Apell1 AS CHARACTER FORMAT "X(20)"
    FIELD FA_Apell2 AS CHARACTER FORMAT "X(20)"
    FIELD FA_NomNit AS CHARACTER FORMAT "X(40)"
    FIELD FA_TipAho AS CHARACTER FORMAT "X(5)"
    FIELD FA_ValIni AS DECIMAL FORMAT "99999999999999999"
    FIELD FA_FecApe AS CHARACTER FORMAT "X(8)"
    FIELD FA_Plazo AS DECIMAL FORMAT "9999"
    FIELD FA_FecVto AS CHARACTER FORMAT "X(8)"
    FIELD FA_Tasa AS DECIMAL FORMAT "999999.99"
    FIELD FA_Amorti AS DECIMAL FORMAT "9"
    FIELD FA_Modali AS DECIMAL FORMAT "9"
    FIELD FA_Saldo AS DECIMAL FORMAT "99999999999999999".

/* Creditos fogacoop */
DEFINE TEMP-TABLE TmpCFg
    FIELD FC_TipIde AS CHARACTER FORMAT "X(1)"
    FIELD FC_CedNit AS CHARACTER FORMAT "X(12)"
    FIELD FC_Apell1 AS CHARACTER FORMAT "X(20)"
    FIELD FC_Apell2 AS CHARACTER FORMAT "X(20)"
    FIELD FC_NomNit AS CHARACTER FORMAT "X(40)"
    FIELD FC_TipObl AS INTEGER FORMAT "9"
    FIELD FC_Califi AS CHARACTER FORMAT "X"
    FIELD FC_FecApe AS CHARACTER FORMAT "X(8)"
    FIELD FC_FecVen AS CHARACTER FORMAT "X(8)"
    FIELD FC_EdaMor AS INTEGER FORMAT "9999"
    FIELD FC_Tasa AS DECIMAL FORMAT "999999.99"
    FIELD FC_PerPag AS DECIMAL FORMAT "9"
    FIELD FC_Modali AS DECIMAL FORMAT "9"
    FIELD FC_SalDeu AS DECIMAL FORMAT "99999999999999999"
    FIELD FC_SalIni AS DECIMAL FORMAT "99999999999999999"
    FIELD FC_SalOtr AS DECIMAL FORMAT "99999999999999999"
    FIELD FC_TipGar AS DECIMAL FORMAT "9"
    FIELD FC_Provis AS DECIMAL FORMAT "99999999999999999"
    FIELD FC_CuoExt AS DECIMAL FORMAT "9999999999"
    FIELD FC_MesExt AS DECIMAL FORMAT "9999999999".

/* Ahorros SUPERSOLIDARIA */
DEFINE TEMP-TABLE SSAho
    FIELD SA_TipIde AS CHARACTER FORMAT "X" INITIAL "C"
    FIELD SA_CedNit AS CHARACTER FORMAT "X(12)"
    FIELD SA_NomNit AS CHARACTER FORMAT "X(40)"
    FIELD SA_CodCon AS DECIMAL FORMAT "999999999999"
    FIELD SA_NomDep AS CHARACTER FORMAT "X(30)"
    FIELD SA_TipAho AS CHARACTER FORMAT "X(2)"
    FIELD SA_Amorti AS INTEGER FORMAT "9"
    FIELD SA_FecApe AS CHARACTER FORMAT "99999999"
    FIELD SA_Plazo  AS INTEGER FORMAT "9999"
    FIELD SA_FecVen AS CHARACTER FORMAT "99999999"
    FIELD SA_Modali AS INTEGER FORMAT "9"
    FIELD SA_TasNom AS DECIMAL FORMAT "999.999999"
    FIELD SA_TasEfe AS DECIMAL FORMAT "999.999999"
    FIELD SA_IntPag AS DECIMAL FORMAT "999999999"
    FIELD SA_Saldo AS DECIMAL FORMAT "99999999999999999"
    FIELD SA_ValIni AS DECIMAL FORMAT "99999999999999999"
    FIELD SA_NumCta AS DECIMAL FORMAT "99999999999999"
    FIELD SA_Exgmf AS CHARACTER FORMAT "X"
    FIELD SA_FecExgmf AS CHARACTER FORMAT "9999999999"
    FIELD SS_AGE AS CHARACTER FORMAT "XXX".

/* Aportes SUPERSOLIDARIA */
DEFINE TEMP-TABLE SSApo
    FIELD SS_TipIde AS CHARACTER FORMAT "X"
    FIELD SS_NumNit AS CHARACTER FORMAT "X(12)"
    FIELD SS_Saldo AS DECIMAL FORMAT "99999999999999"
    FIELD SS_AGE AS CHARACTER FORMAT "XXX"
    FIELD SS_Cuota AS DECIMAL FORMAT "99999999999999"
    FIELD saldoPromedio AS INTEGER
    FIELD fecUltTrans AS DATE
    INDEX ss_numnit IS PRIMARY ss_numnit.

/*Clientes SUPERSOLIDARIA - Sin utilizar*/
DEFINE TEMP-TABLE SScli
    FIELD Sc_TipIde AS CHARACTER FORMAT "X"
    FIELD Sc_NumNit AS CHARACTER FORMAT "X(12)"
    FIELD Sc_apell1 AS CHARACTER FORMAT "X(20)"
    FIELD Sc_Apell2 AS CHARACTER FORMAT "X(20)"
    FIELD Sc_NomNit AS CHARACTER FORMAT "X(30)"
    FIELD Sc_FecApe AS CHARACTER FORMAT "99999999"
    FIELD Sc_tel AS CHARACTER FORMAT "X(15)"
    FIELD Sc_dir AS character FORMAT "X(25)"
    FIELD Sc_soc AS character FORMAT "9"
    FIELD Sc_act AS character FORMAT "9"
    FIELD Sc_ciiu AS character FORMAT "9999"
    FIELD Sc_mun AS character FORMAT "999"
    FIELD Sc_email AS character FORMAT "X(15)".

/* Creditos SUPERSOLIDARIA */
DEFINE TEMP-TABLE SSCre
    FIELD SC_TipIde AS CHARACTER FORMAT "X" INITIAL "C"
    FIELD SC_CodCon AS character FORMAT "X(14)"
    FIELD SC_CedNit AS CHARACTER FORMAT "X(12)"
    FIELD SC_NomNit AS CHARACTER FORMAT "X(40)"
    FIELD SC_Clasif AS DECIMAL FORMAT "9"
    FIELD SC_Catego AS CHARACTER FORMAT "X"
    FIELD SC_FecApe AS CHARACTER FORMAT "X(8)"
    FIELD SC_EdaMor AS DECIMAL FORMAT "9999"
    FIELD SC_TipCuo AS DECIMAL FORMAT "9"
    FIELD SC_ValCuo AS DECIMAL FORMAT "9999999.99"
    FIELD SC_CuoPag AS DECIMAL FORMAT "999"
    FIELD SC_Pagare AS DECIMAL FORMAT "9999999999"
    FIELD SC_FecVen AS CHARACTER FORMAT "X(8)"
    FIELD SC_TasNom AS DECIMAL FORMAT ">>>>>.99"
    FIELD SC_TasEfe AS DECIMAL FORMAT ">>>>>.99"
    FIELD SC_Amorti AS DECIMAL FORMAT "99"
    FIELD SC_Modali AS DECIMAL FORMAT "9"
    FIELD SC_SdoCap AS DECIMAL FORMAT "9999999999.99"
    FIELD SC_PLAZO AS DECIMAL FORMAT "9999"
    FIELD SC_Monto AS DECIMAL FORMAT "9999999999.99"
    FIELD SC_SdoInt AS DECIMAL FORMAT "9999999999.99"
    FIELD SC_SdoOtr AS DECIMAL FORMAT "9999999999.99"
    FIELD SC_ClaGar AS CHARACTER FORMAT "X(12)"
    FIELD SC_VlrPro AS DECIMAL FORMAT "9999999999"
    FIELD sc_vlrproint AS DECIMAL FORMAT "9999999999"
    FIELD SC_ConInt AS DECIMAL FORMAT "9999999999"
    FIELD SC_VlrExt AS DECIMAL FORMAT "9999999999"
    FIELD SC_MesExt AS DECIMAL FORMAT "999"
    FIELD SC_FecUltpago AS CHAR FORMAT "X(8)"    
    FIELD SC_ClGar  AS CHARACTER FORMAT "X"  
    FIELD SC_DesCre AS CHARACTER FORMAT "9999"   
    FIELD SC_Agen AS CHARACTER FORMAT "X"
    FIELD sc_amork AS DECIMAL FORMAT "9"
    FIELD sc_capmora AS DECIMAL FORMAT "9999999999"
    FIELD sc_garan AS DECIMAL FORMAT "9999999999.99"
    FIELD SC_Reest AS INTEGER FORMAT "9" INITIAL "4"
    FIELD SC_FecReest AS CHAR FORMAT "x(10)"
    FIELD SC_CatReest AS CHAR FORMAT "X(1)"
    FIELD SC_EntRedes AS CHAR FORMAT "X(1)"
    FIELD SC_MarRedes AS CHAR FORMAT "x(1)"
    FIELD SC_ClaseViv AS CHAR FORMAT "x(1)"
    FIELD SC_SenalVIS AS CHAR FORMAT "x(1)"
    FIELD SC_TipoViv AS CHAR FORMAT "x(1)"
    FIELD SC_SenalSub AS CHAR FORMAT "x(1)"
    FIELD aportes AS DECIMAL FORMAT "-999999999.99"
    FIELD codCredito AS INTEGER
    FIELD numActualizaciones AS INTEGER
    FIELD estadoCredito AS INTEGER
    FIELD nitPatronal AS CHARACTER
    FIELD nombrePatronal AS CHARACTER.

DEFINE TEMP-TABLE brecha
   FIELD producto as integer
   FIELD banda    as integer
   FIELD valor    as decimal FORMAT "999,999,999,999" 
   FIELD tasa     as decimal
   FIELD Nro      as integer
   FIELD Captura  AS CHARACTER FORMAT "X(3)"
   FIELD Nombre   as CHARACTER FORMAT "X(30)"
   FIELD Renglon  as CHARACTER FORMAT "X(1)"
   FIELD Cuenta   LIKE Cuentas.cuenta
   INDEX id_producto producto ASCENDING.

DEFINE TEMP-TABLE Procredito
   FIELD Tip_Doc  AS INTEGER   FORMAT "9"
   FIELD Num_Doc  AS CHARACTER FORMAT "X(11)"
   FIELD Tip_Obl  AS INTEGER   FORMAT "9"
   FIELD Num_Obl  AS INTEGER   FORMAT "9999999"
   FIELD Ide_Per  AS INTEGER   FORMAT "9"
   FIELD Tpd_Cde  AS INTEGER   FORMAT "9"
   FIELD Doc_Cde  AS CHARACTER FORMAT "X(11)"
   FIELD DIR_Cde  AS CHARACTER FORMAT "X(35)"
   FIELD Tel_Cde  AS CHARACTER FORMAT "X(7)"
   FIELD Nom_Cde  AS CHARACTER FORMAT "X(25)"
   FIELD Val_Ini  AS DECIMAL   FORMAT "99999999"
   FIELD Fec_Ini  AS INTEGER   FORMAT "99999999"
   FIELD Cuo_Mor  AS INTEGER   FORMAT "99"
   FIELD Val_Mor  AS DECIMAL   FORMAT "99999999"
   FIELD Fec_Ult  AS INTEGER   FORMAT "99999999"
   FIELD Agencia  AS INTEGER   FORMAT "999".

/*UIAF - Transacciones en efectivo del sector financiero*/
DEFINE TEMP-TABLE TUIAF
   FIELD Fecha  AS DATE FORMAT "99/99/9999"
   FIELD Valor  AS DECIMAL FORMAT "99999999999999"
   FIELD Moneda AS INTEGER FORMAT "9" INITIAL 1
   FIELD CodAge AS CHARACTER FORMAT "X(15)"
   FIELD TipPdt AS INTEGER FORMAT "9"
   FIELD TipTra AS INTEGER FORMAT "9"
   FIELD NumCta AS CHARACTER FORMAT "X(20)"
   FIELD TipNit AS INTEGER FORMAT "9"
   FIELD NumNit AS CHARACTER FORMAT "X(15)"
   FIELD CodMpi AS DECIMAL FORMAT "99999"
   FIELD Consec AS DECIMAL FORMAT "99999999".

/*tabla para el manejo de cifin*/
DEF VAR W-Vlrgtia AS CHAR INIT "000000000000"  NO-UNDO.
DEF VAR W-CodDep  AS CHAR INIT "005"    NO-UNDO.
DEF VAR W-CodCiu  AS CHAR INIT "000001" NO-UNDO.
DEF VAR W-NomCiu  AS CHAR INIT "" NO-UNDO. 
DEF VAR W-NomDep  AS CHAR INIT "" NO-UNDO.

DEF TEMP-TABLE Tcifin
/* 1 */ FIELD TipReg    AS CHAR FORM "X(1)"     INIT "2" /* Detalle */
/* 2 */ FIELD TipIde    AS CHAR FORM "X(2)"
/* 3 */ FIELD NroIde    AS CHAR FORM "X(15)"
/* 4a*/ FIELD Ape1      AS CHAR FORM "X(15)"
/* 4b*/ FIELD Ape2      AS CHAR FORM "X(15)" 
/* 4c*/ FIELD Nom1      AS CHAR FORM "X(15)" 
/* 4d*/ FIELD Nom2      AS CHAR FORM "X(15)" 
/* 5 */ FIELD Reserv1   AS CHAR FORM "X(10)" INIT "          "
/* 6 */ FIELD NroObli   AS CHAR FORM "X(20)"
/* 7 */ FIELD CodSuc    AS CHAR FORM "X(6)"
/* 8 */ FIELD calidad   AS CHAR FORM "X(1)"
/* 9 */ FIELD Califica  AS CHAR FORM "X(2)"
/*10 */ FIELD EstadoTit AS CHAR FORM "X(2)"
/*11 */ FIELD Estado    AS CHAR FORM "X(2)" INIT "01" 
/*12 */ FIELD EdadMora  AS CHAR FORM "X(2)"
/*13 */ FIELD AnosMora  AS CHAR FORM "X(2)"
/*14 */ FIELD FecCorte  AS CHAR FORM "X(8)"
/*15 */ FIELD FecIExp   AS CHAR FORM "X(8)"
/*16 */ FIELD FecTerm   AS CHAR FORM "X(8)"
/*17 */ FIELD FecExig   AS CHAR FORM "X(8)" INIT "        "
/*18 */ FIELD FecPresc  AS CHAR FORM "X(8)" INIT "        "
/*19 */ FIELD FecPago   AS CHAR FORM "X(8)" INIT "        "
/*20 */ FIELD ModoExtin AS CHAR FORM "X(2)"
/*21 */ FIELD TipoPago  AS CHAR FORM "X(2)"
/*22 */ FIELD PerPago   AS CHAR FORM "X(2)"
/*23 */ FIELD ProbNoPag AS CHAR FORM "X(3)"
/*24 */ FIELD NroCuoPag AS CHAR FORM "X(3)"
/*25 */ FIELD NroCuoPac AS CHAR FORM "X(3)"
/*26 */ FIELD CuotaMora AS CHAR FORM "X(3)"
/*27 */ FIELD VlrCupo   AS CHAR FORM "X(12)"
/*28 */ FIELD VlrMora   AS CHAR FORM "X(12)"
/*29 */ FIELD VlrSaldo  AS CHAR FORM "X(12)"
/*30 */ FIELD VlrCuota  AS CHAR FORM "X(12)"
/*31 */ FIELD VlrCargoF AS CHAR FORM "X(12)" INIT "            " 
/*32 */ FIELD LineaCre  AS CHAR FORM "X(3)"  
/*33 */ FIELD ClauPerm  AS CHAR FORM "X(3)"  INIT "   "
/*34 */ FIELD TipoContr AS CHAR FORM "X(3)"
/*35 */ FIELD EstContr  AS CHAR FORM "X(3)"
/*36 */ FIELD TerContr  AS CHAR FORM "X(2)"  INIT "  "   
/*37 */ FIELD MesesCont AS CHAR FORM "X(3)"  INIT "   "
/*38 */ FIELD NatJurid  AS CHAR FORM "X(3)"
/*39 */ FIELD ModCredit AS CHAR FORM "X(2)"
/*40 */ FIELD TipoMoned AS CHAR FORM "X(2)"
/*41 */ FIELD TipoGaran AS CHAR FORM "X(2)"
/*42 */ FIELD VlrGaran  AS CHAR FORM "X(12)"
/*43 */ FIELD ObligRee  AS CHAR FORM "X(2)"
/*44 */ FIELD NatReestr AS CHAR FORM "X(2)"
/*45 */ FIELD NroReest  AS CHAR FORM "X(3)"
/*46 */ FIELD ClaseTar  AS CHAR FORM "X(2)"  INIT "  "
/*47 */ FIELD NroCheDev AS CHAR FORM "X(4)"  INIT "    "
/*48 */ FIELD CatServic AS CHAR FORM "X(2)"  INIT "  "
/*49 */ FIELD Plazo     AS CHAR FORM "X(2)"  INIT "  "
/*50 */ FIELD DiasCarte AS CHAR FORM "X(6)"  INIT "      "
/*51 */ FIELD TipoCuent AS CHAR FORM "X(2)"  INIT "  "
/*52 */ FIELD CupoSobre AS CHAR FORM "X(12)" INIT "            "
/*53 */ FIELD DiasAutor AS CHAR FORM "X(3)"  INIT "   "
/*54 */ FIELD DirCasaTi AS CHAR FORM "X(60)"
/*55 */ FIELD TelTitul  AS CHAR FORM "X(20)"
/*56 */ FIELD CodCiuCas AS CHAR FORM "X(6)"
/*57 */ FIELD CiudadCas AS CHAR FORM "X(20)"
/*58 */ FIELD CodDeptoC AS CHAR FORM "X(3)"
/*59 */ FIELD DepCasaTi AS CHAR FORM "X(20)"
/*60 */ FIELD NomEmpre  AS CHAR FORM "X(60)"
/*61 */ FIELD DirEmpre  AS CHAR FORM "X(60)"
/*62 */ FIELD TelEmpre  AS CHAR FORM "X(20)"
/*63 */ FIELD CodCiuEmp AS CHAR FORM "X(6)"
/*64 */ FIELD CiudadEmp AS CHAR FORM "X(20)"
/*65 */ FIELD CodDepEmp AS CHAR FORM "X(3)"
/*66 */ FIELD DepEmpTit AS CHAR FORM "X(20)"
/*67 */ FIELD FecIniExe AS CHAR FORM "X(8)"  INIT "        " 
/*68 */ FIELD FecTerExe AS CHAR FORM "X(8)"  INIT "        " 
/*69 */ FIELD NroRenCDT AS CHAR FORM "X(2)"  INIT "  "
/*70 */ FIELD CtaExGMF  AS CHAR FORM "X(2)"  INIT "  "
/*71 */ FIELD TipoIdeOr AS CHAR FORM "X(2)"
/*72 */ FIELD NroIdeOr  AS CHAR FORM "X(14)" INIT "              "
/*73 */ FIELD TipoEntOr AS CHAR FORM "X(3)"
/*74 */ FIELD CodEntOr  AS CHAR FORM "X(3)"
/*75 */ FIELD TipoFidec AS CHAR FORM "X(2)"  INIT "  "
/*76 */ FIELD NroFidec  AS CHAR FORM "X(12)" INIT "            "
/*77 */ FIELD NomFidec  AS CHAR FORM "X(60)" INIT "                                                               "
/*78 */ FIELD TipoDeuda AS CHAR FORM "X(4)"  INIT "    "
/*79 */ FIELD TipoPoliz AS CHAR FORM "X(4)"  INIT "    "
/*80 */ FIELD CodRamo   AS CHAR FORM "X(6)"  INIT "      "
    INDEX IdxTip NroObli.
DEF TEMP-TABLE TCcifin LIKE Tcifin.

DEFINE TEMP-TABLE tmDatacredito
    FIELD Tregistro2  AS CHARACTER FORMAT "X(360)".
DEFINE VAR zswcal    AS LOGICAL INITIAL TRUE.
DEFINE VAR zestado   AS INTEGER INITIAL 0.

/*  FIELD Tregistro2A AS CHARACTER FORMAT "X(180)"
    FIELD Tregistro2B AS CHARACTER FORMAT "X(180)".*/

/* Nuevo clientes supersolidaria */
DEFINE TEMP-TABLE tClientes
    FIELD tId AS CHAR
    FIELD tnit AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD tnombre AS CHARACTER
    FIELD tFecIng       AS CHAR FORMAT "x(10)"
    FIELD tTelefono     AS CHAR
    FIELD tDireccion    AS CHAR
    FIELD tAsociado     AS CHAR
    FIELD tActivo       AS CHAR
    FIELD tActividad    AS CHAR
    FIELD tCodMun       AS CHAR
    FIELD tGenero       AS CHAR
    FIELD tEmpleado     AS CHAR
    FIELD tContrato     AS CHAR
    FIELD tNivelEsc     AS CHAR
    FIELD tEstrato      AS CHAR
    FIELD tIngresos     AS CHAR
    FIELD tFecNac       AS CHAR
    FIELD tEstCivil     AS CHAR
    FIELD tMujer        AS CHAR
    FIELD tOcupacion    AS CHAR
    FIELD tSector       AS CHAR
    FIELD tJornada      AS CHAR
    FIELD tTotIng       AS DECIMAL
    FIELD tNSalMin      AS INT
    FIELD tAsistioAsamblea AS INTEGER.

DEF VAR W-DiasPer    AS INTEGER INITIAL 0 NO-UNDO.

DEF VAR W-VlrCuoMes AS DECIMAL INITIAL 0.    
DEF VAR w-PlazoMes  AS INTEGER INITIAL 0.
DEF VAR W-CodDANE   AS CHAR FORMAT "X(8)".
DEF VAR W_FecTra    AS DATE    INITIAL ? NO-UNDO.

DEF TEMP-TABLE Tincidencias
    FIELD Tnit          LIKE Creditos.Nit
    FIELD Tpagare       LIKE Creditos.Pagare
    FIELD Tcomentario   AS CHAR FORMAT "X(120)"
    INDEX idxtnit Tnit Tpagare.

DEFINE TEMP-TABLE tMaxDias
    FIELD wnit      LIKE creditos.nit
    FIELD wTip      LIKE creditos.tip_credito
    FIELD wDiaMax   AS INTEGER.

DEFINE TEMP-TABLE tmpClientes
    FIELD tipoId AS CHARACTER
    FIELD numId AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-25 RECT-26 RECT-28 RECT-29 RECT-290 ~
RECT-291 t-2 T_UIAF t1 T-10 BUTTON-3 BUTTON-2 t-5 t4 T-6 T5 T-7 T6 t-8 t7 ~
Btn_Ejecutar t-13 t-9 t8 t-11 t-18 t-12 tgSaldosDiariosDepositos t-19 t2 t3 ~
Fec_Corte SalMin UVR BUTTON-11 BtnDone BUTTON-5 
&Scoped-Define DISPLAYED-OBJECTS t-2 T_UIAF t1 T-10 t-5 t4 T-6 T5 T-7 T6 ~
t-8 t7 t-13 t-9 t8 t-11 t-18 t-12 tgSaldosDiariosDepositos t-19 t2 t3 ~
Fec_Corte SalMin UVR m2 m1 Prc CED 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Empresa wWin 
FUNCTION Empresa RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FecTermina2 wWin 
FUNCTION FecTermina2 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FNomAgencia wWin 
FUNCTION FNomAgencia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HallaRangoVivienda wWin 
FUNCTION HallaRangoVivienda RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD retFormato wWin 
FUNCTION retFormato RETURNS CHARACTER
    /*
    GioCam Oct 08/07
    Retorna valor entero en tipo caracter, con formato de 10 caracteres
    distinguiendo si es entero negativo o entero positivo...
    */

    ( INPUT pivalor AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tabla19 wWin 
FUNCTION Tabla19 RETURNS CHARACTER
  ( INPUT TipoClte AS INTEGER, INPUT Nombre AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tabla2 wWin 
FUNCTION Tabla2 RETURNS CHARACTER
  ( INPUT TipCre AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tabla3 wWin 
FUNCTION Tabla3 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tabla31 wWin 
FUNCTION Tabla31 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tabla33 wWin 
FUNCTION Tabla33 RETURNS CHARACTER
  ( INPUT CodLugar AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tabla4 wWin 
FUNCTION Tabla4 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ejecutar 
     LABEL "&Ejecutar" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-11 
     LABEL "Ver UVR" 
     SIZE 12.14 BY .81.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 3" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 5" 
     SIZE 4 BY 1.15.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 6" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE CED AS CHARACTER FORMAT "X(15)":U 
     LABEL "Documento Procesado" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Corte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Corte" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE m1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre Archivo Fisico" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE m2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Archivo en Proceso" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Prc AS DECIMAL FORMAT "999999":U INITIAL 0 
     LABEL "Registro Procesado" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE SalMin AS CHARACTER FORMAT "X(256)":U INITIAL "616000" 
     LABEL "Salario Minimo" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UVR AS DECIMAL FORMAT ">>>9.9999":U INITIAL 0 
     LABEL "UVR" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 1.62.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 7.54.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 7.54.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 6.92.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 1.85.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 7.

DEFINE VARIABLE T-10 AS LOGICAL INITIAL no 
     LABEL "PROCREDITO" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .77 NO-UNDO.

DEFINE VARIABLE t-11 AS LOGICAL INITIAL no 
     LABEL "Renta Fija" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE t-12 AS LOGICAL INITIAL no 
     LABEL "Venta Bienes Y Servicios" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE t-13 AS LOGICAL INITIAL no 
     LABEL "Riesgo de liquidez" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE t-18 AS LOGICAL INITIAL no 
     LABEL "Evaluación Cartera  Consolidado" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .54 NO-UNDO.

DEFINE VARIABLE t-19 AS LOGICAL INITIAL no 
     LABEL "Operaciones Por Producto (Mensual)" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .77 NO-UNDO.

DEFINE VARIABLE t-2 AS LOGICAL INITIAL no 
     LABEL "CIFIN" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1.12 NO-UNDO.

DEFINE VARIABLE t-5 AS LOGICAL INITIAL no 
     LABEL "PUC. Solidaria" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "Información Estadistica" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .77 NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "Relación de Inversiones" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .77 NO-UNDO.

DEFINE VARIABLE t-8 AS LOGICAL INITIAL no 
     LABEL "Fondo de Liquidez" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .77 NO-UNDO.

DEFINE VARIABLE t-9 AS LOGICAL INITIAL no 
     LABEL "Renta Variable" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE t1 AS LOGICAL INITIAL no 
     LABEL "DATACREDITO" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.12 NO-UNDO.

DEFINE VARIABLE t2 AS LOGICAL INITIAL no 
     LABEL "Ahorro" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .54 NO-UNDO.

DEFINE VARIABLE t3 AS LOGICAL INITIAL no 
     LABEL "Crédito" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .54 NO-UNDO.

DEFINE VARIABLE t4 AS LOGICAL INITIAL no 
     LABEL "Individual Captaciones" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE T5 AS LOGICAL INITIAL no 
     LABEL "Individual Aportes" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE T6 AS LOGICAL INITIAL no 
     LABEL "Individual Cartera" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE t7 AS LOGICAL INITIAL no 
     LABEL "Individual de Clientes" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE t8 AS LOGICAL INITIAL no 
     LABEL "Brecha de liquidez" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .54 NO-UNDO.

DEFINE VARIABLE tgSaldosDiariosDepositos AS LOGICAL INITIAL no 
     LABEL "Saldos diarios Depósitos" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE T_UIAF AS LOGICAL INITIAL no 
     LABEL "UIAF lavado de Activos" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1.04 NO-UNDO.

DEFINE BUTTON BUTTON-10 
     LABEL "Ejecutar" 
     SIZE 17 BY 1.08.

DEFINE VARIABLE Efec_noa AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Entrada de efectivo no Anticipado" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE Peefec AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Proyeccion entrada de efectivo" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE Psefe AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Proyeccion Salidas de efectivo" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE sefena AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Salida de efectivo no Anticipado" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm0
     t-2 AT ROW 2.62 COL 50
     T_UIAF AT ROW 2.62 COL 62
     t1 AT ROW 2.65 COL 31
     T-10 AT ROW 2.85 COL 12
     BUTTON-3 AT ROW 2.85 COL 98
     BUTTON-2 AT ROW 4.54 COL 98
     t-5 AT ROW 4.77 COL 52
     t4 AT ROW 5.04 COL 8
     T-6 AT ROW 5.58 COL 52
     T5 AT ROW 5.85 COL 8
     BUTTON-6 AT ROW 6.12 COL 98
     T-7 AT ROW 6.38 COL 52
     T6 AT ROW 6.65 COL 8
     t-8 AT ROW 7.19 COL 52
     t7 AT ROW 7.42 COL 8
     Btn_Ejecutar AT ROW 7.73 COL 98
     t-13 AT ROW 8 COL 52
     t-9 AT ROW 8.27 COL 8
     t8 AT ROW 8.81 COL 52
     t-11 AT ROW 9.08 COL 8
     t-18 AT ROW 9.62 COL 52
     t-12 AT ROW 9.88 COL 8
     tgSaldosDiariosDepositos AT ROW 10.23 COL 52.14 WIDGET-ID 14
     t-19 AT ROW 10.69 COL 8 WIDGET-ID 6
     t2 AT ROW 13.12 COL 8
     t3 AT ROW 13.12 COL 22
     Fec_Corte AT ROW 15.19 COL 36 COLON-ALIGNED
     SalMin AT ROW 15.19 COL 64 COLON-ALIGNED WIDGET-ID 8
     UVR AT ROW 15.19 COL 79 COLON-ALIGNED WIDGET-ID 10
     BUTTON-11 AT ROW 16 COL 80.86 WIDGET-ID 12
     m2 AT ROW 16.85 COL 36 COLON-ALIGNED
     BtnDone AT ROW 17.42 COL 98
     m1 AT ROW 17.96 COL 36 COLON-ALIGNED
     Prc AT ROW 19.04 COL 36 COLON-ALIGNED
     BUTTON-5 AT ROW 19.35 COL 103
     CED AT ROW 20.15 COL 36 COLON-ALIGNED
     "  SUPERSOLIDARIA" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 3.96 COL 7
          FGCOLOR 7 
     " Evolución de los Informes Exportados" VIEW-AS TEXT
          SIZE 33 BY .85 AT ROW 14.35 COL 8
          FGCOLOR 7 
     "Versión: Abril 19 / 2012" VIEW-AS TEXT
          SIZE 36 BY 1.08 AT ROW 1 COL 76 WIDGET-ID 4
          FGCOLOR 9 FONT 1
     " Centrales de Riesgo" VIEW-AS TEXT
          SIZE 19 BY .85 AT ROW 1.65 COL 8
          FGCOLOR 7 
     "  FOGACOOP" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 12.04 COL 7
          FGCOLOR 7 
     RECT-25 AT ROW 12.58 COL 5
     RECT-26 AT ROW 4.23 COL 5
     RECT-28 AT ROW 4.23 COL 45
     RECT-29 AT ROW 14.46 COL 5
     RECT-290 AT ROW 2.12 COL 5
     RECT-291 AT ROW 2.62 COL 97
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 24.24
         BGCOLOR 17 FONT 5.

DEFINE FRAME frm_brechas
     Efec_noa AT ROW 1.27 COL 33 COLON-ALIGNED
     sefena AT ROW 2.35 COL 6.57
     Psefe AT ROW 3.42 COL 33 COLON-ALIGNED
     Peefec AT ROW 4.5 COL 33 COLON-ALIGNED
     BUTTON-10 AT ROW 6.12 COL 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 49 ROW 12.04
         SIZE 55 BY 7.27
         BGCOLOR 17 
         TITLE "Flujos Brecha de Liquidez".


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
         TITLE              = "Reportes Entidades Externas"
         HEIGHT             = 20.73
         WIDTH              = 111.86
         MAX-HEIGHT         = 37.5
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 37.5
         VIRTUAL-WIDTH      = 182.86
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
/* REPARENT FRAME */
ASSIGN FRAME frm_brechas:FRAME = FRAME Frm0:HANDLE.

/* SETTINGS FOR FRAME Frm0
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME Frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CED IN FRAME Frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m1 IN FRAME Frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m2 IN FRAME Frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Prc IN FRAME Frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frm_brechas
                                                                        */
ASSIGN 
       FRAME frm_brechas:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN sefena IN FRAME frm_brechas
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Reportes Entidades Externas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Reportes Entidades Externas */
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
ON CHOOSE OF BtnDone IN FRAME Frm0 /* Salir */
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


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME Frm0 /* Ejecutar */
DO:
  IF DECIMAL(UVR:SCREEN-VALUE) LE 0 AND (T6:SCREEN-VALUE EQ "Yes") THEN DO:
      MESSAGE "Debe digitar el valor del UVR para el Individual de Cartera"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF Fec_Corte:SCREEN-VALUE EQ "" OR Fec_Corte:SCREEN-VALUE EQ "?" THEN DO:
     MESSAGE "Se debe entrar una fecha de corte para" SKIP
             "la exportación de los datos. Rectfique!" VIEW-AS ALERT-BOX ERROR.
     APPLY "Entry" TO Fec_Corte.
     RETURN NO-APPLY.
  END.
  RUN Procesar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frm_brechas
&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 wWin
ON CHOOSE OF BUTTON-10 IN FRAME frm_brechas /* Ejecutar */
DO:
  RUN Brecha.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm0
&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 wWin
ON CHOOSE OF BUTTON-11 IN FRAME Frm0 /* Ver UVR */
DO:
  /*
  http://www.elcolombiano.com/IndicadoresCorfinsura.asp?numid=83
  */

  RUN ejecutar.r (INPUT "http://www.elcolombiano.com/IndicadoresCorfinsura.asp?numid=83").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fec_Corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_Corte wWin
ON LEAVE OF Fec_Corte IN FRAME Frm0 /* Fecha de Corte */
DO:
  ASSIGN Fec_Corte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-19 wWin
ON VALUE-CHANGED OF t-19 IN FRAME Frm0 /* Operaciones Por Producto (Mensual) */
DO:
   IF NOT t-19 THEN
        ASSIGN t-19 = YES.
   ELSE
        ASSIGN t-19= NO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AhoFgc wWin 
PROCEDURE AhoFgc :
/*------------------------------------------------------------------------------
  Purpose: Genera Archivo Plano Ahorros FOGACOOP     
------------------------------------------------------------------------------*/
ASSIGN Prc = 0
       acum = 0
       xacum = 0.

EMPTY TEMP-TABLE TmpAFG.

FOR EACH Ahorros WHERE Ahorros.Estado = 1
                   AND Ahorros.Cod_Ahorro NE 1 NO-LOCK:
    IF (Ahorros.Sdo_Disponible + Sdo_Canje) GT 0 THEN DO:
        ASSIGN Prc = Prc + 1
               Ced = Ahorros.Nit.

        DISPLAY Prc Ced WITH FRAME FRM0.

        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
        
        CREATE TmpAFG.
        ASSIGN TmpAFG.FA_TipIde = "C"
               TmpAFG.FA_CedNit = (Ahorros.Nit).

        IF Ahorros.Fec_Apertura EQ ? THEN
            TmpAFG.FA_FecApe = "31/12/1999".
        ElSE
            Assign TmpAFG.FA_FecApe = STRING(DAY(Ahorros.Fec_Apertura),"99") + "/" +
                                      STRING(MONTH(Ahorros.Fec_Apertura),"99")  + "/" +
                                      STRING(YEAR(Ahorros.Fec_Apertura),"9999").

        Assign TmpAFG.FA_Plazo = Ahorros.Plazo
               TmpAFG.FA_Modali = 1.

        ASSIGN TmpAFG.FA_ValIni = Ahorros.Sdo_disponible + Ahorros.sdo_canje
               TmpAFG.FA_Saldo = Ahorros.Sdo_Disponible + ahorros.sdo_canje.

        IF Ahorros.Fec_Vencimiento EQ ? THEN
            TmpAFG.FA_FecVto = "31/12/2003".
        ELSE
            TmpAFG.FA_FecVto = STRING(DAY(Ahorros.Fec_Vencimiento),"99") + "/" +
                               STRING(MONTH(Ahorros.Fec_Vencimiento),"99")  + "/" +
                               STRING(YEAR(Ahorros.Fec_Vencimiento),"9999").

        FIND FIRST Clientes WHERE Clientes.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            ASSIGN TmpAFG.FA_Apell1 = Clientes.Apellido1
                   TmpAFG.FA_Apell2 = Clientes.Apellido2
                   TmpAFG.FA_NomNit = Clientes.Nombre.

        CASE Pro_Ahorros.Tip_Ahorro:
            WHEN 1 THEN
                ASSIGN TmpAFG.FA_TipAho = "CAHO"
                       TmpAFG.FA_Amorti = 1
                       TmpAFG.FA_Plazo = 1
                       TmpAFG.FA_Tasa = 4.28.

            WHEN 3 THEN DO:
                ASSIGN TmpAFG.FA_TipAho = "CDAT"
                       TmpAFG.FA_Amorti = 30.

                CASE Ahorros.Per_Liquidacion:
                    WHEN 0 THEN
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 360
                               TmpAFG.FA_Plazo = Ahorros.Plazo.
                    WHEN 4 THEN
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 12
                               TmpAFG.FA_Plazo = Ahorros.Plazo * 30.
                    WHEN 6 THEN
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 4
                               TmpAFG.FA_Plazo = Ahorros.Plazo * 90.
                    WHEN 8 THEN
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 2
                               TmpAFG.FA_Plazo = Ahorros.Plazo * 180.
                    WHEN 9 THEN
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa
                               TmpAFG.FA_Plazo = Ahorros.Plazo * 360.
                    WHEN 10 THEN
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 360
                               TmpAFG.FA_Plazo = Ahorros.Plazo.
                    OTHERWISE
                        ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa
                               TmpAFG.FA_Plazo = Ahorros.Plazo.
                END CASE.
            END.

            OTHERWISE
                ASSIGN TmpAFG.FA_TipAho = "OTRO"
                       TmpAFG.FA_Amorti = 30
                       TmpAFG.FA_Plazo = Ahorros.Plazo * 30
                       TmpAFG.FA_Tasa = 10.03.
        END CASE.
    END.
END.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "FGCAHO.TXT") NO-CONVERT.
    FOR EACH TmpAFG BREAK BY FA_TipAho:
        Acum = Acum + FA_Saldo.
        Xacum = xacum + FA_ValIni.
        K = K + 1.

        /*campo0 = */

        DO J = 1 TO 14 BY 1:
            CASE J:
                WHEN 1 THEN Campo = STRING(FA_TipIde).
                WHEN 2 THEN Campo = STRING(FA_CedNit).
                WHEN 3 THEN Campo = STRING(FA_Apell1).
                WHEN 4 THEN Campo = STRING(FA_Apell2).
                WHEN 5 THEN Campo = STRING(FA_NomNit).
                WHEN 6 THEN Campo = STRING(FA_TipAho).
                WHEN 7 THEN Campo = STRING(FA_ValIni).
                WHEN 8 THEN Campo = STRING(FA_FecApe).
                WHEN 9 THEN Campo = STRING(FA_Plazo).
                WHEN 10 THEN Campo = FA_FecVto.
                WHEN 11 THEN Campo = STRING(FA_Tasa).
                WHEN 12 THEN Campo = STRING(FA_Amorti).
                WHEN 13 THEN Campo = STRING(FA_Modali).
                WHEN 14 THEN Campo = STRING(FA_Saldo).
            END CASE.

            /*CAMPO0 = "4," + STRING(J) + ",1," + STRING(K) + "," + Campo.*/



            PUT UNFORMATTED CAMPO0 AT 1.
        END.
    END.

    K = 0.
OUTPUT CLOSE.

OUTPUT TO VALUE("C:\INFRED\" + "TOTFGCAHO.TXT") NO-CONVERT.
    display "Total Captacion " Acum Xacum.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BienesyServicio wWin 
PROCEDURE BienesyServicio :
/*- Purpose: Genera archivo plano de creditos para SUPERSOLIDARIA */
 DEFINE VARIABLE W_tasa LIKE creditos.tasa.
 DEFINE VARIABLE periodo AS INTEGER.
 EMPTY TEMP-TABLE SSCre.
 Assign  Prc = 0
         acum = 0.

 FOR EACH Creditos WHERE creditos.tip_credito EQ 5 AND
                         Creditos.Sdo_Capital    GT 0 AND
                         Creditos.Fec_Desembolso LE Fec_Corte AND
                         Creditos.Estado         EQ 2 
                         NO-LOCK:
   IF creditos.sdo_capital LE 0 THEN NEXT.
   FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ creditos.cod_credito NO-LOCK NO-ERROR.
   /*IF Pro_Creditos.Tip_Credito EQ 4 THEN NEXT.*/
   ASSIGN Prc = Prc + 1
          Ced = Creditos.Nit.
   DISPLAY Prc Ced WITH FRAME FRM0.
   CREATE SSCre.
   ASSIGN W_tasa = creditos.tasa.
   IF creditos.tasa > 18.9 THEN W_tasa = 18.9.

   ASSIGN SSCre.SC_TipIde = "C"
          SSCre.SC_CodCon =  substring(Creditos.Cta_Contable,1,6)
          SSCre.SC_TipCuo = 1
          SSCre.SC_FecApe = STRING(DAY(Creditos.Fec_Desembolso),"99") + "/" +
                            STRING(MONTH(Creditos.Fec_Desembolso),"99") + "/" +
                            STRING(YEAR(Creditos.Fec_Desembolso),"9999") 
          SSCre.SC_ValCuo = Creditos.Cuota
          SSCre.SC_CuoPag = Creditos.Cuo_Pagadas
          SSCre.SC_Pagare = Creditos.Num_Credito
          SSCre.SC_SdoCap = round(Creditos.Sdo_Capital,0)
          SSCre.SC_Monto  = Creditos.Monto
          SSCre.SC_SdoOtr = 0
          SSCre.SC_SdoInt = round(Creditos.Int_Corrientes,0)
          SSCre.SC_VlrPro = Creditos.Provision
          sscre.sc_vlrproint = creditos.provision_interes
          SSCre.SC_ConInt = creditos.INT_difcobro
          SSCre.SC_VlrExt = 0
          SSCre.SC_MesExt = 0
          SSCre.SC_Modali = 2
          SSCre.SC_TasNom = round(W_Tasa,2).
          
   IF creditos.plazo > 12 THEN 
      SSCre.SC_PLAZO  = 2.
   ELSE 
      SSCre.SC_PLAZO  = 1.

   FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Clientes) THEN  DO:
      ASSIGN SSCre.SC_NomNit = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      IF Clientes.Cod_Anterior NE "" THEN
          SSCre.SC_CedNit = (Clientes.nit).
      ELSE
          SSCre.SC_CedNit = (Creditos.Nit).
   END.
   ELSE ASSIGN SSCre.SC_NomNit = "NOMBRE NO REGISTRADO"
               SSCre.SC_CedNit = (Creditos.Nit).

  
  FIND FIRST Garantias WHERE Garantias.Num_credito = Creditos.Num_Credito NO-LOCK NO-ERROR.
  IF AVAILABLE(Garantias) THEN DO:
    IF Garantias.Tipo_Garantia = 1 THEN DO:
      CASE Garantias.Tipo_Garantia:
       WHEN 1 THEN SSCre.SC_ClaGar = "1".
       WHEN 2 THEN SSCre.SC_ClaGar = "1".
       WHEN 3 THEN SSCre.SC_ClaGar = "1".
       WHEN 4 THEN SSCre.SC_ClaGar = "1".
       OTHERWISE SSCre.SC_ClaGar = "1".
      END CASE.
    END.
    IF Garantias.Tipo_Garantia = 2 THEN SSCre.SC_ClaGar = "1".
  END.
  Assign SSCre.SC_ClaGar = "1".
  /* halla tasa efec */
  RUN NVEF IN W_ManFin (INPUT W_tasa / 1200, 12, OUTPUT SSCre.SC_TasEfe) NO-ERROR.          
  SSCre.SC_TasEfe = (SSCre.SC_TasEfe * 100).
  SSCre.SC_TasEfe = round(SSCre.SC_TasEfe,2).
  CASE Pro_Creditos.Tip_Credito:
    WHEN 1 THEN SSCre.SC_Clasif = 2. /*Consumo*/
    WHEN 2 THEN SSCre.SC_Clasif = 1. /*Comercial*/
    WHEN 2 THEN SSCre.SC_Clasif = 3. /*Hipotecario*/
    WHEN 4 THEN SSCre.SC_Clasif = 4. /*Sin Identificar*/
  END CASE.
  Assign SSCre.SC_Clasif = 2.
  CASE Creditos.categoria:
   WHEN "A" THEN ASSIGN SSCre.SC_EdaMor  = 0
                      SSCre.SC_Catego = "A".
   WHEN "A" THEN ASSIGN SSCre.SC_EdaMor = 0
                      SSCre.SC_Catego = "A".
   WHEN "B" THEN ASSIGN SSCre.SC_EdaMor = 31
                      SSCre.SC_Catego = "B".
   WHEN "C" THEN 
    DO:
     SSCre.SC_Catego = "C".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN SSCre.SC_EdaMor = 61.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN SSCre.SC_EdaMor = 121.
    END.
   WHEN "D" THEN 
    DO:
     SSCre.SC_Catego = "D".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN SSCre.SC_EdaMor = 91.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN SSCre.SC_EdaMor = 181.
    END.
   WHEN "E" THEN
    DO:
     SSCre.SC_Catego = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN SSCre.SC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN SSCre.SC_EdaMor = 361.
    END.
    OTHERWISE ASSIGN SSCre.SC_EdaMor  = 0
                      SSCre.SC_Catego = "A".
  END CASE. 
  ASSIGN SSCre.SC_Catego = Creditos.Categoria.
  CASE Creditos.Per_Pago:
   WHEN 1 THEN ASSIGN SSCre.SC_Amorti = 7
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 7)).
   WHEN 2 THEN ASSIGN SSCre.SC_Amorti = 10
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 10)).
   WHEN 3 THEN ASSIGN SSCre.SC_Amorti = 15
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 15)).
   WHEN 4 THEN ASSIGN SSCre.SC_Amorti = 30
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 30)).
   WHEN 5 THEN ASSIGN SSCre.SC_Amorti = 60
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 60)).
   WHEN 6 THEN ASSIGN SSCre.SC_Amorti = 90
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 90)).
   WHEN 7 THEN ASSIGN SSCre.SC_Amorti = 120
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 120)).
   WHEN 8 THEN ASSIGN SSCre.SC_Amorti = 180
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 180)).
   WHEN 9 THEN ASSIGN SSCre.SC_Amorti = 360
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 360)).
  END CASE.
  
  IF dias_atraso < 30 AND fecaux < Fec_Corte THEN fecaux = fec_corte + 30. 

  IF SSCre.SC_Amorti GT 30  THEN
     SSCre.SC_Amorti = 30.
  SSCre.SC_FecVen = STRING(DAY(FecAux),"99") + "/" +
                    STRING(MONTH(FecAux),"99")  + "/" +
                    STRING(YEAR(FecAux),"9999").
  SSCre.SC_FecUltpago = STRING(DAY(Creditos.Fec_UltPago),"99") + "/" +
                        STRING(MONTH(Creditos.Fec_UltPago),"99")  + "/" +
                        STRING(YEAR(Creditos.Fec_UltPago),"9999").
  IF SSCre.SC_FecUltpago EQ ? THEN SSCre.SC_FecUltpago = "".
  ASSIGN SSCre.SC_ClGar = "4"
         SSCre.SC_DesCre = "O".

  SSCre.SC_Agen = string(creditos.agencia).
  ssCre.SC_capmora = 0.
  IF SSCre.SC_EdaMor GT 0 THEN
     SSCre.SC_capmora = int(creditos.val_atraso).
  IF sscre.sc_capmora GT creditos.sdo_capital  THEN ssCre.SC_capmora = creditos.sdo_capital - 2.
  
END.
OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSByS.csv") NO-CONVERT.

 FOR EACH SSCre:
     ASSIGN acum = acum + SSCre.SC_sdocap.
     ASSIGN CAMPO0 = SSCre.SC_TipIde           + ";" +
                     SSCre.SC_CedNit           + ";" +
                     SSCre.SC_NomNit           + ";" +
                     STRING(SSCre.SC_CodCon)   + ";" +
                     STRING(SSCre.SC_Pagare)   + ";" +
                     "1"                       + ";" +  
                     "2"                       + ";" +
                     STRING(SSCre.SC_Pagare)   + ";" +
                     SSCre.SC_FecApe           + ";" +
                     SSCre.SC_FecVen           + ";" +
                     "12"                      + ";" +           
                     STRING(SSCre.SC_Monto)    + ";" +
                     "0"                       + ";" +
                     "0"                       + ";" +
                     "0"                       + ";" +
                     STRING(SSCre.SC_SdoCap)   + ";" +
                     "1"                       + ";" +
                     STRING(SSCre.SC_TasNom)   + ";" +
                     STRING(SSCre.SC_TasEfe)   + ";" +
                     STRING(SSCre.SC_EdaMor)   + ";" +
                     "1"                       + ";" +
                     STRING(SSCre.SC_SdoInt)   + ";" +                                              
                     STRING(SSCre.SC_SdoOtr)   + ";" +
                     STRING(SC_VlrPro)         + ";" +
                     string(sc_vlrproint)      + ";" +
                     STRING(SSCre.SC_ConInt)   + ";" +
                     "1"                       + ";" +
                     "0"                       + ";" +
                     SSCre.SC_Agen             . 
   PUT UNFORMATTED
       CAMPO0 AT 1.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\Info_Utrahuilca\" + "TOTSSByS.txt") NO-CONVERT.
  display "Total Creditos " Acum .
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Brecha wWin 
PROCEDURE Brecha :
DEFINE VAR NomPdt AS CHARACTER FORMAT "X(15)".
DEFINE VAR Prom   AS DECIMAL FORMAT ">>.99".
DEFINE VAR Tot    AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
ASSIGN FECHA1 = Fec_Corte.
DEFINE VAR columna1  AS DECIMAL.
DEFINE VAR columna2  AS DECIMAL.
DEFINE VAR columna3  AS DECIMAL.
DEFINE VAR columna4  AS DECIMAL.
DEFINE VAR columna5  AS DECIMAL.
DEFINE VAR columna6  AS DECIMAL.
DEFINE VAR columna7  AS DECIMAL.
DEFINE VAR columna8  AS DECIMAL.
DEFINE VAR columna9  AS DECIMAL.
DEFINE VAR columna10 AS DECIMAL.
DEFINE VAR columna11 AS DECIMAL.
CAMPO0 = "".
/********************/
/*entradas AHORROS A TERMINO*/
/********************/

 for each ahorros WHERE AHORROS.ESTADO = 1 AND SDO_DISPONIBLE > 0 AND (AHORROS.COD_Ahorro = 3 OR AHORROS.COD_Ahorro = 5)
                                           AND ((month(Ahorros.Fec_Apertura) = MONTH (Fec_corte) AND YEAR (Ahorros.Fec_Apertura) = year (Fec_corte)) OR (month(Ahorros.Fec_Prorroga) = MONTH (Fec_corte)AND YEAR(Ahorros.Fec_Prorroga) = YEAR(Fec_corte))):
    ASSIGN columna1 = columna1 + SDO_DISPONIBLE + SDO_CANJE + int_pagar
           columna3  = columna3 + TASA
           XTITULOS   = XTITULOS + 1.
 end.
 COLUMNA3 = COLUMNA3 / xtitulos.

/********************/
/*salidas AHORROS A TERMINO*/
/********************/
 for each ahorros WHERE AHORROS.ESTADO = 1 AND SDO_DISPONIBLE > 0 AND (AHORROS.COD_Ahorro = 3 OR AHORROS.COD_Ahorro = 5)
                                           AND ((month(Ahorros.Fec_Cancelacion) = MONTH (Fec_corte) AND YEAR (Ahorros.Fec_Cancelacion) = year (Fec_corte))):
    ASSIGN COLUMNA2 = COLUMNA2 + SDO_DISPONIBLE + SDO_CANJE + int_pagar.
 end.

 /******/
/* SALIDA CARTERA*/
/*******/
 ASSIGN Xsumavalor = 0
        xsumatasa  = 0
        xtitulos   = 0.

 for each CREDITOS WHERE CREDITOS.ESTADO = 1 AND  (MONTH(Fec_Desembolso) = MONTH(FEC_CORTE) AND YEAR(Fec_Desembolso) = YEAR(FEC_CORTE)):
    ASSIGN COLUMNA5 = COLUMNA5 + CREDITOS.Monto . 
 end.

/******/
/* ENTRADA CARTERA*/
/*******/

 for each CREDITOS WHERE CREDITOS.ESTADO = 1:
    ASSIGN COLUMNA4 = COLUMNA4 + Int_Corrientes. 
    columna6  = columna6 + CREDITOS.TASA.
    XTITULOS   = XTITULOS + 1.
 end.
 COLUMNA6 = COLUMNA6 / xtitulos.

 columna7  = Efec_noa.
 columna8  = sefena.
 columna9  = Psefe.
 columna10 = Peefec.



 OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "BRECHA.TXT") NO-CONVERT.
  CAMPO0 =  STRING(FEC_CORTE) + ";" +
            string(COLUMNA1)  + ";" +
            string(COLUMNA2)  + ";" +
            string(COLUMNA3)  + ";" +
            string(COLUMNA4)  + ";" +
            string(COLUMNA5)  + ";" +
            string(COLUMNA6)  + ";" +
            string(COLUMNA7)  + ";" +
            string(COLUMNA8)  + ";" +
            string(COLUMNA9)  + ";" +
            string(COLUMNA10) + ";" +
            string(COLUMNA11).
       
   PUT UNFORMATTED
       CAMPO0 AT 1.
 OUTPUT TO CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BreFG wWin 
PROCEDURE BreFG :
DEFINE VAR NomPdt AS CHARACTER FORMAT "X(15)".
DEFINE VAR Prom   AS DECIMAL FORMAT ">>.99".
DEFINE VAR Tot    AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
EMPTY TEMP-TABLE brecha.
ASSIGN FECHA1 = Fec_Corte.
DO I = 1 TO 8 BY 1:
  CASE i:
        WHEN 1 THEN
                run hallarango ((FECHA1 - 999),(FECHA1 + 30)).
        WHEN 2 THEN  
                run hallarango ((FECHA1 + 31),(FECHA1 + 60)).
        WHEN 3 THEN  
                run hallarango ((FECHA1 + 61),(FECHA1 + 90)).
        WHEN 4 THEN
                run hallarango ((FECHA1 + 91),(FECHA1 + 120)).
        WHEN 5 THEN  
                run hallarango ((FECHA1 + 121),(FECHA1 + 180)).
        WHEN 6 THEN  
                run hallarango ((FECHA1 + 181),(FECHA1 + 270)).
        WHEN 7 THEN  
                run hallarango ((FECHA1 + 271),(FECHA1 + 360)).
        WHEN 8 THEN  
                run hallarango ((FECHA1 + 361),(FECHA1 + 9999)).
   END CASE.
END.

/******/
/* AHORROS A LA VISTA*/
/*******/
 ASSIGN Xsumavalor = 0
        xsumatasa  = 0
        xtitulos   = 0.

for each ahorros WHERE AHORROS.ESTADO = 1 AND SDO_DISPONIBLE > 0 AND AHORROS.COD_Ahorro = 2:
    ASSIGN XSUMAVALOR = XSUMAVALOR + ahorros.SDO_DISPONIBLE + ahorros.SDO_CANJE + ahorros.int_pagar
           XSUMATASA  = XSUMATASa + ahorros.TASA
           XTITULOS   = XTITULOS + 1.
end.
create brecha.
assign brecha.producto = 3
       brecha.Banda    = i
       brecha.valor    = xsumavalor
       brecha.tasa     = 0.20 * XTITULOS
       brecha.Nro      = xtitulos.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "FGCBRE.TXT") NO-CONVERT.

DISPLAY "BRECHA DE LIQUIDEZ" AT 25 skip
        "MES : "             AT 5
        MONTH(Fec_Corte)      At 16 skip(1)
        "PRODUCTO             BANDA           VALOR          TASA PROMEDIO " AT 1
        "__________________________________________________________________" AT 1.
        
FOR EACH brecha BREAK BY Brecha.Producto BY Banda:
  IF FIRST-OF(Brecha.Producto) THEN
  DO:
    CASE Brecha.Producto:
      WHEN 1 THEN
        NomPdt = "Ahorros".
      WHEN 2 THEN
        NomPdt = "Creditos".
      WHEN 3 THEN
        NomPdt = "A la Vista".
    END CASE.
  END.
  ELSE
   NomPdt = "".
  Prom = (Brecha.Tasa / Brecha.Nro) * 12.
  DISPLAY NomPdt      AT 1
          banda       AT 17
          valor       AT 30 FORMAT ">,>>>,>>>,>>>,>>9"
          Prom        AT 60 FORMAT ">>9.999" WITH WIDTH 132 NO-BOX USE-TEXT NO-LABELS.    
  Tot = Tot + Valor.
  IF LAST-OF(Brecha.Producto) THEN
  DO:
     DISPLAY SKIP(1) 
             "Total: " AT 1
             Tot AT 29 SKIP(2) 
             WITH WIDTH 132 FRAME Ftot USE-TEXT NO-BOX NO-LABELS.
     Tot = 0.
  END.     
END.
OUTPUT TO CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calificacion wWin 
PROCEDURE Calificacion :
FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
  CASE Pro_Creditos.Tip_Credito:
   WHEN 1 THEN /*Consumo */
    TmpDat.ITipCre = 2.
   WHEN 2 THEN /* Comercial */
    TmpDat.ITipCre = 1.
   WHEN 3 THEN /* Hipotecario */
    TmpDat.ITipCre = 3.
   WHEN 4 THEN /* Microcredito */
    TmpDat.ITipCre = 4.
  END CASE.
  
  ASSIGN TmpDat.INoveda = 01 
         IEdaMor        = 1
         TmpDat.ICalifi = "A"
         TmpDat.ISdoMor = 0
         TmpDat.ICuoMor = 0.

  IF creditos.dias_atraso < 31 THEN ASSIGN TmpDat.INoveda = 001 
                                           IEdaMor        = 1
                                           TmpDat.ICalifi = "A".
  ELSE 
    IF creditos.dias_atraso < 60 THEN ASSIGN TmpDat.INoveda = 001     /*006*/ 
                                             IEdaMor        = 1
                                             TmpDat.ICalifi = "A".
    ELSE 
      IF creditos.dias_atraso < 90 THEN ASSIGN TmpDat.INoveda = 007  
                                               IEdaMor        = 60
                                               TmpDat.ICalifi = "B"
                                               TmpDat.ICuoMor = Creditos.Cuo_Atraso
                                               TmpDat.ISdoMor = Creditos.Val_Atraso.
      ELSE                                      
        IF creditos.dias_atraso < 120 THEN ASSIGN TmpDat.INoveda = 008 
                                                  IEdaMor        = 90
                                                  TmpDat.ICalifi = "C"
                                                  TmpDat.ICuoMor = Creditos.Cuo_Atraso
                                                  TmpDat.ISdoMor = Creditos.Val_Atraso.
        ELSE                                    
          IF creditos.dias_atraso < 150 THEN ASSIGN TmpDat.INoveda = 009 
                                                    IEdaMor        = 120
                                                    TmpDat.ICalifi = "D"
                                                    TmpDat.ICuoMor = Creditos.Cuo_Atraso
                                                    TmpDat.ISdoMor = Creditos.Val_Atraso.
          ELSE  DO: 
            ASSIGN TmpDat.ICalifi = "E"
                   TmpDat.ISdoMor = Creditos.Val_Atraso
                   TmpDat.ICuoMor = Creditos.Cuo_Atraso.
            IF creditos.dias_atraso < 180 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 150.                    
            ELSE                                                                      
              IF creditos.dias_atraso < 210 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 180.                      
              ELSE                                     
                IF creditos.dias_atraso < 240 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 210.                      
                ELSE                                     
                  IF creditos.dias_atraso < 270 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 240.                      
                  ELSE                                     
                    IF creditos.dias_atraso < 300 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 270.                      
                    ELSE                                     
                      IF creditos.dias_atraso < 330 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 300.                      
                      ELSE                                     
                        IF creditos.dias_atraso < 360 THEN ASSIGN TmpDat.INoveda = 009 IEdaMor = 330.                      
                        ELSE
                           ASSIGN TmpDat.INoveda = 009 IEdaMor = 360. 
          END.
 /* CASE creditos.Cod_calificaMes:
      WHEN 0 THEN
        TmpDat.ICalifi = "A".
      WHEN 1 THEN
        TmpDat.ICalifi = "A".
      WHEN 2 THEN
        TmpDat.ICalifi = "A".
      WHEN 3 THEN
        TmpDat.ICalifi = "B".
      WHEN 4 THEN
        TmpDat.ICalifi = "C".
      WHEN 5 THEN
        TmpDat.ICalifi = "D".
      OTHERWISE 
        TmpDat.ICalifi = "E".
  END.*/

  IF IEdaMor EQ ? OR IEdaMor LE 0 THEN 
     ASSIGN IEdaMor        = 1
            TmpDat.INoveda = 001
            TmpDat.ICalifi = "A".

  /* temporal  Coment.Feb.3/07
  TmpDat.ICalifi = "A".
  ASSIGN TmpDat.INoveda = 001 
         IEdaMor = 1.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cifin_Codeudores wWin 
PROCEDURE Cifin_Codeudores :
DEF VAR W-Desc AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR W-LTiposIde AS CHAR INITIAL "01,03,04,05" NO-UNDO.
DEF VAR W-Ceros AS CHAR INITIAL "0000000000000000000000000000000000000000000000000000000000000000000000000000000".
DEF VAR W-Espacios AS CHAR INITIAL "                                          ".

FOR EACH Relaciones WHERE Relaciones.Nit EQ rep_Creditos.Nit
                      AND INTEG(Relaciones.Cuenta) EQ rep_Creditos.Num_Credito
                      AND Relaciones.Clase_Producto EQ 2
                      AND Relaciones.Cod_Producto EQ rep_Creditos.Cod_Credito
                      AND Relaciones.Cod_Relacion EQ 11
                      AND Relaciones.Estado EQ 1
                      AND Relaciones.Aprobada NO-LOCK:
    IF Relaciones.Nit_relacion EQ Relaciones.nit OR Relaciones.nit_relacion EQ "" THEN
        NEXT.

    FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Clientes) THEN DO:
        MESSAGE "No se encontro la cédula Codeudora " Relaciones.Nit_relacion SKIP
                "c.c. Titular "  rep_Creditos.Nit SKIP
                "para el crédito Nro " rep_Creditos.Num_credito SKIP
                "...Valide la información, informela a la dirección" SKIP
                "informática." SKIP(2)
                "Este registro no se procesará en el informe de CIFIN," SKIP
                "hasta no organizar el inconveniente descrito."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        RETURN.
    END.

    EMPTY TEMP-TABLE TCcifin.
    
    CREATE TCcifin.
    BUFFER-COPY Tcifin TO TCcifin.

    CREATE Tcifin.
    BUFFER-COPY TCcifin TO Tcifin.

    RELEASE TCcifin.

    w-CodDep = "".
    W-CodCiu = "".
    W-NomCiu = "".
    W-NomDep = "".

    /*tcifin.TipIde*/
    RUN tabla13.

    Tcifin.NroIde = Relaciones.Nit_Relacion.

    /* 4 - Nombre titular */
    IF LOOKUP(Tcifin.TipIde, W-LTiposIde) GT 0 THEN DO:
        ASSIGN Tcifin.Ape1 = SUBSTRING(Clientes.Apellido1,1,15).
               Tcifin.Ape2 = SUBSTRING(Clientes.Apellido2,1,15).
               Tcifin.Nom1 = SUBSTRING(Clientes.Nombre,1,INDEX(Clientes.Nombre," ") - 1).
               Tcifin.Nom2 = IF INDEX(Clientes.Nombre," ") NE 0 THEN SUBSTRING(Clientes.Nombre, INDEX(Clientes.Nombre, " ") + 1, 15)
                             ELSE "".
    END.
    ELSE DO:
        ASSIGN Tcifin.Ape1 = SUBSTRING(Clientes.Nombre,1,15)
               Tcifin.Ape2 = SUBSTRING(Clientes.Nombre,16,15)
               Tcifin.Nom1 = SUBSTRING(Clientes.Nombre,31,15)
               Tcifin.Nom2 = SUBSTRING(Clientes.Nombre,46,15).
    END.

    /* Depuracion de registros */
    IF INDEX(Tcifin.NroIde, "-") GT 0 THEN /* No envio de guiones */
        Tcifin.NroIde = SUBSTRING(Tcifin.NroIde,1,INDEX(Tcifin.NroIde, "-") - 1).

    Tcifin.Calidad   = "C". /* Codeudor Tabla 1 */

    Tcifin.NatJurid  = Tabla19(INPUT Clientes.Tipo_Cliente, INPUT Clientes.Nombre + " " + 
                                     Clientes.Apellido1 + Clientes.Apellido2).

    Tcifin.DirCasaTi = SUBSTRING(Clientes.Dir_Residencia,1,60).

    Tcifin.TelTitul  = Clientes.Tel_Residencia.

    Tabla33(Clientes.Lugar_Residencia).

    ASSIGN Tcifin.CodCiuCas = W-CodCiu
           Tcifin.CiudadCas = W-NomCiu
           Tcifin.CodDeptoC = W-CodDep
           Tcifin.DepCasaTi = W-NomDep.

    Empresa().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cifin_junio2009 wWin 
PROCEDURE Cifin_junio2009 :
EMPTY TEMP-TABLE  Tcifin.

/* 050: Tipo de Entidad
   153: Codigo de Entidad dado por CIFIN */

DEF VAR RegT1 AS CHAR FORMAT "X(30)" NO-UNDO.
DEF VAR RegT2 AS CHAR FORMAT "X(34)" NO-UNDO.
DEF VAR W-Desc AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR W-LTiposIde AS CHAR INITIAL "01,03,04,05" NO-UNDO.
DEF VAR W-Ceros AS CHAR INITIAL "0000000000000000000000000000000000000000000000000000000000000000000000000000000".
DEF VAR W-Espacios AS CHAR INITIAL "                                          ".
DEF VAR zfecPagAnti AS DATE INITIAL ? NO-UNDO.
DEF VAR zreg AS INTEG INITIAL 0 NO-UNDO.
DEFINE VAR valGarantia AS DECIMAL.
DEFINE VAR fechaAux AS DATE.
DEFINE BUFFER bfrRep_creditos FOR rep_creditos.

RegT1 = "121050153          01" + STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + STRING(DAY(Fec_Corte),"99").

FOR EACH rep_Creditos WHERE ((rep_creditos.fec_canceTotal >= fec_corte - DAY(fec_corte) + 1 AND rep_creditos.estado = 3) OR
                             (rep_creditos.estado = 5) OR
                             (rep_creditos.estado = 2))
                        AND rep_creditos.fecCorte = fec_corte NO-LOCK BREAK BY rep_Creditos.Nit:
    FIND FIRST clientes WHERE clientes.nit = rep_creditos.nit NO-LOCK NO-ERROR.

    IF rep_creditos.cod_credito <> 123 AND rep_creditos.estado <> 3 AND rep_creditos.estado <> 5 AND rep_creditos.fec_canceTotal <> ? THEN
        NEXT.

    ASSIGN w-CodDep = ""
           W-CodCiu = ""
           W-NomCiu = ""
           W-NomDep = ""
           W-Vlrgtia = "000000000000".

    /* Creditos desembolsados y cancelados dentro del mismo mes, no se informan  */
    IF YEAR(rep_Creditos.Fec_Desembolso) EQ YEAR(rep_Creditos.Fec_Cancetotal) AND MONTH(rep_Creditos.Fec_Desembolso) EQ MONTH(rep_Creditos.Fec_Cancetotal) THEN
        NEXT.

    CREATE Tcifin.

    Tcifin.TipReg = "2". /* 1 - Tipo de registro */
    RUN Tabla13. /* 2 - Tipo de identificación - Tcifin.TipIde */
    Tcifin.NroIde = rep_Creditos.Nit. /* 3 - Número de identificación */

    IF INDEX(Tcifin.NroIde, "-") > 0 THEN
        Tcifin.NroIde = SUBSTRING(Tcifin.NroIde,1,INDEX(Tcifin.NroIde, "-") - 1).

    /* 4 - Nombre titular */
    IF LOOKUP(Tcifin.TipIde, W-LTiposIde) GT 0 THEN DO:
        ASSIGN Tcifin.Ape1 = SUBSTRING(Clientes.Apellido1,1,15).
               Tcifin.Ape2 = SUBSTRING(Clientes.Apellido2,1,15).
               Tcifin.Nom1 = SUBSTRING(Clientes.Nombre,1,INDEX(Clientes.Nombre," ") - 1).
               Tcifin.Nom2 = IF INDEX(Clientes.Nombre," ") NE 0 THEN SUBSTRING(Clientes.Nombre, INDEX(Clientes.Nombre, " ") + 1, 15)
                             ELSE "".
    END.
    ELSE DO:
        ASSIGN Tcifin.Ape1 = SUBSTRING(Clientes.Nombre,1,15)
               Tcifin.Ape2 = SUBSTRING(Clientes.Nombre,16,15)
               Tcifin.Nom1 = SUBSTRING(Clientes.Nombre,31,15)
               Tcifin.Nom2 = SUBSTRING(Clientes.Nombre,46,15).
    END.

    /* 5 - Reservado */

    Tcifin.NroObli = TRIM(string(rep_Creditos.Num_Credito)). /* 6 - Número de la obligación */
    Tcifin.CodSuc = STRING(rep_creditos.agencia,"999999"). /* 7 - Código de la Sucursal */
    Tcifin.Calidad = "P". /* 8 - Calidad */
    RUN Tabla7. /* 9 - Calilficación - Tcifin.Califica */

    /* 10 - Situación o Estado del Titular */
    IF rep_creditos.abogado THEN
        Tcifin.EstadoTit = "05".
    ELSE
        Tcifin.EstadoTit = "06".

    /* 11 - Estado */
    IF rep_Creditos.Sdo_capital <= 0 THEN DO:
        Tcifin.Estado = "07".
        TCifin.NroCuoPag = STRING(rep_creditos.plazo,"999").

        IF rep_creditos.cod_credito = 123 AND rep_creditos.estado = 2 THEN
            Tcifin.Estado = "01".

        IF rep_creditos.abogado = TRUE THEN DO:
            TCifin.fecPago = STRING(YEAR(rep_Creditos.Fec_ultPago),"9999") + STRING(MONTH(rep_Creditos.Fec_ultPago),"99") + STRING(DAY(rep_Creditos.Fec_ultPago),"99").
            TCifin.tipoPago = "01".
            Tcifin.ModoExtin = "02".
        END.
    END.
    ELSE DO:
        IF rep_Creditos.Abogado AND rep_Creditos.Sdo_capital GT 0 AND rep_Creditos.Dias_Atraso LE 120 THEN
            Tcifin.Estado = "01".
        ELSE DO:
            IF rep_Creditos.Reestructurado = 1 AND rep_Creditos.Fec_Reestructurado NE ? AND rep_Creditos.Sdo_capital GT 0 THEN
                Tcifin.Estado = "02".
            ELSE DO:
                IF rep_Creditos.Dias_Atraso GT 120 AND rep_Creditos.Sdo_capital GT 0 THEN
                    Tcifin.Estado = "04".
                ELSE
                    Tcifin.Estado = "01".
            END.
        END.
    END.

    IF rep_creditos.estado EQ 5 THEN /* Creditos castigados */
        Tcifin.Estado = "06".

    /* 12 - Edad de mora o manejo */
    IF rep_creditos.dias_atraso LT 30 THEN Tcifin.EdadMora = "00".
    ELSE
        IF rep_creditos.dias_atraso LT 60 THEN Tcifin.EdadMora = "01".
        ELSE
            IF rep_creditos.dias_atraso LT 90 THEN Tcifin.EdadMora = "02".
            ELSE
                IF rep_creditos.dias_atraso LT 120 THEN Tcifin.EdadMora = "03".
                ELSE
                    IF rep_creditos.dias_atraso LT 150 THEN Tcifin.EdadMora = "04".
                    ELSE
                        IF rep_creditos.dias_atraso LT 180 THEN Tcifin.EdadMora = "05".
                        ELSE
                            IF rep_creditos.dias_atraso LT 210 THEN Tcifin.EdadMora = "06".
                            ELSE
                                IF rep_creditos.dias_atraso LT 240 THEN Tcifin.EdadMora = "07".
                                ELSE
                                    IF rep_creditos.dias_atraso LT 270 THEN Tcifin.EdadMora = "08".
                                    ELSE
                                        IF rep_creditos.dias_atraso LT 300 THEN Tcifin.EdadMora = "09".
                                        ELSE
                                            IF rep_creditos.dias_atraso LT 330 THEN Tcifin.EdadMora = "10".
                                            ELSE
                                                IF rep_creditos.dias_atraso LT 360 THEN Tcifin.EdadMora = "11".
                                                ELSE
                                                    IF rep_creditos.dias_atraso LT 540 THEN Tcifin.EdadMora = "12".
                                                    ELSE
                                                        IF rep_creditos.dias_atraso LT 730 THEN Tcifin.EdadMora = "13".
                                                        ELSE Tcifin.EdadMora = "14".

    Tcifin.AnosMora = STRING(TRUNCATE(rep_Creditos.Dias_Atraso / 360, 0),"99"). /* 13 - Años en mora */
    Tcifin.FecCorte = STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + STRING(DAY(Fec_Corte),"99"). /* 14 - Fecha de corte */
    Tcifin.FecIExp = STRING(YEAR(rep_Creditos.Fec_Desembolso),"9999") + STRING(MONTH(rep_Creditos.Fec_Desembolso),"99") + STRING(DAY(rep_Creditos.Fec_Desembolso),"99"). /* 15 - Fecha inicial o de expedición */

    /* 16 - Fecha de terminación */
    CASE rep_Creditos.Per_Pago:
         WHEN 1 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 7).
         WHEN 2 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 10).
         WHEN 3 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 15).
         WHEN 4 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 30).
         WHEN 5 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 60).
         WHEN 6 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 90).
         WHEN 7 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 120).
         WHEN 8 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 180).
         WHEN 9 THEN fechaAux = rep_Creditos.Fec_Desembolso + (rep_Creditos.Plazo * 360).
    END CASE.

    TCifin.FecTerm = STRING(YEAR(fechaAux),"9999") + STRING(MONTH(fechaAux),"99") + STRING(DAY(fechaAux),"99").
    Tcifin.FecExig   = "        ". /* 17 - Fecha de exigibilidad */
    Tcifin.FecPresc  = "        ". /* 18 - Fecha de prescripción */

    /* 19 - Fecha de pago */
    IF rep_Creditos.Fec_Cancetotal <> ? THEN
        Tcifin.FecPago = STRING(YEAR(rep_Creditos.Fec_Cancetotal),"9999") + STRING(MONTH(rep_Creditos.Fec_Cancetotal),"99") + STRING(DAY(rep_Creditos.Fec_Cancetotal),"99").
    ELSE
        Tcifin.FecPago = "        ".

    /* 20 - Modo de extinción */
    IF rep_Creditos.abogado THEN
        Tcifin.ModoExtin = "02".  /* Proceso Ejecutivo  */
    ELSE DO:
        CASE INTEGER(Tcifin.Estado):
            WHEN 1 /* Vigente*/ THEN Tcifin.ModoExtin = "01".  /* Voluntario */
            WHEN 2 /* Abogado*/ THEN Tcifin.ModoExtin = "02".  /* Proceso Ejecutivo  */
            WHEN 4 /* Dif Cob*/ THEN Tcifin.ModoExtin = "01".  /* Voluntario */
            WHEN 5 /* Irrecu */ THEN Tcifin.ModoExtin = "03".  /* Mandatoria de pago */
            WHEN 7 /* Cancela*/ THEN Tcifin.ModoExtin = "01".  /* Voluntario */
            WHEN 8 /* Recuper*/ THEN Tcifin.ModoExtin = "03".  /* Mandatoria de pago */
            WHEN 10 /* Concord*/ THEN Tcifin.ModoExtin = "02". /* Voluntario */
            OTHERWISE RETURN "  ".
        END CASE.
    END.

    /* 21 - Tipo de pago */
    IF rep_Creditos.Abogado /* Otra : Abogado */ THEN
        Tcifin.TipoPago = "02".
    ELSE
        IF rep_creditos.estado EQ 3 THEN
            Tcifin.TipoPago = "01".
        ELSE
            Tcifin.TipoPago = "  ".

    FIND FIRST bfrRep_creditos WHERE bfrrep_creditos.fecCorte = ADD-INTERVAL(fec_corte,-1 * DAY(fec_corte) ,"days")
                                AND bfrrep_creditos.nit = rep_creditos.nit
                                AND bfrrep_creditos.num_credito = rep_creditos.num_credito
                                AND bfrrep_creditos.dias_atraso > rep_creditos.dias_atraso NO-LOCK NO-ERROR.
    IF AVAILABLE bfrrep_creditos AND TCifin.fecPago = "        " AND TCifin.tipoPago = "  " THEN DO:
        tCifin.estado = "08".
        TCifin.fecPago = STRING(YEAR(rep_Creditos.Fec_ultPago),"9999") + STRING(MONTH(rep_Creditos.Fec_ultPago),"99") + STRING(DAY(rep_Creditos.Fec_ultPago),"99").
        TCifin.tipoPago = "01".
        Tcifin.ModoExtin = "02".
    END.

                
    /* 22 - Periodicidad de pago */
    CASE rep_Creditos.Per_Pago:
        WHEN 1 THEN Tcifin.PerPago = "01".  /* Semanal       */
        WHEN 2 THEN Tcifin.PerPago = "23".  /* Decadal       */
        WHEN 3 THEN Tcifin.PerPago = "04".  /* Quincenal     */
        WHEN 4 THEN Tcifin.PerPago = "07".  /* Mensual       */
        WHEN 5 THEN Tcifin.PerPago = "10".  /* Bimestral     */
        WHEN 6 THEN Tcifin.PerPago = "13".  /* Trimestral    */
        WHEN 7 THEN Tcifin.PerPago = "23".  /* Cuatrimestral */
        WHEN 8 THEN Tcifin.PerPago = "16".  /* semestral     */
        WHEN 9 THEN Tcifin.PerPago = "19".  /* Anual         */
        OTHERWISE Tcifin.PerPago = "23".   /* Otros Periodos*/
    END CASE.

    Tcifin.ProbNoPag = "   ". /* 23 - Probabilidad de no pago */
    Tcifin.NroCuoPag = STRING(rep_creditos.cuo_pagadas,"999"). /* 24 - Número de cuotas pagadas */
    Tcifin.NroCuoPac = STRING(rep_Creditos.Plazo,"999"). /* 25 - Número de cuotas pactadas */
    Tcifin.CuotaMora = STRING(rep_creditos.cuo_atraso,"999"). /* 26 - Cuotas en mora */
    Tcifin.VlrCupo = STRING(rep_Creditos.Monto / 1000,"999999999999"). /* 27 - Valor o Cupo */

    IF rep_creditos.monto < rep_creditos.sdo_capital THEN
        Tcifin.VlrCupo = STRING(rep_Creditos.sdo_capital / 1000,"999999999999"). /* 27 - Valor o Cupo */
    
    /*IF DECIMAL(Tcifin.VlrCupo) > 99999 THEN
        Tcifin.VlrCupo = STRING(99999,"999999999999").*/

    IF rep_Creditos.Val_Atraso > 0 AND rep_creditos.val_atraso < 10000 THEN
        TCifin.VlrMora = STRING(10,"999999999999").
    ELSE
        Tcifin.VlrMora = STRING(ROUND(rep_Creditos.Val_Atraso / 1000,0),"999999999999"). /* 28 - Valor de mora */

    /*IF DECIMAL(TCifin.VlrMora) > 99999 THEN
        TCifin.VlrMora = STRING(99999,"999999999999").*/

    IF integer(TCifin.VlrMora) > 0 AND integer(TCifin.cuotaMora) = 0 AND fechaAux <= fec_corte THEN
        TCifin.cuotaMora = STRING(rep_creditos.plazo - rep_creditos.cuo_pagadas,"999").

    IF rep_creditos.sdo_Capital > 0 AND rep_creditos.sdo_Capital < 10000 THEN
        TCifin.vlrSaldo = STRING(10,"999999999999").
    ELSE
        Tcifin.VlrSaldo = STRING(rep_Creditos.Sdo_Capital / 1000,"999999999999"). /* 29 - Valor del saldo */

    /*IF DECIMAL(TCifin.VlrSaldo) > 99999 THEN
        TCifin.VlrSaldo = STRING(99999,"999999999999").*/

    IF rep_creditos.cuota > 0 AND rep_creditos.cuota < 10000 THEN
        TCifin.vlrCuota = STRING(10,"999999999999").
    ELSE
        Tcifin.VlrCuota = STRING(rep_Creditos.Cuota / 1000,"999999999999"). /* 30 - Valor de la cuota */

   /*IF DECIMAL(TCifin.VlrCuota) > 99999 THEN
        TCifin.VlrCuota = STRING(99999,"999999999999").*/

    IF rep_creditos.sdo_capital = 0 /*AND rep_creditos.estado = 3*/ THEN
        TCifin.vlrCuota = "000000000000".
    
    /* 31 - Valor de cargo fijo */

    /* 32 - Línea de Crédito */
    CASE rep_Creditos.Cod_credito:
        WHEN 17 OR WHEN 27 OR WHEN 57 OR WHEN 158 THEN DO:
            IF rep_creditos.FOR_pago = 2 THEN Tcifin.LineaCre = "074".
            ELSE Tcifin.LineaCre = "003".
        END.

        WHEN 22 THEN DO:
            IF rep_creditos.FOR_pago = 2 THEN Tcifin.LineaCre = "078".
            ELSE Tcifin.LineaCre = "011".
        END.

        WHEN 32 OR WHEN 186 THEN DO:
            IF rep_creditos.FOR_pago = 2 THEN Tcifin.LineaCre = "077".
            ELSE Tcifin.LineaCre = "009".
        END.

        WHEN 62 THEN DO:
            IF rep_creditos.FOR_pago = 2 THEN Tcifin.LineaCre = "084".
            ELSE Tcifin.LineaCre = "002".
        END.

        WHEN 108 THEN Tcifin.LineaCre = "051".
        WHEN 113 OR WHEN 114 THEN Tcifin.LineaCre = "003".
        WHEN 123 THEN Tcifin.LineaCre = "001".
    END CASE.

    /* 33 - Cláusula de permanencia */

    Tcifin.TipoContr = "001". /* 34 - Tipo de contrato - Tabla 25 - 1:Credito 2:Fiducia  3:Leasing  4:Otros */

    /* 35 - Estado del contrato */
    IF rep_creditos.estado = 2 THEN
        Tcifin.EstContr = "001". /* Vigente */
    ELSE
        Tcifin.EstContr = "002". /* No Vigente */

    /* 36 - Término o vigencia del contrato */

    /* 37 - Número de meses del contrato */

    /* 38 - Naturaleza jurídica */
    IF INDEX(Clientes.Nombre + Clientes.Apellido1 + Clientes.Apellido2, "COOP") GT 0 THEN
        Tcifin.NatJurid = "008".
    ELSE DO:
        CASE Clientes.Tipo_Cliente:
            WHEN 1 THEN Tcifin.NatJurid = "000".
            WHEN 2 THEN Tcifin.NatJurid = "005".
        END CASE.
    END.

    /* 39 - Modalidad de crédito */
    CASE rep_Creditos.Tip_credito:
        WHEN 1 THEN /* Consumo   */ Tcifin.ModCredit = "02".
        WHEN 2 THEN /* Comercial */ Tcifin.ModCredit = "01".
        WHEN 3 THEN /* Vivienda  */ Tcifin.ModCredit = "03".
        WHEN 4 THEN /* MicroCred */ Tcifin.ModCredit = "04".
        OTHERWISE   /* Otros     */ Tcifin.ModCredit = "05". 
    END CASE.

    /* 40 - Tipo de moneda */
    Tcifin.TipoMoned = "01". /* Tabla 5   1:legal   2:Extranjeta */

    /* 41 - Tipo de garantía */
    valGarantia = 0.

    FOR EACH Garantias WHERE Garantias.Cod_Credito EQ rep_Creditos.Cod_Credito
                         AND Garantias.Tip_Credito EQ rep_Creditos.Tip_Credito
                         AND Garantias.Num_Solicitud EQ rep_Creditos.Num_Solicitud
                         AND Garantias.Num_Credito EQ rep_Creditos.Num_Credito
                         AND Garantias.Estado EQ 1 NO-LOCK BREAK BY  Garantias.Tipo_Garantia:
        IF FIRST-OF(Garantias.Tipo_Garantia) THEN DO:
            CASE Garantias.Tipo_Garantia:
                WHEN 1 THEN Tcifin.TipoGaran = "03". /* propiedad- Hipotecas */
                WHEN 2 THEN Tcifin.TipoGaran = "04". /* vehiculo -Pignoracion*/
                OTHERWISE Tcifin.TipoGaran = "11".   /* Otras Garantias  */
            END CASE.
        END.

        valGarantia = valGarantia + Garantias.Val_Bien
               .
    END.

    IF valGarantia = 0 THEN DO:
        FIND FIRST Relaciones WHERE Relaciones.Nit EQ rep_Creditos.Nit
                                AND INTEG(Relaciones.Cuenta) EQ rep_Creditos.Num_Credito
                                AND Relaciones.Clase_Producto EQ 2
                                AND Relaciones.Cod_Producto EQ rep_Creditos.Cod_Credito
                                AND Relaciones.Cod_Relacion EQ 11
                                AND Relaciones.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Relaciones THEN
            Tcifin.TipoGaran = "14". /*codeudores*/
    END.

    IF Tcifin.TipoGaran EQ "" THEN
        Tcifin.TipoGaran = "01".

    /* 42 - Valor de la garantía */
    Tcifin.VlrGaran = STRING(valGarantia / 1000,"999999999999").

    /* 43 - Obligación reestructurada */
    /* 44 - Naturaleza de la reestructuración */
    /* 45 - Número de reestructuraciones */
    IF rep_creditos.reestructurado = 1 AND rep_creditos.fec_reestructurado <> ? THEN DO:
        Tcifin.ObligRee = "01".
        Tcifin.NatReest = "06".
        Tcifin.NroReest = "01".
    END.
    ELSE DO:
        Tcifin.ObligRee = "02".
        Tcifin.NatReest = "  ".
        Tcifin.NroReest = "  ".
    END.

    /* 46 - Clase de tarjeta */

    /* 47 - Número de cheques devueltos */

    /* 48 - Categoría de servicios */

    /* 49 - Plazo */

    /* 50 - Días de cartera */

    /* 51 - Tipo de cuenta */

    /* 52 - Cupo sobregiro */

    /* 53 - Días autorizados */

    /* 54 - Dirección de la casa del titular */
    Tcifin.DirCasaTi = SUBSTRING(Clientes.Dir_Residencia,1,60).

    /* 55 - Teléfono de la casa del titular */
    Tcifin.TelTitul = Clientes.Tel_Residencia.
    
    /* 56 - Código ciudad casa */
    Tabla33(Clientes.Lugar_Residencia).

    ASSIGN Tcifin.CodCiuCas = W-CodCiu
           Tcifin.CiudadCas = W-NomCiu
           Tcifin.CodDeptoC = W-CodDep
           Tcifin.DepCasaTi = W-NomDep.

    Empresa().

    /*IF rep_creditos.cod_credito = 123 THEN
        TCifin.estado = "01".*/

    RUN Cifin_Codeudores.
END.

OUTPUT TO VALUE(W_Pathspl + "\" + "CIFIN_Inconsistencias_" + STRING(MONTH(Fec_Corte),"99") + STRING(YEAR(Fec_Corte),"9999") + ".TXT") NO-CONVERT.
    FOR EACH Tincidencias:
        EXPORT DELIMITER ";" Tincidencias.
    END.
OUTPUT CLOSE.

OUTPUT TO VALUE(W_Pathspl + "\" + "CIFIN" + STRING(MONTH(Fec_Corte),"99") + STRING(YEAR(Fec_Corte),"9999") + ".TXT") NO-CONVERT.
    PUT RegT1 SKIP.

    FOR EACH Tcifin:
        PUT Tcifin.TipReg
            Tcifin.TipIde
            Tcifin.NroIde
            Tcifin.Ape1 FORMAT "X(15)"
            Tcifin.Ape2 FORMAT "X(15)"
            Tcifin.Nom1 FORMAT "X(15)"
            Tcifin.Nom2 FORMAT "X(15)"
            Tcifin.Reserv1
            Tcifin.NroObli
            Tcifin.CodSuc
            Tcifin.calidad
            Tcifin.Califica
            Tcifin.EstadoTit
            Tcifin.Estado
            Tcifin.EdadMora
            Tcifin.AnosMora
            Tcifin.FecCorte
            Tcifin.FecIExp
            Tcifin.FecTerm
            Tcifin.FecExig
            Tcifin.FecPresc
            Tcifin.FecPago
            Tcifin.ModoExtin
            Tcifin.TipoPago
            Tcifin.PerPago
            Tcifin.ProbNoPag
            Tcifin.NroCuoPag
            Tcifin.NroCuoPac
            Tcifin.CuotaMora
            Tcifin.VlrCupo
            Tcifin.VlrMora
            Tcifin.VlrSaldo
            Tcifin.VlrCuota
            Tcifin.VlrCargoF
            Tcifin.LineaCre
            Tcifin.ClauPerm
            Tcifin.TipoContr
            Tcifin.EstContr
            Tcifin.TerContr
            Tcifin.MesesCont
            Tcifin.NatJurid
            Tcifin.ModCredit
            Tcifin.TipoMoned
            Tcifin.TipoGaran
            Tcifin.VlrGaran
            Tcifin.ObligRee
            Tcifin.NatReestr
            Tcifin.NroReest
            Tcifin.ClaseTar
            Tcifin.NroCheDev
            Tcifin.CatServic
            Tcifin.Plazo
            Tcifin.DiasCarte
            Tcifin.TipoCuent
            Tcifin.CupoSobre
            Tcifin.DiasAutor
            Tcifin.DirCasaTi
            Tcifin.TelTitul
            Tcifin.CodCiuCas
            Tcifin.CiudadCas
            Tcifin.CodDeptoC
            Tcifin.DepCasaTi
            Tcifin.NomEmpre
            Tcifin.DirEmpre
            Tcifin.TelEmpre
            Tcifin.CodCiuEmp
            Tcifin.CiudadEmp
            Tcifin.CodDepEmp
            Tcifin.DepEmpTit
            Tcifin.FecIniExe
            Tcifin.FecTerExe
            Tcifin.NroRenCDT
            Tcifin.CtaExGMF
            Tcifin.TipoIdeOr
            Tcifin.NroIdeOr
            Tcifin.TipoEntOr
            Tcifin.CodEntOr
            Tcifin.TipoFidec
            Tcifin.NroFidec
            Tcifin.NomFidec
            Tcifin.TipoDeuda
            Tcifin.TipoPoliz
            Tcifin.CodRamo SKIP.

        zreg = zreg + 1.
    END.

    RegT2 = "9" + STRING( zreg + 2,"99999999") + STRING( zreg,"99999999") + "00000000" + "00000000".

    PUT RegT2.
OUTPUT CLOSE.

OUTPUT TO VALUE(W_Pathspl + "\" + "CIFIN_Auditar_" + STRING(MONTH(Fec_Corte),"99") + 
                STRING(YEAR(Fec_Corte),"9999") + ".csv") NO-CONVERT.

PUT "TipIde;NroIde;Ape1 Ape2 Nom1 Nom2;Reserv1;NroObli;CodSuc;calidad;Califica;EstadoTit;Estado;EdadMora;AnosMora;FecCorte;FecIExp;FecTerm;FecExig;FecPresc;
    FecPago;ModoExtin;TipoPago;PerPago;ProbNoPag;NroCuoPag;NroCuoPac;CuotaMora;VlrCupo;VlrMora;VlrSaldo;VlrCuota;VlrCargoF;LineaCre;ClauPerm;TipoContr;
    EstContr;TerContr;MesesCont;NatJurid;ModCredit;TipoMoned;TipoGaran;VlrGaran;ObligRee;NatReestr;NroReest;ClaseTar;NroCheDev;CatServic;Plazo;DiasCarte;
    TipoCuent;CupoSobre;DiasAutor;DirCasaTi;TelTitul;CodCiuCas;CiudadCas;CodDeptoC;DepCasaTi;NomEmpre;DirEmpre;TelEmpre;CodCiuEmp;CiudadEmp;CodDepEmp;
    DepEmpTit;FecIniExe;FecTerExe;NroRenCDT;CtaExGMF;TipoIdeOr;NroIdeOr;TipoEntOr;CodEntOr;TipoFidec;NroFidec;NomFidec;TipoDeuda;TipoPoliz;CodRamo" SKIP.

    /* Tcifin.TipReg    ";"     se elimina tipo registro */

PUT RegT1 SKIP.
FOR EACH Tcifin:
    PUT Tcifin.TipIde     ";"  Tcifin.NroIde    ";"  
        Tcifin.Ape1              FORMAT "X(15)"         Tcifin.Ape2      FORMAT "X(15)"     
        Tcifin.Nom1              FORMAT "X(15)"         Tcifin.Nom2      FORMAT "X(15)"        ";"
        Tcifin.Reserv1   ";"     Tcifin.NroObli    ";"  Tcifin.CodSuc     ";" Tcifin.calidad   ";"
        Tcifin.Califica  ";"     
        Tcifin.EstadoTit ";"     Tcifin.Estado     ";"  Tcifin.EdadMora   ";" Tcifin.AnosMora  ";"
        Tcifin.FecCorte  ";"     Tcifin.FecIExp    ";"  Tcifin.FecTerm    ";" Tcifin.FecExig   ";"
        Tcifin.FecPresc  ";"     Tcifin.FecPago    ";"  Tcifin.ModoExtin  ";" Tcifin.TipoPago  ";"
        Tcifin.PerPago   ";"     Tcifin.ProbNoPag  ";"  Tcifin.NroCuoPag  ";" Tcifin.NroCuoPac ";"
        Tcifin.CuotaMora ";"     Tcifin.VlrCupo    ";"  Tcifin.VlrMora    ";" Tcifin.VlrSaldo  ";"
        Tcifin.VlrCuota  ";"     Tcifin.VlrCargoF  ";"  Tcifin.LineaCre   ";" Tcifin.ClauPerm  ";"
        Tcifin.TipoContr ";"     Tcifin.EstContr   ";"  Tcifin.TerContr   ";" Tcifin.MesesCont ";"
        Tcifin.NatJurid  ";"     Tcifin.ModCredit  ";"  Tcifin.TipoMoned  ";" Tcifin.TipoGaran ";"
        Tcifin.VlrGaran  ";"     Tcifin.ObligRee   ";"  Tcifin.NatReestr  ";" Tcifin.NroReest  ";"
        Tcifin.ClaseTar  ";"     Tcifin.NroCheDev  ";"  Tcifin.CatServic  ";" Tcifin.Plazo     ";"
        Tcifin.DiasCarte ";"     Tcifin.TipoCuent  ";"  Tcifin.CupoSobre  ";" Tcifin.DiasAutor ";"
        Tcifin.DirCasaTi ";"     Tcifin.TelTitul   ";"  Tcifin.CodCiuCas  ";" Tcifin.CiudadCas ";"
        Tcifin.CodDeptoC ";"     Tcifin.DepCasaTi  ";"  Tcifin.NomEmpre   ";" Tcifin.DirEmpre  ";"
        Tcifin.TelEmpre  ";"     Tcifin.CodCiuEmp  ";"  Tcifin.CiudadEmp  ";" Tcifin.CodDepEmp ";"
        Tcifin.DepEmpTit ";"     Tcifin.FecIniExe  ";"  Tcifin.FecTerExe  ";" Tcifin.NroRenCDT ";"
        Tcifin.CtaExGMF  ";"     Tcifin.TipoIdeOr  ";"  Tcifin.NroIdeOr   ";" Tcifin.TipoEntOr ";"
        Tcifin.CodEntOr  ";"     Tcifin.TipoFidec  ";"  Tcifin.NroFidec   ";" Tcifin.NomFidec  ";"
        Tcifin.TipoDeuda ";"     Tcifin.TipoPoliz  ";"  Tcifin.CodRamo SKIP.
        zreg = zreg + 1.
END.
ASSIGN RegT2 = "9" + STRING( zreg + 2,"99999999") + STRING( zreg,"99999999") + "00000000" + "00000000".
PUT RegT2.
OUTPUT CLOSE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ciudades wWin 
PROCEDURE Ciudades :
DEFINE INPUT   PARAMETER Ciudad LIKE Clientes.Lugar_Residencia.
 DEFINE OUTPUT  PARAMETER CiuBak AS CHARACTER FORMAT "X(15)".
 
 IF SUBSTRING(ciudad,1,5) NE "05001" THEN DO:
   FIND FIRST Ubicacion WHERE SUBSTRING(Ubicacion.Ubicacion,1,5) = SUBSTRING(ciudad,1,5) NO-LOCK NO-ERROR.
   IF AVAILABLE(Ubicacion) THEN CiuBak = Ubicacion.Nombre.
   ELSE CiuBak = "               ".
   IF LENGTH(CiuBak) LT 15 THEN DO:
     Z = 15 - LENGTH(CiuBak).
     Ciubak = CiuBak + SUBSTRING(C,1,Z).
   END.
 END.
 ELSE
   Ciubak = "Medellin       ".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CodeudoresDTCR wWin 
PROCEDURE CodeudoresDTCR :
DEFINE VAR NCodeudor AS INTEGER FORMAT "9".
DEFINE VAR WCli_Nit LIKE Clientes.Nit.
DEFINE VAR Id_Guion AS LOGICAL INITIAL NO.
DEFINE VAR NitAux LIKE Clientes.Nit.
EMPTY TEMP-TABLE TmpCod.
FOR EACH Relaciones WHERE
         Relaciones.Cod_relacion   EQ 11 AND
         Relaciones.Clase_Producto EQ 2 AND
         Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
         Relaciones.Cuenta         EQ STRING(Creditos.Num_Credito) AND
         Relaciones.Nit            EQ Creditos.Nit /* AND
         Relaciones.Estado         EQ 1 */ NO-LOCK:
    FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       IF Clientes.Tipo_Identificacion EQ "R.C" OR
          Clientes.Tipo_Identificacion EQ "T.I" THEN NEXT.
       CREATE TmpCod.
       BUFFER-COPY TmpDat TO TmpCod.
       NCodeudor = NCodeudor + 1.
       ASSIGN TmpCod.INomCli = trim(Clientes.Apellido1) + " " + trim(Clientes.Apellido2) + " " + trim(Clientes.Nombre)
              TmpCod.ITelLab = SUBSTR(TRIM(Clientes.Tel_Comercial),1,10) NO-ERROR. 
           /*DECIMAL(Clientes.Tel_Comercial) NO-ERROR.*/
       IF ERROR-STATUS:ERROR THEN TmpCod.ITelLab = "".
       RUN Ciudades(INPUT Clientes.Lugar_Comercial, OUTPUT TmpCod.ICiuLab).
       CASE Clientes.Tipo_Identificacion:
         WHEN "C.C" THEN TmpCod.ITipIde = 1.
         WHEN "C.E" THEN TmpCod.ITipIde = 4.
         WHEN "NIT" THEN TmpCod.ITipIde = 2.
         WHEN "R.C" THEN TmpCod.ITipIde = 9.
       END CASE.
       IF TmpCod.ITipIde EQ 0 THEN TmpCod.ITipIde = 1.
       /* RUN Tipo_Identificacion. */
       RUN Ciudades(INPUT Clientes.Lugar_Residencia, OUTPUT TmpCod.ICiuRes).
       ASSIGN TmpCod.IFecNac = STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99")
              TmpCod.IDirRes = SUBSTRING(Clientes.Dir_Residencia,1,30)
              TmpCod.IDirCor = SUBSTRING(Clientes.Dir_Residencia,1,30)
              TmpCod.ICiuCor = TmpCod.ICiuRes
              TmpCod.ITelRes = STRING(Clientes.Tel_Residencia)
              TmpCod.ICiiu   = DECIMAL(Clientes.Codigo_CIIU)
              TmpCod.IEstrat = Clientes.Estrato.
       IF TmpDat.IEstrat EQ 0 THEN
          TmpDat.IEstrat = 3. /* La generalidad en la empresa */

       IF LENGTH(TmpCod.ITelRes) LT 10 THEN
          ASSIGN Z = 10 - LENGTH(TmpCod.ITelRes)
                 TmpCod.ITelRes = SUBSTRING(C,1,Z) + TmpCod.ITelRes
                 TmpCod.ITelRes = REPLACE(TmpCod.ITelRes," ","0").
       ELSE
                 TmpCod.ITelRes = SUBSTR(TmpCod.ITelRes,1,10).

       IF LENGTH(TmpCod.IDirRes) LT 30 THEN
          ASSIGN Z = 30 - LENGTH(TmpCod.IDirRes)
                 TmpCod.IDirRes = TmpCod.IDirRes + SUBSTRING(C,1,Z).

       IF LENGTH(TmpCod.IDirCor) LT 30 THEN
          ASSIGN Z = 30 - LENGTH(TmpCod.IDirCor)
                 TmpCod.IDirCor = TmpCod.IDirCor + SUBSTRING(C,1,Z).

       IF TmpCod.IFecNac = ? THEN
           TmpCod.IFecNac = "      ". 
       /*   TmpCod.IFecNac = STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99").*/
       
        CASE Creditos.Agencia:
          WHEN  1 THEN TmpDat.ICiuRad = "Medellin       ".
          WHEN  2 THEN TmpDat.ICiuRad = "Medellin       ".
          WHEN  3 THEN TmpDat.ICiuRad = "Sabaneta       ".
          WHEN  4 THEN TmpDat.ICiuRad = "Caldas         ".
          WHEN  5 THEN TmpDat.ICiuRad = "Envigado       ".
          WHEN  6 THEN TmpDat.ICiuRad = "Itagui         ".
          WHEN  7 THEN TmpDat.ICiuRad = "Medellin       ".
          WHEN  8 THEN TmpDat.ICiuRad = "Rionegro       ".
          WHEN  9 THEN TmpDat.ICiuRad = "Medellin       ".
          WHEN 10 THEN TmpDat.ICiuRad = "Medellin       ".
        END CASE.

       FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.

       ASSIGN  WCli_Nit = Creditos.Nit
               TmpCod.INumIde = "00000000000"
               NitAux = "".

       DO i = 1 TO 12 BY 1:
          IF SUBSTRING(Creditos.Nit,i,1) NE "-" AND NOT Id_Guion THEN
             NitAux = NitAux + SUBSTRING(Creditos.Nit,i,1).
          ELSE Id_Guion = YES.
       END.

/*       TmpDat.INumCta = "0" + SUBSTRING(STRING(creditos.agencia),1,1).*/
       TmpDat.INumCta = STRING(Creditos.pagare,"999999") + "             ". 
       
       /*ASSIGN TmpCod.INumCta = STRING(Creditos.pagare,"999999") + "0" + SUBSTRING(STRING(creditos.agencia),1,1)  + "C" + STRING(NCodeudor)*/
       ASSIGN TmpCod.INumCta = STRING(Creditos.pagare,"999999") + "          "  + "C" + STRING(NCodeudor)
              Id_Guion = NO  
              NitAux = "".
       TmpCod.INumCta = TmpCod.INumCta + "00000000000000". 

       DO i = 1 TO 12 BY 1:
          IF SUBSTRING(Relaciones.Nit_Relacion,i,1) NE "-" AND NOT Id_Guion THEN
             NitAux = NitAux + SUBSTRING(Relaciones.Nit_Relacion,i,1).
          ELSE
             Id_Guion = YES.
       END.
       ASSIGN Id_Guion = NO
              TmpCod.INumIde = SUBSTRING(TmpCod.INumIde,1,11 - LENGTH(NitAux)) + NitAux.


       IF (MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte))  AND 
          (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte))    AND sdo_capital LE 0 THEN 
            ASSIGN TmpDat.INoveda = 05 TmpDat.IEdaMor = 1 TmpDat.ICalifi = "A".

       IF Creditos.Abogado THEN   DO:
          ASSIGN TmpDat.IAdjeti = 11.
          IF creditos.sdo_capital EQ 0 THEN 
             ASSIGN TmpDat.INoveda = 14 TmpDat.IEdaMor = 1 TmpDat.ICalifi = "A" TmpDat.ISdoMor = 0.
       END.

       IF Creditos.FOR_pago = 2       THEN 
          IF sdo_capital = 0 THEN
             ASSIGN TmpDat.IAdjeti = 00
                    TmpDat.ICuoMor = 0
                    TmpDat.Inoveda = 05
                    TmpDat.IEdaMor = 1
                    TmpDat.ICalifi = "A".
          ELSE
              ASSIGN TmpDat.IAdjeti = 00
                     TmpDat.ICuoMor = 0
                     TmpDat.Inoveda = 01
                     TmpDat.IEdaMor = 1
                     TmpDat.ICalifi = "A".

       IF Clientes.Fec_Fallecido NE ? THEN TmpDat.IAdjeti = 16.

       IF creditos.FOR_pago = 2 THEN /* Libranza */
       DO:
          IF Creditos.Sdo_Capital EQ 0 THEN
            IF Creditos.reestructurado = 1 THEN
               TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
            ELSE
               TmpDat.IForPag   = 1. /* Voluntario */
          ELSE
            TmpDat.IForPag   = 0. /* Vigente */
       END.
       ELSE
         IF Creditos.Sdo_Capital EQ 0 THEN DO:
           IF  Creditos.Abogado THEN
             TmpDat.IForPag = 2. /* Proceso Ejecutivo */
           ELSE
               IF Creditos.reestructurado = 1 THEN
                  TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
               ELSE
                  TmpDat.IForPag = 1. /* Vigente */
         END.

       IF TmpDat.Inoveda = 05 OR TmpDat.Inoveda = 01 OR TmpDat.Inoveda = 14 THEN /* 9 sept-2005 jjmp */
          TmpDat.ICuoMor = 000.
       
       IF TmpDat.Inoveda = 14 THEN ASSIGN TmpDat.IAdjeti = 00.   /* 9 sept-2005 jjmp */

        IF TmpCod.IFecApe = 0 THEN
           TmpCod.IFecApe = DECIMAL(STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99")).
        ASSIGN TmpCod.IRespon = 01.
               

        /* Nuevo control para castigados 17 de enero de 2006 */
        IF creditos.estado EQ 5 THEN DO:
           ASSIGN TmpDat.INoveda = 13
                  TmpDat.IAdjeti = 11
                  TmpDat.ICalifi = "E"
                  TmpDat.IFecPag = 000000
                  TmpDat.IForPag = 2 /*  Proceso ejecutivo */
                  TmpDat.IEdaMor = 360.
           FIND LAST planpagos WHERE planpagos.Nit         EQ creditos.nit         AND 
                                     planpagos.Num_Credito EQ creditos.num_credito AND
                                     Id_PdoMes             EQ 2                    AND 
                                     Nro_Cuota             GT 0 NO-LOCK NO-ERROR.
           IF AVAILABLE(planpagos) THEN
               ASSIGN TmpDat.ICuoCan = planpagos.cuo_pagas
                      TmpDat.ICuoMor = Creditos.plazo - planpagos.cuo_pagas
                      TmpDat.IsdoCre = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum
                      TmpDat.ISdoMor = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum.
           ELSE
           DO:
               ASSIGN TmpDat.ICuoCan = Creditos.plazo - creditos.cuo_atraso
                      TmpDat.ICuoMor = Creditos.cuo_atraso.
               IF MONTH(Creditos.Fec_CanceTotal) NE 1 THEN
                  ASSIGN TmpDat.IsdoCre = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1]
                         TmpDat.ISdoMor = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1].
               ELSE
                  ASSIGN TmpDat.IsdoCre = creditos.sdo_anuales[12]
                         TmpDat.IsdoMor = creditos.sdo_anuales[12].

           END.
        END.
        ASSIGN Prc = Prc + 1
               Ced = Creditos.Nit.
        DISPLAY Prc Ced WITH FRAME FRM0.
        /* Termina  control para castigados al 17 de enero de  2006*/
    END.
END.
FOR EACH TmpCod:
    NumReg = NumReg + 1.
    CREATE TmpDat.
    BUFFER-COPY TmpCod TO TmpDat.
    ASSIGN TmpDat.Registro = NumReg.
END.
FOR EACH TmpCod: DELETE TmpCod. END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CodeudoresDTCRNuevo wWin 
PROCEDURE CodeudoresDTCRNuevo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ttele AS CHARACTER   NO-UNDO.
DEFINE VARIABLE NCodeudor AS INTEGER     NO-UNDO.
ASSIGN NCodeudor = 0.
EMPTY TEMP-TABLE TempDatNewCodeudor.
FOR EACH Relaciones WHERE
         Relaciones.Cod_relacion   EQ 11                            AND
         Relaciones.Clase_Producto EQ 2                             AND
         Relaciones.Cod_Producto   EQ Creditos.Cod_Credito          AND
         Relaciones.Cuenta         EQ STRING(Creditos.Num_Credito)  AND
         Relaciones.Nit            EQ Creditos.Nit                  AND
         Relaciones.Nit            NE ""                            AND
         Relaciones.Nit_Relacion   NE Creditos.Nit                  AND
         Relaciones.estado         EQ 1
         /* AND
         Relaciones.Estado         EQ 1 */ NO-LOCK:
    FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion AND clientes.nit NE "" NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
        CREATE TempDatNewCodeudor.
        BUFFER-COPY TempDatNew TO TempDatNewCodeudor.

        /**********************************************/
        CASE Clientes.Tipo_Identificacion:
          WHEN "C.C" THEN TempDatNewCodeudor.tTipoId = 1.
          WHEN "C.E" THEN TempDatNewCodeudor.tTipoId = 4.
          WHEN "NIT" THEN TempDatNewCodeudor.tTipoId = 2.
          WHEN "R.C" THEN TempDatNewCodeudor.tTipoId = 1.
        END CASE.
        ASSIGN TempDatNewCodeudor.tNumId        = DECIMAL(clientes.nit)
               TempDatNewCodeudor.tNombre       = TRIM(Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre).
        ASSIGN TempDatNewCodeudor.tRespCalidad = 01.


        IF (clientes.lugar_residencia = "" OR clientes.lugar_residencia EQ "0") THEN
            ASSIGN TempDatNewCodeudor.tCiudadResidencia = "Medellin"
                   TempDatNewCodeudor.tCodigoDaneCiuRes = 05001
                   TempDatNewCodeudor.tDepartamentoRes = "Antioquia".
        ELSE DO:
            FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_residencia NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNewCodeudor.tCiudadResidencia = ubicacion.nombre
                       TempDatNewCodeudor.tCodigoDaneCiuRes = INTEGER(SUBSTRING(ubicacion.ubicacion, 1, 5)).
            END.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion, 1, 2) EQ SUBSTRING(clientes.lugar_residencia, 1, 2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNewCodeudor.tDepartamentoRes = ubicacion.nombre.
            END.
            ELSE ASSIGN TempDatNewCodeudor.tDepartamentoRes = "Antioquia".
        END.
        IF clientes.DIR_residencia NE "" THEN ASSIGN TempDatNewCodeudor.tDireccionRes = clientes.DIR_residencia.
        ELSE IF clientes.dir_comercial NE "" THEN ASSIGN TempDatNewCodeudor.tDireccionRes = clientes.DIR_comercial.
        ttele = "".
        ttele = string(DECIMAL(Clientes.Tel_Residencia)) NO-ERROR.
        IF ttele EQ "" OR ttele EQ " " THEN DO:
            REPEAT i = 1 TO 12:
                IF SUBSTRING(Clientes.Tel_Residencia, i, 1) LT "0" THEN
                    LEAVE.
                ELSE
                    ttele = ttele + SUBSTRING(Clientes.Tel_Residencia, i, 1).
            END.
        END.
        IF DECIMAL(ttele) LT 0 THEN ASSIGN ttele = STRING(ABS(DECIMAL(ttele))).
        ASSIGN TempDatNewCodeudor.tTelefonoRes = ttele.


        /* ************************** */

        IF (clientes.lugar_comercial = "" OR clientes.lugar_comercial EQ "0") THEN
            ASSIGN TempDatNewCodeudor.tCiudadLaboral    = "Medellin"
                   TempDatNewCodeudor.tCodigoDaneCiuLab = 05001
                   TempDatNewCodeudor.tDepartamentoLab  = "Antioquia".
        ELSE DO:
            FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_comercial NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNewCodeudor.tCiudadLaboral    = ubicacion.nombre
                       TempDatNewCodeudor.tCodigoDaneCiuLab = INTEGER(SUBSTRING(ubicacion.ubicacion, 1, 5)).
            END.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion, 1, 2) EQ SUBSTRING(clientes.lugar_comercial, 1, 2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNewCodeudor.tDepartamentoLab = ubicacion.nombre.
            END.
            ELSE ASSIGN TempDatNewCodeudor.tDepartamentoLab = "Antioquia".
        END.
        IF clientes.DIR_comercial NE "" THEN ASSIGN TempDatNewCodeudor.tDireccionLab = clientes.DIR_comercial.
        ELSE IF clientes.dir_comercial NE "" THEN ASSIGN TempDatNewCodeudor.tDireccionLab = clientes.DIR_comercial.
        ttele = "".
        ttele = string(DECIMAL(Clientes.Tel_comercial)) NO-ERROR.
        IF ttele EQ "" OR ttele EQ " " THEN DO:
            REPEAT i = 1 TO 12:
                IF SUBSTRING(Clientes.Tel_comercial, i, 1) LT "0" THEN
                    LEAVE.
                ELSE
                    ttele = ttele + SUBSTRING(Clientes.Tel_comercial, i, 1).
            END.
        END.
        IF DECIMAL(ttele) LT 0 THEN ASSIGN ttele = STRING(ABS(DECIMAL(ttele))).
        ASSIGN TempDatNewCodeudor.tTelefonoLab = ttele.

        /*****************************/

        ASSIGN
                TempDatNewCodeudor.tCiudadCorresp       = TempDatNewCodeudor.tCiudadResidencia
                TempDatNewCodeudor.tCodigoDaneCiuCor    = TempDatNewCodeudor.tCodigoDaneCiuRes
                TempDatNewCodeudor.tDepartamentoCor     = TempDatNewCodeudor.tDepartamentoRes 
                TempDatNewCodeudor.tDireccionCor        = TempDatNewCodeudor.tDireccionRes.

        IF clientes.email NE "" THEN
            ASSIGN TempDatNewCodeudor.tCorreoE = clientes.email.
        ELSE
            ASSIGN TempDatNewCodeudor.tCorreoE = "".
        IF Clientes.Celular NE "" THEN ASSIGN TempDatNewCodeudor.tCelular = Clientes.Celular.
        ELSE ASSIGN TempDatNewCodeudor.tCelular = "000000000000".

        /**********************************************/
/*         ASSIGN NCodeudor = NCodeudor + 1.                                                                            */
/*         ASSIGN TempDatNewCodeudor.tNumCta = SUBSTRING(TempDatNewCodeudor.tNumCta, 1, 16)  + "C" + STRING(NCodeudor). */


        CREATE TempDatNew.
        BUFFER-COPY TempDatNewCodeudor TO TempDatNew.
        DELETE TempDatNewCodeudor.

    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreFgc wWin 
PROCEDURE CreFgc :
/* Genera archivo plano con informacion de creditos para FOGACOOP*/
EMPTY TEMP-TABLE TmpCFG.
 Assign Prc   = 0
        Acum  = 0
        Xacum = 0.
        
 FOR EACH Creditos WHERE Creditos.Estado         EQ 2 AND
                         Creditos.Fec_Desembolso LE Fec_Corte and
                         creditos.Sdo_Capital    GT 0 NO-LOCK:
  ASSIGN Prc = Prc + 1
         Ced = Creditos.Nit.
  DISPLAY Prc Ced WITH FRAME FRM0.
  
  FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
  CREATE TmpCFG.
  ASSIGN TmpCFG.FC_TipIde = "C"
         TmpCFG.FC_CedNit = (Creditos.Nit)
         TmpCFG.FC_SalIni = Creditos.cuota
         TmpCFG.FC_FecApe = STRING(DAY(Creditos.Fec_Desembolso),"99") + "/" +
                            STRING(MONTH(Creditos.Fec_Desembolso),"99")  + "/" +
                            STRING(YEAR(Creditos.Fec_Desembolso),"9999")
         TmpCFG.FC_Modali = 1
         TmpCFG.FC_SalDeu = Creditos.Sdo_Capital
         TmpCFG.FC_PerPag = 1
         TmpCFG.FC_SalOtr = 0
         TmpCFG.FC_Provis = 0
         TmpCFG.FC_CuoExt = 1
         TmpCFG.FC_MesExt = 1
         TmpCFG.FC_Tasa   = Creditos.Tasa * 12.
  FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
    ASSIGN TmpCFG.FC_Apell2 = Clientes.Apellido2
           TmpCFG.FC_Apell1 = Clientes.Apellido2
           TmpCFG.FC_NomNit = Clientes.Nombre.
  FIND FIRST Garantias WHERE Garantias.Num_credito EQ Creditos.Num_credito NO-LOCK NO-ERROR.
  IF AVAILABLE(Garantias) THEN TmpCFG.FC_TipGar = 1.
  ELSE TmpCFG.FC_TipGar = 2.
  CASE Pro_Creditos.Tip_Credito:
    WHEN 1 THEN
      TmpCFG.FC_TipObl = 1. /*Consumo*/
    WHEN 2 THEN
      TmpCFG.FC_TipObl = 2. /*Comercial*/
    WHEN 2 THEN
      TmpCFG.FC_TipObl = 3. /*Hipotecario*/
    OTHERWISE 
      TmpCFG.FC_TipObl = 1. 
  END CASE.
  CASE Creditos.Categoria:
   WHEN "A" THEN 
    ASSIGN TmpCFG.FC_EdaMor = 1
           TmpCFG.FC_Califi = "A".
   WHEN "A" THEN 
    ASSIGN TmpCFG.FC_EdaMor = 1
           TmpCFG.FC_Califi = "A".
   WHEN "B" THEN
    ASSIGN TmpCFG.FC_EdaMor = 31
           TmpCFG.FC_Califi = "B".
   WHEN "C" THEN 
    DO:
     TmpCFG.FC_Califi = "C".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 61.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 121.
    END.
   WHEN "D" THEN 
    DO:
     TmpCFG.FC_Califi = "D".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 91.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 181.
    END.
   WHEN "E" THEN
    DO:
     TmpCFG.FC_Califi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 361.
    END.
   WHEN "E" THEN
    DO:
     TmpCFG.FC_Califi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 361.
    END.
   WHEN "E" THEN
    DO:
     TmpCFG.FC_Califi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 361.
    END.
  END CASE. 
  CASE Creditos.Per_Pago:
   WHEN 1 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 7)).
   WHEN 2 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 10)).
   WHEN 3 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 15)).
   WHEN 4 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 30)).
   WHEN 5 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 60)).
   WHEN 6 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 90)).
   WHEN 7 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 120)).
   WHEN 8 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 180)).
   WHEN 9 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 360)).
  END CASE.
  FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 30)).
  TmpCFG.FC_FecVen = STRING(DAY(FecAux),"99") + "/" +
                     STRING(MONTH(FecAux),"99")  + "/" +
                     STRING(YEAR(FecAux),"9999").
 END.
OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "FGCCRE.TXT") NO-CONVERT.
 FOR EACH TmpCFG:
   K = K + 1.
   acum = acum + FC_SalDeu.
   DO J = 1 TO 20 BY 1:
    CASE J:
     WHEN 1 THEN Campo = STRING(FC_TipIde).
     WHEN 2 THEN Campo = STRING(FC_CedNit).
     WHEN 3 THEN Campo = STRING(FC_Apell1).
     WHEN 4 THEN Campo = STRING(FC_Apell2).
     WHEN 5 THEN Campo = STRING(FC_NomNit).
     WHEN 6 THEN Campo = STRING(FC_TipObl).
     WHEN 7 THEN Campo = STRING(FC_Califi).
     WHEN 8 THEN Campo = STRING(FC_FecApe).
     WHEN 9 THEN Campo = STRING(FC_FecVen).
     WHEN 10 THEN Campo = STRING(FC_EdaMor).
     WHEN 11 THEN Campo = STRING(FC_Tasa).
     WHEN 12 THEN Campo = STRING(FC_PerPag).
     WHEN 13 THEN Campo = STRING(FC_Modali).
     WHEN 14 THEN Campo = STRING(FC_SalDeu).
     WHEN 15 THEN Campo = STRING(FC_SalIni).
     WHEN 16 THEN Campo = STRING(FC_SalOtr).
     WHEN 17 THEN Campo = STRING(FC_TipGar).
     WHEN 18 THEN Campo = STRING(FC_Provis).
     WHEN 19 THEN Campo = STRING(FC_CuoExt).
     WHEN 20 THEN Campo = STRING(FC_MesExt).
    END CASE.
    CAMPO0 = "5," + STRING(J) + ",1," + STRING(K) + "," + Campo.
    PUT UNFORMATTED
    CAMPO0 AT 1.
   END.
END.
OUTPUT CLOSE.

OUTPUT TO VALUE("C:\INFRED\" + "TOTFGCre.TXT") NO-CONVERT.
DISPLAY  "totales Cartera " Acum.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataCredito wWin 
PROCEDURE DataCredito :
/*------------------------------------------------------------------------------
  Purpose:     Exporta el informe de datacrédito con la nueva ley de habeas data
  Parameters:  <none>
  Notes:       Creado por William Martínez
------------------------------------------------------------------------------*/
DEFINE VARIABLE Texto1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tNum AS INTEGER     NO-UNDO.
DEFINE VARIABLE tSum AS DECIMAL     NO-UNDO.
DEFINE VARIABLE tfec AS DATE        NO-UNDO.
DEFINE VARIABLE ttele AS CHAR FORMAT "x(12)"  NO-UNDO.
DEFINE VARIABLE IntCausado AS DECIMAL     NO-UNDO.
DEFINE VARIABLE tEspBlanco AS CHARACTER   NO-UNDO.

REPEAT i = 1 TO 326:
    Texto1 = Texto1 + "0".
END.

EMPTY TEMP-TABLE TempDatNew.
ASSIGN CRegCon = "".
FIND FIRST Entidad WHERE Entidad.Entidad EQ 01 NO-LOCK NO-ERROR.
ASSIGN CRegCon = "HHHHHHHHHHHHHHHHHH" + STRING(Entidad.Cod_Datacredito,"999999") + "06" + STRING(YEAR(Fec_Corte), "9999") + STRING(MONTH(fec_corte),"99") + STRING(DAY(fec_corte),"99") + "M T                N" + Texto1.
OUTPUT TO VALUE(W_Pathspl + "\" + "PTCAC" + STRING(Entidad.Cod_Datacredito,"999999") + STRING(YEAR(Fec_Corte), "9999") + STRING(MONTH(fec_corte),"99") + STRING(DAY(fec_corte),"99") + ".TXT"
                ) NO-CONVERT.

PUT UNFORMATTED CRegCon SKIP.
FOR EACH Creditos WHERE 
                          (Creditos.Fec_Desembolso LE Fec_Corte AND Creditos.Num_Credito NE 0 AND
                          /* Creditos.Sdo_Capital EQ 0 AND   Para Marzo y Abril */
                          Creditos.Estado EQ 2 OR 
                          (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte)    AND
                          MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte) ) ) OR 
                          Creditos.Estado  EQ 5 /* Castigados a partir del 17 enero 2006 */
                          NO-LOCK BREAK BY Creditos.Nit:
                          IF  YEAR(Creditos.Fec_CanceTotal) EQ  YEAR(Fec_desembolso) AND
                          MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_desembolso) AND 
                          Creditos.sdo_capital EQ 0     THEN NEXT.
    FIND FIRST Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        CREATE TempDatNew.
        CASE Clientes.Tipo_Identificacion:
          WHEN "C.C" THEN TempDatNew.tTipoId = 1.
          WHEN "C.E" THEN TempDatNew.tTipoId = 4.
          WHEN "NIT" THEN TempDatNew.tTipoId = 2.
          WHEN "R.C" THEN TempDatNew.tTipoId = 1.
        END CASE.
        ASSIGN TempDatNew.tNumId        = DECIMAL(clientes.nit)
               TempDatNew.tNumCta       = STRING(creditos.pagare)
               TempDatNew.tNombre       = TRIM(Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre)
               TempDatNew.tSituacion    = 0
               TempDatNew.tFecApertura  = STRING(YEAR(Creditos.Fec_Desembolso),"9999") + STRING(MONTH(Creditos.Fec_Desembolso),"99") + STRING(DAY(Creditos.Fec_Desembolso),"99").
        ASSIGN  tEspBlanco = "                  "
                TempDatNew.tNumCta = TempDatNew.tNumCta + tEspBlanco.
        ASSIGN TempDatNew.tNumCta = SUBSTRING(TempDatNew.tNumCta, 1, 18).
        IF creditos.sistema = 2 THEN
            TempDatNew.tFecVenc = STRING(YEAR(Creditos.Fec_paganti),"9999") + STRING(MONTH(Creditos.Fec_paganti),"99") + STRING(DAY(Creditos.Fec_paganti),"99").
        ELSE DO:
            ASSIGN TempDatNew.tFecVenc = FecTermina2().
        END.
        ASSIGN TempDatNew.tRespCalidad = 0.
        CASE creditos.cod_credito:
             WHEN 524 THEN ASSIGN TempDatNew.tTipoOblig = 1.
             WHEN 15  THEN ASSIGN TempDatNew.tTipoOblig = 3.
             OTHERWISE ASSIGN TempDatNew.tTipoOblig = 2.
        END CASE.
        ASSIGN TempDatNew.tSubsidio     = 0
                TempDatNew.tFecSubsidio  = "00000000"
                TempDatNew.tTerminoCont  = 1.
        IF creditos.estado EQ 3 THEN TempDatNew.tForPago = 1.
            ELSE IF creditos.fec_reestructurado NE ? AND creditos.reestructurado EQ 1 THEN TempDatNew.tForPago = 4.
                ELSE IF Creditos.Estado EQ 5 THEN TempDatNew.tForPago = 2.
                    ELSE DO:
                         ASSIGN TempDatNew.tForPago = 0.
                    END.
                    
        IF creditos.sistema = 2 THEN TempDatNew.tPerPago = 6.
        ELSE DO:
            CASE creditos.per_pago:
                WHEN 1 THEN ASSIGN TempDatNew.tPerPago = 9 TempDatNew.tCuotaMes = (creditos.cuota * 4).
                WHEN 2 THEN ASSIGN TempDatNew.tPerPago = 9 TempDatNew.tCuotaMes = (creditos.cuota * 3).
                WHEN 3 THEN ASSIGN TempDatNew.tPerPago = 9 TempDatNew.tCuotaMes = (creditos.cuota * 2).
                WHEN 4 THEN ASSIGN TempDatNew.tPerPago = 1 TempDatNew.tCuotaMes = (creditos.cuota).
                WHEN 5 THEN ASSIGN TempDatNew.tPerPago = 2 TempDatNew.tCuotaMes = (creditos.cuota / 2).
                WHEN 6 THEN ASSIGN TempDatNew.tPerPago = 3 TempDatNew.tCuotaMes = (creditos.cuota / 3).
                WHEN 7 THEN ASSIGN TempDatNew.tPerPago = 9 TempDatNew.tCuotaMes = (creditos.cuota / 4).
                WHEN 8 THEN ASSIGN TempDatNew.tPerPago = 4 TempDatNew.tCuotaMes = (creditos.cuota / 6).
                WHEN 9 THEN ASSIGN TempDatNew.tPerPago = 5 TempDatNew.tCuotaMes = (creditos.cuota / 12).
            END CASE.
        END.
        IF creditos.estado EQ 3 THEN DO:
            ASSIGN TempDatNew.tNovedad = 5 TempDatNew.tEstado  = 03.
        END.
        ELSE DO:
            IF creditos.estado EQ 5 THEN
                ASSIGN TempDatNew.tNovedad  = 13 TempDatNew.tEstado   = 06.
            ELSE DO:
                IF creditos.dias_atraso LT 31 OR Creditos.CategoriaMes EQ "A"  THEN ASSIGN TempDatNew.tNovedad = 01 TempDatNew.tEstado  = 01.
                ELSE IF creditos.dias_atraso GE 30  AND creditos.dias_atraso LT 60  THEN ASSIGN TempDatNew.tNovedad = 06 TempDatNew.tEstado  = 02.
                ELSE IF creditos.dias_atraso GE 60  AND creditos.dias_atraso LT 90  THEN ASSIGN TempDatNew.tNovedad = 07 TempDatNew.tEstado  = 02.
                ELSE IF creditos.dias_atraso GE 90  AND creditos.dias_atraso LT 120 THEN ASSIGN TempDatNew.tNovedad = 08 TempDatNew.tEstado  = 02.
                ELSE IF creditos.dias_atraso GE 120                                 THEN ASSIGN TempDatNew.tNovedad = 09 TempDatNew.tEstado  = 02.
            END.
        END.
        ASSIGN TempDatNew.tFecEstado = STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + STRING(DAY(Fec_Corte),"99").
        IF TempDatNew.tForPago = 4 THEN DO:
            ASSIGN  TempDatNew.tEstadoOrig      = 1
                    TempDatNew.tFecEstadoOrig   = STRING(YEAR(Creditos.Fec_reestructurado),"9999") + STRING(MONTH(Creditos.Fec_reestructurado),"99") + STRING(DAY(Creditos.Fec_reestructurado),"99").            
        END.
        ELSE DO:
            ASSIGN TempDatNew.tEstadoOrig = 0
                   TempDatNew.tFecEstadoOrig   = STRING(YEAR(Creditos.Fec_desembolso),"9999") + STRING(MONTH(Creditos.Fec_desembolso),"99") + STRING(DAY(Creditos.Fec_desembolso),"99").            
        END.
        ASSIGN  TempDatNew.tEstadoPlast     = 0
                TempDatNew.tFecEstadoPlast  = "00000000" 
                TempDatNew.tAdjetivo        = 0
                TempDatNew.tFecAdjetivo     = "00000000"
                TempDatNew.tClaseTarj       = 0
                TempDatNew.tFranquicia      = 0
                TempDatNew.tNombreMarca     = "                              "
                TempDatNew.tTipoMoneda      = 1.

        FIND FIRST Garantias WHERE
         Garantias.Num_Credito   EQ creditos.num_credito  AND
         Garantias.Tip_Credito   EQ creditos.tip_credito AND
         Garantias.Num_Solicitud EQ creditos.num_solicitud AND
         Garantias.estado        EQ 1 AND
         Garantias.Tipo_Garantia LT 4             
         NO-LOCK NO-ERROR.
        IF AVAILABLE garantias THEN DO:
            ASSIGN TempDatNew.tTipoGrantia = 1.
        END.
        ELSE DO:
            FIND FIRST Garantias WHERE
                     /*Garantias.Cod_Credito   EQ TmpCre.CodCre  AND*/
                     Garantias.Num_Credito   EQ creditos.num_credito  AND
                     Garantias.Tip_Credito   EQ creditos.tip_credito AND
                     Garantias.Num_Solicitud EQ creditos.num_solicitud AND
                     Garantias.estado        EQ 1 AND
                     Garantias.Tipo_Garantia GE 4             
                     NO-LOCK NO-ERROR.
            IF AVAILABLE garantias THEN DO:
                ASSIGN TempDatNew.tTipoGrantia = 1.            
            END.
            ELSE DO:
                ASSIGN TempDatNew.tTipoGrantia = 2.
            END.
        END.
        
        CASE creditos.CategoriaMes:
            WHEN "A" THEN ASSIGN TempDatNew.tCalificacion = " A".
            WHEN "B" THEN ASSIGN TempDatNew.tCalificacion = " B".
            WHEN "C" THEN ASSIGN TempDatNew.tCalificacion = " C".
            WHEN "D" THEN ASSIGN TempDatNew.tCalificacion = " D".
            WHEN "E" THEN ASSIGN TempDatNew.tCalificacion = " E".
            OTHERWISE     ASSIGN TempDatNew.tCalificacion = " A".
        END CASE.
        ASSIGN TempDatNew.tProbabilidadI    = 0
               TempDatNew.tEdadMora         = abs(creditos.dias_atraso)
               TempDatNew.tValorInicial     = creditos.val_desembolso
               TempDatNew.tSaldoDeuda       = creditos.sdo_capital
               TempDatNew.tValorDisponible  = 0.
        IF TempDatNew.tEdadMora GT 999 THEN TempDatNew.tEdadMora = 999.
        IF TempDatNew.tEdadMora LT 31  THEN TempDatNew.tEdadMora = 0.
        IF TempDatNew.tCalificacion EQ " A" THEN TempDatNew.tEdadMora = 0.
        ASSIGN IntCausado = 0.


        
        
        RUN HallaIntCausados.r (INPUT creditos.nit, INPUT creditos.num_credito, OUTPUT IntCausado).
        

        IF TempDatNew.tEstado EQ 1 OR TempDatNew.tEstado EQ 3 OR Creditos.CategoriaMes EQ "A" THEN ASSIGN TempDatNew.tSaldoMora = 0.
        ELSE IF TempDatNew.tEstado EQ 2 THEN ASSIGN TempDatNew.tSaldoMora = creditos.Val_Atraso + creditos.Honorarios + creditos.Costas + creditos.Polizas +
                                                                            creditos.Int_MorCobrar + creditos.Int_MoraDifCob + creditos.INT_DifCobro + creditos.int_corrientes - IntCausado.
        IF TempDatNew.tSaldoMora LT 0 THEN TempDatNew.tSaldoMora = 0.
        ASSIGN TempDatNew.tCuotas           = creditos.plazo
               TempDatNew.tCuotasCanceladas = creditos.Cuo_Pagadas.
        IF TempDatNew.tCuotasCanceladas GT creditos.plazo AND creditos.sdo_capital GT 0 THEN ASSIGN TempDatNew.tCuotasCanceladas = creditos.plazo - 1.
        IF creditos.sdo_capital EQ 0 THEN ASSIGN TempDatNew.tCuotasCanceladas = creditos.plazo.
        IF TempDatNew.tEstado EQ 2 AND TempDatNew.tSaldoMora GT 0 THEN ASSIGN TempDatNew.tCuotasMora = INTEGER(TempDatNew.tSaldoMora / creditos.cuota).
        ELSE ASSIGN TempDatNew.tCuotasMora = 0.
        ASSIGN TempDatNew.tClausulaPerm = 0
               TempDatNew.tFecClausulaPerm = "00000000".
        IF creditos.fec_pago NE ? THEN
               ASSIGN TempDatNew.tFecLimitePago = STRING(YEAR(Creditos.fec_pago),"9999") + STRING(MONTH(Creditos.Fec_pago),"99") + STRING(DAY(Creditos.Fec_pago),"99").
        ELSE DO:
            IF creditos.estado EQ 3 THEN ASSIGN TempDatNew.tFecLimitePago = "00000000".
            ELSE ASSIGN TempDatNew.tFecLimitePago = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99").
        END.
        IF Creditos.Fec_UltPago NE ? THEN ASSIGN TempDatNew.tFecUltimoPago = STRING(YEAR(Creditos.Fec_UltPago),"9999") + STRING(MONTH(Creditos.Fec_UltPago),"99") + STRING(DAY(Creditos.Fec_UltPago),"99").
        ELSE ASSIGN TempDatNew.tFecUltimoPago = "00000000".
        ASSIGN  TempDatNew.tOficinaRad        = "Principal"
                TempDatNew.tCiudadRad         = "Neiva"
                TempDatNew.tCodigoDaneRad     = 41001.

        /****************************/
        IF (clientes.lugar_residencia = "" OR clientes.lugar_residencia EQ "0") THEN
            ASSIGN TempDatNew.tCiudadResidencia = "Neiva"
                   TempDatNew.tCodigoDaneCiuRes = 41001
                   TempDatNew.tDepartamentoRes = "Huila".
        ELSE DO:
            FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_residencia NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNew.tCiudadResidencia = ubicacion.nombre
                       TempDatNew.tCodigoDaneCiuRes = INTEGER(SUBSTRING(ubicacion.ubicacion, 1, 5)).
            END.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion, 1, 2) EQ SUBSTRING(clientes.lugar_residencia, 1, 2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNew.tDepartamentoRes = ubicacion.nombre.
            END.
            ELSE ASSIGN TempDatNew.tDepartamentoRes = "Huila".
        END.
        IF clientes.DIR_residencia NE "" THEN ASSIGN TempDatNew.tDireccionRes = clientes.DIR_residencia.
        ELSE IF clientes.dir_comercial NE "" THEN ASSIGN TempDatNew.tDireccionRes = clientes.DIR_comercial.
        ttele = "".
        ttele = string(DECIMAL(Clientes.Tel_Residencia)) NO-ERROR.
        IF ttele EQ "" OR ttele EQ " " THEN DO:
            REPEAT i = 1 TO 12:
                IF SUBSTRING(Clientes.Tel_Residencia, i, 1) LT "0" THEN
                    LEAVE.
                ELSE
                    ttele = ttele + SUBSTRING(Clientes.Tel_Residencia, i, 1).
            END.
        END.
        IF DECIMAL(ttele) LT 0 THEN ASSIGN ttele = STRING(ABS(DECIMAL(ttele))).
        ASSIGN TempDatNew.tTelefonoRes = ttele.

        /* ************************** */

        IF (clientes.lugar_comercial = "" OR clientes.lugar_comercial EQ "0") THEN
            ASSIGN TempDatNew.tCiudadLaboral    = "Neiva"
                   TempDatNew.tCodigoDaneCiuLab = 41001
                   TempDatNew.tDepartamentoLab  = "Huila".
        ELSE DO:
            FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ clientes.lugar_comercial NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNew.tCiudadLaboral    = ubicacion.nombre
                       TempDatNew.tCodigoDaneCiuLab = INTEGER(SUBSTRING(ubicacion.ubicacion, 1, 5)).
            END.
            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion, 1, 2) EQ SUBSTRING(clientes.lugar_comercial, 1, 2) AND ubicacion.tipo EQ "D" NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN DO:
                ASSIGN TempDatNew.tDepartamentoLab = ubicacion.nombre.
            END.
            ELSE ASSIGN TempDatNew.tDepartamentoLab = "Antioquia".
        END.
        IF clientes.DIR_comercial NE "" THEN ASSIGN TempDatNew.tDireccionLab = clientes.DIR_comercial.
        ELSE IF clientes.dir_comercial NE "" THEN ASSIGN TempDatNew.tDireccionLab = clientes.DIR_comercial.
        ttele = "".
        ttele = string(DECIMAL(Clientes.Tel_comercial)) NO-ERROR.
        IF ttele EQ "" OR ttele EQ " " THEN DO:
            REPEAT i = 1 TO 12:
                IF SUBSTRING(Clientes.Tel_comercial, i, 1) LT "0" THEN
                    LEAVE.
                ELSE
                    ttele = ttele + SUBSTRING(Clientes.Tel_comercial, i, 1).
            END.
        END.
        IF DECIMAL(ttele) LT 0 THEN ASSIGN ttele = STRING(ABS(DECIMAL(ttele))).
        ASSIGN TempDatNew.tTelefonoLab = ttele.

        /*****************************/

        ASSIGN
                TempDatNew.tCiudadCorresp       = TempDatNew.tCiudadResidencia
                TempDatNew.tCodigoDaneCiuCor    = TempDatNew.tCodigoDaneCiuRes
                TempDatNew.tDepartamentoCor     = TempDatNew.tDepartamentoRes 
                TempDatNew.tDireccionCor        = TempDatNew.tDireccionRes.

        IF clientes.email NE "" THEN
            ASSIGN TempDatNew.tCorreoE = clientes.email.
        ELSE
            ASSIGN TempDatNew.tCorreoE = "".
        IF Clientes.Celular NE "" THEN ASSIGN TempDatNew.tCelular = replace(trim(Clientes.Celular), " ", "").
        ELSE ASSIGN TempDatNew.tCelular = "000000000000".
        ASSIGN TempDatNew.tSuscriptorDest = integer(STRING(Entidad.Cod_Datacredito,"999999"))
               TempDatNew.tEspacioBlanco  = "                                      ".
        
        RUN CodeudoresDTCRNuevo.


    END.    /* Cierra available clientes */
END.    /* cierra ciclo clientes */

RUN ExportaArchivoDataC.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DatacreditoClientes wWin 
PROCEDURE DatacreditoClientes :
DEFINE VARIABLE i AS INTEGER     NO-UNDO.
DEFINE VARIABLE Texto1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE Texto2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tNum AS INTEGER     NO-UNDO.
DEFINE VARIABLE tSum AS DECIMAL     NO-UNDO.

REPEAT i = 1 TO 72:
    Texto1 = Texto1 + " ".
END.

EMPTY TEMP-TABLE TmpDatCli.

FIND FIRST Entidad WHERE Entidad.Entidad EQ 01 NO-LOCK NO-ERROR.
CRegCon = "CCCCCCCCCCCCCCCCCC" + STRING(Entidad.Cod_Datacredito,"999999") + STRING(YEAR(Fec_Corte), "9999") + STRING(MONTH(fec_corte),"99") + STRING(DAY(fec_corte),"99") + "00890985032".
CRegCon = CRegCon + 
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "0000000".
OUTPUT TO VALUE(W_Pathspl + "\" + "CL" + STRING(Entidad.Cod_Datacredito,"999999") + "00890985032" + STRING(YEAR(Fec_Corte), "9999") + STRING(MONTH(fec_corte),"99") + STRING(DAY(fec_corte),"99") + ".TXT"
                ) NO-CONVERT.
PUT UNFORMATTED 
    CRegCon
    SKIP.

    FOR EACH clientes WHERE tipo_vinculo  EQ 1 AND clientes.nit NE "" NO-LOCK:
        CREATE TmpDatCli.
        ASSIGN  tNum = tNum + 1
                tSum = tSum + DECIMAL(clientes.nit)
                TmpDatCli.ITipIde = 0.
        CASE Clientes.Tipo_Identificacion:
          WHEN "C.C" THEN TmpDatCli.ITipIde = 1.
          WHEN "C.E" THEN TmpDatCli.ITipIde = 4.
          WHEN "NIT" THEN TmpDatCli.ITipIde = 2.
          WHEN "R.C" THEN TmpDatCli.ITipIde = 1.
        END CASE.
        IF TmpDatCli.ITipIde EQ 0 THEN TmpDatCli.ITipIde = 1.
        ASSIGN TmpDatCli.INumIde = clientes.nit
               TmpDatCli.INomCli = TRIM(trim(Clientes.Apellido1) + " " + trim(Clientes.Apellido2) + " " + trim(Clientes.Nombre)).
        IF Clientes.Fec_Expedicion EQ ? THEN
          TmpDatCli.IFecExp = "00000000". 
        ELSE
          TmpDatCli.IFecExp = STRING(YEAR(Clientes.Fec_Expedicion),"9999") + STRING(MONTH(Clientes.Fec_Expedicion),"99") + STRING(DAY(Clientes.Fec_Expedicion),"99").
        IF clientes.lugar_expedicion EQ ? OR clientes.lugar_expedicion EQ "" OR clientes.lugar_expedicion EQ "0" THEN
            TmpDatCli.iLugarExp = "00000000".
        ELSE
            TmpDatCli.iLugarExp = string(integer(substring(clientes.lugar_expedicion, 1, 5)), "99999999").
        IF Clientes.Fec_Nacimiento EQ ? THEN
          TmpDatCli.IFecNac = "00000000". 
        ELSE
          TmpDatCli.IFecNac = STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99") + STRING(DAY(Clientes.Fec_Nacimiento),"99").
        ASSIGN TmpDatCli.IActividad = 1.
        TmpDatCli.ICiiu   = 8050 /* INTEGER(Clientes.Codigo_CIIU) */ .
        IF Clientes.Codigo_CIIU EQ ? OR Clientes.Codigo_CIIU LE 0 THEN
          TmpDatCli.ICiiu = 8050.
        IF  Clientes.Estrato GT 0 THEN
          TmpDatCli.IEstrat = Clientes.Estrato.
        ELSE
          TmpDatCli.IEstrat = 3. /* Promedio */
        ASSIGN TmpDatCli.IPer_Acargo = clientes.Per_Acargo.
        IF clientes.Fec_UltActualiza EQ ? THEN
            ASSIGN TmpDatCli.IFecActAcargo = "00000000".
        ELSE
            ASSIGN TmpDatCli.IFecActAcargo = STRING(YEAR(Clientes.Fec_UltActualiza),"9999") + STRING(MONTH(Clientes.Fec_UltActualiza),"99") + STRING(DAY(Clientes.Fec_UltActualiza),"99").
        CASE clientes.est_civil:
            WHEN "Soltero"      THEN TmpDatCli.IEst_Civil = 1.
            WHEN "Casado"       THEN TmpDatCli.IEst_Civil = 2.
            WHEN "Viudo"        THEN TmpDatCli.IEst_Civil = 1.
            WHEN "Divorciado"   THEN TmpDatCli.IEst_Civil = 3.
        END CASE.
        ASSIGN TmpDatCli.IFecActEstCivil = TmpDatCli.IFecActAcargo.
        CASE clientes.niv_educativo:
            WHEN "Ninguno"          THEN TmpDatCli.INivelEstudio = 8.
            WHEN "Bachiller"        THEN TmpDatCli.INivelEstudio = 3.
            WHEN "Tecnico"          THEN TmpDatCli.INivelEstudio = 3.
            WHEN "Tecnologo"        THEN TmpDatCli.INivelEstudio = 3.
            WHEN "Profesional"      THEN TmpDatCli.INivelEstudio = 4.
            WHEN "Especializacion"  THEN TmpDatCli.INivelEstudio = 5.
            WHEN "Maestria"         THEN TmpDatCli.INivelEstudio = 6.
            WHEN "Doctorado"        THEN TmpDatCli.INivelEstudio = 7.
        END CASE.
        ASSIGN TmpDatCli.IFecActNivelEst = TmpDatCli.IFecActAcargo
               TmpDatCli.IFecActIngresos = TmpDatCli.IFecActAcargo
               TmpDatCli.IIngresos = (Clientes.Ing_Otros + Clientes.Ing_Honorarios + Clientes.Ing_financieros + Clientes.Ing_arriendos + Clientes.Salario).
        /* Preguntar a Cartera */
        IF clientes.Tipo_Cliente = 2 THEN 
            ASSIGN
            TmpDatCli.IActivos = 0
            TmpDatCli.IPasivos = 0.
        ELSE 
            ASSIGN
            TmpDatCli.IActivos = 0
            TmpDatCli.IPasivos = 0.
        ASSIGN IFecBalance = "00000000".
        FIND FIRST creditos WHERE creditos.nit EQ clientes.nit AND (creditos.FOR_pago EQ 2 OR creditos.FOR_pago EQ 4) NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            FIND FIRST empresas WHERE empresas.cod_empresa EQ 3 NO-LOCK NO-ERROR.            
            ASSIGN  TmpDatCli.INitEmpleador = decimal(empresas.nit)
                    TmpDatCli.INomEmpleador = Alias_Empresa.
        END.
        ELSE DO:
            ASSIGN  TmpDatCli.INitEmpleador = 0
                    TmpDatCli.INomEmpleador = "                                             ".
        END.
        IF Clientes.Fec_IngEmpresa NE ? THEN
            TmpDatCli.IFecIngEmpresa = STRING(YEAR(Clientes.Fec_IngEmpresa),"9999") + STRING(MONTH(Clientes.Fec_IngEmpresa),"99") + STRING(DAY(Clientes.Fec_IngEmpresa),"99").
        ELSE
            TmpDatCli.IFecIngEmpresa = "00000000".
        ASSIGN TmpDatCli.IFecActEmpleador = TmpDatCli.IFecActAcargo.
        CASE clientes.tip_contrato:
            WHEN 1 THEN TmpDatCli.ITipContrato = 2.
            WHEN 2 THEN TmpDatCli.ITipContrato = 1.
            OTHERWISE TmpDatCli.ITipContrato = 1.
        END CASE.
        ASSIGN TmpDatCli.IFecActTipCont = TmpDatCli.IFecActAcargo
               TmpDatCli.IOperacionesInt  = 0
               TmpDatCli.IFecActOperaInt  = TmpDatCli.IFecActAcargo
               TmpDatCli.ISituacionTit    = 0
               TmpDatCli.IFuerzaMayor     = 1
               TmpDatCli.IFecActFuerzaMay = TmpDatCli.IFecActAcargo
               TmpDatCli.IFiller          = Texto1.

    END.    /* Cierrra el for each clientes */

    FOR EACH TmpDatCli NO-LOCK:
        PUT UNFORMATTED
        ITipIde          FORMAT "9"            
        decimal(INumIde) FORMAT "99999999999"        
        INomCli          FORMAT "X(45)"        
        IFecExp          FORMAT "X(8)"         
        iLugarExp        FORMAT "X(8)"         
        IFecNac          FORMAT "X(8)"         
        IActividad       FORMAT "99"           
        ICiiu            FORMAT "9999"         
        IEstrat          FORMAT "99"           
        IPer_Acargo      FORMAT "999999"       
        IFecActAcargo    FORMAT "x(8)"         
        IEst_Civil       FORMAT "99"           
        IFecActEstCivil  FORMAT "x(8)"         
        INivelEstudio    FORMAT "99"           
        IFecActNivelEst  FORMAT "x(8)"         
        IIngresos        FORMAT "9999999999999"
        IFecActIngresos  FORMAT "x(8)"         
        IActivos         FORMAT "9999999999999"
        IPasivos         FORMAT "9999999999999"
        IFecBalance      FORMAT "x(8)"         
        INitEmpleador    FORMAT "99999999999"  
        INomEmpleador    FORMAT "x(45)"        
        IFecIngEmpresa   FORMAT "x(8)"         
        IFecActEmpleador FORMAT "x(8)"         
        ITipContrato     FORMAT "9"            
        IFecActTipCont   FORMAT "x(8)"         
        IOperacionesInt  FORMAT "9"            
        IFecActOperaInt  FORMAT "x(8)"         
        ISituacionTit    FORMAT "9"            
        IFuerzaMayor     FORMAT "9"            
        IFecActFuerzaMay FORMAT "x(8)"         
        IFiller          FORMAT "x(72)"        
        SKIP(0)
        .
    END.

    ASSIGN Texto1 = "".
    REPEAT i = 1 TO 298:
        Texto1 = Texto1 + " ".
    END.
    ASSIGN tNum = tNum + 2.
    ASSIGN Texto2 = "ZZZZZZZZZZZZZZZZZZ" + STRING(YEAR(Fec_Corte), "9999") + STRING(MONTH(fec_corte),"99") + STRING(DAY(fec_corte),"99") + STRING(tnum, "99999999") + STRING(tSum, "999999999999999999") + Texto1.
    PUT UNFORMATTED Texto2 SKIP(0).

END.        /* Termina el procedimiento */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DatacreditoOld wWin 
PROCEDURE DatacreditoOld :
DEFINE VAR FecAuxi AS DATE.
DEFINE VAR XMul    AS INTEGER   FORMAT "99".
DEFINE VAR XDiaAtr AS INTEGER   FORMAT "9999".
DEFINE VAR WCli_nit LIKE Clientes.Cod_Anterior .
DEFINE VAR TmpCuota LIKE Creditos.Cuota.
DEFINE VAR Id_Guion AS LOGICAL INITIAL NO.
DEFINE VAR W_TSdoVdo  LIKE creditos.sdo_capital.
DEFINE VAR W_SdoDeuda LIKE creditos.sdo_capital.
DEFINE VAR Espacios AS CHARACTER FORMAT "X(15)" INITIAL "               ".
DEFINE VARIABLE viNumReg AS INTEGER  NO-UNDO. /*Cuenta registros a reportar a central de riesgo*/
DEFINE VARIABLE viSumNov AS INTEGER  NO-UNDO. /*Suma campo novedades, para control de Datacredito*/
DEFINE VARIABLE viCntErr AS INTEGER  NO-UNDO. /*Cuenta registros con error*/                  
EMPTY TEMP-TABLE TmpDat.
ASSIGN NumReg = 0
       Prc    = 0.
       
FOR EACH Creditos WHERE 
    ( Creditos.Fec_Desembolso LE Fec_Corte AND Creditos.Num_Credito NE 0 AND
                          /* Creditos.Sdo_Capital EQ 0 AND   Para Marzo y Abril */
                          Creditos.Estado EQ 2 OR 
                          (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte)    AND
                           MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte) ) ) OR 
                           Creditos.Estado  EQ 5 /* Castigados a partir del 17 enero 2006 */
                   NO-LOCK BREAK BY Creditos.Nit:
    IF  YEAR(Creditos.Fec_CanceTotal) EQ  YEAR(Fec_desembolso) AND
       MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_desembolso) AND 
        Creditos.sdo_capital EQ 0     THEN NEXT.
  FIND FIRST Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN DO:
    CREATE TmpDat.
    NumReg = NumReg + 1.
    FIND FIRST Garantias WHERE
               Garantias.Cod_Credito EQ Creditos.Cod_Credito AND
               Garantias.Tip_Credito EQ Creditos.Tip_Credito AND
               Garantias.Num_Credito EQ Creditos.Num_Credito AND
               Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
    ASSIGN TmpDat.ITipGar = 2
           TmpDat.Registro = NumReg.
    IF AVAILABLE Garantias THEN TmpDat.ITipGar = 1.
    ASSIGN TmpDat.INomCli = trim(Clientes.Apellido1) + " " + trim(Clientes.Apellido2) + " " + trim(Clientes.Nombre)
           TmpDat.ITelLab = SUBSTR(TRIM(Clientes.Tel_Comercial),1,10) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN TmpDat.ITelLab = "".
    RUN Ciudades(INPUT Clientes.Lugar_Comercial, OUTPUT TmpDat.ICiuLab).
    RUN Tipo_Identificacion.
    RUN Ciudades(INPUT Clientes.Lugar_Residencia, OUTPUT TmpDat.ICiuRes).
    IF Clientes.Fec_Nacimiento EQ ? THEN
      TmpDat.IFecNac = "      ". 
    ELSE
      TmpDat.IFecNac = STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99").
    ASSIGN TmpDat.IDirRes = SUBSTRING(Clientes.Dir_Residencia,1,30)
           TmpDat.IDirCor = SUBSTRING(Clientes.Dir_Residencia,1,30)
           TmpDat.ICiuCor = TmpDat.ICiuRes
           TmpDat.ITelRes = STRING(Clientes.Tel_Residencia)
           TmpDat.ICiiu   = DECIMAL(Clientes.Codigo_CIIU).
    IF Clientes.Codigo_CIIU EQ ? OR Clientes.Codigo_CIIU LE 0 THEN
       TmpDat.ICiiu = 0.
    IF  Clientes.Estrato GT 0 THEN
      TmpDat.IEstrat = Clientes.Estrato.
    ELSE
      TmpDat.IEstrat = 3. /* Promedio */
    IF LENGTH(TmpDat.ITelRes) LT 10 THEN
     ASSIGN Z = 10 - LENGTH(TmpDat.ITelRes)
            TmpDat.ITelRes = SUBSTRING(C,1,Z) + TmpDat.ITelRes
            TmpDat.ITelRes = REPLACE(TmpDat.ITelRes," ","0").
    ELSE
      ASSIGN TmpDat.ITelRes = SUBSTRING(TmpDat.ITelRes,1,10).

    IF LENGTH(TmpDat.IDirRes) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)
            TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z).
    ELSE
            TmpDat.IDirRes = SUBSTR(TmpDat.IDirRes,1,30).

    IF LENGTH(TmpDat.IDirCor) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)
            TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z).
    ELSE
            TmpDat.IDirCor = SUBSTR(TmpDat.IDirCor,1,30).

    RUN Calificacion.

  END.
  ELSE DO:
    ASSIGN TmpDat.ICiuRes = "               "
           TmpDat.IFecNac = "      "
           TmpDat.IDirRes = "               "
           TmpDat.IDirCor = "               "
           TmpDat.ICiuCor = "               "
           TmpDat.ICiuRad = "               "
           TmpDat.ITelRes = "0000000000"
           TmpDat.ICiiu   = 0.
    IF LENGTH(TmpDat.IDirRes) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)
            TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z).
    IF LENGTH(TmpDat.IDirCor) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)
            TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z).
  END.
   IF Creditos.per_pago = 4 THEN 
        TmpDat.IPerPag = 1.  /* Mensual */
   ELSE TmpDat.IPerPag = 9. /*  Otras  */
 
  /*calcula la fecha de vencimiento o busca en planpagos*/
    FIND LAST PlanPagos WHERE
             PlanPagos.Nit         EQ Creditos.Nit AND 
             PlanPagos.Num_Credito EQ Creditos.Num_Credito  AND
             planpagos.id_pdomes   LE 2                     AND
             PlanPagos.Nro_Cuota   EQ Creditos.Plazo NO-LOCK NO-ERROR.
    IF NOT AVAIL(planpagos) THEN
      FOR EACH planpagos WHERE  planpagos.nit         EQ creditos.nit         AND 
                                planpagos.num_credito EQ creditos.num_credito AND 
                                planpagos.id_pdomes   LE 2  NO-LOCK BY planpagos.nro_cuota:  
          ASSIGN IFecVen = DECIMAL(STRING(YEAR(PlanPagos.Fec_Vcto)) + STRING(MONTH(PlanPagos.Fec_Vcto),"99")).
      END.
    ELSE
      ASSIGN IFecVen = DECIMAL(STRING(YEAR(PlanPagos.Fec_Vcto)) + STRING(MONTH(PlanPagos.Fec_Vcto),"99")).

   CASE Creditos.Agencia:
      WHEN  1 THEN TmpDat.ICiuRad = "Medellin       ".
      WHEN  2 THEN TmpDat.ICiuRad = "Medellin       ".
      WHEN  3 THEN TmpDat.ICiuRad = "Estrella       ".
      OTHERWISE 
            TmpDat.ICiuRad = "Medellin       ".
   END CASE.

  WCli_Nit = Creditos.Nit.
  IF LENGTH(WCli_Nit) LT 7 THEN
     WCli_Nit = "00" + WCli_Nit.
  ASSIGN
        TmpDat.ICalifi = creditos.categoria /*GIOCAM*/
        NitAux = ""
         TmpDat.INumIde = "0000000000000".
    
  DO i = 1 TO 12 BY 1:
     IF SUBSTRING(Creditos.Nit,i,1) NE "-" AND NOT Id_Guion THEN
        NitAux = NitAux + SUBSTRING(Creditos.Nit,i,1).
     ELSE Id_Guion = YES.
  END.
  Id_Guion = NO.
  TmpDat.INumCta = "0" + SUBSTRING(STRING(creditos.agencia),1,1).
  TmpDat.INumIde = SUBSTRING(TmpDat.INumIde,1,11 - LENGTH(NitAux)) + NitAux.
    
  ASSIGN 
   TmpDat.INumCta = STRING(Creditos.pagare,"99999")  + "             "
   TmpDat.IFecApe = DECIMAL(STRING(YEAR(Creditos.Fec_Desembolso),"9999") + STRING(MONTH(Creditos.Fec_Desembolso),"99"))
   TmpDat.IAdjeti = 0
   TmpDat.IValIni = Creditos.Monto
   TmpDat.ISdoCre = Creditos.Sdo_Capital
   TmpDat.IValDis = 0
   TmpDat.ITipMon = 1
   TmpDat.ITotCuo = Creditos.Plazo
   TmpDat.ICuoCan = Creditos.Cuo_Pagadas
   /*TmpDat.ISdoMor = 0*/
   TmpDat.IReclam = 0.
   /*TmpDat.ICuoMor = Creditos.Cuo_Atraso.*/

  IF (MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte))  AND 
     (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte))    AND sdo_capital LE 0 THEN 
       ASSIGN TmpDat.INoveda = 05 
        TmpDat.IEdaMor = 1 
        ICalifi = "A".
         
  IF Creditos.Abogado THEN   DO:
     ASSIGN TmpDat.IAdjeti = 11.
     IF creditos.sdo_capital EQ 0 THEN 
        ASSIGN TmpDat.INoveda = 14 
         TmpDat.IEdaMor = 1 
         TmpDat.ICalifi = "A" 
         TmpDat.ISdoMor = 0.
  END.
 
  IF Creditos.FOR_pago = 2  AND sdo_capital = 0 THEN
        ASSIGN TmpDat.IAdjeti = 00
               TmpDat.ICuoMor = 0
               TmpDat.Inoveda = 05
               TmpDat.IEdaMor = 1
               TmpDat.ICalifi = "A".

  IF Clientes.Fec_Fallecido NE ? THEN TmpDat.IAdjeti = 16.

  CASE Creditos.Per_Pago:
      WHEN 1 THEN TmpDat.ICuoMes = Creditos.Cuota * 4.
      WHEN 2 THEN TmpDat.ICuoMes = Creditos.Cuota * 3.
      WHEN 3 THEN TmpDat.ICuoMes = Creditos.Cuota * 2.
      WHEN 4 THEN TmpDat.ICuoMes = Creditos.Cuota.
      WHEN 5 THEN TmpDat.ICuoMes = Creditos.Cuota / 2.
      WHEN 6 THEN TmpDat.ICuoMes = Creditos.Cuota / 3.
      WHEN 7 THEN TmpDat.ICuoMes = Creditos.Cuota / 4.
      WHEN 8 THEN TmpDat.ICuoMes = Creditos.Cuota / 6.
      WHEN 9 THEN TmpDat.ICuoMes = Creditos.Cuota / 12.
  END CASE.

  IF TmpDat.IFecApe = 0 THEN
     TmpDat.IFecApe = DECIMAL(STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99")).
  RUN Nomagencia (INPUT Creditos.agencia, OUTPUT TmpDat.IOfiRad).
  
  IF creditos.FOR_pago = 2 THEN /* Libranza */
  DO:
     IF Creditos.Sdo_Capital EQ 0 THEN
       IF Creditos.reestructurado = 1 THEN
          TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
       ELSE
          TmpDat.IForPag   = 1. /* Voluntario */
     ELSE
       TmpDat.IForPag   = 0. /* Vigente */
  END.
  ELSE
    IF Creditos.Sdo_Capital EQ 0 THEN DO:
      IF  Creditos.Abogado THEN
        TmpDat.IForPag = 2. /* Proceso Ejecutivo */
      ELSE
          IF Creditos.reestructurado = 1 THEN
             TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
          ELSE
             TmpDat.IForPag = 1. /* Vigente */

   /*  IF creditos.reestructurado = 1 THEN  /* se inactiva por los anteriores 9 sept jjmp */
        TmpDat.IForPag = 4. /*  Reestructurado */ */

    END.
    ELSE
      TmpDat.IForPag = 0.
  
  TmpDat.IFecAct = DECIMAL(STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + STRING(DAY(Fec_Corte),"99")).

  IF Creditos.Fec_UltPago NE ? THEN
     TmpDat.IFecPag = DECIMAL(STRING(YEAR(Creditos.Fec_UltPago),"9999") +   
                              STRING(MONTH(Creditos.Fec_UltPago),"99") +    
                              STRING(DAY(Creditos.Fec_UltPago),"99")).      
  ELSE
     TmpDat.IFecPag = 0. 

  ASSIGN /*TmpDat.ISdoMor = Creditos.Val_Atraso*/
         TmpDat.IRespon = 0.

  ASSIGN W_TSdoVdo = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                     Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob
        W_SdoDeuda = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                     Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                     Creditos.Sdo_Capital    + Creditos.INT_DifCobro   - Creditos.Int_Anticipado.

  IF (Creditos.Fec_CanceTotal NE ? AND Creditos.Sdo_Capital EQ 0 ) OR 
     (MONTH(fec_desembolso) = MONTH(fec_corte) AND YEAR(fec_desembolso) = YEAR(fec_corte) ) THEN 
      TmpDat.ISdoMor = 0.

  IF INoveda = 05 THEN  /* cancelados */
     ASSIGN IEdaMor = 1 
            ICalifi = "A" 
            TmpDat.ISdoMor = 0.

  IF Inoveda = 05 OR Inoveda = 01 OR Inoveda = 14 THEN /* 9 sept-2005 jjmp */
     TmpDat.ICuoMor = 000.

  IF Inoveda = 14 THEN ASSIGN TmpDat.IAdjeti = 00.   /* 9 sept-2005 jjmp */

 /* IF Creditos.FOR_pago NE 2 THEN /* Nuevo a partir del 13 de febrero de 2006 jjmp*/
     RUN Data_ValCalif.    Coment.Feb.3/07*/

  /* Nuevo control para castigados 17 de enero de 2006 */
  IF creditos.estado EQ 5 THEN DO:
     ASSIGN TmpDat.INoveda = 13
            TmpDat.IAdjeti = 11
            TmpDat.ICalifi = "E"
            TmpDat.IFecPag = 000000
            TmpDat.IForPag = 2 /*  Proceso ejecutivo */
            TmpDat.IEdaMor = 360.
     FIND LAST planpagos WHERE planpagos.Nit         EQ creditos.nit         AND 
                               planpagos.Num_Credito EQ creditos.num_credito AND
                               Id_PdoMes             EQ 2                    AND 
                               Nro_Cuota             GT 0 NO-LOCK NO-ERROR.
     IF AVAILABLE(planpagos) THEN
         ASSIGN TmpDat.ICuoCan = planpagos.cuo_pagas
                TmpDat.ICuoMor = Creditos.plazo - planpagos.cuo_pagas
                TmpDat.IsdoCre = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum
                TmpDat.ISdoMor = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum.
     ELSE
     DO:
         ASSIGN TmpDat.ICuoCan = Creditos.plazo - creditos.cuo_atraso
                TmpDat.ICuoMor = Creditos.cuo_atraso.
         IF MONTH(Creditos.Fec_CanceTotal) NE 1 THEN
            ASSIGN TmpDat.IsdoCre = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1]
                   TmpDat.ISdoMor = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1].
         ELSE
            ASSIGN TmpDat.IsdoCre = creditos.sdo_anuales[12]
                   TmpDat.IsdoMor = creditos.sdo_anuales[12].
     END.
  END.
  /* Termina  control para castigados al 17 de enero de  2006*/

  IF tmpDat.ICuoMor LE 0 THEN
     TmpDat.IsdoMor = 0.

  IF Creditos.Cuo_Atraso EQ ? OR Creditos.Cuo_Atraso LE 0 THEN
     ASSIGN TmpDat.ICuoMor = 0
            TmpDat.IsdoMor = 0.

  ASSIGN Prc = Prc + 1
         Ced = Creditos.Nit.
  DISPLAY Prc Ced WITH FRAME FRM0.
  
  RUN CodeudoresDtCr.
END.

/*contingenica juriscoop */
 ASSIGN        TmpDat.IAdjeti = 00
               TmpDat.ICuoMor = 0
               TmpDat.IEdaMor = 1
               TmpDat.ICalifi = "A".


FIND FIRST Entidad WHERE Entidad.Entidad EQ 01 NO-LOCK NO-ERROR.
CRegCon = "000000000000000000" + STRING(Entidad.Cod_Datacredito,"999999") + "06" + STRING(YEAR(Fec_Corte)) + STRING(MONTH(fec_corte),"99") + "M".
CRegCon = CRegCon + 
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "000000000000000000000000000".
OUTPUT TO VALUE(W_Pathspl + "\" + "DATACREDITO" + STRING(MONTH(Fec_Corte),"99") +
                 STRING(YEAR(Fec_Corte),"9999")  + ".TXT") NO-CONVERT.
PUT UNFORMATTED 
    CRegCon
    SKIP.

IF TmpDat.IEstrat EQ 0 THEN
   TmpDat.IEstrat = 3. /* La generalidad en la empresa */

ASSIGN viNUmReg = 0
    viSumNov = 0.

/*gIOCAM Oct 04/07
solo genera plano para saldos mayores a cero
*/
FOR EACH TmpDat WHERE 
    TmpDat.ICuoMes GE 0 AND  
    TmpDat.IValIni GE 0 AND 
    TmpDat.ISdoCre GE 0 AND 
    TmpDat.ISdoMor GE 0 AND 
    TmpDat.IValDis GE 0 NO-LOCK:

    ASSIGN viNumReg = viNumReg + 1
            viSumNov = viSumNov + TmpDat.INoveda.    


    IF TmpDat.IFecNac EQ "" OR TmpDat.IFecNac EQ "      " THEN
        ASSIGN TmpDat.IFecNac = "000000".

    PUT UNFORMATTED
     TmpDat.INumCta    FORMAT "X(18)" 
     TmpDat.INumIde    FORMAT "X(11)"
     TmpDat.INomCli    FORMAT "X(45)"
     TmpDat.IFecNac    FORMAT "999999"
     TmpDat.IFecApe    FORMAT "999999"
     TmpDat.IFecVen    FORMAT "999999"
     TmpDat.ICuoMes    FORMAT "9999999999"
     TmpDat.INoveda    FORMAT "99"
     TmpDat.IAdjeti    FORMAT "99"
     TmpDat.ITipIde    FORMAT "9"
     TmpDat.IValIni    FORMAT "9999999999"
     TmpDat.ISdoCre    FORMAT "9999999999"
     TmpDat.ISdoMor    FORMAT "9999999999"
     TmpDat.IValDis    FORMAT "9999999999"
     TmpDat.ITipMon    FORMAT "9"
     TmpDat.ITipCre    FORMAT "9"
     TmpDat.ITipGar    FORMAT "9"
     TmpDat.ICalifi    FORMAT "X"
     TmpDat.ICiuRes    FORMAT "X(15)"
     TmpDat.IDirRes    FORMAT "X(30)"
     TmpDat.ITelRes    FORMAT "X(10)"
     TmpDat.ICiuLab    FORMAT "X(15)"
     TmpDat.ITelLab    FORMAT "X(10)"
     TmpDat.ICiuCor    FORMAT "X(15)"
     TmpDat.IDirCor    FORMAT "X(30)"
     TmpDat.ICiiu      FORMAT "999999"
     TmpDat.ITotCuo    FORMAT "999"
     TmpDat.ICuoCan    FORMAT "999"
     TmpDat.ICuoMor    FORMAT "999"
     TmpDat.IFecPag    FORMAT "99999999"
     "               "
/*     TmpDat.IOfiRad    FORMAT "X(15)" */
     TmpDat.ICiuRad    FORMAT "X(15)"
     TmpDat.IForPag    FORMAT "9"
     TmpDat.IPerPag    FORMAT "9"
     TmpDat.IEdaMor    FORMAT "999"
     TmpDat.IFecAct    FORMAT "99999999"
     TmpDat.IReclam    FORMAT "9"
     TmpDat.IRespon    FORMAT "99"
     TmpDat.IEstrat    FORMAT "9"
     TmpDat.IFil       FORMAT "X(14)"
        SKIP.
END.

/*RUN Datacredito_AdiPlano.*/ /*gIOCAM Oct/04/07*/
/* giocam
CRegFin = "ZZZZZZZZZZZZZZZZZZ" + STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99") + 
          STRING(DAY(Fec_Corte),"99") + STRING(Prc + 2,"99999999").
*/          

CRegFin = "ZZZZZZZZZZZZZZZZZZ" + STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99") + 
          STRING(DAY(Fec_Corte),"99") + STRING(viNumReg + 2,"99999999") + STRING(viSumNov,"99999999").

PUT CRegFin SKIP(0).
/* DISPLAY CRegFin NO-LABELS.*/
OUTPUT CLOSE.
/*MESSAGE "informe en: " W_Pathspl + STRING(W_agencia,"99") + "DATACR.TXT".*/


OUTPUT TO VALUE(W_Pathspl + "\" + "DATACREDITO" + STRING(MONTH(Fec_Corte),"99") +
                 STRING(YEAR(Fec_Corte),"9999")  + "error.TXT") NO-CONVERT.

ASSIGN viCntErr = 0. /*Contador de registros con error*/

FOR EACH TmpDat WHERE 
    TmpDat.ICuoMes LT 0 OR
    TmpDat.IValIni LT 0 OR
    TmpDat.ISdoCre LT 0 OR
    TmpDat.ISdoMor LT 0 OR
    TmpDat.IValDis LT 0 NO-LOCK:

    FORM 
        Tmpdat.INumCta  COLUMN-LABEL "Num. Cta."
        TmpDat.INumIde  COLUMN-LABEL "Nit"
        TmpDat.INomCli  COLUMN-LABEL "Cliente"
        TmpDat.IValIni  COLUMN-LABEL "Val. Ini."    FORMAT "-999999999"
        TmpDat.ISdoCre  COLUMN-LABEL "Saldo Cred."  FORMAT "-999999999"
        TmpDat.ISdoMor  COLUMN-LABEL "Saldo Mora"   FORMAT "-999999999"
        TmpDat.IValDis  COLUMN-LABEL "Val. Dispon"  FORMAT "-999999999"
      WITH FRAME errores DOWN COLUMN 1 WIDTH 150
      NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    DISPLAY 
        TmpDat.INumCta  
        TmpDat.INumIde
        TmpDat.INomCli
        TmpDat.IValIni
        TmpDat.ISdoCre
        TmpDat.ISdoMor
        TmpDat.IValDis
        WITH FRAME errores. 
    DOWN WITH FRAME errores.
END. /*FOR EACH TmpDat*/

OUTPUT CLOSE.

IF viCntErr NE 0 THEN DO:
    MESSAGE "SE HA GENERADO ARCHIVO DE REGISTROS CON ERROR." SKIP
    "Saldos Negativos."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Datacredito_AdiPlano wWin 
PROCEDURE Datacredito_AdiPlano :
DEFINE VAR w-treg2     AS INTEGER INITIAL 0.
DEFINE VAR w-registro2 AS CHARACTER FORMAT "X(360)".
/* DEFINE VAR w-registro2B AS CHARACTER FORMAT "X(181)".*/
  /*DEFINE TEMP-TABLE tmDatacredito
    FIELD Tregistro2 AS CHARACTER FORMAT "X(360)".*/

INPUT FROM "c:\Castig\datacast.txt".  
PUT "" SKIP(0).
REPEAT:
   CREATE TmDatacredito.
   IMPORT UNFORMATTED TmDatacredito.
   w-treg2 = w-treg2 + 1.
   /* CREATE TmpCifin.  Pendiente creacion en datacredito */
   W-registro2 = SUBSTRING(tregistro2,1,334) +  STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + 
                                            STRING(DAY(Fec_Corte),"99") + SUBSTRING(tregistro2,343,18).         
  /* w-registro2A = SUBSTRING(tregistro2A,1,180).
   w-registro2B = SUBSTRING(tregistro2B,1,154) + STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + 
                                            STRING(DAY(Fec_Corte),"99") + SUBSTRING(tregistro2B,163,18).          */
   ASSIGN NumReg = NumReg + 1
          Prc    = Prc + 1
          Ced    = Creditos.Nit.
   /* DISPLAY Prc Ced WITH FRAME FRM0. */

   PUT W-registro2 SKIP(0).
   /*PUT tregistro2A.
   PUT tregistro2B SKIP(0).*/
   /* TmpCifin.RegCif = tregistro2. Asignacion en Datacredito */
END.
MESSAGE "Cartera Castigada - Nro. de Registros tipo 2 adicionados:  " w-treg2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data_ValCalif wWin 
PROCEDURE Data_ValCalif :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR w_Rowid  AS ROWID.
DEFINE VAR Zced         LIKE clientes.nit.
DEFINE VAR W_CalCliente LIKE clientes.califica.
DEFINE VAR w_calmayor   LIKE clientes.califica.
ASSIGN w_Rowid  = rowid(creditos)
       zced     = creditos.nit.

FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
IF AVAILABLE(clientes) THEN
   ASSIGN W_CalCliente = clientes.califica.

FIND FIRST Creditos WHERE creditos.nit = zced AND creditos.estado = 2 AND creditos.FOR_pago = 2 NO-LOCK NO-ERROR.
IF AVAILABLE(creditos) THEN DO:
  FIND FIRST creditos WHERE ROWID(Creditos) EQ W_RowId NO-LOCK NO-ERROR.
  IF AVAIL(Creditos) THEN DO:
    IF W_CalCliente GT Creditos.Cod_Califica THEN
       W_CalMayor = W_CalCliente.
    ELSE
       W_CalMayor = Creditos.Cod_Califica.

    CASE W_CalMayor:
        WHEN 0 THEN
          TmpDat.ICalifi = "A".
        WHEN 1 THEN
          TmpDat.ICalifi = "A".
        WHEN 2 THEN
          TmpDat.ICalifi = "A".
        WHEN 3 THEN
          TmpDat.ICalifi = "B".
        WHEN 4 THEN
          TmpDat.ICalifi = "C".
        WHEN 5 THEN
          TmpDat.ICalifi = "D".
        OTHERWISE 
          TmpDat.ICalifi = "E".
    END.
  END.
END.
ELSE
   FIND FIRST creditos WHERE ROWID(Creditos) EQ W_RowId NO-LOCK NO-ERROR.

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
  DISPLAY t-2 T_UIAF t1 T-10 t-5 t4 T-6 T5 T-7 T6 t-8 t7 t-13 t-9 t8 t-11 t-18 
          t-12 tgSaldosDiariosDepositos t-19 t2 t3 Fec_Corte SalMin UVR m2 m1 
          Prc CED 
      WITH FRAME Frm0 IN WINDOW wWin.
  ENABLE RECT-25 RECT-26 RECT-28 RECT-29 RECT-290 RECT-291 t-2 T_UIAF t1 T-10 
         BUTTON-3 BUTTON-2 t-5 t4 T-6 T5 T-7 T6 t-8 t7 Btn_Ejecutar t-13 t-9 t8 
         t-11 t-18 t-12 tgSaldosDiariosDepositos t-19 t2 t3 Fec_Corte SalMin 
         UVR BUTTON-11 BtnDone BUTTON-5 
      WITH FRAME Frm0 IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-Frm0}
  DISPLAY Efec_noa sefena Psefe Peefec 
      WITH FRAME frm_brechas IN WINDOW wWin.
  ENABLE Efec_noa sefena Psefe Peefec BUTTON-10 
      WITH FRAME frm_brechas IN WINDOW wWin.
  VIEW FRAME frm_brechas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-frm_brechas}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportaArchivoDataC wWin 
PROCEDURE ExportaArchivoDataC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE tnumreg     AS INTEGER  FORMAT "99999999"       NO-UNDO.
DEFINE VARIABLE tSumNov     AS DECIMAL  FORMAT "99999999"       NO-UNDO.
DEFINE VARIABLE tFillNew    AS CHARACTER FORMAT "x(758)"        NO-UNDO.
ASSIGN
tnumreg = 0
tSumNov = 0.
FOR EACH TempDatNew WHERE tValorInicial GT 0:
    tnumreg = tnumreg + 1.
    tSumNov = tSumNov + TempDatNew.tEstado.
    PUT UNFORMATTED
        TempDatNew.tTipoId                 FORMAT "9"                                   
        TempDatNew.tNumId                  FORMAT "99999999999"
        TempDatNew.tNumCta                 FORMAT "x(18)"
        TempDatNew.tNombre                 FORMAT "x(45)"
        TempDatNew.tSituacion              FORMAT "9"
        TempDatNew.tFecApertura            FORMAT "x(8)"
        TempDatNew.tFecVenc                FORMAT "x(8)"
        TempDatNew.tRespCalidad            FORMAT "99"
        TempDatNew.tTipoOblig              FORMAT "9"
        TempDatNew.tSubsidio               FORMAT "9"
        TempDatNew.tFecSubsidio            FORMAT "x(8)"
        TempDatNew.tTerminoCont            FORMAT "9"
        TempDatNew.tForPago                FORMAT "9"
        TempDatNew.tPerPago                FORMAT "9"
        TempDatNew.tNovedad                FORMAT "99"
        TempDatNew.tEstadoOrig             FORMAT "9"
        TempDatNew.tFecEstadoOrig          FORMAT "x(8)"
        TempDatNew.tEstado                 FORMAT "99"
        TempDatNew.tFecEstado              FORMAT "x(8)"
        TempDatNew.tEstadoPlast            FORMAT "9"
        TempDatNew.tFecEstadoPlast         FORMAT "x(8)"                                
        TempDatNew.tAdjetivo               FORMAT "9"                                   
        TempDatNew.tFecAdjetivo            FORMAT "x(8)"                                
        TempDatNew.tClaseTarj              FORMAT "9"                                   
        TempDatNew.tFranquicia             FORMAT "9"                                   
        TempDatNew.tNombreMarca            FORMAT "x(30)"                               
        TempDatNew.tTipoMoneda             FORMAT "9"                                   
        TempDatNew.tTipoGrantia            FORMAT "9"                                   
        TempDatNew.tCalificacion           FORMAT "x(2)"
        TempDatNew.tProbabilidadI          FORMAT "999"                                   
        TempDatNew.tEdadMora               FORMAT "999"                                   
        TempDatNew.tValorInicial           FORMAT "99999999999"                         
        TempDatNew.tSaldoDeuda             FORMAT "99999999999"                         
        TempDatNew.tValorDisponible        FORMAT "99999999999"                         
        TempDatNew.tCuotaMes               FORMAT "99999999999"                         
        TempDatNew.tSaldoMora              FORMAT "99999999999"                         
        TempDatNew.tCuotas                 FORMAT "999"                                 
        TempDatNew.tCuotasCanceladas       FORMAT "999"                                 
        TempDatNew.tCuotasMora             FORMAT "999"                                 
        TempDatNew.tClausulaPerm           FORMAT "999"                                 
        TempDatNew.tFecClausulaPerm        FORMAT "x(8)"                                
        TempDatNew.tFecLimitePago          FORMAT "x(8)"                                
        TempDatNew.tFecUltimoPago          FORMAT "x(8)"                                
        TempDatNew.tOficinaRad             FORMAT "x(30)"                               
        TempDatNew.tCiudadRad              FORMAT "x(20)"                               
        TempDatNew.tCodigoDaneRad          FORMAT "99999999"                            
        TempDatNew.tCiudadResidencia       FORMAT "x(20)"                               
        TempDatNew.tCodigoDaneCiuRes       FORMAT "99999999"                            
        TempDatNew.tDepartamentoRes        FORMAT "x(20)"                               
        TempDatNew.tDireccionRes           FORMAT "x(60)"                               
        decimal(TempDatNew.tTelefonoRes)   FORMAT "999999999999"  /* Convertir a número */     
        TempDatNew.tCiudadLaboral          FORMAT "x(20)"                               
        TempDatNew.tCodigoDaneCiuLab       FORMAT "99999999"                            
        TempDatNew.tDepartamentoLab        FORMAT "x(20)"                               
        TempDatNew.tDireccionLab           FORMAT "x(60)"                               
        decimal(TempDatNew.tTelefonoLab)   FORMAT "999999999999"  /* Convertir a número */     
        TempDatNew.tCiudadCorresp          FORMAT "x(20)"                               
        TempDatNew.tCodigoDaneCiuCor       FORMAT "99999999"                            
        TempDatNew.tDepartamentoCor        FORMAT "x(20)"                               
        TempDatNew.tDireccionCor           FORMAT "x(60)"                               
        TempDatNew.tCorreoE                FORMAT "x(60)"                               
        decimal(TempDatNew.tCelular)       FORMAT "999999999999"  /* Convertir a número */     
        TempDatNew.tSuscriptorDest         FORMAT "999999"                              
        TempDatNew.tEspacioBlanco          FORMAT "x(37)"                               
    SKIP.

END.


    PUT UNFORMATTED
        "ZZZZZZZZZZZZZZZZZZ" +
        STRING(year(Fec_Corte), "9999") + STRING(MONTH(Fec_Corte), "99") + STRING(DAY(Fec_Corte), "99") +
        STRING(tnumreg + 2, "99999999")
        STRING(tSumNov, "99999999")
        STRING(tFillNew, "x(758)")
    SKIP.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fondo_liquidez wWin 
PROCEDURE fondo_liquidez :
/* Purpose:  Genera el  PUC en un archivo plano para la SUPERSOLIDARIA */
/*definicion cuentas*/

DEFINE VAR v_csaldo2105                     AS DECIMAL INITIAL 2105.
DEFINE VAR v_saldo2105                      AS DECIMAL. 
DEFINE VAR v_csaldo2110                     AS DECIMAL INITIAL 2110.
DEFINE VAR v_saldo2110                      AS DECIMAL.
DEFINE VAR v_csaldo2125                     AS DECIMAL INITIAL 2125.
DEFINE VAR v_saldo2125                      AS DECIMAL.
DEFINE VAR v_csaldo2130                     AS DECIMAL INITIAL 2130.
DEFINE VAR v_saldo2130                      AS DECIMAL.
DEFINE VAR v_csaldoahorrostotal             AS DECIMAL INITIAL 21.
DEFINE VAR v_saldoahorrostotal              AS DECIMAL.
DEFINE VAR v_fondoliqahorros                AS DECIMAL.
DEFINE VAR v_fondoliquidezdv                AS DECIMAL.
DEFINE VAR v_cfondoliquidezdv               AS DECIMAL INITIAL 1120.
DEFINE VAR v_fondoliquidezdt                AS DECIMAL.
DEFINE VAR v_cfondoliquidezdt               AS DECIMAL INITIAL 1203.
DEFINE VAR v_fondoliqvalores                AS DECIMAL.
DEFINE VAR v_cfondoliquidezpat              AS DECIMAL INITIAL 1203.
DEFINE VAR v_fondoliquidezpat               AS DECIMAL.
DEFINE VAR v_inva6                          AS DECIMAL.
DEFINE VAR v_inva12                         AS DECIMAL.
DEFINE VAR v_invmay12                       AS DECIMAL.
DEFINE VAR v_ctotalinversiones              AS DECIMAL INITIAL 1203.
DEFINE VAR v_totalinversiones               AS DECIMAL.



DEFINE var v_porcentaje AS DECIMAL.
DEFINE var v_cuentaactual AS character.
DEFINE var v_nombrecuentaactual AS character.
DEFINE VAR b_primero AS LOGICAL.



DEFINE VAR v_saldo AS decimal.
DEFINE VAR v_saldo_total AS DECIMAL.
DEFINE VAR tmp_control AS integer.
DEFINE VAR tmp_controlinterno AS integer.
DEFINE VAR tmp_array AS integer.
DEFINE var v_codigos AS character EXTENT 10.
Assign  Prc = 0
        Acum = 0
        v_saldo = 0
        v_saldo_total = 0
        tmp_control = 0
        tmp_controlinterno = 0
        tmp_array = 0
        v_porcentaje = 0
        b_primero = TRUE.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSFLIQ.csv") NO-CONVERT.


/*Hallar totales Y porcentaje*/
/*21*/
RUN hallarsaldo(INPUT v_csaldoahorrostotal,1,2,year(w_fecha),OUTPUT v_saldoahorrostotal).
/*1120*/
RUN hallarsaldo(v_cfondoliquidezdv,1,4,year(w_fecha),OUTPUT v_fondoliquidezdv).
/*1203*/
RUN hallarsaldo(v_cfondoliquidezdt,1,4,year(w_fecha),OUTPUT v_fondoliquidezdt).
/*120306*/
RUN hallarsaldo(INPUT v_cfondoliquidezpat,1,4,year(w_fecha),OUTPUT v_fondoliquidezpat).



 /*hallar total inversiones*/
FOR EACH inversion_sdos WHERE Cod_Producto = 4 AND categoria = 1 AND estado = 1:
    IF  Inversion_Sdos.Fec_Vcto - Fec_Apertura  < 180 THEN
        v_inva6 = v_inva6 +  Sdo_Actual.
    ELSE  
        IF  Inversion_Sdos.Fec_Vcto - Fec_Apertura  < 360 THEN
            v_inva12 = v_inva12 +  Sdo_Actual.
        ELSE 
            v_invmay12 =  v_invmay12 +  Sdo_Actual.
END.
v_totalinversiones = v_inva6 + v_inva12 + v_invmay12.

IF (v_fondoliquidezdv + v_fondoliquidezdt) = 0 THEN
    v_porcentaje = decimal("0,1").
ELSE
    v_porcentaje = v_saldoahorrostotal / (v_fondoliquidezdv + v_fondoliquidezdt).      

v_fondoliqahorros = v_saldoahorrostotal * (v_porcentaje / 100).

/*hallar saldos 2105*/
v_cuentaactual = "0".
RUN hallarsaldo(v_csaldo2105,1,4,year(w_fecha),OUTPUT v_saldo2105).


/*DISPLAY "1;" + string(v_csaldo2105) + ";DEPOSITOS DE AHORRO;;;;;;;;;;".*/
CAMPO0 = "1;" + string(v_csaldo2105) + ";DEPOSITOS DE AHORRO;"+ STRING(v_saldo2105) + ";;;;;;;;;" .
PUT UNFORMATTED
    CAMPO0 AT 1.
FOR EACH sal_cuenta WHERE substr(sal_cuenta.cuenta,1,4) = string(v_csaldo2105) AND year(w_fecha) = ano NO-LOCK  BY cuenta:
   
   v_saldo = 0.
   FIND FIRST Cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
   IF AVAILABLE(Cuentas)  THEN
   DO:
     IF cuentas.Naturaleza = "DB" THEN
              DO tmp_control = 1 TO month(w_fecha):
                 IF tmp_control = 1 THEN
                    v_saldo =   v_saldo + Sal_Cuenta.sal_inicial + Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
                 ELSE
                    v_saldo =   v_saldo +  Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
              end.
     ELSE 
             DO tmp_control = 1 TO month(w_fecha):
                IF tmp_control = 1 THEN 
                      v_saldo = v_saldo +  Sal_Cuenta.sal_inicial + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
                   ELSE
                      v_saldo = v_saldo + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
             END.
     
   END.
   IF b_primero  THEN
   DO:
       b_primero = FALSE.
       v_saldo_total =  v_saldo_total + v_saldo.
       v_cuentaactual = substring(cuentas.cuenta,1,6).
       v_nombrecuentaactual = cuentas.nombre. 
   END.
   ELSE
   DO:
      IF string(v_cuentaactual) NE SUBSTRING(cuentas.Cuenta,1,6) THEN
      DO:    
         /*DISPLAY "1;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".*/
         CAMPO0 = "1;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
         PUT UNFORMATTED
         CAMPO0 AT 1.

         v_cuentaactual = substring(cuentas.cuenta,1,6).
         v_nombrecuentaactual = cuentas.nombre. 
         v_saldo_total = 0.
         v_saldo_total =  v_saldo_total + v_saldo.
      END.
      ELSE
      DO:
             v_saldo_total =  v_saldo_total + v_saldo.
      END.
   END.
END.
/*message "1;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;" VIEW-AS ALERT-BOX.*/
IF NOT b_primero THEN
DO:
CAMPO0 = "1;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
PUT UNFORMATTED
    CAMPO0 AT 1.
END.


/*hallar saldos 2110*/
v_cuentaactual = "0".
v_saldo_total = 0.
b_primero = TRUE.
RUN hallarsaldo(v_csaldo2110,1,4,year(w_fecha),OUTPUT v_saldo2110).
/*DISPLAY "2;" + string(v_csaldo2110) + ";DEPOSITOS DE AHORRO;;;;;;;;;;".*/
CAMPO0 = "2;" + string(v_csaldo2110) + ";DEPOSITOS DE AHORRO;" + STRING(v_saldo2110) + ";;;;;;;;;".
         PUT UNFORMATTED
         CAMPO0 AT 1.
FOR EACH sal_cuenta WHERE substr(sal_cuenta.cuenta,1,4) = string(v_csaldo2110) AND ano = year(w_fecha)  NO-LOCK  BY cuenta:
   
   v_saldo = 0.
   FIND FIRST Cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
   IF AVAILABLE(Cuentas)  THEN
   DO:
     IF cuentas.Naturaleza = "DB" THEN
              DO tmp_control = 1 TO month(w_fecha):
                 IF tmp_control = 1 THEN
                    v_saldo =   v_saldo + Sal_Cuenta.sal_inicial + Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
                 ELSE
                    v_saldo =   v_saldo +  Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
              end.
     ELSE 
             DO tmp_control = 1 TO month(w_fecha):
                IF tmp_control = 1 THEN 
                      v_saldo = v_saldo +  Sal_Cuenta.sal_inicial + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
                   ELSE
                      v_saldo = v_saldo + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
             END.
     
   
   IF b_primero  THEN
   DO:
       b_primero = FALSE.
       v_saldo_total =  v_saldo_total + v_saldo.
       v_cuentaactual = substring(cuentas.cuenta,1,6).
       v_nombrecuentaactual = cuentas.nombre. 
   END.
   ELSE
   DO:
      IF string(v_cuentaactual) NE SUBSTRING(cuentas.Cuenta,1,6) THEN
      DO:    
         /*DISPLAY "2;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".*/
         CAMPO0 = "2;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
         PUT UNFORMATTED
         CAMPO0 AT 1.
         v_cuentaactual = substring(cuentas.cuenta,1,6).
         v_nombrecuentaactual = cuentas.nombre. 
         v_saldo_total = 0.
         v_saldo_total =  v_saldo_total + v_saldo.
      END.
      ELSE
      DO:
             v_saldo_total =  v_saldo_total + v_saldo.
      END.
   END.
  END.
END.
/*DISPLAY "2;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".*/
IF NOT b_primero THEN
DO:
CAMPO0 =  "2;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
PUT UNFORMATTED
    CAMPO0 AT 1.
END.

/*hallar saldos 2125*/

v_cuentaactual = "0".
v_saldo_total = 0.
b_primero = TRUE.
RUN hallarsaldo(v_csaldo2125,1,4,year(w_fecha),OUTPUT v_saldo2125).
/*DISPLAY "3;" + string(v_csaldo2125) + ";DEPOSITOS DE AHORRO;;;;;;;;;;".*/
CAMPO0 = "3;" + string(v_csaldo2125) + ";DEPOSITOS DE AHORRO;" + STRING(v_saldo2125) + ";;;;;;;;;".
         PUT UNFORMATTED
         CAMPO0 AT 1.
FOR EACH sal_cuenta WHERE substr(sal_cuenta.cuenta,1,4) = string(v_csaldo2125) AND year(w_fecha) = ano NO-LOCK  BY cuenta:
   v_saldo = 0.
   FIND FIRST Cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
   IF AVAILABLE(Cuentas)  THEN
   DO:
     IF cuentas.Naturaleza = "DB" THEN
              DO tmp_control = 1 TO month(w_fecha):
                 IF tmp_control = 1 THEN
                    v_saldo =   v_saldo + Sal_Cuenta.sal_inicial + Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
                 ELSE
                    v_saldo =   v_saldo +  Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
              end.
     ELSE 
             DO tmp_control = 1 TO month(w_fecha):
                IF tmp_control = 1 THEN 
                      v_saldo = v_saldo +  Sal_Cuenta.sal_inicial + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
                ELSE
                      v_saldo = v_saldo + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
             END.
     
   
   IF b_primero  THEN
   DO:
       b_primero = FALSE.
       v_saldo_total =  v_saldo_total + v_saldo.
       v_cuentaactual = substring(cuentas.cuenta,1,6).
       v_nombrecuentaactual = cuentas.nombre. 
   END.
   ELSE
   DO:
      IF string(v_cuentaactual) NE SUBSTRING(cuentas.Cuenta,1,6) THEN
      DO:    
         /*DISPLAY "1;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".*/
         CAMPO0 = "3;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
                  PUT UNFORMATTED
                  CAMPO0 AT 1.

     
         v_cuentaactual = substring(cuentas.cuenta,1,6).
         v_nombrecuentaactual = cuentas.nombre. 
         v_saldo_total = 0.
         v_saldo_total =  v_saldo_total + v_saldo.
      END.
      ELSE
      DO:
             v_saldo_total =  v_saldo_total + v_saldo.
     
      END.
   END.
  END.
END.
/*DISPLAY "3;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".*/
IF NOT b_primero THEN
DO:
CAMPO0 = "3;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
PUT UNFORMATTED
    CAMPO0 AT 1.

END.

/*hallar saldos 2130*/
v_cuentaactual = "0".
v_saldo_total = 0.
b_primero = TRUE.
RUN hallarsaldo(v_csaldo2130,1,4,year(w_fecha),OUTPUT v_saldo2130).

/*DISPLAY "4;" + string(v_csaldo2130) + ";DEPOSITOS DE AHORRO;;;;;;;;;;".*/
CAMPO0 = "4;" + string(v_csaldo2130) + ";DEPOSITOS DE AHORRO;" + STRING(v_saldo2130) + ";;;;;;;;;".
PUT UNFORMATTED
    CAMPO0 AT 1.

FOR EACH sal_cuenta WHERE substr(sal_cuenta.cuenta,1,4) = string(v_csaldo2130) AND year(w_fecha) = ano NO-LOCK  BY cuenta:
   
   v_saldo = 0.
   FIND FIRST Cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
      IF AVAILABLE(Cuentas)  THEN
   DO:
     IF cuentas.Naturaleza = "DB" THEN
              DO tmp_control = 1 TO month(w_fecha):
                 IF tmp_control = 1 THEN
                    v_saldo =   v_saldo + Sal_Cuenta.sal_inicial + Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
                 ELSE
                    v_saldo =   v_saldo +  Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
              end.
     ELSE 
             DO tmp_control = 1 TO month(w_fecha):
                IF tmp_control = 1 THEN 
                      v_saldo = v_saldo +  Sal_Cuenta.sal_inicial + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
                   ELSE
                      v_saldo = v_saldo + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
             END.
     
        
   
   IF b_primero  THEN
   DO:
       b_primero = FALSE.
       v_saldo_total =  v_saldo_total + v_saldo.
       v_cuentaactual = substring(cuentas.cuenta,1,6).
       v_nombrecuentaactual = cuentas.nombre. 
   END.
   ELSE
   DO:
      IF string(v_cuentaactual) NE SUBSTRING(cuentas.Cuenta,1,6) THEN
      DO:    
         CAMPO0 = "4;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
         PUT UNFORMATTED
         CAMPO0 AT 1.

         v_cuentaactual = substring(cuentas.cuenta,1,6).
         v_nombrecuentaactual = cuentas.nombre. 
         v_saldo_total = 0.
         v_saldo_total =  v_saldo_total + v_saldo.
      END.
      ELSE
      DO:
             v_saldo_total =  v_saldo_total + v_saldo.
      END.
   END.
   END.
END.
IF NOT b_primero THEN
DO:
CAMPO0 = "4;" + substring(cuentas.cuenta,1,6) + ";" + v_nombrecuentaactual + ";" + STRING(v_saldo_total) + ";" + STRING(v_saldo_total / v_porcentaje) + ";" + STRING(v_porcentaje) + ";;;;;;;".
PUT UNFORMATTED
    CAMPO0 AT 1.
END.
 
/*renglon total*/
CAMPO0 = "5;000005;TOTAL;" + string(v_saldoahorrostotal) + ";" + string(v_fondoliquidezdv + v_fondoliquidezdt) + ";" + STRING (v_porcentaje) + ";" + string(v_fondoliquidezdv) + ";;;" + STRING(v_inva6) + ";" + STRING(v_inva12) + ";" + STRING(v_invmay12)+ ";" + string(v_fondoliquidezpat).
PUT UNFORMATTED
    CAMPO0 AT 1.

OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaBrecha wWin 
PROCEDURE HallaBrecha :
/*DEFINE INPUT PARAMETER xfec1 AS DATE.
DEFINE INPUT PARAMETER xfec2 AS DATE.
DEFINE VAR v_cxc AS DECIMAL INITIAL 1620.
ASSIGN xtitulos   = 0.


/*cuentas de ahorro*/
FOR EACH Ctas_REPORTES WHERE Cod_Reporte = "01" AND tipo = 2:
    FOR EACH Ahorros WHERE Fec_Vencimiento GE XFec1 AND 
                           Fec_Vencimiento LE XFec2 AND
                           Ahorros.Estado = 1 AND (Ahorros.tip_Ahorro = Ctas_REPORTES.Tipo_producto):

        ASSIGN XTITULOS   = XTITULOS + 1.

    end.
    create brecha.
    assign brecha.Banda    = i
           brecha.Nro      = xtitulos
           brecha.nombre   = Ctas_REPORTES.Nombre
           brecha.captura  = Ctas_REPORTES.Captura
           brecha.renglon  = Ctas_REPORTES.Renglon
           brecha.cuenta   = Ctas_REPORTES.Cuenta.

    /*Hallamos cuntas de ahorros*/
    ASSIGN  xtitulos   = 0.

END.


/******************/
/*Cuentas Credito*/
/******************/

 ASSIGN xtitulos   = 0.
FOR EACH Ctas_REPORTES WHERE Cod_Reporte = "01" AND tipo = 3:
    for each CREDITOS WHERE (FEC_DESEMBOLSO + (CREDITOS.PLAZO * 30)) GE XFEC1 
                            AND  CREDITOS.ESTADO = 2 
                            AND  Creditos.Tip_Credito = Ctas_REPORTES.tipo_producto:

        IF I = 8 THEN DO:
           IF CREDITOS.SDO_CAPITAL > (CREDITOS.CUOTA * 10) THEN 
                ASSIGN XTITULOS   = XTITULOS + 1.

        END.
        ELSE DO:
                ASSIGN XTITULOS   = XTITULOS + 1.
        END.
    
    end.
    create brecha.
           ASSIGN   brecha.Banda    = i
                    brecha.Nro      = xtitulos
                    brecha.nombre   = Ctas_REPORTES.Nombre
                    brecha.captura  = Ctas_REPORTES.Captura
                    brecha.renglon  = Ctas_REPORTES.Renglon
                    brecha.cuenta   = Ctas_REPORTES.Cuenta.
                    
           /*Hallamos cuntas de ahorros*/
    ASSIGN  xtitulos   = 0.

END.



/*Cuentas tabla detalle*/
ASSIGN  xtitulos   = 0.

FOR EACH Ctas_REPORTES WHERE Cod_Reporte = "01" AND tipo = 1:
    for each DETALLE WHERE DETALLE.CUENTA = Ctas_REPORTES.CUENTA AND  (FEC_Contable + (detalle.PLAZO )) GE XFEC1:
        IF Ctas_REPORTES.Naturaleza = "DB" THEN
           ASSIGN XTITULOS   = XTITULOS + 1.     
        ELSE 
           ASSIGN XTITULOS   = XTITULOS + 1.     
                  
    end.
    create brecha.
    assign brecha.Banda    = i
           brecha.Nro      = xtitulos
           brecha.nombre   = Ctas_REPORTES.Nombre
           brecha.captura  = Ctas_REPORTES.Captura
           brecha.renglon  = Ctas_REPORTES.Renglon
           brecha.cuenta   = Ctas_REPORTES.Cuenta.
           
    ASSIGN xtitulos   = 0.


END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaCausado wWin 
PROCEDURE HallaCausado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER IntCausado AS DECIMAL     NO-UNDO.
  
  DEFINE VARIABLE dias_no_vencidos  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE Tint_CteNoVencido AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE W_tasaDiaria      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE WPer              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE W_Tasa2           LIKE creditos.tasa.
  DEFINE VARIABLE Periodo           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE WDia              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE WMes              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE WnDias            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE tFecha            AS DATE        NO-UNDO.
  

      /*** Halla el número de dias del mes ***/
    tfecha = ?.
    WDia = DAY(TODAY).
    WMes = MONTH(TODAY).
    /* Meses de 31 dias */
    IF  Wmes EQ 1   OR
        Wmes EQ 3   OR
        Wmes EQ 5   OR
        Wmes EQ 7   OR
        Wmes EQ 8   OR
        Wmes EQ 10  OR
        Wmes EQ 12 
        THEN DO:
        WnDias = 31.
    END.
    ELSE DO:
        IF Wmes EQ 2 THEN
            WnDias = 28.
        ELSE
            WnDias = 30.
    END.
    IF creditos.per_Pago EQ 3 THEN DO:
        WPer = 15.
        Periodo = 24.
        W_Tasa2 = creditos.tasa / 2400.
        W_tasaDiaria = W_Tasa2 / 15.

        IF day(creditos.fec_paganti) LE WnDias THEN
            tFecha = DATE(WMes , DAY(fec_paganti), YEAR(TODAY)).
        ELSE
            tFecha = DATE(WMes , WnDias, YEAR(TODAY)).
        IF tfecha - TODAY GT 15 THEN
            tfecha = tfecha - INTEGER(WnDias / 2).
        IF tfecha LT TODAY THEN tfecha = tfecha + INTEGER(WnDias / 2).
    END.
    ELSE IF creditos.per_Pago EQ 4 THEN DO:
        WPer = WnDias.
        W_Tasa2 = creditos.tasa / 1200.
        W_tasaDiaria = W_Tasa2 / 30.
        Periodo = 12.
        
        IF creditos.diapago NE 0 THEN DO:
            IF creditos.diapago LE WnDias THEN
                tfecha = DATE(WMes, creditos.diapago, YEAR(TODAY)).
            ELSE
                tfecha = DATE(WMes, WnDias, YEAR(TODAY)).
        END.
        ELSE DO:
            IF day(creditos.fec_paganti) LE WnDias THEN
                tfecha = DATE(WMes, day(creditos.fec_paganti), YEAR(TODAY)).
            ELSE
                tfecha = DATE(WMes, WnDias, YEAR(TODAY)).
        END.
        IF tfecha LT TODAY THEN tfecha = tfecha + INTEGER(WnDias).        
    END.

  /****************************************************/

  

    dias_no_vencidos = TODAY  - (tFecha - WPer).
    Tint_CteNoVencido = sdo_proyectado * W_tasaDiaria * dias_no_vencidos.
    IF Tint_CteNoVencido LT 0 THEN Tint_CteNoVencido = 0.


    IF creditos.sistema EQ 2 OR (creditos.per_pago NE 3 AND creditos.per_pago NE 4) THEN DO:
      ASSIGN Tint_CteNoVencido = 0.
    END.
    IF creditos.sdo_capital GT Sdo_Proyectado THEN DO:    /* Atrasado */
        IF Tint_CteNoVencido GT creditos.Int_Corrientes THEN DO:
          ASSIGN Tint_CteNoVencido = 0.
        END.
        IF tFecha - TODAY LE 3 THEN do:
          ASSIGN Tint_CteNoVencido = 0.
        END.
    END.
    ELSE DO:  /* Al dia */
         ASSIGN Tint_CteNoVencido = 0.
  /*       IF tFecha - TODAY LE 3 THEN do:                              */
  /*         ASSIGN Tint_CteNoVencido = 0.                              */
  /*       END.                                                         */
  /*       ELSE DO:                                                     */
  /*           IF Tint_CteNoVencido GT creditos.Int_Corrientes THEN DO: */
  /*             ASSIGN Tint_CteNoVencido = 0.                          */
  /*           END.                                                     */
  /*       END.                                                         */
    END.

    ASSIGN IntCausado = Tint_CteNoVencido.
/********************************************************************************************************************************/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaMaxDias wWin 
PROCEDURE HallaMaxDias :
FOR EACH rep_creditos
    FIELDS(rep_creditos.dias_atraso
           rep_creditos.reestructurado
           rep_creditos.fec_reestructurado
           rep_creditos.nit
           rep_creditos.tip_credito
           rep_creditos.categoriames) WHERE rep_creditos.dias_atraso GT 0 AND rep_creditos.estado EQ 2 AND rep_creditos.fecCorte = fec_corte NO-LOCK:
    FIND FIRST tMaxDias WHERE wnit EQ rep_creditos.nit AND wTip EQ rep_creditos.tip_credito NO-ERROR.
    IF AVAILABLE tMaxDias THEN DO:
        IF rep_creditos.Dias_Atraso GT wDiaMax THEN
            wDiaMax = rep_creditos.Dias_Atraso.
    END.
    ELSE DO:
        CREATE tMaxDias.
        ASSIGN wnit     = rep_creditos.nit
               wTip     = rep_creditos.tip_credito
               wDiaMax  = rep_creditos.Dias_Atraso.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaRango wWin 
PROCEDURE HallaRango :
DEFINE INPUT PARAMETER xfec1 AS DATE.
DEFINE INPUT PARAMETER xfec2 AS DATE.
DEFINE VAR v_cxc AS DECIMAL INITIAL 1620.
ASSIGN Xsumavalor = 0
       xsumatasa  = 0
       xtitulos   = 0.

/*****************/
/*  HALLA AHORROS*/
/*****************/
FOR EACH Ahorros WHERE Fec_Vencimiento GE XFec1 AND 
                       Fec_Vencimiento LE XFec2 AND
                       Ahorros.Estado = 1 AND (
                       Ahorros.Cod_Ahorro = 5 OR Ahorros.Cod_Ahorro = 3):

    ASSIGN XSUMAVALOR = XSUMAVALOR + SDO_DISPONIBLE + SDO_CANJE + int_pagar
           XTITULOS   = XTITULOS + 1.
    CASE Ahorros.Per_Liquidacion:
         WHEN 0 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 360) / 12).
         WHEN 1 THEN
           XSUMATASA = XSUMATASA + (( Ahorros.Tasa * 52) / 12).
         WHEN 2 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 36) / 12).
         WHEN 3 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 26) / 12).
         WHEN 4 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 12) / 12).
         WHEN 5 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 6) / 12).
         WHEN 6 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 4) / 12).
         WHEN 7 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 3) / 12).
         WHEN 8 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 2) / 12).
         WHEN 9 THEN
           XSUMATASA = XSUMATASA + (Ahorros.Tasa) / 12.
         WHEN 10 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 360) / 12).
     END CASE.

end.
create brecha.
assign brecha.producto = 1
       brecha.Banda    = I
       brecha.valor    = xsumavalor
       brecha.tasa     = xsumatasa
       brecha.Nro      = xtitulos
       brecha.nombre   = "CERTIFICADOS DE DEPOSITOS A TERMINO FIJO"
       brecha.captura  = "010"
       brecha.renglon  = "2"
       brecha.cuenta   =  "211015".

/******************/
/*  hALLA CREDITOS*/
/******************/

 ASSIGN Xsumavalor = 0
        xsumatasa  = 0
        xtitulos   = 0.

for each CREDITOS WHERE (FEC_DESEMBOLSO + (CREDITOS.PLAZO * 30)) GE XFEC1 AND 
                       CREDITOS.ESTADO = 1 :

    IF I = 8 THEN DO:
       IF CREDITOS.SDO_CAPITAL > (CREDITOS.CUOTA * 10) THEN 
          ASSIGN XSUMAVALOR = XSUMAVALOR + (CREDITOS.SDO_CAPITAL - (CREDITOS.CUOTA * 10))
                 XSUMATASA  = XSUMATASa + CREDITOS.TASA
                 XTITULOS   = XTITULOS + 1.

    END.
    ELSE DO:
       ASSIGN XSUMAVALOR = XSUMAVALOR + CREDITOS.CUOTA
              XSUMATASA  = XSUMATASa + CREDITOS.TASA
              XTITULOS   = XTITULOS + 1.
    END.
    
end.
create brecha.
assign brecha.producto = 2
       brecha.Banda    = i
       brecha.valor    = xsumavalor
       brecha.tasa     = xsumatasa
       brecha.Nro      = xtitulos
       brecha.nombre   = "CARTERA DE CREDITOS CONSUMO"
       brecha.captura  = "050"
       brecha.renglon  = "1"
       brecha.cuenta   = "140501".


 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarSaldo wWin 
PROCEDURE HallarSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER n_cuenta AS DECIMAL.
DEFINE INPUT PARAMETER n_posini AS integer.
DEFINE INPUT PARAMETER n_posfin AS INTEGER.
DEFINE INPUT PARAMETER f_fecha  AS integer.
DEFINE OUTPUT PARAMETER n_saldo AS DECIMAL.
DEFINE VAR tmp_control AS INTEGER.

DEFINE VAR v_saldo AS DECIMAL.      
v_saldo = 0.
FOR each sal_Cuenta WHERE substr(cuenta,n_posini,n_posfin) = string(n_cuenta) AND ano = f_fecha NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas  THEN
            IF cuentas.Naturaleza = "DB" THEN
                DO tmp_control = 1 TO month(w_fecha):
                   IF tmp_control = 1 THEN
                      v_saldo =   v_saldo + Sal_Cuenta.sal_inicial + Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
                   ELSE
                      v_saldo =   v_saldo +  Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
                end.
             ELSE 
                DO tmp_control = 1 TO month(w_fecha):
                   IF tmp_control = 1 THEN 
                      v_saldo = v_saldo +  Sal_Cuenta.sal_inicial + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
                   ELSE
                      v_saldo = v_saldo + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
                END.
END.
n_saldo = v_saldo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
HIDE FRAME frm.
HIDE FRAME frm_brechas.
ASSIGN Fec_Corte:SCREEN-VALUE IN FRAME Frm0 = STRING(TODAY).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NomAgencia wWin 
PROCEDURE NomAgencia :
DEFINE INPUT  PARAMETER agencia LIKE agencias.agencia.
 DEFINE OUTPUT PARAMETER OfiBak  AS CHARACTER FORMAT "X(15)".
 FIND FIRST agencias WHERE agencias.agencia EQ agencia NO-LOCK NO-ERROR.
 IF AVAILABLE(agencias) THEN
  OfiBak = agencias.Nombre.
 ELSE
  OfiBak = "MEDELLIN".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OperacionesPorProducto wWin 
PROCEDURE OperacionesPorProducto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PeriodoEmpresa wWin 
PROCEDURE PeriodoEmpresa :
DEFINE VAR NumDias AS DECIMAL FORMAT "99999".
 DEFINE VAR FecAuxi AS DATE.
 DEFINE VAR NCuoAtr AS DECIMAL FORMAT "99999".
 DEFINE VAR NCuoEmp AS DECIMAL FORMAT "99999". 
 DEFINE VAR Multdor AS DECIMAL FORMAT "99".
 DEFINE VAR DifAno  AS DECIMAL FORMAT "99".
 DEFINE VAR DifCuo  AS DECIMAL FORMAT "99".
 CASE Creditos.Per_pago:                           
  WHEN 1 THEN ASSIGN NumDias = Creditos.Plazo * 7    /* semanal */
                     NCuoEmp = 52                  
                     Multdor = 7
                     TmpDat.IPerPag = 9.
  WHEN 2 THEN ASSIGN NumDias = Creditos.Plazo * 10   /* Decadal */
                     NCuoEmp = 36
                     MultDor = 10
                     TmpDat.IPerPag = 9.
  WHEN 3 THEN ASSIGN NumDias = Creditos.Plazo * 15  /* Quincenal */
                     NCuoEmp = 26
                     MultDor = 15
                     TmpDat.IPerPAg = 9.
  WHEN 4 THEN ASSIGN NumDias = Creditos.Plazo * 30  /* Mensual */
                     NCuoEmp = 12
                     Multdor = 30
                     TmpDat.IPerPag = 1.

  WHEN 5 THEN ASSIGN NumDias = Creditos.Plazo * 60  /* BiMensual */
                     NCuoEmp = 6
                     Multdor = 60
                     TmpDat.IPerPag = 2.
  WHEN 6 THEN ASSIGN NumDias = Creditos.Plazo * 90  /* TriMestral */
                     NCuoEmp = 4
                     Multdor = 90
                     TmpDat.IPerPag = 3.

  WHEN 8 THEN ASSIGN NumDias = Creditos.Plazo * 180  /* Semestral */
                     NCuoEmp = 2
                     Multdor = 180
                     TmpDat.IPerPag = 4.
  WHEN 9 THEN ASSIGN NumDias = Creditos.Plazo * 360  /* TriMestral */
                     NCuoEmp = 1
                     Multdor = 360
                     TmpDat.IPerPag = 5.
  OTHERWISE  IPerPag = 9.        
 END CASE.
 ASSIGN FecAuxi = Creditos.Fec_Desembolso + NumDias
        IFecVen = DECIMAL(STRING(YEAR(FecAuxi)) + STRING(MONTH(FecAuxi),"99")). 

 NCuoAtr = 10.
 ASSIGN ICuoMor = NCuoAtr
        IFecPag = 0.
  ASSIGN ICuoMor = 0
         IFecPag = 0
         INoveda = 0
         ICalifi = "A"
         IEdaMor = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesar wWin 
PROCEDURE Procesar :
ASSIGN FRAME frm0
    t1
    t2
    t3
    t4
    t5
    t6
    t7
    t8
    t-5
    t-8
    t-10
    t-12
    t-13
    t-18
    T_UIAF
    t-2
    tgSaldosDiariosDepositos
    Fec_Corte.

IF T1 THEN DO:
   m2 = W_Pathspl + "DATACREDITO" + STRING(MONTH(Fec_Corte),"99") +
                 STRING(YEAR(Fec_Corte),"9999")  + ".txt".
   DISPLAY "Generando Archivo Creditos DATACREDITO..." @ m1 m2 WITH FRAME Frm0.
   MESSAGE  "ojo" SKIP m2 VIEW-AS ALERT-BOX  INFO BUTTON OK.
 
/*
   RUN DataCreditoOLD.  /* Inhabilitar para el cierre de julio */
*/


   RUN DataCredito.
   DISPLAY "Generando Archivo Clientes DATACREDITO..." @ m1 m2 WITH FRAME Frm0.
   RUN DataCreditoClientes.



   t1 = TRUE.
   DISPLAY T1 WITH FRAME Frm0.
END.
IF T2 THEN DO:
   m2 = W_Pathspl + "\FGCAHO.TXT".
   DISPLAY "Generando Archivo Ahorros FOGACOOP..." @ m1 m2 WITH FRAME Frm0.
   RUN AhoFgc.
   t2 = TRUE.
   DISPLAY T2 WITH FRAME Frm0.
END.
IF T3 THEN DO:
   m2 = W_Pathspl + "\FGCcre.TXT".
   DISPLAY "Generando Archivo Creditos FOGACOOP..." @ m1 m2 WITH FRAME Frm0.
   RUN CreFgc.
   t3 = TRUE.
   DISPLAY T3 WITH FRAME Frm0.
END.
IF T4 THEN DO:
   m2 = W_Pathspl + "\SSAho.csv".
   DISPLAY "Generando Archivo Ahorros SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SSAhorro.
   t4 = TRUE.
   DISPLAY T4 WITH FRAME Frm0.
END.
IF T5 THEN DO:
   m2 = W_Pathspl + "\SSApo.csv".
   DISPLAY "Generando Archivo Aportes SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SSAportes.
   t5 = TRUE.
   DISPLAY T5 WITH FRAME Frm0.
END.
IF T6 THEN DO:
   m2 = W_Pathspl + "SSCre.csv".
   DISPLAY "Generando Archivo Creditos SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SSCreditos.
   t6 = TRUE.
   DISPLAY T6 WITH FRAME Frm0.
END.

IF T7 THEN DO:
   m2 = W_Pathspl + "SScli.csv".
   DISPLAY "Generando Archivo Clientes SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SScliente.
   t7 = TRUE.
   DISPLAY T7 WITH FRAME Frm0.
END.

IF T8 THEN DO:
   m2 = W_Pathspl + "\FGCbre.TXT".
   DISPLAY "Generando Archivo BRECHA DE LIQUIDEZ FOGACOOP..." @ m1 m2 WITH FRAME Frm0.
   RUN BreFG.
   t8 = TRUE.
   DISPLAY T8 WITH FRAME Frm0.
END.
IF T-18 THEN DO:
   VIEW FRAME frm_brechas.
   m2 = W_Pathspl + "\brecha.TXT".
   DISPLAY "Generando Archivo BRECHA DE LIQUIDEZ .." @ m1 m2 WITH FRAME Frm0.
   t-18 = TRUE.
   DISPLAY T-18 WITH FRAME Frm0.
END.

IF t-5 THEN DO:
   m2 = W_Pathspl + "\PUC.TXT".
   DISPLAY "Generando Archivo PUC..." @ m1 m2 WITH FRAME Frm0.
   RUN Puc2.
   t-5 = TRUE.
   DISPLAY T-5 WITH FRAME Frm0.
END.

IF t-8 THEN DO:
   m2 = W_Pathspl + "\FOLIQ.TXT".
   DISPLAY "Generando Archivo Fondo de liquidez..." @ m1 m2 WITH FRAME Frm0.
   RUN fondo_liquidez.
   t-8 = TRUE.
   DISPLAY T-8 WITH FRAME Frm0.
END.

IF T-10 THEN DO:
   m2 = W_Pathspl + "\PROCRE.TXT".
   DISPLAY "Generando Archivo PROCREDITO..." @ m1 m2 WITH FRAME Frm0.
   RUN ProCredito.
   t-10 = TRUE.
   DISPLAY T-10 WITH FRAME Frm0.
END.
IF T-9 THEN DO:
   m2 = W_Pathspl + "\RentaFija.TXT".
   DISPLAY "Generando Archivo RentaFija..." @ m1 m2 WITH FRAME Frm0.
   RUN RentaFija.
   t-9 = TRUE.
   DISPLAY T-10 WITH FRAME Frm0.
END.
IF T-11 THEN DO:
   m2 = W_Pathspl + "\RentaVariable.TXT".
   DISPLAY "Generando Archivo RentaVariable..." @ m1 m2 WITH FRAME Frm0.
   RUN RentaVariable.
   t-11 = TRUE.
   DISPLAY T-11 WITH FRAME Frm0.
END.
IF t-12 THEN DO:
   m2 = W_Pathspl + "\BienesyServicios.TXT".
   DISPLAY "Generando Archivo BienesyServicios..." @ m1 m2 WITH FRAME Frm0.
   RUN BienesyServicio.
   t-12 = TRUE.
   DISPLAY T-12 WITH FRAME Frm0.
END.
IF t-13 THEN DO:
   m2 = W_Pathspl + "\RiesgoLiquidez.TXT".
   DISPLAY "Generando Archivo RiesgoLiquidez..." @ m1 m2 WITH FRAME Frm0.
   RUN RiesgoLiquidez.
   t-13 = TRUE.
   DISPLAY T-13 WITH FRAME Frm0.
END.

IF T_UIAF THEN DO:
   m2 = W_Pathspl + "utrahuilca" + STRING(MONTH(Fec_Corte),"99") + SUBSTRING(STRING(YEAR(Fec_Corte),"9999"),3,2) + ".TXT".
   DISPLAY "Generando Archivo TRANS.EFEC.SEC.FINANCIERO..." @ m1 m2 WITH FRAME Frm0.
   RUN UIAF.
   T_UIAF = TRUE.
   DISPLAY T_UIAF WITH FRAME Frm0.
END.

IF T-2 THEN DO:
   m2 = W_Pathspl + "CIFIN" + STRING(MONTH(Fec_Corte),"99") + STRING(YEAR(Fec_Corte),"9999") + ".TXT".
   DISPLAY "Generando Archivo CIFIN..." @ m1 m2 WITH FRAME Frm0.
   RUN CIFIN_junio2009.
   T-2 = TRUE.
   DISPLAY T-2 WITH FRAME Frm0.
END.

/* Operaciones por producto */
IF t-19 EQ YES THEN DO:
   m2 = W_Pathspl + "OperacionesPorProducto.csv".
   DISPLAY "Generando Archivo SES..." @ m1 m2 WITH FRAME Frm0.   
   RUN OperacionesxProducto.r (INPUT date(Fec_Corte:SCREEN-VALUE IN FRAME Frm0), INPUT DECIMAL(SalMin:SCREEN-VALUE IN FRAME Frm0)).
   
   MESSAGE "Archivo Generado con éxito en: " SKIP
       m2
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   t-19 = TRUE.
   DISPLAY T-19 WITH FRAME Frm0.
END.

IF tgSaldosDiariosDepositos = TRUE THEN DO:
    m2 = W_Pathspl + "saldosDiarios2105_" + STRING(DAY(fec_corte)) + STRING(MONTH(fec_corte)) + STRING(YEAR(fec_corte)) + ".csv".
    DISPLAY "Generando Archivo Saldos Diarios de Depósitos..." @ m1 m2 WITH FRAME Frm0.
    
    RUN Rp-saldosDiarios2105.r (INPUT fec_corte,
                               INPUT m2) NO-ERROR.

    MESSAGE "El archivo de Saldos Diarios de Depósitos" SKIP
            "fue generado con éxito"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

MESSAGE "Proceso de Exportación Culminado" VIEW-AS ALERT-BOX.

ASSIGN t1 = NO
       t2 = NO
       t3 = NO
       t4 = NO
       t5 = NO
       t6 = NO
       t7 = NO
       t8 = NO
       t-10 = NO
       t-5 = NO
       T_UIAF = NO
       t-19 = NO
       m1 = ""
       m2 = ""
       prc = 0
       ced = "".

DISPLAY t1 t2 t3 t4 t5 t6 t7 t8 t-10 T_UIAF prc ced m1 m2 WITH FRAME frm0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procredito wWin 
PROCEDURE Procredito :
DEFINE VAR NumCod  AS INTEGER   FORMAT "9".
DEFINE VAR XMul    AS INTEGER   FORMAT "99".
DEFINE VAR XDiaAtr AS INTEGER   FORMAT "9999".
DEFINE VAR Campolargo AS CHARACTER FORMAT "X(300)".
Prc = 0.
FOR EACH Creditos WHERE /*Creditos.nit GE "22005017" and Creditos.nit LT "22005205" and*/
                        Creditos.Num_Credito    NE 0 AND 
                        Creditos.Sdo_Capital    GT 0 AND
                        Creditos.Estado         EQ 2 AND
                        Creditos.Fec_Desembolso LE Fec_Corte
                        NO-LOCK BREAK BY Creditos.Nit:
  ASSIGN Prc = Prc + 1
         Ced = Creditos.Nit.
  DISPLAY Prc Ced WITH FRAME FRM0.
  FIND FIRST Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
  CREATE Procredito.
  IF AVAILABLE(Clientes) THEN
   DO:
    CASE Clientes.Tipo_Identificacion:
      WHEN "C.C" THEN ProCredito.Tip_Doc = 1.
      WHEN "NIT" THEN ProCredito.Tip_Doc = 2.
      OTHERWISE ProCredito.Tip_Doc = 3.
    END CASE.
    ASSIGN Procredito.Agencia = Creditos.Agencia
           Procredito.Num_Doc = Clientes.Nit
           ProCredito.Tip_Obl = 1
           Procredito.Ide_Per = 1
           ProCredito.Num_Obl = Creditos.Num_Credito
           Procredito.Nom_Cde = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre
           Procredito.DIR_Cde = Clientes.DIR_Residencia
           Procredito.Tel_Cde = Clientes.Tel_Residencia
           Procredito.Val_Mor = Creditos.Val_Atraso
           Procredito.Cuo_Mor = Creditos.Cuo_Atraso
           Procredito.Fec_Ini = DECIMAL(STRING(YEAR(Creditos.Fec_Desembolso),"9999") + STRING(MONTH(Creditos.Fec_Desembolso),"99") + STRING(DAY(Creditos.Fec_Desembolso),"99"))
           Procredito.Fec_Ult = DECIMAL(STRING(YEAR(Creditos.Fec_UltPago),"9999") + STRING(MONTH(Creditos.Fec_UltPago),"99") + STRING(DAY(Creditos.Fec_UltPago),"99"))
           Procredito.Val_Ini = Monto NO-ERROR.
   END.
   NumCod = 0.
   FOR EACH Relaciones WHERE 
            Relaciones.Nit             EQ Creditos.Nit AND
            Relaciones.Cod_Relacion    EQ 11           AND
            Relaciones.Clase_Producto  EQ 2            AND
            Relaciones.Fec_Inactividad EQ ?            AND
            Relaciones.Cod_Producto    EQ Creditos.Cod_Credito AND
            Relaciones.Cuenta          EQ STRING(Creditos.Num_Solicitud) AND
            Relaciones.Estado          EQ 1 NO-LOCK:
            NumCod = NumCod + 1.
            IF NumCod GT 2 THEN NEXT.
            ASSIGN Prc = Prc + 1
                   Ced = Creditos.Nit.
            DISPLAY Prc Ced WITH FRAME FRM0.
            CREATE Procredito.
            FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF AVAILABLE Clientes THEN DO:
               CASE Clientes.Tipo_Identificacion:
                WHEN "C.C" THEN ProCredito.Tpd_Cde = 1.
                WHEN "NIT" THEN ProCredito.Tpd_Cde = 2.
                OTHERWISE ProCredito.Tpd_Cde = 3.
               END CASE.
               ASSIGN Procredito.Num_Doc = Creditos.Nit
                      ProCredito.Doc_Cde = Relaciones.Nit_Relacion
                      ProCredito.Tip_Obl = 1
                      ProCredito.Ide_Per = 2
                      ProCredito.Num_Obl = Creditos.Num_Credito
                      Procredito.Nom_Cde = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre
                      Procredito.DIR_Cde = Clientes.DIR_Residencia
                      Procredito.Tel_Cde = Clientes.Tel_Residencia.
            END.
   END.
END.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_Agencia,"99") + "PROCRE.TXT") NO-CONVERT.
PUT UNFORMATTED CRegCon.
FOR EACH ProCredito:
    CampoLargo = 
         STRING(ProCredito.Tip_Doc,"9") + ";" + 
         STRING(ProCredito.Num_Doc,"999999999999") + ";" + 
         STRING(ProCredito.Tip_Obl,"9") + ";" + 
         STRING(ProCredito.Num_Obl,"9999999") + ";" +
         STRING(ProCredito.Ide_Per,"9") + ";" +
         STRING(ProCredito.Tpd_Cde,"9") + ";" +
         ProCredito.Doc_Cde             + ";" +
         ProCredito.DIR_Cde             + ";" +
         ProCredito.Tel_Cde             + ";" +
         ProCredito.Nom_Cde             + ";" +
         STRING(ProCredito.Val_Ini,"99999999") + ";" +
         STRING(ProCredito.Fec_Ini)  + ";" +
         STRING(ProCredito.Cuo_Mor,"99") + ";" +
         STRING(ProCredito.Val_Mor,"99999999") + ";" +
         STRING(ProCredito.Fec_Ult) + ";" +
         STRING(ProCredito.Agencia,"999").
    
     PUT UNFORMATTED
         CampoLargo AT 1.

/*    ProCredito.Tip_Doc  AT 1  FORMAT "9"
    ProCredito.Num_Doc  AT 2  FORMAT "X(11)"
    ProCredito.Tip_Obl  AT 13 FORMAT "9"
    ProCredito.Num_Obl  AT 14 FORMAT "9999999"
    ProCredito.Ide_Per  AT 21 FORMAT "9"
    ProCredito.Tpd_Cde  AT 22 FORMAT "9"
    ProCredito.Doc_Cde  AT 23 FORMAT "X(11)"
    ProCredito.DIR_Cde  AT 34 FORMAT "X(35)"
    ProCredito.Tel_Cde  AT 69 FORMAT "X(7)"
    ProCredito.Nom_Cde  AT 76 FORMAT "X(25)"
    ProCredito.Val_Ini  AT 101 FORMAT "99999999"
    ProCredito.Fec_Ini  AT 109 FORMAT "99999999"
    ProCredito.Cuo_Mor  AT 117 FORMAT "99"
    ProCredito.Val_Mor  AT 119 FORMAT "99999999"
    ProCredito.Fec_Ult  AT 127 FORMAT "99999999"
    ProCredito.Agencia  AT 135 FORMAT "999".*/
END.
DISPLAY CRegFin NO-LABELS.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Puc wWin 
PROCEDURE Puc :
/* Purpose:  Genera el PUC en un archivo plano para la SUPERSOLIDARIA */
/* Modificado Giocam  - 01/23/08 - no validaba nombre de la cuenta, la estaba dejando fuera de foreach*/
DEFINE VAR v_saldo AS decimal.
DEFINE VAR tmp_control AS integer.
CAMPO0 = "".
 Assign Prc = 0
        Acum = 0
        v_saldo = 0
        tmp_control = 0.
 
 /*DISPLAY Prc  WITH FRAME FRM0.*/
 EMPTY TEMP-TABLE tpuc.
 FOR EACH sal_cuenta WHERE cuenta NE ? AND ano = YEAR(DATE(Fec_Corte:SCREEN-VALUE IN FRAME Frm0)) NO-LOCK:   
   ASSIGN Prc = Prc + 1
          tmp_control = 0.
   FIND FIRST Cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
   IF AVAILABLE(Cuentas)  THEN DO:
      IF cuentas.Naturaleza = "DB" THEN
         DO tmp_control = 1 TO month(w_fecha):
            v_saldo = v_saldo + Sal_Cuenta.Db[tmp_control] - Sal_Cuenta.Cr[tmp_control].
         end.
      ELSE 
         DO tmp_control = 1 TO month(w_fecha):
            v_saldo = v_saldo + Sal_Cuenta.CR[tmp_control] - Sal_Cuenta.DB[tmp_control].
         END.
      CREATE tpuc.
      ASSIGN tpuc.pucCta    = Sal_Cuenta.cuenta
             tpuc.pucNomCta = UPPER(Cuentas.nombre)
             tpuc.pucSaldo  = v_saldo.
/*        CAMPO0 = Sal_Cuenta.cuenta + ";" + */
/*                 Cuentas.nombre + ";" +    */
/*                 STRING(v_saldo).          */
   END.
/*    ELSE                                                         */
/*        CAMPO0 = Sal_Cuenta.cuenta + ";" +                       */
/*            "- CUENTA NO ENCONTRADA EN TABLA CUENTAS - " + ";" + */
/*            STRING(v_saldo).                                     */

/*     PUT UNFORMATTED  */
/*     CAMPO0 AT 1.     */
    ASSIGN v_saldo = 0. /* giocam 01/23/08 */
 END.


 OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSPUC.csv") NO-CONVERT.
 EXPORT DELIMITER ";" "Cuenta" "Nombre cuenta" "Saldo".
 FOR EACH tpuc BY pucCta:
    EXPORT DELIMITER ";" pucCta pucNomCta pucSaldo.
 END.
 OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Puc2 wWin 
PROCEDURE Puc2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SESPuc.r (INPUT DATE(Fec_Corte:SCREEN-VALUE IN FRAME Frm0)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RentaFija wWin 
PROCEDURE RentaFija :
/* Purpose:  Genera archivo plano de ahorro para SUPERSOLIDARIA */
DEFINE VAR v_clase AS integer.
DEFINE VAR v_tipo  AS INTEGER.
 Assign Prc = 0
        Acum = 0
        Xacum = 0.
 OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "RentaFija.csv") NO-CONVERT.
 FOR EACH Inversion_Sdos:
 Assign acum = acum + Inversion_Sdos.Sdo_Actual.
 CASE Inversion_Sdos.cod_producto:
     WHEN 1 THEN
        ASSIGN v_clase = 1
               v_tipo = 1.
     WHEN 2 THEN
        ASSIGN v_clase = 2
               v_tipo = 2.
     WHEN 3 THEN 
        ASSIGN v_clase = 3
               v_tipo = 3.
     WHEN 3 THEN 
        ASSIGN v_clase = 4
               v_tipo = 4.
     WHEN 3 THEN 
        ASSIGN v_clase = 5
               v_tipo = 5.
     WHEN 3 THEN 
        ASSIGN v_clase = 6
               v_tipo = 6.
     WHEN 3 THEN 
        ASSIGN v_clase = 7
               v_tipo = 7.
     END CASE.

     
 CAMPO0 =     STRING(v_clase)                           + "," +
              STRING(Inversion_Sdos.Cuenta_Contab)      + "," +
              STRING(Inversion_Sdos.Nit_Emisor)         + "," +
              STRING(Inversion_Sdos.Nombre_Emisor)      + "," +
              STRING(v_tipo)                            + "," +
              STRING(Inversion_Sdos.Nro_Titulo)         + "," + 
              STRING(Inversion_Sdos.Fec_Apertura)       + "," +
              STRING(Inversion_Sdos.Fec_Apertura)       + "," +
              STRING(Inversion_Sdos.Fec_Vcto)           + "," +
              STRING(Inversion_Sdos.Fec_Apertura)       + "," +
              STRING(Inversion_Sdos.Valor_Unidad)       + "," +
              STRING(Inversion_Sdos.VrInvers_Inic)      + "," +
              "MODALIDAD DE PAGO"                       + "," +
              STRING(Inversion_Sdos.LiqInt_Dias)        + "," +
              STRING(Inversion_Sdos.Interes_Causado)    + "," +
              STRING(Inversion_Sdos.Interes_Recibido)   + "," +
              STRING(Inversion_Sdos.Sdo_Actual)         + "," +
              STRING(Inversion_Sdos.Vr_Provision)       + "," +
              STRING(Inversion_Sdos.Tasa_NomiAnual)     + "," +
              "tasa Efectiva"                           + "," +
              "Valor Actual"                            + "," +
              STRING(Inversion_Sdos.Categoria).
         PUT UNFORMATTED
         CAMPO0 AT 1.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\Info_Utrahuilca\" + "TOTREFJA.txt") NO-CONVERT.
display "Total RENTAFIJA " Acum.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RentaVariable wWin 
PROCEDURE RentaVariable :
/* Purpose:  Genera archivo plano de ahorro para SUPERSOLIDARIA */
DEFINE VAR v_clase AS integer.
DEFINE VAR v_tipo  AS INTEGER.
 Assign Prc = 0
        Acum = 0
        Xacum = 0.
 OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "RentaVble.csv") NO-CONVERT.
 FOR EACH Inversion_Sdos:
 Assign acum = acum + Inversion_Sdos.Sdo_Actual.
 CASE Inversion_Sdos.cod_producto:
     WHEN 1 THEN
        ASSIGN v_clase = 1
               v_tipo = 1.
     WHEN 2 THEN
        ASSIGN v_clase = 2
               v_tipo = 2.
     WHEN 3 THEN 
        ASSIGN v_clase = 3
               v_tipo = 3.
     WHEN 3 THEN 
        ASSIGN v_clase = 4
               v_tipo = 4.
     WHEN 3 THEN 
        ASSIGN v_clase = 5
               v_tipo = 5.
     WHEN 3 THEN 
        ASSIGN v_clase = 6
               v_tipo = 6.
     WHEN 3 THEN 
        ASSIGN v_clase = 7
               v_tipo = 7.
     END CASE.

     
 CAMPO0 =     STRING(v_clase)                           + ";" +
              STRING(Inversion_Sdos.Cuenta_Contab)      + ";" +
              STRING(Inversion_Sdos.Nit_Emisor)         + ";" +
              STRING(Inversion_Sdos.Nombre_Emisor)      + ";" +
              STRING(v_tipo)                            + ";" +
              "VALOR MERCADO"                           + ";" +
              STRING(Inversion_Sdos.Fec_Apertura)       + ";" +
              STRING(Inversion_Sdos.VrInvers_Inic)      + ";" +
              "VALORIZACION"                            + ";" +
              STRING(Inversion_Sdos.Vr_Provision)       + ";" +
              "GRAVAMEN".
         PUT UNFORMATTED
         CAMPO0 AT 1.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\Info_Utrahuilca\" + "TOTREVBE.txt") NO-CONVERT.
display "Total RENTA VARIABLE " Acum.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RiesgoLiquidez wWin 
PROCEDURE RiesgoLiquidez :
DEFINE VAR NomPdt AS CHARACTER FORMAT "X(15)".
DEFINE VAR Prom   AS DECIMAL FORMAT ">>.99".
DEFINE VAR Tot    AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
DEFINE VAR n_cod_renglon AS DECIMAL.
DEFINE VAR v_saldo  AS decimal.
DEFINE VAR n_codigo AS DECIMAL.
DEFINE VAR n_cod    AS DECIMAL.
DEFINE VAR banda1   AS DECIMAL.
DEFINE VAR banda2   AS DECIMAL.
DEFINE VAR banda3   AS DECIMAL.
DEFINE VAR banda4   AS DECIMAL.
DEFINE VAR banda5   AS DECIMAL.
DEFINE VAR banda6   AS DECIMAL.
DEFINE VAR banda7   AS DECIMAL.
DEFINE VAR banda8   AS DECIMAL.
DEFINE VAR v_cfondoliquidezdv AS DECIMAL INITIAL 2105.
DEFINE VAR v_cartera AS DECIMAL INITIAL 1405.
DEFINE VAR v_termino AS DECIMAL INITIAL 2110.

ASSIGN FECHA1 = Fec_Corte.
DO I = 1 TO 7 BY 1:
  CASE i:
        WHEN 1 THEN
                run HallaBrecha ((FECHA1 - 999),(FECHA1 + 30)).
        WHEN 2 THEN  
                run HallaBrecha ((FECHA1 + 31),(FECHA1 + 60)).
        WHEN 3 THEN  
                run HallaBrecha ((FECHA1 + 61),(FECHA1 + 90)).
        WHEN 4 THEN
                run HallaBrecha ((FECHA1 + 91),(FECHA1 + 180)).
        WHEN 5 THEN  
                run HallaBrecha ((FECHA1 + 181),(FECHA1 + 270)).
        WHEN 6 THEN  
                run HallaBrecha ((FECHA1 + 271),(FECHA1 + 360)).
        WHEN 7 THEN  
                run HallaBrecha ((FECHA1 + 361),(FECHA1 + 9999)).
   END CASE.
END.

/******/
/* AHORROS A LA VISTA*/
/*******/
 ASSIGN Xsumavalor = 0
        xsumatasa  = 0
        xtitulos   = 0.

for each ahorros WHERE AHORROS.ESTADO = 1 AND SDO_DISPONIBLE > 0 AND AHORROS.COD_Ahorro = 2:
    ASSIGN XSUMAVALOR = XSUMAVALOR + SDO_DISPONIBLE + SDO_CANJE + int_pagar
           XSUMATASA  = XSUMATASa + TASA
           XTITULOS   = XTITULOS + 1.
end.
create brecha.
assign brecha.producto = 3
       brecha.Banda    = i
       brecha.valor    = xsumavalor
       brecha.tasa     = 0.20 * XTITULOS
       brecha.Nro      = xtitulos
       brecha.nombre   = "DEPOSITOS DE AHORRO"
       brecha.captura  = "2"
       brecha.renglon  = "005"
       brecha.cuenta   =  "21050501".
OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "RiesgoLiquidez.csv") NO-CONVERT.
n_cod = 0.
FOR EACH brecha BREAK BY Brecha.Producto BY Banda:
  IF FIRST-OF(Brecha.Producto) THEN
  DO:
      RUN hallarsaldo(INPUT DECIMAL(Brecha.cuenta),1,integer(length(brecha.cuenta)),year(FECHA1),OUTPUT v_saldo).
  END.
  CASE Brecha.banda:
      WHEN 1 THEN 
          banda1 = Brecha.valor.
      WHEN 2 THEN 
          banda2 = Brecha.valor.
      WHEN 3 THEN 
          banda3 = Brecha.valor.
      WHEN 4 THEN 
          banda4 = Brecha.valor.
      WHEN 5 THEN 
          banda5 = Brecha.valor.
      WHEN 6 THEN 
          banda6 = Brecha.valor.
      WHEN 7 THEN 
          banda7 = Brecha.valor.
      WHEN 8 THEN 
          banda8 = Brecha.valor.
  END CASE.
  IF last-OF(Brecha.Producto) THEN
  DO:
      CAMPO0 = Brecha.Captura             + ";" +
               Brecha.renglon             + ";" +
               Brecha.Nombre              + ";" +
               STRING(v_saldo)            + ";" +
               STRING(banda1)             + ";" +
               STRING(banda2)             + ";" +
               STRING(banda3)             + ";" +
               STRING(banda4)             + ";" +
               STRING(banda5)             + ";" +
               STRING(banda6)             + ";" +
               STRING(banda7)             + ";" +
               STRING(banda8).
    
   PUT UNFORMATTED CAMPO0 AT 1.
  END.
 END.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSAhorro wWin 
PROCEDURE SSAhorro :
Prc = 0.

EMPTY TEMP-TABLE SSAho.

FOR EACH rep_Ahorros WHERE rep_Ahorros.Estado = 1
                       AND rep_ahorros.tip_ahorro < 4
                       AND rep_ahorros.cod_ahorro <> 8
                       AND rep_ahorros.sdo_disponible > 0
                       AND rep_ahorros.fecCorte = fec_corte NO-LOCK:
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = rep_Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Ahorro THEN
        NEXT.

    ASSIGN Prc = Prc + 1
           Ced = rep_Ahorros.Nit.

    DISPLAY Prc Ced WITH FRAME FRM0.

    CREATE SSAho.
    
    CASE Pro_Ahorros.Tip_Ahorro:
        WHEN 1 THEN
            ASSIGN SSAho.SA_NomDep = "A la Vista"
                   SSAho.SA_TipAho = "1"
                   SSAho.SA_CodCon = 210505
                   SSAho.SA_Plazo = 0
                   SSAho.SA_Amorti = 1
                   SSAho.SA_TasNom = round(rep_ahorros.tasa,2).
        
        WHEN 2 THEN DO:
            ASSIGN SSAho.SA_NomDep = "Contractual"
                   SSAho.SA_TipAho = "3"
                   SSAho.SA_CodCon = 212505
                   SSAho.SA_Plazo = rep_Ahorros.Plazo
                   SSAho.SA_Amorti = 30
                   SSAho.SA_TasNom = round(rep_ahorros.tasa,2).

            IF rep_ahorros.cod_ahorro = 3 THEN DO:
                SSAho.SA_CodCon = 213010.
                SSAho.SA_NomDep = "Permanente".
                SSAho.SA_TipAho = "4".
                SSAho.SA_Plazo = 0.
            END.
        END.
        
        WHEN 3 THEN DO:
            FIND FIRST CortoLargo WHERE CortoLargo.Clase_Producto EQ 1
                                    AND CortoLargo.Cod_Producto EQ rep_Ahorros.Cod_Ahorro
                                    AND CortoLargo.Plazo_Inicial le rep_Ahorros.Plazo
                                    AND CortoLargo.Plazo_Final ge rep_Ahorros.Plazo NO-LOCK NO-ERROR.
            IF AVAILABLE CortoLargo THEN
                SSAho.SA_CodCon = DECIMAL(SUBSTRING(CortoLargo.Cta_AsoAd,1,6)).
            ELSE
                SSAho.SA_CodCon = 211005.

            ASSIGN SSAho.SA_NomDep = "CDAT"
                   SSAho.SA_TipAho = "2".

            CASE rep_Ahorros.Per_Liquidacion:
                WHEN 0 THEN
                    ASSIGN SSAho.SA_Plazo = 1
                           SSAho.SA_Amorti = 1
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
                
                WHEN 1 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = 7
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
                
                WHEN 2 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = 10
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
                
                WHEN 3 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = 15
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
                
                WHEN 4 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = 30
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
                
                WHEN 5 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = 60
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.

                WHEN 6 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = 90
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
                
                WHEN 7 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo * 120
                           SSAho.SA_Amorti = 120
                           SSAho.SA_TasNom = rep_Ahorros.Tasa * 3.
                
                WHEN 8 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo * 180
                           SSAho.SA_Amorti = 180
                           SSAho.SA_TasNom = rep_Ahorros.Tasa * 2.

                WHEN 9 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo * 360
                           SSAho.SA_Amorti = 360
                           SSAho.SA_TasNom = rep_Ahorros.Tasa * 1.
                
                WHEN 10 THEN
                    ASSIGN SSAho.SA_Plazo = rep_Ahorros.Plazo
                           SSAho.SA_Amorti = rep_Ahorros.plazo
                           SSAho.SA_TasNom = rep_Ahorros.Tasa.
            END CASE.

            SSAho.SA_TasNom = round(SSAho.SA_TasNom,2).

            RUN NVEF IN W_ManFin (INPUT rep_Ahorros.Tasa / 1200,
                                  INPUT 12,
                                  OUTPUT SSAho.SA_TasEfe) NO-ERROR.

            SSAho.SA_TasEfe = round(SSAho.SA_TasEfe,2).
            SSAho.SA_TasEfe = (SSAho.SA_TasEfe * 100).

            IF SSAho.SA_Amorti GT rep_Ahorros.Plazo THEN
                SSAho.SA_Amorti = rep_Ahorros.Plazo.
        END.

        WHEN 4 THEN
            ASSIGN SSAho.SA_NomDep = "Permanente"
                   SSAho.SA_TipAho = "1"
                   SSAho.SA_CodCon = 213005
                   SSAho.SA_Plazo = 999
                   SSAho.SA_Amorti = 1
                   SSAho.SA_TasNom = (0.65 * 12).
    END CASE.

    IF SSAho.SA_Plazo > 9998 THEN
        SSAho.SA_Plazo = 9998.

    IF SSAho.SA_TasEfe EQ ? THEN
        SSAho.SA_TasEfe = SSAho.SA_TasNom.

    IF ssaho.sa_tasefe LT SSAho.SA_TasNom THEN
        ssaho.sa_tasefe = SSAho.SA_TasNom.

    ASSIGN SSAho.SA_TipIde = "C"
           SSAho.SA_CedNit = (rep_Ahorros.Nit).

    IF rep_Ahorros.Fec_Apertura EQ ? THEN
        SSAho.SA_FecApe = "01/01/1990".
    ELSE
        SSAho.SA_FecApe = STRING(rep_Ahorros.Fec_Apertura,"99/99/9999").

    IF rep_Ahorros.Fec_Vencimiento EQ ? THEN
        SSAho.SA_FecVen = STRING(w_fecha,"99/99/9999").
    ELSE
        SSAho.SA_FecVen = STRING(rep_Ahorros.Fec_Vencimiento,"99/99/9999").

    IF rep_Ahorros.fec_vencimiento < w_fecha THEN
        SSAho.SA_FecVen = STRING(w_fecha,"99/99/9999").

    IF SSAho.SA_TipAho = "1" OR SSAho.SA_TipAho = "4" THEN
        SSAho.SA_FecVen = "".

    SSAho.SA_Modali = 2.
    
    SSAho.SA_Saldo = rep_Ahorros.Sdo_Disponible + rep_ahorros.sdo_canje + rep_ahorros.INT_causado.
    
    SSAho.SA_ValIni = rep_Ahorros.Sdo_Disponible + sdo_canje.
    SSAho.SA_NumCta = DECIMAL(rep_Ahorros.Cue_Ahorros).
    SSAho.SA_intpag = rep_Ahorros.Int_pagar + rep_Ahorros.Int_causado.

    FIND FIRST Clientes WHERE Clientes.Nit EQ rep_Ahorros.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        SSaho.SA_NomNit = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

        IF Clientes.Cod_Anterior NE "" THEN
            SSAho.SA_CedNit = Clientes.nit.
        ELSE
            SSAho.SA_CedNit = Clientes.Nit.

        CASE Clientes.Tipo_Identificacion:
            WHEN "C.C" THEN SSAho.SA_TipIde = "C".
            WHEN "C.E" THEN SSAho.SA_TipIde = "E".
            WHEN "NIT" THEN SSAho.SA_TipIde = "N".
            OTHERWISE SSAho.SA_TipIde = "C".
        END CASE.
    END.
    ELSE
        SSaho.SA_NomNit = "NOMBRE NO REGISTRADO".

    /* Exento GMF */
    IF rep_Ahorros.Exento_3xm THEN
        SA_Exgmf = "1".
    ELSE
        SA_Exgmf = "0".

    IF rep_Ahorros.Exento_3xm AND rep_Ahorros.FecIniExentoGMF NE ? THEN
        SA_FecExgmf = STRING(FecIniExentoGMF, "99/99/9999").
    ELSE
        SA_FecExgmf = "".

    SA_Amorti = 1. /* Por solicitud de Liliana Alzate 07/12/2009 */
END.

/*OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSAho" + STRING(YEAR(DATE(Fec_Corte:SCREEN-VALUE))) + "-" + STRING(MONTH(DATE(Fec_Corte:SCREEN-VALUE))) + ".txt").
    PUT UNFORMATTED "TipoDto,Cedula,Codigo,Nombre Ahorro,Tipo Ahorro,Amortizacion,Fecha Apertura,Plazo,Fecha Vencimiento,Modalidad,Tasa Nominal,Tasa Efectiva,Intereses,Saldo Disponible,Valor Inicial,Cuenta" SKIP(0).

    FOR EACH SSAho:
        CAMPO0 = SSAho.SA_TipIde + CHR(9) +
                 STRING(SSAho.SA_CedNit) + CHR(9) +
                 STRING(SSAho.SA_CodCon) + CHR(9) +
                 SSAho.SA_NomDep + CHR(9) +
                 SSAho.SA_TipAho + CHR(9) +
                 STRING(SSAho.SA_Amorti) + CHR(9) +
                 SSAho.SA_FecApe + CHR(9) +
                 STRING(SSAho.SA_Plazo) + CHR(9) +
                 SSAho.SA_FecVen + CHR(9) +
                 STRING(SSAho.SA_Modali) + CHR(9) +
                 REPLACE(STRING(SSAho.SA_TasNom), ",", ".") + CHR(9) +
                 REPLACE(STRING(SSAho.SA_TasEfe), ",", ".") + CHR(9) +
                 STRING(SSAho.SA_INTPAG) + CHR(9) +
                 STRING(SSAho.SA_Saldo) + CHR(9) +
                 STRING(SSAho.SA_ValIni) + CHR(9) +
                 STRING(SSAho.SA_NumCta) + CHR(9) +
                 STRING(SA_Exgmf) + CHR(9) +
                 STRING(SA_FecExgmf) + CHR(9) +
                 "1" + CHR(9) +
                 "0".

        PUT UNFORMATTED CAMPO0 AT 1.
    END.
OUTPUT CLOSE.*/

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSAho" + STRING(YEAR(DATE(Fec_Corte:SCREEN-VALUE))) + "-" + STRING(MONTH(DATE(Fec_Corte:SCREEN-VALUE))) + ".csv").
    PUT UNFORMATTED "TipoDto;Cedula;Codigo;Nombre Ahorro;Tipo Ahorro;Amortizacion;Fecha Apertura;Plazo;Fecha Vencimiento;Modalidad;Tasa Nominal;Tasa Efectiva;Intereses;Saldo Disponible;Valor Inicial;Cuenta;exentaGMF;FechaExencionGMF;Estado;CuentaBajoMonto;Cotitulares" SKIP(0).

    FOR EACH SSAho:
        CAMPO0 = SSAho.SA_TipIde + ";" +
                 STRING(SSAho.SA_CedNit) + ";" +
                 STRING(SSAho.SA_CodCon) + ";" +
                 SSAho.SA_NomDep + ";" +
                 SSAho.SA_TipAho + ";" +
                 STRING(SSAho.SA_Amorti) + ";" +
                 SSAho.SA_FecApe + ";" +
                 STRING(SSAho.SA_Plazo) + ";" +
                 SSAho.SA_FecVen + ";" +
                 STRING(SSAho.SA_Modali) + ";" +
                 REPLACE(STRING(SSAho.SA_TasNom), ",", ".") + ";" +
                 REPLACE(STRING(SSAho.SA_TasEfe), ",", ".") + ";" +
                 STRING(SSAho.SA_INTPAG) + ";" +
                 STRING(SSAho.SA_Saldo) + ";" +
                 STRING(SSAho.SA_ValIni) + ";" +
                 STRING(SSAho.SA_NumCta) + ";" +
                 STRING(SA_Exgmf) + ";" +
                 STRING(SA_FecExgmf) + ";" +
                 "1" + ";" +
                 "0" + ";" +
                 "0".

        PUT UNFORMATTED CAMPO0 AT 1.
    END.
OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSAportes wWin 
PROCEDURE SSAportes :
DEFINE VARIABLE tnit AS CHARACTER NO-UNDO.
DEFINE BUFFER ttAportes FOR rep_ahorros.
DEFINE VAR vPromedio AS DECIMAL.
DEFINE VAR cont AS INTEGER.

prc = 0.

EMPTY TEMP-TABLE SSApo.

FOR EACH rep_Ahorros WHERE rep_ahorros.tip_ahorro EQ 4
                       AND rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje > 0
                       AND rep_ahorros.fecCorte = fec_corte NO-LOCK BREAK BY rep_ahorros.nit:
    ASSIGN Prc = Prc + 1
           Ced = rep_Ahorros.Nit.

    DISPLAY Prc Ced WITH FRAME FRM0.

    FIND FIRST clientes WHERE clientes.nit EQ rep_ahorros.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        tnit = clientes.nit.
    ELSE DO:
        MESSAGE "La cédula" rep_ahorros.nit "no fue encontrada en la tabla de Clientes." SKIP
                "no se exportará este registro."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        NEXT.
    END.

    IF FIRST-OF(rep_ahorros.nit) THEN DO:
        FIND FIRST ssapo WHERE ssapo.ss_numnit = tnit NO-ERROR.
        IF NOT AVAILABLE(ssapo) THEN DO:
            CREATE SSApo.
            ASSIGN SSApo.SS_NumNit = rep_ahorros.nit.

            CASE Clientes.Tipo_Identificacion:
                WHEN "C.C" THEN ASSIGN SSApo.SS_TipIde = "C".
                WHEN "C.E" THEN ASSIGN SSApo.SS_TipIde = "E".
                WHEN "NIT" THEN ASSIGN SSApo.SS_TipIde = "N".
                OTHERWISE ASSIGN SSApo.SS_TipIde = "C".
            END CASE.
        END.
    END.

    SSApo.SS_Saldo = SSApo.SS_Saldo + rep_Ahorros.Sdo_Disponible + rep_Ahorros.sdo_canje.

    CASE rep_ahorros.per_deduccion:
        WHEN 1 THEN SSApo.SS_Cuota = SSApo.SS_Cuota + (rep_ahorros.cuota * 4).
        WHEN 2 THEN SSApo.SS_Cuota = SSApo.SS_Cuota + (rep_ahorros.cuota * 3).
        WHEN 3 THEN SSApo.SS_Cuota = SSApo.SS_Cuota + (rep_ahorros.cuota * 2).
        WHEN 4 THEN SSApo.SS_Cuota = SSApo.SS_Cuota + rep_ahorros.cuota.
    END CASE.

    cont = 0.
    vPromedio = 0.

    FOR EACH ttAportes WHERE ttAportes.nit = rep_ahorros.nit
                         AND ttAportes.cue_ahorros = rep_ahorros.cue_ahorros NO-LOCK:
        vPromedio = vPromedio + ttAportes.sdo_disponible + ttAportes.sdo_canje.
        cont = cont + 1.
    END.

    SSApo.saldoPromedio = ROUND(vPromedio / cont,0).

    FOR EACH mov_ahorros WHERE mov_ahorros.nit = rep_ahorros.nit
                           AND mov_ahorros.cue_ahorros = rep_ahorros.cue_ahorros
                           AND mov_ahorros.fecha <= fec_corte NO-LOCK BY mov_ahorros.fecha:
        FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion
                               AND operacion.tipo_operacion = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE operacion THEN DO:
            SSApo.fecUltTrans = mov_ahorros.fecha.
            LEAVE.
        END.
    END.

    IF SSApo.fecUltTrans = ? THEN
        SSApo.fecUltTrans = rep_ahorros.fec_ultTrans.
END.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSApo" + STRING(YEAR(DATE(Fec_Corte:SCREEN-VALUE))) + "-" + STRING(MONTH(DATE(Fec_Corte:SCREEN-VALUE))) + ".csv").
    PUT UNFORMATTED "Tipo Documento;Documento;Saldo Total;Cuota Mensual;Saldo Aportes Ordinarios;Saldo Aportes Extraordinarios;Valor Revalorización;Monto Promedio;Fecha Último Pago" SKIP(0).

    FOR EACH SSApo:
        CAMPO0 = SS_TipIde + ";" +
                 STRING(SS_NumNit) + ";" +
                 STRING(SS_Saldo) + ";" +
                 STRING(SS_Cuota) + ";" +
                 STRING(SS_Saldo) + ";" +
                 "0" + ";" +
                 "0" + ";" +
                 STRING(SSApo.saldoPromedio) + ";" +
                 STRING(SSApo.fecUltTrans,"99/99/9999").

        PUT UNFORMATTED CAMPO0 AT 1.
    END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSCliente wWin 
PROCEDURE SSCliente :
DEFINE VAR cedulaConv AS DECIMAL.
DEFINE VAR dv AS INTEGER.

OUTPUT TO VALUE (W_Pathspl + "\" + "IndividualClientesSupersolidaria" + STRING(YEAR(DATE(Fec_Corte:SCREEN-VALUE IN FRAME FRM0))) + "-" + STRING(MONTH(DATE(Fec_Corte:SCREEN-VALUE))) + ".csv").
EMPTY TEMP-TABLE tClientes.

PUT UNFORMATTED "Tipo Doc;Documento;Primer Apellido;Segundo Apellido;Nombres;Fecha Ingreso;Telefono;Direccion;Asociado;Activo;Actividad Economica;Cod Municipio;Correo;Genero;Empleado;Tipo Contrato;Nivel Escolaridad;Estrato;Ingresos;Nacimiento;Estado Civil;Mujer Cabeza;Ocupacion;Sector Economico;Jornada Laboral;fechaRetiro;AsistioUltimaAsamblea" SKIP(0).

/* Asociados */
FOR EACH clientes WHERE clientes.nit <> ""
                    AND INDEX(clientes.nit,"_") = 0
                    AND clientes.nit <> "001"
                    AND clientes.nit <> "002"
                    AND clientes.nit <> "003"
                    AND clientes.nit <> "004"
                    AND clientes.tipo_vinculo <> 5 NO-LOCK:
    RUN I_Validaciones. /* Validaciones */

    IF clientes.tipo_vinculo <> 5 THEN DO:
        FIND FIRST rep_ahorros WHERE rep_ahorros.fecCorte = fec_corte
                                 AND rep_ahorros.nit = clientes.nit
                                 AND rep_ahorros.sdo_disponible > 0
                                 AND rep_ahorros.estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE rep_ahorros THEN DO:
            FIND FIRST rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                                      AND rep_creditos.nit = clientes.nit
                                      AND rep_creditos.estado = 2 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE rep_creditos THEN
                NEXT.
        END.
    END.

    CREATE tClientes.
    RUN I_TipoId. /* Tipo identificación */
    RUN I_numId. /* Número de identificación */
    RUN I_apellidos_yNombres. /* Apellidos y Nombres */

    

    /* Fecha de ingreso */
    IF Clientes.Fec_Ingreso NE ? THEN
        tFecIng = STRING(clientes.fec_ingreso,"99/99/9999").
    ELSE
        tFecIng = "01/11/2008".

    /* Teléfono */
    IF Clientes.Tel_Residencia <> "" THEN
        tClientes.tTelefono = Clientes.Tel_Residencia.
    ELSE DO:
        IF Clientes.Tel_Comercial <> "" THEN
            tClientes.tTelefono = Clientes.Tel_Comercial.
        ELSE
            tClientes.tTelefono = "No asignado".
    END.

    /* Dirección */
    IF Clientes.Dir_Residencia <> "" THEN
        tClientes.tDireccion = Clientes.Dir_Residencia.
    ELSE DO:
        IF Clientes.Dir_Comercial <> "" THEN
            tClientes.tDireccion = Clientes.Dir_Comercial.
        ELSE
            tClientes.tDireccion = "No asignada".
    END.

    /* Asociado - Estado */
    tClientes.tAsociado = "1".
    tClientes.tActivo = "1".

    IF clientes.tipo_vinculo = 5 THEN DO:
        tClientes.tAsociado = "4".

        IF clientes.estado <> 1 THEN
            tClientes.tActivo = "0".
    END.

    /* Actividad */
    tActividad = "0000".

    /* CodMunicipio */
    IF SUBSTRING(Clientes.Lugar_Residencia,1,5) = "" OR SUBSTRING(Clientes.Lugar_Residencia,1,5) = "0" OR SUBSTRING(Clientes.Lugar_Residencia,1,5) = ? THEN
        tClientes.tCodMun = "11001".
    ELSE
        tClientes.tCodMun = SUBSTRING(Clientes.Lugar_Residencia,1,5).

    /* Género */
    IF tClientes.tId = "N" THEN
        tClientes.tGenero = "3".
    ELSE DO:
        tClientes.tGenero = string(clientes.sexo).

        /* Validación de nùmeros de cédula */
        cedulaConv = DECIMAL(clientes.nit) NO-ERROR.

        IF cedulaConv > 0 THEN DO:
            IF cedulaConv <= 20000000 OR (cedulaConv > 60000000 AND cedulaConv < 100000000) THEN
                tClientes.tGenero = "1".
            ELSE DO:
                IF cedulaConv > 20000000 AND cedulaConv <= 60000000 THEN
                    tClientes.tGenero = "2".
            END.
        END.
    END.

    /* Empleado */
    tEmpleado = "0".

    /* Tipo de contrato */
    tContrato = "0".

    /* Nivel de escolaridad */
    IF tClientes.tId = "N" THEN
        tClientes.tNivelEsc = "0".
    ELSE DO:
        CASE clientes.Niv_Educativo:
            WHEN "Ninguno" THEN tClientes.tNivelEsc = "0".
            WHEN "Primaria" THEN tClientes.tNivelEsc = "1".
            WHEN "Secundaria" THEN tClientes.tNivelEsc = "2".
            WHEN "Tecnico" THEN tClientes.tNivelEsc = "3".
            WHEN "Tecnologo" THEN tClientes.tNivelEsc = "4".
            WHEN "Profesional" THEN tClientes.tNivelEsc = "5".
            WHEN "Posgrado" THEN tClientes.tNivelEsc = "6".
            WHEN "Especializacion" THEN tClientes.tNivelEsc = "6".
            WHEN "Maestria" THEN tClientes.tNivelEsc = "7".
            WHEN "Doctorado" THEN tClientes.tNivelEsc = "8".
            OTHERWISE tClientes.tNivelEsc = "0".
        END CASE.
    END.

    /* Estrato */
    IF tClientes.tId = "N" THEN
        tClientes.tEstrato = "0".
    ELSE
        tClientes.tEstrato = STRING(clientes.estrato).

    /* Nivel ingresos */
    tTotIng = Clientes.Salario + Clientes.Ing_arriendos + Clientes.Ing_financieros + Clientes.Ing_Honorarios + Clientes.Ing_Otros.

    IF DECIMAL(SalMin:SCREEN-VALUE) > 0 THEN
        tNSalMin = tTotIng / DECIMAL(SalMin:SCREEN-VALUE).
    
    IF tClientes.tId <> "N" THEN DO:
        IF tNSalMin = 0 THEN tIngresos = "1".
        ELSE IF tNSalMin <= 1 THEN tIngresos = "2".
        ELSE IF tNSalMin <= 2 THEN tIngresos = "3".
        ELSE IF tNSalMin <= 3 THEN tIngresos = "4".
        ELSE IF tNSalMin <= 4 THEN tIngresos = "5".
        ELSE IF tNSalMin <= 6 THEN tIngresos = "6".
        ELSE IF tNSalMin <= 8 THEN tIngresos = "7".
        ELSE IF tNSalMin <= 11 THEN tIngresos = "8".
        ELSE IF tNSalMin <= 17 THEN tIngresos = "9".
        ELSE IF tNSalMin <= 24 THEN tIngresos = "10".
        ELSE IF tNSalMin <= 48 THEN tIngresos = "11".
        ELSE tIngresos = "12".
    END.
    ELSE DO:
        IF tNSalMin <= 10 THEN tIngresos = "1".
        ELSE IF tNSalMin <= 50 THEN tIngresos = "2".
        ELSE IF tNSalMin <= 150 THEN tIngresos = "3".
        ELSE IF tNSalMin <= 500 THEN tIngresos = "4".
        ELSE IF tNSalMin <= 1000 THEN tIngresos = "5".
        ELSE IF tNSalMin <= 5000 THEN tIngresos = "6".
        ELSE IF tNSalMin <= 10000 THEN tIngresos = "7".
        ELSE IF tNSalMin <= 50000 THEN tIngresos = "8".
        ELSE IF tNSalMin <= 100000 THEN tIngresos = "9".
        ELSE IF tNSalMin <= 500000 THEN tIngresos = "10".
        ELSE IF tNSalMin <= 1000000 THEN tIngresos = "11".
        ELSE tIngresos = "12".
    END.

    /* Fecha de nacimiento */
    IF Clientes.Fec_Nacimiento NE ? THEN
        tFecNac = STRING(clientes.fec_nacimiento,"99/99/9999").
    ELSE
        tFecNac = "01/04/1976".

    /* Estado civil */
    IF tClientes.tId <> "N" THEN DO:
        CASE clientes.est_civil:
            WHEN "Soltero" THEN tEstCivil = "1".
            WHEN "Casado" THEN tEstCivil = "2".
            WHEN "Union Libre" THEN tEstCivil = "3".
            WHEN "Separado" THEN tEstCivil = "4".
            WHEN "Divorciado" THEN tEstCivil = "5".
            WHEN "Viudo" THEN tEstCivil = "6".
            OTHERWISE tEstCivil = "1".
        END CASE.
    END.
    ELSE
        tEstCivil = "0".

    /* Mujer cabeza de familia */
    IF tGenero = "2" AND clientes.Id_Cab_Familia = YES THEN
        tMujer = "1".
    ELSE
        tMujer = "0".

    /* Ocupación */
    CASE clientes.tipo_actividad:
        WHEN "Cesante" THEN tOcupacion = "6".
        WHEN "Empleado Público" THEN tOcupacion = "1".
        WHEN "Empleado Privado" THEN tOcupacion = "1".
        WHEN "Estudiante" THEN tOcupacion = "4".
        WHEN "Hogar" THEN tOcupacion = "5".
        WHEN "Independiente o Empleado Socio" THEN tOcupacion = "2".
        WHEN "Pensionado" THEN tOcupacion = "3".
        WHEN "Otro" OR WHEN "" THEN tOcupacion = "6".
    END CASE.

    tSector = "99".
    tJornada = "0".

    IF clientes.id_AsistioAsamblea THEN
        tAsistioAsamblea = 1.

    PUT UNFORMATTED
        tid ";"
        tClientes.tnit ";"
        clientes.Apellido1 ";"
        clientes.apellido2 ";"
        tclientes.tnombre ";"
        tFecIng ";"
        tTelefono ";"
        REPLACE(tDireccion,";",",") ";"
        tAsociad ";"
        tactivo ";"
        tActividad ";"
        tCodMun ";"
        REPLACE(clientes.Email,";",",") ";"
        tGenero ";"
        tEmpleado ";"
        tContrato ";"
        tNivelEsc ";"
        tEstrato ";"
        tIngresos ";"
        tFecNac ";"
        tEstCivil ";"
        tMujer ";"
        tOcupacion ";"
        tSector ";"
        tJornada ";"
        ""  ";"
        tAsistioAsamblea SKIP(0).
END.

/* Empleados */
FOR EACH clientes WHERE clientes.nit <> ""
                    AND INDEX(clientes.nit,"_") = 0
                    AND clientes.nit <> "001"
                    AND clientes.nit <> "002"
                    AND clientes.nit <> "003"
                    AND clientes.nit <> "004"
                    AND clientes.tipo_vinculo = 5 NO-LOCK:
    RUN I_validaciones. /* Validaciones */

    CREATE tClientes.
    RUN I_TipoId. /* Tipo de identificación */
    RUN I_NumId. /* Número de identificación */
    RUN I_Apellidos_yNombres. /* Apellidos y Nombres */
END.

OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE I_validaciones:
    IF clientes.fec_ingreso < clientes.fec_nacimiento THEN
        MESSAGE "El asociado" clientes.nit "-" clientes.nombre clientes.apellido1 clientes.apellido2 SKIP
                "tiene registrada una fecha de nacimiento posterior a la fecha de ingreso" SKIP
                "al Fondo. El archivo se genera pero presentará una inconsistencia en SICSES"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

PROCEDURE I_tipoId:
    CASE clientes.tipo_identificacion:
        WHEN "C.C" THEN tClientes.tId = "C".
        WHEN "C.E" THEN tClientes.tId = "E".
        WHEN "T.I" THEN tClientes.tId = "I".
        WHEN "R.C" THEN tClientes.tId = "R".
        OTHERWISE tClientes.tId = "C".
    END CASE.

END PROCEDURE.

PROCEDURE I_NumId:
    IF tClientes.tId <> "N" THEN
        tClientes.tNit = clientes.nit.
    ELSE DO:
        RUN digitoVerificacion.r(INPUT clientes.nit,
                                 OUTPUT dv).

        tClientes.tNit = SUBSTRING(clientes.nit,1,3) + "-" + SUBSTRING(clientes.nit,4,3) + "-" + SUBSTRING(clientes.nit,7) + "-" + STRING(dv).
    END.
END PROCEDURE.

PROCEDURE I_apellidos_yNombres:
    IF clientes.tipo_id <> "NIT" THEN DO:
        tClientes.apellido1 = clientes.apellido1.
        tClientes.apellido2 = clientes.apellido2.
    END.

    tClientes.tnombre = clientes.nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSCreditos wWin 
PROCEDURE SSCreditos :
DEFINE VARIABLE W_tasa AS DECIMAL.
DEFINE VARIABLE tNewFec AS CHARACTER NO-UNDO.
DEFINE VAR x_TasaUsura AS DECIMAL NO-UNDO.

/* oakley */

DEFINE VAR contRepetidos AS INTEGER INITIAL 1.

Prc = 0.

EMPTY TEMP-TABLE SSCre.
EMPTY TEMP-TABLE tMaxDias.

RUN HallaMaxDias.

/* Tasa de usura */
FIND FIRST indicadores WHERE indicadores.indicador EQ 1 AND indicadores.estado EQ 1 NO-LOCK NO-ERROR.
IF AVAILABLE indicadores THEN
    x_TasaUsura = Indicadores.Tasa.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.Fec_Desembolso LE Fec_Corte
                        /*AND SUBSTRING(rep_creditos.Cta_Contable,1,2) <> "16"*/
                        AND rep_creditos.sdo_capital > 0
                        AND rep_creditos.Estado EQ 2 NO-LOCK BY rep_creditos.nit:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ rep_creditos.cod_credito NO-LOCK NO-ERROR.

    ASSIGN Prc = Prc + 1
           Ced = rep_creditos.Nit.

    DISPLAY Prc Ced WITH FRAME FRM0.

    FIND FIRST SSCre WHERE SSCre.SC_pagare = rep_creditos.num_credito NO-ERROR.
    IF AVAILABLE SSCre THEN
        SSCre.SC_pagare = DECIMAL(STRING(SSCre.SC_pagare) + STRING(SSCre.SC_CedNit)).

    CREATE SSCre.
    W_tasa = rep_creditos.tasa.

    /* Controla la tasa de usura */
    IF rep_creditos.tasa GT x_TasaUsura THEN
        W_tasa = x_TasaUsura.

    ASSIGN SSCre.SC_TipIde = "C"
           SSCre.SC_CodCon = SUBSTRING(rep_creditos.Cta_Contable,1,6)
           SSCre.SC_TipCuo = 1
           SSCre.SC_FecApe = STRING(rep_creditos.Fec_Desembolso,"99/99/9999")
           SSCre.SC_ValCuo = rep_creditos.Cuota
           SSCre.SC_CuoPag = rep_creditos.Cuo_Pagadas
           SSCre.SC_Pagare = rep_creditos.Num_Credito
           SSCre.SC_SdoCap = rep_creditos.Sdo_Capital
           SSCre.SC_Monto = rep_creditos.Val_Desembolso
           SSCre.SC_SdoOtr = rep_creditos.costas + rep_creditos.polizas + rep_creditos.honorarios
           SSCre.SC_SdoInt = rep_creditos.Int_Corrientes + rep_creditos.Int_MorCobrar
           SSCre.SC_VlrPro = rep_creditos.Provision
           sscre.sc_vlrproint = rep_creditos.provision_interes
           SSCre.SC_ConInt = rep_creditos.INT_difcobro
           SSCre.SC_VlrExt = 0
           SSCre.SC_MesExt = 0
           SSCre.SC_Modali = 2
           SSCre.SC_TasNom = round(W_Tasa,2)
           SSCre.SC_PLAZO = 0.

    IF rep_creditos.plazo > 12 THEN
        SSCre.SC_PLAZO  = 2.
    ELSE
        SSCre.SC_PLAZO  = 1.

    IF SSCre.SC_Monto = 0 THEN
        SSCre.SC_Monto = rep_creditos.Sdo_Capital.

    FIND FIRST Clientes WHERE Clientes.Nit EQ rep_creditos.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN  DO:
        ASSIGN SSCre.SC_NomNit = TRIM(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).

        IF Clientes.Cod_Anterior NE "" THEN
            SSCre.SC_CedNit = (Clientes.nit).
        ELSE
            SSCre.SC_CedNit = (rep_creditos.Nit).
    END.
    ELSE
        ASSIGN SSCre.SC_NomNit = "NOMBRE NO REGISTRADO"
               SSCre.SC_CedNit = (rep_creditos.Nit).

    CASE Clientes.Tipo_Identificacion:
        WHEN "C.C" THEN SSCre.SC_TipIde = "C".
        WHEN "C.E" THEN SSCre.SC_TipIde = "E".
        WHEN "NIT" THEN SSCre.SC_TipIde = "N".
        OTHERWISE SSCre.SC_TipIde = "C".
    END CASE.

    /* Muestra el crédito como reestructurado wmr */
    IF rep_creditos.Reestructurado EQ 1 AND rep_creditos.Fec_Reestructurado NE ? THEN DO:
        ASSIGN SSCre.SC_Reest = 2
               SSCre.SC_FecReest = STRING(rep_creditos.Fec_Reestructurado,"99/99/9999").

        CASE rep_creditos.cod_CalificaReest:
            WHEN 0 OR
            WHEN 1 OR
            WHEN 2 THEN SSCre.SC_CatReest = "A".

            WHEN 3 THEN SSCre.SC_CatReest = "B".
            WHEN 4 THEN SSCre.SC_CatReest = "C".
            WHEN 5 THEN SSCre.SC_CatReest = "D".
            WHEN 6 THEN SSCre.SC_CatReest = "E".
            WHEN 7 THEN SSCre.SC_CatReest = "E".
            OTHERWISE SSCre.SC_CatReest = "A".
        END CASE.
    END.
    ELSE DO:
        FOR EACH amortizacion WHERE amortizacion.nit = rep_credito.nit
                                AND amortizacion.num_credito = rep_credito.num_credito
                                AND amortizacion.nro_cuota = 0
                                AND amortizacion.fec_pago <= fec_corte
                                AND amortizacion.fec_pago > rep_creditos.fec_desembolso NO-LOCK:
            SSCre.SC_Reest = 3.
            SSCre.numActualizaciones = SSCre.numActualizaciones + 1.
            SSCre.SC_FecReest = STRING(amortizacion.fec_pago,"99/99/9999").
            SSCre.SC_CatReest = 'A'.
        END.

        /*ASSIGN SSCre.SC_Reest = 0
               SSCre.SC_FecReest = ""
               SSCre.SC_CatReest = "".*/
    END.

    /* Para controlar que sólo se diligencie para vivienda */
    IF rep_creditos.cod_credito EQ 15 OR rep_creditos.cod_credito EQ 541 THEN
        ASSIGN SC_EntRedes = "0"
               SC_MarRedes = "0"
               SC_ClaseViv = "2"
               SC_SenalVIS = "0"
               SC_TipoViv  = "0"
               SC_SenalSub = "0".

    /* halla las garantias - wmr */
    FIND FIRST Garantias WHERE Garantias.Num_Credito EQ rep_creditos.num_credito
                           AND Garantias.Tip_Credito EQ rep_creditos.tip_credito
                           AND Garantias.Num_Solicitud EQ rep_creditos.num_solicitud
                           AND Garantias.estado EQ 1
                           AND Garantias.Tipo_Garantia LT 4 NO-LOCK NO-ERROR.
    IF AVAILABLE garantias THEN DO:
        SSCre.SC_garan = Garantias.Val_Bien.

        if garantias.Tipo_Garantia eq 1 THEN
            ASSIGN SSCre.SC_ClGar = "2"
                   SSCre.SC_ClaseViv = STRING(garantias.ClaseVivienda)
                   SSCre.SC_SenalVIS = STRING(garantias.VIS)
                   SSCre.SC_SenalSub = STRING(garantias.SenalSubsidio)
                   SSCre.SC_TipoViv = HallaRangoVivienda().
        else 
            if garantias.Tipo_Garantia eq 2 then
                SSCre.SC_ClGar = "3".
            else
                if garantias.Tipo_Garantia gt 2 then
                    SSCre.SC_ClGar = "1".
    END.
    ELSE DO:
        ASSIGN SSCre.SC_garan = 0
               SSCre.SC_ClGar = "4".

        FOR EACH rep_Ahorros WHERE rep_ahorros.fecCorte = fec_corte
                               AND rep_ahorros.Nit EQ rep_creditos.Nit
                               AND rep_ahorros.tip_ahorro = 4
                               AND rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje > 0
                               AND rep_ahorros.estado = 1 NO-LOCK: /*calcula el total de los aportes del cliente*/
            SSCre.SC_garan = SSCre.SC_garan + (rep_ahorros.Sdo_Disponible + rep_ahorros.Sdo_Canje).
        END.
    END.

    FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte = fec_corte
                           AND rep_ahorros.Nit EQ rep_creditos.Nit
                           AND rep_ahorros.tip_ahorro = 4
                           AND rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje > 0
                           AND rep_ahorros.estado = 1 NO-LOCK: /*calcula el total de los aportes del cliente*/
        SSCre.aportes = SSCre.aportes + rep_ahorros.Sdo_Disponible + rep_ahorros.Sdo_Canje.
    END.

    SSCre.codCredito = rep_creditos.cod_credito.

    IF rep_creditos.cod_credito NE 15 AND rep_creditos.cod_credito NE 541 THEN DO:
        ASSIGN SC_EntRedes = ""
               SC_MarRedes = ""
               SC_ClaseViv = ""
               SC_SenalVIS = ""
               SC_TipoViv  = ""
               SC_SenalSub = "".

        IF SSCre.SC_ClGar EQ "2" THEN
            ASSIGN SSCre.SC_ClGar = "1".
    END.

    /* Controla inconsistencias VIS y no VIS */
    IF rep_creditos.cod_credito EQ 15 OR rep_creditos.cod_credito EQ 541 THEN DO:
        IF SSCre.SC_ClGar = "2" AND SC_ClaseViv = "" THEN
            SC_ClaseViv = "2".

        IF SSCre.SC_ClGar = "2" AND SC_ClaseViv = "0" THEN
            SC_ClaseViv = "2".

        IF SSCre.SC_ClGar = "2" AND SC_SenalVIS = "" THEN
            SC_SenalVIS = "0".

        IF SSCre.SC_ClGar = "2" AND SC_SenalSub = "" THEN 
            SC_SenalSub = "0".

        IF SSCre.SC_ClGar = "2" AND SC_TipoViv = "" THEN
            SC_TipoViv  = "1".

        IF SSCre.SC_ClGar NE "2" AND SC_TipoViv = "0" THEN
            SC_TipoViv  = "1".
    END.

    /* halla tasa efec */
    RUN NVEF IN W_ManFin (INPUT W_tasa / 1200,
                          INPUT 12,
                          OUTPUT SSCre.SC_TasEfe) NO-ERROR.

    SSCre.SC_TasEfe = (SSCre.SC_TasEfe * 100).
    SSCre.SC_TasEfe = round(SSCre.SC_TasEfe,2).

    CASE Pro_Creditos.Tip_Credito:
        WHEN 1 THEN SSCre.SC_Clasif = 2. /*Consumo*/
        WHEN 2 THEN SSCre.SC_Clasif = 1. /*Comercial*/
        WHEN 2 THEN SSCre.SC_Clasif = 3. /*Hipotecario*/
        WHEN 4 THEN SSCre.SC_Clasif = 4. /*Sin Identificar*/
    END CASE.

    SSCre.SC_Clasif = 2.

    CASE rep_creditos.categoriames:
        WHEN "A" THEN
            ASSIGN SSCre.SC_EdaMor = 0
                   SSCre.SC_Catego = "A".

        WHEN "B" THEN
            ASSIGN SSCre.SC_EdaMor = 31
                   SSCre.SC_Catego = "B".

        WHEN "C" THEN DO:
            SSCre.SC_Catego = "C".

            IF Pro_Creditos.Tip_Credito EQ 1 THEN
                SSCre.SC_EdaMor = 61.

            IF Pro_Creditos.Tip_Credito EQ 3 THEN
                SSCre.SC_EdaMor = 121.
        END.

        WHEN "D" THEN DO:
            SSCre.SC_Catego = "D".

            IF Pro_Creditos.Tip_Credito EQ 1 THEN
                SSCre.SC_EdaMor = 91.

            IF Pro_Creditos.Tip_Credito EQ 3 THEN
                SSCre.SC_EdaMor = 181.
        END.

        WHEN "E" THEN DO:
            SSCre.SC_Catego = "E".

            IF Pro_Creditos.Tip_Credito EQ 1 THEN
                SSCre.SC_EdaMor = 181.

            IF Pro_Creditos.Tip_Credito EQ 3 THEN
                SSCre.SC_EdaMor = 361.
        END.

        OTHERWISE
            ASSIGN SSCre.SC_EdaMor = 0
                   SSCre.SC_Catego = "A".
    END CASE.

    /* Asigna los dias de atraso por arrastre  SC_EdaMor - wmr */
    FIND FIRST tMaxDias WHERE tMaxDias.wnit EQ rep_creditos.nit AND tMaxDias.wTip EQ rep_creditos.tip_credito NO-LOCK NO-ERROR.
    /*IF AVAILABLE tMaxDias THEN
        SC_EdaMor = tMaxDias.wDiaMax.
    ELSE*/
        SC_EdaMor = rep_creditos.dias_atraso.

    SSCre.SC_Catego = rep_creditos.Categoriames.

    CASE rep_creditos.Per_Pago:
        WHEN 1 THEN
            ASSIGN SSCre.SC_Amorti = 7
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 7)).

        WHEN 2 THEN
            ASSIGN SSCre.SC_Amorti = 10
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 10)).

        WHEN 3 THEN
            ASSIGN SSCre.SC_Amorti = 15
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 15)).

        WHEN 4 THEN
            ASSIGN SSCre.SC_Amorti = 30
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 30)).

        WHEN 5 THEN
            ASSIGN SSCre.SC_Amorti = 60
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 60)).

        WHEN 6 THEN
            ASSIGN SSCre.SC_Amorti = 90
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 90)).

        WHEN 7 THEN
            ASSIGN SSCre.SC_Amorti = 120
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 120)).

        WHEN 8 THEN
            ASSIGN SSCre.SC_Amorti = 180
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 180)).

        WHEN 9 THEN
            ASSIGN SSCre.SC_Amorti = 360
                   FecAux = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 360)).
    END CASE.

    IF rep_creditos.dias_atraso < 30 AND fecaux < Fec_Corte THEN
        fecaux = fec_corte + 30.

    tNewFec = FecTermina2().

    IF tNewFec NE "" THEN
        FecAux = DATE(INT(SUBSTRING(tNewFec,5,2)),INT(SUBSTRING(tNewFec,7,2)),INT(SUBSTRING(tNewFec,1,4))).

    IF SSCre.SC_Amorti GT 30 THEN
        SSCre.SC_Amorti = 30.

    SSCre.SC_FecVen = STRING(FecAux,"99/99/9999").

    IF ADD-INTERVAL(rep_creditos.fec_desembolso,60,"years") < fecAux THEN DO:
        fecAux = ADD-INTERVAL(rep_creditos.fec_desembolso,60,"years").
        SSCre.SC_FecVen = STRING(FecAux,"99/99/9999").
    END.

    IF rep_creditos.Fec_UltPago NE ? THEN
        SSCre.SC_FecUltpago = STRING(rep_creditos.Fec_UltPago,"99/99/9999").
    ELSE
        ASSIGN SSCre.SC_FecUltpago = STRING(rep_creditos.fec_desembolso,"99/99/9999").

    IF SSCre.SC_FecVen EQ ? THEN
        SSCre.SC_FecVen = "".

    SSCre.SC_DesCre = "O".

    SSCre.SC_Agen = /*string(creditos.agencia).*/ "1".
    ssCre.SC_capmora = 0.

    IF SSCre.SC_EdaMor GT 0 THEN
        SSCre.SC_capmora = int(rep_creditos.val_atraso).

    IF sscre.sc_capmora GT rep_creditos.sdo_capital THEN
        ssCre.SC_capmora = rep_creditos.sdo_capital - 2.

    IF SSCre.SC_EdaMor = 0 AND fecAux < fec_Corte THEN DO:
        fecAux = fec_Corte.
        SSCre.SC_FecVen = STRING(FecAux,"99/99/9999").
    END.

    IF rep_credito.abogado = TRUE THEN
        SSCre.estadoCredito = 2.

    IF rep_creditos.cod_credito <> 108 AND
       rep_creditos.cod_credito <> 113 AND
       rep_creditos.cod_credito <> 114 AND
       rep_creditos.cod_credito <> 123 THEN DO:
        FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.
        IF AVAILABLE empresas THEN DO:
            SSCre.nitPatronal = empresas.nit.
            
            FIND FIRST clientes WHERE clientes.nit = empresas.nit NO-LOCK NO-ERROR.
            SSCre.nombrePatronal = clientes.nombre.
        END.
    END.
END.

OUTPUT TO VALUE(W_Pathspl + "\" + STRING(W_agencia,"99") + "SSCre" + STRING(YEAR(DATE(Fec_Corte:SCREEN-VALUE))) + "-" + STRING(MONTH(DATE(Fec_Corte:SCREEN-VALUE))) + ".csv").
    PUT UNFORMATTED "Tipo Documento;Cedula;Codigo;Reestructurado;Pagare;Fecha Apertura;Fecha Vencimiento;Edad Mora;TipCuo;Cuotas Pagas;Amortizacion;Modalidad;Tasa Nominal;Tasa Efectiva;Monto;Cuota;Saldo Capital;Saldo Interes;Otro Saldo;Garan;Fecha Apertura;Provision;Provision Interes;Interes Dificil Cobro;VlrExt;MesExt;Fecha Ultimo Pago;ClGar;DesCre;Agencia;Amortizacion;Val Atraso;Clase de vivienda;Señal VIS;Tipo o rango de vivienda;Señal de subsidio;Entidad de redescuento;Margen de redescuento;Sujeto de desembolso;Moneda;Fecha Reeestructurado;Categ Reeestructurado;APORTES;COD_CREDITO;NUM_ACTUALIZACIONES;ESTADO;NIT_PATRONAL;NOMBRE_PATRONAL" SKIP (0).

    FOR EACH SSCre:
        /*DISPLAY sscre WITH 1 COL.*/
        
        FIND FIRST rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                                  AND rep_creditos.nit <> SSCre.SC_CedNit
                                  AND rep_creditos.num_credito = INTEGER(SSCre.SC_Pagare) NO-LOCK NO-ERROR.
        IF AVAILABLE rep_creditos THEN
            SSCre.SC_Pagare = DECIMAL(STRING(SSCre.SC_Pagare) + SSCre.SC_CedNit).

        ASSIGN CAMPO0 = SSCre.SC_TipIde + ";" + /* A Tipo Documento */
                        SSCre.SC_CedNit + ";" + /* B Cedula */
                        STRING(SSCre.SC_CodCon) + ";" + /* C Codigo */
                        STRING(SSCre.SC_Reest,"9") + ";" + /* D Reestructurado */
                        STRING(SSCre.SC_Pagare) + ";" + /* E Pagare */
                        SSCre.SC_FecApe + ";" + /* F Fecha Apertura */
                        SSCre.SC_FecVen + ";" + /* G Fecha Vencimiento */
                        STRING(SSCre.SC_EdaMor) + ";" + /* H Edad Mora */
                        STRING(SSCre.SC_TipCuo) + ";" + /* I TipCuo */
                        STRING(SSCre.SC_CuoPag) + ";" + /* J Cuotas Pagas */
                        STRING(SSCre.SC_Amorti) + ";" + /* K Amortizacion */
                        STRING(SSCre.SC_Modali) + ";" + /* L Modalidad */
                        replace(STRING(SSCre.SC_TasNom), ",", ".") + ";" + /* M Tasa Nominal */
                        replace(STRING(SSCre.SC_TasEfe), ",", ".") + ";" + /* N Tasa Efectiva */
                        STRING(ROUND(SSCre.SC_Monto,0)) + ";" + /* O Monto */
                        STRING(ROUND(SSCre.SC_ValCuo,0)) + ";" + /* P Cuota */
                        STRING(SSCre.SC_SdoCap) + ";" + /* Q Saldo Capital */
                        STRING(SSCre.SC_SdoInt) + ";" + /* R Saldo Interes */
                        STRING(SSCre.SC_SdoOtr) + ";" + /* S Otro Saldo */
                        STRING(ROUND(SSCre.SC_garan, 0)) + ";" + /* T Garan */
                        SSCre.SC_FecApe + ";" + /* U Fecha Apertura */
                        STRING(SC_VlrPro) + ";" + /* V Provision */
                        STRING(sc_vlrproint) + ";" + /* W Provision Interes */
                        STRING(SSCre.SC_ConInt) + ";" + /* X Interes Dificil Cobro */
                        STRING(ROUND(SSCre.SC_VlrExt, 0)) + ";" + /* Y VlrExt */
                        STRING(SSCre.SC_MesExt) + ";" + /* Z MesExt */
                        SSCre.SC_FecUltpago + ";" + /* AA Fecha Ultimo Pago */
                        SSCre.SC_ClGar + ";" + /* AB ClGar */
                        SSCre.SC_DesCre + ";" + /* AC Destino Credito */
                        SSCre.SC_Agen + ";" + /* AD Agencia */
                        STRING(SSCre.SC_Amorti) + ";" + /* AE Amortizacion */
                        STRING(ROUND(SSCre.sc_capmora,0)) + ";" + /* AF Val Atraso */
                        SSCre.SC_ClaseViv + ";" + /* AG Clase de vivienda */
                        SSCre.SC_SenalVIS + ";" + /* AH Señal VIS */
                        SSCre.SC_TipoViv + ";" + /* AI Tipo o rango de viviend */
                        SSCre.SC_SenalSub + ";" + /* AJ Señal de subsidio */
                        SSCre.SC_EntRedes + ";" + /* AK Entidad de redescuento */
                        SSCre.SC_MarRedes + ";" + /* AL Margen de redescuento */
                        "1" + ";" + /* AM Sujeto de desembolso */
                        "1" + ";" + /* AN Moneda */
                        SSCre.SC_FecReest + ";" + /* AO Fecha Reeestructurado */
                        SSCre.SC_CatReest + ";" + /* AP Categ Reeestructurado */
                        STRING(SSCre.aportes) + ";" +
                        STRING(SSCre.codCredito) + ";" +
                        STRING(SSCre.numActualizaciones) + ";" +
                        STRING(SSCre.estadoCredito) + ";" +
                        SSCre.nitPatronal + ";" +
                        SSCre.nombrePatronal.

        PUT UNFORMATTED CAMPO0 AT 1.
    END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tabla13 wWin 
PROCEDURE Tabla13 :
CASE clientes.tipo_identificacion:
    WHEN "C.C" OR WHEN "NUI" THEN Tcifin.TipIde = "01".
    WHEN "NIT" THEN Tcifin.TipIde = "02".
    WHEN "C.E" THEN Tcifin.TipIde = "03".
    WHEN "T.I" THEN Tcifin.TipIde = "04".
    WHEN "PAS" THEN Tcifin.TipIde = "05".
    WHEN "TSS" THEN Tcifin.TipIde = "06".
    WHEN "SES" THEN Tcifin.TipIde = "07".
    WHEN "FID" THEN Tcifin.TipIde = "08".
    WHEN "R.C" THEN Tcifin.TipIde = "09".
    WHEN "CDP" THEN Tcifin.TipIde = "10".
    OTHERWISE Tcifin.TipIde = "01".
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tabla7 wWin 
PROCEDURE Tabla7 :
CASE rep_Creditos.Cod_calificaMes:
    WHEN 1 THEN Tcifin.Califica = "01".  /* "A" */
    WHEN 2 THEN Tcifin.Califica = "01".  /* "A" */
    WHEN 3 THEN Tcifin.Califica = "02".  /* "B" */
    WHEN 4 THEN Tcifin.Califica = "03".  /* "C" */
    WHEN 5 THEN Tcifin.Califica = "04".  /* "D" */
    WHEN 6 THEN Tcifin.Califica = "05".  /* "E" */
    WHEN 7 THEN Tcifin.Califica = "05".  /* "E" */
    WHEN 8 THEN Tcifin.Califica = "07"   /* "NC" (No Calificado) */.
    OTHERWISE Tcifin.Califica = "01".
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tipo_Identificacion wWin 
PROCEDURE Tipo_Identificacion :
TmpDat.ITipIde = 0.
CASE Clientes.Tipo_Identificacion:
  WHEN "C.C" THEN TmpDat.ITipIde = 1.
  WHEN "C.E" THEN TmpDat.ITipIde = 4.
  WHEN "NIT" THEN TmpDat.ITipIde = 2.
  WHEN "R.C" THEN TmpDat.ITipIde = 1.
 END CASE.
 IF TmpDat.ITipIde EQ 0 THEN TmpDat.ITipIde = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UIAF wWin 
PROCEDURE UIAF :
DEFINE VAR I AS DECIMAL FORMAT "99999999".
DEFINE VAR W_Valor  LIKE Taquilla.Val_Efectivo.
DEFINE VAR W_TipPro AS INTEGER FORMAT "9".
DEFINE VAR W_TipTra AS INTEGER FORMAT "9".
DEFINE VAR W_TipNit AS INTEGER FORMAT "9".
DEFINE VAR W_CodMpi AS INTEGER FORMAT "99999".
DEFINE VAR W_Linea  AS CHARACTER FORMAT "X(89)".
DEFINE VAR W_Total  AS DECIMAL FORMAT "9999999999999999999".

FOR EACH Taquilla WHERE MONTH(Taquilla.Fec_Transaccion) EQ MONTH(Fec_Corte) AND
                         YEAR(Taquilla.Fec_Transaccion) EQ  YEAR(Fec_Corte) AND
                         (Taquilla.Id_NUD EQ YES OR 
                          Taquilla.Id_NUM)
                         BREAK BY Taquilla.Nit BY Taquilla.Agencia:
    ASSIGN Prc = Prc + 1
           Ced = Taquilla.Nit.
    DISPLAY Prc Ced WITH FRAME FRM0.
                      
    IF FIRST-OF(Taquilla.Agencia) THEN DO:
       FIND FIRST Agencias WHERE Agencias.Agencia EQ Taquilla.Agencia NO-LOCK NO-ERROR.
       IF AVAILABLE Agencias THEN
          W_CodMpi = INTEGER(SUBSTRING(Agencias.Ciudad,1,5)).
    END.
    IF FIRST-OF(Taquilla.Nit) THEN DO:
       FIND FIRST Clientes WHERE Clientes.Nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN DO:
          CASE Clientes.Tipo_Identificacion:
            WHEN "C.C" THEN W_TipNit = 1.
            WHEN "C.E" THEN W_TipNit = 2.
            WHEN "NIT" THEN W_TipNit = 3.
            OTHERWISE W_TipNit = 4.
          END CASE.
       END.
    END.
    IF Taquilla.Tip_Producto EQ 1 THEN DO:
       FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Ahorros THEN DO:
          CASE Pro_Ahorros.Tip_Ahorro:
            WHEN 3 THEN W_TipPro = 5.
            OTHERWISE W_TipPro = 1.
          END CASE.
       END.
    END.
    ELSE W_TipPro = 3.
    IF Taquilla.Naturaleza EQ "DB" THEN W_TipTra = 1.
    ELSE W_TipTra = 2.
    
    W_Valor = Taquilla.Val_Efectivo + Taquilla.Val_Cheque.
    W_Total = W_Total + W_Valor.
    i = i + 1.
    CREATE TUIAF.
    ASSIGN TUIAF.Fecha     = Taquilla.Fec_Transaccion.
           TUIAF.Valor     = W_Valor.
           TUIAF.Moneda    = 1.
           TUIAF.CodAge    = STRING(Taquilla.Agencia,"999999999999999").
           TUIAF.TipPdt    = W_TipPro.
           TUIAF.TipTra    = W_TipTra.
           TUIAF.NumCta    = STRING(Taquilla.Nro_Cuenta,"99999999999999999999").
           TUIAF.TipNit    = W_TipNit.
           TUIAF.NumNit    = Taquilla.Nit.
           TUIAF.CodMpi    = W_CodMpi.
           TUIAF.Consec    = I.
END.

FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
IF AVAILABLE Entidad THEN
 W_Linea = "01" + STRING(SUBSTRING(Entidad.Cod_EntidadControl,10,3),"999") + 
           STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + STRING(DAY(Fec_Corte),"99")
           + STRING(i,"99999999") + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".
OUTPUT TO VALUE(M2).
DISPLAY W_Linea WITH FRAME F1 WIDTH 95 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
FOR EACH TUIAF BREAK BY TUIAF.Consec:
 W_Linea = STRING(YEAR(TUIAF.Fecha),"9999") + STRING(MONTH(TUIAF.Fecha),"99") + STRING(DAY(TUIAF.Fecha),"99") +
           STRING(TUIAF.Valor,"99999999999999") + "1" + TUIAF.CodAge + STRING(TUIAF.TipPdt,"9") +
           STRING(TUIAF.TipTra,"9") + TUIAF.NumCta + STRING(TUIAF.TipNit) + TUIAF.NumNit +
           STRING(TUIAF.CodMpi,"99999") + STRING(TUIAF.Consec,"99999999").
 DISPLAY W_Linea WITH FRAME F2 WIDTH 95 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
END.
W_Linea = "01" + STRING(SUBSTRING(Entidad.Cod_EntidadControl,10,3),"999") + 
           STRING(W_Total,"9999999999999999999") + 
           "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".
DISPLAY W_Linea WITH WIDTH 95 NO-LABELS FRAME F3 NO-BOX USE-TEXT STREAM-IO.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Empresa wWin 
FUNCTION Empresa RETURNS CHARACTER:
DEF VAR W-Espacios AS CHAR INIT "                   ".

/*------------------ DATOS DE LA EMPRESA DONDE LABORA EL CLIENTE  -------------- */

    FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
    IF AVAIL(Empresas) THEN DO:
        ASSIGN Tcifin.NomEmpre = Empresas.Alias_Empresa
               Tcifin.DirEmpre = Clientes.Dir_comercial
               Tcifin.TelEmpre = Clientes.Tel_Comercial.
       Tabla33(Clientes.Lugar_comercial).
       ASSIGN Tcifin.CodCiuEmp = W-CodCiu   Tcifin.CiudadEmp = W-NomCiu
              Tcifin.CodDepEmp = W-CodDep   Tcifin.DepEmpTit = W-NomDep.
    END.
    ELSE
        ASSIGN Tcifin.NomEmpre   = W-Espacios + W-Espacios + W-Espacios
               Tcifin.DirEmpre   = W-Espacios + W-Espacios + W-Espacios
               Tcifin.TelEmpre   = W-Espacios
               Tcifin.CodCiuEmp  = "      "
               Tcifin.CiudadEmp  = W-Espacios
               Tcifin.CodDepEmp  = "   "
               Tcifin.DepEmpTit  = W-Espacios.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FecTermina2 wWin 
FUNCTION FecTermina2 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

DEFINE VARIABLE zfecVcto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tfec AS DATE        NO-UNDO.

CASE rep_creditos.Per_Pago:
    WHEN 1 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 7)).
    WHEN 2 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 10)).
    WHEN 3 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 15)).
    WHEN 4 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 30)).
    WHEN 5 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 60)).
    WHEN 6 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 90)).
    WHEN 7 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 120)).
    WHEN 8 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 180)).
    WHEN 9 THEN ASSIGN tfec = (rep_creditos.Fec_Desembolso + (rep_creditos.Plazo * 360)).
END CASE.

zfecVcto = STRING(YEAR(tfec),"9999") + STRING(MONTH(tfec),"99") + STRING(DAY(tfec),"99").

RETURN zfecVcto. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FNomAgencia wWin 
FUNCTION FNomAgencia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
 FIND FIRST Agencias WHERE Agencias.agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
 IF AVAILABLE(Agencias) THEN DO: 
   Tabla33(Agencias.Ciudad).
   RETURN TRIM(Agencias.Nombre).
 END.
 ELSE DO:
     Tabla33("05001000").
     RETURN "MEDELLIN".
 END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HallaRangoVivienda wWin 
FUNCTION HallaRangoVivienda RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: 
    
    Si el crèdito es VIS (Vivienda de Interès Social) los tipos son los siguientes:
    
    1-Tipo 1: Cuyo valor de la vivienda sea menor o igual a 50 SMML    
    2-Tipo 2: Cuyo valor de la vivienda sea mayor a 50 SMML y menor o igual a 70 SMML    
    3-Tipo 3: Cuyo valor de la vivienda sea mayor a 70 SMML y menor o igual a 100 SMML    
    4-Tipo 4: Cuyo valor de la vivienda sea mayor a 100 SMML y menor o igual a 135 SMML
            
    Para Vivienda No VIS:
    
    1-Rango 1: Cuyo monto sea mayor a VIS y menor o igual a 643.100 UVR    
    2-Rango 2: Cuyo monto sea mayor a 643.100 UVR y menor o igual a 2’411.625 UVR   
    3-Rango 3: Cuyo valor sea mayor a 2’411.625 UVR.
     
------------------------------------------------------------------------------*/
  DEFINE VARIABLE SalarioMin AS DECIMAL     NO-UNDO.
  ASSIGN SalarioMin = DECIMAL(SalMin:SCREEN-VALUE IN FRAME Frm0).
  IF garantias.Tipo_Garantia EQ 1 AND (garantias.ClaseVivienda EQ 1 OR garantias.ClaseVivienda EQ 2) THEN DO:
      IF garantias.VIS EQ 1 THEN DO:
            IF Garantias.Val_Bien LE (SalarioMin * 50) THEN
                RETURN "1".
            ELSE IF Garantias.Val_Bien GT (SalarioMin * 50) AND Garantias.Val_Bien LE (SalarioMin * 70) THEN
                RETURN "2".
            ELSE IF Garantias.Val_Bien GT (SalarioMin * 70) AND Garantias.Val_Bien LE (SalarioMin * 100) THEN
                RETURN "3".
            ELSE
                RETURN "4".
      END.
      ELSE DO:
            IF Garantias.Val_Bien GT (SalarioMin * 135) AND Garantias.Val_Bien LE (DECIMAL(UVR:SCREEN-VALUE) * 643100) THEN
                RETURN "1".
            ELSE IF Garantias.Val_Bien GT (DECIMAL(UVR:SCREEN-VALUE) * 643100) AND Garantias.Val_Bien LE (DECIMAL(UVR:SCREEN-VALUE) * 2411625) THEN
                RETURN "2".
            ELSE IF Garantias.Val_Bien GT (DECIMAL(UVR:SCREEN-VALUE) * 24116255) THEN
                RETURN "3".
            ELSE
                RETURN "1".
      END.
  END.
  ELSE RETURN "1".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION retFormato wWin 
FUNCTION retFormato RETURNS CHARACTER
    /*
    GioCam Oct 08/07
    Retorna valor entero en tipo caracter, con formato de 10 caracteres
    distinguiendo si es entero negativo o entero positivo...
    */

    ( INPUT pivalor AS DECIMAL ) :
    DEFINE VARIABLE vipos AS CHARACTER FORMAT "999999999999" NO-UNDO.
    DEFINE VARIABLE vineg AS CHARACTER FORMAT "-99999999999"    NO-UNDO.
    
    IF pivalor < 0 THEN DO:
        ASSIGN vineg = STRING(pivalor, "-99999999999").
        RETURN vineg.
    END.
    ELSE DO:
        ASSIGN vipos = STRING(pivalor, "999999999999").
        RETURN vipos.
    END.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tabla19 wWin 
FUNCTION Tabla19 RETURNS CHARACTER
  ( INPUT TipoClte AS INTEGER, INPUT Nombre AS CHAR) :
/*------------------------------------------------------------------------------
Código  Descripción
000     NO APLICA - PERSONA NATURAL   
001     ENTIDAD y ORGANISMO PUBLICO A NIVEL NACIONAL
002     ENTIDAD y ORGANISMO PUBLICO A NIVEL DEPARTAMENTAL
003     ENTIDAD y ORGANISMO PUBLICO A NIVEL MUNICIPAL
004     SOCIEDADES PRIVADAS EXTRANJERAS
005     SOCIEDADES PRIVADAS NACIONALES
006     SOCIEDADES MULTINACIONALES
007     SOCIEDADES SIN ANIMO DE LUCRO
008     SOCIEDADES COOPERATIVAS
009     ENTIDAD PÚBLICA EXTRANJERA

 Tipo_Cliente:"Natural Mayor de Edad", 1, "Natural Menor de Edad", 2,
                "Juridica S.A", 3, "Juridica C.A", 4

------------------------------------------------------------------------------*/
  IF INDEX(UPPER(Nombre), "COOP") GT 0 THEN
      RETURN "008".
  
   CASE TipoClte:
      WHEN 1 THEN RETURN "000".
      WHEN 2 THEN RETURN "005".
  END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tabla2 wWin 
FUNCTION Tabla2 RETURNS CHARACTER
  ( INPUT TipCre AS INTEGER ) :
/*----------------------------- T I P O   D E   C R E D I T O -----------------
    Código      Descripción
    01  Comercial
    02  Consumo
    03  Vivienda
    04  Microcrédito
    05  Otro
------------------------------------------------------------------------------*/
    CASE TipCre:
        WHEN 1 THEN /* Consumo   */ RETURN "02".
        WHEN 2 THEN /* Comercial */ RETURN "01".
        WHEN 3 THEN /* Vivienda  */ RETURN "03".
        WHEN 4 THEN /* MicroCred */ RETURN "04".
        OTHERWISE   /* Otros     */ RETURN "05". 
    END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tabla3 wWin 
FUNCTION Tabla3 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*--------------------------  L I N E A S   D E   C R E D I T O    ---------------
   Código       Descripción
   001  Rotativo         002  Empleado            003 Ordinario        004  Normal
   005  Compra Cartera   006  Venta Cartera       007 Otro             008  Libre Inversión
   009  Recreación       010  Tarjeta de Crédito  011 Vehículo         012  Bienes y Servicios
   013  Calamidad        014  Vivienda            015 Venta de Bienes  016  Pago en Especie
   017  Redescuento      018  Serv Arrendamiento  019 Solidario        040  Universidad
   041  Educativo        042  Educ en el exterior 043 Libranza         044  Consolidado Rotativo
   045  Vivda Int Soc    046  Emergente           047 Hospitalización  048      Bienestar Social
   049  Pignoración      050  Transitorio         051 Credirápido      052      Suministro
   053  RepLocaty MejVda 054  Empleados no Asocia 055 Crediexito       056  Credialkosto
   057  Conexel          058  Credi-Ya            059 Credi-Prima      060      Crédito de Salud
   061  Nuevo Milenio    062  Nvo Mil Credi Secc  063 Celulares Conex  064      Inmediato
   065  Consumo          066  Órdenes de Compra   067 Especializaciones
   
  Lineas del sistema                          HOMOLOGACION realizada con Beatriz Munera 3 sep 2009
  1:consumo                                     
     1 010 Libre Destinacion                     008  
     1 011 Credito Gerencia                      065  
     1 050 Credito Vehiculo                      011  
     1 070 Credito Refinanciado                  007  
     1 540 Empleados Consumo                     065  
     1 541 Empleados Vivienda                    014  
     1 570 Rotativo                              001  
     1 571 Credito Libreria                      064  
     1 572 Novaciones                            007  
     
  2:Comercial
     2 524 Comercial Libre Destinacion           003 
     
  3:Vivienda                                     014
     3 015 Credito Vivienda            
  
------------------------------------------------------------------------------*/
CASE Creditos.Cod_credito:
    WHEN 123 THEN RETURN "001".
    WHEN  10 THEN RETURN "008".
    WHEN  11 OR WHEN 540 THEN RETURN "065".
    WHEN  50 THEN RETURN "011".
    WHEN  70 OR WHEN 572 THEN RETURN "007".
    WHEN 541 OR WHEN 15 THEN RETURN "014".
    WHEN 571 THEN RETURN "064".
    WHEN 524 THEN RETURN "003".
    OTHERWISE RETURN "007".
END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tabla31 wWin 
FUNCTION Tabla31 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*---------------------------  T I P O   D E   P A G O ------------------------------
                       01       Voluntario
                       02       No Voluntario
------------------------------------------------------------------------------*/
  IF Creditos.Abogado /* Otra : Abogado */ THEN
     RETURN "02". 
  ELSE IF creditos.estado EQ 3 THEN
     RETURN "01".
  ELSE 
     RETURN "  ".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tabla33 wWin 
FUNCTION Tabla33 RETURNS CHARACTER
  ( INPUT CodLugar AS CHAR ) :
 IF SUBSTRING(CodLugar,1,5) NE "05001" AND SUBSTRING(CodLugar,1,2) NE "00" AND TRIM(CodLugar) NE "" THEN DO:
    FIND FIRST Ubicacion WHERE SUBSTRING(Ubicacion.Ubicacion,1,5) = SUBSTRING(CodLugar,1,5)
               AND Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
    IF AVAILABLE(Ubicacion) THEN
       ASSIGN W-CodCiu  = STRING(INTEGER(SUBSTRING(Ubicacion.Ubicacion,3,3)),"999999")
              W-NomCiu  = UPPER(Ubicacion.Nombre)
              W-CodDANE = CodLugar.
    ELSE
       ASSIGN W-CodDep = "005"
              W-NomDep = "ANTIOQUIA           "
              W-CodCiu = "000001"
              W-NomCiu = "MEDELLIN            "
              W-CodDANE = "05001001".

    FIND FIRST Ubicacion WHERE SUBSTRING(Ubicacion.Ubicacion,1,2) = SUBSTRING(CodLugar,1,2)
               AND Ubicacion.Tipo EQ "D" NO-LOCK NO-ERROR.
    IF AVAILABLE(Ubicacion) THEN
       ASSIGN W-CodDep  = STRING(INTEGER(SUBSTRING(Ubicacion.Ubicacion,1,2)),"999")
              W-NomDep  = UPPER(Ubicacion.Nombre)
              W-CodDANE = CodLugar.
    ELSE
       ASSIGN W-CodDep = "005"
              W-NomDep = "ANTIOQUIA           "
              W-CodCiu = "000001"
              W-NomCiu = "MEDELLIN            "
              W-CodDANE = "05001001".
 END.
 ELSE
     ASSIGN W-CodDep  = "005"
            W-NomDep  = "ANTIOQUIA           "
            W-CodCiu  = "000001"
            W-NomCiu  = "MEDELLIN            "
            W-CodDANE = "05001001".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tabla4 wWin 
FUNCTION Tabla4 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
    DEF VAR zcodGtia  AS CHAR INITIAL "".
    DEF VAR Zvlrgtia  AS DECIMAL INITIAL 0.
    W-Vlrgtia = "000000000000".
    /*------------------------------------- G A R A N T I A S -----------------
       Código       Descripción
       01   Sin Garantía
       02   No Idónea
       03   Idónea Contratos de hipoteca
       04   Idónea Contratos de Prenda
       05   Idónea Pignoración De Rentas De Entidades Territoriales y descentralizadas de todos los ordenes
       06   Idónea Garantía Soberana De La Nación (Ley 617 De 2000)
       07   Idónea Contrato Irrevocable De Fiducia Mercantil De Garantía,
       08   Idónea Garantía Otorgadas Por El Fondo Nacional De Garantía S.A.
       09   Idónea Cartas De Crédito
       10   Idónea Otras Garantías
       11   Otra
       12   Idónea
       13   FAG (Fondo agropecuario de garantías)
       14   Personal
       15   Leasing
       16   Personal con Libranza
       17   Personal sin Libranza
       18   Real con Libranza
       19   Real sin Libranza
    ------------------------------------------------------------------------------*/
        FOR EACH Garantias WHERE 
                 Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
                 Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
                 Garantias.Num_Solicitud EQ Creditos.Num_Solicitud AND
                 Garantias.Num_Credito   EQ Creditos.Num_Credito   AND
                 Garantias.Estado        EQ 1 NO-LOCK BREAK BY  Garantias.Tipo_Garantia:
            IF FIRST-OF(Garantias.Tipo_Garantia) THEN DO:
                CASE Garantias.Tipo_Garantia:
                    WHEN 1 THEN zcodGtia = "03". /* propiedad- Hipotecas */
                    WHEN 2 THEN zcodGtia = "04". /* vehiculo -Pignoracion*/
                    OTHERWISE zcodGtia = "11".   /* Otras Garantias  */
                END CASE.
            END.
            ASSIGN Zvlrgtia = Zvlrgtia + Garantias.Val_Bien
                   W-Vlrgtia = STRING(Zvlrgtia / 1000,"999999999999").
        END.
        IF W-Vlrgtia EQ "000000000000" THEN DO:
            FIND FIRST Relaciones WHERE 
                       Relaciones.Nit            EQ Creditos.Nit         AND
                       INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito AND
                       Relaciones.Clase_Producto EQ 2                    AND
                       Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
                       Relaciones.Cod_Relacion   EQ 11                   AND
                       Relaciones.Estado         EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE Relaciones THEN  
              zcodGtia = "14". /*codeudores*/
        END.
        IF zcodGtia EQ "" THEN ASSIGN zcodGtia = "01".
        RETURN zcodGtia.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

