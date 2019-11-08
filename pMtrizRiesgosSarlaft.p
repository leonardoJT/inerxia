/* 
    NOMBRE      : pMtrizRiesgosSarlaft.p
    DESCRIPCION : Genera A Excel Matriz De Riesgos
    LOG         : Creado 13 ago 2008, Ing Edilberto Mariño Moya
*/ 
DEF VAR chExcelApplication  AS COM-HANDLE.
DEF VAR chWorksheet         AS COM-HANDLE.
DEF VAR chWorkbook          AS COM-HANDLE.
DEF VAR chPage              AS COM-HANDLE.
DEF VAR chExcel             AS COM-HANDLE.
DEF VAR lERROR AS LOGICAL NO-UNDO.
DEF VAR w_pathspl AS CHAR NO-UNDO INITIAL "c:\exito\".
DEF VAR daFchaCrte AS DATE NO-UNDO INITIAL TODAY.
DEF VAR iTme AS INTEGER NO-UNDO.
DEF VAR dafchaIni AS DATE NO-UNDO.
DEF VAR daFchaFin AS DATE NO-UNDO.

FUNCTION fClasfccionSarlaft RETURN CHAR(cMnsje AS CHAR):
    /*
        DESCRIPCION     : DEVUELVE LA CALIFICACION O CODIGO DE ANALISIS
        CREADO          : 12 AGO 2008, ING EDILBERTO MARIÑO MOYA
        ESTRUCTURA DEL MENAJE:
            ENTRADA UNO: CODIGO SEVERIDAD
            ENTRADA DOS: CODIGO PROBABILIDAD
    */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    FOR FIRST cfg_sarlaft NO-LOCK
        WHERE
            cfg_sarlaft.tipo = 3
        AND cfg_sarlaft.cod_severidad      = integer(ENTRY(1,cMnsje,CHR(1)))
        AND cfg_sarlaft.cod_probabilidad   = integer(ENTRY(2,cMnsje,CHR(1))):
        cRtrno = cfg_sarlaft.cod_analisis.
    END.
    RETURN  cRtrno.
END FUNCTION. /* FUNCTION fClasfccion RETURN CHAR(cMnsje AS CHAR): */
FUNCTION fFchaaaaammdd RETURN INTEGER(daFcha AS DATE):
    /*  Descripcion: Devuelve una fecha en formato aaaammdd
        creado: 23 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN YEAR(daFcha) * 10000 + MONTH(dafcha) * 100 + DAY(dafcha).
END FUNCTION. /* FUNCTION fFchaaaaammdd */
FUNCTION fToExcelVlor RETURNS LOGICAL(pCol AS char,pval AS char):
    /*  
        Descripcion: Envia Valor A Excel
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    
    IF SUBSTRING(pval,1,1) = "="
    THEN chExcel:Range(pCol):FORMULA = trim(pVal) NO-ERROR.
    ELSE chExcel:Range(pCol):VALUE = pVal NO-ERROR.
    RETURN TRUE.    
END FUNCTION. /* FUNCTION fToExcelVlor RETURNS LOGICAL(pCol AS char,pval AS char): */

FUNCTION fColExcel RETURN CHAR(j AS INTEGER):
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
END FUNCTION. /* FUNCTION fColExcel RETURN CHAR(j AS INTEGER): */
FUNCTION fCoor RETURN CHAR(i AS INTEGER, j AS INTEGER):
    RETURN fColExcel(j) + STRING(i).
END FUNCTION. /* FUNCTION fCoor RETURN CHAR(i AS INTEGER, j AS INTEGER): */

/********************/
/*PROCESO PRINCIPAL */
/********************/
    SESSION:SET-WAIT("").
    BELL.
    RUN ExcelAbre("SARLAFT").
    RUN pPrcsa.
/*     RUN ExcelCierra("SARLAFT"). */
/*     RUN ExcelTrmna.          */
    SESSION:SET-WAIT("").
    BELL.
    

    

    MESSAGE "Tiempo: " TIME - iTme " Segundos"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*****************************/
/* FIN --- PROCESO PRINCIPAL */
/*****************************/

PROCEDURE ExcelAbre :
/*------------------------------------------------------------------------------
  Purpose: ABRE EXCEL
  Parameters:  <none>
  Notes:     InfrmeScringAExcel  
  LOG: CREADO, 3 MAR 2008, ING. Edilberto Mariño Moya
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER cFrmto AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.    
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR cRngo AS char NO-UNDO.
    DEF VAR cRngo1 AS char NO-UNDO.
    DEF VAR cvlor AS CHAR NO-UNDO.
    DEF VAR sw AS LOGICAL NO-UNDO.
    DEF VAR deTSocioDemo AS DECIMAL NO-UNDO.
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.
    IF NOT VALID-HANDLE(chExcelApplication) THEN CREATE "Excel.Application" chExcelApplication .
    IF NOT VALID-HANDLE(chExcelApplication)
    THEN DO:
        MESSAGE  "ERROR: Abriendo Excel".
        RETURN.
    END.
    chExcelApplication:VISIBLE = FALSE.
    chExcelApplication:screenupdating = FALSE.


    /* las siguientes líneas son mientras se desarrolla */
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    /* FIN las siguientes líneas son mientras se desarrolla */


    chWorkSheet = chExcelApplication:workbooks:add().
    chWorkSheet = chExcelApplication:Sheets:Item(iSheetNumber).
    chWorkSheet = chExcelApplication:workbooks:item(1):worksheets:add().
    chWorkSheet:activate().
    /* chWorkSheet:Name = "Formato" + cFrmto + "_" + string(fFchaaaaammdd(daFchaCrte)). */

    chExcel = chExcelApplication.
    /* RUN DNEElHdeExelSUPER IN hSper001(chExcel). */
/**************************************************************************************************************************************/
END PROCEDURE.

PROCEDURE ExcelCierra :
/*------------------------------------------------------------------------------
  Purpose: CIERRA EXCEL
  Parameters:  <none>
  Notes:     InfrmeScringAExcel  
  LOG: CREADO, 3 MAR 2008, ING. Edilberto Mariño Moya
------------------------------------------------------------------------------*/


/**************************************************************************************************************************************/
    DEF INPUT PARAMETER cFrmto AS CHAR NO-UNDO.
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    chExcelApplication:displayalerts = FALSE.
    chExcelApplication:ActiveWorkbook:SaveAs(w_pathspl + "Formato" + cFrmto + "_" + string(fFchaaaaammdd(daFchaCrte)),1,"","",FALSE,FALSE,,).
    RELEASE OBJECT chWorksheet.
    RELEASE OBJECT chWorkBook   NO-ERROR.

END PROCEDURE.

PROCEDURE ExcelTrmna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chExcelApplication:QUIT(). 
/*     RELEASE OBJECT chWorksheet.           */
/*     RELEASE OBJECT chWorkBook   NO-ERROR. */
    RELEASE OBJECT chExcelApplication.

END PROCEDURE.

PROCEDURE pPrcsa:
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR cSvrdad AS CHAR NO-UNDO.
    DEF VAR cPrbbldad AS CHAR NO-UNDO.
    DEF VAR cTSvrdad AS CHAR NO-UNDO.
    DEF VAR cTPrbbldad AS CHAR NO-UNDO.
    DEF VAR cEvntos AS CHAR NO-UNDO.
    DEF VAR iNmro AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cClfccion AS CHAR NO-UNDO.
    DEF VAR cClsfciones AS CHAR NO-UNDO INITIAL "A,E,M,B".
    DEF VAR cClres AS CHAR NO-UNDO INITIAL "6,4,44,3".
    DEF VAR k AS INTEGER NO-UNDO.
    DEF VAR cCoor AS CHAR no-undo.
    /* determina lista de severidades */
    FOR EACH cfg_sarlaft NO-LOCK
        WHERE
            cfg_sarlaft.tipo = 1
        BREAK
            BY cfg_sarlaft.cod_severidad:
       cSvrdad = cSvrdad + string(cfg_sarlaft.cod_severidad) + ",".
       cTSvrdad = cTSvrdad + string(cfg_sarlaft.Nombre) + ",".
    END.
    /* determina lista de probabilidades */
    FOR EACH cfg_sarlaft NO-LOCK
        WHERE
            cfg_sarlaft.tipo = 2
        BREAK
            BY cfg_sarlaft.cod_probabilidad DESC:
       cPrbbldad = cPrbbldad + string(cfg_sarlaft.cod_probabilidad) + ",".
       cTPrbbldad = cTPrbbldad + cfg_sarlaft.Nombre + ",".
    END.
    cSvrdad = TRIM(cSvrdad,",").
    cPrbbldad = TRIM(cPrbbldad,",").
    cTSvrdad = TRIM(cTSvrdad,",").
    cTPrbbldad = TRIM(cTPrbbldad,",").
    DO i = 1 TO NUM-ENTRIES(cSvrdad,","):
        cCoor = fcoor(1,i + 1).
        ftoexcelVlor(cCoor,entry(i,ctSvrdad,",")).
        chExcel:Range(cCoor):FONT:bold = TRUE.
        chExcel:Range(cCoor):HorizontalAlignment = -4108.
    END.
    DO i = 1 TO NUM-ENTRIES(cPrbbldad,","):
        cCoor = fcoor(i + 1,1).
        ftoexcelVlor(cCoor,entry(i,ctPrbbldad,",")).
        chExcel:Range(cCoor):FONT:bold = TRUE.
    END.
    FOR EACH pro_sarlaft NO-LOCK:
        i  = maximum(0,LOOKUP(string(pro_sarlaft.cod_probabilidad),cPrbbldad,",")) + 1.
        j  = maximum(0,LOOKUP(string(pro_sarlaft.cod_severidad),cSvrdad,",")) + 1.
        cCoor = fcoor(i,j).
        c  = chExcel:Range(cCoor):VALUE.
        ftoexcelVlor(cCoor, "'" + TRIM(
                     (IF c = ? THEN "" ELSE c) + ";" + string(pro_sarlaft.cod_factor),";")).
        chExcel:Range(cCoor):WrapText = TRUE.
        cClfccion = fClasfccionSarlaft(STRING(j) + CHR(1) + STRING(i)).
        k = LOOKUP(cClfccion,cClsfciones,",").
        cCoor = fcoor(i,j).
        IF k <> 0 
        THEN chExcel:Range(cCoor):interior:COLORindex = ENTRY(k,cClres,",").
    END.     /* fin determina lista de probabilidades */
    chExcel:Range("a1:a5"):entirerow:INSERT.
    ASSIGN i = 1 j = 1.
    cCoor = fcoor(i,j).
    ftoexcelVlor(cCoor,"MATRIZ DE RIESGOS").
    chExcel:Range(cCoor):FONT:bold = TRUE.
    i = 2.
    cCoor = fcoor(i,j).
    ftoexcelVlor(cCoor,"FECHA:").
    chExcel:Range(cCoor):FONT:bold = TRUE.
    j = 2.
    cCoor = fcoor(i,j).
    ftoexcelVlor(cCoor,string(TODAY,"99/99/9999")).
END PROCEDURE.
