/* Created by: Edgar Ortiz
   Date: 14 de agosto de 2008
   Description: Reporte de cuentas de mayor que aparecen en la tabla de saldos y mov_contable */
/*--------------------------------------------------------------------------------------------------*/

{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}       
        
DEFINE VARIABLE vi_Cont   AS INTEGER NO-UNDO.
DEFINE VARIABLE vc_Mes    AS CHARACTER EXTENT 12 NO-UNDO.
DEFINE VARIABLE vd_Saldo  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE vd_SalDeb AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE vd_SalCre AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE vc_NombreMes AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc_stado  AS CHARACTER FORMAT "X(12)" NO-UNDO.

ASSIGN vc_Mes[1] = "Enero"
       vc_Mes[2] = "Febrero"
       vc_Mes[3] = "Marzo"
       vc_Mes[4] = "Abril"
       vc_Mes[5] = "Mayo"
       vc_Mes[6] = "Junio"
       vc_Mes[7] = "Julio"
       vc_Mes[8] = "Agosto"
       vc_Mes[9] = "Septiembre"
       vc_Mes[10] = "Octubre"
       vc_Mes[11] = "Noviembre"
       vc_Mes[12] = "Diciembre".        
        
    
 IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_Pathspl + "\CtasMayorEnSald.txt".

 
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (pCtasMayorEnSaldos.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

FORM 
    cuentas.cuenta             COLUMN-LABEL "CUENTA" 
    vc_stado                   COLUMN-LABEL "ESTADO"
    mov_contable.Agencia       COLUMN-LABEL "AGENCIA" 
    Mov_Contable.Nit           COLUMN-LABEL "NIT"
    Mov_Contable.Comprobante   FORMAT ">>>9" COLUMN-LABEL "COMPROBANTE"
    Mov_contable.Num_Documento FORMAT ">>>>>>>>9"COLUMN-LABEL "DOCUMENTO"
    Mov_Contable.fec_Contable  FORMAT "9999/99/99" COLUMN-LABEL "FECHA CONT"
    Mov_contable.Fec_grabacion FORMAT "9999/99/99" COLUMN-LABEL "FECHA GRAB"
    Mov_Contable.Usuario       COLUMN-LABEL "USUARIO"
    Mov_Contable.Db            COLUMN-LABEL "DÉBITO"
    Mov_Contable.Cr            COLUMN-LABEL "CRÉDITO"
WITH FRAME b DOWN COLUMN 1 WIDTH 350
NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
    

FOR EACH cuentas WHERE (IF pc01 = '1' THEN cuentas.cuenta BEGINS pc02 ELSE cuentas.cuenta = pc02 ) AND Cuentas.Tipo = 1 NO-LOCK,
    EACH sal_cuenta WHERE sal_cuenta.cuenta = cuentas.cuenta AND 
         sal_cuenta.ano = YEAR(TODAY) NO-LOCK BREAK BY cuentas.cuenta:
  REPEAT vi_Cont = 1 TO 12:
    IF DB[vi_Cont] <> 0 OR CR[vi_Cont] <> 0 THEN DO:      
      FOR EACH mov_contable WHERE mov_contable.Agencia = sal_cuenta.Agencia AND mov_contable.cuenta = sal_cuenta.cuenta AND
                            MONTH(mov_contable.fec_contable) = vi_Cont NO-LOCK:
        ASSIGN vc_stado = IF cuentas.estado = 1 THEN "Activa" ELSE "Inactiva".
        DISPLAY cuentas.cuenta
                vc_stado
                mov_contable.Agencia      
                Mov_Contable.Nit          
                Mov_Contable.Comprobante  
                Mov_contable.Num_Documento
                Mov_Contable.fec_Contable 
                Mov_contable.Fec_grabacion
                Mov_Contable.Usuario      
                Mov_Contable.Db  (TOTAL BY cuentas.cuenta)          
                Mov_Contable.Cr  (TOTAL BY cuentas.cuenta) WITH FRAME b.
        DOWN WITH FRAME b.  
      END.
    END.
  END.
END.

    
OUTPUT CLOSE.  

