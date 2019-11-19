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
DEFINE VARIABLE vc_stado  AS CHARACTER NO-UNDO.

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
    cuentas.cuenta      COLUMN-LABEL "CUENTA"   
    Cuentas.nombre      COLUMN-LABEL "NOMBRE" 
    Cuentas.naturaleza  COLUMN-LABEL "NATURALEZA"
    vc_stado            COLUMN-LABEL "ESTADO"
    vc_NombreMes        COLUMN-LABEL "MES"
    vd_SalDeb           COLUMN-LABEL "DÉBITO"
    vd_SalCre           COLUMN-LABEL "CRÉDITO"
    vd_Saldo            COLUMN-LABEL "SALDO"
WITH FRAME a DOWN COLUMN 1 WIDTH 300
NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
FORM 
    cuentas.cuenta             COLUMN-LABEL "CUENTA"   
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
    
    
IF pc01 = '1' THEN DO:
  FOR EACH cuentas WHERE Cuentas.Tipo = 1 NO-LOCK,
    EACH sal_cuenta WHERE sal_cuenta.cuenta = cuentas.cuenta AND 
                          sal_cuenta.ano = INT(pc02) NO-LOCK:
    REPEAT vi_Cont = 1 TO 12:
      IF DB[vi_Cont] <> 0 OR CR[vi_Cont] <> 0 THEN DO:
        IF cuenta.naturaleza = 'db' THEN ASSIGN vd_Saldo = (sal_cuenta.DB[vi_Cont] - sal_cuenta.CR[vi_Cont]).
        ELSE ASSIGN vd_Saldo = (sal_cuenta.CR[vi_Cont] - sal_cuenta.DB[vi_Cont]).
        ASSIGN vd_SalDeb = sal_cuenta.DB[vi_Cont]
               vd_SalCre = sal_cuenta.CR[vi_Cont]
               vc_nombreMes = vc_Mes[vi_Cont]
               vc_stado = IF Cuentas.Estado = 1 THEN "Activa" ELSE "Inactiva".
        DISPLAY cuentas.cuenta 
                cuentas.nombre
                Cuentas.naturaleza
                vc_stado
                vc_nombreMes
                vd_SalDeb 
                vd_SalCre
                vd_Saldo WITH FRAME a.
        DOWN WITH FRAME a.
      END.
    END.
  END.
END.
    
IF pc01 = '2' THEN DO:
  FOR EACH cuentas WHERE Cuentas.Tipo = 1 NO-LOCK,
      EACH sal_cuenta WHERE sal_cuenta.cuenta = cuentas.cuenta AND 
           sal_cuenta.ano = INT(pc02) NO-LOCK BREAK BY cuentas.cuenta:
    REPEAT vi_Cont = 1 TO 12:
      IF DB[vi_Cont] <> 0 OR CR[vi_Cont] <> 0 THEN DO:      
        FOR EACH mov_contable WHERE mov_contable.Agencia = sal_cuenta.Agencia AND mov_contable.cuenta = sal_cuenta.cuenta AND
                              MONTH(mov_contable.fec_contable) = vi_Cont NO-LOCK:
          DISPLAY cuentas.cuenta            
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
END.
    
OUTPUT CLOSE.  

