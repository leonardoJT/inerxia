/*  Borra cuenta Vacia en Banca Mes de Enero de 2008            */ 
/* Este procedimiento se debe ejecutar con la base de datos Banca  */ 
OUTPUT TO "c:\INFo_JURISCOOP\bor_cta51701015.txt" .
DEFINE VARIABLE vtde LIKE bdcentral.Mov_Contable.Db INIT 0 NO-UNDO.
DEFINE VARIABLE vtcr LIKE bdcentral.Mov_Contable.Cr INIT 0 NO-UNDO.
DEFINE VARIABLE vtdi LIKE bdcentral.Mov_Contable.Cr INIT 0 NO-UNDO.
FOR EACH    bdcentral.Mov_Contable WHERE 
            bdcentral.Mov_Contable.Cuenta = "51701015" 
            AND bdcentral.Mov_Contable.Comprobante = 04 
            AND bdcentral.Mov_Contable.Fec_Contable = DATE(01,31,2008) :
            ASSIGN 
                vtde = vtde + bdcentral.Mov_Contable.Db
                vtcr = vtcr + bdcentral.Mov_Contable.Cr.
            DISPLAY 
                bdcentral.Mov_Contable.agencia
                bdcentral.Mov_Contable.Cr
                bdcentral.Mov_Contable.Comprobante
                bdcentral.Mov_Contable.Db
                bdcentral.Mov_Contable.Cuenta
                bdcentral.Mov_Contable.Fec_Contable
                WITH  WIDTH 300.
/*             DELETE mov_contable. */
END.
ASSIGN vtdi = vtde - vtcr.
DISPLAY vtde vtcr vtdi .
OUTPUT CLOSE.
