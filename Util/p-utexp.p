/*OUTPUT TO Ahorros.d.
FOR EACH Ahorros:
   EXPORT Ajuste Bloqueo Clave Cod_Producto Cue_Ahorros Cue_Corriente Cue_Destino Cuota Des_Intereses Dias_Causar Dia_Sobregiro Estado Fec_Apertura Fec_Cancelacion Fec_ProLiquidacion Fec_Prorroga Fec_UltLiquidacion Fec_Ulttransaccion For_Liquidacion For_Pago Id_ForLiq Id_Sobregiro Id_TasDiferencial Ind_TasVariable Int_Causado Int_Pagar Int_Sobregiro Nit Num_Asesoria Oficina Per_Deduccion Per_Liquidacion Plazo Pun_TasVariable Sal_Intpagados Sdo_Anuales[1] Sdo_Anuales[2] Sdo_Anuales[3] Sdo_Anuales[4] Sdo_Anuales[5] Sdo_Anuales[6] Sdo_Anuales[7] Sdo_Anuales[8] Sdo_Anuales[9] Sdo_Anuales[10] Sdo_Anuales[11] Sdo_Anuales[12] Sdo_Canje Sdo_Disponible Sdo_Inicial Sdo_Minimo Sdo_UltLiquidacion Tasa Tip-Tasa Tip_Producto Usu_Creacion Val_Embargo Val_Pignorar Val_RetAcum Val_RetPeriodo Vlr_Sobregiro.
END.

OUTPUT TO Ext_Ase.d.
FOR EACH Ext_Asesoria:
    EXPORT Cod_Producto Estado Nit Num_Asesoria Oficina Pagare Plazo Valor.
END.*/

OUTPUT TO Cupos.d.
FOR EACH Cupos:
    EXPORT Clave Corte Cue_Ahorros Cupo Envio Estado Fec_Actualizacion Fec_Creacion Nit Num_Tarjeta Oficina Plazo Tip_Cupo Usu_Creo Usu_Vendio.
END.

OUTPUT TO Libreta_Che.d.
FOR EACH Libreta_Chequera:
    EXPORT Cod_Producto Cue_Ahorros Estado Fec_Entrega Num_Final Num_Inicial Oficina Pagada Reg_Chebloqueados[1] Reg_Chebloqueados[2] Reg_Chebloqueados[3] Reg_Chebloqueados[4] Reg_Chebloqueados[5] Tip_Producto Tip_Talonario.
END.
        
OUTPUT CLOSE.
