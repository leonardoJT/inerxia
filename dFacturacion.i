  FIELD Per_Factura LIKE Facturacion.Per_Factura VALIDATE  LABEL "Periodo" COLUMN-LABEL "Periodo"~
  FIELD Agencia LIKE Facturacion.Agencia VALIDATE ~
  FIELD Num_factura LIKE Facturacion.Num_factura VALIDATE  LABEL "Factura" COLUMN-LABEL "Factura"~
  FIELD Nit LIKE Facturacion.Nit VALIDATE  LABEL "Cédula" COLUMN-LABEL "Cédula"~
  FIELD Num_Credito LIKE Facturacion.Num_Credito VALIDATE  LABEL "Crédito" COLUMN-LABEL "Crédito"~
  FIELD fec_corte LIKE Facturacion.fec_corte VALIDATE  LABEL "Fec.Corte" COLUMN-LABEL "Fec.Corte"~
  FIELD Sdo_Anterior LIKE Facturacion.Sdo_Anterior VALIDATE  LABEL "Sdo.Anterior" COLUMN-LABEL "Sdo.Anterior"~
  FIELD Sdo_Mora LIKE Facturacion.Sdo_Mora VALIDATE  LABEL "Sdo.Mora" COLUMN-LABEL "Sdo.Mora"~
  FIELD Fec_Mora LIKE Facturacion.Fec_Mora VALIDATE  LABEL "Fec.Mora" COLUMN-LABEL "Fec.Mora"~
  FIELD Cuota LIKE Facturacion.Cuota VALIDATE  LABEL "Cuota" COLUMN-LABEL "Cuota"~
  FIELD CupoDisponible LIKE Facturacion.CupoDisponible VALIDATE  LABEL "Disponible" COLUMN-LABEL "Disponible"~
  FIELD CupoTotal LIKE Facturacion.CupoTotal VALIDATE  LABEL "Cupo" COLUMN-LABEL "Cupo"~
  FIELD Tasa_Ic LIKE Facturacion.Tasa_Ic VALIDATE  LABEL "Tasa.Dia" COLUMN-LABEL "Tasa.Dia"~
  FIELD Int_Corrientes LIKE Facturacion.Int_Corrientes VALIDATE  FORMAT "->>>,>>>,>>9.99" LABEL "Int.Corriente" COLUMN-LABEL "Int.Corriente"~
  FIELD Int_MorCobrar LIKE Facturacion.Int_MorCobrar VALIDATE  FORMAT "->>>,>>>,>>9.99" LABEL "Int. Mora" COLUMN-LABEL "Int. Mora"~
  FIELD Pagos LIKE Facturacion.Pagos VALIDATE  COLUMN-LABEL "Pagos"~
  FIELD Cargos LIKE Facturacion.Cargos VALIDATE  LABEL "Retiros" COLUMN-LABEL "Retiros"~
  FIELD OtrosCargos LIKE Facturacion.OtrosCargos VALIDATE  LABEL "Comisión" COLUMN-LABEL "Comisión"~
  FIELD Seg_cartera LIKE Facturacion.Seg_cartera VALIDATE  LABEL "Seg.Cartera" COLUMN-LABEL "Seg.Cartera"~
  FIELD Nuevo_Saldo LIKE Facturacion.Nuevo_Saldo VALIDATE  LABEL "Nvo.Saldo" COLUMN-LABEL "Nvo.Saldo"~
  FIELD Pago_Minimo LIKE Facturacion.Pago_Minimo VALIDATE  LABEL "Pago.Mínimo" COLUMN-LABEL "Pago.Mínimo"~
  FIELD Pago_Total LIKE Facturacion.Pago_Total VALIDATE  LABEL "Pago.Total" COLUMN-LABEL "Pago.Total"~
  FIELD Sobrecupo LIKE Facturacion.Sobrecupo VALIDATE ~
  FIELD Val_Recaudo LIKE Facturacion.Val_Recaudo VALIDATE  LABEL "Tot.Recaudo" COLUMN-LABEL "Tot.Recaudo"~
  FIELD Rec_Capital LIKE Facturacion.Rec_Capital VALIDATE  LABEL "Rec.Capital" COLUMN-LABEL "Rec.Capital"~
  FIELD Rec_Honorarios LIKE Facturacion.Rec_Honorarios VALIDATE ~
  FIELD Rec_IntCorrientes LIKE Facturacion.Rec_IntCorrientes VALIDATE  LABEL "Rec.Int.Corrientes" COLUMN-LABEL "Rec.Int.Corrientes"~
  FIELD Rec_IntMora LIKE Facturacion.Rec_IntMora VALIDATE  LABEL "Rec.Int.Mora" COLUMN-LABEL "Rec.Int.Mora"~
  FIELD Rec_Segcartera LIKE Facturacion.Rec_Segcartera VALIDATE  LABEL "Rec.Seg.Cartera" COLUMN-LABEL "Rec.Seg.Cartera"~
  FIELD PagoTotal LIKE Facturacion.PagoTotal VALIDATE  FORMAT "Si/No" LABEL "Canc.Tot.Cuota" COLUMN-LABEL "Canc.Tot.Cuota"~
  FIELD Estado LIKE Facturacion.Estado VALIDATE ~
  FIELD Tasa_Ea LIKE Facturacion.Tasa_Ea VALIDATE  LABEL "Tasa. EA" COLUMN-LABEL "Tasa. EA"~
  FIELD Val_Atraso LIKE Facturacion.Val_Atraso VALIDATE  LABEL "Val.Atraso.Capital" COLUMN-LABEL "Val.Atraso.Capital"~
  FIELD Val_atrasokpen LIKE Facturacion.Val_atrasokpen VALIDATE  LABEL "Val.Atraso.Capital.Pend" COLUMN-LABEL "Val.Atraso.Capital.Pend"~
  FIELD ciudad LIKE Facturacion.ciudad VALIDATE  LABEL "Ciudad" COLUMN-LABEL "Ciudad"~
  FIELD direccion LIKE Facturacion.direccion VALIDATE  LABEL "Dirección" COLUMN-LABEL "Dirección"~
  FIELD Fec_Inicial LIKE Facturacion.Fec_Inicial VALIDATE  LABEL "Fec.Ini" COLUMN-LABEL "Fec.Ini"~
  FIELD Fec_Final LIKE Facturacion.Fec_Final VALIDATE  LABEL "Fec.Fin" COLUMN-LABEL "Fec.Fin"~
  FIELD Fec_LimPago LIKE Facturacion.Fec_LimPago VALIDATE  LABEL "Fec.Lte.Pago" COLUMN-LABEL "Fec.Lte.Pago"~
  FIELD nombre LIKE Facturacion.nombre VALIDATE  LABEL "Nombres"~
  FIELD Nombre_Agencia LIKE Facturacion.Nombre_Agencia VALIDATE  LABEL "Nom.Agencia" COLUMN-LABEL "Nom.Agencia"~
  FIELD Telefono LIKE Facturacion.Telefono VALIDATE  LABEL "Teléfono" COLUMN-LABEL "Teléfono"
