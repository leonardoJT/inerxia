IF NOT CONNECTED("banca") THEN
   CONNECT -db /bandat/datos/banca.db -ld bdcenbanc -U BASH -P BASH.


IF NOT CONNECTED("bdcentral") THEN
  CONNECT -db /muldat1/datos/multi.db -ld bdcenmult -U BASH -P BASH.

RUN rluis.p.


IF CONNECTED("banca") THEN
  DISCONNECT  banca.

