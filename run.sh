#!/bin/bash

## --------------------------------------------------------------------------
## UFPR, BCC, ci210 2015-2 trabalho semestral, autor: Roberto Hexsel, 04mar22
## --------------------------------------------------------------------------

## ESTE ARQUIVO NAO PODE SER ALTERADO


#set -x

trab=pid

src="packageWires files aux $trab tb_${trab}"
sim=$trab
simulator=tb_${sim}
visual=v_${sim}.vcd
save=v.sav

length=200
unit=n

curva=STEP

usage() {
cat << EOF
usage:  $0 [options] 
        re-create simulator/model and run simulation

OPTIONS:
   -h    Show this message
   -t T  number of time-units to run (default ${length})
   -u U  unit of time scale {m,u,n,p} (default ${unit}s)
   -n    send simulator output do /dev/null, else to $visual
   -w    invoke GTKWAVE -- do not use with -n
   -L    simule trajetória "louca" (aleatória)
   -S    simule trajetória "semi-louca" (semi-aleatória)
   -C    simule curva à bombordo
   -D    simule trajetória com dois degraus (step, default)
   -R    simule trajetória em linha reta
EOF
}

while true ; do

    case "$1" in
        -h | "-?") usage ; exit 1
            ;;
        -x) set -x
            ;;
        -t) length=$2
            shift
            ;;
        -u) unit=$2
            shift
            ;;
        -n) visual=/dev/null
            ;;
        -w | -v) WAVE=true
            ;;
        -L | -LOUCA) curva=LOUCA
		     ;;
        -S | -SEMI) curva=SEMI
		    ;;
        -C | -CURVA) curva=CURVA
		     ;;
	-D | -DEGRAU | -STEP) curva=STEP
			      ;;
	-R | -RETA) curva=RETA
		    ;;
        *) break
            ;;
    esac
    shift
done

gcc -D ${curva} genData.c && ./a.out > input.data


# compila simulador
ghdl --clean

ghdl -a --ieee=standard -fexplicit packageWires.vhd || exit 1

for F in ${src} ; do
    if [ ! -s ${F}.o  -o  ${F}.vhd -nt ${F}.o ] ; then
        ghdl -a --ieee=standard ${F}.vhd  ||  exit 1
    fi
done

ghdl -c packageWires.vhd {aux,files}.vhd ${sim}.vhd tb_${sim}.vhd \
     -e ${simulator} || exit 1

./$simulator --ieee-asserts=disable --stop-time=${length}${unit}s \
           --vcd=${visual} >output.txt

test -v $WAVE  ||  gtkwave.exe -O /dev/null ${visual} ${save}

./plota.gp -pdf

