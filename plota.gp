#!/bin/bash

# set -x

if [ $# -lt 1 ] ; then
   echo -e "\n\t$0 -p|-f|-l|-x|-j \n\tARGS: $*\n"
   exit 1
fi

declare -a inp lbl
declare -i idx=0
ptSZ="1"

for P in "$@" ; do      # while true 

    case "$1" in
        -e|-ps|-eps|-postscript)  # postscript
            outTerm='postscript eps enhanced color blacktext "Times-Roman" 18'
	    ptSZ="1.4"
            outSfx="eps"
            lmargin="at screen -1"
            sz="0.625,0.650"
            ;;

        -p|-pdf)                  # pdf
            outTerm='pdfcairo enhanced color dashed font "Arial,10"'
	    ptSZ="1.4"
            outSfx="pdf"
            sz="1.0,1.0"
            ;;

        -f|-fig|-xfig)            # xfig  
            outTerm="fig color landscape metric fontsize 11"
            outSfx="fig"
            sz="1.0,1.0"
            ;;

        -l|-lat|-latex)           # latex
            outTerm="latex 10 size 6cm, 5cm"
            outSfx="tex"
            sz="1.0,1.0"
            ;;

        -x|-x11|-terminal)        # na tela, X11
            outTerm='x11 enhanced font "terminal-14" persist raise'
            outSfx="null"
            sz="1.0,1.0"
            ;;

        -j|-jpg|-jpeg)            # jpg
            outTerm='jpeg large enhanced'
            outSfx="jpg"
            sz="1.0,1.0"
            ;;

        -t|-tit|-title)
            outTitle=$2
	    shift;
	    ;;

        *)  echo -e "\n\n\t erro: $0 $1\n\n"
	    exit 1
	    ;;
    esac

    shift;

done

TITLE=$(echo $outTitle | sed -e 's:_:-:g')

outFile="res.${outSfx}"
echo $outFile

if [ "x$XRANGE" = "x" ] ; then
  XRANGE="[0:160]"        ## ELSE usa variavel do ambiente
fi

if [ "x$YRANGE" = "x" ] ; then
  YRANGE="[-40:40]"       ## ELSE usa variavel do ambiente
fi

cat <<EOF0 | gnuplot
set encoding iso_8859_1
set notitle
set xlabel "ciclo"
set ylabel ""
set xrange $XRANGE
set yrange $YRANGE 
set noylabel
## set lmargin $lmargin 
set key outside top right
set tics in ; set grid ytics
set border 3 lw 0  ; set ytics nomirror
set pointsize $ptSZ
set size $sz
set term $outTerm
set output "$outFile"
plot \
 "output.txt" us 1 tit "sigma"  w li lt 1 lw 2 lc rgb "black",\
 "output.txt" us 2 tit "epsil"  w li lt 1 lw 2 lc rgb "red",\
 "output.txt" us 3 tit "delta"  w li lt 1 lw 2 lc rgb "magenta",\
 "output.txt" us 4 tit "prop "  w li lt 2 lw 1 lc rgb "blue",\
 "output.txt" us 5 tit "integ"  w li lt 3 lw 1 lc rgb "cyan",\
 "output.txt" us 6 tit "deriv"  w li lt 5 lw 1 lc rgb "orange",\
 "output.txt" us 7 tit "lambda" w li lt 1 lw 3 lc rgb "green"
EOF0

