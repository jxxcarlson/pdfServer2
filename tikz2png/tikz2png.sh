#!/bin/bash

# tikz2png.sh - Convert TikZ code to PNG
# Usage: ./tikz2png.sh "tikz code" [output.png] [dpi] [preamble]
#   or:  ./tikz2png.sh -f input.tikz [output.png] [dpi]

set -e  # Exit on error

# Default values
DPI=300
OUTPUT=""
TIKZ_CODE=""
PREAMBLE=""
FROM_FILE=false

# Parse arguments
if [ "$1" = "-f" ]; then
    FROM_FILE=true
    if [ ! -f "$2" ]; then
        echo "Error: File '$2' not found" >&2
        exit 1
    fi
    TIKZ_CODE=$(cat "$2")
    shift 2
else
    TIKZ_CODE="$1"
    shift
fi

# Check if TikZ code is provided
if [ -z "$TIKZ_CODE" ]; then
    echo "Usage: $0 \"tikz code\" [output.png] [dpi] [preamble]"
    echo "   or: $0 -f input.tikz [output.png] [dpi]"
    echo ""
    echo "Examples:"
    echo "  $0 '\draw (0,0) circle (1cm);' output.png 600"
    echo "  $0 -f mycode.tikz output.png"
    exit 1
fi

# Parse optional output filename, DPI, and preamble
if [ -n "$1" ]; then
    OUTPUT="$1"
fi
if [ -n "$2" ]; then
    DPI="$2"
fi
if [ -n "$3" ]; then
    PREAMBLE="$3"
fi

# Create temporary directory
OLDPWD=$(pwd)
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

# Generate LaTeX file
cat > "$TMPDIR/figure.tex" <<EOF
\documentclass[tikz,border=2mm]{standalone}
\usepackage{amsmath, amssymb}
\usepackage{physics}
\usepackage{tikz-3dplot}
\usetikzlibrary{calc,3d,backgrounds}
\tdplotsetmaincoords{70}{110}
$PREAMBLE
\begin{document}
$TIKZ_CODE
\end{document}
EOF

# Compile to PDF
cd "$TMPDIR"
pdflatex -interaction=nonstopmode figure.tex > /dev/null 2>&1

if [ ! -f figure.pdf ]; then
    echo "Error: PDF compilation failed" >&2
    echo "Check your TikZ code for errors" >&2
    exit 1
fi

# Convert to PNG
pdftoppm -png -r "$DPI" figure.pdf figure > /dev/null 2>&1

if [ ! -f figure-1.png ]; then
    echo "Error: PNG conversion failed" >&2
    exit 1
fi

# Determine output location
if [ -z "$OUTPUT" ]; then
    # Generate timestamped filename in current directory
    OUTPUT="$OLDPWD/tikz_$(date +%Y%m%d_%H%M%S).png"
else
    # Convert to absolute path if relative
    case "$OUTPUT" in
        /*) ;;  # Already absolute
        *) OUTPUT="$OLDPWD/$OUTPUT" ;;
    esac
fi

# Copy to output location
cp figure-1.png "$OUTPUT"

# Print the output path
echo "$OUTPUT"
