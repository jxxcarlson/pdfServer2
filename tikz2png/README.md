# tikz2png.sh - TikZ to PNG Converter

A bash script that converts TikZ code to PNG images.

## Requirements

- pdflatex (from TeX Live or similar)
- pdftoppm (from poppler-utils)

## Usage

```bash
./tikz2png.sh "tikz code" [output.png] [dpi]
./tikz2png.sh -f input.tikz [output.png] [dpi]
```

### Arguments

- `tikz code`: String containing valid TikZ code (including `\begin{tikzpicture}...\end{tikzpicture}`)
- `-f input.tikz`: Read TikZ code from a file instead
- `output.png`: Output filename (optional, defaults to timestamped file)
- `dpi`: Resolution in DPI (optional, defaults to 300)

## Examples

### Basic usage
```bash
./tikz2png.sh '\begin{tikzpicture}\draw (0,0) circle (1cm);\end{tikzpicture}' output.png
```

### Specify DPI
```bash
./tikz2png.sh '\begin{tikzpicture}\draw (0,0) circle (1cm);\end{tikzpicture}' output.png 600
```

### Auto-generate filename
```bash
./tikz2png.sh '\begin{tikzpicture}\draw (0,0) circle (1cm);\end{tikzpicture}'
# Creates: tikz_20241021_062345.png
```

### From file
```bash
./tikz2png.sh -f mycode.tikz output.png
```

### Complex example
```bash
./tikz2png.sh '\begin{tikzpicture}[scale=1.0]
  % Axes
  \draw[->] (-0.2,0) -- (4.2,0) node[right] {$x$};
  \draw[->] (0,-0.2) -- (0,3.2) node[above] {$y$};
  % Function
  \draw[thick,blue] plot[samples=100,domain=0:4] (\x,{sqrt(\x)});
  \node[blue] at (3,1.8) {$y=\sqrt{x}$};
\end{tikzpicture}' sqrt_plot.png
```

## Programmatic Usage

The script outputs the path to the generated PNG file, making it easy to use in other scripts:

```bash
# Capture the output filename
OUTPUT=$(./tikz2png.sh '\begin{tikzpicture}\draw (0,0) circle (1cm);\end{tikzpicture}')
echo "Created: $OUTPUT"

# Use in a pipeline
./tikz2png.sh '\begin{tikzpicture}\draw (0,0) circle (1cm);\end{tikzpicture}' | xargs open
```

## Available LaTeX Packages

The script includes these packages by default:
- `amsmath, amssymb` - Mathematical symbols and environments
- `physics` - Physics notation and commands

To use additional packages, modify the script's LaTeX template.

## How It Works

1. Wraps your TikZ code in a `standalone` document class
2. Compiles to PDF using `pdflatex`
3. Converts PDF to PNG using `pdftoppm`
4. Cleans up temporary files automatically

## Error Handling

If compilation fails, the script will exit with an error message. Common issues:
- Missing TikZ libraries (add `\usetikzlibrary{...}` to your code)
- Syntax errors in TikZ code
- Missing LaTeX packages

## Notes

- The `standalone` class with `border=2mm` ensures tight cropping with a small margin
- Higher DPI values (600+) produce sharper images but larger file sizes
- All temporary files are automatically cleaned up
