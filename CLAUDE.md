# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell-based PDF and image processing server built with the Scotty web framework. The server provides REST APIs for:
- Converting LaTeX documents to PDF with comprehensive error reporting
- Creating tar archives of LaTeX documents with images
- Processing and uploading images to Cloudflare

## Build and Development Commands

### Building the Application
```bash
# Standard build
stack build

# Build and deploy (preferred method)
./start.sh  # Pulls latest, builds, stops old server, starts new one
```

### Running the Server
```bash
# Development mode (local testing)
stack run  # Runs on port 3000

# Production deployment
./start.sh  # Handles full build/restart cycle with systemd or background process
```

### Testing
```bash
# Local test endpoint
curl http://localhost:3000/hello

# Important: Do NOT use https on localhost - only HTTP is supported locally
```

## Architecture

### Core Components

1. **Main Server** (`app/Main.hs`): Scotty web server running on port 3000
   - Routes: `/pdf`, `/json`, `/tar`, `/image`, `/tikz2png`, `/hello`
   - Middleware: CORS (allows all origins), static file serving, request logging
   - Error handling: Returns different HTTP status codes based on PDF generation results

2. **Document Processing** (`src/Document.hs`): Handles LaTeX document parsing and preparation
   - Downloads images from URLs using curl
   - Validates images (must be >100 bytes)
   - Replaces failed image downloads with LaTeX placeholder boxes
   - Copies required packages from `./package` to `./inbox`
   - Returns list of failed images for error reporting

3. **PDF Generation** (`src/Pdf.hs`): LaTeX to PDF conversion using xelatex
   - Runs xelatex 3 times for complete TOC/reference resolution
   - 30-second timeout per run to prevent infinite loops
   - Creates PDFs in the `outbox/` directory
   - Advanced error handling with three result types:
     - `PdfSuccess`: Clean compilation
     - `PdfWithErrors`: PDF created but with errors (returns PDF + error report + JSON)
     - `PdfError`: Complete failure (no PDF generated)
   - When errors occur, generates both text and JSON error reports

4. **Error Reporting System** (`src/Pdf.hs` + `src/Concordance.hs`):
   - **Concordance Module**: Maps LaTeX line numbers to original source line numbers
   - Parses `%%% Line N` annotations in LaTeX source to track origins
   - Extracts error line numbers from xelatex log files
   - Generates JSON error records with structure: `{scripta-line, latex-line, latex-begin, latex-end, latex-text}`
   - Combines multiple errors on same source line with numbered latex-text fields
   - Deduplicates errors by scripta-line only
   - Annotates error logs with `[scriptaErrorAt N]` markers pointing to source lines
   - **Preamble errors**: Errors before first `%%% Line` annotation return `scripta-line: 0` to indicate they're in the document preamble

5. **Tar Archive Creation** (`src/Tar.hs`): Bundles LaTeX documents with images
   - Creates temporary directory structure with document name as base
   - Includes all images from `image/` directory in the archive
   - Creates tar file in `outbox/` directory

6. **Image Processing** (`src/Image.hs`):
   - Handles Cloudflare image uploads via multi-part form data
   - Requests one-time upload URLs from Cloudflare API
   - Downloads images to `cf-image/` directory using wget
   - Requires environment variables: `CF_ACCOUNT_ID`, `CF_API_KEY`

7. **TikZ to PNG Conversion** (`src/Tikz.hs`):
   - Converts TikZ graphics code to PNG images
   - Uses `tikz2png/tikz2png.sh` shell script for conversion pipeline
   - Process: TikZ → LaTeX → PDF (via pdflatex) → PNG (via pdftoppm)
   - Uploads generated PNG to Cloudflare for hosting
   - Returns Cloudflare image URL on success
   - Requires `pdflatex` and `pdftoppm` (from poppler-utils)

### API Endpoints

- `POST /pdf`: Convert LaTeX to PDF
  - Input: `{"id": "doc-id.tex", "content": "LaTeX content", "urlList": [{url, filename}], "packageList": []}`
  - Returns: JSON with `{pdf, errorReport, errorJson, hasErrors}` fields
  - Status: 200 for success or partial success, 400 for complete failure

- `POST /json`: Convert LaTeX to PDF (returns PdfResult JSON)
  - Same input as `/pdf`
  - Returns: Full PdfResult object with success/error details
  - Status: 200 for success/partial, 400 for complete failure

- `POST /tar`: Create tar archive
  - Same input as `/pdf`
  - Returns: Document ID as plain text

- `POST /image`: Process and upload images to Cloudflare
  - Input: `{"url": "image-url", "filename": "name.ext", "username": "user"}`
  - Returns: Cloudflare upload response

- `POST /tikz2png`: Convert TikZ graphics to PNG
  - Input: `{"name": "output-name", "content": "TikZ code"}`
  - Returns: JSON with `{name, url}` on success or `{name, error}` on failure
  - Status: 200 for success, 400 for failure

- `GET /pdf/:id`: Download generated PDF or error report
- `GET /tar/:id`: Download tar archive
- `GET /hello`: Health check endpoint

### Directory Structure
- `inbox/`: Incoming LaTeX files (written here before compilation)
- `outbox/`: Generated PDFs, error reports, JSON error data, and tar archives
- `image/`: Downloaded images for LaTeX documents
- `cf-image/`: Cloudflare image cache (temporary storage before upload)
- `package/`: Custom LaTeX packages that can be included in documents
- `save/`: Debug copies of LaTeX source files

### Key Data Types

**Document** (`src/Document.hs`):
```haskell
data Document = Document Text Text [ImageElement] [Text]
-- Fields: id, content, urlList, packageList
```

**ImageElement**:
```haskell
data ImageElement = ImageElement { url :: String, filename :: String }
```

**PdfResult** (`src/Pdf.hs`):
```haskell
data PdfResult =
    PdfSuccess { filename :: Text }
  | PdfError { error :: Text, errorLog :: Text }
  | PdfWithErrors { pdfFile :: Text, errorPdfFile :: Text, errorJsonData :: [ErrorRecord], errorMsg :: Text }
```

**ErrorRecord**:
```haskell
data ErrorRecord = ErrorRecord
  { scriptaLine :: Int    -- Source line number
  , latexLine :: Int      -- LaTeX line number
  , latexBegin :: Int     -- Paragraph start
  , latexEnd :: Int       -- Paragraph end
  , latexText :: String   -- LaTeX code causing error
  }
```

## Dependencies

### Haskell Dependencies
The project uses Stack with package.yaml. Key dependencies:
- scotty (web framework)
- aeson (JSON processing)
- wai, wai-cors, wai-middleware-static, wai-extra (middleware)
- http-client, http-client-tls (HTTP operations)
- MissingH (utility functions)
- process (running external commands like xelatex)

### System Dependencies
LaTeX packages and tools required for document compilation and image generation:
- `texlive-xetex` (provides xelatex for PDF generation)
- `texlive-latex-extra` (common LaTeX packages)
- `texlive-science` (provides mhchem for chemistry notation)
- `poppler-utils` (provides pdftoppm for PDF to PNG conversion, required for `/tikz2png` endpoint)

Install on Ubuntu/Debian:
```bash
apt-get install texlive-xetex texlive-latex-extra texlive-science poppler-utils
```

**Note**: The required runtime directories (`outbox/`, `cf-image/`, `inbox/`, `image/`, `save/`) are created automatically if they don't exist, but for a fresh deployment you can create them manually:
```bash
mkdir -p outbox cf-image inbox image save
```

## Production Deployment

Server runs at `http://pdfServ.app` on DigitalOcean. The executable is deployed to `/root/pdfServer/pdfServer`.

**Deployment process:**
1. Run `./start.sh` which:
   - Pulls latest changes from git
   - Runs `stack build`
   - Stops existing server (systemd or pkill)
   - Copies built executable to `/root/pdfServer/pdfServer`
   - Starts server (via systemd service or background process)
   - Verifies server is running on port 3000

## Error Handling Strategy

The server has sophisticated error handling for LaTeX compilation:

1. **Image Download Failures**: Replaced with placeholder boxes containing URL info
2. **Compilation Timeouts**: Detected (exit code 124), returns error message
3. **Partial Success**: If PDF created but xelatex reports errors:
   - Returns the (potentially damaged) PDF
   - Generates text error report with filtered log
   - Generates JSON error data mapping errors to source lines
4. **Complete Failure**: Generates comprehensive error PDF with diagnostics
5. **Concordance Tracking**: Maps LaTeX errors back to original source using `%%% Line N` annotations

## Important Notes

- Server port is 3000 (not 3001 as mentioned in old docs)
- xelatex runs 3 times to resolve cross-references (books, TOC, etc.)
- Each xelatex run has 30-second timeout to prevent hanging
- Log files are preserved when compilation fails (for debugging)
- The concordance system relies on `%%% Line N` comments in LaTeX source
- JSON error output uses one element per line for readability
- **Preamble errors**: Errors occurring before the first `%%% Line` annotation (typically in the document preamble) are reported with `scripta-line: 0`, `latex-begin: 0`, and `latex-end: 0` since they cannot be mapped to source lines
