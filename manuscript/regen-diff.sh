#!/usr/bin/env bash
# Regenerate ms-jqas-r1-diff.{tex,pdf} (latexdiff of ms-jqas.tex vs ms-jqas-r1.tex).
# Post-processes the .tex to render deleted subsection headings as unnumbered
# starred subsections so they don't share a section number with a nearby new heading.
set -euo pipefail
cd "$(dirname "$0")"

OLD=ms-jqas.tex
NEW=ms-jqas-r1.tex
DIFF=ms-jqas-r1-diff

latexdiff "$OLD" "$NEW" > "$DIFF.tex"

# Collapse latexdiff's "\DIFdelbegin \subsection{\DIFdel{...}} %DIFAUXCMD
# \addtocounter{subsection}{-1}%DIFAUXCMD \DIFdelend" pattern (a deleted, but
# still-numbered subsection heading that visually clashes with the new heading
# that takes its number) into an unnumbered \subsection*{} so only the new
# heading carries the section number.
perl -i -0pe 'for my $lvl (qw(section subsection subsubsection)) {
  s|\\DIFdelbegin\s+\\\Q$lvl\E\{\\DIFdel\{([^}]*)\}\}\s*\n%DIFAUXCMD\n\\addtocounter\{\Q$lvl\E\}\{-1\}%DIFAUXCMD\n\\DIFdelend|\\DIFdelbegin \\${lvl}*\{\\DIFdel\{$1\}\}\n\\DIFdelend|g;
}' "$DIFF.tex"

pdflatex -interaction=nonstopmode -halt-on-error "$DIFF.tex" > /dev/null
bibtex "$DIFF" > /dev/null
makeglossaries "$DIFF" > /dev/null
pdflatex -interaction=nonstopmode -halt-on-error "$DIFF.tex" > /dev/null
pdflatex -interaction=nonstopmode -halt-on-error "$DIFF.tex" > /dev/null

echo "Built $DIFF.pdf"
