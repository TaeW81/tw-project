# tw-project

This repository contains AutoLISP utilities. The file `line-rotate.lsp` provides
functions to rotate text entities when the slope of a line changes.

All `TEXT` and `MTEXT` within a vertical band of ±4 units around the chosen line
are rotated to match the new angle.
