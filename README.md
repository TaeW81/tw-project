# tw-project

This repository contains a simple AutoLISP routine for AutoCAD Korean
environment. Load `profileedit.lsp` in AutoCAD to use the `PROFILEEDIT`
command. The command adjusts a selected line based on nearby numeric
text values. After moving the line's endpoints up or down by 4 units, it
rotates numeric text near the endpoints and any text within a 5-unit
radius of the midpoint so that all text follows the updated slope.
