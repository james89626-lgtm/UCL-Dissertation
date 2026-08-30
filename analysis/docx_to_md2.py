import docx
from docx.document import Document
from docx.oxml.text.paragraph import CT_P
from docx.oxml.table import CT_Tbl
from docx.table import Table, _Cell
from docx.text.paragraph import Paragraph
from pathlib import Path

# Paths are resolved relative to this file, so the script runs from anywhere.
ROOT = Path(__file__).resolve().parents[2]      # the Dissertation folder
SRC = ROOT / "Can Machine Learning Beat the Market After Considering Trading Costs.docx"
OUT = ROOT / "dissertation-draft-user.md"


def iter_block_items(parent):
    if isinstance(parent, Document):
        parent_elm = parent.element.body
    elif isinstance(parent, _Cell):
        parent_elm = parent._tc
    else:
        raise ValueError("unsupported parent")
    for child in parent_elm.iterchildren():
        if isinstance(child, CT_P):
            yield Paragraph(child, parent)
        elif isinstance(child, CT_Tbl):
            yield Table(child, parent)

def heading_level(style_name):
    s = (style_name or "").lower()
    if s == "title":
        return 1
    if s.startswith("heading "):
        try:
            return int(s.split(" ")[1])
        except (IndexError, ValueError):
            return None
    return None

def merged_runs_to_md(para):
    # Merge consecutive runs with identical bold/italic state before wrapping,
    # so "5" + "." + " " + "5" doesn't become "**5****.**** ****5**".
    spans = []
    for r in para.runs:
        if not r.text:
            continue
        b, i = bool(r.bold), bool(r.italic)
        if spans and spans[-1][0] == b and spans[-1][1] == i:
            spans[-1] = (b, i, spans[-1][2] + r.text)
        else:
            spans.append((b, i, r.text))
    out = []
    for b, i, text in spans:
        if b and i:
            out.append(f"***{text}***")
        elif b:
            out.append(f"**{text}**")
        elif i:
            out.append(f"*{text}*")
        else:
            out.append(text)
    return "".join(out)

def para_to_md(para):
    lvl = heading_level(para.style.name)
    plain_text = "".join(r.text for r in para.runs).strip()
    if not plain_text:
        return ""
    if lvl:
        return f"{'#' * lvl} {plain_text}"
    if "list" in (para.style.name or "").lower():
        return f"- {merged_runs_to_md(para)}"
    return merged_runs_to_md(para)

def table_to_md(table):
    rows = []
    for i, row in enumerate(table.rows):
        cells = [c.text.strip().replace("\n", " ") for c in row.cells]
        rows.append("| " + " | ".join(cells) + " |")
        if i == 0:
            rows.append("|" + "---|" * len(cells))
    return "\n".join(rows)

doc = docx.Document(SRC)
out_lines = []
for block in iter_block_items(doc):
    if isinstance(block, Paragraph):
        md = para_to_md(block)
        if md:
            out_lines.append(md)
            out_lines.append("")
    elif isinstance(block, Table):
        out_lines.append(table_to_md(block))
        out_lines.append("")

with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(out_lines))

print(f"Wrote {len(out_lines)} lines to {OUT}")
