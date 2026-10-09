#!/usr/bin/env python3
"""Generate the PowerPoint copy of a deck from its HTML source.

    python3 reports-src/make_pptx_copy.py reports-src/slides.qmd \\
        reports-src/slides-pptx.qmd

slides-pptx.qmd is not edited by hand: change slides.qmd, run this, render
both. The copy has the same slides in the same order, flattened into one
level (PowerPoint has no vertical stacks), with what the HTML theme draws
turned into what the PowerPoint template lays out (GUIDE.md section 8):

- the title page becomes the YAML title, subtitle and author;
- each row of number cards becomes a native table (titles as the header,
  then values with their units, then details); further rows of cards on the
  same slide add rows;
- notes boxes, small lines and figure captions become one short paragraph
  before the table or figure, which the template sets under it;
- speaker notes, lists and plain tables are kept.

Python standard library only; run with python3 (outside renv).
"""

import re
import sys

SETUP_RE = re.compile(r"```\{r\}\n#\| label: setup\n.*?\n```\n", re.S)
SLIDE_SPLIT_RE = re.compile(r"\n(?=#{1,2} )")
CARDS_RE = re.compile(r"::: \{\.cards\}\n.*?\n:::\n:::", re.S)
CARD_RE = re.compile(r"::: \{\.card [^}]*\}\n(.*?)\n:::", re.S)
VALUE_RE = re.compile(r"\[(.*?)\]\{\.big\}\s*\[(.*?)\]\{\.unit\}")
NOTE_RE = re.compile(r"::: \{\.note\}\n(.*?)\n:::", re.S)
SMALL_RE = re.compile(r"\[((?:[^\[\]]|\[[^\]]*\])*)\]\{\.small\}", re.S)
NOTES_RE = re.compile(r"::: \{\.notes\}\n.*?\n:::", re.S)
FIGURE_RE = re.compile(r"```\{r\}\n(.*?)```", re.S)
CAPTION_RE = re.compile(r"#\| fig-cap: !expr '(.*)'\n")
GRAPHIC_RE = re.compile(r"knitr::include_graphics\((.*)\)")


def span(text, name):
    """Text of the first [ ... ]{.name} span, or None."""
    match = re.search(r"\[([^\[\]]*)\]\{\." + name + r"\}", text)
    return " ".join(match.group(1).split()) if match else None


def front_matter(title_page):
    """YAML for the PowerPoint title slide, from the HTML title page."""
    fields = [("title", span(title_page, "title")),
              ("subtitle", span(title_page, "subtitle")),
              ("author", span(title_page, "byline"))]
    lines = ["---"]
    lines += [f'{key}: "{value}"' for key, value in fields if value]
    lines += [
        "date: today",
        "date-format: long",
        "format:",
        "  # Generated from slides.qmd by make_pptx_copy.py; do not edit",
        "  pptx:",
        "    reference-doc: slides-reference.pptx",
        "    output-file: slides.pptx",
        "---",
        "",
    ]
    return "\n".join(lines)


def parse_cards(block):
    """(title, value, unit, detail) for each card of a row."""
    cards = []
    for body in CARD_RE.findall(block):
        paras = [" ".join(p.split()) for p in body.strip().split("\n\n")]
        title = paras[0].strip("*")
        match = VALUE_RE.match(paras[1]) if len(paras) > 1 else None
        value, unit = match.groups() if match else (paras[1], "")
        detail = paras[2] if len(paras) > 2 else ""
        cards.append((title, value, unit, detail))
    return cards


def cards_table(rows):
    """A pipe table from one or more rows of cards."""
    first = rows[0]
    n = len(first)
    lines = ["| " + " | ".join(c[0] for c in first) + " |",
             "|" + "---|" * n,
             "| " + " | ".join(f"**{c[1]}** {c[2]}".strip()
                               for c in first) + " |"]
    if any(c[3] for c in first):
        lines.append("| " + " | ".join(c[3] for c in first) + " |")
    for row in rows[1:]:
        cells = [f"{c[0]}: **{c[1]}** {c[2]}".strip() for c in row]
        cells = (cells + [""] * n)[:n]
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)


def convert_slide(slide):
    """One slide of slides.qmd as a slide of the PowerPoint copy."""
    head, _, rest = slide.partition("\n")
    title = re.sub(r"\s*\{[^}]*\}\s*$", "", head).lstrip("#").strip()

    rows = []

    def take_cards(match):
        rows.append(parse_cards(match.group(0)))
        return "\n@@CARDS@@\n"

    rest = CARDS_RE.sub(take_cards, rest)

    lead = []

    def take_lead(match):
        # Line breaks are kept: a trailing backslash is a hand-set break
        lead.append(match.group(1).strip())
        return ""

    rest = NOTE_RE.sub(take_lead, rest)
    rest = SMALL_RE.sub(take_lead, rest)
    notes = NOTES_RE.findall(rest)
    rest = NOTES_RE.sub("", rest)

    figure = FIGURE_RE.search(rest)
    if figure:
        chunk = figure.group(1)
        caption = CAPTION_RE.search(chunk)
        graphic = GRAPHIC_RE.search(chunk)
        if caption:
            lead.insert(0, '`r gsub(" ?<br>", " ", '
                        + caption.group(1) + ")`")
        if graphic:
            rest = rest.replace(
                figure.group(0),
                "```{r}\nknitr::include_graphics("
                + graphic.group(1) + ")\n```")

    if rows:
        rest = rest.replace("\n@@CARDS@@\n", "\n" + cards_table(rows) + "\n",
                            1)
        rest = rest.replace("\n@@CARDS@@\n", "\n")
    rest = re.sub(r"\n{3,}", "\n\n", rest).strip("\n")

    parts = ["## " + title, ""]
    if lead:
        parts += [" ".join(lead), ""]
    if rest:
        parts.append(rest)
    if notes:
        parts += ["", *notes]
    return "\n".join(parts).rstrip() + "\n"


def main(source, target):
    text = open(source, encoding="utf-8").read()
    setup = SETUP_RE.search(text)
    if not setup:
        sys.exit(f"{source}: no setup chunk (#| label: setup)")
    body = text[setup.end():]
    slides = [s.strip("\n") for s in SLIDE_SPLIT_RE.split("\n" + body)
              if re.match(r"#{1,2} ", s.strip("\n"))]
    title_page = ""
    if slides and "{.title-page}" in slides[0].split("\n", 1)[0]:
        title_page = slides.pop(0)
    # here::i_am() names the file being rendered
    name = target.replace("\\", "/").split("/")[-1]
    setup_chunk = re.sub(r'(here::i_am\("(?:[^"]*/)?)[^"/]*("\))',
                         lambda m: m.group(1) + name + m.group(2),
                         setup.group(0))
    out = [front_matter(title_page), setup_chunk]
    out += [convert_slide(s) for s in slides]
    open(target, "w", encoding="utf-8").write("\n".join(out))


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2])
