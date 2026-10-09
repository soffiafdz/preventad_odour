#!/usr/bin/env bash
# Screenshot every slide of a rendered revealjs deck, to check each one
# before the deck is shared (docs/STYLE.md, "Checking a deck").
#   reports-src/screenshot_slides.sh outputs/reports/slides.html <out_dir>
# Slides are found by their position, so slides stacked under a part (a
# level-1 heading) are included; files are named slide_<part>_<slide>.png.
# Needs Firefox (FIREFOX=/path/to/firefox if it is not on the PATH); on some
# machines Chrome's headless mode crashes or prints blank pages. The window
# size is the deck's width and height (SLIDE_SIZE, default 1600,900).
set -euo pipefail

if [ $# -ne 2 ]; then
  echo "usage: $0 <deck.html> <out_dir>" >&2
  exit 1
fi
deck=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
out=$2
firefox=${FIREFOX:-firefox}
size=${SLIDE_SIZE:-1600,900}

mkdir -p "$out"

# Reveal positions as h/v: a top-level <section> with the slide class is one
# slide (h/0); one without it wraps a vertical stack whose slides are h/0,
# h/1, ...
positions=$(grep -Eo '<section[^>]*>|</section>' "$deck" | awk '
  /^<\/section>/ { depth--; next }
  {
    depth++
    if (depth == 1) {
      h++; v = -1
      if ($0 ~ /class="([^"]* )?slide[ "]/) { print h - 1 "/0" }
    } else if (depth == 2) {
      v++; print h - 1 "/" v
    }
  }')

n=0
for pos in $positions; do
  png=$(printf "%s/slide_%02d_%02d.png" "$out" "${pos%/*}" "${pos#*/}")
  # A fresh profile for each slide: one profile reused for many screenshots
  # can hang
  profile=$(mktemp -d)
  "$firefox" --headless --no-remote --profile "$profile" \
    --window-size="$size" --screenshot "$png" "file://$deck#/$pos" \
    > /dev/null 2>&1
  rm -rf "${profile:?}"
  n=$((n + 1))
done
echo "$n slides saved in $out"
