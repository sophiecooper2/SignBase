-- alpha-labels.lua – item labels after Z (AA, AB, ...) for `alpha A` crossrefs
--
-- Register this file at post-render in a document that uses
-- `fig-labels: alpha A` or `tbl-labels: alpha A`:
--
--   filters:
--     - at: post-render
--       path: ../templates/alpha-labels.lua
--
-- Quarto's alpha labels count up through Unicode code points, so after "Z" the
-- 27th item is labelled "[" and the 33rd "a". PLOS ONE SI files continue with
-- AA, AB, ... This filter rewrites such a label where Quarto writes it: in
-- cross-reference links and at the start of captions. In both places the label
-- is the Str after the "Table"/"Fig" title and a non-breaking space.

local TITLES = { Table = true, Fig = true }

-- 1 -> "A", 26 -> "Z", 27 -> "AA", 53 -> "BA"
local function letters(n)
  local s = ""
  while n > 0 do
    local r = (n - 1) % 26
    s = string.char(65 + r) .. s
    n = (n - 1 - r) // 26
  end
  return s
end

-- Rewrite the first title, non-breaking space, label run in `inlines`.
local function relabel(inlines)
  for i = 1, #inlines - 2 do
    local a, b, c = inlines[i], inlines[i + 1], inlines[i + 2]
    if a.t == "Str" and TITLES[a.text] and b.t == "Str" and b.text == "\u{a0}"
        and c.t == "Str" and utf8.len(c.text) == 1 then
      local n = utf8.codepoint(c.text) - 64 -- "A" is item 1
      if n > 26 then c.text = letters(n) end
      return inlines
    end
  end
  return inlines
end

function Link(link)
  if link.classes:includes("quarto-xref") then
    link.content = relabel(link.content)
    return link
  end
end

-- Captions are the paragraphs that start with Quarto's ImageCaption style run.
function Para(para)
  local first = para.content[1]
  if first and first.t == "RawInline" and first.text:match("ImageCaption") then
    para.content = relabel(para.content)
    return para
  end
end
