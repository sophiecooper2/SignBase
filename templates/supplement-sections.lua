-- supplement-sections.lua – three-part S-numbers for supplement docs
--
-- Pre-Quarto filter (listed before built-in `quarto` processing, which is the
-- default position for user filters). It owns section numbering so source
-- headings carry no hardcoded numbers:
--
--   S1 file (supplement-number: 1): H1 n -> S1.<n>, H2 -> S1.<n>.<m>,
--     H3 -> S1.<n>.<m>.<k>
--   S2 file (supplement-number: 2): H2 n -> S2.<n>, H3 -> S2.<n>.<m>
--
-- Requires `number-sections: false` (Pandoc must not add its own numbers).
-- Tables and figures stay on native Quarto crossref with Table S / Figure S
-- titles; only `@sec-` cites are rewritten here, before Quarto sees them.
--
-- A header opts into numbering when it has a `sec-` identifier or its text
-- still carries a legacy manual prefix (e.g. "S5.5"). Headers with class
-- `unnumbered` or `unlisted` are skipped. `@tbl-` / `@fig-` cites and cites
-- with affixes other than lone parentheses are left untouched for Quarto /
-- citeproc. A lone parenthesised cite `(@sec-x)` keeps tight parens:
-- `(Section S..)` rather than Quarto's padded `( Section S.. )`.

local supp = nil
local sec_map = {}
local warned_no_supp = false

local function has_skip_class(el)
  if el.classes then
    for _, c in ipairs(el.classes) do
      if c == "unnumbered" or c == "unlisted" then
        return true
      end
    end
  end
  return false
end

-- Remove one leading legacy "S<n>[.<m>[.<k>]]" token, if present.
-- Operates on the inline list in place, preserving all other formatting.
local function strip_legacy_prefix(content)
  local first = content[1]
  if first and first.t == "Str"
      and first.text:match("^S%d+%.?%d*%.?%d*%.?$") then
    content:remove(1)
    while content[1] and content[1].t == "Space" do
      content:remove(1)
    end
    return true
  end
  return false
end

local function eligible_header(el)
  if el.t ~= "Header" or el.level > 3 then
    return false
  end
  if has_skip_class(el) then
    return false
  end
  if el.identifier and el.identifier:lower():match("^sec%-") then
    return true
  end
  if pandoc.utils.stringify(el.content):match("^S%d") then
    return true
  end
  return false
end

function Meta(meta)
  if meta["supplement-number"] ~= nil then
    supp = tonumber(pandoc.utils.stringify(meta["supplement-number"]))
  end
  return nil
end

-- Rewrite `@sec-` cites using the map built in the Pandoc pass below.
-- Runs on each Cite element; returns nil to leave tbl/fig cites for Quarto.
-- A Cite carrying exactly lone parentheses, `(@sec-x)` or `([-@sec-x])`,
-- is also handled: the parens are re-emitted tight, avoiding Quarto's
-- padded `( Section S.. )` rendering.
local function is_lone_paren(inlines, want)
  if inlines == nil or #inlines ~= 1 then
    return false
  end
  return pandoc.utils.stringify(inlines) == want
end

local function rewrite_sec_cite(el)
  local texts = {}
  local wrap_parens = false
  if #el.citations == 1 then
    local single = el.citations[1]
    if is_lone_paren(single.prefix, "(")
        and is_lone_paren(single.suffix, ")") then
      wrap_parens = true
    elseif (single.prefix and #single.prefix > 0)
        or (single.suffix and #single.suffix > 0) then
      return nil
    end
  end
  for _, cit in ipairs(el.citations) do
    local idl = cit.id:lower()
    if not idl:match("^sec%-") then
      return nil
    end
    if not wrap_parens then
      if (cit.prefix and #cit.prefix > 0)
          or (cit.suffix and #cit.suffix > 0) then
        return nil
      end
    end
    local label = sec_map[idl]
    if not label then
      io.stderr:write(
        "[supplement-sections] unknown sec id: " .. cit.id .. "\n")
      label = "??"
    end
    if cit.mode == "SuppressAuthor" then
      texts[#texts + 1] = label
    else
      texts[#texts + 1] = "Section " .. label
    end
  end
  local inlines = pandoc.List({})
  if wrap_parens then
    inlines:insert(pandoc.Str("("))
  end
  for i, t in ipairs(texts) do
    inlines:insert(pandoc.Str(t))
    if i < #texts then
      inlines:insert(pandoc.Str(";"))
      inlines:insert(pandoc.Space())
    end
  end
  if wrap_parens then
    inlines:insert(pandoc.Str(")"))
  end
  return pandoc.Span(inlines)
end

function Pandoc(doc)
  if supp == nil then
    if not warned_no_supp then
      io.stderr:write(
        "[supplement-sections] WARNING: supplement-number not set, " ..
        "defaulting to 1\n")
      warned_no_supp = true
    end
    supp = 1
  end

  local sec, sub, subsub = 0, 0, 0
  local h1_seen = false

  for _, blk in ipairs(doc.blocks) do
    if eligible_header(blk) then
      local lvl = blk.level
      local label = nil
      if lvl == 1 then
        sec = sec + 1
        sub, subsub = 0, 0
        h1_seen = true
        label = "S" .. tostring(supp) .. "." .. tostring(sec)
      elseif lvl == 2 then
        if h1_seen then
          sub = sub + 1
          subsub = 0
          label = "S" .. tostring(supp) .. "." .. tostring(sec)
            .. "." .. tostring(sub)
        else
          -- S2 shape: H2 tops the hierarchy
          sec = sec + 1
          sub, subsub = 0, 0
          label = "S" .. tostring(supp) .. "." .. tostring(sec)
        end
      elseif lvl == 3 then
        if h1_seen then
          subsub = subsub + 1
          label = "S" .. tostring(supp) .. "." .. tostring(sec)
            .. "." .. tostring(sub) .. "." .. tostring(subsub)
        else
          sub = sub + 1
          label = "S" .. tostring(supp) .. "." .. tostring(sec)
            .. "." .. tostring(sub)
        end
      end
      if label then
        if blk.identifier and blk.identifier ~= "" then
          sec_map[blk.identifier:lower()] = label
        end
        strip_legacy_prefix(blk.content)
        blk.content:insert(1, pandoc.Space())
        blk.content:insert(1, pandoc.Str(label))
      end
    end
  end

  doc.blocks = pandoc.walk_block(
    pandoc.Div(doc.blocks), { Cite = rewrite_sec_cite }).content
  return doc
end
