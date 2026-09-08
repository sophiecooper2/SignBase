-- supplement-xref.lua – resolve cross-document supplement section references
--
-- paper.qmd cannot use native `@sec-` cites to reach a separately rendered
-- supplement (Quarto resolves cross-refs only within one document/project).
-- Instead, each supplement render (via supplement-sections.lua) exports an
-- id->label map to `s<n>-sec-map.json` beside the .qmd. This filter loads those
-- maps and rewrites namespaced cites so the number is resolved at render time:
--
--   @s1-sec-s2      -> Section S1.2
--   [-@s1-sec-s2]   -> S1.2                       (bare, for "S1 §[-@...]")
--   (@s1-sec-s2)    -> (Section S1.2)             (tight parens)
--   @s2-sec-x       -> resolved against the S2 map
--
-- The `s1-`/`s2-` prefix selects the supplement; the remainder is the target
-- header's identifier (with the prefix stripped). Any cite whose id does not
-- start with `s1-`/`s2-` is left untouched for Quarto / citeproc.
--
-- Map paths default to `s1-sec-map.json` / `s2-sec-map.json` in the working
-- directory (Quarto runs pandoc with cwd = the input document's folder). Override
-- with `s1-sec-map:` / `s2-sec-map:` in the document front matter.

local maps = {}          -- supp_number -> { id = label }
local loaded = {}        -- supp_number -> true if file read
local warned_missing = {}
local map_paths = { [1] = "s1-sec-map.json", [2] = "s2-sec-map.json" }

function Meta(meta)
  if meta["s1-sec-map"] ~= nil then
    map_paths[1] = pandoc.utils.stringify(meta["s1-sec-map"])
  end
  if meta["s2-sec-map"] ~= nil then
    map_paths[2] = pandoc.utils.stringify(meta["s2-sec-map"])
  end
  return nil
end

local function load_map(supp, path)
  if loaded[supp] then return maps[supp] end
  loaded[supp] = true
  local fh = io.open(path, "r")
  if not fh then
    warned_missing[supp] = path
    io.stderr:write(
      "[supplement-xref] WARNING: no section map at '" .. path .. "'. " ..
      "Render the supplement first (e.g. quarto render paper/S" .. supp ..
      "-*.qmd) so cross-document section numbers resolve.\n")
    maps[supp] = {}
    return maps[supp]
  end
  local text = fh:read("*a")
  fh:close()
  local m = {}
  -- Flat JSON object of quoted string pairs; the numeric "supplement" field and
  -- the "labels" object brace are skipped because their values are unquoted.
  for k, v in text:gmatch('"([%w_%-]+)"%s*:%s*"([%w%.]+)"') do
    m[k:lower()] = v
  end
  maps[supp] = m
  return m
end

local function is_lone_paren(inlines, want)
  if inlines == nil or #inlines ~= 1 then
    return false
  end
  return pandoc.utils.stringify(inlines) == want
end

local function resolve(id)
  local supp = id:match("^s1%-") and 1 or (id:match("^s2%-") and 2 or nil)
  if not supp then return nil end
  local key = id:gsub("^s[12]%-", ""):lower()
  local m = load_map(supp, map_paths[supp])
  local label = m[key]
  if not label then
    io.stderr:write(
      "[supplement-xref] unknown section id in cite: " .. id ..
      " (looked up '" .. key .. "' in " .. map_paths[supp] .. ")\n")
    return "??"
  end
  return label
end

function rewrite_xref_cite(el)
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
    if not (idl:match("^s1%-") or idl:match("^s2%-")) then
      return nil
    end
    if not wrap_parens then
      if (cit.prefix and #cit.prefix > 0)
          or (cit.suffix and #cit.suffix > 0) then
        return nil
      end
    end
    local label = resolve(cit.id)
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
  doc.blocks = pandoc.walk_block(
    pandoc.Div(doc.blocks), { Cite = rewrite_xref_cite }).content
  return doc
end
