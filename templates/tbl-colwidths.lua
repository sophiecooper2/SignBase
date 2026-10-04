-- tbl-colwidths.lua – DOCX column widths and left-aligned table cells
--
-- Register this file at BOTH entry points:
--
--   filters:
--     - ../templates/tbl-colwidths.lua          # pre-quarto (default)
--     - at: post-render
--       path: ../templates/tbl-colwidths.lua
--
-- It handles two separate defects in Quarto 1.4's DOCX table output.
--
-- 1. Column widths. Quarto's `tbl-colwidths` cell option is gated by format
--    (share/schema/cell-table.yml declares formats [$pdf-all, $html-all]), so
--    it is silently inert for docx: a table with
--    `#| tbl-colwidths: [15, 30, 25, 30]` renders with equal columns, exactly
--    like a table with no widths at all. Quarto does still copy the option onto
--    the Table's attributes, so the values are picked up here and written into
--    the AST column widths, which pandoc turns into <w:gridCol> proportions.
--    The scale is percentages of table width, matching Quarto's own scale, so
--    the same numbers mean the same thing if a document is ever rendered to
--    pdf or html.
--
-- 2. Left-aligned cells. Quarto wraps every captioned table in a 1x1 container
--    table so the caption can sit beside it, and that container cell defaults
--    to centred. Pandoc propagates a parent cell's paragraph alignment into
--    nested table cells and emits it *in addition to* the nested cell's own
--    alignment, so a trailing <w:jc w:val="center"/> lands after each cell's own
--    alignment and wins. That is why every captioned table renders centred here
--    regardless of the `align=` argument passed to kable(). The container is
--    only built at `post-render`, so that is where it is left-aligned; both
--    values then agree on "left".
--
-- Both jobs are idempotent, which is what lets one file serve both entry
-- points: at `pre-quarto` the container does not exist yet and widths are
-- applied; at `post-render` the widths are already set and the container is
-- fixed.

local warned = {}

local function warn_once(key, msg)
  if warned[key] then return end
  warned[key] = true
  io.stderr:write("[tbl-colwidths] " .. msg .. "\n")
end

-- Normalise a `tbl-colwidths` attribute value to a list of numbers.
-- Quarto hands the option over as the literal string "[15,30,25,30]" on some
-- paths and as a real list on others, so both are accepted, with or without
-- the brackets.
local function width_numbers(value)
  local nums = {}
  if type(value) == "table" then
    for _, v in ipairs(value) do
      -- each item may be a number or a single-element attribute list
      if type(v) == "table" then
        v = v[1]
      end
      if v ~= nil then
        local n = tonumber(pandoc.utils.stringify(v))
        if n == nil then return nil, "non-numeric entry" end
        nums[#nums + 1] = n
      end
    end
  elseif type(value) == "string" then
    local body = value:gsub("%s", ""):gsub("^%[", ""):gsub("%]$", "")
    for piece in body:gmatch("[^,]+") do
      local n = tonumber(piece)
      if n == nil then return nil, "non-numeric entry" end
      nums[#nums + 1] = n
    end
  else
    return nil, "unrecognised value type"
  end
  if #nums == 0 then return nil, "empty" end
  return nums
end

-- Fit `nums` to `ncol` columns and return fractions summing to 1.
--
-- The count must match exactly, or be a single value meaning "share the table
-- equally". Padding a short list with its last entry was tried and is a trap:
-- [50, 50] on six columns reads as "two halves" but pads to [50,50,50,50,50,50],
-- which normalises to six equal columns, i.e. silently discards the request.
local function fit_widths(nums, ncol)
  if #nums == 1 then
    for i = 2, ncol do nums[i] = nums[1] end
  elseif #nums ~= ncol then
    return nil, "expected 1 or " .. ncol .. " values for " .. ncol .. " columns, got " .. #nums
  end
  local total = 0
  for i = 1, ncol do
    if nums[i] == nil or nums[i] <= 0 then return nil, "widths must be positive" end
    total = total + nums[i]
  end
  local out = {}
  for i = 1, ncol do out[i] = nums[i] / total end
  return out
end

-- Left-align every column of a table.
local function left_align(tbl)
  for i = 1, #tbl.colspecs do
    tbl.colspecs[i][1] = "AlignLeft"
  end
end

local function apply_widths(tbl)
  local raw = tbl.attributes["tbl-colwidths"]
  if raw == nil then return end
  tbl.attributes["tbl-colwidths"] = nil
  local ncol = #tbl.colspecs
  if ncol == 0 then return end
  local nums, err = width_numbers(raw)
  if nums == nil then
    warn_once("parse", "cannot read tbl-colwidths (" .. err .. "); "
      .. "columns left equal. Use percentages, e.g. [15, 30, 25, 30].")
    return
  end
  local fracs, ferr = fit_widths(nums, ncol)
  if fracs == nil then
    warn_once("value", "tbl-colwidths rejected (" .. ferr
      .. "); columns left equal.")
    return
  end
  for i = 1, ncol do
    tbl.colspecs[i][2] = fracs[i]
  end
end

-- Left-align the container tables Quarto builds for captions. Only meaningful at
-- post-render; before that no such container exists.
local function left_align_containers(el)
  if el.t ~= "Div" then return end
  local is_cell = false
  for _, c in ipairs(el.classes or {}) do
    if c == "cell" then is_cell = true end
  end
  if not is_cell then return end
  for _, b in ipairs(el.content) do
    if b.t == "Table" then left_align(b) end
  end
end

-- Element handlers only. Quarto runs user filters in an emulated environment
-- where neither pandoc.walk nor _quarto.ast.walk is exposed, so the filter chain
-- does the traversal. Each handler must return the element: returning nil does
-- not commit the in-place edits to the column specs.
function Table(el)
  apply_widths(el)
  left_align(el)
  return el
end

function Div(el)
  left_align_containers(el)
  return el
end
