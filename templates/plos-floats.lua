-- plos-floats.lua – PLOS ONE layout for figures and tables in DOCX
--
-- Register this file at post-render, after tbl-colwidths.lua:
--
--   filters:
--     - at: post-render
--       path: ../templates/plos-floats.lua
--
-- PLOS ONE wants each figure uploaded as a separate file with only its caption
-- in the manuscript, and each table as a plain cell-based table with its title
-- above it. Quarto 1.4 instead puts every captioned figure and table in a 1x1
-- container table (the docx FloatRefTarget renderer in Quarto's main.lua), so
-- the DOCX has the images in the body and every table nested in another table.
-- This filter replaces each container with:
--
--   figure: the caption paragraph only (the image is dropped);
--   table:  the caption paragraph, then the table itself.
--
-- The caption keeps the float's id on a wrapping Div, so in-text links such as
-- "Fig 1" still point at it. The "Fig 1." / "Table 1." label is moved into the
-- bold title written in the source caption ("**Title.** Legend."), which gives
-- the "**Fig 1. Title.** Legend." form of the PLOS formatting sample.
--
-- To render a review copy with the figures in place, remove this filter entry.

-- Return the float Div that a Quarto container table holds, or nil.
local function float_div(tbl)
  if #tbl.bodies ~= 1 then return nil end
  local rows = tbl.bodies[1].body
  if #rows ~= 1 or #rows[1].cells ~= 1 then return nil end
  local blocks = rows[1].cells[1].contents
  if #blocks ~= 1 or blocks[1].t ~= "Div" then return nil end
  local id = blocks[1].identifier
  if id:match("^fig%-") or id:match("^tbl%-") then return blocks[1] end
  return nil
end

-- True for the caption paragraph that Quarto's docxDivCaption() builds: it
-- starts with a raw OpenXML run that sets the ImageCaption style.
local function is_caption(block)
  if block.t ~= "Para" or #block.content == 0 then return false end
  local first = block.content[1]
  return first.t == "RawInline" and first.format == "openxml"
    and first.text:match("ImageCaption") ~= nil
end

-- Drop the ImageCaption style run and move the label into the bold title.
local function plos_caption(para)
  local inlines = pandoc.List(para.content)
  inlines:remove(1)
  for i, el in ipairs(inlines) do
    if el.t == "Strong" then
      el.content = pandoc.List({ table.unpack(inlines, 1, i - 1) }) .. el.content
      return pandoc.Para({ table.unpack(inlines, i) })
    end
  end
  return pandoc.Para(inlines)
end

function Table(tbl)
  local div = float_div(tbl)
  if div == nil then return nil end
  local out = pandoc.List()
  for _, block in ipairs(div.content) do
    if is_caption(block) then
      out:insert(pandoc.Div({ plos_caption(block) }, pandoc.Attr(div.identifier)))
    elseif block.t == "Table" then
      out:insert(block)
    end
  end
  return out
end
