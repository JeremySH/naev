--[[ 

A simple table saver that doesn't use loadstring(). I'm sure there's a better one
out there somewhere, but I couldn't find one without loadstring(), so I made
my own. It saves only pure lua types, and never functions. It's not very error-tolerant, 
but it's  compatible with itself, which is good enough.

to save:
mystring = table2string(myTable)

to load:
myTable = string2table(myString)


string format is like this:
[element count]
[types]index value
  [types]index value

[types] contain two type ids, the first for the key type, the second for the value type

types are: n, s, b, 0, u
AKA: number, string, boolean, nil, unknown

Unknowns are bad, and will be treated as nil on load.

when a nested table appears, "value" is the element count. It might look like this:
stmyTable 2
  ssname  Joe
  snheight  5.6

The 2 here is the count of elements.

Nesting is done through tabs.

--]]
function table2string(theTable, depth)
	local thisDepth = depth

	if not depth then thisDepth = 1 end

  -- hopefully one doesn't nest past this point. That would be insane.
	local bunchaTabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
	
	local tabs = string.sub(bunchaTabs, 1, thisDepth-1)
	
  local tableString = ""
  local count = 0
  
  -- count of elements
  for _,_ in pairs(theTable) do
    count = count + 1
  end
  
  tableString = tabs .. tostring(count) .. "\n"
  
  for k,v in pairs(theTable) do
    
    -- prepend tabs at current depth
    tableString = tableString .. tabs
  
    -- key type character
    if type(k) == 'string' then
      tableString = tableString .. 's'
    elseif type(k) == 'number' then
      tableString = tableString .. 'n'
    else
      -- UNKNOWN!
      tableString = tableString .. 'u'
    end
    
    -- value type character
    if type(v) == 'string' then
      tableString = tableString .. 's'
    elseif type(v) == 'number' then
      tableString = tableString .. 'n'
    elseif type(v) == 'boolean' then
      tableString = tableString .. 'b'
    elseif type(v) == 'nil' then
      tableString = tableString .. '0'
    elseif type(v) == 'table' then
      tableString = tableString .. 't'
    else
      -- UNKNOWN!
      tableString = tableString .. 'u'
    end

    -- key and value
    if type(v) == 'table' then
      local count = 0
      for _, _ in pairs(v) do
        count = count+1
      end
      tableString = tableString .. __escape(k) .. "\t" .. count .. "\n"

      -- recurse into nested table
      tableString = tableString .. table2string(theTable[k], thisDepth+1)
    else
      -- key and value
      tableString = tableString .. __escape(k) .. "\t" ..  __escape(tostring(v)) .. "\n"
    end
  end
  return tableString
end

function string2table(inString, depth)
	local bunchaTabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"

  local str = inString
  local rest = ""
  local tableOut = {}
    
  local thisDepth = depth
  if not depth then thisDepth = 1 end

	local tabs = string.sub(bunchaTabs, 1, thisDepth-1)

  -- first, the count of items
  str,rest = _grabline(str)
  
  local count = tonumber(__unescape(string.sub(str, thisDepth-1)))
  str = rest
  

  for i=1, count do
    str, rest = _grabline(str)

    if string.len(str) == 0 then
      break
    end
    
    -- jump the tabs
    str = string.sub(str, thisDepth)
    
    -- crudely grab everything in the line
    local keyType = string.sub(str, 1,1)
    local valueType = string.sub(str,2,2)
    local keyEnd = string.find(str, "\t")
  
    local theKey = __unescape(string.sub(str, 3, keyEnd-1))
    local theValue = __unescape(string.sub(str, keyEnd+1))
  
    --print(string.format("Key type, value type: %s, %s", tostring(keyType), tostring(valueType) ) )

    if keyType == 's' then
      theKey = theKey
    elseif keyType == 'n' then
      theKey = tonumber(theKey)
    end
     
    if valueType == 's' then
      tableOut[theKey] = theValue
    elseif valueType == 'n' then
      tableOut[theKey] = tonumber(theValue)
    elseif valueType == 'b' then
      if theValue == "true" then
        tableOut[theKey] = true
      else
        tableOut[theKey] = false
      end
    elseif theType == 'nil' then
        tableOut[theKey] = nil
    elseif valueType == 't' then
      tableOut[theKey], rest = string2table(rest, thisDepth+1)
    else
      tableOut[theKey] = nil
    end

   --print(string.format("Key, Value: %s, %s", tostring(theKey), tostring(tableOut[theKey]) ) )
    
    str = rest
  end
  return tableOut, str
end


function _grabline(str)
  -- return first line of text, and the rest of the string,
  -- discarding the newline
  local theEnd = string.find(str, "\n")
  if not theEnd then
    theEnd = string.len(str)
  end
  return string.sub(str, 1, theEnd-1), string.sub(str, theEnd+1)
end

-- http://luacode.wordpress.com/2011/08/18/lua-escape-and-unescape-function-code/
-- kind of overkill, since I only need to escape tabs and newlines
function __escape(s)
	s = string.gsub(s, "([&=+%c])", function(c)return string.format("%%%02X", string.byte(c))end)
	return s
end

function __unescape(s)
	s = string.gsub(s, "%%(%x%x)", function(h)return string.char(tonumber(h, 16))end)
	return s
end