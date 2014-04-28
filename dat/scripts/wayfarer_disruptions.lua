-- communicate with the wayfarer economy via
-- hackey var.push()s

include "table2string.lua"
include "ugens.lua"

function wayfarerCreateDisruption(systemName, commodityName, uGen, inDuration, inDistance)
  local currentDisruptions = hackadoodleIn("WF-disruptions")
  if currentDisruptions ~= nil then
    currentDisruptions = string2table(currentDisruptions)
  else
    currentDisruptions = {}
  end
  inDistance = inDistance or 4
  inDuration = inDuration or 10

  if systemName == nil then error("System Name cannot be nil") end
  if commodityName == nil then error("Commodity Name cannot be nil") end
  
  if inDuration > 15 then
    inDuration = 15
  end
  
  if inDistance > 7 then
    inDistance = 7
  end
    
  local ugentab = uGen
  -- allow simple constants to be specified if desired
  if type(ugentab) ~= "number" then
    ugen2table(uGen)
  end
  local wtf = {system=systemName, commodity=commodityName, ugenTable=ugentab, duration=inDuration, distance=inDistance}
  table.insert (currentDisruptions, wtf)

  hackadoodleOut("WF-disruptions", currentDisruptions)
end

-- overcome var length limit
function hackadoodleIn(varName)
  local len = var.peek(varName .. "_length")
  if len == nil then return nil end
  if len == 0 then
    var.pop(varName .. "_length")
    return nil
  end
  
  local str = ""
  for i=1, len do
    str = str .. var.peek(varName .. tostring(i))
    var.pop(varName .. tostring(i))
  end  
  var.pop(varName .. "_length")
  return string2table(str)
end

function hackadoodleOut(varName, aTable, chunkSize)
  chunkSize = chunkSize or 8192
  if false then
    for k, v in pairs(aTable) do
      print (k .. " " .. tostring(v))
    end
  end
  
  local str = table2string(aTable)
  local chunks = math.floor(string.len(str)/(chunkSize))

  var.push(varName .. "_length", chunks+1)
  for i=1, chunks+1 do
    var.push(varName .. tostring(i), string.sub(str, 1, chunkSize-1))
    str = string.sub(str, chunkSize)
  end
end
