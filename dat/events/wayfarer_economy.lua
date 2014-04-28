-- wayfarer is an economy system for NAEV, where price differences are based on 
-- travel distance plus a layer of cusomizable fluctuation. The main idea behind this
-- is to keep the system tweakable for gameplay design.

-- it requires two C code changes:
-- 1. planet:setPriceAt() -- must be implemented
-- 2. planet:getCommoditiesSold() -- must use Commodity*, not tech groups.

-- these are functions useful for other reasons, so they are include files.
include "table2string.lua"
include "ugens.lua"

debugging = true

startSystem = "Gamma Polaris" -- first system to start the crawl.


-- BASIC GAMEPLAY TWEAKS
typicalTonnage = 20 -- typical units delivered per trade in tons. Raise to be mean to Mule owners.
revenuePerJump = 1200 -- revenue per jump in credits. Raise to increase base profit.
percentageRevenue = 5. -- percent to increase price per jump distance, raise to make high price goods more profitable.
roundTo = 5 -- round all prices to this multiple.
deepestJump = 15 -- deepest jump to consider "valid". Should be > 8 for NAEV, or you'll get nils.
distancePower = 1.2 -- increase to create higher prices as a system becomes more isolated

-- fixer-uppers
invertEverything = false -- invert the calculation so low is high and vice versa
randomInversion = false -- invert some commodities' calculation to make return routes interesting.
priceOffset = 0. -- -300. -- use care. all prices are shifted by this number.

useFluctuation = true -- use disruptions and gradients
randomPepper = false -- randomize each commodity with a 0.02 swing for fun

lockPrices = true -- lock prices until player changes systems (a landing fee would be better, but ah well)

if debugging then
  lockPrices = false
end
--------------
allSystems = {}
commoditySystems = {}
allCommodities = {}
calculatedSystems = {}

lastSystem = nil
lastSystemPrices = {}

-- these are for economic disruptions (which are saved) and gradients (which are not saved)
-- that can be set by other lua files using the wayfarer_disruptions.lua interface.
-- Disruptions are temporary fluctuations that propogate outwards from the source system
-- while gradients are more of a stable characteristic of an area.
disruptions = {}
gradients = {}

stpConversion = 10000.*5000.


function debugMsg(str)
  if debugging then
    -- chunking doesn't really help matters.
    while str ~= "" do
      io.write(string.sub(str, 1, 1024))
      str = string.sub(str, 1025)
    end
    io.write("\n")
  end
end

function create()
  local chosen

  _, chosen = tk.choice("Experimental Economy?", "Do you wish to use the experimental economy? NOTE: prices are different than usual!", "Yes", "No")

  if chosen == "No" then
    evt.finish(true)
    return
  end

  discoverSystems()

  if debugging then
    --destroyDisruptions() -- clear save game data and start fresh
    
    -- (example) they are rich in gamma polaris
    addGradient("Gamma Polaris", "Luxury Goods", .08,5)

    -- (example) Dvaer is volitile because of war
    addGradient("Darkstone", "Medicine", ugenSin(2, .1, .1), 5)
    addGradient("Darkstone", "Food", ugenVolitility(1, -.1, .1, 4), 5)
    
    -- (example) uninhabited space has a lot of scrap
    addGradient("Pilatis", "Scrap", -.3, 6)
  end

  loadDisruptions()
  purgeElapsedDisruptions()

  -- this should just be "onCommodity" but we have to bash the internal
  -- economy calculations to listen to us
  hook.enter("onEnter")
  hook.land("onCommodity", "commodity")
  hook.land("onLand")
  --hook.timer(5000, "onTime")
  -- evt.finish( true ) -- we never finish.
end

function onEnter()
  checkVars()
  calcSystemData(system.cur():name())
  if debugging then
    announcePrices()
    -- printDisruptions() -- too long to print
  end
end

function onLand()
  -- supercede the internal NAEV economy
  -- NEEDS setPriceAt() C hack to work.
  purgeElapsedDisruptions()
  checkVars()
  setCurrentSystemPrices()
end

function onCommodity()
  -- bash it again to override the internal economy
  checkVars()
  setCurrentSystemPrices()
end

function discoverSystems()  
  -- crawl the systems from Gamma Polaris outward
  -- to discover and record their commodities.
  traverseSystems(startSystem)

  if randomInversion then
    -- the inversion table should be deterministic
    -- so let's sort by name and then randomize
    local nameTable = {}
    for commName, value in pairs(allCommodities) do
      debugMsg ("    " .. tostring(commName))
      table.insert(nameTable, commName)
    end
    table.sort(nameTable)
  
    -- invert the market forces for some commodities
    -- so that interesting back & forth routes
    -- show up.
    math.randomseed(42)
    for i, commName in ipairs(nameTable) do
      if math.random() > .5 then
        allCommodities[commName]["inverted"] = true
      end
    end
  end  

end

function traverseSystems(theSystemName)
  -- needed because NAEV doesn't provide a getAllSystems() function
  local sys = system.get(theSystemName)
  local commod = {}

  allSystems[theSystemName] = true

  local count = 0
  for key, plannet in ipairs(sys:planets()) do
    if #plannet:commoditiesSold() > 0 then
      count = count + 1
      commoditySystems[theSystemName] = commoditySystems[theSystemName] or {}
      for _, commodity in ipairs(plannet:commoditiesSold()) do
          if commoditySystems[theSystemName][commodity:name()] == nil then
            commoditySystems[theSystemName][commodity:name()] = {
              ugen=ugenCommoditySwing(0.02, 1, math.random()) -- for more fun
            }
          end
      end
    end
  end

  if count == 0 then -- no commodities
    calculatedSystems[theSystemName] = true
  end

  -- just populate inversions for the moment
  if commoditySystems[theSystemName] then
    for commodityName, commodityTab in pairs(commoditySystems[theSystemName]) do
      allCommodities[commodityName] = {inverted=false}
    end
  end
  
  for _, s in ipairs(sys:adjacentSystems()) do
    if allSystems[s:name()] == nil then
      traverseSystems(s:name())
    end
  end
  
end

function calcSystemData(systemName)
  -- gather statistics about a system, and store them
  -- for later use
    local sysData = commoditySystems[systemName]
    local median
    local average
    
    if sysData == nil then return end
    
    if calculatedSystems[systemName] ~= nil and calculatedSystems[systemName] then
      return
    end
    
    local numConnections = #system.get(systemName):jumps()
    
    for commKey, _ in pairs(commoditySystems[systemName]) do
      histogram = calculateHistogram(systemName, commKey)
      median = calculateMedian(histogram)
      average = calculateAverage(histogram)
      commoditySystems[systemName][commKey]["jumpHistogram"] = histogram
      commoditySystems[systemName][commKey]["jumpMedian"] = median
      commoditySystems[systemName][commKey]["jumpAverage"] = average
    end
    
    calculatedSystems[systemName] = true
      
end

function calculateHistogram(systemName, commodityName)
  -- calculate jumps from every other system to here
  -- and return a histogram of jump distances
  -- takes much time, and ideally should be pre-rendered or cached
  local jumps = {}
  local thisSystem = system.get(systemName)
  local j
  -- for every other system
  j = 0
  for s, _ in pairs(commoditySystems) do
    if s ~= systemName then
      if commoditySystems[s][commodityName] ~= nil then
        -- it's actually different depending on the direction!
        -- not sure why this is, but important bit is how many jumps from
        -- other places to here. EXCEPT IT DOESN"T WORK for some reason(delivers nil?)
        --j = system.get(s):jumpDist(thisSystem:name()) -- thisSystem:jumpDist(s)
        j = thisSystem:jumpDist(s) -- thisSystem:jumpDist(s)

        if j == 0 then
          -- don't know why a zero shows up sometimes
        elseif j > deepestJump then
          -- skip it
        else
          if jumps[j] == nil then
            jumps[j] = 1
          else
            jumps[j] = jumps[j] + 1
          end
        end
      end
    end
  end
  return jumps
end

function calculateMedian(histo)
  local numbertable = {}

  -- sort the histogram and find the middle number
  for theNum, theCount in pairs(histo) do
    for i=1,theCount do
      table.insert(numbertable, theNum)
    end
  end
  table.sort(numbertable)
  local mid = #numbertable/2
  
  if #numbertable == 1 then return numbertable[1] end
  
  if math.floor(mid) ~= mid then
    return (numbertable[math.floor(mid)] + numbertable[math.floor(mid)+1]) /2
  else
    return numbertable[#numbertable/2]
  end
end

function calculateAverage(histo)
  local count = 0
  local total = 0
  for theNum, theCount in pairs(histo) do
    count = count + theCount
    total = total + theNum*theCount
  end
  return total/count
end

function calcSystemPrice(systemName, commodityName)
  local basePrice = commodity.get(commodityName):price()

  -- failsafe, but may cause crazy prices!
  --if commoditySystems[systemName] == nil then return basePrice end
  --if commoditySystems[systemName][commodityName] == nil then return basePrice end
  
  local median = commoditySystems[systemName][commodityName]["jumpMedian"]
  -- adjust by power
  median = math.pow(median, distancePower)

  local unitOverhead =  median * revenuePerJump / typicalTonnage
  local myInvert = allCommodities[commodityName]["inverted"]
  
  if invertEverything then
    myInvert = not myInvert
  end
    

  if myInvert then
    -- should be based on deepest median jump for the commodity but
    -- we don't know that without a pre-bake of universe
    local biggestPrice = basePrice+(revenuePerJump*math.pow(deepestJump, distancePower)/typicalTonnage)
    biggestPrice = biggestPrice + biggestPrice * percentageRevenue*math.pow(deepestJump, distancePower)/100.
    
    price = biggestPrice
    
    price = price * 1.0/((100 +percentageRevenue*median/100)/100.)
    price = price - unitOverhead
  else
    price = basePrice  + unitOverhead    
    price = price  + price * percentageRevenue*median/100.
  end
  
  if useFluctuation then
    f = getFluctuation(systemName, commodityName)
    debugMsg("FLUCTUATION FOR " .. systemName .. " " .. commodityName .. ": " .. tostring(f))
    price = price + price*getFluctuation(systemName, commodityName)
    if randomPepper then
      local t = time.get():tonumber()/stpConversion
      price = price + price * commoditySystems[systemName][commodityName]["ugen"]:get(t)
    end
  end
  
  price = price + priceOffset
  
  price = round(price/roundTo) * roundTo
  
  if price < roundTo then price = roundTo end

  return price
end

-- SETTING PRICES.
-- This sets the price seen in the commodity window.
function setCurrentSystemPrices()
  local planett = planet.cur()
  local s = planett:system()
  local basePrice
  local newPrice
  local sameSys = false

  if lastSystem == s:name() then
    sameSys = true
  end
  
  lastSystem = s:name()

  -- needed because player might start out landed
  calcSystemData(system.cur():name())
  for i, commod in ipairs(planett:commoditiesSold()) do
    if lockPrices and sameSys and lastSystemPrices[commod:name()] then
      newPrice = lastSystemPrices[commod:name()]
    else
      newPrice = calcSystemPrice(s:name(), commod:name())
      debugMsg ("setting price of " .. commod:name() .. " in " .. s:name() .. " to " .. newPrice)
      lastSystemPrices[commod:name()] = newPrice
    end
    -- this is a C code mod:
    commod:setPriceAt(planett, newPrice)
  end
end


--- DISRUPTIONS
function addDisruption(systemName, commodityName, inUgen, inDuration, inDistance)
  -- (check for sanity here)
  assert(systemName)
  assert(commodityName)
  local ug
  if type(inUgen) == "table" then
    ug = ugen2table(inUgen)
  else
    ug  = inUgen
  end
  
  local startTime = time.get():tonumber() / stpConversion
  local disrupt = {system=systemName, start=startTime, duration=inDuration, 
    ugen=inUgen, ugenTable=ug, distance=inDistance}
  disruptions[commodityName] = disruptions[commodityName] or {}
  table.insert(disruptions[commodityName], disrupt)
  saveDisruptions()
end

function saveDisruptions()
  local allDisruptions = {}
  -- the following junk basically amounts to
  -- removing the ugen function from the table before save

  for commodityName, array in pairs(disruptions) do
      local commArray = {}
      for i, disrupt in ipairs(array) do
        local purged = {}
        for k,v in pairs(disrupt) do
          if k ~= "ugen" then 
            purged[k] = v
          end
        end
        table.insert(commArray, purged)
      end
      allDisruptions[commodityName] = commArray
  end
  var.push("WayfarerDisruptions", table2string(allDisruptions))

end

function loadDisruptions()
  local loaded = var.peek("WayfarerDisruptions")
  if loaded == nil then return end
  
  loaded = string2table(loaded)
  for commodityName, array in pairs(loaded) do
    for i, disrupt in ipairs(array) do
      -- restore the ugen
      if type(disrupt.ugenTable) == "number" then
        loaded[commodityName][i]["ugen"] = disrupt.ugenTable
      else
        loaded[commodityName][i]["ugen"] = table2ugen(disrupt["ugenTable"])
      end
    end
  end
  disruptions = loaded
end

function printDisruptions()
  print ("DISRUPTION TABLE")
  print(table2string(disruptions))
  print ("-------------------")
end

function destroyDisruptions()
  var.pop("WayfarerDisruptions")
  local i = 1
  while true do
    if var.peek("WF-disruptions" .. tostring(i)) == nil then
      break
    end
    var.pop("WF-disruptions" .. tostring(i))
    i = i +1
  end
  var.pop("WF-disruptions_length")
end

function checkVars()
  local disruptionData = hackadoodleIn("WF-disruptions")
  if disruptionData == nil then 
    return 
  end

  for i,v in ipairs(disruptionData) do
    if v.system ~= nil and v.commodity ~= nil then
      local ugen  = v.ugenTable
      
      if type(v.ugenTable) ~= "number" then
        ugen = table2ugen(v.ugenTable)
      end
      
      addDisruption(v.system, v.commodity, ugen, v.duration, v.distance)
      debugMsg("Wayfarer disruption created in system " .. v.system .. " with commodity " .. v.commodity)
    else
      debugMsg("Found a disruption but it was just bad.")
      debugMsg("BEGIN JUNK-------------------------------")
      debugMsg(table2string(v))
      debugMsg("END JUNK-------------------------------")
    end
  end
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
  
  for k, v in pairs(aTable) do
    debugMsg (k .. " " .. tostring(v))
  end
  
  local str = table2string(aTable)
  local chunks = math.ceil(string.len(str)/(chunkSize))

  var.push(varName .. "_length", chunks+1)
  for i=1, chunks do
    var.push(varName .. tostring(i), string.sub(str, 1, chunkSize-1))
    str = string.sub(str, chunkSize)
  end
end


function addGradient(systemName, commodityName, inUgen, inDistance, inStartTime)
  -- (check for sanity here)
  local gradient = {}
  inUgen = inUgen or 0
  inDistance = inDistance or 4
  inStart = inStart or time.get():tonumber()/stpConversion
  
  if type(inUgen) == "number" then
    inUgen = inUgen
  end
  
  gradient = {system=systemName, start=startTime, ugen=inUgen, distance=inDistance}

  gradients[commodityName] = gradients[commodityName] or {}
  table.insert(gradients[commodityName], gradient)
end

function getFluctuation(systemName, commodityName)
  -- fluctuation is the combined gradient & disruptions for a commodity
  -- in a system. It's to be plugged into the formula
  -- price = price + price*fluctuation
  local disruptFluctuation = 0
  local count = 0
  local total = 0.0
  local t = time.get():tonumber() /stpConversion
  if disruptions[commodityName] then
    for i,v in ipairs(disruptions[commodityName]) do
      if v["system"] == systemName then
        if type(v.ugen) == "number" then
          total = total + v.ugen
        else
          total = total + v["ugen"]:get(t)
        end
        count = count + 1
      else
        local maxDist = v["distance"]
        local dist = system.get(systemName):jumpDist(v["system"])
        --debugMsg("Checking distance from " .. systemName .. " to " .. v["system"])
        --debugMsg("disruption dist, maxDist: " .. tostring(dist) .. " " .. maxDist)
        if dist <= maxDist then 
          if type(v.ugen) == "number" then
            total = total + v["ugen"] * (1+maxDist-dist)/(maxDist+1)
          else
            total = total + v["ugen"]:get(t) * (1+maxDist-dist)/(maxDist+1)
          end
          count = count + 1
        end
      end
    end
  end
  -- two ways of doing it. Either a simple add, or
  -- a "mixer" (averaged). Simple add makes more sense
  -- for a mission designer (she gets what she expects)
  -- but can result in extreme results when combined
  if count >0  then disruptFluctuation = total end
  --if count >0  then disruptFluctuation = total/count end
  
  count = 0; total = 0
  if gradients[commodityName] then   
    for i,v in ipairs(gradients[commodityName]) do
      if v.system == systemName then
        if type(v.ugen) == "number" then
          total = total + v.ugen
        else
          total = total + v["ugen"]:get(t)
        end
        count = count + 1
      else
        local maxDist = v["distance"] + 1
        local dist = system.get(systemName):jumpDist(v.system)
        if dist <= maxDist then 
          if type(v.ugen) == "number" then
            total = total + v["ugen"] * (1+maxDist-dist)/(maxDist+1)
          else
            total = total + v["ugen"]:get(t) * (1+maxDist-dist)/(maxDist+1)
          end
          count = count + 1
        end
      end
    end
  end
  local gradientFlux = 0
  if count > 0 then gradientFlux = total end
  --if count > 0 then gradientFlux = total/count end
  
  local totalFlux = gradientFlux + disruptFluctuation
  -- local totalFlux = (gradientFlux + disruptFluctuation)/2
  
  if totalFlux <= -1 then -- BAD
      return -.99
  else
    return totalFlux
  end
end

function purgeElapsedDisruptions()
  -- should be called only occasionally
  local t = time:get():tonumber()/stpConversion
  local changed = false
  for commodityname,disrupts in pairs(disruptions) do
    --print(table2string(disrupts))
    for i=#disrupts, 1, -1 do
      if disrupts[i].start + disrupts[i].duration < t then
        debugMsg("Current time is " .. tostring(t))
        debugMsg("Purging a " .. commodityname .. " disruption in " .. disrupts[i].system .. " from the list...")
        table.remove(disruptions[commodityname],i)
        changed = true
      end
    end
  end
  if changed then
    saveDisruptions()
  end
end

---- extra stuff
function round(number)
  -- lua provides math.ldexp() but not math.round(). Who woulda guessed?
  if math.ceil(number) - math.floor(number) >=.5  then
    return math.ceil(number)
  else
    return math.floor(number)
  end
  
end


---- debugging
function announcePrices()
  local s = system.cur()
  local string = ""
  local basePrice
  if commoditySystems[s:name()] == nil then return end
  
  for commodityName, _ in pairs(commoditySystems[s:name()]) do
    basePrice = commodity.get(commodityName):price()
    string = string .. "\n" .. commodityName .. ": " .. calcSystemPrice(s:name(), commodityName)
  end
  
  if string ~= "" then
    local name, but = tk.choice("Prices", string, "OK", "Dump System", "Dump Universe (takes time!)", "Dump CSV")
    if but == "Dump Universe (takes time!)" then
      dumpUniverse()
    end
    if but == "Dump System" then
      dumpSystem(system.cur():name())
    end
    if but == "Dump CSV" then
      dumpCSV()
    end
  end
end

function dumpSystem(systemName)
  if commoditySystems[systemName] == nil then
    debugMsg("No commodities sold in " .. systemName)
    return
  end
  debugMsg ("Commodities for system " .. systemName)
  debugMsg (table2string(commoditySystems[systemName]))
end

function dumpUniverse()
  -- calculate entire universe
  -- and print out some statistics
  local lowHighTable = {}
  local price
  local basePrice
  local medianJumps = {}
  for systemName, _ in pairs(commoditySystems) do
    systemData = commoditySystems[systemName]
    debugMsg("Calculating prices for " .. systemName)
    calcSystemData(systemName)
    for commName, comData in pairs(commoditySystems[systemName]) do
      
      if medianJumps[commName] == nil then
        medianJumps[commName] = {}
      end
      
      table.insert(medianJumps[commName], comData["jumpMedian"])
      
      basePrice = commodity.get(commName):price()
      price = calcSystemPrice(systemName, commName)
      if lowHighTable[commName] == nil then
        lowHighTable[commName]={low= price, high=price, count=1, total=price, lowSystem=systemName, highSystem=systemName}
      else
        lowHighTable[commName]["count"] = lowHighTable[commName]["count"] + 1
        lowHighTable[commName]["total"] = lowHighTable[commName]["total"] + price
        
        if price < lowHighTable[commName]["low"] then
          lowHighTable[commName]["low"] = price
          lowHighTable[commName]["lowSystem"] = systemName
        end
        if price > lowHighTable[commName]["high"] then
          lowHighTable[commName]["high"] = price
          lowHighTable[commName]["highSystem"] = systemName
        end
      end
    end
  end
  for commName, commodityTab in pairs(lowHighTable) do
    local jumpTotal = 0
    for i,v in ipairs(medianJumps[commName]) do
      jumpTotal = jumpTotal + v
    end
    debugMsg ("Highest " .. commName .. ": " .. commodityTab["high"] .. "(" .. commodityTab["highSystem"] .. ")")
    debugMsg ("Lowest " .. commName .. ": " .. commodityTab["low"] .. "(" .. commodityTab["lowSystem"] .. ")")
    debugMsg ("Jumpdist from low to high: " .. system.get(commodityTab["highSystem"]):jumpDist(commodityTab["lowSystem"]) )
    debugMsg (commName .. " inverted? " .. tostring(allCommodities[commName]["inverted"]))
    debugMsg ("Average " .. commName .. ": " .. commodityTab["total"]/commodityTab["count"])
    debugMsg ("Average jumps: " .. jumpTotal/#medianJumps[commName])    
    debugMsg ("Total Serving " .. commName .. ": " .. commodityTab["count"])
    debugMsg ("----------------------")
    
  end
end

function dumpCSV(filename)
  -- take a snapshot of prices right now and dump
  -- them to a CSV file for perusal.

  local commods = {}
  local theCSV = {}
  
  
  filename = filename or "wayfarer_prices.csv"
  
  for name, _ in pairs(allCommodities) do
    table.insert(commods, name)
  end
  table.sort(commods)
  
  table.insert(theCSV, "System Name," .. table.concat(commods,","))
  
  for sysName, _ in pairs(commoditySystems) do
    calcSystemData(sysName)
    local oneline = {sysName}
    for i, commod in ipairs(commods) do
      if commoditySystems[sysName][commod] == nil then
        table.insert(oneline, "")
      else
        --print(commod .. " " .. sysName)
        table.insert(oneline, tostring(calcSystemPrice(sysName, commod)))
      end
    end
    oneline = table.concat(oneline, ",")
    table.insert(theCSV, oneline)
  end

  theCSV=table.concat(theCSV, "\n")
  f = io.open(filename, "wb")
  f:write(theCSV)
  f:close()
end
