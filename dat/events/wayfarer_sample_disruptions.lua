include "wayfarer_disruptions.lua"
include "ugens.lua"
include "table2string.lua"

commodityGuess = {"Luxury Goods", "Food", "Industrial Goods", "Ore", "Medicine"}
disruptionFuncs = {"doPriceHike", "doOverProduction", "doSpaceWeather"}

function create()
  -- should check here to see if wayfarer is active,
  -- but that's remarkably complicated ATM
  
  local current = system.cur()
  for i=1, 5 do
    local sysies = current:adjacentSystems(current)
    current = sysies[rnd.rnd(1, #sysies)]
  end
  
  local commodSold = {}
  
  for i, plannet in ipairs(current:planets()) do
    for i,c in ipairs(plannet:commoditiesSold()) do
     table.insert(commodSold, c)
    end
  end
  
  if #commodSold > 0 then
    local commodity = commodSold[rnd.rnd(1, #commodSold)]:name()
    local func = disruptionFuncs[rnd.rnd(1, #disruptionFuncs)]

    _G[func](current:name(), commodity)
    --doOverProduction(current:name(), commodity)
    evt.finish(true)
  end
end

function doPriceHike(sysName, commodityName)
  local priceHike = ugenAdd(ugenCommoditySwing(0.05, 1, math.random()), .1)
  wayfarerCreateDisruption(sysName, commodityName, priceHike, 5)

  local title =  sysName .. " sees " .. commodityName .. " Prices Boom"
  local content = "Corruption behind the scenes has made " .. commodityName .. " prices inflate surrounding " .. sysName .. ". Take advantage quickly!"
  news.add("Generic", title,content, time.get() + time.create(0,2,0), time.get())
end

function doOverProduction(sysName, commodityName)
  wayfarerCreateDisruption(sysName, commodityName, -0.1, 2)
  local title = "Over production of " .. commodityName .. " in " .. sysName .. " Leads to Price Drop"
  local content = sysName .. " has seen some rough times lately getting over production under control for " .. commodityName .. ". \"Price drops are killing us!\" says one multibillionaire interviewed on condition of anonymity."
  news.add("Generic", title,content, time.get() + time.create(0,2,0), time.get())
end

function doSpaceWeather(sysName, commodityName)
  local volitility =  ugenSin(ugenSin(.5, 2, 4.1), .05)
  wayfarerCreateDisruption(sysName, commodityName, volitility, 3)
  local title = "Space Weather Causes High Volitility in " .. sysName
  local content = "Prices for " .. commodityName .. " in " .. sysName .. " are going wild following some unfortunate \"space weather\" as the locals call it. It should only last for 3 STPs, according to computer models."
  news.add("Generic", title,content, time.get() + time.create(0,2,0), time.get())
end
