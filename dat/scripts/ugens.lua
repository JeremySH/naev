-- ugens

--[[ 
A little library for generating signals that vary over infinite time.
You can combine ugens together to model any crazy signal you like.
You can also save a ugen in a pure lua table (without functions) to easily
serialize without loadstring().

This was made for modelling a game economy. But it could be useful
for modelling anything that changes cyclically over time, like fashion trends (not really).

You can either pass a constant number or another ugen in any parameter.

how to use (frequency mod example):
modSignal = ugenSin(10, 0.5, 0.5) -- oscillate from 0 to 1 every 10t
mySignal = ugenSin(modSignal, 1, 0) -- modify the period by modSignal

mySignal:get(t) -- get value of mySignal at time t

myTable = ugen2table(mySignal) -- get mySignal as a pure lua table

mySignal = table2ugen(myTable) -- load it.

---
Constructing a signal generator chain is a one-way operation. That is,
you really shouldn't change the ugens after you've chained them. If you did,
you would introduce non-determinism into the system, in which case saving wouldn't
work as expected.


a list of ugens. this should be enough for simple needs.
ugenSin(period, amplitude, offset, phase)
ugenSaw(period, amplitude, offset, phase)
ugenTri(period, amplitude, offset, phase)
ugenSquare(period, amplitude, offset, phase)
ugenPulse(period, amplitude, offset, phase, size)
ugenRand(period, amplitude, offset)
ugenRandLerp(period, amplitude, offset)
ugenRandStep(period, amplitude, offset)

ugenClamp(sig1, min, max)
ugenTimeClamp(sig1, t1, t2, default) -- only perform calc between times t1 and t2
ugenSqueeze(inSig1, inMin, inMax, inDistortion)
ugenDelay(sig1, dt)
ugenTimeScale(sig1, factor) -- multiply t by factor before sig1 uses t.
ugenHold(sig1, period) -- "sample and hold" for period

ugenMult(sig1, sig2)
ugenAdd(sig1, sig2)
ugenMix(sig1, sig2, amount) -- fade from full sig1 (amount=0) to full sig2 (amount=1.0)

-- pre-fab signal chains.
-- this will emulate a stock price poorly (heh). oneDay is how many t's in a day.
ugenCommoditySwing(priceSwing, oneDay, salt) -- factor intended for price = price + price*stock:get(t)

examples
sig = ugenSin(period, amplitude, offset, phase)

sig:get(t) -- get the value at time t.

any parameter can be substituted with another ugen:
sig1 = ugenSin(3, .5, .5) -- oscilate from 0 to 1 over 3t
sig2 = ugenSin(sig1, .2) -- use sig1 to modulate frequency

-- math operations are possible:
sig3 = ugenMult(3, 3) -- produce the number 9 forever.
sig3 = ugenMult(sig5, 3) -- amplify sig5 3 times
sig4 = ugenMult(sig2, sig1) -- do a ring modulation-style signal

]]--



-- this is so I don't corrupt math.randomseed() and
-- so that seeds based on t can be used deterministically
theRNG = {
  seed = 12387,
  get = function(tempSeed) 
    if tempSeed then
      local seed = math.fmod(tempSeed * 16807, 2147483647)
      return math.fmod(seed * 16807, 2147483647) /2147483647
    else
      theRNG.seed = math.fmod(theRNG.seed * 16807, 2147483647) 
      return theRNG.seed/2147483647
    end
  end
}

function valorfunc(thing)
  if type(thing) == 'table' then
    local f = function (self, t) return thing:get(t) end
    return f
  else
    local f = function(self, t) return thing end
    return f
  end
end

function table2ugen(aTable)
  local name = aTable["ugen"]
  local createFunction=ugenMap[name]
  local params = {}
  local ug = {}  

  for i, v in ipairs(aTable["params"]) do
    if type(aTable[v]) == "table" then
      ug[v] = table2ugen(aTable[v])
    else
        ug[v] = aTable[v]
    end
    table.insert(params, ug[v])
  end
  
  ug = createFunction(params)

  return ug
end

function ugen2table(uGen)
  return deconstruct(uGen)
end

function construct(uGen)
  if uGen["params"] == nil then return uGen end
  
  -- construct a function for each param
  for i, v in ipairs(uGen["params"]) do
    local functionName = v .. "F"
    uGen[functionName] = valorfunc(uGen[v])
  end
  return uGen
end

function deconstruct(uGen)
  local copy = {ugen=uGen["ugen"], params=uGen["params"]}
  if uGen["params"] == nil then return copy end

  -- need only params
  for i,v in ipairs(uGen["params"]) do
    if type(uGen[v]) == 'table' then
      copy[v] = deconstruct(uGen[v])
    else
      copy[v] = uGen[v]
    end
  end
  return copy
end

function safe_period(num)
  -- ensure that num is >0
  local n = math.abs(num)
  if n == 0 then n = 0.0000001 end
  return n
end

function printValues(uGen, startT, endT, resolution)
  startT= startT or 0.0
  endT= endT or 5.0
  resolution = resolution or 50.0
  for t=startT, endT, (endT-startT)/resolution do
    print (tostring(t) .. "," .. uGen:get(t))
  end
end

function saveCSV(uGen, filename, startT, endT, resolution)
  -- convenience for debugging
  startT= startT or 0.0
  endT= endT or 5.0
  resolution = resolution or 50.0
  f = io.open(filename, "w")
  if f == nil then return end
  
  for t=startT, endT, (endT-startT)/resolution do
    f:write (tostring(t) .. "," .. uGen:get(t) .."\n")
  end
  f:close()
end

function asWaveform(uGen, startT, endT, stepT)
  -- give an array of values from startT to endT, stepping by stepT
  -- yes, it is a simple loop.
  local tab = {}
  for t=startT, endT, stepT do
    table.insert(tab, uGen:get(t))
  end
  return tab
end

function ugenSinP(p) return ugenSin(p[1], p[2], p[3],p[4]) end
function ugenSin(inPeriod, inAmplitude, inOffset, inPhase)
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0
  inPeriod = inPeriod or 1
  if inPeriod == 0 then inPeriod = .0000001 end
    
  local tab = {
  ugen = "sin",
  params = {"period", "amplitude", "offset", "phase"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  get = function(self, t) 
    local p = safe_period(self:periodF(t))
    return self:amplitudeF(t) * math.sin((t/p)*math.pi*2 + self:phaseF(t)*2*math.pi) + self:offsetF(t) end
  }
  return construct(tab)
end

function ugenConstantP(p) return ugenConstant(p[1]) end
function ugenConstant(inNum)
  -- more useful than you think at first.
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0
  inPeriod = inPeriod or 1
  if inPeriod == 0 then inPeriod = .0000001 end
    
  local tab = {
  ugen = "constant",
  params = {"num"},
  num = inNum,
  get = function(self, t)
    return self:numF(t)
    end
  }
  return construct(tab)
end

function ugenTriP(p) return ugenTri(p[1], p[2], p[3],p[4]) end
function ugenTri(inPeriod, inAmplitude, inOffset, inPhase)
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0
  inPeriod = inPeriod or 1
  if inPeriod == 0 then inPeriod = .0000001 end
    
  local tab = {
  ugen = "tri",
  params = {"period", "amplitude", "offset", "phase"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  get = function(self, t)
    -- note: this shape mimics the start & direction of a sine wave (although triangular)
    local p = safe_period(self:periodF(t))
    return self:amplitudeF(t)*2*(
      (
        math.abs(math.fmod( t+(0.75+self:phaseF(t))*p, p) - p/2)
        /(p/2)
      )
      -0.5)  + self:offsetF(t)
    end
  }
  return construct(tab)
end

function ugenSquareP(p) return ugenSquare(p[1], p[2], p[3],p[4]) end
function ugenSquare(inPeriod, inAmplitude, inOffset, inPhase)
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0
  if inPeriod == 0 then inPeriod = .0000001 end
    
  local tab = {
  ugen = "square",
  params = {"period", "amplitude", "offset", "phase"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  get = function(self, t)
    -- note: this shape mimics the start & direction of a sine wave
    local p = safe_period(self:periodF(t))
    if math.mod(t + self:phaseF(t)*p, p) > p/2. then
      return -self:amplitudeF(t) + self:offsetF(t)
    else
      return self:amplitudeF(t) + self:offsetF(t)
    end
  end
  }
  return construct(tab)
end

function ugenSawP(p) return ugenSaw(p[1], p[2], p[3],p[4]) end
function ugenSaw(inPeriod, inAmplitude, inOffset, inPhase)
  -- inverted saw (from max to min) is acheived by setting
  -- a negative amplitude. 
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0
  inPeriod = inPeriod or 1
  if inPeriod == 0 then inPeriod = .0000001 end
    
  local tab = {
  ugen = "saw",
  params = {"period", "amplitude", "offset", "phase"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  get = function(self, t)
    local p = safe_period(self:periodF(t))

    return -1 + self:amplitudeF(t)*2*math.fmod(t+self:phaseF(t), p)/p + self:offsetF(t)
    end
  }
  return construct(tab)
end

function ugenRandP(p) return ugenRand(p[1], p[2], p[3],p[4],p[5]) end
function ugenRand(inPeriod, inAmplitude, inOffset, inPhase, inSalt)
  -- random number based on t
  -- thus, it's not really random (default ugenRands() will get the same number at same time t)
  -- to introduce different random numbers, add salt. Same salts will generate same
  -- numbers for a given t.
  -- phase will shift the noise in time by phase*period, thus you can scale the noise
  -- with period, and offset in time by phase
  -- normally, you'll just do
  -- myRandom = ugenRand(nil, nil, nil, nil, math.random())
  -- and get on with it.
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0.0
  inSalt = inSalt or 0.0
  inPeriod = inPeriod or 1.0
  if inPeriod == 0 then inPeriod = .0000001 end

  local tab = {
  ugen = "random",
  params = {"period", "amplitude", "offset", "phase", "salt"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  salt = inSalt,
  get = function(self, t)
      return (1.0 - 2.0*tRand(t, self:saltF(t))) * self:amplitudeF(t) + self:offsetF(t)
    end
  }
  return construct(tab)
end

function tRand(num, salt)
  -- a way to get a deterministic random number for t
  salt = salt or 0
  local mantissa, exp = math.frexp(num)
  --math.randomseed(mantissa*2000 + exp + salt)
  --math.random()
  return theRNG.get(mantissa*2000 + exp + salt)
end

function ugenRandLerpP(p) return ugenRandLerp(p[1], p[2], p[3],p[4],p[5]) end
function ugenRandLerp(inPeriod, inAmplitude, inOffset, inPhase, inSalt)
  -- like ugenRand, but ejects a number only at start, middle, and end of a period,
  -- then interpolates. It looks like ugenTri with faulty peaks.
  -- this is useful for smooth but random changes
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0.0
  inSalt = inSalt or 0.0
  inPeriod = inPeriod or 1.0
  if inPeriod == 0 then inPeriod = .0000001 end

  local tab = {
  ugen = "randomlerp",
  params = {"period", "amplitude", "offset", "phase", "salt"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  salt = inSalt,
  get = function(self, t)
      local p = safe_period(self:periodF(t))
      local salt = self:saltF(t)
      local t1
      local t2
      local phased = t+self:phaseF(t)*p
      local mod = math.mod(phased,p)

      -- snap to nearest time
      if mod >= p/2 then
        t1 = (phased-mod) + p/2
        t2 = (phased-mod) + p
      else
        t1 = (phased-mod) + 0
        t2 = (phased-mod) + p/2
      end
      local first = tRand(t1, salt)
      local second = tRand(t2, salt)
      local lerp = first + (second - first) * math.mod(phased, p/2)/(p/2)
      return (1.0 - 2*lerp) * self:amplitudeF(t) + self:offsetF(t)
    end
  }
  return construct(tab)
end

function ugenRandStepP(p) return ugenRandStep(p[1], p[2], p[3],p[4],p[5]) end
function ugenRandStep(inPeriod, inAmplitude, inOffset, inPhase, inSalt)
  -- random but stepping instead of interpolating
  -- this keeps same number over the entire length of the period (thus it's a little
  -- different from tri() or sin() or even randLerp()
  
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0.0
  inSalt = inSalt or 0.0
  inPeriod = inPeriod or 1.0
  if inPeriod == 0 then inPeriod = .0000001 end

  local tab = {
  ugen = "randomstep",
  params = {"period", "amplitude", "offset", "phase", "salt"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  salt = inSalt,
  get = function(self, t)
      local p = safe_period(self:periodF(t))
      local salt = self:saltF(t)
      local phased = t+self:phaseF(t)*p
      local mod = math.mod(phased,p)
      
      -- snap to previous time
      return self:amplitudeF(t) * (.5 - tRand(phased-mod, salt)) + self:offsetF(t)
  end
  }
  return construct(tab)

end

function ugenPulseP(p) return ugenPulse(p[1], p[2], p[3],p[4],p[5]) end
function ugenPulse(inPeriod, inAmplitude, inOffset, inPhase, inWidth)
  -- generalized square wave with a width. Width is specified
  -- as a factor of the period, e.g. 0.25, and occurs
  -- at the start of the period
  inAmplitude = inAmplitude or 1
  inOffset = inOffset or 0.0
  inPhase = inPhase or 0
  inPeriod = inPeriod or 1
  
  if inPeriod == 0 then inPeriod = 0.0000001 end
  inWidth = inWidth or 0.5
  
  if inWidth <=0. then inWidth = 0.000001 end
  if inWidth > inPeriod  then inWidth = inPeriod end
  
  local tab = {
  ugen = "pulse",
  params = {"period", "amplitude", "offset", "phase", "width"},
  period = inPeriod,
  amplitude = inAmplitude,
  offset = inOffset,
  phase = inPhase,
  width = inWidth,
  get = function(self, t)
    local p = safe_period(self:periodF(t))
      
    if math.mod(t + self:phaseF(t)*p, p) > self:widthF(t) then
      return -self:amplitudeF(t) + self:offsetF(t)
    else
      return self:amplitudeF(t) + self:offsetF(t)
    end
  end
  }
  return construct(tab)
end

function ugenClampP(p) return ugenClamp(p[1], p[2], p[3]) end
function ugenClamp(inSig1, inMin, inMax)
  inSig1 = inSig1 or 1
  inMin = inMin or -1
  inMax = inMax or 1
  
  dT = dT or 0
  local tab = {
    ugen = "clamp",
    params = {"sig1", "min", "max"},
    sig1 = inSig1,
    max = inMax,
    min = inMin,    
    get = function(self, t)
      local sig = self:sig1F(t)
      local min = self:minF(t)

      if sig < min then return min end

      local max = self:maxF(t)
      if sig > max then return max end

      return sig
    end
    }
  return construct(tab)
end

function ugenTimeClampP(p) return ugenTimeClamp(p[1], p[2], p[3],p[4]) end
function ugenTimeClamp(inSig1, inStart, inEnd, inDefault)
  inSig1 = inSig1 or 1
  inStart = inStart or 0
  inEnd = inEnd or 1
  inDefault = inDefault or 0
  
  dT = dT or 0
  local tab = {
    ugen = "timeclamp",
    params = {"sig1", "start", "end", "default"},
    sig1 = inSig1,
    max = inStart,
    min = inEnd,    
    default = inDefault,
    get = function(self, t)
      if t < self:startF(t) then return self:defaultF(t) end
      if t > self:endF(t) then return self:defaultF(t) end

      return self:sigF(t)

    end
    }
  return construct(tab)
end

function ugenDelayP(p) return ugenDelay(p[1], p[2]) end
function ugenDelay(inSig1, dT)
  -- delay by dt. This works with negative dt's too.
  inSig1 = inSig1 or 1
  dT = dT or 0
  local tab = {
    ugen = "delay",
    params = {"sig1", "dt"},
    sig1 = inSig1,
    dt = dT,
    
    get = function(self, t) return self:sig1F(t+self:dtF(t)) end
    }
  return construct(tab)
  
end

function ugenTimeScaleP(p) return ugenTimeScale(p[1], p[2], p[3]) end
function ugenTimeScale(inSig1, inSig2)
  -- scale t for sig1 by sig2
  -- note that the greater inSig2, the faster inSig1 will change
  -- (this might be counter-intuitive)
  inSig1 = inSig1 or 1
  inSig2 = inSig2 or 1
  
  local tab = {
    ugen = "timescale",
    params = {"sig1", "sig2"},
    sig1 = inSig1,
    sig2 = inSig2,
    get = function(self, t) 
      local thescale = self:sig2F(t)
      if thescale == 0.0 then thescale = .00001 end
      return self:sig1F(t*thescale) end
    }
  return construct(tab)
end

function ugenHoldP(p) return ugenHold(p[1], p[2], p[3]) end
function ugenHold(inSig1, inPeriod, inPhase) -- "sample and hold" for period
  inSig1 = inSig1 or 1
  inPeriod = inPeriod or 1 
  inPhase = inPhase or 0
  
  local tab = {
    ugen = "hold",
    params = {"sig1", "period", "phase"},
    sig1 = inSig1,
    period = inPeriod,
    phase = inPhase,
    get = function(self, t)
      local p = safe_period(self:periodF(t))
      local phased = t+self:phaseF(t)*p
      local mod = math.mod(phased,p)      

      -- snap to previous time
      return self:sig1F(phased-mod)
      end
    }
  return construct(tab)
end

function ugenMultP(p) return ugenMult(p[1], p[2]) end
function ugenMult(inSig1, inSig2)
  inSig1 = inSig1 or 1
  inSig2 = inSig2 or 1
  
  local tab = {
    ugen = "mult",
    params = {"sig1", "sig2"},
    sig1 = inSig1,
    sig2 = inSig2,
    get = function(self, t) return self:sig1F(t) * self:sig2F(t) end
    }
  return construct(tab)
end

function ugenAddP(t) return ugenAdd(t[1], t[2]) end
function ugenAdd(inSig1, inSig2)
  inSig1 = inSig1 or 1
  inSig2 = inSig2 or 1
  
  local tab = {
    ugen = "add",
    params = {"sig1", "sig2"},
    sig1 = inSig1,
    sig2 = inSig2,
    get = function(self, t) return self:sig1F(t) + self:sig2F(t) end
    }
  return construct(tab)
end

function ugenMixP(p) return ugenMix(p[1], p[2], p[3]) end
function ugenMix(inSig1, inSig2, inFade)
  inSig1 = inSig1 or 1
  inSig2 = inSig2 or 1
  inFade = inFade or 0.5
    
  local tab = {
    ugen = "mix",
    params = {"sig1", "sig2", "fade"},
    sig1 = inSig1,
    sig2 = inSig2,
    fade = inFade,
    get = function(self, t) return (self:sig1F(t) * self:fadeF(t) + (1.0 - self:fadeF(t)) * self:sig2F(t)) end
    }
  return construct(tab)
end

function ugenSqueezeP(p) return ugenSqueeze(p[1], p[2], p[3],p[4]) end
function ugenSqueeze(inSig1, inMin, inMax, inDistortion)
  -- squeeze sig1 so that it fits within min, max
  -- distortion factor is from 1 to ...
  -- greater distortion (approx 10.0) will approach the full range of (min, max)
  -- but will distort the signal more. High distortion values can "smooth out"
  -- jagged signals

  inSig1 = inSig1 or 1
  inMin = inMin or -1
  inMax = inMax or 1
  inDistortion = inDistortion or 1
  if inDistortion <= 0 then inDistortion = 1 end
  
  local tab = {
    ugen = "squeeze",
    params = {"sig1", "min", "max", "distortion"},
    sig1 = inSig1,
    min = inMin,
    max = inMax,
    distortion = inDistortion,
    get = function(self, t) 
      -- fit within -0.5 - +0.5
      local min = self:minF(t)
      local max = self:maxF(t)
      local offset = (max + min)/2.
      local range = (max-min)
      local sig = self:sig1F(t)
      local noDC  = sig - offset
      
      local squeezed = math.atan(math.pi*self:distortionF(t)*noDC)/math.pi
      return squeezed * range + offset
    end
    }
  return construct(tab)  
end

function ugenRandTri(period, amplitude, offset, phase, salt)
  -- oscillate back and forth like a tri, but with random peaks
  period = period or 1
  salt = salt or 42
  local rand = ugenRandStep(ugenMult(period,.5), amplitude, ugenMult(amplitude,.5), phase, salt)
  local tri = ugenTri(period, rand, offset, phase)
  return tri  
end

function ugenCommoditySwing(percentSwing, oneDay, salt)
  -- because of the squeezing, percentSwing will be more
  -- than a typical value, so we multiply by 1.3
  salt = salt or 0
  oneDay = oneDay or 1
  
  local phase = tRand(42, salt)
  local noise = ugenRandLerp(oneDay/2, .03, 0, phase, salt)
  local stretch = ugenRandLerp(oneDay*10, oneDay/2, oneDay, phase, salt)
  local daily = ugenSin(stretch, percentSwing/10, noise, phase)
  local dc = ugenRandLerp(oneDay*19, 0.1, 0, phase, salt)
  local stretch2 = ugenRandLerp(oneDay*10, 10*oneDay, 30*oneDay, 0, salt)
  local movement = ugenRandLerp(oneDay*20, 0.25, .50, phase, salt)
  local seasonal = ugenSin(stretch2, movement, dc, phase)
  local bump = ugenDelay(seasonal, oneDay*10, nil, phase)
  
  local combined = ugenAdd(daily, ugenMix(seasonal, bump))

  local tScaled = ugenTimeScale(combined, 5)
  --return noise
  return ugenSqueeze(tScaled, -percentSwing*1.15, percentSwing*1.15)
  
end

function ugenVolitility(amount, min,max, oneCycle, salt)
  local a = amount
  if a <= 0 then a = 0 end
  if a >= 1 then a = .999999 end
  salt = salt or 42
  local freqMod = ugenRandTri(oneCycle*4, amount, 1, nil, salt)
  local freq = ugenRandTri(ugenAdd(freqMod,onCycle), (max-min)/2, min)
  return freq
end


--- MUST be at bottom of file
ugenMap = {
  sin = ugenSinP,
  tri=ugenTriP,
  saw=ugenSawP,
  square=ugenSquareP,
  random=ugenRandP,
  randomlerp=ugenRandLerpP,
  randomstep=ugenRandStepP,
  pulse=ugenPulseP,
  clamp=ugenClamp,
  timeclamp=ugenTimeClampP,
  delay=ugenDelayP,
  timescale=ugenTimeScaleP,
  hold=ugenHoldP,
  mult=ugenMultP,
  add=ugenAddP,
  mix=ugenMixP,
  squeeze=ugenSqueezeP,
  constant=ugenConstantP
}
