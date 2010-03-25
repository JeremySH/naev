--[[

   Pirate Bounty

   Randomly appearing bar mission to kill a unique pirate.

   Author: bobbens

--]]

-- Localization, choosing a language if naev is translated for non-english-speaking locales.
lang = naev.lang()
if lang == "es" then
else -- Default to English
   -- Bar information
   bar_desc = "You see an Empire Official handing out bounty information. There seems to be an interested crowd gathering around."

   -- Mission details
   misn_title  = "Pirate Bounty near %s"
   misn_reward = "%d credits"
   misn_desc   = "There is a bounty on the head of the pirate known as %s who was last seen near the %s system."

   -- Text
   title    = {}
   text     = {}
   title[1] = "Spaceport Bar"
   text[1]  = [[It seems like the bounty is on the head of a pirate terrorizing the area known as %s for %d credits. It seems like he was last seen in the %s system. Quite a few other mercenaries seem interested and it looks like you'll have to outrace them.
   
Will you take up the bounty?]]
   text[2] = [[You roll up your sleeve and grab one of the pamphlets given out by the Empire official.]]

   -- Messages
   msg      = {}
   msg[1]   = "MISSION SUCCESS! Payment received."
   msg[2]   = "Pursue %s!"
end


include("dat/missions/empire/common.lua")


-- Scripts we need
include("scripts/pilot/pirate.lua")


function create ()
   -- Create the target pirate
   pir_name, pir_ship, pir_outfits = pir_generate()

   -- Get target system
   near_sys = get_pir_system( system.get() )

   -- Get credits
   credits  = rnd.rnd(5,10) * 10000

   -- Spaceport bar stuff
   misn.setNPC( "Official", emp_getOfficialRandomPortrait() )
   misn.setDesc( bar_desc )
end


--[[
Mission entry point.
--]]
function accept ()
   -- Mission details:
   if not tk.yesno( title[1], string.format( text[1],
         pir_name, credits, near_sys:name() ) ) then
      misn.finish()
   end
   misn.accept()

   -- Set mission details
   misn.setTitle( string.format( misn_title, near_sys:name()) )
   misn.setReward( string.format( misn_reward, credits) )
   misn.setDesc( string.format( misn_desc, pir_name, near_sys:name() ) )
   misn.setMarker( near_sys, "misc" )

   -- Some flavour text
   tk.msg( title[1], text[2] )

   -- Set hooks
   hook.enter("sys_enter")
end


-- Gets a piratey system
function get_pir_system( sys )
   local adj_sys = sys:adjacentSystems()

   -- Only take into account system with pirates.
   local pir_sys = {}
   for k,v in ipairs(adj_sys) do
      if k:hasPresence( "Pirate" ) then
         table.insert( pir_sys, k )
      end
   end

   -- Make sure system has pirates
   if #pir_sys == nil then
      return sys
   else
      return pir_sys[ rnd.rnd(1,#pir_sys) ]
   end
end

-- Player won, gives rewards.
function give_rewards ()
   -- Give monies
   player.pay(credits)

   -- Give factions
   player.modFaction( "Empire", 5 )
   
   -- Finish mission
   misn.finish(true)
end


-- Entering a system
function sys_enter ()
   cur_sys = system.get()
   -- Check to see if reaching target system
   if cur_sys == near_sys then

      -- Create the badass enemy
      p     = pilot.add(pir_ship)
      pir   = p[1]
      pir:rename(pir_name)
      pir:setHostile()
      pir:rmOutfit("all") -- Start naked
      pilot_outfitAddSet( pir, pir_outfits )
      hook.pilot( pir, "death", "pir_dead" )
      hook.pilot( pir, "jump", "pir_jump" )
   end
end


-- Pirate is dead
function pir_dead ()
   player.msg( msg[1] )
   give_rewards()
end


-- Pirate jumped away
function pir_jump ()
   player.msg( string.format(msg[2], pir_name) )

   -- Basically just swap the system
   near_sys = get_pir_system( near_sys )
end


--[[
Functions to create pirates based on difficulty more easily.
--]]
function pir_generate ()
   -- Get the pirate name
   pir_name = pirate_name()

   -- Get the pirate details
   rating = player.getRating()
   if rating < 50 then
      pir_ship, pir_outfits = pir_easy()
   elseif rating < 150 then
      pir_ship, pir_outfits = pir_medium()
   else
      pir_ship, pir_outfits = pir_hard()
   end

   -- Make sure to save the outfits.
   pir_outfits["__save"] = true

   return pir_name, pir_ship, pir_outfits
end
function pir_easy ()
   if rnd.rnd() < 0.5 then
      return pirate_createAncestor(false)
   else
      return pirate_createVendetta(false)
   end
end
function pir_medium ()
   if rnd.rnd() < 0.5 then
      return pirate_createAdmonisher(false)
   else
      return pir_easy()
   end
end
function pir_hard ()
   if rnd.rnd() < 0.5 then
      return pirate_createKestrel(false)
   else
      return pir_medium()
   end
end

