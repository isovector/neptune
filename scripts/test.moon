export player

player = getPlayer()
player.interact = (verb) => do
  @say(verb)
  @pos = V2(474, 172)

