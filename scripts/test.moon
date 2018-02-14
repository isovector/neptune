export player

player = hsGetPlayer()
player.interact = (verb) => do
  @say(verb)
  @walkTo(V2(474, 172))

