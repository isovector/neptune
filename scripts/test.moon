export player

rooms[roomId.costume]\load()

player = getPlayer()
player.interact = (verb) => do
  @say(verb)
  @pos = V2(474, 172)

