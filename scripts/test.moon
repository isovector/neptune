export player

player = hsGetPlayer()
player.interact = (verb) => do
  @say(verb)
  @walkTo(V2(474, 172))
  sleep(3)
  @say("hello from the taskmgr")

