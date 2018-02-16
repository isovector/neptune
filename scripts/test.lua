player = hsGetPlayer()
player.interact = function(self, verb)
  do
    self:say(verb)
    self:walkTo(V2(474, 172))
    sleep(3)
    return self:say("hello from the taskmgr")
  end
end
