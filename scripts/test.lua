player = hsGetPlayer()
player.interact = function(self, verb)
  do
    self:say(verb)
    return self:walkTo(V2(474, 172))
  end
end
