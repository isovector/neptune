rooms[roomId.costume]:load()
print(hsGetPlayer())
player = getPlayer()
player.interact = function(self, verb)
  do
    self:say(verb)
    self.pos = V2(474, 172)
  end
end
