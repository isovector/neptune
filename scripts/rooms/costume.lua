return Room(roomId.costume, function(self)
  do
    local redDude = self:newActor()
    redDude.pos = V2(474, 172)
    redDude.isAvatar = true
    redDude.hasFocus = true
    redDude.speed = 50
    return hsMakeRedDude(redDude.ent)
  end
end)
