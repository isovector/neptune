Room roomId.costume, () => do
  redDude = @newActor!
  redDude.pos = V2(474, 172)
  redDude.isAvatar = true
  redDude.hasFocus = true
  redDude.speed = 50
  hsMakeRedDude(redDude.ent)

