player = Actor()
player.interact = function(self, verb)
  do
    local pos = hsEntPos(hsGetPlayer())
    return hsSay({
      1,
      0,
      1
    }, pos, verb)
  end
end
tasks:start(function()
  print("hello")
  coroutine.yield()
  coroutine.yield()
  return print("goodbye")
end)
return tasks:start(function()
  print("hello2")
  for i = 1, 10 do
    coroutine.yield()
    print(i)
  end
end)
