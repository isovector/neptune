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
