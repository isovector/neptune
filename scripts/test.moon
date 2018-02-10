export player

player = Actor!
player.interact = (verb) => do
  hsSay(1, 0, 1, 50, 50, verb)

tasks\start ->
  print("hello")
  coroutine.yield()
  coroutine.yield()
  print("goodbye")

tasks\start ->
  print("hello2")
  for i=1,10
    coroutine.yield()
    print(i)

