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

