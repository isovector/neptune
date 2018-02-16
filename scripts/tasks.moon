export TaskManager, tasks, sleep, waitForEvent

class Task
  new: (fn) => do
    @job = coroutine.create(fn)
    @time = 0
    @event = nil


class TaskManager
  new: => @tasks = {}

  start: (fn) => do
    table.insert(@tasks, Task(fn))

  continue: (dt) =>
    @jobs = do
      for _, task in ipairs(@tasks)
        task.time -= dt
        if not task.event and task.time <= 0
          status, typ, arg = coroutine.resume(task.job)
          continue unless status
          switch typ
            when "sleep"
              task.time = arg
            when "waitForEvent"
              task.event = arg
        task

tasks = TaskManager!

sleep        = (dt) -> coroutine.yield("sleep", dt)
waitForEvent = (ev) -> coroutine.yield("waitForEvent", ev)

