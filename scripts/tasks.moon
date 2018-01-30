export TaskManager, tasks

class TaskManager
  new: =>
    @jobs = {}

  start: (f) => do
    job = coroutine.create(f)
    table.insert(@jobs, job)

  continue: => do
    @jobs = for _, job in ipairs(@jobs)
              continue unless coroutine.resume(job)
              job

tasks = TaskManager!

