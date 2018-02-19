export Room, roomId, rooms

class Room
  new: (@name, @load) => rooms[@name] = @
  newActor: () => do
    ent = Actor!
    ent.fromRoom = @name
    ent

roomId = enum { "costume", "city" }
rooms = {}

