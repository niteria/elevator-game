(function() {
  var onIdle = function() {}
  HASKELL
  return {
    init: function(elevators, floors) {

        let elevator = elevators[0]; // Let's use the first elevator

        elevator.on("idle", function() {
            console.log("before");
            onIdle(elevator);
            console.log("after");
        });

    },
    update: function(dt, elevators, floors) {
        // We normally don't need to do anything here
    }
  }
})()
