import traci

from vehicle import Vehicle

class Ev(Vehicle):
    def __init__(self, vehicle_id):
        super().__init__(vehicle_id)
        self.dispatch()
    
    def dispatch(self):
        print('Dispatching ' + self.vehicle_id)
