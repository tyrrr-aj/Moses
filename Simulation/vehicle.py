import traci
import tracking


position_update_interval = 5

class Vehicle:
    def __init__(self, vehicle_id):
        self.vehicle_id = vehicle_id
        self.step_no = 0
    
    def stop(self):
        print('Vehicle left: ' + self.vehicle_id)

    def step(self):
        self.step_no += 1
        if (self.step_no % position_update_interval == 0):
            self.update_position()
            self.step_no = 0
    
    def update_position(self):
        print(f'{self.vehicle_id}\'s position is {tracking.get_position(self.vehicle_id)}')
