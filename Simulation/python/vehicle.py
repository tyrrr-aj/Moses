position_update_interval = 5

class Vehicle:
    def __init__(self, vehicle_id):
        self.vehicle_id = vehicle_id

    def stop(self):
        print('Vehicle left: ' + self.vehicle_id)
