from vehicle import Vehicle, position_update_interval
import tracking
from driver_app_emulator import DriverAppEmulator

import traci


stop_duration = 10


class RegularVehicle(Vehicle):
    def __init__(self, vehicle_id):
        super().__init__(vehicle_id)
        self.step_no = 0
        self.coords = tracking.get_position(self.vehicle_id)
        self.app = DriverAppEmulator(self)
        self.app.listen_for_notifications()
        self.received_notification = False
        self.remaining_stop = 0
    
    def step(self):
        self.step_no += 1
        if (self.step_no % position_update_interval == 0):
            self.update_localization()
            self.step_no = 0
        
        if self.received_notification:
            traci.vehicle.highlight(self.vehicle_id, alphaMax=255, duration=10)
            self.received_notification = False
            traci.vehicle.setSpeed(self.vehicle_id, 1)

        if self.remaining_stop > 0:
            self.remaining_stop -= 1
            if self.remaining_stop == 0:
                traci.vehicle.setSpeed(self.vehicle_id, -1)
        
    
    def update_localization(self):
        self.coords = tracking.get_position(self.vehicle_id)

    def highlight(self):
        self.received_notification= True
        self.remaining_stop = stop_duration

    def stop(self):
        self.app.stop()
