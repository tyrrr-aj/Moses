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
        self.should_make_way = False
        self.remaining_stop = 0
        self.was_calmed = False
    
    def step(self):
        self.step_no += 1
        if (self.step_no % position_update_interval == 0):
            self.update_localization()
            self.step_no = 0
        
        if self.should_make_way:
            traci.vehicle.highlight(self.vehicle_id, alphaMax=255, duration=10)
            self.should_make_way = False
            traci.vehicle.setSpeed(self.vehicle_id, 5)

        if self.remaining_stop > 0:
            self.remaining_stop -= 1
            if self.remaining_stop == 0:
                traci.vehicle.setSpeed(self.vehicle_id, -1)

        if self.was_calmed:
            traci.vehicle.highlight(self.vehicle_id, color=(0, 0, 255, 255), alphaMax=255, duration=10)
            self.was_calmed = False
        
    
    def update_localization(self):
        self.coords = tracking.get_position(self.vehicle_id)

    def make_way(self):
        self.should_make_way = True
        self.remaining_stop = stop_duration

    def be_calmed(self):
        self.was_calmed = True

    def stop(self):
        self.app.stop()
