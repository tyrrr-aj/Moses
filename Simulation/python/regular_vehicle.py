from vehicle import Vehicle, position_update_interval
import tracking
from driver_app_emulator import DriverAppEmulator

import traci


class RegularVehicle(Vehicle):
    def __init__(self, vehicle_id):
        super().__init__(vehicle_id)
        self.step_no = 0
        self.coords = tracking.get_position(self.vehicle_id)
        self.app = DriverAppEmulator(self)
        self.app.listen_for_notifications()
    
    def step(self):
        self.step_no += 1
        if (self.step_no % position_update_interval == 0):
            self.update_localization()
            self.step_no = 0
    
    def update_localization(self):
        self.coords = tracking.get_position(self.vehicle_id)
        # new_position = self.app.update_localization(*coords)
        # if new_position is not None:
        #     print(f'{self.vehicle_id}\'s position is {new_position.toString()}')
        # else:
        #     print(f'{self.vehicle_id}\'s position unknown')

    def highlight(self):
        traci.vehicle.highlight(self.vehicle_id)

    # def react_to_notification(self, notification_body):
    #     print(f'{self.vehicle_id} notified: {notification_body}')

    def stop(self):
        self.app.stop()
