import traci

from vehicle import Vehicle, position_update_interval
from ev_server_connector import LocalEvConnector
from tracking import get_position


class Ev(Vehicle):
    def __init__(self, vehicle_id, vehicle_type='simulation'):
        super().__init__(vehicle_id)
        self.step_no = 0
        self.connector = LocalEvConnector(vehicle_id, vehicle_type)
        self.dispatch()
    
    def dispatch(self):
        coords = get_position(self.vehicle_id)
        self.connector.send_dispatch_message(*coords)
    
    def step(self):
        self.step_no += 1
        if (self.step_no % position_update_interval == 0):
            self.send_localization_update()
            self.step_no = 0

    def send_localization_update(self):
        coords = get_position(self.vehicle_id)
        self.connector.send_tracking_update(*coords)

    def stop(self):
        self.connector.send_end_ride_message()
