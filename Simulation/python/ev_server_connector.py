import jnius

from jnius import autoclass

exchange_name = 'moses_simulation_exchange'

dispatch_routing_key_root = 'dispatch'
tracking_routing_key_root = 'tracking'

class EvConnector:
    def __init__(self, vehicle_id, vehicle_type):
        ExtEvConnector = autoclass('com.moses.simulation.EVConnector')
        self.connector = ExtEvConnector(vehicle_type, vehicle_id)

    def send_dispatch_message(self, lat, lon):
        self.connector.sendDispatchMessageAsync(lon, lat)

    def send_tracking_update(self, lat, lon):
        self.connector.sendTrackingMessageAsync(lon, lat)
    
    def send_end_ride_message(self):
        self.connector.sendEndRideMessageAsync()
