import jpype.imports

from com.moses.simulation import EVConnector


class LocalEvConnector:
    def __init__(self, vehicle_id, vehicle_type):\
        self.connector = EVConnector(vehicle_type, vehicle_id)

    def send_dispatch_message(self, lat, lon):
        self.connector.sendDispatchMessageAsync(lon, lat)

    def send_tracking_update(self, lat, lon):
        self.connector.sendTrackingMessageAsync(lon, lat)
    
    def send_end_ride_message(self):
        self.connector.sendEndRideMessageAsync()
