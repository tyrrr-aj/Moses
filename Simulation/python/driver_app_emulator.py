import jpype.imports
from jpype import JImplements, JOverride

from com.moses.driverapp.backend import NotificationsReceiver

from com.moses.driverapp.backend.interfaces import Displayer, GPSAccessor
from com.moses.driverapp.backend.dto import GPSCoords


class DriverAppEmulator:
    def __init__(self, vehicle):        
        displayer = Displayer(vehicle)
        gps_accessor = GPSAccessor(vehicle)

        self.receiver = NotificationsReceiver(gps_accessor, displayer)
    
    # def update_localization(self, lat, lon):
    #     coords = self._get_coords_object(lon, lat)
    #     return self.connector.updateLocalization(coords)

    def listen_for_notifications(self):
        self.receiver.receiveNotifications()
    
    # def setup_and_listen(self, callback):
    #     local_conn = Connector()
    #     local_conn.setup_connection()
    #     queue = local_conn.setup_queue(exchange, self.routing_key)
    #     self.connector.listen_for_messages(queue, callback)

    def stop(self):
        self.receiver.shutdown()


@JImplements(GPSAccessor)
class GPSAccessor:
    def __init__(self, vehicle):
        self.vehicle = vehicle

    @JOverride
    def getCurrentCoords(self):
        return GPSCoords(*self.vehicle.coords[::-1])


@JImplements(Displayer)
class Displayer:
    def __init__(self, vehicle):
        self.vehicle = vehicle
    
    @JOverride
    def displayNotification(self, notification):
        self.vehicle.highlight()
        print(f'[{self.vehicle.vehicle_id}] received notification!')
