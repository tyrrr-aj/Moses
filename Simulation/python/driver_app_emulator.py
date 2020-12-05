import threading

# import jnius_config
# jnius_config.set_classpath('C:\\Users\\adams\\Projekty\\Moses\\RabbitMQConnector\\out\\artifacts\\RabbitMQConnector_jar\\RabbitMQConnector.jar')
import jnius

from jnius import autoclass, PythonJavaClass, java_method


class DriverAppEmulator:
    def __init__(self, vehicle):
        NotificationsReceiver = autoclass('com.moses.driverapp.NotificationsReceiver')
        
        displayer = Displayer(vehicle)
        gps_accessor = GPSAccessor(vehicle)

        self.receiver = NotificationsReceiver(gps_accessor, displayer)
    
    # def update_localization(self, lat, lon):
    #     coords = self._get_coords_object(lon, lat)
    #     return self.connector.updateLocalization(coords)

    def listen_for_notifications(self):
        thread = threading.Thread(target=self.receiver.receiveNotifications)
        thread.start()
    
    # def setup_and_listen(self, callback):
    #     local_conn = Connector()
    #     local_conn.setup_connection()
    #     queue = local_conn.setup_queue(exchange, self.routing_key)
    #     self.connector.listen_for_messages(queue, callback)


class GPSAccessor(PythonJavaClass):
    __javainterfaces__ = ['com/moses/driverapp/interfaces/GPSAccessor']

    def __init__(self, vehicle):
        super().__init__(self)
        self.vehicle = vehicle
        self._gps_coords_class = autoclass('com.moses.driverapp.dto.GPSCoords')

    @java_method('()Lcom/moses/driverapp/dto/GPSCoords;')
    def getCurrentCoords(self):
        return self._get_coords_object(*self.vehicle.coords[::-1])
    
    def _get_coords_object(self, lon, lat):
        return self._gps_coords_class(lon, lat)


class Displayer(PythonJavaClass):
    __javainterfaces__ = ['com/moses/driverapp/interfaces/Displayer']

    def __init__(self, vehicle):
        super().__init__(self)
        self.vehicle = vehicle
    
    @java_method('(Lcom/moses/notifications/Notification;)V')
    def displayNotification(self, notification):
        vehicle.highlight()


# class NotificationCallback(PythonJavaClass):
#     __javainterfaces__ = ['java/util/function/Consumer']

#     def __init__(self, callback):
#         super(NotificationCallback, self).__init__()
#         self.callback = callback
    
#     @java_method('(Ljava/lang/Object;)V')
#     def accept(self, notification):
#         self.callback(notification)
