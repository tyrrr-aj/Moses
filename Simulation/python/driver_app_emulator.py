import threading

# import jnius_config
# jnius_config.set_classpath('C:\\Users\\adams\\Projekty\\Moses\\RabbitMQConnector\\out\\artifacts\\RabbitMQConnector_jar\\RabbitMQConnector.jar')
import jnius

from jnius import autoclass, PythonJavaClass, java_method


class DriverAppEmulator:
    def __init__(self):
        DriverAppConnector = autoclass('com.moses.driverapp.DriverAppConnector')
        self.connector = DriverAppConnector()
    
    def update_localization(self, lat, lon):
        return self.connector.updateLocalization(lon, lat)

    def listen_for_notifications(self, callback):
        thread = threading.Thread(target=self.connector.listenForNotifications, args=(NotificationCallback(callback),))
        thread.start()
    
    # def setup_and_listen(self, callback):
    #     local_conn = Connector()
    #     local_conn.setup_connection()
    #     queue = local_conn.setup_queue(exchange, self.routing_key)
    #     self.connector.listen_for_messages(queue, callback)


class NotificationCallback(PythonJavaClass):
    __javainterfaces__ = ['java/util/function/Consumer']

    def __init__(self, callback):
        super(NotificationCallback, self).__init__()
        self.callback = callback
    
    @java_method('(Ljava/lang/Object;)V')
    def accept(self, notification):
        self.callback(notification)
