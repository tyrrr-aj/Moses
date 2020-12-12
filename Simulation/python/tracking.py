import threading
import traci


def get_position(vehicleId):
    try:
        raw_position = traci.vehicle.getPosition(vehicleId)
        return traci.simulation.convertGeo(*raw_position)
    except:
        return 'error'

def print_position(position, vehicleId):
    lon, lat = position
    print(f'{vehicleId}: lon={lon}, lat={lat}')

def track(traci, vehicleId):
    position = ''
    while traci.simulation.getMinExpectedNumber() > 0 and position != 'error':
        position = get_position(traci, vehicleId)
        print_position(position, vehicleId)

def track_in_new_thread(traci, vehicleId):
    threading.Thread(target=track, args=(traci, vehicleId, )).start()
