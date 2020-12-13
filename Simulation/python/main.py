import os, sys, time
import traci
from traci.exceptions import FatalTraCIError

import jpype
from jpype.types import *

jpype.startJVM(classpath=['/home/adams/Projekty/Moses/RabbitMQConnector/out/artifacts/RabbitMQConnector_jar/RabbitMQConnector.jar'])

from ev import Ev
from regular_vehicle import RegularVehicle


ev_types = ['ambulance', 'firebrigade', 'police']

vehicles = {}

def get_new_vehicles():
    return {vehicle_id: traci.vehicle.getTypeID(vehicle_id) for vehicle_id in traci.simulation.getDepartedIDList()}

def start_tracking_new_vehicles(new_vehicles):
    for vehicle_id, vehicle_type in new_vehicles.items():
        if vehicle_type in ev_types:
            vehicles[vehicle_id] = Ev(vehicle_id)
        else:
            vehicles[vehicle_id] = RegularVehicle(vehicle_id)


def get_vehicles_that_left_map():
    return traci.simulation.getArrivedIDList()

def stop_tracking_vehicles(vehilce_id_list):
    for vehicle_id in vehilce_id_list:
        vehicles.pop(vehicle_id).stop()

def update_vehicles():
    new_vehicles = get_new_vehicles()
    start_tracking_new_vehicles(new_vehicles)
    vehicles_that_left = get_vehicles_that_left_map()
    stop_tracking_vehicles(vehicles_that_left)

def vehicles_make_step():
    for vehicle in vehicles.values():
        vehicle.step()

def setupCmd():
    sumoBinary = "/usr/bin/sumo-gui"
    return [sumoBinary, "-c", "../sumo/osm.sumocfg", '--device.bluelight.explicit', 'amb0,pol0,fir0']

def runSimulation():
    traci.start(sumoCmd)

    try:
        while traci.simulation.getMinExpectedNumber() > 0:
            traci.simulationStep()
            update_vehicles()
            vehicles_make_step()
    
    except KeyboardInterrupt:
        print('simulation stopped before finished')
        stop_tracking_vehicles(list(vehicles.keys()))
        traci.close()
        return
    except FatalTraCIError:
        print('simulation stopped before finished')
        stop_tracking_vehicles(list(vehicles.keys()))
        return

    stop_tracking_vehicles(list(vehicles.keys()))
    traci.close()

if __name__ == '__main__':
    if 'SUMO_HOME' in os.environ:
        tools = os.path.join(os.environ['SUMO_HOME'], 'tools')
        sys.path.append(tools)

        sumoCmd = setupCmd()
        runSimulation()
        traci.init()
        runSimulation()
    else:
        sys.exit("please declare environment variable 'SUMO_HOME'")

jpype.shutdownJVM()