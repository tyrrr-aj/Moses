import os, sys, time, traceback
import traci
from traci.exceptions import FatalTraCIError

import jpype
from jpype.types import *
import jpype.imports

jpype.startJVM(classpath=['/home/adams/Projekty/Moses/RabbitMQConnector/out/artifacts/RabbitMQConnector_jar/RabbitMQConnector.jar'])

from ev import Ev
from regular_vehicle import RegularVehicle

from com.moses.simulation import AppConnector

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

def setup_app_connector():
    app_connector = AppConnector()
    app_connector.startTracking()
    return app_connector

def update_app_tracking(app_connector):
    tracked = app_connector.getTrackedVehicles()
    for vehicle_id in tracked:
        app_connector.updateVehiclePosition(vehicle_id, *vehicles[vehicle_id].coords[::-1])

def setupCmd(simulation_directory_name):
    sumoBinary = "/usr/bin/sumo-gui"
    base_dir = '../sumo/scenarios'

    with open(os.path.join(base_dir, simulation_directory_name, 'evlist.txt'), 'r') as list_file:
        ev_list = list_file.read().replace(' ', '')

    config_path = os.path.join(base_dir, simulation_directory_name, f'{simulation_directory_name}.sumocfg')

    return [sumoBinary, "-c", config_path, '-d', '1000', '--device.bluelight.explicit', ev_list]

def runSimulation(sumoCmd):
    traci.start(sumoCmd)
    app_connector = setup_app_connector()

    try:
        while traci.simulation.getMinExpectedNumber() > 0:
            traci.simulationStep()
            update_vehicles()
            vehicles_make_step()
            update_app_tracking(app_connector)
    
    except KeyboardInterrupt:
        print('simulation interrupted')
        stop_tracking_vehicles(list(vehicles.keys()))
        traci.close()
        app_connector.stop()
        return
    except FatalTraCIError:
        print('simulation interrupted (stopped by TraCI)')
        traceback.print_exc()
        stop_tracking_vehicles(list(vehicles.keys()))
        app_connector.stop()
        return

    stop_tracking_vehicles(list(vehicles.keys()))
    app_connector.stop()
    traci.close()

if __name__ == '__main__':
    try:
        if 'SUMO_HOME' in os.environ:
            tools = os.path.join(os.environ['SUMO_HOME'], 'tools')
            sys.path.append(tools)

            if len(sys.argv) < 2:
                sys.exit('no simulation specified: quiting')
            else:
                simulation_directory_name = sys.argv[1]

            sumoCmd = setupCmd(simulation_directory_name)
            runSimulation(sumoCmd)
        else:
            sys.exit("please declare environment variable 'SUMO_HOME'")
    finally:
        jpype.shutdownJVM()

