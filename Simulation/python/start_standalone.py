import os, sys
import traci


def setup_cmd():
    sumoBinary = "C:\\Program Files (x86)\\Eclipse\\Sumo\\bin\\sumo-gui.exe"
    return [sumoBinary, "-c", "..\\sumo\\osm.sumocfg", '--device.bluelight.explicit', 'amb0,pol0,fir0', '--num-clients', '2']

def start_server(sumo_cmd):
    traci.start(sumo_cmd, port=1111)
    traci.setOrder(1)

    #tracking.track_in_new_thread(traci, 'veh0')
    # vehicleId = 'veh0'
    # traci.vehicle.highlight(vehicleId)

    while traci.simulation.getMinExpectedNumber() > 0:
        traci.simulationStep()

    traci.close()


if __name__ == '__main__':
    if 'SUMO_HOME' in os.environ:
        tools = os.path.join(os.environ['SUMO_HOME'], 'tools')
        sys.path.append(tools)

        sumo_cmd = setup_cmd()
        start_server(sumo_cmd)
    else:
        sys.exit("please declare environment variable 'SUMO_HOME'")