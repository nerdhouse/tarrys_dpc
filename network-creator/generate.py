#!/bin/python
import sys

def main():

    if len(sys.argv) != 2:
        print("Please only supply a single parameter which is the path to an input file.")
        exit(0)

    containers = []
    networks = []

    class container:
        def __init__(self,name):
            self.name = name
            self.networks = []

        def add_network(self,network):
            self.networks.append(network)


    try:
        layout = list(open(sys.argv[1],"r"))
        layout = layout[1:]

        nodes = []
        for x in range(len(layout)):
            layout[x] = layout[x][:-1]
            for y in range(len(layout[x])):
                if(layout[x][y] != " "):
                    nodes.append(layout[x][y])

        nodes = sorted(list(set(nodes)))

        for x in nodes:
            cont = container(x)
            containers.append(cont)

        
        for l in layout:
            nodes = l.split(" ")
            network = nodes[0]
            nodes = nodes[1:]
            for n in nodes:
                add_to_container(containers,network,n)


        generate_compose(containers,"docker-compose.yaml")

    except Exception as e:
        print(e)

def indent(count):
    return " "*count*4

def generate_compose(conts,outfile):
    networks = []
    compose = open(outfile,"w+")

    compose.write("version: \"2.3\"\nservices:\n")
    for cont in conts:
        compose.write(indent(1) + cont.name + ":\n")
        compose.write(indent(2) + "hostname: " + cont.name + "\n")
        compose.write(indent(2) + "entrypoint: \"tail -f /dev/null\"\n")
        compose.write(indent(2) + "build: .\n")
        compose.write(indent(2) + "networks:\n")
        for n in cont.networks:
            p = []
            p.append(n)
            p.append(cont.name)
            p = sorted(p)
            nname = p[0] + "-" + p[1] + "-network"
            networks.append(nname)
            compose.write(indent(3) + " - " + nname + "\n")


    networks = list(set(networks))
    compose.write("\n\nnetworks:\n")
    for n in networks:
        compose.write(indent(1)+n+":\n")



    compose.close()


def add_to_container(conts,network,cont):
    for c in conts:
        if(c.name == cont):
            c.add_network(network)

if __name__ == "__main__": main()
