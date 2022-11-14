import * as THREE from "three";
import { warehousePosition, warehouseConnections } from "./default-data";
import NodeTemplate from "./node-template";


export default class roadNetworkTemplate {
    object: THREE.Group;

    constructor(parameters: any) {
        this.object = new THREE.Group();
        this.createNodes(parameters.positions, warehouseConnections);
    }


    private createNodes(positions: typeof warehousePosition, connections: typeof warehouseConnections) {

      
       for (let index = 0, connectionID = 0; index < positions.matrix.length; index++, connectionID+=2) {
            const element = positions.matrix[index];
            const whConnections = [connections.matrix[connectionID], connections.matrix[connectionID+1]]
           
            connections.matrix.forEach(element => {
                if(element[1] == index){
                    whConnections.push(positions.matrix[element[0]-1]);
                }
            });

            const node = new NodeTemplate(element,whConnections);
            this.object.add(node.object);
    
       }

    }
}