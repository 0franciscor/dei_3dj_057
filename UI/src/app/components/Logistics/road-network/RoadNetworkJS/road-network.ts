import * as THREE from "three";
import { warehousePosition, warehouseConnections } from "./default-data";
import NodeTemplate from "./node-template";


export default class roadNetworkTemplate {
    object: THREE.Group;

    constructor(parameters: any) {
        // Create a group of objects
        this.object = new THREE.Group();
        this.createNodes(parameters.positions, warehouseConnections);


    }


    private createNodes(positions: typeof warehousePosition, connections: typeof warehouseConnections) {

       //for loop positions matrix
      
       for (let index = 0, connectionID = 0; index < positions.matrix.length; index++, connectionID+=2) {
            const element = positions.matrix[index];
            const whConnections = {
                connection1: connections.matrix[connectionID],
                connection2: connections.matrix[connectionID+1],
            }
           
            const node = new NodeTemplate(element,whConnections, positions.matrix);
            this.object.add(node.object);
    
        
       }

    }
}