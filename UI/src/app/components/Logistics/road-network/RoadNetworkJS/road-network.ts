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

        console.log(positions)

      
       for (let index = 0, connectionID = 0; index < positions.matrix.length; index++, connectionID+=2) {
            const element = positions.matrix[index];
            const whConnections = {
                connection1: connections.matrix[connectionID],
                connection2: connections.matrix[connectionID+1],
            }
           let destinations:any[][] = [];
            for(let i = 0; i < connections.matrix.length; i++){
                
                if(connections.matrix[i][1] == index+1){
                    destinations.push(positions.matrix[connections.matrix[i][0]-1]);
                }
            }
            
            const node = new NodeTemplate(element,whConnections, positions.matrix, destinations);
            this.object.add(node.object);
    
        
       }

    }
}