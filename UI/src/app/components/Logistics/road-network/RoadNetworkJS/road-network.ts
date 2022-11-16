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
            // console.log(connections.matrix[connectionID][2])
            const whConnections = [positions.matrix[connections.matrix[connectionID][1]-1], positions.matrix[connections.matrix[connectionID+1][1]-1]];
            const roadWidth: number[] = [];
            connections.matrix.forEach(element => {
                roadWidth.push(element[2]);
                if(element[1] == index+1){
                    whConnections.push(positions.matrix[element[0]-1]);
                }
            });
            
            const node = new NodeTemplate(element,whConnections,roadWidth);
            this.object.add(node.object);
    
       }

    }
}