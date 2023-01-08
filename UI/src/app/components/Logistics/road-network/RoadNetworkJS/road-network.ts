import * as THREE from "three";
import NodeTemplate from "./node-template";

interface whAndWidth {
    wh: string,
    width: number
}

export default class roadNetworkTemplate {
    object: THREE.Group;
    whAndWidths: whAndWidth[] = [];
    constructor(parameters: any) {
        this.object = new THREE.Group();

        this.createNodes(parameters.positions, parameters.paths);

        

    }

    

    public static calculatePositions(warehouses: any[]) {
        let positions: any[] = [];
        const scaleX = 1.5;
        const scaleY = 1.5;
        const scaleZ = 0.25; //height
        warehouses.forEach(warehouse => {
            let latitudeString = warehouse.latitude.toString().replace(/º N/,"");
            let latitude = parseFloat(latitudeString);

            let longitudeString = warehouse.longitude.toString().replace(/º W/,"");
            let longitude = parseFloat(longitudeString);

            let height = warehouse.altitude

            let x = ( (50 - (-50))/(8.7613-8.2451) )*(longitude-8.2451) + (-50);
            let y = ( (50 - (-50))/(42.1115-40.8387) )*(latitude-40.8387) + (-50);
            let z = ((50 -0)/800-0) * (height-0) + 0;
            
            x=x*scaleX;
            y=y*scaleY;
            z=z*scaleZ;

            positions.push({wh:warehouse.id, x: x, y: y, z: z });
        });
        return positions;

    }

    private createNodes(positions: any, connections: any) {
        
      
        positions.forEach((element: any) => {
            
            let originatingFrom = connections.filter((connection: any) => connection.startWHId == element.wh);
            let comingConnections = connections.filter((connection: any) => connection.destinationWHId == element.wh);

            let node = new NodeTemplate(element,originatingFrom,comingConnections,positions);
            this.object.add(node.object);
            this.whAndWidths.push(node.whAndWidth);
        });

    

    }
}