import * as THREE from "three";
import Truck from "./truck";


export default class truckNetowrk {
    object: THREE.Group;
    warehouseNames: string[] = [];
    constructor(positions: any,paths: any) {
        this.object = new THREE.Group();
        this.createTrucks(positions, paths);
    }

   

    private createTrucks(warehousePos: any, paths:any) {
        
      
        warehousePos.forEach((pos: any) => {
            
         

            let truck = new Truck(pos, paths);
            this.object.add(truck.object);
            this.warehouseNames.push(truck.warehouseOG);
        });

    

    }
}