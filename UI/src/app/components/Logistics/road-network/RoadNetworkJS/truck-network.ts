import * as THREE from "three";
import Truck from "./truck";


export default class TruckNetwork {
    object: THREE.Group;
    truckNames: string[] = [];
    constructor(positions: any,paths: any, trucks: any[]) {
        this.object = new THREE.Group();
        this.createTrucks(positions, paths, trucks);

    }

   

    private createTrucks(warehousePos: any, paths:any, trucks: any[]) {
        let chosenWarehouses: any[] = [];
        trucks.forEach((truck: any) => {
            //choose random warehouse from warehousePos
            let randomNumber = Math.floor(Math.random() * warehousePos.length);
            while(chosenWarehouses.includes(randomNumber))
                randomNumber = Math.floor(Math.random() * warehousePos.length); 
            chosenWarehouses.push(randomNumber);
            let randomWarehouse = warehousePos[randomNumber];
            let truckObject = new Truck(randomWarehouse, paths);
            this.object.add(truckObject.object);
            this.truckNames.push(truck.truckID);

        });
        

    

    }
}