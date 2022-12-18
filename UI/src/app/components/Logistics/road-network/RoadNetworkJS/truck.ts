import * as THREE from "three";

import { GLTFLoader } from "three/examples/jsm/loaders/GLTFLoader";
interface posProps {
    wh: string,
    x: number,
    y: number,
    z: number
}

interface whAndWidth {
    wh: string,
    width: number
}
export default class Truck {
    object: THREE.Group;
    warehouseOG: string;

    
    constructor(pos: posProps, paths:any) {
        this.object = new THREE.Group();
        this.warehouseOG = pos.wh;
        const thisWH = paths.filter((path: any) => path.wh == pos.wh).at(0);
        
        const startPositionConstant = 0.6;
        const startPositonOffset = startPositionConstant*thisWH.width; 
        this.object = new THREE.Group();
        const truckTexture = new THREE.Object3D();
        const truckloader = new GLTFLoader();
        const truckScale = 0.00025;
        truckloader.load(
            './assets/italeri_truck/scene.gltf', 
            (object) => {
            // object.scene.scale.set(truckScale, truckScale, truckScale);
            // object.scene.position.set(pos.x, pos.y - startPositonOffset, pos.z + 0.003);
            // object.scene.rotateZ(Math.PI / 2);
            // object.scene.rotateX(Math.PI / 2);
            truckTexture.add(object.scene);

        });
        truckTexture.scale.set(truckScale, truckScale, truckScale);
        // truckTexture.rotateZ(Math.PI / 2);
        truckTexture.rotateX(Math.PI / 2);
        truckTexture.position.set(pos.x, pos.y - startPositonOffset, pos.z + 0.003);

        this.object.add(truckTexture);
        // this.object.position.set(pos.x, pos.y - startPositonOffset, pos.z + 0.003);
        // this.object.rotateZ(Math.PI / 2);
        // this.object.rotateX(Math.PI / 2);
        // this.object.scale.set(truckScale, truckScale, truckScale);
        this.object.name = pos.wh + "Truck";
      



    }
}