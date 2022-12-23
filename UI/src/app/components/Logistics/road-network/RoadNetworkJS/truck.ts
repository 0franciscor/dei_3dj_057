import * as THREE from "three";

import { GLTFLoader } from "three/examples/jsm/loaders/GLTFLoader";
interface posProps {
    wh: string,
    x: number,
    y: number,
    z: number
}

export default class Truck {
    object: THREE.Group;
    warehouseOG: string;
    mass = 8000;

    
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

            truckTexture.add(object.scene);

        });
        truckTexture.scale.set(truckScale, truckScale, truckScale);
        // truckTexture.rotateZ(Math.PI / 2);
        truckTexture.rotateX(Math.PI / 2);
        truckTexture.position.set(pos.x, pos.y - startPositonOffset, pos.z + 0.003);

        this.object.add(truckTexture);

        this.object.name = pos.wh + "Truck";
      



    }
}