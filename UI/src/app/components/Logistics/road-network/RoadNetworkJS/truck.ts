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

    
    constructor(pos: posProps, paths:any) {
        this.object = new THREE.Group();
        this.warehouseOG = pos.wh;
        const thisWH = paths.filter((path: any) => path.wh == pos.wh).at(0);
        
        const startPositionConstant = 0.6;
        const startPositonOffset = startPositionConstant*thisWH.width; 
        this.object = new THREE.Group();
        const truckTexture = new THREE.Object3D();
        const truckScale = 0.00025;
        const truckloader = new GLTFLoader();
        truckloader.load(
            './assets/italeri_truck/scene.gltf', 
            (object) => {
                object.scene.traverse(function(node){
                    if(node)
                        node.castShadow = true;
                });
    

            truckTexture.add(object.scene);

        });
        truckTexture.scale.set(truckScale, truckScale, truckScale);
        truckTexture.castShadow = true;
        
        truckTexture.position.set(pos.x, pos.y - startPositonOffset, pos.z + 0.1);
        const target = new THREE.Vector3();
        truckTexture.getWorldDirection(target);
        
        
        this.object.add(truckTexture);

        this.object.name = pos.wh + "Truck";
      



    }
}