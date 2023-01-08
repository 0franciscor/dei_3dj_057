import * as THREE from 'three';
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader';
import * as YUKA from 'yuka';

export default class AutoTruck {

    public time!: YUKA.Time;
    public entityManager!: YUKA.EntityManager;

    public scene: THREE.Scene;

    public vehicle!: YUKA.Vehicle;

    public vehiclePath!: YUKA.Path;

    truckObject!: THREE.Object3D;

    constructor(private sceneComponent: THREE.Scene) {
        this.scene = sceneComponent;
        this.buildTexture();
    }


    private buildTexture() {

        this.truckObject = new THREE.Object3D();
        const truckScale = 0.00025;
        const truckloader = new GLTFLoader();
        truckloader.load(
            './assets/italeri_truck/scene.gltf', 
            (object) => {
                object.scene.traverse(function(node){
                    if(node)
                        node.castShadow = true;
                });
            this.truckObject.add(object.scene);
        });

        this.truckObject.scale.set(truckScale, truckScale, truckScale);
        this.truckObject.castShadow = true;

        //this.truckObject.rotateX(Math.PI/2);
        //this.truckObject.rotateY(Math.PI/2);
        //this.truckObject.rotateZ(Math.PI/2);

        this.truckObject.matrixAutoUpdate = false;
        this.scene.add(this.truckObject);
        this.vehicle = new YUKA.Vehicle();
        this.vehicle.scale.set(0.001, 0.001, 0.001);
        this.vehicle.updateOrientation = false;
        this.vehicle.setRenderComponent(this.truckObject, this.sync);
    }

    yukaObject() {

        this.entityManager = new YUKA.EntityManager();

        this.entityManager.add(this.vehicle);

        this.time = new YUKA.Time();

    }

    buildPath(paths: any, positions: any) {
        this.vehiclePath = new YUKA.Path();

        const pathMap = new Map();
        for (const path of paths) {
            if (!pathMap.has(path.startWHId))
            pathMap.set(path.startWHId, path.destinationWHId);
        }
        
        let numPaths = 5;
        if(numPaths > paths.length)
            numPaths = paths.length;

        let startWHId = paths[0].startWHId;
        
        let list = new Array();
        list.push(positions.find((position: any) => position.wh == startWHId));

        for (let i = 0; i < numPaths; i++) {
            const destinationWhId = pathMap.get(startWHId);
            
            for(const position of positions)
                if(position.wh == startWHId && !list.includes(position))
                    list.push(position);

            startWHId = destinationWhId;
        }
        
        for(const positions of list)
            this.vehiclePath.add(new YUKA.Vector3(positions.x, positions.y, positions.z));
        
        const newList = list.slice(1, list.length-1).reverse();

        for(const positions of newList)
            this.vehiclePath.add(new YUKA.Vector3(positions.x, positions.y, positions.z));

        this.vehiclePath.loop = true;

        this.vehicle.position.copy(this.vehiclePath.current());
        this.vehicle.maxSpeed = 3;

        const folllowPathBehavior = new YUKA.FollowPathBehavior(this.vehiclePath, 0.5);
        this.vehicle.steering.add(folllowPathBehavior);

        const onPathBehavior = new YUKA.OnPathBehavior(this.vehiclePath);
        this.vehicle.steering.add(onPathBehavior);

    }

    private sync(entity: any, renderComponent: any) {
        renderComponent.matrix.copy(entity.worldMatrix);
    }
}