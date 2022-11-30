import { elementAt } from "rxjs";
import * as THREE from "three";
import { OBJLoader } from "three/examples/jsm/loaders/OBJLoader";
import { MTLLoader } from "three/examples/jsm/loaders/MTLLoader";
import { GLTFLoader } from "three/examples/jsm/loaders/GLTFLoader";
import { warehousePosition, warehouseConnections } from "./default-data";

interface posProps {
    wh: string,
    x: number,
    y: number,
    z: number
}

interface elementProps {
    startWHId: string,
    destinationWHId: string,
    roadWidth: number
}
export default class NodeTemplate {
    object: THREE.Group;



    constructor(pos: posProps, outGoingConnections: any[], incomingConnections: any[], allPositions: any[]) {

        this.object = new THREE.Group();


        let largestWidth = 0;

        incomingConnections.forEach((element: elementProps) => {
            if (element.roadWidth > largestWidth)
                largestWidth = element.roadWidth;
        })
        outGoingConnections.forEach((element: elementProps) => {
            if (element.roadWidth > largestWidth)
                largestWidth = element.roadWidth;
        })
        const circleConstant = 2;
        let circleRadius = (largestWidth * circleConstant) / 2;


        const connectionConstant = 0.5;
        let connectionLength = connectionConstant * circleConstant;
        incomingConnections.forEach((element: elementProps) => {



            //incoming connection element
            let starting = allPositions.filter((allPositions: any) => allPositions.wh == element.startWHId).at(0);
            if (starting) {

                let rectangleGeometry = new THREE.PlaneGeometry(element.roadWidth, connectionLength, 32);
                let rectangleMaterial = new THREE.MeshBasicMaterial({ color: 0x40e0d0, side: THREE.DoubleSide });
                let rectangle: THREE.Mesh = new THREE.Mesh(rectangleGeometry, rectangleMaterial);

                let startY = (Math.PI * starting.y) / 180;
                let posY = (Math.PI * pos.y) / 180;
                let startX = (Math.PI * starting.x) / 180;
                let posX = (Math.PI * pos.x) / 180;

                rectangle.rotation.z = Math.atan2((startY - posY), (startX - posX)) - Math.PI / 2;

                rectangle.position.set(pos.x - connectionLength / 2 * Math.sin(rectangle.rotation.z), pos.y + connectionLength / 2 * Math.cos(rectangle.rotation.z), pos.z);

                this.object.add(rectangle);

            }
        });



        outGoingConnections.forEach((element: elementProps) => {

            //outgoing connection element
            let destination = allPositions.filter((allPositions: any) => allPositions.wh == element.destinationWHId).at(0);
            if (destination) {


                let rectangleGeometry = new THREE.PlaneGeometry(element.roadWidth, connectionLength, 32);
                let rectangleMaterial = new THREE.MeshBasicMaterial({ color: 0x40e0d0, side: THREE.DoubleSide });
                let rectangle: THREE.Mesh = new THREE.Mesh(rectangleGeometry, rectangleMaterial);

                let destY = (Math.PI * destination.y) / 180;
                let posY = (Math.PI * pos.y) / 180;
                let destX = (Math.PI * destination.x) / 180;
                let posX = (Math.PI * pos.x) / 180;

                rectangle.rotation.z = Math.atan2((destY - posY), (destX - posX)) - Math.PI / 2;

                rectangle.position.set(pos.x - connectionLength / 2 * Math.sin(rectangle.rotation.z), pos.y + connectionLength / 2 * Math.cos(rectangle.rotation.z), pos.z);

                this.object.add(rectangle);

                //calculate connectionLength in x axis
                let connectionLengthX = connectionLength * Math.sin(rectangle.rotation.z);
                //calculate connectionLength in y axis
                let connectionLengthY = connectionLength * Math.cos(rectangle.rotation.z);



                //outgoing road
                let roadLength = Math.sqrt(Math.pow((destination.x - pos.x), 2) + Math.pow((destination.y - pos.y), 2) + Math.pow(destination.z - pos.z, 2))-Math.tan(connectionLength);



                let angle = Math.sqrt(Math.pow((destination.x - pos.x), 2) + Math.pow((destination.y - pos.y), 2)) - connectionLength * 2;

                let roadGeometry = new THREE.PlaneGeometry(element.roadWidth, roadLength, 32);
                let roadMaterial = new THREE.MeshBasicMaterial({ color: 0xA52A2A, side: THREE.DoubleSide });
                let road = new THREE.Mesh(roadGeometry, roadMaterial);
                road.position.set((pos.x + destination.x) / 2, (pos.y + destination.y) / 2, (pos.z + destination.z) / 2);

                road.rotation.z = Math.atan2((destination.y - pos.y), (destination.x - pos.x)) - Math.PI / 2;



                road.rotateOnAxis(new THREE.Vector3(1, 0, 0), Math.atan2((destination.z - pos.z), angle));

                this.object.add(road);
            }

        });

        //Circle
        let geometry = new THREE.CircleGeometry(circleRadius, 32);
        let material = new THREE.MeshBasicMaterial({ color: 0x008080, side: THREE.DoubleSide });

        let circle: THREE.Mesh = new THREE.Mesh(geometry, material);
        circle.position.set(pos.x, pos.y, pos.z + 0.1);
        this.object.add(circle);

        //Lighting
        const light = new THREE.AmbientLight(0xffffff, 1);
        //q: what are the ideal coordinates for the light?
        light.position.set(40,10,1200);
        this.object.add(light);


        // Warehouse Texture
        const warehouseTexture = new THREE.Object3D();
        const gltfloader = new GLTFLoader();

        gltfloader.load(
            './assets/farmhouse/scene.gltf', 
            (object) => {
            object.scene.scale.set(0.06, 0.06, 0.06);
            object.scene.position.set(pos.x, pos.y, pos.z + 0.2);
            object.scene.rotateX(Math.PI / 2);
            warehouseTexture.add(object.scene);

        });
        this.object.add(warehouseTexture);

    }
}