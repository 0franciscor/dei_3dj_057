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

interface elementProps {
    startWHId: string,
    destinationWHId: string,
    roadWidth: number
}
export default class NodeTemplate {
    object: THREE.Group;
    whAndWidth: whAndWidth = { wh: "", width: 0 };
    roadInclination: number = 0;


    constructor(pos: posProps, outGoingConnections: any[], incomingConnections: any[], allPositions: any[]) {

        this.object = new THREE.Group();
        this.whAndWidth.width=0;

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


        const connectionConstant = 1;
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

                
                


                //outgoing road
                
                const roadBeginX = pos.x - connectionLength * Math.sin(rectangle.rotation.z);
                const roadBeginY = pos.y + connectionLength * Math.cos(rectangle.rotation.z);
                const roadBeginZ = pos.z;

                
                const roadEndX = destination.x + connectionLength * Math.sin(rectangle.rotation.z);
                const roadEndY = destination.y - connectionLength * Math.cos(rectangle.rotation.z);
                const roadEndZ = destination.z;


               
            

                let roadLength = Math.sqrt(
                    Math.pow((roadEndX - roadBeginX), 2) + 
                    Math.pow((roadEndY - roadBeginY), 2) + 
                    Math.pow(roadEndZ- roadBeginZ, 2));

                

                let angle = Math.sqrt(Math.pow((destination.x - pos.x), 2) + Math.pow((destination.y - pos.y), 2)) - connectionLength * 2;
                let color = 0xA52A2A;
                

                let roadGeometry = new THREE.PlaneGeometry(element.roadWidth, roadLength, 32);
                let roadMaterial = new THREE.MeshBasicMaterial({ color: color, side: THREE.DoubleSide });
                let road = new THREE.Mesh(roadGeometry, roadMaterial);
                road.position.set((pos.x + destination.x) / 2, (pos.y + destination.y) / 2, (pos.z + destination.z) / 2);

                road.rotation.z = Math.atan2((destination.y - pos.y), (destination.x - pos.x)) - Math.PI / 2;



                road.rotateOnAxis(new THREE.Vector3(1, 0, 0), Math.atan2((destination.z - pos.z), angle));
                
                this.object.add(road);
            }

        });

        //Circle
        let geometry = new THREE.CircleGeometry(circleRadius, 32);
        let material = new THREE.MeshBasicMaterial({ color: 0x40e0d0, side: THREE.DoubleSide });

        let circle: THREE.Mesh = new THREE.Mesh(geometry, material);
        circle.position.set(pos.x, pos.y, pos.z + 0.001);
        this.object.add(circle);

        //Lighting
        const light = new THREE.AmbientLight(0xffffff, 1);
        //q: what are the ideal coordinates for the light?
        light.position.set(40,10,1200);
        light.name = "light";
        this.object.add(light);

        

        // Warehouse Texture
        const warehouseTexture = new THREE.Object3D();
        const gltfloader = new GLTFLoader();

        const warehouseScale = largestWidth *0.035;
        gltfloader.load(
            './assets/farmhouse/scene.gltf', 
            (object) => {
            object.scene.scale.set(warehouseScale, warehouseScale, warehouseScale);
            object.scene.position.set(pos.x, pos.y, pos.z + 0.002);
            object.scene.rotateX(Math.PI / 2);
            warehouseTexture.add(object.scene);

        });
        
        warehouseTexture.name = pos.wh
        this.object.add(warehouseTexture);



        this.whAndWidth.wh = pos.wh;
        this.whAndWidth.width = largestWidth;
        

    }
}