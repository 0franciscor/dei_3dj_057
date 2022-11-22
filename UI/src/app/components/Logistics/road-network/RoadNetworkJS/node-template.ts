import * as THREE from "three";

import { warehousePosition, warehouseConnections } from "./default-data";

interface posProps{
    wh:string,
    x:number,
    y:number,
    z:number
}

interface elementProps{
    startWHId: string,
    destinationWHId: string,
    roadWidth:number
}
export default class NodeTemplate {
    object: THREE.Group;
    
    

    constructor(pos:posProps, outGoingConnections: any[],incomingConnections: any[],allPositions: any[]) {
        
        this.object = new THREE.Group();
        let length = 2;
        let largestWidth=0.5;

        let circleWidth=largestWidth*2;
        

        incomingConnections.forEach((element:elementProps ) => {
            
            let starting = allPositions.filter((allPositions:any) => allPositions.wh == element.startWHId).at(0);
            
          
            
            
            let rectangleGeometry = new THREE.PlaneGeometry( largestWidth, length, 32 );
            let rectangleMaterial = new THREE.MeshBasicMaterial( {color:  0x40e0d0, side: THREE.DoubleSide} );
            let rectangle: THREE.Mesh = new THREE.Mesh( rectangleGeometry, rectangleMaterial );
            
            let startY = (Math.PI*starting.y)/180;
            let posY = (Math.PI*pos.y)/180;
            let startX = (Math.PI*starting.x)/180;
            let posX = (Math.PI*pos.x)/180;
            
            rectangle.rotation.z= Math.atan2((startY-posY),(startX-posX))-Math.PI/2;

            rectangle.position.set(pos.x-length/2*Math.sin(rectangle.rotation.z), pos.y+length/2*Math.cos(rectangle.rotation.z), pos.z);
            
            this.object.add(rectangle);

        });

       

        outGoingConnections.forEach((element:elementProps) => {
           

            let destination = allPositions.filter((allPositions:any) => allPositions.wh == element.destinationWHId).at(0);

            let rectangleGeometry = new THREE.PlaneGeometry( largestWidth, length, 32 );
            let rectangleMaterial = new THREE.MeshBasicMaterial( {color:  0x40e0d0, side: THREE.DoubleSide} );
            let rectangle: THREE.Mesh = new THREE.Mesh( rectangleGeometry, rectangleMaterial );
            
            let destY = (Math.PI*destination.y)/180;
            let posY = (Math.PI*pos.y)/180;
            let destX = (Math.PI*destination.x)/180;
            let posX = (Math.PI*pos.x)/180;
            
            rectangle.rotation.z= Math.atan2((destY-posY),(destX-posX))-Math.PI/2;

            rectangle.position.set(pos.x-length/2*Math.sin(rectangle.rotation.z), pos.y+length/2*Math.cos(rectangle.rotation.z), pos.z);

            this.object.add(rectangle);


            
            let roadLength = Math.sqrt(Math.pow((destination.x-pos.x),2)+Math.pow((destination.y-pos.y),2))-length*2;
            let roadGeometry = new THREE.PlaneGeometry( largestWidth, roadLength, 32 );
            let roadMaterial = new THREE.MeshBasicMaterial( {color:  0xA52A2A , side: THREE.DoubleSide} );
            let road = new THREE.Mesh(roadGeometry, roadMaterial);
            road.position.set((pos.x+destination.x)/2, (pos.y+destination.y)/2, (pos.z+destination.z)/2);

            road.rotation.z= Math.atan2((destination.y-pos.y),(destination.x-pos.x))-Math.PI/2;
           
            
            this.object.add(road);
            

        
            
            
             

        });
        
        let geometry = new THREE.CircleGeometry(circleWidth, 32 );
        let material = new THREE.MeshBasicMaterial( { color: 0x008080, side: THREE.DoubleSide } );
        
        


        let circle: THREE.Mesh = new THREE.Mesh(geometry, material);
        circle.position.set(pos.x, pos.y, pos.z+0.1);



        this.object.add(circle);
        
    }
}