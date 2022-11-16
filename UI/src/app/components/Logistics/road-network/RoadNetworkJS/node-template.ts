import * as THREE from "three";
import { warehousePosition, warehouseConnections } from "./default-data";
export default class NodeTemplate {
    object: THREE.Group;
    

    constructor(pos:number[], connections: any[][], allWidths: number[]) {
        console.log(warehouseConnections.matrix)
        this.object = new THREE.Group();
        let length = 2;
        let width=0;
        let largestWidth=0;
        let index=0;

        let circleWidth=0;
        connections.forEach(element => {
            largestWidth=0;
            
            warehouseConnections.matrix.forEach(whElement => {
                
                if(whElement[0] == index+1 || whElement[1] == index+1){
                    width = whElement[2];
                    if(width>largestWidth){
                        largestWidth=width
                    }
                    if(largestWidth>circleWidth){
                        circleWidth=largestWidth;
                    }
                }
            });

            index++;
            
            
            let rectangleGeometry = new THREE.PlaneGeometry( largestWidth, length, 32 );
            let rectangleMaterial = new THREE.MeshBasicMaterial( {color:  0x40e0d0, side: THREE.DoubleSide} );
            let rectangle: THREE.Mesh = new THREE.Mesh( rectangleGeometry, rectangleMaterial );

            let y2 = (Math.PI*element[1])/180;
            let y1 = (Math.PI*pos[1])/180;
            let x2 = (Math.PI*element[0])/180;
            let x1 = (Math.PI*pos[0])/180;

            rectangle.rotation.z= Math.atan2((y2-y1),(x2-x1))-Math.PI/2;
            
            rectangle.position.set(pos[0]-length/2*Math.sin(rectangle.rotation.z), pos[1]+length/2*Math.cos(rectangle.rotation.z), pos[2]);
            this.object.add(rectangle);


            
            let roadLength = Math.sqrt(Math.pow((element[0]-pos[0]),2)+Math.pow((element[1]-pos[1]),2))-length*2;
            let roadGeometry = new THREE.PlaneGeometry( largestWidth, roadLength, 32 );
            let roadMaterial = new THREE.MeshBasicMaterial( {color:  0xA52A2A , side: THREE.DoubleSide} );
            let road = new THREE.Mesh(roadGeometry, roadMaterial);
  
            

            
            


            road.rotation.z= Math.atan2((y2-y1),(x2-x1))-Math.PI/2;
            road.position.set((pos[0]+element[0])/2, (pos[1]+element[1])/2, (pos[2]+element[2])/2);
            this.object.add(road);
            
            
        });
        console.log("largestWidth: " + largestWidth)
        let geometry = new THREE.CircleGeometry(circleWidth, 32 );
        let material = new THREE.MeshBasicMaterial( { color: 0x008080, side: THREE.DoubleSide } );
        if(connections[0][0] === 1){
            material = new THREE.MeshBasicMaterial( { color: 0xffff00, side: THREE.DoubleSide } );
        }
        


        let circle: THREE.Mesh = new THREE.Mesh(geometry, material);
        circle.position.set(pos[0], pos[1], pos[2]);



        this.object.add(circle);
        
    }
}