import * as THREE from "three";
export default class NodeTemplate {
    object: THREE.Group;

    constructor(pos:number[], connections: any, positions: any,destinations:any[][]) {
        console.log(positions)
        
        // console.log(connections)
        // console.log(positions[connections.connection1[1]-1]);
        // console.log(positions[connections.connection2[1]-1]);
        // console.log("---");
        // console.log();
        // Create a group of objects
        this.object = new THREE.Group();
        let geometry = new THREE.CircleGeometry( 1, 32 );
        let material = new THREE.MeshBasicMaterial( { color: 0x008080, side: THREE.DoubleSide } );
        let circle: THREE.Mesh = new THREE.Mesh(geometry, material);
        circle.position.set(pos[0], pos[1], pos[2]);

        let length = 5;

        
        //origin
        for (let i = 0; i < 2; i++) {
            let rectangleGeometry = new THREE.PlaneGeometry( 0.5, length, 32 );
            let rectangleMaterial = new THREE.MeshBasicMaterial( {color:  0x40e0d0, side: THREE.DoubleSide} );
            let rectangle: THREE.Mesh = new THREE.Mesh( rectangleGeometry, rectangleMaterial );
            
            let y2=0, y1=0, x2=0, x1 = 0;
            if(i == 0){
                y2 = (Math.PI*positions[connections.connection1[1]-1][1])/180;
                y1 = (Math.PI*pos[1])/180;

                x2 = (Math.PI*positions[connections.connection1[1]-1][0])/180;
                x1 = (Math.PI*pos[0])/180;
            }
            else{
                y2 = (Math.PI*positions[connections.connection2[1]-1][1])/180;
                y1 = (Math.PI*pos[1])/180;

                x2 = (Math.PI*positions[connections.connection2[1]-1][0])/180;
                x1 = (Math.PI*pos[0])/180;
            }
            rectangle.rotation.z= Math.atan2((y2-y1),(x2-x1))-Math.PI/2;
            
            rectangle.position.set(pos[0]-length/2*Math.sin(rectangle.rotation.z), pos[1]+length/2*Math.cos(rectangle.rotation.z), pos[2]);
            this.object.add(rectangle);
        }

        //destinations
        for (let i = 0; i < destinations.length; i++) {
            length = 4;
            let rectangleGeometry = new THREE.PlaneGeometry( 0.5, length, 32 );
            let rectangleMaterial = new THREE.MeshBasicMaterial( {color:  0xFFA500, side: THREE.DoubleSide} );
            let rectangle: THREE.Mesh = new THREE.Mesh( rectangleGeometry, rectangleMaterial );
            let y2 = (Math.PI*destinations[i][1])/180;
            let y1 = (Math.PI*pos[1])/180;
            
            let x2 = (Math.PI*destinations[i][0])/180;
            let x1 = (Math.PI*pos[0])/180;

            rectangle.rotation.z= Math.atan2((y2-y1),(x2-x1))-Math.PI/2;

            rectangle.position.set(pos[0]-length/2*Math.sin(rectangle.rotation.z), pos[1]+length/2*Math.cos(rectangle.rotation.z), pos[2]);
            this.object.add(rectangle);
        }
        

        this.object.add(circle);
        
    }
}