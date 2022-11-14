import * as THREE from "three";
export default class NodeTemplate {
    object: THREE.Group;

    constructor(pos:number[], connections: any, positions: any) {
        

        // console.log(positions[connections.connection1[1]-1]);
        // console.log(positions[connections.connection2[1]-1]);
        // console.log("---");
        // console.log();
        // Create a group of objects
        this.object = new THREE.Group();
        let geometry = new THREE.CircleGeometry( 1, 32 );
        let material = new THREE.MeshBasicMaterial( { color: 0x008080 } );
        let circle: THREE.Mesh = new THREE.Mesh(geometry, material);
        circle.position.set(pos[0], pos[1], pos[2]);

        let length = 5;

        let rectangleGeometry = new THREE.PlaneGeometry( 0.5, length, 32 );
        let rectangleMaterial = new THREE.MeshBasicMaterial( {color:  0x40e0d0, side: THREE.DoubleSide} );
        let rectangle: THREE.Mesh = new THREE.Mesh( rectangleGeometry, rectangleMaterial );
        let y2 = (Math.PI*positions[connections.connection1[1]-1][1])/180;
        let y1 = (Math.PI*pos[1])/180;

        let x2 = (Math.PI*positions[connections.connection1[1]-1][0])/180;
        let x1 = (Math.PI*pos[0])/180;

        rectangle.rotation.z= Math.atan2((y2-y1),(x2-x1))-Math.PI/2;
        console.log(rectangle.rotation.z);

        //displace position based on destination position
        
        

        rectangle.position.set(pos[0], pos[1], pos[2]);
        

        
        

        let rectangleGeometry2 = new THREE.PlaneGeometry( 0.5, length, 32 );
        let rectangleMaterial2 = new THREE.MeshBasicMaterial( {color:  0x40e0d0, side: THREE.DoubleSide} );
        let rectangle2: THREE.Mesh = new THREE.Mesh( rectangleGeometry2, rectangleMaterial2 );

        rectangle2.position.set(pos[0], pos[1], pos[2]);
        rectangle2.rotation.z= Math.PI/2;



        this.object.add(rectangle);
        // this.object.add(rectangle2);
        this.object.add(circle);
        
    }
}