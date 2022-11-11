import * as THREE from "three";

/*
 * parameters = {
 *  textureUrl: String
 * }
 */

export default class NodeTemplate {
    object: THREE.Group;

    constructor() {
        


        // Create a group of objects
        this.object = new THREE.Group();
        let cubeGeometry = new THREE.BoxGeometry(1, 1, 1);
        let cubeMaterial = new THREE.MeshBasicMaterial({color: 0x00ff00});
        let cube: THREE.Mesh = new THREE.Mesh(cubeGeometry, cubeMaterial);

        let circle: THREE.Mesh = new THREE.Mesh(new THREE.CircleGeometry( 1, 32 ), new THREE.MeshBasicMaterial( { color: 0xffff00 } ));
 
        this.object.add(circle);
        this.object.add(cube);
    }
}