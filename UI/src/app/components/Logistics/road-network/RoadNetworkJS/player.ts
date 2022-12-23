import { Object3D } from "three";
import Truck from "./truck";
import * as THREE from "three";
export default class Player {
    private movementSpeed = 0.1;
    private windowsKeyDownListener: (event: KeyboardEvent) => void;
    constructor(selectedTruck:Object3D) {
        
    
        this.windowsKeyDownListener = (event: KeyboardEvent) => {
            const directionWS = new THREE.Vector3(0,-1,0);
            const directionAD = new THREE.Vector3(0,0,1);
            
            
            // Check the key that was pressed
            switch (event['key']) {
            case 'A':
                // Move the object forward along the z-axis
                selectedTruck.rotateOnWorldAxis(directionAD, 0.1);
                break;
            case 'D':
                selectedTruck.rotateOnWorldAxis(directionAD, -0.1);
                // selectedTruck.position.y += this.movementSpeed;
                break;
            case 'W':
                // Move the object left along the x-axis
     
                selectedTruck.translateOnAxis(directionWS, this.movementSpeed);
                break;
            case 'S':
               
                selectedTruck.translateOnAxis(directionWS, -this.movementSpeed);
                break;
            }
            };
            window.addEventListener('keydown', this.windowsKeyDownListener);
       
            
            
    }

    public destroy() {
        window.removeEventListener('keydown', this.windowsKeyDownListener);
    }

}