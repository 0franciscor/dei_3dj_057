import { Object3D } from "three";
import Truck from "./truck";

export default class Player {
    private movementSpeed = 0.1;
    
    constructor(selectedTruck:Object3D) {

        window.addEventListener('keydown', (event) => {

            // Check the key that was pressed
            switch (event['key']) {
            case 'A':
                // Move the object forward along the z-axis
                selectedTruck.position.y -= this.movementSpeed;
                break;
            case 'D':
                // Move the object backward along the z-axis
                selectedTruck.position.y += this.movementSpeed;
                break;
            case 'W':
                // Move the object left along the x-axis
                selectedTruck.position.x -= this.movementSpeed;
                break;
            case 'S':
                // Move the object right along the x-axis
                selectedTruck.position.x += this.movementSpeed;
                break;
            }
        });

    }

}